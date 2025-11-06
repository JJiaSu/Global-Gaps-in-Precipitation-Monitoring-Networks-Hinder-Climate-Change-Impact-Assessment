'Created by Jiajia.Su
This code create absolute error based on CMIP6, gauge data ##'

library(tidyverse)
library(ggplot2)
library(sp)
library(rgdal)
library(dplyr)
library(plyr)
library(raster)
library(ggx)
library(data.table)
library(sf)
library(spatialEco)
gc()

crs_84 <- st_crs("EPSG:4326")  ## WGS 84 大地坐标
crs_al <- st_crs("+proj=aea +lat_1=25 +lat_2=47 +lon_0=105") ## Albers Equal Area Conic投影

#set your working directory
data.dir = "F:/sujiajia_2/data/sample_data"
proj.geo = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0 "


#import continet boundary
continent.shp<-readOGR(paste0(data.dir,"/globalCountry/continents.shp"))
projection(continent.shp) = '+proj=longlat +datum=WGS84 +no_defs '

#remove the Antarctica
continent.shp <- subset(continent.shp, OBJECTID!= 8)


# get the observed and unobserved zone ------------------------------------
gaugeDen.df<-fread(paste0(data.dir,
                          '/sample_gauge_density_1degree.csv'))


unobserved_mask<-rasterFromXYZ(data.frame(lon=gaugeDen.df$lon,
                                          lat = gaugeDen.df$lat,
                                          density = gaugeDen.df$density),
                               res=c(1,1),crs=crs(proj.geo))

unobserved_mask[unobserved_mask>0]=1
unobserved_mask <- crop(unobserved_mask,continent.shp)

crs(unobserved_mask ) <- '+proj=longlat +datum=WGS84 +no_defs '

# pre-processing CMIP6 data -----------------------------------------------
'tas:air temperature ，near surface(2m) air tempereture，k
pr:precipitation flux， kg m-2 s-1
hur:Relative Humidity [%]
evspsbl:water evapotranspiration flux，kg m-2 s-1
emibc:Total Emission Rate of Black Carbon Aerosol Mass [kg m-2 s-1]
'
ClimateModels = c('CESM2-WACCM')# 

#'ssp245','ssp370',
ClimateCondition = c('ssp585')
CMIP_Variables = c('tas','evspsbl')#'hur',,'emibc'

pr_Variables<-c('pr')

#import CMIP6 data
library(terra)
CMIP6.ls<-list.files(paste0(data.dir,'/CMIP6'),pattern = "*.nc$")

# Define the time period to estimate climate and subset the original raster 
# from = 2015 
from = 2025
to = 2100

from_his = 1970
to_his = 1999
library(lubridate)


layer_name <- seq(as.Date(paste(from, "1", "1", sep = "/")), 
                  as.Date(paste(to, "12", "1", sep = "/")), 
                  by = "month") 

#Create an indices to prepare it for stackApply, 
#which takes the means for all years within each month.
indices <- format(as.Date(layer_name, format = "%Y.%m.%d"), format = "%m") 
indices_year <- format(as.Date(layer_name, format = "%Y.%m.%d"), format = "%Y")

layer_name_his <- seq(as.Date(paste(from_his, "1", "1", sep = "/")), 
                  as.Date(paste(to_his, "12", "1", sep = "/")), 
                  by = "month") 
#Create an indices to prepare it for stackApply, 
#which takes the means for all years within each month.
indices_year_his <- format(as.Date(layer_name_his, format = "%Y.%m.%d"), format = "%Y")


names.yrs <- seq.Date(from = as.Date("2025/01/01",format = "%Y/%m/%d"),
                      to = as.Date("2100/12/01",format = "%Y/%m/%d"),
                      by = "month")
# 
# indices_YR <- format(as.Date(names.yrs, format = "%Y.%m.%d"), format = "%Y") 


library(VoCC)
for (ssp in 1:length(ClimateCondition)) { #SSP
  select_SPP <- grep(ClimateCondition[ssp],CMIP6.ls,value=T)
  select_his <- grep('historical',CMIP6.ls,value = T)
  for (model in 1:length(ClimateModels)) { #model
    select_model <- grep(ClimateModels[model],select_SPP,value=T)
    (select_his_mod <- grep(ClimateModels[model],select_his,value=T))
    #get the monthly and yearly CMIP variables
    CMIP_MON_stack<-stack()
    CMIP_YR_stack <- stack()
    baseline <- stack()
    CMIP_mon_ls <- list()
    
    #get the baseline multi-yearly mean
    for(baseline.idx in 1:length(select_his_mod)){
      if(baseline.idx == 1){
        baseline<-brick(paste0(data.dir,'/CMIP6/',select_his_mod[baseline.idx]),
                        stopIfNotEqualSpaced=FALSE)
      }else{
        baseline1<-brick(paste0(data.dir,'/CMIP6/',select_his_mod[baseline.idx]),
                         stopIfNotEqualSpaced=FALSE)
        baseline <- addLayer(baseline,baseline1)
      }
    }
    
    
    if(length(names(baseline))!=1980){
      print(paste0('the baseline length is ',length(names(baseline))),
            ": ",ClimateModels[model] )
    }
  
    #1970-1999
    baseline <- baseline[[1441:1800]]
    
    for (var in 1:length(CMIP_Variables)) {
      (select_var<-grep(CMIP_Variables[var],select_model,value=T))
      'FGOALS-g3 have error when stack, so add loop'
      # cmip<-c()
      for (cmip.ind in 1:length(select_var)) {
        if(cmip.ind == 1){
          cmip<-brick(paste0(data.dir,'/CMIP6/',select_var[cmip.ind]),
                       stopIfNotEqualSpaced=FALSE)
        }else{
          cmip1<-brick(paste0(data.dir,'/CMIP6/',select_var[cmip.ind]),
                       stopIfNotEqualSpaced=FALSE)
          cmip <- addLayer(cmip,cmip1)
        }

      }
      'change at 5/6/2023, time serise is 2025-2100'
      cmip <- cmip[[121:1032]]
      
      names(cmip)<-layer_name
      names(cmip)<-sub('.', CMIP_Variables[var], names(cmip))  
      
      # if(CMIP_Variables[var] == 'emibc'){
      #   cmip <- resample(cmip,CMIP_YR_stack[[1]],method='bilinear') 
      # }
      CMIP_mon_ls[[var]]<-cmip
     
      ##  Mean annual trend
      if(var==1){#'tas'
        YR.cmip<-stackApply(cmip, indices_year, mean)
 
      }else{#'hur','evspsbl','emibc'
        YR.cmip<-stackApply(cmip, indices_year, sum)

      }
      names(YR.cmip)<-paste0(CMIP_Variables[var], '_',c(from:to))  
      CMIP_YR_stack <- addLayer(CMIP_YR_stack, YR.cmip)
      
    }
    rm(YR.cmip)
    gc()
    
    #get the precipitation data stack, same as CMIP6
    (select_var<-grep(pr_Variables,select_model,value=T))
    # pr<-stack(paste0(data.dir,'/CMIP6/',select_var))
    'FGOALS-g3 have error when stack, so add loop'
    for (cmip.ind in 1:length(select_var)) {
      if(cmip.ind == 1){
        pr<-brick(paste0(data.dir,'/CMIP6/',select_var[cmip.ind]),
                    stopIfNotEqualSpaced=FALSE)
      }else{
        pr1<-brick(paste0(data.dir,'/CMIP6/',select_var[cmip.ind]),
                     stopIfNotEqualSpaced=FALSE)
        pr <- addLayer(pr,pr1)
      }
      
    }
    'time serise is 2025-2100'
    pr <- pr[[121:1032]]


    ##  Mean annual trend
    pr_YR_stack <- stackApply(pr, indices_year, sum)
     names.yr <- paste0('pr','_',c(from:to))
    names(pr_YR_stack)<-paste0('pr','_',c(from:to))  
    

    #get the baseline multi-yearly mean
    baseline_YR_stack <- stackApply(baseline, indices_year_his, sum)
    baseline_mean <- mean(baseline_YR_stack)
    #remove the varibale to save memory
    rm(cmip)
    rm(pr)
    rm(baseline_YR_stack)
    gc()
    
    # get the same resolution data, CMIP6
    pr_stack <- resample(rotate(pr_YR_stack),unobserved_mask,method='bilinear') 
    baseline_mean <- resample(rotate(baseline_mean),unobserved_mask,method='bilinear') 
    
    # projection(pr_stack ) <- crs(unobserved_mask)
    CMIP_stack <- resample(rotate(CMIP_YR_stack),unobserved_mask,method='bilinear')
    # crs(CMIP_stack) <- '+proj=longlat +datum=WGS84 +no_defs '
    
    for(i in 1:length(CMIP_Variables)){
      CMIP_mon_ls[[i]] <- resample(rotate(CMIP_mon_ls[[i]]),unobserved_mask,
                                   method='bilinear')
      # crs(CMIP_mon_ls[[i]]) <- '+proj=longlat +datum=WGS84 +no_defs '
    }
    
    
    print(paste0('finish stack raster'))
    
    # up-scale CMIP6 ----------------------------------------------------------
    
    #divide grid into observed and unobserved
    baseline_obs = baseline_mean
    baseline_unobs = baseline_mean
    y_test=(pr_stack)
    y_train=pr_stack
    
    x_test = CMIP_stack
    x_train = CMIP_stack
    

    #get same extent of grid 1
    baseline_unobs[is.na(unobserved_mask)] = NA
    y_train[is.na(unobserved_mask)] = NA
    x_train[is.na(unobserved_mask)] = NA
    y_test[is.na(unobserved_mask)] = NA
    x_test[is.na(unobserved_mask)] = NA
    for(i in 1:length(CMIP_Variables)){
      CMIP_mon_ls[[i]] [is.na(unobserved_mask)] = NA
    }
    
    #mask unobserved pixel as NA
    y_train[unobserved_mask==0] = NA
    x_train[unobserved_mask==0] = NA
    baseline_unobs[unobserved_mask==0] = NA
    
    
    # train and predict with random forest
    library(randomForest)
    library(pdp)           # for partial dependence plots
    library(vip)           # for variable importance plots
    pts <- data.frame(long = gaugeDen.df$lon, lat = gaugeDen.df$lat)
    i=1
    
    #loop predict pr for each month
    y_test_predict <- stack()
    y_test_predict_erro <- stack()
    y_train_predict <- stack()
    y_train_predict_erro <- stack()
    R_record <- c()
    'change at 20/9/2023, add the recording of explained information of
    each variable'
    explained_record <- c()
    
    #Create an indices to prepare it for stackApply, 
    #which takes the means for all years within each month.

    indices_YR <- indices_year
  
    YR <- c(from:to)
    for (i in 1:length(c(from:to))) { #YEAR
      
      #stack same year and each month within same year CMIP
      x_train_yr =stack( x_train[[i]],x_train[[i+76]])#两个变量
      #,x_train[[i+86*2]]
      for (j in 1:length(CMIP_Variables)) { #each month within same year
        x_train_yr = stack(x_train_yr,
                           CMIP_mon_ls[[j]] [[which(indices_YR==YR[i])]])
      }  
      
      x_test_yr =stack( x_test[[i]],x_test[[i+76]])
      #,x_test[[i+86*2]]
      for (j in 1:length(CMIP_Variables)) { #each month within same year
        x_test_yr = stack(x_test_yr,
                          CMIP_mon_ls[[j]] [[which(indices_YR==YR[i])]])
      }
      
      
      data.grd = stack(y_train[[i]], x_train_yr)
      data.df = as.data.frame(raster::extract(data.grd, pts)) %>% 
        filter(!is.na(.[,1])) 
      names(data.df)[1]<-'pr'
      
      
      # divide the train and test data 
      set.seed(123)
      train_idx <- sample(nrow(data.df), 0.7*nrow(data.df))
      train_data <- data.df[train_idx, ]
      test_data <- data.df[-train_idx, ]

      rf.fit1 <- randomForest(pr ~ ., data = train_data, ntree=300, mtry=2, importance=TRUE,
                              na.action=na.omit)


      pred <- predict(rf.fit1, newdata = test_data)
      rmse <- sqrt(mean((test_data$pr - pred)^2))  # 回归问题评估
      # 计算R²
      test_r2 <- 1 - (sum((test_data$pr - pred)^2) /
                        sum((test_data$pr - mean(test_data$pr))^2))
  

      
      # rf.fit1 <- randomForest(pr ~ ., data=data.df, 
      #                         ntree=300, mtry=2, importance=TRUE,
      #                         na.action=na.omit)
      #########################################################
      
      #get the fitness of model
      R_record_YR <- data.frame(
                                var_explained = test_r2,
                                year = YR[i],
                                model = ClimateModels[model],
                                ssp = ClimateCondition[ssp])
      R_record <- rbind(R_record, R_record_YR)

      explained_record_YR <- importance(rf.fit1, type=1) %>% 
        as.data.frame(.) %>% 
        mutate(var = rownames(.),
               year = YR[i],
               model = ClimateModels[model],
               ssp = ClimateCondition[ssp])
      explained_record <- rbind(explained_record, explained_record_YR)
      
      # varImpPlot(rf.fit1)
      # plot(rf.fit1)
      # print(rf.fit1) # view results 
      # (a<-importance(rf.fit1, type=1)) # importance of each predictor 
      y.test.predict =  predict(x_test_yr, rf.fit1, type="response")
      y.train.predict = predict(x_train_yr, rf.fit1, type="response")
      
      y.train.predict.erro = y_train[[i]] - y.train.predict
      # hist(y_train_predic_erro)
      y.test.predict.erro = y_test[[i]] - y.test.predict
      
      
      y_test_predict <- addLayer(y_test_predict, y.test.predict)
      y_test_predict_erro <- addLayer(y_test_predict_erro,
                                      y.test.predict.erro)
      y_train_predict <- addLayer(y_train_predict, y.train.predict)
      y_train_predict_erro <- addLayer(y_train_predict_erro,
                                       y.train.predict.erro)
    }

    write.table(R_record,
                paste0(data.dir,'/PreErro/',
                       ClimateCondition[ssp],'_',ClimateModels[model],
                       '_RF_var_explained.csv'),row.names = F,
                col.names=TRUE,sep=",",quote=F)
    
    write.table(explained_record,
                paste0(data.dir,'/PreErro/',
                       ClimateCondition[ssp],'_',ClimateModels[model],
                       '_RF_var_explained_each.csv'),row.names = F,
                col.names=TRUE,sep=",",quote=F)
    names(y_train_predict)<-names.yr
    names(y_train_predict_erro)<-names.yr
    names(y_test_predict)<-names.yr
    names(y_test_predict_erro)<-names.yr
    
    print(paste0('finish random forest prediction'))
    

# calculate the difference bt up-scale model ------------------------------


    # calculating the the difference between the
    # trend estimates of both CMIP6 model data and
    # up-scaling model prediction
    # # first, calculate the # of NA value in each grid
    # NoNa = function(x){
    #   len = length(which(!is.na(x)))
    #   return(len)
    # }
    #


    library(spatialEco)
    y_test_predict <- as(y_test_predict, "SpatRaster")
    y_train_predict <- as(y_train_predict, "SpatRaster")
    y_test <- as(y_test, "SpatRaster")
    y_train <- as(y_train, "SpatRaster")
    baseline_obs <- as(baseline_obs,'SpatRaster')
    baseline_unobs <- as(baseline_unobs,'SpatRaster')

    #calculate the anomalies
    y_test_predict = y_test_predict - baseline_obs
    y_test = y_test - baseline_obs
    y_train_predict = y_train_predict - baseline_unobs
    y_train = y_train - baseline_unobs

    # calc trend
    y_test_predict_slop = raster.kendall(y_test_predict, tau = FALSE,
                                         intercept = FALSE,  p.value = FALSE,
                                         z.value = FALSE, confidence = FALSE)
    y_test_slop = raster.kendall(y_test, tau = FALSE,
                                 intercept = FALSE,  p.value = FALSE,
                                 z.value = FALSE, confidence = FALSE)

    y_train_predict_slop = raster.kendall(y_train_predict, tau = FALSE,
                                          intercept = FALSE,  p.value = FALSE,
                                          z.value = FALSE, confidence = FALSE)
    y_train_slop = raster.kendall(y_train, tau = FALSE,
                                  intercept = FALSE,  p.value = FALSE,
                                  z.value = FALSE, confidence = FALSE)


    corr_test_trend <- abs(y_test_slop - y_test_predict_slop)
    corr_train_trend <- abs(y_train_slop - y_train_predict_slop)



    # calculating the the difference between the
    # inter-annual variability of both CMIP6 model data and
    # up-scaling model prediction
    corvec <- function(vec = NULL) {
      cor(
        x      = vec[1:(length(vec)/2)],
        y      = vec[((length(vec)/2) + 1):length(vec)],
        use    = 'complete.obs',
        method = 'spearman'
      )
    }

    y_test_predict <- as(y_test_predict, "Raster")
    y_train_predict <- as(y_train_predict, "Raster")
    y_test <- as(y_test, "Raster")
    y_train <- as(y_train, "Raster")

    corr_test_variability <- abs(calc(
      stack(y_test,y_test_predict),
      fun = function(x) {
        # 对缺失值进行处理
        if (all(is.na(x))) {
          NA_real_
        } else {
          corvec(vec = x)
        }
      }
    ))

    corr_train_variability <- abs(calc(
      stack(y_train,y_train_predict),
      fun = function(x) {
        # 对缺失值进行处理
        if (all(is.na(x))) {
          NA_real_
        } else {
          corvec(vec = x)
        }
      }
    ))

    corr_test_trend <- as(corr_test_trend, "Raster")
    corr_train_trend <- as(corr_train_trend, "Raster")

    resultstack<-stack(corr_test_trend*10^12,corr_train_trend*10^12,
                       corr_test_variability,corr_train_variability)
    names(resultstack)<-c('corr_test_trend','corr_train_trend',
                          'corr_test_variability','corr_train_variability')
    writeRaster(resultstack,
                filename=paste0(data.dir,"/PreErro/prediction_error_",
                                ClimateCondition[ssp],'_',ClimateModels[model]),
                format = 'GTiff',overwrite = TRUE)
    writeRaster(y_test_predict_erro,
                filename=paste0(data.dir,"/PreErro/y_test_predict_erro_",
                                ClimateCondition[ssp],'_',ClimateModels[model]),
                format = 'GTiff',overwrite = TRUE)
    writeRaster(y_train_predict_erro,
                filename=paste0(data.dir,"/PreErro/y_train_predict_erro",
                                ClimateCondition[ssp],'_',ClimateModels[model]),
                format = 'GTiff',overwrite = TRUE)

    print(paste0('finish prediction error for ', 
                 ClimateCondition[ssp],' ',ClimateModels[model]))
  }
}


