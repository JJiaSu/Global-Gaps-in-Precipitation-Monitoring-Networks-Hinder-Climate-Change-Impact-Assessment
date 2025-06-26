# Author：Jiajia Su
# Date：2025-06-08
'Created by Jiajia.Su
This code create priority development of gauges, analysis entropy##'

library(tidyverse)
library(ggplot2)
library(sp)
# library(rgdal)
library(plyr)
library(dplyr)

library(raster)
library(data.table)
library(sf)
# library(spatialEco)
gc()

crs_84 <- st_crs("EPSG:4326")  ## WGS 84 大地坐标
crs_al <- st_crs("+proj=aea +lat_1=25 +lat_2=47 +lon_0=105") ## Albers Equal Area Conic投影

data.dir = "F:/sujiajia_2/data"

proj.geo = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0 "



# # **1.2 start crop extent from here ---------------------------------------


#import location of gauges
newly_loc <- fread(paste0(data.dir,
                          '/gauge1/location/location_station_valid_from_allstationdata.csv')
                   ,quote = "",header=TRUE,check.names=F)%>%
  dplyr::mutate(FID = 1:n())
newly_loc.df <- newly_loc

coordinates(newly_loc) = ~longitude+latitude
projection(newly_loc) = proj.geo
# spplot(newly_loc, "PRCP.VALUE")


duration <- c(1900:2022)
windows <- c(1)

library(rgdal)
continent.shp<-readOGR(paste0(data.dir,"/globalCountry/continent_shp/continents.shp"))
projection(continent.shp) = '+proj=longlat +datum=WGS84 +no_defs '


daily_station<-readRDS(paste0(data.dir,'/precipitation_gauges/MI/stationdata_by_daily.rds'))


#get the station within the extent
pts_ext_original <- newly_loc

# setp2: examine the candidate station importance by entropy-----------------------------------------------
library(entropy) #discrete
# * 2.1 prapare the dataset for entropy -----------------------------------------

daily_station <- daily_station %>% 
  dplyr::group_by(FID,DATE) %>%
  # #remove depulicated
  distinct() %>% 
  ungroup()



# reset dataframe for analysing
daily_station_long <- daily_station %>% 
  dplyr::select(FID,PRCP.VALUE,DATE) %>%
  
  pivot_wider(., names_from = FID, values_from = PRCP.VALUE,
              values_fill = NA)  


# loop for each one gauge, Calculate the summarize of 
# mutual information, joint entropy
# between all columns by gauge extent
# library(mpmi)
# library(Hmisc)
gaugeEntropy.df<- daily_station%>%
  filter(!duplicated(FID)) #%>% 

# get the spatial points of candidate for select points within window
loc_candidate_pts<-gaugeEntropy.df
coordinates(loc_candidate_pts) = ~longitude+latitude
projection(loc_candidate_pts) = CRS("+init=epsg:4326")

# rm(daily_station)
# gc()


# * 2.2 loop begain here --------------------------------------------------
library(entropy)
#* this is newly entropy, which maybe is right -------------------------------
# calculate the entropy of each columns
pts_cadidate_EN<-daily_station %>% 
  group_by(FID) %>% 
  # mutate(preci_log = log(preci)) %>% 
  dplyr::summarise(entropy = entropy::entropy(PRCP.VALUE)) 
rm(daily_station)
gc()

# Discretization
delta_A =1
Discretization <- function(x,weishu_add=1){
  floor((x*2+delta_A)/(2*delta_A))*delta_A
}

for (wids in 1:length(windows)) {  #different dynamic window
  
  # MI_all<-c()
  MI_all<-c()  #256
  library(foreach)
  library(doParallel)
  library(raster)
  library(dplyr)
  library(entropy)
  if(exists("cl")) {
    try(stopCluster(cl), silent = TRUE)  
    rm(cl)
  }
  
  ncores <- detectCores() - 1  
  cl <- parallel::makeCluster(40)
  doParallel::registerDoParallel(cl)
  
  
  
  message(paste("Using", ncores, "cores for parallel processing"))
  message(paste("Total iterations:", ncol(daily_station_long)-1))
  #start 6.17   12:48
  
  
  MI_all <- foreach(j = 1:(ncol(daily_station_long)-1), 
                    .combine = function(...) bind_rows(list(...)),
                    .packages = c("dplyr", "raster", "entropy", "tidyr"),
                    .export = c("Discretization", "windows", "wids", 
                                "loc_candidate_pts", "daily_station_long"),
                    .multicombine = TRUE,
                    .maxcombine = 1000,
                    .errorhandling = "pass") %dopar% {
                      
                      
                      if(j %% 100 == 0) {
                        message(paste0("[", Sys.time(), "] Processing station ", j, "/", ncol(daily_station_long)-1))
                      }
                      
                      
                      tryCatch({
                        gauge.sp.ext1 <- as(raster::extent(loc_candidate_pts[j,]), "SpatialPolygons")
                        crs(gauge.sp.ext1) <- '+proj=utm +zone=10 +datum=WGS84'
                        gauge.sp.ext1 <- raster::buffer(gauge.sp.ext1, windows[wids])
                        
                        loc_candidate_pts_ext1 <- raster::crop(loc_candidate_pts, gauge.sp.ext1)
                        
                        daily_long_ext1 <- daily_station_long %>%
                          dplyr::select(-1) %>% 
                          dplyr::select(!!as.character(loc_candidate_pts[j, 'FID']$FID),
                                        !!(as.character(loc_candidate_pts_ext1$FID)))
                        
                        if(dim(daily_long_ext1)[2] > 1){
                          main_station <- names(daily_long_ext1)[1]
                          
                          calculate_time_range <- function(column) {
                            non_na_indices <- which(!is.na(column))
                            if(length(non_na_indices) == 0) return(list(min = NA, max = NA))
                            return(list(min = min(non_na_indices), max = max(non_na_indices)))
                          }
                          
                          calculate_overlap_with_main <- function(station_name, main_range, data) {
                            station_range <- calculate_time_range(data[[station_name]])
                            if(is.na(main_range$min) || is.na(station_range$min)) return(0)
                            overlap_start <- max(main_range$min, station_range$min)
                            overlap_end <- min(main_range$max, station_range$max)
                            if(overlap_end < overlap_start) return(0)
                            return(overlap_end - overlap_start + 1)
                          }
                          
                          main_range <- calculate_time_range(daily_long_ext1[[main_station]])
                          
                          while(TRUE) {
                            common_data <- daily_long_ext1[complete.cases(daily_long_ext1), ]
                            n_common <- nrow(common_data)
                            if(n_common >= 150) break
                            
                            station_overlaps <- sapply(names(daily_long_ext1), 
                                                       function(name) {
                                                         if(name == main_station) return(Inf)
                                                         return(calculate_overlap_with_main(name, 
                                                                                            main_range, 
                                                                                            daily_long_ext1))
                                                       })
                            
                            station_df <- data.frame(
                              name = names(daily_long_ext1),
                              overlap = station_overlaps,
                              stringsAsFactors = FALSE
                            ) %>%
                              filter(overlap != Inf) %>%
                              arrange(overlap)
                            
                            if(nrow(station_df) == 0 || max(station_df$overlap, na.rm = TRUE) <= 0) break
                            
                            remove_station <- station_df$name[1]
                            daily_long_ext1 <- daily_long_ext1 %>% dplyr::select(-all_of(remove_station))
                            
                            if(ncol(daily_long_ext1) <= 1) break
                            main_range <- calculate_time_range(daily_long_ext1[[main_station]])
                          }
                          
                          daily_long_ext1_disc <- daily_long_ext1 %>%
                            mutate_all(Discretization) %>%
                            .[complete.cases(.), ]
                          
                          length_data <- dim(daily_long_ext1_disc)[1]
                          
                          if(length_data > 4 && sum(daily_long_ext1_disc) != 0) {
                            pts_cadidate_MI <- entropy::mi.empirical(daily_long_ext1_disc, unit = "log2") %>%
                              data.frame() %>%
                              rename_with(~'mutual_Info') %>%
                              mutate(FID = as.character(loc_candidate_pts[j, 'FID']$FID))
                            
                            pts_cadidate_JEN <- entropy::entropy(as.matrix(daily_long_ext1_disc), unit = "log2") %>%
                              data.frame() %>%
                              rename_with(~'joint_E') %>%
                              mutate(FID = as.character(loc_candidate_pts[j, 'FID']$FID))
                          } else {
                            pts_cadidate_MI <- data.frame(
                              mutual_Info = NA, 
                              FID = as.character(loc_candidate_pts[j, 'FID']$FID)
                            )
                            pts_cadidate_JEN <- data.frame(
                              joint_E = NA, 
                              FID = as.character(loc_candidate_pts[j, 'FID']$FID)
                            )
                          }
                        } else {
                          pts_cadidate_MI <- data.frame(
                            mutual_Info = 0, 
                            FID = as.character(loc_candidate_pts[j, 'FID']$FID)
                          )
                          pts_cadidate_JEN <- data.frame(
                            joint_E = 0, 
                            FID = as.character(loc_candidate_pts[j, 'FID']$FID)
                          )
                        }
                        
                        result <- inner_join(pts_cadidate_MI, pts_cadidate_JEN, by = "FID")
                        return(result)
                        
                      }, error = function(e) {
                        # get error, if have
                        error_df <- data.frame(
                          mutual_Info = NA,
                          joint_E = NA,
                          FID = if(exists("loc_candidate_pts") && nrow(loc_candidate_pts) >= j) {
                            as.character(loc_candidate_pts[j, 'FID']$FID)
                          } else paste0("Error_", j),
                          error_message = paste("Error in station", j, ":", conditionMessage(e))
                        )
                        return(error_df)
                      })
                    }
  
  
  try({
    stopCluster(cl)
    rm(cl)
    registerDoSEQ() 
  })
  
  
  pts_cadidate_EN <- pts_cadidate_EN %>% 
    merge(MI_all)    
  
  gaugeEntropy.df<-merge(gaugeEntropy.df,pts_cadidate_EN,
                         by='FID')
  
  # rm(pts_cadidate_EN)
  # rm(daily_station)
  # rm(daily_station_long)
  rm(daily_long_ext1_disc)
  rm(daily_long_ext1)
  gc()
  
  
  
  write.table(gaugeEntropy.df,
              paste0(data.dir,'/precipitation_gauges/MI/1900-22_entropy_by_daily.csv'),row.names = F,
              col.names=TRUE,sep=",",quote=F)
  print('write successful')
}


