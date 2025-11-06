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


#set your working directory
data.dir = "F:/sujiajia_2/code/station_optimal/Global Gaps in Precipitation Monitoring Networks Hinder Climate Change Impact Assessment/sample_data"

proj.geo = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0 "



# # **1.2 start crop extent from here ---------------------------------------

duration <- c(1900:2022)
windows <- c(1)

library(rgdal)
continent.shp<-readOGR(paste0(data.dir,"/globalCountry/continents.shp"))
projection(continent.shp) = '+proj=longlat +datum=WGS84 +no_defs '


daily_station<-readRDS(paste0(data.dir,'/stationdata_sample.rds'))



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
# Discretization
delta_A =1
Discretization <- function(x,weishu_add=1){
  floor((x*2+delta_A)/(2*delta_A))*delta_A
}

# calculate the entropy of each columns
pts_cadidate_EN<-daily_station %>% 
  group_by(FID) %>% 
  # mutate(preci_log = log(preci)) %>% 
  dplyr::summarise(entropy = entropy::entropy(PRCP.VALUE)) 
rm(daily_station)
gc()

na_ranges <- lapply(daily_station_long[-1], function(col) {
  non_na <- which(!is.na(col))
  if (length(non_na) == 0) c(Inf, -Inf) else range(non_na)
})

# calculate the time overlap 
optimized_calculate_overlap <- function(name, main_range, station_ranges) {
  station_range <- station_ranges[[name]]
  if (any(is.infinite(station_range))) return(0)
  
  overlap_start <- max(main_range[1], station_range[1])
  overlap_end <- min(main_range[2], station_range[2])
  
  if (overlap_end < overlap_start) 0 else (overlap_end - overlap_start + 1)
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
  
  
  
  start_time <- Sys.time()
  message(paste("开始并行计算，时间:", start_time))
  message(paste("总迭代次数:", ncol(daily_station_long), 
                "| 使用核心数:", ncores))
  
  
  MI_all <- foreach(j = 1:ncol(daily_station_long),
                    .combine = rbind,
                    .packages = c("dplyr", "raster", "entropy", "tidyr", "purrr"),
                    .export = c("Discretization", "optimized_calculate_overlap",
                                "loc_candidate_pts", "daily_station_long", 
                                "windows", "wids", "na_ranges"),
                    .multicombine = TRUE,
                    .maxcombine = 1000,
                    .errorhandling = "pass") %dopar% {
                      
                      # 进度提示（每1000次迭代）
                      if (j %% 1000 == 0) {
                        message(sprintf("正在处理第 %d/%d 个站点 (已用时: %.1f 分钟)",
                                        j, ncol(daily_station_long),
                                        difftime(Sys.time(), start_time, units = "mins")))
                      }
                      
                      tryCatch({
                        gauge.sp.ext1 <- as(extent(loc_candidate_pts[j,]), "SpatialPolygons")
                        crs(gauge.sp.ext1) <- '+proj=utm +zone=10 +datum=WGS84'
                        gauge.sp.ext1  = buffer(gauge.sp.ext1, windows[wids])
                        
                        loc_candidate_pts_ext1 = crop(loc_candidate_pts, gauge.sp.ext1)
                        daily_long_ext1 <- daily_station_long %>%
                          dplyr::select(-1) %>% 
                          dplyr::select(!!as.character(loc_candidate_pts[j, 'FID']$FID),
                                        !!(as.character(loc_candidate_pts_ext1$FID)))
                        
                        if (dim(daily_long_ext1)[2] > 1) {
                          main_station <- names(daily_long_ext1)[1]
                          main_range <- na_ranges[[main_station]]
                          
                          station_overlaps <- sapply(names(daily_long_ext1), function(name) {
                            if (name == main_station) return(Inf)
                            optimized_calculate_overlap(name, main_range, na_ranges)
                          })
                          
                          station_df <- data.frame(
                            name = names(daily_long_ext1),
                            overlap = station_overlaps,
                            stringsAsFactors = FALSE
                          ) %>%
                            dplyr::filter(overlap != Inf) %>%
                            dplyr::arrange(overlap)
                          
                          # 二分法筛选实现（与原始代码相同）
                          # 第一步：删除重叠度为0的站点
                          if (nrow(station_df) > 0 && any(station_df$overlap == 0)) {
                            remove_station <- station_df %>% 
                              filter(overlap == 0) %>% 
                              pull(name)
                            
                            daily_long_ext1 <- daily_long_ext1 %>% 
                              dplyr::select(-all_of(remove_station))
                            station_df <- station_df %>% 
                              filter(overlap != 0)
                          }
                          
                          # 初始化终止条件检查
                          if (nrow(station_df) == 0) {
                            warning("所有站点重叠度均为0")
                            break
                          }
                          
                          if (ncol(daily_long_ext1) <= 1) {
                            warning("只剩主站点，无法继续处理")
                            break
                          }
                          
                          # 第二步：精确二分法实现
                          if (nrow(station_df) > 0) {
                            # 按重叠度降序排序（高重叠度在前）
                            station_df_sorted <- station_df %>% 
                              arrange(desc(overlap)) %>%
                              dplyr::mutate(index = row_number())
                            
                            total_sta <- nrow(station_df_sorted)
                            
                            # 二分搜索参数
                            low_idx <- 1
                            high_idx <- total_sta
                            best_idx <- NULL
                            best_count <- 0
                            iter_count <- 0
                            max_iter <- 50  # 防止无限循环
                            
                            # 精确搜索临界点
                            while (low_idx <= high_idx && iter_count < max_iter) {
                              # 终止条件检查
                              if (ncol(daily_long_ext1) <= 1) {
                                warning("只剩主站点，二分法终止")
                                break
                              }
                              
                              if (nrow(station_df_sorted) == 0) {
                                warning("没有可用的站点，二分法终止")
                                break
                              }
                              
                              # 每次迭代增加计数
                              iter_count <- iter_count + 1
                              mid_idx <- floor((low_idx + high_idx) / 2)
                              
                              # 获取前mid_idx个站点
                              candidate_stations <- station_df_sorted %>% 
                                filter(index <= mid_idx) %>% 
                                pull(name)
                              
                              # 计算非NA行数
                              candidate_data <- daily_long_ext1 %>% 
                                dplyr::select(all_of(candidate_stations))
                              non_na_count <- nrow(na.omit(candidate_data))
                              
                              # cat(paste("迭代:", iter_count, 
                              #           "| 测试站点数:", mid_idx, 
                              #           "| 非NA行数:", non_na_count,
                              #           "| 搜索范围[", low_idx, ",", high_idx, "]\n"))
                              
                              # 情况1：满足条件 - 尝试更多站点
                              if (non_na_count >= 365) {
                                best_idx <- mid_idx
                                best_count <- non_na_count
                                # cat(paste(">> 满足条件 - 尝试增加站点数\n"))
                                
                                # 如果还有更多站点可测试
                                if (mid_idx < high_idx) {
                                  low_idx <- mid_idx + 1
                                } else {
                                  break  # 已经测试最大范围
                                }
                              } 
                              # 情况2：不满足条件 - 尝试更少站点
                              else {
                                # cat(paste(">> 不满足 - 尝试减少站点数\n"))
                                
                                # 如果之前有满足条件的记录，尝试中间点
                                if (!is.null(best_idx)) {
                                  # 移动到中间位置
                                  if (mid_idx > best_idx) {
                                    high_idx <- floor((best_idx + mid_idx) / 2)
                                  } else {
                                    high_idx <- mid_idx - 1
                                  }
                                } else {
                                  # 如果没有满足条件的记录，减少测试站点数量
                                  high_idx <- mid_idx - 1
                                }
                              }
                              
                              # 当搜索范围很小时进行强制收敛
                              if (high_idx - low_idx <= 1) {
                                # 测试高位边界
                                if (high_idx > low_idx) {
                                  high_stations <- station_df_sorted %>% 
                                    filter(index <= high_idx) %>% 
                                    pull(name)
                                  high_data <- daily_long_ext1 %>% 
                                    dplyr::select(all_of(high_stations))
                                  high_count <- nrow(na.omit(high_data))
                                  
                                  if (high_count >= 150) {
                                    best_idx <- high_idx
                                    best_count <- high_count
                                  }
                                }
                                
                                # 测试低位边界
                                low_stations <- station_df_sorted %>% 
                                  filter(index <= low_idx) %>% 
                                  pull(name)
                                low_data <- daily_long_ext1 %>% 
                                  dplyr::select(all_of(low_stations))
                                low_count <- nrow(na.omit(low_data))
                                
                                if (low_count >= 150 && (is.null(best_idx) || low_idx > best_idx)) {
                                  best_idx <- low_idx
                                  best_count <- low_count
                                }
                                
                                break
                              }
                            }
                            
                            # 第三步：应用最佳结果
                            if (!is.null(best_idx)) {
                              # 获取最终站点集合
                              final_stations <- station_df_sorted %>% 
                                filter(index <= best_idx) %>% 
                                pull(name)
                              
                              # 计算实际阈值（最后入选站点的重叠度）
                              threshold <- station_df_sorted %>% 
                                filter(index == best_idx) %>% 
                                pull(overlap)
                              
                              # cat(paste("\n最终方案: 保留前", best_idx, "个站点",
                              #           "| 非NA行数:", best_count,
                              #           "| 临界重叠度:", threshold, "\n"))
                              
                              # 删除非核心站点
                              remove_stations <- setdiff(names(daily_long_ext1), c(final_stations))
                              if (length(remove_stations) > 0) {
                                daily_long_ext1 <- daily_long_ext1 %>% 
                                  dplyr::select(-all_of(remove_stations))
                              }
                              station_df <- station_df_sorted %>% 
                                filter(name %in% final_stations)
                            } 
                          }
                          
                          # 最终校验
                          final_na_count <- nrow(na.omit(daily_long_ext1))
                          if (ncol(daily_long_ext1) <= 1) {
                            warning("警告: 最终只剩主站点")
                          } else if (final_na_count >= 150) {
                            message("成功: 保留", ncol(daily_long_ext1)-1, "个站点 | 非NA行:", final_na_count)
                          } else {
                            warning("警告: 仅", final_na_count, "行非NA数据 | 站点数:", ncol(daily_long_ext1)-1)
                          }
                          
                          
                          print(paste0(#'remove sta: ',remove_station,
                            ' remain sta n: ',dim(daily_long_ext1)[2]))
                          
                          daily_long_ext1_disc <- daily_long_ext1 %>% 
                            mutate_all(Discretization) %>% 
                            .[complete.cases(.), ]
                          
                          length_data <- dim(daily_long_ext1_disc)[1]
                          
                          if (length_data > 4 && sum(daily_long_ext1_disc) != 0) {
                            pts_cadidate_MI <- mi.empirical(daily_long_ext1_disc, unit = "log2") %>% 
                              data.frame() %>% 
                              rename_with(~ 'mutual_Info') %>% 
                              mutate(FID = as.character((loc_candidate_pts[j,])$FID))
                            
                            pts_cadidate_JEN <- entropy::entropy(as.matrix(daily_long_ext1_disc), unit = "log2") %>% 
                              data.frame() %>% 
                              rename_with(~ 'joint_E') %>% 
                              mutate(FID = as.character((loc_candidate_pts[j,])$FID))
                          } else {
                            pts_cadidate_MI <- data.frame(
                              mutual_Info = NA,
                              FID = as.character((loc_candidate_pts[j,])$FID))
                            
                            pts_cadidate_JEN <- data.frame(
                              joint_E = NA,
                              FID = as.character((loc_candidate_pts[j,])$FID))
                          }
                        } else {
                          pts_cadidate_MI <- data.frame(
                            mutual_Info = 0,
                            FID = as.character((loc_candidate_pts[j,])$FID))
                          
                          pts_cadidate_JEN <- data.frame(
                            joint_E = 0,
                            FID = as.character((loc_candidate_pts[j,])$FID))
                        }
                        
                        # 合并结果
                        inner_join(pts_cadidate_MI, pts_cadidate_JEN, by = "FID") %>% 
                          dplyr::mutate(error_message = NA_character_)  
                        
                      }, error = function(e) {
                        # 错误处理
                        data.frame(
                          mutual_Info = NA,
                          joint_E = NA,
                          FID = if (exists("loc_candidate_pts") && nrow(loc_candidate_pts) >= j) {
                            as.character(loc_candidate_pts[j,]$FID)
                          } else paste0("Error_", j),
                          error_message = conditionMessage(e)
                        )
                      })
                    }
  
  # 停止并行集群
  stopCluster(cl)
  registerDoSEQ()  # 
  
  gc()  # 
  
  # 计算结束时间
  end_time <- Sys.time()
  elapsed_time <- difftime(end_time, start_time, units = "mins")
  message(paste("计算完成! 总用时:", round(elapsed_time, 1), "分钟"))
  
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
              paste0(data.dir,'/MI/1900-22_sample_entropy_by_daily.csv'),row.names = F,
              col.names=TRUE,sep=",",quote=F)
  print('write successful')
}


