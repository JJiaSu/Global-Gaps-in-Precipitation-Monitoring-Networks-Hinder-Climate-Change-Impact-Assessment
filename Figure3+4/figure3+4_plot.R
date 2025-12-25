# Author：Jiajia Su
# Date：2025-12-02
'create figure 3,4 for paper 
Global Gaps in Precipitation Monitoring Networks Hinder Climate Change Impact Assessment'
library(tidyverse)
library(ggplot2)
library(sp)
library(sf)
library(rgdal)
library(dplyr)
library(plyr)
library(data.table)
library(ggsci)
library(raster)
library(ggsignif)
library(ggsci)
library(ggforce)

#set your working directory
data.dir = "F:/sujiajia_2/data"
#import continet boundary
continent.shp<-readOGR(paste0(data.dir,"/globalCountry/continent_shp/continents_no_smallIland.shp"))
# projection(continent.shp) = '+proj=longlat +datum=WGS84 +no_defs '

#remove the Antarctica
continent.shp <- subset(continent.shp, OBJECTID!= 8) 

global.shp<-readOGR(paste0(data.dir,"/globalCountry/countries.shp"))
proj4string(global.shp) <- proj.geo
#remove the Antarctica
global.shp <- subset(global.shp, OBJECTID!= 61)

greedy_rank <- fread(paste0(data.dir,'/precipitation_gauges/fig/sample_data/greedy_rank.csv'))

windows <- c(1)
names.ls <- c(
              'percent_rank_level_EWM_soc_126',
              'percent_rank_level_EWM_soc_585','percent_rank_level_EWM_His')
geo.map_1_fig.ls = list()


library(RColorBrewer)
my.color <- brewer.pal(9, "Blues")[-1]  #RdYlBu

class_plot <- c(
                'percent_rank_level_EWM_soc_126',
                'percent_rank_level_EWM_soc_585',
                'percent_rank_level_EWM_His')
class_label <- c(
                 'EWM_soc_126',
                 'EWM_soc_585','EWM_His')
#plot map with bar, left bottom fig
label_continents <- c('WD',"AS", "EU","AF","OC","NA",
                      "SA")
level_continents <- c('World',"Asia", "Europe","Africa","Oceania","North America",
                      "South America")
# plot geographic map of PSNG ---------------------------------------------

#calculate the decrease or increase
greedy_rank_long <- greedy_rank %>% 
  dplyr::select(#stationID,
    CONTINENT,
    percent_rank_level_EWM_His,
    percent_rank_level_EWM_soc_126,
    percent_rank_level_EWM_soc_585) %>% 
  set_names('CONTINENT','History','soc_126','soc_585') %>% 
  pivot_longer(cols = !c(CONTINENT), 
               names_to = "class",
               values_to = "value") %>% 
  dplyr::mutate(CONTINENT = factor(CONTINENT,
                                   levels = level_continents),
                class = factor(class,
                               levels = c('History','soc_126','soc_585')))

#需要计算fut/soc减去his的百分比
# 世界级分析
temp_bar.world.plot <- greedy_rank_long %>% 
  group_by(class, value) %>% 
  dplyr::summarise(percent = n()/nrow(greedy_rank) * 100) %>% 
  ungroup() %>%
  mutate(class = factor(class, levels = c("History", 
                                          'soc_126','soc_585'))) %>%
  arrange(value, class)

change.bar.world.data <- temp_bar.world.plot %>%
  dplyr::mutate(CONTINENT='WD') %>% 
  group_by(CONTINENT,value) %>%
  dplyr::mutate(
    base_percent = percent[class == "History"],
    change_abs = ifelse(class == "History", 0, percent - base_percent),
    direction = ifelse(change_abs >= 0, "increase", "decrease")
  ) %>%
  ungroup() %>%
  filter(class != "History")

# 大洲级分析
temp_bar.continent.plot <- greedy_rank_long %>% 

  group_by(CONTINENT, class, value) %>% 
  dplyr::summarise(percent = n()/nrow(greedy_rank) * 100) %>% 
  ungroup() %>%
  # 补全所有可能的CONTINENT × class × value组合
  tidyr::complete(
    CONTINENT, 
    class = factor(levels = c("History",  'soc_126','soc_585')), 
    value = c(1:8),#factor(levels = c(1:8)), 
    fill = list(percent = 0)
  ) %>%
  mutate(class = factor(class, levels = c("History", 
                                          'soc_126','soc_585'))) %>%
  arrange(CONTINENT, value, class) 


change.bar.continent.data <- temp_bar.continent.plot %>%
  group_by(CONTINENT, value) %>%
  dplyr::mutate(
    base_percent = percent[class == "History"],
    change_abs = ifelse(class == "History", 0, percent - base_percent),
    direction = ifelse(change_abs >= 0, "increase", "decrease")
  ) %>%
  ungroup() %>%
  filter(class != "History") %>% 
  dplyr::mutate(CONTINENT = factor(CONTINENT,
                                   levels = level_continents,
                                   labels = label_continents),
  ) %>% 
  dplyr::filter(!(CONTINENT=='WD'))

change.bar.all <- rbind(change.bar.world.data,change.bar.continent.data)

library(scales)#geo.map <-


for (class.p in 1:length(class_plot)) {
  bar.world.plot <- greedy_rank %>%
    dplyr::mutate(idx.CUT = get(class_plot[class.p])) %>%
    filter(!is.na(idx.CUT))
  #plot map with bar, left bottom fig
  label_continents <- c('WD',"AS", "EU","AF","OC","NA",
                        "SA")
  level_continents <- c('World',"Asia", "Europe","Africa","Oceania","North America",
                        "South America")
  
  bar.continent.plot1 <- bar.world.plot %>%
    group_by(CONTINENT) %>%
    dplyr::mutate(totalnums = n()) %>%
    group_by(CONTINENT,idx.CUT) %>%
    dplyr::summarise( lon= mean(lon),
                      lat= mean(lat),
                      percent  = n()/totalnums*100) %>%
    ungroup() %>%
    group_by(CONTINENT) %>%
    filter(!duplicated(idx.CUT))
  
  totalnums <- dim(bar.world.plot)[1]
  bar.world.plot1 <- bar.world.plot %>%
    # dplyr::mutate(totalnums = n()) %>%
    group_by(idx.CUT) %>%
    dplyr::summarise(lon= mean(lon),
                     lat= mean(lat),
                     # percent = n()/15390*100,
                     percent= n()/totalnums*100,
                     CONTINENT = 'World') %>%
    ungroup() %>%
    dplyr::select(CONTINENT,idx.CUT,lon,lat,percent)
  
  
  bar.continent.plot <- rbind(bar.continent.plot1, bar.world.plot1)%>%
    dplyr::mutate(CONTINENT = factor(CONTINENT,
                                     levels = level_continents,
                                     labels = label_continents),
    )
  
  #plot latitude change, right fig
  line.lat <- bar.world.plot %>%
    group_by(lat) %>%
    dplyr::mutate(idx_mean = mean(as.numeric(get(class_plot[class.p]))),
                  idx_sd = sd(as.numeric(get(class_plot[class.p])))) %>%
    filter(!duplicated(lat))
  my.color <- brewer.pal(9, "Blues")[-1]  #RdYlBu
  
  windowsFonts(A=windowsFont('Times New Roman'),
               B= windowsFont('Arial'))
  
  geo.map <-ggplot(data=greedy_rank)+
    geom_raster(aes( x=lon,y=lat,#
                     fill=factor((get(class_plot[class.p])))), interpolate = TRUE,
                size=5,position = "identity")+
    geom_sf(data = as(continent.shp,'sf'),fill='NA',
            colour='black',linewidth=0.2)+
    coord_sf(expand = FALSE)+
    labs(title='',
    fill='Rank',
    tag = "Lowest                                                                                                  Highest")+
    ggplot2::scale_fill_manual(values =c('1'=my.color[1],
                                         '2'=my.color[2],
                                         '3'=my.color[3],
                                         '4'=my.color[4],
                                         '5'=my.color[5],
                                         '6'=my.color[6],
                                         '7'=my.color[7],
                                         '8'=my.color[8]),#values = my.color,#(brewer.pal(9, "YlGnBu")[-1]),#Blues
                               name = "" ,
                               guide = ggplot2::guide_legend(title = "", nrow = 1,byrow = TRUE,
                                                             label.position='bottom'))+
    theme_bw()+  #背景设置为白色
    theme(legend.spacing.x = unit(0, 'cm'),
          # legend.key.spacing.x = unit(0, "pt"),
          legend.key.width=unit(3, 'cm'),#3
          legend.title=element_text(size=11,hjust =-0.15,vjust = 2),
          plot.tag.position = c(0.0, 0.15),
          plot.tag = element_text(vjust =0, size=10,hjust =-0.05))+
    coord_sf(expand = FALSE,clip = "off")+
    theme(panel.grid = element_blank(),
          panel.border = element_rect(size = 0.4),
          plot.title = element_text(hjust = 0.5,vjust = 0.2,size=1.5),
          legend.background=element_rect(I(0),linetype=1),
          legend.spacing.y = unit(0.3, 'cm'),
          legend.key.height=unit(0.8, 'cm'),
          # legend.key.width=unit(3.5, 'cm'),#3.5
          legend.key.size=unit(0.5, 'cm'),
          legend.key = element_rect(colour ="transparent", fill ="NA"),
          legend.title=element_text(size=1.5,hjust =-0.15,vjust = 1),#legend.direction="horizontal",
          axis.text.x =element_text(size=7),
          axis.text.y=element_text(size=7,angle = 90,hjust = 0.5),
          # axis.title.x =element_text(size=0.1),
          # axis.title.y =element_text(size=8),
          axis.title =element_text(size=8),
          legend.text=element_text(size = 6),
          legend.position = "bottom" ,#legend.justification = "center",
          text=element_text(family = 'Arial',face='bold'))+
    scale_y_continuous(name="", breaks =  c(-50,0,50),
                       labels = c('-50°S','0','50°N')
                       ,limits = c(-64,90))+

    scale_x_continuous(name = "", breaks =  c(-100,0,100)
                       ,labels = c('-100°W','0','100°E'))
          
  
   whole.circle.continet <- c('AF', 'AS', 'EU', 'NA',
                             'OC', 'SA')
  geo.circle.bar_fig.ls <- list()
  
  for (circule.bar in 1:6) {
    circle.bar.continent.plot <- bar.continent.plot %>% 
      filter(CONTINENT==whole.circle.continet[circule.bar])
    bar.plot1 <- ggplot(data=circle.bar.continent.plot,
                        aes(x=2,y=percent,fill=factor(idx.CUT, levels = 8:1)))+
      #geom_tile(color = "white") + #
      geom_bar(
        stat="identity", 
        color="NA" ,width = 1.2 )+
      scale_fill_manual( values =c('1'=my.color[1],
                                   '2'=my.color[2],
                                   '3'=my.color[3],
                                   '4'=my.color[4],
                                   '5'=my.color[5],
                                   '6'=my.color[6],
                                   '7'=my.color[7],
                                   '8'=my.color[8]),#rev(my.color),
                         name = "")+
      coord_polar(theta = "y",start = 0,clip = "off")+
      xlim(0.3,2.7)+
      labs(title="",size='')+
      guides(size = "none",fill='none')+
      theme_bw()+  #背景设置为白色
      theme_void()+
      theme(strip.text = element_blank()
            # strip.text = element_text(size = 16,
            #                               vjust = 1.5,family = 'Arial')
      )+
      # coord_sf(clip = "off")+
      geom_text(
        x=0,y=0,aes(label =whole.circle.continet[circule.bar]),
        vjust=-0.05,fontface="bold",family = 'Arial',size=1.5)
    
    geo.circle.bar_fig.ls[[circule.bar]] <- ggdraw() +
      draw_plot(bar.plot1,0,0,1,1)
  }
  names(geo.circle.bar_fig.ls) <- whole.circle.continet
  
  #plot the bar plot indicate the increase or decrease
  count_decimal_places <- function(x) {
    if (is.integer(x)) {
      return(0)
    }
    
    # 将数字转换为字符串
    x_str <- as.character(x)
    
    # 检查是否有小数点
    if (!grepl("\\.", x_str)) {
      return(0)
    }
    
    # 分割字符串并计算小数部分长度
    parts <- strsplit(x_str, "\\.")[[1]]
    if (length(parts) == 2) {
      return(nchar(parts[2]))
    } else {
      return(0)
    }
  }
  geo.change.bar_fig.ls <- list()
  dynamic_widths <- list()
  class.short_plot <- c(
                        'soc_126',
                        'soc_585','')
  whole.circle.continet.change <- c('WD','AF', 'AS', 'EU', 'NA',
                                    'OC', 'SA')
  if((class.p<=2)){
    for (change.bar in 1:7) { #加一个world  5
      change.bar.continent <- change.bar.all %>% 
        filter(CONTINENT==whole.circle.continet.change[change.bar])%>% 
        filter(class==(class.short_plot[class.p]),
               value == 7 | value==8)
      trunc_dec <- function(x, digits = 3) {
        multiplier <- 10^digits
        trunc(x * multiplier) / multiplier
      }
      
      # 计算Y轴的极值
      y_min <-trunc_dec (min(change.bar.continent$change_abs),  3)
      y_max <- trunc_dec(max(change.bar.continent$change_abs), 3)
      
      # 确定Y轴的刻度和范围
      if (abs(y_min) > abs(y_max)) {
        # 负值范围更大
        y_breaks <- c(y_min, 0)
        y_limits <- c(y_min * 1.1, 0)  # 留一点边距
      } else if((abs(y_min) < abs(y_max))) {
        # 正值范围更大
        y_breaks <- c(0, y_max)
        y_limits <- c(0, y_max * 1.1)  # 留一点边距
      } else{
        #相等
        y_breaks <- c(y_min,0, y_max)
      }
      
      # 处理极小数的情况
      if (abs(y_min) < 0.01 && abs(y_max) < 0.01) {
        if(length(y_breaks)==3){
          # 使用科学计数法或更小的刻度
          y_breaks <- c(trunc_dec(y_breaks[1],3),
                        trunc_dec(y_breaks[2],3),
                        trunc_dec(y_breaks[3],3))
        }else{
          # 使用科学计数法或更小的刻度
          y_breaks <- c(trunc_dec(y_breaks[1],3),
                        trunc_dec(y_breaks[2],3))
        }
        
        
      }else if (abs(y_min) < 0.1 && abs(y_max) < 0.1) {
        if(length(y_breaks)==3){
          # 使用科学计数法或更小的刻度
          y_breaks <- c(trunc_dec(y_breaks[1],2),
                        trunc_dec(y_breaks[2],2),
                        trunc_dec(y_breaks[3],2))
        }else{
          # 使用科学计数法或更小的刻度
          y_breaks <- c(trunc_dec(y_breaks[1],2),
                        trunc_dec(y_breaks[2],2))
        }
        
        
      }else{
        y_breaks <- c(trunc_dec(y_breaks[1],1),
                      trunc_dec(y_breaks[2],1))
      }
      
      
      # 定义动态宽度计算函数
      calculate_dynamic_width <- function(y.place, base_width = 0.13) {
        
        # 根据Y轴范围调整宽度
        if (y.place == 1) {
          # 极小的范围，增加宽度
          return(base_width+0.01 )
        } else if (y.place== 2) {
          # 较小的范围，适度增加宽度
          return(base_width+0.02)
        }else if (y.place == 3) {
          # 较小的范围，适度增加宽度
          return(base_width+0.02)
        } else {
          # 正常范围，使用基准宽度
          return(base_width)
        }
      }
      #因为y轴的小数位数问题，所以需要动态定义图的宽度，用于组图
      
      #基于breaks的位数，来确定宽度
      decimal_places <- sapply(y_breaks, count_decimal_places)
      if(max(y_breaks)>10){#大於10的，等同於兩位數
        decimal_places[3]=2
      }
      decimal_place <- max(decimal_places)
      # print(decimal_place)
      # 计算动态宽度
      dynamic_widths[[change.bar]] <- calculate_dynamic_width(decimal_place)
      
      
      
      
      geo.change.bar_fig.ls[[change.bar]] <- ggplot(change.bar.continent, 
                                                    aes(x = factor(value), y = change_abs, 
                                                        fill = direction)) +
        geom_col(position = position_dodge()) +
        # geom_hline(yintercept = 0, linetype = "dashed", color = "gray50",
        #            size=1) +
        scale_fill_manual(values = c("decrease" = "#90BD8C", #1f77b4#003262
                                     "increase" = "#d62728")) +#d62728#7C0902
        labs(title="",x='Rank',y='(%)')+
        {if(change.bar==1)#只在wd的有y轴全称
          labs(title="",x='Rank',y='Change (%)')}+
        
        guides(size = "none",fill='none')+
        theme_minimal()+
        theme(# 移除所有边框和背景
          panel.background = element_blank(),
          plot.background = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          
          # 只保留轴线
          axis.line.x = element_line(color = "black", size = 0.2),
          axis.line.y = element_line(color = "black", size = 0.2),
          
          # 移除刻度线和刻度标签背景
          axis.ticks.length = unit(0, "pt"),
          axis.ticks = element_blank(),
          
          axis.text.x =element_text(size=4.7, margin = margin(t = 0.5, unit = "pt")),
          axis.text.y=element_text(size=4.7,margin = margin(r = 0.5, unit = "pt") ),
          axis.title.x =element_text(size=4.5, margin = margin(t = -0.1, unit = "pt")  ),
          axis.title.y =element_text(size=4.5,margin = margin(r = -0.1, unit = "pt") ))+
         scale_y_continuous(
          breaks = y_breaks
        )+
        scale_x_discrete(breaks = c(7,8))
    }
    
    names(geo.change.bar_fig.ls) <- whole.circle.continet.change
    names (dynamic_widths)<- whole.circle.continet.change
  }
 
  #start plot latitude change
  line.lat.plot <- ggplot(data = line.lat,
                          aes(x = lat, y =idx_mean) ) +
    geom_ribbon(aes(ymin = idx_mean-idx_sd
                    , ymax = idx_mean+idx_sd,fill="G2"), alpha = 0.3) +
    geom_line(linewidth=0.5,aes(color="G2"))+ #,linetype=category
    
    geom_hline(aes(yintercept=4), colour="black", linetype="dashed",
               alpha = 5,linewidth=0.5)+
    theme_bw()+  #背景设置为白色
    theme(panel.background = element_rect(fill = 'transparent'),
          plot.background = element_rect(fill = "transparent",colour = NA),
          panel.border = element_rect(size = 0.5),
          panel.grid = element_blank(),plot.title = element_text(hjust = 0.5,size=1.5),
          legend.position=c(9,9),legend.background=element_rect(I(0),linetype=1),
          axis.text.y =element_text(size=7),
          axis.text.x=element_text(size=7),
          axis.title.x =element_text(size=8),
          axis.title.y =element_text(size=8),
          legend.text=element_text(size = 6),
          text=element_text(family = 'Arial',face='bold')
     )+
    scale_y_continuous(breaks =  c(2,4,6,8),
                       labels = c(2,4,6,8))+

    scale_x_continuous(expand = c(0,0),breaks = c(),# c(-50,0,50),
                       # labels = c('-50°S','0','50°N'),
                       position = "top"
                       ,limits = c(-64,90))+
    coord_flip()+
    labs(x = '',y = '',color="")
  
  library(cowplot)
  library(egg)
  
  size.fig = 0.39
  geo.map_1_fig.ls[[class.p]] <- ggdraw() +
    draw_plot(geo.map+
                labs(title = '',tag='')+
                theme(legend.position = 'none' ,
                      plot.title = element_text(size=0.1),
                      panel.spacing = unit(0, "cm"),
                      plot.margin = ggplot2::margin(t = 0.1,  # 顶部边缘距离
                                                    r = 0,  # 右边边缘距离
                                                    b = 0,  # 底部边缘距离
                                                    l = 0,  # 左边边缘距离
                                                    unit = "cm")),
              0,0,0.85,1)+
    # draw_plot(bar.plot,0.005,0.09,0.27,0.49)+
    
    
    draw_plot(geo.circle.bar_fig.ls$`NA`,-0.03,0.5,size.fig,size.fig)+
    draw_plot(geo.circle.bar_fig.ls$SA,0.06,0.25,size.fig,size.fig)+
    draw_plot(geo.circle.bar_fig.ls$AF,0.24,0.25,size.fig,size.fig)+
    draw_plot(geo.circle.bar_fig.ls$EU,0.20,0.56,size.fig,size.fig)+
    draw_plot(geo.circle.bar_fig.ls$OC,0.47,0.25,size.fig,size.fig)+
    draw_plot(geo.circle.bar_fig.ls$AS,0.61,0.49,size.fig,size.fig)+
    draw_plot(line.lat.plot,0.840,-0.05,0.2,1.02)
  
  
  #2025.9.2 把fut和soc的一度的126和585的拼图改为与his相对的变化百分比
  
  if(class.p<=2){
    x.delta=0.10
    y.delta=0.23
    change.bar.h=0.45
    
    
    
    geo.map_1_fig.ls[[class.p]] <-ggdraw() +
      draw_plot(geo.map+
                  labs(title = '',tag='')+
                  theme(legend.position = 'none' ,
                        plot.title = element_text(size=0.1),
                        panel.spacing = unit(0, "cm"),
                        panel.background = element_rect(fill = 'aliceblue'),
                        plot.margin = ggplot2::margin(t = 0.1,  # 顶部边缘距离
                                                      r = 0,  # 右边边缘距离
                                                      b = 0,  # 底部边缘距离
                                                      l = 0,  # 左边边缘距离
                                                      unit = "cm")),
                0,0,0.85,1)+
      # 添加WD子图及标签
      draw_plot(geo.change.bar_fig.ls$`WD`, 0.0558, 0.128, dynamic_widths[['WD']]+0.01, change.bar.h) +
      draw_label("Global", x = 0.05 + x.delta, y = 0.16 + y.delta, size = 5, fontface = "bold", color = "black") +
      
      # 添加NA子图及标签
      draw_plot(geo.change.bar_fig.ls$`NA`, 0.06, 0.45, dynamic_widths[['NA']], change.bar.h) +
      draw_label("NA", x = 0.05 + x.delta, y = 0.48 + y.delta, size = 5, fontface = "bold", color = "black") +
      
      # 添加SA子图及标签
      draw_plot(geo.change.bar_fig.ls$SA, 0.18, 0.16, dynamic_widths[['SA']], change.bar.h) +
      draw_label("SA", x = 0.18 + x.delta, y = 0.19 + y.delta, size = 5, fontface = "bold", color = "black") +
      
      # 添加AF子图及标签
      draw_plot(geo.change.bar_fig.ls$AF, 0.37, 0.16, dynamic_widths[['AF']], change.bar.h) +
      draw_label("AF", x = 0.37 + x.delta, y = 0.19 + y.delta, size = 5, fontface = "bold", color = "black") +
      
      # 添加EU子图及标签
      draw_plot(geo.change.bar_fig.ls$EU, 0.31, 0.48, dynamic_widths[['EU']], change.bar.h) +
      draw_label("EU", x = 0.31 + x.delta, y = 0.51 + y.delta, size = 5, fontface = "bold", color = "black") +
      
      # 添加OC子图及标签，使用动态宽度
      draw_plot(geo.change.bar_fig.ls$OC, 0.58, 0.17, dynamic_widths[['OC']], change.bar.h) +
      draw_label("OC", x = 0.59 + x.delta, y = 0.19 + y.delta, size = 5, fontface = "bold", color = "black") +
      
      # 添加AS子图及标签
      draw_plot(geo.change.bar_fig.ls$AS, 0.73, 0.43, dynamic_widths[['AS']], change.bar.h) +
      draw_label("AS", x = 0.73 + x.delta, y = 0.46 + y.delta, size = 5, fontface = "bold", color = "black") +
      
      # 添加纬度线图
      draw_plot(line.lat.plot,0.840, -0.04, 0.2, 1)
    
  }
  
 
}


names(geo.map_1_fig.ls) <- names.ls

# 

# plot PSNG at country scale ----------------------------------------------
library(maptools)

grid.pts <- greedy_rank 
coordinates(grid.pts) <- ~ lon + lat
projection(grid.pts) = '+proj=longlat +datum=WGS84 +no_defs '

# #get the country labels
grid.pts <-  grid.pts %>%
  #overlap the points and global shp
  over(.,global.shp[,c('FCNAME','FENAME','OBJECTID','SHAPE_AREA','Continent')]) %>%
  spCbind(grid.pts, .)

greedy_rank_cou <- grid.pts@data


geo.country_1_fig.ls = list()

future.class <- c('percent_rank_level_EWM_His')

for (future.idx in 1:length(future.class)) {
  greedy_rank.map <- greedy_rank_cou %>% 
    group_by(OBJECTID) %>% 
    dplyr::mutate(totalnums = n()) %>% 
    group_by(OBJECTID) %>% 
    dplyr::summarise( 
      n_high_priority = sum(get(future.class[future.idx]) %in% c(7, 8), na.rm = TRUE),
      percent = ifelse(totalnums > 0, 
                       (n_high_priority / totalnums) * 100, 
                       0)) %>% 
    ungroup() %>% 
    dplyr::filter(!duplicated(OBJECTID)) %>% 
    dplyr::select(OBJECTID,percent) %>% 
    as.data.frame()
  
  global.shp1 <- global.shp %>% 
    merge(greedy_rank.map,by='OBJECTID')
  

  aaaa <- ggplot()+
    geom_sf(data = as(global.shp1,'sf' ),aes(fill=percent),
            colour='white',linewidth=0.2)+    
    geom_sf(data = as(continent.shp,'sf'),fill='NA',
            colour='black',linewidth=0.2)+  
    coord_sf(expand = FALSE)+
    scale_fill_distiller(palette ='PuBu',direction = 1,#RdBu  PuBu
                         name='',
                         breaks=c(0,50,100),
                         label=c('0%','50%','100%'))+#RdBu  YlOrBu
    labs(title="",
         tag = "            Proportions of each country covered by PSNG under levels 7–8")+
    theme_bw()+  #背景设置为白色
    theme(panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5,size=1.5),
          panel.border = element_rect(size = 0.5),
          legend.background=element_rect(I(0),linetype=1),
          legend.spacing.y = unit(0.3, 'cm'),
          legend.spacing.x = unit(0.05, 'cm'),
          legend.key.height=unit(0.8, 'cm'),
          legend.key.width=unit(5.3, 'cm'),#5.5
          legend.key.size=unit(0.5, 'cm'),
          legend.title=element_text(size=7,hjust = -2,vjust = 5),
          legend.text=element_text(size = 6),
          legend.position = "bottom",legend.box = "horizontal",
          legend.justification = "center",
          plot.tag.position = c(0.0, 0.15),
          plot.tag = element_text(vjust =0, size=10,hjust =-0.06),
          axis.text.x =element_text(size=7), 
          axis.text.y=element_text(size=7,angle = 90, hjust = 0.5),
          axis.title =element_text(size=8),
          text=element_text(family = 'Arial',face='bold'))+  
    scale_y_continuous(name="", breaks =  c(-50,0,50),
                       labels = c('-50°S','0','50°N'),
                       limits = c(-64,90))+

    scale_x_continuous(name = "", breaks =  c(-100,0,100)
                       ,labels = c('-100°W','0','100°E'))

  # legend_geo.country<-cowplot::get_legend(
  #   aaaa +
  #     theme(legend.key.width = unit(5.2, 'cm'))) 
  
  
  geo.country_1_fig.ls[[future.idx]] <- aaaa+
    labs(title = '',tag='')+
    theme(legend.position = 'none' ,
          plot.title = element_text(size=1.5),
          panel.spacing = unit(0, "cm"),
          plot.margin = ggplot2::margin(t = 0.1,  # 顶部边缘距离
                                        r = 0.2,  # 右边边缘距离
                                        b = 0,  # 底部边缘距离
                                        l = 0,  # 左边边缘距离
                                        unit = "cm"))
  
 
}
names(geo.country_1_fig.ls) <- "percent_rank_level_EWM_His"


# plot heatmap ------------------------------------------------------------

library(grid)
library(gtable)

my_triangle_colourbar <- function(...) {
  guide <- guide_colourbar(...)
  class(guide) <- c("my_triangle_colourbar", class(guide))
  guide
}

guide_gengrob.my_triangle_colourbar <- function(...) {
  # First draw normal colourbar
  guide <- NextMethod()
  # Extract bar / colours
  is_bar <- grep("^bar$", guide$layout$name)
  bar <- guide$grobs[[is_bar]]
  extremes <- c(bar$raster[1], bar$raster[length(bar$raster)])
  # Extract size
  width  <- guide$widths[guide$layout$l[is_bar]]
  height <- guide$heights[guide$layout$t[is_bar]]
  short  <- min(convertUnit(width, "cm",  valueOnly = TRUE),
                convertUnit(height, "cm", valueOnly = TRUE))
  print(paste0(width,height,short))
  # Make space for triangles
  #指定 pos 为 0 在左边添加列，-1（默认）在右边添加列
  guide <- gtable_add_cols(guide, unit(short, "cm"),
                           guide$layout$t[is_bar] -4)#left
  guide <- gtable_add_cols(guide, unit(short, "cm"),
                           guide$layout$t[is_bar]-6)#right
  print(paste0(is_bar))
  # print(paste0('guide$layout',guide$layout))
  # print(paste0('guide$layout$t',guide$layout$t))
  print(paste0(guide$layout$t[is_bar]-6))
  # Draw triangles
  
  left <- polygonGrob(
    x = unit(c(1, 0, 1), "npc"),
    y = unit(c(0, 0.5, 1), "npc"),
    gp = gpar(fill = extremes[1], col = NA)
  )
  right <- polygonGrob(
    x = unit(c(0, 1, 0), "npc"),
    y = unit(c(0, 0.5, 1), "npc"),
    gp = gpar(fill = extremes[2], col = NA)
  )
  
  guide <- gtable_add_grob(
    guide, right,
    t = guide$layout$t[is_bar],
    l = guide$layout$l[is_bar]+1
  )
  guide <- gtable_add_grob(
    guide, left,
    t = guide$layout$t[is_bar] ,
    l = guide$layout$l[is_bar]-1
  )
  
  return(guide)
}

label_continents <- c("AS", "EU","AF","OC","NA",
                      "SA")
level_continents <- c("Asia", "Europe","Africa","Oceania","North America",
                      "South America")
data_text<-data.frame(WMO_zone=c('1','2','3','4','5','6','7'),
                      label = c( 'Plains','Hilly','Mountains',
                                 ' Coastal','Islands','Urban',
                                 'Polar/Arid'))

data_vline <- data.frame(WMO_zone=c('1','2','3','4','5','6','7'),
                         baseline = c(NA,1,1,1,1,1,1 ))

data_text <- within(data_text, WMO_class <- factor(WMO_zone, 
                                                   levels = c( '1','2','3','4','5','6','7')))
future.class <- c('percent_rank_level_EWM_His',
                  'percent_rank_level_EWM_soc_126',
                  'percent_rank_level_EWM_soc_585')
bar.continent <- c()
for (future.idx in 1:length(future.class)) {
  bar.continent.plot <- greedy_rank %>% 
    group_by(CONTINENT,WMO_zone) %>% 
    dplyr::mutate(totalnums = n()) %>% 
    group_by(CONTINENT,WMO_zone,get(future.class[future.idx])) %>% 
    dplyr::summarise( percent  = n()/totalnums*100) %>% 
    ungroup() %>% 
    group_by(CONTINENT,WMO_zone) %>%
    set_names('CONTINENT','WMO_zone','rank','percent') %>% 
    filter(!duplicated(rank)) %>% 
    dplyr::mutate(percent = round(percent, 2),
                  CONTINENT = factor(CONTINENT,
                                     levels = level_continents,
                                     labels = label_continents)) %>% 
    filter(WMO_zone != 8)
  if(future.idx==1){
    bar.continent <- bar.continent.plot
  }else{
    bar.continent <- bar.continent %>% 
      merge(bar.continent.plot,by=c("CONTINENT", "WMO_zone",  "rank" ),all=T)
  }
  
}

bar.continent <- bar.continent%>% 
  set_names("CONTINENT", "WMO_zone",  "rank",'percent_His',
            'percent_soc_126','percent_soc_585') %>% 
  mutate_at(c('percent_His',
              'percent_soc_126','percent_soc_585'),function(x){
                ifelse(is.na(x),0,x)
              }) %>% 
  
  mutate( delta_soc_his_585 = percent_soc_585 - percent_His)

library(ggplot2)
library(dplyr) # easier data wrangling 
library(viridis) # colour blind friendly palette, works in B&W also
# library(Interpol.T) #  will generate a large dataset on initial load
library(lubridate) # for easy date manipulation
library(ggExtra) # because remembering ggplot theme options is beyond me
library(tidyr) 

bar.continent1 <- bar.continent %>% 
  dplyr::mutate_at('delta_soc_his_585', function(x){
    case_when(x < (-10) ~ (-10),
              x > 10 ~ 10,
              TRUE ~ x)
  }) 

#复制原来的delta列，重命名为negetive_，得到正负的lable
bar.continent1 <- bar.continent1[c(names(bar.continent1)[1:6],
                                   rep(names(bar.continent1)[7], c(2)))]%>%  
  rename_at(vars(-c(CONTINENT:delta_soc_his_585)), 
            function(x) {
              paste0('negetive',substring(x,6,17))}
  ) %>% 
  dplyr::mutate_at(vars(-c(CONTINENT:delta_soc_his_585)), function(x){
    ifelse(x>=0,'+','-')
  })

library(gguidance)

bbbb <- ggplot(bar.continent1)+
  geom_tile(aes(CONTINENT,(rank),fill=delta_soc_his_585),color= "grey",
            size=0.1,
            lwd = 0.2,
            linetype = 1) + 
  
  scale_fill_gradient2(name="",
                       low = scales::muted("#003262"),
                       mid = "white",
                       high = scales::muted("#7C0902"),
                       na.value="grey",
                       midpoint = 0,
                       limits=c(-10,10),
                       breaks = c(-7.5,-5,-2.5,0,2.5,5,7.5),
                       labels=c('-7.5%','-5%','-2.5%','0%','2.5%','5%',
                                '7.5%'),#,
                       oob = scales::oob_squish,
                       guide = my_triangle_colourbar()
                       
  )+
  
  geom_text(data=subset(bar.continent,abs(delta_soc_his_585)>=10),
            aes(CONTINENT,rank,label = round(delta_soc_his_585,0)),
            fontface="bold",family = 'Arial',
            color = "white", size = 2.0) +
  
  facet_wrap(~WMO_zone, nrow = 1 )+
  coord_fixed() +
  # scale_y_continuous('Continent')+
  # scale_x_continuous("Priority rank")+
  theme_minimal(base_size = 8)+
  labs(title= (""),
       y ='Priority rank\n',x='Continent')+#\n
  # theme(panel.border = element_rect(fill=NA,color="black", size=1, linetype="solid"))+
  theme(legend.position = "bottom",
        legend.justification = "center",
        # legend.background=element_rect(I(0),linetype=1),
        legend.key=element_rect(fill = NA, color = NA),
        legend.key.width=unit(9.4, 'cm'),#9.8  9.4
        legend.spacing.y = unit(0.3, 'cm'),
        legend.spacing.x = unit(0.0, 'cm'),
        legend.title=element_text(size=10),
        legend.text=element_text(size = 10))+
  # theme_bw()+
  theme(panel.grid.major.x = element_blank(),
        # panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5,size=1.5),
        # plot.caption = element_text(hjust = 0, vjust = 45,size=11), 
        # axis.text = element_text(face = "bold",size = 5),
        axis.text.y = element_text(face = "bold",hjust =-15,size = 7),
        axis.text.x = element_text(face = "bold",size = 6),
        axis.title = element_text(face = "bold",size = 8),
        axis.title.y = element_text(face = "bold",size = 8),
        # plot.margin = margin(t = 0, b = 0.9, r = 0.5, unit = "in"),
        strip.background = element_rect(
          color = "darkgreen", fill = "darkgreen"),
        strip.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.spacing = unit(0.58,"lines"),
        #设置xy轴线
        axis.line = element_line(colour = "black",size = 0.1),
        text = element_text(family = 'Arial',face='bold')) +
  scale_y_continuous(breaks =  c(1:8),
                     limits=c(0.5,9))+#scale_y_continuous
  # add wmo zone in the top of point
  geom_text(data=data_text, x=2,y=9,parse=T,
            mapping=aes(label=label),fontface="bold",family = 'Arial',
            hjust=c(0,-0.2,0.05,0.1,0.1,-0.1,0.05),
            nudge_x=0.1,nudge_y=0.1,size=2.5)

geo.heatmap_fig <- ggdraw() +
  draw_plot(bbbb+
              labs(title = '')+
              theme(legend.position = 'none' ,
                    panel.spacing = unit(0, "cm"),
                    axis.title.y = element_text(face = "bold",size = 8,vjust = 4),
                    plot.margin = ggplot2::margin(t = 0.1,  # 顶部边缘距离
                                                  r = 0.2,  # 右边边缘距离
                                                  b = 0,  # 底部边缘距离
                                                  l = 0,  # 左边边缘距离
                                                  unit = "cm")
              )
            ,0,0,1,1)


legend_geo.heatmap <- get_legend(
  bbbb +
    theme(legend.key.width = unit(9.6, 'cm'))#,"guide-box-bottom"
)
as_ggplot(legend_geo.heatmap) -> legend_geo.heatmap1



# merge figure ------------------------------------------------------------


# figure 3 ----------------------------------------------------------------

library(cowplot)
legend_geo.map <- cowplot::get_legend(
  geo.map +
    theme(legend.key.width = unit(0.95, 'cm'),legend.spacing.y = unit(0.1, 'cm'),
          legend.key.height  = unit(0.2, 'cm'))#,"guide-box-bottom"
)
as_ggplot(legend_geo.map) -> legend_geo.map2

legend_geo.country<-cowplot::get_legend(
  aaaa +
    theme(legend.key.width = unit(1.5, 'cm'),legend.spacing.y = unit(0.1, 'cm'),
          legend.key.height  = unit(0.2, 'cm'))#,"guide-box-bottom"
) 
as_ggplot(legend_geo.country) -> legend_geo.country2

ggpubr::ggarrange(geo.map_1_fig.ls$percent_rank_level_EWM_His,
                  # legend_geo.map2,
                  plot_grid(NULL, legend_geo.map2,
                            rel_widths = c(0.08,1), #align = "h",
                            labels = c('',''), nrow = 1),
                  ggdraw()+draw_plot(geo.country_1_fig.ls$percent_rank_level_EWM_His,
                                     0,0,1,1),
                  # legend_geo.country2,
                  plot_grid(NULL, legend_geo.country2,
                            rel_widths = c(0.04,1), #align = "h",
                            labels = c('',''), nrow = 1),
                  nrow = 4,ncol = 1,#widths = c(1.2,1,1.3,1),
                  heights = c(0.9,0.2,1.03,0.2),
                  labels = c( 
                    paste0('(a) Priority results under historical period'),
                    
                    paste0('       Lowest                                                                                    Highest'), 
                    paste0('(b) Priority results averaged by country'),
                    paste0('         Proportion of each country covered by PSNG values 7 or 8')#Proportions of each country covered by PSNG lower than 7–8
                  ),
                  font.label = list(family = 'Arial',size = 7),hjust=0,
                  vjust=c(1.1,0.35,1.1,0.4),
                  label.x = 0.04,
                  align = "v"
)
ggsave( paste0(data.dir,
               "/precipitation_gauges/fig/sample_data/figure3.png"),
        dpi = 400,
        width = 85, height = 100,units = 'mm')#width = 305, height = 320,

ggsave(  paste0(data.dir,
                "/precipitation_gauges/fig/sample_data/figure3.pdf"),
        dpi = 400,
        width = 85, height = 90,units = 'mm', device = cairo_pdf)

# figure 4 ----------------------------------------------------------------

geo.map4legend = geo.map
legend_geo.map <- get_legend(
  geo.map +
    theme(legend.key.width = unit(1.9, 'cm'),
          legend.key.height = unit(0.2,'cm'),legend.spacing.y = unit(0.1, 'cm'),
          legend.text=element_text(size = 8))#element_text(size = 6)
)
as_ggplot(legend_geo.map) -> legend_geo.map.sankey

legend_geo.heatmap <- cowplot::get_legend(
  bbbb +
    theme(legend.key.width = unit(3, 'cm'),#11
          legend.key.height = unit(0.2,'cm'),legend.spacing.y = unit(0.1, 'cm'),
          legend.text = element_text(size = 8))#,"guide-box-bottom"
)
as_ggplot(legend_geo.heatmap) -> legend_geo.heatmap1


arrange.plot<- ggpubr::ggarrange(
  
  ggpubr::ggarrange(
    #a图geo-map+图列
    geo.map_1_fig.ls$percent_rank_level_EWM_soc_126,
    
    #b图geo-country+图列
    geo.map_1_fig.ls$percent_rank_level_EWM_soc_585,
    
    
    nrow = 1,
    align = "hv",
    labels = c( 
      paste0('(a) Priority results under future SSP1-2.6'),
      paste0('(b) Priority results under future SSP5-8.5')
    ),font.label = list(family = 'Arial',size = 7),hjust=0,
    vjust=c(1.1,1.1),label.x = 0.02),
  
  plot_grid(NULL, legend_geo.map.sankey,
            rel_widths = c( 0.02, 1), align = "hv",
            labels = c('',''), nrow = 1),
  
  #c图heatmap+图列
  plot_grid(geo.heatmap_fig, 
            plot_grid(NULL, legend_geo.heatmap1,
                      rel_widths = c( 0.02, 1), align = "hv",
                      labels = c('',''), nrow = 1),
            rel_heights = c( 1, 0.2), align = "hv",
            labels = c('',''), nrow = 2),
  
  nrow = 3,ncol = 1,widths = c(1,1,1),
  heights = c(1.0,0.2,1.4),
  labels = c( 
    paste0(''),#\U1D463\U1D460
    paste0('          Lowest                                                                                                                                                                                                   Highest'),
    
    paste0('(c) Proportional area differences in physiographic unit rankings (SSP5-8.5 vs. History)')
  ),
  font.label = list(family = 'Arial',size = 7),hjust=0,
  vjust=c(1.3,0.0,1.1),label.x = 0.01,
  align = "v"
)

ggdraw(arrange.plot) +
  # 垂直线段 126
  draw_line(x = c(0.094, 0.094), y = c(0.685, 0.784), 
            color = "black", size = 0.3, lineend = "square") +
  # 水平线段
  draw_line(x = c(0.0389, 0.094), y = c(0.784, 0.784), 
            color = "black", size = 0.3, lineend = "square") +
  
  # 垂直线段 585
  draw_line(x = c(0.5984, 0.5984), y = c(0.685, 0.784), 
            color = "black", size = 0.3, lineend = "square") +
  # 水平线段
  draw_line(x = c(0.5393, 0.5984), y = c(0.784, 0.784), 
            color = "black", size = 0.3, lineend = "square")

# dev.off()
ggsave( paste0(data.dir,
               "/precipitation_gauges/fig/sample_data/figure4.png"),
        dpi = 400,
        width =  165, height = 100,
        units = 'mm')
ggsave(  paste0(data.dir,
                "/precipitation_gauges/fig/sample_data/figure4.pdf"),
         dpi = 400,
         width =  165, height = 100,units = 'mm', device = cairo_pdf)
