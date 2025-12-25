# Author：Jiajia Su
# Date：2025-12-02
'create figure 2 for paper 
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


get_circle_plot <- function(gaugeDen.df){
  pie.Wrold <- gaugeDen.df %>% 
    
    group_by(CONTINENT) %>% 
    # dplyr::mutate(density_ind = density/tile_area) %>% 
    dplyr::summarise( totalnums  = n(),
                      Satisfy = length(density_delta[density_delta <0] )/totalnums,
                      Dissatisfy = length(density_delta[density_delta >=0] )/totalnums) %>% 
    # mutate(Continent = 0) %>% 
    tidyr::pivot_longer(cols = c("Satisfy", "Dissatisfy")) %>% 
    dplyr::mutate(CONTINENT = factor(CONTINENT,
                                     levels = level_continents,
                                     labels = label_continents),
                  WMO_class =0)
  
  pie.Continent <- gaugeDen.df %>% 
    group_by(CONTINENT,WMO_class) %>% 
    dplyr::summarise( totalnums  = n(),
                      Satisfy = length(density_delta[density_delta <0] )/totalnums,
                      Dissatisfy = length(density_delta[density_delta >=0] )/totalnums )%>%
    # rbind(pie.Wrold) %>% 
    tidyr::pivot_longer(cols = c("Satisfy", "Dissatisfy"))%>% 
    dplyr::mutate(CONTINENT = factor(CONTINENT,
                                     levels = level_continents,
                                     labels =  label_continents ))
  
  library(tidyverse)
  library(viridis)
  names(pie.Wrold)
  names(pie.Continent)
  circle_chart_data <- pie.Wrold %>% 
    dplyr::select(names(pie.Continent)) %>% 
    rbind(pie.Continent) %>% 
    dplyr::mutate(value = value*100) %>% 
    filter(WMO_class!=8)
  
  # Set a number of 'empty bar' to add at the end of each group
  empty_bar <- 2
  nObsType <- nlevels(as.factor(circle_chart_data$name))
  to_add <- data.frame( matrix(NA, 
                               empty_bar*nlevels(circle_chart_data$CONTINENT)*nObsType, 
                               ncol(circle_chart_data)) )
  colnames(to_add) <- colnames(circle_chart_data)
  to_add$CONTINENT <- rep(levels(circle_chart_data$CONTINENT), 
                          each=empty_bar*nObsType )
  circle_chart_data <- rbind(circle_chart_data, to_add)
  circle_chart_data <- circle_chart_data %>% arrange(CONTINENT, WMO_class)
  circle_chart_data$id <- rep( seq(1, nrow(circle_chart_data)/nObsType) , each=nObsType)
  
  # Get the name and the y position of each label
  label_data <- circle_chart_data %>% 
    group_by(id, WMO_class) %>% 
    dplyr::summarize(tot=sum(value))
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust <- ifelse( angle <= -90, 1, 0)
  # flip angle BY to make them readable
  label_data$angle <- ifelse(angle <= -90, angle+180, angle)
  
  # Get the value and the y position of each label
  value_data <- circle_chart_data %>% 
    group_by(CONTINENT,WMO_class) %>%
    dplyr::filter(value == max(value) | is.na(value)) %>%
    ungroup() %>%
    group_by(id, value,name) %>%
    dplyr::summarize(tot=sum(value))
  
  
  number_of_bar <- nrow(value_data)
  angle <- 90 - 360 * (value_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  value_data$hjust <- ifelse( angle <= -90, 1, 0)
  # flip angle BY to make them readable
  value_data$angle <- ifelse(angle <= -90, angle+180, angle)
  #satisfy value need to process the position
  value_data <- value_data %>% 
    mutate(pos = ifelse((name=='Satisfy' & value<70 & value>50), value+30,value),
           pos = ifelse((name=='Satisfy' & value==50 ), value+50, pos),
           pos = ifelse((name=='Satisfy' & value>70 ), value-10,pos))
  #add 30 which is position adjust parameter
  
  # prepare a data frame for base lines
  base_data <- circle_chart_data %>% 
    group_by(CONTINENT) %>% 
    dplyr::summarize(start=min(id), end=max(id) - empty_bar) %>% 
    rowwise() %>% 
    dplyr::mutate(title=mean(c(start, end)))
  
  # prepare a data frame for grid (scales)
  grid_data <- base_data
  grid_data$end <- grid_data$end[ c( nrow(grid_data), 
                                     1:nrow(grid_data)-1)] + 1
  grid_data$start <- grid_data$start - 1
  grid_data <- grid_data[-1,]
  
  
  colors_4<-c("#156077","#f46f20")
  # Make the plot
  p <- ggplot(circle_chart_data) +      
    
    # Add the stacked bar
    geom_bar(aes(x=as.factor(id), y=value, fill=name), 
             stat="identity", alpha=0.8,
             position = position_stack(reverse = TRUE)) +
    # scale_fill_viridis(discrete=TRUE) +
    # ggsci::scale_fill_npg() +
    scale_fill_manual(values = colors_4)+
    
    # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
    # geom_segment(aes(x = -1, y = 0, xend = 0, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.4 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 25, xend = start, yend = 25), colour = "grey", alpha=1, size=0.4 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha=1, size=0.4 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 75, xend = start, yend = 75), colour = "grey", alpha=1, size=0.4 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, 
                                     yend = 100), colour = "grey", 
                 alpha=1, size=0.4 , inherit.aes = FALSE ) +
    
    #add same line after remove the y axis test
    geom_segment(aes(x = 59, y = 100, xend = 60, 
                     yend = 100), colour = "grey", 
                 alpha=1, size=0.4 , inherit.aes = FALSE ) +
    geom_segment(aes(x = 59, y = 75, xend = 60, 
                     yend = 75), colour = "grey", 
                 alpha=1, size=0.4 , inherit.aes = FALSE ) +
    geom_segment(aes(x = 59, y = 50, xend = 60, 
                     yend = 50), colour = "grey", 
                 alpha=1, size=0.4 , inherit.aes = FALSE ) +
    geom_segment(aes(x = 59, y = 20, xend = 60, 
                     yend = 20), colour = "grey", 
                 alpha=1, size=0.4 , inherit.aes = FALSE ) +
    geom_segment(aes(x = 59, y = 0, xend = 60, 
                     yend = 0), colour = "grey", 
                 alpha=1, size=0.4 , inherit.aes = FALSE ) +
    
    
    ylim(-80,max(label_data$tot, na.rm=T)+85) +
    theme_minimal() +
    labs(title="",fill='')+
    theme(
      legend.position = c(0.9,0.18),#"none",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.text=element_text(size = 8),
      plot.margin = unit(rep(-1,4), "cm") ,
      text=element_text(family = 'Arial',face='bold')
    ) +
    coord_polar() +
    
    #add large value indicate on each bar
    geom_text(data = subset(value_data), 
              aes(x=as.factor(id),y=(pos-35),
                  label=round(value, digits = 1),hjust=hjust),
              color='white',size=3, angle= value_data$angle,fontface="bold", 
              inherit.aes = FALSE,family = 'Arial')+
    
    
    # Add labels on top of each bar
    geom_text(data=label_data, 
              aes(x=id, y=tot+10, label=factor(WMO_class,
                                               levels = c( '0','1','2','3','4','5','6','7'),
                                               labels = c('ALL','Plains','Hilly','Mountains',
                                                          'Coastal','Islands','Urban',
                                                          'Polar/Arid')), 
                  hjust=hjust),
              color="black", fontface="bold",alpha=0.6, family = 'Arial',
              size=3.5, angle= label_data$angle, inherit.aes = FALSE ) +
    
    # Add base line information
    geom_segment(data=base_data, 
                 aes(x = start, y = -5, xend = end, yend = -5), 
                 colour = "black", alpha=0.8, size=0.6 , 
                 inherit.aes = FALSE )  +
    geom_text(data=base_data, 
              aes(x = title, y = -18, label=CONTINENT), 
              hjust=c(0.5,0.8,0.8,0.5,0.1,0.2), colour = "black",family = 'Arial', 
              alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)
  
  return(p)
}

get_bubble_pts_plot <- function(gaugeDen.df){
  order_continents <- c("Asia", "Europe","Africa","Oceania","Norther\n America",
                        "South\n America")
  label_continents <- c("AS", "EU","AF","OC","NA",
                        "SA")
  point.Continent <- gaugeDen.df %>% 
    #removing na in continent
    drop_na(CONTINENT) %>%
    dplyr::mutate(density_ind = density/tile_area) %>% 
    filter(WMO_class != 8) %>% 
    dplyr::mutate(CONTINENT = factor(CONTINENT,
                                     levels = level_continents,
                                     labels = label_continents))
  
  point.Continent_mean <- point.Continent %>% 
    group_by(WMO_class,CONTINENT) %>% 
    dplyr::summarise( 
      mean_den_ind = mean(density_ind))
  point.WMO_class <- point.Continent %>% 
    filter(!duplicated(WMO_class)) %>% 
    dplyr::select(WMO_class,baseline) %>% 
    dplyr::mutate(label = c('Recommended by WMO','','','','','',''))
  
  data_text<-data.frame(WMO_class=c('1','2','3','4','5','6','7'),
                        label = c( 'Plains','Hilly','Mountains',
                                   ' Coastal','Islands','Urban',
                                   'Polar/Arid'))
  
  data_text <- within(data_text, WMO_class <- factor(WMO_class, 
                                                     levels = c( '1','2','3','4','5','6','7')))
  p2 <- power_trans(1/3)
  p2$transform(1:7)
  fig <- ggplot2::ggplot(subset(point.Continent,density_ind>0)) +
    
    geom_jitter(aes(y = density_ind*1000, x = CONTINENT,
                    size = density_ind*1000,#tile_area  density_delta
                    # alpha = (density_delta < 0),
                    colour = CONTINENT),alpha=0.4,
                position = ggplot2::position_jitter(seed = 1L, width = 0.15, height = 0))+
    
    #plot WMO creteria in rect and line
    geom_rect(data = point.WMO_class,
              aes(xmin = 0,xmax = 7 ,
                  ymin = baseline*1000 , ymax = Inf ), fill = "darkgreen",
              colour = NA, alpha = 0.1) +
    geom_hline(data = point.WMO_class,aes( yintercept = (baseline)*1000) ,
               colour = "darkgreen", 
               linetype = "dashed",size=1.8,
               show.legend =NA) +
    geom_text(data = point.WMO_class,aes(y = baseline*1000+0.004*1000, 
                                         x = 3.5,
                                         label=label)
              , colour = "darkgreen",fontface="bold",family = 'B',
              size = 5, #label = "Recommended by WMO",
              alpha = 0.95)+
    # hjust = 1, vjust = -0.2)+
    #plot mean value
    geom_point(data = point.Continent_mean,aes(y = mean_den_ind*1000, x = CONTINENT, 
                                               fill = CONTINENT),
               shape = 23, colour = "black", size = 5.8,stroke = 1.9)+
    geom_point(data = point.Continent_mean,aes(y = mean_den_ind*1000, x = CONTINENT),
               shape = 5, colour = "white", size = 5.3,stroke = 0.6)+    
    facet_wrap(~WMO_class, nrow = 1 )+
    
    scale_y_continuous(expand=c(0,0),
                       limits = c(0.00001, 0.085)*1000, #0.07
                       breaks = c(0.00001,0.001,0.005,0.015,
                                  0.03,0.06)*1000,
                       minor_breaks = NULL,
                       labels = scales::label_number(accuracy = 0.001*1000),
                       trans = p2) +
    scale_x_discrete(position = "bottom") +
    ggsci::scale_colour_npg(name ="") +#
    ggsci::scale_fill_npg(name ="") +#
    scale_alpha_discrete(range = c(0.3, 0.95)) +
    scale_size_continuous(range = c(2, 12),
                          name =""#
    ) +
    labs(x = "", y = expression(paste("Gauge density (stations per 1000km"^"2", ")")))+
    ggplot2::theme_minimal(base_size = 7) +
    theme_bw()+
    theme(legend.position=c(0.93,0.7),#legend.direction="horizontal",
          legend.background=element_rect(I(0),linetype=1),
          legend.key=element_rect(fill = NA, color = NA),
          legend.spacing.y = unit(0.0, 'cm'),
          legend.spacing.x = unit(0.0, 'cm'),
          legend.title=element_text(size=20),
          legend.text=element_text(size = 18),
          panel.grid.major.x = element_blank(),
          panel.border = element_blank(),
          plot.caption = element_text(hjust = 0, vjust = 45,size=25), 
          axis.text.x = element_text(face = "bold",size = 18,angle = 90,vjust= 0.5),
          axis.text.y = element_text(face = "bold",size = 18),
          axis.title.y = element_text(face = "bold",size = 24),
          # plot.margin = margin(t = 0, b = 0.9, r = 0.5, unit = "in"),
          strip.background = element_rect(
            color = "darkgreen", fill = "darkgreen"),
          strip.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          panel.spacing = unit(0.58,"lines"),
          #设置xy轴线
          axis.line = element_line(colour = "black",size = 1),
          text = element_text(family = 'B',face='bold')) +
    
    guides(alpha = "none", colour = "none",size='none',fill='none')+
    # add wmo zone in the top of point
    geom_text(data=data_text, x=2,y=0.445*9.6,
              mapping=aes(label=label),fontface="bold",family = 'B',
              hjust=c(0,-0.2,0.05,0.1,0.1,-0.1,0.1),
              nudge_x=0.1,nudge_y=0.1,size=8)
  return(fig)
}

# figure 2-a ------------------------------------------------------------
gaugeDen.df <- fread(paste0(data.dir,'/precipitation_gauges/fig/all_loc_gauge_density_1degree_period_1900_urban0.025.csv'))


level_continents <- c("Asia", "Europe","Africa","Oceania","North America",
                      "South America")
order_continents <- c("Asia", "Europe","Africa","Oceania","Norther\n America",
                      "South\n America")
label_continents <- c("AS", "EU","AF","OC","NA",
                      "SA")
figure2a <- get_circle_plot(gaugeDen.df)

# figure 2-b ------------------------------------------------------------
gaugeDen.df <- fread(paste0(data.dir,'/precipitation_gauges/fig/valid_loc_gauge_density_1degree_period_1900_urban0.025.csv'))
figure2b <- get_circle_plot(gaugeDen.df)

# figure 2-c ------------------------------------------------------------
gaugeDen.df <- fread(paste0(data.dir,'/precipitation_gauges/fig/all_loc_gauge_density_1degree_period_1900_urban0.025.csv'))

figure2c <- get_bubble_pts_plot(gaugeDen.df)

