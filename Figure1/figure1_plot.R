# Author：Jiajia Su
# Date：2025-12-02

'create figure 1 for paper 
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

# figure 1-a --------------------------------------------------------------


gauge.TS <- fread(paste0(data.dir,'/precipitation_gauges/fig/TS_gauge_by_YEAR&continet_period_1900.csv'))
gauge.TS <- gauge.TS %>% 
  group_by(continet) %>% 
  dplyr::mutate(sum = sum(nums)) %>% 
  arrange(desc(sum))



#for 1900开始
all.area <- ggplot(data = subset(gauge.TS,!is.na(continet)), 
                   aes(x = YEAR)) + 
  #add the grey area to show the nums still working during past 30 years.
  annotate("rect", xmin = 1992, xmax = 2022, ymin = 0, ymax = 12.66,
           colour = "grey", #family = 'Arial',fontface="bold",
           alpha = 0.2)+
  
  # geom_area(aes(fill=reorder(continet,(sum)), y = nums/10000),
  #           alpha=1 ,colour="white",colour="white",size=0.2)+ #,linetype=category
  geom_area(aes(fill=factor(continet,levels = c('Africa','South America',
                                                'Oceania','Asia','Europe','North America')),
                y = nums/10000),
            alpha=1 ,colour="white",colour="white",size=0.2)+ #,linetype=category
  
  geom_vline(aes(xintercept=2007), colour="black", linetype="dashed", 
             alpha = 0.5,linewidth=0.5)+
  annotate("text", x = 1978 , y = 10.1,label = "2007",colour="black",
           size=2,family = 'Arial',fontface="bold")+
  annotate("segment", x = 1984, xend = 2005, y = 10.6, yend = 11.6,
           colour = "black", size = 0.3,family = 'Arial',fontface="bold")+
  
  #add the indicate show the nums still working during past 30 years.
  geom_segment(aes(x=1992,xend=1992,y=12,yend=12.66),
               size = 0.3,family = 'Arial')+#竖线
  geom_segment(aes(x=2022,xend=2022,y=12,yend=12.66),
               size = 0.3,family = 'Arial')+#竖线
  geom_segment(aes(x=1992,xend=2022,y=12.65,yend=12.65),
               size = 0.3,family = 'Arial')+#横线
  annotate("text", x = 2006 , y = 13.3,label = "N=139,901",colour="black",
           size=2,family = 'Arial',fontface="bold")+
  

  labs(x = 'Year',y = expression(paste("Numbers (", 10^4,')')),
       fill="",family = 'Arial',font="bold")+
  theme_bw()+  #背景设置为白色    
  scale_fill_jco()+
  # scale_color_manual('larger than 30 YR', values=c('#0073C2FF', '#EFC000FF')) +
  theme(panel.grid.major=element_line(colour=NA),
        panel.border = element_blank(),
        panel.grid = element_blank(),plot.title = element_text(hjust = 0.5),
        legend.position=c(0.23,0.41),legend.background=element_rect(I(0),linetype=1),
        legend.key.height=unit(0.1, 'cm'),
        legend.key.width=unit(0.2, 'cm'),
        axis.text.x =element_text(size=6), axis.text.y=element_text(size=6),
        axis.title.x =element_text(size=6),axis.title.y =element_text(size=6),
        axis.ticks = element_line(lineend =35,size=0.3),
        axis.ticks.length = unit(0.1, "cm"),
        legend.text=element_text(size = 6),legend.title=element_text(size=7),
        #设置xy轴线
        axis.line = element_line(colour = "black",size = 0.3),
        text=element_text(family = 'Arial',face='bold'))+
  #scale_linetype_manual(values=c("twodash", "solid","dashed","dotdash"))+  #"dotdash"
  scale_y_continuous(
    breaks=c(0,2.5,5,7.5,10,12.4),
    labels = c(0,2.5,5,7.5,10,12.5))+
  scale_x_continuous(breaks=seq(from=1780, to=2022, by=40),
                     limits = c(1880,2022))+#1780
  # guides(fill=guide_legend(title = "", label = F))+
  guides(fill=guide_legend(nrow =3,ncol =2,byrow=TRUE))

valid.area <- ggplot(data = subset(gauge.TS,!is.na(continet)), aes(x = YEAR)) + 

  geom_area(aes(fill=factor(continet,levels = c('Africa','South America',
                                                'Oceania','Asia','Europe','North America')),
                y = nums_valid/10000),
            alpha=1 ,colour="white",colour="white",size=0.2)+ #,linetype=category
  
  theme_bw()+  #背景设置为白色  
  geom_text(aes(y=2.4,x=1893,label=''),
            color='black',size = 3,family = 'Arial',fontface='bold')+
  labs(x = 'Year',y = expression(paste("Numbers (", 10^4,')')),
       fill="Continent")+
  scale_fill_jco()+
  # scale_color_manual('larger than 30 YR', values=c('#0073C2FF', '#EFC000FF')) +
  theme(panel.grid.major=element_line(colour=NA),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA),
        legend.position='NA',
        panel.grid = element_blank(),plot.title = element_text(hjust = 0.5),
        axis.text.x =element_text(size=6), axis.text.y=element_text(size=6),
        axis.title.x =element_text(size=7),axis.title.y =element_text(size=7),
        axis.ticks = element_line(lineend =30,size=0.3),
        axis.ticks.length = unit(0.1, "cm"),
        legend.text=element_text(size = 5),legend.title=element_text(size=6),
        #设置xy轴线
        axis.line = element_line(colour = "black",size = 0.3),
        text=element_text(family = 'Arial',face='bold'))+
  #scale_linetype_manual(values=c("twodash", "solid","dashed","dotdash"))+  #"dotdash"
  scale_y_continuous(
    breaks=c(0,1.5,3))+
  scale_x_continuous(breaks=seq(from=1780, to=2022, by=80),
                     limits = c(1880,2022))+#1780
  # guides(fill=guide_legend(title = "", label = F))+
  guides(fill=guide_legend(nrow =3,ncol =2,byrow=TRUE))

library(cowplot)
time.plot <- cowplot::ggdraw() +draw_plot(all.area,0,0,1,1)+
  draw_plot(valid.area,0.15,0.54,0.55,0.45)

ggsave( paste0(data.dir,"/figure1/TS_gauge_by_YEAR&continet3_period_1900.png"),dpi = 300,
        width = 80, height = 50,units = 'mm') #width = 297, height = 180


# figure 1-b --------------------------------------------------------------

clean_location_sta <- fread(paste0(data.dir,'/gauge1/location/location_station_all_new_1900.csv'),
                            quote = "",header=TRUE,check.names=F)# %>% 

library(ggmap)
library(RColorBrewer)
library(ggfx)
my_colourmap <- colorRampPalette(rev(brewer.pal(n=11,
                                                name = 'RdYlBu')))(32)

clean_location_sta<-clean_location_sta %>% 
  # dplyr::mutate(period_1900 = as.numeric(period_1900),
  #               period_1900 = ifelse(period_1900 >= (100),100,period_1900)) %>% 
  # arrange((period_1900))#desc
  arrange((period))


#图列垂直时加上三角，运行以下代码
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
  print(guide$layout)
  bar <- guide$grobs[[is_bar]]
  extremes <- c(bar$raster[1], bar$raster[length(bar$raster)])
  # Extract size
  width  <- guide$widths[guide$layout$l[is_bar]]
  height <- guide$heights[guide$layout$t[is_bar]]
  short  <- min(convertUnit(width, "cm",  valueOnly = TRUE),
                convertUnit(height, "cm", valueOnly = TRUE))
  # Make space for triangles
  guide <- gtable_add_rows(guide, unit(short, "cm"),
                           guide$layout$t[is_bar] - 1)
  guide <- gtable_add_rows(guide, unit(short, "cm"),
                           guide$layout$t[is_bar])
  
  # Draw triangles
  top <- polygonGrob(
    x = unit(c(0, 0.5, 1), "npc"),
    y = unit(c(0, 1, 0), "npc"),
    gp = gpar(fill = extremes[1], col = NA)
  )
  bottom <- polygonGrob(
    x = unit(c(0, 0.5, 1), "npc"),
    y = unit(c(1, 0, 1), "npc"),
    gp = gpar(fill = 0, col = NA)
  )
  
  # Add triangles to guide, top and bottom
  guide <- gtable_add_grob(
    guide, top,
    t = guide$layout$t[is_bar] - 1,
    l = guide$layout$l[is_bar]
  )
  guide <- gtable_add_grob(
    guide
    , bottom,
    t = guide$layout$t[is_bar] + 1,
    l = guide$layout$l[is_bar]
  )
  
  return(guide)
}


windowsFonts(A=windowsFont('Times New Roman'),
             B= windowsFont('Arial'))#geo.map <- 
geo.map<- ggplot()+

  with_shadow(
    geom_sf(
      data = as(continent.shp,'sf') ,
      fill='#F2EDEF',#aes(fill = status),
      linewidth = 0.2,
      color = "black"
    ),
    sigma = 3,
    x_offset = 13,
    y_offset = 13
  )+
  
  geom_sf(data = as(continent.shp,'sf'),fill='NA',
          colour='black',linewidth=0.4)+
  #coord_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")+
  coord_sf(expand = FALSE)+
  geom_point(data=subset(clean_location_sta),
             aes(x=as.numeric(longitude),y=as.numeric(latitude),
                 color=as.numeric(period)),
             alpha = 0.7,  size=0.1,shape=16,position = "identity")+
  scale_colour_gradientn(colours = my_colourmap,
                         breaks=c(0,25,50,75,100),limits = c(0,100),
                         labels=c(0,25,50,75,'100'),oob = scales::squish,
                         guide = my_triangle_colourbar(),
                         values = scales::rescale(c(0,25,50,75,100)))+
  #zoom Na, china, swis, oceania
  #first, get the rect to indicate
  geom_rect(aes(xmin = -125,xmax = -65,
                ymin = 25 , ymax = 50 ), fill = "NA",
            colour = 'SpringGreen', alpha = 0.3,linewidth=1) +
  geom_rect(aes(xmin = 3.5,xmax = 18 ,
                ymin = 47 , ymax = 55.1), fill = "NA",
            colour = 'Cyan', alpha = 0.3,linewidth=1) +
  geom_rect(aes(xmin = 73, xmax = 135.4 ,
                ymin = 17.5 , ymax = 53.9), fill = "NA",
            colour = 'red', alpha = 0.3,linewidth=1) +
  geom_rect(aes(xmin = 130,xmax = 154 ,
                ymin = -43 , ymax = -20 ), fill = "NA",
            colour = 'purple', alpha = 0.3,linewidth=1) +
  
  #add the label
  annotate("text",label ='(c)',x=-65+4,y=25-5,# x=74,y=-43,
           fontface="bold",family = 'Arial',size=8.7)+
  annotate("text",label ='(d)',x=0-5,y=55+7,# x=74,y=-43,
           fontface="bold",family = 'Arial',size=8.7)+
  annotate("text",label ='(e)',x=110,y=54+7,# x=74,y=-43,
           fontface="bold",family = 'Arial',size=8.7)+
  annotate("text",label ='(f)',x=130-4,y=-43-5,# x=74,y=-43,
           fontface="bold",family = 'Arial',size=8.7)+
  
  labs(title="",color='')+
  annotate('text',x=-165,y=-5,  #定义添加文本和其位置
           label='Duration (year)',angle=90,family = 'Arial',fontface='bold',
           size=7)+
  theme_bw()+  #
  theme(
    panel.background = element_rect(fill = '#9ecae1'),#9ecae1
    panel.border = element_rect(size = 1.3),
    legend.position=c(0.1,0.4),panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5,size=24),
    legend.background=element_rect(I(0),linetype=1),
    legend.spacing.y = unit(0.3, 'cm'),legend.spacing.x = unit(0.0, 'cm'),
    legend.key.height=unit(1.2, 'cm'),legend.key.width=unit(0.8, 'cm'),
    legend.key.size=unit(1.5, 'cm'),
    legend.title=element_text(size=20),#legend.direction="horizontal",
    axis.text.x =element_text(size=20),
    axis.text.y=element_text(size=20,angle = 90,hjust = 0.5),
    axis.title.x =element_text(size=21),
    axis.title.y =element_text(size=21),
    legend.text=element_text(size = 18,hjust =0.2),

    text=element_text(family = 'Arial',face='bold'))+  
  scale_y_continuous(name="", limits = c(-64,90),#limits = c(-100,90),
                     breaks =  c(-50,0,50),expand = c(0, 0),
                     labels = c('-50°S','0','50°N'))+

  scale_x_continuous(name = "", expand = c(0, 0),breaks =  c(-100,0,100)
                     ,labels = c('-100°W','0','100°E'))

ggsave( paste0(data.dir,"/figure1/all_duration_gauge1_period_1900.png"),dpi = 300,
        width = 297, height = 170,units = 'mm')


# figure 1 -c -------------------------------------------------------------
library(terra)
all.sta.vc <- vect(clean_location_sta,geom=c("longitude", "latitude"),crs="4326")

US.ext <- crop(subset(global.shp,FENAME == "United States Of America"),
               extent(-125, -65, 25,50))

library(ggfx)
pts.NA <- terra::intersect(all.sta.vc, terra::vect(US.ext))


geo.map.NA <- ggplot()+
  geom_sf(data = as(global.shp,'sf'),fill='#F2EDEF',
          colour='black',linewidth=0.2)+
  coord_sf(expand = FALSE,clip="on" )+
  with_shadow(
    geom_sf(
      data = as(subset(global.shp,FENAME == "United States Of America"),'sf') ,
      fill='#F2EDEF',
      linewidth = 0.5,
      color = "black"
    ),
    sigma = 3,
    x_offset = 5,
    y_offset = 5
  )+
  geom_point(data=(clean_location_sta %>% 
                     filter(stationUID %in% pts.NA$stationUID)
  ),
  aes(x=as.numeric(longitude),y=as.numeric(latitude),
      color=as.numeric(period)),
  alpha = 0.7,  size=0.1,shape=16,position = "identity")+
  scale_colour_gradientn(colours = my_colourmap,
                         breaks=c(0,25,50,75,100),limits = c(0,100),
                         labels=c(0,25,50,75,'>100'), oob = scales::squish,
                         values = scales::rescale(c(0,25,50,75,100)))+
  labs(title="",color='',tag = 'N=65,794')+
  theme_bw()+  
  theme(
    panel.background = element_rect(fill = '#9ecae1'),
    plot.tag.position = c(0.55, -0.02), plot.tag = element_text(vjust =-1, size=10),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.position='NA',panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.background=element_rect(I(0),linetype=1),
    panel.border = element_rect(color = "SpringGreen", size = 0.5),
    axis.text.x =element_text(size=9.5,face='bold'),
    axis.text.y=element_text(size=9.5,angle = 90,hjust = 0.5,face='bold'),
    axis.title.y =element_text(size=1),
    text=element_text(family = 'Arial'))+  
  scale_y_continuous(name="",limits = c(25,50),breaks=c(30,45),expand = c(0, 0),
                     labels = c('30°N','45°N'))+
  scale_x_continuous(name = "",limits = c(-125, -65),breaks=c(-115,-70),expand = c(0, 0)
                     ,labels = c('-115°W','-70°W'))


ggsave(paste0(data.dir,"/figure1/NA_duration_gauge1_period_1900_dis.png"),
       dpi = 300,
       width = 80, height = 55,units = 'mm') 


# figure 1-d --------------------------------------------------------------
swisa.ext <- subset(global.shp,FENAME == "Federal Republic of Germany")
pts.SW <- terra::intersect(all.sta.vc, terra::vect(swisa.ext))

geo.map.Swis <- ggplot()+
  geom_sf(data = as(global.shp,'sf'),fill='#F2EDEF',
          colour='black',linewidth=0.2)+
  coord_sf(expand = FALSE,clip="on" )+
  with_shadow(
    geom_sf(
      data = as(subset(global.shp,FENAME == "Federal Republic of Germany"),'sf') ,
      fill='#F2EDEF',
      linewidth = 0.5,
      color = "black"
    ),
    sigma = 3,
    x_offset = 5,
    y_offset = 5
  )+
  geom_point(data=(clean_location_sta %>% 
                     filter(stationUID %in% pts.SW$stationUID)
  ),
  aes(x=as.numeric(longitude),y=as.numeric(latitude),
      color=as.numeric(period)),
  alpha = 0.7,  size=0.1,shape=16,position = "identity")+
  scale_colour_gradientn(colours = my_colourmap,
                         breaks=c(0,25,50,75,100),limits = c(0,100),
                         labels=c(0,25,50,75,'>100'), oob = scales::squish,
                         values = scales::rescale(c(0,25,50,75,100)))+
  
  labs(title="",color='',tag = 'N=8,518')+
  theme_bw()+  
  theme(
    panel.background = element_rect(fill = '#9ecae1'),
    plot.tag.position = c(0.55, -0.02), plot.tag = element_text(vjust =-1, size=10),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.position='NA',panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.background=element_rect(I(0),linetype=1),
    panel.border = element_rect(color = "Cyan", size = 0.5),
    axis.text.x =element_text(size=9.5,face='bold'),
    axis.text.y=element_text(size=9.5,angle = 90,hjust = 0.5,face='bold'),
    axis.title.y =element_text(size=1),
    text=element_text(family = 'Arial'))+  
  scale_y_continuous(name="",limits = c(47,55.1),breaks=c(48,54),expand = c(0, 0),
                     labels = c('48°N','54°N'))+
  scale_x_continuous(name = "",limits = c(3.5, 18),breaks=c(5,15),expand = c(0, 0),
                     labels = c('5°E','15°E'))


ggsave(paste0(data.dir,"/figure1/Swis_duration_gauge1_period_1900_dis.png"),dpi = 300,
       width = 80, height = 55,units = 'mm')


# figure 1-e --------------------------------------------------------------
China.ext <-crop(subset(global.shp,FENAME == "Peoples Republic of China"),
                 extent(73, 135.4, 17.5, 53.9))

pts.China <- terra::intersect(all.sta.vc, terra::vect(China.ext))

geo.map.china <- ggplot()+
  geom_sf(data = as(global.shp,'sf'),fill='#F2EDEF',
          colour='black',linewidth=0.2)+
  coord_sf(expand = FALSE,clip="on" )+
  with_shadow(
    geom_sf(
      data = as(subset(global.shp,FENAME == "Peoples Republic of China"),'sf') ,
      fill='#F2EDEF',
      linewidth = 0.5,
      color = "black"
    ),
    sigma = 3,
    x_offset = 5,
    y_offset = 5
  )+
  geom_point(data=(clean_location_sta %>% 
                     filter(stationUID %in% pts.China$stationUID)),
             aes(x=as.numeric(longitude),y=as.numeric(latitude),
                 color=as.numeric(period)),
             alpha = 0.7,  size=0.01,shape=16,position = "identity")+
  scale_colour_gradientn(colours = my_colourmap,
                         breaks=c(0,25,50,75,100),limits = c(0,100),
                         labels=c(0,25,50,75,'>100'), oob = scales::squish,
                         values = scales::rescale(c(0,25,50,75,100)))+
  
  labs(title="",color='',tag = 'N=75,018')+
  theme_bw()+  
  theme(
    panel.background = element_rect(fill = '#9ecae1'),
    plot.tag.position = c(0.55, -0.02), plot.tag = element_text(vjust =-1, size=10),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.position='NA',panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.background=element_rect(I(0),linetype=1),
    panel.border = element_rect(color = "red", size = 0.5),
 
    axis.text.x =element_text(size=9.5,face='bold'),
    axis.text.y=element_text(size=9.5,angle = 90,hjust = 0.5,face='bold'),
    axis.title.y =element_text(size=1),
    text=element_text(family = 'Arial'))+  
  scale_y_continuous(name="",limits = c(17.5,53.9),breaks=c(25,45),expand = c(0, 0),
                     labels = c('25°N','45°N'))+

  scale_x_continuous(name = "",limits = c(73, 135.4),breaks=c(80,130),expand = c(0, 0),
                     labels = c('80°E','130°E'))


ggsave( paste0(data.dir,"/figure1/china_duration_gauge1_period_1900_dis.png"),dpi = 300,
        width = 80, height = 55,units = 'mm')


# figure 1 -f -------------------------------------------------------------
Australia.ext <- crop(subset(global.shp,FENAME == "Commonwealth of Australia"),
                      extent(130,154 ,-43 , -20))

pts.OC <- terra::intersect(all.sta.vc, terra::vect(Australia.ext))

geo.map.oc <- ggplot()+
  geom_sf(data = as(global.shp,'sf'),fill='#F2EDEF',
          colour='black',linewidth=0.2)+
  coord_sf(expand = FALSE,clip="on" )+
  with_shadow(
    geom_sf(
      data = as(subset(global.shp,FENAME == "Commonwealth of Australia"),'sf') ,
      fill='#F2EDEF',
      linewidth = 0.5,
      color = "black"
    ),
    sigma = 3,
    x_offset = 5,
    y_offset = 5
  )+
  geom_point(data=(clean_location_sta %>% 
                     filter(stationUID %in% pts.OC$stationUID)),
             aes(x=as.numeric(longitude),y=as.numeric(latitude),
                 color=as.numeric(period)),
             alpha = 0.7,  size=0.01,shape=16,position = "identity")+
  scale_colour_gradientn(colours = my_colourmap,
                         breaks=c(0,25,50,75,100),limits = c(0,100),
                         labels=c(0,25,50,75,'>100'), oob = scales::squish,
                         values = scales::rescale(c(0,25,50,75,100)))+
  labs(title="",color='',tag = 'N=12,567')+
  theme_bw()+  #背景设置为白色
  theme(#背景为海洋色
    panel.background = element_rect(fill = '#9ecae1'),
    plot.tag.position = c(0.55, -0.02), plot.tag = element_text(vjust =-1, size=10),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.position='NA',panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.background=element_rect(I(0),linetype=1),
    panel.border = element_rect(color = "purple", size = 0.5),
    axis.text.x =element_text(size=9.5,face='bold'),
    axis.text.y=element_text(size=9.5,angle = 90,hjust = 0.5,face='bold'),
    axis.title.y =element_text(size=1),
    text=element_text(family = 'Arial'))+
  scale_y_continuous(name="",limits = c(-43,-20),breaks=c(-40,-25),expand = c(0, 0),
                     labels = c('-40°S','-25°S'))+
  scale_x_continuous(name = "",limits = c(130, 154),breaks=c(135,150),expand = c(0, 0),
                     labels = c('135°E','150°E'))

ggsave(paste0(data.dir,"/figure1/oc_duration_gauge1_period_1900_dis.png"),dpi = 300,
       width = 80, height = 55,units = 'mm')
