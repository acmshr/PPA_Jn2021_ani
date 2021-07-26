# required -------------
library(raster)
library(rgeos)
library(rgdal)
library(sp)
library(tidyverse)
library(lubridate)
library(scales)
library(colorspace)
library(metR)
library(gganimate)
library(transformr)

#Improt data for plot --------------------
#Data path
dt_path <- "/mnt/c/Users/aseem/Documents/PPA_Jn2021_ani/Heatwave_ani_plt_data"
#World shapefile
wrld<-readOGR(dsn = dt_path, layer = "World_borders")
plot(wrld)
crs(wrld)<-CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 ")
crs(wrld)
wrld.df<-fortify(wrld)
head(wrld.df)
range(wrld.df$long)

wrld.dff<-wrld.df %>%
  dplyr::filter(long<(-10) & lat>=11 & lat<=81)
head(wrld.dff)

wrld.dff$lon<-if_else(wrld.dff$long<0,wrld.dff$long+360,wrld.dff$long)
head(wrld.dff)
#Canada shapefile
cashp<-readOGR(dsn = dt_path, layer = "Canada")
plot(cashp)
crs(cashp)
cashp_pro<-sp::spTransform(cashp, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
cashp_pro
cashp.df<-fortify(cashp_pro)
head(cashp.df)

#Z anomalies and Z climatology during heatwave
z_ano_df_l<-read_csv(paste0(dt_path, "/Z_anomalies_climatology_0618_0704.csv"))
z_ano_df_l
z_ano_df_l1<-z_ano_df_l%>%
    dplyr::filter(lon >= (xmi_p) & lon <= (xmx_p) )%>%
    dplyr::filter(lat >= (ymi_p) & lat <= (ymx_p) )%>%
    select(lon,lat,Date,zano,zclm)
z_ano_df_l1
  
#Heatwave outline
ppa_poly_df <- read_csv(paste0(dt_path,"/Daily_heatwave_outline.csv"))
ppa_poly_df

#Lytoon location
lyt_bc_cord<-as_tibble(data.frame(lat=50.2333,lon=-121.5914))
lyt_bc_cord$pl<-"Lytton BC"
lyt_bc_cord
lyt_bc_cord_lst<-list()
  for ( i in 1: length(dates_4an)){
    # i<-17
    day<-dates_4an[i]
    day
    lyt_bc_cord_d<-lyt_bc_cord  
    lyt_bc_cord_d
    lyt_bc_cord_d$Date<-day
    lyt_bc_cord_d
    lyt_bc_cord_lst[[i]]<-lyt_bc_cord_d
  }
lyt_bc_cord_d<-bind_rows(lyt_bc_cord_lst)
lyt_bc_cord_d
tail(lyt_bc_cord_d)

#Fire data
fr_ing_dt<-read_csv(paste0(dt_path,"/Heatwave_Fire_data.csv"))
fr_ing_dt
dp_frr_f<-fr_ing_dt%>%
    dplyr::filter(lon >= (xmi_p) & lon <= (xmx_p) )%>%
    dplyr::filter(lat >= (ymi_p) & lat <= (ymx_p) )
dp_frr_f
#Animation plot -------------------------------------
#Create repeated data for animation plot
hcl_palettes(plot=T)
# Domains extent to clip -----------------------------------------
xmi_p = -162
xmx_p = -67
ymi_p = 35
ymx_p = 70

dates_4an<-seq(ymd('2021-06-18'),ymd('2021-07-04'), by = '1 day')
dates_4an

# Evolution Plot with fixed coordinate --------------------------------------------------------
# Heatwave evolution animation 

 wrld_lst<-list() # nolint
  for ( i in 1: length(dates_4an)){ # nolint
    # i<-17
    day<-dates_4an[i]
    day
    wrld1.df_i<-wrld.dff       
    wrld1.df_i
    wrld1.df_i$Date<-day
    wrld1.df_i
    head(wrld1.df_i)
    wrld1.df_i<- wrld1.df_i%>%
                  dplyr::filter(long >= (xmi_p) & long <= (xmx_p) )%>%
                  dplyr::filter(lat >= (ymi_p) & lat <= (ymx_p) )
    wrld1.df_i             
    wrld_lst[[i]]<-wrld1.df_i
  }
  
wrld.df3<-bind_rows(wrld_lst)
head(wrld.df3)
tail(wrld.df3)

#Canada shapefile
CAshp1.df_lst<-list()
for ( i in 1: length(dates_4an)){
    # i<-1
    day<-dates_4an[i]
    day
    CAshp1.df_i<-cashp.df 
    CAshp1.df_i
    CAshp1.df_i$Date<-day
    head(CAshp1.df_i)
    CAshp1.df_ii<-CAshp1.df_i%>%
      dplyr::filter(long >= (xmi_p) & long <= (xmx_p) )%>%
      dplyr::filter(lat >= (ymi_p) & lat <= (ymx_p) ) 
   CAshp1.df_ii
    CAshp1.df_lst[[i]]<-CAshp1.df_ii
  }
can.df3<-bind_rows(CAshp1.df_lst)
head(can.df3)
#Plot  
ppa_evo_plt1<-ggplot()+
    geom_tile(data= z_ano_df_l1,aes(x=lon,y=lat,fill=zano),alpha=1)+
    geom_path(data=wrld.df3, aes(x=long,y=lat,group=group), colour="Black", alpha=0.8,size=0.9)+
    geom_path(data=can.df3, aes(x=long,y=lat,group=group), colour="Black", alpha=0.8,size=0.9)+
    geom_point(data=lyt_bc_cord_d,aes(x=lon,y=lat),color="Magenta",size=4.2)+
    geom_point(data=dp_frr_f,aes(x=lon,y=lat,size=size,color=size),shape =17)+
    scale_fill_continuous_diverging(palette="Blue-Red3",n_interp=21,
      limits=c(-434.2,434.2),
      breaks=seq(-500,500,50),
      name="z-anomalies (gpm)")+
    xlab("Longitude (°W)") +
    ylab("Latitude (°N)")+
    scale_y_continuous(expand=c(0,0))+
    scale_x_continuous(expand=c(0,0))+
    geom_path(data=ppa_poly_df, aes(x=long,y=lat),
      alpha=0.9,size=1.5,show.legend = F,color="Blue")+
    scale_size_manual(name="Fire size (Ha)",
                      values = c("big"=5.0, "sml"=3.2),
                      labels= c("big"="> 200", "sml"="< 200"))+
    scale_color_manual(name="Fire size (Ha)",
      values = c("big"="firebrick1", "sml"="darkorange1"),
      labels= c("big"="> 200", "sml"="< 200"))+
    geom_contour(data= z_ano_df_l1,aes(x=lon,y=lat,z=(zclm)),color="Gray50",alpha=0.8,size=0.25,
      breaks=seq(5000,6000,50))+
    geom_text_contour(data= z_ano_df_l1,aes(x=lon,y=lat,z=(zclm)),skip=1,color="black",alpha=0.8,size=2.2,
      check_overlap=T,stroke=0.1,breaks=seq(5000,6000,100))+
    # geom_rect(data=dom_dt_f, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
    #   fill=NA,size=1,color="Blue")+
    scale_x_continuous(
      breaks = seq(-180,-45,5),
      expand = c(0, 0),
      labels = abs)+
    scale_y_continuous(
      breaks = seq(30,80,5),
      expand = c(0, 0))+
    coord_fixed(ratio=1.41)
  # coord_sf(xlim = c(xmi_p,xmx_p),
  #   ylim = c(ymi_p,ymx_p),
  #   datum = st_crs(4326),
  #   expand = F)
ppa_evo_plt1 
ppa_evo_plt2<-ppa_evo_plt1+
    theme_bw()+
    theme(panel.grid.minor=element_blank(), 
      panel.grid.major=element_line(color="gray75",size=0.05, linetype = "dashed"),
      axis.line = element_line(colour="black", size=1.1),
      axis.ticks.length = unit(-0.20, "cm"),element_line(colour="black", size=1),
      axis.title.y=element_text(angle=90, face="plain", size=12, colour="Black"),
      axis.title.x=element_text(angle=0, face="plain", size=12, colour="Black"),
      axis.text.x=element_text(angle=0, hjust=0.5,vjust=2, colour="black", size=10,
        margin = margin(t = 10, r = 10, b = 10, l = 10)),
      axis.text.y=element_text(angle=90, hjust=0.5,vjust=2, colour="black", size=10,
        margin = margin(t = 10, r = 10, b = 10, l = 10)),
      legend.position = c(0.95,0.6),legend.direction = "vertical")+
    guides(fill = guide_colorbar(barwidth = 1.2, barheight = 14,label.vjust=0.5,label.hjust=0.5,
      title.vjust =0.5,title.hjust =0.5, title.position = "left",
      title.theme = element_text(colour = "Black", angle = 90)))
ppa_evo_plt2
  
#Animate
#Daily PPA with all days data displayed
dly_HW_anim =   ppa_evo_plt2 +
    transition_states(Date, transition_length = 2, state_length = 0, wrap = F) +
    # view_follow(fixed_x = TRUE)  +
    ease_aes('cubic-in-out') +
    # transition_states(states=Date, transition_length = 2, state_length = 1) +
    # ease_aes('linear')+
    enter_fade() +
    exit_fade()+
    labs(title = 'Heatwave Evolution: {closest_state}')
dly_HW_anim
getwd()
#GIF
animate(dly_HW_anim,
    duration = 21, # = 365 days/yr x 3 years x 0.25 sec/day = 274 seconds
    # # fps  =  20,
    height = 610, 
    width = 900,
    renderer = gifski_renderer("Heatwave_evo_FRdate_Jul05_4.gif"))  
  
# #MP4
# animate(dly_HW_anim,
#     duration = 21, # = 365 days/yr x 3 years x 0.25 sec/day = 274 seconds
#     # # fps  =  20,
#     height = 610, 
#     width = 900,
#     renderer = av_renderer("Heatwave_evo_FRdate_Jul05_4.mp4"))  
  


# End ---------------------------------------------------------------------



