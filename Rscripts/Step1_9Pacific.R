#####################################################################################
# Project: Aggregate Heresh's compositions from grids to zip codes                  #
# Code: Pacific                                                                     #
# Author: Yaguang Wei                                                               #
#####################################################################################

############################# 0. Setup load packages ##############################
rm(list=ls())
gc()

library(gdalUtils)
library(devtools)
library(magrittr)
library(rgdal)
library(rgeos)
library(raster)
library(dplyr)
library(reshape2)
library(scales)
library(sf)
library(gstat)
library(spatialEco)
library(sp)
library(stringr)

dir_grid_urban <- '/media/qnap4/Exposure modeling predictions for PM2.5 composition -- continuation from QNAP3 EM directory/9 Pacific/Urban areas at 50m spatial resolution/'
dir_grid_nonurban <- '/media/qnap3/Exposure modeling/3 National scale/USA/7 PM2.5 speciation v1.2000_19/9 Pacific/Non-urban areas at 1km spatial resolution/'
dir_shp <- '/media/qnap4/Yaguang/ZIPCODE_INFO/polygon/'
dir_pobox <- '/media/qnap4/Yaguang/ZIPCODE_INFO/pobox_csv/'
dir_save <- '/media/qnap4/Yaguang/Aggregate_PM_Composition/output/'

years_char <- c('00','01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19')



############################# 1. br ##############################
for (i in years_char) {
  if (!file.exists(paste0(dir_save,'br_20',i,'_P.rds'))){
    ### combine urban and nonurban grids
    br_grid_urban_files <- list.files(path=dir_grid_urban,pattern = paste0("^20",i,"(.*)final.predictions.urban.P9.50m.br.rds$"))
    br_grid_urban <- readRDS(paste0(dir_grid_urban,br_grid_urban_files[1]))
    for (j in 2:length(br_grid_urban_files)) {
      br_grid_urban_tmp <- readRDS(paste0(dir_grid_urban,br_grid_urban_files[j]))
      br_grid_urban <- bind_rows(br_grid_urban,br_grid_urban_tmp)
      rm(br_grid_urban_tmp)
      gc()
    }
    br_grid_urban <- br_grid_urban[,c("lon","lat","final.predicted.br")]
    br_grid_nonurban <- readRDS(paste0(dir_grid_nonurban,"20",i,".final.predictions.nonurban.P9.br.rds"))
    br_grid_nonurban <- br_grid_nonurban[br_grid_nonurban$year==paste0('20',i),]
    br_grid_nonurban <- br_grid_nonurban[,c("lon","lat","final.predicted.br")]
    br_grid <- rbind(br_grid_urban,br_grid_nonurban)
    
    ### aggregate for standard zipcode
    br_grid_zipcode <- br_grid
    cty <- shapefile(paste0(dir_shp,"ESRI",i,"USZIP5_POLY_WGS84.shp"))
    coordinates(br_grid_zipcode)<-~lon+lat
    proj4string(br_grid_zipcode)<-proj4string(cty)
    br_grid_zipcode <- point.in.poly(br_grid_zipcode, cty)
    br_grid_zipcode <- as.data.frame(br_grid_zipcode)[,c('ZIP','final.predicted.br')]
    br_zip <- br_grid_zipcode %>% group_by(ZIP) %>% summarise(br=mean(final.predicted.br,na.rm=T))
    br_zip <- br_zip[!is.na(br_zip$ZIP),]
    
    ### aggregate for po box
    br_grid_pobox <- br_grid
    pobox <- read.csv(paste0(dir_pobox,'ESRI',i,'USZIP5_POINT_WGS84_POBOX.csv'))
    pobox <- pobox[,c(1,3,4)]
    names(pobox) <- c('ZIP','lon','lat')
    link <- nabor::knn(br_grid_pobox[,c("lon","lat")],pobox[,c("lon","lat")],k=1,radius=2*sqrt(2)/10*0.1)
    link <- cbind.data.frame(link$nn.idx,link$nn.dists)
    names(link) <- c("id","dis")
    pobox$pobox_id <- 1:dim(pobox)[1]
    pobox <- cbind.data.frame(pobox,link)
    br_grid_pobox$id <- 1:dim(br_grid_pobox)[1]
    pobox <- left_join(pobox,br_grid_pobox,by=c("id"))
    pobox <- pobox[pobox$dis!=Inf,]
    pobox <- pobox[,c('ZIP','final.predicted.br')]
    names(pobox) <- c("ZIP","br")
    pobox <- pobox[!is.na(pobox$ZIP),]
    pobox$ZIP <- as.character(pobox$ZIP)
    pobox$ZIP <- str_pad(pobox$ZIP, width=5, side="left", pad="0")
    
    ### merge and save
    br_combine <- rbind(br_zip,pobox)
    br_combine <- br_combine[order(br_combine$ZIP),]
    saveRDS(br_combine,file = paste0(dir_save,'br_20',i,'_P.rds'))
    
    rm(br_grid_urban_files,br_grid_urban,br_grid_nonurban_files,br_grid_nonurban,br_grid,br_grid_zipcode,cty,br_zip,br_grid_pobox,pobox,link,br_combine)
    gc()
  }
  cat(i,'\n')
}



############################# 2. ca ##############################
for (i in years_char) {
  if (!file.exists(paste0(dir_save,'ca_20',i,'_P.rds'))){
    ### combine urban and nonurban grids
    ca_grid_urban_files <- list.files(path=dir_grid_urban,pattern = paste0("^20",i,"(.*)final.predictions.urban.P9.50m.ca.rds$"))
    ca_grid_urban <- readRDS(paste0(dir_grid_urban,ca_grid_urban_files[1]))
    for (j in 2:length(ca_grid_urban_files)) {
      ca_grid_urban_tmp <- readRDS(paste0(dir_grid_urban,ca_grid_urban_files[j]))
      ca_grid_urban <- bind_rows(ca_grid_urban,ca_grid_urban_tmp)
      rm(ca_grid_urban_tmp)
      gc()
    }
    ca_grid_urban <- ca_grid_urban[,c("lon","lat","final.predicted.ca")]
    ca_grid_nonurban <- readRDS(paste0(dir_grid_nonurban,"20",i,".final.predictions.nonurban.P9.ca.rds"))
    ca_grid_nonurban <- ca_grid_nonurban[ca_grid_nonurban$year==paste0('20',i),]
    ca_grid_nonurban <- ca_grid_nonurban[,c("lon","lat","final.predicted.ca")]
    ca_grid <- rbind(ca_grid_urban,ca_grid_nonurban)
    
    ### aggregate for standard zipcode
    ca_grid_zipcode <- ca_grid
    cty <- shapefile(paste0(dir_shp,"ESRI",i,"USZIP5_POLY_WGS84.shp"))
    coordinates(ca_grid_zipcode)<-~lon+lat
    proj4string(ca_grid_zipcode)<-proj4string(cty)
    ca_grid_zipcode <- point.in.poly(ca_grid_zipcode, cty)
    ca_grid_zipcode <- as.data.frame(ca_grid_zipcode)[,c('ZIP','final.predicted.ca')]
    ca_zip <- ca_grid_zipcode %>% group_by(ZIP) %>% summarise(ca=mean(final.predicted.ca,na.rm=T))
    ca_zip <- ca_zip[!is.na(ca_zip$ZIP),]
    
    ### aggregate for po box
    ca_grid_pobox <- ca_grid
    pobox <- read.csv(paste0(dir_pobox,'ESRI',i,'USZIP5_POINT_WGS84_POBOX.csv'))
    pobox <- pobox[,c(1,3,4)]
    names(pobox) <- c('ZIP','lon','lat')
    link <- nabor::knn(ca_grid_pobox[,c("lon","lat")],pobox[,c("lon","lat")],k=1,radius=2*sqrt(2)/10*0.1)
    link <- cbind.data.frame(link$nn.idx,link$nn.dists)
    names(link) <- c("id","dis")
    pobox$pobox_id <- 1:dim(pobox)[1]
    pobox <- cbind.data.frame(pobox,link)
    ca_grid_pobox$id <- 1:dim(ca_grid_pobox)[1]
    pobox <- left_join(pobox,ca_grid_pobox,by=c("id"))
    pobox <- pobox[pobox$dis!=Inf,]
    pobox <- pobox[,c('ZIP','final.predicted.ca')]
    names(pobox) <- c("ZIP","ca")
    pobox <- pobox[!is.na(pobox$ZIP),]
    pobox$ZIP <- as.character(pobox$ZIP)
    pobox$ZIP <- str_pad(pobox$ZIP, width=5, side="left", pad="0")
    
    ### merge and save
    ca_combine <- rbind(ca_zip,pobox)
    ca_combine <- ca_combine[order(ca_combine$ZIP),]
    saveRDS(ca_combine,file = paste0(dir_save,'ca_20',i,'_P.rds'))
    
    rm(ca_grid_urban_files,ca_grid_urban,ca_grid_nonurban_files,ca_grid_nonurban,ca_grid,ca_grid_zipcode,cty,ca_zip,ca_grid_pobox,pobox,link,ca_combine)
    gc()
  }
  cat(i,'\n')
}




############################# 3. cu ##############################
for (i in years_char) {
  if (!file.exists(paste0(dir_save,'cu_20',i,'_P.rds'))){
    ### combine urban and nonurban grids
    cu_grid_urban_files <- list.files(path=dir_grid_urban,pattern = paste0("^20",i,"(.*)final.predictions.urban.P9.50m.cu.rds$"))
    cu_grid_urban <- readRDS(paste0(dir_grid_urban,cu_grid_urban_files[1]))
    for (j in 2:length(cu_grid_urban_files)) {
      cu_grid_urban_tmp <- readRDS(paste0(dir_grid_urban,cu_grid_urban_files[j]))
      cu_grid_urban <- bind_rows(cu_grid_urban,cu_grid_urban_tmp)
      rm(cu_grid_urban_tmp)
      gc()
    }
    cu_grid_urban <- cu_grid_urban[,c("lon","lat","final.predicted.cu")]
    cu_grid_nonurban <- readRDS(paste0(dir_grid_nonurban,"20",i,".final.predictions.nonurban.P9.cu.rds"))
    cu_grid_nonurban <- cu_grid_nonurban[cu_grid_nonurban$year==paste0('20',i),]
    cu_grid_nonurban <- cu_grid_nonurban[,c("lon","lat","final.predicted.cu")]
    cu_grid <- rbind(cu_grid_urban,cu_grid_nonurban)
    
    ### aggregate for standard zipcode
    cu_grid_zipcode <- cu_grid
    cty <- shapefile(paste0(dir_shp,"ESRI",i,"USZIP5_POLY_WGS84.shp"))
    coordinates(cu_grid_zipcode)<-~lon+lat
    proj4string(cu_grid_zipcode)<-proj4string(cty)
    cu_grid_zipcode <- point.in.poly(cu_grid_zipcode, cty)
    cu_grid_zipcode <- as.data.frame(cu_grid_zipcode)[,c('ZIP','final.predicted.cu')]
    cu_zip <- cu_grid_zipcode %>% group_by(ZIP) %>% summarise(cu=mean(final.predicted.cu,na.rm=T))
    cu_zip <- cu_zip[!is.na(cu_zip$ZIP),]
    
    ### aggregate for po box
    cu_grid_pobox <- cu_grid
    pobox <- read.csv(paste0(dir_pobox,'ESRI',i,'USZIP5_POINT_WGS84_POBOX.csv'))
    pobox <- pobox[,c(1,3,4)]
    names(pobox) <- c('ZIP','lon','lat')
    link <- nabor::knn(cu_grid_pobox[,c("lon","lat")],pobox[,c("lon","lat")],k=1,radius=2*sqrt(2)/10*0.1)
    link <- cbind.data.frame(link$nn.idx,link$nn.dists)
    names(link) <- c("id","dis")
    pobox$pobox_id <- 1:dim(pobox)[1]
    pobox <- cbind.data.frame(pobox,link)
    cu_grid_pobox$id <- 1:dim(cu_grid_pobox)[1]
    pobox <- left_join(pobox,cu_grid_pobox,by=c("id"))
    pobox <- pobox[pobox$dis!=Inf,]
    pobox <- pobox[,c('ZIP','final.predicted.cu')]
    names(pobox) <- c("ZIP","cu")
    pobox <- pobox[!is.na(pobox$ZIP),]
    pobox$ZIP <- as.character(pobox$ZIP)
    pobox$ZIP <- str_pad(pobox$ZIP, width=5, side="left", pad="0")
    
    ### merge and save
    cu_combine <- rbind(cu_zip,pobox)
    cu_combine <- cu_combine[order(cu_combine$ZIP),]
    saveRDS(cu_combine,file = paste0(dir_save,'cu_20',i,'_P.rds'))
    
    rm(cu_grid_urban_files,cu_grid_urban,cu_grid_nonurban_files,cu_grid_nonurban,cu_grid,cu_grid_zipcode,cty,cu_zip,cu_grid_pobox,pobox,link,cu_combine)
    gc()
  }
  cat(i,'\n')
}



############################# 4. ec ##############################
for (i in years_char) {
  if (!file.exists(paste0(dir_save,'ec_20',i,'_P.rds'))){
    ### combine urban and nonurban grids
    ec_grid_urban_files <- list.files(path=dir_grid_urban,pattern = paste0("^20",i,"(.*)final.predictions.urban.P9.50m.ec.rds$"))
    ec_grid_urban <- readRDS(paste0(dir_grid_urban,ec_grid_urban_files[1]))
    for (j in 2:length(ec_grid_urban_files)) {
      ec_grid_urban_tmp <- readRDS(paste0(dir_grid_urban,ec_grid_urban_files[j]))
      ec_grid_urban <- bind_rows(ec_grid_urban,ec_grid_urban_tmp)
      rm(ec_grid_urban_tmp)
      gc()
    }
    ec_grid_urban <- ec_grid_urban[,c("lon","lat","final.predicted.ec")]
    ec_grid_nonurban <- readRDS(paste0(dir_grid_nonurban,"20",i,".final.predictions.nonurban.P9.ec.rds"))
    ec_grid_nonurban <- ec_grid_nonurban[ec_grid_nonurban$year==paste0('20',i),]
    ec_grid_nonurban <- ec_grid_nonurban[,c("lon","lat","final.predicted.ec")]
    ec_grid <- rbind(ec_grid_urban,ec_grid_nonurban)
    
    ### aggregate for standard zipcode
    ec_grid_zipcode <- ec_grid
    cty <- shapefile(paste0(dir_shp,"ESRI",i,"USZIP5_POLY_WGS84.shp"))
    coordinates(ec_grid_zipcode)<-~lon+lat
    proj4string(ec_grid_zipcode)<-proj4string(cty)
    ec_grid_zipcode <- point.in.poly(ec_grid_zipcode, cty)
    ec_grid_zipcode <- as.data.frame(ec_grid_zipcode)[,c('ZIP','final.predicted.ec')]
    ec_zip <- ec_grid_zipcode %>% group_by(ZIP) %>% summarise(ec=mean(final.predicted.ec,na.rm=T))
    ec_zip <- ec_zip[!is.na(ec_zip$ZIP),]
    
    ### aggregate for po box
    ec_grid_pobox <- ec_grid
    pobox <- read.csv(paste0(dir_pobox,'ESRI',i,'USZIP5_POINT_WGS84_POBOX.csv'))
    pobox <- pobox[,c(1,3,4)]
    names(pobox) <- c('ZIP','lon','lat')
    link <- nabor::knn(ec_grid_pobox[,c("lon","lat")],pobox[,c("lon","lat")],k=1,radius=2*sqrt(2)/10*0.1)
    link <- cbind.data.frame(link$nn.idx,link$nn.dists)
    names(link) <- c("id","dis")
    pobox$pobox_id <- 1:dim(pobox)[1]
    pobox <- cbind.data.frame(pobox,link)
    ec_grid_pobox$id <- 1:dim(ec_grid_pobox)[1]
    pobox <- left_join(pobox,ec_grid_pobox,by=c("id"))
    pobox <- pobox[pobox$dis!=Inf,]
    pobox <- pobox[,c('ZIP','final.predicted.ec')]
    names(pobox) <- c("ZIP","ec")
    pobox <- pobox[!is.na(pobox$ZIP),]
    pobox$ZIP <- as.character(pobox$ZIP)
    pobox$ZIP <- str_pad(pobox$ZIP, width=5, side="left", pad="0")
    
    ### merge and save
    ec_combine <- rbind(ec_zip,pobox)
    ec_combine <- ec_combine[order(ec_combine$ZIP),]
    saveRDS(ec_combine,file = paste0(dir_save,'ec_20',i,'_P.rds'))
    
    rm(ec_grid_urban_files,ec_grid_urban,ec_grid_nonurban_files,ec_grid_nonurban,ec_grid,ec_grid_zipcode,cty,ec_zip,ec_grid_pobox,pobox,link,ec_combine)
    gc()
  }
  cat(i,'\n')
}



############################# 5. fe ##############################
for (i in years_char) {
  if (!file.exists(paste0(dir_save,'fe_20',i,'_P.rds'))){
    ### combine urban and nonurban grids
    fe_grid_urban_files <- list.files(path=dir_grid_urban,pattern = paste0("^20",i,"(.*)final.predictions.urban.P9.50m.fe.rds$"))
    fe_grid_urban <- readRDS(paste0(dir_grid_urban,fe_grid_urban_files[1]))
    for (j in 2:length(fe_grid_urban_files)) {
      fe_grid_urban_tmp <- readRDS(paste0(dir_grid_urban,fe_grid_urban_files[j]))
      fe_grid_urban <- bind_rows(fe_grid_urban,fe_grid_urban_tmp)
      rm(fe_grid_urban_tmp)
      gc()
    }
    fe_grid_urban <- fe_grid_urban[,c("lon","lat","final.predicted.fe")]
    fe_grid_nonurban <- readRDS(paste0(dir_grid_nonurban,"20",i,".final.predictions.nonurban.P9.fe.rds"))
    fe_grid_nonurban <- fe_grid_nonurban[fe_grid_nonurban$year==paste0('20',i),]
    fe_grid_nonurban <- fe_grid_nonurban[,c("lon","lat","final.predicted.fe")]
    fe_grid <- rbind(fe_grid_urban,fe_grid_nonurban)
    
    ### aggregate for standard zipcode
    fe_grid_zipcode <- fe_grid
    cty <- shapefile(paste0(dir_shp,"ESRI",i,"USZIP5_POLY_WGS84.shp"))
    coordinates(fe_grid_zipcode)<-~lon+lat
    proj4string(fe_grid_zipcode)<-proj4string(cty)
    fe_grid_zipcode <- point.in.poly(fe_grid_zipcode, cty)
    fe_grid_zipcode <- as.data.frame(fe_grid_zipcode)[,c('ZIP','final.predicted.fe')]
    fe_zip <- fe_grid_zipcode %>% group_by(ZIP) %>% summarise(fe=mean(final.predicted.fe,na.rm=T))
    fe_zip <- fe_zip[!is.na(fe_zip$ZIP),]
    
    ### aggregate for po box
    fe_grid_pobox <- fe_grid
    pobox <- read.csv(paste0(dir_pobox,'ESRI',i,'USZIP5_POINT_WGS84_POBOX.csv'))
    pobox <- pobox[,c(1,3,4)]
    names(pobox) <- c('ZIP','lon','lat')
    link <- nabor::knn(fe_grid_pobox[,c("lon","lat")],pobox[,c("lon","lat")],k=1,radius=2*sqrt(2)/10*0.1)
    link <- cbind.data.frame(link$nn.idx,link$nn.dists)
    names(link) <- c("id","dis")
    pobox$pobox_id <- 1:dim(pobox)[1]
    pobox <- cbind.data.frame(pobox,link)
    fe_grid_pobox$id <- 1:dim(fe_grid_pobox)[1]
    pobox <- left_join(pobox,fe_grid_pobox,by=c("id"))
    pobox <- pobox[pobox$dis!=Inf,]
    pobox <- pobox[,c('ZIP','final.predicted.fe')]
    names(pobox) <- c("ZIP","fe")
    pobox <- pobox[!is.na(pobox$ZIP),]
    pobox$ZIP <- as.character(pobox$ZIP)
    pobox$ZIP <- str_pad(pobox$ZIP, width=5, side="left", pad="0")
    
    ### merge and save
    fe_combine <- rbind(fe_zip,pobox)
    fe_combine <- fe_combine[order(fe_combine$ZIP),]
    saveRDS(fe_combine,file = paste0(dir_save,'fe_20',i,'_P.rds'))
    
    rm(fe_grid_urban_files,fe_grid_urban,fe_grid_nonurban_files,fe_grid_nonurban,fe_grid,fe_grid_zipcode,cty,fe_zip,fe_grid_pobox,pobox,link,fe_combine)
    gc()
  }
  cat(i,'\n')
}



############################# 6. k ##############################
for (i in years_char) {
  if (!file.exists(paste0(dir_save,'k_20',i,'_P.rds'))){
    ### combine urban and nonurban grids
    k_grid_urban_files <- list.files(path=dir_grid_urban,pattern = paste0("^20",i,"(.*)final.predictions.urban.P9.50m.k.rds$"))
    k_grid_urban <- readRDS(paste0(dir_grid_urban,k_grid_urban_files[1]))
    for (j in 2:length(k_grid_urban_files)) {
      k_grid_urban_tmp <- readRDS(paste0(dir_grid_urban,k_grid_urban_files[j]))
      k_grid_urban <- bind_rows(k_grid_urban,k_grid_urban_tmp)
      rm(k_grid_urban_tmp)
      gc()
    }
    k_grid_urban <- k_grid_urban[,c("lon","lat","final.predicted.k")]
    k_grid_nonurban <- readRDS(paste0(dir_grid_nonurban,"20",i,".final.predictions.nonurban.P9.k.rds"))
    k_grid_nonurban <- k_grid_nonurban[k_grid_nonurban$year==paste0('20',i),]
    k_grid_nonurban <- k_grid_nonurban[,c("lon","lat","final.predicted.k")]
    k_grid <- rbind(k_grid_urban,k_grid_nonurban)
    
    ### aggregate for standard zipcode
    k_grid_zipcode <- k_grid
    cty <- shapefile(paste0(dir_shp,"ESRI",i,"USZIP5_POLY_WGS84.shp"))
    coordinates(k_grid_zipcode)<-~lon+lat
    proj4string(k_grid_zipcode)<-proj4string(cty)
    k_grid_zipcode <- point.in.poly(k_grid_zipcode, cty)
    k_grid_zipcode <- as.data.frame(k_grid_zipcode)[,c('ZIP','final.predicted.k')]
    k_zip <- k_grid_zipcode %>% group_by(ZIP) %>% summarise(k=mean(final.predicted.k,na.rm=T))
    k_zip <- k_zip[!is.na(k_zip$ZIP),]
    
    ### aggregate for po box
    k_grid_pobox <- k_grid
    pobox <- read.csv(paste0(dir_pobox,'ESRI',i,'USZIP5_POINT_WGS84_POBOX.csv'))
    pobox <- pobox[,c(1,3,4)]
    names(pobox) <- c('ZIP','lon','lat')
    link <- nabor::knn(k_grid_pobox[,c("lon","lat")],pobox[,c("lon","lat")],k=1,radius=2*sqrt(2)/10*0.1)
    link <- cbind.data.frame(link$nn.idx,link$nn.dists)
    names(link) <- c("id","dis")
    pobox$pobox_id <- 1:dim(pobox)[1]
    pobox <- cbind.data.frame(pobox,link)
    k_grid_pobox$id <- 1:dim(k_grid_pobox)[1]
    pobox <- left_join(pobox,k_grid_pobox,by=c("id"))
    pobox <- pobox[pobox$dis!=Inf,]
    pobox <- pobox[,c('ZIP','final.predicted.k')]
    names(pobox) <- c("ZIP","k")
    pobox <- pobox[!is.na(pobox$ZIP),]
    pobox$ZIP <- as.character(pobox$ZIP)
    pobox$ZIP <- str_pad(pobox$ZIP, width=5, side="left", pad="0")
    
    ### merge and save
    k_combine <- rbind(k_zip,pobox)
    k_combine <- k_combine[order(k_combine$ZIP),]
    saveRDS(k_combine,file = paste0(dir_save,'k_20',i,'_P.rds'))
    
    rm(k_grid_urban_files,k_grid_urban,k_grid_nonurban_files,k_grid_nonurban,k_grid,k_grid_zipcode,cty,k_zip,k_grid_pobox,pobox,link,k_combine)
    gc()
  }
  cat(i,'\n')
}



############################# 7. nh4 ##############################
for (i in years_char) {
  if (!file.exists(paste0(dir_save,'nh4_20',i,'_P.rds'))){
    ### combine urban and nonurban grids
    nh4_grid_urban_files <- list.files(path=dir_grid_urban,pattern = paste0("^20",i,"(.*)final.predictions.urban.P9.50m.nh4.rds$"))
    nh4_grid_urban <- readRDS(paste0(dir_grid_urban,nh4_grid_urban_files[1]))
    for (j in 2:length(nh4_grid_urban_files)) {
      nh4_grid_urban_tmp <- readRDS(paste0(dir_grid_urban,nh4_grid_urban_files[j]))
      nh4_grid_urban <- bind_rows(nh4_grid_urban,nh4_grid_urban_tmp)
      rm(nh4_grid_urban_tmp)
      gc()
    }
    nh4_grid_urban <- nh4_grid_urban[,c("lon","lat","final.predicted.nh4")]
    nh4_grid_nonurban <- readRDS(paste0(dir_grid_nonurban,"20",i,".final.predictions.nonurban.P9.nh4.rds"))
    nh4_grid_nonurban <- nh4_grid_nonurban[nh4_grid_nonurban$year==paste0('20',i),]
    nh4_grid_nonurban <- nh4_grid_nonurban[,c("lon","lat","final.predicted.nh4")]
    nh4_grid <- rbind(nh4_grid_urban,nh4_grid_nonurban)
    
    ### aggregate for standard zipcode
    nh4_grid_zipcode <- nh4_grid
    cty <- shapefile(paste0(dir_shp,"ESRI",i,"USZIP5_POLY_WGS84.shp"))
    coordinates(nh4_grid_zipcode)<-~lon+lat
    proj4string(nh4_grid_zipcode)<-proj4string(cty)
    nh4_grid_zipcode <- point.in.poly(nh4_grid_zipcode, cty)
    nh4_grid_zipcode <- as.data.frame(nh4_grid_zipcode)[,c('ZIP','final.predicted.nh4')]
    nh4_zip <- nh4_grid_zipcode %>% group_by(ZIP) %>% summarise(nh4=mean(final.predicted.nh4,na.rm=T))
    nh4_zip <- nh4_zip[!is.na(nh4_zip$ZIP),]
    
    ### aggregate for po box
    nh4_grid_pobox <- nh4_grid
    pobox <- read.csv(paste0(dir_pobox,'ESRI',i,'USZIP5_POINT_WGS84_POBOX.csv'))
    pobox <- pobox[,c(1,3,4)]
    names(pobox) <- c('ZIP','lon','lat')
    link <- nabor::knn(nh4_grid_pobox[,c("lon","lat")],pobox[,c("lon","lat")],k=1,radius=2*sqrt(2)/10*0.1)
    link <- cbind.data.frame(link$nn.idx,link$nn.dists)
    names(link) <- c("id","dis")
    pobox$pobox_id <- 1:dim(pobox)[1]
    pobox <- cbind.data.frame(pobox,link)
    nh4_grid_pobox$id <- 1:dim(nh4_grid_pobox)[1]
    pobox <- left_join(pobox,nh4_grid_pobox,by=c("id"))
    pobox <- pobox[pobox$dis!=Inf,]
    pobox <- pobox[,c('ZIP','final.predicted.nh4')]
    names(pobox) <- c("ZIP","nh4")
    pobox <- pobox[!is.na(pobox$ZIP),]
    pobox$ZIP <- as.character(pobox$ZIP)
    pobox$ZIP <- str_pad(pobox$ZIP, width=5, side="left", pad="0")
    
    ### merge and save
    nh4_combine <- rbind(nh4_zip,pobox)
    nh4_combine <- nh4_combine[order(nh4_combine$ZIP),]
    saveRDS(nh4_combine,file = paste0(dir_save,'nh4_20',i,'_P.rds'))
    
    rm(nh4_grid_urban_files,nh4_grid_urban,nh4_grid_nonurban_files,nh4_grid_nonurban,nh4_grid,nh4_grid_zipcode,cty,nh4_zip,nh4_grid_pobox,pobox,link,nh4_combine)
    gc()
  }
  cat(i,'\n')
}



############################# 8. ni ##############################
for (i in years_char) {
  if (!file.exists(paste0(dir_save,'ni_20',i,'_P.rds'))){
    ### combine urban and nonurban grids
    ni_grid_urban_files <- list.files(path=dir_grid_urban,pattern = paste0("^20",i,"(.*)final.predictions.urban.P9.50m.ni.rds$"))
    ni_grid_urban <- readRDS(paste0(dir_grid_urban,ni_grid_urban_files[1]))
    for (j in 2:length(ni_grid_urban_files)) {
      ni_grid_urban_tmp <- readRDS(paste0(dir_grid_urban,ni_grid_urban_files[j]))
      ni_grid_urban <- bind_rows(ni_grid_urban,ni_grid_urban_tmp)
      rm(ni_grid_urban_tmp)
      gc()
    }
    ni_grid_urban <- ni_grid_urban[,c("lon","lat","final.predicted.ni")]
    ni_grid_nonurban <- readRDS(paste0(dir_grid_nonurban,"20",i,".final.predictions.nonurban.P9.ni.rds"))
    ni_grid_nonurban <- ni_grid_nonurban[ni_grid_nonurban$year==paste0('20',i),]
    ni_grid_nonurban <- ni_grid_nonurban[,c("lon","lat","final.predicted.ni")]
    ni_grid <- rbind(ni_grid_urban,ni_grid_nonurban)
    
    ### aggregate for standard zipcode
    ni_grid_zipcode <- ni_grid
    cty <- shapefile(paste0(dir_shp,"ESRI",i,"USZIP5_POLY_WGS84.shp"))
    coordinates(ni_grid_zipcode)<-~lon+lat
    proj4string(ni_grid_zipcode)<-proj4string(cty)
    ni_grid_zipcode <- point.in.poly(ni_grid_zipcode, cty)
    ni_grid_zipcode <- as.data.frame(ni_grid_zipcode)[,c('ZIP','final.predicted.ni')]
    ni_zip <- ni_grid_zipcode %>% group_by(ZIP) %>% summarise(ni=mean(final.predicted.ni,na.rm=T))
    ni_zip <- ni_zip[!is.na(ni_zip$ZIP),]
    
    ### aggregate for po box
    ni_grid_pobox <- ni_grid
    pobox <- read.csv(paste0(dir_pobox,'ESRI',i,'USZIP5_POINT_WGS84_POBOX.csv'))
    pobox <- pobox[,c(1,3,4)]
    names(pobox) <- c('ZIP','lon','lat')
    link <- nabor::knn(ni_grid_pobox[,c("lon","lat")],pobox[,c("lon","lat")],k=1,radius=2*sqrt(2)/10*0.1)
    link <- cbind.data.frame(link$nn.idx,link$nn.dists)
    names(link) <- c("id","dis")
    pobox$pobox_id <- 1:dim(pobox)[1]
    pobox <- cbind.data.frame(pobox,link)
    ni_grid_pobox$id <- 1:dim(ni_grid_pobox)[1]
    pobox <- left_join(pobox,ni_grid_pobox,by=c("id"))
    pobox <- pobox[pobox$dis!=Inf,]
    pobox <- pobox[,c('ZIP','final.predicted.ni')]
    names(pobox) <- c("ZIP","ni")
    pobox <- pobox[!is.na(pobox$ZIP),]
    pobox$ZIP <- as.character(pobox$ZIP)
    pobox$ZIP <- str_pad(pobox$ZIP, width=5, side="left", pad="0")
    
    ### merge and save
    ni_combine <- rbind(ni_zip,pobox)
    ni_combine <- ni_combine[order(ni_combine$ZIP),]
    saveRDS(ni_combine,file = paste0(dir_save,'ni_20',i,'_P.rds'))
    
    rm(ni_grid_urban_files,ni_grid_urban,ni_grid_nonurban_files,ni_grid_nonurban,ni_grid,ni_grid_zipcode,cty,ni_zip,ni_grid_pobox,pobox,link,ni_combine)
    gc()
  }
  cat(i,'\n')
}



############################# 9. no3 ##############################
for (i in years_char) {
  if (!file.exists(paste0(dir_save,'no3_20',i,'_P.rds'))){
    ### combine urban and nonurban grids
    no3_grid_urban_files <- list.files(path=dir_grid_urban,pattern = paste0("^20",i,"(.*)final.predictions.urban.P9.50m.no3.rds$"))
    no3_grid_urban <- readRDS(paste0(dir_grid_urban,no3_grid_urban_files[1]))
    for (j in 2:length(no3_grid_urban_files)) {
      no3_grid_urban_tmp <- readRDS(paste0(dir_grid_urban,no3_grid_urban_files[j]))
      no3_grid_urban <- bind_rows(no3_grid_urban,no3_grid_urban_tmp)
      rm(no3_grid_urban_tmp)
      gc()
    }
    no3_grid_urban <- no3_grid_urban[,c("lon","lat","final.predicted.no3")]
    no3_grid_nonurban <- readRDS(paste0(dir_grid_nonurban,"20",i,".final.predictions.nonurban.P9.no3.rds"))
    no3_grid_nonurban <- no3_grid_nonurban[no3_grid_nonurban$year==paste0('20',i),]
    no3_grid_nonurban <- no3_grid_nonurban[,c("lon","lat","final.predicted.no3")]
    no3_grid <- rbind(no3_grid_urban,no3_grid_nonurban)
    
    ### aggregate for standard zipcode
    no3_grid_zipcode <- no3_grid
    cty <- shapefile(paste0(dir_shp,"ESRI",i,"USZIP5_POLY_WGS84.shp"))
    coordinates(no3_grid_zipcode)<-~lon+lat
    proj4string(no3_grid_zipcode)<-proj4string(cty)
    no3_grid_zipcode <- point.in.poly(no3_grid_zipcode, cty)
    no3_grid_zipcode <- as.data.frame(no3_grid_zipcode)[,c('ZIP','final.predicted.no3')]
    no3_zip <- no3_grid_zipcode %>% group_by(ZIP) %>% summarise(no3=mean(final.predicted.no3,na.rm=T))
    no3_zip <- no3_zip[!is.na(no3_zip$ZIP),]
    
    ### aggregate for po box
    no3_grid_pobox <- no3_grid
    pobox <- read.csv(paste0(dir_pobox,'ESRI',i,'USZIP5_POINT_WGS84_POBOX.csv'))
    pobox <- pobox[,c(1,3,4)]
    names(pobox) <- c('ZIP','lon','lat')
    link <- nabor::knn(no3_grid_pobox[,c("lon","lat")],pobox[,c("lon","lat")],k=1,radius=2*sqrt(2)/10*0.1)
    link <- cbind.data.frame(link$nn.idx,link$nn.dists)
    names(link) <- c("id","dis")
    pobox$pobox_id <- 1:dim(pobox)[1]
    pobox <- cbind.data.frame(pobox,link)
    no3_grid_pobox$id <- 1:dim(no3_grid_pobox)[1]
    pobox <- left_join(pobox,no3_grid_pobox,by=c("id"))
    pobox <- pobox[pobox$dis!=Inf,]
    pobox <- pobox[,c('ZIP','final.predicted.no3')]
    names(pobox) <- c("ZIP","no3")
    pobox <- pobox[!is.na(pobox$ZIP),]
    pobox$ZIP <- as.character(pobox$ZIP)
    pobox$ZIP <- str_pad(pobox$ZIP, width=5, side="left", pad="0")
    
    ### merge and save
    no3_combine <- rbind(no3_zip,pobox)
    no3_combine <- no3_combine[order(no3_combine$ZIP),]
    saveRDS(no3_combine,file = paste0(dir_save,'no3_20',i,'_P.rds'))
    
    rm(no3_grid_urban_files,no3_grid_urban,no3_grid_nonurban_files,no3_grid_nonurban,no3_grid,no3_grid_zipcode,cty,no3_zip,no3_grid_pobox,pobox,link,no3_combine)
    gc()
  }
  cat(i,'\n')
}



############################# 10. oc ##############################
for (i in years_char) {
  if (!file.exists(paste0(dir_save,'oc_20',i,'_P.rds'))){
    ### combine urban and nonurban grids
    oc_grid_urban_files <- list.files(path=dir_grid_urban,pattern = paste0("^20",i,"(.*)final.predictions.urban.P9.50m.oc.rds$"))
    oc_grid_urban <- readRDS(paste0(dir_grid_urban,oc_grid_urban_files[1]))
    for (j in 2:length(oc_grid_urban_files)) {
      oc_grid_urban_tmp <- readRDS(paste0(dir_grid_urban,oc_grid_urban_files[j]))
      oc_grid_urban <- bind_rows(oc_grid_urban,oc_grid_urban_tmp)
      rm(oc_grid_urban_tmp)
      gc()
    }
    oc_grid_urban <- oc_grid_urban[,c("lon","lat","final.predicted.oc")]
    oc_grid_nonurban <- readRDS(paste0(dir_grid_nonurban,"20",i,".final.predictions.nonurban.P9.oc.rds"))
    oc_grid_nonurban <- oc_grid_nonurban[oc_grid_nonurban$year==paste0('20',i),]
    oc_grid_nonurban <- oc_grid_nonurban[,c("lon","lat","final.predicted.oc")]
    oc_grid <- rbind(oc_grid_urban,oc_grid_nonurban)
    
    ### aggregate for standard zipcode
    oc_grid_zipcode <- oc_grid
    cty <- shapefile(paste0(dir_shp,"ESRI",i,"USZIP5_POLY_WGS84.shp"))
    coordinates(oc_grid_zipcode)<-~lon+lat
    proj4string(oc_grid_zipcode)<-proj4string(cty)
    oc_grid_zipcode <- point.in.poly(oc_grid_zipcode, cty)
    oc_grid_zipcode <- as.data.frame(oc_grid_zipcode)[,c('ZIP','final.predicted.oc')]
    oc_zip <- oc_grid_zipcode %>% group_by(ZIP) %>% summarise(oc=mean(final.predicted.oc,na.rm=T))
    oc_zip <- oc_zip[!is.na(oc_zip$ZIP),]
    
    ### aggregate for po box
    oc_grid_pobox <- oc_grid
    pobox <- read.csv(paste0(dir_pobox,'ESRI',i,'USZIP5_POINT_WGS84_POBOX.csv'))
    pobox <- pobox[,c(1,3,4)]
    names(pobox) <- c('ZIP','lon','lat')
    link <- nabor::knn(oc_grid_pobox[,c("lon","lat")],pobox[,c("lon","lat")],k=1,radius=2*sqrt(2)/10*0.1)
    link <- cbind.data.frame(link$nn.idx,link$nn.dists)
    names(link) <- c("id","dis")
    pobox$pobox_id <- 1:dim(pobox)[1]
    pobox <- cbind.data.frame(pobox,link)
    oc_grid_pobox$id <- 1:dim(oc_grid_pobox)[1]
    pobox <- left_join(pobox,oc_grid_pobox,by=c("id"))
    pobox <- pobox[pobox$dis!=Inf,]
    pobox <- pobox[,c('ZIP','final.predicted.oc')]
    names(pobox) <- c("ZIP","oc")
    pobox <- pobox[!is.na(pobox$ZIP),]
    pobox$ZIP <- as.character(pobox$ZIP)
    pobox$ZIP <- str_pad(pobox$ZIP, width=5, side="left", pad="0")
    
    ### merge and save
    oc_combine <- rbind(oc_zip,pobox)
    oc_combine <- oc_combine[order(oc_combine$ZIP),]
    saveRDS(oc_combine,file = paste0(dir_save,'oc_20',i,'_P.rds'))
    
    rm(oc_grid_urban_files,oc_grid_urban,oc_grid_nonurban_files,oc_grid_nonurban,oc_grid,oc_grid_zipcode,cty,oc_zip,oc_grid_pobox,pobox,link,oc_combine)
    gc()
  }
  cat(i,'\n')
}



############################# 11. pb ##############################
for (i in years_char) {
  if (!file.exists(paste0(dir_save,'pb_20',i,'_P.rds'))){
    ### combine urban and nonurban grids
    pb_grid_urban_files <- list.files(path=dir_grid_urban,pattern = paste0("^20",i,"(.*)final.predictions.urban.P9.50m.pb.rds$"))
    pb_grid_urban <- readRDS(paste0(dir_grid_urban,pb_grid_urban_files[1]))
    for (j in 2:length(pb_grid_urban_files)) {
      pb_grid_urban_tmp <- readRDS(paste0(dir_grid_urban,pb_grid_urban_files[j]))
      pb_grid_urban <- bind_rows(pb_grid_urban,pb_grid_urban_tmp)
      rm(pb_grid_urban_tmp)
      gc()
    }
    pb_grid_urban <- pb_grid_urban[,c("lon","lat","final.predicted.pb")]
    pb_grid_nonurban <- readRDS(paste0(dir_grid_nonurban,"20",i,".final.predictions.nonurban.P9.pb.rds"))
    pb_grid_nonurban <- pb_grid_nonurban[pb_grid_nonurban$year==paste0('20',i),]
    pb_grid_nonurban <- pb_grid_nonurban[,c("lon","lat","final.predicted.pb")]
    pb_grid <- rbind(pb_grid_urban,pb_grid_nonurban)
    
    ### aggregate for standard zipcode
    pb_grid_zipcode <- pb_grid
    cty <- shapefile(paste0(dir_shp,"ESRI",i,"USZIP5_POLY_WGS84.shp"))
    coordinates(pb_grid_zipcode)<-~lon+lat
    proj4string(pb_grid_zipcode)<-proj4string(cty)
    pb_grid_zipcode <- point.in.poly(pb_grid_zipcode, cty)
    pb_grid_zipcode <- as.data.frame(pb_grid_zipcode)[,c('ZIP','final.predicted.pb')]
    pb_zip <- pb_grid_zipcode %>% group_by(ZIP) %>% summarise(pb=mean(final.predicted.pb,na.rm=T))
    pb_zip <- pb_zip[!is.na(pb_zip$ZIP),]
    
    ### aggregate for po box
    pb_grid_pobox <- pb_grid
    pobox <- read.csv(paste0(dir_pobox,'ESRI',i,'USZIP5_POINT_WGS84_POBOX.csv'))
    pobox <- pobox[,c(1,3,4)]
    names(pobox) <- c('ZIP','lon','lat')
    link <- nabor::knn(pb_grid_pobox[,c("lon","lat")],pobox[,c("lon","lat")],k=1,radius=2*sqrt(2)/10*0.1)
    link <- cbind.data.frame(link$nn.idx,link$nn.dists)
    names(link) <- c("id","dis")
    pobox$pobox_id <- 1:dim(pobox)[1]
    pobox <- cbind.data.frame(pobox,link)
    pb_grid_pobox$id <- 1:dim(pb_grid_pobox)[1]
    pobox <- left_join(pobox,pb_grid_pobox,by=c("id"))
    pobox <- pobox[pobox$dis!=Inf,]
    pobox <- pobox[,c('ZIP','final.predicted.pb')]
    names(pobox) <- c("ZIP","pb")
    pobox <- pobox[!is.na(pobox$ZIP),]
    pobox$ZIP <- as.character(pobox$ZIP)
    pobox$ZIP <- str_pad(pobox$ZIP, width=5, side="left", pad="0")
    
    ### merge and save
    pb_combine <- rbind(pb_zip,pobox)
    pb_combine <- pb_combine[order(pb_combine$ZIP),]
    saveRDS(pb_combine,file = paste0(dir_save,'pb_20',i,'_P.rds'))
    
    rm(pb_grid_urban_files,pb_grid_urban,pb_grid_nonurban_files,pb_grid_nonurban,pb_grid,pb_grid_zipcode,cty,pb_zip,pb_grid_pobox,pobox,link,pb_combine)
    gc()
  }
  cat(i,'\n')
}



############################# 12. si ##############################
for (i in years_char) {
  if (!file.exists(paste0(dir_save,'si_20',i,'_P.rds'))){
    ### combine urban and nonurban grids
    si_grid_urban_files <- list.files(path=dir_grid_urban,pattern = paste0("^20",i,"(.*)final.predictions.urban.P9.50m.si.rds$"))
    si_grid_urban <- readRDS(paste0(dir_grid_urban,si_grid_urban_files[1]))
    for (j in 2:length(si_grid_urban_files)) {
      si_grid_urban_tmp <- readRDS(paste0(dir_grid_urban,si_grid_urban_files[j]))
      si_grid_urban <- bind_rows(si_grid_urban,si_grid_urban_tmp)
      rm(si_grid_urban_tmp)
      gc()
    }
    si_grid_urban <- si_grid_urban[,c("lon","lat","final.predicted.si")]
    si_grid_nonurban <- readRDS(paste0(dir_grid_nonurban,"20",i,".final.predictions.nonurban.P9.si.rds"))
    si_grid_nonurban <- si_grid_nonurban[si_grid_nonurban$year==paste0('20',i),]
    si_grid_nonurban <- si_grid_nonurban[,c("lon","lat","final.predicted.si")]
    si_grid <- rbind(si_grid_urban,si_grid_nonurban)
    
    ### aggregate for standard zipcode
    si_grid_zipcode <- si_grid
    cty <- shapefile(paste0(dir_shp,"ESRI",i,"USZIP5_POLY_WGS84.shp"))
    coordinates(si_grid_zipcode)<-~lon+lat
    proj4string(si_grid_zipcode)<-proj4string(cty)
    si_grid_zipcode <- point.in.poly(si_grid_zipcode, cty)
    si_grid_zipcode <- as.data.frame(si_grid_zipcode)[,c('ZIP','final.predicted.si')]
    si_zip <- si_grid_zipcode %>% group_by(ZIP) %>% summarise(si=mean(final.predicted.si,na.rm=T))
    si_zip <- si_zip[!is.na(si_zip$ZIP),]
    
    ### aggregate for po box
    si_grid_pobox <- si_grid
    pobox <- read.csv(paste0(dir_pobox,'ESRI',i,'USZIP5_POINT_WGS84_POBOX.csv'))
    pobox <- pobox[,c(1,3,4)]
    names(pobox) <- c('ZIP','lon','lat')
    link <- nabor::knn(si_grid_pobox[,c("lon","lat")],pobox[,c("lon","lat")],k=1,radius=2*sqrt(2)/10*0.1)
    link <- cbind.data.frame(link$nn.idx,link$nn.dists)
    names(link) <- c("id","dis")
    pobox$pobox_id <- 1:dim(pobox)[1]
    pobox <- cbind.data.frame(pobox,link)
    si_grid_pobox$id <- 1:dim(si_grid_pobox)[1]
    pobox <- left_join(pobox,si_grid_pobox,by=c("id"))
    pobox <- pobox[pobox$dis!=Inf,]
    pobox <- pobox[,c('ZIP','final.predicted.si')]
    names(pobox) <- c("ZIP","si")
    pobox <- pobox[!is.na(pobox$ZIP),]
    pobox$ZIP <- as.character(pobox$ZIP)
    pobox$ZIP <- str_pad(pobox$ZIP, width=5, side="left", pad="0")
    
    ### merge and save
    si_combine <- rbind(si_zip,pobox)
    si_combine <- si_combine[order(si_combine$ZIP),]
    saveRDS(si_combine,file = paste0(dir_save,'si_20',i,'_P.rds'))
    
    rm(si_grid_urban_files,si_grid_urban,si_grid_nonurban_files,si_grid_nonurban,si_grid,si_grid_zipcode,cty,si_zip,si_grid_pobox,pobox,link,si_combine)
    gc()
  }
  cat(i,'\n')
}



############################# 13. so4 ##############################
for (i in years_char) {
  if (!file.exists(paste0(dir_save,'so4_20',i,'_P.rds'))){
    ### combine urban and nonurban grids
    so4_grid_urban_files <- list.files(path=dir_grid_urban,pattern = paste0("^20",i,"(.*)final.predictions.urban.P9.50m.so4.rds$"))
    so4_grid_urban <- readRDS(paste0(dir_grid_urban,so4_grid_urban_files[1]))
    for (j in 2:length(so4_grid_urban_files)) {
      so4_grid_urban_tmp <- readRDS(paste0(dir_grid_urban,so4_grid_urban_files[j]))
      so4_grid_urban <- bind_rows(so4_grid_urban,so4_grid_urban_tmp)
      rm(so4_grid_urban_tmp)
      gc()
    }
    so4_grid_urban <- so4_grid_urban[,c("lon","lat","final.predicted.so4")]
    so4_grid_nonurban <- readRDS(paste0(dir_grid_nonurban,"20",i,".final.predictions.nonurban.P9.so4.rds"))
    so4_grid_nonurban <- so4_grid_nonurban[so4_grid_nonurban$year==paste0('20',i),]
    so4_grid_nonurban <- so4_grid_nonurban[,c("lon","lat","final.predicted.so4")]
    so4_grid <- rbind(so4_grid_urban,so4_grid_nonurban)
    
    ### aggregate for standard zipcode
    so4_grid_zipcode <- so4_grid
    cty <- shapefile(paste0(dir_shp,"ESRI",i,"USZIP5_POLY_WGS84.shp"))
    coordinates(so4_grid_zipcode)<-~lon+lat
    proj4string(so4_grid_zipcode)<-proj4string(cty)
    so4_grid_zipcode <- point.in.poly(so4_grid_zipcode, cty)
    so4_grid_zipcode <- as.data.frame(so4_grid_zipcode)[,c('ZIP','final.predicted.so4')]
    so4_zip <- so4_grid_zipcode %>% group_by(ZIP) %>% summarise(so4=mean(final.predicted.so4,na.rm=T))
    so4_zip <- so4_zip[!is.na(so4_zip$ZIP),]
    
    ### aggregate for po box
    so4_grid_pobox <- so4_grid
    pobox <- read.csv(paste0(dir_pobox,'ESRI',i,'USZIP5_POINT_WGS84_POBOX.csv'))
    pobox <- pobox[,c(1,3,4)]
    names(pobox) <- c('ZIP','lon','lat')
    link <- nabor::knn(so4_grid_pobox[,c("lon","lat")],pobox[,c("lon","lat")],k=1,radius=2*sqrt(2)/10*0.1)
    link <- cbind.data.frame(link$nn.idx,link$nn.dists)
    names(link) <- c("id","dis")
    pobox$pobox_id <- 1:dim(pobox)[1]
    pobox <- cbind.data.frame(pobox,link)
    so4_grid_pobox$id <- 1:dim(so4_grid_pobox)[1]
    pobox <- left_join(pobox,so4_grid_pobox,by=c("id"))
    pobox <- pobox[pobox$dis!=Inf,]
    pobox <- pobox[,c('ZIP','final.predicted.so4')]
    names(pobox) <- c("ZIP","so4")
    pobox <- pobox[!is.na(pobox$ZIP),]
    pobox$ZIP <- as.character(pobox$ZIP)
    pobox$ZIP <- str_pad(pobox$ZIP, width=5, side="left", pad="0")
    
    ### merge and save
    so4_combine <- rbind(so4_zip,pobox)
    so4_combine <- so4_combine[order(so4_combine$ZIP),]
    saveRDS(so4_combine,file = paste0(dir_save,'so4_20',i,'_P.rds'))
    
    rm(so4_grid_urban_files,so4_grid_urban,so4_grid_nonurban_files,so4_grid_nonurban,so4_grid,so4_grid_zipcode,cty,so4_zip,so4_grid_pobox,pobox,link,so4_combine)
    gc()
  }
  cat(i,'\n')
}



############################# 14. v ##############################
for (i in years_char) {
  if (!file.exists(paste0(dir_save,'v_20',i,'_P.rds'))){
    ### combine urban and nonurban grids
    v_grid_urban_files <- list.files(path=dir_grid_urban,pattern = paste0("^20",i,"(.*)final.predictions.urban.P9.50m.v.rds$"))
    v_grid_urban <- readRDS(paste0(dir_grid_urban,v_grid_urban_files[1]))
    for (j in 2:length(v_grid_urban_files)) {
      v_grid_urban_tmp <- readRDS(paste0(dir_grid_urban,v_grid_urban_files[j]))
      v_grid_urban <- bind_rows(v_grid_urban,v_grid_urban_tmp)
      rm(v_grid_urban_tmp)
      gc()
    }
    v_grid_urban <- v_grid_urban[,c("lon","lat","final.predicted.v")]
    v_grid_nonurban <- readRDS(paste0(dir_grid_nonurban,"20",i,".final.predictions.nonurban.P9.v.rds"))
    v_grid_nonurban <- v_grid_nonurban[v_grid_nonurban$year==paste0('20',i),]
    v_grid_nonurban <- v_grid_nonurban[,c("lon","lat","final.predicted.v")]
    v_grid <- rbind(v_grid_urban,v_grid_nonurban)
    
    ### aggregate for standard zipcode
    v_grid_zipcode <- v_grid
    cty <- shapefile(paste0(dir_shp,"ESRI",i,"USZIP5_POLY_WGS84.shp"))
    coordinates(v_grid_zipcode)<-~lon+lat
    proj4string(v_grid_zipcode)<-proj4string(cty)
    v_grid_zipcode <- point.in.poly(v_grid_zipcode, cty)
    v_grid_zipcode <- as.data.frame(v_grid_zipcode)[,c('ZIP','final.predicted.v')]
    v_zip <- v_grid_zipcode %>% group_by(ZIP) %>% summarise(v=mean(final.predicted.v,na.rm=T))
    v_zip <- v_zip[!is.na(v_zip$ZIP),]
    
    ### aggregate for po box
    v_grid_pobox <- v_grid
    pobox <- read.csv(paste0(dir_pobox,'ESRI',i,'USZIP5_POINT_WGS84_POBOX.csv'))
    pobox <- pobox[,c(1,3,4)]
    names(pobox) <- c('ZIP','lon','lat')
    link <- nabor::knn(v_grid_pobox[,c("lon","lat")],pobox[,c("lon","lat")],k=1,radius=2*sqrt(2)/10*0.1)
    link <- cbind.data.frame(link$nn.idx,link$nn.dists)
    names(link) <- c("id","dis")
    pobox$pobox_id <- 1:dim(pobox)[1]
    pobox <- cbind.data.frame(pobox,link)
    v_grid_pobox$id <- 1:dim(v_grid_pobox)[1]
    pobox <- left_join(pobox,v_grid_pobox,by=c("id"))
    pobox <- pobox[pobox$dis!=Inf,]
    pobox <- pobox[,c('ZIP','final.predicted.v')]
    names(pobox) <- c("ZIP","v")
    pobox <- pobox[!is.na(pobox$ZIP),]
    pobox$ZIP <- as.character(pobox$ZIP)
    pobox$ZIP <- str_pad(pobox$ZIP, width=5, side="left", pad="0")
    
    ### merge and save
    v_combine <- rbind(v_zip,pobox)
    v_combine <- v_combine[order(v_combine$ZIP),]
    saveRDS(v_combine,file = paste0(dir_save,'v_20',i,'_P.rds'))
    
    rm(v_grid_urban_files,v_grid_urban,v_grid_nonurban_files,v_grid_nonurban,v_grid,v_grid_zipcode,cty,v_zip,v_grid_pobox,pobox,link,v_combine)
    gc()
  }
  cat(i,'\n')
}



############################# 15. z ##############################
for (i in years_char) {
  if (!file.exists(paste0(dir_save,'z_20',i,'_P.rds'))){
    ### combine urban and nonurban grids
    z_grid_urban_files <- list.files(path=dir_grid_urban,pattern = paste0("^20",i,"(.*)final.predictions.urban.P9.50m.z.rds$"))
    z_grid_urban <- readRDS(paste0(dir_grid_urban,z_grid_urban_files[1]))
    for (j in 2:length(z_grid_urban_files)) {
      z_grid_urban_tmp <- readRDS(paste0(dir_grid_urban,z_grid_urban_files[j]))
      z_grid_urban <- bind_rows(z_grid_urban,z_grid_urban_tmp)
      rm(z_grid_urban_tmp)
      gc()
    }
    z_grid_urban <- z_grid_urban[,c("lon","lat","final.predicted.z")]
    z_grid_nonurban <- readRDS(paste0(dir_grid_nonurban,"20",i,".final.predictions.nonurban.P9.z.rds"))
    z_grid_nonurban <- z_grid_nonurban[z_grid_nonurban$year==paste0('20',i),]
    z_grid_nonurban <- z_grid_nonurban[,c("lon","lat","final.predicted.z")]
    z_grid <- rbind(z_grid_urban,z_grid_nonurban)
    
    ### aggregate for standard zipcode
    z_grid_zipcode <- z_grid
    cty <- shapefile(paste0(dir_shp,"ESRI",i,"USZIP5_POLY_WGS84.shp"))
    coordinates(z_grid_zipcode)<-~lon+lat
    proj4string(z_grid_zipcode)<-proj4string(cty)
    z_grid_zipcode <- point.in.poly(z_grid_zipcode, cty)
    z_grid_zipcode <- as.data.frame(z_grid_zipcode)[,c('ZIP','final.predicted.z')]
    z_zip <- z_grid_zipcode %>% group_by(ZIP) %>% summarise(z=mean(final.predicted.z,na.rm=T))
    z_zip <- z_zip[!is.na(z_zip$ZIP),]
    
    ### aggregate for po box
    z_grid_pobox <- z_grid
    pobox <- read.csv(paste0(dir_pobox,'ESRI',i,'USZIP5_POINT_WGS84_POBOX.csv'))
    pobox <- pobox[,c(1,3,4)]
    names(pobox) <- c('ZIP','lon','lat')
    link <- nabor::knn(z_grid_pobox[,c("lon","lat")],pobox[,c("lon","lat")],k=1,radius=2*sqrt(2)/10*0.1)
    link <- cbind.data.frame(link$nn.idx,link$nn.dists)
    names(link) <- c("id","dis")
    pobox$pobox_id <- 1:dim(pobox)[1]
    pobox <- cbind.data.frame(pobox,link)
    z_grid_pobox$id <- 1:dim(z_grid_pobox)[1]
    pobox <- left_join(pobox,z_grid_pobox,by=c("id"))
    pobox <- pobox[pobox$dis!=Inf,]
    pobox <- pobox[,c('ZIP','final.predicted.z')]
    names(pobox) <- c("ZIP","z")
    pobox <- pobox[!is.na(pobox$ZIP),]
    pobox$ZIP <- as.character(pobox$ZIP)
    pobox$ZIP <- str_pad(pobox$ZIP, width=5, side="left", pad="0")
    
    ### merge and save
    z_combine <- rbind(z_zip,pobox)
    z_combine <- z_combine[order(z_combine$ZIP),]
    saveRDS(z_combine,file = paste0(dir_save,'z_20',i,'_P.rds'))
    
    rm(z_grid_urban_files,z_grid_urban,z_grid_nonurban_files,z_grid_nonurban,z_grid,z_grid_zipcode,cty,z_zip,z_grid_pobox,pobox,link,z_combine)
    gc()
  }
  cat(i,'\n')
}
