#####################################################################################
# Project: Compress grid PM compositions by year and region                         #
# Author: Yaguang Wei                                                               #
#####################################################################################

############################# 0. Setup load packages ##############################
rm(list=ls())
gc()

library(dplyr)
library(stringr)
library(zip)

dir_NE1_grid_urban <- '/media/qnap3/Exposure modeling/3 National scale/USA/7 PM2.5 speciation v1.2000_19/1 New England/Urban areas at 50m spatial resolution/'
dir_NE1_grid_nonurban <- '/media/qnap3/Exposure modeling/3 National scale/USA/7 PM2.5 speciation v1.2000_19/1 New England/Non-urban areas at 1km spatial resolution/'
dir_MA2_grid_urban <- '/media/qnap3/Exposure modeling/3 National scale/USA/7 PM2.5 speciation v1.2000_19/2 Mid-Atlantic/Urban areas at 50m spatial resolution/'
dir_MA2_grid_nonurban <- '/media/qnap3/Exposure modeling/3 National scale/USA/7 PM2.5 speciation v1.2000_19/2 Mid-Atlantic/Non-urban areas at 1km spatial resolution/'
dir_NEC3_grid_urban <- '/media/qnap3/Exposure modeling/3 National scale/USA/7 PM2.5 speciation v1.2000_19/3 North East Central/Urban areas at 50m spatial resolution/'
dir_NEC3_grid_nonurban <- '/media/qnap3/Exposure modeling/3 National scale/USA/7 PM2.5 speciation v1.2000_19/3 North East Central/Non-urban areas at 1km spatial resolution/'
dir_WNC4_grid_urban <- '/media/qnap3/Exposure modeling/3 National scale/USA/7 PM2.5 speciation v1.2000_19/4 West North Central/Urban areas at 50m spatial resolution/'
dir_WNC4_grid_nonurban <- '/media/qnap3/Exposure modeling/3 National scale/USA/7 PM2.5 speciation v1.2000_19/4 West North Central/Non-urban areas at 1km spatial resolution/'
dir_SA5_grid_urban <- '/media/qnap4/Exposure modeling predictions for PM2.5 composition -- continuation from QNAP3 EM directory/5 South Atlantic/Urban areas at 50m spatial resolution/'
dir_SA5_grid_nonurban <- '/media/qnap3/Exposure modeling/3 National scale/USA/7 PM2.5 speciation v1.2000_19/5 South-Atlantic/Non-urban areas at 1km spatial resolution/'
dir_ESC6_grid_urban <- '/media/qnap3/Exposure modeling/3 National scale/USA/7 PM2.5 speciation v1.2000_19/6 East South Central/Urban areas at 50m spatial resolution/'
dir_ESC6_grid_nonurban <- '/media/qnap3/Exposure modeling/3 National scale/USA/7 PM2.5 speciation v1.2000_19/6 East South Central/Non-urban areas at 1km spatial resolution/'
dir_WSC7_grid_urban <- '/media/qnap4/Exposure modeling predictions for PM2.5 composition -- continuation from QNAP3 EM directory/7 West South Central/Urban areas at 50m spatial resolution/'
dir_WSC7_grid_nonurban <- '/media/qnap3/Exposure modeling/3 National scale/USA/7 PM2.5 speciation v1.2000_19/7 West South Central/Non-urban areas at 1km spatial resolution/'
dir_M8_grid_urban <- '/media/qnap4/Exposure modeling predictions for PM2.5 composition -- continuation from QNAP3 EM directory/8 Mountain/Urban areas at 50m spatial resolution/'
dir_M8_grid_nonurban <- '/media/qnap3/Exposure modeling/3 National scale/USA/7 PM2.5 speciation v1.2000_19/8 Mountain/Non-urban areas at 1km spatial resolution/'
dir_P9_grid_urban <- '/media/qnap4/Exposure modeling predictions for PM2.5 composition -- continuation from QNAP3 EM directory/9 Pacific/Urban areas at 50m spatial resolution/'
dir_P9_grid_nonurban <- '/media/qnap3/Exposure modeling/3 National scale/USA/7 PM2.5 speciation v1.2000_19/9 Pacific/Non-urban areas at 1km spatial resolution/'
dir_save <- '/media/qnap4/Yaguang/Aggregate_PM_Composition/Compressed/'

years_char <- c('00','01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19')



############################# 1. NE ##############################
## urban
setwd(dir_NE1_grid_urban)
n <- list.files(path=dir_NE1_grid_urban)
for (i in years_char) {
  files_tmp <- list.files(path=dir_NE1_grid_urban,pattern = paste0("^20",i,"(.*).rds$"))
  zip::zipr(zipfile = paste0(dir_save,"20",i,".urban.NE1.zip"), files = files_tmp)
  cat(paste0("Year 20", i, " done\n"))
  gc()
}



############################# 2. MA ##############################
## urban
setwd(dir_MA2_grid_urban)
n <- list.files(path=dir_MA2_grid_urban)
for (i in years_char) {
  files_tmp <- list.files(path=dir_MA2_grid_urban,pattern = paste0("^20",i,"(.*).rds$"))
  zip::zipr(zipfile = paste0(dir_save,"20",i,".urban.MA2.zip"), files = files_tmp)
  cat(paste0("Year 20", i, " done\n"))
  gc()
}



############################# 3. NEC ##############################
## urban
setwd(dir_NEC3_grid_urban)
n <- list.files(path=dir_NEC3_grid_urban)
for (i in years_char) {
  files_tmp <- list.files(path=dir_NEC3_grid_urban,pattern = paste0("^20",i,"(.*).rds$"))
  zip::zipr(zipfile = paste0(dir_save,"20",i,".urban.NEC3.zip"), files = files_tmp)
  cat(paste0("Year 20", i, " done\n"))
  gc()
}



############################# 4. WNC ##############################
## urban
setwd(dir_WNC4_grid_urban)
n <- list.files(path=dir_WNC4_grid_urban)
for (i in years_char) {
  files_tmp <- list.files(path=dir_WNC4_grid_urban,pattern = paste0("^20",i,"(.*).rds$"))
  zip::zipr(zipfile = paste0(dir_save,"20",i,".urban.WNC4.zip"), files = files_tmp)
  cat(paste0("Year 20", i, " done\n"))
  gc()
}



############################# 5. SA ##############################
## urban
setwd(dir_SA5_grid_urban)
n <- list.files(path=dir_SA5_grid_urban)
for (i in years_char) {
  files_tmp <- list.files(path=dir_SA5_grid_urban,pattern = paste0("^20",i,"(.*).rds$"))
  zip::zipr(zipfile = paste0(dir_save,"20",i,".urban.SA5.zip"), files = files_tmp)
  cat(paste0("Year 20", i, " done\n"))
  gc()
}



############################# 6. ESC ##############################
## urban
setwd(dir_ESC6_grid_urban)
n <- list.files(path=dir_ESC6_grid_urban)
for (i in years_char) {
  files_tmp <- list.files(path=dir_ESC6_grid_urban,pattern = paste0("^20",i,"(.*).rds$"))
  zip::zipr(zipfile = paste0(dir_save,"20",i,".urban.ESC6.zip"), files = files_tmp)
  cat(paste0("Year 20", i, " done\n"))
  gc()
}



############################# 7. WSC ##############################
## urban
setwd(dir_WSC7_grid_urban)
n <- list.files(path=dir_WSC7_grid_urban)
for (i in years_char) {
  files_tmp <- list.files(path=dir_WSC7_grid_urban,pattern = paste0("^20",i,"(.*).rds$"))
  zip::zipr(zipfile = paste0(dir_save,"20",i,".urban.WSC7.zip"), files = files_tmp)
  cat(paste0("Year 20", i, " done\n"))
  gc()
}



############################# 8. M ##############################
## urban
setwd(dir_M8_grid_urban)
n <- list.files(path=dir_M8_grid_urban)
for (i in years_char) {
  files_tmp <- list.files(path=dir_M8_grid_urban,pattern = paste0("^20",i,"(.*).rds$"))
  zip::zipr(zipfile = paste0(dir_save,"20",i,".urban.M8.zip"), files = files_tmp)
  cat(paste0("Year 20", i, " done\n"))
  gc()
}



############################# 9. P ##############################
## urban
setwd(dir_P9_grid_urban)
n <- list.files(path=dir_P9_grid_urban)
for (i in years_char) {
  files_tmp <- list.files(path=dir_P9_grid_urban,pattern = paste0("^20",i,"(.*).rds$"))
  zip::zipr(zipfile = paste0(dir_save,"20",i,".urban.P9.zip"), files = files_tmp)
  cat(paste0("Year 20", i, " done\n"))
  gc()
}