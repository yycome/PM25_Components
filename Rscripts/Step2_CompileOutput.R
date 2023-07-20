#####################################################################################
# Project: Aggregate Heresh's compositions from grids to zip codes                  #
# Code: Compile                                                                     #
# Author: Yaguang Wei                                                               #
#####################################################################################

############################# 0. Setup load packages ##############################
rm(list=ls())
gc()

library(dplyr)
library(stringr)

dir_output <- '/media/qnap4/Yaguang/Aggregate_PM_Composition/output/'
dir_compile <- '/media/qnap4/Yaguang/Aggregate_PM_Composition/compile/'
dir_save <- '/media/qnap3/Exposure modeling/3 National scale/USA/7 PM2.5 speciation v1.2000_19/Zipcode_average/'

years_char <- c('00','01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19')
components <- c('br','ca','cu','ec','fe','k','nh4','ni','no3','oc','pb','si','so4','v','z')
regions <- c('NE','MA','NEC','WNC','SA','ESC','WSC','M','P')



############################# 1. Bind columns by year and region ##############################
for (i in 1:length(years_char)) {
  for (j in 1:length(regions)) {
    file_list_tmp <- list.files(path=dir_output,pattern = paste0("^(.*)_20",years_char[i],"_",regions[j],".rds"))
    dat_tmp_merged <- readRDS(paste0(dir_output,file_list_tmp[1])) 
    dat_tmp_merged <- dat_tmp_merged[!duplicated(dat_tmp_merged$ZIP),]
    for (k in 2:length(file_list_tmp)) {
      dat_tmp <- readRDS(paste0(dir_output,file_list_tmp[k])) 
      dat_tmp <- dat_tmp[!duplicated(dat_tmp$ZIP),]
      dat_tmp_merged <- left_join(dat_tmp_merged,dat_tmp,by='ZIP')
      rm(dat_tmp)
    }
    saveRDS(dat_tmp_merged,file=paste0(dir_compile,'20',years_char[i],"_",regions[j],".rds"))
    rm(dat_tmp_merged);gc()
  }
  cat(paste0("year 20", years_char[i], " complete \n"))
}



############################# 2. Bind rows by year ##############################
for (i in 1:length(years_char)) {
  file_list_tmp <- list.files(path=dir_compile,pattern = paste0("^20",years_char[i],"(.*).rds"))
  dat_tmp_merged <- readRDS(paste0(dir_compile,file_list_tmp[1]))
  dat_tmp_merged <- dat_tmp_merged[!duplicated(dat_tmp_merged$ZIP),]
  for (k in 2:length(file_list_tmp)) {
    dat_tmp <- readRDS(paste0(dir_compile,file_list_tmp[k])) 
    dat_tmp <- dat_tmp[!duplicated(dat_tmp$ZIP),]
    dat_tmp_merged <- bind_rows(dat_tmp_merged,dat_tmp)
    rm(dat_tmp)
  }
  dat_tmp_merged <- dat_tmp_merged[!duplicated(dat_tmp_merged$ZIP),]
  saveRDS(dat_tmp_merged,file=paste0(dir_save,'20',years_char[i],".rds"))
  rm(dat_tmp_merged);gc()
  cat(paste0("year 20", years_char[i], " complete \n"))
}
