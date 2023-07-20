#####################################################################################
# Project: Aggregate Heresh's compositions from grids to zip codes                  #
# Code: Validation                                                                  #
# Author: Yaguang Wei                                                               #
#####################################################################################

############################# 0. Setup load packages ##############################
rm(list=ls())
gc()

library(dplyr)
library(stringr)

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

dir_save <- '/media/qnap4/Yaguang/Aggregate_PM_Composition/output/'
dir_sumnmary <- '/media/qnap4/Yaguang/Aggregate_PM_Composition/'
dir_final <- '/media/qnap3/Exposure modeling/3 National scale/USA/7 PM2.5 speciation v1.2000_19/Zipcode_average/'

years_char <- c('00','01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19')

components <- c('br','ca','cu','ec','fe','k','nh4','ni','no3','oc','pb','si','so4','v','z')

regions <- c('NE','MA','NEC','WNC','SA','ESC','WSC','M','P')



######################## 1. Check number of grid files for each component in each region  ######################
### NE1
files_NE1_grid_urban <- data.frame(matrix(NA, nrow=length(components)*length(years_char), ncol=4))
names(files_NE1_grid_urban) <- c('region','year','component','n_files')
for (i in 1:length(years_char)) {
  for (j in 1:length(components)) {
    files_NE1_grid_urban[(i-1)*length(components)+j,'region'] <- 'NE1'
    files_NE1_grid_urban[(i-1)*length(components)+j,'year'] <- years_char[i]
    files_NE1_grid_urban[(i-1)*length(components)+j,'component'] <- components[j]
    files_NE1_grid_urban[(i-1)*length(components)+j,'n_files'] <- length(list.files(path=dir_NE1_grid_urban,pattern = paste0("^20",years_char[i],"(.*)final.predictions.urban.NE1.50m.",components[j],".rds$")))
  }
}
summary(files_NE1_grid_urban$n_files)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1       1       1       1       1       1 

files_NE1_grid_nonurban <- data.frame(matrix(NA, nrow=length(components), ncol=3))
names(files_NE1_grid_nonurban) <- c('region','component','n_files')
for (j in 1:length(components)) {
  files_NE1_grid_nonurban[j,'region'] <- 'NE1'
  files_NE1_grid_nonurban[j,'component'] <- components[j]
  files_NE1_grid_nonurban[j,'n_files'] <- length(list.files(path=dir_NE1_grid_nonurban,pattern = paste0("^final.predictions.nonurban.NE1(.*)",components[j],".rds$")))
}
summary(files_NE1_grid_nonurban$n_files)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1       1       1       1       1       1 


### MA2
files_MA2_grid_urban <- data.frame(matrix(NA, nrow=length(components)*length(years_char), ncol=4))
names(files_MA2_grid_urban) <- c('region','year','component','n_files')
for (i in 1:length(years_char)) {
  for (j in 1:length(components)) {
    files_MA2_grid_urban[(i-1)*length(components)+j,'region'] <- 'MA2'
    files_MA2_grid_urban[(i-1)*length(components)+j,'year'] <- years_char[i]
    files_MA2_grid_urban[(i-1)*length(components)+j,'component'] <- components[j]
    files_MA2_grid_urban[(i-1)*length(components)+j,'n_files'] <- length(list.files(path=dir_MA2_grid_urban,pattern = paste0("^20",years_char[i],".final.predictions.urban.MA2.50m.",components[j],"(.*).rds$")))
  }
}
summary(files_MA2_grid_urban$n_files)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   1.000   1.000   1.003   1.000   2.000

files_MA2_grid_nonurban <- data.frame(matrix(NA, nrow=length(components), ncol=3))
names(files_MA2_grid_nonurban) <- c('region','component','n_files')
for (j in 1:length(components)) {
  files_MA2_grid_nonurban[j,'region'] <- 'MA2'
  files_MA2_grid_nonurban[j,'component'] <- components[j]
  files_MA2_grid_nonurban[j,'n_files'] <- length(list.files(path=dir_MA2_grid_nonurban,pattern = paste0("^final.predictions.nonurban.MA2(.*)",components[j],".rds$")))
}
summary(files_MA2_grid_nonurban$n_files)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1       1       1       1       1       1 


### NEC3
files_NEC3_grid_urban <- data.frame(matrix(NA, nrow=length(components)*length(years_char), ncol=4))
names(files_NEC3_grid_urban) <- c('region','year','component','n_files')
for (i in 1:length(years_char)) {
  for (j in 1:length(components)) {
    files_NEC3_grid_urban[(i-1)*length(components)+j,'region'] <- 'NEC3'
    files_NEC3_grid_urban[(i-1)*length(components)+j,'year'] <- years_char[i]
    files_NEC3_grid_urban[(i-1)*length(components)+j,'component'] <- components[j]
    files_NEC3_grid_urban[(i-1)*length(components)+j,'n_files'] <- length(list.files(path=dir_NEC3_grid_urban,pattern = paste0("^20",years_char[i],"(.*)final.predictions.urban.NEC3.50m.",components[j],".rds$")))
  }
}
summary(files_NEC3_grid_urban$n_files)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4       4       4       4       4       4

files_NEC3_grid_nonurban <- data.frame(matrix(NA, nrow=length(components), ncol=3))
names(files_NEC3_grid_nonurban) <- c('region','component','n_files')
for (j in 1:length(components)) {
  files_NEC3_grid_nonurban[j,'region'] <- 'NEC3'
  files_NEC3_grid_nonurban[j,'component'] <- components[j]
  files_NEC3_grid_nonurban[j,'n_files'] <- length(list.files(path=dir_NEC3_grid_nonurban,pattern = paste0("^final.predictions.nonurban.NEC3(.*)",components[j],".rds$")))
}
summary(files_NEC3_grid_nonurban$n_files)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2       2       2       2       2       2 


### WNC4
files_WNC4_grid_urban <- data.frame(matrix(NA, nrow=length(components)*length(years_char), ncol=4))
names(files_WNC4_grid_urban) <- c('region','year','component','n_files')
for (i in 1:length(years_char)) {
  for (j in 1:length(components)) {
    files_WNC4_grid_urban[(i-1)*length(components)+j,'region'] <- 'WNC4'
    files_WNC4_grid_urban[(i-1)*length(components)+j,'year'] <- years_char[i]
    files_WNC4_grid_urban[(i-1)*length(components)+j,'component'] <- components[j]
    files_WNC4_grid_urban[(i-1)*length(components)+j,'n_files'] <- length(list.files(path=dir_WNC4_grid_urban,pattern = paste0("^20",years_char[i],"(.*)final.predictions.urban.WNC4.50m.",components[j],".rds$")))
  }
}
summary(files_WNC4_grid_urban$n_files)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2       2       2       2       2       2 

files_WNC4_grid_nonurban <- data.frame(matrix(NA, nrow=length(components), ncol=3))
names(files_WNC4_grid_nonurban) <- c('region','component','n_files')
for (j in 1:length(components)) {
  files_WNC4_grid_nonurban[j,'region'] <- 'WNC4'
  files_WNC4_grid_nonurban[j,'component'] <- components[j]
  files_WNC4_grid_nonurban[j,'n_files'] <- length(list.files(path=dir_WNC4_grid_nonurban,pattern = paste0("^final.predictions.nonurban.WNC4(.*)",components[j],".rds$")))
}
summary(files_WNC4_grid_nonurban$n_files)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4       4       4       4       4       4 


### SA5
files_SA5_grid_urban <- data.frame(matrix(NA, nrow=length(components)*length(years_char), ncol=4))
names(files_SA5_grid_urban) <- c('region','year','component','n_files')
for (i in 1:length(years_char)) {
  for (j in 1:length(components)) {
    files_SA5_grid_urban[(i-1)*length(components)+j,'region'] <- 'SA5'
    files_SA5_grid_urban[(i-1)*length(components)+j,'year'] <- years_char[i]
    files_SA5_grid_urban[(i-1)*length(components)+j,'component'] <- components[j]
    files_SA5_grid_urban[(i-1)*length(components)+j,'n_files'] <- length(list.files(path=dir_SA5_grid_urban,pattern = paste0("^20",years_char[i],"(.*)final.predictions.urban.SA5.50m.",components[j],".rds$")))
  }
}
summary(files_SA5_grid_urban$n_files)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6       6       6       6       6       6

files_SA5_grid_nonurban <- data.frame(matrix(NA, nrow=length(components), ncol=3))
names(files_SA5_grid_nonurban) <- c('region','component','n_files')
for (j in 1:length(components)) {
  files_SA5_grid_nonurban[j,'region'] <- 'SA5'
  files_SA5_grid_nonurban[j,'component'] <- components[j]
  files_SA5_grid_nonurban[j,'n_files'] <- length(list.files(path=dir_SA5_grid_nonurban,pattern = paste0("^final.predictions.nonurban.SA5(.*)",components[j],".rds$")))
}
summary(files_SA5_grid_nonurban$n_files)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2       2       2       2       2       2 


### ESC6
files_ESC6_grid_urban <- data.frame(matrix(NA, nrow=length(components)*length(years_char), ncol=4))
names(files_ESC6_grid_urban) <- c('region','year','component','n_files')
for (i in 1:length(years_char)) {
  for (j in 1:length(components)) {
    files_ESC6_grid_urban[(i-1)*length(components)+j,'region'] <- 'ESC6'
    files_ESC6_grid_urban[(i-1)*length(components)+j,'year'] <- years_char[i]
    files_ESC6_grid_urban[(i-1)*length(components)+j,'component'] <- components[j]
    files_ESC6_grid_urban[(i-1)*length(components)+j,'n_files'] <- length(list.files(path=dir_ESC6_grid_urban,pattern = paste0("^20",years_char[i],"(.*)final.predictions.urban.ESC6.50m.",components[j],".rds$")))
  }
}
summary(files_ESC6_grid_urban$n_files)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2       2       2       2       2       2

files_ESC6_grid_nonurban <- data.frame(matrix(NA, nrow=length(components), ncol=3))
names(files_ESC6_grid_nonurban) <- c('region','component','n_files')
for (j in 1:length(components)) {
  files_ESC6_grid_nonurban[j,'region'] <- 'ESC6'
  files_ESC6_grid_nonurban[j,'component'] <- components[j]
  files_ESC6_grid_nonurban[j,'n_files'] <- length(list.files(path=dir_ESC6_grid_nonurban,pattern = paste0("^final.predictions.nonurban.ESC6(.*)",components[j],".rds$")))
}
summary(files_ESC6_grid_nonurban$n_files)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1       1       1       1       1       1 


### WSC7
files_WSC7_grid_urban <- data.frame(matrix(NA, nrow=length(components)*length(years_char), ncol=4))
names(files_WSC7_grid_urban) <- c('region','year','component','n_files')
for (i in 1:length(years_char)) {
  for (j in 1:length(components)) {
    files_WSC7_grid_urban[(i-1)*length(components)+j,'region'] <- 'WSC7'
    files_WSC7_grid_urban[(i-1)*length(components)+j,'year'] <- years_char[i]
    files_WSC7_grid_urban[(i-1)*length(components)+j,'component'] <- components[j]
    files_WSC7_grid_urban[(i-1)*length(components)+j,'n_files'] <- length(list.files(path=dir_WSC7_grid_urban,pattern = paste0("^20",years_char[i],"(.*)final.predictions.urban.WSC7.50m.",components[j],".rds$")))
  }
}
summary(files_WSC7_grid_urban$n_files)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3       3       3       3       3       3 

files_WSC7_grid_nonurban <- data.frame(matrix(NA, nrow=length(components), ncol=3))
names(files_WSC7_grid_nonurban) <- c('region','component','n_files')
for (j in 1:length(components)) {
  files_WSC7_grid_nonurban[j,'region'] <- 'WSC7'
  files_WSC7_grid_nonurban[j,'component'] <- components[j]
  files_WSC7_grid_nonurban[j,'n_files'] <- length(list.files(path=dir_WSC7_grid_nonurban,pattern = paste0("^final.predictions.nonurban.WSC7(.*)",components[j],".rds$")))
}
summary(files_WSC7_grid_nonurban$n_files)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4       4       4       4       4       4


### M8
files_M8_grid_urban <- data.frame(matrix(NA, nrow=length(components)*length(years_char), ncol=4))
names(files_M8_grid_urban) <- c('region','year','component','n_files')
for (i in 1:length(years_char)) {
  for (j in 1:length(components)) {
    files_M8_grid_urban[(i-1)*length(components)+j,'region'] <- 'M8'
    files_M8_grid_urban[(i-1)*length(components)+j,'year'] <- years_char[i]
    files_M8_grid_urban[(i-1)*length(components)+j,'component'] <- components[j]
    files_M8_grid_urban[(i-1)*length(components)+j,'n_files'] <- length(list.files(path=dir_M8_grid_urban,pattern = paste0("^20",years_char[i],"(.*)final.predictions.urban.M8.50m.",components[j],".rds$")))
  }
}
summary(files_M8_grid_urban$n_files)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    2.00    2.00    1.84    2.00    2.00

files_M8_grid_nonurban <- data.frame(matrix(NA, nrow=length(components)*length(years_char), ncol=4))
names(files_M8_grid_nonurban) <- c('region','year','component','n_files')
for (i in 1:length(years_char)) {
  for (j in 1:length(components)) {
    files_M8_grid_nonurban[(i-1)*length(components)+j,'region'] <- 'M8'
    files_M8_grid_nonurban[(i-1)*length(components)+j,'year'] <- years_char[i]
    files_M8_grid_nonurban[(i-1)*length(components)+j,'component'] <- components[j]
    files_M8_grid_nonurban[(i-1)*length(components)+j,'n_files'] <- length(list.files(path=dir_M8_grid_nonurban,pattern = paste0("^20",years_char[i],"(.*)final.predictions.nonurban.M8(.*)",components[j],".rds$")))
  }
}
summary(files_M8_grid_nonurban$n_files)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1       1       1       1       1       1 


### P9
files_P9_grid_urban <- data.frame(matrix(NA, nrow=length(components)*length(years_char), ncol=4))
names(files_P9_grid_urban) <- c('region','year','component','n_files')
for (i in 1:length(years_char)) {
  for (j in 1:length(components)) {
    files_P9_grid_urban[(i-1)*length(components)+j,'region'] <- 'P9'
    files_P9_grid_urban[(i-1)*length(components)+j,'year'] <- years_char[i]
    files_P9_grid_urban[(i-1)*length(components)+j,'component'] <- components[j]
    files_P9_grid_urban[(i-1)*length(components)+j,'n_files'] <- length(list.files(path=dir_P9_grid_urban,pattern = paste0("^20",years_char[i],"(.*)final.predictions.urban.P9.50m.",components[j],".rds$")))
  }
}
summary(files_P9_grid_urban$n_files)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3       3       3       3       3       3 

files_P9_grid_nonurban <- data.frame(matrix(NA, nrow=length(components)*length(years_char), ncol=4))
names(files_P9_grid_nonurban) <- c('region','year','component','n_files')
for (i in 1:length(years_char)) {
  for (j in 1:length(components)) {
    files_P9_grid_nonurban[(i-1)*length(components)+j,'region'] <- 'P9'
    files_P9_grid_nonurban[(i-1)*length(components)+j,'year'] <- years_char[i]
    files_P9_grid_nonurban[(i-1)*length(components)+j,'component'] <- components[j]
    files_P9_grid_nonurban[(i-1)*length(components)+j,'n_files'] <- length(list.files(path=dir_P9_grid_nonurban,pattern = paste0("^20",years_char[i],"(.*)final.predictions.nonurban.P9.",components[j],".rds$")))
  }
}
summary(files_P9_grid_nonurban$n_files)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1       1       1       1       1       1 



######################## 2. Check number of zipcode files for each component in each region  ######################
files_zipcode <- data.frame(matrix(NA, nrow=length(components)*length(years_char)*length(regions), ncol=4))
names(files_zipcode) <- c('region','year','component','n_files')
for (i in 1:length(years_char)) {
  for (j in 1:length(components)) {
    for (k in 1:length(regions)) {
      files_zipcode[(i-1)*length(components)*length(regions)+(j-1)*length(regions)+k,'region'] <- regions[k]
      files_zipcode[(i-1)*length(components)*length(regions)+(j-1)*length(regions)+k,'year'] <- years_char[i]
      files_zipcode[(i-1)*length(components)*length(regions)+(j-1)*length(regions)+k,'component'] <- components[j]
      files_zipcode[(i-1)*length(components)*length(regions)+(j-1)*length(regions)+k,'n_files'] <- length(list.files(path=dir_save,pattern = paste0(components[j],"_20",years_char[i],"_",regions[k],".rds")))
    }
  }
}
summary(files_zipcode)
#   region              year            component            n_files 
# Length:2700        Length:2700        Length:2700        Min.   :1  
# Class :character   Class :character   Class :character   1st Qu.:1  
# Mode  :character   Mode  :character   Mode  :character   Median :1  
#                                                          Mean   :1  
#                                                          3rd Qu.:1  
#                                                          Max.   :1 

files_zipcode <- files_zipcode[order(files_zipcode$region,files_zipcode$year),]
files_zipcode_missing <- files_zipcode[files_zipcode$n_files==0,]

# saveRDS(files_zipcode_missing,file = paste0(dir_sumnmary,'components_cannot_be_aggregated.rds'))



############################# 3. Validate zipcode files ##############################
files <- list.files(path=dir_save)

n_zip_missing <- data.frame(matrix(NA, nrow=length(files), ncol=6))
names(n_zip_missing) <- c('component','year','region','n_zip','n_unique','n_missing')
for (i in 1:length(files)) {
  n_zip_missing[i,1] <- sub("\\_.*", "", files[i])
  n_zip_missing[i,2] <- ifelse(length(str_extract_all(files[i],"[[:digit:]]+")[[1]])>1,
                               str_extract_all(files[i],"[[:digit:]]+")[[1]][2],
                               str_extract_all(files[i],"[[:digit:]]+")[[1]])
  pattern <- paste0(sub("\\_.*", "", files[i]),"_",
                    ifelse(length(str_extract_all(files[i],"[[:digit:]]+")[[1]])>1,
                           str_extract_all(files[i],"[[:digit:]]+")[[1]][2],
                           str_extract_all(files[i],"[[:digit:]]+")[[1]]),"_")
  region <- str_remove(files[i], pattern)
  region <- str_remove(region, '.rds')
  n_zip_missing[i,3] <- region
  n_zip_missing[i,4] <- dim(readRDS(paste0(dir_save,files[i])))[1]
  n_zip_missing[i,5] <- length(unlist(unique(readRDS(paste0(dir_save,files[i]))[,1])))
  n_zip_missing[i,6] <- sum(is.na(readRDS(paste0(dir_save,files[i]))[,2]))
  if( i %% 100 == 0 ) cat(paste("iteration", i, "complete\n"))
}
n_zip_missing$year <- as.numeric(n_zip_missing$year)
n_zip_missing$duplicates <- n_zip_missing$n_zip-n_zip_missing$n_unique
summary(n_zip_missing)
# component              year         region              n_zip         n_unique      n_missing   duplicates     
# Length:2700        Min.   :2000   Length:2700        Min.   :2293   Min.   :2293   Min.   :0   Min.   :0.00000  
# Class :character   1st Qu.:2005   Class :character   1st Qu.:3220   1st Qu.:3220   1st Qu.:0   1st Qu.:0.00000  
# Mode  :character   Median :2010   Mode  :character   Median :4847   Median :4846   Median :0   Median :0.00000  
#                    Mean   :2010                      Mean   :4607   Mean   :4607   Mean   :0   Mean   :0.06667  
#                    3rd Qu.:2014                      3rd Qu.:5470   3rd Qu.:5470   3rd Qu.:0   3rd Qu.:0.00000  
#                    Max.   :2019                      Max.   :7261   Max.   :7261   Max.   :0   Max.   :2.00000  

n_zip_missing_problem <- n_zip_missing[n_zip_missing$n_missing>10,]

# saveRDS(n_zip_missing_problem,file = paste0(dir_sumnmary,'aggregated_zipcode_missingness.rds'))



################ 4. Check whether grid files for NEC, WSC, and M are readable ###############
files_NEC3_grid_urban <- list.files(path=dir_NEC3_grid_urban)
file_broken_NEC3_grid_urban <- data.frame(matrix(NA, nrow=length(files_NEC3_grid_urban), ncol=4))
names(file_broken_NEC3_grid_urban) <- c('region','readable','pred_value','n_missing')
for (i in 1:length(files_NEC3_grid_urban)) {
  file_broken_NEC3_grid_urban[i,'region'] <- 'NEC'
  file_broken_NEC3_grid_urban[i,'readable'] <- tryCatch(length(class(readRDS(paste0(dir_NEC3_grid_urban,files_NEC3_grid_urban[i])))), error=function(err) 0)
  file_broken_NEC3_grid_urban[i,'pred_value'] <- tryCatch(grepl("final.predicted",toString(names(readRDS(paste0(dir_NEC3_grid_urban,files_NEC3_grid_urban[i])))),
                                                                fixed=TRUE), error=function(err) NA)
  file_broken_NEC3_grid_urban[i,'n_missing'] <- tryCatch(sum(is.na(readRDS(paste0(dir_NEC3_grid_urban,files_NEC3_grid_urban[i]))[,grep("final.predicted",names(readRDS(paste0(dir_NEC3_grid_urban,files_NEC3_grid_urban[i]))),value=TRUE)])),
                                                         error=function(err) NA)
  if( i %% 100 == 0 ) cat(paste("iteration", i, "complete\n"))
  gc()
}

files_NEC3_grid_nonurban <- list.files(path=dir_NEC3_grid_nonurban)
file_broken_NEC3_grid_nonurban <- data.frame(matrix(NA, nrow=length(files_NEC3_grid_nonurban), ncol=4))
names(file_broken_NEC3_grid_nonurban) <- c('region','readable','pred_value','n_missing')
for (i in 1:length(files_NEC3_grid_nonurban)) {
  file_broken_NEC3_grid_nonurban[i,'region'] <- 'NEC'
  file_broken_NEC3_grid_nonurban[i,'readable'] <- tryCatch(length(class(readRDS(paste0(dir_NEC3_grid_nonurban,files_NEC3_grid_nonurban[i])))), error=function(err) 0)
  file_broken_NEC3_grid_nonurban[i,'pred_value'] <- tryCatch(grepl("final.predicted",toString(names(readRDS(paste0(dir_NEC3_grid_nonurban,files_NEC3_grid_nonurban[i])))),
                                                                   fixed=TRUE), error=function(err) NA)
  file_broken_NEC3_grid_nonurban[i,'n_missing'] <- tryCatch(sum(is.na(readRDS(paste0(dir_NEC3_grid_nonurban,files_NEC3_grid_nonurban[i]))[,grep("final.predicted",names(readRDS(paste0(dir_NEC3_grid_nonurban,files_NEC3_grid_nonurban[i]))),value=TRUE)])),
                                                            error=function(err) NA)
  if( i %% 100 == 0 ) cat(paste("iteration", i, "complete\n"))
  gc()
}

files_WSC7_grid_urban <- list.files(path=dir_WSC7_grid_urban)
file_broken_WSC7_grid_urban <- data.frame(matrix(NA, nrow=length(files_WSC7_grid_urban), ncol=4))
names(file_broken_WSC7_grid_urban) <- c('region','readable','pred_value','n_missing')
for (i in 1:length(files_WSC7_grid_urban)) {
  file_broken_WSC7_grid_urban[i,'region'] <- 'WSC'
  file_broken_WSC7_grid_urban[i,'readable'] <- tryCatch(length(class(readRDS(paste0(dir_WSC7_grid_urban,files_WSC7_grid_urban[i])))), error=function(err) 0)
  file_broken_WSC7_grid_urban[i,'pred_value'] <- tryCatch(grepl("final.predicted",toString(names(readRDS(paste0(dir_WSC7_grid_urban,files_WSC7_grid_urban[i])))),
                                                                fixed=TRUE), error=function(err) NA)
  file_broken_WSC7_grid_urban[i,'n_missing'] <- tryCatch(sum(is.na(readRDS(paste0(dir_WSC7_grid_urban,files_WSC7_grid_urban[i]))[,grep("final.predicted",names(readRDS(paste0(dir_WSC7_grid_urban,files_WSC7_grid_urban[i]))),value=TRUE)])),
                                                         error=function(err) NA)
  if( i %% 100 == 0 ) cat(paste("iteration", i, "complete\n"))
  gc()
}

files_WSC7_grid_nonurban <- list.files(path=dir_WSC7_grid_nonurban)
file_broken_WSC7_grid_nonurban <- data.frame(matrix(NA, nrow=length(files_WSC7_grid_nonurban), ncol=4))
names(file_broken_WSC7_grid_nonurban) <- c('region','readable','pred_value','n_missing')
for (i in 1:length(files_WSC7_grid_nonurban)) {
  file_broken_WSC7_grid_nonurban[i,'region'] <- 'WSC'
  file_broken_WSC7_grid_nonurban[i,'readable'] <- tryCatch(length(class(readRDS(paste0(dir_WSC7_grid_nonurban,files_WSC7_grid_nonurban[i])))), error=function(err) 0)
  file_broken_WSC7_grid_nonurban[i,'pred_value'] <- tryCatch(grepl("final.predicted",toString(names(readRDS(paste0(dir_WSC7_grid_nonurban,files_WSC7_grid_nonurban[i])))),
                                                                fixed=TRUE), error=function(err) NA)
  file_broken_WSC7_grid_nonurban[i,'n_missing'] <- tryCatch(sum(is.na(readRDS(paste0(dir_WSC7_grid_nonurban,files_WSC7_grid_nonurban[i]))[,grep("final.predicted",names(readRDS(paste0(dir_WSC7_grid_nonurban,files_WSC7_grid_nonurban[i]))),value=TRUE)])),
                                                         error=function(err) NA)
  if( i %% 100 == 0 ) cat(paste("iteration", i, "complete\n"))
  gc()
}

files_M8_grid_urban <- list.files(path=dir_M8_grid_urban)
file_broken_M8_grid_urban <- data.frame(matrix(NA, nrow=length(files_M8_grid_urban), ncol=4))
names(file_broken_M8_grid_urban) <- c('region','readable','pred_value','n_missing')
for (i in 1:length(files_M8_grid_urban)) {
  file_broken_M8_grid_urban[i,'region'] <- 'M'
  file_broken_M8_grid_urban[i,'readable'] <- tryCatch(length(class(readRDS(paste0(dir_M8_grid_urban,files_M8_grid_urban[i])))), error=function(err) 0)
  file_broken_M8_grid_urban[i,'pred_value'] <- tryCatch(grepl("final.predicted",toString(names(readRDS(paste0(dir_M8_grid_urban,files_M8_grid_urban[i])))),
                                                                fixed=TRUE), error=function(err) NA)
  file_broken_M8_grid_urban[i,'n_missing'] <- tryCatch(sum(is.na(readRDS(paste0(dir_M8_grid_urban,files_M8_grid_urban[i]))[,grep("final.predicted",names(readRDS(paste0(dir_M8_grid_urban,files_M8_grid_urban[i]))),value=TRUE)])),
                                                         error=function(err) NA)
  if( i %% 100 == 0 ) cat(paste("iteration", i, "complete\n"))
  gc()
}

files_M8_grid_nonurban <- list.files(path=dir_M8_grid_nonurban)
file_broken_M8_grid_nonurban <- data.frame(matrix(NA, nrow=length(files_M8_grid_nonurban), ncol=4))
names(file_broken_M8_grid_nonurban) <- c('region','readable','pred_value','n_missing')
for (i in 1:length(files_M8_grid_nonurban)) {
  file_broken_M8_grid_nonurban[i,'region'] <- 'M'
  file_broken_M8_grid_nonurban[i,'readable'] <- tryCatch(length(class(readRDS(paste0(dir_M8_grid_nonurban,files_M8_grid_nonurban[i])))), error=function(err) 0)
  file_broken_M8_grid_nonurban[i,'pred_value'] <- tryCatch(grepl("final.predicted",toString(names(readRDS(paste0(dir_M8_grid_nonurban,files_M8_grid_nonurban[i])))),
                                                              fixed=TRUE), error=function(err) NA)
  file_broken_M8_grid_nonurban[i,'n_missing'] <- tryCatch(sum(is.na(readRDS(paste0(dir_M8_grid_nonurban,files_M8_grid_nonurban[i]))[,grep("final.predicted",names(readRDS(paste0(dir_M8_grid_nonurban,files_M8_grid_nonurban[i]))),value=TRUE)])),
                                                       error=function(err) NA)
  if( i %% 100 == 0 ) cat(paste("iteration", i, "complete\n"))
  gc()
}



################ 5. Delete zipcode files for NEC, WSC, and M ###############
files_NEC <- list.files(path=dir_save,pattern = "^(.*)_NEC.rds$")
for (i in 1:length(files_NEC)) {
  file.remove(paste0(dir_save,files_NEC[i]))
}

files_WSC <- list.files(path=dir_save,pattern = "^(.*)_WSC.rds$")
for (i in 1:length(files_WSC)) {
  file.remove(paste0(dir_save,files_WSC[i]))
}

files_M <- list.files(path=dir_save,pattern = "^(.*)_M.rds$")
for (i in 1:length(files_M)) {
  file.remove(paste0(dir_save,files_M[i]))
}



################ 6. Delete zipcode files for 2017-2019 ###############
files_2017 <- list.files(path=dir_save,pattern = "2017(.*).rds$")
for (i in 1:length(files_2017)) {
  file.remove(paste0(dir_save,files_2017[i]))
}

files_2018 <- list.files(path=dir_save,pattern = "2018(.*).rds$")
for (i in 1:length(files_2018)) {
  file.remove(paste0(dir_save,files_2018[i]))
}

files_2019 <- list.files(path=dir_save,pattern = "2019(.*).rds$")
for (i in 1:length(files_2019)) {
  file.remove(paste0(dir_save,files_2019[i]))
}



################ 7. Check the final zipcode files ###############
files_final <- list.files(path=dir_final)

test <- readRDS(paste0(dir_final,files_final[12]))
length(unique(test$ZIP))/dim(test)[1]
summary(test)
