#############################################################################
##
##   Level0 to Level1
##
##   filter and flags for each variable
##
##   by: stephan.lange@awi.de -2201; peter.schreiber@awi.de
##   last modified: 2018-00-09
##
#############################################################################
##
## last modification:
##  2018-06-04: add read out of manual flagged snow cover of sensors (Tair_50) [PSc]
##  2018-10-09: changed flag funktions in db_filter_II.R for dirt/snow cover of radiation sensor [PSc]
## 
##
##
#############################################################################
##  to run this script seperat, you have to uncomment the next 10 lines!
if (.Platform$OS.type == "windows") {
  path<-read.table("N:/sparc/LTO/R_database/database_R/settings/path_windoof.txt",sep="\t",header=T)
  maint<-read.table("N:/sparc/LTO/R_database/database_R/settings/maintance.txt",sep="\t",header=T)
  p.1<-read.table("N:/sparc/LTO/R_database/database_R/settings/path_windoof.txt",sep="\t",header=T)
  p.1maint<-read.table("N:/sparc/LTO/R_database/database_R/settings/maintance.txt",sep="\t",header=T)

  source("N:/sparc/LTO/R_database/database_R/settings/db_func.R")
}else{
  path<-read.table("/sparc/LTO/R_database/database_R/settings/path_linux.txt",sep="\t",header=T, fileEncoding="UTF-8")
  maint<-read.table("/sparc/LTO/R_database/database_R/settings/maintance.txt",sep="\t",header=T)
  p.1<-read.table("/sparc/LTO/R_database/database_R/settings/path_linux.txt",sep="\t",header=T, fileEncoding="UTF-8")
  p.1maint<-read.table("/sparc/LTO/R_database/database_R/settings/maintance.txt",sep="\t",header=T)

  source("/sparc/LTO/R_database/database_R/settings/db_func.R")
}
############################################################################# 
require(zoo)
origin="1970-01-01"

aktuell<-as.numeric(format(Sys.Date(),"%Y"))

# station <- 'KuQ12013'
# years <- 2013:2018 # years of existing data!!

# station <- 'KuLucky22014'
# years <- 2014:2017

# station <- 'KuLucky12013'
# years <- 2013:2014

 station <- 'KuLucky2013'
 years <- 2013

################################################
#
# # years for processinig data!!
#
run.year <- years #years
#run.year <- 2018

################################################
#
# choose faster option without peakdetection
# 1 for yes, 0 for no
#
mit.peak.detection <- 1




#####################
#####################
##################### don't change anything below
#####################
#####################
#####################


for(t.year in run.year){
  
  
  #cat(t.year)
  file.name.main <- paste0(p.1$w[p.1$n=="LV0.p"],station,"/00_full_dataset/",station,"_",t.year,"_lv0.dat")
  file.name.before <- paste0(p.1$w[p.1$n=="LV0.p"],station,"/00_full_dataset/",station,"_",t.year-1,"_lv0.dat")
  file.name.after <- paste0(p.1$w[p.1$n=="LV0.p"],station,"/00_full_dataset/",station,"_",t.year+1,"_lv0.dat")
  
  # load manual filters
  db.filter<-read.table(paste0(paste0(p.1$w[p.1$n=="LV1.p"]),"Filter/Sa_filter_",t.year,".dat"),sep=",",dec=".",header=T)
  db.filter<-db.filter[db.filter$dataset==station,]
  db.filter[,1]<-format(as.POSIXct(db.filter[,1],origin=origin,tz="UTC",format='%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d %H:%M')
  db.filter[,2]<-format(as.POSIXct(db.filter[,2],origin=origin,tz="UTC",format='%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d %H:%M')
  
  # load maintanance filters
  db.maint<-read.table(paste0(paste0(p.1$w[p.1$n=="LV1.p"]),"Filter/Sa_maintenance_",t.year,".dat"),sep=",",dec=".",header=T)
  db.maint<-db.maint[db.maint$dataset==station,]
  db.maint[,1]<-format(as.POSIXct(db.maint[,1],origin=origin,tz="UTC",format='%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d %H:%M')
  db.maint[,2]<-format(as.POSIXct(db.maint[,2],origin=origin,tz="UTC",format='%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d %H:%M')
  
  # read level 0 data
  lv0.data<-read.table(file.name.main,sep=",",dec=".",header=T)
  # set time format
  lv0.data[,1] <- format(as.POSIXct(lv0.data[,1],origin=origin,tz="UTC",format='%Y-%m-%d %H:%M'),format='%Y-%m-%d %H:%M')
  
  # compute time resolution in values measured per day
  time.res <- 1/as.numeric(difftime(lv0.data[2,1], lv0.data[1,1], units = 'days'))
  
  # add the last week of the previous year and the first week of the next year if available
  # needed for moving average operations
  time.extra <- time.res * 7
  if(t.year > years[1]){
    tmp <- read.table(file.name.before,sep=",",dec=".",header=T)
    tmp[,1]<-format(as.POSIXct(tmp[,1],origin=options()$origin,tz='UTC',format='%Y-%m-%d %H:%M'),format='%Y-%m-%d %H:%M')
    tmp <- tmp[(nrow(tmp)-time.extra+1):nrow(tmp),]
  }else{
    tmp <- lv0.data[(nrow(lv0.data)-time.extra+1):nrow(lv0.data),]
    tmp[,2:ncol(lv0.data)] <- NA
    tmp[,1] <- format((as.POSIXct(tmp[,1]) - difftime(strptime('1999-01-01','%Y-%m-%d'),strptime('1998-01-01','%Y-%m-%d'))),format='%Y-%m-%d %H:%M')
  }
  lv0.data <- rbind(tmp,lv0.data)
  if(t.year < years[length(years)]){
    tmp <- read.table(file.name.after,sep=",",dec=".",header=T)
    tmp[,1]<-format(as.POSIXct(tmp[,1],origin=options()$origin,tz='UTC',format='%Y-%m-%d %H:%M'),format='%Y-%m-%d %H:%M')
    tmp <- tmp[1:time.extra,]
  }else{
    tmp <- lv0.data[(time.extra+1):(time.extra*2),]
    tmp[,2:ncol(tmp)] <- NA
    tmp[,1] <- format((as.POSIXct(tmp[,1]) + difftime(strptime('1999-01-01','%Y-%m-%d'),strptime('1998-01-01','%Y-%m-%d'))),format='%Y-%m-%d %H:%M')
  }
  lv0.data <- rbind(lv0.data,tmp)
  rownames(lv0.data) <- NULL
  rm(tmp)
  
  # make all columns numeric (for some reason BaSnow ends up having factor or character columns)
  tmp <- which(sapply(lv0.data, is.factor))
  if(length(tmp) > 0){
    lv0.data[,tmp] <- as.numeric(as.character(lv0.data[,tmp]))
  }
  tmp <- which(sapply(lv0.data, is.character))
  if(length(tmp) > 1){
    lv0.data[,tmp[2:length(tmp)]] <- as.numeric(lv0.data[,tmp[2:length(tmp)]])
  }
  rm(tmp)
  
  # remove columns which are not needed for level 1
  tmp <- c(which(grepl('Tpan', colnames(lv0.data)) | 
                   grepl('Tsen', colnames(lv0.data)) | 
                   grepl('Usen', colnames(lv0.data)) | 
                   grepl('Ubat', colnames(lv0.data)) | 
                   grepl('batt_U', colnames(lv0.data)) | 
                   grepl('Bat_V', colnames(lv0.data)) | 
                   grepl('BattV', colnames(lv0.data)) |
                   grepl('TableFlag', colnames(lv0.data)) |
                   grepl('dist', colnames(lv0.data)) | 
                   grepl('Dist', colnames(lv0.data)) |
                   grepl('raw', colnames(lv0.data)) |
                   grepl('sq', colnames(lv0.data)) |
                   grepl('RECORD', colnames(lv0.data)) |
                   grepl('Tsurf_cor', colnames(lv0.data))
                 ))
  if(length(tmp) > 0){
    lv0.data <- lv0.data[,-tmp]
  }
  rm(tmp)
  # lv0.data <- lv0.data[,-c(2:57)]
  # add one column with a flag to each data series
  lv1.data <- get.100flag.columns(lv0.data)
  
  # delete level 0 data
  rm(lv0.data)
  
  # Define data categories based on column names
  cats <- c('Tair','prec','RH','Dsn','radnet','NetRad','SwIn','SwOut','LwIn','LwOut','windv','winddeg','windsddeg',
            'Ts','Tw','vwc','cond','E2','E2sn','G','SwNet','LwNet','Albedo','distcor','WT','WTch','WL','Q')

  # Retreive columns in the data for each category
  col.cat <- as.data.frame(setNames(replicate(length(cats),numeric(100), simplify = F), cats))
  Iflag <- which(grepl('_fl', colnames(lv1.data)))
  tmp = colnames(lv1.data[Iflag-1])
  # remove '_' from column names
  for(i in 1:length(tmp)){
    tmp[i] <- gsub('_', '', tmp[i])
  }
  for(i in 1:length(cats)){
    tmp2 <- which(grepl(cats[i], tmp))*2
    if(cats[i] == 'E2'){
      tmp3 <- which(grepl('E2sn', tmp))*2
      if(length(tmp3)>0){
        tmp2 <- setdiff(tmp2,tmp3)
      }
    }
    if(length(tmp2)>0){
      col.cat[1:length(tmp2),cats[i]] <- tmp2
    }
  } 
  col.cat <- col.cat[rowSums(col.cat)>0,]
  rm(tmp)
  
  # get all column names
  cols <- colnames(lv1.data)
  
  ## ==============================================================================
  ##
  ## FILTER for LEVEL 1
  ## ==============================================================================
  ##
  ## flag == 1    (no data)
  ##
  # Snow depth sensor no data value changes some times between 1.4 and 1.5
  i <- which(cats=='Dsn')
  m <- (ncol(lv1.data)-1)/2
  for(i in 1:m){
    lv1.data[which(lv1.data[,(i*2)]<=(-99999)),(i*2)] <- NA          # set NA if no data
    lv1.data[which(lv1.data[,(i*2)]>=(100000)),(i*2)] <- NA          # set NA if no data
    lv1.data[which(is.na(lv1.data[,(i*2)])==TRUE),(i*2)+1] <- 1      # set flag to 1 if no data
  }
  rm(i,m)
  
  
  ## ==============================================================================
  ##
  ## flag == 2     (System error)
  ##
  tmpflag <- lv1.data
  db.filter.2 <- db.filter[db.filter$flag=="2",]
  if(nrow(db.filter.2) > 0){
    for(elea in 1:nrow(db.filter.2)){
      tmpflag[which(lv1.data$UTC>=db.filter.2$from[elea]&
                      lv1.data$UTC<=db.filter.2$to[elea]),
              which(colnames(lv1.data)==db.filter.2$variable[elea])+1] <- 2
    }
    lv1.data <- update.flags(lv1.data,tmpflag,Iflag,2)
  }
  
  # Additional errors that can only be identified based on other variables:
  tmpflag <- add.systemerror(tmpflag,station)
  lv1.data <- update.flags(lv1.data,tmpflag,Iflag,2)
  
  ## ==============================================================================
  ##
  ## flag == 3    (Maintenance)
  ##
  tmpflag <- lv1.data
  if(nrow(db.maint) > 0){
    for(elea in 1:nrow(db.maint)){
      tmpflag[which(lv1.data$UTC>=db.maint$from[elea]&
                      lv1.data$UTC<=db.maint$to[elea]),
              which(colnames(lv1.data)==db.maint$variable[elea])+1] <- 3
      
    }
  lv1.data <- update.flags(lv1.data,tmpflag,Iflag,3)
  }
  
  ## ==============================================================================
  ##
  ## flag == 4     (Physical limits)
  ##
  tmpflag <- physical.limits.ku(lv1.data,col.cat)
  lv1.data <- update.flags(lv1.data,tmpflag,Iflag,4)
  # if(station %in% c("SaMet2002")){#,"SaSoil2012"
  #   lv1.data$RH_200_fl[which(lv1.data$Tair_a_200_fl==4)] <- 6
  #   lv1.data$RH_50_fl[which(lv1.data$Tair_a_50_fl==4)] <- 6
  # }
  
  ## ==============================================================================
  ##
  ## flag == 5    (Gradient)
  ##
  if(mit.peak.detection==1){
  tmpflag <- detect.peaks(lv1.data,col.cat,time.res,station)
  lv1.data <- update.flags(lv1.data,tmpflag,Iflag,5)
  }
  
  ## ==============================================================================
  ##
  ## flag == 6    (Plausibility)
  ##
  tmpflag <- lv1.data
  db.filter.6 <- db.filter[db.filter$flag=="6",]
  if(nrow(db.filter.6) > 0){
    for(elea in 1:nrow(db.filter.6)){
      tmpflag[which(lv1.data$UTC>=db.filter.6$from[elea]&     # Minimum Time
                      lv1.data$UTC<=db.filter.6$to[elea]&     # Maximum Time
                      lv1.data[,(which(colnames(lv1.data)==db.filter.6$variable[elea]))] >= db.filter.6$min[elea]&     # Minimum Value
                      lv1.data[,(which(colnames(lv1.data)==db.filter.6$variable[elea]))] <= db.filter.6$max[elea]),    # Maximum Value
              which(colnames(lv1.data)==db.filter.6$variable[elea])+1] <- 6
    }
  lv1.data <- update.flags(lv1.data,tmpflag,Iflag,6)
  }
  
  tmpflag <- add.plausibility(lv1.data,station)
  lv1.data <- update.flags(lv1.data,tmpflag,Iflag,6)
  
  ## ==============================================================================
  ##
  ## flag == 7    (Decreased accuracy)
  ##
  tmpflag <- lv1.data
  db.filter.7 <- db.filter[db.filter$flag=="7",]
  if(nrow(db.filter.7) > 0){
    for(elea in 1:nrow(db.filter.7)){
      tmpflag[which(lv1.data$UTC>=db.filter.7$from[elea]&     # Minimum Time
                      lv1.data$UTC<=db.filter.7$to[elea]&     # Maximum Time
                      lv1.data[,(which(colnames(lv1.data)==db.filter.7$variable[elea]))] >= db.filter.7$min[elea]&     # Minimum Value
                      lv1.data[,(which(colnames(lv1.data)==db.filter.7$variable[elea]))] <= db.filter.7$max[elea]),    # Maximum Value
              which(colnames(lv1.data)==db.filter.7$variable[elea])+1] <- 7
    }
  lv1.data <- update.flags(lv1.data,tmpflag,Iflag,7)
  }
  
  tmpflag <- detect.T.degradation(lv1.data,col.cats,station)
  lv1.data <- update.flags(lv1.data,tmpflag,Iflag,7)
  
  ## ==============================================================================
  ##
  ## flag == 8    (Snow covered)
  ##
  # tmpflag <- frozen.water.table(lv1.data,col.cats,time.res,t.year)
  # lv1.data <- update.flags(lv1.data,tmpflag,Iflag,8)
  #rm(tmpflag)
  
  tmpflag <- detect.snow.cover(lv1.data,col.cats,time.res)
  lv1.data <- update.flags(lv1.data,tmpflag,Iflag,8)
  rm(tmpflag)
  
  
  tmpflag <- lv1.data
  db.filter.8 <- db.filter[db.filter$flag=="8",]
  if(nrow(db.filter.8) > 0){
    for(elea in 1:nrow(db.filter.8)){
      tmpflag[which(lv1.data$UTC>=db.filter.8$from[elea]&     # Minimum Time
                      lv1.data$UTC<=db.filter.8$to[elea]&     # Maximum Time
                      lv1.data[,(which(colnames(lv1.data)==db.filter.8$variable[elea]))] >= db.filter.8$min[elea]&     # Minimum Value
                      lv1.data[,(which(colnames(lv1.data)==db.filter.8$variable[elea]))] <= db.filter.8$max[elea]),    # Maximum Value
              which(colnames(lv1.data)==db.filter.8$variable[elea])+1] <- 8
    }
    lv1.data <- update.flags(lv1.data,tmpflag,Iflag,8)
  }
  
  tmpflag <- add.plausibility(lv1.data,station)
  lv1.data <- update.flags(lv1.data,tmpflag,Iflag,8)
  
 
  ## ==============================================================================
  ##
  ## Save the data with flags and as noflag version
  ##
  
  # remove extra periods from previous and next years
  lv1.data <- lv1.data[(time.extra+1):(nrow(lv1.data)-time.extra),]
  
  # set 'no flag' to 0 instead of 100
  tmp <- lv1.data[,Iflag]
  tmp[tmp>10] <- 0
  lv1.data[,Iflag] <- tmp
  rm(tmp)
  
  # remove vwc for sensors above the ground
  tmp <- which(grepl('vwc_sn', colnames(lv1.data)))
  if(length(tmp) > 0){
    lv1.data <- lv1.data[,-tmp]
  }
  rm(tmp)
  
  Iflag <- which(grepl('_fl', colnames(lv1.data)))
  
  # create noflag dataset where all flagged data is set to 'NA'
  lv1.data.noflag<-lv1.data
  for(i in Iflag){# set data to NA if flag is not 0
    lv1.data.noflag[which(lv1.data.noflag[,i] >= 1),(i-1)] <- NA} 
  
  lv1.data[,1]       <-format( as.POSIXct(lv1.data[,1],       origin=origin,tz='UTC',format='%Y-%m-%d %H:%M'),format='%Y-%m-%d %H:%M')
  lv1.data.noflag[,1]<-format( as.POSIXct(lv1.data.noflag[,1],origin=origin,tz='UTC',format='%Y-%m-%d %H:%M'),format='%Y-%m-%d %H:%M')
  
  # make flag columns character columns to avoid printing many digits
  lv1.data[, Iflag] <- sapply(lv1.data[, Iflag], as.character)
  
  # write files
  write.table(lv1.data                              ,paste0(p.1$w[p.1$n=="LV1.p"],station,"/00_full_dataset/",station,"_",t.year,"_lv1.dat"),quote = F,dec=".",sep=",",row.names=F)
  write.table(lv1.data.noflag[,c(1,seq( 2,(ncol(lv1.data)-1),by=2))],paste0(p.1$w[p.1$n=="LV1.p"],station,"/00_full_dataset/",station,"_",t.year,"_lv1_noflag.dat"),quote = F,dec=".",sep=",",row.names=F)
  
  ###############################################################################################################################
  log.peaks(station,t.year,mit.peak.detection)
  cat("#\n# level1 ",station,": ",t.year,"without problems!\n#\n")
}





