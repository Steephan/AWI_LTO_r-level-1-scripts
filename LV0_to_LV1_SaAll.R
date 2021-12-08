###............................................................................
##
##   Level0 to Level1 ----
##
##   filter and flags for each variable
##
##   written by:  stephan.lange@awi.de
## 
##
##   last modified: 2021-03-25
##
##   last check: 2020-01-29
##   checked by: christian.lehr@awi.de
##
###............................................................................
##
## open issues:
##
##
###............................................................................
##
##  last modifications:
## 2021-12-08 IG after SLs last change, disabling peak detection in the header was no longer possible
##               therefore I disabled it in the code for the following stations: "SaSnow2012", "SaPond2014", "SaCalm2002"
## 2021-10-27 SL read more date options of maintenance files
## 2021-10-14 SL inheritate SWE from distraw_2 and rho and SWE
## 2021-10-07 SL inheritate Dsn from distraw and Dsn
## 2021-08-18 SL rename noflag data to final data
## 2021-08-16 SL inheritate humidity sensors at SaMet2002
## 2021-05-06 SL adapted to refresh app
## 2021-03-25 SL new git path
## 2020-10-30 CL implement new way of choosing station, years and run.year
## 2020-10-29 CL replaced t.year with year_i
## 2020-10-06 CL new condition: flag 5 is only applied for years before 2019
## 2020-09-16 CL argument "time.res" removed from function detect.peaks
## 2020-08-31 CL table vwc_calc_columns_TSoil+E2.csv with parameters Ts, E2, phi and theta_tot_prior for the vwc calculation introduced
## 2020-07-29 CL add SaHole2018
## 2020-07-28 CL flagging of "RH50" and "RH200" with flag 6 in dataset "SaMet2002" for all air temperature values
#                 of the respective height with flag 4 ("TAir 50", respectively "TAir200") moved to db_filter_II.R
## 2020-07-02 CL variables and categories "NetRad" and "radnet" changed to "RadNet" for consistency with the naming convention of the wiki, the level 0 data and BaMet data
## 2020-07-02 CL inheritance of flags: "SwNet", "LwNet", "Albedo" inherit from "SwIn", "SwOut", "LwIn" and "LwOut"
##               ==> because SwNet, LwNet and Albedo are calculated from SwIn, SwOut, LwIn and LwOut
## 2019-05-21 PSc add SaSnow2012
## 2018-10-09 PSc changed flag funktions in db_filter_II.R for dirt/snow cover of radiation sensor 
## 2018-06-04 PSc add read out of manual flagged snow cover of sensors (Tair_50) 
##
###............................................................................
##
## Comments:
##
###............................................................................
# to run this script separately, you have to uncomment the next 10 lines!
# rm(list = ls())
# if (.Platform$OS.type == "windows") {
#   p.1 <- read.table("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/path_win.txt", sep = "\t", header = T)
#   p.1maint <- read.table("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/maintenance.files/maintance.txt", sep = "\t", header = T)
# 
#   source("N:/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/functions/db_func.R")
# } else {
#   p.1 <- read.table("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/path_linux.txt", sep = "\t", header = T, fileEncoding = "UTF-8")
#   p.1maint <- read.table("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/settings/maintenance.files/maintance.txt", sep = "\t", header = T)
# 
#   source("/sparc/LTO/R_database/Time_series_preprocessing/required-scripts-and-files/functions/db_func.R")
# }
###............................................................................
# to run this script separately, you have to uncomment the next 3 lines and choose station, years and run.year
# require(zoo)
# origin <- "1970-01-01"
# recent.year <- as.numeric(format(Sys.Date(),"%Y"))
# station <- 'SaCalm2002'
# run.year <- 2002:2021
###............................................................................

stations <- c('SaSoil1998', 'SaSoil2002', 'SaSoil2012', 'SaMet1998', 'SaMet2002','SaPrec2019',
              'SaSnow2012', 'SaSnow2016', 'SaHole2006', 'SaHole2010', 'SaHole2018',
              'SdHole2009', 'SaPond2006', 'SaPond2014',
              'SaCalm2002')

list.years <- list(1998:2002, 2002:recent.year, 2012:recent.year, 1998:2002, 2002:recent.year,2019:recent.year,
                   2012:recent.year, 2016:recent.year, 2006:recent.year, 2010:recent.year, 2018:recent.year,
                   2009:recent.year, 2006:2014, 2014:recent.year,
                   2002:recent.year)

years <- list.years[[which(stations == station)]]


###............................................................................
###............................................................................
##################### don't change anything below !!!!!!!!!!!!!!!!!!!!!!!!
###............................................................................
###............................................................................
###............................................................................


for (year_i in run.year) {
  # disable peak detection for recent years (all stations)
  if (year_i < 2019) {mit.peak.detection = 1}else{mit.peak.detection = 0}
  # disable peak detection for certain stations
  if (station == "SaSnow2012" || station == "SaPond2014" || station == "SaCalm2002") {mit.peak.detection = 0}
  
  #cat(year_i)
  file.name.main <- paste0(p.1$w[p.1$n == "LV0.p"], station, "/00_full_dataset/", station, "_", year_i, "_lv0.dat")
  file.name.before <- paste0(p.1$w[p.1$n == "LV0.p"], station, "/00_full_dataset/", station, "_", year_i - 1, "_lv0.dat")
  file.name.after <- paste0(p.1$w[p.1$n == "LV0.p"], station, "/00_full_dataset/", station, "_", year_i + 1, "_lv0.dat")
  
  # load manual filters
  db.filter <- read.table(paste0(paste0(p.1$w[p.1$n == "settings.p"]), "filter.files/Sa_filter_", year_i, ".dat"), sep = ",", dec = ".", header = T)
  db.filter <- db.filter[db.filter$dataset == station, ]
  db.filter[, 1] <- format(as.POSIXct(db.filter[, 1], origin = origin, tz = "UTC", format = '%Y-%m-%d %H:%M:%S'), format = '%Y-%m-%d %H:%M')
  db.filter[, 2] <- format(as.POSIXct(db.filter[, 2], origin = origin, tz = "UTC", format = '%Y-%m-%d %H:%M:%S'), format = '%Y-%m-%d %H:%M')
  
  # load maintanance filters
  db.maint <- read.table(paste0(paste0(p.1$w[p.1$n == "settings.p"]), "maintenance.files/Sa_maintenance_", year_i, ".dat"), sep = ",", dec = ".", header = T)
  db.maint <- db.maint[db.maint$dataset == station, ]
  if(is.na(format(as.POSIXct(db.maint[1, 1], origin = origin, tz = "UTC", format = '%Y-%m-%d %H:%M:%S'), format = '%Y-%m-%d %H:%M'))!=T){
    db.maint[, 1] <- format(as.POSIXct(db.maint[1, 1], origin = origin, tz = "UTC", format = '%Y-%m-%d %H:%M:%S'), format = '%Y-%m-%d %H:%M')
    db.maint[, 2] <- format(as.POSIXct(db.maint[, 2], origin = origin, tz = "UTC", format = '%Y-%m-%d %H:%M:%S'), format = '%Y-%m-%d %H:%M')
  }else if(is.na(format(as.POSIXct(db.maint[1, 1], origin = origin, tz = "UTC", format = '%Y-%m-%d %H:%M'), format = '%Y-%m-%d %H:%M'))!=T){
    db.maint[, 1] <- format(as.POSIXct(db.maint[1, 1], origin = origin, tz = "UTC", format = '%Y-%m-%d %H:%M'), format = '%Y-%m-%d %H:%M')
    db.maint[, 2] <- format(as.POSIXct(db.maint[, 2], origin = origin, tz = "UTC", format = '%Y-%m-%d %H:%M'), format = '%Y-%m-%d %H:%M')
  }else if(is.na(format(as.POSIXct(db.maint[1, 1], origin = origin, tz = "UTC", format = '%Y-%m-%d'), format = '%Y-%m-%d %H:%M'))!=T){
    db.maint[, 1] <- format(as.POSIXct(db.maint[1, 1], origin = origin, tz = "UTC", format = '%Y-%m-%d'), format = '%Y-%m-%d %H:%M')
    db.maint[, 2] <- format(as.POSIXct(db.maint[, 2], origin = origin, tz = "UTC", format = '%Y-%m-%d'), format = '%Y-%m-%d %H:%M')
  }

  # read level 0 data
  lv0.data <- read.table(file.name.main, sep = ",", dec = ".", header = T)
  # set time format
  lv0.data[, 1] <- format(as.POSIXct(lv0.data[, 1], origin = origin, tz = "UTC", format = '%Y-%m-%d %H:%M'), format = '%Y-%m-%d %H:%M')
  
  # compute time resolution in values measured per day
  time.res <- 1 / as.numeric(difftime(lv0.data[2, 1],  lv0.data[1, 1],  units = 'days'))
  
  # add the last week of the previous year and the first week of the next year if available
  # needed for moving average operations
  time.extra <- time.res * 7
  if (year_i > years[1]) {
    tmp <- read.table(file.name.before, sep = ",", dec = ".", header = T)
    tmp[, 1] <- format(as.POSIXct(tmp[, 1], origin = options()$origin, tz = 'UTC', format = '%Y-%m-%d %H:%M'), format = '%Y-%m-%d %H:%M')
    tmp <- tmp[(nrow(tmp) - time.extra + 1):nrow(tmp), ]
  } else {
    tmp <- lv0.data[(nrow(lv0.data) - time.extra + 1):nrow(lv0.data), ]
    tmp[, 2:ncol(lv0.data)] <- NA
    tmp[, 1] <- format((as.POSIXct(tmp[, 1]) -
                          difftime(strptime('1999-01-01', '%Y-%m-%d'), strptime('1998-01-01', '%Y-%m-%d'))), format = '%Y-%m-%d %H:%M')
  }
  lv0.data <- rbind(tmp, lv0.data)
  if (year_i < years[length(years)]) {
    tmp <- read.table(file.name.after, sep = ",", dec = ".", header = T)
    tmp[, 1] <- format(as.POSIXct(tmp[, 1], origin = options()$origin, tz = 'UTC', format = '%Y-%m-%d %H:%M'), format = '%Y-%m-%d %H:%M')
    tmp <- tmp[1:time.extra, ]
  } else {
    tmp <- lv0.data[(time.extra + 1):(time.extra * 2), ]
    tmp[, 2:ncol(tmp)] <- NA
    tmp[, 1] <- format((as.POSIXct(tmp[, 1]) +
                          difftime(strptime('1999-01-01', '%Y-%m-%d'), strptime('1998-01-01', '%Y-%m-%d'))), format = '%Y-%m-%d %H:%M')
  }
  lv0.data <- rbind(lv0.data, tmp)
  rownames(lv0.data) <- NULL
  rm(tmp)
  
  # make all columns numeric (for some reason BaSnow ends up having factor or character columns)
  tmp <- which(sapply(lv0.data,  is.factor))
  if (length(tmp) > 0) {
    lv0.data[, tmp] <- as.numeric(as.character(lv0.data[, tmp]))
  }
  tmp <- which(sapply(lv0.data,  is.character))
  if (length(tmp) > 1) {
    lv0.data[, tmp[2:length(tmp)]] <- as.numeric(lv0.data[, tmp[2:length(tmp)]])
  }
  rm(tmp)
  
  # remove columns which are not needed for level 1
  tmp <- c(which(grepl('RECORD', colnames(lv0.data)) |
                   grepl('Tpan', colnames(lv0.data)) |
                   grepl('Tsen', colnames(lv0.data)) |
                   grepl('Tair_sd', colnames(lv0.data)) |
                   #grepl('Tring', colnames(lv0.data)) |
                   grepl('Usen', colnames(lv0.data)) |
                   grepl('Ubat', colnames(lv0.data)) |
                   grepl('batt_U', colnames(lv0.data)) |
                   grepl('U_min', colnames(lv0.data)) |
                   grepl('Bat_V', colnames(lv0.data)) |
                   grepl('TableFlag', colnames(lv0.data)) |
                   grepl('QA', colnames(lv0.data)) |
                   grepl('distcor', colnames(lv0.data)) |
                   grepl('rawcor', colnames(lv0.data)) |
                   grepl('WT', colnames(lv0.data)) |
                   grepl('sq', colnames(lv0.data)) |
                   grepl('SQ', colnames(lv0.data)) |
                   grepl('Tsurf_cor', colnames(lv0.data)) |
                   grepl('prec_sum', colnames(lv0.data)) |
                   grepl('prec_bucket', colnames(lv0.data)) |
                   grepl('heat', colnames(lv0.data)) |
                   grepl('status', colnames(lv0.data))|
                   grepl('Counts', colnames(lv0.data))
  ))
  if (length(tmp) > 0) {
    lv0.data <- lv0.data[, -tmp]
  }
  rm(tmp)
  # lv0.data <- lv0.data[, -c(2:57)]
  # add one column with a flag to each data series
  lv1.data <- get.100flag.columns(lv0.data)
  
  # delete level 0 data
  rm(lv0.data)
  
  # Define data categories based on column names
  cats <- c('Tair', 'prec', 'RH', 'Dsn', 'SwIn', 'SwOut', 'LwIn', 'LwOut', 'RadNet', #'radnet', 'NetRad',
            'windv', 'winddeg', 'windsddeg',
            'Ts', 'Tw', 'vwc', 'cond', 'E2', 'E2sn', 'G', 'SwNet', 'LwNet', 'Albedo', 'distraw', 'WL',
            'Dal')
  
  # Retreive columns in the data for each category
  col.cat <- as.data.frame(setNames(replicate(length(cats), numeric(100), simplify = F), cats))
  Iflag <- which(grepl('_fl', colnames(lv1.data)))
  tmp <- colnames(lv1.data[Iflag - 1])
  # remove '_' from column names
  for (i in 1:length(tmp)) {
    tmp[i] <- gsub('_', '', tmp[i])
  }
  for (i in 1:length(cats)) {
    tmp2 <- which(grepl(cats[i], tmp)) * 2
    if (cats[i] == 'E2') {
      tmp3 <- which(grepl('E2sn', tmp)) * 2
      if (length(tmp3) > 0) {
        tmp2 <- setdiff(tmp2, tmp3)
      }
    }
    if (length(tmp2) > 0) {
      col.cat[1:length(tmp2), cats[i]] <- tmp2
    }
  }
  col.cat <- col.cat[rowSums(col.cat) > 0, ]
  rm(tmp)
  
  # get all column names
  cols <- colnames(lv1.data)
  
  ###............................................................................
  ##
  ## FILTER for LEVEL 1 ----
  ###............................................................................
  ##
  ## flag == 1    (no data) ----
  ##
  
  ##
  # This part can be removed, it is not used any more....
  # Snow depth sensor no data value changes some times between 1.4 and 1.5
  i <- which(cats == 'Dsn')
  
  ###............................................................................
  
  m <- (ncol(lv1.data) - 1) / 2
  for (i in 1:m) {
    lv1.data[which(lv1.data[, (i * 2)] <= (-99999)), (i * 2)] <- NA          # set NA if no data
    lv1.data[which(lv1.data[, (i * 2)] >= (100000)), (i * 2)] <- NA          # set NA if no data
    lv1.data[which(is.na(lv1.data[, (i * 2)]) == TRUE), (i * 2) + 1] <- 1      # set flag to 1 if no data
  }
  rm(i, m)
  
  
  ###............................................................................
  ##
  ## flag == 2     (System error) ----
  ##
  tmpflag <- lv1.data
  db.filter.2 <- db.filter[db.filter$flag == "2", ]
  if (nrow(db.filter.2) > 0) {
    for (elea in 1:nrow(db.filter.2)) {
      tmpflag[which(lv1.data$UTC >= db.filter.2$from[elea] &
                      lv1.data$UTC <= db.filter.2$to[elea]),
              which(colnames(lv1.data) == db.filter.2$variable[elea]) + 1] <- 2
    }
    lv1.data <- update.flags(lv1.data, tmpflag, Iflag, 2)
  }
  
  # Additional errors that can only be identified based on other variables:
  tmpflag <- add.systemerror(tmpflag, station)
  lv1.data <- update.flags(lv1.data, tmpflag, Iflag, 2)
  
  ###............................................................................
  ##
  ## flag == 3    (Maintenance) ----
  ##
  tmpflag <- lv1.data
  if (nrow(db.maint) > 0) {
    for (elea in 1:nrow(db.maint)) {
      tmpflag[which(lv1.data$UTC >= db.maint$from[elea] &
                      lv1.data$UTC <= db.maint$to[elea]),
              which(colnames(lv1.data) == db.maint$variable[elea]) + 1] <- 3
    }
    lv1.data <- update.flags(lv1.data, tmpflag, Iflag, 3)
  }
  
  ###............................................................................
  ##
  ## flag == 4     (Physical limits) ----
  ##
  tmpflag <- physical.limits.sa(lv1.data, col.cat)
  lv1.data <- update.flags(lv1.data, tmpflag, Iflag, 4)
  
  ###............................................................................
  ##
  ## flag == 5    (Gradient) ----
  ##
  
  # if (run.year < 2019) {
  if (mit.peak.detection == 1) {
    tmpflag <- detect.peaks(lv1.data, col.cat, station) # time.res,
    lv1.data <- update.flags(lv1.data, tmpflag, Iflag, 5)
  }
  # }
  
  ###............................................................................
  ##
  ## flag == 6    (Plausibility) ----
  ##
  tmpflag <- lv1.data
  db.filter.6 <- db.filter[db.filter$flag == "6", ]
  if (nrow(db.filter.6) > 0) {
    for (elea in 1:nrow(db.filter.6)) {
      tmpflag[which(lv1.data$UTC >= db.filter.6$from[elea] &     # Minimum Time
                      lv1.data$UTC <= db.filter.6$to[elea] &     # Maximum Time
                      lv1.data[, (which(colnames(lv1.data) == db.filter.6$variable[elea]))] >= db.filter.6$min[elea] &     # Minimum Value
                      lv1.data[, (which(colnames(lv1.data) == db.filter.6$variable[elea]))] <= db.filter.6$max[elea]),     # Maximum Value
              which(colnames(lv1.data) == db.filter.6$variable[elea]) + 1] <- 6
    }
    lv1.data <- update.flags(lv1.data, tmpflag, Iflag, 6)
  }
  
  tmpflag <- add.plausibility(lv1.data, station)
  lv1.data <- update.flags(lv1.data, tmpflag, Iflag, 6)
  
  ###............................................................................
  ##
  ## flag == 7    (Decreased accuracy) ----
  ##
  tmpflag <- lv1.data
  db.filter.7 <- db.filter[db.filter$flag == "7", ]
  if (nrow(db.filter.7) > 0) {
    for (elea in 1:nrow(db.filter.7)) {
      tmpflag[which(lv1.data$UTC >= db.filter.7$from[elea] &     # Minimum Time
                      lv1.data$UTC <= db.filter.7$to[elea] &     # Maximum Time
                      lv1.data[, (which(colnames(lv1.data) == db.filter.7$variable[elea]))] >= db.filter.7$min[elea] &     # Minimum Value
                      lv1.data[, (which(colnames(lv1.data) == db.filter.7$variable[elea]))] <= db.filter.7$max[elea]),     # Maximum Value
              which(colnames(lv1.data) == db.filter.7$variable[elea]) + 1] <- 7
    }
    lv1.data <- update.flags(lv1.data, tmpflag, Iflag, 7)
  }
  
  tmpflag <- detect.T.degradation(lv1.data, col.cats, station)
  lv1.data <- update.flags(lv1.data, tmpflag, Iflag, 7)
  
  ###............................................................................
  ##
  ## flag == 8    (Snow covered) ----
  ##
  # tmpflag <- frozen.water.table(lv1.data, col.cats, time.res, year_i)
  # lv1.data <- update.flags(lv1.data, tmpflag, Iflag, 8)
  # rm(tmpflag)
  
  tmpflag <- detect.snow.cover(lv1.data, col.cats, time.res)
  lv1.data <- update.flags(lv1.data, tmpflag, Iflag, 8)
  rm(tmpflag)
  
  
  tmpflag <- lv1.data
  db.filter.8 <- db.filter[db.filter$flag == "8", ]
  if (nrow(db.filter.8) > 0) {
    for (elea in 1:nrow(db.filter.8)) {
      tmpflag[which(lv1.data$UTC >= db.filter.8$from[elea] &     # Minimum Time
                      lv1.data$UTC <= db.filter.8$to[elea] &     # Maximum Time
                      lv1.data[, (which(colnames(lv1.data) == db.filter.8$variable[elea]))] >= db.filter.8$min[elea] &     # Minimum Value
                      lv1.data[, (which(colnames(lv1.data) == db.filter.8$variable[elea]))] <= db.filter.8$max[elea]),     # Maximum Value
              which(colnames(lv1.data) == db.filter.8$variable[elea]) + 1] <- 8
    }
    lv1.data <- update.flags(lv1.data, tmpflag, Iflag, 8)
  }
  
  tmpflag <- add.plausibility(lv1.data, station)
  lv1.data <- update.flags(lv1.data, tmpflag, Iflag, 8)
  
  ###............................................................................
  ##
  ## Soil moisture computation following Roth ----
  ##
  
  if (station %in% c("SaSoil2002", "SaSoil2012")) {
    # path for threshold table for calculation of volumetric water content (vwc)
    path.input <- paste(p.1$w[p.1$n == "LV2.p"], "Samoylov/VWC/threshold_", station, "_", sep = "")
    # tab.vwc_calc: table with column names of soil temperature (Ts), dielectricity (E2), porosity (phi) and vwc of the first year (theta_tot_prior) for the the calculation of vwc
    tab.vwc_calc <- read.table(file = paste(p.1$w[p.1$n == "settings.p"], "vwc_calc_columns_TSoil+E2.csv", sep = ""), header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)
    # mw.width: width of moving window to calculate vwc
    mw.width <- time.res
    lv1.data <- compute.vwc(lv1.data, col.cat, station, years, mw.width, path.input, tab.vwc_calc)
  }
  
  ###............................................................................
  ##
  ## Inheritance of flags ----
  ## PART A
  
  ###............................................................................
  # For the calculated variables, keep the flags of the variables used in the calculation.
  # Because the default flag is 100, the function min can be used for this purpose.
  # The "good data" flag 100 is changed to 0 in the next section.
  ###............................................................................
  
  # a) SwNet, LwNet and Albedo inherit the flags of SwIn, SwOut, LwIn and LwOut
  if (station == "SaMet2002") {
    # Radiation
    lv1.data$SwNet_fl  <- apply(lv1.data[, c("SwNet_fl", "SwIn_fl", "SwOut_fl")], 1, FUN = min)
    lv1.data$LwNet_fl  <- apply(lv1.data[, c("LwNet_fl", "LwIn_fl", "LwOut_fl")], 1, FUN = min)
    lv1.data$Albedo_fl <- apply(lv1.data[, c("Albedo_fl", "SwIn_fl", "SwOut_fl")], 1, FUN = min)
    lv1.data$RadNet_fl <- apply(lv1.data[, c("SwIn_fl", "SwOut_fl", "LwIn_fl", "LwOut_fl")], 1, FUN = max)
    # Humidity
    lv1.data$RH_200_fl <- apply(lv1.data[, c("RH_200_fl","Tair_a_200_fl")], 1, FUN = min)
    lv1.data$RH_50_fl  <- apply(lv1.data[, c("RH_50_fl", "Tair_a_50_fl")], 1, FUN = min)
  }
  
  if (station == "SaSnow2012") {
    # Snow height from raw distance
    lv1.data$Dsn_0_fl  <- apply(lv1.data[, c("Dsn_0_fl", "distraw_0_fl")], 1, FUN = min)
    lv1.data$Dsn_1_fl  <- apply(lv1.data[, c("Dsn_1_fl", "distraw_1_fl")], 1, FUN = min)
    lv1.data$Dsn_2_fl  <- apply(lv1.data[, c("Dsn_2_fl", "distraw_2_fl")], 1, FUN = min)
    lv1.data$Dsn_3_fl  <- apply(lv1.data[, c("Dsn_3_fl", "distraw_3_fl")], 1, FUN = min)
    lv1.data$Dsn_4_fl  <- apply(lv1.data[, c("Dsn_4_fl", "distraw_4_fl")], 1, FUN = min)
    lv1.data$Dsn_5_fl  <- apply(lv1.data[, c("Dsn_5_fl", "distraw_5_fl")], 1, FUN = min)
    lv1.data$Dsn_6_fl  <- apply(lv1.data[, c("Dsn_6_fl", "distraw_6_fl")], 1, FUN = min)
    lv1.data$Dsn_7_fl  <- apply(lv1.data[, c("Dsn_7_fl", "distraw_7_fl")], 1, FUN = min)
    lv1.data$Dsn_8_fl  <- apply(lv1.data[, c("Dsn_8_fl", "distraw_8_fl")], 1, FUN = min)
    lv1.data$Dsn_9_fl  <- apply(lv1.data[, c("Dsn_9_fl", "distraw_9_fl")], 1, FUN = min)
    
    # SWE from sensor 2
    lv1.data$SWE_2_fl  <- apply(lv1.data[, c("SWE_2_fl", "rho_2_fl", "distraw_2_fl")], 1, FUN = min)
    lv1.data$SWE_3_fl  <- apply(lv1.data[, c("SWE_3_fl", "rho_3_fl", "distraw_2_fl")], 1, FUN = min)
    lv1.data$SWE_4_fl  <- apply(lv1.data[, c("SWE_4_fl", "rho_4_fl", "distraw_2_fl")], 1, FUN = min)
    
    
  }
  if (station == "SaSnow2016") {
    # Snow height from raw distance
    lv1.data$Dsn_fl  <- apply(lv1.data[, c("Dsn_fl", "distraw_fl")], 1, FUN = min)
    
  }
  if (station == "SaSoil2002") {
    # Snow height from raw distance
    lv1.data$Dsn_fl  <- apply(lv1.data[, c("Dsn_fl", "distraw_fl")], 1, FUN = min)
    
  }
  
  ###............................................................................
  ##
  ## Save the data with flags and as noflag version ----
  ##
  
  # remove extra periods from previous and next years
  lv1.data <- lv1.data[(time.extra + 1):(nrow(lv1.data) - time.extra), ]
  
  # set 'good data flag' to 0 instead of 100
  # because the function get.100flag.columns() from db_filter_I sets the default flag 100
  tmp <- lv1.data[, Iflag]
  tmp[tmp > 10] <- 0
  lv1.data[, Iflag] <- tmp
  rm(tmp)
  
  # remove vwc for sensors above the ground
  tmp <- which(grepl('vwc_sn', colnames(lv1.data)))
  if (length(tmp) > 0) {
    lv1.data <- lv1.data[, -tmp]
  }
  rm(tmp)
  
  Iflag <- which(grepl('_fl', colnames(lv1.data)))
  
  
  # create noflag dataset where all flagged data is set to 'NA'
  lv1.data.noflag <- lv1.data
  for (i in Iflag) {# set data to NA if flag is not 0
    lv1.data.noflag[which(lv1.data.noflag[, i] >= 1), (i - 1)] <- NA
  }
  
  lv1.data[, 1] <- format( as.POSIXct(lv1.data[, 1], origin = origin, tz = 'UTC', format = '%Y-%m-%d %H:%M'), format = '%Y-%m-%d %H:%M')
  lv1.data.noflag[, 1] <- format( as.POSIXct(lv1.data.noflag[, 1], origin = origin, tz = 'UTC', format = '%Y-%m-%d %H:%M'), format = '%Y-%m-%d %H:%M')
  
  # make flag columns character columns to avoid printing many digits
  lv1.data[, Iflag] <- sapply(lv1.data[,  Iflag],  as.character)
  
  # Write files ----
  write.table(x = lv1.data,
              file = paste0(p.1$w[p.1$n == "LV1.p"], station, "/00_full_dataset/", station, "_", year_i, "_lv1.dat"),
              quote = F, dec = ".", sep = ",", row.names = F)
  write.table(x = lv1.data.noflag[, c(1, seq( 2, (ncol(lv1.data) - 1), by = 2))],
              file = paste0(p.1$w[p.1$n == "LV1.p"], station, "/00_full_dataset/", station, "_", year_i, "_lv1_final.dat"),
              quote = F, dec = ".", sep = ",", row.names = F)
  
  ###............................................................................
  log.peaks(station, year_i, mit.peak.detection)
  cat("#\n# level1 ", station, ": ", year_i, "without problems!\n#\n")
}





