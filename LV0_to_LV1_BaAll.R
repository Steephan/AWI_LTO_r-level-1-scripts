#############################################################################
##
##   Level0 to Level1
##
##   filter and flags for each variable
##
##   by:  stephan.lange@awi.de
##
##   last check: 2020-02-03
##   checked by: christian.lehr@awi.de
##
###############################################################################
##
## open issues:
##
##
#############################################################################
##
## last modification:
## 2020-10-29 CL replaced t.year with year_i
## 2020-10-06 CL new condition: flag 5 is only applied for years before 2019
## 2020-09-28 CL update index of flagging columns before reformatting "no flag" from 100 to 0
## 2020-09-28 CL removal of vwc sensors above the ground "vwc_sn_xxx" moved from the end of the script to the beginning of the script prior to where col.cat and cols are defined
## 2020-09-09 CL Dsn-correction at flag 1 updated ==> exact values of the Dsn-correction are set to NA
## 2020-08-31 CL table vwc_calc_columns_TSoil+E2.csv with parameters Ts, E2, phi and theta_tot_prior for the vwc calculation introduced
## 2020-08-20 CL variable time.res removed in function detect.peaks because it is not used
## 2020-08-20 CL replacing roule of default flag 100 with flag 0 at the end of the script change from tmp[tmp > 10] <- 0 to tmp[tmp == 100] <- 0
## 2020-08-18 CL phys.limit_ba of "rho_K" (snow density calculated from "SWE_K" and "Dsn") updated
## 2020-08-18 CL "TL" and "KTL" changed to "Tl" and "KTl" (Tl: Thallium)
## 2020-08-18 CL inheritance of flags "KTl_Ratio" corrected
## 2020-08-18 CL modification of the adding of a) the last week of the previous year and b) the first week of the next year
##               for the moving average operations
##               (==> ndays.t.year: number of days of the processed year introduced, because of leap years)
## 2020-08-18 CL correction of the inheritance of "Dsn" and "Dsn_fl" from "BaSnow2019sr" to "BaSnow2019cs"
##               such that the data is assigned to the correct time stamp
## 2020-07-02 CL inheritance of flags: "KTL_Ratio" inherits from "CountsK" and "CountsTl" (dataset "BaSnow2019cs")
## 2020-05-13 CL inheritance of flags: "SwNet", "LwNet", "Albedo" inherit from "SwIn", "SwOut", "LwIn" and "LwOut"
##               ==> because SwNet, LwNet and Albedo are calculated from SwIn, SwOut, LwIn and LwOut
## 2020-04-30 CL add calculation of flagging of RadNet, analogue to the netto radiation for the Pangaea data set
## - add peak detection option
## - add peak.log function
##
###############################################################################
##
## Comments:
##
##
#############################################################################
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
# ############################################################################
# to run this script separately, you have to uncomment the next 3 lines and choose station, years and run.year
# require(zoo)
# origin <- "1970-01-01"
# recent.year <- as.numeric(format(Sys.Date(),"%Y"))
# station <- 'BaSoil2009'
# run.year <- 2009:2020
##############################################################################

stations <- c('BaSoil1998', 'BaSoil2009', 'BaSoil2017', 'BaMet1998', 'BaMet2009',
              'BaSnow2013', 'BaSnow2019sr','BaSnow2019cs', 'BaHole2009', 'BaHole2015', 'BaEddy2007')
# please update BaSnow2019sr before BaSnow2019cs because BaSnow2019cs
# inherits "Dsn" from BaSnow2019sr (see RAW_to_LV0_BaSnow2019.R)
# consequently, the Dsn_flag is likewise inherited
list.years <- list(1998:2012, 2009:recent.year, 2017:recent.year, 1998:2012, 2009:recent.year,
              2013:recent.year, 2019:recent.year, 2019:recent.year, 2009:recent.year, 2015:recent.year, 2007:2017)

years <- list.years[[which(stations == station)]]

################################################
#
# choose faster option without peakdetection
# 1 for yes, 0 for no
#
mit.peak.detection <- 1
################################################

# year_i <- run.year
for (year_i in run.year) {

  # cat(year_i)
  file.name.main <- paste0(p.1$w[p.1$n == "LV0.p"], station, "/00_full_dataset/", station,"_", year_i, "_lv0.dat")
  file.name.before <- paste0(p.1$w[p.1$n == "LV0.p"], station, "/00_full_dataset/", station,"_", year_i - 1, "_lv0.dat")
  file.name.after <- paste0(p.1$w[p.1$n == "LV0.p"], station, "/00_full_dataset/", station, "_", year_i + 1, "_lv0.dat")

  # load manual filters ==> flag 2 system error, flag 6 plausibility and flag 7 Decreased accuracy
  db.filter <- read.table(paste0(paste0(p.1$w[p.1$n == "settings.p"]), "filter.files/Ba_filter_", year_i, ".dat"),
                          sep = ",", dec = ".", header = T)
  db.filter <- db.filter[db.filter$dataset == station, ]
  db.filter[, 1] <- format(as.POSIXct(db.filter[, 1], origin = origin, tz = "UTC", format = '%Y-%m-%d %H:%M:%S'), format = '%Y-%m-%d %H:%M')
  db.filter[, 2] <- format(as.POSIXct(db.filter[, 2], origin = origin, tz = "UTC", format = '%Y-%m-%d %H:%M:%S'), format = '%Y-%m-%d %H:%M')

  # load maintenance filters flag 3
  db.maint <- read.table(paste0(paste0(p.1$w[p.1$n == "settings.p"]), "maintenance.files/Ba_maintenance_", year_i, ".dat"),
                         sep = ",", dec = ".", header = T)
  db.maint <- db.maint[db.maint$dataset == station, ]
  db.maint$from <- format(as.POSIXct(db.maint$from, origin = origin, tz = "UTC", format = '%Y-%m-%d %H:%M:%S'), format = '%Y-%m-%d %H:%M')
  db.maint$to <- format(as.POSIXct(db.maint$to, origin = origin, tz = "UTC", format = '%Y-%m-%d %H:%M:%S'), format = '%Y-%m-%d %H:%M')

  # read level 0 data
  lv0.data <- read.table(file.name.main, sep = ",", dec = ".", header = T)
  # set time format
  lv0.data[, 1] <- format(as.POSIXct(lv0.data[, 1], origin = origin, tz = "UTC", format = '%Y-%m-%d %H:%M'), format = '%Y-%m-%d %H:%M')

  # ndays.year_i: number of days of the processed year (this has to be calculated for each year, because of leap years)
  ndays.year_i <- round(difftime(lv0.data$UTC[length(lv0.data$UTC)], lv0.data$UTC[1], units = 'days'))

  # compute time resolution in values measured per day
  time.res <- 1 / as.numeric(difftime(lv0.data$UTC[2], lv0.data$UTC[1], units = 'days'))

  # add a) the last week of the previous year and b) the first week of the next year if available
  # needed for moving window operations
  time.extra <- time.res * 7

  # a) add the last week of the previous year
  if (year_i > years[1]) {
    tmp <- read.table(file.name.before, sep = ",", dec = ".", header = T)
    tmp[, 1] <- format(as.POSIXct(tmp[, 1], origin = options()$origin, tz = 'UTC', format = '%Y-%m-%d %H:%M'), format = '%Y-%m-%d %H:%M')
    tmp <- tmp[(nrow(tmp) - time.extra + 1):nrow(tmp), ]
  } else {
    tmp <- lv0.data[(nrow(lv0.data) - time.extra + 1):nrow(lv0.data), ]
    tmp[, 2:ncol(lv0.data)] <- NA


# version 2: replace the next line with the following line (because of leap years)
    tmp[, 1] <- format((as.POSIXct(tmp[, 1]) - difftime(strptime('1999-01-01','%Y-%m-%d'), strptime('1998-01-01','%Y-%m-%d'))), format = '%Y-%m-%d %H:%M')
    #tmp[, 1] <- format((as.POSIXct(tmp[, 1]) - ndays.year_i), format = '%Y-%m-%d %H:%M')
  }
  lv0.data <- rbind(tmp, lv0.data)

  # b) add the first week of the next year
  if (year_i < years[length(years)]) {
    tmp <- read.table(file.name.after, sep = ",", dec = ".", header = T)
    tmp[, 1] <- format(as.POSIXct(tmp[, 1], origin = options()$origin, tz = 'UTC', format = '%Y-%m-%d %H:%M'), format = '%Y-%m-%d %H:%M')
    tmp <- tmp[1:time.extra, ]
  } else {
    tmp <- lv0.data[(time.extra + 1):(time.extra * 2), ]
    tmp[, 2:ncol(tmp)] <- NA
    # tmp[, 1] <- format((as.POSIXct(tmp[, 1]) + difftime(strptime('1999-01-01', '%Y-%m-%d'), strptime('1998-01-01', '%Y-%m-%d'))), format = '%Y-%m-%d %H:%M')
    tmp[, 1] <- format((as.POSIXct(tmp[, 1]) + ndays.year_i), format = '%Y-%m-%d %H:%M')
  }
  lv0.data <- rbind(lv0.data, tmp)

  rownames(lv0.data) <- NULL
  rm(tmp)

  # make all columns numeric (for some reason BaSnow ends up having factor or character columns)
  tmp <- which(sapply(lv0.data, is.factor))
  if (length(tmp) > 0) {
    lv0.data[, tmp] <- as.numeric(as.character(lv0.data[, tmp]))
  }
  tmp <- which(sapply(lv0.data, is.character))
  if (length(tmp) > 1) {
    lv0.data[, tmp[2:length(tmp)]] <- as.numeric(lv0.data[, tmp[2:length(tmp)]])
  }
  rm(tmp)

  # remove columns which are not needed (and not wanted) for level 1
  # !! It is best to exclude those columns here, that the variables col.cat, cols, Iflag are correctly defined
  # and that the calculations (for example vwc) and the flagging routines are performed with the correct set of columns only.
  # Otherwise the final set of columns would have to be selected after the flagging routines and calculations, which is more error-prune.
  # Especially if the order of the columns is changing, this would have be to adapted consistently in different parts of the script and possibly also in sub-scripts. !!
  tmp <- c(which(grepl('Tpan', colnames(lv0.data)) |
                 grepl('batt_U', colnames(lv0.data)) |
                 grepl('Bat_V', colnames(lv0.data)) |
                 grepl('TableFlag', colnames(lv0.data)) |
                 grepl('dist', colnames(lv0.data)) |
                 grepl('raw', colnames(lv0.data)) |
                 grepl('QA', colnames(lv0.data)) |
                 grepl('sq', colnames(lv0.data)) |
                 grepl('vwc_sn', colnames(lv0.data))))
                  ###############
                  # new condition above:
                  # remove vwc for sensors above the ground "vwc_sn_xxx"
                  # those sensors are not used in the external vwc-calculation
                  # instead vwc is calculated internally in the sensor
                  ###############

  if (length(tmp) > 0) {
    lv0.data <- lv0.data[, -tmp]
  }
  rm(tmp)

  # add one column with a flag to each data series
  lv1.data <- get.100flag.columns(lv0.data)

  # delete level 0 data
  rm(lv0.data)

  # Define data categories based on column names
  cats <- c('Tair', 'prec', 'RH', 'Dsn', 'RadNet', 'SwIn', 'SwOut', 'LwIn', 'LwOut', 'windv', 'winddeg', 'windsddeg',
            'Ts', 'vwc', 'cond', 'E2', 'E2sn', 'G', 'SwNet', 'LwNet', 'Albedo', 'distcor', 'CountsK', 'CountsTl', 'KTl',
            'SWE', 'Tcryst', 'rho')

  ####################
  # If the column names are assigned to several categories the flagging does not work.
  # The function grepl searches for the patterns of cats in the column names.
  # ==> The names of the categories have to be such that in this search each column is assigned to one distinct category only.
  # Check with View (col.cat) for non-unique assignment of column names after the following loops.
  ####################

  # Retrieve columns in the data for each category
  col.cat <- as.data.frame(setNames(replicate(length(cats), numeric(100), simplify = F), cats))
  Iflag <- which(grepl('_fl', colnames(lv1.data)))
  tmp <- colnames(lv1.data[Iflag - 1])
  # remove '_' from column names
  for (i in 1:length(tmp)) {
    tmp[i] <- gsub('_', '', tmp[i])
  }

  # special case for non-unique grepl search pattern "E2" and "E2sn"
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

  # View(col.cat)

  # get all column names
  cols <- colnames(lv1.data)

  ## ==============================================================================
  ##
  ## FILTER for LEVEL 1
  ## ==============================================================================
  ##
  ## flag == 1    (no data)
  ##
  # Snow depth sensor no data value changes some times between 1.4 and 1.6
  if (station %in% c("BaMet2009")) {
    i <- which(cats == 'Dsn')
    if ('Dsn' %in% colnames(lv1.data)) {
      lv1.data[which(lv1.data$Dsn == 1.46), 'Dsn'] <- NA
      lv1.data[which(lv1.data$Dsn == 1.45), 'Dsn'] <- NA
      lv1.data[which(lv1.data$Dsn == 1.50), 'Dsn'] <- NA
      lv1.data[which(lv1.data$Dsn == 1.57), 'Dsn'] <- NA
      # add this for year > 2020
      # lv1.data[which(lv1.data$Dsn == 1.56), 'Dsn'] <- NA
    }
  }
  # all others
  m <- (ncol(lv1.data) - 1) / 2
  for (i in 1:m) {
    # set NA if no data
    lv1.data[which(lv1.data[, (i * 2)] <= (-99999)), (i * 2)] <- NA
        if (station != "BaSnow2019cs") {
        # set NA if values above treshold, with exception of station BaSnow2019cs
        lv1.data[which(lv1.data[, (i * 2)] >= (100000)), (i * 2)] <- NA
        }
    # set flag to 1 if no data
    lv1.data[which(is.na(lv1.data[, (i * 2)]) == TRUE), (i * 2) + 1] <- 1
  }
  rm(i, m)


  ## ==============================================================================
  ##
  ## flag == 2     (System error)
  ##
  tmpflag <- lv1.data
  db.filter.2 <- db.filter[db.filter$flag == "2", ]
  if (nrow(db.filter.2) > 0) {
    for (row.i in 1:nrow(db.filter.2)) {
      tmpflag[which(lv1.data$UTC >= db.filter.2$from[row.i] &
                      lv1.data$UTC <= db.filter.2$to[row.i]),
              which(colnames(lv1.data) == db.filter.2$variable[row.i]) + 1] <- 2
    }
    lv1.data <- update.flags(lv1.data, tmpflag, Iflag, 2)
  }

  # Additional errors that can only be identified based on other variables:
  tmpflag <- add.systemerror(tmpflag, station)
  lv1.data <- update.flags(lv1.data, tmpflag, Iflag, 2)

  ## ==============================================================================
  ##
  ## flag == 3    (Maintenance)
  ##
  tmpflag <- lv1.data
  if (nrow(db.maint) > 0) {
    for (row.i in 1:nrow(db.maint)) {
      tmpflag[which(lv1.data$UTC >= db.maint$from[row.i] &
                    lv1.data$UTC <= db.maint$to[row.i]),
              which(colnames(lv1.data) == db.maint$variable[row.i]) + 1] <- 3

    }
  lv1.data <- update.flags(lv1.data, tmpflag, Iflag, 3)
  }

  ## ==============================================================================
  ##
  ## flag == 4     (Physical limits)
  ##
  tmpflag <- physical.limits.ba(lv1.data, col.cat)
  lv1.data <- update.flags(lv1.data, tmpflag, Iflag, 4)

  ## ==============================================================================
  ##
  ## flag == 5    (Gradient)
  ##

  if (run.year < 2019) {
    if (mit.peak.detection == 1) {
      tmpflag <- detect.peaks(lv1.data, col.cat, station)
      lv1.data <- update.flags(lv1.data, tmpflag, Iflag, 5)
    }
  }
  ## ==============================================================================
  ##
  ## flag == 6    (Plausibility)
  ##
  tmpflag <- lv1.data
  db.filter.6 <- db.filter[db.filter$flag == "6", ]
  if (nrow(db.filter.6) > 0) {
    for (row.i in 1:nrow(db.filter.6)) {
      tmpflag[which(lv1.data$UTC >= db.filter.6$from[row.i] &     # Minimum Time
                      lv1.data$UTC <= db.filter.6$to[row.i] &     # Maximum Time
                      lv1.data[, which(colnames(lv1.data) == db.filter.6$variable[row.i])] >= db.filter.6$min[row.i] &     # Minimum Value
                      lv1.data[, which(colnames(lv1.data) == db.filter.6$variable[row.i])] <= db.filter.6$max[row.i]),    # Maximum Value
              which(colnames(lv1.data) == db.filter.6$variable[row.i]) + 1] <- 6
    }
  lv1.data <- update.flags(lv1.data, tmpflag, Iflag, 6)
  }

  tmpflag <- add.plausibility(lv1.data, station)
  lv1.data <- update.flags(lv1.data, tmpflag, Iflag, 6)

  ## ==============================================================================
  ##
  ## flag == 7    (Decreased accuracy)
  ##
  tmpflag <- lv1.data
  db.filter.7 <- db.filter[db.filter$flag == "7", ]
  if (nrow(db.filter.7) > 0) {
    for (row.i in 1:nrow(db.filter.7)) {
      tmpflag[which(lv1.data$UTC >= db.filter.7$from[row.i] &     # Minimum Time
                      lv1.data$UTC <= db.filter.7$to[row.i] &     # Maximum Time
                      lv1.data[, which(colnames(lv1.data) == db.filter.7$variable[row.i])] >= db.filter.7$min[row.i] &     # Minimum Value
                      lv1.data[, which(colnames(lv1.data) == db.filter.7$variable[row.i])] <= db.filter.7$max[row.i]),    # Maximum Value
              which(colnames(lv1.data) == db.filter.7$variable[row.i]) + 1] <- 7
    }
  lv1.data <- update.flags(lv1.data, tmpflag, Iflag, 7)
  }

  tmpflag <- detect.T.degradation(lv1.data, col.cats, station)
  lv1.data <- update.flags(lv1.data, tmpflag, Iflag, 7)

  ## ==============================================================================
  ##
  ## flag == 8    (Snow covered)
  ##
## time.res is overwritten with default value 24 in db_filter_II.R
  tmpflag <- detect.snow.cover(lv1.data, col.cats, time.res)
  lv1.data <- update.flags(lv1.data, tmpflag, Iflag, 8)
  rm(tmpflag)

  ## ==============================================================================
  ##
  ## Soil moisture computation following Roth
  ##

# can be deleted after check
#####################
# # remove vwc for sensors above the ground "vwc_sn_xxx"
# # those sensors are not used in the external vwc-calculation
# # instead vwc is calculated internally in the sensor
# tmp <- which(grepl('vwc_sn', colnames(lv1.data)))
# if (length(tmp) > 0) {
#   lv1.data <- lv1.data[, -tmp]
# }
# rm(tmp)

# remove the "vwc_sn_xxx" columns from the index of the flags Iflag
#Iflag <- which(grepl('_fl', colnames(lv1.data)))

#####################

  if (station %in% c("BaSoil1998", "BaSoil2009")) {
    # path.input: path for threshold table for calculation of volumetric water content (vwc)
    path.input <- paste(p.1$w[p.1$n == "settings.p"], "vwc.thresholds/threshold_", station, "_", sep = "")
    # tab.vwc_calc: table with column names of soil temperature (Ts), dielectricity (E2), porosity (phi) and vwc of the first year (theta_tot_prior) for the the calculation of vwc
    tab.vwc_calc <- read.table(file = paste(p.1$w[p.1$n == "settings.p"], "vwc_calc_columns_TSoil+E2.csv", sep = ""),
                               header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)
    # mw.width: width of moving window to calculate vwc
    mw.width <- time.res
    lv1.data <- compute.vwc(lv1.data, col.cat, station, years, mw.width, path.input, tab.vwc_calc)
  }

  ##==============================================================================
  ##
  ## Inheritance of flags
  ## PART A

  ###########
  # For the calculated variables, keep the flags of the variables used in the calculation.
  # Because the default flag is 100, the function min can be used for this purpose.
  # The "good data" flag 100 is changed to 0 in the next section.
  ###########

  # dataset "BaSnow2019cs"
  # a) The snow depth ("Dsn") of "BaSnow2019cs" is inherited from dataset "BaSnow2019sr" ==> so it inherits also its flag
  #   ==> see RAW_to_LV0_BaSnow2019.R for the inheritance of "Dsn"
  # b) snow density ("rho_K") is calculated from snow depth ("Dsn") and snow water equivalent determined with K ("SWE_K") and inherits their flags
  #   ==> see RAW_to_LV0_BaSnow2019.R for the calculation of "rho_K"
  # c) Flags of "K" and "Tl" are inherited to "KTl_Ratio".
  if (station == 'BaSnow2019cs') {
    # a)
    ba.snow.sr <- read.table(file = paste0(p.1$w[p.1$n == "LV1.p"], "BaSnow2019sr/00_full_dataset/BaSnow2019sr_", year_i, "_lv1.dat"),
                             sep = ",", dec = ".", header = T)

    # ind.6hours.ba.snow.sr: index of the hours "00:00, 06:00, 12:00, 18:00" (6 hours temporal resolution) in the ba.snow.sr data set
    # ==> this is the same temporal resolution like "BaSnow2019cs"
    ind.6hours.ba.snow.sr <- seq(1, nrow(ba.snow.sr), by = 6)
    # ind.lv1.data_6hours.ba.snow.sr: index of the same time stamp in the lv1.data (here the BaSnow2019cs data set) like ind.6hours.ba.snow.sr
    ind.lv1.data_6hours.ba.snow.sr <- which(lv1.data$UTC %in% ba.snow.sr[ind.6hours.ba.snow.sr, "UTC"])

    # assign the "Dsn_fl" values from the ba.snow.sr data set to lv1.data
    lv1.data$Dsn_fl[ind.lv1.data_6hours.ba.snow.sr] <- ba.snow.sr[ind.6hours.ba.snow.sr, "Dsn_fl"]

    # replace flag 0 with 100, because the default flag is 100
    # ==> this enables the inheritance of the flags with function min
    # ==> at the end flags 100 will be replaced with 0
    lv1.data$Dsn_fl[which(lv1.data$Dsn_fl == 0)] <- 100

    ###########
    # b) snow density ("rho_K") is calculated from snow depth ("Dsn") and snow water equivalent determined with K ("SWE_K")
    # and the snow density flag is inherited from both
    lv1.data$rho_K_fl <- apply(lv1.data[, c("rho_K_fl", "SWE_K_fl", "Dsn_fl")], 1, FUN = min)

    ###########
    # c)
    # KTl_Ratio
    lv1.data$KTl_Ratio_fl <- apply(lv1.data[, c("KTl_Ratio_fl", "CountsK_fl", "CountsTl_fl")], 1, FUN = min)
  }

  ###########
  # d) SwNet, LwNet and Albedo inherit the flags of SwIn, SwOut, LwIn and LwOut
  if (station == "BaMet2009") {
    # SwNet
    lv1.data$SwNet_fl <- apply(lv1.data[, c("SwNet_fl", "SwIn_fl", "SwOut_fl")], 1, FUN = min)
    # LwNet
    lv1.data$LwNet_fl <- apply(lv1.data[, c("LwNet_fl", "LwIn_fl", "LwOut_fl")], 1, FUN = min)
    # Albedo
    lv1.data$Albedo_fl <- apply(lv1.data[, c("Albedo_fl", "SwIn_fl", "SwOut_fl")], 1, FUN = min)
  }

  ## ==============================================================================
  ##
  ## !! Replace the remaining default flag 100 values with flag 0 !!
  ## Save the data with flags and as noflag version
  ##

  # remove extra periods from previous and next years
  lv1.data <- lv1.data[(time.extra + 1):(nrow(lv1.data) - time.extra), ]

  # update index of flagging columns before reformatting "no flag" from 100 to 0
  Iflag <- which(grepl('_fl', colnames(lv1.data)))

  # set 'no flag' to 0 instead of 100
  # because the function get.100flag.columns() from db_filter_I sets the default flag 100
  tmp <- lv1.data[, Iflag]
  tmp[tmp == 100] <- 0
  lv1.data[, Iflag] <- tmp
  rm(tmp)


  ######################
  ########################
  # Inheritance of flags
  # PART B
  if (station == "BaMet2009") {
    ########################
    ########################
    # in Version 2 RadNet will be changed analogue to the above radiation parameters in PART A to the minimum of all non-zero flags and moved there
    ########################
    # analogue to Pangaea data set
    # use the maximum flag of all radiation flags for the RadNet flag
    lv1.data$RadNet_fl <- apply(lv1.data[, c("SwIn_fl", "SwOut_fl", "LwIn_fl", "LwOut_fl")], 1, FUN = max)
  }
  ########################

  # create noflag dataset where all flagged data is set to 'NA'
  lv1.data.noflag <- lv1.data
  for (i in Iflag) {# set data to NA if flag is not 0
    lv1.data.noflag[which(lv1.data.noflag[, i] >= 1), (i - 1)] <- NA
  }

  lv1.data[, 1] <- format(as.POSIXct(lv1.data[, 1], origin = origin, tz = 'UTC', format = '%Y-%m-%d %H:%M'), format = '%Y-%m-%d %H:%M')
  lv1.data.noflag[, 1] <- format(as.POSIXct(lv1.data.noflag[, 1], origin = origin, tz = 'UTC', format = '%Y-%m-%d %H:%M'), format = '%Y-%m-%d %H:%M')

  # make flag columns character columns to avoid printing many digits
  lv1.data[, Iflag] <- sapply(lv1.data[, Iflag], as.character)

  # write files
  write.table(x = lv1.data,
              file = paste0(p.1$w[p.1$n == "LV1.p"], station, "/00_full_dataset/", station, "_", year_i, "_lv1.dat"),
              quote = F, dec = ".", sep = ",", row.names = F)
  write.table(x = lv1.data.noflag[, c(1, seq( 2, (ncol(lv1.data) - 1), by = 2))],
              file = paste0(p.1$w[p.1$n == "LV1.p"], station, "/00_full_dataset/", station, "_", year_i, "_lv1_noflag.dat"),
              quote = F, dec = ".", sep = ",", row.names = F)

  ###############################################################################################################################
  log.peaks(station, year_i, mit.peak.detection)
  cat("#\n# level1 ", station,": ", year_i, "without problems!\n#\n")
}

