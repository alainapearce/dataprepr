#' score_sleeplog: Score data from the week-long sleep log
#'
#' This function scores the week-long sleep log used for study BRAKE during the week children wore an actigraphy watch
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The columns/variables must match the following naming convention: 'var_day' where var is the variable measured (e.g., bedtime, date, etc) and day is the abbreviation for day of the week
#'
#' Note, as long as variable names match those listed in this script, the dataset can include other variables
#'
#' @param sleep_data a data.frame all items for the the week-long sleep log following the naming conventions described above
#' @inheritParams score_pds
#' @param summer_start a date for the start of summer holiday (check area school districts during data collection). Must be formatted as character string 'YYYY-MM-DD'.
#' @param summer_end a date for the end of summer holiday (check area school districts during data collection). Must be formatted as character string 'YYYY-MM-DD'.
#'
#' @return A dataset with total score for the the week-long sleep log
#' @examples
#'
#' # scoring for the sleep log with IDs
#' sleep_score_data <- score_sleeplog(sleep_data, id = 'ID', summer_start = '2023-06-06', summer_end = '2023-08-23')
#'
#' \dontrun{
#' } 
#'
#'
#' @export

score_sleeplog <- function(sleep_data, id, summer_start, summer_end) {
  
  #### 1. Set up/initial checks #####
  
  # check that sleep_data exist and is a data.frame
  data_arg <- methods::hasArg(sleep_data)
  
  if (isTRUE(data_arg) & !is.data.frame(sleep_data)) {
    stop('sleep_data must be entered as a data.frame')
  } else if (isFALSE(data_arg)) {
    stop('sleep_data must set to the data.frame with amount consumed for each food item')
  }
  
  # check if id exists
  ID_arg <- methods::hasArg(id)
  
  if (isTRUE(ID_arg)){
    if (!(id %in% names(sleep_data))) {
      stop('variable name entered as id is not in sleep_data')
    }
  }
  
  #### 2. Set Up Data #####
  
  # set up database for results create empty matrix
  sleep_score_dat <- data.frame(is_summer = rep(NA, nrow(sleep_data)), bedtime = rep(NA, nrow(sleep_data)), bedtime_wkday = rep(NA, nrow(sleep_data)), bedtime_wkend = rep(NA, nrow(sleep_data)), bedtime_range_wkday = rep(NA, nrow(sleep_data)), bedtime_range_wkend = rep(NA, nrow(sleep_data)), awake = rep(NA, nrow(sleep_data)), awake_wkday = rep(NA, nrow(sleep_data)), awake_wkend = rep(NA, nrow(sleep_data)), awake_range_wkday = rep(NA, nrow(sleep_data)), awake_range_wkend = rep(NA, nrow(sleep_data)), sleep_latency = rep(NA, nrow(sleep_data)), sleep_latency_wkday = rep(NA, nrow(sleep_data)), sleep_latency_wkend = rep(NA, nrow(sleep_data)), sleep_latency_range_wkday = rep(NA, nrow(sleep_data)), sleep_latency_range_wkend = rep(NA, nrow(sleep_data)), night_wakings = rep(NA, nrow(sleep_data)), night_wakings_wkday = rep(NA, nrow(sleep_data)), night_wakings_wkend = rep(NA, nrow(sleep_data)), night_wakings_range_wkday = rep(NA, nrow(sleep_data)), night_wakings_range_wkend = rep(NA, nrow(sleep_data)), sleep_hrs = rep(NA, nrow(sleep_data)), sleep_hrs_wkday = rep(NA, nrow(sleep_data)), sleep_hrs_wkend = rep(NA, nrow(sleep_data)), sleep_hrs_range_wkday = rep(NA, nrow(sleep_data)), sleep_hrs_range_wkend = rep(NA, nrow(sleep_data)), in_bed_hr = rep(NA, nrow(sleep_data)), in_bed_wkday = rep(NA, nrow(sleep_data)), in_bed_wkend = rep(NA, nrow(sleep_data)), in_bed_range_wkday = rep(NA, nrow(sleep_data)), in_bed_range_wkend = rep(NA, nrow(sleep_data)))
  
  if (isTRUE(ID_arg)) {
    sleep_score_dat <- data.frame(sleep_data[[id]], sleep_score_dat)
    names(sleep_score_dat)[1] <- id
  }
  
  # fix non-military times
  
  ## bedtime 
  sleep_data[, grepl('bedtime', names(sleep_data))] <- lapply(sleep_data[, grepl('bedtime', names(sleep_data))], function(x) ifelse(lubridate::hour(as.POSIXct(x, format = '%H:%M')) > 5 & lubridate::hour(as.POSIXct(x, format = '%H:%M')) <= 11, paste0(lubridate::hour(as.POSIXct(x, format = '%H:%M')) + 12, ':', sprintf("%02d", lubridate::minute(as.POSIXct(x, format = '%H:%M')))), x))
  
  ## attempt sleep
  sleep_data[, grepl('attempt', names(sleep_data))] <- lapply(sleep_data[, grepl('attempt', names(sleep_data))], function(x) ifelse(lubridate::hour(as.POSIXct(x, format = '%H:%M')) > 5 & lubridate::hour(as.POSIXct(x, format = '%H:%M')) <= 11, paste0(lubridate::hour(as.POSIXct(x, format = '%H:%M')) + 12, ':', sprintf("%02d", lubridate::minute(as.POSIXct(x, format = '%H:%M')))), x))
  
  ## fall asleep
  sleep_data[, grepl('asleep', names(sleep_data))] <- lapply(sleep_data[, grepl('asleep', names(sleep_data))], function(x) ifelse(lubridate::hour(as.POSIXct(x, format = '%H:%M')) > 5 & lubridate::hour(as.POSIXct(x, format = '%H:%M')) <= 11, paste0(lubridate::hour(as.POSIXct(x, format = '%H:%M')) + 12, ':', sprintf("%02d", lubridate::minute(as.POSIXct(x, format = '%H:%M')))), x))
  
  # new data set
  sleep_data_edit <- sleep_data
  
  ## Score - used sum to get total servings per week
  
  #need to check for low military time (e.g., 1 am = 01:00 and add 24 hours to them before getting mean bedtime)
  time_hours <- function(time){
    #add hours to min/60 to get numeric value of time
    num_time <- 
      ifelse(lubridate::hour(as.POSIXct(time, format = '%H:%M')) < 3, 
             #add 24 hours if it wraps to next day (hour < 3)
             24 + lubridate::hour(as.POSIXct(time, format = '%H:%M')) + lubridate::minute(as.POSIXct(time, format = '%H:%M'))/60, 
             lubridate::hour(as.POSIXct(time, format = '%H:%M')) + lubridate::minute(as.POSIXct(time, format = '%H:%M'))/60)
    
    return(num_time)
  }
  
  ### bedtime
  #average
  bedtime_avg <- rowMeans(as.data.frame(lapply(sleep_data_edit[, grepl('bedtime', names(sleep_data_edit))], function(x) time_hours(x))), na.rm = TRUE)
  
  bedtime_avg_time <- sapply(bedtime_avg, function(x) ifelse(is.nan(x), NA, paste0(floor(x), ':', sprintf("%02d",round((x - floor(x))*60, 0)))), simplify = TRUE)
  
  sleep_score_dat[['bedtime']] <- bedtime_avg_time
  
  #weekday
  bedtime_wkday <- rowMeans(as.data.frame(lapply(sleep_data_edit[, grepl('bedtime_mon|bedtime_tu|bedtime_wed|bedtime_th|bedtime_fri', names(sleep_data_edit))], function(x) time_hours(x))), na.rm = TRUE)
  
  bedtime_wkday_time <- sapply(bedtime_wkday, function(x) ifelse(is.nan(x), NA, paste0(floor(x), ':', sprintf("%02d",round((x - floor(x))*60, 0)))), simplify = TRUE)
  
  sleep_score_dat[['bedtime_wkday']] <- bedtime_wkday_time
  
  #weekend
  bedtime_wkend <- rowMeans(as.data.frame(lapply(sleep_data_edit[, grepl('bedtime_sat|bedtime_sun', names(sleep_data_edit))], function(x) time_hours(x))), na.rm = TRUE)
  
  bedtime_wkend_time <- sapply(bedtime_wkend, function(x) ifelse(is.nan(x), NA, paste0(floor(x), ':', sprintf("%02d",round((x - floor(x))*60, 0)))), simplify = TRUE)
  
  sleep_score_dat[['bedtime_wkend']] <- bedtime_wkend_time
  
  #weekday range
  weekday_bedtimes <- as.data.frame(lapply(sleep_data_edit[, grepl('bedtime_mon|bedtime_tu|bedtime_wed|bedtime_th|bedtime_fri', names(sleep_data_edit))], function(x) time_hours(x)))
  
  bedtime_wkday_min <- ifelse(rowSums(is.na(weekday_bedtimes)) < 5, apply(weekday_bedtimes[rowSums(is.na(weekday_bedtimes)) < 5, ], 1, FUN = min, na.rm = TRUE), NA)
  
  bedtime_wkday_max <- ifelse(rowSums(is.na(weekday_bedtimes)) < 5, apply(weekday_bedtimes[rowSums(is.na(weekday_bedtimes)) < 5, ], 1, FUN = max, na.rm = TRUE), NA)

  sleep_score_dat[['bedtime_range_wkday']] <- round(bedtime_wkday_max - bedtime_wkday_min, 2)
  
  #weekend range
  weekend_bedtimes <- as.data.frame(lapply(sleep_data_edit[, grepl('bedtime_sat|bedtime_sun', names(sleep_data_edit))], function(x) time_hours(x)))
  
  bedtime_wkend_min <- ifelse(rowSums(is.na(weekend_bedtimes)) < 2, apply(weekend_bedtimes[rowSums(is.na(weekend_bedtimes)) < 2, ], 1, FUN = min, na.rm = TRUE), NA)

  bedtime_wkend_max <- ifelse(rowSums(is.na(weekend_bedtimes)) < 2, apply(weekend_bedtimes[rowSums(is.na(weekend_bedtimes)) < 2, ], 1, FUN = max, na.rm = TRUE), NA)
  
  sleep_score_dat[['bedtime_range_wkend']] <- round(bedtime_wkend_max - bedtime_wkend_min, 2)
  
  ### awake
  sleep_data_edit[, grepl('awake', names(sleep_data_edit))] <- sapply(sleep_data_edit[, grepl('awake', names(sleep_data_edit))], function(x) ifelse(x == '', NA, x))
  
  #average
  awake_avg <- rowMeans(as.data.frame(lapply(sleep_data_edit[, grepl('awake', names(sleep_data_edit))], function(x) time_hours(x))), na.rm = TRUE)
  
  awake_avg_time <- sapply(awake_avg, function(x) ifelse(is.nan(x), NA, paste0(floor(x), ':', sprintf("%02d",round((x - floor(x))*60, 0)))), simplify = TRUE)
  
  sleep_score_dat[['awake']] <- awake_avg_time
  
  #weekday
  awake_wkday <- rowMeans(as.data.frame(lapply(sleep_data_edit[, grepl('awake_mon|awake_tu|awake_wed|awake_th|awake_fri', names(sleep_data_edit))], function(x) time_hours(x))), na.rm = TRUE)
  
  awake_wkday_time <- sapply(awake_wkday, function(x) ifelse(is.nan(x), NA, paste0(floor(x), ':', sprintf("%02d",round((x - floor(x))*60, 0)))), simplify = TRUE)
  
  sleep_score_dat[['awake_wkday']] <- awake_wkday_time
  
  #weekend
  awake_wkend <- rowMeans(as.data.frame(lapply(sleep_data_edit[, grepl('awake_sat|awake_sun', names(sleep_data_edit))], function(x) time_hours(x))), na.rm = TRUE)
  
  awake_wkend_time <- sapply(awake_wkend, function(x) ifelse(is.nan(x), NA, paste0(floor(x), ':', sprintf("%02d",round((x - floor(x))*60, 0)))), simplify = TRUE)
  
  sleep_score_dat[['awake_wkend']] <- awake_wkend_time
  
  #weekday range
  weekday_awake <- as.data.frame(lapply(sleep_data_edit[, grepl('awake_mon|awake_tu|awake_wed|awake_th|awake_fri', names(sleep_data_edit))], function(x) time_hours(x)))
  
  awake_wkday_min <- ifelse(rowSums(is.na(weekday_awake)) < 5, apply(weekday_awake[rowSums(is.na(weekday_awake)) < 5, ], 1, FUN = min, na.rm = TRUE), NA)
  
  awake_wkday_max <- ifelse(rowSums(is.na(weekday_awake)) < 5, apply(weekday_awake[rowSums(is.na(weekday_awake)) < 5, ], 1, FUN = max, na.rm = TRUE), NA)

  sleep_score_dat[['awake_range_wkday']] <- round(awake_wkday_max - awake_wkday_min, 2)
  
  #weekend range
  weekend_awake <- as.data.frame(lapply(sleep_data_edit[, grepl('awake_sat|awake_sun', names(sleep_data_edit))], function(x) time_hours(x)))
  
  awake_wkend_min <- ifelse(rowSums(is.na(weekend_awake)) < 2, apply(weekend_awake[rowSums(is.na(weekend_awake)) < 2, ], 1, FUN = min, na.rm = TRUE), NA)
  
  awake_wkend_max <- ifelse(rowSums(is.na(weekend_awake)) < 2, apply(weekend_awake[rowSums(is.na(weekend_awake)) < 2, ], 1, FUN = max, na.rm = TRUE), NA)

  sleep_score_dat[['awake_range_wkend']] <- round(awake_wkend_max - awake_wkend_min, 2)
  
  ### Sleep Latency
  sleep_data_edit[['sleep_latency_mon']] <- time_hours(sleep_data_edit[['asleep_mon']]) - time_hours(sleep_data_edit[['attempt_mon']])
  
  sleep_data_edit[['sleep_latency_tu']] <- time_hours(sleep_data_edit[['asleep_tu']]) - time_hours(sleep_data_edit[['attempt_tu']])
  
  sleep_data_edit[['sleep_latency_wed']] <- time_hours(sleep_data_edit[['asleep_wed']]) - time_hours(sleep_data_edit[['attempt_wed']])
  
  sleep_data_edit[['sleep_latency_th']] <- time_hours(sleep_data_edit[['asleep_th']]) - time_hours(sleep_data_edit[['attempt_th']])
  
  sleep_data_edit[['sleep_latency_fri']] <- time_hours(sleep_data_edit[['asleep_fri']]) - time_hours(sleep_data_edit[['attempt_fri']])
  
  sleep_data_edit[['sleep_latency_sat']] <- time_hours(sleep_data_edit[['asleep_sat']]) - time_hours(sleep_data_edit[['attempt_sat']])
  
  sleep_data_edit[['sleep_latency_sun']] <- time_hours(sleep_data_edit[['asleep_sun']]) - time_hours(sleep_data_edit[['attempt_sun']])
  
  #average
  sleep_score_dat[['sleep_latency']] <- round(rowMeans(sleep_data_edit[, grepl('sleep_latency', names(sleep_data_edit))], na.rm = TRUE), 2)
  
  #weekday
  sleep_score_dat[['sleep_latency_wkday']] <- round(rowMeans(sleep_data_edit[, grepl('sleep_latency_mon|sleep_latency_tu|sleep_latency_wed|sleep_latency_th|sleep_latency_fri', names(sleep_data_edit))], na.rm = TRUE), 2)
  
  #weekend
  sleep_score_dat[['sleep_latency_wkend']] <- round(rowMeans(sleep_data_edit[, grepl('sleep_latency_sat|sleep_latency_sun', names(sleep_data_edit))], na.rm = TRUE), 2)
  
  #weekday range
  weekday_sleep_latency <- sleep_data_edit[, grepl('sleep_latency_mon|sleep_latency_tu|sleep_latency_wed|sleep_latency_th|sleep_latency_fri', names(sleep_data_edit))]
  
  sleep_latency_wkday_min <- ifelse(rowSums(is.na(weekday_sleep_latency)) < 5, apply(weekday_sleep_latency[rowSums(is.na(weekday_sleep_latency)) < 5, ], 1, FUN = min, na.rm = TRUE), NA)
  
  sleep_latency_wkday_max <- ifelse(rowSums(is.na(weekday_sleep_latency)) < 5, apply(weekday_sleep_latency[rowSums(is.na(weekday_sleep_latency)) < 5, ], 1, FUN = max, na.rm = TRUE), NA)
  
  sleep_score_dat[['sleep_latency_range_wkday']] <- round(sleep_latency_wkday_max - sleep_latency_wkday_min, 2)
  
  #weekend range
  weekend_sleep_latency <- sleep_data_edit[, grepl('sleep_latency_sat|sleep_latency_sun', names(sleep_data_edit))]
  
  sleep_latency_wkend_min <- ifelse(rowSums(is.na(weekend_sleep_latency)) < 2, apply(weekend_sleep_latency[rowSums(is.na(weekend_sleep_latency)) < 2, ], 1, FUN = min, na.rm = TRUE), NA)
  
  sleep_latency_wkend_max <- ifelse(rowSums(is.na(weekend_sleep_latency)) < 2, apply(weekend_sleep_latency[rowSums(is.na(weekend_sleep_latency)) < 2, ], 1, FUN = max, na.rm = TRUE), NA)

  sleep_score_dat[['sleep_latency_range_wkend']] <- round(sleep_latency_wkend_max - sleep_latency_wkend_min, 2)
  
  ### Night Wakings
  
  #average
  sleep_score_dat[['night_wakings']] <- round(rowMeans(sleep_data_edit[, grepl('times_', names(sleep_data_edit))], na.rm = TRUE), 2)
  
  #weekday
  sleep_score_dat[['night_wakings_wkday']] <- round(rowMeans(sleep_data_edit[, grepl('times_mon|times_tu|times_wed|times_th|times_fri', names(sleep_data_edit))], na.rm = TRUE), 2)
  
  #weekend
  sleep_score_dat[['night_wakings_wkend']] <- round(rowMeans(sleep_data_edit[, grepl('times_sat|times_sun', names(sleep_data_edit))], na.rm = TRUE), 2)
  
  #weekday range
  weekday_sleep_wake <- sleep_data_edit[, grepl('times_mon|times_tu|times_wed|times_th|times_fri', names(sleep_data_edit))]
  
  sleep_wake_wkday_min <- ifelse(rowSums(is.na(weekday_sleep_wake)) < 5, apply(weekday_sleep_wake[rowSums(is.na(weekday_sleep_wake)) < 5, ], 1, FUN = min, na.rm = TRUE), NA)
  
  sleep_wake_wkday_max <- ifelse(rowSums(is.na(weekday_sleep_wake)) < 5, apply(weekday_sleep_wake[rowSums(is.na(weekday_sleep_wake)) < 5, ], 1, FUN = max, na.rm = TRUE), NA)
  
  sleep_score_dat[['night_wakings_range_wkday']] <- round(sleep_wake_wkday_max - sleep_wake_wkday_min, 2)
  
  #weekend range
  weekend_sleep_wake <- sleep_data_edit[, grepl('times_sat|times_sun', names(sleep_data_edit))]
  
  sleep_wake_wkend_min <- ifelse(rowSums(is.na(weekend_sleep_wake)) < 2, apply(weekend_sleep_wake[rowSums(is.na(weekend_sleep_wake)) < 2, ], 1, FUN = min, na.rm = TRUE), NA)
  
  sleep_wake_wkend_max <- ifelse(rowSums(is.na(weekend_sleep_wake)) < 2, apply(weekend_sleep_wake[rowSums(is.na(weekend_sleep_wake)) < 2, ], 1, FUN = max, na.rm = TRUE), NA)

  sleep_score_dat[['night_wakings_range_wkend']] <- round(sleep_wake_wkend_max - sleep_wake_wkend_min, 2)
  
  ### Sleep hours - total
  
  ## deal with awake time
  sleep_data_edit[, grepl('waso', names(sleep_data_edit))] <- sapply(sleep_data_edit[, grepl('waso', names(sleep_data_edit))], function(x) ifelse(grepl('minute|min|mins|minutes|m', x), sub('minute|min|mins|minutes|m', '', x), x))
  
  sleep_data_edit[, grepl('waso', names(sleep_data_edit))] <- sapply(sleep_data_edit[, grepl('waso', names(sleep_data_edit))], function(x) ifelse(grepl(' |  ', x), sub(' |  |   ', '', x), x))
  
  sleep_data_edit[, grepl('waso', names(sleep_data_edit))] <- sapply(sleep_data_edit[, grepl('waso', names(sleep_data_edit))], function(x) ifelse(grepl('Na|<', x), sub('Na|<', '', x), x))
  
  sleep_data_edit[, grepl('waso', names(sleep_data_edit))] <- sapply(sleep_data_edit[, grepl('waso', names(sleep_data_edit))], function(x) ifelse(x == '', NA, x))
  
  sleep_data_edit[, grepl('waso', names(sleep_data_edit))] <- sapply(sleep_data_edit[, grepl('waso', names(sleep_data_edit))], function(x) ifelse(grepl('hours|hr|hrs', x), as.numeric(sub('hours|hr|hrs', '', x))*60, x))
  
  sleep_data_edit[, grepl('waso', names(sleep_data_edit))] <- sapply(sleep_data_edit[, grepl('waso', names(sleep_data_edit))], function(x) ifelse(grepl(':00|:', x), as.numeric(sub(':00|:', '', x)), x))
  
  
  ## replace '' with NA so will still compute numeric values when na.rm = TRUE
  sleep_data_edit[, grepl('date', names(sleep_data_edit))] <- sapply(sleep_data_edit[, grepl('date', names(sleep_data_edit))], function(x) ifelse(x == '', NA, x))
  
  sleep_data_edit[['first_day']] <- as.data.frame(lubridate::wday(lubridate::parse_date_time(apply(sleep_data_edit[, grepl('date', names(sleep_data_edit))], 1, FUN = min, na.rm = TRUE), '%Y-%m-%d'), label = TRUE))
  
  sleep_data_edit[, grepl('waso', names(sleep_data_edit))] <- sapply(sleep_data_edit[, grepl('waso', names(sleep_data_edit))], function(x) as.numeric(x), simplify = TRUE)
  
  sleep_data_edit[['sleep_hrs_mon']] <- ifelse(sleep_data_edit[['first_day']] != 'Tue', ifelse(is.na(sleep_data_edit[['waso_mon']]), time_hours(sleep_data_edit[['awake_tu']]) + (24 - time_hours(sleep_data_edit[['asleep_mon']])), time_hours(sleep_data_edit[['awake_tu']]) + (24 - time_hours(sleep_data_edit[['asleep_mon']])) - sleep_data_edit[['waso_mon']]/60), NA)
  
  sleep_data_edit[['sleep_hrs_tu']] <- ifelse(sleep_data_edit[['first_day']] != 'Wed', ifelse(is.na(sleep_data_edit[['waso_tu']]), time_hours(sleep_data_edit[['awake_wed']]) + (24 - time_hours(sleep_data_edit[['asleep_tu']])), time_hours(sleep_data_edit[['awake_wed']]) + (24 - time_hours(sleep_data_edit[['asleep_tu']])) - sleep_data_edit[['waso_tu']]/60), NA)
  
  sleep_data_edit[['sleep_hrs_wed']] <- ifelse(sleep_data_edit[['first_day']] != 'Thu', ifelse(is.na(sleep_data_edit[['waso_wed']]), time_hours(sleep_data_edit[['awake_th']]) + (24 - time_hours(sleep_data_edit[['asleep_wed']])), time_hours(sleep_data_edit[['awake_th']]) + (24 - time_hours(sleep_data_edit[['asleep_wed']])) - sleep_data_edit[['waso_wed']]/60), NA)
  
  sleep_data_edit[['sleep_hrs_th']] <- ifelse(sleep_data_edit[['first_day']] != 'Fri', ifelse(is.na(sleep_data_edit[['waso_th']]), time_hours(sleep_data_edit[['awake_fri']]) + (24 - time_hours(sleep_data_edit[['asleep_th']])), time_hours(sleep_data_edit[['awake_fri']]) + (24 - time_hours(sleep_data_edit[['asleep_th']])) - sleep_data_edit[['waso_th']]/60), NA)
  
  sleep_data_edit[['sleep_hrs_fri']] <- ifelse(sleep_data_edit[['first_day']] != 'Sat', ifelse(is.na(sleep_data_edit[['waso_fri']]), time_hours(sleep_data_edit[['awake_sat']]) + (24 - time_hours(sleep_data_edit[['asleep_fri']])), time_hours(sleep_data_edit[['awake_sat']]) + (24 - time_hours(sleep_data_edit[['asleep_fri']])) - sleep_data_edit[['waso_fri']]/60), NA)
  
  sleep_data_edit[['sleep_hrs_sat']] <- ifelse(sleep_data_edit[['first_day']] != 'Sun', ifelse(is.na(sleep_data_edit[['waso_sat']]), time_hours(sleep_data_edit[['awake_sun']]) + (24 - time_hours(sleep_data_edit[['asleep_sat']])), time_hours(sleep_data_edit[['awake_sun']]) + (24 - time_hours(sleep_data_edit[['asleep_sat']])) - sleep_data_edit[['waso_sat']]/60), NA)
  
  sleep_data_edit[['sleep_hrs_sun']] <- ifelse(sleep_data_edit[['first_day']] != 'Mon', ifelse(is.na(sleep_data_edit[['waso_sun']]), time_hours(sleep_data_edit[['awake_mon']]) + (24 - time_hours(sleep_data_edit[['asleep_sun']])), time_hours(sleep_data_edit[['awake_mon']]) + (24 - time_hours(sleep_data_edit[['asleep_sun']])) - sleep_data_edit[['waso_sun']]/60), NA)
  
  #average
  sleep_score_dat[['sleep_hrs']] <- round(rowMeans(sleep_data_edit[, grepl('sleep_hrs', names(sleep_data_edit))], na.rm = TRUE), 2)
  
  #weekday
  sleep_score_dat[['sleep_hrs_wkday']] <- round(rowMeans(sleep_data_edit[, grepl('sleep_hrs_mon|sleep_hrs_tu|sleep_hrs_wed|sleep_hrs_th|sleep_hrs_fri', names(sleep_data_edit))], na.rm = TRUE), 2)
  
  #weekend
  sleep_score_dat[['sleep_hrs_wkend']] <- round(rowMeans(sleep_data_edit[, grepl('sleep_hrs_sat|sleep_hrs_sun', names(sleep_data_edit))], na.rm = TRUE), 2)
  
  #weekday range
  weekday_sleep_hrs <- sleep_data_edit[, grepl('sleep_hrs_mon|sleep_hrs_tu|sleep_hrs_wed|sleep_hrs_th|sleep_hrs_fri', names(sleep_data_edit))]
  
  sleep_hrs_wkday_min <- ifelse(rowSums(is.na(weekday_sleep_hrs)) < 5, apply(weekday_sleep_hrs[rowSums(is.na(weekday_sleep_hrs)) < 5, ], 1, FUN = min, na.rm = TRUE), NA)
  
  sleep_hrs_wkday_max <- ifelse(rowSums(is.na(weekday_sleep_hrs)) < 5, apply(weekday_sleep_hrs[rowSums(is.na(weekday_sleep_hrs)) < 5, ], 1, FUN = max, na.rm = TRUE), NA)
  
  sleep_score_dat[['sleep_hrs_range_wkday']] <- round(sleep_hrs_wkday_max - sleep_hrs_wkday_min, 2)
  
  #weekend range
  weekend_sleep_hrs <- sleep_data_edit[, grepl('sleep_hrs_sat|sleep_hrs_sun', names(sleep_data_edit))]
  
  sleep_hrs_wkend_min <- ifelse(rowSums(is.na(weekend_sleep_hrs)) < 2, apply(weekend_sleep_hrs[rowSums(is.na(weekend_sleep_hrs)) < 2, ], 1, FUN = min, na.rm = TRUE), NA)
  
  sleep_hrs_wkend_max <- ifelse(rowSums(is.na(weekend_sleep_hrs)) < 2, apply(weekend_sleep_hrs[rowSums(is.na(weekend_sleep_hrs)) < 2, ], 1, FUN = max, na.rm = TRUE), NA)
  
  sleep_score_dat[['sleep_hrs_range_wkend']] <- round(sleep_hrs_wkend_max - sleep_hrs_wkend_min, 2)
  
  ### In Bed hours - total
  sleep_data_edit[, grepl('out_on', names(sleep_data_edit))] <- sapply(sleep_data_edit[, grepl('out_on', names(sleep_data_edit))], function(x) ifelse(x == '', NA, x))
  
  sleep_data_edit[['in_bed_mon']] <- ifelse(sleep_data_edit[['first_day']] != 'Tue', time_hours(sleep_data_edit[['out_on_tu']]) + (24 - time_hours(sleep_data_edit[['bedtime_mon']])), NA)
  
  sleep_data_edit[['in_bed_tu']] <- ifelse(sleep_data_edit[['first_day']] != 'Wed', time_hours(sleep_data_edit[['out_on_wed']]) + (24 - time_hours(sleep_data_edit[['bedtime_tu']])), NA)
  
  sleep_data_edit[['in_bed_wed']] <- ifelse(sleep_data_edit[['first_day']] != 'Thu', time_hours(sleep_data_edit[['out_on_th']]) + (24 - time_hours(sleep_data_edit[['bedtime_wed']])), NA)
  
  sleep_data_edit[['in_bed_th']] <- ifelse(sleep_data_edit[['first_day']] != 'Fri', time_hours(sleep_data_edit[['out_on_fri']]) + (24 - time_hours(sleep_data_edit[['bedtime_th']])), NA)
  
  sleep_data_edit[['in_bed_fri']] <- ifelse(sleep_data_edit[['first_day']] != 'Sat', time_hours(sleep_data_edit[['out_on_sat']]) + (24 - time_hours(sleep_data_edit[['bedtime_fri']])), NA)
  
  sleep_data_edit[['in_bed_sat']] <- ifelse(sleep_data_edit[['first_day']] != 'Sun', time_hours(sleep_data_edit[['out_on_sun']]) + (24 - time_hours(sleep_data_edit[['bedtime_sat']])), NA)
  
  sleep_data_edit[['in_bed_sun']] <- ifelse(sleep_data_edit[['first_day']] != 'Mon', time_hours(sleep_data_edit[['out_on_mon']]) + (24 - time_hours(sleep_data_edit[['bedtime_sun']])), NA)
  
  #average
  sleep_score_dat[['in_bed_hr']] <- round(rowMeans(sleep_data_edit[, grepl('in_bed', names(sleep_data_edit))], na.rm = TRUE), 2)
  
  #weekday
  sleep_score_dat[['in_bed_wkday']] <- round(rowMeans(sleep_data_edit[, grepl('in_bed_mon|in_bed_tu|in_bed_wed|in_bed_th|in_bed_fri', names(sleep_data_edit))], na.rm = TRUE), 2)
  
  #weekend
  sleep_score_dat[['in_bed_wkend']] <- round(rowMeans(sleep_data_edit[, grepl('in_bed_sat|in_bed_sun', names(sleep_data_edit))], na.rm = TRUE), 2)
  
  #weekday range
  weekday_in_bed <- sleep_data_edit[, grepl('in_bed_mon|in_bed_tu|in_bed_wed|in_bed_th|in_bed_fri', names(sleep_data_edit))]
  
  in_bed_wkday_min <- ifelse(rowSums(is.na(weekday_in_bed)) < 5, apply(weekday_in_bed[rowSums(is.na(weekday_in_bed)) < 5, ], 1, FUN = min, na.rm = TRUE), NA)
  
  in_bed_wkday_max <- ifelse(rowSums(is.na(weekday_in_bed)) < 5, apply(weekday_in_bed[rowSums(is.na(weekday_in_bed)) < 5, ], 1, FUN = max, na.rm = TRUE), NA)
  
  sleep_score_dat[['in_bed_range_wkday']] <- round(in_bed_wkday_max - in_bed_wkday_min, 2)
  
  #weekend range
  weekend_in_bed <- sleep_data_edit[, grepl('in_bed_sat|in_bed_sun', names(sleep_data_edit))]
  
  in_bed_wkend_min <- ifelse(rowSums(is.na(weekend_in_bed)) < 2, apply(weekend_in_bed[rowSums(is.na(weekend_in_bed)) < 2, ], 1, FUN = min, na.rm = TRUE), NA)
  
  in_bed_wkend_max <- ifelse(rowSums(is.na(weekend_in_bed)) < 2, apply(weekend_in_bed[rowSums(is.na(weekend_in_bed)) < 2, ], 1, FUN = max, na.rm = TRUE), NA)
  
  sleep_score_dat[['in_bed_range_wkend']] <- round(in_bed_wkend_max - in_bed_wkend_min, 2)
  
  
  ## mark if in non-school/summery months
  ## area school districts:
  summer_interval <- lubridate::interval(lubridate::ymd(summer_start), lubridate::ymd(summer_end))
  
  sleep_score_dat[['is_summer']] <- ifelse(lubridate::parse_date_time(apply(sleep_data_edit[, grepl('date', names(sleep_data_edit))], 1, FUN = min, na.rm = TRUE), '%Y-%m-%d') %within% summer_interval, 1, 0)
  
  #### 3. Clean Export/Scored Data #####
  
  ## remove NANs
  sleep_score_dat[sapply(sleep_score_dat, is.nan)] <- NA
  sleep_score_dat[sapply(sleep_score_dat, function(x) x == '<NA>')] <- NA
  
  ## merge raw responses with scored data
  if (isTRUE(ID_arg)){
    sleep_phenotype <- merge(sleep_data, sleep_score_dat, by = id)
    
    return(list(score_dat = as.data.frame(sleep_score_dat),
                bids_phenotype = as.data.frame(sleep_phenotype)))
  } else {
    return(list(score_dat = as.data.frame(sleep_score_dat)))
  }
  
}

