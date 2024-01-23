#' util_fbs_microstructue: Process raw microstructure coding data
#'
#' This function loads the .txt raw data file from ObserverXT for specified Meal. Cleaning the data involves:
#' 1) separating event log names to get visit, participant, and coder information,
#' 2) selecting relevant data columns,
#' 3) removing all events that are not needed/old/duplicate
#' 4) re-ordering and re-name data columns
#' 5) making wide behavioral data for summary metrics (nbites, bite rate)
#' 6) separating event logs by coder and padding for equal entrees - need to fix mismatched number of bites/sips in a different step
#' 7) creating variable labels, levels, and dictionaries
#'
#' The databases MUST follow the naming convention: 'PSX_EventLogs_YYY-MM-DD.txt' or 'vX_EventLogs_YYYY-MM-DD'
#'
#' @param filepattern string with the pattern to search for to find the raw data file. The pattern must contain meal type ('PS' for portion size; 'v' for baseline/follow-up meals) and portion or visit number (e.g., for portion size 1 it would be 'PS1' and for baseline it would be 'v1').
#' @inheritParams util_fbs_parent_v1dat
#'
#'
#' @return A list containing 2 data lists:
#' 1) beh_wide contains - data: data.frame summary metrics in wide format by code, dict: data dictionary with variable descriptions
#' 2) event contains - data: event data by coder and time, dict: data dictionary with variable descriptions
#'
#' @examples
#' #if in same working directory as data:
#' ps1_microstructure <- util_fbs_microstructue('PS1')
#'
#' \dontrun{
#' #file_pattern must be a string. The following will not run:
#' ps1_microstructure <- util_fbs_microstructue(PS1)
#'
#' #file_pattern must have the protion size number ('PS1' ... 'PS4') OR visit number for baseline/follow-up('v1', 'v7'). If just enter 'PS' or 'v', the script will not run because it will return multiple files for different visits. The following will not run:
#' ps1_microstructure <- util_fbs_microstructue('PS')
#' ps1_microstructure <- util_fbs_microstructue('v')
#' }
#'
#'
#' @export
#'
util_fbs_microstructure <- function(file_pattern, data_path) {

  #### 1. Set up/initial checks #####

  # check that file_pattern exist and is a string

  filepat_arg <- methods::hasArg(file_pattern)

  if (isTRUE(filepat_arg) & !is.character(file_pattern)) {
    stop("file_pattern must be entered as a string: e.g., 'PS1'")
  } else if (isFALSE(filepat_arg)) {
    stop("file_pattern must set to the a string matching the name of the raw data file for child visit: e.g., 'PS1'")
  }

  # check datapath
  datapath_arg <- methods::hasArg(data_path)

  if (isTRUE(datapath_arg)) {
    if (!is.character(data_path)) {
      stop("data_path must be entered as a string: e.g., '.../Participant_Data/untouchedRaw/Microstructure_Raw'")
    }
  }

  #### 2. Load Data #####

  # microstructure data data
  if (isTRUE(datapath_arg)) {
    micro_event_pathlist <- list.files(path = data_path, pattern = paste0(file_pattern, '_EventLogs'), full.names = TRUE)
  } else {
    micro_event_pathlist <- list.files(pattern = paste0(file_pattern, '_EventLogs'), full.names = TRUE)

  }

  # check number of files found
  if (length(micro_event_pathlist) > 1) {
    stop("More than one file matched the file_pattern for one of the data sets. Be sure thefile_pattern specifies both the meal and protion or visit number. If have more than 1 file matching the pattern in the directory, may need to move to enter a more specific file_pattern than is standard.")
  } else if (length(micro_event_pathlist) == 0) {
    stop('No files found for one of the data sets. Be sure the data_path and file_pattern are correct and that the file exists')
  } else {
    micro_event_path <- micro_event_pathlist
  }

  # check that file is of type 'txt'
  if (!grepl('.txt',micro_event_path, fixed = TRUE)){
    stop("The event file found is not an .txt database")
  }

  # check if file exists
  micro_event_exists <- file.exists(micro_event_path)

  # load data if it exists
  if (isTRUE(micro_event_exists)) {
    micro_dat <- as.data.frame(read.csv(micro_event_path, header = T, sep = '\t', fileEncoding = 'UTF-16'))

  } else {
    if (isTRUE(datapath_arg)) {
      stop("Event file does not exist. Check file_pattern and data_path entered")
    } else {
      stop("Event file does not exist. Check file_pattern and that the data exists in current working directory")
    }
  }

  #### 3. Clean Data #####

  # 1) organize data - remove NA rows/columns
  ## done by column name becasue can have extra/different columns across different observer projects
  if (file_pattern == 'PS1' | file_pattern == 'PS2' | file_pattern == 'PS3' | file_pattern == 'PS4'){

    names(micro_dat) <- tolower(names(micro_dat))

    if (file_pattern == 'PS2') {
      micro_dat <- micro_dat[c('time_relative_hms', 'time_relative_sf', 'duration_sf', 'observation', 'event_log', 'behavior', 'modifier_1', 'modifier_2', 'modifier_3', 'modifier_4', 'modifier_5', 'modifier_6', 'modifier_7', 'modifier_8', 'modifier_9', 'modifier_10', 'modifier_11', 'modifier_12', 'modifier_13', 'modifier_16', 'event_type', 'coder', 'end.of.meal.affect', 'engagement.with.story', 'reading.of.story', 'researcher.prompts.meal.end', 'start.of.meal.affect')]
    } else if (file_pattern == 'PS4') {
      micro_dat <- micro_dat[c('time_relative_hms', 'time_relative_sf', 'duration_sf', 'observation', 'event_log', 'behavior', 'modifier_1', 'modifier_2', 'modifier_3', 'modifier_4', 'modifier_5', 'modifier_6', 'modifier_7', 'modifier_8', 'modifier_9', 'modifier_10', 'modifier_11', 'modifier_12', 'event_type', 'coder', 'end.of.meal.affect', 'engagement.with.story', 'reading.of.story', 'researcher.prompts.meal.end', 'start.of.meal.affect')]
    } else {
      micro_dat <- micro_dat[c('time_relative_hms', 'time_relative_sf', 'duration_sf', 'observation', 'event_log', 'behavior', 'modifier_1', 'modifier_2', 'modifier_3', 'modifier_4', 'modifier_5', 'modifier_6', 'modifier_7', 'modifier_8', 'modifier_9', 'modifier_10', 'modifier_11', 'modifier_12', 'modifier_13', 'event_type', 'coder', 'end.of.meal.affect', 'engagement.with.story', 'reading.of.story', 'researcher.prompts.meal.end', 'start.of.meal.affect')]
    }

    ## split coder information
    split_obs <- strsplit(micro_dat[['observation']], '_', fixed = T)

    ## pad with NAs so all have same number of columns
    split_obs_dat <-  do.call(rbind.data.frame, lapply(split_obs , `length<-`, max(sapply(split_obs , length))))
    if (length(names(split_obs_dat)) == 7){
      names(split_obs_dat) <- c('study', 'id', 'visit', 'coder_phase1', 'coder_phase2', 'extra', 'version')
    } else if (length(names(split_obs_dat)) == 6){
      names(split_obs_dat) <- c('study', 'id', 'visit', 'coder_phase1', 'coder_phase2', 'extra')
    } else if (length(names(split_obs_dat)) == 5){
      names(split_obs_dat) <- c('study', 'id', 'visit', 'coder_phase1', 'coder_phase2')
    }

    micro_dat <- data.frame(c(micro_dat, split_obs_dat))

    ## double check each time until final coding is done
    if (file_pattern == 'PS1'){
      names(micro_dat)[c(4, 13:19, 22:26)] <- c('observation.name', 'utensil', 'talk_hunger', 'talk_fulness', 'talk_story', 'problem', 'straw', 'distraction', 'affect_mealend', 'story_engagement', 'story_read', 'endmeal_prompt', 'affect_mealstart')
    } else if (file_pattern == 'PS2'){
      names(micro_dat)[c(4, 14:20, 23:27)] <- c('observation.name', 'utensil', 'talk_hunger', 'talk_fulness', 'talk_story', 'problem', 'straw', 'distraction', 'affect_mealend', 'story_engagement', 'story_read', 'endmeal_prompt', 'affect_mealstart')
    } else if (file_pattern == 'PS3'){
      names(micro_dat)[c(4, 13:19, 22:26)] <- c('observation.name', 'utensil','talk_hunger', 'talk_fulness', 'talk_story', 'problem', 'distraction', 'straw', 'affect_mealend', 'story_engagement', 'story_read', 'endmeal_prompt', 'affect_mealstart')
    } else if (file_pattern == 'PS4'){
      names(micro_dat)[c(4, 13:18, 21:25)] <- c('observation.name', 'utensil','talk_fulness', 'talk_story', 'problem', 'distraction', 'straw', 'affect_mealend', 'story_engagement', 'story_read', 'endmeal_prompt', 'affect_mealstart')
      micro_dat$talk_hunger <- NA
    }

    ## remove duplicates/old
    micro_dat <- micro_dat[is.na(micro_dat[['coder']]) | micro_dat[['coder']] <= 2, ]

    ## remove unneeded codes
    micro_dat <- micro_dat[micro_dat[['behavior']] != 'Child Transcription', ]
    micro_dat <- micro_dat[micro_dat[['behavior']] != 'Researcher Transcription', ]
    micro_dat <- micro_dat[micro_dat[['behavior']] != 'Researcher Prompts Meal End', ]


    ## concatenate foods
    if (file_pattern == 'PS2'){
      micro_dat[['food']]<-paste0(micro_dat[['modifier_1']], micro_dat[['modifier_2']], micro_dat[['modifier_3']], micro_dat[['modifier_4']], micro_dat[['modifier_5']], micro_dat[['modifier_6']], micro_dat[['modifier_7']])
    } else {
      micro_dat[['food']]<-paste0(micro_dat[['modifier_1']], micro_dat[['modifier_2']], micro_dat[['modifier_3']], micro_dat[['modifier_4']], micro_dat[['modifier_5']], micro_dat[['modifier_6']])
    }

    ## fix food strings
    micro_dat[['food']] <- ifelse(micro_dat[['food']] == 'Chicken NuggetKetchup', 'Chicken Nugget, Ketchup',ifelse(micro_dat[['food']] == 'KetchupBroccoli', 'Broccoli, Ketchup',ifelse(micro_dat[['food']] == 'KetchupGrapes', 'Grapes, Ketchup', ifelse(micro_dat[['food']] == 'KetchupMac and Cheese', 'Mac and Cheese, Ketchup', ifelse(micro_dat[['food']] == 'Chicken NuggetKetchupMac and Cheese', 'Chicken Nugget, Mac and Cheese, Ketchup', ifelse(micro_dat[['food']] == 'KetchupGrapesMac and Cheese', 'Mac and Cheese, Grapes, Ketchup', ifelse(micro_dat[['food']] == 'Chicken NuggetMac and Cheese', 'Chicken Nugget, Mac and Cheese', ifelse(micro_dat[['food']] == 'Chicken NuggetKetchupGrapes', 'Chicken Nugget, Grapes, Ketchup', ifelse(micro_dat[['food']] == 'BroccoliMac and Cheese', 'Mac and Cheese, Broccoli', ifelse(micro_dat[['food']] == 'GrapesBroccoli', 'Grapes, Broccoli', ifelse(micro_dat[['food']] == 'Chicken NuggetChicken Nugget Breading Only', 'Chicken Nugget, Chicken Nugget Breading Only', ifelse(micro_dat[['food']] == 'Chicken NuggetChicken Nugget Breading OnlyKetchup', 'Chicken Nugget, Chicken Nugget Breading Only, Ketchup', ifelse(micro_dat[['food']] == 'Chicken Nugget Breading OnlyKetchup', 'Chicken Nugget Breading Only, Ketchup', ifelse(micro_dat[['food']] == 'Chicken Nugget Meat OnlyKetchup', 'Chicken Nugget Meat Only, Ketchup', as.character(micro_dat[['food']])))))))))))))))


    # select columns
    micro_dat_clean <- micro_dat[c('id', 'visit', 'coder', 'coder_phase1', 'coder_phase2', 'behavior', 'food', 'time_relative_hms', 'time_relative_sf', 'duration_sf', 'talk_hunger', 'talk_fulness', 'talk_story', 'straw', 'distraction', 'affect_mealstart', 'affect_mealend', 'story_read', 'story_engagement', 'endmeal_prompt')]

    # add levels
    micro_dat_clean[["affect_mealstart"]] <- as.numeric(ifelse(micro_dat_clean[["affect_mealstart"]] == 'n/a', '99', micro_dat_clean[["affect_mealstart"]]))

    micro_dat_clean[["affect_mealstart"]] <- sjlabelled::add_labels(micro_dat_clean[["affect_mealstart"]], labels = c(`Laughter, clapping, moving hands to show excitement, open mouth smiles, explicit and direct verbal excitement and approval` = 2, `Slight smiles, shaking head yes, verbal suggestion of approval` = 1, `Shows no emotion or reaction to meal start/end. Completely neutral` = 0, `Frowning, shaking head, looking confused/disgusted, any verbal suggestion of dislike or disapproval` = -1, `Vigorously shaking head or waving hands to reject food, explicit and direct negative verbal remark` = -2, `Meal starts or ends outside of video, so affect cannot be ranked` = 99))

    micro_dat_clean[["affect_mealend"]] <- as.numeric(ifelse(micro_dat_clean[["affect_mealend"]] == 'n/a', '99', micro_dat_clean[["affect_mealend"]]))

    micro_dat_clean[["affect_mealend"]] <- sjlabelled::add_labels(micro_dat_clean[["affect_mealend"]], labels = c(`Laughter, clapping, moving hands to show excitement, open mouth smiles, explicit and direct verbal excitement and approval` = 2, `Slight smiles, shaking head yes, verbal suggestion of approval` = 1, `Shows no emotion or reaction to meal start/end. Completely neutral` = 0, `Frowning, shaking head, looking confused/disgusted, any verbal suggestion of dislike or disapproval` = -1, `Vigorously shaking head or waving hands to reject food, explicit and direct negative verbal remark` = -2, `Meal starts or ends outside of video, so affect cannot be ranked` = 99))

    micro_dat_clean[["story_read"]] <- as.numeric(ifelse(micro_dat_clean[["story_read"]] == 'n/a' | micro_dat_clean[["story_read"]] == '', '99', ifelse(micro_dat_clean[["story_read"]] == 'researcher', 0, ifelse(micro_dat_clean[["story_read"]] == 'audible', 1, ifelse(micro_dat_clean[["story_read"]] == 'parent', 2, 99)))))

    micro_dat_clean[["story_read"]] <- sjlabelled::add_labels(micro_dat_clean[["story_read"]], labels = c(`parent` = 2, `audible` = 1, `researcher` = 0, `NA or not entered` = 99))

    micro_dat_clean[["story_engagement"]] <- as.numeric(ifelse(micro_dat_clean[["story_engagement"]] == 'n/a' | micro_dat_clean[["story_engagement"]] == '', '99', ifelse(micro_dat_clean[["story_engagement"]] == 'none', 0, ifelse(micro_dat_clean[["story_engagement"]] == 'during active eating', 1, ifelse(micro_dat_clean[["story_engagement"]] == 'not during active eating', 2, 99)))))

    micro_dat_clean[["story_engagement"]] <- sjlabelled::add_labels(micro_dat_clean[["story_engagement"]], labels = c(`not during active eating` = 2, `during active eating` = 1, `none` = 0, `NA or not entered` = 99))

    micro_dat_clean[["endmeal_prompt"]] <- as.numeric(ifelse(micro_dat_clean[["endmeal_prompt"]] == 'n/a' | micro_dat_clean[["endmeal_prompt"]] == '', '99', ifelse(micro_dat_clean[["endmeal_prompt"]] == 'no', 0, ifelse(micro_dat_clean[["endmeal_prompt"]] == 'yes', 1, 99))))

    micro_dat_clean[["endmeal_prompt"]] <- sjlabelled::add_labels(micro_dat_clean[["endmeal_prompt"]], labels = c(`yes` = 1, `no` = 0, `NA or not entered` = 99))

    #add variable labels
    micro_clean_labels <- lapply(micro_dat_clean, function(x) attributes(x)$label)
    micro_clean_labels['id'] <- 'participant id'
    micro_clean_labels['visit'] <- 'visit number'
    micro_clean_labels['coder'] <- 'coder order - first or second'
    micro_clean_labels['coder_phase1'] <- 'initials of phase 1 coder'
    micro_clean_labels['coder_phase2'] <- 'initials of phase 2 coder'
    micro_clean_labels['behavior'] <- 'behavior coded'
    micro_clean_labels['food'] <- 'food item(s)'
    micro_clean_labels['time_relative_hms'] <- 'time relative to start in hours:minums:seconds'
    micro_clean_labels['time_relative_sf'] <- 'time relative to start seconds'
    micro_clean_labels['duration_sf'] <- 'duration in seconds'
    micro_clean_labels['talk_hunger'] <- 'vocalization about hunger'
    micro_clean_labels['talk_fulness'] <- 'vocalization about fullness'
    micro_clean_labels['talk_story'] <- 'vocalization about story'
    micro_clean_labels['straw'] <- 'difficulties with straw'
    micro_clean_labels['distraction'] <- 'distraction behaviors'
    micro_clean_labels['affect_mealstart'] <- 'affect at meal start'
    micro_clean_labels['affect_mealend'] <- 'affect at meal end'
    micro_clean_labels['story_read'] <- 'mode in which story was read'
    micro_clean_labels['story_engagement'] <- 'engagement with story'
    micro_clean_labels['endmeal_prompt'] <- 'researcher prompted end of meal'

    micro_dat_clean = sjlabelled::set_label(micro_dat_clean, label = matrix(unlist(micro_clean_labels, use.names = FALSE)))

    # 4. make wide ####

    ## affect
    #get wide
    micro_dat_affectstart_wide <- reshape2::dcast(micro_dat_clean[c(1:3, 16)], id + visit ~ coder, value.var = c("affect_mealstart"), mean)
    names(micro_dat_affectstart_wide)[3:4] <- c('affect_mealstart_c1', 'affect_mealstart_c2')

    micro_dat_affectend_wide <- reshape2::dcast(micro_dat_clean[c(1:3, 17)], id + visit ~ coder, value.var = c('affect_mealend'), mean)
    names(micro_dat_affectend_wide)[3:4] <- c('affect_mealend_c1', 'affect_mealend_c2')

    micro_dat_beh_wide_count <- reshape2::dcast(micro_dat_clean[micro_dat_clean[['behavior']] == 'Bite' | micro_dat_clean[['behavior']] == 'Sips', c(1:3, 6, 9)], id + visit ~ coder + behavior, value.var = c('time_relative_sf'), length)
    names(micro_dat_beh_wide_count)[c(3:6)] <- c('nbites_c1', 'nsips_c1', 'nbites_c2', 'nsips_c2')

    micro_dat_beh_wide_dur <- reshape2::dcast(micro_dat_clean[micro_dat_clean[['behavior']] == 'Meal Duration' | micro_dat_clean[['behavior']] == 'Latency to First Bite' | micro_dat_clean[['behavior']] == 'Active Eating Time' | micro_dat_clean[['behavior']] == 'Leaving Chair', c(1:3, 6, 10)], id + visit ~ coder + behavior, value.var = c('duration_sf'), sum)
    names(micro_dat_beh_wide_dur)[c(3:10)] <- c('total_active_eating_c1', 'bite_latency_c1', 'total_leave_chair_c1', 'meal_duration_c1', 'total_active_eating_c2', 'bite_latency_c2', 'total_leave_chair_c2', 'meal_duration_c2')

    #convert to minutes
    micro_dat_beh_wide_dur[c(3:10)] <- micro_dat_beh_wide_dur[c(3:10)]/60

    #merge
    micro_dat_wide <- merge(micro_dat_affectstart_wide, micro_dat_affectend_wide[c(1, 3:4)], by = 'id')
    micro_dat_wide <- merge(micro_dat_wide, micro_dat_beh_wide_count[c(1, 3:6)], by = 'id')
    micro_dat_wide <- merge(micro_dat_wide, micro_dat_beh_wide_dur[c(1, 3:10)], by = 'id')

    #generate other data by coder
    micro_dat_wide[['bite_rate_c1']] <- micro_dat_wide[['nbites_c1']]/micro_dat_wide[['meal_duration_c1']]
    micro_dat_wide[['bite_rate_c2']] <- micro_dat_wide[['nbites_c2']]/micro_dat_wide[['meal_duration_c2']]

    micro_dat_wide[['bite_rate_active_c1']] <- micro_dat_wide[['nbites_c1']]/micro_dat_wide[['total_active_eating_c1']]
    micro_dat_wide[['bite_rate_active_c2']] <- micro_dat_wide[['nbites_c2']]/micro_dat_wide[['total_active_eating_c2']]

    micro_dat_wide[['sip_rate_c1']] <- micro_dat_wide[['nsips_c1']]/micro_dat_wide[['meal_duration_c1']]
    micro_dat_wide[['sip_rate_c2']] <- micro_dat_wide[['nsips_c2']]/micro_dat_wide[['meal_duration_c2']]

    micro_dat_wide[['sip_rate_active_c1']] <- micro_dat_wide[['nsips_c1']]/micro_dat_wide[['total_active_eating_c1']]
    micro_dat_wide[['sip_rate_active_c2']] <- micro_dat_wide[['nsips_c2']]/micro_dat_wide[['total_active_eating_c2']]

    #clean up order
    micro_dat_wide <- micro_dat_wide[c(1:3, 5, 7:8, 11:14, 19, 21, 23, 25, 4, 6, 9:10, 15:18, 20, 22, 24, 26)]


    ## 5 - Event Data by Coder ####
    micro_dat_clean_event <- micro_dat_clean[micro_dat_clean[['behavior']] == 'Bite' | micro_dat_clean[['behavior']] == 'Sips', 1:16]

    # get coder datasets
    micro_dat_event_c1 <- micro_dat_clean_event[micro_dat_clean_event[['coder']] == 1, ]
    micro_dat_event_c2 <- micro_dat_clean_event[micro_dat_clean_event[['coder']] == 2, ]

    #re_name
    names(micro_dat_event_c1)[6:16] <- sapply(names(micro_dat_event_c1)[6:16], function(x) paste0(x, '_c1'))
    names(micro_dat_event_c2)[6:16] <- sapply(names(micro_dat_event_c2)[6:16], function(x) paste0(x, '_c2'))

    names(micro_dat_event_c1)[4:5] <- c('coder1_phase1', 'coder1_phase2')
    names(micro_dat_event_c2)[4:5] <- c('coder2_phase1', 'coder2_phase2')

    #get event number by id
    micro_dat_event_c1[['event_num']] <- unlist(sapply(unique(micro_dat_event_c1$id), function(x) seq(1, nrow(micro_dat_event_c1[micro_dat_event_c1[['id']] == x, ]), 1), USE.NAMES = FALSE))

    micro_dat_event_c2[['event_num']] <- unlist(sapply(unique(micro_dat_event_c2$id), function(x) seq(1, nrow(micro_dat_event_c2[micro_dat_event_c2[['id']] == x, ]), 1), USE.NAMES = FALSE))

    #code switches
    switch_fn <- function(food, event){
      if (event == 1 | is.na(food[[event]])) {
        switch <- NA
      } else if (food[[event]] != food[[event - 1]]){
        switch <- 1
      } else {
        switch <- 0
      }
    }

    switch_wrapper_food <- function(id, coder_num, dat){
      id_dat <- dat[dat[['id']] == id, ]

      if (coder_num == 1){
        food_list <- id_dat[['food_c1']]
      } else {
        food_list <- id_dat[['food_c2']]
      }

      switch <- sapply(id_dat[['event_num']], function(x) switch_fn(food_list, x))
    }

    micro_dat_event_c1[['switch_c1']] <- unlist(sapply(unique(micro_dat_event_c1$id), function(x) switch_wrapper_food(x, 1, micro_dat_event_c1), USE.NAMES = FALSE))

    micro_dat_event_c1[["switch_c1"]] <- sjlabelled::add_labels(micro_dat_event_c1[["switch_c1"]], labels = c(`yes` = 1, `no` = 0, `NA or not entered` = 99))

    micro_dat_event_c2[['switch_c2']] <- unlist(sapply(unique(micro_dat_event_c2$id), function(x) switch_wrapper_food(x, 2, micro_dat_event_c2), USE.NAMES = FALSE))

    micro_dat_event_c2[["switch_c2"]] <- sjlabelled::add_labels(micro_dat_event_c2[["switch_c2"]], labels = c(`yes` = 1, `no` = 0, `NA or not entered` = 99))

    # add switch by ED
    micro_dat_event_c1[["food_ed_c1"]] <- ifelse(grepl('Mac', micro_dat_event_c1[["food_c1"]]) | grepl('Chicken', micro_dat_event_c1[["food_c1"]]), ifelse(grepl('Grape', micro_dat_event_c1[["food_c1"]]) | grepl('Broccoli', micro_dat_event_c1[["food_c1"]]), 'mixed', 'h_ed'), ifelse(grepl('Grape', micro_dat_event_c1[["food_c1"]]) | grepl('Broccoli', micro_dat_event_c1[["food_c1"]]), 'l_ed', as.character(micro_dat_event_c1[["food_c1"]])))

    micro_dat_event_c1[["food_ed_c1"]] <- sjlabelled::add_labels(micro_dat_event_c1[["food_ed_c1"]], labels = c(`Ketchup` = 3, `mixed` = 2, `h_ed` = 1, `l_ed` = 0, `NA or not entered` = 99))

    micro_dat_event_c2[["food_ed_c2"]] <- ifelse(grepl('Mac', micro_dat_event_c2[["food_c2"]]) | grepl('Chicken', micro_dat_event_c2[["food_c2"]]), ifelse(grepl('Grape', micro_dat_event_c2[["food_c2"]]) | grepl('Broccoli', micro_dat_event_c2[["food_c2"]]), 'mixed', 'h_ed'), ifelse(grepl('Grape', micro_dat_event_c2[["food_c2"]]) | grepl('Broccoli', micro_dat_event_c2[["food_c2"]]), 'l_ed', as.character(micro_dat_event_c2[["food_c2"]])))

    micro_dat_event_c2[["food_ed_c2"]] <- sjlabelled::add_labels(micro_dat_event_c2[["food_ed_c2"]], labels = c(`Ketchup` = 3, `mixed` = 2, `h_ed` = 1, `l_ed` = 0, `NA or not entered` = 99))

    switch_wrapper_ed <- function(id, coder_num, dat){
      id_dat <- dat[dat[['id']] == id, ]

      if (coder_num == 1){
        food_list <- id_dat[['food_ed_c1']]
      } else {
        food_list <- id_dat[['food_ed_c2']]
      }

      switch <- sapply(id_dat[['event_num']], function(x) switch_fn(food_list, x))
    }

    micro_dat_event_c1[['switch_ed_c1']] <- unlist(sapply(unique(micro_dat_event_c1$id), function(x) switch_wrapper_ed(x, 1, micro_dat_event_c1), USE.NAMES = FALSE))

    micro_dat_event_c1[["switch_ed_c1"]] <- sjlabelled::add_labels(micro_dat_event_c1[["switch_ed_c1"]], labels = c(`yes` = 1, `no` = 0, `NA or not entered` = 99))

    micro_dat_event_c2[['switch_ed_c2']] <- unlist(sapply(unique(micro_dat_event_c2$id), function(x) switch_wrapper_ed(x, 2, micro_dat_event_c2), USE.NAMES = FALSE))

    micro_dat_event_c2[["switch_ed_c2"]] <- sjlabelled::add_labels(micro_dat_event_c2[["switch_ed_c2"]], labels = c(`yes` = 1, `no` = 0, `NA or not entered` = 99))

    #merge
    micro_dat_event_wide <- merge(micro_dat_event_c1[c(1:2, 4:20)], micro_dat_event_c2[c(1, 4:20)], by = c('id', 'event_num'), all = TRUE)

    #organize
    micro_dat_event_wide <- micro_dat_event_wide[c(1, 3:5, 20:21, 2, 6:9, 17:19, 22:25, 33:35, 11:16, 27:32)]

    ## 6. Add switches to behavior summary output ####
    micro_dat_wide[c('nswitch_c1', 'nswitch_ed_c1', 'nswitch_c2', 'nswitch_ed_c2')]  <- t(sapply(unique(micro_dat_event_wide[['id']]), function(x) colSums(micro_dat_event_wide[micro_dat_event_wide[['id']] == x, c('switch_c1', 'switch_ed_c1', 'switch_c2', 'switch_ed_c2')], na.rm = TRUE), USE.NAMES = FALSE))

    micro_dat_wide <- micro_dat_wide[c(1:14, 27:28, 15:26, 29:30)]

    ## 7. add variable labels ####
    micro_wide_labels <- lapply(micro_dat_wide, function(x) attributes(x)$label)
    micro_wide_labels['id'] <- 'participant id'
    micro_wide_labels['visit'] <- 'visit number'
    micro_wide_labels['affect_mealstart_c1'] <- 'coder 1 - start of meal affect'
    micro_wide_labels['affect_mealend_c1'] <- 'coder 1 - end of meal affect'
    micro_wide_labels['nbites_c1'] <- 'coder 1 - number of bites'
    micro_wide_labels['nsips_c1'] <- 'coder 1 - number of sips'
    micro_wide_labels['total_active_eating_c1'] <- 'coder 1 - total active eating time in seconds'
    micro_wide_labels['bite_latency_c1'] <- 'coder 1 - latency to first bite in seconds'
    micro_wide_labels['total_leave_chair_c1'] <- 'coder 1 - total time out of chair in seconds'
    micro_wide_labels['meal_duration_c1'] <- 'coder 1 - meal duration in seconds'
    micro_wide_labels['bite_rate_c1'] <- 'coder 1 - bites per second of meal duration'
    micro_wide_labels['bite_rate_active_c1'] <- 'coder 1 - bites per second of active eating time'
    micro_wide_labels['sip_rate_c1'] <- 'coder 1 - sips per second of meal duration'
    micro_wide_labels['sip_rate_active_c1'] <- 'coder 1 - sips per second of active eating time'
    micro_wide_labels['nswitch_c1'] <- 'coder 1 - number of switches between foods and/or drinks'
    micro_wide_labels['nswitch_ed_c1'] <- 'coder 1 - number of switches between food energy density categories and/or drinks, Ketchup (left separate if consumed alone)'
    micro_wide_labels['affect_mealstart_c2'] <- 'coder 2 - start of meal affect'
    micro_wide_labels['affect_mealend_c2'] <- 'coder 2 - end of meal affect'
    micro_wide_labels['nbites_c2'] <- 'coder 2 - number of bites'
    micro_wide_labels['nsips_c2'] <- 'coder 2 - number of sips'
    micro_wide_labels['total_active_eating_c2'] <- 'coder 2 - total active eating time in seconds'
    micro_wide_labels['bite_latency_c2'] <- 'coder 2 - latency to first bite in seconds'
    micro_wide_labels['total_leave_chair_c2'] <- 'coder 2 - total time out of chair in seconds'
    micro_wide_labels['meal_duration_c2'] <- 'coder 2 - meal duration in seconds'
    micro_wide_labels['bite_rate_c2'] <- 'coder 2 - bites per second of meal duration'
    micro_wide_labels['bite_rate_active_c2'] <- 'coder 2 - bites per second of active eating time'
    micro_wide_labels['sip_rate_c2'] <- 'coder 2 - sips per second of meal duration'
    micro_wide_labels['sip_rate_active_c2'] <- 'coder 2 - sips per second of active eating time'
    micro_wide_labels['nswitch_c2'] <- 'coder 2 - number of switches between foods and/or drinks'
    micro_wide_labels['nswitch_ed_c2'] <- 'coder 2 - number of switches between food energy density categories and/or drinks, Ketchup (left separate if consumed alone)'

    micro_dat_wide = sjlabelled::set_label(micro_dat_wide, label = matrix(unlist(micro_wide_labels, use.names = FALSE)))

    #add variable labels
    micro_wide_event_labels <- lapply(micro_dat_event_wide, function(x) attributes(x)$label)
    micro_wide_event_labels['id'] <- 'participant id'
    micro_wide_event_labels['visit'] <- 'visit number'
    micro_wide_event_labels['coder1_phase1'] <- 'coder 1 - phase 1'
    micro_wide_event_labels['coder1_phase2'] <- 'coder 1 - phase 2'
    micro_wide_event_labels['coder2_phase1'] <- 'coder 2 - phase 1'
    micro_wide_event_labels['coder2_phase2'] <- 'coder 2 - phase 2'
    micro_wide_event_labels['event_num'] <- 'event number coded in sequence'
    micro_wide_event_labels['behavior_c1'] <- 'coder 1 - event behavior coded'
    micro_wide_event_labels['food_c1'] <- 'coder 1 - food  item'
    micro_wide_event_labels['time_relative_hms_c1'] <- 'coder 1 - time relative to start in hours:minums:seconds'
    micro_wide_event_labels['time_relative_sf_c1'] <- 'coder 1 - time relative to start in seconds'
    micro_wide_event_labels['switch_c1'] <- 'coder 1 - food/drink switch'
    micro_wide_event_labels['food_ed_c1'] <- 'coder 1 - food energy density category'
    micro_wide_event_labels['switch_ed_c1'] <- 'coder 1 - food switch by energy density'
    micro_wide_event_labels['behavior_c2'] <- 'coder 2 - event behavior coded'
    micro_wide_event_labels['food_c2'] <- 'coder 2 - food  item'
    micro_wide_event_labels['time_relative_hms_c2'] <- 'coder 2 - time relative to start in hours:minums:seconds'
    micro_wide_event_labels['time_relative_sf_c2'] <- 'coder 2 - time relative to start in seconds'
    micro_wide_event_labels['switch_c2'] <- 'coder 2 - food/drink switch'
    micro_wide_event_labels['food_ed_c2'] <- 'coder 2 - food energy density category'
    micro_wide_event_labels['switch_ed_c2'] <- 'coder 2 - food switch by energy density'
    micro_wide_event_labels['talk_hunger_c1'] <- 'coder 1 - vocalization about hunger'
    micro_wide_event_labels['talk_fulness_c1'] <- 'coder 1 - vocalization about fullness'
    micro_wide_event_labels['talk_story_c1'] <- 'coder 1 - vocalization about story'
    micro_wide_event_labels['problem_c1'] <- 'coder 1 - other vocalization?'
    micro_wide_event_labels['straw_c1'] <- 'coder 1 - difficulties with straw'
    micro_wide_event_labels['distraction_c1'] <- 'coder 1 - distraction behaviors'
    micro_wide_event_labels['talk_hunger_c2'] <- 'coder 2 - vocalization about hunger'
    micro_wide_event_labels['talk_fulness_c2'] <- 'coder 2 - vocalization about fullness'
    micro_wide_event_labels['talk_story_c2'] <- 'coder 2 - vocalization about story'
    micro_wide_event_labels['problem_c2'] <- 'coder 2 - other vocalization?'
    micro_wide_event_labels['straw_c2'] <- 'coder 2 - difficulties with straw'
    micro_wide_event_labels['distraction_c2'] <- 'coder 2 - distraction behaviors'

    micro_dat_event_wide = sjlabelled::set_label(micro_dat_event_wide, label = matrix(unlist(micro_wide_event_labels, use.names = FALSE)))

  }

  ## make list of data frame and associated labels
  meal_micro <- list(beh_wide = list(data = micro_dat_wide, dict = micro_wide_labels), event = list(data=micro_dat_event_wide, dict=micro_wide_event_labels))

  ## want an export options??

  return(meal_micro)
}
