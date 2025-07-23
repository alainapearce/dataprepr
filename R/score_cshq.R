#' score_cshq: Scored data from the Children Sleep Habits Questionnaire
#'
#' This function scores the Children Sleep Habits Questionnaire and provides both total scores and subscale scores for the following behaviors: Morning Wake Up, Bedtime Resistance, Sleep Onset Delay, Sleep Duration, Sleep Anxiety, Night Wakings, Parasomnias, Sleep Disordered Breathing, and Daytime Sleepiness
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'cshq#' or 'cshq_#' where # is the question number (1-33)
#' 3a) If base_zero = FALSE, all questions must have the numeric values 1-3: (3 - Usually, 2 - Sometimes, 1 - Rarely or 1 - not sleepy, 2 - very sleepy, 3 - falls asleep) Script will reverse-score questions 1-6 will be reversed scored in script and adjust 7-8.
#' 3b) If base_zero = TRUE, all questions must have the numeric values 0-2: (2 - Usually, 1 - Sometimes, 0 - Rarely or 0 - not sleepy, 1 - very sleepy, 2 - falls asleep). Script will reverse-score questions 1-6 will be reversed scored in script and adjust 7-8.
#' 4) This script will apply reverse scoring so all levels must be true to the scale described above
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#'
#' Full Measure:
#' Owens, J. A., Spirito, A., & McGuinn, M. (2000). The Children’s Sleep Habits Questionnaire (CSHQ): Psychometric Properties of A Survey Instrument for School-Aged Children. SLEEP, 23(8), 1043–1052. (\href{https://pubmed.ncbi.nlm.nih.gov/11145319/}{PubMed})
#'
#' @param cshq_data a data.frame all items for the Children Sleep Habits Questionnaire following the naming conventions described above
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @param reverse_score is data already reversed scored (default = FALSE)
#'
#'
#' @return A dataset with total and subscale scores for the Children Sleep Habits Questionnaire
#'
#' @examples
#'
#' cshq_score_data <- score_cshq(cshq_data, id = 'ID')
#'
#' \dontrun{
#' }
#'

#'
#'
#' @export

score_cshq <- function(cshq_data, pna_value, base_zero = TRUE, id, session_id, reverse_score = FALSE) {
  
  #### 1. Set up/initial checks #####
  
  # check that cshq_data exist and is a data.frame
  data_arg <- methods::hasArg(cshq_data)
  
  if (isTRUE(data_arg) & !is.data.frame(cshq_data)) {
    stop("cshq_data must be entered as a data.frame")
  } else if (isFALSE(data_arg)) {
    stop("cshq_data must set to the data.frame with amount consumed for each food item")
  }
  
  # check if id exists
  ID_arg <- methods::hasArg(id)
  
  if (isTRUE(ID_arg)){
    if (!(id %in% names(cshq_data))) {
      stop("variable name entered as id is not in cshq_data")
    }
  }
  
  # check if session_id exists
  sessionID_arg <- methods::hasArg(session_id)
  
  if (isTRUE(sessionID_arg)){
    if (!(id %in% names(cshq_data))) {
      stop("variable name entered as session_id is not in cshq_data")
    }
  }
  
  # check base_zero is logical
  if (!is.logical(base_zero)) {
    stop("base_zero arg must be logical (TRUE/FALSE)")
  }
  
  #### 2. Set Up Data #####
  
  # set up database for results
  
  ## create empty matrix
  cshq_score_dat <- data.frame(cshq_bedtime_resit = rep(NA, nrow(cshq_data)), cshq_sleep_delay = rep(NA, nrow(cshq_data)), cshq_sleepdur = rep(NA, nrow(cshq_data)), cshq_anxiety = rep(NA, nrow(cshq_data)), cshq_nightwake = rep(NA, nrow(cshq_data)), cshq_parasomnias = rep(NA, nrow(cshq_data)), cshq_dis_breathing = rep(NA, nrow(cshq_data)), cshq_daysleepy = rep(NA, nrow(cshq_data)), cshq_total = rep(NA, nrow(cshq_data)))
  
  if (isTRUE(ID_arg)) {
    if (isTRUE(sessionID_arg)) {
      cshq_score_dat <- data.frame(cshq_data[[id]], cshq_data[[session_id]], cshq_score_dat)
      names(cshq_score_dat)[1:2] <- c(id, session_id)
    } else {
      cshq_score_dat <- data.frame(cshq_data[[id]], cshq_score_dat)
      names(cshq_score_dat)[1] <- id
    }
  }
  
  
  # remove underscore if in column names
  names(cshq_data) <- ifelse(grepl('id|notes|time|hour|min', names(cshq_data)), names(cshq_data), gsub('cshq_', 'cshq', names(cshq_data)))
  
  # get primary (scored) questions
  q_numbers <- seq(1, 33)
  cshq_primary_qs <- paste0("cshq", q_numbers)

  # re-scale scored questions
  cshq_data_edit <- cshq_data
  
  if (isTRUE(base_zero)){
    cshq_data_edit[cshq_primary_qs] <- sapply(cshq_primary_qs, function(x) cshq_data_edit[[x]] + 1, simplify = TRUE)
  }
  
  # if pna_value arg, replace not applicable values with NA
  if (isTRUE(methods::hasArg(pna_value))) {
    
    # replace pna_value with NA in pcw_vars
    cshq_data_edit[cshq_primary_qs] <- lapply(cshq_data_edit[cshq_primary_qs] , function(x) ifelse(x == pna_value, NA, x))
    
  }
  
  # calculate reversed scores
  if (isFALSE(reverse_score)){
    
    reverse_qs_set <- c('cshq1', 'cshq2', 'cshq3', 'cshq4', 'cshq5', 'cshq6')
    for (var in 1:length(reverse_qs_set)){
      var_name <- reverse_qs_set[var]

      cshq_data_edit[[var_name]] <- ifelse(cshq_data_edit[[var_name]] == 1, 3, ifelse(cshq_data_edit[[var_name]] == 2, 2, ifelse(cshq_data_edit[[var_name]] == 3, 1, NA)))
      
    }
  }
  
  #adjust question with zero base (7 and 8)
  if (min(cshq_data_edit[c('cshq7', 'cshq8')], na.rm = TRUE) == 1){
    cshq_data_edit[c('cshq7', 'cshq8')] <- sapply(c('cshq7', 'cshq8'), function(x) cshq_data_edit[[x]] - 1, simplify = TRUE)
  }
  
  # Bedtime Resistance
  bedtime_vars <- c('cshq1', 'cshq2', 'cshq9', 'cshq10', 'cshq11', 'cshq12')
  cshq_score_dat[['cshq_bedtime_resit']] <- rowSums(cshq_data_edit[bedtime_vars])
  
  # Sleep Onset Delay
  cshq_score_dat[['cshq_sleep_delay']] <- as.numeric(cshq_data_edit[['cshq3']])
  
  # Sleep Duration
  sleep_dur_vars <- c('cshq4', 'cshq5', 'cshq13')
  cshq_score_dat[['cshq_sleepdur']] <- rowSums(cshq_data_edit[sleep_dur_vars])
  
  # Sleep Anxiety
  sleepanx_vars <- c('cshq11', 'cshq12', 'cshq14', 'cshq15')
  cshq_score_dat[['cshq_anxiety']] <- rowSums(cshq_data_edit[sleepanx_vars])
  
  # Night Wakings
  nightwake_vars <- c('cshq16',  'cshq17', 'cshq18')
  cshq_score_dat[['cshq_nightwake']] <- rowSums(cshq_data_edit[nightwake_vars])
  
  # Parasomnias
  parasomnias_vars <- c('cshq19', 'cshq20', 'cshq21', 'cshq22', 'cshq23', 'cshq24', 'cshq25')
  cshq_score_dat[['cshq_parasomnias']] <- rowSums(cshq_data_edit[parasomnias_vars])
  
  # Sleep Disordered Breathing
  dis_breathing_vars <- c('cshq26', 'cshq27', 'cshq28')
  cshq_score_dat[['cshq_dis_breathing']] <- rowSums(cshq_data_edit[dis_breathing_vars])
  
  # Daytime Sleepiness
  daysleepy_vars <- c('cshq6', 'cshq7', 'cshq8', 'cshq29', 'cshq30', 'cshq31', 'cshq32', 'cshq33')
  cshq_score_dat[['cshq_daysleepy']] <- rowSums(cshq_data_edit[daysleepy_vars])
  
  # Total Score
  cshq_score_dat[['cshq_total']] <- rowSums(cshq_data_edit[cshq_primary_qs])
  
  #### 3. Clean Export/Scored Data #####
  ## merge raw responses with scored data
  if (isTRUE(ID_arg)){
    if (isTRUE(sessionID_arg)) {
      cshq_phenotype <- merge(cshq_data, cshq_score_dat, by = c(id, session_id))
    } else {
      cshq_phenotype <- merge(cshq_data, cshq_score_dat, by = id)
    }
    
    return(list(score_dat = as.data.frame(cshq_score_dat),
                bids_phenotype = as.data.frame(cshq_phenotype)))
  } else {
    return(list(score_dat = as.data.frame(cshq_score_dat)))
  }
  
}

