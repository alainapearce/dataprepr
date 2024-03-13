#' score_cshq: Scored data from the Children Sleep Habits Questionnaire
#'
#' This function scores the Children Sleep Habits Questionnaire and provides both total scores and subscale scores for the following behaviors: Morning Wake Up, Bedtime Resistance, Sleep Onset Delay, Sleep Duration, Sleep Anxiety, Night Wakings, Parasomnias, Sleep Disordered Breathing, and Daytime Sleepiness
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'cshq#' or 'cshq_#' where # is the question number (1-33)
#' 3) All questions must have the numeric value for the choice: 3 - Usually, 2 - Sometimes, 1 - Rarely
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

score_cshq <- function(cshq_data, score_base = TRUE, id, reverse_score = FALSE) {
  
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
  
  #### 2. Set Up Data #####
  
  # set up database for results
  
  ## create empty matrix
  cshq_score_dat <- data.frame(cshq_bedtime_resit = rep(NA, nrow(cshq_data)), cshq_sleep_delay = rep(NA, nrow(cshq_data)), cshq_sleepdur = rep(NA, nrow(cshq_data)), cshq_anxiety = rep(NA, nrow(cshq_data)), cshq_nightwake = rep(NA, nrow(cshq_data)), cshq_parasomnias = rep(NA, nrow(cshq_data)), cshq_dis_breathing = rep(NA, nrow(cshq_data)), cshq_daysleepy = rep(NA, nrow(cshq_data)), cshq_total = rep(NA, nrow(cshq_data)))
  
  if (isTRUE(ID_arg)) {
    cshq_score_dat <- data.frame(cshq_data[[id]], cshq_score_dat)
    names(cshq_score_dat)[1] <- id
  }
  
  
  # remove underscore if in column names
  names(cshq_data) <- ifelse(grepl('id|notes|time|hour|min', names(cshq_data)), names(cshq_data), gsub('cshq_', 'cshq', names(cshq_data)))
  
  # get primary (scored) questions
  q_numbers <- seq(1, 33)
  cshq_primary_qs <- paste0("cshq", q_numbers)

  # re-scale scored questions
  cshq_data_edit <- cshq_data
  
  if (isTRUE(score_base)){
    cshq_data_edit[cshq_primary_qs] <- sapply(cshq_primary_qs, function(x) cshq_data[[x]] + 1, simplify = TRUE)
  }
  
  #### 2. Set Up Data #####
  
  # set up database for results
  ## create empty matrix
 
  # calculate reversed scores
  if (isFALSE(reverse_score)){
    
    # are these the correct items?
    reverse_qs_set <- c('cshq1', 'cshq2', 'cshq3', 'cshq4', 'cshq5', 'cshq6')
    for (var in 1:length(reverse_qs_set)){
      var_name <- reverse_qs_set[var]

      cshq_data_edit[[var_name]] <- ifelse(is.na(cshq_data_edit[[var_name]]), NA, ifelse(cshq_data_edit[[var_name]] == 1, 3, ifelse(cshq_data_edit[[var_name]] == 3, 1, 2)))
    }
  }
  
  #adjust question with zero base
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
    cshq_phenotype <- merge(cshq_data, cshq_score_dat, by = id)
    
    return(list(score_dat = as.data.frame(cshq_score_dat),
                bids_phenotype = as.data.frame(cshq_phenotype)))
  } else {
    return(list(score_dat = as.data.frame(cshq_score_dat)))
  }
  
}

