#' score_cfpq: Scored data from the Comprehensive Feeding Practices Questionnaire
#'
#' This function scores the Comprehensive Feeding Practices Questionnaire and provides subscale scores for: child control, emotional regulation, Encourage balance and variety, environment, food as reward, involvement, modeling, monitoring, pressure, restriction for health, restriction for weight control, and teaching about nutrition
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'cfpq#' where # is the question number (1-49)
#' 3) Questions 1-13 must have the numeric value for the choices: 1 - never, 2 - rarely, 3 - sometimes, 4 - mostly, 5 - always
#'    Questions 14-49 must have the numeric value for the choices: 1 - disagree, 2 - slightly disagree, 3 - neutral, 4 - slightly agree, 5 - agree.
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Primary Reference for the Questionnaire and Scoring:
#' Musher-Eizenman, D., Holub. S., Comprehensive Feeding Practices Questionnaire: Validation of a New Measure of Parental Feeding Practices, Journal of Pediatric Psychology, 32, 8, September 2007, Pages 960–972, https://doi.org/10.1093/jpepsy/jsm037
#'
#' @param cfpq_data a data.frame all items for the Comprehensive Feeding Practices Questionnaire following the naming conventions described above
#' @inheritParams score_bes
#'
#' @return A dataset with subscale scores for the Comprehensive Feeding Practices Questionnaire 
#' @examples
#'
#' # scoring for the cfpq with IDs
#' cfpq_score_data <- score_cfpq(cfpq_data, id = 'ID')
#'
#'
#' \dontrun{
#' }
#'
#' @export

score_cfpq <- function(cfpq_data, score_base = TRUE, id) {
  
  #### 1. Set up/initial checks #####
  
  # check that cfpq_data exist and is a data.frame
  data_arg <- methods::hasArg(cfpq_data)
  
  if (isTRUE(data_arg) & !is.data.frame(cfpq_data)) {
    stop("cfpq_data must be entered as a data.frame")
  } else if (isFALSE(data_arg)) {
    stop("cfpq_data must set to the data.frame with amount consumed for each food item")
  }
  
  # check if id exists
  ID_arg <- methods::hasArg(id)
  
  if (isTRUE(ID_arg)){
    if (!(id %in% names(cfpq_data))) {
      stop("variable name entered as id is not in cfpq_data")
    }
  }
  
  #### 2. Set Up Data #####
  
  # set up database for results create empty matrix
  cfpq_score_dat <- data.frame(cfpq_control = rep(NA, nrow(cfpq_data)), 
                               cfpq_emoreg = rep(NA, nrow(cfpq_data)), 
                               cfpq_balance = rep(NA, nrow(cfpq_data)),
                               cfpq_env = rep(NA, nrow(cfpq_data)),
                               cfpq_reward = rep(NA, nrow(cfpq_data)),
                               cfpq_involve = rep(NA, nrow(cfpq_data)),
                               cfpq_model = rep(NA, nrow(cfpq_data)),
                               cfpq_monitor = rep(NA, nrow(cfpq_data)),
                               cfpq_pressure = rep(NA, nrow(cfpq_data)),
                               cfpq_restrict_health = rep(NA, nrow(cfpq_data)),
                               cfpq_restrict_weight = rep(NA, nrow(cfpq_data)),
                               cfpq_teach = rep(NA, nrow(cfpq_data)))
  
  if (isTRUE(ID_arg)) {
    cfpq_score_dat <- data.frame(cfpq_data[[id]], cfpq_score_dat)
    names(cfpq_score_dat)[1] <- id
  }
  
  # remove underscore if in column names
  names(cfpq_data) <- gsub('cfpq_', 'cfpq', names(cfpq_data))
  
  # get primary questions to score
  q_numbers <- seq(1, 49)
  cfpq_primary_qs <- paste0("cfpq", q_numbers)
  
  # re-scale data
  cfpq_data_edit <- cfpq_data
  
  if (isTRUE(score_base)){
    cfpq_data_edit[cfpq_primary_qs] <- sapply(cfpq_primary_qs, function(x) cfpq_data[[x]] + 1, simplify = TRUE)
  }
  
  # calculate reversed scores
  reverse_qs <- c("cfpq16", "cfpq37", "cfpq42")
  
  for (var in 1:length(reverse_qs)) {
    var_name <- reverse_qs[var]
    reverse_name <- paste0(var_name, "_rev")
    
    cfpq_data_edit[[reverse_name]] <- ifelse(is.na(cfpq_data_edit[[var_name]]), NA, ifelse(cfpq_data_edit[[var_name]] == 1, 5, ifelse(cfpq_data_edit[[var_name]] == 2, 4, 3)))
  }
  
  ## Score Subscales
  
  # Child Control
  control_vars <- c("cfpq5", "cfpq6", "cfpq10", "cfpq11", "cfpq12")
  cfpq_score_dat[["cfpq_control"]] <- rowMeans(cfpq_data_edit[control_vars])
  
  # Emotion regulation
  emo_vars <- c("cfpq7", "cfpq8", "cfpq9")
  cfpq_score_dat[["cfpq_emoreg"]] <- rowMeans(cfpq_data_edit[emo_vars])
  
  # Encourage balance and variety
  balance_vars <- c("cfpq13", "cfpq24", "cfpq26", "cfpq38")
  cfpq_score_dat[["cfpq_balance"]] <- rowMeans(cfpq_data_edit[balance_vars])
  
  # Environment
  env_vars <- c("cfpq14", "cfpq16_rev", "cfpq22", "cfpq37_rev")
  cfpq_score_dat[["cfpq_env"]] <- rowMeans(cfpq_data_edit[env_vars])
  
  # Food as reward
  reward_vars <- c("cfpq23", "cfpq36", "cfpq19")
  cfpq_score_dat[["cfpq_reward"]] <- rowMeans(cfpq_data_edit[reward_vars])
  
  # Involvement
  involve_vars <- c("cfpq15", "cfpq20", "cfpq32")
  cfpq_score_dat[["cfpq_involve"]] <- rowMeans(cfpq_data_edit[involve_vars])
  
  # Modeling
  model_vars <- c("cfpq44", "cfpq46", "cfpq47", "cfpq48")
  cfpq_score_dat[["cfpq_model"]] <- rowMeans(cfpq_data_edit[model_vars])
  
  # Monitoring
  monitor_vars <- c("cfpq1", "cfpq2", "cfpq3", "cfpq4")
  cfpq_score_dat[["cfpq_monitor"]] <- rowMeans(cfpq_data_edit[monitor_vars])
  
  # Pressure
  pressure_vars <- c("cfpq17", "cfpq30", "cfpq39", "cfpq49")
  cfpq_score_dat[["cfpq_pressure"]] <- rowMeans(cfpq_data_edit[pressure_vars])
  
  # Restriction for Health
  restrict_health_vars <- c("cfpq21", "cfpq28", "cfpq40", "cfpq43")
  cfpq_score_dat[["cfpq_restrict_health"]] <- rowMeans(cfpq_data_edit[restrict_health_vars])
  
  # Restriction for weight control
  restrict_weight_vars <- c("cfpq18", "cfpq27", "cfpq29", "cfpq33", "cfpq34", "cfpq35", "cfpq41", "cfpq45")
  cfpq_score_dat[["cfpq_restrict_weight"]] <- rowMeans(cfpq_data_edit[restrict_weight_vars])
  
  # Teaching about nutrition
  teach_vars <- c("cfpq25", "cfpq31", "cfpq42_rev")
  cfpq_score_dat[["cfpq_teach"]] <- rowMeans(cfpq_data_edit[teach_vars])

  
  #### 3. Clean Export/Scored Data #####
  
  ## merge raw responses with scored data
  if (isTRUE(ID_arg)){
    cfpq_phenotype <- merge(cfpq_data, cfpq_score_dat, by = id)
    
    return(list(score_dat = as.data.frame(cfpq_score_dat),
                bids_phenotype = as.data.frame(cfpq_phenotype)))
  } else {
    return(list(score_dat = as.data.frame(cfpq_score_dat)))
  }
}

