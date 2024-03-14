#' score_cfq: Scored data from the Child Feeding Questionnaire
#'
#' This function scores the Child Feeding Questionnaire and provides subscale scores for the following behaviors: Perceived Responsibility, Perceived Child Weight, Perceived Parent Weight, Child Weight Concerns, Restriction, Pressure to Eat, and Monitoring
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'cfq#' or 'cfq_#' where # is the question number (1-31). Question 13 be skipped for subscale - Perceived Child Weight due to age range.
#' 3) All questions must have the numeric value for the choice with the base value being either 0 (score_base = TRUE) or 1 (score_base = FALSE)
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Birch, L. L., Fisher, J. O., Grimm-Thomas, K., Markey, C. N., Sawyer, R., & Johnson, S. L. (2001). Confirmatory factor analysis of the Child Feeng Questionnaire: A measure of parental attitudes, beliefs and practices about child feeng and obesity proneness. Appetite, 36(3), 201–210. https://doi.org/10.1006/appe.2001.0398 (\href{https://pubmed.ncbi.nlm.nih.gov/11358344/}{PubMed})
#'
#' @param cfq_data a data.frame all items for the Child Feeding Questionnaire following the naming conventions described above
#' @inheritParams score_bes
#' @param restriction_split a boolean incating if the Restriction subscale should be split to remove food as reward items. Default = FALSE. The standard Restriction subscale will always be available. If restriction_split = TRUE, then two adtional scales will be computed: 1) cfq_rest_noreward: questions 17-20, 23-24 and 1) cfq_foodreward: questions 21-22
#' @inheritParams score_bes
#'
#' @return A dataset with subscale scores for the Child Feeding Questionnaire
#'
#' @examples
#'
#' # scoring for the cfq with IDs
#' cfq_score_data <- score_cfq(cfq_data, score_base = TRUE, id = 'ID')
#'
#' # scoring for the cfq with extra Restriction subscales
#' cfq_score_data <- score_cfq(cfq_data, restriction_split = TRUE, id = 'ID')
#'
#' \dontrun{
#' }
#'
#'
#' @export

score_cfq <- function(cfq_data, score_base = TRUE, restriction_split = FALSE, id) {
  
  #### 1. Set up/initial checks #####
  
  # check that cfq_data exist and is a data.frame
  data_arg <- methods::hasArg(cfq_data)
  
  if (isTRUE(data_arg) & !is.data.frame(cfq_data)) {
    stop('cfq_data must be entered as a data.frame')
  } else if (isFALSE(data_arg)) {
    stop('cfq_data must set to the data.frame with amount consumed for each food item')
  }
  
  # check that restriction_split exist and is a string
  rest_arg <- methods::hasArg(restriction_split)
  
  if (isTRUE(rest_arg)) {
    if (restriction_split == 'true' | restriction_split == 'True' | restriction_split ==
        'TRUE') {
      # convert to boolean
      restriction_split = TRUE
    } else if (restriction_split == 'false' | restriction_split == 'False' |
               restriction_split == 'FALSE') {
      # convert to boolean
      restriction_split = FALSE
    }
  }
  
  if (!isTRUE(restriction_split) & !isFALSE(restriction_split)) {
    stop('restriction_split must be entered as a boolean and can either be: TRUE or FALSE')
  }
  
  # check if id exists
  ID_arg <- methods::hasArg(id)
  
  if (isTRUE(ID_arg)){
    if (!(id %in% names(cfq_data))) {
      stop('variable name entered as id is not in cfq_data')
    }
  }
  
  #### 2. Set Up Data #####
  
  # set up database for results
  if (isTRUE(restriction_split)) {
    # create empty matrix
    cfq_score_dat <- data.frame(cfq_resp = rep(NA, nrow(cfq_data)), cfq_pcw = rep(NA, nrow(cfq_data)), cfq_ppw = rep(NA, nrow(cfq_data)), cfq_cwc = rep(NA, nrow(cfq_data)), cfq_rest = rep(NA, nrow(cfq_data)), cfq_rest_noreward = rep(NA, nrow(cfq_data)), cfq_food_reward = rep(NA, nrow(cfq_data)), cfq_pressure = rep(NA, nrow(cfq_data)), cfq_mon = rep(NA, nrow(cfq_data)))
  } else {
    # create empty matrix
    cfq_score_dat <- data.frame(cfq_resp = rep(NA, nrow(cfq_data)), cfq_pcw = rep(NA, nrow(cfq_data)), cfq_ppw = rep(NA, nrow(cfq_data)), cfq_cwc = rep(NA, nrow(cfq_data)), cfq_rest = rep(NA, nrow(cfq_data)), cfq_pressure = rep(NA, nrow(cfq_data)), cfq_mon = rep(NA, nrow(cfq_data)))
  }
  
  if (isTRUE(ID_arg)) {
    cfq_score_dat <- data.frame(cfq_data[[id]], cfq_score_dat)
    names(cfq_score_dat)[1] <- id
  }
  
  
  # remove underscore if in column names
  names(cfq_data) <- gsub('cfq_', 'cfq', names(cfq_data))
  
  # re-scale data
  cfq_data_edit <- cfq_data
  
  if (isTRUE(score_base)){
    cfq_data_edit[2:31] <- sapply(names(cfq_data_edit)[2:31], function(x) cfq_data_edit[[x]] + 1, simplify = TRUE)
  }
  
  ## Score Subscales
  
  # Perceived Responsibility
  resp_vars <- c('cfq1', 'cfq2', 'cfq3')
  cfq_score_dat[['cfq_resp']] <- rowMeans(cfq_data_edit[resp_vars])
  
  # Perceived Child Weight
  if ('cfq13' %in% names(cfq_data)) {
    pcw_vars <- c('cfq8', 'cfq9', 'cfq10', 'cfq11', 'cfq12', 'cfq13')
  } else {
    pcw_vars <- c('cfq8', 'cfq9', 'cfq10', 'cfq11', 'cfq12')
  }
  
  cfq_score_dat[['cfq_pcw']] <- rowMeans(cfq_data_edit[pcw_vars])
  
  # Perceived Parent Weight
  ppw_vars <- c('cfq4', 'cfq5', 'cfq6', 'cfq7')
  cfq_score_dat[['cfq_ppw']] <- rowMeans(cfq_data_edit[ppw_vars])
  
  # Child Weight Concern
  cwc_vars <- c('cfq14', 'cfq15', 'cfq16')
  cfq_score_dat[['cfq_cwc']] <- rowMeans(cfq_data_edit[cwc_vars])
  
  # Restriction
  rest_vars <- c('cfq17', 'cfq18', 'cfq19', 'cfq20', 'cfq21', 'cfq22', 'cfq23',
                 'cfq24')
  cfq_score_dat[['cfq_rest']] <- rowMeans(cfq_data_edit[rest_vars])
  
  # Pressure to Eat
  pressure_vars <- c('cfq25', 'cfq26', 'cfq27', 'cfq28')
  cfq_score_dat[['cfq_pressure']] <- rowMeans(cfq_data_edit[pressure_vars])
  
  # Monitoring
  mon_vars <- c('cfq29', 'cfq30', 'cfq31')
  cfq_score_dat[['cfq_mon']] <- rowMeans(cfq_data_edit[mon_vars])
  
  #### 3. Clean Export/Scored Data #####
  ## round data
  if (isTRUE(ID_arg)){
    cfq_score_dat[2:ncol(cfq_score_dat)] <- round(cfq_score_dat[2:ncol(cfq_score_dat)], digits = 3)
  } else {
    cfq_score_dat <- round(cfq_score_dat, digits = 3)
  }
  
  ## merge raw responses with scored data
  if (isTRUE(ID_arg)){
    cfq_phenotype <- merge(cfq_data, cfq_score_dat, by = id)
    
    return(list(score_dat = as.data.frame(cfq_score_dat),
                bids_phenotype = as.data.frame(cfq_phenotype)))
  } else {
    return(list(score_dat = as.data.frame(cfq_score_dat)))
  }
  
}

