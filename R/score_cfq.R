#' score_cfq: Scored data from the Child Feeding Questionnaire
#'
#' This function scores the Child Feeding Questionnaire and provides subscale scores for the following behaviors: Perceived Responsibility, Perceived Child Weight, Perceived Parent Weight, Child Weight Concerns, Restriction, Pressure to Eat, and Monitoring
#' 
#' For all subscales except Perceived Child Weight, scores will only be computed if there are no missing values in subscale items. For Perceived Child Weight, subscale scores will be calculated ignoring missing items and pcw_na_value responses (see Arguments) 
#' 
#' For data to be scored correctly, the data must be prepared according to the following criteria: \cr
#' \itemize{
#'  \item{The data must include all individual questionnaire items except for question 13:}
#'  \itemize{
#'    \item{Question 13 can be excluded from data due to age range. If excluded, Perceived Child Weight will be calculated using items 8-12}
#'   }
#'  \item{The columns/variables must match the following naming convention: 'cfq#' or 'cfq_#' where # is the question number (1-31).}
#'  \item{All questionnaire responses must be a numeric value ranging from 0-4 (base_zero = TRUE) or 1-5 (base_zero = FALSE) where: }
#'  \itemize{
#'    \item{For base_zero = TRUE:}
#'    \itemize{
#'      \item{Items 1-3: 0 = Never; 1 = Seldom; 2 = Half of time; 3 = Most of time; 4 = Always}
#'      \item{Items 4-13: 0 = Markedly Underweight; 1 = Underweight; 2 = Average; 3 = Overweight; 4 = Markedly Overweight}
#'      \item{Items 14-16: 0 = Unconcerned; 1 = Slightly unconcerned; 2 = Neutral; 3 = Slightly concerned; 4 = Very Concerned}
#'      \item{Items 17-28: 0 = Disagree; 1 = Slightly disagree; 2 = Neutral; 3 = Slightly agree; 4 = Agree}
#'      \item{Items 29-31: 0 = Never; 1 = Rarely; 2 = Sometimes; 3 = Mostly; 4 = Always}
#'    }
#'    \item{For base_zero = TRUE:}
#'    \itemize{
#'      \item{Items 1-3: 1 = Never; 2 = Seldom; 3 = Half of time; 4 = Most of time; 5 = Always}
#'      \item{Items 4-13: 1 = Markedly Underweight; 2 = Underweight; 3 = Average; 4 = Overweight; 5 = Markedly Overweight}
#'      \item{Items 14-16: 1 = Unconcerned; 2 = Slightly unconcerned; 3 = Neutral; 4 = Slightly concerned; 5 = Very Concerned}
#'      \item{Items 17-28: 1 = Disagree; 2 = Slightly disagree; 3 = Neutral; 4 = Slightly agree; 5 = Agree}
#'      \item{Items 29-31: 1 = Never; 2 = Rarely; 3 = Sometimes; 4 = Mostly; 5 = Always}
#'    }
#'   }
#'  \item{Missing values must be coded as NA}
#'  \item{Items 8-13 (Perceived Child Weight subscale) can include responses reflecting "not applicable", as these items ask about certain age ranges. The value encoding "not applicable" must be specified with the pcw_na_value parameter}
#' }
#' \cr
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Birch, L. L., Fisher, J. O., Grimm-Thomas, K., Markey, C. N., Sawyer, R., & Johnson, S. L. (2001). Confirmatory factor analysis of the Child Feeng Questionnaire: A measure of parental attitudes, beliefs and practices about child feeding and obesity proneness. Appetite, 36(3), 201–210. https://doi.org/10.1006/appe.2001.0398 (\href{https://pubmed.ncbi.nlm.nih.gov/11358344/}{PubMed})
#'
#' @param cfq_data a data.frame all items for the Child Feeding Questionnaire following the naming conventions described above
#' @param pcw_na_value (integer) integer used for items 8-13 (Perceived Child Weight items) to indicate "not applicable" response.
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @param restriction_split logical indicating if the Restriction subscale should be split to remove food as reward items. Default = FALSE. The standard Restriction subscale will always be available. If restriction_split = TRUE, then two additional scales will be computed: 1) cfq_rest_noreward: questions 17-20, 23-24 and 1) cfq_foodreward: questions 21-22
#' @param extra_scale_cols a vector of character strings that begin with 'cfq' but are not scale items. Any columns in cfq_data that begin with 'cfq' but are not scale items must be included here. Default is empty vector.
#'
#' @return A dataset with subscale scores for the Child Feeding Questionnaire
#'
#' @examples
#'
#' # scoring for the cfq with IDs, with scale from 0-4, and items 8-13 have the value 99 indicating "not applicable"
#' cfq_score_data <- score_cfq(cfq_data, base_zero = TRUE, id = 'ID', pcw_na_value = 99)
#'
#' # scoring for the cfq with extra Restriction subscales, with scale from 1-5
#' cfq_score_data <- score_cfq(cfq_data, base_zero = FALSE, restriction_split = TRUE, id = 'ID')
#'
#' \dontrun{
#' }
#'
#'
#' @export

score_cfq <- function(cfq_data, base_zero = TRUE, restriction_split = FALSE, id, session_id, extra_scale_cols = c(), pcw_na_value) {
  
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
  
  # check if session_id exists
  sessionID_arg <- methods::hasArg(session_id)
  
  if (isTRUE(sessionID_arg)){
    if (!(id %in% names(cfq_data))) {
      stop("variable name entered as session_id is not in cfq_data")
    }
  }
  
  # check base_zero is logical
  if (!is.logical(base_zero)) {
    stop("base_zero arg must be logical (TRUE/FALSE)")
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
    if (isTRUE(sessionID_arg)) {
      cfq_score_dat <- data.frame(cfq_data[[id]], cfq_data[[session_id]], cfq_score_dat)
      names(cfq_score_dat)[1:2] <- c(id, session_id)
    } else {
      cfq_score_dat <- data.frame(cfq_data[[id]], cfq_score_dat)
      names(cfq_score_dat)[1] <- id
    }
  }
  
  # assign cfq scale items to cfq_items, excluding columns in extra_scale_cols
  cfq_items <- grep("^cfq", names(cfq_data), value = TRUE) %>% setdiff(extra_scale_cols)
  
  # remove underscore in column names for cfq_items
  names(cfq_data)[names(cfq_data) %in% cfq_items] <- gsub('cfq_', 'cfq', names(cfq_data)[names(cfq_data) %in% cfq_items])
  
  # remove underscore in cfq_items
  cfq_items <- gsub("cfq_", "cfq", cfq_items)
  
  # make copy of data
  cfq_data_edit <- cfq_data

  # if pcw_na_value arg, replace not applicable values with NA
  if (isTRUE(methods::hasArg(pcw_na_value))) {
    
    # define pcw_vars 
    if ('cfq13' %in% names(cfq_data_edit)) {
      pcw_vars <- c('cfq8', 'cfq9', 'cfq10', 'cfq11', 'cfq12', 'cfq13')
    } else {
      pcw_vars <- c('cfq8', 'cfq9', 'cfq10', 'cfq11', 'cfq12')
    }
    
    # replace pcw_na_value with NA in pcw_vars
    cfq_data_edit[pcw_vars] <- lapply(cfq_data_edit[pcw_vars], function(x) ifelse(x == pcw_na_value, NA, x))
    
  }
  
  # check range of data and print warnings (check in cfq_data_edit where not_applicable_value has been replaced with NA)
  min <- min(cfq_data_edit[c(cfq_items)], na.rm = TRUE)
  max <- max(cfq_data_edit[c(cfq_items)], na.rm = TRUE)
  
  if (isTRUE(base_zero)){
    if (min < 0 | max > 4) {
      warning("range in CFQ data (excluding pcw_na_value, if specified) is outside expected range given base_zero = TRUE (expected range: 0-4). Scoring may be incorrect")
    } 
  } else {
    if (min < 1 | max > 5) {
      warning("range in CFQ data (excluding pcw_na_value, if specified) is outside expected range given base_zero = FALSE (expected range: 1-5). Scoring may be incorrect")
    } 
  }
  
  # re-scale data to base 1
  if (isTRUE(base_zero)){
    cfq_data_edit[cfq_items] <- sapply(cfq_items, function(x) cfq_data_edit[[x]] + 1, simplify = TRUE)
  }
  
  ## Score Subscales
  
  # Perceived Responsibility
  resp_vars <- c('cfq1', 'cfq2', 'cfq3')
  cfq_score_dat[['cfq_resp']] <- rowMeans(cfq_data_edit[resp_vars])
  
  # Perceived Child Weight
  if ('cfq13' %in% names(cfq_data_edit)) {
    pcw_vars <- c('cfq8', 'cfq9', 'cfq10', 'cfq11', 'cfq12', 'cfq13')
  } else {
    pcw_vars <- c('cfq8', 'cfq9', 'cfq10', 'cfq11', 'cfq12')
  }

  # calculate the mean across pcw_vars (ignoring NAs), except if all missing values
  cfq_score_dat[['cfq_pcw']] <- apply(cfq_data_edit[pcw_vars], 1, function(row) {
    if (all(is.na(row))) {
      NA
    } else {
      mean(row, na.rm = TRUE)
    }
  })
  
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
    if (isTRUE(sessionID_arg)) {
      cfq_phenotype <- merge(cfq_data, cfq_score_dat, by = c(id, session_id))
    } else {
      cfq_phenotype <- merge(cfq_data, cfq_score_dat, by = id)
    }
    
    return(list(score_dat = as.data.frame(cfq_score_dat),
                bids_phenotype = as.data.frame(cfq_phenotype)))
  } else {
    return(list(score_dat = as.data.frame(cfq_score_dat)))
  }
  
}

