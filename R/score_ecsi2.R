#' score_ecsi2: Scored data from the Satter Eating Competence Inventory 2.0
#'
#' This function scores the Satter Eating Competence Inventory 2.0 and provides subscale scores for the following behaviors: Eating Attitudes, Food Acceptance, Internal Regulation, and Contextual Skills
#' 
#' 
#' For data to be scored correctly, the data must be prepared according to the following criteria: \cr
#' \itemize{
#'  \item{The data must include all individual questionnaire items except for question 13:}
#'  \item{The columns/variables must match the following naming convention: 'ecsi#' or 'ecsi_#' where # is the question number (1-16).}
#'  \item{All questionnaire responses must be a numeric value ranging from 0-4 (base_zero = TRUE) or 1-5 (base_zero = FALSE) where: }
#'  \itemize{
  #'    \item{For base_zero = TRUE: 0 = Never; 1 = Rarely; 2 = Sometimes; 3 = Often; 4 = Always}
#'    \item{For base_zero = TRUE: 1 = Never; 2 = Rarely; 3 = Sometimes; 4 = Often; 5 = Always}
#'   }
#'  \item{Missing values must be coded as NA}
#' }
#' \cr
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Lohse B, Satter E, Horacek T, Gebreselassie T, Oakland MJ. Measuring eating competence: psychometric properties and validity of the ecSatter Inventory. J Nutr Educ Behav. 2007 Sep-Oct;39(5 Suppl):S154-66. doi: 10.1016/j.jneb.2007.04.371. PMID: 17826696.(\href{https://pubmed.ncbi.nlm.nih.gov/17826696/}{PubMed})
#' 
#' Godleski S, Lohse B, Krall JS. Satter Eating Competence Inventory Subscale Restructure After Confirmatory Factor Analysis. J Nutr Educ Behav. 2019 Sep;51(8):1003-1010. doi: 10.1016/j.jneb.2019.05.287. Epub 2019 Jul 24. PMID: 31350197. (\href{https://pubmed.ncbi.nlm.nih.gov/31350197/}{PubMed})
#'
#' @param ecsi_data a data.frame all items for the Satter Eating Competence Inventory 2.0 following the naming conventions described above
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @param extra_scale_cols a vector of character strings that begin with 'ecsi' but are not scale items. Any columns in ecsi_data that begin with 'ecsi' but are not scale items must be included here. Default is empty vector.
#'
#' @return A dataset with subscale scores for the Satter Eating Competence Inventory 2.0
#'
#' @examples
#'
#' # scoring for the ecsi with IDs, with scale from 0-1, prefer not to answer indicated with 2
#' ecsi_score_data <- score_ecsi2(ecsi_data, base_zero = TRUE, id = 'ID', pna_value = 2)
#'
#'
#' \dontrun{
#' }
#'
#'
#' @export

score_ecsi2 <- function(ecsi_data, pna_value, base_zero = TRUE, id, session_id, extra_scale_cols = c()) {
  
  #### 1. Set up/initial checks #####
  
  # check that ecsi_data exist and is a data.frame
  data_arg <- methods::hasArg(ecsi_data)
  
  if (isTRUE(data_arg) & !is.data.frame(ecsi_data)) {
    stop('ecsi_data must be entered as a data.frame')
  } else if (isFALSE(data_arg)) {
    stop('ecsi_data must set to the data.frame with amount consumed for each food item')
  }
  
  # check if id exists
  ID_arg <- methods::hasArg(id)
  
  if (isTRUE(ID_arg)){
    if (!(id %in% names(ecsi_data))) {
      stop('variable name entered as id is not in ecsi_data')
    }
  }
  
  # check if session_id exists
  sessionID_arg <- methods::hasArg(session_id)
  
  if (isTRUE(sessionID_arg)){
    if (!(id %in% names(ecsi_data))) {
      stop("variable name entered as session_id is not in ecsi_data")
    }
  }
  
  # check base_zero is logical
  if (!is.logical(base_zero)) {
    stop("base_zero arg must be logical (TRUE/FALSE)")
  }
  
  #### 2. Set Up Data #####
  
  # set up database for results
  
  # create empty matrix
  ecsi_score_dat <- data.frame(ecsi_ea = rep(NA, nrow(ecsi_data)), ecsi_fa = rep(NA, nrow(ecsi_data)), ecsi_ir = rep(NA, nrow(ecsi_data)), ecsi_cs = rep(NA, nrow(ecsi_data)), ecsi_total = rep(NA, nrow(ecsi_data)))
  
  if (isTRUE(ID_arg)) {
    if (isTRUE(sessionID_arg)) {
      ecsi_score_dat <- data.frame(ecsi_data[[id]], ecsi_data[[session_id]], ecsi_score_dat)
      names(ecsi_score_dat)[1:2] <- c(id, session_id)
    } else {
      ecsi_score_dat <- data.frame(ecsi_data[[id]], ecsi_score_dat)
      names(ecsi_score_dat)[1] <- id
    }
  }
  
  # assign ecsi scale items to ecsi_items, excluding columns in extra_scale_cols
  ecsi_items <- setdiff(grep("^ecsi", names(ecsi_data), value = TRUE), extra_scale_cols)
  
  if (isTRUE(sessionID_arg)) {
    ecsi_items <- setdiff(grep("^ecsi", names(ecsi_data), value = TRUE), session_id)
  }
  
  # remove underscore in column names for ecsi_items
  names(ecsi_data)[names(ecsi_data) %in% ecsi_items] <- gsub('ecsi_', 'ecsi', names(ecsi_data)[names(ecsi_data) %in% ecsi_items])
  
  # remove underscore in ecsi_items
  ecsi_items <- gsub("ecsi_", "ecsi", ecsi_items)
  
  # make copy of data
  ecsi_data_edit <- ecsi_data
  
  # if pna_value arg, replace not applicable values with NA
  if (isTRUE(methods::hasArg(pna_value))) {
    
    # replace pna_value with NA in pcw_vars
    ecsi_data_edit[ecsi_items] <- lapply(ecsi_data_edit[ecsi_items], function(x) ifelse(x == pna_value, NA, x))
    
  }
  
  # check range of data and print warnings (check in ecsi_data_edit where not_applicable_value has been replaced with NA)
  min <- min(ecsi_data_edit[c(ecsi_items)], na.rm = TRUE)
  max <- max(ecsi_data_edit[c(ecsi_items)], na.rm = TRUE)
  
  if (isTRUE(base_zero)){
    if (min < 0 | max > 4) {
      warning("range in ecsi data (excluding pna_value, if specified) is outside expected range given base_zero = TRUE (expected range: 0-4). Scoring may be incorrect")
    } 
  } else {
    if (min < 1 | max > 5) {
      warning("range in ecsi data (excluding pna_value, if specified) is outside expected range given base_zero = FALSE (expected range: 1-5). Scoring may be incorrect")
    } 
  }
  
  # re-scale data to base 1
  if (isFALSE(base_zero)){
    ecsi_data_edit[ecsi_items] <- sapply(ecsi_items, function(x) ecsi_data_edit[[x]] - 1, simplify = TRUE)
  }
  
  # recode variables
  for (var in 1:length(ecsi_items)) {
    var_name <- ecsi_items[var]
    recode_name <- paste0(var_name, '_recode')
    
      ecsi_data_edit[[recode_name]] <- ifelse(ecsi_data_edit[[var_name]] <=1, 0, ifelse(ecsi_data_edit[[var_name]] == 2, 1, ifelse(ecsi_data_edit[[var_name]] == 3, 2, ifelse(ecsi_data_edit[[var_name]] == 4, 3, NA))))
  }
  
  ## Score Subscales
  
  # Eating Attitudes
  ea_vars <- c('ecsi1_recode', 'ecsi2_recode', 'ecsi4_recode', 'ecsi8_recode', 'ecsi9_recode', 'ecsi14_recode')
  ecsi_score_dat['ecsi_ea'] <- rowSums(ecsi_data_edit[ea_vars])
  
  # Food Acceptance
  fa_vars <- c('ecsi5_recode', 'ecsi6_recode', 'ecsi7_recode')
  ecsi_score_dat['ecsi_fa'] <- rowSums(ecsi_data_edit[fa_vars])
  
  
  # Internal Regulation
  ir_vars <- c('ecsi10_recode', 'ecsi13_recode')
  ecsi_score_dat['ecsi_ir'] <- rowSums(ecsi_data_edit[ir_vars])
  
  # Contextual Skills
  cs_vars <- c('ecsi3_recode', 'ecsi11_recode', 'ecsi12_recode', 'ecsi15_recode', 'ecsi16_recode')
  ecsi_score_dat['ecsi_cs'] <- rowSums(ecsi_data_edit[cs_vars])
  
  # total
  resp_vars <- names(ecsi_data_edit)[grepl('_recode', names(ecsi_data_edit))]
  
  ecsi_score_dat['ecsi_total'] <- rowSums(ecsi_data_edit[resp_vars])
  
  #### 3. Clean Export/Scored Data #####
  ## round data
  if (isTRUE(ID_arg)){
    ecsi_score_dat[2:ncol(ecsi_score_dat)] <- round(ecsi_score_dat[2:ncol(ecsi_score_dat)], digits = 3)
  } else {
    ecsi_score_dat <- round(ecsi_score_dat, digits = 3)
  }
  
  ## merge raw responses with scored data
  if (isTRUE(ID_arg)){
    if (isTRUE(sessionID_arg)) {
      ecsi_phenotype <- merge(ecsi_data, ecsi_score_dat, by = c(id, session_id))
    } else {
      ecsi_phenotype <- merge(ecsi_data, ecsi_score_dat, by = id)
    }
    
    return(list(score_dat = as.data.frame(ecsi_score_dat),
                bids_phenotype = as.data.frame(ecsi_phenotype)))
  } else {
    return(list(score_dat = as.data.frame(ecsi_score_dat)))
  }
  
}

