#' score_sdor: Scored data from the Scatter Division of Responsibility in Feeding Scale
#'
#' This function scores the Scatter Division of Responsibility in Feeding Scale
#' 
#' 
#' For data to be scored correctly, the data must be prepared according to the following criteria: \cr
#' \itemize{
#'  \item{The data must include all individual questionnaire items except for question 13:}
#'  \item{The columns/variables must match the following naming convention: 'sdor#' or 'sdor_#' where # is the question number (1-15).}
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
#' Lohse B, Satter E, Arnold K. Development of a tool to assess adherence to a model of the division of responsibility in feeding young children: using response mapping to capacitate validation measures. Child Obes. 2014 Apr;10(2):153-68. doi: 10.1089/chi.2013.0085. PMID: 24716583. (\href{https://pubmed.ncbi.nlm.nih.gov/24716583/}{PubMed})
#'
#' @param sdor_data a data.frame all items for the Scatter Division of Responsibility in Feeding Scale following the naming conventions described above
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @param extra_scale_cols a vector of character strings that begin with 'sdor' but are not scale items. Any columns in sdor_data that begin with 'sdor' but are not scale items must be included here. Default is empty vector.
#'
#' @return A dataset with subscale scores for the Scatter Division of Responsibility in Feeding Scale
#'
#' @examples
#'
#' # scoring for the sdor with IDs, with scale from 0-1, prefer not to answer indicated with 2
#' sdor_score_data <- score_sdor(sdor_data, base_zero = TRUE, id = 'ID', pna_value = 2)
#'
#'
#' \dontrun{
#' }
#'
#'
#' @export

score_sdor <- function(sdor_data, pna_value, base_zero = TRUE, id, session_id, extra_scale_cols = c()) {
  
  #### 1. Set up/initial checks #####
  
  # check that sdor_data exist and is a data.frame
  data_arg <- methods::hasArg(sdor_data)
  
  if (isTRUE(data_arg) & !is.data.frame(sdor_data)) {
    stop('sdor_data must be entered as a data.frame')
  } else if (isFALSE(data_arg)) {
    stop('sdor_data must set to the data.frame with amount consumed for each food item')
  }
  
  # check if id exists
  ID_arg <- methods::hasArg(id)
  
  if (isTRUE(ID_arg)){
    if (!(id %in% names(sdor_data))) {
      stop('variable name entered as id is not in sdor_data')
    }
  }
  
  # check if session_id exists
  sessionID_arg <- methods::hasArg(session_id)
  
  if (isTRUE(sessionID_arg)){
    if (!(id %in% names(sdor_data))) {
      stop("variable name entered as session_id is not in sdor_data")
    }
  }
  
  # check base_zero is logical
  if (!is.logical(base_zero)) {
    stop("base_zero arg must be logical (TRUE/FALSE)")
  }
  
  #### 2. Set Up Data #####
  
  # set up database for results
  
  # create empty matrix
  sdor_score_dat <- data.frame(sdor_total = rep(NA, nrow(sdor_data)))
  
  if (isTRUE(ID_arg)) {
    if (isTRUE(sessionID_arg)) {
      sdor_score_dat <- data.frame(sdor_data[[id]], sdor_data[[session_id]], sdor_score_dat)
      names(sdor_score_dat)[1:2] <- c(id, session_id)
    } else {
      sdor_score_dat <- data.frame(sdor_data[[id]], sdor_score_dat)
      names(sdor_score_dat)[1] <- id
    }
  }
  
  # assign sdor scale items to sdor_items, excluding columns in extra_scale_cols
  sdor_items <- setdiff(grep("^sdor", names(sdor_data), value = TRUE), extra_scale_cols)
  
  if (isTRUE(sessionID_arg)) {
    sdor_items <- setdiff(grep("^sdor", names(sdor_data), value = TRUE), session_id)
  }
  
  # remove underscore in column names for sdor_items
  names(sdor_data)[names(sdor_data) %in% sdor_items] <- gsub('sdor_', 'sdor', names(sdor_data)[names(sdor_data) %in% sdor_items])
  
  # remove underscore in sdor_items
  sdor_items <- gsub("sdor_", "sdor", sdor_items)
  
  # make copy of data
  sdor_data_edit <- sdor_data
  
  # if pna_value arg, replace not applicable values with NA
  if (isTRUE(methods::hasArg(pna_value))) {
    
    # replace pna_value with NA in pcw_vars
    sdor_data_edit[sdor_items] <- lapply(sdor_data_edit[sdor_items], function(x) ifelse(x == pna_value, NA, x))
    
  }
  
  # check range of data and print warnings (check in sdor_data_edit where not_applicable_value has been replaced with NA)
  min <- min(sdor_data_edit[c(sdor_items)], na.rm = TRUE)
  max <- max(sdor_data_edit[c(sdor_items)], na.rm = TRUE)
  
  if (isTRUE(base_zero)){
    if (min < 0 | max > 4) {
      warning("range in sdor data (excluding pna_value, if specified) is outside expected range given base_zero = TRUE (expected range: 0-4). Scoring may be incorrect")
    } 
  } else {
    if (min < 1 | max > 5) {
      warning("range in sdor data (excluding pna_value, if specified) is outside expected range given base_zero = FALSE (expected range: 1-5). Scoring may be incorrect")
    } 
  }
  
  # re-scale data to base 1
  if (isFALSE(base_zero)){
    sdor_data_edit[sdor_items] <- sapply(sdor_items, function(x) sdor_data_edit[[x]] - 1, simplify = TRUE)
  }
  
  # calculate reversed scores
  reverse_qs <- c('sdor2', 'sdor3', 'sdor4', 'sdor5', 'sdor7', 'sdor9', 'sdor13')
  
  for (var in 1:length(sdor_items)) {
    var_name <- sdor_items[var]
    recode_name <- paste0(var_name, '_recode')
    
    if (var %in% reverse_qs){
      sdor_data_edit[[recode_name]] <- ifelse(sdor_data_edit[[var_name]] >=3, 0, ifelse(sdor_data_edit[[var_name]] == 2, 1, ifelse(sdor_data_edit[[var_name]] == 1, 2, ifelse(sdor_data_edit[[var_name]] == 0, 3, NA))))
    } else {
      sdor_data_edit[[recode_name]] <- ifelse(sdor_data_edit[[var_name]] <=1, 0, ifelse(sdor_data_edit[[var_name]] == 2, 1, ifelse(sdor_data_edit[[var_name]] == 3, 2, ifelse(sdor_data_edit[[var_name]] == 4, 3, NA))))
    }
  }
  
  ## Score Subscales
  
  # total
  resp_vars <- names(sdor_data_edit)[grepl('_recode', names(sdor_data_edit))]
  
  sdor_score_dat['sdor_total'] <- rowSums(sdor_data_edit[resp_vars])
  
  #### 3. Clean Export/Scored Data #####
  ## round data
  if (isTRUE(ID_arg)){
    sdor_score_dat[2:ncol(sdor_score_dat)] <- round(sdor_score_dat[2:ncol(sdor_score_dat)], digits = 3)
  } else {
    sdor_score_dat <- round(sdor_score_dat, digits = 3)
  }
  
  ## merge raw responses with scored data
  if (isTRUE(ID_arg)){
    if (isTRUE(sessionID_arg)) {
      sdor_phenotype <- merge(sdor_data, sdor_score_dat, by = c(id, session_id))
    } else {
      sdor_phenotype <- merge(sdor_data, sdor_score_dat, by = id)
    }
    
    return(list(score_dat = as.data.frame(sdor_score_dat),
                bids_phenotype = as.data.frame(sdor_phenotype)))
  } else {
    return(list(score_dat = as.data.frame(sdor_score_dat)))
  }
  
}

