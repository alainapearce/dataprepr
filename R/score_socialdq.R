#' score_socialdq: Score data from the Social Desirability Questionnaire
#'
#' This function scores the Social Desirability Questionnaire
#'
#' To use this function, the data must be prepared according to the following criteria: \cr
#' \itemize{
#'  \item{The data must include all individual questionnaire items}
#'  \item{The  columns/variables must match the following naming convention: 'sdq#' or 'sdq_# where # is the question number (1-28)}
#'  \item{All questions must have the numeric value for the choice starting with the value 0 (base_zero = TRUE) or 1 (base_zero = FALSE)}
#'}
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Ford LH. A social desirability questionnaire for young children. Journal of Consulting and Clinical Psychology 1970; 35(2):195-204. 
#'
#' @param sdq_data a data.frame all items for the Social Desirability Questionnaire following the naming conventions described above
#' @param pna_value (integer) integer used for items where child 'prefers not to answer'
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @param extra_scale_cols vector of character strings that begin with 'sdq' but are not scale items. Any columns in sdq_data that begin with 'sdq' but are not scale items must be included here. Default is empty vector.
#'
#' @return A dataset with a score for the Social Desirability Questionnaire
#' @examples
#'
#' # scoring for the sdq with IDs
#' sdq_score_data <- score_socialdq(sdq_data, parID = 'ID')
#'
#' \dontrun{
#' }
#'
#'
#' @export

score_socialdq <- function(sdq_data, pna_value, id, session_id, base_zero = TRUE, extra_scale_cols = c()) {
  
  # check that sdq_data exist and is a data.frame
  data_arg <- methods::hasArg(sdq_data)
  
  if (isTRUE(data_arg) & !is.data.frame(sdq_data)) {
    stop('sdq_data must be entered as a data.frame')
  } else if (isFALSE(data_arg)) {
    stop('sdq_data must set to the data.frame with amount consumed for each food item')
  }
  
  # check if parID exists
  ID_arg <- methods::hasArg(id)
  
  if (isTRUE(ID_arg)){
    if (!(id %in% names(sdq_data))) {
      stop('variable name entered as id is not in sdq_data')
    }
  }
  
  # check if session ID exists
  sessionID_arg <- methods::hasArg(session_id)
  
  if (isTRUE(sessionID_arg)){
    if (!(session_id %in% names(sdq_data))) {
      stop('variable name entered as session_id is not in sdq_data')
    }
  }
  
  # check base_zero is logical
  if (!is.logical(base_zero)) {
    stop('base_zero arg must be logical (TRUE/FALSE)')
  }
  
  #### 2. Set Up Data #####
  
  # set up database for results create empty matrix
  sdq_score_dat <- data.frame(sdq_total = rep(NA, nrow(sdq_data)))
  
  if (isTRUE(ID_arg)) {
    if (isTRUE(sessionID_arg)){
      sdq_score_dat <- data.frame(sdq_data[[id]], sdq_data[[session_id]], sdq_score_dat)
      names(sdq_score_dat)[1:2] <- c(id, session_id)
    } else {
      sdq_score_dat <- data.frame(sdq_data[[id]], sdq_score_dat)
      names(sdq_score_dat)[1] <- id
    }
  }
  
  # assign sdq scale items to sdq_items, excluding columns in extra_scale_cols
  sdq_items <- setdiff(grep('^sdq', names(sdq_data), value = TRUE), extra_scale_cols)
  sdq_items <- sdq_items[!grepl('sdq1$|sdq2$', sdq_items)]
  
  if (isTRUE(sessionID_arg)) {
    sdq_items <- setdiff(grep('^sdq', names(sdq_data), value = TRUE), session_id)
  }
  
  # remove underscore in column names for sdq_items
  names(sdq_data)[names(sdq_data) %in% sdq_items] <- gsub('sdq_', 'sdq', names(sdq_data)[names(sdq_data) %in% sdq_items])
  
  # remove underscore in sdq_items
  sdq_items <- gsub('sdq_', 'sdq', sdq_items)
  
  # make copy of data
  sdq_data_edit <- sdq_data
  
  # if pna_value arg, replace not applicable values with NA
  if (isTRUE(methods::hasArg(pna_value))) {
    
    # replace pna_value with NA in pcw_vars
    sdq_data_edit[sdq_items] <- lapply(sdq_data_edit[sdq_items] , function(x) ifelse(x == pna_value, NA, x))
    
  }
  
  # check ranges
  min = min(sdq_data_edit[c(sdq_items)], na.rm = TRUE)
  max = max(sdq_data_edit[c(sdq_items)], na.rm = TRUE)
  
  if (isTRUE(base_zero)){
    if (min < 0 | max > 1) {
      warning('range in sdq data items is outside expected range given base_zero = TRUE (expected range: 0-1). Scoring may be incorrect')
    } 
  } else {
    if (min < 1 | max > 2) {
      warning('range in sdq data items is outside expected range given base_zero = FALSE (expected range: 1-2). Scoring may be incorrect')
    } 
  }
  
  # re-scale data to base 1
  if (isFALSE(base_zero)){
    sdq_data_edit[sdq_items] <- sapply(sdq_items, function(x) sdq_data_edit[[x]] - 1, simplify = TRUE)
  }
  
  # calculate reversed scores
  reverse_qs <- c('sdq4', 'sdq5', 'sdq8', 'sdq9', 'sdq12', 'sdq13', 'sdq16', 'sdq19', 'sdq20', 'sdq21', 'sdq25', 'sdq26', 'sdq28')
  
  for (var in 1:length(reverse_qs)) {
    var_name <- reverse_qs[var]
    reverse_name <- paste0(var_name, '_rev')
    
    sdq_data_edit[[reverse_name]] <- ifelse(sdq_data_edit[[var_name]] == 1, 0, ifelse(sdq_data_edit[[var_name]] == 0, 1, NA))
    
  }
  
  #score
  resp_vars <- c(sdq_items[!(sdq_items %in% reverse_qs)], names(sdq_data_edit)[grepl('rev', names(sdq_data_edit))])
  
  sdq_score_dat['sdq_total'] <- rowSums(sdq_data_edit[resp_vars])
  
  
  #### 3. Clean Export/Scored Data #####
  ## round data
  if (isTRUE(ID_arg)){
    sdq_score_dat[2:ncol(sdq_score_dat)] <- round(sdq_score_dat[2:ncol(sdq_score_dat)], digits = 3)
  } else {
    sdq_score_dat <- round(sdq_score_dat, digits = 3)
  }
  
  ## merge raw responses with scored data
  if (isTRUE(ID_arg)){
    if (isTRUE(sessionID_arg)) {
      sdq_phenotype <- merge(sdq_data, sdq_score_dat, by = c(id, session_id))
    } else {
      sdq_phenotype <- merge(sdq_data, sdq_score_dat, by = id)
    }
    
    return(list(score_dat = as.data.frame(sdq_score_dat),
                bids_phenotype = as.data.frame(sdq_phenotype)))
  } else {
    return(list(score_dat = as.data.frame(sdq_score_dat)))
  }
}

