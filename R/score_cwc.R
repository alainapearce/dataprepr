#' score_cwc: Score data from the Child Weight Concerns
#'
#' This function scores the Child Weight Concerns
#'
#' To use this function, the data must be prepared according to the following criteria: \cr
#' \itemize{
#'  \item{The data must include all individual questionnaire items}
#'  \item{The  columns/variables must match the following naming convention: 'cwc#' or 'cwc_# where # is the question number (1-5)}
#'  \item{All questions must have the numeric value for the choice starting with the value 0 (base_zero = TRUE) or 1 (base_zero = FALSE). Each item is scored differently so responses are scaled to be 0-100 for each item.}
#'}
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Killen JD, Taylor CB, Hayward C, et al. Pursuit of thinness and onset of eating disorder symptoms in a community sample of adolescent girls: A three-year prospective analysis. Int J Eat Disord. 1994;16(3):227-238. doi:10.1002/1098-108X(199411)16:3<227::AID-EAT2260160303>3.0.CO;2-L (\href{https://pubmed.ncbi.nlm.nih.gov/7833956/}{PubMed})
#'
#' Taylor CB, Sharpe T, Shisslak C, et al. Factors associated with weight concerns in adolescent girls. Int J Eat Disord. 1998;24(1):31-42. doi:10.1002/(SICI)1098-108X(199807)24:1<31::AID-EAT3>3.0.CO;2-1 (\href{https://pubmed.ncbi.nlm.nih.gov/9589309/}{PubMed})
#'
#' @param cwc_data a data.frame all items for the Child Weight Concerns following the naming conventions described above
#' @param pna_value (integer) integer used for items where child 'prefers not to answer'
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @param extra_scale_cols vector of character strings that begin with 'cwc' but are not scale items. Any columns in cwc_data that begin with 'cwc' but are not scale items must be included here. Default is empty vector.
#'
#' @return A dataset with a score for the Child Weight Concerns
#' @examples
#'
#' # scoring for the cwc with IDs
#' cwc_score_data <- score_cwc(cwc_data, parID = 'ID')
#'
#' \dontrun{
#' }
#'
#'
#' @export

score_cwc <- function(cwc_data, pna_value, id, session_id, base_zero = TRUE, extra_scale_cols = c()) {
  
  # check that cwc_data exist and is a data.frame
  data_arg <- methods::hasArg(cwc_data)
  
  if (isTRUE(data_arg) & !is.data.frame(cwc_data)) {
    stop('cwc_data must be entered as a data.frame')
  } else if (isFALSE(data_arg)) {
    stop('cwc_data must set to the data.frame with amount consumed for each food item')
  }
  
  # check if parID exists
  ID_arg <- methods::hasArg(id)
  
  if (isTRUE(ID_arg)){
    if (!(id %in% names(cwc_data))) {
      stop('variable name entered as id is not in cwc_data')
    }
  }
  
  # check if session ID exists
  sessionID_arg <- methods::hasArg(session_id)
  
  if (isTRUE(sessionID_arg)){
    if (!(session_id %in% names(cwc_data))) {
      stop('variable name entered as session_id is not in cwc_data')
    }
  }
  
  # check base_zero is logical
  if (!is.logical(base_zero)) {
    stop('base_zero arg must be logical (TRUE/FALSE)')
  }
  
  #### 2. Set Up Data #####
  
  # set up database for results create empty matrix
  cwc_score_dat <- data.frame(cwc_total = rep(NA, nrow(cwc_data)))
  
  if (isTRUE(ID_arg)) {
    if (isTRUE(sessionID_arg)){
      cwc_score_dat <- data.frame(cwc_data[[id]], cwc_data[[session_id]], cwc_score_dat)
      names(cwc_score_dat)[1:2] <- c(id, session_id)
    } else {
      cwc_score_dat <- data.frame(cwc_data[[id]], cwc_score_dat)
      names(cwc_score_dat)[1] <- id
    }
  }
  
  # assign cwc scale items to cwc_items, excluding columns in extra_scale_cols
  cwc_items <- setdiff(grep('^cwc', names(cwc_data), value = TRUE), extra_scale_cols)
  
  if (isTRUE(sessionID_arg)) {
    cwc_items <- setdiff(grep('^cwc', names(cwc_data), value = TRUE), session_id)
  }
  
  # remove underscore in column names for cwc_items
  names(cwc_data)[names(cwc_data) %in% cwc_items] <- gsub('cwc_', 'cwc', names(cwc_data)[names(cwc_data) %in% cwc_items])
  
  # remove underscore in cwc_items
  cwc_items <- gsub('cwc_', 'cwc', cwc_items)
  
  # make copy of data
  cwc_data_edit <- cwc_data
  
  # if pna_value arg, replace not applicable values with NA
  if (isTRUE(methods::hasArg(pna_value))) {
    
    # replace pna_value with NA in pcw_vars
    cwc_data_edit[cwc_items] <- lapply(cwc_data_edit[cwc_items] , function(x) ifelse(x == pna_value, NA, x))
    
  }
  
  # check ranges
  min = min(cwc_data_edit[c(cwc_items)], na.rm = TRUE)
  max = max(cwc_data_edit[c(cwc_items)], na.rm = TRUE)
  
  if (isTRUE(base_zero)){
    if (min < 0 | max > 6) {
      warning('range in CWC data items is outside expected range given base_zero = TRUE (expected range: 0-6). Scoring may be incorrect')
    } 
  } else {
    if (min < 1 | max > 7) {
      warning('range in CWC data items is outside expected range given base_zero = FALSE (expected range: 1-7). Scoring may be incorrect')
    } 
  }
  
  # re-scale data to base 1
  if (isTRUE(base_zero)){
    cwc_data_edit[cwc_items] <- sapply(cwc_items, function(x) cwc_data_edit[[x]] + 1, simplify = TRUE)
  }
  
  ## scale values to be 0-100
  qs_5scale <- c('cwc1', 'cwc2', 'cwc5')
  
  cwc_data_edit[c(qs_5scale)] <- sapply(qs_5scale, function(x) ifelse(is.na(cwc_data_edit[[x]]), NA, ifelse(cwc_data_edit[[x]] == 1, 0, ifelse(cwc_data_edit[[x]] == 2, 16.67, ifelse(cwc_data_edit[[x]] == 3, 50, ifelse(cwc_data_edit[[x]] == 4, 75, ifelse(cwc_data_edit[[x]] == 5, 100, NA)))))))
  
  # 7-chioce question
  cwc_data[['cwc3']] <- ifelse(is.na(cwc_data_edit[['cwc3']]), NA, ifelse(cwc_data_edit[['cwc3']] == 1, 0, ifelse(cwc_data_edit[['cwc3']] == 2, 25, ifelse(cwc_data_edit[['cwc3']] == 3, 33.34, ifelse(cwc_data_edit[['cwc3']] == 4, 50, ifelse(cwc_data_edit[['cwc3']] == 5, 66.68, ifelse(cwc_data_edit[['cwc3']] == 6, 83.35, ifelse(cwc_data_edit[['cwc3']] == 7, 100, NA))))))))
  
  # 4-chioce question
  cwc_data_edit[['cwc4']] <- ifelse(is.na(cwc_data_edit[['cwc4']]), NA, ifelse(cwc_data_edit[['cwc4']] == 1, 0, ifelse(cwc_data_edit[['cwc4']] == 2, 33.33, ifelse(cwc_data_edit[['cwc4']] == 3, 66.66, ifelse(cwc_data_edit[['cwc4']] == 4, 100, NA)))))
  
  #score if have at least 2 scores
  cwc_score_dat[['cwc_total']] <- rowMeans(cwc_data_edit[cwc_items])
  
  
  #### 3. Clean Export/Scored Data #####
  ## round data
  if (isTRUE(ID_arg)){
    cwc_score_dat[2:ncol(cwc_score_dat)] <- round(cwc_score_dat[2:ncol(cwc_score_dat)], digits = 3)
  } else {
    cwc_score_dat <- round(cwc_score_dat, digits = 3)
  }
  
  ## merge raw responses with scored data
  if (isTRUE(ID_arg)){
    if (isTRUE(sessionID_arg)) {
      cwc_phenotype <- merge(cwc_data, cwc_score_dat, by = c(id, session_id))
    } else {
      cwc_phenotype <- merge(cwc_data, cwc_score_dat, by = id)
    }
    
    return(list(score_dat = as.data.frame(cwc_score_dat),
                bids_phenotype = as.data.frame(cwc_phenotype)))
  } else {
    return(list(score_dat = as.data.frame(cwc_score_dat)))
  }
}

