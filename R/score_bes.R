#' score_bes: Scored data from the Binge Eating Scale
#'
#' This function scores the Binge Eating Scale
#'
#' To use this function, the data must be prepared according to the following criteria: \cr
#' \cr
#' 1) The data must include all individual questionnaire items \cr
#' \cr
#' 2) The  columns/variables must match the following naming convention: 'bes#' or 'bes_#' where # is the question number (1-16) \cr
#' \cr
#' 3) This script will apply specific scoring transformations that are specific to each question. For example, the first 2 statements are reset to 0 for question 1 but not for question 2. \cr
#' \cr
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Gormally, J., Black, S., Daston, S., & Rardin, D. (1982). The assessment of binge eating severity among obese persons. Addictive Behaviors, 7(1), 47–55. https://doi.org/10.1016/0306-4603(82)90024-7  (\href{https://pubmed.ncbi.nlm.nih.gov/7080884/}{PubMed})
#'
#' Timmerman, G. M. (1999). Binge Eating Scale: Further Assessment of Validity and Reliability. Journal of Applied Biobehavioral Research, 4(1), 1–12. https://doi.org/10.1111/j.1751-9861.1999.tb00051.x
#'
#' @param bes_data a data.frame all items for the Binge Eating Scale following the naming conventions described above
#' @param pna_value value used when participant prefers not to answer/elects to skip
#' @param base_zero (logical) TRUE indicates the smallest value assigned to a choice is 0. FALSE indicates indicates the smallest value assigned to a choice is 1. Default = TRUE.
#' @param id (optional) name of participant ID column in input data. If included, the output dataset will be matched by id, if not included the output dataset will be in the order of the input data but will have no participant identifier. Required to get the phenotype dataset (raw data merged with scores.)
#' @param session_id (optional) name of session ID column in input data if there are multiple observations per participant. If included, the output dataset will be matched by id and session id, if not included the output dataset will be in the order of the input data. Required to get the phenotype dataset if have multiple sessions (raw data merged with scores.)
#' @param extra_scale_cols a vector of character strings that begin with 'bes' but are not scale items. Any columns in bes_data that begin with 'bes' but are not scale items must be included here. Default is empty vector.
#' @return A dataset with total score for the Binge Eating Scale
#' @examples
#'
#' # scoring for the bes with IDs
#' bes_score_data <- score_bes(bes_data, id = 'ID', extra_scale_cols = c("bes_date"))
#'
#' \dontrun{
#' }
#'
#'
#' @export

score_bes <- function(bes_data, base_zero = TRUE, pna_value = NA, id, session_id, extra_scale_cols = c()) {
  
  #### 1. Set up/initial checks #####
  
  # check that bes_data exist and is a data.frame
  data_arg <- methods::hasArg(bes_data)
  
  if (isTRUE(data_arg) & !is.data.frame(bes_data)) {
    stop('bes_data must be entered as a data.frame')
  } else if (isFALSE(data_arg)) {
    stop('bes_data must set to the data.frame with amount consumed for each food item')
  }
  
  # check if id exists
  ID_arg <- methods::hasArg(id)
  
  if (isTRUE(ID_arg)){
    if (!(id %in% names(bes_data))) {
      stop('variable name entered as id is not in bes_data')
    }
  }
  
  # check if session_id exists
  sessionID_arg <- methods::hasArg(session_id)
  
  if (isTRUE(sessionID_arg)){
    if (!(session_id %in% names(bes_data))) {
      stop('variable name entered as session_id is not in bes_data')
    }
  }
  
  # check base_zero is logical
  if (!is.logical(base_zero)) {
    stop("base_zero arg must be logical (TRUE/FALSE)")
  }
  
  #### 2. Set Up Data #####
  
  # set up database for results create empty matrix
  bes_score_dat <- data.frame(bes_total = rep(NA, nrow(bes_data)))
  
  if (isTRUE(ID_arg)) {
    if (isTRUE(sessionID_arg)) {
      bes_score_dat <- data.frame(bes_data[[id]], bes_data[[session_id]], bes_score_dat)
      names(bes_score_dat)[1:2] <- c(id, session_id)
    } else {
      bes_score_dat <- data.frame(bes_data[[id]], bes_score_dat)
      names(bes_score_dat)[1] <- id
    }
  }
  
  # assign bes scale items to bes_items, excluding columns in extra_scale_cols
  bes_items <- setdiff(grep("^bes", names(bes_data), value = TRUE), extra_scale_cols)

  # remove underscore in column names for bes_items
  names(bes_data)[names(bes_data) %in% bes_items] <- gsub('bes_', 'bes', names(bes_data)[names(bes_data) %in% bes_items])
  
  # remove underscore in bes_items
  bes_items <- gsub("bes_", "bes", bes_items)
    
  # make copy of data
  bes_data_edit <- bes_data
  
  # set pna_value value to NA
  bes_data_edit[bes_items] <- sapply(bes_items, function(x) ifelse(bes_data_edit[[x]] == pna_value, NA, bes_data_edit[[x]]), simplify = TRUE)
  
  # check range of data and print warnings
  min <- min(bes_data_edit[c(bes_items)], na.rm = TRUE)
  max <- max(bes_data_edit[c(bes_items)], na.rm = TRUE)
  
  if (isTRUE(base_zero)){
    if (max > 3) {
      warning("max BES value > 3. Expected max with base_zero = TRUE is 3. Scoring may be incorrect")
    } 
  } else {
    if (min < 1) {
      warning("min BES value < 1. Expected min with base_zero = FALSE is 1. Scoring may be incorrect")
    } 
  }
  
  # re-scale data to base 0
  if (isFALSE(base_zero)){
    bes_data_edit[bes_items] <- sapply(bes_items, function(x) bes_data[[x]] - 1, simplify = TRUE)
  }
  
  # calculate question - specific scoring values
  
  # custom scoring by question
  bes_data_edit[['bes1']] <- ifelse(is.na(bes_data_edit[['bes1']]), NA, ifelse(bes_data_edit[['bes1']] < 2, 0, ifelse(bes_data_edit[['bes1']] == 2, 1, 3)))
  
  bes_data_edit[['bes3']] <- ifelse(is.na(bes_data_edit[['bes3']]), NA, ifelse(bes_data_edit[['bes3']] > 1, 3, bes_data_edit[['bes3']]))
  
  bes_data_edit[['bes4']] <- ifelse(is.na(bes_data_edit[['bes4']]), NA, ifelse(bes_data_edit[['bes4']] < 3, 0, 2))
  
  bes_data_edit[['bes6']] <- ifelse(is.na(bes_data_edit[['bes6']]), NA, ifelse(bes_data_edit[['bes6']] == 2, 3, bes_data_edit[['bes6']]))
  
  bes_data_edit[['bes7']] <- ifelse(is.na(bes_data_edit[['bes7']]), NA, ifelse(bes_data_edit[['bes7']] == 1, 2, ifelse(bes_data_edit[['bes7']] == 2, 3, bes_data_edit[['bes7']])))
  
  bes_data_edit[['bes13']] <- ifelse(is.na(bes_data_edit[['bes13']]), NA, ifelse(bes_data_edit[['bes13']] < 2, 0, bes_data_edit[['bes13']]))
  
  ## Score
  
  # Total Score
  bes_score_dat[['bes_total']] <- rowSums(bes_data_edit[bes_items])
  
  #### 3. Clean Export/Scored Data ####
  
  ## merge raw responses with scored data
  if (isTRUE(ID_arg)){
    if (isTRUE(sessionID_arg)) {
      bes_phenotype <- merge(bes_data, bes_score_dat, by = c(id, session_id))
    } else {
      bes_phenotype <- merge(bes_data, bes_score_dat, by = id)
    }
    
    return(list(score_dat = as.data.frame(bes_score_dat),
                bids_phenotype = as.data.frame(bes_phenotype)))
  } else {
    return(list(score_dat = as.data.frame(bes_score_dat)))
  }
}

