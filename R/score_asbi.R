#' score_asbi: Scored data from the Adaptive Social Behavioral Inventory
#'
#' This function scores the Adaptive Social Behavioral Inventory and provides subscale scores for the following behaviors: Express, Comply, Disrupt
#' 
#' 
#' For data to be scored correctly, the data must be prepared according to the following criteria: \cr
#' \itemize{
#'  \item{The data must include all individual questionnaire items except for question 13:}
#'  \item{The columns/variables must match the following naming convention: 'asbi#' or 'asbi_#' where # is the question number (1-30).}
#'  \item{All questionnaire responses must be a numeric value ranging from 0-2 (base_zero = TRUE) or 1-3 (base_zero = FALSE) where: }
#'  \itemize{
  #'    \item{For base_zero = TRUE: 0 = Rarely or Never; 1 = Sometimes; 3 = Almost Always}
#'    \item{For base_zero = TRUE: 1 = Rarely or Never; 2 = Sometimes; 3 = Almost Always}
#'   }
#'  \item{Missing values must be coded as NA}
#' }
#' \cr
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Hogan, Anne E., Keith G. Scott, and Charles R. Bauer. "The Adaptive Social Behavior Inventory (ASBI): A new assessment of social competence in high-risk three-year-olds." Journal of Psychoeducational Assessment 10.3 (1992): 230-239.
#' 
#' Greenfield, Daryl B., et al. "The adaptive social behavior inventory (ASBI): Evaluation with high-risk preschoolers." Journal of Psychoeducational Assessment 15.4 (1997): 322-333.
#'
#' @param asbi_data a data.frame all items for theAdaptive Social Behavioral Inventory following the naming conventions described above
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @param extra_scale_cols a vector of character strings that begin with 'asbi' but are not scale items. Any columns in asbi_data that begin with 'asbi' but are not scale items must be included here. Default is empty vector.
#'
#' @return A dataset with subscale scores for theAdaptive Social Behavioral Inventory
#'
#' @examples
#'
#' # scoring for the asbi with IDs, with scale from 0-2, prefer not to answer indicated with 2
#' asbi_score_data <- score_asbi(asbi_data, base_zero = TRUE, id = 'ID', pna_value = 2)
#'
#'
#' \dontrun{
#' }
#'
#'
#' @export

score_asbi <- function(asbi_data, pna_value, base_zero = TRUE, id, session_id, extra_scale_cols = c()) {
  
  #### 1. Set up/initial checks #####
  
  # check that asbi_data exist and is a data.frame
  data_arg <- methods::hasArg(asbi_data)
  
  if (isTRUE(data_arg) & !is.data.frame(asbi_data)) {
    stop('asbi_data must be entered as a data.frame')
  } else if (isFALSE(data_arg)) {
    stop('asbi_data must set to the data.frame with amount consumed for each food item')
  }
  
  # check if id exists
  ID_arg <- methods::hasArg(id)
  
  if (isTRUE(ID_arg)){
    if (!(id %in% names(asbi_data))) {
      stop('variable name entered as id is not in asbi_data')
    }
  }
  
  # check if session_id exists
  sessionID_arg <- methods::hasArg(session_id)
  
  if (isTRUE(sessionID_arg)){
    if (!(id %in% names(asbi_data))) {
      stop("variable name entered as session_id is not in asbi_data")
    }
  }
  
  # check base_zero is logical
  if (!is.logical(base_zero)) {
    stop("base_zero arg must be logical (TRUE/FALSE)")
  }
  
  #### 2. Set Up Data #####
  
  # set up database for results
  
  # create empty matrix
  asbi_score_dat <- data.frame(asbi_express = rep(NA, nrow(asbi_data)), asbi_comply = rep(NA, nrow(asbi_data)), asbi_disrupt = rep(NA, nrow(asbi_data)))
  
  if (isTRUE(ID_arg)) {
    if (isTRUE(sessionID_arg)) {
      asbi_score_dat <- data.frame(asbi_data[[id]], asbi_data[[session_id]], asbi_score_dat)
      names(asbi_score_dat)[1:2] <- c(id, session_id)
    } else {
      asbi_score_dat <- data.frame(asbi_data[[id]], asbi_score_dat)
      names(asbi_score_dat)[1] <- id
    }
  }
  
  # assign asbi scale items to asbi_items, excluding columns in extra_scale_cols
  asbi_items <- setdiff(grep("^asbi", names(asbi_data), value = TRUE), extra_scale_cols)
  
  if (isTRUE(sessionID_arg)) {
    asbi_items <- setdiff(grep("^asbi", names(asbi_data), value = TRUE), session_id)
  }
  
  # remove underscore in column names for asbi_items
  names(asbi_data)[names(asbi_data) %in% asbi_items] <- gsub('asbi_', 'asbi', names(asbi_data)[names(asbi_data) %in% asbi_items])
  
  # remove underscore in asbi_items
  asbi_items <- gsub("asbi_", "asbi", asbi_items)
  
  # make copy of data
  asbi_data_edit <- asbi_data
  
  # if pna_value arg, replace not applicable values with NA
  if (isTRUE(methods::hasArg(pna_value))) {
    
    # replace pna_value with NA in pcw_vars
    asbi_data_edit[asbi_items] <- lapply(asbi_data_edit[asbi_items], function(x) ifelse(x == pna_value, NA, x))
    
  }
  
  # check range of data and print warnings (check in asbi_data_edit where not_applicable_value has been replaced with NA)
  min <- min(asbi_data_edit[c(asbi_items)], na.rm = TRUE)
  max <- max(asbi_data_edit[c(asbi_items)], na.rm = TRUE)
  
  if (isTRUE(base_zero)){
    if (min < 0 | max > 2) {
      warning("range in asbi data (excluding pna_value, if specified) is outside expected range given base_zero = TRUE (expected range: 0-2). Scoring may be incorrect")
    } 
  } else {
    if (min < 1 | max > 3) {
      warning("range in asbi data (excluding pna_value, if specified) is outside expected range given base_zero = FALSE (expected range: 1-3). Scoring may be incorrect")
    } 
  }
  
  # re-scale data to base 1
  if (isTRUE(base_zero)){
    asbi_data_edit[asbi_items] <- sapply(asbi_items, function(x) asbi_data_edit[[x]] + 1, simplify = TRUE)
  }
  
  # calculate reversed scores
  asbi_data_edit['asbi14_rev'] <- ifelse(!is.na(asbi_data_edit[['asbi14']]), ifelse( asbi_data_edit[['asbi14']] == 1, 3, ifelse(asbi_data_edit[['asbi14']] == 1, 3, 2)), NA)
  
  ## Score Subscales
  
  # Express
  express_vars <- c('asbi1', 'asbi7', 'asbi9', 'asbi11', 'asbi13', 'asbi14_rev', 'asbi16', 'asbi17', 'asbi19', 'asbi22', 'asbi24', 'asbi27', 'asbi30')
  asbi_score_dat['asbi_express'] <- rowSums(asbi_data_edit[express_vars])
  
  # Comply
  comply_vars <- c('asbi2', 'asbi3', 'asbi5', 'asbi8', 'asbi10', 'asbi12', 'asbi15', 'asbi18', 'asbi20', 'asbi25')

  asbi_score_dat['asbi_comply'] <- rowSums(asbi_data_edit[comply_vars])
  
  # Disrupt
  disrupt_vars <- c('asbi4', 'asbi6', 'asbi21', 'asbi23', 'asbi26', 'asbi28', 'asbi29')
  
  asbi_score_dat['asbi_disrupt'] <- rowSums(asbi_data_edit[disrupt_vars])
 
  #### 3. Clean Export/Scored Data #####
  ## round data
  if (isTRUE(ID_arg)){
    asbi_score_dat[2:ncol(asbi_score_dat)] <- round(asbi_score_dat[2:ncol(asbi_score_dat)], digits = 3)
  } else {
    asbi_score_dat <- round(asbi_score_dat, digits = 3)
  }
  
  ## merge raw responses with scored data
  if (isTRUE(ID_arg)){
    if (isTRUE(sessionID_arg)) {
      asbi_phenotype <- merge(asbi_data, asbi_score_dat, by = c(id, session_id))
    } else {
      asbi_phenotype <- merge(asbi_data, asbi_score_dat, by = id)
    }
    
    return(list(score_dat = as.data.frame(asbi_score_dat),
                bids_phenotype = as.data.frame(asbi_phenotype)))
  } else {
    return(list(score_dat = as.data.frame(asbi_score_dat)))
  }
  
}

