#' score_body_es: Scored data from the Body Esteem Scale
#'
#' This function scores the Body Esteem Scale
#' 
#' 
#' For data to be scored correctly, the data must be prepared according to the following criteria: \cr
#' \itemize{
#'  \item{The data must include all individual questionnaire items except for question 13:}
#'  \item{The columns/variables must match the following naming convention: 'bes#' or 'bes_#' where # is the question number (1-24).}
#'  \item{All questionnaire responses must be a numeric value ranging from 0-1 (base_zero = TRUE) or 1-2 (base_zero = FALSE) where: }
#'  \itemize{
#'    \item{For base_zero = TRUE 0 = No; 1 = Yes}
#'    \item{For base_zero = TRUE 1 = No; 2 = Yes}
#'   }
#'  \item{Missing values must be coded as NA}
#' }
#' \cr
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Mendelson, B. K., & White, D. R. (1982). Relation between body-esteem and self-esteem of obese and normal children. Perceptual and motor skills, 54(3), 899-905. https://doi.org/10.2466/pms.1982.54.3.899 (\href{https://pubmed.ncbi.nlm.nih.gov/7099901/}{PubMed})
#'
#' @param bes_data a data.frame all items for the Body Esteem Scale following the naming conventions described above
#' @param pna_value (integer) integer used for items participants indicate "prefer not to answer".
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @param extra_scale_cols a vector of character strings that begin with 'bes' but are not scale items. Any columns in bes_data that begin with 'bes' but are not scale items must be included here. Default is empty vector.
#'
#' @return A dataset with subscale scores for the Body Esteem Scale
#'
#' @examples
#'
#' # scoring for the bes with IDs, with scale from 0-1, prefer not to answer indicated with 2
#' bes_score_data <- score_body_es(bes_data, base_zero = TRUE, id = 'ID', pna_value = 2)
#'
#'
#' \dontrun{
#' }
#'
#'
#' @export

score_body_es <- function(bes_data, pna_value, base_zero = TRUE, id, session_id, extra_scale_cols = c()) {
  
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
    if (!(id %in% names(bes_data))) {
      stop("variable name entered as session_id is not in bes_data")
    }
  }
  
  # check base_zero is logical
  if (!is.logical(base_zero)) {
    stop("base_zero arg must be logical (TRUE/FALSE)")
  }
  
  #### 2. Set Up Data #####
  
  # set up database for results
  
  # create empty matrix
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
  
  if (isTRUE(sessionID_arg)) {
    bes_items <- setdiff(grep("^bes", names(bes_data), value = TRUE), session_id)
  }
  
  # remove underscore in column names for bes_items
  names(bes_data)[names(bes_data) %in% bes_items] <- gsub('bes_', 'bes', names(bes_data)[names(bes_data) %in% bes_items])
  
  # remove underscore in bes_items
  bes_items <- gsub("bes_", "bes", bes_items)
  
  # make copy of data
  bes_data_edit <- bes_data
  
  # if pna_value arg, replace not applicable values with NA
  if (isTRUE(methods::hasArg(pna_value))) {
    
    # replace pna_value with NA in pcw_vars
    bes_data_edit[bes_items] <- lapply(bes_data_edit[bes_items], function(x) ifelse(x == pna_value, NA, x))
    
  }
  
  # check range of data and print warnings (check in bes_data_edit where not_applicable_value has been replaced with NA)
  min <- min(bes_data_edit[c(bes_items)], na.rm = TRUE)
  max <- max(bes_data_edit[c(bes_items)], na.rm = TRUE)
  
  if (isTRUE(base_zero)){
    if (min < 0 | max > 1) {
      warning("range in bes data (excluding pna_value, if specified) is outside expected range given base_zero = TRUE (expected range: 0-1). Scoring may be incorrect")
    } 
  } else {
    if (min < 1 | max > 2) {
      warning("range in bes data (excluding pna_value, if specified) is outside expected range given base_zero = FALSE (expected range: 1-2). Scoring may be incorrect")
    } 
  }
  
  # re-scale data to base 1
  if (isFALSE(base_zero)){
    bes_data_edit[bes_items] <- sapply(bes_items, function(x) bes_data_edit[[x]] - 1, simplify = TRUE)
  }
  
  # calculate reversed scores
  reverse_qs <- c('bes4', 'bes5', 'bes7', 'bes8', 'bes11', 'bes12', 'bes13', 'bes16', 'bes17', 'bes18', 'bes21', 'bes24')
  
  for (var in 1:length(reverse_qs)) {
    var_name <- reverse_qs[var]
    reverse_name <- paste0(var_name, '_rev')
    
    bes_data_edit[[reverse_name]] <- ifelse(bes_data_edit[[var_name]] == 1, 0, ifelse(bes_data_edit[[var_name]] == 0, 1, NA))
    
  }
  
  ## Score Subscales
  
  # total
  resp_vars <- c(names(bes_data_edit)[names(bes_data_edit) %in% reverse_qs], names(bes_data_edit)[grepl('rev', names(bes_data_edit))])
  
  bes_score_dat['bes_total'] <- rowSums(bes_data_edit[resp_vars])
  
  #### 3. Clean Export/Scored Data #####
  ## round data
  if (isTRUE(ID_arg)){
    bes_score_dat[2:ncol(bes_score_dat)] <- round(bes_score_dat[2:ncol(bes_score_dat)], digits = 3)
  } else {
    bes_score_dat <- round(bes_score_dat, digits = 3)
  }
  
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

