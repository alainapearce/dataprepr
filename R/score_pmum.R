#' score_pmum: Scored data from the Problematic Media Use Measure
#'
#' This function scores the Problematic Media Use Measure
#'
#' For data to be scored correctly, the data must be prepared according to the following criteria: \cr
#' \itemize{
#'  \item{The data must include all individual questionnaire items}
#'  \item{The columns/variables must match the following naming convention: 'pmum#' or 'pmum_#' where # is the question number (1-27)}
#'  \item{All questionnaire responses must be a numeric value ranging from 0-4 (base_zero = TRUE) or 1-5 (base_zero = FALSE) where: }
#'  \itemize{
#'     \item{For base_zero = TRUE: 0 = Never; 1 = Rarely; 2 = Sometimes; 3 = Often; 4 = Always}
#'     \item{For base_zero = FALSE: 1 = Never; 2 = Rarely; 3 = Sometimes; 4 = Often; 5 = Always}
#'   }
#'  \item{Missing values must be coded as NA}
#' }
#' \cr
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Domoff SE, Harrison K, Gearhardt AN, Gentile DA, Lumeng JC, Miller AL. Development and Validation of the Problematic Media Use Measure: A Parent Report Measure of Screen Media "Addiction" in Children. Psychol Pop Media Cult. 2019 Jan;8(1):2-11. doi: 10.1037/ppm0000163. Epub 2017 Nov 16. PMID: 30873299; PMCID: PMC6411079.
#'
#' @param pmum_data a data.frame all items for the Problematic Media Use Measure following the naming conventions described above
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @param extra_scale_cols a vector of character strings that begin with 'pmum' but are not scale items. Any columns in pmum_data that begin with 'pmum' but are not scale items must be included here. Default is empty vector.
#' 
#'
#' @return A dataset with total score for the Problematic Media Use Measure
#' @examples
#' # scoring for the PMUM with IDs, when values range from 0-4
#' pmum_score_data <- score_pmum(pmum_data, base_zero = TRUE, id = 'ID')
#' 
#' # scoring for the PMUM with IDs, when values range from 1-5
#' pmum_score_data <- score_pmum(pmum_data, base_zero = FALSE, id = 'ID')
#' 
#' \dontrun{
#' }
#'
#' @export

score_pmum <- function(pmum_data, base_zero = TRUE, id, session_id, extra_scale_cols = c()) {
  
  #### 1. Set up/initial checks #####
  
  # check that pmum_data exist and is a data.frame
  data_arg <- methods::hasArg(pmum_data)
  
  if (isTRUE(data_arg) & !is.data.frame(pmum_data)) {
    stop("pmum_data must be entered as a data.frame")
  } else if (isFALSE(data_arg)) {
    stop("pmum_data must set to the data.frame with amount consumed for each food item")
  }
  
  # check if id exists
  ID_arg <- methods::hasArg(id)
  
  if (isTRUE(ID_arg)){
    if (!(id %in% names(pmum_data))) {
      stop("variable name entered as id is not in pmum_data")
    }
  }
  
  # check if session_id exists
  sessionID_arg <- methods::hasArg(session_id)
  
  if (isTRUE(sessionID_arg)){
    if (!(id %in% names(pmum_data))) {
      stop("variable name entered as session_id is not in pmum_data")
    }
  }
  
  # check base_zero is logical
  if (!is.logical(base_zero)) {
    stop("base_zero arg must be logical (TRUE/FALSE)")
  }
  
  
  #### 2. Set Up Data #####
  
  # set up database for results create empty matrix
  pmum_score_dat <- data.frame(pmum_total = rep(NA, nrow(pmum_data)))
  
  
  if (isTRUE(ID_arg)) {
    pmum_score_dat <- data.frame(pmum_data[[id]], pmum_score_dat)
    names(pmum_score_dat)[1] <- id
  }
  
  # assign pmum scale items to pmum_items, excluding columns in extra_scale_cols
  pmum_items <- setdiff(grep("^pmum", names(pmum_data), value = TRUE), extra_scale_cols)
  
  # remove underscore in column names for pmum_items
  names(pmum_data)[names(pmum_data) %in% pmum_items] <- gsub('pmum_', 'pmum', names(pmum_data)[names(pmum_data) %in% pmum_items])
  
  # remove underscore in pmum_items
  pmum_items <- gsub("pmum_", "pmum", pmum_items)
  
  # check range of data and print warnings
  min <- min(pmum_data[c(pmum_items)], na.rm = TRUE)
  max <- max(pmum_data[c(pmum_items)], na.rm = TRUE)
  
  if (isTRUE(base_zero)){
    if (min < 0 | max > 4) {
      warning("range in PMUM data is outside expected range given base_zero = TRUE (expected range: 0-4). Scoring may be incorrect")
    } 
  } else {
    if (min < 1 | max > 5) {
      warning("range in PMUM data is outside expected range given base_zero = FALSE (expected range: 1-5). Scoring may be incorrect")
    } 
  }
  
  # re-scale data to base 1
  pmum_data_edit <- pmum_data
  
  if (isTRUE(base_zero)){
    pmum_data_edit[pmum_items] <- sapply(pmum_items, function(x) pmum_data[[x]] + 1, simplify = TRUE)
  }
  
  
  ## Score 
  
  pmum_score_dat[["pmum_total"]] <- rowMeans(pmum_data_edit[c(pmum_items)])
  
  #### 3. Clean Export/Scored Data #####
  
  ## merge raw responses with scored data
  if (isTRUE(ID_arg)){
    if (isTRUE(sessionID_arg)) {
      pmum_phenotype <- merge(pmum_data, pmum_score_dat, by = c(id, session_id))
    } else {
      pmum_phenotype <- merge(pmum_data, pmum_score_dat, by = id)
    }
    
    return(list(score_dat = as.data.frame(pmum_score_dat),
                bids_phenotype = as.data.frame(pmum_phenotype)))
  } else {
    return(list(score_dat = as.data.frame(pmum_score_dat)))
  }
}

