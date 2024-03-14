#' score_audit: Score data from the Alcohol Use Disorders Identification Test
#'
#' This function scores the Alcohol Use Disorders Identification Test
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'audit#' or 'audit_#' where # is the question number (1-10)
#' 3) All questions must have the numeric value for the choice:
#' 3a) question 1: 0 - Never, 1 - Monthly or Less, 2 - 2-4 times a month, 3 - 2-3 times a week, 4 - 4 or more times a week
#' 3b) question 2: 0 - 1 or 2, 1 - 3 or 4, 2 - 5 or 6, 3 - 7 to 9, 4 - 10
#' 3c) question 3-8: 0 - Never, 1 - Less than Monthly, 2 - Monthly, 3 - Weekly, 4 - Daily or Almost Daily
#' 3d) questions 9-10: 0 - No, 2 - Yes, but not in the last year, 4 - Yes, during the last year
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Saunders JB, Aasland OG, Babor TF, De La Fuente JR, Grant M. Development of the Alcohol Use Disorders Identification Test (AUDIT): WHO Collaborative Project on Early Detection of Persons with Harmful Alcohol Consumption-II. Addiction. 1993;88(6):791-804. doi:10.1111/j.1360-0443.1993.tb02093.x (\href{https://pubmed.ncbi.nlm.nih.gov/8329970/}{PubMed})
#'
#' @param audit_data a data.frame all items for the Alcohol Use Disorders Identification Test following the naming conventions described above
#' @inheritParams score_bes
#'
#' @return A dataset with a score for the Alcohol Use Disorders Identification Test
#' @examples
#'
#' # scoring for the audit with IDs
#' audit_score_data <- score_audit(audit_data, id = 'ID')
#'
#' \dontrun{
#' }
#'
#' @seealso Raw data from Qualtrics was processed using the following script: \code{\link{util_fbs_parent_v5dat}}
#'
#'
#' @export

score_audit <- function(audit_data, id) {
  
  #### 1. Set up/initial checks #####
  
  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(audit_data)
  
  if (isTRUE(data_arg) & !is.data.frame(audit_data)) {
    stop("audit_data must be entered as a data.frame")
  } else if (isFALSE(data_arg)) {
    stop("audit_data must set to the data.frame with amount consumed for each food item")
  }
  
  # check if parID exists
  ID_arg <- methods::hasArg(id)
  
  if (isTRUE(ID_arg)){
    if (!(id %in% names(audit_data))) {
      stop("variable name entered as id is not in audit_data")
    }
  }
  
  #### 2. Set Up Data #####
  
  # set up database for results create empty matrix
  audit_score_dat <- data.frame(audit_total = rep(NA, nrow(audit_data)), audit_cat = rep(NA, nrow(audit_data)))
  
  if (isTRUE(ID_arg)) {
    audit_score_dat <- data.frame(audit_data[[id]], audit_score_dat)
    names(audit_score_dat)[1] <- id
  }
  
  # remove underscore if in column names
  names(audit_data) <- gsub('audit_', 'audit', names(audit_data))
  
  # get primary questions to score
  q_numbers <- seq(1, 10)
  audit_primary_qs <- paste0("audit", q_numbers)
  
  ## Total Score
  audit_score_dat[["audit_total"]] <- rowSums(audit_data[audit_primary_qs])
  
  # label consumption category
  audit_score_dat[["audit_cat"]] <- ifelse(is.na(audit_score_dat[["audit_total"]]), NA, ifelse(audit_score_dat[["audit_total"]] >=8, "Likely Harmful Consumption", "Not Harmful Consumption"))
  

  #### 3. Clean Export/Scored Data #####
  
  ## merge raw responses with scored data
  if (isTRUE(ID_arg)){
    audit_phenotype <- merge(audit_data, audit_score_dat, by = id)
    
    return(list(score_dat = as.data.frame(audit_score_dat),
                bids_phenotype = as.data.frame(audit_phenotype)))
  } else {
    return(list(score_dat = as.data.frame(audit_score_dat)))
  }
}

