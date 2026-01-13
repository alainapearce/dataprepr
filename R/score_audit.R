#' score_audit: Score data from the Alcohol Use Disorders Identification Test
#'
#' This function scores the Alcohol Use Disorders Identification Test
#'
#'#' For data to be scored correctly, the data must be prepared according to the following criteria: \cr
#' \itemize{
#'  \item{The data must include all individual questionnaire items}
#'  \item{The columns/variables must match the following naming convention: 'audit#' or 'audit_#' where # is the question number (1-10)}
#'  \item{Questionnaire responses for items 1-8 must be a numeric value ranging from 0-4 (base_zero = TRUE) or 1-5 (base_zero = FALSE) where: }
#'  \itemize{
#'     \item{question 1: }
#'     \itemize{
#'        \item{For base_zero = TRUE: 0 = Never, 1 = Monthly or Less, 2 = 2-4 times a month, 3 = 2-3 times a week, 4 = 4 or more times a week}
#'        \item{For base_zero = FALSE: 1 = Never, 2 = Monthly or Less, 3 = 2-4 times a month, 4 = 2-3 times a week, 5 = 4 or more times a week}
#'    }
#'    \item{question 2: }
#'     \itemize{
#'        \item{For base_zero = TRUE: 0 = 1 or 2, 1 = 3 or 4, 2 = 5 or 6, 3 = 7 to 9, 4 = 10}
#'        \item{For base_zero = FALSE: 1 = 1 or 2, 2 = 3 or 4, 3 = 5 or 6, 4 = 7 to 9, 5 = 10}
#'    }
#'    \item{question 3-8: }
#'     \itemize{
#'        \item{For base_zero = TRUE: 0 = Never, 1 = Less than Monthly, 2 = Monthly, 3 = Weekly, 4 = Daily or Almost Daily}
#'        \item{For base_zero = FALSE: 1 = Never, 2 = Less than Monthly, 3 = Monthly, 4 = Weekly, 5 = Daily or Almost Daily}
#'    }
#'   }
#'  \item{Questionnaire responses for items 9-10 must be a numeric value of 0,2,4 (base_zero = TRUE) or 1,3,5 (base_zero = FALSE) where: }
#'  \itemize{
#'     \item{For base_zero = TRUE: 0 = No; 2 = Yes, but not in the last year; 4 = Yes, during the last year}
#'     \item{For base_zero = FALSE: 1 = No; 3 = Yes, but not in the last year; 5 = Yes, during the last year}
#'   }
#'  \item{Missing values must be coded as NA}
#' }
#' \cr
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Saunders JB, Aasland OG, Babor TF, De La Fuente JR, Grant M. Development of the Alcohol Use Disorders Identification Test (AUDIT): WHO Collaborative Project on Early Detection of Persons with Harmful Alcohol Consumption-II. Addiction. 1993;88(6):791-804. doi:10.1111/j.1360-0443.1993.tb02093.x (\href{https://pubmed.ncbi.nlm.nih.gov/8329970/}{PubMed})
#'
#' @param audit_data a data.frame all items for the Alcohol Use Disorders Identification Test following the naming conventions described above
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @param extra_scale_cols vector of character strings that begin with 'audit' but are not scale items. Any columns in audit_data that begin with 'audit' but are not scale items must be included here. Default is empty vector.
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
#' @export

score_audit <- function(audit_data, pna_value, id, session_id, base_zero = TRUE, extra_scale_cols = c()) {
  
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
  
  # check if session ID exists
  sessionID_arg <- methods::hasArg(session_id)
  
  if (isTRUE(sessionID_arg)){
    if (!(session_id %in% names(audit_data))) {
      stop("variable name entered as session_id is not in audit_data")
    }
  }
  
  # check base_zero is logical
  if (!is.logical(base_zero)) {
    stop("base_zero arg must be logical (TRUE/FALSE)")
  }
  
  #### 2. Set Up Data #####
  
  # set up database for results create empty matrix
  audit_score_dat <- data.frame(audit_total = rep(NA, nrow(audit_data)), audit_cat = rep(NA, nrow(audit_data)))
  
  if (isTRUE(ID_arg)) {
    if (isTRUE(sessionID_arg)){
      audit_score_dat <- data.frame(audit_data[[id]], audit_data[[session_id]], audit_score_dat)
      names(audit_score_dat)[1:2] <- c(id, session_id)
    } else {
      audit_score_dat <- data.frame(audit_data[[id]], audit_score_dat)
      names(audit_score_dat)[1] <- id
    }
  }
  
  # assign audit scale items to audit_items, excluding columns in extra_scale_cols
  audit_items <- setdiff(grep("^audit", names(audit_data), value = TRUE), extra_scale_cols)
  
  # remove underscore in column names for audit_items
  names(audit_data)[names(audit_data) %in% audit_items] <- gsub('audit_', 'audit', names(audit_data)[names(audit_data) %in% audit_items])
  
  # remove underscore in audit_items
  audit_items <- gsub("audit_", "audit", audit_items)
  
  # if pna_value arg, replace not applicable values with NA
  if (isTRUE(methods::hasArg(pna_value))) {
    
    # replace pna_value with NA in pcw_vars
    audit_data[audit_items] <- lapply(audit_data[audit_items] , function(x) ifelse(x == pna_value, NA, x))
    
  }
  
  # check ranges for items 1-8
  min = min(audit_data[c("audit1", "audit2", "audit3", "audit4", "audit5", "audit6", "audit7", "audit8")], na.rm = TRUE)
  max = max(audit_data[c("audit1", "audit2", "audit3", "audit4", "audit5", "audit6", "audit7", "audit8")], na.rm = TRUE)
  
  if (isTRUE(base_zero)){
    if (min < 0 | max > 4) {
      warning("range in AUDIT data items 1-8 is outside expected range given base_zero = TRUE (expected range: 0-4). Scoring may be incorrect")
    } 
  } else {
    if (min < 1 | max > 5) {
      warning("range in AUDIT data items 1-8 is outside expected range given base_zero = FALSE (expected range: 1-5). Scoring may be incorrect")
    } 
  }
  
  # check values for items 9 and 10
  if (isTRUE(base_zero)){
    allowed_values = c(0,2,4, NA)
  } else {
    allowed_values = c(1,3,5, NA)
  }
  
  if (any(!audit_data$audit9 %in% allowed_values) | any(!audit_data$audit10 %in% allowed_values)) {
    if (isTRUE(base_zero)){
      warning("AUDIT data items 9-10 contain unexpected values given base_zero = TRUE (expected values: 0,2,4). Scoring may be incorrect")
    } else {
      warning("AUDIT data items 9-10 contain unexpected values given base_zero = FALSE (expected values: 1,3,5). Scoring may be incorrect")
    }
  }
  
  # rescale to base 0
  audit_data_edit <- audit_data
  
  if (isFALSE(base_zero)){
    audit_data_edit[audit_items] <- sapply(audit_items, function(x) audit_data_edit[[x]] - 1, simplify = TRUE)
  }
  
  ## Total Score
  audit_score_dat[["audit_total"]] <- rowSums(audit_data_edit[c("audit1", "audit2", "audit3", "audit4", "audit5", "audit6", "audit7", "audit8", "audit9", "audit10")])
  
  # label consumption category
  audit_score_dat[["audit_cat"]] <- ifelse(is.na(audit_score_dat[["audit_total"]]), NA, ifelse(audit_score_dat[["audit_total"]] >=8, "Likely Harmful Consumption", "Not Harmful Consumption"))
  

  #### 3. Clean Export/Scored Data #####
  
  ## merge raw responses with scored data
  if (isTRUE(ID_arg)){
    if(isTRUE(sessionID_arg)){
      audit_phenotype <- merge(audit_data, audit_score_dat, by = c(id, session_id))
    } else {
      audit_phenotype <- merge(audit_data, audit_score_dat, by = id)
    }
    
    return(list(score_dat = as.data.frame(audit_score_dat),
                bids_phenotype = as.data.frame(audit_phenotype)))
  } else {
    return(list(score_dat = as.data.frame(audit_score_dat)))
  }
}

