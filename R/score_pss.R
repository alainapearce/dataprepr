#' score_pss: Scored data from the 10-item Perceived Stress Scale
#'
#' This function scores the 10-item Perceived Stress Scale and provides a total PSS score and subscale scores for: Perceived Helplessness and Perceived Self-efficacy
#'
#' For data to be scored correctly, the data must be prepared according to the following criteria: \cr
#' \itemize{
#'  \item{The data must include all individual questionnaire items}
#'  \item{The columns/variables must match the following naming convention: 'pss#' or 'pss_#' where # is the question number (1-10)}
#'  \item{All questionnaire responses must be a numeric value ranging from 0-4 (base_zero = TRUE) or 1-5 (base_zero = FALSE) where: }
#'  \itemize{
#'     \item{For base_zero = TRUE: 0 = Never; 1 = Almost Never; 2 = Sometimes; 3 = Fairly Often; 4 = Very Often}
#'     \item{For base_zero = FALSE: 1 = Never; 2 = Almost Never; 3 = Sometimes; 4 = Fairly Often; 5 = Very Often}
#'   }
#'  \item{Missing values must be coded as NA}
#'  \item{Values must not be reversed scored. This script will apply reverse scoring so all levels must be true to the scale described above}
#' }
#' \cr
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' 
#' Cohen, S., Kamarck, T., and Mermelstein, R.(1983). A global measure of perceived stress. Journal of Health and Social Behavior, 24, 386-396. PMID: 6668417.
#' Cohen, S. and Williamson, G. Perceived Stress in a Probability Sample of the United States. Spacapan, S. and Oskamp, S. (Eds.) The Social Psychology of Health. Newbury Park, CA: Sage, 1988.
#' 
#' 
#' Two-factor subscale scoring comes from:
#' Taylor JM. Psychometric analysis of the Ten-Item Perceived Stress Scale. Psychol Assess. 2015 Mar;27(1):90-101. doi: 10.1037/a0038100. Epub 2014 Oct 27. PMID: 25346996.
#' 
#' @param pss_data a data.frame all items for the Perceived Stress Scale following the naming conventions described above
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @param extra_scale_cols a vector of character strings that begin with 'pss' but are not scale items. Any columns in pss_data that begin with 'pss' but are not scale items must be included here. Default is empty vector.
#' 
#'
#' @return A dataset with subscale scores for the Perceived Stress Scale
#' @examples
#'
#' # scoring for the pss with IDs
#' pss_score_data <- score_pss(pss_data, id = 'ID')
#'
#'
#' \dontrun{
#' }
#'
#' @export

score_pss <- function(pss_data, pna_value, base_zero = TRUE, id, session_id, extra_scale_cols = c()) {
  
  #### 1. Set up/initial checks #####
  
  # check that pss_data exist and is a data.frame
  data_arg <- methods::hasArg(pss_data)
  
  if (isTRUE(data_arg) & !is.data.frame(pss_data)) {
    stop("pss_data must be entered as a data.frame")
  } else if (isFALSE(data_arg)) {
    stop("pss_data must set to the data.frame with amount consumed for each food item")
  }
  
  # check if id exists
  ID_arg <- methods::hasArg(id)
  
  if (isTRUE(ID_arg)){
    if (!(id %in% names(pss_data))) {
      stop("variable name entered as id is not in pss_data")
    }
  }
  
  # check if session_id exists
  sessionID_arg <- methods::hasArg(session_id)
  
  if (isTRUE(sessionID_arg)){
    if (!(id %in% names(pss_data))) {
      stop("variable name entered as session_id is not in pss_data")
    }
  }
  
  # check base_zero is logical
  if (!is.logical(base_zero)) {
    stop("base_zero arg must be logical (TRUE/FALSE)")
  }
  
  #### 2. Set Up Data #####
  
  # set up database for results create empty matrix
  pss_score_dat <- data.frame(pss_total = rep(NA, nrow(pss_data)), pss_helplessness = rep(NA, nrow(pss_data)), pss_selfefficacy = rep(NA, nrow(pss_data)))
  
  if (isTRUE(ID_arg)) {
    if (isTRUE(sessionID_arg)) {
      pss_score_dat <- data.frame(pss_data[[id]], pss_data[[session_id]], pss_score_dat)
      names(pss_score_dat)[1:2] <- c(id, session_id)
    } else {
      pss_score_dat <- data.frame(pss_data[[id]], pss_score_dat)
      names(pss_score_dat)[1] <- id
    }
  }
  
  # assign pss scale items to pss_items, excluding columns in extra_scale_cols
  pss_items <- setdiff(grep("^pss", names(pss_data), value = TRUE), extra_scale_cols)
  
  # remove underscore in column names for pss_items
  names(pss_data)[names(pss_data) %in% pss_items] <- gsub('pss_', 'pss', names(pss_data)[names(pss_data) %in% pss_items])
  
  # remove underscore in pss_items
  pss_items <- gsub("pss_", "pss", pss_items)
  
  # if pna_value arg, replace not applicable values with NA
  if (isTRUE(methods::hasArg(pna_value))) {
    
    # replace pna_value with NA in pcw_vars
    pss_data[pss_items] <- lapply(pss_data[pss_items] , function(x) ifelse(x == pna_value, NA, x))
    
  }
  
  # check range of data and print warnings
  min <- min(pss_data[c(pss_items)], na.rm = TRUE)
  max <- max(pss_data[c(pss_items)], na.rm = TRUE)
  
  if (isTRUE(base_zero)){
    if (min < 0 | max > 4) {
      warning("range in PSS data is outside expected range given base_zero = TRUE (expected range: 0-4). Scoring may be incorrect")
    } 
  } else {
    if (min < 1 | max > 5) {
      warning("range in PSS data is outside expected range given base_zero = FALSE (expected range: 1-5). Scoring may be incorrect")
    } 
  }
  
  # re-scale data to base 0
  pss_data_edit <- pss_data
  
  if (isFALSE(base_zero)){
    pss_data_edit[pss_items] <- sapply(pss_items, function(x) pss_data[[x]] - 1, simplify = TRUE)
  }
  
  
  # calculate reversed scores
  reverse_qs <- c("pss4", "pss5", "pss7", "pss8")
  
  for (var in 1:length(reverse_qs)) {
    var_name <- reverse_qs[var]
    reverse_name <- paste0(var_name, "_rev")
    
    pss_data_edit[[reverse_name]] <- ifelse(pss_data_edit[[var_name]] == 0, 4, 
                                                    ifelse(pss_data_edit[[var_name]] == 1, 3, 
                                                           ifelse(pss_data_edit[[var_name]] == 2, 2,
                                                                  ifelse(pss_data_edit[[var_name]] == 3, 1, 
                                                                         ifelse(pss_data_edit[[var_name]] == 4, 0, NA)))))
  }
  
  ## Total Score
  pss_score_dat[["pss_total"]] <- rowSums(pss_data_edit[c("pss1", "pss2", "pss3", "pss4_rev", "pss5_rev", "pss6", "pss7_rev", "pss8_rev", "pss9", "pss10")])
  
  ## Perceived helplessness
  helplessness_vars <- c("pss1", "pss2", "pss3", "pss6", "pss9", "pss10")
  pss_score_dat[["pss_helplessness"]] <- rowSums(pss_data_edit[c(helplessness_vars)])
  
  ## Perceived self-efficacy
  # Note: Positive items are reverse-scored when calculating the total PSS score but are not reverse-scored when calculating the positive subset, 
  # “Perceived Self-Efficacy,” allowing a higher positive subset score to represent higher coping ability.
  
   efficacy_vars <- c("pss4", "pss5", "pss7", "pss8")
   pss_score_dat[["pss_selfefficacy"]] <- rowSums(pss_data_edit[c(efficacy_vars)])

  
  #### 3. Clean Export/Scored Data #####
  
  ## merge raw responses with scored data
  if (isTRUE(ID_arg)){
    if (isTRUE(sessionID_arg)) {
      pss_phenotype <- merge(pss_data, pss_score_dat, by = c(id, session_id))
    } else {
      pss_phenotype <- merge(pss_data, pss_score_dat, by = id)
    }
    
    return(list(score_dat = as.data.frame(pss_score_dat),
                bids_phenotype = as.data.frame(pss_phenotype)))
  } else {
    return(list(score_dat = as.data.frame(pss_score_dat)))
  }
}

