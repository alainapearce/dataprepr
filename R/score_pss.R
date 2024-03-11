#' score_pss: Scored data from the 10-item Percieved Stress Scale
#'
#' This function scores the Percieved Stress Scale and provides XXX
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'pss#' where # is the question number (1-10)
#' 3) Questions 1-10 must have the numeric value for the choices: 0 - Never, 1 - Almost, 2 - Sometimes, 3 - Fairly Often, 4 - Very Often
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' 
#' WHICH IS FOR THE 10-item?
#' Cohen, S., Kamarck, T., and Mermelstein, R.(1983). A global measure of perceived stress. Journal of Health and Social Behavior, 24, 386-396. PMID: 6668417.
#' Cohen, S. and Williamson, G. Perceived Stress in a Probability Sample of the United States. Spacapan, S. and Oskamp, S. (Eds.) The Social Psychology of Health. Newbury Park, CA: Sage, 1988.
#' 
#' 
#' Subscale scoring comes from:
#' Taylor JM. Psychometric analysis of the Ten-Item Perceived Stress Scale. Psychol Assess. 2015 Mar;27(1):90-101. doi: 10.1037/a0038100. Epub 2014 Oct 27. PMID: 25346996.
#' 
#' @param pss_data a data.frame all items for the Percieved Stress Scale following the naming conventions described above
#' @inheritParams score_bes
#'
#' @return A dataset with subscale scores for the Parent Weight-Loss Behavior Questionnaire
#' @examples
#'
#' # scoring for the pss with IDs
#' pss_score_data <- score_pss(pss_data, id = 'ID')
#'
#'
#' \dontrun{
#' }
#'
#' @seealso Raw data from Qualtrics was processed using the following script: \code{\link{util_fbs_parent_v3dat}}
#'
#'
#' @export

score_pss <- function(pss_data, score_base = TRUE, id) {
  
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
  
  #### 2. Set Up Data #####
  
  # set up database for results create empty matrix
  pss_score_dat <- data.frame(pss_total = rep(NA, nrow(pss_data)), pss_helplessness = rep(NA, nrow(pss_data)), pss_selfefficacy = rep(NA, nrow(pss_data)))
  
  if (isTRUE(ID_arg)) {
    pss_score_dat <- data.frame(pss_data[[id]], pss_score_dat)
    names(pss_score_dat)[1] <- id
  }
  
  # remove underscore if in column names
  names(pss_data) <- gsub('pss_', 'pss', names(pss_data))
  
  # get primary questions to score
  q_numbers <- seq(1, 10)
  pss_primary_qs <- paste0("pss", q_numbers)
  
  # re-scale data
  pss_data_edit <- pss_data
  
  if (!isTRUE(score_base)){
    pss_data_edit[pss_primary_qs] <- sapply(pss_primary_qs, function(x) pss_data[[x]] - 1, simplify = TRUE)
  }
  
  
  # calculate reversed scores
  reverse_qs <- c("pss4", "pss5", "pss7", "pss8")
  
  for (var in 1:length(reverse_qs)) {
    var_name <- reverse_qs[var]
    reverse_name <- paste0(var_name, "_rev")
    
    pss_data_edit[[reverse_name]] <- ifelse(is.na(pss_data_edit[[var_name]]), NA, 
                                             ifelse(pss_data_edit[[var_name]] == 0, 4, 
                                                    ifelse(pss_data_edit[[var_name]] == 1, 3, 
                                                           ifelse(pss_data_edit[[var_name]] == 2, 2,
                                                                  ifelse(pss_data_edit[[var_name]] == 3, 1, 0)))))
  }
  
  ## Total Score
  pss_score_dat[["pss_total"]] <- rowSums(pss_data_edit[c("pss1", "pss2", "pss3", "pss4_rev", "pss5_rev", "pss6", "pss7_rev", "pss8_rev", "pss9", "pss10")])
  
  ## Perceived helplessness
  helplessness_vars <- c("pss1", "pss2", "pss3", "pss6", "pss9", "pss10")
  pss_score_dat[["pss_helplessness"]] <- rowSums(pss_data_edit[c(helplessness_vars)])
  
  ## Lack of self-efficacy # confirm these used reverse scores 
#  efficacy_vars <- c("pss4_rev", "pss5_rev", "pss7_rev", "pss8_rev")
#  pss_score_dat[["pss_selfefficacy"]] <- rowSums(pss_data_edit[c(efficacy_vars)])
  
  
  #### 3. Clean Export/Scored Data #####
  
  ## merge raw responses with scored data
  if (isTRUE(ID_arg)){
    pss_phenotype <- merge(pss_data, pss_score_dat, by = id)
    
    return(list(score_dat = as.data.frame(pss_score_dat),
                bids_phenotype = as.data.frame(pss_phenotype)))
  } else {
    return(list(score_dat = as.data.frame(pss_score_dat)))
  }
}

