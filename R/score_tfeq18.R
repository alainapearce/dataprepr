#' score_tfeq18: Scored data from the Three-Factor Eating Questionnaire-R18
#'
#' This function scores the Three-Factor Eating Questionnaire-R18 and provides scores for cognitive restraint (cr), uncontrolled eating (ue), and emotional eating (ee) subscales
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'tfeq#' or 'tfeq_#' where # is question number. 
#' 3) Questions 1-13 must have the numeric value for the choices: 1, Definitely false | 2, Mostly false | 3, Mostly true | 4, Definitely true;
#'    Questions 14 must have the numeric value for the choices: 1, Only at meal times | 2, Sometimes between meals | 3, Often between meals | 4, Almost always
#'    Questions 15 must have the numeric value for the choices: 1, Almost never | 2, Seldom | 3, Usually | 4, Almost always
#'    Questions 16 must have the numeric value for the choices: 1, Unlikely | 2, Slightly likely | 3, Moderately likely | 4, Very likely
#'    Questions 17 must have the numeric value for the choices: 1,  Never | 2, Rarely | 3, Sometimes | 4, At least once a week
#'    Questions 18 must have the numeric values 1-8
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' The Three-Factor Eating Questionnaire-R18 Is Able to Distinguish among Different Eating Patterns in a General Population
#' 
#' @param tfeq_data a data.frame all items for the Three-Factor Eating Questionnaire-R18 following the naming conventions described above
#' @inheritParams score_bes
#'
#' @return A dataset with scores for the Three-Factor Eating Questionnaire-R18
#' @examples
#'
#' # scoring for the tfeq-r18 with IDs
#' tfeq_score_data <- score_tfeq18(tfeq_data, id = 'ID')
#'
#'
#' \dontrun{
#' }
#'
#'
#' @export

score_tfeq18 <- function(tfeq_data, score_base = TRUE, id) {
  
  #### 1. Set up/initial checks #####
  
  # check that tfeq_data exist and is a data.frame
  data_arg <- methods::hasArg(tfeq_data)
  
  if (isTRUE(data_arg) & !is.data.frame(tfeq_data)) {
    stop("tfeq_data must be entered as a data.frame")
  } else if (isFALSE(data_arg)) {
    stop("tfeq_data must set to the data.frame with amount consumed for each food item")
  }
  
  # check if parID exists
  ID_arg <- methods::hasArg(id)
  
  if (isTRUE(ID_arg)){
    if (!(id %in% names(tfeq_data))) {
      stop("variable name entered as id is not in tfeq_data")
    }
  }
  
  #### 2. Set Up Data #####
  
  # set up database for results create empty matrix
  tfeq_score_dat <- data.frame(tfeq18_cr = rep(NA, nrow(tfeq_data)), tfeq18_ue = rep(NA, nrow(tfeq_data)), tfeq18_ee = rep(NA, nrow(tfeq_data)))
  
  if (isTRUE(ID_arg)) {
    tfeq_score_dat <- data.frame(tfeq_data[[id]], tfeq_score_dat)
    names(tfeq_score_dat)[1] <- id
  }
  
  # remove underscore if in column names
  names(tfeq_data) <- gsub('tfeq_', 'tfeq', names(tfeq_data))
  
  # get primary questions to score
  q_numbers <- seq(1, 18)
  tfeq_primary_qs <- paste0("tfeq", q_numbers)
  
  # re-scale data
  tfeq_data_edit <- tfeq_data
  
  if (isTRUE(score_base)){
    tfeq_data_edit[tfeq_primary_qs] <- sapply(tfeq_primary_qs, function(x) tfeq_data[[x]] + 1, simplify = TRUE)
  }
  
  # re-code item 18
  tfeq_data_edit$tfeq18_recode <- ifelse(tfeq_data_edit$tfeq18 %in% c(1, 2), 1,
                                  ifelse(tfeq_data_edit$tfeq18 %in% c(3, 4), 2,
                                  ifelse(tfeq_data_edit$tfeq18 %in% c(5, 6), 3,
                                  ifelse(tfeq_data_edit$tfeq18 %in% c(7, 8), 4, NA))))

  ## Score Subscales

  # Cognitive Restraint
  cr_vars <- c('tfeq2', 'tfeq11', 'tfeq12', 'tfeq15', 'tfeq16', 'tfeq18_recode')
  tfeq_score_dat[["tfeq18_cr"]] <- rowSums(tfeq_data_edit[cr_vars])
  
  # Uncontrolled eating
  ue_vars <- c('tfeq1', 'tfeq4', 'tfeq5', 'tfeq7', 'tfeq8', 'tfeq9', 'tfeq13', 'tfeq14', 'tfeq17')
  tfeq_score_dat[["tfeq18_ue"]] <- rowSums(tfeq_data_edit[ue_vars])
  
  # Emotional eating
  ee_vars <- c('tfeq3', 'tfeq6', 'tfeq5', 'tfeq10')
  tfeq_score_dat[["tfeq18_ee"]] <- rowSums(tfeq_data_edit[ee_vars])
  

  #### 3. Clean Export/Scored Data #####
  
  ## merge raw responses with scored data
  if (isTRUE(ID_arg)){
    tfeq_phenotype <- merge(tfeq_data, tfeq_score_dat, by = id)
    
    return(list(score_dat = as.data.frame(tfeq_score_dat),
                bids_phenotype = as.data.frame(tfeq_phenotype)))
  } else {
    return(list(score_dat = as.data.frame(tfeq_score_dat)))
  }
}
