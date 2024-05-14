#' score_debq: Scored data from the Dutch Eating Behavior Questionnaire
#'
#' This function scores the Dutch Eating Behavior Questionnaire and provides subscale scores for: Emotional Eating, External Eating, Restrained Eating
#' 
#' For data to be scored correctly, the data must be prepared according to the following criteria: \cr
#' \itemize{
#'  \item{The data must include all individual questionnaire items}
#'  \item{The columns/variables must match the following naming convention: 'debq#' or 'debq_#' where # is the question number (1-24).}
#'  \item{All questionnaire responses must be a numeric value ranging from 0-4 (base_zero = TRUE) or 1-5 (base_zero = FALSE) where: }
#'  \itemize{
#'     \item{For base_zero = TRUE: 0 = Never; 1 = Rarely/Seldom; 2 = Sometimes; 3 = Often; 4 = Very Often}
#'     \item{For base_zero = FALSE: 1 = Never; 2 = Rarely/Seldom; 3 = Sometimes; 4 = Often; 5 = Very Often}
#'   }
#'  \item{Missing values must be coded as NA}
#' }
#' Note, as long as variable names match those listed, the dataset can include other variables
#' 
#' Unlike the original Dutch version of the DEBQ (van Strien et al., 1986), the English version (Wardle, 1987) does not require reverse scoring of item 21. This function scores based on Wardle, 1987
#' 
#' @references
#' Wardle J. Eating style: a validation study of the Dutch Eating Behaviour Questionnaire in normal subjects and women with eating disorders. J Psychosom Res. 1987;31(2):161-9. doi: 10.1016/0022-3999(87)90072-9. PMID: 3473234.
#' 
#' @param debq_data a data.frame all items for the Dutch Eating Behavior Questionnaire following the naming conventions described above
#' @inheritParams score_bes
#' @param extra_scale_cols a vector of character strings that begin with 'debq' but are not scale items. Any columns in debq_data that begin with 'debq' but are not scale items must be included here. Default is empty vector.
#' @return A dataset with subscale scores for the Dutch Eating Behavior Questionnaire
#' @examples
#'
#' # scoring for the debq with IDs
#' debq_score_data <- score_debq(debq_data, id = 'ID')
#'
#'
#' \dontrun{
#' }
#'
#'
#' @export

score_debq <- function(debq_data, base_zero = TRUE, id, extra_scale_cols = c()) {
  
  #### 1. Set up/initial checks #####
  
  # check that debq_data exist and is a data.frame
  data_arg <- methods::hasArg(debq_data)
  
  if (isTRUE(data_arg) & !is.data.frame(debq_data)) {
    stop("debq_data must be entered as a data.frame")
  } else if (isFALSE(data_arg)) {
    stop("debq_data must set to the data.frame with amount consumed for each food item")
  }
  
  # check if id exists
  ID_arg <- methods::hasArg(id)
  
  if (isTRUE(ID_arg)){
    if (!(id %in% names(debq_data))) {
      stop("variable name entered as id is not in debq_data")
    }
  }
  
  # check base_zero is logical
  if (!is.logical(base_zero)) {
    stop("base_zero arg must be logical (TRUE/FALSE)")
  }
  
  #### 2. Set Up Data #####
  
  # set up database for results create empty matrix
  debq_score_dat <- data.frame(debq_emotional = rep(NA, nrow(debq_data)), debq_external = rep(NA, nrow(debq_data)), debq_restrained = rep(NA, nrow(debq_data)))
  
  
  if (isTRUE(ID_arg)) {
    debq_score_dat <- data.frame(debq_data[[id]], debq_score_dat)
    names(debq_score_dat)[1] <- id
  }
  
  # assign debq scale items to debq_items, excluding columns in extra_scale_cols
  debq_items <- grep("^debq", names(debq_data), value = TRUE) %>% setdiff(extra_scale_cols)
  
  # remove underscore in column names for debq_items
  names(debq_data)[names(debq_data) %in% debq_items] <- gsub('debq_', 'debq', names(debq_data)[names(debq_data) %in% debq_items])
  
  # remove underscore in debq_items
  debq_items <- gsub("debq_", "debq", debq_items)
  
  # re-scale data
  debq_data_edit <- debq_data
  
  if (isTRUE(base_zero)){
    debq_data_edit[debq_items] <- sapply(debq_items, function(x) debq_data[[x]] + 1, simplify = TRUE)
  }

  ## Score Subscales
  
  # Emotional Eating
  emotional_vars <- c('debq1', 'debq3', 'debq5', 'debq8', 'debq10', 'debq13', 'debq16', "debq20", 'debq23', 'debq25', 'debq28', 'debq30', 'debq32')
  debq_score_dat[['debq_emotional']] <- rowMeans(debq_data_edit[emotional_vars])
  
  # External Eating
  external_vars <- c('debq2', 'debq6', 'debq9', 'debq12', 'debq15', 'debq18', 'debq21', 'debq24', 'debq27', 'debq33')
  debq_score_dat[['debq_external']] <- rowMeans(debq_data_edit[external_vars])
  
  # Restrained Eating
  restrained_vars <- c('debq4', 'debq7', 'debq11', 'debq14', 'debq17', 'debq19', 'debq22', 'debq26', 'debq29', 'debq31')
  debq_score_dat[['debq_restrained']] <- rowMeans(debq_data_edit[restrained_vars])
  
  #### 3. Clean Export/Scored Data #####

  
  ## merge raw responses with scored data
  if (isTRUE(ID_arg)){
    debq_phenotype <- merge(debq_data, debq_score_dat, by = id)
    
    return(list(score_dat = as.data.frame(debq_score_dat),
                bids_phenotype = as.data.frame(debq_phenotype)))
  } else {
    return(list(score_dat = as.data.frame(debq_score_dat)))
  }
}

