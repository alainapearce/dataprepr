#' score_debq: INCOMPLETE FUNCTION. Scored data from the Dutch Eating Behavior Questionnaire
#'
#' This function scores the Dutch Eating Behavior Questionnaire and provides subscale scores for the following behaviors: 
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'debq#' where # is the question number (1-24).
#' 3) Questions must have the numeric value for the choices: 1, Never | 2, Seldom | 3, Sometimes | 4, Often | 5, Very Often
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' need to add
#' @param debq_data a data.frame all items for the Dutch Eating Behavior Questionnaire following the naming conventions described above
#' @inheritParams score_bes
#'
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

score_debq <- function(debq_data, score_base = TRUE, id) {
  
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
  
  #### 2. Set Up Data #####
  
  # set up database for results create empty matrix
  debq_score_dat <- data.frame(bis = rep(NA, nrow(debq_data)), bas = rep(NA, nrow(debq_data)), bas_funseeking = rep(NA, nrow(debq_data)), bas_drive = rep(NA, nrow(debq_data)), bas_rewardresp = rep(NA, nrow(debq_data)))
  
  
  if (isTRUE(ID_arg)) {
    debq_score_dat <- data.frame(debq_data[[id]], debq_score_dat)
    names(debq_score_dat)[1] <- id
  }
  
  # remove underscore if in column names
  names(debq_data) <- gsub('debq_', 'debq', names(debq_data))
  
  # get primary questions
  debq_primary_qs <- names(debq_data[, grepl('debq', names(debq_data))])
  
  # re-scale data
  debq_data_edit <- debq_data
  
  if (isTRUE(score_base)){
    debq_data_edit[debq_primary_qs] <- sapply(debq_primary_qs, function(x) debq_data[[x]] + 1, simplify = TRUE)
  }
  
  # calculate reversed scores
  reverse_qs <- c("")
  
  for (var in 1:length(reverse_qs)) {
    var_name <- reverse_qs[var]
    reverse_name <- paste0(var_name, "_rev")
    
    debq_data_edit[[reverse_name]] <- ifelse(is.na(debq_data_edit[[var_name]]), NA, ifelse(debq_data_edit[[var_name]] == 1, 4, ifelse(debq_data_edit[[var_name]] == 2, 3,  ifelse(debq_data_edit[[var_name]] == 3, 2, 1))))
  }
  
  ## Score Subscales
  
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

