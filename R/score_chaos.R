#' score_chaos: Scored data from the 15-item Confusion, Hubbub, and Order Scale administered with 4-point likert scale 
#'
#' This function scores the 15-item Confusion, Hubbub, and Order Scale and provides a total score. Note, this function will only provide accurate scores if a 4-point likert scale was used during data collection.
#' 
#' For data to be scored correctly, the data must be prepared according to the following criteria: \cr
#' \itemize{
#'  \item{The data must include all individual questionnaire items}
#'  \item{The  columns/variables must match the following naming convention: 'chaos#' or 'chaos_#' where # is the question number (1-15)}
#'  \item{All questionnaire responses must be a numeric value ranging from 0-3 (base_zero = TRUE) or 1-4 (base_zero = FALSE) where: }
#'  \itemize{
#'     \item{For base_zero = TRUE: 0 = Very much like your own home; 1 = Somewhat like your own home; 2 = A little bit like your own home; 3 = Not at all like your own home}
#'     \item{For base_zero = FALSE: 1 = Very much like your own home; 2 = Somewhat like your own home; 3 = A little bit like your own home; 4 = Not at all like your own home}
#'   }
#'  \item{Missing values must be coded as NA}
#'  \item{Values must not be reversed scored. This script will apply reverse scoring so all levels must be true to the scale described above}
#' }
#' \cr
#' Note, as long as variable names match those listed, the dataset can include other variables. See extra_scale_cols argument
#'
#' @references
#' Matheny, A. P., Jr., Wachs, T. D., Ludwig, J. L., Phillips, K. (1995). Bringing order out of chaos: Psychometric characteristics of the confusion, hubbub, and order scale. Journal of Applied Developmental Psychology, 16(3), 429–444. https://doi.org/10.1016/0193-3973(95)90028-4
#'
#' @param chaos_data a data.frame all items for the Confusion, Hubbub, and Order Scale following the naming conventions described in Details
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @param extra_scale_cols a vector of character strings that begin with 'chaos' but are not scale items. Any columns in chaos_data that begin with 'chaos' but are not scale items must be included here. Default is empty vector.
#'
#' @return A dataset with subscale scores for the Confusion, Hubbub, and Order Scale 
#' @examples
#'
#' # scoring for the chaos with IDs
#' chaos_score_data <- score_chaos(chaos_data, id = 'ID')
#'
#'
#' \dontrun{
#' }
#'
#'
#'
#' @export

score_chaos <- function(chaos_data, base_zero = FALSE, id, session_id, extra_scale_cols = c()) {
  
  #### 1. Set up/initial checks #####
  
  # check that chaos_data exist and is a data.frame
  data_arg <- methods::hasArg(chaos_data)
  
  if (isTRUE(data_arg) & !is.data.frame(chaos_data)) {
    stop("chaos_data must be entered as a data.frame")
  } else if (isFALSE(data_arg)) {
    stop("chaos_data must set to the data.frame with amount consumed for each food item")
  }
  
  # check if id exists
  ID_arg <- methods::hasArg(id)
  
  if (isTRUE(ID_arg)){
    if (!(id %in% names(chaos_data))) {
      stop("variable name entered as id is not in chaos_data")
    }
  }
  
  # check if session_id exists
  sessionID_arg <- methods::hasArg(session_id)
  
  if (isTRUE(sessionID_arg)){
    if (!(id %in% names(chaos_data))) {
      stop("variable name entered as session_id is not in chaos_data")
    }
  }
  
  # check base_zero is logical
  if (!is.logical(base_zero)) {
    stop("base_zero arg must be logical (TRUE/FALSE)")
  }
  
  #### 2. Set Up Data #####
  
  # set up database for results create empty matrix
  chaos_score_dat <- data.frame(chaos_total = rep(NA, nrow(chaos_data)))
  
  if (isTRUE(ID_arg)) {
    if (isTRUE(sessionID_arg)) {
      chaos_score_dat <- data.frame(chaos_data[[id]], chaos_data[[session_id]], chaos_score_dat)
      names(chaos_score_dat)[1:2] <- c(id, session_id)
    } else {
      chaos_score_dat <- data.frame(chaos_data[[id]], chaos_score_dat)
      names(chaos_score_dat)[1] <- id
    }
  }
  
  # assign chaos scale items to chaos_items, excluding columns in extra_scale_cols
  chaos_items <- grep("^chaos", names(chaos_data), value = TRUE) %>% setdiff(extra_scale_cols)
  
  # remove underscore in column names for chaos_items
  names(chaos_data)[names(chaos_data) %in% chaos_items] <- gsub('chaos_', 'chaos', names(chaos_data)[names(chaos_data) %in% chaos_items])
  
  # remove underscore in chaos_items
  chaos_items <- gsub("chaos_", "chaos", chaos_items)
  
  # check range of data and print warnings
  min <- min(chaos_data[c(chaos_items)], na.rm = TRUE)
  max <- max(chaos_data[c(chaos_items)], na.rm = TRUE)
  
  if (isTRUE(base_zero)){
    if (min < 0 | max > 3) {
      warning("range in CHAOS data is outside expected range given base_zero = TRUE (expected range: 0-3). Scoring may be incorrect")
    } 
  } else {
    if (min < 1 | max > 4) {
      warning("range in CHAOS data is outside expected range given base_zero = FALSE (expected range: 1-4). Scoring may be incorrect")
    } 
  }
  
  # re-scale data
  chaos_data_edit <- chaos_data
  
  if (isTRUE(base_zero)){
    chaos_data_edit[chaos_items] <- sapply(chaos_items, function(x) chaos_data[[x]] + 1, simplify = TRUE)
  }

  # calculate reversed scores
  reverse_qs <- c("chaos3", "chaos5", "chaos6", "chaos8", "chaos9", "chaos10","chaos11", "chaos13")

  for (var in 1:length(reverse_qs)) {
    
    var_name <- reverse_qs[var]
    chaos_data_edit[[var_name]] <- ifelse(chaos_data_edit[[var_name]] == 1, 4, ifelse(chaos_data_edit[[var_name]] == 2, 3,  ifelse(chaos_data_edit[[var_name]] == 3, 2, ifelse(chaos_data_edit[[var_name]] == 4, 1, NA))))
    
  }

  ## Score
  score_vars <- c("chaos1", "chaos2", "chaos3", "chaos4", "chaos5", "chaos6", "chaos7", "chaos8", "chaos9", "chaos10", "chaos11", "chaos12", "chaos13", "chaos14","chaos15")
  
  chaos_score_dat[['chaos_total']] <- rowSums(chaos_data_edit[score_vars])
  
  
  #### 3. Clean Export/Scored Data #####

  
  ## merge raw responses with scored data
  if (isTRUE(ID_arg)){
    if (isTRUE(sessionID_arg)) {
      chaos_phenotype <- merge(chaos_data, chaos_score_dat, by = c(id, session_id))
    } else {
      chaos_phenotype <- merge(chaos_data, chaos_score_dat, by = id)
    }
    
    return(list(score_dat = as.data.frame(chaos_score_dat),
                bids_phenotype = as.data.frame(chaos_phenotype)))
  } else {
    return(list(score_dat = as.data.frame(chaos_score_dat)))
  }
}

