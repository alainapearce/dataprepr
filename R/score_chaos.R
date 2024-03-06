#' score_chaos: INCOMPLETE FUNCTION -- Scored data from the Confusion, Hubbub, and Order Scale
#'
#' This function scores the Confusion, Hubbub, and Order Scale and provides XXX
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'chaos#' where # is the question number (1-24).
#' 3) Questions must have the numeric value for the choices: 1 - Very much like your own home, 2 - Somewhat like your own home, 3 - A little bit like your own home, 4 - Not at all like your own home
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' need to add
#'
#' @param chaos_data a data.frame all items for the Confusion, Hubbub, and Order Scale following the naming conventions described above
#' @inheritParams score_bes
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

score_chaos <- function(chaos_data, score_base = TRUE, id) {
  
  #### 1. Set up/initial checks #####
  
  # check that bisbas_data exist and is a data.frame
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
  
  #### 2. Set Up Data #####
  
  # set up database for results create empty matrix
  chaos_score_dat <- data.frame()
  
  
  if (isTRUE(ID_arg)) {
    chaos_score_dat <- data.frame(chaos_data[[id]], chaos_score_dat)
    names(chaos_score_dat)[1] <- id
  }
  
  # remove underscore if in column names
  names(chaos_data) <- gsub('chaos_', 'chaos', names(chaos_data))
  
  # get primary questions
  q_numbers <- seq(1, 15)
  chaos_primary_qs <- paste0("chaos", q_numbers)
  
  # re-scale data
  chaos_data_edit <- chaos_data
  
  if (isTRUE(score_base)){
    chaos_data_edit[chaos_primary_qs] <- sapply(chaos_primary_qs, function(x) chaos_data[[x]] + 1, simplify = TRUE)
  }
  
  # # calculate reversed scores
  # reverse_qs <- c("", "")
  # 
  # for (var in 1:length(reverse_qs)) {
  #   var_name <- reverse_qs[var]
  #   reverse_name <- paste0(var_name, "_rev")
  #   
  #   chaos_data_edit[[reverse_name]] <- ifelse(is.na(chaos_data_edit[[var_name]]), NA, ifelse(chaos_data_edit[[var_name]] == 1, 4, ifelse(chaos_data_edit[[var_name]] == 2, 3,  ifelse(chaos_data_edit[[var_name]] == 3, 2, 1))))
  # }
  # 
  # ## Score
  # 
  # #### 3. Clean Export/Scored Data #####
  # 
  
  ## merge raw responses with scored data
  if (isTRUE(ID_arg)){
    chaos_phenotype <- merge(chaos_data, chaos_score_dat, by = id)
    
    return(list(score_dat = as.data.frame(chaos_score_dat),
                bids_phenotype = as.data.frame(chaos_phenotype)))
  } else {
    return(list(score_dat = as.data.frame(chaos_score_dat)))
  }
}

