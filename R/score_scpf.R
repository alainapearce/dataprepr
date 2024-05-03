#' score_scpf: Scored data from the Structure and Control in Parent Feeding questionnaire
#'
#' This function scores the Structure and Control in Parent Feeding questionnaire and provides subscale scores for: Limit Exposure, Consistent feeding routines, Restriction, Pressure to eat
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'scpf#' or 'scpf_#' where # is the question number (1-34)
#' 3) All questions must have the numeric value for the choice: 0 - Never, 1 - Rarely, 2 - Sometimes, 3 - Often, 4 - Always if base_zero = TRUE or
#'                                                              1 - Never, 2 - Rarely, 3 - Sometimes, 4 - Often, 5 - Always if base_zero = FALSE
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Savage, J.S., Rollins, B.Y., Kugler, K.C. et al. Development of a theory-based questionnaire to assess structure and control in parent feeding (SCPF). Int J Behav Nutr Phys Act 14, 9 (2017). https://doi.org/10.1186/s12966-017-0466-2
#' 
#' @param scpf_data a data.frame all items for the Structure and Control in Parent Feeding questionnaire following the naming conventions described above
#' @param extra_scale_cols a vector of character strings that begin with 'scpf' but are not scale items. Any columns in scpf_data that begin with 'scpf' but are not scale items must be included here. Default is empty vector.
#' @inheritParams score_bes
#'
#' @return A dataset with subscale scores for the Structure and Control in Parent Feeding questionnaire
#' @examples
#'
#' # scoring for the scpf with IDs
#' scpf_score_data <- score_scpf(scpf_data, id = 'ID', base_zero = TRUE)
#'
#' \dontrun{
#' }
#'
#'
#' @export

score_scpf <- function(scpf_data, base_zero = TRUE, id, extra_scale_cols = c()) {
  
  #### 1. Set up/initial checks #####
  
  # check that scpf_data exist and is a data.frame
  data_arg <- methods::hasArg(scpf_data)
  
  if (isTRUE(data_arg) & !is.data.frame(scpf_data)) {
    stop("scpf_data must be entered as a data.frame")
  } else if (isFALSE(data_arg)) {
    stop("scpf_data must set to a data.frame")
  }
  
  # check if id exists
  ID_arg <- methods::hasArg(id)
  
  if (isTRUE(ID_arg)){
    if (!(id %in% names(scpf_data))) {
      stop("variable name entered as id is not in scpf_data")
    }
  }
  
  #### 2. Set Up Data #####
  
  # set up database for results create empty matrix
  scpf_score_dat <-
    data.frame(
      scpf_limit_exp = rep(NA, nrow(scpf_data)),
      scpf_consistent = rep(NA, nrow(scpf_data)),
      scpf_restriction = rep(NA, nrow(scpf_data)),
      scpf_pressure = rep(NA, nrow(scpf_data))
    )
  
  if (isTRUE(ID_arg)) {
    scpf_score_dat <- data.frame(scpf_data[[id]], scpf_score_dat)
    names(scpf_score_dat)[1] <- id
  }
  
  # assign scpf scale items to scpf_items, excluding columns in extra_scale_cols
  scpf_items <- grep("^scpf", names(scpf_data), value = TRUE) %>% setdiff(extra_scale_cols)
  
  # remove underscore in column names for scpf_items
  names(scpf_data)[names(scpf_data) %in% scpf_items] <- gsub('scpf_', 'scpf', names(scpf_data)[names(scpf_data) %in% scpf_items])
  
  # remove underscore in scpf_items
  scpf_items <- gsub("scpf_", "scpf", scpf_items)
  
  # re-scale data
  scpf_data_edit <- scpf_data

  if (isFALSE(base_zero)){
    scpf_data_edit[scpf_items] <- sapply(scpf_items, function(x) scpf_data[[x]] - 1, simplify = TRUE)
  }

  # calculate reversed scores
  reverse_qs <- c("scpf2", "scpf4", "scpf6", "scpf8", "scpf13", "scpf16", "scpf22", "scpf31")

  for (var in 1:length(reverse_qs)) {
    var_name <- reverse_qs[var]
    
    scpf_data_edit[[var_name]] <- ifelse(is.na(scpf_data_edit[[var_name]]), NA,
                                        ifelse(scpf_data_edit[[var_name]] == 0, 4,
                                               ifelse(scpf_data_edit[[var_name]] == 1, 3,
                                                      ifelse(scpf_data_edit[[var_name]] == 3, 1,
                                                             ifelse(scpf_data_edit[[var_name]] == 4, 0, 
                                                                    ifelse(scpf_data_edit[[var_name]] == 2, 2, NA))))))
  }

  
  ## Score Subscales

  # Limit Exposure
  exposure_vars <- c("scpf1", "scpf2", "scpf3", "scpf4", "scpf6", "scpf8", "scpf13", "scpf22", "scpf31", "scpf32", "scpf33")
  scpf_score_dat[["scpf_limit_exp"]] <- rowMeans(scpf_data_edit[exposure_vars], na.rm = TRUE)

  # Consistent feeding routines
  consistent_vars <- c("scpf5", "scpf7", "scpf15", "scpf16", "scpf20", "scpf23", "scpf25", "scpf26", "scpf27", "scpf28", "scpf30")
  scpf_score_dat[["scpf_consistent"]] <- rowMeans(scpf_data_edit[consistent_vars], na.rm = TRUE)

  # Restriction
  restriction_vars <- c("scpf9", "scpf10", "scpf12", "scpf14", "scpf34")
  scpf_score_dat[["scpf_restriction"]] <- rowMeans(scpf_data_edit[restriction_vars], na.rm = TRUE)
  
  # Pressure to eat
  pressure_vars <- c("scpf17", "scpf18", "scpf19", "scpf21", "scpf24", "scpf29")
  scpf_score_dat[["scpf_pressure"]] <- rowMeans(scpf_data_edit[pressure_vars], na.rm = TRUE)
  

  #### 3. Clean Export/Scored Data #####
  ## merge raw responses with scored data
  if (isTRUE(ID_arg)){
    scpf_phenotype <- merge(scpf_data, scpf_score_dat, by = id)
    
    return(list(score_dat = as.data.frame(scpf_score_dat),
                bids_phenotype = as.data.frame(scpf_phenotype)))
  } else {
    return(list(score_dat = as.data.frame(scpf_score_dat)))
  }
}

