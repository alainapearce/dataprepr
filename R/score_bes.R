#' score_bes: Scored data from the Binge Eating Scale
#'
#' This function scores the Binge Eating Scale
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'bes#' or 'bes_#' where # is the question number (1-16)
#' 3) This script will apply specific scoring transformations that are specific to each question. For example, the first 2 statements are reset to 0 for question 1 but not for question 2.
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Gormally, J., Black, S., Daston, S., & Rardin, D. (1982). The assessment of binge eating severity among obese persons. Addictive Behaviors, 7(1), 47–55. https://doi.org/10.1016/0306-4603(82)90024-7  (\href{https://pubmed.ncbi.nlm.nih.gov/7080884/}{PubMed})
#'
#' Timmerman, G. M. (1999). Binge Eating Scale: Further Assessment of Validity and Reliability. Journal of Applied Biobehavioral Research, 4(1), 1–12. https://doi.org/10.1111/j.1751-9861.1999.tb00051.x
#'
#' @param bes_data a data.frame all items for the Binge Eating Scale following the naming conventions described above
#' @param pna value used when participant prefers not to answer/elects to skip
#' @param score_base the smallest value assigned to a choice is 0 (i.e., range 0-3). Default = TRUE.
#' @param id (optional) name of participant ID column in bes_data. If included the output dataset will be matched by id, if not included the output dataset will be in the order of bes_data but will have no participant identifier. Required to get the phenotype dataset (raw data merged with scores.)
#'
#' @return A dataset with total score for the Binge Eating Scale
#' @examples
#'
#' # scoring for the bes with IDs
#' bes_score_data <- score_bes(bes_data, id = 'ID')
#'
#' \dontrun{
#' }
#'
#'
#' @export

score_bes <- function(bes_data, score_base = TRUE, pna = NA, id) {
  
  #### 1. Set up/initial checks #####
  
  # check that bes_data exist and is a data.frame
  data_arg <- methods::hasArg(bes_data)
  
  if (isTRUE(data_arg) & !is.data.frame(bes_data)) {
    stop('bes_data must be entered as a data.frame')
  } else if (isFALSE(data_arg)) {
    stop('bes_data must set to the data.frame with amount consumed for each food item')
  }
  
  # check if id exists
  ID_arg <- methods::hasArg(id)
  
  if (isTRUE(ID_arg)){
    if (!(id %in% names(bes_data))) {
      stop('variable name entered as id is not in bes_data')
    }
  }
  
  #### 2. Set Up Data #####
  
  # set up database for results create empty matrix
  bes_score_dat <- data.frame(bes_total = rep(NA, nrow(bes_data)))
  
  if (isTRUE(ID_arg)) {
    bes_score_dat <- data.frame(bes_data[[id]], bes_score_dat)
    names(bes_score_dat)[1] <- id
  }
  
  # remove underscore if in column names
  names(bes_data) <- gsub('bes_', 'bes', names(bes_data))
  
  # get primary questions
  q_numbers <- seq(1, 16)
  bes_primary_qs <- paste0("bes", q_numbers)

  # re-scale data
  bes_data_edit <- bes_data
  
  if (isFALSE(score_base)){
    bes_data_edit[bes_primary_qs] <- sapply(bes_primary_qs, function(x) bes_data[[x]] - 1, simplify = TRUE)
    
    if (is.numeric(pna)){
      pna <- pna - 1
    }
  }
  
  # calculate question - specific scoring values
  
  # reset pna value to NA
  bes_data_edit[bes_primary_qs] <- sapply(bes_primary_qs, function(x) ifelse(bes_data[[x]] == pna, NA, bes_data[[x]]), simplify = TRUE)
  
  # custom scoring by question
  bes_data_edit[['bes1']] <- ifelse(is.na(bes_data_edit[['bes1']]), NA, ifelse(bes_data_edit[['bes1']] < 2, 0, ifelse(bes_data_edit[['bes1']] == 2, 1, 3)))
  
  bes_data_edit[['bes3']] <- ifelse(is.na(bes_data_edit[['bes3']]), NA, ifelse(bes_data_edit[['bes3']] > 1, 3, bes_data_edit[['bes3']]))
  
  bes_data_edit[['bes4']] <- ifelse(is.na(bes_data_edit[['bes4']]), NA, ifelse(bes_data_edit[['bes4']] < 3, 0, 2))
  
  bes_data_edit[['bes6']] <- ifelse(is.na(bes_data_edit[['bes6']]), NA, ifelse(bes_data_edit[['bes6']] == 2, 3, bes_data_edit[['bes6']]))
  
  bes_data_edit[['bes7']] <- ifelse(is.na(bes_data_edit[['bes7']]), NA, ifelse(bes_data_edit[['bes7']] == 1, 2, ifelse(bes_data_edit[['bes7']] == 2, 3, bes_data_edit[['bes7']])))
  
  bes_data_edit[['bes13']] <- ifelse(is.na(bes_data_edit[['bes13']]), NA, ifelse(bes_data_edit[['bes13']] < 2, 0, bes_data_edit[['bes13']]))
  
  ## Score
  
  # Total Score
  bes_score_dat[['bes_total']] <- rowSums(bes_data_edit[bes_primary_qs])
  
  #### 3. Clean Export/Scored Data ####
  
  ## merge raw responses with scored data
  if (isTRUE(ID_arg)){
    bes_phenotype <- merge(bes_data_edit, bes_score_dat, by = id)
    
    return(list(score_dat = as.data.frame(bes_score_dat),
                bids_phenotype = as.data.frame(bes_phenotype)))
  } else {
    return(list(score_dat = as.data.frame(bes_score_dat)))
  }
}

