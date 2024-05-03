#' score_pmum: Scored data from the Problematic Media Use Measure
#'
#' This function scores the Problematic Media Use Measure
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'pmum#' or 'pmum_#' where # is the question number (1-27).
#' 3) Questions must have the numeric value for the choices: 1, Never | 2, Rarely | 3, Sometimes | 4, Often | 5, Always
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Domoff SE, Harrison K, Gearhardt AN, Gentile DA, Lumeng JC, Miller AL. Development and Validation of the Problematic Media Use Measure: A Parent Report Measure of Screen Media "Addiction" in Children. Psychol Pop Media Cult. 2019 Jan;8(1):2-11. doi: 10.1037/ppm0000163. Epub 2017 Nov 16. PMID: 30873299; PMCID: PMC6411079.
#'
#' @param pmum_data a data.frame all items for the Problematic Media Use Measure following the naming conventions described above
#' @param extra_scale_cols a vector of character strings that begin with 'pmum' but are not scale items. Any columns in pmum_data that begin with 'pmum' but are not scale items must be included here. Default is empty vector.
#' @inheritParams score_bes
#'
#' @return A dataset with total score for the Problematic Media Use Measure
#' @examples
#'
#' # scoring for the pmum with IDs
#' pmum_score_data <- score_pmum(pmum_data, id = 'ID')
#'
#'
#' \dontrun{
#' }
#'
#' @export

score_pmum <- function(pmum_data, base_zero = TRUE, id, extra_scale_cols = c()) {
  
  #### 1. Set up/initial checks #####
  
  # check that pmum_data exist and is a data.frame
  data_arg <- methods::hasArg(pmum_data)
  
  if (isTRUE(data_arg) & !is.data.frame(pmum_data)) {
    stop("pmum_data must be entered as a data.frame")
  } else if (isFALSE(data_arg)) {
    stop("pmum_data must set to the data.frame with amount consumed for each food item")
  }
  
  # check if id exists
  ID_arg <- methods::hasArg(id)
  
  if (isTRUE(ID_arg)){
    if (!(id %in% names(pmum_data))) {
      stop("variable name entered as id is not in pmum_data")
    }
  }
  
  #### 2. Set Up Data #####
  
  # set up database for results create empty matrix
  pmum_score_dat <- data.frame(pmum_total = rep(NA, nrow(pmum_data)))
  
  
  if (isTRUE(ID_arg)) {
    pmum_score_dat <- data.frame(pmum_data[[id]], pmum_score_dat)
    names(pmum_score_dat)[1] <- id
  }
  
  # assign pmum scale items to pmum_items, excluding columns in extra_scale_cols
  pmum_items <- grep("^pmum", names(pmum_data), value = TRUE) %>% setdiff(extra_scale_cols)
  
  # remove underscore in column names for pmum_items
  names(pmum_data)[names(pmum_data) %in% pmum_items] <- gsub('pmum_', 'pmum', names(pmum_data)[names(pmum_data) %in% pmum_items])
  
  # remove underscore in pmum_items
  pmum_items <- gsub("pmum_", "pmum", pmum_items)
  
  # re-scale data
  pmum_data_edit <- pmum_data
  
  if (isTRUE(base_zero)){
    pmum_data_edit[pmum_items] <- sapply(pmum_items, function(x) pmum_data[[x]] + 1, simplify = TRUE)
  }
  
  
  ## Score 
  
  pmum_score_dat[["pmum_total"]] <- rowMeans(pmum_data_edit[c(pmum_items)])
  
  #### 3. Clean Export/Scored Data #####
  
  ## merge raw responses with scored data
  if (isTRUE(ID_arg)){
    pmum_phenotype <- merge(pmum_data, pmum_score_dat, by = id)
    
    return(list(score_dat = as.data.frame(pmum_score_dat),
                bids_phenotype = as.data.frame(pmum_phenotype)))
  } else {
    return(list(score_dat = as.data.frame(pmum_score_dat)))
  }
}

