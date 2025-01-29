#' score_fmcb: Scored data from the Feeding to Manage Child Behavior Questionnaire
#'
#' This function scores the Feeding to Manage Child Behavior Questionnaire and provides subscale scores for the following behaviors: food to soothe and food as reward
#' 
#' For data to be scored correctly, the data must be prepared according to the following criteria: \cr
#' \itemize{
#'  \item{The data must include all individual questionnaire items}
#'  \item{The columns/variables must match the following naming convention: 'fmcb#' or 'fmcb_#' where # is the question number (1-10)}
#'  \item{All questionnaire responses must be a numeric value ranging from 0-4 (base_zero = TRUE) or 1-5 (base_zero = FALSE) where: }
#'  \itemize{
#'     \item{For base_zero = TRUE: 0 = Never; 1 = Rarely; 2 = Sometimes; 3 = Often; 4 = Always}
#'     \item{For base_zero = FALSE: 1 = Never; 2 = Rarely; 3 = Sometimes; 4 = Often; 5 = Always}
#'   }
#'  \item{Missing values must be coded as NA}
#' }
#' \cr
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Primary References for the Feeding to Manage Child Behavior Questionnaire and Scoring:
#' Savage JS, Ruggiero CF, Eagleton SG, Marini ME, Harris HA. The feeding to Manage Child Behavior Questionnaire: Development of a tool to measure' non-nutritive feeding practices in low income families with preschool-aged children. Appetite. 2022 Feb 1;169:105849. doi: 10.1016/j.appet.2021.105849. PMID: 34883138; PMCID: PMC8748389. (\href{https://pubmed.ncbi.nlm.nih.gov/34883138/}{PubMed})
#'
#' @param fmcb_data a data.frame all items for the Feeding to Manage Child Behavior Questionnaire following the naming conventions described above
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @param extra_scale_cols a vector of character strings that begin with 'fmcb' but are not scale items. Any columns in fmcb_data that begin with 'fmcb' but are not scale items must be included here. Default is empty vector.
#' @return If 'id' argument is used, returns a list with 2 dataframes: (1) bids_phenotype (contains input fmcb_data [values identical to input, underscores removed from fmcb items col names, if they existed] and FMCB scores) and (2) score_dat (contains FMCB subscale scores only). If 'id' argument is not used, returns a list with score_dat dataframe only.
#' @examples
#'
#' # scoring for the fmcb Scale with IDs
#' fmcb_score_data <- score_fmcb(fmcb_data, base_zero = TRUE, id = 'ID')
#' 
#' 
#' @export

score_fmcb <- function(fmcb_data, base_zero = TRUE, id, session_id, extra_scale_cols = c()) {

    #### 1. Set up/initial checks #####

    # check that fmcb_data exist and is a data.frame
    data_arg <- methods::hasArg(fmcb_data)

    if (isTRUE(data_arg) & !is.data.frame(fmcb_data)) {
        stop('fmcb_data must be entered as a data.frame')
    } else if (isFALSE(data_arg)) {
        stop('fmcb_data must set to the data.frame with amount consumed for each food item')
    }

    # check if id exists
    ID_arg <- methods::hasArg(id)

    if (isTRUE(ID_arg)){
        if (!(id %in% names(fmcb_data))) {
            stop('variable name entered as id is not in fmcb_data')
        }
    }
    
    # check if session_id exists
    sessionID_arg <- methods::hasArg(session_id)
    
    if (isTRUE(sessionID_arg)){
      if (!(id %in% names(fmcb_data))) {
        stop("variable name entered as session_id is not in fmcb_data")
      }
    }

    # check base_zero is logical
    if (!is.logical(base_zero)) {
      stop("base_zero arg must be logical (TRUE/FALSE)")
    }
    
    #### 2. Set Up Data #####

    # set up database for results create empty matrix
    fmcb_score_dat <- data.frame(fmcb_score = rep(NA, nrow(fmcb_data)))

    if (isTRUE(ID_arg)) {
      if (isTRUE(sessionID_arg)) {
        fmcb_score_dat <- data.frame(fmcb_data[[id]], fmcb_data[[session_id]], fmcb_score_dat)
        names(fmcb_score_dat)[1:2] <- c(id, session_id)
      } else {
        fmcb_score_dat <- data.frame(fmcb_data[[id]], fmcb_score_dat)
        names(fmcb_score_dat)[1] <- id
      }
    }

    # assign fmcb scale items to fmcb_items, excluding columns in extra_scale_cols
    fmcb_items <- grep("^fmcb", names(fmcb_data), value = TRUE) %>% setdiff(extra_scale_cols)
    
    # remove underscore in column names for fmcb_items
    names(fmcb_data)[names(fmcb_data) %in% fmcb_items] <- gsub('fmcb_', 'fmcb', names(fmcb_data)[names(fmcb_data) %in% fmcb_items])
    
    # remove underscore in fmcb_items
    fmcb_items <- gsub("fmcb_", "fmcb", fmcb_items)
    
    # check range of data and print warnings
    min <- min(fmcb_data[c(fmcb_items)], na.rm = TRUE)
    max <- max(fmcb_data[c(fmcb_items)], na.rm = TRUE)
    
    if (isTRUE(base_zero)){
      if (min < 0 | max > 4) {
        warning("range in FMCB data is outside expected range given base_zero = TRUE (expected range: 0-4). Scoring may be incorrect")
      } 
    } else {
      if (min < 1 | max > 5) {
        warning("range in FMCB data is outside expected range given base_zero = FALSE (expected range: 1-5). Scoring may be incorrect")
      } 
    }
    
    # re-scale data
    fmcb_data_edit <- fmcb_data
    
    if (isFALSE(base_zero)){
      fmcb_data_edit[fmcb_items] <- sapply(fmcb_items, function(x) fmcb_data_edit[[x]] - 1, simplify = TRUE)
    }
    
    ## Score
    fmcb_score_dat[['fmcb_fts']] <- rowMeans(fmcb_data_edit[c('fmcb1', 'fmcb4', 'fmcb6', 'fmcb9', 'fmcb10')])
    fmcb_score_dat[['fmcb_far']] <- rowMeans(fmcb_data_edit[c('fmcb2', 'fmcb3', 'fmcb7', 'fmcb8')])
    
 
    #### 3. Clean Export/Scored Data #####
    ## round data
    if (isTRUE(ID_arg)){
        fmcb_score_dat[2:ncol(fmcb_score_dat)] <- round(fmcb_score_dat[2:ncol(fmcb_score_dat)], digits = 3)
    } else {
        fmcb_score_dat <- round(fmcb_score_dat, digits = 3)
    }

    ## merge raw responses with scored data
    if (isTRUE(ID_arg)){
      if (isTRUE(sessionID_arg)) {
        fmcb_phenotype <- merge(fmcb_data, fmcb_score_dat, by = c(id, session_id))
      } else {
        fmcb_phenotype <- merge(fmcb_data, fmcb_score_dat, by = id)
      }
      
      return(list(score_dat = as.data.frame(fmcb_score_dat),
                  bids_phenotype = as.data.frame(fmcb_phenotype)))
    } else {
      return(list(score_dat = as.data.frame(fmcb_score_dat)))
    }
    
}

