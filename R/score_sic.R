#' score_sic: Scored data from the Stress in Children Questionnaire
#'
#' This function scores the Stress in Children Questionnaire and provides subscale scores for the following behaviors: Well Being, Distress, Social Support, and the Global Mean Score.
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'sic#' or 'sic_#' where # is the question number (1-21)
#' 3) All questions must have the numeric value for the choice: 1 - Never, 2 - Sometimes, 3 - Often, 4 - Very Often
#'
#' Note, as long as variable names match those listed, the dataset can include other variables. Up to 2 missing responses are allowed
#'
#' @references
#' 1. Osika W, Friberg P, Wahrborg P. A new short self-rating questionnaire to assess stress in children. Int J Behav Med. 2007;14(2):108-117. doi:10.1007/BF03004176 (\href{https://pubmed.ncbi.nlm.nih.gov/17926439/}{PubMed})
#'
#' @param sic_data a data.frame all items for the Child Behavior Questionnaire following the naming conventions described above
#' @inheritParams score_bes
#' @inheritParams score_bes
#'
#' @return A dataset with subscale scores for the Stress in Chidlren Questionnaire
#' @examples
#'
#' # scoring for the sic with IDs
#' sic_score_data <- score_sic(sic_data, id = 'ID')
#'
#' \dontrun{
#' }
#'
#' @seealso Raw data from Qualtrics was processed using the following script: \code{\link{util_fbs_parent_v2dat}}
#'
#'
#' @export

score_sic <- function(sic_data, score_base = TRUE, id) {

    #### 1. Set up/initial checks #####

    # check that sic_data exist and is a data.frame
    data_arg <- methods::hasArg(sic_data)

    if (isTRUE(data_arg) & !is.data.frame(sic_data)) {
        stop("sic_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("sic_data must set to the data.frame with amount consumed for each food item")
    }

    # check if id exists
    ID_arg <- methods::hasArg(id)

    if (isTRUE(ID_arg)){
        if (!(id %in% names(sic_data))) {
            stop("variable name entered as id is not in sic_data")
        }
    }

    #### 2. Set Up Data #####

    # set up database for results

    ## create empty matrix
    sic_score_dat <- data.frame(sic_wellbeing = rep(NA, nrow(sic_data)), sic_distress = rep(NA, nrow(sic_data)), sic_socialsupport = rep(NA, nrow(sic_data)), sic_grand_mean = rep(NA, nrow(sic_data)))

    if (isTRUE(ID_arg)) {
        sic_score_dat <- data.frame(sic_data[[id]], sic_score_dat)
        names(sic_score_dat)[1] <- id
    }

    # re-scale data
    sic_data_edit <- sic_data
    
    if (isTRUE(score_base)){
      sic_data_edit[2:17] <- sapply(names(sic_data)[2:17], function(x) sic_data[[x]] + 1, simplify = TRUE)
    }

    ## Score Subscales

    # Well Being
    activity_vars <- c("sic1", "sic12", "sic18_rev", "sic22", "sic50_rev", "sic85",
        "sic93_rev")
    sic_score_dat[["sic_activity"]] <- rowMeans(sic_data[activity_vars], na.rm = TRUE)

    # Distress
    anger_vars <- c("sic2", "sic14", "sic30", "sic40", "sic61_rev", "sic87")
    sic_score_dat[["sic_anger"]] <- rowMeans(sic_data[anger_vars], na.rm = TRUE)

    # Social Support
    approach_vars <- c("sic6", "sic15", "sic46", "sic58", "sic90_rev", "sic92_rev")
    sic_score_dat[["sic_approach"]] <- rowMeans(sic_data[approach_vars], na.rm = TRUE)

    # Grand Mean
    attention_vars <- c("sic16_rev", "sic21_rev", "sic62", "sic71", "sic84_rev",
        "sic89")
    sic_score_dat[["sic_attention"]] <- rowMeans(sic_data[attention_vars], na.rm = TRUE)

    #### 3. Clean Export/Scored Data #####
    ## round data
    if (isTRUE(ID_arg)){
        sic_score_dat[2:ncol(sic_score_dat)] <- round(sic_score_dat[2:ncol(sic_score_dat)], digits = 3)
    } else {
        sic_score_dat <- round(sic_score_dat, digits = 3)
    }

    ## round data
    if (isTRUE(ID_arg)){
      hfi_score_dat[2:ncol(hfi_score_dat)] <- round(hfi_score_dat[2:ncol(hfi_score_dat)], digits = 3)
    } else {
      hfi_score_dat <- round(hfi_score_dat, digits = 3)
    }
    
    ## merge raw responses with scored data
    if (isTRUE(ID_arg)){
      hfi_phenotype <- merge(hfi_data, hfi_score_dat, by = id)
      
      return(list(score_dat = as.data.frame(hfi_score_dat),
                  bids_phenotype = as.data.frame(hfi_phenotype)))
    } else {
      return(list(score_dat = as.data.frame(hfi_score_dat)))
    }

    return(sic_score_dat)
}

