#' score_psi: Score data from the Parenting Style Inventory-II
#'
#' This function scores ONLY the Responsiveness subscale of the Parenting Style Inventory-II. Questions were asked for both mom AND dad.
#'
#' Note: for the Food and Brain Study, questions 1 and 2 were re-phrased so no reverse scoring was needed. Reverse scoring is included in the script for other studies that use the published question phrasing.
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'psi_resp_mom#' and 'psi_resp_dad#' where # is the question number (1-5)
#' 3) All questions must have the numeric value for the choice: 1 - Strongly Disagree, 2 - Disagree, 3 - I am in between, 4 - Agree, 5 - Strongly Agree
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Darling N, Toyokawa T. Construction and validation of the parenting style inventory II (PSI-II). Unpublished manuscript. 1997;89. (https://www.researchgate.net/profile/Nancy-Darling/publication/341909949_Construction_and_Validation_of_the_Parenting_Style_Inventory_II_PSI-II/links/5ed9027792851c9c5e7bc63d/Construction-and-Validation-of-the-Parenting-Style-Inventory-II-PSI-II.pdf)
#'
#' @param psi_data a data.frame all items for the Parenting Style Inventory-II following the naming conventions described above
#' @param study a string indicating which study collected the data. Currently, only option and default is 'fbs'. This parameter is included so this script can be adapted for future studies that use published question 1 and 2 phrasing that need to be reversed scored.
#' @inheritParams fbs_intake
#'
#' @return A dataset with a score for the Parenting Style Inventory-II
#' @examples
#'
#' # scoring for the psi with IDs
#' psi_score_data <- score_psi(psi_data, study = 'fbs', parID = 'ID')
#'
#' \dontrun{
#' # won't run without parameter study
#' psi_score_data <- score_psi(psi_data, parID = 'ID')
#' }
#'
#' @seealso Raw data from Qualtrics was processed using the following script: \code{\link{util_fbs_child_v4dat}} and \code{\link{util_fbs_child_v4dat_home}}
#'
#'
#' @export

score_psi <- function(psi_data, study, parID) {

    #### 1. Set up/initial checks #####

    # check that psi_data exist and is a data.frame
    data_arg <- methods::hasArg(psi_data)

    if (isTRUE(data_arg) & !is.data.frame(psi_data)) {
        stop("psi_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("psi_data must set to the data.frame with amount consumed for each food item")
    }

    # check that study exists and is a string
    study_arg <- methods::hasArg(study)

    if (isTRUE(study_arg)) {
        if (study != "fbs" & study != "FBS") {
            stop("only option currently available for study is 'fbs'. If have an alternative study you wish to use this script for please contact package maintainers to get your study added")
        }
    }

    # check if parID exists
    ID_arg <- methods::hasArg(parID)

    if (isTRUE(ID_arg)){
        if (!(parID %in% names(psi_data))) {
            stop("variable name entered as parID is not in psi_data")
        }
    }

    #### 2. Set Up Data #####

    # set up database for results create empty matrix
    psi_score_dat <- data.frame(psi_score_mom = rep(NA, nrow(psi_data)), psi_score_dad = rep(NA, nrow(psi_data)))

    if (isTRUE(ID_arg)) {
        psi_score_dat <- data.frame(psi_data[[parID]], psi_score_dat)
        names(psi_score_dat)[1] <- parID
    }

    # set up labels for psi_score_dat
    psi_score_dat_labels <- lapply(psi_score_dat, function(x) attributes(x)$label)

    # reverse scoring
    if (study != 'fbs' & study != 'FBS'){
        reverse_vars <- c('psi_resp_dad1', 'psi_resp_dad2', 'psi_resp_mom1', 'psi_resp_mom2')

        for (v in 1:length(reverse_vars)){
            var_name <- reverse_vars[v]

            psi_data[[var_name]] <- ifelse(is.na(psi_data[[var_name]]), NA, ifelse(psi_data[[var_name]] == 1, 5, ifelse(psi_data[[var_name]] == 2, 4, ifelse(psi_data[[var_name]] == 4, 2, ifelse(psi_data[[var_name]] == 5, 1, 3)))))
        }
    }


    # Mom score
    mom_vars <- c('psi_resp_mom1', 'psi_resp_mom2', 'psi_resp_mom3', 'psi_resp_mom4', 'psi_resp_mom5')
    psi_score_dat[["psi_score_mom"]] <- rowMeans(psi_data[mom_vars])

    ## add labels to data
    psi_score_dat_labels[["psi_score_mom"]] <- "PSI Responsiveness Score for Mom"

    # Dad score
    dad_vars <- c('psi_resp_dad1', 'psi_resp_dad2', 'psi_resp_dad3', 'psi_resp_dad4', 'psi_resp_dad5')
    psi_score_dat[["psi_score_dad"]] <- rowMeans(psi_data[dad_vars])

    ## add labels to data
    psi_score_dat_labels[["psi_score_dad"]] <- "PSI Responsiveness Score for Dad"

    #### 3. Clean Export/Scored Data #####

    ## make sure the variable labels match in the dataset
    psi_score_dat = sjlabelled::set_label(psi_score_dat, label = matrix(unlist(psi_score_dat_labels,
        use.names = FALSE)))

    return(psi_score_dat)
}

