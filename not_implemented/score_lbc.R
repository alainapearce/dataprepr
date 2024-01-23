#' score_lbc: Scored data from the Lifestyle Behavior Checklist
#'
#' This function scores the Lifestyle Behavior Checklist and provides subscale scores for the following behaviors: Food-Related Misbehavior, Overeating, Emotions Related to Overweight, and Physical Activity. NOTE: for FBS, caution should be used in interpreting the Food-Related Misbehavior and Overeating subscales as this study is missing 3 (of 7) and 2 (of 7) questions for the respective subscales.
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'lbc#' where # is the question number (1-25; note - script will allow only 6-25 for study = 'fbs')
#' 3) All questions must have the numeric value for the choice: 1 - Not At All, 2 - A Little (-), 3 - A Little (+), 4 - Somewhat, 5 - Much (-), 6 - Much (+), 7 - Very Much
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' West F, Sanders MR. The Lifestyle Behaviour Checklist: A measure of weight-related problem behaviour in obese children. International Journal of Pediatric Obesity. 2009;4(4):266-273. doi:10.3109/17477160902811199
#'
#' Subscales:
#' West F, Morawska A, Joughin K. The Lifestyle Behaviour Checklist: evaluation of the factor structure. Child: Care, Health and Development. 2010;36(4):508-515. doi:10.1111/j.1365-2214.2010.01074.x (\href{https://pubmed.ncbi.nlm.nih.gov/20337641/}{PubMed})
#'
#' @param lbc_data a data.frame all items for the Lifestyle Behavior Checklist following the naming conventions described above
#' @param study a string indicating which study collected the data. Currently, only option and default is 'fbs'. This parameter is included so this script can be adapted for future studies that collect all questions - FBS skipped first 5 questions
#' @inheritParams fbs_intake
#'
#' @return A dataset with subscale scores for the Lifestyle Behavior Checklist
#' @examples
#'
#' # scoring for the lbc with IDs
#' lbc_score_data <- score_lbc(lbc_data, parID = 'ID')
#'
#' study specified
#' lbc_score_data <- score_lbc(lbc_data, study = 'fbs', parID = 'ID')
#'
#' \dontrun{
#' }
#'
#' @seealso Raw data from Qualtrics was processed using the following script: \code{\link{util_fbs_parent_v3dat}}
#'
#'
#' @export

score_lbc <- function(lbc_data, study = "fbs", parID) {

    #### 1. Set up/initial checks #####

    # check that lbc_data exist and is a data.frame
    data_arg <- methods::hasArg(lbc_data)

    if (isTRUE(data_arg) & !is.data.frame(lbc_data)) {
        stop("lbc_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("lbc_data must set to the data.frame with amount consumed for each food item")
    }

    # check that study exists and is a string
    study_arg <- methods::hasArg(study)

    if (isTRUE(study_arg)) {
        if (study != "fbs" & study != "FBS") {
            stop("only option currently available for study is 'fbs'. If have an alternative
                 study you wish to use this script for please contact package maintainers to
                 get your study added")
        }
    }

    # check if parID exists
    ID_arg <- methods::hasArg(parID)

    if (isTRUE(ID_arg)){
        if (!(parID %in% names(lbc_data))) {
            stop("variable name entered as parID is not in lbc_data")
        }
    }

    #### 2. Set Up Data #####

    # set up database for results create empty matrix
    lbc_score_dat <- data.frame(lbc_misbeh = rep(NA, nrow(lbc_data)), lbc_overeat = rep(NA, nrow(lbc_data)), lbc_em_overweight = rep(NA, nrow(lbc_data)), lbc_pa = rep(NA, nrow(lbc_data)), lbc_total = rep(NA, nrow(lbc_data)))

    if (isTRUE(ID_arg)) {
        lbc_score_dat <- data.frame(lbc_data[[parID]], lbc_score_dat)
        names(lbc_score_dat)[1] <- parID
    }

    # set up labels for lbc_score_dat
    lbc_score_dat_labels <- lapply(lbc_score_dat, function(x) attributes(x)$label)

    ## Score Subscales

    # Food-Related Misbehavior
    if (study == "fbs" | study == "FBS") {
        misbeh_vars <- c("lbc6", "lbc8", "lbc10", "lbc11")
    } else {
        misbeh_vars <- c("lbc3", "lbc4", "lbc5", "lbc6", "lbc8", "lbc10", "lbc11")
    }

    lbc_score_dat[["lbc_misbeh"]] <- rowSums(lbc_data[misbeh_vars])

    ## add labels to data
    lbc_score_dat_labels[["lbc_misbeh"]] <- "LBC Food-Related Misbehavior Total Score"

    # Overeating
    if (study == "fbs" | study == "FBS") {
        overeat_vars <- c("lbc9", "lbc12", "lbc13", "lbc14", "lbc15")
    } else {
        overeat_vars <- c("lbc1", "lbc2", "lbc9", "lbc12", "lbc13", "lbc14", "lbc15")
    }

    lbc_score_dat[["lbc_overeat"]] <- rowSums(lbc_data[overeat_vars])

    ## add labels to data
    lbc_score_dat_labels[["lbc_overeat"]] <- "LBC Overeating Total Score"

    # Emotion Related to Being Overweight
    emOW_vars <- c("lbc20", "lbc21", "lbc22", "lbc23", "lbc24")
    lbc_score_dat[["lbc_em_overweight"]] <- rowSums(lbc_data[emOW_vars])

    ## add labels to data
    lbc_score_dat_labels[["lbc_em_overweight"]] <- "LBC Emotion Related to Being Overweight Total Score"

    # Physical Activity
    pa_vars <- c("lbc7", "lbc16", "lbc17", "lbc18", "lbc19")
    lbc_score_dat[["lbc_pa"]] <- rowSums(lbc_data[pa_vars])

    ## add labels to data
    lbc_score_dat_labels[["lbc_pa"]] <- "LBC Physical Activity Total Score"

    ## Total
    pa_vars <- c("lbc7", "lbc16", "lbc17", "lbc18", "lbc19")
    lbc_score_dat[["lbc_total"]] <- rowSums(lbc_data[c(misbeh_vars, overeat_vars,
        emOW_vars, pa_vars)])

    ## add labels to data
    lbc_score_dat_labels[["lbc_total"]] <- "LBC Total Score"

    #### 3. Clean Export/Scored Data #####
    ## round data
    if (isTRUE(ID_arg)){
        lbc_score_dat[2:ncol(lbc_score_dat)] <- round(lbc_score_dat[2:ncol(lbc_score_dat)], digits = 3)
    } else {
        lbc_score_dat <- round(lbc_score_dat, digits = 3)
    }

    ## make sure the variable labels match in the dataset
    lbc_score_dat = sjlabelled::set_label(lbc_score_dat, label = matrix(unlist(lbc_score_dat_labels,
        use.names = FALSE)))

    return(lbc_score_dat)
}

