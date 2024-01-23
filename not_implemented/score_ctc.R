#' score_ctc: Scored data from the Communities that Care
#'
#' This function scores the Child Behavior Questionnaire and provides subscale scores for the following behaviors: Child Approval of Drug/Alcohol Use, Child View of Drug/Alcohol Use as Harmful, Friend Approval of Drug/Acohol Use, and Parent Disapproval of Drug/Alcohol Use.
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'ctc#' where # is the question number (1-16 for the Food and Brain Study)
#' 3) All questions must have the numeric value for the choice: 1 - Not at all, 2 - A little, 3 - Not sure/in the middle, 4 - Somewhat, 5 - A lot, -99 - Skip
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' NEED
#'
#'
#' @param ctc_data a data.frame all items for the Child Behavior Questionnaire following the naming conventions described above
#' @param study a string indicating which study collected the data. Currently, only option and default is 'fbs'. This parameter is included because the Food and Brain study used a highly adapted verison of the Communities that Care Questionnaire with only 16 questions
#' @inheritParams fbs_intake
#'
#' @return A dataset with subscale scores for the Child Behavior Questionnaire
#' @examples
#'
#' # scoring for the ctc with IDs
#' ctc_score_data <- score_ctc(ctc_data, parID = 'ID')
#'
#' \dontrun{
#' }
#'
#' @seealso Raw data from Qualtrics was processed using the following script: \code{\link{util_fbs_child_v5dat}}
#'
#'
#' @export

score_ctc <- function(ctc_data, study, parID) {

    #### 1. Set up/initial checks #####

    # check that ctc_data exist and is a data.frame
    data_arg <- methods::hasArg(ctc_data)

    if (isTRUE(data_arg) & !is.data.frame(ctc_data)) {
        stop("ctc_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("ctc_data must set to the data.frame with amount consumed for each food item")
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
        if (!(parID %in% names(ctc_data))) {
            stop("variable name entered as parID is not in ctc_data")
        }
    }

    #### 2. Set Up Data #####

    # FBS specific scoring - if using true CTC survey need to fully add appropriate scoring
    if (study == "fbs" | study == "FBS") {

        ## create empty matrix
        ctc_score_dat <- data.frame(ctc_child_cool = rep(NA, nrow(ctc_data)), ctc_child_harmful = rep(NA, nrow(ctc_data)), ctc_friend_cool = rep(NA, nrow(ctc_data)), ctc_parent_disaprove = rep(NA, nrow(ctc_data)))

        if (isTRUE(ID_arg)) {
            ctc_score_dat <- data.frame(ctc_data[[parID]], ctc_score_dat)
            names(ctc_score_dat)[1] <- parID
        }

        # set up labels for ctc_score_dat
        ctc_score_dat_labels <- lapply(ctc_score_dat, function(x) attributes(x)$label)

        # remove skip = -99
        ctc_qs <- c("ctc1", "ctc2", "ctc3", "ctc4", "ctc5", "ctc6", "ctc7", "ctc8", "ctc9", "ctc10", "ctc11", "ctc12", "ctc13", "ctc14", "ctc15", "ctc16")

        for (var in 1:length(ctc_qs)) {

            ctc_data[[var]] <- ifelse(is.na(ctc_data[[var]]) | ctc_data[[var]] == -99,  NA, ctc_data[[var]])

        }

        ## Score Subscales

        # Child - cool
        c_cool_vars <- c("ctc1", "ctc2", "ctc3", "ctc4")

        #allow 1 skip
        ctc_data[['child_cool_n_miss']] <- rowSums(is.na(ctc_data[c_cool_vars]))
        ctc_data[['child_cool_nas']] <- rowMeans(ctc_data[c_cool_vars], na.rm = TRUE)
        ctc_data[['child_cool_no_nas']] <- rowMeans(ctc_data[c_cool_vars], na.rm = FALSE)

        ctc_score_dat[['ctc_child_cool']] <- ifelse(ctc_data[['child_cool_n_miss']] <= 3, ctc_data[['child_cool_nas']], ctc_data[['child_cool_no_nas']])

        ## add labels to data
        ctc_score_dat_labels[["ctc_child_cool"]] <- "CTC Child condsiders drug/acohol use cool; 1 missing/NA allowed when computing subscale"

        # Child - harmful
        c_harmful_vars <- c("ctc9", "ctc10", "ctc11", "ctc12")

        #allow 1 skip
        ctc_data[['child_harm_n_miss']] <- rowSums(is.na(ctc_data[c_harmful_vars]))
        ctc_data[['child_harm_nas']] <- rowMeans(ctc_data[c_harmful_vars], na.rm = TRUE)
        ctc_data[['child_harm_no_nas']] <- rowMeans(ctc_data[c_harmful_vars], na.rm = FALSE)

        ctc_score_dat[['ctc_child_harmful']] <- ifelse(ctc_data[['child_harm_n_miss']] <= 3, ctc_data[['child_harm_nas']], ctc_data[['child_harm_no_nas']])

        ## add labels to data
        ctc_score_dat_labels[["ctc_child_harmful"]] <- "CTC Child condsiders drug/acohol harmful; 1 missing/NA allowed when computing subscale"

        # Friend - cool
        f_cool_vars <- c("ctc5", "ctc6", "ctc5", "ctc8")

        #allow 1 skip
        ctc_data[['friend_cool_n_miss']] <- rowSums(is.na(ctc_data[f_cool_vars]))
        ctc_data[['friend_cool_nas']] <- rowMeans(ctc_data[f_cool_vars], na.rm = TRUE)
        ctc_data[['friend_cool_no_nas']] <- rowMeans(ctc_data[f_cool_vars], na.rm = FALSE)

        ctc_score_dat[['ctc_friend_cool']] <- ifelse(ctc_data[['friend_cool_n_miss']] <= 3, ctc_data[['friend_cool_nas']], ctc_data[['friend_cool_no_nas']])

        ## add labels to data
        ctc_score_dat_labels[["ctc_friend_cool"]] <- "CTC Friends condsider drug/acohol harmful; 1 missing/NA allowed when computing subscale"

        # Parents
        p_disaprove_vars <- c("ctc13", "ctc14", "ctc15", "ctc16")

        #allow 1 skip
        ctc_data[['parent_n_miss']] <- rowSums(is.na(ctc_data[p_disaprove_vars]))
        ctc_data[['parent_nas']] <- rowMeans(ctc_data[p_disaprove_vars], na.rm = TRUE)
        ctc_data[['parent_no_nas']] <- rowMeans(ctc_data[p_disaprove_vars], na.rm = FALSE)

        ctc_score_dat[['ctc_parent_disaprove']] <- ifelse(ctc_data[['parent_n_miss']] <= 3, ctc_data[['parent_nas']], ctc_data[['parent_no_nas']])

        ## add labels to data
        ctc_score_dat_labels[["ctc_parent_disaprove"]] <- "CTC Parents disaprove of drug/acohol use; 1 missing/NA allowed when computing subscale"
    }


    #### 3. Clean Export/Scored Data #####
    ## round data
    if (isTRUE(ID_arg)){
        ctc_score_dat[2:ncol(ctc_score_dat)] <- round(ctc_score_dat[2:ncol(ctc_score_dat)], digits = 3)
    } else {
        ctc_score_dat <- round(ctc_score_dat, digits = 3)
    }

    ## make sure the variable labels match in the dataset
    ctc_score_dat = sjlabelled::set_label(ctc_score_dat, label = matrix(unlist(ctc_score_dat_labels, use.names = FALSE)))

    return(ctc_score_dat)
}

