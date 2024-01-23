#' score_bes: Scored data from the Binge Eating Scale
#'
#' This function scores the Binge Eating Scale
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'bes#' where # is the question number (1-16)
#' 3) All questions must have the numeric value for the choice: 1 - 4 from least to most severe
#' 4) This script will apply specific scoring transformations that are specific to each question. For example, the first 2 statement are reset to 0 for question 1 but not for question 2.
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Gormally, J., Black, S., Daston, S., & Rardin, D. (1982). The assessment of binge eating severity among obese persons. Addictive Behaviors, 7(1), 47–55. https://doi.org/10.1016/0306-4603(82)90024-7  (\href{https://pubmed.ncbi.nlm.nih.gov/7080884/}{PubMed})
#'
#' Timmerman, G. M. (1999). Binge Eating Scale: Further Assessment of Validity and Reliability. Journal of Applied Biobehavioral Research, 4(1), 1–12. https://doi.org/10.1111/j.1751-9861.1999.tb00051.x
#'
#' @param bes_data a data.frame all items for the Binge Eating Scale following the naming conventions described above
#' @param parID (optional) name of participant ID column in bes_data. If included the output dataset will be matched by parID, if not included the output dataset will be in the order of bes_data but will have no participant identifier.
#'
#' @return A dataset with total score for the Binge Eating Scale
#' @examples
#'
#' # scoring for the bes with IDs
#' bes_score_data <- score_bes(bes_data, parID = 'ID')
#'
#' \dontrun{
#' }
#'
#' @seealso Raw data from Qualtrics was processed using the following script: \code{\link{util_fbs_parent_v2dat}}
#'
#'
#' @export

score_bes <- function(bes_data, parID) {

    #### 1. Set up/initial checks #####

    # check that bes_data exist and is a data.frame
    data_arg <- methods::hasArg(bes_data)

    if (isTRUE(data_arg) & !is.data.frame(bes_data)) {
        stop("bes_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("bes_data must set to the data.frame with amount consumed for each food item")
    }

    # check if parID exists
    ID_arg <- methods::hasArg(parID)

    if (isTRUE(ID_arg)){
        if (!(parID %in% names(bes_data))) {
            stop("variable name entered as parID is not in bes_data")
        }
    }

    #### 2. Set Up Data #####

    # set up database for results create empty matrix
    bes_score_dat <- data.frame(bes_total = rep(NA, nrow(bes_data)))

    if (isTRUE(ID_arg)) {
        bes_score_dat <- data.frame(bes_data[[parID]], bes_score_dat)
        names(bes_score_dat)[1] <- parID
    }

    # set up labels for bes_score_dat
    bes_score_dat_labels <- lapply(bes_score_dat, function(x) attributes(x)$label)

    # calculate question - specific scoring values

    ## subset with consistent scoring
    cons_score_vars <- c("bes2", "bes5", "bes8", "bes9", "bes10", "bes11", "bes12",
        "bes14", "bes15", "bes16")

    for (var in 1:length(cons_score_vars)) {
        var_name <- cons_score_vars[var]

        score_varname <- paste0(var_name, "_s")
        bes_data[[score_varname]] <- bes_data[[var_name]] - 1
    }

    # custom scoring by question
    bes_data[["bes1_s"]] <- ifelse(is.na(bes_data[["bes1"]]), NA, ifelse(bes_data[["bes1"]] <
        3, 0, ifelse(bes_data[["bes1"]] == 3, 1, 4)))

    bes_data[["bes3_s"]] <- ifelse(is.na(bes_data[["bes3"]]), NA, ifelse(bes_data[["bes3"]] >
        2, 3, ifelse(bes_data[["bes3"]] == 2, 1, 0)))

    bes_data[["bes4_s"]] <- ifelse(is.na(bes_data[["bes4"]]), NA, ifelse(bes_data[["bes4"]] <
        4, 0, 2))

    bes_data[["bes6_s"]] <- ifelse(is.na(bes_data[["bes6"]]), NA, ifelse(bes_data[["bes6"]] ==
        3, 3, bes_data[["bes6"]] - 1))

    bes_data[["bes7_s"]] <- ifelse(is.na(bes_data[["bes7"]]), NA, ifelse(bes_data[["bes7"]] ==
        1, 0, ifelse(bes_data[["bes7"]] > 2, 3, 2)))

    bes_data[["bes13_s"]] <- ifelse(is.na(bes_data[["bes13"]]), NA, ifelse(bes_data[["bes13"]] <
        3, 0, ifelse(bes_data[["bes13"]] == 3, 2, 3)))

    ## Score

    # Total Score
    bes_scored_vars <- c("bes1_s", "bes2_s", "bes3_s", "bes4_s", "bes5_s", "bes6_s",
        "bes7_s", "bes8_s", "bes9_s", "bes10_s", "bes11_s", "bes12_s", "bes13_s",
        "bes14_s", "bes15_s", "bes16_s")
    bes_score_dat[["bes_total"]] <- rowSums(bes_data[bes_scored_vars])

    ## add labels to data
    bes_score_dat_labels[["bes_total"]] <- "BES Total Score"

    #### 3. Clean Export/Scored Data #####
    ## round data
    if (isTRUE(ID_arg)){
        bes_score_dat[2:ncol(bes_score_dat)] <- round(bes_score_dat[2:ncol(bes_score_dat)], digits = 3)
    } else {
        bes_score_dat <- round(bes_score_dat, digits = 3)
    }

    ## make sure the variable labels match in the dataset
    bes_score_dat = sjlabelled::set_label(bes_score_dat, label = matrix(unlist(bes_score_dat_labels,
        use.names = FALSE)))

    return(bes_score_dat)
}

