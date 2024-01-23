#' score_cebq: Scored data from the Children's Eating Behavior Questionnaire
#'
#' This function scores the Children's Eating Behavior Questionnaire and provides subscale scores for the following behaviors: Food Responsiveness, Emotional Overeating, Enjoyment of Food, Desire to Drink, Satiety Responsiveness, Slowness in Eating, Emotional Undereating, and Food Fussiness
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'cebq#' where # is the question number (1-35)
#' 3) All questions must have the numeric value for the choice: 1 - Never, 2 - Rarely, 3 - Sometimes, 4 - Often, 5 - Always
#' 4) This script will apply reverse scoring so all levels must be true to the scale described above
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Primary References for the Children's Eating Behavior Questionniare and Scoring:
#' Wardle, J., Guthrie, C. A., Sanderson, S., & Rapoport, L. (2001). Development of the children’s eating behaviour questionnaire. Journal of Child Psychology and Psychiatry, 42, 963–970. https://doi.org/10.1017/S0021963001007727 (\href{https://pubmed.ncbi.nlm.nih.gov/11693591/}{PubMed})
#'
#' @param cebq_data a data.frame all items for the Children's Eating Behavior Questionnaire following the naming conventions described above
#' @inheritParams fbs_intake
#'
#' @return A dataset with subscale scores for the Children's Eating Behavior Questionnaire
#' @examples
#'
#' # scoring for the CEBQ with IDs
#' cebq_score_data <- score_cebq(cebq_data, parID = 'ID')
#'
#' \dontrun{
#' }
#'
#' @seealso Raw data from Qualtrics was processed using the following script: \code{\link{util_fbs_parent_v2dat}}
#'
#'
#' @export

score_cebq <- function(cebq_data, parID) {

    #### 1. Set up/initial checks #####

    # check that cebq_data exist and is a data.frame
    data_arg <- methods::hasArg(cebq_data)

    if (isTRUE(data_arg) & !is.data.frame(cebq_data)) {
        stop("cebq_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("cebq_data must set to the data.frame with amount consumed for each food item")
    }

    # check if parID exists
    ID_arg <- methods::hasArg(parID)

    if (isTRUE(ID_arg)){
        if (!(parID %in% names(cebq_data))) {
            stop("variable name entered as parID is not in cebq_data")
        }
    }

    #### 2. Set Up Data #####

    # set up database for results create empty matrix
    cebq_score_dat <- data.frame(cebq_fr = rep(NA, nrow(cebq_data)), cebq_eoe = rep(NA,
        nrow(cebq_data)), cebq_ef = rep(NA, nrow(cebq_data)), cebq_dd = rep(NA,
        nrow(cebq_data)), cebq_sr = rep(NA, nrow(cebq_data)), cebq_se = rep(NA,
        nrow(cebq_data)), cebq_eue = rep(NA, nrow(cebq_data)), cebq_ff = rep(NA,
        nrow(cebq_data)), cebq_approach = rep(NA, nrow(cebq_data)), cebq_avoid = rep(NA,
        nrow(cebq_data)))

    if (isTRUE(ID_arg)) {
        cebq_score_dat <- data.frame(cebq_data[[parID]], cebq_score_dat)
        names(cebq_score_dat)[1] <- parID
    }

    # set up labels for cebq_score_dat
    cebq_score_dat_labels <- lapply(cebq_score_dat, function(x) attributes(x)$label)

    # calculate reversed scores

    reverse_qs <- c("cebq3", "cebq4", "cebq10", "cebq16", "cebq32")

    for (var in 1:length(reverse_qs)) {
        var_name <- reverse_qs[var]
        reverse_name <- paste0(var_name, "_rev")

        cebq_data[[reverse_name]] <- ifelse(is.na(cebq_data[[var_name]]), NA,
            ifelse(cebq_data[[var_name]] == 1, 5, ifelse(cebq_data[[var_name]] ==
                2, 4, ifelse(cebq_data[[var_name]] == 4, 2, ifelse(cebq_data[[var_name]] ==
                5, 1, 3)))))
    }

    ## Score Subscales

    # Food Responsiveness
    FR_vars <- c("cebq12", "cebq14", "cebq19", "cebq28", "cebq34")
    cebq_score_dat[["cebq_fr"]] <- rowMeans(cebq_data[FR_vars])

    ## add labels to data
    cebq_score_dat_labels[["cebq_fr"]] <- "CEBQ Food Responsiveness Total Score"

    # Emotional Overeating
    EOE_vars <- c("cebq2", "cebq13", "cebq15", "cebq27")
    cebq_score_dat[["cebq_eoe"]] <- rowMeans(cebq_data[EOE_vars])

    ## add labels to data
    cebq_score_dat_labels[["cebq_eoe"]] <- "CEBQ Emotional Overeating Total Score"

    # Enjoyment of Food
    EF_vars <- c("cebq1", "cebq5", "cebq20", "cebq22")
    cebq_score_dat[["cebq_ef"]] <- rowMeans(cebq_data[EF_vars])

    ## add labels to data
    cebq_score_dat_labels[["cebq_ef"]] <- "CEBQ Enjoyment of Food Total Score"

    # Desire to Drink
    DD_vars <- c("cebq6", "cebq29", "cebq31")
    cebq_score_dat[["cebq_dd"]] <- rowMeans(cebq_data[DD_vars])

    ## add labels to data
    cebq_score_dat_labels[["cebq_dd"]] <- "CEBQ Desire to Drink Total Score"

    # Satiety Responsiveness
    SR_vars <- c("cebq3_rev", "cebq17", "cebq21", "cebq26", "cebq30")
    cebq_score_dat[["cebq_sr"]] <- rowMeans(cebq_data[SR_vars])

    ## add labels to data
    cebq_score_dat_labels[["cebq_sr"]] <- "CEBQ Satiety Responsiveness Total Score"

    # Slowness in Eating
    SE_vars <- c("cebq4_rev", "cebq8", "cebq18", "cebq35")
    cebq_score_dat[["cebq_se"]] <- rowMeans(cebq_data[SE_vars])

    ## add labels to data
    cebq_score_dat_labels[["cebq_se"]] <- "CEBQ Slowness in Eating Total Score"

    # Emotional Under Eating
    EUE_vars <- c("cebq9", "cebq11", "cebq23", "cebq35")
    cebq_score_dat[["cebq_eue"]] <- rowMeans(cebq_data[EUE_vars])

    ## add labels to data
    cebq_score_dat_labels[["cebq_eue"]] <- "CEBQ Emotional Under Eating Total Score"

    # Food Fussiness
    FF_vars <- c("cebq7", "cebq10_rev", "cebq16_rev", "cebq24", "cebq32_rev",
        "cebq33")
    cebq_score_dat[["cebq_ff"]] <- rowMeans(cebq_data[FF_vars])

    ## add labels to data
    cebq_score_dat_labels[["cebq_ff"]] <- "CEBQ Food Fussiness Total Score"


    # Total Approach Score
    cebq_score_dat[["cebq_approach"]] <- rowMeans(cebq_data[c(FR_vars, EOE_vars,
        EF_vars, DD_vars)])

    ## add labels to data
    cebq_score_dat_labels[["cebq_approach"]] <- "CEBQ Approach Total Score"

    # Total Avoid Score
    cebq_score_dat[["cebq_avoid"]] <- rowMeans(cebq_data[c(SR_vars, SE_vars, EUE_vars,
        FF_vars)])

    ## add labels to data
    cebq_score_dat_labels[["cebq_avoid"]] <- "CEBQ Avoid Total Score"

    #### 3. Clean Export/Scored Data #####
    ## round data
    if (isTRUE(ID_arg)){
        cebq_score_dat[2:ncol(cebq_score_dat)] <- round(cebq_score_dat[2:ncol(cebq_score_dat)], digits = 3)
    } else {
        cebq_score_dat <- round(cebq_score_dat, digits = 3)
    }

    ## make sure the variable labels match in the dataset
    cebq_score_dat = sjlabelled::set_label(cebq_score_dat, label = matrix(unlist(cebq_score_dat_labels,
        use.names = FALSE)))

    return(cebq_score_dat)
}

