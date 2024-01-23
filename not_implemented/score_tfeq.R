#' score_tfeq: Scored data from the Three Factor Eating Questionnaire
#'
#' This function scores the Three Factor Eating Questionnaire and provides subscale scores for the following behaviors: Cognitive Control of Eating Behaviors, Disinhibition of Control, and Susceptibility to Hunger.
#'
#' Note - the factor structure of this questionnaire has been repeatedly questioned so may want to look further to decide on best approach for each study. The provided subscales are based on the canonical approach in the literature and should not be taken as a recommendation or 'best' factoring approach.
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'tfeq#' where # is the question number (1-51)
#' 3) Questions 1-36 must have the numeric value for the choices: 0 - False, 1 - True. Questions 37-51 must have numeric values 1-4 for the various responses with 1 corresponding to the least frequent/likely options and 4 corresponding to the most frequent/most likely option available. The option differ by question.
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Primary References for the Three Factor Eating Questionnaire and Scoring:
#' Stunkard AJ, Messick S. The three-factor eating questionnaire to measure dietary restraint, disinhibition and hunger. Journal of Psychosomatic Research. 1985;29(1):71-83. doi:10.1016/0022-3999(85)90010-8
#'
#'
#' @param tfeq_data a data.frame all items for the Three Factor Eating Questionnaire following the naming conventions described above
#' @inheritParams fbs_intake
#'
#' @return A dataset with subscale scores for the Three Factor Eating Questionnaire
#' @examples
#'
#' # scoring for the tfeq with IDs
#' tfeq_score_data <- score_tfeq(tfeq_data, parID = 'ID')
#'
#'
#' \dontrun{
#' }
#'
#' @seealso Raw data from Qualtrics was processed using the following script: \code{\link{util_fbs_parent_v3dat}}
#'
#'
#' @export

score_tfeq <- function(tfeq_data, parID) {

    #### 1. Set up/initial checks #####

    # check that tfeq_data exist and is a data.frame
    data_arg <- methods::hasArg(tfeq_data)

    if (isTRUE(data_arg) & !is.data.frame(tfeq_data)) {
        stop("tfeq_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("tfeq_data must set to the data.frame with amount consumed for each food item")
    }

    # check if parID exists
    ID_arg <- methods::hasArg(parID)

    if (isTRUE(ID_arg)){
        if (!(parID %in% names(tfeq_data))) {
            stop("variable name entered as parID is not in tfeq_data")
        }
    }

    #### 2. Set Up Data #####

    # set up database for results create empty matrix
    tfeq_score_dat <- data.frame(tfeq_cogcontrol = rep(NA, nrow(tfeq_data)), tfeq_disinhibition = rep(NA,
        nrow(tfeq_data)), tfeq_hunger = rep(NA, nrow(tfeq_data)))

    if (isTRUE(ID_arg)) {
        tfeq_score_dat <- data.frame(tfeq_data[[parID]], tfeq_score_dat)
        names(tfeq_score_dat)[1] <- parID
    }

    # set up labels for tfeq_score_dat
    tfeq_score_dat_labels <- lapply(tfeq_score_dat, function(x) attributes(x)$label)

    # calculate reversed scores

    reverse_tf_qs <- c("tfeq10", "tfeq16", "tfeq21", "tfeq25", "tfeq30", "tfeq31")

    for (var in 1:length(reverse_tf_qs)) {
        var_name <- reverse_tf_qs[var]
        reverse_name <- paste0(var_name, "_rev")

        tfeq_data[[reverse_name]] <- ifelse(is.na(tfeq_data[[var_name]]), NA,
                                            ifelse(tfeq_data[[var_name]] == 1, 0, 1))
    }

    mc_qs <- c("tfeq37", "tfeq38", "tfeq39", "tfeq40", "tfeq41", "tfeq42", "tfeq43", "tfeq44",
               "tfeq45", "tfeq46", "tfeq47", "tfeq48", "tfeq49", "tfeq50", "tfeq51")

    for (var in 1:length(mc_qs)) {

        var_name <- mc_qs[var]

        if (var_name == 'tfeq47') {
            reverse_name <- paste0(var_name, "_recode_rev")
            tfeq_data[[reverse_name]] <- ifelse(is.na(tfeq_data[[var_name]]), NA,
                                                ifelse(tfeq_data[[var_name]] > 2, 0, 1))
        } else {
            recode_name <- paste0(var_name, "_recode")
            tfeq_data[[recode_name]] <- ifelse(is.na(tfeq_data[[var_name]]), NA,
                                                ifelse(tfeq_data[[var_name]] <= 2, 0, 1))
        }
    }

    ## Score Subscales

    # Cognitive Control of Eating Behaviors
    cont_vars <- c("tfeq4", "tfeq6", "tfeq10_rev", "tfeq14", "tfeq18", "tfeq21_rev", "tfeq23", "tfeq28", "tfeq30_rev", "tfeq32", "tfeq33", "tfeq35", "tfeq37_recode", "tfeq38_recode", "tfeq40_recode", "tfeq42_recode", "tfeq43_recode", "tfeq44_recode", "tfeq46_recode", "tfeq48_recode", "tfeq50_recode")
    tfeq_score_dat[["tfeq_cogcontrol"]] <- rowSums(tfeq_data[cont_vars])

    ## add labels to data
    tfeq_score_dat_labels[["tfeq_cogcontrol"]] <- "TFEQ Cogntiive Control Score"

    # Disinhibition of Control
    disinhib_vars <- c("tfeq1", "tfeq2", "tfeq7", "tfeq9", "tfeq11", "tfeq13", "tfeq15", "tfeq16_rev", "tfeq20", "tfeq25_rev", "tfeq27", "tfeq31_rev", "tfeq36", "tfeq45_recode", "tfeq49_recode", "tfeq51_recode")
    tfeq_score_dat[["tfeq_disinhibition"]] <- rowSums(tfeq_data[disinhib_vars])

    ## add labels to data
    tfeq_score_dat_labels[["tfeq_disinhibition"]] <- "TFEQ Disinhibition Score"

    # Susceptibility to Hunger
    hunger_vars <- c("tfeq3", "tfeq5", "tfeq8", "tfeq12", "tfeq17", "tfeq19", "tfeq22", "tfeq24", "tfeq26", "tfeq29", "tfeq34", "tfeq39_recode", "tfeq41_recode", "tfeq47_recode_rev")
    tfeq_score_dat[["tfeq_hunger"]] <- rowSums(tfeq_data[hunger_vars])

    ## add labels to data
    tfeq_score_dat_labels[["tfeq_hunger"]] <- "TFEQ Susceptibility to Hunger Score"

    #### 3. Clean Export/Scored Data #####
    ## round data
    if (isTRUE(ID_arg)){
        tfeq_score_dat[2:ncol(tfeq_score_dat)] <- round(tfeq_score_dat[2:ncol(tfeq_score_dat)], digits = 3)
    } else {
        tfeq_score_dat <- round(tfeq_score_dat, digits = 3)
    }

    ## make sure the variable labels match in the dataset
    tfeq_score_dat = sjlabelled::set_label(tfeq_score_dat, label = matrix(unlist(tfeq_score_dat_labels,
        use.names = FALSE)))

    return(tfeq_score_dat)
}

