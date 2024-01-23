#' score_cbq: Scored data from the Child Behavior Questionnaire
#'
#' This function scores the Child Behavior Questionnaire and provides subscale scores for the following behaviors: Activity Level, Anger/Frustration, Approach/Positive Anticipation, Attentional Focusing, Discomfort, Falling Reactivity/Soothability, Fear, High Intesity Pleasure, Impulsivity, Inhibitory Control, Low Intensity Pleasure, Perceptual Sensitivity, Sadness, Shyness, Smiling and Laughter. We can also get the Big 3 subcales: Surgency, Negative Affect, and Effortful Control.
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'cbq#' where # is the question number (1-94)
#' 3) All questions must have the numeric value for the choice: 1 - Extremely Untrue, 2 - Quite Untrue, 3 - Sightly Untrue, 4 - Neither True nor False, 5 - Slightly True, 6 - Quite True, 7 - Extremely True, NA - NA
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Putnam SP, Rothbart MK. Development of Short and Very Short Forms of the Children’s Behavior Questionnaire. Journal of Personality Assessment. 2006;87(1):102-112. doi:10.1207/s15327752jpa8701_09 https://doi.org/10.1097/00004703-200112000-00007 (\href{https://pubmed.ncbi.nlm.nih.gov/16856791/}{PubMed})
#'
#'Rothbart MΚ, Ahadi SA, Hershey KL. Temperament and Social Behavior in Childhood. Merrill-Palmer Quarterly. 1994;40(1):21-39 (\href{https://www.jstor.org/stable/23087906}{jstore})
#'
#' @param cbq_data a data.frame all items for the Child Behavior Questionnaire following the naming conventions described above
#' @inheritParams fbs_intake
#'
#' @return A dataset with subscale scores for the Child Behavior Questionnaire
#' @examples
#'
#' # scoring for the cbq with IDs
#' cbq_score_data <- score_cbq(cbq_data, parID = 'ID')
#'
#' \dontrun{
#' }
#'
#' @seealso Raw data from Qualtrics was processed using the following script: \code{\link{util_fbs_parent_v2dat}}
#'
#'
#' @export

score_cbq <- function(cbq_data, parID) {

    #### 1. Set up/initial checks #####

    # check that cbq_data exist and is a data.frame
    data_arg <- methods::hasArg(cbq_data)

    if (isTRUE(data_arg) & !is.data.frame(cbq_data)) {
        stop("cbq_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("cbq_data must set to the data.frame with amount consumed for each food item")
    }

    # check if parID exists
    ID_arg <- methods::hasArg(parID)

    if (isTRUE(ID_arg)){
        if (!(parID %in% names(cbq_data))) {
            stop("variable name entered as parID is not in cbq_data")
        }
    }

    #### 2. Set Up Data #####

    # set up database for results

    ## create empty matrix
    cbq_score_dat <- data.frame(cbq_activity = rep(NA, nrow(cbq_data)), cbq_anger = rep(NA, nrow(cbq_data)), cbq_approach = rep(NA, nrow(cbq_data)), cbq_attention = rep(NA, nrow(cbq_data)), cbq_discomfort = rep(NA, nrow(cbq_data)), cbq_soothability = rep(NA, nrow(cbq_data)), cbq_fear = rep(NA, nrow(cbq_data)), cbq_highintensity_pleasure = rep(NA, nrow(cbq_data)), cbq_impulsivity = rep(NA, nrow(cbq_data)), cbq_inhibitory_cont = rep(NA, nrow(cbq_data)), cbq_lowintensity_pleasure = rep(NA, nrow(cbq_data)), cbq_perceptual_sensitivity = rep(NA, nrow(cbq_data)), cbq_sadness = rep(NA, nrow(cbq_data)), cbq_shyness = rep(NA, nrow(cbq_data)), cbq_smile_laughter = rep(NA,  nrow(cbq_data)), cbq_surgency = rep(NA, nrow(cbq_data)), cbq_neg_affect = rep(NA, nrow(cbq_data)), cbq_effortful_cont = rep(NA, nrow(cbq_data)))

    if (isTRUE(ID_arg)) {
        cbq_score_dat <- data.frame(cbq_data[[parID]], cbq_score_dat)
        names(cbq_score_dat)[1] <- parID
    }

    # set up labels for cbq_score_dat
    cbq_score_dat_labels <- lapply(cbq_score_dat, function(x) attributes(x)$label)

    # calculate reversed scores

    reverse_qs <- c("cbq3", "cbq11", "cbq16", "cbq18", "cbq19", "cbq21", "cbq25",
        "cbq34", "cbq35", "cbq36", "cbq43", "cbq48", "cbq49", "cbq50", "cbq53",
        "cbq54", "cbq56", "cbq60", "cbq61", "cbq68", "cbq74", "cbq75", "cbq78",
        "cbq80", "cbq82", "cbq83", "cbq84", "cbq90", "cbq91", "cbq92", "cbq93")

    for (var in 1:length(reverse_qs)) {
        var_name <- reverse_qs[var]
        reverse_name <- paste0(var_name, "_rev")

        cbq_data[[reverse_name]] <- ifelse(is.na(cbq_data[[var_name]]), NA, ifelse(cbq_data[[var_name]] == 1, 7, ifelse(cbq_data[[var_name]] == 2, 6, ifelse(cbq_data[[var_name]] == 3, 5, ifelse(cbq_data[[var_name]] == 5, 3, ifelse(cbq_data[[var_name]] ==  6, 2, ifelse(cbq_data[[var_name]] == 7, 1, 4)))))))
    }

    ## covert -99 to NA; considered NA in scoring which is sum/#answered
    for (var in 1:ncol(cbq_data)) {
        var_name <- names(cbq_data)[var]

        if (grepl("cbq", var_name, fixed = TRUE)) {
            cbq_data[[var_name]] <- ifelse(is.na(cbq_data[[var_name]]), NA, ifelse(cbq_data[[var_name]] ==
                -99, NA, cbq_data[[var_name]]))
        }

    }

    ## Score Subscales

    # Activity Level
    activity_vars <- c("cbq1", "cbq12", "cbq18_rev", "cbq22", "cbq50_rev", "cbq85",
        "cbq93_rev")
    cbq_score_dat[["cbq_activity"]] <- rowMeans(cbq_data[activity_vars], na.rm = TRUE)

    ## add labels to data
    cbq_score_dat_labels[["cbq_activity"]] <- "CBQ Activity Level Total Score"

    # Anger/Frustration
    anger_vars <- c("cbq2", "cbq14", "cbq30", "cbq40", "cbq61_rev", "cbq87")
    cbq_score_dat[["cbq_anger"]] <- rowMeans(cbq_data[anger_vars], na.rm = TRUE)

    ## add labels to data
    cbq_score_dat_labels[["cbq_anger"]] <- "CBQ Anger/Frustration Total Score"

    # Approach/Positive Anticipation
    approach_vars <- c("cbq6", "cbq15", "cbq46", "cbq58", "cbq90_rev", "cbq92_rev")
    cbq_score_dat[["cbq_approach"]] <- rowMeans(cbq_data[approach_vars], na.rm = TRUE)

    ## add labels to data
    cbq_score_dat_labels[["cbq_approach"]] <- "CBQ Approach/Positive Anticipation Total Score"

    # Attentional Focusing
    attention_vars <- c("cbq16_rev", "cbq21_rev", "cbq62", "cbq71", "cbq84_rev",
        "cbq89")
    cbq_score_dat[["cbq_attention"]] <- rowMeans(cbq_data[attention_vars], na.rm = TRUE)

    ## add labels to data
    cbq_score_dat_labels[["cbq_attention"]] <- "CBQ Attentional Focusing Total Score"

    # Discomfort
    discomfort_vars <- c("cbq3_rev", "cbq9", "cbq29", "cbq49_rev", "cbq64", "cbq91_rev")
    cbq_score_dat[["cbq_discomfort"]] <- rowMeans(cbq_data[discomfort_vars], na.rm = TRUE)

    ## add labels to data
    cbq_score_dat_labels[["cbq_discomfort"]] <- "CBQ Discomfort Total Score"

    # Falling Reactivity/Soothability
    sooth_vars <- c("cbq25_rev", "cbq34_rev", "cbq44", "cbq59", "cbq66", "cbq75_rev")
    cbq_score_dat[["cbq_soothability"]] <- rowMeans(cbq_data[sooth_vars], na.rm = TRUE)

    ## add labels to data
    cbq_score_dat_labels[["cbq_soothability"]] <- "CBQ Falling Reactivity/Soothability Total Score"

    # Fear
    fear_vars <- c("cbq17", "cbq23", "cbq35_rev", "cbq41", "cbq63", "cbq68_rev")
    cbq_score_dat[["cbq_fear"]] <- rowMeans(cbq_data[fear_vars], na.rm = TRUE)

    ## add labels to data
    cbq_score_dat_labels[["cbq_fear"]] <- "CBQ Fear Total Score"

    # High Intensity Pleasure
    hi_pleasure_vars <- c("cbq4", "cbq10", "cbq33", "cbq69", "cbq78_rev", "cbq88")
    cbq_score_dat[["cbq_highintensity_pleasure"]] <- rowMeans(cbq_data[hi_pleasure_vars],
        na.rm = TRUE)

    ## add labels to data
    cbq_score_dat_labels[["cbq_highintensity_pleasure"]] <- "CBQ High Intensity Pleasure Total Score"

    # Impulsivity
    impulsivity_vars <- c("cbq7", "cbq28", "cbq36_rev", "cbq43_rev", "cbq51",
        "cbq82_rev")
    cbq_score_dat[["cbq_impulsivity"]] <- rowMeans(cbq_data[impulsivity_vars],
        na.rm = TRUE)

    ## add labels to data
    cbq_score_dat_labels[["cbq_impulsivity"]] <- "CBQ Impulsivity Total Score"

    # Inhibitory Control
    inhib_vars <- c("cbq38", "cbq45", "cbq53_rev", "cbq67", "cbq73", "cbq81")
    cbq_score_dat[["cbq_inhibitory_cont"]] <- rowMeans(cbq_data[inhib_vars], na.rm = TRUE)

    ## add labels to data
    cbq_score_dat_labels[["cbq_inhibitory_cont"]] <- "CBQ Inhibitory Control Total Score"

    # Low Intensity Pleasure
    li_pleasure_vars <- c("cbq26", "cbq39", "cbq57", "cbq65", "cbq72", "cbq76",
        "cbq86", "cbq94")
    cbq_score_dat[["cbq_lowintensity_pleasure"]] <- rowMeans(cbq_data[li_pleasure_vars],
        na.rm = TRUE)

    ## add labels to data
    cbq_score_dat_labels[["cbq_lowintensity_pleasure"]] <- "CBQ Low Intensity Pleasure Total Score"

    # Perceptual Sensitivity
    percept_vars <- c("cbq5", "cbq13", "cbq24", "cbq32", "cbq47", "cbq83_rev")
    cbq_score_dat[["cbq_perceptual_sensitivity"]] <- rowMeans(cbq_data[percept_vars],
        na.rm = TRUE)

    ## add labels to data
    cbq_score_dat_labels[["cbq_perceptual_sensitivity"]] <- "CBQ Perceptual Sensitivity Total Score"

    # Sadness
    sad_vars <- c("cbq8", "cbq20", "cbq27", "cbq31", "cbq54_rev", "cbq56_rev",
        "cbq74_rev")
    cbq_score_dat[["cbq_sadness"]] <- rowMeans(cbq_data[sad_vars], na.rm = TRUE)

    ## add labels to data
    cbq_score_dat_labels[["cbq_sadness"]] <- "CBQ Sadness Total Score"

    # Shyness
    shy_vars <- c("cbq11_rev", "cbq37", "cbq42", "cbq52", "cbq60_rev", "cbq70")
    cbq_score_dat[["cbq_shyness"]] <- rowMeans(cbq_data[shy_vars], na.rm = TRUE)

    ## add labels to data
    cbq_score_dat_labels[["cbq_shyness"]] <- "CBQ Shyness Total Score"

    # Smiling and Laughter
    smile_vars <- c("cbq19_rev", "cbq48_rev", "cbq55", "cbq77", "cbq79", "cbq80_rev")
    cbq_score_dat[["cbq_smile_laughter"]] <- rowMeans(cbq_data[smile_vars], na.rm = TRUE)

    ## add labels to data
    cbq_score_dat_labels[["cbq_smile_laughter"]] <- "CBQ Smiling and Laughter Total Score"

    # Big 3 - Surgency reverse the shyness scale
    cbq_score_dat[["cbq_shyness_rev"]] <- 8 - cbq_score_dat["cbq_shyness"]

    cbq_score_dat[["cbq_surgency"]] <- rowMeans(cbq_score_dat[c("cbq_activity",
        "cbq_highintensity_pleasure", "cbq_impulsivity", "cbq_shyness_rev")])

    # remove shyness-reveresed scale
    cbq_score_dat <- cbq_score_dat[, !names(cbq_score_dat) == "cbq_shyness_rev"]

    ## add labels to data
    cbq_score_dat_labels[["cbq_surgency"]] <- "CBQ Big 3 - Surgency Total Score"

    # Big 3 - Negative Affect remove soothability scale
    cbq_score_dat[["cbq_soothability_rev"]] <- 8 - cbq_score_dat["cbq_soothability"]

    cbq_score_dat[["cbq_neg_affect"]] <- rowMeans(cbq_score_dat[c("cbq_anger",
        "cbq_discomfort", "cbq_fear", "cbq_sadness", "cbq_soothability_rev")])

    # remove soothability-reversed scale
    cbq_score_dat <- cbq_score_dat[, !names(cbq_score_dat) == "cbq_soothability_rev"]

    ## add labels to data
    cbq_score_dat_labels[["cbq_neg_affect"]] <- "CBQ Big 3 - Negative Affect Total Score"

    # Big 3 - Effortful Control remove soothability scale
    cbq_score_dat[["cbq_soothability_rev"]] <- 8 - cbq_score_dat["cbq_soothability"]

    cbq_score_dat[["cbq_effortful_cont"]] <- rowMeans(cbq_score_dat[c("cbq_attention",
        "cbq_inhibitory_cont", "cbq_lowintensity_pleasure", "cbq_perceptual_sensitivity")])

    # remove soothability-reversed scale
    cbq_score_dat <- cbq_score_dat[, !names(cbq_score_dat) == "cbq_soothability_rev"]

    ## add labels to data
    cbq_score_dat_labels[["cbq_effortful_cont"]] <- "CBQ Big 3 - Effortful Control Total Score"

    #### 3. Clean Export/Scored Data #####
    ## round data
    if (isTRUE(ID_arg)){
        cbq_score_dat[2:ncol(cbq_score_dat)] <- round(cbq_score_dat[2:ncol(cbq_score_dat)], digits = 3)
    } else {
        cbq_score_dat <- round(cbq_score_dat, digits = 3)
    }

    ## make sure the variable labels match in the dataset
    cbq_score_dat = sjlabelled::set_label(cbq_score_dat, label = matrix(unlist(cbq_score_dat_labels,
        use.names = FALSE)))

    return(cbq_score_dat)
}

