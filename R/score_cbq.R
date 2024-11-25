#' score_cbq: Scored data from the Child Behavior Questionnaire
#'
#' This function scores the Child Behavior Questionnaire and provides subscale scores for the following behaviors: Activity Level, Anger/Frustration, Approach/Positive Anticipation, Attentional Focusing, Discomfort, Falling Reactivity/Soothability, Fear, High Intesity Pleasure, Impulsivity, Inhibitory Control, Low Intensity Pleasure, Perceptual Sensitivity, Sadness, Shyness, Smiling and Laughter. We can also get the Big 3 subcales: Surgency, Negative Affect, and Effortful Control.
#' 
#' For data to be scored correctly, the data must be prepared according to the following criteria: \cr
#' \itemize{
#'  \item{The data must include all individual questionnaire items}
#'  \item{The columns/variables must match the following naming convention: 'cbq#' or 'cbq_#' where # is the question number (1-94)}
#'  \item{All questionnaire responses must be a numeric value ranging from 0-6 (base_zero = TRUE) or 1-7 (base_zero = FALSE) where: }
#'  \itemize{
#'     \item{For base_zero = TRUE: 0 = Extremely Untrue; 1 = Quite Untrue; 2 = Sightly Untrue; 3 = Neither True nor False; 4 = Slightly True; 5 - Quite True; 6 - Extremely True, does_not_apply_value (param) = does not apply }
#'     \item{For base_zero = FALSE: 1 = Extremely Untrue; 2 = Quite Untrue; 3 = Sightly Untrue; 4 = Neither True nor False; 5 = Slightly True; 6 - Quite True; 7 - Extremely True, does_not_apply_value (param) = does not apply }
#'   }
#'  \item{Missing values must be coded as NA}
#'  \item{Values must not be reversed scored. This script will apply reverse scoring so all levels must be true to the scale described above}
#' }
#' \cr
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Putnam SP, Rothbart MK. Development of Short and Very Short Forms of the Children’s Behavior Questionnaire. Journal of Personality Assessment. 2006;87(1):102-112. doi:10.1207/s15327752jpa8701_09 (\href{https://pubmed.ncbi.nlm.nih.gov/16856791/}{PubMed})
#'
#' Rothbart MΚ, Ahadi SA, Hershey KL. Temperament and Social Behavior in Childhood. Merrill-Palmer Quarterly. 1994;40(1):21-39 (\href{https://www.jstor.org/stable/23087906}{jstore})
#'
#' @param cbq_data a data.frame all items for the Child Behavior Questionnaire following the naming conventions described above
#' @param extra_scale_cols a vector of character strings that begin with 'cbq' but are not scale items. Any columns in cbq_data that begin with 'cbq' but are not scale items must be included here. Default is empty vector.
#' @param does_not_apply_value value used to code a cbq response of "does not apply". Default is NA
#' @inheritParams score_bes
#' @return A dataset with subscale scores for the Child Behavior Questionnaire
#' @examples
#'
#' # scoring for the cbq with IDs, when cbq values range from 0-6, and "does not apply" responses are scored as 7
#' cbq_score_data <- score_cbq(cbq_data, base_zero = TRUE, id = 'ID', does_not_apply_value = 7)
#' 
#' # scoring for the cbq with IDs, when cbq values range from 1-7, and "does not apply" responses are scored as NA
#' cbq_score_data <- score_cbq(cbq_data, base_zero = FALSE, id = 'ID', does_not_apply_value = NA)
#' 
#' \dontrun{
#' }
#'
#'
#' @export

score_cbq <- function(cbq_data, base_zero = TRUE, id, does_not_apply_value = NA, extra_scale_cols = c()) {

    #### 1. Set up/initial checks #####

    # check that cbq_data exist and is a data.frame
    data_arg <- methods::hasArg(cbq_data)

    if (isTRUE(data_arg) & !is.data.frame(cbq_data)) {
        stop("cbq_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("cbq_data must set to the data.frame with amount consumed for each food item")
    }

    # check if id exists
    ID_arg <- methods::hasArg(id)

    if (isTRUE(ID_arg)){
        if (!(id %in% names(cbq_data))) {
            stop("variable name entered as id is not in cbq_data")
        }
    }

    #### 2. Set Up Data #####

    # set up database for results

    ## create empty matrix
    cbq_score_dat <-
      data.frame(
        cbq_activity = rep(NA, nrow(cbq_data)),
        cbq_anger = rep(NA, nrow(cbq_data)),
        cbq_approach = rep(NA, nrow(cbq_data)),
        cbq_attention = rep(NA, nrow(cbq_data)),
        cbq_discomfort = rep(NA, nrow(cbq_data)),
        cbq_soothability = rep(NA, nrow(cbq_data)),
        cbq_fear = rep(NA, nrow(cbq_data)),
        cbq_highintensity_pleasure = rep(NA, nrow(cbq_data)),
        cbq_impulsivity = rep(NA, nrow(cbq_data)),
        cbq_inhibitory_cont = rep(NA, nrow(cbq_data)),
        cbq_lowintensity_pleasure = rep(NA, nrow(cbq_data)),
        cbq_perceptual_sensitivity = rep(NA, nrow(cbq_data)),
        cbq_sadness = rep(NA, nrow(cbq_data)),
        cbq_shyness = rep(NA, nrow(cbq_data)),
        cbq_smile_laughter = rep(NA,  nrow(cbq_data)),
        cbq_surgency = rep(NA, nrow(cbq_data)),
        cbq_neg_affect = rep(NA, nrow(cbq_data)),
        cbq_effortful_cont = rep(NA, nrow(cbq_data))
      )
    
    if (isTRUE(ID_arg)) {
        cbq_score_dat <- data.frame(cbq_data[[id]], cbq_score_dat)
        names(cbq_score_dat)[1] <- id
    }
    
    # assign cbq scale items to cbq_items, excluding columns in extra_scale_cols
    cbq_items <- grep("^cbq", names(cbq_data), value = TRUE) %>% setdiff(extra_scale_cols)
    
    # remove underscore in column names for cbq_items
    names(cbq_data)[names(cbq_data) %in% cbq_items] <- gsub('cbq_', 'cbq', names(cbq_data)[names(cbq_data) %in% cbq_items])
    
    # remove underscore in cbq_items
    cbq_items <- gsub("cbq_", "cbq", cbq_items)
    
    # make copy of data
    cbq_data_edit <- cbq_data
    
    # set "does not apply" values to NA (unless does_not_apply_value is NA)
    if (!is.na(does_not_apply_value)) {
      cbq_data_edit[cbq_items] <- sapply(cbq_items, function (x) ifelse(cbq_data_edit[[x]] == does_not_apply_value, NA, cbq_data_edit[[x]]), simplify = TRUE)
    }

    # check range of data and print warnings
    min <- min(cbq_data_edit[c(cbq_items)], na.rm = TRUE)
    max <- max(cbq_data_edit[c(cbq_items)], na.rm = TRUE)
    
    if (isTRUE(base_zero)){
      if (min < 0 | max > 6) {
        warning("range in CBQ data (excluding does_not_apply_value) is outside expected range given base_zero = TRUE (expected range: 0-6). Scoring may be incorrect")
      } 
    } else {
      if (min < 1 | max > 7) {
        warning("range in CBQ data (excluding does_not_apply_value) is outside expected range given base_zero = FALSE (expected range: 1-7). Scoring may be incorrect")
      } 
    }
    
    # scale to base 1
    if (isTRUE(base_zero)){
      cbq_data_edit[cbq_items] <- sapply(cbq_items, function(x) cbq_data[[x]] + 1, simplify = TRUE)
    }
    
    # calculate reversed scores
    reverse_qs <- c("cbq3", "cbq11", "cbq16", "cbq18", "cbq19", "cbq21", "cbq25",
        "cbq34", "cbq35", "cbq36", "cbq43", "cbq48", "cbq49", "cbq50", "cbq53",
        "cbq54", "cbq56", "cbq60", "cbq61", "cbq68", "cbq74", "cbq75", "cbq78",
        "cbq80", "cbq82", "cbq83", "cbq84", "cbq90", "cbq91", "cbq92", "cbq93")

    cbq_data_edit[reverse_qs] <- sapply(reverse_qs, function (x) ifelse(cbq_data_edit[[x]] == 1, 7, ifelse(cbq_data_edit[[x]] == 2, 6, ifelse(cbq_data_edit[[x]] == 3, 5, ifelse(cbq_data_edit[[x]] == 5, 3, ifelse(cbq_data_edit[[x]] ==  6, 2, ifelse(cbq_data_edit[[x]] == 7, 1, ifelse(cbq_data_edit[[x]] ==  4, 4, NA))))))), simplify = TRUE)
    
    ## Score Subscales

    # Activity Level
    activity_vars <- c("cbq1", "cbq12", "cbq18", "cbq22", "cbq50", "cbq85",
        "cbq93")
    cbq_score_dat[["cbq_activity"]] <- rowMeans(cbq_data_edit[activity_vars], na.rm = TRUE)

    # Anger/Frustration
    anger_vars <- c("cbq2", "cbq14", "cbq30", "cbq40", "cbq61", "cbq87")
    cbq_score_dat[["cbq_anger"]] <- rowMeans(cbq_data_edit[anger_vars], na.rm = TRUE)

    # Approach/Positive Anticipation
    approach_vars <- c("cbq6", "cbq15", "cbq46", "cbq58", "cbq90", "cbq92")
    cbq_score_dat[["cbq_approach"]] <- rowMeans(cbq_data_edit[approach_vars], na.rm = TRUE)

    # Attentional Focusing
    attention_vars <- c("cbq16", "cbq21", "cbq62", "cbq71", "cbq84",
        "cbq89")
    cbq_score_dat[["cbq_attention"]] <- rowMeans(cbq_data_edit[attention_vars], na.rm = TRUE)

    # Discomfort
    discomfort_vars <- c("cbq3", "cbq9", "cbq29", "cbq49", "cbq64", "cbq91")
    cbq_score_dat[["cbq_discomfort"]] <- rowMeans(cbq_data_edit[discomfort_vars], na.rm = TRUE)

    # Falling Reactivity/Soothability
    sooth_vars <- c("cbq25", "cbq34", "cbq44", "cbq59", "cbq66", "cbq75")
    cbq_score_dat[["cbq_soothability"]] <- rowMeans(cbq_data_edit[sooth_vars], na.rm = TRUE)

    # Fear
    fear_vars <- c("cbq17", "cbq23", "cbq35", "cbq41", "cbq63", "cbq68")
    cbq_score_dat[["cbq_fear"]] <- rowMeans(cbq_data_edit[fear_vars], na.rm = TRUE)

    # High Intensity Pleasure
    hi_pleasure_vars <- c("cbq4", "cbq10", "cbq33", "cbq69", "cbq78", "cbq88")
    cbq_score_dat[["cbq_highintensity_pleasure"]] <- rowMeans(cbq_data_edit[hi_pleasure_vars],
        na.rm = TRUE)

    # Impulsivity
    impulsivity_vars <- c("cbq7", "cbq28", "cbq36", "cbq43", "cbq51",
        "cbq82")
    cbq_score_dat[["cbq_impulsivity"]] <- rowMeans(cbq_data_edit[impulsivity_vars],
        na.rm = TRUE)

    # Inhibitory Control
    inhib_vars <- c("cbq38", "cbq45", "cbq53", "cbq67", "cbq73", "cbq81")
    cbq_score_dat[["cbq_inhibitory_cont"]] <- rowMeans(cbq_data_edit[inhib_vars], na.rm = TRUE)

    # Low Intensity Pleasure
    li_pleasure_vars <- c("cbq26", "cbq39", "cbq57", "cbq65", "cbq72", "cbq76",
        "cbq86", "cbq94")
    cbq_score_dat[["cbq_lowintensity_pleasure"]] <- rowMeans(cbq_data_edit[li_pleasure_vars],
        na.rm = TRUE)

    # Perceptual Sensitivity
    percept_vars <- c("cbq5", "cbq13", "cbq24", "cbq32", "cbq47", "cbq83")
    cbq_score_dat[["cbq_perceptual_sensitivity"]] <- rowMeans(cbq_data_edit[percept_vars],
        na.rm = TRUE)

    # Sadness
    sad_vars <- c("cbq8", "cbq20", "cbq27", "cbq31", "cbq54", "cbq56",
        "cbq74")
    cbq_score_dat[["cbq_sadness"]] <- rowMeans(cbq_data_edit[sad_vars], na.rm = TRUE)

    # Shyness
    shy_vars <- c("cbq11", "cbq37", "cbq42", "cbq52", "cbq60", "cbq70")
    cbq_score_dat[["cbq_shyness"]] <- rowMeans(cbq_data_edit[shy_vars], na.rm = TRUE)

    # Smiling and Laughter
    smile_vars <- c("cbq19", "cbq48", "cbq55", "cbq77", "cbq79", "cbq80")
    cbq_score_dat[["cbq_smile_laughter"]] <- rowMeans(cbq_data_edit[smile_vars], na.rm = TRUE)

    # Big 3 - Surgency reverse the shyness scale
    cbq_score_dat[["cbq_shyness_rev"]] <- 8 - cbq_score_dat["cbq_shyness"]

    cbq_score_dat[["cbq_surgency"]] <- rowMeans(cbq_score_dat[c("cbq_activity",
        "cbq_highintensity_pleasure", "cbq_impulsivity", "cbq_shyness")])

    # remove shyness-reveresed scale
    cbq_score_dat <- cbq_score_dat[, !names(cbq_score_dat) == "cbq_shyness_rev"]

    # Big 3 - Negative Affect remove soothability scale
    cbq_score_dat[["cbq_soothability_rev"]] <- 8 - cbq_score_dat["cbq_soothability"]

    cbq_score_dat[["cbq_neg_affect"]] <- rowMeans(cbq_score_dat[c("cbq_anger",
        "cbq_discomfort", "cbq_fear", "cbq_sadness", "cbq_soothability_rev")])

    # remove soothability-reversed scale
    cbq_score_dat <- cbq_score_dat[, !names(cbq_score_dat) == "cbq_soothability_rev"]

    # Big 3 - Effortful Control remove soothability scale
    cbq_score_dat[["cbq_effortful_cont"]] <- rowMeans(cbq_score_dat[c("cbq_attention",
        "cbq_inhibitory_cont", "cbq_lowintensity_pleasure", "cbq_perceptual_sensitivity")])

    #### 3. Clean Export/Scored Data #####
    ## round data
    if (isTRUE(ID_arg)){
        cbq_score_dat[2:ncol(cbq_score_dat)] <- round(cbq_score_dat[2:ncol(cbq_score_dat)], digits = 3)
    } else {
        cbq_score_dat <- round(cbq_score_dat, digits = 3)
    }

    ## merge raw responses with scored data
    if (isTRUE(ID_arg)){
      cbq_phenotype <- merge(cbq_data, cbq_score_dat, by = id)
      
      return(list(score_dat = as.data.frame(cbq_score_dat),
                  bids_phenotype = as.data.frame(cbq_phenotype)))
    } else {
      return(list(score_dat = as.data.frame(cbq_score_dat)))
    }
}

