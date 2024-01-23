#' score_cfq: Scored data from the Child Feeding Questionnaire
#'
#' This function scores the Child Feeding Questionnaire and provides subscale scores for the following behaviors: Perceived Responsibility, Perceived Child Weight, Perceived Parent Weight, Child Weight Concerns, Restriction, Pressure to Eat, and Monitoring
#'
#' To use this function, the data must be prepared accordinng to the following criteria:
#' 1) The data must include all invidividual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'cfq#' where # is the question number (1-31). For the Food Behavior Study, question was 13 skipped for (subscale - Perceived Child Weight) due to age range.
#' 3) All questions must have the numeric value for the choice:
#' 3a) Perceived Weight: 1 - Markedly Underweight, 2 - Underweight, 3 - Average, 4 - Overweight, 5 - Markedly Overweight
#' 3b) Child Weight Concern: 1 - Unconcerned, 2 - Slightly Unconcerned, 3 - Neutral, 4 - Slightly Concerned, 5 - Very Concerned
#' 3c) Restriction and Pressure to Eat: 1 - Disagree, 2 - Slightly Disagree, 3 - Neutral, 4 - Slightly Agree, 5 - Agree
#' 3d) Monitoring: 1 - Never, 2 - Rarely, 3 - Sometimes, 4 - Mostly, 5 - Always
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Birch, L. L., Fisher, J. O., Grimm-Thomas, K., Markey, C. N., Sawyer, R., & Johnson, S. L. (2001). Confirmatory factor analysis of the Child Feeng Questionnaire: A measure of parental attitudes, beliefs and practices about child feeng and obesity proneness. Appetite, 36(3), 201–210. https://doi.org/10.1006/appe.2001.0398 (\href{https://pubmed.ncbi.nlm.nih.gov/11358344/}{PubMed})
#'
#' @param cfq_data a data.frame all items for the Child Feeng Questionnaire following the naming conventions described above
#' @param restriction_split a boolean incating if the Restriction subscale should be split to remove food as reward items. Default = FALSE. The standard Restriction subscale will always be available. If restriction_split = TRUE, then two adtion scales will be computed: 1) cfq_rest_noreward: questions 17-20, 23-24 and 1) cfq_foodreward: questions 21-22
#' @param study a string incating which study collected the data. Currently, only option and default is 'fbs'. This parameter is included so this script can be adapted for future stues that have wider age rang and may collect question 13.
#' @inheritParams fbs_intake
#'
#' @return A dataset with subscale scores for the Child Feeding Questionnaire
#'
#' @examples
#'
#' # scoring for the cfq with IDs
#' cfq_score_data <- score_cfq(cfq_data, parID = 'ID')
#'
#' # scoring for the cfq with extra Restriction subscales
#' cfq_score_data <- score_cfq(cfq_data, restriction_split = TRUE, parID = 'ID')
#'
#' \dontrun{
#' }
#'
#' @seealso Raw data from Qualtrics was processed using the following script: \code{\link{util_fbs_parent_v2dat}}
#'
#'
#' @export

score_cfq <- function(cfq_data, restriction_split = FALSE, study = "fbs", parID) {

    #### 1. Set up/initial checks #####

    # check that cfq_data exist and is a data.frame
    data_arg <- methods::hasArg(cfq_data)

    if (isTRUE(data_arg) & !is.data.frame(cfq_data)) {
        stop("cfq_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("cfq_data must set to the data.frame with amount consumed for each food item")
    }

    # check that restriction_split exist and is a string
    rest_arg <- methods::hasArg(restriction_split)

    if (isTRUE(rest_arg)) {
        if (restriction_split == "true" | restriction_split == "True" | restriction_split ==
            "TRUE") {
            # convert to boolean
            restriction_split = TRUE
        } else if (restriction_split == "false" | restriction_split == "False" |
            restriction_split == "FALSE") {
            # convert to boolean
            restriction_split = FALSE
        }
    }

    if (!isTRUE(restriction_split) & !isFALSE(restriction_split)) {
        stop("restriction_split must be entered as a boolean and can either be: TRUE or FALSE")
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
        if (!(parID %in% names(cfq_data))) {
            stop("variable name entered as parID is not in cfq_data")
        }
    }

    #### 2. Set Up Data #####

    # set up database for results
    if (isTRUE(restriction_split)) {
        # create empty matrix
        cfq_score_dat <- data.frame(cfq_resp = rep(NA, nrow(cfq_data)), cfq_pcw = rep(NA,
            nrow(cfq_data)), cfq_ppw = rep(NA, nrow(cfq_data)), cfq_cwc = rep(NA,
            nrow(cfq_data)), cfq_rest = rep(NA, nrow(cfq_data)), cfq_rest_noreward = rep(NA,
            nrow(cfq_data)), cfq_food_reward = rep(NA, nrow(cfq_data)), cfq_pressure = rep(NA,
            nrow(cfq_data)), cfq_mon = rep(NA, nrow(cfq_data)))
    } else {
        # create empty matrix
        cfq_score_dat <- data.frame(cfq_resp = rep(NA, nrow(cfq_data)), cfq_pcw = rep(NA,
            nrow(cfq_data)), cfq_ppw = rep(NA, nrow(cfq_data)), cfq_cwc = rep(NA,
            nrow(cfq_data)), cfq_rest = rep(NA, nrow(cfq_data)), cfq_pressure = rep(NA,
            nrow(cfq_data)), cfq_mon = rep(NA, nrow(cfq_data)))
    }

    if (isTRUE(ID_arg)) {
        cfq_score_dat <- data.frame(cfq_data[[parID]], cfq_score_dat)
        names(cfq_score_dat)[1] <- parID
    }

    # set up labels for cfq_score_dat
    cfq_score_dat_labels <- lapply(cfq_score_dat, function(x) attributes(x)$label)

    ## Score Subscales

    # Perceived Responsibility
    resp_vars <- c("cfq1", "cfq2", "cfq3")
    cfq_score_dat[["cfq_resp"]] <- rowMeans(cfq_data[resp_vars])

    ## add labels to data
    cfq_score_dat_labels[["cfq_resp"]] <- "CFQ Perceived Responsibility Total Score"

    # Perceived Child Weight
    if (study == "fbs" | study == "FBS") {
        pcw_vars <- c("cfq8", "cfq9", "cfq10", "cfq11", "cfq12")
    } else {
        pcw_vars <- c("cfq8", "cfq9", "cfq10", "cfq11", "cfq12", "cfq13")
    }

    cfq_score_dat[["cfq_pcw"]] <- rowMeans(cfq_data[pcw_vars])

    ## add labels to data
    if (study == "fbs" | study == "FBS") {
        cfq_score_dat_labels[["cfq_pcw"]] <- "CFQ Percieved Child Weight Total Score; question 13 skipped due to age range of children so average of questions 8-12"
    } else {
        cfq_score_dat_labels[["cfq_pcw"]] <- "CFQ Percieved Child Weight Total Score"
    }

    # Perceived Parent Weight
    ppw_vars <- c("cfq4", "cfq5", "cfq6", "cfq7")
    cfq_score_dat[["cfq_ppw"]] <- rowMeans(cfq_data[ppw_vars])

    ## add labels to data
    cfq_score_dat_labels[["cfq_ppw"]] <- "CFQ Perceived Parent Weight Total Score"

    # Child Weight Concern
    cwc_vars <- c("cfq14", "cfq15", "cfq16")
    cfq_score_dat[["cfq_cwc"]] <- rowMeans(cfq_data[cwc_vars])

    ## add labels to data
    cfq_score_dat_labels[["cfq_cwc"]] <- "CFQ Child Weight Concern Total Score"

    # Restriction
    rest_vars <- c("cfq17", "cfq18", "cfq19", "cfq20", "cfq21", "cfq22", "cfq23",
        "cfq24")
    cfq_score_dat[["cfq_rest"]] <- rowMeans(cfq_data[rest_vars])

    ## add labels to data
    cfq_score_dat_labels[["cfq_rest"]] <- "CFQ Restriction Total Score"

    if (isTRUE(restriction_split)) {
        cfq_score_dat[["cfq_rest_noreward"]] <- rowMeans(cfq_data[rest_vars[c(1:4,
            7:8)]])
        cfq_score_dat[["cfq_food_reward"]] <- rowMeans(cfq_data[rest_vars[5:6]])

        ## add labels to data
        cfq_score_dat_labels[["cfq_rest_noreward"]] <- "CFQ Restriction - No Food Reward Total Score"
        cfq_score_dat_labels[["cfq_food_reward"]] <- "CFQ Restriction - Food Reward Total Score"
    }

    # Pressure to Eat
    pressure_vars <- c("cfq25", "cfq26", "cfq27", "cfq28")
    cfq_score_dat[["cfq_pressure"]] <- rowMeans(cfq_data[pressure_vars])

    ## add labels to data
    cfq_score_dat_labels[["cfq_pressure"]] <- "CFQ Pressure to Eat Total Score"

    # Monitoring
    mon_vars <- c("cfq29", "cfq30", "cfq31")
    cfq_score_dat[["cfq_mon"]] <- rowMeans(cfq_data[mon_vars])

    ## add labels to data
    cfq_score_dat_labels[["cfq_mon"]] <- "CFQ Monitoring Total Score"

    #### 3. Clean Export/Scored Data #####
    ## round data
    if (isTRUE(ID_arg)){
        cfq_score_dat[2:ncol(cfq_score_dat)] <- round(cfq_score_dat[2:ncol(cfq_score_dat)], digits = 3)
    } else {
        cfq_score_dat <- round(cfq_score_dat, digits = 3)
    }

    ## make sure the variable labels match in the dataset
    cfq_score_dat = sjlabelled::set_label(cfq_score_dat, label = matrix(unlist(cfq_score_dat_labels,
        use.names = FALSE)))

    return(cfq_score_dat)
}

