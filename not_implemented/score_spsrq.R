#' score_spsrq: Scored data from the Sensitivity to Punishment and Sensitivity to Reward Questionnaire
#'
#' This function scores the Sensitivity to Punishment and Sensitivity to Reward Questionnaire and provides subscale scores for the following behaviors (2011; 48 item subscales): Fear/Shyness, Anxiety, Conflict Avoidance, Sensory Reward, Drive, Responsiveness to Social Approval, Impulsivity/Fun Seeking. The original 4 subcales (2004; 34 item subscales): Sensitivity to Punishment, Impulsivity/Fun Seeking, Drive, and Reward Responsiveness.
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'spsrq#' where # is the question number (1-48)
#' 3) Questions 1-36 must have the numeric value for the choices: 1 - Strongly Disagree, 2 - Disagree, 3 - Neither Agree nor Disagree, 4 - Agree, 5 - Strongly Agree.
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Primary References for the Sensitivity to Punishment and Sensitivity to Reward Questionnaire and Scoring:
#' Updated/Revised Caregiver (2011 scoring; 48 items) - Colder CR, Trucco EM, Lopez HI, et al. Revised reinforcement sensitivity theory and laboratory assessment of BIS and BAS in children. Journal of Research in Personality. 2011;45(2):198-207. doi:10.1016/j.jrp.2011.01.005 (\href{https://pubmed.ncbi.nlm.nih.gov/21603055/}{PubMed})
#'
#' Original Caregiver (2004 scoring; 34 items) - Colder CR, O’Connor RM. Gray’s Reinforcement Sensitivity Model and Child Psychopathology: Laboratory and Questionnaire Assessment of the BAS and BIS. J Abnorm Child Psychol. 2004;32(4):435-451. doi:10.1023/B:JACP.0000030296.54122.b6 (\href{https://pubmed.ncbi.nlm.nih.gov/15305548/}{PubMed})
#'
#' Original/adult citation:
#' Torrubia R, Ávila C, Moltó J, Caseras X. The Sensitivity to Punishment and Sensitivity to Reward Questionnaire (SPSRQ) as a measure of Gray’s anxiety and impulsivity dimensions. Personality and Individual Differences. 2001;31(6):837-862. doi:10.1016/S0191-8869(00)00183-5
#'
#' @param spsrq_data a data.frame all items for the Sensitivity to Punishment and Sensitivity to Reward Questionnaire following the naming conventions described above
#' @inheritParams fbs_intake
#'
#' @return A dataset with subscale scores for the Sensitivity to Punishment and Sensitivity to Reward Questionnaire
#' @examples
#'
#' # scoring for the spsrq with IDs
#' spsrq_score_data <- score_spsrq(spsrq_data, parID = 'ID')
#'
#'
#' \dontrun{
#' }
#'
#' @seealso Raw data from Qualtrics was processed using the following script: \code{\link{util_fbs_parent_v3dat}}
#'
#'
#' @export

score_spsrq <- function(spsrq_data, parID) {

    #### 1. Set up/initial checks #####

    # check that spsrq_data exist and is a data.frame
    data_arg <- methods::hasArg(spsrq_data)

    if (isTRUE(data_arg) & !is.data.frame(spsrq_data)) {
        stop("spsrq_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("spsrq_data must set to the data.frame with amount consumed for each food item")
    }

    # check if parID exists
    ID_arg <- methods::hasArg(parID)

    if (isTRUE(ID_arg)){
        if (!(parID %in% names(spsrq_data))) {
            stop("variable name entered as parID is not in spsrq_data")
        }
    }

    #### 2. Set Up Data #####

    # set up database for results create empty matrix
    spsrq_score_dat <- data.frame(spsrq34_punishment = rep(NA, nrow(spsrq_data)), spsrq34_impfun = rep(NA, nrow(spsrq_data)), spsrq34_drive = rep(NA, nrow(spsrq_data)), spsrq34_rewardresp = rep(NA, nrow(spsrq_data)), spsrq48_fearshy = rep(NA, nrow(spsrq_data)), spsrq48_anxiety = rep(NA, nrow(spsrq_data)), spsrq48_conflictavoid = rep(NA, nrow(spsrq_data)), spsrq48_impfun = rep(NA, nrow(spsrq_data)), spsrq48_drive = rep(NA, nrow(spsrq_data)), spsrq48_socialapproval = rep(NA, nrow(spsrq_data)), spsrq48_sensoryreward = rep(NA, nrow(spsrq_data)))


    if (isTRUE(ID_arg)) {
        spsrq_score_dat <- data.frame(spsrq_data[[parID]], spsrq_score_dat)
        names(spsrq_score_dat)[1] <- parID
    }

    # set up labels for spsrq_score_dat
    spsrq_score_dat_labels <- lapply(spsrq_score_dat, function(x) attributes(x)$label)

    ## Score Subscales

    # Connor 2004 - 4 factor
    # Sensitivity to Punishment
    sp34_vars <- c("spsrq3", "spsrq5", "spsrq14", "spsrq16", "spsrq18", "spsrq26", "spsrq28", "spsrq30", "spsrq32", "spsrq34", "spsrq36", "spsrq40", "spsrq44", "spsrq46", "spsrq48")
    spsrq_score_dat[["spsrq34_punishment"]] <- rowMeans(spsrq_data[sp34_vars])

    ## add labels to data
    spsrq_score_dat_labels[["spsrq34_punishment"]] <- "SPSRQ-34 Sensitivity to Punishment Score"

    # Impulsivity/Fun Seeking
    impfun34_vars <- c("spsrq21", "spsrq25", "spsrq33", "spsrq35", "spsrq37", "spsrq39", "spsrq47")
    spsrq_score_dat[["spsrq34_impfun"]] <- rowMeans(spsrq_data[impfun34_vars])

    ## add labels to data
    spsrq_score_dat_labels[["spsrq34_impfun"]] <- "SPSRQ-34 Impulsivity/Fun Seeking Score"

    # Drive
    drive34_vars <- c("spsrq27", "spsrq41", "spsrq43", "spsrq45")
    spsrq_score_dat[["spsrq34_drive"]] <- rowMeans(spsrq_data[drive34_vars])

    ## add labels to data
    spsrq_score_dat_labels[["spsrq34_drive"]] <- "SPSRQ-34 Drive Score"

    # Reward Responsiveness
    reward34_vars <- c("spsrq2", "spsrq7", "spsrq9", "spsrq15", "spsrq17", "spsrq19", "spsrq23")
    spsrq_score_dat[["spsrq34_rewardresp"]] <- rowMeans(spsrq_data[reward34_vars])

    ## add labels to data
    spsrq_score_dat_labels[["spsrq34_rewardresp"]] <- "SPSRQ-34 Reward Responsiveness Score"

    # Fear/Shyness
    fearshy48_vars <- c("spsrq14", "spsrq16", "spsrq18", "spsrq24", "spsrq26", "spsrq30", "spsrq32", "spsrq34", "spsrq40")
    spsrq_score_dat[["spsrq48_fearshy"]] <- rowMeans(spsrq_data[fearshy48_vars])

    ## add labels to data
    spsrq_score_dat_labels[["spsrq48_fearshy"]] <- "SPSRQ-48 Fear/Shyness Score"

    # Anxiety
    anxiety48_vars <- c("spsrq8", "spsrq10", "spsrq20", "spsrq46", "spsrq48")
    spsrq_score_dat[["spsrq48_anxiety"]] <- rowMeans(spsrq_data[anxiety48_vars])

    ## add labels to data
    spsrq_score_dat_labels[["spsrq48_anxiety"]] <- "SPSRQ-48 Anxiety Score"

    # Conflict Avoidance
    conflict48_vars <- c("spsrq6", "spsrq22")
    spsrq_score_dat[["spsrq48_conflictavoid"]] <- rowMeans(spsrq_data[conflict48_vars])

    ## add labels to data
    spsrq_score_dat_labels[["spsrq48_conflictavoid"]] <- "SPSRQ-48 Conflict Avoidence Score"

    # Impulsivity/Fun Seeking
    impfun48_vars <- c("spsrq21", "spsrq23", "spsrq25", "spsrq35", "spsrq37", "spsrq39")
    spsrq_score_dat[["spsrq48_impfun"]] <- rowMeans(spsrq_data[impfun48_vars])

    ## add labels to data
    spsrq_score_dat_labels[["spsrq48_impfun"]] <- "SPSRQ-48 Impulsivity/Fun Seeking Score"

    # Drive
    drive48_vars <- c("spsrq27", "spsrq41", "spsrq43", "spsrq45", "spsrq47")
    spsrq_score_dat[["spsrq48_drive"]] <- rowMeans(spsrq_data[drive48_vars])

    ## add labels to data
    spsrq_score_dat_labels[["spsrq48_drive"]] <- "SPSRQ-48 Drive Score"

    # Responsiveness to Social Approval
    social48_vars <- c("spsrq7", "spsrq11", "spsrq13", "spsrq19")
    spsrq_score_dat[["spsrq48_socialapproval"]] <- rowMeans(spsrq_data[social48_vars])

    ## add labels to data
    spsrq_score_dat_labels[["spsrq48_socialapproval"]] <- "SPSRQ-48 Responsiveness to Social Approval Score"

    # Sensory Reward
    reward48_vars <- c("spsrq29", "spsrq31")
    spsrq_score_dat[["spsrq48_sensoryreward"]] <- rowMeans(spsrq_data[reward48_vars])

    ## add labels to data
    spsrq_score_dat_labels[["spsrq48_sensoryreward"]] <- "SPSRQ-48 Sensory Reward Score"

    #### 3. Clean Export/Scored Data #####
    ## round data
    if (isTRUE(ID_arg)){
        spsrq_score_dat[2:ncol(spsrq_score_dat)] <- round(spsrq_score_dat[2:ncol(spsrq_score_dat)], digits = 3)
    } else {
        spsrq_score_dat <- round(spsrq_score_dat, digits = 3)
    }

    ## make sure the variable labels match in the dataset
    spsrq_score_dat = sjlabelled::set_label(spsrq_score_dat, label = matrix(unlist(spsrq_score_dat_labels,
        use.names = FALSE)))

    return(spsrq_score_dat)
}

