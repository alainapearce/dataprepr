#' score_cshqa: Scored data from the Child Sleep Habits Questionnaire - Abbreviated
#'
#' This function scores the Child Sleep Habits Questionnaire - Abbreviated and provides both total scores and subscale scores for the following behaviors: Bedtime Resistance, Sleep Onset Delay, Sleep Duration, Sleep Anxiety, Night Wakings, Parasomnias, Sleep Disordered Breathing, and Daytime Sleepiness. For the Food and Brain Study, the Morning Wake Up subscale was left off the questionnaire
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'cshq_a#' where # is the question number
#' 3) All questions must have the numeric value for the choice: 1 - Never, 2 - Rarely, 3 - Sometimes, 4 - Usually, 5 - Always
#' 4) This script will apply reverse scoring so all levels must be true to the scale described above
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#'
#' Full Measure:
#' Owens, J. A., Spirito, A., & McGuinn, M. (2000). The Children’s Sleep Habits Questionnaire (CSHQ): Psychometric Properties of A Survey Instrument for School-Aged Children. SLEEP, 23(8), 1043–1052. (\href{https://pubmed.ncbi.nlm.nih.gov/11145319/}{PubMed})
#'
#' Abreviated Scoring: C
#' hawla, J. K., Howard, A., Burgess, S., & Heussler, H. (2021). Sleep problems in Australian children with Down syndrome: The need for greater awareness. Sleep Medicine, 78, 81–87. https://doi.org/10.1016/j.sleep.2020.12.022 (\href{https://pubmed.ncbi.nlm.nih.gov/33412456/}{PubMed})
#'
#' @param cshqa_data a data.frame all items for the Child Sleep Habits Questionnaire - Abbreviated following the naming conventions described above
#' @param study a string indicating which study collected the data. Currently, only option and default is 'fbs'. This parameter is included so this script can be adapted for future studies that collect all subscales.
#' @inheritParams fbs_intake
#'
#'
#' @return A dataset with total and subscale scores for the Child Sleep Habits Questionnaire - Abbreviated
#'
#' @examples
#'
#' # scoring for FBS study
#' cshqa_score_date <- score_cshqa(cshqa_data, study = 'fbs', parID = 'ID')
#'
#' \dontrun{
#' }
#'
#' @seealso Raw data from Qualtrics was processed using the following script: \code{\link{util_fbs_parent_v2dat}}
#'
#'
#' @export

score_cshqa <- function(cshqa_data, study = 'fbs', parID) {

    #### 1. Set up/initial checks #####

    # check that cshqa_data exist and is a data.frame
    data_arg <- methods::hasArg(cshqa_data)

    if (isTRUE(data_arg) & !is.data.frame(cshqa_data)) {
        stop("cshqa_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("cshqa_data must set to the data.frame with amount consumed for each food item")
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
        if (!(parID %in% names(cshqa_data))) {
            stop("variable name entered as parID is not in cshqa_data")
        }
    }

    #### 2. Set Up Data #####

    # set up database for results
    ## create empty matrix
    if (study == 'fbs' | study == 'FBS'){
        cshqa_score_dat <- data.frame(cshqa_bedtime_resit = rep(NA, nrow(cshqa_data)),
                                      cshqa_sleep_delay = rep(NA, nrow(cshqa_data)),
                                      cshqa_sleepdur = rep(NA, nrow(cshqa_data)),
                                      cshqa_anxiety = rep(NA, nrow(cshqa_data)),
                                      cshqa_nightwake = rep(NA, nrow(cshqa_data)),
                                      cshqa_parasomnias = rep(NA, nrow(cshqa_data)),
                                      cshqa_dis_breathing = rep(NA, nrow(cshqa_data)),
                                      cshqa_total = rep(NA, nrow(cshqa_data)))
    } else {
        cshqa_score_dat <- data.frame(cshqa_bedtime_resit = rep(NA, nrow(cshqa_data)),
                                      cshqa_sleep_delay = rep(NA, nrow(cshqa_data)),
                                      cshqa_sleepdur = rep(NA, nrow(cshqa_data)),
                                      cshqa_anxiety = rep(NA, nrow(cshqa_data)),
                                      cshqa_nightwake = rep(NA, nrow(cshqa_data)),
                                      cshqa_parasomnias = rep(NA, nrow(cshqa_data)),
                                      cshqa_dis_breathing = rep(NA, nrow(cshqa_data)),
                                      cshqa_daysleepy = rep(NA, nrow(cshqa_data)),
                                      cshqa_total = rep(NA, nrow(cshqa_data)))
    }

    if (isTRUE(ID_arg)) {
        cshqa_score_dat <- data.frame(cshqa_data[[parID]], cshqa_score_dat)
        names(cshqa_score_dat)[1] <- parID
    }

    # set up labels for cshqa_score_dat
    cshqa_score_dat_labels <- lapply(cshqa_score_dat, function(x) attributes(x)$label)

    # calculate reversed scores
    ## FBS study only had 18 questions
    if (study == 'fbs' | study == 'FBS'){
        reverse_qs <- c('cshq_a1', 'cshq_a2', 'cshq_a3', 'cshq_a10')
    } else {
        reverse_qs <- c('cshq_a1', 'cshq_a2', 'cshq_a3', 'cshq_a10', 'cshq_a19')
    }

    for (var in 1:length(reverse_qs)){
        var_name <- reverse_qs[var]
        reverse_name <- paste0(var_name, '_rev')

        cshqa_data[[reverse_name]] <- ifelse(is.na(cshqa_data[[var_name]]), NA, ifelse(cshqa_data[[var_name]] == 1, 5, ifelse(cshqa_data[[var_name]] == 2, 4, ifelse(cshqa_data[[var_name]] == 4, 2, ifelse(cshqa_data[[var_name]] == 5, 1, 3)))))
    }

    ## need to resolve subscale scoring! ###
    #cshq_a5, cshq_a6, cshq_a16, cshq_a20 ###

    # Bedtime Resistance
    bedtime_vars <- c('cshq_a1_rev', 'cshq_a3_rev', 'cshq_a4', 'cshq_a7', 'cshq_a8')
    cshqa_score_dat[['cshqa_bedtime_resit']] <- rowSums(cshqa_data[bedtime_vars])

    ##add labels to data
    cshqa_score_dat_labels[['cshqa_bedtime_resit']] <- 'CSHQ-A Bedtime Resistance Total Score'

    # Sleep Onset Delay
    cshqa_score_dat[['cshqa_sleep_delay']] <- as.numeric(cshqa_data[['cshq_a2_rev']])

    ##add labels to data
    cshqa_score_dat_labels[['cshqa_sleep_delay']] <- 'CSHQ-A Sleep Onset Delay Total Score'

    # Sleep Duration
    cshqa_score_dat[['cshqa_sleepdur']] <- as.numeric(cshqa_data[['cshq_a10_rev']])

    ##add labels to data
    cshqa_score_dat_labels[['cshqa_sleepdur']] <- 'CSHQ-A Sleep Duration Total Score'

    # Sleep Anxiety
    sleepanx_vars <- c('cshq_a7', 'cshq_a9')
    cshqa_score_dat[['cshqa_anxiety']] <- rowSums(cshqa_data[sleepanx_vars])

    ##add labels to data
    cshqa_score_dat_labels[['cshqa_anxiety']] <- 'CSHQ-A Sleep Anxiety Total Score'

    # Night Wakings
    nightwake_vars <- c('cshq_a12',  'cshq_a17', 'cshq_a18')
    cshqa_score_dat[['cshqa_nightwake']] <- rowSums(cshqa_data[nightwake_vars])

    ##add labels to data
    cshqa_score_dat_labels[['cshqa_nightwake']] <- 'CSHQ-A Night Wakings Total Score'

    # Parasomnias
    parasomnias_vars <- c('cshq_a11', 'cshq_a13', 'cshq_a15')
    cshqa_score_dat[['cshqa_parasomnias']] <- rowSums(cshqa_data[parasomnias_vars])

    ##add labels to data
    cshqa_score_dat_labels[['cshqa_parasomnias']] <- 'CSHQ-A Parasomnias Total Score'

    # Sleep Disordered Breathing
    cshqa_score_dat[['cshqa_dis_breathing']] <- as.numeric(cshqa_data[['cshq_a14']])

    ##add labels to data
    cshqa_score_dat_labels[['cshqa_dis_breathing']] <- 'CSHQ-A Sleep Disordered Breathing Total Score'

    # Daytime Sleepiness
    if (study != 'fbs' & study != 'FBS'){
        morning_vars <- c('cshq_a19_rev', 'cshq_a21', 'cshq_a22')
        cshqa_score_dat[['cshqa_daysleepy']] <- rowSums(cshqa_data[morningwake_vars])

        ##add labels to data
        cshqa_score_dat_labels[['cshqa_daysleepy']] <- 'CSHQ-A Morning Wake Up Total Score'
    }

    # Total Score
    if (study == 'fbs' | study == 'FBS'){
        all_vars <- c('cshq_a1_rev', 'cshq_a2_rev', 'cshq_a3_rev', 'cshq_a4', 'cshq_a5', 'cshq_a6', 'cshq_a7', 'cshq_a8', 'cshq_a9', 'cshq_a10_rev', 'cshq_a11', 'cshq_a12', 'cshq_a13', 'cshq_a14', 'cshq_a15', 'cshq_a16',  'cshq_a17', 'cshq_a18')

        cshqa_score_dat[['cshqa_total']] <- rowSums(cshqa_data[all_vars])

        ##add labels to data
        cshqa_score_dat_labels[['cshqa_total']] <- 'CSHQ-A Total Score - questions 19-22 omitted so cannot compare to total sleep problmes cuttoffs'

    } else {
        all_vars <- c('cshq_a1_rev', 'cshq_a2_rev', 'cshq_a3_rev', 'cshq_a4', 'cshq_a5', 'cshq_a6', 'cshq_a7', 'cshq_a8', 'cshq_a9', 'cshq_a10_rev', 'cshq_a11', 'cshq_a12', 'cshq_a13', 'cshq_a14', 'cshq_a15', 'cshq_a16',  'cshq_a17', 'cshq_a18', 'cshq_a19_rev', 'cshq_a20', 'cshq_a21', 'cshq_a22')

        cshqa_score_dat[['cshqa_total']] <- rowSums(cshqa_data[all_vars])

        ##add labels to data
        cshqa_score_dat_labels[['cshqa_total']] <- 'CSHQ-A Total Score'
    }

    #### 3. Clean Export/Scored Data #####

    ## make sure the variable labels match in the dataset
    cshqa_score_dat = sjlabelled::set_label(cshqa_score_dat, label = matrix(unlist(cshqa_score_dat_labels, use.names = FALSE)))

    return(cshqa_score_dat)
}

