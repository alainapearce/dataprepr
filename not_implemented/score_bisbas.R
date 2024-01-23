#' score_bisbas: Scored data from the Behavioral Inhibition System/Behavioral Activation System
#'
#' This function scores the Behavioral Inhibition System (BIS)/Behavioral Activation System (BAS) and provides subscale scores for the following behaviors: BIS, BAS Fun Seeking, BAS Drive, and BAS Reward Responsiveness. Note, this script is used to score the 24-item version, which contains 4 filler questions that are not used for scoring. While the scored questions match the 20-item version exactly, the question numbers differ so this script cannot be sued to score the 20-item version at this time. If this functionality is desired, contact the kellertools package developers.
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'bisbas#' where # is the question number (1-24).
#' 3) Questions must have the numeric value for the choices: 1 - Very True for Me, 2 - Somewhat True for Me, 3 - Somewhat False for Me, 4 - Very False for Me
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Primary References for the child version of the Behavioral Inhibition Scale/Behavioral Activation Scale and Scoring:
#' Muris P, Meesters C, de Kanter E, Timmerman PE. Behavioural inhibition and behavioural activation system scales for children: relationships with Eysenck’s personality traits and psychopathological symptoms. Personality and Individual Differences. 2005;38(4):831-841. doi:10.1016/j.paid.2004.06.007
#'
#' Carver CS, White TL. Behavioral inhibition, behavioral activation, and affective responses to impending reward and punishment: The BIS/BAS Scales. Journal of Personality and Social Psychology. 1994;67(2):319-333. doi:http://dx.doi.org/10.1037/0022-3514.67.2.319.
#'
#'
#' @param bisbas_data a data.frame all items for the Behavioral Inhibition Scale/Behavioral Activation Scale following the naming conventions described above
#' @inheritParams fbs_intake
#'
#' @return A dataset with subscale scores for the Behavioral Inhibition Scale/Behavioral Activation Scale
#' @examples
#'
#' # scoring for the bisbas with IDs
#' bisbas_score_data <- score_bisbas(bisbas_data, parID = 'ID')
#'
#'
#' \dontrun{
#' }
#'
#' @seealso Raw data from Qualtrics was processed using the following script: \code{\link{util_fbs_parent_v3dat}}
#'
#'
#' @export

score_bisbas <- function(bisbas_data, parID) {

    #### 1. Set up/initial checks #####

    # check that bisbas_data exist and is a data.frame
    data_arg <- methods::hasArg(bisbas_data)

    if (isTRUE(data_arg) & !is.data.frame(bisbas_data)) {
        stop("bisbas_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("bisbas_data must set to the data.frame with amount consumed for each food item")
    }

    # check if parID exists
    ID_arg <- methods::hasArg(parID)

    if (isTRUE(ID_arg)){
        if (!(parID %in% names(bisbas_data))) {
            stop("variable name entered as parID is not in bisbas_data")
        }
    }

    #### 2. Set Up Data #####

    # set up database for results create empty matrix
    bisbas_score_dat <- data.frame(bis = rep(NA, nrow(bisbas_data)), bas = rep(NA, nrow(bisbas_data)), bas_funseeking = rep(NA, nrow(bisbas_data)), bas_drive = rep(NA, nrow(bisbas_data)), bas_rewardresp = rep(NA, nrow(bisbas_data)))


    if (isTRUE(ID_arg)) {
        bisbas_score_dat <- data.frame(bisbas_data[[parID]], bisbas_score_dat)
        names(bisbas_score_dat)[1] <- parID
    }

    # set up labels for bisbas_score_dat
    bisbas_score_dat_labels <- lapply(bisbas_score_dat, function(x) attributes(x)$label)

    # calculate reversed scores
    reverse_qs <- c("bisbas2", "bisbas22")

    for (var in 1:length(reverse_qs)) {
        var_name <- reverse_qs[var]
        reverse_name <- paste0(var_name, "_rev")

        bisbas_data[[reverse_name]] <- ifelse(is.na(bisbas_data[[var_name]]), NA, ifelse(bisbas_data[[var_name]] == 1, 4, ifelse(bisbas_data[[var_name]] == 2, 3,  ifelse(bisbas_data[[var_name]] == 3, 2, 1))))
    }

    ## Score Subscales

    # BIS
    bis_vars <- c("bisbas16", "bisbas24", "bisbas8", "bisbas13", "bisbas2_rev", "bisbas19", "bisbas22_rev")
    bisbas_score_dat[["bis"]] <- rowMeans(bisbas_data[bis_vars])

    ## add labels to data
    bisbas_score_dat_labels[["bis"]] <- "BIS/BAS BIS Score"

    #  BAS Fun Seeking
    funseek_vars <- c("bisbas10", "bisbas20", "bisbas5", "bisbas15")
    bisbas_score_dat[["bas_funseeking"]] <- rowMeans(bisbas_data[funseek_vars])

    ## add labels to data
    bisbas_score_dat_labels[["bas_funseeking"]] <- "BIS/BAS  BAS Fun Seeking Score"

    # BAS Drive
    drive_vars <- c("bisbas9", "bisbas3", "bisbas12", "bisbas21")
    bisbas_score_dat[["bas_drive"]] <- rowMeans(bisbas_data[drive_vars])

    ## add labels to data
    bisbas_score_dat_labels[["bas_drive"]] <- "BIS/BAS BAS Drive Score"

    # BAS Reward Responsiveness
    reward_vars <- c("bisbas7", "bisbas4", "bisbas18", "bisbas23", "bisbas14")
    bisbas_score_dat[["bas_rewardresp"]] <- rowMeans(bisbas_data[reward_vars])

    ## add labels to data
    bisbas_score_dat_labels[["bas_rewardresp"]] <- "BIS/BAS BAS Reward Responsiveness Score"

    # BAS
    bisbas_score_dat[["bas"]] <- rowMeans(bisbas_data[c(funseek_vars, drive_vars, reward_vars)])

    ## add labels to data
    bisbas_score_dat_labels[["bas"]] <- "BIS/BAS BAS Total Score"

    #### 3. Clean Export/Scored Data #####

    ## round data
    if (isTRUE(ID_arg)){
        bisbas_score_dat[2:ncol(bisbas_score_dat)] <- round(bisbas_score_dat[2:ncol(bisbas_score_dat)], digits = 3)
    } else {
        bisbas_score_dat <- round(bisbas_score_dat, digits = 3)
    }

    ## make sure the variable labels match in the dataset
    bisbas_score_dat = sjlabelled::set_label(bisbas_score_dat, label = matrix(unlist(bisbas_score_dat_labels, use.names = FALSE)))

    return(bisbas_score_dat)
}

