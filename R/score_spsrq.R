#' score_spsrq: Scored data from the Sensitivity to Punishment and Sensitivity to Reward Questionnaire
#'
#' This function scores the Sensitivity to Punishment and Sensitivity to Reward Questionnaire and provides subscale scores for the following behaviors (2011;  item subscales): Fear/Shyness, Anxiety, Conflict Avoidance, Sensory Reward, Drive, Responsiveness to Social Approval, Impulsivity/Fun Seeking. The original 4 subcales (2004; 34 item subscales): Sensitivity to Punishment, Impulsivity/Fun Seeking, Drive, and Reward Responsiveness.
#'
#' For data to be scored correctly, the data must be prepared according to the following criteria: \cr
#' \itemize{
#'  \item{The data must include all individual questionnaire items}
#'  \item{The  columns/variables must match the following naming convention: 'spsrq#' or 'spsrq_#' where # is the question number (1-48)}
#'  \item{All questionnaire responses must be a numeric value ranging from 0-4 (base_zero = TRUE) or 1-5 (base_zero = FALSE) where: }
#'  \itemize{
#'     \item{For base_zero = TRUE: 0 = Strongly Disagree; 1 = Disagree; 2 = Neither Agree nor Disagree; 3 = Agree; 4 = Strongly Agree}
#'     \item{For base_zero = FALSE: 1 = Strongly Disagree; 2 = Disagree; 3 = Neither Agree nor Disagree; 4 = Agree; 5 = Strongly Agree}
#'   }
#'  \item{Missing values must be coded as NA}
#' }
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Primary References for the Sensitivity to Punishment and Sensitivity to Reward Questionnaire and Scoring:
#' Updated/Revised Caregiver (2011 scoring;  items) - Colder CR, Trucco EM, Lopez HI, et al. Revised reinforcement sensitivity theory and laboratory assessment of BIS and BAS in children. Journal of Research in Personality. 2011;45(2):198-207. doi:10.1016/j.jrp.2011.01.005 (\href{https://pubmed.ncbi.nlm.nih.gov/21603055/}{PubMed})
#'
#' Original Caregiver (2004 scoring; 34 items) - Colder CR, O’Connor RM. Gray’s Reinforcement Sensitivity Model and Child Psychopathology: Laboratory and Questionnaire Assessment of the BAS and BIS. J Abnorm Child Psychol. 2004;32(4):435-451. doi:10.1023/B:JACP.0000030296.54122.b6 (\href{https://pubmed.ncbi.nlm.nih.gov/153055/}{PubMed})
#'
#' Original/adult citation:
#' Torrubia R, Ávila C, Moltó J, Caseras X. The Sensitivity to Punishment and Sensitivity to Reward Questionnaire (SPSRQ) as a measure of Gray’s anxiety and impulsivity dimensions. Personality and Individual Differences. 2001;31(6):837-862. doi:10.1016/S0191-8869(00)00183-5
#'
#' @param spsrq_data a data.frame all items for the Sensitivity to Punishment and Sensitivity to Reward Questionnaire following the naming conventions described above
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @param extra_scale_cols a vector of character strings that begin with 'spsrq' but are not scale items. Any columns in spsrq_data that begin with 'spsrq' but are not scale items must be included here. Default is empty vector.
#' @return A dataset with subscale scores for the Sensitivity to Punishment and Sensitivity to Reward Questionnaire
#' @examples
#'
#' # scoring for the spsrq with IDs
#' spsrq_score_data <- score_spsrq(spsrq_data, id = 'ID')
#'
#'
#' \dontrun{
#' }
#'
#'
#' @export

score_spsrq <- function(spsrq_data, base_zero = TRUE, id, session_id, extra_scale_cols = c()) {

    #### 1. Set up/initial checks #####

    # check that spsrq_data exist and is a data.frame
    data_arg <- methods::hasArg(spsrq_data)

    if (isTRUE(data_arg) & !is.data.frame(spsrq_data)) {
        stop("spsrq_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("spsrq_data must set to the data.frame with amount consumed for each food item")
    }

    # check if id exists
    ID_arg <- methods::hasArg(id)

    if (isTRUE(ID_arg)){
        if (!(id %in% names(spsrq_data))) {
            stop("variable name entered as id is not in spsrq_data")
        }
    }
    
    # check if session_id exists
    sessionID_arg <- methods::hasArg(session_id)
    
    if (isTRUE(sessionID_arg)){
      if (!(id %in% names(spsrq_data))) {
        stop("variable name entered as session_id is not in spsrq_data")
      }
    }

    # check base_zero is logical
    if (!is.logical(base_zero)) {
      stop("base_zero arg must be logical (TRUE/FALSE)")
    }
    
    #### 2. Set Up Data #####

    # set up database for results create empty matrix
    spsrq_score_dat <- data.frame(spsrq_fearshy = rep(NA, nrow(spsrq_data)), spsrq_anxiety = rep(NA, nrow(spsrq_data)), spsrq_conflictavoid = rep(NA, nrow(spsrq_data)), spsrq_impfun = rep(NA, nrow(spsrq_data)), spsrq_drive = rep(NA, nrow(spsrq_data)), spsrq_socialapproval = rep(NA, nrow(spsrq_data)), spsrq_sensoryreward = rep(NA, nrow(spsrq_data)))


    if (isTRUE(ID_arg)) {
      if (isTRUE(sessionID_arg)) {
        spsrq_score_dat <- data.frame(spsrq_data[[id]], spsrq_data[[session_id]], spsrq_score_dat)
        names(spsrq_score_dat)[1:2] <- c(id, session_id)
      } else {
        spsrq_score_dat <- data.frame(spsrq_data[[id]], spsrq_score_dat)
        names(spsrq_score_dat)[1] <- id
      }
    }
    # assign spsrq scale items to spsrq_items, excluding columns in extra_scale_cols
    spsrq_items <- setdiff(grep("^spsrq", names(spsrq_data), value = TRUE), extra_scale_cols)
    
    # remove underscore in column names for spsrq_items
    names(spsrq_data)[names(spsrq_data) %in% spsrq_items] <- gsub('spsrq_', 'spsrq', names(spsrq_data)[names(spsrq_data) %in% spsrq_items])
    
    # remove underscore in spsrq_items
    spsrq_items <- gsub("spsrq_", "spsrq", spsrq_items)
    
    # check range of data and print warnings
    min <- min(spsrq_data[c(spsrq_items)], na.rm = TRUE)
    max <- max(spsrq_data[c(spsrq_items)], na.rm = TRUE)
    
    if (isTRUE(base_zero)){
      if (min < 0 | max > 4) {
        warning("range in SPSRQ data is outside expected range given base_zero = TRUE (expected range: 0-4). Scoring may be incorrect")
      } 
    } else {
      if (min < 1 | max > 5) {
        warning("range in SPSRQ data is outside expected range given base_zero = FALSE (expected range: 1-5). Scoring may be incorrect")
      } 
    }
    
    # re-scale data to base 1
    spsrq_data_edit <- spsrq_data
    
    if (isTRUE(base_zero)){
      spsrq_data_edit[spsrq_items] <- sapply(spsrq_items, function(x) spsrq_data[[x]] + 1, simplify = TRUE)
    }
    
    ## Score Subscales

    # Fear/Shyness
    fearshy_vars <- c("spsrq14", "spsrq16", "spsrq18", "spsrq24", "spsrq26", "spsrq30", "spsrq32", "spsrq34", "spsrq40")
    spsrq_score_dat[["spsrq_fearshy"]] <- rowMeans(spsrq_data_edit[fearshy_vars])

    # Anxiety
    anxiety_vars <- c("spsrq8", "spsrq10", "spsrq20", "spsrq46", "spsrq48")
    spsrq_score_dat[["spsrq_anxiety"]] <- rowMeans(spsrq_data_edit[anxiety_vars])

    # Conflict Avoidance
    conflict_vars <- c("spsrq6", "spsrq22")
    spsrq_score_dat[["spsrq_conflictavoid"]] <- rowMeans(spsrq_data_edit[conflict_vars])

    # Impulsivity/Fun Seeking
    impfun_vars <- c("spsrq21", "spsrq23", "spsrq25", "spsrq35", "spsrq37", "spsrq39")
    spsrq_score_dat[["spsrq_impfun"]] <- rowMeans(spsrq_data_edit[impfun_vars])

    # Drive
    drive_vars <- c("spsrq27", "spsrq41", "spsrq43", "spsrq45", "spsrq47")
    spsrq_score_dat[["spsrq_drive"]] <- rowMeans(spsrq_data_edit[drive_vars])

    # Responsiveness to Social Approval
    social_vars <- c("spsrq7", "spsrq11", "spsrq13", "spsrq19")
    spsrq_score_dat[["spsrq_socialapproval"]] <- rowMeans(spsrq_data_edit[social_vars])

    # Sensory Reward
    reward_vars <- c("spsrq29", "spsrq31")
    spsrq_score_dat[["spsrq_sensoryreward"]] <- rowMeans(spsrq_data_edit[reward_vars])

    #### 3. Clean Export/Scored Data #####
    ## merge raw responses with scored data
    if (isTRUE(ID_arg)){
      if (isTRUE(sessionID_arg)) {
        spsrq_phenotype <- merge(spsrq_data, spsrq_score_dat, by = c(id, session_id))
      } else {
        spsrq_phenotype <- merge(spsrq_data, spsrq_score_dat, by = id)
      }
      
      return(list(score_dat = as.data.frame(spsrq_score_dat),
                  bids_phenotype = as.data.frame(spsrq_phenotype)))
    } else {
      return(list(score_dat = as.data.frame(spsrq_score_dat)))
    }
}

