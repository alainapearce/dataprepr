#' score_bisbas: Scored data from the Behavioral Inhibition System/Behavioral Activation System
#'
#' This function scores the Behavioral Inhibition System (BIS)/Behavioral Activation System (BAS) and provides subscale scores for the following behaviors: BIS, BAS Fun Seeking, BAS Drive, and BAS Reward Responsiveness. Note, this script is used to score the 24-item version, which contains 4 filler questions that are not used for scoring. While the scored questions match the 20-item version exactly, the question numbers differ so this script cannot be sued to score the 20-item version at this time.
#'
#' For data to be scored correctly, the data must be prepared according to the following criteria: \cr
#' \itemize{
#'  \item{The data must include all individual questionnaire items}
#'  \item{The columns/variables must match the following naming convention: 'bisbas#' or 'bisbas_#' where # is the question number (1-24)}
#'  \item{All questionnaire responses must be a numeric value ranging from 0-3 (base_zero = TRUE) or 1-4 (base_zero = FALSE) where: }
#'  \itemize{
#'     \item{For base_zero = TRUE: 0 = Very True for Me; 1 = Somewhat True for Me; 2 = Somewhat False for Me; 3 = Very False for Me}
#'     \item{For base_zero = FALSE: 1 = Very True for Me; 2 = Somewhat True for Me; 3 = Somewhat False for Me; 4 = Very False for Me}
#'   }
#'  \item{Missing values must be coded as NA}
#'  \item{Values must not be reversed scored. This script will apply reverse scoring so all levels must be true to the scale described above}
#'  \itemize{
#'     \item{All items except 2 and 22 will be reverse scored so that higher BIS scores = higher behavioral inhibition and higher BAS scores = higher behavioral approach}
#'   }
#' }
#' \cr
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
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @param extra_scale_cols a vector of character strings that begin with 'bisbas' but are not scale items. Any columns in bisbas_data that begin with 'bisbas' but are not scale items must be included here. Default is empty vector.
#'
#'
#' @return A dataset with subscale scores for the Behavioral Inhibition Scale/Behavioral Activation Scale
#' @examples
#'
#' # scoring for the bisbas with IDs
#' bisbas_score_data <- score_bisbas(bisbas_data, id = 'ID')
#'
#'
#' \dontrun{
#' }
#'
#'
#' @export

score_bisbas <- function(bisbas_data, pna_value, base_zero = TRUE, id, session_id, extra_scale_cols = c()) {
  
  #### 1. Set up/initial checks #####
  
  # check that bisbas_data exist and is a data.frame
  data_arg <- methods::hasArg(bisbas_data)
  
  if (isTRUE(data_arg) & !is.data.frame(bisbas_data)) {
    stop("bisbas_data must be entered as a data.frame")
  } else if (isFALSE(data_arg)) {
    stop("bisbas_data must set to the data.frame with amount consumed for each food item")
  }
  
  # check if id exists
  ID_arg <- methods::hasArg(id)
  
  if (isTRUE(ID_arg)){
    if (!(id %in% names(bisbas_data))) {
      stop("variable name entered as id is not in bisbas_data")
    }
  }
  
  # check if session_id exists
  sessionID_arg <- methods::hasArg(session_id)
  
  if (isTRUE(sessionID_arg)){
    if (!(id %in% names(bisbas_data))) {
      stop("variable name entered as session_id is not in bisbas_data")
    }
  }
  
  # check base_zero is logical
  if (!is.logical(base_zero)) {
    stop("base_zero arg must be logical (TRUE/FALSE)")
  }
  
  #### 2. Set Up Data #####
  
  # set up database for results create empty matrix
  bisbas_score_dat <- data.frame(bis = rep(NA, nrow(bisbas_data)), bas = rep(NA, nrow(bisbas_data)), bas_funseeking = rep(NA, nrow(bisbas_data)), bas_drive = rep(NA, nrow(bisbas_data)), bas_rewardresp = rep(NA, nrow(bisbas_data)))
  
  
  if (isTRUE(ID_arg)) {
    if (isTRUE(sessionID_arg)) {
      bisbas_score_dat <- data.frame(bisbas_data[[id]], bisbas_data[[session_id]], bisbas_score_dat)
      names(bisbas_score_dat)[1:2] <- c(id, session_id)
    } else {
      bisbas_score_dat <- data.frame(bisbas_data[[id]], bisbas_score_dat)
      names(bisbas_score_dat)[1] <- id
    }
  }
  
  # assign bisbas scale items to bisbas_items, excluding columns in extra_scale_cols
  bisbas_items <- setdiff(grep("^bisbas", names(bisbas_data), value = TRUE), extra_scale_cols)
  
  # remove underscore in column names for bisbas_items
  names(bisbas_data)[names(bisbas_data) %in% bisbas_items] <- gsub('bisbas_', 'bisbas', names(bisbas_data)[names(bisbas_data) %in% bisbas_items])
  
  # remove underscore in bisbas_items
  bisbas_items <- gsub("bisbas_", "bisbas", bisbas_items)
  
  # if pna_value arg, replace not applicable values with NA
  if (isTRUE(methods::hasArg(pna_value))) {
    
    # replace pna_value with NA in pcw_vars
    bisbas_data[bisbas_items] <- lapply(bisbas_data[bisbas_items] , function(x) ifelse(x == pna_value, NA, x))
    
  }
  
  # check range of data and print warnings
  min <- min(bisbas_data[c(bisbas_items)], na.rm = TRUE)
  max <- max(bisbas_data[c(bisbas_items)], na.rm = TRUE)
  
  if (isTRUE(base_zero)){
    if (min < 0 | max > 3) {
      warning("range in BISBAS data is outside expected range given base_zero = TRUE (expected range: 0-3). Scoring may be incorrect")
    } 
  } else {
    if (min < 1 | max > 4) {
      warning("range in BISBAS data is outside expected range given base_zero = FALSE (expected range: 1-4). Scoring may be incorrect")
    } 
  }
  
  # re-scale data to base 1
  bisbas_data_edit <- bisbas_data
  
  if (isTRUE(base_zero)){
    bisbas_data_edit[bisbas_items] <- sapply(bisbas_items, function(x) bisbas_data[[x]] + 1, simplify = TRUE)
  }
  
  # calculate reversed scores

  # reverse all items except 2 and 22 
  reverse_qs <- c("bisbas1", "bisbas3", "bisbas4", "bisbas5", "bisbas6", "bisbas7", "bisbas8", "bisbas9", "bisbas10", "bisbas11", "bisbas12", "bisbas13", "bisbas14", "bisbas15", "bisbas16", "bisbas17", "bisbas18", "bisbas19", "bisbas20", "bisbas21", "bisbas23", "bisbas24")
  
  for (var in 1:length(reverse_qs)) {
    var_name <- reverse_qs[var]
    reverse_name <- paste0(var_name, "_rev")
    
    bisbas_data_edit[[reverse_name]] <- ifelse(bisbas_data_edit[[var_name]] == 1, 4, ifelse(bisbas_data_edit[[var_name]] == 2, 3,  ifelse(bisbas_data_edit[[var_name]] == 3, 2,  ifelse(bisbas_data_edit[[var_name]] == 4, 1, NA))))
    
  }
  
  ## Score Subscales
  
  # BIS
  bis_vars <- c("bisbas16_rev", "bisbas24_rev", "bisbas8_rev", "bisbas13_rev", "bisbas2", "bisbas19_rev", "bisbas22")
  bisbas_score_dat[["bis"]] <- rowMeans(bisbas_data_edit[bis_vars])
  
  #  BAS Fun Seeking
  funseek_vars <- c("bisbas10_rev", "bisbas20_rev", "bisbas5_rev", "bisbas15_rev")
  bisbas_score_dat[["bas_funseeking"]] <- rowMeans(bisbas_data_edit[funseek_vars])
  
  # BAS Drive
  drive_vars <- c("bisbas9_rev", "bisbas3_rev", "bisbas12_rev", "bisbas21_rev")
  bisbas_score_dat[["bas_drive"]] <- rowMeans(bisbas_data_edit[drive_vars])
  
  # BAS Reward Responsiveness
  reward_vars <- c("bisbas7_rev", "bisbas4_rev", "bisbas18_rev", "bisbas23_rev", "bisbas14_rev")
  bisbas_score_dat[["bas_rewardresp"]] <- rowMeans(bisbas_data_edit[reward_vars])
  
  # BAS
  bisbas_score_dat[["bas"]] <- rowMeans(bisbas_data_edit[c(funseek_vars, drive_vars, reward_vars)])
  
  #### 3. Clean Export/Scored Data #####
  
  ## round data
  if (isTRUE(ID_arg)){
    bisbas_score_dat[2:ncol(bisbas_score_dat)] <- round(bisbas_score_dat[2:ncol(bisbas_score_dat)], digits = 3)
  } else {
    bisbas_score_dat <- round(bisbas_score_dat, digits = 3)
  }
  

  ## merge raw responses with scored data
  if (isTRUE(ID_arg)){
    if (isTRUE(sessionID_arg)) {
      bisbas_phenotype <- merge(bisbas_data, bisbas_score_dat, by = c(id, session_id))
    } else {
      bisbas_phenotype <- merge(bisbas_data, bisbas_score_dat, by = id)
    }
    
    return(list(score_dat = as.data.frame(bisbas_score_dat),
                bids_phenotype = as.data.frame(bisbas_phenotype)))
  } else {
    return(list(score_dat = as.data.frame(bisbas_score_dat)))
  }
}

