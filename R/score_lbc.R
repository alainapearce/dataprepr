#' score_lbc: Scored data from the Lifestyle Behavior Checklist
#'
#' This function scores the 25-item or revised 24-item Lifestyle Behavior Checklist and provides scores for: Total Problem, Total Confidence and 4-factor subscales: Food-Related Misbehavior, Overeating, Emotions Related to Overweight, and Physical Activity
#'
#' For data to be scored correctly, the data must be prepared according to the following criteria: \cr
#' \itemize{
#'  \item{The data must include all individual Problem Scale items (i.e., "To what extent has this behavior been a problem for your child?") and Confidence Scale items (i.e., "How confident are you in dealing with it?").}
#'  \item{The columns/variables must match the following naming conventions: }
#'  \itemize{
#'     \item{For Problem Scale items: 'lbc#' or 'lbc_#' where # is the question number (1-25 or 1-24)}
#'     \item{For Confidence Scale items: 'lbc#_conf' or 'lbc_#_conf' where # is the question number (1-25 or 1-24)}
#'     \item{Note: The LBC version (25-item or revised 24-item) will be infered based on the number of columns beginning with 'lbc' (excluding extra_scale_cols) }
#'   }
#'  \item{Problem Scale responses must be a numeric value ranging from 0-6 (base_zero = TRUE) or 1-7 (base_zero = FALSE) where: }
#'  \itemize{
#'     \item{For base_zero = TRUE: 0 = Not at all; 3 = Somewhat; 6 = Very much}
#'     \item{For base_zero = FALSE: 1 = Not at all; 4 = Somewhat; 7 = Very much}
#'   }
#'  \item{Confidence Scale responses must be a numeric value ranging from 0-9 (base_zero = TRUE) or 1-10 (base_zero = FALSE) where: }
#'  \itemize{
#'     \item{For base_zero = TRUE: 0 = Certain I connot do it; 9 = Certain I can do it}
#'     \item{For base_zero = FALSE: 1 = Certain I connot do it; 10 = Certain I can do it}
#'   }
#'  \item{Missing values must be coded as NA}
#'  \item{Any columns in lbc_data that begin with 'lbc' but are not scale items must listed in extra_scale_cols}
#' }
#' \cr
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' West F, Sanders MR. The Lifestyle Behaviour Checklist: A measure of weight-related problem behaviour in obese children. International Journal of Pediatric Obesity. 2009;4(4):266-273. doi:10.3109/17477160902811199
#'
#' 4-factor Subscales (based on the revised 24-item LBC):
#' West F, Morawska A, Joughin K. The Lifestyle Behaviour Checklist: evaluation of the factor structure. Child: Care, Health and Development. 2010;36(4):508-515. doi:10.1111/j.1365-2214.2010.01074.x (\href{https://pubmed.ncbi.nlm.nih.gov/20337641/}{PubMed})
#'
#' @param lbc_data a data.frame all items for the Lifestyle Behavior Checklist following the naming conventions described above
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @param extra_scale_cols a vector of character strings that begin with 'lbc' but are not scale items. Any columns in lbc_data that begin with 'lbc' but are not scale items must be included here. Default is empty vector.
#' @return A dataset with subscale scores for the Lifestyle Behavior Checklist
#' @examples
#'
#' # scoring for the lbc with IDs
#' lbc_score_data <- score_lbc(lbc_data, id = 'ID')
#'
#'
#' @export

score_lbc <- function(lbc_data, base_zero = TRUE, id, session_id, extra_scale_cols = c()) {

    #### 1. Set up/initial checks #####

    # check that lbc_data exist and is a data.frame
    data_arg <- methods::hasArg(lbc_data)

    if (isTRUE(data_arg) & !is.data.frame(lbc_data)) {
        stop('lbc_data must be entered as a data.frame')
    } else if (isFALSE(data_arg)) {
        stop('lbc_data must set to the data.frame with amount consumed for each food item')
    }

    # check if id exists
    ID_arg <- methods::hasArg(id)

    if (isTRUE(ID_arg)){
        if (!(id %in% names(lbc_data))) {
            stop('variable name entered as id is not in lbc_data')
        }
    }
    
    # check if session_id exists
    sessionID_arg <- methods::hasArg(session_id)
    
    if (isTRUE(sessionID_arg)){
      if (!(id %in% names(lbc_data))) {
        stop("variable name entered as session_id is not in lbc_data")
      }
    }

    # check base_zero is logical
    if (!is.logical(base_zero)) {
      stop("base_zero arg must be logical (TRUE/FALSE)")
    }
    
    #### 2. Set Up Data #####

    # set up database for results create empty matrix
    lbc_score_dat <- data.frame(lbc_misbeh = rep(NA, nrow(lbc_data)), lbc_overeat = rep(NA, nrow(lbc_data)), lbc_em_overweight = rep(NA, nrow(lbc_data)), lbc_pa = rep(NA, nrow(lbc_data)), lbc_problem_total = rep(NA, nrow(lbc_data)), lbc_conf_total = rep(NA, nrow(lbc_data)))

    if (isTRUE(ID_arg)) {
      if (isTRUE(sessionID_arg)) {
        lbc_score_dat <- data.frame(lbc_data[[id]], lbc_data[[session_id]], lbc_score_dat)
        names(lbc_score_dat)[1:2] <- c(id, session_id)
      } else {
        lbc_score_dat <- data.frame(lbc_data[[id]], lbc_score_dat)
        names(lbc_score_dat)[1] <- id
      }
    }

    # assign lbc scale items to lbc_items, excluding columns in extra_scale_cols
    lbc_items <- setdiff(grep("^lbc", names(lbc_data), value = TRUE), extra_scale_cols)
    
    # count number of LBC items
    n_items <- length(lbc_items)/2
    
    if (n_items < 24 | n_items > 25){
      stop(paste("lbc_data has", n_items, "items. score_lbc() will only score data with 24 or 25 items"))
    }
    
    # remove underscore in column names for lbc_items
    names(lbc_data)[names(lbc_data) %in% lbc_items] <- gsub('lbc_', 'lbc', names(lbc_data)[names(lbc_data) %in% lbc_items])
    
    # remove underscore in lbc_items
    lbc_items <- gsub("lbc_", "lbc", lbc_items)
    
    # check range of data and print warnings
    
    conf_items <- grep("conf", lbc_items,value = TRUE)
    prob_items <- grep("conf", lbc_items,value = TRUE, invert = TRUE)
    
    min_prob <- min(lbc_data[c(prob_items)], na.rm = TRUE)
    max_prob <- max(lbc_data[c(prob_items)], na.rm = TRUE)
    
    min_conf <- min(lbc_data[c(conf_items)], na.rm = TRUE)
    max_conf <- max(lbc_data[c(conf_items)], na.rm = TRUE)
    
    if (isTRUE(base_zero)){
      if (min_prob < 0 | max_prob > 6) {
        warning("range in LBC Problem scale values is outside expected range given base_zero = TRUE (expected range: 0-6). Scoring may be incorrect")
      } 
      if (min_conf < 0 | max_conf > 9) {
        warning("range in LBC Confidence scale values is outside expected range given base_zero = TRUE (expected range: 0-9). Scoring may be incorrect")
      } 
    } else {
      if (min_prob < 1 | min_prob > 7) {
        warning("range in LBC Problem Scale values is outside expected range given base_zero = FALSE (expected range: 1-7). Scoring may be incorrect")
      } 
      if (min_conf < 1 | max_conf > 10) {
        warning("range in LBC Confidence scale values is outside expected range given base_zero = TRUE (expected range: 1-10). Scoring may be incorrect")
      } 
    }
    
    # re-scale data
    lbc_data_edit <- lbc_data
    
    if (isTRUE(base_zero)){
      lbc_data_edit[lbc_items] <- sapply(lbc_items, function(x) lbc_data_edit[[x]] + 1, simplify = TRUE)
    }
    
    ## Score Subscales

    # Food-Related Misbehavior
    misbeh_vars <- c('lbc3', 'lbc4', 'lbc5', 'lbc6', 'lbc8', 'lbc10', 'lbc11') # items are the same for 24 and 25-item scales
    lbc_score_dat[['lbc_misbeh']] <- rowSums(lbc_data_edit[misbeh_vars])

    # Overeating 
    overeat_vars <- c('lbc1', 'lbc2', 'lbc9', 'lbc12', 'lbc13', 'lbc14', 'lbc15')  # items are the same for 24 and 25-item scales
    lbc_score_dat[['lbc_overeat']] <- rowSums(lbc_data_edit[overeat_vars])

    # Emotion Related to Being Overweight
    if (n_items == 24) {
      emOW_vars <- c('lbc20', 'lbc21', 'lbc22', 'lbc23', 'lbc24')
      lbc_score_dat[['lbc_em_overweight']] <- rowSums(lbc_data_edit[emOW_vars])
    } else if (n_items == 25) {
      emOW_vars <- c('lbc21', 'lbc22', 'lbc23', 'lbc24', 'lbc25')
      lbc_score_dat[['lbc_em_overweight']] <- rowSums(lbc_data_edit[emOW_vars])
    }

    # Physical Activity
    pa_vars <- c('lbc7', 'lbc16', 'lbc17', 'lbc18', 'lbc19') # items are the same for 24 and 25-item scales
    lbc_score_dat[['lbc_pa']] <- rowSums(lbc_data_edit[pa_vars])

    ## Total
    prob_vars <- paste0("lbc", seq(1, n_items))
    conf_vars <- paste0("lbc", seq(1, n_items), "_conf")
    
    lbc_score_dat[['lbc_problem_total']] <- rowSums(lbc_data_edit[c(prob_vars)])
    lbc_score_dat[['lbc_conf_total']] <- rowSums(lbc_data_edit[c(conf_vars)])

    #### 3. Clean Export/Scored Data #####
    ## round data
    if (isTRUE(ID_arg)){
        lbc_score_dat[2:ncol(lbc_score_dat)] <- round(lbc_score_dat[2:ncol(lbc_score_dat)], digits = 3)
    } else {
        lbc_score_dat <- round(lbc_score_dat, digits = 3)
    }

    ## merge raw responses with scored data
    if (isTRUE(ID_arg)){
      if (isTRUE(sessionID_arg)) {
        lbc_phenotype <- merge(lbc_data, lbc_score_dat, by = c(id, session_id))
      } else {
        lbc_phenotype <- merge(lbc_data, lbc_score_dat, by = id)
      }
      
      return(list(score_dat = as.data.frame(lbc_score_dat),
                  bids_phenotype = as.data.frame(lbc_phenotype)))
    } else {
      return(list(score_dat = as.data.frame(lbc_score_dat)))
    }

}

