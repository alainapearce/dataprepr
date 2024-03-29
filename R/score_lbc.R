#' score_lbc: Scored data from the Lifestyle Behavior Checklist
#'
#' This function scores the Lifestyle Behavior Checklist and provides subscale scores for the following behaviors: Food-Related Misbehavior, Overeating, Emotions Related to Overweight, and Physical Activity.
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'lbc#' or 'lbc_#' where # is the question number (1-25)
#' 3) All questions must have the numeric value for the choice with the base value being either 0 (score_base = TRUE) or 1 (score_base = FALSE)
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' West F, Sanders MR. The Lifestyle Behaviour Checklist: A measure of weight-related problem behaviour in obese children. International Journal of Pediatric Obesity. 2009;4(4):266-273. doi:10.3109/17477160902811199
#'
#' Subscales:
#' West F, Morawska A, Joughin K. The Lifestyle Behaviour Checklist: evaluation of the factor structure. Child: Care, Health and Development. 2010;36(4):508-515. doi:10.1111/j.1365-2214.2010.01074.x (\href{https://pubmed.ncbi.nlm.nih.gov/20337641/}{PubMed})
#'
#' @param lbc_data a data.frame all items for the Lifestyle Behavior Checklist following the naming conventions described above
#' @inheritParams score_bes
#' @inheritParams score_bes
#'
#' @return A dataset with subscale scores for the Lifestyle Behavior Checklist
#' @examples
#'
#' # scoring for the lbc with IDs
#' lbc_score_data <- score_lbc(lbc_data, id = 'ID')
#'
#'
#' @export

score_lbc <- function(lbc_data, score_base = TRUE, id) {

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

    #### 2. Set Up Data #####

    # set up database for results create empty matrix
    lbc_score_dat <- data.frame(lbc_misbeh = rep(NA, nrow(lbc_data)), lbc_overeat = rep(NA, nrow(lbc_data)), lbc_em_overweight = rep(NA, nrow(lbc_data)), lbc_pa = rep(NA, nrow(lbc_data)), lbc_total = rep(NA, nrow(lbc_data)))

    if (isTRUE(ID_arg)) {
        lbc_score_dat <- data.frame(lbc_data[[id]], lbc_score_dat)
        names(lbc_score_dat)[1] <- id
    }

    # remove underscore if in column names
    names(lbc_data) <- gsub('lbc_', 'lbc', names(lbc_data))
    
    # re-scale data
    lbc_data_edit <- lbc_data
    
    if (isTRUE(score_base)){
      lbc_data_edit[2:26] <- sapply(names(lbc_data_edit)[2:26], function(x) lbc_data_edit[[x]] + 1, simplify = TRUE)
    }
    
    ## Score Subscales

    # Food-Related Misbehavior
    misbeh_vars <- c('lbc3', 'lbc4', 'lbc5', 'lbc6', 'lbc8', 'lbc10', 'lbc11')
    lbc_score_dat[['lbc_misbeh']] <- rowSums(lbc_data_edit[misbeh_vars])

    # Overeating
    overeat_vars <- c('lbc1', 'lbc2', 'lbc9', 'lbc12', 'lbc13', 'lbc14', 'lbc15')
    lbc_score_dat[['lbc_overeat']] <- rowSums(lbc_data_edit[overeat_vars])

    # Emotion Related to Being Overweight
    emOW_vars <- c('lbc20', 'lbc21', 'lbc22', 'lbc23', 'lbc24')
    lbc_score_dat[['lbc_em_overweight']] <- rowSums(lbc_data_edit[emOW_vars])

    # Physical Activity
    pa_vars <- c('lbc7', 'lbc16', 'lbc17', 'lbc18', 'lbc19')
    lbc_score_dat[['lbc_pa']] <- rowSums(lbc_data_edit[pa_vars])

    ## Total
    pa_vars <- c('lbc7', 'lbc16', 'lbc17', 'lbc18', 'lbc19')
    lbc_score_dat[['lbc_total']] <- rowSums(lbc_data_edit[c(misbeh_vars, overeat_vars,
        emOW_vars, pa_vars)])

    #### 3. Clean Export/Scored Data #####
    ## round data
    if (isTRUE(ID_arg)){
        lbc_score_dat[2:ncol(lbc_score_dat)] <- round(lbc_score_dat[2:ncol(lbc_score_dat)], digits = 3)
    } else {
        lbc_score_dat <- round(lbc_score_dat, digits = 3)
    }

    ## merge raw responses with scored data
    if (isTRUE(ID_arg)){
      lbc_phenotype <- merge(lbc_data, lbc_score_dat, by = id)
      
      return(list(score_dat = as.data.frame(lbc_score_dat),
                  bids_phenotype = as.data.frame(lbc_phenotype)))
    } else {
      return(list(score_dat = as.data.frame(lbc_score_dat)))
    }

}

