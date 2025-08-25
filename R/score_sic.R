#' score_sic: INCOMPLETE FUNCTION Scored data from the Stress in Children Questionnaire
#'
#' This function scores the Stress in Children Questionnaire and provides subscale scores for the following behaviors: Well Being, Distress, Social Support, and the Global Mean Score.
#'
#'#' For data to be scored correctly, the data must be prepared according to the following criteria: \cr
#' \itemize{
#'  \item{The data must include all individual questionnaire items}
#'  \item{The  columns/variables must match the following naming convention: 'sic#' or 'sic_#' where # is the question number (1-21)}
#'  \item{All questionnaire responses must be a numeric value ranging from 0-3 (base_zero = TRUE) or 1-4 (base_zero = FALSE) where: }
#'  \itemize{
#'     \item{For base_zero = TRUE: 0 = Never; 1 = Sometimes; 2 = Often; 3 = Very Often}
#'     \item{For base_zero = FALSE: 1 = Never; 2 = Sometimes; 3 = Often; 4 = Very Often}
#'   }
#'  \item{Missing values must be coded as NA}
#' }
#' \cr
#' Note, as long as variable names match those listed, the dataset can include other variables. Up to 2 missing responses are allowed
#'
#' @references
#' 1. Osika W, Friberg P, Wahrborg P. A new short self-rating questionnaire to assess stress in children. Int J Behav Med. 2007;14(2):108-117. doi:10.1007/BF03004176 (\href{https://pubmed.ncbi.nlm.nih.gov/17926439/}{PubMed})
#'
#' @param sic_data a data.frame all items for the Child Behavior Questionnaire following the naming conventions described above
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @param extra_scale_cols a vector of character strings that begin with 'sic' but are not scale items. Any columns in sic_data that begin with 'sic' but are not scale items must be included here. Default is empty vector.
#' @return A dataset with subscale scores for the Stress in Children Questionnaire
#' @examples
#'
#' # scoring for the sic with IDs
#' sic_score_data <- score_sic(sic_data, id = 'ID')
#'
#' \dontrun{
#' }
#'
#'
#' @export

score_sic <- function(sic_data, pna_value, base_zero = TRUE, id, session_id, extra_scale_cols = c()) {

    #### 1. Set up/initial checks #####

    # check that sic_data exist and is a data.frame
    data_arg <- methods::hasArg(sic_data)

    if (isTRUE(data_arg) & !is.data.frame(sic_data)) {
        stop('sic_data must be entered as a data.frame')
    } else if (isFALSE(data_arg)) {
        stop('sic_data must set to the data.frame with amount consumed for each food item')
    }

    # check if id exists
    ID_arg <- methods::hasArg(id)

    if (isTRUE(ID_arg)){
        if (!(id %in% names(sic_data))) {
            stop('variable name entered as id is not in sic_data')
        }
    }

    # check if session_id exists
    sessionID_arg <- methods::hasArg(session_id)
    
    if (isTRUE(sessionID_arg)){
      if (!(id %in% names(sic_data))) {
        stop('variable name entered as session_id is not in sic_data')
      }
    }
    
    # check base_zero is logical
    if (!is.logical(base_zero)) {
      stop('base_zero arg must be logical (TRUE/FALSE)')
    }
    
    #### 2. Set Up Data #####

    # set up database for results

    ## create empty matrix
    sic_score_dat <- data.frame(sic_lackwellbeing = rep(NA, nrow(sic_data)), sic_distress = rep(NA, nrow(sic_data)), sic_lacksocialsupport = rep(NA, nrow(sic_data)), sic_grand_mean = rep(NA, nrow(sic_data)))

    if (isTRUE(ID_arg)) {
      if (isTRUE(sessionID_arg)) {
        sic_score_dat <- data.frame(sic_data[[id]], sic_data[[session_id]], sic_score_dat)
        names(sic_score_dat)[1:2] <- c(id, session_id)
      } else {
        sic_score_dat <- data.frame(sic_data[[id]], sic_score_dat)
        names(sic_score_dat)[1] <- id
      }
    }

    # assign sic scale items to sic_items, excluding columns in extra_scale_cols
    sic_items <- setdiff(grep('^sic', names(sic_data), value = TRUE), extra_scale_cols)
    
    # remove underscore in column names for sic_items
    names(sic_data)[names(sic_data) %in% sic_items] <- gsub('sic_', 'sic', names(sic_data)[names(sic_data) %in% sic_items])
    
    # remove underscore in sic_items
    sic_items <- gsub('sic_', 'sic', sic_items)
    
    # if pna_value arg, replace not applicable values with NA
    if (isTRUE(methods::hasArg(pna_value))) {
      
      # replace pna_value with NA in pcw_vars
      sic_data[sic_items] <- lapply(sic_data[sic_items] , function(x) ifelse(x == pna_value, NA, x))
      
    }
    
    # check range of data and print warnings
    min <- min(sic_data[c(sic_items)], na.rm = TRUE)
    max <- max(sic_data[c(sic_items)], na.rm = TRUE)
    
    if (isTRUE(base_zero)){
      if (min < 0 | max > 3) {
        warning('range in SIC data is outside expected range given base_zero = TRUE (expected range: 0-3). Scoring may be incorrect')
      } 
    } else {
      if (min < 1 | max > 4) {
        warning('range in SIC data is outside expected range given base_zero = FALSE (expected range: 1-4). Scoring may be incorrect')
      } 
    }
    
    # re-scale data to use base 1
    sic_data_edit <- sic_data

    if (isTRUE(base_zero)){
      sic_data_edit[sic_items] <- sapply(sic_items, function(x) sic_data[[x]] + 1, simplify = TRUE)
    }

    # calculate reversed scores - just guessing
    reverse_qs <- c('sic3', 'sic4', 'sic8', 'sic10', 'sic11', 'sic12', 'sic13', 'sic14', 'sic16', 'sic18', 'sic19', 'sic20', 'sic21')
    
    for (var in 1:length(reverse_qs)) {
      var_name <- reverse_qs[var]
      reverse_name <- paste0(var_name, '_rev')
      
      sic_data_edit[[reverse_name]] <- ifelse(sic_data_edit[[var_name]] == 1, 4, ifelse(sic_data_edit[[var_name]] == 2, 3, ifelse(sic_data_edit[[var_name]] == 3, 2, ifelse(sic_data_edit[[var_name]] == 4, 1, NA))))
      
    }
    
    ## Score Subscales

    # Lack of Well Being
    lackwellbeing_vars <- c('sic3_rev', 'sic4_rev', 'sic8_rev', 'sic10_rev', 'sic11_rev', 'sic13_rev', 'sic14_rev')
    sic_score_dat[['sic_lackwellbeing']] <- rowMeans(sic_data_edit[lackwellbeing_vars], na.rm = TRUE)

    # Distress
    distress_vars <- c('sic1', 'sic2', 'sic5', 'sic6', 'sic7', 'sic15', 'sic18', 'sic21_rev')
    sic_score_dat[['sic_distress']] <- rowMeans(sic_data_edit[distress_vars], na.rm = TRUE)

    # Lack of Social Support
    lacksocialsupport_vars <- c('sic9', 'sic12_rev', 'sic16_rev', 'sic17', 'sic19_rev', 'sic20_rev')
    sic_score_dat[['sic_lacksocialsupport']] <- rowMeans(sic_data_edit[lacksocialsupport_vars], na.rm = TRUE)

    # Grand Mean
    grandmean_vars <- c(sic_items[!(sic_items %in% reverse_qs)], paste0(reverse_qs, '_rev'))
    sic_score_dat[['sic_grand_mean']] <- rowMeans(sic_data_edit[grandmean_vars], na.rm = TRUE)

    #### 3. Clean Export/Scored Data #####
    ## round data
    if (isTRUE(ID_arg)){
        sic_score_dat[2:ncol(sic_score_dat)] <- round(sic_score_dat[2:ncol(sic_score_dat)], digits = 3)
    } else {
        sic_score_dat <- round(sic_score_dat, digits = 3)
    }
    
    ## merge raw responses with scored data
    if (isTRUE(ID_arg)){
      if (isTRUE(sessionID_arg)) {
        sic_phenotype <- merge(sic_data, sic_score_dat, by = c(id, session_id))
      } else {
        sic_phenotype <- merge(sic_data, sic_score_dat, by = id)
      }
      
      return(list(score_dat = as.data.frame(sic_score_dat),
                  bids_phenotype = as.data.frame(sic_phenotype)))
    } else {
      return(list(score_dat = as.data.frame(sic_score_dat)))
    }

    return(sic_score_dat)
}

