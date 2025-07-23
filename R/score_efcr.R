#' score_efcr: Scored data from the External Food Cue Responsiveness Scale
#'
#' This function scores the External Food Cue Responsiveness Scale
#' 
#' For data to be scored correctly, the data must be prepared according to the following criteria: \cr
#' \itemize{
#'  \item{The data must include all individual questionnaire items}
#'  \item{The columns/variables must match the following naming convention: 'efcr#' or 'efcr_#' where # is the question number (1-9)}
#'  \item{All questionnaire responses must be a numeric value ranging from 0-4 (base_zero = TRUE) or 1-5 (base_zero = FALSE) where: }
#'  \itemize{
#'     \item{For base_zero = TRUE: 0 = Never; 1 = Rarely; 2 = Sometimes; 3 = Often; 4 = Always}
#'     \item{For base_zero = FALSE: 1 = Never; 2 = Rarely; 3 = Sometimes; 4 = Often; 5 = Always}
#'   }
#'  \item{Missing values must be coded as NA}
#' }
#' \cr
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Primary References for the External Food Cue Responsiveness Scale and Scoring:
#' Masterson TD, Gilbert-Diamond D, Lansigan RK, Kim SJ, Schiffelbein JE, Emond JA. Measurement of external food cue responsiveness in preschool-age children: Preliminary evidence for the use of the external food cue responsiveness scale. Appetite. 2019;139:119-126. doi:10.1016/j.appet.2019.04.024 (\href{https://pubmed.ncbi.nlm.nih.gov/31047939/}{PubMed})
#'
#' Pollack CC, Emond JA, Masterson TD. Associations between adolescent and young adult External Food Cue Responsiveness (EFCR) and brand recall, product craving and product purchasing in the livestreaming food marketing environment. Public Health Nutr. 2022;25(11):3036-3043. doi:10.1017/S1368980022001628 (\href{https://pubmed.ncbi.nlm.nih.gov/35920082/}{PubMed})
#'
#' @param efcr_data a data.frame all items for the External Food Cue Responsiveness Scale following the naming conventions described above
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @param extra_scale_cols a vector of character strings that begin with 'efcr' but are not scale items. Any columns in efcr_data that begin with 'efcr' but are not scale items must be included here. Default is empty vector.
#' @return If 'id' argument is used, returns a list with 2 dataframes: (1) bids_phenotype (contains input efcr_data [values identical to input, underscores removed from efcr items col names, if they existed] and External Food Cue Responsiveness Scale score) and (2) score_dat (contains efcr subscale scores only). If 'id' argument is not used, returns a list with score_dat dataframe only.
#' @examples
#'
#' # scoring for the EFCR Scale with IDs
#' efcr_score_data <- score_efcr(efcr_data, base_zero = TRUE, id = 'ID')
#' 
#' 
#' @export

score_efcr <- function(efcr_data, pna_value, base_zero = TRUE, id, session_id, extra_scale_cols = c()) {

    #### 1. Set up/initial checks #####

    # check that efcr_data exist and is a data.frame
    data_arg <- methods::hasArg(efcr_data)

    if (isTRUE(data_arg) & !is.data.frame(efcr_data)) {
        stop('efcr_data must be entered as a data.frame')
    } else if (isFALSE(data_arg)) {
        stop('efcr_data must set to the data.frame with amount consumed for each food item')
    }

    # check if id exists
    ID_arg <- methods::hasArg(id)

    if (isTRUE(ID_arg)){
        if (!(id %in% names(efcr_data))) {
            stop('variable name entered as id is not in efcr_data')
        }
    }
    
    # check if session_id exists
    sessionID_arg <- methods::hasArg(session_id)
    
    if (isTRUE(sessionID_arg)){
      if (!(id %in% names(efcr_data))) {
        stop("variable name entered as session_id is not in efcr_data")
      }
    }

    # check base_zero is logical
    if (!is.logical(base_zero)) {
      stop("base_zero arg must be logical (TRUE/FALSE)")
    }
    
    #### 2. Set Up Data #####

    # set up database for results create empty matrix
    efcr_score_dat <- data.frame(efcr_score = rep(NA, nrow(efcr_data)))

    if (isTRUE(ID_arg)) {
      if (isTRUE(sessionID_arg)) {
        efcr_score_dat <- data.frame(efcr_data[[id]], efcr_data[[session_id]], efcr_score_dat)
        names(efcr_score_dat)[1:2] <- c(id, session_id)
      } else {
        efcr_score_dat <- data.frame(efcr_data[[id]], efcr_score_dat)
        names(efcr_score_dat)[1] <- id
      }
    }

    # assign efcr scale items to efcr_items, excluding columns in extra_scale_cols
    efcr_items <- setdiff(grep("^efcr", names(efcr_data), value = TRUE), extra_scale_cols)
    
    # remove underscore in column names for efcr_items
    names(efcr_data)[names(efcr_data) %in% efcr_items] <- gsub('efcr_', 'efcr', names(efcr_data)[names(efcr_data) %in% efcr_items])
    
    # remove underscore in efcr_items
    efcr_items <- gsub("efcr_", "efcr", efcr_items)
    
    # if pna_value arg, replace not applicable values with NA
    if (isTRUE(methods::hasArg(pna_value))) {
      
      # replace pna_value with NA in pcw_vars
      efcr_data[efcr_items] <- lapply(efcr_data[efcr_items] , function(x) ifelse(x == pna_value, NA, x))
      
    }
    
    # check range of data and print warnings
    min <- min(efcr_data[c(efcr_items)], na.rm = TRUE)
    max <- max(efcr_data[c(efcr_items)], na.rm = TRUE)
    
    if (isTRUE(base_zero)){
      if (min < 0 | max > 4) {
        warning("range in ECFR data is outside expected range given base_zero = TRUE (expected range: 0-4). Scoring may be incorrect")
      } 
    } else {
      if (min < 1 | max > 5) {
        warning("range in ECFR data is outside expected range given base_zero = FALSE (expected range: 1-5). Scoring may be incorrect")
      } 
    }
    
    # re-scale data
    efcr_data_edit <- efcr_data
    
    if (isTRUE(base_zero)){
      efcr_data_edit[efcr_items] <- sapply(efcr_items, function(x) efcr_data_edit[[x]] + 1, simplify = TRUE)
    }
    
    ## Score
    efcr_score_dat[['efcr_score']] <- rowMeans(efcr_data_edit[efcr_items])
 
    #### 3. Clean Export/Scored Data #####
    ## round data
    if (isTRUE(ID_arg)){
        efcr_score_dat[2:ncol(efcr_score_dat)] <- round(efcr_score_dat[2:ncol(efcr_score_dat)], digits = 3)
    } else {
        efcr_score_dat <- round(efcr_score_dat, digits = 3)
    }

    ## merge raw responses with scored data
    if (isTRUE(ID_arg)){
      if (isTRUE(sessionID_arg)) {
        efcr_phenotype <- merge(efcr_data, efcr_score_dat, by = c(id, session_id))
      } else {
        efcr_phenotype <- merge(efcr_data, efcr_score_dat, by = id)
      }
      
      return(list(score_dat = as.data.frame(efcr_score_dat),
                  bids_phenotype = as.data.frame(efcr_phenotype)))
    } else {
      return(list(score_dat = as.data.frame(efcr_score_dat)))
    }
    
}

