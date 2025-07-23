#' score_bevq: Scored data from the BEVQ-19
#'
#' This function scores the BEVQ-19 and provides subscale scores for the following behaviors: Total habitual intake, water intake, sugar sweetend beverage intake. 
#' 
#' *Note - alcholic drinks removed for child study.
#' 
#' 
#' For data to be scored correctly, the data must be prepared according to the following criteria: \cr
#' \itemize{
#'  \item{The data must include all individual questionnaire items except for question 13:}
#'  \item{The columns/variables must match the following naming convention with each category label as 'bevq_cat_often' or 'bev_cat_amount'. Categories include:}
#'  \itemize{
#'    \item{water: 'water'}
#'    \item{juice: 'juice'}
#'    \item{sweetend juice drink/beverate: 'sweet'}
#'    \item{whole milk: 'wholemilk'}
#'    \item{reduced fat milk: 'reducedmilk'}
#'    \item{low/fat-free milk: 'lowmilk'}
#'    \item{flavored milk: 'flavmilk'}
#'    \item{soft drinks: 'soda'}
#'    \item{diet soft drinks: 'diet'}
#'    \item{sweetend tea: 'sweettea'}
#'    \item{unsweetend tea/coffee (black): 'tea'}
#'    \item{energy drinks/sports drinks: 'sports drinks'}
#'    \item{other: 'other'}
#'    }
#'  \item{All questionnaire responses must be a numeric value ranging from 0-6 (base_zero = TRUE) or 1-7 (base_zero = FALSE) where: }
#'  \itemize{
#'    \item{How often:}
#'    \itemize{
#'      \item{For base_zero = TRUE: 0 = Never or less than 1 time per week; 1 = 1 time per week; 2 = 2-3 times per week; 3 = 4-6 times per week; 4 = 1 time per day; 5 = 2+ times per day; 6 = 3+ times per day}
#'      \item{For base_zero = FALSE: 1 = Never or less than 1 time per week; 2 = 1 time per week; 3 = 2-3 times per week; 4 = 4-6 times per week; 5 = 1 time per day; 6 = 2+ times per day; 7 = 3+ times per day}
#'    }
#'    \item{How much/amount:}
#'    \itemize{
#'      \item{For base_zero = TRUE: 0 = Less than 6 fl oz (3/4 cup); 1 = 8 fl oz (1 cup); 2 = 12 fl oz (1 1/2 cups); 3 = 16 fl oz (2 cups); 4 = More than 20 fl oz (2 1/2 cups)}
#'      \item{For base_zero = FALSE: 1 = Less than 6 fl oz (3/4 cup); 2 = 8 fl oz (1 cup); 3 = 12 fl oz (1 1/2 cups); 4 = 16 fl oz (2 cups); 5 = More than 20 fl oz (2 1/2 cups)}
#'    }
#'   }
#'  \item{Missing values must be coded as NA}
#' }
#' \cr
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Hedrick VE, Savla J, Comber DL, Flack KD, Estabrooks PA, Nsiah-Kumi PA, Ortmeier S, Davy BM. Development of a brief questionnaire to assess habitual beverage intake (BEVQ-15): sugar-sweetened beverages and total beverage energy intake. J Acad Nutr Diet. 2012 Jun;112(6):840-9. doi: 10.1016/j.jand.2012.01.023. PMID: 22709811; PMCID: PMC3379009. (\href{https://pubmed.ncbi.nlm.nih.gov/22709811/}{PubMed})
#' 
#'
#' @param bevq_data a data.frame all items for theBEVQ-19 following the naming conventions described above
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @param extra_scale_cols a vector of character strings that begin with 'bevq' but are not scale items. Any columns in bevq_data that begin with 'bevq' but are not scale items must be included here. Default is empty vector.
#'
#' @return A dataset with subscale scores for theBEVQ-19
#'
#' @examples
#'
#' # scoring for the bevq with IDs, with scale from 0-2, prefer not to answer indicated with 2
#' bevq_score_data <- score_bevq(bevq_data, base_zero = TRUE, id = 'ID', pna_value = 2)
#'
#'
#' \dontrun{
#' }
#'
#'
#' @export

score_bevq <- function(bevq_data, pna_value, base_zero = TRUE, id, session_id, extra_scale_cols = c()) {
  
  #### 1. Set up/initial checks #####
  
  # check that bevq_data exist and is a data.frame
  data_arg <- methods::hasArg(bevq_data)
  
  if (isTRUE(data_arg) & !is.data.frame(bevq_data)) {
    stop('bevq_data must be entered as a data.frame')
  } else if (isFALSE(data_arg)) {
    stop('bevq_data must set to the data.frame with amount consumed for each food item')
  }
  
  # check if id exists
  ID_arg <- methods::hasArg(id)
  
  if (isTRUE(ID_arg)){
    if (!(id %in% names(bevq_data))) {
      stop('variable name entered as id is not in bevq_data')
    }
  }
  
  # check if session_id exists
  sessionID_arg <- methods::hasArg(session_id)
  
  if (isTRUE(sessionID_arg)){
    if (!(id %in% names(bevq_data))) {
      stop("variable name entered as session_id is not in bevq_data")
    }
  }
  
  # check base_zero is logical
  if (!is.logical(base_zero)) {
    stop("base_zero arg must be logical (TRUE/FALSE)")
  }
  
  #### 2. Set Up Data #####
  
  # set up database for results
  
  # create empty matrix
  bevq_score_dat <- data.frame(bevq_water = rep(NA, nrow(bevq_data)), bevq_juice = rep(NA, nrow(bevq_data)), bevq_sweet = rep(NA, nrow(bevq_data)), bevq_wholemilk = rep(NA, nrow(bevq_data)), bevq_reducedmilk = rep(NA, nrow(bevq_data)), bevq_lowmilk = rep(NA, nrow(bevq_data)), bevq_flavmilk = rep(NA, nrow(bevq_data)), bevq_soda = rep(NA, nrow(bevq_data)), bevq_diet = rep(NA, nrow(bevq_data)), bevq_sweettea = rep(NA, nrow(bevq_data)), bevq_tea = rep(NA, nrow(bevq_data)), bevq_sportsbev = rep(NA, nrow(bevq_data)), bevq_ssb = rep(NA, nrow(bevq_data)), bevq_total = rep(NA, nrow(bevq_data)))
  
  if (isTRUE(ID_arg)) {
    if (isTRUE(sessionID_arg)) {
      bevq_score_dat <- data.frame(bevq_data[[id]], bevq_data[[session_id]], bevq_score_dat)
      names(bevq_score_dat)[1:2] <- c(id, session_id)
    } else {
      bevq_score_dat <- data.frame(bevq_data[[id]], bevq_score_dat)
      names(bevq_score_dat)[1] <- id
    }
  }
  
  # assign bevq scale items to bevq_items, excluding columns in extra_scale_cols
  bevq_items <- setdiff(grep("^bevq", names(bevq_data), value = TRUE), extra_scale_cols)
  
  if (isTRUE(sessionID_arg)) {
    bevq_items <- setdiff(grep("^bevq", names(bevq_data), value = TRUE), session_id)
  }
  
  # remove underscore in bevq_items
  bevq_items <- bevq_items[!grepl('other', bevq_items)]
  
  # make copy of data
  bevq_data_edit <- bevq_data
  
  # if pna_value arg, replace not applicable values with NA
  if (isTRUE(methods::hasArg(pna_value))) {
    
    # replace pna_value with NA in pcw_vars
    bevq_data_edit[bevq_items] <- lapply(bevq_data_edit[bevq_items], function(x) ifelse(x == pna_value, NA, x))
    
  }
  
  # check range of data and print warnings (check in bevq_data_edit where not_applicable_value has been replaced with NA)
  min_often <- min(bevq_data_edit[c(bevq_items[grepl('often', bevq_items)])], na.rm = TRUE)
  max_often <- max(bevq_data_edit[c(bevq_items[grepl('often', bevq_items)])], na.rm = TRUE)
  
  if (isTRUE(base_zero)){
    if (min_often < 0 | max_often > 6) {
      warning("range in bevq data (excluding pna_value, if specified) is outside expected range for how OFTEN item is consumed given base_zero = TRUE (expected range: 0-6). Scoring may be incorrect")
    } 
  } else {
    if (min_often < 1 | max_often > 7) {
      warning("range in bevq data (excluding pna_value, if specified) is outside expected range for how OFTEN item is consumed given base_zero = FALSE (expected range: 1-7). Scoring may be incorrect")
    } 
  }
  
  min_amount <- min(bevq_data_edit[c(bevq_items[grepl('amount', bevq_items)])], na.rm = TRUE)
  max_amount <- max(bevq_data_edit[c(bevq_items[grepl('amount', bevq_items)])], na.rm = TRUE)
  
  if (isTRUE(base_zero)){
    if (min_amount < 0 | max_amount > 4) {
      warning("range in bevq data (excluding pna_value, if specified) is outside expected range for AMOUNT of item consumed given base_zero = TRUE (expected range: 0-4). Scoring may be incorrect")
    } 
  } else {
    if (min_amount < 1 | max_amount > 5) {
      warning("range in bevq data (excluding pna_value, if specified) is outside expected range for AMOUNT of item consumed given base_zero = FALSE (expected range: 1-5). Scoring may be incorrect")
    } 
  }
  
  # re-scale data to base 1
  if (isFALSE(base_zero)){
    bevq_data_edit[bevq_items] <- sapply(bevq_items, function(x) bevq_data_edit[[x]] - 1, simplify = TRUE)
  }
  
  # recode
  bevq_data_edit[bevq_items[grepl('often', bevq_items)]] <- sapply(bevq_items[grepl('often', bevq_items)], function(x) ifelse(bevq_data_edit[x] == 0, 0, ifelse(bevq_data_edit[x] == 1, 0.14, ifelse(bevq_data_edit[x] == 2, 0.36, ifelse(bevq_data_edit[x] == 3, 0.71, ifelse(bevq_data_edit[x] == 4, 1, ifelse(bevq_data_edit[x] == 5, 2, ifelse(bevq_data_edit[x] == 6, 3, NA))))))))
  
  bevq_data_edit[bevq_items[grepl('amount', bevq_items)]] <- sapply(bevq_items[grepl('amount', bevq_items)], function(x) ifelse(bevq_data_edit[x] == 0, 6, ifelse(bevq_data_edit[x] == 1, 8, ifelse(bevq_data_edit[x] == 2, 12, ifelse(bevq_data_edit[x] == 3, 16, ifelse(bevq_data_edit[x] == 4, 20, NA))))))
  
  # calculate reversed daily values
  bevq_score_dat['bevq_water'] <- ifelse(bevq_data_edit[['bevq_water_often']] == 0, 0, bevq_data_edit[['bevq_water_often']]*bevq_data_edit[['bevq_water_amount']])
  
  bevq_score_dat['bevq_juice'] <- ifelse(bevq_data_edit[['bevq_juice_often']] == 0, 0, bevq_data_edit[['bevq_juice_often']]*bevq_data_edit[['bevq_juice_amount']])
  
  bevq_score_dat['bevq_sweet'] <- ifelse(bevq_data_edit[['bevq_sweet_often']] == 0, 0, bevq_data_edit[['bevq_sweet_often']]*bevq_data_edit[['bevq_sweet_amount']])
  
  bevq_score_dat['bevq_wholemilk'] <- ifelse(bevq_data_edit[['bevq_wholemilk_often']] == 0, 0, bevq_data_edit[['bevq_wholemilk_often']]*bevq_data_edit[['bevq_wholemilk_amount']])
  
  bevq_score_dat['bevq_reducedmilk'] <- ifelse(bevq_data_edit[['bevq_reducedmilk_often']] == 0, 0, bevq_data_edit[['bevq_reducedmilk_often']]*bevq_data_edit[['bevq_reducedmilk_amount']])
  
  bevq_score_dat['bevq_lowmilk'] <- ifelse(bevq_data_edit[['bevq_lowmilk_often']] == 0, 0, bevq_data_edit[['bevq_lowmilk_often']]*bevq_data_edit[['bevq_lowmilk_amount']])
  
  bevq_score_dat['bevq_flavmilk'] <- ifelse(bevq_data_edit[['bevq_flavmilk_often']] == 0, 0, bevq_data_edit[['bevq_flavmilk_often']]*bevq_data_edit[['bevq_flavmilk_amount']])
  
  bevq_score_dat['bevq_soda'] <- ifelse(bevq_data_edit[['bevq_soda_often']] == 0, 0, bevq_data_edit[['bevq_soda_often']]*bevq_data_edit[['bevq_soda_amount']])
  
  bevq_score_dat['bevq_diet'] <- ifelse(bevq_data_edit[['bevq_diet_often']] == 0, 0, bevq_data_edit[['bevq_diet_often']]*bevq_data_edit[['bevq_diet_amount']])
  
  bevq_score_dat['bevq_sweettea'] <- ifelse(bevq_data_edit[['bevq_sweettea_often']] == 0, 0, bevq_data_edit[['bevq_sweettea_often']]*bevq_data_edit[['bevq_sweettea_amount']])
  
  bevq_score_dat['bevq_tea'] <- ifelse(bevq_data_edit[['bevq_tea_often']] == 0, 0, bevq_data_edit[['bevq_tea_often']]*bevq_data_edit[['bevq_tea_amount']])
  
  bevq_score_dat['bevq_sportsbev'] <- ifelse(bevq_data_edit[['bevq_sportsbev_often']] == 0, 0, bevq_data_edit[['bevq_sportsbev_often']]*bevq_data_edit[['bevq_sportsbev_amount']])
  
  ## Score Subscales
  bevq_score_dat['bevq_ssb'] <- rowSums(bevq_score_dat[c('bevq_soda', 'bevq_sweet', 'bevq_sweettea', 'bevq_sportsbev', 'bevq_flavmilk')])
  
  bevq_score_dat['bevq_total'] <- rowSums(bevq_score_dat[!grepl('ssb|total', names(bevq_score_dat))], na.rm = TRUE)
  
  #### 3. Clean Export/Scored Data #####
  ## round data
  if (isTRUE(ID_arg)){
    bevq_score_dat[2:ncol(bevq_score_dat)] <- round(bevq_score_dat[2:ncol(bevq_score_dat)], digits = 3)
  } else {
    bevq_score_dat <- round(bevq_score_dat, digits = 3)
  }
  
  ## merge raw responses with scored data
  if (isTRUE(ID_arg)){
    if (isTRUE(sessionID_arg)) {
      bevq_phenotype <- merge(bevq_data, bevq_score_dat, by = c(id, session_id))
    } else {
      bevq_phenotype <- merge(bevq_data, bevq_score_dat, by = id)
    }
    
    return(list(score_dat = as.data.frame(bevq_score_dat),
                bids_phenotype = as.data.frame(bevq_phenotype)))
  } else {
    return(list(score_dat = as.data.frame(bevq_score_dat)))
  }
  
}

