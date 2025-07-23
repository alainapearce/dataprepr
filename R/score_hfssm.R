#' score_hfssm: Scored data from the U.S. Household Food Security Survey Module
#'
#' This function scores the U.S. Household Food Security Survey Module and provides scores for: U.S. Household Food Security Scale, U.S. Adult Food Security Scale, 6-item Food Security Scale, U.S. Children’s Food Security Scale
#'
#' To use this function, the data must be prepared according to the following criteria: \cr
#' \cr
#' 1) The data must include all individual questionnaire items, excluding hh1, which is not used for scoring food security scales
#' \cr
#' 2) The  columns/variables must match the following naming conventions: 'hfssm_hh#', 'hfssm_ad#', or 'hfssm_ch#', where # is the question number and hh/ad/ch refers to the questionnaire stage (household, adult, child)
#' \cr
#' 3a) If base_zero = FALSE (default), questions must have the numeric value for the choices: \cr
#' \itemize{
##'  \item{Questions [hh2, hh3, hh4, ch1, ch2, ch3]: 1 - Often True, 2 - Sometimes True, 3 - Never True, 4 - DK or Refused}
##'  \item{Questions [ad1, ad2, ad3, ad4, ad5, ch4, ch5, ch6, ch7]: 1 - yes, 2 - no, 3 - DK}
##'  \item{Questions [ad1a, ad5a, ch5a]: 1 - almost every month, 2 - Some months but not every month, 3 - Only 1 or 2 months, 4 - DK}
##' }
#' 3b) If base_zero = TRUE, questions must have the numeric value for the choices: \cr
#' \itemize{
##'  \item{Questions [hh2, hh3, hh4, ch1, ch2, ch3]: 0 - Often True, 1 - Sometimes True, 2 - Never True, 3 - DK or Refused}
##'  \item{Questions [ad1, ad2, ad3, ad4, ad5, ch4, ch5, ch6, ch7]: 0 - yes, 1 - no, 2 - DK}
##'  \item{Questions [ad1a, ad5a, ch5a]: 0 - almost every month, 1 - Some months but not every month, 2 - Only 1 or 2 months, 3 - DK}
##' }
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' U.S. HOUSEHOLD FOOD SECURITY SURVEY MODULE: THREE-STAGE DESIGN, WITH SCREENERS Economic Research Service, USDA September 2012 https://www.ers.usda.gov/media/8271/hh2012.pdf
#' 
#' @param hfssm_data a data.frame all items for the U.S. Household Food Security Survey Module following the naming conventions described above
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' #' @param extra_scale_cols a vector of character strings that begin with 'hfssm' but are not scale items. Any columns in scpf_data that begin with 'hfssm' but are not scale items must be included here. Default is empty vector.
#'
#' @return A dataset with subscale scores for the U.S. Household Food Security Survey Module
#' @examples
#'
#' # scoring for the hfssm with IDs
#' hfssm_score_data <- score_hfssm(hfssm_data, id = 'ID')
#'
#'
#' \dontrun{
#' }
#'
#' @export
score_hfssm <- function(hfssm_data, pna_value, base_zero = FALSE, id, session_id, extra_scale_cols = c()) {
  
  #### 1. Set up/initial checks #####
  
  # check that hfssm_data exist and is a data.frame
  data_arg <- methods::hasArg(hfssm_data)
  
  if (isTRUE(data_arg) & !is.data.frame(hfssm_data)) {
    stop("hfssm_data must be entered as a data.frame")
  } else if (isFALSE(data_arg)) {
    stop("hfssm_data must set to the data.frame with amount consumed for each food item")
  }
  
  # check if id exists
  ID_arg <- methods::hasArg(id)
  
  if (isTRUE(ID_arg)){
    if (!(id %in% names(hfssm_data))) {
      stop("variable name entered as id is not in hfssm_data")
    }
  }
  
  # check if session_id exists
  sessionID_arg <- methods::hasArg(session_id)
  
  if (isTRUE(sessionID_arg)){
    if (!(id %in% names(hfssm_data))) {
      stop("variable name entered as session_id is not in hfssm_data")
    }
  }
  
  # check base_zero is logical
  if (!is.logical(base_zero)) {
    stop("base_zero arg must be logical (TRUE/FALSE)")
  }
  
  #### 2. Set Up Data #####
  
  # set up database for results create empty matrix
  hfssm_score_dat <- data.frame(hfssm_household_haschildren = rep(NA, nrow(hfssm_data)),
                                hfssm_household_rawscore = rep(NA, nrow(hfssm_data)), 
                                hfssm_household_cat = rep(NA, nrow(hfssm_data)), 
                               hfssm_adult_rawscore = rep(NA, nrow(hfssm_data)), 
                               hfssm_adult_cat = rep(NA, nrow(hfssm_data)), 
                               hfssm_short_rawscore = rep(NA, nrow(hfssm_data)),
                               hfssm_short_cat = rep(NA, nrow(hfssm_data)), 
                               hfssm_children_rawscore = rep(NA, nrow(hfssm_data)), 
                               hfssm_children_cat = rep(NA, nrow(hfssm_data)))
  
  if (isTRUE(ID_arg)) {
    if (isTRUE(sessionID_arg)) {
      hfssm_score_dat <- data.frame(hfssm_data[[id]], hfssm_data[[session_id]], hfssm_score_dat)
      names(hfssm_score_dat)[1:2] <- c(id, session_id)
    } else {
      hfssm_score_dat <- data.frame(hfssm_data[[id]], hfssm_score_dat)
      names(hfssm_score_dat)[1] <- id
    }
  }
  
  # get hfssm items
  hfssm_items <- c("hfssm_hh1", "hfssm_hh2", "hfssm_hh3", "hfssm_hh4", 
                        "hfssm_ad1", "hfssm_ad1a", "hfssm_ad2", "hfssm_ad3", "hfssm_ad4", "hfssm_ad5", "hfssm_ad5a",
                        "hfssm_ch1", "hfssm_ch2", "hfssm_ch3", "hfssm_ch4","hfssm_ch5", "hfssm_ch5a", "hfssm_ch6", "hfssm_ch7")
  
  # if pna_value arg, replace not applicable values with NA
  if (isTRUE(methods::hasArg(pna_value))) {
    
    # replace pna_value with NA in pcw_vars
    hfssm_score_dat[hfssm_items] <- lapply(hfssm_score_dat[hfssm_items] , function(x) ifelse(x == pna_value, NA, x))
    
  }

  # re-scale data to base 1
  hfssm_data_edit <- hfssm_data
  
  if (isTRUE(base_zero)){
    hfssm_data_edit[hfssm_items] <- sapply(hfssm_items, function(x) hfssm_data[[x]] + 1, simplify = TRUE)
  }
  
  ## Score Subscales
  
  # Responses of “yes,” “often,” “sometimes,” “almost every month,” and “some months but not
  # every month” are coded as affirmative. The sum of affirmative responses to a specified set of
  # items is referred to as the household’s raw score on the scale comprising those items.
  
  # create dataframe with affirmative responses and screener indicators
  affirmative_df <- hfssm_data_edit %>% dplyr::mutate(
    
    # create affirmative variables where 1 = affirmative response
    hfssm_hh2 = ifelse(is.na(hfssm_hh2), NA, ifelse(hfssm_hh2 %in% c(1,2), 1, 0)),
    hfssm_hh3 = ifelse(is.na(hfssm_hh3), NA, ifelse(hfssm_hh3 %in% c(1,2), 1, 0)),
    hfssm_hh4 = ifelse(is.na(hfssm_hh4), NA, ifelse(hfssm_hh4 %in% c(1,2), 1, 0)),
    hfssm_ad1 = ifelse(is.na(hfssm_ad1), NA, ifelse(hfssm_ad1 == 1, 1, 0)),
    hfssm_ad1a = ifelse(is.na(hfssm_ad1a), NA, ifelse(hfssm_ad1a %in% c(1,2), 1, 0)),
    hfssm_ad2 = ifelse(is.na(hfssm_ad2), NA, ifelse(hfssm_ad2 == 1, 1, 0)),
    hfssm_ad3 = ifelse(is.na(hfssm_ad3), NA, ifelse(hfssm_ad3 == 1, 1, 0)),
    hfssm_ad4 = ifelse(is.na(hfssm_ad4), NA, ifelse(hfssm_ad4 == 1, 1, 0)),
    hfssm_ad5 = ifelse(is.na(hfssm_ad5), NA, ifelse(hfssm_ad5 == 1, 1, 0)),
    hfssm_ad5a = ifelse(is.na(hfssm_ad5a), NA, ifelse(hfssm_ad5a %in% c(1,2), 1, 0)),
    hfssm_ch1 = ifelse(is.na(hfssm_ch1), NA, ifelse(hfssm_ch1 %in% c(1,2), 1, 0)),
    hfssm_ch2 = ifelse(is.na(hfssm_ch2), NA, ifelse(hfssm_ch2 %in% c(1,2), 1, 0)),
    hfssm_ch3 = ifelse(is.na(hfssm_ch3), NA, ifelse(hfssm_ch3 %in% c(1,2), 1, 0)),
    hfssm_ch4 = ifelse(is.na(hfssm_ch4), NA, ifelse(hfssm_ch4 == 1, 1, 0)),
    hfssm_ch5 = ifelse(is.na(hfssm_ch5), NA, ifelse(hfssm_ch5 == 1, 1, 0)),
    hfssm_ch5a = ifelse(is.na(hfssm_ch5a), NA, ifelse(hfssm_ch5a %in% c(1,2), 1, 0)),
    hfssm_ch6 = ifelse(is.na(hfssm_ch6), NA, ifelse(hfssm_ch6 == 1, 1, 0)),
    hfssm_ch7 = ifelse(is.na(hfssm_ch7), NA, ifelse(hfssm_ch7 == 1, 1, 0)),
    
    # create screener variables where 1 = pass screener
    
    # pass adult stage 2 screener if (1) affirmative response to hfssm_hh2, hfssm_hh3 or hfssm_hh4 or (2) response [3] or [4] to hh1
    screener_ad_stage2 = ifelse((hfssm_hh2 == 1 | hfssm_hh3 == 1 | hfssm_hh4 == 1 ), 1, ifelse((hfssm_hh1 == 3 |  hfssm_hh1 == 4), 1, 0)),
    
    # pass adult stage 3 screener if passed adult stage 2 screener and had affirmative response to one or more questions from ad1-ad4
    screener_ad_stage3 = ifelse((screener_ad_stage2 == 1 & (hfssm_ad1 == 1 | hfssm_ad2 == 1 | hfssm_ad3 == 1 | hfssm_ad4 == 1)), 1, 0),
    
    # pass child stage 2 screener if affirmative response to one or more questions from ch1-ch3
    screener_ch_stage2 = ifelse((hfssm_ch1 == 1 | hfssm_ch2 == 1 | hfssm_ch3 == 1), 1, 0)
    
  )
  
  # determine if NAs exist for administered items per scale (TRUE = NAs in administered items; FALSE = no NAs in administered items)
  
  ## adult scale (HH2 through AD5a): hh2-hh4 administered to all; ad1-ad4 administered if affirmative response to hh2-hh4 (screener_ad_stage2 == 1); ad1a administered if affirmative response to ad1; ad5 administered if affirmative response to ad1-ad4; ad5a administered if affirmative response to ad5
  affirmative_df <- affirmative_df %>% dplyr::mutate(
    adult_scale_admin_nas = dplyr::case_when(
      is.na(hfssm_hh2) | is.na(hfssm_hh3) | is.na(hfssm_hh4) ~ TRUE, # missing administered items (hh2-hh4)
      screener_ad_stage2 == 1 & (is.na(hfssm_ad1) | is.na(hfssm_ad2) | is.na(hfssm_ad3) | is.na(hfssm_ad4))  ~ TRUE, # missing administered items (ad1-ad4)
      hfssm_ad1 == 1 & is.na(hfssm_ad1a) ~ TRUE, # missing administered item (ad1a)
      screener_ad_stage3 == 1 & is.na(hfssm_ad5) ~ TRUE, # missing administered item (ad5)
      hfssm_ad5 == 1 & is.na(hfssm_ad5a) ~ TRUE, # missing administered item (ad5a)
      TRUE ~ FALSE # all other scenarios do not have missing administered items 
    )
  )
  
  ## short scale (HH3 through AD3): hh3-hh4 administered to all; ad1-ad3 administered if affirmative response to hh2-hh4; ad1a administered if affirmative response to ad1
  affirmative_df <- affirmative_df %>% dplyr::mutate(
    short_scale_admin_nas = dplyr::case_when(
      is.na(hfssm_hh3) | is.na(hfssm_hh4) ~ TRUE, # missing administered items (hh3-hh4)
      screener_ad_stage2 == 1 & (is.na(hfssm_ad1) | is.na(hfssm_ad2) | is.na(hfssm_ad3) )  ~ TRUE, # missing administered items (ad1-ad3)
      hfssm_ad1 == 1 & is.na(hfssm_ad1a) ~ TRUE, # missing administered item (ad1a)
      TRUE ~ FALSE # all other scenarios do not have missing administered items 
    )
  )
  
  ## child scale: ch1-ch3 administered to all with children; ch4-ch7 administered if affirmative response to ch1-ch3; ch5a administered if affirmative response to ch5
  affirmative_df <- affirmative_df %>% dplyr::mutate(
    child_scale_admin_nas = dplyr::case_when(
      is.na(hfssm_ch1) | is.na(hfssm_ch2) | is.na(hfssm_ch3) ~ TRUE, # missing administered items (ch1-ch3)
      screener_ch_stage2 == 1 & (is.na(hfssm_ch4) | is.na(hfssm_ch5) | is.na(hfssm_ch6) | is.na(hfssm_ch7)) ~ TRUE, # missing administered items (ch4-ch7)
      hfssm_ch5 == 1 & is.na(hfssm_ch5a) ~ TRUE, # missing administered item (ch5a)
      TRUE ~ FALSE # all other scenarios do not have missing administered items 
      )
    ) 
  
  ## HH scale with children (HH2 through CH7): combo of adult scale and child scale
  affirmative_df <- affirmative_df %>% dplyr::mutate(
    hh_scale_admin_nas = dplyr::case_when(
      is.na(hfssm_hh2) | is.na(hfssm_hh3) | is.na(hfssm_hh4) ~ TRUE, # missing administered items (hh2-hh4)
      screener_ad_stage2 == 1 & (is.na(hfssm_ad1) | is.na(hfssm_ad2) | is.na(hfssm_ad3) | is.na(hfssm_ad4))  ~ TRUE, # missing administered items (ad1-ad4)
      hfssm_ad1 == 1 & is.na(hfssm_ad1a) ~ TRUE, # missing administered item (ad1a)
      screener_ad_stage3 == 1 & is.na(hfssm_ad5) ~ TRUE, # missing administered item (ad5)
      hfssm_ad5 == 1 & is.na(hfssm_ad5a) ~ TRUE, # missing administered item (ad5a)
      (!is.na(hfssm_hh2) & !is.na(hfssm_hh3) & !is.na(hfssm_hh4)) & (is.na(hfssm_ch1) & is.na(hfssm_ch2) & is.na(hfssm_ch3) & is.na(hfssm_ch4) & is.na(hfssm_ch5) & is.na(hfssm_ch6) & is.na(hfssm_ch7)) ~ FALSE, # suggests adult completed form but no child
      is.na(hfssm_ch1) | is.na(hfssm_ch2) | is.na(hfssm_ch3) ~ TRUE, # missing administered items (ch1-ch3)
      screener_ch_stage2 == 1 & (is.na(hfssm_ch4) | is.na(hfssm_ch5) | is.na(hfssm_ch6) | is.na(hfssm_ch7)) ~ TRUE, # missing administered items (ch4-ch7)
      hfssm_ch5 == 1 & is.na(hfssm_ch5a) ~ TRUE, # missing administered item (ch5a)
      
      TRUE ~ FALSE # all other scenarios have missing administered items 
    )
  )
  
  # U.S. Household Food Security Scale - households with children
  hfssm_score_dat$hfssm_household_haschildren <- !is.na(hfssm_data_edit$hfssm_ch1)
  
  # score for U.S. Household Food Security Scale
  ## For households with children, score based on HH2 through CH7 (includes HH, AD, CH items)
  ## For households without children, score based on HH2 through AD5a (includes HH and AD items)
  hh_scale_vars <- c("hfssm_hh2", "hfssm_hh3", "hfssm_hh4", 
                     "hfssm_ad1", "hfssm_ad1a", "hfssm_ad2", "hfssm_ad3", "hfssm_ad4", "hfssm_ad5", "hfssm_ad5a",
                     "hfssm_ch1", "hfssm_ch2", "hfssm_ch3", "hfssm_ch4","hfssm_ch5", "hfssm_ch5a", "hfssm_ch6", "hfssm_ch7")

  # calculate hfssm_household_rawscore if no missing responses to administered questions (hh_scale_admin_nas == 0)
  hfssm_score_dat$hfssm_household_rawscore <- ifelse(affirmative_df$hh_scale_admin_nas == FALSE, rowSums(affirmative_df[hh_scale_vars], na.rm = TRUE), NA)

  hfssm_score_dat$hfssm_household_cat <- ifelse(hfssm_score_dat$hfssm_household_haschildren == TRUE, ifelse(is.na(hfssm_score_dat$hfssm_household_rawscore), NA, 
                                                ifelse(hfssm_score_dat$hfssm_household_rawscore == 0, "high", 
                                                       ifelse(hfssm_score_dat$hfssm_household_rawscore <= 2, "marginal", 
                                                              ifelse(hfssm_score_dat$hfssm_household_rawscore <= 7, "low", "very low")))),
                                          # else, calculate score based on no children in household      
                                          ifelse(is.na(hfssm_score_dat$hfssm_household_rawscore), NA, 
                                                  ifelse(hfssm_score_dat$hfssm_household_rawscore == 0, "high",
                                                         ifelse(hfssm_score_dat$hfssm_household_rawscore <= 2, "marginal",
                                                                ifelse(hfssm_score_dat$hfssm_household_rawscore <= 5, "low", "very low")))))

  # U.S. Adult Food Security Scale
  
  # isolate questions HH2 through AD5a
  ad_scale_vars <- c("hfssm_hh2",  "hfssm_hh3", "hfssm_hh4", 
                     "hfssm_ad1", "hfssm_ad1a", "hfssm_ad2", "hfssm_ad3", "hfssm_ad4", "hfssm_ad5", "hfssm_ad5a")
  
  # calculate hfssm_adult_rawscore if no missing responses to administered questions (adult_scale_admin_nas == FALSE)
  hfssm_score_dat$hfssm_adult_rawscore <- ifelse(affirmative_df$adult_scale_admin_nas == FALSE, rowSums(affirmative_df[ad_scale_vars], na.rm = TRUE), NA)
  
  hfssm_score_dat$hfssm_adult_cat <- ifelse(is.na(hfssm_score_dat$hfssm_adult_rawscore), NA, 
                                            ifelse(hfssm_score_dat$hfssm_adult_rawscore == 0, "high", 
                                                  ifelse(hfssm_score_dat$hfssm_adult_rawscore <= 2, "marginal", 
                                                        ifelse(hfssm_score_dat$hfssm_adult_rawscore <= 5, "low", "very low"))))    
  # 6-item Food Security Scale
  # isolate questions HH3 through AD3
  short_scale_vars <- c("hfssm_hh3",  "hfssm_hh4",  "hfssm_ad1",  "hfssm_ad1a", "hfssm_ad2",  "hfssm_ad3" )
  
  # calculate hfssm_short_rawscore if no missing responses to administered questions (short_scale_admin_nas == FALSE)
  hfssm_score_dat$hfssm_short_rawscore <- ifelse(affirmative_df$short_scale_admin_nas == FALSE, rowSums(affirmative_df[short_scale_vars], na.rm = TRUE), NA)
  
  hfssm_score_dat$hfssm_short_cat <- ifelse(is.na(hfssm_score_dat$hfssm_short_rawscore), NA, 
                                            ifelse(hfssm_score_dat$hfssm_short_rawscore <= 1, "high/marginal", 
                                                   ifelse(hfssm_score_dat$hfssm_short_rawscore <= 4, "low", "very low")))
  
  # U.S. Children’s Food Security Scale
  # isolate questions CH1 through CH7 
  child_scale_vars <-c("hfssm_ch1",  "hfssm_ch2",  "hfssm_ch3",  "hfssm_ch4", "hfssm_ch5", "hfssm_ch5a", "hfssm_ch6", "hfssm_ch7")
  
  # calculate hfssm_short_rawscore if no missing responses to administered questions (child_scale_admin_nas == FALSE)
  hfssm_score_dat$hfssm_children_rawscore <- ifelse(affirmative_df$child_scale_admin_nas == FALSE, rowSums(affirmative_df[child_scale_vars], na.rm = TRUE), NA)
  
  hfssm_score_dat$hfssm_children_cat <- ifelse(hfssm_score_dat$hfssm_household_haschildren == FALSE, NA, ifelse(is.na(hfssm_score_dat$hfssm_children_rawscore), NA, 
                                              ifelse(hfssm_score_dat$hfssm_children_rawscore <= 1, "high/marginal", 
                                                     ifelse(hfssm_score_dat$hfssm_children_rawscore <= 4, "low", "very low"))))
  #### 3. Clean Export/Scored Data #####
  
  ## merge raw responses with scored data
  if (isTRUE(ID_arg)){
    if (isTRUE(sessionID_arg)) {
      hfssm_phenotype <- merge(hfssm_data, hfssm_score_dat, by = c(id, session_id))
    } else {
      hfssm_phenotype <- merge(hfssm_data, hfssm_score_dat, by = id)
    }
    
    return(list(score_dat = as.data.frame(hfssm_score_dat),
                bids_phenotype = as.data.frame(hfssm_phenotype)))
  } else {
    return(list(score_dat = as.data.frame(hfssm_score_dat)))
  }
}

