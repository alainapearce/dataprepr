#' score_hfssm: Scored data from the U.S. Household Food Security Survey Module
#'
#' This function scores the U.S. Household Food Security Survey Module and provides scores for: U.S. Household Food Security Scale, U.S. Adult Food Security Scale, 6-item Food Security Scale, U.S. Children’s Food Security Scale
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items, excluding hh1, which is not used for scoring food security scales
#' 2) The  columns/variables must match the following naming conventions: 'hfssm_hh#', 'hfssm_ad#', or 'hfssm_ch#', where # is the question number and hh/ad/ch refers to the questionnaire stage (household, adult, child)
#' 3) Questions [hh2, hh3, hh4, ch1, ch2, ch3] must have the numeric value for the choices: 1 - Often True, 2 - Sometimes True, 3 - Never True, 4 - DK or Refused
#'    Questions [ad1, ad2, ad3, ad4, ad5, ch4, ch5, ch6, ch7] must have the numeric value for the choices: 1 - yes, 2 - no, 3 - DK
#'    Questions [ad1a, ad5a, ch5a] must have the numeric value for the choices: 1 - almost every month, 2 - Some months but not every month, 3 - Only 1 or 2 months, 4 - DK
#'
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' U.S. HOUSEHOLD FOOD SECURITY SURVEY MODULE: THREE-STAGE DESIGN, WITH SCREENERS Economic Research Service, USDA September 2012 https://www.ers.usda.gov/media/8271/hh2012.pdf
#' 
#' @param hfssm_data a data.frame all items for the U.S. Household Food Security Survey Module following the naming conventions described above
#' @inheritParams score_bes
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
score_hfssm <- function(hfssm_data, base_zero = TRUE, id) {
  
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
    hfssm_score_dat <- data.frame(hfssm_data[[id]], hfssm_score_dat)
    names(hfssm_score_dat)[1] <- id
  }
  
  # get hfssm items to score
  hfssm_items <- c("hfssm_hh2", "hfssm_hh3", "hfssm_hh4", 
                        "hfssm_ad1", "hfssm_ad1a", "hfssm_ad2", "hfssm_ad3", "hfssm_ad4", "hfssm_ad5", "hfssm_ad5a",
                        "hfssm_ch1", "hfssm_ch2", "hfssm_ch3", "hfssm_ch4","hfssm_ch5", "hfssm_ch5a", "hfssm_ch6", "hfssm_ch7")

  # re-scale data
  hfssm_data_edit <- hfssm_data
  
  if (isTRUE(base_zero)){
    hfssm_data_edit[hfssm_items] <- sapply(hfssm_items, function(x) hfssm_data[[x]] + 1, simplify = TRUE)
  }
  
  ## Score Subscales
  
  # Responses of “yes,” “often,” “sometimes,” “almost every month,” and “some months but not
  # every month” are coded as affirmative. The sum of affirmative responses to a specified set of
  # items is referred to as the household’s raw score on the scale comprising those items.
  
  # create affirmative dataframe where 1 = affirmative response
  affirmative_df <- hfssm_data_edit %>% dplyr::mutate(
    hfssm_hh2 = ifelse(hfssm_hh2 %in% c(1,2), 1, 0),
    hfssm_hh3 = ifelse(hfssm_hh3 %in% c(1,2), 1, 0),
    hfssm_hh4 = ifelse(hfssm_hh4 %in% c(1,2), 1, 0),
    hfssm_ad1 = ifelse(hfssm_ad1 == 1, 1, 0),
    hfssm_ad1a = ifelse(hfssm_ad1a %in% c(1,2), 1, 0),
    hfssm_ad2 = ifelse(hfssm_ad2 == 1, 1, 0),
    hfssm_ad3 = ifelse(hfssm_ad3 == 1, 1, 0),
    hfssm_ad4 = ifelse(hfssm_ad4 == 1, 1, 0),
    hfssm_ad5 = ifelse(hfssm_ad5 == 1, 1, 0),
    hfssm_ad5a = ifelse(hfssm_ad5a %in% c(1,2), 1, 0),
    hfssm_ch1 = ifelse(hfssm_ch1 %in% c(1,2), 1, 0),
    hfssm_ch2 = ifelse(hfssm_ch2 %in% c(1,2), 1, 0),
    hfssm_ch3 = ifelse(hfssm_ch3 %in% c(1,2), 1, 0),
    hfssm_ch4 = ifelse(hfssm_ch4 %in% c(1,2), 1, 0),
    hfssm_ch5 = ifelse(hfssm_ch5 == 1, 1, 0),
    hfssm_ch5a = ifelse(hfssm_ch5a %in% c(1,2), 1, 0),
    hfssm_ch6 = ifelse(hfssm_ch6 == 1, 1, 0),
    hfssm_ch7 = ifelse(hfssm_ch7 == 1, 1, 0)
  )
  
  
  # U.S. Household Food Security Scale - households with children
  hfssm_score_dat$hfssm_household_haschildren <- !is.na(hfssm_data_edit$hfssm_ch1)
  
  # score for household with children (HH2 through CH7)
  hh_vars <- hfssm_items
  hfssm_score_dat$hfssm_household_rawscore <- rowSums(affirmative_df[hh_vars], na.rm = TRUE)
  
    
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
  ad_vars <- hfssm_items[4:9]
  
  hfssm_score_dat$hfssm_adult_rawscore <- rowSums(affirmative_df[ad_vars], na.rm = TRUE)
  hfssm_score_dat$hfssm_adult_cat <- ifelse(is.na(hfssm_score_dat$hfssm_adult_rawscore), NA, 
                                            ifelse(hfssm_score_dat$hfssm_adult_rawscore == 0, "high", 
                                                  ifelse(hfssm_score_dat$hfssm_adult_rawscore <= 2, "marginal", 
                                                        ifelse(hfssm_score_dat$hfssm_adult_rawscore <= 5, "low", "very low"))))    
  # 6-item Food Security Scale
  # isolate questions HH3 through AD3
  short_vars <- hfssm_items[2:7]
  
  hfssm_score_dat$hfssm_short_rawscore <- rowSums(affirmative_df[short_vars], na.rm = TRUE)
  hfssm_score_dat$hfssm_short_cat <- ifelse(is.na(hfssm_score_dat$hfssm_short_rawscore), NA, 
                                            ifelse(hfssm_score_dat$hfssm_short_rawscore <= 1, "high/marginal", 
                                                   ifelse(hfssm_score_dat$hfssm_short_rawscore <= 4, "low", "very low")))
  
  # U.S. Children’s Food Security Scale
  # isolate questions CH1 through CH7 
  child_vars <- hfssm_items[11:18]
  
  hfssm_score_dat$hfssm_children_rawscore <- rowSums(affirmative_df[child_vars], na.rm = TRUE)
  hfssm_score_dat$hfssm_children_cat <- ifelse(is.na(hfssm_score_dat$hfssm_children_rawscore), NA, 
                                              ifelse(hfssm_score_dat$hfssm_children_rawscore <= 1, "high/marginal", 
                                                     ifelse(hfssm_score_dat$hfssm_children_rawscore <= 4, "low", "very low")))
  #### 3. Clean Export/Scored Data #####
  
  ## merge raw responses with scored data
  if (isTRUE(ID_arg)){
    hfssm_phenotype <- merge(hfssm_data, hfssm_score_dat, by = id)
    
    return(list(score_dat = as.data.frame(hfssm_score_dat),
                bids_phenotype = as.data.frame(hfssm_phenotype)))
  } else {
    return(list(score_dat = as.data.frame(hfssm_score_dat)))
  }
}

