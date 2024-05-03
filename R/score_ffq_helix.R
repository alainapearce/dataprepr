#' score_ffq_helix: Scored data from the HELIX study Food Frequency Questionnaire
#'
#' This function scores the Food Frequency Questionnaire developed for the HELIX study. Daily frequency of food intake will be calculated according to the following prior scoring conventions by Stratakis et al., 2022:
#' 
#' 1) 16 categories: dairy, eggs, meat/meat products, fish/seafood, nuts, vegetables, lentils/pulses, fruit, potatoes, breads/cereals, sweets, beverages, sweet bakery products, salty snacks (chips), added fats, and dressings
#' 2) Ultra-processed foods: cookies, pastries, sugar-sweetened, low-sugar and artificially sweetened beverages, cold meat cuts; ham, dairy desserts, sugar-sweetened and other breakfast cereals, crispbread and rusks; chocolate, sweets, margarine, dressings, and salty snack
#' 
#' *Note: there are also scoring conventions for 11 categories (Lau et al., 2018; dairy, meat/meat products, fish/seafood, vegetables, fruit, potatoes, breads/cereals, sweets, beverages, sweet bakery products, added fats) and 7 categories (Papadopoulou et al., 2019; animal origin--dairy, meat/meat products, fish/seafood and plant origin--vegetables, fruit, potatoes, breads/cereals, and pulses)
#' 3) 
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The columns/variables must match the following naming convention: 'ffq_xxx#' xxx is the food category and the # is the item number for the category
#' 3) All questions must have the numeric value for the choice with the base value being either 0 (base_zero = TRUE) or 1 (base_zero = FALSE)
#'
#' Note, as long as variable names match those listed in this script, the dataset can include other variables
#'
#' @references
#' Primary for HELIX study: 
#' Maitre L, De Bont J, Casas M, et al. Human Early Life Exposome (HELIX) study: a European population-based exposome cohort. BMJ Open. 2018;8(9):e021311. doi:10.1136/bmjopen-2017-021311 (\href{https://pubmed.ncbi.nlm.nih.gov/30206078/}{PubMed})
#' 
#' Scoring:
#' Stratakis N, Siskos AP, Papadopoulou E, et al. Urinary metabolic biomarkers of diet quality in European children are associated with metabolic health. eLife. 2022;11:e71332. doi:10.7554/eLife.71332 (\href{https://pubmed.ncbi.nlm.nih.gov/35076016/}{PubMed})
#' 
#' Lau CHE, Siskos AP, Maitre L, et al. Determinants of the urinary and serum metabolome in children from six European populations. BMC Med. 2018;16(1):202. doi:10.1186/s12916-018-1190-8 (\href{https://pubmed.ncbi.nlm.nih.gov/30404627/}{PubMed})
#' 
#' Papadopoulou E, Haug LS, Sakhi AK, et al. Diet as a Source of Exposure to Environmental Contaminants for Pregnant Women and Children from Six European Countries. Environ Health Perspect. 2019;127(10):107005. doi:10.1289/EHP5324 (\href{https://pubmed.ncbi.nlm.nih.gov/31617753/}{PubMed})
#'
#' @param ffq_data a data.frame all items for the HELIX Food Fequency Questionnaire following the naming conventions described above
#' @inheritParams score_bes
#' @inheritParams score_pds
#'
#' @return A dataset with servings per week calculated for the various food categories
#' @examples
#'
#' # scoring for the bes with IDs
#' ffq_score_data <- score_ffq_helix(ffq_data, base_zero = TRUE, id = 'ID')
#'
#' \dontrun{
#' } 
#'
#'
#' @export

score_ffq_helix <- function(ffq_data, base_zero = TRUE, id) {
  
  #### 1. Set up/initial checks #####
  
  # check that ffq_data exist and is a data.frame
  data_arg <- methods::hasArg(ffq_data)
  
  if (isTRUE(data_arg) & !is.data.frame(ffq_data)) {
    stop('ffq_data must be entered as a data.frame')
  } else if (isFALSE(data_arg)) {
    stop('ffq_data must set to the data.frame with amount consumed for each food item')
  }
  
  # check if id exists
  ID_arg <- methods::hasArg(id)
  
  if (isTRUE(ID_arg)){
    if (!(id %in% names(ffq_data))) {
      stop('variable name entered as id is not in ffq_data')
    }
  }
  
  #### 2. Set Up Data #####
  
  # set up database for results create empty matrix
  ffq_score_dat <- data.frame(dairy = rep(NA, nrow(ffq_data)), eggs = rep(NA, nrow(ffq_data)), meat = rep(NA, nrow(ffq_data)), fish = rep(NA, nrow(ffq_data)), nuts = rep(NA, nrow(ffq_data)), vegetables = rep(NA, nrow(ffq_data)), pulses = rep(NA, nrow(ffq_data)), fruit = rep(NA, nrow(ffq_data)), potatoes = rep(NA, nrow(ffq_data)), breads_cereals = rep(NA, nrow(ffq_data)), sweets = rep(NA, nrow(ffq_data)), beverages = rep(NA, nrow(ffq_data)), sweet_bakery = rep(NA, nrow(ffq_data)), salty_snacks = rep(NA, nrow(ffq_data)), added_fats = rep(NA, nrow(ffq_data)), dressings = rep(NA, nrow(ffq_data)), animal = rep(NA, nrow(ffq_data)), plants = rep(NA, nrow(ffq_data)), upf = rep(NA, nrow(ffq_data)), not_upf = rep(NA, nrow(ffq_data)), prop_upf = rep(NA, nrow(ffq_data)))
  
  if (isTRUE(ID_arg)) {
    ffq_score_dat <- data.frame(ffq_data[[id]], ffq_score_dat)
    names(ffq_score_dat)[1] <- id
  }
  
  # re-scale data
  ffq_data_edit <- ffq_data
  
  if (isTRUE(base_zero)){
    ffq_data_edit[2:44] <- sapply(names(ffq_data_edit)[2:44], function(x) ffq_data_edit[[x]] + 1, simplify = TRUE)
  }
  
  # convert to servings per week
  servings_wk <- function(var){
    var_quant <- ifelse(is.na(var), NA, 
                        ifelse(var == 1, 0, 
                               ifelse(var == 2, 0.25,
                                      ifelse(var == 3, 0.5,
                                             ifelse(var == 4, 1,
                                                    ifelse(var == 5, 3, 
                                                           ifelse(var == 6, 5.5,
                                                                  ifelse(var == 7, 7, 
                                                                         ifelse(var == 8, 17.5, 28)))))))))
    
    return(var_quant)
  }
  
  ffq_data_edit[2:44] <- sapply(names(ffq_data_edit)[2:44], function(x) servings_wk(ffq_data_edit[[x]]), simplify = TRUE)
  
  ## Score - used sum to get total servings per week
  
  ### Dairy
  ffq_score_dat[['dairy']] <- base::rowMeans(ffq_data_edit[ , grepl('dairy', names(ffq_data_edit))])
  
  ### Eggs
  ffq_score_dat[['eggs']] <- ffq_data_edit[ , grepl('egg', names(ffq_data_edit))]
  
  ### Meat
  ffq_score_dat[['meat']] <- base::rowMeans(ffq_data_edit[ , grepl('meat', names(ffq_data_edit))])
  
  ### Fish/Seafood
  ffq_score_dat[['fish']] <- base::rowMeans(ffq_data_edit[ , grepl('fish', names(ffq_data_edit))])
  
  ### Nuts
  ffq_score_dat[['nuts']] <- ffq_data_edit[ , grepl('nut', names(ffq_data_edit))]
  
  ### Vegetables
  ffq_score_dat[['vegetables']] <- base::rowMeans(ffq_data_edit[ , grepl('veg', names(ffq_data_edit))])
  
  ### Pulses
  ffq_score_dat[['pulses']] <- ffq_data_edit[ , grepl('legume', names(ffq_data_edit))]
  
  ### Fruit
  ffq_score_dat[['fruit']] <- base::rowMeans(ffq_data_edit[ , grepl('fruit', names(ffq_data_edit))])
  
  ### Potatoes
  ffq_score_dat[['potatoes']] <- base::rowMeans(ffq_data_edit[ , grepl('potato', names(ffq_data_edit))])
  
  ### Breads/Cereals
  ffq_score_dat[['breads_cereals']] <- base::rowMeans(ffq_data_edit[ , grepl('cereal', names(ffq_data_edit))])
  
  ### Sweets
  ffq_score_dat[['sweets']] <- base::rowMeans(ffq_data_edit[ , grepl('sweet', names(ffq_data_edit))])
  
  ### Beverages
  ffq_score_dat[['beverages']] <- base::rowMeans(ffq_data_edit[ , grepl('bev', names(ffq_data_edit))])
  
  ### Sweet Bakery
  ffq_score_dat[['sweet_bakery']] <- base::rowMeans(ffq_data_edit[ , grepl('bakery', names(ffq_data_edit))])
  
  ### Salty Snack
  ffq_score_dat[['salty_snacks']] <- ffq_data_edit[ , grepl('salt', names(ffq_data_edit))]
  
  ### Added Fats
  ffq_score_dat[['added_fats']] <- base::rowMeans(ffq_data_edit[ , grepl('fats', names(ffq_data_edit))])
  
  ### Dressings
  ffq_score_dat[['dressings']] <- ffq_data_edit[ , grepl('dressing', names(ffq_data_edit))]
  
  ### Animal
  ffq_score_dat[['animal']] <- base::rowMeans(ffq_data_edit[ , grepl('dairy|egg|meat|fish', names(ffq_data_edit))])
  
  ### Plants
  ffq_score_dat[['plants']] <- base::rowMeans(ffq_data_edit[ , grepl('veg|fruit|nuts|legume|potato|cereal', names(ffq_data_edit))])
  
  ### UPF
  upf_vars <- c('ffq_bakery1', 'ffq_bakery2', 'ffq_bev1', 'ffq_bev2', 'ffq_meat3', 'ffq_meat4', 'ffq_dairy5', 'ffq_cereal3', 'ffq_cereal4', 'ffq_cereal6', 'ffq_sweet1', 'ffq_sweet3', 'ffq_fats4', 'ffq_dressing1', 'ffq_saltysnack1')
  
  # get sum of DAILY servings
  ffq_score_dat[['upf']] <- rowSums(ffq_data_edit[upf_vars])*7
  
  ### Not - UPF
  not_upf_vars <- c('ffq_dairy1', 'ffq_dairy2', 'ffq_dairy3', 'ffq_dairy4', 'ffq_egg1', 'ffq_meat1', 'ffq_meat3', 'ffq_fish1', 'ffq_fish2', 'ffq_fish3', 'ffq_fish4', 'ffq_veg1', 'ffq_veg2', 'ffq_potato1', 'ffq_potato2', 'ffq_nuts1', 'ffq_legume1', 'ffq_fruit1', 'ffq_fruit2', 'ffq_fruit3', 'ffq_fruit4', 'ffq_cereal1', 'ffq_cereal2', 'ffq_cereal5', 'ffq_sweet2', 'ffq_fats1', 'ffq_fats2', 'ffq_fats3')
  
  # get sum of DAILY servings
  ffq_score_dat[['not_upf']] <- rowSums(ffq_data_edit[not_upf_vars])*7
  
  ### we calculated the daily proportion of all UPF in the total diet as the ratio between the sum of daily servings of UPF to the total daily sum of all food and drink servings
  ffq_score_dat[['prop_upf']] <- ffq_score_dat[['upf']]/ffq_score_dat[['not_upf']]
  
  #### 3. Clean Export/Scored Data #####
  ## round data
  if (isTRUE(ID_arg)){
    ffq_score_dat[2:ncol(ffq_score_dat)] <- round(ffq_score_dat[2:ncol(ffq_score_dat)], digits = 3)
  } else {
    ffq_score_dat <- round(ffq_score_dat, digits = 3)
  }
  
  ## merge raw responses with scored data
  if (isTRUE(ID_arg)){
    ffq_phenotype <- merge(ffq_data, ffq_score_dat, by = 'participant_id')
    
    ## re-order to make more sense (supplements at end)
    ffq_phenotype <- ffq_phenotype[c(1:44, 68:88, 45:67)]
    
    return(list(score_dat = as.data.frame(ffq_score_dat),
                bids_phenotype = as.data.frame(ffq_phenotype)))
    
  } else {
    return(list(score_dat = as.data.frame(ffq_score_dat)))
  }
  
}

