#' score_hfi: Score data from the Fulkerson Home Food Inventory
#'
#' This function scores the Fulkerson Home Food Inventory and provides subscale scores for: 1) 13 categories of food and 2) accessibility categories for kitchen and fridge (each sub-divided into unhealthy and healthy), and 3) an obesogenic score
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'hfi_category_letter_type#' where 'letter' is the food item order and # type (where relevant)
#' 3) All questions must have the numeric value for the choice with the base value being either 0 (score_base = TRUE) or 1 (score_base = FALSE)
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Fulkerson JA, Nelson MC, Lytle LA, Moe S, Heitzler C, Pasch KE. The validation of a home food inventory. International Journal of Behavioral Nutrition and Physical Activity, 2008, 5;55 (\href{https://pubmed.ncbi.nlm.nih.gov/18983668/}{PubMed})
#'
#' @param hfi_data a data.frame all items for the Fulkerson Home Food Inventory following the naming conventions described above
#' @inheritParams score_bes
#' @inheritParams score_bes
#'
#' @return A dataset with subscale scores for the ulkerson Home Food Inventory
#' @examples
#'
#' # scoring for the hfi with IDs
#' hfi_score_data <- score_hfi(hfi_data, id = 'ID')
#'
#'
#' @export

score_hfi <- function(hfi_data, score_base = TRUE, id) {

    #### 1. Set up/initial checks #####

    # check that hfi_data exist and is a data.frame
    data_arg <- methods::hasArg(hfi_data)

    if (isTRUE(data_arg) & !is.data.frame(hfi_data)) {
        stop('hfi_data must be entered as a data.frame')
    } else if (isFALSE(data_arg)) {
        stop('hfi_data must set to the data.frame with amount consumed for each food item')
    }

    # check if id exists
    ID_arg <- methods::hasArg(id)

    if (isTRUE(ID_arg)){
        if (!(id %in% names(hfi_data))) {
            stop('variable name entered as id is not in hfi_data')
        }
    }

    #### 2. Set Up Data #####

    # set up database for results create empty matrix
    hfi_score_dat <- data.frame(dairy = rep(NA, nrow(hfi_data)), dairy_lowfat = rep(NA, nrow(hfi_data)), vegetables = rep(NA, nrow(hfi_data)), veg_nopotato = rep(NA, nrow(hfi_data)), fruit = rep(NA, nrow(hfi_data)), meat_protein = rep(NA, nrow(hfi_data)), meat_protein_processed = rep(NA, nrow(hfi_data)), added_fat = rep(NA, nrow(hfi_data)), added_fat_lowfat = rep(NA, nrow(hfi_data)), frozen_dessert = rep(NA, nrow(hfi_data)), frozen_dessert_lowfat = rep(NA, nrow(hfi_data)), prepared_dessert = rep(NA, nrow(hfi_data)), prepared_dessert_lowfat = rep(NA, nrow(hfi_data)), savory_snacks = rep(NA, nrow(hfi_data)), savory_snacks_lowfat = rep(NA, nrow(hfi_data)), microwave_quickfood = rep(NA, nrow(hfi_data)), bread = rep(NA, nrow(hfi_data)), bread_wheat = rep(NA, nrow(hfi_data)), cereal = rep(NA, nrow(hfi_data)), cereal_highsugar = rep(NA, nrow(hfi_data)), cereal_wg = rep(NA, nrow(hfi_data)), candy = rep(NA, nrow(hfi_data)), beverages = rep(NA, nrow(hfi_data)), bev_lowsugar = rep(NA, nrow(hfi_data)), fridge_accesible = rep(NA, nrow(hfi_data)), fridge_healthy = rep(NA, nrow(hfi_data)), fridge_unhealthy = rep(NA, nrow(hfi_data)), kitchen_accesible = rep(NA, nrow(hfi_data)), kitchen_healthy = rep(NA, nrow(hfi_data)), kitchen_unheathy = rep(NA, nrow(hfi_data)), obesogenic_foods = rep(NA, nrow(hfi_data)))

    if (isTRUE(ID_arg)) {
        hfi_score_dat <- data.frame(hfi_data[[id]], hfi_score_dat)
        names(hfi_score_dat)[1] <- id
    }
    
    # re-scale data
    hfi_data_edit <- hfi_data
    
    if (isFALSE(score_base)){
      hfi_data_edit[3:402] <- sapply(names(hfi_data_edit)[3:402], function(x) hfi_data_edit[[x]] - 1, simplify = TRUE)
    }
    
    ## Score Subscales

    # diary
    hfi_score_dat[['dairy']] <- ifelse(rowSums(is.na(hfi_data_edit[ , grepl('dairy|cheese', names(hfi_data_edit))])) == 21, NA, base::rowSums(hfi_data_edit[ , grepl('dairy|cheese', names(hfi_data_edit))], na.rm = TRUE))
    
    hfi_score_dat[['dairy_lowfat']] <- ifelse(rowSums(is.na(hfi_data_edit[ , grepl('cheese_c|cheese_d|cheese_e|cheese_f|cheese_h|cheese_j|dairy_a|dairy_b|dairy_f|dairy_g|dairy_h|dairy_j', names(hfi_data_edit))])) == 12, NA, base::rowSums(hfi_data_edit[ , grepl('dairy|cheese', names(hfi_data_edit))], na.rm = TRUE))

    # vegetables
    hfi_score_dat[['vegetables']] <- ifelse(rowSums(is.na(hfi_data_edit[ , grepl('veg', names(hfi_data_edit)) & !grepl('type', names(hfi_data_edit))])) == 20, NA, base::rowSums(hfi_data_edit[ , grepl('veg', names(hfi_data_edit)) & !grepl('type', names(hfi_data_edit))], na.rm = TRUE))
    
    hfi_score_dat[['veg_nopotato']] <- ifelse(rowSums(is.na(hfi_data_edit[ , grepl('veg', names(hfi_data_edit)) & !grepl('type', names(hfi_data_edit)) & !grepl('_o', names(hfi_data_edit))])) == 19, NA, base::rowSums(hfi_data_edit[ , grepl('veg', names(hfi_data_edit)) & !grepl('type', names(hfi_data_edit)) & !grepl('_o', names(hfi_data_edit))], na.rm = TRUE))

    # fruit
    hfi_score_dat[['fruit']] <- ifelse(rowSums(is.na(hfi_data_edit[ , grepl('fruit', names(hfi_data_edit)) & !grepl('type', names(hfi_data_edit))])) == 26, NA, base::rowSums(hfi_data_edit[ , grepl('veg', names(hfi_data_edit)) & !grepl('type', names(hfi_data_edit))], na.rm = TRUE))

    # meat_protein
    hfi_score_dat[['meat_protein']] <- ifelse(rowSums(is.na(hfi_data_edit[ , grepl('deli|protein', names(hfi_data_edit))])) == 16, NA, base::rowSums(hfi_data_edit[ , grepl('deli|protein', names(hfi_data_edit))], na.rm = TRUE))
    
    hfi_score_dat[['meat_protein_processed']] <- ifelse(rowSums(is.na(hfi_data_edit[ , grepl('deli_c|deli_d|deli_e|deli_f', names(hfi_data_edit))])) == 13, NA, base::rowSums(hfi_data_edit[ , grepl('deli_c|deli_d|deli_e|deli_f', names(hfi_data_edit))], na.rm = TRUE))
    
    # added fat
    hfi_score_dat[['added_fat']] <- ifelse(rowSums(is.na(hfi_data_edit[ , grepl('oils|dressing|cond_a|cond_b|cond_c', names(hfi_data_edit))])) == 13, NA, base::rowSums(hfi_data_edit[ , grepl('oils|dressing|cond_a|cond_b|cond_c', names(hfi_data_edit))], na.rm = TRUE))
    
    hfi_score_dat[['added_fat_lowfat']] <- ifelse(rowSums(is.na(hfi_data_edit[ , grepl('oils_b|oils_d|dressing_b|cond_b|cond_c', names(hfi_data_edit))])) == 5, NA, base::rowSums(hfi_data_edit[ , grepl('oils_b|oils_d|dressing_b|cond_b|cond_c', names(hfi_data_edit))], na.rm = TRUE))
    
    # frozen desert
    hfi_score_dat[['frozen_dessert']] <- ifelse(rowSums(is.na(hfi_data_edit[ , grepl('frozen', names(hfi_data_edit))])) == 7, NA, base::rowSums(hfi_data_edit[ , grepl('frozen', names(hfi_data_edit))], na.rm = TRUE))
    
    hfi_score_dat[['frozen_dessert_lowfat']] <- ifelse(rowSums(is.na(hfi_data_edit[ , grepl('frozen_b|frozen_c|frozen_e|frozen_f', names(hfi_data_edit))])) == 4, NA, base::rowSums(hfi_data_edit[ , grepl('frozen_b|frozen_c|frozen_e|frozen_f', names(hfi_data_edit))], na.rm = TRUE))
    
    # prepared desert
    hfi_score_dat[['prepared_dessert']] <- ifelse(rowSums(is.na(hfi_data_edit[ , grepl('dessert', names(hfi_data_edit)) & !grepl('type', names(hfi_data_edit))])) == 8, NA, base::rowSums(hfi_data_edit[ , grepl('dessert', names(hfi_data_edit)) & !grepl('type', names(hfi_data_edit))], na.rm = TRUE))
    
    hfi_score_dat[['prepared_dessert_lowfat']] <- ifelse(rowSums(is.na(hfi_data_edit[ , grepl('dessert_b|dessert_d', names(hfi_data_edit)) & !grepl('type', names(hfi_data_edit))])) == 2, NA, base::rowSums(hfi_data_edit[ , grepl('dessert_b|dessert_d', names(hfi_data_edit)) & !grepl('type', names(hfi_data_edit))], na.rm = TRUE))
    
    # savory snacks
    hfi_score_dat[['savory_snacks']] <- ifelse(rowSums(is.na(hfi_data_edit[ , grepl('snacks', names(hfi_data_edit))])) == 18, NA, base::rowSums(hfi_data_edit[ , grepl('snacks', names(hfi_data_edit))], na.rm = TRUE))
    
    hfi_score_dat[['savory_snacks_lowfat']] <- ifelse(rowSums(is.na(hfi_data_edit[ , grepl('snacks_c|snacks_e|snacks_h|snacks_j|snacks_l|snacks_m|snacks_n|snacks_r', names(hfi_data_edit))])) == 8, NA, base::rowSums(hfi_data_edit[ , grepl('snacks_c|snacks_e|snacks_h|snacks_j|snacks_l|snacks_m|snacks_n|snacks_r', names(hfi_data_edit))], na.rm = TRUE))
    
    # bread
    hfi_score_dat[['bread']] <- ifelse(rowSums(is.na(hfi_data_edit[ , grepl('bread', names(hfi_data_edit)) & !grepl('type', names(hfi_data_edit))])) == 12, NA, base::rowSums(hfi_data_edit[ , grepl('bread', names(hfi_data_edit)) & !grepl('type', names(hfi_data_edit))], na.rm = TRUE))
    
    hfi_score_dat[['bread_wheat']] <- ifelse(rowSums(is.na(hfi_data_edit[ , grepl('bread_a|bread_c|bread_e|bread_g|bread_j', names(hfi_data_edit)) & !grepl('type', names(hfi_data_edit))])) == 5, NA, base::rowSums(hfi_data_edit[ , grepl('bread_a|bread_c|bread_e|bread_g|bread_j', names(hfi_data_edit)) & !grepl('type', names(hfi_data_edit))], na.rm = TRUE))
    
    # cereal
    hfi_data_edit[ , grepl('hfi_17|hfi_18|hfi_19', names(hfi_data_edit))] <- sapply(hfi_data_edit[ , grepl('hfi_17|hfi_18|hfi_19', names(hfi_data_edit))], function(x) ifelse(x > 0, 1, 0))
    
    hfi_score_dat[['cereal']] <- ifelse(rowSums(is.na(hfi_data_edit[ , grepl('hfi_17|hfi_18|hfi_19', names(hfi_data_edit))])) == 3, NA, base::rowSums(hfi_data_edit[ , grepl('hfi_17|hfi_18|hfi_19', names(hfi_data_edit))], na.rm = TRUE))
    
    hfi_score_dat[['cereal_highsugar']] <- hfi_data_edit['hfi_19']
    
    
    # candy

    #### 3. Clean Export/Scored Data #####
    ## round data
    if (isTRUE(ID_arg)){
        hfi_score_dat[2:ncol(hfi_score_dat)] <- round(hfi_score_dat[2:ncol(hfi_score_dat)], digits = 3)
    } else {
        hfi_score_dat <- round(hfi_score_dat, digits = 3)
    }

    ## merge raw responses with scored data
    if (isTRUE(ID_arg)){
      hfi_phenotype <- merge(hfi_data, hfi_score_dat, by = id)
      
      return(list(score_dat = as.data.frame(hfi_score_dat),
                  bids_phenotype = as.data.frame(hfi_phenotype)))
    } else {
      return(list(score_dat = as.data.frame(hfi_score_dat)))
    }

}

