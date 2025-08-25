#' score_hfi: Score data from the Fulkerson Home Food Inventory
#'
#' This function scores the Fulkerson Home Food Inventory and provides subscale scores for: 1) 13 categories of food and 2) accessibility categories for kitchen and fridge (each sub-divided into unhealthy and healthy), and 3) an obesogenic score
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'hfi_category_letter_type#' where 'letter' is the food item order and # type (where relevant)
#' 3) All questions must have the numeric value for the choice with the base value being either 0 (base_zero = TRUE) or 1 (base_zero = FALSE)
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Fulkerson JA, Nelson MC, Lytle LA, Moe S, Heitzler C, Pasch KE. The validation of a home food inventory. International Journal of Behavioral Nutrition and Physical Activity, 2008, 5;55 (\href{https://pubmed.ncbi.nlm.nih.gov/18983668/}{PubMed})
#'
#' @param hfi_data a data.frame all items for the Fulkerson Home Food Inventory following the naming conventions described above
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes

#' @return A dataset with subscale scores for the Fulkerson Home Food Inventory
#' @examples
#'
#' # scoring for the hfi with IDs
#' hfi_score_data <- score_hfi(hfi_data, id = 'ID')
#'
#'
#' @export

score_hfi <- function(hfi_data, pna_value, base_zero = TRUE, id, session_id, extra_scale_cols = c()) {

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
    
    # check if id exists
    sessionID_arg <- methods::hasArg(session_id)
    
    if (isTRUE(sessionID_arg)){
      if (!(session_id %in% names(hfi_data))) {
        stop('variable name entered as session_id is not in hfi_data')
      }
    }
    

    # check base_zero is logical
    if (!is.logical(base_zero)) {
      stop("base_zero arg must be logical (TRUE/FALSE)")
    }
    
    #### 2. Set Up Data #####

    # set up database for results create empty matrix
    hfi_score_dat <- data.frame(hfi_dairy = rep(NA, nrow(hfi_data)), hfi_dairy_highfat = rep(NA, nrow(hfi_data)), hfi_vegetables = rep(NA, nrow(hfi_data)), hfi_veg_nopotato = rep(NA, nrow(hfi_data)), hfi_fruit = rep(NA, nrow(hfi_data)), hfi_meat_protein = rep(NA, nrow(hfi_data)), hfi_meat_protein_processed = rep(NA, nrow(hfi_data)), hfi_added_fat = rep(NA, nrow(hfi_data)), hfi_added_fat_reg = rep(NA, nrow(hfi_data)), hfi_frozen_dessert = rep(NA, nrow(hfi_data)), hfi_frozen_dessert_highfat = rep(NA, nrow(hfi_data)), hfi_prepared_dessert = rep(NA, nrow(hfi_data)), hfi_prepared_dessert_highfat = rep(NA, nrow(hfi_data)), hfi_savory_snacks = rep(NA, nrow(hfi_data)), hfi_savory_snacks_highfat = rep(NA, nrow(hfi_data)), hfi_bread = rep(NA, nrow(hfi_data)), hfi_bread_wheat = rep(NA, nrow(hfi_data)), hfi_cereal = rep(NA, nrow(hfi_data)), hfi_cereal_highsugar = rep(NA, nrow(hfi_data)), hfi_candy = rep(NA, nrow(hfi_data)), hfi_beverages = rep(NA, nrow(hfi_data)), hfi_bev_highsugar = rep(NA, nrow(hfi_data)), hfi_microwave_quickfood = rep(NA, nrow(hfi_data)), hfi_fridge_accessible = rep(NA, nrow(hfi_data)), hfi_fridge_healthy = rep(NA, nrow(hfi_data)), hfi_fridge_unhealthy = rep(NA, nrow(hfi_data)), hfi_kitchen_accessible = rep(NA, nrow(hfi_data)), hfi_kitchen_healthy = rep(NA, nrow(hfi_data)), hfi_kitchen_unhealthy = rep(NA, nrow(hfi_data)), hfi_obesogenic_foods = rep(NA, nrow(hfi_data)))

    if (isTRUE(ID_arg)) {
      if (isTRUE(sessionID_arg)) {
        hfi_score_dat <- data.frame(hfi_data[[id]], hfi_data[[session_id]], hfi_score_dat)
        names(hfi_score_dat)[1:2] <- c(id, session_id)
      } else {
        hfi_score_dat <- data.frame(hfi_data[[id]], hfi_score_dat)
        names(hfi_score_dat)[1] <- id
      }
    }
    
    # assign hfi scale items to hfi_items, excluding columns in extra_scale_cols
    hfi_items <- setdiff(grep("^hfi", names(hfi_data), value = TRUE), extra_scale_cols)
    
    # if pna_value arg, replace not applicable values with NA
    if (isTRUE(methods::hasArg(pna_value))) {
      
      # replace pna_value with NA in pcw_vars
      hfi_data[hfi_items] <- lapply(hfi_data[hfi_items] , function(x) ifelse(x == pna_value, NA, x))
      
    }
    
    # subset hfi items for scoring
    hfi_data_edit <- hfi_data[c(hfi_items)]
    
    # re-scale data -- all columns in hfi_data_edit get rescaled 
    if (isFALSE(base_zero)){
      hfi_data_edit <- sapply(names(hfi_data_edit), function(x) hfi_data_edit[[x]] - 1, simplify = TRUE)
    }
    
    ## Score Subscales

    # diary
    hfi_score_dat[['hfi_dairy']] <- ifelse(rowSums(is.na(hfi_data_edit[grepl('dairy|cheese', names(hfi_data_edit))])) == 21, NA, base::rowSums(hfi_data_edit[grepl('dairy|cheese', names(hfi_data_edit))], na.rm = TRUE))
    
    hfi_score_dat[['hfi_dairy_highfat']] <- ifelse(rowSums(is.na(hfi_data_edit[grepl('cheese_a|cheese_b|cheese_g|cheese_i|cheese_k|dairy_c|dairy_d|dairy_e|dairy_i', names(hfi_data_edit))])) == 9, NA, base::rowSums(hfi_data_edit[grepl('cheese_a|cheese_b|cheese_g|cheese_i|cheese_k|dairy_c|dairy_d|dairy_e|dairy_i', names(hfi_data_edit))], na.rm = TRUE))

    # vegetables
    hfi_score_dat[['hfi_vegetables']] <- ifelse(rowSums(is.na(hfi_data_edit[grepl('veg', names(hfi_data_edit)) & !grepl('type', names(hfi_data_edit))])) == 20, NA, base::rowSums(hfi_data_edit[grepl('veg', names(hfi_data_edit)) & !grepl('type', names(hfi_data_edit))], na.rm = TRUE))
    
    hfi_score_dat[['hfi_veg_nopotato']] <- ifelse(rowSums(is.na(hfi_data_edit[grepl('veg', names(hfi_data_edit)) & !grepl('type', names(hfi_data_edit)) & !grepl('_o', names(hfi_data_edit))])) == 19, NA, base::rowSums(hfi_data_edit[grepl('veg', names(hfi_data_edit)) & !grepl('type', names(hfi_data_edit)) & !grepl('_o', names(hfi_data_edit))], na.rm = TRUE))

    # fruit
    hfi_score_dat[['hfi_fruit']] <- ifelse(rowSums(is.na(hfi_data_edit[grepl('fruit', names(hfi_data_edit)) & !grepl('type', names(hfi_data_edit))])) == 26, NA, base::rowSums(hfi_data_edit[grepl('veg', names(hfi_data_edit)) & !grepl('type', names(hfi_data_edit))], na.rm = TRUE))

    # meat_protein
    hfi_score_dat[['hfi_meat_protein']] <- ifelse(rowSums(is.na(hfi_data_edit[grepl('deli|protein', names(hfi_data_edit))])) == 16, NA, base::rowSums(hfi_data_edit[grepl('deli|protein', names(hfi_data_edit))], na.rm = TRUE))
    
    hfi_score_dat[['hfi_meat_protein_processed']] <- ifelse(rowSums(is.na(hfi_data_edit[grepl('deli_c|deli_d|deli_e|deli_f', names(hfi_data_edit))])) == 4, NA, base::rowSums(hfi_data_edit[grepl('deli_c|deli_d|deli_e|deli_f', names(hfi_data_edit))], na.rm = TRUE))
    
    # added fat
    hfi_score_dat[['hfi_added_fat']] <- ifelse(rowSums(is.na(hfi_data_edit[grepl('oils|dressing|cond_a|cond_b|cond_c', names(hfi_data_edit))])) == 13, NA, base::rowSums(hfi_data_edit[grepl('oils|dressing|cond_a|cond_b|cond_c', names(hfi_data_edit))], na.rm = TRUE))
    
    hfi_score_dat[['hfi_added_fat_reg']] <- ifelse(rowSums(is.na(hfi_data_edit[grepl('oils_a|oils_c|oils_e|oils_f|oils_g|oils_h|dressing_a|cond_a', names(hfi_data_edit))])) == 8, NA, base::rowSums(hfi_data_edit[grepl('oils_a|oils_c|oils_e|oils_f|oils_g|oils_h|dressing_a|cond_a', names(hfi_data_edit))], na.rm = TRUE))
    
    # frozen desert
    hfi_score_dat[['hfi_frozen_dessert']] <- ifelse(rowSums(is.na(hfi_data_edit[grepl('frozen', names(hfi_data_edit))])) == 7, NA, base::rowSums(hfi_data_edit[grepl('frozen', names(hfi_data_edit))], na.rm = TRUE))
    
    hfi_score_dat[['hfi_frozen_dessert_highfat']] <- ifelse(rowSums(is.na(hfi_data_edit[grepl('frozen_a|frozen_d|frozen_g', names(hfi_data_edit))])) == 3, NA, base::rowSums(hfi_data_edit[grepl('frozen_a|frozen_d|frozen_g', names(hfi_data_edit))], na.rm = TRUE))
    
    # prepared desert
    hfi_score_dat[['hfi_prepared_dessert']] <- ifelse(rowSums(is.na(hfi_data_edit[grepl('dessert', names(hfi_data_edit)) & !grepl('type', names(hfi_data_edit))])) == 8, NA, base::rowSums(hfi_data_edit[grepl('dessert', names(hfi_data_edit)) & !grepl('type', names(hfi_data_edit))], na.rm = TRUE))
    
    hfi_score_dat[['hfi_prepared_dessert_highfat']] <- ifelse(rowSums(is.na(hfi_data_edit[grepl('dessert_a|dessert_c|dessert_e|dessert_f|dessert_g|dessert_h', names(hfi_data_edit)) & !grepl('type', names(hfi_data_edit))])) == 6, NA, base::rowSums(hfi_data_edit[grepl('dessert_a|dessert_c|dessert_e|dessert_f|dessert_g|dessert_h', names(hfi_data_edit)) & !grepl('type', names(hfi_data_edit))], na.rm = TRUE))
    
    # savory snacks
    hfi_score_dat[['hfi_savory_snacks']] <- ifelse(rowSums(is.na(hfi_data_edit[grepl('snacks', names(hfi_data_edit))])) == 18, NA, base::rowSums(hfi_data_edit[grepl('snacks', names(hfi_data_edit))], na.rm = TRUE))
    
    hfi_score_dat[['hfi_savory_snacks_highfat']] <- ifelse(rowSums(is.na(hfi_data_edit[grepl('snacks_a|snacks_b|snacks_d|snacks_f|snacks_g|snacks_i|snacks_k|snacks_o|snacks_p|snacks_q', names(hfi_data_edit))])) == 10, NA, base::rowSums(hfi_data_edit[grepl('snacks_a|snacks_b|snacks_d|snacks_f|snacks_g|snacks_i|snacks_k|snacks_o|snacks_p|snacks_q', names(hfi_data_edit))], na.rm = TRUE))
    
    # bread
    hfi_score_dat[['hfi_bread']] <- ifelse(rowSums(is.na(hfi_data_edit[grepl('bread', names(hfi_data_edit)) & !grepl('type', names(hfi_data_edit))])) == 12, NA, base::rowSums(hfi_data_edit[grepl('bread', names(hfi_data_edit)) & !grepl('type', names(hfi_data_edit))], na.rm = TRUE))
    
    hfi_score_dat[['hfi_bread_wheat']] <- ifelse(rowSums(is.na(hfi_data_edit[grepl('bread_a|bread_c|bread_e|bread_g|bread_j', names(hfi_data_edit)) & !grepl('type', names(hfi_data_edit))])) == 5, NA, base::rowSums(hfi_data_edit[grepl('bread_a|bread_c|bread_e|bread_g|bread_j', names(hfi_data_edit)) & !grepl('type', names(hfi_data_edit))], na.rm = TRUE))
    
    # cereal
    hfi_data_edit[grepl('hfi_17|hfi_18|hfi_19', names(hfi_data_edit))] <- sapply(hfi_data_edit[grepl('hfi_17|hfi_18|hfi_19', names(hfi_data_edit))], function(x) ifelse(x > 0, 1, 0))
    
    hfi_score_dat[['hfi_cereal']] <- ifelse(rowSums(is.na(hfi_data_edit[grepl('hfi_17|hfi_18|hfi_19', names(hfi_data_edit))])) == 3, NA, base::rowSums(hfi_data_edit[grepl('hfi_17|hfi_18|hfi_19', names(hfi_data_edit))], na.rm = TRUE))
    
    hfi_score_dat[['hfi_cereal_highsugar']] <- hfi_data_edit[['hfi_19']]
    
    
    # candy
    hfi_score_dat[['hfi_candy']] <- ifelse(rowSums(is.na(hfi_data_edit[grepl('candy', names(hfi_data_edit))])) == 5, NA, base::rowSums(hfi_data_edit[grepl('candy', names(hfi_data_edit))], na.rm = TRUE))
    
    # beverages
    hfi_score_dat[['hfi_beverages']] <- ifelse(rowSums(is.na(hfi_data_edit[grepl('bev', names(hfi_data_edit))])) == 9, NA, base::rowSums(hfi_data_edit[grepl('bev', names(hfi_data_edit))], na.rm = TRUE))
    
    hfi_score_dat[['hfi_bev_highsugar']] <- ifelse(rowSums(is.na(hfi_data_edit[grepl('bev_a|bev_c|bev_e|bev_f|bev_g|bev_i', names(hfi_data_edit))])) == 6, NA, base::rowSums(hfi_data_edit[grepl('bev_a|bev_c|bev_e|bev_f|bev_g|bev_i', names(hfi_data_edit))], na.rm = TRUE))
    
    # Microwave/Quick foods
    hfi_score_dat[['hfi_microwave_quickfood']] <- ifelse(rowSums(is.na(hfi_data_edit[grepl('quick', names(hfi_data_edit))])) == 8, NA, base::rowSums(hfi_data_edit[grepl('quick', names(hfi_data_edit))], na.rm = TRUE))
    
    # fridge accessible
    hfi_score_dat[['hfi_fridge_accessible']] <- ifelse(rowSums(is.na(hfi_data_edit[grepl('fridge', names(hfi_data_edit))])) == 15, NA, base::rowSums(hfi_data_edit[grepl('fridge', names(hfi_data_edit))], na.rm = TRUE))
    
    hfi_score_dat[['hfi_fridge_healthy']] <- ifelse(rowSums(is.na(hfi_data_edit[grepl('fridge_a|fridge_b|fridge_g|fridge_h|fridge_j|fridge_k|fridge_m|fridge_n|fridge_o', names(hfi_data_edit))])) == 9, NA, base::rowSums(hfi_data_edit[grepl('fridge_a|fridge_b|fridge_g|fridge_h|fridge_j|fridge_k|fridge_m|fridge_n|fridge_o', names(hfi_data_edit))], na.rm = TRUE))
    
    hfi_score_dat[['hfi_fridge_unhealthy']] <- ifelse(rowSums(is.na(hfi_data_edit[grepl('fridge_c|fridge_d|fridge_e|fridge_f|fridge_i|fridge_l', names(hfi_data_edit))])) == 6, NA, base::rowSums(hfi_data_edit[grepl('fridge_c|fridge_d|fridge_e|fridge_f|fridge_i|fridge_l', names(hfi_data_edit))], na.rm = TRUE))
    
    # kitchen accessible
    hfi_score_dat[['hfi_kitchen_accessible']] <- ifelse(rowSums(is.na(hfi_data_edit[grepl('accessible', names(hfi_data_edit)) & !grepl('fridge', names(hfi_data_edit))])) == 12, NA, base::rowSums(hfi_data_edit[grepl('accessible', names(hfi_data_edit)) & !grepl('fridge', names(hfi_data_edit))], na.rm = TRUE))
    
    hfi_score_dat[['hfi_kitchen_healthy']] <- ifelse(rowSums(is.na(hfi_data_edit[grepl('accessible_a|accessible_b|accessible_c|accessible_e|accessible_i|accessible_l', names(hfi_data_edit))])) == 6, NA, base::rowSums(hfi_data_edit[grepl('accessible_a|accessible_b|accessible_c|accessible_e|accessible_i|accessible_l', names(hfi_data_edit))], na.rm = TRUE))
    
    hfi_score_dat[['hfi_kitchen_unhealthy']] <- ifelse(rowSums(is.na(hfi_data_edit[grepl('accessible_d|accessible_f$|accessible_g|accessible_h|accessible_j|accessible_k', names(hfi_data_edit))])) == 6, NA, base::rowSums(hfi_data_edit[grepl('accessible_d|accessible_f$|accessible_g|accessible_h|accessible_j|accessible_k', names(hfi_data_edit))], na.rm = TRUE)) # use $ after accessible_f to avoid accessible_fridge from being included
    
    # obesogenic foods
    hfi_score_dat[['hfi_obesogenic_foods']] <- ifelse(rowSums(is.na(hfi_score_dat[grepl('hfi_dairy_highfat|hfi_meat_protein_processed|hfi_added_fat_reg|hfi_frozen_dessert_highfat|hfi_prepared_dessert_highfat|hfi_savory_snacks_highfat|hfi_candy|hfi_bev_highsugar|hfi_microwave_quickfood|hfi_fridge_unhealthy|hfi_kitchen_unhealthy', names(hfi_score_dat))])) == 11, NA, base::rowSums(hfi_score_dat[grepl('hfi_dairy_highfat|hfi_meat_protein_processed|hfi_added_fat_reg|hfi_frozen_dessert_highfat|hfi_prepared_dessert_highfat|hfi_savory_snacks_highfat|hfi_candy|hfi_bev_highsugar|hfi_microwave_quickfood|hfi_fridge_unhealthy|hfi_kitchen_unhealthy', names(hfi_score_dat))], na.rm = TRUE))
    
    #### 3. Clean Export/Scored Data #####
    ## merge raw responses with scored data
    if (isTRUE(ID_arg)){
      if (isTRUE(sessionID_arg)){
        hfi_phenotype <- merge(hfi_data, hfi_score_dat, by = c(id, session_id))
      } else {
        hfi_phenotype <- merge(hfi_data, hfi_score_dat, by = id)
      }
      
      return(list(score_dat = as.data.frame(hfi_score_dat),
                  bids_phenotype = as.data.frame(hfi_phenotype)))
    } else {
      return(list(score_dat = as.data.frame(hfi_score_dat)))
    }

}

