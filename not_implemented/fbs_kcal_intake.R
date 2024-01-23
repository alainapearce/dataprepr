#' fbs_kcal_intake: Calculate the amount served and eaten in kcal for the Food and Brain Study
#'
#' This function calculates the amount served and eaten in kcal for the Food and Brain Study using food energy density and measured gram weights. This is done for all meals included in the study: standard meal + eating in the absence of hunger (EAH) (visits 1 and 7) and the portion size meals (visits 2-5). The kcal consumed will be calculated for each food item and total grams and kcal consumed will be calculated IF all food items for a meal were included in the dataset for a participant. If a participant has missing data, total consumed will not be calculated for them.
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include the amount consumed for each food item individually
#' 2) The amount consumed columns/variables must match the following naming conventions for each food item - 'consumed_food_g' where 'food' is replaced with each food item
#' 3) The following foods must be included for each meal
#' 3a) standard meal: 'applesauce', 'carrot', 'cheese_sndwch', 'cookies', 'ham_sndwch, 'milk', 'pbj_sndwch', 'potatochip', 'turkey_sndwch', 'ketchup', 'mayo', 'mustard',
#' 3b) EAH: 'brownies', 'cornchips', 'hersheys', icecream', 'oreos', 'popcorn', 'pretzels', 'skittles', 'starbursts', 'water'
#' 3c) portion size meals: 'chkn_nug', 'mac_cheese', 'grapes', broccoli', ketchup', water'
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#'
#' @param intake_data a data.frame including grams consumed for each food item following the naming conventions described above
#' @param meal string indicating which meals are included in intake_data: 'std_meal' - standard meal before EAH (visits 1 and 7); 'EAH' - eating in the absence of hunger paradigm (visits 1 and 7); and 'ps_meal' - portion size meal (visits 2-5)
#' @param parID (optional) name of participant ID column in intake_data. If included the output dataset will be matched by parID, if not included the output dataset will be in the order of intake_data but will have no participant identifier.
#'
#' @return The intake_data data.frame with kcal consumed added for each food item and total consumed in grams and kcal.
#'
#' @examples
#' # intake for standard meal
#' fbs_kcal_data <- fbs_kcal_intake(intake_data, meal = 'std_meal', parID = 'ID')
#'
#' # intake for eah
#' fbs_kcal_data <- fbs_kcal_intake(intake_data, meal = 'EAH', parID = 'ID')
#'
#' \dontrun{
#' # no meal specified
#' fbs_kcal_data <- fbs_kcal_intake(intake_data, parID = 'ID')
#' }
#'
#' @seealso For the Food and Brain Study, the energy density of each food is in \link{fbs_ED.rda}.
#' Raw data from Qualtrics was processed using the following scripts (or there 'lab' equivalent):
#'  \code{\link{qualtrics_child_v1dat}}, \code{\link{qualtrics_child_v2dat}}, \code{\link{qualtrics_child_v3dat}}, \code{\link{qualtrics_child_v4dat}}, \code{\link{qualtrics_child_v5dat}}, \code{\link{qualtrics_child_v7dat}}
#'
#'
#'
#' @export

fbs_kcal_intake <- function(intake_data, meal, parID) {

    #### 1. Set up/initial checks #####

    # check that intake_data exist and is a data.frame
    data_arg <- methods::hasArg(intake_data)

    if (isTRUE(data_arg) & !is.data.frame(intake_data)) {
        stop("intake_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("intake_data must set to the data.frame with amount consumed for each food item")
    }

    # check that meal exist and is a string
    meal_arg <- methods::hasArg(meal)

    if (isTRUE(meal_arg)){
        if (!is.character(meal)) {
            stop("meal must be entered as a string and can either be: 'std_meal', 'EAH', or 'ps_meal'")
        } else if (meal != 'std_meal' & meal != 'EAH' & meal != 'ps_meal') {
            stop("the optional values for meal are: 'std_meal', 'EAH', or 'ps_meal'")
        }
    } else if (isFALSE(meal_arg)) {
        stop("meal must be entered as a string and can be: 'std_meal', 'EAH', or 'ps_meal'")
    }

    # check if parID exists
    ID_arg <- methods::hasArg(parID)

    if (!(parID %in% names(intake_data))) {
        stop("variable name entered as parID is not in intake_data")
    }

    #### 2. Set Up Data #####

    #set up labels for pds_score_dat
    intake_dat_labels <- lapply(intake_data, function(x) attributes(x)$label)

    # check variables in intake_data based on meal

    #get subset of ED values based on entered meal - references package included dataset fbs_ED
    meal_ed <- fbs_ED[fbs_ED$meal == meal, ]

    #loop through meal items in ED dataset to make names that should match variables in intake_data
    meal_ed_foods <- sapply(meal_ed$food, function(x) paste0('consumed_', x, '_g'), USE.NAMES = FALSE)

    #check that the variables names in intake_data match the expected names from fbs_ED data
    n_match <- sum(meal_ed_foods %in% names(intake_data))

    if (n_match == 0){
        stop(paste0("none of the variable names in intake_data match the required variable name for the entered meal. For entered meal ", meal, ", the following variables are needed: ", paste0(as.character(meal_ed_foods), collapse = ', ')))
    } else if (n_match > 0){
        for (f in 1:length(meal_ed_foods)){
            #name of grams consumed variable
            g_var_name <- meal_ed_foods[f]

            #if variable in intake_data
            if (g_var_name %in% names(intake_data)){
                #get food item name
                food_item <- gsub(".*consumed_(.+)_g.*", "\\1", meal_ed_foods[f])

                #make kcal consumed variable name
                kcal_var_name <- paste0('consumed_', food_item, '_kcal')

                #get food ED
                food_ED <- meal_ed[meal_ed$food == food_item, 'ED']

                #calculate kcal consumed (g*ED)
                intake_data[[kcal_var_name]] <- intake_data[[g_var_name]]*food_ED

                #add label
                intake_dat_labels[[kcal_var_name]] <- paste0(meal, ' ', food_item, ': Amount Consumed kcal - calculated in R')

                #add amounts to total consumed
                if (!('consumed_total_g' %in% names(intake_data))){
                    #if don't already have a total consumed variable, add and set to value
                    intake_data[['consumed_total_g']] <- intake_data[[g_var_name]]
                    intake_data[['consumed_total_kcal']] <- intake_data[[kcal_var_name]]
                } else {
                    #add to existing total consumed
                    intake_data[['consumed_total_g']] <- intake_data[['consumed_total_g']] + intake_data[[g_var_name]]
                    intake_dat_labels[['consumed_total_g']] <- paste0(meal, ' ', 'Total: Amount Consumed g - calculated in R. Set to NA if not all foods had available data')

                    intake_data[['consumed_total_kcal']] <- intake_data[['consumed_total_kcal']] + intake_data[[kcal_var_name]]
                    intake_dat_labels[['consumed_total_kcal']] <- paste0(meal, ' ', 'Total: Amount Consumed kcal - calculated in R. Set to NA if not all foods had available data')
                }
            }
        }
    }

    #check intake_data does not if have all needed foods, set total consumed tp NA
    if(n_match != length(meal_ed_foods)){
        intake_data[['consumed_total_g']] <- NA
        intake_data[['consumed_total_kcal']] <- NA
    } else {
        #get variables in correct format
        meal_vars <- sapply(meal_ed_foods, paste0, collapse="")
        #check if each individual has all needed foods
        for (p in 1:nrow(intake_data)){
            n_na <- sum(is.na(intake_data[p, meal_vars]))
            if (n_na > 0){
                intake_data[[p, 'consumed_total_g']] <- NA
                intake_data[[p, 'consumed_total_kcal']] <- NA
            }
        }
    }

    #### 3. Clean Export/Scored Data #####
    if (meal == 'std_meal'){
        meal_vars <- c('consumed_applesauce_kcal', 'consumed_carrot_kcal', 'consumed_cheese_sndwch_kcal', 'consumed_cookies_kcal', 'consumed_ham_sndwch_kcal', 'consumed_milk_kcal', 'consumed_pbj_sndwch_kcal', 'consumed_potatochip_kcal', 'consumed_turkey_sndwch_kcal', 'consumed_ketchup_kcal', 'consumed_mayo_kcal', 'consumed_mustard_kcal', 'consumed_total_g', 'consumed_total_kcal')

    } else if (meal == 'EAH'){
        meal_vars <- c('consumed_brownies_kcal', 'consumed_cornchips_kcal', 'consumed_hersheys_kcal', 'consumed_icecream_kcal', 'consumed_oreos_kcal', 'consumed_popcorn_kcal', 'consumed_pretzels_kcal', 'consumed_skittles_kcal', 'consumed_starbursts_kcal', 'consumed_total_g', 'consumed_total_kcal')

    } else if (meal == 'ps_meal'){
        meal_vars <- c('consumed_chkn_nug_kcal', 'consumed_mac_cheese_kcal', 'consumed_grapes_kcal', 'consumed_broccoli_kcal', 'consumed_ketchup_kcal', 'consumed_total_g', 'consumed_total_kcal')
    }

    intake_data_final <- intake_data[c('id', meal_vars)]
    intake_dat_labels_final <- intake_dat_labels[c('id', meal_vars)]
    intake_dat_labels_final['id'] <- 'Participant ID'

    ## make sure the variable labels match in the dataset
    intake_data = sjlabelled::set_label(intake_data_final, label = matrix(unlist(intake_dat_labels_final, use.names = FALSE)))

    return(intake_data)
}
