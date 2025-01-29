#' score_nik_hfe: Scored data from the Home Food Environment Survey from the Neighborhood Impact on Kids Study
#'
#' This function scores the Home Food Environment Survey from the Neighborhood Impact on Kids Study and provides subscale scores for many different sections related to parent and child behaviors and neighborhood environment
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'hfe#' or 'hfe_#' where # is the question number
#' 3) All questions must have the numeric value for the choice: 0 - Never True, 1 - Rarely, 2 - Sometimes, 3 - Often, 4 - Always
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' B1. Couch SC, Glanz K, Zhou C, Sallis JF, Saelens BE. Home Food Environment in Relation to Children’s Diet Quality and Weight Status. Journal of the Academy of Nutrition and Dietetics. 2014;114(10):1569-1579.e1. doi:10.1016/j.jand.2014.05.015 (\href{https://pubmed.ncbi.nlm.nih.gov/25066057/}{PubMed})
#'
#' @param hfe_data a data.frame all items for the Home Food Environment Survey from the Neighborhood Impact on Kids Study following the naming conventions described above
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @param extra_scale_cols a vector of character strings that begin with 'hfe' but are not scale items. Any columns in hfe_data that begin with 'hfe' but are not scale items must be included here. Default is empty vector.
#' 
#'
#' @return A dataset with subscale scores for the Home Food Environment Survey from the Neighborhood Impact on Kids Study
#' @examples
#'
#' # scoring for the hfe with IDs
#' hfe_score_data <- score_hfe(hfe_data, id = 'ID', base_zero = TRUE)
#'
#' \dontrun{
#' }
#'
#'
#' @export

score_nik_hfe <- function(hfe_data, base_zero = TRUE, id, session_id, extra_scale_cols = c()) {

    #### 1. Set up/initial checks #####

    # check that hfe_data exist and is a data.frame
    data_arg <- methods::hasArg(hfe_data)

    if (isTRUE(data_arg) & !is.data.frame(hfe_data)) {
        stop('hfe_data must be entered as a data.frame')
    } else if (isFALSE(data_arg)) {
        stop('hfe_data must set to a data.frame')
    }

    # check if id exists
    ID_arg <- methods::hasArg(id)

    if (isTRUE(ID_arg)){
        if (!(id %in% names(hfe_data))) {
            stop('variable name entered as id is not in hfe_data')
        }
    }
    
    # check if session_id exists
    sessionID_arg <- methods::hasArg(session_id)
    
    if (isTRUE(sessionID_arg)){
      if (!(id %in% names(hfe_data))) {
        stop("variable name entered as session_id is not in hfe_data")
      }
    }

    # check base_zero is logical
    if (!is.logical(base_zero)) {
      stop("base_zero arg must be logical (TRUE/FALSE)")
    }
    
    #### 2. Set Up Data #####

    # set up database for results create empty matrix
    hfe_score_dat <- data.frame(hfe_modeling = rep(NA, nrow(hfe_data)), hfe_restrictive_practices = rep(NA, nrow(hfe_data)), hfe_permissive_practices = rep(NA, nrow(hfe_data)), hfe_pressure_eat = rep(NA, nrow(hfe_data)), hfe_food_rules = rep(NA, nrow(hfe_data)), hfe_highED_homefood = rep(NA, nrow(hfe_data)), hfe_lowED_homefood = rep(NA, nrow(hfe_data)), hfe_highED_childlike = rep(NA, nrow(hfe_data)), hfe_lowED_childlike = rep(NA, nrow(hfe_data)), hfe_eat_away = rep(NA, nrow(hfe_data)), hfe_eat_school = rep(NA, nrow(hfe_data)), hfe_eatout = rep(NA, nrow(hfe_data)), hfe_nbh_grocery_available = rep(NA, nrow(hfe_data)), hfe_nbh_grocery_cost = rep(NA, nrow(hfe_data)), hfe_p_healthyeating_conf = rep(NA, nrow(hfe_data)), hfe_p_healthyeating_barriers = rep(NA, nrow(hfe_data)), hfe_fam_pos_influence = rep(NA, nrow(hfe_data)), hfe_fam_neg_influence = rep(NA, nrow(hfe_data)))

    if (isTRUE(ID_arg)) {
      if (isTRUE(sessionID_arg)) {
        hfe_score_dat <- data.frame(hfe_data[[id]], hfe_data[[session_id]], hfe_score_dat)
        names(hfe_score_dat)[1:2] <- c(id, session_id)
      } else {
        hfe_score_dat <- data.frame(hfe_data[[id]], hfe_score_dat)
        names(hfe_score_dat)[1] <- id
      }
    }

    # remove underscore if in column names
    names(hfe_data) <- ifelse(grepl('fam', names(hfe_data)), gsub('fam_', 'fam', names(hfe_data)), ifelse(grepl('track', names(hfe_data)), gsub('track_', 'track', names(hfe_data)), ifelse(grepl('rules', names(hfe_data)), gsub('rules_', 'rules', names(hfe_data)), ifelse(grepl('available', names(hfe_data)), gsub('available_', 'avail', names(hfe_data)), ifelse(grepl('like', names(hfe_data)), gsub('like_', 'like', names(hfe_data)), ifelse(grepl('shop_', names(hfe_data)), gsub('shop_', 'shop', names(hfe_data)), ifelse(grepl('shoploc_', names(hfe_data)), gsub('shoploc_', 'shoploc', names(hfe_data)), ifelse(grepl('eatout',  names(hfe_data)), gsub('eatout_', 'eatout', names(hfe_data)), ifelse(grepl('neighborhood',  names(hfe_data)), gsub('neighborhood_', 'nbh', names(hfe_data)), ifelse(grepl('p1', names(hfe_data)), gsub('p1', 'shop', names(hfe_data)), ifelse(grepl('p2', names(hfe_data)), gsub('p2', 'p_lunch', names(hfe_data)), ifelse(grepl('p3', names(hfe_data)),  gsub('p3', 'eatout', names(hfe_data)), ifelse(grepl('p4', names(hfe_data)), gsub('p4_', 'p4', names(hfe_data)), ifelse(grepl('p5', names(hfe_data)), gsub('p5_', 'p5', names(hfe_data)), ifelse(grepl('p6', names(hfe_data)), gsub('p6_', 'p6', names(hfe_data)), names(hfe_data))))))))))))))))
    
    names(hfe_data) <- ifelse(grepl('_a$', names(hfe_data)), gsub('_a', '_taste', names(hfe_data)), ifelse(grepl('_b$', names(hfe_data)), gsub('_b', '_nutr', names(hfe_data)), ifelse(grepl('_c$', names(hfe_data)), gsub('_c', '_cost', names(hfe_data)), ifelse(grepl('_d$', names(hfe_data)), gsub('_d', '_convenience', names(hfe_data)), ifelse(grepl('_e$', names(hfe_data)), gsub('_e', '_weightcontrol', names(hfe_data)), names(hfe_data))))))
    
    # get primary questions
    hfe_primary_qs <- grep("^hfe", names(hfe_data), value = TRUE) %>% setdiff(extra_scale_cols)
    
    # re-scale data
    hfe_data_edit <- hfe_data
    
    if (isTRUE(base_zero)){
      hfe_data_edit[hfe_primary_qs[!grepl('hfe_shoploc5', hfe_primary_qs)]] <- sapply(hfe_primary_qs[!grepl('hfe_shoploc5', hfe_primary_qs)], function(x) hfe_data[[x]] + 1, simplify = TRUE)
    }

  
    ## Score Subscales

    # Modeling
    model_vars <- c('hfe_fam1', 'hfe_fam2', 'hfe_fam3', 'hfe_fam4', 'hfe_fam5', 'hfe_fam6', 'hfe_fam7')
    hfe_score_dat[['hfe_modeling']] <- rowMeans(hfe_data_edit[model_vars])

    # Restrictive practices
    restrict_vars <- c('hfe_track1', 'hfe_track2')
    hfe_score_dat[['hfe_restrictive_practices']] <- rowMeans(hfe_data_edit[restrict_vars])

    # Permissive practices
    permissive_vars <- c('hfe_fam10', 'hfe_fam11', 'hfe_fam12')
    hfe_score_dat[['hfe_permissive_practices']] <- rowMeans(hfe_data_edit[permissive_vars])

    # Pressure to eat
    pressure_vars <- c('hfe_track3', 'hfe_track4', 'hfe_track5', 'hfe_track6')
    hfe_score_dat[['hfe_pressure_eat']] <- rowMeans(hfe_data_edit[pressure_vars])
    
    # Food Rules
    hfe_score_dat[['hfe_food_rules']] <- rowSums(hfe_data_edit[grepl('rules', names(hfe_data_edit))])
    
    # High ED home food
    avail_hED_vars <- c('hfe_avail1', 'hfe_avail2', 'hfe_avail4', 'hfe_avail5', 'hfe_avail9', 'hfe_avail10', 'hfe_avail12', 'hfe_avail14', 'hfe_avail16')
    hfe_score_dat[['hfe_highED_homefood']] <- rowMeans(hfe_data_edit[avail_hED_vars])
    
    # Low ED home food
    avail_lED_vars <- c('hfe_avail3', 'hfe_avail6', 'hfe_avail7', 'hfe_avail8', 'hfe_avail11', 'hfe_avail13', 'hfe_avail15', 'hfe_avail17')
    hfe_score_dat[['hfe_lowED_homefood']] <- rowMeans(hfe_data_edit[avail_lED_vars])
    
    # High ED child like
    like_hED_vars <- c('hfe_like6', 'hfe_like7')
    hfe_score_dat[['hfe_highED_childlike']] <- rowMeans(hfe_data_edit[like_hED_vars])
    
    # Low ED child like
    like_lED_vars <- c('hfe_like1', 'hfe_like2', 'hfe_like3', 'hfe_like4', 'hfe_like5')
    hfe_score_dat[['hfe_lowED_childlike']] <- rowMeans(hfe_data_edit[like_lED_vars])
    
    # Eat out of home
    eat_away_vars <- c('hfe_eatout1', 'hfe_eatout2', 'hfe_eatout3', 'hfe_eatout4')
    hfe_score_dat[['hfe_eat_away']] <- rowMeans(hfe_data_edit[eat_away_vars])
    
    # Eat at school
    hfe_score_dat[['hfe_eat_school']] <- as.numeric(hfe_data_edit[['hfe_eatout6']])
    
    # Eat out
    eatout_vars <- c('hfe_eatout5', 'hfe_eatout7', 'hfe_eatout8', 'hfe_eatout9', 'hfe_eatout10', 'hfe_eatout11')
    hfe_score_dat[['hfe_eatout']] <- rowMeans(hfe_data_edit[eatout_vars])
    
    # Availability of healthy foods at neighborhood grocery stores
    nbh_avail_vars <- c('hfe_nbh1', 'hfe_nbh2', 'hfe_nbh3')
    hfe_score_dat[['hfe_nbh_grocery_available']] <- rowMeans(hfe_data_edit[nbh_avail_vars])
    
    # Neighborhood grocery store cost of healthy food
    nbh_cost_vars <- c('hfe_nbh4', 'hfe_nbh5')
    hfe_score_dat[['hfe_nbh_grocery_cost']] <- rowMeans(hfe_data_edit[nbh_cost_vars])
    
    # Parent confidence in following healthy eating patterns
    hfe_score_dat[['hfe_p_healthyeating_conf']] <- rowMeans(hfe_data_edit[grepl('p4', names(hfe_data_edit))])
    
    # Parent influence of barriers to healthy eating
    hfe_score_dat[['hfe_p_healthyeating_barriers']] <- rowMeans(hfe_data_edit[grepl('p5', names(hfe_data_edit))])
    
    # Family members positive influence on healthy eating
    fam_pos_vars <- c('hfe_p6a', 'hfe_p6c', 'hfe_p6e', 'hfe_p6g')
    hfe_score_dat[['hfe_fam_pos_influence']] <- rowMeans(hfe_data_edit[fam_pos_vars])
    
    # Family members negative influence on healthy eating
    fam_neg_vars <- c('hfe_p6b', 'hfe_p6d', 'hfe_p6f')
    hfe_score_dat[['hfe_fam_neg_influence']] <- rowMeans(hfe_data_edit[fam_neg_vars])
    

    #### 3. Clean Export/Scored Data #####
    ## merge raw responses with scored data
    if (isTRUE(ID_arg)){
      if (isTRUE(sessionID_arg)) {
        hfe_phenotype <- merge(hfe_data, hfe_score_dat, by = c(id, session_id))
      } else {
        hfe_phenotype <- merge(hfe_data, hfe_score_dat, by = id)
      }
      
      return(list(score_dat = as.data.frame(hfe_score_dat),
                  bids_phenotype = as.data.frame(hfe_phenotype)))
    } else {
      return(list(score_dat = as.data.frame(hfe_score_dat)))
    }
}

