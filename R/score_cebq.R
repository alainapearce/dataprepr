#' score_cebq: Scored data from the Children's Eating Behavior Questionnaire
#'
#' This function scores the Children's Eating Behavior Questionnaire and provides subscale scores for the following behaviors: Food Responsiveness, Emotional Overeating, Enjoyment of Food, Desire to Drink, Satiety Responsiveness, Slowness in Eating, Emotional Undereating, and Food Fussiness.
#' 
#' Recent analyses by Manzano et al., (2021) indicate a better-fitting 3 factor solution for school-age children, therefore, the following subscales are also reported: reward-based eating, picky eating, and emotional eating
#'
#' For data to be scored correctly, the data must be prepared according to the following criteria: \cr
#' \itemize{
#'  \item{The data must include all individual questionnaire items}
#'  \item{The columns/variables must match the following naming convention: 'cebq#' or 'cebq_#' where # is the question number (1-35)}
#'  \item{All questionnaire responses must be a numeric value ranging from 0-4 (base_zero = TRUE) or 1-5 (base_zero = FALSE) where: }
#'  \itemize{
#'     \item{For base_zero = TRUE: 0 = Never; 1 = Rarely; 2 = Sometimes; 3 = Often; 4 = Always}
#'     \item{For base_zero = FALSE: 1 = Never; 2 = Rarely; 3 = Sometimes; 4 = Often; 5 = Always}
#'   }
#'  \item{Missing values must be coded as NA}
#'  \item{Values must not be reversed scored. This script will apply reverse scoring so all levels must be true to the scale described above}
#' }
#' \cr
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Primary References for the Children's Eating Behavior Questionnaire and Scoring:
#' Wardle, J., Guthrie, C. A., Sanderson, S., & Rapoport, L. (2001). Development of the children’s eating behaviour questionnaire. Journal of Child Psychology and Psychiatry, 42, 963–970. https://doi.org/10.1017/S0021963001007727 (\href{https://pubmed.ncbi.nlm.nih.gov/11693591/}{PubMed})
#' 
#' Alternative 3-factor scoring:
#' Manzano MA, Strong DR, Kang Sim DE, Rhee KE, Boutelle KN. Psychometric properties of the Child Eating Behavior Questionnaire (CEBQ) in school age children with overweight and obesity: A proposed three‐factor structure. Pediatric Obesity. 2021;16(10):e12795. doi:10.1111/ijpo.12795 (\href{https://pubmed.ncbi.nlm.nih.gov/33945226/}{PubMed})'
#'
#' @param cebq_data a data.frame all items for the Children's Eating Behavior Questionnaire following the naming conventions described in Details
#' @inheritParams score_bes
#' @param extra_scale_cols a vector of character strings that begin with 'cebq' but are not scale items. Any columns in cebq_data that begin with 'cebq' but are not scale items must be included here. Default is empty vector.
#'
#' @return If 'id' argument is used, returns a list with 2 dataframes: (1) bids_phenotype (contains input cebq_data [values identical to input, underscores removed from cebq items col names, if they existed] and cebq subscale scores) and (2) score_dat (contains cebq subscale scores only). If 'id' argument is not used, returns a list with score_dat dataframe only.
#' @export
#' @examples
#'
#' # scoring for the CEBQ with IDs, when values range from 0-4
#' data(cebq_base0) # load example data included with dataprepr
#' cebq_score_data <- score_cebq(cebq_data = cebq_base0, base_zero = TRUE, id = 'ID')
#' 
#' # scoring for the CEBQ with IDs, when values range from 1-5
#' data(cebq_base1) # load example data included with dataprepr
#' cebq_score_data <- score_cebq(cebq_data = cebq_base1, base_zero = FALSE, id = 'ID')
#' 

score_cebq <- function(cebq_data, base_zero = TRUE, id, extra_scale_cols = c()) {

    #### 1. Set up/initial checks #####

    # check that cebq_data exist and is a data.frame
    data_arg <- methods::hasArg(cebq_data)

    if (isTRUE(data_arg) & !is.data.frame(cebq_data)) {
        stop('cebq_data must be entered as a data.frame')
    } else if (isFALSE(data_arg)) {
        stop('cebq_data must set to the data.frame with amount consumed for each food item')
    }

    # check if id exists
    ID_arg <- methods::hasArg(id)

    if (isTRUE(ID_arg)){
        if (!(id %in% names(cebq_data))) {
            stop('variable name entered as id is not in cebq_data')
        }
    }

    # check base_zero is logical
    if (!is.logical(base_zero)) {
      stop("base_zero arg must be logical (TRUE/FALSE)")
    }
    
    #### 2. Set Up Data #####

    # set up database for results create empty matrix
    cebq_score_dat <- data.frame(cebq_fr = rep(NA, nrow(cebq_data)), cebq_eoe = rep(NA, nrow(cebq_data)), cebq_ef = rep(NA, nrow(cebq_data)), cebq_dd = rep(NA, nrow(cebq_data)), cebq_sr = rep(NA, nrow(cebq_data)), cebq_se = rep(NA, nrow(cebq_data)), cebq_eue = rep(NA, nrow(cebq_data)), cebq_ff = rep(NA, nrow(cebq_data)), cebq_approach = rep(NA, nrow(cebq_data)), cebq_avoid = rep(NA, nrow(cebq_data)), cebq_rbe = rep(NA, nrow(cebq_data)), cebq_pe = rep(NA, nrow(cebq_data)), cebq_ee = rep(NA, nrow(cebq_data)))

    if (isTRUE(ID_arg)) {
        cebq_score_dat <- data.frame(cebq_data[[id]], cebq_score_dat)
        names(cebq_score_dat)[1] <- id
    }
    
    # assign cebq scale items to cebq_items, excluding columns in extra_scale_cols
    cebq_items <- grep("^cebq", names(cebq_data), value = TRUE) %>% setdiff(extra_scale_cols)
    
    # remove underscore in column names for cebq_items
    names(cebq_data)[names(cebq_data) %in% cebq_items] <- gsub('cebq_', 'cebq', names(cebq_data)[names(cebq_data) %in% cebq_items])
    
    # remove underscore in cebq_items
    cebq_items <- gsub("cebq_", "cebq", cebq_items)
    
    # check range of data and print warnings
    min <- min(cebq_data[c(cebq_items)], na.rm = TRUE)
    max <- max(cebq_data[c(cebq_items)], na.rm = TRUE)
    
    if (isTRUE(base_zero)){
      if (min < 0 | max > 4) {
        warning("range in CEBQ data is outside expected range given base_zero = TRUE (expected range: 0-4). Scoring may be incorrect")
      } 
    } else {
      if (min < 1 | max > 5) {
        warning("range in CEBQ data is outside expected range given base_zero = FALSE (expected range: 1-5). Scoring may be incorrect")
      } 
    }

    # re-scale data
    cebq_data_edit <- cebq_data
    
    if (isTRUE(base_zero)){
      cebq_data_edit[cebq_items] <- sapply(cebq_items, function(x) cebq_data_edit[[x]] + 1, simplify = TRUE)
    }
    
    # calculate reversed scores

    reverse_qs <- c('cebq3', 'cebq4', 'cebq8', 'cebq10', 'cebq16', 'cebq32')

    for (var in 1:length(reverse_qs)) {
        var_name <- reverse_qs[var]
        reverse_name <- paste0(var_name, '_rev')

        cebq_data_edit[[reverse_name]] <- ifelse(cebq_data_edit[[var_name]] == 1, 5, ifelse(cebq_data_edit[[var_name]] == 2, 4, ifelse(cebq_data_edit[[var_name]] == 4, 2, ifelse(cebq_data_edit[[var_name]] == 5, 1, ifelse(cebq_data_edit[[var_name]] == 3, 3, NA)))))
        
    }

    ## Score Subscales

    # Food Responsiveness
    FR_vars <- c('cebq12', 'cebq14', 'cebq19', 'cebq28', 'cebq34')
    cebq_score_dat[['cebq_fr']] <- rowMeans(cebq_data_edit[FR_vars])

    # Emotional Overeating
    EOE_vars <- c('cebq2', 'cebq13', 'cebq15', 'cebq27')
    cebq_score_dat[['cebq_eoe']] <- rowMeans(cebq_data_edit[EOE_vars])

    # Enjoyment of Food
    EF_vars <- c('cebq1', 'cebq5', 'cebq20', 'cebq22')
    cebq_score_dat[['cebq_ef']] <- rowMeans(cebq_data_edit[EF_vars])

    # Desire to Drink
    DD_vars <- c('cebq6', 'cebq29', 'cebq31')
    cebq_score_dat[['cebq_dd']] <- rowMeans(cebq_data_edit[DD_vars])

    # Satiety Responsiveness
    SR_vars <- c('cebq3_rev', 'cebq17', 'cebq21', 'cebq26', 'cebq30')
    cebq_score_dat[['cebq_sr']] <- rowMeans(cebq_data_edit[SR_vars])

    # Slowness in Eating
    SE_vars <- c('cebq4_rev', 'cebq8', 'cebq18', 'cebq35')
    cebq_score_dat[['cebq_se']] <- rowMeans(cebq_data_edit[SE_vars])

    # Emotional Under Eating
    EUE_vars <- c('cebq9', 'cebq11', 'cebq23', 'cebq35')
    cebq_score_dat[['cebq_eue']] <- rowMeans(cebq_data_edit[EUE_vars])

    # Food Fussiness
    FF_vars <- c('cebq7', 'cebq10_rev', 'cebq16_rev', 'cebq24', 'cebq32_rev', 'cebq33')
    cebq_score_dat[['cebq_ff']] <- rowMeans(cebq_data_edit[FF_vars])

    # Total Approach Score
    cebq_score_dat[['cebq_approach']] <- rowMeans(cebq_data_edit[c(FR_vars, EOE_vars, EF_vars, DD_vars)])

    # Total Avoid Score
    cebq_score_dat[['cebq_avoid']] <- rowMeans(cebq_data_edit[c(SR_vars, SE_vars, EUE_vars, FF_vars)])
    
    # Alternative: Reward-based eating
    RBE_vars <- c('cebq1', 'cebq3_rev', 'cebq4_rev', 'cebq5', 'cebq8_rev', 'cebq12', 'cebq14', 'cebq19', 'cebq20', 'cebq22', 'cebq28', 'cebq34')
    cebq_score_dat[['cebq_rbe']] <- rowMeans(cebq_data_edit[c(RBE_vars)])
    
    # Alternative: Picky eating
    PE_vars <- c('cebq7', 'cebq10', 'cebq16', 'cebq24', 'cebq32', 'cebq33')
    cebq_score_dat[['cebq_pe']] <- rowMeans(cebq_data_edit[c(PE_vars)])
    
    # Alternative: Emotional eating
    EE_vars <- c('cebq2', 'cebq9', 'cebq13', 'cebq15', 'cebq23', 'cebq25')
    cebq_score_dat[['cebq_ee']] <- rowMeans(cebq_data_edit[c(EE_vars)])

 
    #### 3. Clean Export/Scored Data #####
    ## round data
    if (isTRUE(ID_arg)){
        cebq_score_dat[2:ncol(cebq_score_dat)] <- round(cebq_score_dat[2:ncol(cebq_score_dat)], digits = 3)
    } else {
        cebq_score_dat <- round(cebq_score_dat, digits = 3)
    }

    ## merge raw responses with scored data
    if (isTRUE(ID_arg)){
      cebq_phenotype <- merge(cebq_data, cebq_score_dat, by = id)
      
      return(list(score_dat = as.data.frame(cebq_score_dat),
                  bids_phenotype = as.data.frame(cebq_phenotype)))
    } else {
      return(list(score_dat = as.data.frame(cebq_score_dat)))
    }
    
}

