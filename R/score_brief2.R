#' score_brief2: Scored data from the Behavioral Rating Inventory of Executive Function-2
#'
#' This function scores the Behavioral Rating Inventory of Executive Function-2 and provides subscale scores for the following behaviors: brief2_inhibit, Self-Monitor, Shift, Emotional Control, Initiate, Working Memory, Plan/Organize, Task Monitoring, Organization of Materials. There are also 4 index scores: Behavioral Regulation Index, Emotion Regulation Index, Cognitive Regulation Index, and the General Executive Composite. Three scores help indicate if responses were valid: Negativity Score, Inconsistency Score, and Infrequency Score
#'
#' These data are scored using the brief2_scoretables data available in the dataprepr package to look up age- and sex-normed t-scores and percentiles.
#'
#' For data to be scored correctly, the data must be prepared according to the following criteria: \cr
#' \itemize{
#'  \item{The data must include sex, age in years, and all individual questionnaire items}
#'  \item{The questionnaire items must match the following naming convention: 'brief#' or 'brief_#' where # is the question number (1-63)}
#'  \item{All questionnaire responses must be a numeric value ranging from 0-2 (base_zero = TRUE) or 1-3 (base_zero = FALSE) where: }
#'  \itemize{
#'     \item{For base_zero = TRUE: 0 = Never; 1 = Sometimes; 2 = Often}
#'     \item{For base_zero = FALSE: 1 = Never; 2 = Sometimes; 3 = Often}
#'   }
#'  \item{Missing values must be coded as NA}
#' }
#' \cr
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Gioia GA, Isquith PK, Guy SC, Kenworthy L. BRIEF-2: Behavior Rating Inventory of Executive Function: Professional Manual. Psychological Assessment Resources; 2015.
#'
#' @param brief_data a data.frame all items for the Behavioral Rating Inventory of Executive Function-2 following the naming conventions described above
#' @param age_var a string with the name of the age variable in brief_data
#' @param sex_var a string with the name of the sex variable in brief_data
#' @param extra_scale_cols a vector of character strings that begin with 'brief' but are not scale items. Any columns in brief_data that begin with 'brief' but are not scale items must be included here. Default is empty vector.
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#'
#' @return A dataset with subscale scores for the Behavioral Rating Inventory of Executive Function-2
#' @examples
#'
#' # scoring for the brief with IDs
#' brief_score_data <- score_brief2(brief_data, age_var = 'age', sex_var = 'sex', id = 'ID')
#'
#' # scoring for the brief with specified levels for male and female
#' brief_score_data <- score_brief2(brief_data, age_var = 'age', sex_var = 'sex', male = 'male', female = 'female', id = 'ID')
#'
#' \dontrun{
#'
#' # age and sex variable names must be strings - the following would not be correct
#' brief_score_data <- score_brief2(brief_data, age_var = age, sex_var = sex)
#'
#' # male and female specification must match the data in brief_data. Do not give the value label if brief_data has label attributes for sex.
#'
#' #check attributes for sex
#' attributes(brief_data$sex)
#'
#' #$labels
#' #Male Female
#' # 0      1
#'
#' #with the above attributes, the following will not run as the data.frame contains 0's and 1's, not the labels
#' brief_score_data <- score_brief2(brief_data, age_var = 'age', sex_var = 'sex', male = 'Male', female = 'Female')
#'
#' }
#'
#'
#' @export

score_brief2 <- function(brief_data, age_var, sex_var, pna_value, base_zero = TRUE, male = 0, female = 1, id, session_id, extra_scale_cols = c()) {

    #### 1. Set up/initial checks #####

    # check that brief_data exist and is a data.frame
    data_arg <- methods::hasArg(brief_data)

    if (isTRUE(data_arg) & !is.data.frame(brief_data)) {
        stop('brief_data must be entered as a data.frame')
    } else if (isFALSE(data_arg)) {
        stop('brief_data must set to the data.frame with amount consumed for each food item')
    }

    # check that age_var exist and is a string
    age_arg <- methods::hasArg(age_var)

    if (isTRUE(age_arg)) {
        if (!is.character(age_var)) {
            stop('age_var must be entered as a string and match the age variable name in brief_data')
        } else if (!(age_var %in% names(brief_data))) {
            stop(paste0('the provided age_var: "', age_var, '" does not exist in breif_data. To check variable names use "names(brief_data)".'))
        }
    } else if (isFALSE(age_arg)) {
        stop('age_var must be entered as a string and match the age variable name in brief_data')
    }

    # check that sex_var exist and is a string
    sex_arg <- methods::hasArg(sex_var)

    if (isTRUE(sex_arg)) {
        if (!is.character(sex_var)) {
            stop('sex_var must be entered as a string and match the sex variable name in brief_data')
        } else if (!(sex_var %in% names(brief_data))) {
            stop(paste0('the provided sex_var: "', sex_var, '" does not exist in breif_data. To check variable names use "names(brief_data)".'))
        }
    } else if (isFALSE(sex_arg)) {
        stop('sex_var must be entered as a string and match the sex variable name in brief_data')
    }

    # check varaible 'sex' exists and for male and female arguments
    male_arg <- methods::hasArg(male)
    female_arg <- methods::hasArg(female)

    # check number of unique values in dataset
    nsex_unique <- length(unique(brief_data[[sex_var]]))

    # entered arguments match number of different sexes in data
    if (isTRUE(male_arg) | isTRUE(female_arg)) {

        if (sum(isTRUE(male_arg), isTRUE(female_arg)) != nsex_unique) {
          
            stop('The number of alternate values entered for sex do not match the number of different sexes in data. If specifying non-default values for male and/or female, must provide all values that exist in data (e.g., if have both males and females, need to provide values for both)')
        }
    }

    # check if id exists
    ID_arg <- methods::hasArg(id)

    if (isTRUE(ID_arg)){
        if (!(id %in% names(brief_data))) {
            stop('variable name entered as id is not in brief_data')
        }
    }

    # check if session_id exists
    sessionID_arg <- methods::hasArg(session_id)
    
    if (isTRUE(sessionID_arg)){
      if (!(id %in% names(brief_data))) {
        stop("variable name entered as session_id is not in brief_data")
      }
    }
    
    #### 2. Set Up Data #####

    # set up database for results create empty matrix
    brief_score_dat <- data.frame(brief2_inhibit = rep(NA, nrow(brief_data)), brief2_inhibit_t = rep(NA, nrow(brief_data)), brief2_inhibit_p = rep(NA, nrow(brief_data)), brief2_selfmon = rep(NA, nrow(brief_data)), brief2_selfmon_t = rep(NA, nrow(brief_data)), brief2_selfmon_p = rep(NA, nrow(brief_data)), brief2_shift = rep(NA, nrow(brief_data)), brief2_shift_t = rep(NA, nrow(brief_data)), brief2_shift_p = rep(NA, nrow(brief_data)), brief2_emcont = rep(NA, nrow(brief_data)), brief2_emcont_t = rep(NA, nrow(brief_data)), brief2_emcont_p = rep(NA, nrow(brief_data)), brief2_initiate = rep(NA, nrow(brief_data)), brief2_initiate_t = rep(NA, nrow(brief_data)), brief2_initiate_p = rep(NA, nrow(brief_data)), brief2_wm = rep(NA, nrow(brief_data)), brief2_wm_t = rep(NA, nrow(brief_data)), brief2_wm_p = rep(NA, nrow(brief_data)), brief2_planorg = rep(NA, nrow(brief_data)), brief2_planorg_t = rep(NA, nrow(brief_data)), brief2_planorg_p = rep(NA, nrow(brief_data)), brief2_taskmon = rep(NA, nrow(brief_data)), brief2_taskmon_t = rep(NA, nrow(brief_data)), brief2_taskmon_p = rep(NA, nrow(brief_data)), brief2_orgmat = rep(NA, nrow(brief_data)), brief2_orgmat_t = rep(NA, nrow(brief_data)), brief2_orgmat_p = rep(NA, nrow(brief_data)), brief2_bri = rep(NA, nrow(brief_data)), brief2_bri_t = rep(NA, nrow(brief_data)), brief2_bri_p = rep(NA, nrow(brief_data)), brief2_eri = rep(NA, nrow(brief_data)), brief2_eri_t = rep(NA, nrow(brief_data)), brief2_eri_p = rep(NA, nrow(brief_data)), brief2_cri = rep(NA, nrow(brief_data)), brief2_cri_t = rep(NA, nrow(brief_data)), brief2_cri_p = rep(NA, nrow(brief_data)), brief2_gec = rep(NA, nrow(brief_data)), brief2_gec_t = rep(NA, nrow(brief_data)), brief2_gec_p = rep(NA, nrow(brief_data)), brief2_negativity = rep(NA, nrow(brief_data)), brief2_negativity_p = rep(NA, nrow(brief_data)), brief2_negativity_cat = rep(NA, nrow(brief_data)), brief2_inconsistency = rep(NA, nrow(brief_data)), brief2_inconsistency_p = rep(NA, nrow(brief_data)), brief2_inconsistency_cat = rep(NA, nrow(brief_data)), brief2_infrequency = rep(NA, nrow(brief_data)), brief2_infrequency_p = rep(NA, nrow(brief_data)), brief2_infrequency_cat = rep(NA, nrow(brief_data)))

    if (isTRUE(ID_arg)) {
      if (isTRUE(sessionID_arg)) {
        brief_score_dat <- data.frame(brief_data[[id]], brief_data[[session_id]], brief_score_dat)
        names(brief_score_dat)[1:2] <- c(id, session_id)
      } else {
        brief_score_dat <- data.frame(brief_data[[id]], brief_score_dat)
        names(brief_score_dat)[1] <- id
      }
    }

    # assign brief scale items to brief_items, excluding columns in extra_scale_cols
    brief_items <- setdiff(grep("^brief", names(brief_data), value = TRUE), extra_scale_cols)
    
    # remove underscore in column names for brief_items
    names(brief_data)[names(brief_data) %in% brief_items] <- gsub('brief_', 'brief', names(brief_data)[names(brief_data) %in% brief_items])
    
    # remove underscore in brief_items
    brief_items <- gsub("brief_", "brief", brief_items)
    
    # if pna_value arg, replace not applicable values with NA
    if (isTRUE(methods::hasArg(pna_value))) {
      
      # replace pna_value with NA in pcw_vars
      brief_data[brief_items] <- lapply(brief_data[brief_items] , function(x) ifelse(x == pna_value, NA, x))
      
    }
    
    # check range of data and print warnings
    min <- min(brief_data[c(brief_items)], na.rm = TRUE)
    max <- max(brief_data[c(brief_items)], na.rm = TRUE)
    
    if (isTRUE(base_zero)){
      if (min < 0 | max > 2) {
        warning("range in BRIEF2 data is outside expected range given base_zero = TRUE (expected range: 0-2). Scoring may be incorrect")
      } 
    } else {
      if (min < 1 | max > 3) {
        warning("range in BRIEF2 data is outside expected range given base_zero = FALSE (expected range: 1-3). Scoring may be incorrect")
      } 
    }
    
    #make copy of data
    brief_data_edit <- brief_data
    
    # change values of sex in brief_data_edit to default
    if (sum(isTRUE(male_arg), isTRUE(female_arg)) == nsex_unique) {
      if (nsex_unique == 2) {
        brief_data_edit[[sex_var]] <-
          ifelse(brief_data_edit[[sex_var]] == male, 0, 1)
        
        brief_data_edit[[sex_var]] <-
          factor(brief_data_edit[[sex_var]], levels = c(0, 1))
        
      } else if (nsex_unique == 1 & isTRUE(male_arg)) {
        # if only 1 value for sex and male is specified, set default to 0
        brief_data_edit[[sex_var]] <- 0
      } else if (nsex_unique == 1 & isTRUE(female_arg)) {
        # if only 1 value for sex and female is specified, set default to 1
        brief_data_edit[[sex_var]] <- 1
      }
    } else if (sum(isTRUE(male_arg), isTRUE(female_arg)) == 0) {
      brief_data_edit[[sex_var]] <- factor(brief_data_edit[[sex_var]])
    }

    
    # re-scale data
    if (isTRUE(base_zero)){
      brief_data_edit[brief_items] <- sapply(brief_items, function(x) brief_data_edit[[x]] + 1, simplify = TRUE)
    }
    
    ## Score Subscales

    # brief2_inhibit
    brief2_inhib_vars <- c('brief1', 'brief10', 'brief16', 'brief24', 'brief30', 'brief39', 'brief48', 'brief62')
    brief_score_dat[['brief2_inhibit']] <- rowSums(brief_data_edit[brief2_inhib_vars])

    # look up T-score and percentile
    inhib_scores_list <- mapply(ref_brief2_lookup, value = brief_score_dat[['brief2_inhibit']], sex = brief_data_edit[[sex_var]], age = brief_data_edit[[age_var]], MoreArgs = list(item = 'inhibit'))

    inhib_scores <- as.data.frame(matrix(unlist(inhib_scores_list), byrow = TRUE, ncol = 3))
    names(inhib_scores) <- c('item', 't', 'p')

    brief_score_dat[['brief2_inhibit_t']] <- as.numeric(inhib_scores[['t']])
    brief_score_dat[['brief2_inhibit_p']] <- inhib_scores[['p']]

    # Self-Monitoring
    brief2_selfmon_vars <- c('brief4', 'brief13', 'brief20', 'brief26')
    brief_score_dat[['brief2_selfmon']] <- rowSums(brief_data_edit[brief2_selfmon_vars])

    # look up T-score and percentile
    brief2_selfmon_scores_list <- mapply(ref_brief2_lookup, value = brief_score_dat[['brief2_selfmon']], sex = brief_data_edit[[sex_var]], age = brief_data_edit[[age_var]], MoreArgs = list(item = 'selfmon'))

    brief2_selfmon_scores <- as.data.frame(matrix(unlist(brief2_selfmon_scores_list), byrow = TRUE, ncol = 3))
    names(brief2_selfmon_scores) <- c('item', 't', 'p')

    brief_score_dat[['brief2_selfmon_t']] <- as.numeric(brief2_selfmon_scores[['t']])
    brief_score_dat[['brief2_selfmon_p']] <- brief2_selfmon_scores[['p']]

    # Shifting
    brief2_shift_vars <- c('brief2', 'brief11', 'brief17', 'brief31', 'brief40', 'brief49', 'brief58', 'brief60')
    brief_score_dat[['brief2_shift']] <- rowSums(brief_data_edit[brief2_shift_vars])

    # look up T-score and percentile
    brief2_shift_scores_list <- mapply(ref_brief2_lookup, value = brief_score_dat[['brief2_shift']], sex = brief_data_edit[[sex_var]], age = brief_data_edit[[age_var]], MoreArgs = list(item = 'shift'))

    brief2_shift_scores <- as.data.frame(matrix(unlist(brief2_shift_scores_list), byrow = TRUE, ncol = 3))
    names(brief2_shift_scores) <- c('item', 't', 'p')

    brief_score_dat[['brief2_shift_t']] <- as.numeric(brief2_shift_scores[['t']])
    brief_score_dat[['brief2_shift_p']] <- brief2_shift_scores[['p']]

    # Emotional Control
    brief2_emcont_vars <- c('brief6', 'brief14', 'brief22', 'brief27', 'brief34', 'brief43', 'brief51', 'brief56')
    brief_score_dat[['brief2_emcont']] <- rowSums(brief_data_edit[brief2_emcont_vars])

    # look up T-score and percentile
    brief2_emcont_scores_list <- mapply(ref_brief2_lookup, value = brief_score_dat[['brief2_emcont']], sex = brief_data_edit[[sex_var]], age = brief_data_edit[[age_var]], MoreArgs = list(item = 'emcont'))

    brief2_emcont_scores <- as.data.frame(matrix(unlist(brief2_emcont_scores_list), byrow = TRUE, ncol = 3))
    names(brief2_emcont_scores) <- c('item', 't', 'p')

    brief_score_dat[['brief2_emcont_t']] <- as.numeric(brief2_emcont_scores[['t']])
    brief_score_dat[['brief2_emcont_p']] <- brief2_emcont_scores[['p']]

    # Initiate
    brief2_initiate_vars <- c('brief9', 'brief38', 'brief50', 'brief55', 'brief61')
    brief_score_dat[['brief2_initiate']] <- rowSums(brief_data_edit[brief2_initiate_vars])

    # look up T-score and percentile
    brief2_initiate_scores_list <- mapply(ref_brief2_lookup, value = brief_score_dat[['brief2_initiate']], sex = brief_data_edit[[sex_var]], age = brief_data_edit[[age_var]], MoreArgs = list(item = 'initiate'))

    brief2_initiate_scores <- as.data.frame(matrix(unlist(brief2_initiate_scores_list), byrow = TRUE, ncol = 3))
    names(brief2_initiate_scores) <- c('item', 't', 'p')

    brief_score_dat[['brief2_initiate_t']] <- as.numeric(brief2_initiate_scores[['t']])
    brief_score_dat[['brief2_initiate_p']] <- brief2_initiate_scores[['p']]

    # Working Memory
    brief2_wm_vars <- c('brief3', 'brief12', 'brief19', 'brief25', 'brief28', 'brief32', 'brief41', 'brief46')
    brief_score_dat[['brief2_wm']] <- rowSums(brief_data_edit[brief2_wm_vars])

    # look up T-score and percentile
    brief2_wm_scores_list <- mapply(ref_brief2_lookup, value = brief_score_dat[['brief2_wm']], sex = brief_data_edit[[sex_var]], age = brief_data_edit[[age_var]], MoreArgs = list(item = 'wm'))

    brief2_wm_scores <- as.data.frame(matrix(unlist(brief2_wm_scores_list), byrow = TRUE, ncol = 3))
    names(brief2_wm_scores) <- c('item', 't', 'p')

    brief_score_dat[['brief2_wm_t']] <- as.numeric(brief2_wm_scores[['t']])
    brief_score_dat[['brief2_wm_p']] <- brief2_wm_scores[['p']]

    # Planing and Organization
    brief2_planorg_vars <- c('brief7', 'brief15', 'brief23', 'brief35', 'brief44', 'brief52', 'brief57', 'brief59')
    brief_score_dat[['brief2_planorg']] <- rowSums(brief_data_edit[brief2_planorg_vars])

    # look up T-score and percentile
    brief2_planorg_scores_list <- mapply(ref_brief2_lookup, value = brief_score_dat[['brief2_planorg']], sex = brief_data_edit[[sex_var]], age = brief_data_edit[[age_var]], MoreArgs = list(item = 'planorg'))

    brief2_planorg_scores <- as.data.frame(matrix(unlist(brief2_planorg_scores_list), byrow = TRUE, ncol = 3))
    names(brief2_planorg_scores) <- c('item', 't', 'p')

    brief_score_dat[['brief2_planorg_t']] <- as.numeric(brief2_planorg_scores[['t']])
    brief_score_dat[['brief2_planorg_p']] <- brief2_planorg_scores[['p']]

    # Task Monitoring
    taskmon_vars <- c('brief5', 'brief21', 'brief29', 'brief33', 'brief42')
    brief_score_dat[['brief2_taskmon']] <- rowSums(brief_data_edit[taskmon_vars])

    # look up T-score and percentile
    taskmon_scores_list <- mapply(ref_brief2_lookup, value = brief_score_dat[['brief2_taskmon']], sex = brief_data_edit[[sex_var]], age = brief_data_edit[[age_var]], MoreArgs = list(item = 'taskmon'))

    taskmon_scores <- as.data.frame(matrix(unlist(taskmon_scores_list), byrow = TRUE, ncol = 3))
    names(taskmon_scores) <- c('item', 't', 'p')

    brief_score_dat[['brief2_taskmon_t']] <- as.numeric(taskmon_scores[['t']])
    brief_score_dat[['brief2_taskmon_p']] <- taskmon_scores[['p']]

    # Organization of Materials
    brief2_orgmat_vars <- c('brief8', 'brief37', 'brief45', 'brief47', 'brief53', 'brief63')
    brief_score_dat[['brief2_orgmat']] <- rowSums(brief_data_edit[brief2_orgmat_vars])

    # look up T-score and percentile
    brief2_orgmat_scores_list <- mapply(ref_brief2_lookup, value = brief_score_dat[['brief2_orgmat']], sex = brief_data_edit[[sex_var]], age = brief_data_edit[[age_var]], MoreArgs = list(item = 'orgmat'))

    brief2_orgmat_scores <- as.data.frame(matrix(unlist(brief2_orgmat_scores_list), byrow = TRUE, ncol = 3))
    names(brief2_orgmat_scores) <- c('item', 't', 'p')

    brief_score_dat[['brief2_orgmat_t']] <- as.numeric(brief2_orgmat_scores[['t']])
    brief_score_dat[['brief2_orgmat_p']] <- brief2_orgmat_scores[['p']]

    # Behavioral Regulation Index
    brief_score_dat[['brief2_bri']] <- rowSums(brief_data_edit[c(brief2_inhib_vars, brief2_selfmon_vars)])

    # look up T-score and percentile
    brief2_bri_scores_list <- mapply(ref_brief2_lookup, value = brief_score_dat[['brief2_bri']], sex = brief_data_edit[[sex_var]], age = brief_data_edit[[age_var]], MoreArgs = list(item = 'bri'))

    brief2_bri_scores <- as.data.frame(matrix(unlist(brief2_bri_scores_list), byrow = TRUE, ncol = 3))
    names(brief2_bri_scores) <- c('item', 't', 'p')

    brief_score_dat[['brief2_bri_t']] <- as.numeric(brief2_bri_scores[['t']])
    brief_score_dat[['brief2_bri_p']] <- brief2_bri_scores[['p']]

    # Emotional Regulation Index
    brief_score_dat[['brief2_eri']] <- rowSums(brief_data_edit[c(brief2_shift_vars, brief2_emcont_vars)])

    # look up T-score and percentile
    brief2_eri_scores_list <- mapply(ref_brief2_lookup, value = brief_score_dat[['brief2_eri']], sex = brief_data_edit[[sex_var]], age = brief_data_edit[[age_var]], MoreArgs = list(item = 'eri'))

    brief2_eri_scores <- as.data.frame(matrix(unlist(brief2_eri_scores_list), byrow = TRUE, ncol = 3))
    names(brief2_eri_scores) <- c('item', 't', 'p')

    brief_score_dat[['brief2_eri_t']] <- as.numeric(brief2_eri_scores[['t']])
    brief_score_dat[['brief2_eri_p']] <- brief2_eri_scores[['p']]

    # Cognitive Regulation Index
    brief_score_dat[['brief2_cri']] <- rowSums(brief_data_edit[c(brief2_initiate_vars, brief2_wm_vars, brief2_planorg_vars, taskmon_vars, brief2_orgmat_vars)])

    # look up T-score and percentile
    brief2_cri_scores_list <- mapply(ref_brief2_lookup, value = brief_score_dat[['brief2_cri']], sex = brief_data_edit[[sex_var]], age = brief_data_edit[[age_var]], MoreArgs = list(item = 'cri'))

    brief2_cri_scores <- as.data.frame(matrix(unlist(brief2_cri_scores_list), byrow = TRUE, ncol = 3))
    names(brief2_cri_scores) <- c('item', 't', 'p')

    brief_score_dat[['brief2_cri_t']] <- as.numeric(brief2_cri_scores[['t']])
    brief_score_dat[['brief2_cri_p']] <- brief2_cri_scores[['p']]

    # General Executive Composite
    brief_score_dat[['brief2_gec']] <- rowSums(brief_data_edit[c(brief2_inhib_vars, brief2_selfmon_vars, brief2_shift_vars, brief2_emcont_vars, brief2_initiate_vars, brief2_wm_vars, brief2_planorg_vars, taskmon_vars, brief2_orgmat_vars)])

    # look up T-score and percentile
    brief2_gec_scores_list <- mapply(ref_brief2_lookup, value = brief_score_dat[['brief2_gec']], sex = brief_data_edit[[sex_var]], age = brief_data_edit[[age_var]], MoreArgs = list(item = 'gec'))

    brief2_gec_scores <- as.data.frame(matrix(unlist(brief2_gec_scores_list), byrow = TRUE, ncol = 3))
    names(brief2_gec_scores) <- c('item', 't', 'p')

    brief_score_dat[['brief2_gec_t']] <- as.numeric(brief2_gec_scores[['t']])
    brief_score_dat[['brief2_gec_p']] <- brief2_gec_scores[['p']]

    ## Scale Checks

    # Negativity
    neg_vars <- c('brief14', 'brief28', 'brief30', 'brief34', 'brief39', 'brief41', 'brief58', 'brief60')
    brief_score_dat[['brief2_negativity']] <- rowSums(brief_data_edit[neg_vars] == 3)

    # look up T-score and percentile
    neg_scores_list <- mapply(ref_brief2_lookup, value = brief_score_dat[['brief2_negativity']], MoreArgs = list(item = 'negativity'))

    neg_scores <- as.data.frame(matrix(unlist(neg_scores_list), byrow = TRUE, ncol = 3))
    names(neg_scores) <- c('item', 'p', 'cat')

    brief_score_dat[['brief2_negativity_p']] <- ifelse(is.na(neg_scores[['p']]), NA, ifelse(neg_scores[['p']] == '<=98', 0, ifelse(neg_scores[['p']] == '99', 1, 2)))
  
    brief_score_dat[['brief2_negativity_cat']] <- ifelse(is.na(neg_scores[['cat']]), NA, ifelse(neg_scores[['cat']] == 'Acceptable', 0, ifelse(neg_scores[['cat']] == 'Elevated', 1, 2)))
    
    # Inconsistency
    incon_vars1 <- c('brief5', 'brief9', 'brief10', 'brief17', 'brief20', 'brief22', 'brief25', 'brief37')
    incon_vars2 <- c('brief21', 'brief55', 'brief48', 'brief40', 'brief26',  'brief56', 'brief50',  'brief63')

    brief_score_dat[['brief2_inconsistency']] <- rowSums(abs(brief_data_edit[incon_vars1] - brief_data_edit[incon_vars2]))

    # look up T-score and percentile
    incon_scores_list <- mapply(ref_brief2_lookup, value = brief_score_dat[['brief2_inconsistency']], MoreArgs = list(item = 'inconsistency'))

    incon_scores <- as.data.frame(matrix(unlist(incon_scores_list), byrow = TRUE, ncol = 3))
    names(incon_scores) <- c('item', 'p', 'cat')

    brief_score_dat[['brief2_inconsistency_p']] <- ifelse(is.na(incon_scores[['p']]), NA, ifelse(incon_scores[['p']] == '<=98', 0, ifelse(incon_scores[['p']] == '99', 1, 2)))
    
    brief_score_dat[['brief2_inconsistency_cat']] <- ifelse(is.na(incon_scores[['cat']]), NA, ifelse(incon_scores[['cat']] == 'Acceptable', 0, ifelse(incon_scores[['cat']] == 'Questionable', 1, 2)))
    
    # Infrequency
    infreq_vars <- c('brief18', 'brief36', 'brief54')
    brief_score_dat[['brief2_infrequency']] <- rowSums(brief_data_edit[infreq_vars] > 1)

    # look up T-score and percentile
    infreq_scores_list <- mapply(ref_brief2_lookup, value = brief_score_dat[['brief2_infrequency']], MoreArgs = list(item = 'infrequency'))

    infreq_scores <- as.data.frame(matrix(unlist(infreq_scores_list), byrow = TRUE, ncol = 3))
    names(infreq_scores) <- c('item', 'p', 'cat')

    brief_score_dat[['brief2_infrequency_p']] <- ifelse(is.na(infreq_scores[['p']]), NA, ifelse(infreq_scores[['p']] == '99', 0, 1))

    brief_score_dat[['brief2_infrequency_cat']] <- ifelse(is.na(infreq_scores[['cat']]), NA, ifelse(infreq_scores[['cat']] == 'Acceptable', 0, 1))


    #### 3. Clean Export/Scored Data #####
    ## merge raw responses with scored data
    if (isTRUE(ID_arg)){
      if (isTRUE(sessionID_arg)) {
        brief_phenotype <- merge(brief_data, brief_score_dat, by = c(id, session_id))
      } else {
        brief_phenotype <- merge(brief_data, brief_score_dat, by = id)
      }
      
      return(list(score_dat = as.data.frame(brief_score_dat),
                  bids_phenotype = as.data.frame(brief_phenotype)))
    } else {
      return(list(score_dat = as.data.frame(brief_score_dat)))
    }
    
    
}

