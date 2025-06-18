#' score_pds: Scored data from Parent- or Self-Report Pubertal Development Scale
#'
#' This function scores the the Child- and Parent-Pubertal Development Scale. The pubertal development score (pds_score) is an average of all questions, ignoring any 'I Don't Know' responses. Following the unpublished manuscript guidelines by Crockett, L. J. (1988), the Tanner Stage equivalent is computed by sex: Males - on the sum of the scores for body hair growth, voice change, and facial hair growth; Females - sum of scores for body hair growth and breast development along with the binary response to menarche (yes/no). For males, 1 of the 3 items is allowed to be 'I Don't Know' while for females, 1 of the 2 growth questions was allowed to be 'I Don't Know' but response to menarche was required.
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must included a 'sex' variable (defualt: male = 0, female = 1)
#' 2) The following questions are required from the Pubertal Development Scale: questions 1-3 and the sex-specific versions of questions 4-5. Additional variables are allowed in the provided data so long as the above naming conventions below are followed for the questions necessary to score the assessment.
#' 3) The data set columns/variables must match the following naming convention: 'sex', 'pds_1', 'pds_2', 'pds_3', 'pds_4m', 'pds_5m', 'pds_4f', 'pds_5fa'.
#' 4) All variable should be converted to numeric values such that 1 = not started yet, 2 = barely started, 3 = definitely started, 4 = seems complete, and 99 = I Don't Know. For Female ('pds_5fa') question on menarche, the response can be coded as Yes = 1, No = 0.
#'
#' Note, as long as variable names match those listed, pds_data can include only data from male and/or female version scales.
#'
#' @references
#' Carskadon, Mary A., and Christine Acebo. A self-administered rating scale for pubertal development. Journal of Adolescent Health 14, no. 3 (1993): 190-195. https://doi.org/10.1016/1054-139X(93)90004-9 (\href{https://pubmed.ncbi.nlm.nih.gov/8323929/}{PubMed})
#'
#' Crockett, L. J. Pubertal development scale: Pubertal categories. Unpublished manuscript (1988).
#'
#'
#' @param pds_data a data.frame with child sex and all questions from the Pubertal Development Scale. The required data to score the Pubertal Development Scale include sex, questions 1-3, and male/female specific questions 4-5. These questions must have the following names in the dataset: 'sex', 'pds_1', 'pds_2', 'pds_3', 'pds_4m', 'pds_5m', 'pds_4f', 'pds_5fa'. Note, as long as variable names match those listed, pds_data can include only data from male and/or female version scales.
#' @param respondent string to indicate if parent or child completed the Pubertal Development Scale. Must enter either 'parent' or 'child'
#' @inheritParams score_bes
#' @param male (optional) value for male. Default value is male = 0
#' @param female (optional) value for female. Default value is female = 1
#' @inheritParams score_bes
#' @inheritParams score_bes
#'
#' @return A dataset with a Pubertal Development Score and the Tanner Stage equivalents.
#'
#' @examples
#' #default male/female
#' pds_tanner_data <- score_pds(pds_data, respondent = 'parent', id = 'ID')
#'
#' #specify male/female
#' pds_tanner_data <- score_pds(pds_data, respondent = 'parent', male = 'male', female = 'female', id = 'ID')
#'
#' \dontrun{
#'
#' # male and female specification must match the data in brief_data. Do not give the value label if pds_data has label attributes for sex.
#'
#' #check attributes for sex
#' attributes(pds_data$sex)
#'
#' #$labels
#' #Male Female
#' # 0      1
#'
#' #with the above attributes, the following will not run as the data.frame contains 0's and 1's, not the labels
#' pds_tanner_data <- score_pds(pds_data, age_var = 'age', sex_var = 'sex', male = 'Male', female = 'Female')
#'
#' }
#'
#' @seealso \code{\link{json_pds}}
#' @export

score_pds <- function(pds_data, respondent, base_zero = TRUE, male = 0, female = 1, id, session_id) {
  
  #### 1. Set up/initial checks #####
  
  # check that pds_dat exist and is a data.frame
  data_arg <- methods::hasArg(pds_data)
  
  if (isTRUE(data_arg) & !is.data.frame(pds_data)) {
    stop('pds_dat must be entered as a data.frame')
  } else if (isFALSE(data_arg)) {
    stop('pds_dat must set to the data.frame with all responses to the Pubertal Development Scale')
  }
  
  # check if id exists
  ID_arg <- methods::hasArg(id)
  
  if (isTRUE(ID_arg)){
    if (!(id %in% names(pds_data))) {
      stop('variable name entered as id is not in pds_data')
    }
  }
  
  # check if session_id exists
  sessionID_arg <- methods::hasArg(session_id)
  
  if (isTRUE(sessionID_arg)){
    if (!(id %in% names(pds_data))) {
      stop("variable name entered as session_id is not in pds_data")
    }
  }
  
  # check that respondent exist and is a string
  resp_arg <- methods::hasArg(respondent)
  
  if (isTRUE(resp_arg)) {
    if (!is.character(respondent)) {
      stop('respondent must be entered as a string and can either be \'parent\' or \'child\'')
    } else if (respondent != 'parent' & respondent != 'child') {
      stop('the optional values for respondent are: \'parent\' or \'child\'')
    }
  } else if (isFALSE(resp_arg)) {
    stop('respondent must be entered as a string and can either be \'parent\' or \'child\'')
  }
  
  # check base_zero is logical
  if (!is.logical(base_zero)) {
    stop("base_zero arg must be logical (TRUE/FALSE)")
  }
  
  #### 2. Set Up Data #####
  
  # make a working dataset to preserve original data
  pds_data_edits <- pds_data
  
  if(class(pds_data_edits['sex'])[1] == 'haven_labelled'){
    haven::labelled(pds_data_edits[['sex']], labels = NULL)
    haven::labelled(pds_data_edits[['sex']], label = NULL)
  }
  
  # check varaible 'sex' exists and for male and female arguments
  male_arg <- methods::hasArg(male)
  female_arg <- methods::hasArg(female)
  
  # check number of unique values in dataset
  nsex_unique <- length(unique(pds_data_edits[!is.na(pds_data_edits[['sex']]), 'sex']))
  
  if (!('sex' %in% names(pds_data_edits))) {
    stop('There is no variable "sex" in pds_data')
  } else {
    # entered arguments match number of different sexes in data
    if (isTRUE(male_arg) | isTRUE(female_arg)) {
      if (sum(isTRUE(male_arg), isTRUE(female_arg)) == nsex_unique) {
        if (nsex_unique == 2) {
          # change values of sex in pds_data to default
          pds_data_edits[['sex']] <- ifelse(pds_data_edits[['sex']] ==
                                              male, 0, 1)
          pds_data_edits[['sex']] <- factor(pds_data_edits[['sex']], levels = c(0,
                                                                                1))
        } else if (nsex_unique == 1 & isTRUE(male_arg)) {
          # if only 1 value for sex and male is specified, set default to 0
          pds_data_edits[['sex']] <- 0
        } else if (nsex_unique == 1 & isTRUE(female_arg)) {
          # if only 1 value for sex and female is specified, set default to 1
          pds_data_edits[['sex']] <- 1
        }
      } else {
        stop('The number of alternate values entered for sex do not match the number of different sexes in data. If specifying non-default values for male and/or female, must provide all values that exist in data (e.g., if have both males and females, need to provide values for both)')
      }
    } else if (sum(isTRUE(male_arg), isTRUE(female_arg)) == 0) {
      pds_data_edits[['sex']] <- factor(pds_data_edits[['sex']])
    }
  }
  

  # check variables in pds_data
  
  ## standard variable names
  pds_varnames <- c('pds_1', 'pds_2', 'pds_3', 'pds_4m', 'pds_5m', 'pds_4f', 'pds_5fa', 'pds_6')

  if (sum(pds_varnames[1:3] %in% names(pds_data_edits)) < 3) {
    stop('Not all required variable are in pds_data or variable names match: "pds_1", "pds_2", "pds_3"')
  }
  
  ## determine single or mixed sex
  
  sex_levels <- unique(!is.na(pds_data_edits[['sex']]))

  # check male variable names
  if (length(sex_levels) == 2) {
    if (sum(pds_varnames[4:5] %in% names(pds_data_edits)) < 2) {
      stop('The dataset contains data from males - Not all required male variable names are in pds_data or not all variable names match required namining: "pds_4m", "pds_5m"')
    }
  } else if (sex_levels == '0') {
    if (sum(pds_varnames[4:5] %in% names(pds_data_edits)) < 2) {
      stop('The dataset contains data from males - Not all required male variable names are in pds_data or not all variable names match required namining: "pds_4m", "pds_5m"')
    }
  }
  
  # check female variable names
  if (length(sex_levels) == 2) {
    if (sum(pds_varnames[6:7] %in% names(pds_data_edits)) < 2) {
      stop('The dataset contains data from female - Not all required female variable names are in pds_data or not all variable names match required namining: "pds_4f", "pds_5fa"')
    }
  } else if (sex_levels == '1'){
    if (sum(pds_varnames[6:7] %in% names(pds_data_edits)) < 2) {
      stop('The dataset contains data from female - Not all required female variable names are in pds_data or not all variable names match required namining: "pds_4f", "pds_5fa"')
    }
  }
  
  #### 2. Set Up Data #####
  
  # re-scale data except when value = 99
  if (isTRUE(base_zero)){
    pds_data[, names(pds_data) %in% pds_varnames] <- sapply(names(pds_data[, names(puberty_data) %in% pds_varnames]), function(x) ifelse(pds_data[[x]] != 99, pds_data[[x]] + 1, pds_data[[x]]), simplify = TRUE)
  }

  # check if variables are coded in appropriate range and change 99/'I Don't
  # Know' to NA
  for (var in 1:length(pds_varnames)) {
    var_name <- as.character(pds_varnames[var])
    
    if (var_name %in% names(pds_data_edits)) {
      
      # convert 'I Don't Know'/99
      pds_data_edits[[var_name]] <- ifelse(pds_data_edits[[var_name]] == 99, NA, pds_data_edits[[var_name]])
      
      # check range of values
      if (min(pds_data_edits[[var_name]], na.rm = TRUE) < 1 & max(pds_data_edits[[var_name]], na.rm = TRUE) > 4) {
        stop('coded level values should fall between the values 1 and 4. Try setting base_zero = TRUE')
      }
      
      # recode menarche
      if (var_name == 'pds_5fa') {
        pds_data_edits[[var_name]] <- ifelse(is.na(pds_data_edits[[var_name]]),  NA, ifelse(pds_data_edits[[var_name]] == 1, 4, 0))
      }
    }
  }
  
  #### 3. Score Data ##### pds_score
  male_vars <- pds_varnames[c(1:5)]
  female_vars <- pds_varnames[c(1:3, 6:7)]
  
  ## ensure numeric class
  pds_data_edits[male_vars] <- sapply(pds_data_edits[male_vars], function(x) as.numeric(x))
  pds_data_edits[female_vars] <- sapply(pds_data_edits[female_vars], function(x) as.numeric(x))
  
  ## number of NAs
  pds_data_edits[['pds_score_na']] <- ifelse(pds_data_edits[['sex']] == 0, rowSums(is.na(pds_data_edits[male_vars])), rowSums(is.na(pds_data_edits[female_vars])))
  
  pds_data_edits[['pds_score']] <- ifelse(pds_data_edits[['sex']] == 0, ifelse(pds_data_edits[['pds_score_na']] <= 1, rowMeans(pds_data_edits[male_vars], na.rm = TRUE), NA), ifelse(pds_data_edits[['pds_score_na']] <= 1 & !is.na(pds_data_edits[['pds_5fa']]), rowMeans(pds_data_edits[female_vars], na.rm = TRUE), NA))
  
  # tanner category
  male_tanner_vars <- pds_varnames[c(2, 4:5)]
  female_tanner_vars <- pds_varnames[c(2, 6)]
  
  pds_data_edits[['pds_tanner_sum']] <- ifelse(pds_data_edits[['sex']] == 0, rowSums(pds_data_edits[male_tanner_vars]), rowSums(pds_data_edits[female_tanner_vars]))
  
  pds_data_edits[['pds_tanner_cat']] <- ifelse(is.na(pds_data_edits[['pds_tanner_sum']]), NA, ifelse(pds_data_edits[['sex']] == 0, ifelse(pds_data_edits[['pds_tanner_sum']] == 12, 5, ifelse(pds_data_edits[['pds_tanner_sum']] >= 9, 4, ifelse(pds_data_edits[['pds_tanner_sum']] >= 6, ifelse(pds_data_edits['pds_2'] < 4 & pds_data_edits['pds_4m'] < 4 & pds_data_edits['pds_5m'] < 4, 3, 4), ifelse(pds_data_edits[['pds_tanner_sum']] >= 4, ifelse(pds_data_edits['pds_2'] < 3 & pds_data_edits['pds_4m'] < 3 & pds_data_edits['pds_5m'] < 3, 2, 3), 1)))), ifelse(pds_data_edits[['pds_5fa']] == 4, ifelse(pds_data_edits[['pds_tanner_sum']] == 8, 5, ifelse(pds_data_edits[['pds_tanner_sum']] <= 7, 4, NA)), ifelse(pds_data_edits[['pds_tanner_sum']] > 3, 3, ifelse(pds_data_edits[['pds_tanner_sum']] == 3, 2, ifelse(pds_data_edits[['pds_tanner_sum']] == 2, 1, NA))))))
  
  #### 3. Clean Export/Scored Data #####
  
  ## merge raw responses with scored data
  pds_scored <- pds_data_edits[, c(1, which(names(pds_data_edits) %in% c("pds_score_na", "pds_score", "pds_tanner_sum", "pds_tanner_cat")))]
  
  if (isTRUE(ID_arg)){
    if (isTRUE(sessionID_arg)) {
      pds_phenotype <- merge(pds_data, pds_scored, by = c(id, session_id))
    } else {
      pds_phenotype <- merge(pds_data, pds_scored, by = id)
    }

    return(list(score_dat = as.data.frame(pds_scored),
                bids_phenotype = as.data.frame(pds_phenotype)))
  } else {
    return(list(score_dat = as.data.frame(pds_scored)))
  }
  
  
}
