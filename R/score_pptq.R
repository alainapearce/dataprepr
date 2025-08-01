#' score_pptq: Scored data from the Pictorial Personality Traits Questionnaire for Children 
#'
#' This function scores the Pictorial Personality Traits Questionnaire for Children and provides subscale scores for: Extraversion, Neuroticism, Openness, Conscientiousness, Agreeableness
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'pptq#' or 'pptq_#' where # is the question number (1-15)
#' 3) Questions 1-15 must have the numeric value for the choices: 1 - Definitely Yes (left side), 2 - A little bit (left side), 3 - It depends, 4 - A little bit (right side), 5 - Definitely yes (right side) if pptq_scale = 5 or
#'                                                                1 - (left side), 2 - It depends (middle), 3 - (right side) if pptq_scale = 3
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Primary Reference for the Questionnaire and Scoring:
#' Maćkiewicz M, Cieciuch J. Pictorial Personality Traits Questionnaire for Children (PPTQ-C)-A New Measure of Children's Personality Traits. Front Psychol. 2016 Apr 14;7:498. doi: 10.3389/fpsyg.2016.00498. PMID: 27252661; PMCID: PMC4879772.
#' 
#' @param pptq_data a data.frame all items for the Pictorial Personality Traits Questionnaire for Children  following the naming conventions described above
#' @param pptq_scale 3 or 5: indicates if data was collected with a 3-point or 5-point likert scale
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @param extra_scale_cols a vector of character strings that begin with 'pptq' but are not scale items. Any columns in pptq_data that begin with 'pptq' but are not scale items must be included here. Default is empty vector.
#'
#' @return A dataset with subscale scores for the Pictorial Personality Traits Questionnaire for Children 
#' @examples
#'
#' # scoring for the pptq with IDs
#' pptq_score_data <- score_pptq(pptq_data, id = 'ID')
#'
#'
#' \dontrun{
#' }
#'
#' @export

score_pptq <- function(pptq_data, pptq_scale, pna_value, base_zero = TRUE, id, session_id, extra_scale_cols = c()) {
  
  #### 1. Set up/initial checks #####
  
  # check that pptq_data exist and is a data.frame
  data_arg <- methods::hasArg(pptq_data)
  
  if (isTRUE(data_arg) & !is.data.frame(pptq_data)) {
    stop("pptq_data must be entered as a data.frame")
  } else if (isFALSE(data_arg)) {
    stop("pptq_data must set to the data.frame with amount consumed for each food item")
  }
  
  # check if id exists
  ID_arg <- methods::hasArg(id)
  
  if (isTRUE(ID_arg)){
    if (!(id %in% names(pptq_data))) {
      stop("variable name entered as id is not in pptq_data")
    }
  }
  
  # check if session_id exists
  sessionID_arg <- methods::hasArg(session_id)
  
  if (isTRUE(sessionID_arg)){
    if (!(id %in% names(pptq_data))) {
      stop("variable name entered as session_id is not in pptq_data")
    }
  }
  
  if (isTRUE(pptq_scale)) {
    if (!is.numeric(pptq_scale)) {
      stop("pptq_scale argument must be be numeric: enter 3 or 5")
    } else if (pptq_scale != 3 | pptq_scale != 5) {
      stop("pptq_scale argument must be 3 or 5")
    }
  } else if (isFALSE(pptq_scale)) {
    stop("pptq_scale argument must be indicated. options are 3 and 5.")
  }
  
  # check base_zero is logical
  if (!is.logical(base_zero)) {
    stop("base_zero arg must be logical (TRUE/FALSE)")
  }
  
  #### 2. Set Up Data #####
  
  # set up database for results create empty matrix
  pptq_score_dat <- data.frame(pptq_extraversion = rep(NA, nrow(pptq_data)), 
                               pptq_neuroticism = rep(NA, nrow(pptq_data)), 
                               pptq_openness = rep(NA, nrow(pptq_data)),
                               pptq_conscientiousness = rep(NA, nrow(pptq_data)),
                               pptq_agreeableness = rep(NA, nrow(pptq_data)))
  
  if (isTRUE(ID_arg)) {
    if (isTRUE(sessionID_arg)) {
      pptq_score_dat <- data.frame(pptq_data[[id]], pptq_data[[session_id]], pptq_score_dat)
      names(pptq_score_dat)[1:2] <- c(id, session_id)
    } else {
      pptq_score_dat <- data.frame(pptq_data[[id]], pptq_score_dat)
      names(pptq_score_dat)[1] <- id
    }
  }
  
  # assign pptq scale items to pptq_items, excluding columns in extra_scale_cols
  pptq_items <- setdiff(grep("^pptq", names(pptq_data), value = TRUE), extra_scale_cols)
  
  # remove underscore in column names for pptq_items
  names(pptq_data)[names(pptq_data) %in% pptq_items] <- gsub('pptq_', 'pptq', names(pptq_data)[names(pptq_data) %in% pptq_items])
  
  # remove underscore in pptq_items
  pptq_items <- gsub("pptq_", "pptq", pptq_items)
  
  # if pna_value arg, replace not applicable values with NA
  if (isTRUE(methods::hasArg(pna_value))) {
    
    # replace pna_value with NA in pcw_vars
    pptq_data[pptq_items] <- lapply(pptq_data[pptq_items] , function(x) ifelse(x == pna_value, NA, x))
    
  }
  
  # check range of data and print warnings
  min <- min(pptq_data[c(pptq_items)], na.rm = TRUE)
  max <- max(pptq_data[c(pptq_items)], na.rm = TRUE)
  range <- (max - min)
  
  if (isTRUE(base_zero)) {
    if (pptq_scale == 5) {
      if (min < 0 | max > 4) {
        warning(
          "range in PPTQ data is outside expected range given base_zero = TRUE and pptq_scale = 5 (expected range: 0-4). Scoring may be incorrect"
        )
      }
      
      if (range < 5) {
        warning(
          "Range of PPTQ values is < 5. Confirm which pptq_scale (3 or 5) is the correct scale"
        )
      }
      
    } else {
      if (min < 0 | max > 2) {
        warning(
          "range in PPTQ data is outside expected range given base_zero = TRUE and pptq_scale = 3 (expected range: 0-2). Scoring may be incorrect"
        )
      }
      
    }
  } else {
    if (pptq_scale == 5) {
      if (min < 1 | max > 5) {
        warning(
          "range in PPTQ data is outside expected range given base_zero = TRUE and pptq_scale = 5 (expected range: 1-5). Scoring may be incorrect"
        )
      }
      
      if (range < 5) {
        warning(
          "Range of PPTQ values is < 5. Confirm which pptq_scale (3 or 5) is the correct scale"
        )
      }
      
    } else {
      if (min < 1 | max > 3) {
        warning(
          "range in PPTQ data is outside expected range given base_zero = TRUE and pptq_scale = 3 (expected range: 1-3). Scoring may be incorrect"
        )
      }
      
    }
  }
  
  # re-scale data
  pptq_data_edit <- pptq_data
  
  if (isTRUE(base_zero)){
    pptq_data_edit[pptq_items] <- sapply(pptq_items, function(x) pptq_data[[x]] + 1, simplify = TRUE)
  }
  
  # calculate reversed scores
  reverse_qs <- c("pptq2", "pptq4", "pptq6", "pptq8", "pptq10", "pptq12", "pptq14")
  
  for (var in 1:length(reverse_qs)) {
    var_name <- reverse_qs[var]
    reverse_name <- paste0(var_name, "_rev")
    
    if (pptq_scale == 5) {
      pptq_data_edit[[reverse_name]] <- ifelse(is.na(pptq_data_edit[[var_name]]), NA, 
                                               ifelse(pptq_data_edit[[var_name]] == 1, 5, 
                                                      ifelse(pptq_data_edit[[var_name]] == 2, 4, 
                                                             ifelse(pptq_data_edit[[var_name]] == 3, 3,
                                                                    ifelse(pptq_data_edit[[var_name]] == 4, 2, 1)))))
      
    } else {
      if (pptq_scale == 3) {
        pptq_data_edit[[reverse_name]] <- ifelse(is.na(pptq_data_edit[[var_name]]), NA, 
                                                 ifelse(pptq_data_edit[[var_name]] == 1, 3, 
                                                        ifelse(pptq_data_edit[[var_name]] == 2, 2, 1)))
        
      }
    }
  }
  
  ## Score Subscales
  
  # Extraversion
  ex_vars <- c("pptq1", "pptq6_rev", "pptq11")
  pptq_score_dat[["pptq_extraversion"]] <- rowSums(pptq_data_edit[ex_vars])
  
  # Neuroticism
  neurot_vars <- c("pptq2_rev", "pptq7", "pptq12_rev")
  pptq_score_dat[["pptq_neuroticism"]] <- rowSums(pptq_data_edit[neurot_vars])
  
  # Openness
  open_vars <- c("pptq3", "pptq8_rev", "pptq13")
  pptq_score_dat[["pptq_openness"]] <- rowSums(pptq_data_edit[open_vars])
  
  # Conscientiousness
  consci_vars <- c("pptq4_rev", "pptq9", "pptq14_rev")
  pptq_score_dat[["pptq_conscientiousness"]] <- rowSums(pptq_data_edit[consci_vars])
  
  # Agreeableness
  agree_vars <- c("pptq5", "pptq10_rev", "pptq15")
  pptq_score_dat[["pptq_agreeableness"]] <- rowSums(pptq_data_edit[agree_vars])
  
  #### 3. Clean Export/Scored Data #####
  
  ## merge raw responses with scored data
  if (isTRUE(ID_arg)){
    if (isTRUE(sessionID_arg)) {
      pptq_phenotype <- merge(pptq_data, pptq_score_dat, by = c(id, session_id))
    } else {
      pptq_phenotype <- merge(pptq_data, pptq_score_dat, by = id)
    }
    
    return(list(score_dat = as.data.frame(pptq_score_dat),
                bids_phenotype = as.data.frame(pptq_phenotype)))
  } else {
    return(list(score_dat = as.data.frame(pptq_score_dat)))
  }
}

