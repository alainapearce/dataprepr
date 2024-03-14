#' score_hfias: Scored data from the Household Food Insecurity Access Scale
#'
#' This function scores the Household Food Insecurity Access Scale and provides continuous and categorical indicators of food security
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention 'hfias#' or 'hfias_#' for categorical questions and 'hfias#a' or 'hfias_#a' for frequency questions, where # is the question number (1-9)
#' 3) Categorical questions 1-9 must have the numeric value for the choices: 0 - no (skips frequency question), 1 - yes
#'    Frequency Questions must have the numeric value for the choices: 0 - Rarely, 1 - Sometimes, 2 - Often (score_base = TRUE) or 1 - Rarely, 2 - Sometimes, 3 - Often (score_base = FALSE)
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Primary Reference for the Questionnaire and Scoring:
#' Coates, Jennifer, Anne Swindale and Paula Bilinsky. 2007. Household Food Insecurity Access Scale (HFIAS) for Measurement of Household Food Access: Indicator Guide (v. 3). Washington, D.C.:FHI 360/FANTA.
#' 
#' @param hfias_data a data.frame all items for the Pictorial Personality Traits Questionnaire for Children  following the naming conventions described above
#' @inheritParams score_bes
#'
#' @return A dataset with subscale scores for the Pictorial Personality Traits Questionnaire for Children 
#' @examples
#'
#' # scoring for the hfias with IDs
#' hfias_score_data <- score_hfias(hfias_data, id = 'ID')
#'
#'
#' \dontrun{
#' }
#'
#' @export

score_hfias <- function(hfias_data, score_base = TRUE, id) {
  
  #### 1. Set up/initial checks #####
  
  # check that hfias_data exist and is a data.frame
  data_arg <- methods::hasArg(hfias_data)
  
  if (isTRUE(data_arg) & !is.data.frame(hfias_data)) {
    stop("hfias_data must be entered as a data.frame")
  } else if (isFALSE(data_arg)) {
    stop("hfias_data must set to the data.frame with amount consumed for each food item")
  }
  
  # check if id exists
  ID_arg <- methods::hasArg(id)
  
  if (isTRUE(ID_arg)){
    if (!(id %in% names(hfias_data))) {
      stop("variable name entered as id is not in hfias_data")
    }
  }
  
  #### 2. Set Up Data #####
  
  # set up database for results create empty matrix
  hfias_score_dat <- data.frame(hfias_score_cont = rep(NA, nrow(hfias_data)),
                                hfias_score_cat = rep(NA, nrow(hfias_data)),
                                hfias_anx_domain = rep(NA, nrow(hfias_data)),
                                hfias_quality_domain = rep(NA, nrow(hfias_data)),
                                hfias_intake_domain = rep(NA, nrow(hfias_data)))
  
  if (isTRUE(ID_arg)) {
    hfias_score_dat <- data.frame(hfias_data[[id]], hfias_score_dat)
    names(hfias_score_dat)[1] <- id
  }
  
  # remove underscore if in column names
  names(hfias_data) <- gsub('hfias_', 'hfias', names(hfias_data))
  
  # get primary questions to score
  hfias_categorical_qs <- c("hfias1", "hfias2", "hfias3", "hfias4", "hfias5", "hfias6", "hfias7", "hfias8", "hfias9")
  hfias_frequency_qs <- c("hfias1a", "hfias2a", "hfias3a", "hfias4a", "hfias5a", "hfias6a", "hfias7a", "hfias8a", "hfias9a")
  
  # make copy of dataset
  hfias_data_edit <- hfias_data
  
  # re-scale frequeny items
  if (isTRUE(score_base)){
    hfias_data_edit[hfias_frequency_qs] <- sapply(hfias_frequency_qs, function(x) hfias_data[[x]] + 1, simplify = TRUE)
  }
  
  # set the value of frequency question to 0 if the corresponding categorical question == 0
  hfias_data_edit$hfias1a[hfias_data_edit$hfias1 == 0] <- 0
  hfias_data_edit$hfias2a[hfias_data_edit$hfias2 == 0] <- 0
  hfias_data_edit$hfias3a[hfias_data_edit$hfias3 == 0] <- 0
  hfias_data_edit$hfias4a[hfias_data_edit$hfias4 == 0] <- 0
  hfias_data_edit$hfias5a[hfias_data_edit$hfias5 == 0] <- 0
  hfias_data_edit$hfias6a[hfias_data_edit$hfias6 == 0] <- 0
  hfias_data_edit$hfias7a[hfias_data_edit$hfias7 == 0] <- 0
  hfias_data_edit$hfias8a[hfias_data_edit$hfias8 == 0] <- 0
  hfias_data_edit$hfias9a[hfias_data_edit$hfias9 == 0] <- 0
  
  ## Attempt to do the above this with a function and loop -- not working
  # # Define a function to 
  # #   (1) re-scale frequency question if score_base = TRUE
  # #   (2) set the value of frequency question if the corresponding categorical question == 0
  # 
  # set_frequency <- function(hfias_categorical_q, hfias_frequency_q, hfias_data_edit) {
  #   if (isTRUE(score_base)){
  #     hfias_data_edit[hfias_frequency_q] <- sapply(hfias_frequency_q, function(x) hfias_data[[x]] + 1, simplify = TRUE)
  #   }
  #   
  #   hfias_data_edit[[hfias_frequency_q]][hfias_data_edit[[hfias_categorical_q]] == 0] <- 0
  # 
  #   # Return the updated hfias_data_edit
  #   return(hfias_data_edit)
  # }
  # 
  # # apply set_frequency to re-scale frequency data
  # for (i in 1:9) {
  #   hfias_data_edit <- set_frequency(hfias_categorical_qs[[i]], hfias_frequency_qs[[i]], hfias_data_edit)
  # }
  # 
  
  ## Score Subscale
  
  # Continuous total score
  # Sum of the frequency-of-occurrence during the past four weeks for the 9 food insecurity-related conditions
  hfias_score_dat[["hfias_score_cont"]] <- rowSums(hfias_data_edit[hfias_frequency_qs])
  
  # Categorical total score
  hfias_score_dat[["hfias_score_cat"]] <- ifelse(hfias_data_edit[hfias_frequency_qs[1]] <= 1 & (rowSums(hfias_data_edit[hfias_frequency_qs[2:9]]) == 0), 1,
                                                 
                                                 ifelse((hfias_data_edit[hfias_frequency_qs[1]] >= 2 | 
                                                        hfias_data_edit[hfias_frequency_qs[2]] >= 1 |
                                                        hfias_data_edit[hfias_frequency_qs[3]] == 1 | 
                                                        hfias_data_edit[hfias_frequency_qs[4]] == 1) & (rowSums(hfias_data_edit[hfias_frequency_qs[5:9]]) == 0), 2, 
                                                        
                                                        ifelse((hfias_data_edit[hfias_frequency_qs[3]] >= 2 | 
                                                                 hfias_data_edit[hfias_frequency_qs[4]] >= 2 |
                                                                 hfias_data_edit[hfias_frequency_qs[5]] == 1 |
                                                                 hfias_data_edit[hfias_frequency_qs[5]] == 2 | 
                                                                 hfias_data_edit[hfias_frequency_qs[6]] == 1 |
                                                                 hfias_data_edit[hfias_frequency_qs[6]] == 2) & (rowSums(hfias_data_edit[hfias_frequency_qs[7:9]]) == 0), 3, 
                                                               
                                                               ifelse((hfias_data_edit[hfias_frequency_qs[5]] == 3 | 
                                                                         hfias_data_edit[hfias_frequency_qs[6]] == 3 |
                                                                         hfias_data_edit[hfias_frequency_qs[7]] >= 1 | 
                                                                         hfias_data_edit[hfias_frequency_qs[8]] >= 1 |
                                                                         hfias_data_edit[hfias_frequency_qs[9]] >= 1), 4, NA))))

  hfias_score_dat[["hfias_score_cat"]] <- factor(hfias_score_dat[["hfias_score_cat"]], levels = c(1, 2, 3, 4), labels = c("Food Secure", "Mildly Food Insecure Access", "Moderately Food Insecure Access", "Severely Food Insecure Access"))
  
  # Anxiety domain (response == 1 to Q1)
  hfias_score_dat$hfias_anx_domain <- as.numeric(ifelse(hfias_data_edit[hfias_categorical_qs[1]] == 1, 1, 0))
  
  # Insufficient Quality (response == 1 to Q2, Q3, or Q4)
  hfias_score_dat[["hfias_quality_domain"]] <- ifelse((rowSums(hfias_data_edit[hfias_categorical_qs[2:4]]) > 0), 1, 0)
    
  # Insufficient food intake and physical consequences (response == 1 to Q5, Q6, or Q7, Q8, or Q9)
  hfias_score_dat[["hfias_intake_domain"]] <- ifelse((rowSums(hfias_data_edit[hfias_categorical_qs[5:9]]) > 0), 1, 0)

  #### 3. Clean Export/Scored Data #####
  
  ## merge raw responses with scored data
  if (isTRUE(ID_arg)){
    hfias_phenotype <- merge(hfias_data, hfias_score_dat, by = id)
    
    return(list(score_dat = as.data.frame(hfias_score_dat),
                bids_phenotype = as.data.frame(hfias_phenotype)))
  } else {
    return(list(score_dat = as.data.frame(hfias_score_dat)))
  }
}

