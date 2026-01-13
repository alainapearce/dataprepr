#' score_cchip: Scored data from the Community Childhood Hunger Identification Project
#'
#' This function scores the Community Childhood Hunger Identification Project and provides an overall CCHIP Score and Food Insecurity Status
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'cchip#' or 'cchip_#' where # is question number. For the Food and Brain Study, the 8 primary questions needed are: cchip1, cchip5, cchip9, cchip13, cchip17, cchip21, cchip25, and cchip29. Can be adapted to processes other data/studies in the future.
#' 3) The primary indicatory questions must have the following numeric values: Yes - 1, No - 0
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Wehler CA, Scott RI, Anderson JJ. The community childhood hunger identification project: A model of domestic hunger—Demonstration project in Seattle, Washington. Journal of Nutrition Education. 1992;24(1):29S-35S. doi:10.1016/S0022-3182(12)80135-X
#'
#' @param cchip_data a data.frame all items for the Community Childhood Hunger Identification Project following the naming conventions described above
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @param extra_scale_cols a vector of character strings that begin with 'cchip' but are not scale items. Any columns in cchip_data that begin with 'cchip' but are not scale items must be included here. Default is empty vector.
#'
#' @return A dataset with scores for the Community Childhood Hunger Identification Project
#' @examples
#'
#' # scoring for the cchip with IDs
#' cchip_score_data <- score_cchip(cchip_data, id = 'ID')
#'
#'
#' \dontrun{
#' }
#'
#'
#' @export

score_cchip <- function(cchip_data, pna_value, id, session_id, extra_scale_cols = c()) {
  
  #### 1. Set up/initial checks #####
  
  # check that cchip_data exist and is a data.frame
  data_arg <- methods::hasArg(cchip_data)
  
  if (isTRUE(data_arg) & !is.data.frame(cchip_data)) {
    stop("cchip_data must be entered as a data.frame")
  } else if (isFALSE(data_arg)) {
    stop("cchip_data must set to the data.frame with amount consumed for each food item")
  }
  
  # check if parID exists
  ID_arg <- methods::hasArg(id)
  
  if (isTRUE(ID_arg)){
    if (!(id %in% names(cchip_data))) {
      stop("variable name entered as id is not in cchip_data")
    }
  }
  
  # check if session_id exists
  sessionID_arg <- methods::hasArg(session_id)
  
  if (isTRUE(sessionID_arg)){
    if (!(id %in% names(cchip_data))) {
      stop("variable name entered as session_id is not in cchip_data")
    }
  }
  
  #### 2. Set Up Data #####
  
  # set up database for results create empty matrix
  cchip_score_dat <- data.frame(cchip_total = rep(NA, nrow(cchip_data)), cchip_category = rep(NA, nrow(cchip_data)))
  
  if (isTRUE(ID_arg)) {
    if (isTRUE(sessionID_arg)) {
      cchip_score_dat <- data.frame(cchip_data[[id]], cchip_data[[session_id]], cchip_score_dat)
      names(cchip_score_dat)[1:2] <- c(id, session_id)
    } else {
      cchip_score_dat <- data.frame(cchip_data[[id]], cchip_score_dat)
      names(cchip_score_dat)[1] <- id
    }
  }
  
  # assign cchip scale items to cchip_items, excluding columns in extra_scale_cols
  cchip_items <- setdiff(grep("^cchip", names(cchip_data), value = TRUE), extra_scale_cols)
  
  # remove underscore in column names for cchip_items
  names(cchip_data)[names(cchip_data) %in% cchip_items] <- gsub('cchip_', 'cchip', names(cchip_data)[names(cchip_data) %in% cchip_items])
  
  # if pna_value arg, replace not applicable values with NA
  if (isTRUE(methods::hasArg(pna_value))) {
    
    # replace pna_value with NA in pcw_vars
    cchip_data[cchip_items] <- lapply(cchip_data[cchip_items] , function(x) ifelse(x == pna_value, NA, x))
    
  }
  
  ## Score Subscales
  # CCHIP score
  cchip_vars <- c('cchip1', 'cchip5', 'cchip9', 'cchip13', 'cchip17', 'cchip21', 'cchip25', 'cchip29')
  cchip_score_dat[["cchip_total"]] <- rowSums(cchip_data[cchip_vars], na.rm = TRUE)
  
  # Food Insecurity Category
  cchip_score_dat[["cchip_category"]] <- ifelse(cchip_score_dat[["cchip_total"]] >= 5, 2, ifelse(cchip_score_dat[["cchip_total"]] >= 1, 1, 0))
  
  # add levels
  cchip_score_dat[["cchip_category"]] <- factor(cchip_score_dat[["cchip_category"]], levels = c(2, 1, 0), labels = c("Hungry", "At Risk for Hunger", "Not Hungry"))
  
  #### 3. Clean Export/Scored Data #####

  
  ## merge raw responses with scored data
  if (isTRUE(ID_arg)){
    if (isTRUE(sessionID_arg)) {
      cchip_phenotype <- merge(cchip_data, cchip_score_dat, by = c(id, session_id))
    } else {
      cchip_phenotype <- merge(cchip_data, cchip_score_dat, by = id)
    }
    
    return(list(score_dat = as.data.frame(cchip_score_dat),
                bids_phenotype = as.data.frame(cchip_phenotype)))
  } else {
    return(list(score_dat = as.data.frame(cchip_score_dat)))
  }
}
