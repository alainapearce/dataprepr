#' score_ffbs: Scored data from the Family Food Behavior Survey
#'
#' This function scores the Family Food Behavior Survey and provides subscale scores for the following behaviors: Maternal Control, Maternal Presences, Child Choice, and Organization
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'ffbs#' or 'ffbs_#' where # is the question number (1-20)
#' 3) All questions must have the numeric value for the choice: 0 - Never True, 1 - Rarely True, 2 - Sometimes, 3 - Often True, 4 - Always True
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Baughcum, A. E., Powers, S. W., Johnson, S. B., Chamberlin, L. A., Deeks, C. M., Jain, A., & Whitaker, R. C. (2001). Maternal Feeding Practices and Beliefs and Their Relationships to Overweight in Early Childhood: Journal of Developmental & Behavioral Pediatrics, 22(6), 391–408. https://doi.org/10.1097/00004703-200112000-00007 (\href{https://pubmed.ncbi.nlm.nih.gov/11773804/}{PubMed})
#'
#' McCurdy, K., & Gorman, K. S. (2010). Measuring family food environments in diverse families with young children. Appetite, 54(3), 615–618. https://doi.org/10.1016/j.appet.2010.03.004 (\href{https://pubmed.ncbi.nlm.nih.gov/20227449/}{PubMed})
#'
#' @param ffbs_data a data.frame all items for the Family Food Behavior Survey following the naming conventions described above
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @inheritParams score_bes
#' @param extra_scale_cols a vector of character strings that begin with 'ffbs' but are not scale items. Any columns in ffbs_data that begin with 'ffbs' but are not scale items must be included here. Default is empty vector.
#'
#' @return A dataset with subscale scores for the Family Food Behavior Survey
#' @examples
#'
#' # scoring for the ffbs with IDs
#' ffbs_score_data <- score_ffbs(ffbs_data, id = 'ID', base_zero = TRUE)
#'
#' \dontrun{
#' }
#'
#'
#' @export

score_ffbs <- function(ffbs_data, pna_value, base_zero = TRUE, id, session_id, extra_scale_cols = c()) {

    #### 1. Set up/initial checks #####

    # check that ffbs_data exist and is a data.frame
    data_arg <- methods::hasArg(ffbs_data)

    if (isTRUE(data_arg) & !is.data.frame(ffbs_data)) {
        stop("ffbs_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("ffbs_data must set to a data.frame")
    }

    # check if id exists
    ID_arg <- methods::hasArg(id)

    if (isTRUE(ID_arg)){
        if (!(id %in% names(ffbs_data))) {
            stop("variable name entered as id is not in ffbs_data")
        }
    }
    
    # check if session_id exists
    sessionID_arg <- methods::hasArg(session_id)
    
    if (isTRUE(sessionID_arg)){
      if (!(id %in% names(ffbs_data))) {
        stop("variable name entered as session_id is not in ffbs_data")
      }
    }
    
    # check base_zero is logical
    if (!is.logical(base_zero)) {
      stop("base_zero arg must be logical (TRUE/FALSE)")
    }

    #### 2. Set Up Data #####

    # set up database for results create empty matrix
    ffbs_score_dat <- data.frame(ffbs_control = rep(NA, nrow(ffbs_data)), ffbs_presence = rep(NA,
        nrow(ffbs_data)), ffbs_ch_choice = rep(NA, nrow(ffbs_data)), ffbs_org = rep(NA,
        nrow(ffbs_data)))

    if (isTRUE(ID_arg)) {
      if (isTRUE(sessionID_arg)) {
        ffbs_score_dat <- data.frame(ffbs_data[[id]], ffbs_data[[session_id]], ffbs_score_dat)
        names(ffbs_score_dat)[1:2] <- c(id, session_id)
      } else {
        ffbs_score_dat <- data.frame(ffbs_data[[id]], ffbs_score_dat)
        names(ffbs_score_dat)[1] <- id
      }
    }
    
    # assign ffbs scale items to ffbs_items, excluding columns in extra_scale_cols
    ffbs_items <- setdiff(grep("^ffbs", names(ffbs_data), value = TRUE), extra_scale_cols)
    
    # remove underscore in column names for ffbs_items
    names(ffbs_data)[names(ffbs_data) %in% ffbs_items] <- gsub('ffbs_', 'ffbs', names(ffbs_data)[names(ffbs_data) %in% ffbs_items])
    
    # remove underscore in ffbs_items
    ffbs_items <- gsub("ffbs_", "ffbs", ffbs_items)
    
    # if pna_value arg, replace not applicable values with NA
    if (isTRUE(methods::hasArg(pna_value))) {
      
      # replace pna_value with NA in pcw_vars
      ffbs_data[ffbs_items] <- lapply(ffbs_data[ffbs_items] , function(x) ifelse(x == pna_value, NA, x))
      
    }
    
    # re-scale data
    ffbs_data_edit <- ffbs_data
    
    if (isFALSE(base_zero)){
      ffbs_data_edit[ffbs_items] <- sapply(ffbs_items, function(x) ffbs_data[[x]] - 1, simplify = TRUE)
    }

    # calculate reversed scores
    reverse_qs <- c("ffbs1", "ffbs5", "ffbs12")

    for (var in 1:length(reverse_qs)) {
        var_name <- reverse_qs[var]

        ffbs_data_edit[[var_name]] <-
          ifelse(is.na(ffbs_data_edit[[var_name]]),
                 NA,
                 ifelse(ffbs_data_edit[[var_name]] == 0, 4, ifelse(
                   ffbs_data_edit[[var_name]] ==
                     1, 3, ifelse(ffbs_data_edit[[var_name]] == 3, 1, ifelse(
                       ffbs_data_edit[[var_name]] ==
                         4, 0,
                       ifelse(ffbs_data_edit[[var_name]] ==
                                2, 2, NA)
                     ))
                 )))
    }

    ## Score Subscales

    # Maternal Control
    cont_vars <- c("ffbs5", "ffbs6", "ffbs8", "ffbs11", "ffbs17")
    ffbs_score_dat[["ffbs_control"]] <- rowSums(ffbs_data_edit[cont_vars])

    # Maternal Presence
    presence_vars <- c("ffbs10", "ffbs12", "ffbs14", "ffbs15", "ffbs20")
    ffbs_score_dat[["ffbs_presence"]] <- rowSums(ffbs_data_edit[presence_vars])

    # Child Choice
    choice_vars <- c("ffbs1", "ffbs3", "ffbs9", "ffbs13", "ffbs16")
    ffbs_score_dat[["ffbs_ch_choice"]] <- rowSums(ffbs_data_edit[choice_vars])

    # Organization
    org_vars <- c("ffbs2", "ffbs4", "ffbs7", "ffbs18", "ffbs19")
    ffbs_score_dat[["ffbs_org"]] <- rowSums(ffbs_data_edit[org_vars])

    #### 3. Clean Export/Scored Data #####
    ## merge raw responses with scored data
    if (isTRUE(ID_arg)){
      if (isTRUE(sessionID_arg)) {
        ffbs_phenotype <- merge(ffbs_data, ffbs_score_dat, by = c(id, session_id))
      } else {
        ffbs_phenotype <- merge(ffbs_data, ffbs_score_dat, by = id)
      }
      
      return(list(score_dat = as.data.frame(ffbs_score_dat),
                  bids_phenotype = as.data.frame(ffbs_phenotype)))
    } else {
      return(list(score_dat = as.data.frame(ffbs_score_dat)))
    }
}

