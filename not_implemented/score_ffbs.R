#' score_ffbs: Scored data from the Family Food Behvior Survey
#'
#' This function scores the Family Food Behvior Survey and provides subscale scores for the following behaviors: Maternal Control, Maternal Presences, Child Choice, and Organization
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'ffbs#' where # is the question number (1-20)
#' 3) All questions must have the numeric value for the choice: 0 - Never True, 1 - Rarely True, 2 - Sometimes, 3 - Often True, 4 - Always True
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Baughcum, A. E., Powers, S. W., Johnson, S. B., Chamberlin, L. A., Deeks, C. M., Jain, A., & Whitaker, R. C. (2001). Maternal Feeding Practices and Beliefs and Their Relationships to Overweight in Early Childhood: Journal of Developmental & Behavioral Pediatrics, 22(6), 391–408. https://doi.org/10.1097/00004703-200112000-00007 (\href{https://pubmed.ncbi.nlm.nih.gov/11773804/}{PubMed})
#'
#' McCurdy, K., & Gorman, K. S. (2010). Measuring family food environments in diverse families with young children. Appetite, 54(3), 615–618. https://doi.org/10.1016/j.appet.2010.03.004 (\href{https://pubmed.ncbi.nlm.nih.gov/20227449/}{PubMed})
#'
#' @param ffbs_data a data.frame all items for the Family Food Behvior Survey following the naming conventions described above
#' @inheritParams fbs_intake
#'
#' @return A dataset with subscale scores for the Family Food Behavior Survey
#' @examples
#'
#' # scoring for the ffbs with IDs
#' ffbs_score_data <- score_ffbs(ffbs_data, parID = 'ID')
#'
#' \dontrun{
#' }
#'
#' @seealso Raw data from Qualtrics was processed using the following script: \code{\link{util_fbs_parent_v2dat}}
#'
#'
#' @export

score_ffbs <- function(ffbs_data, parID) {

    #### 1. Set up/initial checks #####

    # check that ffbs_data exist and is a data.frame
    data_arg <- methods::hasArg(ffbs_data)

    if (isTRUE(data_arg) & !is.data.frame(ffbs_data)) {
        stop("ffbs_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("ffbs_data must set to the data.frame with amount consumed for each food item")
    }

    # check if parID exists
    ID_arg <- methods::hasArg(parID)

    if (isTRUE(ID_arg)){
        if (!(parID %in% names(ffbs_data))) {
            stop("variable name entered as parID is not in ffbs_data")
        }
    }

    #### 2. Set Up Data #####

    # set up database for results create empty matrix
    ffbs_score_dat <- data.frame(ffbs_control = rep(NA, nrow(ffbs_data)), ffbs_presence = rep(NA,
        nrow(ffbs_data)), ffbs_ch_choice = rep(NA, nrow(ffbs_data)), ffbs_org = rep(NA,
        nrow(ffbs_data)))

    if (isTRUE(ID_arg)) {
        ffbs_score_dat <- data.frame(ffbs_data[[parID]], ffbs_score_dat)
        names(ffbs_score_dat)[1] <- parID
    }

    # set up labels for ffbs_score_dat
    ffbs_score_dat_labels <- lapply(ffbs_score_dat, function(x) attributes(x)$label)

    # calculate reversed scores

    reverse_qs <- c("ffbs1", "ffbs5", "ffbs12")

    for (var in 1:length(reverse_qs)) {
        var_name <- reverse_qs[var]
        reverse_name <- paste0(var_name, "_rev")

        ffbs_data[[reverse_name]] <- ifelse(is.na(ffbs_data[[var_name]]), NA,
            ifelse(ffbs_data[[var_name]] == 0, 4, ifelse(ffbs_data[[var_name]] ==
                1, 3, ifelse(ffbs_data[[var_name]] == 3, 1, ifelse(ffbs_data[[var_name]] ==
                4, 0, 2)))))
    }

    ## Score Subscales

    # Maternal Control
    cont_vars <- c("ffbs5_rev", "ffbs6", "ffbs8", "ffbs11", "ffbs17")
    ffbs_score_dat[["ffbs_control"]] <- rowSums(ffbs_data[cont_vars])

    ## add labels to data
    ffbs_score_dat_labels[["ffbs_control"]] <- "FFBS Maternal Control Total Score"

    # Maternal Presence
    presence_vars <- c("ffbs10", "ffbs12_rev", "ffbs14", "ffbs15", "ffbs20")
    ffbs_score_dat[["ffbs_presence"]] <- rowSums(ffbs_data[presence_vars])

    ## add labels to data
    ffbs_score_dat_labels[["ffbs_presence"]] <- "FFBS Maternal Presence Total Score"

    # Child Choice
    choice_vars <- c("ffbs1_rev", "ffbs3", "ffbs9", "ffbs13", "ffbs16")
    ffbs_score_dat[["ffbs_ch_choice"]] <- rowSums(ffbs_data[choice_vars])

    ## add labels to data
    ffbs_score_dat_labels[["ffbs_ch_choice"]] <- "FFBS Child Choice Total Score"

    # Organization
    org_vars <- c("ffbs2", "ffbs4", "ffbs7", "ffbs18", "ffbs19")
    ffbs_score_dat[["ffbs_org"]] <- rowSums(ffbs_data[org_vars])

    ## add labels to data
    ffbs_score_dat_labels[["ffbs_org"]] <- "FFBS Organization Total Score"

    #### 3. Clean Export/Scored Data #####
    ## round data
    if (isTRUE(ID_arg)){
        ffbs_score_dat[2:ncol(ffbs_score_dat)] <- round(ffbs_score_dat[2:ncol(ffbs_score_dat)], digits = 3)
    } else {
        ffbs_score_dat <- round(ffbs_score_dat, digits = 3)
    }

    ## make sure the variable labels match in the dataset
    ffbs_score_dat = sjlabelled::set_label(ffbs_score_dat, label = matrix(unlist(ffbs_score_dat_labels,
        use.names = FALSE)))

    return(ffbs_score_dat)
}

