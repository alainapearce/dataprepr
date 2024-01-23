#' score_audit: Score data from the Alcohol Use Disorders Identification Test
#'
#' This function scores the Alcohol Use Disorders Identification Test
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'audit#' where # is the question number (1-10)
#' 3) All questions must have the numeric value for the choice:
#' 3a) question 1: 0 - Never, 1 - Monthly or Less, 2 - 2-4 times a month, 3 - 2-3 times a week, 4 - 4 or more times a week
#' 3b) question 2: 0 - 1 or 2, 1 - 3 or 4, 2 - 5 or 6, 3 - 7 to 9, 4 - 10
#' 3c) question 3-8: 0 - Never, 1 - Less than Monthly, 2 - Monthly, 3 - Weekly, 4 - Daily or Almost Daily
#' 3d) questions 9-10: 0 - No, 2 - Yes, but not in the last year, 4 - Yes, during the last year
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Saunders JB, Aasland OG, Babor TF, De La Fuente JR, Grant M. Development of the Alcohol Use Disorders Identification Test (AUDIT): WHO Collaborative Project on Early Detection of Persons with Harmful Alcohol Consumption-II. Addiction. 1993;88(6):791-804. doi:10.1111/j.1360-0443.1993.tb02093.x (\href{https://pubmed.ncbi.nlm.nih.gov/8329970/}{PubMed})
#'
#' @param audit_data a data.frame all items for the Alcohol Use Disorders Identification Test following the naming conventions described above
#' @inheritParams fbs_intake
#'
#' @return A dataset with a score for the Alcohol Use Disorders Identification Test
#' @examples
#'
#' # scoring for the audit with IDs
#' audit_score_data <- score_audit(audit_data, parID = 'ID')
#'
#' \dontrun{
#' }
#'
#' @seealso Raw data from Qualtrics was processed using the following script: \code{\link{util_fbs_parent_v5dat}}
#'
#'
#' @export

score_audit <- function(audit_data, parID) {

    #### 1. Set up/initial checks #####

    # check that audit_data exist and is a data.frame
    data_arg <- methods::hasArg(audit_data)

    if (isTRUE(data_arg) & !is.data.frame(audit_data)) {
        stop("audit_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("audit_data must set to the data.frame with amount consumed for each food item")
    }

    # check if parID exists
    ID_arg <- methods::hasArg(parID)

    if (isTRUE(ID_arg)){
        if (!(parID %in% names(audit_data))) {
            stop("variable name entered as parID is not in audit_data")
        }
    }

    #### 2. Set Up Data #####

    # set up database for results create empty matrix
    audit_score_dat <- data.frame(audit_total = rep(NA, nrow(audit_data)), audit_cat = rep(NA, nrow(audit_data)))

    if (isTRUE(ID_arg)) {
        audit_score_dat <- data.frame(audit_data[[parID]], audit_score_dat)
        names(audit_score_dat)[1] <- parID
    }

    # set up labels for audit_score_dat
    audit_score_dat_labels <- lapply(audit_score_dat, function(x) attributes(x)$label)


    ## Total Score
    audit_vars <- c("audit1", "audit2", "audit3", "audit4", "audit5", "audit6", "audit7", "audit7", "audit8", "audit9", "audit10")
    audit_score_dat[["audit_total"]] <- rowSums(audit_data[audit_vars])

    audit_score_dat[["audit_cat"]] <- ifelse(is.na(audit_score_dat[["audit_total"]]), NA, ifelse(audit_score_dat[["audit_total"]] >=8, 1, 0))

    audit_score_dat[["audit_cat"]] <- sjlabelled::add_labels(audit_score_dat[["audit_cat"]], labels = c(`Not Harmful Consumption` = 0, `Likely Harmful Consumption` = 1))
    class(audit_score_dat[["audit_cat"]]) <- c("haven_labelled", "vctrs_vctr", "double")

    ## add labels to data
    audit_score_dat_labels[["audit_total"]] <- "AUDIT Total Score"
    audit_score_dat_labels[["audit_cat"]] <- "AUDIT Total Score Categorization"

    #### 3. Clean Export/Scored Data #####

    ## make sure the variable labels match in the dataset
    audit_score_dat = sjlabelled::set_label(audit_score_dat, label = matrix(unlist(audit_score_dat_labels,
        use.names = FALSE)))

    return(audit_score_dat)
}

