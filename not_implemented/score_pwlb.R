#' score_pwlb: Scored data from the Parent Weight-Loss Behavior Questionnaire
#'
#' This function scores the Parent Weight-Loss Behavior Questionnaire and provides subscale scores for the following behaviors: Healthy Weight Control and Unhelathy Weight Control.
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'pwlb#' where # is the question number (1-29; only 1-24 are needed to compute scale as 25-29 are contextual questions)
#' 3) Questions 1-24 must have the numeric value for the choices: 1 - Never, 2 - Rarely, 3 - Sometimes, 4 - Often, 5 - Always. Questions 25-29 are scored differently and are follow-up questions.
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Primary References for the Parent Weight-Loss Behavior Questionnaire and Scoring:
#' Savage JS, Birch LL. Patterns of weight control strategies predict differences in women’s 4 y weight gain. Obesity (Silver Spring). 2010;18(3):513-520. doi:10.1038/oby.2009.265 (\href{https://pubmed.ncbi.nlm.nih.gov/19696759/}{PubMed})
#'
#' Measure Adapted from:
#' French SA, Perry CL, Leon GR, Fulkerson JA. Dieting behaviors and weight change history in female adolescents. Health Psychology. 1995;14(6):548-555. doi:http://dx.doi.org/10.1037/0278-6133.14.6.548 (\href{https://pubmed.ncbi.nlm.nih.gov/8565929/}{PubMed})
#'
#' @param pwlb_data a data.frame all items for the Parent Weight-Loss Behavior Questionnaire following the naming conventions described above
#' @inheritParams fbs_intake
#'
#' @return A dataset with subscale scores for the Parent Weight-Loss Behavior Questionnaire
#' @examples
#'
#' # scoring for the pwlb with IDs
#' pwlb_score_data <- score_pwlb(pwlb_data, parID = 'ID')
#'
#'
#' \dontrun{
#' }
#'
#' @seealso Raw data from Qualtrics was processed using the following script: \code{\link{util_fbs_parent_v3dat}}
#'
#'
#' @export

score_pwlb <- function(pwlb_data, parID) {

    #### 1. Set up/initial checks #####

    # check that pwlb_data exist and is a data.frame
    data_arg <- methods::hasArg(pwlb_data)

    if (isTRUE(data_arg) & !is.data.frame(pwlb_data)) {
        stop("pwlb_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("pwlb_data must set to the data.frame with amount consumed for each food item")
    }

    # check if parID exists
    ID_arg <- methods::hasArg(parID)

    if (isTRUE(ID_arg)){
        if (!(parID %in% names(pwlb_data))) {
            stop("variable name entered as parID is not in pwlb_data")
        }
    }

    #### 2. Set Up Data #####

    # set up database for results create empty matrix
    pwlb_score_dat <- data.frame(pwlb_healthy = rep(NA, nrow(pwlb_data)), pwlb_unhealthy = rep(NA,
        nrow(pwlb_data)), pwlb_total = rep(NA, nrow(pwlb_data)))

    if (isTRUE(ID_arg)) {
        pwlb_score_dat <- data.frame(pwlb_data[[parID]], pwlb_score_dat)
        names(pwlb_score_dat)[1] <- parID
    }

    # set up labels for pwlb_score_dat
    pwlb_score_dat_labels <- lapply(pwlb_score_dat, function(x) attributes(x)$label)

    ## Score Subscales

    # Helathy
    healthy_vars <- c("pwlb1", "pwlb2", "pwlb3", "pwlb4", "pwlb5", "pwlb6", "pwlb7", "pwlb8", "pwlb10", "pwlb14",
                      "pwlb15")
    pwlb_score_dat[["pwlb_healthy"]] <- rowSums(pwlb_data[healthy_vars])

    ## add labels to data
    pwlb_score_dat_labels[["pwlb_healthy"]] <- "PWLB Healthy Weight-Loss Strategy Score"

    # Unhelathy
    unhealthy_vars <- c("pwlb9", "pwlb11", "pwlb12", "pwlb13", "pwlb16", "pwlb17", "pwlb19", "pwlb20", "pwlb23")
    pwlb_score_dat[["pwlb_unhealthy"]] <- rowSums(pwlb_data[unhealthy_vars])

    ## add labels to data
    pwlb_score_dat_labels[["pwlb_unhealthy"]] <- "PWLB Unhealthy Weight-Loss Strategy Score"

    ## Total
    pwlb_score_dat[["pwlb_total"]] <- rowSums(pwlb_data[c(healthy_vars, unhealthy_vars)])

    ## add labels to data
    pwlb_score_dat_labels[["pwlb_total"]] <- "PWLB Total Score"

    #### 3. Clean Export/Scored Data #####
    ## round data
    if (isTRUE(ID_arg)){
        pwlb_score_dat[2:ncol(pwlb_score_dat)] <- round(pwlb_score_dat[2:ncol(pwlb_score_dat)], digits = 3)
    } else {
        pwlb_score_dat <- round(pwlb_score_dat, digits = 3)
    }

    ## make sure the variable labels match in the dataset
    pwlb_score_dat = sjlabelled::set_label(pwlb_score_dat, label = matrix(unlist(pwlb_score_dat_labels,
        use.names = FALSE)))

    return(pwlb_score_dat)
}

