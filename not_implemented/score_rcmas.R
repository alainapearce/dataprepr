#' score_rcmas: Score data from the Revised Children's Manifest Anxiety Scale
#'
#' This function scores the Revised Children's Manifest Anxiety Scale for total score and the following subscales: Physiological Manifestations, Worry and Oversensitivity, and Fear/Concentration
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items and child grade. If child grade is not included some variables will be exported with NA's.
#' 2) The columns/variables must match the following naming convention: 'rcmas#' where # is the question number (1-37) and child's grade must be labeled 'grade'
#' 3) All questions must have the numeric value for the choice:
#' 0 - No, 1 - Yes
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Reynolds CR, Richmond BO. What I think and feel: A revised measure of children’s manifest anxiety. Journal of abnormal child psychology. 1978;6(2):271-280. (\href{https://pubmed.ncbi.nlm.nih.gov/670592/}{PubMed})
#'
#' Reynolds CR, Richmond BO. Factor structure and construct validity of’What I think and feel’: The revised children’s manifest anxiety scale. Journal of personality assessment. 1979;43(3):281-283. (\href{https://pubmed.ncbi.nlm.nih.gov/469706/}{PubMed})
#'
#' Stallard P, Velleman R, Langsford J, Baldwin S. Coping and psychological distress in children involved in road traffic accidents. British Journal of Clinical Psychology. 2001;40(2):197-208. (\href{https://pubmed.ncbi.nlm.nih.gov/11446241/}{PubMed})
#'
#' @param rcmas_data a data.frame all items for the Revised Children's Manifest Anxiety Scale following the naming conventions described above
#' @inheritParams fbs_intake
#'
#' @return A dataset with a score for the Revised Children's Manifest Anxiety Scale
#'
#' @examples
#'
#' # scoring for the rcmas with IDs
#' rcmas_score_data <- score_rcmas(rcmas_data, parID = 'ID')
#'
#' \dontrun{
#' }
#'
#' @seealso Raw data from Qualtrics was processed using the following script: \code{\link{util_fbs_child_v2dat}} and \code{\link{util_fbs_child_v2dat_home}}
#'
#'
#' @export

score_rcmas <- function(rcmas_data, parID) {

    #### 1. Set up/initial checks #####

    # check that rcmas_data exist and is a data.frame
    data_arg <- methods::hasArg(rcmas_data)

    if (isTRUE(data_arg) & !is.data.frame(rcmas_data)) {
        stop("rcmas_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("rcmas_data must set to the data.frame with amount consumed for each food item")
    }

    # check if parID exists
    ID_arg <- methods::hasArg(parID)

    if (isTRUE(ID_arg)){
        if (!(parID %in% names(rcmas_data))) {
            stop("variable name entered as parID is not in rcmas_data")
        }
    }

    # check for child grade
    if (('grade' %in% names(rcmas_data))) {
        grad_include <- TRUE
    } else {
        grad_include <- FALSE
        message("variable 'grade' is not in rcmas_data so not all variables will be computed")
    }

    #### 2. Set Up Data #####

    # set up database for results create empty matrix
    rcmas_score_dat <- data.frame(rcmas_phys = rep(NA, nrow(rcmas_data)), rcmas_worry = rep(NA, nrow(rcmas_data)), rcmas_concentration = rep(NA, nrow(rcmas_data)), rcmas_total = rep(NA, nrow(rcmas_data)), rcmas_total_normcat = rep(NA, nrow(rcmas_data)), rcmas_total_cutcat = rep(NA, nrow(rcmas_data)), rcmas_sd1  = rep(NA, nrow(rcmas_data)), rcmas_sd2  = rep(NA, nrow(rcmas_data)), rcmas_sd_total  = rep(NA, nrow(rcmas_data)), rcmas_sd_total_normcat  = rep(NA, nrow(rcmas_data)))

    if (isTRUE(ID_arg)) {
        rcmas_score_dat <- data.frame(rcmas_data[[parID]], rcmas_score_dat)
        names(rcmas_score_dat)[1] <- parID
    }

    # set up labels for rcmas_score_dat
    rcmas_score_dat_labels <- lapply(rcmas_score_dat, function(x) attributes(x)$label)

    ## Physiological Manifestation
    rcmas_phys_vars <- c("rcmas1", "rcmas5", "rcmas9", "rcmas13", "rcmas17", "rcmas21", "rcmas25", "rcmas29", "rcmas33")

    ## check number of NAs
    rcmas_score_dat[["rcmas_phys"]] <- rowSums(rcmas_data[rcmas_phys_vars])

    ## add labels to data
    rcmas_score_dat_labels[["rcmas_phys"]] <- "RCMAS Physiological Manifestation Score"

    ## Worrying and Oversensitivity
    rcmas_worry_vars <- c("rcmas2", "rcmas6", "rcmas10", "rcmas14", "rcmas18", "rcmas22", "rcmas26", "rcmas30", "rcmas34", "rcmas37")

    ## check number of NAs
    rcmas_score_dat[["rcmas_worry"]] <- rowSums(rcmas_data[rcmas_worry_vars])

    ## add labels to data
    rcmas_score_dat_labels[["rcmas_worry"]] <- "RCMAS Worrying and Oversensitivity Score"

    ## Concentration
    rcmas_concentration_vars <- c("rcmas3", "rcmas7", "rcmas11", "rcmas15", "rcmas19", "rcmas23", "rcmas27", "rcmas31", "rcmas35")

    ## check number of NAs
    rcmas_score_dat[["rcmas_concentration"]] <- rowSums(rcmas_data[rcmas_concentration_vars])

    ## add labels to data
    rcmas_score_dat_labels[["rcmas_concentration"]] <- "RCMAS Concentration Total Score"

    ## Total Score
    ## check number of NAs
    rcmas_score_dat[["rcmas_total"]] <- rowSums(rcmas_data[c(rcmas_phys_vars, rcmas_worry_vars, rcmas_concentration_vars)])

    ## add labels to data
    rcmas_score_dat_labels[["rcmas_total"]] <- "RCMAS Total Score"

    if (isTRUE(grad_include)){
        ## get category scale using norms
        rcmas_score_dat[["rcmas_total_normcat"]] <- ifelse(rcmas_data[["grade"]] == 1, ifelse(rcmas_score_dat[["rcmas_total"]] < 18.55, 0, 1), ifelse(rcmas_data[["grade"]] == 2, ifelse(rcmas_score_dat[["rcmas_total"]] < 22.52, 0, 1), ifelse(rcmas_data[["grade"]] == 3, ifelse(rcmas_score_dat[["rcmas_total"]] < 19.28, 0, 1), ifelse(rcmas_data[["grade"]] == 4, ifelse(rcmas_score_dat[["rcmas_total"]] < 22.34, 0, 1), ifelse(rcmas_data[["grade"]] == 5, ifelse(rcmas_score_dat[["rcmas_total"]] < 17.85, 0, 1), ifelse(rcmas_data[["grade"]] == 6, ifelse(rcmas_score_dat[["rcmas_total"]] < 19.1, 0, 1), ifelse(rcmas_data[["grade"]] == 7, ifelse(rcmas_score_dat[["rcmas_total"]] < 17.12, 0, 1), ifelse(rcmas_data[["grade"]] == 8, ifelse(rcmas_score_dat[["rcmas_total"]] < 19.72, 0, 1), ifelse(rcmas_data[["grade"]] == 9, ifelse(rcmas_score_dat[["rcmas_total"]] < 19.52, 0, 1), ifelse(rcmas_data[["grade"]] == 10, ifelse(rcmas_score_dat[["rcmas_total"]] < 19.08, 0, 1), ifelse(rcmas_data[["grade"]] == 11, ifelse(rcmas_score_dat[["rcmas_total"]] < 19.83, 0, 1), ifelse(rcmas_data[["grade"]] == 12, ifelse(rcmas_score_dat[["rcmas_total"]] < 18.25, 0, 1), NA))))))))))))

        rcmas_score_dat[["rcmas_total_normcat"]] <- sjlabelled::add_labels(rcmas_score_dat[["rcmas_total_normcat"]], labels = c(Typical = 0, `Clinical Interest` = 1))
        class(rcmas_score_dat[["rcmas_total_normcat"]]) <- c("haven_labelled", "vctrs_vctr", "double")

    } else {
        # no grade in data so cannot calculate normed categories
        rcmas_score_dat[["rcmas_total_normcat"]] <- NA
    }

    ## get category scale using cutoff score
    rcmas_score_dat[["rcmas_total_cutcat"]] <- ifelse(rcmas_score_dat[["rcmas_total"]] > 19, 1, 0)

    rcmas_score_dat[["rcmas_total_cutcat"]] <- sjlabelled::add_labels(rcmas_score_dat[["rcmas_total_cutcat"]], labels = c(Typical = 0, `Clinically Significant` = 1))
    class(rcmas_score_dat[["rcmas_total_cutcat"]]) <- c("haven_labelled", "vctrs_vctr", "double")


    ## add labels
    rcmas_score_dat_labels[["rcmas_total_normcat"]] <- "RCMAS Total Category Score Based on Grade Norms - Clinical Interest = score > 1SD above mean for grade; Reynolds & Richmond (1978)"

    rcmas_score_dat_labels[["rcmas_total_cutcat"]] <- "RCMAS Total Category Score Based on Grade Norms - Clinical Significant = score > 19; Stallard et al., (2001)"

    ## Social Desirability Scores
    rcmas_SD1_vars <- c("rcmas4", "rcmas8", "rcmas12", "rcmas16", "rcmas20", "rcmas24")

    ## scale 1
    rcmas_score_dat[["rcmas_sd1"]] <- rowSums(rcmas_data[rcmas_SD1_vars])

    rcmas_SD2_vars <- c("rcmas28", "rcmas32", "rcmas36")

    ## scale 2
    rcmas_score_dat[["rcmas_sd2"]] <- rowSums(rcmas_data[rcmas_SD2_vars])

    ## total
    rcmas_score_dat[["rcmas_sd_total"]] <- rowSums(rcmas_data[c(rcmas_SD1_vars, rcmas_SD2_vars)])

    ## add labels to data
    rcmas_score_dat_labels[["rcmas_sd1"]] <- "RCMAS Social Desirability Score 1"
    rcmas_score_dat_labels[["rcmas_sd2"]] <- "RCMAS Social Desirability Score 2"
    rcmas_score_dat_labels[["rcmas_sd_total"]] <- "RCMAS Social Desirability Total Score"


    ## get category scale using norms
    if (isTRUE(grad_include)){
        rcmas_score_dat[["rcmas_sd_total_normcat"]] <- ifelse(rcmas_data[["grade"]] == 1, ifelse(rcmas_score_dat[["rcmas_sd_total"]] < 7.95, 0, 1), ifelse(rcmas_data[["grade"]] == 2, ifelse(rcmas_score_dat[["rcmas_sd_total"]] < 7.18, 0, 1), ifelse(rcmas_data[["grade"]] == 3, ifelse(rcmas_score_dat[["rcmas_sd_total"]] < 6.15, 0, 1), ifelse(rcmas_data[["grade"]] == 4, ifelse(rcmas_score_dat[["rcmas_sd_total"]] < 3.9, 0, 1), ifelse(rcmas_data[["grade"]] == 5, ifelse(rcmas_score_dat[["rcmas_sd_total"]] < 5.17, 0, 1), ifelse(rcmas_data[["grade"]] == 6, ifelse(rcmas_score_dat[["rcmas_sd_total"]] < 6.22, 0, 1), ifelse(rcmas_data[["grade"]] == 7, ifelse(rcmas_score_dat[["rcmas_sd_total"]] < 3.6, 0, 1), ifelse(rcmas_data[["grade"]] == 8, ifelse(rcmas_score_dat[["rcmas_sd_total"]] < 4.44, 0, 1), ifelse(rcmas_data[["grade"]] == 9, ifelse(rcmas_score_dat[["rcmas_sd_total"]] < 5.54, 0, 1), ifelse(rcmas_data[["grade"]] == 10, ifelse(rcmas_score_dat[["rcmas_sd_total"]] < 6.16, 0, 1), ifelse(rcmas_data[["grade"]] == 11, ifelse(rcmas_score_dat[["rcmas_sd_total"]] < 6.43,  0, 1), ifelse(rcmas_data[["grade"]] == 12, ifelse(rcmas_score_dat[["rcmas_sd_total"]] < 5.93, 0, 1), NA))))))))))))

        rcmas_score_dat[["rcmas_sd_total_normcat"]] <- sjlabelled::add_labels(rcmas_score_dat[["rcmas_sd_total_normcat"]], labels = c(Typical = 0, Elevated = 1))
        class(rcmas_score_dat[["rcmas_sd_total_normcat"]]) <- c("haven_labelled", "vctrs_vctr", "double")

    } else {
        # no grade in data so cannot calculate normed categories
        rcmas_score_dat[["rcmas_sd_total_normcat"]] <- NA
    }

    rcmas_score_dat_labels[["rcmas_sd_total_normcat"]] <- "RCMAS Social Desirability Category Based on Grade Norms - Elevated = score > 1SD above mean for grade; Reynolds & Richmond (1978)"

    #### 3. Clean Export/Scored Data #####

    ## make sure the variable labels match in the dataset
    rcmas_score_dat = sjlabelled::set_label(rcmas_score_dat, label = matrix(unlist(rcmas_score_dat_labels, use.names = FALSE)))

    return(rcmas_score_dat)
}

