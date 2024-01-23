#' score_risk: Determine Risk Status of Child
#'
#' This function determines the risk status of children based on parent BMI. Low risk: both parents BMI < 25; High risk: Mom BMI >= 30 and Dad BMI >=25. An error of +/- 0.5 BMI is allowed but only 1 parent can have BMI = 25.5 for Low Risk.
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include who completed the questionnaire (i.e., the respondent), the measured parent BMI, and both parents' reported BMIs.The respondent will have measured BMI at Visit 1 and will self-report the height and weight of the other parent.
#' 2) The BMI columns/variables must match the following naming conventions - 'parent_bmi', 'sr_mom_bmi', and 'sr_dad_bmi'
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @param risk_data a data.frame all parent BMI data following the naming conventions described above
#' @param respondent a string indicating the variable name for the parent who completed the questionnaires and had height/weight measured at Visit 1
#' @return A dataset with amount of MVPA calculated for each day and week. If sleep = TRUE, parent-reported sleep will also be included
#'
#' @examples
#'
#' # risk categories
#' risk_score <- score_risk(risk_data, respondent = 'parent_respondent', parID = 'ID')
#'
#' \dontrun{
#' }
#'
#' @seealso Raw data from Qualtrics was processed using the following script: \code{\link{util_fbs_parent_v1dat}}, \code{\link{util_merge_v1}}
#'
#'
#' @export

score_risk <- function(risk_data, respondent, parID) {

    #### 1. Set up/initial checks #####

    # check that risk_data exist and is a data.frame
    data_arg <- methods::hasArg(risk_data)

    if (isTRUE(data_arg) & !is.data.frame(risk_data)) {
        stop("risk_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("risk_data must set to the data.frame with amount consumed for each food item")
    }

    # check that study exists and is a string
    resp_arg <- methods::hasArg(respondent)

    if (isTRUE(resp_arg)) {
        if (!is.character(respondent)){
            stop("the variable name in the data set that indicates the parent respondent must be entered as a string")
        } else {
            mom_strs <- c('mom', 'Mom', 'mother', 'Mother')
            dad_strs <- c('dad', 'Dad', 'Father', 'father')

            #convert from numbers to strings for respondent if haven/labeled data
            if(class(risk_data[[respondent]])[1] == 'haven_labelled'){
                risk_data[[respondent]] <- sjlabelled::as_label(risk_data[[respondent]])
            }

            risk_data[[respondent]] <- ifelse(is.na(risk_data[[respondent]]), NA, ifelse(risk_data[[respondent]] %in% mom_strs, 'mom', ifelse(risk_data[[respondent]] %in% dad_strs, 'dad', NA)))

            #get number of NA's and report to console
            nNA <- sum(is.na(risk_data[[respondent]]))

            if (nNA == nrow(risk_data)){
                message('All participants had missing data or an entry for respondent that did not match available options: mom, Mom, mother, Mother, dad, Dad, father, Father')
            } else if (nNA > 0){
                message(paste0(nNA, '/', nrow(risk_data), ' participants had missing data or an entry for respondent that did not match available options: mom, Mom, mother, Mother, dad, Dad, father, Father'))
            }
        }
    } else {
        stop("the variable name in the data set that indicates the parent respondent must be entered as a string")
    }

    # check if parID exists
    ID_arg <- methods::hasArg(parID)

    if (isTRUE(ID_arg)){
        if (!(parID %in% names(risk_data))) {
            stop("variable name entered as parID is not in risk_data")
        }
    }

    #### 2. Set Up Data #####

    risk_score_dat <- data.frame(hw_measured = rep(NA, nrow(risk_data)), measured_parent = rep(NA, nrow(risk_data)), risk_status_mom = rep(NA, nrow(risk_data)), risk_status_both = rep(NA, nrow(risk_data)))

    if (isTRUE(ID_arg)) {
        risk_score_dat <- data.frame(risk_data[[parID]], risk_score_dat)
        names(risk_score_dat)[1] <- parID
    }

    # set up labels for risk_score_dat
    risk_score_dat_labels <- lapply(risk_score_dat, function(x) attributes(x)$label)

    # measured BMI
    risk_score_dat[['hw_measured']] <- ifelse(is.na(risk_data[['parent_bmi']]), 0, 1)

    risk_score_dat[['hw_measured']] <- sjlabelled::set_labels(risk_score_dat[['hw_measured']], labels = c(No = 0, Yes = 1))
    class(risk_score_dat[["hw_measured"]]) <- c("haven_labelled", "vctrs_vctr", "double")

    risk_score_dat_labels[['hw_measured']] <- "Parent attending Visit 1 had measured height and weight"

    risk_score_dat[['measured_parent']] <- ifelse(is.na(risk_data[['parent_bmi']]), NA, ifelse(risk_data[[respondent]] %in% mom_strs, 0, 1))

    risk_score_dat[['measured_parent']] <- sjlabelled::set_labels(risk_score_dat[['measured_parent']], labels = c(mom = 0, dad = 1))
    class(risk_score_dat[["measured_parent"]]) <- c("haven_labelled", "vctrs_vctr", "double")

    risk_score_dat_labels[['measured_parent']] <- 'Parent with measured BMI at Visit 1'

    # risk status - Mom only
    risk_score_dat[['risk_status_mom']] <- ifelse(risk_score_dat[['hw_measured']] == 0, ifelse(is.na(risk_data[['sr_mom_bmi']]), NA, ifelse(risk_data[['sr_mom_bmi']] >= 29, 1, ifelse(risk_data[['sr_mom_bmi']] < 26, 0, 2))), ifelse(risk_data[[respondent]] == 'mom', ifelse(risk_data[['parent_bmi']] >= 29, 1, ifelse(risk_data[['parent_bmi']] < 26, 0, 2)), ifelse(risk_data[['sr_mom_bmi']] >= 29, 1, ifelse(risk_data[['sr_mom_bmi']] < 26, 0, 2))))

    risk_score_dat[['risk_status_mom']] <- sjlabelled::set_labels(risk_score_dat[['risk_status_mom']], labels = c(`Low Risk` = 0, `High Risk` = 1, Neither = 2))
    class(risk_score_dat[["risk_status_mom"]]) <- c("haven_labelled", "vctrs_vctr", "double")

    risk_score_dat_labels[['risk_status_mom']] <- "Child risk categor: Low risk: Mom BMI < 26, High Risk: Mom BMI >= 29"

    # risk status - Both
    risk_score_dat[['risk_status_both']] <- ifelse(risk_score_dat[['hw_measured']] == 0, ifelse(is.na(risk_data[['sr_mom_bmi']]) | is.na(risk_data[['sr_dad_bmi']]), NA, ifelse(risk_data[['sr_mom_bmi']] >= 30 & risk_data[['sr_dad_bmi']] >= 25, 1, ifelse(risk_data[['sr_mom_bmi']] < 25 & risk_data[['sr_dad_bmi']] < 25, 0, 2))), ifelse(risk_data[[respondent]] == 'mom', ifelse(risk_data[['parent_bmi']] >= 30 & risk_data[['sr_dad_bmi']] >= 25, 1, ifelse(risk_data[['parent_bmi']] < 25 & risk_data[['sr_dad_bmi']] < 25, 0, 2)), ifelse(risk_data[['sr_mom_bmi']] >= 30 & risk_data[['parent_bmi']] >= 25, 1, ifelse(risk_data[['sr_mom_bmi']] < 25 & risk_data[['parent_bmi']] < 25, 0, 2))))

    # special case of no Mom so base only on dad
    risk_score_dat[['risk_status_both']] <- ifelse(risk_score_dat[['hw_measured']] == 0, ifelse(is.na(risk_data[['sr_mom_bmi']]), ifelse(risk_data[['sr_mom_bmi']] < 25, 0, 1), risk_score_dat[['risk_status_both']]), ifelse(risk_data[[respondent]] == 'dad', ifelse(is.na(risk_data[['sr_mom_bmi']]), ifelse(risk_data[['parent_bmi']] < 25, 0, 1), risk_score_dat[['risk_status_both']]), risk_score_dat[['risk_status_both']]))

    risk_score_dat[['risk_status_both']] <- sjlabelled::set_labels(risk_score_dat[['risk_status_both']], labels = c(`Low Risk` = 0, `High Risk` = 1, Neither = 2))
    class(risk_score_dat[["risk_status_both"]]) <- c("haven_labelled", "vctrs_vctr", "double")

    risk_score_dat_labels[['risk_status_both']] <- "Child risk category: Low Risk: Mom and Dad BMI < 25, High Risk: Mom BMI >=30 and Dad rounded BMI >= 25"

    #### 3. Clean Export/Scored Data #####

    ## make sure the variable labels match in the dataset
    risk_score_dat = sjlabelled::set_label(risk_score_dat, label = matrix(unlist(risk_score_dat_labels, use.names = FALSE)))

    return(risk_score_dat)
}
