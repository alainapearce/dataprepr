#' util_fbs_merge_v1: Merge all Visit 1 raw Qualtrics databases and organize variable in database order
#'
#' This function merges the following visit 1 raw data into a single database and organizes variables in database order: child visit 1, child visit 1-home, child visit 1-lab, and parent visit 1
#'
#' The databases MUST follow the naming convention: Child_V1_YYYY-MM-DD.sav, Child_V1_Home_YYY-MM-DD.sav, Child_V1_Lab_YYY-MM-DD.sav, and Parent_V1_YYY-MM-DD.sav. The databases must all be in the SAME directory to be processed if the data_path is not entered and the directory organization does not follow the structure laid out in the DataManual.
#'
#' @param child_file_pattern string with the pattern to search for to find the raw child data files. The pattern must contain respondent and visit number (e.g., for files from child visit 1, would enter 'Child_V1').
#' @param parent_file_pattern string with the pattern to search for to find the raw parent data files. The pattern must contain respondent and visit number (e.g., for files from parent visit 1, would enter 'Parent_V1').
#' @inheritParams util_fbs_parent_v1dat
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from parent visit 1 Qualtrics; 2) dict: all variable descriptions; 3) pna_data: data.frame marking participants who 'preferred not to answer' (pna) specific questions; and 4) pna_dict: all variable descriptions for pna_data
#'
#' @examples
#' #if in same working directory as data with all data. Note - there is no need to add the COVID protocol (i.e., 'Home' or 'Lab') to file_pattern - these files will be search for automatically:
#' v1dat_scored <- util_fbs_merge_v1(child_file_pattern = 'Child_V1', parent_file_pattern = 'Parent_V1')
#'
#' \dontrun{
#' #child_file_pattern and parent_file_pattern must be a strings. The following will not run:
#' v1dat_scored <- util_fbs_merge_v1(child_file_pattern = Child_V1, parent_file_pattern = Parent_V1)
#'
#' # *_file_pattern must have the respondent ('Child') and visit number ('V1'). If just enter 'Child' or 'Parent', the script will not run because it will return multiple files for different child visits. The following will not run:
#' v1dat_scored <- util_fbs_merge_v1(child_file_pattern = 'Child', parent_file_pattern = 'Parent')
#' }
#'
#' @seealso Raw data from Qualtrics is processed using the following scripts: \code{\link{util_fbs_child_v1dat}}, \code{\link{util_fbs_child_v1dat_home}}, \code{\link{util_fbs_child_v1dat_lab}}, \code{\link{util_fbs_parent_v1dat}}. Visit 1 data is scored using the following scripts: \code{\link{score_pds}}, \code{\link{score_paq}}, \code{\link{score_risk}}
#'
#'
#' @export

util_fbs_merge_v1 <- function(child_file_pattern, parent_file_pattern, data_path) {

    #### 1. Set up/initial checks #####
    # check that file_pattern exist and is a string

    c_filepat_arg <- methods::hasArg(child_file_pattern)

    if (isTRUE(c_filepat_arg) & !is.character(child_file_pattern)) {
        stop("child_file_pattern must be entered as a string: e.g., 'Child_V1'")
    } else if (isFALSE(c_filepat_arg)) {
        stop("child_file_pattern must set to the a string matching the name of the raw data file for child visit: e.g., 'Child_V1'")
    }

    p_filepat_arg <- methods::hasArg(parent_file_pattern)

    if (isTRUE(p_filepat_arg) & !is.character(parent_file_pattern)) {
        stop("parent_file_pattern must be entered as a string: e.g., 'Parent_V1'")
    } else if (isFALSE(p_filepat_arg)) {
        stop("parent_file_pattern must set to the a string matching the name of the raw data file for child visit: e.g., 'Parent_V1'")
    }

    # check datapath
    datapath_arg <- methods::hasArg(data_path)

    if (isTRUE(datapath_arg)) {
        if (!is.character(data_path)) {
            stop("data_path must be entered as a string: e.g., '.../Participant_Data/untouchedRaw/'")
        }
    }

    #### 2. Process Raw Data #####

    if (isTRUE(datapath_arg)) {
        child_v1dat <- util_fbs_child_v1dat(child_file_pattern, data_path)
        child_home_v1dat <- util_fbs_child_v1dat_home(child_file_pattern, data_path)
        child_lab_v1dat <- util_fbs_child_v1dat_lab(child_file_pattern, data_path)
        parent_v1dat <- util_fbs_parent_v1dat(parent_file_pattern, data_path)
    } else {
        child_v1dat <- util_fbs_child_v1dat(child_file_pattern)
        child_home_v1dat <- util_fbs_child_v1dat_home(child_file_pattern)
        child_lab_v1dat <- util_fbs_child_v1dat_lab(child_file_pattern)
        parent_v1dat <- util_fbs_parent_v1dat(parent_file_pattern)
    }

    #### 3. Merge Child Raw Data #####

    # merge child home and lab into single database
    child_covidmerge_v1dat <- merge(child_lab_v1dat[['data']], child_home_v1dat[['data']][c(1, 7:95)], by = 'id', all = TRUE)

    # re-order so matches child_v1dat
    child_covidmerge_v1dat <- child_covidmerge_v1dat[c(1:160, 273:361, 161:272)]

    # add pss soup since it is missing from v1 home - need to have same columns to merge
    child_covidmerge_v1dat[['pss_soup_eat']] <- NA
    child_covidmerge_v1dat[['pss_soup_much']] <- NA
    child_covidmerge_v1dat[['pss_soup_like']] <- NA

    child_covidmerge_v1dat <- child_covidmerge_v1dat[c(1:243, 362:364, 244:361)]

    # merge all child into single database
    all_child_v1dat <- rbind.data.frame(child_v1dat[['data']], child_covidmerge_v1dat)


    #### 4. Merge Parent Raw Data #####

    # add 'p_' to pss data to mark parent
    parent_pss_vars <- names(parent_v1dat[['data']])[c(251:406)]

    for (v in 1:length(parent_pss_vars)) {
        var_name <- parent_pss_vars[v]

        parent_pss_vars[v] <- paste0('p_', var_name)
    }

    names(parent_v1dat[['data']])[c(251:406)] <- parent_pss_vars
    names(parent_v1dat[['dict']])[c(251:406)] <- parent_pss_vars

    # update labels with 'parent report'

    for (v in 1:ncol(parent_v1dat[['data']])) {
        var_name <- names(parent_v1dat[['data']])[v]

        # remove existing label
        if (grepl("parent-reported", parent_v1dat[['dict']][[var_name]], fixed = TRUE)) {
            parent_v1dat[['dict']][[var_name]] <- gsub("parent-reported", "", parent_v1dat[['dict']][[var_name]])
        }

        # add universal label
        parent_v1dat[['dict']][[var_name]] <- paste0('Parent Reported: ', parent_v1dat[['dict']][[var_name]])
    }

    v1dat <- merge(all_child_v1dat, parent_v1dat[['data']][c(1, 5:408)], by = 'id', all = TRUE)

    # merge labels/dictionary
    v1dat_labels <- c(child_v1dat[['dict']], parent_v1dat[['dict']][5:408])

    #### 5. Organize V1 data and score #####

    # order of vars: 1) Demographics, 2) Anthro (hw, DXA, sleep, PA), 3) Intake (FF, liking, intake, want), 4) feeding/food Q's, 5) cog/trait Q's, 6) Delay Discounting, 7) MRI related (CAMS, FF, snack info, image ratings), 8) Notes

    #pull out DXA to add in separately
    v1dat_dxa <- v1dat[255:364]
    v1dat_dxa[['id']] <- as.numeric(v1dat[['id']])
    v1dat_labels_dxa <- v1dat_labels[c(1,255:364)]

    v1dat <- v1dat[c(1:254, 365:768)]
    v1dat_labels <- v1dat_labels[c(1:254, 365:768)]

    # organize non-DXA data
    v1dat_org <- v1dat[c(1:2, 16, 360:361, 3:6, 258:279, 362:364, 386:420, 429:440, 7:15, 255:257, 365:385, 280:359, 17:160, 421:428, 441:500, 161:252, 501:656, 657:658, 253:254)]

    v1dat_labels <- v1dat_labels[c(1:2, 16, 360:361, 3:6, 258:279, 362:364, 386:420, 429:440, 7:15, 255:257, 365:385, 280:359, 17:160, 421:428, 441:500, 161:252, 501:656, 657:658, 253:254)]

    # ensure labels are up to date
    v1dat_org = sjlabelled::set_label(v1dat_org, label = matrix(unlist(v1dat_labels, use.names = FALSE)))

    ## 5a) score puberty data ####
    pds_scored <- score_pds(pds_data = v1dat_org[c(1, 6, 18:29)], respondent = 'parent', male = 0, female = 1, parID = 'id')

    # get labels from scored data and simplify
    pds_scored_labels <- sapply(pds_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(pds_scored_labels) <- names(pds_scored)

    # merge and organize
    v1dat_scored <- merge(v1dat_org, pds_scored[c(1, 3:4)], by = 'id', all = TRUE)
    v1dat_scored <- v1dat_scored[c(1:29, 659:660, 30:658)]

    v1dat_scored_labels <- c(v1dat_labels[1:29], pds_scored_labels[3:4], v1dat_labels[30:658])

    ## 5b) score PAQ data and parent-reported sleep per day of the week ####
    paq_scored <- score_paq(paq_data = v1dat_org[c(1, 115:194)], study = 'fbs', sleep = TRUE, parID = 'id')

    # get labels from scored data and simplify
    paq_scored_labels <- sapply(paq_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(paq_scored_labels) <- names(paq_scored)

    # merge and organize
    v1dat_scored <- merge(v1dat_scored, paq_scored[c(1, 32:54)], by = 'id', all = TRUE)
    v1dat_scored <- v1dat_scored[c(1:196, 661:683, 197:660)]

    v1dat_scored_labels <- c(v1dat_scored_labels[1:196], paq_scored_labels[32:54], v1dat_scored_labels[197:660])

    ## 5c) get risk status ####
    risk_scored <- score_risk(risk_data = v1dat_org[c(1, 4, 100, 113:114)], respondent = 'parent_respondent', parID = 'id')

    # set id = 7 to low risk - only has dad measured, no mom
    risk_scored[risk_scored[['id']] == 7, 'risk_status_mom'] <- 0

    # manually exclude ('Neither' cat) for other reasons/based on older criteria
    risk_scored[risk_scored[['id']] == 8, 'risk_status_mom'] <- 2
    risk_scored[risk_scored[['id']] == 10, 'risk_status_mom'] <- 2
    risk_scored[risk_scored[['id']] == 12, 'risk_status_mom'] <- 2
    risk_scored[risk_scored[['id']] == 13, 'risk_status_mom'] <- 2
    risk_scored[risk_scored[['id']] == 14, 'risk_status_mom'] <- 2
    risk_scored[risk_scored[['id']] == 25, 'risk_status_mom'] <- 2
    risk_scored[risk_scored[['id']] == 27, 'risk_status_mom'] <- 2
    risk_scored[risk_scored[['id']] == 29, 'risk_status_mom'] <- 2
    risk_scored[risk_scored[['id']] == 32, 'risk_status_mom'] <- 2
    risk_scored[risk_scored[['id']] == 42, 'risk_status_mom'] <- 2
    risk_scored[risk_scored[['id']] == 44, 'risk_status_mom'] <- 2
    risk_scored[risk_scored[['id']] == 46, 'risk_status_mom'] <- 2
    risk_scored[risk_scored[['id']] == 61, 'risk_status_mom'] <- 2
    risk_scored[risk_scored[['id']] == 62, 'risk_status_mom'] <- 2
    risk_scored[risk_scored[['id']] == 65, 'risk_status_mom'] <- 2
    risk_scored[risk_scored[['id']] == 66, 'risk_status_mom'] <- 2
    risk_scored[risk_scored[['id']] == 67, 'risk_status_mom'] <- 2
    risk_scored[risk_scored[['id']] == 79, 'risk_status_mom'] <- 2
    risk_scored[risk_scored[['id']] == 85, 'risk_status_mom'] <- 2
    risk_scored[risk_scored[['id']] == 88, 'risk_status_mom'] <- 2

    risk_scored[risk_scored[['id']] == 8, 'risk_status_both'] <- 2
    risk_scored[risk_scored[['id']] == 12, 'risk_status_both'] <- 2
    risk_scored[risk_scored[['id']] == 14, 'risk_status_both'] <- 2
    risk_scored[risk_scored[['id']] == 25, 'risk_status_both'] <- 2
    risk_scored[risk_scored[['id']] == 29, 'risk_status_both'] <- 2
    risk_scored[risk_scored[['id']] == 44, 'risk_status_both'] <- 2
    risk_scored[risk_scored[['id']] == 62, 'risk_status_both'] <- 2
    risk_scored[risk_scored[['id']] == 65, 'risk_status_both'] <- 2
    risk_scored[risk_scored[['id']] == 66, 'risk_status_both'] <- 2
    risk_scored[risk_scored[['id']] == 88, 'risk_status_both'] <- 2

    # get labels from scored data and simplify
    risk_scored_labels <- sapply(risk_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(risk_scored_labels) <- names(risk_scored)

    # merge and organize
    v1dat_scored <- merge(v1dat_scored, risk_scored, by = 'id', all = TRUE)
    v1dat_scored <- v1dat_scored[c(1:5, 684:687, 6:683)]

    v1dat_scored_labels <- c(v1dat_scored_labels[1:5], risk_scored_labels[2:5], v1dat_scored_labels[6:683])

    #### 6. Add manual vars to scored data and updated labels ####

    ## weight status
    v1dat_scored[['weight_status']] <- ifelse(v1dat_scored[['bmi_percentile']] < 5, -99, ifelse(v1dat_scored[['bmi_percentile']] < 85, 0, ifelse(v1dat_scored[['bmi_percentile']] < 95, 1, 2)))

    v1dat_scored[['weight_status']] <- sjlabelled::set_labels(v1dat_scored[['weight_status']], labels = c(UW = -99, HW = 0, OW = 1, OB = 2))
    class(v1dat_scored[['weight_status']]) <- c("haven_labelled", "vctrs_vctr", "double")

    v1dat_scored_labels[['weight_status']] <- 'Child weight status using CDC BMI percentile cutoffs'

    v1dat_scored[['dad_weight_status']] <- ifelse(is.na(v1dat_scored[['parent_respondent']]) | v1dat_scored[['parent_respondent']] == 2, NA, ifelse(v1dat_scored[['parent_respondent']] == 1, ifelse(v1dat_scored[['parent_bmi']] < 18.5, -99, ifelse(v1dat_scored[['parent_bmi']] < 25, 0, ifelse(v1dat_scored[['parent_bmi']] < 30, 1, ifelse(v1dat_scored[['parent_bmi']] < 35, 2, ifelse(v1dat_scored[['parent_bmi']] < 40, 3, 4))))), ifelse(v1dat_scored[['sr_dad_bmi']] < 18.5, -99, ifelse(v1dat_scored[['sr_dad_bmi']] < 25, 0, ifelse(v1dat_scored[['sr_dad_bmi']] < 30, 1, ifelse(v1dat_scored[['sr_dad_bmi']] < 35, 2, ifelse(v1dat_scored[['sr_dad_bmi']] < 40, 3, 4)))))))

    v1dat_scored[['dad_weight_status']] <- sjlabelled::set_labels(v1dat_scored[['dad_weight_status']], labels = c(UW = -99, HW = 0, OW = 1, `C1-OB` = 2, `C2-OB` = 3, `C3-Severe OB` = 4))
    class(v1dat_scored[['dad_weight_status']]) <- c("haven_labelled", "vctrs_vctr", "double")

    v1dat_scored_labels[['dad_weight_status']] <- 'Dad weight status using CDC cutoffs'

    v1dat_scored[['mom_weight_status']] <- ifelse(is.na(v1dat_scored[['parent_respondent']]) | v1dat_scored[['parent_respondent']] == 2, NA, ifelse(v1dat_scored[['parent_respondent']] == 0, ifelse(v1dat_scored[['parent_bmi']] < 18.5, -99, ifelse(v1dat_scored[['parent_bmi']] < 25, 0, ifelse(v1dat_scored[['parent_bmi']] < 30, 1, ifelse(v1dat_scored[['parent_bmi']] < 35, 2, ifelse(v1dat_scored[['parent_bmi']] < 40, 3, 4))))), ifelse(v1dat_scored[['sr_mom_bmi']] < 18.5, -99, ifelse(v1dat_scored[['sr_mom_bmi']] < 25, 0, ifelse(v1dat_scored[['sr_mom_bmi']] < 30, 2, ifelse(v1dat_scored[['sr_mom_bmi']] < 35, 2, ifelse(v1dat_scored[['sr_mom_bmi']] < 40, 3, 4)))))))

    v1dat_scored[['mom_weight_status']] <- sjlabelled::set_labels(v1dat_scored[['mom_weight_status']], labels = c(UW = -99, HW = 0, OW = 1, `C1-OB` = 2, `C2-OB` = 3, `C3-Severe OB` = 4))
    class(v1dat_scored[['mom_weight_status']]) <- c("haven_labelled", "vctrs_vctr", "double")

    v1dat_scored_labels[['mom_weight_status']] <- 'Mom weight status using CDC cutoffs'

    ## organize data
    v1dat_scored <- v1dat_scored[c(1:96, 688, 97:120, 689:690, 121:687)]
    v1dat_scored_labels <- v1dat_scored_labels[c(1:96, 688, 97:120, 689:690, 121:687)]

    #### 7. Food Intake ####

    ## 7a) EAH ####
    v1_eah_kcal <- fbs_kcal_intake(v1dat_scored[c(1, 303:342)], meal = 'EAH', parID = 'id')
    names(v1_eah_kcal)[11:12] <- c('eah_total_g', 'eah_total_kcal')

    # get labels from scored data and simplify
    v1_eah_labels <- sapply(v1_eah_kcal, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(v1_eah_labels) <- names(v1_eah_kcal)

    # merge and organize
    v1dat_scored <- merge(v1dat_scored, v1_eah_kcal, by = 'id', all = TRUE)
    v1dat_scored_labels <- c(v1dat_scored_labels, v1_eah_labels[2:12])

    # organize
    v1dat_scored <- v1dat_scored[c(1:306, 691, 307:310, 692, 311:314, 693, 315:318, 694, 319:322, 695, 323:326, 696, 327:330, 697, 331:334, 698, 335:338, 699, 339:342, 700:701, 343:690)]

    v1dat_scored_labels <- v1dat_scored_labels[c(1:306, 691, 307:310, 692, 311:314, 693, 315:318, 694, 319:322, 695, 323:326, 696, 327:330, 697, 331:334, 698, 335:338, 699, 339:342, 700:701, 343:690)]

    ## 7b) Standard Meal ####
    v1_meal_kcal <- fbs_kcal_intake(v1dat_scored[c(1, 255:302)], meal = 'std_meal', parID = 'id')
    names(v1_meal_kcal)[14:15] <- c('meal_total_g', 'meal_total_kcal')

    # get labels from scored data and simplify
    v1_meal_labels <- sapply(v1_meal_kcal, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(v1_meal_labels) <- names(v1_meal_kcal)

    # merge and organize
    v1dat_scored <- merge(v1dat_scored, v1_meal_kcal, by = 'id', all = TRUE)
    v1dat_scored_labels <- c(v1dat_scored_labels, v1_meal_labels[c(2:15)])

    # organize
    v1dat_scored <- v1dat_scored[c(1:258, 702, 259:262, 703, 263:266, 704, 267:270, 705, 271:274, 706, 275:278, 707, 279:282, 708, 283:286, 709, 287:290, 710, 291:294, 711, 295:298, 712, 299:302, 713:715, 303:701)]

    v1dat_scored_labels <- v1dat_scored_labels[c(1:258, 702, 259:262, 703, 263:266, 704, 267:270, 705, 271:274, 706, 275:278, 707, 279:282, 708, 283:286, 709, 287:290, 710, 291:294, 711, 295:298, 712, 299:302, 713:715, 303:701)]

    #### 8. DXA data #####
    v1dat_scored <- merge(v1dat_scored, v1dat_dxa, by = 'id', all = TRUE)
    v1dat_scored <- v1dat_scored[c(1:100, 716:825, 101:715)]

    v1dat_scored_labels <- c(v1dat_scored_labels[1:100], v1dat_labels_dxa[2:111], v1dat_scored_labels[101:715])

    #### 9. PNA data #####

    # only parent has pna data to organize
    v1dat_pna <- parent_v1dat[['pna_data']]

    #### 10. save to list #####

    # put data in order of participant ID for ease
    v1dat_scored <- v1dat_scored[order(v1dat_scored[["id"]]), ]
    v1dat_pna <- v1dat_pna[order(v1dat_pna[["id"]]), ]

    # set labels
    v1dat_scored = sjlabelled::set_label(v1dat_scored, label = matrix(unlist(v1dat_scored_labels, use.names = FALSE)))
    v1dat_pna = sjlabelled::set_label(v1dat_pna, label = matrix(unlist(parent_v1dat[['pna_dict']], use.names = FALSE)))

    v1data_list <- list(data = v1dat_scored, dict = v1dat_scored_labels, pna_data = v1dat_pna, pna_dict = parent_v1dat[['pna_dict']])

    return(v1data_list)
}

