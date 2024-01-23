#' util_fbs_merge_v5: Merge all Visit 5 raw Qualtrics databases and organize variable in database order
#'
#' This function merges the following visit 5 raw data into a single database and organizes variables in database order: child visit 5, child visit 5-home, child visit 5-lab, and parent visit 5
#'
#' The databases MUST follow the naming convention: Child_V5_YYYY-MM-DD.sav, Child_V5_Home_YYY-MM-DD.sav, Child_V5_Lab_YYY-MM-DD.sav, and Parent_V5_YYY-MM-DD.sav. The databases must all be in the SAME directory to be processed if the data_path is not entered and the directory organization does not follow the structure laid out in the DataManual.
#'
#' @inheritParams util_fbs_merge_v1
#' @inheritParams util_fbs_merge_v1
#' @inheritParams util_fbs_parent_v1dat
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from parent visit 5 Qualtrics; 2) dict: all variable descriptions; 3) pna_data: data.frame marking participants who 'preferred not to answer' (pna) specific questions; and 4) pna_dict: all variable descriptions for pna_data
#'
#' @examples
#' #if in same working directory as data with all data. Note - there is no need to add the COVID protocol (i.e., 'Home' or 'Lab') to file_pattern - these files will be search for automatically:
#' v5dat_scored <- util_fbs_merge_v5(child_file_pattern = 'Child_V5', parent_file_pattern = 'Parent_V5')
#'
#' \dontrun{
#' #child_file_pattern and parent_file_pattern must be a strings. The following will not run:
#' v5dat_scored <- util_fbs_merge_v5(child_file_pattern = Child_V5, parent_file_pattern = Parent_V5)
#'
#' # *_file_pattern must have the respondent ('Child') and visit number ('V5'). If just enter 'Child' or 'Parent', the script will not run because it will return multiple files for different child visits. The following will not run:
#' v5dat_scored <- util_fbs_merge_v5(child_file_pattern = 'Child', parent_file_pattern = 'Parent')
#' }
#'
#' @seealso Raw data from Qualtrics is processed using the following scripts: \code{\link{util_fbs_child_v5dat}}, \code{\link{util_fbs_child_v5dat_home}}, \code{\link{util_fbs_child_v5dat_lab}}, \code{\link{util_fbs_parent_v5dat}}. Visit 5 data is scored using the following scripts: \code{\link{score_ctc}}, \code{\link{score_audit}}
#'
#'
#' @export

util_fbs_merge_v5 <- function(child_file_pattern, parent_file_pattern, data_path) {

    #### 1. Set up/initial checks #####
    # check that file_pattern exist and is a string

    c_filepat_arg <- methods::hasArg(child_file_pattern)

    if (isTRUE(c_filepat_arg) & !is.character(child_file_pattern)) {
        stop("child_file_pattern must be entered as a string: e.g., 'Child_V5'")
    } else if (isFALSE(c_filepat_arg)) {
        stop("child_file_pattern must set to the a string matching the name of the raw data file for child visit: e.g., 'Child_V5'")
    }

    p_filepat_arg <- methods::hasArg(parent_file_pattern)

    if (isTRUE(p_filepat_arg) & !is.character(parent_file_pattern)) {
        stop("parent_file_pattern must be entered as a string: e.g., 'Parent_V5'")
    } else if (isFALSE(p_filepat_arg)) {
        stop("parent_file_pattern must set to the a string matching the name of the raw data file for child visit: e.g., 'Parent_V5'")
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
        child_v5dat <- util_fbs_child_v5dat(child_file_pattern, data_path)
        child_home_v5dat <- util_fbs_child_v5dat_home(child_file_pattern, data_path)
        child_lab_v5dat <- util_fbs_child_v5dat_lab(child_file_pattern, data_path)
        parent_v5dat <- util_fbs_parent_v5dat(parent_file_pattern, data_path)
    } else {
        child_v5dat <- util_fbs_child_v5dat(child_file_pattern)
        child_home_v5dat <- util_fbs_child_v5dat_home(child_file_pattern)
        child_lab_v5dat <- util_fbs_child_v5dat_lab(child_file_pattern)
        parent_v5dat <- util_fbs_parent_v5dat(parent_file_pattern)
    }

    #### 3. Merge Child Raw Data #####

    # merge child home and lab into single database
    child_covidmerge_v5dat <- merge(child_lab_v5dat[['data']], child_home_v5dat[['data']][c(1, 3:18)], by = 'id', all = TRUE)

    # re-order so matches child_v5dat
    child_covidmerge_v5dat <- child_covidmerge_v5dat[c(1:64, 156:171, 65:146, 147:155)]

    # merge all child into single database
    ##manually change par 5 - has 2 v5s, 1 with only interoception

    #get just intero par 5
    child_v5dat_par5intero <- child_v5dat[['data']][child_v5dat[['data']][['id']] == 5 & !is.na(child_v5dat[['data']][['intero_prac_hbcount']]), ]

    #get all other data and full par 5
    child_v5dat[['data']] <- child_v5dat[['data']][!(child_v5dat[['data']][['id']] == 5 & !is.na(child_v5dat[['data']][['intero_prac_hbcount']])), ]

    #merge
    all_child_v5dat <- rbind.data.frame(child_v5dat[['data']], child_covidmerge_v5dat)

    # add par 5 intero back in
    all_child_v5dat[all_child_v5dat[['id']] == 5, 81:167] <- child_v5dat_par5intero[81:167]

    #### 4. Merge Parent Raw Data #####

    # update labels with 'parent report'

    for (v in 1:ncol(parent_v5dat[['data']])) {
        var_name <- names(parent_v5dat[['data']])[v]

        # remove existing label
        if (grepl("parent-reported", parent_v5dat[['dict']][[var_name]], fixed = TRUE)) {
            parent_v5dat[['dict']][[var_name]] <- gsub("parent-reported", "", parent_v5dat[['dict']][[var_name]])
        }

        # add universal label
        parent_v5dat[['dict']][[var_name]] <- paste0('Parent Reported: ', parent_v5dat[['dict']][[var_name]])
    }

    v5dat <- merge(all_child_v5dat, parent_v5dat[['data']][c(1, 3:26)], by = 'id', all = TRUE)

    # merge labels/dictionary
    v5dat_labels <- c(child_v5dat[['dict']], parent_v5dat[['dict']][3:26])

    #### 5. Organize V5 data ####

    # order of vars: 1) Demographics, 2) Anthro (hw, DXA, sleep, PA), 3) Intake (FF, liking, intake, want), 4) feeding/food Q's, 5) cog/trait Q's, 6) Delay Discounting, 7) MRI related (CAMS, FF, snack info, image ratings), 8) Notes

    v5dat_org <- v5dat[c(1:2, 65:80, 172:181, 3:41, 42:64, 81:157, 158:171, 182:195)]

    v5dat_labels <- v5dat_labels[c(1:2, 65:80, 172:181, 3:41, 42:64, 81:157, 158:171, 182:195)]

    # ensure labels are up to date
    v5dat_org = sjlabelled::set_label(v5dat_org, label = matrix(unlist(v5dat_labels, use.names = FALSE)))

    #### 6. Score V5 data ####

    ## 6a) score the Communities that Care Survey ####
    ## need to make
    ctc_scored <- score_ctc(ctc_data = v5dat_org[c(1, 3:18)], study = 'fbs', parID = 'id')

    # get labels from scored data and simplify
    ctc_scored_labels <- sapply(ctc_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(ctc_scored_labels) <- names(ctc_scored)

    # merge and organize
    v5dat_scored <- merge(v5dat_org, ctc_scored, by = 'id', all = TRUE)
    v5dat_scored <- v5dat_scored[c(1:18, 196:199, 19:195)]

    v5dat_scored_labels <- c(v5dat_labels[1:18], ctc_scored_labels[2:5], v5dat_labels[19:195])

    ## 6b) score the Alcohol Use Disorders Identification Test ####
    audit_scored <- score_audit(audit_data = v5dat_org[c(1, 19:28)], parID = 'id')

    # get labels from scored data and simplify
    audit_scored_labels <- sapply(audit_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(audit_scored_labels) <- names(audit_scored)

    # merge and organize
    v5dat_scored <- merge(v5dat_scored, audit_scored, by = 'id', all = TRUE)
    v5dat_scored <- v5dat_scored[c(1:32, 200:201, 33:199)]

    v5dat_scored_labels <- c(v5dat_scored_labels[1:32], audit_scored_labels[2:3], v5dat_scored_labels[33:199])


    #### 7. Food Intake ####

    v5_kcal <- fbs_kcal_intake(v5dat_scored[c(1, 49:73)], meal = 'ps_meal', parID = 'id')

    names(v5_kcal)[7:8] <- c('total_g', 'total_kcal')

    # get labels from scored data and simplify
    v5_kcal_labels <- sapply(v5_kcal, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(v5_kcal_labels) <- names(v5_kcal)

    # merge and organize
    v5dat_scored <- merge(v5dat_scored, v5_kcal, by = 'id', all = TRUE)
    v5dat_scored_labels <- c(v5dat_scored_labels, v5_kcal_labels[2:8])

    ## add portion size label
    v5dat_scored['meal_ps'] <- ifelse(is.na(v5dat_scored[['noplate_mac_cheese_g']]), NA, ifelse(v5dat_scored[['noplate_mac_cheese_g']] < 280, 0, ifelse(v5dat_scored[['noplate_mac_cheese_g']] < 360, 1, ifelse(v5dat_scored[['noplate_mac_cheese_g']] < 440, 2, 3))))

    v5dat_scored[["meal_ps"]] <- sjlabelled::add_labels(v5dat_scored[["meal_ps"]], labels = c(ps1 = 0, ps2 = 1, ps3 = 2, ps4 = 3))
    class(v5dat_scored[["meal_ps"]]) <- c("haven_labelled", "vctrs_vctr", "double")

    v5dat_scored_labels[['meal_ps']] <- 'Visit 5 Portion Size Meal Condition'

    # organize
    v5dat_scored <- v5dat_scored[c(1:48, 209, 49:52, 202, 53:56, 203, 57:60, 204, 61:65, 205, 66:69, 206, 70:73, 207:208, 74:201)]

    v5dat_scored_labels <- v5dat_scored_labels[c(1:48, 209, 49:52, 202, 53:56, 203, 57:60, 204, 61:65, 205, 66:69, 206, 70:73, 207:208, 74:201)]

    #### 8. PNA data #####

    # child pna data

    #find names in common and unique
    common_names <- intersect(names(child_v5dat[['pna_data']]), names(child_lab_v5dat[['pna_data']]))
    child_v5dat_uniq_names <- c('id', names(child_v5dat[['pna_data']])[!(names(child_v5dat[['pna_data']]) %in% common_names)])
    child_lab_v5dat_uniq_names <- c('id', names(child_lab_v5dat[['pna_data']])[!(names(child_lab_v5dat[['pna_data']]) %in% common_names)])

    # initial merge with common names
    child_v5dat_pna_m1 <- rbind.data.frame(child_v5dat[['pna_data']][common_names], child_lab_v5dat[['pna_data']][common_names])

    # full merge with unique names
    if (length(child_v5dat_uniq_names) > 1 & length(child_lab_v5dat_uniq_names) > 1){
        child_v5dat_m2 <- merge(child_v5dat_pna_m1, child_v5dat[['pna_data']][child_v5dat_uniq_names], by = 'id', all = TRUE)
        child_v5dat_pna <- merge(child_v5dat_m2, child_lab_v5dat[['pna_data']][child_lab_v5dat_uniq_names], by = 'id', all = TRUE)
    } else if (length(child_v5dat_uniq_names) > 1) {
        child_v5dat_pna <- merge(child_v5dat_pna_m1, child_v5dat[['pna_data']][child_v5dat_uniq_names], by = 'id', all = TRUE)
    } else if (length(child_lab_v5dat_uniq_names) > 1){
        child_v5dat_pna <- merge(child_v5dat_m1, child_lab_v5dat[['pna_data']][child_lab_v5dat_uniq_names], by = 'id', all = TRUE)
    } else {
        child_v5dat_pna <- child_v5dat_m1
    }

    #labels
    child_v5dat_pna_labels <- lapply(child_v5dat_pna, function(x) attributes(x)$label)

    #loop through to get all labels
    for (v in 1:length(names(child_v5dat_pna))){
        var_name = names(child_v5dat_pna)[v]

        if (var_name %in% names(child_v5dat[['pna_data']])){
            child_v5dat_pna_labels[[var_name]] <- child_v5dat[['pna_dict']][[var_name]]
        } else if (var_name %in% names(child_lab_v5dat[['pna_data']])){
            child_v5dat_pna_labels[[var_name]] <- child_lab_v5dat[['pna_dict']][[var_name]]
        }
    }

    # parent pna data
    for (v in 1:ncol(parent_v5dat[['pna_data']])) {
        var_name <- names(parent_v5dat[['pna_data']])[v]

        # remove existing label
        if (grepl("parent-reported", parent_v5dat[['pna_dict']][[var_name]], fixed = TRUE)) {
            parent_v5dat[['pna_dict']][[var_name]] <- gsub("parent-reported", "", parent_v5dat[['pna_dict']][[var_name]])
        }

        # add universal label
        parent_v5dat[['pna_dict']][[var_name]] <- paste0('Parent Reported: ', parent_v5dat[['pna_dict']][[var_name]])
    }

    v5dat_pna <- merge(child_v5dat_pna, parent_v5dat[['pna_data']], by = 'id', all = TRUE)

    v5dat_pna_labels <- c(child_v5dat_pna_labels, parent_v5dat[['pna_dict']][2:length(parent_v5dat[['pna_dict']])])

    #### 9. save to list #####

    # put data in order of participant ID for ease
    v5dat_scored <- v5dat_scored[order(v5dat_scored[["id"]]), ]
    v5dat_pna <- v5dat_pna[order(v5dat_pna[["id"]]), ]

    # set labels
    v5dat_scored = sjlabelled::set_label(v5dat_scored, label = matrix(unlist(v5dat_scored_labels, use.names = FALSE)))
    v5dat_pna = sjlabelled::set_label(v5dat_pna, label = matrix(unlist(v5dat_pna_labels, use.names = FALSE)))

    v5data_list <- list(data = v5dat_scored, dict = v5dat_scored_labels, pna_data = v5dat_pna, pna_dict = v5dat_pna_labels)

    return(v5data_list)
}

