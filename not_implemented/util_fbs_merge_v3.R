#' util_fbs_merge_v3: Merge all Visit 3 raw Qualtrics databases and organize variable in database order
#'
#' This function merges the following visit 3 raw data into a single database and organizes variables in database order: child visit 3, child visit 3-home, child visit 3-lab, and parent visit 3
#'
#' The databases MUST follow the naming convention: Child_V3_YYYY-MM-DD.sav, Child_V3_Home_YYY-MM-DD.sav, Child_V3_Lab_YYY-MM-DD.sav, and Parent_V3_YYY-MM-DD.sav. The databases must all be in the SAME directory to be processed if the data_path is not entered and the directory organization does not follow the structure laid out in the DataManual.
#' @inheritParams util_fbs_merge_v1
#' @inheritParams util_fbs_merge_v1
#' @inheritParams util_fbs_parent_v1dat
#' @param model_DD Indicate if delay discounting data should be modeled. This will take an addition 3-5 minutes of processing time. Default = FALSE.
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from parent visit 3 Qualtrics; 2) dict: all variable descriptions; 3) pna_data: data.frame marking participants who 'preferred not to answer' (pna) specific questions; and 4) pna_dict: all variable descriptions for pna_data
#'
#' @examples
#' #if in same working directory as data with all data. Note - there is no need to add the COVID protocol (i.e., 'Home' or 'Lab') to file_pattern - these files will be search for automatically:
#' v3dat_scored <- util_fbs_merge_v3(child_file_pattern = 'Child_V3', parent_file_pattern = 'Parent_V3')
#'
#' \dontrun{
#' #child_file_pattern and parent_file_pattern must be a strings. The following will not run:
#' v3dat_scored <- util_fbs_merge_v3(child_file_pattern = Child_V3, parent_file_pattern = Parent_V3)
#'
#' # *_file_pattern must have the respondent ('Child') and visit number ('V3'). If just enter 'Child' or 'Parent', the script will not run because it will return multiple files for different child visits. The following will not run:
#' v3dat_scored <- util_fbs_merge_v3(child_file_pattern = 'Child', parent_file_pattern = 'Parent')
#' }
#'
#' @seealso Raw data from Qualtrics is processed using the following scripts: \code{\link{util_fbs_child_v3dat}}, \code{\link{util_fbs_child_v3dat_home}}, \code{\link{util_fbs_child_v3dat_lab}}, \code{\link{util_fbs_parent_v3dat}}. Visit 3 data is scored using the following scripts: \code{\link{score_lbc}}, \code{\link{score_pwlb}}, \code{\link{score_tfeq}}, \code{\link{score_bisbas}}, \code{\link{score_spsrq}}, \code{\link{model_dd}}
#'
#'
#' @export

util_fbs_merge_v3 <- function(child_file_pattern, parent_file_pattern, data_path, model_DD = FALSE) {

    #### 1. Set up/initial checks #####
    # check that file_pattern exist and is a string

    c_filepat_arg <- methods::hasArg(child_file_pattern)

    if (isTRUE(c_filepat_arg) & !is.character(child_file_pattern)) {
        stop("child_file_pattern must be entered as a string: e.g., 'Child_V3'")
    } else if (isFALSE(c_filepat_arg)) {
        stop("child_file_pattern must set to the a string matching the name of the raw data file for child visit: e.g., 'Child_V3'")
    }

    p_filepat_arg <- methods::hasArg(parent_file_pattern)

    if (isTRUE(p_filepat_arg) & !is.character(parent_file_pattern)) {
        stop("parent_file_pattern must be entered as a string: e.g., 'Parent_V3'")
    } else if (isFALSE(p_filepat_arg)) {
        stop("parent_file_pattern must set to the a string matching the name of the raw data file for child visit: e.g., 'Parent_V3'")
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
        child_v3dat <- util_fbs_child_v3dat(child_file_pattern, data_path)
        child_home_v3dat <- util_fbs_child_v3dat_home(child_file_pattern, data_path)
        child_lab_v3dat <- util_fbs_child_v3dat_lab(child_file_pattern, data_path)
        parent_v3dat <- util_fbs_parent_v3dat(parent_file_pattern, data_path)
    } else {
        child_v3dat <- util_fbs_child_v3dat(child_file_pattern)
        child_home_v3dat <- util_fbs_child_v3dat_home(child_file_pattern)
        child_lab_v3dat <- util_fbs_child_v3dat_lab(child_file_pattern)
        parent_v3dat <- util_fbs_parent_v3dat(parent_file_pattern)
    }

    #### 3. Merge Child Raw Data #####

    # merge child home and lab into single database
    child_covidmerge_v3dat <- merge(child_lab_v3dat[['data']], child_home_v3dat[['data']][c(1, 3:71)], by = 'id', all = TRUE)

    #remove 120 - need to manually incorporate 'home' DD answers only
    child_covidmerge_v3dat_no120 <- child_covidmerge_v3dat[child_covidmerge_v3dat[['id']] != 120, ]

    # re-order so matches child_v3dat
    child_covidmerge_v3dat_no120 <- child_covidmerge_v3dat_no120[c(1:41, 44:112, 42:43)]

    # merge all child into single database
    all_child_v3dat <- rbind.data.frame(child_v3dat[['data']], child_covidmerge_v3dat_no120)

    #add DD back for 120
    all_child_v3dat[all_child_v3dat[['id']] == 120, c(42:110)] <- child_home_v3dat[['data']][child_home_v3dat[['data']][['id']] == 120, 3:71]

    #### 4. Merge Parent Raw Data #####

    # update labels with 'parent report'

    for (v in 1:ncol(parent_v3dat[['data']])) {
        var_name <- names(parent_v3dat[['data']])[v]

        # remove existing label
        if (grepl("parent-reported", parent_v3dat[['dict']][[var_name]], fixed = TRUE)) {
            parent_v3dat[['dict']][[var_name]] <- gsub("parent-reported", "", parent_v3dat[['dict']][[var_name]])
        }

        # add universal label
        parent_v3dat[['dict']][[var_name]] <- paste0('Parent Reported: ', parent_v3dat[['dict']][[var_name]])
    }

    v3dat <- merge(all_child_v3dat, parent_v3dat[['data']][c(1, 3:188)], by = 'id', all = TRUE)

    # merge labels/dictionary
    v3dat_labels <- c(child_v3dat[['dict']], parent_v3dat[['dict']][3:188])

    #### 5. Organize V3 data ####

    # order of vars: 1) Demographics, 2) Anthro (hw, DXA, sleep, PA), 3) Intake (FF, liking, intake, want), 4) feeding/food Q's, 5) cog/trait Q's, 6) Delay Discounting, 7) MRI related (CAMS, FF, snack info, image ratings), 8) Notes

    v3dat_org <- v3dat[c(1:2, 3:41, 114:213, 262:285, 214:261, 42:110, 113, 111:112, 286:298)]

    v3dat_labels <- v3dat_labels[c(1:2, 3:41, 114:213, 262:285, 214:261, 42:110, 113, 111:112, 286:298)]

    # ensure labels are up to date
    v3dat_org = sjlabelled::set_label(v3dat_org, label = matrix(unlist(v3dat_labels, use.names = FALSE)))

    #### 6. Score V3 data ####

    ## 6a) score the Lifestyle Behavior Checklist ####
    lbc_scored <- score_lbc(lbc_data = v3dat_org[c(1, 42:61)], study = 'fbs', parID = 'id')

    # get labels from scored data and simplify
    lbc_scored_labels <- sapply(lbc_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(lbc_scored_labels) <- names(lbc_scored)

    # merge and organize
    v3dat_scored <- merge(v3dat_org, lbc_scored, by = 'id', all = TRUE)
    v3dat_scored <- v3dat_scored[c(1:61, 299:303, 62:298)]

    v3dat_scored_labels <- c(v3dat_labels[1:61], lbc_scored_labels[2:6], v3dat_labels[62:298])

    ## 6b) score the Parent Weight-Loss Behavior Questionnaire ####
    pwlb_scored <- score_pwlb(pwlb_data = v3dat_org[c(1, 62:90)], parID = 'id')

    # get labels from scored data and simplify
    pwlb_scored_labels <- sapply(pwlb_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(pwlb_scored_labels) <- names(pwlb_scored)

    # merge and organize
    v3dat_scored <- merge(v3dat_scored, pwlb_scored, by = 'id', all = TRUE)
    v3dat_scored <- v3dat_scored[c(1:95, 304:306, 96:303)]

    v3dat_scored_labels <- c(v3dat_scored_labels[1:95], pwlb_scored_labels[2:4], v3dat_scored_labels[96:303])

    ## 6c) score the Three Factor Eating Questionnaire ####
    tfeq_scored <- score_tfeq(tfeq_data = v3dat_org[c(1, 91:141)], parID = 'id')

    # get labels from scored data and simplify
    tfeq_scored_labels <- sapply(tfeq_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(tfeq_scored_labels) <- names(tfeq_scored)

    # merge and organize
    v3dat_scored <- merge(v3dat_scored, tfeq_scored, by = 'id', all = TRUE)
    v3dat_scored <- v3dat_scored[c(1:149, 307:309, 150:306)]

    v3dat_scored_labels <- c(v3dat_scored_labels[1:149], tfeq_scored_labels[2:4], v3dat_scored_labels[150:306])

    ## 6d) score the Behavioral Inhibition Scale/Behavioral Activation Scale ####
    bisbas_scored <- score_bisbas(bisbas_data = v3dat_org[c(1, 142:165)], parID = 'id')

    # get labels from scored data and simplify
    bisbas_scored_labels <- sapply(bisbas_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(bisbas_scored_labels) <- names(bisbas_scored)

    # merge and organize
    v3dat_scored <- merge(v3dat_scored, bisbas_scored, by = 'id', all = TRUE)
    v3dat_scored <- v3dat_scored[c(1:176, 310:314, 177:309)]

    v3dat_scored_labels <- c(v3dat_scored_labels[1:176], bisbas_scored_labels[2:6], v3dat_scored_labels[177:309])

    ## 6e) score the Sensitivity to Punishment and Sensitivity to Reward Questionnaire ####
    spsrq_scored <- score_spsrq(spsrq_data = v3dat_org[c(1, 166:213)], parID = 'id')

    # get labels from scored data and simplify
    spsrq_scored_labels <- sapply(spsrq_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(spsrq_scored_labels) <- names(spsrq_scored)

    # merge and organize
    v3dat_scored <- merge(v3dat_scored, spsrq_scored, by = 'id', all = TRUE)
    v3dat_scored <- v3dat_scored[c(1:229, 315:325, 230:314)]

    v3dat_scored_labels <- c(v3dat_scored_labels[1:229], spsrq_scored_labels[2:12], v3dat_scored_labels[230:314])

    ## 6f) model Delay Discounting ####

    if (isTRUE(model_DD)){

        message('Modeling Dealy Discounting data ...')

        dd_scored <- model_dd(dd_data = v3dat_org[c(1, 214:282)], parID = 'id')

        message('Finished modeling Dealy Discounting')

        # get labels from scored data and simplify
        dd_scored_labels <- sapply(dd_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

        # make names match because simplify duplicates - not sure why get nested lists
        names(dd_scored_labels) <- names(dd_scored)

        # merge and organize
        v3dat_scored <- merge(v3dat_scored, dd_scored, by = 'id', all = TRUE)
        v3dat_scored <- v3dat_scored[c(1:309, 326:333, 310:325)]

        v3dat_scored_labels <- c(v3dat_scored_labels[1:309], dd_scored_labels[2:9], v3dat_scored_labels[310:325])
    }

    #### 7. Food Intake ####

    v3_kcal <- fbs_kcal_intake(v3dat_scored[c(1, 17:41)], meal = 'ps_meal', parID = 'id')

    names(v3_kcal)[7:8] <- c('total_g', 'total_kcal')

    # get labels from scored data and simplify
    v3_kcal_labels <- sapply(v3_kcal, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(v3_kcal_labels) <- names(v3_kcal)

    # merge and organize
    v3dat_scored <- merge(v3dat_scored, v3_kcal, by = 'id', all = TRUE)
    v3dat_scored_labels <- c(v3dat_scored_labels, v3_kcal_labels[2:8])

    ## add portion size label
    v3dat_scored['meal_ps'] <- ifelse(is.na(v3dat_scored[['noplate_mac_cheese_g']]), NA, ifelse(v3dat_scored[['noplate_mac_cheese_g']] < 280, 0, ifelse(v3dat_scored[['noplate_mac_cheese_g']] < 360, 1, ifelse(v3dat_scored[['noplate_mac_cheese_g']] < 440, 2, 3))))

    v3dat_scored[["meal_ps"]] <- sjlabelled::add_labels(v3dat_scored[["meal_ps"]], labels = c(ps1 = 0, ps2 = 1, ps3 = 2, ps4 = 3))
    class(v3dat_scored[["meal_ps"]]) <- c("haven_labelled", "vctrs_vctr", "double")

    v3dat_scored_labels[['meal_ps']] <- 'Visit 3 Portion Size Meal Condition'

    if (isTRUE(model_DD)){
        # organize
        v3dat_scored <- v3dat_scored[c(1:16, 341, 17:20, 334, 21:24, 335, 25:28, 336, 29:33, 337, 34:37, 338, 38:41, 339:340, 42:333)]

        v3dat_scored_labels <- v3dat_scored_labels[c(1:16, 341, 17:20, 334, 21:24, 335, 25:28, 336, 29:33, 337, 34:37, 338, 38:41, 339:340, 42:333)]

    } else {
        # organize
        v3dat_scored <- v3dat_scored[c(1:16, 333, 17:20, 326, 21:24, 327, 25:28, 328, 29:33, 329, 34:37, 330, 38:41, 331:332, 42:325)]

        v3dat_scored_labels <- v3dat_scored_labels[c(1:16, 333, 17:20, 326, 21:24, 327, 25:28, 328, 29:33, 329, 34:37, 330, 38:41, 331:332, 42:325)]
    }

    #### 8. PNA data #####

    # only parent has pna data to organize
    v3dat_pna <- parent_v3dat[['pna_data']]

    #### 9. save to list #####

    # put data in order of participant ID for ease
    v3dat_scored <- v3dat_scored[order(v3dat_scored[["id"]]), ]
    v3dat_pna <- v3dat_pna[order(v3dat_pna[["id"]]), ]

    # set labels
    v3dat_scored = sjlabelled::set_label(v3dat_scored, label = matrix(unlist(v3dat_scored_labels, use.names = FALSE)))
    v3dat_pna = sjlabelled::set_label(v3dat_pna, label = matrix(unlist(parent_v3dat[['pna_dict']], use.names = FALSE)))

    v3data_list <- list(data = v3dat_scored, dict = v3dat_scored_labels, pna_data = v3dat_pna, pna_dict = parent_v3dat[['pna_dict']])

    return(v3data_list)
}

