#' util_fbs_merge_v2: Merge all Visit 2 raw Qualtrics databases and organize variable in database order
#'
#' This function merges the following visit 2 raw data into a single database and organizes variables in database order: child visit 2, child visit 2-home, child visit 2-lab, and parent visit 2
#'
#' The databases MUST follow the naming convention: Child_V2_YYYY-MM-DD.sav, Child_V2_Home_YYY-MM-DD.sav, Child_V2_Lab_YYY-MM-DD.sav, and Parent_V2_YYY-MM-DD.sav. The databases must all be in the SAME directory to be processed if the data_path is not entered and the directory organization does not follow the structure laid out in the DataManual.
#'
#' @inheritParams util_fbs_merge_v1
#' @inheritParams util_fbs_merge_v1
#' @param parentV4_file_pattern string with the pattern to search for to find the raw parent data file for visit 4. The pattern must contain respondent and visit number (e.g., 'Parent_V4').
#' @inheritParams util_fbs_parent_v1dat
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from parent visit 2 Qualtrics; 2) dict: all variable descriptions; 3) pna_data: data.frame marking participants who 'preferred not to answer' (pna) specific questions; and 4) pna_dict: all variable descriptions for pna_data
#'
#' @examples
#' #if in same working directory as data with all data. Note - there is no need to add the COVID protocol (i.e., 'Home' or 'Lab') to file_pattern - these files will be search for automatically:
#' v2dat_scored <- util_fbs_merge_v2(child_file_pattern = 'Child_V2', parent_file_pattern = 'Parent_V2')
#'
#' \dontrun{
#' #child_file_pattern and parent_file_pattern must be a strings. The following will not run:
#' v2dat_scored <- util_fbs_merge_v2(child_file_pattern = Child_V2, parent_file_pattern = Parent_V2)
#'
#' # *_file_pattern must have the respondent ('Child') and visit number ('V2'). If just enter 'Child' or 'Parent', the script will not run because it will return multiple files for different child visits. The following will not run:
#' v2dat_scored <- util_fbs_merge_v2(child_file_pattern = 'Child', parent_file_pattern = 'Parent')
#' }
#'
#' @seealso Raw data from Qualtrics is processed using the following scripts: \code{\link{util_fbs_child_v2dat}}, \code{\link{util_fbs_child_v2dat_home}}, \code{\link{util_fbs_child_v2dat_lab}}, \code{\link{util_fbs_parent_v2dat}}. Visit 2 data is scored using the following scripts: \code{\link{score_cshqa}}, \code{\link{score_tesqe}}, \code{\link{score_kfq}}, \code{\link{score_cebq}}, \code{\link{score_bes}}, \code{\link{score_cfq}}, \code{\link{score_ffbs}}, \code{\link{score_rcmas}}, and \code{\link{score_cbq}}
#'
#'
#' @export

util_fbs_merge_v2 <- function(child_file_pattern, parent_file_pattern, parentV4_file_pattern, data_path) {

    #### 1. Set up/initial checks #####
    # check that file_pattern exist and is a string

    c_filepat_arg <- methods::hasArg(child_file_pattern)

    if (isTRUE(c_filepat_arg) & !is.character(child_file_pattern)) {
        stop("child_file_pattern must be entered as a string: e.g., 'Child_V2'")
    } else if (isFALSE(c_filepat_arg)) {
        stop("child_file_pattern must set to the a string matching the name of the raw data file for child visit: e.g., 'Child_V2'")
    }

    p_filepat_arg <- methods::hasArg(parent_file_pattern)

    if (isTRUE(p_filepat_arg) & !is.character(parent_file_pattern)) {
        stop("parent_file_pattern must be entered as a string: e.g., 'Parent_V2'")
    } else if (isFALSE(p_filepat_arg)) {
        stop("parent_file_pattern must set to the a string matching the name of the raw data file for child visit: e.g., 'Parent_V2'")
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
        child_v2dat <- util_fbs_child_v2dat(child_file_pattern, data_path)
        child_home_v2dat <- util_fbs_child_v2dat_home(child_file_pattern, data_path)
        child_lab_v2dat <- util_fbs_child_v2dat_lab(child_file_pattern, data_path)
        parent_v2dat <- util_fbs_parent_v2dat(parent_file_pattern, data_path)
    } else {
        child_v2dat <- util_fbs_child_v2dat(child_file_pattern)
        child_home_v2dat <- util_fbs_child_v2dat_home(child_file_pattern)
        child_lab_v2dat <- util_fbs_child_v2dat_lab(child_file_pattern)
        parent_v2dat <- util_fbs_parent_v2dat(parent_file_pattern)
    }

    ## 2a) Check for parent V4  #####
    # this script can run without parent V4 data so do not want to relay on raw data script for it because if file doesn't exist it will stop the script.

    if (isTRUE(datapath_arg)) {
        qv4_parent_path <- list.files(path = data_path, pattern = parentV4_file_pattern, full.names = TRUE)
    } else {
        qv4_parent_path <- paste0(pattern = parentV4_file_pattern, full.names = TRUE)
    }

    # check number of files found
    if (length(qv4_parent_path) > 1) {
        load_qv4_parent = FALSE
    } else if (length(qv4_parent_path) == 0) {
        load_qv4_parent = FALSE
    }

    # check that file is of type '.sav'
    if (!grepl('.sav', qv4_parent_path, fixed = TRUE)){
        load_qv4_parent = FALSE
    }

    # check if file exists
    load_qv4_parent <- file.exists(qv4_parent_path)

    #### 3. Merge Child Raw Data #####

    # merge child home and lab into single database
    child_covidmerge_v2dat <- merge(child_lab_v2dat[['data']], child_home_v2dat[['data']][c(1, 3:109)], by = 'id', all = TRUE)

    # re-order so matches child_v2dat
    child_covidmerge_v2dat <- child_covidmerge_v2dat[c(1:41, 44:150, 42:43)]

    # merge all child into single database
    all_child_v2dat <- rbind.data.frame(child_v2dat[['data']], child_covidmerge_v2dat)

    #### 4. Merge Parent Raw Data #####

    # update labels with 'parent report'

    for (v in 1:ncol(parent_v2dat[['data']])) {
        var_name <- names(parent_v2dat[['data']])[v]

        # remove existing label
        if (grepl("parent-reported", parent_v2dat[['dict']][[var_name]], fixed = TRUE)) {
            parent_v2dat[['dict']][[var_name]] <- gsub("parent-reported", "", parent_v2dat[['dict']][[var_name]])
        }

        # add universal label
        parent_v2dat[['dict']][[var_name]] <- paste0('Parent Reported: ', parent_v2dat[['dict']][[var_name]])
    }

    v2dat <- merge(all_child_v2dat, parent_v2dat[['data']][c(1, 3:229)], by = 'id', all = TRUE)

    # merge labels/dictionary
    v2dat_labels <- c(child_v2dat[['dict']], parent_v2dat[['dict']][3:229])

    #### 5. Organize V2 data ####

    # order of vars: 1) Demographics, 2) Anthro (hw, DXA, sleep, PA), 3) Intake (FF, liking, intake, want), 4) feeding/food Q's, 5) cog/trait Q's, 6) Delay Discounting, 7) MRI related (CAMS, FF, snack info, image ratings), 8) Notes

    v2dat_org <- v2dat[c(1:2, 152:169, 3:41, 42:111, 170:204, 235:250, 205:234, 251:270, 112:148, 271:364, 151, 149:150, 365:377)]

    v2dat_labels <- v2dat_labels[c(1:2, 152:169, 3:41, 42:111, 170:204, 235:250, 205:234, 251:270, 112:148, 271:364, 151, 149:150, 365:377)]

    # ensure labels are up to date
    v2dat_org = sjlabelled::set_label(v2dat_org, label = matrix(unlist(v2dat_labels, use.names = FALSE)))

    #### 6. Score V2 data ####

    ## 6a) score Children's Sleep Habbits Questionnaire-Abbreviated ####
    cshqa_scored <- score_cshqa(cshqa_data = v2dat_org[c(1, 3:20)], study = 'fbs', parID = 'id')

    # get labels from scored data and simplify
    cshqa_scored_labels <- sapply(cshqa_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(cshqa_scored_labels) <- names(cshqa_scored)

    # merge and organize
    v2dat_scored <- merge(v2dat_org, cshqa_scored, by = 'id', all = TRUE)
    v2dat_scored <- v2dat_scored[c(1:20, 378:385, 21:377)]

    v2dat_scored_labels <- c(v2dat_labels[1:20], cshqa_scored_labels[2:9], v2dat_labels[21:377])

    ## 6b) score Kids Food Questionnaire data ####
    kfq_scored <- score_kfq(kfq_data = v2dat_org[c(1, 60:105)], parID = 'id')

    # get labels from scored data and simplify
    kfq_scored_labels <- sapply(kfq_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(kfq_scored_labels) <- names(kfq_scored)

    # merge and organize
    v2dat_scored <- merge(v2dat_scored, kfq_scored, by = 'id', all = TRUE)
    v2dat_scored <- v2dat_scored[c(1:113, 386:403, 114:385)]

    v2dat_scored_labels <- c(v2dat_scored_labels[1:113], kfq_scored_labels[2:19], v2dat_scored_labels[114:385])

    ## 6c) score the Tempest Self-Regulation Questionnaire for Eating ####
    tesqe_scored <- score_tesqe(tesqe_data = v2dat_org[c(1, 106:129)], parID = 'id')

    # get labels from scored data and simplify
    tesqe_scored_labels <- sapply(tesqe_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(tesqe_scored_labels) <- names(tesqe_scored)

    # merge and organize
    v2dat_scored <- merge(v2dat_scored, tesqe_scored, by = 'id', all = TRUE)
    v2dat_scored <- v2dat_scored[c(1:155, 404:415, 156:403)]

    v2dat_scored_labels <- c(v2dat_scored_labels[1:155], tesqe_scored_labels[2:13], v2dat_scored_labels[156:403])

    ## 6d) score the Children's Eating Behavior Questionnaire ####
    cebq_scored <- score_cebq(cebq_data = v2dat_org[c(1, 130:164)], parID = 'id')

    # get labels from scored data and simplify
    cebq_scored_labels <- sapply(cebq_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(cebq_scored_labels) <- names(cebq_scored)

    # merge and organize
    v2dat_scored <- merge(v2dat_scored, cebq_scored, by = 'id', all = TRUE)
    v2dat_scored <- v2dat_scored[c(1:202, 416:425, 203:415)]

    v2dat_scored_labels <- c(v2dat_scored_labels[1:202], cebq_scored_labels[2:11], v2dat_scored_labels[203:415])

    ## 6e) score the Binge Eating Scale ####
    bes_scored <- score_bes(bes_data = v2dat_org[c(1, 165:180)], parID = 'id')

    # get labels from scored data and simplify
    bes_scored_labels <- sapply(bes_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(bes_scored_labels) <- names(bes_scored)

    # merge and organize
    v2dat_scored <- merge(v2dat_scored, bes_scored, by = 'id', all = TRUE)
    v2dat_scored <- v2dat_scored[c(1:228, 426, 229:425)]

    v2dat_scored_labels <- c(v2dat_scored_labels[1:228], bes_scored_labels[2], v2dat_scored_labels[229:425])

    ## 6f) score the Child Feeding Questionnaire ####
    cfq_scored <- score_cfq(cfq_data = v2dat_org[c(1, 181:210)], parID = 'id')

    # get labels from scored data and simplify
    cfq_scored_labels <- sapply(cfq_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(cfq_scored_labels) <- names(cfq_scored)

    # merge and organize
    v2dat_scored <- merge(v2dat_scored, cfq_scored, by = 'id', all = TRUE)
    v2dat_scored <- v2dat_scored[c(1:259, 427:433, 260:426)]

    v2dat_scored_labels <- c(v2dat_scored_labels[1:259], cfq_scored_labels[2:8], v2dat_scored_labels[260:426])

    ## 6g) score the Family Food Behavior Survey ####
    ffbs_scored <- score_ffbs(ffbs_data = v2dat_org[c(1, 211:230)], parID = 'id')

    # get labels from scored data and simplify
    ffbs_scored_labels <- sapply(ffbs_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(ffbs_scored_labels) <- names(ffbs_scored)

    # merge and organize
    v2dat_scored <- merge(v2dat_scored, ffbs_scored, by = 'id', all = TRUE)
    v2dat_scored <- v2dat_scored[c(1:286, 434:437, 287:433)]

    v2dat_scored_labels <- c(v2dat_scored_labels[1:286], ffbs_scored_labels[2:5], v2dat_scored_labels[287:433])

    ## 6h) score the Revised Children's Manifest Anxiety Scale ####

    #need child grade from parent V4
    if (isTRUE(load_qv4_parent)){
        if (isTRUE(datapath_arg)) {
            parent_v4dat <- util_fbs_parent_v4dat(parentV4_file_pattern, data_path)
        } else {
            parent_v4dat <- util_fbs_parent_v4dat(parentV4_file_pattern)
        }
    }

    if (isTRUE(load_qv4_parent)){
        rcmas_data <- merge(v2dat_org[c(1, 231:267)], parent_v4dat[['data']][c(1, 5)], by = 'id', all.x = TRUE, all.y = FALSE)
        rcmas_data[1:38] <- sjlabelled::set_label(rcmas_data[1:38], label = matrix(unlist(v2dat_scored_labels[c(1, 291:327)], use.names = FALSE)))
        rcmas_data['grade'] <- sjlabelled::set_label(rcmas_data['grade'], label = matrix(unlist(parent_v4dat[['dict']]['grade'], use.names = FALSE)))

        rcmas_scored <- score_rcmas(rcmas_data = rcmas_data, parID = 'id')
    } else {
        rcmas_scored <- score_rcmas(rcmas_data = v2dat_org[c(1, 231:267)], parID = 'id')
    }

    # get labels from scored data and simplify
    rcmas_scored_labels <- sapply(rcmas_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(rcmas_scored_labels) <- names(rcmas_scored)

    # merge and organize
    v2dat_scored <- merge(v2dat_scored, rcmas_scored, by = 'id', all = TRUE)

    # indexing differs by 1 if grade is included for RCMAS
    if (isTRUE(load_qv4_parent)){
        v2dat_scored <- merge(v2dat_scored, parent_v4dat[['data']][c(1, 5)], by = 'id', all.x = TRUE, all.y = FALSE)

        v2dat_scored <- v2dat_scored[c(1:2, 448, 3:327, 438:447, 328:437)]

        v2dat_scored_labels <- c(v2dat_scored_labels[1:2], parent_v4dat[['dict']]['grade'], v2dat_scored_labels[3:327], rcmas_scored_labels[2:11], v2dat_scored_labels[328:437])
    } else {
        v2dat_scored <- v2dat_scored[c(1:327, 438:447, 328:437)]

        v2dat_scored_labels <- c(v2dat_scored_labels[1:327], rcmas_scored_labels[2:11], v2dat_scored_labels[328:437])
    }

    ## 6i) score the Child Behavior Questionnaire ####
    cbq_scored <- score_cbq(cbq_data = v2dat_org[c(1, 268:361)], parID = 'id')

    # get labels from scored data and simplify
    cbq_scored_labels <- sapply(cbq_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(cbq_scored_labels) <- names(cbq_scored)

    # merge and organize
    v2dat_scored <- merge(v2dat_scored, cbq_scored, by = 'id', all = TRUE)

    # indexing differs by 1 if grade is included for RCMAS
    if (isTRUE(load_qv4_parent)){
        v2dat_scored <- v2dat_scored[c(1:432, 449:466, 433:448)]
        v2dat_scored_labels <- c(v2dat_scored_labels[1:432], cbq_scored_labels[2:19], v2dat_scored_labels[433:448])
    } else {
        v2dat_scored <- v2dat_scored[c(1:431, 448:465, 432:447)]
        v2dat_scored_labels <- c(v2dat_scored_labels[1:431], cbq_scored_labels[2:19], v2dat_scored_labels[432:447])
    }

    #### 7. Food Intake ####

    v2_kcal <- fbs_kcal_intake(v2dat_scored[c(1, 43:68)], meal = 'ps_meal', parID = 'id')

    names(v2_kcal)[7:8] <- c('total_g', 'total_kcal')

    # get labels from scored data and simplify
    v2_kcal_labels <- sapply(v2_kcal, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(v2_kcal_labels) <- names(v2_kcal)

    # merge and organize
    v2dat_scored <- merge(v2dat_scored, v2_kcal, by = 'id', all = TRUE)
    v2dat_scored_labels <- c(v2dat_scored_labels, v2_kcal_labels[2:8])

    ## add portion size label
    v2dat_scored['meal_ps'] <- ifelse(is.na(v2dat_scored[['noplate_mac_cheese_g']]), NA, ifelse(v2dat_scored[['noplate_mac_cheese_g']] < 280, 0, ifelse(v2dat_scored[['noplate_mac_cheese_g']] < 360, 1, ifelse(v2dat_scored[['noplate_mac_cheese_g']] < 440, 2, 3))))

    v2dat_scored[["meal_ps"]] <- sjlabelled::add_labels(v2dat_scored[["meal_ps"]], labels = c(ps1 = 0, ps2 = 1, ps3 = 2, ps4 = 3))
    class(v2dat_scored[["meal_ps"]]) <- c("haven_labelled", "vctrs_vctr", "double")

    v2dat_scored_labels[['meal_ps']] <- 'Visit 2 Portion Size Meal Condition'

    # indexing differs by 1 if grade is included for RCMAS
    if (isTRUE(load_qv4_parent)){
        v2dat_scored <- v2dat_scored[c(1:42, 474, 43:46, 467, 47:50, 468, 51:54, 469, 55:59, 470, 60:63, 471, 64:67, 472:473, 68:466)]

        v2dat_scored_labels <- v2dat_scored_labels[c(1:42, 474, 43:46, 467, 47:50, 468, 51:54, 469, 55:59, 470, 60:63, 471, 64:67, 472:473, 68:466)]
    } else {
        v2dat_scored <- v2dat_scored[c(1:42, 473, 43:46, 466, 47:50, 467, 51:54, 468, 55:59, 469, 60:63, 470, 64:67, 471:472, 68:465)]

        v2dat_scored_labels <- v2dat_scored_labels[c(1:42, 473, 43:46, 466, 47:50, 467, 51:54, 468, 55:59, 469, 60:63, 470, 64:67, 471:472, 68:465)]
    }

    #### 8. PNA data #####

    # only parent has pna data to organize
    v2dat_pna <- parent_v2dat[['pna_data']]

    #### 9. save to list #####

    # put data in order of participant ID for ease
    v2dat_scored <- v2dat_scored[order(v2dat_scored[["id"]]), ]
    v2dat_pna <- v2dat_pna[order(v2dat_pna[["id"]]), ]

    # set labels
    v2dat_scored = sjlabelled::set_label(v2dat_scored, label = matrix(unlist(v2dat_scored_labels, use.names = FALSE)))
    v2dat_pna = sjlabelled::set_label(v2dat_pna, label = matrix(unlist(parent_v2dat[['pna_dict']], use.names = FALSE)))

    v2data_list <- list(data = v2dat_scored, dict = v2dat_scored_labels, pna_data = v2dat_pna, pna_dict = parent_v2dat[['pna_dict']])

    return(v2data_list)
}

