#' util_fbs_merge_v4: Merge all Visit 4 raw Qualtrics databases and organize variable in database order
#'
#' This function merges the following visit 4 raw data into a single database and organizes variables in database order: child visit 4, child visit 4-home, child visit 4-lab, and parent visit 4
#'
#' The databases MUST follow the naming convention: Child_V4_YYYY-MM-DD.sav, Child_V4_Home_YYY-MM-DD.sav, Child_V4_Lab_YYY-MM-DD.sav, and Parent_V4_YYY-MM-DD.sav. The databases must all be in the SAME directory to be processed if the data_path is not entered and the directory organization does not follow the structure laid out in the DataManual.
#'
#' @inheritParams util_fbs_merge_v1
#' @inheritParams util_fbs_merge_v1
#' @inheritParams util_fbs_parent_v1dat
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from parent visit 4 Qualtrics; 2) dict: all variable descriptions; 3) pna_data: data.frame marking participants who 'preferred not to answer' (pna) specific questions; and 4) pna_dict: all variable descriptions for pna_data
#'
#' @examples
#' #if in same working directory as data with all data. Note - there is no need to add the COVID protocol (i.e., 'Home' or 'Lab') to file_pattern - these files will be search for automatically:
#' v4dat_scored <- util_fbs_merge_v4(child_file_pattern = 'Child_V4', parent_file_pattern = 'Parent_V4')
#'
#' \dontrun{
#' #child_file_pattern and parent_file_pattern must be a strings. The following will not run:
#' v4dat_scored <- util_fbs_merge_v4(child_file_pattern = Child_V4, parent_file_pattern = Parent_V4)
#'
#' # *_file_pattern must have the respondent ('Child') and visit number ('V4'). If just enter 'Child' or 'Parent', the script will not run because it will return multiple files for different child visits. The following will not run:
#' v4dat_scored <- util_fbs_merge_v4(child_file_pattern = 'Child', parent_file_pattern = 'Parent')
#' }
#'
#' @seealso Raw data from Qualtrics is processed using the following scripts: \code{\link{util_fbs_child_v4dat}}, \code{\link{util_fbs_child_v4dat_home}}, \code{\link{util_fbs_child_v4dat_lab}}, \code{\link{util_fbs_parent_v4dat}}. Visit 4 data is scored using the following scripts: \code{\link{score_hfssm}}, \code{\link{score_hfias}}, \code{\link{score_cchip}}, \code{\link{score_cwc}}, \code{\link{score_cbis}}, \code{\link{score_brief}}, \code{\link{score_psi}}
#'
#'
#' @export

util_fbs_merge_v4 <- function(child_file_pattern, parent_file_pattern, data_path) {

    #### 1. Set up/initial checks #####
    # check that file_pattern exist and is a string

    c_filepat_arg <- methods::hasArg(child_file_pattern)

    if (isTRUE(c_filepat_arg) & !is.character(child_file_pattern)) {
        stop("child_file_pattern must be entered as a string: e.g., 'Child_V4'")
    } else if (isFALSE(c_filepat_arg)) {
        stop("child_file_pattern must set to the a string matching the name of the raw data file for child visit: e.g., 'Child_V4'")
    }

    p_filepat_arg <- methods::hasArg(parent_file_pattern)

    if (isTRUE(p_filepat_arg) & !is.character(parent_file_pattern)) {
        stop("parent_file_pattern must be entered as a string: e.g., 'Parent_V4'")
    } else if (isFALSE(p_filepat_arg)) {
        stop("parent_file_pattern must set to the a string matching the name of the raw data file for child visit: e.g., 'Parent_V4'")
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
        child_v4dat <- util_fbs_child_v4dat(child_file_pattern, data_path)
        child_home_v4dat <- util_fbs_child_v4dat_home(child_file_pattern, data_path)
        child_lab_v4dat <- util_fbs_child_v4dat_lab(child_file_pattern, data_path)
        parent_v4dat <- util_fbs_parent_v4dat(parent_file_pattern, data_path)
    } else {
        child_v4dat <- util_fbs_child_v4dat(child_file_pattern)
        child_home_v4dat <- util_fbs_child_v4dat_home(child_file_pattern)
        child_lab_v4dat <- util_fbs_child_v4dat_lab(child_file_pattern)
        parent_v4dat <- util_fbs_parent_v4dat(parent_file_pattern)
    }

    #### 3. Merge Child Raw Data #####

    # merge child home and lab into single database

    ## weird merging issue related to labeled data - brute force fix so both have same ID's (NAs inserted for other vars)
    for (r in 1:nrow(child_lab_v4dat[['data']])){
        if (!(child_lab_v4dat[['data']][r, 'id'] %in% child_home_v4dat[['data']][, 'id'])){
            new_row <- nrow(child_home_v4dat[['data']]) + 1
            child_home_v4dat[['data']][new_row, 'id'] <- child_lab_v4dat[['data']][r, 'id']
        }
    }

    for (r in 1:nrow(child_home_v4dat[['data']])){
        if (!(child_home_v4dat[['data']][r, 'id'] %in% child_lab_v4dat[['data']][, 'id'])){
            new_row <- nrow(child_lab_v4dat[['data']]) + 1
            child_lab_v4dat[['data']][new_row, 'id'] <- child_home_v4dat[['data']][r, 'id']
        }
    }

    child_covidmerge_v4dat <- merge(child_lab_v4dat[['data']], child_home_v4dat[['data']][c(1, 3:21)], by = 'id', all = TRUE)

    #get wasi
    v4_covidmerge_wasi <- child_covidmerge_v4dat[c(1, 47:56)]
    names(v4_covidmerge_wasi)[3] <- 'wasi_dob'

    # re-order so matches child_v4dat
    child_covidmerge_v4dat <- child_covidmerge_v4dat[c(1:43, 57:75, 44:46)]
    names(child_covidmerge_v4dat)[3] = 'dob'

    # merge all child into single database - need to split of wasi first
    child_v4_wasi <- child_v4dat[['data']][c(1, 66:75)]
    names(child_v4_wasi)[3] <- 'wasi_dob'
    names(child_v4dat[['data']])[3] = 'dob'

    # merge all together
    all_child_v4dat <- rbind.data.frame(child_v4dat[['data']][1:65], child_covidmerge_v4dat)

    v4_wasi <- rbind.data.frame(child_v4_wasi, v4_covidmerge_wasi)

    #### 4. Merge Parent Raw Data #####

    # update labels with 'parent report'

    for (v in 1:ncol(parent_v4dat[['data']])) {
        var_name <- names(parent_v4dat[['data']])[v]

        # remove existing label
        if (grepl("parent-reported", parent_v4dat[['dict']][[var_name]], fixed = TRUE)) {
            parent_v4dat[['dict']][[var_name]] <- gsub("parent-reported", "", parent_v4dat[['dict']][[var_name]])
        }

        # add universal label
        parent_v4dat[['dict']][[var_name]] <- paste0('Parent Reported: ', parent_v4dat[['dict']][[var_name]])
    }

    v4dat <- merge(all_child_v4dat[c(1:3, 5:65)], parent_v4dat[['data']][c(1, 3:152)], by = 'id', all = TRUE)

    # merge labels/dictionary
    v4dat_labels <- c(child_v4dat[['dict']][c(1:3, 5:65)], parent_v4dat[['dict']][3:152])

    #### 5. Organize V4 data ####

    # order of vars: 1) Demographics, 2) Anthro (hw, DXA, sleep, PA), 3) Intake (FF, liking, intake, want), 4) feeding/food Q's, 5) cog/trait Q's, 6) Delay Discounting, 7) MRI related (CAMS, FF, snack info, image ratings), 8) Notes

    v4dat_org <- v4dat[c(1:3, 65:136, 52:61, 4:51, 138:201, 137, 62:64, 202:214)]

    v4dat_labels <- v4dat_labels[c(1:3, 65:136, 52:61, 4:51, 138:201, 137, 62:64, 202:214)]

    v4_wasi_labels <- child_v4dat[['dict']][c(1, 66:75)]
    names(v4_wasi_labels)[3] <- 'wasi_dob'
    v4_wasi_labels[['wasi_dob']] <- 'Participant DOB written on WASI'

    # ensure labels are up to date
    v4dat_org = sjlabelled::set_label(v4dat_org, label = matrix(unlist(v4dat_labels, use.names = FALSE)))
    v4_wasi = sjlabelled::set_label(v4_wasi, label = matrix(unlist(v4_wasi_labels, use.names = FALSE)))


    #### 6. Score V4 data ####

    ## 6a) score the US Household Food Security Survey Module: Three Stage ####
    hfssm_scored <- score_hfssm(hfssm_data = v4dat_org[c(1, 7:25)], parID = 'id')

    # get labels from scored data and simplify
    hfssm_scored_labels <- sapply(hfssm_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(hfssm_scored_labels) <- names(hfssm_scored)

    # merge and organize
    v4dat_scored <- merge(v4dat_org, hfssm_scored, by = 'id', all = TRUE)
    v4dat_scored <- v4dat_scored[c(1:25, 215:222, 26:214)]

    v4dat_scored_labels <- c(v4dat_labels[1:25], hfssm_scored_labels[2:9], v4dat_labels[26:214])

    ## 6b) score the Household Food Insecurity Access Scale ####
    hfias_scored <- score_hfias(hfias_data = v4dat_org[c(1, 26:43)], parID = 'id')

    # get labels from scored data and simplify
    hfias_scored_labels <- sapply(hfias_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(hfias_scored_labels) <- names(hfias_scored)

    # merge and organize
    v4dat_scored <- merge(v4dat_scored, hfias_scored, by = 'id', all = TRUE)
    v4dat_scored <- v4dat_scored[c(1:51, 223:224, 52:222)]

    v4dat_scored_labels <- c(v4dat_scored_labels[1:51], hfias_scored_labels[2:3], v4dat_scored_labels[52:222])

    ## 6c) score the Community Childhood Hunger Identification Project ####
    cchip_scored <- score_cchip(cchip_data = v4dat_org[c(1, 44:75)], parID = 'id')

    # get labels from scored data and simplify
    cchip_scored_labels <- sapply(cchip_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(cchip_scored_labels) <- names(cchip_scored)

    # merge and organize
    v4dat_scored <- merge(v4dat_scored, cchip_scored, by = 'id', all = TRUE)
    v4dat_scored <- v4dat_scored[c(1:85, 225:226, 86:224)]

    v4dat_scored_labels <- c(v4dat_scored_labels[1:85], cchip_scored_labels[2:3], v4dat_scored_labels[86:224])

    ## 6d) score the Child Weight Concerns Scale ####
    cwc_scored <- score_cwc(cwc_data = v4dat_org[c(1, 125:129)], parID = 'id')

    # get labels from scored data and simplify
    cwc_scored_labels <- sapply(cwc_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(cwc_scored_labels) <- names(cwc_scored)

    # merge and organize
    v4dat_scored <- merge(v4dat_scored, cwc_scored, by = 'id', all = TRUE)
    v4dat_scored <- v4dat_scored[c(1:141, 227, 142:226)]

    v4dat_scored_labels <- c(v4dat_scored_labels[1:141], cwc_scored_labels[2], v4dat_scored_labels[142:226])

    ## 6e) score the Children's Body Image Scale Questionnaire ####
    cbis_scored <- score_cbis(cbis_data = v4dat_org[c(1, 130:133)], parID = 'id')

    # get labels from scored data and simplify
    cbis_scored_labels <- sapply(cbis_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(cbis_scored_labels) <- names(cbis_scored)

    # merge and organize
    v4dat_scored <- merge(v4dat_scored, cbis_scored, by = 'id', all = TRUE)
    v4dat_scored <- v4dat_scored[c(1:146, 228:229, 147:227)]

    v4dat_scored_labels <- c(v4dat_scored_labels[1:146], cbis_scored_labels[2:3], v4dat_scored_labels[147:227])

    ## 6f) score the Behavioral Rating Inventory of Executive Function-2 ####
    brief_scored <- score_brief2(brief_data = v4dat_org[c(1, 4:5, 135:197)], age_var = 'age', sex_var = 'sex', parID = 'id')

    # get labels from scored data and simplify
    brief_scored_labels <- sapply(brief_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(brief_scored_labels) <- names(brief_scored)

    # merge and organize
    v4dat_scored <- merge(v4dat_scored, brief_scored, by = 'id', all = TRUE)
    v4dat_scored <- v4dat_scored[c(1:212, 230:277, 213:229)]

    v4dat_scored_labels <- c(v4dat_scored_labels[1:212], brief_scored_labels[2:49], v4dat_scored_labels[213:229])

    ## 6g) score the Parenting Style Inventory-II ####
    psi_scored <- score_psi(psi_data = v4dat_org[c(1, 76:85)], study = 'fbs', parID = 'id')

    # get labels from scored data and simplify
    psi_scored_labels <- sapply(psi_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(psi_scored_labels) <- names(psi_scored)

    # merge and organize
    v4dat_scored <- merge(v4dat_scored, psi_scored, by = 'id', all = TRUE)
    v4dat_scored <- v4dat_scored[c(1:97, 278:279, 98:277)]

    v4dat_scored_labels <- c(v4dat_scored_labels[1:97], psi_scored_labels[2:3], v4dat_scored_labels[98:277])


    #### 7. Food Intake ####

    v4_kcal <- fbs_kcal_intake(v4dat_scored[c(1, 114:138)], meal = 'ps_meal', parID = 'id')

    names(v4_kcal)[7:8] <- c('total_g', 'total_kcal')

    # get labels from scored data and simplify
    v4_kcal_labels <- sapply(v4_kcal, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(v4_kcal_labels) <- names(v4_kcal)

    # merge and organize
    v4dat_scored <- merge(v4dat_scored, v4_kcal, by = 'id', all = TRUE)
    v4dat_scored_labels <- c(v4dat_scored_labels, v4_kcal_labels[2:8])

    ## add portion size label
    v4dat_scored['meal_ps'] <- ifelse(is.na(v4dat_scored[['noplate_mac_cheese_g']]), NA, ifelse(v4dat_scored[['noplate_mac_cheese_g']] < 280, 0, ifelse(v4dat_scored[['noplate_mac_cheese_g']] < 360, 1, ifelse(v4dat_scored[['noplate_mac_cheese_g']] < 440, 2, 3))))

    v4dat_scored[["meal_ps"]] <- sjlabelled::add_labels(v4dat_scored[["meal_ps"]], labels = c(ps1 = 0, ps2 = 1, ps3 = 2, ps4 = 3))
    class(v4dat_scored[["meal_ps"]]) <- c("haven_labelled", "vctrs_vctr", "double")

    v4dat_scored_labels[['meal_ps']] <- 'Visit 4 Portion Size Meal Condition'

    # organize
    v4dat_scored <- v4dat_scored[c(1:113, 287, 114:117, 280, 118:121, 281, 122:125, 282, 126:130, 283, 131:134, 284, 135:138, 285:286, 139:279)]

    v4dat_scored_labels <- v4dat_scored_labels[c(1:113, 287, 114:117, 280, 118:121, 281, 122:125, 282, 126:130, 283, 131:134, 284, 135:138, 285:286, 139:279)]

    #### 8. WASI data ####
    v4dat_scored <- merge(v4dat_scored, v4_wasi, by = 'id')
    v4dat_scored <- v4dat_scored[c(1:270, 288:297, 271:287)]
    v4dat_scored_labels <- c(v4dat_scored_labels[1:270], v4_wasi_labels[2:11], v4dat_scored_labels[271:287])

    #### 9. PNA data #####

    # child pna data

    #find names in common and unique
    common_names <- intersect(names(child_v4dat[['pna_data']]), names(child_home_v4dat[['pna_data']]))
    child_v4dat_uniq_names <- c('id', names(child_v4dat[['pna_data']])[!(names(child_v4dat[['pna_data']]) %in% common_names)])
    child_home_v4dat_uniq_names <- c('id', names(child_home_v4dat[['pna_data']])[!(names(child_home_v4dat[['pna_data']]) %in% common_names)])

    # initial merge with common names
    child_v4dat_pna_m1 <- rbind.data.frame(child_v4dat[['pna_data']][common_names], child_home_v4dat[['pna_data']][common_names])

    # full merge with unique names
    if (length(child_v4dat_uniq_names) > 1 & length(child_home_v4dat_uniq_names) > 1){
        child_v4dat_m2 <- merge(child_v4dat_pna_m1, child_v4dat[['pna_data']][child_v4dat_uniq_names], by = 'id', all = TRUE)
        child_v4dat_pna <- merge(child_v4dat_m2, child_home_v4dat[['pna_data']][child_home_v4dat_uniq_names], by = 'id', all = TRUE)
    } else if (length(child_v4dat_uniq_names) > 1) {
        child_v4dat_pna <- merge(child_v4dat_pna_m1, child_v4dat[['pna_data']][child_v4dat_uniq_names], by = 'id', all = TRUE)
    } else if (length(child_home_v4dat_uniq_names) > 1){
        child_v4dat_pna <- merge(child_v4dat_m1, child_home_v4dat[['pna_data']][child_home_v4dat_uniq_names], by = 'id', all = TRUE)
    } else {
        child_v4dat_pna <- child_v4dat_m1
    }

    #labels
    child_v4dat_pna_labels <- lapply(child_v4dat_pna, function(x) attributes(x)$label)

    #loop through to get all labels
    for (v in 1:length(names(child_v4dat_pna))){
        var_name = names(child_v4dat_pna)[v]

        if (var_name %in% names(child_v4dat[['pna_data']])){
            child_v4dat_pna_labels[[var_name]] <- child_v4dat[['pna_dict']][[var_name]]
        } else if (var_name %in% names(child_home_v4dat[['pna_data']])){
            child_v4dat_pna_labels[[var_name]] <- child_home_v4dat[['pna_dict']][[var_name]]
        }
    }

    # parent pna data
    for (v in 1:ncol(parent_v4dat[['pna_data']])) {
        var_name <- names(parent_v4dat[['pna_data']])[v]

        # remove existing label
        if (grepl("parent-reported", parent_v4dat[['pna_dict']][[var_name]], fixed = TRUE)) {
            parent_v4dat[['pna_dict']][[var_name]] <- gsub("parent-reported", "", parent_v4dat[['pna_dict']][[var_name]])
        }

        # add universal label
        parent_v4dat[['pna_dict']][[var_name]] <- paste0('Parent Reported: ', parent_v4dat[['pna_dict']][[var_name]])
    }

    v4dat_pna <- merge(child_v4dat_pna, parent_v4dat[['pna_data']], by = 'id', all = TRUE)

    v4dat_pna_labels <- c(child_v4dat_pna_labels, parent_v4dat[['pna_dict']][2:length(parent_v4dat[['pna_dict']])])

    #### 10. save to list #####

    # put data in order of participant ID for ease
    v4dat_scored <- v4dat_scored[order(v4dat_scored[["id"]]), ]
    v4dat_pna <- v4dat_pna[order(v4dat_pna[["id"]]), ]

    # set labels
    v4dat_scored = sjlabelled::set_label(v4dat_scored, label = matrix(unlist(v4dat_scored_labels, use.names = FALSE)))
    v4dat_pna = sjlabelled::set_label(v4dat_pna, label = matrix(unlist(v4dat_pna_labels, use.names = FALSE)))

    v4data_list <- list(data = v4dat_scored, dict = v4dat_scored_labels, pna_data = v4dat_pna, pna_dict = v4dat_pna_labels)

    return(v4data_list)
}

