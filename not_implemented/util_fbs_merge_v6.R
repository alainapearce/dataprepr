#' util_fbs_merge_v6: Merge all Visit 6 raw Qualtrics databases and organize variable in database order
#'
#' This function merges the following visit 6 raw data into a single database and organizes variables in database order: child visit 6, child visit 6-home, child visit 6-lab, and parent visit 6
#'
#' The databases MUST follow the naming convention: Child_V6_YYYY-MM-DD.sav, Child_V6_Home_YYY-MM-DD.sav, Child_V6_Lab_YYY-MM-DD.sav, and Parent_V6_YYY-MM-DD.sav. The databases must all be in the SAME directory to be processed if the data_path is not entered and the directory organization does not follow the structure laid out in the DataManual.
#'
#' @inheritParams util_fbs_merge_v1
#' @inheritParams util_fbs_merge_v1
#' @inheritParams util_fbs_parent_v1dat
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from parent visit 6 Qualtrics and 2) dict: all variable descriptions
#'
#' @examples
#' #if in same working directory as data with all data. Note - there is no need to add the COVID protocol (i.e., 'Home' or 'Lab') to file_pattern - these files will be search for automatically:
#' v6dat_scored <- util_fbs_merge_v6(child_file_pattern = 'Child_V6', parent_file_pattern = 'Parent_V6')
#'
#' \dontrun{
#' #child_file_pattern and parent_file_pattern must be a strings. The following will not run:
#' v6dat_scored <- util_fbs_merge_v6(child_file_pattern = Child_V6, parent_file_pattern = Parent_V6)
#'
#' # *_file_pattern must have the respondent ('Child') and visit number ('V6'). If just enter 'Child' or 'Parent', the script will not run because it will return multiple files for different child visits. The following will not run:
#' v6dat_scored <- util_fbs_merge_v6(child_file_pattern = 'Child', parent_file_pattern = 'Parent')
#' }
#'
#' @seealso Raw data from Qualtrics is processed using the following scripts: \code{\link{util_fbs_child_v6dat}}, \code{\link{util_fbs_parent_v6dat}}. Visit 6 data is scored using the following scripts: \code{\link{score_mrivas}}.
#'
#'
#' @export

util_fbs_merge_v6 <- function(child_file_pattern, parent_file_pattern, data_path) {

    #### 1. Set up/initial checks #####
    # check that file_pattern exist and is a string

    c_filepat_arg <- methods::hasArg(child_file_pattern)

    if (isTRUE(c_filepat_arg) & !is.character(child_file_pattern)) {
        stop("child_file_pattern must be entered as a string: e.g., 'Child_V6'")
    } else if (isFALSE(c_filepat_arg)) {
        stop("child_file_pattern must set to the a string matching the name of the raw data file for child visit: e.g., 'Child_V6'")
    }

    p_filepat_arg <- methods::hasArg(parent_file_pattern)

    if (isTRUE(p_filepat_arg) & !is.character(parent_file_pattern)) {
        stop("parent_file_pattern must be entered as a string: e.g., 'Parent_V6'")
    } else if (isFALSE(p_filepat_arg)) {
        stop("parent_file_pattern must set to the a string matching the name of the raw data file for child visit: e.g., 'Parent_V6'")
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
        child_v6dat <- util_fbs_child_v6dat(child_file_pattern, data_path)
        parent_v6dat <- util_fbs_parent_v6dat(parent_file_pattern, data_path)
    } else {
        child_v6dat <- util_fbs_child_v6dat(child_file_pattern)
        parent_v6dat <- util_fbs_parent_v6dat(parent_file_pattern)
    }

    #### 3. Merge Child and Parent Raw Data #####

    # update labels with 'parent report'

    for (v in 1:ncol(parent_v6dat[['data']])) {
        var_name <- names(parent_v6dat[['data']])[v]

        # remove existing label
        if (grepl("parent-reported", parent_v6dat[['dict']][[var_name]], fixed = TRUE)) {
            parent_v6dat[['dict']][[var_name]] <- gsub("parent-reported", "", parent_v6dat[['dict']][[var_name]])
        }

        # add universal label
        parent_v6dat[['dict']][[var_name]] <- paste0('Parent Reported: ', parent_v6dat[['dict']][[var_name]])
    }

    v6dat <- merge(child_v6dat[['data']], parent_v6dat[['data']][c(1, 3:18)], by = 'id', all = TRUE)

    # merge labels/dictionary
    v6dat_labels <- c(child_v6dat[['dict']], parent_v6dat[['dict']][3:18])

    # ensure labels are up to date
    v6dat = sjlabelled::set_label(v6dat, label = matrix(unlist(v6dat_labels, use.names = FALSE)))

    #### 4. save to list #####

    # put data in order of participant ID for ease
    v6dat <- v6dat[order(v6dat[["id"]]), ]

    # set labels
    v6dat = sjlabelled::set_label(v6dat, label = matrix(unlist(v6dat_labels, use.names = FALSE)))

    v6data_list <- list(data = v6dat, dict = v6dat_labels)

    return(v6data_list)
}

