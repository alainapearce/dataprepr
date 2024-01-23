#' util_fbs_merge_micro: Merge all microstructure data
#'
#' This function merges the raw microstructure data into databases
#'
#' The databases MUST follow the naming convention: PS1_EventLogs_YYYY-MM-DD.txt, PS2_EventLogs_YYYY-MM-DD.txt, PS3_EventLogs_YYYY-MM-DD.txt, and PS4_EventLogs_YYYY-MM-DD.txt. The databases must all be in the SAME directory to be processed if the data_path is not entered and the directory organization does not follow the structure laid out in the DataManual.
#'
#' @inheritParams util_fbs_parent_v1dat
#'
#' @return A list containing 2 data lists:
#' 1) beh_wide contains - data: data.frame summary metrics in wide format by code, dict: data dictionary with variable descriptions
#' 2) event contains - data: event data by coder and time, dict: data dictionary with variable descriptions
#'
#' @examples
#' #if in same working directory as data with all data. Note - there is no need to add the COVID protocol (i.e., 'Home' or 'Lab') to file_pattern - these files will be search for automatically:
#' mciro_merged <- util_fbs_merge_micro(data_path = 'untouchedRaw/Microstructure_Raw/')
#'
#' \dontrun{
#' }
#'
#' @seealso Raw data is processed using \code{\link{util_fbs_microstructure}}
#'
#' @export

util_fbs_merge_micro <- function(data_path) {

    #### 1. Set up/initial checks #####
    # check datapath
    datapath_arg <- methods::hasArg(data_path)

    if (isTRUE(datapath_arg)) {
        if (!is.character(data_path)) {
            stop("data_path must be entered as a string: e.g., '.../Participant_Data/untouchedRaw/'")
        }
    }


    #### 2. Process Raw Data #####

    if (isTRUE(datapath_arg)) {
      ps1_data <- util_fbs_microstructure('PS1', data_path)
      ps2_data <- util_fbs_microstructure('PS2', data_path)
      ps3_data <- util_fbs_microstructure('PS3', data_path)
      ps4_data <- util_fbs_microstructure('PS4', data_path)
    } else {
      ps1_data <- util_fbs_microstructure('PS1')
      ps2_data <- util_fbs_microstructure('PS2')
      ps3_data <- util_fbs_microstructure('PS3')
      ps4_data <- util_fbs_microstructure('PS4')
    }

    #### 3. Merge Child Raw Data #####

    # merge wide beh
    ps1_data[['beh_wide']][['data']]['ps'] <- 1
    ps2_data[['beh_wide']][['data']]['ps'] <- 2
    ps3_data[['beh_wide']][['data']]['ps'] <- 3
    ps4_data[['beh_wide']][['data']]['ps'] <- 4

    beh_wide_data <- rbind.data.frame(ps1_data[['beh_wide']][['data']], ps2_data[['beh_wide']][['data']], ps3_data[['beh_wide']][['data']], ps4_data[['beh_wide']][['data']])

    beh_wide_dict <- ps1_data[['beh_wide']][['dict']]
    beh_wide_dict[['ps']] <- 'Portion Size'

    # merge event data
    ps1_data[['event']][['data']]['ps'] <- 1
    ps2_data[['event']][['data']]['ps'] <- 2
    ps3_data[['event']][['data']]['ps'] <- 3
    ps4_data[['event']][['data']]['ps'] <- 4

    event_data <- rbind.data.frame(ps1_data[['event']][['data']], ps2_data[['event']][['data']], ps3_data[['event']][['data']], ps4_data[['event']][['data']])

    event_dict <- ps1_data[['event']][['dict']]
    event_dict[['ps']] <- 'Portion Size'

    # set labels
    beh_wide_data = sjlabelled::set_label(beh_wide_data, label = matrix(unlist(beh_wide_dict, use.names = FALSE)))
    event_data = sjlabelled::set_label(event_data, label = matrix(unlist(event_dict, use.names = FALSE)))

    mcirodata_list <- list(beh_wide = list(data = beh_wide_data, dict = beh_wide_dict), event = list(data = event_data, dict = event_dict))

    return(mcirodata_list)
}

