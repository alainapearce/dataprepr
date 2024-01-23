#' fbs_visit_databases: Generate databases for the Food and Brain Study based on study visit
#'
#' This function will generate up-to-date databases for the Food and Brain Study based on visit only. If multiple visits are entered, the data will be merged into a signle database
#'
#' To process the raw data, the raw databases from Qualtrics MUST follow the naming convention: Child_V1_YYYY-MM-DD.sav, Child_V1_Home_YYY-MM-DD.sav, Child_V1_Lab_YYY-MM-DD.sav, and Parent_V1_YYY-MM-DD.sav. The databases must all be in the SAME directory to be processed if the data_path is not entered and the directory organization does not follow the structure laid out in the DataManual.
#'
#' @param visit1 (optional) logical indicator if want visit 1 data in database. Default = FALSE.
#' @param visit2 (optional) logical indicator if want visit 2 data in database. Default = FALSE.
#' @param visit3 (optional) logical indicator if want visit 3 data in database. Default = FALSE.
#' @param visit4 (optional) logical indicator if want visit 4 data in database. Default = FALSE.
#' @param visit5 (optional) logical indicator if want visit 5 data in database. Default = FALSE.
#' @param visit6 (optional) logical indicator if want visit 6 data in database. Default = FALSE.
#' @param visit7 (optional) logical indicator if want visit 7 data in database. Default = FALSE.
#' @param model_DD If visit3 is set to TRUE, indicate if delay discounting data should be modeled. This will take an addition 3-5 minutes of processing time. Default = FALSE.
#' @inheritParams fbs_databases
#' @inheritParams fbs_databases
#' @inheritParams fbs_databases
#' @inheritParams util_fbs_parent_v1dat
#' @inheritParams fbs_databases
#' @inheritParams fbs_databases
#' @inheritParams fbs_databases
#'
#' @return A list containing all databases that were generated
#'
#' @examples
#' #if in same working directory as data, want delay discounting data modeled:
#' fbs_data_proc <- fbs_visit_databases(visit3 = TRUE, model_DD = TRUE)
#'
#' #if only want the full visit 1 database returned to the terminal:
#' fbs_data_proc <- fbs_visit_databases(visit1 = TRUE, write_dat = FALSE)
#'
#' #if want the visit 1 and visit 6 data written to the working directory:
#' fbs_data_proc <- fbs_visit_databases(visit1 = TRUE, visit6 = TRUE)
#'
#' \dontrun{
#'
#' }
#'
#' @seealso Raw data from Qualtrics is processed using the following scripts: \code{\link{util_fbs_merge_v1dat}}, \code{\link{util_fbs_merge_v2dat}}, \code{\link{util_fbs_merge_v3dat}}, \code{\link{util_fbs_merge_v4dat}}, \code{\link{util_fbs_merge_v5dat}}, \code{\link{util_fbs_merge_v6dat}}, \code{\link{util_fbs_merge_v7dat}}
#'
#'
#' @export

fbs_visit_databases <- function(visit1 = FALSE, visit2 = FALSE, visit3 = FALSE, visit4 = FALSE, visit5 = FALSE, visit6 = FALSE, visit7 = FALSE, model_DD = FALSE, write_dat = TRUE, write_path, return_data = FALSE, data_path, child_file_pattern, parent_file_pattern, visit_file_pattern) {

    #### 1. Set up/initial checks #####

    # check if date_str exists and is a string

    c_filepat_arg <- methods::hasArg(child_file_pattern)

    if (isTRUE(c_filepat_arg)) {
        if (!is.character(child_file_pattern)) {
            stop("child_file_pattern must be entered as a string: e.g., 'Child'")
        } else {
            child_fp <- child_file_pattern
        }
    } else if (isFALSE(c_filepat_arg)) {
        child_fp <- 'Child'
    }

    p_filepat_arg <- methods::hasArg(parent_file_pattern)

    if (isTRUE(p_filepat_arg)) {
        if (!is.character(parent_file_pattern)) {
            stop("parent_file_pattern must be entered as a string: e.g., 'Parent'")
        } else {
            parent_fp <- parent_file_pattern
        }
    } else if (isFALSE(p_filepat_arg)) {
        parent_fp <- 'Parent'
    }

    v_filepat_arg <- methods::hasArg(visit_file_pattern)

    if (isTRUE(p_filepat_arg)) {
        if (!is.character(visit_file_pattern)) {
            stop("parent_file_pattern must be entered as a string: e.g., 'V' or 'Visit_'")
        } else {
            visit_fp <- visit_file_pattern
        }
    } else if (isFALSE(p_filepat_arg)) {
        visit_fp <- 'V'
    }

    # check datapath
    datapath_arg <- methods::hasArg(data_path)

    if (isTRUE(datapath_arg)) {
      if (!is.character(data_path)) {
        stop("data_path must be entered as a string: e.g., '.../Participant_Data/untouchedRaw/")
      }

      #make universal to 'Untouched_Raw'
      if (grepl('Qualtrics_Raw', data_path, fixed = TRUE)){
        data_path <- gsub('Qualtrics_Raw', '', data_path)
      } else if (grepl('Microstructure_Raw', data_path, fixed = TRUE)){
        data_path <- gsub('Microstructure_Raw', '', data_path)
      }
    }

    # check write_path
    writepath_arg <- methods::hasArg(write_path)

    if (isTRUE(writepath_arg)) {
        if (!is.character(write_path)) {
            stop("write_path must be entered as a string: e.g., '.../Participant_Data/Databases/")
        }
    }

    #### 2. Get Visit Databases #####

    ## get empty list
    database_list <- list()

    ## figure out which visits are TRUE
    visits_true <- c(visit1, visit2, visit3, visit4, visit5, visit6, visit7)


    ## Visit 1 data ####
    if (isTRUE(visit1)){
      if (isTRUE(datapath_arg)){
        v1_data <- util_fbs_merge_v1(child_file_pattern = paste0(child_fp, '_', visit_fp, '1'), parent_file_pattern = paste0(parent_fp, '_', visit_fp, '1'), data_path = paste0(data_path, '/Qualtrics_Raw/'))
      } else {
        v1_data <- util_fbs_merge_v1(child_file_pattern = paste0(child_fp, '_', visit_fp, '1'), parent_file_pattern = paste0(parent_fp, '_', visit_fp, '1'))
      }

        #get data
        visit1_data <- v1_data[['data']]
        visit1_labels <- v1_data[['dict']]

        names(visit1_data)[2] <- 'v1_date'
        names(visit1_labels)[2] <- 'v1_date'

        # add to list
        database_list <- c(database_list, list(v1_data = visit1_data, v1_dict = visit1_labels))

        # write out
        if (isTRUE(write_dat) & sum(visits_true) == 1){

            #data dictionary
            visit1_dict <- labelled::generate_dictionary(visit1_data, details = TRUE)
            visit1_dict$label <- matrix(unlist(visit1_labels, use.names = FALSE))
            names(visit1_dict)[1] <- 'column'

            #interprets the value_labels as list so need to make everything a character
            visit1_dict_write <- sapply(visit1_dict[c(1:3, 6:8, 12:13)], FUN = as.character)

            if (isTRUE(writepath_arg)){
                haven::write_sav(visit1_data, path = paste0(write_path, 'visit1_data.sav'))
                write.csv(visit1_dict_write, file = paste0(write_path, 'dict-visit1_data.csv'), row.names = FALSE)
            } else {
                haven::write_sav(intake_data, path = 'visit1_data.sav')
                write.csv(intake_dict_write, file = 'dict-visit1_data.csv', row.names = FALSE)
            }
        }
    }


    ## Visit 2 data ####
    if (isTRUE(visit2)){

      if (isTRUE(datapath_arg)){
        v2_data <- util_fbs_merge_v2(child_file_pattern = paste0(child_fp, '_', visit_fp, '2'), parent_file_pattern = paste0(parent_fp, '_', visit_fp, '2'), parentV4_file_pattern = paste0(parent_fp, '_', visit_fp, '4'), data_path = paste0(data_path, '/Qualtrics_Raw/'))
      } else {
        v2_data <- util_fbs_merge_v2(child_file_pattern = paste0(child_fp, '_', visit_fp, '2'), parent_file_pattern = paste0(parent_fp, '_', visit_fp, '2'), parentV4_file_pattern = paste0(parent_fp, '_', visit_fp, '4'))
      }

        #get data
        visit2_data <- v2_data[['data']]
        visit2_labels <- v2_data[['dict']]

        names(visit2_data)[2] <- 'v2_date'
        names(visit2_labels)[2] <- 'v2_date'

        # add to list
        database_list <- c(database_list, list(v2_data = visit2_data, v2_dict = visit2_labels))

        # write out
        if (isTRUE(write_dat) & sum(visits_true) == 1){

            #data dictionary
            visit2_dict <- labelled::generate_dictionary(visit2_data, details = TRUE)
            visit2_dict$label <- matrix(unlist(visit2_labels, use.names = FALSE))
            names(visit2_dict)[1] <- 'column'

            #interprets the value_labels as list so need to make everything a character
            visit2_dict_write <- sapply(visit2_dict[c(1:3, 6:8, 12:13)], FUN = as.character)

            if (isTRUE(writepath_arg)){
                haven::write_sav(visit2_data, path = paste0(write_path, 'visit2_data.sav'))
                write.csv(visit2_dict_write, file = paste0(write_path, 'dict-visit2_data.csv'), row.names = FALSE)
            } else {
                haven::write_sav(intake_data, path = 'visit2_data.sav')
                write.csv(intake_dict_write, file = 'dict-visit2_data.csv', row.names = FALSE)
            }
        }
    }

    ## Visit 3 data ####
    if (isTRUE(visit3)){

      if (isTRUE(datapath_arg)){
        v3_data <- util_fbs_merge_v3(child_file_pattern = paste0(child_fp, '_', visit_fp, '3'), parent_file_pattern = paste0(parent_fp, '_', visit_fp, '3'), data_path = paste0(data_path, '/Qualtrics_Raw/'), model_DD = model_DD)
      } else {
        v3_data <- util_fbs_merge_v3(child_file_pattern = paste0(child_fp, '_', visit_fp, '3'), parent_file_pattern = paste0(parent_fp, '_', visit_fp, '3'), model_DD = model_DD)
      }

        #get data
        visit3_data <- v3_data[['data']]
        visit3_labels <- v3_data[['dict']]

        names(visit3_data)[2] <- 'v3_date'
        names(visit3_labels)[2] <- 'v3_date'

        # add to list
        database_list <- c(database_list, list(v3_data = visit3_data, v3_dict = visit3_labels))

        # write out
        if (isTRUE(write_dat) & sum(visits_true) == 1){

            #data dictionary
            visit3_dict <- labelled::generate_dictionary(visit3_data, details = TRUE)
            visit3_dict$label <- matrix(unlist(visit3_labels, use.names = FALSE))
            names(visit3_dict)[1] <- 'column'

            #interprets the value_labels as list so need to make everything a character
            visit3_dict_write <- sapply(visit3_dict[c(1:3, 6:8, 12:13)], FUN = as.character)

            if (isTRUE(writepath_arg)){
                haven::write_sav(visit3_data, path = paste0(write_path, 'visit3_data.sav'))
                write.csv(visit3_dict_write, file = paste0(write_path, 'dict-visit3_data.csv'), row.names = FALSE)
            } else {
                haven::write_sav(intake_data, path = 'visit3_data.sav')
                write.csv(intake_dict_write, file = 'dict-visit3_data.csv', row.names = FALSE)
            }
        }
    }

    ## Visit 4 data ####
    if (isTRUE(visit4)){
      if (isTRUE(datapath_arg)){
        v4_data <- util_fbs_merge_v4(child_file_pattern = paste0(child_fp, '_', visit_fp, '4'), parent_file_pattern = paste0(parent_fp, '_', visit_fp, '4'), data_path = paste0(data_path, '/Qualtrics_Raw/'))
      } else {
        v4_data <- util_fbs_merge_v4(child_file_pattern = paste0(child_fp, '_', visit_fp, '4'), parent_file_pattern = paste0(parent_fp, '_', visit_fp, '4'))
      }

        #get data
        visit4_data <- v4_data[['data']]
        visit4_labels <- v4_data[['dict']]

        names(visit4_data)[2] <- 'v4_date'
        names(visit4_labels)[2] <- 'v4_date'

        # add to list
        database_list <- c(database_list, list(v4_data = visit4_data, v4_dict = visit4_labels))

        # write out
        if (isTRUE(write_dat) & sum(visits_true) == 1){

            #data dictionary
            visit4_dict <- labelled::generate_dictionary(visit4_data, details = TRUE)
            visit4_dict$label <- matrix(unlist(visit4_labels, use.names = FALSE))
            names(visit4_dict)[1] <- 'column'

            #interprets the value_labels as list so need to make everything a character
            visit4_dict_write <- sapply(visit4_dict[c(1:3, 6:8, 12:13)], FUN = as.character)

            if (isTRUE(writepath_arg)){
                haven::write_sav(visit4_data, path = paste0(write_path, 'visit4_data.sav'))
                write.csv(visit4_dict_write, file = paste0(write_path, 'dict-visit4_data.csv'), row.names = FALSE)
            } else {
                haven::write_sav(intake_data, path = 'visit4_data.sav')
                write.csv(intake_dict_write, file = 'dict-visit4_data.csv', row.names = FALSE)
            }
        }
    }

    ## Visit 5 data ####
    if (isTRUE(visit5)){
      if (isTRUE(datapath_arg)){
        v5_data <- util_fbs_merge_v5(child_file_pattern = paste0(child_fp, '_', visit_fp, '5'), parent_file_pattern = paste0(parent_fp, '_', visit_fp, '5'), data_path = paste0(data_path, '/Qualtrics_Raw/'))
      } else {
        v5_data <- util_fbs_merge_v5(child_file_pattern = paste0(child_fp, '_', visit_fp, '5'), parent_file_pattern = paste0(parent_fp, '_', visit_fp, '5'))
      }

        #get data
        visit5_data <- v5_data[['data']]
        visit5_labels <- v5_data[['dict']]

        names(visit5_data)[2] <- 'v5_date'
        names(visit5_labels)[2] <- 'v5_date'

        # add to list
        database_list <- c(database_list, list(v5_data = visit5_data, v5_dict = visit5_labels))

        # write out
        if (isTRUE(write_dat) & sum(visits_true) == 1){

            #data dictionary
            visit5_dict <- labelled::generate_dictionary(visit5_data, details = TRUE)
            visit5_dict$label <- matrix(unlist(visit5_labels, use.names = FALSE))
            names(visit5_dict)[1] <- 'column'

            #interprets the value_labels as list so need to make everything a character
            visit5_dict_write <- sapply(visit5_dict[c(1:3, 6:8, 12:13)], FUN = as.character)

            if (isTRUE(writepath_arg)){
                haven::write_sav(visit5_data, path = paste0(write_path, 'visit5_data.sav'))
                write.csv(visit5_dict_write, file = paste0(write_path, 'dict-visit5_data.csv'), row.names = FALSE)
            } else {
                haven::write_sav(intake_data, path = 'visit5_data.sav')
                write.csv(intake_dict_write, file = 'dict-visit5_data.csv', row.names = FALSE)
            }
        }
    }

    ## Visit 6 data ####
    if (isTRUE(visit6)){
      if (isTRUE(datapath_arg)){
        v6_data <- util_fbs_merge_v6(child_file_pattern = paste0(child_fp, '_', visit_fp, '6'), parent_file_pattern = paste0(parent_fp, '_', visit_fp, '6'), data_path = paste0(data_path, '/Qualtrics_Raw/'))
      } else {
        v6_data <- util_fbs_merge_v6(child_file_pattern = paste0(child_fp, '_', visit_fp, '6'), parent_file_pattern = paste0(parent_fp, '_', visit_fp, '6'))
      }

        #get data
        visit6_data <- v6_data[['data']]
        visit6_labels <- v6_data[['dict']]

        names(visit6_data)[2] <- 'v6_date'
        names(visit6_labels)[2] <- 'v6_date'

        # add to list
        database_list <- c(database_list, list(v6_data = visit6_data, v6_dict = visit6_labels))

        # write out
        if (isTRUE(write_dat) & sum(visits_true) == 1){

            #data dictionary
            visit6_dict <- labelled::generate_dictionary(visit6_data, details = TRUE)
            visit6_dict$label <- matrix(unlist(visit6_labels, use.names = FALSE))
            names(visit6_dict)[1] <- 'column'

            #interprets the value_labels as list so need to make everything a character
            visit6_dict_write <- sapply(visit6_dict[c(1:3, 6:8, 12:13)], FUN = as.character)

            if (isTRUE(writepath_arg)){
                haven::write_sav(visit6_data, path = paste0(write_path, 'visit6_data.sav'))
                write.csv(visit6_dict_write, file = paste0(write_path, 'dict-visit6_data.csv'), row.names = FALSE)
            } else {
                haven::write_sav(intake_data, path = 'visit6_data.sav')
                write.csv(intake_dict_write, file = 'dict-visit6_data.csv', row.names = FALSE)
            }
        }
    }

    ## Visit 7 data ####
    if (isTRUE(visit7)){
      if (isTRUE(datapath_arg)){
        v7_data <- util_fbs_merge_v7(child_file_pattern = paste0(child_fp, '_', visit_fp, '7'), parent_file_pattern = paste0(parent_fp, '_', visit_fp, '7'), data_path = paste0(data_path, '/Qualtrics_Raw/'))
      } else {
        v7_data <- util_fbs_merge_v7(child_file_pattern = paste0(child_fp, '_', visit_fp, '7'), parent_file_pattern = paste0(parent_fp, '_', visit_fp, '7'))
      }

        #get data
        visit7_data <- v7_data[['data']]
        visit7_labels <- v7_data[['dict']]

        names(visit7_data)[2] <- 'v7_date'
        names(visit7_labels)[2] <- 'v7_date'

        if(isTRUE(visit1)){
            #re-name variables with 'v7_' and add 'Visit 7 - ' to labels
            for (v in 3:ncol(visit7_data)){
                #names
                var_name <- names(visit7_data)[v]
                v7_name <- paste0('v7_', var_name)
                names(visit7_data)[v] <- v7_name

                #labels
                visit7_labels[[var_name]] <- paste0('Visit 7 - ', visit7_labels[[var_name]])
            }

            #make names match
            names(visit7_labels) <- names(visit7_data)
        }

        # add to list
        database_list <- c(database_list, list(v7_data = visit7_data, v7_dict = visit7_labels))

        # write out
        if (isTRUE(write_dat) & sum(visits_true) == 1){

            #data dictionary
            visit7_dict <- labelled::generate_dictionary(visit7_data, details = TRUE)
            visit7_dict$label <- matrix(unlist(visit7_labels, use.names = FALSE))
            names(visit7_dict)[1] <- 'column'

            #interprets the value_labels as list so need to make everything a character
            visit7_dict_write <- sapply(visit7_dict[c(1:3, 6:8, 12:13)], FUN = as.character)

            if (isTRUE(writepath_arg)){
                haven::write_sav(visit7_data, path = paste0(write_path, 'visit7_data.sav'))
                write.csv(visit7_dict_write, file = paste0(write_path, 'dict-visit7_data.csv'), row.names = FALSE)
            } else {
                haven::write_sav(intake_data, path = 'visit7_data.sav')
                write.csv(intake_dict_write, file = 'dict-visit7_data.csv', row.names = FALSE)
            }
        }
    }

    # 3. Merge Databases (if needed) ####
    if (sum(visits_true) > 1){

        #names of data and labels in database_list
        visit_data_names <- c('v1_data', 'v2_data', 'v3_data', 'v4_data', 'v5_data', 'v6_data', 'v7_data')
        visit_data_labels <- c('v1_dict', 'v2_dict', 'v3_dict', 'v4_dict', 'v5_dict', 'v6_dict', 'v7_dict')

        #index visit data and labels
        visits_merge_data <- visit_data_names[visits_true]
        visits_merge_labels <- visit_data_labels[visits_true]

        #loop through
        for (v in 2:length(visits_merge_data)){
            if (v == 2) {
                #merge data
                data_name1 <- visits_merge_data[[1]]
                data_name2 <- visits_merge_data[[v]]

                merge_data <- merge(database_list[[data_name1]], database_list[[data_name2]], by = 'id', all = TRUE)

                #merge labels
                labels_name1 <- visits_merge_labels[[1]]
                labels_name2 <- visits_merge_labels[[v]]
                n_labels <- length(database_list[[labels_name2]])

                merge_labels <- c(database_list[[labels_name1]], database_list[[labels_name2]][2:n_labels])
            } else {
                data_name <- visits_merge_data[[v]]

                merge_data <- merge(merge_data, database_list[[data_name]], by = 'id', all = TRUE)

                #merge labels
                labels_name <- visits_merge_labels[[v]]
                n_labels <- length(database_list[[labels_name]])

                merge_labels <- c(merge_labels, database_list[[labels_name]][2:n_labels])
            }

        }

        # write out
        if (isTRUE(write_dat)){

            #get visit numbers for naming
            visit_num <- c(1, 2, 3, 4, 5, 6, 7)
            name_visits <- paste0(visit_num[visits_true], collapse = '_v')
            name_visits <- paste0('Database_v', name_visits)

            #data dictionary
            merge_data_dict <- labelled::generate_dictionary(merge_data, details = TRUE)
            merge_data_dict$label <- matrix(unlist(merge_labels, use.names = FALSE))
            names(merge_data_dict)[1] <- 'column'

            #interprets the value_labels as list so need to make everything a character
            merge_data_dict_write <- sapply(merge_data_dict[c(1:3, 6:8, 12:13)], FUN = as.character)

            if (isTRUE(writepath_arg)){
                haven::write_sav(merge_data, path = paste0(write_path, name_visits, '.sav'))
                write.csv(merge_data_dict_write, file = paste0(write_path, name_visits, '_Dictionnary.csv'), row.names = FALSE)
            } else {
                haven::write_sav(merge_data, path = paste0(name_visits, '.sav'))
                write.csv(merge_data_dict_write, file = paste0(name_visits, '_Dictionnary.csv'), row.names = FALSE)
            }
        }
    }

    #### 10. Return  List #####

    if(isTRUE(return_data)){
        return(database_list)
    }
}

