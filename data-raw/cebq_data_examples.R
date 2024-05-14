# This generates example CEBQ dataframes for use with score_cebq.R

#### Create CEBQ data frame, base 0 ####

# Define the column names vector
q_numbers <- seq(1, 35) # make a sequence of numbers for each q in CEBQ (1-35)
cebq_qs <- paste0("cebq", q_numbers) # make character vector of cebq item names
column_names <- c("ID", cebq_qs) # make character vector with ID variable and cebq item names 

# Create the dataframe
cebq_base0 <- data.frame(matrix(nrow = 0, ncol = length(column_names)))
colnames(cebq_base0) <- column_names

# add rows for 2 subjects to cebq_base0
cebq_base0[1, ] <- c("sub-001", 2, 0, 4, 2, 0, 1, 1, 3, 0, 1, 2, 2, 2, 0, 4, 3, 0, 2, 3, 2, 1, 4, 1, 4, 4, 1, 4, 1, 0, 0, 3, 3, 2, 0, 1)
cebq_base0[2, ] <- c("sub-002", 1, 4, 0, 3, 0, 2, 2, 3, 1, 4, 4, 2, 4, 2, 3, 3, 2, 1, 3, 4, 1, 0, 4, 1, 2, 0, 2, 1, 0, 4, 2, 3, 1, 2, NA)

# Convert columns 2 through 36 (cebq values) to numeric
cebq_base0[, 2:36] <- lapply(cebq_base0[, 2:36], as.numeric)

#### Create CEBQ data frame, base 1 ####
cebq_base1 <- data.frame(matrix(nrow = 2, ncol = length(column_names)))
colnames(cebq_base1) <- column_names

# assign the first column of cebq_base1 (ID) to be the same as the first column of cebq_base0
cebq_base1[, 1] <- cebq_base0[, 1]

# assign cebq values with base 1 to columns 2:36 by adding 1 to cebq values in cebq_base0
cebq_base1[, 2:36] <- cebq_base0[, 2:36] + 1

#### Save dataframes to package
usethis::use_data(cebq_base0, overwrite = FALSE)
usethis::use_data(cebq_base1, overwrite = FALSE)
