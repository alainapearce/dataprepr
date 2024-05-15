#### Generate base 0 dataframe for testing ####

# Define the column names vector
q_numbers <- seq(1, 35) # make a sequence of numbers for each q in CEBQ (1-35)
cebq_qs <- paste0("cebq", q_numbers) # make character vector of cebq item names
column_names <- c("ID", cebq_qs) # make character vector with ID variable and cebq item names 

# Initialize dataframe
test_cebq_base0 <- data.frame(matrix(nrow = 0, ncol = length(column_names)))

# add columns names
colnames(test_cebq_base0) <- column_names

# Make vectors that define items in each Wardle 2001 subscale
dd_items <- c("cebq6", "cebq29", "cebq31")
ef_items <- c("cebq1", "cebq5", "cebq20", "cebq22")
eoe_items <- c("cebq2", "cebq13", "cebq15", "cebq27")
eue_items <- c("cebq9", "cebq11", "cebq23", "cebq25")
ff_items_forwardscore <-c("cebq7", "cebq24", "cebq33")
ff_items_reversescore <-c("cebq10", "cebq16", "cebq32") # 10, 16, 32 are reverse scored
fr_items <-c("cebq12", "cebq14", "cebq19", "cebq28", "cebq34")
se_items_forwardscore <-c("cebq8", "cebq18", "cebq35")
se_items_reversescore <-c("cebq4") # 4 is reverse scored
sr_items_forwardscore <-c("cebq17", "cebq21", "cebq26", "cebq30")
sr_items_reversescore <-c("cebq3") # 3 is reverse scored

# manzano subscales
approach_items <- c(dd_items, ef_items, eoe_items, fr_items)
avoid_items <- c(eue_items, ff_items_forwardscore, ff_items_reversescore, se_items_forwardscore, se_items_reversescore, sr_items_forwardscore, sr_items_reversescore)

# manzano 2021 subscales
rbe_items <- c() # Mean of items 1,3 (reverse), 4 (reverse), 5,8 (reverse),12,14,19,20,22,28,34
pe_items <- c("cebq7", "cebq10", "cebq16", "cebq24", "cebq32", "cebq33") # Picky eating: Mean of items 7,10,16,24,32,33
ee_items <- c("cebq2", "cebq9", "cebq13", "cebq15", "cebq23", "cebq25") # Mean of items 2,9,13,15,23,25.

# Add specific values to row 1 by subscale to facilitate testing 
test_cebq_base0[1,c("ID")] <- "sub-001" 
test_cebq_base0[1,dd_items] <- 0 
test_cebq_base0[1,ef_items] <- 1
test_cebq_base0[1,eoe_items] <- 2
test_cebq_base0[1,eue_items] <- 3
test_cebq_base0[1, ff_items_forwardscore] <- 0
test_cebq_base0[1, ff_items_reversescore] <- 4 # reverse scores from score_cebq() should be 0
test_cebq_base0[1, fr_items] <- 3
test_cebq_base0[1, se_items_forwardscore] <- 0
test_cebq_base0[1, se_items_reversescore] <- 4 # reverse scores from score_cebq() should be 0
test_cebq_base0[1, sr_items_forwardscore] <- 0
test_cebq_base0[1, sr_items_reversescore] <- 4 # reverse scores from score_cebq() should be 0

# generate data with nas
test_cebq_base0_nas <- test_cebq_base0
test_cebq_base0_nas[c("cebq1", "cebq2")] <- NA

# generate data base 1
test_cebq_base1 <- test_cebq_base0
test_cebq_base1[2:36] <- test_cebq_base0[2:36] + 1

#### Make dataframe of expected scores ####
expected_scores <-
  data.frame(
    ID = "sub-001",
    cebq_fr = 4,
    cebq_eoe = 3,
    cebq_ef = 2,
    cebq_dd = 1,
    cebq_sr = 1,
    cebq_se = 1,
    cebq_eue = 4,
    cebq_ff = 1,
    cebq_approach = as.numeric(round(rowMeans(test_cebq_base0[1,approach_items]),3) + 1),
    cebq_avoid = round(mean(c(rep(3, 4), rep(0, 6), rep(0, 4),rep(0, 5))), 3) + 1,
    #      Mean of items 1,5,12,14,19,20,22,28,34, 3 (reverse), 4 (reverse) ,8 (reverse)
    cebq_rbe = round(mean(c(1, 1, 3, 3, 3, 1, 1, 3, 3, 0, 0, 4)), 3) + 1,
    cebq_pe = round(rowMeans(test_cebq_base0[1,pe_items]),3) + 1,
    cebq_ee = round(rowMeans(test_cebq_base0[1,ee_items]),3) + 1
  )

expected_scores_nas <-
  data.frame(
    ID = "sub-001",
    cebq_fr = 4,
    cebq_eoe = as.double(NA),
    cebq_ef = as.double(NA),
    cebq_dd = 1,
    cebq_sr = 1,
    cebq_se = 1,
    cebq_eue = 4,
    cebq_ff = 1,
    cebq_approach = as.numeric(round(rowMeans(test_cebq_base0_nas[1,approach_items]),3) + 1),
    cebq_avoid = round(mean(c(rep(3, 4), rep(0, 6), rep(0, 4),rep(0, 5))), 3) + 1,
    cebq_rbe = as.double(NA),
    cebq_pe = round(rowMeans(test_cebq_base0_nas[1,pe_items]),3) + 1,
    cebq_ee = as.double(NA)
  )

# set rownames to be integer 
rownames(expected_scores) <- as.integer(rownames(expected_scores))
rownames(expected_scores_nas) <- as.integer(rownames(expected_scores_nas))

#### Run function ####

scored_base0 <- score_cebq(test_cebq_base0, base_zero = TRUE, id = "ID")
scored_base1 <- score_cebq(test_cebq_base1, base_zero = FALSE, id = "ID")
scored_base0_nas <- score_cebq(test_cebq_base0_nas, base_zero = TRUE, id = "ID")

#### Run tests ####
test_that("scores accurately computed", {

  # test scores are identical if appropriately computed with base 0 or base 1 data
  expect_identical(scored_base0$score_dat,scored_base1$score_dat)
  
  # test score_cebq() scores are equal to expected scores
  expect_equal(scored_base0$score_dat[c(colnames(expected_scores))],expected_scores)
  expect_equal(scored_base1$score_dat[c(colnames(expected_scores))],expected_scores)
  expect_equal(scored_base0_nas$score_dat[c(colnames(expected_scores_nas))],expected_scores_nas)
  
})



test_that("bids_phenotype includes input data plus scores", {

  merged <- merge(test_cebq_base0, expected_scores)
  
  # note: this only works because test_cebq_base0 did not have underscores in col names. otherwise, colnames would be modified
  expect_equal(scored_base0$bids_phenotype, merged)
  
  # test removal of underscores in a different dataframe?

})

test_that("warning if values are out of expected bounds", {

  # expect warning if range is 0-4 and base_zero = FALSE
  expect_warning(score_cebq(test_cebq_base0, base_zero = FALSE))

  # expect warning if range is 1-5 and base_zero = TRUE
  expect_warning(score_cebq(test_cebq_base1, base_zero = TRUE))

})

test_that("errors if arguments don't meet expectations ", {

  # expect error if base_zero isn't logical
  expect_error(score_cebq(test_cebq_base0, base_zero = "FALSE"))
  
  # expect error if ID isn't in database
  expect_error(score_cebq(test_cebq_base0, id = "ilovecats"))
})
