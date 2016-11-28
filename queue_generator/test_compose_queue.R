#R


source("compose_queue.R")
require(testthat)
 

#test data
  test_data <- function(){
    extract.name <- c("Sample_1", "Sample_2", "Sample_3", "Sample_4", "Sample_5", "Sample_6", "Sample_7", "Sample_8", "Sample_9", "Sample_10", "Sample_11", "Sample_12", "Sample_13", "Sample_14", "Sample_15", "Sample_16", "Sample_17", "Sample_18", "Sample_19", "Sample_20" )
    extract.id <- c(1:20)
    Condition <- c("Control", "Control", "Control", "Control", "Ampicillin", "Ampicillin", "Ampicillin", "Ampicillin", "Kanamycin", "Kanamycin", "Kanamycin", "Kanamycin", "Less", "Less", "Less", "More", "More", "More", "More", "More")
    data.frame(extract.name, extract.id, Condition)
  }

test_that("test input format 2 injections", {
  res <- format_input_data(test_data(),2, "easylc")
  expect_true(res[20, 4] == "B2")
})

  
