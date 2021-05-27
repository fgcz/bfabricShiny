#R

context("bfabricShiny")

.test_generate_queue<- function(x, method="default"){
  generate_queue(x,
                      projectid = 3000,
                      area = "Proteomics",
                      instrument = "FUSION_2",
                      username = "roschi",
                      autoQC01 = "TRUE",
                      QC01o = 4,
                      QC01m = 1,
                      autoQC02 = "FALSE",
                      QC02o = 4,
                      QC02m = 1,
                      autoQC4L = "FALSE",
                      QC4Lo = 4,
                      QC4Lm = 1,
                      clean = "FALSE",
                      cleano = 4,
                      cleanm = 1,
                      start1 = 1,
                      start2 = NA,
                      start3 = NA,
                      end1 = 4,
                      end2 = 1,
                      end3 = 3,
                      lists = 1,
                      startposition = 1,
                      nr.methods = 1,
                      nr.replicates = 1,
                      qc.type = 1,
                      method = method,
                      pathprefix = "D:Data2San")
}



test_that("test default", {

 x <- bfabricShiny:::.test_data_medium()
 
  res.default <- .test_generate_queue(x, 'default')
  ground_trues <- c(NA,1,2,3,4,NA,5,6,7,8,NA,9,10,11,12,NA,13,14,15,16,NA,17,18,19,20,NA,NA,NA)
  
  expect_equal(as.integer(res.default$rv[,'Sample ID'] ),  ground_trues)
 
 
})

test_that("test random", {
  
  x <- bfabricShiny:::.test_data_medium()
  
  ground_trues <- c(NA, 3, 4, 10, 1, NA, 12, 15, 2, 20, NA, 13, 18, 7, 17, NA,
                    5, 9, 16, 14, NA, 8, 11, 6, 19, NA, NA, NA)
  
 
  set.seed(1)
  res.random.1 <- .test_generate_queue(x, 'random')
  #expect_equal(as.integer(res.random.1$rv[,'Sample ID'] ),  ground_trues)
  
  set.seed(1)
  res.random.2 <- .test_generate_queue(x, 'random')
  res.random.3 <- .test_generate_queue(x, 'random')
  
  expect_true(all.equal(res.random.1$rv[,'Sample ID'], res.random.2$rv[,'Sample ID']))
  expect_true(sum(res.random.1$rv[,'Sample ID'] != res.random.3$rv[,'Sample ID'], na.rm = TRUE)>1) 
})


  
