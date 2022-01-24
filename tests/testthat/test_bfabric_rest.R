#R

context("bfabricShiny")

test_that("test read", {
  expect_error(bfabricShiny::read(endpoint='user',
                                    query=list(login='cpanse')))
  
  res <- bfabricShiny::read(endpoint='user', query=list(login='cpanse'), login = login, webservicepassword = webservicepassword)
  
  expect_equal(res[[1]][[1]]$login , "cpanse")
  
  expect_error(.createWorkunit())
  
})


  
