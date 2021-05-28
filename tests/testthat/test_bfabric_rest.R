#R

context("bfabricShiny")

test_that("test read", {
  expect_error(bfabricShiny::read(endpoint='user',
                                    query=list(login='cpanse')))
  
  expect_error(.createWorkunit())
  
})


  
