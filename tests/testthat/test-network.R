context("network")

test_that("merge works", {
  # prepare data
  data("mtcars")
  node_data <- tibble::tibble(name = colnames(mtcars), 
                              key = 1:ncol(mtcars))
  
  # correlation net
  net <- correlation_network(data=mtcars, 
                             node_data=node_data)
  
  # check result
  expect_equal(tidygraph::as_tibble(net), 
               node_data)
})
