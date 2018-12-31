context("edgelist")

test_that("all parameter choices of edgelist work", {
  data("mtcars")
  
  # parameter list
  parameters <- expand.grid(
    correlation_method = c('pearson', 'kendall', 'spearman'), 
    use = c('everything', 'all.obs', 'complete.obs', 'na.or.complete', 'pairwise.complete.obs'), 
    threshold_method = c("pvalue", "absolute"), 
    adjust = stats::p.adjust.methods)
  
  # use ecgelist function
  res <- apply(parameters, 1, function(row){
    correlation_edgelist(data=mtcars, 
                         correlation_method = row['correlation_method'], 
                         use = row['use'], 
                         threshold_method = row['threshold_method'], 
                         adjust = row['adjust'])
  })
  res_rows <- sapply(res, nrow)
  
  # check results 
  expect_length(res, nrow(parameters))
  expect_true(all(res_rows <= ncol(mtcars) * (ncol(mtcars) - 1)))
  lapply(res, expect_named)
  lapply(res, expect_type, 'list')
})