#' Correlation edgelist
#' 
#' Calculate the edgelist of a correlation network
#' 
#' @param data input data, a numeric matrix or data frame. 
#' @param correlation_method a character string indicating which correlatin coefficient to compute. 
#' One of `"pearson"` (default), `"kendall"`, or `"spearman"`. 
#' @param use a character string indicating how to deal with missing values. 
#' One of `"everything"`, `"all.obs"`, `"complete.obs"`, `"na.or.complete"`, or `"pairwise.complete.obs"`.
#' @param threshold a numeric threshold 
#' @param threshold_method a character string indicating how correlations should be thresholded. 
#' One of `"pvalue"` (default), or `"absolute"`. 
#' @param adjust a character string indicating how p-values should be adjusted for multiple comparisons. 
#' See \link[stats]{p.adjust.methods} for all methods. 
#' 
#' @examples 
#' data('mtcars')
#' correlation_edgelist(mtcars)
#' 
#' @importFrom corrr correlate stretch as_cordf
#' @importFrom dplyr left_join
#' @importFrom stats cor.test p.adjust.methods p.adjust
#' @export
correlation_edgelist <- function(
  data, 
  correlation_method = c('pearson', 'kendall', 'spearman'), 
  use = c('everything', 'all.obs', 'complete.obs', 'na.or.complete', 'pairwise.complete.obs'),
  threshold = 0.05, 
  threshold_method = c('pvalue', 'absolute'), 
  adjust = stats::p.adjust.methods)
{
  # match arguments
  correlation_method <- match.arg(correlation_method)
  use <- match.arg(use)
  threshold_method <- match.arg(threshold_method)
  adjust <- match.arg(adjust)
  
  # calculate correlations
  tidy_cors <- corrr::stretch(
    corrr::correlate(data, use = use, method = correlation_method, 
                     quiet = TRUE))
  # remove na's
  tidy_cors <- tidy_cors[!is.na(tidy_cors$r), ]
  
  if (threshold_method == 'pvalue'){
    # calculate p values
    p_vals <- corrr::stretch(corrr::as_cordf(matrix(
      stats::p.adjust(apply(expand.grid(seq(ncol(data)), seq(ncol(data))), 1, 
                            function(ix){
                              if (ix[1] == ix[2]) return(NA)
                              stats::cor.test(data[, ix[1]], data[, ix[2]], 
                                              method = correlation_method)$p.value
                            }),
                      method = adjust),
      nrow=ncol(data), ncol=ncol(data), 
      dimnames = list(colnames(data), colnames(data)))))
    colnames(p_vals) <- c('x', 'y', 'p')
    
    # merge
    tidy_cors <- dplyr::left_join(tidy_cors, p_vals, by = c('x', 'y'))
    
    # subset
    edges <- tidy_cors[tidy_cors$p < threshold, ]
  } else {
    edges <- tidy_cors[abs(tidy_cors$r) > threshold, ]
  }
  
  return(edges)
}
