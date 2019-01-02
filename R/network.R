#' Correlation network
#' 
#' Correlation network as a tidy graph
#' 
#' @param data input data, a numeric matrix or data frame. 
#' @param node_data additional data frame containing information for each node. 
#' Should have a column `'name'` that contains the column names of the data. 
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
#' graph_cors <- correlation_network(mtcars)
#' graph_cors
#' \dontrun{
#' require(ggraph)
#' set.seed(42)
#' ggraph(graph_cors, layout = "graphopt") + 
#'   geom_edge_link(aes(color = r), width = 1.3) +
#'   geom_node_label(aes(label = name)) + 
#'   scale_edge_colour_gradient2(limits = c(-1, 1)) +
#'   theme_graph()
#' }
#' 
#' @importFrom tidygraph as_tbl_graph activate
#' @importFrom dplyr right_join
#' @importFrom tibble tibble
#' @export
correlation_network <- function(
  data, 
  node_data = tibble::tibble(name = colnames(data)),
  correlation_method = c('pearson', 'kendall', 'spearman'), 
  use = c('everything', 'all.obs', 'complete.obs', 'na.or.complete', 'pairwise.complete.obs'),
  threshold = 0.05, 
  threshold_method = c('pvalue', 'absolute'), 
  adjust = stats::p.adjust.methods)
{
  # checks
  stopifnot('name' %in% colnames(node_data))
  # get edgelist
  edgelist <- correlation_edgelist(
    data = data, 
    correlation_method = correlation_method, 
    use = use, 
    threshold = threshold, 
    threshold_method = threshold_method, 
    adjust = adjust)
  # make graph
  graph_df <- tidygraph::as_tbl_graph(edgelist, directed = FALSE)
  tidygraph::activate(graph_df, 'nodes')
  dplyr::right_join(graph_df, node_data, by = "name")
  return(graph_df)
}