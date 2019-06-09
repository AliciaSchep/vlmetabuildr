#' vl_chart
#'
#' Initialize a Vega-Lite specification! Can add any top level configuration 
#' parameters, or simply call without arguments to initialize and then use other
#' function (like [vl_mark_point()], [vl_encode_x()], etc) to add on the various
#' pieces of the chart spec. 
{param_docs}
#'
#' @return A vega-lite spec, as an S3 object of class vegaspec_vega_lite
#'  using [vegawidget::as_vegaspec()]
#' @export
#' @importFrom utils hasName
#' @examples 
#' 
#' vl_chart() %>%
#'   vl_add_data(values = mtcars) %>%
#'   vl_mark_point() %>%
#'   vl_encode_x("wt") %>%
#'   vl_encode_y("mpg") 
vl_chart <- function({arg_list}) {{
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  vegawidget::as_vegaspec(args_out)
}}
