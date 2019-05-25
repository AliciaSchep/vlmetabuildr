#' vl_sort_{enc}
#' 
#' Sort {enc} encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
#' @param value either 'ascending' or 'descending' to specify sort order (using 
#' this encoding), a list with a custom ordering, or NA to specify no sorting
#' @return A modified spec
#' @seealso [vl_sort_{enc}_by_field()] to sort by another field, 
#' [vl_sort_{enc}_by_encoding()] to sort by another encoding
#' @export
vl_sort_{enc} <- function({arg_list}) {{
  args_out <- list(spec = spec, .enc = '{enc}', value = value)
  rlang::exec(.add_sort_to_encoding, !!!args_out)
}}