create_chart <- function(schema) {
  
  # Get all props...
  chart_props <- props(schema, list("$ref" = "#/definitions/TopLevelSpec"))
  chart_names <- unique(c("data", names(chart_props)))
  chart_names <- stringr::str_replace(chart_names, "^repeat$","`repeat`")
  defaults <- rep("NULL", length(chart_names))
  defaults[which(chart_names == "$schema")] = "vegawidget::vega_schema()"
  chart_names[which(chart_names == "$schema")] = "`$schema`"
  chart_args <- paste(chart_names,defaults, sep = " = ")
  arg_list <- paste(chart_args, collapse = ", ")
  
  param_docs <- get_param_docs(schema, "#/definitions/TopLevelSpec")
  
  template <- system.file(file.path("templates","template_chart.R"), 
                          package = 'vlmetabuildr')
  glargs <- list(
    arg_list = arg_list,
    param_docs = param_docs
  )
  
  glue::glue_data(glargs, readr::read_file(template), .trim = FALSE)
  
}


create_properties <- function(schema) {
  alt <- c("layer","repeat","concat","hconcat","vconcat","facet","mark",
           "transform","selection","resolve","data","config","$schema","spec",
           "encoding")
  chart_props <- props(schema, list("$ref" = "#/definitions/TopLevelSpec"))
  chart_names <- setdiff(unique(names(chart_props)), alt)
  chart_args <- paste(chart_names, "NULL", sep = " = ")
  arg_list <- paste(c('spec', chart_args), collapse = ", ")
  
  param_docs <- get_param_docs(schema, "#/definitions/TopLevelSpec", only = chart_names)
  
  create_pass_function(
    function_suffix = glue("add_properties"), 
    recipient_function = ".add_property",
    arg_list = arg_list,
    doc_description = glue("#' Add properties to top level of a vega-lite spec. Allows adding", 
                           "\n#' properties like width, height, background which don't have a",
                           "\n#' a specific function for adding them (unlike `mark` or `encoding`)."),
    extra_docs = glue("#' @seealso [vl_chart()], [vl_config()]"),
    param_docs = param_docs)   
  
}