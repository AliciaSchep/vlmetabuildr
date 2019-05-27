create_resolve <- function(enc, type, schema) {
  
  resolve_options <- unlist(enums(schema, 
                               list("$ref" = glue("#/definitions/ResolveMode"))))

  resolve_opt_list <- paste("'",resolve_options,"'", sep = "",collapse = ", ")
  resolve_args <- glue::glue_data(
    list(opts = resolve_opt_list),
    "how = c({opts})"
  )
  arg_list <- paste(c('spec', resolve_args), collapse = ", ")
  
  param_docs <- glue("#' @param how how to resolve {type} for {enc}, one of ",
                     "{paste(resolve_options, collapse = ', ')}")
  
  create_custom_pass_function(
    function_suffix = glue("resolve_{type}_{enc}"), 
    recipient_function = ".add_resolve",
    arg_list = arg_list,
    modify_args = 
      glue("args_out <- list(spec = spec, .enc = '{enc}', .type = '{type}', how = match.arg(how))"),
    doc_description = glue("#' Add resolving axis definitions to a vega-lite spec."),
    param_docs = param_docs)   
  
}

create_resolve_axis_functions <- function(schema){
  
  encs <- names(schema$definitions$AxisResolveMap$properties)
  purrr::map_chr(encs, create_resolve, type = "axis", schema = schema)
  
}

create_resolve_scale_functions <- function(schema){
  
  encs <- names(schema$definitions$ScaleResolveMap$properties)
  purrr::map_chr(encs, create_resolve, type = "scale", schema = schema)
  
}

create_resolve_legend_functions <- function(schema){
  
  encs <- names(schema$definitions$ScaleResolveMap$properties)
  purrr::map_chr(encs, create_resolve, type = "legend", schema = schema)
  
}

#' @export
create_resolve_functions <- function(schema) {
  
  c(
    create_resolve_axis_functions(schema),
    create_resolve_legend_functions(schema),
    create_resolve_scale_functions(schema)
  )
}
