create_resolve <- function(enc, type, schema) {
  
  resolve_options <- unlist(enums(schema, 
                               list("$ref" = glue("#/definitions/ResolveMode"))))

  resolve_opt_list <- paste("'",resolve_options,"'", sep = "",collapse = ", ")
  resolve_args <- glue::glue_data(
    list(opts = resolve_opt_list),
    "how = c({opts})"
  )
  arg_list <- paste(c('spec', resolve_args), collapse = ", ")
  
  create_custom_pass_function(
    function_suffix = glue("resolve_{type}_{enc}"), 
    recipient_function = ".add_resolve",
    arg_list = arg_list,
    modify_args = 
      glue("args_out <- list(spec = spec, .enc = '{enc}', .type = '{type}', how = match.arg(how))"),
    group = 'resolve')   
  
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
  
  resolve_options <- unlist(enums(schema, 
                                  list("$ref" = glue("#/definitions/ResolveMode"))))
  
  resolve_opt_list <- paste("'",resolve_options,"'", sep = "",collapse = ", ")
  param_docs <- glue("#' @param how how to resolve, one of: ",
                     "{resolve_opt_list}")
  
  resolve_doc <- create_group_docs(
    doc_name = "resolve", 
    doc_title = "Resolve axes, legends, or scales for composite charts",
    doc_description = paste0(
      "#' When faceting, layering, repeating, or concatenating a chart, one\n",
      "#' can choose whether axes, legends, or scales are shared or independent\n",
      "#' using the resolve specification"),
    param_docs =  param_docs) 
  
  c(
    resolve_doc,
    create_resolve_axis_functions(schema),
    create_resolve_legend_functions(schema),
    create_resolve_scale_functions(schema)
  )
}
