create_encoding_param_functions <- function(schema) {
  c(
    create_bin_encoding_functions(schema),
    create_impute_encoding_functions(schema),
    create_stack_encoding_functions(schema),
    create_aggregate_encoding_functions(schema),
    create_sort_encoding_functions(schema),
    create_axis_encoding_functions(schema),
    create_scale_encoding_functions(schema),
    create_legend_encoding_functions(schema),
    create_condition_encoding_functions(schema)
  )
}

get_enc_with_prop <- function(schema, prop) {
  encs <- names(props(schema, list("$ref" = "#/definitions/Encoding")))
  with_prop <-
    purrr::map_lgl(
      encs, 
      function(x) {
        prop_names <- names(
          props(schema, 
                list("$ref" = glue("#/definitions/Encoding/properties/{x}"))
          ))
        prop %in% prop_names
      }
    )
  encs[with_prop]
}


create_bin_encoding_functions <- function(schema) {
  
  param_docs <- get_param_docs(schema, "#/definitions/BinParams")
  bin_doc <- create_group_docs(
    doc_name = "bin_encoding", 
    doc_title = "Add bin transform to encoding",
    doc_description = "#' Add binning or bin parameters to an encoding",
    param_docs =  param_docs) 
  
  c(bin_doc,
    purrr::map_chr(get_enc_with_prop(schema,"bin"), 
                   create_bin_for_encoding, 
                   schema = schema)
  )
}

create_bin_for_encoding <- function(enc, schema){
  
  bin_props <- props(schema, list("$ref" = "#/definitions/BinParams"))
  bin_args <- paste(names(bin_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(bin_args)), collapse = ", ")
  
  create_function_for_encode_param(
    enc,
    'bin',
    arg_list
  )
}


create_impute_encoding_functions <- function(schema) {
  
  param_docs <- get_param_docs(schema, "#/definitions/ImputeParams")
  impute_doc <- create_group_docs(
    doc_name = "impute_encoding", 
    doc_title = "Add impute transform to encoding",
    doc_description = "#' Add impute parameters to an encoding",
    param_docs =  param_docs) 
  
  c(impute_doc,
    purrr::map_chr(get_enc_with_prop(schema,"impute"), 
                   create_impute_for_encoding, 
                   schema = schema)
  )
}

create_impute_for_encoding <- function(enc, schema){
  
  impute_props <- props(schema, list("$ref" = "#/definitions/ImputeParams"))
  impute_args <- paste(names(impute_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(impute_args)), collapse = ", ")
  

  create_function_for_encode_param(
    enc,
    'impute',
    arg_list
  )
  
}


create_stack_encoding_functions <- function(schema) {
  
  
  stack_options <- unlist(enums(schema, list("$ref" = "#/definitions/StackOffset")))
  stack_opt_list <- paste(c(paste("'",stack_options,"'", sep = ""),NA), 
                          collapse = ", ")
  param_docs <- glue::glue_data(
    list(opts = stack_opt_list),
    "#' @param stack one of {stack_opt_list}"
  )
  
  stack_doc <- create_group_docs(
    doc_name = "stack_encoding", 
    doc_title = "Add stack transform to encoding",
    doc_description = "#' Add stack parameters to an encoding",
    param_docs =  param_docs,
    extra_docs = glue("#' @seealso [vl_stack()] to add a top level stack transform")) 
  
  c(stack_doc,
    purrr::map_chr(get_enc_with_prop(schema,"stack"), 
                   create_stack_for_encoding, 
                   schema = schema)
  )
}


create_stack_for_encoding <- function(enc, schema){
  
  stack_options <- unlist(enums(schema, list("$ref" = "#/definitions/StackOffset")))
  stack_opt_list <- paste(c(paste("'",stack_options,"'", sep = ""),NA), 
                          collapse = ", ")
  stack_args <- glue::glue_data(
    list(opts = stack_opt_list),
    "stack = c({opts})"
  )
  arg_list <- paste(c('spec', unique(stack_args)), collapse = ", ")
  
  
  create_custom_pass_function (
    function_suffix = glue("stack_{enc}"), 
    recipient_function = ".add_stack_to_encoding",
    arg_list = arg_list,
    modify_args = 
      glue("args_out <- list(spec = spec, .enc = '{enc}', stack = match.arg(stack))"),
    group = "stack_encoding" 
  )
  
}

create_aggregate_encoding_functions <- function(schema) {
  
  
  agg_options <- unlist(enums(schema, list("$ref" = "#/definitions/Aggregate")))
  agg_opt_list <- paste(c(paste("'",agg_options,"'", sep = ""),NA), 
                        collapse = ", ")

  param_docs <- glue::glue_data(
    list(opts = agg_opt_list),
    "#' @param op Aggregation op, one of {agg_opt_list}"
  )
  
  aggregate_doc <- create_group_docs(
    doc_name = "aggregate_encoding", 
    doc_title = "Add aggregate transform to encoding",
    doc_description = "#' Add aggregate parameters to an encoding",
    param_docs =  param_docs,
    extra_docs = 
      glue("#' @seealso [vl_aggregate()] to add a top level aggregate transform")) 
  
  c(aggregate_doc,
    purrr::map_chr(get_enc_with_prop(schema,"aggregate"), 
                   create_aggregate_for_encoding, 
                   schema = schema)
  )
}

create_aggregate_for_encoding <- function(enc, schema){
  
  agg_options <- unlist(enums(schema, list("$ref" = "#/definitions/Aggregate")))
  agg_opt_list <- paste(c(paste("'",agg_options,"'", sep = ""),NA), 
                        collapse = ", ")
  agg_args <- glue::glue_data(
    list(opts = agg_opt_list),
    "op = c({opts})"
  )
  arg_list <- paste(c('spec', unique(agg_args)), collapse = ", ")
  
  
  create_custom_pass_function(
    function_suffix = glue("aggregate_{enc}"), 
    recipient_function = ".add_aggregate_to_encoding",
    arg_list = arg_list,
    modify_args = 
      glue("args_out <- list(spec = spec, .enc = '{enc}', op = match.arg(op))"),
    group = 'aggregate_encoding'
  )
  
}

create_sort_encoding_functions <- function(schema) {
  
  param_docs <- paste0(
    "#' @param value either 'ascending' or 'descending' to specify sort order (using\n",
    "#' this encoding), a list with a custom ordering, or NA to specify no sorting")
  sort_doc <- create_group_docs(
    doc_name = "sort_encoding", 
    doc_title = "Add sort transform to encoding",
    doc_description = "#' Add sort parameters to an encoding",
    param_docs =  param_docs,
    extra_docs = "#' @seealso [sort_encoding_by_field()] and [sort_encoding_by_encoding()]") 
  
  param_docs_bf <- get_param_docs(schema, "#/definitions/EncodingSortField")
  sort_doc_bf <- create_group_docs(
    doc_name = "sort_encoding_by_field", 
    doc_title = "Add sort transform by field to encoding",
    doc_description = "#' Add sort by field parameters to an encoding",
    param_docs =  param_docs_bf,
    extra_docs = "#' @seealso [sort_encoding()] and [sort_encoding_by_encoding()]") 
  
  param_docs_be <- get_param_docs(schema, "#/definitions/SortByEncoding")
  sort_doc_be <- create_group_docs(
    doc_name = "sort_encoding_by_encoding", 
    doc_title = "Add sort transform by encoding to encoding",
    doc_description = "#' Add sort by encoding parameters to an encoding",
    param_docs =  param_docs_be,
    extra_docs =  "#' @seealso [sort_encoding()] and [sort_encoding_by_field()]") 
  
  sort_encs <- get_enc_with_prop(schema,"sort")
  
  c(sort_doc,
    sort_doc_bf,
    sort_doc_be,
    purrr::map_chr(sort_encs, 
                   create_sort_for_encoding, 
                   schema = schema),
    purrr::map_chr(sort_encs, 
                   create_sort_by_field_for_encoding, 
                   schema = schema),
    purrr::map_chr(sort_encs, 
                   create_sort_by_encoding_for_encoding, 
                   schema = schema)
  )
  
}

create_sort_for_encoding <- function(enc, schema) {
  
  create_pass_function(
    function_suffix = glue("sort_{enc}"), 
    recipient_function = ".add_sort_to_encoding",
    arg_list = "spec, value",
    modify_args = glue("args_out <- c(args_out, list(.enc = '{enc}'))"),
    group = "sort_encoding"
  )
}



create_sort_by_field_for_encoding <- function(enc, schema){
  
  sort_props <- props(schema, list("$ref" = "#/definitions/EncodingSortField"))
  sort_args <- paste(names(sort_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(sort_args)), collapse = ", ")
  
  create_pass_function(
    function_suffix = glue("sort_{enc}_by_field"), 
    recipient_function = ".add_sort_obj_to_encoding",
    arg_list = arg_list,
    modify_args = glue("args_out <- c(args_out, list(.enc = '{enc}'))"),
    group = 'sort_encoding_by_field'
  )
  
}


create_sort_by_encoding_for_encoding <- function(enc, schema){
  
  sort_props <- props(schema, list("$ref" = "#/definitions/SortByEncoding"))
  sort_args <- paste(names(sort_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(sort_args)), collapse = ", ")
  
  
  create_pass_function(
    function_suffix = glue("sort_{enc}_by_encoding"), 
    recipient_function = ".add_sort_obj_to_encoding",
    arg_list = arg_list,
    modify_args = glue("args_out <- c(args_out, list(.enc = '{enc}'))"),
    group = 'sort_encoding_by_encoding'
  )
  
}

create_axis_encoding_functions <- function(schema) {
  
  param_docs <- paste0(get_param_docs(schema, "#/definitions/Axis"),
                       "\n#' @param remove Remove the axis?")
  axis_doc <- create_group_docs(
    doc_name = "axis_encoding", 
    doc_title = "Add axis to encoding",
    doc_description = "#' Add axis parameters to an encoding",
    param_docs =  param_docs) 
  
  c(axis_doc,
    purrr::map_chr(get_enc_with_prop(schema,"axis"), 
                   create_axis_for_encoding, 
                   schema = schema)
  )
}

create_axis_for_encoding <- function(enc, schema) {
  
  axis_props <- props(schema, list("$ref" = "#/definitions/Axis"))
  axis_args <- paste(names(axis_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(axis_args), "remove = FALSE"), collapse = ", ")
  
  create_function_for_encode_param(
    enc,
    'axis',
    arg_list
  )
  
}

create_scale_encoding_functions <- function(schema) {
  
  param_docs <- get_param_docs(schema, "#/definitions/Scale")
  scale_doc <- create_group_docs(
    doc_name = "scale_encoding", 
    doc_title = "Add scale to encoding",
    doc_description = "#' Add scale parameters to an encoding",
    param_docs =  param_docs) 
  
  c(scale_doc,
    purrr::map_chr(get_enc_with_prop(schema,"scale"), 
                   create_scale_for_encoding, 
                   schema = schema)
  )
}

create_scale_for_encoding <- function(enc, schema) {
  
  scale_props <- props(schema, list("$ref" = "#/definitions/Scale"))
  scale_args <- paste(names(scale_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(scale_args)), collapse = ", ")
  
  
  create_function_for_encode_param(
    enc,
    'scale',
    arg_list
  )
  
}

create_legend_encoding_functions <- function(schema) {
  
  param_docs <- get_param_docs(schema, "#/definitions/Legend")
  legend_doc <- create_group_docs(
    doc_name = "legend_encoding", 
    doc_title = "Add legend to encoding",
    doc_description = "#' Add legend parameters to an encoding",
    param_docs =  param_docs) 
  
  c(legend_doc,
    purrr::map_chr(get_enc_with_prop(schema,"legend"), 
                   create_legend_for_encoding, 
                   schema = schema)
  )
}

create_legend_for_encoding <- function(enc, schema) {
  
  legend_props <- props(schema, list("$ref" = "#/definitions/Legend"))
  legend_args <- paste(names(legend_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(legend_args)), collapse = ", ")
  
  create_function_for_encode_param(
    enc,
    'legend',
    arg_list
  )
  
}

create_condition_encoding_functions <- function(schema) {
  
  param_docs <- get_param_docs(
    schema, 
    c("#/definitions/ConditionalValueDef","#/definitions/ConditionalMarkPropFieldDef")
  )
  
  condition_doc <- create_group_docs(
    doc_name = "condition_encoding", 
    doc_title = "Add condition to encoding",
    doc_description = "#' Add condition parameters to an encoding",
    param_docs =  param_docs) 
  
  c(condition_doc,
    purrr::map_chr(get_enc_with_prop(schema,"condition"), 
                   create_condition_for_encoding, 
                   schema = schema)
  )
}

create_condition_for_encoding <- function(enc, schema) {
  
  condition_props <- c(
    props(schema, list("$ref" = "#/definitions/ConditionalValueDef")),
    props(schema, list("$ref" = "#/definitions/ConditionalMarkPropFieldDef"))
  )
  condition_args <- paste(names(condition_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(condition_args)), collapse = ", ")
  
  
  create_function_for_encode_param(
    enc,
    'condition',
    arg_list
  )
  
}
