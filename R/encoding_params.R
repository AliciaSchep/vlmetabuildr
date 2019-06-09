create_encoding_param_functions <- function(schema) {
  c(
    purrr::map_chr(get_enc_with_prop(schema,"bin"), 
                   create_bin_for_encoding, 
                   schema = schema),
    purrr::map_chr(get_enc_with_prop(schema,"stack"), 
                   create_stack_for_encoding, 
                   schema = schema),
    purrr::map_chr(get_enc_with_prop(schema,"sort"), 
                   create_sort_for_encoding, 
                   schema = schema),
    purrr::map_chr(get_enc_with_prop(schema,"sort"), 
                   create_sort_by_field_for_encoding, 
                   schema = schema),
    purrr::map_chr(get_enc_with_prop(schema,"sort"), 
                   create_sort_by_encoding_for_encoding, 
                   schema = schema),
    purrr::map_chr(get_enc_with_prop(schema,"impute"), 
                   create_impute_for_encoding, 
                   schema = schema),
    purrr::map_chr(get_enc_with_prop(schema,"aggregate"), 
                   create_aggregate_for_encoding, 
                   schema = schema),
    purrr::map_chr(get_enc_with_prop(schema,"axis"), 
                   create_axis_for_encoding, 
                   schema = schema),
    purrr::map_chr(get_enc_with_prop(schema,"scale"), 
                   create_scale_for_encoding, 
                   schema = schema),
    purrr::map_chr(get_enc_with_prop(schema,"legend"), 
                   create_legend_for_encoding, 
                   schema = schema),
    purrr::map_chr(get_enc_with_prop(VL_SCHEMA,"condition"), 
                   create_condition_for_encoding, 
                   schema = VL_SCHEMA)
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


create_bin_for_encoding <- function(enc, schema){
  
  bin_props <- props(schema, list("$ref" = "#/definitions/BinParams"))
  bin_args <- paste(names(bin_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(bin_args)), collapse = ", ")
  
  param_docs <- get_param_docs(schema, "#/definitions/BinParams")
  
  create_function_for_encode_param(
    enc,
    'bin',
    arg_list,
    param_docs
  )
}


create_impute_for_encoding <- function(enc, schema){
  
  impute_props <- props(schema, list("$ref" = "#/definitions/ImputeParams"))
  impute_args <- paste(names(impute_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(impute_args)), collapse = ", ")
  
  param_docs <- get_param_docs(schema, "#/definitions/ImputeParams")
  
  create_function_for_encode_param(
    enc,
    'impute',
    arg_list,
    param_docs
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
  
  param_docs <- glue::glue_data(
    list(opts = stack_opt_list),
    "#' @param stack one of {stack_opt_list}"
  )
  
  create_custom_pass_function(
    function_suffix = glue("stack_{enc}"), 
    recipient_function = ".add_stack_to_encoding",
    arg_list = arg_list,
    modify_args = 
      glue("args_out <- list(spec = spec, .enc = '{enc}', stack = match.arg(stack))"),
    doc_description = glue("#' Add stack to encoding for {enc} to a vega-lite spec."),
    extra_docs = glue("#' @seealso [vl_stack()] to add a top level stack transform"),
    param_docs = param_docs
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
  
  param_docs <- glue::glue_data(
    list(opts = agg_opt_list),
    "#' @param op Aggregation op, one of {agg_opt_list}"
  )
  
  create_custom_pass_function(
    function_suffix = glue("aggregate_{enc}"), 
    recipient_function = ".add_aggregate_to_encoding",
    arg_list = arg_list,
    modify_args = 
      glue("args_out <- list(spec = spec, .enc = '{enc}', op = match.arg(op))"),
    doc_description = 
      glue("#' Add aggregate to encoding for {enc} to a vega-lite spec."),
    extra_docs = 
      glue("#' @seealso [vl_aggregate()] to add a top level aggregate transform"),
    param_docs = param_docs
  )
  
}


create_sort_for_encoding <- function(enc, schema){
  
  create_pass_function(
    function_suffix = glue("sort_{enc}"), 
    recipient_function = ".add_sort_to_encoding",
    arg_list = "spec, value",
    modify_args = glue("args_out <- c(args_out, list(.enc = '{enc}'))"),
    doc_description = glue("#' Add sort to encoding for {enc} to a vega-lite spec."),
    extra_docs = glue("#' @seealso [vl_sort_{enc}_by_field()] to sort by another field,",
                      "\n#' [vl_sort_{enc}_by_encoding()] to sort by another encoding"),
    param_docs = paste0(
      "#' @param value either 'ascending' or 'descending' to specify sort order (using\n",
      "#' this encoding), a list with a custom ordering, or NA to specify no sorting")
  )
}



create_sort_by_field_for_encoding <- function(enc, schema){
  
  sort_props <- props(schema, list("$ref" = "#/definitions/EncodingSortField"))
  sort_args <- paste(names(sort_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(sort_args)), collapse = ", ")
  
  param_docs <- get_param_docs(schema, "#/definitions/EncodingSortField")
  
  create_pass_function(
    function_suffix = glue("sort_{enc}_by_field"), 
    recipient_function = ".add_sort_obj_to_encoding",
    arg_list = arg_list,
    modify_args = glue("args_out <- c(args_out, list(.enc = '{enc}'))"),
    doc_description = glue("#' Add sort by another field to encoding for {enc} in a vega-lite spec."),
    extra_docs = glue("#' @seealso [vl_sort_{enc}] to sort by same field,",
                      "\n#' [vl_sort_{enc}_by_encoding()] to sort by another encoding"),
    param_docs = param_docs
  )
  
}


create_sort_by_encoding_for_encoding <- function(enc, schema){
  
  sort_props <- props(schema, list("$ref" = "#/definitions/SortByEncoding"))
  sort_args <- paste(names(sort_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(sort_args)), collapse = ", ")
  
  param_docs <- get_param_docs(schema, "#/definitions/SortByEncoding")
  
  create_pass_function(
    function_suffix = glue("sort_{enc}_by_encoding"), 
    recipient_function = ".add_sort_obj_to_encoding",
    arg_list = arg_list,
    modify_args = glue("args_out <- c(args_out, list(.enc = '{enc}'))"),
    doc_description = glue("#' Add sort by another encoding to encoding for {enc} in a vega-lite spec."),
    extra_docs = glue("#' @seealso [vl_sort_{enc}] to sort by same field,",
                      "\n#' [vl_sort_{enc}_by_field()] to sort by another field"),
    param_docs = param_docs
  )
  
}


create_axis_for_encoding <- function(enc, schema) {
  
  axis_props <- props(schema, list("$ref" = "#/definitions/Axis"))
  axis_args <- paste(names(axis_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(axis_args), "remove = FALSE"), collapse = ", ")
  
  param_docs <- paste0(get_param_docs(schema, "#/definitions/Axis"),
                       "\n#' @param remove Remove the axis?")
  
  create_function_for_encode_param(
    enc,
    'axis',
    arg_list,
    param_docs
  )
  
}


create_scale_for_encoding <- function(enc, schema) {
  
  scale_props <- props(schema, list("$ref" = "#/definitions/Scale"))
  scale_args <- paste(names(scale_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(scale_args)), collapse = ", ")
  
  param_docs <- get_param_docs(schema, "#/definitions/Scale")
  
  create_function_for_encode_param(
    enc,
    'scale',
    arg_list,
    param_docs
  )
  
}


create_legend_for_encoding <- function(enc, schema) {
  
  legend_props <- props(schema, list("$ref" = "#/definitions/Legend"))
  legend_args <- paste(names(legend_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(legend_args)), collapse = ", ")
  
  param_docs <- get_param_docs(schema, "#/definitions/Legend")
  
  create_function_for_encode_param(
    enc,
    'legend',
    arg_list,
    param_docs
  )
  
}


create_condition_for_encoding <- function(enc, schema) {
  
  condition_props <- c(
    props(schema, list("$ref" = "#/definitions/ConditionalValueDef")),
    props(schema, list("$ref" = "#/definitions/ConditionalMarkPropFieldDef"))
  )
  condition_args <- paste(names(condition_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(condition_args)), collapse = ", ")
  
  param_docs <- get_param_docs(
    schema, 
    c("#/definitions/ConditionalValueDef","#/definitions/ConditionalMarkPropFieldDef")
  )
  
  create_function_for_encode_param(
    enc,
    'condition',
    arg_list,
    param_docs
  )
  
}
