
get_mark_props <- function(schema){
 props(schema, list("$ref" = "#/definitions/AnyMark"))
}

get_mark_docs <- function(schema) {
  get_param_docs2(schema, "#/definitions/AnyMark")
}

get_mark_props_comp <- function(schema, comp){
  def_names <- stringr::str_subset(names(schema$definitions),"Def$")
  comp_def <- def_names[stringr::str_replace(tolower(def_names),"def$","") == comp]
  
  props(schema, list("$ref" = glue("#/definitions/{comp_def}")))
}

get_mark_props_simple <- function(schema){
  props(schema, list("$ref" = glue("#/definitions/MarkDef")))
}


#' create_mark
#'
create_mark_generic <- function(schema){

  mark_props <- get_mark_props(schema)
  mark_args <- paste(names(mark_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(mark_args)), collapse = ", ")

  param_docs <- get_mark_docs(schema)

  glue("\n#' vl_mark\n#' \n",
       "#' Add a mark to a vega-lite spec.\n",
       "#' @param spec A vega-lite spec\n",
       "{param_docs}\n",
       "#' @return A modified spec\n",
       "#' @export\n",
       "#' @name vl_mark\n",
       "vl_mark <- function({arg_list}) {{\n",
       "  args_in <- rlang::fn_fmls_syms()\n",
       "  args_eval <- lapply(args_in,eval, env = rlang::current_env())\n",
       "  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]\n",
       "  rlang::exec(.add_mark, !!!args_out)\n",
       "}}\n", .trim = FALSE)

}


create_mark <- function(mark, schema) {

  mark_props <- get_mark_props_simple(schema)
  mark_args <- paste(names(mark_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(mark_args)), collapse = ", ")

  glue("\n#' @name vl_mark\n",
       "#' @export\n",
       "vl_mark_{mark} <- function({arg_list}) {{\n",
       "  args_in <- rlang::fn_fmls_syms()\n",
       "  args_eval <- lapply(args_in,eval, env = rlang::current_env())\n",
       "  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]\n",
       "  args_out <- c(args_out, list(.mark = '{mark}'))\n",
       "  rlang::exec(.add_mark, !!!args_out)\n",
       "}}\n", .trim = FALSE)
}

create_mark_composite <- function(mark, schema) {
  
  mark_props <- get_mark_props_comp(schema, mark)
  mark_args <- paste(names(mark_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(mark_args)), collapse = ", ")
  
  glue("\n#' @name vl_mark\n",
       "#' @export\n",
       "vl_mark_{mark} <- function({arg_list}) {{\n",
       "  args_in <- rlang::fn_fmls_syms()\n",
       "  args_eval <- lapply(args_in,eval, env = rlang::current_env())\n",
       "  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]\n",
       "  args_out <- c(args_out, list(.mark = '{mark}'))\n",
       "  rlang::exec(.add_mark, !!!args_out)\n",
       "}}\n", .trim = FALSE)
}

create_mark_functions <- function(schema){
  simple_mark_options = enums(schema, list("$ref" = '#/definitions/Mark'))
  comp_mark_options = enums(schema, list("$ref" = '#/definitions/CompositeMark'))
  c(
    create_mark_generic(schema),
    purrr::map_chr(simple_mark_options, create_mark, schema = schema),
    purrr::map_chr(comp_mark_options, create_mark_composite, schema = schema)
  )
}