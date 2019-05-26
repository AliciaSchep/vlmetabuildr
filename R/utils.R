capitalize <- function(x){
  first <- substring(x, first = 1L, last = 1L)
  rest <- substring(x, first = 2L, last = nchar(x))
  paste0(toupper(first),rest)
}

type_or_ref <- function(x){
  if (hasName(x,"$ref")){
    sp <- strsplit(x[["$ref"]],"/")[[1]]
    sp[length(sp)]
  } else if (hasName(x,"type")){
    paste(x[["type"]],collapse = " OR ")
  } else {
    "Varies"
  }
}

get_description <- function(x) {
  if (hasName(x, "description")) {
    purrr::pluck(x,"description")
  } else if (hasName(x, "enum")) {
    paste(purrr::pluck(x,"enum"), collapse = ", ")
  } else {
    ""
  }
}

get_param_docs <- function(properties) {

  d <- purrr::map_chr(properties, get_description)
  d <- stringr::str_replace_all(d, "\n","\n#' ")
  t <- purrr::map_chr(properties, type_or_ref)

  docs <- paste(d, glue("(type = {t})"))

  param_names <- unique(names(properties))

  param_desc <- purrr::map_chr(
    param_names,
    ~ paste(unique(docs[which(names(properties) == .)]), collapse = " OR ")
  )

  paste("#' @param", param_names, param_desc, sep = " ", collapse = "\n")

}

create_pass_function <- function(function_suffix, 
                                 recipient_function,
                                 modify_args = "",
                                 doc_description = "",
                                 see_also = ""
                                 param_docs = "" ) {

  template <- system.file(file.path("templates",template), 
                          package = 'vlmetabuildr')
  glargs <- list(enc = enc, arg_list = arg_list,
                 param_docs = param_docs)
  glue::glue_data(glargs, readr::read_file(template), .trim = FALSE)
  
}

