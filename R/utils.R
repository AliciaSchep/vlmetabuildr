
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

get_param_docs <- function(properties) {

  d <- purrr::map_chr(properties, ~purrr::pluck(.,"description"))
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
