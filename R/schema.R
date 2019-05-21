lookup <- function(schema, ref = NULL){
  if (is.null(ref)) return(NULL)
  path <- strsplit(ref,"/")[[1]]
  for (i in path) {
    if (i != "#") schema <- schema[[i]]
  }
  return(schema)
}

search <- function(schema, type, check, get, gather, base) {
  if (is.null(type)) {
    base()
  } else if (!is.null(type[["$ref"]])) {
    search(schema, lookup(schema, type[["$ref"]]), check, get, gather, base)
  } else if (check(type)) {
    get(type)
  } else if ("anyOf" %in% names(type) || "allOf" %in% names(type) ||
             "oneOf" %in% names(type)) {
    t <- c(type[["anyOf"]],type[["allOf"]],type[["oneOf"]])
    gather(lapply(t, function(x) search(schema, x, check, get, gather, base)))
  } else{
    base()
  }
}


#' schema utility functions
#'
#' @param schema imported json schema
#' @param type schema reference
#'
#' @return
#' @export
#' @title schema
#' @name schema
#'
#' @examples
#' 
#' schema_file <- Sys.glob(file.path(system.file("schema/vega-lite", package = "vegawidget"),"*.json"))
#' VL_SCHEMA <- jsonlite::read_json(schema_file)
#' encoding_options <- props(VL_SCHEMA, list("$ref" = "#/definitions/Encoding"))
props <- function(schema, type) {
  search(schema,
         type,
         function(x) {"type" %in% names(x) && x[["type"]] ==  'object'},
         function(x) {x[["properties"]]},
         function(x) unlist(x, recursive = FALSE),
         function() {NULL}
  )
}


#' @name schema
#' @export
enums <- function(schema, type) {
  search(schema,
         type,
         function(x) {"enum" %in% names(x)},
         function(x) {x[["enum"]]},
         function(x) unlist(x, use.names = FALSE),
         function() list()
  )
}



#' @name schema
#' @export
types <- function(schema, type) {
  search(schema,
         type,
         function(x) {"type" %in% names(x) && x[["type"]] ==  'object' &&
             "properties" %in% names(x) && "type" %in% names(x[["properties"]])},
         function(x) {x[["properties"]][["type"]][["enum"]]},
         function(x) sort(unlist(x, use.names = FALSE)),
         function() {c()}
  )
}

#' @name schema
#' @export
get_name_from_ref <- function(type){
  stringr::str_remove(type[["$ref"]],"#/definitions/")
}