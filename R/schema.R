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
#' @param schema
#' @param type
#'
#' @return
#' @export
#' @title schema
#' @name schema
#'
#' @examples
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
