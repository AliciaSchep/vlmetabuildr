create_data_generic <- function(schema) {

  make_function( "#/definitions/Data", 
                 schema, 
                 glue::glue("add_data"), 
                 ".add_data", 
                 description = "Add data to a vega-lite spec")

}
