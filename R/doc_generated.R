
#' @title Generated Data
#' @description
#' Data generated for examples and testing.
#' @eval generated_docs_section_format()
"generated"


generated_docs_section_format <- function() {
  c(
    "@format",
    "`environment` object. You probably only need `generated$dt`. ",
    "`generated` has the following objects: ",
    "",
    vapply(
      ls(basicepistats::generated),
      function(obj_nm) {
        obj <- basicepistats::generated[[obj_nm]]
        paste0(
          "- `", obj_nm, "` of class(es) ",
          paste0("\"", class(obj), "\"", collapse = ", ")
        )
      },
      character(1)
    )
  )
}
