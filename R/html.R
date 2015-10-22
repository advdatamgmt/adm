#' Create HTML quickly
#'
#' @param value is what goes inside the tag
#' @param tag is a valid HTML tag
#' @param attr is a list of attributes
#' @export
html <- function(value, tag, attr = NULL) {
  # paste the attributes/values together, attr needs to be a list of lists
  # allows recycling of arguments
  makeattrlist <- function(a) {
    out <- " "
    if (length(a) > 0) {
      out <- paste(mapply(
        function(...) paste(..., sep = ''), names(a), '="' ,a, '"'),
        collapse = " ")
      out <- paste(" ", out, sep = '') # add leading space
    }
    out
  }
  attrlist <- sapply(attr, makeattrlist)
  attrlist <- sub('=""' , "", attrlist, fixed = TRUE)
  paste("<", tag, attrlist, ">", value, "</", tag, ">\n" ,sep = ''
        , collapse = '')
}
