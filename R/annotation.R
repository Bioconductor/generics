#' @import methods
NULL

#' Accessing annotation information
#'
#' Get or set the annotation information contained in an object.
#' @param object An object containing annotation information.
#' @param ... Additional arguments, for use in specific methods.
#' @param value The annotation information to set on \code{object}.
#' @seealso
#'  \itemize{
#'    \item \code{\link[methods]{showMethods}} for displaying a summary of the
#'          methods defined for a given generic function.
#'
#'    \item \code{\link[methods]{selectMethod}} for getting the definition of
#'          a specific method.
#'
#'    \item \link[Biobase]{annotation,eSet-method} in the \pkg{Biobase} package
#'          for an example of a specific \code{annotation} method (defined for
#'          \link[Biobase]{eSet} objects).
#'
#'    \item \link{BiocGenerics} for a summary of all the generics defined
#'          in the \pkg{BiocGenerics} package.
#'  }
#' @examples
#' annotation
#' showMethods("annotation")
#' @keywords methods
#' @export
setGeneric("annotation",
    function(object, ...) UseMethod("annotation")
)

#' @rdname annotation
#' @export
setGeneric("annotation<-",
    function(object, ..., value) UseMethod("annotation<-")
)
