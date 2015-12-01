#' @import methods generics

#' @export
testS3 <- function() structure(list(annotation = "S3"), class = "testS3")

#' @export
annotation.testS3 <- function(object, ...) {
    object[["annotation"]]
}

#' @export
`annotation<-.testS3` <- function(object, ..., value) {
    object[["annotation"]] <- value
    object
}

#' @export
testS4 <- setClass("testS4", slots = c(annotation = "character"))

setMethod("initialize", "testS4",
          function(.Object, annotation = "S4") {
              .Object@annotation <- annotation
              .Object
          })

#' Annotation accessor for testS4
#'
#' @param object blah
#' @param ... Additional arguments ignored
#' @export
setMethod("annotation", signature(object = "testS4"),
          function(object, ...) {
              object@annotation
          })

#' @export
setMethod("annotation<-", signature(object = "testS4"),
          function(object, ..., value) {
              object@annotation <- value
              object
          })
