##' @title Drop unused levels after subset
##' @name subsetDropAll
##'
##' @description This function is designed to be used in place of
##'     \code{\link[base]{subset}} when you want to drop off all empty
##'     levels, of all factor columns in a data frame.
##'
##' @param x Object to be subsetted
##' @param ... further arguments to be passed to or from other methods
##'
##' @details This function works exactly the same way as
##'     \code{\link[base]{subset}} but it will drop off all empty levels
##'     of all columns that are factors in \code{x}.
##'
##' @return An object similar to \code{x} containing just the selected
##'     rows and columns (for a data frame). Column factors that have
##'     one or more levels emptied after the subset, will have this
##'     levels droped off.
##'
##' @author Fernando Mayer
##'
##' @seealso \code{\link[base]{subset}}
##'
##' @export
subsetDropAll <- function(x, ...){
    if(!is.data.frame(x)) stop("'x' must be a data.frame")
    x <- subset(x, ...)
    factors <- which(sapply(x, class) %in% "factor")
    for(i in factors){
        x[,i] <- sapply(x[,i], "[", drop=T)
    }
    return(x)
}
