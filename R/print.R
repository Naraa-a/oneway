#' Print one-way ANOVA results
#'
#' print.oneway prints one-way ANOVA results.
#'
#' @param x an object of class oneway.
#' @param ... additional arguments passed to the function.
#'
#' @return the input object is returned silently.
#' @export
#' @examples
#' mileage <- oneway(mpg~cyl, mtcars)
#' print(mileage)
print.oneway <- function(x, ...){

  ## check if x is oneway object
  if(!inherits(x, "oneway"))
    stop("Must be class 'oneway'")


  cat("Summary Statistics \n", "==============================\n")
  print(x$summarystats, ...)
  cat("Anova \n",  "==============================\n")
  print(summary.lm(x$anova))

  ## more to fix: blank before the ======
  ## space after the titles
}
