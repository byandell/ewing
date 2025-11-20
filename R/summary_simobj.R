summary_simobj <- function(object) {
  out <- sapply(object, function(x) {
    if(is.list(x)) {
      x <- names(x)
    }
    paste(x, collapse=",")
  })
  paste(paste(names(out), out, sep = ": "), collapse = "<br>")
}
