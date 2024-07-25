is.scalar <- function(x) {
  is.atomic(x) && length(x) == 1L
}
