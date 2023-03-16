
.onAttach <- function(...) {
  v = packageVersion("CSSLR")
  packageStartupMessage("Loading CSSLR version ", v)
}
