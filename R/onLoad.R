
.onLoad <- function(...) {
  options("csslr.speed.glm" = FALSE)
  options("csslr.use.ar" = FALSE)
  options("csslr.maxCategories" = 25)
  options("csslr.maxMissingPerc" = 0.1)
}
