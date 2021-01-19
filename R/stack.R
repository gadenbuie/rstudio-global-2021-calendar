Stack <- R6::R6Class(
  "stack",
  public = list(
    stack = "<reactiveVal>",
    initialize = function(x = NULL, label = NULL) {
      self$stack <- shiny::reactiveVal(x, label)
    },
    add = function(x) {
      self$stack(c(self$stack(), x))
    },
    remove = function(x) {
      self$stack(setdiff(self$stack(), x))
    },
    clear = function(x) {
      self$stack(NULL)
    },
    intersect = function(x) {
      self$stack(base::intersect(self$stack(), x))
    },
    union = function(x) {
      self$stack(base::union(self$stack(), x))
    },
    update = function(x) {
      self$stack(x)
    }
  )
)
