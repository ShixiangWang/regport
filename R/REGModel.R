#' R6 class representing a regression model
#'
#' @description
#' Contains fields storing data and methods to build, process and visualize
#' a regression model.
#'
#' @export
#' @examples
#' library(survival)
#' test1 <- data.frame(
#'   time = c(4, 3, 1, 1, 2, 2, 3),
#'   status = c(1, 1, 1, 0, 1, 1, 0),
#'   x = c(0, 2, 1, 1, 1, 0, 0),
#'   sex = c(0, 0, 0, 0, 1, 1, 1)
#' )
#' mm <- REGModel$new(as.data.frame(test1), Surv(time, status) ~ x + strata(sex), f = survival::coxph)
#' mm
#' as.data.frame(mm$result)
#' @testexamples
#' expect_is(mm, "REGModel")
REGModel <- R6::R6Class(
  "REGModel",
  inherit = NULL,
  # cloneable = FALSE,
  lock_objects = TRUE,
  lock_class = TRUE,
  public = list(
    #' @field data a `data.table` storing modeling data.
    #' @field recipe an R `formula` storing model formula.
    #' @field f an R `function` storing model constructor, e.g., [survival::coxph].
    #' @field focal_term a user focused term (covariable).
    #' @field terms all terms (covariables, i.e. columns) used for building model.
    #' @field args other arguments used for building model.
    #' @field model a constructed model.
    #' @field type model type (class).
    #' @field result model result, a object of `parameters_model`. Can be converted into
    #' data.frame with [as.data.frame()] or [data.table::as.data.table()].
    data = NULL,
    recipe = NULL,
    f = NULL,
    focal_term = NULL,
    terms = NULL,
    args = NULL,
    model = NULL,
    type = NULL,
    result = NULL,
    #' @description Build a `REGModel` object.
    #' @param data a `data.table` storing modeling data.
    #' @param recipe an R `formula` storing model formula.
    #' @param f a R `function` storing model constructor, e.g., [survival::coxph].
    #' @param ... other parameters passing to corresponding regression model function.
    #' @param exp indicating whether or not to exponentiate the the coefficients.
    #' @param ci confidence Interval (CI) level. Default to 0.95 (95%).
    #' e.g. [survival::coxph()].
    #' @return a `REGModel` R6 object.
    initialize = function(data, recipe, f, ..., exp = TRUE, ci = 0.95) {
      stopifnot(is.data.frame(data), rlang::is_formula(recipe))
      all_vars <- all.vars(recipe)
      recipe_list <- as.list(recipe)
      if (length(recipe_list) < 2) {
        rlang::abort("Bad recipe (regression formula)")
      } else if (length(recipe_list) == 2) {
        self$terms <- all.vars(recipe_list[[2]])
      } else {
        self$terms <- all.vars(recipe_list[[3]])
      }

      data <- data.table::as.data.table(data)
      if (!all(all_vars %in% colnames(data))) {
        rlang::abort(glue("Column not available: {all_vars[!all_vars %in% colnames(data)]}"))
      }

      self$recipe <- recipe
      self$data <- data
      self$args <- list(...)

      self$model <- f(recipe, data, ...)
      self$type <- class(self$model)
      self$result <- parameters::model_parameters(
        self$model, exponentiate  = exp, ci = ci)
    },
    #' @description print the `REGModel` result with default plot methods from **parameters** package.
    plot = function() {
      plot(self$result)
    },
    #' @description print the `REGModel` object
    #' @param ... unused.
    print = function(...) {
      cat("======================\nA <")
      cat(cli::col_br_cyan("REGModel"))
      cat("> object\n")
      cat("======================\n")
      print(self$result)
      cat("======================\n")
    }
  ),
  private = list(),
  active = list()
)
