#' R6 class representing a regression model
#'
#' @description
#' Contains fields storing data and methods to build, process and visualize
#' a regression model.
#' Currently, this class is designed for CoxPH and GLM regression models.
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
#'
#' # --------------
#' # Build a model
#' # --------------
#'
#' # way 1:
#' mm <- REGModel$new(
#'   as.data.frame(test1),
#'   Surv(time, status) ~ x + strata(sex)
#' )
#' mm
#' as.data.frame(mm$result)
#'
#' # way 2:
#' mm2 <- REGModel$new(
#'   as.data.frame(test1),
#'   recipe = list(
#'     x = c("x", "strata(sex)"),
#'     y = c("time", "status")
#'   )
#' )
#' mm2
#'
#' @testexamples
#' expect_is(mm, "REGModel")
#' expect_is(mm2, "REGModel")
REGModel <- R6::R6Class(
  "REGModel",
  inherit = NULL,
  cloneable = FALSE,
  lock_objects = TRUE,
  lock_class = TRUE,
  public = list(
    #' @field data a `data.table` storing modeling data.
    #' @field recipe an R `formula` storing model formula.
    #' @field f a string specifying modeling function, default is 'coxph'.
    #' @field terms all terms (covariables, i.e. columns) used for building model.
    #' @field args other arguments used for building model.
    #' @field model a constructed model.
    #' @field type model type (class).
    #' @field result model result, a object of `parameters_model`. Can be converted into
    #' data.frame with [as.data.frame()] or [data.table::as.data.table()].
    data = NULL,
    recipe = NULL,
    f = NULL,
    terms = NULL,
    args = NULL,
    model = NULL,
    type = NULL,
    result = NULL,
    #' @description Build a `REGModel` object.
    #' @param data a `data.table` storing modeling data.
    #' @param recipe an R `formula` or a list with two elements 'x' and 'y',
    #' where 'x' is for covariables and 'y' is for label. See example for detail
    #' operation.
    #' @param f a length-1 string specifying modeling function, default is 'coxph'.
    #' Other options are members of GLM family, see [stats::family()].
    #' 'binomial' is logistic, and 'gaussian' is linear.
    #' @param ... other parameters passing to corresponding regression model function.
    #' @param exp indicating whether or not to exponentiate the the coefficients.
    #' @param ci confidence Interval (CI) level. Default to 0.95 (95%).
    #' e.g. [survival::coxph()].
    #' @return a `REGModel` R6 object.
    initialize = function(data, recipe, ...,
                          f = c(
                            "coxph", "binomial", "gaussian",
                            "Gamma", "inverse.gaussian",
                            "poisson", "quasi", "quasibinomial",
                            "quasipoisson"
                          ),
                          exp = TRUE, ci = 0.95) {
      f <- f[1]
      stopifnot(
        is.data.frame(data),
        rlang::is_formula(recipe) | is.list(recipe),
        length(f) == 1 & is.character(f)
      )

      if (f == "coxph") {
        .f <- survival::coxph
      } else {
        .f <- stats::glm
      }

      get_vars <- function(text) {
        all.vars(parse(text = text))
      }

      if (is.list(recipe)) {
        if (!all(c("x", "y") %in% names(recipe))) {
          rlang::abort("If recipe is a list, 'x' and 'y' element must exist")
        }
        x <- recipe$x
        y <- recipe$y
        x_vars <- unique(sapply(x, get_vars))
        y_vars <- unique(sapply(y, get_vars))
        self$terms <- x_vars
        all_vars <- c(x_vars, y_vars)
        # Update recipe to a formula
        recipe <- if (f == "coxph") {
          if (length(y) < 2)
            rlang::warn("time and status for Surv object are not here, maybe a bad input of 'y' element in 'recipe'")
          glue("Surv({paste(y, collapse = ', ')}) ~ {paste(unique(x), collapse = ' + ')}")
        } else {
          glue("{paste(y, collapse = ' + ')} ~ {paste(unique(x), collapse = ' + ')}")
        }
        recipe <- stats::formula(recipe)
      } else {
        all_vars <- all.vars(recipe)
        recipe_list <- as.list(recipe)
        if (length(recipe_list) < 2) {
          rlang::abort("Bad recipe (regression formula)")
        } else if (length(recipe_list) == 2) {
          self$terms <- all.vars(recipe_list[[2]])
        } else {
          self$terms <- all.vars(recipe_list[[3]])
        }
      }

      data <- data.table::as.data.table(data)
      if (!all(all_vars %in% colnames(data))) {
        rlang::abort(glue("Column not available: {all_vars[!all_vars %in% colnames(data)]}"))
      }
      data <- data[, all_vars, with = FALSE]

      self$recipe <- recipe
      self$data <- data
      self$args <- list(...)

      self$model <- if (f == "coxph") {
        .f(recipe, data = data, ...)
      } else {
        is_call <- length(all.vars(parse(text = f))) == 0L
        if (is_call) {
          # e.g., quasi(variance = "mu", link = "log")
          f <- eval(parse(text = f))
        }
        .f(recipe, data = data, family = f, ...)
      }
      self$type <- class(self$model)
      self$result <- parameters::model_parameters(
        self$model,
        exponentiate = exp, ci = ci
      )
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
