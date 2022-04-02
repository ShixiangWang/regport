#' R6 class representing a list of regression model
#'
#' @description
#' Contains fields storing data and methods to build, process and visualize
#' a list of regression model.
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
REGModelList <- R6::R6Class(
  "REGModelList",
  inherit = NULL,
  cloneable = FALSE,
  lock_objects = TRUE,
  lock_class = TRUE,
  public = list(
    #' @field data a `data.table` storing modeling data.
    #' @field recipe an R `formula` storing model formula.
    #' @field terms all terms (covariables, i.e. columns) used for building model.
    #' @field args other arguments used for building model.
    #' @field model a constructed model.
    #' @field type model type (class).
    #' @field result model result, a object of `parameters_model`. Can be converted into
    #' data.frame with [as.data.frame()] or [data.table::as.data.table()].
    data = NULL,
    x = NULL,
    y = NULL,
    covars = NULL,
    args = NULL,
    mlist = NULL,
    type = NULL,
    result = NULL,
    forest_data = NULL,
    #' @description Create a `REGModelList` object.
    #' @param data a `data.table` storing modeling data.
    #' @return a `REGModelList` R6 object.
    initialize = function(data, y, x, covars = NULL) {
      stopifnot(is.data.frame(data))

      all_vars = merge_vars(x, y, covars)
      data <- data.table::as.data.table(data)
      if (!all(all_vars %in% colnames(data))) {
        rlang::abort(glue("Column not available: {all_vars[!all_vars %in% colnames(data)]}"))
      }
      data <- data[, all_vars, with = FALSE]

      self$data = data
      self$x = setdiff(x, y)
      self$y = y
      self$covars = covars
    },
    build = function(
    f = c(
      "coxph", "binomial", "gaussian",
      "Gamma", "inverse.gaussian",
      "poisson", "quasi", "quasibinomial",
      "quasipoisson"
    ),
    exp = NULL, ci = 0.95,
    ...) {
      f <- f[1]
      stopifnot(
        is.character(f),
        is.null(exp) || is.logical(exp)
      )

      ml = list()
      for (i in seq_along(self$x)) {
        m = REGModel$new(
          self$data,
          recipe = list(x = unique(c(self$x[i], self$covars)),
                        y = self$y),
          f = f, exp = exp, ci = ci, ...
        )
        m$get_forest_data()
        ml[[i]] = m
      }
      self$mlist = ml
      self$type = ml[[1]]$type
      self$result = data.table::rbindlist(
        lapply(
          seq_along(ml),
          function(x) cbind(focal_term = self$x[x], ml[[x]]$result)),
      )
      colnames(self$result)[2:3] = c("variable", "estimate")
      self$forest_data = data.table::rbindlist(
        lapply(
          seq_along(ml),
          function(x) cbind(focal_term = self$x[x], ml[[x]]$forest_data)),
      )
      # Only keep focal term
      self$forest_data = self$forest_data[focal_term == term_label]
    },
    plot_forest = function(ref_line = 1, xlim = c(0, 2), ...) {
      data <- self$forest_data
      if (is.null(data)) {
        message("Please run $build() before $plot_forest()")
        return(NULL)
      }
      plot_forest(data, ref_line, xlim, ...)
    },
    #' @description print the `REGModelList` object
    #' @param ... unused.
    print = function(...) {
      cat(glue("<{cli::col_br_magenta('REGModelList')}>    =========="), "\n\n")
      cat(glue("{cli::col_green('X')}(s): {paste(self$x, collapse = ', ')}"), "\n")
      cat(glue("{cli::col_green('Y')}(s): {paste(self$y, collapse = ', ')}"), "\n")
      cat(glue("covars: {paste(self$covars, collapse = ', ')}"), "\n")
      if (is.null(self$result)) {
        cat("\nNot build yet, run $build() method", "\n")
      } else {
        cat("----\n", glue("{cli::col_green('Result')}:"), "\n")
        print(self$result)
      }
      cat(glue("[{cli::col_br_green(paste(self$type, collapse = '/'))}] model =========="))
    }
  ),
  private = list(),
  active = list()
)
