#' R6 class representing a list of regression model
#'
#' @description
#' Contains fields storing data and methods to build, process and visualize
#' a list of regression model.
#' Currently, this class is designed for CoxPH and GLM regression models.
#'
#' @export
#' @examples
#' ml <- REGModelList$new(
#'   data = mtcars,
#'   y = "mpg",
#'   x = c("factor(cyl)", colnames(mtcars)[3:5]),
#'   covars = c(colnames(mtcars)[8:9], "factor(gear)")
#' )
#' ml
#' ml$print()
#' ml$plot_forest()
#'
#' ml$build(f = "gaussian")
#' ml$build(f = "gaussian", parallel = TRUE)
#' ml$print()
#' ml$result
#' ml$forest_data
#' ml$plot_forest()
#' @testexamples
#' expect_is(ml, "REGModelList")
REGModelList <- R6::R6Class(
  "REGModelList",
  inherit = NULL,
  cloneable = FALSE,
  lock_objects = TRUE,
  lock_class = TRUE,
  public = list(
    #' @field data a `data.table` storing modeling data.
    #' @field x focal variables (terms).
    #' @field y predicted variables or expression.
    #' @field covars covariables.
    #' @field mlist a list of `REGModel`.
    #' @field args other arguments used for building model.
    #' @field type model type (class).
    #' @field result model result, a object of `parameters_model`. Can be converted into
    #' data.frame with [as.data.frame()] or [data.table::as.data.table()].
    #' @field forest_data more detailed data used for plotting forest.
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
    #' @param x focal variables (terms).
    #' @param y predicted variables or expression.
    #' @param covars covariables.
    #' @return a `REGModelList` R6 object.
    initialize = function(data, y, x, covars = NULL) {
      stopifnot(is.data.frame(data))

      all_vars <- merge_vars(x, y, covars)
      data <- data.table::as.data.table(data)
      if (!all(all_vars %in% colnames(data))) {
        rlang::abort(glue("Column not available: {all_vars[!all_vars %in% colnames(data)]}"))
      }
      data <- data[, all_vars, with = FALSE]

      self$data <- data
      self$x <- setdiff(x, y)
      self$y <- y
      self$covars <- covars
    },
    #' @description Build `REGModelList` object.
    #' @param f a length-1 string specifying modeling function or family of [glm()], default is 'coxph'.
    #' Other options are members of GLM family, see [stats::family()].
    #' 'binomial' is logistic, and 'gaussian' is linear.
    #' @param ... other parameters passing to corresponding regression model function.
    #' @param exp logical, indicating whether or not to exponentiate the the coefficients.
    #' @param ci confidence Interval (CI) level. Default to 0.95 (95%).
    #' e.g. [survival::coxph()].
    #' @param parallel if `TRUE`, use N-1 cores to run the task.
    #' @return a `REGModel` R6 object.
    build = function(f = c(
                       "coxph", "binomial", "gaussian",
                       "Gamma", "inverse.gaussian",
                       "poisson", "quasi", "quasibinomial",
                       "quasipoisson"
                     ),
                     exp = NULL, ci = 0.95,
                     parallel = FALSE,
                     ...) {
      f <- f[1]
      stopifnot(
        is.character(f),
        is.null(exp) || is.logical(exp)
      )

      self$args <- list(...)
      ml <- list()
      # for (i in seq_along(self$x)) {
      #   m <- REGModel$new(
      #     self$data,
      #     recipe = list(
      #       x = unique(c(self$x[i], self$covars)),
      #       y = self$y
      #     ),
      #     f = f, exp = exp, ci = ci, ...
      #   )
      #   m$get_forest_data()
      #   ml[[i]] <- m
      # }
      #
      build_one <- function(i, ...) {
        m <- REGModel$new(
          self$data,
          recipe = list(
            x = unique(c(self$x[i], self$covars)),
            y = self$y
          ),
          f = f, exp = exp, ci = ci, ...
        )
        m$get_forest_data()
        m
      }

      if (.Platform$OS.type == "windows") {
        message("parallel computation from parallel package is not supported in Windows, disable it.")
        parallel <- FALSE
      }

      fcall <- if (parallel) parallel::mclapply else lapply
      args <- if (!parallel) {
        list(seq_along(self$x), FUN = build_one, ...)
      } else {
        list(seq_along(self$x),
          FUN = build_one,
          mc.cores = max(parallel::detectCores() - 1L, 1L),
          ...
        )
      }
      ml <- do.call("fcall", args = args)

      self$mlist <- ml
      self$type <- ml[[1]]$type
      self$result <- data.table::rbindlist(
        lapply(
          seq_along(ml),
          function(x) cbind(focal_term = self$x[x], ml[[x]]$result)
        ),
      )
      colnames(self$result)[2:3] <- c("variable", "estimate")
      self$forest_data <- data.table::rbindlist(
        lapply(
          seq_along(ml),
          function(x) cbind(focal_term = self$x[x], ml[[x]]$forest_data)
        ),
      )
      # Only keep focal term
      self$forest_data <- self$forest_data[focal_term == term_label]
    },
    #' @description plot forest.
    #' @param ref_line reference line, default is `1` for HR.
    #' @param xlim limits of x axis.
    #' @param ... other plot options passing to [forestploter::forest()].
    #' Also check <https://github.com/adayim/forestploter> to see more complex adjustment of the result plot.
    plot_forest = function(ref_line = NULL, xlim = NULL, ...) {
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
