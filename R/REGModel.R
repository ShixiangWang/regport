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
#' test1$sex <- factor(test1$sex)
#'
#' # --------------
#' # Build a model
#' # --------------
#'
#' # way 1:
#' mm <- REGModel$new(
#'   test1,
#'   Surv(time, status) ~ x + strata(sex)
#' )
#' mm
#' as.data.frame(mm$result)
#'
#' # way 2:
#' mm2 <- REGModel$new(
#'   test1,
#'   recipe = list(
#'     x = c("x", "strata(sex)"),
#'     y = c("time", "status")
#'   )
#' )
#' mm2
#'
#' # Add other parameters, e.g., weights
#' # For more, see ?coxph
#' mm3 <- REGModel$new(
#'   test1,
#'   recipe = list(
#'     x = c("x", "strata(sex)"),
#'     y = c("time", "status")
#'   ),
#'   weights = c(1, 1, 1, 2, 2, 2, 3)
#' )
#' mm3$args
#'
#' # ----------------------
#' # Another type of model
#' # ----------------------
#' library(stats)
#' counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
#' outcome <- gl(3, 1, 9)
#' treatment <- gl(3, 3)
#' data <- data.frame(treatment, outcome, counts)
#'
#' mm4 <- REGModel$new(
#'   data,
#'   counts ~ outcome + treatment,
#'   f = "poisson"
#' )
#' mm4
#' mm4$get_forest_data()
#' mm4$plot_forest(xlim = c(-1, 3), ref_line = 0)
#' @testexamples
#' expect_is(mm, "REGModel")
#' expect_is(mm2, "REGModel")
#' expect_equal(data.frame(mm$result), data.frame(mm2$result))
#' expect_is(mm3, "REGModel")
#' expect_is(mm4, "REGModel")
REGModel <- R6::R6Class(
  "REGModel",
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
    #' @field forest_data more detailed data used for plotting forest.
    data = NULL,
    recipe = NULL,
    terms = NULL,
    args = NULL,
    model = NULL,
    type = NULL,
    result = NULL,
    forest_data = NULL,
    #' @description Build a `REGModel` object.
    #' @param data a `data.table` storing modeling data.
    #' @param recipe an R `formula` or a list with two elements 'x' and 'y',
    #' where 'x' is for covariables and 'y' is for label. See example for detail
    #' operation.
    #' @param f a length-1 string specifying modeling function or family of [glm()], default is 'coxph'.
    #' Other options are members of GLM family, see [stats::family()].
    #' 'binomial' is logistic, and 'gaussian' is linear.
    #' @param ... other parameters passing to corresponding regression model function.
    #' @param exp logical, indicating whether or not to exponentiate the the coefficients.
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
                          exp = NULL, ci = 0.95) {
      f <- f[1]
      stopifnot(
        is.data.frame(data),
        rlang::is_formula(recipe) | is.list(recipe),
        length(f) == 1 & is.character(f),
        is.null(exp) || is.logical(exp)
      )

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
          if (length(y) < 2) {
            rlang::warn("time and status for Surv object are not here, maybe a bad input of 'y' element in 'recipe'")
          }
          glue("survival::Surv({paste(y, collapse = ', ')}) ~ {paste(unique(x), collapse = ' + ')}")
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
        # Strange, if input a formula from outsize, a environment is not attached
        recipe <- stats::formula(deparse(recipe))
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
        survival::coxph(recipe, data = data, ...)
      } else {
        is_call <- length(all.vars(parse(text = f))) == 0L
        if (is_call) {
          # e.g., quasi(variance = "mu", link = "log")
          f <- eval(parse(text = f))
        }
        stats::glm(recipe, data = data, family = f, ...)
      }
      self$type <- class(self$model)
      if (is.null(exp)) {
        exp <- inherits(self$model, "coxph") ||
          (inherits(self$model, "glm") && self$model$family$link == "logit")
      }
      self$result <- parameters::model_parameters(
        self$model,
        exponentiate = exp, ci = ci
      )
      private$model_data <- broom.helpers::model_get_model_frame(self$model)
    },
    #' @description get tidy data for plotting forest.
    #' @param separate_factor separate factor/class as a blank row.
    #' @param global_p if `TRUE`, return global p value.
    get_forest_data = function(separate_factor = FALSE, global_p = FALSE) {
      self$forest_data <- make_forest_terms(
        self$model,
        as.data.frame(self$result),
        private$model_data,
        separate_factor, global_p
      )
    },
    #' @description plot forest.
    #' @param ref_line reference line, default is `1` for HR.
    #' @param xlim limits of x axis.
    #' @param ... other plot options passing to [forestploter::forest()].
    #' Also check <https://github.com/adayim/forestploter> to see more complex adjustment of the result plot.
    plot_forest = function(ref_line = 1, xlim = c(0, 2), ...) {
      data <- self$forest_data
      if (is.null(data)) {
        message("Never call '$get_forest_data()' before, run with default options to get plotting data")
        data <- self$get_forest_data()
      }
      plot_forest(data, ref_line, xlim, ...)
    },
    #' @description print the `REGModel$result` with default plot methods from **see** package.
    plot = function() {
      plot(self$result)
    },
    #' @description print the `REGModel` object
    #' @param ... unused.
    print = function(...) {
      cat(glue("<{cli::col_br_magenta('REGModel')}>    =========="), "\n\n")
      print(self$result)
      cat(glue("[{cli::col_br_green(paste(self$type, collapse = '/'))}] model =========="))
    }
  ),
  private = list(
    model_data = NULL
  ),
  active = list()
)

plot_forest <- function(data, ref_line = 1, xlim = c(0, 2), ...) {
  dt <- data[, c("variable", "level", "n")]
  # Add blank column for the forest plot to display CI.
  # Adjust the column width with space.
  dt$` ` <- paste(rep(" ", 20), collapse = " ")
  # Create confidence interval column to display
  dt$`Estimate (95% CI)` <- data.table::fifelse(
    data$reference, "Reference",
    data.table::fifelse(
      is.na(data$SE),
      "",
      sprintf(
        "%.2f (%.2f to %.2f)",
        data$estimate, data$CI_low, data$CI_high
      )
    )
  )
  dt$p <- ifelse(is.na(data$p), "", format.pval(data$p, digits = 2, eps = 1e-3))

  for (i in seq_len(ncol(dt))) {
    dt[[i]] <- ifelse(is.na(dt[[i]]), "", as.character(dt[[i]]))
  }

  data$estimate <- data.table::fifelse(data$reference, ref_line, data$estimate)
  data$CI_low <- data.table::fifelse(data$reference, ref_line, data$CI_low)
  data$CI_high <- data.table::fifelse(data$reference, ref_line, data$CI_high)

  p <- forestploter::forest(dt,
    est = data$estimate,
    lower = data$CI_low,
    upper = data$CI_high,
    ci_column = 4,
    ref_line = ref_line,
    xlim = xlim,
    ...
  )
  p
}

# Adapted from forestmodel package
make_forest_terms <- function(model, tidy_model, data,
                              separate_factor = FALSE,
                              global_p = FALSE) {
  # tidy_model <- broom::tidy(model, conf.int = TRUE)
  colnames(tidy_model)[1:2] <- c("term", "estimate")

  forest_terms <- merge(
    data.table::data.table(
      term_label = attr(model$terms, "term.labels")
    )[, variable := remove_backticks(term_label)],
    data.table::data.table(
      variable = names(attr(model$terms, "dataClasses"))[-1],
      class = attr(model$terms, "dataClasses")[-1]
    ),
    by = "variable", all.x = FALSE, all.y = FALSE
  )

  # TODO:交互项的处理
  create_term_data <- function(term_row) {
    if (!is.na(term_row$class)) {
      var <- term_row$variable
      if (term_row$class %in% c("factor", "character")) {
        tab <- table(data[, var])
        if (!any(paste0(term_row$term_label, names(tab)) %in% tidy_model$term)) {
          # Filter out terms not in final model summary (e.g. strata)
          out <- data.frame(variable = NA, stringsAsFactors = FALSE)
        } else {
          out <- data.frame(
            term_row,
            level = names(tab),
            level_no = 1:length(tab),
            n = as.integer(tab),
            stringsAsFactors = FALSE
          )
          if (separate_factor) {
            out <- dplyr::bind_rows(as.data.frame(term_row, stringsAsFactors = FALSE), out)
          }
        }
      } else {
        out <- data.frame(term_row,
          level = NA, level_no = NA,
          n = sum(!is.na(data[, var])),
          stringsAsFactors = FALSE
        )
        if (term_row$class == "logical") {
          out$term_label <- paste0(term_row$term_label, "TRUE")
        }
      }
    } else {
      out <- data.frame(
        term_row,
        level = NA, level_no = NA, n = NA,
        stringsAsFactors = FALSE
      )
    }
    out
  }

  forest_terms <- forest_terms %>%
    dplyr::rowwise() %>%
    dplyr::do(create_term_data(.)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(variable)) %>%
    dplyr::mutate(term = paste0(term_label, replace(level, is.na(level), ""))) %>%
    dplyr::left_join(tidy_model, by = "term") %>%
    dplyr::mutate(
      reference = ifelse(is.na(level_no), FALSE, level_no == 1),
      estimate = ifelse(reference, 0, estimate),
      variable = ifelse(is.na(variable), remove_backticks(term), variable)
    ) %>%
    dplyr::mutate(
      variable = ifelse(is.na(level_no) | (level_no == 1 & !separate_factor), variable, NA)
    ) %>%
    dplyr::select(c("variable", "term"), dplyr::everything())

  if (global_p) {
    if (inherits(model, "coxph")) {
      p_val <- as.numeric(summary(model)$sctest[3])
      label <- paste("Global p ", format.pval(p_val, digits = 2, eps = 1e-3))
      forest_terms <- forest_terms %>%
        dplyr::add_row(term_label = "Global p", variable = label)
    } else {
      message("No global p value availabe for non-Cox model")
    }
  }

  data.table::as.data.table(forest_terms)
}
