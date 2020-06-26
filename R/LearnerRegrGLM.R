#' @title builtin Regression algorithm GLM Learner
#'
#' @name mlr_learners_regr.glm
#'
#' @description
#' A [mlr3::LearnerRegr] implementing glm from package
#'   \CRANpkg{stats}.
#' Calls [stats::glm()].
#'
#' @templateVar id regr.glm
#' @template section_dictionary_learner
#'
#' @references
#' Dobson, A. J. (1990) An Introduction to Generalized Linear Models. London: Chapman and Hall.
#' Hastie, T. J. and Pregibon, D. (1992) Generalized linear models. Chapter 6 of Statistical Models in S eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.
#' McCullagh P. and Nelder, J. A. (1989) Generalized Linear Models. London: Chapman and Hall.
#' Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. New York: Springer.
#'
#' @template seealso_learner
#' @template example
#' @export
# <Adapt the name to your learner. For regression learners inherit = LearnerRegr
LearnerRegrGLM = R6Class("LearnerRegrGLM",
  inherit = LearnerRegr,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamUty$new("family", default = gaussian, tags = "train"),
          ParamUty$new("offset", default = NULL, tags = "train"),
          ParamDbl$new("start", default = NULL, tags = "train"),
          ParamDbl$new("etaStart", default = NULL, tags = "train"),
          ParamDbl$new("mustart", default = NULL, tags = "train"),
          ParamLgl$new("model", default = TRUE, tags = "train"),
          ParamLgl$new("singular.ok", default = TRUE, tags = "train"),
          ParamUty$new("control", default = NULL, tags = "train")
        )
      )

      ps$values = list(family = gaussian)
      super$initialize(
        id = "regr.glmnet",
        packages = "stats",
        feature_types = c("logical", "integer", "numeric", "factor")
        predict_types = c("response", "link", "terms"),
        param_set = ps,
        properties = "weights",
        man = "mlr3learners.glm::mlr_learners_regr.glm"
      )
    },
  ),

  private = list(
    .train = function(task) {
      pars = self$param_set$get_values(tags = "train")
      formula = task$formula()
      data = task$data()
      mlr3misc::invoke(stats::glm, formula = formula, data = data, .args = pars)
    },

    .predict = function(task) {
      pars = self$param_set$get_values(tags = "predict")
      newdata = task$data(cols = task$feature_names)
      type = ifelse( == "response", "response", "prob")

      pred = mlr3misc::invoke(predict, self$model, newdata = newdata,
        type = self$predict_type, .args = pars)
      PredictioRegr$new(task = task, response = drop(response))
    }
  )
)
