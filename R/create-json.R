#' @include model-bbinchoice.R
#' @include model-blogit.R
#' @include model-bprobit.R

library(jsonlite)

z5blogit <- zblogit$new()
z5blogit$toJSON()

z5bprobit <- zblogit$new()
z5bprobit$toJSON()

z5mlogit <- zmlogit$new()
z5mlogit$toJSON()

z5ologit <- zologit$new()
z5ologit$toJSON()

z5oprobit <- zologit$new()
z5oprobit$toJSON()

zeligchoicemodels <- list(zelig5choicemodels = list("blogit" = z5blogit$ljson,
                                                    "bprobit" = z5bprobit$ljson,
                                                    "mlogit" = z5mlogit$ljson,
                                                    "ologit" = z5ologit$ljson,
                                                    "oprobit" = z5oprobit$ljson))

cat(toJSON(zeligchoicemodels, pretty = TRUE),
    file = file.path("inst/JSON", "zelig5choicemodels.json"))
