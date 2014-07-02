#' @include model-bbinchoice.R
#' @include model-blogit.R
#' @include model-bprobit.R

library(jsonlite)

z5blogit <- zblogit$new()
z5blogit$toJSON()

z5bprobit <- zblogit$new()
z5bprobit$toJSON()

zeligchoicemodels <- list(zelig5choicemodels = list("blogit" = z5blogit$ljson,
                                                    "bprobit" = z5bprobit$ljson))

cat(toJSON(zeligchoicemodels, pretty = TRUE),
    file = file.path("JSON", "zelig5choicemodels.json"))
