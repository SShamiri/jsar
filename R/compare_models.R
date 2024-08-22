
#' Comparing models
#'
#' @param models A statistical model object or a list of statistical model objects. Lists of models can be specified as
#' models = list(model.1, model.2, ...). Different object types can also be mixed.
#' @param model_names A character vector of labels for the model names. By default,
#' the models are named "mod_1", "mod_2", etc. Specifying model_names = c("Model name 1", "Model name 2") etc. overrides the default behavior.
#'
#' @return A dataframe of compared models for easy model comparison
#' @importFrom texreg extract
#' @importFrom stats lm
#' @importFrom plm plm
#'@importFrom purrr reduce
#' @export
#'
#' @examples
#' library(plm)
#' library(dplyr)
#'
#' data(worldprod, package = 'jsar')
#' countries <- c("Australia", "singapore", "china", "uk")
#'
#' # comparison with same model (lm) for each country
#' lm_models <- worldprod |>
#'   filter(country_name %in% countries) |>
#'   group_by(country_name) |>
#'   group_map(~ lm(log_gdp ~ log_capital + log_labour , data = .x))
#'
#' compare_models(lm_models, model_names = paste0('lm_',countries))
#'
#' # or with piping
#' worldprod |>
#'   filter(country_name %in% countries) |>
#'   group_by(country_name) |>
#'   group_map(~ lm(log_gdp ~ log_capital + log_labour , data = .x)) |>
#'   compare_models( model_names = paste0('lm_',countries))
#'
# comparison with differerent models
#' lm_model <- lm(log_gdp ~ log_capital + log_labour, data = worldprod)
#' plm_model <- plm::plm(log_gdp ~ log_capital + log_labour ,
#'                       model = 'within', index = c('country_name', 'year'),
#'                       data = worldprod)
#'
#' compare_models(list(lm_model, plm_model),model_names = c('Pool', 'Panel'))
#'
compare_models <- function(models, model_names =NULL){

  lst_mods <- lst_models(models)
  coef_names <- character()
  coef_values <- numeric()
  term <- NULL

  coef_names <- tibble::tibble(term = coef_names)
  out <- list()
  lst_gof <- list()
  for(i in 1 : length(lst_mods)){
    mod_name <- paste0('mod_', i)
    estimate = round(lst_mods[[i]]@coef,3)
    se = round(lst_mods[[i]]@se,3)
    sig = stars(lst_mods[[i]]@se)
    param <- paste0(estimate, '(', se, ')', sig)

    dat <- tibble::tibble(term = lst_mods[[i]]@coef.names, mod = param )
    names(dat)[2] = mod_name
    out[[i]] <- dat
    gof <- tibble::tibble(term = lst_mods[[i]]@gof.names, mod = round(lst_mods[[i]]@gof,3))
    names(gof)[2] = mod_name
    lst_gof[[i]] <- gof

  }
  out_df <- purrr::reduce(out, full_join, by = join_by(term) ) |>
    mutate(across(everything(), ~ replace(.x, is.na(.x), "-"))) |>
    add_row() |>
    mutate(across(everything(), ~ replace(.x, is.na(.x), "####"))) |>
    bind_rows(
      purrr::reduce(lst_gof, full_join, by = join_by(term)) |>
        mutate(across(everything(), ~ replace(.x, is.na(.x), "-")))
    )
  # rename columns
  if(length(model_names)>0) {
    names(out_df)[2:ncol(out_df)] = model_names
  }

  return(out_df)
}


lst_models <- function(mods, ...){
  # if a single model is handed over, put model inside a list
  if (!"list" %in% class(mods)[1]) {
    mods <- list(mods)
  }

  # create list of texreg objects
  models <- NULL
  for (i in 1:length(mods)) {
    model <- texreg::extract(mods[[i]], ...)
    if ("list" %in% class(model)) { # must be a nested list of models (e.g., systemfit)
      models <- append(models, model)
    } else { # normal case; one model
      models <- append(models, list(model))
    }
  }
  return(models)

}

stars <- function(p_value){
  stars <- NULL
  for(i in 1:length(p_value)){
    if(p_value[i] < 0.01){
      star <- '***'
    } else if(p_value[i] > 0.01 & p_value[i] < 0.05){
      star <- '**'
    } else if(p_value[i] > 0.05 & p_value[i] < 0.1){
      star <- '*'
    } else{
      star <- ''
    }
    stars <- append(stars, star)
  }
  return(stars)
}

