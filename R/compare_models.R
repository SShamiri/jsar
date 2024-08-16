
#' Comparing models
#'
#' @param models an object containing the results returned by a model fitting function. Supported models are 'lm' & 'sfa', put multiple models in list
#' @param model_names vector of model names as string
#'
#' @return dataframe of compared models and list for individual models
#' @import dplyr
#' @importFrom stats formula
#' @importFrom broom tidy glance
#'@importFrom purrr reduce
#'@importFrom methods is
#'
#' @export
#'
#' @examples
#' load(daimend)
#'
#' mod1 <- lm(Volume ~ Girth + Height, data = datasets::trees)
#' mod2 <- lm(Volume ~ Girth , data = datasets::trees)
#' # Comparing models
#' compare_lm(list(mod1, mod2))$compare
#' compare_lm(mod2)

compare_models <- function(models, model_names = NULL) {

  # model obj lm or sfa
  if(
    !(methods::is(models, "lm") | methods::is(models[[1]], "lm") |
      methods::is(models, "frontier") | methods::is(models[[1]], "frontier"))
  ) {
    stop("models must be an object of lm or sfa")
  }
  if (methods::is(models, "lm") | methods::is(models[[1]], "lm")) {
    if (methods::is(models, "list")) {
      obj <- compare_lm_lst(models, column_labels = model_names)
    }
    if (methods::is(models, "lm")) {
      obj <- compare_lm_lst(list(models), column_labels = model_names)
    }
  }

  if (methods::is(models, "frontier") | methods::is(models[[1]], "frontier")) {
    if (methods::is(models, "list")) {
      obj <- compare_sfa_lst(models, column_labels = model_names)
    }
    if (methods::is(models, "frontier")) {
      obj <- compare_sfa_lst(list(models), column_labels = model_names)
    }
  }

  return(obj)
}
# Linear models -lm
compare_lm <- function(models, model_names = NULL) {

  if (methods::is(models, "list")) {
    obj <- compare_lm_lst(models, column_labels = model_names)
  }
  if (methods::is(models, "lm")) {
    obj <- compare_lm_lst(list(models), column_labels = model_names)
  }
  return(obj)
}

# tidy model with R-square
tidy_lm <- function(model) {
  # global binding
  sig <- p.value <- NULL
  broom::tidy(model) %>%
    add_row(term = "rsq", estimate = broom::glance(model)$r.squared) %>%
    add_row(term = "adj_rsq", estimate = broom::glance(model)$adj.r.squared) %>%
    add_row(term = "f_stat", estimate = broom::glance(model)$statistic, p.value = broom::glance(model)$p.value) %>%
    mutate(
      sig = case_when(
        p.value < 0.01 ~ "***",
        p.value > 0.01 & p.value < 0.05 ~ "**",
        p.value > 0.05 & p.value < 0.1 ~ "*",
        term == "rsq" ~ "",
        term == "adj_rsq" ~ ""
      ),
      model = Reduce(paste, deparse(stats::formula(model)))
    ) %>%
    mutate(across(where(is.numeric), round, 4)) %>%
    relocate(sig, .after = p.value) %>%
    mutate_if(is.character, ~ replace_na(., ""))
}

# compare compare model saved in a list approach for ols models
compare_lm_lst <- function(list_of_models, column_labels = NULL) {
  # global binding
  term <- estimate <- sig <- m1 <- NULL
  # save model objects into list
  lst <- list()
  for (i in 1:length(list_of_models)) {
    mod <- list_of_models[[i]] %>%
      tidy_lm()
    lst[[i]] <- mod
  }

  # model compact in list
  out <- list()
  for (i in 1:length(lst)) {
    rsq <- lst[[i]] %>%
      filter(term == "rsq") %>%
      pull(estimate)
    adj_rsq <- lst[[i]] %>%
      filter(term == "adj_rsq") %>%
      pull(estimate)
    f_stat <- lst[[i]] %>%
      filter(term == "f_stat") %>%
      pull(estimate)
    f_sig <- lst[[i]] %>%
      filter(term == "f_stat") %>%
      pull(sig)
    f_stat <- paste0(f_stat, f_sig)

    dat <- data.frame(term = lst[[i]]$term, m1 = paste0(lst[[i]]$estimate, "(", lst[[i]]$std.error, ")", lst[[i]]$sig)) %>%
      mutate(
        m1 = ifelse(term == "rsq", rsq, m1),
        m1 = ifelse(term == "adj_rsq", adj_rsq, m1),
        m1 = ifelse(term == "f_stat", f_stat, m1)
      )
    mod_name <- paste0("mod_", i)
    names(dat)[2] <- mod_name
    out[[i]] <- dat
  }
  # re-arrange variables
  n <- c()
  for (i in 1:length(lst)) {
    n <- c(n, out[[i]]$term)
  }
  n <- unique(n)
  a <- n[!grepl("rsq|adj_rsq|f_stat", n)]
  b <- n[grepl("rsq|adj_rsq|f_stat", n)]
  n <- c(a, b)
  term_df <- data.frame(term = n)
  out1 <- c(list(term_df), out)
  # convert list of compact models into data frame
  df <- out1 %>%
    purrr::reduce(left_join, by = "term") %>%
    mutate_if(is.character, ~ replace_na(., ""))
  # rename columns
  if (length(column_labels) > 0) {
    names(df)[2:ncol(df)] <- column_labels
  }
  return(list(compare = df, models = lst))
}

# Frontier models sfa
compare_sfa <- function(models, model_names = NULL) {

  if (methods::is(models, "list")) {
    obj <- compare_sfa_lst(models, column_labels = model_names)
  }
  if (methods::is(models, "frontier")) {
    obj <- compare_sfa_lst(list(models), column_labels = model_names)
  }
  return(obj)
}
# tidy model approach for frontier package
tidy_sfa <- function(model) {
  # global binding
  sig <- p.value <- NULL
  coef <- summary(model)
  coef$mleParam %>%
    data.frame() %>%
    rownames_to_column("term") %>%
    rename(estimate = 2, se = 3, z_value = 4, p_value = 5) %>%
    add_row(term = "efficiency", estimate = coef$efficMean) %>%
    add_row(term = "log likelihood", estimate = coef$mleLogl) %>%
    mutate(
      sig = case_when(
        p_value < 0.01 ~ "***",
        p_value > 0.01 & p_value < 0.05 ~ "**",
        p_value > 0.05 & p_value < 0.1 ~ "*"
      ),
      model = Reduce(paste, deparse(stats::formula(model)))
    ) %>%
    mutate(across(where(is.numeric), round, 4)) %>%
    mutate_if(is.character, ~ replace_na(., ""))
}

compare_sfa_lst <- function(list_of_models, column_labels = NULL) {
  # global binding
  term <- estimate <- sig <- m1 <- NULL
  # save model objects into list
  lst <- list()
  for (i in 1:length(list_of_models)) {
    mod <- list_of_models[[i]] %>%
      tidy_sfa()
    lst[[i]] <- mod
  }
  # model compact in list
  out <- list()
  for (i in 1:length(lst)) {
    eff <- lst[[i]] %>%
      filter(term == "efficiency") %>%
      pull(estimate)
    loglike <- lst[[i]] %>%
      filter(term == "log likelihood") %>%
      pull(estimate)
    dat <- data.frame(term = lst[[i]]$term, m1 = paste0(lst[[i]]$estimate, "(", lst[[i]]$se, ")", lst[[i]]$sig)) %>%
      mutate(
        m1 = ifelse(term == "efficiency", eff, m1),
        m1 = ifelse(term == "log likelihood", loglike, m1)
      )
    mod_name <- paste0("mod_", i)
    names(dat)[2] <- mod_name
    out[[i]] <- dat
  }
  # re-arrange variables
  n <- c()
  for (i in 1:length(lst)) {
    n <- c(n, out[[i]]$term)
  }
  n <- unique(n)
  a <- n[!grepl("sigmaSq|gamma|mu|efficiency|log likelihood", n)]
  b <- n[grepl("sigmaSq|gamma|mu|efficiency|log likelihood", n)]
  n <- c(a, b)
  term_df <- data.frame(term = n)
  out1 <- c(list(term_df), out)
  # convert list of compact models into data frame
  df <- out1 %>%
    purrr::reduce(left_join, by = "term") %>%
    mutate_if(is.character, ~ replace_na(., ""))
  # rename columns
  if (length(column_labels) > 0) {
    names(df)[2:ncol(df)] <- column_labels
  }
  return(list(compare = df, models = lst))
}
