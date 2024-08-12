# correlation data frame with p-values
#' Correlation dataframe with p-values
#'
#' @param data numeric data frame
#'
#' @return correlation data frame with p-values in parenthesis
#' @import corrr
#' @importFrom stats cor cor.test
#' @importFrom tibble rownames_to_column
#'
#' @examples
#'
#' corr_df(datasets::iris[,-5])
#'
#' @export
corr_df <- function(data){
  #binding variables to NULL
   r <- p_value <- sig <- y <- NULL
  # only numeric columns are selected
  data <- select_if(data, is.numeric)
  # stop if no numeric columns
  if(ncol(data) < 2) stop("The dataframe must at least has 2 numeric columns")
  ## correlation data
  corr_df <- data %>%
    correlate() %>%
    stretch() %>%
    mutate(across(where(is.numeric), round,3)) %>%
    rename(cor = r) %>%
    drop_na()
  # # p-value for the correlation
  p_df <- colpair_map(data,cor_p_value) %>%
    mutate(across(where(is.numeric), round,4)) %>%
    stretch() %>%
    rename(p_value = r) %>%
    drop_na() %>%
    mutate(
      sig = case_when(
        p_value < 0.01 ~ "***",
        p_value > 0.01 & p_value < 0.05 ~ "**",
        p_value > 0.05 & p_value < 0.1 ~ "*"
      )
    )
  # df
  out_df <- corr_df %>%
    left_join(p_df) %>%
    mutate_if(is.character, ~replace_na(., "")) %>%
    mutate(r = paste0(cor, "(", p_value,")",sig)) %>%
    select(-cor, -p_value, -sig ) %>%
    pivot_wider(values_from= r, names_from=y)
  # get upper tringle
  mat <- as.data.frame(out_df)
  rownames(mat) <- mat[,1]
  mat <- mat[,-1]
  mat[lower.tri(mat)] <- 0
  last_col = colnames(mat)[ncol(mat)]
  last_row =  nrow(mat)
  #
  mat %>%
    tibble::rownames_to_column('term') %>%
    select(-!!last_col) %>%
    slice(-!!last_row) %>%
    mutate(across(where(is.character), ~na_if(., '0'))) %>%
    mutate_if(is.character, ~replace_na(., ""))
}

# function to calculate p-values
cor_p_value <- function(vec_a, vec_b){
  cor.test(vec_a, vec_b)$p.value
}
