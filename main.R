library(tercen)
library(tercenApi)
library(dplyr, warn.conflicts = FALSE)
library(purrr)
library(broom)

get.coefficients <- function(df) {
  cov <- colnames(df)[!colnames(df) %in% c(".y", ".ri")]
  
  form <- paste0(".y ~ ", paste0(cov, collapse = " + "), " - 1")
  
  m <- lm(formula = form, data = df)
  coefs <- broom::tidy(m)
  colnames(coefs) <- c("covariate", "coefficient", "std_error", "statistic", "p_value")
  coefs[["log10_p_value"]] <- -log10(coefs[["p_value"]])
  coefs[[".ri"]] <- df[[".ri"]][1]
  
  perc_var <- broom::tidy(anova(m)) %>% rename(variable = term)
  perc_var[["perc_variance"]] <- 100 * perc_var$sumsq / sum(perc_var$sumsq)
  perc_var <- select(perc_var, perc_variance, variable)
  perc_var[[".ri"]] <- df[[".ri"]][1]
  
  return(list(coefs, perc_var))
}

ctx = tercenCtx()

covariates <- ctx$cselect(ctx$cnames) %>% mutate(.ci=seq_len(nrow(.))-1)

df_out <- ctx %>%
  select(.y, .ri, .ci) %>%
  left_join(covariates, ".ci") %>%
  select(-.ci) %>%
  split(.$.ri) %>%
  map(~get.coefficients(.))

coefs <- lapply(df_out, "[[", 1) %>%
  bind_rows() %>%
  ctx$addNamespace() 

perc_var <- lapply(df_out, "[[", 2) %>%
  bind_rows() %>%
  ctx$addNamespace() 

ctx$save(list(coefs, perc_var))
