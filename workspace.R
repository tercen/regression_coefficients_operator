library(tercen)
library(dplyr, warn.conflicts = FALSE)
library(broom)

options("tercen.workflowId" = "7132ce367ee5df28fea4032b3f011888")
options("tercen.stepId"     = "426b00e4-b970-4042-9a53-93a10ac2da90")

getOption("tercen.workflowId")
getOption("tercen.stepId")

get.coefficients <- function(df) {
  cov <- colnames(df)[!colnames(df) %in% c(".y", ".ri")]
  
  form <- paste0(".y ~ ", paste0(cov, collapse = " + "), " - 1")
  
  m <- lm(formula = form, data = df)
  coefs <- broom::tidy(m)
  colnames(coefs) <- c("covariate", "coefficient", "std_error", "statistic", "p_value")
  coefs[["log10_p_value"]] <- -log10(coefs[["p_value"]])
  coefs[[".ri"]] <- df[[".ri"]][1]
  
  perc_var <- broom::tidy(anova(m)) %>% rename(covariate = term)
  perc_var[["perc_variance"]] <- 100 * perc_var$sumsq / sum(perc_var$sumsq)
  perc_var <- select(perc_var, perc_variance, covariate)
  
  df_out <- left_join(coefs, perc_var, "covariate")
  return(df_out)
}

ctx = tercenCtx()

covariates <- ctx$cselect(ctx$cnames) %>% mutate(.ci=seq_len(nrow(.))-1)

df_out <- ctx %>%
  select(.y, .ri, .ci) %>%
  left_join(covariates, ".ci") %>%
  select(-.ci) %>%
  group_by(.ri) %>%
  do(get.coefficients(.)) %>%
  ctx$addNamespace() 

df_out %>%
  ctx$save()

write.csv(df_out, "./tests/output.csv", row.names = FALSE, quote = FALSE)

