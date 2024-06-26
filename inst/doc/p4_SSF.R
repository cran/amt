## ----include=FALSE------------------------------------------------------------
library(knitr)
knitr::opts_chunk$set(message = FALSE, warning = FALSE)
set.seed(20161113)

## -----------------------------------------------------------------------------
library(lubridate)
library(amt)
data("deer")
deer

## -----------------------------------------------------------------------------
summarize_sampling_rate(deer)

## -----------------------------------------------------------------------------
sh_forest <- get_sh_forest()
sh_forest

## -----------------------------------------------------------------------------
ssf1 <- deer |> steps_by_burst()

## -----------------------------------------------------------------------------
ssf1 <- ssf1 |> random_steps(n_control = 15)

## -----------------------------------------------------------------------------
ssf1 <- ssf1 |> extract_covariates(sh_forest) 

## -----------------------------------------------------------------------------
ssf1 <- ssf1 |> 
  mutate(forest = factor(forest, levels = 1:0, labels = c("forest", "non-forest")), 
         cos_ta = cos(ta_), 
        log_sl = log(sl_)) 

## -----------------------------------------------------------------------------
m0 <- ssf1 |> fit_clogit(case_ ~ forest + strata(step_id_))
m1 <- ssf1 |> fit_clogit(case_ ~ forest + forest:cos_ta + forest:log_sl + log_sl * cos_ta + strata(step_id_))
m2 <- ssf1 |> fit_clogit(case_ ~ forest + forest:cos_ta + forest:log_sl + log_sl + cos_ta + strata(step_id_))
summary(m0)
summary(m1)
summary(m2)

## -----------------------------------------------------------------------------
m1 <- deer |> 
  steps_by_burst() |> random_steps(n = 15) |> 
  extract_covariates(sh_forest) |> 
  mutate(forest = factor(forest, levels = 1:0, labels = c("forest", "non-forest")), 
         cos_ta = cos(ta_), 
         log_sl = log(sl_)) |> 
  fit_clogit(case_ ~ forest + forest:cos_ta + forest:sl_ + sl_ * cos_ta + strata(step_id_))

## -----------------------------------------------------------------------------
summary(m1)

## -----------------------------------------------------------------------------
sessioninfo::session_info()

