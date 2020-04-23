## ---- include=FALSE-----------------------------------------------------------
library(knitr)
knitr::opts_chunk$set(message = FALSE, warning = FALSE)
set.seed(20161113)

## -----------------------------------------------------------------------------
library(raster)
library(lubridate)
library(amt)

set.seed(123)

trks <- tibble(
  id = 1:10, 
  trk = map(1:10, ~ tibble(x = cumsum(rnorm(100)), y = cumsum(rnorm(100)), 
                           ts = ymd_hm("2019-01-01 00:00") + hours(0:99)))
)

dat <- unnest(trks, cols = trk)


## -----------------------------------------------------------------------------
r <- stack(map(1:15, 
    ~ raster(xmn = -100, xmx = 100, ymn = -100, ymx = 100, 
             res = 1, vals = runif(4e4, ., . + 1))))

r <- setZ(r, ymd_hm("2019-01-01 00:00") + hours(seq(0, by = 10, len = 15)))

## -----------------------------------------------------------------------------
trk <- make_track(dat, x, y, ts, id = id)

# For one animal
trk %>% filter(id == 1) %>% 
  extract_covariates_var_time(r, max_time = hours(10 * 24)) -> t1

t1$time_var_covar


## -----------------------------------------------------------------------------
trk %>% 
  nest(data = -c(id)) %>% 
  mutate(data = map(data, ~ .x %>% extract_covariates_var_time(r, max_time = hours(10 * 24)))) %>% 
  unnest(cols = data) -> trk2

trk2


## -----------------------------------------------------------------------------
sessionInfo()

