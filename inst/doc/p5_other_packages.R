## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE)

## -----------------------------------------------------------------------------
library(amt)
data("deer")
class(deer)
deer

## -----------------------------------------------------------------------------
adehabitatLT_ltraj <- as_ltraj(deer)
class(adehabitatLT_ltraj)
adehabitatLT_ltraj

## -----------------------------------------------------------------------------
ctmm_obj <- as_telemetry(deer)
class(ctmm_obj)
head(ctmm_obj)

## -----------------------------------------------------------------------------


## -----------------------------------------------------------------------------
sessioninfo::session_info()

