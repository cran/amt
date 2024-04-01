## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE, message = FALSE
)

## ----habitat, fig.width = 7, fig.height = 4-----------------------------------
# Load all the packages we need
library(amt)
library(dplyr)
library(terra)
library(sf)

data(uhc_hab)
hab <- rast(uhc_hab, type = "xyz", crs = "epsg:32612")
# Convert "cover" layer to factor
levels(hab[[4]]) <- data.frame(id = 1:3,
                                    cover = c("grass", "forest", "wetland"))

# Affect habitat selection in simulation
plot(hab[[1:4]])

# Do not affect habitat selection in simulation
plot(hab[[5:7]])

## ----hsf prep-----------------------------------------------------------------
# Load data
data(uhc_hsf_locs)

# Split into train (80%) and test (20%)
set.seed(1)
uhc_hsf_locs$train <- rbinom(n = nrow(uhc_hsf_locs),
                             size = 1, prob = 0.8)
train <- uhc_hsf_locs[uhc_hsf_locs$train == 1, ]
test <- uhc_hsf_locs[uhc_hsf_locs$train == 0, ]

# Available locations (entire raster extent)
avail_train <- random_points(st_as_sf(st_as_sfc(st_bbox(hab))),
                             n = nrow(train) * 10)

avail_test <- random_points(st_as_sf(st_as_sfc(st_bbox(hab))),
                            n = nrow(test) * 10)

# Combine with used
train_dat <- train |>
  make_track(x, y, crs = 32612) |>
  mutate(case_ = TRUE) |>
  bind_rows(avail_train) |>
  # Attach covariates
  extract_covariates(hab) |>
  # Assign large weights to available
  mutate(weight = case_when(
    case_ ~ 1,
    !case_ ~ 5000
  ))

test_dat <- test |>
  make_track(x, y, crs = 32612) |>
  mutate(case_ = TRUE) |>
  bind_rows(avail_test) |>
  # Attach covariates
  extract_covariates(hab)

# Note 'weight' column not created for test data
# (we assume all variables in test are candidate habitat variables)

## ----hsf fit wrong------------------------------------------------------------
hsf_wrong <- glm(case_ ~ forage + temp + pred,
                 data = train_dat, family = binomial(), weights = weight)

## ----hsf fit right------------------------------------------------------------
hsf_right <- glm(case_ ~ forage + temp + I(temp^2) + pred + cover,
                 data = train_dat, family = binomial(), weights = weight)

## ----hsf prep_uhc-------------------------------------------------------------
# Prep under wrong model
uhc_hsf_wrong <- prep_uhc(object = hsf_wrong, test_dat = test_dat,
                          n_samp = 100, verbose = FALSE) # In reality n_samp should be around 1000.

# Prep under right model
uhc_hsf_right <- prep_uhc(object = hsf_right, test_dat = test_dat,
                          n_samp = 100, verbose = FALSE)

## ----hsf plot wrong, fig.width = 7, fig.height = 4.5--------------------------
plot(uhc_hsf_wrong)

## ----hsf plot right, fig.width = 7, fig.height = 4.5--------------------------
plot(uhc_hsf_right)

## ----hsf plot sub, fig.width = 7, fig.height = 4.5----------------------------
# By index
plot(uhc_hsf_right[c(1, 3)])

# By names (vector of >1 names also allowed)
plot(uhc_hsf_right["cover"])

## ----hsf plot env, fig.width = 7, fig.height = 4.5----------------------------
# Coerce to data.frame
df <- as.data.frame(uhc_hsf_right)

# Create confidence envelopes (95% and 100%)
env <- conf_envelope(df, levels = c(0.95, 1))

# Plot
plot(env)

## ----issf prep----------------------------------------------------------------
# Load data
data(uhc_issf_locs)

# Format as steps
steps <- uhc_issf_locs |>
  make_track(x, y, t, crs = 32612) |>
  steps()

# Split into train (80%) and test (20%)
set.seed(1)
steps$train <- rbinom(n = nrow(steps),
                      size = 1, prob = 0.8)
train <- steps[steps$train == 1, ]
test <- steps[steps$train == 0, ]

# Generate available steps, attribute
train_dat <- train |>
  random_steps(n_control = 15) |>
  # Attach covariates
  extract_covariates(hab) |>
  # Additional movement parameters
  mutate(log_sl_ = log(sl_),
         cos_ta_ = cos(ta_)) |>
  # Drop 'train' column
  dplyr::select(-train) |> 
  # Get rid of any NAs (sometimes available steps fall outside of raster)
  na.omit()

test_dat <- test |>
  random_steps(n_control = 15) |>
  # Attach covariates
  extract_covariates(hab) |>
  # Additional movement parameters
  mutate(log_sl_ = log(sl_),
         cos_ta_ = cos(ta_)) |>
  # Drop 'train' column
  dplyr::select(-train) |> 
  # Get rid of any NAs (sometimes available steps fall outside of raster)
  na.omit()

## ----issf fit wrong-----------------------------------------------------------
issf_wrong <- fit_issf(train_dat, 
                       case_ ~ 
                         # Habitat
                         forage + 
                         # Movement
                         sl_ + log_sl_ + cos_ta_ +
                         # Strata
                         strata(step_id_), model = TRUE)

## ----issf fit right-----------------------------------------------------------
issf_right <- fit_issf(train_dat, 
                       case_ ~ 
                         # Habitat
                         forage + temp + I(temp^2) + pred + cover + dist_to_cent +
                         # Movement
                         sl_ + log_sl_ + cos_ta_ +
                         # Strata
                         strata(step_id_), model = TRUE)

## ----issf prep_uhc------------------------------------------------------------
# Prep under wrong model
uhc_issf_wrong <- prep_uhc(object = issf_wrong, test_dat = test_dat,
                           n_samp = 20, verbose = FALSE)

# Prep under right model
uhc_issf_right <- prep_uhc(object = issf_right, test_dat = test_dat,
                           n_samp = 20, verbose = FALSE)

## ----issf plot wrong, fig.width = 7, fig.height = 4.5-------------------------
plot(uhc_issf_wrong)

## ----issf plot right, fig.width = 7, fig.height = 4.5-------------------------
plot(uhc_issf_right)

## ----as data frame------------------------------------------------------------
# Structure of `uhc_data` object
str(uhc_issf_right, 1)

# Coerce to data.frame
head(as.data.frame(uhc_issf_right), 10)

