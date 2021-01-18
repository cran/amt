## ---- include=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE)

## -----------------------------------------------------------------------------
library(amt)
library(ggplot2)
library(tidygraph)
library(ggraph)

## -----------------------------------------------------------------------------
leroy <- amt_fisher %>% filter(name == "Leroy")
lupe <- amt_fisher %>% filter(name == "Lupe")

## -----------------------------------------------------------------------------
trast <- make_trast(amt_fisher %>% filter(name %in% c("Leroy", "Lupe")), res = 50)

## -----------------------------------------------------------------------------
hr_leroy <- hr_kde(leroy, trast = trast, levels = c(0.5, 0.9))
hr_lupe <- hr_kde(lupe, trast = trast, levels = c(0.5, 0.9))

## -----------------------------------------------------------------------------
hr_overlap(hr_leroy, hr_lupe, type = "hr") 
hr_overlap(hr_lupe, hr_leroy, type = "hr")

## -----------------------------------------------------------------------------
hr_overlap(hr_leroy, hr_lupe, type = "phr", conditional = FALSE) 
hr_overlap(hr_lupe, hr_leroy, type = "phr", conditional = FALSE)

## -----------------------------------------------------------------------------
hr_overlap(hr_leroy, hr_lupe, type = "phr", conditional = TRUE) 
hr_overlap(hr_lupe, hr_leroy, type = "phr", conditional = TRUE)

## -----------------------------------------------------------------------------
hr_overlap(hr_lupe, hr_leroy, type = "vi", conditional = FALSE)
hr_overlap(hr_leroy, hr_lupe, type = "vi", conditional = FALSE)

## -----------------------------------------------------------------------------
trast <- make_trast(lupe, res = 50)

## -----------------------------------------------------------------------------
dat <- lupe %>% 
  mutate(week = lubridate::floor_date(t_, "week")) %>% 
  nest(data = -week) %>% 
  mutate(kde = map(data, hr_kde, trast = trast, levels = c(0.5, 0.95, 0.99)))

## -----------------------------------------------------------------------------
hr_overlap(dat$kde, type = "vi")

## -----------------------------------------------------------------------------
hr_overlap(dat$kde, type = "vi", conditional = TRUE)

## -----------------------------------------------------------------------------
hr_overlap(dat$kde, type = "vi", labels = dat$week)

## -----------------------------------------------------------------------------
data("puechabonsp", package = "adehabitatMA")
dat <- puechabonsp$relocs %>% as.data.frame() %>% 
  make_track(X, Y, id = Name)
trast <- make_trast(dat, res = 50)
dat1 <- dat %>% nest(data = -id) %>% 
  mutate(kde = map(data, ~ hr_kde(., trast = trast, level = c(0.5, 0.9, 0.99))))

## ---- fig.width=7, fig.height=7-----------------------------------------------
ov2 <- hr_overlap(dat1$kde, type = "hr", labels = dat1$id, which = "all", 
                  conditional = TRUE) %>% 
  filter(overlap > 0)
graph <- as_tbl_graph(ov2) %>% 
  mutate(Popularity = centrality_degree(mode = 'in')) 

ggraph(graph, layout = 'stress') + 
  #geom_edge_fan(aes(col = overlap), show.legend = TRUE, arrow = arrow()) + 
  geom_edge_arc(aes(col = overlap), arrow = arrow(length = unit(4, 'mm'), type = "closed"), 
                start_cap = circle(3, 'mm'),
                end_cap = circle(3, 'mm')) + 
  geom_node_point(size = 4) + 
  geom_node_label(aes(label = name), repel = TRUE, alpha = 0.7) +
  facet_edges(~ levels, ncol = 2) + 
  theme_light() +
  scale_edge_color_gradient(low = "blue", high = "red")

## -----------------------------------------------------------------------------
poly <- bbox(lupe, buffer = -500, sf = TRUE)
poly1 <- bbox(lupe, sf = TRUE)
hr <- hr_mcp(lupe)
ggplot() + geom_sf(data = hr_isopleths(hr)) + 
  geom_sf(data = poly, fill = NA, col = "red") +
  geom_sf(data = poly1, fill = NA, col = "blue")

## -----------------------------------------------------------------------------
hr_overlap_feature(hr, poly, direction = "hr_with_feature")
hr_overlap_feature(hr, poly1, direction = "hr_with_feature")

hr_overlap_feature(hr, poly, direction = "feature_with_hr")
hr_overlap_feature(hr, poly1, direction = "feature_with_hr")

## -----------------------------------------------------------------------------
hr <- hr_mcp(lupe, levels = c(0.5, 0.9, 0.95))
hr_overlap_feature(hr, poly, direction = "hr_with_feature")

## -----------------------------------------------------------------------------
sessioninfo::session_info()

