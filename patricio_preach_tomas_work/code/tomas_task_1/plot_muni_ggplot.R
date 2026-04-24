# --- plot_muni_ggplot.R --------------------------------
library(sf)
library(ggplot2)

muni_sf <- get(load("data/clean/muni_division_2015_rand.Rdata"))

p <- ggplot(muni_sf) +
  geom_sf(aes(fill = rand_val), colour = NA) +
  scale_fill_viridis_c(option = "plasma", name = "Random\nvalue") +
  labs(title = "Brazilian municipalities – random smoke test") +
  theme_void()

print(p)            
