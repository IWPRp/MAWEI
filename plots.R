# Extra analysis and plotting for Metro Atlanta
#
# Hassan Niazi, Sep 2025

source("functions.R")

# plot maps
sf_counties <- st_read(paste0(DATA_DIR, "geojson-counties-fips.json")) %>% rename_with(tolower)

sf_counties_GA <- sf_counties %>% filter(state == 13) # GA is 13
sf_counties_atlanta <- sf_counties %>% filter(id %in% fips)

plot(sf_counties_GA$geometry, col = "lightblue", border = "darkblue")
plot(sf_counties_atlanta$geometry, col = "lightblue", border = "darkblue")

county_colors <- rep(brewer.pal(11, "Spectral"), length.out = length(counties))
# county_colors <- sample(colors(), length(counties))
# county_colors <- viridis_discrete(length(counties), option = "plasma")

ggplot() +
  # geom_sf(data = sf_counties_GA, fill = "gray90", color = "gray") +
  geom_sf(data = sf_counties_atlanta, aes(fill = name, color = "white"), alpha = 0.75) +
  geom_sf_text(data = sf_counties_atlanta, aes(label = name), size = 3) +
  scale_fill_manual(values = county_colors) +
  theme_void() +
  labs(title = "Atlanta metro-area Counties in Georgia",
       caption = "", fill = "County", color = "County")

# better one (used this)
ggplot() +
  # geom_sf(data = sf_counties_GA, fill = "gray90", color = "gray") +
  geom_sf(data = sf_counties_atlanta, aes(fill = name), color = "white") +
  geom_sf_text(data = sf_counties_atlanta, aes(label = name), size = 3) +
  scale_fill_d3("category20", alpha = 0.75) +  # D3.js 20-color palette
  # or scale_fill_npg() for Nature Publishing Group colors
  # or scale_fill_aaas() for Science journal colors
  theme_void() +
  theme(legend.position = "none")

# cobb and douglas
ggplot() +
  # geom_sf(data = sf_counties_GA, fill = "gray90", color = "gray") +
  geom_sf(data = sf_counties_atlanta, color = "white") +
  geom_sf(data = sf_counties_atlanta %>% filter(name %in% c("Douglas", "Cobb")), aes(fill = name), color = "white") +
  geom_sf_text(data = sf_counties_atlanta, aes(label = name), size = 3) +
  scale_fill_d3("category20", alpha = 0.75) +  # D3.js 20-color palette
  # or scale_fill_npg() for Nature Publishing Group colors
  # or scale_fill_aaas() for Science journal colors
  theme_void() +
  theme(legend.position = "none")
