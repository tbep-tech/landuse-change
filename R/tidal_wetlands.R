library(tidyverse)
library(sf)
library(tbeptools)
library(here)
library(patchwork)
library(maptiles)
library(tidyterra)

# get tidal wetland spatial data --------------------------------------------------------------

dat1950raw <- st_read('T:/05_GIS/HISTORICAL/1950_LULC_Emergents/1950_emergent_habitat.shp')
dat1950 <- dat1950raw |> 
  select(FLUCCSCODE = HABITAT) |> 
  mutate(
    FLUCCSCODE = case_when(
      FLUCCSCODE == 'Mangrove/Spartina Marsh' ~ 6120, 
      FLUCCSCODE == 'Juncus Marsh' ~ 6420, 
      FLUCCSCODE == 'Salt Barren' ~ 6600
    )
  ) |> 
  st_transform(crs = st_crs(tbshed)) |> 
  st_intersection(tbshed) |>
  select(-Acres) |> 
  st_intersection(tbsegshed)

fls <- list.files('T:/05_GIS/SWFWMD/zipped_all_years/', pattern = 'zip', full.names = T)

out <- vector('list', length(fls))
names(out) <- basename(fls)
for(fl in fls){
  
  cat(fl, '\n')
  tmpdr <- tempdir()
  
  unzip(fl, exdir = tmpdr)
  
  shppth <- list.files(tmpdr, pattern = 'shp$', full.names = T, recursive = T)
  
  shp <- st_read(shppth, quiet = T)
  
  shpfilt <- shp |> 
    filter(FLUCCSCODE %in% c(6120, 6420, 6600)) |> # mangrove, salt marsh, salt barren
    st_transform(crs = st_crs(tbshed)) |> 
    select(FLUCCSCODE) |> 
    st_make_valid() |> 
    st_intersection(tbshed) |>
    select(-Acres) |> 
    st_intersection(tbsegshed)

  out[[basename(fl)]] <- shpfilt
  
  unlink(tmpdr, recursive = T)
  
}
  
swfwmdintertidal <- out
swfwmdintertidal <- c(list(`1950` = dat1950), swfwmdintertidal)
names(swfwmdintertidal) <- gsub('lulc|\\.zip', '', names(swfwmdintertidal))
save(swfwmdintertidal, file = 'data/swfwmdintertidal.RData', compress = 'xz')

# make plot -----------------------------------------------------------------------------------

load(file = here('data/swfwmdintertidal.RData'))

ests <- swfwmdintertidal |> 
  enframe(name = 'yr') |>
  mutate(
    value = map(value, function(x){
      
      x |>
        mutate(
          area = st_area(x)
        ) |> 
        st_set_geometry(NULL) |> 
        summarize(
          area = sum(area), 
          .by = c(FLUCCSCODE, bay_segment)
        )
    
    })
  ) |> 
  unnest('value')
  
toplo <- ests |> 
  mutate(
    yr = as.numeric(yr),
    yr = case_when(
      yr == 1950 ~ 1980, 
      TRUE ~ yr
    ),
    habitat = case_when(
      FLUCCSCODE == 6120 ~ 'Mangrove', 
      FLUCCSCODE == 6420 ~ 'Salt Marsh', 
      FLUCCSCODE == 6600 ~ 'Salt Barren'
    ), 
    habitat = factor(habitat, levels = c('Salt Barren', 'Salt Marsh', 'Mangrove')),
    area = units::set_units(area, 'acres'),
    area = as.numeric(area) / 1000, 
    bay_segment = factor(bay_segment, levels = c('OTB', 'HB', 'MTB', 'LTB', 'BCB', 'TCB', 'MR'), 
                         labels = c('Old Tampa Bay (OTB)', 'Hillsborough Bay (HB)', 'Middle Tampa Bay (MTB)', 'Lower Tampa Bay (LTB)', 
                                    'Boca Ciega Bay (BCB)', 'Terra Ceia Bay (TCB)', 'Manatee River (MR)'))
      
  )

p1 <- ggplot(toplo, aes(x = yr, y = area, fill = habitat)) +
  geom_col(color = 'darkgrey') + 
  scale_fill_manual(values = c('#FFFF14', '#1C99C7', '#83832E')) +
  scale_x_continuous(breaks = seq(1980, 2020, 10), labels = c(1950,  seq(1990, 2020, 10))) +
  facet_wrap(~bay_segment, scales = 'free_y', ncol = 2) + 
  geom_vline(xintercept = 1985, linetype = 'dashed') +
  theme_minimal() +
  theme(
    legend.position = c(0.75, 0.1), 
    panel.grid.minor = element_blank()
  ) +
  labs(
    x = NULL, 
    y = 'Acres (x1000)', 
    fill = NULL
  )

# base tiles
bbx <- tbsegshed %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  st_buffer(dist = units::set_units(1, kilometer)) %>%
  st_transform(crs = 4326) %>% 
  st_bbox()

tls <- get_tiles(bbx, provider = "CartoDB.PositronNoLabels", zoom = 10)
dat_ext <- st_as_sfc(bbx) %>% 
  st_transform(crs = 4326) %>% 
  st_bbox()

curex <- swfwmdintertidal[['2020']] |> 
  mutate(
    habitat = case_when(
      FLUCCSCODE == 6120 ~ 'Mangrove', 
      FLUCCSCODE == 6420 ~ 'Salt Marsh', 
      FLUCCSCODE == 6600 ~ 'Salt Barren'
    ),
    habitat = factor(habitat, levels = c('Salt Barren', 'Salt Marsh', 'Mangrove'))
  )

labs1 <- tbsegshed |> 
  filter(!bay_segment %in% c('OTB', 'HB', 'MTB', 'LTB')) |>
  st_centroid()
labs2 <- tbseg |> 
  st_centroid()
labs <- rbind(labs1, labs2)

m1 <- ggplot() + 
  geom_spatraster_rgb(data = tls, maxcell = 1e8) +
  geom_sf(data = tbsegshed, inherit.aes = F, linewidth = 1, fill = NA, color = 'darkgrey') +
  geom_sf(data = curex, aes(fill = habitat, color = habitat), show.legend = F) +
  geom_sf_text(data = labs, aes(label = bay_segment), inherit.aes = F) +
  annotate('text', x = -82.85, y = 27.37, label = 'Current extent (2020)', size = 5, fontface = 'bold', hjust = 0) +
  scale_fill_manual(values = c('#FFFF14', '#1C99C7', '#83832E')) +
  scale_color_manual(values = c('#FFFF14', '#1C99C7', '#83832E')) +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size = 7),
    legend.position = 'inside',
    legend.position.inside = c(0.15, 0.15),
    legend.background = element_blank(),
    legend.title = element_blank()
  ) +
  ggspatial::annotation_scale(location = 'tr', unit_category = 'metric') +
  coord_sf(xlim = c(dat_ext[1], -82.3), ylim = c(dat_ext[2], 28.1), expand = FALSE, crs = 4326)

p <- p1 + m1 + plot_layout(widths = c(1, 0.8))

png(here('figs/tidal_wetlands.png'), width = 9, height = 6, units = 'in', res = 300, family = 'serif')
print(p)
dev.off()
