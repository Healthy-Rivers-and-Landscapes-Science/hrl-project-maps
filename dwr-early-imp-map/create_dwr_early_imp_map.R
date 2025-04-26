library(ggplot2)
library(ggrepel)


### create projects ------------------------------------------------------------

# read in shapefile of early implementation projects
projects <- sf::st_read(here::here("data-raw", "HRL_projects"))
crs <- sf::st_crs(projects)

# clean sf object of early implementation projects
projects <- projects |>
  dplyr::rename(
    project_name = project_na,
    project_type = project_ty,
    habitat_goal = habitat_go,
    site_footprint_acres = site_footp,
    project_footprint_acres = project_fo
  ) |>
  dplyr::select(
    project_id,
    project_name,
    project_type,
    habitat_goal,
    site_footprint_acres,
    project_footprint_acres
  )

project_enrichment_cols <- tibble::tribble(
  ~project_id, ~project_abbrv, ~project_name_short,    ~hab_1,              ~hab_2,
  "ER020",     "MWT",          "McCormack Williamson", "trib_floodplain",   "trib_rearing", 
  "ER012",     "GSF",          "Grizzly Slough",       "trib_floodplain",   NA_character_,
  "FL1045",    "LEBLS",        "Lower Elkhorn Basin",  "bypass_floodplain", NA_character_,
  "FL1048",    "KOP",          "Kopta Slough",         "trib_floodplain",   NA_character_,
  "HS124",     "LEMBP",        "Little Egbert",        "tidal_wetland",     "bypass_floodplain",
  "ER021",     "PROS",         "Prospect Island",      "tidal_wetland",     NA_character_,
  "HS126",     "TID",          "Tide's End",           "bypass_floodplain", "tidal_wetland",
  "HS128",     "DUT",          "Dutch Slough",         "tidal_wetland",     "trib_floodplain"
)

projects <- projects |>
  dplyr::left_join(project_enrichment_cols, by = "project_id") |>
  dplyr::select(
    project_id,
    project_abbrv,
    project_name_short,
    hab_1, 
    hab_2,
    site_footprint_acres,
    project_footprint_acres,
    project_type,
    habitat_goal
  )

# add Sunset Pumps project
sunset_pumps <- tibble::tibble(
  project_id              = "SP000",
  project_abbrv           = "SUN",
  project_name_short      = "Sunset Pumps",
  hab_1                   = "passage",
  hab_2                   = NA_character_,
  site_footprint_acres    = NA_real_,
  project_footprint_acres = NA_real_,
  project_type            = "passage",
  habitat_goal            = NA_character_
)

sunset_pumps_geom <- sf::st_sfc(
  sf::st_point(c(-121.636048, 39.247715)), 
  crs = crs
)

sunset_pumps <- sunset_pumps |>
  sf::st_sf(geometry = sunset_pumps_geom)


### create watershed -----------------------------------------------------------

# import sf object of the Delta
# I would use the package `deltamapr`, but it's not available for my current version of R
delta_url <- paste0(
  "https://geowebservices.stanford.edu/geoserver/wfs?",
  "service=WFS&version=1.0.0&request=GetFeature&typeName=",
  "druid:qh320kj0191",
  "&outputFormat=application/json"
)

delta <- sf::st_read(dsn = delta_url) |>
  sf::st_transform(crs = crs) |>
  sf::st_make_valid() |>
  dplyr::filter(acres > 20000) |>
  dplyr::mutate(id = dplyr::row_number())

# make a bounding box polygon to zoom in on a portion of the Delta
bbox_delta_cropped <- projects |>
  dplyr::filter(project_abbrv != "KOP") |>
  sf::st_bbox() |>
  sf::st_as_sfc() |>
  sf::st_sf()

# check if CRS is projected and if not, transform to projected CRS
if (sf::st_is_longlat(bbox_delta_cropped)) {
  bbox_delta_cropped <- bbox_delta_cropped |>
    sf::st_transform(crs = 3310)
}

# expand the bbox by 10 miles
bbox_delta_cropped <- bbox_delta_cropped |>
  sf::st_buffer(units::set_units(10, mi) |> units::set_units(m))

# return to original CRS
bbox_delta_cropped <- bbox_delta_cropped |>
  sf::st_transform(crs = crs)

# crop the Delta to a zoomed-in portion
delta_cropped <- delta |>
  sf::st_intersection(bbox_delta_cropped)

# make a bounding box polygon to define the streams and rivers data pull
bbox_all <- projects |>
  sf::st_bbox() |>
  sf::st_as_sfc() |>
  sf::st_sf()

# check if CRS is projected and if not, transform to projected CRS
if (sf::st_is_longlat(bbox_all)) {
  bbox_all <- bbox_all |>
    sf::st_transform(crs = 3310)
}

# expand the bbox by 20 miles
bbox_all <- bbox_all |>
  sf::st_buffer(units::set_units(20, mi) |> units::set_units(m))

# return to original CRS
bbox_all <- bbox_all |>
  sf::st_transform(crs = crs)

# define and create a directory for NHDPlusHR data
nhdplushr_dir <- here::here("data-raw", "nhdplushr")
if (!dir.exists(nhdplushr_dir)) {dir.create(nhdplushr_dir)}

# pull NHDPlusHR data using a download rather than a straight API call for speed
huc8 <- nhdplusTools::get_huc(AOI = bbox_all, type = "huc08")
nhdplusTools::download_nhdplushr(nhd_dir = nhdplushr_dir, hu_list = huc8$huc8)

huc4_codes <- huc8 |>
  dplyr::pull(huc8) |>
  substr(1, 4) |>
  unique()

nhdplushr_huc2_path <- nhdplusTools::download_nhdplushr(
  nhd_dir = nhdplushr_dir,
  hu_list = huc4_codes,
  download_files = TRUE
)

# read in and clean flowlines
file_paths <- list.files(nhdplushr_huc2_path, full.names = TRUE)
gdb_file_paths <- file_paths[stringr::str_ends(file_paths, "\\.gdb$")]
huc4_codes_concat <- stringr::str_c(huc4_codes, collapse = "|")
gdb_file_paths <- gdb_file_paths[stringr::str_detect(gdb_file_paths, huc4_codes_concat)]

flowlines <- list()
vaa <- list()

for (i in 1:length(gdb_file_paths)) {
  file_path <- gdb_file_paths[i]
  
  fl <- sf::st_read(dsn = file_path, layer = "NHDFlowline")
  fl <- sf::st_transform(x = fl, crs = crs)
  fl <- sf::st_zm(fl)
  sf::st_geometry(fl) <- "geometry"
  names(fl) <- tolower(names(fl))
  
  v <- sf::st_read(dsn = file_path, layer = "NHDPlusFlowlineVAA")
  names(v) <- tolower(names(v))
  
  flowlines[[i]] <- fl
  vaa[[i]] <- v
}

flowlines <- do.call(what = "rbind", args = flowlines)
vaa <- do.call(what = "rbind", args = vaa)

# make a single flowlines object enriched with value-added attributes
flowlines <- dplyr::left_join(
  x = flowlines,
  y = vaa,
  by = c("nhdplusid", "reachcode", "vpuid")
)

# clean names to work with low-level topology tools
flowlines <- dplyr::select(
  flowlines,
  
  id = nhdplusid,
  
  fromnode = fromnode,
  tonode = tonode,
  divergence = divergence,
  wbid = wbarea_permanent_identifier,
  
  total_da_sqkm = totdasqkm,
  da_sqkm = areasqkm,
  length_km = lengthkm,
  pathlength_km = pathlength,
  arbolate_sum = arbolatesu,
  
  topo_sort = hydroseq,
  up_topo_sort = uphydroseq,
  dn_topo_sort = dnhydroseq,
  dn_minor_topo_sort = dnminorhyd,
  
  terminal_topo_sort = terminalpa,
  terminal_flag = terminalfl,
  start_flag = startflag,
  
  levelpath = levelpathi,
  up_levelpath = uplevelpat,
  dn_levelpath = dnlevelpat,
  
  stream_level = streamleve,
  dn_stream_level = dnlevel,
  stream_order = streamorde,
  stream_calculator = streamcalc,
  
  feature_type = ftype,
  feature_type_code = fcode,
  vector_proc_unit = vpuid,
  
  aggregate_id = reachcode,
  aggregate_id_from_measure = frommeas,
  aggregate_id_to_measure = tomeas
)

flowlines <- flowlines |>
  dplyr::filter(stream_order >= 7) |>
  sf::st_filter(bbox_all)

flowlines_cropped <- flowlines |>
  sf::st_filter(bbox_delta_cropped)

# read in and clean waterbodies
waterbodies <- list()

for (i in 1:length(gdb_file_paths)) {
  file_path <- gdb_file_paths[i]
  
  wb <- sf::st_read(dsn = file_path, layer = "NHDWaterbody")
  wb <- sf::st_transform(x = wb, crs = crs)
  wb <- sf::st_zm(wb)
  wb <- sf::st_make_valid(wb)
  sf::st_geometry(wb) <- "geometry"
  names(wb) <- tolower(names(wb))
  
  waterbodies[[i]] <- wb
}

waterbodies <- do.call(what = "rbind", args = waterbodies)

waterbodies <- dplyr::select(
  waterbodies,
  
  id = nhdplusid,
  gnis_id = gnis_id,
  gnis_name = gnis_name,
  
  area_sqkm = areasqkm,
  
  feature_type = ftype,
  feature_type_code = fcode,
  vector_proc_unit = vpuid,
  
  aggregate_id = reachcode
)

intersecting_indices <- sf::st_intersects(
  x = waterbodies,
  y = flowlines,
  sparse = TRUE
)

waterbodies <- waterbodies[lengths(intersecting_indices) > 0, ]

waterbodies_cropped <- waterbodies |>
  sf::st_intersection(bbox_delta_cropped)


### create maps ----------------------------------------------------------------

# create map label locations that dodge map objects
label_coords <- tibble::tribble(
  ~project_abbrv, ~label_x,  ~label_y,
  "MWT",          -121.4700, 38.31000,
  "GSF",          -121.3200, 38.26000,
  "LEBLS",        -121.7000, 38.62695,
  "KOP",          -122.0955, 39.93691,
  "LEMBP",        -121.7400, 38.19000,
  "PROS",         -121.7300, 38.30000,
  "TID",          -121.7000, 38.42500,
  "DUT",          -121.6639, 37.95000
)

label_coords <- label_coords |>
  dplyr::left_join(
    projects |> sf::st_drop_geometry() |> dplyr::select(project_abbrv, project_name_short),
    by = "project_abbrv"
  )

# create a projects map zoomed in on the Delta and bypasses
ggplot() +
  geom_sf(data = delta_cropped, color = "darkgrey", fill = "darkgrey") + 
  geom_sf(data = waterbodies_cropped, fill = "darkgrey", color = "darkgrey") +
  geom_sf(data = flowlines_cropped, aes(linewidth = stream_order), color = "darkgrey") + 
  geom_sf(
    data = projects |> dplyr::filter(project_abbrv != "KOP"),
    aes(fill = hab_1, color = hab_1)
  ) +
  scale_linewidth(range = c(0.1, 0.4), guide = "none") +
  scale_fill_discrete(
    name = "Primary habitat type",
    labels = c(
      tidal_wetland = "tidal wetland",
      trib_floodplain = "tributary floodplain",
      bypass_floodplain = "bypass floodplain"
    ),
    guide = guide_legend(override.aes = list(color = NA))
  ) +
  scale_color_discrete(guide = "none") +
  geom_text(
    data = label_coords |> dplyr::filter(project_abbrv != "KOP"),
    aes(
      x = label_x,
      y = label_y,
      label = stringr::str_wrap(project_name_short, width = 6)
    ),
    size = 3,
    fontface = "bold"
  ) +
  coord_sf() +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.title = element_text(face = "bold")
  )

# write out the Delta and bypasses map
ggsave(
  filename = "dwr_early_imp_map.png",
  path = here::here("dwr-early-imp-map"),
  device = "png",
  dpi = 300,
  bg = "white"
)