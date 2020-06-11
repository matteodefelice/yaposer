#' Subset simulation data
#'
#' This function process YAPOS input data limiting the simulation to a list of zones.
#'
#' @param sim_data Simulation inputs loaded with `read_input_from_folder` or `read_input_from_netcdf`
#' @param zones Vector of the zones to be used for subsetting
#' @return YAPOS input data
#' @examples
#' target_nc_file <- system.file("extdata", "es-pt-fr.nc", package = "yaposer", mustWork = TRUE)
#' input_data_3_zones <- read_input_from_netcdf(target_nc_file)
#' input_data_2_zones <- subset_sim(input_data_3_zones, zones = c('ES', 'PT'))
#' @export

subset_sim <- function(sim_data, zones) {
  if (length(zones) == 0) {
    stop("Zones is empty")
  }
  if (!(all(zones %in% sim_data$ZONES))) {
    stop("Specified zones are not ALL in the listed zones in `sim_data`")
  }
  ret_data <- sim_data
  # LOAD and CHECK DIMENSIONS and COLNAMES ---------------------------------------
  id_zones <- match(zones, sim_data$ZONES)
  ret_data$ZONES <- ret_data$ZONES[id_zones]
  ret_data$NZONES <- length(ret_data$ZONES)

  ret_data$lin <- ret_data$lin %>%
    dplyr::filter(
      from %in% (id_zones - 1),
      to %in% (id_zones - 1)
    ) %>%
    rowwise() %>%
    mutate(
      from = match(sim_data$ZONES[from + 1], zones) - 1,
      to = match(sim_data$ZONES[to + 1], zones) - 1
    ) %>%
    ungroup()

  ret_data$NLINES <- nrow(ret_data$lin)

  ret_data$dem <- ret_data$dem %>%
    select(ret_data$ZONES)

  ret_data$ren <- ret_data$ren %>%
    select(ret_data$ZONES)

  ret_data$ren_pp <- ret_data$ren_pp %>%
    select(Technology, ret_data$ZONES)

  ret_data$gen <- ret_data$gen %>%
    dplyr::filter(
      bus %in% (id_zones - 1)
    ) %>%
    rowwise() %>%
    mutate(
      bus = match(sim_data$ZONES[bus + 1], zones) - 1,
    ) %>%
    ungroup()
  ret_data$UNITS <- ret_data$gen$Unit
  ret_data$NUNITS <- length(ret_data$UNITS)

  ret_data$avail <- ret_data$avail %>%
    select(ret_data$UNITS)

  ret_data$inflow <- ret_data$inflow %>%
    select(ret_data$UNITS)

  ret_data$stomin <- ret_data$stomin %>%
    select(ret_data$UNITS)

  return(ret_data)
}
