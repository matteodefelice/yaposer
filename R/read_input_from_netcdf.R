#' Read simulation data from a NetCDF file
#'
#' This function opens a NetCDF file containing the results of a YAPOS simulation and extracts the inputs.
#'
#' @param nc_output_file Path to the NetCDF file
#' @return A list with all the inputs
#' @examples
#' target_nc_file <- system.file("extdata", "es-pt-fr.nc", package = "yaposer", mustWork = TRUE)
#' inputs <- read_inputs_from_netcdf(target_nc_file)
#' @importFrom dplyr rename
#' @importFrom dplyr select
read_inputs_from_netcdf <- function(nc_output_file) {
  f <- tidync::tidync(nc_output_file)

  out <- list()

  out$dem <- f %>%
    tidync::activate(demand) %>%
    tidync::hyper_tibble() %>%
    tidyr::pivot_wider(
      names_from = zone,
      values_from = demand
    ) %>%
    dplyr::select(-day)

  out$NSTEPS <- nrow(out$dem)
  out$NZONES <- ncol(out$dem)
  out$ZONES <- colnames(out$dem)

  out$ren <- f %>%
    tidync::activate(renewables) %>%
    tidync::hyper_tibble() %>%
    tidyr::pivot_wider(
      names_from = zone,
      values_from = renewables
    ) %>%
    dplyr::select(-day)

  out$ren_pp <- f %>%
    tidync::activate(renewables_pp) %>%
    tidync::hyper_tibble() %>%
    tidyr::pivot_wider(
      names_from = zone,
      values_from = renewables_pp
    )

  out$inflow <- f %>%
    tidync::activate(inflow) %>%
    tidync::hyper_tibble() %>%
    tidyr::pivot_wider(
      names_from = unit,
      values_from = inflow
    ) %>%
    dplyr::select(-day)

  out$avail <- f %>%
    tidync::activate(availability) %>%
    tidync::hyper_tibble() %>%
    tidyr::pivot_wider(
      names_from = unit,
      values_from = availability
    ) %>%
    dplyr::select(-day)

  out$stomin <- f %>%
    tidync::activate(storage_min) %>%
    tidync::hyper_tibble() %>%
    tidyr::pivot_wider(
      names_from = unit,
      values_from = storage_min
    ) %>%
    dplyr::select(-day)

  out$gen <- dplyr::bind_cols(
    f %>% tidync::activate(unit) %>% tidync::hyper_tibble() %>% dplyr::rename(Unit = unit),
    f %>% tidync::activate(unit_bus) %>% tidync::hyper_tibble() %>% dplyr::select(-unit),
    f %>% tidync::activate(unit_Technology) %>% tidync::hyper_tibble() %>% dplyr::select(-unit),
    f %>% tidync::activate(unit_Fuel) %>% tidync::hyper_tibble() %>% dplyr::select(-unit),
    f %>% tidync::activate(unit_cost) %>% tidync::hyper_tibble() %>% dplyr::select(-unit),
    f %>% tidync::activate(unit_co2_per_mwh) %>% tidync::hyper_tibble() %>% dplyr::select(-unit),
    f %>% tidync::activate(unit_max) %>% tidync::hyper_tibble() %>% dplyr::select(-unit),
    f %>% tidync::activate(unit_stomax) %>% tidync::hyper_tibble() %>% dplyr::select(-unit),
    f %>% tidync::activate(unit_min) %>% tidync::hyper_tibble() %>% dplyr::select(-unit),
    f %>% tidync::activate(unit_stomin) %>% tidync::hyper_tibble() %>% dplyr::select(-unit)
  ) %>%
    dplyr::rename_at(dplyr::vars(starts_with("unit")), dplyr::funs(sub("unit_", "", .)))

  out$UNITS <- out$gen$Unit
  out$NUNITS <- length(out$UNITS)

  out$lin <- dplyr::bind_cols(
    f %>% tidync::activate(line) %>% tidync::hyper_tibble() %>% dplyr::rename(line_name = line),
    f %>% tidync::activate(line_from) %>% tidync::hyper_tibble() %>% dplyr::select(-line),
    f %>% tidync::activate(line_to) %>% tidync::hyper_tibble() %>% dplyr::select(-line),
    f %>% tidync::activate(line_cap) %>% tidync::hyper_tibble() %>% dplyr::select(-line)
  ) %>%
    dplyr::rename_at(dplyr::vars(starts_with("line")), dplyr::funs(sub("line_", "", .)))

  out$NLINES <- nrow(out$lin)
  return(out)
}
