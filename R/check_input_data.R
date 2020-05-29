#' Check if YAPOS input data is well-formed and return also pre-processing analysis.
#'
#' This function check if a list contains all the right data structures needed for
#' a YAPOS simulation. It also checks if they are well-formed (dimensions and consistency).
#'
#' The function returs also a data frame containing pre-processing information on the simulation
#'
#' @param s List containing all
#' @return A list containing all the data
#' @export

check_input_data <- function(s) {
  # S is structure returned by load_sim_data
  if (!is.list(s)) stop("The function check_input_data expects a list as input")
  if (length(s) != 13) stop("The input list length != 13")

  # SIZE
  if (!all(dim(o$lin) == c(o$NLINES, 4))) stop("`lin` structure has the wrong size [NLINES x 4]")

  if (!all(dim(o$gen) == c(o$NUNITS, 10))) stop("`gen` structure has the wrong size [NUNITS x 10]")

  if (!all(dim(o$avail) == c(o$NSTEPS, o$NUNITS))) stop("`avail` structure has the wrong size [NSTEPS x NUNITS]")
  if (!all(dim(o$inflow) == c(o$NSTEPS, o$NUNITS))) stop("`inflow` structure has the wrong size [NSTEPS x NUNITS]")
  if (!all(dim(o$stomin) == c(o$NSTEPS, o$NUNITS))) stop("`stomin` structure has the wrong size [NSTEPS x NUNITS]")

  if (!all(dim(o$ren) == c(o$NSTEPS, o$NZONES))) stop("`ren` structure has the wrong size [NSTEPS x NZONES]")
  if (!all(dim(o$dem) == c(o$NSTEPS, o$NZONES))) stop("`dem` structure has the wrong size [NSTEPS x NZONES]")

  # NAMES
  if (!all((o$gen$Unit) == o$UNITS)) stop("Unit names in `gen` MUST be the same of UNITS and in the same order")

  if (!all(colnames(o$avail) == o$UNITS)) stop("Column names of `avail` MUST be the same of UNITS and in the same order")
  if (!all(colnames(o$inflow) == o$UNITS)) stop("Column names of `inflow` MUST be the same of UNITS and in the same order")
  if (!all(colnames(o$stomin) == o$UNITS)) stop("Column names of `stomin` MUST be the same of UNITS and in the same order")

  if (!all(colnames(o$ren) == o$ZONES)) stop("Column names of `ren` MUST be the same of ZONES and in the same order")
  if (!all(colnames(o$dem) == o$ZONES)) stop("Column names of `dem` MUST be the same of ZONES and in the same order")

  # WARNING ISOLATED ZONES -------------------------------------
  if (any(is.na(match(seq(0, s$NZONES - 1), unique(c(s$lin$from, s$lin$to)))))) {
    disconnected_zones <- s$ZONES[is.na(match(seq(0, s$NZONES - 1), unique(c(s$lin$from, s$lin$to))))]
    warning(glue('The following zone(s) are not connected: {paste(disconnected_zones, collapse = ",")}'))
  }
  # CHECK ratio DEMAND/POWER -----------------------------------
  demand_df <- tibble(
    zone = s$ZONES,
    demand = colSums(s$dem) / 1e6,
    ren = colSums(s$ren) / 1e6,
    net_demand = demand - ren,
  )
  capacity_df <- s$gen %>%
    mutate(zone = s$ZONES[bus + 1]) %>%
    group_by(zone) %>%
    summarise(
      total_capacity = s$NSTEPS * sum(max) / 1e6,
      must_run_capacity = s$NSTEPS * sum(min[Technology != "HDAM"]) / 1e6,
      hydro_capacity = s$NSTEPS * sum(max[Technology == "HDAM"]) / 1e6,
      total_storage = sum(stomax) / 1e6
    ) %>%
    left_join(
      bind_rows(
        s$lin %>% mutate(zone = s$ZONES[from + 1]) %>% select(zone, cap),
        s$lin %>% mutate(zone = s$ZONES[to + 1]) %>% select(zone, cap)
      ) %>%
        group_by(zone) %>%
        summarise(ntc_capacity = s$NSTEPS * sum(cap) / 1e6),
      by = "zone"
    ) %>%
    left_join(
      s$inflow %>%
        select_if(~ any(. > 0)) %>%
        pivot_longer(
          names_to = "unit",
          values_to = "inflow",
          cols = everything()
        ) %>%
        mutate(zone = str_sub(unit, 1, 2)) %>%
        group_by(zone) %>%
        summarise(inflow = sum(inflow) / 1e6),
      by = "zone"
    )

  merged_df <- inner_join(
    demand_df,
    capacity_df,
    by = "zone"
  ) %>%
    mutate(
      cf = net_demand / total_capacity,
      cf_with_ntc = net_demand / (total_capacity + ntc_capacity),
      cf_must_run = net_demand / (must_run_capacity)
    )
  # Check RATIO CF/net demand -------------------------------------------
  if (any(merged_df$cf_with_ntc > 0.9)) {
    zones_with_high_cf <- merged_df %>%
      dplyr::filter(cf_with_ntc > 0.9) %>%
      pull(zone)
    warning(glue('The zones {paste(zones_with_high_cf, collapse = ", ")} have a ratio between net demand and total capacity (plus NTC) > 0.9'))
  }
  # Check RATIO must-run-CF/net demand -------------------------------------------
  if (any(merged_df$cf_must_run < 1.5)) {
    zones_with_high_must_run <- merged_df %>%
      dplyr::filter(cf_must_run < 1.5) %>%
      pull(zone)
    warning(glue('The zones {paste(zones_with_high_must_run, collapse = ", ")} have a ratio between net demand and  must-run thermal capacity < 1.5'))
  }
  # CHECK RATIO inflow/CAPACITY --------------------------------
  merged_df <- merged_df %>%
    mutate(
      inflow_capacity_ratio = inflow / hydro_capacity
    )
  if (any(merged_df$inflow_capacity_ratio > 0.9, na.rm = TRUE)) {
    zones_with_high_ratio <- merged_df %>%
      dplyr::filter(inflow_capacity_ratio > 0.9) %>%
      pull(zone)
    warning(glue('The zones {paste(zones_with_high_ratio, collapse = ", ")} have a ratio between inflow and hydro_capacity > 0.9'))
  }
  if (any(merged_df$inflow_capacity_ratio < 0.1, na.rm = TRUE)) {
    zones_with_low_ratio <- merged_df %>%
      dplyr::filter(inflow_capacity_ratio < 0.1) %>%
      pull(zone)
    warning(glue('The zones {paste(zones_with_low_ratio, collapse = ", ")} have a ratio between inflow and hydro_capacity < 0.1'))
  }
  # CHECK consistence INFLOW / STORAGe -------------------------
  merged_df <- merged_df %>%
    mutate(
      inflow_storage_ratio = inflow / total_storage
    )
  if (any(merged_df$inflow_storage_ratio > 0.9, na.rm = TRUE)) {
    zones_with_high_ratio <- merged_df %>%
      dplyr::filter(inflow_storage_ratio > 0.9) %>%
      pull(zone)
    warning(glue('The zones {paste(zones_with_high_ratio, collapse = ", ")} have a ratio between inflow and storage > 0.9'))
  }
  if (any(merged_df$inflow_storage_ratio < 0.1, na.rm = TRUE)) {
    zones_with_low_ratio <- merged_df %>%
      dplyr::filter(inflow_storage_ratio < 0.1) %>%
      pull(zone)
    warning(glue('The zones {paste(zones_with_low_ratio, collapse = ", ")} have a ratio between inflow and storage < 0.1'))
  }
  # RETURN NICE TABLE
  # Zone / demand / net demand / CF% (without NTC) / INFLOW / ratio inflow-capacity / ratio inflow-storage
  return(
    merged_df %>%
      select(
        zone, demand, net_demand, cf_with_ntc, cf, cf_must_run, inflow, inflow_capacity_ratio, inflow_storage_ratio
      )
  )
}
