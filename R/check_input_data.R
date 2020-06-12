#' Check if YAPOS input data is well-formed and return also pre-processing analysis.
#'
#' This function check if a list contains all the right data structures needed for
#' a YAPOS simulation. It also checks if they are well-formed (dimensions and consistency).
#'
#' The function returns also a data frame containing pre-processing information on the simulation.
#' The data frame has the following columns:
#' \itemize{
#' \item `zone`
#' \item `demand`: annual total demand
#' \item `net_demand`: annual total net-demand
#' \item `cf_with_ntc`: the ratio between the net demand and the sum between total generation capacity and NTC
#' \item `cf`: the ratio between net demand and total capacity
#' \item `cf_must_run`: the ratio between net demand and must-run capacity (i.e. generation minimum)
#' \item `inflow`: annual total inflow
#' \item `inflow_capacity_ratio`: ratio between inflow and capacity of hydropower units
#' \item `inflow_storage_ratio`: ratio between inflow and storage capacity
#'
#' }
#'
#' @param s List containing all the inputs
#' @return data frame containing pre-processing information
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @importFrom dplyr summarise
#' @importFrom dplyr left_join
#' @importFrom dplyr inner_join
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr pull
#' @importFrom dplyr select_if

check_input_data <- function(s) {
  # S is structure returned by load_sim_data
  if (!is.list(s)) stop("The function check_input_data expects a list as input")
  if (length(s) != 14) stop("The input list length != 14")

  # SIZE
  if (!all(dim(s$lin) == c(s$NLINES, 4))) stop("`lin` structure has the wrong size [NLINES x 4]")

  if (!all(dim(s$gen) == c(s$NUNITS, 10))) stop("`gen` structure has the wrong size [NUNITS x 10]")

  if (!all(dim(s$avail) == c(s$NSTEPS, s$NUNITS))) stop("`avail` structure has the wrong size [NSTEPS x NUNITS]")
  if (!all(dim(s$inflow) == c(s$NSTEPS, s$NUNITS))) stop("`inflow` structure has the wrong size [NSTEPS x NUNITS]")
  if (!all(dim(s$stomin) == c(s$NSTEPS, s$NUNITS))) stop("`stomin` structure has the wrong size [NSTEPS x NUNITS]")

  if (!all(dim(s$ren) == c(s$NSTEPS, s$NZONES))) stop("`ren` structure has the wrong size [NSTEPS x NZONES]")
  if (!all(dim(s$dem) == c(s$NSTEPS, s$NZONES))) stop("`dem` structure has the wrong size [NSTEPS x NZONES]")

  if (!(ncol(s$ren_pp) == 1+s$NZONES)) stop("`ren_pp` structure has the wrong number of columns [1+NZONES]")

  # NAMES
  if (!all((s$gen$Unit) == s$UNITS)) stop("Unit names in `gen` MUST be the same of UNITS and in the same order")

  if (!all(colnames(s$avail) == s$UNITS)) stop("Column names of `avail` MUST be the same of UNITS and in the same order")
  if (!all(colnames(s$inflow) == s$UNITS)) stop("Column names of `inflow` MUST be the same of UNITS and in the same order")
  if (!all(colnames(s$stomin) == s$UNITS)) stop("Column names of `stomin` MUST be the same of UNITS and in the same order")

  if (!all(colnames(s$ren) == s$ZONES)) stop("Column names of `ren` MUST be the same of ZONES and in the same order")
  if (!all(colnames(s$dem) == s$ZONES)) stop("Column names of `dem` MUST be the same of ZONES and in the same order")

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
        tidyr::pivot_longer(
          names_to = "unit",
          values_to = "inflow",
          cols = everything()
        ) %>%
        mutate(zone = stringr::str_sub(unit, 1, 2)) %>%
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
  # Check if there are zones with zero inflow but non-zero hydro capacity
  if (any(merged_df$hydro_capacity[((merged_df$inflow)==0) | is.na(merged_df$inflow)] > 0)) {
    zones_with_inconsistent_hydro <- merged_df %>%
      dplyr::filter((inflow  ==0 ) | is.na(inflow),
                    hydro_capacity > 0) %>%
      pull(zone)
    warning(glue('The zones {paste(zones_with_inconsistent_hydro, collapse = ", ")} have zero inflow but hydropower capacity > 0'))

  }
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
    warning(glue('The zones {paste(zones_with_high_must_run, collapse = ", ")} have a ratio between net demand and must-run thermal capacity < 1.5'))
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
