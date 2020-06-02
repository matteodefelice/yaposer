#' Load and analyse YAPOS results
#'
#' This function loads all the results of the simulation contained in the target NetCDF file
#' and returns a set of pre-defined plots and tables. If the text output files containing the simulation dual
#' values are present in the same folder of the NetCDF file, this function loads and store their value in a data frame.
#'
#' Returned data include the following data frames:
#'
#' \itemize{
#' \item `all_gen`: details on generating units
#' \item `all_lines`: datailed list of transmission lines
#' \item `avail`: availability time-series
#' \item `final_dem`: time-series of demand
#' \item `ZONES`: vector containing all the names of the simulated zones
#' \item `final_ren`: time-series of non-dispatchable renewables
#' \item `shed`: time-series of daily shed load
#' \item `curt`: time-series of daily curtailed electricity
#' \item `storage_level`: storage levels at daily resolution for each generating unit. This data frame contains also availability and storage minimum
#' \item `final_prod_detailed`: generation for each generation unit including fuel type, zone of the unit and the emissions per MWh
#' \item `compare_entsoe`: comparison of the annual generation per zone and fuel with the ENTSO-E data (Statistical Factsheets) for 2016
#' \item `final_co2`: time-series of CO2 generation per zone
#' \item `flow`: time-series of electricity flow per line. It contains also the normalised values.
#' \item `summary_curt_shed`: table with the annual values of shed load and curtailed electricity
#' \item `all_inflow`: cumulated inflow per zone
#' \item `dual`: dual values for all problem constraints
#' }
#'
#' The plots are instead:
#' \itemize{
#' \item `single_res`: storage level per single unit
#' \item `all_res`: area plot of cumulated storage levels
#' \item `dispatch`: dispatch plots per zone
#' \item `annual_gen`: summary of annual generation
#' \item `entsoe`: comparison with ENTSO-E
#' \item `co2`: area plot of the cumulated CO2
#' \item `flow`: heat-map with the electricity normalised flows
#' \item `inflow`: cumulated inflows
#' \item `curtailment`: plot of curtailment
#' \item `shedding`: plot of shedding load
#' }
#'
#' The dual data is loaded only if the function finds in the same directory of `nc_output_file`
#' the txt files containing the dual results saved by YAPOS.
#'
#' @param nc_output_file Path of the NetCDF containing the results
#' @param create_plot Flag to define if the function should generate also the plots
#' @return A list containing results' data and plots
#' @examples
#' target_nc_file <- system.file("extdata", "es-pt-fr.nc", package = "yaposer", mustWork = TRUE)
#' res <- get_results(target_nc_file)
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_cols
#' @importFrom dplyr rename
#' @importFrom dplyr select

get_results <- function(nc_output_file, create_plots = TRUE) {
  f <- tidync::tidync(nc_output_file)
  # Returning data structures --------------------------------------------------
  out_plots <- list()
  out_data <- list()
  # Load units -----------------------------------------------------------------
  out_data$all_gen <- bind_cols(
    f %>% tidync::activate(unit) %>% tidync::hyper_tibble() %>% rename(Unit = unit),
    f %>% tidync::activate(unit_bus) %>% tidync::hyper_tibble() %>% select(-unit),
    f %>% tidync::activate(unit_Technology) %>% tidync::hyper_tibble() %>% select(-unit),
    f %>% tidync::activate(unit_Fuel) %>% tidync::hyper_tibble() %>% select(-unit),
    f %>% tidync::activate(unit_cost) %>% tidync::hyper_tibble() %>% select(-unit),
    f %>% tidync::activate(unit_co2_per_mwh) %>% tidync::hyper_tibble() %>% select(-unit),
    f %>% tidync::activate(unit_max) %>% tidync::hyper_tibble() %>% select(-unit),
    f %>% tidync::activate(unit_stomax) %>% tidync::hyper_tibble() %>% select(-unit),
    f %>% tidync::activate(unit_min) %>% tidync::hyper_tibble() %>% select(-unit),
    f %>% tidync::activate(unit_stomin) %>% tidync::hyper_tibble() %>% select(-unit)
  ) %>%
    rename_at(vars(starts_with("unit")), funs(sub("unit_", "", .)))

  # Load lines --------------------------------------------------------------
  out_data$all_lines <- bind_cols(
    f %>% tidync::activate(line) %>% tidync::hyper_tibble() %>% rename(line_name = line),
    f %>% tidync::activate(line_from) %>% tidync::hyper_tibble() %>% select(-line),
    f %>% tidync::activate(line_to) %>% tidync::hyper_tibble() %>% select(-line),
    f %>% tidync::activate(line_cap) %>% tidync::hyper_tibble() %>% select(-line)
  ) %>%
    rename_at(vars(starts_with("line")), funs(sub("line_", "", .)))
  ## AVAILABILITY --------------------------------------------------------------------
  out_data$avail <- f %>%
    tidync::activate(availability) %>%
    tidync::hyper_tibble()
  ## OUTPUT -----------------------------------------------------------------------------
  prod <- f %>%
    tidync::activate(production) %>%
    tidync::hyper_tibble()
  out_data$final_dem <- f %>%
    tidync::activate(demand) %>%
    tidync::hyper_tibble()
  out_data$ZONES <- f %>%
    tidync::activate(zone) %>%
    tidync::hyper_tibble() %>%
    pull(1)

  out_data$final_ren <- f %>%
    tidync::activate(renewables) %>%
    tidync::hyper_tibble()
  flow <- f %>%
    tidync::activate(flow) %>%
    tidync::hyper_tibble()
  inflow <- f %>%
    tidync::activate(inflow) %>%
    tidync::hyper_tibble()
  out_data$shed <- f %>%
    tidync::activate(shed_load) %>%
    tidync::hyper_tibble()
  out_data$curt <- f %>%
    tidync::activate(curtailed) %>%
    tidync::hyper_tibble()
  # SLACKS
  ws <- f %>%
    tidync::activate(water_slack) %>%
    tidync::hyper_tibble()
  ss <- f %>%
    tidync::activate(storage_slack) %>%
    tidync::hyper_tibble()
  cs <- f %>%
    tidync::activate(curtailment_slack) %>%
    tidync::hyper_tibble()
  # Storage levels
  sl <- f %>%
    tidync::activate(storage_level) %>%
    tidync::hyper_tibble() %>%
    left_join(
      out_data$all_gen %>%
        select(unit = Unit, bus)
    ) %>%
    mutate(zone = out_data$ZONES[bus + 1]) %>%
    left_join(
      f %>% tidync::activate(storage_min) %>% tidync::hyper_tibble()
    ) %>%
    left_join(
      out_data$avail
    ) %>%
    select(-bus)

  out_data$final_prod_detailed <- bind_rows(
    prod %>%
      left_join(
        out_data$all_gen %>% select(unit = Unit, fuel = Fuel, co2_per_mwh)
      ),
    out_data$final_ren %>%
      mutate(unit = glue("{zone}-ren"), Fuel = "ren", co2_per_mwh = 0) %>%
      select(production = renewables, unit, day, fuel = Fuel, co2_per_mwh),
    out_data$shed %>%
      mutate(unit = glue("{zone}-shed"), Fuel = "shed", co2_per_mwh = 0) %>%
      select(production = shed_load, unit, day, fuel = Fuel, co2_per_mwh)
  ) %>%
    mutate(zone = str_sub(unit, 1, 2)) %>%
    mutate(
      fuel_class = as.factor(fuel) %>%
        fct_recode(
          fossil = "GAS", fossil = "LIG", fossil = "OIL",
          fossil = "HRD",
          hydro = "WAT",
          renewables = "ren",
          nuclear = "NUC",
          other = "BIO"
        ) %>%
        fct_relevel("shed", "renewables", "hydro", "other", "fossil", "nuclear")
    )

  final_prod <- out_data$final_prod_detailed %>%
    group_by(
      day, fuel_class, zone
    ) %>%
    summarise(
      prod = sum(production)
    ) %>%
    ungroup()

  fuel_cmap <- c(
    "fossil" = "#d7642dff",
    "nuclear" = "#466eb4ff",
    "other" = "#facdd0",
    "renewables" = "#c9cf5c",
    "hydro" = "#00a0e1ff",
    "shed" = "red"
  )

  ## PLOT: Dispatch plot ------------------------------------------------------
  if (create_plots) {
    single_country_plots <- list()
    for (C in out_data$ZONES) {
      single_country_plots[[C]] <- ggplot(final_prod %>%
        dplyr::filter(zone == C), aes(x = day, y = prod)) +
        # geom_area(aes(fill = fuel_class)) +
        geom_bar(stat = "identity", aes(fill = fuel_class), width = 1) +
        geom_line(
          data = out_data$final_dem %>%
            dplyr::filter(zone == C),
          aes(y = demand)
        ) +
        scale_fill_manual(values = fuel_cmap) +
        theme_light()
    }
    out_plots[["dispatch"]] <- single_country_plots
  }

  ## PLOT: Summary annual  ------------------------------------------------------
  if (create_plots) {
    out_plots[["annual_gen"]] <- ggplot(final_prod %>%
      group_by(
        fuel_class, zone
      ) %>%
      summarise(
        prod = sum(prod)
      ), aes(x = zone)) +
      geom_bar(stat = "identity", aes(y = prod / 1e6, fill = fuel_class)) +
      geom_point(data = out_data$final_dem %>%
        group_by(zone) %>%
        summarise(dem = sum(demand)), aes(y = dem / 1e6)) +
      scale_fill_manual(values = fuel_cmap) +
      theme_light() +
      labs(
        y = "Generation (TWh)"
      )
  }
  ## COMPARISON ENTSO-E
  out_data$compare_entsoe <- inner_join(
    entsoe_data %>%
      select(zone = Country, variable, prod = TWh) %>%
      mutate(
        fuel_class = case_when(
          variable %in% c("Biogas", "Biomass", "Geothermal", "Mixed fules") ~ "other",
          str_detect(variable, "Fossil") ~ "fossil",
          variable %in% c("Hard coal", "Lignite", "Oil shale") ~ "fossil",
          variable %in% c("Hydro pure storage") ~ "hydro",
          variable %in% c("Nuclear") ~ "nuclear",
          variable %in% c(
            "Solar pv", "Wind offshore",
            "Wind onshore", "Hydro RoR and pondage"
          ) ~ "renewables"
        )
      ) %>%
      group_by(zone, fuel_class) %>%
      summarise(entsoe_prod = sum(prod, na.rm = TRUE)),
    final_prod %>%
      group_by(zone, fuel_class) %>%
      summarise(model_prod = sum(prod) / 1e6) %>%
      ungroup()
  ) %>%
    ungroup()
  if (create_plots) {
    out_plots[["entsoe"]] <- ggplot(out_data$compare_entsoe, aes(x = entsoe_prod, y = model_prod, color = fuel_class)) +
      geom_point() +
      facet_wrap(~zone, scales = "free") +
      theme_light() +
      geom_abline(slope = 1)
  }
  ## CO2 --------------------------------------------------------------

  out_data$final_co2 <- out_data$final_prod_detailed %>%
    mutate(
      kg_co2 = production * co2_per_mwh # kg
    ) %>%
    group_by(day, zone) %>%
    summarise(
      tons_co2 = sum(kg_co2) / 1e3
    )
  if (create_plots) {
    out_plots[["co2"]] <- ggplot(out_data$final_co2, aes(x = day, y = tons_co2, fill = zone)) +
      geom_area() +
      theme_light()
  }
  ## PLOT: Flows  ------------------------------------------------------

  out_data$flow <- flow %>%
    left_join(out_data$all_lines %>% select(line = name, cap)) %>%
    mutate(norm_flow = flow / cap)

  if (create_plots) {
    out_plots[["flow"]] <- ggplot(
      out_data$flow,
      aes(x = day, y = as.factor(line))
    ) +
      geom_tile(aes(fill = norm_flow)) +
      scale_fill_distiller(palette = "PRGn") +
      theme_light() +
      coord_equal(ratio = 20) +
      labs(
        x = "day",
        y = 'line'
      )
  }
  ## SHEDDING AND CURTAILMENT
  out_data$summary_curt_shed <- bind_rows(
    out_data$curt %>%
      group_by(zone) %>%
      summarise(energy = sum(curtailed)) %>%
      mutate(label = "curtailed"),
    out_data$shed %>%
      group_by(zone) %>%
      summarise(energy = sum(shed_load)) %>%
      mutate(label = "shed load")
  ) %>%
    pivot_wider(names_from = label, values_from = energy)
  if (any(out_data$summary_curt_shed$curtailed > 0)) {
    message("Curtailment values")
    print(
      out_data$summary_curt_shed %>%
        dplyr::filter(curtailed > 0)
    )
    if (create_plots) {
      out_plots$curtailment <- ggplot(
        out_data$curt,
        aes(x = day, y = curtailed, fill = zone)
      ) +
        geom_bar(stat = "identity", width = 1) +
        theme_light()
    }
  }
  if (any(out_data$summary_curt_shed$`shed load` > 0)) {
    message("Shed load values")
    print(
      out_data$summary_curt_shed %>%
        dplyr::filter(`shed load` > 0)
    )
    if (create_plots) {
      out_plots$shedding <- ggplot(
        out_data$shed,
        aes(x = day, y = shed_load, fill = zone)
      ) +
        geom_bar(stat = "identity", width = 1) +
        theme_light()
    }
  }

  ## WATER SLACK

  if (any(ws$water_slack > 0)) {
    message("Water slack total")
    print(
      ws %>%
        group_by(unit) %>%
        summarise(water_slack = sum(water_slack)) %>%
        dplyr::filter(water_slack > 0)
    )
  }
  if (any(ss$storage_slack > 0)) {
    message("Storage slack total")
    print(
      ss %>%
        group_by(unit) %>%
        summarise(storage_slack = sum(storage_slack)) %>%
        dplyr::filter(storage_slack > 0)
    )
  }
  if (any(cs$curtailment_slack > 0)) {
    message("Curtailment slack total")
    print(
      cs %>%
        group_by(zone) %>%
        summarise(curtailment_slack = sum(curtailment_slack)) %>%
        dplyr::filter(curtailment_slack > 0)
    )
  }

  ## Reservoir levels ------------------------------------------

  out_data$storage_level <- sl %>%
    group_by(unit) %>%
    dplyr::filter(any(storage_level > 0)) %>%
    ungroup()

  if (create_plots) {
    single_country_levels <- list()
    for (U in unique(out_data$storage_level$unit)) {
      single_country_levels[[U]] <- ggplot(
        out_data$storage_level %>%
          dplyr::filter(unit == U),
        aes(x = day, y = storage_level)
      ) +
        geom_area(alpha = 0.8) +
        theme_light()
    }
    out_plots[["single_res"]] <- single_country_levels

    out_plots[["all_res"]] <- ggplot(
      out_data$storage_level,
      aes(x = day, y = storage_level)
    ) +
      geom_area(aes(fill = unit)) +
      theme_light()
  }
  out_data$all_inflow <- inflow %>%
    mutate(zone = str_sub(unit, 1, 2)) %>%
    group_by(day, zone) %>%
    summarise(inflow = sum(inflow))

  if (create_plots) {
    out_plots[["inflow"]] <- ggplot(
      out_data$all_inflow,
      aes(x = day, y = inflow)
    ) +
      geom_area(aes(fill = zone)) +
      theme_light()
  }

  #################################################################
  ######################### DUALS ###################################
  #################################################################
  # IF IN THE BASENAME DIR THERE AERE DUAL FILES
  #
  other_files <- list.files(dirname(nc_output_file))
  if (any(str_detect(other_files, "dual_"))) {
    message("Dual files are present in the same folder of the results")
    read_dual <- function(filename) {
      d <- read_delim(filename,
        delim = " ",
        col_names = c("index", "t", "value"),
        col_types = cols()
      ) %>%
        mutate(
          t = str_sub(t, 1, -2) %>%
            as.numeric(),
          index = str_sub(index, 2, -2) %>%
            as.numeric()
        )
      if (nrow(d) <= 1) {
        return(NULL)
      }
      if (max(d$index) == (length(out_data$ZONES) - 1)) {
        d <- d %>%
          mutate(
            label = out_data$ZONES[index + 1]
          )
      } else if (max(d$index) == (nrow(out_data$all_lines) - 1)) {
        d <- d %>%
          mutate(
            label = paste0(out_data$all_lines$from, "-", out_data$all_lines$to)[index + 1]
          )
      } else {
        d <- d %>%
          mutate(
            label = out_data$all_gen$Unit[index + 1]
          )
      }
      return(d)
    }

    ld <- list.files(dirname(nc_output_file), pattern = "dual", full.names = TRUE)
    out_data$duals <- lapply(ld, read_dual)
    names(out_data$duals) <- basename(ld)
  }
  return(
    list(
      data = out_data,
      plots = out_plots
    )
  )
}
