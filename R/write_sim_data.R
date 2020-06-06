#' Write simulation data
#'
#' this function writes the input data into a folder for a YAPOS simulation.
#'
#' @param sim_data Simulation inputs loaded with `read_input_from_folder` or `read_input_from_netcdf`
#' @param target_folder Target folder
#' @export
write_sim_data <- function(sim_data, target_folder) {
  if (dir.exists(target_folder)) {
    warning(glue("{target_folder} already exist: files will be overwritten"))
  }
  dir.create(target_folder, recursive = TRUE)
  # LOAD and CHECK DIMENSIONS and COLNAMES ---------------------------------------
  write_csv(sim_data$lin, glue("{target_folder}/lin.csv"))
  write_csv(sim_data$avail, glue("{target_folder}/avail.csv"))
  write_csv(sim_data$inflow, glue("{target_folder}/inflow.csv"))
  write_csv(sim_data$ren, glue("{target_folder}/ren.csv"))
  write_csv(sim_data$ren_pp, glue("{target_folder}/ren_pp.csv"))
  write_csv(sim_data$dem, glue("{target_folder}/dem.csv"))
  write_csv(sim_data$gen, glue("{target_folder}/gen.csv"))
  write_csv(sim_data$stomin, glue("{target_folder}/stomin.csv"))
}
