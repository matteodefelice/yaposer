#' Load YAPOS input data.
#'
#' This function loads all the input data for a YAPOS simulation into a `list`
#'
#' The function looks for in the specified folder the following CSV files:
#' \itemize{
#'   \item `gen.csv`: generation units
#'   \item `lin.csv`: tranmission lines
#'   \item `dem.csv`: demand time-series
#'   \item `ren.csv`: time-series of non-dispatchable renewables
#'   \item `inflow.csv`: time-series of inflow
#'   \item `avail.csv`: time-series of units' availabilities
#'   \item `stomin`: time-series with the storage minimum
#' }
#' @param folder Path of the directory containing the files
#' @return A list containing all the data
#' @export
#' @importFrom magrittr %>%

read_input_from_folder <- function(folder) {

    if (!file.exists(folder)) {
    stop(glue("{folder} does not exist"))
  }
  if (!dir.exists(folder)) {
    stop(glue("{folder} is not a directory"))
  }
  # CHECK FILES -------------------------------------------------
  needed_files <- c("lin.csv", "avail.csv", "inflow.csv", "ren.csv", "gen.csv", "dem.csv", "stomin.csv")
  file_in_folder <- list.files(path = folder)
  if (any(is.na(match(needed_files, file_in_folder)))) {
    message(glue("Current files are missing from the folder {folder}"))
    print(needed_files[which(is.na(match(needed_files, file_in_folder)))])
    stop()
  }

  out_data <- list()
  # LOAD and CHECK DIMENSIONS and COLNAMES ---------------------------------------
  out_data$lin <- read_csv(glue("{folder}/lin.csv"),
    col_types = cols(
      line_name = col_character(),
      from = col_double(),
      to = col_double(),
      cap = col_double()
    )
  ) # must be nlines x 4
  out_data$NLINES <- nrow(out_data$lin)
  if (out_data$NLINES == 0) stop(glue("{folder}/lin.csv has zero lines"))
  if (ncol(out_data$lin) != 4) stop(glue("{folder}/lin.csv n. of columns != 4"))
  if (any(is.na(match(colnames(out_data$lin), c("line_name", "from", "to", "cap"))))) stop(glue("{folder}/lin.csv columns names must be [line_name, from, to, cap]"))

  out_data$avail <- read_csv(glue("{folder}/avail.csv"), col_types = cols())
  out_data$UNITS <- colnames(out_data$avail)
  out_data$NUNITS <- ncol(out_data$avail)
  out_data$NSTEPS <- nrow(out_data$avail)
  if (out_data$NUNITS == 0) stop(glue("{folder}/avail.csv has zero columns"))
  if (out_data$NSTEPS == 0) stop(glue("{folder}/avail.csv has zero rows"))
  if (any(out_data$avail < 0)) stop(glue("{folder}/avail.csv has a negative value"))
  if (any(out_data$avail > 1)) warning(glue("{folder}/avail.csv has value(s) > 1"))

  out_data$inflow <- read_csv(glue("{folder}/inflow.csv"), col_types = cols())
  if (ncol(out_data$inflow) == 0) stop(glue("{folder}/inflow.csv has zero columns"))
  if (ncol(out_data$inflow) != out_data$NUNITS) stop(glue("{folder}/inflow.csv n. of columns != avail.csv columns"))
  if (nrow(out_data$inflow) == 0) stop(glue("{folder}/inflow.csv has zero rows"))
  if (nrow(out_data$inflow) != out_data$NSTEPS) stop(glue("{folder}/inflow.csv n. of rows != avail.csv rows"))
  if (any(out_data$inflow < 0)) stop(glue("{folder}/inflow.csv has negative value(s)"))

  out_data$ren <- read_csv(glue("{folder}/ren.csv"), col_types = cols())
  out_data$NZONES <- ncol(out_data$ren)
  out_data$ZONES <- colnames(out_data$ren)
  if (out_data$NZONES == 0) stop(glue("{folder}/ren.csv has zero columns"))
  if (nrow(out_data$ren) == 0) stop(glue("{folder}/ren.csv has zero rows"))
  if (nrow(out_data$ren) != out_data$NSTEPS) stop(glue("{folder}/ren.csv n. of rows != avail.csv rows"))
  if (any(out_data$ren < 0)) stop(glue("{folder}/ren.csv has negative value(s)"))

  out_data$dem <- read_csv(glue("{folder}/dem.csv"),
    col_types = cols(
      .default = col_double()
    )
  )
  if (ncol(out_data$dem) == 0) stop(glue("{folder}/dem.csv has zero columns"))
  if (ncol(out_data$dem) != out_data$NZONES) stop(glue("{folder}/dem.csv n. of columns != dem.csv columns"))
  if (nrow(out_data$dem) == 0) stop(glue("{folder}/dem.csv has zero rows"))
  if (nrow(out_data$dem) != out_data$NSTEPS) stop(glue("{folder}/dem.csv n. of rows != avail.csv rows"))
  if (any(out_data$dem < 0)) stop(glue("{folder}/dem.csv has negative value(s)"))

  out_data$gen <- read_csv(glue("{folder}/gen.csv"),
    col_types = cols(
      Unit = col_character(),
      bus = col_double(),
      Technology = col_character(),
      Fuel = col_character(),
      cost = col_double(),
      co2_per_mwh = col_double(),
      max = col_double(),
      stomax = col_double(),
      min = col_double(),
      stomin = col_double()
    )
  )
  if (nrow(out_data$gen) == 0) stop(glue("{folder}/gen.csv has zero columns"))
  if (nrow(out_data$gen) != out_data$NUNITS) stop(glue("{folder}/gen.csv number of units not consistent"))
  needed_cols <- c(
    "Unit", "bus", "Technology", "Fuel", "cost", "co2_per_mwh",
    "max", "stomax", "min", "stomin"
  )
  if (any(is.na(match(needed_cols, colnames(out_data$gen))))) {
    stop(glue('Column names of {folder}/gen.csv must be ["Unit", "bus", "Technology", "Fuel", "cost", "co2_per_mwh",
"max", "stomax", "min", "stomin"]'))
  }

  out_data$stomin <- read_csv(glue("{folder}/stomin.csv"), col_types = cols())
  if (ncol(out_data$stomin) == 0) stop(glue("{folder}/stomin.csv has zero columns"))
  if (ncol(out_data$stomin) != out_data$NUNITS) stop(glue("{folder}/stomin.csv n. of columns != avail.csv columns"))
  if (nrow(out_data$stomin) == 0) stop(glue("{folder}/stomin.csv has zero rows"))
  if (nrow(out_data$stomin) != out_data$NSTEPS) stop(glue("{folder}/stomin.csv n. of rows != avail.csv rows"))
  if (any(out_data$stomin < 0)) stop(glue("{folder}/stomin.csv has negative value(s)"))

  return(out_data)
}
