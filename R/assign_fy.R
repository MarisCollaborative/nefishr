#' Assign Fishing Year column 
#' 
#' Assigns the fishing year in which a trip was recorded for a given species based on the calendar year date and the fishing year intervals for a Fishery Management Plan
#' 
#' 
#' @param data a dataframe containing trip data for a given species
#' @param col the column containing the date a trip occurred
#' @param species_data a dataframe of species containing fishery management plan and fishing year information
#' @param impose.fy if the data is for a species that is not currently managed by the NEFMC, should a fishing year be imposed for the purposes of analysis. If so, which fishery management plan should be used to impose a fishing year perid. 
#' 
#' @importFrom lubridate %within%
#' 
#' 
#' @return a column in the user provided dataframe that contains values for the fishing year 
#' 
#' @export
#' 
#' @examples
#' 
#' 



# nefmc_species_itis <- unique(nefishr::nefmc_species$ITIS_TSN)
# nefmc_itis_sql <- sql_vector(nefmc_species_itis, collapse = ",", con = garfo.connection)

# landings_rs <- ROracle::dbSendQuery(conn = garfo.connection,
#                                           statement = stringr::str_c("SELECT CAMSID, DATE_TRIP, YEAR, ITIS_TSN, LNDLB, SUBTRIP FROM cams_garfo.cams_land WHERE ITIS_TSN IN", nefmc_itis_sql, "AND YEAR IN ('2016', '2017')", sep = " "))

# test_land <- DBI::dbFetch(landings_rs)

# species_data <- nefishr::nefmc_species |>
#   mutate(BEGIN_FY_MONTH = dplyr::case_when(
#     FMP %in% c("Northeast Multispecies", "Monkfish", "Spiny Dogfish", "Skates", "Small-Mesh Multispecies") ~ "May-1",
#     FMP  == "Sea Scallop" ~ "Apr-1",
#     FMP == "Atlantic Herring" ~ "Jan-1",
#     FMP == "Atlantic Deep-Sea Red Crab" ~ "Mar-1",
#     FMP == "Atlantic Salmon" ~ NA),

#     END_FY_MONTH = dplyr::case_when(
#       FMP %in% c("Northeast Multispecies", "Monkfish", "Spiny Dogfish", "Skates", "Small-Mesh Multispecies") ~ "Apr-30",
#       FMP  == "Sea Scallop" ~ "Mar-31",
#       FMP == "Atlantic Herring" ~ "Dec-31",
#       FMP == "Atlantic Deep-Sea Red Crab" ~ "Feb-28",
#       FMP == "Atlantic Salmon" ~ NA),
#   )


assign_fy <- function(data, col, species_data, impose.fy = NULL){

  if(!is.null(impose.fy)){
    impose_fy <- species_data |>
      dplyr::filter(FMP == {{ impose.fy }}) |>
      dplyr::select(BEGIN_FY_MONTH) |>
      dplyr::distinct()

    species_data <- species_data |>
      dplyr::mutate(BEGIN_FY_MONTH = tidyr::replace_na(BEGIN_FY_MONTH,
                                         impose_fy$BEGIN_FY_MONTH))
  }

  species <- species_data |>
    dplyr::select(BEGIN_FY_MONTH, ITIS_TSN) |> 
    dplyr::distinct()
    


    tidy_data <- data |>
    dplyr::mutate(rounded_date = lubridate::floor_date({{ col }}, "day")) |>
    dplyr::group_by(ITIS_TSN, YEAR) |>
    tidyr::nest() |>
    dplyr::left_join(species, by = "ITIS_TSN") |>
    dplyr::mutate(start_FY = lubridate::ymd(
      stringr::str_c(YEAR, BEGIN_FY_MONTH, sep = "-"), tz = "EST"), # add two new columns with date values for the start and end of the fishing year.
           end_FY = dplyr::case_when(
             BEGIN_FY_MONTH == "Jan-1" ~ start_FY + lubridate::days(365),
             TRUE ~ start_FY + lubridate::days(364)),
           interval = lubridate::interval(start_FY, end_FY, tzone = "EST")) |>
    dplyr::mutate(fy_data = purrr::map(data, ~dplyr::mutate(., FY = dplyr::case_when(
                                           rounded_date %within% interval ~ YEAR,
                                           rounded_date < start_FY ~ YEAR - 1,
                                           rounded_date > end_FY ~ YEAR + 1
                                         )))) |>
    dplyr::select(ITIS_TSN, YEAR, fy_data) |>
    tidyr::unnest(cols = fy_data)


  }
