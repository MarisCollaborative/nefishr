#' Get species code
#' 
#' A function to extract the species code or species name that is used within the GARFO Catch Accounting and Monitoring System database. 
#' 
#' @param species A character string of the species name 
# @param itis A numeric value representing the ITIS Taxonomic Serial Number
#'
#' 
#' @importFrom rlang .data
#' 
#' @return a data frame of species identification information (n x 3)
#' \item{itis_code}{ITIS Taxonomic Serial Number}
#' \item{species}{Full name of stock}
#' \item{FMP}{Fishery Management Council responsible for management}
#' 
#' @export
#' 
#' @examples
#' species <- "Atlantic cod"
#' get_species_code(species = species)
#' 
#' 
#' get_species_code(species = "hake")


get_species_code <- function(species = NULL){ #}, itis = NULL){
  
  # Error check for metric names
  if (is.null(species)) { # & is.null(itis))
    stop("If you do not know the ITIS code then please enter a value for the `species` argument. A character string of any part of the name.
         eg species = \"cod\"")
  }

  # if (!is.null(itis)) {
  #   res <- nefmc_species |> 
  #     dplyr::filter(ITIS_TSN == itis) |>
  #     dplyr::distinct(ITIS_TSN, FMP_NAME, FMP)

  # } else if (!is.null(species)) {
    res <- nefmc_species |> 
      dplyr::filter(grepl(species,
                          FMP_NAME)) |>
      dplyr::distinct(ITIS_TSN, FMP_NAME, FMP) |> 
      dplyr::rename(itis_code = ITIS_TSN,
                    species = FMP_NAME)
  # }

  # res <- res |> 
  #   dplyr::rename(ITIS = ITIS_TSN,
  #                 Species = FMP_NAME)
  
return(res)
# return(res$ITIS)

}