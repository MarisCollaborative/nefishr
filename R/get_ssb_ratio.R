#' Get SSB Ratio
#' 
#' A function to extract the ratio of the estimated SSB from the most recent stock assessment relative to the estimated SSB at MSY for NEFMC managed species. 
#' 
#' @param species A character string of the species name 
#' @param itis A numeric value representing the ITIS Taxon Serial Number 
#' @param year A numeric value of the assement year of interest 
#' 
#' @importFrom rlang .data
#' @return a dataframe of SSB ratios of each species 
#' @export
#' 
#' @examples
#' species <- "Atlantic cod"
#' year <- 2024 
#' get_ssb_ratio(species = species, year = year)
#' 
#' itis <- 164712
#' get_ssb_ratio(itis = itis, year = year)


get_ssb_ratio <- function(species = NULL, itis = NULL, year = NULL){
    # extract stock assessment summary information from stocksmart package
    data <- stocksmart::stockAssessmentSummary |> 
      # clean the names for easier reference
      janitor::clean_names() |> 
      # filter the data for the user provided year and NEFMC jurisdictions
      dplyr::filter(assessment_year == {{year}}, 
                    jurisdiction %in% c("NEFMC", "NEFMC / MAFMC"))

    # if the user provides a species in the function, then
    if(!is.null(species)){
    # identify the stock's itis taxon serial number 
    stock <- stocksmart::get_species_itis(stock = {{species}}) 
    itis <- unique(stock$ITIS)
    
    # filter the stock assessment summary data based on the itis number
    ratio <- data |> 
        dplyr::filter(itis_taxon_serial_number %in% itis) |> 
        # select a specific set of columns
        dplyr::select(stock_id, stock_name, assessment_year, b_bmsy)
    
                        
    } else { # if the user provides the itis numeber instead of the species name in the function

    # filter the stock assessment summary data based on the itis number
    ratio <- data |> 
        dplyr::filter(itis_taxon_serial_number %in% {{itis}}) |> 
        # select a specific set of columns
        dplyr::select(stock_id, stock_name, assessment_year, b_bmsy)
    }
    
    # return the final dataframe containing ratios
    return(ratio)
}