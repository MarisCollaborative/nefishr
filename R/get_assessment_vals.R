#' Get assessment values
#' 
#' A function to extract the F and SSB values from the most recent stock assessment relative to the estimated SSB at MSY for NEFMC managed species. 
#' 
#' @param species A character string of the species name 
#' @param itis A numeric value representing the ITIS Taxon Serial Number 
#' @param year A numeric value of the assement year of interest 
#' 
#' @importFrom rlang .data
#' 
#' @return a data frame of SSB ratios of each species (n x 27)
#' \item{stock_id}{NEFSC Stock ID}
#' \item{stock_name}{Full name of stock}
#' \item{jurisdiction}{Fishery Management Council responsible for management}
#' \item{fmp}{The FMP the stock is managed under}
#' \item{assessment_year}{Most recent assessment year}
#' \item{last_data_year}{The modst recent year of data used as input into the assessment}
#' \item{f_year}{Year of F Estimate}
#' \item{f_unit}{Unit of measure for F}
#' \item{f_basis}{Basis of F estimate}
#' \item{flimit}{The recommended fishing mortality limit from the assessment}
#' \item{flimt_basis}{Basis for F_limit}
#' \item{fmsy}{Estimated F that would produce MSY from a stock at B_MSY}
#' \item{fmsy_basis}{Basis of estimate of F_MSY}
#' \item{b_year}{Year of the biomass estimate}
#' \item{estimated_b}{Estimate of current biomass from the final, accepted assessment}
#' \item{b_unit}{Unit of measure for the biomass estimate}
#' \item{b_basis}{Basis for the biomass estimate; retrospective pattern adjustments are noted here}
#' \item{blimit}{The recommended biomass limit from the assessment}
#' \item{blimit_basis}{Basis for the recommended biomass limit}
#' \item{bmsy}{Estimated stock size that would produce MSY when fished at F_MSY}
#' \item{bmsy_basis}{Basis for the estimated B_MSY}
#' \item{b_bmsy}{Ratio of estimated SSB to SSB_MSY}
#' \item{msy}{Estimate of maximum sustainable yield}
#' \item{msy_unit}{Unit of measure for MSY}
#' \item{overfished}{If B_limit < B_MSY, stock is not considered overfished}
#' \item{overfishing}{If F_limit < F_MSY, stock is not considered to be experiencing overfishing}
#' 
#' @export
#' 
#' @examples
#' species <- "Atlantic cod"
#' year <- 2024 
#' get_assessment_vals(species = species, year = year)
#' 
#' itis <- 164712
#' get_assessment_vals(itis = itis, year = year)


get_assessment_vals <- function(species = NULL, itis = NULL, year = NULL){
  
    if (is.null(itis) & is.null(species)) {
    stop("If you do not know the ITIS code then please enter a value for the
    `species` argument. A character string of any part of the stock name.
         eg stock = \"cod\", stock = \"Georges Bank\"")
  }
    # extract stock assessment summary information from stocksmart package
    data <- stocksmart::stockAssessmentSummary |> 
      # clean the names for easier reference
      janitor::clean_names() |> 
      # filter the data for the user provided year and NEFMC jurisdictions
      dplyr::filter(assessment_year == {{year}}, 
                    jurisdiction %in% c("NEFMC", "NEFMC / MAFMC"))

    # if the user provides a species in the function and not an itis number, then
    if(!is.null(species) & is.null(itis)){
    # identify the stock's itis taxon serial number 
    stock <- stocksmart::get_species_itis(stock = {{species}}) 
    itis <- unique(stock$ITIS)
    }
    # filter the stock assessment summary data based on the itis number
    summary <- data |> 
        dplyr::filter(itis_taxon_serial_number %in% itis) #|> 
        # select a specific set of columns
        # dplyr::select(stock_id, stock_name, assessment_year, b_bmsy)
    
    # select the columns that contain the assessment values for SSB and F
    vals <- summary[, c(1:4, 13, 15, 35:42, 48:56, 58:59)] |> 
        dplyr::mutate(overfished = # add an overfished column with the following values, 
                        dplyr::case_when(
                            blimit < bmsy ~ "No", # Not overfished if blimit is less than bmsy
                            TRUE ~ "Yes" # stock is overfished         
        ), 
                      overfishing = 
                        dplyr::case_when(
                            flimit < fmsy ~ "No", # Overfishing is not occurring if flimit is less than fmsy
                            TRUE ~ "Yes" # overfishing is occurring
                        )
                )
                        
    #} else { # if the user provides the itis numeber instead of the species name in the function

    # filter the stock assessment summary data based on the itis number
    # summary <- data |> 
        # dplyr::filter(itis_taxon_serial_number %in% {{itis}}) |> 
        # select a specific set of columns
        # dplyr::select(stock_id, stock_name, assessment_year, b_bmsy)
    
    # vals <- summary[, c(1:4, 13, 15, 35:42, 48:56, 58:59)]
    #}
    
    # return the final dataframe containing assessment values
    return(vals)
}