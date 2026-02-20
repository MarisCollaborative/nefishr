## code to prepare `nefmc_species` dataset 

library(DBI)
library(ROracle)
library(dbplyr)
library(stringr)
library(dplyr)

## GARFO Connection ####
#*Use your CAMS connection information*
garfo.host <- "155.206.52.11"
garfo.port <- 1522
garfo.service <- "db01p.garfoproddbsn.garfoprodvcn.oraclevcn.com"
garfo.drv <- DBI::dbDriver("Oracle")
garfo.user <- "YOUR USERNAME" ##FIXME
garfo.pw <- "YOUR PASSWORD " ##FIXME

# connection string
garfo.connect.string <- paste(
"(DESCRIPTION=",
"(ADDRESS=(PROTOCOL=tcp)(HOST=", garfo.host, ")(PORT=", garfo.port, "))",
"(CONNECT_DATA=(SERVICE_NAME=", garfo.service, ")))", sep = "")

# make the connection
garfo.connection <- ROracle::dbConnect(garfo.drv, username = garfo.user, password = garfo.pw, dbname = garfo.connect.string)

# send the species query to the CAMS database through the connection
nefmc_species_rs <- ROracle::dbSendQuery(conn = garfo.connection,
                                      statement = "SELECT * FROM cams_garfo.cfg_itis WHERE COUNCIL IN ('NEFMC', 'ASMFC/NEFMC', 'MAFMC/NEFMC', 'NEFMC/MAFMC')")

# save the database query into an R object.
nefmc_species <- DBI::dbFetch(nefmc_species_rs) 

# create a vector that is readable by an SQL query
nefmc_species_itis <- dbplyr::sql_vector(nefmc_species$ITIS_TSN, collapse = ",", con = garfo.connection)

# pull the stock areas for the NEFMC species from the CAMS database through the connection
statarea_stock_rs <- ROracle::dbSendQuery(conn = garfo.connection,
                                      statement = stringr::str_c("SELECT DISTINCT * FROM cams_garfo.cfg_statarea_stock WHERE ITIS_TSN IN", nefmc_species_itis, sep = " "))

# save the database query into an R object.
statarea_stock <- DBI::dbFetch(statarea_stock_rs) 

## NAME WRANGLE ####
# create a column of species name that could be used for ITIS look up 
species_lookup <- nefmc_species |> # from the nefmc_species table
  dplyr::select(ITIS_TSN, ITIS_NAME) |> # select the ITIS Code and Name columns
  dplyr::mutate(name_replace = stringr::str_replace(ITIS_NAME, " & ", "/"), # replace & with / in any names
         
         # from the name_replace column extract any letters preceded by the first comma 
         split_name1 = stringr::str_extract(name_replace, "[:alpha:]+(?=,)"), 
         
         # from the name_replace column, extract any letters/numbers followed by the first comma and replace the comma with a space
         split_name2 = stringr::str_replace(str_extract(name_replace, "(?<=,)[:graph:]+"), ",", " "), 

         # from the split_name2 column, extract any letters preceded by the first space
         split_name3 = stringr::str_extract(split_name2, "[:alpha:]+(?=[:space:])"),
         
         # from the split_name2 column, extract any letter/number followed by the first space
         split_name4 = stringr::str_extract(split_name2, "(?<=[:space:])[:graph:]+"),
         
         # from the name_replace column, extract letters/numbers within parentheses
         split_name5 = stringr::str_extract(name_replace, "(?<=\\()[:graph:]+(?=\\))"),
         
         # create an FMP_NAME column, where values are created where:
         SPECIES_NAME = dplyr::case_when(
          # the split_name5 column contains "DAB", input the combination of split_name4 and split_name3 
          split_name5 == "DAB" ~ stringr::str_c(split_name4, split_name3, sep = " "), 

          # the split_name5 column contains "WINDOWPANE", input the combination of split_name5 and split_name1 
          split_name5 == "WINDOWPANE" ~ stringr::str_c(split_name5, split_name1, sep = " "),

          # the split_name5 column contains "RED", "WHITE", or "RED/WHITE", input the combination of split_name4 and split_name1 
          split_name4 %in% c("RED", "WHITE", "RED/WHITE") ~  stringr::str_c(split_name4, split_name1, sep = " "), 
          
          # the split_name1 column contains "HERRING", input the combination of split_name3 and split_name1 
          split_name1 == "HERRING" ~ stringr::str_c(split_name3, split_name1, sep = " "),

          # the ITIS_NAME column contains "GOOSEFISH", input "MONKFISH" 
          ITIS_NAME == "GOOSEFISH" ~ "MONKFISH",
          
          # the split_name1 column contains "NA", carry over the value from the ITIS_NAME column 
          is.na(split_name1) ~ ITIS_NAME, 

          # all other cases, should input the combination of split_name2 and split_name1
          TRUE ~ stringr::str_c(split_name2, split_name1, sep = " ")
         ),

         # change the values in the new FMP_NAME column to sentence case
         SPECIES_NAME = stringr::str_to_sentence(SPECIES_NAME)
        ) |> 
  
  # clean up the table by removing all intermediary columns
  dplyr::select(ITIS_TSN, ITIS_NAME, SPECIES_NAME) 

# add new columns to the nefmc_species table and create new columns for fishing years
nefmc_species <- nefmc_species |> 
  # add the FMP_NAME column from the species_lookup table by joining based on the species code and name
  dplyr::left_join(species_lookup, by = c("ITIS_TSN", "ITIS_NAME")) |> 
  # add the stat area and estimation region columns from the statarea_stock table by joining based on the species code and the NE species code 
  dplyr::left_join(statarea_stock, by = c("ITIS_TSN", "DLR_NESPP3"="NESPP3")) |> 
  # add columns where: 
  dplyr::mutate(BEGIN_FY_MONTH = dplyr::case_when( # the beginning of the fishing year is dependent on the FMP
                    # when the FMP is one of those in this list, the fishing year begins May 1
                    FMP %in% c("Northeast Multispecies", "Monkfish", "Spiny Dogfish", "Skates", "Small-Mesh Multispecies") ~ "May-1", 
                    # when the FMP is Sea Scallop, the fishing year begins Apr 1
                    FMP  == "Sea Scallop" ~ "Apr-1", 
                    # when the FMP is Atlantic Herring, the fishing year begins Jan 1
                    FMP == "Atlantic Herring" ~ "Jan-1", 
                    # when the FMP is Deep-Sea Red Crab, the fishing year begins Mar 1
                    FMP == "Atlantic Deep-Sea Red Crab" ~ "Mar-1",
                    # Atlantic Salmon is a No-Take FMP, and therefore does not have a fishing year.
                    FMP == "Atlantic Salmon" ~ NA),
                # the end of the fishing year is dependent on the FMP
                END_FY_MONTH = dplyr::case_when(
                    # when the FMP is one of those in this list, the fishing year ends Apr 30
                    FMP %in% c("Northeast Multispecies", "Monkfish", "Spiny Dogfish", "Skates", "Small-Mesh Multispecies") ~ "Apr-30", 
                    # when the FMP Sea Scallop, the fishing year ends Mar 31
                    FMP  == "Sea Scallop" ~ "Mar-31", 
                    # when the FMP is Atlantic Herring, the fishing year ends Dec 31
                    FMP == "Atlantic Herring" ~ "Dec-31",
                    # when the FMP is Deep-Sea Red Crab, the fishing year ends Feb 28 
                    FMP == "Atlantic Deep-Sea Red Crab" ~ "Feb-28",
                    # Atlantic Salmon is a No-Take FMP, and therefore does not have a fishing year.
                    FMP == "Atlantic Salmon" ~ NA), 
                # the stock name is dependent on the statistical areas or the species estimation region
                STOCK_NAME = dplyr::case_when(
                  # when rows with Atlantic cod also have areas in this list, the stock name is SNE Atlantic cod
                  SPECIES_NAME == "Atlantic cod" & AREA %in% c(533:534, 537:539, 611:640) ~ str_c("SNE", SPECIES_NAME, sep = " "), 
                  # when rows with Atlantic cod also have areas in this list, the stock name is WGOM Atlantic cod
                  SPECIES_NAME == "Atlantic cod" & AREA %in% c(513:515,521,526,541) ~ str_c("WGOM", SPECIES_NAME, sep = " "), 
                  # when rows with Atlantic cod also have areas in this list, the stock name is GB Atlantic cod
                  SPECIES_NAME == "Atlantic cod" & AREA %in% c(464,522,525,542:543) ~ str_c("WGB", SPECIES_NAME, sep = " "), 
                  # when rows with Atlantic cod also have areas in this list, the stock name is GB Atlantic cod
                  SPECIES_NAME == "Atlantic cod" & AREA %in% c(551:552,561:562) ~ str_c("EGB", SPECIES_NAME, sep = " "), 
                  # when rows with Atlantic cod also have areas in this list, the stock name is EGOM Atlantic cod
                  SPECIES_NAME == "Atlantic cod" & AREA %in% c(465:512) ~ str_c("EGOM", SPECIES_NAME, sep = " "), 
                  # when rows with Haddock also have the GOM region, the stock name is GOM Haddock
                  SPECIES_NAME == "Haddock" & SPECIES_ESTIMATION_REGION %in% c("GOM", "EGB") ~ str_c(SPECIES_ESTIMATION_REGION, SPECIES_NAME, sep = " "),
                  # when rows with Haddock also have regions in this list, the stock name is WGB Haddock
                  SPECIES_NAME == "Haddock" & SPECIES_ESTIMATION_REGION %in% c("WGB_SNE", "MA") ~ str_c("WGB", SPECIES_NAME, sep = " "),
                  # when rows with Windowpane flounder also have the SNEMA region, the stock name is Southern Windowpane flounder
                  SPECIES_NAME == "Windowpane flounder" & SPECIES_ESTIMATION_REGION == "SNEMA" ~ str_c("Southern", SPECIES_NAME, sep = " "), 
                  # when rows with Windowpane flounder also have the GBGOM SNEMA region, the stock name is Northern Windowpane flounder
                  SPECIES_NAME == "Windowpane flounder" & SPECIES_ESTIMATION_REGION == "GBGOM" ~ str_c("Northern", SPECIES_NAME, sep = " "), 
                  # when rows with Yellowtail flounder also have regions in this list, the stock name is SNEMA Yellowtail flounder
                  SPECIES_NAME == "Yellowtail flounder" & SPECIES_ESTIMATION_REGION %in% c("SNE", "MA") ~ str_c("SNEMA", SPECIES_NAME, sep = " "),
                  # when rows with Yellowtail flounder also have regions in this list, the stock name is the combination of the region and Yellowtail flounder
                  SPECIES_NAME == "Yellowtail flounder" & SPECIES_ESTIMATION_REGION %in% c("GB", "CCGOM") ~ str_c(SPECIES_ESTIMATION_REGION, SPECIES_NAME, sep = " "), 
                  # for rows with Winter flounder, the stock name is the combination of the region and Winter flounder flounder
                  SPECIES_NAME == "Winter flounder"  ~ str_c(SPECIES_ESTIMATION_REGION, SPECIES_NAME, sep = " "),
                  # for rows with species in this list, the stock name is the combination of the region and the species
                  SPECIES_NAME %in% c("Monkfish", "Offshore hake", "Silver hake", "Red hake")  ~ str_c(str_to_title(SPECIES_ESTIMATION_REGION), SPECIES_NAME, sep = " "), 
                  # all other observations outside the criteria above, use the species name
                  TRUE ~ SPECIES_NAME))

# save an updated copy into the package
usethis::use_data(nefmc_species, overwrite = TRUE)
