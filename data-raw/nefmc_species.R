## code to prepare `nefmc_species` dataset 

library(DBI)
library(ROracle)

## GARFO Connection ####
#*Use your CAMS connection information*
garfo.host <- "155.206.52.11"
garfo.port <- 1522
garfo.service <- "db01p.garfoproddbsn.garfoprodvcn.oraclevcn.com"
garfo.drv <- dbDriver("Oracle")
garfo.user <- "YOUR USERNAME" ##FIXME
garfo.pw <- "YOUR PASSWORD " ##FIXME

# connection string
garfo.connect.string <- paste(
"(DESCRIPTION=",
"(ADDRESS=(PROTOCOL=tcp)(HOST=", garfo.host, ")(PORT=", garfo.port, "))",
"(CONNECT_DATA=(SERVICE_NAME=", garfo.service, ")))", sep = "")

# make the connection
garfo.connection <- dbConnect(garfo.drv, username = garfo.user, password = garfo.pw, dbname = garfo.connect.string)

# send the species query to the CAMS database through the connection
nefmc_species_rs <- ROracle::dbSendQuery(conn = garfo.connection,
                                      statement = "SELECT * FROM cams_garfo.cfg_itis WHERE COUNCIL IN ('NEFMC', 'ASFMC/NEFMC', 'MAFMC/NEFMC', 'NEFMC/MAFMC')")

# save the database query into an R object.
nefmc_species <- dbFetch(nefmc_species_rs) 

## NAME WRANGLE ####
# create a column of species name that could be used for ITIS look up 
species_lookup <- nefmc_species |> # from the nefmc_species table
  select(ITIS_TSN, ITIS_NAME) |> # select the ITIS Code and Name columns
  mutate(name_replace = str_replace(ITIS_NAME, " & ", "/"), # replace & with / in any names
         
         # from the name_replace column extract any letters preceded by the first comma 
         split_name1 = str_extract(name_replace, "[:alpha:]+(?=,)"), 
         
         # from the name_replace column, extract any letters/numbers followed by the first comma and replace the comma with a space
         split_name2 = str_replace(str_extract(name_replace, "(?<=,)[:graph:]+"), ",", " "), 

         # from the split_name2 column, extract any letters preceded by the first space
         split_name3 = str_extract(split_name2, "[:alpha:]+(?=[:space:])"),
         
         # from the split_name2 column, extract any letter/number followed by the first space
         split_name4 = str_extract(split_name2, "(?<=[:space:])[:graph:]+"),
         
         # from the name_replace column, extract letters/numbers within parentheses
         split_name5 = str_extract(name_replace, "(?<=\\()[:graph:]+(?=\\))"),
         
         # create an FMP_NAME column, where values are created where:
         FMP_NAME = case_when(
          # the split_name5 column contains "DAB", input the combination of split_name4 and split_name3 
          split_name5 == "DAB" ~ str_c(split_name4, split_name3, sep = " "), 

          # the split_name5 column contains "WINDOWPANE", input the combination of split_name5 and split_name1 
          split_name5 == "WINDOWPANE" ~ str_c(split_name5, split_name1, sep = " "),

          # the split_name5 column contains "RED", "WHITE", or "RED/WHITE", input the combination of split_name4 and split_name1 
          split_name4 %in% c("RED", "WHITE", "RED/WHITE") ~  str_c(split_name4, split_name1, sep = " "), 
          
          # the split_name1 column contains "HERRING", input the combination of split_name3 and split_name1 
          split_name1 == "HERRING" ~ str_c(split_name3, split_name1, sep = " "),

          # the ITIS_NAME column contains "GOOSEFISH", input "MONKFISH" 
          ITIS_NAME == "GOOSEFISH" ~ "MONKFISH",
          
          # the split_name1 column contains "NA", carry over the value from the ITIS_NAME column 
          is.na(split_name1) ~ ITIS_NAME, 

          # all other cases, should input the combination of split_name2 and split_name1
          TRUE ~ str_c(split_name2, split_name1, sep = " ")
         ),

         # change the values in the new FMP_NAME column to sentence case
         FMP_NAME = str_to_sentence(FMP_NAME)
        ) |> 
  
  # clean up the table by removing all intermediary columns
  select(ITIS_TSN, ITIS_NAME, FMP_NAME) 

# join the new FMP_NAME column from the species_lookup table by joining to the original nefmc_species table 
nefmc_species <- nefmc_species |> 
  left_join(species_lookup, by = c("ITIS_TSN", "ITIS_NAME"))

# save an updated copy into the package
usethis::use_data(nefmc_species, overwrite = TRUE)
