## code to prepare `nefmc_species` dataset goes here

library(DBI)
library(ROracle)

#### GARFO Connection ####
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

# save an updated copy into the package
usethis::use_data(nefmc_species, overwrite = TRUE)
