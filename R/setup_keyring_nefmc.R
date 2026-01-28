#' @title setup_keyring_nefmc
#'
#' @description
#' Keyring stores passwords in encrypted file with operating system credentials manager
#'
#' @author Laura Smith and Daniel Hocking
#' @param delete_keyring Logical whether to delete the keyring and start from scratch
#'
#' @return
#'
#' @details
#' NOTE: The keyring name and service names must be identical for others to use your code. Run the code below and enter the usernames and passwords once to store them on that computer/server. The passwords for any service can be updated simply by running that line again and entering the new password. Anyone that uses the code below will be able to run each others code without having to alter the login info. Read about the keyring package here: https://keyring.r-lib.org/index.html
#'
#' @export
#'
#' @examples
#' #' \dontrun{
#' setup_keyring_nefmc()
#' }
setup_keyring_nefmc <- function(delete_keyring = FALSE) {

  if(delete_keyring) {
    if(("nefmc" %in% keyring::keyring_list()$keyring)) {
      keyring::keyring_delete("nefmc")
    }
  }

  # create a keyring named "nefmc"
  if(!("nefmc" %in% keyring::keyring_list()$keyring)) {

    message("You will be prompted to create a password to lock/unlock your keyring.")

    keyring::keyring_create(keyring = "nefmc")
  }

  # unlock keyring - prompts for password to that keyring
  keyring::keyring_unlock("nefmc") # only need to do this once for R Session


  #├ Setup for using TNS names
  if(Sys.getenv("TNS_ADMIN") == "") {
    tns_path = dplyr::case_when(
      .Platform$OS.type == "windows" ~ file.path("U:/NET80/ADMIN"),
      TRUE ~ file.path("/usr/lib/oracle/23/client64/lib/network/admin")
    )
    Sys.setenv(TNS_ADMIN = tns_path)

  }

  message("You will be asked for passwords and usernames for all databases. If you don't use a particular database you can just put an X or other character in each field (not allowed to leave them blank)")

  #------ Database logins ------

  #├ DB01P -----
  keyring::key_set_with_value(key = "nefmc",
                              service = "DB01P",
                              password = .rs.askForPassword("Your Password for GARFO prod DB (DB01P): "),
                              username = .rs.askForPassword("Username: "))


  #├ NEFSC Database -----
  keyring::key_set_with_value(key = "nefmc",
                              service = "NEFSC",
                              password = .rs.askForPassword("Your Password for NEFSC database: "),
                              username = .rs.askForPassword("Username: "))


  #├ Git PAT -----
  keyring::key_set_with_value(key = "nefmc",
                              service = "github",
                              password = .rs.askForPassword("Enter GitHub PAT: "),
                              username = .rs.askForPassword("Github Username: "))


  # Add any other useful username/PW

  return(TRUE)
}


