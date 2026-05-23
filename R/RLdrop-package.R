# RLdrop-package.R
# Package-level documentatie en globale imports.
# Dit bestand definieert de package-documentatie die verschijnt bij ?RLdrop
# en declareert de httr/jsonlite imports voor het hele pakket.

#' RLdrop: Direct Dropbox API Access
#'
#' Biedt functies om bestanden en mappen rechtstreeks te beheren via de
#' Dropbox API v2, onafhankelijk van de Dropbox desktop client en diens
#' sync-wachtrij. Authenticatie verloopt via OAuth2 refresh tokens voor
#' onbeheerd gebruik.
#'
#' @section Authenticatie:
#' Alle functies accepteren een `token` parameter die standaard automatisch
#' wordt opgehaald via [dropbox_token()]. Stel de volgende omgevingsvariabelen
#' in via `usethis::edit_r_environ()`:
#' \itemize{
#'   \item `DROPBOX_APP_KEY`
#'   \item `DROPBOX_APP_SECRET`
#'   \item `DROPBOX_REFRESH_TOKEN`
#' }
#' Zie [dropbox_token()] voor details.
#'
#' @section Downloaden:
#' \itemize{
#'   \item [dropbox_download_file()] — download een enkel bestand
#'   \item [dropbox_download_folder()] — download een volledige map (per bestand)
#'   \item [dropbox_download_zip()] — download een map als zip
#'   \item [dropbox_download_folder_zip()] — download grote map per submap als zip
#' }
#'
#' @section Uploaden:
#' \itemize{
#'   \item [dropbox_upload()] — upload een bestand (automatische keuze gewone/chunked)
#'   \item [dropbox_upload_file()] — upload een klein bestand (<150MB)
#'   \item [dropbox_upload_large_file()] — upload een groot bestand via chunked session
#'   \item [dropbox_upload_folder()] — upload een volledige map
#' }
#'
#' @section Bestandsbeheer:
#' \itemize{
#'   \item [dropbox_list_folder()] — mapinhoud opvragen als data.frame
#'   \item [dropbox_exists()] — controleer of een pad bestaat
#'   \item [dropbox_file_info()] — metadata van een enkel bestand of map
#'   \item [dropbox_search()] — zoeken op bestandsnaam
#'   \item [dropbox_delete()] — bestand of map verwijderen
#'   \item [dropbox_move()] — verplaatsen of hernoemen
#'   \item [dropbox_create_folder()] — map aanmaken
#' }
#'
#' @section Vergelijken:
#' \itemize{
#'   \item [dropbox_compare_folder()] — lokale map vergelijken met Dropbox-map
#' }
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom httr POST add_headers content http_error upload_file
#' @importFrom jsonlite toJSON
## usethis namespace: end
NULL
