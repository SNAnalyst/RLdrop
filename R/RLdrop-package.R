# dropboxr-package.R
# Package-level documentation and global imports.

#' RLdrop: Direct Dropbox API Access
#'
#' Provides functions to upload and download files and folders directly via
#' the Dropbox API v2, bypassing the Dropbox desktop client sync queue.
#'
#' @section Main functions:
#' - [dropbox_download_file()] — download een enkel bestand
#' - [dropbox_download_folder()] — download een volledige map
#' - [dropbox_upload()] — upload een bestand (automatische keuze gewone/chunked)
#' - [dropbox_upload_file()] — upload een klein bestand (<150MB)
#' - [dropbox_upload_large_file()] — upload een groot bestand via chunked session
#' - [dropbox_upload_folder()] — upload een volledige map
#'
#' @section Authenticatie:
#' Genereer een access token via \url{https://www.dropbox.com/developers/apps}.
#' Maak een app aan met `Full Dropbox` scope en genereer een token onder
#' het tabblad `Settings`.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom httr POST add_headers content http_error upload_file
#' @importFrom jsonlite toJSON
## usethis namespace: end
NULL
