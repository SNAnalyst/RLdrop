# metadata.R
# Functies voor het opvragen van metadata van bestanden en mappen op Dropbox.
# Gebruikt het /2/files/get_metadata endpoint voor single-item lookups.


#' Controleer of een bestand of map bestaat op Dropbox
#'
#' @description
#' Controleert of een bestand of map bestaat op het opgegeven Dropbox-pad.
#' Retourneert `TRUE` of `FALSE` zonder een fout te gooien als het pad
#' niet bestaat.
#'
#' @param dropbox_path Pad op Dropbox, bijv. `"/data/bestand.parquet"` of
#'   `"/data"`. Moet beginnen met `/`.
#' @param token Dropbox API access token als character string. Standaard
#'   wordt automatisch een token opgehaald via [RLdrop::dropbox_token()].
#'
#' @return `TRUE` als het pad bestaat, `FALSE` als het niet bestaat.
#'
#' @details
#' Gebruikt het `/2/files/get_metadata` endpoint. Bij een `not_found` fout
#' retourneert de functie `FALSE` in plaats van een fout te gooien. Andere
#' API-fouten (bijv. ongeldige token) gooien wel een fout.
#'
#' @section Authenticatie:
#' Het token wordt standaard automatisch opgehaald via [RLdrop::dropbox_token()].
#' Zie [RLdrop::dropbox_token()] voor het instellen van de benodigde omgevingsvariabelen.
#'
#' @seealso
#' [RLdrop::dropbox_file_info()] voor volledige metadata van een bestand.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Controleer of een bestand bestaat voor je het downloadt
#' if (dropbox_exists("/data/bestand.parquet")) {
#'   dropbox_download_file("/data/bestand.parquet", "lokaal.parquet")
#' }
#'
#' # Controleer of een map bestaat
#' dropbox_exists("/projecten/2026")
#' }
dropbox_exists <- function(dropbox_path, token = dropbox_token()) {

  response <- httr::POST(
    "https://api.dropboxapi.com/2/files/get_metadata",
    httr::add_headers(
      Authorization  = paste("Bearer", token),
      `Content-Type` = "application/json"
    ),
    body = jsonlite::toJSON(
      list(path = dropbox_path),
      auto_unbox = TRUE
    )
  )

  if (!httr::http_error(response)) {
    return(TRUE)
  }

  # [ARCH] Bij 409 + path/not_found is het pad gewoon niet gevonden — geen fout
  # Bij andere fouten (auth, server, etc.) wel een fout gooien
  error_body <- httr::content(response, as = "text", encoding = "UTF-8")

  if (httr::status_code(response) == 409 && grepl("not_found", error_body)) {
    return(FALSE)
  }

  stop(sprintf("[DROPBOX] Metadata ophalen mislukt voor '%s': %s",
    dropbox_path, error_body))
}


#' Haal metadata op van een bestand of map op Dropbox
#'
#' @description
#' Vraagt de metadata op van een enkel bestand of map op Dropbox, zonder de
#' inhoud te downloaden. Retourneert een lijst met naam, pad, type, grootte
#' en wijzigingsdatum.
#'
#' @param dropbox_path Pad op Dropbox, bijv. `"/data/bestand.parquet"` of
#'   `"/data"`. Moet beginnen met `/`.
#' @param token Dropbox API access token als character string. Standaard
#'   wordt automatisch een token opgehaald via [RLdrop::dropbox_token()].
#'
#' @return Een lijst met de volgende elementen:
#' \describe{
#'   \item{`naam`}{Bestandsnaam of mapnaam.}
#'   \item{`pad`}{Volledig pad op Dropbox (`path_display`).}
#'   \item{`type`}{`"file"` of `"folder"`.}
#'   \item{`grootte`}{Bestandsgrootte in bytes. `NA` voor mappen.}
#'   \item{`gewijzigd`}{Wijzigingsdatum (`client_modified`) als POSIXct.
#'     `NA` voor mappen.}
#'   \item{`revisie`}{Dropbox revisie-ID als character string. `NA` voor mappen.
#'     Bruikbaar voor `mode = "update"` bij uploads.}
#' }
#'
#' @details
#' Gebruikt het `/2/files/get_metadata` endpoint. Gooit een fout als het pad
#' niet bestaat. Gebruik [RLdrop::dropbox_exists()] om eerst te controleren of het
#' pad bestaat.
#'
#' @section Authenticatie:
#' Het token wordt standaard automatisch opgehaald via [RLdrop::dropbox_token()].
#' Zie [RLdrop::dropbox_token()] voor het instellen van de benodigde omgevingsvariabelen.
#'
#' @seealso
#' [RLdrop::dropbox_exists()] om te controleren of een pad bestaat. \cr
#' [RLdrop::dropbox_list_folder()] voor metadata van alle items in een map.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Metadata van een bestand
#' info <- dropbox_file_info("/data/bestand.parquet")
#' sprintf("Grootte: %.1f MB", info$grootte / 1024 / 1024)
#'
#' # Metadata van een map
#' info <- dropbox_file_info("/projecten/2026")
#' info$type  # "folder"
#' }
dropbox_file_info <- function(dropbox_path, token = dropbox_token()) {

  response <- httr::POST(
    "https://api.dropboxapi.com/2/files/get_metadata",
    httr::add_headers(
      Authorization  = paste("Bearer", token),
      `Content-Type` = "application/json"
    ),
    body = jsonlite::toJSON(
      list(path = dropbox_path),
      auto_unbox = TRUE
    )
  )

  if (httr::http_error(response)) {
    stop(sprintf(
      "[DROPBOX] Metadata ophalen mislukt voor '%s': %s",
      dropbox_path,
      httr::content(response, as = "text", encoding = "UTF-8")
    ))
  }

  meta <- httr::content(response)

  result <- list(
    naam      = meta$name,
    pad       = meta$path_display,
    type      = meta[[".tag"]]
  )

  if (meta[[".tag"]] == "file") {
    result$grootte   <- meta$size
    result$gewijzigd <- as.POSIXct(meta$client_modified,
                                    format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
    result$revisie   <- meta$rev
  } else {
    result$grootte   <- NA_real_
    result$gewijzigd <- as.POSIXct(NA)
    result$revisie   <- NA_character_
  }

  result
}
