# download.R
# Functions for downloading files and folders directly from Dropbox API v2.
# Bypasses the Dropbox desktop client sync queue for on-demand access.


#' Download een bestand van Dropbox direct via API
#'
#' @description
#' Downloadt één bestand rechtstreeks via de Dropbox API v2, onafhankelijk van
#' de Dropbox desktop client en diens sync-wachtrij. Nuttig wanneer een bestand
#' direct beschikbaar moet zijn zonder te wachten op synchronisatie.
#'
#' @param dropbox_path Pad op Dropbox, bijv. `"/data/bestand.parquet"`. Moet
#'   beginnen met `/`. Gebruik exact het pad zoals het op Dropbox staat;
#'   hoofdlettergevoeligheid hangt af van de Dropbox-server.
#' @param local_path Volledig lokaal pad inclusief bestandsnaam waar het bestand
#'   opgeslagen wordt, bijv. `"D:/lokaal/bestand.parquet"`. De bovenliggende
#'   map moet al bestaan.
#' @param token Dropbox API access token als character string. Genereer via
#'   \url{https://www.dropbox.com/developers/apps} onder het tabblad
#'   `Settings` van je app.
#'
#' @return Invisibly het lokale pad (`local_path`) als character string.
#'   Wordt aangeroepen voor zijn neveneffect (bestand schrijven naar schijf).
#'
#' @details
#' Gebruikt het `/2/files/download` endpoint van de Dropbox Content API.
#' Het bestand wordt binair geschreven via [writeBin()] zodat zowel tekst-
#' als binaire bestanden (parquet, rds, xlsx, etc.) correct worden opgeslagen.
#'
#' Bij een HTTP-fout (bijv. bestand niet gevonden, ongeldige token) gooit de
#' functie een fout met de volledige API-foutmelding als bericht.
#'
#' @section Authenticatie:
#' Maak een Dropbox app aan op \url{https://www.dropbox.com/developers/apps}
#' met `Full Dropbox` scope. Vink onder `Permissions` de scope
#' `files.content.read` aan. Genereer daarna een token onder `Settings`.
#' Sla het token op als omgevingsvariabele via `Sys.setenv(DROPBOX_TOKEN = "...")`.
#'
#' @seealso
#' [dropbox_download_folder()] om een volledige map te downloaden.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' token <- Sys.getenv("DROPBOX_TOKEN")  # token bij voorkeur uit omgevingsvariabele
#'
#' # Enkelvoudig bestand downloaden
#' dropbox_download_file(
#'   dropbox_path = "/data/bestand.parquet",
#'   local_path   = "D:/lokaal/bestand.parquet",
#'   token        = token
#' )
#'
#' # Resultaat opvangen en direct inlezen
#' pad    <- dropbox_download_file("/config/settings.rds", tempfile(), token)
#' config <- readRDS(pad)
#' }
dropbox_download_file <- function(dropbox_path, local_path, token) {

  response <- httr::POST(
    "https://content.dropboxapi.com/2/files/download",
    httr::add_headers(
      Authorization     = paste("Bearer", token),
      `Dropbox-API-Arg` = jsonlite::toJSON(
        list(path = dropbox_path),
        auto_unbox = TRUE
      )
    )
  )

  if (httr::http_error(response)) {
    stop(sprintf(
      "[DROPBOX] Download mislukt voor '%s': %s",
      dropbox_path,
      httr::content(response, as = "text", encoding = "UTF-8")
    ))
  }

  # [ARCH] Schrijf binair zodat ook niet-tekstbestanden correct worden opgeslagen
  writeBin(httr::content(response, as = "raw"), local_path)
  message(sprintf("[DROPBOX] Gedownload: %s -> %s", dropbox_path, local_path))
  invisible(local_path)
}


#' Download een volledige map van Dropbox direct via API
#'
#' @description
#' Downloadt alle bestanden in een Dropbox-map naar een lokale map. Optioneel
#' recursief voor submappen. Mappen met meer dan 2000 bestanden worden
#' automatisch gepagineerd afgehandeld.
#'
#' @param dropbox_folder Pad naar de map op Dropbox, bijv. `"/data/parquet"`.
#'   Moet beginnen met `/`. Gebruik `""` voor de root van je Dropbox.
#' @param local_folder Lokaal pad van de doelmap. Wordt aangemaakt als deze
#'   nog niet bestaat, inclusief bovenliggende mappen.
#' @param token Dropbox API access token als character string. Genereer via
#'   \url{https://www.dropbox.com/developers/apps} onder het tabblad
#'   `Settings` van je app.
#' @param recursive Logisch. Indien `TRUE`, worden submappen ook gedownload
#'   en wordt de mapstructuur lokaal gerepliceerd. Standaard `FALSE`.
#'
#' @return Invisibly een character vector van lokale paden van alle gedownloade
#'   bestanden.
#'
#' @details
#' Gebruikt het `/2/files/list_folder` endpoint om de mapinhoud op te halen,
#' en roept vervolgens [dropbox_download_file()] aan per bestand.
#'
#' Dropbox pagineert de mapinhoud bij meer dan 2000 items. Deze functie volgt
#' automatisch de paginering via `/2/files/list_folder/continue` totdat alle
#' items zijn verwerkt.
#'
#' Bij `recursive = TRUE` wordt de volledige mapboomstructuur gerepliceerd in
#' `local_folder`. Submappen worden aangemaakt als ze nog niet bestaan. De
#' recursie verloopt via herhaalde aanroepen van `dropbox_download_folder()`
#' zelf per submap.
#'
#' @section Foutafhandeling:
#' Als het ophalen van de mapinhoud mislukt (bijv. map bestaat niet of geen
#' leesrechten), gooit de functie een fout met de API-foutmelding. Individuele
#' bestandsfouten worden doorgegeven vanuit [dropbox_download_file()].
#'
#' @section Authenticatie:
#' Vereist de scope `files.content.read` in je Dropbox app permissions.
#' Zie [dropbox_download_file()] voor instructies over het aanmaken van een token.
#'
#' @seealso
#' [dropbox_download_file()] voor het downloaden van een enkel bestand.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' token <- Sys.getenv("DROPBOX_TOKEN")
#'
#' # Platte map downloaden (geen submappen)
#' dropbox_download_folder(
#'   dropbox_folder = "/data/parquet",
#'   local_folder   = "D:/lokaal/parquet",
#'   token          = token
#' )
#'
#' # Inclusief alle submappen
#' dropbox_download_folder(
#'   dropbox_folder = "/projecten/2026",
#'   local_folder   = "D:/lokaal/projecten/2026",
#'   token          = token,
#'   recursive      = TRUE
#' )
#'
#' # Gedownloade paden opvangen
#' paden <- dropbox_download_folder("/data", "D:/lokaal/data", token, recursive = TRUE)
#' message(sprintf("%d bestanden gedownload", length(paden)))
#' }
dropbox_download_folder <- function(dropbox_folder, local_folder, token, recursive = FALSE) {

  # [ARCH] Eerst mapinhoud ophalen via list_folder
  list_response <- httr::POST(
    "https://api.dropboxapi.com/2/files/list_folder",
    httr::add_headers(
      Authorization  = paste("Bearer", token),
      `Content-Type` = "application/json"
    ),
    body = jsonlite::toJSON(
      list(path = dropbox_folder, recursive = recursive),
      auto_unbox = TRUE
    )
  )

  if (httr::http_error(list_response)) {
    stop(sprintf(
      "[DROPBOX] Mapinhoud ophalen mislukt voor '%s': %s",
      dropbox_folder,
      httr::content(list_response, as = "text", encoding = "UTF-8")
    ))
  }

  if (!dir.exists(local_folder)) dir.create(local_folder, recursive = TRUE)

  downloaded <- character(0)

  # [ARCH] Interne helper om entries te verwerken; hergebruikt in paginatielus
  .process_entries <- function(entries) {
    for (entry in entries) {
      if (entry[[".tag"]] == "file") {
        local_path <- file.path(local_folder, basename(entry$path_lower))
        dropbox_download_file(entry$path_display, local_path, token)
        downloaded <<- c(downloaded, local_path)
      } else if (entry[[".tag"]] == "folder" && recursive) {
        sub_local  <- file.path(local_folder, basename(entry$path_lower))
        sub_dl     <- dropbox_download_folder(entry$path_display, sub_local, token, recursive = TRUE)
        downloaded <<- c(downloaded, sub_dl)
      }
    }
  }

  result <- httr::content(list_response)
  .process_entries(result$entries)

  # [ARCH] Dropbox pagineert resultaten bij >2000 bestanden
  while (isTRUE(result$has_more)) {
    continue_response <- httr::POST(
      "https://api.dropboxapi.com/2/files/list_folder/continue",
      httr::add_headers(
        Authorization  = paste("Bearer", token),
        `Content-Type` = "application/json"
      ),
      body = jsonlite::toJSON(
        list(cursor = result$cursor),
        auto_unbox = TRUE
      )
    )

    if (httr::http_error(continue_response)) {
      stop(sprintf(
        "[DROPBOX] Paginering mislukt: %s",
        httr::content(continue_response, as = "text", encoding = "UTF-8")
      ))
    }

    result <- httr::content(continue_response)
    .process_entries(result$entries)
  }

  message(sprintf("[DROPBOX] %d bestanden gedownload naar %s", length(downloaded), local_folder))
  invisible(downloaded)
}
