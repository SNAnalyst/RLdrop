# download.R
# Functions for downloading files and folders directly from Dropbox API v2.
# Bypasses the Dropbox desktop client sync queue for on-demand access.


#' Download een bestand van Dropbox direct via API
#'
#' @description
#' Downloadt een bestand rechtstreeks via de Dropbox API v2, onafhankelijk van
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
#'
#' @seealso
#' [dropbox_download_folder()] om een volledige map te downloaden.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' token <- dropbox_token()
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
  invisible(local_path)
}


# [ARCH] Interne helper: haal alle entries op uit een Dropbox-map via
# list_folder en paginering. Geeft een lijst van entries terug zonder te downloaden.
.list_folder_recursive <- function(dropbox_folder, token, recursive) {

  message(sprintf("[DROPBOX] Mapinhoud ophalen: %s ...", dropbox_folder))

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

  result  <- httr::content(list_response)
  entries <- result$entries

  # [ARCH] Dropbox pagineert bij >2000 items; volg cursor totdat has_more FALSE is
  while (isTRUE(result$has_more)) {
    message(sprintf("[DROPBOX] Meer items ophalen (%d tot nu toe)...", length(entries)))
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

    result  <- httr::content(continue_response)
    entries <- c(entries, result$entries)
  }

  entries
}


#' Download een volledige map van Dropbox direct via API
#'
#' @description
#' Downloadt alle bestanden in een Dropbox-map naar een lokale map. Optioneel
#' recursief voor submappen. Toont een voortgangsbalk tijdens het downloaden.
#' Mappen met meer dan 2000 bestanden worden automatisch gepagineerd afgehandeld.
#'
#' @param dropbox_folder Pad naar de map op Dropbox, bijv. `"/data/parquet"`.
#'   Moet beginnen met `/`. Gebruik `""` voor de root van je Dropbox.
#' @param local_folder Lokaal pad van de doelmap. Wordt aangemaakt als deze
#'   nog niet bestaat, inclusief bovenliggende mappen.
#' @param token Dropbox API access token als character string.
#' @param recursive Logisch. Indien `TRUE`, worden submappen ook gedownload
#'   en wordt de mapstructuur lokaal gerepliceerd. Standaard `FALSE`.
#'
#' @return Invisibly een character vector van lokale paden van alle gedownloade
#'   bestanden.
#'
#' @details
#' De functie werkt in twee fasen:
#' \enumerate{
#'   \item Alle bestandsitems worden eerst volledig geinventariseerd via de
#'     Dropbox `list_folder` API (inclusief paginering bij >2000 items).
#'     Dit geeft het totaal aantal bestanden zodat de voortgangsbalk correct
#'     kan worden weergegeven.
#'   \item De bestanden worden gedownload via [dropbox_download_file()] met
#'     een voortgangsbalk via [utils::txtProgressBar()].
#' }
#'
#' Bij `recursive = TRUE` wordt de volledige mapboomstructuur gerepliceerd in
#' `local_folder`. Paden worden berekend via `path_lower` (de door Dropbox
#' genormaliseerde lowercase variant) voor consistente vergelijking, en
#' submappen worden aangemaakt voor het downloaden begint.
#'
#' @section Foutafhandeling:
#' Als het ophalen van de mapinhoud mislukt (bijv. map bestaat niet of geen
#' leesrechten), gooit de functie een fout met de API-foutmelding. Individuele
#' bestandsfouten worden doorgegeven vanuit [dropbox_download_file()].
#'
#' @section Authenticatie:
#' Vereist de scopes `files.content.read` en `files.metadata.read` in je
#' Dropbox app permissions.
#'
#' @seealso
#' [dropbox_download_file()] voor het downloaden van een enkel bestand.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' token <- dropbox_token()
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
#' }
dropbox_download_folder <- function(dropbox_folder, local_folder, token, recursive = FALSE) {

  # --- Fase 1: inventariseer alle items ---
  entries <- .list_folder_recursive(dropbox_folder, token, recursive)

  # [ARCH] Splits entries in bestanden en mappen voor overzichtelijke verwerking
  file_entries   <- Filter(function(e) e[[".tag"]] == "file",   entries)
  folder_entries <- Filter(function(e) e[[".tag"]] == "folder", entries)

  n_files <- length(file_entries)

  if (n_files == 0) {
    message(sprintf("[DROPBOX] Geen bestanden gevonden in %s", dropbox_folder))
    return(invisible(character(0)))
  }

  message(sprintf("[DROPBOX] %d bestand(en) gevonden, downloaden gestart...", n_files))

  # [ARCH] Gebruik path_lower van de rootmap als prefix voor consistente vergelijking.
  # Trailing slash verwijderen zodat substring() correct werkt voor alle subpaden.
  root_prefix_lower <- tolower(sub("/$", "", dropbox_folder))

  # Maak lokale hoofdmap aan
  if (!dir.exists(local_folder)) dir.create(local_folder, recursive = TRUE)

  # Maak alle submappen aan voor het downloaden begint
  if (recursive && length(folder_entries) > 0) {
    for (folder in folder_entries) {
      # [ARCH] path_lower geeft genormaliseerd pad; strip root prefix voor relatief pad
      rel_path  <- substring(folder$path_lower, nchar(root_prefix_lower) + 1)
      sub_local <- paste0(local_folder, gsub("/", .Platform$file.sep, rel_path))
      if (!dir.exists(sub_local)) dir.create(sub_local, recursive = TRUE)
    }
  }

  # --- Fase 2: download met voortgangsbalk ---
  pb         <- utils::txtProgressBar(min = 0, max = n_files, style = 3)
  downloaded <- character(n_files)

  for (i in seq_along(file_entries)) {
    entry <- file_entries[[i]]

    if (recursive) {
      # [ARCH] path_lower voor consistent relatief pad; path_display voor API-aanroep
      rel_path   <- substring(entry$path_lower, nchar(root_prefix_lower) + 1)
      local_path <- paste0(local_folder, gsub("/", .Platform$file.sep, rel_path))
    } else {
      local_path <- file.path(local_folder, basename(entry$path_lower))
    }

    dropbox_download_file(entry$path_display, local_path, token)
    downloaded[[i]] <- local_path
    utils::setTxtProgressBar(pb, i)
  }

  close(pb)
  message(sprintf("\n[DROPBOX] %d bestand(en) gedownload naar %s", n_files, local_folder))
  invisible(downloaded)
}
