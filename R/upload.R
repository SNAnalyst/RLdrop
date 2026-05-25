# upload.R
# Functions for uploading files and folders directly to Dropbox API v2.
# Bypasses the Dropbox desktop client sync queue for on-demand access.
# Files >= large_file_threshold are uploaded via chunked upload session.


#' Upload een bestand naar Dropbox direct via API
#'
#' @description
#' Uploadt één bestand rechtstreeks via de Dropbox API v2. Geschikt voor
#' bestanden kleiner dan 150MB. Voor grotere bestanden, gebruik
#' [RLdrop::dropbox_upload_large_file()] of de automatische wrapper [RLdrop::dropbox_upload()].
#'
#' @param local_path Volledig lokaal pad van het te uploaden bestand,
#'   bijv. `"D:/data/bestand.parquet"`. Gooit een fout als het bestand
#'   niet bestaat.
#' @param dropbox_path Doelpad op Dropbox inclusief bestandsnaam,
#'   bijv. `"/data/bestand.parquet"`. Moet beginnen met `/`.
#' @param token Dropbox API access token als character string. Standaard
#'   wordt automatisch een token opgehaald via [RLdrop::dropbox_token()].
#' @param mode Schrijfmodus als character string. Opties:
#'   \describe{
#'     \item{`"overwrite"`}{Overschrijft een bestaand bestand (standaard).}
#'     \item{`"add"`}{Voegt toe; hernoemt automatisch bij naamconflict.}
#'     \item{`"update"`}{Vereist een revisie-ID; voor conflictvrije updates.}
#'   }
#'
#' @return Invisibly de Dropbox bestandsmetadata als lijst, met velden zoals
#'   `name`, `path_display`, `size`, en `server_modified`.
#'
#' @details
#' Gebruikt het `/2/files/upload` endpoint van de Dropbox Content API.
#' De Dropbox API heeft een maximale bestandsgrootte van 150MB voor dit
#' endpoint. Gebruik [RLdrop::dropbox_upload_large_file()] voor grotere bestanden,
#' of [RLdrop::dropbox_upload()] als automatische wrapper die de keuze maakt op
#' basis van bestandsgrootte.
#'
#' @section Authenticatie:
#' Het token wordt standaard automatisch opgehaald via [RLdrop::dropbox_token()],
#' die de credentials leest uit omgevingsvariabelen. Zie [RLdrop::dropbox_token()]
#' voor het instellen van de benodigde omgevingsvariabelen.
#'
#' @seealso
#' [RLdrop::dropbox_upload()] voor automatische keuze tussen gewone en chunked upload. \cr
#' [RLdrop::dropbox_upload_large_file()] voor bestanden groter dan 150MB. \cr
#' [RLdrop::dropbox_upload_folder()] voor het uploaden van een volledige map.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Bestand uploaden met overschrijven (standaard)
#' # Token wordt automatisch opgehaald via dropbox_token()
#' dropbox_upload_file(
#'   local_path   = "D:/data/bestand.parquet",
#'   dropbox_path = "/data/bestand.parquet"
#' )
#'
#' # Metadata opvangen
#' meta <- dropbox_upload_file("D:/output/result.rds", "/output/result.rds")
#' message("Opgeslagen als: ", meta$path_display)
#' }
dropbox_upload_file <- function(local_path, dropbox_path, token = dropbox_token(), mode = "overwrite") {

  local_long <- .win_long_path(local_path)

  if (!file.exists(local_long)) {
    stop(sprintf("[DROPBOX] Lokaal bestand niet gevonden: %s", local_path))
  }

  response <- httr::POST(
    "https://content.dropboxapi.com/2/files/upload",
    httr::add_headers(
      Authorization     = paste("Bearer", token),
      `Content-Type`    = "application/octet-stream",
      `Dropbox-API-Arg` = jsonlite::toJSON(
        list(
          path       = dropbox_path,
          mode       = mode,
          autorename = FALSE,
          mute       = FALSE
        ),
        auto_unbox = TRUE
      )
    ),
    body = httr::upload_file(local_long)
  )

  if (httr::http_error(response)) {
    stop(sprintf(
      "[DROPBOX] Upload mislukt voor '%s': %s",
      local_path,
      httr::content(response, as = "text", encoding = "UTF-8")
    ))
  }

  metadata <- httr::content(response)
  message(sprintf("[DROPBOX] Geupload: %s -> %s (%s bytes)",
    local_path, dropbox_path, metadata$size))
  invisible(metadata)
}


#' Upload een groot bestand naar Dropbox via chunked upload session
#'
#' @description
#' Uploadt bestanden groter dan 150MB naar Dropbox via de upload session API,
#' waarbij het bestand in chunks wordt verstuurd. Toont voortgang per chunk.
#' Voor kleinere bestanden is [RLdrop::dropbox_upload_file()] efficiënter.
#'
#' @param local_path Volledig lokaal pad van het te uploaden bestand.
#'   Gooit een fout als het bestand niet bestaat.
#' @param dropbox_path Doelpad op Dropbox inclusief bestandsnaam,
#'   bijv. `"/data/groot_bestand.csv"`. Moet beginnen met `/`.
#' @param token Dropbox API access token als character string. Standaard
#'   wordt automatisch een token opgehaald via [RLdrop::dropbox_token()].
#' @param chunk_size Grootte van elk upload-chunk in bytes. Standaard 128MB
#'   (`128 * 1024 * 1024`). Grotere chunks zijn sneller maar kwetsbaarder
#'   voor netwerkfouten; kleinere chunks zijn robuuster maar langzamer.
#' @param mode Schrijfmodus als character string. Opties:
#'   \describe{
#'     \item{`"overwrite"`}{Overschrijft een bestaand bestand (standaard).}
#'     \item{`"add"`}{Voegt toe; hernoemt automatisch bij naamconflict.}
#'     \item{`"update"`}{Vereist een revisie-ID; voor conflictvrije updates.}
#'   }
#'
#' @return Invisibly de Dropbox bestandsmetadata als lijst, met velden zoals
#'   `name`, `path_display`, `size`, en `server_modified`.
#'
#' @details
#' De upload verloopt in drie fasen via de Dropbox upload session API:
#'
#' \enumerate{
#'   \item **Start** (`/2/files/upload_session/start`): eerste chunk wordt
#'     verstuurd en een `session_id` wordt ontvangen.
#'   \item **Append** (`/2/files/upload_session/append_v2`): tussenliggende
#'     chunks worden verstuurd met de `session_id` en huidige byte-offset.
#'   \item **Finish** (`/2/files/upload_session/finish`): laatste chunk wordt
#'     gecombineerd met de commit-metadata (doelpad, schrijfmodus).
#' }
#'
#' De verbinding met het bestand wordt via [on.exit()] gegarandeerd gesloten,
#' ook bij een fout halverwege de upload.
#'
#' @section Foutafhandeling:
#' Bij een mislukte API-aanroep in een van de drie fasen gooit de functie
#' een fout met de fase (start/append/finish) en de API-foutmelding. Een
#' mislukte upload wordt niet automatisch hervat; start opnieuw bij fout.
#'
#' @section Authenticatie:
#' Het token wordt standaard automatisch opgehaald via [RLdrop::dropbox_token()].
#' Zie [RLdrop::dropbox_token()] voor het instellen van de benodigde omgevingsvariabelen.
#'
#' @seealso
#' [RLdrop::dropbox_upload()] als automatische wrapper die kiest op bestandsgrootte. \cr
#' [RLdrop::dropbox_upload_file()] voor bestanden kleiner dan 150MB. \cr
#' [RLdrop::dropbox_upload_folder()] voor het uploaden van een volledige map.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Groot bestand uploaden met standaard chunk-grootte (128MB)
#' dropbox_upload_large_file(
#'   local_path   = "D:/data/groot_bestand.csv",
#'   dropbox_path = "/data/groot_bestand.csv"
#' )
#'
#' # Kleinere chunks voor instabiele verbinding (bijv. 32MB)
#' dropbox_upload_large_file(
#'   local_path   = "D:/data/groot_bestand.csv",
#'   dropbox_path = "/data/groot_bestand.csv",
#'   chunk_size   = 32 * 1024 * 1024
#' )
#' }
dropbox_upload_large_file <- function(local_path, dropbox_path, token = dropbox_token(),
                                       chunk_size = 128 * 1024 * 1024,
                                       mode = "overwrite") {

  local_long <- .win_long_path(local_path)

  if (!file.exists(local_long)) {
    stop(sprintf("[DROPBOX] Lokaal bestand niet gevonden: %s", local_path))
  }

  file_size <- file.info(local_long)$size
  message(sprintf("[DROPBOX] Start chunked upload: %s (%.1f MB)",
    basename(local_path), file_size / 1024 / 1024))

  con <- file(local_long, "rb")
  on.exit(close(con))  # [ARCH] Altijd sluiten, ook bij fout

  # --- Stap 1: Start upload session ---
  first_chunk <- readBin(con, "raw", n = chunk_size)

  start_response <- httr::POST(
    "https://content.dropboxapi.com/2/files/upload_session/start",
    httr::add_headers(
      Authorization     = paste("Bearer", token),
      `Content-Type`    = "application/octet-stream",
      `Dropbox-API-Arg` = jsonlite::toJSON(list(close = FALSE), auto_unbox = TRUE)
    ),
    body = first_chunk
  )

  if (httr::http_error(start_response)) {
    stop(sprintf("[DROPBOX] Upload session starten mislukt: %s",
      httr::content(start_response, as = "text", encoding = "UTF-8")))
  }

  session_id <- httr::content(start_response)$session_id
  offset     <- length(first_chunk)

  message(sprintf("[DROPBOX] Session gestart (%.1f MB geupload)", offset / 1024 / 1024))

  # --- Stap 2: Tussenliggende en laatste chunks ---
  repeat {
    chunk <- readBin(con, "raw", n = chunk_size)
    if (length(chunk) == 0) break

    bytes_remaining <- file_size - offset - length(chunk)
    is_last_chunk   <- bytes_remaining <= 0

    if (!is_last_chunk) {
      # [ARCH] Append-call voor alle chunks behalve de laatste
      append_response <- httr::POST(
        "https://content.dropboxapi.com/2/files/upload_session/append_v2",
        httr::add_headers(
          Authorization     = paste("Bearer", token),
          `Content-Type`    = "application/octet-stream",
          `Dropbox-API-Arg` = jsonlite::toJSON(
            list(
              cursor = list(session_id = session_id, offset = offset),
              close  = FALSE
            ),
            auto_unbox = TRUE
          )
        ),
        body = chunk
      )

      if (httr::http_error(append_response)) {
        stop(sprintf("[DROPBOX] Chunk upload mislukt op offset %d: %s",
          offset,
          httr::content(append_response, as = "text", encoding = "UTF-8")))
      }

      offset <- offset + length(chunk)
      message(sprintf("[DROPBOX] Voortgang: %.1f / %.1f MB (%.0f%%)",
        offset / 1024 / 1024,
        file_size / 1024 / 1024,
        100 * offset / file_size))

    } else {
      # --- Stap 3: Laatste chunk + finish ---
      # [ARCH] Laatste chunk gecombineerd met finish-call om een extra round-trip te vermijden
      finish_response <- httr::POST(
        "https://content.dropboxapi.com/2/files/upload_session/finish",
        httr::add_headers(
          Authorization     = paste("Bearer", token),
          `Content-Type`    = "application/octet-stream",
          `Dropbox-API-Arg` = jsonlite::toJSON(
            list(
              cursor = list(session_id = session_id, offset = offset),
              commit = list(
                path       = dropbox_path,
                mode       = mode,
                autorename = FALSE,
                mute       = FALSE
              )
            ),
            auto_unbox = TRUE
          )
        ),
        body = chunk
      )

      if (httr::http_error(finish_response)) {
        stop(sprintf("[DROPBOX] Upload afronden mislukt: %s",
          httr::content(finish_response, as = "text", encoding = "UTF-8")))
      }

      metadata <- httr::content(finish_response)
      message(sprintf("[DROPBOX] Voltooid: %s -> %s (%.1f MB)",
        local_path, dropbox_path, file_size / 1024 / 1024))
      return(invisible(metadata))
    }
  }
}


#' Upload een bestand naar Dropbox, kiest automatisch gewone of chunked upload
#'
#' @description
#' Automatische wrapper rond [RLdrop::dropbox_upload_file()] en
#' [RLdrop::dropbox_upload_large_file()]. Bepaalt op basis van de bestandsgrootte
#' welke uploadmethode wordt gebruikt. Dit is de aanbevolen functie voor
#' enkelvoudige uploads.
#'
#' @param local_path Volledig lokaal pad van het te uploaden bestand.
#' @param dropbox_path Doelpad op Dropbox inclusief bestandsnaam.
#'   Moet beginnen met `/`.
#' @param token Dropbox API access token als character string. Standaard
#'   wordt automatisch een token opgehaald via [RLdrop::dropbox_token()].
#' @param mode Schrijfmodus: `"overwrite"` (standaard), `"add"`, of `"update"`.
#' @param large_file_threshold Drempel in bytes waarboven de chunked upload
#'   session wordt gebruikt. Standaard 140MB (`140 * 1024 * 1024`).
#'   Iets onder de API-limiet van 150MB om ruimte te laten voor headers.
#'
#' @return Invisibly de Dropbox bestandsmetadata als lijst.
#'
#' @details
#' De keuze tussen uploadmethoden:
#' \itemize{
#'   \item Bestandsgrootte < `large_file_threshold`: [RLdrop::dropbox_upload_file()]
#'   \item Bestandsgrootte >= `large_file_threshold`: [RLdrop::dropbox_upload_large_file()]
#' }
#'
#' @section Authenticatie:
#' Het token wordt standaard automatisch opgehaald via [RLdrop::dropbox_token()].
#' Zie [RLdrop::dropbox_token()] voor het instellen van de benodigde omgevingsvariabelen.
#'
#' @seealso
#' [RLdrop::dropbox_upload_file()] voor kleine bestanden. \cr
#' [RLdrop::dropbox_upload_large_file()] voor grote bestanden. \cr
#' [RLdrop::dropbox_upload_folder()] voor het uploaden van een volledige map.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Automatische keuze (aanbevolen)
#' dropbox_upload("D:/data/bestand.parquet", "/data/bestand.parquet")
#'
#' # Groot bestand — zelfde aanroep, chunked wordt automatisch gebruikt
#' dropbox_upload("D:/data/groot.csv", "/data/groot.csv")
#'
#' # Lagere drempel instellen (chunked al vanaf 50MB)
#' dropbox_upload(
#'   local_path           = "D:/data/bestand.csv",
#'   dropbox_path         = "/data/bestand.csv",
#'   large_file_threshold = 50 * 1024 * 1024
#' )
#' }
dropbox_upload <- function(local_path, dropbox_path, token = dropbox_token(),
                            mode = "overwrite",
                            large_file_threshold = 140 * 1024 * 1024) {

  file_size <- file.info(.win_long_path(local_path))$size

  if (file_size >= large_file_threshold) {
    dropbox_upload_large_file(local_path, dropbox_path, token, mode = mode)
  } else {
    dropbox_upload_file(local_path, dropbox_path, token, mode = mode)
  }
}


#' Upload een volledige map naar Dropbox direct via API
#'
#' @description
#' Uploadt alle bestanden in een lokale map naar een Dropbox-map. Optioneel
#' recursief voor submappen. Gebruikt [RLdrop::dropbox_upload()] per bestand, zodat
#' grote bestanden automatisch via chunked upload worden verstuurd.
#'
#' Individuele bestanden die niet geupload kunnen worden worden overgeslagen
#' en aan het eind als waarschuwing gemeld, zodat de rest van de upload
#' gewoon doorgaat.
#'
#' @param local_folder Lokale map waarvan de inhoud wordt geupload,
#'   bijv. `"D:/data/parquet"`. Gooit een fout als de map niet bestaat.
#' @param dropbox_folder Doelmap op Dropbox, bijv. `"/data/parquet"`.
#'   Moet beginnen met `/`. De map hoeft niet al te bestaan op Dropbox.
#' @param token Dropbox API access token als character string. Standaard
#'   wordt automatisch een token opgehaald via [RLdrop::dropbox_token()].
#' @param recursive Logisch. Indien `TRUE`, worden bestanden in submappen
#'   ook geupload en wordt de mapstructuur gerepliceerd op Dropbox.
#'   Standaard `FALSE`.
#' @param mode Schrijfmodus: `"overwrite"` (standaard), `"add"`, of `"update"`.
#' @param large_file_threshold Drempel in bytes waarboven chunked upload
#'   wordt gebruikt per bestand. Standaard 140MB. Doorgegeven aan
#'   [RLdrop::dropbox_upload()].
#' @param overwrite Bepaalt welke bestanden geupload worden als ze al op
#'   Dropbox staan:
#'   \describe{
#'     \item{`"always"`}{Upload alles, ongeacht wat er op Dropbox staat.}
#'     \item{`"missing"`}{Sla bestanden over die al op Dropbox staan
#'       (op basis van naam).}
#'     \item{`"changed"`}{Upload alleen ontbrekende bestanden en bestanden
#'       waarvan de grootte (bytes) verschilt.}
#'     \item{`"newer"`}{Upload alleen ontbrekende bestanden en bestanden
#'       waarvan de lokale versie nieuwer is dan de Dropbox-versie
#'       (lokale `mtime` > `client_modified`) (standaard).}
#'   }
#'
#' @return Invisibly een character vector van Dropbox-paden die succesvol
#'   zijn geupload.
#'
#' @details
#' Bij `overwrite` anders dan `"always"` wordt eerst de inhoud van de
#' Dropbox-doelmap opgehaald om te bepalen welke bestanden overgeslagen
#' kunnen worden. Dit kost een extra API-aanroep maar bespaart bandbreedte
#' bij herhaalde uploads.
#'
#' Bij `recursive = TRUE` wordt het relatieve pad ten opzichte van
#' `local_folder` berekend en gerepliceerd onder `dropbox_folder`, zodat
#' de mapstructuur behouden blijft. Op Windows worden lange paden (>260
#' tekens) automatisch afgehandeld.
#'
#' @section Foutafhandeling:
#' Als `local_folder` niet bestaat, gooit de functie direct een fout.
#' Per bestand worden fouten opgevangen via [tryCatch()]; mislukte bestanden
#' worden verzameld en als [warning()] gerapporteerd na afloop.
#'
#' @section Authenticatie:
#' Het token wordt standaard automatisch opgehaald via [RLdrop::dropbox_token()].
#' Zie [RLdrop::dropbox_token()] voor het instellen van de benodigde omgevingsvariabelen.
#'
#' @seealso
#' [RLdrop::dropbox_upload()] voor het uploaden van een enkel bestand. \cr
#' [RLdrop::dropbox_upload_large_file()] voor grote bestanden. \cr
#' [RLdrop::dropbox_download_folder()] voor het downloaden van een map. \cr
#' [RLdrop::dropbox_compare_folder()] om een lokale map te vergelijken met Dropbox.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Platte map uploaden
#' dropbox_upload_folder(
#'   local_folder   = "D:/data/parquet",
#'   dropbox_folder = "/data/parquet"
#' )
#'
#' # Inclusief submappen
#' dropbox_upload_folder(
#'   local_folder   = "D:/data",
#'   dropbox_folder = "/data",
#'   recursive      = TRUE
#' )
#'
#' # Alleen nieuwe of gewijzigde bestanden uploaden
#' dropbox_upload_folder(
#'   local_folder   = "D:/data",
#'   dropbox_folder = "/data",
#'   recursive      = TRUE,
#'   overwrite      = "changed"
#' )
#'
#' # Alleen ontbrekende bestanden uploaden
#' dropbox_upload_folder(
#'   local_folder   = "D:/data",
#'   dropbox_folder = "/data",
#'   recursive      = TRUE,
#'   overwrite      = "missing"
#' )
#' }
dropbox_upload_folder <- function(local_folder, dropbox_folder, token = dropbox_token(),
                                   recursive = FALSE, mode = "overwrite",
                                   large_file_threshold = 140 * 1024 * 1024,
                                   overwrite = "newer") {

  overwrite <- match.arg(overwrite, choices = c("always", "missing", "changed", "newer"))

  if (!dir.exists(local_folder)) {
    stop(sprintf("[DROPBOX] Lokale map niet gevonden: %s", local_folder))
  }

  files <- list.files(local_folder, full.names = TRUE, recursive = recursive)
  files <- files[!file.info(.win_long_path(files))$isdir]

  n_files <- length(files)

  if (n_files == 0) {
    message(sprintf("[DROPBOX] Geen bestanden gevonden in %s", local_folder))
    return(invisible(character(0)))
  }

  message(sprintf("[DROPBOX] %d bestand(en) gevonden, uploaden gestart...", n_files))

  # --- Dropbox-inhoud ophalen voor overwrite-check ---
  dropbox_lookup <- list()
  if (overwrite != "always") {
    db_entries <- tryCatch(
      .list_folder_recursive(dropbox_folder, token, recursive),
      error = function(e) list()
    )
    file_entries <- Filter(function(e) e[[".tag"]] == "file", db_entries)

    root_prefix_lower <- tolower(sub("/$", "", dropbox_folder))
    for (entry in file_entries) {
      rel_path <- substring(entry$path_lower, nchar(root_prefix_lower) + 1)
      dropbox_lookup[[rel_path]] <- list(
        size     = entry$size,
        modified = entry$client_modified
      )
    }
  }

  # --- Upload met voortgangsbalk ---
  pb       <- utils::txtProgressBar(min = 0, max = n_files, style = 3)
  uploaded <- character(0)
  skipped  <- 0L
  failed   <- character(0)

  for (i in seq_along(files)) {
    local_path    <- files[i]
    relative_path <- substring(local_path, nchar(local_folder) + 1)
    relative_path <- gsub("\\\\", "/", relative_path)
    dropbox_path  <- paste0(dropbox_folder, relative_path)

    # --- Overwrite-logica: controleer of we dit bestand moeten overslaan ---
    if (overwrite != "always") {
      rel_key <- tolower(relative_path)
      db_info <- dropbox_lookup[[rel_key]]

      if (!is.null(db_info)) {
        skip      <- FALSE
        local_long <- .win_long_path(local_path)

        if (overwrite == "missing") {
          skip <- TRUE
        } else if (overwrite == "changed") {
          local_size <- file.info(local_long)$size
          skip <- !is.na(local_size) && local_size == db_info$size
        } else if (overwrite == "newer") {
          local_mtime <- file.info(local_long)$mtime
          db_mtime    <- as.POSIXct(db_info$modified,
                                     format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
          skip <- !is.na(local_mtime) && !is.na(db_mtime) && local_mtime <= db_mtime
        }

        if (skip) {
          skipped <- skipped + 1L
          utils::setTxtProgressBar(pb, i)
          next
        }
      }
    }

    # --- Upload met error handling ---
    tryCatch({
      dropbox_upload(local_path, dropbox_path, token,
                     mode                 = mode,
                     large_file_threshold = large_file_threshold)
      uploaded <- c(uploaded, dropbox_path)
    }, error = function(e) {
      failed <<- c(failed, sprintf("  %s\n    %s", local_path, conditionMessage(e)))
    })

    utils::setTxtProgressBar(pb, i)
  }

  close(pb)

  # --- Samenvatting ---
  message(sprintf(
    "\n[DROPBOX] Klaar: %d geupload, %d overgeslagen, %d mislukt (van %d totaal)",
    length(uploaded), skipped, length(failed), n_files
  ))

  if (length(failed) > 0) {
    warning(sprintf(
      "[DROPBOX] %d bestand(en) niet geupload:\n%s",
      length(failed), paste(failed, collapse = "\n")
    ), call. = FALSE)
  }

  invisible(uploaded)
}
