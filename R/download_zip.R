# download_zip.R
# Fast folder download via Dropbox zip endpoint.
# Downloads each subfolder as a zip and extracts locally.
# Much faster than per-file downloads for large folder trees.


# Interne helper: pak zip uit met ondersteuning voor lange paden op Windows.
# PowerShell Expand-Archive gebruikt .NET dat lange paden correct afhandelt.
.unzip_safe <- function(zip_path, exdir) {
  if (.Platform$OS.type == "windows") {
    zip_norm <- normalizePath(zip_path, winslash = "\\", mustWork = TRUE)
    dir_norm <- normalizePath(exdir, winslash = "\\", mustWork = TRUE)
    ps_cmd <- sprintf(
      "Expand-Archive -LiteralPath '%s' -DestinationPath '%s' -Force",
      zip_norm, dir_norm
    )
    output <- system2("powershell",
                       c("-NoProfile", "-NonInteractive", "-Command", ps_cmd),
                       stdout = TRUE, stderr = TRUE)
    exit_code <- attr(output, "status")
    if (!is.null(exit_code) && exit_code != 0) {
      stop(sprintf("[DROPBOX] Uitpakken mislukt: %s", paste(output, collapse = "\n")))
    }
  } else {
    utils::unzip(zip_path, exdir = exdir)
  }
}


#' Download een Dropbox-map als zip en pak deze lokaal uit
#'
#' @description
#' Downloadt een Dropbox-map in zijn geheel als zip via het `/files/download_zip`
#' endpoint. Aanzienlijk sneller dan per-bestand downloaden via
#' [dropbox_download_folder()]. De zip wordt lokaal uitgepakt en daarna
#' verwijderd.
#'
#' Bij mappen groter dan de Dropbox-limiet (10.000 bestanden of 20GB) gebruik
#' dan [dropbox_download_folder_zip()] die automatisch per submap splitst.
#'
#' @param dropbox_folder Pad naar de map op Dropbox, bijv. `"/data/parquet"`.
#'   Moet beginnen met `/`.
#' @param local_folder Lokaal pad waar de inhoud van de zip wordt uitgepakt.
#'   Wordt aangemaakt als deze nog niet bestaat.
#' @param token Dropbox API access token als character string. Standaard
#'   wordt automatisch een token opgehaald via [dropbox_token()].
#' @param keep_zip Logisch. Indien `TRUE`, wordt de zip na uitpakken bewaard.
#'   Standaard `FALSE` (zip wordt verwijderd na uitpakken).
#'
#' @return Invisibly het lokale pad van de uitgepakte map.
#'
#' @details
#' Gebruikt het `/2/files/download_zip` endpoint. Dropbox limiteert dit
#' endpoint tot maximaal 10.000 bestanden of 20GB per aanroep. Grotere mappen
#' geven een API-fout; gebruik dan [dropbox_download_folder_zip()].
#'
#' De zip wordt tijdelijk opgeslagen in `local_folder` en na uitpakken
#' verwijderd (tenzij `keep_zip = TRUE`).
#'
#' @section Authenticatie:
#' Het token wordt standaard automatisch opgehaald via [dropbox_token()].
#' Zie [dropbox_token()] voor het instellen van de benodigde omgevingsvariabelen.
#'
#' @seealso
#' [dropbox_download_folder_zip()] voor grote mappen die automatisch per
#' submap worden gesplitst.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dropbox_download_zip(
#'   dropbox_folder = "/data/parquet/year=2024",
#'   local_folder   = "D:/lokaal/parquet/year=2024"
#' )
#' }
dropbox_download_zip <- function(dropbox_folder, local_folder, token = dropbox_token(), keep_zip = FALSE) {

  local_folder_long <- .win_long_path(local_folder)
  if (!dir.exists(local_folder_long)) dir.create(local_folder_long, recursive = TRUE)

  zip_path <- file.path(local_folder, "_dropbox_download.zip")
  zip_long <- .win_long_path(zip_path)

  message(sprintf("[DROPBOX] Zip downloaden: %s ...", dropbox_folder))

  response <- httr::POST(
    "https://content.dropboxapi.com/2/files/download_zip",
    httr::add_headers(
      Authorization     = paste("Bearer", token),
      `Dropbox-API-Arg` = jsonlite::toJSON(
        list(path = dropbox_folder),
        auto_unbox = TRUE
      )
    ),
    httr::write_disk(zip_long, overwrite = TRUE)
  )

  if (httr::http_error(response)) {
    if (file.exists(zip_long)) file.remove(zip_long)
    stop(sprintf(
      "[DROPBOX] Zip download mislukt voor '%s': %s",
      dropbox_folder,
      httr::content(response, as = "text", encoding = "UTF-8")
    ))
  }

  message(sprintf("[DROPBOX] Uitpakken naar %s ...", local_folder))
  .unzip_safe(zip_path, local_folder)

  if (!keep_zip) file.remove(zip_long)

  message(sprintf("[DROPBOX] Klaar: %s", local_folder))
  invisible(local_folder)
}


#' Download een grote Dropbox-map per submap als zip
#'
#' @description
#' Downloadt een grote Dropbox-map door elke directe submap afzonderlijk als
#' zip te downloaden en lokaal uit te pakken. Omzeilt de Dropbox-limiet van
#' 10.000 bestanden per zip-download. Veel sneller dan per-bestand downloaden
#' via [dropbox_download_folder()].
#'
#' @param dropbox_folder Pad naar de rootmap op Dropbox, bijv.
#'   `"/Datasets/EODHD/hulp"`. Moet beginnen met `/`.
#' @param local_folder Lokaal pad van de doelmap. Wordt aangemaakt als deze
#'   nog niet bestaat.
#' @param token Dropbox API access token als character string. Standaard
#'   wordt automatisch een token opgehaald via [dropbox_token()].
#' @param keep_zip Logisch. Indien `TRUE`, worden de zip-bestanden na uitpakken
#'   bewaard. Standaard `FALSE`.
#'
#' @return Invisibly een character vector van lokale paden van uitgepakte mappen.
#'
#' @details
#' De functie haalt eerst de directe submappen op via de Dropbox API
#' (niet recursief). Elke submap wordt daarna als zip gedownload via
#' [dropbox_download_zip()].
#'
#' Als een submap zelf ook groter is dan 10.000 bestanden of 20GB, zal die
#' individuele zip-download mislukken met een API-fout. In dat geval moet je
#' de functie aanroepen op een dieper niveau, bijv. een specifieke submap.
#'
#' Bestanden direct in de rootmap (niet in een submap) worden gedownload via
#' [dropbox_download_zip()] op de rootmap zelf.
#'
#' @section Authenticatie:
#' Het token wordt standaard automatisch opgehaald via [dropbox_token()].
#' Zie [dropbox_token()] voor het instellen van de benodigde omgevingsvariabelen.
#'
#' @seealso
#' [dropbox_download_zip()] voor het downloaden van een enkele map als zip.\cr
#' [dropbox_download_folder()] voor per-bestand downloaden met voortgangsbalk.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Grote map per submap downloaden
#' dropbox_download_folder_zip(
#'   dropbox_folder = "/Datasets/EODHD/hulp",
#'   local_folder   = "D:/lokaal/hulp"
#' )
#' }
dropbox_download_folder_zip <- function(dropbox_folder, local_folder, token = dropbox_token(), keep_zip = FALSE) {

  local_folder_long <- .win_long_path(local_folder)
  if (!dir.exists(local_folder_long)) dir.create(local_folder_long, recursive = TRUE)

  # [ARCH] Haal alleen directe submappen op (niet recursief) om per submap te splitsen
  message(sprintf("[DROPBOX] Submappen ophalen van %s ...", dropbox_folder))

  list_response <- httr::POST(
    "https://api.dropboxapi.com/2/files/list_folder",
    httr::add_headers(
      Authorization  = paste("Bearer", token),
      `Content-Type` = "application/json"
    ),
    body = jsonlite::toJSON(
      list(path = dropbox_folder, recursive = FALSE),
      auto_unbox = TRUE
    )
  )

  if (httr::http_error(list_response)) {
    stop(sprintf(
      "[DROPBOX] Submappen ophalen mislukt voor '%s': %s",
      dropbox_folder,
      httr::content(list_response, as = "text", encoding = "UTF-8")
    ))
  }

  entries <- httr::content(list_response)$entries

  folder_entries <- Filter(function(e) e[[".tag"]] == "folder", entries)
  file_entries   <- Filter(function(e) e[[".tag"]] == "file",   entries)

  message(sprintf("[DROPBOX] %d submap(pen) gevonden", length(folder_entries)))

  downloaded <- character(0)

  # [ARCH] Download bestanden direct in de rootmap als aparte zip
  if (length(file_entries) > 0) {
    message(sprintf("[DROPBOX] Bestanden in rootmap downloaden als zip..."))
    tryCatch({
      dropbox_download_zip(dropbox_folder, local_folder, token, keep_zip = keep_zip)
      downloaded <- c(downloaded, local_folder)
    }, error = function(e) {
      warning(sprintf("[DROPBOX] Rootmap zip mislukt: %s", e$message))
    })
  }

  # [ARCH] Download elke submap als aparte zip
  pb <- utils::txtProgressBar(min = 0, max = length(folder_entries), style = 3)

  for (i in seq_along(folder_entries)) {
    folder     <- folder_entries[[i]]
    sub_remote <- folder$path_display
    sub_local  <- file.path(local_folder, basename(folder$path_lower))

    tryCatch({
      dropbox_download_zip(sub_remote, sub_local, token, keep_zip = keep_zip)
      downloaded <- c(downloaded, sub_local)
    }, error = function(e) {
      warning(sprintf(
        "[DROPBOX] Submap '%s' mislukt (mogelijk >10.000 bestanden of >20GB): %s\n  Probeer dropbox_download_folder_zip() op dit pad.",
        sub_remote, e$message
      ))
    })

    utils::setTxtProgressBar(pb, i)
  }

  close(pb)
  message(sprintf("\n[DROPBOX] %d/%d submappen gedownload naar %s",
    length(downloaded), length(folder_entries), local_folder))
  invisible(downloaded)
}
