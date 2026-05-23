# compare.R
# Vergelijkt de inhoud van een lokale map met een map op Dropbox.
# Controleert op bestanden die alleen lokaal of alleen op Dropbox bestaan,
# en vergelijkt grootte en optioneel wijzigingsdatum voor gedeelde bestanden.


#' Vergelijk een lokale map met een Dropbox-map
#'
#' @description
#' Vergelijkt de inhoud van een lokale map met een map op Dropbox op basis van
#' bestandsnaam, grootte (bytes) en optioneel wijzigingsdatum. Geeft een
#' overzichtelijk data.frame terug met de status per bestand.
#'
#' Handig om te controleren of een lokale kopie nog synchroon loopt met
#' Dropbox, of om te zien welke bestanden ontbreken aan een van beide kanten.
#'
#' @param local_folder Lokaal pad van de map om te vergelijken, bijv.
#'   `"D:/data/parquet"`. Gooit een fout als de map niet bestaat.
#' @param dropbox_folder Pad naar de map op Dropbox, bijv. `"/data/parquet"`.
#'   Moet beginnen met `/`. Gebruik `""` voor de root van je Dropbox.
#' @param token Dropbox API access token als character string. Standaard
#'   wordt automatisch een token opgehaald via [dropbox_token()].
#' @param recursive Logisch. Indien `TRUE`, worden submappen ook vergeleken.
#'   Standaard `FALSE`.
#' @param compare_date Logisch. Indien `TRUE` (standaard), wordt ook de
#'   wijzigingsdatum vergeleken. Dropbox `client_modified` wordt vergeleken
#'   met de lokale `mtime`. Datums worden afgerond op hele seconden.
#'
#' @return Een data.frame met de volgende kolommen:
#' \describe{
#'   \item{`naam`}{Relatief pad van het bestand ten opzichte van de rootmap.}
#'   \item{`status`}{Status van het bestand:
#'     \itemize{
#'       \item `"gelijk"` — naam, grootte (en optioneel datum) komen overeen.
#'       \item `"grootte_verschilt"` — bestand bestaat aan beide kanten maar
#'         verschilt in grootte.
#'       \item `"datum_verschilt"` — grootte is gelijk maar datum verschilt
#'         (alleen als `compare_date = TRUE`).
#'       \item `"alleen_lokaal"` — bestand bestaat alleen in de lokale map.
#'       \item `"alleen_dropbox"` — bestand bestaat alleen op Dropbox.
#'     }
#'   }
#'   \item{`lokaal_grootte`}{Bestandsgrootte lokaal in bytes, of `NA`.}
#'   \item{`dropbox_grootte`}{Bestandsgrootte op Dropbox in bytes, of `NA`.}
#'   \item{`lokaal_datum`}{Wijzigingsdatum lokaal als POSIXct, of `NA`.}
#'   \item{`dropbox_datum`}{Wijzigingsdatum op Dropbox (`client_modified`)
#'     als POSIXct, of `NA`.}
#' }
#'
#' Het data.frame is gesorteerd op status (problemen eerst) en daarna op naam.
#'
#' @details
#' De vergelijking werkt als volgt:
#' \enumerate{
#'   \item Lokale bestanden worden geinventariseerd via [list.files()] en
#'     [file.info()]. Dropbox-bestanden via de `list_folder` API (met
#'     automatische paginering bij >2000 items).
#'   \item Bestandsnamen worden case-insensitive vergeleken (via [tolower()])
#'     omdat Dropbox case-insensitief is.
#'   \item Voor bestanden die aan beide kanten bestaan wordt eerst de grootte
#'     vergeleken. Als `compare_date = TRUE` en de grootte gelijk is, wordt
#'     ook de datum vergeleken (met een tolerantie van 2 seconden).
#' }
#'
#' Aan het eind wordt een samenvatting naar de console geprint met het
#' aantal bestanden per status.
#'
#' @section Authenticatie:
#' Het token wordt standaard automatisch opgehaald via [dropbox_token()].
#' Zie [dropbox_token()] voor het instellen van de benodigde omgevingsvariabelen.
#'
#' @seealso
#' [dropbox_download_folder()] om ontbrekende bestanden te downloaden. \cr
#' [dropbox_upload_folder()] om ontbrekende bestanden te uploaden.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Snelle vergelijking (platte map, zonder datum)
#' dropbox_compare_folder(
#'   local_folder   = "D:/data/parquet",
#'   dropbox_folder = "/data/parquet",
#'   compare_date   = FALSE
#' )
#'
#' # Diepte-vergelijking met submappen en datumcontrole
#' resultaat <- dropbox_compare_folder(
#'   local_folder   = "D:/projecten/2026",
#'   dropbox_folder = "/projecten/2026",
#'   recursive      = TRUE,
#'   compare_date   = TRUE
#' )
#'
#' # Alleen bestanden met problemen bekijken
#' problemen <- resultaat[resultaat$status != "gelijk", ]
#'
#' # Alleen bestanden die lokaal ontbreken
#' ontbrekend <- resultaat[resultaat$status == "alleen_dropbox", ]
#' }
dropbox_compare_folder <- function(local_folder, dropbox_folder, token = dropbox_token(),
                                    recursive = FALSE, compare_date = TRUE) {

  # --- Validatie ---
  if (!dir.exists(local_folder)) {
    stop(sprintf("[DROPBOX] Lokale map niet gevonden: %s", local_folder))
  }

  # --- Fase 1: lokale bestanden inventariseren ---
  # [ARCH] list.files geeft relatieve paden; file.info geeft grootte en mtime
  local_files <- list.files(local_folder, recursive = recursive, full.names = FALSE)

  # Filter mappen eruit (list.files geeft ook dirs terug bij recursive = TRUE)
  local_full_paths <- file.path(local_folder, local_files)
  is_file          <- !file.info(local_full_paths)$isdir
  local_files      <- local_files[is_file]
  local_full_paths <- local_full_paths[is_file]

  local_info <- file.info(local_full_paths)

  # [ARCH] Gebruik forward slashes als sleutel voor consistente vergelijking
  local_keys <- tolower(gsub("\\\\", "/", local_files))

  message(sprintf("[DROPBOX] %d lokale bestanden gevonden", length(local_keys)))

  # --- Fase 2: Dropbox-bestanden inventariseren ---
  # [ARCH] Hergebruik de interne .list_folder_recursive helper uit download.R
  entries <- .list_folder_recursive(dropbox_folder, token, recursive)

  # Alleen bestanden, geen mappen
  file_entries <- Filter(function(e) e[[".tag"]] == "file", entries)

  message(sprintf("[DROPBOX] %d Dropbox-bestanden gevonden", length(file_entries)))

  # [ARCH] Bouw lookup-structuur voor Dropbox-bestanden.
  # Sleutel = relatief pad in lowercase (consistent met hoe Dropbox path_lower werkt).
  # Dropbox-root prefix verwijderen om relatief pad te krijgen.
  root_prefix_lower <- tolower(sub("/$", "", dropbox_folder))

  dropbox_lookup <- list()
  for (entry in file_entries) {
    # [ARCH] path_lower begint met het volledige Dropbox-pad; strip de root prefix + leading slash
    rel_path <- substring(entry$path_lower, nchar(root_prefix_lower) + 2)
    dropbox_lookup[[rel_path]] <- list(
      size     = entry$size,
      modified = entry$client_modified
    )
  }

  dropbox_keys <- names(dropbox_lookup)

  # --- Fase 3: vergelijk bestanden ---
  # [ARCH] Verzamel alle unieke bestandsnamen van beide kanten
  all_keys <- sort(unique(c(local_keys, dropbox_keys)))

  # [ARCH] Pre-alloceer resultaatvectoren voor snelheid
  n <- length(all_keys)
  naam             <- character(n)
  status           <- character(n)
  lokaal_grootte   <- rep(NA_real_, n)
  dropbox_grootte  <- rep(NA_real_, n)
  lokaal_datum     <- rep(as.POSIXct(NA), n)
  dropbox_datum    <- rep(as.POSIXct(NA), n)

  for (i in seq_along(all_keys)) {
    key <- all_keys[i]
    naam[i] <- key

    in_local   <- key %in% local_keys
    in_dropbox <- key %in% dropbox_keys

    if (in_local && !in_dropbox) {
      # [ARCH] Bestand bestaat alleen lokaal
      status[i] <- "alleen_lokaal"
      idx <- match(key, local_keys)
      lokaal_grootte[i] <- local_info$size[idx]
      lokaal_datum[i]   <- local_info$mtime[idx]

    } else if (!in_local && in_dropbox) {
      # [ARCH] Bestand bestaat alleen op Dropbox
      status[i] <- "alleen_dropbox"
      db <- dropbox_lookup[[key]]
      dropbox_grootte[i] <- db$size
      dropbox_datum[i]   <- as.POSIXct(db$modified, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")

    } else {
      # [ARCH] Bestand bestaat aan beide kanten — vergelijk grootte en optioneel datum
      idx <- match(key, local_keys)
      db  <- dropbox_lookup[[key]]

      lokaal_grootte[i]  <- local_info$size[idx]
      dropbox_grootte[i] <- db$size
      lokaal_datum[i]    <- local_info$mtime[idx]
      dropbox_datum[i]   <- as.POSIXct(db$modified, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")

      if (lokaal_grootte[i] != dropbox_grootte[i]) {
        status[i] <- "grootte_verschilt"
      } else if (compare_date) {
        # [ARCH] Tolerantie van 2 seconden vanwege afrondingsverschillen
        # tussen bestandssystemen en de Dropbox API
        datum_verschil <- abs(as.numeric(lokaal_datum[i]) - as.numeric(dropbox_datum[i]))
        if (is.na(datum_verschil) || datum_verschil > 2) {
          status[i] <- "datum_verschilt"
        } else {
          status[i] <- "gelijk"
        }
      } else {
        status[i] <- "gelijk"
      }
    }
  }

  # --- Fase 4: resultaat samenstellen ---
  result <- data.frame(
    naam            = naam,
    status          = status,
    lokaal_grootte  = lokaal_grootte,
    dropbox_grootte = dropbox_grootte,
    lokaal_datum    = lokaal_datum,
    dropbox_datum   = dropbox_datum,
    stringsAsFactors = FALSE
  )

  # [ARCH] Sorteer op status (problemen eerst) en dan op naam.
  # Volgorde: alleen_dropbox, alleen_lokaal, grootte_verschilt, datum_verschilt, gelijk
  status_order <- c("alleen_dropbox", "alleen_lokaal", "grootte_verschilt", "datum_verschilt", "gelijk")
  result$status_rang <- match(result$status, status_order)
  result <- result[order(result$status_rang, result$naam), ]
  result$status_rang <- NULL
  rownames(result)   <- NULL

  # --- Fase 5: samenvatting printen ---
  tabel <- table(factor(result$status, levels = status_order))
  message("\n[DROPBOX] Vergelijking voltooid:")
  message(sprintf("  Gelijk:             %d", tabel[["gelijk"]]))
  message(sprintf("  Alleen lokaal:      %d", tabel[["alleen_lokaal"]]))
  message(sprintf("  Alleen Dropbox:     %d", tabel[["alleen_dropbox"]]))
  message(sprintf("  Grootte verschilt:  %d", tabel[["grootte_verschilt"]]))
  if (compare_date) {
    message(sprintf("  Datum verschilt:    %d", tabel[["datum_verschilt"]]))
  }
  message(sprintf("  Totaal:             %d", nrow(result)))

  result
}
