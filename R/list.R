# list.R
# Exporteerbare functie om de inhoud van een Dropbox-map op te vragen.
# Wrapper rond de interne .list_folder_recursive helper uit download.R.
# Retourneert een data.frame met metadata per bestand/map.


#' Lijst de inhoud van een Dropbox-map op
#'
#' @description
#' Haalt de inhoud van een map op Dropbox op en retourneert een overzichtelijk
#' data.frame met metadata per item (bestanden en optioneel mappen).
#' Ondersteunt recursief listen en automatische paginering bij grote mappen
#' (>2000 items).
#'
#' @param dropbox_folder Pad naar de map op Dropbox, bijv. `"/data/parquet"`.
#'   Moet beginnen met `/`. Gebruik `""` voor de root van je Dropbox.
#' @param token Dropbox API access token als character string. Standaard
#'   wordt automatisch een token opgehaald via [dropbox_token()].
#' @param recursive Logisch. Indien `TRUE`, worden submappen ook doorlopen.
#'   Standaard `FALSE`.
#' @param type Welke items retourneren: `"all"` (standaard) voor bestanden
#'   en mappen, `"file"` voor alleen bestanden, `"folder"` voor alleen mappen.
#'
#' @return Een data.frame met de volgende kolommen:
#' \describe{
#'   \item{`naam`}{Bestandsnaam of mapnaam.}
#'   \item{`pad`}{Volledig pad op Dropbox zoals weergegeven (`path_display`).}
#'   \item{`type`}{`"file"` of `"folder"`.}
#'   \item{`grootte`}{Bestandsgrootte in bytes. `NA` voor mappen.}
#'   \item{`gewijzigd`}{Wijzigingsdatum (`client_modified`) als POSIXct in
#'     UTC. `NA` voor mappen.}
#' }
#'
#' Het data.frame is gesorteerd op pad.
#'
#' @details
#' Gebruikt de Dropbox `list_folder` API met automatische paginering.
#' Bij mappen met meer dan 2000 items worden alle pagina's automatisch
#' opgehaald via de `list_folder/continue` cursor.
#'
#' De kolom `gewijzigd` bevat de `client_modified` timestamp — dit is de
#' datum die de uploadende client heeft meegegeven, niet de server-datum.
#' Dit komt het meest overeen met de lokale wijzigingsdatum van het bestand.
#'
#' @section Authenticatie:
#' Het token wordt standaard automatisch opgehaald via [dropbox_token()].
#' Zie [dropbox_token()] voor het instellen van de benodigde omgevingsvariabelen.
#'
#' @seealso
#' [dropbox_compare_folder()] om een lokale map te vergelijken met Dropbox. \cr
#' [dropbox_download_folder()] om een Dropbox-map te downloaden.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Inhoud van een map bekijken
#' dropbox_list_folder("/data/parquet")
#'
#' # Alleen bestanden, recursief
#' bestanden <- dropbox_list_folder("/projecten/2026", recursive = TRUE, type = "file")
#'
#' # Alleen directe submappen
#' submappen <- dropbox_list_folder("/data", type = "folder")
#'
#' # Totale grootte berekenen
#' df <- dropbox_list_folder("/data", recursive = TRUE, type = "file")
#' sprintf("%.1f MB", sum(df$grootte, na.rm = TRUE) / 1024 / 1024)
#' }
dropbox_list_folder <- function(dropbox_folder, token = dropbox_token(),
                                 recursive = FALSE, type = "all") {

  # --- Validatie ---
  type <- match.arg(type, choices = c("all", "file", "folder"))

  # --- Entries ophalen via bestaande interne helper ---
  entries <- .list_folder_recursive(dropbox_folder, token, recursive)

  if (length(entries) == 0) {
    message(sprintf("[DROPBOX] Geen items gevonden in %s", dropbox_folder))
    return(data.frame(
      naam      = character(0),
      pad       = character(0),
      type      = character(0),
      grootte   = numeric(0),
      gewijzigd = as.POSIXct(character(0)),
      stringsAsFactors = FALSE
    ))
  }

  # --- Filter op type als gevraagd ---
  if (type != "all") {
    entries <- Filter(function(e) e[[".tag"]] == type, entries)
  }

  if (length(entries) == 0) {
    message(sprintf("[DROPBOX] Geen items van type '%s' gevonden in %s", type, dropbox_folder))
    return(data.frame(
      naam      = character(0),
      pad       = character(0),
      type      = character(0),
      grootte   = numeric(0),
      gewijzigd = as.POSIXct(character(0)),
      stringsAsFactors = FALSE
    ))
  }

  # --- Bouw data.frame op ---
  # [ARCH] Pre-alloceer vectoren voor snelheid bij grote mappen
  n <- length(entries)
  naam      <- character(n)
  pad       <- character(n)
  item_type <- character(n)
  grootte   <- rep(NA_real_, n)
  gewijzigd <- rep(as.POSIXct(NA), n)

  for (i in seq_along(entries)) {
    e <- entries[[i]]
    naam[i]      <- e$name
    pad[i]       <- e$path_display
    item_type[i] <- e[[".tag"]]

    if (e[[".tag"]] == "file") {
      grootte[i]   <- e$size
      gewijzigd[i] <- as.POSIXct(e$client_modified,
                                  format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
    }
  }

  result <- data.frame(
    naam      = naam,
    pad       = pad,
    type      = item_type,
    grootte   = grootte,
    gewijzigd = gewijzigd,
    stringsAsFactors = FALSE
  )

  # [ARCH] Sorteer op pad voor overzichtelijke output
  result <- result[order(result$pad), ]
  rownames(result) <- NULL

  message(sprintf("[DROPBOX] %d item(s) gevonden in %s", nrow(result), dropbox_folder))

  result
}
