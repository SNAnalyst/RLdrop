# search.R
# Zoekfunctie voor het doorzoeken van Dropbox op bestandsnaam.
# Gebruikt het /2/files/search_v2 endpoint met automatische paginering.


#' Zoek naar bestanden en mappen op Dropbox
#'
#' @description
#' Doorzoekt je Dropbox op bestandsnaam en retourneert een data.frame met de
#' gevonden items. Ondersteunt optioneel beperken tot een specifieke map en
#' filteren op bestandsextensies. Bij veel resultaten wordt automatisch
#' gepagineerd.
#'
#' @param query Zoekterm als character string, bijv. `"rapport"` of
#'   `"2026 Q3"`. Dropbox zoekt op bestandsnaam en mapnaam.
#' @param token Dropbox API access token als character string. Standaard
#'   wordt automatisch een token opgehaald via [dropbox_token()].
#' @param path Optioneel pad om de zoekopdracht te beperken tot een specifieke
#'   map, bijv. `"/data"`. Standaard `NULL` (doorzoekt hele Dropbox).
#' @param max_results Maximum aantal resultaten. Standaard 100. Dropbox
#'   retourneert maximaal 1000 resultaten per pagina; bij meer resultaten
#'   wordt automatisch gepagineerd.
#' @param extensions Optioneel character vector van bestandsextensies om op
#'   te filteren, bijv. `c("csv", "xlsx")`. Zonder punt. Standaard `NULL`
#'   (geen filter).
#'
#' @return Een data.frame met de volgende kolommen:
#' \describe{
#'   \item{`naam`}{Bestandsnaam of mapnaam.}
#'   \item{`pad`}{Volledig pad op Dropbox (`path_display`).}
#'   \item{`type`}{`"file"` of `"folder"`.}
#'   \item{`grootte`}{Bestandsgrootte in bytes. `NA` voor mappen.}
#'   \item{`gewijzigd`}{Wijzigingsdatum (`client_modified`) als POSIXct.
#'     `NA` voor mappen.}
#' }
#'
#' Bij geen resultaten wordt een leeg data.frame met de juiste kolommen
#' geretourneerd.
#'
#' @details
#' Gebruikt het `/2/files/search_v2` endpoint. De Dropbox zoekfunctie
#' zoekt op woorden in bestandsnamen en is niet hoofdlettergevoelig.
#' Meerdere woorden in de query worden als AND behandeld.
#'
#' Bij meer resultaten dan een enkele pagina (max 1000 per pagina) wordt
#' automatisch doorgebladerd via `/2/files/search/continue_v2` totdat
#' `max_results` is bereikt of er geen resultaten meer zijn.
#'
#' @section Authenticatie:
#' Het token wordt standaard automatisch opgehaald via [dropbox_token()].
#' Zie [dropbox_token()] voor het instellen van de benodigde omgevingsvariabelen.
#'
#' @seealso
#' [dropbox_list_folder()] voor het listen van een specifieke map. \cr
#' [dropbox_file_info()] voor metadata van een enkel bestand.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Zoek naar alle bestanden met "rapport" in de naam
#' dropbox_search("rapport")
#'
#' # Zoek alleen in een specifieke map
#' dropbox_search("2026", path = "/projecten")
#'
#' # Zoek alleen CSV en Excel bestanden
#' dropbox_search("export", extensions = c("csv", "xlsx"))
#'
#' # Meer resultaten ophalen
#' alle_parquet <- dropbox_search("parquet", max_results = 500)
#' }
dropbox_search <- function(query, token = dropbox_token(), path = NULL,
                            max_results = 100L, extensions = NULL) {

  # --- Bouw request body op ---
  # [ARCH] De search_v2 API verwacht options als apart object met optionele velden
  options <- list()

  if (!is.null(path)) {
    options$path <- path
  }

  if (!is.null(extensions)) {
    options$file_extensions <- as.list(extensions)
  }

  # [ARCH] Dropbox limiteert max_results per pagina tot 1000
  page_size <- min(max_results, 1000L)

  body <- list(
    query   = query,
    options = options
  )

  # [ARCH] Alleen max_results toevoegen aan options als het afwijkt van default
  if (page_size != 100L) {
    options$max_results <- page_size
    body$options <- options
  }

  # --- Eerste pagina ophalen ---
  response <- httr::POST(
    "https://api.dropboxapi.com/2/files/search_v2",
    httr::add_headers(
      Authorization  = paste("Bearer", token),
      `Content-Type` = "application/json"
    ),
    body = jsonlite::toJSON(body, auto_unbox = TRUE)
  )

  if (httr::http_error(response)) {
    stop(sprintf(
      "[DROPBOX] Zoeken mislukt voor '%s': %s",
      query,
      httr::content(response, as = "text", encoding = "UTF-8")
    ))
  }

  result  <- httr::content(response)
  matches <- result$matches

  # --- Paginering: volg cursor als er meer resultaten zijn ---
  while (isTRUE(result$has_more) && length(matches) < max_results) {
    continue_response <- httr::POST(
      "https://api.dropboxapi.com/2/files/search/continue_v2",
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
        "[DROPBOX] Zoekpaginering mislukt: %s",
        httr::content(continue_response, as = "text", encoding = "UTF-8")
      ))
    }

    result  <- httr::content(continue_response)
    matches <- c(matches, result$matches)
  }

  # [ARCH] Beperk tot max_results als paginering meer heeft opgeleverd
  if (length(matches) > max_results) {
    matches <- matches[seq_len(max_results)]
  }

  # --- Bouw data.frame ---
  if (length(matches) == 0) {
    message(sprintf("[DROPBOX] Geen resultaten voor '%s'", query))
    return(data.frame(
      naam      = character(0),
      pad       = character(0),
      type      = character(0),
      grootte   = numeric(0),
      gewijzigd = as.POSIXct(character(0)),
      stringsAsFactors = FALSE
    ))
  }

  n <- length(matches)
  naam      <- character(n)
  pad       <- character(n)
  item_type <- character(n)
  grootte   <- rep(NA_real_, n)
  gewijzigd <- rep(as.POSIXct(NA), n)

  for (i in seq_along(matches)) {
    # [ARCH] search_v2 wraps elke match in een metadata-object met .tag "metadata"
    meta <- matches[[i]]$metadata$metadata

    naam[i]      <- meta$name
    pad[i]       <- meta$path_display
    item_type[i] <- meta[[".tag"]]

    if (meta[[".tag"]] == "file") {
      grootte[i]   <- meta$size
      gewijzigd[i] <- as.POSIXct(meta$client_modified,
                                  format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
    }
  }

  result_df <- data.frame(
    naam      = naam,
    pad       = pad,
    type      = item_type,
    grootte   = grootte,
    gewijzigd = gewijzigd,
    stringsAsFactors = FALSE
  )

  message(sprintf("[DROPBOX] %d resultaat/resultaten voor '%s'", nrow(result_df), query))

  result_df
}
