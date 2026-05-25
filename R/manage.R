# manage.R
# Functies voor het beheren van bestanden en mappen op Dropbox:
# verwijderen, verplaatsen/hernoemen, kopiëren, en mappen aanmaken.
# Gebruikt de /2/files/ endpoints van de Dropbox API v2.


#' Verwijder een bestand of map op Dropbox
#'
#' @description
#' Verwijdert een bestand of map (inclusief inhoud) op Dropbox. De verwijdering
#' is permanent vanuit de API — het item wordt naar de Dropbox prullenbak
#' verplaatst en kan daar nog hersteld worden.
#'
#' @param dropbox_path Pad op Dropbox van het te verwijderen item, bijv.
#'   `"/data/oud_bestand.csv"` of `"/temp"`. Moet beginnen met `/`.
#' @param token Dropbox API access token als character string. Standaard
#'   wordt automatisch een token opgehaald via [RLdrop::dropbox_token()].
#'
#' @return Invisibly de metadata van het verwijderde item als lijst.
#'
#' @details
#' Gebruikt het `/2/files/delete_v2` endpoint. Bij het verwijderen van een
#' map worden alle bestanden en submappen mee verwijderd. Items komen in de
#' Dropbox prullenbak terecht en kunnen daar nog hersteld worden (afhankelijk
#' van je Dropbox plan).
#'
#' Gooit een fout als het pad niet bestaat of bij onvoldoende rechten.
#'
#' @section Authenticatie:
#' Het token wordt standaard automatisch opgehaald via [RLdrop::dropbox_token()].
#' Zie [RLdrop::dropbox_token()] voor het instellen van de benodigde omgevingsvariabelen.
#'
#' @seealso
#' [RLdrop::dropbox_exists()] om te controleren of een pad bestaat voor het verwijderen. \cr
#' [RLdrop::dropbox_move()] om een item te verplaatsen in plaats van te verwijderen.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Bestand verwijderen
#' dropbox_delete("/data/oud_bestand.csv")
#'
#' # Map verwijderen (inclusief inhoud)
#' dropbox_delete("/temp/oude_exports")
#'
#' # Controleren voor verwijderen
#' if (dropbox_exists("/data/temp.csv")) {
#'   dropbox_delete("/data/temp.csv")
#' }
#' }
dropbox_delete <- function(dropbox_path, token = dropbox_token()) {

  response <- httr::POST(
    "https://api.dropboxapi.com/2/files/delete_v2",
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
      "[DROPBOX] Verwijderen mislukt voor '%s': %s",
      dropbox_path,
      httr::content(response, as = "text", encoding = "UTF-8")
    ))
  }

  metadata <- httr::content(response)$metadata
  message(sprintf("[DROPBOX] Verwijderd: %s", dropbox_path))
  invisible(metadata)
}


#' Verplaats of hernoem een bestand of map op Dropbox
#'
#' @description
#' Verplaatst een bestand of map naar een nieuw pad op Dropbox. Kan ook
#' gebruikt worden om items te hernoemen door het pad te wijzigen maar de
#' map hetzelfde te houden.
#'
#' @param from_path Huidig pad op Dropbox, bijv. `"/data/oud.csv"`.
#'   Moet beginnen met `/`.
#' @param to_path Nieuw pad op Dropbox, bijv. `"/archief/oud.csv"` of
#'   `"/data/nieuw.csv"` (voor hernoemen). Moet beginnen met `/`.
#' @param token Dropbox API access token als character string. Standaard
#'   wordt automatisch een token opgehaald via [RLdrop::dropbox_token()].
#' @param allow_ownership_transfer Logisch. Indien `TRUE`, staat Dropbox toe
#'   dat eigendom van het item wordt overgedragen bij verplaatsing naar een
#'   gedeelde map van een andere gebruiker. Standaard `FALSE`.
#'
#' @return Invisibly de metadata van het verplaatste item als lijst.
#'
#' @details
#' Gebruikt het `/2/files/move_v2` endpoint. Bij het verplaatsen van een
#' map worden alle bestanden en submappen mee verplaatst. Eventuele
#' bovenliggende mappen op het doelpad worden niet automatisch aangemaakt —
#' gebruik [RLdrop::dropbox_create_folder()] als het doelpad nog niet bestaat.
#'
#' Gooit een fout als het bronpad niet bestaat, het doelpad al bezet is,
#' of bij onvoldoende rechten.
#'
#' @section Authenticatie:
#' Het token wordt standaard automatisch opgehaald via [RLdrop::dropbox_token()].
#' Zie [RLdrop::dropbox_token()] voor het instellen van de benodigde omgevingsvariabelen.
#'
#' @seealso
#' [RLdrop::dropbox_delete()] om een item te verwijderen. \cr
#' [RLdrop::dropbox_create_folder()] om een doelmap aan te maken.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Bestand verplaatsen
#' dropbox_move("/data/rapport.xlsx", "/archief/rapport.xlsx")
#'
#' # Bestand hernoemen (zelfde map, andere naam)
#' dropbox_move("/data/draft.csv", "/data/final.csv")
#'
#' # Map verplaatsen (inclusief inhoud)
#' dropbox_move("/projecten/oud", "/archief/oud")
#' }
dropbox_move <- function(from_path, to_path, token = dropbox_token(),
                          allow_ownership_transfer = FALSE) {

  response <- httr::POST(
    "https://api.dropboxapi.com/2/files/move_v2",
    httr::add_headers(
      Authorization  = paste("Bearer", token),
      `Content-Type` = "application/json"
    ),
    body = jsonlite::toJSON(
      list(
        from_path                = from_path,
        to_path                  = to_path,
        allow_ownership_transfer = allow_ownership_transfer,
        autorename               = FALSE
      ),
      auto_unbox = TRUE
    )
  )

  if (httr::http_error(response)) {
    stop(sprintf(
      "[DROPBOX] Verplaatsen mislukt van '%s' naar '%s': %s",
      from_path, to_path,
      httr::content(response, as = "text", encoding = "UTF-8")
    ))
  }

  metadata <- httr::content(response)$metadata
  message(sprintf("[DROPBOX] Verplaatst: %s -> %s", from_path, to_path))
  invisible(metadata)
}


#' Maak een map aan op Dropbox
#'
#' @description
#' Maakt een nieuwe map aan op Dropbox. Tussenliggende mappen worden
#' automatisch aangemaakt als ze nog niet bestaan.
#'
#' @param dropbox_path Pad van de aan te maken map, bijv.
#'   `"/projecten/2026/Q3"`. Moet beginnen met `/`.
#' @param token Dropbox API access token als character string. Standaard
#'   wordt automatisch een token opgehaald via [RLdrop::dropbox_token()].
#'
#' @return Invisibly de metadata van de aangemaakte map als lijst.
#'
#' @details
#' Gebruikt het `/2/files/create_folder_v2` endpoint met
#' `autorename = FALSE`. Gooit een fout als het pad al bestaat. Gebruik
#' [RLdrop::dropbox_exists()] om eerst te controleren of de map al bestaat.
#'
#' Tussenliggende mappen in het pad worden automatisch aangemaakt door de
#' Dropbox API.
#'
#' @section Authenticatie:
#' Het token wordt standaard automatisch opgehaald via [RLdrop::dropbox_token()].
#' Zie [RLdrop::dropbox_token()] voor het instellen van de benodigde omgevingsvariabelen.
#'
#' @seealso
#' [RLdrop::dropbox_exists()] om te controleren of een map al bestaat. \cr
#' [RLdrop::dropbox_delete()] om een map te verwijderen.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Map aanmaken
#' dropbox_create_folder("/projecten/2026/Q3")
#'
#' # Alleen aanmaken als de map nog niet bestaat
#' if (!dropbox_exists("/data/exports")) {
#'   dropbox_create_folder("/data/exports")
#' }
#' }
dropbox_create_folder <- function(dropbox_path, token = dropbox_token()) {

  response <- httr::POST(
    "https://api.dropboxapi.com/2/files/create_folder_v2",
    httr::add_headers(
      Authorization  = paste("Bearer", token),
      `Content-Type` = "application/json"
    ),
    body = jsonlite::toJSON(
      list(path = dropbox_path, autorename = FALSE),
      auto_unbox = TRUE
    )
  )

  if (httr::http_error(response)) {
    stop(sprintf(
      "[DROPBOX] Map aanmaken mislukt voor '%s': %s",
      dropbox_path,
      httr::content(response, as = "text", encoding = "UTF-8")
    ))
  }

  metadata <- httr::content(response)$metadata
  message(sprintf("[DROPBOX] Map aangemaakt: %s", dropbox_path))
  invisible(metadata)
}


#' Kopieer een bestand of map op Dropbox
#'
#' @description
#' Kopieert een bestand of map naar een nieuw pad op Dropbox. Het origineel
#' blijft ongewijzigd op de oorspronkelijke locatie.
#'
#' @param from_path Bronpad op Dropbox, bijv. `"/data/rapport.csv"`.
#'   Moet beginnen met `/`.
#' @param to_path Doelpad op Dropbox, bijv. `"/archief/rapport.csv"`.
#'   Moet beginnen met `/`.
#' @param token Dropbox API access token als character string. Standaard
#'   wordt automatisch een token opgehaald via [RLdrop::dropbox_token()].
#' @param allow_ownership_transfer Logisch. Indien `TRUE`, staat Dropbox toe
#'   dat eigendom van het item wordt overgedragen bij kopiëren naar een
#'   gedeelde map van een andere gebruiker. Standaard `FALSE`.
#'
#' @return Invisibly de metadata van het gekopieerde item als lijst.
#'
#' @details
#' Gebruikt het `/2/files/copy_v2` endpoint. Bij het kopiëren van een
#' map worden alle bestanden en submappen mee gekopieerd. Eventuele
#' bovenliggende mappen op het doelpad worden niet automatisch aangemaakt —
#' gebruik [RLdrop::dropbox_create_folder()] als het doelpad nog niet bestaat.
#'
#' Gooit een fout als het bronpad niet bestaat, het doelpad al bezet is,
#' of bij onvoldoende rechten.
#'
#' @section Authenticatie:
#' Het token wordt standaard automatisch opgehaald via [RLdrop::dropbox_token()].
#' Zie [RLdrop::dropbox_token()] voor het instellen van de benodigde omgevingsvariabelen.
#'
#' @seealso
#' [RLdrop::dropbox_move()] om een item te verplaatsen in plaats van te kopiëren. \cr
#' [RLdrop::dropbox_delete()] om een item te verwijderen.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Bestand kopiëren
#' dropbox_copy("/data/rapport.xlsx", "/archief/rapport.xlsx")
#'
#' # Map kopiëren (inclusief inhoud)
#' dropbox_copy("/projecten/template", "/projecten/nieuw_project")
#' }
dropbox_copy <- function(from_path, to_path, token = dropbox_token(),
                         allow_ownership_transfer = FALSE) {

  response <- httr::POST(
    "https://api.dropboxapi.com/2/files/copy_v2",
    httr::add_headers(
      Authorization  = paste("Bearer", token),
      `Content-Type` = "application/json"
    ),
    body = jsonlite::toJSON(
      list(
        from_path                = from_path,
        to_path                  = to_path,
        allow_ownership_transfer = allow_ownership_transfer,
        autorename               = FALSE
      ),
      auto_unbox = TRUE
    )
  )

  if (httr::http_error(response)) {
    stop(sprintf(
      "[DROPBOX] Kopie mislukt van '%s' naar '%s': %s",
      from_path, to_path,
      httr::content(response, as = "text", encoding = "UTF-8")
    ))
  }

  metadata <- httr::content(response)$metadata
  message(sprintf("[DROPBOX] Gekopieerd: %s -> %s", from_path, to_path))
  invisible(metadata)
}
