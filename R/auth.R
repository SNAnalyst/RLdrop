# auth.R
# Authentication helpers for the Dropbox API v2.
# Handles refresh token flow to obtain short-lived access tokens automatically.
# Caches the access token in-session to avoid unnecessary API calls.


# [ARCH] Package-level cache environment - niet geexporteerd, alleen intern gebruikt
.token_cache <- new.env(parent = emptyenv())


#' Haal een geldig Dropbox access token op via refresh token
#'
#' @description
#' Vraagt een nieuw short-lived access token op bij de Dropbox API via de
#' OAuth2 refresh token flow, of geeft een gecached token terug als dat nog
#' geldig is. Het token wordt automatisch ververst wanneer het is verlopen.
#'
#' Credentials worden standaard uit omgevingsvariabelen gelezen zodat ze
#' nooit in code hoeven te staan.
#'
#' @param app_key Dropbox app key als character string. Standaard gelezen uit
#'   de omgevingsvariabele `DROPBOX_APP_KEY`.
#' @param app_secret Dropbox app secret als character string. Standaard gelezen
#'   uit de omgevingsvariabele `DROPBOX_APP_SECRET`.
#' @param refresh_token Dropbox refresh token als character string. Standaard
#'   gelezen uit de omgevingsvariabele `DROPBOX_REFRESH_TOKEN`. De refresh
#'   token verloopt niet en kan herhaaldelijk worden gebruikt.
#' @param margin Aantal seconden voor de officiele vervaltijd waarop het token
#'   al als verlopen wordt beschouwd. Standaard 300 seconden (5 minuten) als
#'   buffer voor klokverschillen en langlopende aanroepen.
#'
#' @return Een geldig access token als character string, direct bruikbaar als
#'   `token` argument in alle andere RLdrop functies.
#'
#' @details
#' Het token wordt gecached in een package-intern environment (`.token_cache`)
#' voor de duur van de R-sessie. Bij elke aanroep wordt gecontroleerd of het
#' gecachede token nog geldig is op basis van de vervaltijd die Dropbox
#' teruggeeft (`expires_in`, in seconden). Een nieuw token wordt alleen
#' opgehaald als:
#' \itemize{
#'   \item Er nog geen token gecached is, of
#'   \item Het gecachede token verlopen is (inclusief `margin`).
#' }
#'
#' De cache is sessiegebonden en wordt niet naar schijf geschreven.
#'
#' @section Credentials instellen:
#' Voeg de volgende regels toe aan je `.Renviron` (via `usethis::edit_r_environ()`):
#'
#' ```
#' DROPBOX_APP_KEY=jouw_app_key
#' DROPBOX_APP_SECRET=jouw_app_secret
#' DROPBOX_REFRESH_TOKEN=jouw_refresh_token
#' ```
#'
#' Herstart R na het opslaan zodat de variabelen beschikbaar zijn.
#'
#' @section Foutafhandeling:
#' Gooit een fout als een van de drie credentials leeg is, of als de
#' API-aanroep mislukt (bijv. bij een ongeldige refresh token).
#'
#' @seealso
#' [dropbox_download_file()], [dropbox_download_folder()],
#' [dropbox_upload()], [dropbox_upload_folder()] voor functies die het
#' token als argument accepteren.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Eerste aanroep: haalt token op via API
#' token <- dropbox_token()
#'
#' # Volgende aanroepen binnen ~4 uur: geeft gecached token terug
#' token <- dropbox_token()
#'
#' # Direct gebruiken - token wordt automatisch opgehaald of gecached teruggegeven
#' dropbox_download_file("/data/bestand.parquet", "D:/lokaal/bestand.parquet", dropbox_token())
#'
#' # Token cachen in variabele voor meerdere aanroepen in een script
#' token <- dropbox_token()
#' dropbox_download_file("/data/a.parquet", "D:/lokaal/a.parquet", token)
#' dropbox_download_file("/data/b.parquet", "D:/lokaal/b.parquet", token)
#' dropbox_upload("D:/lokaal/result.rds", "/output/result.rds", token)
#' }
dropbox_token <- function(app_key       = Sys.getenv("DROPBOX_APP_KEY"),
                           app_secret    = Sys.getenv("DROPBOX_APP_SECRET"),
                           refresh_token = Sys.getenv("DROPBOX_REFRESH_TOKEN"),
                           margin        = 300L) {

  # [ARCH] Vroeg valideren zodat de foutmelding duidelijk aangeeft wat ontbreekt
  if (nchar(app_key) == 0)       stop("[DROPBOX] DROPBOX_APP_KEY is niet ingesteld in .Renviron")
  if (nchar(app_secret) == 0)    stop("[DROPBOX] DROPBOX_APP_SECRET is niet ingesteld in .Renviron")
  if (nchar(refresh_token) == 0) stop("[DROPBOX] DROPBOX_REFRESH_TOKEN is niet ingesteld in .Renviron")

  # [ARCH] Controleer of gecached token nog geldig is (met margin als buffer)
  now <- as.numeric(Sys.time())
  if (!is.null(.token_cache$access_token) &&
      !is.null(.token_cache$expires_at)   &&
      now < (.token_cache$expires_at - margin)) {
    message("[DROPBOX] Geldig gecached token gebruikt")
    return(.token_cache$access_token)
  }

  # Geen geldig gecached token - nieuw ophalen
  message("[DROPBOX] Nieuw access token ophalen...")

  response <- httr::POST(
    "https://api.dropbox.com/oauth2/token",
    httr::authenticate(app_key, app_secret),
    body = list(
      grant_type    = "refresh_token",
      refresh_token = refresh_token
    ),
    encode = "form"
  )

  if (httr::http_error(response)) {
    stop(sprintf(
      "[DROPBOX] Token ophalen mislukt: %s",
      httr::content(response, as = "text", encoding = "UTF-8")
    ))
  }

  content <- httr::content(response)

  if (is.null(content$access_token)) {
    stop("[DROPBOX] Geen access_token in API-response - controleer je credentials")
  }

  # [ARCH] Sla token en vervaltijd op in package-intern cache environment
  .token_cache$access_token <- content$access_token
  .token_cache$expires_at   <- now + as.numeric(content$expires_in)

  message(sprintf("[DROPBOX] Nieuw token opgehaald, geldig tot %s",
    format(as.POSIXct(.token_cache$expires_at, origin = "1970-01-01"), "%H:%M:%S")))

  .token_cache$access_token
}
