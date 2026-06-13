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
#' @param token Dropbox API access token als character string. Standaard
#'   wordt automatisch een token opgehaald via [RLdrop::dropbox_token()].
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
#' Het token wordt standaard automatisch opgehaald via [RLdrop::dropbox_token()].
#' Zie [RLdrop::dropbox_token()] voor het instellen van de benodigde omgevingsvariabelen.
#'
#' @seealso
#' [RLdrop::dropbox_download_folder()] om een volledige map te downloaden.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Enkelvoudig bestand downloaden
#' dropbox_download_file(
#'   dropbox_path = "/data/bestand.parquet",
#'   local_path   = "D:/lokaal/bestand.parquet"
#' )
#'
#' # Resultaat opvangen en direct inlezen
#' pad    <- dropbox_download_file("/config/settings.rds", tempfile())
#' config <- readRDS(pad)
#' }
dropbox_download_file <- function(dropbox_path, local_path, token = dropbox_token()) {

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
  writeBin(httr::content(response, as = "raw"), .win_long_path(local_path))
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


# Interne helper: op Windows paden >250 tekens prefixen met \\?\ zodat de
# wide-char API wordt gebruikt en het 260-karakterlimiet vervalt.
.win_long_path <- function(path) {
  if (.Platform$OS.type != "windows") return(path)
  path <- normalizePath(path, winslash = "\\", mustWork = FALSE)
  # [ARCH] Vectorized: deze helper wordt ook met meerdere paden tegelijk
  # aangeroepen (bv. file.info(.win_long_path(files))), dus elke padelement
  # afzonderlijk toetsen in plaats van && op een vector (die faalt).
  to_prefix <- nchar(path) > 250 & !startsWith(path, "\\\\?\\")
  path[to_prefix] <- paste0("\\\\?\\", path[to_prefix])
  path
}


#' Download een volledige map van Dropbox direct via API
#'
#' @description
#' Downloadt alle bestanden in een Dropbox-map naar een lokale map. Optioneel
#' recursief voor submappen. Toont een voortgangsbalk tijdens het downloaden.
#' Mappen met meer dan 2000 bestanden worden automatisch gepagineerd afgehandeld.
#'
#' Individuele bestanden die niet gedownload kunnen worden (bijv. te lang pad,
#' geen rechten) worden overgeslagen en aan het eind als waarschuwing gemeld,
#' zodat de rest van de download gewoon doorgaat.
#'
#' @param dropbox_folder Pad naar de map op Dropbox, bijv. `"/data/parquet"`.
#'   Moet beginnen met `/`. Gebruik `""` voor de root van je Dropbox.
#' @param local_folder Lokaal pad van de doelmap. Wordt aangemaakt als deze
#'   nog niet bestaat, inclusief bovenliggende mappen.
#' @param token Dropbox API access token als character string. Standaard
#'   wordt automatisch een token opgehaald via [RLdrop::dropbox_token()].
#' @param recursive Logisch. Indien `TRUE`, worden submappen ook gedownload
#'   en wordt de mapstructuur lokaal gerepliceerd. Standaard `FALSE`.
#' @param overwrite Bepaalt welke bestanden gedownload worden als ze lokaal
#'   al bestaan:
#'   \describe{
#'     \item{`"always"`}{Download alles, ongeacht wat er lokaal staat.}
#'     \item{`"missing"`}{Sla bestanden over die lokaal al bestaan (op basis
#'       van naam).}
#'     \item{`"changed"`}{Download alleen ontbrekende bestanden en bestanden
#'       waarvan de grootte (bytes) verschilt.}
#'     \item{`"newer"`}{Download alleen ontbrekende bestanden en bestanden
#'       waarvan de Dropbox-versie nieuwer is dan de lokale
#'       (`client_modified` > lokale `mtime`) (standaard).}
#'   }
#' @param pattern Optioneel character string met een reguliere expressie.
#'   Indien ingevuld worden alleen bestanden gedownload waarvan de Dropbox-
#'   bestandsnaam (`name` uit de Dropbox API) aan deze regex voldoet.
#'   Standaard `NULL`, waarmee alle gevonden bestanden worden gedownload.
#'   Het filter gebruikt base R [grepl()] en is hoofdlettergevoelig.
#' @param welke Optioneel character vector met exacte bestandsnamen die
#'   gedownload moeten worden, bijv. `c("A.txt", "B.txt", "C.R")`.
#'   De waarden worden vergeleken met de Dropbox-bestandsnaam (`name` uit de
#'   Dropbox API), niet met het volledige pad. Standaard `NULL`, waarmee niet
#'   op exacte bestandsnamen wordt gefilterd.
#'
#' @return Invisibly een character vector van lokale paden van daadwerkelijk
#'   gedownloade bestanden.
#'
#' @details
#' De functie werkt in drie fasen:
#' \enumerate{
#'   \item Alle bestandsitems worden eerst volledig geinventariseerd via de
#'     Dropbox `list_folder` API (inclusief paginering bij >2000 items).
#'   \item Bestanden worden gedownload via [RLdrop::dropbox_download_file()] met
#'     een voortgangsbalk. Bij `overwrite` anders dan `"always"` worden
#'     bestanden die al lokaal aanwezig zijn overgeslagen.
#'   \item Een samenvatting wordt getoond: gedownload, overgeslagen, en
#'     eventueel mislukte bestanden.
#' }
#'
#' Bij `recursive = TRUE` wordt de volledige mapboomstructuur gerepliceerd in
#' `local_folder`. Op Windows worden lange paden (>260 tekens) automatisch
#' afgehandeld via de `\\?\` prefix.
#'
#' Als `pattern` is ingevuld, wordt dit patroon alleen toegepast op de
#' bestandsnaam, niet op het volledige Dropbox-pad. Dit is bewust gekozen omdat
#' de meest voorkomende use case is: "download alleen `.csv`", "download alleen
#' bestanden met `2026` in de naam", enzovoort. Gebruik bijvoorbeeld
#' `pattern = "\\.csv$"` voor CSV-bestanden. Twijfelpunt voor later:
#' als padgebaseerde filtering nodig blijkt, kan een apart argument zoals
#' `pattern_scope = c("name", "path")` worden toegevoegd zonder deze interface
#' te breken.
#'
#' Als `welke` is ingevuld, worden alleen bestanden gedownload waarvan de naam
#' exact voorkomt in die vector. Als zowel `pattern` als `welke` zijn ingevuld,
#' gebruikt de functie bewust een EN-filter: een bestand moet dus zowel aan de
#' regex voldoen als in `welke` staan. Dit maakt gecombineerde selecties
#' voorspelbaar, bijvoorbeeld `welke = c("A.csv", "B.txt")` samen met
#' `pattern = "\\.csv$"` downloadt alleen `"A.csv"`.
#'
#' Twijfelpunt voor later: bij `recursive = TRUE` kan dezelfde bestandsnaam in
#' meerdere submappen voorkomen. Omdat `welke` op bestandsnaam filtert, worden
#' dan alle bestanden met die naam gedownload. Als onderscheid op pad nodig
#' blijkt, kan later een apart padgericht selectieargument worden toegevoegd.
#'
#' @section Foutafhandeling:
#' Als het ophalen van de mapinhoud mislukt (bijv. map bestaat niet of geen
#' leesrechten), gooit de functie een fout. Individuele bestandsfouten worden
#' afgevangen zodat de download doorgaat; mislukte bestanden worden aan het
#' eind gemeld via [warning()].
#' Een ongeldig `pattern`- of `welke`-argument geeft een fout voordat er
#' downloads starten.
#'
#' @section Authenticatie:
#' Het token wordt standaard automatisch opgehaald via [RLdrop::dropbox_token()].
#' Zie [RLdrop::dropbox_token()] voor het instellen van de benodigde omgevingsvariabelen.
#'
#' @seealso
#' [RLdrop::dropbox_download_file()] voor het downloaden van een enkel bestand. \cr
#' [RLdrop::dropbox_compare_folder()] om een lokale map te vergelijken met Dropbox.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Platte map downloaden (geen submappen)
#' dropbox_download_folder(
#'   dropbox_folder = "/data/parquet",
#'   local_folder   = "D:/lokaal/parquet"
#' )
#'
#' # Inclusief alle submappen
#' dropbox_download_folder(
#'   dropbox_folder = "/projecten/2026",
#'   local_folder   = "D:/lokaal/projecten/2026",
#'   recursive      = TRUE
#' )
#'
#' # Alleen ontbrekende bestanden downloaden
#' dropbox_download_folder(
#'   dropbox_folder = "/data/parquet",
#'   local_folder   = "D:/lokaal/parquet",
#'   recursive      = TRUE,
#'   overwrite      = "missing"
#' )
#'
#' # Alleen gewijzigde bestanden (grootte verschilt)
#' dropbox_download_folder(
#'   dropbox_folder = "/data/parquet",
#'   local_folder   = "D:/lokaal/parquet",
#'   recursive      = TRUE,
#'   overwrite      = "changed"
#' )
#'
#' # Alleen CSV-bestanden direct in de map downloaden
#' dropbox_download_folder(
#'   dropbox_folder = "/datasets/EODHD",
#'   local_folder   = "D:/lokaal/EODHD",
#'   pattern        = "\\.csv$"
#' )
#'
#' # Alleen expliciet genoemde bestanden downloaden
#' dropbox_download_folder(
#'   dropbox_folder = "/datasets/EODHD",
#'   local_folder   = "D:/lokaal/EODHD",
#'   welke          = c("A.txt", "B.txt", "C.R")
#' )
#' }
dropbox_download_folder <- function(dropbox_folder, local_folder, token = dropbox_token(),
                                     recursive = FALSE, overwrite = "newer",
                                     pattern = NULL, welke = NULL) {

  overwrite <- match.arg(overwrite, choices = c("always", "missing", "changed", "newer"))

  selectie <- .validate_file_selection(pattern = pattern, welke = welke)
  pattern  <- selectie$pattern
  welke    <- selectie$welke

  # --- Fase 1: inventariseer alle items ---
  entries <- .list_folder_recursive(dropbox_folder, token, recursive)

  file_entries   <- Filter(function(e) e[[".tag"]] == "file",   entries)
  folder_entries <- Filter(function(e) e[[".tag"]] == "folder", entries)

  # [ARCH] Selectiefiltering gebeurt NA het listen maar VOOR lokale mappen en
  # downloads worden aangemaakt. Zo blijft de Dropbox API-call simpel en
  # betrouwbaar, terwijl lokaal alleen werk wordt gedaan voor bestanden die
  # daadwerkelijk binnen de selectie vallen.
  #
  # Keuze: filter op e$name in plaats van path_display. Dit voorkomt dat een
  # mapnaam per ongeluk alle bestanden in die map selecteert. Voor de EODHD-
  # vraag ("alleen files in deze folder, eventueel op extensie/naam of expliciete
  # bestandslijst") is dat de veiligste standaard. Zie de roxygen-details voor
  # het expliciete twijfelpunt rond mogelijk toekomstige padgebaseerde filtering.
  file_entries <- .filter_dropbox_file_entries(
    file_entries = file_entries,
    pattern      = pattern,
    welke        = welke,
    context_path = dropbox_folder
  )

  # [ARCH] Als recursive = TRUE is, hoeven alleen mappen te worden aangemaakt
  # die daadwerkelijk een geselecteerd bestand bevatten. Anders zou een brede
  # recursieve listing alsnog lege submappen lokaal neerzetten.
  if (recursive && length(folder_entries) > 0L && length(file_entries) > 0L) {
    matched_file_paths <- vapply(file_entries, function(e) e$path_lower, character(1L))
    folder_entries <- Filter(
      function(e) any(startsWith(matched_file_paths, paste0(e$path_lower, "/"))),
      folder_entries
    )
  } else if (recursive && length(file_entries) == 0L) {
    folder_entries <- list()
  }

  n_files <- length(file_entries)
  filter_omschrijving <- .describe_file_selection(pattern = pattern, welke = welke)

  if (n_files == 0) {
    if (identical(filter_omschrijving, "")) {
      message(sprintf("[DROPBOX] Geen bestanden gevonden in %s", dropbox_folder))
    } else {
      message(sprintf(
        "[DROPBOX] Geen bestanden gevonden in %s die voldoen aan %s",
        dropbox_folder,
        filter_omschrijving
      ))
    }
    return(invisible(character(0)))
  }

  if (identical(filter_omschrijving, "")) {
    message(sprintf("[DROPBOX] %d bestand(en) gevonden, downloaden gestart...", n_files))
  } else {
    message(sprintf(
      "[DROPBOX] %d bestand(en) gevonden voor %s, downloaden gestart...",
      n_files,
      filter_omschrijving
    ))
  }

  root_prefix_lower <- tolower(sub("/$", "", dropbox_folder))

  # Maak lokale hoofdmap aan
  local_folder_long <- .win_long_path(local_folder)
  if (!dir.exists(local_folder_long)) dir.create(local_folder_long, recursive = TRUE)

  # Maak alle submappen aan voor het downloaden begint
  if (recursive && length(folder_entries) > 0) {
    for (folder in folder_entries) {
      rel_path  <- substring(folder$path_lower, nchar(root_prefix_lower) + 1)
      sub_local <- paste0(local_folder, gsub("/", .Platform$file.sep, rel_path))
      sub_long  <- .win_long_path(sub_local)
      if (!dir.exists(sub_long)) dir.create(sub_long, recursive = TRUE)
    }
  }

  # --- Fase 2: download met voortgangsbalk ---
  pb         <- utils::txtProgressBar(min = 0, max = n_files, style = 3)
  downloaded <- character(0)
  skipped    <- 0L
  failed     <- character(0)

  for (i in seq_along(file_entries)) {
    entry <- file_entries[[i]]

    if (recursive) {
      rel_path   <- substring(entry$path_lower, nchar(root_prefix_lower) + 1)
      local_path <- paste0(local_folder, gsub("/", .Platform$file.sep, rel_path))
    } else {
      local_path <- file.path(local_folder, basename(entry$path_lower))
    }

    # --- Overwrite-logica: controleer of we dit bestand moeten overslaan ---
    if (overwrite != "always") {
      local_long <- .win_long_path(local_path)
      if (file.exists(local_long)) {
        skip <- FALSE

        if (overwrite == "missing") {
          skip <- TRUE
        } else if (overwrite == "changed") {
          local_size <- file.info(local_long)$size
          skip <- !is.na(local_size) && local_size == entry$size
        } else if (overwrite == "newer") {
          local_mtime <- file.info(local_long)$mtime
          db_mtime    <- as.POSIXct(entry$client_modified,
                                     format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
          skip <- !is.na(local_mtime) && !is.na(db_mtime) && db_mtime <= local_mtime
        }

        if (skip) {
          skipped <- skipped + 1L
          utils::setTxtProgressBar(pb, i)
          next
        }
      }
    }

    # --- Download met error handling ---
    tryCatch({
      dropbox_download_file(entry$path_display, local_path, token)
      downloaded <- c(downloaded, local_path)
    }, error = function(e) {
      failed <<- c(failed, sprintf("  %s\n    %s", entry$path_display, conditionMessage(e)))
    })

    utils::setTxtProgressBar(pb, i)
  }

  close(pb)

  # --- Fase 3: samenvatting ---
  message(sprintf(
    "\n[DROPBOX] Klaar: %d gedownload, %d overgeslagen, %d mislukt (van %d totaal)",
    length(downloaded), skipped, length(failed), n_files
  ))

  if (length(failed) > 0) {
    warning(sprintf(
      "[DROPBOX] %d bestand(en) niet gedownload:\n%s",
      length(failed), paste(failed, collapse = "\n")
    ), call. = FALSE)
  }

  invisible(downloaded)
}
