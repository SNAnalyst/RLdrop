# vergelijk_alles.R
# Vergelijkt alle top-level mappen in D:/Dropbox met Dropbox online.
# Slaat resultaten op als RDS-bestand voor latere inspectie.
# Gebruik: source("scripts/vergelijk_alles.R")



vergelijk_dropbox_volledig <- function(lokaal_root = "D:/Dropbox",
                                       uitvoer_bestand = "D:/Dropbox/vergelijk_resultaat.rds",
                                       recursive = TRUE,
                                       compare_date = FALSE,
                                       skip_mappen = c(".dropbox.cache")) {

  mappen <- list.dirs(lokaal_root, full.names = FALSE, recursive = FALSE)
  mappen <- setdiff(mappen, skip_mappen)

  message(sprintf("[START] %d mappen gevonden in %s", length(mappen), lokaal_root))
  message(sprintf("[START] Resultaat wordt opgeslagen in: %s", uitvoer_bestand))

  resultaten <- list()

  for (i in seq_along(mappen)) {
    map <- mappen[i]
    lokaal_pad  <- file.path(lokaal_root, map)
    dropbox_pad <- paste0("/", map)

    message(sprintf("\n[%d/%d] Vergelijken: %s", i, length(mappen), map))

    resultaten[[map]] <- tryCatch(
      RLdrop::dropbox_compare_folder(
        local_folder  = lokaal_pad,
        dropbox_folder = dropbox_pad,
        recursive      = recursive,
        compare_date   = compare_date
      ),
      error = function(e) {
        message(sprintf("  FOUT: %s", conditionMessage(e)))
        data.frame(
          naam = NA_character_, status = "FOUT",
          lokaal_grootte = NA_real_, dropbox_grootte = NA_real_,
          lokaal_datum = as.POSIXct(NA), dropbox_datum = as.POSIXct(NA),
          foutmelding = conditionMessage(e),
          stringsAsFactors = FALSE
        )
      }
    )

    saveRDS(resultaten, uitvoer_bestand)
    message(sprintf("  [opgeslagen na %d/%d mappen]", i, length(mappen)))
  }

  message(sprintf("\n[KLAAR] Alle %d mappen verwerkt.", length(mappen)))
  message(sprintf("[KLAAR] Resultaat: %s", uitvoer_bestand))

  invisible(resultaten)
}

# --- Direct uitvoeren ---
resultaten <- vergelijk_dropbox_volledig()
