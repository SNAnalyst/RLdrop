# selection.R
# Interne helpers voor bestandsselectie op naam.
# Deze helpers zijn bewust niet geexporteerd: ze bestaan om dezelfde
# pattern/welke-semantiek consequent te houden bij download, upload en delete.


# [ARCH] Valideer de gedeelde selectieargumenten voor functies die een set
# bestanden verwerken. We gebruiken uitsluitend base R:
# - pattern is een enkele reguliere expressie voor grepl();
# - welke is een exacte allowlist van bestandsnamen.
#
# Belangrijke keuze: beide filters werken op de bestandsnaam, niet op het pad.
# Dat voorkomt dat een mapnaam per ongeluk alle bestanden in die map selecteert.
# Als later padselectie nodig is, voeg dan liever een expliciet scope-argument
# toe dan de betekenis van deze bestaande argumenten te veranderen.
.validate_file_selection <- function(pattern = NULL, welke = NULL) {

  if (!is.null(pattern)) {
    if (!is.character(pattern) || length(pattern) != 1L || is.na(pattern)) {
      stop("[DROPBOX] 'pattern' moet NULL zijn of een character string van lengte 1.")
    }

    pattern_valid <- tryCatch(
      {
        suppressWarnings(grepl(pattern, ""))
        TRUE
      },
      error = function(e) FALSE
    )

    if (!pattern_valid) {
      stop("[DROPBOX] 'pattern' moet een geldige reguliere expressie zijn.")
    }
  }

  if (!is.null(welke)) {
    if (!is.character(welke) || any(is.na(welke))) {
      stop("[DROPBOX] 'welke' moet NULL zijn of een character vector zonder NA-waarden.")
    }

    welke <- unique(welke)
  }

  list(pattern = pattern, welke = welke)
}


# [ARCH] Maak een korte mensleesbare omschrijving voor voortgangsberichten.
# Een lege string betekent: er is geen selectie actief.
.describe_file_selection <- function(pattern = NULL, welke = NULL) {

  parts <- character(0)

  if (!is.null(pattern)) {
    parts <- c(parts, sprintf("pattern '%s'", pattern))
  }

  if (!is.null(welke)) {
    parts <- c(
      parts,
      sprintf("welke = c(%s)", paste(sprintf("'%s'", welke), collapse = ", "))
    )
  }

  paste(parts, collapse = " en ")
}


# [ARCH] Filter Dropbox API entries op e$name. De functie verwacht alleen
# bestandsentries, geen mappen. `context_path` wordt alleen gebruikt voor een
# informatief bericht over namen uit `welke` die niet zijn gevonden.
.filter_dropbox_file_entries <- function(file_entries, pattern = NULL, welke = NULL,
                                         context_path = "") {

  original_file_entries <- file_entries

  if (!is.null(pattern)) {
    file_entries <- Filter(function(e) grepl(pattern, e$name), file_entries)
  }

  if (!is.null(welke)) {
    all_file_names <- vapply(original_file_entries, function(e) e$name, character(1L))
    ontbrekend <- setdiff(welke, unique(all_file_names))

    if (length(ontbrekend) > 0L) {
      message(sprintf(
        "[DROPBOX] Niet gevonden in %s: %s",
        context_path,
        paste(ontbrekend, collapse = ", ")
      ))
    }

    file_entries <- Filter(function(e) e$name %in% welke, file_entries)
  }

  file_entries
}


# [ARCH] Filter lokale bestanden op basename(). Dit is de lokale tegenhanger van
# .filter_dropbox_file_entries() en wordt gebruikt bij upload_folder().
.filter_local_files <- function(files, pattern = NULL, welke = NULL,
                                context_path = "") {

  original_files <- files
  file_names <- basename(files)

  if (!is.null(pattern)) {
    files <- files[grepl(pattern, file_names)]
    file_names <- basename(files)
  }

  if (!is.null(welke)) {
    all_file_names <- basename(original_files)
    ontbrekend <- setdiff(welke, unique(all_file_names))

    if (length(ontbrekend) > 0L) {
      message(sprintf(
        "[DROPBOX] Niet gevonden in %s: %s",
        context_path,
        paste(ontbrekend, collapse = ", ")
      ))
    }

    files <- files[file_names %in% welke]
  }

  files
}
