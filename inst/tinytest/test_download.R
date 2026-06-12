# test_download.R
# Tests voor dropbox_download_folder().
# Deze tests gebruiken gemockte Dropbox-responses en een gemockte downloadfunctie.
# Daardoor wordt er geen echte Dropbox API aangeroepen en blijven de tests snel,
# herhaalbaar en geschikt voor offline package-checks.


# --- Helper: mock entries in Dropbox API-formaat ---
# De Dropbox API gebruikt ".tag" om onderscheid te maken tussen bestanden en
# mappen. Voor download_folder() zijn vooral name, path_lower en path_display
# belangrijk:
# - name wordt gebruikt voor pattern-filtering;
# - path_lower wordt gebruikt om lokale relatieve paden stabiel te berekenen;
# - path_display wordt doorgegeven aan dropbox_download_file().
mock_download_entries <- list(
  list(.tag = "file", name = "EOD_2026.csv",
       path_lower = "/datasets/eodhd/eod_2026.csv",
       path_display = "/datasets/EODHD/EOD_2026.csv",
       size = 100),
  list(.tag = "file", name = "EOD_2026.parquet",
       path_lower = "/datasets/eodhd/eod_2026.parquet",
       path_display = "/datasets/EODHD/EOD_2026.parquet",
       size = 200),
  list(.tag = "file", name = "notes.txt",
       path_lower = "/datasets/eodhd/notes.txt",
       path_display = "/datasets/EODHD/notes.txt",
       size = 50),
  list(.tag = "folder", name = "archive",
       path_lower = "/datasets/eodhd/archive",
       path_display = "/datasets/EODHD/archive")
)

mock_recursive_entries <- c(
  mock_download_entries,
  list(
    list(.tag = "folder", name = "year=2026",
         path_lower = "/datasets/eodhd/year=2026",
         path_display = "/datasets/EODHD/year=2026"),
    list(.tag = "folder", name = "skip",
         path_lower = "/datasets/eodhd/skip",
         path_display = "/datasets/EODHD/skip"),
    list(.tag = "file", name = "prices_2026.csv",
         path_lower = "/datasets/eodhd/year=2026/prices_2026.csv",
         path_display = "/datasets/EODHD/year=2026/prices_2026.csv",
         size = 300),
    list(.tag = "file", name = "prices_2025.csv",
         path_lower = "/datasets/eodhd/skip/prices_2025.csv",
         path_display = "/datasets/EODHD/skip/prices_2025.csv",
         size = 400)
  )
)


# --- Helper: installeer mocks en geef een restore-functie terug ---
# We mocken twee functies in de RLdrop namespace:
# - .list_folder_recursive(): levert vaste API-entries terug;
# - dropbox_download_file(): registreert welke bestanden zouden zijn
#   gedownload en schrijft een klein dummy-bestand weg naar local_path.
#
# De dummy-write is nuttig omdat dropbox_download_folder() ook paden aanmaakt;
# zo controleren we meteen dat lokale paden realistisch bruikbaar zijn.
install_download_folder_mocks <- function(entries) {
  original_list     <- RLdrop:::.list_folder_recursive
  original_download <- RLdrop::dropbox_download_file

  calls <- new.env(parent = emptyenv())
  calls$dropbox_folder <- character(0)
  calls$recursive      <- logical(0)
  calls$remote         <- character(0)
  calls$local          <- character(0)
  calls$token          <- character(0)

  assignInNamespace(
    ".list_folder_recursive",
    function(dropbox_folder, token, recursive) {
      calls$dropbox_folder <- c(calls$dropbox_folder, dropbox_folder)
      calls$recursive      <- c(calls$recursive, recursive)
      calls$token          <- c(calls$token, token)
      entries
    },
    "RLdrop"
  )

  assignInNamespace(
    "dropbox_download_file",
    function(dropbox_path, local_path, token) {
      calls$remote <- c(calls$remote, dropbox_path)
      calls$local  <- c(calls$local, local_path)
      calls$token  <- c(calls$token, token)

      dir.create(dirname(local_path), recursive = TRUE, showWarnings = FALSE)
      writeLines("dummy Dropbox payload", local_path)
      invisible(local_path)
    },
    "RLdrop"
  )

  list(
    calls = calls,
    restore = function() {
      assignInNamespace(".list_folder_recursive", original_list, "RLdrop")
      assignInNamespace("dropbox_download_file", original_download, "RLdrop")
      invisible(TRUE)
    }
  )
}


# --- Test 1: pattern moet NULL of een character string van lengte 1 zijn ---
# [ARCH] tinytest expectations staan bewust zonder tinytest:: prefix. De
# tinytest-runner verzamelt resultaten alleen via deze standaardvorm; met een
# namespace-prefix worden de checks uitgevoerd, maar niet als testresultaat
# geregistreerd.
expect_error(
  RLdrop::dropbox_download_folder(
    "/datasets/EODHD",
    tempfile("download_invalid_pattern_"),
    token = "dummy",
    pattern = c("\\.csv$", "\\.parquet$")
  ),
  pattern = "pattern"
)

expect_error(
  RLdrop::dropbox_download_folder(
    "/datasets/EODHD",
    tempfile("download_invalid_pattern_"),
    token = "dummy",
    pattern = NA_character_
  ),
  pattern = "pattern"
)


# --- Test 2: zonder pattern worden alle directe bestanden gedownload ---
local({
  mocks <- install_download_folder_mocks(mock_download_entries)
  on.exit(mocks$restore())

  local_folder <- tempfile("download_all_")
  on.exit(unlink(local_folder, recursive = TRUE), add = TRUE)

  downloaded <- RLdrop::dropbox_download_folder(
    "/datasets/EODHD",
    local_folder,
    token = "dummy",
    recursive = FALSE
  )

  expect_equal(length(downloaded), 3L)
  expect_equal(length(mocks$calls$remote), 3L)
  expect_true(all(basename(downloaded) %in% c(
    "eod_2026.csv",
    "eod_2026.parquet",
    "notes.txt"
  )))
  expect_equal(mocks$calls$recursive, FALSE)
})


# --- Test 3: pattern filtert op bestandsnaam en downloadt alleen matches ---
local({
  mocks <- install_download_folder_mocks(mock_download_entries)
  on.exit(mocks$restore())

  local_folder <- tempfile("download_csv_")
  on.exit(unlink(local_folder, recursive = TRUE), add = TRUE)

  downloaded <- RLdrop::dropbox_download_folder(
    "/datasets/EODHD",
    local_folder,
    token = "dummy",
    pattern = "\\.csv$"
  )

  expect_equal(length(downloaded), 1L)
  expect_equal(basename(downloaded), "eod_2026.csv")
  expect_equal(mocks$calls$remote, "/datasets/EODHD/EOD_2026.csv")
})


# --- Test 4: pattern gebruikt base R regex en is hoofdlettergevoelig ---
local({
  mocks <- install_download_folder_mocks(mock_download_entries)
  on.exit(mocks$restore())

  local_folder <- tempfile("download_case_sensitive_")
  on.exit(unlink(local_folder, recursive = TRUE), add = TRUE)

  downloaded <- RLdrop::dropbox_download_folder(
    "/datasets/EODHD",
    local_folder,
    token = "dummy",
    pattern = "\\.CSV$"
  )

  expect_equal(length(downloaded), 0L)
  expect_equal(length(mocks$calls$remote), 0L)
})


# --- Test 5: pattern kan op een naamdeel matchen, niet alleen op extensie ---
local({
  mocks <- install_download_folder_mocks(mock_download_entries)
  on.exit(mocks$restore())

  local_folder <- tempfile("download_name_part_")
  on.exit(unlink(local_folder, recursive = TRUE), add = TRUE)

  downloaded <- RLdrop::dropbox_download_folder(
    "/datasets/EODHD",
    local_folder,
    token = "dummy",
    pattern = "^EOD_2026"
  )

  expect_equal(length(downloaded), 2L)
  expect_true(all(basename(downloaded) %in% c(
    "eod_2026.csv",
    "eod_2026.parquet"
  )))
})


# --- Test 6: geen match betekent geen lokale map en geen download-call ---
local({
  mocks <- install_download_folder_mocks(mock_download_entries)
  on.exit(mocks$restore())

  local_folder <- tempfile("download_no_match_")
  on.exit(unlink(local_folder, recursive = TRUE), add = TRUE)

  downloaded <- RLdrop::dropbox_download_folder(
    "/datasets/EODHD",
    local_folder,
    token = "dummy",
    pattern = "\\.xlsx$"
  )

  expect_equal(downloaded, character(0))
  expect_equal(length(mocks$calls$remote), 0L)
  expect_true(!dir.exists(local_folder))
})


# --- Test 7: bij recursive = TRUE blijft de relatieve submapstructuur intact ---
local({
  mocks <- install_download_folder_mocks(mock_recursive_entries)
  on.exit(mocks$restore())

  local_folder <- tempfile("download_recursive_pattern_")
  on.exit(unlink(local_folder, recursive = TRUE), add = TRUE)

  downloaded <- RLdrop::dropbox_download_folder(
    "/datasets/EODHD",
    local_folder,
    token = "dummy",
    recursive = TRUE,
    pattern = "prices_2026\\.csv$"
  )

  expected <- file.path(local_folder, "year=2026", "prices_2026.csv")

  expect_equal(downloaded, expected)
  expect_equal(mocks$calls$remote, "/datasets/EODHD/year=2026/prices_2026.csv")
  expect_true(file.exists(expected))
})


# --- Test 8: bij recursive pattern worden niet-gematchte submappen niet gemaakt ---
local({
  mocks <- install_download_folder_mocks(mock_recursive_entries)
  on.exit(mocks$restore())

  local_folder <- tempfile("download_recursive_no_empty_dirs_")
  on.exit(unlink(local_folder, recursive = TRUE), add = TRUE)

  RLdrop::dropbox_download_folder(
    "/datasets/EODHD",
    local_folder,
    token = "dummy",
    recursive = TRUE,
    pattern = "prices_2026\\.csv$"
  )

  expect_true(dir.exists(file.path(local_folder, "year=2026")))
  expect_true(!dir.exists(file.path(local_folder, "skip")))
})


# --- Test 9: pattern wordt toegepast op name, niet op het volledige pad ---
local({
  entries <- list(
    list(.tag = "folder", name = "csv-folder",
         path_lower = "/datasets/eodhd/csv-folder",
         path_display = "/datasets/EODHD/csv-folder"),
    list(.tag = "file", name = "prices.txt",
         path_lower = "/datasets/eodhd/csv-folder/prices.txt",
         path_display = "/datasets/EODHD/csv-folder/prices.txt",
         size = 10)
  )

  mocks <- install_download_folder_mocks(entries)
  on.exit(mocks$restore())

  local_folder <- tempfile("download_pattern_name_only_")
  on.exit(unlink(local_folder, recursive = TRUE), add = TRUE)

  downloaded <- RLdrop::dropbox_download_folder(
    "/datasets/EODHD",
    local_folder,
    token = "dummy",
    recursive = TRUE,
    pattern = "csv-folder"
  )

  expect_equal(downloaded, character(0))
  expect_equal(length(mocks$calls$remote), 0L)
})


# --- Test 10: ongeldig regex-patroon faalt voordat er download-calls zijn ---
local({
  mocks <- install_download_folder_mocks(mock_download_entries)
  on.exit(mocks$restore())

  expect_error(
    RLdrop::dropbox_download_folder(
      "/datasets/EODHD",
      tempfile("download_bad_regex_"),
      token = "dummy",
      pattern = "["
    )
  )

  expect_equal(length(mocks$calls$remote), 0L)
})


# --- Test 11: welke moet NULL of een character vector zonder NA zijn ---
expect_error(
  RLdrop::dropbox_download_folder(
    "/datasets/EODHD",
    tempfile("download_invalid_welke_"),
    token = "dummy",
    welke = 1:3
  ),
  pattern = "welke"
)

expect_error(
  RLdrop::dropbox_download_folder(
    "/datasets/EODHD",
    tempfile("download_invalid_welke_"),
    token = "dummy",
    welke = c("A.txt", NA_character_)
  ),
  pattern = "welke"
)


# --- Test 12: welke downloadt alleen exact genoemde bestandsnamen ---
local({
  mocks <- install_download_folder_mocks(mock_download_entries)
  on.exit(mocks$restore())

  local_folder <- tempfile("download_welke_")
  on.exit(unlink(local_folder, recursive = TRUE), add = TRUE)

  downloaded <- RLdrop::dropbox_download_folder(
    "/datasets/EODHD",
    local_folder,
    token = "dummy",
    welke = c("EOD_2026.csv", "notes.txt")
  )

  expect_equal(length(downloaded), 2L)
  expect_equal(mocks$calls$remote, c(
    "/datasets/EODHD/EOD_2026.csv",
    "/datasets/EODHD/notes.txt"
  ))
  expect_true(all(basename(downloaded) %in% c("eod_2026.csv", "notes.txt")))
})


# --- Test 13: welke is exact en hoofdlettergevoelig ---
local({
  mocks <- install_download_folder_mocks(mock_download_entries)
  on.exit(mocks$restore())

  local_folder <- tempfile("download_welke_case_sensitive_")
  on.exit(unlink(local_folder, recursive = TRUE), add = TRUE)

  downloaded <- RLdrop::dropbox_download_folder(
    "/datasets/EODHD",
    local_folder,
    token = "dummy",
    welke = "eod_2026.csv"
  )

  expect_equal(downloaded, character(0))
  expect_equal(length(mocks$calls$remote), 0L)
})


# --- Test 14: pattern en welke vormen samen een EN-filter ---
local({
  mocks <- install_download_folder_mocks(mock_download_entries)
  on.exit(mocks$restore())

  local_folder <- tempfile("download_pattern_and_welke_")
  on.exit(unlink(local_folder, recursive = TRUE), add = TRUE)

  downloaded <- RLdrop::dropbox_download_folder(
    "/datasets/EODHD",
    local_folder,
    token = "dummy",
    pattern = "\\.csv$",
    welke = c("EOD_2026.csv", "EOD_2026.parquet")
  )

  expect_equal(length(downloaded), 1L)
  expect_equal(mocks$calls$remote, "/datasets/EODHD/EOD_2026.csv")
})


# --- Test 15: ontbrekende namen in welke worden gemeld en gevonden namen gaan door ---
local({
  mocks <- install_download_folder_mocks(mock_download_entries)
  on.exit(mocks$restore())

  local_folder <- tempfile("download_welke_missing_")
  on.exit(unlink(local_folder, recursive = TRUE), add = TRUE)

  messages <- utils::capture.output(
    downloaded <- RLdrop::dropbox_download_folder(
      "/datasets/EODHD",
      local_folder,
      token = "dummy",
      welke = c("EOD_2026.csv", "missing.txt")
    ),
    type = "message"
  )

  expect_equal(length(downloaded), 1L)
  expect_equal(mocks$calls$remote, "/datasets/EODHD/EOD_2026.csv")
  expect_true(any(grepl("missing.txt", messages)))
})


# --- Test 16: duplicaten in welke veroorzaken geen dubbele download ---
local({
  mocks <- install_download_folder_mocks(mock_download_entries)
  on.exit(mocks$restore())

  local_folder <- tempfile("download_welke_duplicates_")
  on.exit(unlink(local_folder, recursive = TRUE), add = TRUE)

  downloaded <- RLdrop::dropbox_download_folder(
    "/datasets/EODHD",
    local_folder,
    token = "dummy",
    welke = c("notes.txt", "notes.txt")
  )

  expect_equal(length(downloaded), 1L)
  expect_equal(length(mocks$calls$remote), 1L)
  expect_equal(mocks$calls$remote, "/datasets/EODHD/notes.txt")
})


# --- Test 17: bij recursive = TRUE selecteert welke dezelfde naam in meerdere submappen ---
local({
  entries <- list(
    list(.tag = "folder", name = "a",
         path_lower = "/datasets/eodhd/a",
         path_display = "/datasets/EODHD/a"),
    list(.tag = "folder", name = "b",
         path_lower = "/datasets/eodhd/b",
         path_display = "/datasets/EODHD/b"),
    list(.tag = "file", name = "same.txt",
         path_lower = "/datasets/eodhd/a/same.txt",
         path_display = "/datasets/EODHD/a/same.txt",
         size = 10),
    list(.tag = "file", name = "same.txt",
         path_lower = "/datasets/eodhd/b/same.txt",
         path_display = "/datasets/EODHD/b/same.txt",
         size = 20),
    list(.tag = "file", name = "other.txt",
         path_lower = "/datasets/eodhd/b/other.txt",
         path_display = "/datasets/EODHD/b/other.txt",
         size = 30)
  )

  mocks <- install_download_folder_mocks(entries)
  on.exit(mocks$restore())

  local_folder <- tempfile("download_welke_recursive_duplicates_")
  on.exit(unlink(local_folder, recursive = TRUE), add = TRUE)

  downloaded <- RLdrop::dropbox_download_folder(
    "/datasets/EODHD",
    local_folder,
    token = "dummy",
    recursive = TRUE,
    welke = "same.txt"
  )

  expect_equal(length(downloaded), 2L)
  expect_equal(mocks$calls$remote, c(
    "/datasets/EODHD/a/same.txt",
    "/datasets/EODHD/b/same.txt"
  ))
  expect_true(file.exists(file.path(local_folder, "a", "same.txt")))
  expect_true(file.exists(file.path(local_folder, "b", "same.txt")))
  expect_true(!file.exists(file.path(local_folder, "b", "other.txt")))
})


# --- Test 18: lege welke-vector is een expliciete selectie van nul bestanden ---
local({
  mocks <- install_download_folder_mocks(mock_download_entries)
  on.exit(mocks$restore())

  local_folder <- tempfile("download_welke_empty_")
  on.exit(unlink(local_folder, recursive = TRUE), add = TRUE)

  downloaded <- RLdrop::dropbox_download_folder(
    "/datasets/EODHD",
    local_folder,
    token = "dummy",
    welke = character(0)
  )

  expect_equal(downloaded, character(0))
  expect_equal(length(mocks$calls$remote), 0L)
  expect_true(!dir.exists(local_folder))
})
