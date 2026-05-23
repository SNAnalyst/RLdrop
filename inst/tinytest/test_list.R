# test_list.R
# Tests voor dropbox_list_folder()
# Werkt met gemockte Dropbox-responses zodat er geen echte API-aanroepen nodig zijn.


# --- Gedeelde mock entries in Dropbox API-formaat ---
mock_entries <- list(
  list(.tag = "file", name = "rapport.xlsx", path_lower = "/data/rapport.xlsx",
       path_display = "/data/rapport.xlsx", size = 2048,
       client_modified = "2025-06-10T09:30:00Z"),
  list(.tag = "file", name = "data.csv", path_lower = "/data/data.csv",
       path_display = "/data/data.csv", size = 51200,
       client_modified = "2025-03-01T14:00:00Z"),
  list(.tag = "folder", name = "archief", path_lower = "/data/archief",
       path_display = "/data/archief"),
  list(.tag = "file", name = "notes.txt", path_lower = "/data/archief/notes.txt",
       path_display = "/data/archief/notes.txt", size = 128,
       client_modified = "2024-12-25T00:00:00Z")
)


# --- Helper: mock installeren en opruimen ---
# Installeert een mock voor .list_folder_recursive en retourneert een
# restore-functie die de originele terugzet.
install_mock <- function(mock_data) {
  orig_fn <- RLdrop:::.list_folder_recursive
  assignInNamespace(
    ".list_folder_recursive",
    function(dropbox_folder, token, recursive) mock_data,
    "RLdrop"
  )
  orig_fn
}

restore_mock <- function(orig_fn) {
  assignInNamespace(".list_folder_recursive", orig_fn, "RLdrop")
}


# --- Test 1: ongeldige type parameter → fout ---
expect_error(
  dropbox_list_folder("/test", token = "dummy", type = "ongeldig"),
  pattern = "arg"
)


# --- Test 2: lege map → leeg data.frame met juiste kolommen ---
local({
  orig <- install_mock(list())
  on.exit(restore_mock(orig))

  result <- dropbox_list_folder("/leeg", token = "dummy")

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0L)
  expect_true(all(c("naam", "pad", "type", "grootte", "gewijzigd") %in% names(result)))
})


# --- Test 3: alle items retourneren (type = "all") ---
local({
  orig <- install_mock(mock_entries)
  on.exit(restore_mock(orig))

  result <- dropbox_list_folder("/data", token = "dummy")

  expect_equal(nrow(result), 4L)
  expect_true("file" %in% result$type)
  expect_true("folder" %in% result$type)
})


# --- Test 4: alleen bestanden (type = "file") ---
local({
  orig <- install_mock(mock_entries)
  on.exit(restore_mock(orig))

  result <- dropbox_list_folder("/data", token = "dummy", type = "file")

  expect_equal(nrow(result), 3L)
  expect_true(all(result$type == "file"))
  # [ARCH] Alle bestanden moeten grootte en datum hebben
  expect_true(all(!is.na(result$grootte)))
  expect_true(all(!is.na(result$gewijzigd)))
})


# --- Test 5: alleen mappen (type = "folder") ---
local({
  orig <- install_mock(mock_entries)
  on.exit(restore_mock(orig))

  result <- dropbox_list_folder("/data", token = "dummy", type = "folder")

  expect_equal(nrow(result), 1L)
  expect_equal(result$naam, "archief")
  # [ARCH] Mappen hebben geen grootte of datum
  expect_true(is.na(result$grootte))
  expect_true(is.na(result$gewijzigd))
})


# --- Test 6: grootte en datum kloppen ---
local({
  orig <- install_mock(mock_entries)
  on.exit(restore_mock(orig))

  result <- dropbox_list_folder("/data", token = "dummy", type = "file")

  # [ARCH] Zoek het bekende bestand op via naam
  csv_row <- result[result$naam == "data.csv", ]
  expect_equal(csv_row$grootte, 51200)
  expect_equal(
    format(csv_row$gewijzigd, "%Y-%m-%d", tz = "UTC"),
    "2025-03-01"
  )
})


# --- Test 7: resultaat is gesorteerd op pad ---
local({
  orig <- install_mock(mock_entries)
  on.exit(restore_mock(orig))

  result <- dropbox_list_folder("/data", token = "dummy")

  # [ARCH] Controleer dat paden in oplopende volgorde staan
  expect_equal(result$pad, sort(result$pad))
})


# --- Test 8: leeg resultaat na type-filter → leeg data.frame ---
local({
  # [ARCH] Alleen bestanden, geen mappen
  files_only <- list(
    list(.tag = "file", name = "a.txt", path_lower = "/x/a.txt",
         path_display = "/x/a.txt", size = 10,
         client_modified = "2025-01-01T00:00:00Z")
  )
  orig <- install_mock(files_only)
  on.exit(restore_mock(orig))

  result <- dropbox_list_folder("/x", token = "dummy", type = "folder")

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0L)
})
