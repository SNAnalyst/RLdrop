# test_compare.R
# Tests voor dropbox_compare_folder()
# Werkt met lokale dummy-data en gemockte Dropbox-responses zodat er geen
# echte API-aanroepen nodig zijn.


# --- Helper: maak tijdelijke mapstructuur met bestanden ---
# Maakt een temp-map met opgegeven bestanden (naam + inhoud).
# Retourneert het pad naar de aangemaakte map.
setup_local_folder <- function(files = list()) {
  tmp <- tempfile("compare_test_")
  dir.create(tmp, recursive = TRUE)
  for (nm in names(files)) {
    full_path <- file.path(tmp, nm)
    sub_dir   <- dirname(full_path)
    if (!dir.exists(sub_dir)) dir.create(sub_dir, recursive = TRUE)
    writeLines(files[[nm]], full_path)
  }
  tmp
}


# --- Test 1: fout bij niet-bestaande lokale map ---
expect_error(
  dropbox_compare_folder("/niet/bestaand/pad", "/dropbox/pad", token = "dummy"),
  pattern = "Lokale map niet gevonden"
)


# --- Test 2: lege mappen → leeg data.frame met juiste kolommen ---
# [ARCH] Mock .list_folder_recursive om lege entries te retourneren
local({
  tmp <- setup_local_folder()
  on.exit(unlink(tmp, recursive = TRUE))

  # Tijdelijke mock: vervang .list_folder_recursive met een versie die lege lijst retourneert
  orig_fn <- RLdrop:::.list_folder_recursive
  assignInNamespace(".list_folder_recursive", function(dropbox_folder, token, recursive) list(), "RLdrop")
  on.exit(assignInNamespace(".list_folder_recursive", orig_fn, "RLdrop"), add = TRUE)

  result <- dropbox_compare_folder(tmp, "/leeg", token = "dummy")

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0L)
  expect_true(all(c("naam", "status", "lokaal_grootte", "dropbox_grootte",
                               "lokaal_datum", "dropbox_datum") %in% names(result)))
})


# --- Test 3: bestanden alleen lokaal → status "alleen_lokaal" ---
local({
  tmp <- setup_local_folder(list("a.txt" = "hello", "b.txt" = "world"))
  on.exit(unlink(tmp, recursive = TRUE))

  orig_fn <- RLdrop:::.list_folder_recursive
  assignInNamespace(".list_folder_recursive", function(dropbox_folder, token, recursive) list(), "RLdrop")
  on.exit(assignInNamespace(".list_folder_recursive", orig_fn, "RLdrop"), add = TRUE)

  result <- dropbox_compare_folder(tmp, "/test", token = "dummy")

  expect_equal(nrow(result), 2L)
  expect_true(all(result$status == "alleen_lokaal"))
  expect_true(all(!is.na(result$lokaal_grootte)))
  expect_true(all(is.na(result$dropbox_grootte)))
})


# --- Test 4: bestanden alleen op Dropbox → status "alleen_dropbox" ---
local({
  tmp <- setup_local_folder()
  on.exit(unlink(tmp, recursive = TRUE))

  # [ARCH] Simuleer Dropbox entries in het formaat dat de API retourneert
  mock_entries <- list(
    list(.tag = "file", name = "remote.txt", path_lower = "/test/remote.txt",
         path_display = "/test/remote.txt", size = 42,
         client_modified = "2025-01-15T10:30:00Z"),
    list(.tag = "file", name = "data.csv", path_lower = "/test/data.csv",
         path_display = "/test/data.csv", size = 1024,
         client_modified = "2025-03-20T14:00:00Z")
  )

  orig_fn <- RLdrop:::.list_folder_recursive
  assignInNamespace(".list_folder_recursive", function(dropbox_folder, token, recursive) mock_entries, "RLdrop")
  on.exit(assignInNamespace(".list_folder_recursive", orig_fn, "RLdrop"), add = TRUE)

  result <- dropbox_compare_folder(tmp, "/test", token = "dummy")

  expect_equal(nrow(result), 2L)
  expect_true(all(result$status == "alleen_dropbox"))
  expect_true(all(is.na(result$lokaal_grootte)))
  expect_true(all(!is.na(result$dropbox_grootte)))
})


# --- Test 5: bestanden met gelijke grootte → status "gelijk" (zonder datumvergelijking) ---
local({
  # [ARCH] Maak lokale bestanden met bekende inhoud zodat grootte voorspelbaar is
  tmp <- setup_local_folder(list("match.txt" = "exact"))
  on.exit(unlink(tmp, recursive = TRUE))

  local_size <- file.info(file.path(tmp, "match.txt"))$size

  mock_entries <- list(
    list(.tag = "file", name = "match.txt", path_lower = "/test/match.txt",
         path_display = "/test/match.txt", size = local_size,
         client_modified = "2020-01-01T00:00:00Z")
  )

  orig_fn <- RLdrop:::.list_folder_recursive
  assignInNamespace(".list_folder_recursive", function(dropbox_folder, token, recursive) mock_entries, "RLdrop")
  on.exit(assignInNamespace(".list_folder_recursive", orig_fn, "RLdrop"), add = TRUE)

  result <- dropbox_compare_folder(tmp, "/test", token = "dummy", compare_date = FALSE)

  expect_equal(nrow(result), 1L)
  expect_equal(result$status, "gelijk")
  expect_equal(result$lokaal_grootte, result$dropbox_grootte)
})


# --- Test 6: bestanden met verschillende grootte → status "grootte_verschilt" ---
local({
  tmp <- setup_local_folder(list("differ.txt" = "kort"))
  on.exit(unlink(tmp, recursive = TRUE))

  mock_entries <- list(
    list(.tag = "file", name = "differ.txt", path_lower = "/test/differ.txt",
         path_display = "/test/differ.txt", size = 999999,
         client_modified = "2025-06-01T00:00:00Z")
  )

  orig_fn <- RLdrop:::.list_folder_recursive
  assignInNamespace(".list_folder_recursive", function(dropbox_folder, token, recursive) mock_entries, "RLdrop")
  on.exit(assignInNamespace(".list_folder_recursive", orig_fn, "RLdrop"), add = TRUE)

  result <- dropbox_compare_folder(tmp, "/test", token = "dummy")

  expect_equal(result$status, "grootte_verschilt")
  expect_true(result$lokaal_grootte != result$dropbox_grootte)
})


# --- Test 7: gelijke grootte maar andere datum → status "datum_verschilt" ---
local({
  tmp <- setup_local_folder(list("dated.txt" = "inhoud"))
  on.exit(unlink(tmp, recursive = TRUE))

  local_size <- file.info(file.path(tmp, "dated.txt"))$size

  # [ARCH] Geef een datum die zeker afwijkt van de zojuist aangemaakte lokale datum
  mock_entries <- list(
    list(.tag = "file", name = "dated.txt", path_lower = "/test/dated.txt",
         path_display = "/test/dated.txt", size = local_size,
         client_modified = "2020-01-01T00:00:00Z")
  )

  orig_fn <- RLdrop:::.list_folder_recursive
  assignInNamespace(".list_folder_recursive", function(dropbox_folder, token, recursive) mock_entries, "RLdrop")
  on.exit(assignInNamespace(".list_folder_recursive", orig_fn, "RLdrop"), add = TRUE)

  result <- dropbox_compare_folder(tmp, "/test", token = "dummy", compare_date = TRUE)

  expect_equal(result$status, "datum_verschilt")
})


# --- Test 8: case-insensitive vergelijking ---
# [ARCH] Dropbox is case-insensitief; een bestand "Data.TXT" lokaal
# moet matchen met "data.txt" op Dropbox
local({
  tmp <- setup_local_folder(list("Data.TXT" = "test"))
  on.exit(unlink(tmp, recursive = TRUE))

  local_size <- file.info(file.path(tmp, "Data.TXT"))$size

  mock_entries <- list(
    list(.tag = "file", name = "data.txt", path_lower = "/test/data.txt",
         path_display = "/test/data.txt", size = local_size,
         client_modified = "2020-01-01T00:00:00Z")
  )

  orig_fn <- RLdrop:::.list_folder_recursive
  assignInNamespace(".list_folder_recursive", function(dropbox_folder, token, recursive) mock_entries, "RLdrop")
  on.exit(assignInNamespace(".list_folder_recursive", orig_fn, "RLdrop"), add = TRUE)

  result <- dropbox_compare_folder(tmp, "/test", token = "dummy", compare_date = FALSE)

  # Moet 1 rij zijn (match), niet 2 (apart lokaal + apart dropbox)
  expect_equal(nrow(result), 1L)
  expect_equal(result$status, "gelijk")
})


# --- Test 9: sortering — problemen staan bovenaan ---
local({
  tmp <- setup_local_folder(list("gelijk.txt" = "ok", "lokaal.txt" = "extra"))
  on.exit(unlink(tmp, recursive = TRUE))

  gelijk_size <- file.info(file.path(tmp, "gelijk.txt"))$size

  mock_entries <- list(
    list(.tag = "file", name = "gelijk.txt", path_lower = "/test/gelijk.txt",
         path_display = "/test/gelijk.txt", size = gelijk_size,
         client_modified = "2020-01-01T00:00:00Z"),
    list(.tag = "file", name = "remote.txt", path_lower = "/test/remote.txt",
         path_display = "/test/remote.txt", size = 100,
         client_modified = "2025-01-01T00:00:00Z")
  )

  orig_fn <- RLdrop:::.list_folder_recursive
  assignInNamespace(".list_folder_recursive", function(dropbox_folder, token, recursive) mock_entries, "RLdrop")
  on.exit(assignInNamespace(".list_folder_recursive", orig_fn, "RLdrop"), add = TRUE)

  result <- dropbox_compare_folder(tmp, "/test", token = "dummy", compare_date = FALSE)

  # [ARCH] Problemen (alleen_dropbox, alleen_lokaal) moeten voor "gelijk" staan
  gelijk_pos <- which(result$status == "gelijk")
  probleem_pos <- which(result$status != "gelijk")
  expect_true(all(probleem_pos < min(gelijk_pos)))
})
