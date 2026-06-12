# test_upload.R
# Tests voor dropbox_upload_folder().
# De tests mocken dropbox_upload(), zodat er geen echte Dropbox API wordt
# aangeroepen. Lokale tempbestanden worden wel aangemaakt om de bestandsselectie
# en relatieve padopbouw realistisch te testen.


# --- Helper: maak tijdelijke mapstructuur met bestanden ---
setup_upload_folder <- function(files = list()) {
  tmp <- tempfile("upload_folder_")
  dir.create(tmp, recursive = TRUE)

  for (nm in names(files)) {
    full_path <- file.path(tmp, nm)
    parent <- dirname(full_path)

    if (!dir.exists(parent)) {
      dir.create(parent, recursive = TRUE)
    }

    writeLines(files[[nm]], full_path)
  }

  tmp
}


# --- Helper: mock dropbox_upload() en registreer alle aanroepen ---
install_upload_mock <- function() {
  original_upload <- RLdrop::dropbox_upload

  calls <- new.env(parent = emptyenv())
  calls$local_path <- character(0)
  calls$dropbox_path <- character(0)
  calls$token <- character(0)
  calls$mode <- character(0)
  calls$large_file_threshold <- numeric(0)

  assignInNamespace(
    "dropbox_upload",
    function(local_path, dropbox_path, token = RLdrop::dropbox_token(),
             mode = "overwrite", large_file_threshold = 140 * 1024 * 1024) {
      calls$local_path <- c(calls$local_path, local_path)
      calls$dropbox_path <- c(calls$dropbox_path, dropbox_path)
      calls$token <- c(calls$token, token)
      calls$mode <- c(calls$mode, mode)
      calls$large_file_threshold <- c(calls$large_file_threshold, large_file_threshold)

      invisible(list(path_display = dropbox_path, name = basename(dropbox_path)))
    },
    "RLdrop"
  )

  list(
    calls = calls,
    restore = function() {
      assignInNamespace("dropbox_upload", original_upload, "RLdrop")
      invisible(TRUE)
    }
  )
}


# --- Test 1: pattern moet NULL of een geldige regex-string zijn ---
expect_error(
  RLdrop::dropbox_upload_folder(
    tempfile("niet_bestaand_"),
    "/remote",
    token = "dummy",
    pattern = c("\\.csv$", "\\.txt$")
  ),
  pattern = "pattern"
)

expect_error(
  RLdrop::dropbox_upload_folder(
    tempfile("niet_bestaand_"),
    "/remote",
    token = "dummy",
    pattern = "["
  ),
  pattern = "pattern"
)


# --- Test 2: welke moet NULL of een character vector zonder NA zijn ---
expect_error(
  RLdrop::dropbox_upload_folder(
    tempfile("niet_bestaand_"),
    "/remote",
    token = "dummy",
    welke = 1:2
  ),
  pattern = "welke"
)

expect_error(
  RLdrop::dropbox_upload_folder(
    tempfile("niet_bestaand_"),
    "/remote",
    token = "dummy",
    welke = c("A.txt", NA_character_)
  ),
  pattern = "welke"
)


# --- Test 3: zonder filters uploadt de functie alle directe bestanden ---
local({
  tmp <- setup_upload_folder(list(
    "A.txt" = "a",
    "B.csv" = "b",
    "sub/C.txt" = "c"
  ))
  on.exit(unlink(tmp, recursive = TRUE))

  mock <- install_upload_mock()
  on.exit(mock$restore(), add = TRUE)

  uploaded <- RLdrop::dropbox_upload_folder(
    tmp,
    "/remote",
    token = "dummy",
    recursive = FALSE
  )

  expect_equal(length(uploaded), 2L)
  expect_true(all(basename(mock$calls$local_path) %in% c("A.txt", "B.csv")))
  expect_equal(length(mock$calls$dropbox_path), 2L)
})


# --- Test 4: pattern filtert op lokale bestandsnaam ---
local({
  tmp <- setup_upload_folder(list(
    "A.txt" = "a",
    "B.csv" = "b",
    "C.csv" = "c"
  ))
  on.exit(unlink(tmp, recursive = TRUE))

  mock <- install_upload_mock()
  on.exit(mock$restore(), add = TRUE)

  uploaded <- RLdrop::dropbox_upload_folder(
    tmp,
    "/remote",
    token = "dummy",
    pattern = "\\.csv$"
  )

  expect_equal(length(uploaded), 2L)
  expect_true(all(basename(mock$calls$local_path) %in% c("B.csv", "C.csv")))
  expect_true(all(grepl("\\.csv$", mock$calls$dropbox_path)))
})


# --- Test 5: welke selecteert exacte lokale bestandsnamen ---
local({
  tmp <- setup_upload_folder(list(
    "A.txt" = "a",
    "B.csv" = "b",
    "C.R" = "c"
  ))
  on.exit(unlink(tmp, recursive = TRUE))

  mock <- install_upload_mock()
  on.exit(mock$restore(), add = TRUE)

  uploaded <- RLdrop::dropbox_upload_folder(
    tmp,
    "/remote",
    token = "dummy",
    welke = c("A.txt", "C.R")
  )

  expect_equal(length(uploaded), 2L)
  expect_equal(sort(basename(mock$calls$local_path)), c("A.txt", "C.R"))
})


# --- Test 6: pattern en welke vormen samen een EN-filter ---
local({
  tmp <- setup_upload_folder(list(
    "A.csv" = "a",
    "B.txt" = "b",
    "C.csv" = "c"
  ))
  on.exit(unlink(tmp, recursive = TRUE))

  mock <- install_upload_mock()
  on.exit(mock$restore(), add = TRUE)

  uploaded <- RLdrop::dropbox_upload_folder(
    tmp,
    "/remote",
    token = "dummy",
    pattern = "\\.csv$",
    welke = c("A.csv", "B.txt")
  )

  expect_equal(length(uploaded), 1L)
  expect_equal(basename(mock$calls$local_path), "A.csv")
})


# --- Test 7: ontbrekende namen in welke worden gemeld, gevonden namen gaan door ---
local({
  tmp <- setup_upload_folder(list(
    "A.txt" = "a",
    "B.txt" = "b"
  ))
  on.exit(unlink(tmp, recursive = TRUE))

  mock <- install_upload_mock()
  on.exit(mock$restore(), add = TRUE)

  messages <- utils::capture.output(
    uploaded <- RLdrop::dropbox_upload_folder(
      tmp,
      "/remote",
      token = "dummy",
      welke = c("A.txt", "missing.txt")
    ),
    type = "message"
  )

  expect_equal(length(uploaded), 1L)
  expect_equal(basename(mock$calls$local_path), "A.txt")
  expect_true(any(grepl("missing.txt", messages)))
})


# --- Test 8: recursive + welke uploadt dezelfde naam in meerdere submappen ---
local({
  tmp <- setup_upload_folder(list(
    "a/same.txt" = "a",
    "b/same.txt" = "b",
    "b/other.txt" = "other"
  ))
  on.exit(unlink(tmp, recursive = TRUE))

  mock <- install_upload_mock()
  on.exit(mock$restore(), add = TRUE)

  uploaded <- RLdrop::dropbox_upload_folder(
    tmp,
    "/remote",
    token = "dummy",
    recursive = TRUE,
    welke = "same.txt"
  )

  expect_equal(length(uploaded), 2L)
  expect_true(any(grepl("/a/same.txt$", mock$calls$dropbox_path)))
  expect_true(any(grepl("/b/same.txt$", mock$calls$dropbox_path)))
  expect_true(!any(grepl("/b/other.txt$", mock$calls$dropbox_path)))
})


# --- Test 9: lege welke-vector uploadt niets en roept dropbox_upload niet aan ---
local({
  tmp <- setup_upload_folder(list("A.txt" = "a"))
  on.exit(unlink(tmp, recursive = TRUE))

  mock <- install_upload_mock()
  on.exit(mock$restore(), add = TRUE)

  uploaded <- RLdrop::dropbox_upload_folder(
    tmp,
    "/remote",
    token = "dummy",
    welke = character(0)
  )

  expect_equal(uploaded, character(0))
  expect_equal(length(mock$calls$dropbox_path), 0L)
})


# --- Test 10: mode en large_file_threshold worden doorgegeven ---
local({
  tmp <- setup_upload_folder(list("A.txt" = "a"))
  on.exit(unlink(tmp, recursive = TRUE))

  mock <- install_upload_mock()
  on.exit(mock$restore(), add = TRUE)

  RLdrop::dropbox_upload_folder(
    tmp,
    "/remote",
    token = "dummy",
    mode = "add",
    large_file_threshold = 123,
    welke = "A.txt"
  )

  expect_equal(mock$calls$mode, "add")
  expect_equal(mock$calls$large_file_threshold, 123)
})
