# test_manage.R
# Tests voor dropbox_delete(), dropbox_move(), en dropbox_create_folder()
# Mockt httr::POST en httr::content om API-responses te simuleren.


# --- Helper: mock httr::POST en httr::content ---
# [ARCH] httr::content() kan niet overweg met een nep response-object vanwege
# interne content-type parsing. Daarom mocken we zowel POST als content.
mock_api <- function(status_code, body_list) {
  orig_post    <- httr::POST
  orig_content <- httr::content

  mock_response <- structure(
    list(status_code = status_code, .body = body_list),
    class = "response"
  )

  assignInNamespace("POST", function(...) mock_response, "httr")
  assignInNamespace("content", function(x, as = NULL, ...) {
    if (!is.null(x$.body)) {
      if (!is.null(as) && as == "text") {
        return(jsonlite::toJSON(x$.body, auto_unbox = TRUE))
      }
      return(x$.body)
    }
    orig_content(x, as = as, ...)
  }, "httr")

  list(post = orig_post, content = orig_content)
}

restore_api <- function(orig) {
  assignInNamespace("POST", orig$post, "httr")
  assignInNamespace("content", orig$content, "httr")
}


# --- Helper: mock Dropbox listing plus delete_v2-calls voor gefilterde delete ---
mock_filtered_delete <- function(entries) {
  original_list <- RLdrop:::.list_folder_recursive
  original_post <- httr::POST
  original_content <- httr::content

  calls <- new.env(parent = emptyenv())
  calls$list_path <- character(0)
  calls$recursive <- logical(0)
  calls$deleted_paths <- character(0)

  assignInNamespace(
    ".list_folder_recursive",
    function(dropbox_folder, token, recursive) {
      calls$list_path <- c(calls$list_path, dropbox_folder)
      calls$recursive <- c(calls$recursive, recursive)
      entries
    },
    "RLdrop"
  )

  assignInNamespace(
    "POST",
    function(url, ..., body = NULL) {
      parsed_body <- jsonlite::fromJSON(body)
      calls$deleted_paths <- c(calls$deleted_paths, parsed_body$path)

      structure(
        list(
          status_code = 200L,
          .body = list(
            metadata = list(
              .tag = "file",
              name = basename(parsed_body$path),
              path_display = parsed_body$path
            )
          )
        ),
        class = "response"
      )
    },
    "httr"
  )

  assignInNamespace(
    "content",
    function(x, as = NULL, ...) {
      if (!is.null(x$.body)) {
        if (!is.null(as) && as == "text") {
          return(jsonlite::toJSON(x$.body, auto_unbox = TRUE))
        }
        return(x$.body)
      }
      original_content(x, as = as, ...)
    },
    "httr"
  )

  list(
    calls = calls,
    restore = function() {
      assignInNamespace(".list_folder_recursive", original_list, "RLdrop")
      assignInNamespace("POST", original_post, "httr")
      assignInNamespace("content", original_content, "httr")
      invisible(TRUE)
    }
  )
}


mock_delete_entries <- list(
  list(.tag = "file", name = "A.txt",
       path_lower = "/temp/a.txt",
       path_display = "/temp/A.txt"),
  list(.tag = "file", name = "B.csv",
       path_lower = "/temp/b.csv",
       path_display = "/temp/B.csv"),
  list(.tag = "file", name = "C.csv",
       path_lower = "/temp/c.csv",
       path_display = "/temp/C.csv"),
  list(.tag = "folder", name = "sub",
       path_lower = "/temp/sub",
       path_display = "/temp/sub")
)

mock_delete_recursive_entries <- c(
  mock_delete_entries,
  list(
    list(.tag = "file", name = "same.txt",
         path_lower = "/temp/sub/same.txt",
         path_display = "/temp/sub/same.txt"),
    list(.tag = "file", name = "same.txt",
         path_lower = "/temp/other/same.txt",
         path_display = "/temp/other/same.txt"),
    list(.tag = "folder", name = "other",
         path_lower = "/temp/other",
         path_display = "/temp/other")
  )
)


# ===== dropbox_delete() =====

# --- Test 1: succesvol verwijderen ---
local({
  orig <- mock_api(200L, list(
    metadata = list(.tag = "file", name = "oud.csv",
                    path_display = "/data/oud.csv", size = 512)
  ))
  on.exit(restore_api(orig))

  result <- dropbox_delete("/data/oud.csv", token = "dummy")

  # [ARCH] Retourneert invisible metadata
  expect_true(is.list(result))
  expect_equal(result$name, "oud.csv")
})


# --- Test 2: verwijderen van niet-bestaand pad → fout ---
local({
  orig <- mock_api(409L, list(error_summary = "path_lookup/not_found/"))
  on.exit(restore_api(orig))

  expect_error(
    dropbox_delete("/niet/bestaand", token = "dummy"),
    pattern = "Verwijderen mislukt"
  )
})


# --- Test 2b: pattern verwijdert alleen matching files in een map ---
local({
  mock <- mock_filtered_delete(mock_delete_entries)
  on.exit(mock$restore())

  result <- dropbox_delete("/temp", token = "dummy", pattern = "\\.csv$")

  expect_equal(length(result), 2L)
  expect_equal(mock$calls$deleted_paths, c("/temp/B.csv", "/temp/C.csv"))
  expect_equal(mock$calls$recursive, FALSE)
})


# --- Test 2c: welke verwijdert alleen exact genoemde files ---
local({
  mock <- mock_filtered_delete(mock_delete_entries)
  on.exit(mock$restore())

  result <- dropbox_delete("/temp", token = "dummy", welke = c("A.txt", "C.csv"))

  expect_equal(length(result), 2L)
  expect_equal(mock$calls$deleted_paths, c("/temp/A.txt", "/temp/C.csv"))
})


# --- Test 2d: pattern en welke vormen samen een EN-filter ---
local({
  mock <- mock_filtered_delete(mock_delete_entries)
  on.exit(mock$restore())

  result <- dropbox_delete(
    "/temp",
    token = "dummy",
    pattern = "\\.csv$",
    welke = c("A.txt", "B.csv")
  )

  expect_equal(length(result), 1L)
  expect_equal(mock$calls$deleted_paths, "/temp/B.csv")
})


# --- Test 2e: filtered delete verwijdert nooit folders ---
local({
  mock <- mock_filtered_delete(mock_delete_entries)
  on.exit(mock$restore())

  result <- dropbox_delete("/temp", token = "dummy", pattern = "sub")

  expect_equal(length(result), 0L)
  expect_equal(length(mock$calls$deleted_paths), 0L)
})


# --- Test 2f: recursive = TRUE wordt doorgegeven en selecteert submap-files ---
local({
  mock <- mock_filtered_delete(mock_delete_recursive_entries)
  on.exit(mock$restore())

  result <- dropbox_delete(
    "/temp",
    token = "dummy",
    recursive = TRUE,
    welke = "same.txt"
  )

  expect_equal(length(result), 2L)
  expect_equal(mock$calls$recursive, TRUE)
  expect_equal(mock$calls$deleted_paths, c("/temp/sub/same.txt", "/temp/other/same.txt"))
})


# --- Test 2g: ontbrekende welke-namen worden gemeld en gevonden namen gaan door ---
local({
  mock <- mock_filtered_delete(mock_delete_entries)
  on.exit(mock$restore())

  messages <- utils::capture.output(
    result <- dropbox_delete(
      "/temp",
      token = "dummy",
      welke = c("A.txt", "missing.txt")
    ),
    type = "message"
  )

  expect_equal(length(result), 1L)
  expect_equal(mock$calls$deleted_paths, "/temp/A.txt")
  expect_true(any(grepl("missing.txt", messages)))
})


# --- Test 2h: ongeldige selectie faalt voordat er iets wordt verwijderd ---
local({
  mock <- mock_filtered_delete(mock_delete_entries)
  on.exit(mock$restore())

  expect_error(
    dropbox_delete("/temp", token = "dummy", pattern = "["),
    pattern = "pattern"
  )

  expect_equal(length(mock$calls$deleted_paths), 0L)
})


# ===== dropbox_move() =====

# --- Test 3: succesvol verplaatsen ---
local({
  orig <- mock_api(200L, list(
    metadata = list(.tag = "file", name = "rapport.xlsx",
                    path_display = "/archief/rapport.xlsx", size = 2048)
  ))
  on.exit(restore_api(orig))

  result <- dropbox_move("/data/rapport.xlsx", "/archief/rapport.xlsx", token = "dummy")

  expect_true(is.list(result))
  expect_equal(result$path_display, "/archief/rapport.xlsx")
})


# --- Test 4: verplaatsen van niet-bestaand pad → fout ---
local({
  orig <- mock_api(409L, list(error_summary = "from_lookup/not_found/"))
  on.exit(restore_api(orig))

  expect_error(
    dropbox_move("/niet/bestaand", "/ergens", token = "dummy"),
    pattern = "Verplaatsen mislukt"
  )
})


# --- Test 5: verplaatsen naar bestaand pad → fout ---
local({
  orig <- mock_api(409L, list(error_summary = "to/conflict/file"))
  on.exit(restore_api(orig))

  expect_error(
    dropbox_move("/data/a.csv", "/data/b.csv", token = "dummy"),
    pattern = "Verplaatsen mislukt"
  )
})


# ===== dropbox_create_folder() =====

# --- Test 6: map succesvol aangemaakt ---
local({
  orig <- mock_api(200L, list(
    metadata = list(.tag = "folder", name = "Q3",
                    path_display = "/projecten/2026/Q3")
  ))
  on.exit(restore_api(orig))

  result <- dropbox_create_folder("/projecten/2026/Q3", token = "dummy")

  expect_true(is.list(result))
  expect_equal(result$name, "Q3")
})


# --- Test 7: map bestaat al → fout ---
local({
  orig <- mock_api(409L, list(error_summary = "path/conflict/folder"))
  on.exit(restore_api(orig))

  expect_error(
    dropbox_create_folder("/data", token = "dummy"),
    pattern = "Map aanmaken mislukt"
  )
})
