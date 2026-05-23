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
