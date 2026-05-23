# test_metadata.R
# Tests voor dropbox_exists() en dropbox_file_info()
# Mockt httr::POST en httr::content om API-responses te simuleren.


# --- Helper: mock httr::POST en httr::content ---
# [ARCH] httr::content() kan niet overweg met een nep response-object vanwege
# interne content-type parsing. Daarom mocken we zowel POST als content.
# Retourneert een lijst met originele functies voor restore.
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


# ===== dropbox_exists() =====

# --- Test 1: bestand bestaat → TRUE ---
local({
  orig <- mock_api(200L, list(
    .tag = "file", name = "test.csv", path_display = "/data/test.csv",
    size = 1024, client_modified = "2025-01-01T00:00:00Z"
  ))
  on.exit(restore_api(orig))

  expect_true(dropbox_exists("/data/test.csv", token = "dummy"))
})


# --- Test 2: map bestaat → TRUE ---
local({
  orig <- mock_api(200L, list(
    .tag = "folder", name = "data", path_display = "/data"
  ))
  on.exit(restore_api(orig))

  expect_true(dropbox_exists("/data", token = "dummy"))
})


# --- Test 3: pad bestaat niet → FALSE ---
local({
  # [ARCH] Dropbox retourneert 409 met not_found bij niet-bestaand pad
  orig <- mock_api(409L, list(
    error_summary = "path/not_found/",
    error = list(.tag = "path", path = list(.tag = "not_found"))
  ))
  on.exit(restore_api(orig))

  expect_false(dropbox_exists("/niet/bestaand", token = "dummy"))
})


# --- Test 4: andere API-fout → fout gooien ---
local({
  orig <- mock_api(401L, list(error_summary = "invalid_access_token"))
  on.exit(restore_api(orig))

  expect_error(
    dropbox_exists("/test", token = "dummy"),
    pattern = "Metadata ophalen mislukt"
  )
})


# ===== dropbox_file_info() =====

# --- Test 5: bestandsmetadata correct geparsed ---
local({
  orig <- mock_api(200L, list(
    .tag = "file", name = "rapport.xlsx",
    path_display = "/data/rapport.xlsx",
    size = 4096,
    client_modified = "2025-06-15T12:30:00Z",
    rev = "abc123def"
  ))
  on.exit(restore_api(orig))

  info <- dropbox_file_info("/data/rapport.xlsx", token = "dummy")

  expect_equal(info$naam, "rapport.xlsx")
  expect_equal(info$pad, "/data/rapport.xlsx")
  expect_equal(info$type, "file")
  expect_equal(info$grootte, 4096)
  expect_equal(info$revisie, "abc123def")
  expect_equal(format(info$gewijzigd, "%Y-%m-%d", tz = "UTC"), "2025-06-15")
})


# --- Test 6: mapmetadata — grootte en datum zijn NA ---
local({
  orig <- mock_api(200L, list(
    .tag = "folder", name = "archief",
    path_display = "/data/archief"
  ))
  on.exit(restore_api(orig))

  info <- dropbox_file_info("/data/archief", token = "dummy")

  expect_equal(info$naam, "archief")
  expect_equal(info$type, "folder")
  expect_true(is.na(info$grootte))
  expect_true(is.na(info$gewijzigd))
  expect_true(is.na(info$revisie))
})


# --- Test 7: niet-bestaand pad → fout ---
local({
  orig <- mock_api(409L, list(error_summary = "path/not_found/"))
  on.exit(restore_api(orig))

  expect_error(
    dropbox_file_info("/niet/bestaand", token = "dummy"),
    pattern = "Metadata ophalen mislukt"
  )
})
