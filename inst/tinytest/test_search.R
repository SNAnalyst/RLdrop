# test_search.R
# Tests voor dropbox_search()
# Mockt httr::POST en httr::content om API-responses te simuleren.


# --- Helper: mock httr::POST en httr::content met URL-afhankelijke responses ---
# [ARCH] search gebruikt twee endpoints (search_v2 en search/continue_v2),
# dus de mock accepteert een handler-functie die op basis van URL de response bepaalt.
mock_api_fn <- function(handler_fn) {
  orig_post    <- httr::POST
  orig_content <- httr::content

  assignInNamespace("POST", function(url, ...) {
    body_list   <- handler_fn(url)
    status_code <- if (is.null(body_list$.status)) 200L else body_list$.status
    body_list$.status <- NULL
    structure(
      list(status_code = status_code, .body = body_list),
      class = "response"
    )
  }, "httr")

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


# --- Test 1: geen resultaten → leeg data.frame ---
local({
  orig <- mock_api_fn(function(url) {
    list(matches = list(), has_more = FALSE)
  })
  on.exit(restore_api(orig))

  result <- dropbox_search("xyznietsbestaands", token = "dummy")

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0L)
  expect_true(all(c("naam", "pad", "type", "grootte", "gewijzigd") %in% names(result)))
})


# --- Test 2: resultaten correct geparsed ---
local({
  mock_matches <- list(
    list(metadata = list(metadata = list(
      .tag = "file", name = "rapport_2025.xlsx",
      path_display = "/data/rapport_2025.xlsx",
      size = 8192, client_modified = "2025-04-01T10:00:00Z"
    ))),
    list(metadata = list(metadata = list(
      .tag = "folder", name = "rapporten",
      path_display = "/rapporten"
    )))
  )

  orig <- mock_api_fn(function(url) {
    list(matches = mock_matches, has_more = FALSE)
  })
  on.exit(restore_api(orig))

  result <- dropbox_search("rapport", token = "dummy")

  expect_equal(nrow(result), 2L)
  expect_equal(result$naam[1], "rapport_2025.xlsx")
  expect_equal(result$type[1], "file")
  expect_equal(result$grootte[1], 8192)
  expect_equal(result$type[2], "folder")
  expect_true(is.na(result$grootte[2]))
})


# --- Test 3: API-fout → fout gooien ---
local({
  orig <- mock_api_fn(function(url) {
    list(error_summary = "bad_request", .status = 400L)
  })
  on.exit(restore_api(orig))

  expect_error(
    dropbox_search("test", token = "dummy"),
    pattern = "Zoeken mislukt"
  )
})


# --- Test 4: max_results wordt gerespecteerd ---
local({
  # [ARCH] Maak 5 mock-resultaten, maar vraag er maar 3
  mock_matches <- lapply(1:5, function(i) {
    list(metadata = list(metadata = list(
      .tag = "file", name = sprintf("file_%d.txt", i),
      path_display = sprintf("/data/file_%d.txt", i),
      size = 100 * i, client_modified = "2025-01-01T00:00:00Z"
    )))
  })

  orig <- mock_api_fn(function(url) {
    list(matches = mock_matches, has_more = FALSE)
  })
  on.exit(restore_api(orig))

  result <- dropbox_search("file", token = "dummy", max_results = 3L)

  expect_equal(nrow(result), 3L)
})


# --- Test 5: paginering werkt ---
local({
  # [ARCH] Eerste call retourneert 2 resultaten + has_more = TRUE,
  # tweede call (continue) retourneert 1 resultaat + has_more = FALSE
  page1_matches <- list(
    list(metadata = list(metadata = list(
      .tag = "file", name = "a.txt", path_display = "/a.txt",
      size = 10, client_modified = "2025-01-01T00:00:00Z"
    ))),
    list(metadata = list(metadata = list(
      .tag = "file", name = "b.txt", path_display = "/b.txt",
      size = 20, client_modified = "2025-01-02T00:00:00Z"
    )))
  )
  page2_matches <- list(
    list(metadata = list(metadata = list(
      .tag = "file", name = "c.txt", path_display = "/c.txt",
      size = 30, client_modified = "2025-01-03T00:00:00Z"
    )))
  )

  orig <- mock_api_fn(function(url) {
    if (grepl("continue", url)) {
      list(matches = page2_matches, has_more = FALSE)
    } else {
      list(matches = page1_matches, has_more = TRUE, cursor = "xyz")
    }
  })
  on.exit(restore_api(orig))

  result <- dropbox_search("txt", token = "dummy", max_results = 100L)

  expect_equal(nrow(result), 3L)
  expect_equal(result$naam, c("a.txt", "b.txt", "c.txt"))
})
