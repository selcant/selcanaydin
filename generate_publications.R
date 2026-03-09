# Run this script to regenerate publication pages from OpenAlex.
# Output: rewrites index.qmd in existing publications/posts/ subfolders,
#         or creates new ones if no matching folder exists.

library(httr2)
library(dplyr)
library(purrr)
library(stringr)
library(yaml)

# ── Fetch ──────────────────────────────────────────────────────────────────────
orcid      <- "0000-0002-0457-9641"
author_url <- paste0(
  "https://api.openalex.org/authors/orcid:", orcid,
  "?select=id,display_name,works_count"
)
author_id <- request(author_url) |> req_perform() |> resp_body_json() |> _$id

fetch_works <- function(author_id) {
  base_url <- paste0(
    "https://api.openalex.org/works",
    "?filter=authorships.author.id:", author_id,
    "&select=title,publication_year,type,primary_location,authorships,doi,open_access,abstract_inverted_index",
    "&sort=publication_year:desc",
    "&per_page=200"
  )
  resp    <- request(base_url) |> req_perform() |> resp_body_json()
  results <- resp$results
  while (!is.null(resp$meta$next_cursor)) {
    next_url <- paste0(base_url, "&cursor=", resp$meta$next_cursor)
    resp     <- request(next_url) |> req_perform() |> resp_body_json()
    results  <- c(results, resp$results)
  }
  results
}

reconstruct_abstract <- function(inv) {
  if (is.null(inv) || length(inv) == 0) return(NA_character_)
  words <- rep(names(inv), lengths(inv))
  words[order(unlist(inv))] |> paste(collapse = " ")
}

parse_work <- function(w) {
  doi     <- w$doi
  oa_url  <- w$open_access$oa_url
  landing <- w$primary_location$landing_page_url
  url     <- oa_url %||% landing %||%
    if (!is.null(doi)) paste0("https://doi.org/", doi) else NA_character_
  
  abstract <- reconstruct_abstract(w$abstract_inverted_index)
  # Fall back to the landing page description if abstract is missing
  text     <- if (!is.na(abstract) && nzchar(abstract)) abstract else
    w$primary_location$landing_page_url %||% NA_character_
  
  tibble(
    title    = w$title %||% "Untitled",
    year     = w$publication_year %||% NA_integer_,
    authors  = map_chr(w$authorships, ~ .x$author$display_name) |> paste(collapse = ", "),
    journal  = w$primary_location$source$display_name %||% NA_character_,
    type     = w$type %||% NA_character_,
    abstract = text,
    doi      = doi %||% NA_character_,
    url      = url %||% NA_character_
  )
}

works_raw <- fetch_works(author_id)

pubs <- map(works_raw, parse_work) |>
  list_rbind() |>
  mutate(doi_key = if_else(is.na(doi) | doi == "", title, doi)) |>
  distinct(doi_key, .keep_all = TRUE) |>
  select(-doi_key) |>
  filter(type %in% c(
    "article", "journal-article", "preprint", "book-chapter",
    "proceedings-article", "review", "letter"
  )) |>
  arrange(desc(year))

# ── Helpers ────────────────────────────────────────────────────────────────────
out_dir <- "publications/posts"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

make_slug <- function(title, year) {
  slug <- title |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "-") |>
    str_sub(1, 60) |>
    str_remove("-$")
  paste0(year, "-", slug)
}

# Find an existing subfolder whose index.qmd title matches, else use slug
find_existing_dir <- function(title, slug) {
  existing <- list.dirs(out_dir, recursive = FALSE)
  match <- keep(existing, function(d) {
    qmd <- file.path(d, "index.qmd")
    if (!file.exists(qmd)) return(FALSE)
    lines <- readLines(qmd, warn = FALSE)
    any(str_detect(lines, fixed(title)))
  })
  if (length(match) > 0) match[[1]] else file.path(out_dir, slug)
}

# ── Write / overwrite one .qmd per publication ────────────────────────────────
write_pub_qmd <- function(row) {
  slug <- make_slug(row$title, row$year %||% "nd")
  dir  <- find_existing_dir(row$title, slug)
  dir.create(dir, showWarnings = FALSE)
  path <- file.path(dir, "index.qmd")
  
  fm <- list(
    title       = row$title,
    date        = paste0(row$year %||% "1900", "-01-01"),
    author      = row$authors,
    image       = "featured.png",
    description = "",
    categories  = list(str_to_title(str_replace_all(row$type, "-", " ")))
  )
  if (!is.na(row$journal)) fm$venue <- row$journal
  if (!is.na(row$doi))     fm$doi   <- row$doi
  if (!is.na(row$url))     fm$url   <- row$url
  
  abstract_section <- if (!is.na(row$abstract) && nzchar(row$abstract)) {
    paste0("## Abstract\n\n", row$abstract, "\n")
  } else {
    "_No abstract available._\n"
  }
  
  doi_link <- if (!is.na(row$doi)) {
    paste0("\n[{{< fa book >}} DOI](https://doi.org/", row$doi, "){.btn .btn-outline-primary .btn-sm}")
  } else ""
  oa_link <- if (!is.na(row$url) && grepl("^http", row$url)) {
    paste0("\n[{{< fa lock-open >}} Open Access](", row$url, "){.btn .btn-outline-success .btn-sm}")
  } else ""
  
  body <- paste0(
    "---\n",
    yaml::as.yaml(fm),
    "---\n\n",
    "::: {.pub-meta}\n",
    if (!is.na(row$journal)) paste0("*", row$journal, "*") else "",
    " · ", row$year %||% "n.d.",
    "\n:::\n\n",
    abstract_section,
    "\n## Links\n",
    doi_link, oa_link, "\n"
  )
  
  writeLines(body, path)
  message(if (file.exists(path)) "Updated: " else "Created: ", path)
}

pwalk(pubs, function(...) write_pub_qmd(tibble(...)))
message("\nDone — ", nrow(pubs), " publication pages written to ", out_dir)
