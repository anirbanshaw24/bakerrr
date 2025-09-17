
chore <- function() {
  spelling::update_wordlist()
  devtools::document()
  devtools::build_readme()
  devtools::build_vignettes()
  devtools::check()
  covr::report(
    file = "validation/coverage.html",
    browse = FALSE
  )
  pkgdown::build_site_github_pages()
}
chore()

pkgload::load_all(
  export_all = FALSE,
  attach_testthat = FALSE
)
