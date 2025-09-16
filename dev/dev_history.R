
chore <- function() {
  devtools::document()
  devtools::build_readme()
  devtools::build_vignettes()
  devtools::check()
  pkgdown::build_site_github_pages()
}
chore()

pkgload::load_all(
  export_all = FALSE,
  attach_testthat = FALSE
)

covr::report(file = "validation/coverage.html")
