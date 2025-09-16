
devtools::document()
devtools::check()
pkgdown::build_site_github_pages()

pkgload::load_all(
  export_all = FALSE,
  attach_testthat = FALSE
)
