
chores <- function() {
  usethis::use_tidy_description()
  devtools::build_readme()
  lintr::lint_package()
  spelling::update_wordlist()
  devtools::document()
  devtools::build_vignettes()
  devtools::check()
  covr::report(
    file = "validation/coverage.html",
    browse = FALSE
  )
  pkgdown::build_site_github_pages()
}
chores()

pkgload::load_all(
  export_all = FALSE,
  attach_testthat = FALSE
)

lintr::lint_package()
spelling::update_wordlist()
DT::datatable()
htmltools::a()
knitr::all_labels()
qpdf::pdf_subset()
rmarkdown::knitr_options()
testthat::announce_snapshot_file()
