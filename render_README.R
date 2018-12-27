rmarkdown::render("README.Rmd",
                  output_format = rmarkdown::github_document(
                      df_print = function(...) { print(..., row.names = FALSE) },
                      html_preview = FALSE))

pkgdown::build_site()
