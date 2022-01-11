library(reactable)
library(htmltools)

pkgs <- read.csv("cran_pkgs.csv", stringsAsFactors = FALSE)
versions <- jsonlite::read_json("versions.json", simplifyDataFrame = TRUE)

row_details <- function(index) {
  pkg <- pkgs[index, ]
  urls <- unlist(strsplit(gsub(",", " , ", pkg$URL, perl = TRUE), "[ \n]"))

  other_field <- function(name, ...) {
    if (any(is.na(...))) NULL
    else tagList(div(class = "detail-label", name), ...)
  }

  detail <- div(
    class = "package-detail",
    div(class = "detail-header", pkg$Package, span(class = "detail-title", pkg$Title)),
    div(class = "detail-description", pkg$Description),
    pkg_field("Version", pkg$Version),
    pkg_field("Depends", pkg$Depends),
    pkg_field("Imports", pkg$Imports),
    pkg_field("Suggests", pkg$Suggests),
    pkg_field("Author", pkg$Author),
    pkg_field("License", pkg$License),
    pkg_field("URL", lapply(urls, function(url) {
      if (grepl("https?://", url)) tags$a(href = url, url)
      else if (identical(url, ",")) ", "
      else url
    })),
    pkg_field("System Requirements", pkg$SystemRequirements)
  )

  if (length(versions[[pkg$Package]]) > 0) {
    archived <- pkg_field(
      "Archived Versions",
      reactable(
        versions[[pkg$Package]],
        pagination = FALSE,
        defaultColDef = colDef(headerClass = "header"),
        columns = list(
          Date = colDef(name = "Published", align = "right", width = 120, cell = function(value) {
            strftime(value, format = "%b %d, %Y")
          })
        ),
        fullWidth = FALSE,
        class = "archived-table",
        theme = reactableTheme(cellPadding = "8px 12px")
      )
    )
    detail <- tagAppendChild(detail, archived)
  }

  detail
}

tbl <- reactable(
  pkgs[, c("Package", "Version", "Title", "Date", "Downloads")],
  defaultSorted = "Downloads",
  defaultPageSize = 20,
  showPageSizeOptions = TRUE,
  pageSizeOptions = c(10, 20, 50, 100),
  onClick = "expand",
  resizable = TRUE,
  defaultColDef = colDef(headerClass = "header"),
  columns = list(
    Title = colDef(minWidth = 250, class = "package-title", cell = function(value) {
      span(title = value, value)
    }),
    Date = colDef(name = "Published", align = "right", cell = function(value) {
      strftime(value, format = "%b %d, %Y")
    }),
    Downloads = colDef(defaultSortOrder = "desc", cell = function(value) {
      value <- format(round(value / 1e6, 1), nsmall = 1)
      tagList(value, span(class = "units", "M"))
    })
  ),
  details = row_details,
  wrap = FALSE,
  class = "packages-table",
  rowStyle = list(cursor = "pointer"),
  theme = reactableTheme(cellPadding = "8px 12px")
)

div(class = "cran-packages",
    h2(class = "title", "Top CRAN Packages of 2019"),
    tbl
)

