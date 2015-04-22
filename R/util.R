# util.R
# Useful utility functions for my data wrangling

#--------------------------------------------------
# Set up data sources

str_qtr <- function (d) {str_c(year(d), "-Q", quarter(d)) }
thisQ <- str_qtr(now())
lastQ <- str_qtr(now() %m-% months(3))

srcDir <- function (qtr, folder="Reports", base_dir="..") {
  file.path(base_dir, qtr, folder)
}

#--------------------------------------------------
# File functions

# Find the latest version of a file pattern

find_latest <- function (srcDir=".", pattern) {
  files <- sort(list.files(path=srcDir, pattern=pattern))
  assert_that(length(files) > 0)
  in_file <- file.path(srcDir, tail(files, n=1))
  return(in_file)
}

# Read the latest CSV or XLSX file with a given pattern in a given folder.
# Return colnames either exactly, or as valid R identifies for compatibility with read.csv().

read_latest <- function (srcDir=".", aPattern, skip=0, sheet=1, R.identifiers=FALSE) {
  assert_that(is.dir(srcDir))
  in_file <- find_latest(srcDir, aPattern)
  cat("Reading:", in_file, sep=" ")

  if (str_detect(in_file, "xlsx$")) {
    df <- read_excel(in_file, sheet=sheet, skip=skip)
  }
  else {
    df <- read_csv(in_file, col_names=TRUE, skip=skip)
  }

  if (R.identifiers==TRUE) {
    colnames(df) <- make.names(colnames(df))
  }
  return(df)
}

# Removes all but the most recent file with a given pattern, i.e. a directory purge a la VMS.
# Beware of using this indiscriminately.

file_purge <- function (path, pattern="*") {
  files <- sort(list.files(path, pattern=pattern, full.names=TRUE))
  len <- length(files)
  file.remove(files[-len])
}

#--------------------------------------------------
# Utilities

# Turn strings into numbers. Also handles percentages.

toNumber <- function (x) {
  x <- gsub('([0-9]+)%', "0.\\1", x, fixed=FALSE)
  x <- extract_numeric(x) # from tidyr package
  x[is.na(x)] <- 0
  return(x)
}

# Normalise and round

norm <- function (v) {
  return(round(v/sum(v), digits=2))
}

# Select the first value unless zero, in which case return the second!

xnonzero <- function (a, b) {
  ifelse(a==0, b, a)
}

# Change NAs to something else, or zero by default.

changeNA <- function (df, to=0) {
  df[is.na(df)] <- to
  df
}

#--------------------------------------------------
# Viewing

# Show a table as ugly HTML in the Viewer pane
view_as_html <- function (tbl) {
  dir <- tempfile()
  dir.create(dir)
  htmlFile <- file.path(dir, "index.html")
  print(xtable(tbl), type="html", file=htmlFile, include.rownames = FALSE,
        html.table.attributes="border=1")
  rstudio::viewer(htmlFile)
}

# The End
