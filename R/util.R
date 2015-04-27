#' util.R
#' Useful utility functions for my data wrangling
#'
#' Create a quarter folder using the provided date
#' @param d A date value
#' @return A string of the form \code{yyyy-Qq} where \code{q} is one of 1, 2, 3 or 4.
#' @export
#' @examples
#' str_qtr(now())
#' str_qtr(as.Date("2003-03-02"))
str_qtr <- function (d) {
  str_c(year(d), "-Q", quarter(d))
}

#' The current quarter
#' @export
thisQ <- str_qtr(now())

#' The previous quarter
#' @export
lastQ <- str_qtr(now() %m-% months(3))

#' A source folder for data usiny my convention for where it's stored.
#' @export
src_dir <- function (qtr, folder="Reports", base_dir="..") {
  file.path(base_dir, qtr, folder)
}

#' Find the latest version of a file with a given pattern in a given folder.
#' This is typically used for files that start with a ymd date or a monotonically
#' increasing string, e.g. \code{yyyy-mm-dd filename.ext}.
#' @param srcDir The search directory.
#' @param pattern A regular expression matching the file you want to find.
#' @return The full path of the matching file, or NULL.
#' @export
find_latest <- function (srcDir=".", pattern) {
  files <- sort(list.files(path=srcDir, pattern=pattern))
  assertthat::assert_that(length(files) > 0)
  in_file <- file.path(srcDir, tail(files, n=1))
  return(in_file)
}

#' Read the latest CSV or XLSX file with a given pattern in a given folder.
#' @param srcDir The search directory
#' @param pattern A regular expression matching the file you want to find
#' @param skip The number of lines to skip in the input file, defaults to 0
#' @param sheet The sheet number if reading a apreadsheet, defaults to 1
#' @param R.identifiers Convert the column names to valid R identifiers, defaults to \code{FALSE}.
#' @return A data frame with the colnames either as exact strings matching the header
#' values, or as valid R identifies for compatibility with \code{read.csv()}.
#' @seealso \code{\link{find_latest}}
#' @export
read_latest <- function (srcDir=".", pattern, skip=0, sheet=1, R.identifiers=FALSE) {
  assertthat::assert_that(assertthat::is.dir(srcDir))
  in_file <- find_latest(srcDir, pattern)
  cat("Reading:", in_file, sep=" ")

  if (str_detect(in_file, "xlsx$")) {
    df <- readxl::read_excel(in_file, sheet=sheet, skip=skip)
  }
  else {
    df <- readr::read_csv(in_file, col_names=TRUE, skip=skip)
  }

  if (R.identifiers==TRUE) {
    colnames(df) <- make.names(colnames(df))
  }
  return(df)
}

#' Removes all but the most recent file with a given pattern, i.e. a directory purge a la VMS.
#' Beware of using this indiscriminately.
#' @param path The path to the folder containing the files
#' @param pattern A regular expression for the files to remove
#' @return The return value from the \code{file.remove} call.
#' @export
file_purge <- function (path, pattern="*") {
  files <- sort(list.files(path, pattern=pattern, full.names=TRUE))
  len <- length(files)
  file.remove(files[-len])
}

#' Turn strings into numbers. Also handles percentages.
#' @param x The string for converting
#' @return The converted number or NA if it's not convertible.
#' @examples
#' toNumber("9876")
#' toNumber("19%")
#' toNumber("$12,345.50")
#' @export
to_number <- function (x) {
  x <- gsub('([0-9]+)%', "0.\\1", x, fixed=FALSE)
  x <- tidyr::extract_numeric(x)
  x[is.na(x)] <- 0
  return(x)
}

#' Normalise and round to a given number of digits.
#' @param v A vector
#' @param digits The number of digits for rounding, defaults to 2.
#' @return The normalised vector.
#' @export
norm <- function (v, digits=2) {
  return(round(v/sum(v), digits=digits))
}

#' Select the first value unless zero, in which case return the second!
#' @export
xnonzero <- function (a, b) {
  ifelse(a==0, b, a)
}

#' Change NAs in a data frame to something else, or zero by default.
#' @param df The data frame
#' @param to The new value for NAs, defaults to zero
#' @return The updated data frame
#' @examples
#' df %>% change_NA(to=0)
#' df %>% change_NA(to="")
#' @export
change_NA <- function (df, to=0) {
  df[is.na(df)] <- to
  df
}

#' Show a table as ugly HTML in the Viewer pane
#' @param A data frame for printing
#' @return Nothing
#' @export
view_as_html <- function (tbl) {
  dir <- tempfile()
  dir.create(dir)
  htmlFile <- file.path(dir, "index.html")
  print(xtable::xtable(tbl), type="html", file=htmlFile, include.rownames = FALSE,
        html.table.attributes="border=1")
  rstudio::viewer(htmlFile)
}

NULL
