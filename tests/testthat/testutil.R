# util.R unit tests

# Sanity check
expect_that(2+2, equals(4))

#---------------------------------------
context("File functions")
test_that("find_latest() finds stuff", {
  files <- find_latest(".", ".*\\.csv")
  expect_more_than(length(files), 0)
})
test_that("read_latest() reads stuff", {
  df <- read_latest(".", ".*\\.csv", R.identifiers=TRUE)
  expect_equal(nrow(df), 1)
})

#---------------------------------------
context("Conversions")
test_that("to_number() converts correctly", {
  expect_equal(to_number("123"), 123)
  expect_equal(to_number("1,234"), 1234)
  expect_equal(to_number("$1,234.50"), 1234.5)
  expect_equal(to_number(""), 0)
  expect_equal(to_number("19%"), 0.19)
  expect_equal(to_number("0%"), 0.0)
  expect_equal(to_number("0%"), 0)
})

test_that("to_number() works on lists", {
  df <- data.frame(x=c("1,234", "$5678", "$10,987"), row.names=c("a", "b", "c"))
  #expect_equal(sapply(c("1,234", "$5678"), to_number), c(1234, 5678))
})

#---------------------------------------
context("norm")
test_that("norm() works", {
  expect_equal(norm(c(1, 1)), c(0.5, 0.5))
})

#---------------------------------------
context("change_NA")
test_that("changeNA() works", {
  df <- data.frame(x=c(1, 2, NA, 3))
  df1 <- change_NA(df, to=99)
  expect_equal(df1[3,], 99)
})

# The End
