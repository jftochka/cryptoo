test_that("empty string hash", {
  expect_equal(md5(utf8ToInt("")),
               "d41d8cd98f00b204e9800998ecf8427e")
})

test_that("long(more than 1 block) string hash", {
  expect_equal(md5(utf8ToInt("The quick brown fox jumps over the lazy dog")),
               "e107d9d372bb6826bd81d3542a419d6")
})

