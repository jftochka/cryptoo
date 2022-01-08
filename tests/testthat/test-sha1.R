test_that("empty string test", {
  expect_equal(sha1(utf8ToInt("")),"da39a3ee5e6b4b0d3255bfef95601890afd80709")
})
