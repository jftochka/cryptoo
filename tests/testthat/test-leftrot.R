test_that("leftrot() rotates a vector", {
  expect_equal(leftrot(1:10,2),c(3:10,1:2))
})

test_that("leftrot() rotates modulo length(v)", {
  expect_equal(leftrot(1:10,12),c(3:10,1:2))
})
