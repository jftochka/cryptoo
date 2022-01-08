test_that("multiplication works", {
  block <- sample(c(0,1),64,1)
  key <- sample(c(0,1),64,1)
  block
  key
  crypto <- des(block,key)
  crypto
  back <- des(crypto,key,decrypt=TRUE)
  back
  expect_equal(back,block)
})
