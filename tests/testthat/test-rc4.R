test_that("sample", {
  msg <- utf8ToInt("Plaintext")
  key <- utf8ToInt("Key")
  crypto <- "bbf316e8d940af0ad3"
  expect_equal(rc4(msg,key),crypto)
})
