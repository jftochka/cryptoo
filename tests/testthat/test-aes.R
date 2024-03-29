test_that("aes NIST sample test 128 bit key", {
  aes128_nist_in  <- seq(0,255,17)
  aes128_nist_key <- 0:15
  aes128_nist_out <- as.integer(c(
    0x69,0xc4,0xe0,0xd8,0x6a,0x7b,0x04,0x30,0xd8,0xcd,0xb7,0x80,0x70,0xb4,0xc5,0x5a))
  expect_equal(aes(aes128_nist_in,aes128_nist_key),aes128_nist_out)
})

test_that("aes NIST sample test 192 bit key", {
  aes192_nist_in <- seq(0,255,17)
  aes192_nist_key <- 0:23
  aes192_nist_out <- as.integer(c(
    0xdd,0xa9,0x7c,0xa4,0x86,0x4c,0xdf,0xe0,0x6e,0xaf,0x70,0xa0,0xec,0x0d,0x71,0x91))
  expect_equal(aes(aes192_nist_in,aes192_nist_key),aes192_nist_out)
})

test_that("aes NIST sample test 256 bit key", {
  aes256_nist_in <- seq(0,255,17)
  aes256_nist_key <- 0:31
  aes256_nist_out <- as.integer(c(
    0x8e,0xa2,0xb7,0xca,0x51,0x67,0x45,0xbf,0xea,0xfc,0x49,0x90,0x4b,0x49,0x60,0x89))
  expect_equal(aes(aes256_nist_in,aes256_nist_key),aes256_nist_out)
})
