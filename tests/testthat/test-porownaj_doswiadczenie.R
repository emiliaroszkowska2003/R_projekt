test_that("porownaj_doswiadczenie zwraca data.frame", {
  wynik <- porownaj_doswiadczenie(dane_it)
  expect_true(is.data.frame(wynik))
})
