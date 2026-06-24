test_that("porownaj_doswiadczenie działa", {
  wynik <- porownaj_doswiadczenie(dane_it)

  expect_s3_class(wynik, "data.frame")
  expect_true(all(c("doswiadczenie", "srednie_zarobki") %in% names(wynik)))
  expect_true(all(c("Junior", "Mid", "Senior") %in% wynik$doswiadczenie))
})

test_that("porownaj_doswiadczenie filtruje po stanowisku", {
  wynik <- porownaj_doswiadczenie(dane_it, stanowisko = "Developer")

  expect_s3_class(wynik, "data.frame")
  expect_equal(attr(wynik, "stanowisko"), "Developer")
})

test_that("porownaj_doswiadczenie rzuca błąd dla nieistniejącego stanowiska", {
  expect_error(
    porownaj_doswiadczenie(dane_it, stanowisko = "NieIstniejeTakie"),
    "Brak takiego stanowiska"
  )
})
