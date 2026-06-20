test_that("analizuj_zarobki działa", {
  wynik <- analizuj_zarobki(dane_it, "Developer")

  expect_s3_class(wynik, "data.frame")
  expect_true(all(c("stanowisko", "srednia", "minimalne", "maksymalne") %in% names(wynik)))
  expect_equal(wynik$stanowisko, "Developer")
})
