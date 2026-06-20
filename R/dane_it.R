
set.seed(123)

dane_it <- data.frame(
  stanowisko = sample(c("Data Scientist", "Developer", "Analyst", "Tester"), 50, replace = TRUE),
  doswiadczenie = sample(c("Junior", "Mid", "Senior"), 50, replace = TRUE),
  zarobki = sample(seq(6000, 20000, by = 500), 50, replace = TRUE),
  miasto = sample(c("Warszawa", "Kraków", "Poznań", "Wrocław"), 50, replace = TRUE)
)
