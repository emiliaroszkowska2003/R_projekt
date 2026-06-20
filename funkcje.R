# Przykładowy zbiór danych o zarobkach w IT
dane_it <- data.frame(
  stanowisko = c("Data Scientist", "Developer", "Data Scientist", "Analyst", "Developer", "Analyst"),
  doswiadczenie = c("Junior", "Senior", "Mid", "Junior", "Mid", "Senior"),
  zarobki = c(8000, 18000, 13000, 7000, 12000, 11000)
)

#' Title: Analiza zarobkow
#' Description: Funkcja liczy statystyki dla stanowiska
#' @param dane Tabela z danymi
#' @param wybrane_stanowisko Nazwa stanowiska do analizy
#' @return
#' @returns Tekst ze statystykami
#' @export
#'
#' @examples
analizuj_zarobki <- function(dane, wybrane_stanowisko) {
  # Filtrujemy dane dla konkretnego stanowiska
  dane_filtrowane <- dane[dane$stanowisko == wybrane_stanowisko, ]
  
  # Jeśli nie ma takiego stanowiska, zwracamy ostrzeżenie
  if (nrow(dane_filtrowane) == 0) {
    stop("Brak podanego stanowiska w bazie danych!")
  }
  
  # Obliczamy statystyki
  srednia <- mean(dane_filtrowane$zarobki)
  minimalne <- min(dane_filtrowane$zarobki)
  maksymalne <- max(dane_filtrowane$zarobki)
  
  # Składamy wynik w ładny tekst
  wynik <- paste0(
    "Statystyki dla stanowiska ", wybrane_stanowisko, ":\n",
    "- Średnia: ", srednia, " zł\n",
    "- Minimalne: ", minimalne, " zł\n",
    "- Maksymalne: ", maksymalne, " zł"
  )
  
  cat(wynik) # cat wypisze tekst sformatowany z nowymi liniami
}

#' Title: Porównanie doświadczenia 
#' Description: Funkcja grupuje dane według poziomu doświadczenia pracowników i oblicza średnie zarobki dla każdej z grup.
#' @param dane Ramka danych (data frame) zawierająca kolumny 'doswiadczenie' oraz 'zarobki'.
#'
#' @returns Zwraca nową ramkę danych z dwoma kolumnami: poziomem doświadczenia oraz wyliczoną średnią pensją.
#' @export
#'
#' @examples
porownaj_doswiadczenie <- function(dane) {
  # Obliczamy średnią dla każdej grupy doświadczenia za pomocą funkcji aggregate
  tabela_porownawcza <- aggregate(zarobki ~ doswiadczenie, data = dane, FUN = mean)
  
  # Zmieniamy nazwę kolumny na bardziej czytelną
  colnames(tabela_porownawcza)[2] <- "srednie_zarobki"
  
  return(tabela_porownawcza)
}
