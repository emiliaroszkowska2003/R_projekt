#' Analiza zarobków
#'
#' Funkcja filtruje dane według stanowiska i oblicza podstawowe statystyki wynagrodzeń.
#'
#' @param dane data frame zawierający kolumny stanowisko i zarobki
#' @param wybrane_stanowisko nazwa stanowiska (np. "Developer")
#'
#' @return obiekt klasy \code{analiza_zarobkow} (data.frame ze statystykami:
#'   średnia, minimum i maksimum)
#'
#' @export
#'
#' @examples
#' analizuj_zarobki(dane_it, "Developer")
analizuj_zarobki <- function(dane, wybrane_stanowisko) {
  dane_filtrowane <- dane[dane$stanowisko == wybrane_stanowisko, ]

  if (nrow(dane_filtrowane) == 0) {
    stop("Brak podanego stanowiska w bazie danych!")
  }

  wynik <- data.frame(
    stanowisko = wybrane_stanowisko,
    srednia = mean(dane_filtrowane$zarobki),
    minimalne = min(dane_filtrowane$zarobki),
    maksymalne = max(dane_filtrowane$zarobki)
  )
  class(wynik) <- c("analiza_zarobkow", class(wynik))
  wynik
}

#' @export
print.analiza_zarobkow <- function(x, ...) {
  cat(
    "Statystyki dla stanowiska ", x$stanowisko, ":\n",
    "- Średnia: ", x$srednia, " zł\n",
    "- Minimalne: ", x$minimalne, " zł\n",
    "- Maksymalne: ", x$maksymalne, " zł\n",
    sep = ""
  )
  invisible(x)
}

#' Porównanie doświadczenia
#'
#' Funkcja oblicza średnie zarobki dla poziomów doświadczenia.
#'
#' @param dane data frame z kolumnami doswiadczenie i zarobki
#' @param stanowisko opcjonalny filtr po stanowisku
#'
#' @return obiekt klasy \code{porownanie_doswiadczenia} (data.frame ze
#'   średnimi zarobkami wg doświadczenia)
#'
#' @export
#'
#' @examples
#' porownaj_doswiadczenie(dane_it)
porownaj_doswiadczenie <- function(dane, stanowisko = NULL) {
  if (!is.null(stanowisko)) {
    dane <- dane[dane$stanowisko == stanowisko, ]
    if (nrow(dane) == 0) {
      stop("Brak takiego stanowiska w danych!")
    }
  }

  tabela <- aggregate(zarobki ~ doswiadczenie, data = dane, FUN = mean)
  colnames(tabela)[2] <- "srednie_zarobki"

  attr(tabela, "stanowisko") <- if (is.null(stanowisko)) "wszystkich stanowisk" else stanowisko
  class(tabela) <- c("porownanie_doswiadczenia", class(tabela))
  tabela
}

#' @export
print.porownanie_doswiadczenia <- function(x, ...) {
  cat("Średnie zarobki według doświadczenia dla ", attr(x, "stanowisko"), ":\n", sep = "")
  for (i in seq_len(nrow(x))) {
    cat("- ", x$doswiadczenie[i], ": ", x$srednie_zarobki[i], " zł\n", sep = "")
  }
  invisible(x)
}
