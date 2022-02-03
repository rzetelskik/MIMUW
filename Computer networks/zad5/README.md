# Zadanie 5

Zmodyfikuj klienta w ten sposób, aby przyjmował dwa dodatkowe parametry: liczbę pakietów do wysłania (n) i rozmiar porcji danych (k). Klient  n razy przesyła k dowolnych bajtów do serwera.  Do skonstruowania takiego komunikatu możesz użyć funkcji ```memset()```.

Zmodyfikuj serwer w ten sposób, aby czytał w pętli komunikaty od klienta. Po otrzymaniu danych serwer wypisuje na standardowe wyjście liczbę otrzymanych bajtów. Serwer może być wywołany bez parametrów lub z jednym parametrem, który jest nazwą pliku. Jeśli serwer został wywołany z parametrem, to dopisuje dane do pliku o podanej nazwie, w przeciwnym przypadku wypisuje dane na standardowe wyjście.

Przetestuj różne wartości k (np. 10, 100, 1000, 5000). Zaobserwuj, czy wartości wypisywane przez serwer zależą od parametru klienta. Przetestuj działanie klienta i serwera na dwóch różnych maszynach.
