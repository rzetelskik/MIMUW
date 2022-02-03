# Zadanie 4

Zmodyfikuj klienta w ten sposób, aby przyjmował dwa dodatkowe parametry: liczbę pakietów do wysłania (n) i rozmiar porcji danych (k). Klient  n razy przesyła k dowolnych bajtów do serwera.  Do skonstruowania takiego komunikatu możesz użyć funkcji ```memset()```.

Zmodyfikuj serwer w ten sposób, aby czytał w pętli komunikaty od klienta. Po otrzymaniu danych serwer dopisuje je do pliku o ustalonej nazwie, a na standardowe wyjście podaje jedynie liczbę otrzymanych bajtów.

Przetestuj różne wartości k (np. 10, 100, 1000, 5000). Zaobserwuj, czy wartości wypisywane przez serwer zależą od parametru klienta. Jeśli masz możliwość, uruchom klienta i serwera na dwóch różnych maszynach.
