# Zadanie 6

Napisz programy do przesyłania plików: file-server-tcp i file-client-tcp.

Klient przyjmuje trzy parametry - adres ip lub nazwę serwera, port serwera i nazwę pliku do przesłania. Klient łączy się z serwerem, przesyła mu nazwę pliku i jego rozmiar, a następnie przesyła plik i kończy działanie.

Serwer przyjmuje jeden parametr - numer portu, na którym nasłuchuje. Serwer działa w pętli nieskończonej. Dla każdego klienta tworzy osobny wątek. Wątek obsługujący klienta wypisuje na standardowe wyjście:

```
new client [adres_ip_klienta:port_klienta] size=[rozmiar_pliku] file=[nazwa_pliku]
```

Następnie odczekuje 1 sekundę, odbiera plik, zapisuje go pod wskazaną nazwą i wypisuje na standardowe wyjście:

```
client [adres_ip_klienta:port_klienta] has sent its file of size=[rozmiar_pliku]
total size of uploaded files [suma_rozmiarów_odebranych_plików]
```

Po czym wątek kończy działanie.

Przetestuj swoje rozwiązanie uruchamiając klienta i serwera na różnych komputerach. Spróbuj jednocześnie przesłać do jednego serwera kilka dużych plików.
