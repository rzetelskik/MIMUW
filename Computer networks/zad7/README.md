# Zadanie 7

W oparciu o program poll-server napisz program poll-server-telnet, którego działanie jest rozszerzone o możliwość podania liczby aktualnie połączonych klientów oraz łącznej liczby klientów obsłużonych od początku działania serwera. Pozostała funcjonalność serwera pozostaje bez zmian.

Serwer przyjmuje dwa parametry - numer portu, na którym obłsuguje klientów oraz numer portu kontrolnego (przykładowe wywołanie: ./poll-server-telnet 10001 2323).

Kiedy z serwerem łączy się program telnet na port kontrolny i podane zostanie polecenie count, serwer odsłyła informację w postaci:

```
Number of active clients: x
Total number of clients: y
```

po czym zamyka połączenie. Polecenie inne niż count jest ignorowane i połączenie jest również zamykane. Połączenia kontrolne nie liczą się jako połączenia z klientami.
