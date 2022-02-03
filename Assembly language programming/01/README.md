<h2 align=center>Zadanie 1: automat komórkowy</h2>

<p>Naszym celem będzie napisanie funkcji realizujących symulację automatu
komórkowego Conwaya, tzw. 
<a href="https://pl.wikipedia.org/wiki/Gra_w_%C5%BCycie">,,gry w życie''</a>.
Gra odbywa się na prostokątnej planszy składającej się z kwadratowych komórek.
Każda komórka może być żywa lub martwa.

<p>Czas jest dyskretny, w każdym kroku symulacji:
<ul>
<li>Każda żywa komórka, która ma 4 lub więcej żywych sąsiadów ,,umiera''
    z powodu tłoku.
<li>Każda żywa komórka, która ma mniej niż 2 sąsiadów ,,umiera'' z
    osamotnienia.
<li>Jeśli martwa komórka ma dokładnie 3 żywych sąsiadów, to ,,ożywa''.
</ul>

<p>Przez sąsiadów komórki rozumiemy 8 komórek bezpośrednio otaczających ją.

<p>Część napisana w języku wewnętrznym powinna eksportować procedury
wołane z C:
<ul>
<li><p><code>void start (int szer, int wys, char* T)</code>
    
    <p>Przygotowuje symulację od strony asemblera.
<li><p><code>void run (int ile_kroków)</code>

    <p>Przeprowadza podaną liczbę kroków symulacji, po ich wykonaniu
    tablica <code>T</code> (przekazana przez <code>start</code>)
    zawiera stan końcowy.
</ul>

<p>Dokładna postać wewnętrzna ,,tablicy'' T nie jest określona, powinno 
być jednak możliwe jej łatwe zainicjowanie w programie w C przez wczytanie
początkowej zawartości z pliku zawierającego kolejno:
<ul>
<li>liczbę kolumn (szerokość) i wierszy (wysokość) w pierwszym wierszu;
<li>W kolejnych wierszach wiersze planszy (stan początkowy) w postaci zer 
  (martwa komórka) i jedynek (żywa komórka), cyfry rozdzielamy spacją, 
  a każdy wiersz jest w osobnej linii tekstu.
</ul>
Deklaracja <code>char*</code> oznacza tylko tyle, że oczekujemy 
wskaźnika (adresu) do pewnego obszaru reprezentującego planszę (nie musi
to być koniecznie dwuwymiarowa tablica znaków.

<p>Testowy program główny napisany w C powinien zainicjować ,,tablicę'' T
i rozpocząć symulację.  Po każdym wywołaniu procedury <code>run</code>
powinno się wyświetlić aktualną sytuację -- może być tekstowo, czyli gwiazdki
i spacje lub tp.

<p>Rozwiązania nie zawierające pliku <code>Makefile</code> nie będą
sprawdzane.

<p>Rozwiązania (procedury w asemblerze i program w C wraz z przykładowymi 
testami) należy wysłać do dnia 3 grudnia (23:59) pocztą na 
<tt>zbyszek@mimuw.edu.pl</tt> jako <font color=red>pojedynczy 
załącznik</font> -- archiwum o nazwie wskazującej na autora (np.
<code>ab123456-zad1.tgz</code>), spakowane z osobnego katalogu o tej samej
nazwie (ale bez tgz).  Program ma działać w środowisku zainstalowanym 
w laboratoriach w trybie 64-bitowym.
