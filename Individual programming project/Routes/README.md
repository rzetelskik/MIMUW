<div role="main"><span id="maincontent"></span><h2>Duże zadanie, część 1</h2><div id="intro" class="box py-3 generalbox boxaligncenter"><div class="no-overflow"><h1>Zadanie drogi, część 1</h1>

<p>Tegoroczne duże zadanie polega na zaimplementowaniu obsługi map dróg krajowych.
Na potrzeby tego zadania przyjmujemy następujące definicje.</p>

<p>Mapa dróg jest zbiorem miast (ang. city) połączonych odcinkami dróg (ang. road).</p>

<p>Miasto reprezentowane jest przez jego nazwę, która jest niepustym napisem
w stylu C niezawierającym kodów od 0 do 31 ani średnika i zakończonym zerem.</p>

<p>Każdy odcinek drogi łączy dwa różne miasta. Między parą miast może być co
najwyżej jeden odcinek drogi. Odcinek drogi ma dwa atrybuty: długość, która jest
dodatnią liczbą całkowitą i rok, który jest rokiem budowy lub rokiem ostatniego
remontu i jest liczbą całkowitą. Wartości dodatnie reprezentują lata n.e.,
a wartości ujemne to lata p.n.e. Uwaga: nie było roku 0.</p>

<p>Droga jest to ciąg odcinków drogowych łączących dwa różne miasta, bez przerw,
samoprzecięć i pętli.</p>

<p>Droga krajowa (ang. route) jest to droga. Jeden odcinek drogi może należeć do
wielu dróg krajowych. Droga krajowa identyfikowana jest przez jej numer, który
jest liczbą całkowitą z przedziału od 1 do 999. Długość drogi krajowej to suma
długości jej odcinków.</p>

<p>Jako pierwszą część zadania należy zaimplementować moduł operacji na mapach
drogowych. Opis interfejsu modułu znajduje się w pliku <code>src/map.h</code> w formacie
komentarzy dla programu <code><a class="autolink" title="Doxygen" href="https://moodle.mimuw.edu.pl/mod/page/view.php?id=13379">doxygen</a></code>. Przykład użycia znajduje się w pliku
<code>src/map_main.c</code>.</p>

<h2>Dostarczamy</h2>

<p>W repozytorium <code>https://<a class="autolink" title="Git" href="https://moodle.mimuw.edu.pl/mod/page/view.php?id=13375">git</a>.mimuw.edu.pl/IPP-login.<a class="autolink" title="Git" href="https://moodle.mimuw.edu.pl/mod/page/view.php?id=13375">git</a></code> (gdzie login to
identyfikator używany do logowania się w laboratorium komputerowym) znajduje się
szablon implementacji rozwiązania tego zadania. Znajdują się tam następujące
pliki:</p>

<ul>
<li><code>src/map.h</code> – deklaracja interfejsu modułu wraz z jego dokumentacją
w formacie <code><a class="autolink" title="Doxygen" href="https://moodle.mimuw.edu.pl/mod/page/view.php?id=13379">doxygen</a></code>,</li>
<li><code>src/map_main.c</code> – przykład użycia modułu,</li>
<li><code>CMakeLists.txt</code> – plik konfiguracyjny programu <code><a class="autolink" title="CMake" href="https://moodle.mimuw.edu.pl/mod/page/view.php?id=13378">cmake</a></code>,</li>
<li><code>Doxyfile.in</code> – plik konfiguracyjny programu <code><a class="autolink" title="Doxygen" href="https://moodle.mimuw.edu.pl/mod/page/view.php?id=13379">doxygen</a></code>,</li>
<li><code>MainPage.dox</code> – strona główna dokumentacji w formacie <code><a class="autolink" title="Doxygen" href="https://moodle.mimuw.edu.pl/mod/page/view.php?id=13379">doxygen</a></code>.</li>
</ul>

<p>Zastrzegamy sobie możliwość nanoszenia poprawek do tego szablonu.
Będziemy je umieszczać w gałęzi <code>template/part1</code>.
Lista poprawek:</p>

<ul>
<li>na razie nie ma żadnych poprawek.</li>
</ul>

<h2>Wymagamy</h2>

<p>Jako rozwiązanie części 1 zadania wymagamy:</p>

<ul>
<li>uzupełnienia implementacji w pliku <code>src/map.h</code>,</li>
<li>stworzenia pliku <code>src/map.c</code> z implementacją modułu,</li>
<li>uzupełnienia dokumentacji w formacie <code><a class="autolink" title="Doxygen" href="https://moodle.mimuw.edu.pl/mod/page/view.php?id=13379">doxygen</a></code> tak, aby była przydatna dla
programistów rozwijających moduł.</li>
</ul>

<p>Powinna być możliwość skompilowania rozwiązania w dwóch wersjach: release
i debug. Wersję release kompiluje się za pomocą sekwencji poleceń:</p>

<pre><code class="sh">mkdir release
cd release
<a class="autolink" title="CMake" href="https://moodle.mimuw.edu.pl/mod/page/view.php?id=13378">cmake</a> ..
make
make doc
</code></pre>

<p>Wersję debug kompiluje się za pomocą sekwencji poleceń:</p>

<pre><code class="sh">mkdir debug
cd debug
<a class="autolink" title="CMake" href="https://moodle.mimuw.edu.pl/mod/page/view.php?id=13378">cmake</a> -D CMAKE_BUILD_TYPE=Debug ..
make
make doc
</code></pre>

<p>W wyniku kompilacji odpowiednio w katalogu <code>release</code> lub <code>debug</code> powinien
powstać plik wykonywalny <code>map</code> oraz dokumentacja. W poleceniu <code><a class="autolink" title="CMake" href="https://moodle.mimuw.edu.pl/mod/page/view.php?id=13378">cmake</a></code> powinno
być również możliwe jawne określenie wariantu release budowania pliku
wynikowego:</p>

<pre><code class="sh"><a class="autolink" title="CMake" href="https://moodle.mimuw.edu.pl/mod/page/view.php?id=13378">cmake</a> -D CMAKE_BUILD_TYPE=Release ..
</code></pre>

<p>Zawartość dostarczonych przez nas plików można modyfikować, o ile nie zmienia to
interfejsu modułu i zachowuje wymagania podane w treści zadania, przy czym nie
wolno usuwać opcji kompilacji: <code>-std=c11 -Wall -Wextra</code>. Zmiany mogą dotyczyć
np. stylu, dokumentacji, deklaracji typedef, włączania plików nagłówkowych,
implementacji funkcji jako <code>static inline</code>. Inne pliki źródłowe będące częścią
rozwiązania można umieścić w katalogu <code>src</code>. Funkcja <code>main</code> programu musi
znajdować się w pliku <code>src/map_main.c</code>, ale zawartość tego pliku nie będzie
oceniana w tej części zadania.</p>

<h2>Oddawanie rozwiązania</h2>

<p>Rozwiązanie należy oddawać przez wspomniane wyżej repozytorium <a class="autolink" title="Git" href="https://moodle.mimuw.edu.pl/mod/page/view.php?id=13375">git</a>.
W repozytorium mają się znaleźć wszystkie pliki niezbędne do zbudowania pliku
wykonywalnego oraz dokumentacji.
<em>W repozytorium nie wolno umieszczać plików binarnych ani tymczasowych.</em>
W Moodlu jako rozwiązanie należy umieścić tekst zawierający identyfikator
commitu finalnej wersji rozwiązania, na przykład:</p>

<pre><code>518507a7e9ea50e099b33cb6ca3d3141bc1d6638
</code></pre>

<p>Rozwiązanie należy zatwierdzić (<code><a class="autolink" title="Git" href="https://moodle.mimuw.edu.pl/mod/page/view.php?id=13375">git</a> commit</code>) i wysłać do repozytorium
(<code><a class="autolink" title="Git" href="https://moodle.mimuw.edu.pl/mod/page/view.php?id=13375">git</a> push</code>) przed terminem podanym w Moodlu.</p>

<h2>Punktacja</h2>

<p>Za w pełni poprawne rozwiązanie zadania implementujące wszystkie wymagane
funkcjonalności można zdobyć maksymalnie 20 punktów.
Od tej oceny będą odejmowane punkty za poniższe uchybienia:</p>

<ul>
<li>Za problemy ze skompilowaniem rozwiązania można stracić wszystkie punkty.</li>
<li>Za każdy test, którego program nie przejdzie, traci się do 1 punktu.</li>
<li>Za problemy z zarządzaniem pamięcią można stracić do 6 punktów.</li>
<li>Za niezgodną ze specyfikacją strukturę plików w rozwiązaniu, niezgodne ze
specyfikacją nazwy plików w rozwiązaniu lub umieszczenie w repozytorium
niepotrzebnych albo tymczasowych plików można stracić do 4 punktów.</li>
<li>Za złą jakość kodu, brzydki <a class="autolink" title="Styl" href="https://moodle.mimuw.edu.pl/mod/page/view.php?id=13358">styl</a> kodowania można stracić do 4 punktów.</li>
<li>Za ostrzeżenia wypisywane przez <a class="autolink" title="Kompilator" href="https://moodle.mimuw.edu.pl/mod/page/view.php?id=13362">kompilator</a> można stracić do 2 punktów.</li>
<li>Za braki w dokumentacji można stracić do 2 punktów.</li>
</ul>

---
<div role="main"><span id="maincontent"></span><h2>Duże zadanie, część 2</h2><div id="intro" class="box py-3 generalbox boxaligncenter"><div class="no-overflow"><h1>Zadanie drogi, część 2</h1>

<p>Jako drugą część dużego zadania należy zaimplementować program, który,
korzystając z modułu zaimplementowanego w części pierwszej, udostępnia operacje
na mapie dróg. Program obsługuje tylko jedną mapę dróg. Ponadto należy
zaimplementować skrypt w bashu.</p>

<h2>Interfejs tekstowy</h2>

<p>Program czyta dane ze standardowego wejścia, wyniki wypisuje na standardowe
wyjście, a informacje o błędach na standardowe wyjście diagnostyczne.</p>

<h3>Dane wejściowe</h3>

<p>Program akceptuje następujące polecenia.</p>

<p><code>numer drogi krajowej;nazwa miasta;długość odcinka drogi;rok budowy lub ostatniego remontu;nazwa miasta;długość odcinka drogi;rok budowy lub ostatniego remontu;nazwa miasta;…;nazwa miasta</code></p>

<p>Format polecenia jest taki sam, jak w wyniku funkcji <code>getRouteDescription</code>.
To polecenie tworzy drogę krajową o podanym numerze i przebiegu. Jeśli jakieś
miasto lub odcinek drogi nie istnieje, to go tworzy. Jeśli odcinek drogi już
istnieje, ale ma wcześniejszy rok budowy lub ostatniego remontu, to modyfikuje
ten atrybut odcinka drogi. Za błąd uznajemy, jeśli odcinek drogi już istnieje,
ale ma inną długość albo późniejszy rok budowy lub ostatniego remontu. To
polecenie niczego nie wypisuje na standardowe wyjście.</p>

<p><code>addRoad;city1;city2;length;builtYear</code></p>

<p>Wywołuje na mapie dróg funkcję <code>addRoad</code> z podanymi parametrami. Niczego nie
wypisuje na standardowe wyjście.</p>

<p><code>repairRoad;city1;city2;repairYear</code></p>

<p>Wywołuje na mapie dróg funkcję <code>repairRoad</code> z podanymi parametrami. Niczego nie
wypisuje na standardowe wyjście.</p>

<p><code>getRouteDescription;routeId</code></p>

<p>Wywołuje na mapie dróg funkcję <code>getRouteDescription</code> z podanym parametrem.
Jeśli wynik działania tej funkcji jest inny niż NULL, to wypisuje na standardowe
wyjście jedną linię z wynikiem działania tej funkcji.</p>

<p>Każde polecenie znajduje się w osobnej linii. Puste linie i linie zaczynające
się znakiem <code>#</code> należy ignorować. W poleceniach nazwa miasta jest niepustym
napisem niezawierającym kodów od 0 do 31 ani średnika, liczby są zapisywane
przy podstawie 10. Spacje w nazwie miasta są istotne.</p>

<h3>Obsługa błędów</h3>

<p>Jeśli polecenie jest niepoprawne składniowo lub jego wykonanie zakończyło się
błędem, czyli odpowiednia funkcja zakończyła się wynikiem <code>false</code> lub <code>NULL</code>, to
wypisuje na standardowe wyjście diagnostyczne jednoliniowy komunikat</p>

<p><code>ERROR n</code></p>

<p>gdzie <code>n</code> jest numerem linii w danych wejściowych zawierającym to polecenie.
Linie numerujemy od jedynki i uwzględniamy ignorowane linie.</p>

<h2>Zakończenie działania programu</h2>

<p>Program po przetworzeniu wszystkich danych wejściowych powinien zakończyć się
kodem wyjścia 0 (ang. <em>exit code</em>) i powinien zwolnić całą zaalokowaną pamięć.</p>

<h2>Skrypt</h2>

<p>Należy napisać skrypt, którego pierwszy parametr wskazuje (nazwa poprzedzona
opcjonalnie ścieżką) na plik z wynikami działania funkcji <code>getRouteDescription</code>,
każdy wynik w osobnej linii. Kolejne parametry (przynajmniej jeden) to numery
dróg krajowych.</p>

<p>Skrypt dla każdego podanego w argumentach numeru drogi krajowej po kolei szuka
w podanym pliku informacji o tej drodze krajowej, wylicza jej długość i wypisuje
jedną linię w formacie:</p>

<p><code>numer drogi krajowej;długość</code></p>

<p>Jeśli w pliku nie ma informacji o żądanej drodze krajowej, to skrypt nic dla
niej nie wypisuje.</p>

<p>Zakładamy, że zawartość pliku wskazanego pierwszym parametrem skryptu jest
poprawna. W szczególności dla danego numeru drogi krajowej jest w nim co
najwyżej jedna o niej informacja. Natomiast skrypt powinien jak najdokładniej
sprawdzać poprawność parametrów i jeśli jest ich za mało lub któryś z nich jest
niepoprawny, to powinien zakończyć się kodem wyjścia 1 (ang. <em>exit code</em>). Jeśli
parametry są poprawne, skrypt powinien zakończyć się kodem wyjścia 0.</p>

<h2>Dostarczamy</h2>

<p>Nie dostarczamy żadnego kodu źródłowego. Rozwiązanie drugiej części zadania
powinno korzystać z własnego, ewentualnie samodzielnie zmodyfikowanego,
rozwiązania części pierwszej.</p>

<h2>Wymagamy</h2>

<p>Jako rozwiązanie części 2 zadania wymagamy:</p>

<ul>
<li>umieszczenia kodu źródłowego implementacji w katalogu <code>src</code>,</li>
<li>uzupełnienia dokumentacji w formacie <code><a class="autolink" title="Doxygen" href="https://moodle.mimuw.edu.pl/mod/page/view.php?id=13379">doxygen</a></code> tak, aby była przydatna dla
programistów rozwijających program,</li>
<li>dostosowania pliku konfiguracyjnego dla programu <code><a class="autolink" title="CMake" href="https://moodle.mimuw.edu.pl/mod/page/view.php?id=13378">cmake</a></code>,</li>
<li>stworzenia skryptu o nazwie <code>map.sh</code> i umieszczenia go w głównym katalogu
rozwiązania.</li>
</ul>

<p>Gotowe rozwiązanie powinno się kompilować w dwóch wersjach: release i debug, jak
to opisano w pierwszej części zadania.</p>

<p><strong>UWAGA 1: funkcja <code>main</code> programu musi znajdować się w pliku <code>src/map_main.c</code>.</strong></p>

<p><strong>UWAGA 2: w wyniku kompilacji powinien powstać plik wykonywalny <code>map</code>.</strong></p>

<h2>Oddawanie rozwiązania</h2>

<p>Rozwiązanie należy oddawać, podobnie jak części 1, przez repozytorium <a class="autolink" title="Git" href="https://moodle.mimuw.edu.pl/mod/page/view.php?id=13375">git</a>.
W repozytorium mają się znaleźć wszystkie pliki niezbędne do zbudowania pliku
wykonywalnego i dokumentacji oraz skrypt.
<em>W repozytorium nie wolno umieszczać plików binarnych ani tymczasowych.</em>
W Moodlu jako rozwiązanie należy umieścić tekst zawierający identyfikator
commitu finalnej wersji rozwiązania, na przykład:</p>

<pre><code>518507a7e9ea50e099b33cb6ca3d3141bc1d6638
</code></pre>

<p>Rozwiązanie należy zatwierdzić (<code><a class="autolink" title="Git" href="https://moodle.mimuw.edu.pl/mod/page/view.php?id=13375">git</a> commit</code>) i wysłać do repozytorium
(<code><a class="autolink" title="Git" href="https://moodle.mimuw.edu.pl/mod/page/view.php?id=13375">git</a> push</code>) przed terminem podanym w Moodlu.</p>

<h2>Punktacja</h2>

<p>Za w pełni poprawną implementację programu można zdobyć maksymalnie 15 punktów.
Za w pełni poprawną implementację skryptu można zdobyć maksymalnie 5 punktów.
Od tej oceny będą odejmowane punkty za poniższe uchybienia:</p>

<ul>
<li>Za problemy ze skompilowaniem rozwiązania można stracić wszystkie punkty.</li>
<li>Za każdy test, którego program nie przejdzie, traci się do 1 punktu.</li>
<li>Za problemy z zarządzaniem pamięcią można stracić do 6 punktów.</li>
<li>Za niezgodną ze specyfikacją strukturę plików w rozwiązaniu, niezgodne ze
specyfikacją nazwy plików w rozwiązaniu lub umieszczenie w repozytorium
niepotrzebnych albo tymczasowych plików można stracić do 4 punktów.</li>
<li>Za złą jakość kodu, brzydki <a class="autolink" title="Styl" href="https://moodle.mimuw.edu.pl/mod/page/view.php?id=13358">styl</a> kodowania można stracić do 4 punktów.</li>
<li>Za ostrzeżenia wypisywane przez <a class="autolink" title="Kompilator" href="https://moodle.mimuw.edu.pl/mod/page/view.php?id=13362">kompilator</a> można stracić do 2 punktów.</li>
<li>Za braki w dokumentacji można stracić do 2 punktów.</li>
<li>Za brak skryptu lub jego złe działanie można stracić do 5 punktów.</li>
</ul>

---
<div role="main"><span id="maincontent"></span><h2>Duże zadanie, część 3</h2><div id="intro" class="box py-3 generalbox boxaligncenter"><div class="no-overflow"><h1>Zadanie drogi, część 3</h1>

<p>W trzeciej części zadania oczekujemy poprawienia ewentualnych błędów
z poprzednich części oraz zmodyfikowania programu. Skrypt z części drugiej
zadania nie podlega poprawie, ale ma znajdować się w rozwiązaniu. Obowiązują
ustalenia z treści poprzednich części zadania i z forum dyskusyjnego dla
studentów.</p>

<h2>Modyfikacja modułu operacji na mapie dróg</h2>

<p>Moduł <code>map</code> należy uzupełnić o następującą funkcję.</p>

<p><code>bool removeRoute(Map *map, unsigned routeId);</code></p>

<p>Usuwa z mapy dróg drogę krajową o podanym numerze, jeśli taka istnieje, dając
wynik <code>true</code>, a w przeciwnym przypadku, tzn. gdy podana droga krajowa nie
istnieje lub podany numer jest niepoprawny, niczego nie zmienia w mapie dróg,
dając wynik <code>false</code>. Nie usuwa odcinków dróg ani miast.</p>

<h2>Modyfikacja interfejsu tekstowego</h2>

<p>Należy dodać obsługę następujących, brakujących poleceń.</p>

<p><code>newRoute;routeId;city1;city2</code></p>

<p>Wywołuje na mapie dróg funkcję <code>newRoute</code> z podanymi parametrami. Niczego nie
wypisuje na standardowe wyjście.</p>

<p><code>extendRoute;routeId;city</code></p>

<p>Wywołuje na mapie dróg funkcję <code>extendRoute</code> z podanymi parametrami. Niczego nie
wypisuje na standardowe wyjście.</p>

<p><code>removeRoad;city1;city2</code></p>

<p>Wywołuje na mapie dróg funkcję <code>removeRoad</code> z podanymi parametrami. Niczego nie
wypisuje na standardowe wyjście.</p>

<p><code>removeRoute;routeId</code></p>

<p>Wywołuje na mapie dróg funkcję <code>removeRoute</code> z podanym parametrem. Niczego nie
wypisuje na standardowe wyjście.</p>

<h2>Dokumentacja</h2>

<p>Cały kod należy udokumentować w formacie <code><a class="autolink" title="Doxygen" href="https://moodle.mimuw.edu.pl/mod/page/view.php?id=13379">doxygen</a></code>.</p>

<h2>Dostarczamy</h2>

<p>Rozwiązanie części 3 zadania powinno korzystać z własnego rozwiązania
poprzednich jego części.</p>

<h2>Wymagamy</h2>

<p>Jako rozwiązanie części 3 zadania wymagamy:</p>

<ul>
<li>zachowania lub poprawienia struktury plików z poprzednich części,</li>
<li>zmodyfikowania plików <code>src/map.h</code>, <code>src/map.c</code> i ewentualnie innych plików
źródłowych z poprzednich części rozwiązania,</li>
<li>zmodyfikowania implementacji interfejsu tekstowego,</li>
<li>uzupełnienia pliku konfiguracyjnego dla programu <code><a class="autolink" title="CMake" href="https://moodle.mimuw.edu.pl/mod/page/view.php?id=13378">cmake</a></code>,</li>
<li>uzupełnienia dokumentacji w formacie <code><a class="autolink" title="Doxygen" href="https://moodle.mimuw.edu.pl/mod/page/view.php?id=13379">doxygen</a></code> tak, aby była przydatna dla
programistów rozwijających program,</li>
</ul>

<p>Gotowe rozwiązanie powinno się kompilować w dwóch wersjach: release i debug, jak
to opisano w pierwszej części zadania.</p>

<p><strong>UWAGA 1: funkcja <code>main</code> programu musi znajdować się w pliku <code>src/map_main.c</code>.</strong></p>

<p><strong>UWAGA 2: w wyniku kompilacji powinien powstać plik wykonywalny <code>map</code>.</strong></p>

<h2>Oddawanie rozwiązania</h2>

<p>Rozwiązanie należy oddawać, podobnie jak części 1 i 2, przez repozytorium <a class="autolink" title="Git" href="https://moodle.mimuw.edu.pl/mod/page/view.php?id=13375">git</a>.
W repozytorium mają się znaleźć wszystkie pliki niezbędne do zbudowania plików
wykonywalnych i dokumentacji, i tylko te pliki.
<em>W repozytorium nie wolno umieszczać plików binarnych ani tymczasowych.</em>
W Moodlu jako rozwiązanie należy umieścić tekst zawierający identyfikator
commitu finalnej wersji rozwiązania, na przykład:</p>

<pre><code>518507a7e9ea50e099b33cb6ca3d3141bc1d6638
</code></pre>

<p>Rozwiązanie należy zatwierdzić (<code><a class="autolink" title="Git" href="https://moodle.mimuw.edu.pl/mod/page/view.php?id=13375">git</a> commit</code>) i wysłać do repozytorium
(<code><a class="autolink" title="Git" href="https://moodle.mimuw.edu.pl/mod/page/view.php?id=13375">git</a> push</code>) przed terminem podanym w Moodlu.</p>

<h2>Punktacja</h2>

<p>Za w pełni poprawne rozwiązanie zadania implementujące wszystkie wymagane
funkcjonalności można zdobyć maksymalnie 20 punktów.
Od tej oceny będą odejmowane punkty za poniższe uchybienia:</p>

<ul>
<li>Za problemy ze skompilowaniem rozwiązania można stracić wszystkie punkty.</li>
<li>Za każdy test, którego program nie przejdzie, traci się do 1 punktu.</li>
<li>Za problemy z zarządzaniem pamięcią można stracić do 6 punktów.</li>
<li>Za niezgodną ze specyfikacją strukturę plików w rozwiązaniu, niezgodne ze
specyfikacją nazwy plików w rozwiązaniu lub umieszczenie w repozytorium
niepotrzebnych albo tymczasowych plików można stracić do 4 punktów.</li>
<li>Za złą jakość kodu, brzydki <a class="autolink" title="Styl" href="https://moodle.mimuw.edu.pl/mod/page/view.php?id=13358">styl</a> kodowania można stracić do 4 punktów.</li>
<li>Za ostrzeżenia wypisywane przez <a class="autolink" title="Kompilator" href="https://moodle.mimuw.edu.pl/mod/page/view.php?id=13362">kompilator</a> można stracić do 2 punktów.</li>
<li>Za braki w dokumentacji można stracić do 2 punktów.</li>
</ul>