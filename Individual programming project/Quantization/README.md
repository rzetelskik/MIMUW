<span class="notifications" id="user-notifications"></span><div role="main"><span id="maincontent"></span><h2>Małe zadanie</h2><div id="intro" class="box generalbox boxaligncenter"><div class="no-overflow"><p>Celem zadania jest napisanie programu opisującego zachowanie cząstki w pewnym
wyimaginowanym świecie kwantowym. Cząstka może przebywać w czterech stanach
kwantowych, oznaczanych liczbami 0, 1, 2 i 3. Kwantowa historia cząstki jest to
niepusty ciąg kolejnych stanów kwantowych, w jakich cząstka przebywała, ale
tylko niektóre kwantowe historie cząstki są dopuszczalne. Dopuszczalnej
kwantowej historii cząstki można przypisać skwantowaną energię. Energię tę można
zmieniać. Niedopuszczalna kwantowa historia cząstki nie może mieć przypisanej
energii. Energie dwóch dopuszczalnych kwantowych historii cząstki mogą zostać
zrównane, co oznacza, że zmiana energii dla jednej z tych historii powoduje też
zmianę energii drugiej z nich. Relacja zrównania energii przypisanych historiom
jest równoważnością.</p>

<h2>Opis działania programu</h2>

<p>Po uruchomieniu programu nie są dopuszczalne żadne kwantowe historie cząstki,
zatem żadnej historii nie jest przypisana energia i żadne dwie energie nie są
zrównane. W czasie działania program czyta ze standardowego wejścia i wykonuje
następujące polecenia:</p>

<ul>
<li><p><code>DECLARE history</code> –
Powoduje, że każda kwantowa historia cząstki, będąca prefiksem podanej jako
parametr <code>history</code> kwantowej historii cząstki, staje się dopuszczalna.</p></li>
<li><p><code>REMOVE history</code> –
Powoduje, że każda kwantowa historia cząstki, mająca prefiks podany jako
parametr <code>history</code>, przestaje być dopuszczalna.</p></li>
<li><p><code>VALID history</code> –
Sprawdza, czy podana kwantowa historia cząstki jest dopuszczalna.</p></li>
<li><p><code>ENERGY history energy</code> –
Jeśli podana w parametrze <code>history</code> kwantowa historia cząstki jest
dopuszczalna i historia ta nie ma przypisanej energii, to przypisuje jej
wartość energii podanej jako parametr <code>energy</code>. Jeśli ta kwantowa historia
cząstki ma już przypisaną energię, to modyfikuje wartość tej energii.</p></li>
<li><p><code>ENERGY history</code> –
Jeśli podana w parametrze <code>history</code> kwantowa historia cząstki jest
dopuszczalna i historia ta ma przypisaną energię, to wypisuje wartość tej
energii.</p></li>
<li><p><code>EQUAL history_a history_b</code> –
Jeśli podane w parametrach <code>history_a</code> i <code>history_b</code> kwantowe historie cząstki
są dopuszczalne, to: (a) jeśli chociaż jedna z nich ma przypisaną energię,
zrównuje ich energie i domyka relację równoważności; (b) jeśli obie historie
przed wykonaniem tego polecenia mają przypisane energie, to po wykonaniu
polecenia energia przypisana tym historiom jest średnią arytmetyczną tych
energii (z ewentualnym zaokrągleniem w dół); (c) jeśli obie historie są
identyczne lub mają już zrównane energie, niczego nie zmienia.</p></li>
</ul>

<p>Poprawne dane wejściowe spełniają następujące dodatkowe warunki:</p>

<ul>
<li>Parametr opisujący kwantową historię cząstki jest niepustym ciągiem
składającym się z cyfr 0, 1, 2 i 3.</li>
<li>Energia jest dziesiętną liczbą całkowitą z przedziału od 1 do 2^64 − 1.</li>
<li>Nazwa polecenia i jego parametry są oddzielone pojedynczą spacją, a każdy
wiersz wejścia zawiera co najwyżej jedno polecenie i kończy się linuksowym
znakiem przejścia do nowej linii (znak <code>\n</code> w C, kod ASCII 10). Są to jedyne
białe znaki występujące w wierszu.</li>
</ul>

<p>Ponadto:</p>

<ul>
<li>Puste wiersze należy ignorować.</li>
<li>Wiersze rozpoczynające się znakiem <code>#</code> należy ignorować.</li>
</ul>

<h2>Informacje wypisywane przez program i obsługa błędów</h2>

<p>Program kwituje poprawne wykonanie polecenia, wypisując informację na
standardowe wyjście:</p>

<ul>
<li>Dla polecenia innego niż polecenie <code>VALID</code> lub jednoparametrowe polecenie
<code>ENERGY</code> wiersz ze słowem <code>OK</code>.</li>
<li>Dla polecenia <code>VALID</code> wiersz ze słowem <code>YES</code> lub <code>NO</code> zależnie od wyniku tego
polecenia.</li>
<li>Dla jednoparametrowego polecenia <code>ENERGY</code> dziesiętną liczbę całkowitą będącą
wartością energii.</li>
<li>Każdy wiersz wyjścia powinien kończyć się linuksowym znakiem przejścia do
nowej linii (znak <code>\n</code> w C, kod ASCII 10). Jest to jedyny biały znak, jaki
może pojawić się na wyjściu.</li>
</ul>

<p>Program wypisuje informacje o błędach na standardowe wyjście diagnostyczne:</p>

<ul>
<li>Dla każdego błędnego wiersza i dla każdego polecenia, które nie może być
wykonane, gdyż nie spełnia wyżej opisanych warunków, np. z powodu złej liczby
parametrów lub błędnej wartości parametru, należy wypisać wiersz ze słowem
<code>ERROR</code>, zakończony linuksowym znakiem końca linii (znak <code>\n</code> w C, kod ASCII
10). Jest to jedyny biały znak, jaki może pojawić się na wyjściu.</li>
</ul>

<h2>Przykładowe dane</h2>

<p>Przykładowe dane dla programu znajdują się w załączonych plikach.</p>

<h2>Zakończenie programu</h2>

<p>Program kończy się po przetworzeniu wszystkich poleceń z wejścia. Program
powinien wtedy zwolnić całą zaalokowaną pamięć i zakończyć się kodem 0.
Awaryjne zakończenie programu, np. na skutek niemożliwości zaalokowania
potrzebnej pamięci, powinno być sygnalizowane kodem 1.</p>

<h2>Makefile</h2>

<p>Częścią zadania jest napisanie pliku <code>makefile</code>. W wyniku wywołania polecenia
<code>make</code> powinien powstać program wykonywalny <code>quantization</code>. Jeśli któryś
z plików źródłowych ulegnie zmianie, ponowne wpisanie <code>make</code> powinno na nowo
stworzyć plik wykonywalny. Plik <code>makefile</code> powinien działać w następujący
sposób:</p>

<ul>
<li>osobno kompiluje każdy plik <code>.c</code>,</li>
<li>linkuje wszystkie pliki <code>.o</code>,</li>
<li>przy zmianie w pliku <code>.c</code> lub <code>.h</code> wykonuje tylko niezbędne akcje,</li>
<li>wywołanie <code>make clean</code> usuwa plik wykonywalny i dodatkowe pliki powstałe
podczas kompilowania.</li>
</ul>

<p>Docelowo pliki rozwiązania należy kompilować programem <code>gcc</code> z opcjami:</p>

<pre><code>-Wall -Wextra -std=c11 -O2
</code></pre>

<h2>Skrypt testujący</h2>

<p>Osobną częścią zadania jest napisanie skryptu <code>test.sh</code>. Po wywołaniu</p>

<pre><code class="sh">./test.sh prog dir
</code></pre>

<p>skrypt powinien uruchomić program <code>prog</code> dla wszystkich plików wejściowych
postaci <code>dir/*.in</code>, porównać wyniki z odpowiadającymi im plikami <code>dir/*.out</code>
i <code>dir/*.err</code>, a następnie wypisać, które testy zakończyły się powodzeniem,
a które niepowodzeniem. Skrypt powinien akceptować parametry z dowolną
ścieżką, jaką akceptuje powłoka.</p>

<p>Do wykrywania problemów z zarządzaniem pamięcią należy użyć programu <code>valgrind</code>.</p>

<h2>Pozostałe wymagania</h2>

<p>Rozwiązanie zadania powinno być napisane w języku C i korzystać z dynamicznie
alokowanych struktur danych. Implementacja powinna być jak najefektywniejsza.
Należy unikać zbędnego alokowania pamięci i kopiowania danych.</p>

<p>Kod programu program powinien być podzielony na moduły.</p>

<p>Moduł zwykle składa się z dwóch plików, np. <code>x.c</code> i <code>x.h</code>, gdzie <code>x</code> jest nazwą
modułu, implementowanej przez ten moduł struktury danych lub tp.
Plik nagłówkowy <code>x.h</code> zawiera deklaracje operacji, struktur udostępnianych przez
moduł <code>x</code>, a plik <code>x.c</code> – ich implementację. W pliku nagłówkowym należy
umieszczać jedynie deklaracje i definicje, które są częścią interfejsu tego
modułu. Wszystkie szczegóły powinny być ukryte w pliku z implementacją.</p>

<p>Moduł może też składać się z samego pliku nagłówkowego, jeśli udostępnia jedynie
definicje stałych bądź typów, lub funkcji, które sugerujemy kompilatorowi do
rozwijania w miejscu wywołania (<code>static inline</code>).</p>

<p>Moduł może też składać się z samego pliku z implementacją, jeśli nie udostępnia
żadnego interfejsu – żadne funkcje z tego modułu nie są wywoływane z innych
modułów.</p>

<p>Ponadto rozwiązanie powinno zawierać pliki:</p>

<ul>
<li><code>makefile</code> – Patrz punkt „makefile”.</li>
<li><code>test.sh</code> – Patrz punkt „skrypt testujący”.</li>
</ul>

<p>Rozwiązanie należy oddać jako archiwum skompresowane programem <code>zip</code> lub parą
programów <code>tar</code> i <code>gzip</code>.</p>
