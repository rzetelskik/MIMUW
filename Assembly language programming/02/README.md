<h2 align=center>Zadanie 2: przepływ ,,zanieczyszczeń''</h2>

<p>Naszym celem będzie napisanie funkcji symulujących fikcyjny przepływ
pewnej wartości pseudofizycznej (o wartościach z dziedziny liczb 
rzeczywistych) przez prostokątną siatkę.  Dla ułatwienia nazwijmy tę
wartość zanieczyszczeniem.

<p>Dana jest prostokątna siatka w postaci tablicy dwuwymiarowej
(oczywiście program może na swoje potrzeby trzymać to w innej postaci,
ale mówimy o zewnętrznej, ,,logicznej'' reprezentacji).  W każdej
komórce przechowujemy bieżącą wartość zanieczyszczenia w tej okolicy.

<p>Zakładamy, że przepływ zanieczyszczeń odbywa się od lewej do prawej.
Symulacja odbywać się będzie krokowo.  W każdym kroku na lewym
brzegu tablicy pojawiają się nowe, wejściowe wartości.  We wszystkich 
pozostałych miejscach zmiana wartości następuje przez równoczesne policzenie
przyrostu (,,delty'') na podstawie wartości w sąsiednich komórkach.

<p>Dla komórek wewnętrznych za sąsiednie uznajemy 3 komórki po lewej
stronie oraz 2 komórki powyżej i poniżej.  Komórek z prawej strony
nie bierzemy pod uwagę.  Dla komórek na górnej i dolnej krawędzi
bierzemy pod uwagę tylko trzech sąsiadów.

<p><center><table border=1 width=30%>
<tbody align=center>
<tr><td>&nbsp&nbsp<td>&nbsp&nbsp<td>S<td>*<td>&nbsp&nbsp<td>...
<tr><td>&nbsp&nbsp<td>&nbsp&nbsp<td>S<td>S<td>&nbsp&nbsp<td>...
<tr><td>&nbsp&nbsp<td>&nbsp&nbsp<td>&nbsp<td>&nbsp&nbsp<td>&nbsp&nbsp<td>...
<tr><td>&nbsp&nbsp<td>&nbsp&nbsp<td>S<td>S<td>&nbsp&nbsp<td>...
<tr><td>&nbsp&nbsp<td>&nbsp&nbsp<td>S<td>*<td>&nbsp&nbsp<td>...
<tr><td>&nbsp&nbsp<td>&nbsp&nbsp<td>S<td>S<td>&nbsp&nbsp<td>...
<tr><td>&nbsp&nbsp<td>&nbsp&nbsp<td>&nbsp<td>&nbsp&nbsp<td>&nbsp&nbsp<td>...
<tr><td>&nbsp&nbsp<td>&nbsp&nbsp<td>S<td>S<td>&nbsp&nbsp<td>...
<tr><td>&nbsp&nbsp<td>&nbsp&nbsp<td>S<td>*<td>&nbsp&nbsp<td>...
<tr>
</tbody></table></center>

<p>Policzenie przyrostu dla danej komórki polega na zsumowaniu (z pewnymi 
niewielkimi wagami) różnic zanieczyszczeń między tą komórką i jej sąsiadami.
Po obliczeniu przyrostów dla wszystkich komórek są one do nich dodawane,
na lewym ,,wejściu'' pojawiają się nowe wartości i rozpoczynamy kolejny
krok. 

<p>Część napisana w języku wewnętrznym powinna eksportować procedury
wołane z C:
<ul>
<li><p><code>void start (int szer, int wys, float *M, float waga)</code>
    
    <p>Przygotowuje symulację, np. inicjuje pomocnicze struktury.
<li><p><code>void step (float T[])</code>

    <p>Przeprowadza pojedynczy krok symulacji dla podanego wejścia
    (rozmiar tablicy T jest zgodny z parametrem <code>wys</code>
    powyżej.  Po jej wykonaniu matryca <code>M</code> (przekazana przez 
    parametr <code>start</code>) zawiera nowy stan.  Tablica T zawiera
    nowe wartości na lewym brzegu.
</ul>

<p>Procedury w asemblerze powinny w jak największy stopniu wykorzystywać
możliwości instrukcji SSE (co najmniej do SSE3).  W związku z tym dokładna
postać matrycy M nie jest określona (np. mogą to być dwie macierze),
powinno być jednak możliwe jej łatwe zainicjowanie w programie w C przez
wczytanie początkowej zawartości z pliku.

<p>Testowy program główny napisany w C powinien zainicjować matrycę M
(przez wczytanie jej zawartości z pliku) i rozpocząć symulację.  
Po każdym wywołaniu procedury <code>step</code> powinno się wyświetlać 
aktualną sytuację -- może być tekstowo, jako macierz liczb.

<p>Postać danych na pliku
<hr>
<pre>
szerokość wysokość waga
pierwszy wiersz M
....
ostatni wiersz M
liczba-kroków
pierwsze dane wejściowe 
...
ostatnie dane wejściowe
</pre>
<hr>

<p>Dane wejściowe to ciągi liczb pojawiających się na lewej krawędzi.
Wszystkie liczby oddzielamy spacją.

<p>Rozwiązania nie zawierające pliku <code>Makefile</code> nie będą
sprawdzane.

<p>Rozwiązania (procedury w asemblerze i program w C wraz z przykładowymi 
testami) należy wysłać pocztą do dnia 7 stycznia 2021 na 
<tt>zbyszek@mimuw.edu.pl</tt> jako <font color=red>pojedynczy 
załącznik</font> -- archiwum o nazwie wskazującej na autora (np.
<code>ab123456-zad2.tgz</code>), spakowane z osobnego katalogu o tej samej
nazwie (ale bez tgz).  Program ma działać w środowisku zainstalowanym 
w laboratoriach w trybie 64-bitowym.
