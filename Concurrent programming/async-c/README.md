<span class="notifications" id="user-notifications"></span><div role="main"><span id="maincontent"></span><h2>Zadanie zaliczeniowe 2: Asynchroniczne C</h2><div id="intro" class="box generalbox boxaligncenter"><div class="no-overflow"><span class="filter_mathjaxloader_equation"><span class="nolink"><h2>Wprowadzenie</h2>

<p>Pula wątków to mechanizm pozwalający na uzyskanie współbieżnego wykonywanie wielu zadań w ramach jednego programu.
W skład puli wchodzi pewna liczba wątków roboczych (ang. <em>worker threads</em>), czekających na pojawienie się pracy do wykonania.</p>

<p>Użycie puli wątków pozwala uniknąć ciągłego powoływania i czekania na zakończenie się wątku przy wykonywaniu krótkotrwałych
zadań współbieżnych. Pozwala też na wykonywanie bardzo dużej liczby zadań niezależnie od siebie w sytuacji, gdy liczba dostępnych potoków przetwarzania jest ograniczona.</p>

<h2>Polecenie</h2>

<ul>
<li><p>Zaimplementuj pulę wątków zgodnie z poniższym opisem szczegółowym (3 pkt).</p></li>
<li><p>Zaimplementuj obliczenia <em>future</em> zgodnie z poniższym opisem szczegółowym (3 pkt).</p></li>
<li><p>Napisz program przykładowy <strong>macierz</strong>, obliczający za pomocą puli wątków sumy wierszy z zadanej tablicy (1 pkt).</p></li>
<li><p>Napisz program przykładowy <strong>silnia</strong>, obliczający za pomocą mechanizmu <em>future</em> silnię zadanej liczby (1 pkt).</p></li>
<li><p>Zadbaj, aby kod był napisany w sposób klarowny i rzetelny zgodnie z poniższymi wytycznymi. (2 pkt).</p></li>
</ul>

<h3>Szczegółowy opis puli wątków</h3>

<p>Pulę wątków należy zaimplementować jako realizację interfejsu przedstawionego w pliku <code>"threadpool.h"</code>.
Zamieszczone tam są m.in. następujące deklaracje:</p>

<pre><code class="C">typedef struct runnable {
  void (*function)(void *, size_t);
  void *arg;
  size_t argsz;
} runnable_t;

typedef struct thread_pool {

} thread_pool_t;

int thread_pool_init(thread_pool_t *pool, size_t pool_size);

void thread_pool_destroy(thread_pool_t *pool);

int defer(thread_pool_t *pool, runnable_t runnable);
</code></pre>

<p>Wywołanie <code>thread_pool_init</code> inicjuje argument wskazywany przez <code>pool</code> jako nową pulę, w której będzie funkcjonować <code>pool_size</code> wątków obsługujących zgłoszone do wykonania zadania. Za gospodarkę pamięcią wskazywaną przez <code>pool</code> odpowiada użytkownik biblioteki. Poprawność działania biblioteki jest gwarantowana tylko, jeśli każda pula stworzona przez <code>thread_pool_init</code> jest niszczona przez wywołanie <code>thread_pool_destroy</code> z argumentem reprezentującym tę pulę.</p>

<p>Wywołanie <code>defer(pool, runnable)</code> zleca puli wątków <code>pool</code> wykonanie zadania opisanego przez argument <code>runnable</code>, argumenty <code>function</code> są przekazywane przez wskaźnik <code>args</code>, w polu <code>argsz</code> znajduje się długość dostępnego do pisania i czytania buforu znajdującego się pod tym wskaźnikiem. Za zarządzanie pamięcią wskazywaną przez <code>args</code> odpowiada klient biblioteki.</p>

<p>Funkcja <code>function</code> powinna zostać obliczona przez wątek z puli <code>pool</code>; wywołanie <code>defer</code> może zablokować wywołujący je wątek, ale jedynie na potrzeby rejestracji zlecenia: powrót z <code>defer</code> jest niezależny od powrotu z wykonania <code>function</code> przez pulę.</p>

<p>Zadania zlecone do wykonania przez <code>defer</code> powinny móc wykonywać się współbieżnie i na tyle niezależnie od siebie, na ile to możliwe.
Można ograniczyć liczbę współbieżnie wykonywanych zadań do rozmiaru puli. Pula w czasie swojego działania nie powinna powoływać więcej wątków niż określono parametrem <code>pool_size</code>. Utworzone wątki są utrzymywane aż do wywołania <code>thread_pool_destroy</code>.</p>

<p>Zastanów się nad tym, jak zrealizować powyższą bibliotekę tak, aby wykonywała się możliwie sprawnie na współczesnych komputerach. Postaraj się zrealizować taką implementację.</p>

<h3>Szczegółowy opis mechanizmu obliczeń <em>future</em></h3>

<p>Przy pomocy puli wątków i operacji <code>defer</code> należy zaimplenentować asynchroniczne obliczenia <code>future</code> jako realizację interfejsu przedstawionego w pliku  <code>"future.h"</code>. Zamieszczone są tam m.in. następujące deklaracje:</p>

<pre><code class="C">typedef struct callable {
  void *(*function)(void *, size_t, size_t *);
  void *arg;
  size_t argsz;
} callable_t;

typedef struct future {
} future_t;

int async(thread_pool_t *pool, future_t *future, callable_t callable);

int map(thread_pool_t *pool, future_t *future, future_t *from,
        void *(*function)(void *, size_t, size_t *));

void *await(future_t *future);
</code></pre>

<p>Wywołanie <code>int err = async(pool, future_value, callable)</code> inicjuje pamięć wskazywaną przez <code>future_value</code>. Za zarządanie tą pamięcią odpowiada użytkownik biblioteki. Na puli <code>pool</code> zlecane jest wykonanie <code>function</code> z argumentu <code>callable</code>. Funkcja <code>function</code> zwraca wskaźnik do wyniku. Użytkownik biblioteki powinien zadbać, żeby poprawnie ustawiła też rozmiar wyniku wykorzystując do tego celu trzeci argument typu <code>size_t*</code>.</p>

<p>Wołający może teraz:</p>

<ul>
<li><p>Zaczekać na zakończenie wykonania funkcji <code>function</code> przez wywołanie:</p>

<pre><code class="C">void *result = await(future_value);
</code></pre>

<p>Za gospodarkę pamięcią wskazywaną przez wskaźnik <code>result</code> odpowiada użytkownik biblioteki (pamięć ta może zostać przekazana do funkcji <code>function</code> za pomocą jej argumentów lub w tej funkcji zaalokowana).</p></li>
<li><p>Zlecić jakiejś puli, niekoniecznie tej, która zainicjowała <code>future_value</code>, wywołanie innej funkcji na wyniku:</p>

<pre><code class="C">err = map(pool2, mapped_value, future_value, function2);
</code></pre></li>
</ul>

<p>Programy, w których aktywnie działa jakaś pula wątków, powinny mieć automatycznie ustawioną obsługę sygnałów. Ta obsługa powinna zapewniać, że program po otrzymaniu sygnału (SIGINT) zablokuje możliwość dodawania nowych zadań do działających pul, dokończy wszystkie obliczenia zlecone dotąd działającym pulom, a następnie zniszczy działające pule.</p>

<p>Dla ułatwienia implementacji można założyć, że zaimplementowana biblioteka będzie testowana w taki sposób, iż wątki nie będą ginęły w testach.</p>

<h3>Opis programu <strong>macierz</strong></h3>

<p>Program <strong>macierz</strong> ma ze standardowego wejścia wczytywać dwie liczby k oraz n, każda w osobnym wierszu. Liczby te oznaczają odpowiednio liczbę wierszy oraz kolumn macierzy. Następnie program ma wczytać k*n linijek z danymi, z których każda zawiera dwie, oddzielone spacją liczby: v, t. Liczba v umieszczona w linijce i (numerację linijek zaczynamy od 0) określa wartość macierzy z wiersza floor(i/n) (numerację kolumn i wierszy zaczynamy od 0) oraz kolumny i mod n. Liczba t to liczba milisekund, jakie są potrzebne do obliczenia wartości v. Oto przykładowe poprawne dane wejściowe:</p>

<pre><code class="bash">2
3
1 2
1 5
12 4
23 9
3 11
7 2
</code></pre>

<p>Takie dane wejściowe tworzą macierz od dwóch wierszach i trzech kolumnach:</p>

<pre><code class="bash">|  1  1 12 |
| 23  3  7 |
</code></pre>

<p>Program ma za zadanie wczytać tak sformatowane wejście (można zakładać, że podawane będą tylko poprawne dane), a następnie za pomocą puli wątków zawierającej 4 wątki policzyć sumy wierszy, przy czym pojedyncze zadanie obliczeniowe powinno podawać w wyniku wartość pojedynczej komórki macierzy, odczekawszy liczbę milisekund, które zostały wczytane jako potrzebne do obliczenia tej wartości (np. zadanie obliczeniowe wyliczenia wartości 3 z macierzy powyżej powinno odczekiwać 11 milisekund). Po obliczeniu należy wypisać sumy kolejnych wierszy na standardowe wyjście, po jednej sumie w wierszu. Dla przykładowej macierzy powyżej umieszczonej w pliku <code>data1.dat</code> wywołanie:</p>

<pre><code class="bash">$ cat data1.dat | ./macierz
</code></pre>

<p>powinno spowodować pojawienie się na wyjściu</p>

<pre><code>14
33
</code></pre>

<h3>Opis programu <strong>silnia</strong></h3>

<p>Program <strong>silnia</strong> powinien wczytywać ze standardowego wejścia pojedynczą liczbę n, a następnie obliczać za pomocą puli 3 wątków liczbę n!. Po obliczeniu tej liczby wynik powinien zostać wypisany na standardowe wyjście. Program powinien wyliczać silnię, wykorzystując funkcję <code>map</code> i przekazując jej w <code>future_value</code> częściowe iloczyny. Dla przykładu wywołanie:</p>

<pre><code class="bash">$ echo 5 | ./silnia
</code></pre>

<p>powinno spowodować pojawienie się na wyjściu</p>

<pre><code>120
</code></pre>

<h2>Wymagania techniczne</h2>

<p>Do synchronizacji można korzystać tylko z mechanizmów biblioteki <code>pthreads</code>.
Można korzystać z plików nagłówkowych:</p>

<pre><code>#include &lt;pthread.h&gt;
#include &lt;semaphore.h&gt;
#include &lt;stddef.h&gt;
#include &lt;stdint.h&gt;
#include &lt;stdio.h&gt;
#include &lt;stdlib.h&gt;
</code></pre>

<p>Powyższa lista może ulec rozszerzeniu, jeśli okaże się to konieczne.
Można założyć, że kod będzie kompilowany i testowany na serwerze <code>students</code>.</p>

<p>Jako rozwiązanie należy wysłać na moodla plik <code>ab123456.tar.gz</code>, gdzie <code>ab123456</code> to login na students.
W archiwum powinien znaleźć się jeden katalog o nazwie <code>ab123456</code> (login na students) z wszystkimi plikami rozwiązania.
Kod programów przykładowych należy umieścić w plikach <code>macierz.c</code> i <code>silnia.c</code>. Nie powinna być konieczna modyfikacja plików <code>CMakeLists.txt</code>.
Ciąg poleceń:</p>

<pre><code>tar -xf ab123456.tar.gz
mkdir build &amp;&amp; cd build
cmake ../ab123456
make
</code></pre>

<p>Powinien skompilować bibliotekę do pliku <code>build/libasyncc.a</code>, kompilator nie powinien wypisywać żadnych ostrzeżeń.
Następnie można przetestować swoje rozwiązanie:</p>

<pre><code>make test
</code></pre>
