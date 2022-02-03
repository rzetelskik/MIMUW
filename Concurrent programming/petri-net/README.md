<span class="notifications" id="user-notifications"></span><div role="main"><span id="maincontent"></span><h2>Zadanie zaliczeniowe 1: Sieć Petriego i jej zastosowania</h2><div id="intro" class="box generalbox boxaligncenter"><div class="no-overflow"><h2 id="wprowadzenie">Wprowadzenie</h2>
<p><a href="https://en.wikipedia.org/wiki/Petri_net">Sieć Petriego</a> to dwudzielny graf skierowany, którego węzłami są <strong>miejsca</strong> (ang. <em>place</em>) i <strong>przejścia</strong> (ang. <em>transition</em>).</p>
<p>Miejsca i przejścia są połączone <strong>krawędziami</strong> (ang. <em>arc</em>).</p>
<p>W klasycznych sieciach Petriego występują <strong>krawędzie wejściowe</strong> (ang. <em>input arc</em>), prowadzące z miejsca do przejścia, oraz <strong>krawędzie wyjściowe</strong> (ang. <em>output arc</em>), prowadzące z przejścia do miejsca.</p>
<p>Krawędzie wejściowe i wyjściowe mają dodatnie całkowite <strong>wagi</strong> (ang. <em>weight</em>).</p>
<p>W sieciach rozszerzonych, które rozważamy w tym zadaniu, z miejsca do przejścia mogą też prowadzić, nie posiadające wagi, <strong>krawędzie zerujące</strong> (ang. <em>reset arc</em>) i <strong>krawędzie wzbraniające</strong> (ang. <em>inhibitor arc</em>).</p>
<p><strong>Znakowanie</strong> (ang. <em>marking</em>) sieci przyporządkowuje miejscom nieujemną całkowitą liczbę <strong>żetonów</strong> (ang. <em>token</em>).</p>
<p>W danym znakowaniu przejście jest <strong>dozwolone</strong> (ang. <em>enabled</em>), jeśli:</p>
<ul>
<li><p>w każdym miejscu, z którym rozważane przejście jest połączone krawędzią wejściową, liczba żetonów jest większa lub równa wadze tej krawędzi, oraz</p></li>
<li><p>w każdym miejscu, z którym rozważane przejście jest połączone krawędzią wzbraniającą, liczba żetonów jest równa zero.</p></li>
</ul>
<p>Sieć Petriego ma stan, reprezentowany przez aktualne znakowanie.</p>
<p>Stan sieci ulega zmianie w rezultacie <strong>odpalenia</strong> (ang. <em>fire</em>) dozwolonego przejścia.</p>
<p>Odpalenie przejścia to operacja niepodzielna, powodująca wykonanie kolejno trzech kroków:</p>
<ul>
<li><p>usunięcia z każdego miejsca, z którym odpalane przejście jest połączone krawędzią wejściową tylu żetonów, jaka jest waga tej krawędzi,</p></li>
<li><p>usunięcia wszystkich żetonów z każdego miejsca, z którym odpalane przejście jest połączone krawędzią zerującą,</p></li>
<li><p>dodania do każdego miejsca, z którym odpalane przejście jest połączone krawędzią wyjściową tylu żetonów, jaka jest waga tej krawędzi.</p></li>
</ul>
<h2 id="polecenie">Polecenie</h2>
<ul>
<li><p>Zaimplementuj sieć Petriego (6 pkt).</p></li>
<li><p>Napisz program przykładowy <strong>Alternator</strong>, demonstrujący zastosowanie sieci Petriego do modelowania systemów współbieżnych i synchronizacji wątków (2 pkt).</p></li>
<li><p>Napisz program przykładowy <strong>Multiplicator</strong>, demonstrujący zastosowanie sieci Petriego do realizacji obliczeń wielowątkowych (2 pkt).</p></li>
</ul>
<p>Rozwiązanie ma być zgodne z poniższą specyfikacją.</p>
<h2 id="specyfikacja-implementacji-sieci-petriego">Specyfikacja implementacji sieci Petriego</h2>
<p>W pakiecie <code>petrinet</code> jest bezpieczna dla wątków (ang. <em>thread-safe</em>) implementacja sieci Petriego.</p>
<p>Zdefiniowane są klasy:</p>
<pre class="sourceCode java"><code class="sourceCode java"><span class="kw">package petrinet;

import java.util.Collection;
import java.util.Map;
import java.util.Set;
...

public class PetriNet<T> {

    public PetriNet(Map<T, Integer> initial, boolean fair) {
        ...
    }

    public Set<Map<T, Integer>> reachable(Collection<Transition<T>> transitions) {
        ...
    }

    public Transition<T> fire(Collection<Transition<T>> transitions) throws InterruptedException {
        ...
    }

    ...

}</code></pre>
<p>oraz:</p>
<pre class="sourceCode java"><code class="sourceCode java"><span class="kw">package petrinet;

import java.util.Collection;
import java.util.Map;
...

public class Transition<T> {

    public Transition(Map<T, Integer> input, Collection<T> reset, Collection<T> inhibitor, Map<T, Integer> output) {
        ...
    }

    ...

}</code></pre>
<p>Konstruktor <code>Transition&lt;T&gt;(input, reset, inhibitor, output)</code> tworzy przejście między miejscami typu <code>T</code>. Przejście jest połączone:</p>
<ul>
<li><p>krawędziami wejściowymi z miejscami należącymi do <code>input.keySet()</code>, przy czym wagą krawędzi prowadzącej z miejsca <code>x</code> jest <code>input.get(x)</code>,</p></li>
<li><p>krawędziami zerującymi z miejscami należącymi do <code>reset</code>,</p></li>
<li><p>krawędziami wzbraniającymi z miejscami należącymi do <code>inhibitor</code>,</p></li>
<li><p>krawędziami wyjściowymi z miejscami należącymi do <code>output.keySet()</code>, przy czym wagą krawędzi prowadzącej do miejsca <code>x</code> jest <code>output.get(x)</code>.</p></li>
</ul>
<p>Konstruktor <code>PetriNet&lt;T&gt;(initial, fair)</code> tworzy sieć Petriego z miejscami typu <code>T</code>, w której:</p>
<ul>
<li><p>w stanie początkowym miejsce ma niezerową liczbę żetonów wtedy i tylko wtedy, gdy należy do <code>initial.keySet()</code>, przy czym <code>initial.get(x)</code> jest liczbą żetonów w miejscu <code>x</code>,</p></li>
<li><p>kolejność budzenia wątków oczekujących na wykonanie opisanej poniżej metody <code>fire(transitions)</code> jest określona parametrem <code>fair</code>.</p></li>
</ul>
<p>Metoda <code>reachable(transitions)</code> próbuje wyznaczyć zbiór wszystkich znakowań sieci, które są <strong>osiągalne</strong> (ang. <em>reachable</em>) z aktualnego jej stanu w rezultacie odpalenia, zero lub więcej razy, przejść z kolekcji <code>transitions</code>.</p>
<p>Jeśli zbiór osiągalnych znakowań jest skończony, to jest on wynikiem metody. Mapa <code>m</code>, należąca do tego zbioru, reprezentuje znakowanie, w którym miejsce ma niezerową liczbę żetonów wtedy i tylko wtedy, gdy jest elementem <code>m.keySet()</code>, przy czym <code>m.get(x)</code> jest liczbą żetonów w miejscu <code>x</code>.</p>
<p>Jeśli zbiór osiągalnych znakowań jest nieskończony, to wykonanie metody może się zapętlić lub zostać przerwane wyjątkiem.</p>
<p>Można zauważyć, że wywołanie <code>reachable(transitions)</code> z pustą kolekcją przejść daje zbiór znakowań sieci, którego jedynym elementem jest znakowanie aktualne.</p>
<p>Metoda <code>fire(transitions)</code> dostaje jako argument niepustą kolekcję przejść. Wstrzymuje wątek, jeśli żadne przejście z tej kolekcji nie jest dozwolone. Gdy w kolekcji są przejścia dozwolone, metoda odpala jedno z nich, dowolnie wybrane. Wynikiem metody jest odpalone przez nią przejście.</p>
<p>Jeżeli sieć Petriego została utworzona konstruktorem z argumentem <code>fair</code> równym <code>true</code>, to spośród tych wątków wstrzymanych metodą <code>fire(transitions)</code>, które w danej chwili można wznowić, wybierany jest wątek czekający najdłużej.</p>
<p>W przypadku przerwania wątku, metoda <code>fire(transitions)</code> zgłasza wyjątek <code>InterruptedException</code>.</p>
<p>Oprócz klas <code>PetriNet&lt;T&gt;</code> i <code>Transition&lt;T&gt;</code>, w rozwiązaniu mogą być inne definicje. Można je umieścić zarówno w pakiecie <code>petrinet</code>, jak i w pakietach pomocniczych.</p>
<h2 id="specyfikacja-programu-alternator">Specyfikacja programu Alternator</h2>
<p>Program uruchamiamy metodą statyczną <code>main(args)</code> klasy <code>Main</code>, zdefiniowanej w pakiecie <code>alternator</code>.</p>
<p>W programie rozważamy rozwiązanie problemu wzajemnego wykluczania dla trzech procesów, z dodatkowym wymaganiem, by w sekcji krytycznej nie mógł się znaleźć, dwa razy z rzędu, ten sam proces.</p>
<p>Program buduje sieć Petriego, będącą modelem tego systemu. Pisze, ile znakowań jest osiągalnych ze stanu początkowego. Sprawdza, czy wszystkie spełniają warunek bezpieczeństwa.</p>
<p>Następnie program rozpoczyna symulację systemu. Uruchamia trzy wątki <code>A</code>, <code>B</code>, <code>C</code>, działające w nieskończonych pętlach. W sekcji krytycznej każdy wątek pisze, dwoma kolejnymi wywołaniami <code>System.out.print()</code>, swoją nazwę i kropkę.</p>
<p>Dla każdego wątku, protokołem wstępnym i protokołem końcowym są pojedyncze wywołania metody <code>fire(transitions)</code> z odpowiednio dobranymi argumentami.</p>
<p>Jeśli zajdzie taka potrzeba, można uruchomić dodatkowy wątek pomocniczy, odpowiedzialny za odpalanie przejść, których nie odpalają wątki <code>A</code>, <code>B</code>, <code>C</code>.</p>
<p>Po 30 sekundach od rozpoczęcia symulacji wątek główny przerywa wszystkie pozostałe i kończy program.</p>
<h2 id="specyfikacja-programu-multiplicator">Specyfikacja programu Multiplicator</h2>
<p>Program uruchamiamy metodą statyczną <code>main(args)</code> klasy <code>Main</code>, zdefiniowanej w pakiecie <code>multiplicator</code>.</p>
<p>Program buduje sieć Petriego, mnożącą dwie nieujemne liczby całkowite <code>A</code> i <code>B</code>, wczytane z wejścia.</p>
<p>W stanie początkowym sieci jest miejsce, w którym jest <code>A</code> żetonów i miejsce, w którym jest <code>B</code> żetonów.</p>
<p>Iloczyn <code>A * B</code> jest liczony przez wielokrotne odpalanie przejść.</p>
<p>Po zakończeniu obliczenia:</p>
<ul>
<li><p>wyróżnione przejście końcowe staje się dozwolone,</p></li>
<li><p>wszystkie pozostałe przejścia nie są dozwolone,</p></li>
<li><p>w pewnym miejscu sieci jest <code>A * B</code> żetonów.</p></li>
</ul>
<p>Po zbudowaniu sieci, program uruchamia cztery wątki pomocnicze. Każdy, w nieskończonej pętli, wykonuje metodę <code>fire(transitions)</code> z kolekcją przejść, w której są wszystkie przejścia z wyjątkiem końcowego.</p>
<p>Wątek główny czeka na zakończenie obliczeń. W tym celu wywołuje <code>fire(transitions)</code> z jednoelementową kolekcją przejść, w której jest tylko przejście końcowe.</p>
<p>Po zakończeniu obliczeń, wątek główny wypisuje iloczyn <code>A * B</code>, który odczytuje z aktualnego znakowania sieci i przerywa wątki pomocnicze.</p>
<p>Każdy wątek pomocniczy na zakończenie pracy pisze, ile przejść odpalił.</p>
<h2 id="wymagania-techniczne">Wymagania techniczne</h2>
<p>Program ma być w wersji 11 języka Java. Powinien się kompilować kompilatorem <code>javac</code> i działać poprawnie na komputerze <code>students</code>.</p>
<p>Wolno korzystać tylko ze standardowych pakietów zainstalowanych na <code>students</code>.</p>
<p>Jako rozwiązanie należy wysłać na moodla plik <code>ab123456.tar.gz</code>, gdzie <code>ab123456</code> to login na <code>students</code>.</p>
<p>W wysłanym pliku <code>.tar.gz</code> mają być katalogi pakietów z plikami źródłowymi <code>.java</code>.</p>
<h2 id="walidacja-i-testy">Walidacja i testy</h2>
<p>Implementacja sieci Petriego zostanie poddana walidacji, wstępnie sprawdzającej zgodność ze specyfikacją.</p>
<p>Na komputerze <code>students</code>, w katalogu walidacji, będą:</p>
<ul>
<li><p>pliki <code>.tar.gz</code> z rozwiązaniami,</p></li>
<li><p>plik <a href="https://moodle.mimuw.edu.pl/mod/resource/view.php?id=19644">validate.sh</a>,</p></li>
<li><p>plik <a href="https://moodle.mimuw.edu.pl/mod/resource/view.php?id=19645">Validate.java</a>.</p></li>
</ul>
<p>Polecenie:</p>
<pre class="sh"><code>sh <a class="autolink" title="validate.sh" href="https://moodle.mimuw.edu.pl/mod/resource/view.php?id=19644">validate.sh</a> ab123456</code></pre>
<p>przeprowadzi walidację rozwiązania studenta o identyfikatorze <code>ab123456</code>. Komunikat <code>OK</code> poinformuje o sukcesie.</p>
<p>Rozwiązania, które pomyślnie przejdą walidację, zostaną dopuszczone do testów poprawności.</p>
<p>Programy przykładowe, będące częścią rozwiązania, nie będą testowane automatycznie.</p>
