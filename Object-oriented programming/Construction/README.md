<h2 id="wprowadzenie">Wprowadzenie</h2>
<p>Podczas robót budowlanych zachodzi potrzeba rozwiązania problemu optymalizacyjnego dotyczącego przygotowania prętów do konstrukcji stalowych.</p>
<p>Projekt budowy określa liczbę i długości potrzebnych odcinków prętów. W cenniku sprzedawcy są długości prętów i ich ceny. Rozwiązanie wskazuje, ile prętów jakiej długości należy kupić i jak podzielić je na odcinki. Pręt dzielimy na odcinki, tnąc go. Niewykorzystaną część pręta, jeśli taka zostanie, odrzucamy. Łączenie prętów nie jest możliwe.</p>
<p>Za miarę jakości rozwiązania można przyjąć np.:</p>
<ul>
<li><p>całkowity koszt zakupu prętów, lub</p></li>
<li><p>łączną długość odpadów, czyli części kupionych prętów, które nie zostaną wykorzystane.</p></li>
</ul>
<p>Załóżmy, że w cenniku są:</p>
<ul>
<li><p>pręt długości 4000, w cenie 100,</p></li>
<li><p>pręt długości 4500, w cenie 160,</p></li>
<li><p>pręt długości 10000, w cenie 200,</p></li>
</ul>
<p>gdzie długość podana jest w milimetrach a cena w PLN.</p>
<p>Przyjmijmy, że projekt zakłada użycie:</p>
<ul>
<li><p>dwóch odcinków długości 200,</p></li>
<li><p>trzech odcinków długości 350,</p></li>
<li><p>jednego odcinka długości 600,</p></li>
<li><p>dwóch odcinków długości 1500,</p></li>
<li><p>jednego odcinka długości 3000,</p></li>
<li><p>jednego odcinka długości 4500.</p></li>
</ul>
<p>Przykładowym rozwiązaniem problemu jest kupno jednego pręta długości 4500 i trzech prętów długości 4000. Następnie:</p>
<ul>
<li><p>pręt długości 4500 w całości przeznaczamy na odcinek długości 4500,</p></li>
<li><p>pierwszy pręt długości 4000 dzielimy na odcinki długości 3000, 600, 350 i odpad długości 50,</p></li>
<li><p>drugi pręt długości 4000 dzielimy na odcinki długości 1500, 1500, 350, 350, 200 i odpad długości 100,</p></li>
<li><p>trzeci pręt długości 4000 dzielimy na odcinek długości 200 i odpad długości 3800.</p></li>
</ul>
<p>Dla tego rozwiązania:</p>
<ul>
<li><p>całkowity koszt zakupu prętów jest równy 460,</p></li>
<li><p>łączna długość odpadów jest równa 3950.</p></li>
</ul>
<p>Wśród strategii wyboru rozwiązania są:</p>
<ul>
<li><p>strategia <code>minimalistyczna</code></p>
<p>Działa zachłannie. Dopóki problem nie jest rozwiązany, z cennika wybiera najkrótszy pręt, w którym mieści się najdłuższy brakujący odcinek. Następnie rozważa brakujące odcinki w kolejności od najdłuższych. Jeśli rozważany odcinek mieści się w części pręta, która jeszcze pozostała, jest od niej odcinany. To, co zostanie z pręta, po rozważeniu ostatniego odcinka, jest odpadem.</p></li>
<li><p>strategia <code>maksymalistyczna</code></p>
<p>Działa tak, jak strategia minimalistyczna, ale z cennika zawsze wybiera najdłuższy pręt.</p></li>
<li><p>strategia <code>ekonomiczna</code></p>
<p>Znajduje jedno z, być może wielu, rozwiązań minimalizujących koszt zakupu prętów.</p></li>
<li><p>strategia <code>ekologiczna</code></p>
<p>Znajduje jedno z, być może wielu, rozwiązań minimalizujących długość odpadów.</p></li>
</ul>
<h2 id="polecenie">Polecenie</h2>
<p>Napisz program, który:</p>
<ul>
<li><p>czyta ze standardowego wejścia cennik prętów, opis projektu i nazwę strategii,</p></li>
<li><p>za pomocą wskazanej strategii rozwiązuje problem optymalizacyjny,</p></li>
<li><p>pisze na standardowe wyjście rozwiązanie, określając jego jakość, kupione pręty i sposób ich podziału.</p></li>
</ul>
<h2 id="postać-danych">Postać danych</h2>
<p>Dane programu to ciąg wierszy. We wszystkich, z wyjątkiem ostatniego, są liczby całkowite w zapisie dziesiętnym. Między każdą parą liczb sąsiadujących w wierszu jest jedna spacja.</p>
<ul>
<li><p>W pierwszym wierszu danych jest długość cennika <code>C</code>.</p></li>
<li><p>W <code>C</code> kolejnych wierszach są pary dodatnich liczb całkowitych. Pierwsza z tych liczb określa długość pręta a druga to jego cena. Pary są uporządkowane rosnąco po długości pręta.</p></li>
<li><p>Po cenniku jest wiersz z długością projektu <code>P</code>.</p></li>
<li><p>W następnym wierszu jest <code>P</code> dodatnich liczb całkowitych, uporządkowanych niemalejąco. Liczby określają długości odcinków, potrzebnych do realizacji projektu.</p></li>
<li><p>W ostatnim wierszu danych, po projekcie, jest słowo <code>minimalistyczna</code>, <code>maksymalistyczna</code>, <code>ekonomiczna</code> lub <code>ekologiczna</code>, będące nazwą wybranej strategii.</p></li>
</ul>
<h2 id="postać-wyniku">Postać wyniku</h2>
<p>Wynik programu jest ciągiem wierszy z dziesiętnym zapisem liczb całkowitych. Między każdą parą liczb sąsiadujących w wierszu jest jedna spacja.</p>
<ul>
<li><p>W pierwszym wierszu wyniku jest koszt zakupu prętów.</p></li>
<li><p>W drugim wierszu jest łączna długość odpadów.</p></li>
<li><p>Pozostałe wiersze określają sposób podziału kupionych prętów. Dla każdego pręta jest jeden wiersz. Kolejność tych wierszy nie ma znaczenia.</p>
<p>Na początku wiersza określającego podział jest długość pręta. Po niej, w dowolnej kolejności, są długości odcinków, na które pręt został podzielony, z pominięciem ewentualnego pozostałego odpadu. Suma długości odcinków jest więc mniejsza lub równa długości pręta, z którego powstały.</p></li>
</ul>
<h2 id="przykłady">Przykłady</h2>
<ul>
<li><p>Dla danych <a href="https://moodle.mimuw.edu.pl/pluginfile.php?file=%2F41170%2Fmod_assign%2Fintroattachment%2F0%2Fminimalistyczna.in&amp;amp;forcedownload=1">minimalistyczna.in</a> poprawnym wynikiem jest <a href="https://moodle.mimuw.edu.pl/pluginfile.php?file=%2F41170%2Fmod_assign%2Fintroattachment%2F0%2Fminimalistyczna.out&amp;amp;forcedownload=1">minimalistyczna.out</a>.</p></li>
<li><p>Dla danych <a href="https://moodle.mimuw.edu.pl/pluginfile.php?file=%2F41170%2Fmod_assign%2Fintroattachment%2F0%2Fmaksymalistyczna.in&amp;amp;forcedownload=1">maksymalistyczna.in</a> poprawnym wynikiem jest <a href="https://moodle.mimuw.edu.pl/pluginfile.php?file=%2F41170%2Fmod_assign%2Fintroattachment%2F0%2Fmaksymalistyczna.out&amp;amp;forcedownload=1">maksymalistyczna.out</a>.</p></li>
<li><p>Dla danych <a href="https://moodle.mimuw.edu.pl/pluginfile.php?file=%2F41170%2Fmod_assign%2Fintroattachment%2F0%2Fekonomiczna.in&amp;amp;forcedownload=1">ekonomiczna.in</a> poprawnym wynikiem jest <a href="https://moodle.mimuw.edu.pl/pluginfile.php?file=%2F41170%2Fmod_assign%2Fintroattachment%2F0%2Fekonomiczna.out&amp;amp;forcedownload=1">ekonomiczna.out</a>.</p></li>
<li><p>Dla danych <a href="https://moodle.mimuw.edu.pl/pluginfile.php?file=%2F41170%2Fmod_assign%2Fintroattachment%2F0%2Fekologiczna.in&amp;amp;forcedownload=1">ekologiczna.in</a> poprawnym wynikiem jest <a href="https://moodle.mimuw.edu.pl/pluginfile.php?file=%2F41170%2Fmod_assign%2Fintroattachment%2F0%2Fekologiczna.out&amp;amp;forcedownload=1">ekologiczna.out</a>.</p></li>
</ul>
<h2 id="uwagi">Uwagi</h2>
<ul>
<li><p>Wolno założyć, że dane są poprawne.</p></li>
<li><p>Wolno założyć, że wszystkie dane liczby mieszczą się w zakresie typu <code>int</code>.</p></li>
<li><p>Wolno założyć, że długości odcinków w projekcie nie przekraczają maksymalnej długości pręta w cenniku.</p></li>
<li><p>Implementacja strategii ekonomicznej i ekologicznej wymaga znalezienia optymalnego rozwiązania <a href="https://en.wikipedia.org/wiki/Knapsack_problem">problemu plecakowego</a>. Można to zrobić algorytmem o koszcie wykładniczym.</p></li>
<li><p>Jako rozwiązanie należy wysłać archiwum <code>.tar.gz</code>, <code>.zip</code> lub <code>.jar</code>, zawierające kod źródłowy programu.</p></li>
<li><p>Rozwiązanie powinno się kompilować na <code>students</code> za pomocą kompilatora Javy 8 <code>/usr/bin/javac</code> lub Javy 11 <code>/opt/jdk-11.0.2/bin/javac</code>.</p></li>
