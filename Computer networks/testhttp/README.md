<h1 id="zadanie-1">Zadanie 1</h1>
<p>Zadanie polega na napisaniu programu binarnego <code>testhttp_raw</code> zawierającego oprogramowanie klienta oraz skryptu <code>testhttp</code>.
 Zadaniem tych programów jest zapewnienie narzędzia do prostego 
testowania stron WWW. Używać będziemy protokołu warstwy aplikacji HTTP w
 wersji 1.1, protokołu warstwy transportu TCP i warstwy sieci IPv4.</p>
<h2 id="program-klienta">1. Program klienta</h2>
<p>Klient po zinterpretowaniu argumentów wiersza poleceń łączy się ze 
wskazanym adresem i portem, wysyła do serwera HTTP żądanie podania 
wskazanej strony, odbiera odpowiedź od serwera, analizuje wynik i podaje
 raport.</p>
<h3 id="opis-komunikacji">Opis komunikacji</h3>
<p>Techniczny opis formatu żądań i odpowiedzi HTTP znajduje się w dokumencie <a href="https://tools.ietf.org/html/rfc7230">RFC7230</a>.
 Klient ma połączyć się ze wskazanym adresem i portem, a następnie 
wysłać odpowiednie żądanie HTTP. Adres i port połączenia nie muszą się 
zgadzać z adresem i portem wskazanym w adresie testowanej strony. 
Żądanie HTTP ma zawierać odpowiednio umieszczone w swojej treści:</p>
<ul>
<li>adres serwera, z którego będziemy ściągać zasób;</li>
<li>wskazanie zasobu, który ma zostać pobrany;</li>
<li>określone przez parametry w wierszu poleceń ciasteczka;</li>
<li>wskazanie, że po zakończeniu przesyłania zasobu połączenie ma zostać przerwane.</li>
</ul>
<p>Jeśli odpowiedź serwera jest inna niż <code>200 OK</code> (np. <code>202 Accepted</code>) klient ma podać raport w postaci zawartości wiersza statusu uzyskanej odpowiedzi. Jeśli odpowiedź serwera jest <code>200 OK</code>,
 raport ma składać się z dwóch części: zestawienia ciasteczek oraz 
rzeczywistej długości przesyłanego zasobu. Techniczny opis formatu pola 
nagłówkowego <code>Set-Cookie</code> znajduje się w dokumencie <a href="https://tools.ietf.org/html/rfc2109">RFC2109</a>. Należy tutaj pamiętać, że jedna odpowiedź HTTP może zawierać wiele pól <code>Set-Cookie</code>. Należy przyjąć, że jedno pole nagłówkowe <code>Set-Cookie</code>
 ustawia jedno ciasteczko. Jeśli w jednym polu ustawiane jest wiele 
ciasteczek należy ciasteczka poza pierwszym pominąć. Jeśli implementacja
 przyjmuje ograniczenia na liczbę przyjmowanych ciasteczek i ich 
długość, to ograniczenia te powinny zostać dobrane zgodnie z założeniami
 przyjętymi w standardach HTTP dla rozwiązań ogólnego przeznaczenia. 
Dodatkowo przy liczeniu długości przesyłanego zasobu należy uwzględnić 
możliwość, że zasób był wysłany w częściach (kodowanie przesyłowe <em>chunked</em>).</p>
<h3 id="opis-wypisywanego-raportu">Opis wypisywanego raportu</h3>
<p>Jak wspomnieliśmy, raport ma składać się z dwóch następujących 
bezpośrednio jedna po drugiej części: zestawienia ciasteczek oraz 
rzeczywistej długości przesyłanego zasobu. Zestawienie ciasteczek ma 
składać się z liczby linii równej liczbie ciasteczek. Każde ciasteczko 
ma być wypisane w osobnym wierszu w formacie <em>klucz=wartość</em> (bez spacji naokoło znaku <em>=</em>). Rzeczywista długość przesyłanego zasobu ma składać się z jednego wiersza postaci <em>Dlugosc zasobu: <długość></długość></em>, gdzie <em><długość></długość></em> to zapisana w systemie dziesiętnym długość zasobu. Po znaku <em>:</em> ma znajdować się jedna spacja.</p>
<h3 id="opis-wiersza-poleceń">Opis wiersza poleceń</h3>
<p>Wywołanie programu klienta ma ogólną postać:</p>
<p><code>testhttp_raw &lt;adres połączenia&gt;:&lt;port&gt; &lt;plik ciasteczek&gt; &lt;testowany adres http&gt;</code></p>
<p>gdzie</p>
<ul>
<li><code>&lt;adres połączenia&gt;</code> to adres, z którym klient ma się połączyć;</li>
<li><code>&lt;port&gt;</code> to numer portu, z którym klient ma się podłączyć;</li>
<li><code>&lt;plik ciasteczek&gt;</code> to plik zawierający ciasteczka wysyłane do serwera HTTP, format opisany poniżej;</li>
<li><code>&lt;testowany adres http&gt;</code> to adres strony, która ma być zaserwowana przez serwer HTTP.</li>
</ul>
<p>W pliku zawierającym ciasteczka każde ciasteczko jest zawarte w osobnym wierszu w formacie <em>klucz=wartość</em> (bez spacji naokoło znaku <em>=</em>).</p>
<p>Przykładowe wywołania:</p>
<p><code>./testhttp_raw www.mimuw.edu.pl:80 ciasteczka.txt http://www.mimuw.edu.pl/</code></p>
<p><code>./testhttp_raw 127.0.0.1:30303 ciasteczka.txt https://www.mimuw.edu.pl/</code></p>
<p>Należy przyjąć, że <code>&lt;testowany adres http&gt;</code> po prefiksie <code>http://</code> lub <code>https://</code> zawiera uproszczony format pierwszego pola w postaci <code>&lt;adres serwera&gt;[:&lt;numer portu&gt;]</code>, przy czym ujętą w nawiasy kwadratowe część <code>:&lt;numer portu&gt;</code> można pominąć, przykładowe adresy to <code>http://www.mimuw.edu.pl/plik</code>, <code>http://www.mimuw.edu.pl:8080/plik</code>, <code>https://www.mimuw.edu.pl/plik</code>.</p>
<h2 id="skrypt">2. Skrypt</h2>
<p>Wywołanie skryptu ma mieć ogólną postać:</p>
<p><code>./testhttp &lt;plik ciasteczek&gt; &lt;testowany adres http&gt;</code></p>
<p>Zadaniem skryptu jest zapewnienie programowi <code>testhttp_raw</code> możliwości testowania stron HTTPS. Skrypt ten ma rozpoznawać, czy <code>&lt;testowany adres http&gt;</code> jest adresem czystego HTTP, czy HTTPS. W tym pierwszym przypadku ma przekształcać wywołanie skryptu na wywołanie programu <code>testhttp_raw</code>. W tym drugim przypadku ma za pomocą programu <code>stunnel</code> stworzyć tunel, z miejscem wejściowym na adresie pętli zwrotnej i wybranym porcie, a następnie wywołać program <code>testhttp_raw</code> tak, aby klient połączył się z tym adresem i portem oraz testował żądanie na adres <code>&lt;testowany adres http&gt;</code>.</p>
<p>Program <code>stunnel</code> działa w ten sposób, że na podstawie 
danych konfiguracyjnych (mogą być podane w pliku lub przez standardowe 
wejście) łączy się on z odległym serwerem <em>SO</em> pod wskazanym 
adresem i portem, tworząc połączenie, poprzez które przesyłane są 
protokołem SSL/TLS zaszyfrowane dane. Połączenie to nazywane jest <em>tunelem</em>. Dane do przesyłania program <code>stunnel</code>
 uzyskuje w sposób następujący. Tworzy on na wskazanym adresie lokalnym i
 porcie serwer. Jeśli jakiś klient połączy się z tym lokalnym adresem i 
portem, to dane przekazywane tym połączeniem będą trafiały w postaci 
zaszyfrowanej do serwera <em>SO</em>. Na przykład dla pliku konfiguracyjnego o zawartości</p>
<pre><code>[service]
client = yes
accept = 127.0.0.1:3333
connect = www.mimuw.edu.pl:443</code></pre>
<p>połączenie na adres <code>127.0.0.1</code> z portem <code>3333</code> spowoduje, że dane tam przesyłane trafią połączeniem SSL/TLS do serwera pod adresem <code>www.mimuw.edu.pl</code> na porcie <code>443</code>.</p>
<p>Chcielibyśmy jeszcze zwrócić uwagę Państwa na to, że w niektórych konfiguracjach program <code>stunnel</code> zapisuje do wskazanego pliku swój <em>pid</em>. Sposobem tworzenia tego pliku można sterować poprzez zawartość pliku konfiguracyjnego.</p>
<h2 id="dodatkowe-wymagania">3. Dodatkowe wymagania</h2>
<p>Katalog z rozwiązaniem powinien zawierać plik źródłowy <code>testhttp_raw.c</code> oraz plik <code>Makefile</code>
 zapewniający automatyczną kompilację i linkowanie. Można też umieścić 
tam inne pliki potrzebne do skompilowania i uruchomienia programu, jeśli
 to jest konieczne. Dodatkowo w katalogu powinien znajdować się 
wykonywalny skrypt <code>testhttp</code>.</p>
<p>Jeśli skrypt <code>testhttp</code> tworzy pliki tymczasowe lub inne 
obiekty w systemie operacyjnym, muszą one być kasowane przed 
zakończeniem działania skryptu, także w wyniku przerwania sygnałem.</p>
<p>Nie wolno używać dodatkowych bibliotek, czyli innych niż standardowa biblioteka C.</p>
<p>Każdy komunikat błędu (np. o niemożliwości połączenia się ze 
wskazanym serwerem HTTP) musi być wypisywany na standardowy strumień 
błędów i zaczynać się od prefiksu <code>ERROR:</code>.</p>
