<h2 align=center>Zadanie 3: rozjaśnianie i przyciemnianie</h2>

<p>Dany jest obrazek w postaci matrycy pikseli.  Kolor każdego piksela
zapisany jest na 24 bitach, po 8 bitów dla każdego koloru (RGB).  Napiszemy
procedurę w asemblerze dla procesora ARM, pozwalającą rozjaśniać (czyli
zwiększać) lub przyciemniać (czyli zmniejszać) poszczególne składowe.

<p>Argumentem procedury powinny być:
<ul>
<li>adres matrycy: zapis zapewne wierszami, trzeba będzie wybrać sobie
format odpowiedni do konwersji z formatu graficznego;
<li>rozmiary matrycy: liczba wierszy i kolumn;
<li>wybrana składowa, którą chcemy zmienić: R=1, G=2, B=3;
<li>liczba do dodania do składowej: bajtowa liczba całkowita (ze znakiem!).
</ul>

<p>Należy uwzględnić <em>nasycenie</em>: po otrzymaniu przepełnienia składowa
powinna przyjąć największą (dla dodatnich) lub najmniejszą (dla ujemnych)
możliwą wartość.  Wynik działania uzyskujemy jako efekt uboczny na podanej
matrycy.

<p>Dodatkowo należy napisać program główny w C o 3 argumentach:
<ul>
<li>nazwa pliku graficznego, plik ma być w formacie PPM (kto nie zna niech 
    spyta <a href="http://en.wikipedia.org/wiki/Netpbm_format">Ciocię Wikipedię</a>).
    Program ma wczytać ten plik i zamienić go na matrycę (można funkcją
    z jakiejś biblioteki, byle poprawnej ;-), po czym wywołać naszą procedurę.
<li>specyfikacja składowej: jedna z liter R, G lub B
<li>liczba do dodania do składowej, dodatnia lub ujemna, ale z przedziału
    [-127,127].
</ul>
Zmienioną matrycę należy z powrotem zamienić na plik graficzny, o nazwie 
z dodaną literą 'Y' na początku.

<p>Postać matrycy M nie jest dokładnie określona, powinno być jednak
możliwe jej łatwe zainicjowanie w programie w C przez wczytanie początkowej
zawartości z pliku.

<p>W katalogu <a href="../image">image</a> znajduje się kilka prostych plików
z danymi.
 
<p>Rozwiązania (program i przykładowe testy) należy wysłać do dnia   
29 stycznia (23:59) pocztą emaliowaną na <tt>zbyszek@mimuw.edu.pl</tt> jako
<font color=red>pojedynczy załącznik</font> -- archiwum o nazwie wskazującej 
na autora (np. <code>ab123456-zad3.tgz</code>), spakowane z osobnego katalogu 
o tej samej nazwie (ale bez tgz).  Program ma działać w środowisku 
emulatora ARM zainstalowanym w laboratoriach.  Rozwiązania nie 
zawierające pliku <code>Makefile</code> nie będą sprawdzane.
