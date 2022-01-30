# Kompilator Instant

## Wprowadzenie
Rozwiązanie składa się z dwóch kompilatorów języka Instant: do JVM i LLVM.

## Kompilacja
Wykonanie `make` skutkuje zbudowaniem dwóch programów wykonywalnych `insc_jvm` i `insc_llvm`.

## Biblioteki
Wszystkie potrzebne zależności opisują `package.yaml` oraz `instant-compiler.cabal`, wygenerowany z niego za pomocą `hpack`.
Między innymi, standardowymi pakietami, oba kompilatory wykorzystują pakiety `mtl` i `lens`.

## Struktura plików
Struktura plików zgodna jest z poleceniem zadania.
```
-
 - lib
   - jasmin.jar
 - src
   - grammar
     - Grammar
       - Abs.hs
       - Doc.txt
       - ErrM.hs
       - Lex.x
       - Par.y
       - Print.hs
       - Skel.hs
     - Instant.cf
   - jvm
     - Main.hs
   - llvm
     - Main.hs
 - insant-compiler.cabal
 - Makefile
 - package.yaml
 - README.md
```
