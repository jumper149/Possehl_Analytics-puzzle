Ich dachte, dass auch bei roter Ampel noch Körbchen kommen können (oder aber auch keine mehr).
Deshalb habe ich relativ aufwendig concurrent geprüft, ob die Ampel auf rot schaltet, und solange das nicht passiert wartet die Schleife auf das nächste Körbchen.
Als ich damit fertig war habe ich gesehen, dass im Puzzle steht "Wenn Ampel rot, dann kommen keine Körbchen mehr".

Naja, dafür hat das Puzzle so mehr Spaß gemacht! :D
Und da der Overhead für concurrent threads in Haskell nicht sehr groß ist, ist das Endergebnis auch quasi äquivalent.

Ich lasse das jetzt jedenfalls so, nur um damit anzugeben, dass ich auch concurrency kann. :D

--

Die debug-tests sehen so aus wie erwartet:
```haskell
Right ()
Left (CountMap {unCountMap = fromList [('!',1),('?',3)]})
Left (CountMap {unCountMap = fromList [(' ',2),('S',1),('c',1),('e',2),('h',1),('i',2),('l',2),('m',1),('n',1),('t',1)]})
```

Viele Grüße,
Felix
