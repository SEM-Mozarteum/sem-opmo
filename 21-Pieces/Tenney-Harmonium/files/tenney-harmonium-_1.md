# Tenney – Harmonium #1

```text
Opusmodus Tutorial
Studio for Electronic Music (SEM)
Universität Mozarteum Salzburg
(c) Achim Bornhoeft
```

James Tenneys **Harmonium #1** (9/76, gewidmet Lou Harrison) ist ein Stück
spektraler Musik für ein Ensemble von zwölf oder mehr aushaltefähigen
Instrumenten. Das gesamte Tonmaterial entsteht aus der **Obertonreihe** über
wechselnden Grundtönen. Dieses Tutorial rekonstruiert eine konkrete *Lesart*
der Partitur in Opusmodus: aus den Grundtönen und ihren Obertönen werden
Frequenzen berechnet, in Notation überführt und zu sechs Stimmen montiert.


## Die Aufnahme

![Harmonium #1 – Standbild aus der Partiturmitschau](images/harmonium-video.png){center, width=720}

▶ [Aufnahme abspielen (MP4, ca. 10:35)](media/Tenney-Harmonium-1-1976.mp4)

Im Video läuft die Partitur mit. Gut zu erkennen sind die `<mp>`-Schweller und
die handschriftlichen Cent-Zahlen (`-14`, `-31`, `+2`, `-49`) über den Noten –
genau jene Angaben, die unten programmatisch wieder eingesetzt werden.


## Über das Stück

Die Partitur besteht aus **sieben Abschnitten** (Doppelstriche), jeweils in zwei
bis fünf **Segmente** unterteilt (einfache Striche). Jedes Segment notiert die
gerade *verfügbaren Tonhöhen*; die Zahlen über den Noten geben die Abweichung
vom temperierten Ton in **Cent** an.

Die Spielanweisung (sinngemäß): Jede:r Spieler:in wählt nacheinander einen der
verfügbaren Töne und spielt ihn als Schweller `ppp < X > ppp`, wobei `X` die
notierte Dynamik ist. Jeder Ton dauert vier bis zwölf Sekunden, gleichmäßig auf
Crescendo und Decrescendo verteilt; nach einer Pause folgt der nächste – wahlweise
derselbe oder ein anderer verfügbarer Ton. Der Übergang in ein neues Segment kann
von jeder Stimme eingeleitet werden, indem der neu hinzukommende Ton eingeführt
wird. Die Übergänge werden so terminiert, dass jeder Abschnitt etwa ein bis drei
Minuten dauert.

Die Realisation unten fixiert diese aleatorischen Freiheiten zu einer von vielen
möglichen Fassungen: feste Einsätze, feste Dauern, sechs Stimmen, Tempo 20.\
Bei Tempo 20 dauert eine ganze Note ≈ 12 Sekunden – genau das obere Ende von
Tenneys Spanne „vier bis zwölf Sekunden“.


## Die Obertonreihe als Material

![Obertonreihe über einem Grundton mit Cent-Abweichungen; die Primzahl-Obertöne 3, 5, 7, 11, 17 sind hervorgehoben](images/harmonic-scale.png){center, width=720}

Die Obertöne liegen nicht im temperierten Raster. Ihre Abweichung vom nächsten
temperierten Ton ist für jeden Oberton konstant – unabhängig vom Grundton:

| Oberton | Intervall über dem Grundton | Abweichung |
| --- | --- | --- |
| 2 | Oktave | 0 |
| 3 | Quinte | +2c |
| 5 | große Terz | −14c |
| 7 | kleine Septime | −31c |
| 11 | Tritonus | −49c |
| 13 | (Sexte) | +41c |
| 16 | Oktave | 0 |
| 17 | kleine Sekunde | +5c |

Oktavverwandte Obertöne erben die Abweichung ihres ungeraden Kerns: 4 und 16
verhalten sich wie 1, die 6, 12 und 24 wie 3 (+2c), die 10 wie 5 (−14c), die 14
wie 7 (−31c).

Tenneys strukturelles Material sind vor allem die **Primzahl-Obertöne** 3, 5, 7,
11 und 17 (in der Grafik gelb). Wir berechnen die Reihe zunächst „von Hand“ –
Grundtonfrequenz mal Obertonnummer:

```lisp
;; Frequenz von c2 und seine ersten acht Obertöne (in Hertz)
(let ((f0 (car (pitch-to-hertz '(c2)))))
  (loop for k from 1 to 8
        collect (* k f0)))
```


## 1 – Die Grundtöne

Die Grundtöne wandern in fallenden Quinten (a3 → d3 → g2 → c2 → f1) und steigen
dann über einen übermäßigen Quartsprung (f1 → b1) und eine reine Quarte
(b1 → e2) wieder auf:

```lisp
(setf fundamentals '(a3 d3 g2 c2 f1 b1 e2))
```


## 2 – Obertöne je Grundton

Für jeden Grundton wird festgelegt, welche Obertöne im Verlauf erklingen. Die
Auswahl wächst von drei Tönen über a3 bis zu sieben Tönen über c2 und reicht
über f1 bis zum 24. Oberton:

```lisp
(setf overtones
      '((2 3 5)                       ; Obertöne a3
        (2 3 5 7)                     ; Obertöne d3
        (2 3 5 7 11)                  ; Obertöne g2
        (2 3 5 7 11 16 17)            ; Obertöne c2
        (4 7 10 17 24)                ; Obertöne f1
        (4 6 7 10 12 14 17)           ; Obertöne b1
        (4 7 10)))                    ; Obertöne e2
```

Die in der Partitur farbig markierten Bereiche entsprechen genau diesen
Grundton-/Obertongruppen. Die roten, grünen und gelben Felder zeigen die ersten
drei Grundtöne (a3, d3, g2):

![Partitur, Abschnitte 1–2: a3 (rot), d3 (grün), g2 (gelb). Beschriftung „Grundton – Obertonnummer“](images/partitur-1.png){center, width=720}

Blau und orange folgen c2 und f1:

![Partitur, Abschnitte 3–4: c2 (blau, bis Oberton 17) und f1 (orange, bis Oberton 24)](images/partitur-2.png){center, width=720}

Den Schluss bilden b1 (cyan) und e2 (magenta):

![Partitur, Abschnitte 5–7: f1 (orange), b1 (cyan) und e2 (magenta), schließend im ppp](images/partitur-3.png){center, width=720}


## 3 – Frequenzen berechnen

Jeder Grundton wird in Hertz umgerechnet und mit seinen Obertonnummern
multipliziert. Das Ergebnis ist eine Liste von Frequenzlisten – eine pro
Grundton:

```lisp
(setf pitches-hz
      (loop for i in (pitch-to-hertz fundamentals)
            for j in overtones
            collect
              (loop for k in j
                    collect (* k i))))
```

Die Obertöne über c2 (Index 3) als Frequenzkurve:

```plot1
(nth 3 pitches-hz)
```


## 4 – Von Hertz zur Notation

`hertz-to-pitch` quantisiert die Frequenzen auf das nächste Tonhöhenraster. Mit
`:quantize 1/2` werden die Notenköpfe auf Vierteltöne gerundet (gut lesbar); für
genauere Wiedergabe kann auf `1/8` quantisiert werden:

```lisp
(setf pitches (hertz-to-pitch pitches-hz :quantize 1/2))
```

Damit die langen Schwelltöne ungebrochen klingen, erhält jede Note ein
`tie`-Attribut – eine Bindebogen-Liste in der Struktur der Tonhöhen:

```lisp
(setf ties
      (loop for i in pitches
              collect (loop repeat (length i)
                              collect 'tie)))
```

Aus Tonhöhen, ganzen Noten und Bindebögen entsteht das OMN-Material. Mit
`single-events` wird jede Note zu einem einzeln adressierbaren Ereignis – so
lässt sich später ein bestimmter Oberton eines bestimmten Grundtons gezielt
herausgreifen:

```lisp
(setf mat
      (single-events
       (make-omn
        :pitch pitches
        :length '(w)
        :attribute ties
        :span :pitch)))
```

`mat` ist eine Liste aus sieben Stimmen (eine pro Grundton). Der Zugriff erfolgt
zweistufig: `(nth Grundton-Index (nth … ))` – z. B. liefert `(nth 2 (nth 0 mat))`
das Ereignis an Position 2 über Grundton 0 (a3), also den **5. Oberton**.

Das vollständige Material – alle sieben Obertonreihen übereinander – als
mehrstimmige Notation:

```omn2
mat
```

Schließlich noch ein leerer 4/4-Takt als Pausenbaustein für die Stimmeneinsätze:

```lisp
(setf pause (make-omn :length '(-w)))
```


## 5 – Die sechs Stimmen

Jede Stimme wird in zwei Schritten gebaut:

1. Mit `append` und `gen-loop` werden die Bausteine aus `mat` (und `pause`) zur
   gewünschten Taktzahl aneinandergereiht – das ist der zeitliche Ablauf.
2. `dictum` legt anschließend Ausdruck und Feinheiten an exakt definierte Takte:
   die Schweller-Dynamiken (`<mp>`, `<mf>`, `<f>` als Ziel „X“), die Artikulation
   `marc`, das Entfernen der MIDI-Velocity (damit die Hairpins die Lautstärke
   führen) sowie das Entfernen einzelner Bindebögen. Vor allem trägt `dictum` die
   **Cent-Angaben** (`2c`, `-14c`, `-31c`, `-49c`, `5c`) an genau den Takten ein,
   an denen ein neuer Oberton einsetzt – analog zu den handschriftlichen
   Cent-Zahlen in Tenneys Partitur.

### Erste Stimme

```lisp
(setf voc1
      (dictum '((:remove :velocity)
                (:do <mp> :bar 3)
                (:do -14c :bar 3)
                (:do -31c :bar 4)
                (:do -49c :bar 9)
                (:remove tie :bar 12)
                (:do 5c :bar 13)
                (:do 2c :bar 17)
                (:do 5c :bar 21)
                (:do -31c :bar 24)
                (:do -14c :bar 27))
              (append
               (gen-loop 2 pause)
               (gen-loop 1 (nth 2 (nth 0 mat)))
               (gen-loop 5 (nth 3 (nth 1 mat)))
               (gen-loop 4 (nth 4 (nth 2 mat)))
               (gen-loop 3 (nth 6 (nth 3 mat)))
               (gen-loop 1 (nth 5 (nth 3 mat)))
               (gen-loop 4 (nth 4 (nth 4 mat)))
               (gen-loop 3 (nth 6 (nth 5 mat)))
               (gen-loop 3 (nth 5 (nth 5 mat)))
               (gen-loop 1 (nth 2 (nth 6 mat))))))
```

Der Ablauf dieser Stimme deckt sich Takt für Takt mit der obersten Zeile der
Partitur:

| Takte | Grundton | Oberton | Beschriftung / Notiz |
| --- | --- | --- | --- |
| 1–2 | – | – | Pause |
| 3 | a3 | 5 (−14c) | A3‑5, Einsatz `<mp>` |
| 4–8 | d3 | 7 (−31c) | D3‑7 |
| 9–12 | g2 | 11 (−49c) | G2‑11; Bindebogen ab Takt 12 gelöst |
| 13–15 | c2 | 17 (+5c) | C2‑17 |
| 16 | c2 | 16 | C2‑16 |
| 17–20 | f1 | 24 (+2c) | F1‑24 |
| 21–23 | b1 | 17 (+5c) | B1‑17 |
| 24–26 | b1 | 14 (−31c) | B1‑14 |
| 27 | e2 | 10 (−14c) | E2‑10 |

Die fertige Stimme als Notation:

```omn
voc1
```

…und als Tonhöhenverlauf (gut sichtbar: die fallend-steigende Grundtonbewegung):

```plot4
voc1
```

### Zweite bis sechste Stimme

Die übrigen Stimmen folgen demselben Prinzip mit eigenen Einsätzen, Dauern und
Cent-Markierungen. Gemeinsam ergeben sie die Schichtung der farbigen Felder in
der Partitur.

```lisp
(setf voc2
      (dictum '((:remove :velocity)
                (:do <mp> :bar 2)
                (:do 2c :bar 2)
                (:do -14c :bar 5)
                (:do -31c :bar 7)
                (:do -49c :bar 14)
                (:remove tie :bar 17)
                (:do 5c :bar 18)
                (:do 2c :bar 21)
                (:do -14c :bar 22)
                (:do -31c :bar 26))
              (append
               (gen-loop 1 pause)
               (gen-loop 3 (nth 1 (nth 0 mat)))
               (gen-loop 2 (nth 2 (nth 1 mat)))
               (gen-loop 7 (nth 3 (nth 2 mat)))
               (gen-loop 4 (nth 4 (nth 3 mat)))
               (gen-loop 3 (nth 3 (nth 4 mat)))
               (gen-loop 1 (nth 4 (nth 5 mat)))
               (gen-loop 4 (nth 3 (nth 5 mat)))
               (gen-loop 2 (nth 1 (nth 6 mat))))))
```

```lisp
(setf voc3
      (dictum '((:remove :velocity)
                (:do <mp> :bar 1)
                (:do marc :bar 1)
                (:do 2c :bar 6)
                (:do -14c :bar 8)
                (:do -31c :bar 11)
                (:do -14c :bar 20)
                (:do -31c :bar 21)
                (:do 2c :bar 23)
                (:do <mp> :bar 25) ; redundant
                (:do marc :bar 25))
              (append
               (gen-loop 5 (nth 0 (nth 0 mat)))
               (gen-loop 2 (nth 1 (nth 1 mat)))
               (gen-loop 3 (nth 2 (nth 2 mat)))
               (gen-loop 9 (nth 3 (nth 3 mat)))
               (gen-loop 1 (nth 2 (nth 4 mat)))
               (gen-loop 2 (nth 2 (nth 5 mat)))
               (gen-loop 2 (nth 1 (nth 5 mat)))
               (gen-loop 3 (nth 0 (nth 6 mat))))))
```

```lisp
(setf voc4
      (dictum '((:remove :velocity)
                (:do <mp> :bar 6)
                (:do marc :bar 6)
                (:do 2c :bar 10)
                (:do -14c :bar 12)
                (:do -31c :bar 19)
                (:do <mf> :bar 21)
                (:do marc :bar 21))
              (append
               (gen-loop 5 pause)
               (gen-loop 4 (nth 0 (nth 1 mat)))
               (gen-loop 2 (nth 1 (nth 2 mat)))
               (gen-loop 7 (nth 2 (nth 3 mat)))
               (gen-loop 2 (nth 1 (nth 4 mat)))
               (gen-loop 4 (nth 0 (nth 5 mat)))
               (gen-loop 3 pause))))
```

```lisp
(setf voc5
      (dictum '((:remove :velocity)
                (:do <mf> :bar 10)
                (:do marc :bar 10)
                (:do <f> :bar 17)
                (:do marc :bar 17)
                (:do 2c :bar 15))
              (append
               (gen-loop 9 pause)
               (gen-loop 5 (nth 0 (nth 2 mat)))
               (gen-loop 2 (nth 1 (nth 3 mat)))
               (gen-loop 4 (nth 0 (nth 4 mat)))
               (gen-loop 7 pause))))
```

```lisp
(setf voc6
      (dictum '((:remove :velocity)
                (:do <f> :bar 15)
                (:do marc :bar 15))
              (append
               (gen-loop 14 pause)
               (gen-loop 2 (nth 0 (nth 3 mat)))
               (gen-loop 11 pause))))
```

Zwei tiefe Stimmen im Vergleich (voc5 und voc6) als mehrstimmige Notation:

```omn2
(list voc5 voc6)
```


## 6 – Die Partitur

`def-score` fügt die sechs Stimmen zu einer Partitur mit flexiblen Schlüsseln und
chromatischer Vorzeichnung zusammen. Die `controllers (91 …)`-Einträge setzen
Hall; Panorama und Programme verteilen die Stimmen im Klangbild:

```lisp
(def-score harmonium
    (:title "Harmonium #1"
     :composer "James Tenney"
     :copyright "Copyright © 1976"
     :key-signature 'chromatic
     :time-signature '(4 4)
     :flexible-clef t
     :tempo 20
     :layout (bracket-group
              (flute-layout 'flute)
              (violin-layout 'violin)
              (clarinet-layout 'clarinet)
              (viola-layout 'viola)
              (bassoon-layout 'bassoon)
              (violoncello-layout 'violoncello)))

  (flute
   :omn voc1
   :channel 1
   :sound 'gm
   :program 'violin
   :volume 100
   :pan 44
   :controllers (91 '(48)))

  (violin
   :omn voc2
   :channel 2
   :sound 'gm
   :program 'violin
   :volume 100
   :pan 54
   :controllers (91 '(48)))

  (clarinet
   :omn voc3
   :channel 3
   :sound 'gm
   :program 'viola
   :volume 90
   :pan 74
   :controllers (91 '(60)))

  (viola
   :omn voc4
   :channel 4
   :sound 'gm
   :program 'viola
   :volume 90
   :pan 74
   :controllers (91 '(60)))

  (bassoon
   :omn voc5
   :channel 5
   :sound 'gm
   :program 'cello
   :volume 90
   :pan 84
   :controllers (91 '(60)))

  (violoncello
   :omn voc6
   :channel 6
   :sound 'gm
   :program 'cello
   :volume 90
   :pan 84
   :controllers (91 '(60))))
```

Partitur anzeigen:

```lisp
(display-musicxml 'harmonium)
```


## Praktische Hinweise

- Die Code-Blöcke bauen aufeinander auf. Am bequemsten von oben nach unten
  auswerten – **Tools > Markdown Developer > Evaluate Markdown Code Blocks**
  (Cmd-Opt-Shift-E) – oder Block für Block über **Evaluate**.
- `:quantize 1/2` priorisiert lesbare Notenköpfe; für genauere Wiedergabe in
  `(setf pitches (hertz-to-pitch pitches-hz :quantize 1/8))` umstellen.
- Die `dictum`-Listen sind der natürliche Ort für Varianten: andere Einsätze,
  Dauern oder Dynamiken erzeugen eine neue Lesart desselben Stücks.
- Tempo 20 ⇒ ganze Note ≈ 12 s. Wer kürzere Schweller will, erhöht das Tempo
  oder unterteilt die ganzen Noten.

Quelle des Codes: `tenney-harmonium-_1.opmo`.
