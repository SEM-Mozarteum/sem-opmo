# Perception of Chance

*Copyright © 2026, Achim Bornhoeft*

---

Music can be understood, in the broadest sense, as modulated time — distinct from linear, chronometric time. While the latter generates predictable events, a great deal of music consists of varied temporal shapes.

This lecture explores the relationships between stochastic musical processes and their perceptual qualities.

---

## 1. Chromatic Scale (MIDI)

Linearity = determination. A chromatic scale as a MIDI integer sequence forms the deterministic starting point.

```omn
(setf chrom-scale (gen-integer 40 100))

(cons 's (midi-to-pitch chrom-scale))
```

---

## 2. Small Deviations (±2)

Low dissolution of direction — gradual ambiguity around a determinate centre.

```omn
(setf chr-dev2
      (gen-loop 5 (midi-to-pitch
                   (gen-deviation
                    chrom-scale
                    -2 2))
                :seed 123))

(flatten (cons 's chr-dev2))
```

---

## 3. Larger Deviations (±5)

Stronger dissolution of direction — increase of randomness inversely proportional to gestalt formation.

```omn
(setf chr-dev5
      (gen-loop 5 (midi-to-pitch
                   (gen-deviation
                    chrom-scale
                    -5 5))
                :seed 123))

(flatten (cons 's chr-dev5))
```

---

## 4. Reduced Tempo

At reduced tempo the tendency dissolves — pitch shapes tend towards momentary form.

```omn
(flatten (cons 'e chr-dev5))
```

---

## 5. Quarter Tempo with High Deviations (±15)

At quarter tempo and with high deviations, direction is almost no longer perceptible.

```omn
(setf chr-dev15
      (gen-loop 5
                (midi-to-pitch
                 (gen-deviation
                  (gen-integer 40 100)
                  -15 15)) :seed 123))

(flatten (cons 'q chr-dev15))
```

---

## 6a. High Speed (4× Faster)

Same pitches, but four times the tempo — the tendency becomes audible again. 
Speed compensates for the effects of chance.

```omn
(flatten (cons 't chr-dev15))
```

## 6b. 5 similar tendencies

The more voices having the same direction, the tendency is
increasingly perceptible. Listen to 5 different tendencies (voices)
```omn2
(loop for i in chr-dev15
      collect (cons 's i))
```

---

## 7. Rhythmic Deviations

Gestalt (direction) is additionally dissolved by the unpredictability of the rhythmic structure.

```omn
(setf length-dev1
      (vector-map '(1/16 1/8 1/4)
                  (rndn (length (flatten chr-dev15)) 
                        50 400 :seed 123)))

(make-omn
 :pitch (flatten chr-dev15)
 :length length-dev1
 :velocity '(p))
```

---

## 8. Notation with High Rhythmic Deviations

Larger rhythmic deviations increasingly dissolve the direction tendency.

```omn
(setf length-dev2
      (vector-map '(1/32 1/16 1/8 1/4 3/8 1/2)
                  (rndn (length (flatten chr-dev15)) 
                        50 400 :seed 123)))

(ps 'gm
    :treble (list (make-omn
                   :pitch (flatten chr-dev15)
                   :length length-dev2
                   :velocity '(p)))
    :time-signature '(4 4) 
    :tempo 60)
```

Higher tempo lets the direction become perceptible once again:

```omn
(ps 'gm
    :treble (list (make-omn
                   :pitch (flatten chr-dev15)
                   :length length-dev2
                   :velocity '(p)))
    :time-signature '(4 4) 
    :tempo 480)
```
