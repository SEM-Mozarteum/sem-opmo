# Assembling and Disassembling

Opusmodus Tutorial - Studio for Electronic Music (SEM)\
University Mozarteum, Salzburg\
Copyright © 2015-2026, Achim Bornhoeft


---

## Overview

This tutorial covers techniques for assembling and disassembling musical material in Opusmodus - selecting subsequences, extracting events, choosing specific measures, and deconstructing OMN objects back into their parameters.


---

## subseq - Lisp Standard Function for Lists

`subseq` is a standard Common Lisp function that extracts a portion of a list by index position.

```lisp
(setf lis (gen-integer 1 10))
```

Rest of the list from index `2` (= 3rd position):

```lisp
(subseq lis 2) ; = (3 4 5 6 7 8 9 10)
```

A bounded sublist from index `2` up to (but not including) index `5`:

```lisp
(subseq lis 2 5) ; = (3 4 5)
```

## assemble-subseq - Assembling Musical Material

**`assemble-subseq`** lets you combine specific measure ranges from different sequences into a new sequence. First, two sequences of different musical material are created:

```omn
(setf pitches1 (midi-to-pitch
                (loop for i from 1 to 10
                  collect 
                  (loop repeat i collect (+ 48 (random 24))))))

(setf seq-1 (make-omn
             :pitch pitches1
             :length '(1/16)
             :velocity '(f)
             :articulation '(stacc)
             :span :pitch))
                       
(setf pitches2 (midi-to-pitch
                (loop for i from 10 downto 1
                  for n from 60
                  collect (loop repeat i collect n))))   

(setf seq-2 (make-omn
             :pitch pitches2
             :length '(1/8)
             :velocity '(pp)
             :articulation'(ten)
             :span :pitch))
```

### Assembling Specific Subsets (Measures)

Use `:start` and `:end` keywords to select measure ranges from each sequence:

```omn
(setf mat-select1 (assemble-subseq 
               '((seq-1 :start 1 :end 4)
                 (seq-2 :start 1 :end 1)
                 (seq-1 :start 5 :end 7)
                 (seq-2 :start 2 :end 3)
                 (seq-1 :start 8 :end 9)
                 (seq-2 :start 4 :end 6)
                 (seq-1 :start 10 :end 10)
                 (seq-2 :start 7 :end 10))))
```

### Alternating Material with a Loop

Collect material from two sequences measure by measure in alternation:

```omn
(setf mat-select2 (loop for i in seq-1
                for j in seq-2
                collect i
                collect j))
```

### Randomly Chosen Measures

```omn
(loop repeat 10
      with l = (length pitches2)
      collect (nth (random l) pitches2))
```

### Specifically Chosen Measures

```omn
(loop for i in '(0 9 7 9 2 8 9 6)
      collect (nth i pitches2))
```


---

## get-events - Assembling Specific Events

**`get-events`** extracts individual events from OMN lists by specifying voice, bar number, and event index.

```omn
(setf seq1 '((e c4e4g4 he c5)
             (q c4 c4 c4 - - c4)
             (q cs4 cs4 - - cs4 cs4)
             (e c4 cs4d4eb4 d4 cs4 c4 cs5)
             (e c4 e4 g4 he c5)
             (q c4 c4 c4 - - c4)
             (q cs4cs4 - - cs4 cs4)
             (e c4 cs4d6 d4 cs4 c4 cs5)))
```

The argument list specifies: sequence (voice), bar number, event number.

1st voice, 1st bar, 1st event:

```omn
(get-events '(1 1 1) seq1)
```

1st voice, 4th bar, events `2` and `6`:

```omn
(get-events '(1 4 (2 6)) seq1)
```

Events assembled from different measures:

```omn
(get-events '((1 4 2)
              (1 1 1)
              (1 8 2))
            seq1)
```

### Random Event Selection

Use a loop with **`init-seed`** for reproducible random choices. A random measure is picked, then a random event index within that measure is selected:

```omn
(loop repeat 10
      initially (init-seed 13)
      for measure-no = (rndn 1 (length seq1))
      for max-event = (length 
                       (single-events 
                        (nth (- measure-no 1) seq1)))
      append
        (get-events (list
                     1
                     measure-no
                     (rndn 0 max-event)) seq1))
```


---

## select-measure - Extracting Bars and Beats

**`select-measure`** extracts a range of measures and beats from an OMN sequence. The selection is given as a pair of `(measure beat)` positions.

A diatonic scale as example material:

```omn
(setf omn
      '((q c3 q d3 q e3 q f3) 
        (q c4 q d4 q e4 q f4)
        (q c5 q d5 q e5 q f5)
        (q c6 q d6 q e6 q f6)))
```

Select from measure `1`, beat `3` to measure `2`, beat `1`:

```omn
(setf select (select-measure '((1 3) (2 1)) omn))

;; view with cmd-2
(list omn (append '(-q -q) select))
```

### Working with Real Score Material

The `:ratio` keyword determines how the material is subdivided. Using the first violin part of Schubert D 810, 2nd movement:

```omn
(setf vn1-mat '((h g4 pp q q) 
                (h g4 q a4 bb4) 
                (h a4 q g4 fs4) 
                (h g4 q q)))
```

Select from measure `2`, beat `3` to measure `3`, beat `4`, subdivided in quarter notes:

```omn
(setf sel1 (select-measure '((2 3) (3 4)) vn1-mat
:ratio 1/4 :time-signature '(4 4)))

;; display with cmd-2
(list vn1-mat (append '(-w -h) sel1))
```

Select with eighth-note subdivision:

```omn
(setf sel2 (select-measure
            '((2 6) (3 7)) vn1-mat
            :ratio 1/8 :time-signature '(4 4)))

;; display with cmd-2
(list vn1-mat (append '(-w -h -e) sel2))
```

### Multi-Voice Score Selection

Import a MusicXML score and extract the same (or different) sections from each voice simultaneously:

```lisp
(setf 
 material
 (musicxml-to-omn
  "~/Documents/code/github/sem-opmo/11-Assembling/schubert-D810-2.musicxml"))
```

```omn2
(loop for i in material
      collect (select-measure '(((3 3) (4 2)) 
                                ((3 2) (4 1)) 
                                ((6 3) (7 4)) 
                                ((3 4) (5 1))) i 
                              :ratio 1/4 
                              :time-signature '(4 4)))
```

### Algorithmic Window Technique

Shift a fixed window of three quarter notes through the score one beat at a time. Start and end points are computed automatically:

```lisp
(setf vn1-mat 
      '((h g4 pp q q) (h g4 q a4 bb4) (h a4 q g4 fs4) (h g4 q q)))

(setf start-points
      (loop for meas from 1 to 4
      append
      (loop for beat from 1 to 4
            collect (list meas beat))))

;; end-points = start-point + 2 quarters
(setf end-points (cddr start-points))

;; list start- and end-points
(setf windows
      (loop for s in start-points
            for e in end-points
            collect (list s e)))
```

Cycle through all voices using the computed windows (new time-signature 3/4), display with **cmd-2**:

```omn2
(setf score
(loop for mat in material
      collect
        (length-legato
        (loop for i in windows
              collect (select-measure 
                       (list i)
                       mat     
                       :ratio 1/4 
                       :time-signature '(4 4))))))
```


---

## disassemble-omn - Deconstructing OMN Objects

**`disassemble-omn`** breaks an OMN object back into its separate parameters (lengths, pitches, velocities, articulations):

```lisp
(disassemble-omn mat-select2)
```

To extract a specific parameter, use the `omn` function with the parameter keyword. Display results with **cmd-1**:

```lisp
(omn :pitch seq-1)
```

```lisp
(omn :length seq-2)
```

```lisp
(omn :velocity mat-select2)
```


---

## Related Documents

- [Readme First - OMN](Readme First - OMN.md)
- [make-omn](make-omn.md)
- [assemble-subseq](assemble-subseq.md)
- [get-events](get-events.md)
- [select-measure](select-measure.md)
- [disassemble-omn](disassemble-omn.md)
