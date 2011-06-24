#lang scribble/doc

@(require scribble/manual scribble/struct "shared.rkt"
          (for-label scheme
                     teachpack/deinprogramm/image
                     teachpack/deinprogramm/turtle))

@teachpack["turtle"]{Turtle-Grafik}

Note: This is documentation for the @filepath{turtle.rkt} teachpack that goes
with the German textbook
@italic{@link["http://www.deinprogramm.de/dmda/"]{Die Macht der
Abstraktion}}.

@declare-exporting[teachpack/deinprogramm/turtle #:use-sources (teachpack/deinprogramm/turtle)]

Turtle-Grafik ist eine Methode zum Erstellen von Computergrafiken. Das
Zeichnen wird dabei durch das Bewegen einer virtuellen Schildkröte
über den Zeichenbereich modelliert. Eine Schildkröte kann durch drei
Befehle bewegt werden:

@itemize[
 @item{@racket[(move n)] Bewegt die Schildkröte um @racket[n] Pixel ohne zu zeichnen.}
 @item{@racket[(draw n)] Bewegt die Schildkröte um @racket[n] Pixel und zeichnet dabei.}
 @item{@racket[(turn n)] Dreht die Schildkröte um n Grad im Uhrzeigersinn.}
]

Wir stellen jetzt ein Teachpack für DrRacket vor, mit dessen Hilfe
solche Turtle-Grafiken erstellt werden können. 

@section{Tutorial}

Unser Ziel ist es, in diesem Tutorial ein Quadrat mithilfe der
Prozeduren des Teachpacks zu zeichnen. Aus diesem Grund müssen wir
zunächst mit der Prozedur @racket[draw] eine Linie nach rechts malen. Die
initiale Ausgansposition der Turtle ist in der Bildmitte mit Blick
nach rechts. Mit @racket[(draw 20)] bewegen wir die Turtle dann 20 Pixel nach
rechts und zeichnen dabei. Um das resultierende Bild zu sehen ist,
müssen wir die Turtle mittels der Prozedur run laufen lassen. Die
restlichen Parameter für run sind die Höhe und die Breite des Bildes
sowie die Farbe, in der gezeichnet werden soll. Geben Sie also
folgenden Befehl in die REPL ein, um Ihre erste Turtle-Grafik zu
erstellen:

@racketblock[
(run (draw 20) 100 100 "red")
]

Sie erhalten dann eine Ausgabe wie die folgende: 

@image["p1.jpg"] 

Nun vervollständigen wir die Linie zu einem rechten Winkel: wir drehen
die Turtle um 90° nach rechts und zeichnen dann eine Line der Länge 20
Pixel nach unten. Zum Drehen einer Turtle verwenden wir die Prozedur
@racket[turn].

Da wir ein Quadrat aus zwei rechten Winkeln zusammensetzen können,
abstrahieren wir über das Zeichnen des rechten Winkels. Dazu schreiben
wir eine Prozedur @racket[right-angle] die als Parameter eine Turtle
erhält:

@racketblock[
(: right-angle (turtle -> turtle))
(define right-angle 
  (lambda (t1)
    (let* ((t2 ((draw 20) t1))
           (t3 ((turn -90) t2))
           (t4 ((draw 20) t3)))
      t4)))
]

Das Ergebnis sieht dann so aus: 

@image["p2.jpg"]

Um das Quadrat komplett zu zeichnen, sollen nun zwei rechte Winkel
verwendet werden. Wir zeichnen also einen rechten Winkel, drehen uns
um 90° nach rechts, und zeichnen einen zweiten rechten Winkel.

@racketblock[
(: square (turtle -> turtle))
(define square  
  (lambda (t1)
    (let* ((t2 (right-angle t1))
           (t3 ((turn -90) t2))
           (t4 (right-angle t3)))
      t4)))
]

So sieht das Ergebnis aus: 

@image["p3.jpg"]

@subsection{Verbesserungen}

An dem Beispiel ist leicht zu sehen, dass es zum Zeichnen mit Hilfe
von Turtle-Grafik oft erforderlich ist, Zwischenwerte wie @racket[t1],
@racket[t2] etc., an die nächste Prozedur weiterzureichen, die Werte
ansonsten aber nicht weiterverwendet werden. Beispielsweise werden in
der obigen Definition von square die Variablen @racket[t1], ...,
@racket[t4] nur gebraucht, um die Prozeduren @racket[right-angle],
@racket[(turn -90)] und @racket[right-angle] hintereinander
auszuführen.

Um solche Fälle einfach programmieren zu können, enthält das
Turtle-Teachpack die Prozedur @racket[sequence]. Damit können wir eine
zu @racket[right-angle] äquivalente Version wesentlicher einfacher
aufschreiben:

@racketblock[
(define right-angle2 
  (sequence (draw 20) (turn -90) (draw 20)))
]

Ebenso wie @racket[right-angle] können wir square leichter schreiben als: 

@racketblock[
(define square2  
  (sequence right-angle (turn -90) right-angle))
]

@section{Prozeduren}

@declare-exporting[teachpack/deinprogramm/turtle]

@defthing[turtle signature]{
Dies ist die Signatur für Turtles.
} 

@defthing[set-color (color -> (turtle -> turtle))]{ Diese Prozedur ist
eine Prozedurfabrik. Sie liefert als Ergebnis eine Prozedur, die auf
eine Turtle anwendbar ist. Wendet man das Ergebnis auf eine Turtle an,
so ändert dies die Farbe mit der gezeichnet wird.

Folgender Code 

@racketblock[
(define square3
  (sequence right-angle (turn -90) (set-color "blue") right-angle))
]
liefert dieses Bild: 

@image["p4.jpg"]
}

@defthing[turn (number -> (turtle -> turtle))]{ Diese Prozedur ist
eine Prozedurfabrik. Sie liefert als Ergebnis eine Prozedur, die auf
eine Turtle anwendbar ist. Wendet man das Ergebnis auf eine Turtle an,
so ändert sich die Blickrichtung der Turtle um die gegebene Gradzahl
gegen den Uhrzeigersinn.
}

@defthing[draw (number -> (turtle -> turtle))]{ Diese Prozedur ist
eine Prozedurfabrik. Sie liefert als Ergebnis eine Prozedur, die auf
eine Turtle anwendbar ist. Wendet man das Ergebnis auf eine Turtle an,
so bewegt sich die Schildkröte um die gegebene Anzahl von Pixel und
zeichnet dabei eine Linie.}

@defthing[move (number -> (turtle -> turtle))]{ Diese Prozedur ist eine
Prozedurfabrik. Sie liefert als Ergebnis eine Prozedur, die auf ein
Turtle anwendbar ist. Wendet man das Ergebnis auf eine Turtle an, so
bewegt sich die Schildkröte um die gegebene Anzahl von Pixel, zeichnet
dabei aber keine Linie.}

@defthing[run ((turtle -> turtle) number number color -> image)]{
Diese Prozedur wendet die übergebene Prozedur von Turtle nach Turtle
auf die initiale Schildkröte an und zeigt das daraus resultierende
Bild an. Der zweite Parameter ist die Höhe des Bilds, der dritte
Parameter die Breite des Bilds und der vierte Parameter die Farbe, mit
der gezeichnet wird.
}

@defthing[sequence ((turtle -> turtle) ... -> (turtle -> turtle))]{
Diese Prozedur nimmt eine beliebige Anzahl von Turtle-Veränderungen
(d.h. Prozeduren mit Signatur @racket[turtle -> turtle]) und erstellt
eine neue Prozedur, die die Veränderungen der Reihe nach von links
nach rechts abarbeitet.}
