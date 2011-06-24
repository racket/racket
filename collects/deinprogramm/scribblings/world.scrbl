#lang scribble/doc

@(require scribble/manual scribble/struct "shared.rkt"
          (for-label scheme
                     teachpack/deinprogramm/image
                     teachpack/deinprogramm/world))

@teachpack["world"]{Animationen}

Note: This is documentation for the @filepath{world.rkt} teachpack that goes
with the German textbook
@italic{@link["http://www.deinprogramm.de/dmda/"]{Die Macht der
Abstraktion}}.

Dieses Teachpack ermöglicht, kleine Animationen und Spiele zu programmieren.
Es enthält alle Prozeduren aus dem 
@seclink["image"]{image-Teachpack}.

@declare-exporting[teachpack/deinprogramm/world #:use-sources (deinprogramm/world)]

@defthing[world signature]{
Eine @deftech{Welt} (Name: @racket[world]) ist die Repräsentation des Zustands,
der durch die Animation abgebildet wird.
}

@defthing[mouse-event-kind signature]{
@racket[(one-of "enter" "leave" "motion" "left-down" "left-up" "middle-down" "middle-up" "right-down" "right-up")]

Eine @deftech{Mausereignis-Art} (Name: @racket[mouse-event-kind])
bezeichnet die Art eines Maus-Ereignisses:

@racket["enter"] bedeutet, daß der Mauszeiger gerade
in das Fenster hinein bewegt wurde.  @racket["leave"] bedeutet, daß der
Mauszeiger gerade aus dem Fenster heraus bewegt wurde.
@racket["motion"] bedeutet, daß der Mauszeiger innerhalb des
Fensters bewegt wurde.  Die anderen Zeichenketten bedeuten, daß der
entsprechende Mausknopf gedrückt oder losgelassen wurde.}

@defthing[big-bang (natural natural number world -> (one-of #t))]{
Der Aufruf @racket[(big-bang w h n w)]
erzeugt eine Leinwand mit Breite @racket[w] und Höhe
@racket[h], startet die Uhr, die alle @racket[n] Sekunden
tickt, und macht @racket[w] zur ersten Welt.}

@defthing[on-tick-event ((world -> world) -> (one-of #t))]{
Der Aufruf @racket[(on-tick-event tock)]
meldet @racket[tock]
als Prozedur an, die bei jedem Uhren-Tick aufgerufen wird, um aus
der alten Welt eine neue zu machen.}

@defthing[on-key-event ((world string -> world) -> (one-of #t))]{
Der Aufruf @racket[(on-key-event change)]
meldet @racket[change]
als Prozedur an, die bei jedem Tastendruck aufgerufen wird, um aus
der alten Welt eine neue zu machen.  Dabei wird als Argument eine
Zeichenkette übergeben, welche die Taste darstellt, also
@racket["a"] für die A-Taste etc., sowie @racket["up"],
@racket["down"], @racket["left"], und @racket["right"]
für die entsprechenden Pfeiltasten und @racket["wheel-up"] für die
Bewegung des Mausrads nach oben und @racket["wheel-down"] für die
Bewegung des Mausrads nach unten.}

@defthing[on-mouse-event ((world natural natural mouse-event-kind -> world) -> (one-of #t))]{
Der Aufruf @racket[(on-mouse-event change)]
meldet @racket[change]
als Prozedur an, die bei jedem Mausereignis aufgerufen wird, um aus
der alten Welt eine neue zu machen.  Die @racket[change]-Prozedur
wird als @racket[(change w x y k)] aufgerufen. Dabei ist @racket[w]
die alte Welt, @racket[x] und @racket[y] die Koordinaten des
Mauszeigers, und @racket[k] die Art des Mausereignisses.}

@defthing[on-redraw ((world -> image) -> (one-of #t))]{
Der Aufruf @racket[(world->image world->image)]
meldet die
Prozedur @racket[world->image] an, die aus einer Welt
ein Bild macht, das auf der Leinwand dargestellt wird.}

@defthing[end-of-time (string -> world)]{
Diese Prozedur hält die Welt an und druckt ihr Argument in der REPL aus.}

