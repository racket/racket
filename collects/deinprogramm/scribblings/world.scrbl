#lang scribble/doc

@(require scribble/manual
          "shared.ss"
	  scribble/struct
          (for-label scheme
                     teachpack/deinprogramm/image
                     teachpack/deinprogramm/world))

@teachpack["world"]{Animationen}

Note: This is documentation for the @tt{world.ss} teachpack that goes
with the German textbook
@italic{@link["http://www.deinprogramm.de/dmda/"]{Die Macht der
Abstraktion}}.

Dieses Teachpack ermöglicht, kleine Animationen und Spiele zu programmieren.
Es enthält alle Prozeduren aus dem 
@seclink["image"]{image-Teachpack}.

@declare-exporting[teachpack/deinprogramm/world #:use-sources (teachpack/deinprogramm/world)]

@defthing[world signature]{
Eine @deftech{Welt} (Name: @scheme[world]) ist die Repräsentation des Zustands,
der durch die Animation abgebildet wird.
}

@defthing[mouse-event-kind signature]{
@scheme[(one-of "enter" "leave" "motion" "left-down" "left-up" "middle-down" "middle-up" "right-down" "right-up")]

Eine @deftech{Mausereignis-Art} (Name: @scheme[mouse-event-kind])
bezeichnet die Art eines Maus-Ereignisses:

@scheme["enter"] bedeutet, daß der Mauszeiger gerade
in das Fenster hinein bewegt wurde.  @scheme["leave"] bedeutet, daß der
Mauszeiger gerade aus dem Fenster heraus bewegt wurde.
@scheme["motion"] bedeutet, daß der Mauszeiger innerhalb des
Fensters bewegt wurde.  Die anderen Zeichenketten bedeuten, daß der
entsprechende Mausknopf gedrückt oder losgelassen wurde.}

@defthing[big-bang (natural natural number world -> (one-of #t))]{
Der Aufruf @scheme[(big-bang w h n w)]
erzeugt eine Leinwand mit Breite @scheme[w] und Höhe
@scheme[h], startet die Uhr, die alle @scheme[n] Sekunden
tickt, und macht @scheme[w] zur ersten Welt.}

@defthing[on-tick-event ((world -> world) -> (one-of #t))]{
Der Aufruf @scheme[(on-tick-event tock)]
meldet @scheme[tock]
als Prozedur an, die bei jedem Uhren-Tick aufgerufen wird, um aus
der alten Welt eine neue zu machen.}

@defthing[on-key-event ((world string -> world) -> (one-of #t))]{
Der Aufruf @scheme[(on-key-event change)]
meldet @scheme[change]
als Prozedur an, die bei jedem Tastendruck aufgerufen wird, um aus
der alten Welt eine neue zu machen.  Dabei wird als Argument eine
Zeichenkette übergeben, welche die Taste darstellt, also
@scheme["a"] für die A-Taste etc., sowie @scheme["up"],
@scheme["down"], @scheme["left"], und @scheme["right"]
für die entsprechenden Pfeiltasten und @scheme["wheel-up"] für die
Bewegung des Mausrads nach oben und @scheme["wheel-down"] für die
Bewegung des Mausrads nach unten.}

@defthing[on-mouse-event ((world natural natural mouse-event-kind -> world) -> (one-of #t))]{
Der Aufruf @scheme[(on-mouse-event change)]
meldet @scheme[change]
als Prozedur an, die bei jedem Mausereignis aufgerufen wird, um aus
der alten Welt eine neue zu machen.  Die @scheme[change]-Prozedur
wird als @scheme[(change w x y k)] aufgerufen. Dabei ist @scheme[w]
die alte Welt, @scheme[x] und @scheme[y] die Koordinaten des
Mauszeigers, und @scheme[k] die Art des Mausereignisses.}

@defthing[on-redraw ((world -> image) -> (one-of #t))]{
Der Aufruf @scheme[(world->image world->image)]
meldet die
Prozedur @scheme[world->image] an, die aus einer Welt
ein Bild macht, das auf der Leinwand dargestellt wird.}

@defthing[end-of-time (string -> world)]{
Diese Prozedur hält die Welt an und druckt ihr Argument in der REPL aus.}

