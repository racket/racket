#lang scribble/doc

@(require scribble/manual "shared.ss"
          (for-label scheme
                     teachpack/deinprogramm/image))

@teachpack["image"]{Bilder konstruieren}

Note: This is documentation for the @tt{image.ss} teachpack that goes
with the German textbook
@italic{@link["http://www.deinprogramm.de/dmda/"]{Die Macht der
Abstraktion}}.

@declare-exporting[teachpack/deinprogramm/image #:use-sources (deinprogramm/image)]

Dieses Teachpack definiert Prozeduren für die Konstruktion von Bildern.
Einfache Bilder werden als geometrische Formen oder Bitmaps konstruiert.
Zusätzliche Prozeduren erlauben die Komposition von Bildern.

@;-----------------------------------------------------------------------------
@section{Bilder}

@defthing[image signature]{
Ein @deftech{Bild} (Name: @scheme[image]) ist die Repräsentation eines Bildes.
} 

@defthing[empty-image image]{
Ein leeres Bild mit Breite und Höhe 0.
}

@defthing[image? (%a -> boolean?)]{Der Aufruf @scheme[(image? x)] stellt fest, ob @scheme[x] ein Bild ist.}

@;-----------------------------------------------------------------------------
@section[#:tag "modes-colors"]{Modi und Farben}

@defthing[mode signature]{
@scheme[(one-of "solid" "outline")]

Ein Modus (Name: @scheme[mode]) legt fest, ob die Darstellung einer Form diese füllt
oder nur einen Umriss zeichnet.}

@defthing[octet signature]{
@scheme[(combined natural (predicate (lambda (n) (<= n 255))))]

Ein Oktet (Name: @scheme[octet]) ist eine natürliche Zahl zwischen 0 und 255.}

@defthing[rgb-color signature]{
Eine @deftech{RGB-Farbe} ist eine Farbe (Name: @scheme[color], die vom
Record-Konstruktor @scheme[make-color] zurückgegeben wird:
}

@defthing[make-color (octet octet octet -> rgb-color)]{
Eine @tech{RGB-Farbe} beschreibt eine Farbe mit den roten, blauen und grünen Anteilen,
also z.B. @scheme[(make-color 100 200 30)].}

@defthing[color-red (color -> octet)]{
 liefert den Rot-Anteil einer RGB-Farbe.}
@defthing[color-green (color -> octet)]{
 liefert den Grün-Anteil einer RGB-Farbe.}
@defthing[color-blue (color -> octet)]{
 liefert den Blau-Anteil einer RGB-Farbe.}

@defthing[image-color signature]{
@scheme[(mixed string color)] 

Eine @deftech{Farbe} (Name: @scheme[image-color]) ist eine Zeichenkette aus einer Farbbezeichnung
(z.B. @scheme["blue"]) oder eine @tech{RGB-Farbe}.}

@defthing[image-color? (%a -> boolean?)]{ stellt fest, ob ein Objekt
eine @tech{Farbe} ist.}


@;-----------------------------------------------------------------------------
@section[#:tag "creational"]{Einfache geometrische Figuren}

Die folgenden Prozeduren erzeugen Bilder mit einfachen geometrischen Formen:

@defthing[rectangle (natural natural mode image-color -> image)]{
 Der Aufruf @scheme[(rectangle w h m c)]
 erzeugt ein Rechteck mit Breite @scheme[w] und Höhe @scheme[h], gefüllt mit Modus
 @scheme[m] und in Farbe @scheme[c].}

@defthing[circle (natural mode image-color -> image)]{
 Der Aufruf @scheme[(circle r m c)]
 erzeugt einen Kreis oder eine Scheibe mit Radius @scheme[r], gefüllt mit Modus
 @scheme[m] und in Farbe @scheme[c].}

@defthing[ellipse (natural natural mode image-color -> image)]{
 Der Aufruf @scheme[(ellipse w h m c)]
 erzeugt eine Ellipse mit Breite @scheme[w] und Höhe @scheme[h], gefüllt mit Modus
 @scheme[m] und in Farbe @scheme[c].}

@defthing[triangle (integer mode image-color -> image)]{ 
 Der Aufruf @scheme[(triangle s m c)]
 erzeugt ein nach oben zeigendes gleichseitiges Dreieck, wobei
 @scheme[s] die Seitenlänge angibt, gefüllt mit Modus
 @scheme[m] und in Farbe @scheme[c].}

@defthing[line (natural natural number number number number image-color -> image)]{
 Der Aufruf @scheme[(line w h sx sy ex ey c)]
 erzeugt ein Bild mit einer farbigen Strecke, wobei @scheme[w] die Breite und @scheme[h] die Höhe des Bilds,
 sowie @scheme[sx] die X- und @scheme[sx] die Y-Koordinate des Anfangspunkts und 
 @scheme[ex] die X- und @scheme[ey] die Y-Koordinate des Endpunkts angeben, gefüllt mit Modus
 @scheme[m] und in Farbe @scheme[c].}

@defthing[text (string natural image-color -> image)]{
 Der Aufruf @scheme[(text s f c)]
 erzeugt ein Bild mit Text @scheme[s],
 wobei die Buchstaben die Größe @scheme[f] haben, in Farbe @scheme[c]}

Außerdem können beliebige Bitmap-Bilder in ein Scheme-Programm
eingeklebt werden.

@;-----------------------------------------------------------------------------
@section[#:tag "properties"]{Eigenschaften von Bildern}

Zwei Eigenschaften von Bildern sind für ihre Manipulation nützlich,
nämlich Breite und Höhe:

@defthing[image-width (image -> natural)]{
 liefert die Breite von @scheme[i] in Pixeln.}

@defthing[image-height (image -> natural)]{
 liefert die Höhe von @scheme[i] in Pixeln.}

@;-----------------------------------------------------------------------------
@section[#:tag "composition"]{Bilder zusammensetzen}

The nächste Gruppe von Prozeduren baut aus Bildern neue Bilder:

@defthing[h-place signature]{
@scheme[(mixed integer (one-of "left" "right" "center"))]

Eine @deftech{horizontale Positionsangabe} (Name: @scheme[h-place])
gibt an, wie zwei Bilder horizontal zueinander positioniert werden

Im ersten Fall, wenn es sich um eine Zahl @scheme[x] handelt, wird das
zweite  Bild  @scheme[x] Pixel vom  linken  Rand auf das erste gelegt.
Die drei Fälle mit Zeichenketten sagen,  daß die Bilder am linken Rand
bzw. am rechten   Rand bündig plaziert werden,  bzw.  das zweite  Bild
horizontal in die Mitte des ersten gesetzt wird.}

@defthing[v-place signature]{
@scheme[(mixed integer (one-of "top" "bottom" "center"))]

Eine @deftech{vertikale Positionsangabe} (Name: @scheme[v-place]) 
gibt an, wie zwei Bilder vertikal zueinander positioniert werden

Im ersten Fall, wenn es sich um eine Zahl @scheme[y] handelt, wird das
zweite Bild @scheme[y] Pixel vom oberen Rand auf das erste gelegt.
Die drei Fälle mit Zeichenketten sagen, daß die Bilder am oberen Rand
bzw. am unteren Rand bündig plaziert werden, bzw. das zweite Bild
vertikal in die Mitte des ersten gesetzt wird.
}

@defthing[h-mode signature]{
@scheme[(one-of "left" "right" "center")]
Eine @deftech{horizontale Justierungsangabe} (Name: @scheme[h-mode]) 
gibt an, ob zwei Bilder, die übereinander angeordnet werden, entlang der linken
Kante, der rechten Kante oder der Mitte angeordnet werden.
}

@defthing[v-mode signature]{
@scheme[(one-of "top" "bottom" "center")]

Eine @deftech{vertikale Justierungsangabe} (Name: @scheme[V-mode])
gibt an, ob zwei Bilder, die nebenander angeordnet werden, entlang der
oberen Kante, der untern Kante oder der Mitte angeordnet werden.}

@defthing[overlay (image image h-place v-place -> image)]{
 Der Aufruf @scheme[(overlay img other h v)]
 legt zweite Bild @scheme[other] auf das erste @scheme[img].  Die beiden anderen Argumente geben an, wie
 die beiden Bilder zueinander positioniert werden.}

@defthing[beside (image image v-mode -> image)]{
 Der Aufruf @scheme[(beside img other v)]
 ordnet die beiden Bilder entsprechend des @scheme[v]-Arguments
 nebeneinander an.}

@defthing[above (image image h-mode -> image)]{
 Der Aufruf @scheme[(img other h -> image)]
 ordnet die beiden Bilder entsprechend des @scheme[h]-Arguments
 übereinander an.}

@defthing[clip (image natural natural natural natural -> image)]{
 Der Aufruf @scheme[(clip img x y w h)]
 liefert das Teilrechteck des Bildes @scheme[img]
 bei (@scheme[x], @scheme[y]), Breite @scheme[w] und Höhe @scheme[h].}

@defthing[pad (image natural natural natural natural -> image)]{
  Der Aufruf @scheme[(pad img l r t b)]
  fügt an den Seiten von @scheme[img] noch transparenten Leerraum an:
  @scheme[l] Pixel links, @scheme[r] Pixel rechts, @scheme[t] Pixel oben und
  @scheme[b] Pixel unten.}

