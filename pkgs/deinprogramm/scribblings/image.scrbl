#lang scribble/doc

@(require scribble/manual "shared.rkt"
          (for-label scheme teachpack/deinprogramm/image))

@teachpack["image"]{Bilder konstruieren}

Note: This is documentation for the @filepath{image.rkt} teachpack that goes
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
Ein @deftech{Bild} (Name: @racket[image]) ist die Repräsentation eines Bildes.
} 

@defthing[empty-image image]{
Ein leeres Bild mit Breite und Höhe 0.
}

@defthing[image? (any -> boolean?)]{Der Aufruf @racket[(image? x)] stellt fest, ob @racket[x] ein Bild ist.}

@;-----------------------------------------------------------------------------
@section[#:tag "modes-colors"]{Modi und Farben}

@defthing[mode signature]{
@racket[(one-of "solid" "outline")]

Ein Modus (Name: @racket[mode]) legt fest, ob die Darstellung einer Form diese füllt
oder nur einen Umriss zeichnet.}

@defthing[octet signature]{
@racket[(combined natural (predicate (lambda (n) (<= n 255))))]

Ein Oktet (Name: @racket[octet]) ist eine natürliche Zahl zwischen 0 und 255.}

@defthing[rgb-color signature]{
Eine @deftech{RGB-Farbe} ist eine Farbe (Name: @racket[color], die vom
Record-Konstruktor @racket[make-color] zurückgegeben wird:
}

@defthing[make-color (octet octet octet -> rgb-color)]{
Eine @tech{RGB-Farbe} beschreibt eine Farbe mit den roten, blauen und grünen Anteilen,
also z.B. @racket[(make-color 100 200 30)].}

@defthing[color-red (color -> octet)]{
 liefert den Rot-Anteil einer RGB-Farbe.}
@defthing[color-green (color -> octet)]{
 liefert den Grün-Anteil einer RGB-Farbe.}
@defthing[color-blue (color -> octet)]{
 liefert den Blau-Anteil einer RGB-Farbe.}

@defthing[color? (any -> boolean)]{
stellt fest, ob ein Objekt eine @tech{RGB-Farbe} ist.}

@defthing[image-color signature]{
@racket[(mixed string rgb-color)] 

Eine @deftech{Farbe} (Name: @racket[image-color]) ist eine Zeichenkette aus einer Farbbezeichnung
(z.B. @racket["blue"]) oder eine @tech{RGB-Farbe}.}

@defthing[image-color? (any -> boolean?)]{ stellt fest, ob ein Objekt
eine @tech{Farbe} ist.}

@defthing[alpha-rgb-color signature]{
Eine @deftech{Alpha/RGB-Farbe} ist eine Farbe (Name: @racket[color], die vom
Record-Konstruktor @racket[make-alpha-color] zurückgegeben wird:
}

@defthing[make-alpha-color (octet octet octet octet -> alpha-color)]{
Eine @tech{Alpha/RGB-Farbe} beschreibt eine Farbe mit den Alpha-, roten,
blaue und grünen Anteilen, also z.B. @racket[(make-color 50 100 200
30)].  Der Alpha-Anteil beschreibt, wie durchsichtig die Farbe ist.}

@defthing[alpha-color-red (color -> octet)]{
 liefert den Rot-Anteil einer RGB-Farbe.}
@defthing[alpha-color-green (color -> octet)]{
 liefert den Grün-Anteil einer RGB-Farbe.}
@defthing[alpha-color-blue (color -> octet)]{
 liefert den Blau-Anteil einer RGB-Farbe.}
@defthing[alpha-color-alpha (color -> octet)]{
 liefert den Alpha-Anteil einer RGB-Farbe.}

@defthing[alpha-color? (any -> boolean)]{
stellt fest, ob ein Objekt eine @tech{Alpha/RGB-Farbe} ist.}



@;-----------------------------------------------------------------------------
@section[#:tag "creational"]{Einfache geometrische Figuren}

Die folgenden Prozeduren erzeugen Bilder mit einfachen geometrischen Formen:

@defthing[rectangle (natural natural mode image-color -> image)]{
 Der Aufruf @racket[(rectangle w h m c)]
 erzeugt ein Rechteck mit Breite @racket[w] und Höhe @racket[h], gefüllt mit Modus
 @racket[m] und in Farbe @racket[c].}

@defthing[circle (natural mode image-color -> image)]{
 Der Aufruf @racket[(circle r m c)]
 erzeugt einen Kreis oder eine Scheibe mit Radius @racket[r], gefüllt mit Modus
 @racket[m] und in Farbe @racket[c].}

@defthing[ellipse (natural natural mode image-color -> image)]{
 Der Aufruf @racket[(ellipse w h m c)]
 erzeugt eine Ellipse mit Breite @racket[w] und Höhe @racket[h], gefüllt mit Modus
 @racket[m] und in Farbe @racket[c].}

@defthing[triangle (integer mode image-color -> image)]{ 
 Der Aufruf @racket[(triangle s m c)]
 erzeugt ein nach oben zeigendes gleichseitiges Dreieck, wobei
 @racket[s] die Seitenlänge angibt, gefüllt mit Modus
 @racket[m] und in Farbe @racket[c].}

@defthing[line (natural natural number number number number image-color -> image)]{
 Der Aufruf @racket[(line w h sx sy ex ey c)]
 erzeugt ein Bild mit einer farbigen Strecke, wobei @racket[w] die Breite und @racket[h] die Höhe des Bilds,
 sowie @racket[sx] die X- und @racket[sx] die Y-Koordinate des Anfangspunkts und 
 @racket[ex] die X- und @racket[ey] die Y-Koordinate des Endpunkts angeben, gefüllt mit Modus
 @racket[m] und in Farbe @racket[c].}

@defthing[text (string natural image-color -> image)]{
 Der Aufruf @racket[(text s f c)]
 erzeugt ein Bild mit Text @racket[s],
 wobei die Buchstaben die Größe @racket[f] haben, in Farbe @racket[c]}

Außerdem können beliebige Bitmap-Bilder in ein Scheme-Programm
eingeklebt werden.

@;-----------------------------------------------------------------------------
@section[#:tag "properties"]{Eigenschaften von Bildern}

Zwei Eigenschaften von Bildern sind für ihre Manipulation nützlich,
nämlich Breite und Höhe:

@defthing[image-width (image -> natural)]{
 liefert die Breite von @racket[i] in Pixeln.}

@defthing[image-height (image -> natural)]{
 liefert die Höhe von @racket[i] in Pixeln.}

@defthing[image-inside? (image image -> boolean)]{
Der Aufruf @racket[(image-inside? i1 i2)] stellt fest, ob das Bild
@racket[i2] im Bild @racket[i1] enthalten ist.}

@defthing[find-image (image image -> posn)]{
Der Aufruf @racket[(find-image i1 i2)] findet die Position von @racket[i2]
im Bild @racket[i1] (in dem es vorkommen muss).}

@;-----------------------------------------------------------------------------
@section[#:tag "composition"]{Bilder zusammensetzen}

The nächste Gruppe von Prozeduren baut aus Bildern neue Bilder:

@defthing[h-place signature]{
@racket[(mixed integer (one-of "left" "right" "center"))]

Eine @deftech{horizontale Positionsangabe} (Name: @racket[h-place])
gibt an, wie zwei Bilder horizontal zueinander positioniert werden

Im ersten Fall, wenn es sich um eine Zahl @racket[x] handelt, wird das
zweite  Bild  @racket[x] Pixel vom  linken  Rand auf das erste gelegt.
Die drei Fälle mit Zeichenketten sagen,  daß die Bilder am linken Rand
bzw. am rechten   Rand bündig plaziert werden,  bzw.  das zweite  Bild
horizontal in die Mitte des ersten gesetzt wird.}

@defthing[v-place signature]{
@racket[(mixed integer (one-of "top" "bottom" "center"))]

Eine @deftech{vertikale Positionsangabe} (Name: @racket[v-place]) 
gibt an, wie zwei Bilder vertikal zueinander positioniert werden

Im ersten Fall, wenn es sich um eine Zahl @racket[y] handelt, wird das
zweite Bild @racket[y] Pixel vom oberen Rand auf das erste gelegt.
Die drei Fälle mit Zeichenketten sagen, daß die Bilder am oberen Rand
bzw. am unteren Rand bündig plaziert werden, bzw. das zweite Bild
vertikal in die Mitte des ersten gesetzt wird.
}

@defthing[h-mode signature]{
@racket[(one-of "left" "right" "center")]
Eine @deftech{horizontale Justierungsangabe} (Name: @racket[h-mode]) 
gibt an, ob zwei Bilder, die übereinander angeordnet werden, entlang der linken
Kante, der rechten Kante oder der Mitte angeordnet werden.
}

@defthing[v-mode signature]{
@racket[(one-of "top" "bottom" "center")]

Eine @deftech{vertikale Justierungsangabe} (Name: @racket[V-mode])
gibt an, ob zwei Bilder, die nebenander angeordnet werden, entlang der
oberen Kante, der untern Kante oder der Mitte angeordnet werden.}

@defthing[overlay (image image h-place v-place -> image)]{
 Der Aufruf @racket[(overlay img other h v)]
 legt zweite Bild @racket[other] auf das erste @racket[img].  Die beiden anderen Argumente geben an, wie
 die beiden Bilder zueinander positioniert werden.}

@defthing[beside (image image v-mode -> image)]{
 Der Aufruf @racket[(beside img other v)]
 ordnet die beiden Bilder entsprechend des @racket[v]-Arguments
 nebeneinander an.}

@defthing[above (image image h-mode -> image)]{
 Der Aufruf @racket[(img other h -> image)]
 ordnet die beiden Bilder entsprechend des @racket[h]-Arguments
 übereinander an.}

@defthing[clip (image natural natural natural natural -> image)]{
 Der Aufruf @racket[(clip img x y w h)]
 liefert das Teilrechteck des Bildes @racket[img]
 bei (@racket[x], @racket[y]), Breite @racket[w] und Höhe @racket[h].}

@defthing[pad (image natural natural natural natural -> image)]{
  Der Aufruf @racket[(pad img l r t b)]
  fügt an den Seiten von @racket[img] noch transparenten Leerraum an:
  @racket[l] Pixel links, @racket[r] Pixel rechts, @racket[t] Pixel oben und
  @racket[b] Pixel unten.}


@;-----------------------------------------------------------------------------
@section[#:tag "from-pixels"]{Bilder aus Pixeln konstruieren}

@defthing[color-list->image ((list-of color) natural natural -> image)]{
  Der Aufruf @racket[(color-list->image lis w h)] stellt ein Bild mit
  Breite @racket[w] und Höhe @racket[h] her, in dem die Pixel die
  Farben aus der Liste @racket[lis] (welche die Länge @racket[(* w h)]
  haben muß) haben.}

@defthing[image->color-list (image -> (list-of rgb-color))]{
  Diese Prozedur liefert eine Liste der RGB-Farben der Pixel eines Bildes.}

@defthing[alpha-color-list->image ((list-of alpha-rgb-color) natural natural -> image)]{
  Der Aufruf @racket[(color-list->image lis w h)] stellt ein Bild mit
  Breite @racket[w] und Höhe @racket[h] her, in dem die Pixel die
  Farben aus der Liste @racket[lis] (welche die Länge @racket[(* w h)]
  haben muß) haben.}

@defthing[image->alpha-color-list (image -> (list-of rgb-color))]{
  Diese Prozedur liefert eine Liste der Alpha/RGB-Farben der Pixel
  eines Bildes.}
