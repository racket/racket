#lang scribble/doc

@(require scribble/manual "shared.rkt"
          (for-label scheme
                     teachpack/deinprogramm/image
                     teachpack/deinprogramm/line3d))

@teachpack["line3d"]{3D-Liniengraphik}

Note: This is documentation for the @filepath{line3d.rkt} teachpack that goes
with the German textbook
@italic{@link["http://www.deinprogramm.de/dmda/"]{Die Macht der
Abstraktion}}.

@declare-exporting[teachpack/deinprogramm/line3d #:use-sources (teachpack/deinprogramm/line3d)]

Dieses teachpack definiert Prozeduren für lineare Algebra und 3D-Rendering:

@;----------------------------------------------------------------------------------
@section[#:tag "rendering"]{Szenen erzeugen}

@declare-exporting[teachpack/deinprogramm/line3d]

@defthing[render-scene (natural natural (list-of line3d) matrix4x4 -> image)]{
 Der Aufruf @racket[(render-scene width height scene camera-matrix)]erzeugt die Szene
 in ein Bild mit Breite @racket[width] und Höhe @racket[height]. Position,
 Orientierung und Projektion werden durch die @racket[camera-matrix] festgelegt.
}
 
@defthing[create-camera-matrix (vec3 vec3 number natural natural -> matrix4x4)]{
 Der Aufruf @racket[(create-camera-matrix position lookat vertical-fov width height)]
 erzeugt eine 4x4 Matrix. Diese kodiert eine Kamera an der Position @racket[position], die 
 auf die Position @racket[lookat] schaut.
 @racket[vertical-fov] bezeichnet das @deftech{vertikale Feld} der Szene.
}

Zum Beispiel:

@racketblock[
(code:comment @#,t{scene-data (simple box example)})
(define box
  (create-box 1.0 1.0 1.0 "brown")) 
(code:comment @#,t{screen})
(define screenWidth 320)
(define screenHeight 240) 
(code:comment @#,t{camera})
(define pos (make-vec3 5 5 3))
(define lookat (make-vec3 0 0 0))
(define camera
  (create-camera-matrix pos lookat 70.0 screenWidth screenHeight))
(code:comment @#,t{render image})
(render-scene screenWidth screenHeight box camera)
]

@;-------------------------------------------------------------------------------------
@section[#:tag "3Dvectors"]{3D-Vektoren}

@defthing[vec3 signature]{
 Ein @deftech{3D-Vektor} (Name: @racket[vec3]) ist ein Record, der durch den Aufruf @racket[make-vec3] erstellt wird.
}

@defthing[make-vec3 (number number number -> vec3)]{
 @racket[(make-vec3 x y z)] erstellt einen Vektor (x,y,z).
}

@defthing[add-vec3 (vec3 vec3 -> vec3)]{
 @racket[(add-vec3 a b)] gibt die Summe von @racket[a] und @racket[b] zurück.
}

@defthing[sub-vec3 (vec3 vec3 -> vec3)]{
 @racket[(sub-vec3 a b)] gibt die Differenz zwischen @racket[a] und @racket[b] zurück.
}

@defthing[mult-vec3 (vec3 number -> vec3)]{
 @racket[(mult-vec3 a s)] gibt den das Produkt von @racket[a] und @racket[s] zurück.
}

@defthing[div-vec3 (vec3 number -> vec3)]{
 @racket[(div-vec3 a s)] gibt den das Produkt von @racket[a] und dem Kehrwert von @racket[s] zurück.
}

@defthing[dotproduct-vec3 (vec3 vec3 -> number)]{
 @racket[(dotproduct-vec3 a b)] gibt das Produkt von @racket[a] und @racket[b] zurück.
}

@defthing[normQuad-vec3 (vec3 -> number)]{
 @racket[(normQuad-vec3 a)] gibt die quadrierte Norm/Länge |@racket[a]|² eines Vektors @racket[a] zurück (Quadrat der Euklidischen Norm.)
}

@defthing[norm-vec3 (vec3 -> number)]{
 @racket[(norm-vec3 a)] gibt die Norm/Länge |@racket[a]| eines Vektors a zurück (Euklidische Norm.)
}

@defthing[normalize-vec3 (vec3 -> vec3)]{
 @racket[(normalize-vec3 a)] normalisiert @racket[a].
}

@defthing[crossproduct-vec3 (vec3 vec3-> vec3)]{
 @racket[(crossproduct-vec3 a b)] gibt das Kreuzprodukt von @racket[a]
und @racket[b] zurück (einen Vektor der senkrecht auf @racket[a] und @racket[b] steht).
}

@;-------------------------------------------------------------------------------------
@section[#:tag "4Dvectors"]{4D-Vektoren}

@defthing[vec4 signature]{
 Ein @deftech{4D-Vektor} @racket[vec4] ist ein 4D-Vektor. Folgende Prozeduren werden bereitgestellt:
}

@defthing[make-vec4 (number number number number -> vec4)]{
 @racket[(make-vec4 a b c d)] erzeugt einen Vektor aus @racket[a], @racket[b], @racket[c] und @racket[d].
}

@defthing[add-vec4 (vec4 vec4 -> vec4)]{
@racket[(add-vec4 a b)]  gibt die Summe von @racket[a] und @racket[b] zurück.
}

@defthing[sub-vec4 (vec4 vec4 -> vec4)]{
 @racket[(sub-vec4 a b)] gibt die Differenz zwischen @racket[a] und @racket[b] zurück.
}

@defthing[mult-vec4 (vec4 number -> vec4)]{
 @racket[(mult-vec4 a s)] gibt den das Produkt von @racket[a] und @racket[s] zurück.
}

@defthing[div-vec4 (vec4 number -> vec4)]{
 @racket[(div-vec4 a s)] gibt den das Produkt von @racket[a] und dem Kehrwert von @racket[s] zurück.
}

@defthing[dotproduct-vec4 (vec3 vec4 -> number)]{
 @racket[(dotproduct-vec4 a b)] gibt die quadrierte Norm/Länge |@racket[a]|² eines Vektors @racket[a] zurück (Quadrat der Euklidischen Norm.)
}

@defthing[normQuad-vec4 (vec4 -> number)]{
 @racket[(normQuad-vec4 a)] gibt die quadrierte Norm/Länge |@racket[a]|² eines Vektors @racket[a] zurück (Quadrat der Euklidischen Norm.)
}

@defthing[norm-vec4 (vec4 -> number)]{
 @racket[(norm-vec4 a)] gibt die Norm/Länge |a| eines Vektors a zurück (Euklidische Norm)
}

@defthing[normalize-vec4 (vec4 -> vec4)]{
 @racket[(normalize-vec4 a)] normalisiert @racket[a].
}

@defthing[expand-vec3 (vec3 number -> vec4)]{
 @racket[(expand-vec3 a s)] gibt den 4D-Vektor mit @racket[s] als letze Komponente zurück (erweitert @racket[a] mit @racket[s]).
}

@;-------------------------------------------------------------------------------------
@section[#:tag "4x4matrix"]{4x4 Matrizen}

@defthing[matrix4x4 signature]{
 Eine @deftech{Matrix} @racket[matrix4x4] ist ein Record, der durch den Aufruf @racket[make-matrix4x4] erstellt wird.
}

@defthing[make-matrix4x4 (vec4 vec4 vec4 vec4 -> matrix4x4)]{
 @racket[(make-matrix4x4 a b c d)] erstellt eine Matrix aus @racket[a], @racket[b], @racket[c] und @racket[d].
}

@defthing[create-matrix4x4 (vec3 vec3 vec3 vec3 -> matrix4x4)]{
 @racket[(create-matrix4x4 a b c d)] erweitert jeden Vektor in einen 4D-Vektor und kombiniert diese zu
 einer Matrix @racket[a], @racket[b], @racket[c] und @racket[d], wobei
 @racket[a], @racket[b], @racket[c] mit 0 und @racket[d] mit 1 erweitert wird, um eine homogene Matrix zu erzeugen.
}

@defthing[transpose-matrix4x4 (matrix4x4 -> matrix4x)]{
 @racket[(transpose-matrix4x4 m)] erstellt die transponierte Matrix @racket[m]^@racket[T].
}

@defthing[multiply-matrix-vec4 (matrix vec4 -> vec4)]{
 @racket[(multiply-matrix-vec4 m v)] gibt die Matrix @racket[m]@racket[v] zurück. Die @racket[w]-Komponente ist nicht normalisiert.
}

@defthing[transform-vec3 (matrix4x4 vec3 -> vec3)]{
 @racket[(transform-vec3 m v)] erweitert @racket[v] mit 1, multipliziert @racket[m] mit @racket[v] und dividiert das Ergebnis mit @racket[w].
}

@defthing[multiply-matrix (matrix4x4 matrix4x4 -> matrix4x4)]{
 @racket[(multiply-matrix a b)] gibt die Matrix @racket[a]*@racket[b] zurück.
}

@defthing[create-translation-matrix (vec3 -> matrix4x4)]{
 @racket[(create-translation-matrix v)] gibt die Translations-Matrix zurück.
}

@defthing[create-rotation-x-matrix (number -> matrix4x4)]{
 @racket[(create-rotation-x-matrix a)] gibt eine Rotations-Matrix zurück die um die X-Achse mit dem Winkel @racket[a] rotiert.
}

@defthing[create-rotation-y-matrix (number -> matrix4x4)]{
 @racket[(create-rotation-y-matrix a)] gibt eine Rotations-Matrix zurück die um die Y-Achse mit dem Winkel @racket[a] rotiert.
}

@defthing[create-rotation-z-matrix (number -> matrix4x4)]{
 @racket[(create-rotation-z-matrix a)] gibt eine Rotations-Matrix zurück die um die Z-Achse mit dem Winkel @racket[a] rotiert.
}

@defthing[create-lookat-matrix (vec3 vec3 vec3 -> matrix4x4)]{
 @racket[(create-lookat-matrix pos lookat up)] gibt eine Kameramatrix. Ursprungspunkt ist @racket[pos], die Z-Achse zeigt auf @racket[lookat].
}

@defthing[create-projection-matrix (number -> matrix4x4)]{
 @racket[(create-projection-matrix vertical-fov/2)] erzeugt eine Projektions-Matrix. @racket[vertical-fov]/2 gibt den vertikalen Winkel der Ansicht dividiert durch 2 an.
}

@defthing[create-viewport-matrix (natural natural -> matrix4x4)]{
 @racket[(create-viewport-matrix width height)] gibt einen Ausschnitt an.
}

@;-------------------------------------------------------------------------------------
@section[#:tag "3dline"]{3d-Linien}


@defthing[line3d signature]{
 Eine @deftech{3d-Linie} @racket[line3d] ist ein Record, der durch den Aufruf @racket[make-line3d] erstellt wird und eine farbige Linie zwischen zwei Punkten
      im 3-dimensionalen Raum darstellt.
}

@defthing[make-line3d (vec3 vec3 color -> line3d)]{
 @racket[(make-line3d a b col)] erstellt eine 3D-Linie zwischen Punkt @racket[a] und Punkt @racket[b] mit der Farbe @racket[col].
}

@defthing[line3d-a (line3d -> vec3)]{
 extrahiert den Anfangspunkt einer 3D-Linie.}

@defthing[line3d-b (line3d -> vec3)]{
 extrahiert den Endpunkt einer 3D-Linie.}

@defthing[line3d-color (line3d -> color)]{
 extrahiert die Farbe einer 3D-Linie.}

@defthing[create-box (number number number color -> (list-of line3d))]{
 @racket[(create-box width height depth color)] erstellt eine Box am Punkt (0,0,0) in den angebenen Ausmaßen.
}

@defthing[transform-primitive-list ((list-of line3d) matrix4x4 -> (list-of line3d))]{
 @racket[(transform-primitive-list scene transformationr)] wendet @racket[transformation] auf alle Punkte der Linien in @racket[scene] an und gibt 
        diese zurück.
}
