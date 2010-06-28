#lang scribble/doc

@(require scribble/manual "shared.ss"
          (for-label scheme
                     teachpack/deinprogramm/image
		     teachpack/deinprogramm/line3d))

@teachpack["line3d"]{3D-Liniengraphik}

Note: This is documentation for the @tt{line3d.ss} teachpack that goes
with the German textbook
@italic{@link["http://www.deinprogramm.de/dmda/"]{Die Macht der
Abstraktion}}.

@declare-exporting[teachpack/deinprogramm/line3d #:use-sources (teachpack/deinprogramm/line3d)]

Dieses teachpack definiert Prozeduren für lineare Algebra und 3D-Rendering:

@;----------------------------------------------------------------------------------
@section[#:tag "rendering"]{Szenen erzeugen}

@declare-exporting[teachpack/deinprogramm/line3d]

@defthing[render-scene (natural natural (list line3d) matrix4x4 -> image)]{
 Der Aufruf @scheme[(render-scene width height scene camera-matrix)]erzeugt die Szene
 in ein Bild mit Breite @scheme[width] und Höhe @scheme[height]. Position,
 Orientierung und Projektion werden durch die @scheme[camera-matrix] festgelegt.
}
 
@defthing[create-camera-matrix (vec3 vec3 number natural natural -> matrix4x4)]{
 Der Aufruf @scheme[(create-camera-matrix position lookat vertical-fov width height)]
 erzeugt eine 4x4 Matrix. Diese kodiert eine Kamera an der Position @scheme[position], die 
 auf die Position @scheme[lookat] schaut.
 @scheme[vertical-fov] bezeichnet das @deftech{vertikale Feld} der Szene.
}

Zum Beispiel:

@schemeblock[
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
 Ein @deftech{3D-Vektor} (Name: @scheme[vec3]) ist ein Record, der durch den Aufruf @scheme[make-vec3] erstellt wird.
}

@defthing[make-vec3 (number number number -> vec3)]{
 @scheme[(make-vec3 x y z)] erstellt einen Vektor (x,y,z).
}

@defthing[add-vec3 (vec3 vec3 -> vec3)]{
 @scheme[(add-vec3 a b)] gibt die Summe von @scheme[a] und @scheme[b] zurück.
}

@defthing[sub-vec3 (vec3 vec3 -> vec3)]{
 @scheme[(sub-vec3 a b)] gibt die Differenz zwischen @scheme[a] und @scheme[b] zurück.
}

@defthing[mult-vec3 (vec3 number -> vec3)]{
 @scheme[(mult-vec3 a s)] gibt den das Produkt von @scheme[a] und @scheme[s] zurück.
}

@defthing[div-vec3 (vec3 number -> vec3)]{
 @scheme[(div-vec3 a s)] gibt den das Produkt von @scheme[a] und dem Kehrwert von @scheme[s] zurück.
}

@defthing[dotproduct-vec3 (vec3 vec3 -> number)]{
 @scheme[(dotproduct-vec3 a b)] gibt das Produkt von @scheme[a] und @scheme[b] zurück.
}

@defthing[normQuad-vec3 (vec3 -> number)]{
 @scheme[(normQuad-vec3 a)] gibt die quadrierte Norm/Länge |@scheme[a]|² eines Vektors @scheme[a] zurück (Quadrat der Euklidischen Norm.)
}

@defthing[norm-vec3 (vec3 -> number)]{
 @scheme[(norm-vec3 a)] gibt die Norm/Länge |@scheme[a]| eines Vektors a zurück (Euklidische Norm.)
}

@defthing[normalize-vec3 (vec3 -> vec3)]{
 @scheme[(normalize-vec3 a)] normalisiert @scheme[a].
}

@defthing[crossproduct-vec3 (vec3 vec3-> vec3)]{
 @scheme[(crossproduct-vec3 a b)] gibt das Kreuzprodukt von @scheme[a]
und @scheme[b] zurück (einen Vektor der senkrecht auf @scheme[a] und @scheme[b] steht).
}

@;-------------------------------------------------------------------------------------
@section[#:tag "4Dvectors"]{4D-Vektoren}

@defthing[vec4 signature]{
 Ein @deftech{4D-Vektor} @scheme[vec4] ist ein 4D-Vektor. Folgende Prozeduren werden bereitgestellt:
}

@defthing[make-vec4 (number number number number -> vec4)]{
 @scheme[(make-vec4 a b c d)] erzeugt einen Vektor aus @scheme[a], @scheme[b], @scheme[c] und @scheme[d].
}

@defthing[add-vec4 (vec4 vec4 -> vec4)]{
@scheme[(add-vec4 a b)]  gibt die Summe von @scheme[a] und @scheme[b] zurück.
}

@defthing[sub-vec4 (vec4 vec4 -> vec4)]{
 @scheme[(sub-vec4 a b)] gibt die Differenz zwischen @scheme[a] und @scheme[b] zurück.
}

@defthing[mult-vec4 (vec4 number -> vec4)]{
 @scheme[(mult-vec4 a s)] gibt den das Produkt von @scheme[a] und @scheme[s] zurück.
}

@defthing[div-vec4 (vec4 number -> vec4)]{
 @scheme[(div-vec4 a s)] gibt den das Produkt von @scheme[a] und dem Kehrwert von @scheme[s] zurück.
}

@defthing[dotproduct-vec4 (vec3 vec4 -> number)]{
 @scheme[(dotproduct-vec4 a b)] gibt die quadrierte Norm/Länge |@scheme[a]|² eines Vektors @scheme[a] zurück (Quadrat der Euklidischen Norm.)
}

@defthing[normQuad-vec4 (vec4 -> number)]{
 @scheme[(normQuad-vec4 a)] gibt die quadrierte Norm/Länge |@scheme[a]|² eines Vektors @scheme[a] zurück (Quadrat der Euklidischen Norm.)
}

@defthing[norm-vec4 (vec4 -> number)]{
 @scheme[(norm-vec4 a)] gibt die Norm/Länge |a| eines Vektors a zurück (Euklidische Norm)
}

@defthing[normalize-vec4 (vec4 -> vec4)]{
 @scheme[(normalize-vec4 a)] normalisiert @scheme[a].
}

@defthing[expand-vec3 (vec3 number -> vec4)]{
 @scheme[(expand-vec3 a s)] gibt den 4D-Vektor mit @scheme[s] als letze Komponente zurück (erweitert @scheme[a] mit @scheme[s]).
}

@;-------------------------------------------------------------------------------------
@section[#:tag "4x4matrix"]{4x4 Matrizen}

@defthing[matrix4x4 signature]{
 Eine @deftech{Matrix} @scheme[matrix4x4] ist ein Record, der durch den Aufruf @scheme[make-matrix4x4] erstellt wird.
}

@defthing[make-matrix4x4 (vec4 vec4 vec4 vec4 -> matrix4x4)]{
 @scheme[(make-matrix4x4 a b c d)] erstellt eine Matrix aus @scheme[a], @scheme[b], @scheme[c] und @scheme[d].
}

@defthing[create-matrix4x4 (vec3 vec3 vec3 vec3 -> matrix4x4)]{
 @scheme[(create-matrix4x4 a b c d)] erweitert jeden Vektor in einen 4D-Vektor und kombiniert diese zu
 einer Matrix @scheme[a], @scheme[b], @scheme[c] und @scheme[d], wobei
 @scheme[a], @scheme[b], @scheme[c] mit 0 und @scheme[d] mit 1 erweitert wird, um eine homogene Matrix zu erzeugen.
}

@defthing[transpose-matrix4x4 (matrix4x4 -> matrix4x)]{
 @scheme[(transpose-matrix4x4 m)] erstellt die transponierte Matrix @scheme[m]^@scheme[T].
}

@defthing[multiply-matrix-vec4 (matrix vec4 -> vec4)]{
 @scheme[(multiply-matrix-vec4 m v)] gibt die Matrix @scheme[m]@scheme[v] zurück. Die @scheme[w]-Komponente ist nicht normalisiert.
}

@defthing[transform-vec3 (matrix4x4 vec3 -> vec3)]{
 @scheme[(transform-vec3 m v)] erweitert @scheme[v] mit 1, multipliziert @scheme[m] mit @scheme[v] und dividiert das Ergebnis mit @scheme[w].
}

@defthing[multiply-matrix (matrix4x4 matrix4x4 -> matrix4x4)]{
 @scheme[(multiply-matrix a b)] gibt die Matrix @scheme[a]*@scheme[b] zurück.
}

@defthing[create-translation-matrix (vec3 -> matrix4x4)]{
 @scheme[(create-translation-matrix v)] gibt die Translations-Matrix zurück.
}

@defthing[create-rotation-x-matrix (number -> matrix4x4)]{
 @scheme[(create-rotation-x-matrix a)] gibt eine Rotations-Matrix zurück die um die X-Achse mit dem Winkel @scheme[a] rotiert.
}

@defthing[create-rotation-y-matrix (number -> matrix4x4)]{
 @scheme[(create-rotation-y-matrix a)] gibt eine Rotations-Matrix zurück die um die Y-Achse mit dem Winkel @scheme[a] rotiert.
}

@defthing[create-rotation-z-matrix (number -> matrix4x4)]{
 @scheme[(create-rotation-z-matrix a)] gibt eine Rotations-Matrix zurück die um die Z-Achse mit dem Winkel @scheme[a] rotiert.
}

@defthing[create-lookat-matrix (vec3 vec3 vec3 -> matrix4x4)]{
 @scheme[(create-lookat-matrix pos lookat up)] gibt eine Kameramatrix. Ursprungspunkt ist @scheme[pos], die Z-Achse zeigt auf @scheme[lookat].
}

@defthing[create-projection-matrix (number -> matrix4x4)]{
 @scheme[(create-projection-matrix vertical-fov/2)] erzeugt eine Projektions-Matrix. @scheme[vertical-fov]/2 gibt den vertikalen Winkel der Ansicht dividiert durch 2 an.
}

@defthing[create-viewport-matrix (natural natural -> matrix4x4)]{
 @scheme[(create-viewport-matrix width height)] gibt einen Ausschnitt an.
}

@;-------------------------------------------------------------------------------------
@section[#:tag "3dline"]{3d-Linien}


@defthing[line3d signature]{
 Eine @deftech{3d-Linie} @scheme[line3d] ist ein Record, der durch den Aufruf @scheme[make-line3d] erstellt wird und eine farbige Linie zwischen zwei Punkten
      im 3-dimensionalen Raum darstellt.
}

@defthing[make-line3d (vec3 vec3 color -> line3d)]{
 @scheme[(make-line3d a b col)] erstellt eine 3D-Linie zwischen Punkt @scheme[a] und Punkt @scheme[b] mit der Farbe @scheme[col].
}

@defthing[line3d-a (line3d -> vec3)]{
 extrahiert den Anfangspunkt einer 3D-Linie.}

@defthing[line3d-b (line3d -> vec3)]{
 extrahiert den Endpunkt einer 3D-Linie.}

@defthing[line3d-color (line3d -> color)]{
 extrahiert die Farbe einer 3D-Linie.}

@defthing[create-box (number number number color -> (list line3d))]{
 @scheme[(create-box width height depth color)] erstellt eine Box am Punkt (0,0,0) in den angebenen Ausmaßen.
}

@defthing[transform-primitive-list ((list line3d) matrix4x4 -> (list line3d))]{
 @scheme[(transform-primitive-list scene transformationr)] wendet @scheme[transformation] auf alle Punkte der Linien in @scheme[scene] an und gibt 
        diese zurück.
}
