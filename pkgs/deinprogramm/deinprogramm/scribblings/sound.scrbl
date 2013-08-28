#lang scribble/doc

@(require scribble/manual scribble/struct "shared.rkt"
          (for-label scheme teachpack/deinprogramm/sound))

@teachpack["sound"]{Abspielen von Audio-Dateien}

Note: This is documentation for the @filepath{sound.rkt} teachpack that goes
with the German textbook
@italic{@link["http://www.deinprogramm.de/dmda/"]{Die Macht der
Abstraktion}}.

Dieses Teachpack definiert eine Prozedur zum Abspielen einer
Audio-Datei.  Diese Prozedur ist je nach Plattform unterschiedlich
realisiert, und funktioniert möglicherweise nicht auf jedem
Rechner.

@declare-exporting[teachpack/deinprogramm/sound]

@defthing[play-sound-file (string -> unspecific)]{ 
Der Aufruf
@racket[(play-sound-file f)] spielt die Audio-Datei mit dem Namen
@racket[f] ab.}

@defthing[background-play-sound-file (string -> unspecific)]{ 
Der Aufruf
@racket[(background-play-sound-file f)] spielt die Audio-Datei mit dem Namen
@racket[f] im Hintergrund ab, also ohne dass das Scheme-Programm anhält.}


