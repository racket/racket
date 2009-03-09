#lang scribble/doc

@(require scribble/manual
          "shared.ss"
	  scribble/struct
          (for-label scheme
                     teachpack/deinprogramm/sound))

@teachpack["sound"]{Abspielen von Audio-Dateien}

Note: This is documentation for the @tt{sound.ss} teachpack that goes
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
@scheme[(play-sound-file f)] spielt die Audio-Datei mit dem Namen
@scheme[f] ab.}

@defthing[background-play-sound-file (string -> unspecific)]{ 
Der Aufruf
@scheme[(background-play-sound-file f)] spielt die Audio-Datei mit dem Namen
@scheme[f] im Hintergrund ab, also ohne dass das Scheme-Programm anhält.}


