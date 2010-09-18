#lang scribble/doc

@(require scribble/manual
         scribble/basic
         scribble/extract
         scheme/class
         scheme/contract)

@title{Konstruktionsanleitungen 1 bis 10}

This documents the design recipes of the German textbook @italic{Die
Macht der Abstraktion}.

@table-of-contents[]

@section{Konstruktion von Prozeduren}
  Gehen Sie bei der Konstruktion einer Prozedur in folgender Reihenfolge
  vor:
@itemize[
  @item{@bold{Kurzbeschreibung} Schreiben Sie eine einzeilige Kurzbeschreibung.}
  @item{@bold{Datenanalyse} Führen Sie eine Analyse der beteiligten Daten
    durch. Stellen Sie dabei fest, zu welcher Sorte die Daten gehören, ob
    Daten mit Fallunterscheidung vorliegen und ob zusammengesetzte
    oder gemischte Daten vorliegen.}
  @item{@bold{Signatur} (im Buch ``Vertrag'') Wählen Sie einen Namen und schreiben Sie eine Signatur für die Prozedur.}
  @item{@bold{Testfälle}  Schreiben Sie einige Testfälle.}
  @item{@bold{Gerüst} Leiten Sie direkt aus der Signatur das Gerüst der Prozedur her.}
  @item{@bold{Schablone} Leiten Sie aus der Signatur und der Datenanalyse mit
    Hilfe der Konstruktionsanleitungen eine Schablone her.}
  @item{@bold{Rumpf} Vervollständigen Sie den Rumpf der Prozedur.}
  @item{@bold{Test} Vergewissern Sie sich, daß die Tests erfolgreich laufen.}
]

@section{Fallunterscheidung}
Wenn ein Argument einer Prozedur zu einer Fallunterscheidung gehört,
die möglichen Werte also in feste Kategorien sortiert werden können,
steht im Rumpf eine Verzweigung.  Die Anzahl der Zweige entspricht
der Anzahl der Kategorien.

Die Schablone für eine Prozedur @scheme[proc], deren Argument zu einer Sorte gehört,
die @italic{n} Kategorien hat, sieht folgendermaßen aus:

@schemeblock[
(: proc (sig -> ...))
(define proc
  (lambda (a)
    (cond
      (#,(elem (scheme test) (subscript "1")) ...)
      ...
      (#,(elem (scheme test) (subscript "n")) ...))))
]
Dabei ist @scheme[sig] die Signatur, den die Elemente der Sorte erfüllen müssen. 
Die @elem[(scheme test) (subscript "i")]  müssen Tests sein, welche die einzelnen Kategorien
erkennen.  Sie sollten alle Kategorien abdecken.
Der letzte Zweig kann auch ein @scheme[else]-Zweig sein, falls
klar ist, daß @scheme[a] zum letzten Fall gehört, wenn alle vorherigen
@elem[(scheme test) (subscript "i")] @scheme[#f] ergeben haben.
Anschließend werden die Zweige vervollständigt.

Bei Fallunterscheidungen mit zwei Kategorien kann auch @scheme[if]
statt @scheme[cond] verwendet werden.

@section{zusammengesetzte Daten}
Wenn bei der Datenanalyse zusammengesetzte Daten vorkommen, stellen
Sie zunächst fest, welche Komponenten zu welchen Sorten gehören.
Schreiben Sie dann eine Datendefinition, die mit folgenden Worten
anfängt:

@schemeblock[
(code:comment @#,t{Ein @scheme[x] besteht aus / hat:})
(code:comment @#,t{- @scheme[#,(elem (scheme Feld) (subscript "1"))] @scheme[(#,(elem (scheme sig) (subscript "1")))]})
(code:comment @#,t{...})
(code:comment @#,t{- @scheme[#,(elem (scheme Feld) (subscript "n"))] @scheme[(#,(elem (scheme sig) (subscript "n")))]})
]

Dabei ist @scheme[x] ein umgangssprachlicher Name für die Sorte
(``Schokokeks''), die @elem[(scheme Feld) (subscript "i")] sind
umgangssprachliche Namen und kurze Beschreibungen der Komponenten 
und die @elem[(scheme sig) (subscript "i")] die dazugehörigen Signaturen.

Übersetzen Sie die Datendefinition in eine Record-Definition, indem Sie
auch Namen für die Record-Signatur @scheme[sig], Konstruktor @scheme[constr],
Prädikat @scheme[pred?] und die Selektoren @elem[(scheme select) (subscript "i")]
wählen:
@schemeblock[
(define-record-procedures sig
  constr pred?
  (#,(elem (scheme select) (subscript "1")) ... #,(elem (scheme select) (subscript "n"))))
]

Schreiben Sie außerdem eine Signatur für den Konstruktor der
Form:

@schemeblock[
(: constr (#,(elem (scheme sig) (subscript "1")) ... #,(elem (scheme sig) (subscript "n")) -> sig))
]

Ggf. schreiben Sie außerdem Signaturen für das Prädikat und die Selektoren:

@schemeblock[
(: pred? (%a -> boolean))
(: #,(elem (scheme select) (subscript "1")) (sig -> #,(elem (scheme sig) (subscript "1"))))
...
(: #,(elem (scheme select) (subscript "n")) (sig -> #,(elem (scheme sig) (subscript "n"))))
]

@section{zusammengesetzte Daten als Argumente}
Wenn ein Argument einer Prozedur zusammengesetzt ist, stellen Sie
zunächst fest, von welchen Komponenten des Records das Ergebnis der
Prozeduren abhängt.

Schreiben Sie dann für jede Komponente @scheme[(select a)] in die
Schablone, wobei @scheme[select] der Selektor der Komponente und @scheme[a] der Name
des Parameters der Prozedur ist.

Vervollständigen Sie die Schablone, indem Sie einen Ausdruck
konstruieren, in dem die Selektor-Anwendungen vorkommen.

@section{zusammengesetzte Daten als Ausgabe}
Eine Prozedur, die einen neuen zusammengesetzten Wert zurückgibt,
enthält einen Aufruf des Konstruktors des zugehörigen Record-Typs.

@section{gemischte Daten}
Wenn bei der Datenanalyse gemischte Daten auftauchen, schreiben Sie
eine Datendefinition der Form:

@schemeblock[
(code:comment @#,t{Ein @scheme[x] ist eins der Folgenden:})
(code:comment @#,t{- @elem[(scheme Sorte) (subscript "1")] (@elem[(scheme sig) (subscript "1")])})
(code:comment @#,t{...})
(code:comment @#,t{- @elem[(scheme Sorte) (subscript "n")] (@elem[(scheme sig) (subscript "n")])})
(code:comment @#,t{Name: @scheme[sig]})
]

Dabei sind die @elem[(scheme Sorte) (subscript "i")] umgangssprachliche Namen
für die möglichen Sorten, die ein Wert aus diesen gemischten Daten
annehmen kann.  Die @elem[(scheme sig) (subscript "i")] sind die zu den Sorten
gehörenden Signaturen.  Der Name @scheme[sig] ist für die Verwendung als
Signatur.

Aus der Datendefinition entsteht eine Signaturdefinition folgender Form:

@schemeblock[
(define sig
  (signature
    (mixed #,(elem (scheme sig) (subscript "1"))
           ...
           #,(elem (scheme sig) (subscript "n")))))
]

Wenn die Prädikate für die einzelnen Sorten @elem[(scheme pred?)
(subscript "1")] ... @elem[(scheme pred?) (subscript "n")] heißen, hat die
Schablone für eine Prozedur, die gemischte Daten konsumiert, die
folgende Form:

@schemeblock[
(: proc (sig -> ...))

(define proc
  (lambda (a)
    (cond
      ((#,(elem (scheme pred?) (subscript "1")) a) ...)
      ...
      ((#,(elem (scheme pred?) (subscript "n")) a) ...))))
]
 
Die rechten Seiten der Zweige werden dann nach den
Konstruktionsanleitungen der einzelnen Sorten ausgefüllt.

@section{Listen}

Eine Prozedur, die eine Liste konsumiert, hat die folgende
Schablone:
  
@schemeblock[
(: proc ((list elem) -> ...))

(define proc
  (lambda (lis)
    (cond
      ((empty? lis) ...)
      ((pair? lis)
       ... (first lis)
       ... (proc (rest lis)) ...))))
]

Dabei ist @scheme[elem] die Signatur für die Elemente der Liste.  Dies
kann eine Signaturvariable (@scheme[%a], @scheme[%b], ...) sein, falls
die Prozedur unabhängig von der Signatur der Listenelemente ist.

Füllen Sie in der Schablone zuerst den @scheme[empty?]-Zweig aus.
Vervollständigen Sie dann den anderen Zweig unter der Annahme, daß
der rekursive Aufruf @scheme[(proc (rest lis))] das gewünschte
Ergebnis für den Rest der Liste liefert.

Beispiel:

@schemeblock[
(: list-sum ((list number) -> number))

(define list-sum
  (lambda (lis)
    (cond
      ((empty? lis) 0)
      ((pair? lis)
       (+ (first lis)
          (list-sum (rest lis)))))))
]

@section{natürliche Zahlen}

Eine Prozedur, die natürliche Zahlen konsumiert, hat die folgende
Schablone:

@schemeblock[
(: proc (natural -> ...))

(define proc
  (lambda (n)
    (if (= n 0)
        ...
        ... (proc (- n 1)) ...)))
]

Füllen Sie in der Schablone zuerst den 0-Zweig aus.  Vervollständigen
Sie dann den anderen Zweig unter der Annahme, daß der rekursive Aufruf
@scheme[(proc (- n 1))] das gewünschte Ergebnis für @scheme[n]-1
liefert.

Beispiel:

@schemeblock[
(: factorial (natural -> natural))

(define factorial
  (lambda (n)
    (if (= n 0)
        1
        (* n (factorial (- n 1))))))
]

@section{Prozeduren mit Akkumulatoren}

Eine Prozedur mit Akkumulator, die Listen konsumiert, hat die
folgende Schablone:

@schemeblock[
(: proc ((list elem) -> ...))

(define proc
  (lambda (lis)
    (proc-helper lis z)))

(: proc ((list elem) sig -> ...))

(define proc-helper
  (lambda (lis acc)
    (cond
      ((empty? lis) acc)
      ((pair? lis)
       (proc-helper (rest lis)
                    (... (first lis) ... acc ...))))))
]

Hier ist @scheme[proc] der Name der zu definierenden Prozedur und
@scheme[proc-helper] der Name der Hilfsprozedur mit Akkumulator.  Der
Anfangswert für den Akkumulator ist der Wert von @scheme[z].  Die Signatur @scheme[sig]
ist die Signatur für den Akkumulator.  Der
Ausdruck @scheme[(... (first lis) ... acc ...)] 
macht aus dem alten Zwischenergebnis @scheme[acc] das neue
Zwischenergebnis.

Beispiel:

@schemeblock[
(: invert ((list %a) -> (list %a)))

(define invert
  (lambda (lis)
    (invert-helper lis empty)))

(: invert ((list %a) (list %a) -> (list %a)))

(define invert-helper
  (lambda (lis acc)
    (cond
      ((empty? lis) acc)
      ((pair? lis)
       (invert-helper (rest lis)
                      (make-pair (first lis) acc))))))
]

Eine Prozedur mit Akkumulator, die natürliche Zahlen konsumiert, hat die
folgende Schablone:

@schemeblock[
(: proc (natural -> ...))

(define proc
  (lambda (n)
    (proc-helper n z)))

(define proc-helper
  (lambda (n acc)
    (if (= n 0)
        acc
        (proc-helper (- n 1) (... acc ...)))))
]

Dabei ist @scheme[z] das gewünschte Ergebnis für @scheme[n] = 0.  Der
Ausdruck @scheme[(... acc ...)] muß den neuen Wert für den
Akkumulator berechnen.

Beispiel:

@schemeblock[
(: ! (natural -> natural))

(define !
  (lambda (n)
    (!-helper n 1)))

(define !-helper
  (lambda (n acc)
    (if (= n 0)
        acc
        (!-helper (- n 1) (* n acc)))))
]

@section{gekapselter Zustand}
Falls ein Wert Zustand enthalten soll, schreiben Sie eine
Datendefinition wie bei zusammengesetzten Daten.

Schreiben Sie dann eine Record-Definition mit
@scheme[define-record-procedures-2] und legen Sie dabei fest, welche
Bestandteile veränderbar sein sollen.  Geben Sie Mutatoren für die
betroffenen Felder an.  Wenn der Selektor für das Feld @scheme[select]
heißt, sollte der Mutator i.d.R. @scheme[set-select!] heißen.  Die Form
sieht folgendermaßen aus, wobei an der Stelle @scheme[k] ein
veränderbares Feld steht:

@schemeblock[
(define-record-procedures-2 sig
  constr pred?
  (#,(elem (scheme select) (subscript "1")) ... (#,(elem (scheme s) (subscript "k")) #,(elem (scheme mutate) (subscript "k"))) ... #,(elem (scheme s) (subscript "n"))))
]

In der Schablone für Prozeduren, die den Zustand eines
Record-Arguments @scheme[r] ändern, benutzen Sie den dazugehörigen Mutator
@elem[(scheme mutate) (subscript "k")]  Wenn @scheme[a] der Ausdruck für den neuen Wert der Komponente ist,
sieht der Aufruf folgendermaßen aus: @scheme[(#,(elem (scheme mutate) (subscript "k")) r a)].
  
Um mehrere Komponenten in einer Prozedur zu verändern, oder um einen
sinnvollen Rückgabewert nach einer Mutation zu liefern, benutzen Sie
@scheme[begin].
