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

Die Schablone für eine Prozedur @racket[proc], deren Argument zu einer Sorte gehört,
die @italic{n} Kategorien hat, sieht folgendermaßen aus:

@racketblock[
(: proc (sig -> ...))
(define proc
  (lambda (a)
    (cond
      (#,(elem @racket[test] @subscript{1}) ...)
      ...
      (#,(elem @racket[test] @subscript{n}) ...))))
]
Dabei ist @racket[sig] die Signatur, den die Elemente der Sorte erfüllen müssen. 
Die @elem[@racket[test] @subscript{i}]  müssen Tests sein, welche die einzelnen Kategorien
erkennen.  Sie sollten alle Kategorien abdecken.
Der letzte Zweig kann auch ein @racket[else]-Zweig sein, falls
klar ist, daß @racket[a] zum letzten Fall gehört, wenn alle vorherigen
@elem[@racket[test] @subscript{i}] @racket[#f] ergeben haben.
Anschließend werden die Zweige vervollständigt.

Bei Fallunterscheidungen mit zwei Kategorien kann auch @racket[if]
statt @racket[cond] verwendet werden.

@section{zusammengesetzte Daten}
Wenn bei der Datenanalyse zusammengesetzte Daten vorkommen, stellen
Sie zunächst fest, welche Komponenten zu welchen Sorten gehören.
Schreiben Sie dann eine Datendefinition, die mit folgenden Worten
anfängt:

@racketblock[
(code:comment @#,t{Ein @racket[x] besteht aus / hat:})
(code:comment @#,t{- @racket[#,(elem @racket[Feld] @subscript{1})] @racket[(#,(elem @racket[sig] @subscript{1}))]})
(code:comment @#,t{...})
(code:comment @#,t{- @racket[#,(elem @racket[Feld] @subscript{n})] @racket[(#,(elem @racket[sig] @subscript{n}))]})
]

Dabei ist @racket[x] ein umgangssprachlicher Name für die Sorte
(``Schokokeks''), die @elem[@racket[Feld] @subscript{i}] sind
umgangssprachliche Namen und kurze Beschreibungen der Komponenten 
und die @elem[@racket[sig] @subscript{i}] die dazugehörigen Signaturen.

Übersetzen Sie die Datendefinition in eine Record-Definition, indem Sie
auch Namen für die Record-Signatur @racket[sig], Konstruktor @racket[constr],
Prädikat @racket[pred?] und die Selektoren @elem[@racket[select] @subscript{i}]
wählen:
@racketblock[
(define-record-procedures sig
  constr pred?
  (#,(elem @racket[select] @subscript{1}) ... #,(elem @racket[select] @subscript{n})))
]

Schreiben Sie außerdem eine Signatur für den Konstruktor der
Form:

@racketblock[
(: constr (#,(elem @racket[sig] @subscript{1}) ... #,(elem @racket[sig] @subscript{n}) -> sig))
]

Ggf. schreiben Sie außerdem Signaturen für das Prädikat und die Selektoren:

@racketblock[
(: pred? (any -> boolean))
(: #,(elem @racket[select] @subscript{1}) (sig -> #,(elem @racket[sig] @subscript{1})))
...
(: #,(elem @racket[select] @subscript{n}) (sig -> #,(elem @racket[sig] @subscript{n})))
]

@section{zusammengesetzte Daten als Argumente}
Wenn ein Argument einer Prozedur zusammengesetzt ist, stellen Sie
zunächst fest, von welchen Komponenten des Records das Ergebnis der
Prozeduren abhängt.

Schreiben Sie dann für jede Komponente @racket[(select a)] in die
Schablone, wobei @racket[select] der Selektor der Komponente und @racket[a] der Name
des Parameters der Prozedur ist.

Vervollständigen Sie die Schablone, indem Sie einen Ausdruck
konstruieren, in dem die Selektor-Anwendungen vorkommen.

@section{zusammengesetzte Daten als Ausgabe}
Eine Prozedur, die einen neuen zusammengesetzten Wert zurückgibt,
enthält einen Aufruf des Konstruktors des zugehörigen Record-Typs.

@section{gemischte Daten}
Wenn bei der Datenanalyse gemischte Daten auftauchen, schreiben Sie
eine Datendefinition der Form:

@racketblock[
(code:comment @#,t{Ein @racket[x] ist eins der Folgenden:})
(code:comment @#,t{- @elem[@racket[Sorte] @subscript{1}] (@elem[@racket[sig] @subscript{1}])})
(code:comment @#,t{...})
(code:comment @#,t{- @elem[@racket[Sorte] @subscript{n}] (@elem[@racket[sig] @subscript{n}])})
(code:comment @#,t{Name: @racket[sig]})
]

Dabei sind die @elem[@racket[Sorte] @subscript{i}] umgangssprachliche Namen
für die möglichen Sorten, die ein Wert aus diesen gemischten Daten
annehmen kann.  Die @elem[@racket[sig] @subscript{i}] sind die zu den Sorten
gehörenden Signaturen.  Der Name @racket[sig] ist für die Verwendung als
Signatur.

Aus der Datendefinition entsteht eine Signaturdefinition folgender Form:

@racketblock[
(define sig
  (signature
    (mixed #,(elem @racket[sig] @subscript{1})
           ...
           #,(elem @racket[sig] @subscript{n}))))
]

Wenn die Prädikate für die einzelnen Sorten @elem[@racket[pred?]
@subscript{1}] ... @elem[@racket[pred?] @subscript{n}] heißen, hat die
Schablone für eine Prozedur, die gemischte Daten konsumiert, die
folgende Form:

@racketblock[
(: proc (sig -> ...))

(define proc
  (lambda (a)
    (cond
      ((#,(elem @racket[pred?] @subscript{1}) a) ...)
      ...
      ((#,(elem @racket[pred?] @subscript{n}) a) ...))))
]
 
Die rechten Seiten der Zweige werden dann nach den
Konstruktionsanleitungen der einzelnen Sorten ausgefüllt.

@section{Listen}

Eine Prozedur, die eine Liste konsumiert, hat die folgende
Schablone:
  
@racketblock[
(: proc ((list-of elem) -> ...))

(define proc
  (lambda (lis)
    (cond
      ((empty? lis) ...)
      ((pair? lis)
       ... (first lis)
       ... (proc (rest lis)) ...))))
]

Dabei ist @racket[elem] die Signatur für die Elemente der Liste.  Dies
kann eine Signaturvariable (@racket[%a], @racket[%b], ...) sein, falls
die Prozedur unabhängig von der Signatur der Listenelemente ist.

Füllen Sie in der Schablone zuerst den @racket[empty?]-Zweig aus.
Vervollständigen Sie dann den anderen Zweig unter der Annahme, daß
der rekursive Aufruf @racket[(proc (rest lis))] das gewünschte
Ergebnis für den Rest der Liste liefert.

Beispiel:

@racketblock[
(: list-sum ((list-of number) -> number))

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

@racketblock[
(: proc (natural -> ...))

(define proc
  (lambda (n)
    (if (= n 0)
        ...
        ... (proc (- n 1)) ...)))
]

Füllen Sie in der Schablone zuerst den 0-Zweig aus.  Vervollständigen
Sie dann den anderen Zweig unter der Annahme, daß der rekursive Aufruf
@racket[(proc (- n 1))] das gewünschte Ergebnis für @racket[n]-1
liefert.

Beispiel:

@racketblock[
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

@racketblock[
(: proc ((list-of elem) -> ...))

(define proc
  (lambda (lis)
    (proc-helper lis z)))

(: proc ((list-of elem) sig -> ...))

(define proc-helper
  (lambda (lis acc)
    (cond
      ((empty? lis) acc)
      ((pair? lis)
       (proc-helper (rest lis)
                    (... (first lis) ... acc ...))))))
]

Hier ist @racket[proc] der Name der zu definierenden Prozedur und
@racket[proc-helper] der Name der Hilfsprozedur mit Akkumulator.  Der
Anfangswert für den Akkumulator ist der Wert von @racket[z].  Die Signatur @racket[sig]
ist die Signatur für den Akkumulator.  Der
Ausdruck @racket[(... (first lis) ... acc ...)] 
macht aus dem alten Zwischenergebnis @racket[acc] das neue
Zwischenergebnis.

Beispiel:

@racketblock[
(: invert ((list-of %a) -> (list-of %a)))

(define invert
  (lambda (lis)
    (invert-helper lis empty)))

(: invert ((list-of %a) (list-of %a) -> (list-of %a)))

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

@racketblock[
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

Dabei ist @racket[z] das gewünschte Ergebnis für @racket[n] = 0.  Der
Ausdruck @racket[(... acc ...)] muß den neuen Wert für den
Akkumulator berechnen.

Beispiel:

@racketblock[
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
@racket[define-record-procedures-2] und legen Sie dabei fest, welche
Bestandteile veränderbar sein sollen.  Geben Sie Mutatoren für die
betroffenen Felder an.  Wenn der Selektor für das Feld @racket[select]
heißt, sollte der Mutator i.d.R. @racket[set-select!] heißen.  Die Form
sieht folgendermaßen aus, wobei an der Stelle @racket[k] ein
veränderbares Feld steht:

@racketblock[
(define-record-procedures-2 sig
  constr pred?
  (#,(elem @racket[select] @subscript{1}) ... (#,(elem @racket[s] @subscript{k}) #,(elem @racket[mutate] @subscript{k})) ... #,(elem @racket[s] @subscript{n})))
]

In der Schablone für Prozeduren, die den Zustand eines Record-Arguments
@racket[r] ändern, benutzen Sie den dazugehörigen Mutator
@elem[@racket[mutate] @subscript{k}] Wenn @racket[a] der Ausdruck für
den neuen Wert der Komponente ist, sieht der Aufruf folgendermaßen aus:
@racket[(#,(elem @racket[mutate] @subscript{k}) r a)].

Um mehrere Komponenten in einer Prozedur zu verändern, oder um einen
sinnvollen Rückgabewert nach einer Mutation zu liefern, benutzen Sie
@racket[begin].
