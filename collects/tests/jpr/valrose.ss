;;; teachpack valrose.ss - jpr, Mars 2010

#lang scheme

(require 2htdp/image 2htdp/universe)                     ; images et animations, version 2

(provide 
 (all-from-out 2htdp/image 2htdp/universe)
 show match                                             ; quelques utilitaires manquants
 arbre racine fg fd fdd feuille? operateur?             ; les arbres (2-3) d'expressions arithmetiques
 pile-vide pile-vide? empiler depiler sommet            ; les piles fonctionnelles
 atome? make-neg make-fbf2 connecteur arg1 arg2)        ; les FBF de la Logique d'ordre 0

; petit utilitaire pour avoir les tests dans l'editeur avec echo au toplevel

(define-syntax show
  (syntax-rules ()
    ((show e) (begin (printf "? ~s\n" 'e) (printf "--> ~s\n" e)))))

; le type abstrait "arbre 2-3 d'expression algebrique". Toutes les operations sont O(1)

(define (arbre r Ag . Lfils)    ; au moins un fils !
  (cons r (cons Ag Lfils)))

(define (racine A)
  (if (feuille? A)
      (error (format "pas de racine pour une feuille : ~a" A))
      (first A)))

(define (fg A)
  (if (feuille? A)
      (error (format "pas de fg pour une feuille : ~a" A))
      (second A)))

(define (fd A)
  (if (feuille? A)
      (error (format "pas de fd pour une feuille : ~a" A))
      (third A)))

(define (fdd A)
  (if (or (feuille? A) (empty? (rest (rest (rest A)))))
      (error (format "le fdd n'existe pas : ~a" A))
      (fourth A)))

(define (feuille? obj)
  (or (number? obj) 
      (boolean? obj)
      (and (symbol? obj) (not (operateur? obj)))))

(define (operateur? obj)
  (if (member obj '(+ * - / < > <= >= =)) #t #f))

; le type abstrait "pile fonctionnelle". Toutes les operations sont O(1)

(define (pile-vide) 
  empty)

(define (pile-vide? pile) 
  (empty? pile))

(define (empiler x pile)
  (cons x pile)) 

(define (sommet pile)
  (if (empty? pile)
      (error "Pile vide !")
      (first pile)))

(define (depiler pile)
  (if (empty? pile)
      (error "Pile vide !")
      (rest pile)))

; le type abstrait "fbf en logique d'ordre 0"
; un parametre F denote une fbf

(define (atome? F)            ; le reconnaisseur d'atomes [symboles p, q, r...]
  (symbol? F))

(define (make-neg F)      ; le constructeur de molecule unaire (negation)
  (cond ((atome? F) (list 'non F))
        ((equal? (connecteur F) 'non) (arg1 F))   ; petite simplification au passage...
        (else (list 'non F))))

(define (make-fbf2 r Fg Fd)   ; le constructeur de molecule binaire (et, ou, =>)
  (if (not (member r '(et ou =>)))
      (error "Mauvais connecteur" r)
      (list Fg r Fd)))          ; representation interne infixee

(define (connecteur mol)      ; on suppose que mol est une molecule
  (if (= (length mol) 2)
      (first mol)             ; non
      (second mol)))          ; et, ou, =>

(define (arg1 mol)            ; mol est une molecule
  (if (= (length mol) 2)
      (second mol)
      (first mol)))

(define (arg2 mol)            ; mol est une molecule
  (if (= (length mol) 2)
      (error "Molecule unaire" mol)
      (third mol)))

;(printf "Module valrose : (show expr), (assoc x AL), (sleep n), (current-milliseconds), (gensym symb),
(printf "Module valrose : 
(show expr), (match expr clauses ...),
(arbre r Ag Ad), (racine A), (fg A), (fd A), (fdd A), (feuille? A), (operateur? obj), 
(pile-vide? P), (pile-vide), (empiler x P), (sommet P), (depiler P), 
(atome? F), (make-neg F), (make-fbf2 r Fg Fd), (connecteur mol), (arg1 mol), (arg2 mol)\n")
