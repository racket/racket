#lang scheme
(require redex)
(require racket/set)
(provide red detect-hole detect-hole2 hole-table Get-Free-Name-Patterns 
         Build-Term-Let
         Build-Cond
         Cond-List
         simplify
         simple-swap)

(define hole-table (make-hash))

(define the-hole (term hole))

(define new-var-list '())

(define-language L
  [e (cond [e e] ...)
     (let ((x e) ...) e)
     ;(term-let ((x e) ...) e)
     (e e ...)
     m
     b]
  [v (cond [v v] ...)
     (let ((x v) ...) v)
     ;(term-let ((x v) ...) v)
     (v v ...)
     b]
  [b empty
     
     plug
     reverse
     append
     list
     andmap
     no-match
     cons?
     number?
     natural?
     integer?
     real?
     string?
     symbol?
     context-match
     or
     set-union
     set
     set-from-list
     set-map
     curry
     pair-path-append
     path-append
     ; we don't have these functions yet
     variable-except?
     variable-prefix?
     ;
     ()
     begin
     ∪
     singleton
     eqv?
     set-empty?
     ∅
     else
     car
     cdr
     number
     string
     bool
     cons
     x
     'variable
     'any
     variable-not-otherwise-mentioned
     not
     =
     '()
     ; ambiguous
     term-let
     ]
  [bool #t
        #f]
  [Context 
   (cond [v v] ...
         [v Context]
         [v e] ...)
   (cond [v v] ...
         [Context e]
         [e e] ...)
   (let ((x v) ...) v ... Context e ...)
   ;(term-let ((x v) ...) v ... Context e ...)
   (v ... Context e ...)
   hole]
  [m (matrix (x ...) (row ...) (pvar ...) (pvar ...) natural bool)]
  [row ((p ... -> r) eqs ...)]
  [eqs (pvar bool eq ...)]
  [elip ,'...]
  [eq id
      (= pvar)
      (car eq)
      (cdr eq)
      (cons eq eq)
      (plug eq eq)
      lit-hole]
  (p (or p p)
     lit-hole
     (lit-in-hole p_1 p_2)
     (lit-hide-hole p)
     (lit-name variable p)
     (lit-side-condition p any)
     (nt id)
     rep
     scw)
  (s lit-number
     lit-natural
     lit-integer
     lit-real
     lit-string
     lit-variable
     (lit-variable-except id ...)
     (lit-variable-prefix id)
     )
  (scw s
       cw)
  (cw c
      wildcard)
  (wildcard wc)
  (c number
     string
     'variable
     '()
     cp
     #t
     #f
     ; no longer thought of as a constructor, at least for now...
     ;(nt id)
     ;lit-variable-not-otherwise-mentioned
     ;context-match
     )
  (cp (cons p p))
  (rep (repeat p rep)
       (repeat p cp)
       (repeat p '()))
  ;(pvar (variable-prefix p))
  (id variable)
  ((match-repeat z) variable-not-otherwise-mentioned)
  (pvar variable-not-otherwise-mentioned
        (pvar pvar)
        (pvar elip))
  (x #;(side-condition (name x variable-not-otherwise-mentioned)
                       (not (regexp-match #rx"^[pm]" (format "~a" (term x)))))
     (car x)
     (cdr x)
     (cons x x)
     pvar)
  (r any
     (car any)
     (cdr any))
  ((op replace) any))

(define red
  (reduction-relation
   L
   
   ; Top-Row-All-Wildcard
   ;   If the top row is all wildcard patterns, we're not inside a repeat, or in the non-terminal set mode, 
   ;   replace the top row with a singleton of it's rhs, and union this with the matrix built from the remaining rows.
   
   (--> (in-hole Context (matrix (x_1 ...) 
                                 (((wildcard ... -> r_1) eqs_1 ...) 
                                  ((p_1 ... -> r_2) eqs_2 ...) 
                                  ...)
                                 (pvar ...)
                                 (pvar_3 ...)
                                 0
                                 bool))
        (in-hole Context
                 (begin (Build-Term-Let (eqs_1 ...) ;(set! results (cons 
                                        r_1 
                                        ;results))
                                        )
                        (matrix (x_1 ...) 
                                (((p_1 ... -> r_2) eqs_2 ...) 
                                 ...)
                                (pvar ...)
                                (pvar_3 ...)
                                0
                                bool)
                        )
                 )
        (side-condition (eqv? (length (term (x_1 ...))) (length (term (wildcard ...)))))
        Top-Row-All-Wildcard)
   
   ; Top-Row-All-Wildcard in repeat
   ;   This is to support our hack of using the repeat matching function as a rhs to a matrix when matching repeats. 
   ;   When we see a one rowed matrix, where the top row is all wildcards, replace with the rhs.
   
   (--> (in-hole Context (matrix (x_1 ...) 
                                 (((wildcard ... -> r_1) eqs_1 ...))
                                 (pvar ...)
                                 (pvar_3 ...)
                                 natural
                                 bool))
        (in-hole Context r_1)
        (side-condition (eqv? (length (term (x_1 ...))) (length (term (wildcard ...)))))
        (side-condition (> (term natural) 0))
        Top-Row-All-Wildcard-in-repeat)
   
   ; Empty-Rows
   ;   Transform matrices with no rows into ∅.
   
   (--> (in-hole Context (matrix (x ...)
                                 ()
                                 (pvar ...)
                                 (pvar_3 ...)
                                 natural
                                 bool))
        (in-hole Context ∅)
        Empty-Rows)
   
   ; Empty Variables
   ;   Transform matrices with no input variables into ∅.
   (--> (in-hole Context (matrix ()
                                 (((p_1 p_2 ... -> r_1) eqs_1 ...)
                                  ((p_3 p_4 ... -> r_2) eqs_2 ...)
                                  ...)
                                 (pvar ...)
                                 (pvar_3 ...)
                                 natural
                                 bool))
        (in-hole Context ∅)
        Empty-Variables)
   
   ; Drop-Front-Column-All-Wildcard
   
   (--> (in-hole Context
                 (matrix (x_1 x_2 ...)
                         (((wc p_2 ... -> r_1) eqs_1 ...)
                          ((wc p_4 ... -> r_2) eqs_2 ...)
                          ...)
                         (pvar ...)
                         (pvar_3 ...)
                         natural
                         bool))
        (in-hole Context
                 (matrix (x_2 ...)
                         (((p_2 ... -> r_1) eqs_1 ...)
                          ((p_4 ... -> r_2) eqs_2 ...)
                          ...)
                         (pvar ...)
                         (pvar_3 ...)
                         natural
                         bool))
        (side-condition (ormap not-a-wildcard? (term (p_2 ...))))
        Drop-Front-Column-All-Wildcard)
   
   #;(--> (in-hole Context
                   (matrix (x_1 x_2 ... x_3 x_4 ...)
                           (
                            ((p_1 p_2 ... p_3 p_4 ... -> r_1) eqs_1 ...)
                            ...)
                           (pvar ...)
                           (pvar_3 ...)
                           natural
                           bool))
          (in-hole Context
                   (matrix (x_3 x_2 ... x_1 x_4 ...)
                           (
                            ((p_3 p_2 ... p_1 p_4 ... -> r_1) eqs_1 ...)
                            ...)
                           (pvar ...)
                           (pvar_3 ...)
                           natural
                           bool))
          
          (side-condition
           (apply = 
                  (append (list (length (term (x_2 ...)))
                                (length (term (x_2 ...))))
                          (map length (term ((p_2 ...) ...))))))
          (side-condition
           (apply = 
                  (append (list (length (term (x_4 ...)))
                                (length (term (x_4 ...))))
                          (map length (term ((p_4 ...) ...))))))
          (side-condition (> (length (remove-duplicates (term (simplify (p_1 ...))))) (length (remove-duplicates (term (simplify (p_3 ...)))))))
          simple-swap)
   
   ;   ; Front-Column-All-Wildcard (now force a swap with the backmost non-wildcard column to decrease branching)
   ;   ;   When the front column of a matrix is all wildcard patterns, swap it with another column with at least one non-wildcard pattern.
   ;   
   ;   (--> (in-hole Context
   ;                 (matrix (x_1 x_2 ... x_3 x_4 ...) 
   ;                         ((wc p_2 ... p_3 wildcard_1 ... -> r_1)
   ;                          (wc p_5 ... p_6 wildcard_2 ... -> r_2) 
   ;                          ...)
   ;                         (pvar ...)
   ;                         (pvar_3 ...)
   ;                         natural
   ;                         bool))
   ;        (in-hole Context 
   ;                 (matrix (x_3 x_2 ... x_1 x_4 ...) 
   ;                         ((p_3 p_2 ... wc wildcard_1 ... -> r_1)
   ;                          (p_6 p_5 ... wc wildcard_2 ... -> r_2) 
   ;                          ...)
   ;                         (pvar ...)
   ;                         (pvar_3 ...)
   ;                         natural
   ;                         bool))
   ;        (side-condition
   ;         (eqv? (length (term (x_2 ...)))
   ;               (length (term (p_2 ...)))))
   ;        (side-condition
   ;         (eqv? (length (term (x_4 ...)))
   ;               (length (term (wildcard_1 ...)))))
   ;        (side-condition
   ;         (apply = 
   ;                (append (list (length (term (x_2 ...)))
   ;                              (length (term (x_2 ...))))
   ;                        (map length (term ((p_5 ...) ...))))))
   ;        (side-condition
   ;         (apply = 
   ;                (append (list (length (term (x_4 ...)))
   ;                              (length (term (x_4 ...))))
   ;                        (map length (term ((wildcard_2 ...) ...))))))
   ;        #;(side-condition (not-a-wildcard? (term p_3)))
   ;        (side-condition (or (not-a-wildcard? (term p_3)) (ormap not-a-wildcard? (term (p_6 ...)))))
   ;        Front-Column-All-Wildcard)
   
   ; Unroll-Or
   ;   If the first pattern of a row in the matrix is an or pattern, duplicate the row for each branch of the or.
   
   (--> (in-hole Context
                 (matrix (x_1 ...) 
                         (((p_1 p_2 ... -> r_1) eqs_1 ...)
                          ...
                          (((or p_3 p_4) p_5 ... -> r_2) eqs_2 ...)
                          ((p_6 p_7 ... -> r_3) eqs_3 ...)
                          ...)
                         (pvar ...)
                         (pvar_3 ...)
                         natural
                         bool))
        (in-hole Context 
                 (matrix (x_1 ...) 
                         (((p_1 p_2 ... -> r_1) eqs_1 ...)
                          ...
                          ((p_3 p_5 ... -> r_2) eqs_2 ...)
                          ((p_4 p_5 ... -> r_2) eqs_2 ...)
                          ((p_6 p_7 ... -> r_3) eqs_3 ...)
                          ...)
                         (pvar ...)
                         (pvar_3 ...)
                         natural
                         bool))
        (side-condition (andmap not-an-or? (term (p_1 ...))))
        Unroll-Or)
   
   ; binding-name
   ;   If the first pattern of a row in the matrix has is a name pattern where the id is not yet marked as bound, 
   ;   and we're not inside a repeat, then mark the id as bound, replace the name with the pattern contained in it, 
   ;   wrap the row in a let which binds the id to the current input variable, and union the result of the let with the matrix formed by the remaining rows.
   
   (--> (in-hole Context 
                 (matrix (x_1 x_2 ...) 
                         (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                          ...
                          (((lit-name pvar_1 p_3) p_5 ... -> r_2) (pvar_4 bool_4 eq_4 ...) ... (pvar_1 bool_1 eq_1 ...) (pvar_5 bool_5 eq_5 ...) ...)
                          ((p_6 p_7 ... -> r_3) eqs_3 ...)
                          ...)
                         (pvar_2 ...)
                         (pvar_3 ...)
                         0
                         bool))
        (in-hole Context
                 (matrix (x_1 x_2 ...) 
                         (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                          ...
                          ((p_3 p_5 ... -> r_2) (pvar_4 bool_4 eq_4 ...) ... (pvar_1 bool_1 ,@(remove-duplicates (term (x_1 eq_1 ...)))) (pvar_5 bool_5 eq_5 ...) ...)
                          ((p_6 p_7 ... -> r_3) eqs_3 ...)
                          ...)
                         (pvar_2 ...)
                         (pvar_3 ...)
                         0
                         bool))
        binding-name)
   
   ; binding-name-repeat
   ;   If inside a repeat, there is a 1 row matrix with a name pattern in its front column, where the variable is not marked as bound in either place in the matrix, 
   ;   replace the name with the pattern it matches, wrap the matrix inside a let which binds the id to the current input variable, 
   ;   and mark the variable as bound in only the list of repeat variables.
   
   (--> (in-hole Context 
                 (matrix (x_1 x_2 ...) 
                         ((((lit-name pvar_1 p_3) p_5 ... -> r_2) eqs_1 ...))
                         (pvar_2 ...)
                         (pvar_3 ...)
                         natural
                         bool))
        (in-hole Context 
                 (let ((pvar_1 x_1))
                   (matrix (x_1 x_2 ...)
                           (((p_3 p_5 ... -> r_2) eqs_1 ...))
                           (pvar_2 ...)
                           (pvar_1 pvar_3 ...)
                           natural
                           bool)))
        ;(side-condition (not (member (term pvar_1) (term (pvar_2 ...)))))
        (side-condition (not (member (term pvar_1) (map (λ (x) (term (Get-Pvar ,x))) (append (term (Binding-Eqs (eqs_1 ...))) (append (term (True-Eqs (eqs_1 ...) ())) (term (pvar_3 ...))))  ))))
        (side-condition (> (term natural) 0))
        binding-name-repeat)
   
   ; bound-name-from-outside-repeat
   ;     If a one rowed matrix inside a repeat begins with a name pattern where the identifier is marked as bound outside the repeat, 
   ;   wrap the matrix in an equality test between the car of the already-bound value, and the current input variable, 
   ;   then continue matching starting with the p from inside the name pattern. 
   ;     We take the car because we know that all variables bound inside repeats will bind to lists, 
   ;   so an existing binding is equal one inside a repeat if each element of the bindings is equal.
   
   (--> (in-hole Context 
                 (matrix (x_1 x_2 ...) 
                         ((((lit-name pvar_1 p_3) p_5 ... -> r_2) eqs_1 ...))
                         (pvar_2 ...)
                         (pvar_3 ...)
                         natural
                         bool))
        (in-hole Context 
                 (cond [(equal? (car ,(term pvar_1)) x_1)
                        (matrix (x_1 x_2 ...)
                                (((p_3 p_5 ... -> r_2) eqs_1 ...))
                                (pvar_2 ...)
                                (pvar_3 ...)
                                natural
                                bool)]
                       #;[else ∅]
                       ))
        (side-condition (> (term natural) 0))
        (side-condition (member (term pvar_1) (map (λ (x) (term (Get-Pvar ,x))) (append (term (Binding-Eqs (eqs_1 ...))) (term (True-Eqs (eqs_1 ...) ())) ))))
        bound-name-from-outside-repeat)
   
   ; bound-name-from-inside-repeat
   ;   If a one rowed matrix inside a repeat begins with a name pattern where the identifier is marked as bound inside the repeat, 
   ;   wrap the matrix in an equality test between the already-bound value and the current input variable, then continue matching starting with the p from inside the name pattern.
   
   (--> (in-hole Context 
                 (matrix (x_1 x_2 ...) 
                         ((((lit-name pvar_1 p_3) p_5 ... -> r_2) eqs_1 ...))
                         (pvar_2 ...)
                         (pvar_3 ...)
                         natural
                         bool))
        (in-hole Context 
                 (cond [(equal? pvar_1 x_1)
                        (matrix (x_1 x_2 ...)
                                (((p_3 p_5 ... -> r_2) eqs_1 ...))
                                (pvar_2 ...)
                                (pvar_3 ...)
                                natural
                                bool)
                        ]
                       #;[else ∅]
                       ))
        (side-condition (> (term natural) 0))
        (side-condition (member (term pvar_1) (map (λ (x) (term (Get-Pvar ,x))) (term (pvar_3 ...)))))
        bound-name-from-inside-repeat)
   
   ; non-terminal 
   ;   If a row in a matrix starts with a non-terminal which does not potentially contain a hole, wrap the row with a conditional 
   ;   which calls the function wicth matches the non-terminal using boolean operations instead of sets where possible. Union this with the matrix formed by the remaining rows.
   
   (--> (in-hole Context
                 (matrix (x_1 x_2 ...)
                         (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                          ...
                          (((nt id) p_5 ... -> r_2) eqs_2 ...)
                          ((p_6 p_7 ... -> r_3) eqs_3 ...)
                          ...)
                         (pvar_2 ...)
                         (pvar_3 ...)
                         natural
                         bool))
        (in-hole Context
                 (begin 
                   (cond ((,(string->symbol (format "~s~s" (term id) '-bool)) x_1)
                          (matrix (x_2 ...)
                                  (((p_* ... -> r_*) eqs_* ...) ...)
                                  (pvar_2 ...)
                                  (pvar_3 ...)
                                  natural
                                  bool)
                          )
                         #;(else ∅)
                         )
                   (matrix (x_1 x_2 ...)
                           (((p_** ... -> r_**) eqs_** ...) ...)
                           (pvar_2 ...)
                           (pvar_3 ...)
                           natural
                           bool)))
        
        
        ;(side-condition (eqv? (hash-ref hole-table (term id)) 0))
        (where (((p_* ... -> r_*) eqs_* ...) ...) (drop-first-p (same-starting-pattern 
                                                                 (nt id)
                                                                 (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                                                                  ...
                                                                  (((nt id) p_5 ... -> r_2) eqs_2 ...)
                                                                  ((p_6 p_7 ... -> r_3) eqs_3 ...)
                                                                  ...))))
        (where (((p_** ... -> r_**) eqs_** ...) ...) (diff-starting-pattern (nt id) (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                                                                                     ...
                                                                                     ((p_6 p_7 ... -> r_3) eqs_3 ...)
                                                                                     ...)))
        non-terminal)
   
   ; non-terminal (set)
   ;   If the flag for the special "non-terminal returns a set" mode is true, and a row in the matrix begins with a non-terminal which contains a hole, 
   ;   wrap the row in a let, which binds the result of calling the "set" function which matches the non-terminal to a special value called nt-val. 
   ;   Inside the let but outside the matrix is a conditional to check if nt-val is not the empty set. The rest of the patterns in the row remain the same, 
   ;   but the right hand side is adjusted to add in the results of nt-val. Union the let with the matrix formed by the other rows.
   
   #;(--> (in-hole Context
                   (matrix (x_1 x_2 ...)
                           (((p_1 p_2 ... -> r_1) eqs_1 ...)
                            ...
                            (((nt id) p_5 ... -> r_2) eqs_2 ...)
                            ((p_6 p_7 ... -> r_3) eqs_3 ...)
                            ...)
                           (pvar_2 ...)
                           (pvar_3 ...)
                           natural
                           #t))
          (in-hole Context 
                   (begin 
                     (let ((nt-val (,(string->symbol (format "~s~s" (term id) '-list)) x_1)))
                       (cond ((not (set-empty? nt-val))
                              (matrix (x_2 ...)
                                      (((p_* ... -> r_*) eqs_* ...) ...)#;((p_5 ... -> (set-union (set-from-list (set-map nt-val (curry pair-path-append (quote x_1)))) r_2)))
                                      (pvar_2 ...)
                                      (pvar_3 ...)
                                      natural
                                      #t))
                             (else ∅)))
                     (matrix (x_1 x_2 ...)
                             (((p_** ... -> r_**) eqs_** ...) ...)#;((p_1 p_2 ... -> r_1) 
                                                                     ...
                                                                     (p_6 p_7 ... -> r_3) 
                                                                     ...)
                             (pvar_2 ...)
                             (pvar_3 ...)
                             natural
                             #t)))
          (side-condition (andmap not-a-nt? (term (p_1 ...))))
          ;(side-condition (> (hash-ref hole-table (term id)) 0))
          (fresh nt-val)
          (where (((p_* ... -> r_*) eqs_* ...) ...) (drop-first-p (same-starting-pattern 
                                                                   (nt id)
                                                                   (((p_1 p_2 ... -> r_1 #;(set-union (set-from-list (set-map nt-val (curry pair-path-append (quote x_1)))) r_1)) eqs_1 ...)
                                                                    ...
                                                                    (((nt id) p_5 ... -> r_2 #;(set-union (set-from-list (set-map nt-val (curry pair-path-append (quote x_1)))) r_2)) eqs_2 ...)
                                                                    ((p_6 p_7 ... -> r_3 #;(set-union (set-from-list (set-map nt-val (curry pair-path-append (quote x_1)))) r_3)) eqs_3 ...)
                                                                    ...))))
          (where (((p_** ... -> r_**) eqs_** ...) ...) (diff-starting-pattern (nt id) (((p_1 p_2 ... -> r_1) eqs_1 ...)
                                                                                       ...
                                                                                       ((p_6 p_7 ... -> r_3) eqs_3 ...)
                                                                                       ...)))
          non-terminal-set)
   
   ; Constructor
   ; If the front column is all either "constructor" patterns, or wildcard patterns, with at least one non-wildcard, then specialize the matrix based on the constructors.
   
   (--> (in-hole Context
                 (matrix (x_1 x_2 ...)
                         (((wildcard_1 p_2 ... -> r_1) eqs_1 ...)
                          ...
                          ((c_1 p_3 ... -> r_2) eqs_2 ...)
                          ((cw_2 p_4 ... -> r_3) eqs_3 ...)
                          ...)
                         (pvar_1 ...)
                         (pvar_3 ...)
                         natural
                         bool))
        (in-hole Context 
                 (Build-Cond ((Cond-List (simplify (c_1 cw_2 ...))) 
                              (matrix (x_1 x_2 ...)
                                      (((wildcard_1 p_2 ... -> r_1) eqs_1 ...)
                                       ...
                                       ((c_1 p_3 ... -> r_2) eqs_2 ...)
                                       ((cw_2 p_4 ... -> r_3) eqs_3 ...)
                                       ...)
                                      (pvar_1 ...)
                                      (pvar_3 ...)
                                      natural
                                      bool) carx cdrx)))
        (fresh carx)
        (fresh cdrx)
        Constructor)
   
   ; hole
   ;   If a row in the matrix contains a lit-hole in its first column, and the flag for non-terminal-set is #f, 
   ;   then separate the row from the rest of the matrix, replacing hole with "context-match," which will subsequently be transformed 
   ;   into a conditional based on whether or not there is a current context. Union this with the matrix formed by the remaining rows.
   
   (--> (in-hole Context
                 (matrix (x_1 x_2 ...)
                         (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                          ...
                          ((lit-hole p_5 ... -> r_2) eqs_2 ...)
                          ((p_6 p_7 ... -> r_3) eqs_3 ...)
                          ...)
                         (pvar_1 ...)
                         (pvar_3 ...)
                         natural
                         bool))
        (in-hole Context
                 (begin 
                   (cond [(or (context-match) (eqv? x_1 the-hole))
                          (matrix (x_2 ...)
                                  (((p_* ... -> r_*) eqs_* ...) ...)
                                  (pvar_1 ...)
                                  (pvar_3 ...)
                                  natural
                                  bool)]
                         #;(else ∅)
                         )
                   (matrix (x_1 x_2 ...)
                           (((p_** ... -> r_**) eqs_** ...) ...)
                           (pvar_1 ...)
                           (pvar_3 ...)
                           natural
                           bool)
                   ))
        
        (where (((p_* ... -> r_*) eqs_* ...) ...) (drop-first-p (same-starting-pattern 
                                                                 lit-hole
                                                                 (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                                                                  ...
                                                                  ((lit-hole p_5 ... -> r_2) eqs_2 ...)
                                                                  ((p_6 p_7 ... -> r_3) eqs_3 ...)
                                                                  ...))))
        (where (((p_** ... -> r_**) eqs_** ...) ...) (diff-starting-pattern lit-hole 
                                                                            (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                                                                             ...
                                                                             ((p_6 p_7 ... -> r_3) eqs_3 ...)
                                                                             ...)))
        hole)
   
   ; hole (non-terminal)
   ;   If a row in the matrix contains a lit-hole in its first column, and the flag for non-terminal-set is #t, 
   ;   then separate the row from the rest of the matrix, replacing hole with "context-match," which will subsequently 
   ;   be transformed into a conditional based on whether or not there is a current context. Additionally, transform the 
   ;   right-hand-side into a pair of the quote the input variable (the "path"), and the input variable. Union this with the matrix formed by the remaining rows.
   
   #;(--> (in-hole Context
                   (matrix (x_1 x_2 ...)
                           (((p_1 p_2 ... -> r_1) eqs_1 ...)
                            ...
                            ((lit-hole p_5 ... -> r_2) eqs_2 ...)
                            ((p_6 p_7 ... -> r_3) eqs_3 ...)
                            ...)
                           (pvar_1 ...)
                           (pvar_3 ...)
                           natural
                           #t))
          (in-hole Context
                   (begin
                     (cond [(or (context-match) (eqv? x_1 the-hole))
                            (matrix (x_2 ...)
                                    (((p_* ... -> r_*) eqs_* ...) ...);((context-match p_5 ... -> (set (cons (quote x_1) x_1))))
                                    (pvar_1 ...)
                                    (pvar_3 ...)
                                    natural
                                    #t)]
                           (else ∅))
                     (matrix (x_1 x_2 ...)
                             (((p_** ... -> r_**) eqs_** ...) ...)
                             (pvar_1 ...)
                             (pvar_3 ...)
                             natural
                             #t)))
          (side-condition (andmap not-a-lit-hole? (term (p_1 ...))))
          (where (((p_* ... -> r_*) eqs_* ...) ...) (drop-first-p (same-starting-pattern 
                                                                   lit-hole
                                                                   (((p_1 p_2 ... -> (set (cons (quote x_1) x_1))) eqs_1 ...)
                                                                    ...
                                                                    ((lit-hole p_5 ... -> (set (cons (quote x_1) x_1))) eqs_2 ...)
                                                                    ((p_6 p_7 ... -> (set (cons (quote x_1) x_1))) eqs_3 ...)
                                                                    ...))))
          (where (((p_** ... -> r_**) eqs_** ...) ...) (diff-starting-pattern lit-hole 
                                                                              (((p_1 p_2 ... -> r_1) eqs_1 ...)
                                                                               ...
                                                                               ((p_6 p_7 ... -> r_3) eqs_3 ...)
                                                                               ...)))
          hole-non-terminal)
   
   ; in-hole (wrong number of holes)
   ;   If a row in a matrix begins with a lit-in-hole, but the number of holes in the first pattern is 0 or > 1, eliminate the row.
   
   (--> (in-hole Context
                 (matrix (x_1 x_2 ...)
                         (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                          ...
                          (((lit-in-hole p_3 p_4) p_5 ... -> r_2) eqs_2 ...)
                          ((p_6 p_7 ... -> r_3) eqs_3 ...)
                          ...)
                         (pvar_1 ...)
                         (pvar_3 ...)
                         natural
                         bool))
        (in-hole Context
                 (matrix (x_1 x_2 ...)
                         (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                          ...
                          ((p_6 p_7 ... -> r_3) eqs_3 ...)
                          ...)
                         (pvar_1 ...)
                         (pvar_3 ...)
                         natural
                         bool))
        (side-condition (not (eqv? 1 (term (detect-hole2 0 p_3)))))
        
        in-hole-wrong-number-of-holes)
   
   ; in-hole (not base case)
   ;   If a row in a matrix begins with a lit-in-hole, and the number of holes in the first pattern is 1, then return the same matrix, 
   ;   but change the in-hole pattern so that the pieces of the first pattern that don't contain the hole are shifted to the outside of the in-hole.
   
   (--> (in-hole Context
                 (matrix (x_1 x_2 ...)
                         (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                          ...
                          (((lit-in-hole p_3 p_4) p_5 ... -> r_2) eqs_2 ...)
                          ((p_6 p_7 ... -> r_3) eqs_3 ...)
                          ...)
                         (pvar_1 ...)
                         (pvar_3 ...)
                         natural
                         bool))
        (in-hole Context
                 (matrix (x_1 x_2 ...)
                         (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                          ...
                          (((move-hole-op-inward lit-in-hole p_4 p_3) p_5 ... -> r_2) eqs_2 ...)
                          ((p_6 p_7 ... -> r_3) eqs_3 ...)
                          ...)
                         (pvar_1 ...)
                         (pvar_3 ...)
                         natural
                         bool))       
        
        (side-condition (eqv? 1 (term (detect-hole2 0 p_3))))
        (side-condition (not-a-lit-name? (term p_3)))
        in-hole-move-hole-op)
   
   (--> (in-hole Context
                 (matrix (x_1 x_2 ...)
                         (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                          ...
                          (((lit-in-hole (lit-name pvar p_3) p_4) p_5 ... -> r_2) eqs_2 ...)
                          ((p_6 p_7 ... -> r_3) eqs_3 ...)
                          ...)
                         (pvar_1 ...)
                         (pvar_3 ...)
                         natural
                         bool))
        (in-hole Context
                 (matrix (x_1 x_2 ...)
                         (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                          ...
                          (((lit-in-hole ,(car (term any_*)) p_4) p_5 ... -> r_2) ,@(cadr (term any_*)))
                          ((p_6 p_7 ... -> r_3) eqs_3 ...)
                          ...)
                         (pvar_1 ...)
                         (pvar_3 ...)
                         natural
                         bool))
        (fresh pvar-car)
        (fresh pvar-cdr)
        (where any_* (push-name-downward (lit-name pvar p_3) (eqs_2 ...) pvar-car pvar-cdr))
        (side-condition (not-a-nt? (term p_3)))
        
        ;(where p_* ,(car (term any_*)))
        ;(where (eqs_* ...) ,(cdr (term any_*)))
        in-hole-name)
   
   ; in-hole (base case)
   ;   If a row in a matrix begins with lit-in-hole, where the only thing in the first position is a non-terminal with 1 hole in it, 
   ;   then transform the row into an expression to parameterize a new context, get back the set of pairs which match the non-terminal, 
   ;   then test the cdr of each pair against the second pattern, and finally continue matching the row. Union this with the matrix formed by the remaining rows.
   
   (--> (in-hole Context
                 (matrix (x_1 x_2 ...)
                         (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                          ...
                          (((lit-in-hole (nt id) p_4) p_5 ... -> r_2) eqs_2 ...)
                          ((p_6 p_7 ... -> r_3) eqs_3 ...)
                          ...)
                         (pvar_1 ...)
                         (pvar_3 ...)
                         natural
                         bool))
        (in-hole Context
                 (begin
                   (for
                    ((rt (parameterize ((context-match in-context))
                           (in-list (,(string->symbol (format "~s~s" (term id) '-list)) x_1)))))
                    (let ((cdr-rt (cdr rt)))
                      (matrix (cdr-rt x_2 ...)
                              (((p_* ... -> r_*) eqs_* ...) ...)
                              (pvar_1 ...)
                              (pvar_3 ...)
                              natural
                              bool))
                    )
                   (matrix (x_1 x_2 ...)
                           (((p_** ... -> r_**) eqs_** ...) ...)
                           (pvar_1 ...)
                           (pvar_3 ...)
                           natural
                           bool)
                   )
                 )
        (side-condition (eqv? 1 (term (detect-hole2 0 (nt id)))))
        
        (where (((p_* ... -> r_*) eqs_* ...) ...) (replace-first-p p_4
                                                                   (same-starting-pattern 
                                                                    (lit-in-hole (nt id) p_4)
                                                                    (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                                                                     ...
                                                                     (((lit-in-hole (nt id) p_4) p_5 ... -> r_2) eqs_2 ...)
                                                                     ((p_6 p_7 ... -> r_3) eqs_3 ...)
                                                                     ...))))
        (where (((p_** ... -> r_**) eqs_** ...) ...) (diff-starting-pattern (lit-in-hole (nt id) p_4)
                                                                            (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                                                                             ...
                                                                             ((p_6 p_7 ... -> r_3) eqs_3 ...)
                                                                             ...)))
        (fresh rt)
        (fresh cdr-rt)
        in-hole)
   
   (--> (in-hole Context
                 (matrix (x_1 x_2 ...)
                         (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                          ...
                          (((lit-in-hole (lit-name pvar (nt id)) p_4) p_5 ... -> r_2) (pvar_7 bool_7 eq_7 ...) ... (pvar bool_1 eq_1 ...) (pvar_8 bool_8 eq_8 ...) ...)
                          ((p_6 p_7 ... -> r_3) eqs_3 ...)
                          ...)
                         (pvar_1 ...)
                         (pvar_3 ...)
                         natural
                         bool))
        (in-hole Context
                 (begin 
                   (for
                    ((rt (parameterize ((context-match in-context))
                           (in-list (,(string->symbol (format "~s~s" (term id) '-list)) x_1)))))
                    (let ((car-rt (car rt))
                          (cdr-rt (cdr rt)))
                      (matrix (cdr-rt x_2 ...)
                              (((p_4 p_5 ... -> r_2) (pvar_7 bool_7 eq_7 ...) ... (pvar bool_1 car-rt eq_1 ...) (pvar_8 bool_8 eq_8 ...) ...))
                              (pvar_1 ...)
                              (pvar_3 ...)
                              natural
                              bool))
                    )
                   (matrix (x_1 x_2 ...)
                           (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                            ...
                            ((p_6 p_7 ... -> r_3) eqs_3 ...)
                            ...)
                           (pvar_1 ...)
                           (pvar_3 ...)
                           natural
                           bool)
                   )
                 )
        (fresh rt)
        (fresh car-rt)
        (fresh cdr-rt)
        (side-condition (eqv? 1 (term (detect-hole2 0 (nt id)))))
        
        in-hole-nt-name)
   
   ; hide-hole (not base case)
   ;   If a row in a matrix starts with a lit-hide-hole, replace the lit-hide-hole with a hide-hole where the patterns not containing holes have been shifted to the outside.
   
   (--> (in-hole Context
                 (matrix (x_1 x_2 ...)
                         (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                          ...
                          (((lit-hide-hole p_3) p_5 ... -> r_2) eqs_2 ...)
                          ((p_6 p_7 ... -> r_3) eqs_3 ...)
                          ...)
                         (pvar_1 ...)
                         (pvar_3 ...)
                         natural
                         bool))
        (in-hole Context
                 (matrix (x_1 x_2 ...)
                         (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                          ...
                          (((move-hole-op-inward lit-hide-hole 'lit-hole p_3) p_5 ... -> r_2) eqs_2 ...)
                          ((p_6 p_7 ... -> r_3) eqs_3 ...)
                          ...)
                         (pvar_1 ...)
                         (pvar_3 ...)
                         natural
                         bool))
        
        hide-hole-move-hole-op)
   
   ; hide-hole (base case)
   ;   If a row in a matrix starts with a lit-hide-hole where the pattern is a non-terminal containing at least one hole,
   ;   wrap the row in an expression to parameterize the context to no-context, then match the non-terminal and the rest of the row. 
   ;   Union the result with the rows formed by the rest of the matrix.
   
   (--> (in-hole Context
                 (matrix (x_1 x_2 ...)
                         (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                          ...
                          (((lit-hide-hole (nt id)) p_5 ... -> r_2) eqs_2 ...)
                          ((p_6 p_7 ... -> r_3) eqs_3 ...)
                          ...)
                         (pvar_1 ...)
                         (pvar_3 ...)
                         natural
                         bool))
        (in-hole Context
                 (begin
                   (parameterize ((context-match no-context))
                     (matrix (x_1 x_2 ...)
                             (((p_* ... -> r_*) eqs_* ...) ...) #;(((nt id) p_5 ... -> r_2))
                             (pvar_1 ...)
                             (pvar_3 ...)
                             natural
                             bool))
                   (matrix (x_1 x_2 ...)
                           (((p_** ... -> r_**) eqs_** ...) ...)
                           (pvar_1 ...)
                           (pvar_3 ...)
                           natural
                           bool)
                   )
                 )
        (side-condition (not (eqv? 0 (term (detect-hole2 0 (nt id))))))
        
        (where (((p_* ... -> r_*) eqs_* ...) ...) (replace-first-p (nt id)
                                                                   (same-starting-pattern 
                                                                    (lit-hide-hole (nt id))
                                                                    (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                                                                     ...
                                                                     (((lit-hide-hole (nt id)) p_5 ... -> r_2) eqs_2 ...)
                                                                     ((p_6 p_7 ... -> r_3) eqs_3 ...)
                                                                     ...))))
        (where (((p_** ... -> r_**) eqs_** ...) ...) 
               (diff-starting-pattern
                (lit-hide-hole (nt id))
                (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                 ...
                 ((p_6 p_7 ... -> r_3) eqs_3 ...)
                 ...)))
        hide-hole-nt)
   
   ; hide-hole (hole)
   (--> (in-hole Context
                 (matrix (x_1 x_2 ...)
                         (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                          ...
                          (((lit-hide-hole lit-hole) p_5 ... -> r_2) eqs_2 ...)
                          ((p_6 p_7 ... -> r_3) eqs_3 ...)
                          ...)
                         (pvar_1 ...)
                         (pvar_3 ...)
                         natural
                         bool))
        (in-hole Context
                 (begin
                   (parameterize ((context-match no-context))
                     (matrix (x_1 x_2 ...)
                             (((p_* ... -> r_*) eqs_* ...) ...) #;(((nt id) p_5 ... -> r_2))
                             (pvar_1 ...)
                             (pvar_3 ...)
                             natural
                             bool))
                   (matrix (x_1 x_2 ...)
                           (((p_** ... -> r_**) eqs_** ...) ...)
                           (pvar_1 ...)
                           (pvar_3 ...)
                           natural
                           bool)
                   )
                 )
        
        (where (((p_* ... -> r_*) eqs_* ...) ...) (replace-first-p lit-hole
                                                                   (same-starting-pattern 
                                                                    (lit-hide-hole lit-hole)
                                                                    (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                                                                     ...
                                                                     (((lit-hide-hole lit-hole) p_5 ... -> r_2) eqs_2 ...)
                                                                     ((p_6 p_7 ... -> r_3) eqs_3 ...)
                                                                     ...))))
        (where (((p_** ... -> r_**) eqs_** ...) ...) 
               (diff-starting-pattern
                (lit-hide-hole lit-hole)
                (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                 ...
                 ((p_6 p_7 ... -> r_3) eqs_3 ...)
                 ...)))
        hide-hole)
   
   ; side-condition
   (--> (in-hole Context
                 (matrix (x_1 x_2 ...)
                         (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                          ...
                          (((lit-side-condition p_3 any) p_4 ... -> r_2) eqs_2 ...)
                          ((p_6 p_7 ... -> r_3) eqs_3 ...)
                          ...)
                         (pvar_1 ...)
                         (pvar_3 ...)
                         natural
                         bool))
        (in-hole Context 
                 (matrix (x_1 x_2 ...)
                         (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                          ...
                          ((p_3 p_4 ... -> (if any r_2 ∅)) eqs_2 ...)
                          ((p_6 p_7 ... -> r_3) eqs_3 ...)
                          ...)
                         (pvar_1 ...)
                         (pvar_3 ...)
                         natural
                         bool))
        
        side-condition)
   
   ; Repeat
   ;   	If a row in a matrix starts with (repeat p_1 p_2), first determine all the identifiers for name patterns in p_1. Call the ones which are not marked as bound in the matrix variable_1 ..., and the ones which are marked as bound in the matrix variable_2 ...
   ;	Seperate the matrix into the row with the repeat and all other rows, unioning the result together. Wrap the repeat row in a let expression to bind all the elements of variable_2 ... to fresh temporary variables. Inside this define a letrec called match-repeach as a function which takes a fresh variable z, variable_1 ..., and variable_2 ... as its arguments. variable_1 ... will be used to build up the bindings from inside the repeat, while variable_2 ... will be unwrapped to check that values already bound outside of the repeat match those inside the repeat.
   ;	Inside the function, union the results of two conditionals. The first is the "base case," where every element of variable_2 ... is equal to the empty list, and therefore the variable bound outside the repeat are correct inside the repeat. In this case, we return a matrix where the first input variable is z, the first pattern in the row is p_2, and the rest of the input/row are the same as before. We mark variable_1 ... as bound. This matrix is wrapped in a let expression which restores the values bound outside the repeat back from their temporary forms.
   ;	The second conditional checks if z and variable_2 ... are all cons?. If they are, it stores all the values for variable_1 ... as temporaries, then matches (car z) against p_1, with the righthand side equal to a call to match repeat with (cdr z), the cons of variable_1 ... the temporary values for variable_1 ..., and the cdr of all the elements of variable 2. (The new bindings are built up by one layer, and the ones bound outside of the repeat are "unwrapped" by one layer). In this one row, one pattern matrix, the natural indicating the depth of the repeat is incremented.
   ;	Finally, in the body of the letrec, all the elements of variable_1 ... are bound to empty, and there is the call (match-repeat x_1 variable_1 ... variable_2 ...).
   (--> (in-hole Context
                 (matrix (x_1 x_2 ...)
                         (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                          ...
                          (((repeat p_3 p_4) p_5 ... -> r_2) eqs_2 ...)
                          ((p_6 p_7 ... -> r_3) eqs_3 ...)
                          ...)
                         (pvar_1 ...)
                         (pvar_3 ...)
                         natural
                         bool))
        (in-hole Context
                 (begin (in-hole any_term-let
                                  (letrec ((match-repeat
                                            (λ ,(append (append (list (term z)) (term (binding_temp ...))) (term (bound_temp ...)) )      
                                              (begin
                                                (cond
                                                  ((andmap empty? ,(append (list 'list) (term (bound_temp ...))))
                                                   (Build-Let ;reverse
                                                    (matrix (z x_2 ...)
                                                            (((p_4 p_5 ... -> r_2) ,@(term (Add-Repeat-Vars (eqs_* ...) (binding_temp ...)))))
                                                            (pvar_1 ...)
                                                            ()
                                                            natural
                                                            bool)
                                                    (binding_temp ...)))
                                                  )
                                                (cond
                                                  ((andmap cons? ,(append (list 'list) (append (list (term z)) (term (bound_temp ...)))  ))
                                                   (Build-Temp-Let
                                                    (binding_temp ...)
                                                    (single_binding_temp ...)
                                                    (let ((carz (car z)))
                                                    (matrix (carz)
                                                            (((p_3 -> (match-repeat ,@(append 
                                                                                       (append (list (term (cdr z))) 
                                                                                               (term (Build-Temp-Cons (binding_temp ...) (single_binding_temp ...)))) 
                                                                                       (term (Build-Cdr (bound_temp ...))  ))))
                                                              eqs_* ...))
                                                            (pvar_1 ...)
                                                            (pvar_3 ...)
                                                            ,(+ 1 (term natural))
                                                            bool)))
                                                   )
                                                  (else ∅)
                                                  )
                                                )
                                              )))
                                    (Build-Let-Empty natural ,(map (λ (x) (term (Get-Pvar ,x))) (term (binding_temp ...)))
                                                     (match-repeat ,@(if (> (term natural) 0)
                                                                         (append (append (list (term x_1)) (term (binding_temp ...)) (map (λ (x) `(term ,x)) (term (pvar_22 ...)))))
                                                                         (append (append (list (term x_1)) (term (binding_temp ...)) (term (bound_temp ...))))
                                                                         ))
                                                     )
                                    )
                                 )
                        (matrix (x_1 x_2 ...)
                                (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                                 ...
                                 ((p_6 p_7 ... -> r_3) eqs_3 ...)
                                 ...)
                                (pvar_1 ...)
                                (pvar_3 ...)
                                natural
                                bool)
                        )
                 )
        (where any_term-let ,(if (eqv? 0 (term natural))
                                 (begin (set! new-var-list (term (eqs_2 ...))) (let ((r (term (Build-Term-Let (eqs_2 ...) hole)))) #;(set! new-var-list (term (eqs_2 ...))) r))
                                 (begin (set! new-var-list (term (eqs_2 ...)))
                                        (term hole))))
        (where (eqs_* ...) ,new-var-list)
        (where (pvar_11 ...) ,(remove-duplicates (map (λ (x) (term (Get-Pvar ,x)))
                                                      (term 
                                                       (Get-Free-Name-Patterns
                                                        p_3 
                                                        ,(map (λ (x) (term (Get-Pvar ,x))) (append (term (Binding-Eqs (eqs_* ...))) (term (True-Eqs (eqs_* ...) ())) )) ())))))
        (where (pvar_22 ...) ,(begin
                 ;(printf "~a\n" (remove-duplicates (map (λ (x) (term (Get-Pvar ,x 0))) (append (term (Binding-Eqs (eqs_* ...))) (term (True-Eqs (eqs_* ...) ())))) ))
                 (remove-duplicates (map (λ (x) (term (Get-Pvar ,x 0))) (append (term (Binding-Eqs (eqs_* ...))) (term (True-Eqs (eqs_* ...) ())))) )
                 ))
        (fresh match-repeat)
        (fresh z)
        (fresh carz)
        (fresh ((binding_temp ...)
                (pvar_11 ...)))
        (fresh ((single_binding_temp ...)
                (pvar_11 ...)))
        (fresh ((bound_temp ...)
                (pvar_22 ...)))  
        Repeat)
   
;   (--> (in-hole Context
;                 (matrix (x_1 x_2 ...)
;                         (((cw_1 p_2 ... -> r_1) eqs_1 ...)
;                          ...
;                          (((repeat p_3 p_4) p_5 ... -> r_2) eqs_2 ...)
;                          ((p_6 p_7 ... -> r_3) eqs_3 ...)
;                          ...)
;                         (pvar_1 ...)
;                         (pvar_3 ...)
;                         natural
;                         bool))
;        (in-hole Context
;                 (begin (in-hole any_term-let
;                                 (Build-Temp-Let
;                                  natural
;                                  (bound-temp ...)
;                                  ,(map (λ (x) (list 'term x))
;                                        (term (pvar_22 ...)))
;                                        ;)
;                                  (letrec ((match-repeat
;                                            (λ ,(append (append (list (term z)) (term (pvar_11 ...))) (map (λ (x) (term (Get-Pvar ,x))) (term (pvar_22 ...))) )       
;                                              (begin
;                                                (cond
;                                                  ((andmap empty? ,(append (list 'list) (map (λ (x) (term (Get-Pvar ,x))) (term (pvar_22 ...))) ))
;                                                   (Build-Let
;                                                    (Restore-Temp natural (pvar_22 ...) (bound-temp ...)
;                                                    (matrix (z x_2 ...)
;                                                            (((p_4 p_5 ... -> r_2) ,@(term (Add-Repeat-Vars (eqs_* ...) (pvar_11 ...)))))
;                                                            (pvar_1 ...)
;                                                            ()
;                                                            natural
;                                                            bool)
;                                                    )
;                                                    
;                                                    (pvar_11 ...)))
;                                                  #;(else ∅)
;                                                  )
;                                                
;                                                (cond
;                                                  ((andmap cons? ,(append (list 'list) (append (list (term z)) (map (λ (x) (term (Get-Pvar ,x))) (term (pvar_22 ...)))  )))
;                                                   (Build-Temp-Let
;                                                    (binding-temp ...)
;                                                    (pvar_11 ...)
;                                                    (matrix ((car z))
;                                                            (((p_3 -> (match-repeat ,@(append 
;                                                                                       (append (list (term (cdr z))) 
;                                                                                               (term (Build-Temp-Cons (pvar_11 ...) (binding-temp ...)))) 
;                                                                                       (term (Build-Cdr ,(map (λ (x) (term (Get-Pvar ,x))) (term (pvar_22 ...)))  )))))
;                                                              eqs_* ...))
;                                                            (pvar_1 ...)
;                                                            (pvar_3 ...)
;                                                            ,(+ 1 (term natural))
;                                                            bool))
;                                                   )
;                                                  (else ∅)
;                                                  )
;                                                )
;                                              )))
;                                    (Build-Let-Empty natural ,(map (λ (x) (term (Get-Pvar ,x))) (term (pvar_11 ...)))
;                                                     (match-repeat ,@(if (> (term natural) 0)
;                                                                         (append (append (list (term x_1)) (map (λ (x) (term (Get-Pvar ,x))) (term (pvar_11 ...))) ) (map (λ (x) (term (Get-Pvar ,x))) (term (pvar_22 ...))))
;                                                                         (append (append (list (term x_1)) (map (λ (x) (term (Get-Pvar ,x))) (term (pvar_11 ...))) ) (term (bound-temp ...)))
;                                                                                  ;(map (λ (x) (list 'term x)) (term (pvar_22 ...))))
;                                                                             ;(map (λ (x) (term (Get-Pvar ,x)))#;(λ (x) (list 'term x)) (term (pvar_22 ...)))
;                                                                             ))
;                                                     )
;                                    ))
;                                 )
;                        (matrix (x_1 x_2 ...)
;                                (((cw_1 p_2 ... -> r_1) eqs_1 ...)
;                                 ...
;                                 ((p_6 p_7 ... -> r_3) eqs_3 ...)
;                                 ...)
;                                (pvar_1 ...)
;                                (pvar_3 ...)
;                                natural
;                                bool)
;                        )
;                 )
;        (where any_term-let ,(if (eqv? 0 (term natural))
;                                 (begin (set! new-var-list (term (eqs_2 ...))) (let ((r (term (Build-Term-Let (eqs_2 ...) hole)))) (set! new-var-list (term (eqs_2 ...))) r))
;                                 (begin (set! new-var-list (term (eqs_2 ...)))
;                                        (term hole))))
;        #;(where any_term-let ,(begin (set! new-var-list (term (eqs_2 ...))) (term (Build-Term-Let (eqs_2 ...) hole))
;                                      ))
;        #;(if (> -1 (term natural))
;              (begin (printf "1 ~a ~a\n\n" (term (eqs_2 ...)) new-var-list) (term (Build-Term-Let (eqs_2 ...) hole)))
;              (begin (printf "2 ~a ~a\n\n" (term (eqs_2 ...)) new-var-list) (set! new-var-list (term (eqs_2 ...)))
;                     (printf "3 ~a ~a\n\n" (term (eqs_2 ...)) new-var-list)
;                     (term hole)))
;        (where (eqs_* ...) ,new-var-list)
;        (where (pvar_11 ...) ,(remove-duplicates (map (λ (x) (term (Get-Pvar ,x)))
;                                                      (term 
;                                                       (Get-Free-Name-Patterns
;                                                        p_3 
;                                                        ,(map (λ (x) (term (Get-Pvar ,x))) (append (term (Binding-Eqs (eqs_* ...))) (term (True-Eqs (eqs_* ...) ())) )) ())))))
;        (where (pvar_22 ...) ,
;               (begin
;                 ;(printf "~a\n" (remove-duplicates (map (λ (x) (term (Get-Pvar ,x 0))) (append (term (Binding-Eqs (eqs_* ...))) (term (True-Eqs (eqs_* ...) ())))) ))
;                 (remove-duplicates (map (λ (x) (term (Get-Pvar ,x 0))) (append (term (Binding-Eqs (eqs_* ...))) (term (True-Eqs (eqs_* ...) ())))) )
;                 ))
;        (fresh match-repeat)
;        (fresh z)
;        ;(fresh ((pvar_11 ...) (pvar_111 ...)))
;        ;(fresh ((pvar_22 ...) (pvar_222 ...)))
;        (fresh ((binding-temp ...)
;                (pvar_11 ...)))
;        (fresh ((bound-temp ...)
;                (pvar_22 ...)))
;        
;        ;        (side-condition (andmap not-a-lit-hole? (term (p_1 ...))))
;        ;        (side-condition (andmap not-a-lit-name? (term (p_1 ...))))
;        ;        (side-condition (andmap not-an-in-hole? (term (p_1 ...))))
;        ;        (side-condition (andmap not-a-s? (term (p_1 ...))))
;        ;        (side-condition (andmap not-a-rep? (term (p_1 ...))))
;        ;        (side-condition (andmap not-a-nt? (term (p_1 ...))))
;        ;        (side-condition (andmap not-a-side-condition? (term (p_1 ...))))
;        
;        Repeat)
   
   ; built-in non-terminals
   (-->
    (in-hole Context
             (matrix (x_1 x_2 ...)
                     (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                      ...
                      ((s p_5 ... -> r_2) eqs_2 ...)
                      ((p_6 p_7 ... -> r_3) eqs_3 ...)
                      ...)
                     (pvar_2 ...)
                     (pvar_3 ...)
                     natural
                     bool))
    (in-hole Context
             (begin 
               (cond ((Func-Replace (eqv? s x_1))
                      (matrix (x_2 ...)
                              (((p_* ... -> r_*) eqs_* ...) ...)
                              (pvar_2 ...)
                              (pvar_3 ...)
                              natural
                              bool))
                     (else ∅))
               (matrix (x_1 x_2 ...)
                       (((p_** ... -> r_**) eqs_** ...) ...)
                       (pvar_2 ...)
                       (pvar_3 ...)
                       natural
                       bool)))
    (where (((p_* ... -> r_*) eqs_* ...) ...) (drop-first-p (same-starting-pattern 
                                                             s
                                                             (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                                                              ...
                                                              ((s p_5 ... -> r_2) eqs_2 ...)
                                                              ((p_6 p_7 ... -> r_3) eqs_3 ...)
                                                              ...))))
    (where (((p_** ... -> r_**) eqs_** ...) ...) (diff-starting-pattern s 
                                                                        (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                                                                         ...
                                                                         ((p_6 p_7 ... -> r_3) eqs_3 ...)
                                                                         ...)))
    
    #;(side-condition (andmap not-a-lit-name? (term (p_6 ...))))
    built-in-non-terminals)
   ))

(define not-a-wildcard?
  (let ([wildcard? (redex-match L wildcard)])
    (λ (x) (not (wildcard? x)))))

(define not-an-or?
  (let ([or? (redex-match L (or p_1 p_2))])
    (λ (x) (not (or? x)))))

(define not-a-lit-name?
  (let ([lit-name? (redex-match L (lit-name variable p))])
    (λ (x) (not (lit-name? x)))))

(define not-a-pvar?
  (let ([pvar? (redex-match L pvar)])
    (λ (x) (not (pvar? x)))))

(define not-a-rep?
  (let ([rep? (redex-match L rep)])
    (λ (x) (not (rep? x)))))

(define not-a-side-condition?
  (let ([sc? (redex-match L (lit-side-condition p any))])
    (λ (x) (not (sc? x)))))

(define not-a-cp?
  (let ([cp? (redex-match L cp)])
    (λ (x) (not (cp? x)))))

(define not-a-nt?
  (let ([nt? (redex-match L (nt id))])
    (λ (x) (not (nt? x)))))

(define not-a-lit-hole?
  (let ([lit-hole? (redex-match L lit-hole)])
    (λ (x) (not (lit-hole? x)))))

(define not-a-s?
  (let ([s? (redex-match L s)])
    (λ (x) (not (s? x)))))

(define not-an-id?
  (let ([id? (redex-match L id)])
    (λ (x) (not (id? x)))))

(define not-an-in-hole?
  (let ([in-hole? (redex-match L (lit-in-hole p_1 p_2))])
    (λ (x) (not (in-hole? x)))))

(define is-a-wc?
  (let ([wc? (redex-match L wildcard)])
    (λ (x) (not (not (wc? x))))))

(define natural?
  (λ (x) (and 
          (integer? x) 
          (not (negative? x)))))

(define-metafunction L
  ((Get-Pvar id) id)
  ((Get-Pvar (pvar elip)) (Get-Pvar pvar))
  ((Get-Pvar pvar 0)
   pvar)
  ((Get-Pvar (pvar elip) natural)
   (Get-Pvar pvar ,(- (term natural) 1)))
  ((Get-Pvar id natural)
   id)
  )

(define-metafunction L
  [(Add-Repeat-Vars ((pvar bool eq ...) eqs_2 ...) (pvar_1 ... pvar_3 pvar_2 ...))
   (Add-Repeat-Vars (eqs_2 ... (pvar bool ,@(remove-duplicates (term (pvar_3 eq ...))))) (pvar_1 ... pvar_2 ...))
   (side-condition (equal? (term pvar_3) (term (Get-Pvar pvar))))
   ]
  [(Add-Repeat-Vars (eqs_1 ... (pvar bool eq ...) eqs_2 ...) (pvar_3 pvar_1 ...))
   (Add-Repeat-Vars (eqs_1 ... eqs_2 ... (pvar bool ,@(remove-duplicates (term (pvar_3 eq ...))))) (pvar_1 ...))
   (side-condition (equal? (term pvar_3) (term (Get-Pvar pvar))))
   ]
  [(Add-Repeat-Vars (eqs ...) (pvar_1 ...))
   (eqs ...)])

(define-metafunction L
  [(True-Eqs ((pvar #t eq ...) eqs_2 ...) (pvar_1 ...))
   (True-Eqs (eqs_2 ...) (pvar pvar_1 ...))]
  [(True-Eqs ((pvar #f eq ...) eqs_2 ...) (pvar_1 ...))
   (True-Eqs (eqs_2 ...) (pvar_1 ...))]
  [(True-Eqs () (pvar_1 ...))
   (pvar_1 ...)]
  )

(define-metafunction L
  [(Binding-Eqs ((pvar bool eq ... id eq ...) eqs_2 ...))
   (pvar pvar_* ...)
   (where (pvar_* ...) (Binding-Eqs (eqs_2 ...)))]
  [(Binding-Eqs ((pvar bool eq ...) eqs_2 ...))
   (pvar_* ...)
   (where (pvar_* ...) (Binding-Eqs (eqs_2 ...)))]
  [(Binding-Eqs (eqs ...))
   ()])

(define-metafunction L 
  ; Get-Name-Patterns
  ;(lit-in-hole p_1 p_2)  ?
  ;(lit-hide-hole p)      ?
  [(Get-Free-Name-Patterns (lit-in-hole p_1 p_2) (pvar_1 ...) (pvar_2 ...)) 
   ,(append (term (Get-Free-Name-Patterns p_1 (pvar_1 ...) ())) 
            (append (term (Get-Free-Name-Patterns p_2 (pvar_1 ...) ()))
                    (term (pvar_2 ...))))]
  [(Get-Free-Name-Patterns (lit-hide-hole p_1) (pvar_1 ...) (pvar_2 ...))
   ,(append (term (Get-Free-Name-Patterns p_1 (pvar_1 ...) ())) (term (pvar_2 ...)))]
  [(Get-Free-Name-Patterns (or p_1 p_2) (pvar_1 ...) (pvar_2 ...)) 
   ,(append (term (Get-Free-Name-Patterns p_1 (pvar_1 ...) ())) 
            (append (term (Get-Free-Name-Patterns p_2 (pvar_1 ...) ()))
                    (term (pvar_2 ...))))]
  [(Get-Free-Name-Patterns (cons p_1 p_2) (pvar_1 ...) (pvar_2 ...)) 
   ,(append (term (Get-Free-Name-Patterns p_1 (pvar_1 ...) ())) 
            (append (term (Get-Free-Name-Patterns p_2 (pvar_1 ...) ()))
                    (term (pvar_2 ...))))]
  [(Get-Free-Name-Patterns (lit-name id p) (pvar_1 ...) (pvar_2 ...))
   ,(append (term (Get-Free-Name-Patterns p (pvar_1 ...) ())) (append (list (term id)) (term (pvar_2 ...))))
   (side-condition (andmap (λ (x) (not (eqv? (term id) x))) (term (pvar_1 ...))))]
  [(Get-Free-Name-Patterns (repeat p_1 p_2) (pvar_1 ...) (pvar_2 ...))
   ,(append (map (λ (x) `(,x ...)) (remove-duplicates (term (Get-Free-Name-Patterns p_1 (pvar_1 ...) ())))) 
            (append (term (Get-Free-Name-Patterns p_2 (pvar_1 ...) ()))
                    (term (pvar_2 ...))))]
  [(Get-Free-Name-Patterns (lit-side-condition p_1 any) (pvar_1 ...) (pvar_2 ...))
   ,(append (term (Get-Free-Name-Patterns p_1 (pvar_1 ...) ())) (term (pvar_2 ...)))]
  [(Get-Free-Name-Patterns any (pvar_1 ...) (pvar_2 ...)) ()])

(term (Get-Free-Name-Patterns 1 () ()))
(term (Get-Free-Name-Patterns (lit-name a wc) () ()))
(term (Get-Free-Name-Patterns (lit-name a wc) (a) ()))
(term (Get-Free-Name-Patterns (lit-name a wc) () (b)))
(term (Get-Free-Name-Patterns (lit-name a wc) (a) (b)))
(term (Get-Free-Name-Patterns (cons (lit-name a lit-natural) (cons (lit-name b wc) '())) () ()))
(term (Get-Free-Name-Patterns (or (lit-name x lit-natural) (lit-name y wc)) () ()))
(term (Get-Free-Name-Patterns (or (lit-name x wc) (lit-name z (cons (lit-name y wc) '()))) (y) ()))

(define-metafunction L 
  ; Get-Name-Patterns
  ;(lit-in-hole p_1 p_2)  ?
  ;(lit-hide-hole p)      ?
  [(Get-Bound-Name-Patterns (or p_1 p_2) (pvar_1 ...) (pvar_2 ...)) 
   ,(append (term (Get-Bound-Name-Patterns p_1 (pvar_1 ...) ())) 
            (append (term (Get-Bound-Name-Patterns p_2 (pvar_1 ...) ()))
                    (term (pvar_2 ...))))]
  [(Get-Bound-Name-Patterns (cons p_1 p_2) (pvar_1 ...) (pvar_2 ...)) 
   ,(append (term (Get-Bound-Name-Patterns p_1 (pvar_1 ...) ())) 
            (append (term (Get-Bound-Name-Patterns p_2 (pvar_1 ...) ()))
                    (term (pvar_2 ...))))]
  [(Get-Bound-Name-Patterns (lit-name id p) (pvar_1 ...) (pvar_2 ...))
   ,(append (term (Get-Bound-Name-Patterns p (pvar_1 ...) ())) (append (list (term id)) (term (pvar_2 ...))))
   (side-condition (ormap (λ (x) (eqv? (term id) x)) (term (pvar_1 ...))))]
  [(Get-Bound-Name-Patterns (repeat p_1 p_2) (pvar_1 ...) (pvar_2 ...))
   ,(append (term ((,@(term (Get-Bound-Name-Patterns p_1 (pvar_1 ...) ())) ,'...)))
            (append (term (Get-Bound-Name-Patterns p_2 (pvar_1 ...) ()))
                    (term (pvar_2 ...))))]
  [(Get-Bound-Name-Patterns (lit-side-condition p_1 any) (pvar_1 ...) (pvar_2 ...))
   ,(append (term (Get-Bound-Name-Patterns p_1 (pvar_1 ...) ())) (term (pvar_2 ...)))]
  [(Get-Bound-Name-Patterns any (pvar_1 ...) (pvar_2 ...)) ()])

(term (Get-Bound-Name-Patterns 1 () ()))
(term (Get-Bound-Name-Patterns (lit-name a wc) () ()))
(term (Get-Bound-Name-Patterns (lit-name a wc) (a) ()))
(term (Get-Bound-Name-Patterns (lit-name a wc) () (b)))
(term (Get-Bound-Name-Patterns (lit-name a wc) (a) (b)))
(term (Get-Bound-Name-Patterns (cons (lit-name a lit-natural) (cons (lit-name b wc) '())) () ()))
(term (Get-Bound-Name-Patterns (or (lit-name x lit-natural) (lit-name y wc)) () ()))
(term (Get-Bound-Name-Patterns (or (lit-name x wc) (lit-name z (cons (lit-name y wc) '()))) (y) ()))

(define-metafunction L 
  ((Build-Let-Empty natural () any)
   any)
  [(Build-Let-Empty natural (pvar_1 ...) any)
   (let ((pvar_1 '())
         ...)
     any)])

(define-metafunction L
  [(Build-Let any ())
   any]
  [(Build-Let any (pvar_1 ...))
   (let ((pvar_1 (rev pvar_1))
         ...)
     any)])

(define-metafunction L
  ((Build-Temp-Let 0 () () any)
   any)
  [(Build-Temp-Let 0 (pvar_1 ...) (any_2 ...) any)
   (let ((pvar_1 any_2)
         ...)
     any)]
  ((Build-Temp-Let natural any_1 any_2 any_3)
   any_3)
  ((Build-Temp-Let () () any)
   any)
  [(Build-Temp-Let (pvar_1 ...) (any_2 ...) any)
   (let ((pvar_1 any_2)
         ...)
     any)])

(define-metafunction L 
  ((Restore-Temp () () any)
   any)
  [(Restore-Temp (pvar_1 ...) (any_2 ...) any)
   (term-let ((pvar_1 any_2)
              ...)
             any)]
  ((Restore-Temp 0 () () any)
   any)
  [(Restore-Temp 0 (pvar_1 ...) (any_2 ...) any)
   (term-let ((pvar_1 any_2)
              ...)
             any)]
  ((Restore-Temp natural any_1 any_2 any_3)
   any_3))

(define-metafunction L 
  [(Build-Temp-Cons (pvar_1 ...) (pvar_2 ...))
   ((cons pvar_1 pvar_2) ...)])

(define-metafunction L 
  [(Build-Cdr (pvar_1 ...))
   ((cdr pvar_1) ...)])

(term (Build-Let-Empty 
       0
       (Get-Free-Name-Patterns (or (lit-name x wc) (lit-name z (cons (lit-name y wc) '()))) (y) ())
       'body-goes-here))

(define-metafunction L
  ;
  ((detect-hole natural lit-hole)
   ,(+ 1 (term natural)))
  
  ; assume we have table of non-terminals
  ((detect-hole natural (nt id))
   0)
  ; ,(+ (term natural) (nt-struct-number-of-holes (hash-ref nt-table (term 'id)))))
  
  ((detect-hole natural (lit-in-hole p_1 p_2))
   ,(+ (term natural) (term (detect-hole 0 p_2))))
  ((detect-hole natural (lit-hide-hole p_1))
   natural)
  ((detect-hole natural (or p_1 p_2))
   ,(+ (max (term (detect-hole 0 p_1)) (term (detect-hole 0 p_2))) (term natural)))
  ((detect-hole natural (cons p_1 p_2))
   ,(+ (+ (term (detect-hole 0 p_1)) (term (detect-hole 0 p_2))) (term natural)))
  ((detect-hole natural (repeat p_1 p_2))
   ,(+ (term (detect-hole 0 p_2)) (term natural)))
  ((detect-hole natural (lit-name pvar p))
   ,(+ (term (detect-hole 0 p)) (term natural)))
  ((detect-hole natural any)
   natural))

(define-metafunction L
  ((detect-hole2 natural lit-hole)
   ,(+ 1 (term natural)))
  ((detect-hole2 natural (nt id))
   ,(hash-ref hole-table (term id)))
  ((detect-hole2 natural (lit-in-hole p_1 p_2))
   ,(+ (term natural) (term (detect-hole2 0 p_2))))
  ((detect-hole2 natural (lit-hide-hole p_1))
   natural)
  ((detect-hole2 natural (or p_1 p_2))
   ,(+ (max (term (detect-hole2 0 p_1)) (term (detect-hole2 0 p_2))) (term natural)))
  ((detect-hole2 natural (cons p_1 p_2))
   ,(+ (+ (term (detect-hole2 0 p_1)) (term (detect-hole2 0 p_2))) (term natural)))
  ((detect-hole2 natural (repeat p_1 p_2))
   ,(+ (term (detect-hole2 0 p_2)) (term natural)))
  ((detect-hole2 natural (lit-name pvar p))
   ,(+ (term (detect-hole2 0 p)) (term natural)))
  ((detect-hole2 natural any)
   natural)
  )

(term (detect-hole 0 lit-hole))
(term (detect-hole 0 (or lit-hole lit-hole)))
(term (detect-hole 0 (cons lit-hole (cons (lit-hide-hole lit-hole) '()))))
(term (detect-hole 0 (repeat lit-number (cons lit-hole '()))))
(term (detect-hole 0 (cons lit-hole (cons (lit-in-hole lit-hole lit-hole) '()))))
(term (detect-hole 0 (cons 1 '())))

; op is either lit-in-hole or lit-hide-hole
; replace is either p_2 from (lit-in-hole p_1 p_2) or 'lit-hole
; pattern is either p_1 from (lit-in-hole p_1 p_2) or p from (lit-hide-hole p)
(define-metafunction L
  ((move-hole-op-inward lit-in-hole replace lit-hole)
   replace)
  ((move-hole-op-inward lit-hide-hole replace lit-hole)
   (lit-hide-hole lit-hole))
  ((move-hole-op-inward lit-in-hole replace (nt id))
   (lit-in-hole (nt id) replace)
   (side-condition (eqv? 1 (term (detect-hole2 0 (nt id))))))
  ((move-hole-op-inward lit-hide-hole replace (nt id))
   (nt id)
   (side-condition (eqv? 0 (term (detect-hole2 0 (nt id))))))
  ((move-hole-op-inward lit-hide-hole replace (nt id))
   (lit-hide-hole (nt id)))
  ((move-hole-op-inward lit-hide-hole replace (lit-in-hole p_1 p_2))
   (lit-in-hole p_1 p_2))
  ; context inside of context
  ((move-hole-op-inward lit-in-hole replace (lit-in-hole p_1 p_2))
   (lit-in-hole p_1 (lit-in-hole p_2 replace))
   (side-condition (eqv? 1 (term (detect-hole2 0 p_2)))))
  ((move-hole-op-inward op replace (lit-hide-hole p))
   (lit-hide-hole p))
  ((move-hole-op-inward lit-in-hole replace (lit-name pvar p))
   (lit-in-hole (lit-name pvar p) replace)
   (side-condition (eqv? 1 (term (detect-hole2 0 (lit-name pvar p))))))
  ((move-hole-op-inward lit-in-hole replace (lit-name pvar p))
   (lit-name pvar p)
   (side-condition (eqv? 0 (term (detect-hole2 0 (lit-name pvar p))))))
  ((move-hole-op-inward lit-hide-hole replace (lit-name pvar p))
   (lit-name pvar (lit-hide-hole p)))
  ((move-hole-op-inward op replace (or p_1 p_2))
   (or (move-hole-op-inward op replace p_1) (move-hole-op-inward op replace p_2)))
  ((move-hole-op-inward op replace (cons p_1 p_2))
   (cons (move-hole-op-inward op replace p_1) (move-hole-op-inward op replace p_2)))
  ((move-hole-op-inward op replace any)
   any)
  )

(term (move-hole-op-inward lit-in-hole (cons 1 '()) (cons 1 (cons 2 (cons lit-hole '())))))
#;(term (move-hole-op-inward lit-in-hole (cons 1 '()) (cons 1 (cons (nt A) '()))))
#;(term (move-hole-op-inward lit-in-hole (cons 1 '()) (cons 1 (cons (nt A) (cons 4 (cons 5 '()))))))

(define-metafunction L
  ((push-name-downward (lit-name pvar (cons p_1 p_2)) (eqs_1 ... (pvar bool eq_1 ...) eqs_2 ...) pvar_fresh1 pvar_fresh2)
   ((cons (lit-name pvar_fresh1 p_1) (lit-name pvar_fresh2 p_2)) (eqs_1 ... (pvar bool (cons pvar_fresh1 pvar_fresh2) eq_1 ...) (pvar_fresh1 #f) (pvar_fresh2 #f) eqs_2 ...)))
  ((push-name-downward (lit-name pvar lit-hole) (eqs_1 ... (pvar bool eq_1 ...) eqs_2 ...) pvar_fresh1 pvar_fresh2)
   (lit-hole (eqs_1 ... (pvar bool lit-hole eq_1 ...) eqs_2 ...)))
  ((push-name-downward (lit-name pvar (lit-name pvar_2 p_1)) (eqs_1 ... (pvar bool eq_1 ...) eqs_2 ...) pvar_fresh1 pvar_fresh2)
   ((lit-name pvar_2 p_1) (eqs_1 ... (pvar bool (= pvar_2) eq_1 ...) eqs_2 ...)))
  ((push-name-downward (lit-name pvar (lit-in-hole p_1 p_2)) (eqs_1 ... (pvar bool eq_1 ...) eqs_2 ...) pvar_fresh1 pvar_fresh2)
   ((lit-in-hole (lit-name pvar_fresh1 p_1) (lit-name pvar_fresh2 p_2)) (eqs_1 ... (pvar bool (plug pvar_fresh1 pvar_fresh2) eq_1 ...) (pvar_fresh1 #f) (pvar_fresh2 #f) eqs_2 ...)))
  ((push-name-downward any_1 any_2 any_3 any_4)
   (any_1 any_2))
  )

; Replace (eqv? c x) with (another-function x) where appropriate.
(define-metafunction L
  ;Func-Replace : (eqv? c x) -> 
  [(Func-Replace (eqv? number x)) (eqv? number x)]
  [(Func-Replace (eqv? 'variable x)) (eqv? 'variable x)]
  [(Func-Replace (eqv? '() x)) (eqv? '() x)]
  ; these are probably wrong now! example rows: (5 -> 1) (lit-number -> 2) will return (set 1)!
  [(Func-Replace (eqv? lit-number x)) (number? x)]
  [(Func-Replace (eqv? lit-natural x)) (natural? x)]
  [(Func-Replace (eqv? lit-integer x)) (integer? x)]
  [(Func-Replace (eqv? lit-real x)) (real? x)]
  [(Func-Replace (eqv? lit-string x)) (string? x)]
  [(Func-Replace (eqv? lit-variable x)) (symbol? x)]
  ; temporarily added
  [(Func-Replace (eqv? cp x)) #t]
  
  ; These are functions which need to be written
  [(Func-Replace (eqv? (lit-variable-except variable ...) x)) (and (symbol? x) (andmap (λ (y) (not (equal? y x))) (quote (variable ...))))]
  [(Func-Replace (eqv? (lit-variable-prefix variable) x)) (variable-prefix? variable x)]
  ;[(Func-Replace (eqv? lit-variable-not-otherwise-mentioned x)) (variable-not-otherwise-mentioned? x)]
  ;[(Func-Replace (eqv? context-match x)) (or (context-match) (eqv? x 'lit-hole))]
  ;[(Func-Replace (eqv? (rep-match (pvar ...)) x)) (and (not (set-empty? (match-repeat x pvar ...))) (print  (match-repeat x pvar ...)))]
  
  ; Function given to us by define-language
  [(Func-Replace (eqv? (nt id) x)) (set-member? (,(string->symbol (format "~s~s" (term id) '-list)) x) 'MATCHED)]
  )


(define-metafunction L
  [(clean-up (∪ any ∅)) any]
  [(clean-up (∪ any (matrix (any_1 ...) () (any_2 ...) (any_3 ...) natural bool))) any]
  [(clean-up any) any]
  )

(define-metafunction L
  [(add-nt-to-result x ((nt id) p ... -> r))
   ((nt id) p ... -> (set-union (,(string->symbol (format "~s~s" (term id) '-list)) x) r))]
  [(add-nt-to-result x (p_1 p_2 ... -> r))
   (p_1 p_2 ... -> r)]
  )

(define-metafunction L
  [(simplify (cp_1 p_2 ...))
   ((cons '() '()) p_* ...)
   (where (p_* ...) (simplify (p_2 ...)))]
  [(simplify (wc p_2 ...))
   (p_* ...)
   (where (p_* ...) (simplify (p_2 ...)))]
  [(simplify (p_1 p_2 ...))
   (p_1 p_* ...)
   (where (p_* ...) (simplify (p_2 ...)))]
  [(simplify ())
   ()]
  )

; Remove wc and all but one cp from the list of cw
(define-metafunction L
  Cond-List : (cw ...) -> (cw ...)
  [(Cond-List ()) ()]
  [(Cond-List (cw_1 ... cw_2 cw_3 ... cw_2 cw_4 ...)) 
   (cw_* ...)
   (where (cw_* ...) (Cond-List (cw_1 ... cw_2 cw_3 ... cw_4 ...)))]
  [(Cond-List (c_1 ... wc c_2 ...))
   (cw_* ...)
   (where (cw_* ...) (Cond-List (c_1 ... c_2 ...)))]
  [(Cond-List (cw_1 ... cp_1 cw_2 ... cp_2 cw_3 ...))
   (cw_* ...)
   (where (cw_* ...) (Cond-List (cp_1 cw_1 ... cw_2 ... cw_3 ...)))
   (side-condition (andmap not-a-cp? (term (cw_1 ...))))
   (side-condition (andmap not-a-cp? (term (cw_2 ...))))]
  [(Cond-List (cw_1 cw_2 ... cp_1 cw_3 ...))
   (cw_* ...)
   (where (cw_* ...) (Cond-List (cp_1 cw_1 cw_2 ... cw_3 ...)))]
  [(Cond-List (cw ...)) (cw ...)])

; Build a cond expression given constructors and a matrix
(define-metafunction L
  Build-Cond : ((cw ...) m x x) -> e
  [(Build-Cond ((cp_1 cw_1 ...) 
                (matrix (x_1 x_2 ...)
                        (((cw_2 p_2 ... -> r_1) eqs_1 ...)
                         ...
                         ((c_1 p_3 ... -> r_2) eqs_2 ...)
                         ((cw_3 p_4 ... -> r_3) eqs_3 ...)
                         ...)
                        (pvar_1 ...)
                        (pvar_3 ...)
                        natural
                        bool)
                x_3
                x_4))
   (clean-up (∪ (cond
                  [(cons? x_1)
                   (let ((x_3 (car x_1))
                         (x_4 (cdr x_1)))
                     (matrix (x_4 x_3 x_2 ...)
                             (SL (((cw_2 p_2 ... -> r_1) eqs_1 ...)
                                  ...
                                  ((c_1 p_3 ... -> r_2) eqs_2 ...)
                                  ((cw_3 p_4 ... -> r_3) eqs_3 ...)
                                  ...))
                             (pvar_1 ...)
                             (pvar_3 ...)
                             natural
                             bool)
                     )
                   ]
                  [(Func-Replace (eqv? cw_1 x_1))
                   (matrix (x_2 ...)
                           (S cw_1 (((cw_2 p_2 ... -> r_1) eqs_1 ...)
                                    ...
                                    ((c_1 p_3 ... -> r_2) eqs_2 ...)
                                    ((cw_3 p_4 ... -> r_3) eqs_3 ...)
                                    ...))
                           (pvar_1 ...)
                           (pvar_3 ...)
                           natural
                           bool)
                   ]
                  ...
                  #;(else ∅)
                  )
                (matrix (x_2 ...)
                        (D (((cw_2 p_2 ... -> r_1) eqs_1 ...)
                            ...
                            ((c_1 p_3 ... -> r_2) eqs_2 ...)
                            ((cw_3 p_4 ... -> r_3) eqs_3 ...)
                            ...))
                        (pvar_1 ...)
                        (pvar_3 ...)
                        natural
                        bool)
                ))
   ]
  [(Build-Cond ((cw_1 ...) 
                (matrix (x_1 x_2 ...)
                        (((cw_2 p_2 ... -> r_1) eqs_1 ...)
                         ...
                         ((c_1 p_3 ... -> r_2) eqs_2 ...)
                         ((cw_3 p_4 ... -> r_3) eqs_3 ...)
                         ...)
                        (pvar_1 ...)
                        (pvar_3 ...)
                        natural
                        bool)
                x_3
                x_4))
   (clean-up (∪ (cond 
                  [(Func-Replace (eqv? cw_1 x_1))
                   (matrix (x_2 ...)
                           (S cw_1 (((cw_2 p_2 ... -> r_1) eqs_1 ...)
                                    ...
                                    ((c_1 p_3 ... -> r_2) eqs_2 ...)
                                    ((cw_3 p_4 ... -> r_3) eqs_3 ...)
                                    ...))
                           (pvar_1 ...)
                           (pvar_3 ...)
                           natural
                           bool)
                   ]
                  ...
                  #;(else ∅)
                  )
                (matrix (x_2 ...)
                        (D (((cw_2 p_2 ... -> r_1) eqs_1 ...)
                            ...
                            ((c_1 p_3 ... -> r_2) eqs_2 ...)
                            ((cw_3 p_4 ... -> r_3) eqs_3 ...)
                            ...))
                        (pvar_1 ...)
                        (pvar_3 ...)
                        natural
                        bool)
                ))
   ]
  )

; S metafunction from the paper, applied to cons patterns
(define-metafunction L
  SL : (row ...) -> (row ...)
  [(SL ()) ()]
  [(SL ((((cons p_1 p_2) p_3 ... -> r_1) eqs_1 ...) ((p ... -> r) eqs_2 ...) ...))
   (((p_2 p_1 p_3 ... -> r_1) eqs_1 ...)
    ((p_* ... -> r_*) eqs_* ...)
    ...)
   (where (((p_* ... -> r_*) eqs_* ...) ...)
          (SL (((p ... -> r) eqs_2 ...) ...)))]
  [(SL (((wc p_2 ... -> r_1) eqs_1 ...) ((p ... -> r) eqs_2 ...) ...))
   (;(wc wc p_2 ... -> r_1)
    ((p_* ... -> r_*) eqs_* ...)
    ...)
   (where (((p_* ... -> r_*) eqs_* ...) ...)
          (SL (((p ... -> r) eqs_2 ...) ...)))]
  [(SL (((p_1 ... -> r_1) eqs_1 ...) ((p ... -> r) eqs_2 ...) ...))
   (((p_* ... -> r_*) eqs_* ...)
    ...)
   (where (((p_* ... -> r_*) eqs_* ...) ...)
          (SL (((p ... -> r) eqs_2 ...) ...)))])

; S metafunction from the paper
(define-metafunction L
  S : cw (row ...) -> (row ...)
  [(S cw ()) ()]
  [(S wc (((p ... -> r) eqs_1 ...) ...)) ()]
  [(S c_1 (((c_1 p_2 ... -> r_1) eqs_1 ...) ((p ... -> r) eqs_2 ...) ...))
   (((p_2 ... -> r_1) eqs_1 ...)
    ((p_* ... -> r_*) eqs_* ...)
    ...)
   (where (((p_* ... -> r_*) eqs_* ...) ...)
          (S c_1 (((p ... -> r) eqs_2 ...) ...)))]
  [(S cw (((p_1 p_2 ... -> r_1) eqs_1 ...) ((p ... -> r) eqs_2 ...) ...))
   (((p_* ... -> r_*) eqs_* ...)
    ...)
   (where (((p_* ... -> r_*) eqs_* ...) ...)
          (S cw (((p ... -> r) eqs_2 ...) ...)))]
  [(S cw (((p_1 ... -> r_1) eqs_1 ...) ((p ... -> r) eqs_2 ...) ...))
   (((p_* ... -> r_*) eqs_* ...)
    ...)
   (where (((p_* ... -> r_*) eqs_* ...) ...)
          (S cw (((p ... -> r) eqs_2 ...) ...)))])

; D metafunction from the paper
(define-metafunction L
  D : (row ...) -> (row ...)
  [(D ()) ()]
  [(D (((wc p_2 ... -> r_1) eqs_1 ...) ((p ... -> r) eqs_2 ...) ...))
   (((p_2 ... -> r_1) eqs_1 ...)
    ((p_* ... -> r_*) eqs_* ...)
    ...)
   (where (((p_* ... -> r_*) eqs_* ...) ...)
          (D (((p ... -> r) eqs_2 ...) ...)))]
  [(D (((p_1 p_2 ... -> r_1) eqs_1 ...) ((p ... -> r) eqs_2 ...) ...))
   (((p_* ... -> r_*) eqs_* ...)
    ...)
   (where (((p_* ... -> r_*) eqs_* ...) ...)
          (D (((p ... -> r) eqs_2 ...) ...)))])

(define-metafunction L
  drop-first-p : (row ...) -> (row ...)
  ((drop-first-p (((p_1 p_2 ... -> r) eqs_1 ...) ...))
   (((p_2 ... -> r) eqs_1 ...) ...))
  )


(define-metafunction L
  replace-first-p : p (row ...) -> (row ...)
  ((replace-first-p p_1 (((p_2 p_3 ... -> r) eqs_1 ...) ...))
   (((p_1 p_3 ... -> r) eqs_1 ...) ...))
  )

(define-metafunction L
  same-starting-pattern : p (row ...) -> (row ...)
  ((same-starting-pattern p_1 (((p_1 p_3 ... -> r) eqs_1 ...) row ...))
   (((p_1 p_3 ... -> r) eqs_1 ...) row_* ...) 
   (where (row_* ...)
          (same-starting-pattern p_1 (row ...))))
  ((same-starting-pattern (name p_1 p_!_1) (((p_!_1 p_3 ... -> r) eqs_1 ...) row ...))
   (row_* ...)
   (where (row_* ...)
          (same-starting-pattern p_1 (row ...))))
  ((same-starting-pattern p_1 ()) ())
  )

(define-metafunction L
  diff-starting-pattern : p (row ...) -> (row ...)
  ((diff-starting-pattern p_1 (((p_1 p_3 ... -> r) eqs_1 ...) row ...))
   (diff-starting-pattern p_1 (row ...)))
  ((diff-starting-pattern (name p_1 p_!_1) ((((name p_2 p_!_1) p_3 ... -> r) eqs_1 ...) row ...))
   (((p_2 p_3 ... -> r) eqs_1 ...) row_* ...) 
   (where (row_* ...)
          (diff-starting-pattern p_1 (row ...))))
  ((diff-starting-pattern p_1 ()) ())
  )

(define-metafunction L
  def-no-overlap? : p_1 p_2 -> bool
  ((def-no-overlap? p_1 p_1) #f)
  ((def-no-overlap? p_1 wc) #f)
  ((def-no-overlap? wc p_2) #f)
  ((def-no-overlap? (cons p_1 p_2) (cons p_3 p_4))
   ,(or (term (def-no-overlap? p_1 p_3))
        (term (def-no-overlap? p_2 p_4))))
  )

(define no-context (λ (x) (cond ((eqv? 'lit-hole x) #t)
                                (else #f))))

(define in-context (λ (x) #t))

(define context-match (make-parameter no-context))

(define rev (λ (x) 
              (if (cons? x)
                  (reverse x)
                  x)))

(define-metafunction L
  default-rows : (row ...) -> (row ...)
  ((default-rows ((wc p_3 ... -> r) row ...))
   ((wc p_3 ... -> r) row_* ...)  
   (where (row_* ...)
          (default-rows (row ...))))
  ((default-rows (((nt id) p_3 ... -> r) row ...))
   (((nt id) p_3 ... -> r) row_* ...) 
   (where (row_* ...)
          (default-rows (row ...))))
  ((default-rows ((lit-hole p_3 ... -> r) row ...))
   ((lit-hole p_3 ... -> r) row_* ...) 
   (where (row_* ...)
          (default-rows (row ...))))
  ((default-rows (((repeat p_1 p_2) p_3 ... -> r) row ...))
   (((repeat p_1 p_2) p_3 ... -> r) row_* ...) 
   (where (row_* ...)
          (default-rows (row ...))))
  ((default-rows ((p_1 p_3 ... -> r) row ...))
   (default-rows (row ...)))
  ((default-rows ()) ())
  )

(define-metafunction L
  Build-Default : m -> e
  ((Build-Default (matrix (x_1 x_2 ...)
                          ((p_1 p_2 ... -> r_1)
                           ...)
                          (pvar ...)
                          (pvar_2 ...)
                          natural
                          bool))
   (∪
    m_default
    (cond 
      ((number? x_1)
       m_number)
      ((cons? x_1)
       m_cons)
      ((symbol? x_1)
       m_variable)
      ((string? x_1)
       m_string)
      (else ∅))
    )
   (where m_default
          (matrix (x_1 x_2 ...)
                  (default-rows
                    ((p_1 p_2 ... -> r_1)
                     ...)
                    )
                  (pvar ...)
                  (pvar_2 ...)
                  natural
                  bool))
   (where m_number
          (matrix (x_1 x_2 ...)
                  (number-rows
                   ((p_1 p_2 ... -> r_1)
                    ...)
                   )
                  (pvar ...)
                  (pvar_2 ...)
                  natural
                  bool))
   (where m_cons
          (matrix ((car x_1) (cdr x_1) x_2 ...)
                  (cons-rows
                   ((p_1 p_2 ... -> r_1)
                    ...)
                   )
                  (pvar ...)
                  (pvar_2 ...)
                  natural
                  bool))
   (where m_variable
          (matrix (x_1 x_2 ...)
                  (variable-rows
                   ((p_1 p_2 ... -> r_1)
                    ...)
                   )
                  (pvar ...)
                  (pvar_2 ...)
                  natural
                  bool))
   (where m_string
          (matrix (x_1 x_2 ...)
                  (string-rows
                   ((p_1 p_2 ... -> r_1)
                    ...)
                   )
                  (pvar ...)
                  (pvar_2 ...)
                  natural
                  bool))
   ))

(define-metafunction L
  number-rows : (row ...) -> (row ...)
  ((number-rows ((lit-number p_3 ... -> r) row ...))
   ((wc p_3 ... -> r) row_* ...) 
   (where (row_* ...)
          (number-rows (row ...))))
  ((number-rows ((lit-real p_3 ... -> r) row ...))
   ((lit-real p_3 ... -> r) row_* ...) 
   (where (row_* ...)
          (number-rows (row ...))))
  ((number-rows ((lit-integer p_3 ... -> r) row ...))
   ((lit-integer p_3 ... -> r) row_* ...) 
   (where (row_* ...)
          (number-rows (row ...))))
  ((number-rows ((lit-natural p_3 ... -> r) row ...))
   ((lit-natural p_3 ... -> r) row_* ...) 
   (where (row_* ...)
          (number-rows (row ...))))
  ((number-rows ((number p_3 ... -> r) row ...))
   ((number p_3 ... -> r) row_* ...) 
   (where (row_* ...)
          (number-rows (row ...))))
  ((number-rows ((p_1 p_3 ... -> r) row ...))
   (number-rows (row ...)))
  ((number-rows ()) ())
  )

(define-metafunction L
  cons-rows : (row ...) -> (row ...)
  ((cons-rows (((cons p_1 p_2) p_3 ... -> r) row ...))
   ((p_1 p_2 p_3 ... -> r) row_* ...) 
   (where (row_* ...)
          (cons-rows (row ...))))
  ((cons-rows ((p_1 p_3 ... -> r) row ...))
   (cons-rows (row ...)))
  ((cons-rows ()) ())
  )

(define-metafunction L
  variable-rows : (row ...) -> (row ...)
  ((variable-rows ((lit-variable p_3 ... -> r) row ...))
   ((wc p_3 ... -> r) row_* ...) 
   (where (row_* ...)
          (variable-rows (row ...))))
  ((variable-rows (((lit-variable-except id ...) p_3 ... -> r) row ...))
   (((lit-variable-except id ...) p_3 ... -> r) row_* ...) 
   (where (row_* ...)
          (variable-rows (row ...))))
  ((variable-rows (((lit-variable-prefix id) p_3 ... -> r) row ...))
   (((lit-variable-prefix id) p_3 ... -> r) row_* ...) 
   (where (row_* ...)
          (variable-rows (row ...))))
  ((variable-rows (('variable p_3 ... -> r) row ...))
   (('variable p_3 ... -> r) row_* ...) 
   (where (row_* ...)
          (variable-rows (row ...))))
  ((variable-rows ((p_1 p_3 ... -> r) row ...))
   (variable-rows (row ...)))
  ((variable-rows ()) ())
  )

(define-metafunction L
  string-rows : (row ...) -> (row ...)
  ((string-rows ((lit-string p_3 ... -> r) row ...))
   ((wc p_3 ... -> r) row_* ...) 
   (where (row_* ...)
          (string-rows (row ...))))
  ((string-rows ((string p_3 ... -> r) row ...))
   ((string p_3 ... -> r) row_* ...) 
   (where (row_* ...)
          (string-rows (row ...))))
  ((string-rows ((p_1 p_3 ... -> r) row ...))
   (string-rows (row ...)))
  ((string-rows ()) ())
  )

(define-metafunction L
  ;specialize : m -> (e ...)
  ((specialize
    (matrix (x_1 x_2 ...)
            ((c_1 p_2 ... -> r_1)
             (p_3 p_4 ... -> r_2)
             ...)
            (pvar ...)
            (pvar_2 ...)
            natural
            bool))
   (((Func-Replace (eqv? c_1 x_1))
     (matrix (x_2 ...)
             (drop-first-p 
              (same-starting-pattern
               c_1
               ((c_1 p_2 ... -> r_1)
                (p_3 p_4 ... -> r_2)
                ...)))
             (pvar ...)
             (pvar_2 ...)
             natural
             bool))
    any_* ...)
   (where (any_* ...) (specialize 
                       (matrix (x_1 x_2 ...)
                               (diff-starting-pattern
                                c_1
                                ((c_1 p_2 ... -> r_1)
                                 (p_3 p_4 ... -> r_2)
                                 ...))
                               (pvar ...)
                               (pvar_2 ...)
                               natural
                               bool)))
   )
  ((specialize (matrix (x_1 x_2 ...)
                       ()
                       (pvar ...)
                       (pvar_2 ...)
                       natural
                       bool))
   ())
  )


(define-metafunction L
  ((fix-cond (any ...))
   (cond 
     any 
     ... 
     (else ∅))))

(define-metafunction L
  ((Build-Term-Let ((pvar #f eq_1 ... lit-hole eq_2 ...) eqs_2 ...) r)
   (term-let ((pvar the-hole))
             (Build-Term-Let (eqs_2 ... (pvar #t eq_1 ... eq_2 ...)) r))
   (side-condition (andmap not-an-id? (term (eq_1 ...)))))
  
  ((Build-Term-Let ((pvar #f eq_1 ... id eq_2 ...) eqs_2 ...) r)
   (term-let ((pvar id))
             (Build-Term-Let (eqs_2 ... (pvar #t eq_1 ... eq_2 ...)) r))
   (side-condition (andmap not-an-id? (term (eq_1 ...)))))
  
  ((Build-Term-Let ((pvar #t eq_1 ... id eq_2 ...) eqs_2 ...) r)
   (if (equal? (term pvar) id)
       (Build-Term-Let (eqs_2 ... (pvar #t eq_1 ... eq_2 ...)) r)
       ∅)
   (side-condition (andmap not-an-id? (term (eq_1 ...)))))
  
  ((Build-Term-Let ((pvar #f eq ... (= pvar_3) eq_2 ...) eqs_2 ... (pvar_3 #t eq_3 ...) eqs_3 ...) r)
   (term-let ((pvar pvar_3))
             (Build-Term-Let (eqs_2 ... (pvar_3 #t eq_3 ...) eqs_3 ... (pvar #t eq ... eq_2 ...)) r))
   (side-condition (andmap (λ (x) (not (redex-match L (= pvar_11) x))) (term (eq ...)))))
  
  ((Build-Term-Let ((pvar #t eq ... (= pvar_3) eq_2 ...) eqs_2 ... (pvar_3 #t eq_3 ...) eqs_3 ...) r)
   (if (equal? (term pvar) (term pvar_3))
       (Build-Term-Let (eqs_2 ... (pvar_3 #t eq_3 ...) eqs_3 ... (pvar #t eq ... eq_2 ...)) r)
       ∅)
   (side-condition (andmap (λ (x) (not (redex-match L (= pvar_11) x))) (term (eq ...)))))
  
  ((Build-Term-Let ((pvar bool_1 (= pvar_3) eq_2 ...) eqs_2 ... (pvar_3 bool_2 eq_3 ...) eqs_3 ...) r)
   (term (Build-Term-Let (eqs_2 ... (pvar_3 bool_2 eq_3 ...) eqs_3 ... (pvar bool_1 eq_2 ... (= pvar_3))) r))
   (side-condition (or (and (term bool_1)
                            (not (term bool_2)))
                       (and (not (term bool_1))
                            (term bool_2))
                       (and (not (term bool_1))
                            (not (term bool_2))))))
  
  ((Build-Term-Let ((pvar #t eq ... (cons pvar_1 pvar_3) eq_3 ...) eqs_2 ... (pvar_1 #t eq_1 ...) eqs_3 ... (pvar_3 #t eq_4 ...) eqs_4 ...) r)
   (if (and (cons? (term pvar))
            (equal? (car (term pvar)) (term pvar_1)) 
            (equal? (cdr (term pvar)) (term pvar_3)))
       (Build-Term-Let (eqs_2 ... (pvar_1 #t eq_1 ...) eqs_3 ... (pvar_3 #t eq_4 ...) eqs_4 ... (pvar #t eq ... eq_3 ...)) r)
       ∅)
   (side-condition (and (term pvar)
                        (term pvar_1)
                        (term pvar_3)
                        (andmap (λ (x) (not (redex-match L (cons pvar_11 pvar_12) x))) (term (eq ...))))))
  
  ((Build-Term-Let ((pvar bool (cons pvar_1 pvar_3) eq_3 ...) eqs_2 ... (pvar_1 bool_1 eq_1 ...) eqs_3 ... (pvar_3 bool_3 eq_4 ...) eqs_4 ...) r)
   (Build-Term-Let (eqs_2 ... (pvar_1 bool_1 eq_1 ...) eqs_3 ... (pvar_3 bool_3 eq_4 ...) eqs_4 ... (pvar bool eq_3 ... (cons pvar_1 pvar_3))) r)
   (side-condition (or (not (term bool_1))
                       (not (term bool_3)))))
  
  ((Build-Term-Let ((pvar #f eq ... (cons pvar_1 pvar_3) eq_3 ...) eqs_2 ... (pvar_1 #t eq_1 ...) eqs_3 ... (pvar_3 #t eq_4 ...) eqs_4 ...) r)
   (term-let ((pvar (cons (term pvar_1) (term pvar_3))))
             (Build-Term-Let (eqs_2 ... (pvar_1 #t eq_1 ...) eqs_3 ... (pvar_3 #t eq_4 ...) eqs_4 ... (pvar #t eq ... eq_3 ...)) r))
   (side-condition (andmap (λ (x) (not (redex-match L (cons pvar_11 pvar_12) x))) (term (eq ...)))))
  
  ((Build-Term-Let ((pvar #t eq ... (plug pvar_1 pvar_3) eq_3 ...) eqs_2 ... (pvar_1 #t eq_1 ...) eqs_3 ... (pvar_3 #t eq_4 ...) eqs_4 ...) r)
   (if (equal? (term pvar) (plug (term pvar_1) (term pvar_3)))
       (Build-Term-Let (eqs_2 ... (pvar_1 #t eq_1 ...) eqs_3 ... (pvar_3 #t eq_4 ...) eqs_4 ... (pvar #t eq ... eq_3 ...)) r)
       ∅)
   (side-condition (andmap (λ (x) (not (redex-match L (plug pvar_11 pvar_12) x))) (term (eq ...)))))
  
  ((Build-Term-Let ((pvar #f eq ... (plug pvar_1 pvar_3) eq_3 ...) eqs_2 ... (pvar_1 #t eq_1 ...) eqs_3 ... (pvar_3 #t eq_4 ...) eqs_4 ...) r)
   (term-let ((pvar (plug (term pvar_1) (term pvar_3))))
             (Build-Term-Let (eqs_2 ... (pvar_1 #t eq_1 ...) eqs_3 ... (pvar_3 #t eq_4 ...) eqs_4 ... (pvar #t eq ... eq_3 ...)) r))
   (side-condition (andmap (λ (x) (not (redex-match L (plug pvar_11 pvar_12) x))) (term (eq ...)))))
  
  ((Build-Term-Let ((pvar bool (plug pvar_1 pvar_3) eq_3 ...) eqs_2 ...) r)
   (Build-Term-Let (eqs_2 ... (pvar bool eq_3 ... (plug pvar_1 pvar_3))) r))
  
  ((Build-Term-Let ((pvar #t eq ... (cons pvar_1 pvar_3) eq_3 ...) eqs_2 ... (pvar_3 #t eq_1 ...) eqs_3 ... (pvar_1 #t eq_4 ...) eqs_4 ...) r)
   (if (and (cons? (term pvar))
            (equal? (car (term pvar)) (term pvar_1)) 
            (equal? (cdr (term pvar)) (term pvar_3)))
       (Build-Term-Let (eqs_2 ... (pvar_3 #t eq_1 ...) eqs_3 ... (pvar_1 #t eq_4 ...) eqs_4 ... (pvar #t eq ... eq_3 ...)) r)
       ∅)
   (side-condition (and (term pvar)
                        (term pvar_1)
                        (term pvar_3)
                        (andmap (λ (x) (not (redex-match L (cons pvar_11 pvar_12) x))) (term (eq ...))))))
  
  ((Build-Term-Let ((pvar bool (cons pvar_1 pvar_3) eq_3 ...) eqs_2 ... (pvar_3 bool_1 eq_1 ...) eqs_3 ... (pvar_1 bool_3 eq_4 ...) eqs_4 ...) r)
   (Build-Term-Let (eqs_2 ... (pvar_3 bool_1 eq_1 ...) eqs_3 ... (pvar_1 bool_3 eq_4 ...) eqs_4 ... (pvar bool eq_3 ... (cons pvar_1 pvar_3))) r)
   (side-condition (or (not (term bool_1))
                       (not (term bool_3)))))
  
  ((Build-Term-Let ((pvar #f eq ... (cons pvar_1 pvar_3) eq_3 ...) eqs_2 ... (pvar_3 #t eq_1 ...) eqs_3 ... (pvar_1 #t eq_4 ...) eqs_4 ...) r)
   (term-let ((pvar (cons (term pvar_1) (term pvar_3))))
             (Build-Term-Let (eqs_2 ... (pvar_3 #t eq_1 ...) eqs_3 ... (pvar_1 #t eq_4 ...) eqs_4 ... (pvar #t eq ... eq_3 ...)) r))
   (side-condition (andmap (λ (x) (not (redex-match L (cons pvar_11 pvar_12) x))) (term (eq ...)))))
  
  ((Build-Term-Let ((pvar #t eq ... (plug pvar_1 pvar_3) eq_3 ...) eqs_2 ... (pvar_3 #t eq_1 ...) eqs_3 ... (pvar_1 #t eq_4 ...) eqs_4 ...) r)
   (if (equal? (term pvar) (plug (term pvar_1) (term pvar_3)))
       (Build-Term-Let (eqs_2 ... (pvar_3 #t eq_1 ...) eqs_3 ... (pvar_1 #t eq_4 ...) eqs_4 ... (pvar #t eq ... eq_3 ...)) r)
       ∅)
   (side-condition (andmap (λ (x) (not (redex-match L (plug pvar_11 pvar_12) x))) (term (eq ...)))))
  
  ((Build-Term-Let ((pvar #f eq ... (plug pvar_1 pvar_3) eq_3 ...) eqs_2 ... (pvar_3 #t eq_1 ...) eqs_3 ... (pvar_1 #t eq_4 ...) eqs_4 ...) r)
   (term-let ((pvar (plug (term pvar_1) (term pvar_3))))
             (Build-Term-Let (eqs_2 ... (pvar_3 #t eq_1 ...) eqs_3 ... (pvar_1 #t eq_4 ...) eqs_4 ... (pvar #t eq ... eq_3 ...)) r))
   (side-condition (andmap (λ (x) (not (redex-match L (plug pvar_11 pvar_12) x))) (term (eq ...)))))
  
  ((Build-Term-Let ((pvar bool (plug pvar_1 pvar_3) eq_3 ...) eqs_2 ...) r)
   (Build-Term-Let (eqs_2 ... (pvar bool eq_3 ... (plug pvar_1 pvar_3))) r))
  
  ((Build-Term-Let ((pvar #t eq_1 ... lit-hole eq_2 ...) eqs_2 ...) r)
   (if (equal? (term pvar) the-hole)
       (Build-Term-Let (eqs_2 ... (pvar #t eq_1 ... eq_2 ...)) r)
       ∅)
   (side-condition (andmap not-a-lit-hole? (term (eq_1 ...)))))
  
  ((Build-Term-Let ((pvar bool eq ...) ...) (matrix (x_1 ...) (((p_1 ... -> r) eqs_2 ...)) (pvar_1 ...) (pvar_2 ...) natural bool_0))
   ,(begin (set! new-var-list (term ((pvar bool eq ...) ...)))
           (term (matrix (x_1 ...) (((p_1 ... -> r) (pvar bool eq ...) ...)) (pvar_1 ...) (pvar_2 ...) natural bool_0)))
   (side-condition (andmap (λ (x)
                             (or (eqv? 2 (length x))
                                 (not (or 
                                       (redex-match L (pvar_10 bool_10 eq_10 ... id eq_11 ...) x)
                                       (redex-match L (pvar_10 bool_10 eq_10 ... lit-hole eq_11 ...) x)
                                       (redex-match L (pvar_10 bool_10 eq_10 ... (= (side-condition eq_2 (redex-match L (eqs_4 ... (,(term eq_2) #t eq_4 ...) eqs_5 ...) (term ((pvar bool eq ...) ...))))) eq_11 ...) x)
                                       (redex-match L (pvar_10 bool_10 eq_10 ... (cons 
                                                                                  (side-condition  eq_2 (redex-match L (eqs_4 ... (,(term eq_2) #t eq_4 ...) eqs_5 ...) (term ((pvar bool eq ...) ...))))
                                                                                  (side-condition  eq_6 (redex-match L (eqs_6 ... (,(term eq_6) #t eq_6 ...) eqs_7 ...) (term ((pvar bool eq ...) ...))))
                                                                                  )
                                                               eq_11 ...) x)
                                       (redex-match L (pvar_10 bool_10 eq_10 ... (plug 
                                                                                  (side-condition  eq_2 (redex-match L (eqs_4 ... (,(term eq_2) #t eq_4 ...) eqs_5 ...) (term ((pvar bool eq ...) ...))))
                                                                                  (side-condition  eq_6 (redex-match L (eqs_6 ... (,(term eq_6) #t eq_6 ...) eqs_7 ...) (term ((pvar bool eq ...) ...))))
                                                                                  )
                                                               eq_11 ...) x)
                                       )
                                      
                                      
                                      ))
                             ) (term ((pvar bool eq ...) ...)))))
  
  ((Build-Term-Let ((pvar bool eq ...) ...) r)
   ,(begin (set! new-var-list (term ((pvar bool eq ...) ...))) (term r))
   (side-condition (andmap (λ (x)
                             (or (eqv? 2 (length x))
                                 (not (or 
                                       (redex-match L (pvar_10 bool_10 eq_10 ... id eq_11 ...) x)
                                       (redex-match L (pvar_10 bool_10 eq_10 ... lit-hole eq_11 ...) x)
                                       (redex-match L (pvar_10 bool_10 eq_10 ... (= (side-condition eq_2 (redex-match L (eqs_4 ... (,(term eq_2) #t eq_4 ...) eqs_5 ...) (term ((pvar bool eq ...) ...))))) eq_11 ...) x)
                                       (redex-match L (pvar_10 bool_10 eq_10 ... (cons 
                                                                                  (side-condition  eq_2 (redex-match L (eqs_4 ... (,(term eq_2) #t eq_4 ...) eqs_5 ...) (term ((pvar bool eq ...) ...))))
                                                                                  (side-condition  eq_6 (redex-match L (eqs_6 ... (,(term eq_6) #t eq_6 ...) eqs_7 ...) (term ((pvar bool eq ...) ...))))
                                                                                  )
                                                               eq_11 ...) x)
                                       (redex-match L (pvar_10 bool_10 eq_10 ... (plug 
                                                                                  (side-condition  eq_2 (redex-match L (eqs_4 ... (,(term eq_2) #t eq_4 ...) eqs_5 ...) (term ((pvar bool eq ...) ...))))
                                                                                  (side-condition  eq_6 (redex-match L (eqs_6 ... (,(term eq_6) #t eq_6 ...) eqs_7 ...) (term ((pvar bool eq ...) ...))))
                                                                                  )
                                                               eq_11 ...) x)
                                       )
                                      
                                      
                                      ))
                             ) (term ((pvar bool eq ...) ...)))))
  
  ((Build-Term-Let ((pvar bool eq ...) eqs ...) r)
   (Build-Term-Let (eqs ... (pvar bool eq ...)) r))
  
  #;((Build-Term-Let ((pvar #t eq_1 eq_2 ...) eqs_2 ... (eq_1 #t eq_3 ...) eqs_3 ...) r)
     (if (equal? (term pvar) (term eq_1))
         (Build-Term-Let ((pvar #t eq_2 ...) eqs_2 ... (eq_1 #t eq_3 ...) eqs_3 ...) r)
         ∅))
  
  #;((Build-Term-Let ((pvar #t) (pvar_2 #t) ... (pvar_3 #f) eqs ...) r)
     (Build-Term-Let (eqs ... (pvar_3 #f)) r))
  
  
  
  ; if a one-rowed matrix is the the result, assume that the matching isn't over and just update the eqs
  #;((Build-Term-Let (eqs ...) (matrix (x_1 ...) (((p_1 ... -> r) eqs_2 ...)) (pvar_1 ...) (pvar_2 ...) natural bool))
     (matrix (x_1 ...) (((p_1 ... -> r) eqs ...)) (pvar_1 ...) (pvar_2 ...) natural bool))
  
  #;((Build-Term-Let (eqs ...) r)
     r)
  )


(define-metafunction L
  ((simple-swap (matrix (x_1 x_2 ... x_3 x_4 ...)
                        (
                         ((p_1 p_2 ... p_3 p_4 ... -> r_1) eqs_1 ...)
                         ...)
                        (pvar ...)
                        (pvar_3 ...)
                        natural
                        bool))
   (matrix (x_3 x_2 ... x_1 x_4 ...)
           (
            ((p_3 p_2 ... p_1 p_4 ... -> r_1) eqs_1 ...)
            ...)
           (pvar ...)
           (pvar_3 ...)
           natural
           bool)
   (side-condition
    (apply = 
           (append (list (length (term (x_2 ...)))
                         (length (term (x_2 ...))))
                   (map length (term ((p_2 ...) ...))))))
   (side-condition
    (apply = 
           (append (list (length (term (x_4 ...)))
                         (length (term (x_4 ...))))
                   (map length (term ((p_4 ...) ...))))))
   (side-condition (> (length (remove-duplicates (term (simplify (p_1 ...))))) (length (remove-duplicates (term (simplify (p_3 ...))))))))
  ((simple-swap any) any)
  )

; TEST CASES

(define ∅ (set))
(define singleton set)
(define ∪ set-union)

(test-->
 red
 (term
  (matrix (a b c)
          ()
          ()
          ()
          0
          #f))
 (term ∅))

#;(test-->
   red
   (term
    (matrix ()
            (('a 'b 1 -> 1))
            ()
            ()
            0
            #f))
   (term ∅))

(test-->
 red
 (term 
  (matrix () 
          (((-> 1))) 
          ()
          ()
          0
          #f))
 (term (∪ (singleton 1)
          (matrix () 
                  ()
                  ()
                  ()
                  0
                  #f))))

(test-->
 red
 (term (matrix (a) 
               (((wc -> 1))) 
               ()
               ()
               0
               #f))
 (term (∪ (singleton 1)
          (matrix (a) 
                  ()
                  ()
                  ()
                  0
                  #f))))

(test-->
 red
 (term (matrix (a b) 
               (((wc wc -> 1))
                ((wc 12 -> 2))) 
               ()
               ()
               0
               #f))
 (term (∪ (singleton 1)
          (matrix (a b) 
                  (((wc 12 -> 2)))
                  ()
                  ()
                  0
                  #f))))

(test-->
 red
 (term (matrix (x y z) 
               (((wc wc wc -> 1))
                ((wc wc wc -> 2))
                ((wc wc wc -> 3)))
               ()
               ()
               0
               #f))
 #;(term (matrix (y x z) 
                 ((wc wc wc -> 1)
                  (wc wc wc -> 2)
                  (wc wc wc -> 3))
                 ()
                 ))
 #;(term (matrix (z y x) 
                 ((wc wc wc -> 1)
                  (wc wc wc -> 2)
                  (wc wc wc -> 3))
                 ()))
 (term (∪ (singleton 1)
          (matrix (x y z) 
                  (((wc wc wc -> 2))
                   ((wc wc wc -> 3)))
                  ()
                  ()
                  0
                  #f))))

(test-->
 red
 (term (matrix (a b c) 
               (((wc 'y 'z -> 1))
                ((wc 'q 'r -> 2))
                ((wc 'b 'c -> 3)))
               ()
               ()
               0
               #f))
 #;(term (matrix (b a c) 
                 (('y wc 'z -> 1)
                  ('q wc 'r -> 2)
                  ('b wc 'c -> 3))
                 ()
                 ))
 (term (matrix (b c) 
               ((('y 'z -> 1))
                (('q 'r -> 2))
                (('b 'c -> 3)))
               ()
               ()
               0
               #f)))

(test-->
 red
 (term 
  (matrix (ee a b)
          ((('k 'a 'b -> 1))
           (((or 'ww 'ee) 'j 'k -> 2))
           (('d 'c 'e -> 3)))
          ()
          ()
          0
          #f))
 (term 
  (matrix (ee a b)
          ((('k 'a 'b -> 1))
           (('ww 'j 'k -> 2))
           (('ee 'j 'k -> 2))
           (('d 'c 'e -> 3)))
          ()
          ()
          0
          #f)))

#;(test-->
   red
   (term (matrix (a b)
                 ((px 1 2 -> 1)
                  (wc 3 4 -> 2))
                 ()))
   (term (∪ (let 
                ((px a)) 
              (matrix (b) 
                      ((1 2 -> 1)) 
                      (px))) 
            (matrix (a b) 
                    ((wc 3 4 -> 2)) 
                    ()))))

(test-->
 red
 (term (matrix (a b c)
               ((((lit-name px wc) 1 2 -> 1) (px #f))
                ((wc 3 4 -> 2)))
               ()
               ()
               0
               #f))
 (term (matrix (a b c)
               (((wc 1 2 -> 1) (px #f a))
                ((wc 3 4 -> 2)))
               ()
               ()
               0
               #f)))

(test-->
 red
 (term (matrix (a b c)
               ((((lit-name px lit-number) 1 2 -> 1) (px #f))
                (((lit-name py lit-variable) 3 4 -> 2) (py #f)))
               ()
               ()
               0
               #f))
 (term (matrix (a b c)
               (((lit-number 1 2 -> 1) (px #f a))
                (((lit-name py lit-variable) 3 4 -> 2) (py #f)))
               ()
               ()
               0
               #f)))

#;(test-->
   red
   (term (matrix (a b c)
                 (((lit-name px wc) 1 2 -> 1)
                  ((lit-name py wc) 3 4 -> 2))
                 (px)
                 ()
                 0
                 #f))
   (term (∪ (cond ((equal? px a) 
                   (matrix (a b c) 
                           ((wc 1 2 -> 1)) 
                           (px)
                           ()
                           0
                           #f)) 
                  (else ∅)) 
            (matrix (a b c) 
                    (((lit-name py wc) 3 4 -> 2)) 
                    (px)
                    ()
                    0
                    #f))))

(test-->
 red
 (term (matrix (x y z)
               ((('x 'y 'z -> 1))
                ((1 2 'h -> 2)))
               ()
               ()
               0
               #f))
 (term (∪ (cond ((eqv? 'x x) 
                 (matrix (y z) 
                         ((('y 'z -> 1))) 
                         ()
                         ()
                         0
                         #f))
                ((eqv? 1 x) 
                 (matrix (y z) 
                         (((2 'h -> 2)))
                         ()
                         ()
                         0
                         #f))
                (else ∅))
          (matrix (y z) 
                  () 
                  ()
                  ()
                  0
                  #f))))

(test-->
 red
 (term (matrix (a b c)
               ((((cons 'x (cons 'y '())) 'y 'z -> 1))
                ((1 2 'h -> 2))
                ((wc 9 'g -> 3)))
               ()
               ()
               0
               #f))
 (term (∪ (cond 
            ((cons? a) 
             (matrix ((car a) (cdr a) b c) 
                     ((('x (cons 'y '()) 'y 'z -> 1)))
                     ()
                     ()
                     0
                     #f)) 
            ((eqv? 1 a) 
             (matrix (b c) 
                     (((2 'h -> 2))) 
                     ()
                     ()
                     0
                     #f))
            (else ∅)) 
          (matrix (b c) 
                  (((9 'g -> 3))) 
                  ()
                  ()
                  0
                  #f))))

; Contexts
(test-->
 red
 (term 
  (∪ (matrix () 
             (((-> 1))) 
             ()
             ()
             0
             #f)))
 (term (∪
        (∪ (singleton 1)
           (matrix () 
                   ()
                   ()
                   ()
                   0
                   #f)))))

(test-->
 red
 (term 
  (let ((px 1)) (matrix () 
                        (((-> 1))) 
                        ()
                        ()
                        0
                        #f)))
 (term (let ((px 1))
         (∪ (singleton 1)
            (matrix () 
                    ()
                    ()
                    ()
                    0
                    #f)))))

(test-->
 red
 (term 
  (cond [(cons? a) (matrix () 
                           (((-> 1))) 
                           ()
                           ()
                           0
                           #f)]))
 (term 
  (cond [(cons? a) 
         (∪ (singleton 1)
            (matrix () 
                    ()
                    ()
                    ()
                    0
                    #f))])))

(test-->
 red
 (term 
  (cond [(cons? a) (matrix () 
                           (((-> 1))) 
                           ()
                           ()
                           0
                           #f)]))
 (term 
  (cond [(cons? a) 
         (∪ (singleton 1)
            (matrix () 
                    ()
                    ()
                    ()
                    0
                    #f))])))

(test-->
 red
 (term (cond [(cons? a)
              (singleton 1)]
             [(eqv? 1 a)
              (matrix () 
                      (((-> 2))) 
                      ()
                      ()
                      0
                      #f)]
             [(eqv? 2 b)
              (matrix () 
                      (((-> 3))) 
                      ()
                      ()
                      0
                      #f)]))
 (term (cond [(cons? a)
              (singleton 1)]
             [(eqv? 1 a)
              (∪ (singleton 2)
                 (matrix () 
                         ()
                         ()
                         ()
                         0
                         #f))]
             [(eqv? 2 b)
              (matrix () 
                      (((-> 3))) 
                      ()
                      ()
                      0
                      #f)])))                

(test-results)
