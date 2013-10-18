#lang scheme
(require redex/reduction-semantics)
(require racket/set)
(provide red detect-hole detect-hole2 hole-table Get-Free-Name-Patterns 
         Flush1
         Flushable?
         Build-Cond
         Cond-List
         simplify
         compile
         Get-Pvar)

(define hole-table (make-hash))

(define the-hole (term hole))
(define no-context #f)
(define in-context #t)
(define ∅ #f)
(define context-match (make-parameter no-context))
(define (variable-prefix? x y) 
  (let* ([prefix-str (symbol->string x)]
         [prefix-len (string-length prefix-str)])
    (and (symbol? y)
         (let ([str (symbol->string y)])
           (and ((string-length str) . >= . prefix-len)
                (string=? (substring str 0 prefix-len) prefix-str))))))
(define rev (λ (x) 
              (if (cons? x)
                  (reverse x)
                  x)))

(define new-var-list '())

(define-language L
  [e (cond [e e] ...)
     (let ((x e) ...) e)
     (e e ...)
     m
     b]
  [v (cond [v v] ...)
     (let ((x v) ...) v)
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
     variable-except?
     variable-prefix?
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
     elip
     'any
     not
     =
     term-let
     variable-not-otherwise-mentioned]
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
   (v ... Context e ...)
   hole]
  [m (matrix (x ...) (row ...) (pvar ...) (pvar ...) natural bool)]
  [row ((p ... -> r) eqs ...)]
  [eqs (pvar bool eq ...)]
  [elip ;,'... 
   (side-condition variable_1 (eq? (term variable_1) '...))]
  [eq id
      (= pvar)
      ;(car eq)
      ;(cdr eq)
      (cons eq eq)
      (plug eq eq)
      ; not used yet
      (append eq eq)
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
  (scw s
       cw)
  (s lit-number
     lit-natural
     lit-integer
     lit-real
     lit-string
     lit-variable
     (lit-variable-except id ...)
     (lit-variable-prefix id))
  (cw c
      wildcard)
  (wildcard wc)
  (c number
     string
     'variable
     '()
     cp
     #t
     #f)
  (cp (cons p p))
  (rep (repeat p p))
  (id variable)
  ((match-repeat z) variable-not-otherwise-mentioned)
  (pvar variable-not-otherwise-mentioned
        ; possibly (pvar variable-not-otherwise-mentioned)
        (pvar pvar)
        (pvar elip))
  (x (car x)
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
                 (begin r_1 
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
        (side-condition (not (term (Flushable? (eqs_1 ...)))))
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
                 (cond [(equal? pvar_11 x_1)
                        (matrix (x_1 x_2 ...)
                                (((p_3 p_5 ... -> r_2) eqs_1 ...))
                                (pvar_2 ...)
                                (pvar_3 ...)
                                natural
                                bool)]
                       ))
        (where pvar_11 (Get-Temp-Name pvar_1 (pvar_2 ...)))
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
                 (begin
                   (matrix (x_1 x_2 ...)
                           ((((repeat p_3 p_4) p_5 ... -> r_2) eqs_2 ...))
                           (pvar_1 ...)
                           (pvar_3 ...)
                           natural
                           bool)
                   (matrix (x_1 x_2 ...)
                           (((cw_1 p_2 ... -> r_1) eqs_1 ...)
                            ...
                            ((p_6 p_7 ... -> r_3) eqs_3 ...)
                            ...)
                           (pvar_1 ...)
                           (pvar_3 ...)
                           natural
                           bool)))
        (side-condition (or (> (length (term (((cw_1 p_2 ... -> r_1) eqs_1 ...) ...))) 0)
                            (> (length (term (((p_6 p_7 ... -> r_3) eqs_3 ...) ...))) 0)))
        repeat-split)
   
   ; Repeat
   ;    If a row in a matrix starts with (repeat p_1 p_2), first determine all the identifiers for name patterns in p_1. Call the ones which are not marked as bound in the matrix variable_1 ..., and the ones which are marked as bound in the matrix variable_2 ...
   ;    Seperate the matrix into the row with the repeat and all other rows, unioning the result together. Wrap the repeat row in a let expression to bind all the elements of variable_2 ... to fresh temporary variables. Inside this define a letrec called match-repeach as a function which takes a fresh variable z, variable_1 ..., and variable_2 ... as its arguments. variable_1 ... will be used to build up the bindings from inside the repeat, while variable_2 ... will be unwrapped to check that values already bound outside of the repeat match those inside the repeat.
   ;    Inside the function, union the results of two conditionals. The first is the "base case," where every element of variable_2 ... is equal to the empty list, and therefore the variable bound outside the repeat are correct inside the repeat. In this case, we return a matrix where the first input variable is z, the first pattern in the row is p_2, and the rest of the input/row are the same as before. We mark variable_1 ... as bound. This matrix is wrapped in a let expression which restores the values bound outside the repeat back from their temporary forms.
   ;    The second conditional checks if z and variable_2 ... are all cons?. If they are, it stores all the values for variable_1 ... as temporaries, then matches (car z) against p_1, with the righthand side equal to a call to match repeat with (cdr z), the cons of variable_1 ... the temporary values for variable_1 ..., and the cdr of all the elements of variable 2. (The new bindings are built up by one layer, and the ones bound outside of the repeat are "unwrapped" by one layer). In this one row, one pattern matrix, the natural indicating the depth of the repeat is incremented.
   ;    Finally, in the body of the letrec, all the elements of variable_1 ... are bound to empty, and there is the call (match-repeat x_1 variable_1 ... variable_2 ...).
   
   ;; what happens with the (pvar_3 ...) field of the matrix when you have a matrix from a pattern like this one:
   #;(((((name x number) (name x number)) ...)
       (((name x number) (name x number)) ...))
      ((((name x number) (name x number)) ...)
       (((name x number) (name x number)) ...)) ...)
   ;; or, generally, there are nested repeats where the pvar_3's are significant.
   
   (--> (in-hole Context
                 (matrix (x_1 x_2 ...)
                         ((((repeat p_3 p_4) p_5 ... -> r_2) eqs_2 ...))
                         (pvar_1 ...)
                         (pvar_3 ...) 
                         natural
                         bool))
        (in-hole Context
                 (letrec ((match-repeat
                           (λ (z any_binding_temp ... any_bound_temp ...)      
                             (begin
                               (when (and (null? any_bound_temp) ...)
                                 (Build-Let ;reverse
                                  (let ([pvar_11 any_binding_temp] ...)
                                    (matrix (z x_2 ...)
                                            (((p_4 p_5 ... -> r_2) ,@(term (Add-Repeat-Vars (eqs_2 ...) (pvar_11 ...) (any_binding_temp ...)))))
                                            (((Get-Pvar pvar_22) any_bound_temp) ...)
                                            (pvar_3 ...) ; fix this
                                            natural
                                            bool))
                                  (any_binding_temp ...)))
                               (when (and (cons? z) (cons? any_bound_temp) ...)
                                 (let ((carz (car z))
                                       (any_car_bound_temp (car any_bound_temp))
                                       ...)
                                   (matrix (carz)
                                           (((p_3 -> (match-repeat 
                                                      (cdr z)
                                                      (cons pvar_11 any_binding_temp) ...
                                                      (cdr any_bound_temp) ...))
                                             eqs_2 ...))
                                           (((Get-Pvar pvar_22) any_car_bound_temp) ...)
                                           (pvar_3 ...)
                                           ,(+ 1 (term natural))
                                           bool))
                                 )))))
                   (Build-Temp-Let natural
                                   (any_bound_temp ...) 
                                   (pvar_22 ...)
                                   (Build-Let-Empty natural ,(map (λ (x) (term (Get-Pvar ,x))) (term (any_binding_temp ...)))
                                                    ;,(if (zero? (term natural))
                                                    (match-repeat x_1 any_binding_temp ... any_bound_temp ...)
                                                    ;(term (match-repeat x_1 any_binding_temp ... (car any_bound_temp) ...))
                                                    ;)
                                                    )
                                   )
                   )
                 )
        (where (pvar_11 ...) ,(remove-duplicates (map (λ (x) (term (Get-Pvar ,x)))
                                                      (term 
                                                       (Get-Free-Name-Patterns
                                                        p_3 
                                                        ,(map (λ (x) (term (Get-Pvar ,x))) (append (term (Binding-Eqs (eqs_2 ...))) (term (True-Eqs (eqs_2 ...) ())) )) ())))))
        (where (pvar_22 ...) ,(begin
                                ;(printf "~a\n" (remove-duplicates (map (λ (x) (term (Get-Pvar ,x 0))) (append (term (Binding-Eqs (eqs_* ...))) (term (True-Eqs (eqs_* ...) ())))) ))
                                (remove-duplicates (map (λ (x) (term (Get-Pvar ,x 0))) (append (term (Binding-Eqs (eqs_2 ...))) (term (True-Eqs (eqs_2 ...) ())))) )
                                ))
        (fresh match-repeat)
        (fresh z)
        (fresh carz)
        (fresh ((any_binding_temp ...)
                (pvar_11 ...)))
        (fresh ((single_binding_temp ...)
                (pvar_11 ...)))
        (fresh ((any_bound_temp ...)
                (pvar_22 ...)))
        (fresh ((any_car_bound_temp ...)
                (pvar_22 ...)))
        (side-condition (not (term (Flushable? 
                                    (eqs_2 ...)))))
        Repeat)
   
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
    (side-condition (not (term (Flushable? 
                                    (eqs_2 ...)))))
    built-in-non-terminals)
   
   (--> (in-hole Context
                 (matrix (x_1 x_2 ...)
                         (((p_1 p_5 ... -> r_2) eqs_2 ...))
                         (pvar_1 ...)
                         (pvar_3 ...) 
                         natural
                         bool))
        (in-hole Context
                 (Flush1 
                  (eqs_2 ...)
                  (matrix (x_1 x_2 ...)
                                 (((p_1 p_5 ... -> r_2) eqs_2 ...))
                                 (pvar_1 ...)
                                 (pvar_3 ...) 
                                 natural
                                 bool)))
        (side-condition (term (Flushable? (eqs_2 ...))))
        flush1)
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
          (exact-integer? x) 
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
   (eqs ...)]
  [(Add-Repeat-Vars ((pvar bool eq ...) eqs_2 ...) (pvar_1 ... pvar_3 pvar_2 ...) (pvar_11 ... pvar_33 pvar_22 ...))
   (Add-Repeat-Vars (eqs_2 ... (pvar bool ,@(remove-duplicates (term (pvar_33 eq ...))))) (pvar_1 ... pvar_2 ...) (pvar_11 ... pvar_22 ...))
   (side-condition (equal? (term pvar_3) (term (Get-Pvar pvar))))
   (side-condition (and (equal? (length (term (pvar_1 ...))) (length (term (pvar_11 ...))))
                        (equal? (length (term (pvar_2 ...))) (length (term (pvar_22 ...))))))
   ]
  [(Add-Repeat-Vars (eqs_1 ... (pvar bool eq ...) eqs_2 ...) (pvar_3 pvar_1 ...) (pvar_33 pvar_11 ...))
   (Add-Repeat-Vars (eqs_1 ... eqs_2 ... (pvar bool ,@(remove-duplicates (term (pvar_33 eq ...))))) (pvar_1 ...) (pvar_11 ...))
   (side-condition (equal? (term pvar_3) (term (Get-Pvar pvar))))
   ]
  [(Add-Repeat-Vars (eqs ...) (pvar_1 ...) (pvar_11 ...))
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
  [(Minimize-Names (pvar pvar_2 ... pvar_3 pvar_4 ...))
   (pvar_* ...)
   (where (pvar_* ...) (Minimize-Names (pvar pvar_2 ... pvar_4 ...)))
   (side-condition (and (equal? (term (Get-Pvar pvar)) (term (Get-Pvar pvar_3))) (<= (term (Depth pvar)) (term (Depth pvar_3)) )))
   (side-condition (andmap (λ (x) (not (equal? (term (Get-Pvar pvar)) (term (Get-Pvar ,x))))) (term (pvar_2 ...))))]
  [(Minimize-Names (pvar pvar_2 ... pvar_3 pvar_4 ...))
   (pvar_* ...)
   (where (pvar_* ...) (Minimize-Names (pvar_3 pvar_2 ... pvar_4 ...)))
   (side-condition (and (equal? (term (Get-Pvar pvar)) (term (Get-Pvar pvar_3))) (> (term (Depth pvar)) (term (Depth pvar_3)) )))
   (side-condition (andmap (λ (x) (not (equal? (term (Get-Pvar pvar)) (term (Get-Pvar ,x))))) (term (pvar_2 ...))))]
  [(Minimize-Names (pvar pvar_2 ...))
   (pvar pvar_* ...)
   (where (pvar_* ...) (Minimize-Names (pvar_2 ...)))
   ]
  [(Minimize-Names ())
   ()]
  )

(define-metafunction L
  [(Depth (pvar elip))
   ,(+ 1 (term (Depth pvar)))]
  [(Depth pvar)
   0]
  )

(define-metafunction L 
  [(Get-Free-Name-Patterns (lit-in-hole p_1 p_2) (pvar_1 ...) (pvar_2 ...)) 
   (Minimize-Names ,(append (term (Get-Free-Name-Patterns p_1 (pvar_1 ...) ())) 
                            (append (term (Get-Free-Name-Patterns p_2 (pvar_1 ...) ()))
                                    (term (pvar_2 ...)))))]
  [(Get-Free-Name-Patterns (lit-hide-hole p_1) (pvar_1 ...) (pvar_2 ...))
   (Minimize-Names ,(append (term (Get-Free-Name-Patterns p_1 (pvar_1 ...) ())) (term (pvar_2 ...))))]
  [(Get-Free-Name-Patterns (or p_1 p_2) (pvar_1 ...) (pvar_2 ...)) 
   (Minimize-Names ,(append (term (Get-Free-Name-Patterns p_1 (pvar_1 ...) ())) 
                            (append (term (Get-Free-Name-Patterns p_2 (pvar_1 ...) ()))
                                    (term (pvar_2 ...)))))]
  [(Get-Free-Name-Patterns (cons p_1 p_2) (pvar_1 ...) (pvar_2 ...)) 
   (Minimize-Names ,(append (term (Get-Free-Name-Patterns p_1 (pvar_1 ...) ())) 
                            (append (term (Get-Free-Name-Patterns p_2 (pvar_1 ...) ()))
                                    (term (pvar_2 ...)))))]
  [(Get-Free-Name-Patterns (lit-name id p) (pvar_1 ...) (pvar_2 ...))
   (Minimize-Names ,(append (term (Get-Free-Name-Patterns p (pvar_1 ...) ())) (append (list (term id)) (term (pvar_2 ...)))))
   (side-condition (andmap (λ (x) (not (eqv? (term id) x))) (term (pvar_1 ...))))]
  [(Get-Free-Name-Patterns (repeat p_1 p_2) (pvar_1 ...) (pvar_2 ...))
   (Minimize-Names ,(append (map (λ (x) `(,x ...)) (remove-duplicates (term (Get-Free-Name-Patterns p_1 (pvar_1 ...) ()))))
                            (append (term (Get-Free-Name-Patterns p_2 (pvar_1 ...) ()))
                                    (term (pvar_2 ...)))))]
  [(Get-Free-Name-Patterns (lit-side-condition p_1 any) (pvar_1 ...) (pvar_2 ...))
   (Minimize-Names ,(append (term (Get-Free-Name-Patterns p_1 (pvar_1 ...) ())) (term (pvar_2 ...))))]
  [(Get-Free-Name-Patterns any (pvar_1 ...) (pvar_2 ...)) ()])

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
   (let ((pvar_1 (term any_2))
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
  [(Build-Temp-Cons (pvar_1 ...) (pvar_2 ...))
   ((cons pvar_1 pvar_2) ...)])

(define-metafunction L 
  [(Build-Cdr (pvar_1 ...))
   ((cdr pvar_1) ...)])

; first pass hole detection assumes non-terminals contain no holes
(define-metafunction L
  ((detect-hole natural lit-hole)
   ,(+ 1 (term natural)))
  ((detect-hole natural (nt id))
   0)
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

; detect holes using a table with a hole count for each non-terminal
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
  ((move-hole-op-inward op replace (repeat p_1 p_2))
   (repeat p_1 (move-hole-op-inward op replace p_2))
   (side-condition (eqv? 1 (term (detect-hole2 0 p_2)))))
  ((move-hole-op-inward op replace any)
   any)
  )

(define-metafunction L
  ((push-name-downward (lit-name pvar (cons p_1 p_2)) (eqs_1 ... (pvar bool eq_1 ...) eqs_2 ...) pvar_fresh1 pvar_fresh2)
   ((cons (lit-name pvar_fresh1 p_1) (lit-name pvar_fresh2 p_2)) (eqs_1 ... (pvar bool (cons pvar_fresh1 pvar_fresh2) eq_1 ...) (pvar_fresh1 #f) (pvar_fresh2 #f) eqs_2 ...)))
  ((push-name-downward (lit-name pvar lit-hole) (eqs_1 ... (pvar bool eq_1 ...) eqs_2 ...) pvar_fresh1 pvar_fresh2)
   (lit-hole (eqs_1 ... (pvar bool lit-hole eq_1 ...) eqs_2 ...)))
  ((push-name-downward (lit-name pvar (lit-name pvar_2 p_1)) (eqs_1 ... (pvar bool eq_1 ...) eqs_2 ...) pvar_fresh1 pvar_fresh2)
   ((lit-name pvar_2 p_1) (eqs_1 ... (pvar bool (= pvar_2) eq_1 ...) eqs_2 ...)))
  ((push-name-downward (lit-name pvar (lit-in-hole p_1 p_2)) (eqs_1 ... (pvar bool eq_1 ...) eqs_2 ...) pvar_fresh1 pvar_fresh2)
   ((lit-in-hole (lit-name pvar_fresh1 p_1) (lit-name pvar_fresh2 p_2)) (eqs_1 ... (pvar bool (plug pvar_fresh1 pvar_fresh2) eq_1 ...) (pvar_fresh1 #f) (pvar_fresh2 #f) eqs_2 ...)))
  ((push-name-downward (lit-name pvar (repeat p_1 p_2))  (eqs_1 ... (pvar bool eq_1 ...) eqs_2 ...) pvar_fresh1 pvar_fresh2)
   ((repeat (lit-name pvar_fresh1 p_1) (lit-name pvar_fresh2 p_2)) (eqs_1 ... (pvar bool (cons pvar_fresh1 pvar_fresh2) eq_1 ...) (pvar_fresh1 #f) (pvar_fresh2 #f) eqs_2 ...)))
  ((push-name-downward any_1 any_2 any_3 any_4)
   (any_1 any_2))
  )

; Replace (eqv? c x) with (another-function x) where appropriate.
(define-metafunction L
  [(Func-Replace (eqv? lit-number x)) (number? x)]
  [(Func-Replace (eqv? lit-natural x)) (natural? x)]
  [(Func-Replace (eqv? lit-integer x)) (exact-integer? x)]
  [(Func-Replace (eqv? lit-real x)) (real? x)]
  [(Func-Replace (eqv? lit-string x)) (string? x)]
  [(Func-Replace (eqv? lit-variable x)) (symbol? x)]
  [(Func-Replace (eqv? (lit-variable-except variable ...) x)) (and (symbol? x) (andmap (λ (y) (not (equal? y x))) (quote (variable ...))))]
  [(Func-Replace (eqv? (lit-variable-prefix variable) x)) (variable-prefix? 'variable x)]
  [(Func-Replace (eqv? string x)) (equal? string x)]
  [(Func-Replace (eqv? any x)) (eqv? any x)]
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
   (begin (cond
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
          )
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
   (begin (cond 
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
          )
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
  ((Get-Temp-Name pvar_1 ((pvar_1 pvar_2) pvar_3 ...))
   pvar_2)
  ((Get-Temp-Name pvar_1 ((pvar_1 pvar_2) pvar_3 ...))
   (Get-Temp-Name pvar (pvar_3 ...))
   (side-condition (not (equal? (term pvar) (term pvar_1)))))
  ((Get-Temp-Name pvar_1 (pvar_3 ...))
   ())
  )

; use eqs ... () eqs ... (where lookup)
(define-metafunction L
  ((Flushable? ((pvar bool eq_1 ... lit-hole eq_2 ...) eqs_2 ...))
   #t)
  ((Flushable? ((pvar bool eq_1 ... id eq_2 ...) eqs_2 ...))
   #t)
  ((Flushable? ((pvar bool eq ... (= pvar_3) eq_2 ...) eqs_2 ... (pvar_3 #t eq_3 ...) eqs_3 ...))
   #t)
  ((Flushable? ((pvar bool eq ... (cons pvar_1 pvar_3) eq_3 ...) eqs_2 ... (pvar_1 #t eq_1 ...) eqs_3 ... (pvar_3 #t eq_4 ...) eqs_4 ...))
   #t)
  ((Flushable? ((pvar bool eq ... (plug pvar_1 pvar_3) eq_3 ...) eqs_2 ... (pvar_1 #t eq_1 ...) eqs_3 ... (pvar_3 #t eq_4 ...) eqs_4 ...))
   #t)
  ((Flushable? ((pvar bool eq ... (cons pvar_1 pvar_3) eq_3 ...) eqs_2 ... (pvar_3 #t eq_1 ...) eqs_3 ... (pvar_1 #t eq_4 ...) eqs_4 ...))
   #t)
  ((Flushable? ((pvar bool eq ... (plug pvar_1 pvar_3) eq_3 ...) eqs_2 ... (pvar_3 #t eq_1 ...) eqs_3 ... (pvar_1 #t eq_4 ...) eqs_4 ...))
   #t)
  ((Flushable? (eqs_1 eqs_2 ...))
   (Flushable? (eqs_2 ...)))
  )

; takes matrix plus list to iterate on -> expression, or no
(define-metafunction L
  ((Flush1 ((pvar #f eq_1 ... lit-hole eq_2 ...) eqs_2 ...) 
           (matrix (x_11 ...) (((p_11 ... -> r) eqs_22 ...)) (pvar_11 ...) (pvar_22 ...) natural bool_0))
   (term-let ((pvar the-hole))
             (matrix (x_11 ...) (((p_11 ... -> r) eqs_2 ... (pvar #t eq_1 ... eq_2 ...))) (pvar_11 ...) (pvar_22 ...) natural bool_0))
   (side-condition (andmap not-a-lit-hole? (term (eq_1 ...)))))
  
  ((Flush1 ((pvar #t eq_1 ... lit-hole eq_2 ...) eqs_2 ...) 
           (matrix (x_11 ...) (((p_11 ... -> r) eqs_22 ...)) (pvar_11 ...) (pvar_22 ...) natural bool_0))
   (if (equal? (term pvar) the-hole)
       (matrix (x_11 ...) (((p_11 ... -> r) eqs_2 ... (pvar #t eq_1 ... eq_2 ...))) (pvar_11 ...) (pvar_22 ...) natural bool_0)
       ∅)
   (side-condition (andmap not-a-lit-hole? (term (eq_1 ...)))))
  
  ((Flush1 ((pvar #f eq_1 ... id eq_2 ...) eqs_2 ...) 
           (matrix (x_11 ...) (((p_11 ... -> r) eqs_22 ...)) (pvar_11 ...) (pvar_22 ...) natural bool_0))
   (term-let ((pvar id))
             (matrix (x_11 ...) (((p_11 ... -> r) eqs_2 ... (pvar #t eq_1 ... eq_2 ...))) (pvar_11 ...) (pvar_22 ...) natural bool_0))
   (side-condition (andmap not-an-id? (term (eq_1 ...)))))
  
  ((Flush1 ((pvar #t eq_1 ... id eq_2 ...) eqs_2 ...) 
           (matrix (x_11 ...) (((p_11 ... -> r) eqs_22 ...)) (pvar_11 ...) (pvar_22 ...) natural bool_0))
   (if (equal? (term pvar) id)
       (matrix (x_11 ...) (((p_11 ... -> r) eqs_2 ... (pvar #t eq_1 ... eq_2 ...))) (pvar_11 ...) (pvar_22 ...) natural bool_0)
       ∅)
   (side-condition (andmap not-an-id? (term (eq_1 ...)))))
  
  ((Flush1 ((pvar #f eq ... (= pvar_3) eq_2 ...) eqs_2 ... (pvar_3 #t eq_3 ...) eqs_3 ...) 
           (matrix (x_11 ...) (((p_11 ... -> r) eqs_22 ...)) (pvar_11 ...) (pvar_22 ...) natural bool_0))
   (term-let ((pvar (term pvar_3)))
             (matrix (x_11 ...) (((p_11 ... -> r) eqs_2 ... (pvar_3 #t eq_3 ...) eqs_3 ... (pvar #t eq ... eq_2 ...))) (pvar_11 ...) (pvar_22 ...) natural bool_0))
   (side-condition (andmap (λ (x) (not (redex-match L (= pvar_11) x))) (term (eq ...)))))
  
  ((Flush1 ((pvar #t eq ... (= pvar_3) eq_2 ...) eqs_2 ... (pvar_3 #t eq_3 ...) eqs_3 ...) 
           (matrix (x_11 ...) (((p_11 ... -> r) eqs_22 ...)) (pvar_11 ...) (pvar_22 ...) natural bool_0))
   (if (equal? (term pvar) (term pvar_3))
       (matrix (x_11 ...) (((p_11 ... -> r) eqs_2 ... (pvar_3 #t eq_3 ...) eqs_3 ... (pvar #t eq ... eq_2 ...))) (pvar_11 ...) (pvar_22 ...) natural bool_0)
       ∅)
   (side-condition (andmap (λ (x) (not (redex-match L (= pvar_11) x))) (term (eq ...)))))
  
  ((Flush1 ((pvar #t eq ... (cons pvar_1 pvar_3) eq_3 ...) eqs_2 ... (pvar_1 #t eq_1 ...) eqs_3 ... (pvar_3 #t eq_4 ...) eqs_4 ...) 
           (matrix (x_11 ...) (((p_11 ... -> r) eqs_22 ...)) (pvar_11 ...) (pvar_22 ...) natural bool_0))
   (if (and (cons? (term pvar))
            (equal? (car (term pvar)) (term pvar_1)) 
            (equal? (cdr (term pvar)) (term pvar_3)))
       (matrix (x_11 ...) (((p_11 ... -> r) eqs_2 ... (pvar_1 #t eq_1 ...) eqs_3 ... (pvar_3 #t eq_4 ...) eqs_4 ... (pvar #t eq ... eq_3 ...))) (pvar_11 ...) (pvar_22 ...) natural bool_0)
       ∅)
   (side-condition (and (term pvar)
                        (term pvar_1)
                        (term pvar_3)
                        (andmap (λ (x) (not (redex-match L (cons pvar_11 pvar_12) x))) (term (eq ...))))))
  
  ((Flush1 ((pvar #f eq ... (cons pvar_1 pvar_3) eq_3 ...) eqs_2 ... (pvar_1 #t eq_1 ...) eqs_3 ... (pvar_3 #t eq_4 ...) eqs_4 ...) 
           (matrix (x_11 ...) (((p_11 ... -> r) eqs_22 ...)) (pvar_11 ...) (pvar_22 ...) natural bool_0))
   (term-let ((pvar (cons (term pvar_1) (term pvar_3))))
             (matrix (x_11 ...) (((p_11 ... -> r) eqs_2 ... (pvar_1 #t eq_1 ...) eqs_3 ... (pvar_3 #t eq_4 ...) eqs_4 ... (pvar #t eq ... eq_3 ...))) (pvar_11 ...) (pvar_22 ...) natural bool_0))
   (side-condition (andmap (λ (x) (not (redex-match L (cons pvar_11 pvar_12) x))) (term (eq ...)))))
  
  ((Flush1 ((pvar #t eq ... (plug pvar_1 pvar_3) eq_3 ...) eqs_2 ... (pvar_1 #t eq_1 ...) eqs_3 ... (pvar_3 #t eq_4 ...) eqs_4 ...) 
           (matrix (x_11 ...) (((p_11 ... -> r) eqs_22 ...)) (pvar_11 ...) (pvar_22 ...) natural bool_0))
   (if (equal? (term pvar) (plug (term pvar_1) (term pvar_3)))
       (matrix (x_11 ...) (((p_11 ... -> r) eqs_2 ... (pvar_1 #t eq_1 ...) eqs_3 ... (pvar_3 #t eq_4 ...) eqs_4 ... (pvar #t eq ... eq_3 ...))) (pvar_11 ...) (pvar_22 ...) natural bool_0)
       ∅)
   (side-condition (andmap (λ (x) (not (redex-match L (plug pvar_11 pvar_12) x))) (term (eq ...)))))
  
  ((Flush1 ((pvar #f eq ... (plug pvar_1 pvar_3) eq_3 ...) eqs_2 ... (pvar_1 #t eq_1 ...) eqs_3 ... (pvar_3 #t eq_4 ...) eqs_4 ...) 
           (matrix (x_11 ...) (((p_11 ... -> r) eqs_22 ...)) (pvar_11 ...) (pvar_22 ...) natural bool_0))
   (term-let ((pvar (plug (term pvar_1) (term pvar_3))))
             (matrix (x_11 ...) (((p_11 ... -> r) eqs_2 ... (pvar_1 #t eq_1 ...) eqs_3 ... (pvar_3 #t eq_4 ...) eqs_4 ... (pvar #t eq ... eq_3 ...))) (pvar_11 ...) (pvar_22 ...) natural bool_0))
   (side-condition (andmap (λ (x) (not (redex-match L (plug pvar_11 pvar_12) x))) (term (eq ...)))))
  
  ((Flush1 ((pvar #t eq ... (cons pvar_1 pvar_3) eq_3 ...) eqs_2 ... (pvar_3 #t eq_1 ...) eqs_3 ... (pvar_1 #t eq_4 ...) eqs_4 ...) 
           (matrix (x_11 ...) (((p_11 ... -> r) eqs_22 ...)) (pvar_11 ...) (pvar_22 ...) natural bool_0))
   (if (and (cons? (term pvar))
            (equal? (car (term pvar)) (term pvar_1)) 
            (equal? (cdr (term pvar)) (term pvar_3)))
       (matrix (x_11 ...) (((p_11 ... -> r) eqs_2 ... (pvar_3 #t eq_1 ...) eqs_3 ... (pvar_1 #t eq_4 ...) eqs_4 ... (pvar #t eq ... eq_3 ...))) (pvar_11 ...) (pvar_22 ...) natural bool_0)
       ∅)
   (side-condition (and (term pvar)
                        (term pvar_1)
                        (term pvar_3)
                        (andmap (λ (x) (not (redex-match L (cons pvar_11 pvar_12) x))) (term (eq ...))))))
  
  ((Flush1 ((pvar #f eq ... (cons pvar_1 pvar_3) eq_3 ...) eqs_2 ... (pvar_3 #t eq_1 ...) eqs_3 ... (pvar_1 #t eq_4 ...) eqs_4 ...) 
           (matrix (x_11 ...) (((p_11 ... -> r) eqs_22 ...)) (pvar_11 ...) (pvar_22 ...) natural bool_0))
   (term-let ((pvar (cons (term pvar_1) (term pvar_3))))
             (matrix (x_11 ...) (((p_11 ... -> r) eqs_2 ... (pvar_3 #t eq_1 ...) eqs_3 ... (pvar_1 #t eq_4 ...) eqs_4 ... (pvar #t eq ... eq_3 ...))) (pvar_11 ...) (pvar_22 ...) natural bool_0))
   (side-condition (andmap (λ (x) (not (redex-match L (cons pvar_11 pvar_12) x))) (term (eq ...)))))
  
  ((Flush1 ((pvar #t eq ... (plug pvar_1 pvar_3) eq_3 ...) eqs_2 ... (pvar_3 #t eq_1 ...) eqs_3 ... (pvar_1 #t eq_4 ...) eqs_4 ...) 
           (matrix (x_11 ...) (((p_11 ... -> r) eqs_22 ...)) (pvar_11 ...) (pvar_22 ...) natural bool_0))
   (if (equal? (term pvar) (plug (term pvar_1) (term pvar_3)))
       (matrix (x_11 ...) (((p_11 ... -> r) eqs_2 ... (pvar_3 #t eq_1 ...) eqs_3 ... (pvar_1 #t eq_4 ...) eqs_4 ... (pvar #t eq ... eq_3 ...))) (pvar_11 ...) (pvar_22 ...) natural bool_0)
       ∅)
   (side-condition (andmap (λ (x) (not (redex-match L (plug pvar_11 pvar_12) x))) (term (eq ...)))))
  
  ((Flush1 ((pvar #f eq ... (plug pvar_1 pvar_3) eq_3 ...) eqs_2 ... (pvar_3 #t eq_1 ...) eqs_3 ... (pvar_1 #t eq_4 ...) eqs_4 ...) 
           (matrix (x_11 ...) (((p_11 ... -> r) eqs_22 ...)) (pvar_11 ...) (pvar_22 ...) natural bool_0))
   (term-let ((pvar (plug (term pvar_1) (term pvar_3))))
             (matrix (x_11 ...) (((p_11 ... -> r) eqs_2 ... (pvar_3 #t eq_1 ...) eqs_3 ... (pvar_1 #t eq_4 ...) eqs_4 ... (pvar #t eq ... eq_3 ...))) (pvar_11 ...) (pvar_22 ...) natural bool_0))
   (side-condition (andmap (λ (x) (not (redex-match L (plug pvar_11 pvar_12) x))) (term (eq ...)))))
  )


(define-namespace-anchor here)

(define/contract (compile m)
  (-> (redex-match L m) (-> any/c any/c))
  (eval `(λ (a) 
           (let ([results '()])
             ,(car (apply-reduction-relation* red m))
             results))
        (namespace-anchor->namespace here)))

(collect-garbage) (collect-garbage)
#;(time (void (apply-reduction-relation* red '(matrix (a) (
                                                           (((cons (repeat lit-number (repeat lit-number '())) (cons lit-string '())) -> 1))
                                                           )
                                                      ()
                                                      ()
                                                      0
                                                      #f))))
