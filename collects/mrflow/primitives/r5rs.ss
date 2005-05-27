; R5RS
; When are we going to be able to compute all this directly from an S-exp version of R5RS ?
(
 ; 4.2.6 quasiquotation
 
 ; not part of r5rs, but the expansion of ,@ uses qq-append
 (qq-append (forall ([b_append top][c_append top])
                    (case-lambda
                      [((listof b_append) c_append)
                       (union c_append
                              (rec-type
                               ([improper-list
                                 (union ()
                                        (cons b_append
                                              (union c_append improper-list)))])
                               improper-list))])))
 
 ; 6.1 Equivalence predicates
 
 (eqv? (top top -> boolean))
 (eq? (top top -> boolean))
 (equal? (top top -> boolean))
 
 
 ; 6.2.5 Numerical operations
 
 ; in Scheme it seems that positive = strictly positive and
 ; negative = strictly negative
 
 (number? (top -> boolean))
 (complex? (top -> boolean))
 (real? (top -> boolean))
 (rational? (top -> boolean))
 (integer? (top -> boolean))
 
 (exact? (complex -> boolean))
 (inexact? (complex -> boolean))
 
 (= (complex complex complex *-> boolean))
 (< (real real real *-> boolean))
 (> (real real real *-> boolean))
 (<= (real real real *-> boolean))
 (>= (real real real *-> boolean))
 
 (zero? (complex -> boolean))
 (positive? (real -> boolean))
 (negative? (real -> boolean))
 (odd? (integer -> boolean))
 (even? (integer -> boolean))
 
 ; if any arg inexact => result inexact
 (max (forall ([x_max real])
              (case-lambda
                [(x_max) x_max]
                [(rest real (listof real)) real])))
 ; if any arg inexact => result inexact
 (min (forall ([x_min real])
              (case-lambda
                [(x_min) x_min]
                [(rest real (listof real)) real])))
 
 ; no arg => 0
 ; z => z
 (+ (forall ([z_+ complex])
            (case-lambda
              [(rest complex complex (listof complex)) complex]
              [() 0]
              [(z_+) z_+]
              )))
 ; no arg => 1
 ; z => z
 (* (forall ([z_* complex])
            (case-lambda
              [(rest complex complex (listof complex)) complex]
              [() 1]
              [(z_*) z_*]
              )))
 
 ; z => -z
 (- (complex complex *-> complex))
 ; z => 1/z
 (/ (complex complex *-> complex))
 
 ; returns non-negative real
 (abs (real -> real))
 
 ; second arg non-zero
 (quotient (integer integer -> integer))
 ; second arg non-zero
 ; result has same sign as first arg
 (remainder (integer integer -> integer))
 ; second arg non-zero
 ; result has same sign as second arg
 (modulo (integer integer -> integer))
 
 ; no arg => 0
 ; n => n (from math)
 ; result is non-negative integer
 (gcd (forall ([n_gcd integer])
              (case-lambda
                [(rest integer integer (listof integer)) integer]
                [() 0]
                [(n_gcd) n_gcd]
                )))
 ; no arg => 1
 ; n => n (from math)
 ; result is non-negative integer
 (lcm (forall ([n_lcm integer])
              (case-lambda
                [(rest integer integer (listof integer)) integer]
                [() 1]
                [(n_lcm) n_lcm]
                )))
 
 (numerator (rational -> integer))
 ; result always positive
 ; 0 => 1
 (denominator (rational -> integer))
 
 (floor (real -> integer))
 (ceiling (real -> integer))
 (truncate (real -> integer))
 (round (real -> integer))
 
 (rationalize (real real -> rational))
 
 (exp (complex -> complex))
 (log (complex -> complex))
 (sin (complex -> complex))
 (cos (complex -> complex))
 (tan (complex -> complex))
 (asin (complex -> complex))
 (acos (complex -> complex))
 (atan (case-lambda
         [(complex) complex]
         [(real real) complex]))
 
 ; positive real part, or zero real part and non-negative imaginary part
 (sqrt (complex -> complex))
 
 ; (expt 0 0) = 1
 ; (expt 0 z) = 0
 (expt (complex complex -> complex))
 
 (make-rectangular (real real -> complex))
 (make-polar (real real -> complex))
 (real-part (complex -> real))
 (imag-part (complex -> real))
 ; returns non-negative real
 (magnitude (complex -> real))
 (angle (complex -> real))
 
 (exact->inexact (complex -> inexact-complex))
 (inexact->exact (complex -> exact-complex))
 
 
 ; 6.2.6 Numerical input and output
 
 ; this really ougth to be called complex->string and string->complex,
 ; especially since R5RS explicitely uses a "z" as the first argument
 ; name... R5RS seems to actually confuse complex and number quite a lot,
 ; despite the second note in section 6.2.5, page 21.
 
 ; radix is either 2, 8, 10, or 16
 (number->string (case-lambda
                   [(complex) string]
                   [(complex exact-integer) string]))
 
 ; radix is either 2, 8, 10, or 16
 (string->number (case-lambda
                   [(string) (union complex #f)]
                   [(string exact-integer) (union complex #f)]))
 
 
 ; 6.3.1 Booleans
 
 (not (boolean -> boolean))
 
 (boolean? (top -> boolean))
 
 
 ; 6.3.2 Pairs and lists
 
 (pair? (top -> boolean))
 
 (cons (forall ([a_cons top]
                [b_cons top])
               (a_cons b_cons -> (cons a_cons b_cons))))
 
 (car (forall ([a_car top])
              ((cons a_car top) -> a_car)))
 
 (cdr (forall ([a_cdr top])
              ((cons top a_cdr) -> a_cdr)))
 
 ; b can't be twice in contra-variant position...
 ;(set-car! (forall ([a top][b top][c top])
 ;                  (cons (union a b) c) b -> void
 
 ;(set-cdr! (forall ([a top][b top][c top])
 ;                  (cons a (union b c)) b -> void
 
 (caar (forall ([a_caar top])
               ((cons (cons a_caar top) top) -> a_caar)))
 (cdar (forall ([a_cdar top])
               ((cons (cons top a_cdar) top) -> a_cdar)))
 (cadr (forall ([a_cadr top])
               ((cons top (cons a_cadr top)) -> a_cadr)))
 (cddr (forall ([a_cddr top])
               ((cons top (cons top a_cddr)) -> a_cddr)))
 (caaar (forall ([a_caaar top])
                ((cons (cons (cons a_caaar top) top) top) -> a_caaar)))
 (cdaar (forall ([a_cdaar top])
                ((cons (cons (cons top a_cdaar) top) top) -> a_cdaar)))
 (cadar (forall ([a_cadar top])
                ((cons (cons top (cons a_cadar top)) top) -> a_cadar)))
 (cddar (forall ([a_cddar top])
                ((cons (cons top (cons top a_cddar)) top) -> a_cddar)))
 (caadr (forall ([a_caadr top])
                ((cons top (cons (cons a_caadr top) top)) -> a_caadr)))
 (cdadr (forall ([a_cdadr top])
                ((cons top (cons (cons top a_cdadr) top)) -> a_cdadr)))
 (caddr (forall ([a_caddr top])
                ((cons top (cons top (cons a_caddr top))) -> a_caddr)))
 (cdddr (forall ([a_cdddr top])
                ((cons top (cons top (cons top a_cdddr))) -> a_cdddr)))
 (caaaar (forall ([a_caaaar top])
                 ((cons (cons (cons (cons a_caaaar top) top) top) top) -> a_caaaar)))
 (cdaaar (forall ([a_cdaaar top])
                 ((cons (cons (cons (cons top a_cdaaar) top) top) top) -> a_cdaaar)))
 (cadaar (forall ([a_cadaar top])
                 ((cons (cons (cons top (cons a_cadaar top)) top) top) -> a_cadaar)))
 (cddaar (forall ([a_cddaar top])
                 ((cons (cons (cons top (cons top a_cddaar)) top) top) -> a_cddaar)))
 (caadar (forall ([a_caadar top])
                 ((cons (cons top (cons (cons a_caadar top) top)) top) -> a_caadar)))
 (cdadar (forall ([a_cdadar top])
                 ((cons (cons top (cons (cons top a_cdadar) top)) top) -> a_cdadar)))
 (caddar (forall ([a_caddar top])
                 ((cons (cons top (cons top (cons a_caddar top))) top) -> a_caddar)))
 (cdddar (forall ([a_cdddar top])
                 ((cons (cons top (cons top (cons top a_cdddar))) top) -> a_cdddar)))
 (caaadr (forall ([a_caaadr top])
                 ((cons top (cons (cons (cons a_caaadr top) top) top)) -> a_caaadr)))
 (cdaadr (forall ([a_cdaadr top])
                 ((cons top (cons (cons (cons top a_cdaadr) top) top)) -> a_cdaadr)))
 (cadadr (forall ([a_cadadr top])
                 ((cons top (cons (cons top (cons a_cadadr top)) top)) -> a_cadadr)))
 (cddadr (forall ([a_cddadr top])
                 ((cons top (cons (cons top (cons top a_cddadr)) top)) -> a_cddadr)))
 (caaddr (forall ([a_caaddr top])
                 ((cons top (cons top (cons (cons a_caaddr top) top))) -> a_caaddr)))
 (cdaddr (forall ([a_cdaddr top])
                 ((cons top (cons top (cons (cons top a_cdaddr) top))) -> a_cdaddr)))
 (cadddr (forall ([a_cadddr top])
                 ((cons top (cons top (cons top (cons a_cadddr top)))) -> a_cadddr)))
 (cddddr (forall ([a_cddddr top])
                 ((cons top (cons top (cons top (cons top a_cddddr)))) -> a_cddddr)))
 
 (null? (top -> boolean))
 
 (list? (top -> boolean))
 
 ; the rest argument does all the work
 (list (forall ([a_list top])
               (case-lambda
                 [(rest a_list) a_list])))
 
 (length ((listof top) -> exact-integer))
 
 (append (forall ([a_append top]
                  [b_append top][c_append top]
                  [d_append top][e_append top][f_append top]
                  [g_append top][h_append top][i_append top][j_append top]
                  [k_append top][l_append top][m_append top][n_append top][o_append top]
                  [p_append top][q_append top][r_append top][s_append top][t_append top][u_append top])
                 (case-lambda
                   [() ()]
                   [(a_append) a_append]
                   [((listof b_append) c_append)
                    (union c_append
                           (rec-type
                            ([improper-list
                              (union ()
                                     (cons b_append
                                           (union c_append improper-list)))])
                            improper-list))]
                   [((listof d_append) (listof e_append) f_append)
                    (union f_append
                           (rec-type
                            ([improper-list
                              (union ()
                                     (cons (union d_append e_append)
                                           (union f_append improper-list)))])
                            improper-list))]
                   [((listof g_append) (listof h_append) (listof i_append) j_append)
                    (union j_append
                           (rec-type
                            ([improper-list
                              (union ()
                                     (cons (union g_append h_append i_append)
                                           (union j_append improper-list)))])
                            improper-list))]
                   [((listof k_append) (listof l_append) (listof m_append) (listof n_append) o_append)
                    (union o_append
                           (rec-type
                            ([improper-list
                              (union ()
                                     (cons (union k_append l_append m_append n_append)
                                           (union o_append improper-list)))])
                            improper-list))]
                   [((listof p_append) (listof q_append) (listof r_append) (listof s_append) (listof t_append) u_append)
                    (union u_append
                           (rec-type
                            ([improper-list
                              (union ()
                                     (cons (union p_append q_append r_append s_append t_append)
                                           (union u_append improper-list)))])
                            improper-list))])))
 ; the last element could be not a list => improper list
 ; this doesn't work because it doesn't enforce the listness of args beyond
 ; the first one but before the last one...
 ;[(rest (listof b_append) (listof c_append))
 ; (rec-type ([improper-list (union ()
 ;                                  (cons (union b_append c_append)
 ;                                        (union c_append improper-list)))])
 ;           improper-list)]
 ;(union c_append (listof b_append))]
 ;)))
 
 (reverse (forall ([a_reverse top])
                  ((listof a_reverse) -> (listof a_reverse))))
 
 ; exact-integer should be non-negative...
 (list-tail (forall ([a_list-tail top])
                    ((listof a_list-tail) exact-integer -> (listof a_list-tail))))
 
 (list-ref (forall ([a_list-ref top])
                   ((listof a_list-ref) exact-integer -> a_list-ref)))
 
 (memq (forall ([a_memq top]
                [b_memq top])
               (a_memq (listof b_memq) -> (union #f (cons a_memq (listof b_memq))))))
 (memv (forall ([a_memv top]
                [b_memv top])
               (a_memv (listof b_memv) -> (union #f (cons a_memv (listof b_memv))))))
 (member (forall ([a_member top]
                  [b_member top])
                 (a_member (listof b_member) -> (union #f (cons a_member (listof b_member))))))
 
 (assq (forall ([a_assq top]
                [b_assq top])
               (a_assq (listof (cons top b_assq)) -> (union #f (cons a_assq b_assq)))))
 (assv (forall ([a_assv top]
                [b_assv top])
               (a_assv (listof (cons top b_assv)) -> (union #f (cons a_assv b_assv)))))
 (assoc (forall ([a_assoc top]
                 [b_assoc top])
                (a_assoc (listof (cons top b_assoc)) -> (union #f (cons a_assoc b_assoc)))))
 
 
 ; 6.3.3. Symbols
 
 (symbol? (top -> boolean))
 
 (symbol->string (symbol -> string))
 
 (string->symbol (string -> symbol))
 
 
 ; 6.3.4 Characters
 
 (char? (top -> boolean))
 
 (char=? (char char -> boolean))
 (char<? (char char -> boolean))
 (char>? (char char -> boolean))
 (char<=? (char char -> boolean))
 (char>=? (char char -> boolean))
 
 (char-ci=? (char char -> boolean))
 (char-ci<? (char char -> boolean))
 (char-ci>? (char char -> boolean))
 (char-ci<=? (char char -> boolean))
 (char-ci>=? (char char -> boolean))
 
 (char-alphabetic? (char -> boolean))
 (char-numeric? (char -> boolean))
 (char-whitespace? (char -> boolean))
 (char-upper-case? (letter -> boolean))
 (char-lower-case? (letter -> boolean))
 
 ; R5RS doesn't say the integer has to be positive...
 (char->integer (char -> exact-integer))
 (integer->char (exact-integer -> char))
 
 (char-upcase (char -> char))
 (char-downcase (char -> char))
 
 
 ; 6.3.5 Strings
 
 (string? (top -> boolean))
 
 ; integer should be non-negative
 (make-string (case-lambda
                [(exact-integer) string]
                [(exact-integer char) string]))
 
 (string (case-lambda
           [(rest char (listof char)) string]
           [() ""]
           ))
 
 ; exact positive integer ? exact integer ? integer ? 
 (string-length (string -> exact-integer))
 
 (string-ref (string exact-integer -> char))
 
 ; should inject string into the first arg
 ;(string-set! (string exact-integer char -> void))
 
 (string=? (string string -> boolean))
 (string-ci=? (string string -> boolean))
 
 (string<? (string string -> boolean))
 (string>? (string string -> boolean))
 (string<=? (string string -> boolean))
 (string>=? (string string -> boolean))
 (string-ci<? (string string -> boolean))
 (string-ci>? (string string -> boolean))
 (string-ci<=? (string string -> boolean))
 (string-ci>=? (string string -> boolean))
 
 (substring (string exact-integer exact-integer -> string))
 
 (string-append (forall ([a_string-append string])
                        (case-lambda
                          [(rest string string (listof string)) string]
                          [() ""]
                          [(a_string-append) a_string-append]
                          )))
 
 (string->list (string -> (listof char)))
 (list->string ((listof char) -> string))
 
 ; (string-copy (forall ([a string]) (a -> a))) works only if we don't have string-set!
 (string-copy (string -> string))
 
 ; should inject string into first arg
 ;(string-fill! (string char -> void))
 
 
 ; 6.3.6 Vectors
 
 (vector? (top -> boolean))
 
 ; integer should be non-negative
 (make-vector (forall ([a_make-vector top])
                      (case-lambda
                        [(exact-integer) (vector top)]
                        [(exact-integer a_make-vector) (vector a_make-vector)])))
 
 (vector (forall ([a_vector top])
                 (a_vector *-> (vector a_vector))))
 
 (vector-length ((vector top) -> exact-integer))
 
 (vector-ref (forall ([a_vector-ref top])
                     ((vector a_vector-ref) exact-integer -> a_vector-ref)))
 
 ; should  inject third arg into first
 ;(vector-set! (vector exact-integer top -> void))
 
 (vector->list (forall ([a_vector->list top])
                       ((vector a_vector->list) -> (listof a_vector->list))))
 (list->vector (forall ([a_list->vector top])
                       ((listof a_list->vector) -> (vector a_list->vector))))
 
 ; second arg shoould flow into first
 ;(vector-fill! (vector top -> void))
 
 
 ; 6.4 Control features
 
 (procedure? (top -> boolean))
 
 (apply (forall ([a_apply top][b_apply top]
                 [c_apply top][d_apply top][e_apply top]
                 [f_apply top][g_apply top][h_apply top][i_apply top]
                 [j_apply top][k_apply top][l_apply top][m_apply top][n_apply top]
                 [o_apply top][p_apply top][q_apply top][r_apply top][s_apply top][t_apply top]
                 [u_apply top][v_apply top][w_apply top][x_apply top][y_apply top][z_apply top][aa_apply top]
                 ;[ab_apply top][ac_apply top]
                 )
                (case-lambda
                  [((case-lambda [(rest (listof a_apply)) b_apply])
                    (listof a_apply)) b_apply]
                  [((case-lambda [(rest (listof (union c_apply d_apply))) e_apply])
                    c_apply (listof d_apply)) e_apply]
                  [((case-lambda [(rest (listof (union f_apply g_apply h_apply))) i_apply])
                    f_apply g_apply (listof h_apply)) i_apply]
                  [((case-lambda [(rest (listof (union j_apply k_apply l_apply m_apply))) n_apply])
                    j_apply k_apply l_apply (listof m_apply)) n_apply]
                  [((case-lambda [(rest (listof (union o_apply p_apply q_apply r_apply s_apply))) t_apply])
                    o_apply p_apply q_apply r_apply (listof s_apply)) t_apply]
                  [((case-lambda [(rest (listof (union u_apply v_apply w_apply x_apply y_apply z_apply))) aa_apply])
                    u_apply v_apply w_apply x_apply y_apply (listof z_apply)) aa_apply]
                  ; this would almost work, except for the last argument, that would
                  ; show up as a list in the result
                  ;[(rest (case-lambda
                  ;        [(rest a) b])
                  ;       a)
                  ; b])))
                  ; so we have to deconstruct everything, and be *very* conservative.
                  ; This *will* raise errors about possible infinite lists, but that's the
                  ; best we can if we want to cover all the possible cases.
                  ; this will not work because it doesn't allow for the first args to not
                  ; be lists
                  ;[(rest (case-lambda
                  ;         [(rest (listof ab_apply)) ac_apply])
                  ;       (listof (listof ab_apply)))
                  ; ac_apply])))
                  ; and this doesn't work either because it allows for the last arg
                  ; to not be a list
                  ;[(rest (case-lambda
                  ;         [(rest (listof (union a_apply (listof b_apply)))) c_apply])
                  ;       (listof (union a_apply (listof b_apply))))
                  ; c_apply])))
                  )))
 
 (map (forall ([a_map top][b_map top]
               [c_map top][d_map top][e_map top]
               [f_map top][g_map top][h_map top][i_map top]
               [j_map top][k_map top][l_map top][m_map top][n_map top]
               [o_map top][p_map top][q_map top][r_map top][s_map top][t_map top]
               )
              (case-lambda
                [((a_map -> b_map) (listof a_map)) 
                 (listof b_map)]
                [((c_map d_map -> e_map) (listof c_map) (listof d_map))
                 (listof e_map)]
                [((f_map g_map h_map -> i_map) (listof f_map) (listof g_map) (listof h_map))
                 (listof i_map)]
                [((j_map k_map l_map m_map -> n_map) (listof j_map) (listof k_map) (listof l_map) (listof m_map))
                 (listof n_map)]
                [((o_map p_map q_map r_map s_map -> t_map) (listof o_map) (listof p_map) (listof q_map) (listof r_map) (listof s_map)) 
                 (listof t_map)]
                ; use at your own risks: you'll loose arity checking and get spurious errors
                ; about '() not being a pair or about infinite lists (but the result of map
                ; will be properly conservative, so if you ignore the errors for map itself
                ; and make sure the arity of the function given to map is correct, then you
                ; might be able to use the output of map to detect errors down the flow - except
                ; that the output of map will be a list => using car on it or stuff like that
                ; will trigger another error...)
                ; The whole problem is that map needs a dependent type...
                ;[(rest
                ;  (case-lambda
                ;   [(rest o p q r (listof s)) t])
                ;  (listof o) (listof p) (listof q) (listof r) (listof (listof s)))
                ; (listof t)]
                )))
 
 (for-each (forall ([a_for-each top];[b top]
                    [c_for-each top][d_for-each top];[e top]
                    [f_for-each top][g_for-each top][h_for-each top];[i top]
                    [j_for-each top][k_for-each top][l_for-each top][m_for-each top];[n top]
                    [o_for-each top][p_for-each top][q_for-each top][r_for-each top][s_for-each top];[t top]
                    )
                   (case-lambda
                     [((a_for-each -> top) (listof a_for-each))
                      void]
                     [((c_for-each d_for-each -> top) (listof c_for-each) (listof d_for-each))
                      void]
                     [((f_for-each g_for-each h_for-each -> top) (listof f_for-each) (listof g_for-each) (listof h_for-each))
                      void]
                     [((j_for-each k_for-each l_for-each m_for-each -> top) (listof j_for-each) (listof k_for-each) (listof l_for-each) (listof m_for-each))
                      void]
                     [((o_for-each p_for-each q_for-each r_for-each s_for-each -> top) (listof o_for-each) (listof p_for-each) (listof q_for-each) (listof r_for-each) (listof s_for-each)) 
                      void]
                     ; use at your own risks: you'll loose arity checking and get spurious errors
                     ; about '() not being a pair or about infinite lists (but the result of for-each
                     ; will be properly conservative)
                     ; The whole problem is that for-each needs a dependent type...
                     ;[(rest
                     ;  (case-lambda
                     ;   [(rest o p q r (listof s)) top])
                     ;  (listof o) (listof p) (listof q) (listof r) (listof (listof s)))
                     ; void]
                     )))
 
 ; (delay expr) => (#%app make-promise (lambda () expr))
 ; if we have the arrow type in the argument of make-promise, then the application
 ; will happen immediately, which we don't want. So instead
 ; a will be the thunk, and having the arrow type for this thunk in the type for force
 ; will force the application of the thunk inside force.
 ; pp-type for promises just "forgets" to show the enclosing thunk part of the type.
 ; It's ugly, but it works, and it works well enough to approximate memoization.
 (make-promise (forall ([a_make-promise top])
                       (a_make-promise -> (promise a_make-promise))))
 (force (forall ([a_force top])
                ((promise (-> a_force)) -> a_force)))
 
 (call-with-current-continuation (forall ([a_call/cc top]
                                          [b_call/cc top])
                                         (((a_call/cc -> bottom) -> b_call/cc)
                                          -> (union a_call/cc b_call/cc))))
 
 ; correct, but currently triggers a bug.
 ;(call-with-current-continuation (forall ([a top]
 ;                                         [b top])
 ;                                        ((; continuation
 ;                                          (case-lambda [(rest a) bottom])
 ;                                          ;result of body of lambda that
 ;                                          ;receives the continuation
 ;                                          -> b)
 ;                                         ; result of call/cc
 ;                                         -> (union (values a) b))))
 
 ; multiple values are simulated internally as a list...
 (values (forall ([a_values top])
                 (case-lambda
                   [(rest a_values) (values a_values)]
                   )))
 
 (call-with-values (forall ([a_call/vals top] ; one or multiple values
                            [b_call/vals top])
                           (case-lambda
                             [((case-lambda
                                 [() (values a_call/vals)])
                               (case-lambda
                                 [(rest a_call/vals) b_call/vals]))
                              b_call/vals])))
 
 ; this limited values works fine, but then call-with-values doesnt', because all the clauses
 ; in call-with-values would have only two arguments, making discrimination between the different
 ; cases impossible.
 ;(values (forall ([a top]
 ;                 [b top][c top]
 ;                 [d top][e top][f top]
 ;                 [g top][h top][i top][j top]
 ;                 [k top][l top][m top][n top][o top])
 ;                (case-lambda
 ;                 [() (values)]
 ;                 [(a) (values a)]
 ;                 [(b c) (values b c)]
 ;                 [(d e f) (values d e f)]
 ;                 [(g h i j) (values g h i j)]
 ;                 [(k l m n o) (values k l m n o)])))
 ;
 ;(call-with-values (forall ([a top][b top]
 ;                           [c top][d top][e top])
 ;                          (case-lambda
 ;                           [((case-lambda [() (values a)]) (case-lambda [(a) b])) b]
 ;                           [((case-lambda [() (values c d)]) (case-lambda [(c d) e])) e]
 ;                           )))
 
 (dynamic-wind (forall ([a_dyn/w top])
                       ((-> top) (-> a_dyn/w) (-> top) -> a_dyn/w)))
 
 
 ; 6.5 Eval
 
 ; letter is a subtype of char, all the number types are subtypes of number
 ; see section 7.1.2 of R5RS for the complete definition of datum
 (eval ((rec-type ([datum (union simple-datum compound-datum)]
                   [simple-datum (union boolean number char string symbol)]
                   ;[compound-datum (union list-datum vector-datum)]
                   [compound-datum (union list-datum (vector datum))]
                   [list-datum (union ()
                                      (cons datum list-datum)
                                      (cons datum datum))]
                   ;[vector-datum (vector datum)]
                   )
                  datum) env -> (union top #f))) 
 
 (scheme-report-environment (5 -> env))
 (null-environment (5 -> env))
 
 (interaction-environment (-> env))
 
 
 ; 6.6.1 Ports
 
 ; R5RS doesn't always explicitely differentiate between input and output ports...
 
 (call-with-input-file (forall ([a_call/if top])
                               (string (input-port -> a_call/if) -> a_call/if)))
 (call-with-output-file (forall ([a_call/of top])
                                (string (output-port -> a_call/of) -> a_call/of)))
 
 (input-port? (top -> boolean))
 (output-port? (top -> boolean))
 
 (current-input-port (-> input-port))
 (current-output-port (-> output-port))
 
 (with-input-from-file (forall ([a_with/if top])
                               (string (-> a_with/if) -> a_with/if)))
 (with-output-to-file (forall ([a_with/of top])
                              (string (-> a_with/of) -> a_with/of)))
 
 (open-input-file (string -> input-port))
 
 (open-output-file (string -> output-port))
 
 (close-input-port (input-port -> void))
 (close-output-port (output-port -> void))
 
 
 ; 6.6.2 Input
 
 ; eof is included in top, but #f needs to be included explicitely
 ; because of the simplistic way if-dependency is done
 (read (case-lambda
         [() (union top #f)]
         [(input-port) (union top #f)]))
 
 (read-char (case-lambda
              [() char]
              [(input-port) char]))
 
 (peek-char (case-lambda
              [() char]
              [(input-port) char]))
 
 (eof-object? (top -> boolean))
 
 (char-ready? (case-lambda
                [() boolean]
                [(input-port) boolean]))
 
 
 ; 6.6.3 Output
 
 (write (case-lambda
          [(top) void]
          [(top output-port) void]))
 
 (display (case-lambda
            [(top) void]
            [(top output-port) void]))
 
 (newline (case-lambda
            [() void]
            [(output-port) void]))
 
 (write-char (case-lambda
               [(char) void]
               [(char output-port) void]))
 
 
 ; 6.6.4 System interface
 
 (load (string -> (union top #f)))
 
 (transcript-on (string -> void))
 (transcript-off (-> void))
 
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; not R5RS, just for testing

 ; (if test then) macro-expanded into (if test then (void))
 (void (-> void))
 
 (null ())
 
; (id (forall ([a_id top]) (a_id -> a_id)))
; (make-func (-> (-> 1)))
; (foo (cons 1 2))
; (pi 3.1)
; ; one required argument that has to be a list, the elements are then extracted
; (gather-one1 (forall ([a_go1 top])
;                      ((listof a_go1) -> a_go1)))
; (gather-one2 (forall ([a_go2 top])
;                      (case-lambda
;                        [((listof a_go2)) a_go2])))
; ; unknown number of arguments that are converted into a list by the rest argument,
; ; then extracted
; (gather-many1 (forall ([a_gm1 top])
;                       (a_gm1 *-> a_gm1)))
; (gather-many2 (forall ([a_gm2 top])
;                       (case-lambda
;                         [(rest (listof a_gm2)) a_gm2])))
; ; don't try this at home
; ;(gather-other (forall ([a top])
; ;                      ((a) *-> a)))
; 
; (gen-nums (-> (listof number)))
; 
; (apply-gen (forall ([a_app/gen top]
;                     [b_app/gen top])
;                    (case-lambda
;                      [((case-lambda [(rest a_app/gen) b_app/gen]) a_app/gen) b_app/gen])))
; 
; (lnum (forall ([a_lnum top])
;               ((listof a_lnum) -> a_lnum)))
 
; ; ALGOL60 primitives and runtime, to be able to analyze the expanded version
; 
; (!= (number number -> boolean))
; (! (boolean -> boolean))
; (& (boolean boolean -> boolean))
; (\| (boolean boolean -> boolean))
; (=> (boolean boolean -> boolean))
; (== (boolean boolean -> boolean))
; 
; (sign (forall ([a top])
;               ((real -> a) (-> real) -> a)))
; (entier (forall ([a top])
;                 ((real -> a) (-> real) -> a)))
; 
; (a60:sin (forall ([a top])
;                  ((real -> a) (-> real) -> a)))
; (a60:cos (forall ([a top])
;                  ((real -> a) (-> real) -> a)))
; (a60:arctan (forall ([a top])
;                     ((real -> a) (-> real) -> a)))
; (a60:sqrt (forall ([a top])
;                   ((real -> a) (-> real) -> a)))
; (a60:abs (forall ([a top])
;                  ((real -> a) (-> real) -> a)))
; (a60:ln (forall ([a top])
;                 ((real -> a) (-> real) -> a)))
; (a60:exp (forall ([a top])
;                  ((real -> a) (-> real) -> a)))
;
; (prints (forall ([a top])
;                 ((void -> a) (-> top) -> a)))
; (printn (forall ([a top])
;                 ((void -> a) (-> top) -> a)))
; (printsln (forall ([a top])
;                   ((void -> a) (-> top) -> a)))
; (printnln (forall ([a top])
;                   ((void -> a) (-> top) -> a)))
; 
; ; Algol60 runtime support
; 
; ;(a60:array (struct a60:array (dependant type)))
; ;(a60:switch (struct a60:switch (choices))
; 
; (undefined undefined)
; 
; (check-boolean (forall ([a top]) (a -> a)))
; (goto (forall ([a top]) ((-> a) -> a)))
; (get-value (forall ([a top]) ((-> a) -> a)))
; (set-target! (forall ([a top][b top])
;                      ((a -> b) a -> b)))
; ;make-array
; ;array-ref
; ;array-set!
; ;make-switch
; ;switch-ref
; 
; (coerce (forall ([a top])
;                 (symbol a -> a)))
; 
; 
; ; R5RS runtime support
; 
; (void (-> void))
; 
; (= (real real -> boolean))
; (< (real real -> boolean))
; (> (real real -> boolean))
; (<= (real real -> boolean))
; (>= (real real -> boolean))
;
; (+ (real real -> real)) 
; (* (real real -> real)) 
; (- (real real -> real))
; (/ (real real -> real))
; 
; (quotient (integer integer -> integer))
; (remainder (integer integer -> integer))
; (modulo (integer integer -> integer))
; 
; (values (forall ([a_values top])
;                 (case-lambda
;                   [(rest a_values) (values a_values)]
;                   )))


 )
