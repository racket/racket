(module xelda-checker mzscheme
  (require (lib "class.ss"))
  (require (lib "list.ss"))
  (require (lib "match.ss"))
  
  (require "xl-util.ss")
  (require "formula.ss")
  (require "parser.ss")
  (require "xelda-lib.ss")
  (require "xelda-com.ss")
  
  (provide unit-check)
  
  (define (unit-check gui)
    (let* ([all-formula-texts
            (begin
              (send gui update-status (format "+++SPREADSHEET PREPROCESSING BEGIN+++~n"))
              (send gui update-status "Preprocessing all formula texts.... ")
              (iterate-over-worksheet
               (lambda (cell) (get-cell-formula cell))
               (lambda (formula)
                 (and (not (string=? formula ""))
                      (eq? (string-ref formula 0) #\=)))))]
           [all-names
            (begin
              (send gui next-update-status "Preprocessing all names")
              (iterate-over-worksheet
               (lambda (cell) (get-cell-name cell))
               (lambda (s) (not (string=? s "")))))]
           [symbol-table
            (begin
              (send gui next-update-status "Preprocessing symbol-table")
              (map (lambda (pr) (list (cadr pr) (car pr))) all-names))]
           [parser (make-parser symbol-table)]
           [formulas
            (begin
              (send gui next-update-status "Preprocessing all formulas")
              (map (lambda (f)
                     (let ([cell (car f)]
                           [form (cadr f)])
                       (cond
                         [(tbl-top? form)
                          (list cell
                                (make-tbl-top 
                                 (formula-name form)
                                 (append (formula-dependencies form)
                                         (list (top-of cell)
                                               (get-formula-loc-left cell
                                                                     parser
                                                                     all-formula-texts)))
                                 (tbl-top-input-cell form)))]
                         [(tbl-left? form)
                          (list cell
                                (make-tbl-left 
                                 (formula-name form)
                                 (append (formula-dependencies form)
                                         (list (left-of cell)
                                               (get-formula-loc-up cell
                                                                   parser
                                                                   all-formula-texts)))
                                 (tbl-left-input-cell form)))]
                         [else f])))
                   (map (lambda (frm-text)
                          (list (car frm-text)
                                (call-parser parser (cadr frm-text))))
                        all-formula-texts)))]
           [all-formulas (formula-sort formulas)]
           [circular-non-circular (split-circular-non-circular all-formulas)]
           [all-circular-formulas (first circular-non-circular)]
           [all-non-circular-formulas (second circular-non-circular)]
           [good-and-bad-units
            (split-list
             (begin
               (send gui next-update-status "Preprocessing all units")
               (iterate-over-worksheet
                (lambda (cell)
                  (with-handlers
                      ([void (lambda _ 'bad-unit-format)])
                    (let ([comment-txt (get-cell-comment cell)])
                      (cond
                        [(string=? "" comment-txt) empty]
                        [else (parse-unit comment-txt)]))))
                (lambda (u) (and (not (empty? u)) (pair? u)))))
             (lambda (e) (dim? (second e))))]
           [all-units
            (map (lambda (entry)
                   (list (car entry) (canonicalize-units (cadr entry))))
                 (first good-and-bad-units))]
           [bad-format-units (second good-and-bad-units)]
           [hashed-units (make-hash-table)]
           [hashed-vars (make-hash-table)]
           [hashed-constraints-vars (make-hash-table)])
      (send gui update-status (format "done~n"))
      (send gui update-status (format "+++SPREADSHEET PREPROCESSING END+++~n"))
      
      ;; When bad formatted units are present, don't do unit checking
      (time
       (cond
         [(not (empty? bad-format-units)) 
          (send gui set-bad-format-cells
                (map (lambda (u) (car u)) bad-format-units))]
         [else      
          ;; Place all non-formulas units in the computed units(hashed-units)
          (init-hash hashed-units
                     (filter (lambda (u) (not (assq (car u) all-formulas)))
                             all-units))
          
          (send gui update-status (format "+++UNIT CHECKING BEGIN+++~n"))
          
          ;; Compute non-circular formula units
          (send gui update-status "Checking non-circular formulas.... ")
          ;;(printf "ALL NON CIRCULAR CELLS:~n~a~n" 
          ;;        (map (lambda (f) (car f))
          ;;             (reverse all-non-circular-formulas)))
          (for-each (lambda (f)
                      (compute-cell-unit f hashed-units 
                                         all-formulas all-formula-texts parser))
                    (reverse all-non-circular-formulas))
          
          ;; Compute circular formula units. Start by assigning unit variables.
          (send gui next-update-status "Assigning dimension variables for circular formulas");
          (assign-variables hashed-vars hashed-units all-circular-formulas)
          
          ;; Generate constraints
          (send gui next-update-status "Creating constraints")
          (let* ([constraints (create-constraints 
                               hashed-units hashed-vars 
                               all-formulas all-circular-formulas all-formula-texts parser)]
                 [eq-app-cs (split-list constraints (lambda (c) (eq? (first c) '=)))]
                 [equality-constraints (first eq-app-cs)]
                 [append-constraints (second eq-app-cs)])
            ;; Create equivalence classes from the equality constraints
            (send gui next-update-status "Creating equivalence classes")
            ;;(printf "EQUALITY CONSTRAINTS:~n")
            ;;(show-constraints equality-constraints)
            ;;(printf "APPEND CONSTRAINTS:~n")
            ;;(show-constraints append-constraints)
            (prune-constraints equality-constraints hashed-constraints-vars)
            
            ;; Replace variables in append-constraints with their equivalence class
            ;; representative variable or unit (unit only for operands where possible)
            (let ([append-constraints (replace-equiv-in-constraints hashed-constraints-vars
                                                                    append-constraints)])
              ;; Flatten the append constraints
              (send gui next-update-status "Solving constraints")
              ;;(printf "APPEND CONSTRAINTS: ~a~n" append-constraints)
              (solve-constraints (gaussian-elimination append-constraints)
                                 hashed-constraints-vars hashed-vars hashed-units)
              (send gui update-status (format "done~n"))
              (send gui update-status (format "+++UNIT CHECKING END+++~n"))
              
              ;; Report any error back to the gui
              (send gui set-computation-errors 
                    (computation-errors hashed-units all-formula-texts all-formulas))
              (send gui set-mismatch-errors 
                    (mismatch-errors hashed-units all-units all-formula-texts all-formulas))))]))))
  
  (define (compute-cell-unit _formula hashed-units all-formulas all-formula-texts parser)
    (let ([cell-name (car _formula)]
          [formula (cadr _formula)])
      (when (not (in-hash? hashed-units cell-name))
        (hash-table-put! 
         hashed-units cell-name
         (let ([unit (compute-formula formula cell-name hashed-units 
                                      all-formulas all-formula-texts parser)])
           (cond ((> (length unit) 1)
                  (filter (lambda (u) (not (eq? (car u) 'empty_unit)))
                          unit))
                 (else unit)))))))
  
  (define (compute-formula formula cell-loc hashed-units all-formulas all-formula-texts parser)
    (match formula
      [($ xl-number name deps val) (empty-unit)]
      [($ cell-ref name cell-name)
       (cond
         [(in-hash? hashed-units name)
          (hash-table-get hashed-units name)]
         [(not (not (assq name all-formulas)))
          (let ([u (compute-formula (cadr (assq name all-formulas))
                                    name
                                    hashed-units
                                    all-formulas
                                    all-formula-texts
                                    parser)])
            (hash-table-put! hashed-units name u)
            u)]
         [(string=? "" (get-cell-text (formula-name formula)))
          (list (list 'error/empty-formula cell-loc))]
         [else (empty-unit)])]
      [($ named-cell-ref name cell-name actual-name)
       (cond
         [(in-hash? hashed-units name)
          (hash-table-get hashed-units name)]
         [(not (not (assq name all-formulas)))
          (let ([u (compute-formula (cadr (assq name all-formulas))
                                    name
                                    hashed-units
                                    all-formulas
                                    all-formula-texts
                                    parser)])
            (hash-table-put! hashed-units name u)
            u)]
         [(string=? "" (get-cell-text (formula-name formula)))
          (list (list 'error/empty-formula cell-loc))]
         [else (empty-unit)])]
      [($ binary-op name deps op arg1 arg2)
       (let ([arg1-unit (compute-formula arg1
                                         cell-loc
                                         hashed-units
                                         all-formulas
                                         all-formula-texts
                                         parser)]
             [arg2-unit (compute-formula arg2
                                         cell-loc
                                         hashed-units
                                         all-formulas
                                         all-formula-texts
                                         parser)])
         (case op
           [(+ -) (check-equal-units (list arg1-unit arg2-unit) cell-loc)]
           [(*) (let ([result (gen-mult-units arg1-unit arg2-unit cell-loc)])
                  (cond ((null? result) (empty-unit))
                        (else result)))]
           [(/) (let ([result (gen-div-units arg1-unit arg2-unit cell-loc)])
                  (cond ((null? result) (empty-unit))
                        (else result)))]
           [(^) (gen-exp-units arg1-unit
                               (cond
                                 [(xl-number? arg2) (xl-number-val arg2)]
                                 [(or (cell-ref? arg2)
                                      (named-cell-ref? arg2))
                                  (get-cell-value (formula-name arg2))]
                                 [else 'bad-exp]) cell-loc)]))]
      [($ boolean-op name deps op arg1 arg2)
       (let ([arg1-unit (compute-formula arg1
                                         cell-loc
                                         hashed-units
                                         all-formulas
                                         all-formula-texts
                                         parser)]
             [arg2-unit (compute-formula arg2 
                                         cell-loc
                                         hashed-units
                                         all-formulas
                                         all-formula-texts
                                         parser)])
         (if (equal? arg1-unit arg2-unit)
             (empty-unit)
             (list (list 'error/bool-non-empty cell-loc))))]
      [($ unary-op name deps op arg)
       (let ([arg-unit (compute-formula arg
                                        cell-loc
                                        hashed-units
                                        all-formulas
                                        all-formula-texts
                                        parser)])
         (case op
           [(+ -) arg-unit]
           [else (error op "Unknown unary op")]))]
      [($ tbl-left name deps input-cell)
       (let ([left-cell (left-of cell-loc)]
             [formula-cell (get-formula-loc-up cell-loc parser all-formula-texts)]
             [left-cell-unit empty]
             [input-cell-unit (compute-formula input-cell
                                               cell-loc
                                               hashed-units
                                               all-formulas
                                               all-formula-texts
                                               parser)])
         (if (in-hash? hashed-units left-cell)
             (set! left-cell-unit (hash-table-get hashed-units left-cell))
             (set! left-cell-unit (compute-cell-unit (assoc left-cell all-formulas)
                                                     hashed-units
                                                     all-formulas all-formula-texts parser)))
         (compute-formula (replace-in-formula
                           (cadr (assq formula-cell all-formulas))
                           (formula-name input-cell) left-cell)
                          cell-loc
                          hashed-units
                          all-formulas
                          all-formula-texts
                          parser))]
      [($ tbl-top name deps input-cell)
       (let ([input-cell-unit (compute-formula input-cell
                                               cell-loc
                                               hashed-units
                                               all-formulas
                                               all-formula-texts
                                               parser)]
             [top-cell (top-of cell-loc)]
             [formula-cell (get-formula-loc-left cell-loc parser all-formula-texts)]
             [top-cell-unit empty])
         (if (in-hash? hashed-units top-cell)
             (set! top-cell-unit (hash-table-get hashed-units top-cell))
             (set! top-cell-unit (compute-cell-unit (assoc top-cell all-formulas)
                                                    hashed-units
                                                    all-formulas all-formula-texts parser)))
         (compute-formula (replace-in-formula
                           (cadr (assq formula-cell all-formulas))
                           (formula-name input-cell) top-cell)
                          cell-loc
                          hashed-units
                          all-formulas
                          all-formula-texts
                          parser))]
      [($ application name deps fun args) 
       (let ([arg-units (map (lambda (a)
                               (compute-formula a 
                                                cell-loc
                                                hashed-units
                                                all-formulas
                                                all-formula-texts
                                                parser)) args)])
         (case fun
           [(average sum min max mina maxa) (check-equal-units arg-units cell-loc)]
           [(not or and)
            (if (andmap empty-unit? arg-units)
                (empty-unit)
                (list (list 'error/bool-non-empty cell-loc)))]
           [(if) (cond
                   ((= 3 (length args))
                    (let ([if-unit (second arg-units)]
                          [else-unit (third arg-units)]
                          [test-unit (first arg-units)])
                      (if (empty-unit? test-unit)
                          (check-equal-units (list if-unit else-unit) cell-loc)
                          (list (list 'error/if-bool-non-empty cell-loc)))))
                   (else (error fun "illegal use")))]
           [(abs) (cond
                    ((= 1 (length args)) (first arg-units))
                    (else (error fun "illegal use")))]
           [(ceiling round)
            (cond
              ((= 2 (length args))
               (let ([num-unit (first arg-units)]
                     [prec-unit (second arg-units)])
                 (cond 
                   ((empty-unit? prec-unit) num-unit)
                   (else (list (list 'error/non-empty-precision cell-loc))))))
              (else (error fun "illegal use")))]
           [(large) (let ([split-units (split-large-args arg-units)])
                      (cond
                        ((= 1 (length (car split-units)))
                         (check-equal-units (cadr split-units) cell-loc))
                        (else (list (list 'error/large-empty-unit cell-loc)))))]
           [(acos acosh asin asinh atan atanh cos cosh sin sinh tan tanh exp)
            (let ([arg-unit (first arg-units)])
              (cond ((= 1 (length args))
                     (if (empty-unit? arg-unit)
                         arg-unit
                         (list (list 'error/non-empty-argument cell-loc))))
                    (else (error fun "illegal use"))))]
           [(sqrt) (let ([arg-unit (first arg-units)])
                     (cond
                       ((= 1 (length args))
                        (cond [(empty-unit? arg-unit) arg-unit]
                              [(andmap (lambda (s_u)
                                         (= (modulo (cadr s_u) 2) 0))
                                       arg-unit)
                               (map (lambda (s_u)
                                      (list (car s_u) (/ (cadr s_u) 2)))
                                    arg-unit)]
                              [else
                               (list (list 'error/invalid-sqrt-dimension cell-loc))]))
                       (else (error fun "illegal use"))))]
           [(fact ln log) (let ([arg-unit (first arg-units)])
                            (cond
                              ((= 1 (length args))
                               (if (empty-unit? arg-unit)
                                   arg-unit
                                   (list (list 'error/non-unitless-argument cell-loc))))
                              (else (error fun "illegal use"))))]
           [(isnumber)
            (cond ((= 1 (length arg-units)) (empty-unit))
                  (else (error fun "illegal use")))]
           [(median stdev) (check-equal-units arg-units cell-loc)]
           [(npv) (cond
                    [(>= (length args) 2)
                     (cond [(empty-unit? (first arg-units))
                            (let ([u (first (rest arg-units))])
                              (foldl (lambda (curr prev) (check-equal-units curr prev))
                                     u
                                     (rest (rest arg-units))))]
                           [else
                            (list (list 'error/npv-rate-non-dimensionless cell-loc))])]
                    [else (error fun "illegal use")])]
           [(intercept)
            (cond
              [(> (length arg-units) 0)
               (let* ([x-unit (first arg-units)]
                      [xy-units (split-list (rest arg-units) (lambda (u) (equal? u x-unit)))]
                      [x-units (cons x-unit (first xy-units))]
                      [y-units (second xy-units)])
                 (cond [(= (length x-units) (length y-units))
                        (check-equal-units y-units cell-loc)]
                       [else (list (list 'error/intercept-invalid-points cell-loc))]))]
              [else (empty-unit)])]
           [(count) (empty-unit)]
           [(frequency)
            (let ([args-unit (check-equal-units arg-units cell-loc)])
              (cond [(is-error-unit? args-unit) args-unit]
                    [else (empty-unit)]))]
           [(mod)
            (cond ((= 2 (length arg-units)) (first arg-units))
                  (else (error fun "illegal use")))]
           [(na pi today now) (cond ((empty? args) (empty-unit))
                                    (else (error fun "illegal use")))]
           [(mmult) (let* ([formula-text (lookup-formula-text cell-loc all-formula-texts)]
                           [Ms (identify-matrices
                                (substring formula-text 7
                                           (sub1 (string-length formula-text))))])
                      (cond
                        [(= 2 (length Ms))
                         (let* ([cell-xy (get-cell-row-col cell-loc)]
                                [M1-row (car cell-xy)]
                                [M2-col (cadr cell-xy)]
                                [M1-row-unit (check-equal-units
                                              (matrix-row-units (car Ms) M1-row hashed-units)
                                              cell-loc)]
                                [M2-col-unit (check-equal-units
                                              (matrix-col-units (cadr Ms) M2-col hashed-units)
                                              cell-loc)])
                           (cond
                             [(is-error-unit? M1-row-unit) M1-row-unit]
                             [(is-error-unit? M2-col-unit) M2-col-unit]
                             [else (gen-mult-units M1-row-unit M2-col-unit cell-loc)]))]
                        [else (error fun "illegal use")]))]
           [else (empty-unit)]))]))  ;; unimplemented functions -> empty unit
  
  (define (replace-in-formula formula orig new)
    (match formula
      [($ xl-number name deps val) formula]
      [($ cell-ref name cell-name)
       (cond
         [(eq? name orig) (make-cell-ref new empty)]
         [else formula])]
      [($ named-cell-ref name cell-name actual-name) formula]
      [($ binary-op name deps op arg1 arg2)
       (make-binary-op name deps op
                       (replace-in-formula arg1 orig new)
                       (replace-in-formula arg2 orig new))]
      [($ boolean-op name deps op arg1 arg2)
       (make-boolean-op name deps op
                        (replace-in-formula arg1 orig new)
                        (replace-in-formula arg2 orig new))]
      [($ unary-op name deps op arg)
       (make-unary-op name deps op (replace-in-formula arg orig new))]
      [($ tbl-left name deps input-cell)
       (cond [(eq? input-cell orig)
              (make-tbl-left name deps new)]
             [else formula])]
      [($ tbl-top name deps input-cell)
       (cond [(eq? input-cell orig)
              (make-tbl-top name deps new)]
             [else formula])]
      [($ application name deps fun args) 
       (let ([replaced-args
              (map (lambda (a)
                     (replace-in-formula a orig new)) args)])
         (make-application name deps fun replaced-args))]
      [else formula]))
  
  (define (remove-duplicates lst)
    (cond
      [(null? lst) null]
      [(member (car lst) (cdr lst)) (remove-duplicates (cdr lst))]
      [else (cons (car lst) (remove-duplicates (cdr lst)))]))
  
  (define (formula-sort fs)
    (sort fs (lambda (f1 f2)
               (let* ([name (car f1)]
                      [deps (formula-dependencies (cadr f2))])
                 (memq name deps)))))
  
  (define (get-formula-loc-up loc parser all-formula-texts)
    (let ([formula (call-parser parser (cadr (assoc loc all-formula-texts)))])
      (cond ((not (tbl-left? formula)) loc)
            (else (let* ([xy (cellref->numbers loc)]
                         [x (sub1 (car xy))]
                         [y (sub1 (cadr xy))])
                    (get-formula-loc-up (numbers->cellref (list x y))
                                        parser all-formula-texts))))))
  
  (define (get-formula-loc-left loc parser all-formula-texts)
    (let ([formula (call-parser parser (cadr (assoc loc all-formula-texts)))])
      (cond ((not (tbl-top? formula)) loc)
            (else (let* ([xy (cellref->numbers loc)]
                         [x (- (car xy) 2)]
                         [y (cadr xy)])
                    (get-formula-loc-left (numbers->cellref (list x y))
                                          parser all-formula-texts))))))
  
  (define (assign-variables hashed-vars hashed-units all-circular-formulas)
    (for-each (lambda (f)
                (let ([a_s (gen-alpha-var)])
                  (hash-table-put! hashed-vars a_s 
                                   (list (car f) (list (list 'error/missing-dimensions (car f)))))
                  (hash-table-put! hashed-units (car f) a_s)))
              all-circular-formulas))
  
  (define (gen-alpha-var)
    (string->symbol (string-append "a_" (symbol->string (gensym)))))
  
  (define (gen-beta-var)
    (string->symbol (string-append "b_" (symbol->string (gensym)))))
  
  (define (alpha-var? a)
    (and (symbol? a)
         (let [(l (string->list (symbol->string a)))]
           (and (> (length l) 3)
                (equal? (first l) #\a)
                (equal? (second l) #\_)
                (equal? (third l) #\g)))))
  
  (define (beta-var? b)
    (and (symbol? b)
         (let [(l (string->list (symbol->string b)))]
           (and (> (length l) 3)
                (equal? (first l) #\b)
                (equal? (second l) #\_)
                (equal? (third l) #\g)))))
  
  (define (dim-var? s) (or (alpha-var? s) (beta-var? s)))
  
  (define (dim? u) (and (list? u)
                        (andmap (lambda (u_)
                                  (and (list? u_)
                                       (= 2 (length u_))
                                       (symbol? (car u_))
                                       (integer? (cadr u_)))) u)))
  
  (define (constraint-var c) (second c))
  (define (constraint-operator c) (first c))
  (define (constraint-left-side c) (third c))
  (define (constraint-right-side c) (fourth c))
  
  (define (create-constraints hashed-units hashed-vars 
                              all-formulas all-circular-formulas all-formula-texts parser)
    (foldl (lambda (f constraints)
             (let ([a_s (hash-table-get hashed-units (car f))])
               (create-constraints-for-formula a_s (cadr f) (car f) constraints
                                               hashed-units hashed-vars 
                                               all-formulas all-formula-texts parser)))
           empty all-circular-formulas))
  
  (define (create-constraints-for-formula sym formula cell-loc formula-constraints
                                          hashed-units  hashed-vars 
                                          all-formulas all-formula-texts parser)
    (match formula
      [($ xl-number name deps val)
       (cons (list '= sym (empty-unit)) formula-constraints)]
      [($ cell-ref name cell-name)
       (cond [(in-hash? hashed-units name)
              (cons
               (list '= sym (hash-table-get hashed-units name))
               formula-constraints)]
             [else (cons (list '= sym (empty-unit)) formula-constraints)])]
      [($ named-cell-ref name cell-name actual-name)
       (cond [(in-hash? hashed-units name)
              (cons
               (list '= sym (hash-table-get hashed-units name))
               formula-constraints)]
             [else (cons (list '= sym (empty-unit)) formula-constraints)])]
      [($ binary-op name deps op arg1 arg2)
       (let* ([left-op (gen-beta-var)]
              [right-op (gen-beta-var)]
              [new-constraints
               (create-constraints-for-formula 
                left-op arg1 cell-loc
                (create-constraints-for-formula
                 right-op arg2 cell-loc formula-constraints hashed-units hashed-vars
                 all-formulas all-formula-texts parser)
                hashed-units hashed-vars all-formulas all-formula-texts parser)])
         (case op
           [(+ -)
            (cons (list '= sym left-op)
                  (cons (list '= sym right-op) new-constraints))]
           [(*)
            (cons (list '@ sym left-op right-op) new-constraints)]
           [(/)
            (cons (list '@/ sym left-op right-op) new-constraints)]
           [(^)
            (cons (list '= right-op (empty-unit)) new-constraints)]))]
      [($ boolean-op name deps op arg1 arg2)
       (let* ([left-op (gen-beta-var)]
              [right-op (gen-beta-var)]
              [new-constraints
               (create-constraints-for-formula 
                left-op arg1 cell-loc
                (create-constraints-for-formula
                 right-op arg2 cell-loc formula-constraints hashed-units hashed-vars
                 all-formulas all-formula-texts parser)
                hashed-units hashed-vars all-formulas all-formula-texts parser)])
         (cons (list '= left-op right-op)
               (cons (list '= sym (empty-unit)) new-constraints)))]
      [($ unary-op name deps op arg)
       (let ([op-sym (gen-beta-var)])
         (cons (list '= sym op-sym)
               (create-constraints-for-formula op-sym arg cell-loc formula-constraints
                                               hashed-units hashed-vars
                                               all-formulas all-formula-texts parser)))]
      [($ tbl-left name deps input-cell)
       (let* ([left-cell (left-of cell-loc)]
              [formula-cell (get-formula-loc-up cell-loc parser all-formula-texts)]
              [formula-sym (gen-beta-var)])
         (cons (list '= sym formula-sym)
               (create-constraints-for-formula 
                formula-sym 
                (replace-in-formula (cadr (assq formula-cell all-formulas))
                                    (formula-name input-cell) left-cell)
                formula-cell formula-constraints hashed-units hashed-vars
                all-formulas all-formula-texts parser)))]
      [($ tbl-top name deps input-cell)
       (let* ([top-cell (top-of cell-loc)]
              [formula-cell (get-formula-loc-left cell-loc parser all-formula-texts)]
              [formula-sym (gen-beta-var)])
         (cons (list '= sym formula-sym)
               (create-constraints-for-formula 
                formula-sym 
                (replace-in-formula (cadr (assq formula-cell all-formulas))
                                    (formula-name input-cell) top-cell)
                formula-cell formula-constraints hashed-units hashed-vars
                all-formulas all-formula-texts parser)))]
      [($ application name deps fun args)
       (let* ([arg-syms (map (lambda (a) (gen-beta-var)) args)]
              [arg-constraints
               (letrec ([gen-arg-constraints
                         (lambda (as as-syms)
                           (cond 
                             [(empty? as) formula-constraints]
                             [else
                              (create-constraints-for-formula 
                               (first as-syms) (first as) cell-loc
                               (gen-arg-constraints (rest as) (rest as-syms))
                               hashed-units hashed-vars 
                               all-formulas all-formula-texts parser)]))])
                 (gen-arg-constraints args arg-syms))])
         (case fun
           [(average sum min max mina maxa)
            (foldl (lambda (arg constraints) (cons (list '= sym arg) constraints))
                   arg-constraints arg-syms)]
           [(not or and)
            (cons (list '= sym (empty-unit))
                  (foldl (lambda (arg constraints) (cons (list '= sym empty-unit) constraints))
                         arg-constraints arg-syms))]               
           [(if) (cond
                   [(= 3 (length args))
                    (let ([if-sym (second arg-syms)]
                          [else-sym (third arg-syms)]
                          [test-sym (first arg-syms)])
                      (cons (list '= test-sym (empty-unit))
                            (cons (list '= if-sym else-sym)
                                  (cons (list '= sym if-sym) arg-constraints))))]
                   [else (error fun "illegal use")])]
           [(abs) (cond
                    [(= 1 (length args)) (cons (list '= sym (first arg-syms) arg-constraints))]
                    [else (error fun "illegal use")])]
           [(ceiling round)
            (cond
              [(= 2 (length args))
               (let ([num-sym (first arg-syms)]
                     [prec-sym (second arg-syms)])
                 (cons (list '= prec-sym (empty-unit))
                       (cons (list '= sym num-sym) arg-constraints)))]
              [else (error fun "illegal use")])]
           [(large) (let* ([reversed-syms (reverse arg-syms)]
                           [k-sym (first reversed-syms)]
                           [array-syms (rest reversed-syms)])
                      (cons (list '= k-sym (empty-unit))
                            (foldl (lambda (arg constraints) (cons (list '= sym arg) constraints))
                                   arg-constraints array-syms)))]
           [(acos acosh asin asinh atan atanh cos cosh sin sinh tan tanh exp)
            (let ([arg-sym (first arg-syms)])
              (cond [(= 1 (length args))
                     (cons (list '= arg-sym (empty-unit))
                           (cons (list '= sym (empty-unit)) arg-constraints))]
                    [else (error fun "illegal use")]))]
           [(sqrt) (let ([arg-sym (first arg-syms)])
                     (cond
                       [(= 1 (length args))
                        (cons (list '@/ sym arg-sym sym) arg-constraints)]
                       [else (error fun "illegal use")]))]
           [(fact ln log) (let ([arg-sym (first arg-syms)])
                            (cond
                              [(= 1 (length args))
                               (cons (list '= arg-sym (empty-unit))
                                     (cons (list '= sym (empty-unit)) arg-constraints))]
                              [else (error fun "illegal use")]))]
           [(isnumber)
            (cond [(= 1 (length arg-syms)) (cons (list '= sym (empty-unit)) arg-constraints)]
                  [else (error fun "illegal use")])]
           [(median stdev)
            (foldl (lambda (arg constraints) (cons (list '= sym arg) constraints))
                   arg-constraints arg-syms)]
           [(npv)
            (cond [(>= (length args) 2)
                   (cons (cons '= (first arg-syms) (empty-unit))
                         (foldl (lambda (arg constraints) (cons (list '= sym arg) constraints))
                                arg-constraints (rest arg-syms)))]
                  [else (error fun "illegal use")])]
           [(count) (cons (list '= sym (empty-unit)) arg-constraints)]
           [(frequency)
            (when (not (empty? arg-syms))
              (let ([first-sym (first arg-syms)])
                (cons (list '= sym (empty-unit))
                      (foldl (lambda (arg constraints) (cons (list '= first-sym arg) constraints))
                             arg-constraints (rest arg-syms)))))]
           [(mod)
            (cond [(= 2 (length args)) (cons (list '= sym (first arg-syms)) arg-constraints)]
                  [else (error fun "illegal use")])]
           [(na pi today now) 
            (cond [(empty? args) (cons (list '= sym (empty-unit)) arg-constraints)]
                  [else (error fun "illegal use")])]
           [(mmult) (let* ([formula-text (lookup-formula-text cell-loc all-formula-texts)]
                           [Ms (identify-matrices
                                (substring formula-text 7
                                           (sub1 (string-length formula-text))))])
                      (cond
                        [(= 2 (length Ms))
                         (let* ([cell-xy (get-cell-row-col cell-loc)]
                                [M1-row (car cell-xy)]
                                [M2-col (cadr cell-xy)]
                                [M1-row-sym (gen-beta-var)]
                                [M2-col-sym (gen-beta-var)]
                                [row-syms-constraints (matrix-row-syms (car Ms) M1-row
                                                                       hashed-units hashed-vars)]
                                [row-syms (first row-syms-constraints)]
                                [row-constraints (second row-syms-constraints)]
                                [col-syms-constraints (matrix-col-syms (cadr Ms) M2-col
                                                                       hashed-units hashed-vars)]
                                [col-syms (first col-syms-constraints)]
                                [col-constraints (second col-syms-constraints)])
                           (cons (list '@ sym M1-row-sym M2-col-sym)
                                 (foldl (lambda (arg constraints)
                                          (cons (list '= M1-row-sym arg) constraints))
                                        (foldl (lambda (arg constraints)
                                                 (cons (list '= M2-col-sym arg) constraints))
                                               (append arg-constraints row-constraints
                                                       col-constraints)
                                               col-syms)
                                        row-syms)))]
                        [else (error fun "illegal use")]))]         
           [else (cons (list '= sym (list (list 'error/unimplemented-function 1)))
                       arg-constraints)]))]))
  
  (define (prune-constraints equality-constraints hashed-constraints-vars)
    (for-each (lambda (c)
                (let* ([syms (cond [(or (dim? (second c)) (is-error-unit? (second c)))
                                    (list (third c) (second c))]
                                   [else (list (second c) (third c))])]
                       [sym1 (first syms)]
                       [sym2 (second syms)])
                  (if (or (dim? sym2) (is-error-unit? sym2))
                      (resolve-var-dim sym1 sym2 hashed-constraints-vars)
                      (resolve-var-var sym1 sym2 hashed-constraints-vars))))
              equality-constraints))
  
  (define (replace-equiv-in-constraints hashed-constraints-vars append-constraints)
    (let* ([replace-head
            (lambda (var)
              (cond
                [(in-hash? hashed-constraints-vars var)
                 (first (find-rep (first (hash-table-get hashed-constraints-vars var))))]
                [else var]))]
           [replace-operands
            (lambda (operands)
              (let ([replace-op
                     (lambda (op)
                       (if (dim-var? op)
                           (cond
                             [(in-hash? hashed-constraints-vars op)
                              (let* ([rep (first 
                                           (find-rep 
                                            (first (hash-table-get hashed-constraints-vars op))))]
                                     [rep-u (second (hash-table-get hashed-constraints-vars rep))])
                                (if (empty? rep-u)
                                    (list (list rep 1))
                                    rep-u))]
                             [else (list (list op 1))])
                           op))])
                (append (replace-op (first operands)) (replace-op (second operands)))))]
           [map-replace 
            (lambda (c)
              (cons (replace-head (second c))
                    (cons '= (replace-operands (rest (rest c))))))])
      (remove-duplicates (map map-replace append-constraints))))
  
  (define (resolve-var-dim var u hashed-constraints-vars)
    (if (not (in-hash? hashed-constraints-vars var))
        (hash-table-put! hashed-constraints-vars var (list (make-equiv-class var) u))
        (let* ([tbl-entry (hash-table-get hashed-constraints-vars var)]
               [rep (first (find-rep (first tbl-entry)))]
               [rep-tbl-entry (hash-table-get hashed-constraints-vars rep)])
          (let ([var-u
                 (cond [(empty? (second tbl-entry)) u]
                       [(not (or (equal? (second tbl-entry) u) 
                                 (is-error-unit? (second tbl-entry))))
                        (cond [(is-error-unit? u) u]
                              [else (list (list 'error/equality var))])]
                       [else u])])
            (hash-table-put! hashed-constraints-vars  var
                             (list (first tbl-entry) var-u))
            (let ([rep-u
                   (cond [(empty? (second rep-tbl-entry)) var-u]
                         [(not (or (equal? (second rep-tbl-entry) u)
                                   (is-error-unit? (second rep-tbl-entry))))
                          (cond [(is-error-unit? u) u]
                                [else (list (list 'error/equality rep))])]
                         [else var-u])])
              (hash-table-put! hashed-constraints-vars rep
                               (list (first rep-tbl-entry) rep-u)))))))
  
  (define (resolve-var-var var1 var2 hashed-constraints-vars)
    (when (not (in-hash? hashed-constraints-vars var1))
      (hash-table-put! hashed-constraints-vars var1 (list (make-equiv-class var1) empty)))
    (when (not (in-hash? hashed-constraints-vars var2))
      (hash-table-put! hashed-constraints-vars var2 (list (make-equiv-class var2) empty)))
    (let ([var1-tbl-entry (hash-table-get hashed-constraints-vars var1)]
          [var2-tbl-entry (hash-table-get hashed-constraints-vars var2)])
      (union (first (hash-table-get hashed-constraints-vars var1))
             (first (hash-table-get hashed-constraints-vars var2)))
      (let* ([var1-u (second var1-tbl-entry)]
             [var2-u (second var2-tbl-entry)]
             [var-u
              (cond [(empty? var1-u) var2-u]
                    [(empty? var2-u) var1-u]
                    [(not (equal? var1-u var2-u))
                     (list (list 'error/equality var1))]
                    [else var1-u])]
             [rep (first (find-rep (first var1-tbl-entry)))]
             [rep-tbl-entry (hash-table-get hashed-constraints-vars rep)]
             [rep-u (second rep-tbl-entry)]
             [new-u
              (cond [(empty? var-u) rep-u]
                    [(empty? rep-u) var-u]
                    [(not (equal? var-u rep-u))
                     (list (list 'error/equality rep))]
                    [else var-u])])
        (hash-table-put! hashed-constraints-vars var1
                         (list (first var1-tbl-entry) new-u))
        (hash-table-put! hashed-constraints-vars var2
                         (list (first var2-tbl-entry) new-u))
        (hash-table-put! hashed-constraints-vars rep
                         (list (first rep-tbl-entry) new-u)))))
  
  (define (filter-out-bad-constraints cs)
    (let* ([bad-vars (make-hash-table)]
           [new-cs
            (filter
             (lambda (c)
               (let ([right-side (rest (rest c))]
                     [left-side (first c)])
                 (cond [(or (not (assq left-side right-side))
                            (andmap (lambda (u) (dim-var? (first u))) right-side)) #t]
                       [else
                        (cond [(dim-var? (first (first right-side)))
                               (hash-table-put! bad-vars (first (first right-side))
                                                '((error/circular 1)))]
                              [else
                               (hash-table-put! bad-vars (first (second right-side))
                                                '((error/circular 1)))])
                        #f])))
             cs)])
      new-cs))
  
  (define (rearrange-constraints cs)
    (map (lambda (c)
           (let ([right-side (rest (rest c))]
                 [left-side (first c)])
             (cond [(not (assq left-side right-side))
                    (cons (list left-side -1) right-side)]
                   [else
                    (filter
                     (lambda (u) (not (= 0 (second u))))
                     (map (lambda (u)
                            (cond [(eq? left-side (first u))
                                   (list (first u) (sub1 (second u)))]
                                  [else u]))
                          right-side))])))
         cs))
  
  (define (ordered-variables eqs)
    (remove-duplicates
     (sort
      (foldl (lambda (eq vars)
               (append
                (foldl (lambda (u eq-vars)
                         (cond [(dim-var? (first u)) (cons (first u) eq-vars)]
                               [else eq-vars]))
                       empty eq)
                vars))
             empty eqs)
      (lambda (var1 var2) (string<? (symbol->string var1) (symbol->string var2))))))
  
  (define (reorder-eqs eqs)
    (let ([ordered-vars (ordered-variables eqs)])
      (list 
       ordered-vars
       (map
        (lambda (eq)
          (let* ([vars-dims (split-list eq (lambda (u) (dim-var? (first u))))]
                 [vars (first vars-dims)])
            (list
             (map
              (lambda (var)
                (let ([var-exp (assq var vars)])
                  (cond [var-exp (second var-exp)]
                        [else 0])))
              ordered-vars)
             (second vars-dims))))
        eqs))))
  
  (define (reduce eq)
    (foldr
     (lambda (u new-eq)
       (cond [(= 0 (second u)) new-eq]
             [(assq (first u) new-eq)
              (filter
               (lambda (u) (not (= 0 (second u))))
               (map (lambda (u_new)
                      (cond [(eq? (first u) (first u_new))
                             (list (first u) (+ (second u) (second u_new)))]
                            [else u_new]))
                    new-eq))]
             [else (cons u new-eq)]))
     empty eq))
  
  (define (first-n l n)
    (cond [(> n (length l)) l]
          [else
           (letrec ([loop (lambda (l n)
                            (cond [(>= 0 n) empty]
                                  [else (cons (first l) (loop (rest l) (sub1 n)))]))])
             (loop l n))]))
  
  (define (normalize-unit us)
    (let* ([sorted-us (sort us (lambda (u1 u2)
                                 (string<=? (symbol->string (car u1))
                                            (symbol->string (car u2)))))]
           [new-u (filter
                   (lambda (u)
                     (not (= 0 (second u))))
                   (foldr (lambda (u new-us)
                            (cond
                              [(eq? 'empty_unit (first u)) new-us]
                              [(assq (first u) new-us)
                               (map (lambda (new_u)
                                      (cond [(eq? (first new_u) (first u))
                                             (list (first u) (+ (second u) (second new_u)))]
                                            [new_u]))
                                    new-us)]
                              [else (cons u new-us)]))
                          empty sorted-us))])
      (cond [(empty? new-u) (empty-unit)]
            [else new-u])))
  
  (define (select-nth eqs eqn n)
    (letrec ([split
              (lambda (eqs i)
                (cond [(>= i (length eqs)) (list empty eqs)]
                      [else
                       (let* ([eq-i (list-ref eqs i)]
                              [row (first eq-i)]
                              [nth-val (list-ref row n)])
                         (cond [(not (= 0 nth-val))
                                (list
                                 (list
                                  (map (lambda (val) (/ val nth-val)) row)
                                  (normalize-unit
                                   (map (lambda (u) (list (first u) (/ (second u) nth-val)))
                                        (second eq-i))))
                                 (append (first-n eqs i) (list-tail eqs (add1 i))))]
                               [else (split eqs (add1 i))]))]))])
      (let* ([nth-rest (split eqs 0)]
             [nth-eq (first nth-rest)]
             [nth-eq-u (cond [(empty? nth-eq) (empty-unit)]
                             [else (second nth-eq)])]
             [transform
              (lambda (eqs kth-eq)
                (map
                 (lambda (eq)
                   (let* ([row (first eq)]
                          [kth-row (first kth-eq)]
                          [nth-eq (list-ref row n)]
                          [nth-kth-eq (list-ref kth-row n)]
                          [ratio (/ nth-eq nth-kth-eq)])
                     (letrec ([simplify
                               (lambda (i)
                                 (cond [(>= i (length row)) empty]
                                       [else (cons (- (list-ref row i)
                                                      (* ratio (list-ref kth-row i)))
                                                   (simplify (add1 i)))]))])
                       (cond [(= 0 ratio) eq]
                             [else (list (simplify 0)
                                         (normalize-unit
                                          (append (second eq)
                                                  (map (lambda (u) (list (first u) (* (- 0 ratio) (second u))))
                                                       nth-eq-u))))]))))
                 eqs))])
              ;;(printf "NTH-EQ: ~a || n = ~a~n" nth-eq n)
              ;;(printf "TRANSFORMED REST: ~a~n" (transform (second nth-rest) nth-eq))
        (cond [(empty? (first nth-rest)) (list eqs eqn)]
              [else
               (let ([transformed-eq (transform (second nth-rest) nth-eq)])
                 (cond [(ormap (lambda (eq) (and (andmap (lambda (v) (= 0 v)) (first eq))
                                                 (not (or (empty? (second eq))
                                                          (eq? (first (first (second eq))) 'empty_unit)))))
                               transformed-eq)
                        (list
                         (cons (list (first nth-eq) '((error/circular 1))) transformed-eq)
                         (add1 eqn))]
                       [else
                        (list
                         (cons nth-eq transformed-eq)
                         (add1 eqn))]))]))))
  
  (define (modify-eqs eqs)
    (let* ([order-eqs (reorder-eqs eqs)]
           [ordered-vars (first order-eqs)]
           [eqs (second order-eqs)]
           [select-eq-for-n-var
            (lambda (eqs eqn n)
              ;;(printf "EQS:~a~n" eqs)
              (let* ([good-eqs (first-n eqs eqn)]
                     [mod-eqs (list-tail eqs eqn)]
                     [meqs-eqn (select-nth mod-eqs eqn n)])
                ;;(printf "SELECT-NTH MEQS-EQN: ~a~n" meqs-eqn)
                (list (append good-eqs (first meqs-eqn)) (second meqs-eqn))))])
      (letrec ([modify
                (lambda (eqs n eqn)
                  (cond [(>= n (length ordered-vars))
                         (list ordered-vars (first-n eqs eqn))]
                        [else
                         (let ([neweqs-eqn (select-eq-for-n-var eqs eqn n)])
                           ;;(printf "NEW-EQS-EQN: ~a~n" neweqs-eqn)
                           (modify (first neweqs-eqn) (+ n 1) (second neweqs-eqn)))]))])
        (modify eqs 0 0))))
  
  (define (gaussian-elimination append-constraints)
    (let* ([eqs (rearrange-constraints
                 (filter-out-bad-constraints append-constraints))]
           [order-triang-eqs (modify-eqs eqs)]
           [ordered-vars (first order-triang-eqs)])
      ;;(printf "EQS: ~a~n" eqs)
      ;;(printf "ORDERED TRIANG EQS: ~a~n" order-triang-eqs)
      (letrec ([nth-var (lambda (row n) (cond [(= 0 (length row)) -1]
                                              [(= 1 (first row)) n]
                                              [else (nth-var (rest row) (add1 n))]))])
        (map
         (lambda (var-sol)
           (list (first var-sol)
                 (cond [(ormap (lambda (u) (not (integer? (second u)))) (second var-sol))
                        '((error/circular 1))]
                       [else (second var-sol)])))
         (foldr
          (lambda (eq sols)
            (let* ([n (nth-var (first eq) 0)]
                   [var (list-ref ordered-vars n)])
              ;;(printf "eq: ~a || n = ~a || nth-var = ~a~n" (first eq) n var)
              (cons
               (list
                var
                (letrec [(computed-u (lambda (n)
                                       (cond [(>= n (length (first eq))) empty]
                                             [else
                                              (let* ([n-var (list-ref ordered-vars n)]
                                                     [var-exp (assq n-var sols)])
                                                ;;(printf "n-var = ~a || var-exp = ~a~n" n-var var-exp)
                                                (cond
                                                  [var-exp 
                                                   (let ([factor (- 0 (list-ref (first eq) n))])
                                                     (append (map (lambda (u) (list (first u) (* factor (second u))))
                                                                  (second var-exp))
                                                             (computed-u (add1 n))))]
                                                  [else '((error/circular 1))]))])))]
                  (let ([c-u (computed-u (add1 n))])
                    ;;(printf "VAR = ~a | c-u = ~a | second eq = ~a~n" var c-u (second eq))
                    (cond [(ormap (lambda (u) (or (eq? (first u) 'error/circular)
                                                  (not (integer? (second u)))))
                                  c-u)
                           '((error/circular 1))]
                          [else (normalize-unit
                                 (append c-u (map (lambda (u) (list (first u) (- 0 (second u)))) (second eq))))]))))
               sols)))
          empty
          (second order-triang-eqs))))))
  
  (define (solve-constraints solved-constraints
                             hashed-constraints-vars hashed-vars hashed-units)
    ;;(printf "HASHED VARS:~n")
    ;;(all-hashed-values hashed-vars)
    ;;(printf "HASHED CONSTRAINTS:~n")
    ;;(all-hashed-values hashed-constraints-vars)
    ;;(printf "SOLVED CONSTRAINTS: ~a~n" solved-constraints)
    (for-each (lambda (c) (resolve-var-dim (first c) (second c) hashed-constraints-vars))
              solved-constraints)
    ;;(printf "HASHED CONSTRAINTS AFTER GAUSSIAN:~n")
    ;;(all-hashed-values hashed-constraints-vars)
    (resolve-constraints-vars hashed-constraints-vars)
    ;;(printf "HASHED CONSTRAINTS AFTER RESOLVE:~n")
    ;;(all-hashed-values hashed-constraints-vars)
    (replace-constraints-vars hashed-constraints-vars hashed-vars hashed-units))
  
  (define (resolve-constraints-vars hashed-constraints-vars)
    (hash-table-for-each
     hashed-constraints-vars
     (lambda (var vals)
       (let* ([var-u (second vals)]
              [rep (first (find-rep (first vals)))]
              [rep-tbl-entry (hash-table-get hashed-constraints-vars rep)]
              [rep-u (second rep-tbl-entry)])
         (when (not (eq? var rep))
           (let ([new-u
                  (cond [(empty? rep-u) var-u]
                        [(and (not (empty? var-u)) (not (equal? rep-u var-u)))
                         (list (list 'error/equality rep))]
                        [else rep-u])])
             (hash-table-put! hashed-constraints-vars rep
                              (list (first rep-tbl-entry) new-u)))))))
    (hash-table-for-each
     hashed-constraints-vars
     (lambda (var vals) (hash-table-put!
                         hashed-constraints-vars var
                         (list (first vals)
                               (second (hash-table-get 
                                        hashed-constraints-vars
                                        (first (find-rep (first vals))))))))))
  
  (define (replace-constraints-vars hashed-constraints-vars hashed-vars hashed-units)
    (hash-table-for-each
     hashed-constraints-vars
     (lambda (var val)
       (when (alpha-var? var)
         (let ([cell-loc (first (hash-table-get hashed-vars var))]
               [u (second val)])
           (when (not (empty? u))
             (when (is-error-unit? u)
               (set! u (list (list (first (first u)) cell-loc))))
             (hash-table-put! hashed-vars var (list cell-loc u)))))))
    (hash-table-for-each
     hashed-vars
     (lambda (var val)
       (let ([cell-loc (first val)]
             [u (second val)])
         (hash-table-put! hashed-units cell-loc u)))))
  
  (define (lookup-formula-text cell-sym all-formula-texts)
    (let ([entry (assq cell-sym all-formula-texts)])
      (and entry (cadr entry))))
  
  (define (identify-matrices str)
    (map (lambda (range)
           (map (lambda (cell)
                  (string->symbol cell))
                (split-string range #\:)))
         (split-string (to-lower str) #\,)))
  
  (define (matrix-row-units M row hashed-units)
    (let ([row-cells (get-range-cells (first-cell-in-row M row)
                                      (last-cell-in-row M row))])
      (map (lambda (cellref)
             (cond ((in-hash? hashed-units cellref)
                    (hash-table-get hashed-units cellref))
                   (else (empty-unit))))
           row-cells)))
  
  (define (matrix-row-syms M row hashed-units hashed-vars)
    (let ([row-cells (get-range-cells (first-cell-in-row M row)
                                      (last-cell-in-row M row))])
      (foldl (lambda (cellref syms-constraints)
               (let ([sym (gen-beta-var)]
                     [syms (first syms-constraints)]
                     [constraints (second syms-constraints)])
                 (cond [(in-hash? hashed-units cellref)
                        (let ([u (hash-table-get hashed-units cellref)])
                          (cond [(in-hash? hashed-vars u) (list (cons u syms) constraints)]
                                [else (list (cons sym syms)
                                            (cons (list '= sym u) constraints))]))]
                       [else (list (cons sym syms)
                                   (cons (list '= sym (empty-unit)) constraints))])))
             row-cells)))
  
  (define (first-cell-in-row M row)
    (let ([xy (cellref->numbers (car M))])
      (numbers->cellref (list (sub1 (car xy)) (+ (cadr xy) row)))))
  
  (define (last-cell-in-row M row)
    (let ([xy-upper (cellref->numbers (car M))]
          [xy-lower (cellref->numbers (cadr M))])
      (numbers->cellref (list (sub1 (car xy-lower)) (+ (cadr xy-upper) row)))))
  
  (define (matrix-col-units M col hashed-units)
    (let ([col-cells (get-range-cells (first-cell-in-col M col)
                                      (last-cell-in-col M col))])
      (map (lambda (cellref)
             (cond ((in-hash? hashed-units cellref)
                    (hash-table-get hashed-units cellref))
                   (else (empty-unit))))
           col-cells)))
  
  (define (matrix-col-syms M col hashed-units hashed-vars)
    (let ([col-cells (get-range-cells (first-cell-in-col M col)
                                      (last-cell-in-col M col))])
      (foldl (lambda (cellref syms-constraints)
               (let ([sym (gen-beta-var)]
                     [syms (first syms-constraints)]
                     [constraints (second syms-constraints)])
                 (cond [(in-hash? hashed-units cellref)
                        (let ([u (hash-table-get hashed-units cellref)])
                          (cond [(in-hash? hashed-vars u) (list (cons u syms) constraints)]
                                [else (list (cons sym syms)
                                            (cons (list '= sym u) constraints))]))]
                       [else (list (cons sym syms)
                                   (cons (list '= sym (empty-unit)) constraints))])))
             col-cells)))
  
  (define (first-cell-in-col M col)
    (let ([xy (cellref->numbers (car M))])
      (numbers->cellref (list (+ (sub1 (car xy)) col) (cadr xy)))))
  
  (define (last-cell-in-col M col)
    (let ([xy-upper (cellref->numbers (car M))]
          [xy-lower (cellref->numbers (cadr M))])
      (numbers->cellref (list (+ (sub1 (car xy-upper)) col) (cadr xy-lower)))))
  
  (define (split-large-args l)
    (let ([reversed-l (reverse l)])
      (list (list (first reversed-l)) (reverse (rest reversed-l)))))
  
  (define (split-circular-non-circular all-formulas)
    (let ([visited (make-hash-table)])
      (letrec ([is-circular?
                (lambda (f fs)
                  (ormap 
                   (lambda (dep)
                     (cond [(in-hash? visited dep)
                            (let ([circularity (hash-table-get visited dep)])
                              (cond [(eq? 'circular circularity) #t]
                                    [else #f]))]
                           [else
                            (let ([circularity
                                   (or (memq dep fs)
                                       (let ([dep_f (assq dep all-formulas)])
                                         (and dep_f
                                              (is-circular? dep_f (cons dep fs)))))])
                              (cond [circularity
                                     (hash-table-put! visited dep 'circular)]
                                    [else
                                     (hash-table-put! visited dep 'non-circular)])
                              circularity)]))
                   (formula-dependencies (cadr f))))])
        (split-list (reverse all-formulas) (lambda (f) (is-circular? f (list (car f))))))))
  
  (define (computation-errors hashed-units all-formula-texts all-formulas)
    (filter 
     (lambda (l) (not (empty? l)))
     (hash-table-map
      hashed-units
      (lambda (cell unit)
        (if (is-error-unit? unit)
            (let ([o (open-output-string)])
              (cond [(equal? 'error/propagated (first (first unit)))
                     (fprintf o "ERROR propagated from cell: ~a"
                              (second (first unit)))]
                    [else
                     (fprintf o "ERROR computed: ~n~a " (car (first unit)))
                     (fprintf o ", cell formula: ~a~n" 
                              (lookup-formula-text cell all-formula-texts))])
              (list cell 
                    unit 
                    (get-output-string o) 
                    (formula-dependencies (cadr (assq cell all-formulas)))))
            empty)))))
  
  (define (mismatch-errors hashed-units all-units all-formula-texts all-formulas)
    (foldl 
     (lambda (annot mismatches)
       (let* ([cell (car annot)]
              [actual (hash-table-get hashed-units cell)])
         (cond [(not (or (is-error-unit? actual)
                         (empty-unit? (cadr annot))
                         (equal? actual (cadr annot))))
                (let ([o (open-output-string)])
                  (fprintf o "MISMATCH computed:~n~a " (unit-pp actual))
                  (fprintf o "cell-formula: ~a~n" (lookup-formula-text cell all-formula-texts))
                  (cons (list cell 
                              actual 
                              (get-output-string o) 
                              (formula-dependencies (cadr (assq cell all-formulas))))
                        mismatches))]
               [else mismatches])))
     empty
     all-units))
  
  (define (unit-pp unit)
    (cond [(empty-unit? unit) "()"]
          [else
           (let* ([unit-top-bottom (split-list unit (lambda (u) (> (second u) 0)))]
                  [unit-top (first unit-top-bottom)]
                  [unit-bottom (map (lambda (u) (list (first u) (- 0 (second u))))
                                    (second unit-top-bottom))]
                  [to-string
                   (lambda (unit)
                     (foldl 
                      (lambda (u visited)
                        (string-append
                         (cond [(> (second u) 1)
                                (string-append "-" (symbol->string (first u)) 
                                               "^" (number->string (second u)))]
                               [else
                                (string-append "-" (symbol->string (first u)))])
                         visited))
                      ""
                      unit))]
                  [top-string (to-string unit-top)]
                  [bottom-string (to-string unit-bottom)]
                  [top-len (string-length top-string)]
                  [bottom-len (string-length bottom-string)])
             (cond [(and (= 0 top-len) (= 0 bottom-len)) ""]
                   [(= 0 top-len) (substring bottom-string 1 bottom-len)]
                   [(= 0 bottom-len) (substring top-string 1 top-len)]
                   [else
                    (string-append (substring top-string 1 top-len) "/("
                                   (substring bottom-string 1 bottom-len) ")")]))]))
  )

