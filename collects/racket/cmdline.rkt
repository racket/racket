#lang racket/base

;; Minimize imports here, because `raco setup' has to load this file
;; and its dependencies from source

(require (for-syntax racket/base))

(provide command-line parse-command-line)

(define-syntax (command-line stx)
  (define (id=? x y)
    (eq? (syntax-e x) (syntax-e y)))
  (define (serror msg . detail)
    (apply raise-syntax-error #f msg stx detail))
  (define (extract-one what args . detail)
    (if (null? args)
        (apply serror (format "missing ~a" what) detail)
        (values (car args) (cdr args))))
  (define (extract-list stx/list pred)
    (let loop ([xs null]
               [rest (if (syntax? stx/list) (syntax->list stx/list) stx/list)])
      (if (and (pair? rest) (pred (car rest)))
          (loop (cons (car rest) xs) (cdr rest))
          (values (reverse xs) rest))))
  (define (formal-names l)
    (map (lambda (a)
           (datum->syntax
            (quote-syntax here)
            (let ([s (symbol->string (syntax-e a))])
              (if (char=? #\* (string-ref s (sub1 (string-length s))))
                  (substring s 0 (sub1 (string-length s)))
                  s))
            #f))
         l))
  (define (extract-arg kw lst default)
    (if (and (pair? lst)
             (eq? kw (syntax-e (car lst))))
        (if (null? (cdr lst))
            (serror (format "missing expression for ~a" kw) (car lst))
            (values (cadr lst) (cddr lst)))
        (values default lst)))
  (define (up-to-next-keyword lst)
    (cond
     [(null? lst) null]
     [(keyword? (syntax-e (car lst))) null]
     [else (cons (car lst) (up-to-next-keyword (cdr lst)))]))
  (define (at-next-keyword lst)
    (cond
     [(null? lst) null]
     [(keyword? (syntax-e (car lst))) lst]
     [else (at-next-keyword (cdr lst))]))
  (let ([lst (syntax->list stx)])
    (unless lst
      (raise-syntax-error #f "bad syntax (misuse of `.')" stx))
    (let*-values ([(lst) (cdr lst)]
                  [(prog-name-expr lst)
                   (extract-arg '#:program lst #'(find-system-path 'run-file))]
                  [(argv-expr lst)
                   (extract-arg '#:argv lst #'(current-command-line-arguments))])
      (let-values ([(table args)
                    (let loop ([lst lst] [accum null])
                      (if (null? lst)
                          (loop (syntax->list #'(#:args () (void))) accum)
                          (let ([a (syntax-e (car lst))]
                                [pieces (up-to-next-keyword (cdr lst))])
                            (case a
                              [(#:help-labels)
                               (for ([x (in-list pieces)])
                                 (unless (string? (syntax-e x))
                                   (serror "#:help-labels clause contains non-string"
                                           x)))
                               (loop (at-next-keyword (cdr lst))
                                     (cons (list* #'list #`(quote help-labels) pieces)
                                           accum))]
                              [(#:ps)
                               (for ([x (in-list pieces)])
                                 (unless (string? (syntax-e x))
                                   (serror "#:ps clause contains non-string"
                                           x)))
                               (loop (at-next-keyword (cdr lst))
                                     (cons (list* #'list #`(quote ps) pieces)
                                           accum))]
                              [(#:once-each #:once-any #:multi #:final)
                               (let ([sublines
                                      (let slloop ([sublines pieces])
                                        (if (null? sublines)
                                            #'()
                                            (with-syntax ([looped (slloop (cdr sublines))]
                                                          [subline
                                                           (with-syntax 
                                                               ([flags 
                                                                 (syntax-case (car sublines) ()
                                                                   [((flag ...) . rest)
                                                                    (begin
                                                                      (unless (andmap 
                                                                               (lambda (x) (string? (syntax-e x)))
                                                                               (syntax->list #'(flag ...)))
                                                                        (serror 
                                                                         "flag specification is not a string or sequence of strings" 
                                                                         #'(flag ...)))
                                                                      #'(flag ...))]
                                                                   [(flag . rest)
                                                                    (string? (syntax-e #'flag))
                                                                    #'(flag)]
                                                                   [else
                                                                    (serror "clause does not start with flags")])])
                                                             (syntax-case* (car sublines) (=>) id=?
                                                               [(_ => a b)
                                                                #'(list 'flags a b)]
                                                               [(_ rest ...)
                                                                (let*-values ([(formals rest)
                                                                               (extract-list #'(rest ...) identifier?)]
                                                                              [(helps rest)
                                                                               (cond
                                                                                [(not (pair? rest))
                                                                                 (serror "missing help string(s)" (car sublines))]
                                                                                [(string? (syntax-e (car rest)))
                                                                                 (values (list (car rest)) (cdr rest))]
                                                                                [(syntax->list (car rest))
                                                                                 => (lambda (l)
                                                                                      (values l (cdr rest)))]
                                                                                [else
                                                                                 (serror "missing help string(s)" (car sublines))])]
                                                                              [(expr1 rest)
                                                                               (extract-one
                                                                                "handler body expressions" rest (car sublines))])
                                                                  (with-syntax ([formals formals]
                                                                                [formal-names (formal-names formals)]
                                                                                [helps helps]
                                                                                [expr1 expr1]
                                                                                [rest rest])
                                                                    #'(list 'flags
                                                                            (lambda (flag . formals) expr1 . rest)
                                                                            (cons (list . helps) 'formal-names))))]))])
                                              #'(subline . looped))))])
                                 (loop (at-next-keyword (cdr lst))
                                       (cons (list* #'list
                                                    #`(quote #,(string->symbol (keyword->string a)))
                                                    sublines)
                                             accum)))]
                              [(#:args)
                               (when (null? pieces)
                                 (serror "#:args clause missing formals" (car lst)))
                               (let ([formal-names
                                      (let loop ([f (car pieces)])
                                        (syntax-case f ()
                                          [() null]
                                          [(arg . rest)
                                           (identifier? #'arg)
                                           (cons #'arg (loop #'rest))]
                                          [([arg def] . rest)
                                           (identifier? #'arg)
                                           (cons #'[arg def] (loop #'rest))]
                                          [arg
                                           (identifier? #'arg)
                                           (list #'arg)]
                                          [else
                                           (serror "bad formals for #:args" (car pieces))]))])
                                 (when (null? (cdr pieces))
                                   (serror "#:args clause missing body after formals" (car lst)))
                                 (unless (null? (at-next-keyword (cdr lst)))
                                   (serror "#:args must not be followed by another keyword" (car lst)))
                                 (with-syntax ([formals (car pieces)]
                                               [formal-names (map (lambda (x)
                                                                    (let ([d (syntax->datum x)])
                                                                      (symbol->string
                                                                       (if (pair? d) (car d) d))))
                                                                  formal-names)]
                                               [body (cdr pieces)])
                                   (values (reverse accum)
                                           (list #'(lambda (accume . formals) . body)
                                                 (syntax 'formal-names)))))]
                              [(#:handlers)
                               (let ([len (length pieces)])
                                 (when (len . < . 1)
                                   (serror "missing finish-proc expression for #:handlers" (car lst)))
                                 (when (len . < . 2)
                                   (serror "missing arg-strings expression for #:handlers" (car lst)))
                                 (when (len . > . 4)
                                   (let ([e (list-ref pieces 4)])
                                     (if (keyword? (syntax-e e))
                                         (serror "#:handlers must not be followed by another keyword" e)
                                         (serror "unexpected expression for #:handlers" e)))))
                               (values (reverse accum) pieces)]
                              [else
                               (serror "expected a clause keyword, such as #:multi or #:args" (car lst))]))))])
         (with-syntax ([program-name prog-name-expr]
                       [argv argv-expr]
                       [table table]
                       [args args])
           #'(parse-command-line program-name argv (list . table) . args))))))

(define (print-args port l f)
  (let loop ([l l]
             [a (let a-c ([a (procedure-arity f)])
                  (cond [(number? a) (cons (sub1 a) (sub1 a))]
                        [(arity-at-least? a)
                         (let ([v (sub1 (arity-at-least-value a))])
                           (cons v v))]
                        [else (let ([r (map a-c a)])
                                (cons (apply min (map car r))
                                      (apply max (map cdr r))))]))])
    (unless (null? l)
      (fprintf port " ~a<~a>~a"
               (if (positive? (car a)) "" "[")
               (car l)
               (if (positive? (car a)) "" "]"))
      (unless (positive? (cdr a)) (fprintf port " ..."))
      (loop (cdr l) (cons (sub1 (car a)) (sub1 (cdr a)))))))

(define (procedure-arity-includes-at-least? p n)
  (let a-c ([a (procedure-arity p)])
    (cond [(number? a) (>= a n)]
          [(arity-at-least? a) #t]
          [else (ormap a-c a)])))

(define (program-name program)
  (string->symbol (if (path? program)
                    (let-values ([(base name dir?) (split-path program)])
                      (if (path? name)
                        (path-element->string name)
                        (path->string program)))
                    program)))

(define (parse-command-line
         program arguments0 table finish finish-help
         [help (lambda (s) (display s) (exit 0))]
         [unknown-flag (lambda (flag)
                         (raise-user-error (program-name program)
                                           "unknown switch: ~a" flag))])
  (define arguments
    (if (vector? arguments0) (vector->list arguments0) arguments0))
  (define (bad-table fmt . args)
    (raise-type-error
     'parse-command-line
     (format "table as a list of flag-list/procedure pairs (~a)"
             (apply format fmt args))
     table))
  (unless (or (string? program) (path? program))
    (raise-type-error 'parse-command-line "program name string" program))
  (unless (and (list? arguments)
               (andmap string? arguments))
    (raise-type-error 'parse-command-line "argument vector/list of strings"
                      arguments0))
  (unless (list? table)
    (raise-type-error 'parse-command-line "table of spec sets" table))
  (for ([spec (in-list table)])
    (unless (and (list? spec) (pair? spec))
      (bad-table "spec-set must be a non-empty list: ~a" spec))
    (unless (memq (car spec) '(once-any once-each multi final help-labels ps))
      (bad-table "spec-set type must be 'once-any, 'once-each, 'multi, 'final, 'help-labels, or 'ps: ~a"
                 (car spec)))
    (for ([line (in-list (cdr spec))])
      (if (memq (car spec) '(help-labels ps))
        (unless (string? line)
          (bad-table "~a line must be a string: ~e" (car spec) line))
        (begin
          (unless (and (list? line) (= (length line) 3))
            (bad-table "spec-line must be a list of at three or four items: ~e" line))
          (unless (list? (car line))
            (bad-table "flags part of a spec-line must be a list: ~e" (car line)))
          (for ([flag (in-list (car line))])
            (unless (string? flag)
              (bad-table "flag must be a string: ~e" flag))
            (unless (regexp-match? #rx"^([-+][^-+]$|(--|[+][+])[^-+])" flag)
              (bad-table "no ill-formed flags: ~e" flag))
            (when (regexp-match? #rx"^[-+][0-9]*([.][0-9]*)?$" flag)
              (bad-table "no ill-formed flags: ~e" flag))
            (when (regexp-match? #rx"^(-h|--help)$" flag)
              (bad-table "no pre-defined flags: ~e" flag)))
          (unless (procedure? (cadr line))
            (bad-table "second item in a spec-line must be a procedure: ~e"
                       (cadr line)))
          (let ([a (procedure-arity (cadr line))]
                [h (caddr line)]
                [l (length (caddr line))])
            (cond
              [(number? a)
               (unless (>= a 1)
                 (bad-table "flag handler procedure must take at least 1 argument: ~e" 
                            (cadr line)))]
              [(not (arity-at-least? a))
               (bad-table "flag handler procedure cannot have multiple cases: ~e"
                          (cadr line))])
            (unless (and (pair? h)
                         (or (string? (car h)) (andmap string? (car h)))
                         (andmap string? (cdr h)))
              (bad-table "spec-line help section must be ~a"
                         "a list of string-or-string-list and strings"))
            (unless (if (number? a)
                      (= a l)
                      (and (>= l 1) (>= l (arity-at-least-value a))))
              (bad-table "spec-line help list strings must match procedure arguments")))))))
  (unless (and (procedure? finish)
               (procedure-arity-includes-at-least? finish 1))
    (raise-type-error 'parse-command-line "finish procedure accepting at least 1 argument" finish))
  (unless (and (list? finish-help) (andmap string? finish-help))
    (raise-type-error 'parse-command-line "argument help list of strings" finish-help))
  (unless (and (procedure? help) (procedure-arity-includes? help 1))
    (raise-type-error 'parse-command-line "help procedure of arity 1" help))
  (unless (and (procedure? unknown-flag) (procedure-arity-includes? unknown-flag 1)
               (let ([a (procedure-arity unknown-flag)])
                 (or (number? a) (arity-at-least? a))))
    (raise-type-error 'parse-command-line "unknown-flag procedure of simple arity, accepting 1 argument (an perhaps more)" unknown-flag))

  (letrec ([a (procedure-arity finish)]
           [l (length finish-help)]
           [a-c (lambda (a)
                  (or (and (number? a) (sub1 a))
                      (and (arity-at-least? a)
                           (max 1 (arity-at-least-value a)))
                      (and (list? a) (apply max (map a-c a)))))])
    (unless (= (a-c a) l)
      (error 'parse-command-line "the length of the argument help string list does not match the arity of the finish procedure")))

  (let* ([finalled? #f] ; set to true when 'once-final is seen
         [once-spec-set
          (lambda (lines)
            (let ([set (mcons #f (apply append (map car lines)))])
              (map
               (lambda (line) (cons set line))
               lines)))]
         [first? (lambda (x lst)
                   (and (pair? lst) (eq? x (car lst))))]
         [last? (lambda (x lst)
                  (and (pair? lst)
                       (let loop ([l lst])
                         (if (pair? (cdr l))
                           (loop (cdr l))
                           (eq? x (car l))))))]
         [table
          ;; list of (list <once-set> <spec-line> ...)
          ;; If <once-set> is #f, then flags in <spec-line> are allowed
          ;;  any number of times.
          ;; If <once-set> is 'final, then its like #f, and `finalled?' should
          ;;  be set.
          ;; Otherwise, <once-set> is (mcons <bool> (list <string> ...)) where <bool>
          ;;  starts as #f and is mutated to #t when one of <string> is
          ;;  matched.
          (apply
           append
           (list
            (list #f
                  (list "--help" "-h")
                  (lambda (f)
                    (let ([sp (open-output-string)])
                      (fprintf sp "~a [ <option> ... ]" (program-name program))
                      (print-args sp finish-help finish)
                      (fprintf sp "\n where <option> is one of\n")
                      (for ([set (in-list table)] ; the original table
                            #:unless (eq? (car set) 'ps))
                        (if (eq? (car set) 'help-labels)
                          (for ([line (in-list (cdr set))])
                            (fprintf sp " ~a\n" line))
                          (for ([line (in-list (cdr set))])
                            (let* ([helps (caaddr line)]
                                   [helps (if (string? helps) (list helps) helps)])
                              (for ([help (in-list helps)])
                                (fprintf sp
                                         (cond [(and (eq? (car set) 'once-any)
                                                     (pair? (cddr set)))
                                                (cond
                                                  [(and (first? line (cdr set))
                                                        (first? help helps))
                                                   "/"]
                                                  [(and (last? line (cdr set))
                                                        (last? help helps))
                                                   "\\"]
                                                  [else "|"])]
                                               [(and (memq (car set) '(multi final))
                                                     (first? help helps))
                                                "*"]
                                               [else " "]))
                                (if (first? help helps)
                                  (begin
                                    (let loop ([flags (car line)])
                                      (let ([flag (car flags)])
                                        (fprintf sp " ~a" flag)
                                        (print-args sp (cdaddr line) (cadr line)))
                                      (unless (null? (cdr flags))
                                        (fprintf sp ",")
                                        (loop (cdr flags))))
                                    (fprintf sp " :"))
                                  (fprintf sp "  "))
                                (fprintf sp " ~a\n" help))))))
                      (fprintf sp "  --help, -h : Show this help\n")
                      (fprintf sp "  -- : Do not treat any remaining argument as a switch (at this level)\n")
                      (when (or (assq 'multi table) (assq 'final table))
                        (fprintf sp " * Asterisks indicate options allowed multiple times.\n"))
                      (when (assq 'once-any table)
                        (fprintf sp " /|\\ Brackets indicate mutually exclusive options.\n"))
                      (fprintf sp " Multiple single-letter switches can be combined after one `-'; for\n")
                      (fprintf sp "  example: `-h-' is the same as `-h --'\n")
                      (for ([set (in-list table)] ; the original table
                            #:when (eq? (car set) 'ps))
                        (for ([line (in-list (cdr set))])
                          (fprintf sp " ~a\n" line)))
                      (help (get-output-string sp))))
                  (list "Help")))
           (for/list ([spec (in-list table)])
             (cond
               [(eq? (car spec) 'once-each)
                (apply
                 append
                 (map (lambda (line) (once-spec-set (list line)))
                      (cdr spec)))]
               [(eq? (car spec) 'once-any)
                (once-spec-set (cdr spec))]
               [(eq? (car spec) 'help-labels)
                null]
               [(eq? (car spec) 'multi)
                (map
                 (lambda (line) (cons #f line))
                 (cdr spec))]
               [(eq? (car spec) 'final)
                (map
                 (lambda (line) (cons 'final line))
                 (cdr spec))])))]
         [done
          (lambda (args r-acc)
            (let ([options (reverse r-acc)]
                  [c (length args)])
              (if (procedure-arity-includes? finish (add1 c))
                (apply finish options args)
                (raise-user-error
                 (program-name program)
                 "expects~a on the command line, given ~a argument~a~a"
                 (if (null? finish-help)
                     " no arguments"
                     (let ([s (open-output-string)])
                       (parameterize ([current-output-port s])
                         (print-args s finish-help finish))
                       (let ([s (get-output-string s)])
                         (if (equal? 2 (procedure-arity finish))
                             (format " 1~a" s)
                             s))))
                 c
                 (cond [(zero? c) "s"] [(= c 1) ": "] [else "s: "])
                 (let loop ([args args])
                   (if (null? args)
                       ""
                       (string-append (car args) " " (loop (cdr args)))))))))]
         [call-handler
          (lambda (handler flag args r-acc k)
            (let* ([a (procedure-arity handler)]
                   [remaining (length args)]
                   [needed (if (number? a)
                             (sub1 a)
                             (sub1 (arity-at-least-value a)))]
                   [use (if (number? a)
                          (sub1 a)
                          remaining)])
              (if (< remaining needed)
                (raise-user-error (program-name program)
                                  "the ~s option needs ~a argument~a, but ~a~a provided"
                                  flag needed (if (> needed 1) "s" "")
                                  (if (zero? remaining) "" "only ")
                                  remaining)
                (let ([v (apply handler
                                flag
                                (let loop ([n use][args args])
                                  (if (zero? n)
                                    null
                                    (cons (car args)
                                          (loop (sub1 n) (cdr args))))))])
                  (k (list-tail args use)
                     (if (void? v) r-acc (cons v r-acc)))))))]
         [handle-flag
          (lambda (flag args r-acc orig-multi k)
            (let loop ([table table])
              (cond
                [(null? table)
                 (call-handler unknown-flag flag args r-acc k)]
                [(member flag (cadar table))
                 (when (eq? 'final (caar table))
                   (set! finalled? #t))
                 (when (mpair? (caar table))
                   (let ([set (caar table)])
                     (if (mcar set)
                       (let ([flags (mcdr set)])
                         (raise-user-error
                          (program-name program)
                          (let ([s (if (= 1 (length flags))
                                     (format "the ~a option can only be specified once" (car flags))
                                     (format "only one instance of one option from ~a is allowed" flags))])
                            (if (and orig-multi
                                     (not (equal? flag orig-multi)))
                              (format "~a; note that ~s is shorthand for ~s, in contrast to ~s"
                                      s
                                      orig-multi
                                      (let loop ([prefix (string-ref orig-multi 0)]
                                                 [flags (string->list (substring orig-multi 1 (string-length orig-multi)))]
                                                 [sep ""])
                                        (if (null? flags)
                                          ""
                                          (format "~a~a~a~a" sep prefix (car flags)
                                                  (loop prefix (cdr flags) " "))))
                                      (string-append (substring orig-multi 0 1) orig-multi))
                              s))))
                       (set-mcar! set #t))))
                 (call-handler (caddar table) flag args r-acc k)]
                [else (loop (cdr table))])))])
    (let loop ([args arguments][r-acc null])
      (if (null? args)
        (done args r-acc)
        (let ([arg (car args)]
              [rest (cdr args)])
          (cond
            [finalled?
             (done args r-acc)]
            [(regexp-match #rx"^[-+][0-9]*(|[.][0-9]*)$" arg)
             (done args r-acc)]
            [(regexp-match "^--$" arg)
             (done (cdr args) r-acc)]
            [(regexp-match "^[-+][-+]" arg)
             (handle-flag arg rest r-acc #f loop)]
            [(regexp-match "^[-+]." arg)
             (let a-loop ([s (string->list (substring arg 1 (string-length arg)))]
                          [rest rest]
                          [r-acc r-acc])
               (if (null? s)
                 (loop rest r-acc)
                 (handle-flag (string (string-ref arg 0) (car s)) 
                              rest r-acc
                              arg
                              (lambda (args r-acc)
                                (a-loop (cdr s) args r-acc)))))]
            [else (done args r-acc)]))))))
