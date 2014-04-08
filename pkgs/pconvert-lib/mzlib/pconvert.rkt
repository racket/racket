
(module pconvert mzscheme
  
  (require (only racket/base sort)
           compatibility/mlist
	   "pconvert-prop.rkt"
           racket/class
           racket/undefined)
  
  (provide show-sharing
           constructor-style-printing
           quasi-read-style-printing
           abbreviate-cons-as-list
           whole/fractional-exact-numbers
           booleans-as-true/false
           named/undefined-handler
           use-named/undefined-handler
           add-make-prefix-to-constructor
           
           print-convert
           print-convert-expr
           build-share
           get-shared
           current-read-eval-convert-print-prompt 
           install-converting-printer
           
           current-build-share-name-hook
           current-build-share-hook
           current-print-convert-hook)
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; the value stored in the hash table.  Contains the name
  ;; <which is a number unless we are in donkey and it already has a name>
  ;; and whether or not it is shared in the expr.
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-struct share-info (name shared?))
  
  (define boolean-filter (lambda (x) (and x #t)))
  
  (define show-sharing (make-parameter #t boolean-filter))
  (define constructor-style-printing (make-parameter #f boolean-filter))
  (define quasi-read-style-printing (make-parameter #t boolean-filter))
  (define abbreviate-cons-as-list (make-parameter #t boolean-filter))
  (define whole/fractional-exact-numbers (make-parameter #f boolean-filter))
  (define booleans-as-true/false (make-parameter #t boolean-filter))
  (define use-named/undefined-handler (make-parameter (lambda (x) #f)))
  (define named/undefined-handler (make-parameter (lambda (x) #f)))
  (define add-make-prefix-to-constructor (make-parameter #f))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; share-hash is the hash-table containing info on what cons cells
  ;; of the expression are shared.
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; sometimes you want to go ahead and start displaying a shared
  ;; expression rather than just showing its name.  For instance, in
  ;; the shared list, you want (shared ((-1- (list 1 2))... not
  ;; (shared ((-1- -1-) ...
  ;; expand-shared? controls this
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-struct convert-share-info (share-hash expand-shared?))
  
  (define current-build-share-name-hook 
    (make-parameter (let ([original-build-share-name-hook (lambda (e) #f)]) original-build-share-name-hook)
                    (lambda (f)
                      (unless (procedure-arity-includes? f 1)
                        (raise-argument-error 'current-build-share-name-hook "(procedure-arity-includes/c 1)" f))
                      f)))
  (define current-build-share-hook 
    (make-parameter (lambda (e base sub) (base e))
                    (lambda (f)
                      (unless (procedure-arity-includes? f 3)
                        (raise-argument-error 'current-build-share-hook "(procedure-arity-includes/c 3)" f))
                      f)))
  (define current-print-convert-hook 
    (make-parameter (lambda (e base sub) (base e))
                    (lambda (f)
                      (unless (procedure-arity-includes? f 3)
                        (raise-argument-error 'current--hook "(procedure-arity-includes/c 3)" f))
                      f)))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; builds the hash table
  ;; --------- THIS PROCEDURE IS EXPORTED ----------
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define build-share
    (lambda (expr)
      (letrec
          ([share-cnt 0]
           [share-hash (make-hash-table)]
           [csi (make-convert-share-info share-hash #f)]
           [hash
            (lambda (obj)
              (let ([name ((current-build-share-name-hook) obj)])
                (hash-table-put! share-hash obj
                                 (make-share-info (if name (car name) share-cnt) #f)))
              (set! share-cnt (add1 share-cnt)))]
           [build-sub
            (lambda (expr)
              (let/ec k
                (if (or (equal? expr "")
                        (equal? expr #()))
                    (k #f)
                    (let ([val (hash-table-get share-hash expr 
                                               (lambda ()
                                                 (hash expr)
                                                 (k #f)))])
                      (when val
                        (set-share-info-shared?! val #t))
                      val))))]
           [build
            (lambda (expr)
              ((current-build-share-hook)
               expr
               (lambda (expr)
                 (cond
                   [(print-converter? expr)
                    (unless (build-sub expr)
                      ((print-converter-proc expr) expr build))]
                   [(or (number? expr)
                        (symbol? expr)
                        (boolean? expr)
                        (char? expr) (void? expr)
                        (null? expr)
                        ;; #<undefined> test - yuck, and maybe not worth checking
                        ;; anymore, since undefined generally shouldn't escape
                        (eq? undefined expr) 
                        )
                    'atomic]
                   [(and (not (struct? expr))  ;; struct names are the wrong thing, here
                         (not (regexp? expr))
                         (not (byte-regexp? expr))
                         (not (procedure? expr))
                         (not (promise? expr))
                         (not (object? expr))
                         (not (port? expr))
                         (not (class? expr))
                         (object-name expr))
                    'atomic]
                   [(box? expr) 
                    (unless (build-sub expr)
                      (build (unbox expr)))]
                   [(hash-table? expr) 
                    (unless (build-sub expr)
                      (hash-table-for-each 
                       expr
                       (lambda (key value)
                         (build key)
                         (build value))))]
                   [(pair? expr)  
                    (unless (build-sub expr)
                      (build (car expr))
                      (build (cdr expr)))]
                   [(mpair? expr)  
                    (unless (build-sub expr)
                      (build (mcar expr))
                      (build (mcdr expr)))]
                   [(vector? expr) 
                    (unless (build-sub expr)
                      (for-each build (vector->list expr)))]
                   [(struct? expr)
                    (unless (build-sub expr)
                      (for-each build (vector->list (struct->vector expr))))]
                   [else (build-sub expr)]))
               build-sub))])
        (build expr)
        csi)))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; creates a distinctive symbol out of a name (usually just a number)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define map-share-name
    (lambda (name)
      (string->symbol (format "-~s-" name))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; prints an expression given that it has already been hashed. This
  ;; does not include the list of shared items.
  ;; --------- THIS PROCEDURE IS EXPORTED ----------
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define print-convert-expr
    (lambda (csi expr unroll-once?)
      (letrec
          ([mpair-mode? (is-mpair-mode?)]
           [share-hash (convert-share-info-share-hash csi)]
           [find-hash
            (lambda (expr)
              (hash-table-get share-hash expr (lambda () #f)))]
           [shared?
            (lambda (expr)
              (let* ([info (find-hash expr)]
                     [ans (and info
                               (share-info-shared? info))])
                ans))]
           
           [make-list
            (lambda (f n)
              (letrec ([helper
                        (lambda (n l)
                          (cond [(zero? n) l]
                                [else (helper (sub1 n) (cons (f n) l))]))])
                (helper n null)))]
           [make-lambda-helper
            (lambda (arity)
              (cond
                [(arity-at-least? arity)
                 (let ([v (arity-at-least-value arity)])
                   (if (zero? v)
                       'args
                       (append (make-lambda-helper v) 'args)))]
                [(list? arity)
                 (map (lambda (x)
                        (list (make-lambda-helper x) '...))
                      arity)]
                [else (make-list
                       (lambda (x)
                         (string->symbol
                          (string-append "a" (number->string x))))
                       arity)]))]
           [use-quasi-quote? (not (constructor-style-printing))]
           [use-read-syntax (quasi-read-style-printing)]
           [doesnt-contain-shared-conses
            (lambda (expr)
              (cond
               [(and (pair? expr)
                     (shared? expr))
                #f]
               [(pair? expr)
                (doesnt-contain-shared-conses (cdr expr))]
               [else #t]))]
           [doesnt-contain-shared-mconses
            (lambda (expr)
              (cond
               [(and (mpair? expr)
                     (shared? expr))
                #f]
               [(mpair? expr)
                (doesnt-contain-shared-mconses (mcdr expr))]
               [else #t]))]
           [get-whole/frac
            (lambda (exact-num)
              (let ([split 
                     (lambda (real)
                       (let* ([num (numerator (abs real))]
                              [den (denominator (abs real))]
                              [sign (if (< real 0) - +)])
                         (values (sign (quotient num den))
                                 (sign (* (if (negative? num) -1 1)
                                          (/ (modulo num den) den))))))])
                (let-values ([(whole frac) (split (real-part exact-num))]
                             [(whole-i frac-i) (split (imag-part exact-num))])
                  (values whole frac whole-i frac-i))))]
           [print
            (lambda (in-quasiquote? first-time)
              (lambda (expr)
                (letrec
                    ([lookup (find-hash expr)]
                     [recur (print in-quasiquote? #f)]
                     [self-quoting?
                      (lambda (expr)
                        (or (and (number? expr)
                                 (or (inexact? expr)
                                     (not (whole/fractional-exact-numbers))
                                     (and (real? expr)
                                          (or (let-values ([(whole frac whole-i frac-i) 
                                                            (get-whole/frac expr)])
                                                (and (or (zero? whole)
                                                         (zero? frac))))))))
                            (and (symbol? expr)
                                 (not (eq? expr 'quasiquote))
                                 (not (eq? expr 'unquote))
                                 (not (eq? expr 'unquote-splicing)))
                            (char? expr)
                            (string? expr)
                            (not expr)
                            (eq? #t expr)))]
                     [quasi-read-style
                      (lambda ()
                        (cond
                          [(box? expr) (box (recur (unbox expr)))]
                          [(vector? expr) (apply vector (map recur (vector->list expr)))]
                          [else (quasi-style)]))]
                     [quasi-style
                      (lambda ()
                        (cond
                          [(null? expr) '()]
                          [(pair? expr) 
                           (cons (recur (car expr)) (recur (cdr expr)))]
                          [(and mpair-mode?
                                (mpair? expr))
                           ;; generate pairs, which will be converted back to mpairs later
                           (cons (recur (mcar expr)) (recur (mcdr expr)))]
                          [(self-quoting? expr) expr]
                          [else `(,'unquote ,((print #f first-time) expr))]))]
                     
                     [guard/quasiquote
                      (lambda (f)
                        (cond
                          [use-quasi-quote?
                           `(,'quasiquote ,(if use-read-syntax
                                               ((print #t first-time) expr)
                                               ((print #t first-time) expr)))]
                          [else
                           (f)]))]
                     [constructor-style
                      (let ([build-named
                              (lambda (expr build-unnamed)
                                (let ([answer (and (not (struct? expr))
                                                   (object-name expr))])
                                  (cond
                                    [(not answer)
                                     (build-unnamed)]
                                    [(let/ec k
				       (eq?
					(namespace-variable-value
					 answer
					 #t
                                         (lambda () (k #f)))
					expr))
                                     answer]
                                    [((use-named/undefined-handler) expr)
                                     ((named/undefined-handler) expr)]
                                    [else
                                     (build-unnamed)])))])
                        (lambda ()
                          ((current-print-convert-hook)
                           expr
                           (lambda (expr)
                             (cond
                               [(print-converter? expr)
                                ((print-converter-proc expr) expr recur)]
                               [(null? expr) (guard/quasiquote (lambda () 'empty))]
                               [(and (abbreviate-cons-as-list)
                                     (list? expr)
                                     (or (and first-time
                                              (doesnt-contain-shared-conses (cdr expr)))
                                         (doesnt-contain-shared-conses expr)))
                                (guard/quasiquote
                                 (lambda ()
                                   `(list ,@(map recur expr))))]
                               [(pair? expr)
                                (guard/quasiquote
                                 (lambda ()
                                   `(cons ,(recur (car expr)) ,(recur (cdr expr)))))]
                               [(and mpair-mode?
                                     (abbreviate-cons-as-list)
                                     (mlist? expr)
                                     (or (and first-time
                                              (doesnt-contain-shared-mconses (mcdr expr)))
                                         (doesnt-contain-shared-mconses expr)))
                                (guard/quasiquote
                                 (lambda ()
                                   `(list ,@(map recur (mlist->list expr)))))]
                               [(mpair? expr)
                                (if mpair-mode?
                                    (guard/quasiquote
                                     (lambda ()
                                       `(cons ,(recur (mcar expr)) ,(recur (mcdr expr)))))
                                    `(mcons ,(recur (mcar expr)) ,(recur (mcdr expr))))]
                               [(weak-box? expr) `(make-weak-box ,(recur (weak-box-value expr)))]
                               [(box? expr)
                                `(,(if (immutable? expr)
                                       'box-immutable
                                       'box)
                                  ,(recur (unbox expr)))]
                               [(hash-table? expr) 
                                (let ([contents
                                       (hash-table-map
                                        expr
                                        (lambda (k v)
                                          `(cons ,(recur k) ,(recur v))))]
                                      [constructor
                                       (cond
                                         [(hash-table? expr 'weak 'equal) 'make-weak-hash]
                                         [(hash-table? expr 'equal) 'make-hash]
                                         [(hash-table? expr 'weak 'eqv) 'make-weak-hasheqv]
                                         [(hash-table? expr 'eqv) 'make-hasheqv]
                                         [(hash-table? expr 'weak) 'make-weak-hasheq]
                                         [(hash-table? expr) 'make-hasheq])])
                                  (if (null? contents)
                                      `(,constructor)
                                      `(,constructor (list ,@contents))))]
                               [(vector? expr) 
                                `(,(if (immutable? expr)
                                       'vector-immutable
                                       'vector)
                                  ,@(map recur (vector->list expr)))]
                               [(symbol? expr) `',expr]
                               [(keyword? expr) `',expr]
                               [(string? expr) expr]
                               [(primitive? expr) (object-name expr)]
                               [(procedure? expr)
                                (build-named
                                 expr
                                 (lambda ()
                                   (let ([arity (procedure-arity expr)])
                                     (if (list? arity)
                                         `(case-lambda . ,(make-lambda-helper arity))
                                         `(lambda ,(make-lambda-helper arity) ...)))))]
                               [(regexp? expr) `(,(if (pregexp? expr) 'pregexp 'regexp)
						 ,(or (object-name expr)
						      '...))]
                               [(byte-regexp? expr) `(,(if (byte-pregexp? expr) 'byte-pregexp 'byte-regexp)
						      ,(or (object-name expr)
							   '...))]
                               [(module-path-index? expr) 
                                (let-values ([(left right) (module-path-index-split expr)])
                                  `(module-path-index-join ,(recur left) ,(recur right)))]
                               [(interface? expr) `(interface ...)]
                               [(class? expr) 
                                (build-named 
                                 expr
                                 (lambda () '(class ...)))]
                               [(object? expr) `(instantiate
                                                    ,(build-named 
                                                      (object-interface expr)
                                                      (lambda () '(class ...)))
                                                  ...)]
                               [(void? expr) '(void)]
                               [(promise? expr) '(delay ...)]
                               [(and (number? expr) (exact? expr))
                                (let-values ([(whole frac whole-i frac-i) (get-whole/frac expr)])
                                  (cond
                                    [(not (whole/fractional-exact-numbers)) expr]
                                    [(and (or (zero? whole)
                                              (zero? frac))
                                          (zero? whole-i)
                                          (zero? frac-i))
                                     expr]
                                    [(real? expr) `(+ ,whole ,frac)]
                                    [(and (or (zero? whole) (zero? frac))
                                          (or (zero? whole-i) (zero? frac-i)))
                                     `(+ ,(real-part expr) (* +1i ,(imag-part expr)))]
                                    [(or (zero? whole-i) (zero? frac-i))
                                     `(+ (+ ,whole ,frac) (* +1i ,(imag-part expr)))]
                                    [(or (zero? whole) (zero? frac))
                                     `(+ ,(real-part expr) (* +1i (+ ,whole-i ,frac-i)))]
                                    [else `(+ (+ ,whole ,frac) (* +1i (+ ,whole-i ,frac-i)))]))]
                               [(eq? expr #f) (if (booleans-as-true/false) 'false #f)]
                               [(eq? expr #t) (if (booleans-as-true/false) 'true #t)]
                               
                               [(and (input-port? expr)
                                     (file-stream-port? expr)
                                     (object-name expr))
                                `(open-input-file ,(object-name expr))]
                               [(and (output-port? expr)
                                     (file-stream-port? expr)
                                     (object-name expr))
                                `(open-output-file ,(object-name expr))]
                               [(port? expr) expr]
                               
                               ;; this case must be next to last, so that all of the
                               ;; things with object-name's fall into the cases above first
                               [(or (print-convert-named-constructor? expr)
				    (object-name expr))
                                (let ([constructor
				       (if (print-convert-named-constructor? expr)
					   (print-convert-constructor-name expr)
					   (let ([name (object-name expr)])
                                             (if (and (symbol? name)
                                                      (not (add-make-prefix-to-constructor)))
                                                 name
                                                 (let ([str-name (if (string? name)
                                                                     name
                                                                     (symbol->string name))])
                                                   (string->symbol (string-append "make-" str-name))))))])
                                  `(,constructor
                                    ,@(map (lambda (x) 
                                             (if (eq? uniq x)
                                                 '...
                                                 (recur x)))
                                           (cdr (vector->list (struct->vector expr uniq))))))]
                               
                               [else expr]))
                           recur)))])
                  (let ([es (convert-share-info-expand-shared? csi)])
                    (set-convert-share-info-expand-shared?! csi #f)
                    (if (and lookup
                             (not es)
                             (not first-time)
                             (share-info-shared? lookup))
                        (let ([name (map-share-name (share-info-name lookup))])
                          (if in-quasiquote?
                              `(,'unquote ,name)
                              name))
                        (if in-quasiquote?
                            (if use-read-syntax
                                (quasi-read-style)
                                (quasi-style))
                            (constructor-style)))))))])
        ((print #f unroll-once?) expr))))

  (define (is-mpair-mode?)
    (not (print-mpair-curly-braces)))
  
  ;; type (improper-list a) = (union (cons (improper-list a) (improper-list a)) null a)
  ;; improper-map : (a -> b) -> (improper-list a) -> (improper-list b)
  (define (improper-map f x)
    (cond
      [(pair? x) (cons (f (car x)) (improper-map f (cdr x)))]
      [(null? x) null]
      [else (f x)]))
  
  (define uniq (gensym))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; these functions get the list of shared items.  If just-circular is
  ;; true, then it will modify the hash table so that the only shared
  ;; items are those that are circular.
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define get-shared-helper
    (lambda (csi)
      (let ([shared '()]
            [share-hash (convert-share-info-share-hash csi)])
        (hash-table-for-each share-hash
                             (lambda (key val)
                               (when (share-info-shared? val)
                                 (set! shared (cons (list key val) shared)))))
        (map (lambda (s)
               (set-convert-share-info-expand-shared?! csi #t)
               (let* ([info (cadr s)]
                      [name (share-info-name info)])
                 (list info
                       (map-share-name name)
                       (print-convert-expr csi (car s) #t))))
             shared))))
  
  ;; --------- THIS PROCEDURE IS EXPORTED ----------
  (define get-shared
    (case-lambda
      [(csi) (get-shared csi #f)]
      [(csi just-circular)
       (let ([shared-listss
              (if just-circular
                  (let ([shared (get-shared-helper csi)])
                    (for-each (lambda (x)
                                (unless (member* (cadr x) (caddr x))
                                  (set-share-info-shared?! (car x) #f)))
                              shared)
                    (get-shared-helper csi))
                  (get-shared-helper csi))]
             [cmp (lambda (x y)
                    (string<? (format "~s" (share-info-name (car x)))
                              (format "~s" (share-info-name (car y)))))])
         (map cdr (sort shared-listss cmp)))]))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; helper function for determining if an item is circular.  In the
  ;; shared list: (shared ((-1- (list 1 2)) (-2- (list -2- 2 3)))), you
  ;; can tell by doing a member* of the first item on the second. In this
  ;; case, the second item in the shared list is circular because -2- appears
  ;; in the value
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define member*
    (lambda (a l)
      (cond [(or (not (pair? l)) (null? l)) #f]
            [(eq? a (car l)) #t]
            [else (or (member* a (car l)) (member* a (cdr l)))])))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; takes an expression and completely converts it to show sharing
  ;; (or if just-circular, just circularity) and special forms.
  ;; --------- THIS PROCEDURE IS EXPORTED ----------
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define print-convert
    (case-lambda
      [(expr) (print-convert expr (not (show-sharing)))]
      [(expr just-circ)
       (let* ([csi (build-share expr)])
         (let ([shared (get-shared csi just-circ)]
               [body (print-convert-expr csi expr #f)])
           (if (null? shared)
               body
               `(shared ,shared ,body))))]))
  
  (define current-read-eval-convert-print-prompt
    (make-parameter "|- "))
  
  (define install-converting-printer
    (lambda ()
      (let ([print (current-print)])
        (current-print (lambda (v)
                         (unless (void? v)
                           (print (print-convert v))))))
      (print-as-expression #f)
      (current-prompt-read (lambda ()
                             (display (current-read-eval-convert-print-prompt))
                             (read-syntax 'STDIN))))))
