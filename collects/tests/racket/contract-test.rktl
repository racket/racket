
(load-relative "loadtest.rktl")
(Section 'contract)

(parameterize ([error-print-width 200])
(let ()

  (define contract-namespace
    (let ([n (make-base-namespace)])
      (parameterize ([current-namespace n])
        (namespace-require '(for-syntax scheme/base))
        (namespace-require '(for-template scheme/base))
        (namespace-require 'scheme/contract)
        (namespace-require 'scheme/set)
        (namespace-require '(only racket/contract/private/arrow procedure-accepts-and-more?))
        (namespace-require '(only racket/contract/private/blame blame-fmt->-string make-blame))
        (namespace-require 'scheme/class)
        (namespace-require 'scheme/promise)
        (namespace-require 'scheme/match))
      n))

  (define (contract-eval x)
    (parameterize ([current-namespace contract-namespace])
      (eval x)))

  (define (contract-compile x)
    (parameterize ([current-namespace contract-namespace])
      (compile x)))

  (define (contract-expand-once x)
    (parameterize ([current-namespace contract-namespace])
      (expand-once x)))

  (define exn:fail:contract:blame-object (contract-eval 'exn:fail:contract:blame-object))
  (define exn:fail:contract:blame? (contract-eval 'exn:fail:contract:blame?))

  (define-syntax (ctest stx)
    (syntax-case stx ()
      [(_ a ...)
       (syntax (contract-eval `(,test a ...)))]))

  (define (contract-error-test name exp exn-ok?)
    (test #t
          name
          (contract-eval `(with-handlers ((exn:fail? (λ (x) (and (,exn-ok? x) #t)))) ,exp))))

  (define (contract-syntax-error-test name exp [reg #rx""])
    (test #t
	  name
	  (contract-eval `(with-handlers ((exn:fail:syntax?
					   (lambda (x) (and (regexp-match ,reg (exn-message x)) #t))))
					 (eval ',exp)))))

  ;; test/spec-passed : symbol sexp -> void
  ;; tests a passing specification
  (define (test/spec-passed name expression)
    (printf "testing: ~s\n" name)
    (parameterize ([compile-enforce-module-constants #f])
      (contract-eval
       `(,test
         (void)
         (let ([for-each-eval (lambda (l) (for-each eval l))]) for-each-eval)
         (list ',expression '(void))))
      (let ([new-expression (rewrite-out expression)])
        (unless (equal? new-expression expression)
          (contract-eval
           `(,test
             (void)
             (let ([for-each-eval (lambda (l) (for-each eval l))]) for-each-eval)
             (list ',new-expression '(void)))))))

    (let/ec k
      (contract-eval
       `(,test (void)
          (let ([for-each-eval (lambda (l) (for-each (λ (x) (eval x)) l))])
            for-each-eval)
          (list ',(rewrite expression k) '(void))))))

  (define (test/spec-passed/result name expression result)
    (printf "testing: ~s\n" name)
    (contract-eval `(,test ',result eval ',expression))
    (let/ec k
      (contract-eval
       `(,test
          ',result
          eval
          ',(rewrite expression k)))))

  ;; rewrites `provide/contract' to use `contract-out'
  (define (rewrite-out orig-exp)
    (let loop ([exp orig-exp])
      (match exp
        [`(module ,modname ,lang ,bodies ...)
         (define at-beginning '())

         ;; remove (and save) the provide/contract declarations
         (define removed-bodies
           (apply
            append
            (for/list ([body (in-list bodies)])
              (match body
                [`(provide/contract . ,args)
                 (set! at-beginning (cons `(provide (contract-out . ,args))
                                          at-beginning))
                 (list)]
                [else
                 (list body)]))))

         ;; insert the provide/contract (rewrite to contract-out) after the
         ;; first require that has 'contract' in it
         (define inserted-bodies
           (apply
            append
            (for/list ([body (in-list removed-bodies)])
              (match body
                [`(require ,(? (λ (x) (and (symbol? x) (regexp-match #rx"contract" (symbol->string x)))) mod))
                 (cons body (reverse at-beginning))]
                [else
                 (list body)]))))

         `(module ,modname ,lang ,@inserted-bodies)]
        [(? list?)
         (map loop exp)]
        [else exp])))

  ;; rewrites `contract' to use opt/c. If there is a module definition in there, we skip that test.
  (define (rewrite exp k)
    (let loop ([exp exp])
      (cond
        [(null? exp) null]
        [(list? exp)
         (case (car exp)
           [(contract) `(contract (opt/c ,(loop (cadr exp))) ,@(map loop (cddr exp)))]
           [(module) (k #f)]
           [else (map loop exp)])]
        [(pair? exp) (cons (loop (car exp))
                           (loop (cdr exp)))]
        [else exp])))

  ;; blame : (or/c 'pos 'neg string?)
  ;;   if blame is a string, expect to find the string (format "blaming: ~a" blame) in the exn message
  (define (test/spec-failed name expression blame)
    (let ()
      (define (has-proper-blame? msg)
        (define reg
          (cond
            [(eq? blame 'pos) #rx"broke its contract[\n:,].*blaming: pos"]
            [(eq? blame 'neg) #rx"blaming: neg"]
            [(string? blame) (string-append "blaming: " (regexp-quote blame))]
            [else #f]))

        (when reg
          (unless (regexp-match? reg msg)
            (eprintf "ACK!! ~s ~s\n" blame msg)
            (custodian-shutdown-all (current-custodian))))
        (and reg (regexp-match? reg msg)))
      (printf "testing: ~s\n" name)
      (contract-eval
       `(,thunk-error-test
          (lambda () ,expression)
          (datum->syntax #'here ',expression)
          (lambda (exn)
            (and (exn:fail:contract:blame? exn)
                 (,has-proper-blame? (exn-message exn))))))
      (let/ec k
        (let ([rewritten (rewrite expression k)])
          (contract-eval
           `(,thunk-error-test
             (lambda () ,rewritten)
             (datum->syntax #'here ',rewritten)
             (lambda (exn)
               (and (exn:fail:contract:blame? exn)
                    (,has-proper-blame? (exn-message exn))))))))))

  (define (test/pos-blame name expression) (test/spec-failed name expression 'pos))
  (define (test/neg-blame name expression) (test/spec-failed name expression 'neg))

  (define (test/well-formed stx)
    (contract-eval
     `(,test (void)
            (let ([expand/ret-void (lambda (x) (expand x) (void))]) expand/ret-void)
            ,stx)))

  (define (test/no-error sexp)
    (contract-eval
    `(,test (void)
       eval
       '(begin ,sexp (void)))))

  (define (test-flat-contract contract pass fail)
    (define (run-three-tests contract)
      (let ([name (if (pair? contract)
                      (car contract)
                      contract)])
        (contract-eval `(,test #t flat-contract? ,contract))
        (test/spec-failed (format "~a fail" name)
                          `(contract ,contract ',fail 'pos 'neg)
                          'pos)
        (test/spec-passed/result
         (format "~a pass" name)
         `(contract ,contract ',pass 'pos 'neg)
         pass)))
    (run-three-tests contract)
    (let/ec k (run-three-tests (rewrite contract k))))

  (define-syntax (test-name stx)
    (syntax-case stx ()
      [(_ name contract)
       #'(do-name-test 'name 'contract)]))

  (define (do-name-test name contract-exp)
    (printf "~s\n" (list 'do-name-test name contract-exp))
    (contract-eval `(,test ,name contract-name ,contract-exp))
    (contract-eval `(,test ,name contract-name (opt/c ,contract-exp))))


  (define (test-obligations quoted-expr expected-props)

    (define (cleanup key obj stx)
      (case key
        [(racket/contract:contract)
         (let ([cleanup-ent
                (λ (x)
                  (sort (map syntax->datum (vector-ref obj x)) string<=? #:key (λ (x) (format "~s" x))))])
         (list key (cleanup-ent 1) (cleanup-ent 2)))]
        [(racket/contract:positive-position racket/contract:negative-position)
         (list key (syntax->datum stx))]
        [(racket/contract:contract-on-boundary) `(racket/contract:contract-on-boundary ,(syntax->datum stx))]
        [(racket/contract:internal-contract) `(racket/contract:internal-contract ,(syntax->datum stx))]
        [else
         (error 'test-obligations "unknown property ~s" key)]))

    (let ([props '()])
      (let ([stx (contract-expand-once quoted-expr)])
        (let loop ([stx stx])
          (cond
            [(syntax? stx)
             (for ([key (in-list (syntax-property-symbol-keys stx))])
               (when (regexp-match #rx"^racket/contract:" (symbol->string key))
                 (set! props (cons (cleanup key (syntax-property stx key) stx)
                                   props))))
             (loop (syntax-e stx))]
            [(pair? stx)
             (loop (car stx))
             (loop (cdr stx))])))
      (test expected-props
            `(obligations-for ,quoted-expr)
            (sort props string<=? #:key (λ (x) (format "~s" x))))))


;
;
;
;                                        ;                                ;    ;;
;                                       ;;                               ;;    ;;
;    ;;;;;   ;;;;   ;;;; ;;;   ;;;;;  ;;;;; ;;; ;;; ;;;; ;;;;   ;;;;;  ;;;;;        ;;;;   ;;;; ;;;
;   ;;;;;;  ;;;;;;  ;;;;;;;;; ;;;;;; ;;;;;; ;;;;;;; ;;;; ;;;;  ;;;;;; ;;;;;; ;;;;  ;;;;;;  ;;;;;;;;;
;  ;;;;;;; ;;;;;;;; ;;;; ;;;; ;;;;    ;;;;  ;;;; ;; ;;;; ;;;; ;;;;;;;  ;;;;  ;;;; ;;;;;;;; ;;;; ;;;;
;  ;;;;    ;;;; ;;; ;;;; ;;;;  ;;;;   ;;;;  ;;;;    ;;;; ;;;; ;;;;     ;;;;  ;;;; ;;;; ;;; ;;;; ;;;;
;  ;;;;;;; ;;;;;;;; ;;;; ;;;;   ;;;;  ;;;;; ;;;;    ;;;; ;;;; ;;;;;;;  ;;;;; ;;;; ;;;;;;;; ;;;; ;;;;
;   ;;;;;;  ;;;;;;  ;;;; ;;;; ;;;;;;  ;;;;; ;;;;    ;;;;;;;;;  ;;;;;;  ;;;;; ;;;;  ;;;;;;  ;;;; ;;;;
;    ;;;;;   ;;;;   ;;;; ;;;; ;;;;;    ;;;; ;;;;     ;;; ;;;;   ;;;;;   ;;;; ;;;;   ;;;;   ;;;; ;;;;
;
;
;

  (test/no-error '(-> integer? integer?))
  (test/no-error '(-> (flat-contract integer?) (flat-contract integer?)))
  (test/no-error '(-> integer? any))
  (test/no-error '(-> (flat-contract integer?) any))

  (test/no-error '(->* (integer?) () (values integer?)))
  (test/no-error '(->* (integer?) () #:rest integer? integer?))
  (test/no-error '(->* (integer?) () #:rest integer? any))
  (test/no-error '(->* ((flat-contract integer?)) () (flat-contract integer?)))
  (test/no-error '(->* ((flat-contract integer?)) () #:rest (flat-contract integer?) (flat-contract integer?)))
  (test/no-error '(->* ((flat-contract integer?)) () #:rest (flat-contract integer?)
                       (values (flat-contract integer?) (flat-contract boolean?))))
  (test/no-error '(->* ((flat-contract integer?)) () #:rest (flat-contract integer?) any))
  (test/no-error '(->* ((flat-contract integer?)) () #:pre #t (flat-contract integer?) #:post #t))

  (test/no-error '(->d ([x integer?]) ([y integer?]) any))
  (test/no-error '(->d ([x integer?]) ([y integer?]) (values [a number?] [b boolean?])))
  (test/no-error '(->d ([x integer?] #:z [z integer?]) ([y integer?] #:w [w integer?]) (range boolean?)))
  (test/no-error '(->d ([x integer?] #:z [z integer?]) ([y integer?] #:w [w integer?]) #:rest rest any/c (range boolean?)))
  (test/no-error '(->d ([x integer?] #:z [z integer?]) #:rest rest any/c (range boolean?)))

  (test/no-error '(->i ([x integer?]) ([y integer?]) any))
  (test/no-error '(->i ([x integer?]) ([y integer?]) (values [a number?] [b boolean?])))
  (test/no-error '(->i ([x integer?] #:z [z integer?]) ([y integer?] #:w [w integer?]) (range boolean?)))
  (test/no-error '(->i ([x integer?] #:z [z integer?]) ([y integer?] #:w [w integer?]) #:rest [rest any/c] (range boolean?)))
  (test/no-error '(->i ([x integer?] #:z [z integer?]) #:rest [rest any/c] (range boolean?)))

  (test/no-error '(unconstrained-domain-> number?))
  (test/no-error '(unconstrained-domain-> (flat-contract number?)))

  (test/no-error '(listof any/c))
  (test/no-error '(listof (lambda (x) #t)))
  (test/no-error '(((lambda (x) x) listof) #t))
  (test/no-error '(non-empty-listof any/c))
  (test/no-error '(non-empty-listof (lambda (x) #t)))


  (test/no-error '(list/c 'x "x" #t #f #\c #rx"a" #rx#"b"))



;
;
;
;                                     ;;;;
;                                    ;;;;;;
;  ;;;;;;;  ;;;; ;;;  ;;;  ;;;       ;;; ;;       ;;;; ;;;    ;;;;   ;;;; ;;;    ;;;
;  ;;;;;;;; ;;;;;;;;; ;;; ;;;;       ;;;;;        ;;;;;;;;;  ;;;;;;  ;;;;;;;;;  ;;;;;
;      ;;;; ;;;; ;;;;  ;;;;;;         ;;; ;;;     ;;;; ;;;; ;;;;;;;; ;;;; ;;;; ;;;; ;;
;   ;;;;;;; ;;;; ;;;;  ;;;;;;        ;;;;;;;      ;;;; ;;;; ;;;; ;;; ;;;; ;;;; ;;;;;;;
;  ;;  ;;;; ;;;; ;;;;   ;;;;;       ;;; ;;;;      ;;;; ;;;; ;;;;;;;; ;;;; ;;;; ;;;;;
;  ;;;;;;;; ;;;; ;;;;   ;;;;        ;;;;;;;;;     ;;;; ;;;;  ;;;;;;  ;;;; ;;;;  ;;;;;;
;   ;; ;;;; ;;;; ;;;;   ;;;;         ;;;;  ;;;    ;;;; ;;;;   ;;;;   ;;;; ;;;;   ;;;;
;                      ;;;;
;                      ;;;;
;


  (test/spec-passed/result 'any/c '(contract any/c 1 'pos 'neg) 1)
  (test/pos-blame 'none/c '(contract none/c 1 'pos 'neg))


;
;
;
;                  ;;;
;        ;       ; ;;; ;
;        ;;;    ;;;;;;;;;
;         ;;;;   ;;;;;;;
;           ;;;  ;;;;;;;
;  ;;;;;    ;;; ;;;;;;;;;
;  ;;;;;  ;;;;   ; ;;; ;
;        ;;;       ;;;
;        ;
;
;
;


  (test/spec-passed
   'contract-arrow-star0a
   '(contract (->* (integer?) () integer?)
              (lambda (x) x)
              'pos
              'neg))

  (test/neg-blame
   'contract-arrow-star0b
   '((contract (->* (integer?) () integer?)
               (lambda (x) x)
               'pos
               'neg)
     #f))

  (test/pos-blame
   'contract-arrow-star0c
   '((contract (->* (integer?) () integer?)
               (lambda (x) #f)
               'pos
               'neg)
     1))

  (test/spec-passed
   'contract-arrow-star1
   '(let-values ([(a b) ((contract (->* (integer?) () (values integer? integer?))
                                   (lambda (x) (values x x))
                                   'pos
                                   'neg)
                         2)])
      1))

  (test/neg-blame
   'contract-arrow-star2
   '((contract (->* (integer?) () (values integer? integer?))
               (lambda (x) (values x x))
               'pos
               'neg)
     #f))

  (test/pos-blame
   'contract-arrow-star3
   '((contract (->* (integer?) () (values integer? integer?))
               (lambda (x) (values 1 #t))
               'pos
               'neg)
     1))

  (test/pos-blame
   'contract-arrow-star4
   '((contract (->* (integer?) () (values integer? integer?))
               (lambda (x) (values #t 1))
               'pos
               'neg)
     1))


  (test/spec-passed
   'contract-arrow-star5
   '(let-values ([(a b) ((contract (->* (integer?) ()
                                        #:rest (listof integer?)
                                        (values integer? integer?))
                                   (lambda (x . y) (values x x))
                                   'pos
                                   'neg)
                         2)])
      1))

  (test/neg-blame
   'contract-arrow-star6
   '((contract (->* (integer?) () #:rest (listof integer?) (values integer? integer?))
               (lambda (x . y) (values x x))
               'pos
               'neg)
     #f))

  (test/pos-blame
   'contract-arrow-star7
   '((contract (->* (integer?) () #:rest (listof integer?) (values integer? integer?))
               (lambda (x . y) (values 1 #t))
               'pos
               'neg)
     1))

  (test/pos-blame
   'contract-arrow-star8
   '((contract (->* (integer?) () #:rest (listof integer?) (values integer? integer?))
               (lambda (x) (values #t 1))
               'pos
               'neg)
     1))

  (test/spec-passed
   'contract-arrow-star9
   '((contract (->* (integer?) () #:rest (listof integer?) integer?)
               (lambda (x . y) 1)
               'pos
               'neg)
     1 2))

  (test/neg-blame
   'contract-arrow-star10
   '((contract (->* (integer?) () #:rest (listof integer?) integer?)
               (lambda (x . y) 1)
               'pos
               'neg)
     1 2 'bad))

  (test/spec-passed
   'contract-arrow-star11
   '(let-values ([(a b) ((contract (->* (integer?) ()
                                        #:rest (listof integer?)
					any)
                                   (lambda (x . y) (values x x))
                                   'pos
                                   'neg)
                         2)])
      1))

  (test/pos-blame
   'contract-arrow-star11b
   '(let-values ([(a b) ((contract (->* (integer?) ()
                                        #:rest (listof integer?)
					any)
                                   (lambda (x) (values x x))
                                   'pos
                                   'neg)
                         2)])
      1))

  (test/neg-blame
   'contract-arrow-star12
   '((contract (->* (integer?) () #:rest (listof integer?) any)
               (lambda (x . y) (values x x))
               'pos
               'neg)
     #f))

  (test/spec-passed
   'contract-arrow-star13
   '((contract (->* (integer?) () #:rest (listof integer?) any)
               (lambda (x . y) 1)
               'pos
               'neg)
     1 2))

  (test/neg-blame
   'contract-arrow-star14
   '((contract (->* (integer?) () #:rest (listof integer?) any)
               (lambda (x . y) 1)
               'pos
               'neg)
     1 2 'bad))

  (test/spec-passed
   'contract-arrow-star15
   '(let-values ([(a b) ((contract (->* (integer?) () any)
                                   (lambda (x) (values x x))
                                   'pos
                                   'neg)
                         2)])
      1))

  (test/spec-passed
   'contract-arrow-star16
   '((contract (->* (integer?) () any)
               (lambda (x) x)
               'pos
               'neg)
     2))

  (test/neg-blame
   'contract-arrow-star17
   '((contract (->* (integer?) () any)
               (lambda (x) (values x x))
               'pos
               'neg)
     #f))

  (test/pos-blame
   'contract-arrow-star-arity-check1
   '(contract (->* (integer?) () #:rest (listof integer?) (values integer? integer?))
              (lambda (x) (values 1 #t))
              'pos
              'neg))

  (test/pos-blame
   'contract-arrow-star-arity-check2
   '(contract (->* (integer?) () #:rest (listof integer?) (values integer? integer?))
              (lambda (x y) (values 1 #t))
              'pos
              'neg))

  (test/pos-blame
   'contract-arrow-star-arity-check3
   '(contract (->* (integer?) () #:rest (listof integer?) (values integer? integer?))
              (case-lambda [(x y) #f] [(x y . z) #t])
              'pos
              'neg))

  (test/spec-passed
   'contract-arrow-star-arity-check4
   '(contract (->* (integer?) () #:rest (listof integer?) (values integer? integer?))
              (case-lambda [(x y) #f] [(x y . z) #t] [(x) #f])
              'pos
              'neg))

  (test/pos-blame
   'contract-arrow-star-keyword1
   '(contract (->* (integer?) () #:rest (listof integer?) (values integer?))
              (λ (x #:y y . args) x)
              'pos
              'neg))

  (test/pos-blame
   'contract-arrow-star-keyword2
   '(contract (->* (integer?) () #:rest (listof integer?) any)
              (λ (x #:y y . args) x)
              'pos
              'neg))

  (test/spec-passed
   'contract-arrow-star-keyword3
   '(contract (->* (integer? #:y integer?) () #:rest (listof integer?) (values integer? integer?))
              (λ (x #:y y . args) x)
              'pos
              'neg))

  (test/spec-passed
   'contract-arrow-star-keyword4
   '(contract (->* (integer? #:y integer?) () #:rest (listof integer?) any)
              (λ (x #:y y . args) x)
              'pos
              'neg))

  (test/neg-blame
   'contract-arrow-star-keyword5
   '((contract (->* (integer? #:y integer?) () #:rest (listof integer?) (values integer? integer?))
               (λ (x #:y y . args) x)
               'pos
               'neg)
     1 #:y #t))

  (test/neg-blame
   'contract-arrow-star-keyword6
   '((contract (->* (integer? #:y integer?) () #:rest (listof integer?) any)
               (λ (x #:y y . args) x)
               'pos
               'neg)
     1 #:y #t))

  (test/neg-blame
   'contract-arrow-star-keyword7
   '((contract (->* (integer? #:y integer?) () #:rest (listof integer?) (values integer? integer?))
               (λ (x #:y y . args) x)
               'pos
               'neg)
     #t #:y 1))

  (test/neg-blame
   'contract-arrow-star-keyword8
   '((contract (->* (integer? #:y integer?) () #:rest (listof integer?) any)
               (λ (x #:y y . args) x)
               'pos
               'neg)
     #t #:y 1))

  (test/spec-passed
   'contract-arrow-star-keyword9
   '((contract (->* (integer? #:y integer?) () #:rest (listof integer?) (values integer? integer?))
               (λ (x #:y y . args) (values x x))
               'pos
               'neg)
     2 #:y 1))

  (test/spec-passed
   'contract-arrow-star-keyword10
   '((contract (->* (integer? #:y integer?) () #:rest (listof integer?) any)
               (λ (x #:y y . args) (values x x))
               'pos
               'neg)
     2 #:y 1))

  (test/spec-passed
   'contract-arrow-star-optional1
   '(contract (->* (#:x boolean?) (#:y string? #:z char? integer?) any)
              (λ (#:x x #:y [s "s"] #:z [c #\c] [i 5])
                (list x s c i))
              'pos 'neg))

  (test/pos-blame
   'contract-arrow-star-optional2
   '(contract (->* (#:x boolean?) (#:y string? #:z char? integer?) any)
              (λ (#:x x #:z [c #\c] [i 5])
                (list x s c i))
              'pos 'neg))

  (test/spec-passed
   'contract-arrow-star-optional3
   '(contract (->* (#:x boolean?) (#:z char? integer?) any)
              (λ (#:x x #:y [s "s"] #:z [c #\c] [i 5])
                (list x s c i))
              'pos 'neg))

  (test/pos-blame
   'contract-arrow-star-optional4
   '(contract (->* (#:x boolean?) (#:y string? #:z char? integer?) any)
              (λ (#:x x #:y [s "s"] #:z [c #\c])
                (list x s c i))
              'pos 'neg))

  (test/spec-passed/result
   'contract-arrow-star-optional5
   '((contract (->* (#:x boolean?) (#:y string? #:z char? integer?) any)
               (λ (#:x x #:y [s "s"] #:z [c #\c] [i 5])
                 (list x s c i))
               'pos 'neg)
     #:x #t #:y "" #:z #\d 6)
   '(#t "" #\d 6))

  (test/spec-passed/result
   'contract-arrow-star-optional6
   '((contract (->* (#:x boolean?) (#:y string? #:z char? integer?) any)
               (λ (#:x x #:y [s "s"] #:z [c #\c] [i 5])
                 (list x s c i))
               'pos 'neg)
     #:x #t)
   '(#t "s" #\c 5))

  (test/neg-blame
   'contract-arrow-star-optional7
   '((contract (->* (#:x boolean?) (#:y string? #:z char? integer?) any)
               (λ (#:x x #:y [s "s"] #:z [c #\c] [i 5])
                 (list x s c i))
               'pos 'neg)
     #:x #t #:y 'x #:z #\d 6))

  (test/neg-blame
   'contract-arrow-star-optional8
   '((contract (->* (#:x boolean?) (#:y string? #:z char? integer?) any)
               (λ (#:x x #:y [s "s"] #:z [c #\c] [i 5])
                 (list x s c i))
               'pos 'neg)
     #:x #t #:y "" #:z 'x 6))

  (test/neg-blame
   'contract-arrow-star-optional9
   '((contract (->* (#:x boolean?) (#:y string? #:z char? integer?) any)
               (λ (#:x x #:y [s "s"] #:z [c #\c] [i 5])
                 (list x s c i))
               'pos 'neg)
     #:x #t #:y "" #:z #\d 'x))

  (test/pos-blame
   'contract-arrow-star-optional10
   '(contract (->* (#:x boolean?) (#:y string?) any)
              (λ (#:x [x #f] #:y s)
                (list x s c i))
              'pos 'neg))

  (test/spec-passed
   'contract-arrow-star-optional11
   '(contract (->* (#:x boolean?) (#:y string? #:z char? integer?) (list/c boolean? string? char? integer?))
              (λ (#:x x #:y [s "s"] #:z [c #\c] [i 5])
                (list x s c i))
              'pos 'neg))

  (test/pos-blame
   'contract-arrow-star-optional12
   '(contract (->* (#:x boolean?) (#:y string? #:z char? integer?) (list/c boolean? string? char? integer?))
              (λ (#:x x #:z [c #\c] [i 5])
                (list x s c i))
              'pos 'neg))

  (test/spec-passed
   'contract-arrow-star-optional13
   '(contract (->* (#:x boolean?) (#:z char? integer?) (list/c boolean? string? char? integer?))
              (λ (#:x x #:y [s "s"] #:z [c #\c] [i 5])
                (list x s c i))
              'pos 'neg))

  (test/pos-blame
   'contract-arrow-star-optional14
   '(contract (->* (#:x boolean?) (#:y string? #:z char? integer?) (list/c boolean? string? char? integer?))
              (λ (#:x x #:y [s "s"] #:z [c #\c])
                (list x s c i))
              'pos 'neg))

  (test/spec-passed/result
   'contract-arrow-star-optional15
   '((contract (->* (#:x boolean?) (#:y string? #:z char? integer?) (list/c boolean? string? char? integer?))
               (λ (#:x x #:y [s "s"] #:z [c #\c] [i 5])
                 (list x s c i))
               'pos 'neg)
     #:x #t #:y "" #:z #\d 6)
   '(#t "" #\d 6))

  (test/spec-passed/result
   'contract-arrow-star-optional16
   '((contract (->* (#:x boolean?) (#:y string? #:z char? integer?) (list/c boolean? string? char? integer?))
               (λ (#:x x #:y [s "s"] #:z [c #\c] [i 5])
                 (list x s c i))
               'pos 'neg)
     #:x #t)
   '(#t "s" #\c 5))

  (test/neg-blame
   'contract-arrow-star-optional17
   '((contract (->* (#:x boolean?) (#:y string? #:z char? integer?) (list/c boolean? string? char? integer?))
               (λ (#:x x #:y [s "s"] #:z [c #\c] [i 5])
                 (list x s c i))
               'pos 'neg)
     #:x #t #:y 'x #:z #\d 6))

  (test/neg-blame
   'contract-arrow-star-optional18
   '((contract (->* (#:x boolean?) (#:y string? #:z char? integer?) (list/c boolean? string? char? integer?))
               (λ (#:x x #:y [s "s"] #:z [c #\c] [i 5])
                 (list x s c i))
               'pos 'neg)
     #:x #t #:y "" #:z 'x 6))

  (test/neg-blame
   'contract-arrow-star-optional19
   '((contract (->* (#:x boolean?) (#:y string? #:z char? integer?) (list/c boolean? string? char? integer?))
               (λ (#:x x #:y [s "s"] #:z [c #\c] [i 5])
                 (list x s c i))
               'pos 'neg)
     #:x #t #:y "" #:z #\d 'x))

  (test/pos-blame
   'contract-arrow-star-optional20
   '(contract (->* (#:x boolean?) (#:y string?) (list/c boolean? string? char? integer?))
              (λ (#:x [x #f] #:y s)
                (list x s c i))
              'pos 'neg))

  (test/spec-passed
   'contract-arrow-star-optional21
   '((contract (->* () () (values))
               (λ () (values))
               'pos 'neg)))

  (test/spec-passed
   'contract-arrow-star-optional22
   '((contract (->* () () (values integer? char?))
               (λ () (values 1 #\c))
               'pos 'neg)))

  (test/pos-blame
   'contract-arrow-star-optional23
   '((contract (->* () () (values integer? char?))
               (λ () (values 1 2))
               'pos 'neg)))

  (test/spec-passed
   'contract-arrow-star-optional24
   '(let ()
      (define (statement? s)
        (and (string? s)
             (> (string-length s) 3)))
      (define statement/c (flat-contract statement?))

      (define new-statement
        (make-keyword-procedure
         (λ (kws kw-args . statement)
           (format "kws=~s  kw-args=~s  statement=~s" kws kw-args statement))))

      (contract (->* (statement/c) (#:s string?) statement/c)
                new-statement
                'pos 'neg)))

  (test/spec-passed
   'contract-arrow-star-keyword-ordering
   '((contract (->* (integer? #:x boolean?) (string? #:y char?) any)
               (λ (x #:x b [s ""] #:y [c #\c])
                 (list x b s c))
               'pos
               'neg)
     1 "zz" #:x #f #:y #\d))


  (test/spec-passed
   '->*-pre/post-1
   '((contract (->* () () integer? #:post #t)
			  (λ () 1)
			  'pos
			  'neg)))

  (test/pos-blame
   '->*-pre/post-2
   '((contract (->* () () integer? #:post #t)
			   (λ () 'not-an-int)
			   'pos
			   'neg)))

  (test/spec-passed
   '->*-pre/post-3
   '((contract (->* () () (values integer? boolean?) #:post #t)
			   (λ () (values 1 #t))
			   'pos
			   'neg)))

  (test/pos-blame
   '->*-pre/post-4
   '((contract (->* () () (values integer? boolean?) #:post #t)
			   (λ () (values 1 'not-a-boolean))
			   'pos
			   'neg)))

  (test/neg-blame
   '->*-pre/post-5
   '((contract (->* () () #:pre #f integer? #:post #t)
			   (λ () 1)
			   'pos
			   'neg)))

  (test/pos-blame
   '->*-pre/post-6
   '((contract (->* () () #:pre #t integer? #:post #f)
			   (λ () 1)
			   'pos
			   'neg)))

  (test/neg-blame
   '->*-pre/post-7
   '((contract (->* () () #:pre #f integer? #:post #f)
			   (λ () 1)
			   'pos
			   'neg)))

   (test/spec-passed
   '->*-opt-optional1
   '((contract (->* () integer?) (lambda () 1) 'pos 'neg)))

   (test/spec-passed
   '->*-opt-optional2
   '((contract (->* () (values boolean? integer?)) (lambda () (values #t 1)) 'pos 'neg)))

   (test/spec-passed
   '->*-opt-optional3
   '((contract (->* () #:rest any/c integer?) (lambda x 1) 'pos 'neg)))

   (test/spec-passed
   '->*-opt-optional4
   '((contract (->* () #:pre #t integer?) (lambda x 1) 'pos 'neg)))

   (test/spec-passed
   '->*-opt-optional5
   '((contract (->* () integer? #:post #t) (lambda x 1) 'pos 'neg)))

;
;
;
;
;        ;
;        ;;;
;         ;;;;
;           ;;;
;  ;;;;;    ;;;
;  ;;;;;  ;;;;
;        ;;;
;        ;
;
;
;


  (test/spec-passed
   'contract-arrow-values1
   '(let-values ([(a b) ((contract (-> integer? (values integer? integer?))
                                   (lambda (x) (values x x))
                                   'pos
                                   'neg)
                         2)])
      1))

  (test/neg-blame
   'contract-arrow-values2
   '((contract (-> integer? (values integer? integer?))
               (lambda (x) (values x x))
               'pos
               'neg)
     #f))

  (test/pos-blame
   'contract-arrow-values3
   '((contract (-> integer? (values integer? integer?))
               (lambda (x) (values 1 #t))
               'pos
               'neg)
     1))

  (test/pos-blame
   'contract-arrow-values4
   '((contract (-> integer? (values integer? integer?))
               (lambda (x) (values #t 1))
               'pos
               'neg)
     1))

  (test/pos-blame
   'contract-arrow-values5
   '((contract (-> integer? (values integer? integer?))
               (lambda (x) x)
               'pos
               'neg)
     1))


  (test/pos-blame
   'contract-arrow-keyword1
   '(contract (-> integer? any)
              (λ (x #:y y) x)
              'pos
              'neg))

  (test/pos-blame
   'contract-arrow-keyword1b
   '(contract (-> integer? #:y integer? any)
              (λ (x) x)
              'pos
              'neg))

  (test/spec-passed
   'contract-arrow-keyword2
   '(contract (-> integer? #:y boolean? any)
              (λ (x #:y y) x)
              'pos
              'neg))

  (test/spec-passed
   'contract-arrow-keyword2b
   '(contract (-> #:x boolean? #:y boolean? any)
              (λ (#:x x #:y y) x)
              'pos
              'neg))

  (test/spec-passed
   'contract-arrow-keyword2c
   '(contract (-> #:y boolean? #:x boolean? any)
              (λ (#:x x #:y y) x)
              'pos
              'neg))

  (test/spec-passed
   'contract-arrow-keyword2d
   '(contract (-> #:y boolean? #:x boolean? any)
              (λ (#:y y #:x x) x)
              'pos
              'neg))

  (test/spec-passed
   'contract-arrow-keyword2e
   '(contract (-> #:x boolean? #:y boolean?  any)
              (λ (#:y y #:x x) x)
              'pos
              'neg))

  (test/neg-blame
   'contract-arrow-keyword3
   '((contract (-> integer? #:y boolean? any)
               (λ (x #:y y) x)
               'pos
               'neg)
     1 #:y 1))

  (test/neg-blame
   'contract-arrow-keyword4
   '((contract (-> integer? #:y boolean? any)
               (λ (x #:y y) x)
               'pos
               'neg)
     #t #:y #t))

  (test/spec-passed
   'contract-arrow-keyword5
   '((contract (-> integer? #:y boolean? any)
               (λ (x #:y y) x)
               'pos
               'neg)
     1 #:y #t))

  (test/pos-blame
   'contract-arrow-keyword6
   '(contract (-> integer? integer?)
              (λ (x #:y y) x)
              'pos
              'neg))

  (test/spec-passed
   'contract-arrow-keyword7
   '(contract (-> integer? #:y boolean? integer?)
              (λ (x #:y y) x)
              'pos
              'neg))

  (test/neg-blame
   'contract-arrow-keyword8
   '((contract (-> integer? #:y boolean? integer?)
               (λ (x #:y y) x)
               'pos
               'neg)
     1 #:y 1))

  (test/neg-blame
   'contract-arrow-keyword9
   '((contract (-> integer? #:y boolean? integer?)
               (λ (x #:y y) x)
               'pos
               'neg)
     #t #:y #t))

  (test/spec-passed
   'contract-arrow-keyword10
   '((contract (-> integer? #:y boolean? integer?)
               (λ (x #:y y) x)
               'pos
               'neg)
     1 #:y #t))

  (test/pos-blame
   'contract-arrow-keyword11
   '(contract (-> integer? (values integer? integer?))
              (λ (x #:y y) x)
              'pos
              'neg))

  (test/spec-passed
   'contract-arrow-keyword12
   '(contract (-> integer? #:y boolean? (values integer? integer?))
              (λ (x #:y y) x)
              'pos
              'neg))

  (test/neg-blame
   'contract-arrow-keyword13
   '((contract (-> integer? #:y boolean? (values integer? integer?))
               (λ (x #:y y) x)
               'pos
               'neg)
     1 #:y 1))

  (test/neg-blame
   'contract-arrow-keyword14
   '((contract (-> integer? #:y boolean? (values integer? integer?))
               (λ (x #:y y) x)
               'pos
               'neg)
     #t #:y #t))

  (test/spec-passed
   'contract-arrow-keyword15
   '((contract (-> integer? #:y boolean? (values integer? integer?))
               (λ (x #:y y) (values x x))
               'pos
               'neg)
     1 #:y #t))

  (test/spec-passed
   'contract-arrow1
   '(contract (integer? . -> . integer?) (lambda (x) x) 'pos 'neg))

  ;; make sure we skip the optimizations
  (test/spec-passed
   'contract-arrow1b
   '(contract (integer? integer? integer? integer? integer? integer? integer? integer? integer? integer? . -> . integer?)
              (lambda (x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) x1) 'pos 'neg))

  (test/pos-blame
   'contract-arrow2
   '(contract (integer? . -> . integer?) (lambda (x y) x) 'pos 'neg))

  (test/neg-blame
   'contract-arrow3
   '((contract (integer? . -> . integer?) (lambda (x) #f) 'pos 'neg) #t))

  (test/pos-blame
   'contract-arrow4
   '((contract (integer? . -> . integer?) (lambda (x) #f) 'pos 'neg) 1))

  (test/neg-blame
   'contract-arrow-arity1
   '((contract (-> number? number? number?)
               (λ (x . r) x)
              'pos 'neg)
    1))

  (test/spec-passed
   'contract-arrow-any1
   '(contract (integer? . -> . any) (lambda (x) x) 'pos 'neg))

  (test/pos-blame
   'contract-arrow-any2
   '(contract (integer? . -> . any) (lambda (x y) x) 'pos 'neg))

  (test/neg-blame
   'contract-arrow-any3
   '((contract (integer? . -> . any) (lambda (x) #f) 'pos 'neg) #t))

  (test/spec-passed
   'contract-arrow-all-anys1
   '((contract (-> any) (lambda () #f) 'pos 'neg)))

  (test/pos-blame
   'contract-arrow-all-anys2
   '((contract (-> any) (lambda (x) #f) 'pos 'neg)))

  (test/spec-passed
   'contract-arrow-all-anys3
   '((contract (-> any) (lambda ([x #f]) #f) 'pos 'neg)))

  (test/spec-passed
   'contract-arrow-all-kwds
   '(contract (-> #:a string? string?)
              (make-keyword-procedure void)
              'pos 'neg))

  (test/spec-passed
   'contract-arrow-all-kwds2
   '((contract (-> #:a string? void?)
               (make-keyword-procedure void)
               'pos 'neg)
     #:a "abcdef"))

  (contract-error-test
   'contract-arrow-kwd-name-in-message
   #'((contract
       (-> #:a any/c #:the-missing-keyword-arg-b any/c any)
       (λ (#:a [a 0] #:the-missing-keyword-arg-b [b 0]) b)
       'pos
       'neg)
      #:a 0)
   (λ (x)
     (and (exn:fail:contract:blame? x)
          (regexp-match #rx"expected keyword argument #:the-missing-keyword-arg-b"
                        (exn-message x)))))

  (test/pos-blame
   'predicate/c1
   '(contract predicate/c 1 'pos 'neg))
  (test/pos-blame
   'predicate/c2
   '(contract predicate/c (λ (x y) 1) 'pos 'neg))
  (test/pos-blame
   'predicate/c3
   '((contract predicate/c (λ (x) 1) 'pos 'neg) 12))
  (test/spec-passed
   'predicate/c4
   '((contract predicate/c (λ (x) #t) 'pos 'neg) 12))

  ;; this test ensures that no contract wrappers
  ;; are created for struct predicates
  (test/spec-passed/result
   'predicate/c5
   '(let ()
      (struct x (a))
      (eq? (contract predicate/c x? 'pos 'neg) x?))
   #t)



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; procedure accepts-and-more
  ;;

  (ctest #t procedure-accepts-and-more? (lambda (x . y) 1) 3)
  (ctest #t procedure-accepts-and-more? (lambda (x . y) 1) 2)
  (ctest #t procedure-accepts-and-more? (lambda (x . y) 1) 1)
  (ctest #f procedure-accepts-and-more? (lambda (x . y) 1) 0)

  (ctest #t procedure-accepts-and-more? (case-lambda [(x . y) 1] [(y) 1]) 3)
  (ctest #t procedure-accepts-and-more? (case-lambda [(x . y) 1] [(y) 1]) 2)
  (ctest #t procedure-accepts-and-more? (case-lambda [(x . y) 1] [(y) 1]) 1)
  (ctest #f procedure-accepts-and-more? (case-lambda [(x . y) 1] [(y) 1]) 0)

  (ctest #t procedure-accepts-and-more? (case-lambda [(x y . z) 1] [(x) 1]) 2)
  (ctest #t procedure-accepts-and-more? (case-lambda [(x y . z) 1] [(x) 1]) 1)
  (ctest #f procedure-accepts-and-more? (case-lambda [(x y . z) 1] [(x) 1]) 0)

;
;
;
;                    ;;;;
;        ;           ;;;;
;        ;;;      ;;;;;;;
;         ;;;;   ;;;;;;;;
;           ;;; ;;;;;;;;;
;  ;;;;;    ;;; ;;;; ;;;;
;  ;;;;;  ;;;;  ;;;;;;;;;
;        ;;;     ;;;;;;;;
;        ;        ;;;;;;;
;
;
;

  (test/spec-passed
   '->d1
   '((contract (->d () () [x number?]) (lambda () 1) 'pos 'neg)))

  (test/spec-passed
   '->d2
   '((contract (->d ([x number?]) () (values [r number?])) (lambda (x) (+ x 1)) 'pos 'neg) 1))

  (test/pos-blame
   '->d3
   '((contract (->d () () [r number?]) 1 'pos 'neg)))

  (test/pos-blame
   '->d4
   '((contract (->d () () [r number?]) (lambda (x) x) 'pos 'neg)))

  (test/neg-blame
   '->d5
   '((contract (->d ([x number?]) () any) (lambda (x) (+ x 1)) 'pos 'neg) #f))

  (test/pos-blame
   '->d6
   '((contract (->d ([x number?]) () [r (<=/c x)]) (lambda (x) (+ x 1)) 'pos 'neg) 1))

  (test/spec-passed
   '->d7
   '((contract (->d ([x number?] [y (<=/c x)]) () [r (<=/c x)]) (lambda (x y) (- x 1)) 'pos 'neg) 1 0))

  (test/neg-blame
   '->d8
   '((contract (->d ([x number?] [y (<=/c x)]) () [r (<=/c x)]) (lambda (x y) (+ x 1)) 'pos 'neg) 1 2))

  (test/spec-passed
   '->d9
   '((contract (->d ([y (<=/c x)] [x number?]) () [r (<=/c x)]) (lambda (y x) (- x 1)) 'pos 'neg) 1 2))

  (test/neg-blame
   '->d10
   '((contract (->d ([y (<=/c x)] [x number?]) () [r (<=/c x)]) (lambda (y x) (+ x 1)) 'pos 'neg) 1 0))

  (test/spec-passed
   '->d11
   '((contract (->d () () #:rest rest any/c [r number?]) (lambda x 1) 'pos 'neg)))

  (test/spec-passed
   '->d12
   '((contract (->d ([x number?]) () #:rest rest any/c [r number?]) (lambda (x . y) (+ x 1)) 'pos 'neg) 1))

  (test/pos-blame
   '->d13
   '((contract (->d () () #:rest rest any/c [r number?]) 1 'pos 'neg)))

  (test/pos-blame
   '->d14
   '((contract (->d () () #:rest rest any/c [r number?]) (lambda (x) x) 'pos 'neg)))

  (test/neg-blame
   '->d15
   '((contract (->d ([x number?]) () #:rest rest any/c any) (lambda (x . y) (+ x 1)) 'pos 'neg) #f))

  (test/pos-blame
   '->d16
   '((contract (->d ([x number?]) () #:rest rest any/c [r (<=/c x)]) (lambda (x . y) (+ x 1)) 'pos 'neg) 1))

  (test/spec-passed
   '->d17
   '((contract (->d ([x number?] [y (<=/c x)]) () #:rest rest any/c [r (<=/c x)]) (lambda (x y . z) (- x 1)) 'pos 'neg) 1 0))

  (test/neg-blame
   '->d18
   '((contract (->d ([x number?] [y (<=/c x)]) () #:rest rest any/c [r (<=/c x)]) (lambda (x y . z) (+ x 1)) 'pos 'neg) 1 2))

  (test/spec-passed
   '->d19
   '((contract (->d ([y (<=/c x)] [x number?]) () #:rest rest any/c [r (<=/c x)]) (lambda (y x . z) (- x 1)) 'pos 'neg) 1 2))

  (test/neg-blame
   '->d20
   '((contract (->d ([y (<=/c x)] [x number?]) () #:rest rest any/c [r (<=/c x)]) (lambda (y x . z) (+ x 1)) 'pos 'neg) 1 0))

  (test/spec-passed
   '->d21
   '((contract (->d () () #:rest rst (listof number?) [r any/c]) (lambda w 1) 'pos 'neg) 1))

  (test/neg-blame
   '->d22
   '((contract (->d () () #:rest rst (listof number?) [r any/c]) (lambda w 1) 'pos 'neg) #f))

  (test/spec-passed
   '->d-any1
   '((contract (->d () () any) (lambda () 1) 'pos 'neg)))

  (test/spec-passed
   '->d-any2
   '((contract (->d ([x number?]) () any) (lambda (x) (+ x 1)) 'pos 'neg) 1))

  (test/pos-blame
   '->d-any3
   '((contract (->d () () any) 1 'pos 'neg)))

  (test/pos-blame
   '->d-any4
   '((contract (->d () () any) (lambda (x) x) 'pos 'neg)))

  (test/neg-blame
   '->d-any5
   '((contract (->d ([x number?]) () any) (lambda (x) (+ x 1)) 'pos 'neg) #f))

  (test/spec-passed
   '->d-any6
   '((contract (->d ([x number?] [y (<=/c x)]) () any) (lambda (x y) (- x 1)) 'pos 'neg) 1 0))

  (test/neg-blame
   '->d-any7
   '((contract (->d ([x number?] [y (<=/c x)]) () any) (lambda (x y) (+ x 1)) 'pos 'neg) 1 2))

  (test/spec-passed
   '->d-any8
   '((contract (->d ([y (<=/c x)] [x number?]) () any) (lambda (y x) (- x 1)) 'pos 'neg) 1 2))

  (test/neg-blame
   '->d-any9
   '((contract (->d ([y (<=/c x)] [x number?]) () any) (lambda (y x) (+ x 1)) 'pos 'neg) 1 0))

  (test/spec-passed
   '->d-any10
   '((contract (->d () () #:rest rest any/c any) (lambda x 1) 'pos 'neg)))

  (test/spec-passed
   '->d-any11
   '((contract (->d ([x number?]) () #:rest rest any/c any) (lambda (x . y) (+ x 1)) 'pos 'neg) 1))

  (test/pos-blame
   '->d-any12
   '((contract (->d () () #:rest rest any/c any) 1 'pos 'neg)))

  (test/pos-blame
   '->d-any13
   '((contract (->d () () #:rest rest any/c any) (lambda (x) x) 'pos 'neg)))

  (test/neg-blame
   '->d-any14
   '((contract (->d ([x number?]) () #:rest rest any/c any) (lambda (x . y) (+ x 1)) 'pos 'neg) #f))

  (test/spec-passed
   '->d-any15
   '((contract (->d ([x number?] [y (<=/c x)]) () #:rest rest any/c any) (lambda (x y . z) (- x 1)) 'pos 'neg) 1 0))

  (test/neg-blame
   '->d-any16
   '((contract (->d ([x number?] [y (<=/c x)]) () #:rest rest any/c any) (lambda (x y . z) (+ x 1)) 'pos 'neg) 1 2))

  (test/spec-passed
   '->d-any17
   '((contract (->d ([y (<=/c x)] [x number?]) () #:rest rest any/c any) (lambda (y x . z) (- x 1)) 'pos 'neg) 1 2))

  (test/neg-blame
   '->d-any18
   '((contract (->d ([y (<=/c x)] [x number?]) () #:rest rest any/c any) (lambda (y x . z) (+ x 1)) 'pos 'neg) 1 0))

  (test/spec-passed
   '->d-any19
   '((contract (->d () () #:rest rst (listof number?) any) (lambda w 1) 'pos 'neg) 1))

  (test/neg-blame
   '->d-any20
   '((contract (->d () () #:rest rst (listof number?) any) (lambda w 1) 'pos 'neg) #f))

  (test/spec-passed
   '->d-values1
   '((contract (->d () () (values [x boolean?] [y number?])) (lambda () (values #t 1)) 'pos 'neg)))

  (test/spec-passed
   '->d-values2
   '((contract (->d ([x number?]) () (values [z boolean?] [y number?])) (lambda (x) (values #t (+ x 1))) 'pos 'neg) 1))

  (test/pos-blame
   '->d-values3
   '((contract (->d () () (values [x boolean?] [y number?])) 1 'pos 'neg)))

  (test/pos-blame
   '->d-values4
   '((contract (->d () () (values [x boolean?] [y number?])) (lambda (x) x) 'pos 'neg)))

  (test/neg-blame
   '->d-values5
   '((contract (->d ([x number?]) () (values [y boolean?] [z (<=/c x)])) (lambda (x) (+ x 1)) 'pos 'neg) #f))

  (test/pos-blame
   '->d-values6
   '((contract (->d ([x number?]) () (values [y boolean?] [z (<=/c x)])) (lambda (x) (values #t (+ x 1))) 'pos 'neg) 1))

  (test/spec-passed
   '->d-values7
   '((contract (->d ([x number?] [y (<=/c x)]) () (values [z boolean?] [w (<=/c x)]))
               (lambda (x y) (values #t (- x 1)))
               'pos
               'neg)
     1
     0))

  (test/neg-blame
   '->d-values8
   '((contract (->d ([x number?] [y (<=/c x)]) () (values [z boolean?] [w (<=/c x)]))
               (lambda (x y) (values #f (+ x 1)))
               'pos
               'neg)
     1
     2))

  (test/spec-passed
   '->d-values9
   '((contract (->d ([y (<=/c x)] [x number?]) () (values [z boolean?] [w (<=/c x)]))
               (lambda (y x) (values #f (- x 1)))
               'pos
               'neg)
     1
     2))

  (test/neg-blame
   '->d-values10
   '((contract (->d ([y (<=/c x)] [x number?]) () (values [z boolean?] [w (<=/c x)]))
               (lambda (y x) (values #f (+ x 1))) 'pos 'neg)
     1 0))

  (test/spec-passed
   '->d-values11
   '((contract (->d () () #:rest rest any/c (values [z boolean?] [w number?])) (lambda x (values #f 1)) 'pos 'neg)))

  (test/spec-passed
   '->d-values12
   '((contract (->d ([x number?]) () #:rest rest any/c (values [z boolean?] [w number?]))
               (lambda (x . y) (values #f (+ x 1)))
               'pos
               'neg)
     1))

  (test/pos-blame
   '->d-values13
   '((contract (->d () () #:rest rest any/c (values [z boolean?] [w number?])) 1 'pos 'neg)))

  (test/pos-blame
   '->d-values14
   '((contract (->d () () #:rest rest any/c (values [z boolean?] [w number?])) (lambda (x) x) 'pos 'neg)))

  (test/neg-blame
   '->d-values15
   '((contract (->d ([x number?]) () #:rest rest any/c  (values [z boolean?] [w (<=/c x)]))
               (lambda (x . y) (+ x 1)) 'pos 'neg)
     #f))

  (test/pos-blame
   '->d-values16
   '((contract (->d ([x number?]) () #:rest rest any/c (values [z boolean?] [w (<=/c x)]))
               (lambda (x . y) (values #f (+ x 1))) 'pos 'neg)
     1))

  (test/spec-passed
   '->d-values17
   '((contract (->d ([x number?] [y (<=/c x)]) () #:rest rest any/c (values [z boolean?] [w (<=/c x)]))
               (lambda (x y . z) (values #f (- x 1))) 'pos 'neg)
     1 0))

  (test/neg-blame
   '->d-values18
   '((contract (->d ([x number?] [y (<=/c x)]) () #:rest rest any/c (values [z boolean?] [w (<=/c x)]))
               (lambda (x y . z) (values #f (+ x 1))) 'pos 'neg)
     1 2))

  (test/spec-passed
   '->d-values19
   '((contract (->d ([y (<=/c x)] [x number?]) () #:rest rest any/c  (values [z boolean?] [w (<=/c x)]))
               (lambda (y x . z) (values #f (- x 1))) 'pos 'neg)
     1 2))

  (test/neg-blame
   '->d-values20
   '((contract (->d ([y (<=/c x)] [x number?]) () #:rest rest any/c  (values [z boolean?] [w (<=/c x)]))
               (lambda (y x . z) (values #f (+ x 1))) 'pos 'neg)
     1 0))

  (test/spec-passed
   '->d-values21
   '((contract (->d () () #:rest rst (listof number?) (values [z boolean?] [w any/c])) (lambda w (values #f 1)) 'pos 'neg) 1))

  (test/neg-blame
   '->d-values22
   '((contract (->d () () #:rest rst (listof number?) (values [z boolean?] [w any/c])) (lambda w (values #f 1)) 'pos 'neg) #f))

  (test/spec-passed
   '->d-values23
   '((contract (->d () () (values [x number?] [y (>=/c x)])) (lambda () (values 1 2)) 'pos 'neg)))

  (test/pos-blame
   '->d-values24
   '((contract (->d () () (values [x number?] [y (>=/c x)])) (lambda () (values 2 1)) 'pos 'neg)))

  (test/spec-passed
   '->d-values25
   '((contract (->d ([x number?]) () (values [z number?] [y (>=/c x)])) (lambda (x) (values 1 2)) 'pos 'neg) 1))

  (test/pos-blame
   '->d-values26
   '((contract (->d ([x number?]) () (values [z number?] [y (>=/c x)])) (lambda (x) (values 2 1)) 'pos 'neg) 4))

  (test/spec-passed/result
   '->d23
   '((contract (->d ((i number?) (j (and/c number? (>=/c i)))) () [r number?])
               (λ (i j) 1)
               'pos
               'neg)
     1
     2)
   1)

  (test/spec-passed/result
   '->d24
   '((contract (->d ((i number?) (j (and/c number? (>=/c i)))) () any)
               (λ (i j) 1)
               'pos
               'neg)
     1
     2)
   1)

  (test/spec-passed/result
   '->d25
   '(call-with-values
    (λ ()
      ((contract (->d ((i number?) (j (and/c number? (>=/c i)))) () (values [x number?] [y number?]))
                 (λ (i j) (values 1 2))
                 'pos
                 'neg)
       1
       2))
    list)
   '(1 2))

  (test/spec-passed/result
   '->d26
   '((contract (->d ((i number?) (j (and/c number? (>=/c i)))) () #:rest rest-args any/c [r number?])
               (λ (i j . z) 1)
               'pos
               'neg)
     1
     2)
   1)

  (test/spec-passed/result
   '->d27
   '((contract (->d ((i number?) (j (and/c number? (>=/c i)))) () #:rest rest-args any/c any)
               (λ (i j . z) 1)
               'pos
               'neg)
     1
     2)
   1)

  (test/spec-passed/result
   '->d28
   '(call-with-values
     (λ ()
       ((contract (->d ((i number?) (j (and/c number? (>=/c i)))) () #:rest rest-args any/c (values [x number?] [y number?]))
                  (λ (i j . z) (values 1 2))
                  'pos
                  'neg)
        1
        2))
     list)
   '(1 2))

  (test/neg-blame
   '->d30
   '((contract (->d ([x number?]) () #:rest rst number? any)
               (λ (x . rst) (values 4 5))
               'pos
               'neg)
     #f))

  (test/pos-blame
   '->d-arity1
   '(contract (->d ([x number?]) () any) (λ () 1) 'pos 'neg))

  (test/pos-blame
   '->d-arity2
   '(contract (->d ([x number?]) () any) (λ (x #:y y) 1) 'pos 'neg))

  (test/spec-passed
   '->d-arity3
   '(contract (->d ([x number?] #:y [y integer?]) () any) (λ (x #:y y) 1) 'pos 'neg))

  (test/pos-blame
   '->d-arity4
   '(contract (->d () ([x integer?]) any) (λ (x) 1) 'pos 'neg))

  (test/pos-blame
   '->d-arity5
   '(contract (->d () ([x integer?]) any) (λ () 1) 'pos 'neg))

  (test/spec-passed
   '->d-arity6
   '(contract (->d () ([x integer?]) any) (λ ([x 1]) 1) 'pos 'neg))

  (test/pos-blame
   '->d-arity7
   '(contract (->d () (#:x [x integer?]) any) (λ ([x 1]) 1) 'pos 'neg))

  (test/pos-blame
   '->d-arity8
   '(contract (->d () (#:x [x integer?]) any) (λ () 1) 'pos 'neg))

  (test/pos-blame
   '->d-arity8
   '(contract (->d () (#:x [x integer?]) any) (λ (#:x x) 1) 'pos 'neg))

  (test/spec-passed
   '->d-arity10
   '(contract (->d () (#:x [x integer?]) any) (λ (#:x [x 1]) 1) 'pos 'neg))

  (test/spec-passed
   '->d-pp0
   '((contract (->d ([x number?]) () #:pre (= x 1) [result number?] #:post (= x 1))
               (λ (x) x)
               'pos
               'neg)
     1))

  (test/pos-blame
   '->d-pp1
   '((contract (->d ([x number?]) () #:pre-cond (= x 1) [result number?] #:post-cond (= x 2))
               (λ (x) x)
               'pos
               'neg)
     1))

  (test/neg-blame
   '->d-pp2
   '((contract (->d ([x number?]) () #:pre-cond (= x 1) [result number?] #:post-cond (= x 2))
               (λ (x) x)
               'pos
               'neg)
     2))

  (test/pos-blame
   '->d-pp3
   '((contract (->d ([x number?]) () #:pre-cond (= x 1) [result number?] #:post-cond (= result 2))
               (λ (x) x)
               'pos
               'neg)
     1))

  (test/spec-passed
   '->d-pp3.5
   '((contract (->d ([x number?]) () #:pre-cond (= x 1) [result number?] #:post-cond (= result 2))
               (λ (x) 2)
               'pos
               'neg)
     1))

  (test/neg-blame
   '->d-pp4
   '((contract (->d ([x number?]) () #:pre-cond (= x 1) any)
               (λ (x) x)
               'pos
               'neg)
     2))

  (test/neg-blame
   '->d-pp5
   '((contract (->d ([x number?]) () #:pre-cond (= x 1) (values [z number?] [y number?]) #:post-cond (= x y z 3))
               (λ (x) (values 4 5))
               'pos
               'neg)
     2))

  (test/pos-blame
   '->d-pp6
   '((contract (->d ([x number?]) () #:pre-cond (= x 1) (values [z number?] [y number?]) #:post-cond (= z y 3))
               (λ (x) (values 4 5))
               'pos
               'neg)
     1))

  (test/pos-blame
   '->d-pp-r1
   '((contract (->d ([x number?]) () #:rest rst any/c #:pre-cond (= x 1) [result number?] #:post-cond (= x 2))
               (λ (x . rst) x)
               'pos
               'neg)
     1))

  (test/neg-blame
   '->d-pp-r2
   '((contract (->d ([x number?]) () #:rest rst any/c #:pre-cond (= x 1)  [result number?] #:post-cond (= x 2))
               (λ (x . rst) x)
               'pos
               'neg)
     2))

  (test/pos-blame
   '->d-pp-r3
   '((contract (->d ([x number?]) () #:rest rst any/c #:pre-cond (= x 1) [result number?] #:post-cond (= result 2))
               (λ (x . rst) x)
               'pos
               'neg)
     1))

  (test/spec-passed
   '->d-pp-r3.5
   '((contract (->d ([x number?]) () #:rest rst any/c #:pre-cond (= x 1) [result number?] #:post-cond (= result 2))
               (λ (x . rst) 2)
               'pos
               'neg)
     1))

  (test/neg-blame
   '->d-pp-r4
   '((contract (->d ([x number?]) () #:rest rst any/c #:pre-cond (= x 1) any)
               (λ (x . rst) x)
               'pos
               'neg)
     2))

  (test/neg-blame
   '->d-pp-r5
   '((contract (->d ([x number?]) () #:rest rst any/c #:pre-cond (= x 1) (values [z number?] [y number?]) #:post-cond (= x y z 3))
               (λ (x . rst) (values 4 5))
               'pos
               'neg)
     2))

  (test/pos-blame
   '->d-pp-r6
   '((contract (->d ([x number?]) () #:rest rst any/c #:pre-cond (= x 1) (values [z number?] [y number?]) #:post-cond (= z x y 3))
               (λ (x . rst) (values 4 5))
               'pos
               'neg)
     1))

  (test/neg-blame
   '->d-protect-shared-state
   '(let ([x 1])
      ((contract (let ([save #f])
                   (-> (->d () () #:pre-cond (set! save x) [range any/c] #:post-cond (= save x))
                       any))
                 (λ (t) (t))
                 'pos
                 'neg)
       (lambda () (set! x 2)))))


  (test/spec-passed
   '->d-optopt1
   '((contract (->d ([x number?]) any)
               (λ (x) x)
               'pos 'neg)
     1))

  (test/spec-passed
   '->d-optopt2
   '((contract (->d ([x number?]) #:rest rst any/c any)
               (λ (x . y) x)
               'pos 'neg)
     1))

  (test/spec-passed
   '->d-optopt3
   '((contract (->d ([x number?]) #:pre-cond #t any)
               (λ (x) x)
               'pos 'neg)
     1))

  (test/spec-passed
   '->d-optopt4
   '((contract (->d ([x number?]) #:rest rst any/c #:pre-cond #t any)
               (λ (x . y) x)
               'pos 'neg)
     1))

  (test/spec-passed
   '->d-optopt5
   '((contract (->d ([x number?]) #:rest rst any/c #:pre-cond #t [res any/c] #:post-cond #t)
               (λ (x . y) x)
               'pos 'neg)
     1))

  (test/spec-passed
   '->d-optopt6
   '((contract (->d ([x number?]) #:rest rst any/c [res any/c] #:post-cond #t)
               (λ (x . y) x)
               'pos 'neg)
     1))

  (test/spec-passed
   '->d-optopt7
   '((contract (->d ([x number?]) #:pre-cond #t [res any/c] #:post-cond #t)
               (λ (x . y) x)
               'pos 'neg)
     1))

  (test/spec-passed
   '->d-optopt8
   '((contract (->d ([x number?]) [res any/c] #:post-cond #t)
               (λ (x . y) x)
               'pos 'neg)
     1))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;  make sure the variables are all bound properly
  ;;

  (test/spec-passed
   '->d-binding1
   '((contract (->d ([x number?]) () #:rest rest any/c [range any/c] #:post-cond (equal? rest '(2 3 4)))
               (λ (x . y) y)
               'pos
               'neg)
     1 2 3 4))

  (test/spec-passed
   '->d-binding2
   '((contract (->d ([x number?]) () #:rest rest any/c [range any/c] #:post-cond (equal? x 1))
               (λ (x . y) y)
               'pos
               'neg)
     1 2 3 4))

  (test/spec-passed
   '->d-binding3
   '(let ([p 'p]
          [q 'q]
          [r 'r])
      ((contract (->d ([x number?] [y number?] #:z [z number?] #:w [w number?])
                      ([a number?] [b number?] #:c [c number?] #:d [d number?])
                      #:rest rest any/c
                      #:pre-cond (equal? (list x y z w a b c d rest p q r)
                                         (list 1 2 3 4 5 6 7 8 '(z) 'p 'q 'r))
                      (values [p number?] [q number?] [r number?]))
                 (λ (x y #:z z #:w w [a 101] [b 102] #:c [c 103] #:d [d 104] . rest)
                   (values 11 12 13))
                 'pos
                 'neg)
       1 2 #:z 3 #:w 4 5 6 #:c 7 #:d 8 'z)))

  (test/spec-passed
   '->d-binding4
   '((contract (->d ([x number?] [y number?] #:z [z number?] #:w [w number?])
                    ([a number?] [b number?] #:c [c number?] #:d [d number?])
                    #:rest rest any/c
                    (values [p number?] [q number?] [r number?])
                    #:post-cond (equal? (list x y z w a b c d rest p q r)
                                        (list 1 2 3 4 5 6 7 8 '(z) 11 12 13)))
               (λ (x y #:z z #:w w [a 101] [b 102] #:c [c 103] #:d [d 104] . rest)
                 (values 11 12 13))
               'pos
               'neg)
     1 2 #:z 3 #:w 4 5 6 #:c 7 #:d 8 'z))

  (test/spec-passed
   '->d-binding5
   '(let ([p 'p]
          [q 'q]
          [r 'r])
      ((contract (->d ([x number?] [y number?] #:z [z number?] #:w [w number?])
                      ([a number?] [b number?] #:c [c number?] #:d [d number?])
                      #:rest rest any/c
                      #:pre-cond (equal? (list x y z w a b c d rest p q r)
                                         (list 1 2 3 4
                                               the-unsupplied-arg the-unsupplied-arg the-unsupplied-arg the-unsupplied-arg
                                               '() 'p 'q 'r))
                      (values [p number?] [q number?] [r number?]))
                 (λ (x y #:z z #:w w [a 101] [b 102] #:c [c 103] #:d [d 104] . rest)
                   (values 11 12 13))
                 'pos
                 'neg)
       1 2 #:z 3 #:w 4)))

  (test/spec-passed
   '->d-binding6
   '((contract (->d ([x number?] [y number?] #:z [z number?] #:w [w number?])
                    ([a number?] [b number?] #:c [c number?] #:d [d number?])
                    #:rest rest any/c
                    (values [p number?] [q number?] [r number?])
                    #:post-cond (equal? (list x y z w a b c d rest p q r)
                                        (list 1 2 3 4
                                              the-unsupplied-arg the-unsupplied-arg the-unsupplied-arg the-unsupplied-arg
                                              '() 11 12 13)))
               (λ (x y #:z z #:w w [a 101] [b 102] #:c [c 103] #:d [d 104] . rest)
                 (values 11 12 13))
               'pos
               'neg)
     1 2 #:z 3 #:w 4))

  ;; test that the rest parameter is right when there aren't enough arguments to even make it to the rest parameter
  (test/spec-passed
   '->d-binding7
   '((contract (->d ()
                    ([a number?])
                    #:rest rest any/c
                    [_ any/c]
                    #:post-cond (equal? (list a rest) (list the-unsupplied-arg '())))
               (λ ([a 1] . rest) 1)
               'pos
               'neg)))

  (test/pos-blame
   '->d-underscore1
   '((contract (->d ([b (box/c integer?)])
                    ()
                    [_ (let ([old (unbox b)])
                         (and/c
                          void?
                          (λ (new)
                            (= old (unbox b)))))])
               (λ (b)
                 (set-box! b (+ (unbox b) 1)))
               'pos
               'neg)
     (box 1)))

  (test/spec-passed/result
   '->d-underscore2
   '(let ([x '()])
      ((contract (->d () () [_ (begin (set! x (cons 'ctc x)) any/c)])
                 (λ () (set! x (cons 'body x)))
                 'pos
                 'neg))
      x)
   '(body ctc))

  (test/spec-passed/result
   '->d-underscore3
   '(let ([x '()])
      ((contract (->d () () [res (begin (set! x (cons 'ctc x)) any/c)])
                 (λ () (set! x (cons 'body x)))
                 'pos
                 'neg))
      x)
   '(ctc body))

  (test/spec-passed/result
   '->d-underscore4
   '((contract (->d ([str any/c]) () #:rest rest (listof any/c) [_ any/c])
               (λ (x . y) (cons x y))
               'pos 'neg)
     1 2 3)
   '(1 2 3))

  (test/spec-passed/result
   '->d-underscore5
   '((contract (->d ([str any/c]) () #:rest rest (listof any/c) [_ any/c])
               (λ (x . y) (cons x y))
               'pos 'neg)
     1 2 3 4 5)
   '(1 2 3 4 5))


;
;
;
;
;               ;;
;       ;       ;;
;        ;
;         ;;    ;;
;  ;;;;     ;;  ;;
;          ;;;  ;;
;        ;;;    ;;
;       ;;      ;;
;       ;       ;;
;
;
;



  (test/spec-passed
   '->i-stx-1
   '(->i ([x (y) number?]
	  [y number?])
	 any))

  (test/spec-passed
   '->i-stx-2
   (->i ()
	(values [x (y) number?]
		[y number?])))

  (test/spec-passed
   '->i-stx-3
   (->i ()
	#:rest [x number?]
	[y (x) number?]))

  (contract-syntax-error-test
   '->i-stx4
   '(->i (#:kwd1 [x number?]
		 #:kwd2 [x number?])
	 (values [y number?]
		 [z number?])))

  (contract-syntax-error-test
   '->i-stx5
   #'(->i (#:kwd1 [w number?]
		  #:kwd1 [x number?])
	  (values [y number?]
		  [z number?])))

  (contract-syntax-error-test
   '->i-stx6
   #'(->i (#:kwd1 [w number?]
		  #:kwd2 [x number?])
	  (values [y number?]
		  [w number?])))

  (contract-syntax-error-test
   '->i-stx7
   #'(->i (#:kwd1 [w number?]
		  #:kwd2 [x number?])
	  (values [y number?]
		  [y number?])))

  (contract-syntax-error-test
   '->i-stx8
   #'(->i (#:kwd1 [w number?]
		  #:kwd2 [x number?])
	  (values [y number?]
		  [w number?])))

  (contract-syntax-error-test
   '->i-stx10
   #'(->i (#:kwd1 [x number?]
		  #:kwd2 [y number?])
	  [x number?]))

  (contract-syntax-error-test
   '->i-stx11
   #'(->i (#:kwd1 [x number?]
		  #:kwd2 [y number?])
	  #:rest [x any/c]
	  any))

  (contract-syntax-error-test
   '->i-stx12
   #'(let ([c integer?])
       (->i ((arg any/c)) () (values (_ (arg) c) (x (arg) c) (_ (arg) c)))))

  (contract-syntax-error-test
   '->i-stx13
   #'(->i ([x (y) number?])
	  any))

  (contract-syntax-error-test
   '->i-stx14
   #'(->i ([x number?]) #:pre (y) #t any))

  (contract-syntax-error-test
   '->i-stx15
   #'(->i ([x number?]) #:pre (x) #t [res any/c] #:post (y) #t))

  (contract-syntax-error-test
   '->i-stx16
   #'(->i ([x (y) number?])
	  [y number?]))

  (contract-syntax-error-test
   '->i-stx17
   #'(->i ()
	  #:rest [x (y) number?]
	  [y number?]))

  (contract-syntax-error-test
   '->i-stx18
   #'(->i ([x number?]) #:pre (res) #t [res any/c] #:post (x) #t))

  (contract-syntax-error-test
   '->i-stx19
   #'(->i ([x (x) number?])
	  any))

  (contract-syntax-error-test
   '->i-stx20
   #'(->i ([x (y) number?]
	   [y (x) number?])
	  any))

  (contract-syntax-error-test
   '->i-stx21
   #'(->i ([in number?])
	  (values [x (y) number?]
		  [y (z) number?]
		  [z (x) number?])))

  (contract-syntax-error-test
   '->i-stx22
   #'(->i ()
	  #:rest [x (x) number?]
	  any))

  (test/spec-passed
   '->i1
   '((contract (->i () () [x number?]) (lambda () 1) 'pos 'neg)))

  (test/spec-passed
   '->i2
   '((contract (->i ([x number?]) () (values [r number?])) (lambda (x) (+ x 1)) 'pos 'neg) 1))

  (test/pos-blame
   '->i3
   '((contract (->i () () [r number?]) 1 'pos 'neg)))

  (test/pos-blame
   '->i4
   '((contract (->i () () [r number?]) (lambda (x) x) 'pos 'neg)))

  (test/neg-blame
   '->i5
   '((contract (->i ([x number?]) () any) (lambda (x) (+ x 1)) 'pos 'neg) #f))

  (test/pos-blame
   '->i6
   '((contract (->i ([x number?]) () [r (x) (<=/c x)]) (lambda (x) (+ x 1)) 'pos 'neg) 1))

  (test/spec-passed
   '->i7
   '((contract (->i ([x number?] [y (x) (<=/c x)]) () [r (x) (<=/c x)]) (lambda (x y) (- x 1)) 'pos 'neg) 1 0))

  (test/neg-blame
   '->i8
   '((contract (->i ([x number?] [y (x) (<=/c x)]) () [r (x) (<=/c x)]) (lambda (x y) (+ x 1)) 'pos 'neg) 1 2))

  (test/spec-passed
   '->i9
   '((contract (->i ([y (x) (<=/c x)] [x number?]) () [r (x) (<=/c x)]) (lambda (y x) (- x 1)) 'pos 'neg) 1 2))

  (test/neg-blame
   '->i10
   '((contract (->i ([y (x) (<=/c x)] [x number?]) () [r (x) (<=/c x)]) (lambda (y x) (+ x 1)) 'pos 'neg) 1 0))

  (test/spec-passed
   '->i11
   '((contract (->i () () #:rest [rest any/c] [r number?]) (lambda x 1) 'pos 'neg)))

  (test/spec-passed
   '->i12
   '((contract (->i ([x number?]) () #:rest [rest any/c] [r number?]) (lambda (x . y) (+ x 1)) 'pos 'neg) 1))

  (test/pos-blame
   '->i13
   '((contract (->i () () #:rest [rest any/c] [r number?]) 1 'pos 'neg)))

  (test/pos-blame
   '->i14
   '((contract (->i () () #:rest [rest any/c] [r number?]) (lambda (x) x) 'pos 'neg)))

  (test/neg-blame
   '->i15
   '((contract (->i ([x number?]) () #:rest [rest any/c] any) (lambda (x . y) (+ x 1)) 'pos 'neg) #f))

  (test/pos-blame
   '->i16
   '((contract (->i ([x number?]) () #:rest [rest any/c] [r (x) (<=/c x)]) (lambda (x . y) (+ x 1)) 'pos 'neg) 1))

  (test/spec-passed
   '->i17
   '((contract (->i ([x number?] [y (x) (<=/c x)]) () #:rest [rest any/c] [r (x) (<=/c x)]) (lambda (x y . z) (- x 1)) 'pos 'neg) 1 0))

  (test/neg-blame
   '->i18
   '((contract (->i ([x number?] [y (x) (<=/c x)]) () #:rest [rest any/c] [r (x) (<=/c x)]) (lambda (x y . z) (+ x 1)) 'pos 'neg) 1 2))

  (test/spec-passed
   '->i19
   '((contract (->i ([y (x) (<=/c x)] [x number?]) () #:rest [rest any/c] [r (x) (<=/c x)]) (lambda (y x . z) (- x 1)) 'pos 'neg) 1 2))

  (test/neg-blame
   '->i20
   '((contract (->i ([y (x) (<=/c x)] [x number?]) () #:rest [rest any/c] [r (x) (<=/c x)]) (lambda (y x . z) (+ x 1)) 'pos 'neg) 1 0))

  (test/spec-passed
   '->i21
   '((contract (->i () () #:rest [rst (listof number?)] [r any/c]) (lambda w 1) 'pos 'neg) 1))

  (test/neg-blame
   '->i22
   '((contract (->i () () #:rest [rst (listof number?)] [r any/c]) (lambda w 1) 'pos 'neg) #f))

  (test/spec-passed/result
   '->i22
   '(send (contract (object-contract
                     [m (->i ([x any/c] #:y [y any/c]) ([z any/c]) any)])
                    (new (class object%
                           (define/public (m x #:y y [z 1]) x)
                           (super-new)))
                    'pos
                    'neg)
          m 1 #:y 2)
   1)

  (test/spec-passed/result
   '->i23
   '((contract (->i ([x any/c] #:y [y any/c]) ([z any/c]) any)
               (let ()
                 (define (m x #:y y [z 1]) x)
                 m)
               'pos
               'neg)
     1 #:y 2)
   1)

  (test/spec-passed/result
   '->i24
   '((contract (->i ([x any/c]) ([y any/c]) any)
               (let ()
                 (define (m x [y 1]) x)
                 m)
               'pos
               'neg)
     1)
   1)

  (test/spec-passed/result
   '->i25
   '(send (contract (object-contract
                     [m (->i ([x any/c]) ([y any/c]) any)])
                    (new (class object%
                           (define/public (m x [y 1]) x)
                           (super-new)))
                    'pos
                    'neg)
          m 1)
   1)

  (test/spec-passed/result
   '->i26
   '(send (contract (object-contract
                     [m (->i ([x any/c]) #:rest [rest any/c] any)])
                    (new (class object%
                           (define/public (m x . y) x)
                           (super-new)))
                    'pos
                    'neg)
          m 1)
   1)

  (test/spec-passed/result
   '->i27
   '(send (contract (object-contract
                     [m (->i ([x any/c]) any)])
                    (new (class object%
                           (define/public (m x) x)
                           (super-new)))
                    'pos
                    'neg)
          m 1)
   1)

   (test/spec-passed/result
    '->i28
    '((contract (->i ([x real?])
		     #:rest [rest (x) (listof (>=/c x))]
		     any)
		(λ (x . rest)
		   (cons x rest))
		'pos
		'neg)
      1
      2
      3)
    '(1 2 3))

   (test/neg-blame
    '->i29
    '((contract (->i ([x real?])
		     #:rest [rest (x) (listof (>=/c x))]
		     any)
		(λ (x . rest)
		   (cons x rest))
		'pos
		'neg)
      1
      -2
      -3))

  (test/spec-passed
   '->i-any1
   '((contract (->i () () any) (lambda () 1) 'pos 'neg)))

  (test/spec-passed
   '->i-any2
   '((contract (->i ([x number?]) () any) (lambda (x) (+ x 1)) 'pos 'neg) 1))

  (test/pos-blame
   '->i-any3
   '((contract (->i () () any) 1 'pos 'neg)))

  (test/pos-blame
   '->i-any4
   '((contract (->i () () any) (lambda (x) x) 'pos 'neg)))

  (test/neg-blame
   '->i-any5
   '((contract (->i ([x number?]) () any) (lambda (x) (+ x 1)) 'pos 'neg) #f))

  (test/spec-passed
   '->i-any6
   '((contract (->i ([x number?] [y (x) (<=/c x)]) () any) (lambda (x y) (- x 1)) 'pos 'neg) 1 0))

  (test/neg-blame
   '->i-any7
   '((contract (->i ([x number?] [y (x) (<=/c x)]) () any) (lambda (x y) (+ x 1)) 'pos 'neg) 1 2))

  (test/spec-passed
   '->i-any8
   '((contract (->i ([y (x) (<=/c x)] [x number?]) () any) (lambda (y x) (- x 1)) 'pos 'neg) 1 2))

  (test/neg-blame
   '->i-any9
   '((contract (->i ([y (x) (<=/c x)] [x number?]) () any) (lambda (y x) (+ x 1)) 'pos 'neg) 1 0))

  (test/spec-passed
   '->i-any10
   '((contract (->i () () #:rest [rest any/c] any) (lambda x 1) 'pos 'neg)))

  (test/spec-passed
   '->i-any11
   '((contract (->i ([x number?]) () #:rest [rest any/c] any) (lambda (x . y) (+ x 1)) 'pos 'neg) 1))

  (test/pos-blame
   '->i-any12
   '((contract (->i () () #:rest [rest any/c] any) 1 'pos 'neg)))

  (test/pos-blame
   '->i-any13
   '((contract (->i () () #:rest [rest any/c] any) (lambda (x) x) 'pos 'neg)))

  (test/neg-blame
   '->i-any14
   '((contract (->i ([x number?]) () #:rest [rest any/c] any) (lambda (x . y) (+ x 1)) 'pos 'neg) #f))

  (test/spec-passed
   '->i-any15
   '((contract (->i ([x number?] [y (x) (<=/c x)]) () #:rest [rest any/c] any) (lambda (x y . z) (- x 1)) 'pos 'neg) 1 0))

  (test/neg-blame
   '->i-any16
   '((contract (->i ([x number?] [y (x) (<=/c x)]) () #:rest [rest any/c] any) (lambda (x y . z) (+ x 1)) 'pos 'neg) 1 2))

  (test/spec-passed
   '->i-any17
   '((contract (->i ([y (x) (<=/c x)] [x number?]) () #:rest [rest any/c] any) (lambda (y x . z) (- x 1)) 'pos 'neg) 1 2))

  (test/neg-blame
   '->i-any18
   '((contract (->i ([y (x) (<=/c x)] [x number?]) () #:rest [rest any/c] any) (lambda (y x . z) (+ x 1)) 'pos 'neg) 1 0))

  (test/spec-passed
   '->i-any19
   '((contract (->i () () #:rest [rst (listof number?)] any) (lambda w 1) 'pos 'neg) 1))

  (test/neg-blame
   '->i-any20
   '((contract (->i () () #:rest [rst (listof number?)] any) (lambda w 1) 'pos 'neg) #f))

  (test/spec-passed
   '->i-values1
   '((contract (->i () () (values [x boolean?] [y number?])) (lambda () (values #t 1)) 'pos 'neg)))

  (test/spec-passed
   '->i-values2
   '((contract (->i ([x number?]) () (values [z boolean?] [y number?])) (lambda (x) (values #t (+ x 1))) 'pos 'neg) 1))

  (test/pos-blame
   '->i-values3
   '((contract (->i () () (values [x boolean?] [y number?])) 1 'pos 'neg)))

  (test/pos-blame
   '->i-values4
   '((contract (->i () () (values [x boolean?] [y number?])) (lambda (x) x) 'pos 'neg)))

  (test/neg-blame
   '->i-values5
   '((contract (->i ([x number?]) () (values [y boolean?] [z (x) (<=/c x)])) (lambda (x) (+ x 1)) 'pos 'neg) #f))

  (test/pos-blame
   '->i-values6
   '((contract (->i ([x number?]) () (values [y boolean?] [z (x) (<=/c x)])) (lambda (x) (values #t (+ x 1))) 'pos 'neg) 1))

  (test/spec-passed
   '->i-values7
   '((contract (->i ([x number?] [y (x) (<=/c x)]) () (values [z boolean?] [w (x) (<=/c x)]))
               (lambda (x y) (values #t (- x 1)))
               'pos
               'neg)
     1
     0))

  (test/neg-blame
   '->i-values8
   '((contract (->i ([x number?] [y (x) (<=/c x)]) () (values [z boolean?] [w (x) (<=/c x)]))
               (lambda (x y) (values #f (+ x 1)))
               'pos
               'neg)
     1
     2))

  (test/spec-passed
   '->i-values9
   '((contract (->i ([y (x) (<=/c x)] [x number?]) () (values [z boolean?] [w (x) (<=/c x)]))
               (lambda (y x) (values #f (- x 1)))
               'pos
               'neg)
     1
     2))

  (test/neg-blame
   '->i-values10
   '((contract (->i ([y (x) (<=/c x)] [x number?]) () (values [z boolean?] [w (x) (<=/c x)]))
               (lambda (y x) (values #f (+ x 1))) 'pos 'neg)
     1 0))

  (test/spec-passed
   '->i-values11
   '((contract (->i () () #:rest [rest any/c] (values [z boolean?] [w number?])) (lambda x (values #f 1)) 'pos 'neg)))

  (test/spec-passed
   '->i-values12
   '((contract (->i ([x number?]) () #:rest [rest any/c] (values [z boolean?] [w number?]))
               (lambda (x . y) (values #f (+ x 1)))
               'pos
               'neg)
     1))

  (test/pos-blame
   '->i-values13
   '((contract (->i () () #:rest [rest any/c] (values [z boolean?] [w number?])) 1 'pos 'neg)))

  (test/pos-blame
   '->i-values14
   '((contract (->i () () #:rest [rest any/c] (values [z boolean?] [w number?])) (lambda (x) x) 'pos 'neg)))

  (test/neg-blame
   '->i-values15
   '((contract (->i ([x number?]) () #:rest [rest any/c]  (values [z boolean?] [w (x) (<=/c x)]))
               (lambda (x . y) (+ x 1)) 'pos 'neg)
     #f))

  (test/pos-blame
   '->i-values16
   '((contract (->i ([x number?]) () #:rest [rest any/c] (values [z boolean?] [w (x) (<=/c x)]))
               (lambda (x . y) (values #f (+ x 1))) 'pos 'neg)
     1))

  (test/spec-passed
   '->i-values17
   '((contract (->i ([x number?] [y (x) (<=/c x)]) () #:rest [rest any/c] (values [z boolean?] [w (x) (<=/c x)]))
               (lambda (x y . z) (values #f (- x 1))) 'pos 'neg)
     1 0))

  (test/neg-blame
   '->i-values18
   '((contract (->i ([x number?] [y (x) (<=/c x)]) () #:rest [rest any/c] (values [z boolean?] [w (x) (<=/c x)]))
               (lambda (x y . z) (values #f (+ x 1))) 'pos 'neg)
     1 2))

  (test/spec-passed
   '->i-values19
   '((contract (->i ([y (x) (<=/c x)] [x number?]) () #:rest [rest any/c]  (values [z boolean?] [w (x) (<=/c x)]))
               (lambda (y x . z) (values #f (- x 1))) 'pos 'neg)
     1 2))

  (test/neg-blame
   '->i-values20
   '((contract (->i ([y (x) (<=/c x)] [x number?]) () #:rest [rest any/c]  (values [z boolean?] [w (x) (<=/c x)]))
               (lambda (y x . z) (values #f (+ x 1))) 'pos 'neg)
     1 0))

  (test/spec-passed
   '->i-values21
   '((contract (->i () () #:rest [rst (listof number?)] (values [z boolean?] [w any/c])) (lambda w (values #f 1)) 'pos 'neg) 1))

  (test/neg-blame
   '->i-values22
   '((contract (->i () () #:rest [rst (listof number?)] (values [z boolean?] [w any/c])) (lambda w (values #f 1)) 'pos 'neg) #f))

  (test/spec-passed
   '->i-values23
   '((contract (->i () () (values [x number?] [y (x) (>=/c x)])) (lambda () (values 1 2)) 'pos 'neg)))

  (test/pos-blame
   '->i-values24
   '((contract (->i () () (values [x number?] [y (x) (>=/c x)])) (lambda () (values 2 1)) 'pos 'neg)))

  (test/spec-passed
   '->i-values25
   '((contract (->i ([x number?]) () (values [z number?] [y (x) (>=/c x)])) (lambda (x) (values 1 2)) 'pos 'neg) 1))

  (test/pos-blame
   '->i-values26
   '((contract (->i ([x number?]) () (values [z number?] [y (x) (>=/c x)])) (lambda (x) (values 2 1)) 'pos 'neg) 4))

  (test/spec-passed/result
   '->i23
   '((contract (->i ((i number?) (j (i) (and/c number? (>=/c i)))) () [r number?])
               (λ (i j) 1)
               'pos
               'neg)
     1
     2)
   1)

  (test/spec-passed/result
   '->i24
   '((contract (->i ([i number?] [j (i) (and/c number? (>=/c i))]) () any)
               (λ (i j) 1)
               'pos
               'neg)
     1
     2)
   1)

  (test/spec-passed/result
   '->i25
   '(call-with-values
    (λ ()
      ((contract (->i ((i number?) (j (i) (and/c number? (>=/c i)))) () (values [x number?] [y number?]))
                 (λ (i j) (values 1 2))
                 'pos
                 'neg)
       1
       2))
    list)
   '(1 2))

  (test/spec-passed/result
   '->i26
   '((contract (->i ((i number?) (j (i) (and/c number? (>=/c i)))) () #:rest [rest-args any/c] [r number?])
               (λ (i j . z) 1)
               'pos
               'neg)
     1
     2)
   1)

  (test/spec-passed/result
   '->i27
   '((contract (->i ((i number?) (j (i) (and/c number? (>=/c i)))) () #:rest [rest-args any/c] any)
               (λ (i j . z) 1)
               'pos
               'neg)
     1
     2)
   1)

  (test/spec-passed/result
   '->i28
   '(call-with-values
     (λ ()
       ((contract (->i ((i number?) (j (i) (and/c number? (>=/c i)))) () #:rest [rest-args any/c] (values [x number?] [y number?]))
                  (λ (i j . z) (values 1 2))
                  'pos
                  'neg)
        1
        2))
     list)
   '(1 2))

  (test/neg-blame
   '->i30
   '((contract (->i ([x number?]) () #:rest [rst number?] any)
               (λ (x . rst) (values 4 5))
               'pos
               'neg)
     #f))

  (test/spec-passed/result
   '->i34
   '((contract (->i ([x number?]
		     [y (x z) (between/c x z)]
		     [z number?])
		    any)
	       (λ (x y z) (+ x y z))
	       'pos 'neg)
     1 2 3)
   6)

  (test/neg-blame
   '->i35
   '((contract (->i ([x number?]) #:pre () (= 1 2) any)
	       (λ (x) 1)
	       'pos 'neg) 2))

  (test/neg-blame
   '->i35-b
   '((contract (->i ([x number?]) #:pre () #t #:pre () (= 1 2) any)
	       (λ (x) 1)
	       'pos 'neg) 2))

  (test/neg-blame
   '->i35-c
   '((contract (->i ([x number?]) #:pre (x) (even? x) #:pre (x) (positive? x) any)
	       (λ (x) 1)
	       'pos 'neg) 3))

  (test/neg-blame
   '->i35-d
   '((contract (->i ([x number?]) #:pre (x) (even? x) #:pre (x) (positive? x) any)
	       (λ (x) 1)
	       'pos 'neg) -2))

  (test/neg-blame
   '->i35-e
   '((contract (->i ([x any/c]) #:pre (x) (pair? x) #:pre (x) (car x) any)
	       (λ (x) 1)
	       'pos 'neg)
     (cons #f 1)))

  (test/neg-blame
   '->i35-f
   '((contract (->i ([x any/c]) #:pre/name (x) "pair" (pair? x) #:pre/name (x) "car" (car x) any)
	       (λ (x) 1)
	       'pos 'neg)
     (cons #f 1)))

  (test/spec-passed/result
   '->i36
   '((contract (->i ([f (-> number? number?)]) [res number?])
	       (λ (f) (f 1))
	       'pos 'neg)
     (λ (n) (+ n 1)))
   2)

  (test/pos-blame
   '->i37
   '((contract (->i ([f (-> number? number?)]) [res number?])
	       (λ (f) #f)
	       'pos 'neg)
     (λ (n) (+ n 1))))

  (test/spec-passed/result
   '->i38
   '((contract (->i ([x integer?]) () #:rest [rst (listof number?)] [r any/c]) (lambda w w) 'pos 'neg)
     1 2)
   '(1 2))

  (test/spec-passed/result
   '->i39
   '((contract (->i (#:x [x integer?]) () #:rest [rst (listof number?)] [r any/c]) (lambda (#:x x . w) (cons x w)) 'pos 'neg) #:x 1 2)
   '(1 2))

  (test/spec-passed/result
   '->i40
   '((contract (->i () ([x integer?]) #:rest [rst (listof number?)] [r any/c]) (lambda w w) 'pos 'neg) 1 2)
   '(1 2))

  (test/spec-passed/result
   '->i41
   '((contract (->i () (#:x [x integer?]) #:rest [rst (listof number?)] [r any/c]) (lambda (#:x [x 1] . w) (cons x w)) 'pos 'neg) #:x 2 3)
   '(2 3))

  (test/spec-passed/result
   '->i42
   '((contract (->i () (#:x [x integer?]) #:rest [rst (listof number?)] [r any/c]) (lambda (#:x [x 1] . w) (cons x w)) 'pos 'neg)  2 3)
   '(1 2 3))

  (test/spec-passed/result
   '->i43
   '(let ([b (box '())])
      ((contract (->i ([i (box/c (listof integer?))])
		      (values [_ (i)
				 (begin
				   (set-box! i (cons 1 (unbox i)))
				   (λ (x)
				      (set-box! i (cons 4 (unbox i)))
				      #t))]
			      [_ (i)
				 (begin
				   (set-box! i (cons 2 (unbox i)))
				   (λ (x)
				      (set-box! i (cons 5 (unbox i)))
				      #t))]))
		 (λ (i)
		    (set-box! i (cons 3 (unbox i)))
		    (values 2 2))
		 (quote pos)
		 (quote neg))
       b)
      (unbox b))
   '(5 4 3 2 1))

  (test/spec-passed/result
   '->i44
   '((contract (->i ([x () any/c])
                    [y any/c]
                    #:post (x) x)
               (lambda (x) x)
               'pos
               'neg)
     #t)
   '#t)

  (test/pos-blame
   '->i45
   '((contract (->i ([x () any/c])
                    [y any/c]
                    #:post (x) x)
               (lambda (x) x)
               'pos
               'neg)
     #f))

  (test/spec-passed/result
   '->i46
   '((contract (->i ([x any/c])
                    [y () any/c]
                    #:post (y) y)
               (lambda (x) x)
               'pos
               'neg)
     #t)
   '#t)

  (test/pos-blame
   '->i47
   '((contract (->i ([x any/c])
                    [y () any/c]
                    #:post (y) y)
               (lambda (x) x)
               'pos
               'neg)
     #f))

  (test/pos-blame
   '->i47-b
   '((contract (->i ([x any/c])
                    [y () any/c]
                    #:post (y) (even? y)
                    #:post (y) (positive? y))
               (lambda (x) x)
               'pos
               'neg)
     -2))

  (test/pos-blame
   '->i47-c
   '((contract (->i ([x any/c])
                    [y () any/c]
                    #:post (y) (even? y)
                    #:post (y) (positive? y))
               (lambda (x) x)
               'pos
               'neg)
     3))

  (test/pos-blame
   '->i47-d
   '((contract (->i ([x any/c])
                    [y () any/c]
                    #:post (y) (pair? y)
                    #:post (y) (car y))
               (lambda (x) x)
               'pos
               'neg)
     (cons #f 1)))

  (test/pos-blame
   '->i47-e
   '((contract (->i ([x any/c])
                    [y () any/c]
                    #:post/name (y) "pair" (pair? y)
                    #:post/name (y) "car" (car y))
               (lambda (x) x)
               'pos
               'neg)
     (cons #f 1)))

  (test/spec-passed/result
   '->i48
   '(let ([x '()])
      ((contract (->i ([arg (begin (set! x (cons 'arg-eval x)) integer?)])
                      [res () (begin
                                (set! x (cons 'res-eval x))
				(λ (res)
				   (set! x (cons 'res-check x))))])
		 (λ (arg)
		    (set! x (cons 'body x)))
		 'pos
		 'neg)
       1)
      x)
   '(res-check res-eval body arg-eval))

  (test/spec-passed/result
   '->i49
   '(let ([x '()])
     ((contract (->i ([arg (begin (set! x (cons 'arg-eval x)) integer?)])
		     [_ () (begin
			     (set! x (cons 'res-eval x))
			     (λ (res)
				(set! x (cons 'res-check x))))])
		(λ (arg)
		   (set! x (cons 'body x)))
		'pos
		'neg)
      1)
     x)
   '(res-check body res-eval arg-eval))

  (test/spec-passed/result
   '->i50
   '(let ([x '()])
     ((contract (->i ([arg (begin (set! x (cons 'arg-eval x)) integer?)])
		     [res (begin
			    (set! x (cons 'res-eval x))
			    (λ (res)
			       (set! x (cons 'res-check x))))])
		(λ (arg)
		   (set! x (cons 'body x)))
		'pos
		'neg)
      1)
     x)
   '(res-check body res-eval arg-eval))

  (test/spec-passed/result
   '->i51
   '(let ([x '()])
     ((contract (->i ([arg (begin (set! x (cons 'arg-eval x)) integer?)])
		     [_ (begin
			  (set! x (cons 'res-eval x))
			  (λ (res)
			     (set! x (cons 'res-check x))))])
		(λ (arg)
		   (set! x (cons 'body x)))
		'pos
		'neg)
      1)
     x)
   '(res-check body res-eval arg-eval))

  (test/spec-passed/result
   '->i52
   '((contract (->i ()
                    ([x integer?])
                    any)
               (λ ([x 'qq]) x)
               'pos
               'neg))
   'qq)

  (test/pos-blame
   '->i-arity1
   '(contract (->i ([x number?]) () any) (λ () 1) 'pos 'neg))

  (test/pos-blame
   '->i-arity2
   '(contract (->i ([x number?]) () any) (λ (x #:y y) 1) 'pos 'neg))

  (test/spec-passed
   '->i-arity3
   '(contract (->i ([x number?] #:y [y integer?]) () any) (λ (x #:y y) 1) 'pos 'neg))

  (test/pos-blame
   '->i-arity4
   '(contract (->i () ([x integer?]) any) (λ (x) 1) 'pos 'neg))

  (test/pos-blame
   '->i-arity5
   '(contract (->i () ([x integer?]) any) (λ () 1) 'pos 'neg))

  (test/spec-passed
   '->i-arity6
   '(contract (->i () ([x integer?]) any) (λ ([x 1]) 1) 'pos 'neg))

  (test/pos-blame
   '->i-arity7
   '(contract (->i () (#:x [x integer?]) any) (λ ([x 1]) 1) 'pos 'neg))

  (test/pos-blame
   '->i-arity8
   '(contract (->i () (#:x [x integer?]) any) (λ () 1) 'pos 'neg))

  (test/pos-blame
   '->i-arity8
   '(contract (->i () (#:x [x integer?]) any) (λ (#:x x) 1) 'pos 'neg))

  (test/spec-passed
   '->i-arity10
   '(contract (->i () (#:x [x integer?]) any) (λ (#:x [x 1]) 1) 'pos 'neg))

  (test/pos-blame
   '->i-pp1
   '((contract (->i ([x number?]) () #:pre (x) (= x 1) [result number?] #:post (x) (= x 2))
               (λ (x) x)
               'pos
               'neg)
     1))

  (test/neg-blame
   '->i-pp2
   '((contract (->i ([x number?]) () #:pre (x) (= x 1) [result number?] #:post (x) (= x 2))
               (λ (x) x)
               'pos
               'neg)
     2))

  (test/pos-blame
   '->i-pp3
   '((contract (->i ([x number?]) () #:pre (x) (= x 1) [result number?] #:post (result) (= result 2))
               (λ (x) x)
               'pos
               'neg)
     1))

  (test/spec-passed
   '->i-pp3.5
   '((contract (->i ([x number?]) () #:pre (x) (= x 1) [result number?] #:post (result) (= result 2))
               (λ (x) 2)
               'pos
               'neg)
     1))

  (test/neg-blame
   '->i-pp4
   '((contract (->i ([x number?]) () #:pre (x) (= x 1) any)
               (λ (x) x)
               'pos
               'neg)
     2))

  (test/neg-blame
   '->i-pp5
   '((contract (->i ([x number?]) () #:pre (x) (= x 1) (values [z number?] [y number?]) #:post (x y z) (= x y z 3))
               (λ (x) (values 4 5))
               'pos
               'neg)
     2))

  (test/pos-blame
   '->i-pp6
   '((contract (->i ([x number?]) () #:pre (x) (= x 1) (values [z number?] [y number?]) #:post (z y) (= z y 3))
               (λ (x) (values 4 5))
               'pos
               'neg)
     1))

  (test/pos-blame
   '->i-pp-r1
   '((contract (->i ([x number?]) () #:rest [rst any/c] #:pre (x) (= x 1) [result number?] #:post (x) (= x 2))
               (λ (x . rst) x)
               'pos
               'neg)
     1))

  (test/neg-blame
   '->i-pp-r2
   '((contract (->i ([x number?]) () #:rest [rst any/c] #:pre (x) (= x 1)  [result number?] #:post (x) (= x 2))
               (λ (x . rst) x)
               'pos
               'neg)
     2))

  (test/pos-blame
   '->i-pp-r3
   '((contract (->i ([x number?]) () #:rest [rst any/c] #:pre (x) (= x 1) [result number?] #:post (result) (= result 2))
               (λ (x . rst) x)
               'pos
               'neg)
     1))

  (test/spec-passed
   '->i-pp-r3.5
   '((contract (->i ([x number?]) () #:rest [rst any/c] #:pre (x) (= x 1) [result number?] #:post (result) (= result 2))
               (λ (x . rst) 2)
               'pos
               'neg)
     1))

  (test/neg-blame
   '->i-pp-r4
   '((contract (->i ([x number?]) () #:rest [rst any/c] #:pre (x) (= x 1) any)
               (λ (x . rst) x)
               'pos
               'neg)
     2))

  (test/neg-blame
   '->i-pp-r5
   '((contract (->i ([x number?]) () #:rest [rst any/c] #:pre (x) (= x 1) (values [z number?] [y number?]) #:post (x y z) (= x y z 3))
               (λ (x . rst) (values 4 5))
               'pos
               'neg)
     2))

  (test/pos-blame
   '->i-pp-r6
   '((contract (->i ([x number?]) () #:rest [rst any/c] #:pre (x) (= x 1) (values [z number?] [y number?]) #:post (x y z) (= z x y 3))
               (λ (x . rst) (values 4 5))
               'pos
               'neg)
     1))


  ;; test to make sure the values are in the error messages
  (contract-error-test
   '->i-contract-error-test1
   #'((contract (->i ([x number?]) #:pre (x) #f any)
                (λ (x) x)
                'pos
                'neg)
      123456789)
   (λ (x)
     (and (exn:fail:contract:blame? x)
          (regexp-match #rx"x: 123456789" (exn-message x)))))
  (contract-error-test
   '->i-contract-error-test2
   #'((contract (->i ([|x y| number?]) #:pre (|x y|) #f any)
                (λ (x) x)
                'pos
                'neg)
      123456789)
   (λ (x)
     (and (exn:fail:contract:blame? x)
          (regexp-match (regexp-quote "|x y|: 123456789") (exn-message x)))))

  ;; test to make sure the collects directories are appropriately prefixed
  (contract-error-test
   '->i-contract-error-test3
    #'(contract symbol? "not a symbol" 'pos 'neg 'not-a-symbol #'here)
    (lambda (x)
      (and (exn:fail:contract:blame? x)
           (let ([msg (exn-message x)])
             (define ans (regexp-match? #px"<collects>" msg))
             (unless ans
               (printf "msg: ~s\n" msg))
             ans))))

  ;; make sure that ->i checks its arguments
  (contract-error-test
   '->i-contract-error-test4
   #'(->i ([x (λ (x y z) #f)]) any)
   exn:fail?)

  (contract-error-test
   '->i-contract-error-test5
   #'(->i () (values [x (λ (x y z) #f)][y 5]))
   exn:fail?)

  (test/neg-blame
   '->i-protect-shared-state
   '(let ([x 1])
      ((contract (let ([save #f])
                   (-> (->i () () #:pre () (set! save x) [range any/c] #:post () (= save x))
                       any))
                 (λ (t) (t))
                 'pos
                 'neg)
       (lambda () (set! x 2)))))


  (test/spec-passed
   '->i-optopt1
   '((contract (->i ([x number?]) any)
               (λ (x) x)
               'pos 'neg)
     1))

  (test/spec-passed
   '->i-optopt2
   '((contract (->i ([x number?]) #:rest [rst any/c] any)
               (λ (x . y) x)
               'pos 'neg)
     1))

  (test/spec-passed
   '->i-optopt3
   '((contract (->i ([x number?]) #:pre () #t any)
               (λ (x) x)
               'pos 'neg)
     1))

  (test/spec-passed
   '->i-optopt4
   '((contract (->i ([x number?]) #:rest [rst any/c] #:pre () #t any)
               (λ (x . y) x)
               'pos 'neg)
     1))

  (test/spec-passed
   '->i-optopt5
   '((contract (->i ([x number?]) #:rest [rst any/c] #:pre () #t [res any/c] #:post () #t)
               (λ (x . y) x)
               'pos 'neg)
     1))

  (test/spec-passed
   '->i-optopt6
   '((contract (->i ([x number?]) #:rest [rst any/c] [res any/c] #:post () #t)
               (λ (x . y) x)
               'pos 'neg)
     1))

  (test/spec-passed
   '->i-optopt7
   '((contract (->i ([x number?]) #:pre () #t [res any/c] #:post () #t)
               (λ (x . y) x)
               'pos 'neg)
     1))

  (test/spec-passed
   '->i-optopt8
   '((contract (->i ([x number?]) [res any/c] #:post () #t)
               (λ (x . y) x)
               'pos 'neg)
     1))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;  make sure the variables are all bound properly
  ;;

  (test/spec-passed
   '->i-binding1
   '((contract (->i ([x number?]) () #:rest [rest any/c] [range any/c] #:post (rest) (equal? rest '(2 3 4)))
               (λ (x . y) y)
               'pos
               'neg)
     1 2 3 4))

  (test/spec-passed
   '->i-binding2
   '((contract (->i ([x number?]) () #:rest [rest any/c] [range any/c] #:post (x) (equal? x 1))
               (λ (x . y) y)
               'pos
               'neg)
     1 2 3 4))

  (test/spec-passed
   '->i-binding3
   '(let ([p 'p]
          [q 'q]
          [r 'r])
      ((contract (->i ([x number?] [y number?] #:z [z number?] #:w [w number?])
                      ([a number?] [b number?] #:c [c number?] #:d [d number?])
                      #:rest [rest any/c]
                      #:pre (x y z w a b c d rest)
                      (equal? (list x y z w a b c d rest p q r)
                              (list 1 2 3 4 5 6 7 8 '(z) 'p 'q 'r))
                      (values [p number?] [q number?] [r number?]))
                 (λ (x y #:z z #:w w [a 101] [b 102] #:c [c 103] #:d [d 104] . rest)
                   (values 11 12 13))
                 'pos
                 'neg)
       1 2 #:z 3 #:w 4 5 6 #:c 7 #:d 8 'z)))

  (test/spec-passed
   '->i-binding4
   '((contract (->i ([x number?] [y number?] #:z [z number?] #:w [w number?])
                    ([a number?] [b number?] #:c [c number?] #:d [d number?])
                    #:rest [rest any/c]
                    (values [p number?] [q number?] [r number?])
                    #:post (x y z w a b c d rest p q r)
                    (equal? (list x y z w a b c d rest p q r)
                            (list 1 2 3 4 5 6 7 8 '(z) 11 12 13)))
               (λ (x y #:z z #:w w [a 101] [b 102] #:c [c 103] #:d [d 104] . rest)
                 (values 11 12 13))
               'pos
               'neg)
     1 2 #:z 3 #:w 4 5 6 #:c 7 #:d 8 'z))

  (test/spec-passed
   '->i-binding5
   '(let ([p 'p]
          [q 'q]
          [r 'r])
      ((contract (->i ([x number?] [y number?] #:z [z number?] #:w [w number?])
                      ([a number?] [b number?] #:c [c number?] #:d [d number?])
                      #:rest [rest any/c]
                      #:pre (x y z w a b c d rest)
                      (equal? (list x y z w a b c d rest p q r)
                              (list 1 2 3 4
                                    the-unsupplied-arg the-unsupplied-arg the-unsupplied-arg the-unsupplied-arg
                                    '() 'p 'q 'r))
                      (values [p number?] [q number?] [r number?]))
                 (λ (x y #:z z #:w w [a 101] [b 102] #:c [c 103] #:d [d 104] . rest)
                   (values 11 12 13))
                 'pos
                 'neg)
       1 2 #:z 3 #:w 4)))

  (test/spec-passed
   '->i-binding6
   '((contract (->i ([x number?] [y number?] #:z [z number?] #:w [w number?])
                    ([a number?] [b number?] #:c [c number?] #:d [d number?])
                    #:rest [rest any/c]
                    (values [p number?] [q number?] [r number?])
                    #:post (x y z w a b c d rest p q r)
                    (equal? (list x y z w a b c d rest p q r)
                            (list 1 2 3 4
                                  the-unsupplied-arg the-unsupplied-arg the-unsupplied-arg the-unsupplied-arg
                                  '() 11 12 13)))
               (λ (x y #:z z #:w w [a 101] [b 102] #:c [c 103] #:d [d 104] . rest)
                 (values 11 12 13))
               'pos
               'neg)
     1 2 #:z 3 #:w 4))

  ;; test that the rest parameter is right when there aren't enough arguments to even make it to the rest parameter
  (test/spec-passed
   '->i-binding7
   '((contract (->i ()
                    ([a number?])
                    #:rest [rest any/c]
                    [_ any/c]
                    #:post (a rest) (equal? (list a rest) (list the-unsupplied-arg '())))
               (λ ([a 1] . rest) 1)
               'pos
               'neg)))

  (test/pos-blame
   '->i-underscore1
   '((contract (->i ([b (box/c integer?)])
                    ()
                    [_ (b)
                       (let ([old (unbox b)])
                         (and/c
                          void?
                          (λ (new)
                            (= old (unbox b)))))])
               (λ (b)
                 (set-box! b (+ (unbox b) 1)))
               'pos
               'neg)
     (box 1)))

  (test/spec-passed/result
   '->i-underscore2
   '(let ([x '()])
      ((contract (->i () () [_ (begin (set! x (cons 'ctc x)) any/c)])
                 (λ () (set! x (cons 'body x)))
                 'pos
                 'neg))
      x)
   '(body ctc))

  (test/spec-passed/result
   '->i-underscore3
   '(let ([x '()])
      ((contract (->i () () [res (begin (set! x (cons 'ctc x)) any/c)])
                 (λ () (set! x (cons 'body x)))
                 'pos
                 'neg))
      x)
   '(body ctc))

  (test/spec-passed/result
   '->i-underscore4
   '((contract (->i ([str any/c]) () #:rest [rest (listof any/c)] [_ any/c])
               (λ (x . y) (cons x y))
               'pos 'neg)
     1 2 3)
   '(1 2 3))

  (test/spec-passed/result
   '->i-underscore5
   '((contract (->i ([str any/c]) () #:rest [rest (listof any/c)] [_ any/c])
               (λ (x . y) (cons x y))
               'pos 'neg)
     1 2 3 4 5)
   '(1 2 3 4 5))

  (test/spec-passed/result
   '->i-underscore6
   '(let ([x '()])
      ((contract (->i ([a integer?]) () [_ (a) (begin (set! x (cons 'ctc x)) any/c)])
                 (λ (a) (set! x (cons 'body x)))
                 'pos
                 'neg)
       11)
      x)
   '(body ctc))

  (test/pos-blame
   '->i-bad-number-of-result-values1
   '((contract (->i ((x any/c)) (result (x) any/c))
               (λ (x) (values 1 2))
               'pos
               'neg)
     1))

  (test/pos-blame
   '->i-bad-number-of-result-values2
   '((contract (->i ((giraffe any/c)) (elephant any/c))
               (λ (x) (values 1 2))
               'pos
               'neg)
     1))
  
;
;
;
;
;                                        ;
;    ;;;;; ;;;;;;;   ;;;;;   ;;;         ;;;
;   ;;;;;; ;;;;;;;; ;;;;;;  ;;;;;         ;;;;
;  ;;;;;;;     ;;;; ;;;;   ;;;; ;;          ;;;
;  ;;;;     ;;;;;;;  ;;;;  ;;;;;;; ;;;;;    ;;;
;  ;;;;;;; ;;  ;;;;   ;;;; ;;;;;   ;;;;;  ;;;;
;   ;;;;;; ;;;;;;;; ;;;;;;  ;;;;;;       ;;;
;    ;;;;;  ;; ;;;; ;;;;;    ;;;;        ;
;
;
;


  (test/spec-passed
   'contract-case->0a
   '(contract (case->)
              (lambda (x) x)
              'pos
              'neg))

  (test/spec-passed
   'contract-case->0b
   '(contract (case->)
              (lambda () 1)
              'pos
              'neg))

  (test/pos-blame
   'contract-case->0c
   '(contract (case->)
              1
              'pos
              'neg))

  (test/spec-passed
   'contract-case->0d
   '(contract (case->)
              (case-lambda)
              'pos
              'neg))


  (test/pos-blame
   'contract-case->1
   '(contract (case-> (integer? integer? . -> . integer?) (integer? . -> . integer?))
              (lambda (x) x)
              'pos
              'neg))

  (test/pos-blame
   'contract-case->2
   '((contract (case-> (integer? integer? . -> . integer?) (integer? . -> . integer?))
               (case-lambda
                 [(x y) 'case1]
                 [(x) 'case2])
               'pos
               'neg)
     1 2))

  (test/pos-blame
   'contract-case->3
   '((contract (case-> (integer? integer? . -> . integer?) (integer? . -> . integer?))
               (case-lambda
                 [(x y) 'case1]
                 [(x) 'case2])
               'pos
               'neg)
     1))

  (test/neg-blame
   'contract-case->4
   '((contract (case-> (integer? integer? . -> . integer?) (integer? . -> . integer?))
               (case-lambda
                 [(x y) 'case1]
                 [(x) 'case2])
               'pos
               'neg)
     'a 2))

  (test/neg-blame
   'contract-case->5
   '((contract (case-> (integer? integer? . -> . integer?) (integer? . -> . integer?))
               (case-lambda
                 [(x y) 'case1]
                 [(x) 'case2])
               'pos
               'neg)
     2 'a))

  (test/neg-blame
   'contract-case->6
   '((contract (case-> (integer? integer? . -> . integer?) (integer? . -> . integer?))
               (case-lambda
                 [(x y) 'case1]
                 [(x) 'case2])
               'pos
               'neg)
     #t))

  (test/pos-blame
   'contract-case->7
   '((contract (case-> (integer? integer? . -> . integer?) (-> integer? #:rest any/c boolean?))
               (lambda x #\a)
               'pos
               'neg)
     1 2))


  (test/pos-blame
   'contract-case->8
   '((contract (case-> (integer? integer? . -> . integer?) (-> integer? #:rest any/c boolean?))
               (lambda x #t)
               'pos
               'neg)
     1 2))

  (test/spec-passed
   'contract-case->8
   '((contract (case-> (integer? integer? . -> . integer?) (-> integer? #:rest any/c boolean?))
               (lambda x 1)
               'pos
               'neg)
     1 2))

  (test/spec-passed/result
   'contract-case->9
   '((contract (case-> (-> integer? any))
               (lambda (x) 1)
               'pos
               'neg)
     1)
   1)

  (test/neg-blame
   'contract-case->10
   '((contract (case-> (-> integer? any))
               (lambda (x) 1)
               'pos
               'neg)
     #f))

  (test/pos-blame
   'contract-case->11
   '(contract (case-> (-> integer? any) (->  integer? integer? any))
              (lambda (x) 1)
              'pos
              'neg))

  (test/neg-blame
   'contract-case->12
   '((contract (case-> (-> integer? any) (-> integer? integer? any))
               (case-lambda [(x) 1] [(x y) 1])
               'pos
               'neg)
     #f))

  (test/spec-passed/result
   'contract-case->13
   '((contract (case-> (-> integer? any) (-> integer? integer? any))
               (case-lambda [(x) 1] [(x y) 1])
               'pos
               'neg)
     1)
   1)

  (test/spec-passed/result
   'contract-case->14
   '(let ([f
           (contract (case-> (-> char?) (-> integer? boolean?) (-> symbol? input-port? string?))
                     (case-lambda
                       [() #\a]
                       [(x) (= x 0)]
                       [(sym port)
                        (string-append
                         (symbol->string sym)
                         (read port))])
                     'pos
                     'neg)])
      (list (f)
            (f 1)
            (f 'x (open-input-string (format "~s" "string")))))
   (list #\a #f "xstring"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                                                        ;;
  ;;   case-> arity checking tests                          ;;
  ;;                                                        ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (test/well-formed '(case-> (-> integer? integer?)))
  (test/well-formed '(case-> (-> integer? integer?) (-> integer? integer? integer?)))
  (test/well-formed '(case-> (-> integer? integer?) (-> integer? integer? any)))
  (test/well-formed '(case-> (-> integer? any) (-> integer? integer? any)))

;
;
;
;                                                            ;                     ;;                        ;;;;
;                                                           ;;                     ;;                        ;;;;
;  ;;;; ;;;; ;;;; ;;;    ;;;;;   ;;;;   ;;;; ;;;   ;;;;;  ;;;;; ;;; ;;; ;;;;;;;       ;;;; ;;;    ;;;     ;;;;;;;
;  ;;;; ;;;; ;;;;;;;;;  ;;;;;;  ;;;;;;  ;;;;;;;;; ;;;;;; ;;;;;; ;;;;;;; ;;;;;;;; ;;;; ;;;;;;;;;  ;;;;;   ;;;;;;;;
;  ;;;; ;;;; ;;;; ;;;; ;;;;;;; ;;;;;;;; ;;;; ;;;; ;;;;    ;;;;  ;;;; ;;     ;;;; ;;;; ;;;; ;;;; ;;;; ;; ;;;;;;;;;
;  ;;;; ;;;; ;;;; ;;;; ;;;;    ;;;; ;;; ;;;; ;;;;  ;;;;   ;;;;  ;;;;     ;;;;;;; ;;;; ;;;; ;;;; ;;;;;;; ;;;; ;;;;
;  ;;;; ;;;; ;;;; ;;;; ;;;;;;; ;;;;;;;; ;;;; ;;;;   ;;;;  ;;;;; ;;;;    ;;  ;;;; ;;;; ;;;; ;;;; ;;;;;   ;;;;;;;;;
;  ;;;;;;;;; ;;;; ;;;;  ;;;;;;  ;;;;;;  ;;;; ;;;; ;;;;;;  ;;;;; ;;;;    ;;;;;;;; ;;;; ;;;; ;;;;  ;;;;;;  ;;;;;;;;
;   ;;; ;;;; ;;;; ;;;;   ;;;;;   ;;;;   ;;;; ;;;; ;;;;;    ;;;; ;;;;     ;; ;;;; ;;;; ;;;; ;;;;   ;;;;    ;;;;;;;
;
;
;
;
;
;
;       ;;;;                                   ;;
;       ;;;;                                   ;;                 ;
;    ;;;;;;;   ;;;;   ;;;;;;; ;;;;  ;;;;;;;       ;;;; ;;;        ;;;
;   ;;;;;;;;  ;;;;;;  ;;;;;;;;;;;;; ;;;;;;;; ;;;; ;;;;;;;;;        ;;;;
;  ;;;;;;;;; ;;;;;;;; ;;;; ;;; ;;;;     ;;;; ;;;; ;;;; ;;;;          ;;;
;  ;;;; ;;;; ;;;; ;;; ;;;; ;;; ;;;;  ;;;;;;; ;;;; ;;;; ;;;; ;;;;;    ;;;
;  ;;;;;;;;; ;;;;;;;; ;;;; ;;; ;;;; ;;  ;;;; ;;;; ;;;; ;;;; ;;;;;  ;;;;
;   ;;;;;;;;  ;;;;;;  ;;;; ;;; ;;;; ;;;;;;;; ;;;; ;;;; ;;;;       ;;;
;    ;;;;;;;   ;;;;   ;;;; ;;; ;;;;  ;; ;;;; ;;;; ;;;; ;;;;       ;
;
;
;


  (test/spec-passed
   'unconstrained-domain->1
   '(contract (unconstrained-domain-> number?) (λ (x) x) 'pos 'neg))
  (test/pos-blame
   'unconstrained-domain->2
   '(contract (unconstrained-domain-> number?) 1 'pos 'neg))
  (test/spec-passed
   'unconstrained-domain->3
   '((contract (unconstrained-domain-> number?) (λ (x) x) 'pos 'neg) 1))
  (test/pos-blame
   'unconstrained-domain->4
   '((contract (unconstrained-domain-> number?) (λ (x) x) 'pos 'neg) #f))

  (test/spec-passed/result
   'unconstrained-domain->5
   '((contract (->d ([size natural-number/c]
                     [proc (and/c (unconstrained-domain-> number?)
                                  (λ (p) (procedure-arity-includes? p size)))])
                    ()
                    [range number?])
               (λ (i f) (apply f (build-list i add1)))
               'pos
               'neg)
     10 +)
   55)

  (test/spec-passed/result
   'unconstrained-domain->6
   ((contract (unconstrained-domain-> any/c)
              (λ (#:key k) k)
              'pos
              'neg)
    #:key 1)
   1)

  (test/pos-blame
   'unconstrained-domain->7
   '((contract (unconstrained-domain-> number?) (λ (#:x x) x) 'pos 'neg) #:x #f))

;
;
;
;                    ;;
;                    ;;
;    ;;;;   ;;; ;;;  ;;  ;;;;;
;   ;;;;;;  ;;;;;;;  ;; ;;;;;;
;  ;;;;;;;; ;;;; ;;  ;;;;;;;;;
;  ;;;; ;;; ;;;;    ;; ;;;;
;  ;;;;;;;; ;;;;    ;; ;;;;;;;
;   ;;;;;;  ;;;;    ;;  ;;;;;;
;    ;;;;   ;;;;    ;;   ;;;;;
;                   ;;
;
;


  (test/pos-blame
   'or/c1
   '(contract (or/c false/c) #t 'pos 'neg))

  (test/spec-passed
   'or/c2
   '(contract (or/c false/c) #f 'pos 'neg))

  (test/spec-passed
   'or/c3
   '((contract (or/c (-> integer? integer?)) (lambda (x) x) 'pos 'neg) 1))

  (test/neg-blame
   'or/c4
   '((contract (or/c (-> integer? integer?)) (lambda (x) x) 'pos 'neg) #f))

  (test/pos-blame
   'or/c5
   '((contract (or/c (-> integer? integer?)) (lambda (x) #f) 'pos 'neg) 1))

  (test/spec-passed
   'or/c6
   '(contract (or/c false/c (-> integer? integer?)) #f 'pos 'neg))

  (test/spec-passed
   'or/c7
   '((contract (or/c false/c (-> integer? integer?)) (lambda (x) x) 'pos 'neg) 1))

  (test/spec-passed/result
   'or/c8
   '((contract ((or/c false/c (-> string?))  . -> . any)
               (λ (y) y)
               'pos
               'neg)
     #f)
   #f)

  (test/spec-passed/result
   'or/c9
   '((contract (or/c (-> string?) (-> integer? integer?))
               (λ () "x")
               'pos
               'neg))
   "x")

  (test/spec-passed/result
   'or/c10
   '((contract (or/c (-> string?) (-> integer? integer?))
               (λ (x) x)
               'pos
               'neg)
     1)
   1)

  (test/pos-blame
   'or/c11
   '(contract (or/c (-> string?) (-> integer? integer?))
              1
              'pos
              'neg))

  (test/pos-blame
   'or/c12
   '((contract (or/c (-> string?) (-> integer? integer?))
               1
               'pos
               'neg)
     'x))

  (test/pos-blame
   'or/c13
   '(contract (or/c not) #t 'pos 'neg))

  (test/spec-passed
   'or/c14
   '(contract (or/c not) #f 'pos 'neg))

  (test/spec-passed/result
   'or/c-not-error-early
   '(begin (or/c (-> integer? integer?) (-> boolean? boolean?))
           1)
   1)

  (contract-error-test
   'contract-error-test4
   #'(contract (or/c (-> integer? integer?) (-> boolean? boolean?))
               (λ (x) x)
               'pos
               'neg)
   exn:fail?)

  (test/spec-passed/result
   'or/c-ordering
   '(let ([x '()])
      (contract (or/c (lambda (y) (set! x (cons 2 x)) #f) (lambda (y) (set! x (cons 1 x)) #t))
                'anything
                'pos
                'neg)
      x)
   '(1 2))

  (test/spec-passed/result
   'or/c-ordering2
   '(let ([x '()])
      (contract (or/c (lambda (y) (set! x (cons 2 x)) #t) (lambda (y) (set! x (cons 1 x)) #t))
                'anything
                'pos
                'neg)
      x)
   '(2))

  (test/spec-passed
   'or/c-hmm
   (let ([funny/c (or/c (and/c procedure? (-> any)) (listof (-> number?)))])
     (contract (-> funny/c any) void 'pos 'neg)))


  (test/spec-passed
   'or/c-opt-unknown-flat
   (let ()
     (define arr (-> number? number?))
     ((contract (opt/c (or/c not arr)) (λ (x) x) 'pos 'neg) 1)))



;
;
;
;                          ;;;;  ;;
;                          ;;;;  ;;
;  ;;;;;;;  ;;;; ;;;    ;;;;;;;  ;;  ;;;;;
;  ;;;;;;;; ;;;;;;;;;  ;;;;;;;;  ;; ;;;;;;
;      ;;;; ;;;; ;;;; ;;;;;;;;;  ;;;;;;;;;
;   ;;;;;;; ;;;; ;;;; ;;;; ;;;; ;; ;;;;
;  ;;  ;;;; ;;;; ;;;; ;;;;;;;;; ;; ;;;;;;;
;  ;;;;;;;; ;;;; ;;;;  ;;;;;;;; ;;  ;;;;;;
;   ;; ;;;; ;;;; ;;;;   ;;;;;;; ;;   ;;;;;
;                               ;;
;
;

  (test/spec-passed
   'and/c1
   '((contract (and/c (-> (<=/c 100) (<=/c 100))
                      (-> (>=/c -100) (>=/c -100)))
               (λ (x) x)
               'pos
               'neg)
     1))

  (test/neg-blame
   'and/c2
   '((contract (and/c (-> (<=/c 100) (<=/c 100))
                      (-> (>=/c -100) (>=/c -100)))
               (λ (x) x)
               'pos
               'neg)
     200))

  (test/pos-blame
   'and/c3
   '((contract (and/c (-> (<=/c 100) (<=/c 100))
                      (-> (>=/c -100) (>=/c -100)))
               (λ (x) 200)
               'pos
               'neg)
     1))




  (test/spec-passed/result
   'and/c-ordering
   '(let ([x '()])
      (contract (and/c (lambda (y) (set! x (cons 2 x)) #t) (lambda (y) (set! x (cons 1 x)) #t))
                'anything
                'pos
                'neg)
      x)
   '(1 2))

  (test/spec-passed/result
   'ho-and/c-ordering
   '(let ([x '()])
      ((contract (and/c (-> (lambda (y) (set! x (cons 1 x)) #t)
                            (lambda (y) (set! x (cons 2 x)) #t))
                        (-> (lambda (y) (set! x (cons 3 x)) #t)
                            (lambda (y) (set! x (cons 4 x)) #t)))
                 (λ (x) x)
                 'pos
                 'neg)
       1)
      (reverse x))
   '(3 1 2 4))

  (test/spec-passed/result
   'and/c-isnt
   '(and (regexp-match #rx"isn't: even?"
                       (with-handlers ((exn:fail? exn-message))
                         (contract (and/c integer? even? positive?)
                                   -3
                                   'pos
                                   'neg)
                         "not the error!"))
         #t)
   #t)

  (test/spec-passed
   'contract-flat1
   '(contract not #f 'pos 'neg))

  (test/pos-blame
   'contract-flat2
   '(contract not #t 'pos 'neg))



;
;
;
;                                                                ;                   ;;
;                                                               ;;                   ;;
;  ;;;;;;;   ;;;;;;;  ;;; ;;; ;;;;;;;  ;;;;;;; ;;;;    ;;;    ;;;;;   ;;;   ;;; ;;;  ;;  ;;;;;
;  ;;;;;;;;  ;;;;;;;; ;;;;;;; ;;;;;;;; ;;;;;;;;;;;;;  ;;;;;  ;;;;;;  ;;;;;  ;;;;;;;  ;; ;;;;;;
;  ;;;;;;;;;     ;;;; ;;;; ;;     ;;;; ;;;; ;;; ;;;; ;;;; ;;  ;;;;  ;;;; ;; ;;;; ;;  ;;;;;;;;;
;  ;;;; ;;;;  ;;;;;;; ;;;;     ;;;;;;; ;;;; ;;; ;;;; ;;;;;;;  ;;;;  ;;;;;;; ;;;;    ;; ;;;;
;  ;;;;;;;;; ;;  ;;;; ;;;;    ;;  ;;;; ;;;; ;;; ;;;; ;;;;;    ;;;;; ;;;;;   ;;;;    ;; ;;;;;;;
;  ;;;;;;;;  ;;;;;;;; ;;;;    ;;;;;;;; ;;;; ;;; ;;;;  ;;;;;;  ;;;;;  ;;;;;; ;;;;    ;;  ;;;;;;
;  ;;;;;;;    ;; ;;;; ;;;;     ;; ;;;; ;;;; ;;; ;;;;   ;;;;    ;;;;   ;;;;  ;;;;    ;;   ;;;;;
;  ;;;;                                                                             ;;
;  ;;;;
;


  (test/neg-blame
   'parameter/c1
   '((contract (parameter/c integer?)
               (make-parameter 1)
               'pos 'neg)
     #f))

  (test/pos-blame
   'parameter/c2
   '((contract (parameter/c integer?)
               (make-parameter 'not-an-int)
               'pos 'neg)))

  (test/pos-blame
   'parameter/c3
   '((contract (parameter/c integer? string?)
               (make-parameter 'not-an-int number->string)
               'pos 'neg)))

  (test/neg-blame
   'parameter/c4
   '((contract (parameter/c integer? string?)
               (make-parameter 5 number->string)
               'pos 'neg)
     'not-an-int))

  (test/spec-passed
   'parameter/c5
   '((contract (parameter/c integer? string?)
               (make-parameter "foo" number->string)
               'pos 'neg)))

  (test/spec-passed
   'parameter/c6
   '((contract (parameter/c integer? string?)
               (make-parameter "foo" number->string)
               'pos 'neg)
     5))

  (test/pos-blame
   'parameter/c7
   '((contract (parameter/c integer? string?)
               (make-parameter 5 values)
               'pos 'neg)))

;
;
;
;
;
;
;   ;;                     ;;       ;
;   ;;                     ;;       ;
;   ;;;;;;   ;;;;   ;;;;;  ;;;;;;  ;;  ;;;;
;   ;;;;;;  ;;  ;;  ;; ;;  ;;;;;;  ;  ;;;;;;
;   ;;  ;;    ;;;;  ;;;;;  ;;  ;;  ; ;;;
;   ;;  ;;  ;;; ;;    ;;;; ;;  ;;  ; ;;;
;   ;;  ;; ;;;  ;;  ;; ;;; ;;  ;; ;;  ;;;;;;
;   ;;  ;;  ;;;;;;  ;;;;;  ;;  ;; ;    ;;;;
;
;
;

  (test/spec-passed
   'hash/c1
   '(contract (hash/c symbol? boolean?)
              (make-hash)
              'pos
              'neg))

  (test/spec-passed
   'hash/c1b
   '(contract (hash/c symbol? boolean? #:flat? #t)
              (make-hash)
              'pos
              'neg))

  (test/spec-passed
   'hash/c1c
   '(let ([h (contract (hash/c symbol? boolean?)
                       (make-hash)
                       'pos
                       'neg)])
      (hash-set! h 'x #t)
      (hash-ref h 'x)))

  (test/neg-blame
   'hash/c1d
   '(let ([h (contract (hash/c symbol? boolean?)
                       (make-hash)
                       'pos
                       'neg)])
      (hash-set! h 3 #t)))

  (test/neg-blame
   'hash/c1e
   '(let ([h (contract (hash/c symbol? boolean?)
                       (make-hash)
                       'pos
                       'neg)])
      (hash-set! h 'x 3)))

  (test/neg-blame
   'hash/c1f
   '(let ([h (contract (hash/c symbol? boolean?)
                       (make-hash)
                       'pos
                       'neg)])
      (hash-ref h 3)))

  (test/spec-passed
   'hash/c2
   '(contract (hash/c symbol? boolean?)
              (let ([h (make-hash)])
                (hash-set! h 'x #t)
                h)
              'pos
              'neg))

  (test/pos-blame
   'hash/c3
   '(contract (hash/c symbol? boolean?)
              (let ([h (make-hash)])
                (hash-set! h 'x 'x)
                h)
              'pos
              'neg))

  (test/pos-blame
   'hash/c4
   '(contract (hash/c symbol? boolean?)
              (let ([h (make-hash)])
                (hash-set! h #t #f)
                h)
              'pos
              'neg))

  (test/pos-blame
   'hash/c5
   '(contract (hash/c symbol? boolean? #:immutable #t)
              (let ([h (make-hash)])
                (hash-set! h 'x #f)
                h)
              'pos
              'neg))

  (test/spec-passed
   'hash/c6
   '(contract (hash/c symbol? boolean? #:immutable #t)
              (make-immutable-hash '((x . #f)))
              'pos
              'neg))

  (test/spec-passed
   'hash/c7
   '(contract (hash/c symbol? boolean? #:immutable #f)
              (let ([h (make-hash)])
                (hash-set! h 'x #f)
                h)
              'pos
              'neg))

  (test/pos-blame
   'hash/c8
   '(contract (hash/c symbol? boolean? #:immutable #f)
              (make-immutable-hash '((x . #f)))
              'pos
              'neg))

  (test/spec-passed
   'hash/c9
   '(contract (hash/c symbol? boolean? #:immutable 'dont-care)
              (make-immutable-hash '((x . #f)))
              'pos
              'neg))

  (test/spec-passed
   'hash/c10
   '(contract (hash/c symbol? boolean? #:immutable 'dont-care)
              (let ([h (make-hash)])
                (hash-set! h 'x #f)
                h)
              'pos
              'neg))

  (test/spec-passed/result
   'hash/c11
   '(hash-ref (contract (hash/c symbol? number? #:immutable #t)
                        (make-immutable-hash '((x . 1)))
                        'pos
                        'neg)
              'x)
   1)

  (test/spec-passed/result
   'hash/c12
   '(hash-ref (contract (hash/c symbol? number?)
                        (let ([ht (make-hash)])
                          (hash-set! ht 'x 1)
                          ht)
                        'pos
                        'neg)
              'x)
   1)

  (test/pos-blame
   'hash/c13a
   '(contract (hash/c (hash/c number? number?) number?)
              (make-hasheq)
              'pos
              'neg))

  (test/pos-blame
   'hash/c13b
   '(contract (hash/c (hash/c number? number?) number?)
              (make-hasheq)
              'pos
              'neg))

  (test/neg-blame
   'hash/c13c
   '(let ([h (contract (hash/c (hash/c number? number?) number?)
                       (make-hash)
                       'pos
                       'neg)])
      (hash-set! h (make-hash '((2 . 3))) 2)
      (hash-set! h (make-hash '((3 . #t))) 3)
      (for ([(k v) (in-hash h)])
        (hash-ref k v))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;  prompt-tag/c
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (test/spec-passed
   'prompt-tag/c-fo-1
   '(contract (prompt-tag/c string?)
              (make-continuation-prompt-tag)
              'pos 'neg))

  (test/pos-blame
   'prompt-tag/c-fo-2
   '(contract (prompt-tag/c string?) 5 'pos 'neg))

  (test/spec-passed
   'prompt-tag/c-ho-1
   '(let ([pt (contract (prompt-tag/c number?)
                        (make-continuation-prompt-tag)
                        'pos
                        'neg)])
      (call-with-continuation-prompt
        (λ () (abort-current-continuation pt 3))
        pt
        (λ (x) (+ x 1)))))

  (test/neg-blame
   'prompt-tag/c-ho-2
   '(let ([pt (contract (prompt-tag/c string?)
                        (make-continuation-prompt-tag)
                        'pos
                        'neg)])
      (call-with-continuation-prompt
        (λ () (abort-current-continuation pt 3))
        pt
        (λ (x) (+ x 1)))))

  (test/neg-blame
   'prompt-tag/c-ho-3
   '(let ([pt (contract (prompt-tag/c (-> string? number?))
                        (make-continuation-prompt-tag)
                        'pos
                        'neg)])
      (call-with-continuation-prompt
        (λ () (abort-current-continuation pt (λ (x) 5)))
        pt
        (λ (x) (x 8)))))

  (test/neg-blame
   'prompt-tag/c-ho-4
   '(let ([pt (contract (prompt-tag/c (-> string? number?))
                        (make-continuation-prompt-tag)
                        'pos
                        'neg)])
      (call-with-continuation-prompt
       (λ () (abort-current-continuation pt (λ (x) "bad")))
       pt
       (λ (x) (x "potato")))))

  (test/pos-blame
   'prompt-tag/c-ho-5
   '(let* ([pt (make-continuation-prompt-tag)]
           [do-prompt (contract
                        (-> (-> (prompt-tag/c (-> number? number?))
                                any)
                                number?)
                        (λ (f) (call-with-continuation-prompt
                                (λ () (f pt))
                                pt
                                (λ (f) (f "bad"))))
                        'pos
                        'neg)])
      (do-prompt (λ (pt)
                  (abort-current-continuation pt (λ (v) (+ v 1)))))))

  (test/spec-failed
   'prompt-tag/c-ho-5
   '(let* ([pt (make-continuation-prompt-tag)]
           [do-prompt (contract
                        (-> (-> (prompt-tag/c (-> number? number?))
                                any)
                                number?)
                        (λ (f) (call-with-continuation-prompt
                                (λ () (f pt))
                                pt
                                (λ (f) (f 0))))
                        'A
                        'B)]
           [do-prompt2 (contract
                         (-> (-> (prompt-tag/c (-> string? number?))
                                 any)
                                 number?)
                         do-prompt
                         'B
                         'C)])
      (do-prompt2
        (λ (pt) (abort-current-continuation pt (λ (v) (+ v 1))))))
   "B")

  (test/neg-blame
   'prompt-tag/c-ho-6
   '(let ([pt (contract (prompt-tag/c string? number?)
                        (make-continuation-prompt-tag)
                        'pos
                        'neg)])
      (call-with-continuation-prompt
        (λ () (abort-current-continuation pt 3 "bad"))
        pt
        (λ (x y) (values x y)))))

  (test/spec-passed
   'prompt-tag/c-ho-7
   '(let ([pt (contract (prompt-tag/c string? number?)
                        (make-continuation-prompt-tag)
                        'pos
                        'neg)])
      (call-with-continuation-prompt
        (λ () (abort-current-continuation pt "good" 5))
        pt
        (λ (x y) (values x y)))))

  (test/spec-passed
   'prompt-tag/c-call/cc-1
   '(let* ([pt (contract (prompt-tag/c string?
                                       #:call/cc string?)
                         (make-continuation-prompt-tag)
                         'pos
                         'neg)]
           [abort-k (call-with-continuation-prompt
                     (λ () (call/cc (λ (k) k) pt))
                     pt)])
      (call-with-continuation-prompt
       (λ () (abort-k "ok"))
       pt
       (λ (s) (string-append s "post")))))

  (test/spec-passed
   'prompt-tag/c-call/cc-2
   '(let* ([pt (contract (prompt-tag/c string?
                                       #:call/cc (values string? integer?))
                         (make-continuation-prompt-tag)
                         'pos
                         'neg)]
           [abort-k (call-with-continuation-prompt
                     (λ () (call/cc (λ (k) k) pt))
                     pt)])
      (call-with-continuation-prompt
       (λ () (abort-k "ok" 5))
       pt
       (λ (s n) (string-append s "post")))))

  (test/neg-blame
   'prompt-tag/c-call/cc-2
   '(letrec ([pt (make-continuation-prompt-tag)]
             [do-test (λ ()
                         (+ 1
                            (call-with-continuation-prompt
                             (lambda ()
                               (+ 1 (abort-k 1)))
                             pt)))]
             [cpt (contract (prompt-tag/c #:call/cc number?)
                            pt
                            'pos
                            'neg)]
             [abort-k (call-with-continuation-prompt
                       (λ ()
                          (let ([v (call/cc (lambda (k) k) cpt)])
                            (if (procedure? v)
                                v
                                (format "~a" v))))
                       pt)])
      (do-test)))

  (test/spec-passed/result
   'prompt-tag/c-has-contract
   '(let ([pt (contract (prompt-tag/c string? number?)
                        (make-continuation-prompt-tag)
                        'pos
                        'neg)])
      (has-contract? pt))
   #t)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;  continuation-mark-key/c
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (test/spec-passed
   'continuation-mark-key/c-fo-1
   '(contract (continuation-mark-key/c string?)
              (make-continuation-mark-key)
              'pos 'neg))

  (test/pos-blame
   'continuation-mark-key/c-fo-2
   '(contract (continuation-mark-key/c string?) 5 'pos 'neg))

  (test/neg-blame
   'continuation-mark-key/c-ho-1
   '(let ([mark (contract (continuation-mark-key/c number?)
                          (make-continuation-mark-key)
                          'pos
                          'neg)])
      (with-continuation-mark mark "bad"
        42)))

  (test/spec-passed
   'continuation-mark-key/c-ho-2
   '(let ([mark (contract (continuation-mark-key/c number?)
                          (make-continuation-mark-key)
                          'pos
                          'neg)])
      (with-continuation-mark mark 5
        (continuation-mark-set-first
         (current-continuation-marks) mark))))

  (test/neg-blame
   'continuation-mark-key/c-ho-3
   '(let ([mark (contract (continuation-mark-key/c number?)
                          (make-continuation-mark-key)
                          'pos
                          'neg)])
      (with-continuation-mark mark "bad"
        (continuation-mark-set-first
         (current-continuation-marks) mark))))

  (test/neg-blame
   'continuation-mark-key/c-ho-4
   '(let* ([mark (make-continuation-mark-key)]
           [do-mark (contract (-> (-> (continuation-mark-key/c (-> number? number?))
                                      number?)
                                  number?)
                              (lambda (f)
                                (with-continuation-mark mark (lambda (x) (+ x 1))
                                  (f mark)))
                              'pos
                              'neg)])
      (do-mark
       (lambda (mark)
         ((continuation-mark-set-first
           (current-continuation-marks) mark)
          "bad")))))

  (test/pos-blame
   'continuation-mark-key/c-ho-5
   '(let* ([mark (make-continuation-mark-key)]
           [do-mark (contract (-> (-> (continuation-mark-key/c (-> number? number?))
                                      number?)
                                  number?)
                              (lambda (f)
                                (with-continuation-mark mark (lambda (x) "bad")
                                  (f mark)))
                              'pos
                              'neg)])
      (do-mark
       (lambda (mark)
         ((continuation-mark-set-first
           (current-continuation-marks) mark)
          0)))))

  (test/spec-passed
   'continuation-mark-key/c-ho-6
   '(let* ([mark (make-continuation-mark-key)]
           [do-mark (contract (-> (-> (continuation-mark-key/c (-> number? number?))
                                      number?)
                                  number?)
                              (lambda (f)
                                (with-continuation-mark mark (lambda (x) (+ x 1))
                                  (f mark)))
                              'pos
                              'neg)])
      (do-mark
       (lambda (mark)
         ((continuation-mark-set-first
           (current-continuation-marks) mark)
          0)))))

  (test/neg-blame
   'continuation-mark-key/c-ho-7
   '(let ([mark (contract (continuation-mark-key/c (-> number? number?))
                          (make-continuation-mark-key)
                          'pos
                          'neg)])
      (with-continuation-mark mark (lambda (x) "bad")
        ((continuation-mark-set-first
          (current-continuation-marks) mark)
         5))))

  (test/spec-passed
   'continuation-mark-key/c-ho-8
   '(let ([mark (contract (continuation-mark-key/c (-> number? number?))
                          (make-continuation-mark-key)
                          'pos
                          'neg)])
      (with-continuation-mark mark (lambda (x) (+ x 1))
        ((continuation-mark-set-first
          (current-continuation-marks) mark)
         0))))

  (test/pos-blame
   'continuation-mark-key/c-ho-9
   '(let* ([mark (make-continuation-mark-key)]
           [do-mark (contract (-> (continuation-mark-key/c (-> number? number?))
                                  number?)
                              (lambda (mark)
                                ((continuation-mark-set-first
                                  (current-continuation-marks) mark)
                                 "bad"))
                              'pos
                              'neg)])
      (with-continuation-mark mark (lambda (x) (+ x 1))
        (do-mark mark))))

  (test/pos-blame
   'continuation-mark-key/c-ho-10
   '(let* ([mark (make-continuation-mark-key)]
           [ctc-mark (contract (continuation-mark-key/c number?)
                               mark
                               'pos
                               'neg)])
      (with-continuation-mark mark "not a number"
        (+ 1 (continuation-mark-set-first #f ctc-mark)))))

  (test/spec-passed
   'continuation-mark-key/c-ho-11
   '(let* ([mark (make-continuation-mark-key)]
           [ctc-mark (contract (continuation-mark-key/c number?)
                               mark
                               'pos
                               'neg)])
      (continuation-mark-set-first #f ctc-mark)))

  (test/spec-passed/result
   'continuation-mark-key/c-has-contract
   '(let* ([mark (make-continuation-mark-key)]
           [ctc-mark (contract (continuation-mark-key/c number?)
                               mark
                               'pos
                               'neg)])
      (has-contract? ctc-mark))
   #t)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;  make-contract
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (contract-eval
   '(define proj:add1->sub1
      (make-contract
       #:name 'proj:add1->sub1
       #:projection
       (lambda (blame)
         (lambda (f)
           (unless (and (procedure? f) (procedure-arity-includes? f 1))
             (raise-blame-error blame f "expected a unary function, got: ~e" f))
           (lambda (x)
             (unless (and (integer? x) (exact? x))
               (raise-blame-error (blame-swap blame) x
                                  "expected an integer, got: ~e" x))
             (let* ([y (f (add1 x))])
               (unless (and (integer? y) (exact? y))
                 (raise-blame-error blame y "expected an integer, got: ~e" y))
               (sub1 y)))))
       #:first-order
       (lambda (f)
         (and (procedure? f) (procedure-arity-includes? f 1))))))

  (test/spec-passed/result
   'make-contract-1
   '((contract proj:add1->sub1 sqrt 'pos 'neg) 15)
   3)

  (test/pos-blame
   'make-contract-2
   '(contract proj:add1->sub1 'dummy 'pos 'neg))

  (test/pos-blame
   'make-contract-3
   '((contract proj:add1->sub1 (lambda (x) 'dummy) 'pos 'neg) 2))

  (test/neg-blame
   'make-contract-4
   '((contract proj:add1->sub1 sqrt 'pos 'neg) 'dummy))

  (ctest #t contract? proj:add1->sub1)
  (ctest #f flat-contract? proj:add1->sub1)
  (ctest #f chaperone-contract? proj:add1->sub1)
  (ctest #t impersonator-contract? proj:add1->sub1)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;  make-chaperone-contract
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (contract-eval
   '(define proj:prime-box-list/c
      (let* ([prime? (λ (n)
                       (for/and ([m (in-range 2 (add1 (floor (sqrt n))))])
                         (not (= (remainder n m) 0))))]
             [wrap-box (λ (blame b)
                         (chaperone-box
                          b
                          (λ (b v)
                            (unless (prime? v)
                              (raise-blame-error blame v
                                                 "expected prime, got ~v" v))
                            v)
                          (λ (b v)
                            (unless (prime? v)
                              (raise-blame-error (blame-swap blame) v
                                                 "expected prime, got ~v" v))
                            v)))])
        (make-chaperone-contract
         #:name 'prime-box-list/c
         #:first-order (λ (v) (and (list? v) (andmap box? v)))
         #:projection (λ (blame)
                        (λ (v)
                          (unless (and (list? v) (andmap box? v))
                            (raise-blame-error blame v
                                               "expected list of boxes, got ~v" v))
                          (map (λ (b) (wrap-box blame b)) v)))))))

  (test/spec-passed/result
   'make-chaperone-contract-1
   '(contract proj:prime-box-list/c
              (list (box 2) (box 3) (box 5) (box 7))
              'pos 'neg)
   (list (box 2) (box 3) (box 5) (box 7)))

  (test/pos-blame
   'make-chaperone-contract-2
   '(let ([boxes (contract proj:prime-box-list/c
                           (list (box 2) (box 3) (box 4) (box 5))
                           'pos 'neg)])
      (unbox (caddr boxes))))

  (test/neg-blame
   'make-chaperone-contract-3
   '(let ([boxes (contract proj:prime-box-list/c
                           (list (box 2) (box 3) (box 4) (box 5))
                           'pos 'neg)])
      (set-box! (caddr boxes) 6)))

  (ctest #t contract? proj:prime-box-list/c)
  (ctest #f flat-contract? proj:prime-box-list/c)
  (ctest #t chaperone-contract? proj:prime-box-list/c)
  (ctest #f impersonator-contract? proj:prime-box-list/c)

  (contract-eval
   '(define proj:bad-prime-box-list/c
      (let* ([prime? (λ (n)
                       (for/and ([m (in-range 2 (add1 (floor (sqrt n))))])
                         (not (= (remainder n m) 0))))]
             [wrap-box (λ (blame b) (box (unbox b)))])
        (make-chaperone-contract
         #:name 'bad-prime-box-list/c
         #:first-order (λ (v) (and (list? v) (andmap box? v)))
         #:projection (λ (blame)
                        (λ (v)
                          (unless (and (list? v) (andmap box? v))
                            (raise-blame-error blame v
                                               "expected list of boxes, got ~v" v))
                          (map (λ (b) (wrap-box blame b)) v)))))))

  (ctest #t contract? proj:bad-prime-box-list/c)
  (ctest #f flat-contract? proj:bad-prime-box-list/c)
  (ctest #t chaperone-contract? proj:bad-prime-box-list/c)
  (ctest #f impersonator-contract? proj:bad-prime-box-list/c)

  (contract-error-test
   'contract-error-test5
   '(contract proj:bad-prime-box-list/c (list (box 2) (box 3)) 'pos 'neg)
   exn:fail?)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;  make-flat-contract
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (contract-eval
   '(define proj:prime/c
      (let ([prime? (λ (n)
                      (for/and ([m (in-range 2 (add1 (floor (sqrt n))))])
                        (not (= (remainder n m) 0))))])
        (make-flat-contract
         #:name 'prime/c
         #:first-order prime?))))

  (test/spec-passed/result
   'make-flat-contract-1
   '(contract proj:prime/c 2 'pos 'neg)
   2)

  (test/pos-blame
   'make-flat-contract-2
   '(contract proj:prime/c 4 'pos 'neg))

  (ctest #t contract? proj:prime/c)
  (ctest #t flat-contract? proj:prime/c)

  (test/spec-passed/result
   'make-flat-contract-5
   '(chaperone-contract? proj:prime/c)
   #t)

  ;; Check to make sure that flat contracts always return the original value,
  ;; even if the projection is written badly.
  (contract-eval
   '(define proj:prime-list/c
      (let ([prime? (λ (n)
                      (for/and ([m (in-range 2 (add1 (floor (sqrt n))))])
                        (not (= (remainder n m) 0))))])
        (make-flat-contract
         #:name 'prime-list/c
         #:first-order (λ (v) (and (list? v) (andmap prime? v)))
         #:projection (λ (b)
                        (λ (v)
                          (unless (and (list? v) (andmap prime? v))
                            (raise-blame-error b v "expected prime list, got ~v" v))
                          (map values v)))))))

  (test/spec-passed/result
   'make-flat-contract-bad-1
   '(contract proj:prime-list/c (list 2 3 5 7) 'pos 'neg)
   (list 2 3 5 7))

  (test/pos-blame
   'make-flat-contract-bad-2
   '(contract proj:prime-list/c (list 2 3 4 5) 'pos 'neg))

  (test/spec-passed/result
   'make-flat-contract-bad-3
   '(let ([l (list 2 3 5 7)])
      (eq? l (contract proj:prime-list/c l 'pos 'neg)))
   #t)

  (ctest #t contract? proj:prime-list/c)
  (ctest #t flat-contract? proj:prime-list/c)

  (test/spec-passed/result
   'make-flat-contract-bad-6
   '(chaperone-contract? proj:prime-list/c)
   #t)
  

;; Adding tests for using vector/box/hash contracts with already chaperoned values
  
  (test/no-error
   '(let ([v (chaperone-vector (vector-immutable 1)
                               (λ (vec i v) v)
                               (λ (vec i v) v))])
      (contract (vectorof any/c) v 'pos 'neg)))
  
  (test/no-error
   '(let ([v (chaperone-vector (vector-immutable 1)
                               (λ (vec i v) v)
                               (λ (vec i v) v))])
      (contract (vector/c any/c) v 'pos 'neg)))
  
  (test/no-error
   '(let ([v (chaperone-box (box-immutable 1)
                            (λ (box v) v)
                            (λ (box v) v))])
      (contract (box/c any/c) v 'pos 'neg)))
  
  (test/no-error
   '(let ([v (chaperone-hash (make-immutable-hash (list (cons 1 2)))
                             (λ (hash k) (values k (λ (h k v) v)))
                             (λ (hash k v) (values k v))
                             (λ (hash k) k)
                             (λ (hash k) k))])
      (contract (hash/c any/c any/c) v 'pos 'neg)))
  
  (test/no-error
   '(let ([v (chaperone-hash (make-immutable-hasheq (list (cons 1 2)))
                             (λ (hash k) (values k (λ (h k v) v)))
                             (λ (hash k v) (values k v))
                             (λ (hash k) k)
                             (λ (hash k) k))])
      (contract (hash/c any/c any/c) v 'pos 'neg)))
  
  (test/no-error
   '(let ([v (chaperone-hash (make-immutable-hasheqv (list (cons 1 2)))
                             (λ (hash k) (values k (λ (h k v) v)))
                             (λ (hash k v) (values k v))
                             (λ (hash k) k)
                             (λ (hash k) k))])
      (contract (hash/c any/c any/c) v 'pos 'neg)))
  
  
;
;
;
;        ;          ;;;;                  ;
;       ;;         ;   ;                  ;
;        ;         ;                     ;                        ;                      ;
;        ;         ;                     ;                        ;                      ;
;     ;; ;   ;;;  ;;;; ;  ; ;;    ;;;    ;   ;;;     ;;    ; ;;  ;;;; ; ;;  ;;;    ;;;  ;;;;
;    ;  ;;  ;   ;  ;  ;; ;;;  ;  ;   ;  ;   ;   ;   ;  ;  ;;;  ;  ;  ;;;   ;   ;  ;   ;  ;
;   ;    ;  ;;;;;  ;   ;  ;   ;  ;;;;;  ;   ;      ;    ;  ;   ;  ;   ;       ;;  ;      ;
;   ;    ;  ;      ;   ;  ;   ;  ;      ;   ;      ;    ;  ;   ;  ;   ;     ;; ;  ;      ;
;   ;    ;  ;      ;   ;  ;   ;  ;      ;   ;      ;    ;  ;   ;  ;   ;    ;   ;  ;      ;
;    ;  ;;; ;   ;  ;   ;  ;   ;  ;   ; ;    ;   ;   ;  ;   ;   ;  ; ; ;    ;  ;;  ;   ;  ; ;
;     ;; ;   ;;;  ;;;;;;;;;; ;;;  ;;;  ;     ;;;     ;;   ;;; ;;; ;; ;;;    ;; ;;  ;;;   ;;
;
;
;

  (test/spec-passed
   'define/contract1
   '(let ()
      (define/contract i integer? 1)
      i))

  (test/spec-failed
   'define/contract2
   '(let ()
      (define/contract i integer? #t)
      i)
   "(definition i)")

  (test/spec-failed
   'define/contract3
   '(let ()
      (define/contract i (-> integer? integer?) (lambda (x) #t))
      (i 1))
   "(definition i)")

  (test/spec-failed
   'define/contract4
   '(let ()
      (define/contract i (-> integer? integer?) (lambda (x) 1))
      (i #f))
   "top-level")

  (test/spec-failed
   'define/contract5
   '(let ()
      (define/contract (i x) (-> integer? integer?) 1)
      (i #f))
   "top-level")

  (test/spec-passed
   'define/contract6
   '(let ()
      (define/contract (i x) (-> integer? integer?)
        (cond
          [(not (integer? x)) 1]
          [else (i #f)]))
      (i 1)))

  (test/spec-passed
   'define/contract7
   '(let ()
      (define/contract (contracted-func label t)
                       (string?  string? . -> . string?)
                       t)
      (contracted-func
       "I'm a string constant with side effects"
       "ans")))

  (test/spec-passed
   'define/contract8
   '(let ()
      (eval '(module contract-test-suite-define1 scheme/base
               (require scheme/contract)
               (define/contract x string? "a")
               x))
      (eval '(require 'contract-test-suite-define1))))

  (test/spec-failed
   'define/contract9
   '(let ()
      (define/contract (a n)
        (-> number? number?)
        (define/contract (b m)
          (-> number? number?)
          (+ m 1))
        (b (zero? n)))
      (a 5))
   "(function a)")

  (test/spec-failed
   'define/contract10
   '(let ()
      (define/contract (a n)
        (-> number? number?)
        (define/contract (b m)
          (-> number? number?)
          #t)
        (b (add1 n)))
      (a 5))
   "(function b)")

  (test/spec-passed
   'define/contract11
   '(let ()
      (define/contract (f n)
        (-> number? number?)
        (+ n 1))
      (define/contract (g b m)
        (-> boolean? number? number?)
        (if b (f m) (f #t)))
      (g #t 3)))

  ;; For some of the following tests, it may not be clear
  ;; why the blame is what it is.  The contract(s) entered
  ;; into via with-contract are between the contracting
  ;; region and its context.  If the context allows the
  ;; value to flow into other regions without contracts
  ;; that protect it from misuses in those regions, it's
  ;; the context's fault.
  (test/spec-failed
   'define/contract12
   '(let ()
      (define/contract (f n)
        (-> number? number?)
        (+ n 1))
      (define/contract (g b m)
        (-> boolean? number? number?)
        (if b (f m) (f #t)))
      (g #f 3))
   "top-level")

  (test/spec-failed
   'define/contract13
   '(begin
      (eval '(module foo-dc13 scheme/base
	       (require scheme/contract)
	       (define/contract (foo-dc13 n)
		 (-> number? number?)
		 (+ n 1))
	       (foo-dc13 #t)))
      (eval '(require 'foo-dc13)))
   "foo-dc13")

  (test/spec-failed
   'define/contract14
   '(begin
      (eval '(module foo-dc14 scheme/base
	       (require scheme/contract)
	       (provide foo-dc14)
	       (define/contract (foo-dc14 n)
		 (-> number? number?)
		 (+ n 1))))
      (eval '(module bar-dc14 scheme/base
	       (require 'foo-dc14)
	       (foo-dc14 #t)))
      (eval '(require 'bar-dc14)))
   "foo-dc14")

  (test/spec-failed
   'define/contract15
   '(begin
      (eval '(module foo-dc15 scheme/base
	       (require scheme/contract)
	       (provide foo-dc15)
	       (define/contract (foo-dc15 n)
		 (-> number? number?)
		 (+ n 1))))
      (eval '(require 'foo-dc15))
      (eval '(foo-dc15 #t)))
   "foo-dc15")

  ;; Let's see how units + define/contract interact

  (test/spec-failed
   'define/contract16
   '(begin
      (eval '(module foo-dc16 scheme/base
               (require scheme/contract)
               (require scheme/unit)
               (let ()
                 (define/contract (foo n)
                   (-> number? number?)
                   (define-signature U^
                     ((contracted [x (-> number? number?)])))
                   (define-unit U@
                     (import)
                     (export U^)
                     (define (x n) #t))
                   (define-values/invoke-unit U@
                     (import)
                     (export U^))
                   (x n))
                 (foo 3))))
      (eval '(require 'foo-dc16)))
   "(unit U@)")

  (test/spec-failed
   'define/contract16a
   '(begin
      (eval '(module foo-dc16a scheme/base
               (require scheme/contract)
               (require scheme/unit)
               (let ()
                 (define/contract (foo n)
                   (-> number? number?)
                   (define-signature U^
                     (x))
                   (define-unit/contract U@
                     (import)
                     (export (U^ [x (-> number? number?)]))
                     (define (x n) #t))
                   (define-values/invoke-unit U@
                     (import)
                     (export U^))
                   (x n))
                 (foo 3))))
      (eval '(require 'foo-dc16a)))
   "(unit U@)")

  (test/spec-failed
   'define/contract17
   '(begin
      (eval '(module foo-dc17 scheme/base
               (require scheme/contract)
               (require scheme/unit)
               (let ()
                 (define/contract (foo n)
                   (-> number? number?)
                   (define-signature U^
                     ((contracted [x (-> number? number?)])))
                   (define-unit U@
                     (import)
                     (export U^)
                     (define (x n) 3))
                   (define-values/invoke-unit U@
                     (import)
                     (export U^))
                   (x (zero? n)))
                 (foo 3))))
      (eval '(require 'foo-dc17)))
   "(function foo)")

  (test/spec-failed
   'define/contract18
   '(begin
      (eval '(module foo-dc18 scheme/base
               (require scheme/contract)
               (require scheme/unit)
               (let ()
                 (define-signature U^
                   ((contracted [x (-> number? number?)])))
                 (define-unit U@
                   (import)
                   (export U^)
                   ;; Can't define/contract x directly because
                   ;; x ends up bound to a transformer and thus
                   ;; is syntax.
                   (define/contract (y n)
                     (-> number? boolean?) #t)
                   (define x y))
                 (define-values/invoke-unit U@
                   (import)
                   (export U^))
                 (x 3))))
      (eval '(require 'foo-dc18)))
   "(unit U@)")

  (test/spec-failed
   'define/contract19
   '(let ()
      (define y 3)
      (define/contract (f n)
        (-> number? number?)
        #:freevar y (-> number? boolean?)
        3)
      1)
   "top-level")

  (test/spec-passed
   'define/contract20
   '(let ()
      (define y (lambda (n) 4))
      (define/contract (f n)
        (-> number? number?)
        #:freevar y (-> number? boolean?)
        3)
      1))

  (test/spec-passed
   'define/contract21
   '(let ()
      (define y (lambda (n) 4))
      (define/contract (f n)
        (-> number? number?)
        #:freevar y (-> number? boolean?)
        (if (y n) 3 1))
      1))

  (test/spec-failed
   'define/contract22
   '(let ()
      (define y 4)
      (define/contract (f n)
        (-> number? number?)
        #:freevar y (-> number? boolean?)
        3)
      1)
   "top-level")

  (test/spec-failed
   'define/contract23
   '(let ()
      (define y (lambda (n) #t))
      (define/contract (f n)
        (-> number? number?)
        #:freevar y (-> number? number?)
        (y n))
      (f 5))
   "top-level")

  (test/spec-failed
   'define/contract24
   '(let ()
      (define y (lambda (n) 4))
      (define/contract (f n)
        (-> number? number?)
        #:freevar y (-> number? boolean?)
        (if (y #t) 3 1))
      (f 5))
   "(function f)")

  (test/spec-failed
   'define/contract25
   '(let ()
      (define y #t)
      (define z 3)
      (define/contract f
        number?
        #:freevars ([y number?] [z number?])
        (+ y z))
      1)
   "top-level")



;
;
;
;        ;          ;;;;                                                         ;
;       ;;         ;   ;                                                         ;
;        ;         ;                                ;                      ;    ;                        ;                      ;
;        ;         ;                                ;                      ;    ;                        ;                      ;
;     ;; ;   ;;;  ;;;; ;  ; ;;    ;;;          ;;; ;;;; ; ;;;;  ;;   ;;;  ;;;;  ;   ;;;     ;;    ; ;;  ;;;; ; ;;  ;;;    ;;;  ;;;;
;    ;  ;;  ;   ;  ;  ;; ;;;  ;  ;   ;        ;  ;  ;  ;;;   ;   ;  ;   ;  ;   ;   ;   ;   ;  ;  ;;;  ;  ;  ;;;   ;   ;  ;   ;  ;
;   ;    ;  ;;;;;  ;   ;  ;   ;  ;;;;;        ;     ;   ;    ;   ;  ;      ;   ;   ;      ;    ;  ;   ;  ;   ;       ;;  ;      ;
;   ;    ;  ;      ;   ;  ;   ;  ;      ;;;;   ;;   ;   ;    ;   ;  ;      ;   ;   ;      ;    ;  ;   ;  ;   ;     ;; ;  ;      ;
;   ;    ;  ;      ;   ;  ;   ;  ;               ;  ;   ;    ;   ;  ;      ;   ;   ;      ;    ;  ;   ;  ;   ;    ;   ;  ;      ;
;    ;  ;;; ;   ;  ;   ;  ;   ;  ;   ;        ;  ;  ; ; ;    ;  ;;; ;   ;  ; ;;    ;   ;   ;  ;   ;   ;  ; ; ;    ;  ;;  ;   ;  ; ;
;     ;; ;   ;;;  ;;;;;;;;;; ;;;  ;;;         ;;;   ;; ;;;    ;; ;   ;;;   ;; ;     ;;;     ;;   ;;; ;;; ;; ;;;    ;; ;;  ;;;   ;;
;
;
;

  (test/spec-passed
   'define-struct/contract1
   '(let ()
      (define-struct/contract foo ([x number?] [y number?]))
      1))

  (test/spec-passed
   'define-struct/contract2
   '(let ()
      (define-struct/contract foo ([x number?] [y number?]))
      (make-foo 1 2)))

  (test/spec-failed
   'define-struct/contract3
   '(let ()
      (define-struct/contract foo ([x number?] [y number?]))
      (make-foo 1 #t))
   "top-level")

  (test/spec-passed
   'define-struct/contract4
   '(let ()
      (define-struct/contract foo ([x number?] [y number?]))
      (foo-y (make-foo 2 3))))

  (test/spec-failed
   'define-struct/contract5
   '(let ()
      (define-struct/contract foo ([x number?] [y number?]))
      (foo-y 1))
   "top-level")

  (test/spec-passed
   'define-struct/contract6
   '(let ()
      (define-struct/contract foo ([x number?] [y number?]) #:mutable)
      (set-foo-y! (make-foo 1 2) 3)
      (set-foo-x! (make-foo 1 2) 3)))

  (test/spec-failed
   'define-struct/contract7
   '(let ()
      (define-struct/contract foo ([x number?] [y number?]) #:mutable)
      (set-foo-y! (make-foo 1 2) #f))
   "top-level")

  (test/spec-passed
   'define-struct/contract8
   '(let ()
      (define-struct/contract foo ([(x #:mutable) number?] [y number?]))
      (set-foo-x! (make-foo 1 2) 4)))

  (test/spec-failed
   'define-struct/contract9
   '(let ()
      (define-struct/contract foo ([(x #:mutable) number?] [y number?]))
      (set-foo-x! (make-foo 1 2) #f))
   "top-level")

  (test/spec-failed
   'define-struct/contract10
   '(let ()
      (define-struct/contract foo ([x number?] [(y #:auto) number?]))
      (make-foo 1))
   "(struct foo)")

  (test/spec-passed
   'define-struct/contract11
   '(let ()
      (define-struct/contract foo ([x number?] [(y #:auto) number?]) #:auto-value 3)
      (make-foo 1)))

  (test/spec-passed
   'define-struct/contract12
   '(let ()
      (define-struct/contract foo ([x number?] [(y #:auto #:mutable) number?]) #:auto-value 3)
      (set-foo-y! (make-foo 1) 3)))

  (test/spec-failed
   'define-struct/contract13
   '(let ()
      (define-struct/contract foo ([x number?] [(y #:auto #:mutable) number?]) #:auto-value 3)
      (set-foo-y! (make-foo 1) #t))
   "top-level")

  (test/spec-passed
   'define-struct/contract14
   '(let ()
      (define-struct/contract foo ([x number?] [y number?]) #:transparent)
      1))

  (test/spec-passed
    'define-struct/contract15
    '(let ()
       (define-struct foo (x))
       (define-struct/contract (bar foo) ([z string?]))
       (make-bar 2 "x")))

  (test/spec-failed
    'define-struct/contract16
    '(let ()
       (define-struct foo (x))
       (define-struct/contract (bar foo) ([z string?]))
       (make-bar 2 #f))
    "top-level")

  (test/spec-passed
    'define-struct/contract17
    '(let ()
       (define-struct foo (x))
       (define-struct/contract (bar foo) ([z string?]) #:mutable)
       (set-bar-z! (make-bar 2 "x") "y")))

  (test/spec-failed
    'define-struct/contract18
    '(let ()
       (define-struct foo (x))
       (define-struct/contract (bar foo) ([z string?]) #:mutable)
       (set-bar-z! (make-bar 2 "x") #f))
    "top-level")

  (test/spec-passed
    'define-struct/contract19
    '(let ()
       (define-struct foo (x))
       (define-struct/contract (bar foo) ([z string?]))
       (define-struct/contract (baz bar) ([x number?]))
       (make-baz 2 "x" 5)))

  (test/spec-failed
    'define-struct/contract20
    '(let ()
       (define-struct foo (x))
       (define-struct/contract (bar foo) ([z string?]))
       (define-struct/contract (baz bar) ([x number?]))
       (make-baz 2 "x" #f))
    "top-level")

  (test/spec-failed
    'define-struct/contract21
    '(let ()
       (define-struct foo (x))
       (define-struct/contract (bar foo) ([z string?]))
       (define-struct/contract (baz bar) ([x number?]))
       (make-baz 2 #f 3))
    "top-level")

  (test/spec-passed
    'define-struct/contract21
    '(let ()
       (define-struct foo (x) #:mutable)
       (define-struct/contract (bar foo) ([z string?]))
       (set-foo-x! (make-bar 2 "x") #f)))

  (test/spec-passed
   'define-struct/contract22
   '(define-struct/contract foo ([x number?] [y number?]) #:mutable #:transparent))

  (test/spec-passed
   'define-struct/contract23
   '(define-struct/contract foo ([x number?] [y number?])
                            #:mutable #:transparent
                            #:property prop:custom-write
                            (lambda (a b c) (void))))

  (test/spec-passed/result
   'define-struct/contract24
   '(let ()
      (define-struct/contract point
        ([x number?] [y number?])
        #:transparent)
      (define-struct/contract (color-point point)
        ([c symbol?])
        #:transparent)

      (match (make-color-point 1 2 'red)
        [(struct color-point [dx dy color])
         (list dx dy color)]
        [(struct point [dx dy]) (list dx dy)]
        [v (box v)]))
   (list 1 2 'red))

  (test/spec-passed
   'define-struct/contract25
   '(let ()
      (define-struct/contract point
        ([x number?] [y number?])
        #:transparent)
      (point 1 2)))

  (test/spec-failed
   'define-struct/contract26
   '(let ()
      (define-struct/contract point
        ([x number?] [y number?])
        #:transparent)
      (point 1 #t))
   "top-level")
;
;
;
;              ;      ;
;                    ;;
;                 ;   ;                                  ;                      ;
;                 ;   ;                                  ;                      ;
;  ;;; ;;; ;;; ; ;;;; ; ;;          ;;;     ;;    ; ;;  ;;;; ; ;;  ;;;    ;;;  ;;;;
;   ;   ;   ; ;;  ;   ;;  ;        ;   ;   ;  ;  ;;;  ;  ;  ;;;   ;   ;  ;   ;  ;
;   ;   ;   ;  ;  ;   ;   ;        ;      ;    ;  ;   ;  ;   ;       ;;  ;      ;
;    ; ; ; ;   ;  ;   ;   ;  ;;;;  ;      ;    ;  ;   ;  ;   ;     ;; ;  ;      ;
;    ; ; ; ;   ;  ;   ;   ;        ;      ;    ;  ;   ;  ;   ;    ;   ;  ;      ;
;    ;;  ;;    ;  ; ; ;   ;        ;   ;   ;  ;   ;   ;  ; ; ;    ;  ;;  ;   ;  ; ;
;     ;   ;   ;;; ;; ;;; ;;;        ;;;     ;;   ;;; ;;; ;; ;;;    ;; ;;  ;;;   ;;
;
;
;

  (test/spec-passed
   'with-contract-def-1
   '(let ()
      (with-contract odd-even
        ([oddp (-> number? boolean?)]
         [evenp (-> number? boolean?)])
        (define (oddp n)
          (if (zero? n) #f (evenp (sub1 n))))
        (define (evenp n)
          (if (zero? n) #t (oddp (sub1 n)))))
      (oddp 5)))

  (test/spec-failed
   'with-contract-def-2
   '(let ()
      (with-contract odd-even
        ([oddp (-> number? boolean?)]
         [evenp (-> number? boolean?)])
        (define (oddp n)
          (if (zero? n) #f (evenp (sub1 n))))
        (define (evenp n)
          (if (zero? n) #t (oddp (sub1 n)))))
      (oddp #t))
   "top-level")

  (test/spec-failed
   'with-contract-def-3
   '(let ()
      (with-contract odd-even
        ([oddp (-> number? boolean?)]
         [evenp (-> number? boolean?)])
        (define (oddp n)
          (if (zero? n) n (evenp (sub1 n))))
        (define (evenp n)
          (if (zero? n) #t (oddp (sub1 n)))))
      (oddp 4))
   "(region odd-even)")

  ;; Functions within the same with-contract region can call
  ;; each other however they want, so here we have even?
  ;; call odd? with a boolean, even though its contract in
  ;; the odd-even contract says it only takes numbers.
  (test/spec-passed
   'with-contract-def-4
   '(let ()
      (with-contract odd-even
        ([oddp (-> number? boolean?)]
         [evenp (-> number? boolean?)])
        (define (oddp n)
          (cond
            [(not (number? n)) #f]
            [(zero? n) #f]
            [else (evenp (sub1 n))]))
        (define (evenp n)
          (if (zero? n) #t (oddp (zero? n)))))
      (oddp 5)))

  (test/spec-passed
   'with-contract-def-5
   '(let ()
      (with-contract region1
        ([x (-> number? number?)])
        (with-contract region2
          ([y (-> number? boolean?)])
          (define (y n) #t))
        (define (x n) (if (y n) 0 3)))
      (x 4)))

  (test/spec-failed
   'with-contract-def-6
   '(let ()
      (with-contract region1
        ([x (-> number? number?)])
        (with-contract region2
          ([y (-> number? boolean?)])
          (define (y n) #t))
        (define (x n) (y n)))
      (x 4))
   "(region region1)")

  (test/spec-failed
   'with-contract-def-7
   '(let ()
      (with-contract region1
        ([x (-> number? number?)])
        (with-contract region2
          ([y (-> number? boolean?)])
          (define (y n) #t))
        (define (x n) (if (y #t) 4 0)))
      (x 4))
   "(region region1)")

  (test/spec-failed
   'with-contract-def-8
   '(let ()
      (with-contract region1
        ([x (-> number? number?)])
        (with-contract region2
          ([y (-> number? boolean?)])
          (define (y n) 3))
        (define (x n) (if (y n) 4 0)))
      (x 4))
   "(region region2)")

  ;; make sure uncontracted exports make it out
  (test/spec-passed
   'with-contract-def-9
   '(let ()
      (with-contract region1 ()
        (define f 3))
      f))

  (test/spec-failed
   'with-contract-def-10
   '(let ()
      (with-contract r
        ([x number?])
        (define x 3)
        (define-values ()
          (begin (set! x #f) (values))))
      x)
   "(region r)")

  (test/spec-failed
   'with-contract-def-11
   '(let ()
      (with-contract r
        ([x number?])
        (define x 3))
      (set! x #f)
      x)
   "top-level")

  (test/spec-passed
   'with-contract-exp-1
   '(with-contract r
      #:result number?
      3))

  (test/spec-failed
   'with-contract-exp-2
   '(with-contract r
      #:result number?
      "foo")
   "(region r)")

  (test/spec-passed
   'with-contract-exp-3
   '((with-contract r
      #:result (-> number? number?)
      (λ (x) 5))
     3))

  (test/spec-failed
   'with-contract-exp-4
   '((with-contract r
      #:result (-> number? number?)
      (λ (x) (zero? x)))
     3)
   "(region r)")

  (test/spec-failed
   'with-contract-exp-5
   '((with-contract r
      #:result (-> number? number?)
      (λ (x) 5))
     #t)
   "top-level")

  (test/spec-passed
   'with-contract-exp-values-1
   '(let-values ([() (with-contract r #:results () (values))])
      1))

  (test/spec-passed
   'with-contract-exp-values-1
   '(let-values ([(x y) (with-contract r
                          #:results (number? string?)
                          (values 3 "foo"))])
      1))

  (test/spec-failed
   'with-contract-exp-values-2
   '(let-values ([(x y) (with-contract r
                          #:results (number? string?)
                          (values "bar" "foo"))])
      1)
   "(region r)")

  (test/spec-passed
   'with-contract-exp-values-3
   '(let-values ([(x y) (with-contract r
                          #:results (number? string?)
                          (define (f) (values 3 "foo"))
                          (f))])
      1))

  (test/spec-passed/result
   'with-contract-#%app
   '(begin
      (eval '(module with-contract-#%app-app racket
               (define-syntax (-app x) #''apped)
               (provide (rename-out (-app #%app)))))
      (eval '(module with-contract-#%app-client racket
               (require 'with-contract-#%app-app)
               (provide with-contract-#%app-h with-contract-#%app-i)
               (with-contract x ([f any/c]) (define (f x) 'f))
               (define (g x) 'g)
               (define with-contract-#%app-h (f 2))
               (define with-contract-#%app-i (g 2))))
      (eval '(require 'with-contract-#%app-client))
      (eval '(list with-contract-#%app-h with-contract-#%app-i)))
   (list 'apped 'apped))

;
;
;
;           ;;;;         ;;                     ;                                      ;                               ;
;           ;;;;         ;;                    ;;                                     ;;                              ;;
;    ;;;;   ;;;;;;;           ;;;     ;;;;;  ;;;;;        ;;;;;   ;;;;   ;;;; ;;;   ;;;;; ;;; ;;; ;;;;;;;    ;;;;;  ;;;;;
;   ;;;;;;  ;;;;;;;;   ;;;;  ;;;;;   ;;;;;; ;;;;;;       ;;;;;;  ;;;;;;  ;;;;;;;;; ;;;;;; ;;;;;;; ;;;;;;;;  ;;;;;; ;;;;;;
;  ;;;;;;;; ;;;;;;;;;  ;;;; ;;;; ;; ;;;;;;;  ;;;;       ;;;;;;; ;;;;;;;; ;;;; ;;;;  ;;;;  ;;;; ;;     ;;;; ;;;;;;;  ;;;;
;  ;;;; ;;; ;;;; ;;;;  ;;;; ;;;;;;; ;;;;     ;;;;  ;;;;;;;;;    ;;;; ;;; ;;;; ;;;;  ;;;;  ;;;;     ;;;;;;; ;;;;     ;;;;
;  ;;;;;;;; ;;;;;;;;;  ;;;; ;;;;;   ;;;;;;;  ;;;;; ;;;;;;;;;;;; ;;;;;;;; ;;;; ;;;;  ;;;;; ;;;;    ;;  ;;;; ;;;;;;;  ;;;;;
;   ;;;;;;  ;;;;;;;;   ;;;;  ;;;;;;  ;;;;;;  ;;;;;       ;;;;;;  ;;;;;;  ;;;; ;;;;  ;;;;; ;;;;    ;;;;;;;;  ;;;;;;  ;;;;;
;    ;;;;   ;;;;;;;   ;;;;;   ;;;;    ;;;;;   ;;;;        ;;;;;   ;;;;   ;;;; ;;;;   ;;;; ;;;;     ;; ;;;;   ;;;;;   ;;;;
;                     ;;;;;
;                     ;;;;
;


  (test/spec-passed
   'object-contract0
   '(contract (object-contract)
              (new object%)
              'pos
              'neg))

  (test/pos-blame
   'object-contract/field1
   '(contract (object-contract (field x integer?))
              (new object%)
              'pos
              'neg))

  (test/pos-blame
   'object-contract/field2
   '(get-field
     x
     (contract (object-contract (field x integer?))
               (new (class object% (field [x #t]) (super-new)))
               'pos
               'neg)))

  (test/spec-passed/result
   'object-contract/field3
   '(get-field
     x
     (contract (object-contract (field x integer?))
               (new (class object% (field [x 12]) (super-new)))
               'pos
               'neg))
   12)

  (test/pos-blame
   'object-contract/field4
   '(get-field
     y
     (contract (object-contract (field x boolean?) (field y boolean?))
               (new (class object% (field [x #t] [y 'x]) (super-new)))
               'pos
               'neg)))

  (test/pos-blame
   'object-contract/field5
   '(get-field
     x
     (contract (object-contract (field x symbol?) (field y symbol?))
               (new (class object% (field [x #t] [y 'x]) (super-new)))
               'pos
               'neg)))

  (test/spec-passed/result
   'object-contract/field6
   '(let ([o (contract (object-contract [m (integer? . -> . integer?)])
                       (new (class object% (field [x 1]) (define/public (m y) x) (super-new)))
                       'pos
                       'neg)])
      (list (send o m 2)
            (send/apply o m '(2))
            (let ([x '(2)]) (send o m . x))
            (with-method ([mm (o m)])
              (mm 2))
            (send* o (m 3) (m 4))))
   (list 1 1 1 1 1))

  (test/spec-passed/result
   'object-contract/field7
   '(let ([o (contract (object-contract)
                       (new (class object% (field [x 1]) (define/public (m y) x) (super-new)))
                       'pos
                       'neg)])
      (list (send o m 2)
            (send/apply o m '(2))
            (let ([x '(2)]) (send o m . x))
            (with-method ([mm (o m)])
              (mm 2))
            (send* o (m 3) (m 4))))
   (list 1 1 1 1 1))

  (test/spec-passed/result
   'object-contract/field8
   '(let ([o (contract (object-contract [m (integer? . -> . integer?)])
                       (new (class object% (define x 6) (define/public (m y) x) (super-new)))
                       'pos
                       'neg)])
      (list (send o m 2)
            (send/apply o m '(2))
            (let ([x '(2)]) (send o m . x))
            (with-method ([mm (o m)])
              (mm 2))
            (send* o (m 3) (m 4))))
   (list 6 6 6 6 6))

  (test/spec-passed/result
   'object-contract/field9
   '(let ([o (contract (object-contract)
                       (new (class object% (define x 6) (define/public (m y) x) (super-new)))
                       'pos
                       'neg)])
      (list (send o m 2)
            (send/apply o m '(2))
            (let ([x '(2)]) (send o m . x))
            (with-method ([mm (o m)])
              (mm 2))
            (send* o (m 3) (m 4))))
   (list 6 6 6 6 6))

  (test/spec-passed/result
   'object-contract/field10
   '(send (contract (object-contract)
                    (new (class object% (define x 1) (define/public (m y) x) (super-new)))
                    'pos
                    'neg)
          m
          2)
   1)

  (test/spec-passed/result
   'object-contract->1
   '(send
     (contract (object-contract (m (integer? . -> . integer?)))
               (new (class object% (define/public (m x) x) (super-new)))
               'pos
               'neg)
     m
     1)
   1)

  (test/pos-blame
   'object-contract->2
   '(contract (object-contract (m (integer? . -> . integer?)))
              (make-object object%)
              'pos
              'neg))

  (test/neg-blame
   'object-contract->3
   '(send
     (contract (object-contract (m (integer? . -> . integer?)))
               (make-object (class object% (define/public (m x) x) (super-instantiate ())))
               'pos
               'neg)
     m
     'x))

  (test/pos-blame
   'object-contract->4
   '(send
     (contract (object-contract (m (integer? . -> . integer?)))
               (make-object (class object% (define/public (m x) 'x) (super-instantiate ())))
               'pos
               'neg)
     m
     1))

  (test/pos-blame
   'object-contract->5
   '(contract (object-contract (m (integer? integer? . -> . integer?)))
              (make-object (class object% (define/public (m x) 'x) (super-instantiate ())))
              'pos
              'neg))

  (test/spec-passed/result
   'object-contract->6
   '(send
     (contract (object-contract (m (integer? . -> . any)))
               (new (class object% (define/public (m x) x) (super-new)))
               'pos
               'neg)
     m
     1)
   1)

  (test/neg-blame
   'object-contract->7
   '(send
     (contract (object-contract (m (integer? . -> . any)))
               (make-object (class object% (define/public (m x) x) (super-instantiate ())))
               'pos
               'neg)
     m
     'x))

  (test/spec-passed
   'object-contract->8
   '(begin
      (send
       (contract (object-contract (m (integer? . -> . any)))
                 (make-object (class object% (define/public (m x) (values 1 2)) (super-instantiate ())))
                 'pos
                 'neg)
       m
       1)
      (void)))

  (test/spec-passed
   'object-contract->9
   '(begin
      (send
       (contract (object-contract (m (integer? . -> . any)))
                 (make-object (class object% (define/public (m x) (values)) (super-instantiate ())))
                 'pos
                 'neg)
       m
       1)
      (void)))

  (test/spec-passed
   'object-contract->10
   '(begin
      (send (contract (object-contract (m (integer? . -> . (values integer? boolean?))))
                      (make-object (class object% (define/public (m x) (values 1 #t)) (super-instantiate ())))
                      'pos
                      'neg)
            m 1)
      (void)))

  (test/neg-blame
   'object-contract->11
   '(send
     (contract (object-contract (m (integer? . -> . (values integer? boolean?))))
               (make-object (class object% (define/public (m x) (values #t #t)) (super-instantiate ())))
               'pos
               'neg)
     m
     #f))

  (test/pos-blame
   'object-contract->12
   '(send
     (contract (object-contract (m (integer? . -> . (values integer? boolean?))))
               (make-object (class object% (define/public (m x) (values #t #t)) (super-instantiate ())))
               'pos
               'neg)
     m
     1))

  (test/pos-blame
   'object-contract->13
   '(send (contract (object-contract (m (integer? . -> . (values integer? boolean?))))
                    (make-object (class object% (define/public (m x) (values #f #t)) (super-instantiate ())))
                    'pos
                    'neg)
          m 1))

  (test/pos-blame
   'object-contract->14
   '(send (contract (object-contract (m (integer? . -> . (values integer? boolean?))))
                    (make-object (class object% (define/public (m x) (values 5 6)) (super-instantiate ())))
                    'pos
                    'neg)
          m 1))

  (test/pos-blame
   'object-contract-case->1
   '(contract (object-contract (m (case-> (boolean? . -> . boolean?)
                                          (integer? integer? . -> . integer?))))
              (new object%)
              'pos
              'neg))

  (test/pos-blame
   'object-contract-case->2
   '(contract (object-contract (m (case-> (boolean? . -> . boolean?)
                                          (integer? integer? . -> . integer?))))
              (new (class object% (define/public (m x) x) (super-new)))
              'pos
              'neg))

  (test/pos-blame
   'object-contract-case->3
   '(contract (object-contract (m (case-> (boolean? . -> . boolean?)
                                          (integer? integer? . -> . integer?))))
              (new (class object% (define/public (m x y) x) (super-new)))
              'pos
              'neg))

  (test/spec-passed
   'object-contract-case->4
   '(contract (object-contract (m (case-> (boolean? . -> . boolean?)
                                          (integer? integer? . -> . integer?))))
              (new (class object%
                     (define/public m
                       (case-lambda
                         [(b) (not b)]
                         [(x y) (+ x y)]))
                     (super-new)))
              'pos
              'neg))

  (test/spec-passed/result
   'object-contract-case->5
   '(send (contract (object-contract (m (case-> (boolean? . -> . boolean?)
                                                (integer? integer? . -> . integer?))))
                    (new (class object%
                           (define/public m
                             (case-lambda
                               [(b) (not b)]
                               [(x y) (+ x y)]))
                           (super-new)))
                    'pos
                    'neg)
          m
          #t)
   #f)

  (test/spec-passed/result
   'object-contract-case->6
   '(send (contract (object-contract (m (case-> (boolean? . -> . boolean?)
                                                (integer? integer? . -> . integer?))))
                    (new (class object%
                           (define/public m
                             (case-lambda
                               [(b) (not b)]
                               [(x y) (+ x y)]))
                           (super-new)))
                    'pos
                    'neg)
          m
          3
          4)
   7)

  (test/pos-blame
   'object-contract->*1
   '(contract (object-contract (m (->* (integer?) (symbol? boolean?) number?)))
              (new (class object%
                     (define/public m
                       (lambda (x [y 'a])
                         x))
                     (super-new)))
              'pos
              'neg))

  (test/pos-blame
   'object-contract->*2
   '(contract (object-contract (m (->* (integer?) (symbol? boolean?) number?)))
              (new (class object%
                     (define/public m
                       (lambda (x y [z #t])
                         x))
                     (super-new)))
              'pos
              'neg))

  (test/spec-passed
   'object-contract->*3
   '(contract (object-contract (m (->* (integer?) (symbol? boolean?) number?)))
              (new (class object%
                     (define/public m
                       (lambda (x [y 'a] [z #t])
                         x))
                     (super-new)))
              'pos
              'neg))

  (test/spec-passed/result
   'object-contract->*4
   '(send (contract (object-contract (m (->* (integer?) (symbol? boolean?) number?)))
                    (new (class object%
                           (define/public m
                             (lambda (x [y 'a] [z #t])
                               x))
                           (super-new)))
                    'pos
                    'neg)
          m
          1)
   1)

  (test/spec-passed/result
   'object-contract->*5
   '(send (contract (object-contract (m (->* (integer?) (symbol? boolean?) number?)))
                    (new (class object%
                           (define/public m
                             (lambda (x [y 'a] [z #t])
                               x))
                           (super-new)))
                    'pos
                    'neg)
          m
          2
          'z)
   2)

  (test/spec-passed/result
   'object-contract->*7
   '(send (contract (object-contract (m (->* (integer?) (symbol? boolean?) number?)))
                    (new (class object%
                           (define/public m
                             (lambda (x [y 'a] [z #t])
                               x))
                           (super-new)))
                    'pos
                    'neg)
          m
          3
          'z
          #f)
   3)

  (test/neg-blame
   'object-contract->*8
   '(send (contract (object-contract (m (->* (integer?) (symbol? boolean?) number?)))
                    (new (class object%
                           (define/public m
                             (lambda (x [y 'a] [z #t])
                               x))
                           (super-new)))
                    'pos
                    'neg)
          m
          #f))

  (test/neg-blame
   'object-contract->*9
   '(send (contract (object-contract (m (->* (integer?) (symbol? boolean?) number?)))
                    (new (class object%
                           (define/public m
                             (lambda (x [y 'a] [z #t])
                               x))
                           (super-new)))
                    'pos
                    'neg)
          m
          2
          4))

  (test/neg-blame
   'object-contract->*10
   '(send (contract (object-contract (m (->* (integer?) (symbol? boolean?) number?)))
                    (new (class object%
                           (define/public m
                             (lambda (x [y 'a] [z #t])
                               x))
                           (super-new)))
                    'pos
                    'neg)
          m
          3
          'z
          'y))

  (test/pos-blame
   'object-contract->*11
   '(send (contract (object-contract (m (->* (integer?) (symbol? boolean?) number?)))
                    (new (class object%
                           (define/public m
                             (lambda (x [y 'a] [z #t])
                               'x))
                           (super-new)))
                    'pos
                    'neg)
          m
          3
          'z
          #f))

  (test/spec-passed/result
   'object-contract->*12
   '(let-values ([(x y)
                  (send (contract (object-contract (m (->* (integer?) (symbol? boolean?) (values number? symbol?))))
                                  (new (class object%
                                         (define/public m
                                           (lambda (x [y 'a] [z #t])
                                             (values 1 'x)))
                                         (super-new)))
                                  'pos
                                  'neg)
                        m
                        3
                        'z
                        #f)])
      (cons x y))
   (cons 1 'x))

  (test/pos-blame
   'object-contract->*13
   '(send (contract (object-contract (m (->* (integer?) (symbol? boolean?) (values number? symbol?))))
                    (new (class object%
                           (define/public m
                             (lambda (x [y 'a] [z #t])
                               (values 'x 'x)))
                           (super-new)))
                    'pos
                    'neg)
          m
          3
          'z
          #f))

  (test/pos-blame
   'object-contract->*14
   '(send (contract (object-contract (m (->* (integer?) (symbol? boolean?) (values number? symbol?))))
                    (new (class object%
                           (define/public m
                             (lambda (x [y 'a] [z #t])
                               (values 1 1)))
                           (super-new)))
                    'pos
                    'neg)
          m
          3
          'z
          #f))

  (test/pos-blame
   'object-contract->*1
   '(contract (object-contract (m (-> integer? boolean?)))
              (new (class object% (define/public (m x y) x) (super-new)))
              'pos
              'neg))

  (test/neg-blame
   'object-contract->*2
   '(send (contract (object-contract (m (-> integer? boolean?)))
                    (new (class object% (define/public (m x) x) (super-new)))
                    'pos
                    'neg)
          m #f))

  (test/pos-blame
   'object-contract->*3
   '(send (contract (object-contract (m (-> integer? boolean?)))
                    (new (class object% (define/public (m x) x) (super-new)))
                    'pos
                    'neg)
          m 1))

  (test/spec-passed
   'object-contract->*4
   '(send (contract (object-contract (m (-> integer? boolean?)))
                    (new (class object% (define/public (m x) #f) (super-new)))
                    'pos
                    'neg)
          m 1))

  (test/pos-blame
   'object-contract->*5
   '(contract (object-contract (m (->* (integer?) () #:rest any/c boolean?)))
              (new (class object% (define/public (m x y . z) x) (super-new)))
              'pos
              'neg))

  (test/neg-blame
   'object-contract->*6
   '(send (contract (object-contract (m (->* (integer?) () #:rest any/c boolean?)))
                    (new (class object% (define/public (m x . z) x) (super-new)))
                    'pos
                    'neg)
          m #t))

  (test/pos-blame
   'object-contract->*7
   '(send (contract (object-contract (m (->* (integer?) () #:rest any/c boolean?)))
                    (new (class object% (define/public (m x . z) 1) (super-new)))
                    'pos
                    'neg)
          m 1))

  (test/spec-passed
   'object-contract->*8
   '(send (contract (object-contract (m (->* (integer?) () #:rest any/c boolean?)))
                    (new (class object% (define/public (m x . z) #f) (super-new)))
                    'pos
                    'neg)
          m 1))

  (test/spec-passed
   'object-contract->*9
   '(send (contract (object-contract (m (->* () () #:rest (listof number?) boolean?)))
                    (new (class object% (define/public (m . z) #f) (super-new)))
                    'pos
                    'neg)
          m 1 2 3))

  (test/neg-blame
   'object-contract->*10
   '(send (contract (object-contract (m (->* () () #:rest (listof number?) boolean?)))
                    (new (class object% (define/public (m . z) #f) (super-new)))
                    'pos
                    'neg)
          m
          #t))

  (test/spec-passed
   'object-contract-->d1
   '(send (contract (object-contract (m (->d ([x number?]) () [range (<=/c x)])))
                    (new (class object% (define/public m (lambda (x) (- x 1))) (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/spec-passed
   'object-contract-->d1b
   '(send (contract (object-contract (m (->d ([x number?]) () [range (<=/c x)])))
                    (new (class object% (define/public m (lambda (x) (- x 1))) (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/pos-blame
   'object-contract-->d2
   '(send (contract (object-contract (m (->d ([x number?]) () [range (<=/c x)])))
                    (new (class object% (define/public m (lambda (x) (+ x 1))) (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/pos-blame
   'object-contract-->d2b
   '(send (contract (object-contract (m (->d ([x number?]) () [range (<=/c x)])))
                    (new (class object% (define/public m (lambda (x) (+ x 1))) (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/spec-passed
   'object-contract-->d3
   '(send (contract (object-contract (m (->d () () #:rest rst (listof number?) [range any/c])))
                    (new (class object% (define/public m (lambda w 1)) (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/neg-blame
   'object-contract-->d4
   '(send (contract (object-contract (m (->d () () #:rest rst (listof number?) [range any/c])))
                    (new (class object% (define/public m (lambda w 1)) (super-new)))
                    'pos
                    'neg)
          m
          #f))

  (test/spec-passed
   'object-contract-->d5
   '(send (contract (object-contract (m (->d () () any)))
                    (new (class object% (define/public m (lambda () 1)) (super-new)))
                    'pos
                    'neg)
          m))

  (test/spec-passed
   'object-contract-->d6
   '(send (contract (object-contract (m (->d () () (values [x number?] [y (>=/c x)]))))
                    (new (class object% (define/public m (lambda () (values 1 2))) (super-new)))
                    'pos
                    'neg)
          m))

  (test/pos-blame
   'object-contract-->d7
   '(send (contract (object-contract (m (->d () () (values [x number?] [y (>=/c x)]))))
                    (new (class object% (define/public m (lambda () (values 2 1))) (super-new)))
                    'pos
                    'neg)
          m))

  (test/neg-blame
   'object-contract-->d/this-1
   '(send (contract (object-contract (m (->d ([x (and/c integer? (lambda (x) (= x (get-field f this))))])
                                             ()
                                             any)))
                    (new (class object% (field [f 1]) (define/public m (lambda (x) 1)) (super-new)))
                    'pos
                    'neg)
          m
          2))

  (test/spec-passed
   'object-contract-->d/this-2
   '(send (contract (object-contract (m (->d ([x (and/c integer? (lambda (x) (= x (get-field f this))))])
                                             ()
                                             any)))
                    (new (class object% (field [f 1]) (define/public m (lambda (x) 1)) (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/neg-blame
   'object-contract-->d/this-3
   '(send (contract (object-contract (m (->d ([x (and/c integer? (lambda (x) (= x (get-field f this))))])
                                             ()
                                             #:rest rest-var any/c
                                             any)))
                    (new (class object% (field [f 1]) (define/public m (lambda (x . rest) 1)) (super-new)))
                    'pos
                    'neg)
          m
          2))

  (test/spec-passed
   'object-contract-->d/this-4
   '(send (contract (object-contract (m (->d ([x (and/c integer? (lambda (x) (= x (get-field f this))))])
                                             ()
                                             #:rest rest-var any/c
                                             any)))
                    (new (class object% (field [f 1]) (define/public m (lambda (x . rest) 1)) (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/spec-passed
   'object-contract-->pp1
   '(send (contract (object-contract (m (->d ([x number?]) () #:pre-cond #t [unused (<=/c x)] #:post-cond #t)))
                    (new (class object% (define/public m (lambda (x) (- x 1))) (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/spec-passed
   'object-contract-->pp1b
   '(send (contract (object-contract (m (->d ([x number?]) () #:pre-cond #t [unused (<=/c x)] #:post-cond #t)))
                    (new (class object%
                           (define/public m (case-lambda [(x) (- x 1)]
                                                         [(x y) y]))
                           (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/pos-blame
   'object-contract-->pp2
   '(send (contract (object-contract (m (->d ([x number?]) () #:pre-cond #t [unused (<=/c x)] #:post-cond #t)))
                    (new (class object% (define/public m (lambda (x) (+ x 1))) (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/pos-blame
   'object-contract-->pp2b
   '(send (contract (object-contract (m (->d ([x number?]) () #:pre-cond #t [unused (<=/c x)] #:post-cond #t)))
                    (new (class object%
                           (define/public m (case-lambda [(x) (+ x 1)]))
                           (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/spec-passed
   'object-contract-->pp3
   '(send (contract (object-contract (m (->d () () #:rest rst (listof number?) #:pre-cond #t [unused any/c] #:post-cond #t)))
                    (new (class object% (define/public m (lambda w 1)) (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/neg-blame
   'object-contract-->pp4
   '(send (contract (object-contract (m (->d () () #:rest rst (listof number?) #:pre-cond #t [unused any/c] #:post-cond #t)))
                    (new (class object% (define/public m (lambda w 1)) (super-new)))
                    'pos
                    'neg)
          m
          #f))

  (test/spec-passed
   'object-contract-->pp5
   '(send (contract (object-contract (m (->d () () #:pre-cond #t any)))
                    (new (class object% (define/public m (lambda () 1)) (super-new)))
                    'pos
                    'neg)
          m))

  (test/spec-passed
   'object-contract-->pp6
   '(send (contract (object-contract (m (->d () () #:pre-cond #t (values [x number?] [y (>=/c x)]) #:post-cond #t)))
                    (new (class object% (define/public m (lambda () (values 1 2))) (super-new)))
                    'pos
                    'neg)
          m))

  (test/pos-blame
   'object-contract-->pp7
   '(send (contract (object-contract (m (->d () () #:pre-cond #t (values [x number?] [y (>=/c x)]) #:post-cond #t)))
                    (new (class object% (define/public m (lambda () (values 2 1))) (super-new)))
                    'pos
                    'neg)
          m))

  (test/neg-blame
   'object-contract-->pp/this-1
   '(send (contract (object-contract (m (->d ()
                                             ()
                                             #:pre-cond (= 1 (get-field f this))
                                             [result-x any/c]
                                             #:post-cond (= 2 (get-field f this)))))
                    (new (class object% (field [f 2]) (define/public m (lambda () (set! f 3))) (super-new)))
                    'pos
                    'neg)
          m))

  (test/pos-blame
   'object-contract-->pp/this-2
   '(send (contract (object-contract (m (->d () ()
                                             #:pre-cond (= 1 (get-field f this))
                                             [result-x any/c]
                                             #:post-cond (= 2 (get-field f this)))))
                    (new (class object% (field [f 1]) (define/public m (lambda () (set! f 3))) (super-new)))
                    'pos
                    'neg)
          m))

  (test/spec-passed
   'object-contract-->pp/this-3
   '(send (contract (object-contract (m (->d () ()
                                             #:pre-cond (= 1 (get-field f this))
                                              [result-x any/c]
                                              #:post-cond (= 2 (get-field f this)))))
                    (new (class object% (field [f 1]) (define/public m (lambda () (set! f 2))) (super-new)))
                    'pos
                    'neg)
          m))

  (test/neg-blame
   'object-contract-->pp/this-4
   '(send (contract (object-contract (m (->d () ()
                                             #:rest rest-id any/c
                                             #:pre-cond (= 1 (get-field f this))
                                             [result-x any/c]
                                             #:post-cond (= 2 (get-field f this)))))
                    (new (class object% (field [f 2]) (define/public m (lambda args (set! f 3))) (super-new)))
                    'pos
                    'neg)
          m))

  (test/pos-blame
   'object-contract-->pp/this-5
   '(send (contract (object-contract (m (->d () ()
                                             #:rest rest-id any/c
                                             #:pre-cond (= 1 (get-field f this))
                                             [result-x any/c]
                                             #:post-cond (= 2 (get-field f this)))))
                    (new (class object% (field [f 1]) (define/public m (lambda args (set! f 3))) (super-new)))
                    'pos
                    'neg)
          m))

  (test/spec-passed
   'object-contract-->pp/this-6
   '(send (contract (object-contract (m (->d () ()
                                             #:rest rest-id any/c
                                             #:pre-cond (= 1 (get-field f this))
                                             [result-x any/c]
                                             #:post-cond (= 2 (get-field f this)))))
                    (new (class object% (field [f 1]) (define/public m (lambda args (set! f 2))) (super-new)))
                    'pos
                    'neg)
          m))
#|
  (test/spec-passed
   'object-contract-->i1
   '(send (contract (object-contract (m (->i ([x number?]) () [range (x) (<=/c x)])))
                    (new (class object% (define/public m (lambda (x) (- x 1))) (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/spec-passed
   'object-contract-->i1b
   '(send (contract (object-contract (m (->i ([x number?]) () [range (x) (<=/c x)])))
                    (new (class object% (define/public m (lambda (x) (- x 1))) (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/pos-blame
   'object-contract-->i2
   '(send (contract (object-contract (m (->i ([x number?]) () [range (x) (<=/c x)])))
                    (new (class object% (define/public m (lambda (x) (+ x 1))) (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/pos-blame
   'object-contract-->i2b
   '(send (contract (object-contract (m (->i ([x number?]) () [range (x) (<=/c x)])))
                    (new (class object% (define/public m (lambda (x) (+ x 1))) (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/spec-passed
   'object-contract-->i3
   '(send (contract (object-contract (m (->i () () #:rest [rst (listof number?)] [range any/c])))
                    (new (class object% (define/public m (lambda w 1)) (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/neg-blame
   'object-contract-->i4
   '(send (contract (object-contract (m (->i () () #:rest [rst (listof number?)] [range any/c])))
                    (new (class object% (define/public m (lambda w 1)) (super-new)))
                    'pos
                    'neg)
          m
          #f))

  (test/spec-passed
   'object-contract-->i5
   '(send (contract (object-contract (m (->i () () any)))
                    (new (class object% (define/public m (lambda () 1)) (super-new)))
                    'pos
                    'neg)
          m))

  (test/spec-passed
   'object-contract-->i6
   '(send (contract (object-contract (m (->i () () (values [x number?] [y (x) (>=/c x)]))))
                    (new (class object% (define/public m (lambda () (values 1 2))) (super-new)))
                    'pos
                    'neg)
          m))

  (test/pos-blame
   'object-contract-->i7
   '(send (contract (object-contract (m (->i () () (values [x number?] [y (x) (>=/c x)]))))
                    (new (class object% (define/public m (lambda () (values 2 1))) (super-new)))
                    'pos
                    'neg)
          m))

  (test/neg-blame
   'object-contract-->i/this-1
   '(send (contract (object-contract (m (->i ([x (and/c integer? (lambda (x) (= x (get-field f this))))])
                                             ()
                                             any)))
                    (new (class object% (field [f 1]) (define/public m (lambda (x) 1)) (super-new)))
                    'pos
                    'neg)
          m
          2))

  (test/spec-passed
   'object-contract-->i/this-2
   '(send (contract (object-contract (m (->i ([x (and/c integer? (lambda (x) (= x (get-field f this))))])
                                             ()
                                             any)))
                    (new (class object% (field [f 1]) (define/public m (lambda (x) 1)) (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/neg-blame
   'object-contract-->i/this-3
   '(send (contract (object-contract (m (->i ([x (and/c integer? (lambda (x) (= x (get-field f this))))])
                                             ()
                                             #:rest [rest-var any/c]
                                             any)))
                    (new (class object% (field [f 1]) (define/public m (lambda (x . rest) 1)) (super-new)))
                    'pos
                    'neg)
          m
          2))

  (test/spec-passed
   'object-contract-->i/this-4
   '(send (contract (object-contract (m (->i ([x (and/c integer? (lambda (x) (= x (get-field f this))))])
                                             ()
                                             #:rest [rest-var any/c]
                                             any)))
                    (new (class object% (field [f 1]) (define/public m (lambda (x . rest) 1)) (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/spec-passed
   'object-contract-->i-pp1
   '(send (contract (object-contract (m (->i ([x number?]) () #:pre () #t [unused (x) (<=/c x)] #:post () #t)))
                    (new (class object% (define/public m (lambda (x) (- x 1))) (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/spec-passed
   'object-contract-->i-pp1b
   '(send (contract (object-contract (m (->i ([x number?]) () #:pre () #t [unused (x) (<=/c x)] #:post () #t)))
                    (new (class object%
                           (define/public m (case-lambda [(x) (- x 1)]
                                                         [(x y) y]))
                           (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/pos-blame
   'object-contract-->i-pp2
   '(send (contract (object-contract (m (->i ([x number?]) () #:pre () #t [unused (x) (<=/c x)] #:post () #t)))
                    (new (class object% (define/public m (lambda (x) (+ x 1))) (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/pos-blame
   'object-contract-->i-pp2b
   '(send (contract (object-contract (m (->i ([x number?]) () #:pre () #t [unused (x) (<=/c x)] #:post () #t)))
                    (new (class object%
                           (define/public m (case-lambda [(x) (+ x 1)]))
                           (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/spec-passed
   'object-contract-->i-pp3
   '(send (contract (object-contract (m (->i () () #:rest [rst (listof number?)] #:pre () #t [unused any/c] #:post () #t)))
                    (new (class object% (define/public m (lambda w 1)) (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/neg-blame
   'object-contract-->i-pp4
   '(send (contract (object-contract (m (->i () () #:rest [rst (listof number?)] #:pre () #t [unused any/c] #:post () #t)))
                    (new (class object% (define/public m (lambda w 1)) (super-new)))
                    'pos
                    'neg)
          m
          #f))

  (test/spec-passed
   'object-contract-->i-pp5
   '(send (contract (object-contract (m (->i () () #:pre () #t any)))
                    (new (class object% (define/public m (lambda () 1)) (super-new)))
                    'pos
                    'neg)
          m))

  (test/spec-passed
   'object-contract-->i-pp6
   '(send (contract (object-contract (m (->i () () #:pre () #t (values [x number?] [y (x) (>=/c x)]) #:post () #t)))
                    (new (class object% (define/public m (lambda () (values 1 2))) (super-new)))
                    'pos
                    'neg)
          m))

  (test/pos-blame
   'object-contract-->i-pp7
   '(send (contract (object-contract (m (->i () () #:pre () #t (values [x number?] [y (>=/c x)]) #:post () #t)))
                    (new (class object% (define/public m (lambda () (values 2 1))) (super-new)))
                    'pos
                    'neg)
          m))

  (test/neg-blame
   'object-contract-->i-pp/this-1
   '(send (contract (object-contract (m (->i ()
                                             ()
                                             #:pre () (= 1 (get-field f this))
                                             [result-x any/c]
                                             #:post () (= 2 (get-field f this)))))
                    (new (class object% (field [f 2]) (define/public m (lambda () (set! f 3))) (super-new)))
                    'pos
                    'neg)
          m))

  (test/pos-blame
   'object-contract-->i-pp/this-2
   '(send (contract (object-contract (m (->i () ()
                                             #:pre () (= 1 (get-field f this))
                                             [result-x any/c]
                                             #:post () (= 2 (get-field f this)))))
                    (new (class object% (field [f 1]) (define/public m (lambda () (set! f 3))) (super-new)))
                    'pos
                    'neg)
          m))

  (test/spec-passed
   'object-contract-->i-pp/this-3
   '(send (contract (object-contract (m (->i () ()
                                             #:pre () (= 1 (get-field f this))
                                              [result-x any/c]
                                              #:post () (= 2 (get-field f this)))))
                    (new (class object% (field [f 1]) (define/public m (lambda () (set! f 2))) (super-new)))
                    'pos
                    'neg)
          m))

  (test/neg-blame
   'object-contract-->i-pp/this-4
   '(send (contract (object-contract (m (->i () ()
                                             #:rest [rest-id any/c]
                                             #:pre () (= 1 (get-field f this))
                                             [result-x any/c]
                                             #:post () (= 2 (get-field f this)))))
                    (new (class object% (field [f 2]) (define/public m (lambda args (set! f 3))) (super-new)))
                    'pos
                    'neg)
          m))

  (test/pos-blame
   'object-contract-->i-pp/this-5
   '(send (contract (object-contract (m (->i () ()
                                             #:rest [rest-id any/c]
                                             #:pre () (= 1 (get-field f this))
                                             [result-x any/c]
                                             #:post () (= 2 (get-field f this)))))
                    (new (class object% (field [f 1]) (define/public m (lambda args (set! f 3))) (super-new)))
                    'pos
                    'neg)
          m))

  (test/spec-passed
   'object-contract-->i-pp/this-6
   '(send (contract (object-contract (m (->i () ()
                                             #:rest [rest-id any/c]
                                             #:pre () (= 1 (get-field f this))
                                             [result-x any/c]
                                             #:post () (= 2 (get-field f this)))))
                    (new (class object% (field [f 1]) (define/public m (lambda args (set! f 2))) (super-new)))
                    'pos
                    'neg)
          m))
 |#

  (test/spec-passed/result
   'object-contract-drop-method1
   '(send (contract (object-contract (m (-> integer? integer?)))
                    (new (class object% (define/public (m x) x) (define/public (n x) x) (super-new)))
                    'pos
                    'neg)
          n 1)
   1)

  (test/spec-passed/result
   'object-contract-drop-method2
   '(let ([o (contract (object-contract (m (-> integer? integer?)))
                       (new (class object% (define/public (m x) x) (define/public (n x) x) (super-new)))
                       'pos
                       'neg)])
      (with-method ([m (o m)]
                    [n (o n)])
        (list (m 1) (n 2))))
   '(1 2))

  (test/spec-passed/result
   'object-contract-drop-field1
   '(get-field g (contract (object-contract (field f integer?))
                           (new (class object% (field [f 1] [g 2]) (super-new)))
                           'pos
                           'neg))
   2)

  (test/spec-passed/result
   'object-contract-drop-field2
   '(field-bound? g (contract (object-contract (field f integer?))
                              (new (class object% (field [f 1] [g 2]) (super-new)))
                              'pos
                              'neg))
   #t)

  (test/spec-passed/result
   'object-contract-drop-field3
   '(field-names
     (contract (object-contract)
               (new (class object% (field [g 2]) (super-new)))
               'pos
               'neg))
   '(g))


  (test/spec-passed/result
   'object-contract-ho-method1
   '(send (contract (object-contract (m (-> (-> integer? integer?) integer?)))
                    (new (class object% (define/public (m f) (f 1)) (super-new)))
                    'pos
                    'neg)
          m
          (λ (x) x))
   1)

  (test/spec-passed/result
   'object-contract-ho-method2
   '(send (contract (object-contract (m (-> (->* (integer?) () integer?) integer?)))
                    (new (class object% (define/public (m f) (f 1)) (super-new)))
                    'pos
                    'neg)
          m
          (λ (x) x))
   1)

  (test/spec-passed/result
   'object-contract-ho-method3
   '(send (contract (object-contract (m (-> (->i ([x integer?]) () [r integer?]) integer?)))
                    (new (class object% (define/public (m f) (f 1)) (super-new)))
                    'pos
                    'neg)
          m
          (λ (x) x))
   1)

  (test/spec-passed/result
   'object-contract-layered1
   '(send (contract (object-contract (m (-> number? number?)))
                    (contract (object-contract)
                              (new (class object% (super-new) (define/public (m x) x)))
                              'pos
                              'neg)
                    'pos
                    'neg)
          m
          5)
   5)

  ;; Make sure we're not dropping projections on the floor.
  (test/neg-blame
   'object-contract-layered2
   '(send (contract (object-contract (m (-> number? number?)))
                    (contract (object-contract (m (-> string? string?)))
                              (new (class object% (super-new) (define/public (m x) x)))
                              'pos
                              'neg)
                    'pos
                    'neg)
          m
          5))

  (test/spec-passed/result
   'object-contract/arrow-special-case1
   '(send (contract (object-contract
		     [m (-> any/c boolean?)])
		    (new (class object%
				(define/public (m x) #t)
				(super-new)))
		    'pos
		    'neg)
	  m 1)
   #t)

  (test/spec-passed/result
   'object-contract/arrow-special-case2
   '(send (contract (object-contract
		     [m (-> any/c any)])
		    (new (class object%
				(define/public (m x) #t)
				(super-new)))
		    'pos
		    'neg)
	  m 1)
   #t)



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; test error message has right format
  ;;

  (test/spec-passed/result
   'wrong-method-arity-error-message
   '(with-handlers ([exn:fail? exn-message])
      (send (contract (object-contract [m (integer? . -> . integer?)])
                      (new (class object% (define/public (m x) x) (super-new)))
                      'pos
                      'neg)
            m
            1
            2))
   (string-append
    "m method: arity mismatch;\n"
    " the expected number of arguments does not match the given number\n"
    "  expected: 1\n"
    "  given: 2\n"
    "  arguments...:\n"
    "   1\n"
    "   2"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; tests object utilities to be sure wrappers work right
  ;;

  (let* ([o1 (contract-eval '(new object%))]
         [o2 (contract-eval `(contract (object-contract) ,o1 'pos 'neg))])
    (test #t (contract-eval 'object=?) o1 o1)
    (test #f (contract-eval 'object=?) o1 (contract-eval '(new object%)))
    (test #t (contract-eval 'object=?) o1 o2)
    (test #t (contract-eval 'object=?) o2 o1)
    (test #f (contract-eval 'object=?) (contract-eval '(new object%)) o2))

  (ctest #t
         method-in-interface?
         'm
         (object-interface
          (contract
           (object-contract (m (integer? . -> . integer?)))
           (new (class object% (define/public (m x) x) (super-new)))
           'pos
           'neg)))

  (let* ([i<%> (contract-eval '(interface ()))]
         [c% (contract-eval `(class* object% (,i<%>) (super-new)))]
         [o (contract-eval `(new ,c%))])
    (test #t (contract-eval 'is-a?) o i<%>)
    (test #t (contract-eval 'is-a?) o c%)
    (test #t (contract-eval 'is-a?) (contract-eval `(contract (object-contract) ,o 'pos 'neg)) i<%>)
    (test #t (contract-eval 'is-a?) (contract-eval `(contract (object-contract) ,o 'pos 'neg)) c%))

  ;; Currently the new object contracts using impersonators don't even attempt to ensure that
  ;; these reflective operations still work, and I'm not even sure they should.  For now, I
  ;; just get the class info from the original object, which means that all contracts are evaded.
  ;;
  ;; Just as a note, if we move the class-insp-mk values forward in class/c-proj and make-wrapper-class,
  ;; we get a failure in object->vector for the second testcase because the field-ref/field-set! in the
  ;; contracted version of the class (for a struct subtype of the original class's struct type) doesn't
  ;; know how to get the fields out of the object struct. We can always force it with unsafe-struct-ref,
  ;; but if we had impersonate-struct-type, with the same ability to replace the prop:object as
  ;; impersonate-struct has, then we might be able to handle this better.
  (let ([c% (parameterize ([current-inspector (make-inspector)])
              (contract-eval '(class object% (super-new))))])
    (test (list c% #f)
          'object-info
          (contract-eval
           `(call-with-values
             (lambda () (object-info (contract (object-contract) (new ,c%) 'pos 'neg)))
             list))))

  ;; object->vector tests
  (let* ([obj
          (parameterize ([current-inspector (make-inspector)])
            (contract-eval '(new (class object% (field [x 1] [y 2]) (super-new)))))]
         [vec (contract-eval `(object->vector ,obj))])
    (test vec
          (contract-eval 'object->vector)
          (contract-eval
           `(contract (object-contract (field x integer?) (field y integer?))
                      ,obj
                      'pos
                      'neg))))


;
;
;            ;;                              ;;
;            ;;                              ;;
;            ;;                              ;;
;     ;;;;   ;;   ;;;;;    ;;;;;    ;;;;;   ;;    ;;;;
;    ;;;;;;  ;;  ;;;;;;;  ;;;;;;;  ;;;;;;;  ;;   ;;;;;;
;   ;;;  ;;  ;;  ;;   ;;  ;;   ;;  ;;   ;;  ;;  ;;;  ;;
;   ;;       ;;     ;;;;  ;;;;     ;;;;     ;;  ;;
;   ;;       ;;   ;;;;;;   ;;;;;    ;;;;;   ;;  ;;
;   ;;       ;;  ;;;  ;;     ;;;;     ;;;;  ;;  ;;
;   ;;;  ;;  ;;  ;;   ;;  ;;   ;;  ;;   ;; ;;   ;;;  ;;
;    ;;;;;   ;;  ;;;;;;;  ;;;;;;;  ;;;;;;; ;;    ;;;;;
;     ;;;    ;;   ;;;; ;;  ;;;;;    ;;;;;  ;;     ;;;
;
;
;

  (test/pos-blame
   'class/c-first-order-class-1
   '(contract (class/c)
              3
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-class-2
   '(contract (class/c)
              object%
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-opaque-class-1
   '(contract (class/c #:opaque)
              object%
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-opaque-class-2
   '(contract (class/c #:opaque)
              (class object% (super-new) (define/public (m x) (add1 x)))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-method-1
   '(contract (class/c [m (-> any/c number? number?)])
              object%
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-method-2
   '(contract (class/c [m (-> any/c number? number?)])
              (class object% (super-new) (define/public (m x) (add1 x)))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-method-3
   '(contract (class/c [m (-> any/c number? number?)])
              (class object% (super-new) (define/public (m) 3))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-method-4
   '(contract (class/c m)
              object%
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-method-5
   '(contract (class/c m)
              (class object% (super-new) (define/public (m) 3))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-method-6
   '(contract (class/c [m (-> any/c number? number?)])
              (class object% (super-new) (define/public (m) 3))
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-local-method-1
   '(let ()
     (define-local-member-name m)
     (contract (class/c [m (->m number? number?)])
               (class object% (super-new) (define/public (m x) 3))
               'pos
               'neg)))

  (test/pos-blame
   'class/c-first-order-local-method-2
   '(let ()
     (define-local-member-name m)
     (contract (class/c [m (->m number? number? number?)])
               (class object% (super-new) (define/public (m x) 3))
               'pos
               'neg)))

  (test/pos-blame
   'class/c-first-order-local-method-3
   '(let ()
     (define-local-member-name m)
     (contract (class/c [m (->m number? number? number?)])
               (class object% (super-new))
               'pos
               'neg)))

  (test/spec-passed
   'class/c-first-order-opaque-method-1
   '(contract (class/c #:opaque [m (-> any/c number? number?)])
              (class object% (super-new) (define/public (m x) 3))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-opaque-method-2
   '(contract (class/c #:opaque [m (-> any/c number? number?)])
              (class object% (super-new) (define/public (m x) 3) (define/public (n) 4))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-opaque-method-3
   '(let ()
      (define-local-member-name n)
      (contract (class/c #:opaque [m (-> any/c number? number?)])
                (class object% (super-new) (define/public (m x) 3) (define/public (n) 4))
                'pos
                'neg)))

  (test/pos-blame
   'class/c-first-order-opaque-method-4
   '(contract
      (class/c #:opaque [m (-> any/c number? number?)])
      (let ()
        (define-local-member-name n)
        (class object% (super-new) (define/public (m x) 3) (define/public (n) 4)))
      'pos
      'neg))

  (test/pos-blame
   'class/c-first-order-opaque-method-5
   '(contract
      (class/c #:opaque [m (-> any/c number? number?)] [n (-> any/c number?)])
      (let ()
        (define-local-member-name n)
        (class object% (super-new) (define/public (m x) 3) (define/public (n) 4)))
      'pos
      'neg))

  (test/spec-passed
   'class/c-first-order-opaque-method-6
   '(let ()
      (define-local-member-name n)
      (contract (class/c #:opaque [m (-> any/c number? number?)] [n (-> any/c number?)])
                (class object% (super-new) (define/public (m x) 3) (define/public (n) 4))
                'pos
                'neg)))

  (test/pos-blame
   'class/c-first-order-field-1
   '(contract (class/c (field [n number?]))
              object%
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-field-2
   '(contract (class/c (field [n number?]))
              (class object% (super-new) (field [n 3]))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-field-3
   '(contract (class/c (field n))
              object%
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-field-4
   '(contract (class/c (field n))
              (class object% (super-new) (field [n 3]))
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-local-field-1
   '(let ()
      (define-local-member-name n)
      (contract (class/c (field n))
                (class object% (super-new) (field [n 3]))
                'pos
                'neg)))

  (test/spec-passed
   'class/c-first-order-local-field-2
   '(let ()
      (define-local-member-name n)
      (contract (class/c (field [n integer?]))
                (class object% (super-new) (field [n 3]))
                'pos
                'neg)))

  (test/pos-blame
   'class/c-first-order-local-field-3
   '(let ()
      (define-local-member-name n)
      (contract (class/c (field [n integer?]))
                (class object% (super-new))
                'pos
                'neg)))

  (test/spec-passed
   'class/c-first-order-opaque-field-1
   '(contract (class/c #:opaque (field n))
              (class object% (super-new) (field [n 3]))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-opaque-field-2
   '(contract (class/c #:opaque (field n))
              (class object% (super-new) (field [m 5] [n 3]))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-opaque-field-2
   '(contract (class/c #:opaque (field [m number?]))
              (let ()
                (define-local-member-name n)
                (class object% (super-new) (field [m 5] [n 3])))
              'pos
              'neg))

  ;; No true first-order tests here, other than just to make
  ;; sure they're accepted.  For init-field, we can at least
  ;; make sure the given field is public (which happens
  ;; naturally by splitting an init-field into init and field).
  (test/spec-passed
   'class/c-first-order-init-1
   '(contract (class/c (init [a number?]))
              (class object% (super-new) (init a))
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-init-field-1
   '(contract (class/c (init-field [a number?]))
              (class object% (super-new) (init-field a))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-init-field-2
   '(contract (class/c (init-field [a number?]))
              object%
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-inherit-field-1
   '(contract (class/c (inherit-field [n number?]))
              object%
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-inherit-field-2
   '(contract (class/c (inherit-field [n number?]))
              (class object% (super-new) (field [n 3]))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-inherit-field-3
   '(contract (class/c (inherit-field f))
              object%
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-inherit-field-4
   '(contract (class/c (inherit-field f))
              (class object% (super-new) (field [f 10]))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-super-1
   '(contract (class/c (super [m (-> any/c number? number?)]))
              object%
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-super-2
   '(contract (class/c (super [m (-> any/c number? number?)]))
              (class object% (super-new) (define/pubment (m x) (add1 x)))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-super-3
   '(contract (class/c (super [m (-> any/c number? number?)]))
              (class object% (super-new) (define/public-final (m x) (add1 x)))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-super-4
   '(contract (class/c (super [m (-> any/c number? number?)]))
              (let ([c% (class object% (super-new) (define/public (m x) (add1 x)))])
                (class c% (super-new) (define/overment (m x) (add1 x))))
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-super-5
   '(contract (class/c (super [m (-> any/c number? number?)]))
              (class object% (super-new) (define/public (m x) (add1 x)))
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-super-6
   '(contract (class/c (super [m (-> any/c number? number?)]))
              (let ([c% (class object% (super-new) (define/pubment (m x) (inner x m x)))])
                (class c% (super-new) (define/augride (m x) (add1 x))))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-super-7
   '(contract (class/c (super m))
              object%
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-super-8
   '(contract (class/c (super m))
              (class object% (super-new) (define/public (m) 3))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-super-9
   '(contract (class/c (super [m (-> any/c number? number?)]))
              (class object% (super-new) (define/public (m) 3))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-opaque-super-1
   '(contract (class/c #:opaque (super m))
              (class (class object% (super-new) (define/public (m) 3)) (super-new))
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-opaque-super-2
   '(contract (class/c #:opaque (super m) m)
              (class (class object% (super-new) (define/public (m) 3)) (super-new))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-opaque-super-3
   '(contract (class/c #:opaque)
              (class (let ()
                       (define-local-member-name m)
                       (class object% (super-new) (define/public (m) 3)))
                     (super-new))
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-opaque-super-4
   '(contract (class/c #:opaque (super m) m)
              (class (class object% (super-new) (define/public (m) 3)) (super-new))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-inner-1
   '(contract (class/c (inner [m (-> any/c number? number?)]))
              object%
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-inner-2
   '(contract (class/c (inner [m (-> any/c number? number?)]))
              (class object% (super-new) (define/pubment (m x) (inner x m x)))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-inner-3
   '(contract (class/c (inner [m (-> any/c number? number?)]))
              (class object% (super-new) (define/public (m x) (add1 x)))
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-inner-4
   '(contract (class/c (inner [m (-> any/c number? number?)]))
              (let ([c% (class object% (super-new) (define/pubment (m x) (inner x m x)))])
                (class c% (super-new) (define/augride (m x) (add1 x))))
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-inner-5
   '(contract (class/c (inner [m (-> any/c number? number?)]))
              (let ([c% (class object% (super-new) (define/public (m x) (add1 x)))])
                (class c% (super-new) (define/overment (m x) (+ (super m x) (inner x m x)))))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-inner-6
   '(contract (class/c (inner [m (-> any/c number? number?)]))
              (let* ([c% (class object% (super-new) (define/pubment (m x) (inner x m x)))]
                     [d% (class c% (super-new) (define/augride (m x) (add1 x)))])
                (class d% (super-new) (define/override-final (m x) (add1 x))))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-inner-7
   '(contract (class/c (inner m))
              object%
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-inner-8
   '(let* ([c% (contract (class/c (inner m))
                         (class object% (super-new) (define/pubment (m) (inner 3 m)))
                         'pos
                         'neg)])
      (class c% (super-new) (define/augment (m) 5))))

  (test/neg-blame
   'class/c-first-order-inner-9
   '(let* ([c% (contract (class/c (inner [m (-> any/c number? number?)]))
                         (class object% (super-new) (define/pubment (m x) (inner x m x)))
                         'pos
                         'neg)])
      (class c% (super-new) (define/augment (m) 5))))

  (test/pos-blame
   'class/c-first-order-opaque-inner-1
   '(contract (class/c #:opaque (inner m))
              (let ([c% (class object% (super-new) (define/pubment (m x) (inner x m x)))])
                (class c% (super-new) (define/augride (m x) (add1 x))))
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-opaque-inner-2
   '(contract (class/c #:opaque (inner m) m)
              (let ([c% (class object% (super-new) (define/pubment (m x) (inner x m x)))])
                (class c% (super-new) (define/augride (m x) (add1 x))))
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-local-inner-1
   '(let ()
      (define-local-member-name m)
      (contract (class/c (inner m) m)
                (let ([c% (class object% (super-new) (define/pubment (m x) (inner x m x)))])
                  (class c% (super-new) (define/augride (m x) (add1 x))))
                'pos
                'neg)))

  (test/pos-blame
   'class/c-first-order-local-inner-2
   '(let ()
      (define-local-member-name m)
      (contract (class/c (inner m))
                object%
                'pos
                'neg)))

  (test/pos-blame
   'class/c-first-order-override-1
   '(contract (class/c (override [m (-> any/c number? number?)]))
              object%
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-override-2
   '(contract (class/c (override [m (-> any/c number? number?)]))
              (class object% (super-new) (define/public (m x) (add1 x)))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-override-3
   '(contract (class/c (override [m (-> any/c number? number?)]))
              (class object% (super-new) (define/pubment (m x) (add1 x)))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-override-4
   '(contract (class/c (override [m (-> any/c number? number?)]))
              (let ([c% (class object% (super-new) (define/public (m x) (add1 x)))])
                (class c% (super-new) (define/overment (m x) (+ (super m x) (inner x m x)))))
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-override-5
   '(contract (class/c (override [m (-> any/c number? number?)]))
              (let ([c% (class object% (super-new) (define/public (m x) (add1 x)))])
                (class c% (super-new) (define/override (m x) (add1 (super m x)))))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-override-6
   '(contract (class/c (override [m (-> any/c number? number?)]))
              (let* ([c% (class object% (super-new) (define/public (m x) (add1 x)))]
                     [d% (class c% (super-new) (define/overment (m x) (+ (super m x) (inner x m x))))])
                (class d% (super-new) (define/augride (m x) x)))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-override-7
   '(contract (class/c (override m))
              object%
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-override-8
   '(let ([c% (contract (class/c (override m))
                        (class object% (super-new) (define/public (m) 3))
                        'pos
                        'neg)])
      (class c% (super-new) (define/override (m) 5))))

  (test/neg-blame
   'class/c-first-order-override-9
   '(let ([c% (contract (class/c (override [m (-> any/c number? number?)]))
                        (class object% (super-new) (define/public (m x) 3))
                        'pos
                        'neg)])
      (class c% (super-new) (define/override (m) 5))))

  (test/pos-blame
   'class/c-first-order-opaque-override-1
   '(contract (class/c #:opaque (override m))
              (let ([c% (class object% (super-new) (define/public (m x) (add1 x)))])
                (class c% (super-new) (define/override (m x) (add1 (super m x)))))
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-opaque-override-2
   '(contract (class/c #:opaque (override m) m)
              (let ([c% (class object% (super-new) (define/public (m x) (add1 x)))])
                (class c% (super-new) (define/override (m x) (add1 (super m x)))))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-local-override-1
   '(let ()
     (define-local-member-name m)
     (contract (class/c (override m))
               object%
               'pos
               'neg)))

  (test/spec-passed
   'class/c-first-order-local-override-2
   '(let ()
      (define-local-member-name m)
      (define c% (contract (class/c (override m))
                           (class object% (super-new) (define/public (m) 3))
                           'pos
                           'neg))
      (class c% (super-new) (define/override (m) 5))))

  (test/pos-blame
   'class/c-first-order-augment-1
   '(contract (class/c (augment [m (-> any/c number? number?)]))
              object%
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-augment-2
   '(contract (class/c (augment [m (-> any/c number? number?)]))
              (class object% (super-new) (define/pubment (m x) (add1 x)))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-augment-3
   '(contract (class/c (augment [m (-> any/c number? number?)]))
              (class object% (super-new) (define/public (m x) (add1 x)))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-augment-4
   '(contract (class/c (augment [m (-> any/c number? number?)]))
              (let ([c% (class object% (super-new) (define/pubment (m x) (inner x m x)))])
                (class c% (super-new) (define/augride (m x) (add1 x))))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-augment-5
   '(contract (class/c (augment [m (-> any/c number? number?)]))
              (let ([c% (class object% (super-new) (define/public (m x) (add1 x)))])
                (class c% (super-new) (define/override (m x) (add1 (super m x)))))
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-augment-6
   '(contract (class/c (augment [m (-> any/c number? number?)]))
              (let* ([c% (class object% (super-new) (define/public (m x) (add1 x)))]
                     [d% (class c% (super-new) (define/overment (m x) (+ (super m x) (inner x m x))))])
                (class d% (super-new) (define/augment (m x) x)))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-augment-7
   '(contract (class/c (augment m))
              object%
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-augment-8
   '(let ([c% (contract (class/c (augment m))
                        (class object% (super-new) (define/pubment (m) 3))
                        'pos
                        'neg)])
      (class c% (super-new) (inherit m))))

  (test/pos-blame
   'class/c-first-order-augment-9
   '(let ([c% (contract (class/c (augment [m (-> any/c number? number?)]))
                        (class object% (super-new) (define/pubment (m) 3))
                        'pos
                        'neg)])
      (class c% (super-new) (inherit m))))

  (test/pos-blame
   'class/c-first-order-opaque-augment-1
   '(contract (class/c #:opaque (augment m))
              (class object% (super-new) (define/pubment (m x) (add1 x)))
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-opaque-augment-2
   '(contract (class/c #:opaque (augment m) m)
              (class object% (super-new) (define/pubment (m x) (add1 x)))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-local-augment-1
   '(let ()
      (define-local-member-name m)
      (contract (class/c (augment m))
                object%
                'pos
                'neg)))

  (test/spec-passed
   'class/c-first-order-local-augment-2
   '(let ()
      (define-local-member-name m)
      (define c% (contract (class/c (augment m))
                           (class object% (super-new) (define/pubment (m) 3))
                           'pos
                           'neg))
      (class c% (super-new) (inherit m))))

  (test/pos-blame
   'class/c-first-order-augride-1
   '(contract (class/c (augride [m (-> any/c number? number?)]))
              object%
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-augride-2
   '(contract (class/c (augride [m (-> any/c number? number?)]))
              (class object% (super-new) (define/pubment (m x) (add1 x)))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-augride-4
   '(contract (class/c (augride [m (-> any/c number? number?)]))
              (class object% (super-new) (define/public (m x) (add1 x)))
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-augride-5
   '(contract (class/c (augride [m (-> any/c number? number?)]))
              (let ([c% (class object% (super-new) (define/pubment (m x) (inner x m x)))])
                (class c% (super-new) (define/augride (m x) (add1 x))))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-augride-5
   '(contract (class/c (augride [m (-> any/c number? number?)]))
              (let ([c% (class object% (super-new) (define/public (m x) (add1 x)))])
                (class c% (super-new) (define/override (m x) (add1 (super m x)))))
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-augride-6
   '(contract (class/c (augride [m (-> any/c number? number?)]))
              (let* ([c% (class object% (super-new) (define/public (m x) (add1 x)))]
                     [d% (class c% (super-new) (define/overment (m x) (+ (super m x) (inner x m x))))])
                (class d% (super-new) (define/augride (m x) x)))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-augride-7
   '(contract (class/c (augride m))
              object%
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-augride-8
   '(let ([c% (contract (class/c (augride m))
                        (class (class object% (super-new) (define/pubment (m) 3))
                          (super-new) (define/augride (m) 4))
                        'pos
                        'neg)])
      (class c% (super-new) (inherit m))))

  (test/pos-blame
   'class/c-first-order-augride-9
   '(let ([c% (contract (class/c (augride [m (-> any/c number? number?)]))
                        (class (class object% (super-new) (define/pubment (m) 3))
                          (super-new) (define/augride (m) 4))
                        'pos
                        'neg)])
      (class c% (super-new) (inherit m))))

  (test/pos-blame
   'class/c-first-order-opaque-augride-1
   '(contract (class/c #:opaque (augride m))
              (let ([c% (class object% (super-new) (define/pubment (m x) (inner x m x)))])
                (class c% (super-new) (define/augride (m x) (add1 x))))
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-opaque-augride-2
   '(contract (class/c #:opaque (augride m) m)
              (let ([c% (class object% (super-new) (define/pubment (m x) (inner x m x)))])
                (class c% (super-new) (define/augride (m x) (add1 x))))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-local-augride-1
   '(let ()
      (define-local-member-name m)
      (contract (class/c (augride m))
                object%
                'pos
                'neg)))

  (test/spec-passed
   'class/c-first-order-local-augride-2
   '(let ()
      (define-local-member-name m)
      (define c% (contract (class/c (augride m))
                           (class (class object% (super-new) (define/pubment (m) 3))
                             (super-new) (define/augride (m) 4))
                           'pos
                           'neg))
      (class c% (super-new) (inherit m))))

  (test/pos-blame
   'class/c-first-order-inherit-1
   '(let* ([c% (contract (class/c (inherit [m (-> any/c number? number?)]))
                         object%
                         'pos
                         'neg)]
           [d% (class c% (super-new) (inherit m) (define/public (f) (m 5)))])
      (send (new d%) f)))

  (test/spec-passed
   'class/c-first-order-inherit-2
   '(let* ([c% (contract (class/c (inherit [m (-> any/c number? number?)]))
                         (class object% (super-new) (define/public (m x) x))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (inherit m) (define/public (f) (m 5)))])
      (send (new d%) f)))

  (test/pos-blame
   'class/c-first-order-inherit-3
   '(let* ([c% (contract (class/c (inherit [m (-> any/c number? number?)]))
                         (class object% (super-new) (define/public (m) 3))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (inherit m) (define/public (f) (m 5)))])
      (send (new d%) f)))

  (test/pos-blame
   'class/c-first-order-opaque-inherit-1
   '(let* ([c% (contract (class/c #:opaque (inherit [m (-> any/c number? number?)]))
                         (class object% (super-new) (define/public (m x) x))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (inherit m) (define/public (f) (m 5)))])
      (send (new d%) f)))

  (test/spec-passed
   'class/c-first-order-opaque-inherit-2
   '(let* ([c% (contract (class/c #:opaque m (inherit [m (-> any/c number? number?)]))
                         (class object% (super-new) (define/public (m x) x))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (inherit m) (define/public (f) (m 5)))])
      (send (new d%) f)))

  (test/pos-blame
   'class/c-first-order-local-inherit-1
   '(let ()
      (define-local-member-name m)
      (define c% (contract (class/c (inherit [m (-> any/c number? number?)]))
                           object%
                           'pos
                           'neg))
      (define d% (class c% (super-new) (inherit m) (define/public (f) (m 5))))
      (send (new d%) f)))

  (test/spec-passed
   'class/c-first-order-local-inherit-2
   '(let ()
      (define-local-member-name m)
      (define c% (contract (class/c (inherit [m (-> any/c number? number?)]))
                           (class object% (super-new) (define/public (m x) x))
                           'pos
                           'neg))
      (define d% (class c% (super-new) (inherit m) (define/public (f) (m 5))))
      (send (new d%) f)))

  (test/spec-passed
   'class/c-first-order-absent-1
   '(contract (class/c (absent m)) object% 'pos 'neg))

  (test/pos-blame
   'class/c-first-order-absent-2
   '(contract (class/c (absent m))
              (class object% (super-new) (define/public (m) 3))
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-absent-3
   '(contract (class/c (absent (field f))) object% 'pos 'neg))

  (test/pos-blame
   'class/c-first-order-absent-4
   '(contract (class/c (absent (field f)))
              (class object% (super-new) (field [f 3]))
              'pos
              'neg))

  (test/spec-passed
   'class/c-first-order-local-absent-1
   '(let ()
      (define-local-member-name f)
      (contract (class/c (absent (field f))) object% 'pos 'neg)))

  (test/pos-blame
   'class/c-first-order-local-absent-2
   '(let ()
      (define-local-member-name f)
      (contract (class/c (absent (field f)))
                (class object% (super-new) (field [f 3]))
                'pos
                'neg)))

  (test/spec-passed
   'class/c-first-order-opaque-absent-1
   '(contract (class/c #:opaque (absent (field f))) object% 'pos 'neg))

  (test/pos-blame
   'class/c-first-order-opaque-absent-2
   '(contract (class/c #:opaque (absent (field f)))
              (class object% (super-new) (field [g 0]))
              'pos
              'neg))

  (test/pos-blame
   'class/c-first-order-opaque-absent-3
   '(contract (class/c #:opaque (absent (field f g)))
              (class object% (super-new) (field [g 0]))
              'pos
              'neg))

  (test/spec-passed
   'class/c-higher-order-init-1
   '(let ([c% (contract (class/c (init [a number?]))
                        (class object% (super-new) (init a))
                        'pos
                        'neg)])
      (new c% [a 3])))

  (test/neg-blame
   'class/c-higher-order-init-2
   '(let ([c% (contract (class/c (init [a number?]))
                        (class object% (super-new) (init a))
                        'pos
                        'neg)])
      (new c% [a #t])))

  (test/spec-passed
   'class/c-higher-order-init-3
   '(let* ([c% (class object% (super-new) (init a))]
           [d% (contract (class/c (init [a number?] [a string?]))
                         (class c% (super-new) (init a))
                         'pos
                         'neg)])
      (new d% [a 3] [a "foo"])))

  (test/neg-blame
   'class/c-higher-order-init-4
   '(let* ([c% (class object% (super-new) (init a))]
           [d% (contract (class/c (init [a number?] [a string?]))
                         (class c% (super-new) (init a))
                         'pos
                         'neg)])
      (new d% [a 3] [a 4])))

  (test/neg-blame
   'class/c-higher-order-init-5
   '(let* ([c% (class object% (super-new) (init a))]
           [d% (contract (class/c (init [a number?] [a string?]))
                         (class c% (super-new) (init a))
                         'pos
                         'neg)])
      (new d% [a "bar"] [a "foo"])))

  (test/spec-passed
   'class/c-higher-order-init-6
   '(let* ([c% (class object% (super-new) (init a))]
           [d% (class c% (super-new) (init a))]
           [d%/c (contract (class/c (init [a integer?] [a string?])) d% 'pos 'neg1)]
           [d%/c/c (contract (class/c (init [a number?])) d%/c 'pos1 'neg)])
      (new d%/c/c [a 3] [a "foo"])))

  (test/neg-blame
   'class/c-higher-order-init-7
   '(let* ([c% (class object% (super-new) (init a))]
           [d% (class c% (super-new) (init a))]
           [d%/c (contract (class/c (init [a integer?] [a string?])) d% 'pos1 'neg)]
           [d%/c/c (contract (class/c (init [a number?])) d%/c 'pos 'neg1)])
      (new d%/c/c [a 3.5] [a "foo"])))

  (test/neg-blame
   'class/c-higher-order-init-8
   '(let* ([c% (class object% (super-new) (init a))]
           [d% (class c% (super-new) (init a))]
           [d%/c (contract (class/c (init [a integer?] [a string?])) d% 'pos 'neg)]
           [d%/c/c (contract (class/c (init [a number?])) d%/c 'pos 'neg)])
      (new d%/c/c [a #t] [a "foo"])))

  (test/spec-passed
   'class/c-higher-order-init-field-1
   '(let ([c% (contract (class/c (init-field [f (-> number? number?)]))
                        (class object% (super-new) (init-field f) (f 3))
                        'pos
                        'neg)])
      (new c% [f (lambda (x) x)])))

  (test/pos-blame
   'class/c-higher-order-init-field-2
   '(let ([c% (contract (class/c (init-field [f (-> number? number?)]))
                        (class object% (super-new) (init-field f) (f #t))
                        'pos
                        'neg)])
      (new c% [f (lambda (x) x)])))

  (test/neg-blame
   'class/c-higher-order-init-field-3
   '(let ([c% (contract (class/c (init-field [f (-> number? number?)]))
                        (class object% (super-new) (init-field f) (f 3))
                        'pos
                        'neg)])
      (new c% [f (lambda (x) (zero? x))])))

  ;; Make sure that the original provider of the value is blamed if an
  ;; init arg is given an invalid value, and then that is retrieved by
  ;; an external client.
  (test/neg-blame
   'class/c-higher-order-init-field-4
   '(let* ([c% (contract (class/c (init-field [f (-> number? number?)]))
                         (class object% (super-new) (init-field f))
                         'pos
                         'neg)]
           [o (new c% [f (lambda (x) (zero? x))])])
      ((get-field f o) 3)))

  (test/spec-passed
   'class/c-higher-order-method-1
   '(let ([c% (contract (class/c [m (-> any/c number? number?)])
                        (class object% (super-new) (define/public (m x) (add1 x)))
                        'pos
                        'neg)])
      (send (new c%) m 3)))

  (test/neg-blame
   'class/c-higher-order-method-2
   '(let ([c% (contract (class/c [m (-> any/c number? number?)])
                        (class object% (super-new) (define/public (m x) (add1 x)))
                        'pos
                        'neg)])
      (send (new c%) m #f)))

  (test/pos-blame
   'class/c-higher-order-method-3
   '(let ([c% (contract (class/c [m (-> any/c number? number?)])
                        (class object% (super-new) (define/public (m x) (zero? x)))
                        'pos
                        'neg)])
      (send (new c%) m 3)))

  ;; Test that public method contracts are not checked for implication.
  ;; Public method contracts do not check behavioral subtyping.
  ;; Once interfaces have contracts, those will.
  (test/spec-passed
   'class/c-higher-order-method-4
   '(let* ([c% (contract (class/c [m (-> any/c number? number?)])
                         (class object% (super-new) (define/public (m x) (zero? x)))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (define/override (m) (super m 5)))])
      (send (new d%) m)))

  (test/spec-passed
   'class/c-higher-order-local-method-1
   '(let ()
      (define-local-member-name m)
      (define c% (contract (class/c [m (-> any/c number? number?)])
                           (class object% (super-new) (define/public (m x) (add1 x)))
                           'pos
                           'neg))
      (send (new c%) m 3)))

  (test/neg-blame
   'class/c-higher-order-local-method-2
   '(let ()
      (define-local-member-name m)
      (define c% (contract (class/c [m (-> any/c number? number?)])
                           (class object% (super-new) (define/public (m x) (add1 x)))
                           'pos
                           'neg))
      (send (new c%) m #f)))

  (test/spec-passed
   'class/c-higher-order-super-1
   '(let* ([c% (contract (class/c [m (-> any/c integer? integer?)]
                                  (super [m (-> any/c number? number?)]))
                         (class object% (super-new) (define/public (m x) (add1 x)))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (define/override (m x) (+ x (super m 3.5))))])
      (send (new d%) m 4.5)))

  (test/neg-blame
   'class/c-higher-order-super-2
   '(let* ([c% (contract (class/c [m (-> any/c integer? integer?)]
                                  (super [m (-> any/c number? number?)]))
                         (class object% (super-new) (define/public (m x) (add1 x)))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (define/override (m) (super m #f)))])
      (send (new d%) m)))

  (test/pos-blame
   'class/c-higher-order-super-3
   '(let* ([c% (contract (class/c [m (-> any/c integer? integer?)]
                                  (super [m (-> any/c number? number?)]))
                         (class object% (super-new) (define/public (m x) (zero? x)))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (define/override (m) (super m 3.5)))])
      (send (new d%) m)))

  (test/spec-passed
   'class/c-higher-order-inner-1
   '(let* ([c% (contract (class/c (inner [m (-> any/c integer? integer?)]))
                         (class object% (super-new) (define/pubment (m x) (+ x (inner x m x))))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (define/augride (m x) (add1 x)))])
      (send (new d%) m 3)))

  (test/neg-blame
   'class/c-higher-order-inner-2
   '(let* ([c% (contract (class/c (inner [m (-> any/c integer? integer?)]))
                         (class object% (super-new) (define/pubment (m x) (+ x (inner x m x))))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (define/augride (m x) (zero? x)))])
      (send (new d%) m 3)))

  (test/pos-blame
   'class/c-higher-order-inner-3
   '(let* ([c% (contract (class/c (inner [m (-> any/c integer? integer?)]))
                         (class object% (super-new) (define/pubment (m x) (+ x (inner x m (zero? x)))))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (define/augride (m x) (add1 x)))])
      (send (new d%) m 3)))

  (test/neg-blame
   'class/c-higher-order-inner-4
   '(let* ([c% (contract (class/c (inner [m (-> any/c integer? integer?)]))
                         (class object% (super-new) (define/pubment (m x) (+ x (inner x m x))))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (define/augride (m x) (add1 x)))]
           [e% (class d% (super-new) (define/override (m x) (zero? (super m x))))])
      (send (new e%) m 3)))

  (test/spec-passed
   'class/c-higher-order-inner-5
   '(let* ([c% (contract (class/c (inner [m (-> any/c integer? integer?)]))
                         (class object% (super-new) (define/pubment (m x) (+ x (inner x m x))))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (define/augment (m x) (if (inner x m x) (add1 x) x)))]
           [e% (class d% (super-new) (define/augride (m x) (zero? x)))])
      (send (new e%) m 3)))

  ;; Make sure the order of the wrapping is correct in the next two.
  (test/neg-blame
   'class/c-higher-order-inner-6
   '(let* ([c% (contract (class/c (inner [m (-> any/c integer? integer?)]))
                         (class object% (super-new) (define/pubment (m x) (+ x (inner x m x))))
                         'pos
                         'neg1)]
           [d% (contract (class/c (inner [m (-> any/c number? number?)]))
                         c%
                         'pos1
                         'neg)]
           [e% (class d% (super-new) (define/augride (m x) (zero? x)))])
      (send (new e%) m 3)))

  (test/pos-blame
   'class/c-higher-order-inner-7
   '(let* ([c% (contract (class/c (inner [m (-> any/c integer? integer?)]))
                         (class object% (super-new) (define/pubment (m x) (+ x (inner x m #f))))
                         'pos
                         'neg1)]
           [d% (contract (class/c (inner [m (-> any/c number? number?)]))
                         c%
                         'pos1
                         'neg)]
           [e% (class d% (super-new) (define/augride (m x) (add1 x)))])
      (send (new e%) m 3)))

  ;; Test that overriding an augmenting method can still be effected by an inner contract.
  (test/neg-blame
   'class/c-higher-order-inner-8
   '(let* ([c% (contract (class/c (inner [m (-> any/c number? number?)]))
                         (class object% (super-new) (define/pubment (m x) (+ x (inner x m 3))))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (define/augride (m x) (add1 x)))]
           [e% (class d% (super-new) (define/override (m x) (zero? (super m x))))])
      (send (new e%) m 3)))

  ;; The inner contract can be added before the next augmenting method, as seen here.
  (test/neg-blame
   'class/c-higher-order-inner-9
   '(let* ([c% (class object% (super-new) (define/pubment (m x) (+ x (inner x m 3))))]
           [d% (contract (class/c (inner [m (-> any/c number? number?)]))
                         (class c% (super-new) (define/augride (m x) (add1 x)))
                         'pos
                         'neg)]
           [e% (class d% (super-new) (define/override (m x) (zero? (super m x))))])
      (send (new e%) m 3)))

  ;; Show both inner and super contracts.
  (test/spec-passed
   'class/c-higher-order-inner-10
   '(let* ([c% (class object% (super-new) (define/pubment (m x) (+ x (inner x m 3))))]
           [d% (contract (class/c (inner [m (-> any/c number? number?)])
                                  (super [m (-> any/c number? number?)]))
                         (class c% (super-new) (define/augride (m x) (add1 x)))
                         'pos
                         'neg)]
           [e% (class d% (super-new) (define/override (m x) (+ x (super m x))))])
      (send (new e%) m 3)))

  (test/pos-blame
   'class/c-higher-order-inner-11
   '(let* ([c% (class object% (super-new) (define/pubment (m x) (+ x (inner x m #f))))]
           [d% (contract (class/c (inner [m (-> any/c number? number?)])
                                  (super [m (-> any/c number? number?)]))
                         (class c% (super-new) (define/augride (m x) (add1 x)))
                         'pos
                         'neg)]
           [e% (class d% (super-new) (define/override (m x) (+ x (super m x))))])
      (send (new e%) m 3)))

  (test/neg-blame
   'class/c-higher-order-inner-10
   '(let* ([c% (class object% (super-new) (define/pubment (m x) (+ x (inner x m 3))))]
           [d% (contract (class/c (inner [m (-> any/c number? number?)])
                                  (super [m (-> any/c number? number?)]))
                         (class c% (super-new) (define/augride (m x) (add1 x)))
                         'pos
                         'neg)]
           [e% (class d% (super-new) (define/override (m x) (+ x (super m #f))))])
      (send (new e%) m 3)))

  (test/spec-passed/result
   'class/c-higher-order-field-1
   '(let* ([c% (contract (class/c (field [f number?]))
                         (class object% (super-new) (field [f 10]))
                         'pos
                         'neg)])
      (get-field f (new c%)))
   10)

  (test/spec-passed/result
   'class/c-higher-order-field-2
   '(let* ([c% (contract (class/c (field [f number?]))
                         (class object% (super-new) (field [f 10]))
                         'pos
                         'neg)]
           [o (new c%)])
      (set-field! f o 5)
      (get-field f o))
   5)

  (test/pos-blame
   'class/c-higher-order-field-3
   '(let* ([c% (contract (class/c (field [f number?]))
                         (class object% (super-new) (field [f #f]))
                         'pos
                         'neg)]
           [o (new c%)])
      (get-field f o)))

  (test/neg-blame
   'class/c-higher-order-field-4
   '(let* ([c% (contract (class/c (field [f number?]))
                         (class object% (super-new) (field [f 10]))
                         'pos
                         'neg)]
           [o (new c%)])
      (set-field! f o #f)))

  (test/spec-passed
   'class/c-higher-order-field-5
   '(let ([c% (contract (class/c (field f))
                        (class object% (super-new) (field [f 10]))
                        'pos
                        'neg)])
      (get-field f (new c%))))

  (test/spec-passed/result
   'class/c-higher-order-inherit-field-1
   '(let* ([c% (contract (class/c (inherit-field [f number?]))
                         (class object% (super-new) (field [f 10]))
                         'pos
                         'neg)]
           [d% (class c% (super-new)
                 (inherit-field f)
                 (define/public (m) f))])
      (send (new d%) m))
   10)

  (test/spec-passed/result
   'class/c-higher-order-inherit-field-2
   '(let* ([c% (contract (class/c (inherit-field [f number?]))
                         (class object% (super-new) (field [f 10]))
                         'pos
                         'neg)]
           [d% (class c% (super-new)
                 (inherit-field f)
                 (define/public (m) (set! f 12)))]
           [o (new d%)])
      (send o m)
      (get-field f o))
   12)

  (test/pos-blame
   'class/c-higher-order-inherit-field-3
   '(let* ([c% (contract (class/c (inherit-field [f number?]))
                         (class object% (super-new) (field [f #f]))
                         'pos
                         'neg)]
           [d% (class c% (super-new)
                 (inherit-field f)
                 (define/public (m) f))])
      (send (new d%) m)))

  (test/neg-blame
   'class/c-higher-order-inherit-field-4
   '(let* ([c% (contract (class/c (inherit-field [f number?]))
                         (class object% (super-new) (field [f 10]))
                         'pos
                         'neg)]
           [d% (class c% (super-new)
                 (inherit-field f)
                 (define/public (m) (set! f #f)))])
      (send (new d%) m)))

  (test/spec-passed
   'class/c-higher-order-inherit-field-5
   '(let* ([c% (contract (class/c (inherit-field f))
                         (class object% (super-new) (field [f 10]))
                         'pos
                         'neg)]
           [d% (class c% (super-new)
                 (inherit-field f)
                 (define/public (m) f))])
      (send (new d%) m)))

  (test/spec-passed
   'class/c-higher-order-override-1
   '(let* ([c% (contract (class/c (override [m (-> any/c integer? integer?)]))
                         (class object% (super-new)
                           (define/public (m x) x)
                           (define/public (f x) (m x)))
                         'pos
                         'neg)]
           [d% (class c% (super-new)
                 (define/public (g x) (m x))
                 (define/override (m x) (add1 (super m x))))])
      (send (new d%) g 3.5)))

  (test/neg-blame
   'class/c-higher-order-override-2
   '(let* ([c% (contract (class/c (override [m (-> any/c number? number?)]))
                         (class object% (super-new)
                           (define/public (m x) x)
                           (define/public (f x) (add1 (m x))))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (define/override (m x) (zero? (super m x))))])
      (send (new d%) f 3)))

  (test/neg-blame
   'class/c-higher-order-override-3
   '(let* ([c% (contract (class/c (override [m (-> any/c number? number?)]))
                         (class object% (super-new)
                           (define/public (m x) (zero? x))
                           (define/public (f x) (add1 (m x))))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (define/override (m x) (super m x)))])
      (send (new d%) f 3)))

  (test/pos-blame
   'class/c-higher-order-override-4
   '(let* ([c% (contract (class/c (override [m (-> any/c number? number?)]))
                         (class object% (super-new)
                           (define/public (m x) x)
                           (define/public (f x) (add1 (m #f))))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (define/override (m x) (+ x (super m x))))])
      (send (new d%) f 3)))

  (test/pos-blame
   'class/c-higher-order-override-5
   '(let* ([c% (contract (class/c (override [m (-> any/c number? number?)]))
                         (class object% (super-new)
                           (define/public (m x) x)
                           (define/public (f x) (add1 (m #f))))
                         'pos
                         'neg1)]
           [d% (contract (class/c (override [m (-> any/c string? string?)]))
                         c%
                         'pos1
                         'neg)]
           [e% (class d% (super-new) (define/override (m x) (+ x (super m x))))])
      (send (new e%) f 3)))

  (test/spec-passed
   'class/c-higher-order-override-6
   '(let* ([c% (contract (class/c (override [m (-> any/c number? number?)]))
                         (class object% (super-new)
                           (define/public (m x) x)
                           (define/public (f x) (add1 (m 3.5))))
                         'pos
                         'neg1)]
           [d% (contract (class/c (override [m (-> any/c integer? integer?)]))
                         (class c% (super-new) (inherit m) (define/public (g x) (add1 (m 3))))
                         'pos1
                         'neg)]
           [e% (class d% (super-new) (define/override (m x) (+ x (super m x))))])
      (send (new e%) g 3)))

  (test/pos-blame
   'class/c-higher-order-override-7
   '(let* ([c% (contract (class/c (override [m (-> any/c number? number?)]))
                         (class object% (super-new)
                           (define/public (m x) x)
                           (define/public (f x) (add1 (m #f))))
                         'pos
                         'neg1)]
           [d% (contract (class/c (override [m (-> any/c integer? integer?)]))
                         (class c% (super-new) (define/public (g x) (add1 (m 3))))
                         'pos1
                         'neg)]
           [e% (class d% (super-new) (define/override (m x) (+ x (super m x))))])
      (send (new e%) f 3)))

  (test/spec-passed
   'class/c-higher-order-augment-1
   '(let* ([c% (contract (class/c (augment [m (-> any/c integer? integer?)]))
                         (class object% (super-new)
                           (define/pubment (m x) x)
                           (define/public (f x) (m (zero? x))))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (inherit m) (define/public (g x) (m x)))])
      (send (new d%) f 3)))

  (test/neg-blame
   'class/c-higher-order-augment-2
   '(let* ([c% (contract (class/c (augment [m (-> any/c integer? integer?)]))
                         (class object% (super-new) (define/pubment (m x) x))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (inherit m) (define/public (g x) (m x)))])
      (send (new d%) g 3.5)))

  (test/pos-blame
   'class/c-higher-order-augment-3
   '(let* ([c% (contract (class/c (augment [m (-> any/c integer? integer?)]))
                         (class object% (super-new) (define/pubment (m x) #f))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (inherit m) (define/public (g x) (m x)))])
      (send (new d%) g 3)))

  (test/pos-blame
   'class/c-higher-order-augment-4
   '(let* ([c% (contract (class/c (augment [m (-> any/c number? integer?)]))
                         (class object% (super-new) (define/pubment (m x) #f))
                         'pos
                         'neg1)]
           [d% (contract (class/c (augment [m (-> any/c integer? number?)]))
                         c%
                         'pos1
                         'neg)]
           [e% (class d% (super-new) (inherit m) (define/public (g x) (m x)))])
      (send (new e%) g 3)))

  (test/neg-blame
   'class/c-higher-order-augment-5
   '(let* ([c% (contract (class/c (augment [m (-> any/c number? integer?)]))
                         (class object% (super-new) (define/pubment (m x) (floor x)))
                         'pos
                         'neg1)]
           [d% (contract (class/c (augment [m (-> any/c integer? number?)]))
                         c%
                         'pos1
                         'neg)]
           [e% (class d% (super-new) (inherit m) (define/public (g x) (m x)))])
      (send (new e%) g 3.5)))

  (test/spec-passed
   'class/c-higher-order-augment-6
   '(let* ([c% (contract (class/c (augment [m (-> any/c number? integer?)]))
                         (class object% (super-new) (define/pubment (m x) (floor x)))
                         'pos
                         'neg1)]
           [d% (contract (class/c (augment [m (-> any/c integer? number?)]))
                         (class c% (super-new) (inherit m) (define/public (f x) (m x)))
                         'pos1
                         'neg)]
           [e% (class d% (super-new) (inherit m) (define/public (g x) (m x)))])
      (send (new e%) f 3.5)))

  (test/spec-passed
   'class/c-higher-order-inherit-1
   '(let* ([c% (contract (class/c (inherit [m (-> any/c number? number?)]))
                         (class object% (super-new) (define/public (m x) x))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (inherit m) (define/public (f) (m 5)))])
      (send (new d%) f)))

  (test/neg-blame
   'class/c-higher-order-inherit-2
   '(let* ([c% (contract (class/c (inherit [m (-> any/c number? number?)]))
                         (class object% (super-new) (define/public (m x) x))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (inherit m) (define/public (f) (m #f)))])
      (send (new d%) f)))

  (test/pos-blame
   'class/c-higher-order-inherit-3
   '(let* ([c% (contract (class/c (inherit [m (-> any/c number? number?)]))
                         (class object% (super-new) (define/public (m x) (zero? x)))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (inherit m) (define/public (f) (m 5)))])
      (send (new d%) f)))

  ;; Should not be checked if overridden (i.e. target of dyn disp changes).
  (test/spec-passed
   'class/c-higher-order-inherit-4
   '(let* ([c% (contract (class/c (inherit [m (-> any/c number? number?)]))
                         (class object% (super-new) (define/public (m x) (zero? x)))
                         'pos
                         'neg)]
           [d% (class c% (super-new) (inherit m) (define/public (f) (m 5)))]
           [e% (class d% (super-new) (define/override (m x) x))])
      (send (new e%) f)))

  (test/spec-passed
   '->m-first-order-1
   '(contract (class/c [m (->m number? number?)])
              (class object% (super-new) (define/public (m x) x))
              'pos
              'neg))

  (test/pos-blame
   '->m-first-order-2
   '(contract (class/c [m (->m any/c number? number?)])
              (class object% (super-new) (define/public (m x) x))
              'pos
              'neg))

  (test/spec-passed
   '->*m-first-order-1
   '(contract (class/c [m (->*m (number?) (string?) number?)])
              (class object% (super-new) (define/public (m x [f "foo"]) x))
              'pos
              'neg))

  (test/pos-blame
   '->*m-first-order-2
   '(contract (class/c [m (->*m (any/c number?) (string?) number?)])
              (class object% (super-new) (define/public (m x [f "foo"]) x))
              'pos
              'neg))

  (test/spec-passed
   '->dm-first-order-1
   '(contract (class/c [pop (->dm () () #:pre-cond (not (send this empty?)) [_ number?])]
                       [push (->dm ([arg number?]) () [_ void?] #:post-cond (not (send this empty?)))]
                       [empty? (->m boolean?)])
              (class object% (super-new)
                (define stack null)
                (define/public (empty?) (null? stack))
                (define/public (push v) (set! stack (cons v stack)))
                (define/public (pop) (let ([res (car stack)]) (set! stack (cdr stack)) res)))
              'pos
              'neg))

  (test/pos-blame
   '->dm-first-order-1
   '(contract (class/c [pop (->dm () () #:pre-cond (not (send this empty?)) [_ number?])]
                       [push (->dm ([arg number?]) () [_ void?] #:post-cond (not (send this empty?)))]
                       [empty? (->m boolean?)])
              (class object% (super-new)
                (define stack null)
                (define/public (empty?) (null? stack))
                (define/public (push v) (set! stack (cons v stack)))
                (define/public (pop v) (let ([res (car stack)]) (set! stack (cdr stack)) res)))
              'pos
              'neg))

  (test/spec-passed
   '->dm-higher-order-1
   '(let* ([stack% (contract (class/c [pop (->dm () () #:pre-cond (not (send this empty?)) [_ number?])]
                                      [push (->dm ([arg number?]) () [_ void?] #:post-cond (not (send this empty?)))]
                                      [empty? (->m boolean?)])
                             (class object% (super-new)
                               (define stack null)
                               (define/public (empty?) (null? stack))
                               (define/public (push v) (set! stack (cons v stack)))
                               (define/public (pop) (let ([res (car stack)]) (set! stack (cdr stack)) res)))
                             'pos
                             'neg)]
           [o (new stack%)])
      (send o push 4)
      (send o empty?)
      (send o pop)))

  (test/pos-blame
   '->dm-higher-order-2
   '(let* ([stack% (contract (class/c [pop (->dm () () #:pre-cond (not (send this empty?)) [_ number?])]
                                      [push (->dm ([arg number?]) () [_ void?] #:post-cond (not (send this empty?)))]
                                      [empty? (->m boolean?)])
                             (class object% (super-new)
                               (define stack null)
                               (define/public (empty?) (null? stack))
                               (define/public (push v) (void))
                               (define/public (pop) (let ([res (car stack)]) (set! stack (cdr stack)) res)))
                             'pos
                             'neg)]
           [o (new stack%)])
      (send o push 4)
      (send o empty?)
      (send o pop)))

  (test/neg-blame
   '->dm-higher-order-3
   '(let* ([stack% (contract (class/c [pop (->dm () () #:pre-cond (not (send this empty?)) [_ number?])]
                                      [push (->dm ([arg number?]) () [_ void?] #:post-cond (not (send this empty?)))]
                                      [empty? (->m boolean?)])
                             (class object% (super-new)
                               (define stack null)
                               (define/public (empty?) (null? stack))
                               (define/public (push v) (set! stack (cons v stack)))
                               (define/public (pop) (let ([res (car stack)]) (set! stack (cdr stack)) res)))
                             'pos
                             'neg)]
           [o (new stack%)])
      (send o pop)))

  (test/spec-passed
   'case->m-first-order-1
   '(contract (class/c [m (case->m (-> number? number?) (-> number? number? number?))])
              (class object% (super-new) (define/public (m x [y 3]) (+ x y)))
              'pos
              'neg))

  (test/pos-blame
   'case->m-first-order-2
   '(contract (class/c [m (case->m (-> number? number?) (-> number? number? number?))])
              (class object% (super-new) (define/public (m x) (+ x y)))
              'pos
              'neg))

  (test/spec-passed
   'case->m-higher-order-1
   '(let ([cls% (contract (class/c [m (case->m (-> number? number?) (-> number? number? number?))])
                          (class object% (super-new) (define/public (m x [y 3]) (+ x y)))
                          'pos
                          'neg)])
      (send (new cls%) m 3)
      (send (new cls%) m 3 4)))

  (test/neg-blame
   'case->m-higher-order-2
   '(let ([cls% (contract (class/c [m (case->m (-> number? number?) (-> number? number? number?))])
                          (class object% (super-new) (define/public (m x [y 3]) (+ x y)))
                          'pos
                          'neg)])
      (send (new cls%) m #t)))

  (test/neg-blame
   'case->m-higher-order-3
   '(let ([cls% (contract (class/c [m (case->m (-> number? number?) (-> number? number? number?))])
                          (class object% (super-new) (define/public (m x [y 3]) (+ x y)))
                          'pos
                          'neg)])
      (send (new cls%) m 3 #t)))

;
;
;             ;;        ;;                     ;    ;;
;             ;;        ;;                    ;;    ;;
;             ;;                              ;;    ;;
;     ;;;;    ;; ;;;    ;;    ;;;      ;;;;  ;;;;; ;;    ;;;;
;    ;;;;;;   ;;;;;;;   ;;   ;;;;;    ;;;;;; ;;;;; ;;   ;;;;;;
;   ;;;  ;;;  ;;;  ;;;  ;;  ;;   ;;  ;;;  ;;  ;;   ;;  ;;;  ;;
;   ;;    ;;  ;;    ;;  ;;  ;;;;;;;  ;;       ;;   ;;  ;;
;   ;;    ;;  ;;    ;;  ;;  ;;;;;;;  ;;       ;;   ;;  ;;
;   ;;    ;;  ;;    ;;  ;;  ;;       ;;       ;;   ;;  ;;
;   ;;;  ;;;  ;;;  ;;;  ;;  ;;;  ;;  ;;;  ;;  ;;  ;;   ;;;  ;;
;    ;;;;;;   ;;;;;;;   ;;   ;;;;;    ;;;;;   ;;;;;;    ;;;;;
;     ;;;;    ;; ;;;    ;;    ;;;      ;;;     ;;;;;     ;;;
;                       ;;
;                     ;;;;
;                     ;;;

  (test/pos-blame
   'object/c-first-order-object-1
   '(contract (object/c)
              3
              'pos
              'neg))

  (test/spec-passed
   'object/c-first-order-object-2
   '(contract (object/c)
              (new object%)
              'pos
              'neg))

  (test/pos-blame
   'object/c-first-order-method-1
   '(contract (object/c [m (-> any/c number? number?)])
              (new object%)
              'pos
              'neg))

  (test/spec-passed
   'object/c-first-order-method-2
   '(contract (object/c [m (-> any/c number? number?)])
              (new (class object% (super-new) (define/public (m x) (add1 x))))
              'pos
              'neg))

  (test/pos-blame
   'object/c-first-order-local-method-1
   '(let ()
      (define-local-member-name m)
      (contract (object/c [m (-> any/c number? number?)])
                (new object%)
                'pos
                'neg)))

  (test/spec-passed
   'object/c-first-order-local-method-2
   '(let ()
      (define-local-member-name m)
      (contract (object/c [m (-> any/c number? number?)])
                (new (class object% (super-new) (define/public (m x) (add1 x))))
                'pos
                'neg)))

  (test/pos-blame
   'object/c-first-order-field-1
   '(contract (object/c (field [n number?]))
              (new object%)
              'pos
              'neg))

  (test/spec-passed
   'object/c-first-order-field-2
   '(contract (object/c (field [n number?]))
              (new (class object% (super-new) (field [n 3])))
              'pos
              'neg))

  (test/pos-blame
   'object/c-first-order-local-field-1
   '(let ()
      (define-local-member-name n)
      (contract (object/c (field [n number?]))
                (new object%)
                'pos
                'neg)))

  (test/spec-passed
   'object/c-first-order-local-field-2
   '(let ()
      (define-local-member-name n)
      (contract (object/c (field [n number?]))
                (new (class object% (super-new) (field [n 3])))
                'pos
                'neg)))

  (test/spec-passed/result
   'object/c-higher-order-field-1
   '(get-field
     n
     (contract (object/c (field [n number?]))
               (new (class object% (super-new) (field [n 3])))
               'pos
               'neg))
   3)

  (test/pos-blame
   'object/c-higher-order-field-2
   '(get-field
     n
     (contract (object/c (field [n number?]))
               (new (class object% (super-new) (field [n #t])))
               'pos
               'neg)))

  (test/spec-passed/result
   'object/c-higher-order-field-3
   '(let ([o (contract (object/c (field [n number?]))
                       (new (class object% (super-new) (field [n 3])))
                       'pos
                       'neg)])
      (set-field! n o 5)
      (get-field n o))
   5)

  (test/neg-blame
   'object/c-higher-order-field-4
   '(let ([o (contract (object/c (field [n number?]))
                       (new (class object% (super-new) (field [n 3])))
                       'pos
                       'neg)])
      (set-field! n o #t)))

  (test/spec-passed/result
   'object/c-higher-order-field-5
   '(let* ([pre-o (new (class object% (super-new) (field [n 3])))]
           [o (contract (object/c (field [n number?]))
                        pre-o
                        'pos
                        'neg)])
      (set-field! n pre-o 5)
      (get-field n o))
   5)

  (test/spec-passed/result
   'object/c-higher-order-field-6
   '(let* ([pre-o (new (class object% (super-new) (field [n 3])))]
           [o (contract (object/c (field [n number?]))
                        pre-o
                        'pos
                        'neg)])
      (set-field! n o 5)
      (get-field n pre-o))
   5)

  (test/neg-blame
   'object/c-higher-order-field-7
   '(let* ([pre-o (new (class object% (super-new) (field [n 3])))]
           [o (contract (object/c (field [n number?]))
                        pre-o
                        'pos
                        'neg)])
      (set-field! n o #t)
      (get-field n pre-o)))

  (test/pos-blame
   'object/c-higher-order-field-8
   '(let* ([pre-o (new (class object% (super-new) (field [n 3])))]
           [o (contract (object/c (field [n number?]))
                        pre-o
                        'pos
                        'neg)])
      (set-field! n pre-o #t)
      (get-field n o)))


;
;
;
;   ;                                                                    ;;;
;   ;                                                                   ;
;                        ;                                              ;
;                        ;                                              ;
;   ;   ;; ;;;    ;;;;  ;;;;;  ;;;;   ;; ;;;     ;;;    ;;;     ;;;    ;;;;
;  ;;    ;;   ;  ;    ;  ;         ;   ;;   ;   ;   ;  ;   ;   ;   ;    ;
;   ;    ;    ;  ;    ;  ;         ;   ;    ;  ;    ; ;     ; ;     ;   ;
;   ;    ;    ;   ;;     ;     ;;;;;   ;    ;  ;      ;;;;;;; ;     ;   ;
;   ;    ;    ;     ;;   ;    ;    ;   ;    ;  ;      ;       ;     ;   ;
;   ;    ;    ;  ;    ;  ;    ;    ;   ;    ;  ;      ;       ;     ;   ;
;   ;    ;    ;  ;    ;  ;    ;   ;;   ;    ;   ;   ;  ;    ;  ;   ;    ;
;  ;;;  ;;;  ;;;  ;;;;    ;;;  ;;; ;; ;;;  ;;;   ;;;    ;;;;    ;;;    ;;;
;
;
;
;

  (test/spec-passed
   'instanceof/c-first-order-1
   '(let* ([c% object%]
           [c%/c (class/c)])
      (contract (instanceof/c c%/c) (new c%) 'pos 'neg)))

  (test/pos-blame
   'instanceof/c-first-order-2
   '(let* ([c% object%]
           [c%/c (class/c (field [f number?]))])
      (contract (instanceof/c c%/c) (new c%) 'pos 'neg)))

  (test/pos-blame
   'instanceof/c-first-order-3
   '(let* ([c% object%]
           [c%/c (class/c [m (->m number? number?)])])
      (contract (instanceof/c c%/c) (new c%) 'pos 'neg)))

  (test/spec-passed
   'instanceof/c-first-order-4
   '(let* ([c% (class object% (super-new) (field [f 3]))]
           [c%/c (class/c (field [f number?]))])
      (contract (instanceof/c c%/c) (new c%) 'pos 'neg)))

  (test/spec-passed
   'instanceof/c-first-order-5
   '(let* ([c% (class object% (super-new) (define/public (m x) x))]
           [c%/c (class/c [m (->m number? number?)])])
      (contract (instanceof/c c%/c) (new c%) 'pos 'neg)))

  (test/spec-passed
   'instanceof/c-first-order-6
   '(let* ([c% (class object% (super-new) (define/public (m x) x))]
           [c%/c (class/c [m (->m number? number?)])]
           [d%/c (class/c [n (->m number? number?)])])
      (contract (instanceof/c (or/c c%/c d%/c)) (new c%) 'pos 'neg)))

  (test/spec-passed
   'instanceof/c-first-order-7
   '(let* ([d% (class object% (super-new) (define/public (n x) x))]
           [c%/c (class/c [m (->m number? number?)])]
           [d%/c (class/c [n (->m number? number?)])])
      (contract (instanceof/c (or/c c%/c d%/c)) (new d%) 'pos 'neg)))

  (test/pos-blame
   'instanceof/c-first-order-8
   '(let* ([e% (class object% (super-new) (define/public (p x) x))]
           [c%/c (class/c [m (->m number? number?)])]
           [d%/c (class/c [n (->m number? number?)])])
      (contract (instanceof/c (or/c c%/c d%/c)) (new e%) 'pos 'neg)))

  (test/spec-passed/result
   'instanceof/c-higher-order-1
   '(let* ([c% (class object% (super-new) (field [f 3]))]
           [c%/c (class/c (field [f number?]))]
           [o (contract (instanceof/c c%/c) (new c%) 'pos 'neg)])
      (get-field f o))
   3)

  (test/neg-blame
   'instanceof/c-higher-order-2
   '(let* ([c% (class object% (super-new) (field [f 3]))]
           [c%/c (class/c (field [f number?]))]
           [o (contract (instanceof/c c%/c) (new c%) 'pos 'neg)])
      (set-field! f o #t)))

  (test/pos-blame
   'instanceof/c-higher-order-3
   '(let* ([c% (class object% (super-new) (define/public (m x) (zero? x)))]
           [c%/c (class/c [m (->m number? number?)])]
           [o (contract (instanceof/c c%/c) (new c%) 'pos 'neg)])
      (send o m 3)))

  (test/spec-passed
   'instanceof/c-higher-order-4
   '(let* ([c% (class object% (super-new) (define/public (m x) x))]
           [c%/c (class/c [m (->m number? number?)])]
           [d%/c (class/c [n (->m number? number?)])]
           [o (contract (instanceof/c (or/c c%/c d%/c)) (new c%) 'pos 'neg)])
      (send o m 3)))

  (test/pos-blame
   'instanceof/c-higher-order-4
   '(let* ([c% (class object% (super-new) (define/public (m x) #t))]
           [c%/c (class/c [m (->m number? number?)])]
           [d%/c (class/c [n (->m number? number?)])]
           [o (contract (instanceof/c (or/c c%/c d%/c)) (new c%) 'pos 'neg)])
      (send o m 3)))

  (test/neg-blame
   'instanceof/c-higher-order-4
   '(let* ([c% (class object% (super-new) (define/public (m x) x))]
           [c%/c (class/c [m (->m number? number?)])]
           [d%/c (class/c [n (->m number? number?)])]
           [o (contract (instanceof/c (or/c c%/c d%/c)) (new c%) 'pos 'neg)])
      (send o m #t)))

;; interface contracts

  (test/spec-passed
    'interface-1
    '(interface () [x number?]))

  (test/spec-passed
    'interface-2
    '(interface () [x number?] [y number?]))

  (test/spec-passed
    'interface-3
    '(interface () [get-x (-> integer?)]))

  (contract-syntax-error-test
    'interface-4
    '(interface () [x number?] [x symbol?]))

  (contract-error-test
   'interface-5
   '(interface () [x (λ (x y) x)])
   exn:fail?)

  (contract-error-test
   'interface-6
   '(interface ((interface () x)) x)
   exn:fail?)

  (test/spec-passed
    'interface-7
    '(interface ((interface () x)) [x integer?]))

  (test/spec-passed
    'interface-8
    '(interface ((interface () [x number?])) [x integer?]))

  (contract-error-test
   'interface-9
    '(interface ((interface () [x number?])) x)
   exn:fail?)

  (test/spec-passed
   'interface-first-order-1
   '(let* ([i<%> (interface () [m (->m number? number?)])]
           [c% (class* object% (i<%>) (super-new) (define/public (m x) x))])
      (new c%)))

  (test/spec-failed
   'interface-first-order-2
   '(let* ([i<%> (interface () [m (->m number? number?)])]
           [c% (class* object% (i<%>) (super-new) (define/public (m) x))])
      (new c%))
   "(class c%)")

  (test/spec-passed
   'interface-higher-order-1
   '(let* ([i<%> (interface () [m (->m number? number?)])]
           [c% (class* object% (i<%>) (super-new) (define/public (m x) x))])
      (send (new c%) m 3)))

  (test/spec-failed
   'interface-higher-order-2
   '(let* ([i<%> (interface () [m (->m number? number?)])]
           [c% (class* object% (i<%>) (super-new) (define/public (m x) x))])
      (send (new c%) m "wrong"))
   "top-level")

  (test/spec-failed
   'interface-higher-order-3
   '(let* ([i<%> (interface () [m (->m number? number?)])]
           [c% (class* object% (i<%>) (super-new) (define/public (m x) "bad"))])
      (send (new c%) m 3))
   "(class c%)")

  (test/spec-failed
   'interface-higher-order-4
   '(let* ([i1<%> (interface () [m (->m number? number?)])]
           [i2<%> (interface (i1<%>) [m (->m integer? integer?)])]
           [c% (class* object% (i2<%>) (super-new) (define/public (m x) x))])
      (send (new c%) m 3.14))
   "(interface i2<%>)")

  (test/spec-failed
   'interface-higher-order-5
   '(let* ([i1<%> (interface () [m (->m number? number?)])]
           [i2<%> (interface (i1<%>) [m (->m integer? integer?)])]
           [c% (class* object% (i2<%>) (super-new) (define/public (m x) 3.14))])
      (send (new c%) m 3))
   "(class c%)")

  (test/spec-failed
   'interface-higher-order-6
   '(let* ([i1<%> (interface () [m (->m integer? integer?)])]
           [i2<%> (interface (i1<%>) [m (->m number? number?)])]
           [c% (class* object% (i2<%>) (super-new) (define/public (m x) 3.14))])
      (send (new c%) m 3))
   "(interface i2<%>)")

  (test/spec-passed
   'interface-higher-order-7
   '(let* ([i1<%> (interface () [m (->m integer? number?)])]
           [i2<%> (interface (i1<%>) [m (->m number? integer?)])]
           [c% (class* object% (i2<%>) (super-new) (define/public (m x) x))])
      (send (new c%) m 3)))

  (test/spec-failed
   'interface-higher-order-8
   '(let* ([i1<%> (interface () [m (->m integer? number?)])]
           [i2<%> (interface (i1<%>) [m (->m number? integer?)])]
           [c% (class* object% (i2<%>) (super-new) (define/public (m x) x))])
      (send (new c%) m 3.14))
   "top-level")

  (test/spec-failed
   'interface-higher-order-9
   '(let* ([i1<%> (interface () [m (->m integer? number?)])]
           [i2<%> (interface (i1<%>) [m (->m number? integer?)])]
           [c% (class* object% (i2<%>) (super-new) (define/public (m x) 3.14))])
      (send (new c%) m 3))
   "(class c%)")

  (test/spec-passed
   'interface-higher-order-10
   '(let* ([i1<%> (interface () [m (->m integer? integer?)])]
           [i2<%> (interface (i1<%>) [m (->m integer? integer?)])]
           [c% (class* object% (i2<%>) (super-new) (define/public (m x) x))]
           [c2% (class c% (super-new))])
      (send (new c2%) m 3)))

  (test/spec-passed
   'interface-higher-order-11
   '(let* ([i1<%> (interface () [m (->m integer? integer?)])]
           [i2<%> (interface (i1<%>) [m (->m integer? integer?)])]
           [c% (contract (class/c)
                         (class* object% (i2<%>) (super-new) (define/public (m x) x))
                         'pos
                         'neg)])
      (send (new c%) m 3)))

  (test/neg-blame
   'interface-higher-order-12
   '(let* ([i1<%> (interface () [m (->m integer? integer?)])]
           [i2<%> (interface (i1<%>) [m (->m integer? integer?)])]
           [c% (contract (class/c [m (->m integer? integer?)])
                         (class* object% (i2<%>) (super-new) (define/public (m x) x))
                         'pos
                         'neg)])
      (send (new c%) m 5.14)))

  (test/spec-failed
   'interface-higher-order-13
   '(let* ([i1<%> (interface () [m (->m integer? integer?)])]
           [i2<%> (interface (i1<%>) [m (->m integer? integer?)])]
           [c% (contract (class/c m)
                         (class* object% (i2<%>) (super-new) (define/public (m x) x))
                         'pos
                         'neg)])
      (send (new c%) m 5.14))
   "pos")

  (test/spec-failed
   'interface-higher-order-14
   '(let* ([i1<%> (interface () [m (->m integer? integer?)])]
           [i2<%> (interface (i1<%>) [m (->m integer? integer?)])]
           [c% (contract (class/c)
                         (class* object% (i2<%>) (super-new) (define/public (m x) x))
                         'pos
                         'neg)])
      (send (new c%) m 5.14))
   "top-level")

  (test/spec-passed
   'interface-internal-name-1
   '(let* ([i1<%> (interface () [m (->m integer? integer?)])]
           [i2<%> (interface (i1<%>) [m (->m integer? integer?)])]
           [c% (class* object% (i2<%>)
                 (super-new)
                 (public [n m])
                 (define n (λ (x) x)))])
      (send (new c%) m 3)))

  (test/spec-passed
   'interface-internal-name-2
   '(let* ([i1<%> (interface () [m (->m integer? integer?)])]
           [i2<%> (interface (i1<%>) [m (->m integer? integer?)])]
           [c% (contract
                 (class/c [m (->m integer? integer?)])
                 (class* object% (i2<%>)
                   (super-new)
                   (public [n m])
                   (define n (λ (x) x)))
                 'pos
                 'neg)])
      (send (new c%) m 3)))

  (test/spec-passed
   'interface-mixin-1
   '(let* ([i1<%> (interface () [m (->m integer? integer?)])]
           [i2<%> (interface (i1<%>) [m (->m integer? integer?)])]
           [mixin (λ (cls)
                    (class* cls (i2<%>)
                      (super-new)
                      (define/public (m x) x)))])
      (send (new (mixin object%)) m 3)))

  (test/spec-passed
   'interface-bad-concretization-1
   '(let* ([i1<%> (interface () [m (->m integer? integer?)])]
           [i2<%> (interface (i1<%>) [m (->m integer? integer?)])]
           [c% (class* object% (i2<%>) (super-new) (define/public (m x) x))]
           [c2% (class c% (super-new))])
      (send (new c2%) m 3)
      (with-contract region
        #:result integer?
        (send (new c2%) m 3))))
  
  (contract-error-test
   'interface-method-name-1
   #'(begin
       (eval '(module imn-bug scheme/base
                (require scheme/class)
                (define i<%> (interface () [m (->m integer? integer?)]))
                (define c% (class* object% (i<%>) (super-new) (define/public (m x) x)))
                (send (new c%) m "foo")))
       (eval '(require 'imn-bug)))
   (λ (x)
     (and (exn:fail:contract:blame? x)
          (regexp-match #rx"m: contract violation" (exn-message x)))))

;
;
;
;    ;;                                           ;           ;;;;      ;;;;
;    ;;                                          ;;           ;;;;      ;;;;
;       ;;;;;;; ;;;;  ;;;;;;; ;;;;  ;;;; ;;;;  ;;;;; ;;;;;;;  ;;;;;;;   ;;;;   ;;;
;  ;;;; ;;;;;;;;;;;;; ;;;;;;;;;;;;; ;;;; ;;;; ;;;;;; ;;;;;;;; ;;;;;;;;  ;;;;  ;;;;;
;  ;;;; ;;;; ;;; ;;;; ;;;; ;;; ;;;; ;;;; ;;;;  ;;;;      ;;;; ;;;;;;;;; ;;;; ;;;; ;;
;  ;;;; ;;;; ;;; ;;;; ;;;; ;;; ;;;; ;;;; ;;;;  ;;;;   ;;;;;;; ;;;; ;;;; ;;;; ;;;;;;;
;  ;;;; ;;;; ;;; ;;;; ;;;; ;;; ;;;; ;;;; ;;;;  ;;;;; ;;  ;;;; ;;;;;;;;; ;;;; ;;;;;
;  ;;;; ;;;; ;;; ;;;; ;;;; ;;; ;;;; ;;;;;;;;;  ;;;;; ;;;;;;;; ;;;;;;;;  ;;;;  ;;;;;;
;  ;;;; ;;;; ;;; ;;;; ;;;; ;;; ;;;;  ;;; ;;;;   ;;;;  ;; ;;;; ;;;;;;;   ;;;;   ;;;;
;
;
;


  (test/pos-blame
   'immutable1
   '(let ([ct (contract (listof (boolean? . -> . boolean?))
                        #f
                        'pos
                        'neg)])
      ((car ct) 1)))

  (test/neg-blame
   'immutable2
   '(let ([ct (contract (listof (boolean? . -> . boolean?))
                        (list (lambda (x) x))
                        'pos
                        'neg)])
      ((car ct) 1)))

  (test/neg-blame
   'immutable3
   '(let ([ct (contract (listof (number? . -> . boolean?))
                        (list (lambda (x) 1))
                        'pos
                        'neg)])
      ((car ct) #f)))

  (test/pos-blame
   'immutable4
   '(let ([ct (contract (listof (number? . -> . boolean?))
                        (list (lambda (x) 1))
                        'pos
                        'neg)])
      ((car ct) 1)))

  (test/spec-passed
   'immutable5
   '(let ([ct (contract (listof (number? . -> . boolean?))
                        (list (lambda (x) #t))
                        'pos
                        'neg)])
      ((car ct) 1)))


  (test/pos-blame
   'immutable6
   '(contract (cons/c (boolean? . -> . boolean?) (boolean? . -> . boolean?))
              #f
              'pos
              'neg))

  (test/spec-passed
   'immutable7
   '(let ([ct (contract (non-empty-listof (boolean? . -> . boolean?))
                        (list (λ (x) #t))
                        'pos
                        'neg)])
      ((car ct) #f)))

  (test/neg-blame
   'immutable8
   '(let ([ct (contract (cons/c (number? . -> . boolean?) (number? . -> . boolean?))
                        (cons (lambda (x) 1) (lambda (x) 1))
                        'pos
                        'neg)])
      ((car ct) #f)))

  (test/neg-blame
   'immutable9
   '(let ([ct (contract (cons/c (number? . -> . boolean?) (number? . -> . boolean?))
                        (cons (lambda (x) 1) (lambda (x) 1))
                        'pos
                        'neg)])
      ((cdr ct) #f)))

  (test/pos-blame
   'immutable10
   '(let ([ct (contract (cons/c (number? . -> . boolean?) (number? . -> . boolean?))
                        (cons (lambda (x) 1) (lambda (x) 1))
                        'pos
                        'neg)])
      ((car ct) 1)))

  (test/pos-blame
   'immutable11
   '(let ([ct (contract (cons/c (number? . -> . boolean?) (number? . -> . boolean?))
                        (cons (lambda (x) 1) (lambda (x) 1))
                        'pos
                        'neg)])
      ((cdr ct) 1)))

  (test/spec-passed
   'immutable12
   '(let ([ct (contract (cons/c (number? . -> . boolean?) (number? . -> . boolean?))
                        (cons (lambda (x) #t) (lambda (x) #t))
                        'pos
                        'neg)])
      ((car ct) 1)))

  (test/spec-passed
   'immutable13
   '(let ([ct (contract (cons/c (number? . -> . boolean?) (number? . -> . boolean?))
                        (cons (lambda (x) #t) (lambda (x) #t))
                        'pos
                        'neg)])
      ((cdr ct) 1)))

  (test/spec-passed/result
   'immutable14
   '(contract (cons/c number? boolean?)
              (cons 1 #t)
              'pos
              'neg)
   (cons 1 #t))

  (test/pos-blame
   'immutable15
   '(contract (list/c (number? . -> . boolean?) (number? . -> . boolean?))
              #f
              'pos
              'neg))

  (test/pos-blame
   'immutable17
   '(contract (list/c (number? . -> . boolean?) (number? . -> . boolean?))
              (list (lambda (x) #t))
              'pos
              'neg))

  (test/pos-blame
   'immutable18
   '(contract (list/c (number? . -> . boolean?) (number? . -> . boolean?))
              (list (lambda (x) #t) (lambda (x) #t) (lambda (x) #t))
              'pos
              'neg))

  (test/spec-passed
   'immutable19
   '(let ([ctc (contract (list/c (number? . -> . boolean?) (number? . -> . boolean?))
                         (list (lambda (x) #t) (lambda (x) #t))
                         'pos
                         'neg)])
      (for-each (lambda (x) (x 1)) ctc)))

  (test/pos-blame
   'vector-immutable1
   '(contract (vector-immutableof (boolean? . -> . boolean?))
              #f
              'pos
              'neg))

  (test/pos-blame
   'vector-immutable2
   '(contract (vector-immutableof (boolean? . -> . boolean?))
              (vector (lambda (x) x))
              'pos
              'neg))

  (test/neg-blame
   'vector-immutable3
   '(let ([ct (contract (vector-immutableof (number? . -> . boolean?))
                        (vector->immutable-vector (vector (lambda (x) 1)))
                        'pos
                        'neg)])
      ((vector-ref ct 0) #f)))

  (test/pos-blame
   'vector-immutable4
   '(let ([ct (contract (vector-immutableof (number? . -> . boolean?))
                        (vector->immutable-vector (vector (lambda (x) 1)))
                        'pos
                        'neg)])
      ((vector-ref ct 0) 1)))

  (test/spec-passed
   'vector-immutable5
   '(let ([ct (contract (vector-immutableof (number? . -> . boolean?))
                        (vector->immutable-vector (vector (lambda (x) #t)))
                        'pos
                        'neg)])
      ((vector-ref ct 0) 1)))

  (test/pos-blame
   'vector-immutable6
   '(contract (vector-immutable/c (number? . -> . boolean?) (number? . -> . boolean?))
              #f
              'pos
              'neg))

  (test/pos-blame
   'vector-immutable7
   '(contract (vector-immutable/c (number? . -> . boolean?) (number? . -> . boolean?))
              (vector (lambda (x) #t) (lambda (x) #t))
              'pos
              'neg))

  (test/pos-blame
   'vector-immutable8
   '(contract (vector-immutable/c (number? . -> . boolean?) (number? . -> . boolean?))
              (vector->immutable-vector (vector (lambda (x) #t)))
              'pos
              'neg))

  (test/pos-blame
   'vector-immutable9
   '(contract (vector-immutable/c (number? . -> . boolean?) (number? . -> . boolean?))
              (vector->immutable-vector (vector (lambda (x) #t) (lambda (x) #t) (lambda (x) #t)))
              'pos
              'neg))

  (test/spec-passed
   'vector-immutable10
   '(let ([ctc (contract (vector-immutable/c (number? . -> . boolean?) (number? . -> . boolean?))
                         (vector->immutable-vector (vector (lambda (x) #t) (lambda (x) #t)))
                         'pos
                         'neg)])
      ((vector-ref ctc 0) 1)
      ((vector-ref ctc 1) 1)))

  (test/spec-passed/result
   'vector-immutable11
   '(contract (vector-immutable/c number? boolean?)
              (vector->immutable-vector (vector 1 #t))
              'pos
              'neg)
   (vector->immutable-vector (vector 1 #t)))

  (test/spec-passed/result
   'vector-immutable12
   '(immutable? (contract (vector-immutable/c number? boolean?)
                          (vector->immutable-vector (vector 1 #t))
                          'pos
                          'neg))
   #t)

  (test/pos-blame
   'box-immutable1
   '(contract (box-immutable/c (number? . -> . boolean?))
              #f
              'pos
              'neg))

  (test/pos-blame
   'box-immutable2
   '(contract (box-immutable/c (number? . -> . boolean?))
              (box (lambda (x) #t))
              'pos
              'neg))

  (test/neg-blame
   'box-immutable3
   '(let ([ctc (contract (box-immutable/c (number? . -> . boolean?))
                         (box-immutable (lambda (x) #t))
                         'pos
                         'neg)])
      ((unbox ctc) #f)))

  (test/pos-blame
   'box-immutable4
   '(let ([ctc (contract (box-immutable/c (number? . -> . boolean?))
                         (box-immutable (lambda (x) 1))
                         'pos
                         'neg)])
      ((unbox ctc) 1)))

  (test/spec-passed
   'box-immutable5
   '(let ([ctc (contract (box-immutable/c (number? . -> . boolean?))
                         (box-immutable (lambda (x) #t))
                         'pos
                         'neg)])
      ((unbox ctc) 1)))

  (test/spec-passed/result
   'box-immutable6
   '(contract (box-immutable/c boolean?)
              (box-immutable #t)
              'pos
              'neg)
   (box-immutable #t))

  (test/spec-passed/result
   'box-immutable7
   '(immutable? (contract (box-immutable/c boolean?)
                          (box-immutable #t)
                          'pos
                          'neg))
   #t)


;
;
;
;                                             ;;                 ;;
;                                             ;;                 ;;
;  ;;;;;;;   ;;; ;;;   ;;;;   ;;;;;;; ;;;;        ;;;;;   ;;;    ;;  ;;;;;
;  ;;;;;;;;  ;;;;;;;  ;;;;;;  ;;;;;;;;;;;;; ;;;; ;;;;;;  ;;;;;   ;; ;;;;;;
;  ;;;;;;;;; ;;;; ;; ;;;;;;;; ;;;; ;;; ;;;; ;;;; ;;;;   ;;;; ;;  ;;;;;;;;;
;  ;;;; ;;;; ;;;;    ;;;; ;;; ;;;; ;;; ;;;; ;;;;  ;;;;  ;;;;;;; ;; ;;;;
;  ;;;;;;;;; ;;;;    ;;;;;;;; ;;;; ;;; ;;;; ;;;;   ;;;; ;;;;;   ;; ;;;;;;;
;  ;;;;;;;;  ;;;;     ;;;;;;  ;;;; ;;; ;;;; ;;;; ;;;;;;  ;;;;;; ;;  ;;;;;;
;  ;;;;;;;   ;;;;      ;;;;   ;;;; ;;; ;;;; ;;;; ;;;;;    ;;;;  ;;   ;;;;;
;  ;;;;                                                         ;;
;  ;;;;
;


  (test/pos-blame
   'promise/c1
   '(force (contract (promise/c boolean?)
                     (delay 1)
                     'pos
                     'neg)))

  (test/spec-passed
   'promise/c2
   '(force (contract (promise/c boolean?)
                     (delay #t)
                     'pos
                     'neg)))

  (test/spec-passed/result
   'promise/c3
   '(let ([x 0])
      (contract (promise/c any/c)
                (delay (set! x (+ x 1)))
                'pos
                'neg)
      x)
   0)

  (test/spec-passed/result
   'promise/c4
   '(let ([x 0])
      (force (contract (promise/c any/c)
                       (delay (set! x (+ x 1)))
                       'pos
                       'neg))
      x)
   1)

  (test/spec-passed/result
   'promise/c5
   '(let ([x 0])
      (let ([p (contract (promise/c any/c)
                         (delay (set! x (+ x 1)))
                         'pos
                         'neg)])
        (force p)
        (force p))
      x)
   1)

  (test/spec-passed/result
   'promise/c5
   '(let ([a (delay 7)])
      (equal? a
              (contract (promise/c integer?)
                        a
                        'pos
                        'neg)))
   #t)

  (test/spec-passed/result
   'promise/c6
   '(let ([a (delay 7)])
      (equal? a
              (contract (promise/c (new-∃/c 'α))
                        a
                        'pos
                        'neg)))
   #t)


;
;
;
;                                ;                      ;;
;                               ;;                      ;;
;   ;;;;; ;;;  ;;; ;;;; ;;;   ;;;;; ;;;;;;;  ;;;; ;;;;  ;;  ;;;;;
;  ;;;;;; ;;; ;;;; ;;;;;;;;; ;;;;;; ;;;;;;;;  ;;; ;;;   ;; ;;;;;;
;  ;;;;    ;;;;;;  ;;;; ;;;;  ;;;;      ;;;;  ;;;;;;;   ;;;;;;;;;
;   ;;;;   ;;;;;;  ;;;; ;;;;  ;;;;   ;;;;;;;   ;;;;;   ;; ;;;;
;    ;;;;   ;;;;;  ;;;; ;;;;  ;;;;; ;;  ;;;;  ;;;;;;;  ;; ;;;;;;;
;  ;;;;;;   ;;;;   ;;;; ;;;;  ;;;;; ;;;;;;;;  ;;; ;;;  ;;  ;;;;;;
;  ;;;;;    ;;;;   ;;;; ;;;;   ;;;;  ;; ;;;; ;;;; ;;;; ;;   ;;;;;
;          ;;;;                                        ;;
;          ;;;;
;


  (test/pos-blame
   'syntax/c1
   '(contract (syntax/c boolean?)
              #'x
              'pos
              'neg))

  (test/spec-passed
   'syntax/c2
   '(contract (syntax/c symbol?)
              #'x
              'pos
              'neg))

  (test/no-error '(syntax/c (list/c #f)))
  (contract-error-test 'syntax/c-non-flat '(syntax/c (vector/c #f))
                       (λ (x) (regexp-match #rx"expected a flat contract" (exn-message x))))


;
;
;
;             ;                                ;   ;;
;            ;;                               ;;   ;;
;   ;;;;;  ;;;;; ;;; ;;; ;;;; ;;;;   ;;;;;  ;;;;;  ;;  ;;;;;
;  ;;;;;; ;;;;;; ;;;;;;; ;;;; ;;;;  ;;;;;; ;;;;;;  ;; ;;;;;;
;  ;;;;    ;;;;  ;;;; ;; ;;;; ;;;; ;;;;;;;  ;;;;   ;;;;;;;;;
;   ;;;;   ;;;;  ;;;;    ;;;; ;;;; ;;;;     ;;;;  ;; ;;;;
;    ;;;;  ;;;;; ;;;;    ;;;; ;;;; ;;;;;;;  ;;;;; ;; ;;;;;;;
;  ;;;;;;  ;;;;; ;;;;    ;;;;;;;;;  ;;;;;;  ;;;;; ;;  ;;;;;;
;  ;;;;;    ;;;; ;;;;     ;;; ;;;;   ;;;;;   ;;;; ;;   ;;;;;
;                                                 ;;
;
;


  (test/spec-passed
   'struct/c1
   '(let ()
      (define-struct s (a))
      (contract (struct/c s integer?)
                (make-s 1)
                'pos
                'neg)))

  (test/pos-blame
   'struct/c2
   '(let ()
      (define-struct s (a))
      (contract (struct/c s integer?)
                (make-s #f)
                'pos
                'neg)))

  (test/pos-blame
   'struct/c3
   '(let ()
      (define-struct s (a))
      (contract (struct/c s integer?)
                1
                'pos
                'neg)))

  (test/spec-passed
   'struct/c4
   '(let ()
      (define-struct s (a b))
      (contract (struct/c s integer? (struct/c s integer? boolean?))
                (make-s 1 (make-s 2 #t))
                'pos
                'neg)))

  (test/pos-blame
   'struct/c5
   '(let ()
      (define-struct s (a b))
      (contract (struct/c s integer? (struct/c s integer? boolean?))
                (make-s 1 (make-s 2 3))
                'pos
                'neg)))

  (test/spec-passed
   'struct/c6
   '(let ()
      (define-struct s (f))
      (let ([v (contract (struct/c s (-> number? number?))
                         (make-s values)
                         'pos
                         'neg)])
        ((s-f v) 3))))

  (test/neg-blame
   'struct/c7
   '(let ()
      (define-struct s (f))
      (let ([v (contract (struct/c s (-> number? number?))
                         (make-s values)
                         'pos
                         'neg)])
        ((s-f v) #f))))

  (test/pos-blame
   'struct/c8
   '(let ()
      (define-struct s (f))
      (let ([v (contract (struct/c s (-> number? number?))
                         (make-s (λ (v) #f))
                         'pos
                         'neg)])
        ((s-f v) 3))))

  (test/spec-passed
   'struct/c9
   '(let ()
      (define-struct s (a b) #:mutable)
      (let ([v (contract (struct/c s integer? boolean?)
                         (make-s 3 #t)
                         'pos
                         'neg)])
        (set-s-a! v 4)
        (set-s-b! v #t))))

  (test/neg-blame
   'struct/c10
   '(let ()
      (define-struct s (a b) #:mutable)
      (let ([v (contract (struct/c s integer? boolean?)
                         (make-s 3 #t)
                         'pos
                         'neg)])
        (set-s-a! v #f))))

  (test/neg-blame
   'struct/c11
   '(let ()
      (define-struct s (a [b #:mutable]))
      (let ([v (contract (struct/c s integer? boolean?)
                         (make-s 3 #t)
                         'pos
                         'neg)])
        (set-s-b! v 5))))

  (test/spec-passed/result
   'struct/c12
   '(let ()
      (define-struct s (a) #:mutable)
      (define alpha (new-∃/c 'alpha))
      (define v (make-s 3))
      (let ([v* (contract (struct/c s alpha) v 'pos 'neg)])
        (set-s-a! v* (s-a v*)))
      (s-a v))
   3)

  (test/neg-blame
   'struct/c13
   '(let ()
      (define-struct s (a) #:mutable)
      (define alpha (new-∃/c 'alpha))
      (define v (make-s 3))
      (let ([v* (contract (struct/c s alpha) v 'pos 'neg)])
        (set-s-a! v* 4))))
  
  (test/spec-passed/result
   'struct/c14
   '(let ()
      (struct heap (v))
      (struct heap-node heap ())
      
      (heap-v (contract (struct/c heap-node number?) 
                        (heap-node 11)
                        'pos
                        'neg)))
   11)
  
  (test/spec-passed/result
   'struct/c15
   '(let ()
      (struct a (x))
      (struct b a (y))
      (struct c b (z))
      (struct d c (w))
      
      (b-y (contract (struct/c d number? number? number? number?) 
                     (d 11 22 33 44)
                     'pos
                     'neg)))
   22)
  
  (test/spec-passed/result
   'struct/c16
   '(let ()
      (struct doll (contents))
      (list ((flat-contract-predicate (struct/c doll 'center)) (doll 'center))
            ((flat-contract-predicate (struct/c doll 'center)) (doll 'not-center-center))))
   '(#t #f))
  

;
;
;
;
;           ;                         ;    ;    ;;;
;         ;;;                       ;;;    ;    ;;;
;   ;;;;  ;;;; ;;; ;;;; ;;;   ;;;   ;;;;  ;  ;; ;;;   ;;;
;  ;;; ;; ;;;; ;;;;;;;; ;;;  ;;;;;  ;;;;  ; ;;;;;;;  ;;;;;
;  ;;;    ;;;  ;;;  ;;; ;;; ;;;  ;; ;;;   ; ;;; ;;; ;;;  ;;
;   ;;;;  ;;;  ;;;  ;;; ;;; ;;;     ;;;   ; ;;; ;;; ;;;
;     ;;; ;;;  ;;;  ;;; ;;; ;;;  ;; ;;;   ; ;;; ;;; ;;;  ;;
;  ;; ;;; ;;;; ;;;  ;;;;;;;  ;;;;;  ;;;; ;  ;;;;;;;  ;;;;;
;   ;;;;   ;;; ;;;   ;; ;;;   ;;;    ;;; ;   ;; ;;;   ;;;
;
;
;
;


  (test/spec-passed
   'struct/dc-1
   '(let ()
      (struct s (a b))
      (contract (struct/dc s
                         [a () number?]
                         [b (a) boolean?])
              (s 1 #f)
              'pos
              'neg)))

  (test/spec-passed
   'struct/dc-2
   '(let ()
      (struct s (a b))
      (contract (struct/dc s
                           [a () number?]
                           [b (a) (>=/c a)])
                (s 1 2)
                'pos
                'neg)))

  (test/pos-blame
   'struct/dc-3
   '(let ()
      (struct s (a b))
      (contract (struct/dc s
                         [a () number?]
                         [b (a) (>=/c a)])
              (s 2 1)
              'pos
              'neg)))

  (test/spec-passed
   'struct/dc-3b
   '(let ()
      (struct s (a b))
      (contract (struct/dc s
                         [a () number?]
                         [b (a) (<=/c a)])
              (s 2 1)
              'pos
              'neg)))

  (test/spec-passed
   'struct/dc-4
   '(let ()
      (struct s (a b))
      (contract (struct/dc s
                           [a number?]
                           [b (a) (>=/c a)])
                (s 1 2)
                'pos
                'neg)))


  (test/pos-blame
   'struct/dc-5
   '(let ()
      (struct s (a b))
      (s-b (contract (struct/dc s
                                [a () number?]
                                [b (a) (>=/c a)])
                     (s 2 1)
                     'pos
                     'neg))))

  (test/spec-passed/result
   'struct/dc-6
   '(let ()
      (struct s (a b))
      (define-opt/c (f z)
        (struct/dc s
                   [a (>=/c z)]
                   [b (a) #:lazy (f a)]))

      (s-a (contract (f 11)
                     (s 12 (s 13 #f))
                     'pos
                     'neg)))
   12)

  (test/spec-passed/result
   'struct/dc-7
   '(let ()
      (struct s (a b))
      (define-opt/c (f z)
        (struct/dc s
                   [a (>=/c z)]
                   [b (a) #:lazy (f a)]))

      (s-a (s-b (contract (f 11)
                            (s 12 (s 13 #f))
                            'pos
                            'neg))))
   13)


  (test/pos-blame
   'struct/dc-8
   '(let ()
      (struct s (a b))
      (define-opt/c (f z)
        (struct/dc s
                   [a (>=/c z)]
                   [b (a) #:lazy (f a)]))
      (s-b (s-b (contract (f 11)
                          (s 12 (s 13 #f))
                          'pos
                          'neg)))))


  (test/spec-passed/result
   'struct/dc-9
   '(let ()
      (struct s (a b))

      (define-opt/c (g z)
        (struct/dc s
                   [a (>=/c z)]
                   [b (a) #:lazy (>=/c (+ a 1))]))

      (s-a (contract (g 10)
                     (s 12 (s 14 #f))
                     'pos
                     'neg)))
   12)

  (test/spec-passed/result
   'struct/dc-10
   '(let ()
      (struct s (a b))

      (define-opt/c (g z)
        (struct/dc s
                   [a (>=/c z)]
                   [b (a) #:lazy (>=/c (+ a 1))]))

      (s-b (contract (g 10)
                     (s 12 14)
                     'pos
                     'neg)))
   14)

  (test/pos-blame
   'struct/dc-11
   '(let ()

      (struct s (a b))

      (define-opt/c (g z)
        (struct/dc s
                   [a (>=/c z)]
                   [b (a) #:lazy (>=/c (+ a 1))]))

      (s-b (contract (g 11)
                     (s 12 10)
                     'pos
                     'neg))))

  (test/spec-passed/result
   'struct/dc-12
   '(let ()
      (struct kons (hd tl) #:transparent)
      (define (unknown-function a) (=/c a))
      (define-opt/c (f a b)
        (or/c not
              (struct/dc kons
                         [hd (unknown-function a)]
                         [tl () #:lazy (or/c #f (f b a))])))
      (kons-hd (kons-tl (contract (f 1 2)
                                  (kons 1 (kons 2 #f))
                                  'pos
                                  'neg))))
   2)

  (test/spec-passed
   'struct/dc-13
   '(let ()
      (struct s (a))
      (contract (struct/dc s
                           [a #:lazy integer?])
                (s #f)
                'pos
                'neg)))

  (test/spec-passed
   'struct/dc-14
   '(let ()
      (struct s (a))
      (contract (struct/dc s
                           [a #:lazy (-> integer? integer?)])
                (s #f)
                'pos
                'neg)))

  (test/pos-blame
   'struct/dc-15
   '(let ()
      (struct s (a))
      (contract (struct/dc s
                           [a integer?])
                (s #f)
                'pos
                'neg)))

  (test/pos-blame
   'struct/dc-16
   '(let ()
      (struct s (a))
      (contract (struct/dc s
                           [a (-> integer? integer?)])
                (s #f)
                'pos
                'neg)))

  (test/spec-passed
   'struct/dc-17
   '(let ()
      (struct s (q a))
      (contract (struct/dc s
                           [q integer?]
                           [a (q) #:lazy (<=/c q)])
                (s 1 #f)
                'pos
                'neg)))

  (test/pos-blame
   'struct/dc-18
   '(let ()
      (struct s (q a))
      (contract (struct/dc s
                           [q integer?]
                           [a (q) (<=/c q)])
                (s 1 #f)
                'pos
                'neg)))

  (contract-error-test
   'struct/dc-19
   '(let ()
      (struct s (a b))
      (struct/dc s [a (new-∃/c 'α)] [b integer?]))
   exn:fail?)

  (contract-error-test
   'struct/dc-20
   '(let ()
      (struct s (a b))
      (contract (struct/dc s [a (b) (new-∃/c 'α)] [b integer?])
                (s 1 2)
                'pos 'neg))
   exn:fail?)

  (test/pos-blame
   'struct/dc-new1
   '(let ()
      (struct s (a))
      (contract (struct/dc s [a integer?]) (s #f) 'pos 'neg)))

  (test/spec-passed
   'struct/dc-new2
   '(let ()
      (struct s (a))
      (contract (struct/dc s [a #:lazy integer?]) (s #f) 'pos 'neg)))

  (test/pos-blame
   'struct/dc-new3
   '(let ()
      (struct s (a))
      (s-a (contract (struct/dc s [a #:lazy integer?]) (s #f) 'pos 'neg))))

  (test/spec-passed
   'struct/dc-new4
   '(let ()
      (struct s ([a #:mutable]))
      (contract (struct/dc s [a integer?]) (s #f) 'pos 'neg)))

  (test/pos-blame
   'struct/dc-new5
   '(let ()
      (struct s ([a #:mutable]))
      (s-a (contract (struct/dc s [a integer?]) (s #f) 'pos 'neg))))

  (test/neg-blame
   'struct/dc-new6
   '(let ()
      (struct s ([a #:mutable]))
      (set-s-a! (contract (struct/dc s [a integer?]) (s 1) 'pos 'neg)
                #f)))

  (test/spec-passed
   'struct/dc-new7
   '(let ()
      (struct s (a b c))
      (s-c (contract (struct/dc s [a any/c] [b (a) (non-empty-listof real?)] [c (b) (<=/c (car b))])
                     (s 3 '(2) 1)
                     'pos
                     'neg))))


  (test/spec-passed
   'struct/dc-new8
   '(let ()
      (struct s (a b c))
      (s-c (contract (struct/dc s [a any/c] [b (a) (non-empty-listof real?)] [c (a b) (and/c (<=/c a) (<=/c (car b)))])
                     (s 3 '(2) 1)
                     'pos
                     'neg))))

  (test/spec-passed
   'struct/dc-new9
   '(let ()
      (struct s (a b c))
      (s-c (contract (struct/dc s [a any/c] [b (a) (non-empty-listof real?)] [c (b a) (and/c (<=/c a) (<=/c (car b)))])
                     (s 3 '(2) 1)
                     'pos
                     'neg))))


  (test/spec-passed
   'struct/dc-new10
   '(let ()
      (struct s (a b c))
      (s-c (contract (struct/dc s [a (b) (<=/c (car b))] [b (c) (non-empty-listof real?)] [c real?])
                     (s 1 '(2) 3)
                     'pos
                     'neg))))

  (test/spec-passed
   'struct/dc-new11
   '(let ()
      (struct s (a b c))
      (s-c (contract (struct/dc s [a (b c) (and/c (<=/c (car b)) (<=/c c))] [b (c) (non-empty-listof real?)] [c real?])
                     (s 1 '(2) 3)
                     'pos
                     'neg))))

  (test/spec-passed
   'struct/dc-new12
   '(let ()
      (struct s (a b c))
      (s-c (contract (struct/dc s [a (c b) (and/c (<=/c (car b)) (<=/c c))] [b (c) (non-empty-listof real?)] [c real?])
                     (s 1 '(2) 3)
                     'pos
                     'neg))))


  (test/pos-blame
   'struct/dc-new13
   '(let ()
      (struct s (f b))
      (contract (struct/dc s [f (-> integer? integer?)] [b (f) (<=/c (f 1))])
                (s (λ (x) #f) 123)
                'pos
                'neg)))

  (test/spec-failed
   'struct/dc-new14
   '(let ()
        (struct s (f b))
        (contract (struct/dc s [f (-> integer? integer?)] [b (f) (<=/c (f #f))])
                  (s (λ (x) 1) 123)
                  'pos
                  'neg))
   "top-level")

  (test/pos-blame
   'struct/dc-new15
   '(let ()
        (struct s (f b))
        (contract (struct/dc s [f (-> integer? integer?)] [b (f) #:lazy (<=/c (f 1))])
                  (s (λ (x) #f) 123)
                  'pos
                  'neg)))

  (test/spec-failed
   'struct/dc-new16
   '(let ()
      (struct s (f b))
      (contract (struct/dc s [f (-> integer? integer?)] [b (f) #:lazy (<=/c (f #f))])
                (s (λ (x) 1) 123)
                'pos
                'neg))
   "top-level")

  (test/pos-blame
   'struct/dc-new17
   '(let ()
      (struct s (f b))
      (contract (struct/dc s [f #:lazy (-> integer? integer?)] [b (f) #:lazy (<=/c (f 1))])
                (s (λ (x) #f) 123)
                'pos
                'neg)))

  (test/spec-failed
   'struct/dc-new18
   '(let ()
        (struct s (f b))
        (contract (struct/dc s [f #:lazy (-> integer? integer?)] [b (f) #:lazy (<=/c (f #f))])
                  (s (λ (x) 1) 123)
                  'pos
                  'neg))
   "top-level")

  (test/spec-passed
   'struct/dc-new19
   '(let ()
      (struct s (a b c d))
      (contract (struct/dc s
                           [a integer?]
                           [b #:lazy symbol?]
                           [c (a) boolean?]
                           [d (a c) integer?])
                (s 1 'x #t 5)
                'pos 'neg)))

  (test/spec-passed
   'struct/dc-new20
   '(let ()
      (struct s (a [b #:mutable] c [d #:mutable]))
      (contract (struct/dc s
                           [a integer?]
                           [b symbol?]
                           [c (a) boolean?]
                           [d (a c) integer?])
                (s 1 'x #t 5)
                'pos 'neg)))

  (test/spec-passed
   'struct/dc-new21
   '(let ()
      (struct s ([a #:mutable] b))
      (define an-s (contract (struct/dc s [a integer?] [b boolean?])
                             (s 1 #f)
                             'pos 'neg))
      (set-s-a! an-s 2)))

  (test/neg-blame
   'struct/dc-new22
   '(let ()
      (struct s ([a #:mutable] b))
      (define an-s (contract (struct/dc s [a integer?] [b boolean?])
                             (s 1 #f)
                             'pos 'neg))
      (set-s-a! an-s #f)))

  (test/spec-passed
   'struct/dc-new22
   '(let ()
      (struct s ([a #:mutable] b))
      (contract (struct/dc s [a integer?] [b boolean?])
                (s 'one #f)
                'pos 'neg)))

  (test/pos-blame
   'struct/dc-new23
   '(let ()
      (struct s ([a #:mutable] b))
      (s-a (contract (struct/dc s [a integer?] [b boolean?])
                     (s 'one #f)
                     'pos 'neg))))

  (test/pos-blame
   'struct/dc-new24
   '(let ()
      (struct s ([a #:mutable] b))
      (define an-s (contract (struct/dc s [a (-> integer? integer?)] [b boolean?])
                             (s (λ (x) #f) #f)
                             'pos 'neg))
      ((s-a an-s) 1)))

  (test/neg-blame
   'struct/dc-new25
   '(let ()
      (struct s ([a #:mutable] b))
      (define an-s (contract (struct/dc s [a (-> integer? integer?)] [b boolean?])
                             (s (λ (x) #f) #f)
                             'pos 'neg))
      (set-s-a! an-s (λ (x) #f))
      ((s-a an-s) 1)))

  (test/pos-blame
   'struct/dc-new26
   '(let ()
      (struct s ([a #:mutable] b))
      (contract (struct/dc s [a (-> integer? integer?)] [b (a) (<=/c (a 1))])
                (s (λ (x) #f) #f)
                'pos 'neg)))

  (test/pos-blame
   'struct/dc-new27
   '(let ()
      (struct s ([a #:mutable] b))
      (define an-s (contract (struct/dc s [a (-> integer? integer?)] [b (a) (<=/c (a 1))])
                             (s (λ (x) 1) 1)
                             'pos 'neg))
      (set-s-a! an-s (λ (x) -2))
      (s-b an-s)))

  (test/neg-blame
   'struct/dc-new28
   '(let ()
      (struct s ([a #:mutable] b))
      (define an-s (contract (struct/dc s [a (-> integer? integer?)] [b (a) (<=/c (a 1))])
                             (s (λ (x) 1) 1)
                             'pos 'neg))
      (set-s-a! an-s (λ (x) #f))
      (s-b an-s)))

  (test/pos-blame
   'struct/dc-new29
   '(let ()
      (struct s ([a #:mutable] b c))
      (define an-s (contract (struct/dc s
                                        [a (-> integer? integer?)]
                                        [b (a) (<=/c (a 1))]
                                        [c (b) (<=/c b)])
                             (s (λ (x) 1) -11 1)
                             'pos 'neg))
      (set-s-a! an-s (λ (x) -2))
      (s-c an-s)))

  (test/pos-blame
   'struct/dc-new30
   '(let ()
      (struct s ([a #:mutable] b c))
      (define an-s (contract (struct/dc s
                                        [a (-> integer? integer?)]
                                        [b (a) (<=/c (a 1))]
                                        [c (b) (<=/c b)])
                             (s (λ (x) 1) 1 -2)
                             'pos 'neg))
      (set-s-a! an-s (λ (x) -2))
      (s-c an-s)))

  (test/neg-blame
   'struct/dc-new31
   '(let ()
      (struct s ([a #:mutable] [b #:mutable]))
      (define an-s (contract (struct/dc s
                                        [a (-> integer? integer?)]
                                        [b (a) (<=/c (a 1))])
                             (s (λ (x) 1) 1)
                             'pos 'neg))
      (set-s-b! an-s 3)))

  (test/pos-blame
   'struct/dc-new32
   '(let ()
      (struct s ([a #:mutable] [b #:mutable]))
      (define an-s (contract (struct/dc s
                                        [a (-> integer? integer?)]
                                        [b (a) (<=/c (a 1))])
                             (s (λ (x) 1) 1)
                             'pos 'neg))
      (set-s-a! an-s (λ (x) -1))
      (s-b an-s)))

  (test/spec-failed
   'struct/dc-new33
   '(let ()
      (struct s (a [b #:mutable] [c #:mutable]))
      (define an-s (contract (struct/dc s
                                        [a (-> integer? integer?)]
                                        [b any/c]
                                        [c (a b) (<=/c (a b))])
                             (s (λ (x) 1) 1 1)
                             'pos 'neg))
      (set-s-b! an-s #f)
      (s-c an-s))
   "top-level")

  (contract-error-test
   'struct/dc-new-34
   '(let ()
      (struct s ([a #:mutable] [b #:mutable]))
      (contract (struct/dc s
                           [a boolean?]
                           [b (a)
                              #:flat
                              (if a
                                  (<=/c 1)
                                  (-> integer? integer?))])
                (s #f 1)
                'pos
                'neg))
   (λ (x) (regexp-match #rx"struct/dc: .*flat" (exn-message x))))

  (contract-error-test
   'struct/dc-new-35
   '(let ()
      (struct s ([a #:mutable] [b #:mutable]))
      (define an-s (contract (struct/dc s
                                        [a boolean?]
                                        [b (a)
                                           #:flat
                                           (if a
                                               (<=/c 1)
                                               (-> integer? integer?))])
                             (s #t 1)
                             'pos
                             'neg))
      (set-s-a! an-s #f)
      (s-b an-s))
   (λ (x) (regexp-match #rx"struct/dc: .*flat" (exn-message x))))

  (contract-error-test
   'struct/dc-new-36
   '(let ()
      (struct s ([a #:mutable] b))
      (contract (struct/dc s
                           [a boolean?]
                           [b (a)
                              (if a
                                  (<=/c 1)
                                  (new-∃/c 'α))])
                (s #f 1)
                'pos
                'neg))
   (λ (x) (regexp-match #rx"struct/dc: .*chaperone" (exn-message x))))

  (contract-error-test
   'struct/dc-new-37
   '(let ()
      (struct s ([a #:mutable] b))
      (define an-s (contract (struct/dc s
                                        [a boolean?]
                                        [b (a)
                                           (if a
                                               (<=/c 1)
                                               (new-∃/c 'α))])
                             (s #t 1)
                             'pos
                             'neg))
      (set-s-a! an-s #f)
      (s-b an-s))
   (λ (x) (regexp-match #rx"struct/dc: .*chaperone" (exn-message x))))

  (contract-error-test
   'struct/dc-new-38
   '(let ()
      (struct s ([a #:mutable] b [c #:mutable]))
      (define an-s (contract (struct/dc s
                                        [a boolean?]
                                        [b (a)
                                           (if a
                                               (<=/c 1)
                                               (new-∃/c 'α))]
                                        [c (b) integer?])
                             (s #t 1 1)
                             'pos
                             'neg))
      (set-s-a! an-s #f)
      (s-c an-s))
   (λ (x) (regexp-match #rx"struct/dc: .*chaperone" (exn-message x))))

  (test/spec-passed
   'struct/dc-new-39
   '(let ()
      (struct s (a b))
      (contract (struct/dc s [a integer?] [b integer?]) (s 1 2) 'pos 'neg)))

  (test/spec-passed
   'struct/dc-new40
   '(let ()
      (struct s (a b))
      (contract (struct/dc s [a (-> integer? integer?)] [b (-> integer? integer?)])
                (s (λ (x) x) (λ (y) y))
                'pos
                'neg)))

  (test/spec-passed/result
   'struct/dc-new41
   '(let ()
      (struct s (a [b #:mutable]))
      (define α (new-∀/c 'α))
      (s-b ((contract (-> α (struct/dc s [b α]))
                      (λ (x) (s 11 x))
                      'pos
                      'neg) 1)))
   1)

  (test/spec-passed/result
   'struct/dc-new42
   '(let ()
      (struct s (a [b #:mutable]))
      (define α (new-∀/c 'α))
      (s-b ((contract (-> α (struct/dc s [a integer?] [b (a) #:impersonator α]))
                      (λ (x) (s 11 x))
                      'pos
                      'neg) 1)))
   1)

  (test/spec-passed
   'struct/dc-new42
   '(let ()
      (struct s (a [b #:mutable]))
      (contract (struct/dc s [a (-> integer? integer?)] [b (new-∀/c 'α)])
                (s (λ (x) x) 1)
                'pos
                'neg)))
  
  (test/spec-passed/result
   'struct/dc-new43
   '(let ()
      (struct a (x))
      (struct b a (y))
      (struct c b (z))
      (struct d c (w))
      
      (b-y (contract (struct/dc d 
                                [(x #:parent a) boolean?]
                                [(y #:parent b) char?]
                                [(z #:parent c) number?]
                                [w string?])
                     (d #t #\a 3 "x")
                     'pos
                     'neg)))
   #\a)
  
  (test/spec-passed/result
   'struct/dc-new44
   '(let ()
      (struct a (x))
      (struct b a (y))
      (struct c b (z))
      (struct d c (w))
      
      (b-y (contract (struct/dc d 
                                [(x #:parent a) (w) boolean?]
                                [(y #:parent b) ((x #:parent a)) char?]
                                [(z #:parent c) number?]
                                [w string?])
                     (d #t #\a 3 "x")
                     'pos
                     'neg)))
   #\a)

  (test/spec-passed/result
   'struct/dc-pred1
   '(let ()
      (struct s (a b))
      (define p? (flat-contract-predicate (struct/dc s [a number?] [b (a) #:flat (<=/c a)])))
      (list (p? (s 2 1))
            (p? (s 1 2))))
   '(#t #f))
  
  (test/spec-passed/result
   'struct/dc-pred2
   '(let ()
      (struct s (a b c))
      (define p? (flat-contract-predicate (struct/dc s 
                                                     [a number?]
                                                     [b boolean?]
                                                     [c (a b)
                                                        #:flat
                                                        (if (and (= a 1) b)
                                                            any/c
                                                            none/c)])))
      
      (list (p? (s 1 #t 'whatever))
            (p? (s 11 #f 'whatver))))
   '(#t #f))


  
  (contract-error-test
   'struct/dc-imp-nondep-runtime-error
   #'(let ()
       (struct s (ernie bert))
       (struct/dc s [ernie integer?] [bert (new-∀/c 'α)]))
   (λ (x)
     (and (exn:fail? x)
          (regexp-match #rx"expected chaperone" (exn-message x)))))

  (contract-error-test
   'struct/dc-not-a-field
   #'(eval '(let ()
              (struct s (a b))
              (struct/dc s [a integer?] [y integer?])))
   exn:fail:syntax?)

  (contract-error-test
   'struct/dc-circular-dependecies1
   #'(eval '(let ()
              (struct s (a b))
              (struct/dc s [a (a) integer?] [b (a) integer?])))
   exn:fail:syntax?)

  (contract-error-test
   'struct/dc-circular-dependecies2
   #'(eval '(let ()
              (struct s (a b c))
              (struct/dc s [a (b) integer?] [b (a) integer?] [c integer?])))
   exn:fail:syntax?)

  (contract-error-test
   'struct/dc-dep-on-lazy
   #'(eval '(let ()
              (struct s (a b))
              (struct/dc s [a #:lazy integer?] [b (a) integer?])))
   exn:fail:syntax?)

  (contract-error-test
   'struct/dc-lazy-mutable
   #'(eval '(let ()
              (struct s (a [b #:mutable]))
              (struct/dc s [a integer?] [b #:lazy integer?])))
   exn:fail:syntax?)

  (contract-error-test
   'struct/dc-immutable-impersonator
   #'(eval '(let ()
              (struct s (a b))
              (struct/dc s [a integer?] [b (a) #:impersonator (<=/c a)])))
   (λ (x) (and (exn:fail:syntax? x) (regexp-match #rx"immutable" (exn-message x)))))


;
;
;
;                                                     ;;
;                                                     ;;
;  ;;; ;;;   ;;;     ;;;;; ;;;; ;;;; ;;; ;;;  ;;;;;      ;;;  ;;;   ;;;
;  ;;;;;;;  ;;;;;   ;;;;;; ;;;; ;;;; ;;;;;;; ;;;;;; ;;;; ;;;  ;;;  ;;;;;
;  ;;;; ;; ;;;; ;; ;;;;;;; ;;;; ;;;; ;;;; ;; ;;;;   ;;;;  ;;;;;;  ;;;; ;;
;  ;;;;    ;;;;;;; ;;;;    ;;;; ;;;; ;;;;     ;;;;  ;;;;  ;;;;;;  ;;;;;;; ;;;;;
;  ;;;;    ;;;;;   ;;;;;;; ;;;; ;;;; ;;;;      ;;;; ;;;;  ;;;;;;  ;;;;;   ;;;;;
;  ;;;;     ;;;;;;  ;;;;;; ;;;;;;;;; ;;;;    ;;;;;; ;;;;   ;;;;    ;;;;;;
;  ;;;;      ;;;;    ;;;;;  ;;; ;;;; ;;;;    ;;;;;  ;;;;   ;;;;     ;;;;
;
;
;
;
;
;
;                                 ;                               ;
;                                ;;                              ;;
;    ;;;;;   ;;;;   ;;;; ;;;   ;;;;; ;;; ;;; ;;;;;;;    ;;;;;  ;;;;;
;   ;;;;;;  ;;;;;;  ;;;;;;;;; ;;;;;; ;;;;;;; ;;;;;;;;  ;;;;;; ;;;;;;
;  ;;;;;;; ;;;;;;;; ;;;; ;;;;  ;;;;  ;;;; ;;     ;;;; ;;;;;;;  ;;;;
;  ;;;;    ;;;; ;;; ;;;; ;;;;  ;;;;  ;;;;     ;;;;;;; ;;;;     ;;;;
;  ;;;;;;; ;;;;;;;; ;;;; ;;;;  ;;;;; ;;;;    ;;  ;;;; ;;;;;;;  ;;;;;
;   ;;;;;;  ;;;;;;  ;;;; ;;;;  ;;;;; ;;;;    ;;;;;;;;  ;;;;;;  ;;;;;
;    ;;;;;   ;;;;   ;;;; ;;;;   ;;;; ;;;;     ;; ;;;;   ;;;;;   ;;;;
;
;
;


  (test/spec-passed
   'recursive-contract1
   '(letrec ([ctc (-> integer? (recursive-contract ctc))])
      (letrec ([f (λ (x) f)])
        ((((contract ctc f 'pos 'neg) 1) 2) 3))))

  (test/neg-blame
   'recursive-contract2
   '(letrec ([ctc (-> integer? (recursive-contract ctc))])
      (letrec ([f (λ (x) f)])
        ((contract ctc f 'pos 'neg) #f))))

  (test/neg-blame
   'recursive-contract3
   '(letrec ([ctc (-> integer? (recursive-contract ctc))])
      (letrec ([f (λ (x) f)])
        ((((contract ctc f 'pos 'neg) 1) 2) #f))))

  (test/pos-blame
   'recursive-contract4
   '(letrec ([ctc (-> integer? (recursive-contract ctc))])
      (letrec ([c 0]
               [f (λ (x)
                    (set! c (+ c 1))
                    (if (= c 2)
                        'nope
                        f))])
        ((((contract ctc f 'pos 'neg) 1) 2) 3))))

  (test/spec-passed
   'recursive-contract5
   '(contract (recursive-contract #f)
              #f
              'pos
              'neg))

  (test/spec-passed
   'recursive-contract6
   '(letrec ([ctc (or/c number? (cons/c number? (recursive-contract ctc #:flat)))])
      (contract ctc (cons 1 (cons 2 3)) 'pos 'neg)))

  (test/pos-blame
   'recursive-contract7
   '(letrec ([ctc (or/c number? (cons/c number? (recursive-contract ctc #:flat)))])
      (contract ctc (cons 1 (cons 2 #t)) 'pos 'neg)))

  (test/pos-blame
   'recursive-contract8
   '(letrec ([ctc (or/c number? (cons/c number? (recursive-contract ctc #:flat)))])
      (contract ctc (cons 1 (cons #t 3)) 'pos 'neg)))

  (test/spec-passed
   'recursive-contract9
   '(letrec ([ctc (or/c number? (hash/c (recursive-contract ctc #:chaperone) number?))])
      (make-hash (list (cons (make-hash (list (cons 3 4))) 5)))))
  
  (test/pos-blame
   'recursive-contract10
   '(let ()
      (struct doll (contents))
      (letrec ([doll-ctc (recursive-contract (or/c 'center (struct/c doll doll-ctc)) #:flat)])
        (contract doll-ctc (doll 3) 'pos 'neg))))
  
  (test/pos-blame
   'recursive-contract11
   '(let ()
      (struct doll (contents))
      (letrec ([doll-ctc2 (or/c 'center (struct/c doll (recursive-contract doll-ctc2 #:flat)))])
        (contract doll-ctc2 (doll 4) 'pos 'neg))))



;
;
;
;       ;;;;            ;;;   ;;
;       ;;;;           ;;;;   ;;
;    ;;;;;;;   ;;;    ;;;;;      ;;;; ;;;    ;;;
;   ;;;;;;;;  ;;;;;   ;;;;  ;;;; ;;;;;;;;;  ;;;;;
;  ;;;;;;;;; ;;;; ;; ;;;;;; ;;;; ;;;; ;;;; ;;;; ;;
;  ;;;; ;;;; ;;;;;;; ;;;;;; ;;;; ;;;; ;;;; ;;;;;;; ;;;;;
;  ;;;;;;;;; ;;;;;    ;;;;  ;;;; ;;;; ;;;; ;;;;;   ;;;;;
;   ;;;;;;;;  ;;;;;;  ;;;;  ;;;; ;;;; ;;;;  ;;;;;;
;    ;;;;;;;   ;;;;   ;;;;  ;;;; ;;;; ;;;;   ;;;;
;
;
;
;
;
;
;                                 ;                               ;
;                                ;;                              ;;
;    ;;;;;   ;;;;   ;;;; ;;;   ;;;;; ;;; ;;; ;;;;;;;    ;;;;;  ;;;;;
;   ;;;;;;  ;;;;;;  ;;;;;;;;; ;;;;;; ;;;;;;; ;;;;;;;;  ;;;;;; ;;;;;;
;  ;;;;;;; ;;;;;;;; ;;;; ;;;;  ;;;;  ;;;; ;;     ;;;; ;;;;;;;  ;;;;
;  ;;;;    ;;;; ;;; ;;;; ;;;;  ;;;;  ;;;;     ;;;;;;; ;;;;     ;;;;  ;;;;;
;  ;;;;;;; ;;;;;;;; ;;;; ;;;;  ;;;;; ;;;;    ;;  ;;;; ;;;;;;;  ;;;;; ;;;;;
;   ;;;;;;  ;;;;;;  ;;;; ;;;;  ;;;;; ;;;;    ;;;;;;;;  ;;;;;;  ;;;;;
;    ;;;;;   ;;;;   ;;;; ;;;;   ;;;; ;;;;     ;; ;;;;   ;;;;;   ;;;;
;
;
;
;
;
;
;             ;                                ;
;            ;;                               ;;
;   ;;;;;  ;;;;; ;;; ;;; ;;;; ;;;;   ;;;;;  ;;;;;
;  ;;;;;; ;;;;;; ;;;;;;; ;;;; ;;;;  ;;;;;; ;;;;;;
;  ;;;;    ;;;;  ;;;; ;; ;;;; ;;;; ;;;;;;;  ;;;;
;   ;;;;   ;;;;  ;;;;    ;;;; ;;;; ;;;;     ;;;;
;    ;;;;  ;;;;; ;;;;    ;;;; ;;;; ;;;;;;;  ;;;;;
;  ;;;;;;  ;;;;; ;;;;    ;;;;;;;;;  ;;;;;;  ;;;;;
;  ;;;;;    ;;;; ;;;;     ;;; ;;;;   ;;;;;   ;;;;
;
;
;


  (contract-eval '(define-contract-struct couple (hd tl)))
  (contract-eval '(contract-struct triple (a b c)))

  (test/spec-passed
   'd-c-s-match1
   '(begin
      (eval '(module d-c-s-match1 scheme/base
               (require scheme/contract
                        mzlib/match)

               (define-contract-struct foo (bar baz))

               (match (make-foo #t #f)
                 [($ foo bar baz) #t]
                 [_ #f])))
      (eval '(require 'd-c-s-match1))))

  (test/spec-passed/result
   'd-c-s-match2
   '(begin
      (eval '(module d-c-s-match2 scheme/base
               (require scheme/contract
                        mzlib/match)

               (define-contract-struct foo (bar baz))

               (provide d-c-s-match2-f1)
               (define d-c-s-match2-f1
                 (match (make-foo 'first 'second)
                   [($ foo bar baz) (list bar baz)]
                   [_ #f]))))
      (eval '(require 'd-c-s-match2))
      (eval 'd-c-s-match2-f1))
   '(first second))

  (test/spec-passed/result
   'd-c-s-match3
   '(begin
      (eval '(module d-c-s-match3-a scheme/base

               (require scheme/contract)

               (define-struct super (a b c) #:transparent)
               (define-struct (sub super) () #:transparent)

               (provide/contract
                [struct super       ([a number?] [b number?] [c number?])]
                [struct (sub super) ([a number?] [b number?] [c number?])])))
      (eval '(module d-c-s-match3-b scheme/base
               (require scheme/match)

               (require 'd-c-s-match3-a)

               (provide d-c-s-match3-ans)
               (define d-c-s-match3-ans
                 (match (make-sub 1 2 3)
                   [(struct sub (a b c))
                    (list a b c)]))))
      (eval '(require 'd-c-s-match3-b))
      (eval 'd-c-s-match3-ans))
   '(1 2 3))

  (test/pos-blame 'd-c-s1
                  '(begin
                     (eval '(module d-c-s1 scheme/base
                              (require scheme/contract)
                              (define-contract-struct couple (hd tl))
                              (contract (couple/c any/c any/c) 1 'pos 'neg)))
                     (eval '(require 'd-c-s1))))

  (test/spec-passed 'd-c-s2
                    '(contract (couple/c any/c any/c) (make-couple 1 2) 'pos 'neg))
  (test/spec-passed 'd-c-s3
                    '(contract (couple/c number? number?)
                               (make-couple 1 2)
                               'pos 'neg))
  (test/spec-passed 'd-c-s4
                    '(couple-hd
                      (contract (couple/c number? number?)
                                (make-couple 1 2)
                                'pos 'neg)))
  (test/spec-passed 'd-c-s5
                    '(couple-tl
                      (contract (couple/c number? number?)
                                (make-couple 1 2)
                                'pos 'neg)))


  (test/pos-blame
   'd-c-s6
   '(couple-tl
     (contract (couple/c number?
                         number?)
               (make-couple #f 2)
               'pos 'neg)))
  (test/pos-blame
   'd-c-s7
   '(couple-hd
     (contract (couple/c number? number?)
               (make-couple #f 2)
               'pos 'neg)))

  (test/pos-blame
   'd-c-s8
   '(contract (couple/dc [hd any/c] [tl any/c])
              1
              'pos 'neg))

  (test/pos-blame
   'd-c-s9
   '(contract (couple/dc [hd () any/c] [tl () any/c])
              1
              'pos 'neg))


  (test/spec-passed 'd-c-s10
                    '(contract (couple/dc [hd any/c] [tl any/c])
                               (make-couple 1 2)
                               'pos 'neg))
  (test/spec-passed 'd-c-s11
                    '(contract (couple/dc [hd () any/c] [tl () any/c])
                               (make-couple 1 2)
                               'pos 'neg))

  (test/spec-passed 'd-c-s12
                    '(contract (couple/dc [hd number?]
                                          [tl number?])
                               (make-couple 1 2)
                               'pos 'neg))
  (test/spec-passed 'd-c-s13
                    '(couple-hd
                      (contract (couple/dc [hd number?]
                                           [tl number?])
                                (make-couple 1 2)
                                'pos 'neg)))
  (test/spec-passed 'd-c-s14
                    '(couple-tl
                      (contract (couple/dc [hd number?]
                                           [tl number?])
                                (make-couple 1 2)
                                'pos 'neg)))


  (test/pos-blame
   'd-c-s15
   '(couple-hd
     (contract (couple/dc [hd number?]
                          [tl number?])
               (make-couple #f 2)
               'pos 'neg)))

  (test/pos-blame
   'd-c-s16
   '(couple-tl
     (contract (couple/dc [hd number?]
                          [tl number?])
               (make-couple #f 2)
               'pos 'neg)))

  (test/spec-passed
   'd-c-s17
   '(couple-hd
     (contract (couple/dc [hd number?]
                          [tl (hd) (>=/c hd)])
               (make-couple 1 2)
               'pos 'neg)))

  (test/pos-blame
   'd-c-s18
   '(couple-hd
     (contract (couple/dc [hd number?]
                          [tl (hd) (>=/c hd)])
               (make-couple 2 1)
               'pos 'neg)))

  (test/spec-passed
   'd-c-s19
   '(couple-tl
     (couple-tl
      (contract (couple/dc [hd number?]
                           [tl (hd)
                               (let ([hd1 hd])
                                 (couple/dc [hd (>=/c hd1)]
                                            [tl (hd) (>=/c hd)]))])
                (make-couple 1 (make-couple 2 3))
                'pos 'neg))))

  (test/pos-blame
   'd-c-s20
   '(couple-tl
     (couple-tl
      (contract (couple/dc [hd number?]
                           [tl (hd)
                               (let ([hd1 hd])
                                 (couple/dc [hd (>=/c hd1)]
                                            [tl (hd) (>=/c hd1)]))])
                (make-couple 1 (make-couple 2 0))
                'pos 'neg))))

  (test/spec-passed
   'd-c-s21
   '(couple-hd
     (contract (couple/dc [hd number?]
                          [tl number?])
               (contract (couple/dc [hd number?]
                                    [tl number?])
                         (make-couple 1 2)
                         'pos 'neg)
               'pos 'neg)))

  (test/spec-passed
   'd-c-s22
   '(couple-hd
     (contract (couple/dc [hd (>=/c 0)]
                          [tl (>=/c 0)])
               (contract (couple/dc [hd number?]
                                    [tl number?])
                         (make-couple 1 2)
                         'pos 'neg)
               'pos 'neg)))

  (test/pos-blame
   'd-c-s23
   '(couple-hd
     (contract (couple/dc [hd (>=/c 0)]
                          [tl (>=/c 0)])
               (contract (couple/dc [hd number?]
                                    [tl number?])
                         (make-couple -1 2)
                         'pos 'neg)
               'pos 'neg)))

   (test/pos-blame
    'd-c-s24
    '(couple-hd
      (contract (couple/dc [hd number?]
                           [tl number?])
                (contract (couple/dc [hd (>=/c 0)]
                                     [tl (>=/c 0)])
                          (make-couple -1 2)
                          'pos 'neg)
                'pos 'neg)))

   (test/pos-blame
    'd-c-s25
    '(couple-hd
      (contract (couple/dc [hd number?]
                           [tl number?])
                (contract (couple/dc [hd number?]
                                     [tl number?])
                          (contract (couple/dc [hd (>=/c 0)]
                                               [tl (>=/c 0)])
                                    (make-couple -1 2)
                                    'pos 'neg)
                          'pos 'neg)
                'pos 'neg)))

   (test/pos-blame
    'd-c-s26
    '(couple-hd
      (contract (couple/dc [hd (>=/c 10)]
                           [tl (>=/c 10)])
                (contract (couple/dc [hd positive?]
                                     [tl positive?])
                          (contract (couple/dc [hd number?]
                                               [tl number?])
                                    (make-couple 1 2)
                                    'pos 'neg)
                          'pos 'neg)
                'pos 'neg)))


  ;; test caching
  (test/spec-passed
   'd-c-s27
   '(let ([ctc (couple/c any/c any/c)])
      (couple-hd (contract ctc (contract ctc (make-couple 1 2) 'pos 'neg) 'pos 'neg))))

  ;; make sure lazy really is lazy
  (test/spec-passed
   'd-c-s28
   '(contract (couple/c number? number?)
              (make-couple #f #f)
              'pos 'neg))

  (test/spec-passed
   'd-c-s29
   '(couple-hd
     (contract (couple/c (couple/c number? number?)
                         (couple/c number? number?))
               (make-couple (make-couple #f #f)
                            (make-couple #f #f))
               'pos 'neg)))

  (test/spec-passed
   'd-c-s30
   '(couple-tl
     (contract (couple/c (couple/c number? number?)
                         (couple/c number? number?))
               (make-couple (make-couple #f #f)
                            (make-couple #f #f))
               'pos 'neg)))

  ;; make sure second accesses work
  (test/spec-passed/result
   'd-c-s31
   '(let ([v (contract (couple/c number? number?)
                       (make-couple 1 2)
                       'pos 'neg)])
      (list (couple-hd v) (couple-hd v)))
   (list 1 1))

  (test/pos-blame
   'd-c-s32
   '(let ([v (contract (couple/c number? boolean?)
                       (make-couple 1 2)
                       'pos 'neg)])
      (with-handlers ([void void]) (couple-hd v))
      (couple-hd v)))

  (test/pos-blame
   'd-c-s33
   '(let ([v (contract (couple/c number? number?)
                       (make-couple 1 2)
                       'pos 'neg)])
      (couple-hd v)
      (couple-hd v)
      (couple-hd
       (contract (couple/c boolean? boolean?)
                 v
                 'pos 'neg))))

  (contract-eval '(define-contract-struct single (a)))
  ;; a related test to the above:
  (test/spec-passed/result
   'd-c-s34
   '(let ([v (contract (single/c number?) (make-single 1) 'pos 'neg)])
      (single-a v)
      (let ([v3 (contract (single/c number?) v 'pos 'neg)])
        (single-a v3)))
   1)

  ;; make sure the caching doesn't break the semantics
  (test/pos-blame
   'd-c-s35
   '(let ([v (contract (couple/c any/c
                                 (couple/c any/c
                                           (couple/c any/c
                                                     number?)))
                       (make-couple 1
                                    (make-couple 2
                                                 (make-couple 3
                                                              #f)))
                       'pos 'neg)])
      (let* ([x (couple-tl v)]
             [y (couple-tl x)])
        (couple-hd (couple-tl x)))))

  (test/spec-passed/result
   'd-c-s36
   '(let ([x (make-couple 1 2)]
          [y (make-couple 1 2)]
          [c1 (couple/dc [hd any/c]
                         [tl (hd) any/c])]
          [c2 (couple/c any/c any/c)])
      (couple-hd (contract c1 x 'pos 'neg))
      (couple-hd (contract c2 x 'pos 'neg))
      (couple-hd (contract c2 y 'pos 'neg))
      (couple-hd (contract c1 y 'pos 'neg)))
   1)

  ;; make sure that define-contract-struct contracts can go at the top level
  (test/spec-passed
   'd-c-s37
   '(contract-stronger?
     (couple/dc [hd any/c]
                [tl (hd) any/c])
     (couple/dc [hd any/c]
                [tl (hd) any/c])))

  ;; test functions inside structs

  (test/spec-passed/result
   'd-c-s38
   '(let ([x (make-couple (lambda (x) x) (lambda (x) x))]
          [c (couple/dc [hd (-> integer? integer?)]
                        [tl (hd) any/c])])
      ((couple-hd (contract c x 'pos 'neg)) 1))
   1)

  (test/neg-blame
   'd-c-s39
   '(let ([x (make-couple (lambda (x) x) (lambda (x) x))]
          [c (couple/dc [hd (-> integer? integer?)]
                        [tl (hd) any/c])])
      ((couple-hd (contract c x 'pos 'neg)) #f)))

  (test/pos-blame
   'd-c-s40
   '(let ([x (make-couple (lambda (x) #f) (lambda (x) #f))]
          [c (couple/dc [hd (-> integer? integer?)]
                        [tl (hd) any/c])])
      ((couple-hd (contract c x 'pos 'neg)) 1)))

  (test/spec-passed/result
   'd-c-s41
   '(let ([x (make-couple 5 (lambda (x) x))]
          [c (couple/dc [hd number?]
                        [tl (hd) (-> (>=/c hd) (>=/c hd))])])
      ((couple-tl (contract c x 'pos 'neg)) 6))
   6)

  (test/pos-blame
   'd-c-s42
   '(let ([x (make-couple 5 (lambda (x) -10))]
          [c (couple/dc [hd number?]
                        [tl (hd) (-> (>=/c hd) (>=/c hd))])])
      ((couple-tl (contract c x 'pos 'neg)) 6)))

  (test/neg-blame
   'd-c-s42
   '(let ([x (make-couple 5 (lambda (x) -10))]
          [c (couple/dc [hd number?]
                        [tl (hd) (-> (>=/c hd) (>=/c hd))])])
      ((couple-tl (contract c x 'pos 'neg)) -11)))

  (contract-eval '(contract-struct no-define (x)))
  (test/spec-passed/result
  'd-c-s43
  '(no-define-x (no-define 1))
  '1)
  (test/spec-passed/result
  'd-c-s44
  '(no-define? (no-define 1))
  '#t)



;
;
;
;       ;;;;            ;;;   ;;                                               ;   ;;
;       ;;;;           ;;;;   ;;                                              ;;   ;;
;    ;;;;;;;   ;;;    ;;;;;      ;;;; ;;;    ;;;          ;;;;   ;;;;;;;    ;;;;;  ;;  ;;;;;
;   ;;;;;;;;  ;;;;;   ;;;;  ;;;; ;;;;;;;;;  ;;;;;        ;;;;;;  ;;;;;;;;  ;;;;;;  ;; ;;;;;;
;  ;;;;;;;;; ;;;; ;; ;;;;;; ;;;; ;;;; ;;;; ;;;; ;;      ;;;;;;;; ;;;;;;;;;  ;;;;   ;;;;;;;;;
;  ;;;; ;;;; ;;;;;;; ;;;;;; ;;;; ;;;; ;;;; ;;;;;;; ;;;;;;;;; ;;; ;;;; ;;;;  ;;;;  ;; ;;;;
;  ;;;;;;;;; ;;;;;    ;;;;  ;;;; ;;;; ;;;; ;;;;;   ;;;;;;;;;;;;; ;;;;;;;;;  ;;;;; ;; ;;;;;;;
;   ;;;;;;;;  ;;;;;;  ;;;;  ;;;; ;;;; ;;;;  ;;;;;;       ;;;;;;  ;;;;;;;;   ;;;;; ;;  ;;;;;;
;    ;;;;;;;   ;;;;   ;;;;  ;;;; ;;;; ;;;;   ;;;;         ;;;;   ;;;;;;;     ;;;; ;;   ;;;;;
;                                                                ;;;;             ;;
;                                                                ;;;;
;


  (contract-eval '(define-contract-struct node (val obj rank left right) (make-inspector)))
  (contract-eval '(define (compute-rank n)
                    (cond
                      [(not n) 0]
                      [else (node-rank n)])))

  (contract-eval '(define-opt/c (leftist-heap-greater-than/rank/opt n r)
                    (or/c not
                          (node/dc [val (>=/c n)]
                                   [obj any/c]
                                   [rank (<=/c r)]
                                   [left (val) (leftist-heap-greater-than/rank/opt val +inf.0)]
                                   [right (val left) (leftist-heap-greater-than/rank/opt val (compute-rank left))]))))

  (contract-eval '(define leftist-heap/c (leftist-heap-greater-than/rank/opt -inf.0 +inf.0)))

  (test/pos-blame 'd-o/c1 '(contract leftist-heap/c 2 'pos 'neg))


  (test/spec-passed 'd-o/c2 '(contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg))
  (test/spec-passed 'd-o/c3 '(contract leftist-heap/c #f 'pos 'neg))
  (test/spec-passed 'd-o/c4 '(contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg))
  (test/spec-passed/result 'd-o/c5
                           '(node? (contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg))
                           #t)

  (test/spec-passed/result 'd-o/c6 '(node-val (contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg)) 1)
  (test/spec-passed/result 'd-o/c7 '(node-obj (contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg)) 2)
  (test/spec-passed/result 'd-o/c8 '(node-rank (contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg)) 3)
  (test/spec-passed/result 'd-o/c9 '(node-left (contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg)) #f)
  (test/spec-passed/result 'd-o/c10 '(node-right (contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg)) #f)

  (test/spec-passed/result 'd-o/c11
                           '(node-val (contract leftist-heap/c
                                                (contract leftist-heap/c
                                                          (make-node 1 2 3 #f #f)
                                                          'pos 'neg)
                                                'pos 'neg))
                           1)
  (test/spec-passed/result 'd-o/c12
                           '(node-obj (contract leftist-heap/c
                                                (contract leftist-heap/c
                                                          (make-node 1 2 3 #f #f)
                                                          'pos 'neg)
                                                'pos 'neg))
                           2)
  (test/spec-passed/result 'd-o/c13
                           '(node-rank (contract leftist-heap/c
                                                 (contract leftist-heap/c
                                                           (make-node 1 2 3 #f #f)
                                                           'pos 'neg)
                                                 'pos 'neg))
                           3)
  (test/spec-passed/result 'd-o/c14
                           '(node-left (contract leftist-heap/c
                                                 (contract leftist-heap/c
                                                           (make-node 1 2 3 #f #f)
                                                           'pos 'neg)
                                                 'pos 'neg))
                           #f)
  (test/spec-passed/result 'd-o/c15
                           '(node-right (contract leftist-heap/c
                                                  (contract leftist-heap/c
                                                            (make-node 1 2 3 #f #f)
                                                            'pos 'neg)
                                                  'pos 'neg))
                           #f)

  (test/spec-passed/result 'd-o/c16
                           '(let ([h (contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg)])
                              (node-val h)
                              (node-val h))
                           1)
  (test/spec-passed/result 'd-o/c17
                           '(let ([h (contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg)])
                              (node-obj h)
                              (node-obj h))
                           2)

  (test/spec-passed/result 'd-o/c18
                           '(let ([h (contract leftist-heap/c (make-node 1 2 3 #f #f)'pos 'neg)])
                              (node-rank h)
                              (node-rank h))
                           3)
  (test/spec-passed/result 'd-o/c19
                           '(let ([h (contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg)])
                              (node-left h)
                              (node-left h))
                           #f)
  (test/spec-passed/result 'd-o/c20
                           '(let ([h (contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg)])
                              (node-right h)
                              (node-right h))
                           #f)

  (test/spec-passed/result 'd-o/c21
                           '(node-val
                             (node-right
                              (contract leftist-heap/c
                                        (make-node 1 2 3
                                                   (make-node 7 8 9 #f #f)
                                                   (make-node 4 5 6 #f #f))
                                        'pos 'neg)))
                           4)
  (test/spec-passed/result 'd-o/c22
                           '(node-val
                             (node-left
                              (contract leftist-heap/c
                                        (make-node 1 2 3
                                                   (make-node 7 8 9 #f #f)
                                                   (make-node 4 5 6 #f #f))
                                        'pos 'neg)))
                           7)

  (test/pos-blame 'd-o/c23
                  '(node-val
                    (node-right
                     (contract leftist-heap/c
                               (make-node 5 2 3
                                          (make-node 7 8 9 #f #f)
                                          (make-node 4 5 6 #f #f))
                               'pos 'neg))))

  (test/pos-blame 'd-o/c24
                  '(node-val
                    (node-left
                     (contract leftist-heap/c
                               (make-node 9 2 3
                                          (make-node 7 8 9 #f #f)
                                          (make-node 11 5 6 #f #f))
                               'pos 'neg))))

  (test/neg-blame 'd-o/c25
                  '((contract (-> leftist-heap/c any)
                              (λ (kh)
                                (node-val
                                 (node-left
                                  kh)))
                              'pos 'neg)
                    (make-node 9 2 3
                               (make-node 7 8 9 #f #f)
                               (make-node 11 5 6 #f #f))))



  (test/spec-passed/result
   'd-o/c26
   '(let ([ai (λ (x) (contract leftist-heap/c x 'pos 'neg))])
      (define (remove-min t) (merge (node-left t) (node-right t)))

      (define (merge t1 t2)
        (cond
          [(not t1) t2]
          [(not t2) t1]
          [else
           (let ([t1-val (node-val t1)]
                 [t2-val (node-val t2)])
             (cond
               [(<= t1-val t2-val)
                (pick t1-val
                      (node-obj t1)
                      (node-left t1)
                      (merge (node-right t1)
                             t2))]
               [else
                (pick t2-val
                      (node-obj t2)
                      (node-left t2)
                      (merge t1
                             (node-right t2)))]))]))

      (define (pick x obj a b)
        (let ([ra (compute-rank a)]
              [rb (compute-rank b)])
          (cond
            [(>= ra rb)
             (make-node x obj (+ rb 1) a b)]
            [else
             (make-node x obj (+ ra 1) b a)])))
      (node-val
       (remove-min (ai (make-node 137 'x 1
                                  (ai (make-node 178 'y 1
                                                 (make-node 178 'z 1 #f #f)
                                                 #f))
                                  #f)))))
   178)

  (test/spec-passed/result
   'd-o/c27
   '(let ()
      (define-opt/c (f x)
        (and/c (>=/c x)
               (g x)))
      (define-opt/c (g x)
        (<=/c x))
      (contract (f 11) 11 'pos 'neg))
   11)

  ;; try-one : syntax -> number
  ;; evaluates the exp and returns the number of opt/c warnings found
  (contract-eval
   '(define (eval-and-count-log-messages exp)
      (define my-logger (make-logger))
      (parameterize ([current-logger my-logger])
        (define ans (make-channel))
        (define recv (make-log-receiver my-logger 'warning))
        (thread
         (λ ()
           (let loop ([opt/c-msgs 0])
             (define res (sync recv))
             (cond
               [(equal? "done" (vector-ref res 1))
                (channel-put ans opt/c-msgs)]
               [else
                (define opt/c-msg? (regexp-match? #rx"opt/c" (vector-ref res 1)))
                (loop (if opt/c-msg?
                          (+ opt/c-msgs 1)
                          opt/c-msgs))]))))
        (let/ec k
          (parameterize ([error-escape-handler k])
            (eval exp)))
        (log-warning "done")
        (channel-get ans))))

  (ctest 1 eval-and-count-log-messages
         '(let ()
            (struct s (a b))
            (opt/c (struct/dc s [a (-> integer? integer?)] [b (a) integer?]))))

  (ctest 1 eval-and-count-log-messages
         '(let ()
            (struct s (a b))
            (define-opt/c (f x)
              (-> integer? integer?))
            (define-opt/c (g x)
              (struct/dc s [a (f 1)] [b (a) integer?]))
            1))

  (ctest 0 eval-and-count-log-messages
         '(let ()
            (struct s (a b))
            (define-opt/c (f x) integer?)
            (opt/c (struct/dc s [a (f 1)] [b (a) integer?]))))

  (ctest 0 eval-and-count-log-messages
         '(let ()
            (define-struct h:kons (hd tl) #:transparent)
            (define-struct h:node (rank val obj children) #:transparent)

            (define-opt/c (binomial-tree-rank=/sco r v)
              (or/c #f
                    (struct/dc h:node
                               [rank (=/c r)]
                               [val (>=/c v)]
                               [children (rank val) #:lazy (heap-ordered/desc/sco (- rank 1) val)])))

            (define-opt/c (binomial-tree-rank>/sco r)
              (or/c #f
                    (struct/dc h:node
                               [rank (>=/c r)]
                               [val any/c]
                               [children (rank val) #:lazy (heap-ordered/desc/sco (- rank 1) val)])))

            (define-opt/c (heap-ordered/desc/sco rank val)
              (or/c #f
                    (struct/dc h:kons
                               [hd #:lazy (binomial-tree-rank=/sco rank val)]
                               [tl () #:lazy (heap-ordered/desc/sco (- rank 1) val)])))

            (define-opt/c (binomial-trees/asc/sco rank)
              (or/c #f
                    (struct/dc h:kons
                               [hd #:lazy (binomial-tree-rank>/sco rank)]
                               [tl (hd) #:lazy (binomial-trees/asc/sco (h:node-rank hd))])))

            (define binomial-heap/sco (binomial-trees/asc/sco -inf.0))
            1))


  ;;
  ;;  end of define-opt/c
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;  opt/c and blame
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (contract-eval
   '(begin

      (define proj:blame/c
        (make-contract
         #:name 'proj:blame/c
         #:projection
         (lambda (blame)
           (lambda (x)
             (if (blame-swapped? blame) 'negative 'positive)))))

      (define call*0 'dummy)
      (define (call*1 x0) x0)
      (define (call*2 f1 x0) (f1 x0))
      (define (call*3 f2 x1 x0) (f2 x1 x0))))

  (test/spec-passed/result
   'opt/c-blame-0
   '((contract
      (-> (-> (-> proj:blame/c any/c) any/c any/c) (-> any/c any/c) any/c any/c)
      call*3
      'pos
      'neg)
     call*2
     call*1
     call*0)
   'negative)

  (test/spec-passed/result
   'opt/c-blame-1
   '((contract
      (opt/c (-> (-> (-> proj:blame/c any/c) any/c any/c) (-> any/c any/c) any/c any/c))
      call*3
      'pos
      'neg)
     call*2
     call*1
     call*0)
   'negative)

  (test/spec-passed/result
   'opt/c-blame-2
   '((contract
      (-> (opt/c (-> (-> proj:blame/c any/c) any/c any/c)) (-> any/c any/c) any/c any/c)
      call*3
      'pos
      'neg)
     call*2
     call*1
     call*0)
   'negative)

  (test/spec-passed/result
   'opt/c-blame-3
   '((contract
      (-> (-> (opt/c (-> proj:blame/c any/c)) any/c any/c) (-> any/c any/c) any/c any/c)
      call*3
      'pos
      'neg)
     call*2
     call*1
     call*0)
   'negative)

  (test/spec-passed/result
   'opt/c-blame-4
   '((contract
      (-> (-> (-> (opt/c proj:blame/c) any/c) any/c any/c) (-> any/c any/c) any/c any/c)
      call*3
      'pos
      'neg)
     call*2
     call*1
     call*0)
   'negative)

  ;; NOT YET RELEASED
  #;
  (test/pos-blame
   'd-c-s/attr-1
   '(let ()
      (define-contract-struct pr (x y))
      (pr-x
       (contract (pr/dc [x integer?]
                        [y integer?]
                        where
                        [x-val x]
                        [y-val y]
                        and
                        (= x-val y-val))
                 (make-pr 4 5)
                 'pos
                 'neg))))

  ;; NOT YET RELEASED
  #;
  (test/spec-passed
   'd-c-s/attr-2
   '(let ()
      (define-contract-struct pr (x y))
      (contract (pr/dc [x integer?]
                       [y integer?]
                       where
                       [x-val x]
                       [y-val y]
                       and
                       (= x-val y-val))
                (make-pr 4 5)
                'pos
                'neg)))

  ;; NOT YET RELEASED
  #;
  (let ()
    (define-contract-struct node (n l r) (make-inspector))

    (define (get-val n attr)
      (if (null? n)
          1
          (let ([h (synthesized-value n attr)])
            (if (unknown? h)
                h
                (+ h 1)))))

    (define (full-bbt lo hi)
      (or/c null?
            (node/dc [n (between/c lo hi)]
                     [l (n) (full-bbt lo n)]
                     [r (n) (full-bbt n hi)]

                     where
                     [lheight (get-val l lheight)]
                     [rheight (get-val r rheight)]

                     and
                     (<= 0 (- lheight rheight) 1))))

    (define t (contract (full-bbt -inf.0 +inf.0)
                        (make-node 0
                                   (make-node -1 null null)
                                   (make-node 2
                                              (make-node 1 null null)
                                              (make-node 3 null null)))
                        'pos
                        'neg))
    (test/spec-passed
     'd-c-s/attr-3
     `(,node-l (,node-l ,t)))

    (test/pos-blame
     'd-c-s/attr-4
     `(,node-r (,node-r (,node-r ,t)))))

  ;; NOT YET RELEASED
  #|

need a test that will revisit a node a second time (when it already has a wrapper)
with a new parent. make sure the new parent is recorded in the parents field
so that propagation occurs.

|#


  ;; test the predicate
  (ctest #t couple? (contract (couple/c any/c any/c) (make-couple 1 2) 'pos 'neg))
  (ctest #t couple? (make-couple 1 2))
  (ctest #t couple? (contract (couple/dc [hd any/c] [tl (hd) any/c]) (make-couple 1 2) 'pos 'neg))
  (ctest #f couple? 1)
  (ctest #f couple? #f)


;
;
;
;     ;;; ;;;;              ;                                      ;                               ;    ;;;
;    ;;;; ;;;;             ;;                                     ;;                              ;;   ;;;;;
;   ;;;;; ;;;; ;;;;;;;   ;;;;;        ;;;;;   ;;;;   ;;;; ;;;   ;;;;; ;;; ;;; ;;;;;;;    ;;;;;  ;;;;; ;;;;;;
;   ;;;;  ;;;; ;;;;;;;; ;;;;;;       ;;;;;;  ;;;;;;  ;;;;;;;;; ;;;;;; ;;;;;;; ;;;;;;;;  ;;;;;; ;;;;;;    ;;;
;  ;;;;;; ;;;;     ;;;;  ;;;;       ;;;;;;; ;;;;;;;; ;;;; ;;;;  ;;;;  ;;;; ;;     ;;;; ;;;;;;;  ;;;;     ;;
;  ;;;;;; ;;;;  ;;;;;;;  ;;;;  ;;;;;;;;;    ;;;; ;;; ;;;; ;;;;  ;;;;  ;;;;     ;;;;;;; ;;;;     ;;;;     ;
;   ;;;;  ;;;; ;;  ;;;;  ;;;;; ;;;;;;;;;;;; ;;;;;;;; ;;;; ;;;;  ;;;;; ;;;;    ;;  ;;;; ;;;;;;;  ;;;;;   ;;;
;   ;;;;  ;;;; ;;;;;;;;  ;;;;;       ;;;;;;  ;;;;;;  ;;;; ;;;;  ;;;;; ;;;;    ;;;;;;;;  ;;;;;;  ;;;;;   ;;;
;   ;;;;  ;;;;  ;; ;;;;   ;;;;        ;;;;;   ;;;;   ;;;; ;;;;   ;;;; ;;;;     ;; ;;;;   ;;;;;   ;;;;   ;;;
;
;
;



  (ctest #t flat-contract? (or/c))
  (ctest #t flat-contract? (or/c integer? (lambda (x) (> x 0))))
  (ctest #t flat-contract? (or/c (flat-contract integer?) (flat-contract boolean?)))
  (ctest #t flat-contract? (or/c integer? boolean?))
  (test-flat-contract '(or/c (flat-contract integer?) char?) #\a #t)
  (test-flat-contract '(or/c (flat-contract integer?) char?) 1 #t)

  (ctest #t flat-contract? (-> any/c any/c any))

  (ctest #t flat-contract? (and/c))
  (ctest #t flat-contract? (and/c number? integer?))
  (ctest #t flat-contract? (and/c (flat-contract number?)
				 (flat-contract integer?)))
  (ctest #t flat-contract? (let ()
                             (define-struct s (a b))
                             (struct/c s any/c any/c)))
  (ctest #f flat-contract? (let ()
                             (define-struct s (a b) #:mutable)
                             (struct/c s any/c any/c)))
  (ctest #f flat-contract? (let ()
                             (define-struct s (a b) #:mutable)
                             (struct/c s any/c integer?)))
  (ctest #t chaperone-contract? (let ()
                                  (define-struct s (a b) #:mutable)
                                  (struct/c s any/c any/c)))
  (ctest #f impersonator-contract? (let ()
                                    (define-struct s (a b) #:mutable)
                                    (struct/c s any/c any/c)))
  (ctest #f flat-contract? (let ()
                             (define-struct s ([a #:mutable] b))
                             (struct/c s any/c any/c)))
  (ctest #t chaperone-contract? (let ()
                                  (define-struct s ([a #:mutable] b))
                                  (struct/c s any/c any/c)))
  (ctest #f impersonator-contract? (let ()
                                    (define-struct s ([a #:mutable] b))
                                    (struct/c s any/c any/c)))
  (ctest #f flat-contract? (let ()
                             (define-struct s (a [b #:mutable]))
                             (struct/c s any/c any/c)))
  (ctest #t chaperone-contract? (let ()
                                  (define-struct s (a [b #:mutable]))
                                  (struct/c s any/c any/c)))
  (ctest #f impersonator-contract? (let ()
                                    (define-struct s (a [b #:mutable]))
                                    (struct/c s any/c any/c)))
  (ctest #f flat-contract? (let ()
                             (define-struct s (f))
                             (struct/c s (-> number? any))))
  (ctest #t chaperone-contract? (let ()
                                  (define-struct s (f))
                                  (struct/c s (-> number? any))))
  (ctest #f impersonator-contract? (let ()
                                    (define-struct s (f))
                                    (struct/c s (-> number? any))))

  (ctest #f flat-contract? (let ()
                             (define-struct s (a) #:mutable)
                             (define alpha (new-∃/c 'alpha))
                             (struct/c s alpha)))
  (ctest #f chaperone-contract? (let ()
                                  (define-struct s (a) #:mutable)
                                  (define alpha (new-∃/c 'alpha))
                                  (struct/c s alpha)))
  (ctest #t impersonator-contract? (let ()
                                    (define-struct s (a) #:mutable)
                                    (define alpha (new-∃/c 'alpha))
                                    (struct/c s alpha)))
  (ctest #t contract? (let ()
                        (define-struct s (a) #:mutable)
                        (define alpha (new-∃/c 'alpha))
                        (struct/c s alpha)))

  (ctest #t chaperone-contract?
         (let ()
           (struct s (a b))
           (let ([x (struct/dc s [a integer?] [b integer?])])
             (opt/c x))))

  (ctest #t flat-contract? (set/c integer?))
  (ctest #f flat-contract? (set/c (-> integer? integer?)))
  (ctest #t chaperone-contract? (set/c (-> integer? integer?)))

  (ctest #t flat-contract? (list/c integer?))
  (ctest #t chaperone-contract? (list/c (-> integer? integer?)))

  (ctest #t chaperone-contract? (promise/c integer?))
  (ctest #f chaperone-contract? (promise/c (new-∃/c 'alpha)))

  ;; Make sure that impersonators cannot be used as the element contract in set/c.
  (contract-error-test
   'contract-error-test-set
   '(let ([proxy-ctc
           (make-contract
            #:name 'proxy-ctc
            #:first-order values
            #:projection (λ (b) values))])
      (set/c proxy-ctc))
   exn:fail?)

  ;; Hash contracts with flat domain/range contracts
  (ctest #t contract?              (hash/c any/c any/c #:immutable #f))
  (ctest #t chaperone-contract?    (hash/c any/c any/c #:immutable #f))
  (ctest #f impersonator-contract? (hash/c any/c any/c #:immutable #f))
  (ctest #t flat-contract?         (hash/c any/c any/c #:immutable #f #:flat? #t))

  (ctest #t flat-contract?      (hash/c any/c any/c #:immutable #t))
  (ctest #t flat-contract?      (hash/c any/c any/c #:immutable #t #:flat? #t))

  (ctest #t contract?              (hash/c any/c any/c))
  (ctest #t chaperone-contract?    (hash/c any/c any/c))
  (ctest #f impersonator-contract? (hash/c any/c any/c))
  (ctest #t flat-contract?         (hash/c any/c any/c #:flat? #t))

  ;; Hash contracts with chaperone range contracts
  (ctest #t contract?              (hash/c number? (hash/c number? number?)))
  (ctest #t chaperone-contract?    (hash/c number? (hash/c number? number?)))
  (ctest #f impersonator-contract? (hash/c number? (hash/c number? number?)))
  (ctest #f flat-contract?         (hash/c number? (hash/c number? number?)))

  ;; Hash contracts with proxy range contracts
  (contract-eval
   '(define trivial-proxy-ctc
      (make-contract
       #:name 'trivial-proxy-ctc
       #:first-order values
       #:projection (λ (b) values))))

  (ctest #t contract?              (hash/c number? trivial-proxy-ctc #:immutable #f))
  (ctest #f chaperone-contract?    (hash/c number? trivial-proxy-ctc #:immutable #f))
  (ctest #t impersonator-contract? (hash/c number? trivial-proxy-ctc #:immutable #f))
  (ctest #f flat-contract?         (hash/c number? trivial-proxy-ctc #:immutable #f))

  (ctest #t contract?              (hash/c number? trivial-proxy-ctc #:immutable #t))
  (ctest #f chaperone-contract?    (hash/c number? trivial-proxy-ctc #:immutable #t))
  (ctest #t impersonator-contract? (hash/c number? trivial-proxy-ctc #:immutable #t))
  (ctest #f flat-contract?         (hash/c number? trivial-proxy-ctc #:immutable #t))

  (ctest #t contract?              (hash/c number? trivial-proxy-ctc))
  (ctest #f chaperone-contract?    (hash/c number? trivial-proxy-ctc))
  (ctest #t impersonator-contract? (hash/c number? trivial-proxy-ctc))
  (ctest #f flat-contract?         (hash/c number? trivial-proxy-ctc))

  ;; Make sure that proxies cannot be used as the domain contract in hash/c.
  (contract-error-test
   'contract-error-test6
   '(let ([proxy-ctc
           (make-contract
            #:name 'proxy-ctc
            #:first-order values
            #:projection (λ (b) values))])
      (hash/c proxy-ctc proxy-ctc))
   exn:fail?)

  (ctest #t contract?              (box/c number? #:flat? #t))
  (ctest #t chaperone-contract?    (box/c number? #:flat? #t))
  (ctest #f impersonator-contract? (box/c number? #:flat? #t))
  (ctest #t flat-contract?         (box/c number? #:flat? #t))

  (ctest #t contract?              (box/c number? #:immutable #t))
  (ctest #t chaperone-contract?    (box/c number? #:immutable #t))
  (ctest #f impersonator-contract? (box/c number? #:immutable #t))
  (ctest #t flat-contract?         (box/c number? #:immutable #t))

  (ctest #t contract?              (box/c number?))
  (ctest #t chaperone-contract?    (box/c number?))
  (ctest #f impersonator-contract? (box/c number?))
  (ctest #f flat-contract?         (box/c number?))

  (ctest #t contract?              (box/c (box/c number?) #:immutable #t))
  (ctest #t chaperone-contract?    (box/c (box/c number?) #:immutable #t))
  (ctest #f impersonator-contract? (box/c (box/c number?) #:immutable #t))
  (ctest #f flat-contract?         (box/c (box/c number?) #:immutable #t))

  (ctest #t contract?              (box/c trivial-proxy-ctc))
  (ctest #f chaperone-contract?    (box/c trivial-proxy-ctc))
  (ctest #t impersonator-contract? (box/c trivial-proxy-ctc))
  (ctest #f flat-contract?         (box/c trivial-proxy-ctc))

  (ctest #t contract?              (box/c trivial-proxy-ctc #:immutable #t))
  (ctest #f chaperone-contract?    (box/c trivial-proxy-ctc #:immutable #t))
  (ctest #t impersonator-contract? (box/c trivial-proxy-ctc #:immutable #t))
  (ctest #f flat-contract?         (box/c trivial-proxy-ctc #:immutable #t))

  ;; Test the ability to create different types of contracts with recursive-contract
  (ctest #t flat-contract? (letrec ([ctc (or/c number?
                                               (cons/c (recursive-contract ctc #:flat)
                                                       (recursive-contract ctc #:flat)))])
                             ctc))

  (ctest #f flat-contract? (letrec ([ctc (or/c number?
                                               (box/c (recursive-contract ctc #:chaperone)))])
                             ctc))
  (ctest #t chaperone-contract? (letrec ([ctc (or/c number?
                                                    (box/c (recursive-contract ctc #:chaperone)))])
                                  ctc))
  (ctest #f impersonator-contract? (letrec ([ctc (or/c number?
                                                  (box/c (recursive-contract ctc #:chaperone)))])
                                    ctc))

  (ctest #t contract? 1)
  (ctest #t contract? (-> 1 1))

  (ctest #t chaperone-contract? (let ()
                                  (struct s (a b))
                                  (struct/dc s [a integer?] [b integer?])))
  (ctest #t flat-contract? (let ()
                             (struct s (a b))
                             (struct/dc s [a integer?] [b integer?])))
  (ctest #t flat-contract? (let ()
                             (struct s (a b))
                             (struct/dc s [a integer?] [b (a) #:flat (>=/c a)])))
  (contract-error-test
   'struct/dc-not-really-flat-dep-field
   #'(let ()
       (struct s (a b))
       (contract (struct/dc s [a integer?] [b (a) #:flat (-> integer? integer?)])
                 (s 1 (λ (x) x))
                 'pos
                 'neg))
   exn:fail?)
  (ctest #t chaperone-contract? (let ()
                                  (struct s (a b))
                                  (struct/dc s [a integer?] [b (a) (>=/c a)])))

  (test-flat-contract '(and/c number? integer?) 1 3/2)
  (test-flat-contract '(not/c integer?) #t 1)
  (test-flat-contract '(=/c 2) 2 3)
  (test-flat-contract '(>/c 5) 10 5)
  (test-flat-contract '(>=/c 5) 5 0)
  (test-flat-contract '(<=/c 5) 5 10)
  (test-flat-contract '(</c 5) 0 5)
  (test-flat-contract '(=/c 2) 2 0+1i)
  (test-flat-contract '(>/c 5) 10 0+1i)
  (test-flat-contract '(>=/c 5) 5 0+1i)
  (test-flat-contract '(<=/c 5) 5 0+1i)
  (test-flat-contract '(</c 5) 0 0+1i)
  (test-flat-contract '(integer-in 0 10) 0 11)
  (test-flat-contract '(integer-in 0 10) 10 3/2)
  (test-flat-contract '(integer-in 0 10) 1 1.0)
  (test-flat-contract '(real-in 1 10) 3/2 20)
  (test-flat-contract '(string-len/c 3) "ab" "abc")
  (test-flat-contract 'natural-number/c 5 -1)
  (test-flat-contract 'natural-number/c #e3 #i3.0)
  (test-flat-contract 'natural-number/c 0 -1)
  (test-flat-contract 'false/c #f #t)

  (test-flat-contract #t #t "x")
  (test-flat-contract #f #f "x")
  (test-flat-contract #\a #\a #\b)
  (test-flat-contract #\a #\a 'a)
  (test-flat-contract ''a 'a 'b)
  (test-flat-contract ''a 'a #\a)
  (test-flat-contract "x" "x" "y")
  (test-flat-contract "x" "x" 'x)
  (test-flat-contract 1 1 2)
  (test-flat-contract #e1 #i1.0 'x)
  (test-flat-contract #rx".x." "axq" "x")
  (test-flat-contract #rx#".x." #"axq" #"x")
  (test-flat-contract #rx".x." #"axq" #"x")
  (test-flat-contract #rx#".x." "axq" "x")
  (test-flat-contract ''() '() #f)

  (test/spec-passed 'any/c '(contract any/c 1 'pos 'neg))
  (test-flat-contract 'printable/c (vector (cons 1 (box #f))) (lambda (x) x))
  (let ()
    (define-struct s (a b) #:prefab)
    (test-flat-contract 'printable/c (make-s 1 2) (λ (x) x)))
  (test-flat-contract '(symbols 'a 'b 'c) 'a 'd)
  (test-flat-contract '(one-of/c (expt 2 65)) (expt 2 65) 12)
  (test-flat-contract '(one-of/c '#:x '#:z) '#:x '#:y)

  (let ([c% (contract-eval '(class object% (super-new)))])
    (test-flat-contract `(subclass?/c ,c%) c% (contract-eval `object%))
    (test-flat-contract `(subclass?/c ,c%) (contract-eval `(class ,c%)) (contract-eval `(class object%))))

  (let ([i<%> (contract-eval '(interface ()))])
    (test-flat-contract `(implementation?/c ,i<%>)
                        (contract-eval `(class* object% (,i<%>) (super-new)))
                        (contract-eval 'object%))
    (test-flat-contract `(implementation?/c ,i<%>)
                        (contract-eval `(class* object% (,i<%>) (super-new)))
                        #f))

  (let ([i<%> (contract-eval '(interface ()))]
        [c% (contract-eval '(class object% (super-new)))])
    (test-flat-contract `(is-a?/c ,i<%>)
                        (contract-eval `(new (class* object% (,i<%>) (super-new))))
                        (contract-eval '(new object%)))
    (test-flat-contract `(is-a?/c ,c%)
                        (contract-eval `(new ,c%))
                        (contract-eval '(new object%))))

  (test-flat-contract '(listof boolean?) (list #t #f) (list #f 3 #t))
  (test-flat-contract '(listof any/c) (list #t #f) 3)

  (test-flat-contract '(vectorof boolean? #:flat? #t) (vector #t #f) (vector #f 3 #t))
  (test-flat-contract '(vectorof any/c #:flat? #t) (vector #t #f) 3)
  (test-flat-contract '(vector-immutableof boolean?) (vector-immutable #t #f) (vector-immutable #f 3 #t))
  (test-flat-contract '(vector-immutableof any/c) (vector-immutable #t #f) 3)

  (test-flat-contract '(vector/c boolean? (flat-contract integer?) #:flat? #t) (vector #t 1) (vector 1 #f))
  (test-flat-contract '(vector/c boolean? (flat-contract integer?) #:flat? #t) (vector #t 1) #f)
  (test-flat-contract '(vector-immutable/c boolean? (flat-contract integer?))
                      (vector-immutable #t 1) (vector-immutable 1 #f))
  (test-flat-contract '(vector-immutable/c boolean? (flat-contract integer?)) (vector-immutable #t 1) #f)

  (test-flat-contract '(cons/c boolean? (flat-contract integer?)) (cons #t 1) (cons 1 #f))
  (test-flat-contract '(cons/c boolean? (flat-contract integer?)) (cons #t 1) #f)
  (test-flat-contract '(list/c boolean? (flat-contract integer?)) (list #t 1) (list 1 #f))
  (test-flat-contract '(list/c boolean? (flat-contract integer?)) (list #t 1) #f)

  (contract-eval '(define (a-predicate-that-wont-be-optimized x) (boolean? x)))
  (test-flat-contract '(cons/c a-predicate-that-wont-be-optimized (flat-contract integer?)) (cons #t 1) (cons 1 #f))
  (test-flat-contract '(cons/c a-predicate-that-wont-be-optimized (flat-contract integer?)) (cons #t 1) #f)
  (test-flat-contract '(list/c a-predicate-that-wont-be-optimized (flat-contract integer?)) (list #t 1) (list 1 #f))
  (test-flat-contract '(list/c a-predicate-that-wont-be-optimized (flat-contract integer?)) (list #t 1) #f)

  (test-flat-contract '(box/c boolean? #:flat? #t) (box #f) (box 1))
  (test-flat-contract '(box/c (flat-contract boolean?) #:flat? #t) (box #t) #f)
  (test-flat-contract '(box-immutable/c boolean?) (box-immutable #f) (box-immutable 1))
  (test-flat-contract '(box-immutable/c (flat-contract boolean?)) (box-immutable #t) #f)

  (test-flat-contract '(flat-rec-contract sexp (cons/c sexp sexp) number?) '(1 2 . 3) '(1 . #f))
  (test-flat-contract '(flat-murec-contract ([even1 (or/c null? (cons/c number? even2))]
                                             [even2 (cons/c number? even1)])
                                            even1)
                      '(1 2 3 4)
                      '(1 2 3))

  (test-flat-contract '(hash/c symbol? boolean? #:flat? #t) (make-hash) 1)
  (test-flat-contract '(hash/c symbol? boolean? #:flat? #t)
                      (let ([ht (make-hash)])
                        (hash-set! ht 'x #t)
                        ht)
                      (let ([ht (make-hash)])
                        (hash-set! ht 'x 1)
                        ht))
  (test-flat-contract '(hash/c symbol? boolean? #:flat? #t)
                      (let ([ht (make-hash)])
                        (hash-set! ht 'x #t)
                        ht)
                      (let ([ht (make-hash)])
                        (hash-set! ht 'x 1)
                        ht))

  (test #t 'malformed-binder
        (with-handlers ((exn? exn:fail:syntax?))
          (contract-eval '(flat-murec-contract ([(x) y]) x))
          'no-err))
  (test #t 'missing-body
        (with-handlers ((exn? exn:fail:syntax?))
          (contract-eval '(flat-murec-contract ([x y])))
          'no-err))

  ;; test flat-contract-predicate
  (test #t (flat-contract-predicate integer?) 1)
  (test #t (flat-contract-predicate #t) #t)


;
;
;
;    ;;              ;;;                                      ;;;;
;    ;;             ;;;;                                      ;;;;
;       ;;;; ;;;   ;;;;;   ;;;   ;;; ;;; ;;; ;;;   ;;;     ;;;;;;;     ;;;; ;;;  ;;;;;;;  ;;;;;;; ;;;;    ;;;
;  ;;;; ;;;;;;;;;  ;;;;   ;;;;;  ;;;;;;; ;;;;;;;  ;;;;;   ;;;;;;;;     ;;;;;;;;; ;;;;;;;; ;;;;;;;;;;;;;  ;;;;;
;  ;;;; ;;;; ;;;; ;;;;;; ;;;; ;; ;;;; ;; ;;;; ;; ;;;; ;; ;;;;;;;;;     ;;;; ;;;;     ;;;; ;;;; ;;; ;;;; ;;;; ;;
;  ;;;; ;;;; ;;;; ;;;;;; ;;;;;;; ;;;;    ;;;;    ;;;;;;; ;;;; ;;;;     ;;;; ;;;;  ;;;;;;; ;;;; ;;; ;;;; ;;;;;;;
;  ;;;; ;;;; ;;;;  ;;;;  ;;;;;   ;;;;    ;;;;    ;;;;;   ;;;;;;;;;     ;;;; ;;;; ;;  ;;;; ;;;; ;;; ;;;; ;;;;;
;  ;;;; ;;;; ;;;;  ;;;;   ;;;;;; ;;;;    ;;;;     ;;;;;;  ;;;;;;;;     ;;;; ;;;; ;;;;;;;; ;;;; ;;; ;;;;  ;;;;;;
;  ;;;; ;;;; ;;;;  ;;;;    ;;;;  ;;;;    ;;;;      ;;;;    ;;;;;;;     ;;;; ;;;;  ;; ;;;; ;;;; ;;; ;;;;   ;;;;
;
;
;

  (contract-eval
   '(module contract-test-suite-inferred-name1 racket/base
      (require racket/contract)
      (define contract-inferred-name-test-contract (-> integer? any))
      (define (contract-inferred-name-test1 x) #t)
      (provide/contract (contract-inferred-name-test1 contract-inferred-name-test-contract))

      (define (contract-inferred-name-test2 x) x)
      (provide/contract (contract-inferred-name-test2 (-> number? number?)))

      (define (contract-inferred-name-test2b x) (values x x))
      (provide/contract (contract-inferred-name-test2b (-> number? (values number? number?))))

      (define (contract-inferred-name-test3 x . y) x)
      (provide/contract (contract-inferred-name-test3 (->* (number?) () #:rest (listof number?) number?)))

      (define (contract-inferred-name-test4) 7)
      (provide/contract (contract-inferred-name-test4 (->d () () any)))

      (define (contract-inferred-name-test5) 7)
      (provide/contract (contract-inferred-name-test5 (->i () () any)))
      ))
  (contract-eval '(require 'contract-test-suite-inferred-name1))
  (test 'contract-inferred-name-test1 object-name (contract-eval 'contract-inferred-name-test1))
  (test 'contract-inferred-name-test2 object-name (contract-eval 'contract-inferred-name-test2))
  (test 'contract-inferred-name-test2b object-name (contract-eval 'contract-inferred-name-test2b))
  (test 'contract-inferred-name-test3 object-name (contract-eval 'contract-inferred-name-test3))
  (test 'contract-inferred-name-test4 object-name (contract-eval 'contract-inferred-name-test4))
  (test 'contract-inferred-name-test5 object-name (contract-eval 'contract-inferred-name-test5))


;
;
;
;                                 ;                               ;
;                                ;;                              ;;
;    ;;;;;   ;;;;   ;;;; ;;;   ;;;;; ;;; ;;; ;;;;;;;    ;;;;;  ;;;;;      ;;;; ;;;  ;;;;;;;  ;;;;;;; ;;;;    ;;;
;   ;;;;;;  ;;;;;;  ;;;;;;;;; ;;;;;; ;;;;;;; ;;;;;;;;  ;;;;;; ;;;;;;      ;;;;;;;;; ;;;;;;;; ;;;;;;;;;;;;;  ;;;;;
;  ;;;;;;; ;;;;;;;; ;;;; ;;;;  ;;;;  ;;;; ;;     ;;;; ;;;;;;;  ;;;;       ;;;; ;;;;     ;;;; ;;;; ;;; ;;;; ;;;; ;;
;  ;;;;    ;;;; ;;; ;;;; ;;;;  ;;;;  ;;;;     ;;;;;;; ;;;;     ;;;;  ;;;;;;;;; ;;;;  ;;;;;;; ;;;; ;;; ;;;; ;;;;;;;
;  ;;;;;;; ;;;;;;;; ;;;; ;;;;  ;;;;; ;;;;    ;;  ;;;; ;;;;;;;  ;;;;; ;;;;;;;;; ;;;; ;;  ;;;; ;;;; ;;; ;;;; ;;;;;
;   ;;;;;;  ;;;;;;  ;;;; ;;;;  ;;;;; ;;;;    ;;;;;;;;  ;;;;;;  ;;;;;      ;;;; ;;;; ;;;;;;;; ;;;; ;;; ;;;;  ;;;;;;
;    ;;;;;   ;;;;   ;;;; ;;;;   ;;;; ;;;;     ;; ;;;;   ;;;;;   ;;;;      ;;;; ;;;;  ;; ;;;; ;;;; ;;; ;;;;   ;;;;
;
;
;


  (test-name 'integer? (flat-contract integer?))
  (test-name 'boolean? (flat-contract boolean?))
  (test-name 'char? (flat-contract char?))
  (test-name 'any/c any/c)

  (test-name 'mumble (let ([frotz/c integer?]
                           [bazzle/c boolean?])
                       (flat-named-contract 'mumble
                                            (and/c frotz/c
                                                   (not/c bazzle/c)))))

  
  (test-name '(-> integer? integer?) (-> integer? integer?))
  (test-name '(-> integer? any) (-> integer? any))
  (test-name '(-> integer? (values boolean? char?)) (-> integer? (values boolean? char?)))
  (test-name '(-> integer? boolean? (values char? any/c)) (->* (integer? boolean?) () (values char? any/c)))
  (test-name '(-> integer? boolean? any) (->* (integer? boolean?) () any))
  (test-name '(-> integer? boolean? #:x string? any) (-> integer? #:x string? boolean? any))

  (test-name '(->* (integer?) (string?) #:rest any/c (values char? any/c))
              (->* (integer?) (string?) #:rest any/c (values char? any/c)))
  (test-name '(->* (integer? char?) (boolean?) any) (->* (integer? char?) (boolean?) any))
  (test-name '(->* (integer? char? #:z string?) (integer?) any) (->* (#:z string? integer? char?) (integer?) any))
  (test-name '(->* (integer? char? #:z string?) (boolean? #:i number?) any) (->* (#:z string? integer? char?) (boolean? #:i number?) any))
  (test-name '(->* (integer? char? #:z string?) (boolean? #:i number?) #:rest (listof integer?) any)
              (->* (#:z string? integer? char?) (boolean? #:i number?) #:rest (listof integer?) any))
  (test-name '(->* (integer? char? #:z string?) (boolean? #:i number?) (values number? boolean? symbol?))
              (->* (#:z string? integer? char?) (boolean? #:i number?) (values number? boolean? symbol?)))
  (test-name '(->* (integer? char? #:z string?) (boolean? #:i number?) #:rest (listof integer?) (values number? boolean? symbol?))
              (->* (#:z string? integer? char?) (boolean? #:i number?) #:rest (listof integer?) (values number? boolean? symbol?)))

  (test-name '(->* (integer?) #:pre ... integer?)
			  (->* (integer?) () #:pre (= 1 2) integer?))
  (test-name '(->* (integer?) integer? #:post ...)
			  (->* (integer?) () integer? #:post #f))
  (test-name '(->* (integer?) #:pre ... integer? #:post ...)
			  (->* (integer?) () #:pre (= 1 2) integer? #:post #f))

  (test-name '(->d () () any) (->d () () any))
  (test-name '(->d ([x ...] #:y [y ...]) ([z ...] #:w [w ...]) any) (->d ([x integer?] #:y [y integer?]) ([z integer?] #:w [w integer?]) any))
  (test-name '(->d () () (values [x ...] [y ...])) (->d () () (values [x number?] [y number?])))
  (test-name '(->d () () [x ...]) (->d () () [q number?]))
  (test-name '(->d () () #:pre ... [x ...]) (->d () () #:pre #t [q number?]))
  (test-name '(->d () () #:pre ... [x ...] #:post ...) (->d () () #:pre #t [q number?] #:post #t))
  (test-name '(->d () () [x ...] #:post ...) (->d () () [q number?] #:post #t))

  (test-name '(->i () any) (->i () () any))
  (test-name '(->i () any) (->i () any))
  (test-name '(->i () [x () ...])
              (->i () () [x () number?]))
  (test-name '(->i () [q number?])
              (->i () () [q number?]))
  (test-name '(->i () (values [x number?] [y number?]))
              (->i () (values [x number?] [y number?])))
  (test-name '(->i () (values [x (y) ...] [y number?]))
              (->i () (values [x (y) number?] [y number?])))
  (test-name '(->i ([x integer?] #:y [y integer?]) ([z integer?] #:w [w integer?]) any)
              (->i ([x integer?] #:y [y integer?]) ([z integer?] #:w [w integer?]) any))
  (test-name '(->i () #:pre () ... [q number?])
              (->i () #:pre () #t  [q number?]))
  (test-name '(->i () #:pre () ... [q () ...] #:post () ...)
              (->i () #:pre () #t  [q () number?] #:post () #t))
  (test-name '(->i ([x integer?]) #:pre (x) ... [q (x) ...]     #:post (x) ...)
              (->i ([x integer?]) #:pre (x) #t  [q (x) number?] #:post (x) #t))
  (test-name '(->i ([x real?]) [_ (x) ...])
              (->i ([x real?]) [_ (x) (>/c x)]))
  (test-name '(->i ([x any/c]) #:pre/name (x) "pair" ... #:pre/name (x) "car" ... any)
              (->i ([x any/c]) #:pre/name (x) "pair" (pair? x) #:pre/name (x) "car" (car x) any))
  (test-name '(->i ([x any/c]) [y () ...] #:post/name (y) "pair" ... #:post/name (y) "car" ...)
              (->i ([x any/c]) [y () any/c] #:post/name (y) "pair" (pair? y) #:post/name (y) "car" (car y)))

  (test-name '(case->) (case->))
  (test-name '(case-> (-> integer? any) (-> boolean? boolean? any) (-> char? char? char? any))
             (case-> (-> integer? any) (-> boolean? boolean? any) (-> char? char? char? any)))
  (test-name '(case-> (-> integer? integer?) (-> integer? integer? integer?))
             (case-> (-> integer? integer?) (-> integer? integer? integer?)))
  (test-name '(case-> (-> integer? #:rest any/c any)) (case-> (-> integer? #:rest any/c any)))
  (test-name '(case-> (-> integer? #:rest any/c (values boolean? char? number?)))
             (case-> (-> integer? #:rest any/c (values boolean? char? number?))))
  (test-name '(case-> (-> integer? (values))) (case-> (-> integer? (values))))

  (test-name '(unconstrained-domain-> number?) (unconstrained-domain-> number?))

  (test-name '(or/c) (or/c))
  (test-name '(or/c integer? gt0?) (or/c integer? (let ([gt0? (lambda (x) (> x 0))]) gt0?)))
  (test-name '(or/c integer? boolean?)
             (or/c (flat-contract integer?)
                   (flat-contract boolean?)))
  (test-name '(or/c integer? boolean?)
             (or/c integer? boolean?))
  (test-name '(or/c (-> (>=/c 5) (>=/c 5)) boolean?)
             (or/c (-> (>=/c 5) (>=/c 5)) boolean?))
  (test-name '(or/c (-> (>=/c 5) (>=/c 5)) boolean?)
             (or/c boolean? (-> (>=/c 5) (>=/c 5))))
  (test-name '(or/c (-> (>=/c 5) (>=/c 5))
                    (-> (<=/c 5) (<=/c 5) (<=/c 5)))
             (or/c (-> (>=/c 5) (>=/c 5))
                   (-> (<=/c 5) (<=/c 5) (<=/c 5))))
  (test-name '(or/c boolean?
                    (-> (>=/c 5) (>=/c 5))
                    (-> (<=/c 5) (<=/c 5) (<=/c 5)))
             (or/c boolean?
                   (-> (>=/c 5) (>=/c 5))
                   (-> (<=/c 5) (<=/c 5) (<=/c 5))))

  (test-name 'any/c (and/c))
  (test-name '(and/c any/c) (and/c any/c))
  (test-name '(and/c any/c any/c) (and/c any/c any/c))
  (test-name '(and/c number? integer?) (and/c number? integer?))
  (test-name '(and/c number? integer?) (and/c (flat-contract number?)
                                              (flat-contract integer?)))
  (test-name '(and/c number? (-> integer? integer?)) (and/c number? (-> integer? integer?)))
  (test-name '(and/c (-> boolean? boolean?) (-> integer? integer?)) (and/c (-> boolean? boolean?) (-> integer? integer?)))

  (test-name '(not/c integer?) (not/c integer?))
  (test-name '(=/c 5) (=/c 5))
  (test-name '(>=/c 5) (>=/c 5))
  (test-name '(<=/c 5) (<=/c 5))
  (test-name '(</c 5) (</c 5))
  (test-name '(>/c 5) (>/c 5))
  (test-name '(between/c 5 6) (between/c 5 6))
  (test-name '(between/c -inf.0 +inf.0) (between/c -inf.0 +inf.0))
  (test-name '(integer-in 0 10) (integer-in 0 10))
  (test-name '(between/c 1 10) (real-in 1 10))
  (test-name '(string-len/c 3) (string-len/c 3))
  (test-name 'natural-number/c natural-number/c)
  (test-name #f false/c)
  (test-name #t #t)
  (test-name #\a #\a)
  (test-name "x" "x")
  (test-name ''x 'x)
  (test-name #rx"x" #rx"x")
  (test-name #rx#"x" #rx#"x")
  (test-name 'printable/c printable/c)
  (test-name '(or/c 'a 'b 'c) (symbols 'a 'b 'c))
  (test-name '(or/c 1 2 3) (one-of/c 1 2 3))
  (test-name '(one-of/c '() 'x 1 #f #\a (void) (letrec ([x x]) x))
             (one-of/c '() 'x 1 #f #\a (void) (letrec ([x x]) x)))

  (test-name '(or/c #f #t #\a "x") (or/c #f #t #\a "x"))
  (test-name '(or/c #f #t #\a "x" #rx"x" #rx#"x") (or/c #f #t #\a "x" #rx"x" #rx#"x"))

  (test-name '(subclass?/c c%)
             (let ([c% (class object% (super-new))]) (subclass?/c c%)))

  (test-name '(implementation?/c i<%>)
             (let ([i<%> (interface ())])
               (implementation?/c i<%>)))

  (test-name '(is-a?/c i<%>)
             (let ([i<%> (interface ())])
               (is-a?/c i<%>)))
  (test-name '(is-a?/c c%)
             (let ([i<%> (interface ())]
                   [c% (class object% (super-new))])
               (is-a?/c c%)))

  (test-name '(listof boolean?) (listof boolean?))
  (test-name '(listof any/c) (listof any/c))
  (test-name '(listof boolean?) (listof boolean?))
  (test-name '(listof any/c) (listof any/c))
  (test-name '(listof boolean?) (listof boolean?))
  (test-name '(listof (-> boolean? boolean?)) (listof (-> boolean? boolean?)))

  (test-name '(non-empty-listof boolean?) (non-empty-listof boolean?))
  (test-name '(non-empty-listof any/c) (non-empty-listof any/c))
  (test-name '(non-empty-listof boolean?) (non-empty-listof boolean?))
  (test-name '(non-empty-listof any/c) (non-empty-listof any/c))
  (test-name '(non-empty-listof boolean?) (non-empty-listof boolean?))
  (test-name '(non-empty-listof (-> boolean? boolean?)) (non-empty-listof (-> boolean? boolean?)))


  (test-name '(vectorof boolean?) (vectorof boolean?))
  (test-name '(vectorof any/c) (vectorof any/c))

  (test-name '(vector/c boolean? integer?) (vector/c boolean? integer?))
  (test-name '(vector/c boolean? integer?) (vector/c boolean? (flat-contract integer?)))

  (test-name '(cons/c boolean? integer?) (cons/c boolean? (flat-contract integer?)))
  (test-name '(cons/c boolean? integer?) (cons/c boolean? (flat-contract integer?)))
  (test-name '(list/c boolean? integer?) (list/c boolean? (flat-contract integer?)))
  (test-name '(list/c boolean? integer?) (list/c boolean? (flat-contract integer?)))

  (test-name '(cons/c boolean? integer?) (cons/c boolean? (flat-contract integer?)))
  (test-name '(cons/c boolean? integer?) (cons/c boolean? (flat-contract integer?)))
  (test-name '(cons/c boolean? integer?) (cons/c boolean? (flat-contract integer?)))
  (test-name '(cons/c (-> boolean? boolean?) integer?) (cons/c (-> boolean? boolean?) integer?))

  (test-name '(list/c boolean? integer?)
             (list/c boolean? (flat-contract integer?)))
  (test-name '(list/c boolean? integer?)
             (list/c boolean? (flat-contract integer?)))
  (test-name '(list/c boolean? integer?)
             (list/c boolean? (flat-contract integer?)))
  (test-name '(list/c (-> boolean? boolean?) integer?)
             (list/c (-> boolean? boolean?) integer?))

  (test-name '(parameter/c integer?) (parameter/c integer?))
  (test-name '(parameter/c integer? string?) (parameter/c integer? string?))

  (test-name '(hash/c symbol? boolean?) (hash/c symbol? boolean?))
  (test-name '(hash/c symbol? boolean? #:immutable #t) (hash/c symbol? boolean? #:immutable #t))
  (test-name '(hash/c symbol? boolean? #:immutable #f) (hash/c symbol? boolean? #:immutable #f))
  (test-name '(hash/c symbol? boolean?) (hash/c symbol? boolean? #:immutable 'dont-care))

  (test-name '(box/c boolean?) (box/c boolean?))
  (test-name '(box/c boolean?) (box/c (flat-contract boolean?)))
  (test-name 'the-name (flat-rec-contract the-name))

  (test-name '(object-contract) (object-contract))
  (test-name '(object-contract (field x integer?)) (object-contract (field x integer?)))
  (test-name '(object-contract (m (-> integer? integer?)))
             (object-contract (m (-> integer? integer?))))
  (test-name '(object-contract (m (-> integer? any)))
             (object-contract (m (-> integer? any))))
  (test-name '(object-contract (m (-> integer? (values integer? integer?))))
             (object-contract (m (-> integer? (values integer? integer?)))))
  (test-name '(object-contract (m (case-> (-> integer? integer? integer?)
                                          (-> integer? (values integer? integer?)))))
             (object-contract (m (case->
                                  (-> integer? integer? integer?)
                                  (-> integer? (values integer? integer?))))))
  (test-name
   '(object-contract (m (->* (integer?) (boolean? number?) symbol?)))
   (object-contract (m (->* (integer?) (boolean? number?) symbol?))))

  (test-name '(object-contract (m (->d ((x ...)) () (y ...)))) (object-contract (m (->d ((x number?)) () [result number?]))))
  (test-name '(object-contract (m (->d ((x ...) (y ...) (z ...)) () [w ...])))
             (object-contract (m (->d ((x number?) (y boolean?) (z pair?)) () [result number?]))))
  (test-name '(object-contract (m (->d ((x ...) (y ...) (z ...)) () #:rest w ... [x0 ...])))
             (object-contract (m (->d ((x number?) (y boolean?) (z pair?)) () #:rest rest-x any/c [result number?]))))

  (test-name '(object-contract (m (->i ((x number?)) (result number?))))
	     (object-contract (m (->i ((x number?)) () [result number?]))))
  (test-name '(object-contract (m (->i ((x number?) (y boolean?) (z pair?)) [result number?])))
             (object-contract (m (->i ((x number?) (y boolean?) (z pair?)) () [result number?]))))
  (test-name '(object-contract (m (->i ((x number?) (y boolean?) (z pair?)) #:rest [rest-x any/c] [result number?])))
             (object-contract (m (->i ((x number?) (y boolean?) (z pair?)) () #:rest [rest-x any/c] [result number?]))))

  (test-name '(promise/c any/c) (promise/c any/c))
  (test-name '(syntax/c any/c) (syntax/c any/c))
  (test-name '(struct/c st integer?)
             (let ()
               (define-struct st (a))
               (struct/c st integer?)))

  (test-name '(recursive-contract (box/c boolean?)) (recursive-contract (box/c boolean?)))
  (test-name '(recursive-contract x) (let ([x (box/c boolean?)]) (recursive-contract x)))

  (test-name '(couple/c any/c any/c)
             (couple/c any/c any/c))
  (test-name '(couple/c any/c any/c)
             (couple/dc [hd any/c] [tl any/c]))
  (test-name '(couple/dc [hd any/c] [tl ...])
             (couple/dc [hd any/c] [tl (hd) any/c]))

  (test-name '(set/c integer?) (set/c integer?))
  (test-name '(set/c boolean? #:cmp 'equal) (set/c boolean? #:cmp 'equal))
  (test-name '(set/c char? #:cmp 'eq) (set/c char? #:cmp 'eq))
  (test-name '(set/c (set/c char?) #:cmp 'eqv) (set/c (set/c char? #:cmp 'dont-care) #:cmp 'eqv))
  (test-name '(set/c (-> char? char?) #:cmp 'equal) (set/c (-> char? char?) #:cmp 'equal))

  (test-name '(class/c [m (->m integer? integer?)]) (class/c [m (->m integer? integer?)]))
  (test-name '(class/c [m (->*m (integer?) (integer?) integer?)]) (class/c [m (->*m (integer?) (integer?) integer?)]))
  (test-name '(class/c [m (case->m (-> integer? integer?) (-> integer? integer? integer?))])
             (class/c [m (case->m (-> integer? integer?) (-> integer? integer? integer?))]))
  (test-name '(class/c [m (->dm ((x ...)) () (y ...))]) (class/c [m (->dm ([d integer?]) () [r integer?])]))
  (test-name 'c%/c (let ([c%/c (class/c [m (->m integer? integer?)])])
                     c%/c))

  (test-name '(struct/dc s
                         [a integer?]
                         [b symbol?]
                         [c (a b) ...]
                         [d (a b c) ...])
             (let ()
               (struct s (a b c d))
               (struct/dc s
                          [a integer?]
                          [b symbol?]
                          [c (a b) boolean?]
                          [d (a b c) integer?])))

  (test-name '(struct/dc s
                         [a integer?]
                         [b #:lazy symbol?]
                         [c (a) ...]
                         [d (a c) ...])
             (let ()
               (struct s (a b c d))
               (struct/dc s
                          [a integer?]
                          [b #:lazy symbol?]
                          [c (a) boolean?]
                          [d (a c) integer?])))

  ;; NOT YET RELEASED
  #;
  (test-name '(pr/dc [x integer?]
                     [y integer?]
                     where
                     [x-val ...]
                     [y-val ...]
                     and
                     ...)
             (let ()
               (define-contract-struct pr (x y))
               (pr/dc [x integer?]
                      [y integer?]
                      where
                      [x-val x]
                      [y-val y]
                      and
                      (= x-val y-val))))


;
;
;
;             ;
;            ;;
;   ;;;;;  ;;;;; ;;; ;;;   ;;;;   ;;;; ;;;   ;;;;;;;   ;;;   ;;; ;;;
;  ;;;;;; ;;;;;; ;;;;;;;  ;;;;;;  ;;;;;;;;; ;;;;;;;;  ;;;;;  ;;;;;;;
;  ;;;;    ;;;;  ;;;; ;; ;;;;;;;; ;;;; ;;;; ;;; ;;;; ;;;; ;; ;;;; ;;
;   ;;;;   ;;;;  ;;;;    ;;;; ;;; ;;;; ;;;; ;;;;;;;; ;;;;;;; ;;;;
;    ;;;;  ;;;;; ;;;;    ;;;;;;;; ;;;; ;;;;  ;;;;;;; ;;;;;   ;;;;
;  ;;;;;;  ;;;;; ;;;;     ;;;;;;  ;;;; ;;;; ;   ;;;;  ;;;;;; ;;;;
;  ;;;;;    ;;;; ;;;;      ;;;;   ;;;; ;;;; ;;;;;;;;   ;;;;  ;;;;
;                                           ;;;;;;;;
;                                            ;;;;;;
;

  (ctest #t contract-stronger? any/c any/c)
  (ctest #t contract-stronger? (between/c 1 3) (between/c 0 4))
  (ctest #f contract-stronger? (between/c 0 4) (between/c 1 3))
  (ctest #t contract-stronger? (>=/c 3) (>=/c 2))
  (ctest #f contract-stronger? (>=/c 2) (>=/c 3))
  (ctest #f contract-stronger? (<=/c 3) (<=/c 2))
  (ctest #t contract-stronger? (<=/c 2) (<=/c 3))
  (ctest #f contract-stronger? (recursive-contract (<=/c 2)) (recursive-contract (<=/c 3)))
  (ctest #f contract-stronger? (recursive-contract (<=/c 3)) (recursive-contract (<=/c 2)))
  (let ([f (contract-eval '(λ (x) (recursive-contract (<=/c x))))])
    (test #t (contract-eval 'contract-stronger?) (contract-eval `(,f 1)) (contract-eval `(,f 1))))
  (ctest #t contract-stronger? (-> integer? integer?) (-> integer? integer?))
  (ctest #f contract-stronger? (-> boolean? boolean?) (-> integer? integer?))
  (ctest #t contract-stronger? (-> (>=/c 3) (>=/c 3)) (-> (>=/c 4) (>=/c 3)))
  (ctest #f contract-stronger? (-> (>=/c 4) (>=/c 3)) (-> (>=/c 3) (>=/c 3)))
  (ctest #t contract-stronger? (-> (>=/c 3) (>=/c 3)) (-> (>=/c 3) (>=/c 2)))
  (ctest #f contract-stronger? (-> (>=/c 3) (>=/c 2)) (-> (>=/c 3) (>=/c 3)))
  (ctest #f contract-stronger? (-> (>=/c 2)) (-> (>=/c 3) (>=/c 3)))
  (ctest #f contract-stronger? (-> integer? #:x integer? integer?) (-> integer? #:y integer? integer?))
  (ctest #f contract-stronger? (-> integer? #:y integer? integer?) (-> integer? #:x integer? integer?))
  (ctest #t contract-stronger? (-> integer? #:x integer? integer?) (-> integer? #:x integer? integer?))
  (ctest #t contract-stronger? (-> #:x (>=/c 3) (>=/c 3)) (-> #:x (>=/c 3) (>=/c 2)))

  (let ([c (contract-eval '(->* () () any))])
    (test #t (contract-eval 'contract-stronger?) c c))
  (let ([c (contract-eval '(->d () () any))])
    (test #t (contract-eval 'contract-stronger?) c c))
  (let ([c (contract-eval '(->i () () any))])
    (test #t (contract-eval 'contract-stronger?) c c))

  (ctest #t contract-stronger? (or/c null? any/c) (or/c null? any/c))
  (ctest #f contract-stronger? (or/c null? any/c) (or/c boolean? any/c))
  (ctest #t contract-stronger? (or/c null? boolean?) (or/c null? boolean?))
  (ctest #f contract-stronger? (or/c null? boolean?) (or/c boolean? null?))
  (ctest #t contract-stronger? (or/c null? (-> integer? integer?)) (or/c null? (-> integer? integer?)))
  (ctest #f contract-stronger? (or/c null? (-> boolean? boolean?)) (or/c null? (-> integer? integer?)))

  (ctest #t contract-stronger? number? number?)
  (ctest #f contract-stronger? boolean? number?)

  (ctest #t contract-stronger? (parameter/c (between/c 0 5)) (parameter/c (between/c 0 5)))
  (ctest #f contract-stronger? (parameter/c (between/c 0 5)) (parameter/c (between/c 1 4)))
  (ctest #f contract-stronger? (parameter/c (between/c 1 4)) (parameter/c (between/c 0 5)))

  (ctest #f contract-stronger? (parameter/c (between/c 1 4) (between/c 0 5))
                               (parameter/c (between/c 0 5)))
  (ctest #t contract-stronger? (parameter/c (between/c 0 5) (between/c 1 4))
                               (parameter/c (between/c 1 4)))
  (ctest #t contract-stronger? (parameter/c (between/c 0 5))
                               (parameter/c (between/c 1 4) (between/c 0 5)))
  (ctest #f contract-stronger? (parameter/c (between/c 1 4))
                               (parameter/c (between/c 0 5) (between/c 0 5)))
  (ctest #t contract-stronger? (parameter/c (between/c 0 5) (between/c 1 4))
                               (parameter/c (between/c 1 4) (between/c 0 5)))
  (ctest #f contract-stronger? (parameter/c (between/c 1 4) (between/c 0 5))
                               (parameter/c (between/c 0 5) (between/c 1 4)))

  (ctest #t contract-stronger? (symbols 'x 'y) (symbols 'x 'y 'z))
  (ctest #f contract-stronger? (symbols 'x 'y 'z) (symbols 'x 'y))
  (ctest #t contract-stronger? (symbols 'x 'y) (symbols 'z 'x 'y))
  (ctest #f contract-stronger? (symbols 'z 'x 'y) (symbols 'x 'y))
  (ctest #t contract-stronger? (one-of/c (expt 2 100)) (one-of/c (expt 2 100) 12))

  (ctest #t contract-stronger?
        (or/c (-> (>=/c 3) (>=/c 3)) (-> string?))
        (or/c (-> (>=/c 4) (>=/c 3)) (-> string?)))
  (ctest #f contract-stronger?
        (or/c (-> string?) (-> integer? integer?))
        (or/c (-> string?) (-> any/c integer?)))
  (ctest #f contract-stronger?
        (or/c (-> string?) (-> any/c integer?))
        (or/c (-> string?) (-> integer? integer?)))
  (ctest #t contract-stronger?
        (or/c (-> string?) (-> integer? integer?) integer? boolean?)
        (or/c (-> string?) (-> integer? integer?) integer? boolean?))
  (ctest #f contract-stronger?
        (or/c (-> string?) (-> integer? integer?) integer? char?)
        (or/c (-> string?) (-> integer? integer?) integer? boolean?))
  (ctest #f contract-stronger?
        (or/c (-> string?) (-> integer? integer?) integer?)
        (or/c (-> string?) (-> integer? integer?) integer? boolean?))
  (ctest #f contract-stronger?
        (or/c (-> string?) (-> integer? integer?) integer?)
        (or/c (-> integer? integer?) integer?))

  (contract-eval
   `(let ()
      (define (non-zero? x) (not (zero? x)))
      (define list-of-numbers
        (or/c null?
              (couple/c number?
                        (recursive-contract list-of-numbers))))
      (define (short-list/less-than n)
        (or/c null?
              (couple/c (<=/c n)
                        (or/c null?
                              (couple/c (<=/c n)
                                        any/c)))))
      (define (short-sorted-list/less-than n)
        (or/c null?
              (couple/dc
               [hd (<=/c n)]
               [tl (hd) (or/c null?
                              (couple/c (<=/c hd)
                                        any/c))])))

      (define (sorted-list/less-than n)
        (or/c null?
              (couple/dc
               [hd (<=/c n)]
               [tl (hd) (sorted-list/less-than hd)])))

      ;; for some reason, the `n' makes it harder to optimize. without it, this test isn't as good a test
      (define (closure-comparison-test n)
        (couple/dc
         [hd any/c]
         [tl (hd) any/c]))

      (,test #t contract-stronger? (couple/c any/c any/c) (couple/c any/c any/c))
      (,test #f contract-stronger? (couple/c (>=/c 2) (>=/c 3)) (couple/c (>=/c 4) (>=/c 5)))
      (,test #t contract-stronger? (couple/c (>=/c 4) (>=/c 5)) (couple/c (>=/c 2) (>=/c 3)))
      (,test #f contract-stronger? (couple/c (>=/c 1) (>=/c 5)) (couple/c (>=/c 5) (>=/c 1)))
      (let ([ctc (couple/dc [hd any/c] [tl (hd) any/c])])
        (,test #t contract-stronger? ctc ctc))
      (let ([ctc (couple/dc [hd any/c] [tl (hd) (<=/c hd)])])
        (,test #t contract-stronger? ctc ctc))
      (,test #t contract-stronger? list-of-numbers list-of-numbers)
      (,test #t contract-stronger? (short-list/less-than 4) (short-list/less-than 5))
      (,test #f contract-stronger? (short-list/less-than 5) (short-list/less-than 4))
      (,test #t contract-stronger? (short-sorted-list/less-than 4) (short-sorted-list/less-than 5))
      (,test #f contract-stronger? (short-sorted-list/less-than 5) (short-sorted-list/less-than 4))
      (,test #t contract-stronger? (sorted-list/less-than 4) (sorted-list/less-than 5))
      (,test #f contract-stronger? (sorted-list/less-than 5) (sorted-list/less-than 4))
      (,test #t contract-stronger? (closure-comparison-test 4) (closure-comparison-test 5))

      (letrec ([mk-c
                (λ (x)
                  (triple/dc [a (<=/c x)]
                             [b any/c]
                             [c (a b) (or/c #f (mk-c a))]))])
        (,test #t contract-stronger? (mk-c 1) (mk-c 2)))))


  (contract-eval
   `(let ()

      (struct s (a b))
      (struct t (a b))

      (,test #f contract-stronger?
             (struct/dc s
                        [a (>=/c 1)]
                        [b (>=/c 2)])
             (struct/dc s
                        [a (>=/c 2)]
                        [b (>=/c 3)]))
      (,test #t contract-stronger?
              (struct/dc s
                         [a (>=/c 2)]
                         [b (>=/c 3)])
              (struct/dc s
                         [a (>=/c 1)]
                         [b (>=/c 2)]))

      (,test #f contract-stronger?
             (struct/dc s
                    [a number?]
                    [b number?])
             (struct/dc t
                        [a number?]
                        [b number?]))

      (,test #f contract-stronger?
             (struct/dc t
                        [a number?]
                        [b number?])
             (struct/dc s
                        [a number?]
                        [b number?]))

      (define (mk c)
        (struct/dc s
                   [a (>=/c c)]
                   [b (a) (>=/c a)]))
      (define one (mk 1))
      (define two (mk 2))
      (,test #f contract-stronger? one two)
      (,test #t contract-stronger? two one)))





;
;
;
;     ;;;   ;;                    ;                             ;;;;
;    ;;;;   ;;                   ;;                             ;;;;
;   ;;;;;      ;;; ;;;  ;;;;;  ;;;;;        ;;;;   ;;; ;;;   ;;;;;;;   ;;;   ;;; ;;;
;   ;;;;  ;;;; ;;;;;;; ;;;;;; ;;;;;;       ;;;;;;  ;;;;;;;  ;;;;;;;;  ;;;;;  ;;;;;;;
;  ;;;;;; ;;;; ;;;; ;; ;;;;    ;;;;       ;;;;;;;; ;;;; ;; ;;;;;;;;; ;;;; ;; ;;;; ;;
;  ;;;;;; ;;;; ;;;;     ;;;;   ;;;;  ;;;;;;;;; ;;; ;;;;    ;;;; ;;;; ;;;;;;; ;;;;
;   ;;;;  ;;;; ;;;;      ;;;;  ;;;;; ;;;;;;;;;;;;; ;;;;    ;;;;;;;;; ;;;;;   ;;;;
;   ;;;;  ;;;; ;;;;    ;;;;;;  ;;;;;       ;;;;;;  ;;;;     ;;;;;;;;  ;;;;;; ;;;;
;   ;;;;  ;;;; ;;;;    ;;;;;    ;;;;        ;;;;   ;;;;      ;;;;;;;   ;;;;  ;;;;
;
;
;

  (ctest #t contract-first-order-passes? (flat-contract integer?) 1)
  (ctest #f contract-first-order-passes? (flat-contract integer?) 'x)
  (ctest #t contract-first-order-passes? (flat-contract boolean?) #t)
  (ctest #f contract-first-order-passes? (flat-contract boolean?) 'x)
  (ctest #t contract-first-order-passes? any/c 1)
  (ctest #t contract-first-order-passes? any/c #t)
  (ctest #t contract-first-order-passes? (-> integer? integer?) (λ (x) #t))
  (ctest #f contract-first-order-passes? (-> integer? integer?) (λ (x y) #t))
  (ctest #f contract-first-order-passes? (-> integer? integer?) 'x)
  (ctest #t contract-first-order-passes? (-> integer? boolean? integer?) (λ (x y) #t))
  (ctest #f contract-first-order-passes? (-> integer? boolean? integer?) (λ (x) #t))
  (ctest #f contract-first-order-passes? (-> integer? boolean? integer?) (λ (x y z) #t))
  (ctest #f contract-first-order-passes? (-> integer? boolean? #:x integer? integer?) (λ (x y) #t))
  (ctest #t contract-first-order-passes? (-> integer? boolean? #:x integer? integer?) (λ (x y #:x z) #t))

  (ctest #t contract-first-order-passes? (->* (integer?) () #:rest any/c (values char? any/c)) (λ (x . y) #f))
  (ctest #f contract-first-order-passes? (->* (integer?) () #:rest any/c (values char? any/c)) (λ (x y . z) #f))
  (ctest #f contract-first-order-passes? (->* (integer?) () #:rest any/c (values char? any/c)) (λ (x) #f))
  (ctest #t contract-first-order-passes? (->* (integer?) () #:rest any/c (values char? any/c)) (λ x #f))

  (ctest #t contract-first-order-passes? (->d ((z any/c)) () (result any/c)) (λ (x) x))
  (ctest #f contract-first-order-passes? (->d ((z any/c)) () (result any/c)) (λ (x y) x))

  (ctest #t contract-first-order-passes? (->i ((z any/c)) () (result any/c)) (λ (x) x))
  (ctest #f contract-first-order-passes? (->i ((z any/c)) () (result any/c)) (λ (x y) x))

  (ctest #t contract-first-order-passes? (listof integer?) (list 1))
  (ctest #f contract-first-order-passes? (listof integer?) #f)

  (ctest #f contract-first-order-passes? (list/c #f #f #t) (list))
  (ctest #t contract-first-order-passes? (list/c #f 'x #t) (list #f 'x #t))
  (ctest #f contract-first-order-passes? (list/c (-> number? number?)) (list (λ (x y) x)))
  (ctest #t contract-first-order-passes? (list/c (-> number? number?)) (list (λ (x) x)))

  (ctest #t contract-first-order-passes? (non-empty-listof integer?) (list 1))
  (ctest #f contract-first-order-passes? (non-empty-listof integer?) (list))


  (ctest #t contract-first-order-passes? (vector-immutableof integer?) (vector->immutable-vector (vector 1)))
  (ctest #f contract-first-order-passes? (vector-immutableof integer?) 'x)
  (ctest #f contract-first-order-passes? (vector-immutableof integer?) '())

  (ctest #t contract-first-order-passes? (promise/c integer?) (delay 1))
  (ctest #f contract-first-order-passes? (promise/c integer?) 1)

  (ctest #t contract-first-order-passes? (and/c (-> positive? positive?) (-> integer? integer?)) (λ (x) x))
  (ctest #t contract-first-order-passes? (and/c (-> positive? positive?) (-> integer? integer?)) values)
  (ctest #f contract-first-order-passes? (and/c (-> integer?) (-> integer? integer?)) (λ (x) x))

  (ctest #t contract-first-order-passes?
        (cons/c boolean? (-> integer? integer?))
        (list* #t (λ (x) x)))
  (ctest #f contract-first-order-passes?
        (cons/c boolean? (-> integer? integer?))
        (list* 1 2))

  (ctest #f contract-first-order-passes? (flat-rec-contract the-name) 1)

  (ctest #t contract-first-order-passes?
         (couple/c any/c any/c)
         (make-couple 1 2))

  (ctest #f contract-first-order-passes?
         (couple/c any/c any/c)
         2)

  (ctest #t contract-first-order-passes?
         (couple/dc [hd any/c] [tl any/c])
         (make-couple 1 2))

  (ctest #f contract-first-order-passes?
         (couple/dc [hd any/c] [tl any/c])
         1)

  (ctest #t contract-first-order-passes?
         (couple/dc [hd any/c] [tl (hd) any/c])
         (make-couple 1 2))

  (ctest #f contract-first-order-passes?
         (couple/dc [hd any/c] [tl (hd) any/c])
         1)

  (ctest #t contract-first-order-passes? (or/c (-> (>=/c 5) (>=/c 5)) boolean?) #t)
  (ctest #t contract-first-order-passes? (or/c (-> (>=/c 5) (>=/c 5)) boolean?) (λ (x) x))
  (ctest #f contract-first-order-passes? (or/c (-> (>=/c 5) (>=/c 5)) boolean?) 'x)

  (ctest #t contract-first-order-passes?
        (or/c (-> integer? integer? integer?)
              (-> integer? integer?))
        (λ (x) x))
  (ctest #t contract-first-order-passes?
        (or/c (-> integer? integer? integer?)
              (-> integer? integer?))
        (λ (x y) x))
  (ctest #f contract-first-order-passes?
        (or/c (-> integer? integer? integer?)
              (-> integer? integer?))
        (λ () x))
  (ctest #f contract-first-order-passes?
        (or/c (-> integer? integer? integer?)
              (-> integer? integer?))
        1)

  (ctest #t contract-first-order-passes? (hash/c any/c any/c) (make-hash))
  (ctest #f contract-first-order-passes? (hash/c any/c any/c) #f)
  (ctest #f contract-first-order-passes? (hash/c symbol? boolean?) (let ([ht (make-hash)])
                                                                     (hash-set! ht 'x 1)
                                                                     ht))
  (ctest #f contract-first-order-passes? (hash/c symbol? boolean? #:flat? #t)
         (let ([ht (make-hash)]) (hash-set! ht 'x 1) ht))
  (ctest #f contract-first-order-passes? (hash/c symbol? boolean?) (let ([ht (make-hash)])
                                                                     (hash-set! ht 1 #f)
                                                                     ht))
  (ctest #f contract-first-order-passes? (hash/c symbol? boolean? #:flat? #t)
         (let ([ht (make-hash)]) (hash-set! ht 1 #f) ht))
  (ctest #t contract-first-order-passes? (hash/c symbol? boolean?) (let ([ht (make-hash)])
                                                                     (hash-set! ht 'x #t)
                                                                     ht))

  (test-name '(or/c) (or/c))
  (test-name '(or/c integer? gt0?) (or/c integer? (let ([gt0? (lambda (x) (> x 0))]) gt0?)))
  (test-name '(or/c integer? boolean?)
             (or/c (flat-contract integer?)
                   (flat-contract boolean?)))
  (test-name '(or/c (-> (>=/c 5) (>=/c 5)) boolean?)
             (or/c (-> (>=/c 5) (>=/c 5)) boolean?))
  (test-name '(or/c (-> (>=/c 5) (>=/c 5)) boolean?)
             (or/c boolean? (-> (>=/c 5) (>=/c 5))))


  (ctest 1
        length
        (let ([f (contract (-> integer? any)
                           (lambda (x)
                             (with-continuation-mark 'x 'x
                               (continuation-mark-set->list (current-continuation-marks) 'x)))
                           'pos
                           'neg)])
          (with-continuation-mark 'x 'x
            (f 1))))

  (ctest 2
        length
        (let ([f (contract (-> integer? list?)
                           (lambda (x)
                             (with-continuation-mark 'x 'x
                               (continuation-mark-set->list (current-continuation-marks) 'x)))
                           'pos
                           'neg)])
          (with-continuation-mark 'x 'x
            (f 1))))

  (ctest #t contract-first-order-passes? (or/c 'x "x" #rx"x") 'x)
  (ctest #t contract-first-order-passes? (or/c 'x "x" #rx"x") "x")
  (ctest #t contract-first-order-passes? (or/c 'x "x" #rx"x.") "xy")
  (ctest #f contract-first-order-passes? (or/c 'x "x" #rx"x.") "yx")
  (ctest #f contract-first-order-passes? (or/c 'x "x" #rx"x.") 'y)


;
;
;
;
;    ;          ;;; ;;;
;  ;;;              ;;;
;  ;;;;  ;;;;;  ;;; ;;;
;  ;;;; ;;;;;;; ;;; ;;;
;  ;;;  ;;  ;;; ;;; ;;;
;  ;;;    ;;;;; ;;; ;;;
;  ;;;  ;;; ;;; ;;; ;;;
;  ;;;; ;;; ;;; ;;; ;;;
;   ;;;  ;;;;;; ;;; ;;;
;
;
;
;

  (contract-eval
   `(define (counter)
      (let ([c 0])
        (case-lambda
          [() c]
          [(x) (set! c (+ c 1)) #t]))))

  (ctest 1
         'tail-arrow
         (let ([c (counter)])
           (letrec ([f
                     (contract (-> any/c c)
                               (λ (x) (if (zero? x) x (f (- x 1))))
                               'pos
                               'neg)])
             (f 3))
           (c)))

  (ctest 1
         'tail-unconstrained-domain-arrow
         (let ([c (counter)])
           (letrec ([f
                     (contract (unconstrained-domain-> c)
                               (λ (x) (if (zero? x) x (f (- x 1))))
                               'pos
                               'neg)])
             (f 3))
           (c)))

  (ctest 2
         'tail-multiple-value-arrow
         (let ([c (counter)])
           (letrec ([f
                     (contract (-> any/c (values c c))
                               (λ (x) (if (zero? x) (values x x) (f (- x 1))))
                               'pos
                               'neg)])
             (f 3))
           (c)))

  (ctest 2
         'tail-arrow-star
         (let ([c (counter)])
           (letrec ([f
                     (contract (->* (any/c) () (values c c))
                               (λ (x) (if (zero? x) (values x x) (f (- x 1))))
                               'pos
                               'neg)])
             (f 3))
           (c)))


  (ctest 1
         'case->-regular
         (let ([c (counter)])
           (letrec ([f
                     (contract (case-> (-> any/c c)
                                       (-> any/c any/c c))
                               (case-lambda
                                 [(x) (if (zero? x) x (f (- x 1)))]
                                 [(x y) (f x)])
                               'pos
                               'neg)])
             (f 4 1))
           (c)))

  (ctest 1
         'case->-rest-args
         (let ([c (counter)])
           (letrec ([f
                     (contract (case-> (-> any/c #:rest any/c c)
                                       (-> any/c any/c #:rest any/c c))
                               (case-lambda
                                 [(x) (f x 1)]
                                 [(x y . z) (if (zero? x) x (apply f (- x 1) y (list y y)))])
                               'pos
                               'neg)])
             (f 4))
           (c)))

  (ctest '(1)
         'mut-rec-with-any/c
         (let ()
           (define f
             (contract (-> number? any/c)
                       (lambda (x)
                         (if (zero? x)
                             (continuation-mark-set->list (current-continuation-marks) 'tail-test)
                             (with-continuation-mark 'tail-test x
                               (g (- x 1)))))
                       'pos
                       'neg))

           (define g
             (contract (-> number? any/c)
                       (lambda (x)
                         (f x))
                       'pos
                       'neg))

           (f 3)))

  (test/pos-blame 'free-vars-change-so-cannot-drop-the-check
                  '(let ()
                     (define f
                       (contract (->i ([x number?]) () [_ (x) (</c x)])
                                 (lambda (x)
                                   (cond
                                     [(= x 0) 1]
                                     [else (f 0)]))
                                 'pos
                                 'neg))
                     (f 10)))



;
;
;
;
;                  ;;                                              ;;                       ;;
;                  ;;                                              ;;                       ;;
;  ;;   ;;  ;;;;   ;;  ;;  ;;   ;;;;         ;;;;   ;;;;   ;; ;;  ;;;;  ;; ;  ;;;;    ;;;; ;;;;
;  ;;;  ;; ;;;;;;  ;;  ;;  ;;  ;;;;;;       ;;;;;  ;;;;;   ;;;;;; ;;;;  ;;;; ;;;;;;  ;;;;; ;;;;
;   ;; ;;      ;;  ;;  ;;  ;;  ;;  ;;      ;;   ;;;;   ;;  ;;  ;;  ;;   ;;       ;; ;;   ;; ;;
;   ;; ;;      ;;  ;;  ;;  ;;  ;;;;;; ;;;; ;;   ;;;;   ;;  ;;  ;;  ;;   ;;       ;; ;;   ;; ;;
;   ;; ;;   ;;;;;  ;;  ;;  ;;  ;;;;;; ;;;; ;;     ;;   ;;  ;;  ;;  ;;   ;;    ;;;;; ;;      ;;
;   ;; ;;  ;;  ;;  ;;  ;;  ;;  ;;          ;;   ;;;;   ;;  ;;  ;;  ;;   ;;   ;;  ;; ;;   ;; ;;
;    ;;;   ;;  ;;  ;;  ;;  ;;  ;;          ;;   ;;;;   ;;  ;;  ;;  ;;   ;;   ;;  ;; ;;   ;; ;;
;    ;;;   ;;;;;;  ;;  ;;;;;;  ;;;;;;       ;;;;;  ;;;;;   ;;  ;;  ;;;  ;;   ;;;;;;  ;;;;;  ;;;
;    ;;;    ;; ;;  ;;   ;;;;;   ;;;;        ;;;;   ;;;;;   ;;  ;;  ;;;  ;;    ;; ;;  ;;;;   ;;;
;
;
;
;

  (test #f value-contract #f)
  (test #f value-contract (λ (x) x))
  (test #f value-contract (unit (import) (export)))
  (test #f value-contract object%)

  (let ([ctc (-> number? number?)])
    (test ctc value-contract (contract ctc (λ (x) x) 'pos 'neg)))
  (let ([ctc (->* (number?) (number?) number?)])
    (test ctc value-contract (contract ctc (λ (x [y 3]) x) 'pos 'neg)))
  (let ([ctc (->d ([x number?]) ([y number?]) [_ number?])])
    (test ctc value-contract (contract ctc (λ (x [y 3]) x) 'pos 'neg)))
  (let ([ctc (->i ([x number?]) ([y number?]) [_ number?])])
    (test ctc value-contract (contract ctc (λ (x [y 3]) x) 'pos 'neg)))
  (let ([ctc (unconstrained-domain-> number?)])
    (test ctc value-contract (contract ctc (λ (x) 3) 'pos 'neg)))
  (let ([ctc (case-> (-> number? number? number?) (-> number? number?))])
    (test ctc value-contract (contract ctc (case-lambda [(x) 3] [(x y) (+ x y)]) 'pos 'neg)))

  (let ([ctc (box/c number?)])
    (test ctc value-contract (contract ctc (box 3) 'pos 'neg)))
  (let ([ctc (hash/c number? number?)])
    (test ctc value-contract (contract ctc (make-hash) 'pos 'neg)))
  (let ([ctc (vectorof number?)])
    (test ctc value-contract (contract ctc (vector 1 2 3) 'pos 'neg)))
  (let ([ctc (vector/c number? number?)])
    (test ctc value-contract (contract ctc (vector 4 5) 'pos 'neg)))

  (let ([ctc (object-contract)])
    (test ctc value-contract (contract ctc (new object%) 'pos 'neg)))

;
;
;
;
;
;   ;;;;;;
;        ;   ;
;        ;   ;
;        ;  ;;  ;;;;
;   ;;;;;;  ;  ;;;;;;
;        ;  ; ;;;
;        ;  ; ;;;
;        ; ;;  ;;;;;;
;   ;;;;;; ;    ;;;;
;
;
;
;

  (test/spec-passed
   '∃1
   '(contract (new-∃/c 'pair)
              1
              'pos
              'neg))

  (test/neg-blame
   '∃2
   '((contract (-> (new-∃/c 'pair) any/c)
               (λ (x) x)
               'pos
               'neg)
     1))

  (test/spec-passed/result
   '∃3
   '(let ([pair (new-∃/c 'pair)])
      ((contract (-> (-> pair pair) any/c)
                 (λ (f) (f 11))
                 'pos
                 'neg)
       (λ (x) x)))
   11)

  (test/pos-blame
   '∀1
   '(contract (new-∀/c 'pair)
              1
              'pos
              'neg))

  (test/spec-passed
   '∀2
   '((contract (-> (new-∀/c 'pair) any/c)
               (λ (x) x)
               'pos
               'neg)
     1))

  (test/spec-passed/result
   '∀3
   '(let ([pair (new-∀/c 'pair)])
      ((contract (-> pair pair)
                 (λ (x) x)
                 'pos
                 'neg)
       11))
   11)


;                                 ;
;                                ;
;                    ;           ;
;    ;;;;    ;;;;  ;;;;;;       ;     ;;;;
;   ;        ;   ;   ;         ;     ;
;   ;;      ;    ;   ;         ;    ;
;     ;;    ;;;;;;   ;        ;     ;
;       ;   ;        ;        ;     ;
;       ;   ;        ;       ;       ;
;   ;;;;     ;;;;;    ;;;   ;         ;;;;
;                           ;
;                          ;
;

  (test/spec-passed/result
   'set/c1
   '(contract (set/c integer?)
              (set 0)
              'pos 'neg)
   (contract-eval '(set 0)))

  (test/pos-blame
   'set/c2
   '(contract (set/c integer?)
              (set #t)
              'pos 'neg))

  (test/pos-blame
   'set/c3
   '(contract (set/c integer? #:cmp 'eq)
              (set 0)
              'pos 'neg))

  (test/pos-blame
   'set/c4
   '(contract (set/c integer? #:cmp 'eqv)
              (set 0)
              'pos 'neg))

  (test/pos-blame
   'set/c5
   '(contract (set/c integer? #:cmp 'equal)
              (seteq 0)
              'pos 'neg))

  (test/spec-passed/result
   'set/c6
   '(set-map (contract (set/c integer?)
                       (set 0)
                       'pos 'neg)
             values)
   (list 0))

  (test/neg-blame
   'set/c7
   '(let ([s (set-map (contract (set/c (-> integer? integer?))
                                (set (λ (x) #f))
                                'pos 'neg)
                      values)])
      ((car s) #f)))

  (test/pos-blame
   'set/c8
   '(let ([s (set-map (contract (set/c (-> integer? integer?))
                                (set (λ (x) #f))
                                'pos 'neg)
                      values)])
      ((car s) 1)))


;
;
;
;
;                            ;                    ;
;                          ;;;                  ;;;
;    ;;;     ;;;   ;;; ;;  ;;;;   ;;;;  ;;; ;;; ;;;;
;   ;;;;;   ;;;;;  ;;;;;;; ;;;;  ;; ;;;  ;; ;;  ;;;;
;  ;;;  ;; ;;; ;;; ;;; ;;; ;;;  ;;; ;;;  ;;;;;  ;;;
;  ;;;     ;;; ;;; ;;; ;;; ;;;  ;;;;;;;   ;;;   ;;;
;  ;;;  ;; ;;; ;;; ;;; ;;; ;;;  ;;;      ;;;;;  ;;;
;   ;;;;;   ;;;;;  ;;; ;;; ;;;;  ;;;;;;  ;; ;;  ;;;;
;    ;;;     ;;;   ;;; ;;;  ;;;   ;;;;  ;;; ;;;  ;;;
;
;
;
;

  (contract-eval '(define (extract-context-lines thunk)
                    (define str
                      (with-handlers ((exn:fail:contract:blame? exn-message))
                        (thunk)
                        "didn't raise an exception"))
                    (define m (regexp-match #rx".*\n +in: (.*)$" str))
                    (cond
                      [m
                       (define without-prefix (list-ref m 1))
                       (define m2 (regexp-match #rx"(.*)\n *contract from:" without-prefix))
                       (cond
                         [m2
                          (define lines (regexp-split #rx"\n *" (list-ref m2 1)))
                          ;; drop the lines with the contract (keep lines beginning with an article)
                          (let loop ([lines (regexp-split #rx"\n *" (list-ref m2 1))])
                            (cond
                              [(null? lines) '()]
                              [else
                               (define line (car lines))
                               (cond
                                 [(or (regexp-match #rx"^the " line)
                                      (regexp-match #rx"^an " line)
                                      (regexp-match #rx"^a " line))
                                  (cons line (loop (cdr lines)))]
                                 [else
                                  (loop (cdr lines))])]))]
                         [else
                          (string-append "did not find ``contract from:'', so no context in msg: "
                                      str)])]
                      [else
                       (string-append "did not find ``in:'', so no context in msg: "
                                      str)])))

  (ctest '("the cdr of" "the 1st argument of")
         extract-context-lines
         (λ () ((contract (-> (cons/c integer? boolean?) integer? integer?)
                          (λ (x y) x)
                          'pos
                          'neg)
                (cons 1 2) 1)))

  (ctest '("the 3rd element of" "the 2nd argument of")
         extract-context-lines
         (λ () ((contract (-> integer? (list/c integer? integer? boolean?) integer?)
                          (λ (x y) x)
                          'pos
                          'neg)
                1 (list 1 2 3))))

  (ctest '("the range of" "the 4th element of")
         extract-context-lines
         (λ () ((cadddr (contract (list/c integer? integer? boolean? (-> number? number?))
                                  (list 1 2 #f (λ (x) #f))
                                  'pos
                                  'neg))
                1)))

  (ctest '("a disjunct of")
         extract-context-lines
         (λ () (contract (or/c 1 (-> number? number?))
                         3
                         'pos
                         'neg)))

  (ctest '("the range of" "a disjunct of")
         extract-context-lines
         (λ () ((contract (or/c 1 (-> number? number?) (-> number? boolean? number?))
                          (λ (x) #f)
                          'pos
                          'neg)
                1)))

  (ctest '("the 2nd conjunct of")
         extract-context-lines
         (λ () (contract (and/c procedure? (-> integer? integer?))
                         (λ (x y) 1)
                         'pos
                         'neg)))

  (ctest '("an element of")
         extract-context-lines
         (λ () (contract (listof number?)
                         (list #f)
                         'pos
                         'neg)))

  (ctest '("the promise from")
         extract-context-lines
         (λ () (force (contract (promise/c number?)
                                (delay #f)
                                'pos
                                'neg))))

  (ctest '("the parameter of")
         extract-context-lines
         (λ () ((contract (parameter/c number?)
                          (make-parameter #f)
                          'pos
                          'neg))))
  (ctest '("the parameter of")
         extract-context-lines
         (λ () ((contract (parameter/c number?)
                          (make-parameter 1)
                          'pos
                          'neg)
                #f)))
  (ctest '("the #:x argument of")
         extract-context-lines
         (λ () ((contract (-> #:x number? #:a char? #:w boolean? any)
                          (λ (#:x x #:a a #:w w) x)
                          'pos
                          'neg)
                #:a #\a #:w #f #:x 'two)))

  (ctest '("the #:a argument of")
         extract-context-lines
         (λ () ((contract (-> #:x number? #:a char? #:w boolean? any)
                          (λ (#:x x #:a a #:w w) x)
                          'pos
                          'neg)
                #:a #f #:w #f #:x 2)))

    (ctest '("the #:w argument of")
         extract-context-lines
         (λ () ((contract (-> #:x number? #:a char? #:w boolean? any)
                          (λ (#:x x #:a a #:w w) x)
                          'pos
                          'neg)
                #:a #\a #:w 'false #:x 2)))

  (ctest '("the #:x argument of")
         extract-context-lines
         (λ () ((contract (->* () (#:x number?) any)
                          (λ (#:x [x 1]) x)
                          'pos
                          'neg)
                #:x #f)))

  (ctest '("the #:x argument of")
         extract-context-lines
         (λ () ((contract (->* () (#:x number? #:a char? #:w boolean?) any)
                          (λ (#:x [x 1] #:a [a #\a] #:w [w #f]) x)
                          'pos
                          'neg)
                #:a #\a #:w #f #:x 'two)))

  (ctest '("the #:a argument of")
         extract-context-lines
         (λ () ((contract (->* () (#:x number? #:a char? #:w boolean?) any)
                          (λ (#:x [x 1] #:a [a #\a] #:w [w #f]) x)
                          'pos
                          'neg)
                #:a #f #:w #f #:x 2)))

    (ctest '("the #:w argument of")
         extract-context-lines
         (λ () ((contract (->* () (#:x number? #:a char? #:w boolean?) any)
                          (λ (#:x [x 1] #:a [a #\a] #:w [w #f]) x)
                          'pos
                          'neg)
                #:a #\a #:w 'false #:x 2)))

  (ctest '("the x argument of")
         extract-context-lines
         (λ () ((contract (->i ([w integer?] [x boolean?] [a char?]) any)
                          (λ (w x a) x)
                          'pos
                          'neg)
                1 'true #\a)))

  (ctest '("the x argument of")
         extract-context-lines
         (λ () ((contract (->i ([w integer?]) ([x boolean?] [a char?]) any)
                          (λ (w [x #t] [a #\a]) x)
                          'pos
                          'neg)
                1 'true #\a)))

  (ctest '("the y result of")
         extract-context-lines
         (λ () ((contract (->i () (values [x integer?] [y integer?]))
                          (λ () (values 1 #f))
                          'pos
                          'neg))))

  (ctest '("the x result of")
         extract-context-lines
         (λ () ((contract (->i () (values [x integer?] [y integer?]))
                          (λ () (values #f 1))
                          'pos
                          'neg))))

  (ctest '("the _ result of")
         extract-context-lines
         (λ () ((contract (->i ([x integer?]) [_ (x) (<=/c x)])
                          add1
                          'pos
                          'neg)
                1)))

  (ctest '("the a argument of")
         extract-context-lines
         (λ () ((contract (->i ([a integer?] #:b [b integer?]) ([c integer?] #:d [d integer?]) any)
                          (λ (a #:b b [c 1] #:d [d 1]) 1)
                          'pos
                          'neg)
                'one #:b 2 3 #:d 4)))

  (ctest '("the b argument of")
         extract-context-lines
         (λ () ((contract (->i ([a integer?] #:b [b integer?]) ([c integer?] #:d [d integer?]) any)
                          (λ (a #:b b [c 1] #:d [d 1]) 1)
                          'pos
                          'neg)
                1 #:b 'two 3 #:d 4)))

  (ctest '("the c argument of")
         extract-context-lines
         (λ () ((contract (->i ([a integer?] #:b [b integer?]) ([c integer?] #:d [d integer?]) any)
                          (λ (a #:b b [c 1] #:d [d 1]) 1)
                          'pos
                          'neg)
                1 #:b 2 'three #:d 4)))

  (ctest '("the d argument of")
         extract-context-lines
         (λ () ((contract (->i ([a integer?] #:b [b integer?]) ([c integer?] #:d [d integer?]) any)
                          (λ (a #:b b [c 1] #:d [d 1]) 1)
                          'pos
                          'neg)
                1 #:b 2 3 #:d 'four)))

  ;; indy
  (ctest '("the 2nd argument of" "the x argument of")
         extract-context-lines
         (λ () ((contract (->i ([x (-> number? boolean? integer?)] [a (x) (>=/c (x 11 'true))]) any)
                          (λ (x a) x)
                          'pos
                          'neg)
                (λ (x y) 1) 11)))

  (ctest '("the 2nd argument of" "the x result of")
         extract-context-lines
         (λ () ((contract (->i () (values [x (-> number? boolean? integer?)] [a (x) (>=/c (x 11 'true))]))
                          (λ () (values (λ (x y) x) 1))
                          'pos
                          'neg))))

  (ctest '("the x argument of")
         extract-context-lines
         (λ () ((contract (->i ([x () integer?]) any)
                          (λ (x) x)
                          'pos
                          'neg)
                #f)))

  (ctest '("the a argument of")
         extract-context-lines
         (λ () ((contract (->i ([a integer?] [x (a) integer?]) any)
                          (λ (a x) x)
                          'pos
                          'neg)
                #f 1)))

  (ctest '("the 1st result of")
         extract-context-lines
         (λ () ((contract (->i () (values [_ integer?] [_ integer?]))
                          (λ () (values #f 1))
                          'pos
                          'neg))))

  (ctest '("the result of")
         extract-context-lines
         (λ () ((contract (->i () [_ integer?])
                          (λ () (values #f))
                          'pos
                          'neg))))

  (ctest '("the domain of")
         extract-context-lines
         (λ () ((contract (->d ([x integer?]) [y integer?])
                          (λ (x) #f)
                          'pos
                          'neg)
                #f)))

  (ctest '("the range of")
         extract-context-lines
         (λ () ((contract (->d ([x integer?]) [y integer?])
                          (λ (x) #f)
                          'pos
                          'neg)
                1)))

  (ctest '("the range of")
         extract-context-lines
         (λ () (letrec ([ctc (-> integer? (recursive-contract ctc))])
                 (letrec ([f (λ (x) 'not-f)])
                   ((contract ctc f 'pos 'neg) 1)))))

  (ctest '("the a field of")
         extract-context-lines
         (λ ()
           (struct s (a b))
           (contract (struct/dc s [a (b) (<=/c b)] [b integer?])
                     (s 2 1)
                     'pos
                     'neg)))

  (ctest '("the a field of")
         extract-context-lines
         (λ ()
           (struct s (a b))
           (contract (struct/dc s [a (<=/c 1)] [b integer?])
                     (s 2 1)
                     'pos
                     'neg)))

  (ctest '("an element of" "the 2nd element of")
         extract-context-lines
         (λ () (vector-ref
                (vector-ref
                 (contract (vector/c (vectorof real?) (vectorof number?) (vectorof boolean?))
                           (vector (vector 1) (vector 1) (vector 1))
                           'pos
                           'neg)
                 2)
                0)))

  (ctest '("the 0th element of")
         extract-context-lines
         (λ () (vector-ref (contract (vector/c integer?)
                                     (vector #f)
                                     'pos
                                     'neg)
                           0)))

  (ctest '("the 0th element of")
         extract-context-lines
         (λ () (vector-ref (contract (vector/c (-> integer? integer?))
                                     (vector #f)
                                     'pos
                                     'neg)
                           0)))

  (ctest '("the 0th element of")
         extract-context-lines
         (λ () (vector-ref (contract (vector/c (new-∀/c 'α))
                                     (vector #f)
                                     'pos
                                     'neg)
                           0)))

  (ctest '("an element of")
         extract-context-lines
         (λ () (vector-ref
                (contract (vectorof integer?)
                          (vector #f)
                          'pos
                          'neg)
                0)))

  (ctest '("an element of")
         extract-context-lines
         (λ () (vector-ref (contract (vectorof (-> integer? integer?))
                                     (vector #f)
                                     'pos
                                     'neg)
                           0)))

  (ctest '("an element of")
         extract-context-lines
         (λ () (vector-ref (contract (vectorof (new-∀/c 'α))
                                     (vector #f)
                                     'pos
                                     'neg)
                           0)))

  (ctest '("the keys of")
         extract-context-lines
         (λ () (contract (hash/c integer? (-> integer? integer?))
                         (hash #f (λ (x) #f))
                         'pos
                         'neg)))

  (ctest '("the range of" "the values of")
         extract-context-lines
         (λ () ((hash-ref
                 (contract (hash/c integer? (-> integer? integer?))
                           (hash 0 (λ (x) #f))
                           'pos
                           'neg)
                 0)
                1)))

  (ctest '("an element of" "the rest argument of")
         extract-context-lines
         (λ ()
           ((contract (->* () #:rest (listof number?) number?)
                      +
                      'pos 'neg)
            1 "a")))

  (ctest '("the 2nd argument of")
         extract-context-lines
         (λ ()
           ((contract (->* (number? number?) #:rest (listof number?) number?)
                      +
                      'pos 'neg)
            1 "a")))

  (ctest '("an element of" "the rest argument of")
         extract-context-lines
         (λ ()
           ((contract (->* (number?) #:rest (listof number?) number?)
                      +
                      'pos 'neg)
            1 "a")))


  (let* ([blame-pos (contract-eval '(make-blame #'here #f (λ () 'integer?) 'positive 'negative #t))]
         [blame-neg (contract-eval `(blame-swap ,blame-pos))])
    (ctest "something ~a" blame-fmt->-string ,blame-neg "something ~a")
    (ctest "promised: ~s\n produced: ~e" blame-fmt->-string ,blame-pos '(expected: "~s" given: "~e"))
    (ctest "expected: ~s\n given: ~e" blame-fmt->-string ,blame-neg '(expected: "~s" given: "~e"))
    (ctest "promised ~s produced ~e" blame-fmt->-string ,blame-pos '(expected "~s" given "~e"))
    (ctest "expected ~s given ~e" blame-fmt->-string ,blame-neg '(expected "~s" given "~e")))

;
;
;
;                                         ;;
;                                     ;;  ;;
;                                     ;;
;   ;;;;  ;;;; ;;;   ;;;;   ;;;; ;;;;;;;; ;;  ;;;;  ;;;
;   ;;;;; ;;;;;;;;;  ;;;;; ;;;;;;;;;;;;;; ;; ;;;;;;;;;;;
;   ;; ;; ;;  ;; ;;  ;; ;; ;;; ;;;;   ;;  ;; ;;;;;;;;
;   ;; ;; ;;  ;; ;;  ;; ;; ;;;;;;;;   ;;  ;; ;;;;;;  ;;;
;   ;;;;; ;;  ;;;;;  ;;;;; ;;;;;;;;   ;;  ;; ;;;;;;;; ;;
;   ;;;;  ;;   ;;;   ;;;;   ;;;; ;;   ;;; ;;  ;;;;  ;;;
;   ;;               ;;
;   ;;               ;;
;


  (test-obligations '(-> a b)
                    '((racket/contract:contract (->) ())
                      (racket/contract:negative-position a)
                      (racket/contract:positive-position b)))
  (test-obligations '(->i ([x a]) any)
                    '((racket/contract:contract (->i) ())
                      (racket/contract:contract-on-boundary a)
                      (racket/contract:negative-position a)))
  (test-obligations '(->i ([x a]) [res b])
                    '((racket/contract:contract (->i) ())
                      (racket/contract:contract-on-boundary a)
                      (racket/contract:contract-on-boundary b)
                      (racket/contract:negative-position a)
                      (racket/contract:positive-position b)))
  (test-obligations '(->i ([x a]) #:pre () #t [res b] #:post () #t)
                    '((racket/contract:contract (#:post ->i) (#:pre))
                      (racket/contract:contract-on-boundary a)
                      (racket/contract:contract-on-boundary b)
                      (racket/contract:negative-position a)
                      (racket/contract:positive-position b)))
  (test-obligations '(listof a)
                    '((racket/contract:contract (listof) ())
                      (racket/contract:positive-position a)))
  (test-obligations '(hash/c a b)
                    '((racket/contract:contract (hash/c) ())
                      (racket/contract:negative-position a)
                      (racket/contract:positive-position b)))
  (test-obligations '(box/c a)
                    '((racket/contract:contract (box/c) ())
                      (racket/contract:positive-position a)))
  (test-obligations '(box-immutable/c a)
                    '((racket/contract:contract (box-immutable/c) ())
                      (racket/contract:positive-position a)))
  (test-obligations '(vectorof a)
                    '((racket/contract:contract (vectorof) ())
                      (racket/contract:positive-position a)))
  (test-obligations '(vector-immutableof a)
                    '((racket/contract:contract (vector-immutableof) ())
                      (racket/contract:positive-position a)))
  (test-obligations '(vector/c a b c)
                    '((racket/contract:contract (vector/c) ())
                      (racket/contract:positive-position a)
                      (racket/contract:positive-position b)
                      (racket/contract:positive-position c)))
  (test-obligations '(vector-immutable/c a b c)
                    '((racket/contract:contract (vector-immutable/c) ())
                      (racket/contract:positive-position a)
                      (racket/contract:positive-position b)
                      (racket/contract:positive-position c)))


;
;
;
;                                        ;;      ;;;;          ;;
;                                        ;;      ;;;;          ;;
;  ;;;;;;;   ;;; ;;;   ;;;;   ;;;  ;;;        ;;;;;;;   ;;;    ;;
;  ;;;;;;;;  ;;;;;;;  ;;;;;;  ;;;  ;;; ;;;;  ;;;;;;;;  ;;;;;   ;;
;  ;;;;;;;;; ;;;; ;; ;;;;;;;;  ;;;;;;  ;;;; ;;;;;;;;; ;;;; ;;  ;;
;  ;;;; ;;;; ;;;;    ;;;; ;;;  ;;;;;;  ;;;; ;;;; ;;;; ;;;;;;; ;;
;  ;;;;;;;;; ;;;;    ;;;;;;;;  ;;;;;;  ;;;; ;;;;;;;;; ;;;;;   ;;
;  ;;;;;;;;  ;;;;     ;;;;;;    ;;;;   ;;;;  ;;;;;;;;  ;;;;;; ;;
;  ;;;;;;;   ;;;;      ;;;;     ;;;;   ;;;;   ;;;;;;;   ;;;;  ;;
;  ;;;;                                                       ;;
;  ;;;;
;
;
;
;
;                                 ;                               ;
;                                ;;                              ;;
;    ;;;;;   ;;;;   ;;;; ;;;   ;;;;; ;;; ;;; ;;;;;;;    ;;;;;  ;;;;;
;   ;;;;;;  ;;;;;;  ;;;;;;;;; ;;;;;; ;;;;;;; ;;;;;;;;  ;;;;;; ;;;;;;
;  ;;;;;;; ;;;;;;;; ;;;; ;;;;  ;;;;  ;;;; ;;     ;;;; ;;;;;;;  ;;;;
;  ;;;;    ;;;; ;;; ;;;; ;;;;  ;;;;  ;;;;     ;;;;;;; ;;;;     ;;;;
;  ;;;;;;; ;;;;;;;; ;;;; ;;;;  ;;;;; ;;;;    ;;  ;;;; ;;;;;;;  ;;;;;
;   ;;;;;;  ;;;;;;  ;;;; ;;;;  ;;;;; ;;;;    ;;;;;;;;  ;;;;;;  ;;;;;
;    ;;;;;   ;;;;   ;;;; ;;;;   ;;;; ;;;;     ;; ;;;;   ;;;;;   ;;;;
;
;
;

  ;;
  ;; (at the end, because they are slow w/out .zo files)
  ;;

  (test/spec-passed
   'provide/contract1
   '(let ()
      (eval '(module contract-test-suite1 scheme/base
                (require scheme/contract)
                (define x 1)
                (provide/contract (x integer?))))
      (eval '(require 'contract-test-suite1))
      (eval 'x)))

  (test/spec-passed
   'provide/contract2
   '(let ()
      (eval '(module contract-test-suite2 scheme/base
                (require scheme/contract)
                (provide/contract)))
      (eval '(require 'contract-test-suite2))))

  (test/spec-failed
   'provide/contract3
   '(let ()
      (eval '(module contract-test-suite3 scheme/base
               (require scheme/contract)
               (define x #f)
               (provide/contract (x integer?))))
      (eval '(require 'contract-test-suite3))
      (eval 'x))
   "contract-test-suite3")

  (test/spec-passed
   'provide/contract4
   '(begin
      (eval '(module contract-test-suite4 scheme/base
               (require scheme/contract)
               (define-struct s (a) #:mutable)
               (provide/contract (struct s ((a any/c))))))
      (eval '(require 'contract-test-suite4))
      (eval '(list (make-s 1)
                   (s-a (make-s 1))
                   (s? (make-s 1))
                   (set-s-a! (make-s 1) 2)))))

  (test/spec-passed
   'provide/contract4-b
   '(begin
      (eval '(module contract-test-suite4-b scheme/base
               (require scheme/contract)
               (define-struct s (a))
               (provide/contract (struct s ((a any/c))))))
      (eval '(require 'contract-test-suite4-b))
      (eval '(list (make-s 1)
                   (s-a (make-s 1))
                   (s? (make-s 1))))))

  (test/spec-passed/result
   'provide/contract4-c
   '(begin
      (eval '(module contract-test-suite4-c scheme/base
               (require scheme/contract)
               (define-struct s (a b) #:mutable)
               (provide/contract (struct s ((a any/c) (b any/c))))))
      (eval '(require 'contract-test-suite4-c))
      (eval '(let ([an-s (make-s 1 2)])
               (list (s-a an-s)
                     (s-b an-s)
                     (begin (set-s-a! an-s 3)
                            (s-a an-s))
                     (begin (set-s-b! an-s 4)
                            (s-b an-s))))))

   (list 1 2 3 4))

  (test/spec-passed
   'provide/contract5
   '(begin
      (eval '(module contract-test-suite5 scheme/base
               (require scheme/contract)
               (define-struct s (a))
               (define-struct t (a))
               (provide/contract (struct s ((a any/c)))
                                 (struct t ((a any/c))))))
      (eval '(require 'contract-test-suite5))
      (eval '(list (make-s 1)
                   (s-a (make-s 1))
                   (s? (make-s 1))
                   (make-t 1)
                   (t-a (make-t 1))
                   (t? (make-t 1))))))

  (test/spec-passed
   'provide/contract6
   '(begin
      (eval '(module contract-test-suite6 scheme/base
               (require scheme/contract)
               (define-struct s (a))
               (provide/contract (struct s ((a any/c))))))
      (eval '(require 'contract-test-suite6))
      (eval '(define-struct (t s) ()))))

  (test/spec-passed
   'provide/contract6b
   '(begin
      (eval '(module contract-test-suite6b scheme/base
               (require scheme/contract)
               (define-struct s_ (a))
               (provide/contract (struct s_ ((a any/c))))))
      (eval '(require 'contract-test-suite6b))
      (eval '(module contract-test-suite6b2 scheme/base
               (require 'contract-test-suite6b)
               (require scheme/contract)
               (define-struct (t_ s_) (b))
               (provide s_-a)
               (provide/contract (struct (t_ s_) ((a any/c) (b any/c))))))
      (eval '(require 'contract-test-suite6b2))
      (eval '(define-struct (u_ t_) ()))
      (eval '(s_-a (make-u_ 1 2)))))

  (test/spec-passed
   'provide/contract7
   '(begin
      (eval '(module contract-test-suite7 scheme/base
               (require scheme/contract)
               (define-struct s (a b))
               (define-struct (t s) (c d))
               (provide/contract
                (struct s ((a any/c) (b any/c)))
                (struct (t s) ((a any/c) (b any/c) (c any/c) (d any/c))))))
      (eval '(require 'contract-test-suite7))
      (eval '(let ([x (make-t 1 2 3 4)])
               (s-a x)
               (s-b x)
               (t-c x)
               (t-d x)
               (void)))))

  (test/spec-passed
   'provide/contract8
   '(begin
      (eval '(module contract-test-suite8 scheme/base
               (require scheme/contract)
               (define-struct i-s (contents))
               (define (w-f-s? x) #t)
               (provide/contract
                (struct i-s ((contents (flat-named-contract "integer-set-list" w-f-s?)))))))
      (eval '(require 'contract-test-suite8))
      (eval '(i-s-contents (make-i-s 1)))))

  (test/spec-passed
   'provide/contract9
   '(begin
      (eval '(module contract-test-suite9 scheme/base
               (require scheme/contract)
               (define the-internal-name 1)
               (provide/contract (rename the-internal-name the-external-name integer?))
               (+ the-internal-name 1)))
      (eval '(require 'contract-test-suite9))
      (eval '(+ the-external-name 1))))

  (test/spec-passed
   'provide/contract10
   '(begin
      (eval '(module pc10-m scheme/base
               (require scheme/contract)
               (define-struct s (a b) #:inspector (make-inspector))
               (provide/contract (struct s ((a number?) (b number?))))))
      (eval '(module pc10-n scheme/base
               (require mzlib/struct
                        'pc10-m)
               (print-struct #t)
               (copy-struct s
                            (make-s 1 2)
                            [s-a 3])))
      (eval '(require 'pc10-n))))

  (test/spec-passed
   'provide/contract11
   '(begin
      (eval '(module pc11-m scheme/base
               (require scheme/contract)
               (define x 1)
               (provide/contract [rename x y integer?]
                                 [rename x z integer?])))
      (eval '(module pc11-n scheme/base
               (require 'pc11-m)
               (+ y z)))
      (eval '(require 'pc11-n))))

  ;; this test is broken, not sure why
  #|
  (test/spec-failed
   'provide/contract11b
   '(parameterize ([current-namespace (make-namespace)])
      (eval '(module pc11b-m scheme/base
               (require scheme/contract)
               (define-struct s (a b) #:inspector (make-inspector))
               (provide/contract (struct s ((a number?) (b number?))))))
      (eval '(module pc11b-n scheme/base
               (require mzlib/struct
                        m)
               (print-struct #t)
               (copy-struct s
                            (make-s 1 2)
                            [s-a #f])))
      (eval '(require 'pc11b-n)))
   "n")
|#

  (test/spec-passed
   'provide/contract12
   '(begin
      (eval '(module pc12-m scheme/base
               (require scheme/contract)
               (define-struct (exn2 exn) ())
               (provide/contract (struct (exn2 exn) ((message any/c) (continuation-marks any/c))))))
      (eval '(require 'pc12-m))))

  (test/spec-passed/result
   'provide/contract13
   '(begin
      (eval '(module pc13-common-msg-structs scheme/base
               (require scheme/contract)
               (define-struct register (name type) #:inspector (make-inspector))
               (provide/contract (struct register ([name any/c] [type any/c])))))

      (eval '(require 'pc13-common-msg-structs))
      (eval '(require (lib "plt-match.rkt")))
      (eval '(match (make-register 1 2)
               [(struct register (name type))
                (list name type)])))
   (list 1 2))

  (test/spec-passed
   'provide/contract14
   '(begin
      (eval '(module pc14-test1 scheme/base
               (require scheme/contract)

               (define-struct type (flags))
               (define-struct (type:ptr type) (type))

               (provide/contract
                (struct type
                        ([flags (listof string?)]))

                (struct (type:ptr type)
                        ([flags (listof string?)] [type type?])))))

      (eval '(module pc14-test2 scheme/base
               (require mzlib/plt-match)
               (require 'pc14-test1)
               (match (make-type:ptr '() (make-type '()))
                 [(struct type:ptr (flags type)) #f])))
      (eval '(require 'pc14-test2))))

  ;; make sure unbound identifier exception is raised.
  (contract-error-test
   'contract-error-test7
   #'(begin
       (eval '(module pos scheme/base
                (require scheme/contract)
                (provide/contract [i any/c]))))
   exn:fail:syntax?)

  ;; provide/contract should signal errors without requiring a reference to the variable
  ;; this test is bogus, because provide/contract'd variables can be set!'d.
  (test/spec-failed
   'provide/contract15
   '(begin
      (eval '(module pos scheme/base
               (require scheme/contract)
               (define i #f)
               (provide/contract [i integer?])))
      (eval '(require 'pos)))
   "pos")

  ;; this is really a positive violation, but name the module `neg' just for an addl test
  (test/spec-failed
   'provide/contract16
   '(begin
      (eval '(module neg scheme/base
               (require scheme/contract)
               (define i #f)
               (provide/contract [i integer?])))
      (eval '(require 'neg)))
   "neg")

  ;; this test doesn't pass yet ... waiting for support from define-struct

  #;
  (test/neg-blame
   'provide/contract17
   '(begin
      (eval '(module pos scheme/base
               (require scheme/contract)
               (define-struct s (a))
               (provide/contract [struct s ((a integer?))])))
      (eval '(module neg scheme/base
               (require 'pos)
               (define-struct (t s) ())
               (make-t #f)))
      (eval '(require 'neg))))

  (test/spec-passed
   'provide/contract18
   '(begin
      (eval '(module pc18-pos scheme/base
               (require scheme/contract)
               (define-struct s ())
               (provide/contract [struct s ()])))
      (eval '(require 'pc18-pos))
      (eval '(make-s))))

  (test/spec-passed/result
   'provide/contract19
   '(begin
      (eval '(module pc19-a scheme/base
               (require scheme/contract)
               (define-struct a (x))
               (provide/contract [struct a ([x number?])])))

      (eval '(module pc19-b scheme/base
               (require 'pc19-a
                        scheme/contract)
               (define-struct (b a) (y))
               (provide/contract [struct (b a) ([x number?] [y number?])])))

      (eval '(module pc19-c scheme/base
               (require 'pc19-b
                        scheme/contract)

               (define-struct (c b) (z))
               (provide/contract [struct (c b) ([x number?] [y number?] [z number?])])))

      (eval' (module pc19-d scheme/base
               (require 'pc19-a 'pc19-c)
               (define pc19-ans (a-x (make-c 1 2 3)))
               (provide pc19-ans)))

      (eval '(require 'pc19-d))
      (eval 'pc19-ans))
   1)

  ;; test that unit & contract don't collide over the name `struct'
  (test/spec-passed
   'provide/contract20
   '(eval '(module tmp scheme/base
             (require scheme/contract
                      mzlib/unit)

             (define-struct s (a b))

             (provide/contract
              [struct s ([a number?]
                         [b symbol?])]))))

  (test/spec-passed
   'provide/contract21
   '(begin
      (eval '(module provide/contract21a scheme/base
               (require scheme/contract)
               (provide/contract [f integer?])
               (define f 1)))
      (eval '(module provide/contract21b scheme/base
               (require (for-syntax 'provide/contract21a)
                        (for-syntax scheme/base))
               (define-syntax (unit-body stx)
                 f f
                 #'1)))))

  (test/spec-passed
   'provide/contract22
   '(begin
      (eval '(module provide/contract22a scheme/base
               (require scheme/contract)
               (provide/contract [make-bound-identifier-mapping integer?])
               (define make-bound-identifier-mapping 1)))
      (eval '(module provide/contract22b scheme/base
               (require (for-syntax 'provide/contract22a)
                        (for-syntax scheme/base))

               (define-syntax (unit-body stx)
                 make-bound-identifier-mapping)

               (define-syntax (f stx)
                 make-bound-identifier-mapping)))))

  (test/spec-passed
   'provide/contract23
   '(begin
      (eval '(module provide/contract23a scheme/base
               (require scheme/contract)
               (provide/contract [f integer?])
               (define f 3)))

      (eval '(module provide/contract23b scheme/base
               (require 'provide/contract23a)
               (#%expression f)
               f))

      (eval '(require 'provide/contract23b))))

#|
  (test/spec-passed
   'provide/contract24
   '(begin
      (eval '(module provide/contract24 scheme/base
               (require (prefix-in c: scheme/contract))
               (c:case-> (c:-> integer? integer?)
                         (c:-> integer? integer? integer?))))))
  |#

  ;; tests that contracts pick up the #%app from the context
  ;; instead of always using the scheme/base #%app.
  (test/spec-passed
   'provide/contract25
   '(begin
      (eval '(module provide/contract25a scheme/base
               (require scheme/contract)
               (provide/contract [seventeen integer?])
               (define seventeen 17)))
      (eval '(module provide/contract25b scheme/base
               (require 'provide/contract25a)
               (let-syntax ([#%app (syntax-rules ()
                                     [(#%app e ...) (list e ...)])])
                 (seventeen 18))))
      (eval '(require 'provide/contract25b))))

  (test/spec-passed/result
   'provide/contract26
   '(begin
      (eval '(module provide/contract26 scheme/base
               (require scheme/contract)
               (define-struct pc26-s (a))
               (provide/contract (struct pc26-s ((a integer?))))))
      (eval '(require 'provide/contract26))
      (eval '(pc26-s-a (make-pc26-s 1))))
   1)

  (test/spec-passed/result
   'provide/contract27
   '(begin
      (eval '(module provide/contract27a scheme/base
               (require scheme/contract)
               (define-struct person (name) #:transparent)
               (provide/contract (struct person ([name string?])))))
      (eval '(module provide/contract27b scheme/base
               (require 'provide/contract27a)
               (provide (struct-out person))))
      (eval '(module provide/contract27c scheme/base
               (require 'provide/contract27b)
               (define provide/contract27ans (person-name (make-person "me")))
               (provide provide/contract27ans)))
      (eval '(require 'provide/contract27c))
      (eval 'provide/contract27ans))
   "me")

  #;
  (test/spec-passed/result
   'provide/contract28
   '(begin
      (eval '(module provide/contract28-m1 scheme/base
               (require scheme/contract)
               (define-struct repair () #:transparent)
               (provide/contract [struct repair ()])))
      (eval '(module provide/contract28-m2 scheme/base
               (require 'provide/contract28-m1 scheme/contract)
               (provide/contract [struct repair ()])))
      (eval '(module provide/contract28-m3 scheme/base
               (require 'provide/contract28-m2)
               (provide provide/contract28-res)
               (define provide/contract28-res (repair? (make-repair)))))
      (eval '(require 'provide/contract28-m3))
      (eval 'provide/contract28-res))
   #t)

  #;
  (test/spec-passed/result
   'provide/contract29
   '(begin
      (eval '(module provide/contract29-m1 scheme/base
               (require scheme/contract)
               (define-struct q (a b))
               (define-struct (repair q) (c d) #:transparent)
               (provide/contract [struct repair ([a integer?] [b integer?] [c integer?] [d integer?])])))
      (eval '(module provide/contract29-m2 scheme/base
               (require 'provide/contract29-m1 scheme/contract)
               (provide/contract [struct repair ([a integer?] [b integer?] [c integer?] [d integer?])])))
      (eval '(module provide/contract29-m3 scheme/base
               (require 'provide/contract29-m2)
               (provide provide/contract29-res)
               (define provide/contract29-res (list (repair? (make-repair 1 2 3 4))
                                                    (repair-c (make-repair 1 2 3 4))))))
      (eval '(require 'provide/contract29-m3))
      (eval 'provide/contract29-res))
   (list #t 3))

  ;; for this test I have to be able to track back thru the requirees to find the right
  ;; name for the negative blame (currently it blames m3, but it should  blame m2).
  #;
  (test/spec-failed
   'provide/contract30
   '(begin
      (eval '(module provide/contract30-m1 scheme/base
               (require scheme/contract)
               (provide/contract [f (-> integer? integer?)])
               (define (f x) x)))
      (eval '(module provide/contract30-m2 scheme/base
               (require 'provide/contract30-m1)
               (provide f)))
      (eval '(module provide/contract30-m3 scheme/base
               (require 'provide/contract30-m2)
               (f #f)))
      (eval '(require 'provide/contract30-m3)))
   "provide/contract30-m2")

  (test/spec-passed/result
   'provide/contract31
   '(begin
      (eval '(module provide/contract31-m1 scheme/base
               (require scheme/contract)
               (provide/contract
                #:∃ x
                [f (-> (-> x x) 10)])
               (define (f g) (g 10))))

      (eval '(module provide/contract31-m2 scheme/base
               (require scheme/contract 'provide/contract31-m1)
               (provide provide/contract31-x)
               (define provide/contract31-x (f (λ (x) x)))))

      (eval '(require 'provide/contract31-m2))
      (eval 'provide/contract31-x))
   10)

  (test/spec-passed/result
   'provide/contract32
   '(begin
      (eval '(module provide/contract32-m1 scheme/base
               (require scheme/contract)
               (provide/contract
                #:exists x
                [f (-> (-> x x) 10)])
               (define (f g) (g 10))))

      (eval '(module provide/contract32-m2 scheme/base
               (require scheme/contract 'provide/contract32-m1)
               (provide provide/contract32-x)
               (define provide/contract32-x (f (λ (x) x)))))

      (eval '(require 'provide/contract32-m2))
      (eval 'provide/contract32-x))
   10)

  (test/spec-passed/result
   'provide/contract33
   '(begin
      (eval '(module provide/contract33-m1 scheme/base
               (require scheme/contract)
               (provide/contract
                #:exists (x)
                [f (-> (-> x x) 10)])
               (define (f g) (g 10))))

      (eval '(module provide/contract33-m2 scheme/base
               (require scheme/contract 'provide/contract33-m1)
               (provide provide/contract33-x)
               (define provide/contract33-x (f (λ (x) x)))))

      (eval '(require 'provide/contract33-m2))
      (eval 'provide/contract33-x))
   10)

  (test/spec-passed/result
   'provide/contract34
   '(begin
      (eval '(module provide/contract34-m1 scheme/base
               (require scheme/contract)
               (define x integer?)
               (define g 11)
               (provide/contract
                [g x]
                #:exists (x)
                [f (-> (-> x x) 10)])
               (define (f g) (g 10))))

      (eval '(module provide/contract34-m2 scheme/base
               (require scheme/contract 'provide/contract34-m1)
               (provide provide/contract34-x)
               (define provide/contract34-x (f (λ (x) x)))))

      (eval '(require 'provide/contract34-m2))
      (eval 'provide/contract34-x))
   10)


  ;; The following test is designed to test that source locations for contracts
  ;; survive compilation and being saved to disk (and thus aren't recorded by
  ;; quoted syntax object constant embedded in the expansion).
  (let ()
    ;; compile/wash : like compile, but reads and writes the data
    ;; so that source locations (and other things presumably) get dumped.
    (define (compile/wash x)
      (let-values ([(in out) (make-pipe)])
        (thread
         (λ () (write (contract-compile x) out)))
        (parameterize ([read-accept-compiled #t])
          (read in))))

    ;; drop-var-info : syntax -> syntax
    ;; strips the lexical content from the syntax object, but preserves the source locations
    (define (drop-var-info stx)
      (let loop ([stx stx])
        (cond
          [(syntax? stx)
           (datum->syntax #f (loop (syntax-e stx)) stx)]
          [(pair? stx)
           (cons (loop (car stx))
                 (loop (cdr stx)))]
          [else stx])))

    ;; WARNING: do not add or remove lines between here-line and the two modules
    ;; below it, unless you also revise the expected result of the test case.
    (define here-line (syntax-line #'here))

    (contract-eval
     (compile/wash
      (drop-var-info
       #'(module provide/contract-35/m racket/base
           (require racket/contract)
           (define (f x) x)
           (provide/contract [f (-> integer? integer?)])))))

    (contract-eval
     (compile/wash
      (drop-var-info
       #'(module provide/contract-35/n racket/base
           (require 'provide/contract-35/m)
           (f #f)))))

      (test (format "contract-test.rktl:~a.30"
                    (+ here-line 8))
            'provide/contract-compiled-source-locs
            (with-handlers ((exn:fail? (λ (x)
                                         (let ([m (regexp-match #rx"contract-test.rktl[^ ]*.30" (exn-message x))])
                                           (and m (car m))))))

              (contract-eval '(require 'provide/contract-35/n)))))

  ;; test that provide/contract by itself in a module doesn't signal an error
  (test/spec-passed/result
   'provide/contract35
   '(begin
      (eval '(module provide/contract35-m1 racket
               (provide/contract [add1 (-> number? number?)])))

      (eval '(module provide/contract35-m2 racket/base
               (require 'provide/contract35-m1)
               (provide provide/contract35-three)
               (define provide/contract35-three (add1 2))))

      (eval '(require 'provide/contract35-m2))
      (eval 'provide/contract35-three))
   3)

  (test/spec-passed/result
   'provide/contract36
   '(begin

      (eval '(module provide/contract36-m racket/base
               (require racket/contract)
               (struct a (x))
               (struct b a ())
               (provide/contract
                [struct a ((x symbol?))]
                [struct (b a) ((x symbol?))])))

      (eval '(module provide/contract36-n racket/base
               (require 'provide/contract36-m)
               (provide new-b-x)
               (define new-b-x
                 (a-x
                  (struct-copy b (b 'x)
                               [x #:parent a 'y])))))

      (eval '(require 'provide/contract36-n))
      (eval 'new-b-x))
   'y)

  (test/spec-failed
   'provide/contract37
   '(begin

      (eval '(module provide/contract37-m racket/base
               (require racket/contract)
               (struct a (x))
               (struct b a ())
               (provide/contract
                [struct a ((x symbol?))]
                [struct (b a) ((x symbol?))])))

      (eval '(module provide/contract37-n racket/base
               (require 'provide/contract37-m)
               (struct-copy b (b 'x)
                            [x #:parent a 5])))

      (eval '(require 'provide/contract37-n)))
   "provide/contract37-n")

  (test/spec-passed/result
   'provide/contract38
   '(begin
      (eval
        '(module provide/contract38-a racket
           (define-struct s () #:transparent)
	   (provide/contract [struct s ()])))

      (eval
        '(module provide/contract38-b racket
           (require 'provide/contract38-a)
	   (define a-struct (make-s))
	   (define-values (type _) (struct-info a-struct))
	   (provide the-answer)
	   (define the-answer (eq? type struct:s))))

      (dynamic-require ''provide/contract38-b 'the-answer))
   #t)

  ;; #:forall contract-out clauses
  (test/spec-passed/result
   'provide/contract39
   '(begin
      (eval '(module provide/contract39-m1 racket/base
               (require racket/contract)
               (provide/contract
                #:∀ x
                [f (-> x (-> x x) x)])
               (define (f x g) (g x))))

      (eval '(module provide/contract39-m2 racket/base
               (require racket/contract 'provide/contract39-m1)
               (provide provide/contract39-x)
               (define provide/contract39-x (f 10 (λ (x) x)))))

      (eval '(require 'provide/contract39-m2))
      (eval 'provide/contract39-x))
   10)

  (test/spec-passed/result
   'provide/contract40
   '(begin
      (eval '(module provide/contract40-m1 racket/base
               (require racket/contract)
               (provide/contract
                #:forall x
                [f (-> x (-> x x) x)])
               (define (f x g) (g x))))

      (eval '(module provide/contract40-m2 racket/base
               (require racket/contract 'provide/contract40-m1)
               (provide provide/contract40-x)
               (define provide/contract40-x (f 10 (λ (x) x)))))

      (eval '(require 'provide/contract40-m2))
      (eval 'provide/contract40-x))
   10)

  (test/spec-passed/result
   'provide/contract41
   '(begin
      (eval '(module provide/contract41-m1 racket/base
               (require racket/contract)
               (provide/contract
                #:forall (x)
                [f (-> x (-> x x) x)])
               (define (f x g) (g x))))

      (eval '(module provide/contract41-m2 racket/base
               (require racket/contract 'provide/contract41-m1)
               (provide provide/contract41-x)
               (define provide/contract41-x (f 10 (λ (x) x)))))

      (eval '(require 'provide/contract41-m2))
      (eval 'provide/contract41-x))
   10)

  (test/spec-passed/result
   'provide/contract42
   '(begin
      (eval '(module provide/contract42-m1 racket/base
               (require racket/contract)
               (define x integer?)
               (define g 11)
               (provide/contract
                [g x]
                #:forall (x)
                [f (-> x (-> x x) x)])
               (define (f x g) (g x))))

      (eval '(module provide/contract42-m2 racket/base
               (require racket/contract 'provide/contract42-m1)
               (provide provide/contract42-x)
               (define provide/contract42-x (f 10 (λ (x) x)))))

      (eval '(require 'provide/contract42-m2))
      (eval 'provide/contract42-x))
   10)

  (contract-error-test
   'contract-error-test8
   #'(begin
       (eval '(module pce1-bug scheme/base
                (require scheme/contract)
                (define the-defined-variable1 'five)
                (provide/contract [the-defined-variable1 number?])))
       (eval '(require 'pce1-bug)))
   (λ (x)
     (and (exn:fail:contract:blame? x)
          (regexp-match #rx"the-defined-variable1: broke its contract" (exn-message x)))))

  (contract-error-test
   'contract-error-test9
   #'(begin
       (eval '(module pce2-bug scheme/base
                (require scheme/contract)
                (define the-defined-variable2 values)
                (provide/contract [the-defined-variable2 (-> number? any)])))
       (eval '(require 'pce2-bug))
       (eval '(the-defined-variable2 #f)))
   (λ (x)
     (and (exn:fail:contract:blame? x)
          (regexp-match #rx"the-defined-variable2: contract violation" (exn-message x)))))

  (contract-error-test
   'contract-error-test10
   #'(begin
       (eval '(module pce3-bug scheme/base
                (require scheme/contract)
                (define the-defined-variable3 (λ (x) #f))
                (provide/contract [the-defined-variable3 (-> any/c number?)])))
       (eval '(require 'pce3-bug))
       (eval '(the-defined-variable3 #f)))
   (λ (x)
     (and (exn:fail:contract:blame? x)
          (regexp-match #rx"the-defined-variable3" (exn-message x)))))

  (contract-error-test
   'contract-error-test11
   #'(begin
       (eval '(module pce4-bug scheme/base
                (require scheme/contract)
                (define the-defined-variable4 (λ (x) #f))
                (provide/contract [the-defined-variable4 (-> any/c number?)])))
       (eval '(require 'pce4-bug))
       (eval '((if #t the-defined-variable4 the-defined-variable4) #f)))
   (λ (x)
     (and (exn:fail:contract:blame? x)
          (regexp-match #rx"^the-defined-variable4" (exn-message x)))))

  (contract-error-test
   'contract-error-test12
   #'(begin
       (eval '(module pce5-bug scheme/base
                (require scheme/contract)

                (define-struct bad (a b))

                (provide/contract
                 [struct bad ((string? a) (string? b))])))
       (eval '(require 'pce5-bug)))
   (λ (x)
     (and (exn:fail:syntax? x)
          (regexp-match #rx"expected field name to be b, but found string?" (exn-message x)))))

  (contract-error-test
   'contract-error-test13
   #'(begin
       (eval '(module pce6-bug scheme/base
                (require scheme/contract)

                (define-struct bad-parent (a))
                (define-struct (bad bad-parent) (b))

                (provide/contract
                 [struct bad ((a string?) (string? b))])))
       (eval '(require 'pce6-bug)))
   (λ (x)
     (and (exn:fail:syntax? x)
          (regexp-match #rx"expected field name to be b, but found string?" (exn-message x)))))

  (contract-error-test
   'contract-error-test14
   #'(begin
       (eval '(module pce7-bug scheme/base
                (require scheme/contract)
                (define x 1)
                (provide/contract [x integer?])))
       (eval '(module pce7-bug2 scheme/base
                (require 'pce7-bug)
                (set! x 5))))
   (λ (x)
     (and (exn:fail:syntax? x)
          (regexp-match #rx"cannot set!" (exn-message x)))))

  (contract-error-test
   'contract-error-test15
   #'(begin
       (eval '(module pce8-bug1 scheme/base
                (require scheme/contract)
                (define (f x) x)
                (provide/contract [f (-> integer? integer? integer?)])))
       (eval '(require 'pce8-bug1)))
   (λ (x)
     (and (exn:fail:contract:blame? x)
          (regexp-match #rx"pce8-bug" (exn-message x)))))

  (contract-error-test
   'contract-error-test16
   #'(begin
       (eval '(module pce9-bug scheme
                (define (f x) "wrong")
                (provide/contract
                 [rename f g
                         (-> number? number?)])))
       (eval '(require 'pce9-bug))
       (eval '(g 12)))
   (λ (x)
     (and (exn:fail:contract:blame? x)
          (regexp-match #rx"^g.*contract from: pce9-bug" (exn-message x)))))

  (contract-error-test
   'contract-error-test17
   #'(begin
       (eval '(module pce10-bug scheme
                (define (f x) "wrong")
                (provide/contract
                 [rename f g
                         (-> number? number?)])))
       (eval '(require 'pce10-bug))
       (eval '(g 'a)))
   (λ (x)
     (and (exn:fail:contract:blame? x)
          (regexp-match #rx"^g.*contract from: pce10-bug" (exn-message x)))))

  (contract-eval
   `(,test
     'pos
     (compose blame-positive exn:fail:contract:blame-object)
     (with-handlers ((void values)) (contract not #t 'pos 'neg))))


  ;; check that `contract-out' contracts can use contracts
  ;; defined later in the module
  (test/spec-passed/result
   'contract-out1
   '(begin
      (eval '(module contract-out1-m racket/base
               (require racket/contract)
               (provide (contract-out [f (-> ok? ok?)]))
               (define (f x) (+ x 1))
               (define (ok? v) (exact-integer? v))))
      (eval '(require 'contract-out1-m))
      (eval '(f 10)))
   11)

  (test/spec-passed/result
   'contract-out2
   '(begin
      (eval '(module contract-out2-m racket/base
               (require racket/contract)
               (provide (contract-out (struct s ([x integer?]))))
               (struct s (x))))
      (eval '(require 'contract-out2-m))
      (eval '(s-x (s 11))))
   11)

  ;; expect the syntax errors in the right order
  (contract-syntax-error-test
   'contract-out3
   '(eval '(module contract-out3-m racket/base
             (require racket/contract)
             (provide (contract-out garbage))
             (λ)))
   #rx"contract-out")

  (test/pos-blame
   'contract-struct/c-1
   '(begin
      (eval '(module contract-struct/c-1a racket/base
               (struct s (field))
               (provide s)))
      (eval '(module contract-struct/c-1b racket/base
               (require 'contract-struct/c-1a racket/contract)
               (contract (struct/c s boolean?)
                         (s 1)
                         'pos 'neg)))
      (eval '(require 'contract-struct/c-1b))))

  (test/spec-passed
   'contract-struct/c-2
   '(begin
      (eval '(module contract-struct/c-2a racket/base
               (struct s (field))
               (provide s)))
      (eval '(module contract-struct/c-2b racket/base
               (require 'contract-struct/c-2a racket/contract)
               (contract (struct/c s any/c)
                         (s 1)
                         'pos 'neg)))
      (eval '(require 'contract-struct/c-2b))))


;
;
;
;
;                      ;;;   ;  ;;;      ;;;;;;; ;;;
;                      ;;; ;;;          ;;;      ;;;
;  ;;; ;; ;;;  ;;; ;;; ;;; ;;;; ;;;     ;;;; ;;; ;;;   ;;;;
;  ;;;;;;;;;;; ;;; ;;; ;;; ;;;; ;;;     ;;;; ;;; ;;;  ;; ;;;
;  ;;; ;;; ;;; ;;; ;;; ;;; ;;;  ;;;     ;;;  ;;; ;;; ;;; ;;;
;  ;;; ;;; ;;; ;;; ;;; ;;; ;;;  ;;; ;;;;;;;  ;;; ;;; ;;;;;;;
;  ;;; ;;; ;;; ;;; ;;; ;;; ;;;  ;;; ;;;;;;;  ;;; ;;; ;;;
;  ;;; ;;; ;;; ;;;;;;; ;;; ;;;; ;;;     ;;;  ;;; ;;;  ;;;;;;
;  ;;; ;;; ;;;  ;; ;;; ;;;  ;;; ;;;     ;;;  ;;; ;;;   ;;;;
;
;
;
;

  (let ()
    ;; build-and-run : (listof (cons/c string[filename] (cons/c string[lang-line] (listof sexp[body-of-module]))) -> any
    ;; sets up the files named by 'test-case', dynamically requires the first one, deletes the files
    ;; and returns/raises-the-exception from the require'd file
    (define (build-and-run test-case)
      (define dir (make-temporary-file "contract-test~a" 'directory))
      (for ([f (in-list test-case)])
        (call-with-output-file (build-path dir (car f))
          (lambda (port)
            (display (cadr f) port)
            (newline port)
            (for ([sexp (in-list (cddr f))])
              (fprintf port "~s\n" sexp)))))
      (dynamic-wind
       void
       (lambda () (contract-eval `(dynamic-require ,(build-path dir (car (car test-case))) #f)))
       (lambda ()
         (for ([f (in-list test-case)])
           (delete-file (build-path dir (car f))))
         (delete-directory dir))))

    (define (get-last-part-of-path sexp)
      (define str (format "orig-blame: ~s" sexp))
      (define m (regexp-match #rx"[/\\]([-a-z0-9.]*)[^/\\]*$" str))
      (if m (cadr m) str))

    ;; basic negative blame case
    (let ([blame
           (exn:fail:contract:blame-object
            (with-handlers ((exn:fail:contract:blame? values))
              (build-and-run
               (list (list "a.rkt"
                           "#lang racket/base"
                           '(require "b.rkt")
                           '(f #f))
                     (list "b.rkt"
                           "#lang racket/base"
                           '(require racket/contract)
                           '(provide/contract [f (-> integer? integer?)])
                           '(define (f x) 1))))))])
      (ctest "a.rkt"
             'multi-file-blame1-positive
             (,get-last-part-of-path (blame-positive ,blame)))
      (ctest "b.rkt"
             'multi-file-blame1-negative
             (,get-last-part-of-path (blame-negative ,blame))))

    ;; basic positive blame case
    (let ([blame
           (exn:fail:contract:blame-object
            (with-handlers ((exn:fail:contract:blame? values))
              (build-and-run
               (list (list "a.rkt"
                           "#lang racket/base"
                           '(require "b.rkt")
                           '(f 1))
                     (list "b.rkt"
                           "#lang racket/base"
                           '(require racket/contract)
                           '(provide/contract [f (-> integer? integer?)])
                           '(define (f x) #f))))))])
      (ctest "b.rkt"
             'multi-file-blame2-positive
             (,get-last-part-of-path (blame-positive ,blame)))
      (ctest "a.rkt"
             'multi-file-blame2-negative
             (,get-last-part-of-path (blame-negative ,blame))))

    ;; positive blame via a re-provide
    (let ([blame
           (exn:fail:contract:blame-object
            (with-handlers ((exn:fail:contract:blame? values))
              (build-and-run
               (list (list "a.rkt"
                           "#lang racket/base"
                           '(require "b.rkt")
                           '(f 1))
                     (list "b.rkt"
                           "#lang racket/base"
                           '(require "c.rkt")
                           '(provide f))
                     (list "c.rkt"
                           "#lang racket/base"
                           '(require racket/contract)
                           '(provide/contract [f (-> integer? integer?)])
                           '(define (f x) #f))))))])
      (ctest "c.rkt"
             'multi-file-blame3-positive
             (,get-last-part-of-path (blame-positive ,blame)))
      (ctest "a.rkt"
             'multi-file-blame3-negative
             (,get-last-part-of-path (blame-negative ,blame))))

    ;; negative blame via a re-provide
    (let ([blame
           (exn:fail:contract:blame-object
            (with-handlers ((exn:fail:contract:blame? values))
              (build-and-run
               (list (list "a.rkt"
                           "#lang racket/base"
                           '(require "b.rkt")
                           '(f #f))
                     (list "b.rkt"
                           "#lang racket/base"
                           '(require "c.rkt")
                           '(provide f))
                     (list "c.rkt"
                           "#lang racket/base"
                           '(require racket/contract)
                           '(provide/contract [f (-> integer? integer?)])
                           '(define (f x) 1))))))])
      (ctest "a.rkt"
             'multi-file-blame4-positive
             (,get-last-part-of-path (blame-positive ,blame)))
      (ctest "c.rkt"
             'multi-file-blame4-negative
             (,get-last-part-of-path (blame-negative ,blame))))

    ;; have some sharing in the require graph
    (let ([blame
           (exn:fail:contract:blame-object
            (with-handlers ((exn:fail:contract:blame? values))
              (build-and-run
               (list (list "client.rkt"
                           "#lang racket/base"
                           '(require "server.rkt" "other.rkt")
                           '(turn-init #f))
                     (list "server.rkt"
                           "#lang racket/base"
                           '(require racket/contract)
                           '(provide/contract [turn-init (-> number? any)])
                           '(define turn-init void))
                     (list "other.rkt"
                           "#lang racket/base"
                           '(require "server.rkt"))))))])
      (ctest "client.rkt"
             'multi-file-blame5-positive
             (,get-last-part-of-path (blame-positive ,blame)))
      (ctest "server.rkt"
             'multi-file-blame5-negative
             (,get-last-part-of-path (blame-negative ,blame)))))



;
;
;
;
;  ;;;
;  ;;;
;  ;;;   ;;;;   ;; ;;;  ;;;;;    ;;;   ;;; ;;;
;  ;;;  ;; ;;; ;;;;;;; ;;;;;;;  ;;;;;  ;;; ;;;
;  ;;; ;;; ;;; ;;; ;;; ;;  ;;; ;;;  ;;  ;; ;;
;  ;;; ;;;;;;; ;;; ;;;   ;;;;; ;;;      ;; ;;
;  ;;; ;;;     ;;; ;;; ;;; ;;; ;;;  ;;  ;; ;;
;  ;;;  ;;;;;; ;;;;;;; ;;; ;;;  ;;;;;    ;;;
;  ;;;   ;;;;   ;; ;;;  ;;;;;;   ;;;     ;;;
;                  ;;;                 ;;;;;
;              ;;;;;;                  ;;;;
;
;

;
;
;
;
;                                   ;                         ;
;                                 ;;;                       ;;;
;    ;;;     ;;;   ;;; ;;   ;;;;  ;;;; ;;; ;;;; ;;;   ;;;   ;;;;   ;;;   ;;; ;;;;;;
;   ;;;;;   ;;;;;  ;;;;;;; ;;; ;; ;;;; ;;;;;;;; ;;;  ;;;;;  ;;;;  ;;;;;  ;;;;;;;; ;;
;  ;;;  ;; ;;; ;;; ;;; ;;; ;;;    ;;;  ;;;  ;;; ;;; ;;;  ;; ;;;  ;;; ;;; ;;;  ;;;
;  ;;;     ;;; ;;; ;;; ;;;  ;;;;  ;;;  ;;;  ;;; ;;; ;;;     ;;;  ;;; ;;; ;;;   ;;;;
;  ;;;  ;; ;;; ;;; ;;; ;;;    ;;; ;;;  ;;;  ;;; ;;; ;;;  ;; ;;;  ;;; ;;; ;;;     ;;;
;   ;;;;;   ;;;;;  ;;; ;;; ;; ;;; ;;;; ;;;  ;;;;;;;  ;;;;;  ;;;;  ;;;;;  ;;;  ;; ;;;
;    ;;;     ;;;   ;;; ;;;  ;;;;   ;;; ;;;   ;; ;;;   ;;;    ;;;   ;;;   ;;;   ;;;;
;
;
;
;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;  make-proj-contract
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (contract-eval
   '(define proj:add1->sub1
      (make-proj-contract
       'proj:add1->sub1
       (lambda (pos neg src name blame)
         (lambda (f)
           (unless (and (procedure? f) (procedure-arity-includes? f 1))
             (raise-contract-error f src pos name
                                   "expected a unary function, got: ~e"
                                   f))
           (lambda (x)
             (unless (and (integer? x) (exact? x))
               (raise-contract-error x src neg name
                                     "expected an integer, got: ~e"
                                     x))
             (let* ([y (f (add1 x))])
               (unless (and (integer? y) (exact? y))
                 (raise-contract-error y src pos name
                                       "expected an integer, got: ~e"
                                       y))
               (sub1 y)))))
       (lambda (f)
         (and (procedure? f) (procedure-arity-includes? f 1))))))

  (test/spec-passed/result
   'make-proj-contract-1
   '((contract proj:add1->sub1 sqrt 'pos 'neg) 15)
   3)

  (test/pos-blame
   'make-proj-contract-2
   '(contract proj:add1->sub1 'dummy 'pos 'neg))

  (test/pos-blame
   'make-proj-contract-3
   '((contract proj:add1->sub1 (lambda (x) 'dummy) 'pos 'neg) 2))

  (test/neg-blame
   'make-proj-contract-4
   '((contract proj:add1->sub1 sqrt 'pos 'neg) 'dummy))

  ;; errortrace test
  (let ()
    (define sp (open-input-string (format "~s\n" '(-> (λ (a b c) #f) any))))
    (define stx (read-syntax 'whereitsat sp))
    (define exn
      (parameterize ([current-namespace (make-base-namespace)])
        (namespace-require 'racket/contract)
        (namespace-require 'errortrace)
        (with-handlers ((exn:fail? values))
          (eval stx))))
    (define sp2 (open-output-string))
    (parameterize ([current-error-port sp2])
      ((error-display-handler) (exn-message exn) exn))
    (test #t 
          'checking-arrow-src-locs
          (regexp-match? #rx"whereitsat" (get-output-string sp2))))

  
  (report-errs)

))
