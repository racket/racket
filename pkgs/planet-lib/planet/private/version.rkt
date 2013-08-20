#lang racket/base
(require (for-syntax racket/base syntax/parse)
         racket/path
         racket/match
         unstable/syntax
         racket/syntax
         racket/private/this-expression-source-directory
         planet/planet-archives)

(provide this-package-version
         this-package-version-name
         this-package-version-owner
         this-package-version-maj
         this-package-version-min
         this-package-version-symbol
         package-version->symbol
         make-planet-symbol
         (rename-out [this-package-version/proc path->package-version]))

(define-syntax (this-package-version stx)
  (syntax-case stx ()
    [(_)
     #`(this-package-version/proc
         (this-expression-source-directory #,stx))]))

(define-syntax define-getters
  (syntax-rules ()
    [(define-getters (name position) ...)
     (begin
       (define-syntax (name stx)
         (syntax-case stx ()
           [(name)
            #`(let ([p #,(datum->syntax stx `(,#'this-package-version))])
                (and p (position p)))]))
       ...)]))

(define-getters
  (this-package-version-name pd->name)
  (this-package-version-owner pd->owner)
  (this-package-version-maj pd->maj)
  (this-package-version-min pd->min))

(define-syntax (this-package-version-symbol stx)
  (syntax-parse stx
    [(_ (~optional suffix:id))
     #`(package-version->symbol
         (this-package-version/proc
           (this-expression-source-directory #,stx))
         #,@(if (attribute suffix) #'['suffix] #'[]))]))


;; ----------------------------------------

(define (make-planet-symbol stx [suffix #f])
  (match (syntax-source-directory stx)
    [#f #f]
    [dir (match (this-package-version/proc dir)
           [#f #f]
           [ver (package-version->symbol ver suffix)])]))

(define (package-version->symbol ver [suffix #f])
  (match ver
    [(list owner name major minor)
     (string->symbol
       (format "~a/~a:~a:~a~a"
         owner
         (regexp-replace #rx"\\.plt$" name "")
         major
         minor
         (if suffix (format-symbol "/~a" suffix) "")))]
    [#f #f]))

(define (this-package-version/proc srcdir)
  (define (archive-retval->simple-retval p)
    (list-refs p '(1 2 4 5)))
  
  ;; predicate->projection : #f \not\in X ==> (X -> boolean) -> (X -> X)
  (define (predicate->projection pred) (λ (x) (if (pred x) x #f)))

  (let* ([package-roots (get-all-planet-packages)]
         [thepkg (ormap (predicate->projection (contains-dir? srcdir))
                        package-roots)])
    (and thepkg (archive-retval->simple-retval thepkg))))

;; contains-dir? : path -> pkg -> boolean
(define ((contains-dir? srcdir) alleged-superdir-pkg)
  (let* ([nsrcdir (simple-form-path srcdir)]
         [nsuperdir (simple-form-path (car alleged-superdir-pkg))]
         [nsrclist (explode-path nsrcdir)]
         [nsuperlist (explode-path nsuperdir)])
    (list-prefix? nsuperlist nsrclist)))

(define (list-prefix? sup sub)
  (let loop ([sub sub]
             [sup sup])
    (cond
      [(null? sup) #t]
      [(equal? (car sup) (car sub))
       (loop (cdr sub) (cdr sup))]
      [else #f])))

(define-values (pd->owner pd->name pd->maj pd->min)
  (apply values (map (λ (n) (λ (l) (list-ref l n))) '(0 1 2 3))))

(define (list-refs p ns)
  (map (λ (n) (list-ref p n)) ns))
