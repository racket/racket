#lang racket/base

(require lang/private/rewrite-error-message)

;; -----------------------------------------------------------------------------
;; this module provides one-point functionality to report errors in teachpacks 

;; -----------------------------------------------------------------------------
(provide check-arg
         check-list-list
         check-arity
         check-proc
         check-result 
         check-fun-res
         check-color
         check-dependencies
         natural?
         number->ord
         find-non 
         tp-exn?
         tp-error)

;; check-arg : sym bool str (or/c str non-negative-integer) TST -> void
(define (check-arg pname condition expected arg-posn given)
  (unless condition
    (tp-error pname "expects ~a as ~a argument, given ~e"
              (add-article expected) 
              (spell-out arg-posn)
              given)))

;; Symbol (union true String) String X -> void
(define (check-list-list pname condition pred given)
  (when (string? condition)
    (tp-error pname (string-append condition (format "\nin ~e" given)))))

;; check-arity : sym num (list-of TST) -> void
(define (check-arity name arg# args)
  (unless (= (length args) arg#)
    (tp-error name (argcount-error-message #f arg# (length args)))))

;; check-proc : sym (... *->* ...) num (union sym str) (union sym str) -> void
(define (check-proc name f exp-arity arg# arg-err)
  (define arg#-text (if (number? arg#) (number->ord arg#) arg#))
  (unless (procedure? f)
    (tp-error name "expected a function as ~a argument; given ~e" arg#-text f))
  (define arity-of-f (procedure-arity f))
  (unless (procedure-arity-includes? f exp-arity)
    (tp-error name "expected function of ~a as ~a argument; given function of ~a "
              arg-err arg#-text
              (cond
                [(number? arity-of-f)
                 (if (= arity-of-f 1)
                     (format "1 argument")
                     (format "~s arguments" arity-of-f))]
                [(arity-at-least? arity-of-f) "variable number of arguments"]
                [else (format "multiple arities (~s)" arity-of-f)]))))

;; Symbol (_ -> Boolean) String X  X *-> X 
(define (check-result pname pred? expected given . other-given)
  (if (pred? given)
      given
      (tp-error pname "is expected to return ~a, but it returned ~v" 
                (add-article expected) 
                (if (pair? other-given)
                    (car other-given)
                    given))))

;; check-color : symbol (or/c str non-negative-integer) TST -> void
(define (check-color pname arg-pos given)
  (check-arg pname
             (or (string? given)
                 (symbol? given))
             'color
             arg-pos given)
  ;; this would be good to check, but it isn't possible, since this
  ;; file is not allowed to rely on mred. 
  ;; also nice would be to allow color% objects here, but that's
  ;; not possible for the same reason (but that is why 
  ;; the '[else color]' case is below in the cond.
  #;
  (let ([color
         (cond
           [(symbol? given)
            (send the-color-database find-color (symbol->string given))]
           [(string? given)
            (send the-color-database find-color given)]
           [else given])])
    (unless color
      (tp-error pname 
                "expected the name ~e to be a color, but did not recognize it"
                given))))

;; (: check-fun-res (∀ (γ) (∀ (β α ...) (α ...α -> β)) (_ -γ-> boolean) _ -> γ))
(define (check-fun-res f pred? type)
  (lambda x 
    (check-result (object-name f) pred? type (apply f x))))

;; check-dependencies : Symbol x Boolean x FormatString x Any* -> Void
(define (check-dependencies pname condition fmt . args)
  (unless condition
    (tp-error pname (apply format fmt args))))

(define-struct (tp-exn exn) ())

(define (tp-error name fmt . args)
  (raise 
   (make-exn:fail:contract 
    (string-append (format "~a: " name) (apply format fmt args))
    (current-continuation-marks))))

(define (number->ord i)
  (if (= i 0)
      "zeroth"
      (case (modulo i 10)
        [(0 4 5 6 7 8 9) (format "~ath" i)]
        [(1) (format "~ast" i)]
        [(2) (format "~and" i)]
        [(3) (format "~ard" i)])))

;; (_ -> Boolean) (listof X) -> (union X false)
;; (not (find-non list? '((1 2 3) (a b c))))
;; (symbol? (find-non number? '(1 2 3 a)))
;; (symbol? (find-non list? '((1 2 3) a (b c))))
(define (find-non pred? l)
  (let ([r (filter (compose not pred?) l)])
    (if (null? r) #f (car r))))

(define (natural? w)
  (and (number? w) (integer? w) (>= w 0)))

;; add-article : anything -> string
;; (add-article 'color) should be "a color"
;; (add-article 'acronym) should be "an acronym"
(define (add-article thing)
  (let ((s (format "~a" thing)))
    (string-append
     (if (starts-with-vowel? s)
         "an "
         "a ")
     s)))

;; starts-with-vowel? : string -> boolean
(define (starts-with-vowel? s)
  (and
   (not (string=? s ""))
   (member (string-ref s 0) (list #\a #\e #\i #\o #\u))))

;; spell-out : number-or-string -> string
(define (spell-out arg-posn)
  (cond
    [(string? arg-posn) arg-posn]
    [(number? arg-posn)
     (case arg-posn
       [(1) "first"]
       [(2) "second"]
       [(3) "third"]
       [(4) "fourth"]
       [(5) "fifth"]
       [(6) "sixth"]
       [(7) "seventh"]
       [(8) "eighth"]
       [(9) "ninth"]
       [(10) "tenth"]
       [else (number->ord arg-posn)])]))
