#lang racket/base
(require (for-syntax racket/base))

(define rx:version
  ;; (this restricts the last component to be below 999 too, which is
  ;; not really proper according to the spec in schvers.h)
  (pregexp (string-append "^(0|[1-9][0-9]*)[.]"
                          "(0|(0|[1-9][0-9]{0,1})([.](0|[1-9][0-9]{0,2})){0,2}"
                          "(?<![.]0))$")))

(define (valid-version? s)
  (and (string? s) (regexp-match? rx:version s)))

(define-syntax (define/version-inputs stx)
  (syntax-case stx ()
    [(_ (f x ...) body ...)
     #'(define (f x ...)
         (check-version-inputs 'f (list x ...))
         body ...)]))
(define (check-version-inputs fn args)
  (for ([arg (in-list args)]
        [i (in-naturals)])
    (unless (valid-version? arg)
      (apply raise-argument-error fn "valid-version?" i args))))


;; returns a list of 4 integers (see src/racket/src/schvers.h)
(define/version-inputs (version->list str)
  (define ver (map string->number (regexp-split #rx"[.]" str)))
  (case (length ver)
    [(2) (append ver '(0 0))]
    [(3) (append ver '(0))]
    [(4) ver]
    [else (error 'version->list "bad version: ~e" str)]))

(define/version-inputs (version<? a b)
  (-version<? a b))

(define/version-inputs (version<=? a b)
  (or (equal? a b) (-version<? a b)))

(define (-version<? a b)
  (let loop ([a (version->list a)] [b (version->list b)])
    (cond [(null? a) #f]
          [(< (car a) (car b)) #t]
          [(> (car a) (car b)) #f]
          [else (loop (cdr a) (cdr b))])))

(define/version-inputs (alpha-version? v)
  (define l (version->list v))
  (or ((list-ref l 1) . >= . 90)
      ((list-ref l 2) . >= . 900)
      ((list-ref l 3) . >= . 900)))

;; returns an integer representing the version (XXYYZZZWWW) or #f if invalid
;; works for pre v4 versions too
(define (version->integer ver)
  (define m
    (regexp-match-positions #rx"^(?:0|[1-9][0-9]*)" ver)) ; takes all digits
  ;; translate old versions to new-style versions
  (define n (and m (string->number (substring ver 0 (cdar m)))))
  (define v
    (cond [(not n) #f]
          ;; new versions
          [(< n 49) ver]
          ;; old versions (earliest useful is 49, changed at 3.99)
          [(<= 49 n 379)
           (define-values [q r] (quotient/remainder n 100))
           (define sfx (let ([sfx (substring ver (cdar m))])
                         (cond [(equal? sfx "") ""]
                               ;; NNNpN -> N.NN.N
                               [(regexp-match? #rx"^p[0-9]" sfx)
                                (string-append "." (substring sfx 1))]
                               ;; NNN.N -> N.NN.0.N (not a release version)
                               [(regexp-match? #rx"^[.]" sfx)
                                (string-append ".0" sfx)]
                               [else #f])))
           (and sfx (format "~a.~a~a" q r sfx))]
          ;; bad strings
          [else #f]))
  (and v (valid-version? v)
       (foldl (λ (ver mul acc) (+ ver (* mul acc))) 0
              (version->list v) '(0 100 1000 1000))))

(define-syntax-rule
  (provide+save-in-list exported-functions (x p?) ...)
  (begin
    (provide x ...)
    (module+ test (define exported-functions (list (cons x p?) ...)))))

(provide+save-in-list
 exported-functions
 (valid-version? boolean?)
 (version->list (λ (x) (and (list? x) (= (length x) 4) (andmap integer? x))))
 (version<? boolean?)
 (version<=? boolean?)
 (alpha-version? boolean?)
 (version->integer (λ (x) (or (integer? x) (not x)))))

(module+ test
  (require racket/list)

  (define (random-argument)
    (case (random 10)
      [(1)
       ;; random string of digits, periods lowercase letters, and greek letters
       (define candidates
         (append (build-list 10 (λ (x) (integer->char (+ x (char->integer #\a)))))
                 (build-list 10 (λ (x) (integer->char (+ x (char->integer #\0)))))
                 (build-list 10 (λ (x) (integer->char (+ x (char->integer #\α)))))
                 '(#\.)))
       (apply
        string
        (for/list ([i (in-range (random 100))])
          (list-ref candidates (random (length candidates)))))]
      [(0)
       ;; kind of versionish (periods and digits in 100 chars)
       (apply
        string
        (for/list ([i (in-range (random 100))])
          (case (random 4)
            [(0) #\.]
            [else (integer->char (+ (random 10) (char->integer #\0)))])))]
      [else
       ;; much closer to a version;
       ;; at most 6 fields of digits that are
       ;; between 1 and 4 chars in length
       (apply
        string-append
        (add-between
         (for/list ([i (in-range (+ 1 (random 5)))])
           (apply
            string
            (for/list ([i (in-range (random 4))])
              (integer->char (+ (random 10) (char->integer #\0))))))
         "."))]))

  (define (trial f+p)
    (define f (car f+p))
    (define p (cdr f+p))
    (define args (for/list ([i (in-range (procedure-arity f))])
                   (random-argument)))
    (define (check-exn exn)
      (define m (regexp-match #rx"^([^:]*):" (exn-message exn)))
      (if (equal? (string->symbol (list-ref m 1))
                  (object-name f))
          #f
          args))
    (with-handlers ([exn:fail? check-exn])
      (if (p (apply f args))
          #f
          args)))

  (time
   (let/ec give-up
     (for ([f+p (in-list exported-functions)])
       (for ([_ (in-range 100)])
         (define trial-result (trial f+p))
         (when trial-result
           (eprintf "failed: ~s\n" (cons (object-name (car f+p)) trial-result))
           (give-up)))))))
