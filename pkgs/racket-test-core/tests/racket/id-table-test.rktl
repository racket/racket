(load-relative "loadtest.rktl")

(require syntax/id-table
         racket/dict)
  
(Section 'id-table)

(test #t bound-id-table? (make-bound-id-table))
(test #t bound-id-table? (make-immutable-bound-id-table))

(test #t mutable-bound-id-table? (make-bound-id-table))
(test #t immutable-bound-id-table? (make-immutable-bound-id-table))

(module module-that-supplies-a-b racket/base
  (provide b)
  (define b #'b))

(let ()
  (define i (make-syntax-introducer))
  (define a (i #'a))
  (define b (i #'b))
  (define b2 (dynamic-require ''module-that-supplies-a-b 'b))
  (define b3 ((make-syntax-introducer) b)) ;; free=? to b
  (define alist (list (cons a 1) (cons b 2) (cons b2 3) (cons b3 4)))
  (test 4 bound-id-table-count (make-bound-id-table alist))
  (test 4 bound-id-table-count (make-immutable-bound-id-table alist))
  (test 3 free-id-table-count (make-free-id-table alist))
  (test 3 free-id-table-count (make-immutable-free-id-table alist))
  (test 3 length (free-id-table-keys (make-immutable-free-id-table alist)))
  (test 3 length (free-id-table-values (make-immutable-free-id-table alist)))

  (let ()
    ;; Test in-dict, iteration methods for immutable id-tables
    (define d1 (make-immutable-bound-id-table alist))
    (test (+ 1 2 3 4) (lambda () (for/sum ([(id v) (in-dict d1)]) v)))
    (define d2 (for/fold ([d (make-immutable-bound-id-table)])
                   ([(id v) (in-dict d1)])
                 (dict-set d id (add1 v))))
    ;; Test in-bound-id-table
    (define d3 (for/fold ([d (make-immutable-bound-id-table)])
                   ([(id v) (in-bound-id-table d1)])
                 (dict-set d id (add1 v))))
    (test 2 bound-id-table-ref d2 a)
    (test 3 bound-id-table-ref d2 b)
    (test 4 bound-id-table-ref d2 b2)
    (test 5 bound-id-table-ref d2 b3)
    (test 2 bound-id-table-ref d3 a)
    (test 3 bound-id-table-ref d3 b)
    (test 4 bound-id-table-ref d3 b2)
    (test 5 bound-id-table-ref d3 b3)
    (test 4 sequence-length (in-bound-id-table d1))
    (test (for/list ([(k v) (in-bound-id-table d1)]) (cons k v))
          (λ () (for/list ([(k v) (values (in-bound-id-table d1))]) (cons k v)))) )

  (let ()
    ;; Test in-dict, iteration methods for mutable id-tables
    ;; In particular, test that -set! of *existing* key does not disrupt iter.
    (define d1 (make-bound-id-table alist))
    (test (+ 1 2 3 4) (lambda () (for/sum ([(id v) (in-dict d1)]) v)))
    (for ([(id v) (in-dict d1)])
      (bound-id-table-set! d1 id (add1 v)))
    (test (+ 2 3 4 5) (lambda () (for/sum ([(id v) (in-dict d1)]) v)))
    ;; Repeat test with in-bound-id-table
    (define d2 (make-bound-id-table alist))
    (test (+ 1 2 3 4) (lambda () (for/sum ([(id v) (in-bound-id-table d2)]) v)))
    (for ([(id v) (in-bound-id-table d2)])
      (bound-id-table-set! d2 id (add1 v)))
    (test (+ 2 3 4 5) (lambda () (for/sum ([(id v) (in-bound-id-table d2)]) v)))))

(let ()
  ;; contains-same? : (listof x) (listof x) -> boolean
  (define (contains-same? l1 l2)
    (and (andmap (lambda (x) (member x l2)) l1)
         (andmap (lambda (x) (member x l1)) l2)
         #t))

  (let-values ([(x1 x2 x3 x4)
                (syntax-case (expand #'((lambda (x) x) (lambda (x) x))) ()
                  [(x (a (x1) x2) (c (x3) x4))
                   (values (syntax x1)
                           (syntax x2)
                           (syntax x3)
                           (syntax x4))])])

    (let ([check (lambda (=?)
                   (test #t =? x1 x2)
                   (test #t =? x3 x4)
                   (when (=? x1 x3)
                     ((current-print) "huh!?"))
                   (test #f =? x1 x3)
                   (test #f =? x1 x4)
                   (test #f =? x2 x3)
                   (test #f =? x2 x4))])
      (check bound-identifier=?)
      (check free-identifier=?))

    (let ([table (make-bound-id-table)])
      (bound-id-table-set! table x1 #f)
      (test #f bound-id-table-ref table x1)

      (bound-id-table-set! table x1 1)
      (bound-id-table-set! table x2 2)
      (bound-id-table-set! table x3 3)
      (bound-id-table-set! table x4 4)
      (test 2 bound-id-table-ref table x1)
      (test 2 bound-id-table-ref table x2)
      (test 4 bound-id-table-ref table x3)
      (test 4 bound-id-table-ref table x4)
      (test #t
            contains-same?
            (list 2 4)
            (bound-id-table-map table (lambda (x y) y)))
      (test #t
            contains-same?
            (list 2 4)
            (bound-id-table-values table))
      (test #t
            contains-same?
            (list 2 4)
            (dict-map table (lambda (x y) y)))

      (test 2 bound-id-table-count table)

      
      (test #t
            contains-same?
            (list 2 4)
            (let ([l '()])
              (bound-id-table-for-each
               table
               (lambda (x y)
                 (set! l (cons y l))))
              l))
      (test #t
            contains-same?
            (list 2 4)
            (let ([l '()])
              (dict-for-each
               table
               (lambda (x y)
                 (set! l (cons y l))))
              l)))


    (let ([table (make-free-id-table)])
      (free-id-table-set! table x1 1)
      (free-id-table-set! table x2 2)
      (free-id-table-set! table x3 3)
      (free-id-table-set! table x4 4)
      (test 2 free-id-table-ref table x1)
      (test 2 free-id-table-ref table x2)
      (test 4 free-id-table-ref table x3)
      (test 4 free-id-table-ref table x4)

      (test #t
            contains-same?
            (list 2 4)
            (free-id-table-map table (lambda (x y) y)))
      (test #t
            contains-same?
            (list 2 4)
            (free-id-table-values table))
      (test #t
            contains-same?
            (list 2 4)
            (dict-map table (lambda (x y) y)))
      (test 2 free-id-table-count table)

      (test #t
            contains-same?
            (list 2 4)
            (let ([l '()])
              (free-id-table-for-each
               table
               (lambda (x y)
                 (set! l (cons y l))))
              l))
      (test #t
            contains-same?
            (list 2 4)
            (let ([l '()])
              (dict-for-each
               table
               (lambda (x y)
                 (set! l (cons y l))))
              l))))

  (let-values ([(y1 y2 y3 y4)
                ;; #%plain-module-begin avoids wrapping the ids we want in `call-with-values`
                (syntax-case (expand #'(module m racket/base (#%plain-module-begin (require (prefix-in x: racket/base)) + x:+ - x:-))) ()
                  [(a b c (d e y1 y2 y3 y4))
                   (values (syntax y1)
                           (syntax y2)
                           (syntax y3)
                           (syntax y4))])])

    (let ([table (make-bound-id-table)])
      (bound-id-table-set! table y1 1)
      (bound-id-table-set! table y2 2)
      (bound-id-table-set! table y3 3)
      (bound-id-table-set! table y4 4)
      (test 1 bound-id-table-ref table y1)
      (test 2 bound-id-table-ref table y2)
      (test 3 bound-id-table-ref table y3)
      (test 4 bound-id-table-ref table y4)

      (test #t
            contains-same?
            (list 1 2 3 4)
            (bound-id-table-map table (lambda (x y) y)))
      (test #t
            contains-same?
            (list 1 2 3 4)
            (bound-id-table-values table))
      (test #t
            contains-same?
            (list 1 2 3 4)
            (dict-map table (lambda (x y) y)))
      (test 4 bound-id-table-count table)

      (test #t
            contains-same?
            (list 1 2 3 4)
            (let ([l '()])
              (bound-id-table-for-each
               table
               (lambda (x y)
                 (set! l (cons y l))))
              l))
      (test #t
            contains-same?
            (list 1 2 3 4)
            (let ([l '()])
              (dict-for-each
               table
               (lambda (x y)
                 (set! l (cons y l))))
              l)))

    (let ([table (make-free-id-table)])
      (free-id-table-set! table y1 #f)
      (test #f free-id-table-ref table y1)

      (free-id-table-set! table y1 1)
      (free-id-table-set! table y2 2)
      (free-id-table-set! table y3 3)
      (free-id-table-set! table y4 4)
      (test 2 free-id-table-ref table y1)
      (test 2 free-id-table-ref table y2)
      (test 4 free-id-table-ref table y3)
      (test 4 free-id-table-ref table y4)

      (test #t
            contains-same?
            (list 2 4)
            (free-id-table-map table (lambda (x y) y)))
      (test #t
            contains-same?
            (list 2 4)
            (free-id-table-values table))
      (test #t
            contains-same?
            (list 2 4)
            (dict-map table (lambda (x y) y)))
      (test 2 free-id-table-count table)

      (test #t
            contains-same?
            (list 2 4)
            (let ([l '()])
              (free-id-table-for-each
               table
               (lambda (x y)
                 (set! l (cons y l))))
              l))
      (test #t
            contains-same?
            (list 2 4)
            (let ([l '()])
              (dict-for-each
               table
               (lambda (x y)
                 (set! l (cons y l))))
              l))
      ))

  (test #t contract? (free-id-table/c any/c number?))
  (test #t contract? (bound-id-table/c any/c number?))
  (test #t contract? (bound-id-table/c any/c number? #:immutable #t))
  (test #t contract? (free-id-table/c any/c number? #:immutable #f))
  (test #t contract? (bound-id-table/c any/c number? #:immutable 'dont-care))

  (test #t chaperone-contract? (free-id-table/c any/c number?))
  (test #f flat-contract? (free-id-table/c any/c number?))
  (test #t flat-contract? (free-id-table/c any/c number? #:immutable #t))
  (test #f flat-contract? (free-id-table/c any/c (vectorof number?) #:immutable #t))
;;  --- ryanc: chaperone contracts only for now
;;  (test #f chaperone-contract? (free-id-table/c any/c (new-∀/c 'v)))
;;  (error-test #'(free-id-table/c (new-∀/c 'v) any/c))

  (let ()

    (define (app-ctc ctc value)
      (contract ctc value 'positive 'negative))

    (define (positive-error? exn)
      (and exn:fail:contract?
           (regexp-match? "blaming: positive" (exn-message exn))))
    (define (negative-error? exn)
      (and exn:fail:contract?
           (regexp-match? "blaming: negative" (exn-message exn))))

    (define-syntax-rule (test/blame-pos e)
     (thunk-error-test (lambda () e) #'e positive-error?))
    (define-syntax-rule (test/blame-neg e)
     (thunk-error-test (lambda () e) #'e negative-error?))

    (define-values (a b c d) (values 'A 'B 'C 'D))
    (define tbl (make-free-id-table))
    (free-id-table-set! tbl #'a a)
    (free-id-table-set! tbl #'b b)
    (free-id-table-set! tbl #'c c)
    (free-id-table-set! tbl #'d d)

    (define im-tbl
      ((compose
         (lambda (tbl) (free-id-table-set tbl #'a a))
         (lambda (tbl) (free-id-table-set tbl #'b b))
         (lambda (tbl) (free-id-table-set tbl #'c c))
         (lambda (tbl) (free-id-table-set tbl #'d d)))
       (make-immutable-free-id-table)))


    (test #t free-id-table? (app-ctc (free-id-table/c any/c any/c) (make-free-id-table)))
    (test #t bound-id-table? (app-ctc (bound-id-table/c identifier? number?) (make-bound-id-table)))
    (test #t free-id-table? (app-ctc (free-id-table/c identifier? symbol?) tbl))
    (test #t free-id-table? (app-ctc (free-id-table/c identifier? symbol?) im-tbl))
    (test #t free-id-table? (app-ctc (free-id-table/c identifier? number?) tbl))

    (test/blame-pos (app-ctc (free-id-table/c symbol? symbol? #:immutable #t) im-tbl))
    (test/blame-pos (app-ctc (free-id-table/c identifier? number? #:immutable #t) im-tbl))

    (test/blame-pos (app-ctc (free-id-table/c identifier? number?) im-tbl))
    (test/blame-pos (app-ctc (free-id-table/c symbol? symbol?) im-tbl))

    (define (short-identifier? id)
      (and (identifier? id) (equal? 1 (string-length (symbol->string (syntax-e id))))))
    (define ctced-tbl (app-ctc (free-id-table/c short-identifier? number?) tbl))
    (test/blame-pos (free-id-table-ref ctced-tbl #'a))
    (test/blame-neg (free-id-table-ref ctced-tbl #'ab))
    (test/blame-neg (free-id-table-set! ctced-tbl #'a 'c))
    (test/blame-neg (free-id-table-set! ctced-tbl #'ab 2))
    (test/blame-neg (free-id-table-remove! ctced-tbl #'ab))

    (test/blame-pos
      (let ((ctced-tbl (app-ctc (free-id-table/c symbol? number?) tbl)))
        (free-id-table-iterate-key ctced-tbl (free-id-table-iterate-first ctced-tbl))))
    
    (define/contract ctc-tbl (free-id-table/c any/c number?) (make-free-id-table))
    (test #t void? (free-id-table-set! ctc-tbl #'a 1))
    (test #t void? (free-id-table-set! ctc-tbl #'b 2))
    (test #t number? (free-id-table-ref ctc-tbl #'b))
    (test #t string? (free-id-table-ref ctc-tbl #'c "3"))
    (test #t void? (free-id-table-set! ctc-tbl #'a 4))
    (test #t void? (free-id-table-remove! ctc-tbl #'b))
    (test #t number? (free-id-table-count ctc-tbl))
    (test #t list? (free-id-table-map ctc-tbl (λ (k v) v)))


    ))

;; Tests for id-table-keys
(let ()
  ;; contains-same-keys? : (listof id) (listof id) -> boolean
  (define (contains-same-keys? l1 l2 id=?)
    (and (andmap (lambda (x) (member x l2 id=?)) l1)
         (andmap (lambda (x) (member x l1 id=?)) l2)
         #t))

  (test #t
        contains-same-keys?
        (list #'x #'y)
        (free-id-table-keys
         (make-immutable-free-id-table
          (list (cons #'x 0) (cons #'x 1) (cons #'y 2))))
        free-identifier=?)
  (test #t
        contains-same-keys?
        (list #'x #'y)
        (bound-id-table-keys
         (make-immutable-bound-id-table
          (list (cons #'x 0) (cons #'x 1) (cons #'y 2))))
        bound-identifier=?))

;; Tests for id-table-set*, set*!, update, update!, ref!
(let ()
  (define table (make-bound-id-table))
  (define table2 (make-immutable-bound-id-table))
  (define x0 #'x)
  (define x1 ((make-syntax-introducer) x0))
  (define y0 #'y)
  (define y1 ((make-syntax-introducer) y0))
  (define z0 #'z)
  (define z1 ((make-syntax-introducer) z0))

  (test 0 bound-id-table-ref! table x0 0)
  (test 1 bound-id-table-ref! table x1 1)
  ;; Check that the lambda is immediately called
  (begin
    (test 0 bound-id-table-ref! table z0 (lambda () 0))
    (test 1 bound-id-table-ref! table z1 (lambda () 1)))
  ;; Check that the result of the call was inserted
  (begin
    (test 0 bound-id-table-ref table z0)
    (test 1 bound-id-table-ref table z1))
  (test 0 bound-id-table-ref table x0)
  (test 1 bound-id-table-ref (bound-id-table-update table2 y0 add1 0) y0)
  (test 1 bound-id-table-ref (bound-id-table-set* table2 y0 0 y1 1) y1)
  (test (void) bound-id-table-set*! table y0 1 y1 5)
  (test (void) bound-id-table-update! table y0 add1 0)
  (test 2 bound-id-table-ref table y0))

(let ()
  (define table (make-free-id-table))
  (define table2 (make-immutable-free-id-table))
  (define x0 #'x)
  (define x1 #'x1)
  (define y0 #'y)
  (define y1 #'y1)
  (define z0 #'z)
  (define z1 #'z1)

  (test 0 free-id-table-ref! table x0 0)
  (test 1 free-id-table-ref! table x1 1)
  ;; Check that the lambda is immediately called
  (begin
    (test 0 free-id-table-ref! table z0 (lambda () 0))
    (test 1 free-id-table-ref! table z1 (lambda () 1)))
  ;; Check that the result of the call was inserted
  (begin
    (test 0 free-id-table-ref table z0)
    (test 1 free-id-table-ref table z1))
  (test 0 free-id-table-ref table x0)
  (test 1 free-id-table-ref (free-id-table-update table2 y0 add1 0) y0)
  (test 1 free-id-table-ref (free-id-table-set* table2 y0 0 y1 1) y1)
  (test (void) free-id-table-set*! table y0 1 y1 5)
  (test (void) free-id-table-update! table y0 add1 0)
  (test 2 free-id-table-ref table y0))

(define-syntax name-for-boundmap-test 'dummy)
(define-syntax alias-for-boundmap-test (make-rename-transformer #'name-for-boundmap-test))
(define table (make-free-id-table))
(free-id-table-set! table #'alias-for-boundmap-test 0)
(test 0 free-id-table-ref table #'name-for-boundmap-test)

(report-errs)
