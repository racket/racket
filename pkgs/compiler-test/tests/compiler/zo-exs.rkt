#lang racket
(require compiler/zo-parse
         compiler/zo-marshal
         tests/eli-tester)

(define (read-compiled-bytes bs)
  (parameterize ([read-accept-compiled #t])
    (read (open-input-bytes bs))))

(define (run-compiled-bytes bs [delayed? #t])
  (system "touch test.rkt")
  (system "touch compiled/test_rkt.zo")
  (system (format "racket ~a -t test.rkt" (if delayed? "" "-d"))))

(define (roundtrip ct-in)
  (define ct (if (linkl-directory? ct-in)
                 (linkl-directory
                  (for/hash ([(k v) (linkl-directory-table ct-in)])
                    (if (linkl-bundle? v)
                        (values k
                                (linkl-bundle
                                 (hash-set (linkl-bundle-table v)
                                           'vm
                                           (string->bytes/utf-8 (symbol->string (system-type 'vm))))))
                        (values k v))))
                 ct-in))
  (define bs (zo-marshal ct))
  (test #:failure-prefix (format "~S" ct)
        (test bs
              (zo-parse (open-input-bytes bs)) => ct
              (read-compiled-bytes bs)
              #;(with-output-to-file "compiled/test_rkt.zo" (λ () (write-bytes bs)) #:exists 'replace)
              #;(run-compiled-bytes bs #t)
              #;(run-compiled-bytes bs #f))))

(define mpi (module-path-index-join #f #f))


(test
 (roundtrip
    (compilation-top 0
                     #hash()
                     (prefix 0 empty empty 'insp0)
                     (list 1 (list 2 3) (list 2 3) 4 5)))
 
 (roundtrip
    (compilation-top 0
                     #hash()
                     (prefix 1 empty empty 'insp0)
                     (list (lam 'proc null 0 null #f #(0) '(val/ref) (set 0) 3 1))))
 (roundtrip
    (compilation-top 0
                     #hash()
                     (prefix 1 empty empty 'insp0)
                     (list (lam 'proc null 0 null #f #(0) '(val/ref) #f 3 1))))
 
 #;(roundtrip
  (compilation-top 0 
                   #hash()
                   (prefix 0 empty empty)
                   (let* ([ph (make-placeholder #f)]
                          [x (closure 
                               (lam 'name
                                    empty
                                    0 
                                    empty
                                    #f
                                    #()
                                    empty
                                    0
                                    ph)
                               'name)])
                     (placeholder-set! ph x)
                     (make-reader-graph x))))
 
 ; This should work, but module-path-index-join doesn't create equal? module-path-index's
 #;(roundtrip
    (compilation-top
     0
     #hash()
     (prefix 0 (list #f) (list))
     (mod
      'simple
      'simple
      (module-path-index-join #f #f)
      (prefix
       0
       (list (module-variable 
              (module-path-index-join
               "modbeg.rkt"
               (module-path-index-join
                "pre-base.rkt"
                (module-path-index-join
                 "namespace.rkt"
                 (module-path-index-join "private/base.rkt" (module-path-index-join 'racket/base #f))))) 'print-values 0 0))
       (list))
      (list)
      (list (list 0 (module-path-index-join 'racket/base #f)) (list 1) (list -1) (list #f))
      (list (apply-values
             (toplevel 0 0 #f #t)
             (application
              (primval 231)
              (list 1 'a))))
      (list)
      (list (list) (list) (list))
      2
      (toplevel 0 0 #f #f)
      #(racket/language-info get-info #f)
      #t)))

 (roundtrip 
    (compilation-top 0
                     #hash()
                     (prefix 0 empty empty 'insp0)
                     (current-directory)))
 
 (roundtrip 
    (compilation-top 0
                     #hash()
                     (prefix 0 empty empty 'insp0)
                     (list (current-directory))))
 
 (roundtrip
    (compilation-top
     0
     #hash()
     (prefix 0 empty empty 'insp0)
     (cons #hash()
           #hash())))
 
 (roundtrip
    (compilation-top
     0
     #hash()
     (prefix 0 empty empty 'insp0)
     #hash())))
