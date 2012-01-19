#lang racket
(require setup/parallel-build)

(define tmp1 (make-temporary-file))
(define tmp2 (make-temporary-file))

(define (mk file)
  (with-output-to-file file 
    #:exists 'truncate
    (lambda ()
      (printf "#lang racket/base\n"))))

(mk tmp1)
(mk tmp2)

(parallel-compile-files 
 (list tmp1 tmp2)
 #:worker-count 2
 #:handler (lambda (type work msg out err)
             (match type
               ['done (printf " Made ~a\n" work)]
               ['output (printf " Output from: ~a\n~a~a" work out err)]
               [else (eprintf " Error compiling ~a\n~a\n~a~a" work msg out err)])))

(define (delete-files f)
  (delete-file f)
  (let-values ([(base name dir?) (split-path f)])
    (delete-file (build-path base "compiled" (path-add-suffix name #".dep")))
    (delete-file (build-path base "compiled" (path-add-suffix name #".zo")))))

(delete-files tmp1)
(delete-files tmp2)
