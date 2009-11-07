#lang scheme/gui

(require 2htdp/private/image-more
         "image-util.ss")

(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))
(define expressions
  (parameterize ([current-namespace ns])
    (putenv "PLTSHOWIMAGES" "show")
    (let-values ([(in out) (make-pipe)])
      (thread
       (λ () 
         (parameterize ([current-output-port out])
           (dynamic-require "image.scrbl" #f))
         (close-output-port out)))
      (let loop ()
        (let ([exp (read in)])
          (if (eof-object? exp)
              '()
              (cons exp (loop))))))))

(define-namespace-anchor image-anchor)
(define image-ns (namespace-anchor->namespace anchor))

(define (handle-image exp)
  (printf "saving ~s\n" exp)
  (parameterize ([current-namespace image-ns])
    (save-image (eval exp)
                (build-path "img" (exp->filename exp)))))

(let ([ht (make-hash)])
  (for-each
   (λ (exp)
     (when (hash-ref ht (exp->filename exp) #f)
       (error 'image-gen.ss
              "~s and ~s go to the same string, namely ~s"
              (hash-ref ht (exp->filename exp))
              exp
              (exp->filename exp)))
     (hash-set! ht (exp->filename exp) exp))
   expressions))

(for-each handle-image expressions)
