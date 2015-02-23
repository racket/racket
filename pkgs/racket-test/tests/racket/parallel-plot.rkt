#lang racket/base
(require racket/file
         racket/place)

(define N 3)

(define base-dir (build-path (find-system-path 'temp-dir) "parplot"))
(make-directory* base-dir)

(define (go n)
  (define dir (build-path base-dir (format "sub~a" n)))
  (make-directory* dir)
  (printf "going ~s\n" n)
  (parameterize ([current-namespace (make-base-namespace)]
                 [current-directory (collection-path "plot/scribblings")])
    ((dynamic-require 'scribble/render 'render)
     (list (dynamic-require '(lib "plot/scribblings/plot.scrbl") 
                            'doc))
     (list "plot")
     #:render-mixin (dynamic-require 'scribble/latex-render 'render-mixin)
     #:dest-dir dir))
  (printf "done ~a\n" n))

(define (run)
  (define ps
    (for/list ([i N])
      (if #f
          (thread (lambda () (go i)))
          (let ()
            (define p (place ch (go (sync ch))))
            (place-channel-put p i)
            (place-dead-evt p)))))
  (time
   (for ([p ps])
     (sync p))))

(module+ main
  (run)
  (delete-directory/files base-dir))
