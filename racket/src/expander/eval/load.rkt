#lang racket/base
(require racket/private/check
         racket/private/executable-path
         "module.rkt"
         "parameter.rkt")

(provide load
         load-extension
         load/use-compiled
         embedded-load)

(define/who (load s)
  (check who path-string? s)
  (call-with-current-load-relative-directory
   s
   (lambda ()
     ((current-load) s #f))))

(define/who (load-extension s)
  (check who path-string? s)
  (call-with-current-load-relative-directory
   s
   (lambda ()
     ((current-load-extension) s #f))))

(define (call-with-current-load-relative-directory p thunk)
  (define-values (base name dir?) (split-path p))
  (parameterize ([current-load-relative-directory
                  (if (eq? base 'relative)
                      (current-directory)
                      (path->complete-path base))])
    (thunk)))

;; ----------------------------------------

(define/who (load/use-compiled f)
  (check who path-string? f)
  ((current-load/use-compiled) f #f))

;; used for the -k command-line argument:
(define (embedded-load start end str as-predefined?)
  (let* ([s (if str
                str
                (let* ([sp (find-system-path 'exec-file)] 
                       [exe (find-executable-path sp #f)]
                       [start (or (string->number start) 0)]
                       [end (or (string->number end) 0)])
                  (with-input-from-file exe 
                    (lambda ()
                      (file-position (current-input-port) start)
                      (read-bytes (max 0 (- end start)))))))]
         [p (open-input-bytes s)])
    (let loop ()
      (let ([e (parameterize ([read-accept-compiled #t]
                              [read-accept-reader #t]
                              [read-accept-lang #t]
                              [read-on-demand-source #t])
                 (read p))])
        (unless (eof-object? e)
          (parameterize ([current-module-declare-as-predefined as-predefined?])
            ((current-eval) e))
          (loop))))))
