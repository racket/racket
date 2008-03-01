#lang scheme

(provide basename
         with-temporary-file
         port->bytes
         port->string
         file->bytes
         file->string)

(require scheme/port)

;; basename : path -> relative-path
;  (from PLaneT dherman/io.plt/1/8/file.ss)
(define (basename p)
  (let-values ([(_ name __) (split-path p)])
    (if (symbol? name) 
        (build-path name)
        name)))

;; with-temporary-file
;;   creates a temporary file and automatically deletes it when finished
; (from PLaneT dherman/io.plt/1/8/io.ss)
(define-syntax with-temporary-file
  (syntax-rules ()
    [(_ file (args ...) e1 e2 ...)
     (let ([file (make-temporary-file args ...)])
       (dynamic-wind
        void
        (lambda () e1 e2 ...)
        (lambda ()
          (when (file-exists? file)
            (delete-file file)))))]))

(define (port->bytes port)
  (let ([ob (open-output-bytes)])
    (copy-port port ob)
    (get-output-bytes ob)))

(define (port->string port)
  (let ([os (open-output-string)])
    (copy-port port os)
    (get-output-string os)))
  
(define (file->bytes path)
  (call-with-input-file path
    port->bytes))

(define (file->string path)
  (call-with-input-file path
    port->string))



