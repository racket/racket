#lang racket/base
(require racket/file
         racket/system
         racket/future
         racket/fasl
         racket/match
         racket/path
         racket/list
         racket/serialize
         (for-syntax syntax/parse
                     racket/base
                     racket/file))

(provide
  dynamic-place
  place-wait
  place-kill
  place-channel-get
  place-channel-put
  place-channel-put/get
  place-child-channel
  place/base
  map-reduce/lambda
  split-n)

(define-struct place-s (ch subprocess-obj stderr))
(define-struct place-channel-s (in out))

(define (resolve->channel o)
  (match o
    [(? place-s? p) (place-s-ch p)]
    [(? place-channel-s? p) p]))

;; create a place-channel, should be called from children workers
(define (place-child-channel) (make-place-channel-s (current-input-port) (current-output-port)))

;; send x on channel ch
(define (place-channel-put ch x)
  (define out (place-channel-s-out (resolve->channel ch)))
  (write (s-exp->fasl (serialize x)) out)
  (flush-output out))

;; receives msg on channel ch
(define (place-channel-get ch)
  (deserialize (fasl->s-exp (read (place-channel-s-in (resolve->channel ch))))))

;; create a place given a module file path and a func-name to invoke
(define (dynamic-place module-name func-name)
  (define (send/msg x ch)
    (write x ch)
    (flush-output ch))
  (define (module-name->bytes name)
    (cond
      [(path? name)   (path->bytes name)]
      [(string? name) (string->bytes/locale name)]
      [(bytes? name)  name]
      [(and (list? name)
            (= 3 (length name))
            (eq? (car name) 'submod))
       `(submod ,(module-name->bytes (cadr name)) ,(caddr name))]
      [else (error 'module->path "expects a path or string")]))
  (define (current-executable-path) 
    (parameterize ([current-directory (find-system-path 'orig-dir)])
      (find-executable-path (find-system-path 'exec-file) #f)))
  (define (current-collects-path)
     (let ([p (find-system-path 'collects-dir)])
                           (if (complete-path? p)
                               p
                               (path->complete-path p (or (path-only (current-executable-path))
                                                          (find-system-path 'orig-dir))))))
  (define worker-cmdline-list (list (current-executable-path) "-X" (path->string (current-collects-path)) "-e" "(eval(read))"))
  (let-values ([(process-handle out in err) (apply subprocess #f #f (current-error-port) worker-cmdline-list)])
    (send/msg `((dynamic-require ,(let ([bstr (module-name->bytes module-name)])
                                    (if (bytes? bstr)
                                        `(bytes->path ,bstr)
                                        `(list ',(car bstr) (bytes->path ,(cadr bstr)) ',(caddr bstr))))
                                 (quote ,func-name))) 
              in)
    (make-place-s (make-place-channel-s out in) process-handle err)))

;; kill a place
(define (place-kill pl) (subprocess-kill (place-s-subprocess-obj pl) #t))

;; wait for a place to finish 
(define (place-wait pl)
  (let ((spo (place-s-subprocess-obj pl)))
    (subprocess-wait spo)
    (subprocess-status spo)))

(define (place-channel-put/get ch x)
  (place-channel-put ch x)
  (place-channel-get ch))

;; splits lst into n equal pieces
(define (split-n n lst)
  (match n 
    [(? (lambda (x) (x . < . 0))) (raise (format "split-n: n: ~a less than 0" n))]
    [1  lst]
    [else
      (define splits (sub1 n))
      (define-values (q r) (quotient/remainder (length lst) n))
      (let loop ([lst-in lst]
                 [splits splits]
                 [left-overs r]
                 [result null])
        (define have-remainder (left-overs . > . 0))
        (define split-pos (if have-remainder (add1 q) q))
        (define-values (lst1 lst2) (split-at lst-in split-pos))
        (define new-result (cons lst1 result))
        (define new-splits (sub1 splits))
        (if (zero? new-splits)
            (reverse (cons lst2 new-result))
            (loop
              lst2
              new-splits
              (sub1 left-overs)
              new-result)))]))

(define-syntax (place/base stx)
  (syntax-case stx ()
    [(_ module-name (name ch) body ...)
     #'(module module-name racket/base
         (require "place-processes.rkt")
         (provide name)
         (define (name)
           (let ([ch (place-child-channel)])
             body ...)))]))

(define-syntax (place/lambda stx)
  (syntax-case stx ()
    [(_ (name args ...) body ...)
     (begin
       (define (place/current-module-path funcname)
         (with-syntax ([funcname funcname])
           #'(let ([module-path (resolved-module-path-name (variable-reference->resolved-module-path (#%variable-reference)))])
               (dynamic-place module-path (quote funcname)))))
       (with-syntax ([interal-def-name (syntax-local-lift-expression #'(lambda () ((lambda (args ...) body ...) (place-child-channel))))])
         (syntax-local-lift-provide #'(rename interal-def-name name)))
       (place/current-module-path #'name))]))
;; map-reduce/lambda
;; (map-reduce/lambda srclist (my-worker sublist) body ...) -> reduced-value
;; map-reduce/lambda divides srclist into (processor-count) pieces and maps them out to places to be reduced.
;;
;; === READ THIS ===
;; map-reduce/lambda cannot be used in a script or main program file! It must be used in a module without top-level expressions.
;; map-reduce/lambda dynamic-requires the module in which it is defined in children worker processes.
;; reduce-body CANNOT close over any values it can only use the sublist-identifier and top level module defines
;; =================
;;
;; unique-identifier: the body of map-reduce/lambda is lifted to module scope and defined as unique-identifier.  The user does not need to reference unique-identifier, it just needs to be unique in the defining module.
;; sublist-identifier: the sublist map creates will be bound to sublist-identifier in map-reduce/lambda's body.
(define-syntax (map-reduce/lambda stx)
  (syntax-case stx ()
    [(_ lst (name listvar) body ...)
      #'(begin
          (define places
            (for/list ([i (in-range (processor-count))])
              (place/lambda (name ch)
                (place-channel-put ch
                                   ((lambda (listvar) body ...)
                                    (place-channel-get ch))))))

          (for ([p places]
                [item (split-n (processor-count) lst)])
            (place-channel-put p item))
          (define result ((lambda (listvar) body ...) (map place-channel-get places)))
          (map place-wait places)
          (map place-kill places)
          result)]))
