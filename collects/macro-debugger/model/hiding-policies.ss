
#lang scheme/base
(require (for-syntax scheme/base)
         scheme/match
         syntax/boundmap
         "reductions-config.ss")
(provide make-policy
         standard-policy
         base-policy
         hide-all-policy
         hide-none-policy)

;; make-policy : bool^4 (listof (identifier bindinglist (bool -> void) -> void))
;;            -> identifier -> bool
(define (make-policy hide-mzscheme?
                     hide-libs?
                     hide-contracts?
                     hide-transformers?
                     specialized-policies)
  (lambda (id)
    (define now (phase))
    (define binding
      (cond [(= now 0) (identifier-binding id)]
            [(= now 1) (identifier-transformer-binding id)]
            [else #f]))
    (define-values (def-mod def-name nom-mod nom-name)
      (if (pair? binding)
          (values (car binding)
                  (cadr binding)
                  (caddr binding)
                  (cadddr binding))
          (values #f #f #f #f)))
    (let/ec return
      (let loop ([policies specialized-policies])
        (when (pair? policies)
          ((car policies) id binding return)
          (loop (cdr policies))))
      (cond [(and hide-mzscheme? def-mod (scheme-module? def-mod))
             #f]
            [(and hide-libs? def-mod (lib-module? def-mod))
             #f]
            [(and hide-contracts? def-name
                  (regexp-match #rx"^provide/contract-id-"
                                (symbol->string def-name)))
             #f]
            [(and hide-transformers? (positive? now))
             #f]
            [else #t]))))

(define standard-policy
  (make-policy #t #t #t #t null))

(define base-policy
  (make-policy #t #f #f #f null))

(define (hide-all-policy id) #f)
(define (hide-none-policy id) #t)


;;

(define (scheme-module? mpi)
  (let ([abs (find-absolute-module-path mpi)])
    (and abs
         (or (base-module-path? abs)
             (scheme-lib-module-path? abs)))))

(define (lib-module? mpi)
  (let ([abs (find-absolute-module-path mpi)])
    (and abs (lib-module-path? abs))))


(define (find-absolute-module-path mpi)
  (and (module-path-index? mpi)
       (let-values ([(path rel) (module-path-index-split mpi)])
         (cond [(and (pair? path) (memq (car path) '(quote lib planet)))
                path]
               [(symbol? path) path]
               [(string? path) (find-absolute-module-path rel)]
               [else #f]))))

(define (base-module-path? mp)
  (and (pair? mp)
       (eq? 'quote (car mp))
       (regexp-match #rx"^#%" (symbol->string (cadr mp)))))

(define (scheme-lib-module-path? mp)
  (cond [(symbol? mp)
         (scheme-collection-name? (symbol->string mp))]
        [(and (pair? mp) (eq? (car mp) 'lib))
         (cond [(string? (cadr mp)) (null? (cddr mp))
                (scheme-collection-name? (cadr mp))]
               [(symbol? (cadr mp))
                (scheme-collection-name? (symbol->string (cadr mp)))]
               [else #f])]
        [else #f]))

(define (scheme-collection-name? path)
  (or (regexp-match? #rx"^scheme/base(/.)?" path)
      (regexp-match? #rx"^mzscheme(/.)?" path)))

(define (lib-module-path? mp)
  (or (symbol? mp)
      (and (pair? mp) (memq (car mp) '(lib planet)))))
