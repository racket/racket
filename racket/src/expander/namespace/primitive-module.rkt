#lang racket/base
(require "../common/module-path.rkt"
         "../syntax/module-binding.rkt"
         "../host/linklet.rkt"
         "namespace.rkt"
         "module.rkt"
         "provided.rkt")

;; Used from the virtual machine to support C "extensions" that
;; declare modules

(provide declare-primitive-module!)

(define (declare-primitive-module! name inst in-ns protected cross-phase-persistent?)
  (define mpi (module-path-index-join (list 'quote name) #f))
  (declare-module!
   in-ns
   (make-module #:source-name (current-module-declare-source)
                #:cross-phase-persistent? cross-phase-persistent?
                #:no-protected? (zero? (hash-count protected))
                #:self mpi
                #:provides
                (hasheqv 0 (for/hash ([sym (in-list (instance-variable-names inst))])
                             (define binding (make-module-binding mpi 0 sym))
                             (values sym
                                     (if (hash-ref protected sym #f)
                                         (provided binding #t #f)
                                         binding))))
                #:instantiate-phase-callback
                (lambda (data-box ns phase-shift phase-level self bulk-binding-registry insp)
                  (when (= 0 phase-level)
                    (for ([sym (in-list (instance-variable-names inst))])
                      (define val (instance-variable-value inst sym))
                      (namespace-set-variable! ns 0 sym val)))))
   (substitute-module-declare-name name)))
