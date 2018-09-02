#lang racket/base
(require syntax/modcollapse)

(provide languages->libraries)

;; Used to implement the `++lang` flag for `raco exe`
(define (languages->libraries langs #:who who)
  (apply
   append
   (for/list ([lang (in-list langs)])
     (define in (open-input-string lang))
     (define mod (read in))
     (define reader-mod
       (let ([submod (collapse-module-path-index
                      (module-path-index-join
                       `(submod "." reader)
                       (module-path-index-join mod #f)))])
         (if (module-declared? submod #t)
             submod
             (collapse-module-path-index
              (module-path-index-join
               "lang/reader.rkt"
               (module-path-index-join mod #f))))))
     (unless (module-declared? reader-mod #t)
       (raise-user-error who
                         (string-append
                          "cannot find language module\n"
                          "  language: ~a"
                          "  module path: ~a")
                         lang
                         reader-mod))
     (define get-info-proc (dynamic-require reader-mod 'get-info (lambda ()
                                                                   (lambda args
                                                                     (lambda args #f)))))
     (define reader-mods (make-hash))
     (hash-set! reader-mods reader-mod #t)
     (define get-info (parameterize ([current-reader-guard
                                      ;; Record potential chains of reader modules.
                                      ;; For example, the `s-exp` reader chains to
                                      ;; other reader modules.
                                      (lambda (mod)
                                        (hash-set! reader-mods mod #t)
                                        mod)])
                        (get-info-proc in #f #f #f #f)))
     (define mod-lang-mod (get-info 'module-language #f))
     (unless mod-lang-mod
        (raise-user-error who
                          (string-append
                           "cannot extract module language\n"
                           "  language: ~a\n"
                           "  info field not available: 'module-language")
                          lang))
     (cons mod-lang-mod
           (hash-keys reader-mods)))))
