#lang racket
(require "path-utils.rkt"
         "run-collect.rkt"
         "replay.rkt"
         racket/runtime-path
         racket/system)

(match-define 
 (list* command real-args)
 (vector->list (current-command-line-arguments)))

(define-match-expander solo-flag
  (syntax-rules () 
    [(_ [flag ...] everything-else) 
     (list* (or (regexp (string-append "^" (regexp-quote flag) "$" ) (list _))
                ...)
            everything-else)]))
(define-match-expander emopt-flag
  (syntax-rules () 
    [(_ [flag ...] everything-else) 
     (list* (or (regexp (string-append "^" (regexp-quote flag) "(.+)$") (list _ _))
                ...)
            everything-else)]))
(define-match-expander opt-flag
  (syntax-rules () 
    [(_ [flag ...] opt everything-else) 
     (list* (or (regexp (string-append "^" (regexp-quote flag) "$") (list _))
                ...)
            (and opt (not (? flag?)))
            everything-else)]))

(define (flag? x)
  (equal? #\- (string-ref x 0)))

(define-syntax-rule (define-snocer var setter!)
  (begin (define var empty)
         (define (setter! x)
           (set! var (append var (list x))))))

(define-snocer outputs output!)
(define-snocer inputs input!)

(define loop
  (match-lambda
    [(solo-flag ["--version" "-c" "-V" "-v" "-E" "-traditional-cpp" "-g" "-print-search-dirs" "-print-multi-os-directory" "-pthread" "-dynamiclib" "-all_load"] as) (loop as)]
    [(emopt-flag ["-O" "-X" "-D" "-m" "-l" "-W" "-I" "-f" "-F"] as) (loop as)]
    [(opt-flag ["-install_name" "-compatibility_version" "-current_version" "-framework"] f as) (loop as)]
    [(opt-flag ["-o"] f as) (output! f) (loop as)]
    [(list* (and (not (? flag?)) f) as) (input! f) (loop as)]
    [args
     (unless (empty? args)
       (error 'drdr-cc "Unhandled args: ~S  [~S]" args real-args))]))

(loop real-args)

(define cc-path
  (find-executable-path command))

(define the-input
  (match inputs
    [(list f) f]
    [_ #f]))

(define-runtime-path output-dir "output")

(if the-input
    (local [(define the-input-base
              (apply build-path output-dir (filter path-for-some-system? (explode-path the-input))))
            (define status
              (run/collect/wait 
               #:env (make-hash)
               #:timeout (* 60 60)
               (path->string cc-path)
               real-args))]
      
      (make-parent-directory the-input-base)
      (with-output-to-file (path-add-suffix the-input-base ".log") #:exists 'truncate/replace
        (lambda () (write status)))
      
      (replay-status status))
    (exit (apply system*/exit-code cc-path real-args)))
