#lang racket/base

(provide no-c-prefix
         cify
         genid
         reset-genid-counters!)

;; The "c_" prefix is reserved for names that are provided
;; by the Racket glue or that we make up, where we choose
;; a prefix for any made-up name that won't conflict with
;; the Racket-glue names.
(define (no-c-prefix s)
  (if (regexp-match? #rx"c_" (symbol->string s))
      (string->symbol (format "o_~a" s))
      s))

(define c-names (make-hasheq))
(define used-names (make-hasheq))

;; Count reserved names as used, as well as anything
;; that doesn't fit the "c_..." or "scheme_..." pattern.
(for ([n (in-list '(void int long double short
                         if else return const goto switch case
                         SAME_OBJ))])
  (hash-set! used-names n #t))

(define replacements
  '((#rx"!" "_B_")
    (#rx"-" "_")
    (#rx"[.]" "_T_")
    (#rx"[?]" "_Q_")
    (#rx"[+]" "_P_")
    (#rx":" "_C_")
    (#rx"/" "_S_")
    (#rx"<" "_L_")
    (#rx">" "_G_")
    (#rx"#%" "_HP_")
    (#rx"[*]" "_R_")
    (#rx"[=]" "_E_")
    (#rx"[$]" "_M_")
    (#rx"#" "_H_")
    (#rx"~" "_I")
    (#rx"@" "_A_")))

(define (cify name)
  (or (hash-ref c-names name #f)
      (let* ([c-name
              (string->symbol
               (regexp-replace*
                #rx"^(?=[0-9]|_[_A-Z]|scheme|SCHEME|Scheme|MZ_)" ; c_ prefix is avoided via `no-c-prefix`
                (for/fold ([s (symbol->string name)]) ([r (in-list replacements)])
                  (regexp-replace* (car r) s (cadr r)))
                "o_"))]
             [c-name (if (not (hash-ref used-names c-name #f))
                         c-name
                         ;; collisions should be very rare
                         (let loop ([i 2])
                           (define new-c-name (string->symbol (format "~a_~a" c-name i)))
                           (if (hash-ref used-names new-c-name #f)
                               (loop (add1 i))
                               new-c-name)))])
        (hash-set! c-names name c-name)
        (hash-set! used-names c-name #t)
        c-name)))

;; ----------------------------------------

(define compiler-ids (make-hasheq))

(define (genid in-s)
  (define s (if (string? in-s) (string->symbol in-s) in-s))
  (define c (hash-ref compiler-ids s 0))
  (hash-set! compiler-ids s (add1 c))
  (string->symbol (format "~a~a" s c)))

(define (reset-genid-counters! l)
  (for ([c (in-list l)])
    (hash-set! compiler-ids c 0)))
