#lang racket/base
(require "wrap.rkt")

(provide infer-procedure-name)

(define (infer-procedure-name orig-s new-s)
  (define inferred-name (wrap-property orig-s 'inferred-name))
  (cond
    [(symbol? inferred-name)
     ;; Let propagation of properties (at the call site of
     ;; `infer-procedure-name`) take care of it
     new-s]
    [else
     (define-values (src line col pos span) (wrap-source orig-s))
     (define (add-property str)
       (wrap-property-set (reannotate orig-s new-s)
                          'inferred-name
                          ;; Hack: starting with "[" means
                          ;; "derived from path"
                          (string->symbol (string-append "[" str))))
     (cond
       [(and (or (path? src) (string? src)) line col)
        (add-property
         (string-append (source->string src)
                        ":"
                        (number->string line)
                        ":"
                        (number->string col)))]
       [(and (or (path? src) (string? src)) src pos)
        (add-property
         (string-append (source->string src)
                        "::"
                        (number->string pos)))]
       [(void? inferred-name)
        ;; We can't provide a source name, but explicity
        ;; suppress any other inferred name:
        (wrap-property-set (reannotate orig-s new-s)
                           'inferred-name
                           ;; Hack: "[" means "no name"
                           '|[|)]
       [else new-s])]))

(define (source->string src)
  (define str (if (string? src) src (path->string src)))
  (define short-str
    (cond
      [((string-length str) . < . 20) (string-copy str)]
      [else (string-append "..." (substring str (- (string-length str) 19)))]))
  (for ([i (in-range (string-length short-str))])
    (when (char=? #\\ (string-ref short-str i))
      (string-set! short-str i #\/)))
  short-str)

