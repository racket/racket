#lang racket/base
(require racket/fixnum
         racket/symbol
         "wrap.rkt")

(provide infer-procedure-name)

(define (infer-procedure-name orig-s new-s explicit-unnamed?)
  (define inferred-name (wrap-property orig-s 'inferred-name))
  (cond
    [(symbol? inferred-name)
     (define s (symbol->immutable-string inferred-name))
     (cond
       [(and (fx> (string-length s) 0)
             (let ([ch (string-ref s 0)])
               (or (char=? #\[ ch)
                   (char=? #\] ch))))
        ;; Symbolic name starts with "[" or "]". To avoid confusing
        ;; it with a path or "no name" encoding, add an extra
        ;; "]" to be stripped away.
        (wrap-property-set (reannotate orig-s new-s)
                          'inferred-name
                          (string->symbol (string-append-immutable "]" s)))]
       [else
        ;; Let propagation of properties (at the call site of
        ;; `infer-procedure-name`) take care of it
        new-s])]
    [else
     (define-values (src line col pos span) (wrap-source orig-s))
     (define (add-property str)
       (wrap-property-set (reannotate orig-s new-s)
                          'inferred-name
                          ;; Starting with "[" means "derived from
                          ;; path". This distinction is used when
                          ;; printing function names in a stack trace.
                          ;; Furthermore, "!" or "^" after "[" indicates
                          ;; methodness or not, so add an explicit "^"
                          ;; if necessary.
                          (let ([prefix (if (or (char=? (string-ref str 0) #\!)
                                                (char=? (string-ref str 0) #\^))
                                            "[^"
                                            "[")])
                            (string->symbol (string-append-immutable prefix str)))))
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
       [(or explicit-unnamed?
            (void? inferred-name))
        ;; We can't provide a source name, but explicitly
        ;; suppress any other inferred name:
        (wrap-property-set (reannotate orig-s new-s)
                           'inferred-name
                           ;; "[" means "no name"
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
