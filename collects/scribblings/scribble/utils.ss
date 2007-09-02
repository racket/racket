
(module utils mzscheme
  (require (lib "struct.ss" "scribble")
           (lib "manual.ss" "scribble")
           (prefix scheme: (lib "scheme.ss" "scribble"))
           (prefix scribble: (lib "reader.ss" "scribble"))
           (lib "string.ss"))

  (define-syntax bounce-for-label
    (syntax-rules ()
      [(_ mod) (begin
                 (require-for-label mod)
                 (provide-for-label (all-from mod)))]
      [(_ mod ...) (begin (bounce-for-label mod) ...)]))

  (bounce-for-label (lib "lang.ss" "big")
                    (lib "struct.ss" "scribble")
                    (lib "base-render.ss" "scribble")
                    (lib "decode.ss" "scribble")
                    (lib "basic.ss" "scribble")
                    (lib "manual.ss" "scribble")
                    (lib "scheme.ss" "scribble")
                    (lib "eval.ss" "scribble")
                    (lib "bnf.ss" "scribble"))

  (provide scribble-examples litchar/lines)

  (define (litchar/lines . strs)
    (let ([strs (regexp-split #rx"\n" (apply string-append strs))])
      (if (= 1 (length strs))
        (litchar (car strs))
        (make-table
         #f
         (map (lambda (s)
                (list (make-flow (list (make-paragraph 
					(if (string=? s "")
					    '(nbsp) ; needed for IE
					    (list (litchar s))))))))
              strs)))))

  (define (as-flow e)
    (make-flow (list (if (flow-element? e)
                         e
                         (make-paragraph (list e))))))

  (define spacer (hspace 2))

  (define ((norm-spacing base) p)
    (cond
     [(and (syntax->list p)
           (not (null? (syntax-e p))))
      (let loop ([e (syntax->list p)]
                 [line (syntax-line (car (syntax-e p)))]
                 [pos base]
                 [second #f]
                 [accum null])
        (cond
         [(null? e)
          (datum->syntax-object
           p
           (reverse accum)
           (list (syntax-source p)
                 (syntax-line p)
                 base
                 (add1 base)
                 (- pos base))
           p)]
         [else
          (let* ([v ((norm-spacing (if (= line (syntax-line (car e)))
                                       pos
                                       (or second pos)))
                     (car e))]
                 [next-pos (+ (syntax-column v) (syntax-span v) 1)])
            (loop (cdr e)
                  (syntax-line v)
                  next-pos
                  (or second next-pos)
                  (cons v accum)))]))]
     [else
      (datum->syntax-object
       p
       (syntax-e p)
       (list (syntax-source p)
             (syntax-line p)
             base
             (add1 base)
             1)
       p)]))

  (define (scribble-examples . lines)
    (define reads-as (make-paragraph (list spacer "reads as" spacer)))
    (let* ([lines (apply string-append lines)]
           [p (open-input-string lines)])
      (port-count-lines! p)
      (let loop ([r '()] [newlines? #f])
        (regexp-match? #px#"^[[:space:]]*" p)
        (let* ([p1  (file-position p)]
               [stx (scribble:read-syntax #f p)]
               [p2  (file-position p)])
          (if (not (eof-object? stx))
            (let ([str (substring lines p1 p2)])
              (loop (cons (list str stx) r)
                    (or newlines? (regexp-match? #rx#"\n" str))))
            (let* ([r (reverse! r)]
                   [r (if newlines?
                        (cdr (apply append! (map (lambda (x) (list #f x)) r)))
                        r)])
              (make-table
               #f
               (map (lambda (x)
                      (let ([@expr (if x (litchar/lines (car x)) "")]
                            [sexpr (if x
                                     (scheme:to-paragraph
                                      ((norm-spacing 0) (cadr x)))
                                     "")]
                            [reads-as (if x reads-as "")])
                        (map as-flow (list spacer @expr reads-as sexpr))))
                    r)))))))))
