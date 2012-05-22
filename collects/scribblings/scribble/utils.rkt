#lang racket/base

(require scribble/core
         scribble/html-properties
         scribble/manual
         (prefix-in racket: scribble/racket)
         (prefix-in scribble: scribble/reader))

(define-syntax bounce-for-label
  (syntax-rules (all-except)
    [(_ (all-except mod (id ...) (id2 ...)))
     (begin (require (for-label (except-in mod id ...)))
            (provide (for-label (except-out (all-from-out mod) id2 ...))))]
    [(_ mod) (begin (require (for-label mod))
                    (provide (for-label (all-from-out mod))))]
    [(_ mod ...) (begin (bounce-for-label mod) ...)]))

(bounce-for-label (all-except racket (abstract link) ())
                  scribble/core
                  scribble/base-render
                  scribble/decode
                  scribble/manual
                  scribble/racket
                  scribble/html-properties
                  scribble/latex-properties
                  scribble/eval
                  scribble/bnf)

(provide scribble-examples litchar/lines)

(define (as-flow e)
  (if (block? e) e (make-paragraph plain (list e))))

(define (litchar/lines . strs)
  (let ([strs (regexp-split #rx"\n" (apply string-append strs))])
    (if (= 1 (length strs))
      (litchar (car strs))
      (make-table
       plain
       (map (lambda (s) ; the nbsp is needed for IE
              (list (as-flow (if (string=? s "") 'nbsp (litchar s)))))
            strs)))))

(define spacer (hspace 2))

(define ((norm-spacing base) p)
  (cond [(and (syntax->list p) (not (null? (syntax-e p))))
         (let loop ([e (syntax->list p)]
                    [line (syntax-line (car (syntax-e p)))]
                    [pos base]
                    [second #f]
                    [accum null])
           (if (null? e)
             (datum->syntax
              p (reverse accum)
              (list (syntax-source p) (syntax-line p) base (add1 base)
                    (- pos base))
              p)
             (let* ([v ((norm-spacing (if (= line (syntax-line (car e)))
                                        pos
                                        (or second pos)))
                        (car e))]
                    [next-pos (+ (syntax-column v) (syntax-span v) 1)])
               (loop (cdr e)
                     (syntax-line v)
                     next-pos
                     (or second next-pos)
                     (cons v accum)))))]
        [else (datum->syntax
               p (syntax-e p)
               (list (syntax-source p) (syntax-line p) base (add1 base) 1)
               p)]))

(define (scribble-examples . lines)
  (define reads-as (make-paragraph plain (list spacer "reads as" spacer)))
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
          (let* ([r (reverse r)]
                 [r (if newlines?
                      (cdr (apply append (map (lambda (x) (list #f x)) r)))
                      r)])
            (make-table
             plain
             (map (lambda (x)
                    (let ([@expr (if x (litchar/lines (car x)) "")]
                          [sexpr (if x
                                   (racket:to-paragraph
                                    ((norm-spacing 0) (cadr x)))
                                   "")]
                          [reads-as (if x reads-as "")])
                      (map as-flow (list spacer @expr reads-as sexpr))))
                  r))))))))

;; stuff for the scribble/text examples

(require racket/list (for-syntax racket/base racket/list))

(define max-textsample-width 45)

(define (textsample-verbatim-boxes line in-text out-text more)
  (define (split str) (regexp-split #rx"\n" str))
  (define strs1 (split in-text))
  (define strs2 (split out-text))
  (define strsm (map (compose split cdr) more))
  (define (str->elts str)
    (let ([spaces (regexp-match-positions #rx"(?:^| ) +" str)])
      (if spaces
        (list* (substring str 0 (caar spaces))
               (hspace (- (cdar spaces) (caar spaces)))
               (str->elts (substring str (cdar spaces))))
        (list (make-element 'tt (list str))))))
  (define (make-line str)
    (if (equal? str "")
      ;;FIXME: this works in html, but in latex it creates a redundant newline
      (list (as-flow (make-element 'newline '())))
      (list (as-flow (make-element 'tt (str->elts str))))))
  (define (small-attr attr)
    (make-style attr (list
                      (make-attributes '([style . "font-size: 82%;"])))))
  (define (make-box strs)
    (make-table (small-attr "Shaded") (map make-line strs)))
  (define filenames (map car more))
  (define indent (let ([d (- max-textsample-width
                             (for*/fold ([m 0])
                                        ([s (in-list (cons strs1 strsm))]
                                         [s (in-list s)])
                               (max m (string-length s))))])
                   (if (negative? d)
                     (error 'textsample-verbatim-boxes
                            "left box too wide for sample at line ~s" line)
                     (make-element 'tt (list (hspace d))))))
  ;; Note: the font-size property is reset for every table, so we need it
  ;; everywhere there's text, and they don't accumulate for nested tables
  (values
   (make-table 
     (make-style #f
                 (list (make-table-columns (list (make-style (if (null? filenames)
                                                                 "Short"
                                                                 "Medium")
                                                             '(right top))
                                                 (make-style #f '(left top))))))
     (cons (list (as-flow (make-table (small-attr #f)
                                      (list (list (as-flow indent)))))
                 (as-flow (make-box strs1)))
           (map (lambda (file strs)
                  (let* ([file (make-element 'tt (list file ":" 'nbsp))]
                         [file (list (make-element 'italic (list file)))])
                    (list (as-flow (make-element (make-style #f (list (make-background-color-property '(232 232 255)))) file))
                          (as-flow (make-box strs)))))
                filenames strsm)))
   (make-box strs2)))

(define (textsample line in-text out-text more)
  (define-values (box1 box2)
    (textsample-verbatim-boxes line in-text out-text more))
  (make-table 
   (make-style #f (list (make-table-columns (list (make-style #f '(left vcenter))
                                                  (make-style "Short" '(left vcenter))
                                                  (make-style #f '(left vcenter))))))
    (list (map as-flow (list box1 (make-paragraph plain '(nbsp rarr nbsp)) box2)))))

(define-for-syntax tests-ids #f)

(provide initialize-tests)
(define-syntax (initialize-tests stx)
  (set! tests-ids (map (lambda (x) (datum->syntax stx x stx))
                       '(tests add-to-tests)))
  (with-syntax ([(tests add-to-tests) tests-ids])
    #'(begin (provide tests)
             (define-values (tests add-to-tests)
               (let ([l '()])
                 (values (lambda () (reverse l))
                         (lambda (x) (set! l (cons x l)))))))))

(provide example)
(define-syntax (example stx)
  (define sep-rx  #px"^---[*]{3}---(?: +(.*))?$")
  (define file-rx #rx"^[a-z0-9_.+-]+$")
  (define-values (body hidden?)
    (syntax-case stx ()
      [(_ #:hidden x ...) (values #'(x ...) #t)]
      [(_          x ...) (values #'(x ...) #f)]))
  (let loop ([xs body] [text '(#f)] [texts '()])
    (syntax-case xs ()
      [("\n" sep "\n" . xs)
       (and (string? (syntax-e #'sep)) (regexp-match? sep-rx (syntax-e #'sep)))
       (let ([m (cond [(regexp-match sep-rx (syntax-e #'sep)) => cadr]
                      [else #f])])
         (if (and m (not (regexp-match? file-rx m)))
           (raise-syntax-error #f "bad filename specified" stx #'sep)
           (loop #'xs
                 (list (and m (datum->syntax #'sep m #'sep #'sep)))
                 (cons (reverse text) texts))))]
      [(x . xs) (loop #'xs (cons #'x text) texts)]
      [() (let ([texts (reverse (cons (reverse text) texts))]
                [line  (syntax-line stx)])
            (define-values (files i/o) (partition car texts))
            (unless ((length i/o) . = . 2)
              (raise-syntax-error
               'example "need at least an input and an output block" stx))
            (with-syntax ([line  line]
                          [((in ...) (out ...)) (map cdr i/o)]
                          [((file text ...) ...) files]
                          [add-to-tests (cadr tests-ids)])
              (quasisyntax/loc stx
                (let* ([in-text  (string-append in ...)]
                       [out-text (string-append out ...)]
                       [more (list (cons file (string-append text ...)) ...)])
                  (add-to-tests (list line in-text out-text more))
                  #,(if hidden? #'""
                        #'(textsample line in-text out-text more))))))]
      [_ (raise-syntax-error #f "no separator found in example text")])))

(provide ltx ltxe ltxd)
(define (ltx s) (tt "\\" s)) ; command
(define (ltxe s) (tt s)) ; enviornment
(define (ltxd n s)
  (make-element #f (cons (index (list s) (ltx s))
                         (for/list ([i (in-range n)]) (tt "{}")))))
