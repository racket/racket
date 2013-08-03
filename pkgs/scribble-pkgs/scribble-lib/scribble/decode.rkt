#lang scheme/base
(require "core.rkt"
         "private/provide-structs.rkt"
         "decode-struct.rkt"
         racket/contract/base
         racket/contract/combinator
         scheme/list)

(define (pre-content? i)
  (or (string? i)
      (content? i)
      (and (splice? i)
           (andmap pre-content? (splice-run i)))
      (and (list? i)
           (andmap pre-content? i))
      (void? i)))

(define (pre-flow? i)
  (or (string? i)
      (content? i)
      (block? i)
      (and (splice? i)
           (andmap pre-flow? (splice-run i)))
      (and (list? i)
           (andmap pre-flow? i))
      (void? i)))

(define (pre-part? v)
  (or (pre-flow? v)
      (title-decl? v)
      (part-start? v)
      (part-index-decl? v)
      (part-collect-decl? v)
      (part-tag-decl? v)
      (part? v)
      (and (splice? v)
           (andmap pre-part? (splice-run v)))
      (and (list? v)
           (andmap pre-part? v))))

(provide-structs
 [title-decl ([tag-prefix (or/c false/c string?)]
              [tags (listof tag?)]
              [version (or/c string? false/c)]
              [style style?]
              [content content?])]
 [part-start ([depth integer?]
              [tag-prefix (or/c false/c string?)]
              [tags (listof tag?)]
              [style style?]
              [title content?])]
 [splice ([run list?])]
 [part-index-decl ([plain-seq (listof string?)]
                   [entry-seq list?])]
 [part-collect-decl ([element (or/c element? part-relative-element?)])]
 [part-tag-decl ([tag tag?])])

(provide whitespace?
         pre-content?
         pre-flow?
         pre-part?)

(provide/contract
 [decode (-> (listof pre-part?)
             part?)]
 [decode-part  (-> (listof pre-part?)
                   (listof string?)
                   (or/c #f content?)
                   exact-nonnegative-integer?
                   part?)]
 [decode-flow  (-> (listof pre-flow?)
                   (listof block?))]
 [decode-paragraph (-> (listof pre-content?)
                       paragraph?)]
 [decode-compound-paragraph (-> (listof pre-flow?)
                                block?)]
 [decode-content (-> (listof pre-content?)
                     content?)]
 [rename decode-content decode-elements
         (-> (listof pre-content?)
             content?)]
 [decode-string (-> string? content?)]
 [clean-up-index-string (-> string? string?)])

(define (spliceof c)
  (define name `(spliceof ,(contract-name c)))
  (define p (flat-contract-predicate c))
  (make-flat-contract #:name name
                      #:first-order (lambda (x)
                                      (and (splice? x)
                                           (andmap p (splice-run x))))))
(provide/contract
 [spliceof (flat-contract? . -> . flat-contract?)])

(define the-part-index-desc (make-part-index-desc))

(define (clean-up-index-string s)
  ;; Collapse whitespace, and remove leading or trailing spaces, which
  ;; might appear there due to images or something else that gets
  ;; dropped in string form.
  (let* ([s (regexp-replace* #px"\\s+" s " ")]
         [s (regexp-replace* #rx"^ " s "")]
         [s (regexp-replace* #rx" $" s "")])
    (datum-intern-literal s)))


(define (decode-string s)
  (define pattern #rx"(---|--|``|''|'|`)")
  (let loop ([start 0])
    (cond
     [(regexp-match-positions pattern s start)
      => (lambda (m)
           (define the-match (substring s (caar m) (cdar m)))
           (list* (datum-intern-literal (substring s start (caar m)))
                  (cond
                   [(string=? the-match "---") 'mdash]
                   [(string=? the-match "--") 'ndash]
                   [(string=? the-match "``") 'ldquo]
                   [(string=? the-match "''") 'rdquo]
                   [(string=? the-match "'") 'rsquo]
                   [(string=? the-match "`") 'lsquo])
                  (loop (cdar m))))]
     ;; Common case: nothing to decode, so don't copy strings.
     ;; Assume that the input is already interned.
     [(= start 0)
      (list s)]
     [else
      (list (datum-intern-literal (substring s start)))])))
   

(define (line-break? v)
  (equal? v "\n"))

(define (whitespace? v)
  (and (string? v) (regexp-match? #px"^[\\s]*$" v)))

(define (decode-accum-para accum)
  (if (andmap whitespace? accum)
      null
      (list (decode-compound-paragraph (reverse (skip-whitespace accum))))))

(define (decode-flow* l keys colls tag-prefix tags vers style title part-depth)
  (let loop ([l l] [next? #f] [keys keys] [colls colls] [accum null]
             [title title] [tag-prefix tag-prefix] [tags tags] [vers vers]
             [style style])
    (cond
      [(null? l)
       (let ([k-tags (map (lambda (k) `(idx ,(make-generated-tag))) keys)]
             [tags (if (null? tags)
                     (list `(part ,(make-generated-tag)))
                     tags)])
         (make-part
          tag-prefix
          (append tags k-tags)
          title
          (if vers
              (make-style (style-name style)
                          (cons (make-document-version vers)
                                (style-properties style)))
              style)
          (let ([l (append
                    (map (lambda (k tag)
                           (make-index-element #f null tag
                                               (part-index-decl-plain-seq k)
                                               (part-index-decl-entry-seq k)
                                               #f))
                         keys k-tags)
                     colls)])
            (if (and title 
                     (not (memq 'hidden (style-properties style))))
              (cons (make-index-element
                     #f null (car tags)
                     (list (clean-up-index-string
                            (regexp-replace #px"^\\s+(?:(?:A|An|The)\\s)?"
                                            (content->string title) "")))
                     (list (make-element #f title))
                     the-part-index-desc)
                    l)
              l))
          (decode-accum-para accum)
          null))]
      [(void? (car l))
       (loop (cdr l) next? keys colls accum title tag-prefix tags vers style)]
      [(title-decl? (car l))
       (cond [(not part-depth) (error 'decode "misplaced title: ~e" (car l))]
             [title (error 'decode "found extra title: ~v" (car l))]
             [else (loop (cdr l) next? keys colls accum
                         (title-decl-content (car l))
                         (title-decl-tag-prefix (car l))
                         (title-decl-tags (car l))
                         (title-decl-version (car l))
                         (title-decl-style (car l)))])]
      #;
      ;; Blocks are now handled by decode-accum-para
      [(block? (car l))
       (let ([para (decode-accum-para accum)]
             [part (decode-flow* (cdr l) keys colls tag-prefix tags vers style
                                 title part-depth)])
         (make-part
          (part-tag-prefix part)
          (part-tags part)
          (part-title-content part)
          (part-style part)
          (part-to-collect part)
          (append para (list (car l)) (part-flow part))
          (part-parts part)))]
      [(part? (car l))
       (let ([para (decode-accum-para accum)]
             [part (decode-flow* (cdr l) keys colls tag-prefix tags vers style
                                 title part-depth)])
         (make-part
          (part-tag-prefix part)
          (part-tags part)
          (part-title-content part)
          (part-style part)
          (part-to-collect part)
          (append para (part-blocks part))
          (cons (car l) (part-parts part))))]
      [(part-start? (car l))
       (unless part-depth
         (error 'decode "misplaced part; title: ~s" (part-start-title (car l))))
       (unless ((part-start-depth (car l)) . <= . part-depth)
         (error 'decode
                "misplaced part (the part is more than one layer deeper than its container); title: ~s"
                (part-start-title (car l))))
       (let ([s (car l)])
         (let loop ([l (cdr l)] [s-accum null])
           (if (or (null? l)
                   (and (part-start? (car l))
                        ((part-start-depth (car l)) . <= . part-depth))
                   (part? (car l)))
             (let ([para (decode-accum-para accum)]
                   [s (decode-styled-part (reverse s-accum)
                                          (part-start-tag-prefix s)
                                          (part-start-tags s)
                                          (part-start-style s)
                                          (part-start-title s)
                                          (add1 part-depth))]
                   [part (decode-flow* l keys colls tag-prefix tags vers style
                                       title part-depth)])
               (make-part (part-tag-prefix part)
                          (part-tags part)
                          (part-title-content part)
                          (part-style part)
                          (part-to-collect part)
                          para
                          (cons s (part-parts part))))
             (cond
              [(splice? (car l))
               (loop (append (splice-run (car l)) (cdr l)) s-accum)]
              [(list? (car l))
               (loop (append (car l) (cdr l)) s-accum)]
              [else
               (loop (cdr l) (cons (car l) s-accum))]))))]
      [(splice? (car l))
       (loop (append (splice-run (car l)) (cdr l))
             next? keys colls accum title tag-prefix tags vers style)]
      [(list? (car l))
       (loop (append (car l) (cdr l))
             next? keys colls accum title tag-prefix tags vers style)]
       [(null? (cdr l))
        (loop null #f keys colls (cons (car l) accum) title tag-prefix tags
              vers style)]
       [(part-index-decl? (car l))
        (loop (cdr l) next? (cons (car l) keys) colls accum title tag-prefix
              tags vers style)]
       [(part-collect-decl? (car l))
        (loop (cdr l) next? keys
              (cons (part-collect-decl-element (car l)) colls)
              accum title tag-prefix tags vers style)]
       [(part-tag-decl? (car l))
        (loop (cdr l) next? keys colls accum title tag-prefix
              (append tags (list (part-tag-decl-tag (car l))))
              vers style)]
       [(and (pair? (cdr l))
	     (or (splice? (cadr l))
                 (list? (cadr l))))
	(loop (cons (car l) (append ((if (splice? (cadr l)) splice-run values) (cadr l)) (cddr l)))
              next? keys colls accum title tag-prefix tags vers style)]
       [(line-break? (car l))
	(if next?
          (loop (cdr l) #t keys colls accum title tag-prefix tags vers style)
          (let ([m (match-newline-whitespace (cdr l))])
            (if m
              (let ([part (loop m #t keys colls null title tag-prefix tags vers
                                style)])
                (make-part
                 (part-tag-prefix part)
                 (part-tags part)
                 (part-title-content part)
                 (part-style part)
                 (part-to-collect part)
                 (append (decode-accum-para accum)
                         (part-blocks part))
                 (part-parts part)))
              (loop (cdr l) #f keys colls (cons (car l) accum) title tag-prefix
                    tags vers style))))]
       [else (loop (cdr l) #f keys colls (cons (car l) accum) title tag-prefix
                   tags vers style)])))

(define (decode-part l tags title depth)
  (decode-flow* l null null #f tags #f plain title depth))

(define (decode-styled-part l tag-prefix tags style title depth)
  (decode-flow* l null null tag-prefix tags #f style title depth))

(define (decode-flow l)
  (part-blocks (decode-flow* l null null #f null #f plain #f #f)))

(define (match-newline-whitespace l)
  (cond [(null? l) #f]
        [(void? (car l)) (match-newline-whitespace (cdr l))]
        [(line-break? (car l)) (skip-whitespace l)]
        [(splice? (car l))
         (match-newline-whitespace (append (splice-run (car l)) (cdr l)))]
        [(list? (car l))
         (match-newline-whitespace (append (car l) (cdr l)))]
        [(whitespace? (car l)) (match-newline-whitespace (cdr l))]
        [else #f]))

(define (skip-whitespace l)
  (if (or (null? l) 
          (not (or (whitespace? (car l))
                   (void? (car l)))))
      l
      (skip-whitespace (cdr l))))

(define (decode l)
  (decode-part l null #f 0))

(define (decode-paragraph l)
  (make-paragraph plain (decode-content l)))

(define (decode-content l)
  (append-map (lambda (s) (cond
                           [(string? s) (decode-string s)]
                           [(void? s) null]
                           [(splice? s) (decode-content (splice-run s))]
                           [(list? s) (decode-content s)]
                           [else (list s)]))
              (skip-whitespace l)))

(define (decode-compound-paragraph l)
  (define (finish-accum para-accum)
    (if (null? para-accum)
        null
        (list (make-paragraph plain (skip-whitespace (apply append (reverse para-accum)))))))
  (let ([r (let loop ([l (skip-whitespace l)]
                      [para-accum null])
             (cond
              [(null? l)
               (finish-accum para-accum)]
              [else
               (let ([s (car l)])
                 (cond
                  [(block? s) (append
                               (finish-accum para-accum)
                               (cons s (loop (skip-whitespace (cdr l)) null)))]
                  [(string? s) (loop (cdr l)
                                     (cons (decode-string s) para-accum))]
                  [else (loop (cdr l)
                              (cons (list (car l)) para-accum))]))]))])
    (cond
     [(null? r)
      (make-paragraph plain null)]
     [(null? (cdr r))
      (car r)]
     [(make-compound-paragraph plain r)])))
