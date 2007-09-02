
(module decode mzscheme
  (require "struct.ss"
           (lib "contract.ss")
           (lib "class.ss"))

  (provide decode
           decode-part
           decode-flow
           decode-paragraph
           decode-content
           decode-string
           whitespace?)

  (provide-structs
   [title-decl ([tag-prefix (or/c false/c string?)]
                [tags (listof tag?)]
                [style any/c]
                [content list?])]
   [part-start ([depth integer?]
                [tag-prefix (or/c false/c string?)]
                [tags (listof tag?)]
                [style any/c]
                [title list?])]
   [splice ([run list?])]
   [part-index-decl ([plain-seq (listof string?)]
                     [entry-seq list?])]
   [part-collect-decl ([element element?])])

  (define (decode-string s)
    (let loop ([l '((#rx"---" mdash)
                    (#rx"--" ndash)
                    (#rx"``" ldquo)
                    (#rx"''" rdquo)
                    (#rx"'" rsquo))])
      (cond
       [(null? l) (list s)]
       [(regexp-match-positions (caar l) s)
	=> (lambda (m)
	     (append (decode-string (substring s 0 (caar m)))
		     (cdar l)
		     (decode-string (substring s (cdar m)))))]
       [else (loop (cdr l))])))

  (define (line-break? v)
    (and (string? v)
         (equal? v "\n")))

  (define (whitespace? v)
    (and (string? v)
         (regexp-match #px"^[\\s]*$" v)))

  (define (decode-accum-para accum)
    (if (andmap whitespace? accum)
        null
        (list (decode-paragraph (reverse (skip-whitespace accum))))))

  (define (decode-flow* l keys colls tag-prefix tags style title part-depth)
    (let loop ([l l][next? #f][keys keys][colls colls][accum null][title title][tag-prefix tag-prefix][tags tags][style style])
      (cond
       [(null? l) 
        (let ([k-tags (map (lambda (k)
                             `(idx ,(make-generated-tag)))
                           keys)]
              [tags (if (null? tags)
                        (list `(part ,(make-generated-tag)))
                        tags)])
          (make-part tag-prefix
                     (append tags k-tags)
                     title 
                     style
                     (let ([l (map (lambda (k tag)
                                     (make-index-element
                                      #f
                                      null
                                      tag
                                      (part-index-decl-plain-seq k)
                                      (part-index-decl-entry-seq k)))
                                   keys k-tags)])
                       (append
                        (if title
                            (cons (make-index-element
                                   #f
                                   null
                                   (car tags)
                                   (list (regexp-replace #px"^(?:A|An|The)\\s" (content->string title)
                                                         ""))
                                   (list (make-element #f title)))
                                  l)
                            l)
                        colls))
                     (make-flow (decode-accum-para accum))
                     null))]
       [(title-decl? (car l))
        (unless part-depth
          (error 'decode
                 "misplaced title: ~e"
                 (car l)))
        (when title
          (error 'decode
                 "found extra title: ~v"
                 (car l)))
        (loop (cdr l) next? keys colls accum 
              (title-decl-content (car l))
              (title-decl-tag-prefix (car l))
              (title-decl-tags (car l))
              (title-decl-style (car l)))]
       [(flow-element? (car l))
        (let ([para (decode-accum-para accum)]
              [part (decode-flow* (cdr l) keys colls tag-prefix tags style title part-depth)])
          (make-part (part-tag-prefix part)
                     (part-tags part)
                     (part-title-content part)
                     (part-style part)
                     (part-to-collect part)
                     (make-flow (append para
                                        (list (car l)) 
                                        (flow-paragraphs (part-flow part))))
                     (part-parts part)))]
       [(part? (car l))
        (let ([para (decode-accum-para accum)]
              [part (decode-flow* (cdr l) keys colls tag-prefix tags style title part-depth)])
          (make-part (part-tag-prefix part)
                     (part-tags part)
                     (part-title-content part)
                     (part-style part)
                     (part-to-collect part)
                     (make-flow (append para
                                        (flow-paragraphs
                                         (part-flow part))))
                     (cons (car l) (part-parts part))))]
       [(and (part-start? (car l))
             (or (not part-depth)
                 ((part-start-depth (car l)) . <= . part-depth)))
        (unless part-depth
          (error 'decode
                 "misplaced part: ~e"
                 (car l)))
        (let ([s (car l)])
          (let loop ([l (cdr l)]
                     [s-accum null])
            (if (or (null? l)
                    (or (and (part-start? (car l))
                             ((part-start-depth (car l)) . <= . part-depth))
                        (part? (car l))))
                (let ([para (decode-accum-para accum)]
                      [s (decode-styled-part (reverse s-accum)
                                             (part-start-tag-prefix s)
                                             (part-start-tags s)
                                             (part-start-style s)
                                             (part-start-title s)
                                             (add1 part-depth))]
                      [part (decode-flow* l keys colls tag-prefix tags style title part-depth)])
                  (make-part (part-tag-prefix part)
                             (part-tags part)
                             (part-title-content part)
                             (part-style part)
                             (part-to-collect part)
                             (make-flow para)
                             (cons s (part-parts part))))
                (if (splice? (car l))
                    (loop (append (splice-run (car l)) (cdr l)) s-accum)
                    (loop (cdr l) (cons (car l) s-accum))))))]
       [(splice? (car l))
	(loop (append (splice-run (car l)) (cdr l)) next? keys colls accum title tag-prefix tags style)]
       [(null? (cdr l)) (loop null #f keys colls (cons (car l) accum) title tag-prefix tags style)]
       [(part-index-decl? (car l))
        (loop (cdr l) next? (cons (car l) keys) colls accum title tag-prefix tags style)]
       [(part-collect-decl? (car l))
        (loop (cdr l) next? keys (cons (part-collect-decl-element (car l)) colls) accum title tag-prefix tags style)]
       [(and (pair? (cdr l))
	     (splice? (cadr l)))
	(loop (cons (car l) (append (splice-run (cadr l)) (cddr l))) next? keys colls accum title tag-prefix tags style)]
       [(line-break? (car l))
	(if next?
	    (loop (cdr l) #t keys colls accum title tag-prefix tags style)
	    (let ([m (match-newline-whitespace (cdr l))])
              (if m
                  (let ([part (loop m #t keys colls null title tag-prefix tags style)])
                    (make-part (part-tag-prefix part)
                               (part-tags part)
                               (part-title-content part)
                               (part-style part)
                               (part-to-collect part)
                               (make-flow (append (decode-accum-para accum)
                                                  (flow-paragraphs (part-flow part))))
                               (part-parts part)))
                  (loop (cdr l) #f keys colls (cons (car l) accum) title tag-prefix tags style))))]
       [else (loop (cdr l) #f keys colls (cons (car l) accum) title tag-prefix tags style)])))

  (define (decode-part l tags title depth)
    (decode-flow* l null null #f tags #f title depth))

  (define (decode-styled-part l tag-prefix tags style title depth)
    (decode-flow* l null null tag-prefix tags style title depth))

  (define (decode-flow l)
    (part-flow (decode-flow* l null null #f null #f #f #f)))

  (define (match-newline-whitespace l)
    (cond
     [(null? l) #f]
     [(line-break? (car l))
      (skip-whitespace l)]
     [(splice? (car l))
      (match-newline-whitespace (append (splice-run (car l))
                                        (cdr l)))]
     [(whitespace? (car l))
      (match-newline-whitespace (cdr l))]
     [else #f]))

  (define (skip-whitespace l)
    (let loop ([l l])
      (if (or (null? l)
              (not (whitespace? (car l))))
          l
          (loop (cdr l)))))

  (define (decode l)
    (decode-part l null #f 0))

  (define (decode-paragraph l)
    (make-paragraph 
     (decode-content l)))

  (define (decode-content l)
    (apply append
           (map (lambda (s)
                  (cond
                   [(string? s)
                    (decode-string s)]
                   [else (list s)]))
                (skip-whitespace l)))))
