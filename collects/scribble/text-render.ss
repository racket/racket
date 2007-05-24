
(module text-render mzscheme
  (require "struct.ss"
           (lib "class.ss"))
  (provide render-mixin)

  (define (render-mixin %)
    (class %
      (define/override (get-substitutions)
        '((#rx"---" "\U2014")
	  (#rx"--" "\U2013")
	  (#rx"``" "\U201C")
	  (#rx"''" "\U201D")
	  (#rx"'" "\U2019")))

      (inherit render-content
               render-paragraph
               render-flow-element)

      (define/override (render-part d ht)
        (let ([number (collected-info-number (part-collected-info d))])
          (when (or (ormap values number)
                    (part-title-content d))
            (newline))
          (for-each (lambda (n)
                      (when n
                        (printf "~s." n)))
                    (reverse number))
          (when (part-title-content d)
            (when (ormap values number)
              (printf " "))
            (render-content (part-title-content d) d ht))
          (when (or (ormap values number)
                    (part-title-content d))
            (newline))
          (newline)
          (render-flow (part-flow d) d ht)
          (let loop ([pos 1]
                     [secs (part-parts d)])
            (unless (null? secs)
              (newline)
              (render-part (car secs) ht)
              (loop (add1 pos) (cdr secs))))))

      (define/override (render-flow f part ht)
        (let ([f (flow-paragraphs f)])
          (if (null? f)
              null
              (apply
               append
               (render-flow-element (car f) part ht)
               (map (lambda (p)
                      (newline) (newline)
                      (render-flow-element p part ht))
                    (cdr f))))))

      (define/override (render-table i part ht)
        (let ([flowss (table-flowss i)])
          (if (null? flowss)
              null
              (apply
               append
               (map (lambda (d) (render-flow d part ht)) (car flowss))
               (map (lambda (flows)
                      (newline)
                      (map (lambda (d) (render-flow d part ht)) flows))
                    (cdr flowss))))))

      (define/override (render-itemization i part ht)
        (let ([flows (itemization-flows i)])
          (if (null? flows)
              null
              (apply append
                     (begin
                       (printf "* ")
                       (render-flow (car flows) part ht))
                     (map (lambda (d)
                            (printf "\n\n* ")
                            (render-flow d part ht))
                          (cdr flows))))))
      
      (define/override (render-other i part ht)
        (cond
         [(symbol? i)
          (display (case i
                     [(mdash) "\U2014"]
                     [(ndash) "\U2013"]
                     [(ldquo) "\U201C"]
                     [(rdquo) "\U201D"]
                     [(rsquo) "\U2019"]
                     [(rarr) "->"]
                     [else (error 'text-render "unknown element symbol: ~e" i)]))]
         [(string? i) (display i)]
         [else (write i)])
        null)

      (super-new))))
