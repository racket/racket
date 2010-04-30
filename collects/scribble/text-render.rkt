
(module text-render mzscheme
  (require "core.ss"
           mzlib/class)
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
               render-block)

      (define/override (render-part d ht)
        (let ([number (collected-info-number (part-collected-info d ht))])
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
          (render-flow (part-blocks d) d ht #f)
          (let loop ([pos 1]
                     [secs (part-parts d)])
            (unless (null? secs)
              (newline)
              (render-part (car secs) ht)
              (loop (add1 pos) (cdr secs))))))

      (define/override (render-flow f part ht starting-item?)
        (if (null? f)
            null
            (apply
             append
             (render-block (car f) part ht starting-item?)
             (map (lambda (p)
                    (newline) (newline)
                    (render-block p part ht #f))
                  (cdr f)))))


      (define/override (render-intrapara-block p part ri first? last? starting-item?)
        (unless first? (newline) (newline))
        (super render-intrapara-block p part ri first? last? starting-item?))
      
      (define/override (render-table i part ht inline?)
        (let ([flowss (table-blockss i)])
          (if (null? flowss)
              null
              (apply
               append
               (map (lambda (d) (unless (eq? d 'cont) (render-block d part ht #f))) (car flowss))
               (map (lambda (flows)
                      (newline)
                      (map (lambda (d) (unless (eq? d 'cont) (render-block d part ht #f))) flows))
                    (cdr flowss))))))

      (define/override (render-itemization i part ht)
        (let ([flows (itemization-blockss i)])
          (if (null? flows)
              null
              (apply append
                     (begin
                       (printf "* ")
                       (render-flow (car flows) part ht #t))
                     (map (lambda (d)
                            (printf "\n\n* ")
                            (render-flow d part ht #f))
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
                     [(lang) ">"]
                     [(rang) "<"]
                     [(rarr) "->"]
                     [(nbsp) " "]
                     [(prime) "'"]
                     [(alpha) "\u03B1"]
                     [(infin) "\u221E"]
                     [else (error 'text-render "unknown element symbol: ~e" i)]))]
         [(string? i) (display i)]
         [else (write i)])
        null)

      (super-new))))
