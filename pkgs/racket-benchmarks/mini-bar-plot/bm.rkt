#lang racket/base
(require racket/class
         racket/cmdline
         racket/path
         racket/draw
         pict
         "main.rkt")

(module+ main
  (define current-dest-file (make-parameter #f))
  (define current-sort (make-parameter #f))
  (define current-mean-wrt (make-parameter #f))
  (define current-rev-inputs (make-parameter null))
  (define current-rev-implementations (make-parameter null))

  (define inputs
    (command-line
     #:once-each
     [("-o" "--dest") file "Write plot to <file> inferring format from extension"
                      (current-dest-file file)]
     [("--sort-wrt") impl vs-impl "Sort by ratio of <impl> to <vs-impl>"
                     (current-sort (cons (string->symbol impl) (string->symbol vs-impl)))]
     [("--mean-wrt") impl "Show goemetric mean of ration relative to <impl>"
                     (current-mean-wrt (string->symbol impl))]
     #:multi
     [("++file") file impl "Load <file> using <impl> instead file's recorded <impl>"
                 (let ([impl (string->symbol impl)])
                   (current-rev-inputs (cons (input file (lambda (n) impl))
                                             (current-rev-inputs))))]
     [("++bar") impl name color "Set order and colors of bars"
                (current-rev-implementations (cons (implementation (string->symbol impl) name color)
                                                   (current-rev-implementations)))]
     #:args file
     (append
      (reverse (current-rev-inputs))
      (for/list ([file (in-list file)])
        (input file (lambda (n) n))))))

  (define p (benchmark-plot inputs
                            #:implementations (reverse (current-rev-implementations))
                            #:sort-by-ratio (current-sort)
                            #:geometic-mean-wrt (current-mean-wrt)))

  (let ([file (current-dest-file)])
    (cond
      [(not file)
       (parameterize ([current-command-line-arguments (vector)])
         ((dynamic-require 'slideshow 'slide) (scale p 0.5)))]
      [else
       (define ext (path-get-extension file))
       (define bm-format
         (case ext
           [(#".png") 'png]
           [(#".jpg" #".jpeg") 'jpeg]
           [(#".bmp") 'bmp]
           [else #f]))
       (cond
         [bm-format
          (send (pict->bitmap p) save-file file bm-format)
          (void)]
         [(equal? ext #".pdf")
          (define pss (new ps-setup%))
          (send pss set-scaling 1.0 1.0)
          (parameterize ([current-ps-setup pss])
            (define dc (new pdf-dc%
                            [interactive #f]
                            [as-eps #t]
                            [width (pict-width p)]
                            [height (pict-height p)]
                            [output file]))
            (send dc start-doc "plot")
            (send dc start-page)
            (draw-pict p dc 0 0)
            (send dc end-page)
            (send dc end-doc))]
         [else
          (raise-user-error 'bm "could not determine format for output: ~a" file)])])))
       
(struct input (file impl->impl))
(struct implementation (key name color))

(define (benchmark-plot inputs
                        #:implementations [implementations null]
                        #:sort-by-ratio [sort-by-ratio/names #f]
                        #:geometic-mean-wrt [geo-mean-impl #f])
  (define all-timess
    (for/fold ([accum #hasheq()]) ([i (in-list inputs)])
      (get-times #:accum accum
                 (input-file i)
                 #:impl->impl (input-impl->impl i))))
  
  (define med-timess (median-times all-timess))

  (define given-keys
    (for/list ([i (in-list implementations)])
      (implementation-key i)))
  
  (define keys (append given-keys
                       (for/list ([k (in-hash-keys med-timess)]
                                  #:unless (member k given-keys))
                         k)))

  (define names
    (for/list ([k (in-list keys)])
      (or (for/or ([i (in-list implementations)])
            (and (eq? k (implementation-key i))
                 (implementation-name i)))
          k)))
  
  (define timess
    (for/list ([k (in-list keys)])
      (hash-ref med-timess k)))

  (define colors
    (let ([cs (for/list ([i (in-list implementations)])
                (implementation-color i))])
      (append cs
              (generate-colors (list-tail keys (length cs))
                               #:used cs))))

  (define (find-key k)
    (or (for/or ([a-k (in-list keys)]
                 [i (in-naturals)])
          (and (eq? k a-k) i))
        (raise-user-error 'bm "sort key not found: ~a" k)))

  (define sort-by-ratio
    (and sort-by-ratio/names
         (cons (find-key (car sort-by-ratio/names))
               (find-key (cdr sort-by-ratio/names)))))

  (define ((ratio-geo-mean wrt-times) times)
    (define ratios
      (for/list ([(k v) (in-hash times)])
        (/ v (hash-ref wrt-times k))))
    (hash '|geometric mean|
          (expt (apply * ratios) (/ (length ratios)))))

  (define (plot timess
                #:key->pict [key->pict (lambda (k) #f)])
    (define base (mini-bar-plot names
                                timess
                                #:sort-ratio sort-by-ratio
                                #:columns 6
                                #:colors colors
                                #:key->pict key->pict
                                #:suffix " msec"))
    (cond
      [geo-mean-impl
       (ht-append (* 2 (current-h-plot-sep))
                  (mini-bar-plot names
                                 (map (ratio-geo-mean (hash-ref med-timess geo-mean-impl))
                                      timess)
                                 #:prefix "x"
                                 #:decimal-places 2
                                 #:colors colors
                                 #:key->pict (lambda (s) ((current-t) (format "~a" s))))
                  base)]
      [else base]))

  (define ((filter-times ?) ht)
    (for/hasheq ([(k v) (in-hash ht)]
                 #:when (? k))
      (values k v)))

  (cond
    [(ormap r5rs? (hash-keys (car timess)))
     (vc-append (current-v-plot-sep)
                (plot (map (filter-times (lambda (x) (not (r5rs? x)))) timess))
                (plot (map (filter-times r5rs?) timess)
                      #:key->pict (lambda (k)
                                    (colorize ((current-tt) (symbol->string k)) r5rs-color))))]
    [else (plot timess)]))

(define (get-times f
                   #:accum [hts #hasheq()]
                   #:impl->impl [impl->impl (lambda (impl) impl)])
  (call-with-input-file*
   f
   (lambda (i)
     (for/fold ([hts hts]) ([i (in-port read i)])
       (define impl (impl->impl (car i)))
       (define ht (hash-ref hts impl #hasheq()))
       (define k (rename (cadr i)))
       (define t (caaddr i))
       (define e (hash-ref ht k null))
       (hash-set hts impl (if t
                              (hash-set ht k (cons t e))
                              ht))))))

(define (median-times hts)
  (define (median l)
    (list-ref (sort l <) (quotient (length l) 2)))
  (for/hasheq ([(impl ht) (in-hash hts)])
    (values impl
            (for/hasheq ([(k e) (in-hash ht)]
                         #:unless (or (eq? k 'nothing)
                                      (eq? k 'hello)))
              (values k (median e))))))

(define (rename k)
  (hash-ref #hasheq((mandelbrot-generic . mandelbrot-g)
                    (reversecomplement . reversecomp)
                    (spectralnorm-generic . spectralnorm-g)
                    (nbody-vec-generic . nbody-vec-g)
                    (cheapconcurrency . cheapconcur))
            k
            k))

(define r5rs-color "forestgreen")

(define r5rs-keys
  '(conform destruct dynamic lattice maze peval psyntax scheme-c scheme-i scheme sort1))
(define (r5rs? key) (memq key r5rs-keys))

