(require (lib "mztake.ss" "mztake" )
         (lib "match.ss")
         (lib "base-gm.ss" "frtime")
         (only mzscheme hash-table-map))

(set-main! "picture.ss")

(define (hash-table-increment! h k)
  (let ([old (hash-get h k (lambda () 0))])
    (hash-put! h k (add1 old))))

(define pings (make-hash 'equal))

(for-each-e! (where)
             (match-lambda [(line function context rest ...) 
                            (hash-table-increment! pings (list function context))]
                           [_ (void)]))

(define clicks (changes (quotient milliseconds 50)))

(set-running-e! (merge-e (clicks . -=> . false)
                         (clicks . -=> . true)))

(define (hash-pairs ht)
  (hash-table-map ht (lambda (k v) (list k v))))

(define (show-profile)
  (sort (hash-pairs pings) (lambda (a b) (> (second a) (second b)))))
