#lang racket/base
(require rackunit
         redex/reduction-semantics)

(define-language Base
  (x a b c)
  (y d e f))

;; Simple repeats
(define-extended-language S1 Base
  (s (x ... y ...)))

#;
(append/e (many/e (enum x))
          (many/e (enum y)))

(define-extended-language S2 Base
  (s (x ..._1 y ..._1)))

#;
(dep/e nats
       (λ (len)
          (append/e (listof n (enum x))
                    (listof n (enum y)))))

(define-extended-language S3 Base
  (s (x_1 ..._1 x_1 ..._1)))

#;
(dep/e nats
       (λ (len)
          (dep/e (listof n (enum x))
                 (λ (x-vals)
                    (append/e x-vals
                              x-vals)))))

;; Nesting!
(define-extended-language S4 Base
  (s ((x ...) ... (y ...) ...)))

#;
(append/e (many/e (append/e (many/e (enum x))))
          (many/e (append/e (many/e (enum y)))))

(define-extended-language S5 Base
  (s ((x ...) ..._1 (y ...) ..._1)))

#;
(dep/e nats
       (λ (len)
          (append/e (listof n (append/e (many (enum x))))
                    (listof n (append/e (many (enum y)))))))

(define-extended-language S6 Base
  (s ((x ..._1) ..._2 (y ..._1) ..._2)))


;; mapM/e : (a -> enum b) -> [a] -> ([a] -> enum [b])
;; (compose mapM/e mapM/e) : (a -> enum b) -> [[a]] -> ([[a]] -> enum [[b]])
;; 
#;
(dep/e nats
       (λ (len2)
          (dep/e (listof nats)
                 (λ (len1s)
                    (append/e (listof len2
                                      (mapM/e (λ (len1)
                                                 (listof len1 (enum x)))
                                              len1s))
                              (listof len2
                                      (mapM/e (λ (len1)
                                                 (listof len1 (enum y)))
                                              len1s)))))) )

;; 
(define-extended-language S7 Base
  (s ((x_1 ..._1) ..._2 (x_1 ..._1) ..._2)))


#;
(dep/e nats
       (λ (len2)
          (dep/e (listof nats)
                 (λ (len1s)
                    (dep/e
                     ;; We need to enumerate them in the context of the outermost "rigid"/named pattern
                     (listof len2
                             (mapM/e (λ (len1)
                                        (listof len1 (enum x))))) ;; this is an important part!!!!
                     ;; [[x]]
                     ;; we're lucky that they're the same this time, what if it's more complicated though?
                     (λ (xss)
                        (append/e xss
                                  xss)))))))

;; This one has different underlying patterns
(define-extended-language S8 Base
  (s (((x_1 y) ..._1) ..._2
      (x_1     ..._1) ..._2)))

#;
(dep/e nats
       (λ (len2)
          (dep/e (listof nats)
                 (λ (len1s)
                    (dep/e
                     ;; We need to enumerate them in the context of the outermost "rigid"/named pattern
                     (listof len2
                             (mapM/e (λ (len1)
                                        (listof len1 (enum x))))) ;; this is an important part!!!!
                     ;; [[x]]
                     (λ (xss)
                        (append/e ((compose  mapM/e  mapM/e)
                                   (λ (x)
                                      (map/e (λ (y)
                                                (cons x y))
                                             (enum y)))
                                   (xss))
                                  ((compose mapM/e mapM/e)
                                   (λ (x)
                                      (const/enum x))
                                   xss))))))))


;; This one has different patterns that differ at a higher branch
(define-extended-language S9 Base
  (s ((x_1 ..._1 y) ..._2
      (x_1 ..._1)   ..._2)))

#;
(dep/e nats
       (λ (len2)
          (dep/e  (listof len2 nats)
                  (λ (len1s)
                     (dep/e
                      (listof len2
                              (mapM/e (λ (len1)
                                         (listof len1 (enum x)))
                                      len1s))
                      (λ (x1ss)
                         (append/e
                          (mapM/e
                           (λ (x1s)
                              (append/e (mapM/e const x1s)
                                        (singleton/e 1 (enum y)))))
                          ((compose mapM/e mapM/e)
                           const/e
                           x1ss))))))))


;; Next lets make the insides even more compilcated
(define-extended-language S10 Base
  (s (((y_2
        (x_1 y)              ..._1
        y ...)     ..._2
       y ...
       (y
        (x_1 (x_1 x_1))      ..._1
        y_2)       ..._2)
       y ...
       y)))

#;
;; This one's a doozy so hopefully it will be similar to the general construction
(dep/e (vals/e ((iterate 0) nats))
       (λ (len2)
          ;; as long as multiple parts of the list need access to the
          ;; same vars you need to keep everyone within the dep/e
          
          ;; as you descend deeper you need to do that many listofs
          ;; when you enumerate so in the algorithm you'll need to
          ;; keep track of that.
          
          ;; you also must enumerate all variables that are at that
          ;; depth, in this way named vals and named repeats are similar
          (dep/e (vals/e ((iterate 1 (curry many/e
                                            len2)) nats)
                         ((iterate 1 (curry many/e
                                            len2)) (enum y)))
                 (λ (len1s ys)
                    (dep/e
                     (vals/e (((iterate 0 mapM/e)
                               (λ (len2)
                                  ((iterate 1 mapM/e)
                                   (λ (len1)
                                      (listof len1 (enum x))))
                                  len1s))
                              len2))

                     ;; 2 levels deep
                     (λ (xss)
                        (append/e
                         ;;
                         ((iterate 0 mapM/e)
                          (λ (len2)
                             ((iterate 1 mapM/e)
                              (λ (len1 y xs)
                                 (append/e
                                  (singleton/e (const/e y))
                                  (mapM/e (λ (x)
                                             (list/e (const/e x)
                                                     (enum y)))
                                          xs)
                                  (many y)))
                              ys
                              xss))
                          len2)
                         (many (enum y))
                         ;; The complications of the above iterate
                         ;; mapM/e's might not be necessary so I'm not using them here
                         (mapM/e
                          (λ (y xs)
                             (append/e
                              (singleton/e (enum y))
                              (mapM/e
                               (λ (x)
                                  (list/e (const/e x)
                                          (list/e x x)))
                               xs)
                              (mapM/e
                               const/e
                               ys)))
                          ys
                          xss)
                         (many (enum y))
                         (singleton/e (enum y)))))))))


;; Then have them be 3 wide instead of 2
#;
(define-extended-language S11 Base
  (s ((x_1 ..._1 y) ..._2
      (x_1 ..._1)   ..._2)))

;; We should also do 3 deep  

  #;

  
;; Nesting something within itself, yikes!
(define-extended-language S12 Base
  (s ((x_1 ..._1 y) ..._2
      (x_1 ..._1)   ..._2)))
