(library (nanopass prefix-matcher)
  (export empty-prefix-tree insert-prefix match-prefix)
  (import (chezscheme))

  (define-record-type prefix-node
    (nongenerative)
    (sealed #t)
    (fields str start end result next*))

  (define substring=?
    (lambda (str0 str1 s e)
      (let loop ([i s])
        (or (fx= i e)
            (and (char=? (string-ref str0 i) (string-ref str1 i))
                 (loop (fx+ i 1)))))))

  (define empty-prefix-tree (lambda () '()))

  (define match-prefix
    (case-lambda
      [(pt str) (match-prefix pt str (lambda (str s e) #t))]
      [(pt str ok-suffix?)
       (let ([len (string-length str)])
         (let loop ([pt pt] [curr-result #f] [curr-end 0])
           (if (null? pt)
               (and curr-result (ok-suffix? (substring str curr-end len)) curr-result)
               (let ([node (car pt)] [pt (cdr pt)])
                 (let ([end (prefix-node-end node)])
                   (if (fx> end len)
                       (loop pt curr-result curr-end)
                       (let ([node-str (prefix-node-str node)])
                         (if (substring=? node-str str (prefix-node-start node) end)
                             (cond
                               [(fx= end len)
                                (or (prefix-node-result node)
                                    (and curr-result (ok-suffix? (substring str curr-end len)) curr-result))]
                               [(prefix-node-result node)
                                (loop (prefix-node-next* node) (prefix-node-result node) end)]
                               [else (loop (prefix-node-next* node) curr-result curr-end)])
                             (loop pt curr-result curr-end)))))))))]))

  ;; NB: the following assumes that no one will be mutating the strings put into this tree
  (define insert-prefix
    (lambda (pt str result)
      (let ([len (string-length str)])
        (let f ([pt pt] [start 0])
          (if (null? pt)
              (list (make-prefix-node str start len result '()))
              (let* ([node (car pt)] [pt (cdr pt)] [node-str (prefix-node-str node)])
                (when (string=? node-str str) (errorf 'add-prefix "prefix already in tree"))
                (let loop ([offset start])
                  (if (fx= offset len)
                      (cons
                        (make-prefix-node node-str start offset #f
                          (cons (make-prefix-node str offset len result '())
                                (make-prefix-node node-str offset (prefix-node-end node)
                                  (prefix-node-result node) (prefix-node-next* node))))
                        pt)
                      (let ([end (prefix-node-end node)])
                        (cond
                          [(fx= offset end)
                           (cons (make-prefix-node node-str start (prefix-node-end node)
                                   (prefix-node-result node)
                                   (f (prefix-node-next* node) offset))
                                 pt)]
                          [(char=? (string-ref str offset) (string-ref node-str offset)) (loop (fx+ offset 1))]
                          [(fx= offset start) (cons node (f pt start))]
                          [else (cons (make-prefix-node node-str start offset #f
                                        (list (make-prefix-node node-str offset end
                                                (prefix-node-result node) (prefix-node-next* node))
                                          (make-prefix-node str offset len result '())))
                                      pt)]))))))))))
  (define remove-prefix
    (lambda (pt str)
      #|
      (let ([len (string-length str)])
        (let f ([pt pt])
          (if (null? pt)
              pt
              (let ([node (car pt)] [pt (cdr pt)])
                (let ([end (prefix-node-end node)])
                  (if (fx> end len)
                      pt
                      (let ([node-str (prefix-node-str node)])
                        (if (substring=? node-str str (prefix-node-str node) end)
                            (if (fx= end len)
                                (let ([next* (prefix-node-next* node)])
                                  (cond
                                    [(null? next*) pt]
                                    [(fx= (length next*) 1)
                                     (let ([next (car next*)])
                                       (make-prefix-node (prefix-node-str next) (prefix-node-start node)
                                         (prefix-node-end next) (prefix-node-result next) (prefix-node-next* next)))]
                                    [else (make-prefix-node (prefix-node-str (car next*))
                                            (prefix-node-start node) (prefix-node
      |#
      (errorf 'remove-prefix "not yet implemented")))
  )
