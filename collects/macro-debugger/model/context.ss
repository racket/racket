
(module context mzscheme
  (require (lib "stx.ss" "syntax"))
  (provide (struct ref (n))
           (struct tail (n))
           path-get
           pathseg-get
           path-replace
           pathseg-replace
           find-subterm-paths)
  
  ;; A Path is a (list-of PathSeg)
  ;; where the PathSegs are listed outermost to innermost
  ;; for example: (path-get #'((a b) (c d)) (list (make-ref 0) (make-ref 1))) = #'b, not #'c
  
  ;; A PathSeg is one of:
  ;;   - (make-ref number)
  ;;   - (make-tail number)
  
  (define-struct pathseg () #f)
  (define-struct (ref pathseg) (n) #f)
  (define-struct (tail pathseg) (n) #f)

  ;; path:ref->splicing-tail : PathSeg -> ???
  ;; ????
  (define (path:ref->splicing-tail path)
    (unless (ref? path)
      (raise-type-error 'path:ref->splicing-tail "ref path" path))
    (make-tail (sub1 (ref-n path))))

  ;; path-get : syntax Path -> syntax
  (define (path-get stx path)
    (let loop ([stx stx] [path path])
      (cond [(null? path) stx]
            [(pair? path)
             (loop (pathseg-get stx (car path)) (cdr path))]
            [else
             (error 'path-get "bad path: ~s" path)])))

  ;; pathseg-get : syntax PathSeg -> syntax
  (define (pathseg-get stx path)
    (cond [(ref? path) (pathseg-get/ref stx (ref-n path))]
	  [(tail? path) (pathseg-get/tail stx (tail-n path))]))
  
  ;; pathseg-get/ref : syntax number -> syntax
  (define (pathseg-get/ref stx0 n0)
    (let loop ([n n0] [stx stx0])
      (unless (stx-pair? stx)
        (error 'pathseg-get "ref path out of bounds for syntax: ~s, ~s" 
               n0
               (syntax-object->datum stx0)))
      (if (zero? n)
	  (stx-car stx)
	  (loop (sub1 n) (stx-cdr stx)))))

  ;; pathseg-get/tail : syntax number -> syntax
  (define (pathseg-get/tail stx0 n0)
    (let loop ([n n0] [stx stx0])
      (unless (stx-pair? stx)
        (error 'pathseg-get "tail path out of bounds for syntax: ~s, ~s" n0 stx0))
      (if (zero? n)
	  (stx-cdr stx)
	  (loop (sub1 n) (stx-cdr stx)))))

  ;; path-replace : syntax Path syntax -> syntax
  (define (path-replace stx path x)
    (cond [(null? path) x]
          [(pair? path)
           (let ([pathseg0 (car path)])
             (pathseg-replace stx
                              pathseg0
                              (path-replace (pathseg-get stx pathseg0)
                                            (cdr path)
                                            x)))]
          [else
           (error 'path-replace "bad path: ~s" path)]))

  ;; pathseg-replace : syntax PathSeg syntax -> syntax
  (define (pathseg-replace stx pathseg x)
    (cond [(ref? pathseg) (pathseg-replace/ref stx (ref-n pathseg) x)]
	  [(tail? pathseg) (pathseg-replace/tail stx (tail-n pathseg) x)]
          [else (error 'pathseg-replace "bad path: ~s" pathseg)]))

  ;; pathseg-replace/ref : syntax number syntax -> syntax
  (define (pathseg-replace/ref stx0 n0 x)
    (let loop ([n n0] [stx stx0])
      (unless (stx-pair? stx)
        (error 'pathseg-replace "ref path out of bounds for syntax: ~s, ~s" n0 stx0))
      (if (zero? n)
	  (stx-replcar stx x)
	  (stx-replcdr stx (loop (sub1 n) (stx-cdr stx))))))

  ;; pathseg-replace/tail : syntax number syntax -> syntax
  (define (pathseg-replace/tail stx0 n0 x)
    (let loop ([n n0] [stx stx0])
      (unless (stx-pair? stx)
        (error 'pathseg-replace "tail path out of bounds for syntax: ~s, ~s" n0 stx0))
      (if (zero? n)
	  (stx-replcdr stx x)
	  (stx-replcdr stx (loop (sub1 n) (stx-cdr stx))))))

  ;; stx-replcar : syntax syntax -> syntax
  (define (stx-replcar stx x)
    (cond [(pair? stx)
	   (cons x (cdr stx))]
	  [(syntax? stx)
	   (datum->syntax-object stx (cons x (stx-cdr stx)))]
	  [else (raise-type-error 'stx-replcar "stx-pair" stx)]))

  ;; stx-replcdr : syntax syntax -> syntax
  (define (stx-replcdr stx x)
    (cond [(pair? stx)
	   (cons (car stx) x)]
	  [(and (syntax? stx) (pair? (syntax-e stx)))
	   (datum->syntax-object stx (cons (stx-car stx) x))]
	  [else (raise-type-error 'stx-replcdr "stx-pair" stx)]))
  
  (define (sd x)
    (syntax-object->datum (datum->syntax-object #f x)))
  
  ;;=======
  
  ;; find-subterm-paths : syntax syntax -> (list-of Path)
  (define (find-subterm-paths subterm term)
    (let outer-loop ([term term])
      (cond [(eq? subterm term)
             (list null)]
            [(stx-pair? term)
             ;; Optimized for lists...
             (let loop ([term term] [n 0])
               (if (stx-pair? term)
                   (let* ([seg0 (make-ref n)])
                     (append (map (lambda (p) (cons seg0 p)) (outer-loop (stx-car term)))
                             (if (eq? subterm (stx-cdr term))
                                 (list (list (make-tail n)))
                                 (loop (stx-cdr term) (add1 n)))))
                   (let ([seg0 (make-tail n)])
                     (map (lambda (p) (cons seg0 p))
                          (outer-loop term)))))]
            ;; FIXME: more structured cases here: box, vector, ...
            [else null])))
  )
