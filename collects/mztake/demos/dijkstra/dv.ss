; -*- Scheme -*-

; Shriram Krishnamurthi (shriram@cs.rice.edu)
; Tue Jul 25 23:20:45 EDT 1995

; (define-structure (dv:vector length size contents))

(module dv mzscheme 

  (provide dv:make dv:make-w/-init dv:length dv:contents dv:append
           dv:remove-last dv:legitimate-index dv:ref dv:set!)
  
  (define dv:vector?
    (lambda (obj)
      (if (vector? obj)
          (if (= (vector-length obj) 4)
              (eq? (vector-ref obj 0) 'dv:vector)
              #f)
          #f)))
  (define dv:vector-length
    (lambda (obj) (vector-ref obj 1)))
  (define dv:vector-size
    (lambda (obj) (vector-ref obj 2)))
  (define dv:vector-contents
    (lambda (obj) (vector-ref obj 3)))
  (define dv:set-vector-length!
    (lambda (obj newval) (vector-set! obj 1 newval)))
  (define dv:set-vector-size!
    (lambda (obj newval) (vector-set! obj 2 newval)))
  (define dv:set-vector-contents!
    (lambda (obj newval) (vector-set! obj 3 newval)))
  (define dv:make-vector
    (lambda (length size contents)
      ((lambda () (vector 'dv:vector length size contents)))))
  
  (define dv:make
    (let* ((default-initial-size 8)
           (default-initial-vector (make-vector default-initial-size)))
      (lambda arg
        (cond
          ((null? arg)
           (dv:make-vector 0 default-initial-size default-initial-vector))
          ((= 1 (length arg))
           (let ((l (car arg)))
             (dv:make-vector 0 l (make-vector l))))
          (else
           (error 'dv:make "wrong number of arguments"))))))
  
  (define dv:make-w/-init
    (lambda values
      (let ((l (length values)))
        (dv:make-vector l l (list->vector values)))))
  
  (define dv:append
    (lambda (dv item)
      (let ((length   (dv:vector-length dv))
            (size     (dv:vector-size dv))
            (contents (dv:vector-contents dv)))
        (if (< length size)
            (begin
              (vector-set! contents length item)
              (dv:set-vector-length! dv (+ length 1)))
            (begin
              (let ((new-vector (make-vector (* size 2))))
                (let loop
		  ((i 0))
                  (when (< i size)
                    (vector-set! new-vector i (vector-ref contents i))
                    (loop (+ i 1))))
                (dv:set-vector-contents! dv new-vector)
                (dv:set-vector-size! dv (* size 2))
                (dv:append dv item)))))))
  
  (define dv:remove-last
    (lambda (dv) 
      (dv:set-vector-length! dv (- (dv:vector-length dv) 1))
      (vector-set! (dv:vector-contents dv) (dv:vector-length dv) 0)))
  
  
  (define dv:legitimate-index
    (lambda (dv index)
      (< index (dv:vector-length dv))))
  
  (define dv:ref
    (lambda (dv index)
      (if (dv:legitimate-index dv index)
          (vector-ref (dv:vector-contents dv) index)
          (error 'dv:ref "index too large"))))
  
  (define dv:set!
    (lambda (dv index value)
      (if (dv:legitimate-index dv index)
          (vector-set! (dv:vector-contents dv) index value)
          (error 'dv:set! "index too large"))))
  
  (define dv:contents dv:vector-contents)
  
  (define dv:length dv:vector-length)
  )
  
