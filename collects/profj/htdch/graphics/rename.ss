(module rename mzscheme

  (require mzlib/class
           mred
           htdp/image)
  
  (provide to-symbol new-object call-back-canvas% overlay-x-y 
           inner->function image-eq? empty-list printer cons-l style-list)
  
  (define (inner->function num-args inner)
    (cond
      ((= 0 num-args) 
       (lambda () (send inner call-back)))
      ((= 1 num-args)
       (lambda (a) (send inner call-back a)))
      ((= 2 num-args)
       (lambda (a b) (send inner call-back a b)))))

  (define (printer s)
    (printf "~a~n" s))

  (define to-symbol string->symbol)
  
  (define style-list `(list 'no-autoclear))
  
  (define cons-l cons)
  
  (define (new-object class . args)
    ((current-eval) #`(make-object #,class #,@args)))
  
  (define empty-list null)

  (define overlay-x-y overlay/xy)
  
  (define image-eq? image=?)
  
  (define call-back-canvas%
    (class canvas%
      (define call-back-proc (lambda (a) (void)))
      (define/override (on-char char)
        (call-back-proc (to-string char)))
      (define/public (set-callback proc)
        (set! call-back-proc proc))
      (super-instantiate ())))  

  (define (to-string ke)
    (let ((ke (send ke get-key-code)))
      (if (char? ke) (string ke) (symbol->string ke))))


)
