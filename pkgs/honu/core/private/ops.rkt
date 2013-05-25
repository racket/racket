#lang scheme

  (provide unary-prefix-ops
           unary-postfix-ops
           (struct-out op)
           (struct-out prefix)
           (struct-out cast-prefix)
           (struct-out infix)
           (struct-out postfix)
           prec-key
           precedence-table
           op-table)

(define unary-prefix-ops '(++
                            --
                            +
                            -
                            !
                            ~))

(define unary-postfix-ops '(++
                             --))

(define-struct op (id))
(define-struct (prefix op) ())
(define-struct (cast-prefix prefix) (type))
(define-struct (infix op) ())
(define-struct (postfix op) ())

(define (prec-key op)
  (and op
       (cons (cond
               [(prefix? op) 'pre]
               [(infix? op) 'in]
               [(postfix? op) 'post])
             (syntax-e (op-id op)))))

(define precedence-table (make-immutable-hash
                           '(((in . |.|) . 100)
                             ((in . #%parens) . 100)
                             ((in . #%brackets) . 100)
                             ((in . #%angles) . 100)
                             ((post . ++) . 100)
                             ((post . --) . 100)
                             ((pre . ++) . 95)
                             ((pre . --) . 95)
                             ((pre . +) . 95)
                             ((pre . -) . 95)
                             ((pre . ~) . 95)
                             ((pre . !) . 95)
                             ((pre . #%parens) . 95)
                             ((in . *) . 90)
                             ((in . %) . 90)
                             ((in . /) . 90)
                             ((in . +) . 85)
                             ((in . -) . 85)
                             ((in . >>) . 80)
                             ((in . <<) . 80)
                             ((in . >>>) . 80)
                             ((in . <) . 70)
                             ((in . >) . 70)
                             ((in . <=) . 70)
                             ((in . >=) . 70)
                             ((in . ==) . 60)
                             ((in . !=) . 60)
                             ((in . &) . 55)
                             ((in . ^) . 50)
                             ((in . \|) . 45)
                             ((in . &&) . 40)
                             ((in . \|\|) . 35)
                             ((in . =) . 10)
                             ((in . +=) . 10)
                             ((in . -=) . 10)
                             ((in . *=) . 10)
                             ((in . /=) . 10)
                             ((in . %=) . 10)
                             ((in . &=) . 10)
                             ((in . ^=) . 10)
                             ((in . \|=) . 10)
                             ((in . <<=) . 10)
                             ((in . >>=) . 10)
                             ((in . >>>=) . 10)
                             ((in . \,) . 6)
                             ((in . :) . 5)
                             ((in . ?) . 4))))
                           

(define op-table (make-hash))
(hash-for-each precedence-table (lambda (k v)
                                  (hash-set! op-table (cdr k) #t)))

