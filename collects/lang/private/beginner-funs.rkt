#lang at-exp racket 

;; weed out: string-copy, eqv?, struct? -- no need for beginners, move to advanced 
;; eq? is questionable, but okay if someone uses BSL to teach not out of HtDP 
;; 

(require mzlib/etc mzlib/list mzlib/math syntax/docprovide
         (for-syntax "firstorder.rkt")
         (for-syntax syntax/parse) 
         (for-syntax racket/syntax))

;; Implements the procedures:
(require "teachprims.rkt" "teach.rkt" lang/posn lang/imageeq "provide-and-scribble.rkt")

(define-syntax (provide-and-wrap stx)
  (syntax-parse stx 
    ; (defproc (name args ...) range w ...)
    [(provide-and-wrap wrap doc-tag:id requires (title df ...) ...)
     (let* ((defs (map syntax->list (syntax->list #'((df ...) ...))))
            (names (map extract-names defs))
            (tmps  (map generate-temporaries names))
            (internals (map (lambda (x)
                              (map (lambda (n) 
                                     (syntax-parse n
                                       [(internal-name:id external-name:id) #'internal-name]
                                       [n:id #'n]))
                                   x))
                            names))
            (externals (map (lambda (x)
                              (map (lambda (n) 
                                     (syntax-parse n
                                       [(internal-name:id external-name:id) #'external-name]
                                       [n:id #'n]))
                                   x))
                            names)))
       (with-syntax* ([((f ...) ...) tmps]
                      [((internal-name ...) ...) internals]
                      [((dg ...) ...)
                       (map (lambda (d.. f.. ex..)
                              (map (lambda (d f external-name)
                                     (syntax-case d ()
                                       [(defproc [name args ...] range w ...)
                                        #`(defproc [(#,f #,external-name) args ...] range w ...)]
                                       [(defthing name range w ...)
                                        #'(defthing name range w ...)]))
                                   d.. f.. ex..))
                            defs tmps externals)])
                     #'(begin ;; create two modules: 
                         ;; one that makes definitions first-order 
                         (module+ with-wrapper 
                           (wrap f internal-name) ... ...
                           (provide-and-scribble doc-tag requires (title dg ...) ...))
                         ;; and one that doesn't
                         (module+ without-wrapper 
                           (provide-and-scribble doc-tag requires (title df ...) ...)))))]))

;; MF: this is now an ugly kludge, left over from my original conversion of Matthew's docs for *SL
(define-syntax (in-rator-position-only stx)
  (syntax-case stx ()
    [(_ new-name orig-name)
     (let ([new (syntax new-name)]
           [orig (syntax orig-name)])
       (cond
         ;; Some things are not really functions:
         [(memq (syntax-e orig) '(pi e null eof))
          #'(define new-name orig-name)]
         [else 
          #'(define-syntax new-name 
              (make-first-order
               (lambda (stx)
                 (syntax-case stx ()
                   [(id . args) (syntax/loc stx (beginner-app orig-name . args))]
                   [_else
                    (raise-syntax-error
                     #f
                     "expected a function call, but there is no open parenthesis before this function"
                     stx)]))
               #'orig-name))]))]))


;; procedures with documentation:
(provide-and-wrap
 in-rator-position-only
 procedures
 
 (begin
   (require scribble/manual scribble/eval "sl-eval.rkt")
   (define (bsl)
     (define *bsl
       (bsl+-eval
        (require 2htdp/image)
        (define c1 (circle 10 "solid" "green"))
        
        (define zero 0)
        
        (define one (list 1))
        
        (define q (make-posn "bye" 2))
        (define p (make-posn 2 -3))
        
        (define a (list (list 'a 22) (list 'b 8) (list 'c 70)))
        (define v (list 1 2 3 4 5 6 7 8 9 'A))
        (define w (list (list (list (list "bye") 3) true) 42))
        (define z (list (list (list (list 'a 'b) 2 3) ) (list false true) "world"))
        (define y (list (list (list 1 2 3) false "world")))
        (define x (list 2 "hello" true))))
     (set! bsl (lambda () *bsl))
     *bsl))
 
 ("Numbers: Integers, Rationals, Reals, Complex, Exacts, Inexacts"
  @defproc[(number? [n any/c]) boolean?]{
                                         Determines whether some value is a number:
                                         @interaction[#:eval (bsl) (number? "hello world") (number? 42)] 
                                         }
  @defproc[(= [x number][y number][z number] ...) boolean?]{
                                                            Compares numbers for equality.
                                                            @interaction[#:eval (bsl) (= 42 2/5)]
                                                            }
  @defproc[(< [x real][y real][z real] ...) boolean?]{
                                                      Compares (real) numbers for less-than.
                                                      @interaction[#:eval (bsl) (< 42 2/5)]
                                                      }
  @defproc[(> [x real][y real][z real] ...) boolean?]{
                                                      Compares (real) numbers for greater-than.
                                                      @interaction[#:eval (bsl) (> 42 2/5)]   
                                                      }
  @defproc[(<= [x real][y real][z real] ...) boolean?]{
                                                       Compares (real) numbers for less-than or equality.
                                                       @interaction[#:eval (bsl) (<= 42 2/5)]
                                                       }
  @defproc[(>= [x real][y real][z real] ...) boolean?]{
                                                       Compares (real) numbers for greater-than or equality.
                                                       @interaction[#:eval (bsl) (>= 42 42)]
                                                       }
  @defproc[((beginner-+ +) [x number][y number][z number] ...) number]{
                                                                       @index["plus"]{}@index["add"]{Adds} up all numbers.
                                                                        @interaction[#:eval (bsl) (+ 2/3 1/16) (+ 3 2 5 8)]
                                                                        }
  @defproc[(- [x number][y number] ...) number]{
                                                @index["subtract"]{Subtracts} the second (and following) number(s) from the first ;
                                                 negates the number if there is only one argument.
                                                 @interaction[#:eval (bsl) (- 5) (- 5 3) (- 5 3 1)]
                                                 }
  @defproc[((beginner-* *) [x number][y number][z number] ...) number]{
                                                                       @index["times"]{}@index["product"]{}@index["multiply"]{Multiplies} all numbers. 
                                                                        @interaction[#:eval (bsl) (* 5 3) (* 5 3 2)]
                                                                        }
  @defproc[((beginner-/ /) [x number][y number][z number] ...) number]{
                                                                       @index["divide"]{Divides} the first by the second (and all following) number(s).
                                                                        @interaction[#:eval (bsl) (/ 12 2) (/ 12 2 3)]
                                                                        }
  @defproc[(max [x real][y real] ...) real]{
                                            Determines the largest number---aka, the @index["maximum"]{maxiumum}. 
                                                                                     @interaction[#:eval (bsl) (max 3 2 8 7 2 9 0)]
                                                                                     }
  @defproc[(min [x real][y real] ...) real]{
                                            Determines the smallest number---aka, the @index["minimum"]{miniumum}. 
                                                                                      @interaction[#:eval (bsl) (min 3 2 8 7 2 9 0)]
                                                                                      }
  @defproc[(quotient [x integer][y integer]) integer]{
                                                      Divides the second integer---also called divisor---into the first---known as 
                                                      dividend---to obtain the @index[(list "divide" "quotient")]{quotient}.
                                                      @interaction[#:eval (bsl) (quotient 9 2) (quotient 3 4)] 
                                                      }
  @defproc[(remainder [x integer][y integer]) integer]{
                                                       Determines the @index[(list "divide" "remainder")]{remainder} of dividing the first by the second integer
                                                                      (exact or inexact). 
                                                                      @interaction[#:eval (bsl) (remainder 9 2) (remainder 3 4)] 
                                                                      } 
  @defproc[(modulo [x integer][y integer]) integer]{
                                                    Finds the remainder of the division of the first number by the second:
                                                    @interaction[#:eval (bsl) (modulo 9 2) (modulo 3 -4)] 
                                                    }
  @defproc[((beginner-sqr sqr) [x number]) number]{
                                                   Computes the @index["square"]{square} of a number.
                                                                @interaction[#:eval (bsl) (sqr 8)]
                                                                }
  @defproc[(sqrt [x number]) number]{
                                     Computes the square root of a number.
                                     @interaction[#:eval (bsl) (sqrt 9) (sqrt 2)]
                                     }
  @defproc[(integer-sqrt [x number]) integer]{
                                              Computes the integer (exact or inexact) square root of a number. 
                                              @interaction[#:eval (bsl) (sqrt 11)]
                                              }
  @defproc[(abs [x real]) real]{
                                Determines the absolute value of a real number.
                                @interaction[#:eval (bsl) (abs -12)]
                                }
  @defproc[(sgn [x real]) (union 1 #i1.0 0 #i0.0 -1 #i-1.0)]{
                                                             Determines the sign of a real number.
                                                             @interaction[#:eval (bsl) (sgn -12)]
                                                             }		       
  
  ;; exponents and logarithms 
  @defproc[(expt [x number][y number]) number]{
                                               Computes the power of the first to the second number.
                                               @interaction[#:eval (bsl) (expt 16 1/2) (expt 3 -4)]
                                               }
  @defproc[(exp [x number]) number]{
                                    Determines e raised to a number.
                                    @interaction[#:eval (bsl) (exp -2)]
                                    }
  @defproc[(log [x number]) number]{
                                    Determines the base-e logarithm of a number.
                                    @interaction[#:eval (bsl) (log 12)]
                                    }
  
  ;; trigonometry
  @defproc[(sin [x number]) number]{
                                    Computes the @index["sine"]{sine} of a number (radians).
                                                 @interaction[#:eval (bsl) (sin pi)]
                                                 }
  @defproc[(cos [x number]) number]{
                                    Computes the @index["cosine"]{cosine} of a number (radians).
                                                 @interaction[#:eval (bsl) (cos pi)]
                                                 }
  @defproc[(tan [x number]) number]{
                                    Computes the @index["tangent"]{tangent} of a number (radians). 
                                                 @interaction[#:eval (bsl) (tan pi)]
                                                 }
  @defproc[(asin [x number]) number]{
                                     Computes the arcsine (inverse of sin) of a number.
                                     @interaction[#:eval (bsl) (asin 0)]
                                     }
  @defproc[(acos [x number]) number]{
                                     Computes the arccosine (inverse of cos) of a number. 
                                     @interaction[#:eval (bsl) (acos 0)]
                                     }
  @defproc[(atan [x number] [y number]) number]{
                                                Computes the arctan of the given number or the ratio of the two given numbers. 
                                                @interaction[#:eval (bsl) (atan 0) (atan 3 4)]
                                                }
  @defproc[(sinh [x number]) number]{
                                     Computes the hyperbolic sine of a number. 
                                     @interaction[#:eval (bsl) (sinh 10)]
                                     }
  @defproc[(cosh [x number]) number]{
                                     Computes the hyperbolic cosine of a number. 
                                     @interaction[#:eval (bsl) (cosh 10)]
                                     }
  
  ;; predicates 
  @defproc[(exact? [x number]) boolean?]{
                                         Determines whether some number is exact.
                                         @interaction[#:eval (bsl) (exact? (sqrt 2))]
                                         }
  @defproc[(integer? [x any/c]) boolean?]{
                                          Determines whether some value is an integer (exact or inexact). 
                                          @interaction[#:eval (bsl) (integer? (sqrt 2))]
                                          }
  @defproc[(zero? [x number]) boolean?]{
                                        Determines if some value is zero or not.  
                                        @interaction[#:eval (bsl) (zero? 2)]
                                        }
  @defproc[(positive? [x number]) boolean?]{
                                            Determines if some value is strictly larger than zero. 
                                            @interaction[#:eval (bsl) (positive? -2)]
                                            }
  @defproc[(negative? [x number]) boolean?]{
                                            Determines if some value is strictly smaller than zero.
                                            @interaction[#:eval (bsl) (negative? -2)]
                                            }
  @defproc[(odd? [x integer]) boolean?]{
                                        Determines if some integer (exact or inexact) is odd or not. 
                                        @interaction[#:eval (bsl) (odd? 2)]
                                        }
  @defproc[(even? [x integer]) boolean?]{
                                         Determines if some integer (exact or inexact) is even or not. 
                                         @interaction[#:eval (bsl) (even? 2)]
                                         }
  @defproc[(rational? [x any/c]) boolean?]{
                                           Determines whether some value is a rational number. 
                                           @interaction[#:eval (bsl) (rational? 1-2i)]
                                           }
  @defproc[(inexact? [x number]) boolean?]{
                                           Determines whether some number is inexact.
                                           @interaction[#:eval (bsl) (inexact? 1-2i)]
                                           }
  @defproc[(real? [x any/c]) boolean?]{
                                       Determines whether some value is a real number. 
                                       @interaction[#:eval (bsl) (real? 1-2i)]
                                       }
  @defproc[(complex? [x any/c]) boolean?]{
                                          Determines whether some value is complex. 
                                          @interaction[#:eval (bsl) (real? 1-2i)]
                                          }
  
  ;; common utilities 
  @defproc[(add1 [x number]) number]{
                                     Incrementes the given number.
                                     @interaction[#:eval (bsl) (add1 2)]
                                     }
  @defproc[(sub1 [x number]) number]{
                                     Decrements the given number.
                                     @interaction[#:eval (bsl) (sub1 2)]
                                     }
  @defproc[(lcm [x integer][y integer] ...) integer]{
                                                     Determines the least common multiple of two integers (exact or inexact). 
                                                     @interaction[#:eval (bsl) (lcm 6 12 8)]
                                                     }
  @defproc[(gcd [x integer][y integer] ...) integer]{
                                                     Determines the greatest common divisior of two integers (exact or inexact). 
                                                     @interaction[#:eval (bsl) (gcd 6 12 8)]
                                                     }
  @defproc[(numerator [x rational?]) integer]{
                                              Computes the numerator of a rational.
                                              @interaction[#:eval (bsl) (numerator 2/3)]
                                              }
  @defproc[(denominator [x rational?]) integer]{
                                                Computees the denominator of a rational.
                                                @interaction[#:eval (bsl) (denominator 2/3)]
                                                }
  @defproc[(floor [x real]) integer]{
                                     Determines the closest integer (exact or inexact) below a real
                                     number. See @racket[round].
                                     @interaction[#:eval (bsl) (floor 12.3)]
                                     }
  @defproc[(ceiling [x real]) integer]{
                                       Determines the closest integer (exact or inexact) above a real
                                       number. See @racket[round]. 
                                       @interaction[#:eval (bsl) (ceiling 12.3)]
                                       }
  @defproc[(round [x real]) integer]{
                                     Rounds a real number to an integer (rounds to even to break ties). See
                                     @racket[floor] and @racket[ceiling]. 
                                     @interaction[#:eval (bsl) (round 12.3)]
                                     }
  @defproc[(make-polar [x real ][y real]) number]{
                                                  Creates a complex from a magnitude and angle. 
                                                  @interaction[#:eval (bsl) (make-polar 3 4)]
                                                  }
  @defproc[(make-rectangular [x real][y real]) number]{
                                                       Creates a complex from a real and an imaginary part. 
                                                       @interaction[#:eval (bsl) (make-rectangular 3 4)] 
                                                       }
  @defproc[(real-part [x number]) real]{
                                        Extracts the real part from a complex number. 
                                        @interaction[#:eval (bsl) (real-part 3+4i)] 
                                        }
  @defproc[(imag-part [x number]) real]{
                                        Extracts the imaginary part from a complex number. 
                                        @interaction[#:eval (bsl) (imag-part 3+4i)] 
                                        }
  @defproc[(magnitude [x number]) real]{
                                        Determines the magnitude of a complex number. 
                                        @interaction[#:eval (bsl) (magnitude (make-polar 3 4))]
                                        }
  @defproc[(angle [x number]) real]{
                                    Extracts the angle from a complex number. 
                                    @interaction[#:eval (bsl) (angle (make-polar 3 4))]
                                    }
  @defproc[(conjugate [x number]) number]{
                                          Extracts the conjugate of a complex number. 
                                          @interaction[#:eval (bsl) (conjugate (make-polar 3 4))]
                                          }
  @defproc[(exact->inexact [x number]) number]{
                                               Converts an exact number to an inexact one. 
                                               @interaction[#:eval (bsl) (exact->inexact 12)]
                                               }
  @defproc[(inexact->exact [x number]) number]{
                                               Approximates an inexact number by an exact one. 
                                               @interaction[#:eval (bsl) (inexact->exact #i12)]
                                               }
  @defproc[(number->string [x number]) string]{
                                               Converts a number to a string. 
                                               @interaction[#:eval (bsl) (number->string 42)]
                                               }
  @defproc[(integer->char [x exact-integer?]) char]{
                                                    Lookups the character that corresponds to the given exact integer in the ASCII table (if any). 
                                                    @interaction[#:eval (bsl) (integer->char 42)]
                                                    }
  @defproc[((beginner-random random) [x integer]) integer]{
                                                           Generates a random natural number less than some given exact integer. 
                                                           @interaction[#:eval (bsl) (random 42)]
                                                           }
  @defproc[(current-seconds) integer]{
                                      Determines the current time in seconds elapsed (since a platform-specific starting date). 
                                      @interaction[#:eval (bsl) (current-seconds)]
                                      }
  @defthing[e real]{Euler's number.
                     @interaction[#:eval (bsl) e]
                     }
  @defthing[pi real]{The ratio of a circle's circumference to its diameter. 
                      @interaction[#:eval (bsl) pi]
                      })
 
 ("Booleans"
  @defproc[(boolean? [x any/c]) boolean?]{
                                          Determines whether some value is a boolean. 
                                          @interaction[#:eval (bsl) (boolean? 42) (boolean? false)]
                                          }
  @defproc[(boolean=? [x boolean?][y boolean?]) boolean?]{
                                                          Determines whether two booleans are equal.
                                                          @interaction[#:eval (bsl) (boolean=? true false)]
                                                          }
  @defproc[(false? [x any/c]) boolean?]{
                                        Determines whether a value is false.
                                        @interaction[#:eval (bsl) (false? false)]
                                        }
  @defproc[((beginner-not not) [x boolean?]) boolean?]{
                                                       Negates a boolean value.
                                                       @interaction[#:eval (bsl) (not false)]
                                                       })
 
 ("Symbols"
  @defproc[(symbol? [x any/c]) boolean?]{
                                         Determines whether some value is a symbol.
                                         @interaction[#:eval (bsl) (symbol? 'a)]
                                         }
  @defproc[(symbol=? [x symbol][y symbol]) boolean?]{
                                                     Determines whether two symbols are equal.
                                                     @interaction[#:eval (bsl) (symbol=? 'a 'b)]
                                                     }
  @defproc[(symbol->string [x symbol]) string]{
                                               Converts a symbol to a string.
                                               @interaction[#:eval (bsl) (symbol->string 'c)]
                                               })
 
 ("Lists"
  @defproc[(cons? [x any/c]) boolean?]{
                                       Determines whether some value is a constructed list. 
                                       @interaction[#:eval (bsl-eval) (cons? (cons 1 empty)) (cons? 42)]
                                       }
  @defproc[(empty? [x any/c]) boolean?]{
                                        Determines whether some value is the empty list.
                                        @interaction[#:eval (bsl) (empty? empty) (empty? 42)]
                                        }
  @defproc[((beginner-cons cons) [x any/x][y list?]) list?]{
                                                            Constructs a list.
                                                            @interaction[#:eval (bsl-eval) (cons 1 empty)]
                                                            }
  @defproc[((beginner-first first) [x cons?]) any/c]{
                                                     Selects the first item of a non-empty list.
                                                     @interaction[#:eval (bsl) x (first x)]
                                                     }
  @defproc[((beginner-rest rest) [x cons?]) any/c]{
                                                   Selects the rest of a non-empty list.
                                                   @interaction[#:eval (bsl) x (rest x)]
                                                   }
  @defproc[(second [x list?]) any/c]{
                                     Selects the second item of a non-empty list. 
                                     @interaction[#:eval (bsl) x (second x)]
                                     }
  @defproc[(third [x list?]) any/c]{
                                    Selects the third item of a non-empty list.
                                    @interaction[#:eval (bsl) x (third x)]
                                    }
  
  @defproc[(fourth [x list?]) any/c]{
                                     Selects the fourth item of a non-empty list. 
                                     @interaction[#:eval (bsl) v (fourth v)]
                                     }
  
  @defproc[(fifth [x list?]) any/c]{
                                    Selects the fifth item of a non-empty list. 
                                    @interaction[#:eval (bsl) v (fifth v)]
                                    }
  @defproc[(sixth [x list?]) any/c]{
                                    Selects the sixth item of a non-empty list.
                                    @interaction[#:eval (bsl) v (sixth v)]
                                    }
  @defproc[(seventh [x list?]) any/c]{
                                      Selects the seventh item of a non-empty list.
                                      @interaction[#:eval (bsl) v (seventh v)]
                                      }
  @defproc[(eighth [x list?]) any/c]{
                                     Selects the eighth item of a non-empty list. 
                                     @interaction[#:eval (bsl) v (eighth v)]
                                     }
  @defproc[(list-ref [x list?][i natural?]) any/c]{
                                                   Extracts the indexed item from the list.
                                                   @interaction[#:eval (bsl) v (list-ref v 9)]
                                                   }
  @defproc[(list [x any/c] ... ) list?]{
                                        Constructs a list of its arguments.
                                        @interaction[#:eval (bsl-eval) (list 1 2 3 4 5 6 7 8 9 0)]
                                        }
  @defproc[(make-list [i natural-number] [x any/c]) list?]{
                                                           Constructs a list of @racket[i] copies of @racket[x].
                                                                                @interaction[#:eval (bsl-eval) (make-list 3 "hello")]
                                                                                }
  @defproc[((beginner-list* list*) [x any/c]  ... [l list?]) list?]{
                                                                    Constructs a list by adding multiple items to a list.
                                                                    @interaction[#:eval (bsl-eval (define x (list 2 "hello" true))) x (list* 4 3 x)]
                                                                    }
  @defproc[((beginner-range range) [start number][end number][step number]) list?]{
                                                                                   Constructs a list of numbers by @racket[step]ping from @racket[start]
                                                                                                                   to @racket[end]. 
                                                                                                                   @interaction[#:eval (bsl-eval) (range 0 10 2)]
                                                                                                                   }
  @defproc[((beginner-append append) [x list?][y list?][z list?]  ...) list?]{
                                                                              Creates a single list from several, by juxtaposition of the items.  
                                                                              @interaction[#:eval (bsl) (append (cons 1 (cons 2 empty)) (cons "a" (cons "b" empty)))]
                                                                              }
  @defproc[(length (l list?)) natural-number?]{
                                               Evaluates the number of items on a list.
                                               @interaction[#:eval (bsl) x (length x)]
                                               }
  @defproc[((beginner-memq memq) [x any/c][l list?]) boolean?]{
                                                               Determines whether some value @racket[x] is on some list @racket[l], 
                                                                                             using @racket[eq?] to compare @racket[x] with items on @racket[l].
                                                                                             @interaction[#:eval (bsl) x (memq (list (list 1 2 3)) x)]
                                                                                             }
  @defproc[((beginner-memq? memq?) [x any/c][l list?]) boolean?]{
                                                                 Determines whether some value @racket[x] is on some list @racket[l], 
                                                                                               using @racket[eq?] to compare @racket[x] with items on @racket[l].
                                                                                               @interaction[#:eval (bsl) x (memq? (list (list 1 2 3)) x)]
                                                                                               }
  @defproc[(memv [x any/c][l list?]) (or/c false list)]{
                                                        Determines whether some value is on the list if so, it produces the
                                                        suffix of the list that starts with x if not, it produces false. (It
                                                        compares values with the eqv? predicate.)  
                                                        @interaction[#:eval (bsl) x (memv (list (list 1 2 3)) x)]
                                                        }
  @defproc[((beginner-member? member?) [x any/c][l list?]) boolean?]{
                                                                     Determines whether some value is on the list (comparing values with equal?).
                                                                     @interaction[#:eval (bsl) x (member? "hello" x)]
                                                                     }
  @defproc[((beginner-member member) [x any/c][l list?]) boolean?]{
                                                                   Determines whether some value is on the list (comparing values with equal?).
                                                                   @interaction[#:eval (bsl) x (member "hello" x)]
                                                                   }
  @defproc[((beginner-remove remove) [x any/c][l list?]) list?]{
                                                                Constructs a list like the given one with the first occurrence of the
                                                                given item removed (comparing values with equal?).
                                                                @interaction[#:eval (bsl) x (remove "hello" x)]
                                                                }
  @defproc[(reverse [l list?]) list]{
                                     Creates a reversed version of a list.
                                     @interaction[#:eval (bsl) x (reverse x)]
                                     }
  @defproc[(assq [x any/c][l list?]) (union false cons?)]{
                                                          Determines whether some item is the first item of a pair in a list of
                                                          pairs. (It compares the items with @racket[eq?].)
                                                          @interaction[#:eval (bsl) a (assq 'b a)]
                                                          }
  
  
  ;; LISP-ish selectors:
  @defproc[(null? [x any/c]) boolean?]{
                                       Determines whether some value is the empty list.
                                       @interaction[#:eval (bsl) (null? empty) (null? 42)]
                                       }
  @defthing[null list]{Another name for the empty list
                        @interaction[#:eval (bsl) null]
                        }
  @defproc[((beginner-car car) [x cons?]) any/c]{
                                                 Selects the first item of a non-empty list.
                                                 @interaction[#:eval (bsl) x (car x)]
                                                 }
  @defproc[((beginner-cdr cdr) [x cons?]) any/c]{
                                                 Selects the rest of a non-empty list.
                                                 @interaction[#:eval (bsl) x (cdr x)]
                                                 }
  @defproc[(cadr [x list?]) any/c]{
                                   LISP-style selector: @racket[(car (cdr x))].
                                                        @interaction[#:eval (bsl) x (cadr x)]
                                                        }
  @defproc[(cdar [x list?]) list?]{
                                   LISP-style selector: @racket[(cdr (car x))].
                                                        @interaction[#:eval (bsl) y (cdar y)]
                                                        }
  @defproc[(caar [x list?]) any/c]{
                                   LISP-style selector: @racket[(car (car x))].
                                                        @interaction[#:eval (bsl) y (caar y)]
                                                        }
  @defproc[(cddr [x list?]) list? ]{
                                    LISP-style selector: @racket[(cdr (cdr x))].
                                                         @interaction[#:eval (bsl) x (cddr x)]
                                                         }
  @defproc[(caddr [x list?]) any/c]{
                                    LISP-style selector: @racket[(caddr x)].
                                                         @interaction[#:eval (bsl) x (caddr x)]
                                                         }
  @defproc[(caadr [x list?]) any/c]{
                                    LISP-style selector: @racket[(car (car (cdr x)))].
                                                         @interaction[#:eval (bsl) z (caadr z)]
                                                         }
  @defproc[(caaar [x list?]) any/c]{
                                    LISP-style selector: @racket[(car (car (car (car x))))].
                                                         @interaction[#:eval (bsl) w (caaar w)]
                                                         }
  @defproc[(cdaar [x list?]) any/c]{
                                    LISP-style selector: @racket[(cdr (cadr (car x)))].
                                                         @interaction[#:eval (bsl) w (cdaar w)]
                                                         }
  @defproc[(cdadr [x list?]) any/c]{
                                    LISP-style selector: @racket[(cdr (car (cdr x)))].
                                                         @interaction[#:eval (bsl) z (cdadr z)]
                                                         }
  @defproc[(cadar [x list?]) any/c]{
                                    LISP-style selector: @racket[(car (cdr (car x)))].
                                                         @interaction[#:eval (bsl) w (cadar w)]
                                                         }
  @defproc[(cddar [x list?]) any/c]{
                                    LISP-style selector: @racket[(cdr (cdr (car x)))]
                                                         @interaction[#:eval (bsl) w (cddar w)]
                                                         }
  @defproc[(cdddr [x list?]) any/c]{
                                    LISP-style selector: @racket[(cdr (cdr (cdr x)))].
                                                         @interaction[#:eval (bsl) v (cdddr v)]
                                                         }
  @defproc[(cadddr [x list?]) any/c]{
                                     LISP-style selector: @racket[(car (cdr (cdr (cdr x))))].
                                                          @interaction[#:eval (bsl) v (cadddr v)]
                                                          })
 
 ("Posns"
  @defproc[(posn) signature]{Signature for posns.}
  @defproc[(make-posn [x any/c][y any/c]) posn]{
                                                Constructs a posn from two arbitrary values.
                                                @interaction[#:eval (bsl) (make-posn 3 3) (make-posn "hello" true)]
                                                }
  @defproc[(posn? [x any/c]) boolean?]{
                                       Determines if its input is a posn.
                                       @interaction[#:eval (bsl) q (posn? q) (posn? 42)]
                                       }
  @defproc[(posn-x [p posn]) any]{
                                  Extracts the x component of a posn.
                                  @interaction[#:eval (bsl) p (posn-x p)]
                                  }
  @defproc[(posn-y [p posn]) any]{
                                  Extracts the y component of a posn.
                                  @interaction[#:eval (bsl) p (posn-y p)]
                                  })
 
 ("Characters"
  @defproc[(char? [x any/c]) boolean?]{
                                       Determines whether a value is a character.
                                       @interaction[#:eval (bsl) (char? "a") (char? #\a)]
                                       }
  @defproc[(char=? [c char][d char][e char] ...) boolean?]{
                                                           Determines whether the characters are equal.
                                                           @interaction[#:eval (bsl) (char=? #\b #\a)]
                                                           }
  @defproc[(char<? [x char][d char][e char] ...) boolean?]{
                                                           Determines whether the characterc are ordered in a strictly increasing manner. 
                                                           @interaction[#:eval (bsl) (char<? #\a #\b #\c)]
                                                           }
  @defproc[(char>? [c char][d char][e char] ...) boolean?]{
                                                           Determines whether the characters are sorted in a strictly decreasing manner. 
                                                           @interaction[#:eval (bsl) (char>? #\A #\z #\a)]
                                                           }
  @defproc[(char<=? [c char][d char][e char] ...) boolean?]{
                                                            Determines whether the characterc are ordered in a strictly increasing manner. 
                                                            @interaction[#:eval (bsl) (char<=? #\a #\a #\b)]
                                                            }
  @defproc[(char>=? [c char][d char][e char] ...) boolean?]{
                                                            Determines whether the characters are sorted in a decreasing manner. 
                                                            @interaction[#:eval (bsl) (char>=? #\b #\b #\a)]
                                                            }
  @defproc[(char-ci=? [c char][d char][e char] ...) boolean?]{
                                                              Determines whether two characters are equal in a case-insensitive
                                                              manner.
                                                              @interaction[#:eval (bsl) (char-ci=? #\b #\B)]
                                                              }
  @defproc[(char-ci<? [c char][d char][e char] ...) boolean?]{
                                                              Determines whether the characterc are ordered in a strictly increasing and case-insensitive manner. 
                                                              @interaction[#:eval (bsl) (char-ci<? #\B #\c) (char<? #\b #\B)]
                                                              }
  @defproc[(char-ci>? [c char][d char][e char] ...) boolean?]{
                                                              Determines whether the characters are sorted in a strictly decreasing and case-insensitive manner.
                                                              @interaction[#:eval (bsl) (char-ci>? #\b #\B) (char>? #\b #\B)]
                                                              }
  @defproc[(char-ci<=? [c char][d char][e char] ...) boolean?]{
                                                               Determines whether the characterc are ordered in an increasing and case-insensitive manner. 
                                                               @interaction[#:eval (bsl) (char-ci<=? #\b #\B) (char<=? #\b #\B)]
                                                               }
  @defproc[(char-ci>=? [c char][d char][e char] ...) boolean?]{
                                                               Determines whether the characters are sorted in a decreasing and case-insensitive manner.
                                                               @interaction[#:eval (bsl) (char-ci>=? #\b #\C) (char>=? #\b #\C)]
                                                               }
  @defproc[(char-numeric? [c char]) boolean?]{
                                              Determines whether a character represents a digit.
                                              @interaction[#:eval (bsl) (char-numeric? #\9)]
                                              }
  @defproc[(char-alphabetic? [c char]) boolean?]{
                                                 Determines whether a character represents an alphabetic character.
                                                 @interaction[#:eval (bsl) (char-alphabetic? #\Q)]
                                                 }
  @defproc[(char-whitespace? [c char]) boolean?]{
                                                 Determines whether a character represents space.
                                                 @interaction[#:eval (bsl) (char-whitespace? #\tab)]
                                                 }
  @defproc[(char-upper-case? [c char]) boolean?]{
                                                 Determines whether a character is an upper-case character.
                                                 @interaction[#:eval (bsl) (char-upper-case? #\T)]
                                                 }
  @defproc[(char-lower-case? [c char]) boolean?]{
                                                 Determines whether a character is a lower-case character.
                                                 @interaction[#:eval (bsl) (char-lower-case? #\T)]
                                                 }
  @defproc[(char-upcase [c char]) char]{
                                        Produces the equivalent upper-case character.
                                        @interaction[#:eval (bsl) (char-upcase #\t)]
                                        }
  @defproc[(char-downcase [c char]) char]{
                                          Produces the equivalent lower-case character.
                                          @interaction[#:eval (bsl) (char-downcase #\T)]
                                          }
  @defproc[(char->integer [c char]) integer]{
                                             Lookups the number that corresponds to the given character in the ASCII table (if any). 
                                             @interaction[#:eval (bsl) (char->integer #\a) (char->integer #\z)]
                                             })
 
 ("Strings"
  @defproc[(string? [x any/c]) boolean?]{
                                         Determines whether a value is a string. 
                                         @interaction[#:eval (bsl) (string? "hello world") (string? 42)]
                                         }
  @defproc[(string-length [s string]) nat]{
                                           Determines the length of a string. 
                                           @interaction[#:eval (bsl) (string-length "hello world")]
                                           }
  @defproc[((beginner-string-ith string-ith) [s string][i natural-number]) 1string?]{
                                                                                     Extracts the @racket[i]th 1-letter substring from @racket[s].
                                                                                                  @interaction[#:eval (bsl) (string-ith "hello world" 1)]
                                                                                                  }
  @defproc[((beginner-replicate replicate) [i natural-number][s string]) string]{
                                                                                 Replicates @racket[s] @racket[i] times. 
                                                                                            @interaction[#:eval (bsl) (replicate 3 "h")]
                                                                                            }
  @defproc[((beginner-int->string int->string) [i integer]) string]{
                                                                    Converts an integer in [0,55295] or [57344 1114111] to a 1-letter string. 
                                                                    @interaction[#:eval (bsl) (int->string 65)]
                                                                    }
  @defproc[((beginner-string->int string->int) [s string]) integer]{
                                                                    Converts a 1-letter string to an integer in [0,55295] or [57344, 1114111]. 
                                                                    @interaction[#:eval (bsl) (string->int "a")]
                                                                    }
  @defproc[((beginner-explode explode) [s string]) (listof string)]{
                                                                    Translates a string into a list of 1-letter strings. 
                                                                    @interaction[#:eval (bsl) (explode "cat")]
                                                                    }
  @defproc[((beginner-implode implode) [l list?]) string]{
                                                          Concatenates the list of 1-letter strings into one string. 
                                                          @interaction[#:eval (bsl) (implode (cons "c" (cons "a" (cons "t" empty))))]
                                                          }
  @defproc[((beginner-string-numeric? string-numeric?) [s string]) boolean?]{
                                                                             Determines whether all 'letters' in the string are numeric. 
                                                                             @interaction[#:eval (bsl) (string-numeric? "123") (string-numeric? "1-2i")]
                                                                             }
  @defproc[((beginner-string-alphabetic? string-alphabetic?) [s string]) boolean?]{
                                                                                   Determines whether all 'letters' in the string are alphabetic. 
                                                                                   @interaction[#:eval (bsl) (string-alphabetic? "123") (string-alphabetic? "cat")]
                                                                                   }
  @defproc[((beginner-string-whitespace? string-whitespace?) [s string]) boolean?]{
                                                                                   Determines whether all 'letters' in the string are white space. 
                                                                                   @interaction[#:eval (bsl) (string-whitespace? (string-append " " (string #\tab #\newline #\return)))]
                                                                                   }
  @defproc[((beginner-string-upper-case? string-upper-case?) [s string]) boolean?]{
                                                                                   Determines whether all 'letters' in the string are upper case. 
                                                                                   @interaction[#:eval (bsl) (string-upper-case? "CAT")]
                                                                                   }
  @defproc[((beginner-string-lower-case? string-lower-case?) [s string]) boolean?]{
                                                                                   Determines whether all 'letters' in the string are lower case. 
                                                                                   @interaction[#:eval (bsl) (string-lower-case? "CAT")]
                                                                                   }
  @defproc[((beginner-string-contains? string-contains?) [s string] [t string]) boolean?]{
                                                                                          Determines whether the first string appears literally in the second one. 
                                                                                          @interaction[#:eval (bsl) (string-contains? "at" "cat")]
                                                                                          }
  @defproc[(string [c char] ...) string?]{
                                          Builds a string of the given characters. 
                                          @interaction[#:eval (bsl) (string #\d #\o #\g)]
                                          }
  @defproc[(make-string [i natural-number][c char]) string]{
                                                            Produces a string of length @racket[i] from @racket[c].
                                                                                        @interaction[#:eval (bsl) (make-string 3 #\d)]
                                                                                        }
  @defproc[(string-ref [s string][i natural-number]) char]{
                                                           Extracts the @racket[i]th character from @racket[s]. 
                                                                        @interaction[#:eval (bsl) (string-ref "cat" 2)]
                                                                        }
  @defproc[(substring [s string][i natural-number][j natural-number]) string]{
                                                                              Extracts the substring starting at @racket[i] up to @racket[j] (or the
                                                                                                                 end if @racket[j] is not provided). 
                                                                                                                 @interaction[#:eval (bsl) (substring "hello world" 1 5) (substring "hello world" 4)]
                                                                                                                 }
  @defproc[(string-copy [s string]) string]{
                                            Copies a string. @;why is it included? 
                                            @interaction[#:eval (bsl) (string-copy "hello")]
                                            }
  @defproc[(string-append [s string] ...) string]{
                                                  Juxtaposes the characters of several strings. 
                                                  @interaction[#:eval (bsl) (string-append "hello" " " "world" " " "good bye")]
                                                  }
  @defproc[(string=? [s string][t string][x string] ...) boolean?]{
                                                                   Determines whether all strings are equal, character for character.
                                                                   @interaction[#:eval (bsl) (string=? "hello" "world") (string=? "bye" "bye")]
                                                                   }
  @defproc[(string<? [s string][t string][x string] ...) boolean?]{
                                                                   Determines whether the strings are ordered in a lexicographically strictly increasing manner.
                                                                   @interaction[#:eval (bsl) (string<? "hello" "world" "zoo")]
                                                                   }
  @defproc[(string>? [s string][t string][x string] ...) boolean?]{
                                                                   Determines whether the strings are ordered in a lexicographically strictly decreasing manner.
                                                                   @interaction[#:eval (bsl) (string>?  "zoo" "world" "hello")]
                                                                   }
  @defproc[(string<=? [s string][t string][x string] ...) boolean?]{
                                                                    Determines whether the strings are ordered in a lexicographically increasing manner.
                                                                    @interaction[#:eval (bsl) (string<=? "hello" "hello" "world" "zoo")]
                                                                    }
  @defproc[(string>=? [s string][t string][x string] ...) boolean?]{
                                                                    Determines whether the strings are ordered in a lexicographically decreasing manner.
                                                                    @interaction[#:eval (bsl) (string>=?  "zoo" "zoo" "world" "hello")]
                                                                    }
  @defproc[(string-ci=?  [s string][t string][x string] ...) boolean?]{
                                                                       Determines whether all strings are equal, character for character, regardless of case. 
                                                                       @interaction[#:eval (bsl) (string-ci=?  "hello" "HellO")]
                                                                       }
  @defproc[(string-ci<?  [s string][t string][x string] ...) boolean?]{
                                                                       Determines whether the strings are ordered in a lexicographically strictly increasing and case-insensitive manner.
                                                                       @interaction[#:eval (bsl) (string-ci<? "hello" "WORLD" "zoo")]
                                                                       }
  @defproc[(string-ci>?  [s string][t string][x string] ...) boolean?]{
                                                                       Determines whether the strings are ordered in a lexicographically strictly decreasing and case-insensitive manner.
                                                                       @interaction[#:eval (bsl) (string-ci>?  "zoo" "WORLD" "hello")]
                                                                       }
  @defproc[(string-ci<=? [s string][t string][x string] ...) boolean?]{
                                                                       Determines whether the strings are ordered in a lexicographically increasing and case-insensitive manner. 
                                                                       @interaction[#:eval (bsl) (string-ci<=? "hello" "WORLD" "zoo")]
                                                                       }
  @defproc[(string-ci>=? [s string][t string][x string] ...) boolean?]{
                                                                       Determines whether the strings are ordered in a lexicographically decreasing and case-insensitive manner.
                                                                       @interaction[#:eval (bsl) (string-ci>?  "zoo" "WORLD" "hello")]
                                                                       }
  @defproc[(string->symbol [s string]) symbol]{
                                               Converts a string into a symbol. 
                                               @interaction[#:eval (bsl) (string->symbol "hello")]
                                               }
  @defproc[(string->number [s string]) (union number false)]{
                                                             Converts a string into a number, produce false if impossible. 
                                                             @interaction[#:eval (bsl) (string->number "-2.03") (string->number "1-2i")]
                                                             }
  @defproc[(string->list [s string]) (listof char)]{
                                                    Converts a string into a list of characters. 
                                                    @interaction[#:eval (bsl) (string->list "hello")]
                                                    }
  @defproc[(list->string [l list?]) string]{
                                            Converts a s list of characters into a string. 
                                            @interaction[#:eval (bsl) (list->string (cons #\c (cons #\a (cons #\t empty))))]
                                            }
  @defproc[(format [f string] [x any/c] ...) string]{
                                                     Formats a string, possibly embedding values. 
                                                     @interaction[#:eval (bsl)
                                                                         (format "Dear Dr. ~a:" "Flatt")
                                                                         (format "Dear Dr. ~s:" "Flatt")
                                                                         (format "the value of ~s is ~a" '(+ 1 1) (+ 1 1))
                                                                         ]
                                                     })
 
 ("Images"
  @defproc[(image? [x any/c]) boolean?]{
                                        Determines whether a value is an image. 
                                        @interaction[#:eval (bsl) c1 (image? c1)]
                                        }
  @defproc[(image=? [i image][j image]) boolean?]{
                                                  Determines whether two images are equal. 
                                                  @interaction[#:eval (bsl) c1 (image=? (circle 5 "solid" "green") c1) (image=? (circle 10 "solid" "green") c1)]
                                                  })
 
 ("Misc"
  @defproc[(identity [x any/c]) any]{
                                     Returns @racket[x].
                                             @interaction[#:eval (bsl) (identity 42) (identity c1)  (identity "hello")]
                                             }
  @defproc[((beginner-error error) [x any/c] ...) void?]{
                                                         Signals an error, combining the given values
                                                         into an error message. If any of the values' 
                                                         printed representations is too long, it is
                                                         truncated and ``...'' is put into the string.
                                                         If the first value is a symbol, it is suffixed with a colon and the
                                                         result pre-pended on to the error message. 
                                                         @interaction[#:eval (bsl) zero (if (= zero 0) (error "can't divide by 0") (/ 1 zero))]
                                                         }
  @defproc[((beginner-struct? struct?) [x any/c]) boolean?]{
                                                            Determines whether some value is a structure. 
                                                            @interaction[#:eval (bsl) (struct? (make-posn 1 2)) (struct? 43)]
                                                            }
  @defproc[((beginner-equal? equal?) [x any/c][y any/c]) boolean?]{
                                                                   Determines whether two values are structurally equal where basic values
                                                                   are compared with the eqv? predicate. 
                                                                   @interaction[#:eval (bsl) (equal? (make-posn 1 2) (make-posn (- 2 1) (+ 1 1)))]
                                                                   }
  @defproc[(eq? [x any/c][y any/c]) boolean?]{
                                              Determines whether two values are equivalent from the computer's perspective (intensional). 
                                              @interaction[#:eval (bsl) (eq? (cons 1 empty) (cons 1 empty)) one (eq? one one)]
                                              }
  @defproc[(eqv? [x any/c][y any/c]) boolean?]{
                                               Determines whether two values are equivalent from the perspective of all functions that can be applied to it (extensional). 
                                               @interaction[#:eval (bsl) (eqv? (cons 1 empty) (cons 1 empty)) one (eqv? one one)]
                                               }
  @defproc[((beginner-=~ =~) [x number][y number][z non-negative-real]) boolean?]{
                                                                                  Checks whether @racket[x] and @racket[y] are within @racket[z] of either other. 
                                                                                                 @interaction[#:eval (bsl) (=~ 1.01 1.0 .1) (=~ 1.01 1.5 .1)]
                                                                                                 }
  @defproc[((beginner-equal~? equal~?) [x any/c][y any/c][z non-negative-real]) boolean?]{
                                                                                          Compares @racket[x] and @racket[y] like @racket[equal?] but uses =~ in the case of numbers. 
                                                                                                   @interaction[#:eval (bsl) (equal~? (make-posn 1.01 1.0) (make-posn 1.01 .99) .2)]
                                                                                                   }
  @defthing[eof eof-object?]{A value that represents the end of a file: 
                              @interaction[#:eval (bsl) eof]
                              }
  @defproc[(eof-object? [x any/c]) boolean?]{
                                             Determines whether some value is the end-of-file value. 
                                             @interaction[#:eval (bsl) (eof-object? eof) (eof-object? 42)]
                                             }
  @defproc[((beginner-exit exit)) void]{
                                        Evaluating @racket[(exit)] terminates the running program. 
                                                   }))
