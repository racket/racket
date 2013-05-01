#lang racket/unit

  (require racket/draw
           racket/class
           racket/list
           (prefix-in file: file/convertible)
           "convertible.rkt")

  (require "common-sig.rkt")

  (import texpict-common-setup^)
  (export texpict-common^ texpict-internal^)

      (define default-seg 5)
      (define recordseplinespace 4)

      (define-struct pict (draw       ; drawing instructions
			   width      ; total width
			   height     ; total height >= ascent + desecnt
			   ascent     ; portion of height above top baseline
			   descent    ; portion of height below bottom baseline
			   children   ; list of child records
			   panbox     ; panorama box, computed on demand
                           last)      ; a descendent for the bottom-right
        #:mutable
        #:property prop:pict-convertible (λ (v) v)
        #:property file:prop:convertible (lambda (v mode default)
                                           (convert-pict v mode default)))
      (define-struct child (pict dx dy sx sy syx sxy))
      (define-struct bbox (x1 y1 x2 y2 ay dy))

      (define (quotient* a b)
	(if (integer? a)
	    (quotient a b)
	    (/ a b)))

      (define blank 
	(case-lambda
	 [() (blank 0 0 0)]
	 [(s) (blank s s)]
	 [(w h) (blank w h 0)]
	 [(w a d) (make-pict `(picture ,w ,(+ a d)) w (+ a d) a d null #f #f)]
	 [(w h a d) (make-pict `(picture ,w ,h) w h a d null #f #f)]))

      (define (extend-pict box dx dy dw da dd draw)
	(let ([w (pict-width box)]
	      [h (pict-height box)]
	      [d (pict-descent box)]
	      [a (pict-ascent box)])
	  (make-pict (if draw draw (pict-draw box))
		     (+ w dw) (+ h da dd) 
		     (max 0 (+ a da)) (max 0 (+ d dd))
		     (list (make-child box dx dy 1 1 0 0))
		     #f
                     (pict-last box))))

      (define (transform dx dy tdx tdy tsx tsy tsxy tsyx)
        (values (+ (* tsx dx) (* tsxy dy) tdx)
                (+ (* tsy dy) (* tsyx dx) tdy)))

      (define (single-pict-offset pict subbox dx dy)
	(let floop ([box pict]
		    [found values]
		    [not-found (lambda () (error 'find-XX
						 "sub-pict: ~a not found in: ~a" 
						 subbox pict))])
	  (if (eq? box subbox)
	      (found dx dy)
	      (let loop ([c (pict-children box)])
		(if (null? c) 
		    (not-found)
		    (floop (child-pict (car c))
			   (lambda (dx dy)
			     (let ([c (car c)])
                               (let-values ([(dx dy)
                                             (transform
                                              dx dy
                                              (child-dx c) (child-dy c)
                                              (child-sx c) (child-sy c)
                                              (child-sxy c) (child-syx c))])
                                 (found dx dy))))
			   (lambda ()
			     (loop (cdr c)))))))))

      (define (find-lbx pict subbox-path dx dy)
	(if (pict? subbox-path)
	    (single-pict-offset pict subbox-path dx dy)
	    (let loop ([l (cons pict subbox-path)])
	      (if (null? (cdr l))
		  (values dx dy)
		  (let-values ([(dx dy) (loop (cdr l))])
                    (single-pict-offset (car l) (cadr l) dx dy))))))

      (define-values (find-lt
		      find-lc
		      find-lb
		      find-ltl
		      find-lbl
		      find-ct
		      find-cc
		      find-cb
		      find-ctl
		      find-cbl
		      find-rt
		      find-rc
		      find-rb
		      find-rtl
		      find-rbl)
	(let ([lb (lambda (x sx w d a) x)]
	      [c (lambda (x sx w d a) (+ x (* sx (quotient* w 2))))]
	      [rt (lambda (x sx w d a) (+ x (* sx w)))]
	      [tline (lambda (x sx w d a) (+ x (* sx (- w a))))]
	      [bline (lambda (x sx w d a) (+ x (* sx d)))]
	      [find (lambda (get-x get-y)
		      (lambda (pict pict-path)
                        (let ([p (let loop ([path pict-path])
                                   (cond
                                    [(pict? path) path]
                                    [(null? (cdr path)) (loop (car path))]
                                    [else (loop (cdr path))]))])
                            (let ([w (pict-width p)]
                                  [h (pict-height p)]
                                  [d (pict-descent p)]
                                  [a (pict-ascent p)])
                              (find-lbx pict pict-path
                                        (get-x 0 1 w 0 0)
                                        (get-y 0 1 h d a))))))])
	  (values (find lb rt)
		  (find lb c)
		  (find lb lb)
		  (find lb tline)
		  (find lb bline)
		  (find c rt)
		  (find c c)
		  (find c lb)
		  (find c tline)
		  (find c bline)
		  (find rt rt)
		  (find rt c)
		  (find rt lb)
		  (find rt tline)
		  (find rt bline))))

      (define-values (lt-find
		      lc-find
		      lb-find
		      ltl-find
		      lbl-find
		      ct-find
		      cc-find
		      cb-find
		      ctl-find
		      cbl-find
		      rt-find
		      rc-find
		      rb-find
		      rtl-find
		      rbl-find)
	(let ([flip (lambda (orig)
		      (lambda (pict pict-path)
			(let-values ([(x y) (orig pict pict-path)])
			  (values x (- (pict-height pict) y)))))])
	  (values (flip find-lt)
		  (flip find-lc)
		  (flip find-lb)
		  (flip find-ltl)
		  (flip find-lbl)
		  (flip find-ct)
		  (flip find-cc)
		  (flip find-cb)
		  (flip find-ctl)
		  (flip find-cbl)
		  (flip find-rt)
		  (flip find-rc)
		  (flip find-rb)
		  (flip find-rtl)
		  (flip find-rbl))))

      (define (launder box)
	(unless (pict-panbox box)
	  (panorama-box! box))
	(let ([b (extend-pict box 0 0 0 0 0 #f)])
	  (set-pict-children! b null)
	  (set-pict-panbox! b (pict-panbox box))
          ;; After laundering, we can't find the last-pos box.
          ;; So create a new last-position box to preserve the
          ;; original shape:
          (let ([l (pict-last box)])
            (set-pict-last! box #f) ; preserve invariants
            (cond
             [(not l) b]
             [else 
              (let-values ([(x y) (lt-find box l)]
                           [(l) (let loop ([l l])
                                  (if (pair? l)
                                      (if (null? (cdr l))
                                          (car l)
                                          (loop (cdr l)))
                                      l))])
                (let ([b2 (blank (pict-width l) (pict-height l)
                                 (pict-ascent l) (pict-descent l))])
                  (use-last/unchecked
                   (pin-over b x y b2)
                   b2)))]))))

      (define (lift p n)
	(let* ([dh (- (max 0 (- n (pict-descent p))))]
	       [do-a? (= (pict-height p)
			 (+ (pict-ascent p) (pict-descent p)))]
	       [h2 (+ dh (pict-height p))]
	       [d2 (max 0 (- (pict-descent p) n))])
	  (make-pict (pict-draw p)
		     (pict-width p) h2
		     (if do-a?
			 (- h2 d2)
			 (pict-ascent p))
		     d2
		     (map (lambda (c)
			    (make-child 
			     (child-pict c)
			     (child-dx c)
			     (+ dh (child-dy c))
			     1 1
                             0 0))
			  (pict-children p))
		     #f
                     (pict-last p))))

      (define (drop p n)
	(let* ([dh (- (max 0 (- n (pict-ascent p))))]
	       [do-d? (= (pict-height p)
			 (+ (pict-ascent p) (pict-descent p)))]
	       [h2 (+ dh (pict-height p))]
	       [a2  (max 0 (- (pict-ascent p) n))])
	  (make-pict (pict-draw p)
		     (pict-width p) h2
		     a2
		     (if do-d?
			 (- h2 a2)
			 (pict-descent p))
		     (pict-children p)
		     #f
                     (pict-last p))))

      (define (baseless p)
	(let ([p (lift p (pict-descent p))])
	  (drop p (- (pict-ascent p) (pict-height p)))))

      (define (refocus p c)
	(let-values ([(x y) (find-lt p c)])
	  (let ([p (inset p 
			  (- x) (- y (pict-height p)) 
			  (- (- (pict-width p) x (pict-width c)))
			  (- (pict-height c) y))])
	    (make-pict (pict-draw p)
		       (pict-width c) (pict-height c)
		       (pict-ascent c) (pict-descent c)
		       (pict-children p)
		       #f
                       (last* c)))))

      (define (panorama-box! p)
	(let ([bb (pict-panbox p)])
	  (if bb
	      (values (bbox-x1 bb) (bbox-y1 bb) (bbox-x2 bb) (bbox-y2 bb)
		      (bbox-ay bb) (bbox-dy bb))
	      (let loop ([x1 0][y1 0][x2 (pict-width p)][y2 (pict-height p)]
			 [ay (- (pict-height p) (pict-ascent p))][dy (pict-descent p)]
			 [l (pict-children p)])
		(if (null? l)
		    (begin
		      (set-pict-panbox! p (make-bbox x1 y1 x2 y2 ay dy))
		      (values x1 y1 x2 y2 ay dy))
		    (let ([c (car l)])
		      (let-values ([(cx1 cy1 cx2 cy2 cay cdy) (panorama-box! (child-pict c))])
			(loop (min x1 (* (+ cx1 (child-dx c)) (child-sx c)))
			      (min y1 (* (+ cy1 (child-dy c)) (child-sy c)))
			      (max x2 (* (+ cx2 (child-dx c)) (child-sx c)))
			      (max y2 (* (+ cy2 (child-dy c)) (child-sy c)))
			      (max ay (* (+ cay (child-dy c)) (child-sy c)))
			      (min dy (* (+ cdy (child-dy c)) (child-sy c)))
			      (cdr l)))))))))

      (define (panorama p)
	(let-values ([(x1 y1 x2 y2 ay dy) (panorama-box! p)])
	  (let ([h (- y2 y1)])
	    (place-over (blank (- x2 x1) h (- h ay) dy)
			(- x1) (- y2 (pict-height p))
			p))))

      (define (clip-descent b)
	(let* ([w (pict-width b)]
	       [h (pict-height b)]
	       [d (pict-descent b)])
	  (extend-pict
	   b 0 (- d) 
	   0 0 (- d)
	   `(picture ,w ,(- h d)
		     (put 0 ,(- d) ,(pict-draw b))))))

      (define (clip-ascent b)
	(let* ([w (pict-width b)]
	       [h (pict-height b)]
	       [a (pict-descent b)])
	  (extend-pict
	   b 0 a
	   0 (- a) 0
	   `(picture ,w ,(- h a)
		     (put 0 0 ,(pict-draw b))))))

      (define (thickness mode b)
	(let* ([w (pict-width b)]
	       [h (pict-height b)])
	  (extend-pict
	   b 0 0 0 0 0
	   `(picture ,w ,h
		     (thickness ,mode ,(pict-draw b))))))

      (define (thick b) (thickness 'thicklines b))
      (define (thin b) (thickness 'thinlines b))
      (define (line-thickness n b) (thickness n b))
      (define (line-style n s) (thickness n s))

      (define inset
	(case-lambda
	 [(box a) (inset box a a a a)]
	 [(box h v) (inset box h v h v)]
	 [(box l t r b)
	  (let ([w (+ l r (pict-width box))]
		[h (+ t b (pict-height box))])
	    (extend-pict
	     box l b
	     (+ l r) t b
	     `(picture
	       ,w ,h
	       (put ,l ,b ,(pict-draw box)))))]))

      (define (use-last* p sub-p)
        (use-last p (last* sub-p)))

      (define (last* sub-p)
        ;; Either use `sub-p' for last or create a path though `sub-p'
        ;; to the last of `sub-p' (in case the last of `sub-p' is also
        ;; in other places within `p')
        (let ([l (pict-last sub-p)])
          (cond
           [(not l) sub-p]
           [(eq? l sub-p) sub-p]
           [(pair? l) (if (eq? (car l) sub-p)
                          l
                          (cons sub-p l))]
           [else (list sub-p l)])))

      (define (use-last p sub-p)
	(if (let floop ([p p] [sub-p sub-p])
              (or (eq? p sub-p)
                  (and (pair? sub-p)
                       (eq? p (car sub-p))
                       (or (null? (cdr sub-p))
                           (floop p (cdr sub-p))))
                  (ormap (lambda (c) (floop (child-pict c) sub-p))
                         (pict-children p))))
            (use-last/unchecked p sub-p)
            (error 'use-last
                   "given new last-pict path: ~e not in the base pict: ~e"
                   sub-p
                   p)))

     (define (use-last/unchecked p sub-p)
       (make-pict (pict-draw p)
                  (pict-width p) (pict-height p)
                  (pict-ascent p) (pict-descent p)
                  (list (make-child p 0 0 1 1 0 0))
                  #f
                  sub-p))

      (define dash-frame 
	(case-lambda
	 [(box) (dash-frame box default-seg)]
	 [(box seg)
	  (let ([w (pict-width box)]
		[h (pict-height box)])
	    (extend-pict
	     box 0 0 0 0 0
	     `(picture
	       ,w ,h
	       (put 0 0 ,(pict-draw box))
	       (put 0 0 ,(pict-draw (dash-hline w 0 seg)))
	       (put 0 ,h ,(pict-draw (dash-hline w 0 seg)))
	       (put 0 0 ,(pict-draw (dash-vline 0 h seg)))
	       (put ,w 0 ,(pict-draw (dash-vline 0 h seg))))))]))

      (define (frame box)
	(dash-frame box (max (pict-width box) (pict-height box))))

      (define (dash-line width height rotate seg)
	(let ([vpos (quotient* height 2)])
	  (make-pict
	   `(picture
	     ,@(rotate width height)
	     ,@(if (>= seg width)
		   `((put ,@(rotate 0 vpos) (line ,@(rotate 1 0) ,width)))
		   (let* ([remain (+ (- width (floor width))
				     (remainder (floor width) (* 2 seg)))]
			  [count (inexact->exact (floor (quotient* width (* 2 seg))))]
			  [lremain (quotient* remain 2)]
			  [rremain (- remain lremain)])
		     `((put ,@(rotate 0 vpos) (line ,@(rotate 1 0) ,lremain))
		       ,@(let loop ([count count][pos lremain])
			   (if (zero? count)
			       null
			       (cons `(put ,@(rotate (+ pos seg) vpos) 
					   (line ,@(rotate 1 0) ,seg))
				     (loop (sub1 count) (+ pos seg seg)))))
		       (put ,@(rotate (- width rremain) vpos) 
			    (line ,@(rotate 1 0) ,rremain))))))
	   (car (rotate width height))
	   (cadr (rotate width height))
	   (cadr (rotate 0 height)) 0
	   null
	   #f
           #f)))

      (define (rlist b a) (list a b))

      (define (hline width height)
	(dash-line width height list width))

      (define (vline width height)
	(dash-line height width rlist height))

      (define dash-hline
	(case-lambda 
	 [(width height) (dash-hline width height default-seg)]
	 [(width height seg) (dash-line width height list seg)]))

      (define dash-vline
	(case-lambda 
	 [(width height) (dash-vline width height default-seg)]
	 [(width height seg) (dash-line height width rlist seg)]))

      (define (oval box)
	(let ([w (pict-width box)]
	      [h (pict-height box)])
	  (extend-pict
	   box 0 0 0 0 0
	   `(picture
	     ,w ,h
	     (put 0 0 ,(pict-draw box))
	     (put ,(quotient* w 2) ,(quotient* h 2) (oval "" ,w ,h))))))

      (define (oval/radius box r)
	(let* ([w (pict-width box)]
	       [h (pict-height box)]
	       [rr (* 2 r)]
	       [lw (- w rr)]
	       [lh (- h rr)])
	  (extend-pict
	   box 0 0 0 0 0
	   `(picture
	     ,w ,h
	     (put 0 0 ,(pict-draw box))
	     (put ,r ,r (oval "[bl]" ,rr ,rr))
	     (put ,r 0 (line 1 0 ,lw))
	     (put ,(- w r) ,r (oval "[br]" ,rr ,rr))
	     (put ,w ,r (line 0 1 ,lh))
	     (put ,r ,(- h r) (oval "[tl]" ,rr ,rr))
	     (put ,r ,h (line 1 0 ,lw))
	     (put ,(- w r) ,(- h r) (oval "[tr]" ,rr ,rr))
	     (put ,0 ,r (line 0 1 ,lh))))))

      (define (big-circle d)
	(let ([r (quotient* d 2)])
	  (picture
	   d d
	   `((curve 0 ,r ,r 0 0 0)
	     (curve ,r 0 ,d ,r ,d 0)
	     (curve ,d ,r ,r ,d ,d ,d)
	     (curve ,r ,d 0 ,r 0 ,d)))))

      (define (ghost box)
	(let ([w (pict-width box)]
	      [h (pict-height box)])
	  (extend-pict
	   box 0 0 0 0 0
	   `(picture
	     ,w ,h))))

      (define-values (vl-append 
		      vc-append 
		      vr-append 
		      ht-append
		      hc-append
		      hb-append
		      htl-append
		      hbl-append)
	(let ([make-append-boxes 
	       (lambda (wcomb hcomb fxoffset fyoffset rxoffset ryoffset 
			      combine-ascent combine-descent)
                 (letrec ([*-append
                           (lambda (sep . args)
                             (unless (number? sep)
                               (set! args (cons sep args))
                               (set! sep 0))
                             (let append-boxes ([args args])
                               (cond
                                 [(null? args) (blank)]
                                 [(null? (cdr args)) (car args)]
                                 [else
                                  (let* ([first (car args)]
                                         [rest (append-boxes (cdr args))]
                                         [w (wcomb (pict-width first) (pict-width rest) sep first rest)]
                                         [h (hcomb (pict-height first) (pict-height rest) sep first rest)]
                                         [fw (pict-width first)]
                                         [fh (pict-height first)]
                                         [rw (pict-width rest)]
                                         [rh (pict-height rest)]
                                         [fd1 (pict-ascent first)]
                                         [fd2 (pict-descent first)]
                                         [rd1 (pict-ascent rest)]
                                         [rd2 (pict-descent rest)]
                                         [dx1 (fxoffset fw fh rw rh sep fd1 fd2 rd1 rd2)]
                                         [dy1 (fyoffset fw fh rw rh sep fd1 fd2 rd1 rd2 h)]
                                         [dx2 (rxoffset fw fh rw rh sep fd1 fd2 rd1 rd2)]
                                         [dy2 (ryoffset fw fh rw rh sep fd1 fd2 rd1 rd2 h)])
                                    (make-pict
                                     `(picture 
                                       ,w ,h
                                       (put ,dx1
                                            ,dy1
                                            ,(pict-draw first))
                                       (put ,dx2
                                            ,dy2
                                            ,(pict-draw rest)))
                                     w h
                                     (combine-ascent fd1 rd1 fd2 rd2 fh rh h (+ dy1 fh) (+ dy2 rh))
                                     (combine-descent fd2 rd2 fd1 rd1 fh rh h (- h dy1) (- h dy2))
                                     (list (make-child first dx1 dy1 1 1 0 0)
                                           (make-child rest dx2 dy2 1 1 0 0))
                                     #f
                                     (last* rest)))])))])
                   *-append))]
	      [2max (lambda (a b c . rest) (max a b))]
	      [zero (lambda (fw fh rw rh sep fd1 fd2 rd1 rd2 . args) 0)]
	      [fv (lambda (a b . args) a)]
	      [sv (lambda (a b . args) b)]
	      [min2 (lambda (a b . args) (min a b))]
	      [max2 (lambda (a b . args) (max a b))]
              [3+ (lambda (a b c . args) (+ a b c))]
              [a-max (lambda (a b c first rest)
                       (+ (max (pict-ascent first) (pict-ascent rest))
                          (max (- (pict-height first) (pict-ascent first))
                               (- (pict-height rest) (pict-ascent rest)))))]
              [d-max (lambda (a b c first rest)
                       (+ (max (pict-descent first) (pict-descent rest))
                          (max (- (pict-height first) (pict-descent first))
                               (- (pict-height rest) (pict-descent rest)))))]
	      [min-ad (lambda (a b oa ob ah bh h da db)
			(- h (max oa ob) (max (- ah oa a)
					      (- bh ob b))))]
	      [xmin-ad (lambda (a b oa ob ah bh h da db)
			 (min (+ (- h da) a) (+ (- h db) b)))])
	  (values
	   (make-append-boxes 2max 3+ 
			      zero (lambda (fw fh rw rh sep . a) (+ sep rh))
			      zero zero 
			      fv sv)
	   (make-append-boxes 2max 3+ 
			      (lambda (fw fh rw rh sep . a) (quotient* (- (max fw rw) fw) 2))
			      (lambda (fw fh rw rh sep . a) (+ sep rh))
			      (lambda (fw fh rw rh sep . a) (quotient* (- (max fw rw) rw) 2))
			      zero 
			      fv sv)
	   (make-append-boxes 2max 3+ 
			      (lambda (fw fh rw rh sep . a) (- (max fw rw) fw))
			      (lambda (fw fh rw rh sep . a) (+ sep rh))
			      (lambda (fw fh rw rh sep . a) (- (max fw rw) rw))
			      zero 
			      fv sv)
	   (make-append-boxes 3+ 2max
			      zero
			      (lambda (fw fh rw rh sep . a) (- (max fh rh) fh))
			      (lambda (fw fh rw rh sep . a) (+ sep fw))
			      (lambda (fw fh rw rh sep . a) (- (max fh rh) rh))
			      xmin-ad xmin-ad)
	   (make-append-boxes 3+ 2max
			      zero
			      (lambda (fw fh rw rh sep . a) (quotient* (- (max fh rh) fh) 2))
			      (lambda (fw fh rw rh sep . a) (+ sep fw))
			      (lambda (fw fh rw rh sep . a) (quotient* (- (max fh rh) rh) 2))
			      xmin-ad xmin-ad)
	   (make-append-boxes 3+ 2max 
			      zero zero
			      (lambda (fw fh rw rh sep . a) (+ sep fw)) zero
			      xmin-ad xmin-ad)
	   (make-append-boxes 3+ a-max
			      zero
			      (lambda (fw fh rw rh sep fd1 fd2 rd1 rd2 h) 
				(- h fh (- (max fd1 rd1) fd1)))
			      (lambda (fw fh rw rh sep . a) (+ sep fw))
			      (lambda (fw fh rw rh sep fd1 fd2 rd1 rd2 h) 
				(- h rh (- (max fd1 rd1) rd1)))
			      max2 min-ad)
	   (make-append-boxes 3+ d-max
			      zero
			      (lambda (fw fh rw rh sep fd1 fd2 rd1 rd2 h) 
				(- (max fd2 rd2) fd2))
			      (lambda (fw fh rw rh sep . a) (+ sep fw))
			      (lambda (fw fh rw rh sep fd1 fd2 rd1 rd2 h) 
				(- (max fd2 rd2) rd2))
			      min-ad max2))))

      (define-values (lt-superimpose
		      lb-superimpose
		      lc-superimpose
		      ltl-superimpose
		      lbl-superimpose
		      rt-superimpose
		      rb-superimpose
		      rc-superimpose
		      rtl-superimpose
		      rbl-superimpose
		      ct-superimpose
		      cb-superimpose
		      cc-superimpose
		      ctl-superimpose
		      cbl-superimpose)
	(let ([make-superimpose 
	       (lambda (get-h get-v get-th name)
		 (lambda boxes
                   (when (null? boxes)
                     (error name "expected at least one argument, got none"))
                   (unless (andmap pict? boxes)
                     (error name "expected all picts as arguments, got ~a"
                            (apply string-append (add-between (map (λ (x) (format "~e" x)) boxes) " "))))
		   (let ([max-w (apply max (map pict-width boxes))]
			 [max-h (apply max (map pict-height boxes))]
			 [max-a (apply max (map pict-ascent boxes))]
			 [max-a-complement (apply max (map (lambda (b) (- (pict-height b) (pict-ascent b)))
							   boxes))]
			 [max-d (apply max (map pict-descent boxes))]
			 [max-d-complement (apply max (map (lambda (b) (- (pict-height b) (pict-descent b)))
							   boxes))])
		     (let ([p (picture max-w (get-th max-h max-a max-d max-a-complement max-d-complement)
				       (map (lambda (box)
					      `(place ,(get-h max-w (pict-width box))
						      ,(get-v max-h (pict-height box)
							      max-d (pict-descent box)
							      max-a-complement (pict-ascent box))
						      ,box))
					    boxes))])
		       ;; Figure out top and bottom baselines by finding the picts again, etc:
		       (let ([ys (map (lambda (box)
					(let-values ([(x y) (find-lt p box)])
					  y))
				      boxes)])
			 (let ([min-a (apply min (map (lambda (box y)
							(+ (- (pict-height p) y) (pict-ascent box)))
						      boxes ys))]
			       [min-d (apply min (map (lambda (box y)
							(+ (- y (pict-height box)) (pict-descent box)))
						      boxes ys))])
			   (make-pict (pict-draw p)
				      (pict-width p) (pict-height p)
				      min-a min-d
				      (pict-children p)
				      #f
                                      ;; Find bottomost, rightmost of old last picts to be the
                                      ;;  new last pict.
                                      (let ([subs (map (lambda (box)
                                                         (let ([last (last* box)])
                                                           (let-values ([(x y) (rb-find p last)])
                                                             (list last x y))))
                                                       boxes)])
                                        (if (null? subs)
                                            #f
                                            (caar (sort subs
                                                        (lambda (a b)
                                                          (let ([ay (caddr a)]
                                                                [by (caddr b)])
                                                            (cond
                                                             [(ay . > . by) #t]
                                                             [(= ay by) ((cadr a) . > . (cadr b))]
                                                             [else #f]))))))))))))))]
	      [norm (lambda (h a d ac dc) h)]
	      [tbase (lambda (h a d ac dc) (+ a ac))] 
	      [bbase (lambda (h a d ac dc) (+ d dc))] 
	      [lb (lambda (m v . rest) 0)]
	      [rt (lambda (m v . rest) (- m v))]
	      [tline (lambda (m v md d mac a) (- mac (- v a)))]
	      [bline (lambda (m v md d mac a) (- md d))]
	      [c (lambda (m v . rest) (quotient* (- m v) 2))])
          (values
           (make-superimpose lb rt norm 'lt-superimpose)
           (make-superimpose lb lb norm 'lb-superimpose)
           (make-superimpose lb c norm 'lc-superimpose)
           (make-superimpose lb tline tbase 'ltl-superimpose)
           (make-superimpose lb bline bbase 'lbl-superimpose)
           (make-superimpose rt rt norm 'rt-superimpose)
           (make-superimpose rt lb norm 'rb-superimpose)
           (make-superimpose rt c norm 'rc-superimpose)
           (make-superimpose rt tline tbase 'rtl-superimpose)
           (make-superimpose rt bline bbase 'rbl-superimpose)
           (make-superimpose c rt norm 'ct-superimpose)
           (make-superimpose c lb norm 'cb-superimpose)
           (make-superimpose c c norm 'cc-superimpose)
           (make-superimpose c tline tbase 'ctl-superimpose)
           (make-superimpose c bline bbase 'cbl-superimpose))))

      (define table
	(case-lambda
	 [(ncol cells col-aligns row-aligns col-seps row-seps)
	  (unless (positive? ncol)
	    (raise-type-error 'table "positive column count" ncol))
	  (let ([count (length cells)])
	    (unless (zero? (remainder count ncol))
	      (error 'table "cell count isn't divisble by the provided column count"))
	    (let* ([w ncol]
		   [h (/ count w)]
		   [cells (let rloop ([r h][cells cells][r-acc null])
			    (if (zero? r)
				(list->vector (reverse r-acc))
				(let loop ([c w][cells cells][one-acc null])
				  (if (zero? c)
				      (rloop (sub1 r) cells (cons (list->vector (reverse one-acc)) r-acc))
				      (loop (sub1 c) (cdr cells) (cons (car cells) one-acc))))))]
		   [imp-list->vector (lambda (l n)
				       (let ([v (make-vector n)])
					 (let loop ([l l][p 0])
					   (unless (= n p)
					     (vector-set! v
							  p
							  (if (pair? l)
							      (car l)
							      l))
					     (loop (if (pair? l) (cdr l) l) (add1 p))))
					 v))]
		   [ralign (imp-list->vector row-aligns h)]
		   [calign (imp-list->vector col-aligns w)]
		   [rsep (imp-list->vector row-seps h)]
		   [csep (imp-list->vector col-seps w)]
		   [get-cell (lambda (c r) (vector-ref (vector-ref cells r) c))]
		   [nmap (lambda (f w)
			   (let loop ([n w][acc null])
			     (if (zero? n)
				 acc
				 (loop (sub1 n) (cons (f (sub1 n)) acc)))))]
		   [rowmap (lambda (f) (nmap f h))]
		   [colmap (lambda (f) (nmap f w))]
		   [superimposed-rows (list->vector
				       (rowmap (lambda (r)
						 (apply
						  (vector-ref ralign r)
						  (colmap (lambda (c) (get-cell c r)))))))]
		   [superimposed-cols (list->vector
				       (colmap (lambda (c)
						 (apply
						  (vector-ref calign c)
						  (rowmap (lambda (r) (get-cell c r)))))))])
	      ; No space after the last row/col
	      (vector-set! rsep (sub1 h) 0)
	      (vector-set! csep (sub1 w) 0)

	      (apply
	       vl-append
	       0
	       (rowmap
		(lambda (r)
		  (vl-append
		   0
		   (apply
		    ht-append
		    0
		    (colmap (lambda (c)
			      (ht-append
			       0
			       (let* ([cell (get-cell c r)]
				      [sc (vector-ref superimposed-cols c)]
				      [sr (vector-ref superimposed-rows r)]
				      [w (pict-width sc)]
				      [h (pict-height sr)])
				 (let-values ([(x __) (find-lb sc cell)]
					      [(_  y) (find-lb sr cell)])
				   (picture
				    w h
				    `((place ,x ,y ,cell)))))
			       (blank (vector-ref csep c) 0)))))
		   (blank 0 (vector-ref rsep r))))))))]))

      (define (record title . fields)
	(let* ([totalwidth (apply max (pict-width title) (map pict-width fields))]
	       [linespace (if (null? fields) 0 recordseplinespace)]
	       [totalheight (+ (pict-height title) (apply + (map pict-height fields))
			       linespace)]
	       [title-y (- totalheight (pict-height title))]
	       [field-ys (let loop ([pos (- totalheight (pict-height title) linespace)]
				    [fields fields])
			   (if (null? fields)
			       null
			       (let* ([p (- pos (pict-height (car fields)))])
				 (cons p
				       (loop p (cdr fields))))))])
	  (make-pict
	   `(picture
	     ,totalwidth ,totalheight
	     (put 0 0 (line 1 0 ,totalwidth))
	     (put 0 ,totalheight (line 1 0 ,totalwidth))
	     (put 0 0 (line 0 1 ,totalheight))
	     (put ,totalwidth 0 (line 0 1 ,totalheight))
	     (put 0 ,title-y ,(pict-draw title))
	     ,@(if (null? fields)
		   '()
		   `((put 0 ,(- totalheight (pict-height title) (quotient* linespace 2))
			  (line 1 0 ,totalwidth))))
	     ,@(map (lambda (f p) `(put 0 ,p ,(pict-draw f)))
		    fields field-ys))
	   totalwidth totalheight
	   totalheight 0
	   (cons
	    (make-child title 0 title-y 1 1 0 0)
	    (map (lambda (child child-y) (make-child child 0 child-y 1 1 0 0)) fields field-ys))
	   #f
           #f)))

      (define (picture* w h a d commands)
	(let loop ([commands commands][translated null][children null])
	  (if (null? commands)
	      (make-pict
	       `(picture ,w ,h
			 ,@(reverse translated))
	       w h a d
	       children
	       #f
               #f)
	      (let ([c (car commands)]
		    [rest (cdr commands)])
		(unless (and (pair? c) (symbol? (car c)))
		  (error 'picture "bad command: ~a" c))
		(case (car c)
		  [(connect) (loop rest
				   (append (apply connect (cdr c))
					   translated)
				   children)]
		  [(dconnect) (loop rest
				    (let ([x (cadr c)]
					  [y (caddr c)]
					  [dx (cadddr c)]
					  [dy (list-ref c 4)])
				      (append (connect x y (+ x dx) (+ y dy)
						       (if (null? (list-tail c 5))
							   #t
							   (list-ref c 5)))
					      translated))
				    children)]
		  [(connect~y) (loop rest
				     (append (apply ~connect 'x (cdr c))
					     translated)
				     children)]
		  [(connect~x) (loop rest
				     (append (apply ~connect 'y (cdr c))
					     translated)
				     children)]
		  [(connect~xy) (loop rest
				      (append (apply ~connect 'r (cdr c))
					      translated)
				      children)]
		  [(curve) (loop rest
				 (let ([x1 (cadr c)]
				       [y1 (caddr c)]
				       [x2 (cadddr c)]
				       [y2 (list-ref c 4)]
				       [xm (list-ref c 5)]
				       [ym (list-ref c 6)]
				       [d (if (null? (list-tail c 7))
					      1.0
					      (list-ref c 7))])
				   (let ([p (if (and d (>= d 0))
						(inexact->exact (floor (* d (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))))))
						#f)])
				     (if (and (= x1 x2) (= y1 y2))
					 translated
					 (cons `(qbezier ,p ,x1 ,y1 ,xm ,ym ,x2 ,y2)
					       translated))))
				 children)]
		  [(place) (let ([x (cadr c)]
				 [y (caddr c)]
				 [p (cadddr c)])
			     (loop rest
				   (cons
				    `(put ,x ,y ,(pict-draw p))
				    translated)
				   (cons
				    (make-child p x y 1 1 0 0)
				    children)))]
		  [else (loop rest (cons c translated) children)])))))

      (define (picture w h commands)
	(picture* w h h 0 commands))

      (define (cons-picture p commands)
	(picture
	 (pict-width p) (pict-height p)
	 (cons
	  `(place 0 0 ,p)
	  commands)))

      (define (cons-picture* p commands)
	(picture*
	 (pict-width p) (pict-height p)
	 (pict-ascent p) (pict-descent p)
	 (cons
	  `(place 0 0 ,p)
	  commands)))

      (define (place-it who flip? base dx dy target)
	(let-values ([(dx dy)
		      (cond
		       [(and (number? dx) (number? dy))
			(values dx (- (pict-height base) dy))]
		       [(and (or (pict? dx) 
                                 (and (list? dx) (andmap pict? dx)))
                             (procedure? dy)
			     (procedure-arity-includes? dy 2))
			(if flip?
			    (let-values ([(dx dy) (dy base dx)])
			      (values dx (- (pict-height base) dy)))
			    (dy base dx))]
		       [else
			(error who
			       "expects two numbers or a sub-pict and a find procedure")])])
	  (use-last/unchecked (cons-picture*
                               base
                               `((place ,dx ,(- dy (pict-height target)) ,target)))
                              (last* base))))

      (define (place-over base dx dy target)
	(place-it 'place-over #f base dx dy target))
      (define (place-under base dx dy target)
	(cc-superimpose
	 (place-it 'place-under #f (ghost base) dx dy target)
	 base))

      (define (pin-over base dx dy target)
	(place-it 'pin-over #t base dx dy target))
      (define (pin-under base dx dy target)
	(cc-superimpose
	 (place-it 'pin-under #t (ghost base) dx dy target)
	 (launder base)))

      (define black-and-white
	(make-parameter #f
			(lambda (x)
			  (and x #t))))

      (define (colorize p color)
        (unless (or (string? color)
                    (is-a? color color%)
                    (and (list? color) (= 3 (length color)) (andmap byte? color)))
          (error 'colorize "expected a color, given ~e" color))
        (let ([color (if (list? color) 
                         (apply make-color color) 
                         color)])
          (if (black-and-white)
              p
              (extend-pict 
               p 0 0 0 0 0
               `(color ,color ,(pict-draw p))))))

      (define (optimize s)
	(let o-loop ([s s][dx 0][dy 0])
	  (if (string? s)
	      s
	      (let ([tag (car s)])
		(case tag
		  [(picture)
		   (list* 'picture (cadr s) (caddr s)
			  (map optimize (cdddr s)))]
		  [(color)
		   (let ([next (caddr s)])
		     (if (and (pair? next) (eq? (car next) 'color))
			 (optimize next)
			 (list* 'color (cadr s) 
				(list 'put dx dy (optimize next)))))]
		  [(thickness)
		   (let ([t (cadr s)]
			 [p (caddr s)])
		     (list 'put dx dy 
			   (list 'thickness t 
				 (optimize p))))]
		  [(put)
		   (let ([x (cadr s)]
			 [y (caddr s)]
			 [next (cadddr s)])
		     (if (and (pair? next) (eq? (car next) 'picture))
			 ; optmize put-picture to just contents ...
			 (cons 'begin (map (lambda (s) (o-loop s (+ x dx) (+ y dy))) (cdddr next)))
			 ; normal
			 (list 'put (+ x dx) (+ y dy) (optimize next))))]
		  [(qbezier)
		   (let ([x1 (list-ref s 2)]
			 [y1 (list-ref s 3)]
			 [xm (list-ref s 4)]
			 [ym (list-ref s 5)]
			 [x2 (list-ref s 6)]
			 [y2 (list-ref s 7)]
			 [p (list-ref s 1)])
		     (list 'qbezier p
			   (+ x1 dx) (+ y1 dy)
			   (+ xm dx) (+ ym dy)
			   (+ x2 dx) (+ y2 dy)))]
		  [(frame)
		   (list 'frame (optimize (cadr s)))]
		  [(colorbox)
		   (list 'colorbox (cadr s) (optimize (caddr s)))]
		  [(line vector dirline dirvector circle circle* make-box oval prog) s]
		  [else (error 'optimize "bad tag: ~s" tag)])))))

      (define (fixup-top s)
	(cond
	 [(and (pair? s) (eq? (car s) 'color))
	  ;; Drop initial put
	  (list* 'color (cadr s) (caddr (cdddr s)))]
	 [(and (pair? s) (eq? (car s) 'put))
	  ;; Wrap initial put (from thickness) in a pair of braces
	  `(local ,(cadddr s))]
	 [else
	  ;; Do nothing
	  s]))

      (define (prepare-for-output s)
	(fixup-top (optimize (pict-draw s))))

      (define (pict->command-list s)
	(let output ([s (prepare-for-output s)])
	  (if (string? s)
	      (list s)
	      (let ([tag (car s)])
		(case tag
		  [(local)
		   (output (cadr s))]
		  [(begin)
		   (apply append (map output (cdr s)))]
		  [(picture)
		   (apply append (map output (cdddr s)))]
		  [(color)
		   `((with-color ,(cadr s) ,(output (cddr s))))]
		  [(thickness)
		   `((with-thickness ,(cadr s) ,(output (caddr s))))]
		  [(put)
		   `((offset ,(cadr s) ,(caddr s) ,(output (cadddr s))))]
		  [(qbezier)
		   `((bezier ,@(cddr s)))]
		  [(line vector)
		   `((,tag ,(cadr s) ,(caddr s) ,(cadddr s)))]
		  [(circle circle*)
		   `((,tag ,(cadr s)))]
		  [(frame)
		   `((frame ,(output (cadr s))))]
		  [(colorbox)
		   `((colorbox ,(cadr s) ,(output (caddr s))))]
		  [(oval)
		   `((oval ,(caddr s) ,(cadddr s) ,(cadr s)))]
		  [(make-box)
		   `((make-box ,(cadr s) ,(caddr s) ,(cadddr s) ,(car (cddddr s))))]
		  [(prog)
		   `((prog ,(cadr s) ,(caddr s)))]
		  [else (error 'pict->commands "bad tag: ~s" tag)])))))
