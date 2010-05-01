(require mzlib/list)

(define png-suite (build-path (or (current-load-relative-directory)
				  (current-directory))
			      "png-suite"))

(unless (directory-exists? png-suite)
  (error 'png-test
	 (string-append
	  "The png-suite subdirectory appears to be missing. "
	  "It should contain the PNG test files (including GIFs for comparisons).")))

(define l (map (lambda (f) (build-path png-suite f))
	       (sort (filter (lambda (x) (regexp-match #rx"^[^x].*[.]png$" x))
                             (directory-list png-suite))
                     string<?)))

(define (png->gif f)
  (regexp-replace #rx"[.]png$" f ".gif"))

(define f (make-object frame% "Tester"))
(define name (new message% 
		  [label (car l)]
		  [parent f]
		  [stretchable-width #t]))
(define no-mask-bm (let* ([bm (make-object bitmap% 32 32 1)]
			  [dc (make-object bitmap-dc% bm)])
		     (send dc clear)
		     (send dc draw-line 0 0 32 32)
		     (send dc draw-line 0 32 32 0)
		     (send dc set-bitmap #f)
		     bm))

(define last-bm (make-object bitmap% (car l)))

(define ppng (make-object horizontal-panel% f))
(define png (new message%
		 [label last-bm]
		 [parent ppng]
		 [stretchable-width #t]
		 [stretchable-height #t]))
(define pngm (new message%
		 [label no-mask-bm]
		 [parent ppng]
		 [stretchable-width #t]
		 [stretchable-height #t]))
(define png-canvas (new canvas%
			[parent ppng]
			[stretchable-width #t]
			[stretchable-height #t]
			[paint-callback (lambda (c dc)
					  (send dc set-brush 
						(send the-brush-list find-or-create-brush "GREEN" 'solid))
					  (send dc draw-rectangle -1 -1 500 500)
					  (send dc draw-bitmap
						last-bm 0 0
						'solid 
						(send the-color-database find-color "BLACK")
						(send last-bm get-loaded-mask)))]))
(define ppng-mono (make-object vertical-panel% ppng))
(define mono? (new message%
		   [label "mono"]
		   [parent ppng-mono]))
(define mono-mask? (new message%
			[label "mono mask"]
			[parent ppng-mono]))
(unless (= 1 (send last-bm get-depth))
  (send mono? show #f))
(unless (and (send last-bm get-loaded-mask)
	     (= 1 (send (send last-bm get-loaded-mask) get-depth)))
  (send mono-mask? show #f))

(define gif (new message%
		 [label (make-object bitmap% (png->gif (car l)))]
		 [parent f]
		 [stretchable-width #t]
		 [stretchable-height #t]))

(define pld (make-object group-box-panel% "Save and Reload" f))
(new button%
     [label "Go"]
     [parent pld]
     [callback (lambda (b e)
		 (if (send last-bm save-file "tmp.png" 'png)
		     (let ([bm (make-object bitmap% "tmp.png" (get-mask-mode) (get-bg-color))])
		       (send ld-png set-label (if (send bm ok?)
						  bm
						  no-mask-bm))
		       (send ld-pngm set-label (or (send bm get-loaded-mask)
						   no-mask-bm))
		       (send ld-mono? show (and (send bm ok?)
						(= 1 (send bm get-depth))))
		       (send ld-mono-mask? show (and (send bm get-loaded-mask)
						     (= 1 (send (send bm get-loaded-mask) get-depth)))))
		     (error "write failed!")))])
(define ppld (make-object horizontal-panel% pld))
(define ld-png (new message%
		    [label no-mask-bm]
		    [parent ppld]
		    [stretchable-width #t]
		    [stretchable-height #t]))
(define ld-pngm (new message%
		     [label no-mask-bm]
		     [parent ppld]
		     [stretchable-width #t]
		     [stretchable-height #t]))
(define ppld-mono (make-object vertical-panel% ppld))
(define ld-mono? (new message%
		      [label "mono"]
		      [parent ppld-mono]))
(define ld-mono-mask? (new message%
			   [label "mono mask"]
			   [parent ppld-mono]))
(send ld-mono? show #f)
(send ld-mono-mask? show #f)

(define mask (new choice%
		  [label "Alpha"]
		  [choices '("Auto" "Mask")]
		  [parent f]
		  [callback (lambda (c e) (refresh))]))
(define bg (new choice%
		[label "Background"]
		[choices '("Default" "White" "Black" "Red")]
		[parent f]
		[callback (lambda (c e) (refresh))]))

(define slider
  (new slider% 
       [label #f]
       [parent f]
       [min-value 1]
       [max-value (length l)]
       [init-value 1]
       [callback (lambda (s e) (refresh))]))
(let ([p (make-object horizontal-panel% f)])
  (make-object button% "Prev" p (lambda (b e) 
				  (send slider set-value (max 1 (sub1 (send slider get-value))))
				  (refresh)))
  (make-object vertical-pane% p)
  (make-object button% "Next" p (lambda (b e)
				  (send slider set-value (min (length l) (add1 (send slider get-value))))
				  (refresh))))

(define (refresh)
  (let ([n (list-ref l (sub1 (send slider get-value)))])
    (send name set-label n)
    (let ([bm (make-object bitmap% n (get-mask-mode) (get-bg-color))])
      (set! last-bm bm)
      (send png set-label bm)
      (send pngm set-label (or (send bm get-loaded-mask)
			       no-mask-bm)))
    (send gif set-label (make-object bitmap% (png->gif n)))
    (send mono? show (= 1 (send last-bm get-depth)))
    (send mono-mask? show (and (send last-bm get-loaded-mask)
			       (= 1 (send (send last-bm get-loaded-mask) get-depth))))
    (send png-canvas refresh)))

(define (get-mask-mode)
  (case (send mask get-selection)
    [(0) 'unknown]
    [(1) 'unknown/mask]))

(define get-bg-color
  (let ([white (make-object color% 255 255 255)]
	[black (make-object color% 0 0 0)]
	[red (make-object color% 255 0 0)])
    (lambda ()
      (case (send bg get-selection)
	[(0) #f]
	[(1) white]
	[(2) black]
	[(3) red]))))

(send f show #t)
