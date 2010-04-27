(module mrgroupbox mzscheme
  (require mzlib/class
	   mzlib/class100
	   (prefix wx: "kernel.ss")
	   "lock.ss"
	   "const.ss"
	   "check.ss"
	   "helper.ss"
	   "wx.ss"
	   "wxgroupbox.ss"
	   "mrcontainer.ss"
	   "mritem.ss")

  (provide (protect group-box%))

  ;; Not exported from MrEd:
  (define group-box%
    (class100 basic-control% (label parent [style null] [font #f])
      (override
	[hidden-child? (lambda () #t)])
      (sequence
	(let ([cwho '(constructor group-box)])
	  (check-label-string cwho label)
	  (check-container-parent cwho parent)
	  (check-style cwho #f '(deleted) style))
	(super-init (lambda () (make-object wx-group-box% this this
					    style
					    (mred->wx-container parent)
					    label
					    style
					    (no-val->#f font)))
		    (lambda ()
		      (let ([cwho '(constructor group-box)])
			(check-container-ready cwho parent)))
		    label parent void #f)))))
