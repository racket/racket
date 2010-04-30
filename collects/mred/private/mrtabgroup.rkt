(module mrtabgroup mzscheme
  (require mzlib/class
	   mzlib/class100
	   (prefix wx: "kernel.ss")
	   "lock.ss"
	   "const.ss"
	   "check.ss"
	   "helper.ss"
	   "wx.ss"
	   "wxtabgroup.ss"
	   "mritem.ss")

  (provide (protect tab-group%))

  ;; Not exported from MrEd:
  (define tab-group%
    (class100 basic-control% (label choices parent callback [style null] [font #f])
      (override
	[hidden-child? (lambda () #t)])
      (sequence
	(let ([cwho '(constructor tab-group)])
	  (check-list-control-args cwho label choices parent callback)
	  (check-style cwho #f '(deleted border) style))
	(super-init (lambda () (make-object (if (and (eq? 'macosx (system-type))
						     (not (memq 'border style)))
						canvas-based-tab-group%
						wx-tab-group%)
					    this this
					    style
					    (mred->wx-container parent)
					    (wrap-callback callback)
					    label
					    choices
					    style
					    (no-val->#f font)))
		    (lambda ()
		      (let ([cwho '(constructor tab-group)])
			(check-container-ready cwho parent)))
		    label parent callback #f)))))
