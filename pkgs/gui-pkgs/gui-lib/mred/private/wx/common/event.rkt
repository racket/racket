#lang racket/base
(require racket/class
         "../../syntax.rkt")

(provide event%
         mouse-event%
         key-event%
         control-event%
         column-control-event%
         scroll-event%
         popup-event%)

(defclass event% object%
  (init-properties [[exact-integer? time-stamp] 0])
  (super-new))

(defclass mouse-event% event%
  (init-properties [[(symbol-in enter leave left-down left-up
                                middle-down middle-up
                                right-down right-up motion)
                     event-type]]
                   [[bool? left-down] #f]
                   [[bool? middle-down] #f]
                   [[bool? right-down] #f]
                   [[exact-integer? x] 0]
                   [[exact-integer? y] 0]
                   [[bool? shift-down] #f]
                   [[bool? control-down] #f]
                   [[bool? meta-down] #f]
                   [[bool? alt-down] #f])
  (init [time-stamp 0])
  (init-properties [[bool? caps-down] #f])
  (super-new [time-stamp time-stamp])

  (def/public (button-changed? [(symbol-in left middle right any) [button 'any]])
    (and (memq event-type
               (case button
                 [(any) '(left-down left-up middle-down middle-up right-down right-up)]
                 [(left) '(left-down left-up)]
                 [(middle) '(middle-down middle-up)]
                 [(right) '(right-down right-up)]))
         #t))

  (def/public (button-down? [(symbol-in left middle right any) [button 'any]])
    (and (memq event-type
               (case button
                 [(any) '(left-down middle-down right-down)]
                 [(left) '(left-down)]
                 [(middle) '(middle-down)]
                 [(right) '(right-down)]))
         #t))

  (def/public (button-up? [(symbol-in left middle right any) [button 'any]])
    (and (memq event-type
               (case button
                 [(any) '(left-up middle-up right-up)]
                 [(left) '(left-up)]
                 [(middle) '(middle-up)]
                 [(right) '(right-up)]))
         #t))

  (def/public (dragging?)
    (and (eq? event-type 'motion)
         (or left-down middle-down right-down)))

  (def/public (entering?)
    (eq? event-type 'enter))

  (def/public (leaving?)
    (eq? event-type 'leave))

  (def/public (moving?)
    (eq? event-type 'motion)))

(defclass key-event% event%
  (init-properties [[(make-alts symbol? char?) key-code] #\nul]
                   [[bool? shift-down] #f]
                   [[bool? control-down] #f]
                   [[bool? meta-down] #f]
                   [[bool? alt-down] #f]
                   [[exact-integer? x] 0]
                   [[exact-integer? y] 0])
  (init [time-stamp 0])
  (init-properties [[bool? caps-down] #f])
  (properties [[(make-alts symbol? char?) key-release-code] 'press]
              [[(make-or-false (make-alts symbol? char?)) other-shift-key-code] #f]
              [[(make-or-false (make-alts symbol? char?)) other-altgr-key-code] #f]
              [[(make-or-false (make-alts symbol? char?)) other-shift-altgr-key-code] #f]
              [[(make-or-false (make-alts symbol? char?)) other-caps-key-code] #f])
  (super-new [time-stamp time-stamp]))

(defclass control-event% event%
  (init-properties [[(symbol-in button check-box choice
                                list-box list-box-dclick list-box-column text-field
                                text-field-enter slider radio-box
                                menu-popdown menu-popdown-none tab-panel) 
                     event-type]])
  (init [time-stamp 0])
  (super-new [time-stamp time-stamp]))

(defclass column-control-event% control-event%
  (init-properties [[exact-nonnegative-integer? column]])
  (init event-type
        [time-stamp 0])
  (unless (eq? event-type 'list-box-column)
    (raise-type-error (init-name 'column-control-event%)
                      "'list-box-column"
                      event-type))
  (super-new [event-type event-type]
             [time-stamp time-stamp]))

(defclass popup-event% control-event%
  (properties [[any? menu-id] 0])
  (super-new))

(defclass scroll-event% event%
  (init-properties [[(symbol-in top bottom line-up line-down page-up page-down thumb) event-type]
                    'thumb]
                   [[(symbol-in horizontal vertical) direction] 'vertical]
                   [[(integer-in 0 10000) position] 0])
  (init [time-stamp 0])
  (super-new [time-stamp time-stamp]))

