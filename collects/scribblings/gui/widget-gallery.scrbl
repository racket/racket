#lang scribble/doc
@(require "common.rkt"
          "widget-gallery.rkt")

@title[#:style '(toc quiet)]{Widget Gallery}

@; Note: Instructions for taking widget screenshots are avaiable at:
@;   collects/meta/images/taking-screenshots/instructions

This section shows the main widgets available in the Racket Graphical User
Interface Toolkit. Each image is a link to the documentation of the relevant
widget.

@(showcase-widget button%
(define button (new button%
                    (parent panel)
                    (label "Button")))
)

@(showcase-widget check-box%
(define check-box (new check-box%
                       (parent panel)
                       (label "Check Box")
                       (value #t)))
)

@(showcase-widget choice%
(define choice (new choice%
                    (label "Choice")
                    (parent panel)
                    (choices (list "Item 0"))))
)

@(showcase-widget combo-field%
(define combo-field (new combo-field%
                         (label "Combo")
                         (parent panel)
                         (choices (list "Field"))
                         (init-value "Field")))
)

@(showcase-widget editor-canvas%
(define editor-canvas (new editor-canvas%
                           (parent panel)
                           (label "Editor Canvas")))
(define text (new text%))
(send text insert "Editor Canvas")
(send editor-canvas set-editor text)
)


@(showcase-widget gauge%
(define gauge (new gauge%
                   (label "Gauge")
                   (parent panel)
                   (range 100)))
(send gauge set-value 42)
)

@(showcase-widget group-box-panel%
(define group-box-panel (new group-box-panel%
                             (parent panel)
                             (label "Group Box Panel")))
)

@(showcase-widget list-box%
(define list-box (new list-box%
                      (label "List Box")
                      (parent (new horizontal-panel%
                                   (parent panel)
                                   (style (list 'border))))
                      (choices (list "Item 0"
                                     "Item 1"
                                     "Item 2"))
                      (style (list 'single
                                   'column-headers))
                      (columns (list "First Column"))))
)

@(showcase-widget menu-bar%
(define menu-bar (new menu-bar%
                      (parent frame)))
(new menu%
     (label "&File")
     (parent menu-bar))
(new menu%
     (label "&Edit")
     (parent menu-bar))
(new menu%
     (label "&Help")
     (parent menu-bar))
)

@(showcase-widget message%
(define message (new message%
                     (parent panel)
                     (label "Message")))
)

@(showcase-widget panel%
(define a-panel (new panel%
                     (parent panel)
                     (style (list 'border))))
(new message%
     (parent a-panel)
     (label "Panel"))
)


@(showcase-widget radio-box%
(define radio-box (new radio-box%
                       (label "Radio Box")
                       (parent panel)
                       (choices (list "Button 0"
                                      "Button 1"
                                      "Button 2"))))
)

@(showcase-widget slider%
(define slider (new slider%
                    (label "Slider")
                    (parent panel)
                    (min-value 0)
                    (max-value 100)
                    (init-value 42)))
)

@(showcase-widget tab-panel%
(define tab-panel (new tab-panel%
                       (parent panel)
                       (choices (list "Tab 0"
                                      "Tab 1"
                                      "Tab 2"))))
)

@(showcase-widget text-field%
(define text-field (new text-field%
                        (label "Text")
                        (parent panel)
                        (init-value "Field")))
)
