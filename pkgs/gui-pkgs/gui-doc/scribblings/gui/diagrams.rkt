(module diagrams scheme/base
  (require scribble/struct
           scribble/scheme
           scribble/manual
           (for-label scheme/gui/base))

  (provide diagram->table
           short-windowing-diagram
           windowing-diagram
           event-diagram
           menu-diagram
           editor-diagram
           snip-diagram
           editor-snip-diagram
           style-diagram
           snip-list-diagram
           editor-admin-diagram
           stream-diagram)

  (define (diagram->table d)
    (make-table
     #f
     (map (lambda (line)
            (list (make-flow 
                   (list (make-paragraph 
                          (let loop ([line line])
                            (cond
                             [(regexp-match #rx"(.*)( +)(.*)" line)
                              => (lambda (m)
                                   (append (loop (cadr m))
                                           (list (hspace (string-length (caddr m))))
                                           (loop (cadddr m))))]
                             [(regexp-match #rx"([^-a-zA-Z0-9]*)([-a-zA-Z0-9<%>]+)(.*)" line)
                              => (lambda (m)
                                   (append (loop (cadr m))
                                           (list (to-element (make-just-context (string->symbol (caddr m))
                                                                                #'here)))
                                           (loop (cadddr m))))]
                             [else (list (make-element 'tt (list line)))])))))))
          (regexp-split #rx"[\r\n]+" d))))

  (define short-windowing-diagram
#<<DIAG
                           area<%>
       ______________________|_______________
       |                  |                 |
  subarea<%>          window<%>      area-container<%>      
       |____       _______|__________       |
            |      |                |       |
           subwindow<%>          area-container-window<%>
        ________|________                |
        |               |                |
     control<%>       canvas<%>   top-level-window<%>
DIAG
)

  (define windowing-diagram
#<<DIAG
                           area<%>
        _____________________|_______________
        |               |                   |
      subarea<%>     window<%>       area-container<%>      
<<<____|____       _____|__________       __|___  ___________________<<<
            |      |              |       |    |  |                  
           subwindow<%>           |       |    |  |                  
<<<______________|___________     |       |    |  |                 _<<<
            |               |     |       |    pane%                |
       control<%>           |     |       |     |- horizontal-pane% |
        |- message%         |     |       |     |- vertical-pane%   |
        |- button%          |     |       |                         |
        |- check-box%       |  area-container-window<%>             |
        |- slider%          |        |                              |
        |- gauge%           |        |            __________________|
        |- text-field%      |        |            |   
            |- combo-field% |        |-------- panel%       
        |- radio-box%       |        |          |- horizontal-panel%
        |- list-control<%>  |        |          |- vertical-panel%
            |- choice%      |        |              |- tab-panel%
            |- list-box%    |        |              |- group-box-panel%
                            |        |
                            |        |- top-level-window<%>
                            |            |- frame% 
                         canvas<%>       |- dialog%
                          |- canvas%
                          |- editor-canvas%
DIAG
)

 
  (define event-diagram
#<<DIAG
       event%                                 timer%
        |- key-event%                         cursor%
        |- mouse-event%                    
        |- scroll-event%                      clipboard<%>
        |- control-event%                     clipboard-client%
DIAG
)

  (define menu-diagram
#<<DIAG
    menu-item<%>                menu-item-container<%> 
        |                              | 
        |- separator-menu-item%   _____|___ 
        |- labelled-menu-item<%>  |       |- menu-bar% 
            _________|_________   |       |- popup-menu% 
            |                 |   | 
            |                 menu%
            |                          
            |- selectable-menu-item<%>               
                |- menu-item%                        
                |- checkable-menu-item%
DIAG
)

  (define editor-diagram
#<<DIAG
  editor<%>
   |- text%                   
   |- pasteboard%
DIAG
)

  (define snip-diagram
#<<DIAG
  snip%                   readable-snip<%>
   |- string-snip%
   |   |- tab-snip%
   |- image-snip%
   |- editor-snip% (not provided by racket/snip)

  snip-admin%
DIAG
)

  (define editor-snip-diagram
#<<DIAG
  snip%
   |- editor-snip%
DIAG
)

 
  (define editor-admin-diagram
#<<DIAG
  editor-canvas%
 
  editor-admin%                     snip-admin%
   |- editor-snip-editor-admin<%>
 
  editor-wordbreak-map%    keymap%
DIAG
)
 
  (define style-diagram
#<<DIAG
  style<%>         style-delta%       add-color<%>
  style-list%                         mult-color<%>
DIAG
)

  (define snip-list-diagram
#<<DIAG
  snip-class%
  snip-class-list<%>
DIAG
)

  (define stream-diagram
#<<DIAG
 editor-data%
 editor-data-class%
 editor-data-class-list<%>
 
 editor-stream-in%                 editor-stream-out%
 editor-stream-in-base%            editor-stream-out-base%
  |- editor-stream-in-bytes-base%   |- editor-stream-out-bytes-base%
DIAG
))
