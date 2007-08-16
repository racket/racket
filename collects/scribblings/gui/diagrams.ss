(module diagrams mzscheme
  (require (lib "string.ss")
           (lib "struct.ss" "scribble")
           (lib "scheme.ss" "scribble")
           (lib "manual.ss" "scribble"))

  (provide diagram->table
           short-windowing-diagram
           windowing-diagram
           event-diagram
           menu-diagram)

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
                                           (list (to-element (string->symbol (caddr m))))
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
       event%                                        timer%
        |- key-event%                                cursor%
        |- mouse-event%                    
        |- scroll-event%                             clipboard<%>
        |- control-event%                            clipboard-client%
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
))
