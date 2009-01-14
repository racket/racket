#readerscribble/reader
(module blurbs scheme/base
  (require scribble/struct
           scribble/manual
           scribble/scheme
           scribble/decode
           (for-label scheme/gui/base))

  (provide (except-out (all-defined-out) p))

  (define (p . l)
    (decode-paragraph l))

  (define (itemstyleinfo)
    @elem{The @scheme[style] argument is reserved for future use.})

  (define (labelsimplestripped where what)
    @elem{If @litchar{&} occurs in @|where|, it is specially parsed; 
          under Windows and X, the character
 following @litchar{&} is underlined in the displayed control to
 indicate a keyboard mnemonic. (Under Mac OS X, mnemonic underlines are
 not shown.) The mnemonic is meaningless for a @|what| (as far as
 @xmethod[top-level-window<%> on-traverse-char] is concerned),
 but it is supported for consistency with other control types. A
 programmer may assign a meaning to the mnemonic (e.g., by overriding
 @method[top-level-window<%> on-traverse-char]).})

  (define (labelstripped where detail what)
    @elem{If @litchar{&} occurs in @|where|@|detail|, it 
 is specially parsed as for @scheme[button%].})

  (define (bitmapuseinfo pre what thing then detail)
   @elem{@|pre| @|what| is @|thing|, @|then| the bitmap@|detail|
 must be valid (see @xmethod[bitmap% ok?]) and not installed
 in a @scheme[bitmap-dc%] object; otherwise, @|MismatchExn|. If the
 bitmap has a mask (see @xmethod[bitmap% get-loaded-mask])
 that is the same size as the bitmap, then the mask is used for the
 label; furthermore, in contrast to the limitations of
 @xmethod[dc<%> draw-bitmap], non-monochrome label masks work
 consistently on all platforms.})

  (define-syntax bitmaplabeluse
   (syntax-rules ()
     [(_ id) @bitmapuseinfo["If" (scheme id) "a bitmap" "then" ""]]))
  (define-syntax bitmaplabelusearray
   (syntax-rules ()
     [(_ id) @bitmapuseinfo["If" (scheme id) "a list of bitmaps" "then" "s"]]))
  (define-syntax bitmaplabeluseisbm
    (syntax-rules ()
      [(_ id) @bitmapuseinfo["Since" (scheme id) "a bitmap" "" ""]]))
  
  (define bitmapiforiglabel
    @elem{The bitmap label is installed only
          if the control was originally created with a bitmap label.})

  (define (popupmenuinfo what other more)
   (make-splice
    (list*
     @p{Pops up the given @scheme[popup-menu%] object at the specified
        coordinates (in this window's coordinates), and returns after
        handling an unspecified number of events; the menu may still be
        popped up when this method returns. If a menu item is selected from
        the popup-menu, the callback for the menu item is called. (The
        eventspace for menu item's callback is the @|what|'s eventspace.)}

     @p{While the menu is popped up, its target is set to the @|other|. See
        @method[popup-menu% get-popup-target]
        for more information.}
 
     (if (equal? more "")
         null
         (list @p{@|more|})))))

  (define insertcharundos
    @elem{Multiple calls to the character-inserting method are grouped together
 for undo purposes, since this case of the method is typically used
 for handling user keystrokes. However, this undo-grouping feature
 interferes with the undo grouping performed by
 @method[editor<%> begin-edit-sequence] and
 @method[editor<%> end-edit-sequence], so the string-inserting
 method should be used instead during undoable edit sequences.})

  (define (insertscrolldetails what)
    @elem{@|what| editor's display is scrolled to show the new selection @techlink{position}.})

  (define (insertdetails what)
    @elem{If @scheme[end] is
          not @scheme['same], then the region from @scheme[start] to @scheme[end] is
          replaced with the text. @insertmovedetails[@scheme[end]]. If @scheme[scroll-ok?] is not @scheme[#f]
          @insertscrolldetails[@elem{and @scheme[start] is the same as the
          current caret @techlink{position}, then the}]})

  (define (insertmovedetails what)
    @elem{If the insertion @techlink{position} is before
or equal to the selection's start/end @techlink{position}, then the selection's
start/end @techlink{position} is incremented by @|what|.})

  (define OVD
    @elem{The result is only valid when the editor is displayed (see
          @secref["tb:miaoverview"]).})

  (define (FCAX c details)
    @elem{@|c|alling this method may force the recalculation of @techlink{location}
information@|details|, even if the editor currently has delayed refreshing (see
@method[editor<%> refresh-delayed?]).})

  (define FCA (FCAX "C" ""))
  (define FCAMW (FCAX "C" " if a maximum width is set for the editor"))
  (define (FCAME) (FCAX @elem{For @scheme[text%] objects, c} " if a maximum width is set for the editor"))
  
  (define EVD
    @elem{If the editor is not displayed and the editor has a
          maximum width, line breaks are calculated as for
          @method[text% line-start-position] (which handles specially
          the case of no display when the editor has a maximum width).})

  (define (LineToPara what)
    @elem{See also @method[text% paragraph-start-position], which
          operates on paragraphs (determined by explicit newline characters)
          instead of lines (determined by both explicit newline
          characters and automatic line-wrapping).})

  (define admindiscuss @secref["editoradministrators"])
  (define ateoldiscuss @secref["editoreol"])
  (define textdiscuss @secref["editorflattened"])
  (define clickbackdiscuss @secref["editorclickback"])
  (define stylediscuss @secref["editorstyles"])
  (define timediscuss @secref["editorcutandpastetime"])
  (define filediscuss @secref["editorfileformat"])
  (define editordatadiscuss @secref["editordata"])
  (define snipclassdiscuss @secref["editorsnipclasses"])
  (define togglediscuss @secref["styledeltatoggle"])
  (define drawcaretdiscuss @secref["drawcaretinfo"])
  (define eventspacediscuss @secref["eventspaceinfo"])
  (define lockdiscuss @secref["lockinfo"])
  (define mousekeydiscuss @secref["mouseandkey"])
  (define globaleditordatadiscuss @secref["globaleditordata"])

  (define geomdiscuss @secref["containeroverview"])

  (define mrprefsdiscuss @secref["mredprefs"])

  (define seesniporderdiscuss
    @elem{See @secref["tb:miaoverview"] for information about snip order in pasteboards.})

  (define (clipboardtypes)
    @elem{The @scheme[format] string is typically four capital letters. (Under
          Mac OS X, only four characters for @scheme[format] are ever used.) For
          example, @scheme["TEXT"] is the name of the UTF-8-encoded string format. New
          format names can be used to communicate application- and
          platform-specific data formats.})

  (define PrintNote
    (make-splice
     (list
      @p{Be sure to use the following methods to start/end drawing:}
      @itemize{@item{@method[dc<%> start-doc]}
               @item{@method[dc<%> start-page]}
               @item{@method[dc<%> end-page]}
               @item{@method[dc<%> end-doc]}}
      @p{Attempts to use a drawing method outside of an active page raises an exception.})))

  (define reference-doc '(lib "scribblings/reference/reference.scrbl"))

  (define SeeMzParam @elem{(see @secref[#:doc reference-doc "parameters"])})
  
  (define DrawSizeNote @elem{Restrictions on the magnitude of
                            drawing coordinates are described with @scheme[dc<%>].})

  (define LineNumbering @elem{Lines are numbered starting with @scheme[0].})
  (define ParagraphNumbering @elem{Paragraphs are numbered starting with @scheme[0].})

  (define (italicptyStyleNote)
    @elem{The @scheme[style] argument is provided for future extensions. Currently, @scheme[style] must be the empty list.})

  (define (HVLabelNote what)
    @elem{If @scheme[style] includes @scheme['vertical-label], then the @|what| is
          created with a label above the control; if @scheme[style] does not include
          @scheme['vertical-label] (and optionally includes @scheme['horizontal-label]), then the
          label is created to the left of the @|what|.})

  (define (DeletedStyleNote what)
    @elem{If @scheme[style] includes @scheme['deleted], then the @|what| is created as hidden,
          and it does not affect its parent's geometry; the @|what| can be made active later by calling
          @scheme[parent]'s @method[area-container<%> add-child] method.})

  (define (InStyleListNote)
    @elem{The editor's style list must contain @scheme[style], otherwise
          the style is not changed. See also @xmethod[style-list% convert].})

  (define (FontKWs) @elem{The @scheme[font] argument determines the font for the control.})
  (define (FontLabelKWs) @elem{The @scheme[font] argument determines the font for the control content, 
                               and @scheme[label-font] determines the font for the control label.})

  (define (WindowKWs) @elem{For information about the @scheme[enabled] argument, see @scheme[window<%>].})
  (define (SubareaKWs) @elem{For information about the @scheme[horiz-margin] and @scheme[vert-margin]
                                 arguments, see @scheme[subarea<%>].})
  (define (AreaContKWs) @elem{For information about the @scheme[border], @scheme[spacing], and @scheme[alignment]
                                  arguments, see @scheme[area-container<%>].})

  (define (AreaKWs) @elem{For information about the
                              @scheme[min-width], @scheme[min-height], @scheme[stretchable-width], and 
                              @scheme[stretchable-height] arguments, see @scheme[area<%>].})

  (define MismatchExn @elem{an @scheme[exn:fail:contract] exception is raised})
  
  (define AFM @elem{Adobe Font Metrics})
  
  (define (MonitorMethod what by-what method whatsit)
    @elem{@|what| can be changed
          by @|by-what|, and such changes do not go through this method; use @|method| to
          monitor @|whatsit| changes.})

  (define (MonitorCallbackX a b c d)
    (MonitorMethod a b @elem{the @|d|callback procedure (provided as an initialization argument)} c))

  (define (MonitorCallback a b c)
    (MonitorCallbackX a b c "control"))
  
  (define (Unmonitored what by-what the-what method)
    @elem{@|what| can be changed
          by @|by-what|, and such changes do not go through this method. A program
          cannot detect when @|the-what| except by polling @|method|.})
  
  (define OnInsertNote
    (MonitorMethod @elem{The content of an editor}
                   @elem{the system in response to other method calls}
                   @elem{@xmethod[text% on-insert] or @xmethod[pasteboard% on-insert]}
                   @elem{content additions}))
  
  (define OnDeleteNote
    (MonitorMethod @elem{The content of an editor}
                   @elem{the system in response to other method calls}
                   @elem{@xmethod[text% on-delete] or @xmethod[pasteboard% on-delete]}
                   @elem{content deletions}))
  
  (define OnSelectNote
    (MonitorMethod @elem{The selection in a pasteboard}
                   @elem{the system in response to other method calls}
                   @elem{@method[pasteboard% on-select]}
                   @elem{selection}))

  (define OnMoveNote
    (MonitorMethod @elem{Snip @techlink{location}s in a pasteboard}
                   @elem{the system in response to other method calls}
                   @elem{@method[pasteboard% on-move-to]}
                   @elem{snip @techlink{position}}))

  (define (colorName name name2 r g b)
    (make-element #f
                  (list (make-element `(bg-color ,r ,g ,b)
                                      (list (hspace 5)))
                        (hspace 1)
                        (bytes->string/latin-1 name))))
  
  (define (res-sym s)
    (string->symbol (string-append "MrEd:" s)))

  (define (Resource s)
    @elem{@to-element[`(quote ,(res-sym s))]
          preference})
  (define (ResourceFirst s) ; fixme -- add index
    (let ([r (Resource s)])
      (index* (list (format "~a preference" (res-sym s)))
              (list r) 
              r)))

  (define (edsnipsize a b c)
    @elem{An @scheme[editor-snip%] normally stretches to wrap around the size
          of the editor it contains. This method @|a| of the snip
          (and if the editor is @|b|, @|c|).})
  (define (edsnipmax n)
    (edsnipsize @elem{limits the @|n|}
                @elem{larger}
                @elem{only part of the editor is displayed}))
  (define (edsnipmin a b)
    (edsnipsize @elem{sets the minimum @|a|}
                "smaller"
                @elem{the editor is @|b|-aligned in the snip}))

  (define (boxisfill which what)
    @elem{The @|which| box is filled with @|what|.})
  (define (boxisfillnull which what)
    @elem{The @|which| box is filled with @|what|, unless @|which| is @scheme[#f].})

  (define (slant . s)
    (make-element "slant" (decode-content s)))

  )

