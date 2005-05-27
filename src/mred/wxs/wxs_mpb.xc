
@INCLUDE prefix.xci

#include "wx_media.h"

@INCLUDE wxs.xci

@HEADER

@INCLUDE wxs_eds.xci
@INCLUDE wxs_bmt.xci

@CLASSBASE wxMediaPasteboard "pasteboard" : "editor" / nofnl

@CREATOR ()

@CLASSID wxTYPE_MEDIA_PASTEBOARD

@SETMARK X = 
@SETMARK Y = d
@SETMARK Z = d
@INCLUDE wxs_mbuf.xci

@ "insert" : void Insert(wxSnip!,double,double); <> snip% with location
@ "insert" : void Insert(wxSnip!,wxSnip^); <> snip% with before-snip%
@ "insert" : void Insert(wxSnip!,wxSnip^,double,double); <> snip% with before-snip% and location

@ "delete" : void Delete(); <> no argument
@ "delete" : void Delete(wxSnip!); <> snip%

@ v "do-copy" : void DoCopy(ExactLong,bool);
@ v "do-paste" : void DoPaste(ExactLong);
@ v "do-paste-x-selection" : void DoPasteSelection(ExactLong);

@ "erase" : void Erase();

@ "remove" : void Remove(wxSnip!);

@ "move-to" : void MoveTo(wxSnip!,double,double);
@ "move" : void Move(wxSnip!,double,double); <> snip%
@ "move" : void Move(double,double); <> without snip%

@ "resize" : bool Resize(wxSnip!, nndouble, nndouble);

@ "raise" : void Raise(wxSnip!);
@ "lower" : void Lower(wxSnip!);
@ "set-before" : void SetBefore(wxSnip!,wxSnip^);
@ "set-after" : void SetAfter(wxSnip!,wxSnip^);
  
@ "change-style" : void ChangeStyle(wxStyleDelta^,wxSnip^); <> style-delta% and snip%
@ "change-style" : void ChangeStyle(wxStyle^,wxSnip^=NULL); <> style%

@ "set-selected" : void SetSelected(wxSnip!);
@ "add-selected" : void AddSelected(wxSnip!); <> snip%
@ "add-selected" : void AddSelected(double,double,nndouble,nndouble); <> rectangle
@ "no-selected" :  void NoSelected();
@ "remove-selected" :  void RemoveSelected(wxSnip!);

@ "get-center" : void GetCenter(double*, double*);

@ "find-snip" : wxSnip^ FindSnip(double,double,wxSnip^=NULL);
@ "is-selected?" : bool IsSelected(wxSnip^);
@ "find-next-selected-snip" : wxSnip^ FindNextSelectedSnip(wxSnip^);

@ v "can-insert?" : bool CanInsert(wxSnip!,wxSnip^,double, double);
@ v "on-insert" : void OnInsert(wxSnip!,wxSnip^,double, double);
@ v "after-insert" : void AfterInsert(wxSnip!,wxSnip^,double,double);
@ v "can-delete?" : bool CanDelete(wxSnip!);
@ v "on-delete" : void OnDelete(wxSnip!);
@ v "after-delete" :  void AfterDelete(wxSnip!);
@ v "can-move-to?" :  bool CanMoveTo(wxSnip!,double,double,bool);
@ v "on-move-to" :  void OnMoveTo(wxSnip!,double,double,bool);
@ v "after-move-to" :  void AfterMoveTo(wxSnip!,double,double,bool);
@ v "can-resize?" :  bool CanResize(wxSnip!,nndouble,nndouble);
@ v "on-resize" :  void OnResize(wxSnip!,nndouble,nndouble);
@ v "after-resize" :  void AfterResize(wxSnip!,nndouble,nndouble,bool);
@ v "can-reorder?" :  bool CanReorder(wxSnip!,wxSnip!,bool);
@ v "on-reorder" :  void OnReorder(wxSnip!,wxSnip!,bool);
@ v "after-reorder" :  void AfterReorder(wxSnip!,wxSnip!,bool);

@ v "can-select?" : bool CanSelect(wxSnip!, bool);
@ v "on-select" : void OnSelect(wxSnip!, bool);
@ v "after-select" :  void AfterSelect(wxSnip!, bool);

@ v "on-double-click" : void OnDoubleClick(wxSnip!, wxMouseEvent!);

@ v "interactive-adjust-mouse" : void InteractiveAdjustMouse(double*,double*);
@ v "interactive-adjust-move" : void InteractiveAdjustMove(wxSnip!,double*,double*);
@ v "interactive-adjust-resize" : void InteractiveAdjustResize(wxSnip!,nndouble*,nndouble*);

@ v "can-interactive-move?" : bool CanInteractiveMove(wxMouseEvent!);
@ v "on-interactive-move" : void OnInteractiveMove(wxMouseEvent!);
@ v "after-interactive-move" : void AfterInteractiveMove(wxMouseEvent!);
@ v "can-interactive-resize?" : bool CanInteractiveResize(wxSnip!);
@ v "on-interactive-resize" : void OnInteractiveResize(wxSnip!);
@ v "after-interactive-resize" : void AfterInteractiveResize(wxSnip!);

@ "get-dragable" : bool GetDragable();
@ "set-dragable" : void SetDragable(bool);
@ "get-selection-visible" : bool GetSelectionVisible();
@ "set-selection-visible" : void SetSelectionVisible(bool);

@ "get-scroll-step" : double GetScrollStep();
@ "set-scroll-step" : void SetScrollStep(nndouble);

@END
