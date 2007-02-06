
@INCLUDE prefix.xci

#include "wx_menu.h"

@HEADER

@INCLUDE wxs.xci

#ifdef wx_mac
# define MAC_UNUSED(x) /**/
#else
# define MAC_UNUSED(x) x
#endif
#ifdef wx_xt
# define WINMAC_UNUSED(x) x
#else
# define WINMAC_UNUSED(x) /**/
#endif

static void menuSelect(wxMenu *MAC_UNUSED(m), wxMenuBar *WINMAC_UNUSED(mb))
{
#ifdef wx_msw
  m->SelectMenu();
#endif
#ifdef wx_xt
  mb->SelectAMenu(m);
#endif
}

// @CLASSBASE wxMenuItem "menu-item" : "object"
// @END

// wxMenu is really derived from wxItem
//  but that makes no sense. Enforce different hierarchy here
@CLASSBASE wxMenu "menu" : "object"

@SET CALLBACK_CLASS = wxMenu
@SET CALLBACK_CLASS_USER = METHODNAME("menu%","initialization")
@INCLUDE cb_start.xci

@MACRO CHECKNEG[pos.result] = if (x<pos> < 0) { READY_TO_RETURN; return <result>; }
@MACRO CHECKNEGVOID[pos] = $$CHECKNEG[<pos>.scheme_void]
@MACRO CHECKNEGNULL[pos] = $$CHECKNEG[<pos>.XC_SCHEME_NULL]
@MACRO CHECKNEGFALSE[pos] = $$CHECKNEG[<pos>.scheme_false]

@CREATOR (nstring=NULL,wxFunction=NULL/bCallback/ubCallback/cCallback//spCallback/nopush,wxFont^=NULL); : : ubCallbackSetup///ubCallbackCreatorFinish

@ "append" : void Append(ExactLong,string,wxMenu!,nstring=NULL); : : <> submenu
@ "append" : void Append(ExactLong,string,nstring=NULL,bool=FALSE); : : <> string item
@ "delete" : bool Delete(ExactLong); : :
@ "delete-by-position" : bool DeleteByPosition(int); : :
@ "append-separator" : void AppendSeparator();
@ "checked?" : bool Checked(ExactLong); : :
@ "check" : void Check(ExactLong,bool); : :
@ "enable" : void Enable(ExactLong,bool); : :
@ "number" : int Number()

@ "set-help-string" : void SetHelpString(ExactLong, nstring); : :
@ "set-label" : void SetLabel(ExactLong, string); : :
@ "set-title" : void SetTitle(string);

@ "set-width" : void SetWidth(int);

@ "get-font" : wxFont! GetFont();

@ m "select" : void menuSelect(wxMenuBar^);

@END

@INCLUDE cb_end.xci

// wxMenuBar is really derived from wxItem
//  but that makes no sense. Enforce different hierarchy here
@CLASSBASE wxMenuBar "menu-bar" : "object"

@MACRO spMenuList = (listof wxMenu-object)

@CREATOR (); <> no argument

@ "append" : void Append(wxMenu!,string);
@ "delete" : bool Delete(wxMenu^,int=0);
@ "enable-top" : void EnableTop(int,bool); : : /CHECKNEGVOID[0]
@ "number" : int Number()

@ "set-label-top" : void SetLabelTop(int, string); : : /CHECKNEGVOID[0]

@END

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

class wxsMenuItem : public wxObject
{
#ifdef MZ_PRECISE_GC
  void *my_id;
#endif
public:
  wxsMenuItem(void);
  ~wxsMenuItem();

  ExactLong Id(void) {
#ifdef MZ_PRECISE_GC
    return (ExactLong)my_id;
#else
    return (ExactLong)this;
#endif
  }
};

wxsMenuItem::wxsMenuItem(void)
#ifndef MZ_PRECISE_GC
: wxObject(WXGC_NO_CLEANUP)
#endif
{
#ifdef MZ_PRECISE_GC
  void *mid;
  mid = GC_malloc_immobile_box(GC_malloc_weak_box(gcOBJ_TO_PTR(this), NULL, 0));
  my_id = mid;
#endif
}

wxsMenuItem::~wxsMenuItem()
{
#ifdef MZ_PRECISE_GC
  GC_free_immobile_box((void **)my_id);
#endif
}

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

wxsMenuItem* wxsIdToMenuItem(ExactLong id)
{
#ifdef MZ_PRECISE_GC
  if (!id)
    return NULL;
  else
    return (wxsMenuItem *)gcPTR_TO_OBJ(GC_weak_box_val(*(void **)id));
#else
  return (wxsMenuItem *)id;
#endif
}

@CLASSBASE wxsMenuItem "menu-item" : "object"

@CREATOR ()

@ "id" : ExactLong Id();

@END


@GLOBAL wxsMenuItemGlobal

@ "id-to-menu-item" : wxsMenuItem! wxsIdToMenuItem(ExactLong);

@END
