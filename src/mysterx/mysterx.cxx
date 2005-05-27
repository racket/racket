// mysterx.cxx : COM/ActiveX/DHTML extension for PLT Scheme
// Author: Paul Steckler

#include "stdafx.h"

#include <stdio.h>
#include <malloc.h>
#include <float.h>
#include <limits.h>
#include <io.h>
#include <process.h>

#define _WIN32_DCOM

#include <objbase.h>
#include <mshtml.h>
#include <initguid.h>
#include <winnls.h>
#include <exdisp.h>
#include <shellapi.h>
#include <htmlhelp.h>

#include "resource.h"

#include "escheme.h"
#include "schvers.h"

#include "bstr.h"

// ATL support

#include <atlbase.h>
extern CComModule _Module;
#include <atlcom.h>
#include <atlhost.h>
CComModule _Module;

// end ATL support

#include "myspage.h"
#include "myspage_i.c"
#include "myssink.h"
#include "myssink_i.c"

#include "mysterx.h"

static WNDPROC AtlWndProc;

HINSTANCE hInstance;
HICON hIcon;
HANDLE browserHwndMutex;
HANDLE createHwndSem;
HANDLE eventSinkMutex;

const CLSID emptyClsId;

static Scheme_Object *mx_omit_obj; /* omitted argument placeholder */
static Scheme_Object *mx_name;     /* module name */

/* Normally false, but when true, mysterx will marshal any scheme */
/* object it would otherwise fail to marshal by simply sticking   */
/* the 32-bit value in an UNSIGNED I4 variant and hoping for the  */
/* best.  Obviously this has GC implications, so don't use it.    */
/* jrm uses it for dotnet.                                        */

Scheme_Object * mx_marshal_raw_scheme_objects;

Scheme_Object *scheme_date_type;

static MX_TYPE_TBL_ENTRY *typeTable[TYPE_TBL_SIZE];

MYSSINK_TABLE myssink_table;

static char *objectAttributes[] = { "InprocServer", "InprocServer32",
				    "LocalServer", "LocalServer32", NULL };
static char *controlAttributes[] = { "Control", NULL };

static MX_PRIM mxPrims[] = {

  // version

  { mx_version,"mx-version",0,0},

  // COM reflection

  { mx_com_invoke,"com-invoke",2,-1},
  { mx_com_set_property,"com-set-property!",2,-1 },
  { mx_com_get_property,"com-get-property",2, -1 },
  { mx_com_methods,"com-methods",1,1 },
  { mx_com_get_properties,"com-get-properties",1,1 },
  { mx_com_set_properties,"com-set-properties", 1, 1 },
  { mx_com_events,"com-events",1,1 },
  { mx_com_method_type,"com-method-type",2,2 },
  { mx_com_get_property_type,"com-get-property-type",2,2 },
  { mx_com_set_property_type,"com-set-property-type",2,2 },
  { mx_com_event_type,"com-event-type",2,2 },
  { mx_com_help,"com-help",1,2 },

  // COM types

  { mx_com_get_object_type,"com-object-type",1,1 },
  { mx_com_is_a,"com-is-a?",2,2 },
  { mx_cy_pred_ex,"com-currency?",1,1 },
  { mx_date_pred_ex,"com-date?",1,1 },
  { mx_date_to_scheme_date,"com-date->date",1,1 },
  { scheme_date_to_mx_date,"date->com-date",1,1 },
  { mx_scode_pred_ex,"com-scode?",1,1 },
  { mx_scode_to_scheme_number,"com-scode->number",1,1 },
  { scheme_number_to_mx_scode,"number->com-scode",1,1 },
  { mx_currency_to_scheme_number,"com-currency->number",1,1 },
  { scheme_number_to_mx_currency,"number->com-currency",1,1 },
  { mx_comobj_pred_ex,"com-object?",1,1 },
  { mx_iunknown_pred_ex,"com-iunknown?",1,1 },

  // COM events

  { mx_com_register_event_handler,"com-register-event-handler",3,3 },
  { mx_com_unregister_event_handler,"com-unregister-event-handler",2,2 },

  // coclasses

  { mx_all_coclasses,"com-all-coclasses",0,0 },
  { mx_all_controls,"com-all-controls",0,0 },
  { mx_coclass_to_html,"coclass->html",3,4 },
  { mx_progid_to_html,"progid->html",3,4 },

  // COM objects

  { mx_cocreate_instance_from_coclass,"cocreate-instance-from-coclass",1,2 },
  { mx_cocreate_instance_from_progid,"cocreate-instance-from-progid",1,2 },
  { mx_coclass,"coclass",1,1 },
  { mx_progid,"progid",1,1 },
  { mx_set_coclass,"set-coclass!",2,2 },
  { mx_set_coclass_from_progid,"set-coclass-from-progid!",2,2 },
  { mx_com_object_eq,"com-object-eq?",2,2 },
  { mx_com_register_object,"com-register-object",1,1 },
  { mx_com_release_object,"com-release-object",1,1 },
  { mx_com_add_ref,"com-add-ref",1,1 },
  { mx_com_ref_count,"com-ref-count",1,1 },

  // browsers

  { mx_make_browser,"make-browser",6,6},
  { mx_block_while_browsers,"block-while-browsers",0,0},
  { mx_browser_show,"browser-show",2,2},
  { mx_navigate,"navigate",2,2 },
  { mx_go_back,"go-back",1,1 },
  { mx_go_forward,"go-forward",1,1 },
  { mx_refresh,"refresh",1,1 },
  { mx_iconize,"iconize",1,1 },
  { mx_restore,"restore",1,1 },
  { mx_current_url,"current-url",1,1 },
  { mx_register_navigate_handler,"register-navigate-handler",2,2 },
  { mx_current_document,"current-document",1,1 },
  { mx_print,"print-document",1,1 },

  // documents

  { mx_document_pred,"document?",1,1 },
  { mx_document_title,"document-title",1,1 },
  { mx_insert_html,"document-insert-html",2,2 },
  { mx_append_html,"document-append-html",2,2 },
  { mx_replace_html,"document-replace-html",2,2 },
  { mx_find_element,"document-find-element",3,4 },
  { mx_find_element_by_id_or_name,"document-find-element-by-id-or-name",2,3 },
  { mx_elements_with_tag,"document-elements-with-tag",2,2 },
  { mx_document_objects,"document-objects",1,1 },

  // elements

  { mx_element_insert_html,"element-insert-html",2,2 },
  { mx_element_append_html,"element-append-html",2,2 },
  { mx_element_insert_text,"element-insert-text",2,2 },
  { mx_element_append_text,"element-append-text",2,2 },
  { mx_element_replace_html,"element-replace-html",2,2 },
  { mx_element_get_html,"element-get-html",1,1 },
  { mx_element_get_text,"element-get-text",1,1 },
  { mx_element_focus,"element-focus",1,1 },
  { mx_element_selection,"element-selection",1,1 },
  { mx_element_set_selection,"element-set-selection!",2,2 },
  { mx_element_attribute,"element-attribute",2,2 },
  { mx_element_set_attribute,"element-set-attribute!",3,3 },
  { mx_element_click,"element-click",1,1 },
  { mx_element_tag,"element-tag",1,1 },
  { mx_element_font_family,"element-font-family",1,1 },
  { mx_element_set_font_family,"element-set-font-family!",2,2 },
  { mx_element_font_style,"element-font-style",1,1 },
  { mx_element_set_font_style,"element-set-font-style!",2,2 },
  { mx_element_font_variant,"element-font-variant",1,1 },
  { mx_element_set_font_variant,"element-set-font-variant!",2,2 },
  { mx_element_font_weight,"element-font-weight",1,1 },
  { mx_element_set_font_weight,"element-set-font-weight!",2,2 },
  { mx_element_font,"element-font",1,1 },
  { mx_element_set_font,"element-set-font!",2,2 },
  { mx_element_background,"element-background",1,1 },
  { mx_element_set_background,"element-set-background!",2,2 },
  { mx_element_background_attachment,"element-background-attachment",1,1 },
  { mx_element_set_background_attachment,"element-set-background-attachment!",2,2 },
  { mx_element_background_image,"element-background-image",1,1 },
  { mx_element_set_background_image,"element-set-background-image!",2,2 },
  { mx_element_background_repeat,"element-background-repeat",1,1 },
  { mx_element_set_background_repeat,"element-set-background-repeat!",2,2 },
  { mx_element_background_position,"element-background-position",1,1 },
  { mx_element_set_background_position,"element-set-background-position!",2,2 },
  { mx_element_text_decoration,"element-text-decoration",1,1 },
  { mx_element_set_text_decoration,"element-set-text-decoration!",2,2 },
  { mx_element_text_transform,"element-text-transform",1,1 },
  { mx_element_set_text_transform,"element-set-text-transform!",2,2 },
  { mx_element_text_align,"element-text-align",1,1 },
  { mx_element_set_text_align,"element-set-text-align!",2,2 },
  { mx_element_margin,"element-margin",1,1 },
  { mx_element_set_margin,"element-set-margin!",2,2 },
  { mx_element_padding,"element-padding",1,1 },
  { mx_element_set_padding,"element-set-padding!",2,2 },
  { mx_element_border,"element-border",1,1 },
  { mx_element_set_border,"element-set-border!",2,2 },
  { mx_element_border_top,"element-border-top",1,1 },
  { mx_element_set_border_top,"element-set-border-top!",2,2 },
  { mx_element_border_bottom,"element-border-bottom",1,1 },
  { mx_element_set_border_bottom,"element-set-border-bottom!",2,2 },
  { mx_element_border_left,"element-border-left",1,1 },
  { mx_element_set_border_left,"element-set-border-left!",2,2 },
  { mx_element_border_right,"element-border-right",1,1 },
  { mx_element_set_border_right,"element-set-border-right!",2,2 },
  { mx_element_border_color,"element-border-color",1,1 },
  { mx_element_set_border_color,"element-set-border-color!",2,2 },
  { mx_element_border_width,"element-border-width",1,1 },
  { mx_element_set_border_width,"element-set-border-width!",2,2 },
  { mx_element_border_style,"element-border-style",1,1 },
  { mx_element_set_border_style,"element-set-border-style!",2,2 },
  { mx_element_border_top_style,"element-border-top-style",1,1 },
  { mx_element_set_border_top_style,"element-set-border-top-style!",2,2 },
  { mx_element_border_bottom_style,"element-border-bottom-style",1,1 },
  { mx_element_set_border_bottom_style,"element-set-border-bottom-style!",2,2 },
  { mx_element_border_left_style,"element-border-left-style",1,1 },
  { mx_element_set_border_left_style,"element-set-border-left-style!",2,2 },
  { mx_element_border_right_style,"element-border-right-style",1,1 },
  { mx_element_set_border_right_style,"element-set-border-right-style!",2,2 },
  { mx_element_style_float,"element-style-float",1,1 },
  { mx_element_set_style_float,"element-set-style-float!",2,2 },
  { mx_element_clear,"element-clear",1,1 },
  { mx_element_set_clear,"element-set-clear!",2,2 },
  { mx_element_display,"element-display",1,1 },
  { mx_element_set_display,"element-set-display!",2,2 },
  { mx_element_visibility,"element-visibility",1,1 },
  { mx_element_set_visibility,"element-set-visibility!",2,2 },
  { mx_element_list_style_type,"element-list-style-type",1,1 },
  { mx_element_set_list_style_type,"element-set-list-style-type!",2,2 },
  { mx_element_list_style_position,"element-list-style-position",1,1 },
  { mx_element_set_list_style_position,"element-set-list-style-position!",2,2 },
  { mx_element_list_style_image,"element-list-style-image",1,1 },
  { mx_element_set_list_style_image,"element-set-list-style-image!",2,2 },
  { mx_element_list_style,"element-list-style",1,1 },
  { mx_element_set_list_style,"element-set-list-style!",2,2 },
  { mx_element_position,"element-position",1,1 },
  { mx_element_overflow,"element-overflow",1,1 },
  { mx_element_set_overflow,"element-set-overflow!",2,2 },
  { mx_element_pagebreak_before,"element-pagebreak-before",1,1 },
  { mx_element_set_pagebreak_before,"element-set-pagebreak-before!",2,2 },
  { mx_element_pagebreak_after,"element-pagebreak-after",1,1 },
  { mx_element_set_pagebreak_after,"element-set-pagebreak-after!",2,2 },
  { mx_element_css_text,"element-css-text",1,1 },
  { mx_element_set_css_text,"element-set-css-text!",2,2 },
  { mx_element_cursor,"element-cursor",1,1 },
  { mx_element_set_cursor,"element-set-cursor!",2,2 },
  { mx_element_clip,"element-clip",1,1 },
  { mx_element_set_clip,"element-set-clip!",2,2 },
  { mx_element_filter,"element-filter",1,1 },
  { mx_element_set_filter,"element-set-filter!",2,2 },
  { mx_element_style_string,"element-style-string",1,1 },
  { mx_element_text_decoration_none,"element-text-decoration-none",1,1 },
  { mx_element_set_text_decoration_none,"element-set-text-decoration-none!",2,2 },
  { mx_element_text_decoration_underline,"element-text-decoration-underline",1,1 },
  { mx_element_set_text_decoration_underline,"element-set-text-decoration-underline!",2,2 },
  { mx_element_text_decoration_overline,"element-text-decoration-overline",1,1 },
  { mx_element_set_text_decoration_overline,"element-set-text-decoration-overline!",2,2 },
  { mx_element_text_decoration_linethrough,"element-text-decoration-linethrough",1,1 },
  { mx_element_set_text_decoration_linethrough,"element-set-text-decoration-linethrough!",2,2 },
  { mx_element_text_decoration_blink,"element-text-decoration-blink",1,1 },
  { mx_element_set_text_decoration_blink,"element-set-text-decoration-blink!",2,2 },
  { mx_element_pixel_top,"element-pixel-top",1,1 },
  { mx_element_set_pixel_top,"element-set-pixel-top!",2,2 },
  { mx_element_pixel_left,"element-pixel-left",1,1 },
  { mx_element_set_pixel_left,"element-set-pixel-left!",2,2 },
  { mx_element_pixel_width,"element-pixel-width",1,1 },
  { mx_element_set_pixel_width,"element-set-pixel-width!",2,2 },
  { mx_element_pixel_height,"element-pixel-height",1,1 },
  { mx_element_set_pixel_height,"element-set-pixel-height!",2,2 },
  { mx_element_pos_top,"element-pos-top",1,1 },
  { mx_element_set_pos_top,"element-set-pos-top!",2,2 },
  { mx_element_pos_left,"element-pos-left",1,1 },
  { mx_element_set_pos_left,"element-set-pos-left!",2,2 },
  { mx_element_pos_width,"element-pos-width",1,1 },
  { mx_element_set_pos_width,"element-set-pos-width!",2,2 },
  { mx_element_pos_height,"element-pos-height",1,1 },
  { mx_element_set_pos_height,"element-set-pos-height!",2,2 },
  { mx_element_font_size,"element-font-size",1,1 },
  { mx_element_set_font_size,"element-set-font-size!",2,2 },
  { mx_element_color,"element-color",1,1 },
  { mx_element_set_color,"element-set-color!",2,2 },
  { mx_element_background_color,"element-background-color",1,1 },
  { mx_element_set_background_color,"element-set-background-color!",2,2 },
  { mx_element_background_position_x,"element-background-position-x",1,1 },
  { mx_element_set_background_position_x,"element-set-background-position-x!",2,2 },
  { mx_element_background_position_y,"element-background-position-y",1,1 },
  { mx_element_set_background_position_y,"element-set-background-position-y!",2,2 },
  { mx_element_letter_spacing,"element-letter-spacing",1,1 },
  { mx_element_set_letter_spacing,"element-set-letter-spacing!",2,2 },
  { mx_element_vertical_align,"element-vertical-align",1,1 },
  { mx_element_set_vertical_align,"element-set-vertical-align!",2,2 },
  { mx_element_text_indent,"element-text-indent",1,1 },
  { mx_element_set_text_indent,"element-set-text-indent!",2,2 },
  { mx_element_line_height,"element-line-height",1,1 },
  { mx_element_set_line_height,"element-set-line-height!",2,2 },
  { mx_element_margin_top,"element-margin-top",1,1 },
  { mx_element_set_margin_top,"element-set-margin-top!",2,2 },
  { mx_element_margin_bottom,"element-margin-bottom",1,1 },
  { mx_element_set_margin_bottom,"element-set-margin-bottom!",2,2 },
  { mx_element_margin_left,"element-margin-left",1,1 },
  { mx_element_set_margin_left,"element-set-margin-left!",2,2 },
  { mx_element_margin_right,"element-margin-right",1,1 },
  { mx_element_set_margin_right,"element-set-margin-right!",2,2 },
  { mx_element_padding_top,"element-padding-top",1,1 },
  { mx_element_set_padding_top,"element-set-padding-top!",2,2 },
  { mx_element_padding_bottom,"element-padding-bottom",1,1 },
  { mx_element_set_padding_bottom,"element-set-padding-bottom!",2,2 },
  { mx_element_padding_left,"element-padding-left",1,1 },
  { mx_element_set_padding_left,"element-set-padding-left!",2,2 },
  { mx_element_padding_right,"element-padding-right",1,1 },
  { mx_element_set_padding_right,"element-set-padding-right!",2,2 },
  { mx_element_border_top_color,"element-border-top-color",1,1 },
  { mx_element_set_border_top_color,"element-set-border-top-color!",2,2 },
  { mx_element_border_bottom_color,"element-border-bottom-color",1,1 },
  { mx_element_set_border_bottom_color,"element-set-border-bottom-color!",2,2 },
  { mx_element_border_left_color,"element-border-left-color",1,1 },
  { mx_element_set_border_left_color,"element-set-border-left-color!",2,2 },
  { mx_element_border_right_color,"element-border-right-color",1,1 },
  { mx_element_set_border_right_color,"element-set-border-right-color!",2,2 },
  { mx_element_border_top_width,"element-border-top-width",1,1 },
  { mx_element_set_border_top_width,"element-set-border-top-width!",2,2 },
  { mx_element_border_bottom_width,"element-border-bottom-width",1,1 },
  { mx_element_set_border_bottom_width,"element-set-border-bottom-width!",2,2 },
  { mx_element_border_left_width,"element-border-left-width",1,1 },
  { mx_element_set_border_left_width,"element-set-border-left-width!",2,2 },
  { mx_element_border_right_width,"element-border-right-width",1,1 },
  { mx_element_set_border_right_width,"element-set-border-right-width!",2,2 },
  { mx_element_width,"element-width",1,1 },
  { mx_element_set_width,"element-set-width!",2,2 },
  { mx_element_height,"element-height",1,1 },
  { mx_element_set_height,"element-set-height!",2,2 },
  { mx_element_top,"element-top",1,1 },
  { mx_element_set_top,"element-set-top!",2,2 },
  { mx_element_left,"element-left",1,1 },
  { mx_element_set_left,"element-set-left!",2,2 },
  { mx_element_z_index,"element-z-index",1,1 },
  { mx_element_set_z_index,"element-set-z-index!",2,2 },

  // events

  { mx_event_pred,"event?",1,1 },
  { mx_get_event,"get-event",1,1 },
  { mx_event_tag,"event-tag",1,1},
  { mx_event_id,"event-id",1,1},
  { mx_event_from_tag,"event-from-tag",1,1},
  { mx_event_from_id,"event-from-id",1,1},
  { mx_event_to_tag,"event-to-tag",1,1},
  { mx_event_to_id,"event-to-id",1,1},
  { mx_event_keycode,"event-keycode",1,1},
  { mx_event_shiftkey,"event-shiftkey",1,1},
  { mx_event_ctrlkey,"event-ctrlkey",1,1},
  { mx_event_altkey,"event-altkey",1,1},
  { mx_event_x,"event-x",1,1},
  { mx_event_y,"event-y",1,1},
  { mx_event_keypress_pred,"event-keypress?",1,1},
  { mx_event_keydown_pred,"event-keydown?",1,1},
  { mx_event_keyup_pred,"event-keyup?",1,1},
  { mx_event_mousedown_pred,"event-mousedown?",1,1},
  { mx_event_mousemove_pred,"event-mousemove?",1,1},
  { mx_event_mouseover_pred,"event-mouseover?",1,1},
  { mx_event_mouseout_pred,"event-mouseout?",1,1},
  { mx_event_mouseup_pred,"event-mouseup?",1,1},
  { mx_event_click_pred,"event-click?",1,1},
  { mx_event_dblclick_pred,"event-dblclick?",1,1},
  { mx_event_error_pred,"event-error?",1,1},
  { mx_block_until_event,"block-until-event",1,1},
  { mx_process_win_events,"process-win-events",0,0},

  // dotnet hack
  { initialize_dotnet_runtime,"%%initialize-dotnet-runtime",0,0},

};

#if !defined(SCHEME_NONNEGATIVE)
#define SCHEME_NONNEGATIVE(thing) (SCHEME_INTP (thing) && SCHEME_INT_VAL (thing) >= 0)
#endif

BOOL isEmptyClsId (CLSID clsId)
{
  return memcmp (&clsId, &emptyClsId, sizeof (CLSID)) == 0;
}

void scheme_release_typedesc (void *p, void *)
{
  MX_TYPEDESC *pTypeDesc;
  ITypeInfo *pITypeInfo, *pITypeInfoImpl;
  IDispatch *pInterface;

  /* NEED TO DO SOME NEW CLEANUP HERE */


  pTypeDesc = (MX_TYPEDESC *)p;

  if (MX_MANAGED_OBJ_RELEASED (pTypeDesc)) {
    return;
  }

  pITypeInfo = pTypeDesc->pITypeInfo;
  pITypeInfoImpl = pTypeDesc->pITypeInfoImpl;
  pInterface = pTypeDesc->pInterface;

  if (pTypeDesc->descKind == funcDesc) {
    pITypeInfo->ReleaseFuncDesc (pTypeDesc->funcdescs.pFuncDesc);
    if (pITypeInfoImpl) {
      pITypeInfoImpl->ReleaseFuncDesc (pTypeDesc->funcdescs.pFuncDescImpl);
    }
  }
  else if (pTypeDesc->descKind == varDesc) {
    pITypeInfo->ReleaseVarDesc (pTypeDesc->pVarDesc);
  }

  pITypeInfo->Release();
  if (pITypeInfoImpl) {
    pITypeInfoImpl->Release();
  }

  if (pInterface) {
    pInterface->Release();
  }

  MX_MANAGED_OBJ_RELEASED (pTypeDesc) = TRUE;
}

void scheme_release_com_object (void *comObject, void *pIDispatch)
{
  ITypeInfo *pITypeInfo;
  ITypeInfo *pEventTypeInfo;
  IConnectionPoint *pIConnectionPoint;
  ISink *pISink;

  if (MX_MANAGED_OBJ_RELEASED (comObject)) {
    return;
  }

  // when COM object GC'd, release associated interfaces

  pITypeInfo = MX_COM_OBJ_TYPEINFO (comObject);

  pEventTypeInfo = MX_COM_OBJ_EVENTTYPEINFO (comObject);
  pIConnectionPoint = MX_COM_OBJ_CONNECTIONPOINT (comObject);
  pISink = MX_COM_OBJ_EVENTSINK (comObject);

  if (pITypeInfo) {
    pITypeInfo->Release();
  }

  if (pEventTypeInfo) {
    pEventTypeInfo->Release();
  }

  if (pIConnectionPoint) {
    pIConnectionPoint->Release();
  }

  if (pISink) {
    pISink->Release();
  }

  if (pIDispatch) {
    ((IDispatch *)pIDispatch)->Release();
  }

  MX_MANAGED_OBJ_RELEASED (comObject) = TRUE;
}

void mx_register_object (Scheme_Object *obj, IUnknown *pIUnknown,
			void (*release_fun) (void *p, void *data))
{

  if (pIUnknown == NULL) {
    // nothing to do
    return;
  }

  scheme_register_finalizer (obj, release_fun, pIUnknown, NULL, NULL);

  scheme_add_managed ((Scheme_Custodian *)scheme_get_param (scheme_current_config(),
							  MZCONFIG_CUSTODIAN),
		     (Scheme_Object *)obj,
		     (Scheme_Close_Custodian_Client *)release_fun,
		     pIUnknown, 0);
}

Scheme_Object *mx_com_add_ref (int argc, Scheme_Object **argv)
{
  IDispatch *pIDispatch;

  pIDispatch = MX_COM_OBJ_VAL (GUARANTEE_COM_OBJ ("com-add-ref", 0));

  pIDispatch->AddRef();

  return scheme_void;
}

Scheme_Object *mx_com_ref_count (int argc, Scheme_Object **argv)
{
  IDispatch *pIDispatch;
  unsigned long n;

  pIDispatch = MX_COM_OBJ_VAL (GUARANTEE_COM_OBJ ("com-ref-count", 0));

  n = pIDispatch->AddRef();
  n--;

  pIDispatch->Release();

  return scheme_make_integer_value_from_unsigned (n);
}

void mx_register_com_object (Scheme_Object *obj, IDispatch *pIDispatch)
{
  mx_register_object (obj, pIDispatch, scheme_release_com_object);
}

Scheme_Object *mx_com_register_object (int argc, Scheme_Object **argv)
{
  GUARANTEE_COM_OBJ ("com-register-com-object", 0);

  mx_register_com_object (argv[0], MX_COM_OBJ_VAL (argv[0]));

  return scheme_void;
}

void scheme_release_simple_com_object (void *comObject, void *pIUnknown)
{

  if (MX_MANAGED_OBJ_RELEASED (comObject)) {
    return;
  }

  if (pIUnknown) {
    ((IUnknown *)pIUnknown)->Release();
  }

  MX_MANAGED_OBJ_RELEASED (comObject) = TRUE;
}

void mx_register_simple_com_object (Scheme_Object *obj, IUnknown *pIUnknown)
{
  mx_register_object (obj, pIUnknown, scheme_release_simple_com_object);
}

void scheme_release_browser (void *wb, void *hwndDestroy)
{
  MX_Browser_Object *b;

  if (MX_MANAGED_OBJ_RELEASED (wb)) {
    return;
  }

  b = (MX_Browser_Object *)wb;

  if (b->pIWebBrowser2) {
    b->pIWebBrowser2->Release();
  }

  if (((MX_Browser_Object *)wb)->pISink) {
    b->pISink->Release();
  }

  if (b->pIEventQueue) {
    b->pIEventQueue->Release();
  }

  if (hwndDestroy) {
    b->destroy = TRUE;
    // dummy msg to force GetMessage() to return
    PostMessage (b->hwnd, WM_NULL, 0, 0);
  }

  browserCount--;

  MX_MANAGED_OBJ_RELEASED (wb) = TRUE;
}

void scheme_release_document (void *doc, void *)
{

  if (MX_MANAGED_OBJ_RELEASED (doc)) {
    return;
  }

  if (((MX_Document_Object *)doc)->pIHTMLDocument2) {
    ((MX_Document_Object *)doc)->pIHTMLDocument2->Release();
  }

  MX_MANAGED_OBJ_RELEASED (doc) = TRUE;
}

Scheme_Object *mx_com_release_object (int argc, Scheme_Object **argv)
{
  GUARANTEE_COM_OBJ ("com-release-object", 0);

  scheme_release_com_object ((void *)argv[0], MX_COM_OBJ_VAL (argv[0]));

  return scheme_void;
}

static
const char * inv_kind_string (INVOKEKIND invKind)
{
  return
      invKind == INVOKE_FUNC ? "method"
      : invKind == INVOKE_PROPERTYGET ? "property"
      : invKind == INVOKE_PROPERTYPUT ? "property"
      : invKind == INVOKE_EVENT ? "event"
      : NULL;
}

static
const char * mx_fun_string (INVOKEKIND invKind)
{
  return
      invKind == INVOKE_FUNC ? "com-invoke"
      : invKind == INVOKE_PROPERTYGET ? "com-get-property"
      : invKind == INVOKE_PROPERTYPUT ? "com-set-property!"
      : NULL;
}

static
unsigned short getHashValue (IDispatch *pIDispatch,
			     INVOKEKIND invKind,
			     LPCTSTR name)
{
  LPCTSTR p;
  unsigned short hashVal;

  hashVal = (unsigned short)pIDispatch + invKind;

  p = name;
  while (*p) {
      hashVal ^= (hashVal << 5) + (hashVal >> 2) + (unsigned short) (*p);
      p++;
      }

  return hashVal % TYPE_TBL_SIZE;
}

void addTypeToTable (IDispatch *pIDispatch, LPCTSTR name,
		    INVOKEKIND invKind,
		    MX_TYPEDESC *pTypeDesc)
{
  unsigned short hashVal;
  MX_TYPE_TBL_ENTRY *pEntry, *p;

  // we don't call AddRef() for the IDispatch pointer
  // because it's not used as an interface, only its
  // pointer value is used, for hashing

  pTypeDesc->pITypeInfo->AddRef();

  pEntry = (MX_TYPE_TBL_ENTRY *)scheme_malloc (sizeof (MX_TYPE_TBL_ENTRY));
  scheme_dont_gc_ptr (pEntry);
  pEntry->pTypeDesc = pTypeDesc;
  pEntry->pIDispatch = pIDispatch;
  pEntry->invKind = invKind;
  pEntry->name = name;
  pEntry->next = NULL;

  hashVal = getHashValue (pIDispatch, invKind, name);

  p = typeTable[hashVal];

  if (p == NULL)
      typeTable[hashVal] = pEntry;

  else {
      while (p->next != NULL)
	  p = p->next;
      p->next = pEntry;
      }
}

MX_TYPEDESC * lookupTypeDesc (IDispatch *pIDispatch, LPCTSTR name,
			      INVOKEKIND invKind)
{
  unsigned short hashVal;
  MX_TYPE_TBL_ENTRY *p;

  hashVal = getHashValue (pIDispatch, invKind, name);

  p = typeTable[hashVal];

  while (p) {
      if (p->pIDispatch == pIDispatch &&
	  p->invKind == invKind &&
	  lstrcmp (p->name, name) == 0)
	  return p->pTypeDesc;

      p = p->next;
      }

  return NULL;
}

void codedComError (const char *s, HRESULT hr)
{
  char buff[1024];
  char finalBuff[2048];

  if (FormatMessage (FORMAT_MESSAGE_FROM_SYSTEM,
		     0, hr, 0, buff, sizeof (buff), NULL) > 0)
      sprintf (finalBuff, "%s, code = %X: %s", s, hr, buff);
  else
      sprintf (finalBuff, "%s, code = %X", s, hr);

  scheme_signal_error (finalBuff);
}

Scheme_Object *mx_version (int argc, Scheme_Object **argv)
{
  return scheme_make_utf8_string (MX_VERSION);
}

Scheme_Object *do_cocreate_instance (CLSID clsId,
				     LPCTSTR name,
				     LPCTSTR location,
				     LPCTSTR machine)
{
  HRESULT hr;
  IDispatch *pIDispatch;
  MX_COM_Object *com_object;

  if (lstrcmpi (location, TEXT ("local")) == 0) {
    hr = CoCreateInstance (clsId, NULL,
			  CLSCTX_LOCAL_SERVER | CLSCTX_INPROC_SERVER,
			  IID_IDispatch, (void **)&pIDispatch);
  }
  else if (lstrcmpi (location, TEXT ("remote")) == 0) {
    COSERVERINFO csi;
    MULTI_QI mqi;
    OLECHAR machineBuff[1024];

    if (machine) {
        unsigned int len;
        unsigned int count;

      csi.dwReserved1 = 0;
      csi.dwReserved2 = 0;
      csi.pAuthInfo = NULL;

      len = (unsigned int)lstrlen (machine);
      count = MultiByteToWideChar (CP_ACP, (DWORD)0,
				  machine, len,
				  machineBuff,
				  sizeray (machineBuff) - 1);
      machineBuff[len] = '\0';

      if (count < len) {
	scheme_signal_error ("cocreate-instance-from-{coclass, progid}: "
			    "Unable to translate machine name to Unicode");
      }

      csi.pwszName = machineBuff;

    }

    mqi.pIID = &IID_IDispatch;
    mqi.pItf = NULL;
    mqi.hr = 0;

    hr = CoCreateInstanceEx (clsId, NULL,
			    CLSCTX_REMOTE_SERVER,
			    machine ? &csi : NULL,
			    1, &mqi);

    pIDispatch = (IDispatch *) (mqi.pItf);

    if (mqi.hr != S_OK || pIDispatch == NULL) {
      codedComError ("cocreate-instance-from-{coclass, progid}: "
		    "Unable to obtain IDispatch interface for remote server",
		    hr);
    }

  }
  else {
    scheme_signal_error ("cocreate-instance-from-{coclass, progid}: "
			"Expected 'local, 'remote, or machine name for 2nd argument, "
			"got '%s", location);
  }

  if (hr != ERROR_SUCCESS) {
    char errBuff[2048];
    sprintf (errBuff, "cocreate-instance-from-{coclass, progid}: Unable to create instance of %s",
	    name);
    codedComError (errBuff, hr);
  }

  com_object = (MX_COM_Object *)scheme_malloc (sizeof (MX_COM_Object));

  com_object->type = mx_com_object_type;
  com_object->pIDispatch = pIDispatch;
  com_object->pITypeInfo = NULL;
  com_object->clsId = clsId;
  com_object->pEventTypeInfo = NULL;
  com_object->pIConnectionPoint = NULL;
  com_object->pISink = NULL;
  com_object->connectionCookie = (DWORD)0;
  com_object->released = FALSE;

  mx_register_com_object ((Scheme_Object *)com_object, pIDispatch);

  return (Scheme_Object *)com_object;
}

static
void bindCocreateLocation (int argc, Scheme_Object **argv,
			   LPCTSTR * pLocation, LPCTSTR * pMachine,
			   char *f)
{
  if (argc == 2) {
      if (SCHEME_SYMBOLP (argv[1])) {
	  *pLocation = schemeSymbolToText (argv[1]);
	  *pMachine = NULL;
	  }
      else if (SCHEME_CHAR_STRINGP (argv[1])) {
	  *pLocation = TEXT ("remote");
	  *pMachine = schemeCharStringToText (argv[1]);
	  }
      else
	  scheme_wrong_type (f, "symbol or string", 0, argc, argv);
      }
  else {
      *pLocation = TEXT ("local");
      *pMachine = NULL;
      }
}

Scheme_Object *mx_cocreate_instance_from_coclass (int argc, Scheme_Object **argv)
{
  LPCTSTR coclass;
  LPCTSTR location;
  LPCTSTR machine;

  GUARANTEE_STRSYM ("cocreate-instance-from-coclass", 0);

  bindCocreateLocation (argc, argv, &location, &machine,
			"cocreate-instance-from-coclass");

  coclass = schemeToText (argv[0]);

  return do_cocreate_instance (getCLSIDFromCoClass (coclass), coclass, location, machine);
}

CLSID schemeProgIdToCLSID (Scheme_Object *obj, const char * fname)
{
  CLSID clsId;
  BSTR wideProgId = schemeToBSTR (obj);

  HRESULT hr = CLSIDFromProgID (wideProgId, &clsId);

  SysFreeString (wideProgId);

  if (FAILED (hr)) {
      char errBuff[2048];
      sprintf (errBuff, "%s: Error retrieving CLSID from ProgID %s",
	       fname, schemeToMultiByte (obj));
      codedComError (errBuff, hr);
      }

  return clsId;
}

Scheme_Object *mx_cocreate_instance_from_progid (int argc,
						 Scheme_Object **argv)
{
  LPCTSTR location;
  LPCTSTR machine;

  GUARANTEE_STRSYM ("cocreate-instance-from-progid", 0);

  bindCocreateLocation (argc, argv, &location, &machine,
			"cocreate-instance-from-progid");

  return do_cocreate_instance (schemeProgIdToCLSID (argv[0], "cocreate-instance-from-progid"),
                               schemeToText (argv[0]),
                               location, machine);
}

Scheme_Object *mx_set_coclass (int argc, Scheme_Object **argv)
{
  GUARANTEE_COM_OBJ ("set-coclass!", 0);
  GUARANTEE_STRSYM ("set-coclass!", 1);

  MX_COM_OBJ_CLSID (argv[0]) = getCLSIDFromCoClass (schemeToText (argv[1]));

  return scheme_void;
}

Scheme_Object *mx_coclass (int argc, Scheme_Object **argv)
{
  HRESULT hr;
  HKEY hkey, hsubkey;
  LONG result;
  FILETIME fileTime;
  unsigned long keyIndex;
  TCHAR clsIdBuffer[256];
  OLECHAR oleClsIdBuffer[256];
  DWORD clsIdBufferSize;
  DWORD dataType;
  BYTE dataBuffer[256];
  DWORD dataBufferSize;
  CLSID clsId, registryClsId;
  int count;
  Scheme_Object *retval;

  clsId = MX_COM_OBJ_CLSID (GUARANTEE_COM_OBJ ("coclass", 0));

  if (isEmptyClsId (clsId))
    scheme_signal_error ("coclass: No coclass for object");

  // use CLSID to rummage through Registry to find coclass

  result = RegOpenKeyEx (HKEY_CLASSES_ROOT,
			"CLSID",
			 (DWORD)0,
			KEY_READ,
			&hkey);


  if (result != ERROR_SUCCESS)
      scheme_signal_error ("Error while searching Windows registry");

  // enumerate subkeys until we find the one we want

  // really, should call RegQueryInfoKey to find size needed for buffers

  keyIndex = 0;

  retval = NULL;

  while (1) {

    // get next subkey

    clsIdBufferSize = sizeof (clsIdBuffer);

    result = RegEnumKeyEx (hkey, keyIndex++,
			  clsIdBuffer,
			  &clsIdBufferSize,
			  0, NULL, NULL,
			  &fileTime);

    if (result == ERROR_NO_MORE_ITEMS)
      break;

    if (result != ERROR_SUCCESS)
      scheme_signal_error ("Error enumerating subkeys in Windows registry");

    if (strlen (clsIdBuffer) != CLSIDLEN) // not a CLSID -- bogus entry
      continue;


    count = MultiByteToWideChar (CP_ACP, (DWORD)0,
				clsIdBuffer, (unsigned int)strlen (clsIdBuffer),
				oleClsIdBuffer, sizeray (oleClsIdBuffer));

    if (count == 0)
      scheme_signal_error ("Error translating CLSID to Unicode");

    oleClsIdBuffer[CLSIDLEN] = '\0';

    hr = CLSIDFromString (oleClsIdBuffer, &registryClsId);

    if (hr != NOERROR)
      scheme_signal_error ("coclass: Error obtaining coclass CLSID");

    if (registryClsId != clsId)
      continue;

    // open subkey

    result = RegOpenKeyEx (hkey, clsIdBuffer,
			  (DWORD)0,
			  KEY_READ, &hsubkey);

    if (result != ERROR_SUCCESS)
      scheme_signal_error ("coclass: Error obtaining coclass value");

    dataBufferSize = sizeof (dataBuffer);

    RegQueryValueEx (hsubkey, "", 0, &dataType, dataBuffer, &dataBufferSize);

    RegCloseKey (hsubkey);

    if (dataType == REG_SZ) {
	retval = multiByteToSchemeCharString ((char *)dataBuffer);
	break;
	}
  }

  RegCloseKey (hkey);

  if (retval == NULL)
    scheme_signal_error ("coclass: object's coclass not found in Registry");

  return retval;
}

Scheme_Object * mx_progid (int argc, Scheme_Object **argv)
{
  HRESULT hr;
  LPOLESTR wideProgId;
  CLSID clsId;

  clsId = MX_COM_OBJ_CLSID (GUARANTEE_COM_OBJ ("progid", 0));

  if (isEmptyClsId (clsId))
      scheme_signal_error ("progid: No coclass for object");

  hr = ProgIDFromCLSID (clsId, &wideProgId);

  if (FAILED (hr))
      scheme_signal_error ("progid: Error finding coclass");

  return BSTRToSchemeString (wideProgId);
}

Scheme_Object *mx_set_coclass_from_progid (int argc, Scheme_Object **argv)
{
  GUARANTEE_COM_OBJ ("set-coclass-from-progid!", 0);
  GUARANTEE_STRSYM ("set-coclass-from-progid!", 1);

  MX_COM_OBJ_CLSID (argv[0]) = schemeProgIdToCLSID (argv[1], "set-coclass-from-progid!");

  return scheme_void;
}

ITypeInfo *typeInfoFromComObject (MX_COM_Object *obj)
{
  HRESULT hr;
  ITypeInfo *pITypeInfo;
  IDispatch *pIDispatch;
  unsigned int count;

  pITypeInfo = obj->pITypeInfo;

  if (pITypeInfo)
    return pITypeInfo;

  pIDispatch = obj->pIDispatch;

  pIDispatch->GetTypeInfoCount (&count);

  if (count == 0)
    scheme_signal_error ("COM object does not expose type information");

  hr = pIDispatch->GetTypeInfo (0, LOCALE_SYSTEM_DEFAULT, &pITypeInfo);

  if (FAILED (hr) || pITypeInfo == NULL)
    codedComError ("Error getting COM type information", hr);

  obj->pITypeInfo = pITypeInfo;

  return pITypeInfo;
}

Scheme_Object *mx_com_get_object_type (int argc, Scheme_Object **argv)
{
  ITypeInfo *pITypeInfo;
  MX_COM_Type *retval;
  MX_COM_Object *obj;

  GUARANTEE_COM_OBJ ("com-object-type", 0);

  obj = (MX_COM_Object *)argv[0];
  pITypeInfo = typeInfoFromComObject (obj);

  retval = (MX_COM_Type *)scheme_malloc (sizeof (MX_COM_Type));

  retval->type = mx_com_type_type;
  retval->released = FALSE;
  retval->pITypeInfo = pITypeInfo;
  retval->clsId = obj->clsId;

  mx_register_simple_com_object ((Scheme_Object *)retval, pITypeInfo);

  return (Scheme_Object *)retval;
}

BOOL typeInfoEq (ITypeInfo *pITypeInfo1, ITypeInfo *pITypeInfo2)
{
  HRESULT hr;
  TYPEATTR *pTypeAttr1, *pTypeAttr2;
  BOOL retval;

  // intensional equality

  if (pITypeInfo1 == pITypeInfo2)
    return TRUE;

  hr = pITypeInfo1->GetTypeAttr (&pTypeAttr1);

  if (FAILED (hr) || pTypeAttr1 == NULL)
    codedComError ("Error getting type attributes", hr);

  hr = pITypeInfo2->GetTypeAttr (&pTypeAttr2);

  if (FAILED (hr) || pTypeAttr2 == NULL)
    codedComError ("Error getting type attributes", hr);

  retval = (pTypeAttr1->guid == pTypeAttr2->guid);

  pITypeInfo1->ReleaseTypeAttr (pTypeAttr1);
  pITypeInfo2->ReleaseTypeAttr (pTypeAttr2);

  return retval;
}

Scheme_Object *mx_com_is_a (int argc, Scheme_Object **argv)
{
  ITypeInfo *pITypeInfo1, *pITypeInfo2;

  GUARANTEE_COM_OBJ ("com-is-a?", 0);
  GUARANTEE_COM_TYPE ("com-is-a?", 1);

  pITypeInfo1 = typeInfoFromComObject ((MX_COM_Object *)argv[0]);
  pITypeInfo2 = MX_COM_TYPE_VAL ((MX_COM_Type *)argv[1]);

  return typeInfoEq (pITypeInfo1, pITypeInfo2) ? scheme_true : scheme_false;
}

Scheme_Object *mx_com_help (int argc, Scheme_Object **argv)
{
  HRESULT hr;
  ITypeInfo *pITypeInfo;
  BSTR helpFileName;
  char buff[MAX_PATH];
  unsigned int len;

  GUARANTEE_COM_OBJ_OR_TYPE ("com-help", 0);

  if (argc == 2)
      GUARANTEE_STRSYM ("com-help", 1);

  pITypeInfo =
      MX_COM_TYPEP (argv[0])
      ? MX_COM_TYPE_VAL (argv[0])
      : (MX_COM_OBJ_VAL (argv[0]) == NULL)
      ? (scheme_signal_error ("com-help: NULL COM object"), (ITypeInfo *) NULL)
      : typeInfoFromComObject ((MX_COM_Object *)argv[0]);

  hr = pITypeInfo->GetDocumentation (MEMBERID_NIL, NULL, NULL, NULL,
				    &helpFileName);

  if (FAILED (hr))
    codedComError ("Can't get help", hr);

  else if (helpFileName == NULL || wcscmp (helpFileName, L"") == 0)
    scheme_signal_error ("No help available");

  WideCharToMultiByte (CP_ACP, (DWORD)0, helpFileName, SysStringLen (helpFileName),
		      buff, sizeof (buff) - 1,
		      NULL, NULL);

  SysFreeString (helpFileName);

  buff[sizeof (buff)-1] = '\0';

  len = (unsigned int) strlen (buff);

  if (stricmp (buff + len - 4, ".CHM") == 0) {
      HWND hwnd = (argc >= 2)
	  ? HtmlHelp (NULL, buff,
		      HH_DISPLAY_INDEX, PtrToInt (schemeToText (argv[1])))
	  : HtmlHelp (NULL, buff, HH_DISPLAY_TOPIC, 0);

    if (hwnd)
      SetForegroundWindow (hwnd);
  }
  else if (stricmp (buff + len - 4, ".HLP") == 0) {
    if (argc >= 2)
      WinHelp (NULL, buff, HELP_KEY, PtrToInt (schemeToText (argv[1])));
    else
      WinHelp (NULL, buff, HELP_FINDER, 0);
  }
  else
    scheme_signal_error ("Unknown help file type: %s", buff);

  return scheme_void;
}

void signalCodedEventSinkError (char *s, HRESULT hr)
{
  ReleaseSemaphore (eventSinkMutex, 1, NULL);
  codedComError (s, hr);
}

void connectComObjectToEventSink (MX_COM_Object *obj)
{
  HRESULT hr;
  IUnknown *pIUnknown;
  IDispatch *pIDispatch;
  ITypeInfo *pITypeInfo;
  IConnectionPointContainer *pIConnectionPointContainer;
  IConnectionPoint *pIConnectionPoint;
  ISink *pISink;
  DWORD cookie;
  TYPEATTR *pTypeAttr;

  if (obj->pIConnectionPoint)
    return;

  WaitForSingleObject (eventSinkMutex, INFINITE);

  pIDispatch = obj->pIDispatch;

  hr = pIDispatch->QueryInterface (IID_IConnectionPointContainer, (void **)&pIConnectionPointContainer);

  if (FAILED (hr) || pIConnectionPointContainer == NULL)
    signalCodedEventSinkError ("cocreate-instance-from-{coclass, progid}: "
			      "Unable to get COM object connection point "
			      "container", hr);

  pITypeInfo = eventTypeInfoFromComObject (obj);

  if (pITypeInfo == NULL) {
    ReleaseSemaphore (eventSinkMutex, 1, NULL);
    scheme_signal_error ("cocreate-instance-from-{coclass, progid}: "
			"Unable to get type information for events");
  }

  hr = pITypeInfo->GetTypeAttr (&pTypeAttr);

  if (FAILED (hr) || pTypeAttr == NULL)
    signalCodedEventSinkError ("cocreate-instance-from-{coclass, progid}: "
			      "Unable to get type attributes for events", hr);

  hr = pIConnectionPointContainer->FindConnectionPoint (pTypeAttr->guid, &pIConnectionPoint);

  pITypeInfo->ReleaseTypeAttr (pTypeAttr);
  pIConnectionPointContainer->Release();

  if (FAILED (hr) || pIConnectionPoint == NULL)
    signalCodedEventSinkError ("cocreate-instance-from-{coclass, progid}: "
			      "Unable to find COM object connection point", hr);

  hr = CoCreateInstance (CLSID_Sink, NULL, CLSCTX_LOCAL_SERVER | CLSCTX_INPROC_SERVER,
			IID_IUnknown, (void **)&pIUnknown);

  if (FAILED (hr) || pIUnknown == NULL)
    signalCodedEventSinkError ("cocreate-instance-from-{coclass, progid}: "
			      "Unable to create sink object", hr);

  hr = pIUnknown->QueryInterface (IID_ISink, (void **)&pISink);

  if (FAILED (hr) || pISink == NULL)
    signalCodedEventSinkError ("cocreate-instance-from-{coclass, progid}: "
			      "Unable to find sink interface", hr);

  pISink->set_myssink_table (&myssink_table);

  hr = pIConnectionPoint->Advise (pIUnknown, &cookie);

  pIUnknown->Release();

  if (FAILED (hr))
    signalCodedEventSinkError ("cocreate-instance-from-{coclass, progid}: "
			      "Unable to connect sink to connection point", hr);

  obj->pEventTypeInfo = pITypeInfo;
  obj->pIConnectionPoint = pIConnectionPoint;
  obj->connectionCookie = cookie;
  obj->pISink = pISink;

  ReleaseSemaphore (eventSinkMutex, 1, NULL);
}

FUNCDESC *getFuncDescForEvent (LPOLESTR name, ITypeInfo *pITypeInfo)
{
  HRESULT hr;
  TYPEATTR *pTypeAttr;
  FUNCDESC *pFuncDesc;
  BSTR bstr;
  UINT bstrCount;
  unsigned short numFuncDescs;
  int i;

  hr = pITypeInfo->GetTypeAttr (&pTypeAttr);

  if (FAILED (hr) || pTypeAttr == NULL)
    codedComError ("Unable to get type attributes for events", hr);

  numFuncDescs = pTypeAttr->cFuncs;

  pITypeInfo->ReleaseTypeAttr (pTypeAttr);

  for (i = 0; i < numFuncDescs; i++) {

    hr = pITypeInfo->GetFuncDesc (i, &pFuncDesc);

    if (FAILED (hr))
      codedComError ("Error getting event method type description", hr);

    // rely on name of event

    hr = pITypeInfo->GetNames (pFuncDesc->memid, &bstr, 1, &bstrCount);

    if (FAILED (hr))
      codedComError ("Error getting event method name", hr);

    if (wcscmp (name, bstr) == 0) {
      SysFreeString (bstr);
      return pFuncDesc;
    }

    SysFreeString (bstr);

    pITypeInfo->ReleaseFuncDesc (pFuncDesc);
  }

  return NULL;
}

Scheme_Object *mx_com_register_event_handler (int argc, Scheme_Object **argv)
{
  ITypeInfo *pITypeInfo;
  ISink *pISink;
  FUNCDESC *pFuncDesc;
  BSTR unicodeName;

  GUARANTEE_COM_OBJ   ("com-register-event-handler", 0);
  GUARANTEE_STRSYM    ("com-register-event-handler", 1);
  GUARANTEE_PROCEDURE ("com-register-event-handler", 2);

  connectComObjectToEventSink ((MX_COM_Object *)argv[0]);

  pITypeInfo = MX_COM_OBJ_EVENTTYPEINFO (argv[0]);
  pISink = MX_COM_OBJ_EVENTSINK (argv[0]);

  unicodeName = schemeToBSTR (argv[1]);

  pFuncDesc = getFuncDescForEvent (unicodeName, pITypeInfo);

  SysFreeString (unicodeName);

  if (pFuncDesc == NULL)
      scheme_signal_error ("Can't find event %s in type description", schemeToText (argv[1]));

  pISink->register_handler (pFuncDesc->memid, argv[2]);

  pITypeInfo->ReleaseFuncDesc (pFuncDesc);

  return scheme_void;
}

Scheme_Object *mx_com_unregister_event_handler (int argc, Scheme_Object **argv)
{
  ITypeInfo *pITypeInfo;
  ISink *pISink;
  FUNCDESC *pFuncDesc;
  BSTR unicodeName;

  GUARANTEE_STRSYM  ("com-unregister-event-handler", 1);

  pITypeInfo = MX_COM_OBJ_EVENTTYPEINFO (GUARANTEE_COM_OBJ ("com-unregister-event-handler", 0));

  if (pITypeInfo == NULL)
    scheme_signal_error ("No event type information for object");

  pISink = MX_COM_OBJ_EVENTSINK (argv[0]);

  if (pISink == NULL) // no events registered
    return scheme_void;

  unicodeName = schemeToBSTR (argv[1]);

  pFuncDesc = getFuncDescForEvent (unicodeName, pITypeInfo);

  SysFreeString (unicodeName);

  if (pFuncDesc == NULL)
      scheme_signal_error ("Can't find event %s in type description", schemeToText (argv[1]));

  pISink->unregister_handler (pFuncDesc->memid);

  pITypeInfo->ReleaseFuncDesc (pFuncDesc);

  return scheme_void;
}

MX_TYPEDESC *doTypeDescFromTypeInfo (BSTR name, INVOKEKIND invKind,
				    ITypeInfo *pITypeInfo) {
  HRESULT hr;
  TYPEATTR *pTypeAttr;
  FUNCDESC *pFuncDesc;
  VARDESC *pVarDesc;
  MEMBERID memID;
  MX_DESCKIND descKind;
  MX_TYPEDESC *pTypeDesc;
  BSTR bstr;
  UINT nameCount;
  UINT funcDescIndex;
  BOOL foundDesc;
  unsigned short dispFuncs, implFuncs;
  int i;

  hr = pITypeInfo->GetTypeAttr (&pTypeAttr);

  if (FAILED (hr))
    codedComError ("Error getting attributes for type library", hr);

  foundDesc = FALSE;

  // can skip first 7, because those are IDispatch-specific
  dispFuncs = pTypeAttr->cFuncs;
  for (i = 7; i < dispFuncs; i++) {

    hr = pITypeInfo->GetFuncDesc (i, &pFuncDesc);

    if (FAILED (hr))
      codedComError ("Error getting type description", hr);

    pITypeInfo->GetNames (pFuncDesc->memid, &bstr, 1, &nameCount);

    // see if this FUNCDESC is the one we want

    if (wcscmp (bstr, name) == 0 &&
	 (invKind == INVOKE_EVENT || pFuncDesc->invkind == invKind)) {

      foundDesc = TRUE;
      descKind = funcDesc;
      SysFreeString (bstr);
      memID = pFuncDesc->memid;
      funcDescIndex = i;

      break;
    }

    // if not, throw it back

    SysFreeString (bstr);
    pITypeInfo->ReleaseFuncDesc (pFuncDesc);

  }

  if (invKind == INVOKE_PROPERTYGET ||
      invKind == INVOKE_PROPERTYPUT ||
      invKind == INVOKE_PROPERTYPUTREF) {

    for (i = 0; i < pTypeAttr->cVars; i++) {
      hr = pITypeInfo->GetVarDesc (i, &pVarDesc);
      if (FAILED (hr)) {
	codedComError ("Error getting type description", hr);
      }

      // see if this VARDESC is the one we want

      pITypeInfo->GetNames (pVarDesc->memid, &bstr, 1, &nameCount);

      if (wcscmp (bstr, name)) {
	foundDesc = TRUE;
	descKind = varDesc;
	memID = pVarDesc->memid;

	break;
      }

      // if not, throw it back

      pITypeInfo->ReleaseVarDesc (pVarDesc);

    }
  }

  pITypeInfo->ReleaseTypeAttr (pTypeAttr);

  if (foundDesc == FALSE) {
    ITypeInfo *pITypeInfoImpl;
    TYPEATTR *pTypeAttrImpl;
    HREFTYPE refType;

    // search in inherited interfaces
    for (i = 0; i < pTypeAttr->cImplTypes; i++) {
      hr = pITypeInfo->GetRefTypeOfImplType (i, &refType);

      if (FAILED (hr))
	scheme_signal_error ("Can't get implementation type library handle");

      hr = pITypeInfo->GetRefTypeInfo (refType, &pITypeInfoImpl);

      if (FAILED (hr))
	scheme_signal_error ("Can't get implementation type library");

      hr = pITypeInfoImpl->GetTypeAttr (&pTypeAttrImpl);

      if (FAILED (hr))
	scheme_signal_error ("Can't get implementation type library attributes");

      // recursion, to ascend the inheritance graph
      pTypeDesc = doTypeDescFromTypeInfo (name, invKind, pITypeInfoImpl);

      // release interfaces
      pITypeInfoImpl->ReleaseTypeAttr (pTypeAttrImpl);
      pITypeInfoImpl->Release();

      if (pTypeDesc)
	return pTypeDesc;
    }

    return NULL;
  }

  pTypeDesc = (MX_TYPEDESC *)scheme_malloc (sizeof (MX_TYPEDESC));

  pTypeDesc->type = mx_com_typedesc_type;
  pTypeDesc->released = FALSE;

  pTypeDesc->memID = memID;

  pTypeDesc->pITypeInfo = pITypeInfo;
  pITypeInfo->AddRef();

  pTypeDesc->descKind = descKind;

  pTypeDesc->funOffset = NO_FUNPTR; // assume for now

  if (descKind == funcDesc) {
    HREFTYPE refType;
    ITypeInfo *pITypeInfoImpl;

    pTypeDesc->funcdescs.pFuncDesc = pFuncDesc;
    hr = pITypeInfo->GetRefTypeOfImplType (-1, &refType);
    if (hr == S_OK) {
      hr = pITypeInfo->GetRefTypeInfo (refType, &pITypeInfoImpl);
      if (hr == S_OK) {
	TYPEATTR *pTypeAttrImpl;
	FUNCDESC *pFuncDescImpl;
	hr = pITypeInfoImpl->GetTypeAttr (&pTypeAttrImpl);
	if (hr == S_OK) {
	  implFuncs = pTypeAttrImpl->cFuncs;
	  // assumption: impl TypeInfo has FuncDescs in same order
	  //             as the Dispatch TypeInfo
	  // but dispFuncs has IDispatch methods
	  funcDescIndex -= dispFuncs - implFuncs;
	  hr = pITypeInfoImpl->GetFuncDesc (funcDescIndex, &pFuncDescImpl);
	  if (hr == S_OK) {
	    if (pFuncDescImpl->funckind == FUNC_VIRTUAL ||
		pFuncDescImpl->funckind == FUNC_PUREVIRTUAL) {
	      pTypeDesc->implGuid = pTypeAttrImpl->guid;
	      pTypeDesc->funOffset = pFuncDescImpl->oVft/4;
	      pTypeDesc->pITypeInfoImpl = pITypeInfoImpl;
	      pITypeInfoImpl->AddRef();
	      pTypeDesc->funcdescs.pFuncDescImpl = pFuncDescImpl;
	    }
	    else {
	      pITypeInfoImpl->ReleaseFuncDesc (pFuncDescImpl);
	    }
	  }
	  pITypeInfoImpl->ReleaseTypeAttr (pTypeAttrImpl);
	}
	else {
	  pITypeInfoImpl->Release();
	}
      }
    }
  }
  else {
    pTypeDesc->pVarDesc = pVarDesc;
  }

  scheme_add_managed ((Scheme_Custodian *)scheme_get_param (scheme_current_config(), MZCONFIG_CUSTODIAN),
		     (Scheme_Object *)pTypeDesc,
		     (Scheme_Close_Custodian_Client *)scheme_release_typedesc,
		     NULL, 0);
  scheme_register_finalizer (pTypeDesc, scheme_release_typedesc, NULL, NULL, NULL);

  return pTypeDesc;

}

static
MX_TYPEDESC *typeDescFromTypeInfo (LPCTSTR name,
                                   INVOKEKIND invKind,
                                   ITypeInfo *pITypeInfo)
{
  BSTR unicodeName;
  MX_TYPEDESC *retval;

  unicodeName = textToBSTR (name, strlen (name));
  retval = doTypeDescFromTypeInfo (unicodeName, invKind, pITypeInfo);

  SysFreeString (unicodeName);

  return retval;
}

MX_TYPEDESC *getMethodType (MX_COM_Object *obj, LPCTSTR name, INVOKEKIND invKind)
{
  IDispatch *pIDispatch;
  MX_TYPEDESC *pTypeDesc;
  ITypeInfo *pITypeInfo;

  // need Unicode version of name to please ITypeInfo::GetIDsOfNames
  // note that we need string length + 1

  pIDispatch = obj->pIDispatch;

  // check in hash table to see if we already have the type information

  pTypeDesc = lookupTypeDesc (pIDispatch, name, invKind);

  if (pTypeDesc)
      return pTypeDesc;

  if (invKind == INVOKE_EVENT) {
      pITypeInfo = eventTypeInfoFromComObject (obj);

      if (pITypeInfo == NULL)
	  scheme_signal_error ("Can't find event type information");
      }
  else
      pITypeInfo = typeInfoFromComObject (obj);

  pTypeDesc = typeDescFromTypeInfo (name, invKind, pITypeInfo);
  // pTypeDesc may be NULL
  if (pTypeDesc != NULL)
      addTypeToTable (pIDispatch, name, invKind, pTypeDesc);

  return pTypeDesc;
}

static int dispatchCmp (const char * s1, const char * * s2)
{
  return lstrcmp (s1, *s2);
}

BOOL isDispatchName (const char *s)
{
  static char *names[] = { // must be in alpha order
    "AddRef",
    "GetIDsOfNames",
    "GetTypeInfo",
    "GetTypeInfoCount",
    "Invoke",
    "QueryInterface",
    "Release",
  };

  return bsearch (s, names, sizeray (names), sizeof (names[0]),
		  (int (*) (const void *, const void *))dispatchCmp)
      ? TRUE
      : FALSE;
}

Scheme_Object *getTypeNames (ITypeInfo *pITypeInfo,
			    TYPEATTR *pTypeAttr, Scheme_Object *retval,
			    INVOKEKIND invKind)
{
  ITypeInfo *pITypeInfoImpl;
  TYPEATTR *pTypeAttrImpl;
  BSTR bstr;
  FUNCDESC *pFuncDesc;
  VARDESC *pVarDesc;
  HREFTYPE refType;
  unsigned int count;
  int i;

  for (i = 0; i < pTypeAttr->cImplTypes; i++) {
      HRESULT hr = pITypeInfo->GetRefTypeOfImplType (i, &refType);

      if (FAILED (hr))
	  scheme_signal_error ("Can't get implementation type library handle");

      hr = pITypeInfo->GetRefTypeInfo (refType, &pITypeInfoImpl);

      if (FAILED (hr))
	  scheme_signal_error ("Can't get implementation type library");

      hr = pITypeInfoImpl->GetTypeAttr (&pTypeAttrImpl);

      if (FAILED (hr))
	  scheme_signal_error ("Can't get implementation type library attributes");

      // recursion, to ascend the inheritance graph
      retval = getTypeNames (pITypeInfoImpl, pTypeAttrImpl, retval, invKind);

      // release interfaces
      pITypeInfoImpl->ReleaseTypeAttr (pTypeAttrImpl);
      pITypeInfoImpl->Release();
      }

  // properties can appear in list of functions
  // or in list of variables

  for (i = 0; i < pTypeAttr->cFuncs; i++) {
      char buff[256];
      unsigned int len;

      pITypeInfo->GetFuncDesc (i, &pFuncDesc);
      if (pFuncDesc->invkind == invKind) {
	  pITypeInfo->GetNames (pFuncDesc->memid, &bstr, 1, &count);

	  if (invKind == INVOKE_FUNC) {
	      len = SysStringLen (bstr);
	      WideCharToMultiByte (CP_ACP, (DWORD)0, bstr, len,
				   buff, sizeof (buff) - 1,
				   NULL, NULL);
	      buff[len] = '\0';
	      }

	  // don't consider names inherited from IDispatch
	  if (invKind != INVOKE_FUNC || !isDispatchName (buff))
	      retval = scheme_make_pair (BSTRToSchemeString (bstr), retval);
	  SysFreeString (bstr);
	  }
      pITypeInfo->ReleaseFuncDesc (pFuncDesc);
      }

  if (invKind == INVOKE_FUNC) // done, if not a property
      return retval;

  for (i = 0; i < pTypeAttr->cVars; i++) {
      pITypeInfo->GetVarDesc (i, &pVarDesc);
      pITypeInfo->GetNames (pVarDesc->memid, &bstr, 1, &count);
      retval = scheme_make_pair (BSTRToSchemeString (bstr), retval);
      SysFreeString (bstr);
      pITypeInfo->ReleaseVarDesc (pVarDesc);
      }

  return retval;
}


Scheme_Object *mx_do_get_methods (int argc, Scheme_Object **argv, INVOKEKIND invKind)
{
  ITypeInfo *pITypeInfo;
  HRESULT hr;
  TYPEATTR *pTypeAttr;
  Scheme_Object *retval;

  GUARANTEE_COM_OBJ_OR_TYPE ("com-{methods, {get, set}-properties}", 0);

  pITypeInfo =
      MX_COM_TYPEP (argv[0])
      ? MX_COM_TYPE_VAL (argv[0])
      : (MX_COM_OBJ_VAL (argv[0]) == NULL)
      ? (scheme_signal_error ("com-{methods, {get, set}-properties}: NULL COM object"), (ITypeInfo *) NULL)
      : typeInfoFromComObject ((MX_COM_Object *)argv[0]);

  hr = pITypeInfo->GetTypeAttr (&pTypeAttr);

  if (FAILED (hr) || pTypeAttr == NULL)
    codedComError ("Error getting type attributes", hr);

  retval = getTypeNames (pITypeInfo, pTypeAttr, scheme_null, invKind);

  pITypeInfo->ReleaseTypeAttr (pTypeAttr);

  return retval;

}

Scheme_Object *mx_com_methods (int argc, Scheme_Object **argv)
{
  return mx_do_get_methods (argc, argv, INVOKE_FUNC);
}

Scheme_Object *mx_com_get_properties (int argc, Scheme_Object **argv)
{
  return mx_do_get_methods (argc, argv, INVOKE_PROPERTYGET);
}

Scheme_Object *mx_com_set_properties (int argc, Scheme_Object **argv)
{
  return mx_do_get_methods (argc, argv, INVOKE_PROPERTYPUT);
}

ITypeInfo *coclassTypeInfoFromTypeInfo (ITypeInfo *pITypeInfo, CLSID clsId)
{
  HRESULT hr;
  ITypeLib *pITypeLib;
  ITypeInfo *pCoclassTypeInfo;
  ITypeInfo *pCandidateTypeInfo;
  TYPEATTR *pTypeAttr;
  TYPEKIND typeKind;
  HREFTYPE hRefType;
  UINT ndx;
  UINT typeInfoCount;
  UINT coclassCount;
  UINT typeCount;
  UINT coclassNdx;
  UINT i, j;

  hr = pITypeInfo->GetContainingTypeLib (&pITypeLib, &ndx);

  if (FAILED (hr))
    scheme_signal_error ("Can't get dispatch type library");

  // first try using explicit clsId

  if (!isEmptyClsId (clsId)) {
    hr = pITypeLib->GetTypeInfoOfGuid (clsId, &pCoclassTypeInfo);

    pITypeLib->Release();

    if (FAILED (hr) || pCoclassTypeInfo == NULL) {
      codedComError ("Error getting type info for coclass", hr);
      return NULL;
    }

    return pCoclassTypeInfo;
  }

  // if no CLSID, search for coclass implementing supplied
  // interface

  typeInfoCount = pITypeLib->GetTypeInfoCount();

  coclassCount = 0;

  // check for ambiguity

  for (i = 0; i < typeInfoCount; i++) {

    pITypeLib->GetTypeInfoType (i, &typeKind);

    if (typeKind == TKIND_COCLASS) {

      hr = pITypeLib->GetTypeInfo (i, &pCoclassTypeInfo);

      if (FAILED (hr) || pCoclassTypeInfo == NULL) {
	pITypeLib->Release();
	codedComError ("Error getting type info for coclass", hr);
      }

      hr = pCoclassTypeInfo->GetTypeAttr (&pTypeAttr);

      if (FAILED (hr) || pTypeAttr == NULL) {
	pCoclassTypeInfo->Release();
	pITypeLib->Release();
	codedComError ("Error getting coclass type attributes", hr);
      }

      typeCount = pTypeAttr->cImplTypes;

      pCoclassTypeInfo->ReleaseTypeAttr (pTypeAttr);

      for (j = 0; j < typeCount; j++) {
	hr = pCoclassTypeInfo->GetRefTypeOfImplType (j, &hRefType);

	if (FAILED (hr)) {
	  pCoclassTypeInfo->Release();
	  pITypeLib->Release();
	  codedComError ("Error retrieving type info handle", hr);
	}

	hr = pCoclassTypeInfo->GetRefTypeInfo (hRefType, &pCandidateTypeInfo);

	if (FAILED (hr) || pCandidateTypeInfo == NULL) {
	  pCoclassTypeInfo->Release();
	  pITypeLib->Release();
	  codedComError ("Error retrieving candidate type info", hr);
	}

	if (typeInfoEq (pCandidateTypeInfo, pITypeInfo)) {
	  coclassNdx = i;
	  if (++coclassCount >= 2) {
	    pCandidateTypeInfo->Release();
	    pCoclassTypeInfo->Release();
	    pITypeLib->Release();
	    scheme_signal_error ("Ambiguous coclass for object");
	  }
	}

	pCandidateTypeInfo->Release();

      }

      pCoclassTypeInfo->Release();

    }
  }

  if (coclassCount == 0) {
    pITypeLib->Release();
    return NULL;
  }

  hr = pITypeLib->GetTypeInfo (coclassNdx, &pCoclassTypeInfo);

  pITypeLib->Release();

  if (FAILED (hr) || pCoclassTypeInfo == NULL)
    codedComError ("Error getting type info for coclass", hr);

  return pCoclassTypeInfo;
}

ITypeInfo *eventTypeInfoFromCoclassTypeInfo (ITypeInfo *pCoclassTypeInfo)
{
  HRESULT hr;
  ITypeInfo *pEventTypeInfo;
  TYPEATTR *pTypeAttr;
  HREFTYPE hRefType;
  UINT typeCount;
  UINT eventTypeInfoNdx;
  int typeFlags;
  UINT i;

  hr = pCoclassTypeInfo->GetTypeAttr (&pTypeAttr);

  if (FAILED (hr) || pTypeAttr == NULL)
    codedComError ("Error getting type attributes", hr);

  typeCount = pTypeAttr->cImplTypes;

  pCoclassTypeInfo->ReleaseTypeAttr (pTypeAttr);

  eventTypeInfoNdx = -1;

  for (i = 0; i < typeCount; i++) {

    hr = pCoclassTypeInfo->GetImplTypeFlags (i, &typeFlags);

    if (FAILED (hr))
      codedComError ("Error retrieving type flags", hr);

    // look for [source, default]

    if ((typeFlags & IMPLTYPEFLAG_FSOURCE) &&
	 (typeFlags & IMPLTYPEFLAG_FDEFAULT)) {
      eventTypeInfoNdx = i;
      break;
    }
  }

  if (eventTypeInfoNdx == -1)
    return NULL;

  hr = pCoclassTypeInfo->GetRefTypeOfImplType (eventTypeInfoNdx, &hRefType);

  if (FAILED (hr))
    codedComError ("Error retrieving type info handle", hr);

  hr = pCoclassTypeInfo->GetRefTypeInfo (hRefType, &pEventTypeInfo);

  if (FAILED (hr))
    codedComError ("Error retrieving event type info", hr);

  return pEventTypeInfo;
}

ITypeInfo *eventTypeInfoFromComObject (MX_COM_Object *obj)
{
  HRESULT hr;
  IDispatch *pIDispatch;
  ITypeInfo *pCoclassTypeInfo, *pEventTypeInfo;
  IProvideClassInfo *pIProvideClassInfo;

  pEventTypeInfo = obj->pEventTypeInfo;

  if (pEventTypeInfo)
    return pEventTypeInfo;

  pIDispatch = obj->pIDispatch;

  /* preferred mechanism for finding coclass ITypeInfo */

  hr = pIDispatch->QueryInterface (IID_IProvideClassInfo,
				  (void **)&pIProvideClassInfo);

  if (SUCCEEDED (hr) && pIProvideClassInfo != NULL) {

    hr = pIProvideClassInfo->GetClassInfo (&pCoclassTypeInfo);

    if (FAILED (hr) || pCoclassTypeInfo == NULL)
      scheme_signal_error ("Error getting coclass type information");
  }
  else if (hr == E_NOINTERFACE) {
    ITypeInfo *pDispatchTypeInfo;

    /* alternate mechanism */

    hr = pIDispatch->GetTypeInfo (0, LOCALE_SYSTEM_DEFAULT, &pDispatchTypeInfo);

    if (FAILED (hr))
      codedComError ("Can't get dispatch type information", hr);

    pCoclassTypeInfo = coclassTypeInfoFromTypeInfo (pDispatchTypeInfo,
						   obj->clsId);
    pDispatchTypeInfo->Release();

    if (pCoclassTypeInfo == NULL)
      scheme_signal_error ("Error getting coclass type information");
  }

  else
    codedComError ("Error getting COM event type information", hr);

  // have type info for coclass
  // event type info is one of the "implemented" interfaces

  pEventTypeInfo = eventTypeInfoFromCoclassTypeInfo (pCoclassTypeInfo);

  pCoclassTypeInfo->Release();

  if (pEventTypeInfo == NULL)
    scheme_signal_error ("Error retrieving event type info");

  obj->pEventTypeInfo = pEventTypeInfo;

  return pEventTypeInfo;
}

ITypeInfo *eventTypeInfoFromComType (MX_COM_Type *obj)
{
  ITypeInfo *pCoclassTypeInfo, *pEventTypeInfo;

  pCoclassTypeInfo = coclassTypeInfoFromTypeInfo (obj->pITypeInfo,
						 obj->clsId);

  if (pCoclassTypeInfo == NULL)
    scheme_signal_error ("Error getting coclass type information");

  // have type info for coclass
  // event type info is one of the "implemented" interfaces

  pEventTypeInfo = eventTypeInfoFromCoclassTypeInfo (pCoclassTypeInfo);

  pCoclassTypeInfo->Release();

  if (pEventTypeInfo == NULL)
    scheme_signal_error ("Error retrieving event type info");

  return pEventTypeInfo;
}

Scheme_Object *mx_com_events (int argc, Scheme_Object **argv)
{
  HRESULT hr;
  ITypeInfo *pEventTypeInfo;
  TYPEATTR *pEventTypeAttr;
  FUNCDESC *pFuncDesc;
  Scheme_Object *retval;
  UINT nameCount;
  BSTR bstr;
  UINT i;

  GUARANTEE_COM_OBJ_OR_TYPE ("com-events", 0);

  pEventTypeInfo =
      MX_COM_TYPEP (argv[0])
      ? eventTypeInfoFromComType ((MX_COM_Type *)argv[0])
      : (MX_COM_OBJ_VAL (argv[0]) == NULL)
      ? (scheme_signal_error ("com-events: NULL COM object"), (ITypeInfo *) NULL)
      : eventTypeInfoFromComObject ((MX_COM_Object *)argv[0]);

  // query for outbound interface info

  if (pEventTypeInfo == NULL)
    scheme_signal_error ("Can't find event type information");

  hr = pEventTypeInfo->GetTypeAttr (&pEventTypeAttr);

  if (FAILED (hr) || pEventTypeAttr == NULL)
    codedComError ("Error retrieving event type attributes", hr);

  retval = scheme_null;

  for (i = 0; i < pEventTypeAttr->cFuncs; i++) {
    pEventTypeInfo->GetFuncDesc (i, &pFuncDesc);
    pEventTypeInfo->GetNames (pFuncDesc->memid, &bstr, 1, &nameCount);
    retval = scheme_make_pair (BSTRToSchemeString (bstr), retval);
    SysFreeString (bstr);
  }

  pEventTypeInfo->ReleaseFuncDesc (pFuncDesc);
  pEventTypeInfo->ReleaseTypeAttr (pEventTypeAttr);

  return retval;
}


VARTYPE getVarTypeFromElemDesc (ELEMDESC * pElemDesc)
{

  unsigned short flags = pElemDesc->paramdesc.wParamFlags;

  return (flags & PARAMFLAG_FOPT) && (flags & PARAMFLAG_FHASDEFAULT)
      ? pElemDesc->paramdesc.pparamdescex->varDefaultValue.vt
      : pElemDesc->tdesc.vt == VT_PTR ? pElemDesc->tdesc.lptdesc->vt | VT_BYREF
      : pElemDesc->tdesc.vt;
}

Scheme_Object *elemDescToSchemeType (ELEMDESC *pElemDesc, BOOL ignoreByRef, BOOL isOpt)
{
  static char buff[256];
  char *s;
  BOOL isBox;
  VARTYPE vt;

  vt = getVarTypeFromElemDesc (pElemDesc);

  if (ignoreByRef)
    vt &= ~VT_BYREF;

  isBox = FALSE;

  switch (vt) {

  case VT_HRESULT :
  case VT_NULL :

    s = "void";
    break;

  case VT_UI1 :

    s = "char";
    break;

  case VT_UI1 | VT_BYREF :

    s = "char";
    isBox = TRUE;
    break;

  case VT_UI2 :

    s = "unsigned-short";
    break;

  case VT_UI2 | VT_BYREF :

    s = "unsigned-short";
    isBox = TRUE;
    break;

  case VT_UI4 :
  case VT_UINT :

    s = "unsigned-int";
    break;

  case VT_UI4 | VT_BYREF :
  case VT_UINT | VT_BYREF :

    s = "unsigned-int";
    isBox = TRUE;
    break;

  case VT_UI8 :

    s = "unsigned-long-long";
    break;

  case VT_UI8 | VT_BYREF :

    s = "unsigned-long-long";
    isBox = TRUE;
    break;

  case VT_I1 :

    s = "signed-char";
    break;

  case VT_I1 | VT_BYREF :

    s = "signed-char";
    isBox = TRUE;
    break;

  case VT_I2 :

    s = "short-int";
    break;

  case VT_I2 + VT_BYREF :

    s = "short-int";
    isBox = TRUE;
    break;

  case VT_I4 :
  case VT_INT :

    s = "int";
    break;

  case VT_I4 | VT_BYREF:
  case VT_INT | VT_BYREF:

    s = "int";
    isBox = TRUE;
    break;

  case VT_I8 :

    s = "long-long";
    break;

  case VT_I8 | VT_BYREF :

    s = "long-long";
    isBox = TRUE;
    break;

  case VT_R4 :

    s = "float";
    break;

  case VT_R4 | VT_BYREF :

    s = "float";
    isBox = TRUE;
    break;

  case VT_R8 :

    s = "double";
    break;

  case VT_R8 | VT_BYREF :

    s = "double";
    isBox = TRUE;
    break;

  case VT_BSTR :

    s = "string";
    break;

  case VT_BSTR | VT_BYREF :

    s = "string";
    isBox = TRUE;
    break;

  case VT_CY :

    s = "mx-currency";
    break;

  case VT_CY | VT_BYREF :

    s = "mx-currency";
    isBox = TRUE;
    break;

  case VT_DATE :

    s = "mx-date";
    break;

  case VT_DATE | VT_BYREF :

    s = "mx-date";
    isBox = TRUE;
    break;

  case VT_BOOL :

    s = "boolean";
    break;

  case VT_BOOL | VT_BYREF :

    s = "boolean";
    isBox = FALSE;
    break;

  case VT_ERROR :

    s = "mx-scode";
    break;

  case VT_ERROR | VT_BYREF:

    s = "mx-scode";
    isBox = TRUE;
    break;

  case VT_UNKNOWN :

    s = "mx-unknown-com-object";
    break;

  case VT_UNKNOWN | VT_BYREF :

    s = "mx-unknown-com-object";
    isBox = TRUE;
    break;

  case VT_DISPATCH :

    s = "com-object";
    break;

  case VT_DISPATCH | VT_BYREF :

    s = "com-object";
    isBox = TRUE;
    break;

  case VT_VARIANT :

    s = "mx-any";
    break;

  case VT_VARIANT | VT_BYREF :

    s = "mx-any";
    isBox = FALSE;  // Yes, FALSE.
    break;

  case VT_USERDEFINED :
    // Reporting this as `user-defined' is sure to confuse somebody.
    // The convention is that these are ENUMs that the caller and the
    // callee have agreed upon.  For our purposes, they will be INTs,
    // but we'll report them as an enumeration.
    // s = "user-defined";
    s = "com-enumeration";
    break;

  case VT_USERDEFINED | VT_BYREF:
    // Reporting this as `user-defined-box' is sure to confuse somebody.
    // The convention is that these represent specific COM interfaces
    // that the caller and callee have agreed upon.  For our purposes,
    // it is an IUnknown pointer.
    // s = "user-defined";
    // isBox = TRUE;
    s = "mx-unknown-com-object";
    break;

  case VT_VOID :
    s = "void";
    break;

  default :

    {
      char defaultBuff[32];
      sprintf (defaultBuff, "COM-0x%X", vt);
      return scheme_intern_symbol (defaultBuff);
    }
  }

  if (isBox) {
    if (isOpt)
      sprintf (buff, "%s-box-opt", s);
    else
      sprintf (buff, "%s-box", s);
  }
  else {
    if (isOpt)
      sprintf (buff, "%s-opt", s);
    else
      strcpy (buff, s);
  }

  return scheme_intern_exact_symbol (buff, (unsigned int)strlen (buff));
}

Scheme_Object * mx_make_function_type (Scheme_Object *paramTypes,
				       Scheme_Object *returnType)
{
  return
      scheme_append (paramTypes,
		     scheme_make_pair (scheme_intern_symbol ("->"),
				       scheme_make_pair (returnType,
							 scheme_null)));
}

BOOL isDefaultParam (FUNCDESC *pFuncDesc, short int i)
{
  unsigned short flags;

  if (pFuncDesc->lprgelemdescParam == NULL)
    return FALSE;

  flags = pFuncDesc->lprgelemdescParam[i].paramdesc.wParamFlags;
  return ((flags & PARAMFLAG_FOPT) && (flags & PARAMFLAG_FHASDEFAULT));
}

BOOL isOptionalParam (FUNCDESC *pFuncDesc, short int i)
{
  unsigned short flags;

  if (pFuncDesc->lprgelemdescParam == NULL)
    return FALSE;

  flags = pFuncDesc->lprgelemdescParam[i].paramdesc.wParamFlags;
  return (flags & PARAMFLAG_FOPT);
}

short getOptParamCount (FUNCDESC *pFuncDesc, short hi)
{
  short i;
  short numOptParams;

  numOptParams = 0;

  for (i = hi; i >= 0; i--)
    if (isOptionalParam (pFuncDesc, i))
      numOptParams++;

  return numOptParams;
}

BOOL isLastParamRetval (short int numParams,
		       INVOKEKIND invKind, FUNCDESC *pFuncDesc)
{
  return (numParams > 0 &&
	  (invKind == INVOKE_PROPERTYGET || invKind == INVOKE_FUNC)
	  &&
	  (pFuncDesc->lprgelemdescParam[numParams-1].paramdesc.wParamFlags
	  & PARAMFLAG_FRETVAL));
}


Scheme_Object *mx_do_get_method_type (int argc, Scheme_Object **argv,
				     INVOKEKIND invKind)
{
  MX_TYPEDESC *pTypeDesc;
  ITypeInfo* pITypeInfo;
  FUNCDESC *pFuncDesc;
  VARDESC *pVarDesc;
  Scheme_Object *s, *paramTypes, *returnType;
  const char *name;
  short int numActualParams;
  short int numOptParams;
  short int firstOptArg;
  short int hiBound;
  BOOL lastParamIsRetval;
  int i;

  GUARANTEE_COM_OBJ_OR_TYPE ("com-method-type", 0);


  if (MX_COM_OBJ_VAL (argv[0]) == NULL)
    scheme_signal_error ("NULL COM object");

  name = schemeToMultiByte (GUARANTEE_STRSYM ("com-method-type", 1));

  if (invKind == INVOKE_FUNC && isDispatchName (name))
    scheme_signal_error ("com-method-type: IDispatch methods not available");

  if (MX_COM_OBJP (argv[0]))
    pTypeDesc = getMethodType ((MX_COM_Object *)argv[0], name, invKind);

  else {
      pITypeInfo =
	  invKind == INVOKE_EVENT
	  ? eventTypeInfoFromComType ((MX_COM_Type *)argv[0])
	  : MX_COM_TYPE_VAL (argv[0]);
      pTypeDesc = typeDescFromTypeInfo (name, invKind, pITypeInfo);
      }

  // pTypeDesc may be NULL if there is no type info.

  if (pTypeDesc == NULL)
    return scheme_false;

  if (pTypeDesc->descKind == funcDesc) {
    pFuncDesc = pTypeDesc->funcdescs.pFuncDesc;

    paramTypes = scheme_null;

    numActualParams = pFuncDesc->cParams;

    if (pFuncDesc->cParamsOpt == -1) { // all args > pFuncDesc->cParams - 1 get packaged into SAFEARRAY

      // this branch is untested

      lastParamIsRetval = FALSE;
      paramTypes = scheme_make_pair (scheme_intern_symbol ("..."), paramTypes);
      for (i = numActualParams - 1; i >= 0; i--) {
	s = elemDescToSchemeType (&pFuncDesc->lprgelemdescParam[i], FALSE, FALSE);
	paramTypes = scheme_make_pair (s, paramTypes);
      }
    }
    else {
      lastParamIsRetval =
	isLastParamRetval (numActualParams, invKind, pFuncDesc);

      hiBound = numActualParams - (lastParamIsRetval ? 2 : 1);

      // parameters that are optional with a default value in IDL are not
      // counted in pFuncDesc->cParamsOpt, so look for default bit flag

      numOptParams = getOptParamCount (pFuncDesc, hiBound);

      firstOptArg = hiBound - numOptParams + 1;

      for (i = hiBound; i >= 0; i--) {
	s = elemDescToSchemeType (&pFuncDesc->lprgelemdescParam[i], FALSE, i >= firstOptArg);
	paramTypes = scheme_make_pair (s, paramTypes);
      }
    }
  }

  // if not a function type, distinguish varDesc's
  // by invKind

  else if (invKind == INVOKE_PROPERTYGET) {
    pVarDesc = pTypeDesc->pVarDesc;
    paramTypes = scheme_null;
    numActualParams = 0;
  }
  else if (invKind == INVOKE_PROPERTYPUT) {
    pVarDesc = pTypeDesc->pVarDesc;
    paramTypes =
      scheme_make_pair (elemDescToSchemeType (&pVarDesc->elemdescVar, FALSE, FALSE),
		       scheme_null);
    numActualParams = 1;
  }

  switch (invKind) {

  case INVOKE_FUNC :

    // if final parameter is marked as retval, use its type

    returnType = lastParamIsRetval
	? elemDescToSchemeType (&pFuncDesc->lprgelemdescParam[numActualParams-1], TRUE, FALSE)
	: elemDescToSchemeType (&pFuncDesc->elemdescFunc, TRUE, FALSE);

    break;

  case INVOKE_EVENT :
  case INVOKE_PROPERTYPUT :

    returnType = scheme_intern_symbol ("void");

    break;

  case INVOKE_PROPERTYGET :

    // pTypeDesc->descKind may be either funcDesc or varDesc

    if (pTypeDesc->descKind == funcDesc)

	returnType = (lastParamIsRetval == FALSE || pFuncDesc->cParams == 0)
	    ? elemDescToSchemeType (&pFuncDesc->elemdescFunc, TRUE, FALSE)
	    : elemDescToSchemeType (&pFuncDesc->lprgelemdescParam[numActualParams-1], TRUE, FALSE);

    else // pTypeDesc->descKind == varDesc
      returnType = elemDescToSchemeType (&pVarDesc->elemdescVar, TRUE, FALSE);

    break;
  }

  return mx_make_function_type (paramTypes, returnType);

}


Scheme_Object *mx_com_method_type (int argc, Scheme_Object **argv)
{
  return mx_do_get_method_type (argc, argv, INVOKE_FUNC);
}

Scheme_Object *mx_com_get_property_type (int argc, Scheme_Object **argv)
{
  return mx_do_get_method_type (argc, argv, INVOKE_PROPERTYGET);
}

Scheme_Object *mx_com_set_property_type (int argc, Scheme_Object **argv)
{
  return mx_do_get_method_type (argc, argv, INVOKE_PROPERTYPUT);
}

Scheme_Object *mx_com_event_type (int argc, Scheme_Object **argv)
{
  return mx_do_get_method_type (argc, argv, (INVOKEKIND)INVOKE_EVENT);
}

BOOL schemeValueFitsVarType (Scheme_Object *val, VARTYPE vt)
{
  long int longInt;
  unsigned long uLongInt;

  switch (vt) {

  case VT_NULL :

    return SCHEME_VOIDP (val);

  case VT_I1 :
  case VT_UI1 :

    return SCHEME_CHARP (val);

  case VT_I2 :

    return SCHEME_INTP (val) &&
      scheme_get_int_val (val, &longInt) &&
      longInt <= SHRT_MAX && longInt >= SHRT_MIN;

  case VT_UI2 :

    return SCHEME_INTP (val) &&
      scheme_get_unsigned_int_val (val, &uLongInt) &&
      uLongInt <= USHRT_MAX;

  case VT_I4 :
  case VT_INT :

    return SCHEME_EXACT_INTEGERP (val) &&
      scheme_get_int_val (val, &longInt);

  case VT_UI4 :
  case VT_UINT :

    return SCHEME_EXACT_INTEGERP (val) &&
      scheme_get_unsigned_int_val (val, &uLongInt);

  case VT_R4 :

    return SCHEME_FLTP (val) ||
      (SCHEME_DBLP (val) &&
       SCHEME_DBL_VAL (val) >= FLT_MIN &&
       SCHEME_DBL_VAL (val) <= FLT_MAX);

  case VT_R8 :

    return SCHEME_DBLP (val);

  case VT_BSTR :

    return SCHEME_STRSYMP (val);

  case VT_CY :

    return MX_CYP (val);

  case VT_DATE :

    return MX_DATEP (val);

  case VT_BOOL :

    return TRUE; // ain't Scheme great

  case VT_ERROR :

    return MX_SCODEP (val);

  case VT_UNKNOWN :

    return MX_IUNKNOWNP (val);

  case VT_DISPATCH :

    return MX_COM_OBJP (val);

  case VT_VARIANT : // we can package anything into a VARIANTARG

    return TRUE;

  case VT_USERDEFINED :
    return TRUE;

  default :

    return FALSE;

  }
}


BOOL subArrayFitsVarType (Scheme_Object *val,
			 unsigned short numDims, SAFEARRAYBOUND *bounds,
			 VARTYPE vt)
{
  Scheme_Object **els;
  unsigned long len;

  if (SCHEME_VECTORP (val) == FALSE)
    return FALSE;

  len = SCHEME_VEC_SIZE (val);

  if (len != bounds->cElements)
    return FALSE;

  els = SCHEME_VEC_ELS (val);

  if (numDims == 1) { // innermost vector
    for (unsigned long i = 0; i < len; i++)
      if (schemeValueFitsVarType (els[i], vt) == FALSE)
	return FALSE;
  }
  else {
    for (unsigned long i = 0; i < len; i++)
      // recursion, the programmer's best friend
      if (subArrayFitsVarType (els[i], numDims - 1, bounds + 1, vt) == FALSE)
	return FALSE;
  }

  return TRUE;
}

BOOL schemeValueFitsElemDesc (Scheme_Object *val, ELEMDESC *pElemDesc)
{
  unsigned short flags;

  // if default available, check value has appropriate type

  flags = pElemDesc->paramdesc.wParamFlags;
  if (flags & PARAMFLAG_FOPT) {
    if (val == mx_omit_obj)
      return TRUE;

    if (flags & PARAMFLAG_FHASDEFAULT)
      return schemeValueFitsVarType (val, pElemDesc->paramdesc.pparamdescex->varDefaultValue.vt);
  }

  // if array, check we have a vector of proper dimension and contained types

  if (pElemDesc->tdesc.vt & VT_ARRAY) {
    return subArrayFitsVarType (val,
			       pElemDesc->tdesc.lpadesc->cDims,
			       pElemDesc->tdesc.lpadesc->rgbounds,
			       pElemDesc->tdesc.lpadesc->tdescElem.vt);

  }


  // if box, check the contained value

  if (pElemDesc->tdesc.vt == VT_PTR) {
      // A VT_PTR to a VT_USERDEFINED isn't a box, it's
      // an IUnknown.
      return
	  (pElemDesc->tdesc.lptdesc->vt == VT_VARIANT) ? TRUE
	  : (pElemDesc->tdesc.lptdesc->vt == VT_USERDEFINED) ? (MX_COM_OBJP (val) || MX_IUNKNOWNP (val))
          : (SCHEME_BOXP (val)
             && schemeValueFitsVarType (SCHEME_BOX_VAL (val), pElemDesc->tdesc.lptdesc->vt));
      }

  // not array or box or default value
  return schemeValueFitsVarType (val, pElemDesc->tdesc.vt);
}

VARIANT_BOOL schemeValToBool (Scheme_Object *val)
{
  return SCHEME_FALSEP (val) ? 0 : 0xFFFF;
}

VARTYPE schemeValueToVarType (Scheme_Object *obj)
{

  // test for global constants
  if (SCHEME_FALSEP (obj))
    return VT_BOOL;

  if (SCHEME_VOIDP (obj))
    return VT_NULL;

  // handle fixnums
  if (SCHEME_INTP (obj))
    return VT_I4;

  // otherwise, dispatch on value type

  switch (obj->type) {
  case scheme_char_type :
    return VT_UI1;
  case scheme_integer_type :
    return VT_I4;
  case scheme_float_type :
    return VT_R4;
  case scheme_double_type :
    return VT_R8;
  case scheme_symbol_type :
  case scheme_char_string_type :
  case scheme_byte_string_type :
    return VT_BSTR;
  case scheme_vector_type : // may need to specify elt type
    return VT_ARRAY;
  }

  scheme_signal_error ("Unable to coerce value to VARIANT");

  return 0; // keep compiler happy
}

void *allocParamMemory (size_t n)
{
  void *retval;

  // do we need a semaphore here?

  retval = scheme_malloc (n);
  scheme_dont_gc_ptr (retval);
  return retval;
}

void marshalSchemeValueToVariant (Scheme_Object *val, VARIANTARG *pVariantArg)
{

  // called when COM type spec allows any VARIANT
  // or when COM type spec is unknown

  if (SCHEME_CHARP (val)) {
    pVariantArg->vt = VT_UI1;
    pVariantArg->bVal = SCHEME_CHAR_VAL (val);
  }

  else if (SCHEME_EXACT_INTEGERP (val)) {
    pVariantArg->vt = VT_I4;
    scheme_get_int_val (val, &pVariantArg->lVal);
  }

#ifdef MZ_USE_SINGLE_FLOATS
  else if (SCHEME_FLTP (val)) {
    pVariantArg->vt = VT_R4;
    pVariantArg->fltVal = SCHEME_FLT_VAL (val);
  }
#endif

  else if (SCHEME_DBLP (val)) {
    pVariantArg->vt = VT_R8;
    pVariantArg->dblVal = SCHEME_DBL_VAL (val);
  }

  else if (SCHEME_STRSYMP (val)) {
    pVariantArg->vt = VT_BSTR;
    pVariantArg->bstrVal = schemeToBSTR (val);
  }

  else if (MX_CYP (val)) {
    pVariantArg->vt = VT_CY;
    pVariantArg->cyVal = MX_CY_VAL (val);
  }

  else if (MX_DATEP (val)) {
    pVariantArg->vt = VT_DATE;
    pVariantArg->date = MX_DATE_VAL (val);
  }

  else if (val == scheme_false) {
    pVariantArg->vt = VT_BOOL;
    pVariantArg->boolVal = 0;
  }

  else if (val == scheme_true) {
    pVariantArg->vt = VT_BOOL;
    pVariantArg->boolVal = -1;
  }

  else if (MX_SCODEP (val)) {
    pVariantArg->vt = VT_ERROR;
    pVariantArg->scode = MX_SCODE_VAL (val);
  }

  else if (MX_COM_OBJP (val)) {
    pVariantArg->pdispVal = MX_COM_OBJ_VAL (val);
    pVariantArg->vt = VT_DISPATCH;
  }

  else if (MX_IUNKNOWNP (val)) {
    pVariantArg->vt = VT_UNKNOWN;
    pVariantArg->punkVal = MX_IUNKNOWN_VAL (val);
  }

  else if (SCHEME_VECTORP (val)) {
    pVariantArg->vt = VT_ARRAY | VT_VARIANT;
    pVariantArg->parray = schemeVectorToSafeArray (val);
  }

  else if (scheme_apply (mx_marshal_raw_scheme_objects, 0, NULL) == scheme_false)
      scheme_signal_error ("Unable to inject Scheme value %V into VARIANT", val);
  else {
      pVariantArg->vt = VT_INT;
      pVariantArg->intVal = PtrToInt (val);
      }
  return;
}

void marshalSchemeValue (Scheme_Object *val, VARIANTARG *pVariantArg)
{
  char errBuff[128];

  if (pVariantArg->vt & VT_ARRAY)
    pVariantArg->parray = schemeVectorToSafeArray (val);

  switch (pVariantArg->vt) {

  case VT_NULL :
    break;

  case VT_I1 :
    pVariantArg->cVal = SCHEME_CHAR_VAL (val);
    break;

  case VT_I1 | VT_BYREF :
    pVariantArg->pcVal =
      (char *)allocParamMemory (sizeof (char));
    *pVariantArg->pcVal = SCHEME_CHAR_VAL (SCHEME_BOX_VAL (val));
    break;

  case VT_UI1 :
    pVariantArg->bVal = SCHEME_CHAR_VAL (val);
    break;

  case VT_UI1 | VT_BYREF :
    pVariantArg->pbVal = (unsigned char *)allocParamMemory (sizeof (unsigned char));
    *pVariantArg->pbVal = (unsigned char)SCHEME_CHAR_VAL (SCHEME_BOX_VAL (val));
    break;

  case VT_I2 :
    pVariantArg->iVal = (short)SCHEME_INT_VAL (val);
    break;

  case VT_I2 | VT_BYREF :
    pVariantArg->piVal = (short *)allocParamMemory (sizeof (short));
    *pVariantArg->piVal = (short)SCHEME_INT_VAL (SCHEME_BOX_VAL (val));
    break;

  case VT_UI2 :
    pVariantArg->uiVal = (unsigned short)SCHEME_INT_VAL (val);
    break;

  case VT_UI2 | VT_BYREF :
    pVariantArg->puiVal = (unsigned short *)allocParamMemory (sizeof (unsigned short));
    *pVariantArg->puiVal = (unsigned short)SCHEME_INT_VAL (SCHEME_BOX_VAL (val));
    break;

  case VT_I4 :
    pVariantArg->lVal = SCHEME_INT_VAL (val);
    break;

  case VT_I4 | VT_BYREF :
    pVariantArg->plVal = (long *)allocParamMemory (sizeof (long));
    *pVariantArg->plVal = (long)SCHEME_INT_VAL (SCHEME_BOX_VAL (val));
    break;

  case VT_UI4 :
    pVariantArg->ulVal = SCHEME_INT_VAL (val);
    break;

  case VT_UI4 | VT_BYREF :
    pVariantArg->pulVal = (unsigned long *)allocParamMemory (sizeof (unsigned long));
    *pVariantArg->pulVal = (unsigned long)SCHEME_INT_VAL (SCHEME_BOX_VAL (val));
    break;

  case VT_INT :
    pVariantArg->intVal = SCHEME_INT_VAL (val);
    break;

  case VT_INT | VT_BYREF :
    pVariantArg->pintVal = (int *)allocParamMemory (sizeof (long));
    *pVariantArg->pintVal = (int)SCHEME_INT_VAL (SCHEME_BOX_VAL (val));
    break;

  case VT_UINT :
    pVariantArg->uintVal = SCHEME_INT_VAL (val);
    break;

  case VT_UINT | VT_BYREF :
    pVariantArg->puintVal = (unsigned int *)allocParamMemory (sizeof (long));
    *pVariantArg->puintVal = (unsigned int)SCHEME_INT_VAL (SCHEME_BOX_VAL (val));
    break;

    // VT_USERDEFINED in the typeDesc indicates an ENUM,
    // but VT_USERDEFINED is illegal to use in the DISPPARAMS.
    // The right thing to do is pass it as an INT.  Note that
    // we have to bash out the variant tag.
    //  ** NOTE THAT VT_USERDEFINED | VT_BYREF IS NOT
    //  ** A REFERENCE TO AN INT
  case VT_USERDEFINED:
    pVariantArg->vt = VT_INT;
    pVariantArg->intVal = SCHEME_INT_VAL (val);
    break;

  case VT_R4 :
    pVariantArg->fltVal = (float)SCHEME_DBL_VAL (val);
    break;

  case VT_R4 | VT_BYREF :
    pVariantArg->pfltVal = (float *)allocParamMemory (sizeof (float));
    *pVariantArg->pfltVal = (float)SCHEME_DBL_VAL (SCHEME_BOX_VAL (val));
    break;

  case VT_R8 :
    pVariantArg->dblVal = SCHEME_DBL_VAL (val);
    break;

  case VT_R8 | VT_BYREF :
    pVariantArg->pdblVal = (double *)allocParamMemory (sizeof (double));
    *pVariantArg->pdblVal = (double)SCHEME_DBL_VAL (SCHEME_BOX_VAL (val));
    break;

  case VT_BSTR :
    pVariantArg->bstrVal = schemeToBSTR (val);
    break;

  case VT_BSTR | VT_BYREF :
    pVariantArg->pbstrVal = (BSTR *)allocParamMemory (sizeof (BSTR));
    *pVariantArg->pbstrVal = schemeToBSTR (val);
    break;

  case VT_CY :
    pVariantArg->cyVal = MX_CY_VAL (val);
    break;

  case VT_CY | VT_BYREF :
    pVariantArg->pcyVal = (CY *)allocParamMemory (sizeof (CY));
    *pVariantArg->pcyVal = (CY)MX_CY_VAL (val);
    break;

  case VT_DATE :
    pVariantArg->date = MX_DATE_VAL (val);
    break;

  case VT_DATE | VT_BYREF :
    pVariantArg->pdate = (DATE *)allocParamMemory (sizeof (DATE));
    *pVariantArg->pdate = (DATE)MX_DATE_VAL (val);
    break;

  case VT_BOOL :
    pVariantArg->boolVal = schemeValToBool (val);
    break;

  case VT_BOOL | VT_BYREF :
    pVariantArg->pboolVal = (VARIANT_BOOL *)allocParamMemory (sizeof (VARIANT_BOOL));
    *pVariantArg->pboolVal = schemeValToBool (val);
    break;

  case VT_ERROR :
    pVariantArg->scode = MX_SCODE_VAL (val);
    break;

  case VT_ERROR | VT_BYREF :
    pVariantArg->pscode = (SCODE *)allocParamMemory (sizeof (SCODE));
    *pVariantArg->pscode = MX_SCODE_VAL (SCHEME_BOX_VAL (val));
    break;

  case VT_DISPATCH :
    pVariantArg->pdispVal = MX_COM_OBJ_VAL (val);
    break;

  case VT_DISPATCH | VT_BYREF :
    pVariantArg->ppdispVal = (IDispatch **)allocParamMemory (sizeof (IDispatch *));
    *pVariantArg->ppdispVal = MX_COM_OBJ_VAL (SCHEME_BOX_VAL (val));
    break;

    // VT_USERDEFINED | VT_BYREF indicates that we should pass
    // the IUnknown pointer of a COM object.
    // VT_USERDEFINED | VT_BYREF is illegal in the DISPPARAMS, so
    // we bash it out to VT_UNKNOWN.

  case VT_USERDEFINED | VT_BYREF :
      pVariantArg->vt = VT_UNKNOWN;

      if (MX_COM_OBJP (val))
          // shouldn't fail
          MX_COM_OBJ_VAL (val)->QueryInterface (IID_IUnknown, (void **)&pVariantArg->punkVal);

      else if (MX_IUNKNOWNP (val))
          pVariantArg->punkVal = MX_COM_OBJ_VAL (val);
      // should never happen
      else
          scheme_signal_error ("Attempt to marshal non-com object into VT_USERDEFINED");
      break;

  case VT_VARIANT | VT_BYREF :
    // pass boxed value of almost-arbitrary type
    pVariantArg->pvarVal = (VARIANTARG *) allocParamMemory (sizeof (VARIANTARG));
    pVariantArg->pvarVal->vt = schemeValueToVarType (val);
    marshalSchemeValue (val, pVariantArg->pvarVal);
    break;

  case VT_UNKNOWN :
    pVariantArg->punkVal = MX_IUNKNOWN_VAL (val);
    break;

  case VT_UNKNOWN | VT_BYREF :
    pVariantArg->ppunkVal = (IUnknown **)allocParamMemory (sizeof (IUnknown *));
    *pVariantArg->ppunkVal = MX_IUNKNOWN_VAL (SCHEME_BOX_VAL (val));
    break;

  case VT_VARIANT :
    marshalSchemeValueToVariant (val, pVariantArg);
    break;

  case VT_PTR:
    scheme_signal_error ("unable to marshal VT_PTR");
    break;

  default :
    sprintf (errBuff, "Unable to marshal Scheme value into VARIANT: 0x%X",
	    pVariantArg->vt);
    scheme_signal_error (errBuff);

  }
}

Scheme_Object *variantToSchemeObject (VARIANTARG *pVariantArg)
{
  char errBuff[128];

  if (pVariantArg->vt & VT_ARRAY)
    return safeArrayToSchemeVector (pVariantArg->parray);

  switch (pVariantArg->vt) {

  case VT_EMPTY :
  case VT_NULL :
    return scheme_void;

  case VT_I1 :
    return scheme_make_character (pVariantArg->cVal);

  case VT_I2 :
    return scheme_make_integer_value (pVariantArg->iVal);

  case VT_I4 :
    return scheme_make_integer (pVariantArg->lVal);

  case VT_I8 :
    return scheme_make_integer_value_from_long_long (pVariantArg->llVal);

  case VT_UI1 :
    return scheme_make_character ((char) (pVariantArg->bVal));

  case VT_UI2 :
    return scheme_make_integer (pVariantArg->uiVal);

  case VT_UI4 :
    return scheme_make_integer_value_from_unsigned (pVariantArg->ulVal);

  case VT_UI8 :
    return scheme_make_integer_value_from_unsigned_long_long (pVariantArg->ullVal);

  case VT_INT :
    return scheme_make_integer (pVariantArg->intVal);

  case VT_UINT :
    return scheme_make_integer_value_from_unsigned (pVariantArg->uintVal);

  case VT_R4 :
#ifdef MZ_USE_SINGLE_FLOATS
    return scheme_make_float (pVariantArg->fltVal);
#else
    return scheme_make_double ((double) (pVariantArg->fltVal));
#endif

  case VT_R8 :
    return scheme_make_double (pVariantArg->dblVal);

  case VT_BSTR :
    return unmarshalBSTR (pVariantArg->bstrVal);

  case VT_CY :
    return mx_make_cy (&pVariantArg->cyVal);

  case VT_DATE :
    return mx_make_date (&pVariantArg->date);

  case VT_BOOL :
    return mx_make_bool (pVariantArg->boolVal);

  case VT_ERROR :
    return mx_make_scode (pVariantArg->scode);

  case VT_DISPATCH :
    return mx_make_idispatch (pVariantArg->pdispVal);

  case VT_UNKNOWN :
    return mx_make_iunknown (pVariantArg->punkVal);

  default :
    sprintf (errBuff, "Can't make Scheme value from VARIANT 0x%X",
	    pVariantArg->vt);
    scheme_signal_error (errBuff);

  }

  return NULL;
}

// we need this for direct calls, where the return value
// is created by passing as a C pointer, which is stored in a VARIANTARG
Scheme_Object *retvalVariantToSchemeObject (VARIANTARG *pVariantArg)
{
  switch (pVariantArg->vt) {
  case VT_HRESULT :
  case VT_VOID :
    return scheme_void;
  case VT_BYREF|VT_UI1 :
    return scheme_make_character (*pVariantArg->pcVal);
  case VT_BYREF|VT_I2 :
    return scheme_make_integer (*pVariantArg->piVal);
  case VT_BYREF|VT_I4 :
    return scheme_make_integer_value (*pVariantArg->plVal);
  case VT_BYREF|VT_I8 :
    return
      scheme_make_integer_value_from_long_long (*pVariantArg->pllVal);
  case VT_BYREF|VT_R4 :
#ifdef MZ_USE_SINGLE_FLOATS
    return scheme_make_float (*pVariantArg->pfltVal);
#else
    return scheme_make_double ((double) (*pVariantArg->pfltVal));
#endif
  case VT_BYREF|VT_R8 :
    return scheme_make_double (*pVariantArg->pdblVal);
  case VT_BYREF|VT_BOOL :
    return mx_make_bool (*pVariantArg->pboolVal);
  case VT_BYREF|VT_ERROR :
    return mx_make_scode (*pVariantArg->pscode);
  case VT_BYREF|VT_CY :
    return mx_make_cy (pVariantArg->pcyVal);
  case VT_BYREF|VT_DATE :
    return mx_make_date (pVariantArg->pdate);
  case VT_BYREF|VT_BSTR :
    return unmarshalBSTR (*pVariantArg->pbstrVal);
  case VT_BYREF|VT_UNKNOWN :
    return mx_make_iunknown (*pVariantArg->ppunkVal);
  case VT_BYREF|VT_PTR :
  case VT_BYREF|VT_DISPATCH :
    return mx_make_idispatch (*pVariantArg->ppdispVal);
  case VT_BYREF|VT_SAFEARRAY :
  case VT_BYREF|VT_ARRAY :
    return safeArrayToSchemeVector (*pVariantArg->pparray);
  case VT_BYREF|VT_VARIANT :
    return variantToSchemeObject (pVariantArg->pvarVal);
  case VT_BYREF|VT_I1 :
    return scheme_make_character (*pVariantArg->pcVal);
  case VT_BYREF|VT_UI2 :
    return scheme_make_integer_value_from_unsigned (*pVariantArg->puiVal);
  case VT_BYREF|VT_UI4 :
    return scheme_make_integer_value_from_unsigned (*pVariantArg->pulVal);
  case VT_BYREF|VT_UI8 :
    return
      scheme_make_integer_value_from_unsigned_long_long (*pVariantArg->pullVal);
  case VT_BYREF|VT_INT :
    return scheme_make_integer_value (*pVariantArg->pintVal);
  case VT_BYREF|VT_UINT :
    return scheme_make_integer_value_from_unsigned (*pVariantArg->puintVal);
  default :
    {char buff[128];
    sprintf (buff, "Can't create return value for VARIANT 0x%X", pVariantArg->vt);
    scheme_signal_error (buff); }
  }
  return NULL;
}

void unmarshalVariant (Scheme_Object *val, VARIANTARG *pVariantArg)
{

  switch (pVariantArg->vt) {

  case VT_I1 | VT_BYREF :
    SCHEME_BOX_VAL (val) = scheme_make_character (*pVariantArg->pcVal);
    scheme_gc_ptr_ok (pVariantArg->pcVal);
    break;

  case VT_UI1 | VT_BYREF :
    SCHEME_BOX_VAL (val) = scheme_make_character ((char) (*pVariantArg->pbVal));
    scheme_gc_ptr_ok (pVariantArg->pbVal);
    break;

  case VT_I2 | VT_BYREF :
    SCHEME_BOX_VAL (val) = scheme_make_integer (*pVariantArg->piVal);
    scheme_gc_ptr_ok (pVariantArg->piVal);
    break;

  case VT_UI2 | VT_BYREF :
    SCHEME_BOX_VAL (val) =
      scheme_make_integer_value_from_unsigned (*pVariantArg->puiVal);
    scheme_gc_ptr_ok (pVariantArg->puiVal);
    break;

  case VT_I4 | VT_BYREF :
    SCHEME_BOX_VAL (val) = scheme_make_integer_value (*pVariantArg->plVal);
    scheme_gc_ptr_ok (pVariantArg->plVal);
    break;

  case VT_UI4 | VT_BYREF :
    SCHEME_BOX_VAL (val) =
      scheme_make_integer_value_from_unsigned (*pVariantArg->pulVal);
    scheme_gc_ptr_ok (pVariantArg->pulVal);
    break;

  case VT_INT | VT_BYREF :
    SCHEME_BOX_VAL (val) = scheme_make_integer_value (*pVariantArg->pintVal);
    scheme_gc_ptr_ok (pVariantArg->pintVal);
    break;

  case VT_UINT | VT_BYREF :
    SCHEME_BOX_VAL (val) =
      scheme_make_integer_value_from_unsigned (*pVariantArg->puintVal);
    scheme_gc_ptr_ok (pVariantArg->puintVal);
    break;

  case VT_R4 | VT_BYREF :
#ifdef MZ_USE_SINGLE_FLOATS
    SCHEME_BOX_VAL (val) = scheme_make_float (*pVariantArg->pfltVal);
#else
    SCHEME_BOX_VAL (val) = scheme_make_double ((double) (*pVariantArg->pfltVal));
#endif
    scheme_gc_ptr_ok (pVariantArg->pfltVal);
    break;

  case VT_R8 | VT_BYREF :
    SCHEME_BOX_VAL (val) = scheme_make_double (*pVariantArg->pdblVal);
    scheme_gc_ptr_ok (pVariantArg->pdblVal);
    break;

  case VT_CY | VT_BYREF :
    SCHEME_BOX_VAL (val) = mx_make_cy (pVariantArg->pcyVal);
    scheme_gc_ptr_ok (pVariantArg->pcyVal);
    break;

  case VT_DATE | VT_BYREF :
    SCHEME_BOX_VAL (val) = mx_make_date (pVariantArg->pdate);
    scheme_gc_ptr_ok (pVariantArg->pdate);
    break;

  case VT_BOOL | VT_BYREF :
    SCHEME_BOX_VAL (val) = mx_make_bool (*pVariantArg->pboolVal);
    scheme_gc_ptr_ok (pVariantArg->pboolVal);
    break;

  case VT_ERROR | VT_BYREF :
    SCHEME_BOX_VAL (val) = mx_make_scode (*pVariantArg->pscode);
    scheme_gc_ptr_ok (pVariantArg->pscode);
    break;

  case VT_DISPATCH | VT_BYREF :
    SCHEME_BOX_VAL (val) = mx_make_idispatch (*pVariantArg->ppdispVal);
    scheme_gc_ptr_ok (pVariantArg->ppdispVal);
    break;

  case VT_UNKNOWN | VT_BYREF :
    SCHEME_BOX_VAL (val) = mx_make_iunknown (*pVariantArg->ppunkVal);
    scheme_gc_ptr_ok (pVariantArg->ppunkVal);
    break;

  case VT_VARIANT | VT_BYREF :
    scheme_gc_ptr_ok (pVariantArg->pvarVal);
    break;

  case VT_BSTR :
    // Don't try to update symbols!
    if (!SCHEME_SYMBOLP (val))
        updateSchemeFromBSTR (val, pVariantArg->bstrVal);
    SysFreeString (pVariantArg->bstrVal);
    break;

  case VT_BSTR | VT_BYREF :
    SCHEME_BOX_VAL (val) = unmarshalBSTR (*pVariantArg->pbstrVal);
    SysFreeString (*pVariantArg->pbstrVal);
    scheme_gc_ptr_ok (pVariantArg->pbstrVal);
    break;

  default :

    ;

    // no unmarshaling or cleanup needed

  }
}

// Build the DISPPARAMS by filling out the fields
// according to the Scheme type of object.
// No optional or named args, no type checking.
short int buildMethodArgumentsUsingDefaults (INVOKEKIND invKind,
                                             int argc, Scheme_Object **argv,
                                             DISPPARAMS *methodArguments)
{
  short int numParamsPassed;
  BOOL lastParamIsRetval;
  int i, j, k;
  static DISPID dispidPropPut = DISPID_PROPERTYPUT;

  // First argument is object, second is name of method.
  numParamsPassed = argc - 2;

  // Need a return value if property get or invoking a function.
  lastParamIsRetval = (invKind == INVOKE_PROPERTYGET || invKind == INVOKE_FUNC);

  switch (invKind) {

  case INVOKE_PROPERTYPUT :

    // Named argument represents the assigned value

    methodArguments->rgdispidNamedArgs = &dispidPropPut;
    methodArguments->cNamedArgs = methodArguments->cArgs = 1;
    methodArguments->cArgs = numParamsPassed;
    break;

  case INVOKE_PROPERTYGET :

    methodArguments->rgdispidNamedArgs = NULL;
    methodArguments->cNamedArgs = 0;
    methodArguments->cArgs = numParamsPassed;
    break;

  default :

    methodArguments->rgdispidNamedArgs = NULL;
    methodArguments->cNamedArgs = 0;
    methodArguments->cArgs = numParamsPassed;
    break;
  }

  if (numParamsPassed > 0) {
    methodArguments->rgvarg =
      (VARIANTARG *)scheme_malloc (numParamsPassed * sizeof (VARIANTARG));
    scheme_dont_gc_ptr (methodArguments->rgvarg);
  }

  // marshal Scheme argument list into COM argument list
  // arguments are in reverse order in rgvarg

  for (i = 0, j = numParamsPassed - 1, k = 2; i < argc - 2; i++, j--, k++) {

    // i = index of ELEMDESC's
    // j = index of VARIANTARG's

    VariantInit (&methodArguments->rgvarg[j]);

    if (argv[k] == mx_omit_obj) { // omitted argument
      methodArguments->rgvarg[j].vt = VT_ERROR;
      methodArguments->rgvarg[j].lVal = DISP_E_PARAMNOTFOUND;
    }
    else
      marshalSchemeValueToVariant (argv[k], &methodArguments->rgvarg[j]);
  }

  return numParamsPassed;
}

short int getLcidParamIndex (FUNCDESC *pFuncDesc, short int numParams)
{
  ELEMDESC *pElemDescs;
  int i;

  pElemDescs = pFuncDesc->lprgelemdescParam;
  for (i = 0; i < numParams; i++) {
    if (pElemDescs[i].paramdesc.wParamFlags & PARAMFLAG_FLCID)
      return i;
  }
  return NO_LCID;
}

void checkArgTypesAndCounts (FUNCDESC *pFuncDesc,
			    BOOL direct,
			    INVOKEKIND invKind,
			    int argc, Scheme_Object **argv,
			    MX_ARGS_COUNT *argsCount)
{
  char errBuff[256];
  short int numParamsPassed;
  short int numOptParams;
  short int lcidIndex;
  int i, j, k;

  numParamsPassed = pFuncDesc->cParams;

  argsCount->retvalInParams =
    isLastParamRetval (numParamsPassed, invKind, pFuncDesc);

  if (argsCount->retvalInParams)
    numParamsPassed--;

  numOptParams = getOptParamCount (pFuncDesc, numParamsPassed - 1);

  lcidIndex = NO_LCID;
  if (direct) {
    lcidIndex = getLcidParamIndex (pFuncDesc, numParamsPassed);
    if (lcidIndex != NO_LCID)
      numParamsPassed--;
  }
  argsCount->lcidIndex = lcidIndex;

  argsCount->numParamsPassed = numParamsPassed;
  argsCount->numOptParams = numOptParams;

  if (pFuncDesc->cParamsOpt == -1) {  // last args get packaged into SAFEARRAY

    // this branch is untested

    // optional parameters with default values not counted in pFuncDesc->cParamsOpt

    if (argc < numParamsPassed + 2 - 1) {
      sprintf (errBuff, "%s (%s \"%s\")",
	      mx_fun_string (invKind),
	      inv_kind_string (invKind),
	      schemeToText (argv[1]));
      scheme_wrong_count (errBuff, numParamsPassed-1, -1, argc-2, argv+2);
    }
  }
  else {

    // optional parameters with default values
    // not counted in pFuncDesc->cParamsOpt

    if (argc < numParamsPassed - numOptParams + 2 ||  // too few
	argc > numParamsPassed + 2) {  // too many
      sprintf (errBuff, "%s (%s \"%s\")",
	      mx_fun_string (invKind),
	      inv_kind_string (invKind),
	      schemeToText (argv[1]));
      scheme_wrong_count (errBuff, numParamsPassed-numOptParams, numParamsPassed, argc-2, argv+2);
    }
  }

  // compare types of actual arguments to prescribed types

  for (i = 0, j = 2, k = 0; i < argc - 2; i++, j++, k++) {

    // i = index of ELEMDESC's
    // j = index of actual args in argv

    if (direct && k == lcidIndex) // skip an entry
      k++;

    if (schemeValueFitsElemDesc (argv[j], &pFuncDesc->lprgelemdescParam[k]) == FALSE) {
      sprintf (errBuff, "%s (%s \"%s\")", mx_fun_string (invKind),
	      inv_kind_string (invKind), schemeToText (argv[1]));
      scheme_wrong_type (errBuff,
			SCHEME_SYM_VAL (elemDescToSchemeType (&(pFuncDesc->lprgelemdescParam[k]), FALSE, FALSE)),
			j, argc, argv);
    }
  }
}

short int buildMethodArgumentsUsingFuncDesc (FUNCDESC *pFuncDesc,
					    INVOKEKIND invKind,
					    int argc, Scheme_Object **argv,
					    DISPPARAMS *methodArguments)
{
  MX_ARGS_COUNT argsCount;
  short int numParamsPassed;
  short int numOptParams;
  static DISPID dispidPropPut = DISPID_PROPERTYPUT;
  int i, j, k;

  checkArgTypesAndCounts (pFuncDesc, FALSE, // indirect
			 invKind, argc, argv, &argsCount);
  numParamsPassed = argsCount.numParamsPassed;
  numOptParams = argsCount.numOptParams;

  switch (invKind) {

  case INVOKE_PROPERTYPUT :

    // Named argument represents the assigned value

    methodArguments->rgdispidNamedArgs = &dispidPropPut;
    methodArguments->cNamedArgs = methodArguments->cArgs = 1;
    methodArguments->cArgs = numParamsPassed;
    break;

  case INVOKE_PROPERTYGET :

    methodArguments->rgdispidNamedArgs = NULL;
    methodArguments->cNamedArgs = 0;
    methodArguments->cArgs = numParamsPassed;
    break;

  default :

    methodArguments->rgdispidNamedArgs = NULL;
    methodArguments->cNamedArgs = 0;
    methodArguments->cArgs = numParamsPassed;
    break;
  }

  if (numParamsPassed > 0) {
    methodArguments->rgvarg =
      (VARIANTARG *)scheme_malloc (numParamsPassed * sizeof (VARIANTARG));
    scheme_dont_gc_ptr (methodArguments->rgvarg);
  }

  // marshal Scheme argument list into COM argument list
  // arguments are in reverse order in rgvarg

  for (i = 0, j = numParamsPassed - 1, k = 2; i < argc - 2; i++, j--, k++) {

    // i = index of ELEMDESC's
    // j = index of VARIANTARG's

    VariantInit (&methodArguments->rgvarg[j]);

    if (argv[k] == mx_omit_obj) { // omitted argument
      methodArguments->rgvarg[j].vt = VT_ERROR;
      methodArguments->rgvarg[j].lVal = DISP_E_PARAMNOTFOUND;
    }
    else {
      methodArguments->rgvarg[j].vt =
	getVarTypeFromElemDesc (&pFuncDesc->lprgelemdescParam[i]);
      marshalSchemeValue (argv[k], &methodArguments->rgvarg[j]);
    }
  }

  // omitted optional arguments
  // supply default if available

  if (numOptParams > 0) {
    for (i = argc - 2, j = numParamsPassed - 1 - (argc - 2); j >= 0; i++, j--) {
      if (isDefaultParam (pFuncDesc, i))
	methodArguments->rgvarg[j] =
	  pFuncDesc->lprgelemdescParam[i].paramdesc.pparamdescex->varDefaultValue;
      else {
	VariantInit (&methodArguments->rgvarg[j]);
	methodArguments->rgvarg[j].vt = VT_ERROR;
	methodArguments->rgvarg[j].lVal = DISP_E_PARAMNOTFOUND;
      }
    }
  }

  return numParamsPassed;
}

short int buildMethodArgumentsUsingVarDesc (VARDESC *pVarDesc,
					   INVOKEKIND invKind,
					   int argc, Scheme_Object **argv,
					   DISPPARAMS *methodArguments)
{
  char errBuff[256];
  short int numParamsPassed;
  int i, j, k;
  static DISPID dispidPropPut = DISPID_PROPERTYPUT;

  numParamsPassed =
      (invKind == INVOKE_PROPERTYGET) ? 0
      : (invKind == INVOKE_PROPERTYPUT) ? 1
      : 0;

  if (argc != numParamsPassed + 2) {
    sprintf (errBuff, "%s (%s \"%s\")",
	    mx_fun_string (invKind),
	    inv_kind_string (invKind),
	    schemeToText (argv[1]));
    scheme_wrong_count (errBuff,
		       numParamsPassed + 2, numParamsPassed + 2,
		       argc, argv);
  }

  switch (invKind) {

  case INVOKE_PROPERTYPUT :

    // check that value is of expected type

    if (schemeValueFitsElemDesc (argv[2],
				&pVarDesc->elemdescVar) == FALSE) {
      sprintf (errBuff, "%s (%s \"%s\")", mx_fun_string (invKind),
	      inv_kind_string (invKind), schemeToText (argv[1]));
      scheme_wrong_type (errBuff,
			SCHEME_SYM_VAL (elemDescToSchemeType (&(pVarDesc->elemdescVar), FALSE, FALSE)), 2, argc, argv);
    }

    methodArguments->rgdispidNamedArgs = &dispidPropPut;
    methodArguments->cNamedArgs = methodArguments->cArgs = 1;

    break;


  case INVOKE_PROPERTYGET :

    methodArguments->rgdispidNamedArgs = NULL;
    methodArguments->cNamedArgs = 0;
    methodArguments->cArgs = numParamsPassed;
    break;

  }

  if (numParamsPassed > 0) {
    methodArguments->rgvarg =
      (VARIANTARG *)scheme_malloc (numParamsPassed * sizeof (VARIANTARG));
    scheme_dont_gc_ptr (methodArguments->rgvarg);
  }

  // marshal Scheme argument list into COM argument list

  for (i = 0, j = numParamsPassed - 1, k = 2; i < numParamsPassed; i++, j--, k++) {

    // i = index of ELEMDESC's
    // j = index of VARIANTARG's

    VariantInit (&methodArguments->rgvarg[j]);

    methodArguments->rgvarg[j].vt =
      getVarTypeFromElemDesc (&pVarDesc->elemdescVar);
    marshalSchemeValue (argv[k], &methodArguments->rgvarg[j]);
  }

  return numParamsPassed;
}

short int buildMethodArguments (MX_TYPEDESC *pTypeDesc,
			       INVOKEKIND invKind,
			       int argc, Scheme_Object **argv,
			       DISPPARAMS *methodArguments)
{
    return (pTypeDesc == NULL)
        ? buildMethodArgumentsUsingDefaults (invKind, argc, argv,
                                             methodArguments)
        : (pTypeDesc->descKind == funcDesc)
        ? buildMethodArgumentsUsingFuncDesc (pTypeDesc->funcdescs.pFuncDesc,
                                             invKind, argc, argv,
                                             methodArguments)
        : buildMethodArgumentsUsingVarDesc (pTypeDesc->pVarDesc,
                                            invKind, argc, argv,
                                            methodArguments);
}

void allocateDirectRetval (VARIANT *va)
{
  switch (va->vt) {
  case VT_BYREF|VT_UI1 :
    va->pbVal = (BYTE *)allocParamMemory (sizeof (BYTE));
    break;
  case VT_BYREF|VT_I2 :
    va->piVal = (SHORT *)allocParamMemory (sizeof (SHORT));
    break;
  case VT_BYREF|VT_I4 :
    va->plVal = (LONG *)allocParamMemory (sizeof (LONG));
    break;
  case VT_BYREF|VT_I8 :
    va->pllVal = (LONGLONG *)allocParamMemory (sizeof (LONGLONG));
    break;
  case VT_BYREF|VT_R4 :
    va->pfltVal = (FLOAT *)allocParamMemory (sizeof (FLOAT));
    break;
  case VT_BYREF|VT_R8 :
    va->pdblVal = (DOUBLE *)allocParamMemory (sizeof (DOUBLE));
    break;
  case VT_BYREF|VT_BOOL :
    va->pboolVal = (VARIANT_BOOL *)allocParamMemory (sizeof (VARIANT_BOOL));
    break;
  case VT_BYREF|VT_ERROR :
    va->pscode = (SCODE *)allocParamMemory (sizeof (SCODE));
    break;
  case VT_BYREF|VT_CY :
    va->pcyVal = (CY *)allocParamMemory (sizeof (CY));
    break;
  case VT_BYREF|VT_DATE :
    va->pdate = (DATE *)allocParamMemory (sizeof (DATE));
    break;
  case VT_BYREF|VT_BSTR :
    va->pbstrVal = (BSTR *)allocParamMemory (sizeof (BSTR));
    break;
  case VT_BYREF|VT_UNKNOWN :
    va->ppunkVal = (IUnknown **)allocParamMemory (sizeof (IUnknown *));
    break;
  case VT_BYREF|VT_PTR :
  case VT_BYREF|VT_DISPATCH :
    va->ppdispVal = (IDispatch **)allocParamMemory (sizeof (IDispatch *));
    break;
  case VT_BYREF|VT_ARRAY :
  case VT_BYREF|VT_SAFEARRAY :
    va->pparray = (SAFEARRAY **)allocParamMemory (sizeof (SAFEARRAY *));
    break;
  case VT_BYREF|VT_VARIANT :
    va->pvarVal = (VARIANT *)allocParamMemory (sizeof (VARIANT));
    break;
  case VT_BYREF|VT_I1 :
    va->pcVal = (CHAR *)allocParamMemory (sizeof (CHAR));
    break;
  case VT_BYREF|VT_UI2 :
    va->puiVal = (USHORT *)allocParamMemory (sizeof (USHORT));
    break;
  case VT_BYREF|VT_UI4 :
    va->pulVal = (ULONG *)allocParamMemory (sizeof (ULONG));
    break;
  case VT_BYREF|VT_UI8 :
    va->pullVal = (ULONGLONG *)allocParamMemory (sizeof (ULONGLONG));
    break;
  case VT_BYREF|VT_INT :
    va->pintVal = (INT *)allocParamMemory (sizeof (INT));
    break;
  case VT_BYREF|VT_UINT :
    va->puintVal = (UINT *)allocParamMemory (sizeof (UINT));
    break;
  default :
    {char buff[128];
    sprintf (buff, "Can't allocate return value for VARIANT 0x%X", va->vt);
    scheme_signal_error (buff); }
  }
}

static Scheme_Object *mx_make_direct_call (int argc, Scheme_Object **argv,
					   INVOKEKIND invKind,
					   IDispatch *pIDispatch,
					   const char * name,
					   MX_TYPEDESC *pTypeDesc)
{
  HRESULT hr;
  Scheme_Object *retval;
  MX_ARGS_COUNT argsCount;
  IDispatch *pInterface;
  COMPTR funPtr;
  VARIANT retvalVa, va, *vaPtr;
  static VARIANT argVas[MAXDIRECTARGS];
  static VARIANT optArgVas[MAXDIRECTARGS];
  FUNCDESC *pFuncDesc;
  short numParamsPassed;
  short numOptParams;
  short lcidIndex;
  char buff[128];
  int i, j;

  pFuncDesc = pTypeDesc->funcdescs.pFuncDescImpl;
  checkArgTypesAndCounts (pFuncDesc, TRUE, // direct
			  invKind, argc, argv, &argsCount);
  numParamsPassed = argsCount.numParamsPassed;
  numOptParams = argsCount.numOptParams;
  lcidIndex = argsCount.lcidIndex;

  if (pTypeDesc->pInterface == NULL) {
      COMPTR *vtbl;

      hr = pIDispatch->QueryInterface (pTypeDesc->implGuid, (void **)&pInterface);

      if (FAILED (hr) || pInterface == NULL) {
	  sprintf (buff, "Failed to get direct interface for call to `%s'", name);
	  codedComError (buff, hr);
	  }
      vtbl = ((COMPTR * *)pInterface)[0];
      pTypeDesc->pInterface = pInterface;
      pTypeDesc->funPtr = funPtr = vtbl[pTypeDesc->funOffset];
      }
  else {
      pInterface = pTypeDesc->pInterface;
      funPtr = pTypeDesc->funPtr;
      }

  // push return value ptr

  VariantInit (&retvalVa);
  retvalVa.vt = getVarTypeFromElemDesc (argsCount.retvalInParams
                                        ? &pFuncDesc->lprgelemdescParam[pFuncDesc->cParams-1]
                                        : &pFuncDesc->elemdescFunc);

  if (invKind != INVOKE_PROPERTYPUT &&
      retvalVa.vt != VT_VOID &&
      retvalVa.vt != VT_HRESULT) {
      retvalVa.vt |= VT_BYREF;
      allocateDirectRetval (&retvalVa);
      pushOneArg (retvalVa, buff);
      }

  // these must be macros, not functions, so that stack is maintained

  pushOptArgs (pFuncDesc, numParamsPassed, numOptParams, optArgVas, vaPtr, va,
	       argc, i, j, lcidIndex, buff);

  pushSuppliedArgs (pFuncDesc, numParamsPassed, argc, argv, argVas, vaPtr, va,
		   i, j, lcidIndex, buff);

  // push the "this" pointer before calling

  __asm {
    push pInterface;
    call funPtr;
    mov hr, eax;
  }

  if (FAILED (hr)) {
      char buff[128];
      sprintf (buff, "COM method `%s' failed", name);
      codedComError (buff, hr);
      }

  // unmarshal boxed values, cleanup
  i = argc - 1;
  j = argc - 3;
  if (lcidIndex != NO_LCID && lcidIndex <= j + 1)
      j++;

  vaPtr = argVas + j;
  for ( ; j >= 0; i--, j--, vaPtr--) {
      if (j == lcidIndex)
	  i++;
      else
	  unmarshalVariant (argv[i], vaPtr);
      }

  if (invKind == INVOKE_PROPERTYPUT)
      return scheme_void;

  retval = retvalVariantToSchemeObject (&retvalVa);

  // all pointers are 32 bits, choose arbitrary one
  if (retvalVa.vt != VT_VOID)
      scheme_gc_ptr_ok (retvalVa.pullVal);

  return retval;
}

static Scheme_Object *mx_make_call (int argc, Scheme_Object **argv,
				   INVOKEKIND invKind)
{
  Scheme_Object *retval;
  MX_TYPEDESC *pTypeDesc;
  DISPID dispid = 0;
  DISPPARAMS methodArguments;
  VARIANT methodResult;
  EXCEPINFO exnInfo;
  unsigned int errorIndex;
  IDispatch *pIDispatch;
  const char *name;
  short numParamsPassed;
  int i, j;
  HRESULT hr;
  char buff[256];

  pIDispatch = MX_COM_OBJ_VAL (GUARANTEE_COM_OBJ (mx_fun_string (invKind), 0));

  if (pIDispatch == NULL)
    scheme_signal_error ("NULL COM object");

  name = schemeToText (GUARANTEE_STRSYM (mx_fun_string (invKind), 1));

  if (invKind == INVOKE_FUNC && isDispatchName (name)) {
    sprintf (buff, "%s: IDispatch methods may not be called",
	    mx_fun_string (invKind));
    scheme_signal_error (buff);
  }

  // check arity, types of method arguments

  pTypeDesc = getMethodType ((MX_COM_Object *)argv[0], name, invKind);

  // try direct call via function pointer
  // otherwise, use COM Automation

  if (pTypeDesc &&
      (pTypeDesc->funOffset != NO_FUNPTR) &&
      /* assignment */
      (retval = mx_make_direct_call (argc, argv, invKind,
				    pIDispatch, name, pTypeDesc)))
    return retval;

  if (pTypeDesc)
    dispid = pTypeDesc->memID;

  else {
    // If there is no pTypeDesc, then we have to wing it.
    // Look for a dispid for the method name.  If we find it, just push
    // the arguments and let the COM object figure things out.

    // Translate the name to Unicode.
    OLECHAR namebuf[1024];
    unsigned int len = (unsigned int)strlen (name);
    unsigned int count = MultiByteToWideChar (CP_ACP, (DWORD)0, name, len,
				    namebuf, sizeray (namebuf)-1);
    namebuf[len] = '\0';
    if (count < len) {
      sprintf (buff, "%s: Unable to translate name \"%s\" to Unicode",
	      mx_fun_string (invKind), name);
      scheme_signal_error (buff);
    }

    LPOLESTR namearray = (LPOLESTR)&namebuf;

    hr = pIDispatch->GetIDsOfNames (IID_NULL, &namearray, 1,
				    LOCALE_SYSTEM_DEFAULT, &dispid);

    if (FAILED (hr)) {
	const char *funString = mx_fun_string (invKind);
      switch (hr) {
      case E_OUTOFMEMORY :
	sprintf (buff, "%s: out of memory", funString);
	scheme_signal_error (buff);
      case DISP_E_UNKNOWNNAME :
	sprintf (buff, "%s: unknown name \"%s\"", funString, name);
	scheme_signal_error (buff);
      case DISP_E_UNKNOWNLCID :
	sprintf (buff, "%s: unknown LCID", funString);
	scheme_signal_error (buff);
      default :
	codedComError (funString, hr);
      }
    }
  }

  // Build the method arguments even if pTypeDesc is NULL.
  numParamsPassed = buildMethodArguments (pTypeDesc,
					 invKind,
					 argc, argv,
					 &methodArguments);

  if (invKind != INVOKE_PROPERTYPUT)
    VariantInit (&methodResult);

  // invoke requested method

  hr = pIDispatch->Invoke (dispid, IID_NULL, LOCALE_SYSTEM_DEFAULT,
			  invKind,
			  &methodArguments,
			  (invKind == INVOKE_PROPERTYPUT) ?
			    NULL : &methodResult,
			  &exnInfo,
			  &errorIndex);

  if (hr == DISP_E_EXCEPTION) {
    char errBuff[2048];
    char description[1024];
    BOOL hasErrorCode;
    BOOL hasDescription;

    hasErrorCode = (exnInfo.wCode > 0);
    hasDescription = (exnInfo.bstrDescription != NULL);

    if (hasDescription) {
      unsigned int len;

      len = SysStringLen (exnInfo.bstrDescription);
      WideCharToMultiByte (CP_ACP, (DWORD)0,
			  exnInfo.bstrDescription, len,
			  description, sizeof (description)-1,
			  NULL, NULL);
      description[len] = '\0';
    }

    if (hasErrorCode) {
      sprintf (errBuff,
	      "COM object exception, error code 0x%X%s%s",
	      exnInfo.wCode,
	      hasDescription ? "\nDescription: " : "" ,
	      hasDescription ? description : "");
      scheme_signal_error (errBuff);
    }
    else {
      sprintf (errBuff,
	      "COM object exception%s%s",
	      hasDescription ? "\nDescription: " : "" ,
	      hasDescription ? description : "");
      codedComError (errBuff, exnInfo.scode);
    }
  }

  if (FAILED (hr)) {
    char buff[2048];
    sprintf (buff, "\"%s\" (%s) failed",
	    schemeToText (argv[1]), inv_kind_string (invKind));
    codedComError (buff, hr);
  }

  // unmarshal data passed by reference, cleanup

  for (i = 2, j = numParamsPassed - 1; i < argc; i++, j--)
    unmarshalVariant (argv[i], &methodArguments.rgvarg[j]);

  if (numParamsPassed > 0)
    scheme_gc_ptr_ok (methodArguments.rgvarg);

  if (invKind == INVOKE_PROPERTYPUT)
    return scheme_void;

  // unmarshal return value

  return variantToSchemeObject (&methodResult);

}

Scheme_Object *mx_com_invoke (int argc, Scheme_Object **argv)
{
  return mx_make_call (argc, argv, INVOKE_FUNC);
}

Scheme_Object *mx_com_get_property (int argc, Scheme_Object **argv)
{
  return mx_make_call (argc, argv, INVOKE_PROPERTYGET);
}

Scheme_Object *mx_com_set_property (int argc, Scheme_Object **argv)
{
  return mx_make_call (argc, argv, INVOKE_PROPERTYPUT);
}

Scheme_Object *mx_all_clsid (int argc, Scheme_Object **argv, char **attributes)
{
  LONG result;
  Scheme_Object *retval;
  HKEY hkey, hsubkey;
  FILETIME fileTime;
  unsigned long keyIndex;
  TCHAR clsidBuffer[256];
  DWORD clsidBufferSize;
  DWORD dataType;
  BYTE dataBuffer[256];
  DWORD dataBufferSize;
  BOOL loopFlag;
  char **p;

  retval = scheme_null;

  result = RegOpenKeyEx (HKEY_CLASSES_ROOT,
			"CLSID",
			 (DWORD)0,
			KEY_READ,
			&hkey);

  if (result != ERROR_SUCCESS)
    return retval;

  // enumerate subkeys until we find the one we want

  keyIndex = 0;

  while (1) {

    // get next subkey

    clsidBufferSize = sizeray (clsidBuffer);

    result = RegEnumKeyEx (hkey, keyIndex++,
			  clsidBuffer,
			  &clsidBufferSize,
			  0, NULL, NULL,
			  &fileTime);

    if (result == ERROR_NO_MORE_ITEMS)
      break;

    if (strlen (clsidBuffer) != CLSIDLEN) // not a CLSID -- bogus entry
      continue;

    // open subkey

    result = RegOpenKeyEx (hkey, clsidBuffer,
			  (DWORD)0,
			  KEY_READ, &hsubkey);

    if (result != ERROR_SUCCESS)
      scheme_signal_error ("Error while searching Windows registry");

    dataBufferSize = sizeof (dataBuffer);

    RegQueryValueEx (hsubkey, "", 0, &dataType, dataBuffer, &dataBufferSize);

    if (dataType == REG_SZ) {
      int subkeyIndex;
      TCHAR subkeyBuffer[256];
      DWORD subkeyBufferSize;

      subkeyIndex = 0;

      loopFlag = TRUE;

      while (loopFlag) {

	subkeyBufferSize = sizeray (subkeyBuffer);

	result = RegEnumKeyEx (hsubkey, subkeyIndex++,
			      subkeyBuffer,
			      &subkeyBufferSize,
			      0, NULL, NULL,
			      &fileTime);

	if (result == ERROR_NO_MORE_ITEMS)
	  break;

	p = attributes;

	while (*p) {
	  if (stricmp (subkeyBuffer, *p) == 0) {
	    retval = scheme_make_pair (multiByteToSchemeCharString ((char *)dataBuffer),
				       retval);
	    loopFlag = FALSE;
	    break; // *p loop
	  }
	  p++;
	}
      }
    }

    RegCloseKey (hsubkey);
  }

  RegCloseKey (hkey);

  return retval;
}

Scheme_Object *mx_all_controls (int argc, Scheme_Object **argv)
{
  return mx_all_clsid (argc, argv, controlAttributes);
}

Scheme_Object *mx_all_coclasses (int argc, Scheme_Object **argv)
{
  return mx_all_clsid (argc, argv, objectAttributes);
}

Scheme_Object *mx_com_object_eq (int argc, Scheme_Object **argv)
{
  IUnknown *pIUnknown1, *pIUnknown2;
  IDispatch *pIDispatch1, *pIDispatch2;
  Scheme_Object *retval;

  pIDispatch1 = MX_COM_OBJ_VAL (GUARANTEE_COM_OBJ ("com-object-eq?", 0));
  pIDispatch2 = MX_COM_OBJ_VAL (GUARANTEE_COM_OBJ ("com-object-eq?", 1));

  // these should never fail

  pIDispatch1->QueryInterface (IID_IUnknown, (void **)&pIUnknown1);
  pIDispatch2->QueryInterface (IID_IUnknown, (void **)&pIUnknown2);

  retval = (pIUnknown1 == pIUnknown2) ? scheme_true : scheme_false;

  pIUnknown1->Release();
  pIUnknown2->Release();

  return retval;
}

Scheme_Object *mx_document_title (int argc, Scheme_Object **argv)
{
  HRESULT hr;
  IHTMLDocument2 *pDocument;
  BSTR bstr;
  Scheme_Object *retval;

  pDocument = MX_DOCUMENT_VAL (GUARANTEE_DOCUMENT ("document-title", 0));

  hr = pDocument->get_title (&bstr);

  if (FAILED (hr))
    scheme_signal_error ("document-title: Can't get title");

  retval = BSTRToSchemeString (bstr);

  SysFreeString (bstr);

  return retval;
}

Scheme_Object *mx_document_objects (int argc, Scheme_Object **argv)
{
  HRESULT hr;
  IHTMLDocument2 *pDocument;
  IHTMLElement *pBody;
  IHTMLElementCollection *pObjectsCollection;
  long numObjects;
  Scheme_Object *retval;
  int i;
  IDispatch *pObjectDispatch;
  MX_COM_Object *com_object;

  pDocument = MX_DOCUMENT_VAL (GUARANTEE_DOCUMENT ("document-objects", 0));

  hr = pDocument->get_body (&pBody);

  if (FAILED (hr) || pBody == NULL)
    codedComError ("document-objects: Can't find document BODY", hr);

  pObjectsCollection = getBodyElementsWithTag (pBody, TEXT ("OBJECT"));

  pBody->Release();

  pObjectsCollection->get_length (&numObjects);

  retval = scheme_null;

  for (i = numObjects - 1; i >= 0; i--) {

    pObjectDispatch = getObjectInCollection (pObjectsCollection, i);

    com_object = (MX_COM_Object *)scheme_malloc (sizeof (MX_COM_Object));

    com_object->type = mx_com_object_type;
    com_object->pIDispatch = pObjectDispatch;
    com_object->clsId = emptyClsId;
    com_object->pITypeInfo = NULL;
    com_object->pEventTypeInfo = NULL;
    com_object->pIConnectionPoint = NULL;
    com_object->pISink = NULL;
    com_object->connectionCookie = (DWORD)0;
    com_object->released = FALSE;

    mx_register_com_object ((Scheme_Object *)com_object, pObjectDispatch);

    retval = scheme_make_pair ((Scheme_Object *)com_object, retval);
  }

  pObjectsCollection->Release();

  return retval;
}

MX_Element *make_mx_element (IHTMLElement *pIHTMLElement)
{
  MX_Element *elt;

  elt = (MX_Element *)scheme_malloc (sizeof (MX_Element));

  elt->type = mx_element_type;
  elt->released = FALSE;
  elt->valid = TRUE;
  elt->pIHTMLElement = pIHTMLElement;

  // this should not be necessary
  // apparently, IE does not always call AddRef()
  //  for HTML elements
  if (pIHTMLElement->AddRef() > 2)
    pIHTMLElement->Release();

  mx_register_simple_com_object ((Scheme_Object *)elt, pIHTMLElement);

  return elt;
}

Scheme_Object *mx_elements_with_tag (int argc, Scheme_Object **argv)
{
  HRESULT hr;
  IHTMLDocument2 *pDocument;
  IHTMLElement *pBody, *pIHTMLElement;
  IHTMLElementCollection *pCollection;
  long numObjects;
  Scheme_Object *retval;
  MX_Element *elt;
  IDispatch *pDispatch;
  int i;

  GUARANTEE_STRSYM ("elements-with-tag", 1);

  pDocument = MX_DOCUMENT_VAL (GUARANTEE_DOCUMENT ("elements-with-tag", 0));

  pDocument->get_body (&pBody);

  if (pBody == NULL)
    scheme_signal_error ("elements-with-tag: Can't find document BODY");

  if (stricmp (schemeToText (argv[1]), "BODY") == 0)
    return scheme_make_pair ((Scheme_Object *) (make_mx_element (pBody)),
			    scheme_null);

  pCollection = getBodyElementsWithTag (pBody, schemeToText (argv[1]));

  pBody->Release();

  pCollection->get_length (&numObjects);

  retval = scheme_null;

  for (i = numObjects - 1; i >= 0; i--) {

    pDispatch = getElementInCollection (pCollection, i);

    hr = pDispatch->QueryInterface (IID_IHTMLElement, (void **)&pIHTMLElement);

    if (FAILED (hr) || pIHTMLElement == NULL)
      codedComError ("elements-with-tag: Can't get IHTMLElement interface", hr);

    elt = make_mx_element (pIHTMLElement);

    mx_register_simple_com_object ((Scheme_Object *)elt, pIHTMLElement);

    retval = scheme_make_pair ((Scheme_Object *)elt, retval);
  }

  pCollection->Release();

  return retval;
}

CLSID getCLSIDFromCoClass (LPCTSTR name)
{
  HKEY hkey, hsubkey;
  LONG result;
  FILETIME fileTime;
  unsigned long keyIndex;
  TCHAR clsIdBuffer[256];
  OLECHAR oleClsIdBuffer[256];
  DWORD clsIdBufferSize;
  DWORD dataType;
  BYTE dataBuffer[256];
  DWORD dataBufferSize;
  CLSID clsId;
  BOOL loopFlag;
  int count;
  unsigned int len;
  char **p;

  // dummy entry

  clsId = emptyClsId;

  // get HKEY to Interfaces listing in Registry

  result = RegOpenKeyEx (HKEY_CLASSES_ROOT,
			"CLSID",
			 (DWORD)0,
			KEY_READ,
			&hkey);


  if (result != ERROR_SUCCESS)
    scheme_signal_error ("Error while searching Windows registry");

  // enumerate subkeys until we find the one we want

  // really, should call RegQueryInfoKey to find size needed for buffers

  keyIndex = 0;

  while (1) {

    // get next subkey

    clsIdBufferSize = sizeof (clsIdBuffer);

    result = RegEnumKeyEx (hkey, keyIndex++,
			  clsIdBuffer,
			  &clsIdBufferSize,
			  0, NULL, NULL,
			  &fileTime);

    if (result == ERROR_NO_MORE_ITEMS)
      break;

    if (result != ERROR_SUCCESS)
      scheme_signal_error ("Error enumerating subkeys in Windows registry");

    if (strlen (clsIdBuffer) != CLSIDLEN) // not a CLSID -- bogus entry
      continue;

    // open subkey

    result = RegOpenKeyEx (hkey, clsIdBuffer,
			  (DWORD)0,
			  KEY_READ, &hsubkey);

    if (result != ERROR_SUCCESS)
      return clsId;

    dataBufferSize = sizeof (dataBuffer);

    RegQueryValueEx (hsubkey, "", 0, &dataType, dataBuffer, &dataBufferSize);

    if (dataType == REG_SZ && lstrcmp (name, (char *)dataBuffer) == 0) {
      int subkeyIndex;
      TCHAR subkeyBuffer[256];
      DWORD subkeyBufferSize;

      // confirm this is a COM object

      subkeyIndex = 0;

      loopFlag = TRUE;

      while (loopFlag) {

	subkeyBufferSize = sizeray (subkeyBuffer);

	result = RegEnumKeyEx (hsubkey, subkeyIndex++,
			      subkeyBuffer,
			      &subkeyBufferSize,
			      0, NULL, NULL,
			      &fileTime);

	if (result == ERROR_NO_MORE_ITEMS)
	  break;

	if (result != ERROR_SUCCESS)
	  scheme_signal_error ("Error enumerating subkeys in Windows registry");

	p = objectAttributes;

	while (*p) {
	  if (stricmp (subkeyBuffer, *p) == 0) {
	    len = (unsigned int) strlen (clsIdBuffer);
	    count = MultiByteToWideChar (CP_ACP, (DWORD)0,
					clsIdBuffer, len,
					oleClsIdBuffer,
					sizeray (oleClsIdBuffer) - 1);
	    oleClsIdBuffer[len] = '\0';

	    if (count == 0)
	      scheme_signal_error ("Error translating CLSID to Unicode", name);

	    CLSIDFromString (oleClsIdBuffer, &clsId);
	    loopFlag = FALSE;
	    break; // *p loop
	  }
	  p++;
	}
      }
    }

    RegCloseKey (hsubkey);

  }

  RegCloseKey (hkey);

  if (isEmptyClsId (clsId))
    scheme_signal_error ("Coclass %s not found", name);

  return clsId;
}

Scheme_Object *mx_find_element (int argc, Scheme_Object **argv)
{
  IHTMLElement *pIHTMLElement;
  int index;

  GUARANTEE_DOCUMENT ("find-element", 0);
  GUARANTEE_STRSYM   ("find-element", 1);
  GUARANTEE_STRSYM   ("find-element", 2);

  if (argc > 3)
      GUARANTEE_NONNEGATIVE ("find-element", 3);

  index = (argc > 3) ? SCHEME_INT_VAL (argv[3]) : 0;

  pIHTMLElement = findBodyElement (MX_DOCUMENT_VAL (argv[0]),
				  schemeToText (argv[1]),
				  schemeToText (argv[2]),
				  index);

  if (pIHTMLElement == NULL)
    scheme_signal_error ("find-element: HTML element with tag = %s, id = %s not found",
			schemeToText (argv[1]),
                        schemeToText (argv[2]));

  return (Scheme_Object *) make_mx_element (pIHTMLElement);
}

Scheme_Object *mx_find_element_by_id_or_name (int argc, Scheme_Object **argv)
{
  HRESULT hr;
  IHTMLElement *pIHTMLElement;
  IHTMLElementCollection *pIHTMLElementCollection;
  IHTMLDocument2 *pIHTMLDocument2;
  VARIANT name, index;
  BSTR bstr;
  IDispatch *pEltDispatch;

  if (argc > 2)
      GUARANTEE_NONNEGATIVE ("find-element-by-id-or-name", 2);

  pIHTMLDocument2 = MX_DOCUMENT_VAL (GUARANTEE_DOCUMENT ("find-element-by-id-or-name", 0));

  hr = pIHTMLDocument2->get_all (&pIHTMLElementCollection);

  if (FAILED (hr) || pIHTMLElementCollection == NULL) {
    scheme_signal_error ("find-element-by-id-or-name: "
			"Couldn't retrieve element collection "
			"from HTML document");
  }

  bstr = schemeToBSTR (GUARANTEE_STRSYM ("find-element-by-id-or-name", 1));

  name.vt = VT_BSTR;
  name.bstrVal = bstr;

  index.vt = VT_I4;
  index.lVal = (argc > 2) ? SCHEME_INT_VAL (argv[2]) : 0;

  pIHTMLElementCollection->item (name, index, &pEltDispatch);

  SysFreeString (bstr);

  pIHTMLElementCollection->Release();

  if (pEltDispatch == NULL)
    scheme_signal_error ("find-element-by-id-or-name: "
			"Couldn't find element with id = %s", schemeToText (argv[1]));

  hr = pEltDispatch->QueryInterface (IID_IHTMLElement, (void **)&pIHTMLElement);

  if (FAILED (hr) || pIHTMLElement == NULL)
    scheme_signal_error ("find-element-by-id-or-name: "
			"Couldn't retrieve element interface "
			"for element with id = %s",
                        schemeToText (argv[1]));

  return (Scheme_Object *) make_mx_element (pIHTMLElement);
}

// for coclass->html, progid->html
Scheme_Object *mx_clsid_to_html (CLSID clsId,
                                const char *controlName,
				const char *fname,
				int argc, Scheme_Object **argv )
{
  LPOLESTR clsIdString;
  char widthBuff[25];
  char heightBuff[25];
  char buff[512];
  char *format;

  GUARANTEE_INTEGER (fname, 1);
  GUARANTEE_INTEGER (fname, 2);

  format = "%u";

  if (argc > 3) {
      const char * symString = schemeToMultiByte (GUARANTEE_STRSYM (fname, 3));

      if (stricmp (symString, "percent") == 0)
	  format = "%u%%";

      else if (stricmp (symString, "pixels"))
	  scheme_signal_error ("%s: Invalid size specifier '%s: "
			      "must be either 'pixels or 'percent",
			      fname, symString);

      }

  sprintf (widthBuff, format, SCHEME_INT_VAL (argv[1]));
  sprintf (heightBuff, format, SCHEME_INT_VAL (argv[2]));

  StringFromCLSID (clsId, &clsIdString);

  * (clsIdString + wcslen (clsIdString) - 1) = L'\0';

  if (clsIdString == NULL)
      scheme_signal_error ("%s: Can't convert control CLSID to string", fname);

  sprintf (buff,
	  "<OBJECT ID=\"%s\" WIDTH=\"%s\" HEIGHT=\"%s\" CLASSID=\"clsid:%S\">\n"
	  "</OBJECT>",
	  controlName,
	  widthBuff, heightBuff,
	  clsIdString + 1);

  return multiByteToSchemeCharString (buff);
}

Scheme_Object * mx_coclass_to_html (int argc, Scheme_Object **argv)
{
  LPCTSTR controlName = schemeToText (GUARANTEE_STRSYM ("coclass->html", 0));
  CLSID clsId = getCLSIDFromCoClass (controlName);

  if (isEmptyClsId (clsId))
      scheme_signal_error ("coclass->html: Coclass \"%s\" not found",
			   schemeToMultiByte (argv[0]));

  return mx_clsid_to_html (clsId, controlName, "coclass->html", argc, argv);
}

Scheme_Object *mx_progid_to_html (int argc, Scheme_Object **argv)
{
  HRESULT hr;
  BSTR wideProgId;
  CLSID clsId;

  wideProgId = schemeToBSTR (GUARANTEE_STRSYM ("progid->html", 0));

  hr = CLSIDFromProgID (wideProgId, &clsId);

  SysFreeString (wideProgId);

  if (FAILED (hr))
      scheme_signal_error ("progid->html: ProgID \"%s\" not found", schemeToText (argv[0]));

  return mx_clsid_to_html (clsId, schemeToText (argv[0]), "progid->html", argc, argv);
}

Scheme_Object *mx_stuff_html (int argc, Scheme_Object **argv,
			     WCHAR *oleWhere, char *scheme_name) {
  IHTMLDocument2 *pDocument;
  IHTMLElement *pBody;
  BSTR where, html;

  pDocument = MX_DOCUMENT_VAL (GUARANTEE_DOCUMENT (scheme_name, 0));

  html = schemeToBSTR (GUARANTEE_STRSYM (scheme_name, 1));
  pDocument->get_body (&pBody);

  if (pBody == NULL)
    scheme_signal_error ("Can't find document BODY");

  where = SysAllocString (oleWhere);

  pBody->insertAdjacentHTML (where, html);

  SysFreeString (where);
  SysFreeString (html);

  return scheme_void;

}

Scheme_Object *mx_insert_html (int argc, Scheme_Object **argv)
{
  return mx_stuff_html (argc, argv, L"AfterBegin", "doc-insert-html");
}

Scheme_Object *mx_append_html (int argc, Scheme_Object **argv)
{
  return mx_stuff_html (argc, argv, L"BeforeEnd", "doc-append-html");
}

Scheme_Object *mx_replace_html (int argc, Scheme_Object **argv)
{
  IHTMLDocument2 *pDocument;
  IHTMLElement *pBody;
  BSTR html;

  pDocument = MX_DOCUMENT_VAL (GUARANTEE_DOCUMENT ("replace-html", 0));
  html = schemeToBSTR (GUARANTEE_STRSYM ("replace-html", 1));

  pDocument->get_body (&pBody);

  if (pBody == NULL)
    scheme_signal_error ("Can't find document body");

  pBody->put_innerHTML (html);

  SysFreeString (html);

  return scheme_void;
}

/*

blocking on Win events doesn't seem to work
any longer

static BOOL win_event_available (void *)
{
  MSG msg;

  return (PeekMessage (&msg, NULL, 0x400, 0x400, PM_NOREMOVE) ||
	  PeekMessage (&msg, NULL, 0x113, 0x113, PM_NOREMOVE));
}

static void win_event_sem_fun (MX_Document_Object *doc, void *fds)
{
  static HANDLE dummySem;

  if (!dummySem) {
    dummySem = CreateSemaphore (NULL, 0, 1, NULL);
    if (!dummySem) {
      scheme_signal_error ("Error creating Windows event semaphore");
    }
  }

  scheme_add_fd_eventmask (fds, QS_ALLINPUT);
  scheme_add_fd_handle (dummySem, fds, TRUE);
}
*/

Scheme_Object *mx_process_win_events (int argc, Scheme_Object **argv)
{
  MSG msg;

  /* this used to work, sort of

    scheme_block_until ((int (*) (Scheme_Object *))win_event_available,
		     (void (*) (Scheme_Object *, void *))win_event_sem_fun,
		     NULL, 0.0F);
  */

  while (PeekMessage (&msg, NULL, 0x400, 0x400, PM_REMOVE) ||
	 PeekMessage (&msg, NULL, 0x113, 0x113, PM_REMOVE)) {
    TranslateMessage (&msg);
    DispatchMessage (&msg);
  }

  return scheme_void;
}

void initMysSinkTable (void)
{
  myssink_table.pmake_cy = mx_make_cy;
  myssink_table.pmake_date = mx_make_date;
  myssink_table.pmake_bool = mx_make_bool;
  myssink_table.pmake_scode = mx_make_scode;
  myssink_table.pmake_idispatch = mx_make_idispatch;
  myssink_table.pmake_iunknown = mx_make_iunknown;

  myssink_table.pcy_pred = mx_cy_pred;
  myssink_table.pdate_pred = mx_date_pred;
  myssink_table.pscode_pred = mx_scode_pred;
  myssink_table.pcomobj_pred = mx_comobj_pred;
  myssink_table.piunknown_pred = mx_iunknown_pred;

  myssink_table.pcy_val = mx_cy_val;
  myssink_table.pdate_val = mx_date_val;
  myssink_table.pscode_val = mx_scode_val;
  myssink_table.pcomobj_val = mx_comobj_val;
  myssink_table.piunknown_val = mx_iunknown_val;
}

Scheme_Object *mx_release_type_table (void)
{
  int i;
  MX_TYPE_TBL_ENTRY *p, *psave;

  for (i = 0; i < sizeray (typeTable); i++) {
    p = typeTable[i];
    while (p) {
      scheme_release_typedesc ((void *)p->pTypeDesc, NULL);
      psave = p;
      p = p->next;
      scheme_gc_ptr_ok (psave);
    }
  }

  return scheme_void;
}

void mx_exit_closer (Scheme_Object *obj,
		    Scheme_Close_Custodian_Client *fun, void *data)
{
  if ((fun == (Scheme_Close_Custodian_Client *)
                     scheme_release_com_object) ||
      (fun == (Scheme_Close_Custodian_Client *)
       scheme_release_simple_com_object)) {
    (*fun) (obj, data);
  }
}

void mx_cleanup (void)
{
  mx_release_type_table();
  /* looks like CoUninitialize() gets called automatically */
}

Scheme_Object *scheme_module_name (void)
{
    return scheme_intern_symbol (MXMAIN);
}

Scheme_Object *scheme_initialize (Scheme_Env *env)
{
  HRESULT hr;
  Scheme_Object *mx_fun;
  int i;

  scheme_register_extension_global (&mx_omit_obj, sizeof (mx_omit_obj));

  // should not be necessary, but sometimes
  // this variable is not 0'd out - bug in VC++ or MzScheme?

  memset (typeTable, 0, sizeof (typeTable));

  // globals in mysterx.cxx

  mx_name = scheme_intern_symbol (MXMAIN);
  scheme_date_type = scheme_builtin_value ("struct:date");

  mx_com_object_type = scheme_make_type ("<com-object>");
  mx_com_type_type = scheme_make_type ("<com-type>");
  mx_browser_type = scheme_make_type ("<mx-browser>");
  mx_document_type = scheme_make_type ("<mx-document>");
  mx_element_type = scheme_make_type ("<mx-element>");
  mx_event_type = scheme_make_type ("<mx-event>");
  mx_com_cy_type = scheme_make_type ("<com-currency>");
  mx_com_date_type = scheme_make_type ("<com-date>");
  mx_com_scode_type = scheme_make_type ("<com-scode>");
  mx_com_iunknown_type = scheme_make_type ("<com-iunknown>");
  mx_com_omit_type = scheme_make_type ("<com-omit>");
  mx_com_typedesc_type = scheme_make_type ("<com-typedesc>");

  hr = CoInitialize (NULL);

  // S_OK means success, S_FALSE means COM already loaded

  if (FAILED (hr) && hr != S_FALSE) {
    return scheme_false;
  }

  Scheme_Object * arglist[1] = {scheme_false};
  scheme_register_extension_global (&mx_unmarshal_strings_as_symbols, sizeof mx_unmarshal_strings_as_symbols);
  scheme_register_extension_global (&mx_marshal_raw_scheme_objects, sizeof mx_marshal_raw_scheme_objects);

  mx_unmarshal_strings_as_symbols = scheme_apply (scheme_builtin_value ("make-parameter"), 1, arglist);
  mx_marshal_raw_scheme_objects = scheme_apply (scheme_builtin_value ("make-parameter"), 1, arglist);

  // export prims + omit value

  env = scheme_primitive_module (mx_name, env);

  for (i = 0; i < sizeray (mxPrims); i++) {
    mx_fun = scheme_make_prim_w_arity (mxPrims[i].c_fun,
				      mxPrims[i].name,
				      mxPrims[i].minargs,
				      mxPrims[i].maxargs);
    scheme_add_global (mxPrims[i].name, mx_fun, env);
  }

  mx_omit_obj = (Scheme_Object *)scheme_malloc (sizeof (MX_OMIT));
  mx_omit_obj->type = mx_com_omit_type;

  scheme_add_global ("com-omit", mx_omit_obj, env);

  scheme_finish_primitive_module (env);

  initEventNames();

  initMysSinkTable();

  if (isatty (fileno (stdin))) {
    fprintf (stderr,
	    "MysterX extension for PLT Scheme, "
	    "Copyright (c) 1999-2003 PLT (Paul Steckler)\n");
  }

  scheme_add_atexit_closer (mx_exit_closer);
  atexit (mx_cleanup);

  return scheme_void;
}

Scheme_Object * scheme_reload (Scheme_Env *env)
{
  return scheme_initialize (env);
}

// for some reason, couldn't put ATL stuff in browser.cxx
// so we leave the Win message loop here

void browserHwndMsgLoop (LPVOID p)
{
  HRESULT hr;
  MSG msg;
  HWND hwnd;
  IUnknown *pIUnknown;
  BROWSER_WINDOW_INIT *pBrowserWindowInit;
  LONG hasScrollBars;
  BOOL *destroy;

  pBrowserWindowInit = (BROWSER_WINDOW_INIT *)p;

  // set apparently-unused low bit in style to inform
  // DHTMLPage object that we want scrollbars

  hasScrollBars = (pBrowserWindowInit->browserWindow.style & (WS_HSCROLL|WS_VSCROLL))
      ? 1L
      : 0L;

  hwnd = CreateWindow ("AtlAxWin7", "myspage.DHTMLPage.1",
		      WS_VISIBLE | hasScrollBars |
		      (pBrowserWindowInit->browserWindow.style & ~ (WS_HSCROLL|WS_VSCROLL)),
		      pBrowserWindowInit->browserWindow.x, pBrowserWindowInit->browserWindow.y,
		      pBrowserWindowInit->browserWindow.width, pBrowserWindowInit->browserWindow.height,
		      NULL, NULL, hInstance, NULL);

  if (hwnd == NULL)
    scheme_signal_error ("make-browser: Can't create browser window");

  ShowWindow (hwnd, SW_SHOW);
  SetForegroundWindow (hwnd);

  browserHwnd = hwnd;

  if (hasScrollBars)
    // clear spurious low bit to avoid trouble
    SetWindowLong (hwnd, GWL_STYLE,
		  GetWindowLong (hwnd, GWL_STYLE) & ~1L);

  SetClassLong (hwnd, GCL_HICON, HandleToLong (hIcon));

  SetWindowText (hwnd, pBrowserWindowInit->browserWindow.label);

  pIUnknown = NULL;

  destroy = & (pBrowserWindowInit->browserObject->destroy);

  while (IsWindow (hwnd)) {

    if (pIUnknown == NULL) {
      AtlAxGetControl (hwnd, &pIUnknown);

      if (pIUnknown) {

	hr = CoMarshalInterThreadInterfaceInStream (IID_IUnknown, pIUnknown,
						   pBrowserWindowInit->ppIStream);

	if (FAILED (hr)) {
	  DestroyWindow (hwnd);
	  ReleaseSemaphore (createHwndSem, 1, NULL);
	  codedComError ("Can't marshal document interface", hr);
	}

	ReleaseSemaphore (createHwndSem, 1, NULL);
      }
    }

    while (IsWindow (hwnd) && GetMessage (&msg, NULL, 0, 0)) {
      TranslateMessage (&msg);
      DispatchMessage (&msg);
      if (*destroy) {
	*destroy = FALSE;
	DestroyWindow (hwnd);
      }
    }

    browserCount--;
  }
}

BOOL APIENTRY DllMain (HANDLE hModule, DWORD reason, LPVOID lpReserved)
{

  if (reason == DLL_PROCESS_ATTACH) {

    hInstance = (HINSTANCE)hModule;

    browserHwndMutex = CreateSemaphore (NULL, 1, 1, NULL);
    createHwndSem = CreateSemaphore (NULL, 0, 1, NULL);
    eventSinkMutex = CreateSemaphore (NULL, 1, 1, NULL);

    hIcon = (HICON)LoadImage (hInstance,
			     MAKEINTRESOURCE (MYSTERX_ICON),
			     IMAGE_ICON, 0, 0, 0);

    _Module.Init (NULL, hInstance, &LIBID_ATLLib);
    AtlAxWinInit();

  }
  else if (reason == DLL_PROCESS_DETACH)
    _Module.Term();

  return TRUE;
}

#if defined (MYSTERX_DOTNET)
/// JRM HACKS for CLR
// Note that these must come last because the #include and #import
// both screw up some names used above.

#include <Mscoree.h>
// The import has a useless warning in it.
#pragma warning (disable: 4278)
#import <mscorlib.tlb>
#pragma warning (default: 4278)

// This doesn't appear to be necessary.
//
//  raw_interfaces_only high_property_prefixes ("_get", "_put", "_putref")
//
using namespace mscorlib;

ICorRuntimeHost * pCLR = NULL;

Scheme_Object *
initialize_dotnet_runtime (int argc, Scheme_Object **argv)
{
    HRESULT hr;
    _AppDomain *pDefaultDomain = NULL;
    IUnknown   *pAppDomainPunk = NULL;
    IDispatch  *pAppDomainDispatch = NULL;

    hr = CorBindToRuntimeEx (NULL, // latest version
                             // workspace mode
                             L"wks",
                             // We'll only be running one domain.
                             STARTUP_LOADER_OPTIMIZATION_SINGLE_DOMAIN,
                             CLSID_CorRuntimeHost,
                             IID_ICorRuntimeHost,
                             (void **) &pCLR);

    if (FAILED (hr))
        scheme_signal_error ("%%%%initialize-dotnet-runtime:  CorBindToRuntimeEx() failed.");

    hr = pCLR->Start();
    if (FAILED (hr))
        scheme_signal_error ("%%%%initialize-dotnet-runtime:  CLR failed to start.");

    hr = pCLR->GetDefaultDomain (&pAppDomainPunk);
    if (FAILED (hr) || pAppDomainPunk == NULL)
        scheme_signal_error ("%%%%initialize-dotnet-runtime:  GetDefaultDomain() failed.");

    hr = pAppDomainPunk->QueryInterface (__uuidof (_AppDomain),
                                         (void **) &pDefaultDomain);
    if (FAILED (hr) || pDefaultDomain == NULL)
        scheme_signal_error ("%%%%initialize-dotnet-runtime:  QueryInterface for _AppDomain failed.");
    pDefaultDomain->Release();

    hr = pAppDomainPunk->QueryInterface (IID_IDispatch, (void **) &pAppDomainDispatch);
    if (FAILED (hr) || pAppDomainDispatch == NULL)
        scheme_signal_error ("%%%%initialize-dotnet-runtime:  QueryInterface for IDispatch failed.");

    Scheme_Object * arglist[1] = {scheme_true};

    scheme_apply (mx_unmarshal_strings_as_symbols, 1, arglist);
    scheme_apply (mx_marshal_raw_scheme_objects, 1, arglist);

    return mx_make_idispatch (pAppDomainDispatch);
}

/// END OF JRM HACK
#else
Scheme_Object *
initialize_dotnet_runtime (int argc, Scheme_Object **argv)
{
  scheme_signal_error ("%%%%initialize-dotnet-runtime:  Support for .NET is not available in this image.");
  return scheme_false;
}
#endif
