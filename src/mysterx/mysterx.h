// mysterx.h

#define MX_PATCH_LEVEL ""
#define MX_VERSION MZSCHEME_VERSION // "-" MX_PATCH_LEVEL

#ifndef _SINKTBL_
#include "sinktbl.h"
#endif

#define sizeray(x) (sizeof(x)/sizeof(*x))

#define MXMAIN "mxmain"

#define MX_PRIM_DECL(f) Scheme_Object *f(int,Scheme_Object **)

#define MX_DEFAULT_WIDTH  (400)
#define MX_DEFAULT_HEIGHT (400)

#define MAXDIRECTARGS (256)

#define DOCHWND_TRIES 40
#define DOCDISPATCH_TRIES 60

#define MAXARRAYDIMS 32

#define CLSIDLEN 38

#define NORETVALINDEX (-1)
#define UNICODE_BUFFER_SIZE 256
#define TYPE_TBL_SIZE 1019

/* extends INVOKEKIND enum in OAIDL.H */
#define INVOKE_EVENT 16

#define NO_LCID (-1)

typedef HRESULT (STDMETHODCALLTYPE *COMPTR)(IDispatch *);

typedef struct _mx_args_ {
  short int numParamsPassed;
  short int numOptParams;
  short int lcidIndex;
  BOOL retvalInParams;
} MX_ARGS_COUNT;

typedef struct _mx_prim_ {
  Scheme_Object *(*c_fun)(int argc,Scheme_Object **);
  char *name;
  short minargs;
  short maxargs;
} MX_PRIM;

typedef struct _scheme_com_obj_ {
  Scheme_Type type;
  BOOL released;
  IDispatch *pIDispatch;
  ITypeInfo *pITypeInfo;
  ITypeInfo *pEventTypeInfo;
  CLSID clsId;
  IConnectionPoint *pIConnectionPoint;
  DWORD connectionCookie;
  ISink *pISink;
} MX_COM_Object;

typedef struct _scheme_com_type_ {
  Scheme_Type type;
  BOOL released;
  ITypeInfo *pITypeInfo;
  CLSID clsId; // of coclass
} MX_COM_Type;

typedef struct _scheme_mx_event_ {
  Scheme_Type type;
  BOOL released;
  IEvent *pEvent;
} MX_Event;

typedef enum _mx_desckind_ {
  funcDesc,varDesc
} MX_DESCKIND;

typedef  HRESULT (STDMETHODCALLTYPE *COMFUNPTR)(IDispatch *);

#define NO_FUNPTR (-1)

typedef struct _method_desc_ {
  Scheme_Type type;
  BOOL released;
  MEMBERID memID;
  ITypeInfo *pITypeInfo;
  ITypeInfo *pITypeInfoImpl;
  IDispatch *pInterface;
  COMPTR funPtr;
  short funOffset; // NO_FUNPTR means no direct call possible
  GUID implGuid;
  MX_DESCKIND descKind;
  union {
    struct funcdescs {
      FUNCDESC *pFuncDesc;
      FUNCDESC *pFuncDescImpl;
    } funcdescs;
    VARDESC *pVarDesc;
  };
} MX_TYPEDESC;

typedef struct _mx_com_data_ {
  Scheme_Type type;
  BOOL released;
  union { // MS representations
    DATE date;
    CY cy;
    SCODE scode;
    IUnknown *pIUnknown;
  };
} MX_COM_Data_Object;

typedef struct _com_browser_ {
  Scheme_Type type;
  BOOL released;
  BOOL destroy;
  HWND hwnd;
  IWebBrowser2 *pIWebBrowser2;
  ISink *pISink;
  IEventQueue *pIEventQueue;
  HANDLE readSem;
} MX_Browser_Object;

typedef struct _com_document_ {
  Scheme_Type type;
  BOOL released;
  IHTMLDocument2 *pIHTMLDocument2;
} MX_Document_Object;

typedef struct _mx_element_ {
  Scheme_Type type;
  BOOL released;
  BOOL valid;
  IHTMLElement *pIHTMLElement;
} MX_Element;

typedef struct _date_ {
  Scheme_Type type;
} MX_Date_Object;

typedef struct _mx_omit_ {
  Scheme_Type type;
} MX_OMIT;

typedef struct _mx_type_tbl_entry_ {
  IDispatch *pIDispatch;
  LPCTSTR name;
  INVOKEKIND invKind;
  MX_TYPEDESC *pTypeDesc;
  struct _mx_type_tbl_entry_ *next;
} MX_TYPE_TBL_ENTRY;

typedef enum _mx_html_where_ {
  insert,append
} MX_HTML_WHERE;

typedef struct _browser_window_ { // parameters a la MrEd frame% class
  const char *label;
  int width;
  int height;
  int x;
  int y;
  DWORD style;
} BROWSER_WINDOW;

typedef struct _browser_window_init_ {
  BROWSER_WINDOW browserWindow;
  IStream **ppIStream; // for passing COM interface back to main thread
  MX_Browser_Object *browserObject;
} BROWSER_WINDOW_INIT;

typedef struct _browser_window_style_option {
  char *name;
  DWORD bits;
  BOOL enable;
} BROWSER_WINDOW_STYLE_OPTION;

// dummy type for "subtyping"
// a managed object has a Scheme_Type, followed by a released flag

typedef struct _managed_obj_ {
  Scheme_Type type;
  BOOL released;
} MX_MANAGED_OBJ;

#define MX_MANAGED_OBJ_RELEASED(o) (((MX_MANAGED_OBJ *)o)->released)

#define TYPE_PRED(o,ty) (!SCHEME_INTP(o) && o->type == ty)

#define MX_COM_OBJP(o) TYPE_PRED(o,mx_com_object_type)
#define MX_COM_OBJ_VAL(o) (((MX_COM_Object *)o)->pIDispatch)
#define MX_COM_OBJ_CONNECTIONPOINT(o) (((MX_COM_Object *)o)->pIConnectionPoint)
#define MX_COM_OBJ_TYPEINFO(o) (((MX_COM_Object *)o)->pITypeInfo)
#define MX_COM_OBJ_CLSID(o) (((MX_COM_Object *)o)->clsId)
#define MX_COM_OBJ_EVENTTYPEINFO(o) (((MX_COM_Object *)o)->pEventTypeInfo)
#define MX_COM_OBJ_EVENTSINK(o) (((MX_COM_Object *)o)->pISink)
#define GUARANTEE_COM_OBJ(fname, argnum) GUARANTEE_TYPE (fname, argnum, MX_COM_OBJP, "com-object")

#define MX_COM_TYPEP(o) TYPE_PRED(o,mx_com_type_type)
#define MX_COM_TYPE_VAL(o) (((MX_COM_Type *)o)->pITypeInfo)
#define GUARANTEE_COM_TYPE(fname, argnum) GUARANTEE_TYPE (fname, argnum, MX_COM_TYPEP, "com-type")

#define MX_COM_OBJ_OR_TYPE(o) (MX_COM_OBJP(o) || MX_COM_TYPEP(o))
#define GUARANTEE_COM_OBJ_OR_TYPE(fname, argnum) \
    GUARANTEE_TYPE (fname, argnum, MX_COM_OBJ_OR_TYPE, "com-object or com-type")

#define MX_DOCUMENTP(o) TYPE_PRED(o,mx_document_type)
#define MX_DOCUMENT_VAL(o) (((MX_Document_Object *)o)->pIHTMLDocument2)
#define GUARANTEE_DOCUMENT(fname, argnum) GUARANTEE_TYPE (fname, argnum, MX_DOCUMENTP, "mx-document")

#define MX_BROWSERP(o) TYPE_PRED(o,mx_browser_type)
#define MX_BROWSER_VAL(o) (((MX_Browser_Object *)o)->pIWebBrowser2)
#define MX_BROWSER_EVENTQUEUE(o) (((MX_Browser_Object *)o)->pIEventQueue)
#define MX_BROWSER_SINK(o) (((MX_Browser_Object *)o)->pISink)
#define MX_BROWSER_HWND(o) (((MX_Browser_Object *)o)->hwnd)
#define GUARANTEE_BROWSER(fname, argnum) GUARANTEE_TYPE (fname, argnum, MX_BROWSERP, "mx-browser")

#define MX_ELEMENTP(o) TYPE_PRED(o,mx_element_type)
#define MX_ELEMENT_VALIDITY(o) (((MX_Element *)o)->valid)
#define MX_ELEMENT_VAL(o) (((MX_Element *)o)->pIHTMLElement)
#define GUARANTEE_ELEMENT(fname, argnum) GUARANTEE_TYPE (fname, argnum, MX_ELEMENTP, "mx-element")

#define MX_EVENTP(o) TYPE_PRED(o,mx_event_type)
#define MX_EVENT_VAL(o) (((MX_Event *)o)->pEvent)
#define GUARANTEE_EVENT(fname, argnum) GUARANTEE_TYPE (fname, argnum, MX_EVENTP, "mx-event")

#define MX_CYP(o) TYPE_PRED(o,mx_com_cy_type)
#define MX_CY_VAL(o) (((MX_COM_Data_Object *)o)->cy)
#define GUARANTEE_CY(fname, argnum) GUARANTEE_TYPE (fname, argnum, MX_CYP, "mx-currency")

#define MX_DATEP(o) TYPE_PRED(o,mx_com_date_type)
#define MX_DATE_VAL(o) (((MX_COM_Data_Object *)o)->date)
#define GUARANTEE_DATE(fname, argnum) GUARANTEE_TYPE (fname, argnum, MX_DATEP, "mx-date")

#define MX_SCODEP(o) TYPE_PRED(o,mx_com_scode_type)
#define MX_SCODE_VAL(o) (((MX_COM_Data_Object *)o)->scode)
#define GUARANTEE_SCODE(fname, argnum) GUARANTEE_TYPE (fname, argnum, MX_SCODEP, "mx-scode")

#define MX_IUNKNOWNP(o) TYPE_PRED(o,mx_com_iunknown_type)
#define MX_IUNKNOWN_VAL(o) (((MX_COM_Data_Object *)o)->pIUnknown)
#define GUARANTEE_IUNKNOWN(fname, argnum) GUARANTEE_TYPE (fname, argnum, MX_SCODEP, "mx-iunknown")

#define SCHEME_NONNEGATIVE(thing) (SCHEME_INTP(thing) && SCHEME_INT_VAL(thing) >= 0)
#define GUARANTEE_NONNEGATIVE(fname, argnum) GUARANTEE_TYPE (fname, argnum, SCHEME_NONNEGATIVE, "non-negative integer")

extern const CLSID emptyClsId;

extern Scheme_Object *scheme_date_type;

extern Scheme_Type mx_com_object_type;
extern Scheme_Type mx_com_type_type;
extern Scheme_Type mx_browser_type;
extern Scheme_Type mx_document_type;
extern Scheme_Type mx_element_type;
extern Scheme_Type mx_event_type;
extern Scheme_Type mx_com_cy_type;
extern Scheme_Type mx_com_date_type;
extern Scheme_Type mx_com_scode_type;
extern Scheme_Type mx_com_iunknown_type;
extern Scheme_Type mx_com_omit_type;
extern Scheme_Type mx_com_typedesc_type;

extern Scheme_Object *hash_table_get;
extern Scheme_Object *hash_table_put;
extern Scheme_Object *hash_table_remove;
extern Scheme_Object *make_hash_table;

Scheme_Object *mx_make_cy(CY *);
Scheme_Object *mx_make_date(DATE *);
Scheme_Object *mx_make_bool(unsigned);
Scheme_Object *mx_make_scode(SCODE);
Scheme_Object *mx_make_idispatch(IDispatch *);
Scheme_Object *mx_make_iunknown(IUnknown *);

BOOL mx_cy_pred(Scheme_Object *);
BOOL mx_date_pred(Scheme_Object *);
BOOL mx_scode_pred(Scheme_Object *);
BOOL mx_comobj_pred(Scheme_Object *);
BOOL mx_iunknown_pred(Scheme_Object *);

CY mx_cy_val(Scheme_Object *);
DATE mx_date_val(Scheme_Object *);
SCODE mx_scode_val(Scheme_Object *);
IDispatch *mx_comobj_val(Scheme_Object *);
IUnknown *mx_iunknown_val(Scheme_Object *);

// version

MX_PRIM_DECL(mx_version);

// browsers

MX_PRIM_DECL(mx_browser_show);
MX_PRIM_DECL(mx_block_while_browsers);
MX_PRIM_DECL(mx_navigate);
MX_PRIM_DECL(mx_go_back);
MX_PRIM_DECL(mx_go_forward);
MX_PRIM_DECL(mx_refresh);
MX_PRIM_DECL(mx_iconize);
MX_PRIM_DECL(mx_restore);
MX_PRIM_DECL(mx_current_url);
MX_PRIM_DECL(mx_register_navigate_handler);
MX_PRIM_DECL(mx_make_browser);
MX_PRIM_DECL(mx_current_document);
MX_PRIM_DECL(mx_print);

// documents

MX_PRIM_DECL(mx_document_title);
MX_PRIM_DECL(mx_find_element);
MX_PRIM_DECL(mx_find_element_by_id_or_name);
MX_PRIM_DECL(mx_elements_with_tag);
MX_PRIM_DECL(mx_document_objects);
MX_PRIM_DECL(mx_coclass_to_html);
MX_PRIM_DECL(mx_progid_to_html);
MX_PRIM_DECL(mx_insert_html);
MX_PRIM_DECL(mx_append_html);
MX_PRIM_DECL(mx_replace_html);
MX_PRIM_DECL(mx_get_event);
MX_PRIM_DECL(mx_document_pred);

// COM

MX_PRIM_DECL(mx_com_terminate);
MX_PRIM_DECL(mx_com_invoke);
MX_PRIM_DECL(mx_com_set_property);
MX_PRIM_DECL(mx_com_get_property);
MX_PRIM_DECL(mx_com_methods);
MX_PRIM_DECL(mx_com_get_properties);
MX_PRIM_DECL(mx_com_set_properties);
MX_PRIM_DECL(mx_com_events);
MX_PRIM_DECL(mx_com_method_type);
MX_PRIM_DECL(mx_com_get_property_type);
MX_PRIM_DECL(mx_com_set_property_type);
MX_PRIM_DECL(mx_com_event_type);
MX_PRIM_DECL(mx_cocreate_instance_from_coclass);
MX_PRIM_DECL(mx_cocreate_instance_from_progid);
MX_PRIM_DECL(mx_coclass);
MX_PRIM_DECL(mx_progid);
MX_PRIM_DECL(mx_set_coclass);
MX_PRIM_DECL(mx_set_coclass_from_progid);
MX_PRIM_DECL(mx_com_object_eq);
MX_PRIM_DECL(mx_com_object_pred);
MX_PRIM_DECL(mx_com_register_object);
MX_PRIM_DECL(mx_com_release_object);
MX_PRIM_DECL(mx_com_add_ref);
MX_PRIM_DECL(mx_com_ref_count);
MX_PRIM_DECL(mx_com_get_object_type);
MX_PRIM_DECL(mx_com_is_a);
MX_PRIM_DECL(mx_com_help);
MX_PRIM_DECL(mx_com_register_event_handler);
MX_PRIM_DECL(mx_com_unregister_event_handler);
MX_PRIM_DECL(mx_all_controls);
MX_PRIM_DECL(mx_all_coclasses);

// COM types

MX_PRIM_DECL(mx_cy_pred_ex);
MX_PRIM_DECL(mx_currency_to_scheme_number);
MX_PRIM_DECL(scheme_number_to_mx_currency);
MX_PRIM_DECL(mx_date_pred_ex);
MX_PRIM_DECL(mx_date_to_scheme_date);
MX_PRIM_DECL(scheme_date_to_mx_date);
MX_PRIM_DECL(mx_scode_pred_ex);
MX_PRIM_DECL(mx_scode_to_scheme_number);
MX_PRIM_DECL(scheme_number_to_mx_scode);
MX_PRIM_DECL(mx_comobj_pred_ex);
MX_PRIM_DECL(mx_iunknown_pred_ex);

// elements

MX_PRIM_DECL(mx_element_insert_html);
MX_PRIM_DECL(mx_element_append_html);
MX_PRIM_DECL(mx_element_replace_html);
MX_PRIM_DECL(mx_element_insert_text);
MX_PRIM_DECL(mx_element_append_text);
MX_PRIM_DECL(mx_element_get_html);
MX_PRIM_DECL(mx_element_get_text);
MX_PRIM_DECL(mx_element_focus);
MX_PRIM_DECL(mx_element_selection);
MX_PRIM_DECL(mx_element_set_selection);
MX_PRIM_DECL(mx_element_attribute);
MX_PRIM_DECL(mx_element_set_attribute);
MX_PRIM_DECL(mx_element_click);
MX_PRIM_DECL(mx_element_tag);
MX_PRIM_DECL(mx_element_font_family);
MX_PRIM_DECL(mx_element_set_font_family);
MX_PRIM_DECL(mx_element_font_style);
MX_PRIM_DECL(mx_element_set_font_style);
MX_PRIM_DECL(mx_element_font_variant);
MX_PRIM_DECL(mx_element_set_font_variant);
MX_PRIM_DECL(mx_element_font_weight);
MX_PRIM_DECL(mx_element_set_font_weight);
MX_PRIM_DECL(mx_element_font);
MX_PRIM_DECL(mx_element_set_font);
MX_PRIM_DECL(mx_element_background);
MX_PRIM_DECL(mx_element_set_background);
MX_PRIM_DECL(mx_element_background_attachment);
MX_PRIM_DECL(mx_element_set_background_attachment);
MX_PRIM_DECL(mx_element_background_image);
MX_PRIM_DECL(mx_element_set_background_image);
MX_PRIM_DECL(mx_element_background_repeat);
MX_PRIM_DECL(mx_element_set_background_repeat);
MX_PRIM_DECL(mx_element_background_position);
MX_PRIM_DECL(mx_element_set_background_position);
MX_PRIM_DECL(mx_element_text_decoration);
MX_PRIM_DECL(mx_element_set_text_decoration);
MX_PRIM_DECL(mx_element_text_transform);
MX_PRIM_DECL(mx_element_set_text_transform);
MX_PRIM_DECL(mx_element_text_align);
MX_PRIM_DECL(mx_element_set_text_align);
MX_PRIM_DECL(mx_element_margin);
MX_PRIM_DECL(mx_element_set_margin);
MX_PRIM_DECL(mx_element_padding);
MX_PRIM_DECL(mx_element_set_padding);
MX_PRIM_DECL(mx_element_border);
MX_PRIM_DECL(mx_element_set_border);
MX_PRIM_DECL(mx_element_border_top);
MX_PRIM_DECL(mx_element_set_border_top);
MX_PRIM_DECL(mx_element_border_bottom);
MX_PRIM_DECL(mx_element_set_border_bottom);
MX_PRIM_DECL(mx_element_border_left);
MX_PRIM_DECL(mx_element_set_border_left);
MX_PRIM_DECL(mx_element_border_right);
MX_PRIM_DECL(mx_element_set_border_right);
MX_PRIM_DECL(mx_element_border_color);
MX_PRIM_DECL(mx_element_set_border_color);
MX_PRIM_DECL(mx_element_border_width);
MX_PRIM_DECL(mx_element_set_border_width);
MX_PRIM_DECL(mx_element_border_style);
MX_PRIM_DECL(mx_element_set_border_style);
MX_PRIM_DECL(mx_element_border_top_style);
MX_PRIM_DECL(mx_element_set_border_top_style);
MX_PRIM_DECL(mx_element_border_bottom_style);
MX_PRIM_DECL(mx_element_set_border_bottom_style);
MX_PRIM_DECL(mx_element_border_left_style);
MX_PRIM_DECL(mx_element_set_border_left_style);
MX_PRIM_DECL(mx_element_border_right_style);
MX_PRIM_DECL(mx_element_set_border_right_style);
MX_PRIM_DECL(mx_element_style_float);
MX_PRIM_DECL(mx_element_set_style_float);
MX_PRIM_DECL(mx_element_clear);
MX_PRIM_DECL(mx_element_set_clear);
MX_PRIM_DECL(mx_element_display);
MX_PRIM_DECL(mx_element_set_display);
MX_PRIM_DECL(mx_element_visibility);
MX_PRIM_DECL(mx_element_set_visibility);
MX_PRIM_DECL(mx_element_list_style_type);
MX_PRIM_DECL(mx_element_set_list_style_type);
MX_PRIM_DECL(mx_element_list_style_position);
MX_PRIM_DECL(mx_element_set_list_style_position);
MX_PRIM_DECL(mx_element_list_style_image);
MX_PRIM_DECL(mx_element_set_list_style_image);
MX_PRIM_DECL(mx_element_list_style);
MX_PRIM_DECL(mx_element_set_list_style);
MX_PRIM_DECL(mx_element_position);
MX_PRIM_DECL(mx_element_overflow);
MX_PRIM_DECL(mx_element_set_overflow);
MX_PRIM_DECL(mx_element_pagebreak_before);
MX_PRIM_DECL(mx_element_set_pagebreak_before);
MX_PRIM_DECL(mx_element_pagebreak_after);
MX_PRIM_DECL(mx_element_set_pagebreak_after);
MX_PRIM_DECL(mx_element_css_text);
MX_PRIM_DECL(mx_element_set_css_text);
MX_PRIM_DECL(mx_element_cursor);
MX_PRIM_DECL(mx_element_set_cursor);
MX_PRIM_DECL(mx_element_clip);
MX_PRIM_DECL(mx_element_set_clip);
MX_PRIM_DECL(mx_element_filter);
MX_PRIM_DECL(mx_element_set_filter);
MX_PRIM_DECL(mx_element_style_string);
MX_PRIM_DECL(mx_element_text_decoration_none);
MX_PRIM_DECL(mx_element_set_text_decoration_none);
MX_PRIM_DECL(mx_element_text_decoration_underline);
MX_PRIM_DECL(mx_element_set_text_decoration_underline);
MX_PRIM_DECL(mx_element_text_decoration_overline);
MX_PRIM_DECL(mx_element_set_text_decoration_overline);
MX_PRIM_DECL(mx_element_text_decoration_linethrough);
MX_PRIM_DECL(mx_element_set_text_decoration_linethrough);
MX_PRIM_DECL(mx_element_text_decoration_blink);
MX_PRIM_DECL(mx_element_set_text_decoration_blink);
MX_PRIM_DECL(mx_element_pixel_top);
MX_PRIM_DECL(mx_element_set_pixel_top);
MX_PRIM_DECL(mx_element_pixel_left);
MX_PRIM_DECL(mx_element_set_pixel_left);
MX_PRIM_DECL(mx_element_pixel_width);
MX_PRIM_DECL(mx_element_set_pixel_width);
MX_PRIM_DECL(mx_element_pixel_height);
MX_PRIM_DECL(mx_element_set_pixel_height);
MX_PRIM_DECL(mx_element_pos_top);
MX_PRIM_DECL(mx_element_set_pos_top);
MX_PRIM_DECL(mx_element_pos_left);
MX_PRIM_DECL(mx_element_set_pos_left);
MX_PRIM_DECL(mx_element_pos_width);
MX_PRIM_DECL(mx_element_set_pos_width);
MX_PRIM_DECL(mx_element_pos_height);
MX_PRIM_DECL(mx_element_set_pos_height);
MX_PRIM_DECL(mx_element_font_size);
MX_PRIM_DECL(mx_element_set_font_size);
MX_PRIM_DECL(mx_element_color);
MX_PRIM_DECL(mx_element_set_color);
MX_PRIM_DECL(mx_element_background_color);
MX_PRIM_DECL(mx_element_set_background_color);
MX_PRIM_DECL(mx_element_background_position_x);
MX_PRIM_DECL(mx_element_set_background_position_x);
MX_PRIM_DECL(mx_element_background_position_y);
MX_PRIM_DECL(mx_element_set_background_position_y);
MX_PRIM_DECL(mx_element_letter_spacing);
MX_PRIM_DECL(mx_element_set_letter_spacing);
MX_PRIM_DECL(mx_element_vertical_align);
MX_PRIM_DECL(mx_element_set_vertical_align);
MX_PRIM_DECL(mx_element_text_indent);
MX_PRIM_DECL(mx_element_set_text_indent);
MX_PRIM_DECL(mx_element_line_height);
MX_PRIM_DECL(mx_element_set_line_height);
MX_PRIM_DECL(mx_element_margin_top);
MX_PRIM_DECL(mx_element_set_margin_top);
MX_PRIM_DECL(mx_element_margin_bottom);
MX_PRIM_DECL(mx_element_set_margin_bottom);
MX_PRIM_DECL(mx_element_margin_left);
MX_PRIM_DECL(mx_element_set_margin_left);
MX_PRIM_DECL(mx_element_margin_right);
MX_PRIM_DECL(mx_element_set_margin_right);
MX_PRIM_DECL(mx_element_padding_top);
MX_PRIM_DECL(mx_element_set_padding_top);
MX_PRIM_DECL(mx_element_padding_bottom);
MX_PRIM_DECL(mx_element_set_padding_bottom);
MX_PRIM_DECL(mx_element_padding_left);
MX_PRIM_DECL(mx_element_set_padding_left);
MX_PRIM_DECL(mx_element_padding_right);
MX_PRIM_DECL(mx_element_set_padding_right);
MX_PRIM_DECL(mx_element_border_top_color);
MX_PRIM_DECL(mx_element_set_border_top_color);
MX_PRIM_DECL(mx_element_border_bottom_color);
MX_PRIM_DECL(mx_element_set_border_bottom_color);
MX_PRIM_DECL(mx_element_border_left_color);
MX_PRIM_DECL(mx_element_set_border_left_color);
MX_PRIM_DECL(mx_element_border_right_color);
MX_PRIM_DECL(mx_element_set_border_right_color);
MX_PRIM_DECL(mx_element_border_top_width);
MX_PRIM_DECL(mx_element_set_border_top_width);
MX_PRIM_DECL(mx_element_border_bottom_width);
MX_PRIM_DECL(mx_element_set_border_bottom_width);
MX_PRIM_DECL(mx_element_border_left_width);
MX_PRIM_DECL(mx_element_set_border_left_width);
MX_PRIM_DECL(mx_element_border_right_width);
MX_PRIM_DECL(mx_element_set_border_right_width);
MX_PRIM_DECL(mx_element_width);
MX_PRIM_DECL(mx_element_set_width);
MX_PRIM_DECL(mx_element_height);
MX_PRIM_DECL(mx_element_set_height);
MX_PRIM_DECL(mx_element_top);
MX_PRIM_DECL(mx_element_set_top);
MX_PRIM_DECL(mx_element_left);
MX_PRIM_DECL(mx_element_set_left);
MX_PRIM_DECL(mx_element_z_index);
MX_PRIM_DECL(mx_element_set_z_index);

// HTML events

MX_PRIM_DECL(mx_event_keypress_pred);
MX_PRIM_DECL(mx_event_keydown_pred);
MX_PRIM_DECL(mx_event_keyup_pred);
MX_PRIM_DECL(mx_event_mousedown_pred);
MX_PRIM_DECL(mx_event_mouseover_pred);
MX_PRIM_DECL(mx_event_mousemove_pred);
MX_PRIM_DECL(mx_event_mouseout_pred);
MX_PRIM_DECL(mx_event_mouseup_pred);
MX_PRIM_DECL(mx_event_click_pred);
MX_PRIM_DECL(mx_event_dblclick_pred);
MX_PRIM_DECL(mx_event_error_pred);
MX_PRIM_DECL(mx_event_tag);
MX_PRIM_DECL(mx_event_id);
MX_PRIM_DECL(mx_event_from_tag);
MX_PRIM_DECL(mx_event_from_id);
MX_PRIM_DECL(mx_event_to_tag);
MX_PRIM_DECL(mx_event_to_id);
MX_PRIM_DECL(mx_event_keycode);
MX_PRIM_DECL(mx_event_shiftkey);
MX_PRIM_DECL(mx_event_ctrlkey);
MX_PRIM_DECL(mx_event_altkey);
MX_PRIM_DECL(mx_event_x);
MX_PRIM_DECL(mx_event_y);
MX_PRIM_DECL(mx_event_pred);
MX_PRIM_DECL(mx_event_keypress_pred);
MX_PRIM_DECL(mx_event_keydown_pred);
MX_PRIM_DECL(mx_event_keyup_pred);
MX_PRIM_DECL(mx_event_mousedown_pred);
MX_PRIM_DECL(mx_event_mouseover_pred);
MX_PRIM_DECL(mx_event_mouseout_pred);
MX_PRIM_DECL(mx_event_mouseup_pred);
MX_PRIM_DECL(mx_event_click_pred);
MX_PRIM_DECL(mx_event_dblclick_pred);
MX_PRIM_DECL(mx_event_error_pred);
MX_PRIM_DECL(mx_block_until_event);
MX_PRIM_DECL(mx_process_win_events);
MX_PRIM_DECL(mx_release_type_table);

MX_PRIM_DECL(initialize_dotnet_runtime);

void browserHwndMsgLoop(LPVOID);
void mx_register_com_object(Scheme_Object *,IDispatch *);
void mx_register_simple_com_object(Scheme_Object *,IUnknown *);
void scheme_release_browser(void *,void *);
void scheme_release_document(void *,void *);
void codedComError(const char *,HRESULT);
IHTMLElementCollection *getBodyElementsWithTag(IHTMLElement *,LPCTSTR);
IDispatch *getElementInCollection(IHTMLElementCollection *,int);
IDispatch *getObjectInCollection(IHTMLElementCollection *,int);
Scheme_Object *variantToSchemeObject(VARIANTARG *);
void marshalSchemeValueToVariant(Scheme_Object *,VARIANTARG *);
void initEventNames(void);
IHTMLElement *findBodyElement(IHTMLDocument2 *,LPCTSTR,LPCTSTR,int);
CLSID getCLSIDFromCoClass (LPCTSTR);
ITypeInfo *eventTypeInfoFromComObject(MX_COM_Object *);
void signalCodedEventSinkError(char *,HRESULT);

// array procedures

Scheme_Object *safeArrayToSchemeVector(SAFEARRAY *);
SAFEARRAY *schemeVectorToSafeArray(Scheme_Object *);

extern MYSSINK_TABLE myssink_table;
extern HINSTANCE hInstance;
extern HICON hIcon;
extern HANDLE browserHwndMutex;
extern HANDLE createHwndSem;
extern HANDLE eventSinkMutex;
extern HWND browserHwnd;
extern BROWSER_WINDOW_STYLE_OPTION styleOptions[6];
extern WCHAR *eventNames[11];

// misc

extern unsigned long browserCount;

// inline assembly

/* for 4-byte values */
#define pusharg(v) \
  __asm { \
   push v \
  }

/* for single-byte values */
#define pushByte(v)  \
  __asm { \
    mov al,v \
  } \
  pusharg(eax)

/* for two-byte values */
#define pushShort(v)  \
  __asm { \
    mov ax,v \
  } \
  pusharg(eax)

/* for 8-byte values */
#define pushWords(v) do \
      { ULONG loWord = (*(ULONG *)(&v)) & 0xFFFFFFFF; \
        ULONG hiWord = (ULONG)((*(ULONGLONG *)(&v)) >> 32);  \
        pusharg(hiWord); \
        pusharg(loWord); \
      } while (0)

// push right to left
// i indexes argv's, j indexes COM type info
#define pushSuppliedArgs(pFuncDesc,numParamsPassed,argc,argv, \
                         argVas,vaPtr,va,i,j,lcidIndex,buff) \
  do { \
  /* j is index into COM type descriptions */ \
  j = argc - 3; \
  if (lcidIndex != NO_LCID && lcidIndex <= j + 1) { \
    j++; \
  } \
  /* i is index into argv */ \
  i = argc - 1; \
  if (j > MAXDIRECTARGS - 1) { \
    scheme_signal_error("Too many arguments to COM method or property"); \
  } \
  vaPtr = argVas + j; \
  for ( ; j >= 0; i--,j--,vaPtr--) { \
    VariantInit(vaPtr); \
    if (j == lcidIndex) { \
      vaPtr->vt = VT_UI4; \
      vaPtr->ulVal = LOCALE_SYSTEM_DEFAULT; \
      i++; \
    } \
    else if (argv[i] == mx_omit_obj) { \
      vaPtr->vt = VT_ERROR; \
      vaPtr->lVal = DISP_E_PARAMNOTFOUND; \
      va = *vaPtr; \
      pushVariant(va); \
      continue; \
    } \
    else { \
      vaPtr->vt = getVarTypeFromElemDesc(&pFuncDesc->lprgelemdescParam[j]); \
      if (vaPtr->vt == VT_VARIANT) { \
	marshalSchemeValueToVariant(argv[i],vaPtr); \
        va = *vaPtr; \
        pushVariant(va); \
        continue; \
      } \
      marshalSchemeValue(argv[i],vaPtr); \
    } \
    va = *vaPtr; \
    pushOneArg(va,buff); \
  }; } while (0)

// push right to left
#define pushOptArgs(pFuncDesc,numParamsPassed,numOptParams, \
                    optArgVas,vaPtr,va,argc,i,j,lcidIndex,buff) \
  do { \
  if (numOptParams > 0) { \
    /* i is index into param type descriptions */ \
    i = numParamsPassed - 1; \
    if (lcidIndex != NO_LCID) { \
      i++; \
    } \
    /* j is size of VARIANT array */ \
    j = i - argc + 3; \
    /* lcid to be handled in supplied loop */ \
    if (lcidIndex != NO_LCID && lcidIndex < argc - 3) { \
      j--; \
    } \
    if (j > MAXDIRECTARGS) { \
      scheme_signal_error("Too many arguments to COM method or property"); \
    } \
    vaPtr = optArgVas + (j - 1); \
    for ( ; j > 0; i--,j--,vaPtr--) { \
      VariantInit(vaPtr); \
      if (isDefaultParam(pFuncDesc,i)) { \
	vaPtr = &(pFuncDesc->lprgelemdescParam[i].paramdesc.pparamdescex->varDefaultValue); \
      } \
      else if (i == lcidIndex) { \
        vaPtr->vt = VT_UI4; \
        vaPtr->ulVal = LOCALE_SYSTEM_DEFAULT; \
      } \
      else { \
	vaPtr->vt = VT_ERROR; \
	vaPtr->lVal = DISP_E_PARAMNOTFOUND; \
        va = *vaPtr; \
        pushVariant(va); \
        continue; \
      } \
      va = *vaPtr; \
      pushOneArg(va,buff); \
    } \
      }; } while (0)

/* VARIANT is 16 bytes */
#define pushVariant(va) do {\
  ULONGLONG loDword,hiDword; \
  loDword = *(ULONGLONG *)&va; \
  hiDword = *((ULONGLONG *)&va + 1); \
  pushWords(hiDword); \
  pushWords(loDword); \
} while (0)

#define pushOneArg(va,buff) \
  do {\
    switch(va.vt) { \
    case VT_I8 : \
      pushWords(va.llVal); \
      break; \
    case  VT_I4 : \
      pusharg(va.lVal); \
      break; \
    case VT_UI1 : \
      pushByte(va.bVal); \
      break; \
    case VT_I2 : \
      pushShort(va.iVal); \
      break; \
    case VT_R4 : \
      pusharg(va.fltVal); \
      break; \
    case VT_R8 : \
      pushWords(va.dblVal); \
      break; \
    case VT_BOOL : \
      pushShort(va.boolVal); \
      break; \
    case VT_ERROR : \
      pusharg(va.scode); \
      break; \
    case VT_CY : \
      pusharg(va.cyVal); \
      break; \
   case VT_DATE : \
      pushWords(va.date); \
      break; \
    case VT_BSTR : \
      pusharg(va.bstrVal); \
      break; \
    case VT_UNKNOWN : \
      pusharg(va.punkVal); \
      break; \
    case VT_DISPATCH : \
      pusharg(va.pdispVal); \
      break; \
    case VT_ARRAY : \
      pusharg(va.parray); \
      break; \
    case VT_BYREF|VT_UI1 : \
      pusharg(va.pbVal); \
      break; \
    case VT_BYREF|VT_I2 : \
      pusharg(va.piVal); \
      break; \
    case VT_BYREF|VT_I4 : \
      pusharg(va.plVal); \
      break; \
    case VT_BYREF|VT_I8 : \
      pusharg(va.pllVal); \
      break; \
    case VT_BYREF|VT_R4 : \
      pusharg(va.pfltVal); \
      break; \
    case VT_BYREF|VT_R8 : \
      pusharg(va.pdblVal); \
      break; \
    case VT_BYREF|VT_BOOL : \
      pusharg(va.pboolVal); \
      break; \
    case VT_BYREF|VT_ERROR : \
      pusharg(va.pscode); \
      break; \
    case VT_BYREF|VT_CY : \
      pusharg(va.pcyVal); \
      break; \
    case VT_BYREF|VT_DATE : \
      pusharg(va.pdate); \
      break; \
    case VT_BYREF|VT_BSTR : \
      pusharg(va.pbstrVal); \
      break; \
    case VT_BYREF|VT_UNKNOWN : \
      pusharg(va.ppunkVal); \
      break; \
    case VT_BYREF|VT_PTR : \
    case VT_BYREF|VT_DISPATCH : \
      pusharg(va.ppdispVal); \
      break; \
    case VT_BYREF|VT_SAFEARRAY : \
    case VT_BYREF|VT_ARRAY : \
      pusharg(va.pparray); \
      break; \
    case VT_BYREF|VT_VARIANT : \
      pusharg(va.pvarVal); \
      break; \
    case VT_I1 : \
      pushByte(va.cVal); \
      break; \
    case VT_UI2 : \
      pushShort(va.uiVal); \
      break; \
    case VT_UI4 : \
      pusharg(va.ulVal); \
      break; \
    case VT_UI8 : \
      pushWords(va.ullVal); \
      break; \
    case VT_INT : \
      pusharg(va.intVal); \
      break; \
    case VT_UINT : \
      pusharg(va.uintVal); \
      break; \
    case VT_VOID : \
      /* put property */ \
      break; \
    case VT_BYREF|VT_I1 : \
      pusharg(va.puiVal); \
      break; \
    case VT_BYREF|VT_UI2 : \
      pusharg(va.puiVal); \
      break; \
    case VT_BYREF|VT_UI4 : \
      pusharg(va.pulVal); \
      break; \
    case VT_BYREF|VT_UI8 : \
      pusharg(va.pullVal); \
      break; \
    case VT_BYREF|VT_INT : \
      pusharg(va.pintVal); \
      break; \
    case VT_BYREF|VT_UINT : \
      pusharg(va.puintVal); \
      break; \
   default : \
      sprintf(buff,"Can't push argument with VARIANT tag = %X",va.vt); \
      scheme_signal_error(buff); }; } while (0)


