// htmlutil.cxx

#include "stdafx.h"

#include <objbase.h>
#include <mshtml.h>
#include <initguid.h>
#include <winnls.h>
#include <exdisp.h>

#include "escheme.h"

#include "bstr.h"
#include "myspage.h"
#include "myssink.h"

#include "mysterx.h"

IHTMLElement *findBodyElement(IHTMLDocument2 *pDocument,
			      LPCTSTR tag, LPCTSTR id,int index) {
  IHTMLElementCollection *pTagCollection;
  IHTMLElement *pBody,*pIHTMLElement,*retval;
  IDispatch *pIDispatch;
  long numElts;
  BSTR idAttribute;
  BSTR idBSTR;
  VARIANT variant;
  int count;
  int i;

  // filter elements by tag

  pDocument->get_body(&pBody);

  if (pBody == NULL) {
    scheme_signal_error("Can't find document BODY");
  }

  retval = NULL;

  // special-handling if body element requested

  if (lstrcmpi(tag,TEXT("BODY")) == 0) {
    idAttribute = SysAllocString(L"id");
    idBSTR = textToBSTR(id,lstrlen(id));

    pBody->getAttribute(idAttribute,FALSE,&variant);

    if (variant.vt == VT_BSTR && variant.bstrVal &&
        _wcsicmp(idBSTR,variant.bstrVal) == 0) {
      retval = pBody;
    }

    SysFreeString(idAttribute);
    SysFreeString(idBSTR);

    return retval;
  }

  pTagCollection = getBodyElementsWithTag(pBody,tag);

  pBody->Release();

  // enumerate until we find id or return NULL

  pTagCollection->get_length(&numElts);

  idAttribute = SysAllocString(L"id");
  idBSTR = stringToBSTR(id,strlen(id));

  count = 0;

  for (i = 0; i < numElts; i++) {

    pIDispatch = getElementInCollection(pTagCollection,i);

    pIDispatch->QueryInterface(IID_IHTMLElement,(void **)&pIHTMLElement);

    pIDispatch->Release();

    if (pIHTMLElement == NULL) {
      continue;
    }

    pIHTMLElement->getAttribute(idAttribute,FALSE,&variant);

    if (variant.vt == VT_BSTR && variant.bstrVal &&
        _wcsicmp(idBSTR,variant.bstrVal) == 0) {
      if (count++ == index) {
	retval = pIHTMLElement;
	break;
      }
    }

    pIHTMLElement->Release();

  }

  SysFreeString(idAttribute);
  SysFreeString(idBSTR);

  pTagCollection->Release();

  return retval;

}

IHTMLElementCollection *getBodyElementsWithTag(IHTMLElement *pBody, LPCTSTR tag) {
  IDispatch *pBodyDispatch;
  IDispatch *pObjectsDispatch;
  IHTMLElementCollection *pBodyEltCollection;
  IHTMLElementCollection *pObjectsCollection;
  long numBodyItems;
  BSTR bstr;
  VARIANT variant;
  VARIANT emptyVariant;

  pBody->get_all(&pBodyDispatch);

  pBodyDispatch->QueryInterface(IID_IHTMLElementCollection,(void **)&pBodyEltCollection);

  pBodyDispatch->Release();

  if (pBodyEltCollection == NULL) {
    scheme_signal_error("Can't get body elements");
  }

  pBodyEltCollection->get_length(&numBodyItems);

  bstr = textToBSTR(tag,lstrlen(tag));

  VariantInit(&variant);
  VariantInit(&emptyVariant);

  variant.vt = VT_BSTR;
  variant.bstrVal = bstr;

  pBodyEltCollection->tags(variant,&pObjectsDispatch);

  pBodyEltCollection->Release();
  SysFreeString(bstr);

  if (pObjectsDispatch == NULL) {
    scheme_signal_error("Can't get body objects");
  }

  pObjectsDispatch->QueryInterface(IID_IHTMLElementCollection,(void **)&pObjectsCollection);

  pObjectsDispatch->Release();

  if (pObjectsCollection == NULL) {
    scheme_signal_error("Can't get body elements");
  }

  return pObjectsCollection;
}

IDispatch *getElementInCollection(IHTMLElementCollection *pEltCollection,int ndx) {
  VARIANT variant,emptyVariant;
  IDispatch *pIDispatch;

  VariantInit(&variant);
  VariantInit(&emptyVariant);

  variant.vt = VT_I2;
  variant.iVal = ndx;

  pEltCollection->item(variant,emptyVariant,&pIDispatch);

  if (pIDispatch == NULL) {
    scheme_signal_error("Unable to find element %d in collection",ndx);
  }

  return pIDispatch;
}

IDispatch *getObjectInCollection(IHTMLElementCollection *pObjectCollection,int ndx) {
  IDispatch *pIDispatch;
  IHTMLObjectElement *pIHTMLObjectElement;

  pIDispatch = getElementInCollection(pObjectCollection,ndx);

  // sometimes pIDispatch is HTML element containing the control
  // sometimes it's the control itself
  // apparent bug in DHTML

  pIDispatch->QueryInterface(IID_IHTMLObjectElement,(void **)&pIHTMLObjectElement);

  if (pIHTMLObjectElement != NULL) {

    pIDispatch->Release();

    pIHTMLObjectElement->get_object(&pIDispatch);

    pIHTMLObjectElement->Release();

    if (pIDispatch == NULL) {
      scheme_signal_error("Unable to get object element interface for object");
    }
  }

  return pIDispatch;
}

Scheme_Object *mx_element_focus(int argc,Scheme_Object **argv) {
  HRESULT hr;
  IHTMLElement *pIHTMLElement;
  IHTMLElement2 *pIHTMLElement2;

  pIHTMLElement = MX_ELEMENT_VAL (GUARANTEE_ELEMENT ("element-focus", 0));

  hr = pIHTMLElement->QueryInterface(IID_IHTMLElement2,
				     (void **)&pIHTMLElement2);

  if (hr != S_OK || pIHTMLElement2 == NULL) {
    codedComError("element-focus: Couldn't find IHTMLElement2 interface",hr);
  }

  pIHTMLElement2->focus();

  pIHTMLElement2->Release();

  return scheme_void;
}

// selections

Scheme_Object *mx_element_selection(int argc,Scheme_Object **argv) {
  HRESULT hr;
  IHTMLElement *pIHTMLElement;
  IHTMLSelectElement *pIHTMLSelectElement;
  BSTR selection;

  pIHTMLElement = MX_ELEMENT_VAL (GUARANTEE_ELEMENT ("element-selection", 0));

  hr = pIHTMLElement->QueryInterface(IID_IHTMLSelectElement,
				     (void **)&pIHTMLSelectElement);

  if (hr != S_OK || pIHTMLSelectElement == NULL) {
    codedComError("element-selection: Couldn't find IHTMLSelectElement interface",hr);
  }

  hr = pIHTMLSelectElement->get_value(&selection);

  pIHTMLSelectElement->Release();

  if (hr != S_OK) {
    codedComError("element-selection: Error getting selection value",hr);
  }

  return unmarshalBSTR (selection);
}

Scheme_Object *mx_element_set_selection(int argc,Scheme_Object **argv) {
  HRESULT hr;
  IHTMLElement *pIHTMLElement;
  IHTMLSelectElement *pIHTMLSelectElement;
  BSTR selection;

  GUARANTEE_ELEMENT ("element-set-selection!", 0);
  GUARANTEE_STRSYM ("element-set-selection!", 1);

  pIHTMLElement = MX_ELEMENT_VAL(argv[0]);

  hr = pIHTMLElement->QueryInterface(IID_IHTMLSelectElement,
				     (void **)&pIHTMLSelectElement);

  if (hr != S_OK || pIHTMLSelectElement == NULL) {
    codedComError("element-set-selection!: Couldn't find IHTMLSelectElement interface",hr);
  }

  selection = schemeToBSTR (argv[1]);

  hr = pIHTMLSelectElement->put_value(selection);

  SysFreeString(selection);

  pIHTMLSelectElement->Release();

  if (hr != S_OK) {
    codedComError("element-selection: Error getting selection value",hr);
  }

  return scheme_void;
}

// IHTMLElement wrappers

Scheme_Object *mx_element_stuff_html(int argc,Scheme_Object **argv,WCHAR *where,char *name) {
  IHTMLElement *pIHTMLElement;
  BSTR whereBSTR,htmlBSTR;

  GUARANTEE_ELEMENT (name, 0);
  GUARANTEE_STRSYM (name, 1);

  pIHTMLElement = MX_ELEMENT_VAL(argv[0]);

  whereBSTR = SysAllocString(where);
  htmlBSTR = schemeToBSTR (argv[1]);

  pIHTMLElement->insertAdjacentHTML(whereBSTR,htmlBSTR);

  SysFreeString(whereBSTR);
  SysFreeString(htmlBSTR);

  return scheme_void;
}

Scheme_Object *mx_element_insert_html(int argc,Scheme_Object **argv) {
  return mx_element_stuff_html(argc,argv,L"BeforeBegin","element-insert-html");
}

Scheme_Object *mx_element_append_html(int argc,Scheme_Object **argv) {
  return mx_element_stuff_html(argc,argv,L"AfterEnd","element-append-html");
}

Scheme_Object *mx_element_stuff_text(int argc,Scheme_Object **argv,WCHAR *where,char *name) {
  IHTMLElement *pIHTMLElement;
  BSTR whereBSTR,textBSTR;

  GUARANTEE_ELEMENT (name, 0);
  GUARANTEE_STRSYM (name, 1);

  pIHTMLElement = MX_ELEMENT_VAL(argv[0]);

  whereBSTR = SysAllocString(where);
  textBSTR = schemeToBSTR (argv[1]);

  pIHTMLElement->insertAdjacentText(whereBSTR,textBSTR);

  SysFreeString(whereBSTR);
  SysFreeString(textBSTR);

  return scheme_void;
}

Scheme_Object *mx_element_insert_text(int argc,Scheme_Object **argv) {
  return mx_element_stuff_html(argc,argv,L"BeforeBegin","element-insert-text");
}

Scheme_Object *mx_element_append_text(int argc,Scheme_Object **argv) {
  return mx_element_stuff_html(argc,argv,L"AfterEnd","element-append-text");
}

Scheme_Object *mx_element_replace_html(int argc,Scheme_Object **argv) {
  IHTMLElement *pIHTMLElement;
  BSTR htmlBSTR;

  GUARANTEE_ELEMENT ("element-replace-html", 0);
  GUARANTEE_STRSYM ("element-replace-html", 1);

  if (MX_ELEMENT_VALIDITY(argv[0]) == FALSE) {
    scheme_signal_error("element-replace-html: Element no longer valid");
  }

  MX_ELEMENT_VALIDITY(argv[0]) = FALSE;

  pIHTMLElement = MX_ELEMENT_VAL(argv[0]);

  htmlBSTR = schemeToBSTR (argv[1]);

  pIHTMLElement->put_outerHTML(htmlBSTR);

  pIHTMLElement->Release();

  SysFreeString(htmlBSTR);

  return scheme_void;
}

Scheme_Object *mx_element_get_html(int argc,Scheme_Object **argv) {
  BSTR bstr;
  IHTMLElement *pIHTMLElement;
  Scheme_Object *retval;

  GUARANTEE_ELEMENT ("get-html", 0);

  if (MX_ELEMENT_VALIDITY(argv[0]) == FALSE) {
    scheme_signal_error("get-html: Element no longer valid");
  }

  pIHTMLElement = MX_ELEMENT_VAL(argv[0]);

  pIHTMLElement->get_innerHTML(&bstr);

  retval = BSTRToSchemeString (bstr);

  SysFreeString(bstr);

  return retval;
}

Scheme_Object *mx_element_get_text(int argc,Scheme_Object **argv) {
  BSTR bstr;
  IHTMLElement *pIHTMLElement;
  Scheme_Object *retval;

  GUARANTEE_ELEMENT ("get-text", 0);

  if (MX_ELEMENT_VALIDITY(argv[0]) == FALSE) {
    scheme_signal_error("get-text}: Element no longer valid");
  }

  pIHTMLElement = MX_ELEMENT_VAL(argv[0]);

  pIHTMLElement->get_innerText(&bstr);

  retval = BSTRToSchemeString(bstr);

  SysFreeString(bstr);

  return retval;
}

Scheme_Object *mx_element_attribute(int argc,Scheme_Object **argv) {
  IHTMLElement *pIHTMLElement;
  BSTR attributeBSTR;
  VARIANT variant;

  GUARANTEE_ELEMENT ("element-attribute", 0);
  GUARANTEE_STRSYM ("element-attribute", 1);

  if (MX_ELEMENT_VALIDITY(argv[0]) == FALSE) {
    scheme_signal_error("Element no longer valid");
  }

  pIHTMLElement = MX_ELEMENT_VAL(argv[0]);

  attributeBSTR = schemeToBSTR (argv[1]);

  pIHTMLElement->getAttribute(attributeBSTR,FALSE,&variant);

  SysFreeString(attributeBSTR);

  return variantToSchemeObject(&variant);

}

Scheme_Object *mx_element_set_attribute(int argc,Scheme_Object **argv) {
  IHTMLElement *pIHTMLElement;
  BSTR attributeBSTR;
  VARIANT variant;

  GUARANTEE_ELEMENT ("element-set-attribute!", 0);
  GUARANTEE_STRSYM ("element-set-attribute!", 1);

  if (SCHEME_STRSYMP (argv[2]) == FALSE && SCHEME_INTP(argv[2]) == FALSE &&
#ifdef MZ_USE_SINGLE_FLOATS
      SCHEME_FLTP(argv[2]) == FALSE &&
#endif
      SCHEME_DBLP(argv[2]) == FALSE &&
      argv[2] != scheme_true && argv[2] != scheme_false) {
    scheme_signal_error("Attribute must have a type in {string,symbol,integer,float,double,{#t,#f}}");
  }

  if (MX_ELEMENT_VALIDITY(argv[0]) == FALSE) {
    scheme_signal_error("Element no longer valid");
  }

  pIHTMLElement = MX_ELEMENT_VAL(argv[0]);

  attributeBSTR = schemeToBSTR (argv[1]);

  marshalSchemeValueToVariant(argv[2],&variant);

  pIHTMLElement->setAttribute(attributeBSTR,variant,FALSE);

  SysFreeString(attributeBSTR);

  return scheme_void;

}

Scheme_Object *mx_element_remove_attribute(int argc,Scheme_Object **argv) {
  IHTMLElement *pIHTMLElement;
  BSTR attributeBSTR;
  VARIANT_BOOL success;

  GUARANTEE_ELEMENT ("element-remove-attribute!", 0);
  GUARANTEE_STRSYM ("element-remove-attribute!", 1);

  if (MX_ELEMENT_VALIDITY(argv[0]) == FALSE) {
    scheme_signal_error("Element no longer valid");
  }

  pIHTMLElement = MX_ELEMENT_VAL(argv[0]);

  attributeBSTR = schemeToBSTR (argv[1]);

  pIHTMLElement->removeAttribute(attributeBSTR,FALSE,&success);

  SysFreeString(attributeBSTR);

  if (success == 0) {
    scheme_signal_error("Failure in removing attribute");
  }

  return scheme_void;

}

Scheme_Object *mx_element_tag(int argc,Scheme_Object **argv) {
  IHTMLElement *pIHTMLElement;
  BSTR tagBSTR;
  Scheme_Object *retval;

  GUARANTEE_ELEMENT ("element-tag", 0);

  if (MX_ELEMENT_VALIDITY(argv[0]) == FALSE) {
    scheme_signal_error("Element no longer valid");
  }

  pIHTMLElement = MX_ELEMENT_VAL(argv[0]);

  pIHTMLElement->get_tagName(&tagBSTR);

  retval = unmarshalBSTR (tagBSTR);

  SysFreeString(tagBSTR);

  return retval;
}

Scheme_Object *mx_element_click(int argc,Scheme_Object **argv) {
  IHTMLElement *pIHTMLElement;

  GUARANTEE_ELEMENT ("element-click", 0);

  if (MX_ELEMENT_VALIDITY(argv[0]) == FALSE) {
    scheme_signal_error("Element no longer valid");
  }

  pIHTMLElement = MX_ELEMENT_VAL(argv[0]);

  pIHTMLElement->click();

  return scheme_void;
}

// IHTMLStyle functions via IHTMLElement

IHTMLStyle *styleInterfaceFromElement(Scheme_Object *o) {
  HRESULT hr;
  IHTMLElement *pIHTMLElement;
  IHTMLStyle *pIHTMLStyle;

  if (MX_ELEMENT_VALIDITY(o) == FALSE) {
    scheme_signal_error("Element no longer valid");
  }

  pIHTMLElement = MX_ELEMENT_VAL(o);

  pIHTMLStyle = NULL;

  hr = pIHTMLElement->get_style(&pIHTMLStyle);

  if (hr != S_OK || pIHTMLStyle == NULL) {
    codedComError("Can't get IHTMLStyle interface from element",hr);
  }

  return pIHTMLStyle;
}

    // strings

#define elt_style_string_getter(fun_name,scm_name,dhtml_name) \
Scheme_Object *fun_name(int argc,Scheme_Object **argv) { \
  HRESULT hr; \
  IHTMLStyle *pIHTMLStyle; \
  BSTR bstr; \
  Scheme_Object *retval; \
  pIHTMLStyle = styleInterfaceFromElement (GUARANTEE_ELEMENT (scm_name, 0)); \
  hr = pIHTMLStyle->dhtml_name(&bstr); \
  retval = unmarshalBSTR (bstr); \
  SysFreeString(bstr); \
  pIHTMLStyle->Release(); \
  if (FAILED(hr)) { \
    scheme_signal_error(#dhtml_name " failed with code %X",hr); \
  } \
  return retval; \
}

#define elt_style_string_setter(fun_name,scm_name,dhtml_name) \
Scheme_Object *fun_name(int argc,Scheme_Object **argv) { \
  HRESULT hr; \
  IHTMLStyle *pIHTMLStyle; \
  BSTR bstr; \
  GUARANTEE_ELEMENT (scm_name, 0); \
  GUARANTEE_STRSYM (scm_name, 1);  \
  pIHTMLStyle = styleInterfaceFromElement(argv[0]); \
  bstr = schemeToBSTR (argv[1]); \
  hr = pIHTMLStyle->dhtml_name(bstr); \
  SysFreeString(bstr); \
  pIHTMLStyle->Release(); \
  if (FAILED(hr)) { \
    scheme_signal_error(#dhtml_name " failed with code %X",hr); \
  } \
  return scheme_void; \
}

elt_style_string_getter(mx_element_font_family,"element-font-family",
			get_fontFamily)
elt_style_string_setter(mx_element_set_font_family,"element-set-font-family!",
			put_fontFamily)

elt_style_string_getter(mx_element_font_style,"element-font-style",
			get_fontStyle)
elt_style_string_setter(mx_element_set_font_style,"element-set-font-style!",
			put_fontStyle)

elt_style_string_getter(mx_element_font_variant,"element-font-variant",
			get_fontVariant)
elt_style_string_setter(mx_element_set_font_variant,"element-set-font-variant!",
			put_fontVariant)

elt_style_string_getter(mx_element_font_weight,"element-font-weight",
			get_fontWeight)
elt_style_string_setter(mx_element_set_font_weight,"element-set-font-weight!",
			put_fontWeight)

elt_style_string_getter(mx_element_font,"element-font",
			get_font)
elt_style_string_setter(mx_element_set_font,"element-set-font!",
			put_font)

elt_style_string_getter(mx_element_background,"element-background",
			get_background)
elt_style_string_setter(mx_element_set_background,"element-set-background!",
			put_background)

elt_style_string_getter(mx_element_background_image,
			"element-background-image",
			get_backgroundImage)
elt_style_string_setter(mx_element_set_background_image,
			"element-set-background-image!",
			put_backgroundImage)

elt_style_string_getter(mx_element_background_repeat,
			"element-background-repeat",
			get_backgroundRepeat)
elt_style_string_setter(mx_element_set_background_repeat,
			"element-set-background-repeat!",
			put_backgroundRepeat)

elt_style_string_getter(mx_element_background_attachment,
			"element-background-attachment",
			get_backgroundAttachment)
elt_style_string_setter(mx_element_set_background_attachment,
			"element-set-background-attachment!",
			put_backgroundAttachment)

elt_style_string_getter(mx_element_background_position,
			"element-background-position",
			get_backgroundPosition)
elt_style_string_setter(mx_element_set_background_position,
			"element-set-background-position!",
			put_backgroundPosition)

elt_style_string_getter(mx_element_text_decoration,
			"element-text-decoration",
			get_textDecoration)
elt_style_string_setter(mx_element_set_text_decoration,
			"element-set-text-decoration!",
			put_textDecoration)

elt_style_string_getter(mx_element_text_transform,
			"element-text-transform",
			get_textTransform)
elt_style_string_setter(mx_element_set_text_transform,
			"element-set-text-transform!",
			put_textTransform)

elt_style_string_getter(mx_element_text_align,
			"element-text-align",
			get_textAlign)
elt_style_string_setter(mx_element_set_text_align,
			"element-set-text-align!",
			put_textAlign)

elt_style_string_getter(mx_element_margin,
			"element-margin",
			get_margin)
elt_style_string_setter(mx_element_set_margin,
			"element-set-margin!",
			put_margin)

elt_style_string_getter(mx_element_padding,
			"element-padding",
			get_padding)
elt_style_string_setter(mx_element_set_padding,
			"element-set-padding!",
			put_padding)

elt_style_string_getter(mx_element_border,
			"element-border",
			get_border)
elt_style_string_setter(mx_element_set_border,
			"element-set-border!",
			put_border)

elt_style_string_getter(mx_element_border_top,
			"element-border-top",
			get_borderTop)
elt_style_string_setter(mx_element_set_border_top,
			"element-set-border-top!",
			put_borderTop)

elt_style_string_getter(mx_element_border_left,
			"element-border-left",
			get_borderLeft)
elt_style_string_setter(mx_element_set_border_left,
			"element-set-border-left!",
			put_borderLeft)

elt_style_string_getter(mx_element_border_right,
			"element-border-right",
			get_borderRight)
elt_style_string_setter(mx_element_set_border_right,
			"element-set-border-right!",
			put_borderRight)

elt_style_string_getter(mx_element_border_bottom,
			"element-border-bottom",
			get_borderBottom)
elt_style_string_setter(mx_element_set_border_bottom,
			"element-set-border-bottom!",
			put_borderBottom)

elt_style_string_getter(mx_element_border_color,
			"element-border-color",
			get_borderColor)
elt_style_string_setter(mx_element_set_border_color,
			"element-set-border-color!",
			put_borderColor)

elt_style_string_getter(mx_element_border_width,
			"element-border-width",
			get_borderWidth)
elt_style_string_setter(mx_element_set_border_width,
			"element-set-border-width!",
			put_borderWidth)

elt_style_string_getter(mx_element_border_style,
			"element-border-style",
			get_borderStyle)
elt_style_string_setter(mx_element_set_border_style,
			"element-set-border-style!",
			put_borderStyle)

elt_style_string_getter(mx_element_border_top_style,
			"element-border-top-style",
			get_borderTopStyle)
elt_style_string_setter(mx_element_set_border_top_style,
			"element-set-border-top-style!",
			put_borderTopStyle)

elt_style_string_getter(mx_element_border_bottom_style,
			"element-border-bottom-style",
			get_borderBottomStyle)
elt_style_string_setter(mx_element_set_border_bottom_style,
			"element-set-border-bottom-style!",
			put_borderBottomStyle)

elt_style_string_getter(mx_element_border_left_style,
			"element-border-left-style",
			get_borderLeftStyle)
elt_style_string_setter(mx_element_set_border_left_style,
			"element-set-border-left-style!",
			put_borderLeftStyle)

elt_style_string_getter(mx_element_border_right_style,
			"element-border-right-style",
			get_borderRightStyle)
elt_style_string_setter(mx_element_set_border_right_style,
			"element-set-border-right-style!",
			put_borderRightStyle)

elt_style_string_getter(mx_element_style_float,
			"element-style-float",
			get_styleFloat)
elt_style_string_setter(mx_element_set_style_float,
			"element-set-style-float!",
			put_styleFloat)

elt_style_string_getter(mx_element_clear,
			"element-clear",
			get_clear)
elt_style_string_setter(mx_element_set_clear,
			"element-set-clear!",
			put_clear)

elt_style_string_getter(mx_element_display,
			"element-display",
			get_display)
elt_style_string_setter(mx_element_set_display,
			"element-set-display!",
			put_display)

elt_style_string_getter(mx_element_visibility,
			"element-visibility",
			get_visibility)
elt_style_string_setter(mx_element_set_visibility,
			"element-set-visibility!",
			put_visibility)

elt_style_string_getter(mx_element_list_style_type,
			"element-list-style-type",
			get_listStyleType)
elt_style_string_setter(mx_element_set_list_style_type,
			"element-set-list-style-type!",
			put_listStyleType)

elt_style_string_getter(mx_element_list_style_position,
			"element-list-style-position",
			get_listStylePosition)
elt_style_string_setter(mx_element_set_list_style_position,
			"element-set-list-style-position!",
			put_listStylePosition)

elt_style_string_getter(mx_element_list_style_image,
			"element-list-style-image",
			get_listStyleImage)
elt_style_string_setter(mx_element_set_list_style_image,
			"element-set-list-style-image!",
			put_listStyleImage)

elt_style_string_getter(mx_element_list_style,
			"element-list-style",
			get_listStyle)
elt_style_string_setter(mx_element_set_list_style,
			"element-set-list-style!",
			put_listStyle)

// note: no setter here
elt_style_string_getter(mx_element_position,
			"element-position",
			get_position)

elt_style_string_getter(mx_element_overflow,
			"element-overflow",
			get_overflow)
elt_style_string_setter(mx_element_set_overflow,
			"element-set-overflow!",
			put_overflow)

elt_style_string_getter(mx_element_pagebreak_before,
			"element-pagebreak-before",
			get_pageBreakBefore)
elt_style_string_setter(mx_element_set_pagebreak_before,
			"element-set-pagebreak-before!",
			put_pageBreakBefore)

elt_style_string_getter(mx_element_pagebreak_after,
			"element-pagebreak-after",
			get_pageBreakAfter)
elt_style_string_setter(mx_element_set_pagebreak_after,
			"element-set-pagebreak-after!",
			put_pageBreakAfter)

elt_style_string_getter(mx_element_css_text,
			"element-css-text",
			get_cssText)
elt_style_string_setter(mx_element_set_css_text,
			"element-set-css_text!",
			put_cssText)

elt_style_string_getter(mx_element_cursor,
			"element-cursor",
			get_cursor)
elt_style_string_setter(mx_element_set_cursor,
			"element-set-cursor!",
			put_cursor)

elt_style_string_getter(mx_element_clip,
			"element-clip",
			get_clip)
elt_style_string_setter(mx_element_set_clip,
			"element-set-clip!",
			put_clip)

elt_style_string_getter(mx_element_filter,
			"element-filter",
			get_filter)
elt_style_string_setter(mx_element_set_filter,
			"element-set-filter!",
			put_filter)

// note: no setter here
elt_style_string_getter(mx_element_style_string,
			"element-style-string",
			toString)

  // bools

#define elt_style_bool_getter(fun_name,scm_name,dhtml_name) \
Scheme_Object *fun_name(int argc,Scheme_Object **argv) { \
  HRESULT hr; \
  IHTMLStyle *pIHTMLStyle; \
  VARIANT_BOOL boolVal; \
  pIHTMLStyle = styleInterfaceFromElement (GUARANTEE_ELEMENT (scm_name, 0)); \
  hr = pIHTMLStyle->dhtml_name(&boolVal); \
  pIHTMLStyle->Release(); \
  if (FAILED(hr)) { \
    scheme_signal_error(#dhtml_name " failed with code %X",hr); \
  } \
  return (boolVal == 0) ? scheme_false : scheme_true; \
}

#define elt_style_bool_setter(fun_name,scm_name,dhtml_name) \
Scheme_Object *fun_name(int argc,Scheme_Object **argv) { \
  HRESULT hr; \
  IHTMLStyle *pIHTMLStyle; \
  VARIANT_BOOL boolVal;	\
  pIHTMLStyle = styleInterfaceFromElement (GUARANTEE_ELEMENT (scm_name, 0)); \
  boolVal = (argv[1] == scheme_false) ? 0 : -1; \
  hr = pIHTMLStyle->dhtml_name(boolVal); \
  pIHTMLStyle->Release(); \
  if (FAILED(hr)) { \
    scheme_signal_error(#dhtml_name " failed with code %X",hr); \
  } \
  return scheme_void; \
}

elt_style_bool_getter(mx_element_text_decoration_none,
		      "element-text-decoration-none",
		      get_textDecorationNone)
elt_style_bool_setter(mx_element_set_text_decoration_none,
		      "element-set-text-decoration-none!",
		      put_textDecorationNone)

elt_style_bool_getter(mx_element_text_decoration_underline,
		      "element-text-decoration-underline",
		      get_textDecorationUnderline)
elt_style_bool_setter(mx_element_set_text_decoration_underline,
		      "element-set-text-decoration-underline!",
		      put_textDecorationUnderline)

elt_style_bool_getter(mx_element_text_decoration_overline,
		      "element-text-decoration-overline",
		      get_textDecorationOverline)
elt_style_bool_setter(mx_element_set_text_decoration_overline,
		      "element-set-text-decoration-overline!",
		      put_textDecorationOverline)

elt_style_bool_getter(mx_element_text_decoration_linethrough,
		      "element-text-decoration-linethrough",
		      get_textDecorationLineThrough)
elt_style_bool_setter(mx_element_set_text_decoration_linethrough,
		      "element-set-text-decoration-linethrough!",
		      put_textDecorationLineThrough)

elt_style_bool_getter(mx_element_text_decoration_blink,
		      "element-text-decoration-blink",
		      get_textDecorationBlink)
elt_style_bool_setter(mx_element_set_text_decoration_blink,
		      "element-set-text-decoration-blink!",
		      put_textDecorationBlink)

  // longs

#define elt_style_long_getter(fun_name,scm_name,dhtml_name) \
Scheme_Object *fun_name(int argc,Scheme_Object **argv) { \
  HRESULT hr; \
  IHTMLStyle *pIHTMLStyle; \
  long val; \
  pIHTMLStyle = styleInterfaceFromElement (GUARANTEE_ELEMENT (scm_name, 0)); \
  hr = pIHTMLStyle->dhtml_name(&val); \
  pIHTMLStyle->Release(); \
  if (FAILED(hr)) { \
    scheme_signal_error(#dhtml_name " failed with code %X",hr); \
  } \
  return scheme_make_integer(val); \
}

#define elt_style_long_setter(fun_name,scm_name,dhtml_name) \
Scheme_Object *fun_name(int argc,Scheme_Object **argv) { \
  HRESULT hr; \
  IHTMLStyle *pIHTMLStyle; \
  long val; \
  GUARANTEE_ELEMENT (scm_name, 0); \
  GUARANTEE_INTEGER (scm_name, 1); \
  pIHTMLStyle = styleInterfaceFromElement(argv[0]); \
  if (scheme_get_int_val(argv[1],&val) == 0) { \
      scheme_signal_error("Integer argument won't fit in a long"); \
  } \
  hr = pIHTMLStyle->dhtml_name(val); \
  pIHTMLStyle->Release(); \
  if (FAILED(hr)) { \
    scheme_signal_error(#dhtml_name " failed with code %X",hr); \
  } \
  return scheme_void; \
}

elt_style_long_getter(mx_element_pixel_top,
		      "element-pixel-top",
		      get_pixelTop)
elt_style_long_setter(mx_element_set_pixel_top,
		      "element-set-pixel-top!",
		      put_pixelTop)

elt_style_long_getter(mx_element_pixel_left,
		      "element-pixel-left",
		      get_pixelLeft)
elt_style_long_setter(mx_element_set_pixel_left,
		      "element-set-pixel-left!",
		      put_pixelLeft)

elt_style_long_getter(mx_element_pixel_width,
		      "element-pixel-width",
		      get_pixelWidth)
elt_style_long_setter(mx_element_set_pixel_width,
		      "element-set-pixel-width!",
		      put_pixelWidth)

elt_style_long_getter(mx_element_pixel_height,
		      "element-pixel-height",
		      get_pixelHeight)
elt_style_long_setter(mx_element_set_pixel_height,
		      "element-set-pixel-height!",
		      put_pixelHeight)

  // floats

#ifdef MZ_USE_SINGLE_FLOATS
#define elt_style_float_getter(fun_name,scm_name,dhtml_name) \
Scheme_Object *fun_name(int argc,Scheme_Object **argv) { \
  HRESULT hr; \
  IHTMLStyle *pIHTMLStyle; \
  float val; \
  pIHTMLStyle = styleInterfaceFromElement (GUARANTEE_ELEMENT (scm_name, 0)); \
  hr = pIHTMLStyle->dhtml_name(&val); \
  pIHTMLStyle->Release(); \
  if (FAILED(hr)) { \
    scheme_signal_error(#dhtml_name " failed with code %X",hr); \
  } \
  return scheme_make_float(val); \
}

#define elt_style_float_setter(fun_name,scm_name,dhtml_name) \
Scheme_Object *fun_name(int argc,Scheme_Object **argv) { \
  HRESULT hr; \
  IHTMLStyle *pIHTMLStyle; \
  GUARANTEE_ELEMENT (scm_name, 0); \
  GUARANTEE_TYPE (scm_name, 1, SCHEME_FLTP, "float"); \
  pIHTMLStyle = styleInterfaceFromElement(argv[0]); \
  hr = pIHTMLStyle->dhtml_name(SCHEME_FLT_VAL(argv[1]); \
  pIHTMLStyle->Release(); \
  if (FAILED(hr)) { \
    scheme_signal_error(#dhtml_name " failed with code %X",hr); \
  } \
  return scheme_void; \
}
#else
#define elt_style_float_getter(fun_name,scm_name,dhtml_name) \
Scheme_Object *fun_name(int argc,Scheme_Object **argv) { \
  HRESULT hr; \
  IHTMLStyle *pIHTMLStyle; \
  float val; \
  pIHTMLStyle = styleInterfaceFromElement(GUARANTEE_ELEMENT (scm_name, 0)); \
  hr = pIHTMLStyle->dhtml_name(&val); \
  pIHTMLStyle->Release(); \
  if (FAILED(hr)) { \
    scheme_signal_error(#dhtml_name " failed with code %X",hr); \
  } \
  return scheme_make_double((double)val); \
}

#define elt_style_float_setter(fun_name,scm_name,dhtml_name) \
Scheme_Object *fun_name(int argc,Scheme_Object **argv) { \
  HRESULT hr; \
  IHTMLStyle *pIHTMLStyle; \
  GUARANTEE_ELEMENT (scm_name, 0); \
  GUARANTEE_TYPE (scm_name, 1, SCHEME_DBLP, "double"); \
  pIHTMLStyle = styleInterfaceFromElement(argv[0]); \
  hr = pIHTMLStyle->dhtml_name((float)SCHEME_DBL_VAL(argv[1])); \
  pIHTMLStyle->Release(); \
  if (FAILED(hr)) { \
    scheme_signal_error(#dhtml_name " failed with code %X",hr); \
  } \
  return scheme_void; \
}
#endif

elt_style_float_getter(mx_element_pos_top,
		       "element-pos-top",
		       get_posTop)
elt_style_float_setter(mx_element_set_pos_top,
		       "element-set-pos-top!",
		       put_posTop)

elt_style_float_getter(mx_element_pos_left,
		       "element-pos-left",
		       get_posLeft)
elt_style_float_setter(mx_element_set_pos_left,
		       "element-set-pos-left!",
		       put_posLeft)

elt_style_float_getter(mx_element_pos_width,
		       "element-pos-width",
		       get_posWidth)
elt_style_float_setter(mx_element_set_pos_width,
		       "element-set-pos-width!",
		       put_posWidth)

elt_style_float_getter(mx_element_pos_height,
		       "element-pos-height",
		       get_posHeight)
elt_style_float_setter(mx_element_set_pos_height,
		       "element-set-pos-height!",
		       put_posHeight)

  // variants

#define elt_style_variant_getter(fun_name,scm_name,dhtml_name) \
Scheme_Object *fun_name(int argc,Scheme_Object **argv) { \
  HRESULT hr; \
  IHTMLStyle *pIHTMLStyle; \
  VARIANT variant; \
  pIHTMLStyle = styleInterfaceFromElement (GUARANTEE_ELEMENT (scm_name, 0)); \
  hr = pIHTMLStyle->dhtml_name(&variant); \
  pIHTMLStyle->Release(); \
  if (FAILED(hr)) { \
    scheme_signal_error(#dhtml_name " failed with code %X",hr); \
  } \
  return variantToSchemeObject(&variant); \
}

#define elt_style_variant_setter(fun_name,scm_name,dhtml_name) \
Scheme_Object *fun_name(int argc,Scheme_Object **argv) { \
  HRESULT hr; \
  IHTMLStyle *pIHTMLStyle; \
  VARIANT variant; \
  pIHTMLStyle = styleInterfaceFromElement(GUARANTEE_ELEMENT (scm_name, 0)); \
  marshalSchemeValueToVariant(argv[1],&variant); \
  hr = pIHTMLStyle->dhtml_name(variant); \
  pIHTMLStyle->Release(); \
  if (FAILED(hr)) { \
    scheme_signal_error(#dhtml_name " failed with code %X",hr); \
  } \
  return scheme_void; \
}

elt_style_variant_getter(mx_element_font_size,
			 "element-font-size",
			 get_fontSize)
elt_style_variant_setter(mx_element_set_font_size,
			 "element-set-font-size!",
			 put_fontSize)

elt_style_variant_getter(mx_element_color,
			 "element-color",
			 get_color)
elt_style_variant_setter(mx_element_set_color,
			 "element-set-color!",
			 put_color)

elt_style_variant_getter(mx_element_background_color,
			 "element-background-color",
			 get_backgroundColor)
elt_style_variant_setter(mx_element_set_background_color,
			 "element-set-background-color!",
			 put_backgroundColor)

elt_style_variant_getter(mx_element_background_position_x,
			 "element-background-position-x",
			 get_backgroundPositionX)
elt_style_variant_setter(mx_element_set_background_position_x,
			 "element-set-background_position-x!",
			 put_backgroundPositionX)

elt_style_variant_getter(mx_element_background_position_y,
			 "element-background-position-y",
			 get_backgroundPositionY)
elt_style_variant_setter(mx_element_set_background_position_y,
			 "element-set-background_position-y!",
			 put_backgroundPositionY)

elt_style_variant_getter(mx_element_letter_spacing,
			 "element-letter-spacing",
			 get_letterSpacing)
elt_style_variant_setter(mx_element_set_letter_spacing,
			 "element-set-letter-spacing",
			 put_letterSpacing)

elt_style_variant_getter(mx_element_vertical_align,
			 "element-vertical-align",
			 get_verticalAlign)
elt_style_variant_setter(mx_element_set_vertical_align,
			 "element-set-vertical-align",
			 put_verticalAlign)

elt_style_variant_getter(mx_element_text_indent,
			 "element-text-indent",
			 get_textIndent)
elt_style_variant_setter(mx_element_set_text_indent,
			 "element-set-text-indent",
			 put_textIndent)

elt_style_variant_getter(mx_element_line_height,
			 "element-line-height",
			 get_lineHeight)
elt_style_variant_setter(mx_element_set_line_height,
			 "element-set-line-height",
			 put_lineHeight)

elt_style_variant_getter(mx_element_margin_top,
			 "element-margin-top",
			 get_marginTop)
elt_style_variant_setter(mx_element_set_margin_top,
			 "element-set-margin-top",
			 put_marginTop)

elt_style_variant_getter(mx_element_margin_bottom,
			 "element-margin-bottom",
			 get_marginBottom)
elt_style_variant_setter(mx_element_set_margin_bottom,
			 "element-set-margin-bottom",
			 put_marginBottom)

elt_style_variant_getter(mx_element_margin_left,
			 "element-margin-left",
			 get_marginLeft)
elt_style_variant_setter(mx_element_set_margin_left,
			 "element-set-margin-left",
			 put_marginLeft)

elt_style_variant_getter(mx_element_margin_right,
			 "element-margin-right",
			 get_marginRight)
elt_style_variant_setter(mx_element_set_margin_right,
			 "element-set-margin-right",
			 put_marginRight)

elt_style_variant_getter(mx_element_padding_top,
			 "element-padding-top",
			 get_paddingTop)
elt_style_variant_setter(mx_element_set_padding_top,
			 "element-set-padding-top",
			 put_paddingTop)

elt_style_variant_getter(mx_element_padding_bottom,
			 "element-padding-bottom",
			 get_paddingBottom)
elt_style_variant_setter(mx_element_set_padding_bottom,
			 "element-set-padding-bottom",
			 put_paddingBottom)

elt_style_variant_getter(mx_element_padding_left,
			 "element-padding-left",
			 get_paddingLeft)
elt_style_variant_setter(mx_element_set_padding_left,
			 "element-set-padding-left",
			 put_paddingLeft)

elt_style_variant_getter(mx_element_padding_right,
			 "element-padding-right",
			 get_paddingRight)
elt_style_variant_setter(mx_element_set_padding_right,
			 "element-set-padding-right",
			 put_paddingRight)

elt_style_variant_getter(mx_element_border_top_color,
			 "element-border-top-color",
			 get_borderTopColor)
elt_style_variant_setter(mx_element_set_border_top_color,
			 "element-set-border-top-color",
			 put_borderTopColor)

elt_style_variant_getter(mx_element_border_bottom_color,
			 "element-border-bottom-color",
			 get_borderBottomColor)
elt_style_variant_setter(mx_element_set_border_bottom_color,
			 "element-set-border-bottom-color",
			 put_borderBottomColor)

elt_style_variant_getter(mx_element_border_left_color,
			 "element-border-left-color",
			 get_borderLeftColor)
elt_style_variant_setter(mx_element_set_border_left_color,
			 "element-set-border-left-color",
			 put_borderLeftColor)

elt_style_variant_getter(mx_element_border_right_color,
			 "element-border-right-color",
			 get_borderRightColor)
elt_style_variant_setter(mx_element_set_border_right_color,
			 "element-set-border-right-color",
			 put_borderRightColor)

elt_style_variant_getter(mx_element_border_top_width,
			 "element-border-top-width",
			 get_borderTopWidth)
elt_style_variant_setter(mx_element_set_border_top_width,
			 "element-set-border-top-width",
			 put_borderTopWidth)

elt_style_variant_getter(mx_element_border_bottom_width,
			 "element-border-bottom-width",
			 get_borderBottomWidth)
elt_style_variant_setter(mx_element_set_border_bottom_width,
			 "element-set-border-bottom-width",
			 put_borderBottomWidth)

elt_style_variant_getter(mx_element_border_left_width,
			 "element-border-left-width",
			 get_borderLeftWidth)
elt_style_variant_setter(mx_element_set_border_left_width,
			 "element-set-border-left-width",
			 put_borderLeftWidth)

elt_style_variant_getter(mx_element_border_right_width,
			 "element-border-right-width",
			 get_borderRightWidth)
elt_style_variant_setter(mx_element_set_border_right_width,
			 "element-set-border-right-width",
			 put_borderRightWidth)

elt_style_variant_getter(mx_element_width,
			 "element-width",
			 get_width)
elt_style_variant_setter(mx_element_set_width,
			 "element-set-width",
			 put_width)

elt_style_variant_getter(mx_element_height,
			 "element-height",
			 get_height)
elt_style_variant_setter(mx_element_set_height,
			 "element-set-height",
			 put_height)

elt_style_variant_getter(mx_element_top,
			 "element-top",
			 get_top)
elt_style_variant_setter(mx_element_set_top,
			 "element-set-top",
			 put_top)

elt_style_variant_getter(mx_element_left,
			 "element-left",
			 get_left)
elt_style_variant_setter(mx_element_set_left,
			 "element-set-left",
			 put_left)

elt_style_variant_getter(mx_element_z_index,
			 "element-z-index",
			 get_zIndex)
elt_style_variant_setter(mx_element_set_z_index,
			 "element-set-z-index",
			 put_zIndex)
