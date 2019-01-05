/* This file should be included by Racket dynamically-loaded
   extenstion files */

#ifndef E_SCHEME_H
#define E_SCHEME_H

#define SCHEME_DIRECT_EMBEDDED 0

#include "scheme.h"

#ifdef CODEFRAGMENT_DYNAMIC_LOAD
#pragma export on
#endif

#ifdef __cplusplus
extern "C" 
{
#endif

extern Scheme_Object *scheme_initialize(Scheme_Env *global_env);
extern Scheme_Object *scheme_reload(Scheme_Env *global_env);
extern Scheme_Object *scheme_module_name(void);

#ifdef __cplusplus
}
#endif

#ifdef CODEFRAGMENT_DYNAMIC_LOAD
#pragma export off
#endif

#endif /* ! E_SCHEME_H */

