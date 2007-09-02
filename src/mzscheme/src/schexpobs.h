
#ifndef __mzscheme_expobs__
#define __mzscheme_expobs__

#define SCHEME_EXPAND_OBSERVE_ENABLE

extern void scheme_call_expand_observe(Scheme_Object *obs, int signal, Scheme_Object *argument);
extern Scheme_Object *scheme_expand_observe_renames(Scheme_Object *env_pair);
extern void scheme_init_expand_observe(Scheme_Env *);
extern Scheme_Object *scheme_get_expand_observe();


#ifdef SCHEME_EXPAND_OBSERVE_ENABLE
# define _SCHEME_EXPOBS(observer, signal, argument)         \
    if (observer) { scheme_call_expand_observe(observer, signal, argument); } else {}
#endif

#ifndef SCHEME_EXPAND_OBSERVE_ENABLE
#define _SCHEME_EXPOBS(observer, signal, argument) \
    ((void)0)
#endif

/* Individual signals */

#define SCHEME_EXPAND_OBSERVE_VISIT(observer,stx)         _SCHEME_EXPOBS(observer,0,stx)
#define SCHEME_EXPAND_OBSERVE_RESOLVE(observer,stx)       _SCHEME_EXPOBS(observer,1,stx)
#define SCHEME_EXPAND_OBSERVE_RETURN(observer,stx)        _SCHEME_EXPOBS(observer,2,stx)
#define SCHEME_EXPAND_OBSERVE_NEXT(observer)              _SCHEME_EXPOBS(observer,3,NULL)
#define SCHEME_EXPAND_OBSERVE_ENTER_LIST(observer,stx)    _SCHEME_EXPOBS(observer,4,stx)
#define SCHEME_EXPAND_OBSERVE_EXIT_LIST(observer,stx)     _SCHEME_EXPOBS(observer,5,stx)
#define SCHEME_EXPAND_OBSERVE_ENTER_PRIM(observer,stx)    _SCHEME_EXPOBS(observer,6,stx)
#define SCHEME_EXPAND_OBSERVE_EXIT_PRIM(observer,stx)     _SCHEME_EXPOBS(observer,7,stx)
#define SCHEME_EXPAND_OBSERVE_ENTER_MACRO(observer,stx)   _SCHEME_EXPOBS(observer,8,stx)
#define SCHEME_EXPAND_OBSERVE_EXIT_MACRO(observer,stx)    _SCHEME_EXPOBS(observer,9,stx)
#define SCHEME_EXPAND_OBSERVE_ENTER_BLOCK(observer,stx)   _SCHEME_EXPOBS(observer,10,stx)
#define SCHEME_EXPAND_OBSERVE_SPLICE(observer,stx)        _SCHEME_EXPOBS(observer,11,stx)
#define SCHEME_EXPAND_OBSERVE_BLOCK_TO_LIST(observer,stx) _SCHEME_EXPOBS(observer,12,stx)
#define SCHEME_EXPAND_OBSERVE_NEXT_GROUP(observer)       _SCHEME_EXPOBS(observer,13,NULL)
#define SCHEME_EXPAND_OBSERVE_BLOCK_TO_LETREC(observer,stx) _SCHEME_EXPOBS(observer,14,stx)
#define SCHEME_EXPAND_OBSERVE_LET_RENAMES(observer,vars,body)        \
  _SCHEME_EXPOBS(observer,16, scheme_make_immutable_pair(vars, body))
#define SCHEME_EXPAND_OBSERVE_LAMBDA_RENAMES(observer,vars,body)     \
  _SCHEME_EXPOBS(observer,17, scheme_make_immutable_pair(vars, body))
#define SCHEME_EXPAND_OBSERVE_CASE_LAMBDA_RENAMES(observer,vars,body)        \
  _SCHEME_EXPOBS(observer,18, scheme_make_immutable_pair(vars, body))
#define SCHEME_EXPAND_OBSERVE_LETREC_SYNTAXES_RENAMES(observer,sbinds,vbinds,body) \
  _SCHEME_EXPOBS(observer,19, scheme_make_immutable_pair(sbinds, scheme_make_immutable_pair(vbinds, body)))
#define SCHEME_EXPAND_OBSERVE_PHASE_UP(observer)           _SCHEME_EXPOBS(observer,20,NULL)

#define SCHEME_EXPAND_OBSERVE_MACRO_PRE_X(observer,stx)   _SCHEME_EXPOBS(observer,21,stx)
#define SCHEME_EXPAND_OBSERVE_MACRO_POST_X(observer,stx)  _SCHEME_EXPOBS(observer,22,stx)

#define SCHEME_EXPAND_OBSERVE_MODULE_BODY(observer,list)  _SCHEME_EXPOBS(observer,23,list)
#define SCHEME_EXPAND_OBSERVE_BLOCK_RENAMES(observer,old,new)        \
  _SCHEME_EXPOBS(observer,24, scheme_make_immutable_pair(old, new))

/* Prim signals */
#define SCHEME_EXPAND_OBSERVE_PRIM_STOP(observer) \
        _SCHEME_EXPOBS(observer,100,NULL)
#define SCHEME_EXPAND_OBSERVE_PRIM_MODULE(observer) \
        _SCHEME_EXPOBS(observer,101,NULL)
#define SCHEME_EXPAND_OBSERVE_PRIM_MODULE_BEGIN(observer) \
         _SCHEME_EXPOBS(observer,102,NULL)
#define SCHEME_EXPAND_OBSERVE_PRIM_DEFINE_SYNTAXES(observer) \
        _SCHEME_EXPOBS(observer,103,NULL)
#define SCHEME_EXPAND_OBSERVE_PRIM_DEFINE_VALUES(observer) \
        _SCHEME_EXPOBS(observer,104,NULL)
#define SCHEME_EXPAND_OBSERVE_PRIM_IF(observer) \
        _SCHEME_EXPOBS(observer,105,NULL)
#define SCHEME_EXPAND_OBSERVE_PRIM_WCM(observer) \
        _SCHEME_EXPOBS(observer,106,NULL)
#define SCHEME_EXPAND_OBSERVE_PRIM_BEGIN(observer) \
        _SCHEME_EXPOBS(observer,107,NULL)
#define SCHEME_EXPAND_OBSERVE_PRIM_BEGIN0(observer) \
        _SCHEME_EXPOBS(observer,108,NULL)
#define SCHEME_EXPAND_OBSERVE_PRIM_APP(observer) \
        _SCHEME_EXPOBS(observer,109,NULL)
#define SCHEME_EXPAND_OBSERVE_PRIM_LAMBDA(observer) \
        _SCHEME_EXPOBS(observer,110,NULL)
#define SCHEME_EXPAND_OBSERVE_PRIM_CASE_LAMBDA(observer) \
        _SCHEME_EXPOBS(observer,111,NULL)
#define SCHEME_EXPAND_OBSERVE_PRIM_LET_VALUES(observer) \
        _SCHEME_EXPOBS(observer,112,NULL)
#define SCHEME_EXPAND_OBSERVE_PRIM_LETREC_VALUES(observer) \
        _SCHEME_EXPOBS(observer,113,NULL)
#define SCHEME_EXPAND_OBSERVE_PRIM_LETREC_SYNTAXES_VALUES(observer) \
        _SCHEME_EXPOBS(observer,114,NULL)
#define SCHEME_EXPAND_OBSERVE_PRIM_DATUM(observer) \
        _SCHEME_EXPOBS(observer,115,NULL)
#define SCHEME_EXPAND_OBSERVE_PRIM_TOP(observer) \
        _SCHEME_EXPOBS(observer,116,NULL)
#define SCHEME_EXPAND_OBSERVE_PRIM_QUOTE(observer) \
        _SCHEME_EXPOBS(observer,117,NULL)
#define SCHEME_EXPAND_OBSERVE_PRIM_QUOTE_SYNTAX(observer) \
        _SCHEME_EXPOBS(observer,118,NULL)
#define SCHEME_EXPAND_OBSERVE_PRIM_REQUIRE(observer) \
        _SCHEME_EXPOBS(observer,119,NULL)
#define SCHEME_EXPAND_OBSERVE_PRIM_REQUIRE_FOR_SYNTAX(observer) \
        _SCHEME_EXPOBS(observer,120,NULL)
#define SCHEME_EXPAND_OBSERVE_PRIM_REQUIRE_FOR_TEMPLATE(observer)   \
        _SCHEME_EXPOBS(observer,121,NULL)
#define SCHEME_EXPAND_OBSERVE_PRIM_PROVIDE(observer) \
        _SCHEME_EXPOBS(observer,122,NULL)

#define SCHEME_EXPAND_OBSERVE_PRIM_SET(observer) \
        _SCHEME_EXPOBS(observer,123,NULL)
#define SCHEME_EXPAND_OBSERVE_PRIM_LETSTAR_VALUES(observer) \
        _SCHEME_EXPOBS(observer,124,NULL)
#define SCHEME_EXPAND_OBSERVE_PRIM_EXPRESSION(obs) \
        _SCHEME_EXPOBS(obs,138,scheme_false)

#define SCHEME_EXPAND_OBSERVE_VARIABLE(observer,e1,e2)       \
        _SCHEME_EXPOBS(observer,125,scheme_make_pair(e1, e2))

#define SCHEME_EXPAND_OBSERVE_ENTER_CHECK(observer,stx) \
        _SCHEME_EXPOBS(observer,126,stx)
#define SCHEME_EXPAND_OBSERVE_EXIT_CHECK(observer,stx) \
        _SCHEME_EXPOBS(observer,127,stx)

#define SCHEME_EXPAND_OBSERVE_LIFT_LOOP(observer,stx) \
        _SCHEME_EXPOBS(observer,128,stx)
#define SCHEME_EXPAND_OBSERVE_LETLIFT_LOOP(observer,stx) \
        _SCHEME_EXPOBS(observer,136,stx)
#define SCHEME_EXPAND_OBSERVE_MODULE_LIFT_LOOP(observe,stxs) \
        _SCHEME_EXPOBS(observer,137,stxs)
#define SCHEME_EXPAND_OBSERVE_MODULE_LIFT_END_LOOP(observer,stx) \
        _SCHEME_EXPOBS(observer,135,stx)

#define SCHEME_EXPAND_OBSERVE_LOCAL_LIFT(obs,id,stx) \
        _SCHEME_EXPOBS(obs,129,scheme_make_pair(id,stx))
#define SCHEME_EXPAND_OBSERVE_LIFT_STATEMENT(obs,stx) \
        _SCHEME_EXPOBS(obs,134,stx)
#define SCHEME_EXPAND_OBSERVE_ENTER_LOCAL(obs,stx) \
        _SCHEME_EXPOBS(obs,130,stx)
#define SCHEME_EXPAND_OBSERVE_EXIT_LOCAL(obs,stx) \
        _SCHEME_EXPOBS(obs,131,stx)
#define SCHEME_EXPAND_OBSERVE_LOCAL_PRE(obs,stx) \
        _SCHEME_EXPOBS(obs,132,stx)
#define SCHEME_EXPAND_OBSERVE_LOCAL_POST(obs,stx) \
        _SCHEME_EXPOBS(obs,133,stx)

#define SCHEME_EXPAND_OBSERVE_ENTER_LOCAL_EXPR(obs,stx) \
        _SCHEME_EXPOBS(obs,139,stx)
#define SCHEME_EXPAND_OBSERVE_EXIT_LOCAL_EXPR(obs,stx,opaque) \
        _SCHEME_EXPOBS(obs,140,scheme_make_pair(stx,opaque))

#define SCHEME_EXPAND_OBSERVE_START_EXPAND(obs) \
        _SCHEME_EXPOBS(obs,141,scheme_false)

#endif
