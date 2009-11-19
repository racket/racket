#define define_ts_siS_s(id) \
static Scheme_Object* ts_ ## id(Scheme_Object* g7, int g8, Scheme_Object** g9) \
{ \
  START_XFORM_SKIP; \
  if (scheme_use_rtcall) \
    return scheme_rtcall_siS_s(id, g7, g8, g9); \
  else \
    return id(g7, g8, g9); \
  END_XFORM_SKIP; \
}
#define define_ts_iSs_s(id) \
static Scheme_Object* ts_ ## id(int g10, Scheme_Object** g11, Scheme_Object* g12) \
{ \
  START_XFORM_SKIP; \
  if (scheme_use_rtcall) \
    return scheme_rtcall_iSs_s(id, g10, g11, g12); \
  else \
    return id(g10, g11, g12); \
  END_XFORM_SKIP; \
}
#define define_ts_s_s(id) \
static Scheme_Object* ts_ ## id(Scheme_Object* g13) \
{ \
  START_XFORM_SKIP; \
  if (scheme_use_rtcall) \
    return scheme_rtcall_s_s(id, g13); \
  else \
    return id(g13); \
  END_XFORM_SKIP; \
}
#define define_ts_n_s(id) \
static Scheme_Object* ts_ ## id(Scheme_Native_Closure_Data* g14) \
{ \
  START_XFORM_SKIP; \
  if (scheme_use_rtcall) \
    return scheme_rtcall_n_s(id, g14); \
  else \
    return id(g14); \
  END_XFORM_SKIP; \
}
#define define_ts__s(id) \
static Scheme_Object* ts_ ## id() \
{ \
  START_XFORM_SKIP; \
  if (scheme_use_rtcall) \
    return scheme_rtcall__s(id, ); \
  else \
    return id(); \
  END_XFORM_SKIP; \
}
#define define_ts_ss_s(id) \
static Scheme_Object* ts_ ## id(Scheme_Object* g15, Scheme_Object* g16) \
{ \
  START_XFORM_SKIP; \
  if (scheme_use_rtcall) \
    return scheme_rtcall_ss_s(id, g15, g16); \
  else \
    return id(g15, g16); \
  END_XFORM_SKIP; \
}
#define define_ts_ss_m(id) \
static MZ_MARK_STACK_TYPE ts_ ## id(Scheme_Object* g17, Scheme_Object* g18) \
{ \
  START_XFORM_SKIP; \
  if (scheme_use_rtcall) \
    return scheme_rtcall_ss_m(id, g17, g18); \
  else \
    return id(g17, g18); \
  END_XFORM_SKIP; \
}
#define define_ts_Sl_s(id) \
static Scheme_Object* ts_ ## id(Scheme_Object** g19, long g20) \
{ \
  START_XFORM_SKIP; \
  if (scheme_use_rtcall) \
    return scheme_rtcall_Sl_s(id, g19, g20); \
  else \
    return id(g19, g20); \
  END_XFORM_SKIP; \
}
#define define_ts_l_s(id) \
static Scheme_Object* ts_ ## id(long g21) \
{ \
  START_XFORM_SKIP; \
  if (scheme_use_rtcall) \
    return scheme_rtcall_l_s(id, g21); \
  else \
    return id(g21); \
  END_XFORM_SKIP; \
}
#define define_ts_bsi_v(id) \
static void ts_ ## id(Scheme_Bucket* g22, Scheme_Object* g23, int g24) \
{ \
  START_XFORM_SKIP; \
  if (scheme_use_rtcall) \
     scheme_rtcall_bsi_v(id, g22, g23, g24); \
  else \
     id(g22, g23, g24); \
  END_XFORM_SKIP; \
}
#define define_ts_iiS_v(id) \
static void ts_ ## id(int g25, int g26, Scheme_Object** g27) \
{ \
  START_XFORM_SKIP; \
  if (scheme_use_rtcall) \
     scheme_rtcall_iiS_v(id, g25, g26, g27); \
  else \
     id(g25, g26, g27); \
  END_XFORM_SKIP; \
}
#define define_ts_ss_v(id) \
static void ts_ ## id(Scheme_Object* g28, Scheme_Object* g29) \
{ \
  START_XFORM_SKIP; \
  if (scheme_use_rtcall) \
     scheme_rtcall_ss_v(id, g28, g29); \
  else \
     id(g28, g29); \
  END_XFORM_SKIP; \
}
#define define_ts_b_v(id) \
static void ts_ ## id(Scheme_Bucket* g30) \
{ \
  START_XFORM_SKIP; \
  if (scheme_use_rtcall) \
     scheme_rtcall_b_v(id, g30); \
  else \
     id(g30); \
  END_XFORM_SKIP; \
}
#define define_ts_sl_s(id) \
static Scheme_Object* ts_ ## id(Scheme_Object* g31, long g32) \
{ \
  START_XFORM_SKIP; \
  if (scheme_use_rtcall) \
    return scheme_rtcall_sl_s(id, g31, g32); \
  else \
    return id(g31, g32); \
  END_XFORM_SKIP; \
}
#define define_ts_iS_s(id) \
static Scheme_Object* ts_ ## id(int g33, Scheme_Object** g34) \
{ \
  START_XFORM_SKIP; \
  if (scheme_use_rtcall) \
    return scheme_rtcall_iS_s(id, g33, g34); \
  else \
    return id(g33, g34); \
  END_XFORM_SKIP; \
}
#define define_ts_S_s(id) \
static Scheme_Object* ts_ ## id(Scheme_Object** g35) \
{ \
  START_XFORM_SKIP; \
  if (scheme_use_rtcall) \
    return scheme_rtcall_S_s(id, g35); \
  else \
    return id(g35); \
  END_XFORM_SKIP; \
}
#define define_ts_s_v(id) \
static void ts_ ## id(Scheme_Object* g36) \
{ \
  START_XFORM_SKIP; \
  if (scheme_use_rtcall) \
     scheme_rtcall_s_v(id, g36); \
  else \
     id(g36); \
  END_XFORM_SKIP; \
}
#define define_ts_iSi_s(id) \
static Scheme_Object* ts_ ## id(int g37, Scheme_Object** g38, int g39) \
{ \
  START_XFORM_SKIP; \
  if (scheme_use_rtcall) \
    return scheme_rtcall_iSi_s(id, g37, g38, g39); \
  else \
    return id(g37, g38, g39); \
  END_XFORM_SKIP; \
}
#define define_ts_siS_v(id) \
static void ts_ ## id(Scheme_Object* g40, int g41, Scheme_Object** g42) \
{ \
  START_XFORM_SKIP; \
  if (scheme_use_rtcall) \
     scheme_rtcall_siS_v(id, g40, g41, g42); \
  else \
     id(g40, g41, g42); \
  END_XFORM_SKIP; \
}
