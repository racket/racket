#define define_ts_siS_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g12, int g13, Scheme_Object** g14) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_siS_s("[" #id "]", src_type, id, g12, g13, g14); \
  else \
    return id(g12, g13, g14); \
}
#define define_ts_iSs_s(id, src_type) \
static Scheme_Object* ts_ ## id(int g15, Scheme_Object** g16, Scheme_Object* g17) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_iSs_s("[" #id "]", src_type, id, g15, g16, g17); \
  else \
    return id(g15, g16, g17); \
}
#define define_ts_s_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g18) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_s_s("[" #id "]", src_type, id, g18); \
  else \
    return id(g18); \
}
#define define_ts_n_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Native_Closure_Data* g19) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_n_s("[" #id "]", src_type, id, g19); \
  else \
    return id(g19); \
}
#define define_ts__s(id, src_type) \
static Scheme_Object* ts_ ## id() \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall__s("[" #id "]", src_type, id); \
  else \
    return id(); \
}
#define define_ts_ss_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g20, Scheme_Object* g21) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_ss_s("[" #id "]", src_type, id, g20, g21); \
  else \
    return id(g20, g21); \
}
#define define_ts_ssi_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g22, Scheme_Object* g23, int g24) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_ssi_s("[" #id "]", src_type, id, g22, g23, g24); \
  else \
    return id(g22, g23, g24); \
}
#define define_ts_tt_s(id, src_type) \
static Scheme_Object* ts_ ## id(const Scheme_Object* g25, const Scheme_Object* g26) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_tt_s("[" #id "]", src_type, id, g25, g26); \
  else \
    return id(g25, g26); \
}
#define define_ts_ss_m(id, src_type) \
static MZ_MARK_STACK_TYPE ts_ ## id(Scheme_Object* g27, Scheme_Object* g28) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_ss_m("[" #id "]", src_type, id, g27, g28); \
  else \
    return id(g27, g28); \
}
#define define_ts_Sl_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object** g29, intptr_t g30) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_Sl_s("[" #id "]", src_type, id, g29, g30); \
  else \
    return id(g29, g30); \
}
#define define_ts_l_s(id, src_type) \
static Scheme_Object* ts_ ## id(intptr_t g31) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_l_s("[" #id "]", src_type, id, g31); \
  else \
    return id(g31); \
}
#define define_ts_bsi_v(id, src_type) \
static void ts_ ## id(Scheme_Bucket* g32, Scheme_Object* g33, int g34) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_bsi_v("[" #id "]", src_type, id, g32, g33, g34); \
  else \
     id(g32, g33, g34); \
}
#define define_ts_iiS_v(id, src_type) \
static void ts_ ## id(int g35, int g36, Scheme_Object** g37) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_iiS_v("[" #id "]", src_type, id, g35, g36, g37); \
  else \
     id(g35, g36, g37); \
}
#define define_ts_ss_v(id, src_type) \
static void ts_ ## id(Scheme_Object* g38, Scheme_Object* g39) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_ss_v("[" #id "]", src_type, id, g38, g39); \
  else \
     id(g38, g39); \
}
#define define_ts_b_v(id, src_type) \
static void ts_ ## id(Scheme_Bucket* g40) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_b_v("[" #id "]", src_type, id, g40); \
  else \
     id(g40); \
}
#define define_ts_sl_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g41, intptr_t g42) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_sl_s("[" #id "]", src_type, id, g41, g42); \
  else \
    return id(g41, g42); \
}
#define define_ts_iS_s(id, src_type) \
static Scheme_Object* ts_ ## id(int g43, Scheme_Object** g44) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_iS_s("[" #id "]", src_type, id, g43, g44); \
  else \
    return id(g43, g44); \
}
#define define_ts_S_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object** g45) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_S_s("[" #id "]", src_type, id, g45); \
  else \
    return id(g45); \
}
#define define_ts_s_v(id, src_type) \
static void ts_ ## id(Scheme_Object* g46) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_s_v("[" #id "]", src_type, id, g46); \
  else \
     id(g46); \
}
#define define_ts_iSi_s(id, src_type) \
static Scheme_Object* ts_ ## id(int g47, Scheme_Object** g48, int g49) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_iSi_s("[" #id "]", src_type, id, g47, g48, g49); \
  else \
    return id(g47, g48, g49); \
}
#define define_ts_siS_v(id, src_type) \
static void ts_ ## id(Scheme_Object* g50, int g51, Scheme_Object** g52) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_siS_v("[" #id "]", src_type, id, g50, g51, g52); \
  else \
     id(g50, g51, g52); \
}
#define define_ts_z_p(id, src_type) \
static void* ts_ ## id(size_t g53) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_z_p("[" #id "]", src_type, id, g53); \
  else \
    return id(g53); \
}
#define define_ts_si_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g54, int g55) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_si_s("[" #id "]", src_type, id, g54, g55); \
  else \
    return id(g54, g55); \
}
#define define_ts_sis_v(id, src_type) \
static void ts_ ## id(Scheme_Object* g56, int g57, Scheme_Object* g58) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_sis_v("[" #id "]", src_type, id, g56, g57, g58); \
  else \
     id(g56, g57, g58); \
}
#define define_ts_ss_i(id, src_type) \
static int ts_ ## id(Scheme_Object* g59, Scheme_Object* g60) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_ss_i("[" #id "]", src_type, id, g59, g60); \
  else \
    return id(g59, g60); \
}
#define define_ts_iSp_v(id, src_type) \
static void ts_ ## id(int g61, Scheme_Object** g62, void* g63) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_iSp_v("[" #id "]", src_type, id, g61, g62, g63); \
  else \
     id(g61, g62, g63); \
}
#define define_ts_sss_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g64, Scheme_Object* g65, Scheme_Object* g66) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_sss_s("[" #id "]", src_type, id, g64, g65, g66); \
  else \
    return id(g64, g65, g66); \
}
#define define_ts__v(id, src_type) \
static void ts_ ## id() \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall__v("[" #id "]", src_type, id); \
  else \
     id(); \
}
#define define_ts_iS_v(id, src_type) \
static void ts_ ## id(int g67, Scheme_Object** g68) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_iS_v("[" #id "]", src_type, id, g67, g68); \
  else \
     id(g67, g68); \
}
