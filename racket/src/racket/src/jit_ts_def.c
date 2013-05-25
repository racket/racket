#define define_ts_siS_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g8, int g9, Scheme_Object** g10) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_siS_s("[" #id "]", src_type, id, g8, g9, g10); \
  else \
    return id(g8, g9, g10); \
}
#define define_ts_iSs_s(id, src_type) \
static Scheme_Object* ts_ ## id(int g11, Scheme_Object** g12, Scheme_Object* g13) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_iSs_s("[" #id "]", src_type, id, g11, g12, g13); \
  else \
    return id(g11, g12, g13); \
}
#define define_ts_s_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g14) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_s_s("[" #id "]", src_type, id, g14); \
  else \
    return id(g14); \
}
#define define_ts_n_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Native_Closure_Data* g15) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_n_s("[" #id "]", src_type, id, g15); \
  else \
    return id(g15); \
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
static Scheme_Object* ts_ ## id(Scheme_Object* g16, Scheme_Object* g17) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_ss_s("[" #id "]", src_type, id, g16, g17); \
  else \
    return id(g16, g17); \
}
#define define_ts_ssi_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g18, Scheme_Object* g19, int g20) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_ssi_s("[" #id "]", src_type, id, g18, g19, g20); \
  else \
    return id(g18, g19, g20); \
}
#define define_ts_tt_s(id, src_type) \
static Scheme_Object* ts_ ## id(const Scheme_Object* g21, const Scheme_Object* g22) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_tt_s("[" #id "]", src_type, id, g21, g22); \
  else \
    return id(g21, g22); \
}
#define define_ts_ss_m(id, src_type) \
static MZ_MARK_STACK_TYPE ts_ ## id(Scheme_Object* g23, Scheme_Object* g24) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_ss_m("[" #id "]", src_type, id, g23, g24); \
  else \
    return id(g23, g24); \
}
#define define_ts_Sl_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object** g25, intptr_t g26) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_Sl_s("[" #id "]", src_type, id, g25, g26); \
  else \
    return id(g25, g26); \
}
#define define_ts_l_s(id, src_type) \
static Scheme_Object* ts_ ## id(intptr_t g27) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_l_s("[" #id "]", src_type, id, g27); \
  else \
    return id(g27); \
}
#define define_ts_bsi_v(id, src_type) \
static void ts_ ## id(Scheme_Bucket* g28, Scheme_Object* g29, int g30) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_bsi_v("[" #id "]", src_type, id, g28, g29, g30); \
  else \
     id(g28, g29, g30); \
}
#define define_ts_iiS_v(id, src_type) \
static void ts_ ## id(int g31, int g32, Scheme_Object** g33) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_iiS_v("[" #id "]", src_type, id, g31, g32, g33); \
  else \
     id(g31, g32, g33); \
}
#define define_ts_ss_v(id, src_type) \
static void ts_ ## id(Scheme_Object* g34, Scheme_Object* g35) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_ss_v("[" #id "]", src_type, id, g34, g35); \
  else \
     id(g34, g35); \
}
#define define_ts_b_v(id, src_type) \
static void ts_ ## id(Scheme_Bucket* g36) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_b_v("[" #id "]", src_type, id, g36); \
  else \
     id(g36); \
}
#define define_ts_sl_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g37, intptr_t g38) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_sl_s("[" #id "]", src_type, id, g37, g38); \
  else \
    return id(g37, g38); \
}
#define define_ts_iS_s(id, src_type) \
static Scheme_Object* ts_ ## id(int g39, Scheme_Object** g40) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_iS_s("[" #id "]", src_type, id, g39, g40); \
  else \
    return id(g39, g40); \
}
#define define_ts_S_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object** g41) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_S_s("[" #id "]", src_type, id, g41); \
  else \
    return id(g41); \
}
#define define_ts_s_v(id, src_type) \
static void ts_ ## id(Scheme_Object* g42) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_s_v("[" #id "]", src_type, id, g42); \
  else \
     id(g42); \
}
#define define_ts_iSi_s(id, src_type) \
static Scheme_Object* ts_ ## id(int g43, Scheme_Object** g44, int g45) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_iSi_s("[" #id "]", src_type, id, g43, g44, g45); \
  else \
    return id(g43, g44, g45); \
}
#define define_ts_siS_v(id, src_type) \
static void ts_ ## id(Scheme_Object* g46, int g47, Scheme_Object** g48) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_siS_v("[" #id "]", src_type, id, g46, g47, g48); \
  else \
     id(g46, g47, g48); \
}
#define define_ts_z_p(id, src_type) \
static void* ts_ ## id(size_t g49) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_z_p("[" #id "]", src_type, id, g49); \
  else \
    return id(g49); \
}
#define define_ts_si_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g50, int g51) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_si_s("[" #id "]", src_type, id, g50, g51); \
  else \
    return id(g50, g51); \
}
#define define_ts_sis_v(id, src_type) \
static void ts_ ## id(Scheme_Object* g52, int g53, Scheme_Object* g54) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_sis_v("[" #id "]", src_type, id, g52, g53, g54); \
  else \
     id(g52, g53, g54); \
}
#define define_ts_ss_i(id, src_type) \
static int ts_ ## id(Scheme_Object* g55, Scheme_Object* g56) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_ss_i("[" #id "]", src_type, id, g55, g56); \
  else \
    return id(g55, g56); \
}
#define define_ts_iSp_v(id, src_type) \
static void ts_ ## id(int g57, Scheme_Object** g58, void* g59) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_iSp_v("[" #id "]", src_type, id, g57, g58, g59); \
  else \
     id(g57, g58, g59); \
}
#define define_ts_sss_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g60, Scheme_Object* g61, Scheme_Object* g62) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_sss_s("[" #id "]", src_type, id, g60, g61, g62); \
  else \
    return id(g60, g61, g62); \
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
