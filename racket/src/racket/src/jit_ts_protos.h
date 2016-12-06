#define SIG_siS_s 20
typedef Scheme_Object* (*prim_siS_s)(Scheme_Object*, int, Scheme_Object**);
Scheme_Object* scheme_rtcall_siS_s(const char *who, int src_type, prim_siS_s f, Scheme_Object* g358, int g359, Scheme_Object** g360);
#define SIG_iSs_s 21
typedef Scheme_Object* (*prim_iSs_s)(int, Scheme_Object**, Scheme_Object*);
Scheme_Object* scheme_rtcall_iSs_s(const char *who, int src_type, prim_iSs_s f, int g361, Scheme_Object** g362, Scheme_Object* g363);
#define SIG_s_s 22
typedef Scheme_Object* (*prim_s_s)(Scheme_Object*);
Scheme_Object* scheme_rtcall_s_s(const char *who, int src_type, prim_s_s f, Scheme_Object* g364);
#define SIG_n_s 23
typedef Scheme_Object* (*prim_n_s)(Scheme_Native_Lambda*);
Scheme_Object* scheme_rtcall_n_s(const char *who, int src_type, prim_n_s f, Scheme_Native_Lambda* g365);
#define SIG__s 24
typedef Scheme_Object* (*prim__s)();
Scheme_Object* scheme_rtcall__s(const char *who, int src_type, prim__s f );
#define SIG_ss_s 25
typedef Scheme_Object* (*prim_ss_s)(Scheme_Object*, Scheme_Object*);
Scheme_Object* scheme_rtcall_ss_s(const char *who, int src_type, prim_ss_s f, Scheme_Object* g366, Scheme_Object* g367);
#define SIG_ssi_s 26
typedef Scheme_Object* (*prim_ssi_s)(Scheme_Object*, Scheme_Object*, int);
Scheme_Object* scheme_rtcall_ssi_s(const char *who, int src_type, prim_ssi_s f, Scheme_Object* g368, Scheme_Object* g369, int g370);
#define SIG_tt_s 27
typedef Scheme_Object* (*prim_tt_s)(const Scheme_Object*, const Scheme_Object*);
Scheme_Object* scheme_rtcall_tt_s(const char *who, int src_type, prim_tt_s f, const Scheme_Object* g371, const Scheme_Object* g372);
#define SIG_ss_m 28
typedef MZ_MARK_STACK_TYPE (*prim_ss_m)(Scheme_Object*, Scheme_Object*);
MZ_MARK_STACK_TYPE scheme_rtcall_ss_m(const char *who, int src_type, prim_ss_m f, Scheme_Object* g373, Scheme_Object* g374);
#define SIG_Sl_s 29
typedef Scheme_Object* (*prim_Sl_s)(Scheme_Object**, intptr_t);
Scheme_Object* scheme_rtcall_Sl_s(const char *who, int src_type, prim_Sl_s f, Scheme_Object** g375, intptr_t g376);
#define SIG_l_s 30
typedef Scheme_Object* (*prim_l_s)(intptr_t);
Scheme_Object* scheme_rtcall_l_s(const char *who, int src_type, prim_l_s f, intptr_t g377);
#define SIG_bsi_v 31
typedef void (*prim_bsi_v)(Scheme_Bucket*, Scheme_Object*, int);
void scheme_rtcall_bsi_v(const char *who, int src_type, prim_bsi_v f, Scheme_Bucket* g378, Scheme_Object* g379, int g380);
#define SIG_iiS_v 32
typedef void (*prim_iiS_v)(int, int, Scheme_Object**);
void scheme_rtcall_iiS_v(const char *who, int src_type, prim_iiS_v f, int g381, int g382, Scheme_Object** g383);
#define SIG_ss_v 33
typedef void (*prim_ss_v)(Scheme_Object*, Scheme_Object*);
void scheme_rtcall_ss_v(const char *who, int src_type, prim_ss_v f, Scheme_Object* g384, Scheme_Object* g385);
#define SIG_b_v 34
typedef void (*prim_b_v)(Scheme_Bucket*);
void scheme_rtcall_b_v(const char *who, int src_type, prim_b_v f, Scheme_Bucket* g386);
#define SIG_sl_s 35
typedef Scheme_Object* (*prim_sl_s)(Scheme_Object*, intptr_t);
Scheme_Object* scheme_rtcall_sl_s(const char *who, int src_type, prim_sl_s f, Scheme_Object* g387, intptr_t g388);
#define SIG_iS_s 36
typedef Scheme_Object* (*prim_iS_s)(int, Scheme_Object**);
Scheme_Object* scheme_rtcall_iS_s(const char *who, int src_type, prim_iS_s f, int g389, Scheme_Object** g390);
#define SIG_S_s 37
typedef Scheme_Object* (*prim_S_s)(Scheme_Object**);
Scheme_Object* scheme_rtcall_S_s(const char *who, int src_type, prim_S_s f, Scheme_Object** g391);
#define SIG_s_v 38
typedef void (*prim_s_v)(Scheme_Object*);
void scheme_rtcall_s_v(const char *who, int src_type, prim_s_v f, Scheme_Object* g392);
#define SIG_iSi_s 39
typedef Scheme_Object* (*prim_iSi_s)(int, Scheme_Object**, int);
Scheme_Object* scheme_rtcall_iSi_s(const char *who, int src_type, prim_iSi_s f, int g393, Scheme_Object** g394, int g395);
#define SIG_siS_v 40
typedef void (*prim_siS_v)(Scheme_Object*, int, Scheme_Object**);
void scheme_rtcall_siS_v(const char *who, int src_type, prim_siS_v f, Scheme_Object* g396, int g397, Scheme_Object** g398);
#define SIG_Sii_s 41
typedef Scheme_Object* (*prim_Sii_s)(Scheme_Object**, int, int);
Scheme_Object* scheme_rtcall_Sii_s(const char *who, int src_type, prim_Sii_s f, Scheme_Object** g399, int g400, int g401);
#define SIG_z_p 42
typedef void* (*prim_z_p)(size_t);
void* scheme_rtcall_z_p(const char *who, int src_type, prim_z_p f, size_t g402);
#define SIG_si_s 43
typedef Scheme_Object* (*prim_si_s)(Scheme_Object*, int);
Scheme_Object* scheme_rtcall_si_s(const char *who, int src_type, prim_si_s f, Scheme_Object* g403, int g404);
#define SIG_sis_v 44
typedef void (*prim_sis_v)(Scheme_Object*, int, Scheme_Object*);
void scheme_rtcall_sis_v(const char *who, int src_type, prim_sis_v f, Scheme_Object* g405, int g406, Scheme_Object* g407);
#define SIG_ss_i 45
typedef int (*prim_ss_i)(Scheme_Object*, Scheme_Object*);
int scheme_rtcall_ss_i(const char *who, int src_type, prim_ss_i f, Scheme_Object* g408, Scheme_Object* g409);
#define SIG_iSp_v 46
typedef void (*prim_iSp_v)(int, Scheme_Object**, void*);
void scheme_rtcall_iSp_v(const char *who, int src_type, prim_iSp_v f, int g410, Scheme_Object** g411, void* g412);
#define SIG_sss_s 47
typedef Scheme_Object* (*prim_sss_s)(Scheme_Object*, Scheme_Object*, Scheme_Object*);
Scheme_Object* scheme_rtcall_sss_s(const char *who, int src_type, prim_sss_s f, Scheme_Object* g413, Scheme_Object* g414, Scheme_Object* g415);
#define SIG__v 48
typedef void (*prim__v)();
void scheme_rtcall__v(const char *who, int src_type, prim__v f );
#define SIG_iS_v 49
typedef void (*prim_iS_v)(int, Scheme_Object**);
void scheme_rtcall_iS_v(const char *who, int src_type, prim_iS_v f, int g416, Scheme_Object** g417);
