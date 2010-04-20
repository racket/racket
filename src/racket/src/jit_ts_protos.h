#define SIG_siS_s 5
typedef Scheme_Object* (*prim_siS_s)(Scheme_Object*, int, Scheme_Object**);
Scheme_Object* scheme_rtcall_siS_s(const char *who, int src_type, prim_siS_s f, Scheme_Object* g133, int g134, Scheme_Object** g135);
#define SIG_iSs_s 6
typedef Scheme_Object* (*prim_iSs_s)(int, Scheme_Object**, Scheme_Object*);
Scheme_Object* scheme_rtcall_iSs_s(const char *who, int src_type, prim_iSs_s f, int g136, Scheme_Object** g137, Scheme_Object* g138);
#define SIG_s_s 7
typedef Scheme_Object* (*prim_s_s)(Scheme_Object*);
Scheme_Object* scheme_rtcall_s_s(const char *who, int src_type, prim_s_s f, Scheme_Object* g139);
#define SIG_n_s 8
typedef Scheme_Object* (*prim_n_s)(Scheme_Native_Closure_Data*);
Scheme_Object* scheme_rtcall_n_s(const char *who, int src_type, prim_n_s f, Scheme_Native_Closure_Data* g140);
#define SIG__s 9
typedef Scheme_Object* (*prim__s)();
Scheme_Object* scheme_rtcall__s(const char *who, int src_type, prim__s f );
#define SIG_ss_s 10
typedef Scheme_Object* (*prim_ss_s)(Scheme_Object*, Scheme_Object*);
Scheme_Object* scheme_rtcall_ss_s(const char *who, int src_type, prim_ss_s f, Scheme_Object* g141, Scheme_Object* g142);
#define SIG_ss_m 11
typedef MZ_MARK_STACK_TYPE (*prim_ss_m)(Scheme_Object*, Scheme_Object*);
MZ_MARK_STACK_TYPE scheme_rtcall_ss_m(const char *who, int src_type, prim_ss_m f, Scheme_Object* g143, Scheme_Object* g144);
#define SIG_Sl_s 12
typedef Scheme_Object* (*prim_Sl_s)(Scheme_Object**, long);
Scheme_Object* scheme_rtcall_Sl_s(const char *who, int src_type, prim_Sl_s f, Scheme_Object** g145, long g146);
#define SIG_l_s 13
typedef Scheme_Object* (*prim_l_s)(long);
Scheme_Object* scheme_rtcall_l_s(const char *who, int src_type, prim_l_s f, long g147);
#define SIG_bsi_v 14
typedef void (*prim_bsi_v)(Scheme_Bucket*, Scheme_Object*, int);
void scheme_rtcall_bsi_v(const char *who, int src_type, prim_bsi_v f, Scheme_Bucket* g148, Scheme_Object* g149, int g150);
#define SIG_iiS_v 15
typedef void (*prim_iiS_v)(int, int, Scheme_Object**);
void scheme_rtcall_iiS_v(const char *who, int src_type, prim_iiS_v f, int g151, int g152, Scheme_Object** g153);
#define SIG_ss_v 16
typedef void (*prim_ss_v)(Scheme_Object*, Scheme_Object*);
void scheme_rtcall_ss_v(const char *who, int src_type, prim_ss_v f, Scheme_Object* g154, Scheme_Object* g155);
#define SIG_b_v 17
typedef void (*prim_b_v)(Scheme_Bucket*);
void scheme_rtcall_b_v(const char *who, int src_type, prim_b_v f, Scheme_Bucket* g156);
#define SIG_sl_s 18
typedef Scheme_Object* (*prim_sl_s)(Scheme_Object*, long);
Scheme_Object* scheme_rtcall_sl_s(const char *who, int src_type, prim_sl_s f, Scheme_Object* g157, long g158);
#define SIG_iS_s 19
typedef Scheme_Object* (*prim_iS_s)(int, Scheme_Object**);
Scheme_Object* scheme_rtcall_iS_s(const char *who, int src_type, prim_iS_s f, int g159, Scheme_Object** g160);
#define SIG_S_s 20
typedef Scheme_Object* (*prim_S_s)(Scheme_Object**);
Scheme_Object* scheme_rtcall_S_s(const char *who, int src_type, prim_S_s f, Scheme_Object** g161);
#define SIG_s_v 21
typedef void (*prim_s_v)(Scheme_Object*);
void scheme_rtcall_s_v(const char *who, int src_type, prim_s_v f, Scheme_Object* g162);
#define SIG_iSi_s 22
typedef Scheme_Object* (*prim_iSi_s)(int, Scheme_Object**, int);
Scheme_Object* scheme_rtcall_iSi_s(const char *who, int src_type, prim_iSi_s f, int g163, Scheme_Object** g164, int g165);
#define SIG_siS_v 23
typedef void (*prim_siS_v)(Scheme_Object*, int, Scheme_Object**);
void scheme_rtcall_siS_v(const char *who, int src_type, prim_siS_v f, Scheme_Object* g166, int g167, Scheme_Object** g168);
#define SIG_z_p 24
typedef void* (*prim_z_p)(size_t);
void* scheme_rtcall_z_p(const char *who, int src_type, prim_z_p f, size_t g169);
#define SIG_si_s 25
typedef Scheme_Object* (*prim_si_s)(Scheme_Object*, int);
Scheme_Object* scheme_rtcall_si_s(const char *who, int src_type, prim_si_s f, Scheme_Object* g170, int g171);
#define SIG_sis_v 26
typedef void (*prim_sis_v)(Scheme_Object*, int, Scheme_Object*);
void scheme_rtcall_sis_v(const char *who, int src_type, prim_sis_v f, Scheme_Object* g172, int g173, Scheme_Object* g174);
