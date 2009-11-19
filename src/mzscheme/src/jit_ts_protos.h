#define SIG_siS_s 5
typedef Scheme_Object* (*prim_siS_s)(Scheme_Object*, int, Scheme_Object**);
Scheme_Object* scheme_rtcall_siS_s(prim_siS_s f, Scheme_Object* g118, int g119, Scheme_Object** g120);
#define SIG_iSs_s 6
typedef Scheme_Object* (*prim_iSs_s)(int, Scheme_Object**, Scheme_Object*);
Scheme_Object* scheme_rtcall_iSs_s(prim_iSs_s f, int g121, Scheme_Object** g122, Scheme_Object* g123);
#define SIG_s_s 7
typedef Scheme_Object* (*prim_s_s)(Scheme_Object*);
Scheme_Object* scheme_rtcall_s_s(prim_s_s f, Scheme_Object* g124);
#define SIG_n_s 8
typedef Scheme_Object* (*prim_n_s)(Scheme_Native_Closure_Data*);
Scheme_Object* scheme_rtcall_n_s(prim_n_s f, Scheme_Native_Closure_Data* g125);
#define SIG__s 9
typedef Scheme_Object* (*prim__s)();
Scheme_Object* scheme_rtcall__s(prim__s f );
#define SIG_ss_s 10
typedef Scheme_Object* (*prim_ss_s)(Scheme_Object*, Scheme_Object*);
Scheme_Object* scheme_rtcall_ss_s(prim_ss_s f, Scheme_Object* g126, Scheme_Object* g127);
#define SIG_ss_m 11
typedef MZ_MARK_STACK_TYPE (*prim_ss_m)(Scheme_Object*, Scheme_Object*);
MZ_MARK_STACK_TYPE scheme_rtcall_ss_m(prim_ss_m f, Scheme_Object* g128, Scheme_Object* g129);
#define SIG_Sl_s 12
typedef Scheme_Object* (*prim_Sl_s)(Scheme_Object**, long);
Scheme_Object* scheme_rtcall_Sl_s(prim_Sl_s f, Scheme_Object** g130, long g131);
#define SIG_l_s 13
typedef Scheme_Object* (*prim_l_s)(long);
Scheme_Object* scheme_rtcall_l_s(prim_l_s f, long g132);
#define SIG_bsi_v 14
typedef void (*prim_bsi_v)(Scheme_Bucket*, Scheme_Object*, int);
void scheme_rtcall_bsi_v(prim_bsi_v f, Scheme_Bucket* g133, Scheme_Object* g134, int g135);
#define SIG_iiS_v 15
typedef void (*prim_iiS_v)(int, int, Scheme_Object**);
void scheme_rtcall_iiS_v(prim_iiS_v f, int g136, int g137, Scheme_Object** g138);
#define SIG_ss_v 16
typedef void (*prim_ss_v)(Scheme_Object*, Scheme_Object*);
void scheme_rtcall_ss_v(prim_ss_v f, Scheme_Object* g139, Scheme_Object* g140);
#define SIG_b_v 17
typedef void (*prim_b_v)(Scheme_Bucket*);
void scheme_rtcall_b_v(prim_b_v f, Scheme_Bucket* g141);
#define SIG_sl_s 18
typedef Scheme_Object* (*prim_sl_s)(Scheme_Object*, long);
Scheme_Object* scheme_rtcall_sl_s(prim_sl_s f, Scheme_Object* g142, long g143);
#define SIG_iS_s 19
typedef Scheme_Object* (*prim_iS_s)(int, Scheme_Object**);
Scheme_Object* scheme_rtcall_iS_s(prim_iS_s f, int g144, Scheme_Object** g145);
#define SIG_S_s 20
typedef Scheme_Object* (*prim_S_s)(Scheme_Object**);
Scheme_Object* scheme_rtcall_S_s(prim_S_s f, Scheme_Object** g146);
#define SIG_s_v 21
typedef void (*prim_s_v)(Scheme_Object*);
void scheme_rtcall_s_v(prim_s_v f, Scheme_Object* g147);
#define SIG_iSi_s 22
typedef Scheme_Object* (*prim_iSi_s)(int, Scheme_Object**, int);
Scheme_Object* scheme_rtcall_iSi_s(prim_iSi_s f, int g148, Scheme_Object** g149, int g150);
#define SIG_siS_v 23
typedef void (*prim_siS_v)(Scheme_Object*, int, Scheme_Object**);
void scheme_rtcall_siS_v(prim_siS_v f, Scheme_Object* g151, int g152, Scheme_Object** g153);
#define SIG_z_p 24
typedef void* (*prim_z_p)(size_t);
void* scheme_rtcall_z_p(prim_z_p f, size_t g154);
