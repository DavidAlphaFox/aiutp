#include "aiutp_ctx"
using namespace aiutp;

ErlNifResourceType * AIUTPCtx::_sockResource(NULL);

AIUTPCtx::AIUTPCtx(ErlNifEnv* callerEnv){
    _env = enif_alloc_env();
    enif_make_pid(_env,enif_self(caller,&_pid));
    _mutex =  enif_mutex_create("AIUTPCtxMutex");
    _ctx = utp_init(2);
    utp_context_set_userdata(_ctx,this);
}

AIUTPCtx::~AIUTPCtx(){
    if(NULL != _env) enif_free_env(_env);
    if(NULL != _ctx) utp_destroy(_ctx);
    if(NULL != _mutex)  enif_mutex_destroy(_mutex);
}


void
AIUTPCtx::DoRegisterType(ErlNifEnv* cllerEnv)
{
    ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);

    _sockResource = enif_open_resource_type(callerEnv, NULL, "aiutp_sock",
                                            &AIUTPCtx::DoSockClenup,flags, NULL);

    return;

}


unit64 AIUTPCtx::OnAccept(utp_socket* sock){
    void*
}
