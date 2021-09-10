#ifndef __AI_UTP_CTX__
#define __AI_UTP_CTX__
#include<set>
#include<erl_nif.h>
#include"utp.h"

namespace aiutp {
    struct AIUTPSock {
        bool attached = false;
        utp_socket* sock;
        ErlNifPid _pid;
    };

    class AIUTPCtx {
        private:
            ErlNifEnv* _env;
            ErlNifPid _pid; //主进程的pid
            ErlNifMutex* _mutex;
            utp_context* _ctx;
            std::set<struct AIUTPSock*> _socks;
        protected:
            static ErlNifResourceType* _sockResource;
        public:
            static void DoRegisterType(ErlNifEnv* callerEnv);
            static void DoSockClenup(ErlNifEnv* callerEnv, void* sock);
            AIUTPCtx(ErlNifEnv* callerEnv);
            uint64 OnAccept(utp_socket* sock);
            ~AIUTPCtx();
    };
}
#endif
