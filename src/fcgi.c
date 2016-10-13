#include <signal.h>
#include <unistd.h>

#include <R.h>
#include <Rinternals.h>

#include "fcgiapp.h"


static int rfcgi_socket = -1;
static FCGX_Request request;

SEXP rfcgi_stop();

SEXP rfcgi_start(SEXP a) {

    const char *path = CHAR(STRING_ELT(a, 0));

    FCGX_Init();
    rfcgi_stop();

    int sock = FCGX_OpenSocket(path, 5);
    if(sock < 0) {
        error("FCGX_OpenSocket failed.");
    }

    rfcgi_socket = sock;


    return R_NilValue;
}


void my_sighandler(int sig) {
    return;
}

SEXP rfcgi_accept() {
    FCGX_InitRequest(&request, rfcgi_socket, FCGI_FAIL_ACCEPT_ON_INTR);

    struct sigaction oldsa, newsa, oldsa2, newsa2;
    newsa.sa_handler = my_sighandler;
    sigemptyset(&newsa.sa_mask);
    newsa.sa_flags=0;
    newsa2.sa_handler = my_sighandler;
    sigemptyset(&newsa2.sa_mask);
    newsa2.sa_flags=0;

    sigaction(SIGINT, &newsa, &oldsa);
    sigaction(SIGPIPE, &newsa2, &oldsa2);

    int res = FCGX_Accept_r(&request);

    sigaction(SIGINT, &oldsa, NULL);
    sigaction(SIGPIPE, &oldsa2, NULL);

    SEXP ret = Rf_allocVector(INTSXP,1);
    if(res < 0)
        INTEGER(ret)[0] = 0;
    else 
        INTEGER(ret)[0] = 1;

    return ret;
}

SEXP rfcgi_finish() {

    FCGX_Finish_r(&request);

    return R_NilValue;
}


SEXP rfcgi_putstr(SEXP s) {

    //if(!IS_SCALAR(s, STRSXP)) error("s needs to be a scalar string");

    if(LENGTH(s) < 1) {
        return R_NilValue;
    }

    int rv=-1;

    if(TYPEOF(s) == RAWSXP) {

        rv = FCGX_PutStr(RAW(s), LENGTH(s), request.out);

    } else {
        const char *x = CHAR(STRING_ELT(s,0));
        const int len = strlen(x);

        //printf("%s (%d)\n", x, len);

        rv = FCGX_PutStr(x, len, request.out);
    }

    SEXP ret = PROTECT(Rf_allocVector(INTSXP,1));
    INTEGER(ret)[0] = rv;
    UNPROTECT(1);

    return ret;
}


SEXP rfcgi_getparam(SEXP _name) {

    const char *name = CHAR(STRING_ELT(_name,0));

    const char *val = FCGX_GetParam(name, request.envp);

    if(val == NULL) {
        return R_NilValue;
    }

    SEXP res = Rf_allocVector(STRSXP, 1);
    PROTECT(res);

    SET_STRING_ELT(res, 0, Rf_mkChar(val));


    UNPROTECT(1);
    return res;    
}


SEXP rfcgi_print_vars() {
    int i;
    for(i=0; request.envp[i] != NULL; i++) {
        puts(request.envp[i]);
    }

}

SEXP rfcgi_stop() {
    if(rfcgi_socket != -1) {
        close(rfcgi_socket);
    }
    rfcgi_socket = -1;
    return R_NilValue;
}


