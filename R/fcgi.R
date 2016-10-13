

cleanup_children <- function(children) {
    cat("cleanup\n")
    for(chld in children) { 
        try(parallel:::mckill(chld,9), silent=TRUE) 
    }
    parallel:::selectChildren() # for cleanup_zombies()
}

do_fork <- function(fn) {
    ret <- parallel:::mcfork()
    if(is(ret, "masterProcess")) { 
        fn()
    } else if(is(ret, "childProcess")) {
        ret
    } else {
        stop("fork")
    }
}



fastcgi_run <- function(path="127.0.0.1:5000", app, nworker=1L) {

    .Call("rfcgi_start", path)

    run1 <- function(i) {
        parallel <- nworker > 1L
        if(parallel) .Call(parallel:::C_mc_interactive, FALSE)

        while(.Call("rfcgi_accept")) {
            
            try({

            req <- new.env(parent=emptyenv())
            assign("REQUEST_METHOD", .Call("rfcgi_getparam", "REQUEST_METHOD"), envir=req)
            assign("QUERY_STRING", .Call("rfcgi_getparam", "QUERY_STRING"), envir=req)
            assign("DOCUMENT_URI", .Call("rfcgi_getparam", "DOCUMENT_URI"), envir=req)
            assign("SCRIPT_NAME", .Call("rfcgi_getparam", "SCRIPT_NAME"), envir=req)

            ifmodified <- .Call("rfcgi_getparam", "HTTP_IF_MODIFIED_SINCE")
            if(!is.null(ifmodified)) {
                assign("HTTP_IF_MODIFIED_SINCE", ifmodified, envir=req)
            }

            #.Call("rfcgi_print_vars")

            #try(cat(get("DOCUMENT_URI", envir=req), "\n"))
            try({
                txt <- sprintf("[%s] %s\n", 
                    as.character(Sys.time()), 
                    get("DOCUMENT_URI", envir=req))
                cat(txt)
            })

            #cat(paste0(Sys.getpid(), "\n"))
            #Sys.sleep(10)


            res <- try(app$call(req))

            if(is(res, "try-error")) {
                res <- res_error
            } 
            
            # TODO: test if length(res$headers) == 0
            hdrs <- paste0(mapply(function(n,v) {
                paste0(n, ": ", v, "\r\n")
            }, names(res$headers), res$headers, SIMPLIFY=FALSE), collapse="")
            
            rv <- .Call("rfcgi_putstr", paste0(hdrs, "\r\n"))
            if(rv > 0) rv <- .Call("rfcgi_putstr", res$body) else cat("rv <= 0\n")

            #.Call("rfcgi_putstr", "Content-type: text/plain\r\n\r\nhallo\n");
            })

            .Call("rfcgi_finish")
        }

        if(parallel) parallel:::mcexit(0L)
    }

    trycl <- function(expr) {
        tryCatch(expr, 
            finally={ if(nworker>1) cleanup_children(children) }, 
            interrupt=function(...) { if(nworker>1) cleanup_children(children); stop("interrupt")})
    }

    if(nworker==1) {
        trycl(run1(0))
    } else {
        #mclapply(seq.int(nworker), run1)

        cores <- as.integer(nworker)
        children <- vector(cores, mode="list")

        trycl({
            for(i in seq(cores)) {
                children[[i]] <- do_fork(run1)
            }

            repeat {
                Sys.sleep(10L)

                for(i in seq(cores)) {
                    ret <- try(parallel:::mckill(children[[i]], 0))
                    if(is(ret, "try-error")) {
                        cat("respawning\n")
                        children[[i]] <- do_fork(run1)
                    }
                }
            }
        })

        cleanup_children(children)
    }

    .Call("rfcgi_stop") 

}


