library(curl)

benchmark <- function(concurrency, make_handle_fun){
  pool <- new_pool()
  out <- vector("list", concurrency)
  lapply(seq_len(concurrency), function(i){
    multi_add(make_handle_fun(), pool = pool, done = function(res){
      out[[i]] <<- res
      if(res$status > 201)
        message(sprintf("HTTP %d: %s", res$status, rawToChar(res$content)))
    }, fail = stop)
  })
  multi_run(pool = pool)
  sapply(out, function(x){x$times['total']})
}

benchmark(6, function(){
  curl::new_handle(url = 'http://dev.opencpu.org/ocpu/library/datasets/data/mtcars/json')
})

benchmark(12, function(){
  handle_setform(new_handle(url = 'http://dev.opencpu.org/ocpu/library/stats/R/rnorm'), n = "100")
})

