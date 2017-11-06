# ===========================================================================
# 
#
# 陈华珊
# 2017-11-06 15:59:33
#
# logging package for `foreach` and parallel computing
#
# todo
# 实现 OutputDebugString()，移植 debug view 
# ===========================================================================

#' @examples 
#' library(R6)
#' log <- logr$new()
#' log$loginfo('test')
#' log$loginfo(999)
#' log$loginfo(1:9)
#' log$loginfo(TRUE)
#' log$loginfo(factor('a'))
#' 
#' log$loginfoT('test')
#' log$loginterval(TRUE)
#' Sys.sleep(3)
#' log$loginterval('done!')
#' rm(log); gc(TRUE)
#' 
#' demo for multithread computing
#' source("e:/Huashan/Stat/R/packages/textnet/R/mc_utils.R", chdir = T, encoding = 'UTF-8')
#' do_thread <- function(x, loger) {
#'   loger$loginfoT(x)
#' }
#' loger <- logr$new()
#' mcStart()
#' foreach(j = 1:6, .packages = 'logr', .export = c('do_thread')) %dopar% do_thread(j, loger)
#' mcEnd()
#' rm(loger);gc(T)
logr <- R6Class("logr",
                    public = list(
                      initialize = function(host = 'localhost', port = 5555) {
                        if (Sys.info()['sysname'] == 'Windows') {
                          nc <- system.file('exe/nc.exe', package = 'logr')
                          if (nc == '') stop('nc.exe not exists')
                          cmd <- sprintf('%s -l -s %s -p %d', shQuote(nc), host, port)
                        } else {
                          cmd <- sprintf('nc -lk %d', port)
                        }
                        system(cmd, wait = FALSE,
                               show.output.on.console = FALSE, invisible = FALSE)
                        private$log.socket <- make.socket(host, port)
                      },
                      finalize = function() {
                        if (!is.null(private$log.socket)) close.socket(private$log.socket)
                      },
                      loginfo = function(msg) {
                        msg <- as.character(msg)
                        write.socket(private$log.socket, msg)
                        write.socket(private$log.socket, '\n')
                      },
                      #' log info with time stamp
                      loginfoT = function(msg) {
                        msg <- as.character(msg)
                        msg <- sprintf('%s: %s\n', as.character(Sys.time()), msg)
                        write.socket(private$log.socket, msg)
                      },
                      #' log info with time interval
                      #' @param start set TRUE to begin interval counting
                      loginterval = function(msg) {
                        #if (isTRUE(start)) {
                        if (is.logical(msg)) {
                          private$saved.tick <- as.character(Sys.time())
                        } else {
                          msg <- as.character(msg)
                          msg <- sprintf('%s - %s: %s\n', private$saved.tick,
                                         as.character(Sys.time()), msg)
                          write.socket(private$log.socket, msg)
                          private$saved.tick = ''
                        }
                      },
                      quit = function() {
                        # todo
                        if (!is.null(private$log.socket)) close.socket(private$log.socket)
                        private$log.socket <- NULL
                      }
                    ),
                    private = list(log.socket = NULL,
                                   saved.tick = ''),
)
