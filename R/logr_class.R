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
#' log$quit()
#' rm(log); gc(TRUE)
#' 
#' 
#' demo for multithread computing
#' source("e:/Huashan/Stat/R/packages/textnet/R/mc_utils.R", chdir = T, encoding = 'UTF-8')
#' do_thread <- function(x, loger) {
#'   #loger$loginfoT(paste(x, Sys.getpid()))
#'   loger$loginterval(TRUE)
#'   Sys.sleep(sample(5))
#'   loger$loginterval(x)
#' }
#' loger <- logr$new()
#' mcStart()
#' foreach(j = 1:6, .packages = 'logr', .export = c('do_thread')) %dopar% do_thread(j, loger)
#' mcEnd()
#' loger$quit()
#' rm(loger);gc(T)
#' 
#' logr$new('192.168.1.3', port = 9999, logfile = 'log.txt')
logr <- R6Class("logr",
                    public = list(
                      withPID = TRUE,
                      withTime = TRUE,
                      initialize = function(host = 'localhost', port = 5555, logfile) {
                        if (Sys.info()['sysname'] == 'Windows') {
                          nc <- system.file('exe/nc.exe', package = 'logr')
                          if (nc == '') stop('nc.exe not exists')
                          cmd <- sprintf('%s -l -s %s -p %d', shQuote(nc), host, port)
                        } else {
                          cmd <- sprintf('nc -lk %d', port)
                        }
                        if (!missing(logfile)) {
                          dir.create(dirname(logfile), recursive = T, showWarnings = F)
                          
                          # Linux 下使用文件管道，Windows下使用参数 -o
                          fo <- if (Sys.info()['sysname'] == 'Windows') '-o' else '>>'
                          cmd <- sprintf('%s %s %s', cmd, fo, shQuote(logfile))
                        }
                        system(cmd, wait = FALSE,
                               show.output.on.console = FALSE, invisible = FALSE)
                        private$log.socket <- make.socket(host, port)
                      },
                      finalize = function() {
                        if (!is.null(private$log.socket)) close.socket(private$log.socket)
                      },
                      loginfo = function(msg) {
                        private$.msg(msg)
                      },
                      #' log info with time stamp
                      loginfoT = function(msg) {
                        private$.msg(msg, TRUE)
                      },
                      #' log info with time interval
                      #' @param msg string or logic. Set msg to TRUE to begin interval counting,
                      #'            FALSE or string to stop interval counting.
                      loginterval = function(msg) {
                        if (is.logical(msg) & isTRUE(msg)) {
                          private$saved.tick[[Sys.getpid()]] <- as.character(Sys.time())
                        } else {
                          private$.msg(msg, isInterval = TRUE)
                        }
                      },
                      quit = function() {
                        # todo
                        if (!is.null(private$log.socket)) close.socket(private$log.socket)
                        private$log.socket <- NULL
                      }
                    ),
                    private = list(
                      log.socket = NULL,
                      saved.tick = list(),
                      .msg = function(msg, withTime = self$withTime, isInterval = FALSE) {
                          if (length(msg) > 1) warning('length of msg > 1, only the first element is processed.')
                          fpid <- if (self$withPID) paste0(Sys.getpid(), '\t') else ''
                          ftime <- if (isInterval) 
                            sprintf('%s - %s\t', private$saved.tick[[Sys.getpid()]],
                                             as.character(Sys.time()))
                          else {
                            if (withTime) paste0(as.character(Sys.time()), '\t') else ''
                          }
                          msg <- sprintf('%s%s%s\n', fpid, ftime, as.character(msg))
                          write.socket(private$log.socket, msg)
                      }
                    )
)
