# logr: R logging package for parallel computing

# Installation

```r
library(devtools)
install_github('huashan/logr')
```

# demo code

```r
do_thread <- function(x, loger) {
   loger$loginfoT(x)
}

loger <- logr$new()

# start clustering
# mcStart()

foreach(j = 1:6, .packages = 'logr') %dopar% do_thread(j, loger)

# stop clustering
# mcEnd()

rm(loger);gc(T)
```
