library(ggplot2)
library(doSNOW)
library(plyr)

createCluster = function(noCores, logfile = "/dev/null", export = NULL, lib = NULL) {
  require(doSNOW)
  cl <- makeCluster(noCores, type = "SOCK", outfile = logfile)
  if(!is.null(export)) clusterExport(cl, export)
  if(!is.null(lib)) {
    l_ply(lib, function(dum) { 
      clusterExport(cl, "dum", envir = environment())
      clusterEvalQ(cl, library(dum, character.only = TRUE))
    })
  }
  registerDoSNOW(cl)
  return(cl)
}

bla = function(arg) {
  dum = ggplot(aes(x = x, y = x), data = arg)
  summary(dum)
  xi = bla2(arg$x)
  return(arg$x*xi)
}

bla2 = function(arg) {
  return(arg + 1)
}

# Constants
y = 10
dat = data.frame(x = 1:10, category = LETTERS[1:10])

# Create a cluster

# Fails
# Error in do.ply(i) : task 1 failed - "could not find function "ggplot""
cl = createCluster(2)
res = ddply(dat, .(category), bla, .parallel = TRUE)
stopCluster(cl)

# Fails, pacakge is loaded, function 'bla2' is not
# Error in do.ply(i) : task 1 failed - "could not find function "bla2""
cl = createCluster(2, lib = list("ggplot2"))
res =ddply(dat, .(category), bla, .parallel = TRUE)
stopCluster(cl)

# Works! Also export the function 'bla2' and object 'y'
cl = createCluster(2, export = list("bla2","y"), lib = list("ggplot2"))
res = ddply(dat, .(category), bla, .parallel = TRUE)
stopCluster(cl)

# Sanity check
all.equal(res, ddply(dat, .(category), bla, .parallel = FALSE))
# TRUE!