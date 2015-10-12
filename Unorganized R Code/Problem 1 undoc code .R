

all.subsets <- function(set) {
  n <- length(set)
  bin <-  expand.grid(rlply(n, c(F, T)))
 mlply(bin, function(...) { set[c(...)] })
}

get.reg <- function(vars) {
  if (length(vars) == 0) {
    vars = "1"
  }
  vars.form <- paste("lpsa ~", paste(vars, collapse = " + "))
  lm(vars.form, data = data.set, subset = train)
}

data.set <- read.table("~/prostate.data", sep = "\t", header = TRUE)
varlist <- all.subsets(names(data.set)[2:9])

models <- llply(varlist, get.reg)
models.RSS <- ldply(models,function(x){
  c(RSS = sum(x$residuals^2), k = length(x$coeff))
})

models.min <- ddply(models.RSS, .(k), function(x){
  x[which.min(x$RSS),]})


qplot(k, RSS, data = models.RSS, ylim= c(0,100),
      xlab = "Subset Size k", ylab = "ResidualSum-of-Squares",) +
  geom_point(data = models.min, aes(x = k, y = RSS), colour = "red") +
  geom_line(data = models.min, aes(x = k, y = RSS), colour = "red") +
  theme_bw()

