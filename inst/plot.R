library(rstanarm)
options(mc.cores = parallel::detectCores())
library(mgcv)
set.seed(2) ## simulate some data...
dat <- gamSim(1,n=150,dist="normal",scale=2)
library(dplyr)
head(dat)
m <- stan_gamm4(y ~ s(x2), data = dat, iter = 200)
nd <- data.frame(x2 = seq(min(dat$x2), max(dat$x2), length.out = 100))
nd <- bind_rows(nd, nd, nd, nd, nd, nd) %>%
  arrange(x2)
pp <- posterior_predict(m, newdata = nd)
pp2 <- reshape2::melt(pp)
names(pp2) <- c("iter", "x", "y")
# pp2 <- dplyr::filter(pp2, iter %in% sample(seq_len(max(pp2$iter)), 50))
library(ggplot2)
# ggplot(pp2, aes(x, y, group = iter)) + geom_line(alpha = 0.1)

head(pp2)


dp <- plyr::ddply(pp2, "x", function(x) {
  temp <- data.frame(density(x$y, n = 200, from = -10, to = 30)[c("x", "y")])
  data.frame(x = rep(unique(x$x), nrow(temp)), y = temp$x, density = temp$y)
})

ggplot(dp, aes(x, y, alpha = density)) + geom_raster() +
  theme_light() +
  scale_alpha_continuous(range=c(0.0001, 1))
