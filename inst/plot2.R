library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library(dplyr)
d <- data.frame(x = runif(100, -1, 1))
d$y <- rnorm(nrow(d), -0.3 + 0.4 * d$x, 0.4)
nd <- data.frame(x = seq(min(d$x), max(d$x), length.out = 200))

m <- stan("lm.stan", data = list(x = d$x, y = d$y, N = nrow(d),
  N_new = nrow(nd), x_new = nd$x))

pp <- extract(m, pars = "y_hat")[[1]]
pp2 <- reshape2::melt(pp)
names(pp2) <- c("iter", "x", "y")
pp2_ <- dplyr::filter(pp2, iter %in% sample(seq_len(max(pp2$iter)), 200))
library(ggplot2)
ggplot(pp2_, aes(x, y, group = iter)) + geom_line(alpha = 0.1)

min_y <- quantile(pp2$y, probs = 0.001)
max_y <- quantile(pp2$y, probs = 0.999)
dp <- plyr::ddply(pp2, "x", function(x) {
  temp <- data.frame(density(x$y, n = 1500, from = min_y, to = max_y)[c("x", "y")])
  data.frame(x = rep(unique(x$x), nrow(temp)), y = temp$x, density = temp$y)
})

ggplot(dp, aes(x, y, alpha = density^1)) + geom_raster() +
  theme_light() +
  scale_alpha_continuous(range=c(0.0001, 1))
ggsave("test.pdf", width = 5, height = 5)

b1 <- readRDS("~/Downloads/B1.rds")

pp2 <- reshape2::melt(b1)
names(pp2) <- c("iter", "x", "region", "y")

min_y <- quantile(pp2$y, probs = 0.0001)
max_y <- quantile(pp2$y, probs = 0.9999)
dp <- plyr::ddply(pp2, c("x", "region"), function(x) {
  temp <- data.frame(density(x$y, n = 800, from = min_y, to = max_y)[c("x", "y")])
  data.frame(x = rep(unique(x$x), nrow(temp)),
    region = rep(unique(x$region), nrow(temp)), y = temp$x,
    density = temp$y, median = median(x$y), stringsAsFactors = FALSE)
})


pal <- RColorBrewer::brewer.pal(5, "Blues")
ggplot(dp, aes(x, y, alpha = density^0.8)) +
  geom_raster(fill = pal[[5]]) +
  scale_alpha_continuous(range=c(0.0001, 1)) +
  # viridis::scale_fill_viridis() +
  facet_wrap(~region) +
  geom_line(aes(x = x, y = median), col = pal[[5]], alpha = 1) +
  ggsidekick::theme_sleek() +
  guides(alpha = FALSE, fill = FALSE)

ggsave("test1.pdf", width = 6, height = 4)

dp2 <- group_by(pp2, x, region) %>%
  summarise(
    l3 = quantile(y, probs = 0.05),
    l2 = quantile(y, probs = 0.10),
    l1 = quantile(y, probs = 0.25),
    m  = quantile(y, probs = 0.50),
    u1 = quantile(y, probs = 0.75),
    u2 = quantile(y, probs = 0.90),
    u3 = quantile(y, probs = 0.95)
)

pal <- RColorBrewer::brewer.pal(5, "Blues")
ggplot(dp2, aes(x, m)) +
  facet_wrap(~region) +
  ggsidekick::theme_sleek() +
  geom_ribbon(aes(ymin = l3, ymax = u3), col = pal[[1]], fill = pal[1]) +
  geom_ribbon(aes(ymin = l2, ymax = u2), col = pal[[2]], fill = pal[2]) +
  geom_ribbon(aes(ymin = l1, ymax = u1), col = pal[[3]], fill = pal[3]) +
  geom_line(col = pal[[5]], lwd = 0.8) +
  guides(alpha = FALSE, fill = FALSE)
ggsave("test2.pdf", width = 6, height = 4)


dp$iter <- 1e6
filter(pp2, iter %in% 1:50) %>%
  ggplot(aes(x, y, group = iter)) +
  facet_wrap(~region) +
  ggsidekick::theme_sleek() +
  geom_line(col = pal[[4]], alpha = 0.08) +
  guides(alpha = FALSE, fill = FALSE) +
  geom_line(data = dp, aes(x = x, y = median), col = pal[[5]], alpha = 1)
ggsave("test3.pdf", width = 6, height = 4)



xout <- seq(1, max(pp2$x), 0.2)
pp2_approx <- filter(pp2, iter %in% sample(seq_len(max(pp2$iter)), 700)) %>%
  group_by(iter, region) %>%
  do(data.frame(approx(x = .$x, y = .$y, xout = xout)))

min_y <- quantile(pp2_approx$y, probs = 0)
max_y <- quantile(pp2_approx$y, probs = 1)
dp <- plyr::ddply(pp2_approx, c("x", "region"), function(x) {
  temp <- data.frame(density(x$y, n = 600, from = min_y, to = max_y)[c("x", "y")])
  data.frame(x = rep(unique(x$x), nrow(temp)),
    region = rep(unique(x$region), nrow(temp)), y = temp$x,
    density = temp$y, median = mean(x$y), stringsAsFactors = FALSE)
})



pal <- RColorBrewer::brewer.pal(5, "Blues")
g <- ggplot(dp, aes(x, y, fill = density, alpha = density^0.65)) +
  geom_raster(fill = pal[[5]]) +
  scale_alpha_continuous(range=c(0.0001, 1)) +
  viridis::scale_fill_viridis(option = "A") +
  # scale_fill_distiller(palette = "Blu", direction = 1) +
  facet_wrap(~region) +
  geom_line(aes(x = x, y = median), col = pal[[5]], alpha = 1) +
  ggsidekick::theme_sleek() +
  guides(alpha = FALSE, fill = FALSE)

ggsave("test6.pdf", width = 6, height = 4)




