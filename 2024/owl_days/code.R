
talent_mean <- .250
talent_sd <- .02
n <- 200
luck_sd <- sqrt(talent_mean * (1 - talent_mean) / n)
performance <- .330
posterior_mean <- (performance / luck_sd^2 + talent_mean / talent_sd^2) / (1 / luck_sd^2 + 1 / talent_sd^2)
posterior_sd <- sqrt(1 / (1 / luck_sd^2 + 1 / talent_sd^2))

talent <- rnorm(n = 200, mean = talent_mean, sd = talent_sd)
sprintf("%.3f", talent)

grid <- seq(from = 0, to = 1, by = 0.0001)
xlim <- c(.17, .4)
ylim <- c(0, 40)

pdf("images/illustration_1.pdf", height = 5, width = 10)
par(mar = c(0, 0, 0, 0))
plot(0, 0, xlim = xlim, ylim = ylim, type = "n", axes = FALSE)
polygon(x = grid, y = dnorm(x = grid, mean = performance, sd = luck_sd), col = rgb(255, 140, 0, maxColorValue = 255, alpha = 127), border = FALSE)
dev.off()

pdf("images/illustration_2.pdf", height = 5, width = 10)
par(mar = c(0, 0, 0, 0))
plot(0, 0, xlim = xlim, ylim = ylim, type = "n", axes = FALSE)
polygon(x = grid, y = dnorm(x = grid, mean = performance, sd = luck_sd), col = rgb(255, 140, 0, maxColorValue = 255, alpha = 127), border = FALSE)
polygon(x = grid, y = dnorm(x = grid, mean = talent_mean, sd = .01), col = rgb(30, 144, 255, maxColorValue = 255, alpha = 127), border = FALSE)
dev.off()

pdf("images/illustration_3.pdf", height = 5, width = 10)
par(mar = c(0, 0, 0, 0))
plot(0, 0, xlim = xlim, ylim = ylim, type = "n", axes = FALSE)
polygon(x = grid, y = dnorm(x = grid, mean = performance, sd = luck_sd), col = rgb(255, 140, 0, maxColorValue = 255, alpha = 127), border = FALSE)
polygon(x = grid, y = dnorm(x = grid, mean = talent_mean, sd = .03), col = rgb(30, 144, 255, maxColorValue = 255, alpha = 127), border = FALSE)
dev.off()

pdf("images/illustration_4.pdf", height = 5, width = 10)
par(mar = c(0, 0, 0, 0))
plot(0, 0, xlim = xlim, ylim = ylim, type = "n", axes = FALSE)
polygon(x = grid, y = dnorm(x = grid, mean = performance, sd = luck_sd), col = rgb(255, 140, 0, maxColorValue = 255, alpha = 127), border = FALSE)
polygon(x = grid, y = dnorm(x = grid, mean = talent_mean, sd = talent_sd), col = rgb(30, 144, 255, maxColorValue = 255, alpha = 127), border = FALSE)
dev.off()

pdf("images/illustration_5.pdf", height = 5, width = 10)
par(mar = c(0, 0, 0, 0))
plot(0, 0, xlim = xlim, ylim = ylim, type = "n", axes = FALSE)
polygon(x = grid, y = dnorm(x = grid, mean = performance, sd = luck_sd), col = rgb(255, 140, 0, maxColorValue = 255, alpha = 127), border = FALSE)
polygon(x = grid, y = dnorm(x = grid, mean = talent_mean, sd = talent_sd), col = rgb(30, 144, 255, maxColorValue = 255, alpha = 127), border = FALSE)
polygon(x = grid, y = dnorm(x = grid, mean = posterior_mean, sd = posterior_sd), col = rgb(34, 139, 34, maxColorValue = 255, alpha = 127), border = FALSE)
dev.off()
