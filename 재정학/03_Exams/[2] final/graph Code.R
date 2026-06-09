png("Lorenz_blank.png", width = 1000, height = 1000, res = 150)

par(
  mar = c(5, 6, 2, 2),
  mgp = c(3, 1, 0)
)

plot(
  NA,
  xlim = c(0, 100),
  ylim = c(0, 100),
  xlab = "누적인구비율 (%)",
  ylab = "누적소득비율 (%)",
  cex.lab = 1.5,
  axes = FALSE,
  type = "n",
  xaxs = "i",
  yaxs = "i"
)

# 격자선
abline(v = seq(0, 100, by = 5), col = "grey90", lwd = 0.7)
abline(h = seq(0, 100, by = 5), col = "grey90", lwd = 0.7)

# 축
axis(1,
     at = seq(0, 100, by = 5),
     cex.axis = 0.8)

axis(2,
     at = seq(0, 100, by = 5),
     las = 1,
     cex.axis = 0.8)

# 테두리
box(col = "grey50")

dev.off()