png("고래잡이 문제.png", width = 1000, height = 1000, res = 150)

par(mar = c(5, 6, 2, 2),
    mgp = c(3 ,1,0))

plot(
  NA,
  xlim = c(0, 6),
  ylim = c(0, 110),
  xlab = "배의 수 (척)",
  ylab = "한계비용, 한계수입, 평균수입 (만원)",
  cex.lab = 1.5,
  axes = FALSE,
  type = "n",
  xaxs = "i",
  yaxs = "i"
)

# 격자: 축 눈금과 정확히 맞춤
abline(v = seq(0, 6, by = 1), col = "grey90", lwd = 0.7)
abline(h = seq(0, 110, by = 10), col = "grey90", lwd = 0.7)

# 축 숫자
axis(1, at = seq(0, 6, by = 1), cex.axis = 0.9)
axis(2, at = seq(0, 110, by = 10), las = 1, cex.axis = 0.9)

# 테두리
box(col = "grey50")

dev.off()


### 목초지 문제 ###
png("목초지 문제.png", width = 1000, height = 1000, res = 150)

par(mar = c(5, 6, 2, 2),
    mgp = c(3 ,1,0))

plot(
  NA,
  xlim = c(0, 6),
  ylim = c(0, 1100),
  xlab = "소의 수 (마리)",
  ylab = "한계비용, 한계수입, 평균수입 (만원)",
  cex.lab = 1.5,
  axes = FALSE,
  type = "n",
  xaxs = "i",
  yaxs = "i"
)

# 격자: 축 눈금과 정확히 맞춤
abline(v = seq(0, 6, by = 1), col = "grey90", lwd = 0.7)
abline(h = seq(0, 1100, by = 100), col = "grey90", lwd = 0.7)

# 축 숫자
axis(1, at = seq(0, 6, by = 1), cex.axis = 0.9)
axis(2, at = seq(0, 1100, by = 100), las = 1, cex.axis = 0.9)

# 테두리
box(col = "grey50")

dev.off()