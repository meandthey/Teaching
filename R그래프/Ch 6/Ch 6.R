
#### 122page #### 6-2 최적배출기준
## ===== 공통 설정 =====
oldpar <- par(no.readonly = TRUE)
par(mar = c(5,4.8,2,1), mgp = c(2.6,0.8,0),
    xaxs = "i", yaxs = "i", xaxt="n", yaxt="n")

## x, y 범위
x <- seq(0, 100, length.out = 800)
ylim_all <- c(0, 60)

## 두 함수 정의
mac_a <- 0.00045 * x^3 + 0.04 * x - 0.5        # (a) 저감량 기준
mac_b <- 600/(x + 3) + 3                  # (b) 배출량 기준

## 교차점 계산
diff_fun <- function(z) 0.00045*z^3 + 0.04*z - 0.5 - (600/(z+3) + 3)
e_star <- uniroot(diff_fun, c(0,100))$root
y_star <- 0.00045*e_star^3 + 0.04*e_star - 0.5

## 빈 캔버스
plot(NA, xlim = c(0,100), ylim = ylim_all,
     xlab = "배출량", ylab = "금액",
     main = "최적 배출량",
     cex.axis = 2.0, 
     cex.lab = 2.0, 
     cex.main = 2.0)

# 곡선
lines(x, mac_a, lwd = 3, col = "blue")
lines(x, mac_b, lwd = 3, col = "darkgreen")

# 교차점 및 안내선
abline(v = e_star, lty = 3, col = "gray60")
points(e_star, y_star, pch = 16, col = "red", cex = 1.2)


# e* 라벨: x축 '아래'에 배치
mtext(expression(e^"*"), side = 1, at = e_star, line = 1.2, cex = 1.2)
mtext(expression(e[0]), side = 1, at = 7.6, line = 1.2, cex = 1.2)
mtext(expression(e[1]), side = 1, at = 20, line = 1.2, cex = 1.2)

# 라벨
text(50, 45, "MD", col = "blue", cex = 1.5)
text(20, 25, "MAC", col = "darkgreen", cex = 1.5)

par(oldpar)





#### 125page #### 6-4 최적배출기준
## ===== 공통 설정 =====
oldpar <- par(no.readonly = TRUE)
par(mar = c(5,4.8,2,1), mgp = c(2.6,0.8,0),
    xaxs = "i", yaxs = "i", xaxt="n", yaxt="n")

## x, y 범위
x <- seq(0, 100, length.out = 800)
ylim_all <- c(0, 60)

## 두 함수 정의
mac_a <- 0.000045 * x^3 + 0.04 * x - 0.5        # (a) 저감량 기준
mac_b <- 400/(x + 3) + 3                  # (b) 배출량 기준

## 교차점 계산
diff_fun <- function(z) 0.000045*z^3 + 0.04*z - 0.5 - (400/(z+3) + 3)
e_star <- uniroot(diff_fun, c(0,100))$root
y_star <- 0.000045*e_star^3 + 0.04*e_star - 0.5

## 빈 캔버스
plot(NA, xlim = c(0,100), ylim = ylim_all,
     xlab = "저감량", ylab = "MAC",
     main = "",
     cex.axis = 2.0, 
     cex.lab = 2.0, 
     cex.main = 2.0)

# 곡선
lines(x, mac_a, lwd = 3, col = "blue")
lines(x, mac_b, lwd = 3, col = "darkgreen")

# 교차점 및 안내선
abline(v = e_star, lty = 3, col = "gray60")
abline(v = e_star-20, lty = 3, col = "gray60")
points(e_star, y_star, pch = 16, col = "red", cex = 1.2)


# e* 라벨: x축 '아래'에 배치
mtext(expression(a[1]^"*"), side = 1, at = e_star, line = 1.2, cex = 1.7)
mtext(expression(a[1]^"1"), side = 1, at = e_star-20, line = 1.2, cex = 1.7)
#mtext(expression(e[0]), side = 1, at = 7.6, line = 1.2, cex = 1.2)
#mtext(expression(e[1]), side = 1, at = 20, line = 1.2, cex = 1.2)

# 라벨
text(80, 25, expression(MAC[1]), col = "blue", cex = 1.5)
text(20, 25, expression(MAC[2]), col = "darkgreen", cex = 1.5)

par(oldpar)



### 130 page ###
## 101 page (b) ##

## ===== 공통 설정 =====
oldpar <- par(no.readonly = TRUE)
par(mar = c(5,4.8,2,1), mgp = c(2.6,0.8,0),
    xaxs = "i", yaxs = "i", xaxt="n", yaxt="n")

## x, y 범위
x <- seq(0, 100, length.out = 800)
ylim_all <- c(0, 60)

## 함수 정의 (function 형태로 정의해야 교차점 자동 계산 가능)
mac_a_fun  <- function(z) 0.00005*z^3 + 0.04*z     # MD1

mac_b_fun  <- function(z) 900/(z + 3) - 900/103         # MAC
mac_bb_fun  <- function(z) 400/(z + 3) - 400/103        # MAC

## 교차점 계산
diff1 <- function(z) mac_a_fun(z)  - mac_b_fun(z)  # MD1 - MAC
diff2 <- function(z) mac_a_fun(z) - mac_bb_fun(z)  # MD2 - MAC

e_star  <- uniroot(diff1, c(0,100))$root
y_star  <- mac_a_fun(e_star)

e_star2 <- uniroot(diff2, c(0,100))$root
y_star2 <- mac_bb_fun(e_star2)

## 빈 캔버스
plot(NA, xlim = c(0,100), ylim = ylim_all,
     xlab = "배출량", ylab = "금액",
     main = "저감기술 발전",
     cex.axis = 2.0, 
     cex.lab = 2.0, 
     cex.main = 2.0)

# 곡선
lines(x, mac_a_fun(x),  lwd = 3, col = "blue")
lines(x, mac_bb_fun(x), lwd = 3, col = "darkgreen")
lines(x, mac_b_fun(x),  lwd = 3, col = "darkgreen")

# 교차점 및 안내선
abline(v = e_star,  lty = 3, col = "gray60")
abline(v = e_star2, lty = 3, col = "gray60")
points(e_star,  y_star,  pch = 16, col = "red", cex = 1.2)
points(e_star2, y_star2, pch = 16, col = "red", cex = 1.2)

# 라벨: e1*, e2*를 x축 아래에
mtext(expression(e[1]), side = 1, at = e_star,  line = 1.2, cex = 1.2)
mtext(expression(e[2]), side = 1, at = e_star2, line = 1.2, cex = 1.2)

mtext(expression(e[0]), side = 1, at = c(100),  line = 1.2, cex = 1.2)
mtext('0', side = 1, at = c(0),  line = 1.2, cex = 1.2)

# 그래프 내 라벨
text(80, 25, expression(MD), col = "blue", cex = 1.5)

text(30, 25, expression(MAC[1]), col = "darkgreen", cex = 1.5)
text(5, 25, expression(MAC[2]), col = "darkgreen", cex = 1.5)

par(oldpar)




