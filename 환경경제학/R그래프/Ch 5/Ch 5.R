## ===== 공통 설정 =====
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(1,2), mar = c(4,4,2,1), mgp = c(2.2,0.7,0))

x <- seq(0, 10, length.out = 400)

## -------------------- (a) 오랫동안 x축에 붙어있다가 급격히 증가 -------------------- ##
y_a <- exp(0.1 * (x^2)) - 1   # 한동안 거의 0, 끝에서 폭발적으로 증가
plot(x, y_a, type = "l", lwd = 2, col = "blue",
     xlab = "배출량", ylab = "오염피해", main = "(a)",
     cex.lab = 2.0, cex.main = 2.0, cex.axis = 2.0, 
     ylim=c(0, max(y_a)))
text(8, y_a[320], "MD", pos=3, cex=1.3)

## -------------------- (b) y절편 크게, 이후 곡선 증가 -------------------- ##
y_b <- 5 + exp(0.2 * x) - 1   # y절편 = 5
plot(x, y_b, type = "l", lwd = 2, col = "blue",
     xlab = "배출량", ylab = "오염피해", main = "(b)",
     cex.lab = 2.0, cex.main = 2.0, cex.axis = 2.0, 
     ylim=c(0, max(y_b)))
text(8, y_b[320], "MD", pos=3, cex=1.3)

## 원래 그래픽 설정 복원
par(oldpar)





# 데이터 생성 (아래로 오목 곡선)
q <- seq(0, 5, by = 0.01)
p <- 1000 * (1 - sqrt(q/5))

mid <- length(q) %/% 2  # 중간 인덱스
plot(q, p,
     type = "l", lwd = 2, col = "blue",
     xlab = "저감량",
     ylab = "한계편익",
     main = "",
     xaxs = "i",
     yaxs = "i",
     xaxt = "n",
     yaxt = "n",
     cex.axis = 1.2,
     cex.lab = 1.5,
     cex.main = 2,
     mgp = c(2, 0.7, 0))

# 중간 지점에 "MB" 추가
text(q[mid], p[mid], "MB", cex = 1.5, pos = 3)  



#### 98page #### 위쪽 그래프
## ===== 공통 설정 =====
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(1,2), mar = c(5,4.8,2,1), mgp = c(2.6,0.8,0),
    xaxs = "i", yaxs = "i",
    xaxt = "n",
    yaxt = "n")

## x, y 범위
x1 <- seq(0, 100, length.out = 800)   # (a) 저감량
x2 <- seq(0, 100, length.out = 800)   # (b) 배출량/오염도
ylim_a <- c(0, 60)
ylim_b <- c(0, 60)

## (a) 저감량 기준: 매우 완만하게 시작해서 급격히 증가(볼록)
#  - 시작부 완만함을 주기 위해 3차+선형 항을 조합
mac_a <- 0.00045 * x1^3 + 0.04 * x1

plot(NA, xlim = c(0, 100), ylim = ylim_a,
     xlab = "저감량", ylab = "한계저감비용", main = "(a) 저감량 기준",
     cex.axis = 2.0, 
     cex.lab = 2.0, 
     cex.main = 2.0,
     xaxt = "n",
     yaxt = "n")
axis(1, at = pretty(c(0,100)))
axis(2, at = pretty(ylim_a))
lines(x1, mac_a, lwd = 3)

# 원점 0 표시
text(0, 0, "0", pos = 1, xpd = NA)
# 'MAC' 라벨
text(55, 22, "Marginal Abatement Cost", cex = 1.5)

## (b) 배출량 또는 오염도 기준: 우하향(감소), 상한에서 시작해 점점 0에 근접
#  - 1/x 형태를 변형해 원점에서 유한한 값, 끝에서 0 근접
mac_b <- 600/(x2 + 3) + 3   # x=0에서 ~20, x=100에서 ~3

plot(NA, xlim = c(0, 100), ylim = ylim_b,
     xlab = "배출량 혹은 오염도", ylab = "한계저감비용", main = "(b) 배출량 또는 오염도 기준",
     cex.axis = 2.0, 
     cex.lab = 2.0, 
     cex.main = 2.0,)
axis(1, at = pretty(c(0,100)))
axis(2, at = pretty(ylim_b))
lines(x2, mac_b, lwd = 3)

# 원점 0 표시
text(0, 0, "0", pos = 1, xpd = NA)
# 'MAC' 라벨
text(35, 28, "Marginal Abatement Cost", cex = 1.5)

## 원래 설정 복구
par(oldpar)



#### 98page #### 아래쪽 그래프
## ===== 공통 설정 =====
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(1,3), mar = c(5,5,5,1), mgp = c(2.6,0.8,0),
    xaxs = "i", yaxs = "i")

## ----- 개별 MAC (원하는 모양으로 조정 가능) -----
mac_A <- function(q) 6 + 0.05*q + 0.005*q^2
mac_B <- function(q) 15 + 0.02*q + 0.010*q^2

## ----- 안전한 역함수: 범위 밖이면 0 또는 qmax 반환 -----
safe_inv <- function(f, c, lower = 0, upper = 100) {
  fL <- f(lower); fU <- f(upper)
  if (c <= fL) return(lower)
  if (c >= fU) return(upper)
  uniroot(function(q) f(q) - c, lower = lower, upper = upper)$root
}

qmax <- 100
qgrid <- seq(0, qmax, length.out = 800)
ylim  <- c(0, 70)

## 점선용 공통 한계비용
c_star <- 30
a_A <- safe_inv(mac_A, c_star, 0, qmax)
a_B <- safe_inv(mac_B, c_star, 0, qmax)

## ========== (1) 오염자 A ==========
plot(NA, xlim = c(0, qmax), ylim = ylim,
     xlab = "저감량", ylab = "한계저감비용",
     cex.axis = 2.0, 
     cex.lab = 2.0, 
     cex.main = 2.0)
lines(qgrid, mac_A(qgrid), lwd = 3)
abline(h = c_star, v = a_A, lty = 2)
text(a_A, par("usr")[3]-1.5, expression(a[A]), pos = 1, xpd = NA, cex = 1.6)
text(qmax*0.60, mac_A(qmax*0.60)+2.5, expression(MAC[A]), cex = 1.3)

## ========== (2) 오염자 B ==========
plot(NA, xlim = c(0, qmax), ylim = ylim,
     xlab = "저감량", ylab = "한계저감비용",
     cex.axis = 2.0, 
     cex.lab = 2.0, 
     cex.main = 2.0)
lines(qgrid, mac_B(qgrid), lwd = 3)
abline(h = c_star, v = a_B, lty = 2)
text(a_B, par("usr")[3]-1.5, expression(a[B]), pos = 1, xpd = NA, cex = 1.6)
text(qmax*0.60, mac_B(qmax*0.60)+2.5, expression(MAC[B]), cex = 1.3)

## ========== (3) 전체 MAC (수평합) ==========
# c를 파라미터로: Q(c) = a_A(c)+a_B(c), (Q(c), c)를 플로팅
c_grid <- seq(0, 65, length.out = 600)
Qa <- vapply(c_grid, function(cc) safe_inv(mac_A, cc, 0, qmax), numeric(1))
Qb <- vapply(c_grid, function(cc) safe_inv(mac_B, cc, 0, qmax), numeric(1))
Qtot <- Qa + Qb

plot(NA, xlim = c(0, max(Qtot)), ylim = ylim,
     xlab = "저감량", ylab = "한계저감비용",
     cex.axis = 2.0, 
     cex.lab = 2.0, 
     cex.main = 2.0)
lines(Qtot, c_grid, lwd = 3)
abline(h = c_star, v = a_A + a_B, lty = 2)
text(a_A + a_B, par("usr")[3]-1.5, expression(a[A] + a[B]), pos = 1, xpd = NA, cex = 1.6)
text(max(Qtot)*0.70,
     approx(Qtot, c_grid, xout = max(Qtot)*0.70)$y + 2.5,
     "MAC", cex = 1.3)

par(oldpar)


#### 100page #### 5-6 (a) 최적배출량
## ===== 공통 설정 =====
oldpar <- par(no.readonly = TRUE)
par(mar = c(5,4.8,2,1), mgp = c(2.6,0.8,0),
    xaxs = "i", yaxs = "i", xaxt="n", yaxt="n")

## x, y 범위
x <- seq(0, 100, length.out = 800)
ylim_all <- c(0, 60)

## 두 함수 정의
mac_a <- 0.00045 * x^3 + 0.04 * x         # (a) 저감량 기준
mac_b <- 600/(x + 3) + 3                  # (b) 배출량 기준

## 교차점 계산
diff_fun <- function(z) 0.00045*z^3 + 0.04*z - (600/(z+3) + 3)
e_star <- uniroot(diff_fun, c(0,100))$root
y_star <- 0.00045*e_star^3 + 0.04*e_star

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

# 라벨
text(50, 45, "MD", col = "blue", cex = 1.5)
text(20, 25, "MAC", col = "darkgreen", cex = 1.5)

par(oldpar)

#### 100page #### 5-6 (b) 최적 저감량
## ===== 공통 설정 =====
oldpar <- par(no.readonly = TRUE)
par(mar = c(5,4.8,2,1), mgp = c(2.6,0.8,0),
    xaxs = "i", yaxs = "i", xaxt="n", yaxt="n")

## x, y 범위
x <- seq(0, 100, length.out = 800)
ylim_all <- c(0, 60)

## 함수 정의
mac_a <- function(z) 0.005 * z^3 + 0.04 * z
mac_b <- function(z) 200/(z + 3) + 3

## 교차점 계산
diff_fun <- function(z) mac_a(z) - mac_b(z)
e_star <- uniroot(diff_fun, c(0,100))$root
y_star <- mac_a(e_star)

## 빈 캔버스
plot(NA, xlim = c(0,100), ylim = ylim_all,
     xlab = "저감량", ylab = "금액",
     main = "최적 저감량",
     cex.axis = 2.0, 
     cex.lab = 2.0, 
     cex.main = 2.0)

# 곡선
lines(x, mac_a(x), lwd = 3, col = "blue")
lines(x, mac_b(x), lwd = 3, col = "darkgreen")

# 교차점 및 안내선
abline(v = e_star, lty = 3, col = "gray60")
#abline(h = y_star, lty = 3, col = "gray60")
points(e_star, y_star, pch = 16, col = "red", cex = 1.2)
mtext(expression(a^"*"), side=1, at=e_star, line=1.2, cex=1.2)

# 라벨
text(25, 45, "MAC", col = "blue", cex = 1.5)
text(70, 10, "MB", col = "darkgreen", cex = 1.5)

par(oldpar)


##### 101 page (a) 인규규모 차이
## ===== 공통 설정 =====
oldpar <- par(no.readonly = TRUE)
par(mar = c(5,4.8,2,1), mgp = c(2.6,0.8,0),
    xaxs = "i", yaxs = "i", xaxt="n", yaxt="n")

## x, y 범위
x <- seq(0, 100, length.out = 800)
ylim_all <- c(0, 60)

## 함수 정의 (function 형태로 정의해야 교차점 자동 계산 가능)
mac_a_fun  <- function(z) 0.00045*z^3 + 0.04*z     # MD1
mac_aa_fun <- function(z) 0.00045*z^3 + 0.5*z      # MD2
mac_b_fun  <- function(z) 600/(z + 3) + 3          # MAC

## 교차점 계산
diff1 <- function(z) mac_a_fun(z)  - mac_b_fun(z)  # MD1 - MAC
diff2 <- function(z) mac_aa_fun(z) - mac_b_fun(z)  # MD2 - MAC

e_star  <- uniroot(diff1, c(0,100))$root
y_star  <- mac_a_fun(e_star)

e_star2 <- uniroot(diff2, c(0,100))$root
y_star2 <- mac_aa_fun(e_star2)

## 빈 캔버스
plot(NA, xlim = c(0,100), ylim = ylim_all,
     xlab = "배출량", ylab = "금액",
     main = "인규 규모 차이",
     cex.axis = 2.0, 
     cex.lab = 2.0, 
     cex.main = 2.0)

# 곡선
lines(x, mac_a_fun(x),  lwd = 3, col = "blue")
lines(x, mac_aa_fun(x), lwd = 3, col = "blue")
lines(x, mac_b_fun(x),  lwd = 3, col = "darkgreen")

# 교차점 및 안내선
abline(v = e_star,  lty = 3, col = "gray60")
abline(v = e_star2, lty = 3, col = "gray60")
points(e_star,  y_star,  pch = 16, col = "red", cex = 1.2)
points(e_star2, y_star2, pch = 16, col = "red", cex = 1.2)

# 라벨: e1*, e2*를 x축 아래에
mtext(expression(e[1]), side = 1, at = e_star,  line = 1.2, cex = 1.2)
mtext(expression(e[2]), side = 1, at = e_star2, line = 1.2, cex = 1.2)

# 그래프 내 라벨
text(60, 50, expression(MD[1]), col = "blue", cex = 1.5)
text(30, 50, expression(MD[2]), col = "blue", cex = 1.5)
text(20, 25, "MAC", col = "darkgreen", cex = 1.5)

par(oldpar)


## 101 page (b) ##

## ===== 공통 설정 =====
oldpar <- par(no.readonly = TRUE)
par(mar = c(5,4.8,2,1), mgp = c(2.6,0.8,0),
    xaxs = "i", yaxs = "i", xaxt="n", yaxt="n")

## x, y 범위
x <- seq(0, 100, length.out = 800)
ylim_all <- c(0, 60)

## 함수 정의 (function 형태로 정의해야 교차점 자동 계산 가능)
mac_a_fun  <- function(z) 0.00045*z^3 + 0.04*z     # MD1

mac_b_fun  <- function(z) 600/(z + 3) + 3          # MAC
mac_bb_fun  <- function(z) 300/(z + 3) + 3          # MAC

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

# 그래프 내 라벨
text(55, 50, expression(MD), col = "blue", cex = 1.5)

text(30, 25, expression(MAC[1]), col = "darkgreen", cex = 1.5)
text(5, 25, expression(MAC[2]), col = "darkgreen", cex = 1.5)

par(oldpar)
