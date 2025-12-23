

##### 55 page #####
## ===== 파라미터(원하는 값으로 바꿔도 모양 동일) =====
P_int <- 10          # AR, MR의 y절편(수입곡선 절편)
s_ar  <- 0.5         # AR 기울기 (P = P_int - s_ar * Q)
s_mr  <- 2*s_ar      # MR 기울기 (AR보다 2배 가파름)
AC    <- 4           # AC = MC (수평선 높이)

## 교차점들
# B* : MR = AC
Q_star <- (P_int - AC) / s_mr
P_star <- AC
# B^A : AR = AC
Q_A    <- (P_int - AC) / s_ar
P_A    <- AC

# 보조 포인트: a(= AR at Q*), b(= AC at Q*)
P_a <- P_int - s_ar * Q_star   # a의 높이
P_b <- AC                      # b의 높이(=AC)

## ===== 좌표 준비 =====
Qmax <- max(Q_A*1.05, 14)      # x축 끝(살짝 여유)
q    <- seq(0, Qmax, length.out = 500)
AR   <- P_int - s_ar * q
MR   <- P_int - s_mr * q

## ===== 그리기 =====
op <- par(no.readonly=TRUE)
par(mar=c(5,6,3.5,2), mgp=c(3,1,0), las=1)

plot(NA, xlim=c(0, Qmax), ylim=c(0, P_int*1.02),
     xaxs="i", yaxs="i",
     xaxt="n", yaxt="n",    # <-- 기본 눈금 끄기
     xlab="배의 수", ylab="수입, 비용",
     main="개방자원시장의 비효율성",
     cex.axis=1.4, cex.lab=1.5, cex.main=1.5)

# 축 직접 추가
axis(1, at=seq(0,14,2), labels=seq(0,14,2), cex.axis=1.2)
axis(2, at=seq(0,10,2), labels=seq(0,10,2), cex.axis=1.2, las=1)

# # 축(원한다면 눈금 조정)
# axis(1); axis(2)

# AR, MR, AC=MC
lines(q, AR, lwd=2.5, col="steelblue")     # AR
lines(q, MR, lwd=2.5, col="steelblue3")    # MR
abline(h=AC, lwd=2.5, col="gray30")        # AC = MC

# 수직 점선: B*, B^A
abline(v=Q_star, lty=3, col="gray50")
abline(v=Q_A,    lty=3, col="gray50")

# 수평 점선: a, b 높이
abline(h=P_a, lty=3, col="gray65")
abline(h=P_b, lty=3, col="gray65")

# 포인트와 라벨
points(Q_star, P_a, pch=16, col="black", cex=1)
points(Q_star, P_b, pch=16, col="black", cex=1)

text(Q_star-0.2, P_a+0.25, "a", cex=1.3)
text(Q_star-0.2, P_b+0.25, "b", cex=1.3)

mtext(expression(AC==MC), side=4, at=AC, las=0, line=0.2, cex=1.1)



# 범례
legend("topright", bty="n", cex=1.1,
       legend=c("AR(평균수입)", "MR(한계수입)", "AC = MC(평균/한계비용)"),
       lwd=2.5, col=c("steelblue","steelblue3","gray30"))

## AR, MR, AC=MC 그린 후에 라벨 추가
lines(q, AR, lwd=2.5, col="steelblue")
lines(q, MR, lwd=2.5, col="steelblue3")
abline(h=AC, lwd=2.5, col="gray30")

# 라벨 추가 (좌표는 살짝 옆/위로 조정 가능)
text(8, P_int - s_ar*8 + 0.3, "AR", col="steelblue", cex=1.3)
text(4, P_int - s_mr*4 - 0.3, "MR", col="steelblue3", cex=1.3)
text(Qmax-1, AC+0.3, "AC = MC", col="gray30", cex=1.3)


par(op)



##### 62 page #####


##### 개방자원 도식: D = d_A + d_B (수직합) #####

rm(list = ls())

## 1) 기본 파라미터
AC    <- 4.0
qA    <- 2.0
qB    <- 5.0
Qstar <- 7.5
QAzero <- 7.0
QBzero <- 12.0

## 2) 헬퍼 함수 (하향 직선)
make_down_line <- function(x1, y1, x2, y2){
  stopifnot(x2 > x1, y1 > y2)
  s  <- (y1 - y2) / (x2 - x1)
  P0 <- y1 + s*x1
  list(s = s, P0 = P0,
       f = function(x) P0 - s*x,
       x_int = P0 / s)
}

## 3) d_A, d_B
dA <- make_down_line(qA, AC, QAzero, 0)
dB <- make_down_line(qB, AC, QBzero, 0)

# d_A 절편이 d_B보다 작도록 자동 보정
if (dA$P0 >= dB$P0) {
  QAzero_min <- qA + (AC*qA) / (dB$P0 - AC)
  QAzero     <- max(QAzero, QAzero_min + 0.5)
  dA         <- make_down_line(qA, AC, QAzero, 0)
}

## 4) D = d_A + d_B
D <- list(
  s   = dA$s + dB$s,
  P0  = dA$P0 + dB$P0
)
D$f     <- function(x) D$P0 - D$s*x
D$x_int <- D$P0 / D$s

## 5) 교차점과 균형점
Qx   <- dA$x_int
Px   <- dB$f(Qx)
Qst  <- (D$P0 - AC) / D$s
Pst  <- AC

## 6) 플롯
Qmax <- max(D$x_int, dB$x_int, Qx, QBzero) * 1.12
Ymax <- max(D$P0, dB$P0, AC, Px) * 1.06
q    <- seq(0, Qmax, length.out = 900)

op <- par(no.readonly = TRUE)
par(mar=c(5,6,4,2), mgp=c(3,1.1,0), las=1)

plot(NA, xlim=c(0, Qmax), ylim=c(0, Ymax),
     xaxs="i", yaxs="i", xaxt="n", yaxt="n",
     xlab="수량", ylab="가격, 비용", main="공공재시장의 비효율성",
     cex.axis=1.8, cex.lab=1.8, cex.main=1.8)

axis(1, at=seq(0, round(Qmax), by=2))
axis(2, at=seq(0, 12, by=2), las=1)

# AC = MC (수평선)
abline(h=AC, lwd=3.2, col="gray25")

# 곡선들
lines(q, dB$f(q), lwd=3, col="#1f78b4")
lines(q, dA$f(q), lwd=3, col="#1f78b4")
qD <- seq(0, Qx, length.out=500)
lines(qD, D$f(qD), lwd=3, col="#1f78b4")

# 보조선
abline(v=qA, lty=3, col="gray60")
abline(v=qB, lty=3, col="gray60")
abline(v=Qst, lty=3, col="gray60")

# 교차점
#points(Qx, Px, pch=19, col="red", cex=1.1)
#abline(v=Qx, h=Px, lty=3, col="gray70")

# x축 라벨
axis(1, at=c(qA, qB, Qst),
     labels=c(expression(q[A]), expression(q[B]), expression(Q^"*")),
     tick=FALSE, line=1)

# 라벨들
text(Qst*0.25, D$f(Qst*0.25)+0.35,  expression(D == d[A] + d[B]), cex=1.25) # D = d_A + d_B
text(Qst*0.95, dB$f(Qst*0.95)-0.40, expression(d[B]), cex=1.25)
text(Qst*0.65, dA$f(Qst*0.65)-0.40, expression(d[A]), cex=1.25)
text(Qmax*0.85, AC+0.2, expression(AC == MC), cex=1.1)  # 가로 표기

par(op)





##### 69 page #####
## ===== 공통 설정 =====
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(1,2), mar = c(4,6,2,1), mgp = c(2.8,1.8,0), xaxs = "i", yaxs = "i")
x <- seq(0, 20, length.out = 400)

## -------------------- (a) 음의 외부효과 -------------------- ##
# 수요: P = a - bQ
a <- 14; b <- 0.6
D  <- a - b*x

# 한계비용(사적, 사회): 평행하게 사회가 더 높은 비용
alpha_p <- 2;  beta <- 0.6  # MC_P = alpha_p + beta*Q
alpha_s <- 4                 # MC_S = alpha_s + beta*Q (alpha_s > alpha_p)
MCp <- alpha_p + beta*x
MCs <- alpha_s + beta*x

# 균형량: D = MC
qP_a   <- (a - alpha_p)/(beta + b)  # 시장균형(과잉생산)
qstar_a<- (a - alpha_s)/(beta + b)  # 사회적으로 바람직한 수준 (더 작음)

ylimA <- c(0, max(c(D,MCs)))
plot(NA, xlim = c(0, max(x)), ylim = ylimA,
     xlab = "수량", ylab = "가격, 비용", main = "(a) 음의 외부효과",
     cex.axis=2.0, cex.lab=2.0, cex.main=2.0)
lines(x, D,   lwd = 2)
lines(x, MCp, lwd = 2)
lines(x, MCs, lwd = 2)

abline(v = qP_a,    lty = 2)
abline(v = qstar_a, lty = 2)
# 수직선 아래 표기
mtext(expression(q[P]), side = 1, at = qP_a,    line = 1, cex = 2.0)
mtext(expression(q^"*"), side = 1, at = qstar_a, line = 1, cex = 2.0)

# 곡선 라벨
text(15.5, a - b*15.5,  expression(D),   pos = 4, cex = 2.0)
text(15.5, alpha_p + beta*15.5, expression(MC[P]), pos = 4, cex = 2.0)
text(15.5, alpha_s + beta*15.5, expression(MC[S]), pos = 4, cex = 2.0)

## -------------------- (b) 양의 외부효과 -------------------- ##
# 한계비용 동일
MC <- alpha_p + beta*x

# 수요(사적이 낮고, 사회가 높음): 평행
a_p <- 8.5; a_s <- 11      # D_P = a_p - bQ, D_S = a_s - bQ  (a_s > a_p)
Dp <- a_p - b*x
Ds <- a_s - b*x

# 균형량
qP_b    <- (a_p - alpha_p)/(beta + b)
qstar_b <- (a_s - alpha_p)/(beta + b)

ylimB <- c(0, max(c(Ds,MC)))
plot(NA, xlim = c(0, max(x)), ylim = ylimB,
     xlab = "수량", ylab = "가격, 비용", main = "(b) 양의 외부효과",
     cex.axis=2.0, cex.lab=2.0, cex.main=2.0)
lines(x, Dp, lwd = 2)
lines(x, Ds, lwd = 2)
lines(x, MC, lwd = 2)

abline(v = qP_b,    lty = 2)
abline(v = qstar_b, lty = 2)
mtext(expression(q[P]), side = 1, at = qP_b,    line = 1, cex = 2.0)
mtext(expression(q^"*"), side = 1, at = qstar_b, line = 1, cex = 2.0)

# 곡선 라벨
#text(15.5, a_p - b*15.5, expression(D[P]), pos = 4, cex = 2.0)
text(9, a_p - b*9, expression(D[P]), pos = 4, cex = 2.0)   # y = 8.5 - 5.4 = 3.1
text(15.5, a_s - b*15.5, expression(D[S]), pos = 4, cex = 2.0)
text(15.5, alpha_p + beta*15.5, expression(MC), pos = 4, cex = 2.0)

par(oldpar)



##### 72 page #####
#### 설정값(원하는 모양으로 조정 가능) ####
e0    <- 2     # MD가 x축과 만나는 점
e_star<- 9     # 균형 배출량
eP    <- 18    # D가 x축과 만나는 점
p_star<- 8     # 균형 가격(피해액)

## D(x) = A - b x  (D(eP)=0, D(e_star)=p_star)
b <- p_star/(eP - e_star)
A <- b*eP
Dfun  <- function(x) A - b*x

## MD(x) = m (x - e0)  (MD(e_star)=p_star, MD(e0)=0)
m <- p_star/(e_star - e0)
MDfun <- function(x) m*(x - e0)

#### 캔버스 ####
oldpar <- par(no.readonly = TRUE)
par(mar = c(4.5, 5.3, 1.2, 1), mgp = c(3.2, 1.0, 0), xaxs = "i", yaxs = "i")
xlim <- c(0, 20); ylim <- c(0, 16)

plot(NA, xlim = xlim, ylim = ylim,
     xlab = "배출량", ylab = "금액",
     cex.lab = 1.6, cex.axis = 1.3)

## 곡선
xx <- seq(xlim[1], xlim[2], length.out = 400)
lines(xx, Dfun(xx), lwd = 2)
lines(xx, MDfun(xx), lwd = 2)

## 보조선 (p*, e*, eP)
abline(h = p_star, lty = 2)
abline(v = e_star, lty = 2)
abline(v = eP, lty = 2)

## 축 위 표기
mtext(expression(p^"*"), side = 2, at = p_star, line = 1, cex = 1.4, las = 1)
mtext(expression(e[0]),  side = 1, at = e0,     line = 1, cex = 1.4)
mtext(expression(e^"*"), side = 1, at = e_star, line = 1, cex = 1.4)
mtext(expression(e[P]),  side = 1, at = eP,     line = 1, cex = 1.4)

## 곡선 라벨
text(3.5, Dfun(8.5)+5.6, "폐수 수요 (D)", cex = 1.5)
text(14.2, MDfun(14.2)+0.6, "한계피해액 (MD)", cex = 1.5)

## 영역 라벨 (대략적 위치)
#text(4.2,  p_star+2.0, "a", cex = 1.4)                         # 좌상
#text(4.2,  p_star-2.0, "b", cex = 1.4)                         # 좌하
#text((e0+e_star)/2, MDfun((e0+e_star)/2)-0.8, "c", cex = 1.4)  # 하좌 MD 아래
#text(e_star+2.0, p_star-2.0, "d", cex = 1.4)                   # 하우
#text(e_star+4.2, p_star+1.0, "e", cex = 1.4)                   # 상우(수평선 위)
#text(15.6, (MDfun(15.6)+p_star)/2 + 0.6, "f", cex = 1.4)      # 우측 상단

par(oldpar)









