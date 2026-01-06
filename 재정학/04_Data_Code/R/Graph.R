##### Figure 4-5: 시장수요곡선의 도출 (사용재 vs 공공재) #####

## 공통 설정
oldpar <- par(no.readonly = TRUE)
par(mar = c(4.5,4.5,2.5,1.5), mgp = c(2.2,1.4,0))

## 축(화살표) + 축라벨(P, Q/Z)만 깔끔하게
draw_axes_arrow <- function(xlim, ylim, xlab_txt, ylab_txt, cex = 1.4){
  # x축, y축 (화살표)
  arrows(xlim[1], 0, xlim[2], 0, length = 0.08, lwd = 2)
  arrows(0, ylim[1], 0, ylim[2], length = 0.08, lwd = 2)
  
  # 축 라벨
  text(xlim[2], -0.35, xlab_txt, cex = cex, xpd = NA)
  text(-0.35, ylim[2], ylab_txt, cex = cex, xpd = NA)
}

############################################################
## (i) 사용재: 시장수요 = 수평합 (horizontal sum)
############################################################
xlim1 <- c(0, 10)
ylim1 <- c(0, 10)

plot(NA, xlim = xlim1, ylim = ylim1,
     xlab = "", ylab = "", main = "",
     xaxt = "n", yaxt = "n", bty = "n")

draw_axes_arrow(xlim1, ylim1, xlab_txt = "Q", ylab_txt = "P", cex = 1.5)

# 개별 수요 (역수요: P = a - bQ)
dA <- function(Q) 9 - 1.20*Q
dB <- function(Q) 9 - 0.80*Q

# 시장수요(사용재): Q_M(P)=Q_A(P)+Q_B(P) => 역수요 P_M(Q)
inv_dM <- function(Q){
  # Q_A=(9-P)/1.2, Q_B=(9-P)/0.8
  # Q = (9-P)*(1/1.2 + 1/0.8) = (9-P)*(0.8333+1.25)= (9-P)*2.0833
  # P = 9 - Q/2.0833
  9 - Q/2.083333
}

# 공급 (P = c + dQ)
S1 <- function(Q) 1.2 + 0.85*Q

Qgrid <- seq(0, 10, length.out = 800)

# 곡선들
lines(Qgrid, dA(Qgrid), col = "steelblue", lwd = 3)
lines(Qgrid, dB(Qgrid), col = "steelblue", lwd = 3)
lines(Qgrid, inv_dM(Qgrid), col = "black", lwd = 3)
lines(Qgrid, S1(Qgrid), col = "firebrick", lwd = 4)

# 균형(E): inv_dM(Q)=S1(Q)
# 9 - Q/2.083333 = 1.2 + 0.85Q
Qstar1 <- (9 - 1.2) / (0.85 + 1/2.083333)
Pstar1 <- S1(Qstar1)

points(Qstar1, Pstar1, pch = 16, cex = 1.6)
#text(Qstar1 + 0.35, Pstar1 + 0.35, "E", cex = 1.4)

# P* 수평선, Q* 수직선
segments(xlim1[1], Pstar1, Qstar1, Pstar1, lty = 3, lwd = 2)
segments(Qstar1, ylim1[1], Qstar1, Pstar1, lty = 3, lwd = 2)

# Q_A, Q_B: P*에서 개별이 수요하는 수량
QA <- (9 - Pstar1) / 1.20
QB <- (9 - Pstar1) / 0.80

# 점선(수직) 2개 + 바닥 라벨
segments(QA, ylim1[1], QA, Pstar1, lty = 3, lwd = 1.8)
segments(QB, ylim1[1], QB, Pstar1, lty = 3, lwd = 1.8)

#text(QA, -0.55, expression(Q[A]), cex = 1.2, xpd = NA)
#text(QB, -0.55, expression(Q[B]), cex = 1.2, xpd = NA)
#text(Qstar1, -0.55, expression(Q^"*"), cex = 1.2, xpd = NA)
#text(-0.55, Pstar1, expression(P^"*"), cex = 1.2, xpd = NA)

# 곡선 라벨
#text(8.7, dB(8.7), expression(d[B]), cex = 1.2)
#text(7.3, dA(7.3), expression(d[A]), cex = 1.2)
#text(8.9, inv_dM(8.9) - 0.2, expression(d[M]), cex = 1.2)
#text(6.7, S1(6.7) + 0.3, "S", cex = 1.2, col = "firebrick")

#mtext("(i) 사용재", side = 1, line = 3.0, cex = 1.3)



############################################################
## (ii) 공공재: 시장수요 = 수직합 (vertical sum)
############################################################
xlim2 <- c(0, 10)
ylim2 <- c(0, 14)

plot(NA, xlim = xlim2, ylim = ylim2,
     xlab = "", ylab = "", main = "",
     xaxt = "n", yaxt = "n", bty = "n")

draw_axes_arrow(xlim2, ylim2, xlab_txt = "Z", ylab_txt = "P", cex = 1.5)

# 개별 한계편익(역수요) P_i(Z)
DA <- function(Z) 5.5 - 0.35*Z
DB <- function(Z) 8.5 - 0.55*Z

# 수직합: DM(Z)=DA(Z)+DB(Z)
DM <- function(Z) DA(Z) + DB(Z)

# 공급
S2 <- function(Z) 2.0 + 0.80*Z

Zgrid <- seq(0, 10, length.out = 800)

# 곡선들
lines(Zgrid, DA(Zgrid), col = "steelblue", lwd = 3)
lines(Zgrid, DB(Zgrid), col = "steelblue", lwd = 3)
lines(Zgrid, DM(Zgrid), col = "black", lwd = 3)
lines(Zgrid, S2(Zgrid), col = "firebrick", lwd = 4)

# 균형(F): DM(Z)=S2(Z)
# (5.5-0.35Z)+(8.5-0.55Z)=2+0.8Z  => 14 - 0.9Z = 2 + 0.8Z
Zstar <- (14 - 2) / (0.9 + 0.8)
Pstar2 <- S2(Zstar)

points(Zstar, Pstar2, pch = 16, cex = 1.6)
#text(Zstar + 0.35, Pstar2 + 0.35, "F", cex = 1.4)

# Z* 수직선
segments(Zstar, ylim2[1], Zstar, Pstar2, lty = 3, lwd = 2)

# Z*에서 개인별 지불의사(가격기여) J, K 그리고 합(=균형가격) 표시
PJ <- DB(Zstar)   # J
PK <- DA(Zstar)   # K
# 합은 DM(Zstar)=Pstar2

segments(xlim2[1], PJ, Zstar, PJ, lty = 3, lwd = 1.8)
segments(xlim2[1], PK, Zstar, PK, lty = 3, lwd = 1.8)
segments(xlim2[1], Pstar2, Zstar, Pstar2, lty = 3, lwd = 2)

#text(-0.55, PJ, "J", cex = 1.2, xpd = NA)
#text(-0.55, PK, "K", cex = 1.2, xpd = NA)
#text(-0.55, Pstar2, "L", cex = 1.2, xpd = NA)   # 책 그림처럼 3번째 눈금도 문자로

#text(Zstar, -0.55, expression(Z^"*"), cex = 1.2, xpd = NA)

# 곡선 라벨
#text(7.8, DB(7.8) + 0.2, expression(D[B]), cex = 1.2)
#text(7.0, DA(7.0) - 0.3, expression(D[A]), cex = 1.2)
#text(6.5, DM(6.5) + 0.4, expression(D[M]), cex = 1.2)
#text(8.0, S2(8.0) + 0.5, "S", cex = 1.2, col = "firebrick")

#mtext("(ii) 공공재", side = 1, line = 3.0, cex = 1.3)

par(oldpar)






##### 46 page #####
## 공통 설정
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(1,3), mar = c(4,4,2,1), mgp = c(2.2,1.5,0))
x <- seq(0, 10, length.out = 800)

draw_axes <- function(x_at = seq(0,10,2), y_at = c(6,8,10,12), cex = 2.0){
  axis(1, at = x_at, labels = x_at, cex.axis = cex)
  axis(2, at = y_at, labels = y_at, cex.axis = cex)
}

## (a)
plot(NA, xlim=c(0,10), ylim=c(5.2,12.2),
     xlab=expression("수량"), ylab="비용", main="(a)",
     cex.axis=3.0, cex.lab=3.0, cex.main=3,
     xaxt="n", yaxt="n", xaxs="i", yaxs="i")
draw_axes()
mc_a <- 0.11*(x-4.2)^2 + 5.75 + 0.03*(x-4.2)
lines(x, mc_a, lwd=2)
abline(h=6, lty=3, lwd=1.5)

## (b)
plot(NA, xlim=c(0,10), ylim=c(5.2,12.2),
     xlab=expression("수량"), ylab="비용", main="(b)",
     cex.axis=3.0, cex.lab=3.0, cex.main=3,
     xaxt="n", yaxt="n", xaxs="i", yaxs="i")
draw_axes()
mc_b <- 5.85 + 0.55/(x+1.4) + 0.025*(x-5.5)^2
lines(x, mc_b, lwd=2)

## (c)
plot(NA, xlim=c(0,10), ylim=c(5.2,12.5),
     xlab=expression("수량"), ylab="비용", main="(c)",
     cex.axis=3.0, cex.lab=3.0, cex.main=3,
     xaxt="n", yaxt="n", xaxs="i", yaxs="i")
draw_axes()
q4 <- 6
segments(q4, par("usr")[3], q4, par("usr")[4], lty=3, lwd=1.5)
text(q4, 5.05, expression(""), xpd=NA)

a <- 0.18; b <- 0.045
xL <- seq(0, q4, length.out=400); sL <- xL;    baseL <- 7.20
lines(xL, baseL - a*sL + b*sL^2, lwd=2)
xR <- seq(q4, 10, length.out=400); sR <- xR-q4; baseR <- 9.30
lines(xR, baseR - a*sR + b*sR^2, lwd=2)

par(oldpar)



#### 47 page ####
## ===== 파라미터: 여기만 바꿔도 항상 자연수로 맞춰짐 =====
c0     <- 200      # y절편 (공급 시작가격, 두 생산자 공통)
p_ref  <- 600      # 기준가격: 이때의 수량을 자연수로 맞춤
QA_ref <- 2        # A의 공급량(자연수)
QB_ref <- 8        # B의 공급량(자연수)
Pmax   <- 1000     # y축 상한(그림용)

## 기울기 계산: d = (p_ref - c0)/Q_ref
dA <- (p_ref - c0) / QA_ref
dB <- (p_ref - c0) / QB_ref

## 각 곡선(선형)
pA_of_q <- function(q) c0 + dA*q
pB_of_q <- function(q) c0 + dB*q

## 시장공급(두 생산자 y절편 동일 -> 선형)
dM <- 1 / (1/dA + 1/dB)
pM_of_q <- function(q) c0 + dM*q

## 참조: Pmax에서의 최대 수량(그림 x축용)
QA_max <- (Pmax - c0)/dA
QB_max <- (Pmax - c0)/dB
QM_max <- (Pmax - c0)/dM

## 데이터(부드럽게 보이도록 연속)
qA <- seq(0, QA_max, length.out = 400)
qB <- seq(0, QB_max, length.out = 400)
qM <- seq(0, QM_max, length.out = 600)

## ===== 그림(수요 그림과 동일한 테마) =====
op <- par(mfrow = c(1,3),
          mar   = c(5, 7.5, 4, 2),
          mgp   = c(3, 1.2, 0),
          las   = 1)

## A
plot(qA, pA_of_q(qA), type="l", lwd=2, col="blue",
     xlab="수량", ylab="비용", main="생산자 A의 공급곡선",
     xlim=c(0, 14), ylim=c(0, Pmax), xaxs="i", yaxs="i",
     cex.axis=2.0, cex.lab=2.0, cex.main=2)
abline(h=p_ref, lty=2, col="gray55")
abline(v=QA_ref, lty=3, col="gray55")

## B
plot(qB, pB_of_q(qB), type="l", lwd=2, col="blue",
     xlab="수량", ylab="비용", main="생산자 B의 공급곡선",
     xlim=c(0, 14), ylim=c(0, Pmax), xaxs="i", yaxs="i",
     cex.axis=2.0, cex.lab=2.0, cex.main=2)
abline(h=p_ref, lty=2, col="gray55")
abline(v=QB_ref, lty=3, col="gray55")

## 시장
plot(qM, pM_of_q(qM), type="l", lwd=2.5, col="blue",
     xlab="수량", ylab="비용", main="시장공급곡선 (A와 B)",
     xlim=c(0, 14), ylim=c(0, Pmax), xaxs="i", yaxs="i",
     cex.axis=2.0, cex.lab=2.0, cex.main=2)
abline(h=p_ref, lty=2, col="gray55")
abline(v=QA_ref + QB_ref, lty=3, col="gray55")

par(op)



#### 48 page ####
## ===== 파라미터: 여기만 바꿔도 항상 자연수로 맞춰짐 =====
c0     <- 200      # y절편 (공급 시작가격, 두 생산자 공통)
p_ref  <- 600      # 기준가격: 이때의 수량을 자연수로 맞춤
QA_ref <- 2        # A의 공급량(자연수)
#QB_ref <- 5        # B의 공급량(자연수)
Pmax   <- 1000     # y축 상한(그림용)

## 기울기 계산: d = (p_ref - c0)/Q_ref
dA <- (p_ref - c0) / QA_ref
#dB <- (p_ref - c0) / QB_ref

## 각 곡선(선형)
pA_of_q <- function(q) c0 + dA*q
#pB_of_q <- function(q) c0 + dB*q

## 시장공급(두 생산자 y절편 동일 -> 선형)
#dM <- 1 / (1/dA + 1/dB)
#pM_of_q <- function(q) c0 + dM*q

## 참조: Pmax에서의 최대 수량(그림 x축용)
QA_max <- (Pmax - c0)/dA
#QB_max <- (Pmax - c0)/dB
#QM_max <- (Pmax - c0)/dM

## 데이터(부드럽게 보이도록 연속)
qA <- seq(0, QA_max, length.out = 400)
#qB <- seq(0, QB_max, length.out = 400)
#qM <- seq(0, QM_max, length.out = 600)

## ===== 그림(수요 그림과 동일한 테마) =====
op <- par(mfrow = c(1,1),
          mar   = c(5, 7.5, 4, 2),
          mgp   = c(3, 1.2, 0),
          las   = 1)
## A
plot(qA, pA_of_q(qA), type="l", lwd=2, col="blue",
     xlab="수량", ylab="비용", main="생산자 A의 공급곡선",
     xlim=c(0, 8), ylim=c(0, Pmax), xaxs="i", yaxs="i",
     cex.axis=2.0, cex.lab=2.0, cex.main=2)

segments(x0=2, y0=0, 
         x1=2, y1=600,
         lty=3, col="gray55", lwd=2)
segments(x0=0, y0=600, 
         x1=2, y1=600,
         lty=3, col="gray55", lwd=2)



##### 49 page #####
## ===== 함수 정의 =====
pD_of_q <- function(q) 1000 - (1000/15)*q   # 시장수요: P = 1000 - (1000/15) Q
pS_of_q <- function(q)  200 + 40*q          # 시장공급: P = 200 + 40 Q

## 균형 해(연립방정식)
Q_eq <- 800 / (40 + 1000/15)   # = 7.5
P_eq <- pS_of_q(Q_eq)          # = 500

## ===== 그림 =====
op <- par(no.readonly = TRUE)
par(mar=c(5,7.5,4,2), mgp=c(3,1.2,0), las=1)

plot(NA, xlim=c(0,14), ylim=c(0,1000), xaxs="i", yaxs="i",
     xlab="수량", ylab="금액", main="시장 수요·공급과 균형",
     cex.axis=2.0, cex.lab=2.0, cex.main=2)

q <- seq(0, 14, length.out=600)

## 수요(파란색), 공급(초록색)
lines(q, pD_of_q(q), lwd=2.5, col="blue")
lines(q, pS_of_q(q), lwd=2.5, col="darkgreen")

## 균형점 표시
#abline(h=P_eq, v=Q_eq, lty=3, col="gray55")
points(Q_eq, P_eq, pch=19, cex=1.3, col="red")
text(Q_eq+0.4, P_eq+35, labels=expression(paste("균형 (", c ",", P^"*", ")")),
     cex=1.4)

## 범례(수식 표시)
legend("topright", bty="n", cex=1.3,
       legend=c(expression(P[D](Q) == 1000 - frac(1000,15)*Q),
                expression(P[S](Q) == 200 + 40*Q),
                expression(paste("균형: ", Q^"*"==7.5, ", ", P^"*"==500))),
       lwd=c(2.5, 2.5, NA), 
       col=c("blue", "darkgreen", NA),
       pch=c(NA, NA, 19),
       pt.cex=1.1, 
       text.col=c("black","black","red"))

## Equilibrium, (7.5, 500)
segments(x0=7.5, y0=0, 
         x1=7.5, y1=500,
         lty=3, col="gray55", lwd=2)
segments(x0=0, y0=500, 
         x1=7.5, y1=500,
         lty=3, col="gray55", lwd=2)


## When Q = 5
segments(x0=0, y0=666.7, 
         x1=5, y1=666.7,
         lty=3, col="gray55", lwd=2)
segments(x0=5, y0=0, 
         x1=5, y1=666.7,
         lty=3, col="gray55", lwd=2)



## When Q = 10, P = 400
segments(x0=0, y0=333.3, 
         x1=10, y1=333.3,
         lty=3, col="gray55", lwd=2)
segments(x0=10, y0=0, 
         x1=10, y1=333.3,
         lty=3, col="gray55", lwd=2)




par(op)















