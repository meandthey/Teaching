

## 37 page ##
# 데이터: 각 아이스크림에 대한 지불의사 (1~4번째까지)
quantity <- 1:4
willingness <- c(1000, 500, 200, 50)

# --------------------------
# (1) 계단형 수요곡선 (위쪽 그림)
# --------------------------

# 빈 좌표계 먼저 그리기
plot(0, 0, type = "n",
     xlim = c(0, 4),
     ylim = c(0, 1100),
     xlab = "수량",
     ylab = "가격",
     xaxt = "n",  # 기본 x축 안 그림
     main = "지불의사 (계단형)",
     cex.axis = 1.2,
     cex.lab = 1.5,
     cex.main = 2)

# 사용자 정의 x축 (0,1,2,3,4)
axis(1, at = 0:4)

# 계단형 직사각형 그리기
for (i in 1:4) {
  rect(xleft = i-1, 
       ybottom = 0, 
       xright = i, 
       ytop = willingness[i],
       col = "lightblue",
       border = "black")
}

# --------------------------
# (2) 한계지불의사 곡선 (아래쪽 그림)
# --------------------------
# 데이터 생성 (아래로 오목 곡선)
q <- seq(0, 5, by = 0.01)
p <- 1000 * (1 - sqrt(q/5))

# 그래프 그리기
plot(q, p,
     type = "l", lwd = 2, col = "blue",
     xlab = "수량",
     ylab = "가격",
     main = "한계지불의사 곡선 (아래로 오목)",
     xlim = c(0, 5),   # x축 0~5 정확히
     ylim = c(0, 1000),# y축 0~1000 정확히
     xaxs = "i",       # x축 여백 제거
     yaxs = "i",
     cex.axis = 1.2,
     cex.lab = 1.5,
     cex.main = 2)       # y축 여백 제거

# 꼭짓점 강조
points(c(0,5), c(1000,0), pch = 19, col = "red")



## 39 page ##
## ----------------------------
## 시장수요의 도출: x절편까지 그리기 (0에 닿음)
## ----------------------------

a <- 1000    # y절편

# 소비자 A
Q_A <- 5
bA  <- a/Q_A
pA_of_q <- function(q) a - bA*q

# 소비자 B
Q_B <- 10
bB  <- a/Q_B
pB_of_q <- function(q) a - bB*q

# 시장
Q_M <- Q_A + Q_B
bM  <- a/Q_M
pM_of_q <- function(q) a - bM*q

# 데이터
qA <- 0:Q_A
qB <- 0:Q_B
qM <- 0:Q_M
pA <- pA_of_q(qA)
pB <- pB_of_q(qB)
pM <- pM_of_q(qM)

p_ref <- 200

op <- par(mfrow = c(1,3),
          mar   = c(5, 7.5, 4, 2),   # (아래, 왼쪽, 위, 오른쪽)
          mgp   = c(3, 0.8, 0),      # 축 제목/숫자/선 간격
          las   = 1)                 # 축 숫자 가로

## A
plot(qA, pA, type="l", lwd=2, col="blue",
     xlab="", ylab="", main="소비자 A의 수요곡선",
     xlim=c(0,Q_M), ylim=c(0,a), xaxs="i", yaxs="i", 
     cex.axis = 2.0,
     cex.lab = 2.0,
     cex.main = 2,
     las = 1)

abline(h=p_ref, lty=2, col="gray55")
abline(v=(a-p_ref)/bA, lty=3, col="gray55")

## B
plot(qB, pB, type="l", lwd=2, col="blue",
     xlab="", ylab="", main="소비자 B의 수요곡선",
     xlim=c(0,Q_M), ylim=c(0,a), xaxs="i", yaxs="i",
     cex.axis = 2.0,
     cex.lab = 2.0,
     cex.main = 2,
     las = 1)
abline(h=p_ref, lty=2, col="gray55")
abline(v=(a-p_ref)/bB, lty=3, col="gray55")

## 시장
plot(qM, pM, type="l", lwd=2.5, col="blue",
     xlab="", ylab="", main="시장수요곡선 (A와 B)",
     xlim=c(0,Q_M), ylim=c(0,a), xaxs="i", yaxs="i",
     cex.axis = 2.0,
     cex.lab = 2.0,
     cex.main = 2,
     las = 1)
abline(h=p_ref, lty=2, col="gray55")
abline(v=(a-p_ref)/bM, lty=3, col="gray55")

par(op)
par(mfrow=c(1,3), mar=c(5,6,5,2))  # (아래, 왼쪽, 위, 오른쪽)



## 41 page ##
## ----------------------------
## 편익 그래프
## ----------------------------

## B
par(mfrow = c(1,1),
    mar   = c(5, 7.5, 4, 2),   # (아래, 왼쪽, 위, 오른쪽)
    mgp   = c(3, 0.8, 0),      # 축 제목/숫자/선 간격
    las   = 1)                 # 축 숫자 가로
plot(qB, pB, type="l", lwd=2, col="blue",
     xlab="", ylab="", main="소비자 B의 수요곡선",
     xlim=c(0,Q_M), ylim=c(0,a), xaxs="i", yaxs="i",
     cex.axis = 2.0,
     cex.lab = 2.0,
     cex.main = 2,
     las = 1)
abline(h=p_ref, lty=2, col="gray55")
segments(x0=2, y0=0, 
         x1=2, y1=800,
         lty=3, col="gray55", lwd=2)
segments(x0=4, y0=0, 
         x1=4, y1=600,
         lty=3, col="gray55", lwd=2)



## 44 page ##
# 데이터: 비용 (4개의 구간)
costs <- c(50, 100, 150, 200)

# 빈 좌표계 만들기
plot(0, 0, type = "n",
     xlim = c(0, 4), ylim = c(0, 225),
     xlab = "수량", ylab = "비용",
     main = "비용곡선 (계단형)",
     xaxs = "i", yaxs = "i",
     cex.axis = 1.2,
     cex.lab = 1.5,
     cex.main = 2)

# 막대(계단형) 그리기
for (i in 1:4) {
  rect(xleft = i-1,
       ybottom = 0,
       xright = i,
       ytop = costs[i],
       col = "lightblue",
       border = "black")
}




# --------------------------
# (2) 곡선형 한계비용곡선
# --------------------------
# 예시 함수: MC(q) = 20 + 10*q^2  (우상향 곡선)
q <- seq(0, 4, by = 0.1)
mc <- 20 + 10*q^2

plot(q, mc, type = "l", lwd = 2, col = "blue",
     xlim = c(0, 4), ylim = c(0, 200),
     xlab = "수량", ylab = "한계비용",
     main = "한계비용곡선 (곡선형)",
     xaxs = "i", yaxs = "i",
     cex.axis = 1.2,
     cex.lab = 1.5,
     cex.main = 2)

par(op)





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















