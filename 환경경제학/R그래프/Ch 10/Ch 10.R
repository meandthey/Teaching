
#############################
##### 246 page ##### 그림 10-1
#############################

## ===== 공통 설정 =====
oldpar <- par(no.readonly = TRUE)

##### 그래프 바깥쪽 여백(margin) 조정정
graph_margin_opt <- c(
  5, # 아래쪽 여백
  5, # 왼쪽 여백
  2, # 위쪽 여백
  3  # 오른쪽 여백
)

graph_mgp_opt <- c(
  3,  # 축제목(xlab, lab)이 축선에서 떨어져 있는 정도
  1,  # 눈금 숫자가 축선에서 떨어져 있는 정도
  0   # 축선 자체 위치
)


##### 그래프 선을 여유있게 그릴건지?
graph_xaxs_opt <- "i"   # i: 딱 정확히, r: 여유있게
graph_yaxs_opt <- "i"   # i: 딱 정확히, r: 여유있게


##### 축의 눈금(tick)을 표시할건지?
graph_xaxt_opt <- "n"   # n: tick 안그림,  s: tick 그림
graph_yaxt_opt <- "n"   # n: tick 안그림,  s: tick 그림


par(mar = graph_margin_opt, mgp = graph_mgp_opt,
    xaxs = graph_xaxs_opt, yaxs = graph_yaxs_opt, xaxt= graph_xaxt_opt, yaxt= graph_yaxt_opt)


## x, y 범위
xlim_all <- c(0,100)
ylim_all <- c(0, 90)
x <- seq(xlim_all[1], xlim_all[2], length.out = 1000)


## 함수 정의 (function 형태로 정의해야 교차점 자동 계산 가능)


## 공급곡선: 우상향
demand_fun  <- function(z) 90 - 1*z

supply_fun <- function(z) 0 + 1*z


## 교차점 계산
#diff1 <- function(z) supply_fun(z)  - demand_fun(z) 


#q_star  <- uniroot(diff1, xlim_all)$root
#y_star  <- supply_fun(q_star)


## 빈 캔버스
graph_cex.axis_opt <- 2.0 # 눈금(tick)의 숫자 크기 조절
graph_cex.lab_opt <- 2.0  # 축제목의 크기 조절
graph_cex.main_opt <- 2.0 # 그래프 제목의 글자 크기 조절절



plot(NA, xlim = xlim_all, ylim = ylim_all,
     xlab = "배출량", ylab = "금액",
     main = "환경정책의 효율성과 형평성",
     cex.axis = graph_cex.axis_opt, 
     cex.lab = graph_cex.lab_opt, 
     cex.main = graph_cex.main_opt)

# 곡선
lines(x, demand_fun(x),  lwd = 3, col = "blue")
lines(x, supply_fun(x), lwd = 3, col = "blue")



## 교차점 계산
diff1 <- function(z) demand_fun(z) - supply_fun(z)
diff2 <- function(z) demand_fun(z) - 0

#q_star  <- uniroot(diff1, xlim_all)$root
root_pos <- uniroot(diff1, c(0, 100))$root
abline(h = root_pos ,  lty = 2, col = "gray60", lwd = 0.1)
abline(v = supply_fun(root_pos) ,  lty = 2, col = "gray60", lwd = 0.1)

root_pos2 <- uniroot(diff2, c(0, 100))$root
abline(v = (root_pos2) ,  lty = 2, col = "gray60", lwd = 0.1)




mtext(expression(e^"*"), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   


mtext(expression(e^"0"), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos2,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   





#############################
##### 247 page ##### 그림 10-2
#############################
## ====== 공통 파라미터 ======
p  <- 0.4        # ↓ 기울기 완만하게 (이전엔 0.9)
alpha <- 0.40      # 두 사람 동일한 α → 같은 곡률

## 접점을 원하는 위치로 지정
qP_star <- 42
qR_star <- 82

## 절편 계산 (같은 α, 같은 p)
mP <- p*qP_star/alpha
mR <- p*qR_star/alpha

## y절편을 전체적으로 낮춤 (원하면 0.8~0.9 정도)
scale_factor <- 0.8
mP <- mP * scale_factor
mR <- mR * scale_factor

## 새 접점 계산
qP_star <- alpha*mP/p
qR_star <- alpha*mR/p
yP_star <- mP - p*qP_star
yR_star <- mR - p*qR_star

## IC 효용수준
U_P <- qP_star^alpha * yP_star^(1-alpha)
U_R <- qR_star^alpha * yR_star^(1-alpha)

## ====== 함수 정의 ======
b_fun  <- function(m, q) m - p*q
ic_fun <- function(U, q) (U / (pmax(q, 1e-6)^alpha))^(1/(1-alpha))

## ====== 그리기 ======
oldpar <- par(no.readonly = TRUE)
par(mar=c(5,5,2,3), mgp=c(3,1,0), xaxs="i", yaxs="i", xaxt="n", yaxt="n")
#par(mar=c(5,5,2,3), mgp=c(3,1,0), xaxs="i", yaxs="i", xaxt="s", yaxt="n")

xlim_all <- c(0,100); ylim_all <- c(0,100)
x <- seq(xlim_all[1], xlim_all[2], length.out = 2000)

plot(NA, xlim=xlim_all, ylim=ylim_all,
     xlab="환경재", ylab="기타 재화 및 용역",
     main="환경재에 대한 수요: 동일한 환경재 가격",
     cex.axis=2, cex.lab=2, cex.main=2)

## 예산선 (완만한 기울기)
lines(x, b_fun(mP, x), lwd=2)
lines(x, b_fun(mR, x), lwd=2)

## 무차별곡선 (같은 α)
lines(x, ic_fun(U_P, x), lwd=3, col="blue")
lines(x, ic_fun(U_R, x), lwd=3, col="blue")

## 접점 표시
abline(v=qP_star, lty=3, col="gray70")
abline(v=qR_star, lty=3, col="gray70")

mtext(expression(q^P), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = qP_star,      # 텍스트 좌표 위치
      line = 1.5,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   
mtext(expression(q^R), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = qR_star,      # 텍스트 좌표 위치
      line = 1.5,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
) 


## 라벨
# text(12, b_fun(mP,12)+6, expression(b^P), cex=1.6)
# text(12, b_fun(mR,12)+14, expression(b^R), cex=1.6)
# text(85, ic_fun(U_R,85)+2, expression(I^R), col="blue", cex=1.6)
# text(75, ic_fun(U_P,75)-6, expression(I^P), col="blue", cex=1.6)

par(oldpar)



#############################
##### 248 page ##### 그림 10-3
#############################


## ===== 공통 설정 =====
oldpar <- par(no.readonly = TRUE)

graph_margin_opt <- c(5,5,2,3)
graph_mgp_opt    <- c(1,1,0)
graph_xaxs_opt   <- "i"
graph_yaxs_opt   <- "i"
graph_xaxt_opt   <- "n"   # 눈금/숫자 숨김
graph_yaxt_opt   <- "n"

par(mar=graph_margin_opt, mgp=graph_mgp_opt,
    xaxs=graph_xaxs_opt, yaxs=graph_yaxs_opt,
    xaxt=graph_xaxt_opt, yaxt=graph_yaxt_opt)

## ===== 범위 =====
xlim_all <- c(0, 150)
ylim_all <- c(0, 100)
x <- seq(xlim_all[1], xlim_all[2], length.out=2000)

## ===== 예산선: y = m - p*q  (p가 서로 다름) =====
p_top <- 0.50   # 위 예산선 기울기 (더 가파름)
m_top <- 62     # 위 예산선 y절편

p_bot <- 0.35   # 아래 예산선 기울기 (더 완만)
m_bot <- 32     # 아래 예산선 y절편

b_fun <- function(m, p, q) m - p*q

## x절편(확인용): 서로 다르게 떨어지게 설정됨
xint_top <- m_top / p_top  # 위 선 x절편
xint_bot <- m_bot / p_bot  # 아래 선 x절편
# cat(sprintf("Δy-int=%.2f, Δx-int=%.2f\n", abs(m_top-m_bot), abs(xint_top-xint_bot)))

## ===== 빈 캔버스 =====
plot(NA, xlim=xlim_all, ylim=ylim_all,
     xlab="환경재", ylab="기타 재화 및 용역",
     main="환경재에 대한 수요: 누진적인 환경재 가격",
     cex.axis=2.0, cex.lab=2.0, cex.main=2.0)

## ===== 예산선 그리기 =====
lines(x, b_fun(m_top, p_top, x), lwd=3, col="blue")  # 위 예산선
lines(x, b_fun(m_bot, p_bot, x), lwd=3, col="blue")  # 아래 예산선

## (선택) 라벨 넣고 싶으면 주석 해제
# text(8,  b_fun(m_top, p_top, 8)+5,  expression(b^R), cex=1.6)
# text(8,  b_fun(m_bot, p_bot, 8)+5,  expression(b^P), cex=1.6)
# abline(v=xint_top, lty=3, col="gray70"); abline(v=xint_bot, lty=3, col="gray70")
# abline(h=m_top, lty=3, col="gray70");    abline(h=m_bot, lty=3, col="gray70")

par(oldpar)



#############################
##### 250 page ##### 그림 10-4 (a)
#############################
#############################
##### (a) 패널: U^i(Q) 3곡선 #####
#############################

## ---- 네가 쓰던 그래프 기본 포맷 ----
oldpar <- par(no.readonly = TRUE)

par(mar=c(5,5,2,2), mgp=c(3,1,0),
    xaxs="i", yaxs="i", xaxt="n", yaxt="n")

xlim_all <- c(0, 110)
ylim_all <- c(0, 100)
plot(NA, xlim=xlim_all, ylim=ylim_all,
     xlab=expression(Q), ylab=expression(U^i(Q)),
     main="다수결 원칙에 따른 환경질 선택 (a)",
     cex.lab=1.8, cex.axis=1.6, cex.main=1.8)

## ---- 효용곡선 모양 설정 (아래로 볼록한 포물선) ----
## U^k(Q) = Hk - a * (Q - Qk)^2
a  <- 0.08                 # 곡률(모양)
QA <- 30; QB <- 60; QC <- 90  # 각 곡선의 정점 위치 (Q^A, Q^B, Q^C)
HA <- 80; HB <- 86; HC <- 92  # 정점 높이(원본처럼 C가 살짝 더 높게)

U <- function(Q, H, Q0, a) H - a*(Q - Q0)^2

## 곡선 그리기
xx <- seq(xlim_all[1], xlim_all[2], length.out=2000)
lines(xx, U(xx, HA, QA, a), lwd=3, col="#1f5aa6")  # U^A(Q)
lines(xx, U(xx, HB, QB, a), lwd=3, col="#1f5aa6")  # U^B(Q)
lines(xx, U(xx, HC, QC, a), lwd=3, col="#1f5aa6")  # U^C(Q)

## ---- 수직 보조선: Q^1, Q^B, Q^2 ----
Q1 <- QA                       # 왼쪽 정점 위치 (Q^1)
Q2 <- QC                       # 오른쪽 정점 위치 (Q^2)
abline(v=Q1-5, lty=3, col="gray65")
abline(v=QB, lty=3, col="gray65")
abline(v=Q2+5, lty=3, col="gray65")

## ---- Q^B에서의 각 효용값: U^A(Q^B), U^C(Q^B), U^B(Q^B) ----
UA_QB <- U(QB, HA, QA, a)
UC_QB <- U(QB, HC, QC, a)
UB_QB <- U(QB, HB, QB, a)

abline(h=UA_QB, lty=3, col="gray65")
abline(h=UC_QB, lty=3, col="gray65")
abline(h=UB_QB, lty=3, col="gray65")

## ---- 축 아래 라벨 (Q^1, Q^B, Q^2) ----
mtext(expression(Q^1), side=1, at=Q1-5, line=1.4, cex=1.5)
mtext(expression(Q^B), side=1, at=QB, line=1.4, cex=1.5)
mtext(expression(Q^2), side=1, at=Q2+5, line=1.4, cex=1.5)

## ---- 좌측 y축 라벨 (U^A(Q^B), U^C(Q^B), U^B(Q^B)) ----
# ##    위치는 살짝 왼쪽에 텍스트로 붙임
# text(x=2, y=UA_QB, labels=expression(U^A(Q^B)), adj=c(0,0.5), cex=1.3)
# text(x=2, y=UC_QB, labels=expression(U^C(Q^B)), adj=c(0,0.5), cex=1.3)
# text(x=2, y=UB_QB, labels=expression(U^B(Q^B)), adj=c(0,0.5), cex=1.3)
# 
# ## ---- 곡선 하단 라벨 (U^A(Q), U^B(Q), U^C(Q)) ----
# text(QA-10, 5,  labels=expression(U^A(Q)), cex=1.3)
# text(QB,    5,  labels=expression(U^B(Q)), cex=1.3)
# text(QC+10, 5,  labels=expression(U^C(Q)), cex=1.3)

par(oldpar)




#############################
##### 250 page ##### 그림 10-4 (b)
#############################
#############################
##### (b) 패널: U^i(Q) 3곡선 #####
#############################

## ---- 네가 쓰던 그래프 기본 포맷 ----
oldpar <- par(no.readonly = TRUE)

par(mar=c(5,5,2,2), mgp=c(3,1,0),
    xaxs="i", yaxs="i", xaxt="n", yaxt="n")

xlim_all <- c(0, 150)
ylim_all <- c(-150, 100)
plot(NA, xlim=xlim_all, ylim=ylim_all,
     xlab=expression(Q), ylab=expression(U^i(Q)),
     main="다수결 원칙에 따른 환경질 선택 (b)",
     cex.lab=1.8, cex.axis=1.6, cex.main=1.8)

## ---- 효용곡선 모양 설정 (아래로 볼록한 포물선) ----
## U^k(Q) = Hk - a * (Q - Qk)^2
a  <- 0.08                 # 곡률(모양)
QA <- 30; QB <- 60; QC <- 110  # 각 곡선의 정점 위치 (Q^A, Q^B, Q^C)
HA <- 80; HB <- 86; HC <- 92  # 정점 높이(원본처럼 C가 살짝 더 높게)

U <- function(Q, H, Q0, a) H - a*(Q - Q0)^2

## 곡선 그리기
xx <- seq(xlim_all[1], xlim_all[2], length.out=2000)
lines(xx, U(xx, HA, QA, a), lwd=3, col="#1f5aa6")  # U^A(Q)
lines(xx, U(xx, HB, QB, a), lwd=3, col="#1f5aa6")  # U^B(Q)
lines(xx, U(xx, HC, QC, a), lwd=3, col="#1f5aa6")  # U^C(Q)

## ---- 수직 보조선: Q^1, Q^B, Q^2 ----
Q1 <- QA                       # 왼쪽 정점 위치 (Q^1)
Q2 <- QC                       # 오른쪽 정점 위치 (Q^2)
abline(v=Q1-5, lty=3, col="gray65")
abline(v=QB, lty=3, col="gray65")
abline(v=Q2+5, lty=3, col="gray65")

## ---- Q^B에서의 각 효용값: U^A(Q^B), U^C(Q^B), U^B(Q^B) ----
UA_QB <- U(QB, HA, QA, a)
UC_QB <- U(QB, HC, QC, a)
UB_QB <- U(QB, HB, QB, a)

abline(h=UA_QB, lty=3, col="gray65")
abline(h=UC_QB, lty=3, col="gray65")
abline(h=UB_QB, lty=3, col="gray65")

## ---- 축 아래 라벨 (Q^1, Q^B, Q^2) ----
mtext(expression(Q^1), side=1, at=Q1-5, line=1.4, cex=1.5)
mtext(expression(Q^B), side=1, at=QB, line=1.4, cex=1.5)
mtext(expression(Q^2), side=1, at=Q2+5, line=1.4, cex=1.5)

abline(h=0, lwd=1.5, col="black")
## ---- 좌측 y축 라벨 (U^A(Q^B), U^C(Q^B), U^B(Q^B)) ----
# ##    위치는 살짝 왼쪽에 텍스트로 붙임
# text(x=2, y=UA_QB, labels=expression(U^A(Q^B)), adj=c(0,0.5), cex=1.3)
# text(x=2, y=UC_QB, labels=expression(U^C(Q^B)), adj=c(0,0.5), cex=1.3)
# text(x=2, y=UB_QB, labels=expression(U^B(Q^B)), adj=c(0,0.5), cex=1.3)
# 
# ## ---- 곡선 하단 라벨 (U^A(Q), U^B(Q), U^C(Q)) ----
# text(QA-10, 5,  labels=expression(U^A(Q)), cex=1.3)
# text(QB,    5,  labels=expression(U^B(Q)), cex=1.3)
# text(QC+10, 5,  labels=expression(U^C(Q)), cex=1.3)

par(oldpar)


#############################
##### 258 page ##### 그림 10-5
#############################

## ===== 공통 설정 =====
oldpar <- par(no.readonly = TRUE)

##### 그래프 바깥쪽 여백(margin) 조정정
graph_margin_opt <- c(
  5, # 아래쪽 여백
  5, # 왼쪽 여백
  2, # 위쪽 여백
  3  # 오른쪽 여백
)

graph_mgp_opt <- c(
  3,  # 축제목(xlab, lab)이 축선에서 떨어져 있는 정도
  1,  # 눈금 숫자가 축선에서 떨어져 있는 정도
  0   # 축선 자체 위치
)


##### 그래프 선을 여유있게 그릴건지?
graph_xaxs_opt <- "i"   # i: 딱 정확히, r: 여유있게
graph_yaxs_opt <- "i"   # i: 딱 정확히, r: 여유있게


##### 축의 눈금(tick)을 표시할건지?
graph_xaxt_opt <- "n"   # n: tick 안그림,  s: tick 그림
graph_yaxt_opt <- "n"   # n: tick 안그림,  s: tick 그림


par(mar = graph_margin_opt, mgp = graph_mgp_opt,
    xaxs = graph_xaxs_opt, yaxs = graph_yaxs_opt, xaxt= graph_xaxt_opt, yaxt= graph_yaxt_opt)


## x, y 범위
xlim_all <- c(0,80)
ylim_all <- c(0,80)
x <- seq(xlim_all[1], xlim_all[2], length.out = 1000)


## 함수 정의 (function 형태로 정의해야 교차점 자동 계산 가능)


## 공급곡선: 우상향
demand_fun_a  <- function(z) 80 - 0.5*z
demand_fun_b  <- function(z) 40 - 0.5*z

supply_fun_a <- function(z) 25 + 1*z
supply_fun_b <- function(z) -20 + 1*z


## 교차점 계산
#diff1 <- function(z) supply_fun(z)  - demand_fun(z) 


#q_star  <- uniroot(diff1, xlim_all)$root
#y_star  <- supply_fun(q_star)


## 빈 캔버스
graph_cex.axis_opt <- 2.0 # 눈금(tick)의 숫자 크기 조절
graph_cex.lab_opt <- 2.0  # 축제목의 크기 조절
graph_cex.main_opt <- 2.0 # 그래프 제목의 글자 크기 조절절



plot(NA, xlim = xlim_all, ylim = ylim_all,
     xlab = "배출량", ylab = "금액",
     main = "중앙정부와 지방자치단체의 환경정책",
     cex.axis = graph_cex.axis_opt, 
     cex.lab = graph_cex.lab_opt, 
     cex.main = graph_cex.main_opt)

# 곡선
lines(x, demand_fun_a(x),  lwd = 3, col = "blue")
lines(x, demand_fun_b(x), lwd = 3, col = "blue")
lines(x, supply_fun_a(x),  lwd = 3, col = "blue")
lines(x, supply_fun_b(x), lwd = 3, col = "blue")



## 교차점 계산
diff1 <- function(z) demand_fun_b(z) - supply_fun_a(z)
diff2 <- function(z) demand_fun_a(z) - supply_fun_a(z)
diff3 <- function(z) demand_fun_a(z) - supply_fun_b(z)


root_pos1 <- uniroot(diff1, c(0, 100))$root
abline(v = (root_pos1) ,  lty = 2, col = "gray60", lwd = 0.1)

root_pos2 <- uniroot(diff2, c(0, 100))$root
abline(v = (root_pos2) ,  lty = 2, col = "gray60", lwd = 0.1)

root_pos3 <- uniroot(diff3, c(0, 100))$root
abline(v = (root_pos3) ,  lty = 2, col = "gray60", lwd = 0.1)



mtext(expression(e[2]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos1,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   


mtext(expression(e^"C"), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos2,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   



mtext(expression(e[1]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos3,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   



#############################
##### 265 page ##### 그림 10-6
#############################

## ===== 공통 설정 =====
oldpar <- par(no.readonly = TRUE)

##### 그래프 바깥쪽 여백(margin) 조정정
graph_margin_opt <- c(
  5, # 아래쪽 여백
  5, # 왼쪽 여백
  2, # 위쪽 여백
  3  # 오른쪽 여백
)

graph_mgp_opt <- c(
  3,  # 축제목(xlab, lab)이 축선에서 떨어져 있는 정도
  1,  # 눈금 숫자가 축선에서 떨어져 있는 정도
  0   # 축선 자체 위치
)


##### 그래프 선을 여유있게 그릴건지?
graph_xaxs_opt <- "i"   # i: 딱 정확히, r: 여유있게
graph_yaxs_opt <- "i"   # i: 딱 정확히, r: 여유있게


##### 축의 눈금(tick)을 표시할건지?
graph_xaxt_opt <- "n"   # n: tick 안그림,  s: tick 그림
graph_yaxt_opt <- "n"   # n: tick 안그림,  s: tick 그림


par(mar = graph_margin_opt, mgp = graph_mgp_opt,
    xaxs = graph_xaxs_opt, yaxs = graph_yaxs_opt, xaxt= graph_xaxt_opt, yaxt= graph_yaxt_opt)


## x, y 범위
xlim_all <- c(0,100)
ylim_all <- c(0, 100)
x <- seq(xlim_all[1], xlim_all[2], length.out = 1000)


## 함수 정의 (function 형태로 정의해야 교차점 자동 계산 가능)


## 공급곡선: 우상향
demand_fun  <- function(z) 90 - 0.7*z
#demand_fun_a  <- function(z) 70 - 0.7*z



## 교차점 계산
#diff1 <- function(z) supply_fun(z)  - demand_fun(z) 


#q_star  <- uniroot(diff1, xlim_all)$root
#y_star  <- supply_fun(q_star)


## 빈 캔버스
graph_cex.axis_opt <- 2.0 # 눈금(tick)의 숫자 크기 조절
graph_cex.lab_opt <- 2.0  # 축제목의 크기 조절
graph_cex.main_opt <- 2.0 # 그래프 제목의 글자 크기 조절절



plot(NA, xlim = xlim_all, ylim = ylim_all,
     xlab = "수량", ylab = "가격",
     main = "부과금의 비용",
     cex.axis = graph_cex.axis_opt, 
     cex.lab = graph_cex.lab_opt, 
     cex.main = graph_cex.main_opt)

# 곡선
lines(x, demand_fun(x),  lwd = 3, col = "blue")




## 교차점 계산
diff1 <- function(z) demand_fun(z) - 50

#q_star  <- uniroot(diff1, xlim_all)$root
root_pos <- uniroot(diff1, c(0, 100))$root
abline(h = 50 ,  lty = 1, col = "blue", lwd = 2.5)

abline(v = (root_pos) ,  lty = 3, col = "gray60")
mtext(expression(X[i]^"0"), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos,      # 텍스트 좌표 위치
      line = 1.7,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   



## 교차점 계산
diff2 <- function(z) demand_fun(z) - 70
root_pos_a <- uniroot(diff2, c(0, 100))$root

abline(h = 70 ,  lty = 3, col = "black", lwd = 2.5)
abline(v = (root_pos_a) ,  lty = 3, col = "gray60")
mtext(expression(X[i]^"1"), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos_a,      # 텍스트 좌표 위치
      line = 1.7,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)

#abline(h = demand_fun(root_pos) ,  lty = 6, col = "black", lwd = 2.5)



mtext(expression(P[X]^"0"), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = demand_fun(root_pos),      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   


mtext(expression(P[X]^"1"), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = demand_fun(root_pos_a),      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   




