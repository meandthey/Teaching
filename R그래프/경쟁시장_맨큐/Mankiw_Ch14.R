

#############################
##### 383 page ##### 그림 15.4
#############################
## ===== 공통 설정 =====
oldpar <- par(no.readonly = TRUE)

##### 그래프 바깥쪽 여백(margin) 조정정
graph_margin_opt <- c(
  4, # 아래쪽 여백
  5, # 왼쪽 여백
  2, # 위쪽 여백
  1  # 오른쪽 여백
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
    xaxs = graph_xaxs_opt, yaxs = graph_yaxs_opt)


## x, y 범위
xlim_all <- c(0,13)
ylim_all <- c(0, 13)
x <- seq(xlim_all[1], xlim_all[2], length.out = 1000)


## 함수 정의 (function 형태로 정의해야 교차점 자동 계산 가능)

## 공급곡선: 우상향 ↗
#demand_fun_A  <- function(z) 11-z

## 수요곡선: 우하향 ↘
demand_fun_A  <- function(z) 5 + z^(0)


atc_fun <- function(q) 0.5 + 0.4*q + 9/q  # q>0

mc_fun  <- function(z) 0.5 + 0.8*z
## 교차점 계산
#diff1 <- function(z) supply_fun(z)  - demand_fun(z) 


#q_star  <- uniroot(diff1, xlim_all)$root
#y_star  <- supply_fun(q_star)


## 빈 캔버스
graph_cex.axis_opt <- 2.0 # 눈금(tick)의 숫자 크기 조절
graph_cex.lab_opt <- 2.0  # 축제목의 크기 조절
graph_cex.main_opt <- 2.0 # 그래프 제목의 글자 크기 조절절



plot(NA, xlim = xlim_all, ylim = ylim_all,
     xlab = "수량", ylab = "비용과 수입",
     main = "경쟁시장 기업의 이윤 극대화",
     cex.axis = graph_cex.axis_opt, 
     cex.lab = graph_cex.lab_opt, 
     cex.main = graph_cex.main_opt,
     xaxt= graph_xaxt_opt,
     yaxt= graph_yaxt_opt
     
     # panel.first = {
     #   # 1단위 격자
     #   abline(h = seq(ceiling(ylim_all[1]), floor(ylim_all[2]), by = 1),
     #          col = "gray85", lty = 3)
     #   abline(v = seq(ceiling(xlim_all[1]), floor(xlim_all[2]), by = 1),
     #          col = "gray85", lty = 3)}
)

# 곡선
lines(x, demand_fun_A(x),  lwd = 3, col = "red")
lines(x, mc_fun(x),  lwd = 3, col = "red")

atc_x <- x[x>=1.2]
lines(atc_x, atc_fun(atc_x),  lwd = 3, col = "blue")





## 교차점 계산
diff1 <- function(z) mc_fun(z) - demand_fun_A(z)

#q_star  <- uniroot(diff1, xlim_all)$root
root_pos <- uniroot(diff1, c(0, 100))$root



segments(x0=root_pos, y0=0, 
         x1=root_pos, y1=mc_fun(root_pos),
         lty=3, col="gray55", lwd=2)


# 점 찍기
points(root_pos, demand_fun_A(root_pos), pch = 19, col = "black", cex = 1.5)



## text 추가
text(10, 9, "MC", col = "black", cex = 1.5)
text(10, 5, "ATC", col = "black", cex = 1.5)
text(4.5, 6.2, "P=AR=MR", col = "black", cex = 1.5)





mtext(expression(Q[MAX]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 1.5,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   
