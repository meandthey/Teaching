

###
#  xaxt= graph_xaxt_opt   를 par에서 쓰는게 아니라 plot에서 써야지, axis함수가 먹힘


#############################
##### 379 page ##### 그림 15.2
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
  2,  # 축제목(xlab, lab)이 축선에서 떨어져 있는 정도
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

## 공급곡선: 우상향 ↗
supply_fun  <- function(z) 40*z^0

## 수요곡선: 우하향 ↘
demand_fun  <- function(z) 80 - z


## 교차점 계산
diff1 <- function(z) supply_fun(z)  - demand_fun(z) 


q_star  <- uniroot(diff1, xlim_all)$root
y_star  <- supply_fun(q_star)


## 빈 캔버스
graph_cex.axis_opt <- 2.0 # 눈금(tick)의 숫자 크기 조절
graph_cex.lab_opt <- 2.0  # 축제목의 크기 조절
graph_cex.main_opt <- 2.0 # 그래프 제목의 글자 크기 조절절



plot(NA, xlim = xlim_all, ylim = ylim_all,
     xlab = "산출량", ylab = "가격",
     main = "경쟁시장 기업의 수요곡선",
     cex.axis = graph_cex.axis_opt, 
     cex.lab = graph_cex.lab_opt, 
     cex.main = graph_cex.main_opt)

# 곡선
lines(x, supply_fun(x),  lwd = 3, col = "blue")



plot(NA, xlim = xlim_all, ylim = ylim_all,
     xlab = "산출량", ylab = "가격",
     main = "독점시장 기업의 수요곡선",
     cex.axis = graph_cex.axis_opt, 
     cex.lab = graph_cex.lab_opt, 
     cex.main = graph_cex.main_opt)

# 곡선
lines(x, demand_fun(x),  lwd = 3, col = "blue")






#############################
##### 382 page ##### 그림 15.3
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
  2,  # 축제목(xlab, lab)이 축선에서 떨어져 있는 정도
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
ylim_all <- c(-4, 13)
x <- seq(xlim_all[1], xlim_all[2], length.out = 1000)


## 함수 정의 (function 형태로 정의해야 교차점 자동 계산 가능)

## 공급곡선: 우상향 ↗
demand_fun_A  <- function(z) 11-z

## 수요곡선: 우하향 ↘
demand_fun_B  <- function(z) 11-2*z


## 교차점 계산
#diff1 <- function(z) supply_fun(z)  - demand_fun(z) 


#q_star  <- uniroot(diff1, xlim_all)$root
#y_star  <- supply_fun(q_star)


## 빈 캔버스
graph_cex.axis_opt <- 2.0 # 눈금(tick)의 숫자 크기 조절
graph_cex.lab_opt <- 2.0  # 축제목의 크기 조절
graph_cex.main_opt <- 2.0 # 그래프 제목의 글자 크기 조절절



plot(NA, xlim = xlim_all, ylim = ylim_all,
     xlab = "물의 수량", ylab = "가격",
     main = "독점기업의 수요곡선과 한계수입곡선",
     cex.axis = graph_cex.axis_opt, 
     cex.lab = graph_cex.lab_opt, 
     cex.main = graph_cex.main_opt,
     xaxt= graph_xaxt_opt,
     yaxt= graph_yaxt_opt,
     
     panel.first = {
       # 1단위 격자
       abline(h = seq(ceiling(ylim_all[1]), floor(ylim_all[2]), by = 1),
              col = "gray85", lty = 3)
       abline(v = seq(ceiling(xlim_all[1]), floor(xlim_all[2]), by = 1),
              col = "gray85", lty = 3)}
      )

# 곡선
lines(x, demand_fun_A(x),  lwd = 3, col = "blue")
lines(x, demand_fun_B(x),  lwd = 3, col = "red")




# x축 눈금 추가 (1,2,3,...,13)
axis(1, at = seq(0, 13, 1), labels = seq(0, 13, 1), cex.axis = 1.4, pos = 0)


# y축 눈금 추가 (1,2,3,...,13)
axis(2, at = seq(-4, 13, 1), labels = seq(-4, 13, 1), cex.axis = 1.4, pos = 0)


# 특정 좌표에 점 찍기
# x가 1, 2, 3, ..., 10일 때 demand_fun_A(x)의 값
x_vals <- seq(0, 7, 0.5)
demand_fun_A_vals <- demand_fun_A(x_vals)
demand_fun_B_vals <- demand_fun_B(x_vals)

# 점 찍기
points(x_vals, demand_fun_A_vals, pch = 19, col = "blue", cex = 1.5)
points(x_vals, demand_fun_B_vals, pch = 19, col = "red", cex = 1.5)


## text 추가
text(7, 6, "수요(평균수입)", col = "blue", cex = 1.5)
text(2, 4, "한계수입", col = "red", cex = 1.5)





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
  2,  # 축제목(xlab, lab)이 축선에서 떨어져 있는 정도
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
demand_fun_A  <- function(z) 11-z

## 수요곡선: 우하향 ↘
demand_fun_B  <- function(z) 11-2*z


atc_fun <- function(q) 0.5 + 0.4*q + 1.6/q  # q>0

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
     xlab = "수량", ylab = "가격",
     main = "독점기업의 이윤 극대화 과정",
     cex.axis = graph_cex.axis_opt, 
     cex.lab = graph_cex.lab_opt, 
     cex.main = graph_cex.main_opt,
     xaxt= graph_xaxt_opt,
     yaxt= graph_yaxt_opt,
     
     panel.first = {
       # 1단위 격자
       abline(h = seq(ceiling(ylim_all[1]), floor(ylim_all[2]), by = 1),
              col = "gray85", lty = 3)
       abline(v = seq(ceiling(xlim_all[1]), floor(xlim_all[2]), by = 1),
              col = "gray85", lty = 3)}
)

# 곡선
lines(x, demand_fun_A(x),  lwd = 3, col = "blue")
lines(x, demand_fun_B(x),  lwd = 3, col = "red")

atc_x <- x[x>=0.5]
lines(atc_x, atc_fun(atc_x),  lwd = 3, col = "blue")
lines(x, mc_fun(x),  lwd = 3, col = "red")




## 교차점 계산
diff1 <- function(z) demand_fun_B(z) - mc_fun(z)

#q_star  <- uniroot(diff1, xlim_all)$root
root_pos <- uniroot(diff1, c(0, 100))$root



segments(x0=root_pos, y0=0, 
         x1=root_pos, y1=demand_fun_A(root_pos),
         lty=3, col="gray55", lwd=2)

segments(x0=0, y0=demand_fun_A(root_pos), 
         x1=root_pos, y1=demand_fun_A(root_pos),
         lty=3, col="gray55", lwd=2)



# 점 찍기
points(root_pos, demand_fun_A(root_pos), pch = 19, col = "black", cex = 1.5)
points(root_pos, demand_fun_B(root_pos), pch = 19, col = "black", cex = 1.5)


## text 추가
text(4.2, 3.5, "A", col = "black", cex = 1.5)
text(4.2, 7.3, "B", col = "black", cex = 1.5)


text(10.5, 8, "한계비용", col = "black", cex = 1.5)
text(10.5, 1, "수요", col = "black", cex = 1.5)
text(6, 1, "한계수입", col = "black", cex = 1.5)
text(10.5, 4.3, "평균총비용", col = "black", cex = 1.5)



mtext(expression(Q[MAX]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 1.5,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   
axis(side = 1,             # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
     at  = root_pos,   # tick 위치
     labels = FALSE,       # 라벨은 mtext로 따로
     pos = 0,              # y=0에 놓은 x축 위에 그림 (중요!)
     lwd = 0,              # 축 선(line)은 그리지 않음
     lwd.ticks = 1.2       # tick 두께
)

mtext(expression(Q[1]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos-1.2,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 1.5,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)  
axis(side = 1,             # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
     at  = root_pos - 1.2,   # tick 위치
     labels = FALSE,       # 라벨은 mtext로 따로
     pos = 0,              # y=0에 놓은 x축 위에 그림 (중요!)
     lwd = 0,              # 축 선(line)은 그리지 않음
     lwd.ticks = 1.2       # tick 두께
)



mtext(expression(Q[2]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos+1.2,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 1.5,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)  
axis(side = 1,             # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
     at  = root_pos + 1.2,   # tick 위치
     labels = FALSE,       # 라벨은 mtext로 따로
     pos = 0,              # y=0에 놓은 x축 위에 그림 (중요!)
     lwd = 0,              # 축 선(line)은 그리지 않음
     lwd.ticks = 1.2       # tick 두께
)






## 교차점 계산
diff2 <- function(z) demand_fun_A(z) - mc_fun(z)

#q_star  <- uniroot(diff1, xlim_all)$root
root_pos_2 <- uniroot(diff2, c(0, 100))$root

# 점 찍기
#points(0, demand_fun_A(root_pos), pch = 19, col = "black", cex = 1.5)
#points(0, demand_fun_B(root_pos), pch = 19, col = "black", cex = 1.5)

#points(root_pos_2, demand_fun_A(root_pos_2), pch = 19, col = "black", cex = 1.5)


## text 추가
# text(0.3, 3.5, "C", col = "black", cex = 1.5)
# text(0.3, 7.3, "D", col = "black", cex = 1.5)
#text(5.8, 4.7, "E", col = "black", cex = 1.5)




# mtext(expression(Q[C]), 
#       side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
#       at = root_pos_2,      # 텍스트 좌표 위치
#       line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
#       cex = 1.5,      # 글자 크기
#       las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
# )  





#############################
##### 385 page ##### 그림 15.5
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
  2,  # 축제목(xlab, lab)이 축선에서 떨어져 있는 정도
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
demand_fun_A  <- function(z) 11-z

## 수요곡선: 우하향 ↘
demand_fun_B  <- function(z) 11-2*z


atc_fun <- function(q) 0.5 + 0.4*q + 1.6/q  # q>0

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
     xlab = "수량", ylab = "가격",
     main = "독점기업의 이윤",
     cex.axis = graph_cex.axis_opt, 
     cex.lab = graph_cex.lab_opt, 
     cex.main = graph_cex.main_opt,
     xaxt= graph_xaxt_opt,
     yaxt= graph_yaxt_opt,
     
     panel.first = {
       # 1단위 격자
       abline(h = seq(ceiling(ylim_all[1]), floor(ylim_all[2]), by = 1),
              col = "gray85", lty = 3)
       abline(v = seq(ceiling(xlim_all[1]), floor(xlim_all[2]), by = 1),
              col = "gray85", lty = 3)}
)

# 곡선
lines(x, demand_fun_A(x),  lwd = 3, col = "blue")
lines(x, demand_fun_B(x),  lwd = 3, col = "red")

atc_x <- x[x>=0.5]
lines(atc_x, atc_fun(atc_x),  lwd = 3, col = "blue")
lines(x, mc_fun(x),  lwd = 3, col = "red")




## 교차점 계산
diff1 <- function(z) demand_fun_B(z) - mc_fun(z)

#q_star  <- uniroot(diff1, xlim_all)$root
root_pos <- uniroot(diff1, c(0, 100))$root



segments(x0=root_pos, y0=0, 
         x1=root_pos, y1=demand_fun_A(root_pos),
         lty=3, col="gray55", lwd=2)

segments(x0=0, y0=demand_fun_A(root_pos), 
         x1=root_pos, y1=demand_fun_A(root_pos),
         lty=3, col="gray55", lwd=2)



# 점 찍기
points(root_pos, atc_fun(root_pos), pch = 19, col = "black", cex = 1.5)
text(4.1, 2.25, "C", col = "black", cex = 1.5)

points(root_pos, demand_fun_A(root_pos), pch = 19, col = "black", cex = 1.5)
text(4.1, 7.3, "B", col = "black", cex = 1.5)


points(0, demand_fun_A(root_pos), pch = 19, col = "black", cex = 1.5)
text(0.3, 7.3, "E", col = "black", cex = 1.5)

points(0, atc_fun(root_pos), pch = 19, col = "black", cex = 1.5)
text(0.3, 2.25, "D", col = "black", cex = 1.5)




## text 추가
text(10.5, 8, "한계비용", col = "black", cex = 1.5)
text(10.5, 1, "수요", col = "black", cex = 1.5)
text(6, 1, "한계수입", col = "black", cex = 1.5)
text(10.5, 4.3, "평균총비용", col = "black", cex = 1.5)



mtext(expression(Q[MAX]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 1.5,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   
axis(side = 1,             # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
     at  = root_pos,   # tick 위치
     labels = FALSE,       # 라벨은 mtext로 따로
     pos = 0,              # y=0에 놓은 x축 위에 그림 (중요!)
     lwd = 0,              # 축 선(line)은 그리지 않음
     lwd.ticks = 1.2       # tick 두께
)




## 교차점 계산
diff2 <- function(z) demand_fun_A(z) - mc_fun(z)

#q_star  <- uniroot(diff1, xlim_all)$root
root_pos_2 <- uniroot(diff2, c(0, 100))$root







#############################
##### 389 page ##### 그림 15.8과 대비되는 완전경쟁시장 그래프
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
  2,  # 축제목(xlab, lab)이 축선에서 떨어져 있는 정도
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
demand_fun_A  <- function(z) 11-z

## 수요곡선: 우하향 ↘
#demand_fun_B  <- function(z) 11-2*z


#atc_fun <- function(q) 0.5 + 0.4*q + 1.6/q  # q>0

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
     xlab = "수량", ylab = "가격",
     main = "완전경쟁시장의 효율성",
     cex.axis = graph_cex.axis_opt, 
     cex.lab = graph_cex.lab_opt, 
     cex.main = graph_cex.main_opt,
     xaxt= graph_xaxt_opt,
     yaxt= graph_yaxt_opt,
     
     panel.first = {
       # 1단위 격자
       abline(h = seq(ceiling(ylim_all[1]), floor(ylim_all[2]), by = 1),
              col = "gray85", lty = 3)
       abline(v = seq(ceiling(xlim_all[1]), floor(xlim_all[2]), by = 1),
              col = "gray85", lty = 3)}
)

# 곡선
lines(x, demand_fun_A(x),  lwd = 3, col = "blue")
#lines(x, demand_fun_B(x),  lwd = 3, col = "red")

#atc_x <- x[x>=0.5]
#lines(atc_x, atc_fun(atc_x),  lwd = 3, col = "blue")
lines(x, mc_fun(x),  lwd = 3, col = "red")




## 교차점 계산
diff1 <- function(z) demand_fun_A(z) - mc_fun(z)

#q_star  <- uniroot(diff1, xlim_all)$root
root_pos <- uniroot(diff1, c(0, 100))$root



segments(x0=root_pos, y0=0, 
         x1=root_pos, y1=demand_fun_A(root_pos),
         lty=3, col="gray55", lwd=2)

segments(x0=0, y0=demand_fun_A(root_pos), 
         x1=root_pos, y1=demand_fun_A(root_pos),
         lty=3, col="gray55", lwd=2)



# 점 찍기
points(root_pos, demand_fun_A(root_pos), pch = 19, col = "black", cex = 1.5)
#text(6.5, 5.3, "균형", col = "black", cex = 1.5)



#############################
##### 389 page ##### 그림 15.8
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
  2,  # 축제목(xlab, lab)이 축선에서 떨어져 있는 정도
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
demand_fun_A  <- function(z) 11-z

## 수요곡선: 우하향 ↘
demand_fun_B  <- function(z) 11-2*z

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
     xlab = "수량", ylab = "가격",
     main = "독점시장의 비효율성",
     cex.axis = graph_cex.axis_opt, 
     cex.lab = graph_cex.lab_opt, 
     cex.main = graph_cex.main_opt,
     xaxt= graph_xaxt_opt,
     yaxt= graph_yaxt_opt,
     
     panel.first = {
       # 1단위 격자
       abline(h = seq(ceiling(ylim_all[1]), floor(ylim_all[2]), by = 1),
              col = "gray85", lty = 3)
       abline(v = seq(ceiling(xlim_all[1]), floor(xlim_all[2]), by = 1),
              col = "gray85", lty = 3)}
)

# 곡선
lines(x, demand_fun_A(x),  lwd = 3, col = "blue")
lines(x, demand_fun_B(x),  lwd = 3, col = "red")

lines(x, mc_fun(x),  lwd = 3, col = "red")




## 교차점 계산
diff1 <- function(z) demand_fun_B(z) - mc_fun(z)

#q_star  <- uniroot(diff1, xlim_all)$root
root_pos <- uniroot(diff1, c(0, 100))$root



segments(x0=root_pos, y0=0, 
         x1=root_pos, y1=demand_fun_A(root_pos),
         lty=3, col="gray55", lwd=2)

segments(x0=0, y0=demand_fun_A(root_pos), 
         x1=root_pos, y1=demand_fun_A(root_pos),
         lty=3, col="gray55", lwd=2)



# 점 찍기

points(root_pos, demand_fun_A(root_pos), pch = 19, col = "black", cex = 1.5)
#text(4.1, 7.3, "B", col = "black", cex = 1.5)





