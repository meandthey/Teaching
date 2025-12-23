

#############################
##### 337 page ##### 그림 14-1
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
demand_fun  <- function(z) 80 - 0.7*z
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
     xlab = expression(X[1]), ylab = "",
     main = "소비자잉여의 변화분",
     cex.axis = graph_cex.axis_opt, 
     cex.lab = graph_cex.lab_opt, 
     cex.main = graph_cex.main_opt)

# 곡선
lines(x, demand_fun(x),  lwd = 3, col = "blue")




## 교차점 계산
diff1 <- function(z) demand_fun(z) - 60

#q_star  <- uniroot(diff1, xlim_all)$root
root_pos <- uniroot(diff1, c(0, 100))$root
abline(h = 60 ,  lty = 3, col = "gray60", lwd = 2.5)
abline(h = 40 ,  lty = 3, col = "gray60", lwd = 2.5)

mtext(expression(p[1]^"0"), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = 60,      # 텍스트 좌표 위치
      line = 1.7,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   

mtext(expression(p[1]^"1"), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = 40,      # 텍스트 좌표 위치
      line = 1.7,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   






#############################
##### 339 page ##### 그림 14-2 (a)
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
demand_fun_a  <- function(z) 70 - 0.7*z
demand_fun_b  <- function(z) 70 - 1.4*z
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
     xlab = "", ylab = "",
     main = "통상수요와 보상수요",
     cex.axis = graph_cex.axis_opt, 
     cex.lab = graph_cex.lab_opt, 
     cex.main = graph_cex.main_opt)

# 곡선
lines(x, demand_fun_a(x),  lwd = 3, col = "blue")
lines(x, demand_fun_b(x),  lwd = 3, col = "blue")




#############################
##### 339 page ##### 그림 14-2 (b)
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
demand_fun_a  <- function(z) 80 - 0.7*z
demand_fun_b  <- function(z) 71.5 - 0.4*z
#demand_fun_a  <- function(z) 70 - 0.7*z



## 교차점 계산
diff1 <- function(z) demand_fun_a(z)  - demand_fun_b(z) 


q_star  <- uniroot(diff1, xlim_all)$root



## 빈 캔버스
graph_cex.axis_opt <- 2.0 # 눈금(tick)의 숫자 크기 조절
graph_cex.lab_opt <- 2.0  # 축제목의 크기 조절
graph_cex.main_opt <- 2.0 # 그래프 제목의 글자 크기 조절절



plot(NA, xlim = xlim_all, ylim = ylim_all,
     xlab = "", ylab = "",
     main = "통상수요와 보상수요",
     cex.axis = graph_cex.axis_opt, 
     cex.lab = graph_cex.lab_opt, 
     cex.main = graph_cex.main_opt)

# 곡선
lines(x, demand_fun_a(x),  lwd = 3, col = "blue")
lines(x, demand_fun_b(x),  lwd = 3, col = "blue")


abline(h = demand_fun_a(q_star),  lty = 3, col = "gray60", lwd = 2.5)
abline(h = 40 ,  lty = 3, col = "gray60", lwd = 2.5)
abline(v = q_star,  lty = 3, col = "gray60", lwd = 2.5)


diff2 <- function(z) demand_fun_a(z)  - 40
diff3 <- function(z) demand_fun_b(z)  - 40

q_star_2  <- uniroot(diff2, xlim_all)$root
q_star_3  <- uniroot(diff3, xlim_all)$root

abline(v = q_star_2,  lty = 3, col = "gray60", lwd = 2.5)
abline(v = q_star_3,  lty = 3, col = "gray60", lwd = 2.5)

