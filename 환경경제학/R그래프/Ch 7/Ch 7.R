

#############################
##### 139 page ##### 그림 7-1
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
supply_fun  <- function(z) 0.00005*z^3 + 0.04*z     

## 수요곡선: 우하향 ↘
demand_fun  <- function(z) 900/(z + 3) - 900/103


## 교차점 계산
diff1 <- function(z) supply_fun(z)  - demand_fun(z) 


q_star  <- uniroot(diff1, xlim_all)$root
y_star  <- supply_fun(q_star)


## 빈 캔버스
graph_cex.axis_opt <- 2.0 # 눈금(tick)의 숫자 크기 조절
graph_cex.lab_opt <- 2.0  # 축제목의 크기 조절
graph_cex.main_opt <- 2.0 # 그래프 제목의 글자 크기 조절절



plot(NA, xlim = xlim_all, ylim = ylim_all,
     xlab = "배출량", ylab = "금액",
     main = "저감기술 발전",
     cex.axis = graph_cex.axis_opt, 
     cex.lab = graph_cex.lab_opt, 
     cex.main = graph_cex.main_opt)

# 곡선
#lines(x, supply_fun(x),  lwd = 3, col = "blue")
lines(x, demand_fun(x), lwd = 3, col = "darkgreen")

# 교차점 및 안내선: 가로 선
abline(h = 40,  lty = 3, col = "gray60")
mtext(expression(t), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = 40,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
      )      



## 교차점 계산
diff1 <- function(z) 40 - demand_fun(z) 

q_star  <- uniroot(diff1, xlim_all)$root
abline(v = q_star ,  lty = 3, col = "gray60")
mtext(expression(e[t]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = q_star,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   


## 그래프 안쪽에 곡선 제목 붙이기
text(50, 15, "MAC", col = "darkgreen", cex = 1.5)





#############################
##### 140 page ##### 그림 7-2
#############################

## ===== 공통 설정 =====
oldpar <- par(no.readonly = TRUE)

##### 그래프 바깥쪽 여백(margin) 조정정
graph_margin_opt <- c(
  5, # 아래쪽 여백
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
    xaxs = graph_xaxs_opt, yaxs = graph_yaxs_opt, xaxt= graph_xaxt_opt, yaxt= graph_yaxt_opt)


## x, y 범위
xlim_all <- c(0,100)
ylim_all <- c(0, 100)
x <- seq(xlim_all[1], xlim_all[2], length.out = 1000)


## 함수 정의 (function 형태로 정의해야 교차점 자동 계산 가능)

## 공급곡선: 우상향 ↗
supply_fun  <- function(z) 0.00005*z^3 + 0.04*z     

## 수요곡선: 우하향 ↘
demand_fun  <- function(z) 900/(z + 3) - 900/103


## 교차점 계산
diff1 <- function(z) supply_fun(z)  - demand_fun(z) 


q_star  <- uniroot(diff1, xlim_all)$root
y_star  <- supply_fun(q_star)


## 빈 캔버스
graph_cex.axis_opt <- 2.0 # 눈금(tick)의 숫자 크기 조절
graph_cex.lab_opt <- 2.0  # 축제목의 크기 조절
graph_cex.main_opt <- 2.0 # 그래프 제목의 글자 크기 조절절



plot(NA, xlim = xlim_all, ylim = ylim_all,
     xlab = "배출량", ylab = "금액",
     main = "최적 배출부과금",
     cex.axis = graph_cex.axis_opt, 
     cex.lab = graph_cex.lab_opt, 
     cex.main = graph_cex.main_opt)

# 곡선
lines(x, supply_fun(x),  lwd = 3, col = "blue")
lines(x, demand_fun(x), lwd = 3, col = "darkgreen")

# 교차점 및 안내선: 가로 선
abline(h = y_star,  lty = 3, col = "gray60")
mtext(expression(t^"*"), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = y_star,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)      



## 교차점 계산
#diff1 <- function(z) 40 - demand_fun(z) 

#q_star  <- uniroot(diff1, xlim_all)$root
abline(v = q_star ,  lty = 3, col = "gray60")
mtext(expression(e^"*"), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = q_star,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   



## 교차점 계산
#diff1 <- function(z) 40 - demand_fun(z) 

#q_star  <- uniroot(diff1, xlim_all)$root
abline(v = 36 ,  lty = 3, col = "gray60")
mtext(expression(e[1]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = 36,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   


mtext(expression(e[0]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = 100,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   

## 그래프 안쪽에 곡선 제목 붙이기
text(15,70 , "MAC", col = "darkgreen", cex = 1.5)
text(83,40 , "MD", col = "blue", cex = 1.5)






#############################
##### 142 page ##### 그림 7-3
#############################
## ===== 공통 설정 =====
oldpar <- par(no.readonly = TRUE)

##### 그래프 바깥쪽 여백(margin) 조정정
graph_margin_opt <- c(
  5, # 아래쪽 여백
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
    xaxs = graph_xaxs_opt, yaxs = graph_yaxs_opt, xaxt= graph_xaxt_opt, yaxt= graph_yaxt_opt)


## x, y 범위
xlim_all <- c(-100,100)
ylim_all <- c(0, 100)
#x <- seq(xlim_all[1], xlim_all[2], length.out = 1000)
x <- seq(0, 100, length.out = 1000)

## 함수 정의 (function 형태로 정의해야 교차점 자동 계산 가능)

## 공급곡선: 우상향 ↗
supply_fun  <- function(z) 0.00005*z^3 + 0.04*z     

## 수요곡선: 우하향 ↘
demand_fun  <- function(z) 900/(z + 3) - 900/103
demand_fun_a  <- function(z) 600/(z + 3) - 900/103


## 교차점 계산
diff1 <- function(z) supply_fun(z)  - demand_fun(z) 


q_star  <- uniroot(diff1, xlim_all)$root
y_star  <- supply_fun(q_star)


## 빈 캔버스
graph_cex.axis_opt <- 2.0 # 눈금(tick)의 숫자 크기 조절
graph_cex.lab_opt <- 2.0  # 축제목의 크기 조절
graph_cex.main_opt <- 2.0 # 그래프 제목의 글자 크기 조절절



plot(NA, xlim = xlim_all, ylim = ylim_all,
     xlab = "배출량", ylab = "금액",
     main = "배출부과금의 비용효과성",
     cex.axis = graph_cex.axis_opt, 
     cex.lab = graph_cex.lab_opt, 
     cex.main = graph_cex.main_opt)

# 곡선
lines(x, demand_fun(x), lwd = 3, col = "darkgreen")
lines(-x, demand_fun_a(x), lwd = 3, col = "darkgreen")

# 오른쪽 끝 x 절편
root_pos_right <- uniroot(function(z) demand_fun(z) - 0, c(0, 100))$root
mtext(expression(e[1]^"0"), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos_right,      # 텍스트 좌표 위치
      line = 2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   

# 가로선 1 (t1)
abline(h = demand_fun(20),  lty = 3, col = "gray60")
text(0, demand_fun(20), expression(t^1), col = "black", 
     cex = 2.0) # size

# 오른쪽 세로선 1 (t1)
abline(v = 20,  lty = 3, col = "gray60")

mtext(expression(e[1]^"1"), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = 20,      # 텍스트 좌표 위치
      line = 2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   


# 왼쪽 세로선 1 (t1)
root_pos <- uniroot(function(z) demand_fun_a(z) - demand_fun(20), c(0, 100))$root

abline(v = -root_pos,  lty = 3, col = "gray60")
mtext(expression(e[2]^"1"), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = -root_pos,      # 텍스트 좌표 위치
      line = 2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   


# 가로선 2 (t2)
abline(h = demand_fun(15),  lty = 3, col = "gray60")
text(0, demand_fun(15), expression(t^2), col = "black", 
     cex = 2.0) # size


# 왼쪽 끝 x 절편
root_pos_a <- uniroot(function(z) demand_fun_a(z) - 0, c(0, 100))$root
mtext(expression(e[2]^"0"), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = -root_pos_a,      # 텍스트 좌표 위치
      line = 2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   

# 영점
#root_pos_a <- uniroot(function(z) demand_fun_a(z) - 0, c(0, 100))$root
mtext(expression(0), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = 0,      # 텍스트 좌표 위치
      line = 1.5,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)  

# 진짜 y축
abline(v = 0, lty = 1, col = "gray70")   # 진짜 y축 보조선

## 그래프 안쪽에 곡선 제목 붙이기
text(30, 60, expression(MAC[1]), col = "darkgreen", cex = 1.5)
text(-30, 60, expression(MAC[2]), col = "darkgreen", cex = 1.5)






#############################
##### 146 page ##### 그림 7-5
#############################

## ===== 공통 설정 =====
oldpar <- par(no.readonly = TRUE)

##### 그래프 바깥쪽 여백(margin) 조정정
graph_margin_opt <- c(
  5, # 아래쪽 여백
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
    xaxs = graph_xaxs_opt, yaxs = graph_yaxs_opt, xaxt= graph_xaxt_opt, yaxt= graph_yaxt_opt)


## x, y 범위
xlim_all <- c(0,100)
ylim_all <- c(0, 100)
x <- seq(xlim_all[1], xlim_all[2], length.out = 1000)


## 함수 정의 (function 형태로 정의해야 교차점 자동 계산 가능)

  
## 수요곡선: 우하향 ↘
demand_fun  <- function(z) 2000/(z + 3) - 2000/103
demand_fun_a  <- function(z) 1000/(z + 3) - 1000/103

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
     main = "배출부과금제의 기술혁신 유인",
     cex.axis = graph_cex.axis_opt, 
     cex.lab = graph_cex.lab_opt, 
     cex.main = graph_cex.main_opt)

# 곡선
lines(x, demand_fun(x),  lwd = 3, col = "blue")
lines(x, demand_fun_a(x), lwd = 3, col = "blue")

# 교차점 및 안내선: 가로 선
abline(h = demand_fun(35),  lty = 3, col = "gray60")
mtext(expression(t), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = demand_fun(35),      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)      



## 교차점 계산
#diff1 <- function(z) 40 - demand_fun(z) 

#q_star  <- uniroot(diff1, xlim_all)$root
root_pos_a <- uniroot(function(z) demand_fun_a(z) - demand_fun(35), c(0, 100))$root
abline(v = root_pos_a ,  lty = 3, col = "gray60")
mtext(expression(e[2]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos_a,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   



## 교차점 계산
#diff1 <- function(z) 40 - demand_fun(z) 

#q_star  <- uniroot(diff1, xlim_all)$root

root_pos_b <- uniroot(function(z) demand_fun(z) - demand_fun(35), c(0, 100))$root

abline(v = root_pos_b ,  lty = 3, col = "gray60")
mtext(expression(e[1]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos_b,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   


# 영점
#root_pos_a <- uniroot(function(z) demand_fun_a(z) - 0, c(0, 100))$root
mtext(expression(0), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = 0,      # 텍스트 좌표 위치
      line = 1,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)  



text(38, 45, expression(MAC[1]), col = "black", cex = 1.5)
text(9, 45, expression(MAC[2]), col = "black", cex = 1.5)





#############################
##### 150 page ##### 그림 7-6
#############################

## ===== 공통 설정 =====
oldpar <- par(no.readonly = TRUE)

##### 그래프 바깥쪽 여백(margin) 조정정
graph_margin_opt <- c(
  5, # 아래쪽 여백
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
    xaxs = graph_xaxs_opt, yaxs = graph_yaxs_opt, xaxt= graph_xaxt_opt, yaxt= graph_yaxt_opt)


## x, y 범위
xlim_all <- c(0,100)
ylim_all <- c(0, 100)
x <- seq(xlim_all[1], xlim_all[2], length.out = 1000)


## 함수 정의 (function 형태로 정의해야 교차점 자동 계산 가능)


## 공급곡선: 우상향
demand_fun  <- function(z) 10 + 0.5*z
demand_fun_a  <- function(z) 30 + 0.5*z


## 교차점 계산
#diff1 <- function(z) supply_fun(z)  - demand_fun(z) 


#q_star  <- uniroot(diff1, xlim_all)$root
#y_star  <- supply_fun(q_star)


## 빈 캔버스
graph_cex.axis_opt <- 2.0 # 눈금(tick)의 숫자 크기 조절
graph_cex.lab_opt <- 2.0  # 축제목의 크기 조절
graph_cex.main_opt <- 2.0 # 그래프 제목의 글자 크기 조절절



plot(NA, xlim = xlim_all, ylim = ylim_all,
     xlab = "생산량", ylab = "금액",
     main = "오염저감 유도를 위한 상품세",
     cex.axis = graph_cex.axis_opt, 
     cex.lab = graph_cex.lab_opt, 
     cex.main = graph_cex.main_opt)

# 곡선
lines(x, demand_fun(x),  lwd = 3, col = "blue")
lines(x, demand_fun_a(x), lwd = 3, col = "blue")

# 교차점 및 안내선: 가로 선
abline(h = demand_fun(70),  lty = 1, col = "blue")
# mtext(expression(t), 
#       side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
#       at = demand_fun(70),      # 텍스트 좌표 위치
#       line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
#       cex = 2,      # 글자 크기
#       las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
# )      



## 교차점 계산
diff1 <- function(z) demand_fun(z) - demand_fun(70)

#q_star  <- uniroot(diff1, xlim_all)$root
root_pos <- uniroot(diff1, c(0, 100))$root
abline(v = root_pos ,  lty = 3, col = "gray60")
mtext(expression(Q[P]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   



## 교차점 계산
diff2 <- function(z) demand_fun_a(z) - demand_fun(70)

root_pos_b <- uniroot(diff2, c(0, 100))$root

abline(v = root_pos_b ,  lty = 3, col = "gray60")
mtext(expression(Q[S]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos_b,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   




text(80, 75, expression(MC[S]), col = "black", cex = 1.5)
text(80, 55, expression(MC[P]), col = "black", cex = 1.5)
text(80, 42, expression('D=MB'), col = "black", cex = 1.5)




#############################
##### 153 page ##### 그림 7-7
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
supply_fun  <- function(z) 10 + 0.6*z
demand_fun  <- function(z) 90 - 1.1*z


## 교차점 계산
#diff1 <- function(z) supply_fun(z)  - demand_fun(z) 


#q_star  <- uniroot(diff1, xlim_all)$root
#y_star  <- supply_fun(q_star)


## 빈 캔버스
graph_cex.axis_opt <- 2.0 # 눈금(tick)의 숫자 크기 조절
graph_cex.lab_opt <- 2.0  # 축제목의 크기 조절
graph_cex.main_opt <- 2.0 # 그래프 제목의 글자 크기 조절절



plot(NA, xlim = xlim_all, ylim = ylim_all,
     xlab = "버려지는 병의 수", ylab = "금액",
     main = "폐기물 예치금제도",
     cex.axis = graph_cex.axis_opt, 
     cex.lab = graph_cex.lab_opt, 
     cex.main = graph_cex.main_opt)

# 곡선
lines(x, supply_fun(x),  lwd = 3, col = "blue")
lines(x, demand_fun(x), lwd = 3, col = "blue")


## 교차점 계산
diff1 <- function(z) supply_fun(z) - demand_fun(z)

#q_star  <- uniroot(diff1, xlim_all)$root
root_pos <- uniroot(diff1, c(0, 100))$root
abline(h = supply_fun(root_pos) ,  lty = 3, col = "gray60")



## 교차점 계산
diff2 <- function(z) demand_fun(z) - 0
root_pos_a <- uniroot(diff2, c(0, 100))$root

mtext(expression(Q[S]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   

mtext(expression(100), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = 100,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   


mtext(expression(Q[P]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos_a,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   




text(80, 65, expression(MC[S]), col = "black", cex = 1.5)
text(10, 43, expression(MC[P]^1), col = "black", cex = 1.5)
text(70, 20, expression(MB), col = "black", cex = 1.5)





#############################
##### 155 page ##### 그림 7-8
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
demand_fun_a  <- function(z) 70 - 0.7*z



## 교차점 계산
#diff1 <- function(z) supply_fun(z)  - demand_fun(z) 


#q_star  <- uniroot(diff1, xlim_all)$root
#y_star  <- supply_fun(q_star)


## 빈 캔버스
graph_cex.axis_opt <- 2.0 # 눈금(tick)의 숫자 크기 조절
graph_cex.lab_opt <- 2.0  # 축제목의 크기 조절
graph_cex.main_opt <- 2.0 # 그래프 제목의 글자 크기 조절절



plot(NA, xlim = xlim_all, ylim = ylim_all,
     xlab = "정화시설 규모", ylab = "금액",
     main = "저감시설에 대한 보조금",
     cex.axis = graph_cex.axis_opt, 
     cex.lab = graph_cex.lab_opt, 
     cex.main = graph_cex.main_opt)

# 곡선
lines(x, demand_fun(x),  lwd = 3, col = "blue")
lines(x, demand_fun_a(x), lwd = 3, col = "blue")



## 교차점 계산
diff1 <- function(z) demand_fun(z) - 50

#q_star  <- uniroot(diff1, xlim_all)$root
root_pos <- uniroot(diff1, c(0, 100))$root
abline(h = 50 ,  lty = 1, col = "blue", lwd = 2.5)

abline(v = (root_pos) ,  lty = 3, col = "gray60")
mtext(expression(Q[S]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   



## 교차점 계산
diff2 <- function(z) demand_fun_a(z) - 50
root_pos_a <- uniroot(diff2, c(0, 100))$root

abline(v = (root_pos_a) ,  lty = 3, col = "gray60")
mtext(expression(Q[P]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos_a,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)

abline(h = demand_fun_a(root_pos) ,  lty = 6, col = "black", lwd = 2.5)



mtext(expression(P[0]), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = 50,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   

mtext(expression(P[1]), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = demand_fun_a(root_pos),      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   



text(root_pos, demand_fun(root_pos)+3, expression(a), col = "black", cex = 1.5)
text(root_pos, demand_fun_a(root_pos)+3, expression(b), col = "black", cex = 1.5)


text(20, 60, expression(MB[P]), col = "black", cex = 1.5)
text(20, 80, expression(MB[S]), col = "black", cex = 1.5)
text(80, 53, expression(MC), col = "black", cex = 1.5)




#############################
##### 157 page ##### 그림 7-9
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
supply_fun  <- function(z) 0.00005*z^3 + 0.04*z  
demand_fun  <- function(z) 900/(z + 3) - 900/103



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
     main = "저감 보조금",
     cex.axis = graph_cex.axis_opt, 
     cex.lab = graph_cex.lab_opt, 
     cex.main = graph_cex.main_opt)

# 곡선
lines(x, supply_fun(x),  lwd = 3, col = "blue")
lines(x, demand_fun(x), lwd = 3, col = "blue")



## 교차점 계산
diff1 <- function(z) demand_fun(z) - supply_fun(z)

#q_star  <- uniroot(diff1, xlim_all)$root
root_pos <- uniroot(diff1, c(0, 100))$root
abline(h = supply_fun(root_pos) ,  lty = 3, col = "gray60", lwd = 2.5)
abline(v = root_pos ,  lty = 3, col = "gray60", lwd = 2.5)

#abline(v = (root_pos) ,  lty = 3, col = "gray60")
mtext(expression(e^"*"), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   


mtext(expression(a), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = demand_fun(root_pos),      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   

mtext(expression(e[0]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = 100,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   

text(18, 60, expression(MAC), col = "black", cex = 1.5)
text(75, 35, expression(MD), col = "black", cex = 1.5)




#############################
##### 161 page ##### 그림 7-10
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
     main = "조세의 초과부담",
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
mtext(expression(Q^"*"), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   



## 교차점 계산
diff2 <- function(z) demand_fun(z) - 70
root_pos_a <- uniroot(diff2, c(0, 100))$root

abline(h = 70 ,  lty = 3, col = "black", lwd = 2.5)
abline(v = (root_pos_a) ,  lty = 3, col = "gray60")
mtext(expression(Q[1]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos_a,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)

#abline(h = demand_fun(root_pos) ,  lty = 6, col = "black", lwd = 2.5)



mtext(expression(P^"*"), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = 50,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   

mtext(expression(P[D]), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = 70,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   



text(80, demand_fun(root_pos)+3, expression(S(시장공급)), col = "black", cex = 1.5)
text(20, demand_fun(root_pos_a)+15, expression(D(시장수요)), col = "black", cex = 1.5)




#############################
##### 169 page ##### 그림 7-11
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
demand_fun  <- function(z) 90 - 1.4*z
demand_fun_a  <- function(z) 90 - 0.7*z



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
     main = "독점기업의 오염규제",
     cex.axis = graph_cex.axis_opt, 
     cex.lab = graph_cex.lab_opt, 
     cex.main = graph_cex.main_opt)

# 곡선
lines(x, demand_fun(x),  lwd = 3, col = "blue")
lines(x, demand_fun_a(x), lwd = 3, col = "blue")



## 교차점 계산
diff1 <- function(z) demand_fun(z) - 20

#q_star  <- uniroot(diff1, xlim_all)$root
root_pos <- uniroot(diff1, c(0, 100))$root
abline(h = 20 ,  lty = 1, col = "blue", lwd = 2.5)



abline(v = (root_pos) ,  lty = 3, col = "gray60")
mtext(expression(Q^"*"), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   




## 교차점 계산
diff2 <- function(z) demand_fun(z) - 40

#q_star  <- uniroot(diff1, xlim_all)$root
root_pos_a <- uniroot(diff2, c(0, 100))$root
abline(h = 40 ,  lty = 1, col = "blue", lwd = 2.5)
mtext(expression(P[C]), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = 40,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   

abline(v = (root_pos_a) ,  lty = 3, col = "gray60")
mtext(expression(Q[M]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos_a,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   



# 
# ## 교차점 계산
# diff3 <- function(z) demand_fun(z) - 20
# 
# #q_star  <- uniroot(diff1, xlim_all)$root
# root_pos_b <- uniroot(diff3, c(0, 100))$root
abline(h = demand_fun_a(50) ,  lty = 1, col = "blue", lwd = 2.5)
mtext(expression(P^"*"), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = demand_fun_a(50),      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)  




## 교차점 계산
diff4 <- function(z) demand_fun(z) - demand_fun_a(root_pos)
root_pos_c <- uniroot(diff4, c(0, 100))$root

abline(v = (root_pos_c) ,  lty = 3, col = "gray60")

mtext(expression(Q[1]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos_c,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   



abline(h = demand_fun_a(root_pos_c) ,  lty = 3, col = "gray60")
mtext(expression(P[1]), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = demand_fun_a(root_pos_c),      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)  

abline(h = demand_fun_a(root_pos_a) ,  lty = 3, col = "gray60")
mtext(expression(P[M]), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = demand_fun_a(root_pos_a) ,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)  





## 교차점 계산
diff5 <- function(z) demand_fun_a(z) - 40
root_pos_d <- uniroot(diff5, c(0, 100))$root

abline(v = (root_pos_d) ,  lty = 3, col = "gray60")
mtext(expression(Q[C]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos_d,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   



## 교차점 계산  MR 1
demand_fun_c  <- function(z) demand_fun_a(50) - demand_fun(50) + 90 - 1.4*z

lines(x, demand_fun_c(x),  lty = 3, lwd = 2, col = "blue")


##############
  












## 교차점 계산
diff2 <- function(z) demand_fun_a(z) - 50
root_pos_a <- uniroot(diff2, c(0, 100))$root

abline(v = (root_pos_a) ,  lty = 3, col = "gray60")
mtext(expression(Q[P]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos_a,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)

abline(h = demand_fun_a(root_pos) ,  lty = 6, col = "black", lwd = 2.5)



mtext(expression(P[0]), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = 50,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   

mtext(expression(P[1]), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = demand_fun_a(root_pos),      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   



text(root_pos, demand_fun(root_pos)+3, expression(a), col = "black", cex = 1.5)
text(root_pos, demand_fun_a(root_pos)+3, expression(b), col = "black", cex = 1.5)


text(20, 60, expression(MB[P]), col = "black", cex = 1.5)
text(20, 80, expression(MB[S]), col = "black", cex = 1.5)
text(80, 53, expression(MC), col = "black", cex = 1.5)

