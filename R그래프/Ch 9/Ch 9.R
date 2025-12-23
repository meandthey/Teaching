
#############################
##### 218 page ##### 그림 9-1
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
demand_fun_a  <- function(z) 60 - 0.7*z

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
     xlab = "저감량", ylab = "금액",
     main = "한계편익이 불확실한 경우",
     cex.axis = graph_cex.axis_opt, 
     cex.lab = graph_cex.lab_opt, 
     cex.main = graph_cex.main_opt)

# 곡선
lines(x, demand_fun(x),  lwd = 3, col = "blue")
lines(x, demand_fun_a(x), lwd = 3, col = "blue")
lines(x, supply_fun(x), lwd = 3, col = "blue")



## 교차점 계산
diff1 <- function(z) demand_fun(z) - supply_fun(z)
diff2 <- function(z) demand_fun_a(z) - supply_fun(z)

#q_star  <- uniroot(diff1, xlim_all)$root
root_pos <- uniroot(diff1, c(0, 100))$root
abline(h = root_pos ,  lty = 2, col = "gray60", lwd = 0.1)
abline(v = supply_fun(root_pos) ,  lty = 2, col = "gray60", lwd = 0.1)

root_pos2 <- uniroot(diff2, c(0, 100))$root
abline(h = root_pos2 ,  lty = 2, col = "gray60", lwd = 0.1)
abline(v = supply_fun(root_pos2) ,  lty = 2, col = "gray60", lwd = 0.1)




mtext(expression(a[A]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   


mtext(expression(a[E]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos2,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   


mtext(expression(t[A]), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = supply_fun(root_pos),      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   

mtext(expression(t[E]), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = supply_fun(root_pos2),      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   




#############################
##### 219 page ##### 그림 9-2
#############################

## ===== 공통 설정 =====
oldpar <- par(no.readonly = TRUE)

##### 그래프 바깥쪽 여백(margin) 조정정
graph_margin_opt <- c(
  5, # 아래쪽 여백
  5, # 왼쪽 여백
  5, # 위쪽 여백
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
demand_fun  <- function(z) 70 - 0.3*z

supply_fun <- function(z) -10 + 1*z
supply_fun_a <- function(z) 20 + 1*z


## 교차점 계산
#diff1 <- function(z) supply_fun(z)  - demand_fun(z) 


#q_star  <- uniroot(diff1, xlim_all)$root
#y_star  <- supply_fun(q_star)


## 빈 캔버스
graph_cex.axis_opt <- 2.0 # 눈금(tick)의 숫자 크기 조절
graph_cex.lab_opt <- 2.0  # 축제목의 크기 조절
graph_cex.main_opt <- 2.0 # 그래프 제목의 글자 크기 조절절



plot(NA, xlim = xlim_all, ylim = ylim_all,
     xlab = "저감량", ylab = "금액",
     main = "한계저감비용이 불확실한 경우\n (한계저감비용이 더 가파른 경우)",
     cex.axis = graph_cex.axis_opt, 
     cex.lab = graph_cex.lab_opt, 
     cex.main = graph_cex.main_opt)

# 곡선
lines(x, demand_fun(x),  lwd = 3, col = "blue")
lines(x, supply_fun(x), lwd = 3, col = "blue")
lines(x, supply_fun_a(x), lwd = 3, col = "blue")



## 교차점 계산
diff1 <- function(z) demand_fun(z) - supply_fun(z)
diff2 <- function(z) demand_fun(z) - supply_fun_a(z)

#q_star  <- uniroot(diff1, xlim_all)$root
root_pos <- uniroot(diff1, c(0, 100))$root
abline(h = demand_fun(root_pos) ,  lty = 2, col = "gray60", lwd = 0.1)
abline(v = (root_pos) ,  lty = 2, col = "gray60", lwd = 0.1)

root_pos2 <- uniroot(diff2, c(0, 100))$root
abline(h = demand_fun(root_pos2) ,  lty = 2, col = "gray60", lwd = 0.1)
abline(v = (root_pos2) ,  lty = 2, col = "gray60", lwd = 0.1)

## 교차점 (추가)
diff3 <- function(z) supply_fun_a(z) - demand_fun(root_pos)

root_pos3 <-  uniroot(diff3, c(0, 100))$root
abline(v = (root_pos3) ,  lty = 2, col = "gray60", lwd = 0.1)


mtext(expression(a[1]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   


mtext(expression(a[A]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos2,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   

mtext(expression(a[2]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos3,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   



mtext(expression(t[E]), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = demand_fun(root_pos),      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   

mtext(expression(t[A]), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = demand_fun(root_pos2),      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   


#############################
##### 221 page ##### 그림 9-3
#############################

## ===== 공통 설정 =====
oldpar <- par(no.readonly = TRUE)

##### 그래프 바깥쪽 여백(margin) 조정정
graph_margin_opt <- c(
  5, # 아래쪽 여백
  5, # 왼쪽 여백
  5, # 위쪽 여백
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
demand_fun  <- function(z) 135 - 1.5*z

supply_fun <- function(z) 5 + 0.5*z
supply_fun_a <- function(z) 25 + 0.5*z


## 교차점 계산
#diff1 <- function(z) supply_fun(z)  - demand_fun(z) 


#q_star  <- uniroot(diff1, xlim_all)$root
#y_star  <- supply_fun(q_star)


## 빈 캔버스
graph_cex.axis_opt <- 2.0 # 눈금(tick)의 숫자 크기 조절
graph_cex.lab_opt <- 2.0  # 축제목의 크기 조절
graph_cex.main_opt <- 2.0 # 그래프 제목의 글자 크기 조절절



plot(NA, xlim = xlim_all, ylim = ylim_all,
     xlab = "저감량", ylab = "금액",
     main = "한계저감비용이 불확실한 경우\n (한계편익곡선이 더 가파른 경우)",
     cex.axis = graph_cex.axis_opt, 
     cex.lab = graph_cex.lab_opt, 
     cex.main = graph_cex.main_opt)

# 곡선
lines(x, demand_fun(x),  lwd = 3, col = "blue")
lines(x, supply_fun(x), lwd = 3, col = "blue")
lines(x, supply_fun_a(x), lwd = 3, col = "blue")



## 교차점 계산
diff1 <- function(z) demand_fun(z) - supply_fun(z)
diff2 <- function(z) demand_fun(z) - supply_fun_a(z)

#q_star  <- uniroot(diff1, xlim_all)$root
root_pos <- uniroot(diff1, c(0, 100))$root
abline(h = demand_fun(root_pos) ,  lty = 2, col = "gray60", lwd = 0.1)
abline(v = (root_pos) ,  lty = 2, col = "gray60", lwd = 0.1)

root_pos2 <- uniroot(diff2, c(0, 100))$root
abline(h = demand_fun(root_pos2) ,  lty = 2, col = "gray60", lwd = 0.1)
abline(v = (root_pos2) ,  lty = 2, col = "gray60", lwd = 0.1)

## 교차점 (추가)
diff3 <- function(z) supply_fun_a(z) - demand_fun(root_pos)

root_pos3 <-  uniroot(diff3, c(0, 100))$root
abline(v = (root_pos3) ,  lty = 2, col = "gray60", lwd = 0.1)


mtext(expression(a[1]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   


mtext(expression(a[A]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos2,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   

mtext(expression(a[2]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos3,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   



mtext(expression(t[E]), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = demand_fun(root_pos),      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   

mtext(expression(t[A]), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = demand_fun(root_pos2),      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   




#############################
##### 225 page ##### 그림 9-4
#############################

## ===== 공통 설정 =====
oldpar <- par(no.readonly = TRUE)

##### 그래프 바깥쪽 여백(margin) 조정정
graph_margin_opt <- c(
  5, # 아래쪽 여백
  5, # 왼쪽 여백
  3, # 위쪽 여백
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
demand_fun  <- function(z) 95 - 1*z

supply_fun <- function(z) 5 + 0.5*z
supply_fun_a <- function(z) 25 + 0.5*z
supply_fun_b <- function(z) 45 + 0.5*z


## 교차점 계산
#diff1 <- function(z) supply_fun(z)  - demand_fun(z) 


#q_star  <- uniroot(diff1, xlim_all)$root
#y_star  <- supply_fun(q_star)


## 빈 캔버스
graph_cex.axis_opt <- 2.0 # 눈금(tick)의 숫자 크기 조절
graph_cex.lab_opt <- 2.0  # 축제목의 크기 조절
graph_cex.main_opt <- 2.0 # 그래프 제목의 글자 크기 조절절



plot(NA, xlim = xlim_all, ylim = ylim_all,
     xlab = "저감량", ylab = "금액",
     main = "혼합정책이 사용될 경우",
     cex.axis = graph_cex.axis_opt, 
     cex.lab = graph_cex.lab_opt, 
     cex.main = graph_cex.main_opt)

# 곡선
lines(x, demand_fun(x),  lwd = 2, col = "blue")
lines(x, supply_fun(x), lwd = 2, col = "blue")
lines(x, supply_fun_a(x), lwd = 2, col = "blue")
lines(x, supply_fun_b(x), lwd = 2, col = "blue")



## 교차점 계산
diff1 <- function(z) demand_fun(z) - supply_fun(z)
diff2 <- function(z) demand_fun(z) - supply_fun_a(z)
diff3 <- function(z) demand_fun(z) - supply_fun_b(z)

root_pos1 <- uniroot(diff1, c(0, 100))$root
root_pos2 <- uniroot(diff2, c(0, 100))$root
root_pos3 <- uniroot(diff3, c(0, 100))$root


abline(h = demand_fun(root_pos2) ,  lty = 2, col = "gray60", lwd = 0.1)
abline(h = demand_fun(root_pos1) ,  lty = 2, col = "gray60", lwd = 0.1)

abline(v = (root_pos1) ,  lty = 2, col = "gray60", lwd = 0.1)
abline(v = (root_pos2) ,  lty = 2, col = "gray60", lwd = 0.1)
abline(v = (root_pos3) ,  lty = 2, col = "gray60", lwd = 0.1)

segments(x0 = 0, y0 = demand_fun(root_pos3),
         x1 = root_pos2, y1 = demand_fun(root_pos3), lwd = 3)

segments(x0 = root_pos2, y0 = demand_fun(root_pos1),
         x1 = root_pos2, y1 =  demand_fun(root_pos3), lwd = 3)

segments(x0 = root_pos2, y0 = demand_fun(root_pos1),
         x1 = 100, y1 =  demand_fun(root_pos1), lwd = 3)



mtext(expression(a[2]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos1,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   


mtext(expression(bar(a)), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos2,      # 텍스트 좌표 위치
      line = 1,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   

mtext(expression(a[1]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos3,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   



mtext(expression(s), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = demand_fun(root_pos1),      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   

mtext(expression(bar(t)), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = demand_fun(root_pos2),      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   

mtext(expression(t), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = demand_fun(root_pos3),      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   



#############################
##### 228 page ##### 그림 9-5
#############################

## ===== 공통 설정 =====
oldpar <- par(no.readonly = TRUE)

##### 그래프 바깥쪽 여백(margin) 조정정
graph_margin_opt <- c(
  5, # 아래쪽 여백
  5, # 왼쪽 여백
  3, # 위쪽 여백
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
xlim_all <- c(0,73)
ylim_all <- c(0, 73)
x <- seq(xlim_all[1], xlim_all[2], length.out = 1000)


## 함수 정의 (function 형태로 정의해야 교차점 자동 계산 가능)


## 공급곡선: 우상향
demand_fun_a  <- function(z) 60 - 1*z
demand_fun_b  <- function(z) 40 - 1*z

supply_fun <- function(z) 0 + 0.75*z

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
     main = "배출권거래제에서의 오염원의 전략적 행위",
     cex.axis = graph_cex.axis_opt, 
     cex.lab = graph_cex.lab_opt, 
     cex.main = graph_cex.main_opt)

# 곡선
lines(x, demand_fun_a(x),  lwd = 2, col = "blue")
lines(x, demand_fun_b(x), lwd = 2, col = "blue")
lines(x, supply_fun(x), lwd = 2, col = "blue")




## 교차점 계산
diff1 <- function(z) demand_fun_a(z) - supply_fun(z)
diff2 <- function(z) demand_fun_b(z) - supply_fun(z)



root_pos1 <- uniroot(diff1, c(0, 100))$root
root_pos2 <- uniroot(diff2, c(0, 100))$root



abline(h = demand_fun_b(root_pos2) ,  lty = 2, col = "gray60", lwd = 0.1)


abline(v = (root_pos1) ,  lty = 2, col = "gray60", lwd = 0.1)
abline(v = (root_pos2) ,  lty = 2, col = "gray60", lwd = 0.1)

abline(h = demand_fun_b(root_pos1) ,  lty = 2, col = "gray60", lwd = 0.1)



mtext(expression(e[0]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = uniroot(demand_fun_b, c(0, 100))$root,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   

mtext(expression(e[1]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos1,      # 텍스트 좌표 위치
      line = 1,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   

mtext(expression(e[A]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos2,      # 텍스트 좌표 위치
      line = 1,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   

mtext(expression(P[1]), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = demand_fun_b(root_pos1),      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   


mtext(expression(P[A]), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = demand_fun_b(root_pos2),      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   

#############################
##### 229 page ##### 그림 9-6
#############################

## ===== 공통 설정 =====
oldpar <- par(no.readonly = TRUE)

##### 그래프 바깥쪽 여백(margin) 조정정
graph_margin_opt <- c(
  5, # 아래쪽 여백
  5, # 왼쪽 여백
  3, # 위쪽 여백
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
xlim_all <- c(0,73)
ylim_all <- c(0, 73)
x <- seq(xlim_all[1], xlim_all[2], length.out = 1000)


## 함수 정의 (function 형태로 정의해야 교차점 자동 계산 가능)


## 공급곡선: 우상향
demand_fun_a  <- function(z) 60 - 1*z
demand_fun_b  <- function(z) 40 - 1*z

supply_fun <- function(z) 0 + 0.75*z


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
     main = "배출부과금제에서의 오염원의 전략적 행위",
     cex.axis = graph_cex.axis_opt, 
     cex.lab = graph_cex.lab_opt, 
     cex.main = graph_cex.main_opt)

# 곡선
lines(x, demand_fun_a(x),  lwd = 2, col = "blue")
lines(x, demand_fun_b(x), lwd = 2, col = "blue")
lines(x, supply_fun(x), lwd = 2, col = "blue")




## 교차점 계산
diff1 <- function(z) demand_fun_a(z) - supply_fun(z)
diff2 <- function(z) demand_fun_b(z) - supply_fun(z)



root_pos1 <- uniroot(diff1, c(0, 100))$root
root_pos2 <- uniroot(diff2, c(0, 100))$root


abline(h = supply_fun(root_pos1) ,  lty = 2, col = "gray60", lwd = 0.1)
abline(h = supply_fun(root_pos2) ,  lty = 2, col = "gray60", lwd = 0.1)

abline(v = (root_pos1) ,  lty = 2, col = "gray60", lwd = 0.1)


diff3 <- function(z) demand_fun_a(z) - supply_fun(root_pos2)
root_pos3 <- uniroot(diff3, c(0, 100))$root
abline(v = root_pos3 ,  lty = 2, col = "gray60", lwd = 0.1)



mtext(expression(e[0]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = uniroot(demand_fun_a, c(0, 100))$root,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   

mtext(expression(e[1]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos3,      # 텍스트 좌표 위치
      line = 1,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   

mtext(expression(e[A]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos1,      # 텍스트 좌표 위치
      line = 1,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   

mtext(expression(t[A]), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = demand_fun_a(root_pos1),      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   


mtext(expression(t[1]), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = demand_fun_b(root_pos2),      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   




#############################
##### 230 page ##### 그림 9-7
#############################

## ===== 공통 설정 =====
oldpar <- par(no.readonly = TRUE)

##### 그래프 바깥쪽 여백(margin) 조정정
graph_margin_opt <- c(
  5, # 아래쪽 여백
  5, # 왼쪽 여백
  3, # 위쪽 여백
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
xlim_all <- c(0,73)
ylim_all <- c(0, 73)
x <- seq(xlim_all[1], xlim_all[2], length.out = 1000)


## 함수 정의 (function 형태로 정의해야 교차점 자동 계산 가능)


## 공급곡선: 우상향
demand_fun_a  <- function(z) 60 - 1*z
demand_fun_b  <- function(z) 40 - 1*z

supply_fun <- function(z) 0 + 0.75*z


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
     main = "퀴렐 메커니즘",
     cex.axis = graph_cex.axis_opt, 
     cex.lab = graph_cex.lab_opt, 
     cex.main = graph_cex.main_opt)

# 곡선
lines(x, demand_fun_a(x),  lwd = 2, col = "blue")
lines(x, demand_fun_b(x), lwd = 2, col = "blue")
lines(x, supply_fun(x), lwd = 2, col = "blue")




## 교차점 계산
diff1 <- function(z) demand_fun_a(z) - supply_fun(z)
diff2 <- function(z) demand_fun_b(z) - supply_fun(z)



root_pos1 <- uniroot(diff1, c(0, 100))$root
root_pos2 <- uniroot(diff2, c(0, 100))$root


abline(h = supply_fun(root_pos1) ,  lty = 2, col = "gray60", lwd = 0.1)
abline(h = supply_fun(root_pos2) ,  lty = 2, col = "gray60", lwd = 0.1)

abline(v = (root_pos1) ,  lty = 2, col = "gray60", lwd = 0.1)
abline(v = (root_pos2) ,  lty = 2, col = "gray60", lwd = 0.1)


diff3 <- function(z) demand_fun_b(z) - supply_fun(root_pos1)
root_pos3 <- uniroot(diff3, c(0, 100))$root
abline(v = root_pos3 ,  lty = 2, col = "gray60", lwd = 0.1)



mtext(expression(e[0]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = uniroot(demand_fun_b, c(0, 100))$root,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   

mtext(expression(e[1]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos1,      # 텍스트 좌표 위치
      line = 1,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   

mtext(expression(e[A]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos2,      # 텍스트 좌표 위치
      line = 1,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   

mtext(expression(e[2]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos3,      # 텍스트 좌표 위치
      line = 1,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   


mtext(expression(P[A]), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = demand_fun_b(root_pos2),      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   


mtext(expression(s), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = demand_fun_a(root_pos1),      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   




#############################
##### 233 page ##### 그림 9-8 (1)
#############################

## ===== 공통 설정 =====
oldpar <- par(no.readonly = TRUE)

##### 그래프 바깥쪽 여백(margin) 조정정
graph_margin_opt <- c(
  5, # 아래쪽 여백
  5, # 왼쪽 여백
  3, # 위쪽 여백
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
xlim_all <- c(0,65)
ylim_all <- c(0,65)
x <- seq(xlim_all[1], xlim_all[2], length.out = 1000)


## 함수 정의 (function 형태로 정의해야 교차점 자동 계산 가능)


## 공급곡선: 우상향
demand_fun_a  <- function(z) 60 - 1*z
demand_fun_b  <- function(z) 40 - 1*z

supply_fun <- function(z) 0 + 0.75*z


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
     main = "몬테로 메커니즘",
     cex.axis = graph_cex.axis_opt, 
     cex.lab = graph_cex.lab_opt, 
     cex.main = graph_cex.main_opt)

# 곡선
lines(x, demand_fun_a(x),  lwd = 2, col = "blue")
lines(x, demand_fun_b(x), lwd = 2, col = "blue")
lines(x, supply_fun(x), lwd = 2, col = "blue")




## 교차점 계산
diff1 <- function(z) demand_fun_a(z) - supply_fun(z)
diff2 <- function(z) demand_fun_b(z) - supply_fun(z)



root_pos1 <- uniroot(diff1, c(0, 100))$root
root_pos2 <- uniroot(diff2, c(0, 100))$root


abline(h = supply_fun(root_pos1) ,  lty = 2, col = "gray60", lwd = 0.1)
abline(h = supply_fun(root_pos2) ,  lty = 2, col = "gray60", lwd = 0.1)

abline(v = (root_pos1) ,  lty = 2, col = "gray60", lwd = 0.1)
abline(v = (root_pos2) ,  lty = 2, col = "gray60", lwd = 0.1)



mtext(expression(e[0]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = uniroot(demand_fun_a, c(0, 100))$root,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   

mtext(expression(e[A]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos1,      # 텍스트 좌표 위치
      line = 1,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   

mtext(expression(e[1]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos2,      # 텍스트 좌표 위치
      line = 1,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   

 


mtext(expression(P[1]), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = supply_fun(root_pos2),      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   


mtext(expression(P[A]), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = supply_fun(root_pos1),      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   



#############################
##### 233 page ##### 그림 9-8 (2)
#############################

## ===== 공통 설정 =====
oldpar <- par(no.readonly = TRUE)

##### 그래프 바깥쪽 여백(margin) 조정정
graph_margin_opt <- c(
  5, # 아래쪽 여백
  5, # 왼쪽 여백
  3, # 위쪽 여백
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
xlim_all <- c(0,65)
ylim_all <- c(0,65)
x <- seq(xlim_all[1], xlim_all[2], length.out = 1000)


## 함수 정의 (function 형태로 정의해야 교차점 자동 계산 가능)


## 공급곡선: 우상향
demand_fun_a  <- function(z) 60 - 1*z
demand_fun_b  <- function(z) 40 - 1*z

supply_fun <- function(z) 0 + 0.75*z


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
     main = "몬테로 메커니즘",
     cex.axis = graph_cex.axis_opt, 
     cex.lab = graph_cex.lab_opt, 
     cex.main = graph_cex.main_opt)

# 곡선
lines(x, demand_fun_a(x),  lwd = 2, col = "blue")
lines(x, demand_fun_b(x), lwd = 2, col = "blue")
lines(x, supply_fun(x), lwd = 2, col = "blue")




## 교차점 계산
diff1 <- function(z) demand_fun_a(z) - supply_fun(z)
diff2 <- function(z) demand_fun_b(z) - supply_fun(z)



root_pos1 <- uniroot(diff1, c(0, 100))$root
root_pos2 <- uniroot(diff2, c(0, 100))$root


abline(h = supply_fun(root_pos1) ,  lty = 2, col = "gray60", lwd = 0.1)
abline(h = supply_fun(root_pos2) ,  lty = 2, col = "gray60", lwd = 0.1)

abline(v = (root_pos1) ,  lty = 2, col = "gray60", lwd = 0.1)
abline(v = (root_pos2) ,  lty = 2, col = "gray60", lwd = 0.1)



mtext(expression(e[0]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = uniroot(demand_fun_b, c(0, 100))$root,      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   

mtext(expression(e[1]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos1,      # 텍스트 좌표 위치
      line = 1,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   

mtext(expression(e[A]), 
      side = 1,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = root_pos2,      # 텍스트 좌표 위치
      line = 1,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   




mtext(expression(P[A]), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = supply_fun(root_pos2),      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   


mtext(expression(P[1]), 
      side = 2,     # 1:x축, 2:y축, 3:위쪽, 4:오른쪽
      at = supply_fun(root_pos1),      # 텍스트 좌표 위치
      line = 1.2,   # 축에서 가까운 정도 (숫자가 커지면 축선에서 멀어짐)
      cex = 2,      # 글자 크기
      las = 1       # 0: 축과평행,  1:수평으로 (똑바르게),  2:축과 수직,  3:항상 수직 
)   




#####
library(ggplot2)
library(dplyr)
library(tidyr)

# ----- 파라미터 -----
q_star <- 0                 # 중앙 q*
gap    <- 1.2               # 두 분포의 중심 간 거리
mu_L   <- q_star - gap      # 저감행위 할 경우 평균
mu_R   <- q_star + gap      # 저감행위 안 할 경우 평균
sigma  <- 0.6               # 표준편차

# ----- 데이터 생성 -----
x <- seq(q_star - 4*gap, q_star + 4*gap, length.out = 1000)
df <- tibble(
  x = x,
  pdf_L = dnorm(x, mean = mu_L, sd = sigma),
  pdf_R = dnorm(x, mean = mu_R, sd = sigma)
)

df_long <- df %>%
  pivot_longer(cols = c(pdf_L, pdf_R),
               names_to = "case", values_to = "pdf") %>%
  mutate(case = ifelse(case == "pdf_L",
                       "저감행위를 할 경우의 오염도 발생 확률",
                       "저감행위를 안 할 경우의 오염도 발생 확률"))

# ----- 그래프 -----
p <- ggplot(df_long, aes(x = x, y = pdf, color = case)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = q_star, linewidth = 0.8, color = "black") +  # q*
  geom_vline(xintercept = c(mu_L, mu_R), linetype = "dashed", color = "gray50") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_color_manual(values = c("#1B4F72", "#922B21")) +
  labs(x = "", y = "확률") +
  theme_minimal(base_size = 20) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )

# ----- 상단 및 q* 표시 -----
p 

