#генератор количества

FILE_SUPPLY <- 'in'
FILE_SALE <- 'out'

h <- list(
  list(name = 'Молоко, уп.', min = 100, max = 110, price = 60),
  list(name = 'Масло, уп.', min = 50, max = 60, price = 100),
  list(name = 'Хлеб, шт.', min = 50, max = 70, price = 40),
  list(name = 'Вода, бут.', min = 50, max = 60, price = 35),
  list(name = 'Печенье, пач.', min = 100, max = 140, price = 75),
  list(name = 'Овсянка, пач.', min = 220, max = 300, price = 100),
  list(name = 'Пирог, шт.', min = 40, max = 45, price = 500), 
  list(name = 'Бананы, шт.', min = 70, max = 75, price = 150)
)

generate.data <- function(file.name = "Новый день", path = "D:/Новый день/Анализ", type = 'SUPPLY', days = 7, goods = h) {
  if (!dir.exists(path)) {
    dir.create(path)
  }
  if (dir.exists(path)) {
    setwd(path)
    tabl <- data.frame('День' = 1:days)
    if (type == 'SUPPLY') {
      for (i in 1:length(goods)) {
        tabl[i + 1] <- sample(x = goods[[i]]$min:goods[[i]]$max, size = days, replace = TRUE)
        colnames(x = tabl)[i + 1] = goods[[i]]$name
      }
      write.table(tabl, file = paste0(file.name, '.', FILE_SUPPLY), col.names=TRUE,row.names = FALSE)
    } else {
      for (i in 1:length(goods)) {
        supply <- read.table(file = paste0(file.name, '.', FILE_SUPPLY), head = TRUE)
        sale_vector <- vector()
        for (j in 1:days) {
          sale_vector[j] <- as.integer(runif(n = 1,min=goods[[i]]$min, max=goods[[i]]$max))
          sale_vector[j] <- ifelse(sale_vector[j]>supply[j,i+1],supply[j,i+1],sale_vector[j])
        }
        tabl[i+1] <- sale_vector
        colnames(x = tabl)[i + 1] = goods[[i]]$name
      }
      write.table(tabl, file = paste0(file.name, '.',FILE_SALE), col.names=TRUE,row.names = FALSE)
    }
    return(tabl)
  }
}

# генератор цены

w1 <- c(50,90,30,25,65,80,450,50)
w2 <- c(60,100,40,35,80,95,550,150)

h1 <- list(
  list(name = 'Название товара'),
  list(name = 'Цена поставки, руб.'),
  list(name = 'Цена продажи, руб.'),
  list(name = 'Цена утилизации, руб.')
)  

generate.price <- function(file.name = "price.txt", path = "D:/Новый день/Анализ_", kd=length(h),good=h1,goods=h) {
  setwd(path)
  w3 <- rep(0,kd)
  tab2 <- data.frame('Товар' = w3)
  for (j in 1:kd) {
    tab2[j,1] = h[[j]]$name
  }
  w4 <- vector()
  for (j in 1:kd) {
    w4[j] <- as.integer(runif(n=1, min = w1[j], max = w2[j]))
  }
  tab2[2] <- w4
  colnames(x = tab2)[2] = good[[2]]$name
  for (j in 1:kd) {
    tab2[j,3]=1.3*tab2[j,2] 
  }
  colnames(x = tab2)[3] = good[[3]]$name
  for (j in 1:kd) {
    tab2[j,4]=0.5*tab2[j,2] 
  }
  colnames(x = tab2)[4] = good[[4]]$name
  write.table(tab2, file = paste0(path, '/', file.name), col.names=TRUE,row.names = FALSE)
  return(tab2)
} 


#итоговая таблица

setwd('D:/Новый день/Анализ_/Магазин_1')
in1 <- read.table('Новый день.in', head = TRUE,encoding='UTF-8')
out1 <- read.table('Новый день.out', head = TRUE,encoding='UTF-8')

setwd('D:/Новый день/Анализ_/Магазин_2')
in2 <- read.table('Новый день.in', head = TRUE,encoding='UTF-8')
out2 <- read.table('Новый день.out', head = TRUE,encoding='UTF-8')

setwd('D:/Новый день/Анализ_/Магазин_3')
in3 <- read.table('Новый день.in', head = TRUE,encoding='UTF-8')
out3 <- read.table('Новый день.out', head = TRUE,encoding='UTF-8')

setwd('D:/Новый день/Анализ_/Магазин_4')
in4 <- read.table('Новый день.in', head = TRUE,encoding='UTF-8')
out4 <- read.table('Новый день.out', head = TRUE,encoding='UTF-8')

setwd('D:/Новый день/Анализ_/Магазин_5')
in5 <- read.table('Новый день.in', head = TRUE,encoding='UTF-8')
out5 <- read.table('Новый день.out', head = TRUE,encoding='UTF-8')

setwd('D:/Новый день/Анализ_/Магазин_6')
in6 <- read.table('Новый день.in', head = TRUE,encoding='UTF-8')
out6 <- read.table('Новый день.out', head = TRUE,encoding='UTF-8')

setwd('D:/Новый день/Анализ_/Магазин_7')
in7 <- read.table('Новый день.in', head = TRUE,encoding='UTF-8')
out7 <- read.table('Новый день.out', head = TRUE,encoding='UTF-8')

setwd('D:/Новый день/Анализ_/Магазин_8')
in8 <- read.table('Новый день.in', head = TRUE,encoding='UTF-8')
out8 <- read.table('Новый день.out', head = TRUE,encoding='UTF-8')

setwd('D:/Новый день/Анализ_/Магазин_9')
in9 <- read.table('Новый день.in', head = TRUE,encoding='UTF-8')
out9 <- read.table('Новый день.out', head = TRUE,encoding='UTF-8')

setwd('D:/Новый день/Анализ_/Магазин_10')
in10 <- read.table('Новый день.in', head = TRUE,encoding='UTF-8')
out10 <- read.table('Новый день.out', head = TRUE,encoding='UTF-8')

price <- read.table('price.txt', head = TRUE,encoding='UTF-8')

setwd('D:/Новый день/Анализ_')

for (i in 1:length(h)) {
  rev <- rep(0,12)
  profit <- rep(0,12)
  res.tab <- data.frame('Выручка, руб.' = rev, 'Прибыль, руб.' = profit)
  sale <-rep(0, nrow(res.tab))
  res.tab$'Реализация, конт.' <- sale
  res.tab$'Списание, конт.' <- 0
  res.tab$'sd' <- 0
  res.tab$'Продажи, макс.' <- 0
  res.tab$'День макс. продажи ' <- 0
  res.tab$'Продажи , мин.' <- 0
  res.tab$'День мин. продажи ' <- 0
  res.tab$'Списание , макс.' <- 0
  res.tab$'День макс. списания  ' <- 0
  for (j in 6:11) {
    res.tab[11, j] = ' '
    res.tab[12, j] = ' '
  }
  
  P_supply1 = price[i, 2]
  P_sale1 = price[i, 3]
  P_util1 = price[i, 4]
  
  Q_in1 = c(
    sum(in1[i+1]),
    sum(in2[i+1]),
    sum(in3[i+1]),
    sum(in4[i+1]),
    sum(in5[i+1]),
    sum(in6[i+1]),
    sum(in7[i+1]),
    sum(in8[i+1]),
    sum(in9[i+1]),
    sum(in10[i+1])
  )
  Q_out1 = c(
    sum(out1[i+1]),
    sum(out2[i+1]),
    sum(out3[i+1]),
    sum(out4[i+1]),
    sum(out5[i+1]),
    sum(out6[i+1]),
    sum(out7[i+1]),
    sum(out8[i+1]),
    sum(out9[i+1]),
    sum(out10[i+1])
  )
  
  for (j in 1:10) {
    Q_util1 = Q_in1[j] - Q_out1[j]
    TR1 = Q_out1[j] * P_sale1
    TC1 = Q_in1[j] * P_supply1 + P_util1 * Q_util1
    Pr1 = TR1 - TC1
    res.tab[j, 1] = TR1
    res.tab[j, 2] = Pr1
    res.tab[j, 3] = Q_out1[j]
    res.tab[j, 4] = Q_util1
  }
  
  
  
  Q_out_day1 = c(out1[1:7, i+1])
  Q_out_day2 = c(out2[1:7, i+1])
  Q_out_day3 = c(out3[1:7, i+1])
  Q_out_day4 = c(out4[1:7, i+1])
  Q_out_day5 = c(out5[1:7, i+1])
  Q_out_day6 = c(out6[1:7, i+1])
  Q_out_day7 = c(out7[1:7, i+1])
  Q_out_day8 = c(out8[1:7, i+1])
  Q_out_day9 = c(out9[1:7, i+1])
  Q_out_day10 = c(out10[1:7,i+1])
  
  res.tab[1, 5] = round(sd(Q_out_day1), i+1)
  res.tab[2, 5] = round(sd(Q_out_day2), i+1)
  res.tab[3, 5] = round(sd(Q_out_day3), i+1)
  res.tab[4, 5] = round(sd(Q_out_day4), i+1)
  res.tab[5, 5] = round(sd(Q_out_day5), i+1)
  res.tab[6, 5] = round(sd(Q_out_day6), i+1)
  res.tab[7, 5] = round(sd(Q_out_day7), i+1)
  res.tab[8, 5] = round(sd(Q_out_day8), i+1)
  res.tab[9, 5] = round(sd(Q_out_day9), i+1)
  res.tab[10, 5] = round(sd(Q_out_day10), i+1)
  
  for (j in 1:5) {
    res.tab[11, j] = sum(res.tab[j])
    res.tab[12, j] = res.tab[11, j] / 10
  }
  
  res.tab[1, 6] = max(Q_out_day1)
  res.tab[1, 7] = which.max(Q_out_day1)
  res.tab[2, 6] = max(Q_out_day2)
  res.tab[2, 7] = which.max(Q_out_day2)
  res.tab[3, 6] = max(Q_out_day3)
  res.tab[3, 7] = which.max(Q_out_day3)
  res.tab[4, 6] = max(Q_out_day4)
  res.tab[4, 7] = which.max(Q_out_day4)
  res.tab[5, 6] = max(Q_out_day5)
  res.tab[5, 7] = which.max(Q_out_day5)
  res.tab[6, 6] = max(Q_out_day6)
  res.tab[6, 7] = which.max(Q_out_day6)
  res.tab[7, 6] = max(Q_out_day7)
  res.tab[7, 7] = which.max(Q_out_day7)
  res.tab[8, 6] = max(Q_out_day8)
  res.tab[8, 7] = which.max(Q_out_day8)
  res.tab[9, 6] = max(Q_out_day9)
  res.tab[9, 7] = which.max(Q_out_day9)
  res.tab[10, 6] = max(Q_out_day10)
  res.tab[10, 7] = which.max(Q_out_day10)
  
  
  
  res.tab[1, 8] = min(Q_out_day1)
  res.tab[1, 9] = which.min(Q_out_day1)
  res.tab[2, 8] = min(Q_out_day2)
  res.tab[2, 9] = which.min(Q_out_day2)
  res.tab[3, 8] = min(Q_out_day3)
  res.tab[3, 9] = which.min(Q_out_day3)
  res.tab[4, 8] = min(Q_out_day4)
  res.tab[4, 9] = which.min(Q_out_day4)
  res.tab[5, 8] = min(Q_out_day5)
  res.tab[5, 9] = which.min(Q_out_day5)
  res.tab[6, 8] = min(Q_out_day6)
  res.tab[6, 9] = which.min(Q_out_day6)
  res.tab[7, 8] = min(Q_out_day7)
  res.tab[7, 9] = which.min(Q_out_day7)
  res.tab[8, 8] = min(Q_out_day8)
  res.tab[8, 9] = which.min(Q_out_day8)
  res.tab[9, 8] = min(Q_out_day9)
  res.tab[9, 9] = which.min(Q_out_day9)
  res.tab[10, 8] = min(Q_out_day10)
  res.tab[10, 9] = which.min(Q_out_day10)
  
  
  
  
  Q_util_day1 = c(in1[1, i+1] - out1[1, i+1],
                  in1[2, i+1] - out1[2, i+1],
                  in1[3, i+1] - out1[3, i+1],
                  in1[4, i+1] - out1[4, i+1],
                  in1[5, i+1] - out1[5, i+1],
                  in1[6, i+1] - out1[6, i+1],
                  in1[7, i+1] - out1[7, i+1])
  Q_util_day2 = c(in2[1, i+1] - out2[1, i+1],
                  in2[2, i+1] - out2[2, i+1],
                  in2[3, i+1] - out2[3, i+1],
                  in2[4, i+1] - out2[4, i+1],
                  in2[5, i+1] - out2[5, i+1],
                  in2[6, i+1] - out2[6, i+1],
                  in2[7, i+1] - out2[7, i+1])
  Q_util_day3 = c(in3[1, i+1] - out3[1, i+1],
                  in3[2, i+1] - out3[2, i+1],
                  in3[3, i+1] - out3[3, i+1],
                  in3[4, i+1] - out3[4, i+1],
                  in3[5, i+1] - out3[5, i+1],
                  in3[6, i+1] - out3[6, i+1],
                  in3[7, i+1] - out3[7, i+1])
  Q_util_day4 = c(in4[1, i+1] - out4[1, i+1],
                  in4[2, i+1] - out4[2, i+1],
                  in4[3, i+1] - out4[3, i+1],
                  in4[4, i+1] - out4[4, i+1],
                  in4[5, i+1] - out4[5, i+1],
                  in4[6, i+1] - out4[6, i+1],
                  in4[7, i+1] - out4[7, i+1])
  Q_util_day5 = c(in5[1, i+1] - out5[1, i+1],
                  in5[2, i+1] - out5[2, i+1],
                  in5[3, i+1] - out5[3, i+1],
                  in5[4, i+1] - out5[4, i+1],
                  in5[5, i+1] - out5[5, i+1],
                  in5[6, i+1] - out5[6, i+1],
                  in5[7, i+1] - out5[7, i+1])
  Q_util_day6 = c(in6[1, i+1] - out6[1, i+1],
                  in6[2, i+1] - out6[2, i+1],
                  in6[3, i+1] - out6[3, i+1],
                  in6[4, i+1] - out6[4, i+1],
                  in6[5, i+1] - out6[5, i+1],
                  in6[6, i+1] - out6[6, i+1],
                  in6[7, i+1] - out6[7, i+1])
  Q_util_day7 = c(in7[1, i+1] - out7[1, i+1],
                  in7[2, i+1] - out7[2, i+1],
                  in7[3, i+1] - out7[3, i+1],
                  in7[4, i+1] - out7[4, i+1],
                  in7[5, i+1] - out7[5, i+1],
                  in7[6, i+1] - out7[6, i+1],
                  in7[7, i+1] - out7[7, i+1])
  Q_util_day8 = c(in8[1, i+1] - out8[1, i+1],
                  in8[2, i+1] - out8[2, i+1],
                  in8[3, i+1] - out8[3, i+1],
                  in8[4, i+1] - out8[4, i+1],
                  in8[5, i+1] - out8[5, i+1],
                  in8[6, i+1] - out8[6, i+1],
                  in8[7, i+1] - out8[7, i+1])
  Q_util_day9 = c(in9[1, i+1] - out9[1, i+1],
                  in9[2, i+1] - out9[2, i+1],
                  in9[3, i+1] - out9[3, i+1],
                  in9[4, i+1] - out9[4, i+1],
                  in9[5, i+1] - out9[5, i+1],
                  in9[6, i+1] - out9[6, i+1],
                  in9[7, i+1] - out9[7, i+1])
  Q_util_day10 = c(
    in10[1, i+1] - out10[1, i+1],
    in10[2, i+1] - out10[2, i+1],
    in10[3, i+1] - out10[3, i+1],
    in10[4, i+1] - out10[4, i+1],
    in10[5, i+1] - out10[5, i+1],
    in10[6, i+1] - out10[6, i+1],
    in10[7, i+1] - out10[7, i+1]
  )
  
  
  res.tab[1, 10] = max(Q_util_day1)
  res.tab[1, 11] = which.max(Q_util_day1)
  res.tab[2, 10] = max(Q_util_day2)
  res.tab[2, 11] = which.max(Q_util_day2)
  res.tab[3, 10] = max(Q_util_day3)
  res.tab[3, 11] = which.max(Q_util_day3)
  res.tab[4, 10] = max(Q_util_day4)
  res.tab[4, 11] = which.max(Q_util_day4)
  res.tab[5, 10] = max(Q_util_day5)
  res.tab[5, 11] = which.max(Q_util_day5)
  res.tab[6, 10] = max(Q_util_day6)
  res.tab[6, 11] = which.max(Q_util_day6)
  res.tab[7, 10] = max(Q_util_day7)
  res.tab[7, 11] = which.max(Q_util_day7)
  res.tab[8, 10] = max(Q_util_day8)
  res.tab[8, 11] = which.max(Q_util_day8)
  res.tab[9, 10] = max(Q_util_day9)
  res.tab[9, 11] = which.max(Q_util_day9)
  res.tab[10, 10] = max(Q_util_day10)
  res.tab[10, 11] = which.max(Q_util_day10)
  write.table(res.tab, file = paste0(h[[i]]$name, '.csv'), sep = ';', dec = ',', row.names=FALSE)
  
}

generate.data(path = "D:/Новый день/Анализ_/Магазин_1", type = 'SUPPLY')
generate.data(path = "D:/Новый день/Анализ_/Магазин_1", type = 'SALE')
generate.data(path = "D:/Новый день/Анализ_/Магазин_2", type = 'SUPPLY')
generate.data(path = "D:/Новый день/Анализ_/Магазин_2", type = 'SALE')
generate.data(path = "D:/Новый день/Анализ_/Магазин_3", type = 'SUPPLY')
generate.data(path = "D:/Новый день/Анализ_/Магазин_3", type = 'SALE')
generate.data(path = "D:/Новый день/Анализ_/Магазин_4", type = 'SUPPLY')
generate.data(path = "D:/Новый день/Анализ_/Магазин_4", type = 'SALE')
generate.data(path = "D:/Новый день/Анализ_/Магазин_5", type = 'SUPPLY')
generate.data(path = "D:/Новый день/Анализ_/Магазин_5", type = 'SALE')
generate.data(path = "D:/Новый день/Анализ_/Магазин_6", type = 'SUPPLY')
generate.data(path = "D:/Новый день/Анализ_/Магазин_6", type = 'SALE')
generate.data(path = "D:/Новый день/Анализ_/Магазин_7", type = 'SUPPLY')
generate.data(path = "D:/Новый день/Анализ_/Магазин_7", type = 'SALE')
generate.data(path = "D:/Новый день/Анализ_/Магазин_8", type = 'SUPPLY')
generate.data(path = "D:/Новый день/Анализ_/Магазин_8", type = 'SALE')
generate.data(path = "D:/Новый день/Анализ_/Магазин_9", type = 'SUPPLY')
generate.data(path = "D:/Новый день/Анализ_/Магазин_9", type = 'SALE')
generate.data(path = "D:/Новый день/Анализ_/Магазин_10", type = 'SUPPLY')
generate.data(path = "D:/Новый день/Анализ_/Магазин_10", type = 'SALE')
generate.price()