# 脚本说明：
# 本脚本为主要的模型部分：
#     包括数据预处理、学习参数、sl生成、距离测算及入格、计数模型、绘图函数六大块
#     本脚本与主程序模块1.2的不同之处在于：入格函数先对同一个wifi出发的所有被交格子
#   频数设为1，再对其进行频数统计，取最大频数的格子
# -----------------------------------------------

# 1.1子函数f.dataExtract，筛选出wifi_correction_data_table对应设备的起止时间段内的数据
f.dataExtract<-function(data){
  interval <- wifi_correction_data_table[wifi_correction_data_table$T_Mac==data["T_Mac"],c("WCD_Tstart","WCD_Tstop")];
  logic <- any(as.character(data["datetime_minute"]) >= interval$WCD_Tstart&as.character(data["datetime_minute"]) < 
                 interval$WCD_Tstop)
  if(logic){
    return(TRUE)
  }
  else 
    return(FALSE)
}

# 1.2子函数f.dsMacCompute，计算校验数据与对应探针的距离
f.dsMacCompute<-function(data){
  interval <- wifi_correction_data_table[,c("WCD_Tstart","WCD_Tstop")];
  logic <- as.character(data["datetime_minute"]) >= interval$WCD_Tstart&as.character(data["datetime_minute"]) < interval$WCD_Tstop &
    wifi_correction_data_table$T_Mac == as.character(data["T_Mac"])
  x <- wifi_correction_data_table[logic,]["WCD_x"]
  y <- wifi_correction_data_table[logic,]["WCD_y"]
  result <- as.numeric(sqrt((as.numeric(x) - as.numeric(data["WP_x"]))^2+(as.numeric(y) - as.numeric(data["WP_y"]))^2))
  return(result)
}

# 1.3封装上述函数对标运算,输入表D.merge.wifi.data，输出带距离的表。
F.TraindataExtract<-function(data.train){
  #创建d.wifi_correction用于存放输出结果。
  d.wifi_correction <- data.train[data.train$T_Mac %in% wifi_correction_data_table$T_Mac,]
  d.wifi_correction <- data.frame(d.wifi_correction,as.data.frame(matrix(NA,dim(d.wifi_correction)[1],1)))
  colnames(d.wifi_correction) <- c(colnames(data.train),"distance")
  d.wifi_correction$datetime_minute <- as.character(d.wifi_correction$datetime_minute)
  
  #调用子函数1，筛选出wifi_correction_data_table对应设备的起止时间段内的数据
  d.wifi_correction <- d.wifi_correction[apply(d.wifi_correction,1,f.dataExtract),]
  
  #调用子函数2，计算校验数据与对应探针的距离
  d.wifi_correction$distance <- apply(d.wifi_correction,1,f.dsMacCompute)
  return(d.wifi_correction)
}

# -----------------------------------------------
#       2.学习器
# -----------------------------------------------

## 2.1 学习函数F.paraLearning
# 输入：wifi_correction
# 输出：参数p0和n
F.paraLearning <- function(train_data=D.train_data,method="mean"){
  moo <- as.data.frame(matrix(NA,0,2))
  colnames(moo) <- c("p0","n")
  if(method == "mean"){
    data <- aggregate(train_data[,c("RSSI_minute_max","distance")],list(train_data$distance),mean)#在每个距离水平上汇总取平均值。
    train_data_transfer <- data.frame("pth" = data[,"RSSI_minute_max"],"logd" = 10*log10(as.numeric(data[,"distance"])))
    lm.sol <- coef(lm(pth ~ 1+logd, data = train_data_transfer))
    noo <- t(as.data.frame(lm.sol))
    moo <- rbind(moo,noo)
    for(index2 in wifi_probe_info_table$WP_Mac){
      rownames(noo) <- index2
      moo <- rbind(moo,noo)
    }
    moo <- moo[-1,]
  }
  else if(method == "max"){
    data <- aggregate(train_data[,c("RSSI_minute_max","distance")],list(train_data$distance),max)#在每个距离水平上汇总，求最大值
    train_data_transfer <- data.frame("pth"=data[,"RSSI_minute_max"],"logd"=10*log10(as.numeric(data[,"distance"])))
    lm.sol <- coef(lm(pth ~ 1+logd, data = train_data_transfer))
    noo <- t(as.data.frame(lm.sol))
    moo <- rbind(moo,noo)
    for(index2 in wifi_probe_info_table$WP_Mac){
      rownames(noo) <- index2
      moo <- rbind(moo,noo)
    }
    moo <- moo[-1,]
  }
  else if(method == "each"){
    data <- na.omit(train_data[,c("RSSI_minute_max","distance","WP_Mac")])
    train_data_transfer <- data.frame("WP_Mac" = data$WP_Mac, "pth" = data[,"RSSI_minute_max"], "logd" = 10*log10(as.numeric(data[,"distance"])))
    for(index2 in wifi_probe_info_table$WP_Mac){
      lm.sol <- coef(lm(pth ~ 1+logd, data = train_data_transfer, subset = (WP_Mac == index2)))
      noo <- t(as.data.frame(lm.sol))
      rownames(noo)<-index2
      moo <- rbind(moo,noo)
    }
  }
  colnames(moo) <- c("p0","n")
  return(moo)
}

F.paraLearning.safe <- function(D.para){
  if(all(D.para$n < 1)){
    d.para <- D.para
  }else{
    p0 <- mean(D.para[which(D.para$n < 1),]$p0)
    n <- mean(D.para[which(D.para$n < 1),]$n)
    d.para <-D.para
    d.para$p0 <- p0
    d.para$n <- n
  }
  return(d.para)
}



##  2.2入格函数F.gridJoinFunction
# 输入校验数据及其坐标，生成入格的校验数据
F.gridJoinFunction <- function(train_data = wifi_correction_data_table,width = floor_grid_length)
{
  grid_t <- matrix(NA, dim(train_data)[1] , 3);
  colnames(grid_t) <- c("grid_id","grid_x","grid_y")
  train_data_grid <- data.frame(train_data,grid_t)
  train_data_grid$grid_x <- train_data_grid$WCD_x %/% width + 0.5
  train_data_grid$grid_y <- train_data_grid$WCD_y %/% width + 0.5
  
  train_data_grid$grid_id <- apply(train_data_grid,1,function(x) paste( paste("x",x["grid_x"],sep="_"), 
                                                                        paste("y",x["grid_y"],sep="_") , sep=","))
  return(train_data_grid)
}

# -----------------------------------------------
#       3.设备位置估计
# -----------------------------------------------

#说明：该脚本为非随机sl生成模型，
#主要可调节参数为：
#       步长step.set.initial；是否是等比数列arith.progress.set
### 3.1数列生成函数,其实是准备函数的一支，不过这里不放在4里面。用于生成等差或等比数列。
f.series.generator<-function(start,end,ratio,first.term,last.term,step = 5,arith.progress =0){
  if(arith.progress == 1){
    series <- seq(end,start,by = step)
  }
  else{
    if(ratio >= 1 | first.term/(1-ratio) <= (start -end) |start <= end){
      message("\"ratio\" or \"first.term\" is not suitable,also,start is needed to be greater-than end")
      break
    }
    else{
      series <-start
      term <- first.term
      while(1){
        if(series[length(series)] < 0){
          break
        }
        else{
          term <- max(term*ratio,2)
          series <- append(series,series[length(series)]-term)
        }
      }
      while(series[length(series)] <= 0){
        series <- series[1:length(series)-1]
      }
    }
  }
  return(series) 
}

### 3.2 计算单行的sl函数f.drame
f.drame<-function(x,step.set=5,arith.progress.set=0){
  p <- D.para[as.character(x["WP_Mac"]),"p0"]
  #有障碍
  mm <- wifi_signal_loss_value_table[,c("SL_value_min","SL_value_max")]
  mm1<-mm
  mm1[6:7,] <- 0
  interval1 <- as.matrix(D.wifi.collection[D.wifi.collection$WP_Mac == as.character(x["WP_Mac"]),][,
                                                                                                   c("SL_type1","SL_type2","SL_type3","SL_type4","SL_type5","SL_type6","SL_type7")]) %*% as.matrix(mm1)
  #综合
  interval2 <- as.matrix(D.wifi.collection[D.wifi.collection$WP_Mac == as.character(x["WP_Mac"]),][,
                                                                                                   c("SL_type1","SL_type2","SL_type3","SL_type4","SL_type5","SL_type6","SL_type7")]) %*% as.matrix(mm)
  
  #形成等比数列
  series1 <- f.series.generator(interval1[2],0,ratio = 1/1.1,first.term = 5,last.term = 2,step=step.set,arith.progress = arith.progress.set)
  series2 <- f.series.generator(interval2[2],interval1[1],ratio = 1/1.1,first.term = 5,last.term = 2,step=step.set,arith.progress = arith.progress.set)
  
  #按不同模式给sl赋值
  if(as.numeric(x["RSSI_minute_max"]) >= 2*p){
    value <- series1
  }
  else{
    value <- series2
  }
  y <- as.data.frame(matrix(NA,length(value),9))
  colnames(y) <- colnames(x)
  for(i in 1:length(value)){
    y[i,][1:7] <- x[1:7]
    y[i,8] <- i
    y[i,9] <- value[i]
  }
  return(y)
}

### 3.3封装sl生成函数F.sl.Constructor
F.sl.Constructor<-function(d.data.for.estimate,step.set.initial=5,arith.progress.set.initial=0){
  d.sl <- data.frame(d.data.for.estimate,matrix(NA,dim(d.data.for.estimate)[1],2))
  colnames(d.sl) <- c(colnames(d.data.for.estimate),"type","sl")
  
  #list化,因为长度不等
  d.sl.list<-list()
  for(i in 1:dim(d.sl)[1]){
    df <- d.sl[i,]
    d.sl.list[[i]] <- df
  } 
  
  #ldply调用
  d.sl.result<-ldply(d.sl.list,f.drame,step.set=step.set.initial,arith.progress.set=arith.progress.set.initial)
  return(d.sl.result)
}

##  3.2 信号筛选函数F.filter
### 3.2.1子函数f.compute.maxdis，求坐标到边界的最大值
f.compute.maxdis <- function(x , y, floor_length = floor_length_initial , length_cut=0)#floor_length为房间长，length_cut为房间长比宽多出的部分。
{
  x2 <- floor_length;
  y2 <- floor_length-length_cut;
  
  if(x < (x2/2) & y < (y2/2)){
    dis <- sqrt((x - x2)^2 + (y - y2)^2)
  }
  else if(x >= (x2/2) & y < (y2/2)){
    dis <- sqrt((x)^2 + (y - y2)^2)
  }
  else if(x < (x2/2) & y >= (y2/2)){
    dis <- sqrt((x - x2)^2 + (y)^2)
  }
  else if(x >= (x2/2) & y >= (y2/2)){
    dis <- sqrt((x)^2 + (y)^2)
  }
  return(dis)
}

### 3.2.2子函数f.filter，里面调用了子函数f.compute.maxdis
f.filter <- function(x , para = D.para , floor_length_intl = floor_length_initial){ 
  #利用综合sl模型筛选出界的设备
  logd <- log10(f.compute.maxdis(as.numeric(x["WP_x"]),as.numeric(x["WP_y"]),floor_length=floor_length_intl,length_cut=0))
  
  p <- para[as.character(x["WP_Mac"]),"p0"]
  n <- para[as.character(x["WP_Mac"]),"n"]
  
  mm <- wifi_signal_loss_value_table[,c("SL_value_min","SL_value_max")]
  interval <- as.numeric(D.wifi.collection[D.wifi.collection$WP_Mac == as.character(x["WP_Mac"]),][,
                                                                                                   c("SL_type1","SL_type2","SL_type3","SL_type4","SL_type5","SL_type6","SL_type7")]) %*% as.matrix(mm)
  
  faf <- interval[1]#这是最小值
  
  min.pth <- p + 10*n*logd - faf
  
  return(as.numeric(x["RSSI_minute_max"]) >= min.pth)
}

### 3.2.3 封装筛选函数F.filter
F.filter<-function(d.sl,floor_length_initial){
  d.strl<-apply(d.sl , 1 , f.filter , floor_length_intl = floor_length_initial)
  noo <- d.sl[d.strl,]
  return(noo)
}

##  3.3距离计算器f.distCompute
### 3.3.1子函数f.distCompute
f.distCompute <- function(data)
{
  p <- D.para[as.character(data["WP_Mac"]) , "p0"]
  n <- D.para[as.character(data["WP_Mac"]) , "n"]
  d <- 10^((as.numeric(data["RSSI_minute_max"]) + as.numeric(data["sl"]) - p)/(10*n))
  return(d)
}

##3.4入格统计函数，输入前面经过重重变换的d.sl，输出入格统计表
## 3.4.1子函数入格函数F.grid.join
#创建内部封装的子函数，对每一条DWMac_TID对应的记录进行入格统计并筛选出频数大于3的格子
f.gridJoin.estimate <- function(datals,type.num = 0.6035534){
  d.result <- as.data.frame(matrix(0 , 0 , 2))
  colnames(d.result) <- c("DWMac_TID","grid_id")
  
  for(index in unique(datals$WP_Mac)){
    datals2<-datals[datals$WP_Mac==index,]
    d.equal <- as.data.frame(matrix(0 , 0 , 2))
    colnames(d.equal) <- c("DWMac_TID","grid_id")
    for(i in 1:dim(datals2)[1]){
      dist.table <- D.dist_warehouse[D.dist_warehouse$WP_Mac == datals2[i,"WP_Mac"],]
      logic <- (datals2[i,"distance"] <= dist.table$dist+type.num) & (datals2[i,
                                                                              "distance"] > (dist.table$dist-type.num))
      df <- dist.table[logic,"grid_id"]
      if(length(df)){
        df <- data.frame(rep(unique(datals$DWMac_TID),length(df)),df)
        colnames(df) <- c("DWMac_TID","grid_id")
        d.equal <- rbind(d.equal,df)
      }
    }
    d.equal <- unique(d.equal)#模型1.1和模型1.2的差异在这里
    d.result <- rbind(d.result,d.equal)
  }
  freq.table <- count(d.result,vars = "grid_id")
  d.result <- d.result[d.result$grid_id  %in%  freq.table[freq.table$freq >=3,]$grid_id,]
  return(unique(d.result))
}

### 3.4.2封装函数
F.grid.join <- function(d.sl,type.num.initial = 0.6035534){
  #创建一个list以供调用
  D.list <- list()
  for(index in unique(d.sl$DWMac_TID)){
    df <- d.sl[d.sl$DWMac_TID == index,]
    #筛选出至少被三个wifi同时探测到的记录
    if(length(unique(df$WP_Mac)) >= 3){
      D.list[[index]] <- df
    }
  } 
  #ldply运行函数对每条DWMac_TID进行运算
  result <- ldply(D.list, f.gridJoin.estimate,type.num = type.num.initial)
  d.result <- data.frame(result[,c("DWMac_TID","grid_id")],rep(grid_CFN,dim(result)[1]))
  colnames(d.result) <- c("DWMac_TID","grid_id","grid_CFN")
  return(d.result)
}

### 3.4.5循环封装，暂时模型二用不上循环。
#需要缓存数据D.dist_warehouse,D.data.for.estimate
F.grid.loop <- function(D.data.for.estimate = D.merge.wifi.data,type.num.initial,
                        step.set.initial,arith.progress.set.initial){#前面生成的缓存也要加进来
  d.data.for.estimate <- D.data.for.estimate#初始化data.for.estimate
  loop.nums <- 1#为了保持函数与模型一差异不大添加该变量。
  #创建结果接收的容器
  result <- as.data.frame(matrix(NA,0,3))
  colnames(result) <- c("DWMac_TID","grid_id","grid_CFN")
  
  #生成sl
  d.sl <- F.sl.Constructor(d.data.for.estimate,
                           step.set.initial=step.set.initial,arith.progress.set.initial=arith.progress.set.initial)
  
  #噪声筛选
  d.sl <- F.filter(d.sl,floor_length_initial)
  
  #求距离
  d.sl$RSSI_minute_max <- apply(d.sl,1,f.distCompute)#这里如果再建立一个库可以提高效率
  colnames(d.sl) <- c("T_Mac","WP_Mac","datetime_minute","DWMac_TID","distance","WP_x","WP_y","type","sl" )
  
  #入格
  d.sl <- d.sl[!is.na(d.sl$distance),]
  
  #将有效数据写入数据库data_program_mac_valid表中
  #将有效数据写入数据库data_program_mac_valid表中
  faf <- unique(d.sl[,c("T_Mac","WP_Mac")])
  file <- data.frame(faf,"grid_CFN"=rep(grid_CFN,dim(faf)[1]))
  F.load_into_MySQL(host,port,dbname,user,
                    password,'data_program_mac_valid',file)
  
  #输出结果
  result_now <- F.grid.join(d.sl,type.num.initial)
  result <- rbind(result,result_now)#这一步是为后面模型修改为循环做准备的。
  return(result)
}

# -----------------------------------------------
#      4.一些数据准备函数
# -----------------------------------------------

##  4.1#合并数据函数F.merge.data.generator
# 将wifi_data_warehouse_minute中的数据和对应的wifi坐标拼接起来用于第1,2,3步调用，以空间换时间。
F.merge.data.generator<-function(data,unit="minute"){
  if(unit=="minute"){
    d.merge.wifi.data<-data.frame(data,as.data.frame(matrix(NA,dim(data)[1],2)))
    colnames(d.merge.wifi.data)=c(colnames(data),"WP_x","WP_y")
    d.merge.wifi.data[,"WP_x"]<-
      apply(d.merge.wifi.data,1,function(x) return(wifi_probe_info_table[wifi_probe_info_table$WP_Mac==
                                                                           as.character(x["WP_Mac"]),"WP_x"]))
    d.merge.wifi.data[,"WP_y"]<-
      apply(d.merge.wifi.data,1,function(x) return(wifi_probe_info_table[wifi_probe_info_table$WP_Mac==
                                                                           as.character(x["WP_Mac"]),"WP_y"]))
    return(d.merge.wifi.data)
  }
  else if(unit=="second"){
    return(0)
  }
}

##  4.2 探针的收集表变换函数F.wifi.collection
#wifi_signal_loss_collection_table不好调用，截取第一列前12个字符单独生成一列
F.wifi.collection <- function(wifi_signal_loss_collection_table,intercept="WPI_ID",add="WP_Mac"){
  D.wifi.collection <- data.frame(wifi_signal_loss_collection_table,as.data.frame(matrix(0,dim(wifi_signal_loss_collection_table)[1],1)))
  colnames(D.wifi.collection) <- c(colnames(wifi_signal_loss_collection_table),add)
  D.wifi.collection$WP_Mac <- apply(D.wifi.collection,1, function(x) substr(x[intercept],1,12))#截取WPI_ID中的前12个字段——WP_Mac
  D.wifi.collection <- D.wifi.collection[D.wifi.collection$WPI_ID %in% wifi_probe_info_table$WPI_ID,]
  return(D.wifi.collection)
}

##  4.3建立距离库函数F.dist.warehouse
### 4.3.1子函数f.gridJoin，计算wifi到格子中心的距离
f.gridJoin<-function(temp.file , width = floor_grid_length)
{
  x1 <- floor_grid_table[floor_grid_table$grid_id == as.character(temp.file["grid_id"]),]$grid_x
  x2 <- wifi_probe_info_table[wifi_probe_info_table$WP_Mac == as.character(temp.file["WP_Mac"]),]$WP_x#这里要保证wifi在给定时间段内不移动否则不唯一
  y1 <- floor_grid_table[floor_grid_table$grid_id == as.character(temp.file["grid_id"]),]$grid_y
  y2 <- wifi_probe_info_table[wifi_probe_info_table$WP_Mac == as.character(temp.file["WP_Mac"]),]$WP_y
  dist <- sqrt((x1 - x2)^2 + (y1 - y2)^2)
  return(dist)
}

### 4.3.2封装子函数f.gridJoin生成距离库。
F.dist.warehouse <- function(floor_grid_table , D.merge.wifi.data){
  D.dist_warehouse <- as.data.frame(matrix(NA,length(floor_grid_table$grid_id) * length(unique(D.merge.wifi.data$WP_Mac)) , 3))
  colnames(D.dist_warehouse) <- c("WP_Mac","grid_id","dist")
  d.names <- rep(floor_grid_table$grid_id , rep(length(unique(D.merge.wifi.data$WP_Mac)),length(floor_grid_table$grid_id)))
  D.dist_warehouse$grid_id <- d.names
  D.dist_warehouse$WP_Mac <- rep(unique(D.merge.wifi.data$WP_Mac),length(floor_grid_table$grid_id))
  
  # apply调用子函数生成距离库
  D.dist_warehouse$dist<-apply(D.dist_warehouse,1,f.gridJoin)
  return(D.dist_warehouse)
}

##  4.4模型中秒钟分钟的切换
F.transform <- function(wifi_data_second,units = "minute"){
  if(units == "minute"){
    d.wifi_data_second <- wifi_data_second
  }
  else if(units == "second"){
    d.wifi_data_second <- wifi_data_second[,c("T_Mac","WP_Mac","datetime_second","DWMac_TID","RSSI_second_max")]
    colnames(d.wifi_data_second) <- c("T_Mac","WP_Mac","datetime_minute","DWMac_TID","RSSI_minute_max")
  }
  else{
    message("The para units must be \"minute\" or \"second\"")
  }
  return(d.wifi_data_second)
}

# -----------------------------------------------
#      5.结合数据库调用将上述整体封装为一个大的函数
# -----------------------------------------------

F.function.call <- function(time_inferior = "20161229150000",time_superior = "20161229153000",grid_CFN = "JRCSLFZ00220161229125000",
                            type.num.initial=0.6035534,step.set.initial=5,arith.progress.set.initial=0){
  #参数说明：输入的为起止时间、地点，这里待估数据为起止时间内的以分钟为单位的数据，校验数据为对应于该地址编码的校验数据。
  grid_CFN <<- grid_CFN
  #需要的表名
  global_name <- c("wifi_correction_data_table","wifi_data_warehouse_second","wifi_data_warehouse_minute",
                   "wifi_probe_info_table","wifi_signal_loss_value_table","wifi_signal_collection_table"
                   ,"floor_table","floor_grid_table","work_floor_grid_table")
  
  #从数据库中读取相关表并赋值给对应名字，不要改
  i=1;sql00<-'SELECT * FROM';sql00<-paste(sql00,global_name[i],sep=" ")
  wifi_correction_data_table <<- F.extract_from_MySQL(host,port,dbname,user,password,sql00,max_obs)
  
  i=2;sql00<-'SELECT * FROM';sql00<-paste(sql00,global_name[i],sep=" ")
  wifi_data_warehouse_second <<- F.extract_from_MySQL(host,port,dbname,user,password,sql00,max_obs)
  
  i=3;sql00<-'SELECT * FROM';sql00<-paste(sql00,global_name[i],sep=" ")
  wifi_data_warehouse_minute <<- F.extract_from_MySQL(host,port,dbname,user,password,sql00,max_obs)
  
  i=4;sql00<-'SELECT * FROM';sql00<-paste(sql00,global_name[i],sep=" ")
  wifi_probe_info_table <<- F.extract_from_MySQL(host,port,dbname,user,password,sql00,max_obs)
  
  i=5;sql00<-'SELECT * FROM';sql00<-paste(sql00,global_name[i],sep=" ")
  wifi_signal_loss_value_table <<- F.extract_from_MySQL(host,port,dbname,user,password,sql00,max_obs)
  
  i=6;sql00<-'SELECT * FROM';sql00<-paste(sql00,global_name[i],sep=" ")
  wifi_signal_loss_collection_table <<- F.extract_from_MySQL(host,port,dbname,user,password,sql00,max_obs)
  
  i=7;sql00<-'SELECT * FROM';sql00<-paste(sql00,global_name[i],sep=" ")
  floor_table <<- F.extract_from_MySQL(host,port,dbname,user,password,sql00,max_obs)
  
  i=8;sql00<-'SELECT * FROM';sql00<-paste(sql00,global_name[i],sep=" ")
  floor_grid_table <<- F.extract_from_MySQL(host,port,dbname,user,password,sql00,max_obs)
  
  i=9;sql00<-'SELECT * FROM';sql00<-paste(sql00,global_name[i],sep=" ")
  work_floor_grid_table <<- F.extract_from_MySQL(host,port,dbname,user,password,sql00,max_obs)
  
  
  
  #wifi_data_warehouse_minute <<- F.transform(wifi_data_warehouse_second,units = "second")
  
  
  #通过起止时间筛选分钟表
  wifi_data_warehouse_minute.copy <<- wifi_data_warehouse_minute
  logic <- wifi_data_warehouse_minute$datetime_minute < time_superior &
    wifi_data_warehouse_minute$datetime_minute >= time_inferior
  wifi_data_warehouse_minute <<- wifi_data_warehouse_minute[logic,]
  
  
  #通过起止时间筛选秒钟表
  logic <- wifi_data_warehouse_second$datetime_second < time_superior &
    wifi_data_warehouse_second$datetime_second >= time_inferior
  wifi_data_warehouse_second <<- wifi_data_warehouse_second[logic,]
  
  #通过起止时间和地址编码筛选校验数据编码
  wifi_correction_data_table <<- wifi_correction_data_table[!is.na(wifi_correction_data_table$WCD_Tstop),]
  wifi_correction_data_table <<- wifi_correction_data_table[!is.na(wifi_correction_data_table$WCD_Tstart),]
  logic <- wifi_correction_data_table$grid_CFN == grid_CFN 
  wifi_correction_data_table <<- wifi_correction_data_table[logic,]
  
  #根据上述表更新用于校验的数据
  wifi_data_warehouse_minute.train <<- wifi_data_warehouse_minute.copy[wifi_data_warehouse_minute.copy$T_Mac %in%
                                                                         wifi_correction_data_table$T_Mac,]
  
  #选取wifi信息表，方法同上
  wifi_probe_info_table <<- wifi_probe_info_table[!is.na(wifi_probe_info_table$WPI_Tstop),]
  wifi_probe_info_table <<- wifi_probe_info_table[!is.na(wifi_probe_info_table$WPI_Tstart),]
  logic <- wifi_probe_info_table$grid_CFN == grid_CFN &
    wifi_probe_info_table$WPI_Tstop >= time_inferior & 
    wifi_probe_info_table$WPI_Tstart <= time_superior
  wifi_probe_info_table <<- wifi_probe_info_table[logic,]
  
  #wifi_floor_grid_table筛选提取空间的长度和最小方格尺寸
  work_floor_grid_table <<- work_floor_grid_table[!is.na(work_floor_grid_table$work_datetime),]
  logic <- work_floor_grid_table$grid_CFN == grid_CFN 
  work_floor_grid_table <<- work_floor_grid_table[logic,]
  floor_length_initial <<- as.numeric(work_floor_grid_table[,"floor_length"])
  floor_grid_length <<- as.numeric(work_floor_grid_table[,"grid_length"])
  
  #通过地址编码筛选对应的floor_grid_table
  floor_grid_table <<- floor_grid_table[floor_grid_table$grid_CFN == grid_CFN,]
  
  #-----------------------------------------------------------------------------
  #调用模型：
  #数据准备，将数据拼接或者将表的结构改变，提高程序速度或者方便调用
  #分钟信息和wifi信息表的合并
  D.merge.wifi.data <<- F.merge.data.generator(wifi_data_warehouse_minute)
  
  # wifi_signal_loss_collection_table不好调用，截取第一列前12个字符单独生成一列
  D.wifi.collection <<- F.wifi.collection(wifi_signal_loss_collection_table)
  
  #校验数据和wifi信息表的合并和校验数据提取
  D.merge.wifi.data.train <<- F.merge.data.generator(wifi_data_warehouse_minute.train)
  D.train_data <<- F.TraindataExtract(D.merge.wifi.data.train)#校验数据
  
  #训练校验数据
  D.para <<- F.paraLearning(D.train_data,method="each")
  D.para <<- F.paraLearning.safe(D.para)
  
  #   将上一步的结果进行变形写入数据库方便验证。
  d.wifi_probe <- wifi_probe_info_table
  d.wifi_probe$WP_x <- wifi_probe_info_table$WP_x %/% floor_grid_length + 0.5
  d.wifi_probe$WP_y <- wifi_probe_info_table$WP_y %/% floor_grid_length + 0.5
  v.col.names <- colnames(wifi_probe_info_table)
  colnames(d.wifi_probe)[which(v.col.names == "WP_x")] <- "grid_x"
  colnames(d.wifi_probe)[which(v.col.names == "WP_y")] <- "grid_y"
  F.load_into_MySQL(host,port,dbname,user,password,"data_program_probe_location",d.wifi_probe)
  #校验数据入格，并将结果写入临时表data_program_mac_valid中，覆盖
  
  train_data_in_grid.D <<- F.gridJoinFunction()
  F.load_into_MySQL(host,port,dbname,user,
                    password,"data_program_correction",train_data_in_grid.D)
  
  #生成探针指纹库
  D.dist_warehouse <<- F.dist.warehouse(floor_grid_table, D.merge.wifi.data)
  
  #待估数据入格，并将结果写入临时表data_program_grid_temp_table中，delete+append
  wifi.in.grid.D <<- F.grid.loop(D.merge.wifi.data,type.num.initial,
                                 step.set.initial,arith.progress.set.initial)
  F.load_into_MySQL(host,port,dbname,user,
                    password,"data_program_grid_temp_table",wifi.in.grid.D)
  
  # 最终shuchu结果为wifi.in.grid.D
  return(wifi.in.grid.D)
} 

#===============================================================================================================
# -----------------------------------------------
#           定义产生网格化的函数
# -----------------------------------------------
F.gridding <- function(d.floor_info)####这个d.floor_into是不是work_floor_grid_table中与项目对应的行
{
  c.floor_length <- d.floor_info[1,'floor_length']
  c.grid_length <- d.floor_info[1,'grid_length']
  v.tmp_seq <- seq(from = c.grid_length/2,
                   to = c.floor_length - c.grid_length/2,
                   by = c.grid_length)
  
  c.tmp_len <- length(v.tmp_seq)
  v.grid_x <- rep(x = v.tmp_seq,times = c.tmp_len)
  v.grid_y <- rep(x = v.tmp_seq,each = c.tmp_len)
  
  v.x_lab <- paste('x',v.grid_x,sep = '_')
  v.y_lab <- paste('y',v.grid_y,sep = '_')
  
  v.grid_id <- paste(v.x_lab,v.y_lab,sep = ',')
  
  grid.df <- data.frame(grid_x = v.grid_x,
                        grid_y = v.grid_y,
                        grid_id = v.grid_id,
                        stringsAsFactors = F)
  grid.df <- within(grid.df,{
    grid_CFN <- d.floor_info[1,'grid_CFN']
    code_FN <- d.floor_info[1,'code_FN']
    grid_length <- c.grid_length
    true_false <- 1
  })
  return(grid.df)
}###

# ---------------------------------------------
#                  计数模型
# ---------------------------------------------
# 从网格编号还原xy数值坐标
F.add_xy_grids <- function(d.with_grid_id)
{
  v.grid_id <- d.with_grid_id$grid_id
  # 拆分x_0.5,y_0.5
  m.xy_lab <- str_split(v.grid_id,',',simplify = T)
  # 取出x_0.5
  v.x_lab <- m.xy_lab[,1]
  # 取出0.5
  v.grid_x <- as.numeric(str_split(v.x_lab,'_',simplify = T)[,2])
  # 取出y_0.5
  v.y_lab <- m.xy_lab[,2]
  # 取出0.5
  v.grid_y <- as.numeric(str_split(v.y_lab,'_',simplify = T)[,2])
  
  with_xy_grids.df <- cbind.data.frame(d.with_grid_id,
                                       grid_x = v.grid_x,
                                       grid_y = v.grid_y)
  return(with_xy_grids.df)
}

# 生成以DWMac_TID为分组的计数
F.count_by_DWMac_TID <- function(d.with_xy_grids)
{
  tab.count <- table(d.with_xy_grids$DWMac_TID)
  d.count <- data.frame(DWMac_TID = names(tab.count),
                        count = as.vector(tab.count))
  d.with_count <- merge(d.with_xy_grids,d.count,
                        by = 'DWMac_TID',all = F)
  return(d.with_count)
}

# 筛选同类结果,删除曼哈顿距离>3的样本
F.valid_DWMac_TID_filter <- function(d.with_DWMac_TID_count,
                                     manhattan_ = 3)
{
  # 筛选出计数>1的样本
  v.index_more_count <- d.with_DWMac_TID_count$count > 1
  d.only_one <- d.with_DWMac_TID_count[!v.index_more_count,]
  # 计数大于1的样本参与同类筛选
  d.more_count <- d.with_DWMac_TID_count[v.index_more_count,]
  # 对每个mac去重
  v.unique_DWMac_TID <- unique(d.more_count$DWMac_TID)
  # 对不重复的mac进行循环
  for(c.each_DWMac_TID in v.unique_DWMac_TID){
    m.xy <- d.more_count[d.more_count$DWMac_TID == c.each_DWMac_TID,
                         c('grid_x','grid_y')]
    # 删除最大manhattan距离大于设定值的样本
    if (max(dist(m.xy,'manhattan')) > manhattan_) {
      keep_rows <- which(d.more_count$DWMac_TID != c.each_DWMac_TID)
      d.more_count <- d.more_count[keep_rows,]
    }
  }
  # 纵向合并有效样本
  valid.d <- rbind.data.frame(d.more_count,d.only_one)
  return(valid.d)
}

#----------------------------------------------------
#         降噪
#----------------------------------------------------
F.denoise <- function(d.with_DWMac_TID_count,manhattan_ = 3)##?#输入的d.with_DWMac_TID_count是count之后的数据？
{
  grid_cfn <- unique(d.with_DWMac_TID_count$grid_CFN)
  # 检验grid_cfn是否唯一，不唯一则打回
  if (length(grid_cfn) != 1){
    print("grid_CFN must be unique !!")
    return()
  }
  # 只有一个点的直接保留
  d.only_one <- subset(d.with_DWMac_TID_count,
                       count == 1,select = - count)
  # 计数大于1的点需要汇聚，取平均
  d.more_count <- subset(d.with_DWMac_TID_count,
                         count > 1,
                         select = c('DWMac_TID','grid_x','grid_y'))
  x_new <- NULL
  y_new <- NULL
  DWMac_TID_vec <- NULL
  i <- 1
  unique_DWMac_TID <- unique(d.more_count$DWMac_TID)
  
  for(each_DWMac_TID in unique_DWMac_TID){
    df <- subset(d.more_count,
                 DWMac_TID == each_DWMac_TID,
                 select = - DWMac_TID)
    ordered_df <- df[order(df$grid_x,df$grid_y),]
    
    repeat{
      x0y0 <- ordered_df[1,]
      m_dist <- abs(ordered_df[,1] - x0y0[,1])+ 
        abs(ordered_df[,2] - x0y0[,2])
      m_dist_rows <- which(m_dist < manhattan_)
      
      if(length(m_dist_rows) > 1){
        pick_grids <- ordered_df[m_dist_rows,]
        xy_new <- apply(pick_grids,2,mean)
        x_new[i] <- xy_new[1]
        y_new[i] <- xy_new[2]
        DWMac_TID_vec[i] <- each_DWMac_TID
        ordered_df <- ordered_df[-m_dist_rows,]
        i <- i + 1
      }
      ordered_df <- ordered_df[-m_dist_rows,]
      
      if (nrow(ordered_df) == 0) break
    }
  }
  # 组合x、y和DWMac_TID
  combined_df <- data.frame(grid_x = x_new,
                            grid_y = y_new,
                            DWMac_TID = DWMac_TID_vec)
  # 求汇聚点所在的方格
  out.d <- within(combined_df,{
    grid_id <- str_c(str_c('x_',grid_x%/%1+0.5),',',
                     str_c('y_',grid_y%/%1+0.5))
    grid_CFN <- grid_cfn
  })
  out.d <- rbind.data.frame(d.only_one,out.d)
  return(out.d)
}

#--------------------------------------
# 障碍物所在格子数据的删除
##  编写每条记录所在格子是否有障碍的判断函数
f.logic.nonobstacle <- function(temp,x.cut  , y.cut  , floor_map_table){
  x.logic <- (abs(as.numeric(temp["grid_x"])*20 - floor_map_table$map_x) < x.cut )
  y.logic <- (abs(as.numeric(temp["grid_y"])*10 - floor_map_table$map_y) < y.cut )
  logic <- !any(x.logic & y.logic)
  return(logic)
}

##  封装行数判断整个表所在格式是否有障碍物
F.logic.nonobstacle <- function(data_denoise,x.cut=1,y.cut=0.5,floor_map_table){
  logic <- apply(data_denoise , 1 , f.logic.nonobstacle,x.cut=x.cut,y.cut=y.cut,floor_map_table=floor_map_table)
  data_denoise_nonobstacle <- data_denoise[logic,]
  # x zhou zuidazhi quchu   -----201701161500
  logic.d <- data_denoise_nonobstacle$grid_x < 10
  data_denoise_nonobstacle <- data_denoise_nonobstacle[logic.d,]
  return(data_denoise_nonobstacle)
}

#===================================================================================================
#HQL
####_______________________________________________________________________
#### 定义从MySQL读取数据的函数                            
####_______________________________________________________________________
#提取原始数据网格化坐标统计数据_____用途1：子函数；用途2；数据绘图
F.D.denoise <- function()
{
  sql <- '
  SELECT
  t1.DWMac_TID,
  t1.grid_id,
  t1.grid_x,
  t1.grid_y
  FROM
  data_denoise_nonobstacle AS t1
  '
  df <- F.extract_from_MySQL(host,port,dbname,user,
                             password,sql,max_obs)
  return(df)
}

#提取有效时间内的探针数据，用途计算逗留时长--注意条件约束中14800000 是当前实验环境的排错机制，后期需要调整
F.D.Mac_valid_second <- function(){
  sql <- '
  SELECT
  t1.T_Mac,
  t1.datetime,
  t1.WP_Mac,
  t2.T_Mac,
  t2.WP_Mac
  FROM
  wifi_data_warehouse_second AS t1 ,
  data_program_mac_valid AS t2
  WHERE
  t1.T_Mac = t2.T_Mac AND
  t1.WP_Mac = t2.WP_Mac AND
  t1.datetime > 1480000000
  '
  df <- F.extract_from_MySQL(host,port,dbname,user,
                             password,sql,max_obs)
  return(df)
}

#提取SQL提取设备品牌制造厂商数据函数
F.D.Mac_ltd <- function(){
  sql <-'
  SELECT
  t1.Mac_Basic_6,
  t1.Mac_Ltd
  FROM
  mac_mede_ltd_table AS t1
  '
  df <- F.extract_from_MySQL(host,port,dbname,user,
                             password,sql,max_obs)
  return(df)
}

#提交SQL提取校验设备的落点网格数据函数
F.D.Pr_correction <- function(){
  sql <-'
  SELECT
  t1.T_Mac,
  t1.grid_id,
  t1.WCD_Tstart,
  t1.WCD_Tstop,
  t1.grid_CFN,
  t1.grid_x,
  t1.grid_y
  FROM
  data_program_correction AS t1
  '
  df <- F.extract_from_MySQL(host,port,dbname,user,
                             password,sql,max_obs)
  df1 <- data.frame(
    map_x <- df$grid_x*20,
    map_y <- df$grid_y*10
  )
  colnames(df1)=c("x","y")
  df0 <- data.frame(df,df1)
  
  return(df0)
}

#MYsql导出项目所在楼层的地图数据  ---------  新增改动20170110
F.D.floor_map <- function(){
  sql0 <- 'SELECT
  t1.code_FN_map,
  t1.map_x,
  t1.map_y
  FROM
  floor_map_table AS t1
  WHERE
  t1.code_FN_map ='
  sql <- paste(sql0,"'",code_FN_map,"'",sep = "")
  df <- F.extract_from_MySQL(host,port,dbname,user,
                             password,sql,max_obs)
  return(df)
}

#MYSQL 导出项目探针所在点坐标                                          #20170116新增
F.D.pr_probe_location <- function(){
  sql0 <- '
  SELECT
*
  FROM
  data_program_probe_location AS t1
  WHERE
  t1.grid_CFN = '
  sql <- paste(sql0,"'",grid_CFN,"'",sep = "")
  df <- F.extract_from_MySQL(host,port,dbname,user,
                             password,sql,max_obs)
  df1 <- data.frame(
    map_x <- df$grid_x*20,
    map_y <- df$grid_y*10
  )
  colnames(df1)=c("x","y")
  df0 <- data.frame(df,df1)
  
  return(df0)
}

#————————————————————————————————————————————————————有改动20170110
#    数据操纵模块定义函数
#————————————————————————————————————————————————————
#————————————识别热力数据中的终端设备品牌厂商定义函数————————————
#将函数F.with_spreaded_count的DWMac_TID提取T_Mac,和函数F.Mac_ltd的MAC设备厂商编码（Mac_Basic_6)进行合并
F.with_mac_mede <- function(){
  df <- data.frame(
    df1 <- F.D.denoise(),
    apply(df1,1,function(x) substr(x["DWMac_TID"],1,12)),
    apply(df1,1,function(x) substr(x["DWMac_TID"],1,6))
  )
  #对提取的T_MAC列重命名
  colnames(df)=c(colnames(df1),"T_Mac","Mac_Basic_6")
  
  #合并筛选出手机厂商名称
  df1 <- merge(F.D.Mac_ltd(),df,by="Mac_Basic_6",all.y=1)
  df0 <- data.frame(
    x <- df1$grid_x*20,
    y <- df1$grid_y*10
  )
  #重命名标签名称便于合并筛选出校验数据时将原来信息标识出来
  colnames(df0)=c("x","y")
  #合并筛选出通过路径衰减模型转化的校验点分布坐标
  df2 <- data.frame(df1,df0)
  
  return(df2)
}

# ————————————筛选出校验终端设备路径转化结果————————————
#提取数据采集时录入的校验点网格终端设备标识和所在坐标网格坐标点(函数F.pr_correction)和已热力数据中识别的设备厂
#商数据（函数F.wifi_mac_ltd)结合
F.with_correction <- function(){
  df1 <- data.frame(
    x <- F.D.Pr_correction()[,5],
    y <- F.D.Pr_correction()[,1]
  )
  #重命名标签名称便于合并筛选出校验数据时将原来信息标识出来
  colnames(df1)=c("grid_CFN","T_Mac")
  #合并筛选出通过路径衰减模型转化的校验点分布坐标
  df <- merge(df1,F.with_mac_mede(),by="T_Mac")
  return(df)
}

#—————————————计算有效逗留时长————————————
#限定的是在一定时间段内有效终端MAC地址筛选出满足逗留时长内终端活动范围
F.with_stay_time <- function(stay_time){
  #有效MAC开始时间
  min_datetime_df <- aggregate(formula = datetime~T_Mac+datetime,
                               data = F.D.Mac_valid_second(),FUN = min)
  #有效MAC结束时间
  max_datetime_df <- aggregate(formula = datetime~T_Mac+datetime,
                               data = F.D.Mac_valid_second(),FUN = max)
  
  #拼接数据用于计算
  df1 <- merge(min_datetime_df,max_datetime_df,by="T_Mac")
  
  #计算获取10分钟=600秒，以上的有效终端MAC清单
  df2 <- subset(df1,{
    x<- df1[,3] 
    y<- df1[,2]
    x-y >= stay_time 
  })[,1:3]
  
  #更改df2列名
  names(df2) <- c("T_Mac","datetime_start","datetime_down")
  
  #合并筛选出逗留时长范围内的热点数据stay
  df <- merge(df2,F.with_mac_mede(),by="T_Mac")
  return(df)
}

#——————————————————————————————————————————————————————————————————————————————————————————————
#—————————————————————————————————————热力图绘制步骤———————————————————————————————————————————
#——————————————————————————————————————————————————————————————————————————————————————————————

#plot 绘图结果 保存路径及命名子函数  ________________20170112 new
F.save_plot_pach <- function(file_name){
  #定义文件夹路径和文件名 采用字符串接方式
  file_pach0 <- 'd:/data/heatmap/www/'
  file_formats <- '.png'
  pach <- paste(file_pach0,file_name,file_formats,sep="")
  return(pach)
}

#——————————————————————————————————————————————
#               数据热力呈现
#——————————————————————————————————————————————修改20170110
F.plot_all_dot <- function(){
  #——————————————————————————————————————————————————
  # 绘制热力图1（密度分布彩色）
  ##绘制
  ggplot(F.D.floor_map(),aes(x=map_x,y=map_y)) +
    stat_density2d(data=F.with_mac_mede(),aes(x,y,fill= ..density..),geom="raster",contour = F)+ 
    scale_fill_continuous(high='darkred',low='white')+
    geom_point()
  
  ##注意用width,height和dpi三个指标来控制生成的图片规格，9*80=720，所以生成的是720*720规格的图片，保存到创建文件夹
  file_name <- "all_1"
  ggsave(plot=, file = F.save_plot_pach(file_name),width = 9, height = 8,dpi=80)
  
  # 绘制热力图2(密度分布透视定位点)
  ggplot(F.D.floor_map(),aes(x=map_x,y=map_y)) +
    stat_density2d(data=F.with_mac_mede(),aes(x,y,fill= ..density..),geom="raster",contour = F)+ 
    scale_fill_continuous(high='darkred',low='white')+
    geom_point(data=F.with_mac_mede(),aes(x,y),colour= "green")+
    geom_point()
  
  ##注意用width,height和dpi三个指标来控制生成的图片规格，9*80=720，所以生成的是720*720规格的图片，保存到创建文件夹
  file_name <- "all_2"
  ggsave(plot=, file = F.save_plot_pach(file_name),width = 9, height = 8,dpi=80)
}

#_______________________________________修改20170110
#          逗留时长内的热力分布
#_______________________________________
F.plot_stay_dot <- function(){
  ggplot(F.D.floor_map(),aes(x=map_x,y=map_y)) +
    stat_density2d(data=F.with_stay_time(stay_time),aes(x,y,fill= ..density..),geom="raster",contour = F)+ 
    scale_fill_continuous(high='darkred',low='white')+
    geom_point()
  
  ##注意用width,height和dpi三个指标来控制生成的图片规格，9*80=720，所以生成的是720*720规格的图片，保存到创建文件夹
  file_name <- "stay_1"
  ggsave(plot=, file = F.save_plot_pach(file_name),width = 9, height = 8,dpi=80)
  
  #绘制热力图4（逗留时长内的密度分布图透视定位点）
  ggplot(F.D.floor_map(),aes(x=map_x,y=map_y)) +
    stat_density2d(data=F.with_stay_time(stay_time),aes(x,y,fill= ..density..),geom="raster",contour = F)+
    scale_fill_continuous(high='darkred',low='white')+
    # geom_text(data=F.with_stay_time(stay_time),aes(x,y+2,label=Mac_Ltd),colour= "green")+
    geom_point(data=F.with_stay_time(stay_time),aes(x,y),colour= "green")+                       #20170116 xinzeng shebeixinghao
    geom_point()
  
  ##注意用width,height和dpi三个指标来控制生成的图片规格，9*80=720，所以生成的是720*720规格的图片，保存到创建文件夹
  file_name <- "stay_2"
  ggsave(plot=, file = F.save_plot_pach(file_name),width = 9, height = 8,dpi=80)
}

#_______________________________________________________已修改20170111
#           校验数据和预设采集信息的落点校验
#———————————————————————————————————————————————————————
F.plot_correction_dot <- function(){
  ggplot(F.D.floor_map(),aes(x=map_x,y=map_y)) +
    geom_point(data=F.with_correction(),aes(x,y),colour= "green")+
    geom_text(data=F.with_correction(),aes(x,y+2,label=T_Mac),colour= "red")+
    geom_point()+
    geom_point(data=F.D.Pr_correction(),aes(x,y),colour= "blue",size=5)
  
  ##注意用width,height和dpi三个指标来控制生成的图片规格，9*80=720，所以生成的是720*720规格的图片，保存到创建文件夹
  file_name <- "correction_dot"
  ggsave(plot=, file = F.save_plot_pach(file_name),width = 9, height = 8,dpi=80)
  
  ggplot(F.D.Pr_correction(),aes(x,y)) +
  geom_point(colour= "green",size=5)+
  geom_point(data=F.D.floor_map(),aes(x=map_x,y=map_y))+
  geom_text(aes(x,y+2,label=T_Mac),colour= "green",size=5)

  
  ##注意用width,height和dpi三个指标来控制生成的图片规格，9*80=720，所以生成的是720*720规格的图片，保存到创建文件夹
  file_name <- "correction"
  ggsave(plot=, file = F.save_plot_pach(file_name),width = 9, height = 8,dpi=80)
}

#_________________________________________________
#       floor_map yuantu          20170110 new
#_________________________________________________
# floor_map   plot地图效果
F.plot_floor_map <- function(code_FN_map){
  file_name <- code_FN_map
  #绘制当前项目所在楼层底图
  ggplot(F.D.floor_map(),aes(x=map_x,y=map_y))+
    geom_point()
  ##注意用width,height和dpi三个指标来控制生成的图片规格，9*80=720，所以生成的是720*720规格的图片，保存到创建文件夹
  ggsave(plot=, file = F.save_plot_pach(file_name) ,width = 9, height = 8,dpi=80)
  
}
#_________________________________________________
#       wifi probe wifi探针坐标点          20170116 新增
#_________________________________________________
# program_probe   plot地图效果
F.plot_probe_dot <- function(grid_CFN){
  file_name <- grid_CFN
  #绘制当前项目所在楼层  探针分布图
  ggplot(F.D.pr_probe_location(),aes(x,y)) +
    geom_point(data=F.D.floor_map(),aes(x=map_x,y=map_y))+
    geom_point(colour= "blue",size=5)+
    geom_text(aes(x,y+2,label=WP_Mac),colour= "red",size=5)
  ##注意用width,height和dpi三个指标来控制生成的图片规格，9*80=720，所以生成的是720*720规格的图片，保存到创建文件夹
  ggsave(plot=, file = F.save_plot_pach(file_name) ,width = 9, height = 8,dpi=80)
  
}












