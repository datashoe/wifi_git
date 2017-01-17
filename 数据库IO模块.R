#脚本说明：
#   本脚本主要定义了数据库的读取、写入函数，其中写入分为了两种一种是overwrite模式
# 覆盖原有的表，另一种是append模式在表中添加新的记录。
# ------------------------------------------------
#              读写MySQL数据库
# ------------------------------------------------

#-----------------------------------------------------
#     1.导入相关软件包
#-----------------------------------------------------

# 导入软件包
pkgs_name <- c("DBI","RMySQL","ggplot2","plyr","readr","stringr","lubridate")
for (pkg in pkgs_name) 
  library(pkg,character.only = T)

#-----------------------------------------------------
#     2.定义数据库连接参数
#-----------------------------------------------------

#MYSQL  valurs   数据库连接参数
host <- "localhost"
user <- "root"
password <- "123456"
dbname <- "wifi_test_db2"
port <- 3306
max_obs <- 1000000

#_________________________________________________

# 定义写入数据库到MySQL的函数(overwrite格式)
F.load_into_MySQL <- function(host,port,dbname,user,
                              password,table_name,df)
{
  # 定义数据库链接
  conn <- dbConnect(MySQL(),host = host,port = port,
                    dbname = dbname,user = user,
                    password = password)
  # 写入数据库
  dbWriteTable(conn,name = table_name,value = df,
               row.names = F,overwrite = TRUE)
  # 断开数据库链接
  dbDisconnect(conn)
}

#定义全局写入数据库函数 （append模式）    于20170110添加
F.append_into_MySQL <- function(host,port,dbname,user,
                                password,table_name,df)
{
  # 定义数据库链接
  conn <- dbConnect(MySQL(),host = host,port = port,
                    dbname = dbname,user = user,
                    password = password)
  # 写入数据库
  dbWriteTable(conn,name = table_name,value = df,
               row.names = F,append = T)
  # 断开数据库链接
  dbDisconnect(conn)
}

# 定义从MySQL读取数据的函数
F.extract_from_MySQL <- function(host,port,dbname,user,
                                 password,sql,max_obs)
{ 
  # 定义数据库引擎
  drv <- MySQL(fetch.default.rec = max_obs)
  conn <- dbConnect(drv,host = host,port = port,
                    dbname = dbname,user = user,
                    password = password)
  # 获取查询结果
  res <- dbSendQuery(conn,sql)
  df <- dbFetch(res)
  
  # 清空资源、断开数据库链接
  dbClearResult(res)
  dbDisconnect(conn)
  
  return(df)
}

