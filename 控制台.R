#说明文档：
# 1.本脚本为总的控制台,前面主要是软件包和相关脚本的加载，后面是对模块函数的控制调用。
# 2.控制三个模块：原图数据转化模块、ETL模块、主程序模块。
# 3.参数说明：所有模块都需要数据库参数，写在了第二部分
#             每个模块分别需要自己的参数，写在每个模块的第一部分
#             每个模块的每个函数需要的参数，写在函数的调用中
#======================================================================================================
#-----------------------------------------------------
#     1.导入相关软件包和程序
#-----------------------------------------------------

# 导入相关程序
source("D:/data/R program/R_heatmap/数据库IO模块.R")
# source("D:/data/R program/R_heatmap/原图数据转化模块.R")
# source("D:/data/R program/R_heatmap/ETL模块.R")
source("D:/data/R program/R_heatmap/主程序模块1.1.R")#从三个主程序模块(1.1-1.3)选择一个


# #-----------------------------------------------------
# #     3.原图数据转化模块
# #-----------------------------------------------------
# 
# # 3.1该模块需要的参数
# code_FN_map <- "JRCSLFZ002M01"  #图的名字
# map_file_path.in <- "D:/data/wififile/mysql/DBMAP/"  #图txt文件所在路径
# map_file_path.out <- "D:/data/wififile/mysql/DBMAP/" #图csv文件要输出的路径
# 
# # 3.2对数据进行转化操作
# # txt数据清理并写入csv文件
# F.matrix.tranform(code_FN_map,map_file_path.in,map_file_path.out)
# # 读取清理后的csv文件
# a <- F.load_mapfile(map_file_path.out,code_FN_map)
# # 对读取图像数据进行操作并写入数据库
# F.into_floor_map_MySQL(a)
# # 
# #-----------------------------------------------------
# #     4.ETL模块
# #-----------------------------------------------------
# # 4.1定义本模块需要的参数
# dir0 <- 'D:/data/scandata_updata/scandata_20117011'
# 
# # 4.2获取存储地址和wp_macs
# D.wp_macs_file_path <- F.get_wp_macs_file_path(dir0 )
# 
# # 4.3汇总原始数据和分钟数据、秒钟数据
# D.raws <- F.integration_raws(D.wp_macs_file_path)
# D.max_ssi_bys <- F.maximum_by_second(D.raws)
# D.max_ssi_bym <- F.maximum_by_minute(D.raws)
# 
# # 4.4写入数据库  (test阶段慎用!!!)
# F.append_into_MySQL(conn,"wifi_data_raw",D.raws,append = T)
# F.append_into_MySQL(conn,"wifi_data_warehouse_minute",D.max_ssi_bys,append = T)
# F.append_into_MySQL(conn,"wifi_data_warehouse_second",D.max_ssi_bys,append = T)
# 

# #-----------------------------------------------------
#     5.主函数模块
#-----------------------------------------------------
# 5.1定义本模块需要的参数
time_inferior = "20161229143000"
time_superior = "20161229150000"
grid_CFN = "JRCSLFZ00220161229125000"
stay_time = 600

# 5.2生成相应的格子并写入数据库
grid_CFN.name = paste("'",grid_CFN,"'",sep="")
sql00<-"SELECT * FROM work_floor_grid_table WHERE grid_CFN = ";sql00<-paste(sql00,grid_CFN.name,sep="")
d.floor_info <- F.extract_from_MySQL(host,port,dbname,user,password,sql00,max_obs)
grid.D <- F.gridding(d.floor_info)
F.load_into_MySQL(host,port,dbname,user,password,"floor_grid_table",grid.D)

# 5.3按时间和地点提取数据，估计所在格子，并更新相应的表格
wifi.in.grid.D <- F.function.call(time_inferior,time_superior,grid_CFN,
                  type.num.initial=0.6035534,step.set.initial=5,arith.progress.set.initial=1)
    # 该函数的参数说明：
    # 参数依次为：开始时间；结束时间；项目地址编号；圆和方格相交的判定数（0.5，0.6035534，0.7071068）；
    # 等差数列的步长，是否是等差数列。（等比数列默认首相5，公比为1/1.1,到2时变为常数列,如果调节添加相应参数即可）

# 5.4计数模型
#计数
d.with_xy_grids <- F.add_xy_grids(wifi.in.grid.D)
d.with_DWMac_TID_count <- F.count_by_DWMac_TID(d.with_xy_grids) #计数统计
d.data_denoise  <- F.denoise(d.with_DWMac_TID_count,manhattan_ = 3) #降噪
d.similar_kind <- F.valid_DWMac_TID_filter(d.with_DWMac_TID_count,manhattan_ = 3)

#写入数据库
F.load_into_MySQL(host,port,dbname,user,password,"data_denoise",d.data_denoise)
    # 将降噪结果进行变形筛选方便并写入数据库方便验证
d.data_denoise_aggregated <- d.data_denoise 
d.data_denoise_aggregated$DWMac_TID <- apply(d.data_denoise,1,function(x) substr(x["DWMac_TID"],1,12))
d.temp <-  aggregate(d.data_denoise_aggregated[,c("grid_x","grid_y")],by = list(T_Mac = d.data_denoise_aggregated$DWMac_TID,grid_CFN = d.data_denoise_aggregated$grid_CFN),mean)
d.temp$grid_x <- d.temp$grid_x %/% floor_grid_length + 0.5
d.temp$grid_y <- d.temp$grid_y %/% floor_grid_length + 0.5
F.load_into_MySQL(host,port,dbname,user,password,"data_program_Tmac_mean",d.temp)

#_________________________
#20170116xinzeng
##  读取表格
sql <- "SELECT * FROM floor_map_table"
floor_map_table <- F.extract_from_MySQL(host,port,dbname,user,password,sql,max_obs)
data_denoise_nonobstacle <-F.logic.nonobstacle(d.data_denoise,x.cut=2,y.cut=1,floor_map_table)
F.load_into_MySQL(host,port,dbname,user,password,"data_denoise_nonobstacle",data_denoise_nonobstacle)


#(该函数overwrite只对已存在的这个表会报错，why？)
# 5.5绘图模型
#函数需要参数：
code_FN_map <- "JRCSLFZ002M01"
#从数据库读取数据
d.data_denoise <- F.D.denoise()#表1
d.mac_mede_ltd_table <- F.D.Mac_ltd()#表2
d.pr_correction <- F.D.Pr_correction()#表3
d.valid_second <- F.D.Mac_valid_second()#表4

# 绘图
#利用d.with_mac_mede绘制总的热力图
F.plot_all_dot()
#利用d.with_stay_time绘制逗留时间图
F.plot_stay_dot()
#利用d.with_correction绘制校验数据对应的热力图
F.plot_correction_dot()
# floor_map   plot地图效果
F.plot_floor_map(code_FN_map)
#probe_hot    项目探针所在的点位
F.plot_probe_dot(grid_CFN)


