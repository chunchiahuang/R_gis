########## Using R as a GIS #############

# read raster files (e.g. *.tif, *.nc)

library(raster)

setwd("/Volumes/TRANSCEND 1/Climatic_data/cru_ts_3.24.01 2/pre") # load my storage
qq<-raster("cru_ts3.24.01.1901.2015.pre.dat.nc")    #讀cru的雨量資料(wrong way, 這個指令只能讀一層)
qq<-brick("cru_ts3.24.01.1901.2015.pre.dat.nc")     #讀cru的雨量資料(可以讀多層)
setwd("/Volumes/Transcend/Teaching/R_GIS/R_GIS")    # load my working space
rr<-crop(qq, extent(115,125,20,30))                 #把台灣的資料切出來
writeRaster(rr,"cru_pre_tw.tif","GTiff")            #輸出成新的GeoTiff檔

my.raster<-raster("C:/Users/farewell/Downloads/R_GIS/R_GIS/cru_pre_tw.tif")   #讀資料(只讀了一層)
my.raster<-brick("C:/Users/farewell/Downloads/R_GIS/R_GIS/cru_pre_tw.tif")    #讀資料(讀很多層)
my.raster                             #全部圖層的summary
my.raster[[1]]                        #第一層的summary
plot(my.raster[[1]])                  #畫第一層

# read vector files (e.g. *.shp)
library(maptools)
my.polygon <- readShapeSpatial("test.shp")

# extract values from a raster file
extract(my.raster,150)                                      # 以網格編號讀取資料
extract(my.raster,SpatialPoints(matrix(c(115.5,25.5),1,2))) # 以空間座標讀取資料
extract(my.raster,my.polygon)                               # 以多邊形讀取資料

########## Writing R functions #########

f1<-function(x){x^2+2*x+5}            #(範例)自己寫的超簡單小程式
f1(5)                                 #對一個對象(數字)執行程式

#lapply
aa<-seq(1:10)                         #產生一串數列
unlist(lapply(aa,f1))                 #對數列裡的每個數字執行相同的程式

#apply
pp<-matrix(my.raster[1],12,length(my.raster[1])/12) #產生月*年的雨量矩陣
apply(pp,1,max)                                     #挑出橫向最大值
apply(pp,2,max)                                     #挑出縱向最大值

# raster::calc                        #對raster做運算
my.calc<-calc(my.raster,f1)
my.raster[[1]]
my.calc[[1]]

## 範例：計算年間雨量變異度

#算法一：先算年平均雨量，再算平均值在115年間的變異程度
f2<-function(x){
  qq2<-matrix(x,12,length(x)/12)*10
  qq3<-sd(apply(qq2,2,mean),na.rm=T)
  return(qq3)
}

pre_var <- calc(my.raster, f2)
plot(pre_var)
# writeRaster(pre_var, "pre_var_among_year.tif", "GTiff")

# 另一種寫法
f3<-function(x){
  f2<-function(x){
    qq2<-matrix(x,12,length(x)/12)*10
    qq3<-sd(apply(qq2,2,mean),na.rm=T)
    return(qq3)
  }
  if(is.na(sum(my.raster[x]))==T){
    return(NA)
  }else{
    return(f2(my.raster[x]))
  }
}

pre_var3<-raster(my.raster, layer=0)
values(pre_var3)<-unlist(lapply(seq(1:400),f3))
plot(pre_var3)


#算法二：先算每個月份在115年間的變異程度，再取12個月份的平均
f4<-function(x){
  qq2<-matrix(x,12,length(x)/12)*10
  qq3<-mean(apply(qq2,1,sd),na.rm=T)
  return(qq3)
}

pre_var2 <- calc(my.raster, f4)
plot(pre_var2)


## 範例：埋葬蟲適合繁殖的溫度範圍
f_bb<-function(x){
  f1<-function(x,lower,upper){
    qq<-x
    qq[is.na(x)==F&x>=upper]<-0
    qq[is.na(x)==F&x<upper&qq>lower]<-1
    qq[is.na(x)==F&x<=lower]<-0
    return(qq)
  }
  qq2<-f1(x,lower=8.2,upper=21.7)
  return(qq2)
}




#########################################################


test<-brick(E:/CRU/cru_ts3.24.01.1901.2015.dtr.dat.nc)





