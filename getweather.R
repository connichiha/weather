weathermain <- function()
{
    # prepare data
    date <- Sys.time()  # 选择日期
    loadDate(date)   # 爬取数据
    
    # show map
    library(maps)
    library(mapdata)
    library(maptools)
    map<-readShapePoly('mapdata/bou2_4p.shp')  # 加载中国行政区地图数据
    plot(map)
    
    # 数据可视化
    library(RColorBrewer)
    
}

getWeather<-function (x)
{
    url<-paste('http://weather.yahooapis.com/forecastrss?w=',x,'&u=c',sep="")       # yahoo的数据源地址
    doc = xmlTreeParse(getURL(url),useInternal = TRUE)                              # 解析XML文档
    
    ans<-getNodeSet(doc, "//yweather:atmosphere")
    humidity<-as.numeric(sapply(ans, xmlGetAttr, "humidity"))                       # 温度
    visibility<-as.numeric(sapply(ans, xmlGetAttr, "visibility"))                   # 能见度
    pressure<-as.numeric(sapply(ans, xmlGetAttr, "pressure"))                       # 气压
    rising<-as.numeric(sapply(ans, xmlGetAttr, "rising"))                           # 气压变动
    
    ans<-getNodeSet(doc, "//item/yweather:condition")
    code<-sapply(ans, xmlGetAttr, "code")                                           # 天气情况
    
    ans<-getNodeSet(doc, "//item/yweather:forecast[1]")
    low<-as.numeric(sapply(ans, xmlGetAttr, "low"))                                 # 最高气温
    high<-as.numeric(sapply(ans, xmlGetAttr, "high"))                               # 最低气温
    
    print(paste(x,'==>',low,high,code,humidity,visibility,pressure,rising))
    cbind(low,high,code,humidity,visibility,pressure,rising)                        # 以data.frame格式返回
}

# 文件根据日期来命名
filename<-function(date=Sys.time())
{           
    paste(format(date, "%Y%m%d"),".csv",sep="")
}
# 读取城市列表，调用爬虫函数，合并数据保存到一个文件中。
loadDate<-function(date = Sys.time())
{
    library(XML)
    library(RCurl)
    #date <- Sys.time()
    print(paste('Date','==>',date))
    city<-read.csv(file="WOEID.csv",header=FALSE,fileEncoding="utf-8", encoding="utf-8")  # 加载城市列表
    names(city)<-c("en","woeid","zh",'prov','long','lat')
    #city<-city[-nrow(city),]
    
    wdata<-do.call(rbind, lapply(city$woeid,getWeather))
    w<-cbind(city,wdata)
    write.csv(w,file=filename(date),row.names=FALSE,fileEncoding="utf-8")
}

getColors2<-function(map,prov,ctype){
         #name change to ADCODE99
     ADCODE99<-read.csv(file="ADCODE99.csv",header=TRUE,fileEncoding="utf-8", encoding="utf-8")
     fc<-function(x){ADCODE99$ADCODE99[which(x==ADCODE99$prov)]}
     code<-sapply(prov,fc)
     
     f=function(x,y) ifelse(x %in% y,which(y==x),0);
     colIndex=sapply(map$ADCODE99,f,code);
     ctype[which(is.na(ctype))]=19
     return(ctype[colIndex])
 }


summary<-function(data=data,output=FALSE,path='')
{
    colors<-c(rev(brewer.pal(9,"Blues")),rev(c('#b80137','#8c0287','#d93c5d','#d98698','#f6b400','#c4c4a7','#d6d6cb','#d1b747','#ffeda0')))    # 定义18种天气特征对应的颜色
    
    temp<-data$code
    title<-"中国各省天气概况"
    ofile<-paste(format(date,"%Y%m%d"),"_code.png",sep="")
    sign<-''
    colors<-rev(colors)
    code<-read.csv(file="code.csv",header=TRUE,fileEncoding="utf-8", encoding="utf-8")
    labelcode<-read.csv(file="labelcode.csv",header=TRUE,fileEncoding="utf-8", encoding="utf-8")
    ctype<-sapply(temp,function(x){code$type[which(x==code$code)]})
    if(output)png(file=paste(path,ofile,sep=''),width=600,height=600)
    layout(matrix(data=c(12),nrow=1,ncol=2),widths=c(8,1),heights=c(1,2))
    par(mar=c(0,0,3,12),omac(0.2,0.2,0.2,0.2),mex=0.3)
    plot(map, border = "white",col = colors[getColors2(map,data$prov,ctype)] )     # 地图和天气可视化
         points(data$long,data$lt,pch=19,col=rgb(0,0,0,0.3),cex=0.8)                     # 标出采样城市
         # =======================================          # 图片中的辅助文字
         if(FALSE){
         grid()
         axis(1,lwd=0);axis(2,lwd=0);axis(3,lwd=0);axis(4,lwd=0)
         }
         text(100,58, title,cex=2)
         text(100,54,format(date,"%Y-%m-%d"))
         text(98,65,paste('每日中国天气','http://apps.weibo.com/chinaweatherapp'))
         text(120,-8,paste('provided by The Weather Channel',format(date, "%Y-%m-%d %H:%M")),cex=0.8)
         
         #=======================================          # 文字说明
         for(row in 1:nrow(data)){
         name<-as.character(data$zh[row])
         label<-labelcode$alias[labelcode$type==ctype[row]]
         x1<-ceiling(row/7)
         x2<-ifelse(row%%7==0,7,row%%7)
         x3<-ctype[row]
         fontCol<-'#000000'
         if(x3<=5)fontCol<-head(colors,1)
         if(x3>=12)fontCol<-tail(colors,1)
         text(68+x1*11,17-x2*3,paste(name,' ',label,sign,sep=''),col=fontCol)
         }
         
         #=======================================          # 图例
         par(mar = c(5, 0, 15, 10))
         image(x=1, y=1:length(colors),z=t(matrix(1:length(colors))),col=rev(colors),axes=FALSE,xlab="",ylab="",xaxt="n")
         axis(4, at = 1:(nrow(labelcode)-1), labels=rev(labelcode$alias)[-1], col = "white", las = 1)
         abline(h=c(1:(nrow(labelcode)-2)+0.5), col = "white", lwd = 2, xpd = FALSE)
         if(output)dev.off()
}
