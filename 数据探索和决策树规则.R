yonghu_kuanbiao_fk <- read.delim("~/Desktop/数亿惠/RdataAnylst/yonghu_kuanbiao_fk.csv")
yonghu_kuanbiao_yuqi_dy <- read.delim("~/Desktop/数亿惠/RdataAnylst/yonghu_kuanbiao_yuqi_dy")

library(tidyverse)
library(funModeling)
library(Hmisc)

yonghu_kuanbiao_yuqi_dy$fgood <- as.factor(yonghu_kuanbiao_yuqi_dy$fgood)

yonghu_kuanbiao_yuqi_dy$mer_user_id <- as.factor(yonghu_kuanbiao_yuqi_dy$mer_user_id)

basic_eda <- function(data){
  glimpse(data)
  df_status(data)
  freq(data)
  profiling_num(data)
  plot_num(data)
  describe(data)
}

#数据大概探索

df_status(yonghu_kuanbiao_yuqi_dy)

freq(yonghu_kuanbiao_yuqi_dy)

basic_eda(yonghu_kuanbiao_yuqi_dy)

basic_eda(yonghu_kuanbiao_yuqi_dy)

table(yonghu_kuanbiao_yuqi_dy$fgood)

library(sqldf)

df_fk_new <- sqldf(
  "
  select mer_user_id,
  fgood as target,
  loan_hour,
  register_hour,
  age,
  loan_app_cnt,
  user_contact_valid_count
  from yonghu_kuanbiao_yuqi_dy
  "
)


basic_eda(df_fk_new)

library(DataExplorer)

#数据区分探索
plot_density(df_fk_new)
plot_bar(df_fk_new)
plot_intro(df_fk_new)
plot_missing(df_fk_new)
plot_boxplot(lishiqingkuang,by = "target")


#离群值
library(dlookr)
liqunzhi <- diagnose_outlier(df_fk_new)

#数据质量报告
diagnose_report(df_fk_new,output_format = "html")


df_fk_new$fgood <- as.numeric(df_fk_new$fgood)

df_fk_new[is.na(df_fk_new)] <- -999

chi1 <- chiM(df_fk_new$loan_hour, alpha = 0.05)

library(smbinning)

df_fk_new <- df_fk_new

result1 <- smbinning(df = df_fk_new,y = "target",x = "loan_app_cnt")
result1
par(mfrow=c(2,2))
boxplot(df_fk_new$loan_app_cnt~df_fk_new$target,
        horizontal=TRUE, frame=FALSE, col="lightgray",main="Distribution")

mtext("Credit Score",3)

smbinning.plot(result1,option="dist",sub="Credit Score")


smbinning.plot(result1,option="badrate",sub="Credit Score")

smbinning.plot(result1,option="WoE",sub="Credit Score")

par(mfrow=c(1,1))


df_fk_new_woe <- sqldf(
  "
  select mer_user_id,
  case when fgood = 1 then 0 else 1 end as target,
  loan_hour,
  add_hour,
  age,
  city_per,
  loan_app_cnt,
  user_contact_valid_count
  from yonghu_kuanbiao_yuqi_dy
  "
)



write.csv(df_fk_new_woe,file = "/Users/mazhecheng/Desktop/数亿惠/RdataAnylst/yonghufenxi.csv")

library(dlookr)

plot_correlate(df_fk_new)


library(DataExplorer)


plot_correlation(df_fk_new)


library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

control <- rpart.control(minsplit=20,minbucket=10,maxdepth = 6) 

model<- rpart(df_fk_new$target~loan_hour+
                register_hour+
                age+
                loan_app_cnt+
                user_contact_valid_count, df_fk_new,
      method = "class",
      control = control)
      

model2<- rpart(df_fk_new_woe$target~loan_hour+
                add_hour+
                age+
                loan_app_cnt+
                user_contact_valid_count, df_fk_new,
              method = "class",
              control = control)


summary(model)


rpart.plot(model)
fancyRpartPlot(model)

asRules(model)

rpart.plot(model2)
fancyRpartPlot(model2)

df_rule <- asRules(model2)

model$cptable    #查看交叉验证结果，见图5
plotcp(model)    #查看交叉验证结果图，见图6
grid()           #加网格线，在这里不执行


conf.int=function(x,sigma,alpha) {
  mean=mean(x)
  n=length(x)
  z=qnorm(1-alpha/2,mean=0,sd=1,lower.tail = T)
  c(mean-sigma*z/sqrt(n),mean+sigma*z/sqrt(n))
}