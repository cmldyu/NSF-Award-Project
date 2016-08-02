#提取主要变量
msg1<-msg[,1:27]#1-27是主要变量
msg1<-unique(msg1)#去重
library(lubridate)
#简化变量名称
#转换时间变量和数字变量
colname=colnames(msg1);colname<-gsub(x=colname,pattern = "Award\\.",replacement = "")
colnames(msg1)<-colname
msg1$AwardEffectiveDate<-as.Date(msg1$AwardEffectiveDate,"%m/%d/%Y")
msg1$AwardExpirationDate<-as.Date(msg1$AwardExpirationDate,"%m/%d/%Y")
msg1$Investigator.StartDate<-as.Date(msg1$Investigator.StartDate,"%m/%d/%Y")
msg1$MinAmdLetterDate<-as.Date(msg1$MinAmdLetterDate,"%m/%d/%Y")
msg1$MaxAmdLetterDateAmdLetterDate<-as.Date(msg1$MaxAmdLetterDate,"%m/%d/%Y")
msg1$AwardAmount<-as.integer(msg1$AwardAmount)
#处理机构名称
Organization.Directorate.LongName<-toupper(msg1$Organization.Directorate.LongName)
Organization.Directorate.LongName<-gsub(x=Organization.Directorate.LongName,
                                        pattern = "AND",replacement = "&")
Organization.Directorate.LongName<-gsub(x=Organization.Directorate.LongName,
                                        pattern = ",",replacement = "")
Organization.Directorate.LongName<-gsub(x=Organization.Directorate.LongName,
                                        pattern = "DIRECTORATE",replacement = "DIRECT")
Organization.Directorate.LongName<-gsub(x=Organization.Directorate.LongName,
                                        pattern = "DIRECT FOR COMPUTER & INFORMATION SCIENCE & ENGINEERING",replacement = "DIRECT FOR COMPUTER & INFO SCIE & ENGINR")
Organization.Directorate.LongName<-gsub(x=Organization.Directorate.LongName,
                                        pattern = "OFFICE OF INFORMATION & RESOURCE MANAGEMENT",replacement = "OFFICE OF INFORMATION & RESOURCE MGMT")
Organization.Directorate.LongName<-gsub(x=Organization.Directorate.LongName,
                                        pattern = "DIRECT FOR MATHEMATICAL & PHYSICAL SCIENCES",replacement = "DIRECT FOR MATHEMATICAL & PHYSICAL SCIEN")
Organization.Directorate.LongName<-gsub(x=Organization.Directorate.LongName,
                                        pattern = "DIRECT FOR SOCIAL BEHAVIORAL & ECONOMIC SCIENCES",replacement = "DIRECT FOR SOCIAL BEHAV & ECONOMIC SCIE")
msg1$Organization.Directorate.LongName=Organization.Directorate.LongName
#项目执行期
msg1$Duration<-round((msg1$AwardExpirationDate-msg1$AwardEffectiveDate)/365,1)
#项目所属董事会
barplot(table(msg1$Organization.Directorate.LongName))
#统计历年立项数
msg1$year<-year(msg1$AwardEffectiveDate)
msg2<-subset(msg1,year>=1950 & year<=2015)#选取NSF建立后到2015年的数据
a=ddply(msg2,c("year","Organization.Directorate.LongName"),summarise,freq=length(year))
ggplot(a,aes(x=year,y=freq,color=Organization.Directorate.LongName))+
  geom_line(stat="identity")+
  geom_point(size=0.8)+
  facet_wrap(~Organization.Directorate.LongName,scales = "free_y",ncol=3)+
  guides(color=F)
#资金支持(平均数)
a=ddply(msg2,c("year","Organization.Directorate.LongName"),summarise,freq=mean(AwardAmount))
ggplot(a,aes(x=year,y=freq,color=Organization.Directorate.LongName))+
  geom_line(stat="identity")+
  geom_point(size=0.8)+
  facet_wrap(~Organization.Directorate.LongName,scales = "free_y",ncol=3)+
  guides(color=F)
#合作类型
a=ddply(msg2,c("year","AwardInstrument.Value"),summarise,freq=length(AwardInstrument.Value))
ggplot(a,aes(x=year,y=freq,color=AwardInstrument.Value))+
  geom_line(stat="identity")+
  geom_point(size=0.8)+
  facet_wrap(~AwardInstrument.Value,scales = "free_y",ncol=4)+
  guides(color=F)
#执行期限
msg2$Duration<-round((msg2$AwardExpirationDate-msg2$AwardEffectiveDate)/365,2)
a=ddply(msg2,c("year","Organization.Directorate.LongName"),summarise,freq=mean(Duration))
ggplot(a,aes(x=year,y=freq,color=Organization.Directorate.LongName))+
  geom_line(stat="identity")+
  geom_point(size=0.8)+
  facet_wrap(~Organization.Directorate.LongName,scales = "free_y",ncol=4)+
  guides(color=F)+geom("Project Duration (y)")
