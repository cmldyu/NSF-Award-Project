#从NSF上下载的数据文件是若干独立的xml，需要批量读取到R中，整理成数据框

path = "J:\\研究助理\\国家自然基金项目\\NSF\\NSF"#设定文件路径

xml2dataframe=function(xmlfile){
  xmlfile<-xmlParse(xmlfile)#使用XML进行读取
  xmlfile = xmlRoot(xmlfile) #获取根节点的内容
  data.frame(t(unlist(xmlToList(xmlfile))),stringsAsFactors = F)
  #将XML转化成list，并将list转化成dataframe
  }
  
msg <- sapply(paste0(path,"\\",docs), function(p) xml2dataframe(p))
#批量处理目录中的xml

names(msg)<-""#删除文件名

msg=ldply(msg, as.data.frame)#将文本由sapply生成的list转化成dataframe
