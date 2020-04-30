# library -----------------------------------------------------------------
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(forcats)
library(car)
library(ggplot2)


# data import -------------------------------------------------------------
data<-read_csv("~/data_science_project/data/2019.06~2019.12.csv")
glimpse(data)
tail(data)
names(data)
data$X1<-data$X1+1
names(data)[1]<-"id"

#data의 song의 id 만들기
table(duplicated(data[,c("name","artist")]))  
names(data)

only1<-data[which(!duplicated(data[,c("name","artist")])),] #duplicated_none
only1$song_id<-c(1:686)
only1<-subset(only1,select=c("name","artist","song_id"))
data<-left_join(data,only1,by=c("name","artist"))

# rank --------------------------------------------------------------------
data$rank_g<-ifelse(data$rank==c(1:10),1,
                    ifelse(data$rank==c(11:20),2,
                           ifelse(data$rank==c(21:30),3,
                                  ifelse(data$rank==c(31:40),4,
                                         ifelse(data$rank==c(41:50),5,
                                                ifelse(data$rank==c(51:60),6,
                                                       ifelse(data$rank==c(61:70),7,
                                                              ifelse(data$rank==c(71:80),8,
                                                                     ifelse(data$rank==c(81:90),9,
                                                                            ifelse(data$rank==c(91:100),10,
                                                                                   ifelse(data$rank==c(101:110),11,
                                                                                          ifelse(data$rank==c(111:120),12,
                                                                                                 ifelse(data$rank==c(121:130),13,
                                                                                                        ifelse(data$rank==c(131:140),14,
                                                                                                               ifelse(data$rank==c(141:150),15,
                                                                                                                      ifelse(data$rank==c(151:160),16,
                                                                                                                             ifelse(data$rank==c(161:170),17,
                                                                                                                                    ifelse(data$rank==c(171:180),18,
                                                                                                                                           ifelse(data$rank==c(181:190),19,20)))))))))))))))))))





# change to weeks variable ----------------------------------------------------
data<-data %>% separate(time,c("st_day","ed_day"),"~") 

data$st_day<-ymd(data$st_day)
data$ed_day<-ymd(data$ed_day)

data<- data %>% group_by(year(st_day)) %>% group_by(month(st_day)) %>% group_by(day(st_day))
names(data)
names(data)[15]<-"yr"
names(data)[16]<-"mon"
names(data)[17]<-"day"
data<-data %>% group_by(yr) %>% group_by(mon) %>% mutate(num=dense_rank(day)) 
data$yr<-substr(data$yr,3,4)
data$mon<-ifelse(str_count(data$mon)==1,paste0(0,data$mon),data$mon)
data$wks<-paste0(data$yr,data$mon,"_",data$num,"wk") #In what weeks of the month?

# sex ---------------------------------------------------------------------
data$sex<- fct_recode(data$sex,male="남성",female="여성",mixed="혼성")

ggplot(data,aes(x=sex))+
  geom_bar(fill=c("sky blue","pink","orange"))

ggplot(data,aes(x=sex,y=rank))+
  geom_boxplot(fill=c("sky blue","pink","orange"))

# production --------------------------------------------------------------정리안함
dim(addmargins(table(data$production)))

# distributor -------------------------------------------------------------
dim(addmargins(table(data$distributor)))


# genre -------------------------------------------------------------------
table(data$genre)
A<-str_split_fixed(data$genre,"/ ",n=2)
A<-data.frame(A)
names(A)[1]<-"type"
names(A)[2]<-"gen"
data<-bind_cols(data,A)

mode(data$type)
table(data$type)
data$type

data$type<-fct_recode(data$type,K_POP="가요 ",POP="POP ",OST="OST ",Others="그외장르 ") 
data$type<-fct_relevel(data$type,c("K_POP","POP","OST","Others"))
addmargins(table(data$type))

addmargins(table(data$gen))


data$genre_g<-recode(data$gen,"'댄스'='Dance_etc';'락'='Dance_etc';'랩/힙합'='Dance_etc';'일렉트로니카'='Dance_etc';'캐롤'='Dance_etc';'트로트'='Dance_etc';
                              '발라드'='Balad_etc';'블루스/포크'='Balad_etc';'R&B/소울'='Balad_etc';'인디'='Balad_etc';
                              '드라마'='Others';'애니메이션/게임'='Others';'팝'='Others';'해외영화'='Others';'전체'='Others'")


names(data)
str(data)

ggplot(data,aes(x=type))+
  geom_bar(fill=c("LightSky Blue","pink","violet","orange"))

ggplot(data,aes(x=type,y=rank))+
  geom_boxplot(fill=c("LightSky Blue","pink","violet","orange"))

ggplot(data,aes(x=genre_g))+
  geom_bar(fill=c("LightSky Blue","pink","orange"))

ggplot(data,aes(x=genre_g,y=rank))+
  geom_boxplot(fill=c("LightSky Blue","pink","orange"))

# runtime -----------------------------------------------------------------
data$runtime<-substr(data$runtime,1,5)
data$runtime<-as.numeric(substr(data$runtime,1,2))*60+as.numeric(substr(data$runtime,4,5))

a<-data %>% arrange(runtime) %>% mutate(qaun_runtime=ntile(runtime,2))
a<-subset(a,select=c(runtime,qaun_runtime))

percent_rank(data$runtime)
min_rank(data$runtime)

summary(data$runtime)
data$runtime_g<-as.factor(ifelse(data$runtime<=199,"Short",
                          ifelse(data$runtime>=241,"Long","Middle")))


ggplot(data,aes(x=runtime))+
  geom_histogram(fill="#F8766D",color="black",binwidth = 3.5)

ggplot(data,aes(x=runtime_g,y=rank))+
  geom_boxplot(fill="#F8766D",color="black")


# active_type -------------------------------------------------------------
data$active_type<-factor(data$active_type,levels=c("솔로","그룹","밴드","프로젝트","듀엣"),
                         labels=c("Solo","Group","Band","Project","Duet"))

table(data$active_type)

data$active_type_g<-as.factor(ifelse(data$active_type=="Solo","Solo","Non_solo"))


ggplot(data,aes(x=active_type))+
  geom_bar(fill=c("#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF"))

ggplot(data,aes(x=active_type_g))+
  geom_bar(fill=c("#F46D43","#D73027"))

ggplot(data,aes(x=active_type,y=rank))+
  geom_boxplot(fill=c("#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF"))

ggplot(data,aes(x=active_type_g,y=rank))+
  geom_boxplot(fill=c("#F46D43","#D73027"))


# 200위안에 6개월동안 몇주동안 진입하였는지? ----------------------------------------------------------
data$song_id<-factor(data$song_id)
a<-table(data$song_id)
A<-data.frame(a)
names(A)[1]<-"song_id"
names(A)[2]<-"song_freq"

ggplot(A,aes(x=song_freq))+
  geom_histogram(fill="#F8766D",color="black",binwidth = 1)

summary(A$song_freq)

A$song_freq_g<-factor(ifelse(A$song_freq<=2,"Low",
                      ifelse(A$song_freq>=13,"High","Middle")))
A$song_freq_g<-fct_relevel(A$song_freq_g,"High","Middle","Low")

data<-left_join(data,A,by="song_id")

ggplot(data,aes(x=song_freq_g))+
  geom_bar(aes(fill=genre_g))

ggplot(data,aes(x=song_freq_g))+
  geom_bar(aes(fill=sex))

ggplot(data,aes(x=song_freq_g))+
  geom_bar(aes(fill=runtime_g))

ggplot(data,aes(x=song_freq_g))+
  geom_bar(aes(fill=active_type_g))
