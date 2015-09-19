#http://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
#http://www.r-bloggers.com/evaluating-logistic-regression-models/
options(java.parameters = "- Xmx1024m")

library(chron)
library(data.table)
library(DT)
library(dplyr)
library(vcd)
library(vcdExtra)
library(psych)
library(rattle)
library(extracat)
library(iplots)
library(ggplot2)
library(caret)
library(bitops)
library(lubridate)
library(vcd)
library(rpart)
library(rpart.plot)
library(partykit)
library(Formula)

netconn <- read.csv("~/Documents/Datasets/R-code/Datasets/1.netconn.csv", stringsAsFactors=TRUE)
dim(netconn)
netconn<-as.data.frame(netconn)


clean_dates <- function(x,y){
# y<-NULL
  x<-as.character(x)
  y<-gsub("T"," ",x)
  y<-gsub("Z","",y)
  y<-parse_date_time(y, 'Ymd HMS')
#  y<-as.POSIXlt(x,format = "%y-%m-%d H:M:S")
#  head(y,20)
  return(y)
}

extract_path<-function(x){
   c<-which(strsplit(x,"")[[1]]=="\\")
   return(substr(x, 1, max(c)))
}

jgc <- function()
{
  .jcall("java/lang/System", method = "gc")
} 

long2ip<-function(longip) {
  octet<-function(nbits)bitAnd(bitwShiftR(longip,nbits),0xFF)
  paste(Map(octet,c(24,16,8,0)),sep="",collapse=".")
  
}


#unique(sapply(head(netconn$Remote_IP_address,10), function(x) long2ip(x)))


# ---------------- ADD NEW COLUMNS BASED oN DATES AND TIME ----------------------------------
#netconn$Event_time1<-as.character(netconn$Event_time)
#netconn$Event_time1 <- chron(times=head(netconn$Event_time1))
#netconn$Event_time1<-as.POSIXct(netconn$Event_time1, format="%H:%M:%S")
#head(time(netconn$Event_time1))
#typeof(netconn$Event_time1)
#netconn$Event_time1<-ymd_hms(netconn$Event_time1, "%H:%M:%S")


netconn$start_date<-clean_dates(netconn$Start,netconn$start_date)
netconn$end_date<-clean_dates(netconn$Last_update,netconn$end_date)

#head(netconn$start_date)
#head(netconn$end_date)

netconn$time_diff<-round(difftime(netconn$end_date, netconn$start_date, units="days"),2)
#head(netconn$time_diff,20)

#--------------IP address ----------------------------------------------
netconn$Remote_IP<-as.character(sapply(netconn$Remote_IP_address, function(x) long2ip(x)))
head(netconn$Remote_IP)


# ------------ DATA SET EXPLORATIOn ------------------------------------
(vars<- names(netconn))
#dim(netconn)
#head(netconn)
summary(netconn)
#str(netconn)

#--------------------- Ignore IDs ------------------------------------------------
ignore<-c('Segment_id','Sensor_id','Process_pid','Uid','Parent_id','Parent_pid','Unique_id','Domain_name')

# -------------- Ignore constants ----------------------------------------
(constants<- names(which(sapply(netconn[vars], function(x) all(x == x[1L])))))
ignore <- union(ignore, constants)          

#--------------------- Remove old date columns -----------------------------------
(ignore <- union(ignore, c('Start','Last_update')))
## ------------Initialise the variables -----------------------------------------
(vars <- setdiff(vars, ignore))
(inputi <- sapply(vars, function(x) which(x == names(netconn)), USE.NAMES=FALSE))
netconn<- netconn[,inputi]
#head(netconn)
#describe(netconn)
# ---------------- list of factors ------------------------------------
netconn$Event_time<-as.character(netconn$Event_time)
netconn$Path<-as.character(netconn$Path)
netconn$Remote_port<-as.integer(netconn$Remote_port)

(factors<- which(sapply(netconn[vars], is.factor)))
(lvls <- sapply(factors, function(x) length(levels(netconn[[x]]))))

# --------------- Normalise factors ----------------------------------
(factors     <- which(sapply(netconn[vars], is.factor)))
 for (f in factors) levels(netconn[[f]]) <- normVarNames(levels(netconn[[f]]))


#---------------- filter out factors with more tnan 20 -----------------
#(many  <- names(which(lvls > 20)))

# --------------- Extract Path ---------------------------------------
netconn$Path_new<-extract_path(netconn$Path)

# -------------- start count metrics calculation -----------------------
DT::datatable(netconn, filter = 'top', options = list(pageLength = 5, autoWidth = TRUE))



netconn.select <- netconn %>% dplyr::select(Hostname,Process_name,Remote_IP_address,Remote_port)

xtabs(~ Username+Host_type + Os_type, data = netconn)


(HEC <- vcd::structable(~Username+ +Host_type+Os_type, data = netconn))
mosaic(HEC)
mosaic(xtabs( ~ Os_type+Username, data = netconn))
rmb(~ Os_type + Host_type, data = netconn)
doubledecker(xtabs(~ Os_type + Host_type, data = netconn))

attach(netconn)
ihist(Host_type)
iplot(Host_type, Username)
imosaic(Os_type,Host_type)
ipcp(Os_type,Host_type,Username)
detach(netconn)

user.20<-names(summary(Username))[1:10]

ggplot(data=subset(netconn,Username %in% user.20),aes(x=reorder(Username,Username,length)))+geom_bar()+coord_flip()
       

mylogit <- glm(Os_type ~ Host_type, data = head(netconn,1000), family = "binomial")

ggplot(netconn, aes(x=Os_type, y=Host_type)) + geom_point() + 
  stat_smooth(method="glm", family="binomial", se=FALSE)



pred = predict(mylogit, newdata=head(netconn,1000))
accuracy <- table(pred, head(netconn,1000)[,"Os_type"])



plot(netconn$Os_type, netconn$Host_type)
curve(predict(mylogit, data.frame(x=netconn$Os_type), type="response"), add=TRUE) 

               
colnames(netconn)



netconn.mod1 <- loglm(Remote_port~ Hostname+Remote_IP_address, data=head(netconn.select,20))
netconn.mod1
plot(netconn.mod1, main="Model [AGC][S]")



netconn.group_by<-netconn.select %>% 
                              dplyr:: group_by(Hostname,Process_name,Remote_port) %>%
                              dplyr::summarize(RemoteIPs=n_distinct(Remote_IP_address))  %>% as.data.frame
                                            #Username,Path_new,Host_type,Os_type,Remote_IP_address,Remote_port,Protocol)
head(netconn.select)

structable(head(netconn.select))
#typeof(as.table(netconn.select))
#datatable(netconn.select)
sapply(dimnames(netconn.select), length)
ftable(structable(head(netconn.select)))

mosaic(structable(head(netconn.select)))

mosaic(HairEyeColor, shade=TRUE, legend=TRUE)
summary(HairEyeColor)
class(HairEyeColor)
nrow(HairEyeColor)

library(MASS)
data(Titanic, package="datasets")  # effects::Titanic gives a case-form version

Titanic <- Titanic + 0.5   # adjust for 0 cells
titanic.mod1 <- loglm(~ (Class * Age * Sex) + Survived, data=Titanic)
titanic.mod1
plot(titanic.mod1, main="Model [AGC][S]")

titanic.mod2 <- loglm(~ (Class * Age * Sex) + Survived*(Class + Age + Sex), data=Titanic)
titanic.mod2
plot(titanic.mod2,  main="Model [AGC][AS][GS][CS]")



phone_numbers <- daily_fraud %>%
  filter(phone !='NA') %>%
  group_by(customer_id) %>%
  summarize(phones=n_distinct(phone)) %>%
  arrange(desc(phones)) %>%  
  mutate(le_rank = min_rank(desc(phones))) %>%
  filter(le_rank<=localTopN)  %>%
  arrange(desc(phones))



## 


table(netconn$Uid)

set.seed(999)
ind <- sample(2, nrow(train), replace=T, prob=c(0.60,0.40))
trainData<-train[ind==1,]
testData <- train[ind==2,]
