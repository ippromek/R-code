# 09/22/2015
library("arules") ### attach package 'arules'
library("arulesViz")
library(dplyr)
netconn <- read.csv("~/Datasets/1.netconn.csv",stringsAsFactors = TRUE)

netconn<- graph_netconn%>%select(Hostname,Process_name,Username,Remote_IP_address)

netconn$Hostname<-as.factor(netconn$Hostname)
netconn$Process_name<-as.factor(netconn$Process_name)
netconn$Username<-as.factor(netconn$Username)
netconn$Remote_IP_address<-as.factor(netconn$Remote_IP_address)



netconn.transactions <- as(netconn, "transactions")
data<-netconn.transactions

f <- eclat(data, parameter = list(supp = 0.5, maxlen = 4, tidLists = TRUE))
## Get dimensions of the tidLists.
dim(tidLists(f))
## Coerce tidLists to list.
as(tidLists(f), "list")
## Inspect visually.
image(tidLists(f))
##Show the Frequent itemsets and respectives supports
inspect(f)
## Display the 5 itemsets with the highest support.
fsets.top5 <- sort(f)[1:5]
inspect(fsets.top5)

#------------- apriori -------------------------------------------------------
rules <- apriori(data, parameter = list(supp = 0.1, conf = 0.8))
rules
### visualize rules as a scatter plot (with jitter to reduce occlusion)
plot(rules, control=list(jitter=2))
### select and inspect rules with highest lift
rules_high_lift <- head(sort(rules, by="lift"), 5)
inspect(rules_high_lift)

### plot selected rules as graph
plot(rules_high_lift, method="graph", control=list(type="items"))
plot(rules_high_lift, measure=c("support", "lift"), shading="confidence")

sel <- plot(rules_high_lift, measure=c("support", "lift"), shading="confidence", interactive=TRUE)

plot(rules_high_lift, method="matrix", measure="lift")
plot(rules_high_lift, method="matrix", measure="lift", control=list(reorder=TRUE))
plot(rules_high_lift, method="matrix3D", measure="lift")
plot(rules_high_lift, method="matrix3D", measure="lift", control=list(reorder=TRUE))
plot(rules_high_lift, method="matrix", measure=c("lift", "confidence"))
plot(rules_high_lift, method="grouped")

plot(rules_high_lift, method="graph")
plot(rules_high_lift, method="graph", control=list(type="itemsets"))

plot(rules_high_lift, method="paracoord")
plot(rules_high_lift, method="paracoord", control=list(reorder=TRUE))

plot(rules_high_lift, method="doubledecker", data = data)
