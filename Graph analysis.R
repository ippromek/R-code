#http://kateto.net/network-visualization
#install.packages("igraph")
#install.packages("network") 
#install.packages("sna")
#install.packages("ndtv")
#install.packages("extrafont")
#install.packages('intergraph')
#install.packages("networkD3")

library(igraph)
library(network)
library(sna)
library(ndtv)
library(bitops)
library(extrafont)
library(dplyr)
library(intergraph)
library(networkD3)

long2ip<-function(longip) {
  octet<-function(nbits)bitAnd(bitwShiftR(longip,nbits),0xFF)
  paste(Map(octet,c(24,16,8,0)),sep="",collapse=".")
  
}

graph_netconn <- read.csv("~/Datasets/1.netconn.csv",stringsAsFactors = FALSE)

graph_netconn<-subset(graph_netconn,graph_netconn$Hostname=='W7VM0980-ANCL') 
                      #| graph_netconn$Hostname=='ajlcd-irrhel02')
#graph_netconn<-subset(graph_netconn, graph_netconn$Hostname=='ajlcd-irrhel02')
unique(graph_netconn$Hostname)
#graph_netconn<-subset(graph_netconn,graph_netconn$Hostname=='ajlcd-irrhel02')
#graph_netconn<-subset(graph_netconn,graph_netconn$Hostname=='PBNKWER')
nrow(graph_netconn)

graph_netconn.outbound<-subset(graph_netconn,graph_netconn$Connection_type=='TRUE')
nrow(graph_netconn.outbound)

graph_netconn.inbound<-subset(graph_netconn,graph_netconn$Connection_type=='FALSE')
nrow(graph_netconn.inbound)

#graph_netconn.outbound.edges<-graph_netconn.outbound%>%select(Hostname,Remote_IP_address)
graph_netconn.outbound.edges<-graph_netconn.outbound%>%select(Process_name,Remote_IP_address)
graph_netconn.outbound.edges$Count<-1
#--------------- clean ip --------------
colnames(graph_netconn.outbound.edges)<-c('from','to','weight')
graph_netconn.outbound.edges$to<-as.character(sapply(graph_netconn.outbound.edges$to, function(x) long2ip(x)))
head(graph_netconn.outbound.edges)
# -----------unique hostnames --------------------------
graph_netconn.outbound.vertices1<-as.data.frame(as.character(unique(graph_netconn.outbound.edges$from)))
colnames(graph_netconn.outbound.vertices1)<-c("vertices")
nrow(graph_netconn.outbound.vertices1)

#------------ unique remote ip --------------------------
graph_netconn.outbound.vertices2<-as.data.frame(as.character(unique(graph_netconn.outbound.edges$to)))
colnames(graph_netconn.outbound.vertices2)<-c("vertices")
nrow(graph_netconn.outbound.vertices2)

# --------- final list of vertices -----------------------
graph_netconn.outbound.vertices<-rbind(graph_netconn.outbound.vertices1,graph_netconn.outbound.vertices2)
nrow(graph_netconn.outbound.vertices)

#----- aggreagte multiple links between the same two nodes --------------------------
graph_netconn.outbound.edges <- aggregate(graph_netconn.outbound.edges[,3], graph_netconn.outbound.edges[,-3], sum)
graph_netconn.outbound.edges <- graph_netconn.outbound.edges[order(graph_netconn.outbound.edges$from, graph_netconn.outbound.edges$to),]
head(graph_netconn.outbound.edges)

# ------------ grpah visualization ------------------------------------------------
graph_netconn.outbound.edges<-na.omit(graph_netconn.outbound.edges)
graph_netconn.outbound.vertices<-na.omit(graph_netconn.outbound.vertices)
nrow(graph_netconn.outbound.edges)
nrow(graph_netconn.outbound.vertices)

head(graph_netconn.outbound.vertices)
head(graph_netconn.outbound.edges)

net <- graph.data.frame(graph_netconn.outbound.edges, graph_netconn.outbound.vertices, directed=T)
#net
#E(net)       # The edges of the "net" object
#V(net)   
plot(net, edge.arrow.size=.4,vertex.size=15, vertex.label.family="Arial Black")
# - remove loops --------------------------
net <- simplify(net, remove.multiple = F, remove.loops = T) 

E(net)$width <- 1+E(net)$x/12

plot(net, layout=layout.fruchterman.reingold, vertex.size=4,
     vertex.label.dist=0.5, vertex.color="red", edge.arrow.size=0.5,vertex.label.family="Arial Black")


# We could also use the audience size value:
V(net)$size <- V(net)$count
head(V(net))

E(net)$width <- E(net)$x
  class(E(net)$width)


edge.start <- igraph::get.edges(net, 1:ecount(net))[,1]
edge.col <- V(net)$color[edge.start]
plot(net, edge.color=edge.col, edge.curved=.1)  


l <- layout.fruchterman.reingold(net, repulserad=vcount(net)^3, 
                                 area=vcount(net)^2.4)

plot(net, edge.arrow.size=.5, edge.color="orange", 
     #edge.label=E(net)$x,
     vertex.label.cex=0.8,
     vertex.size=7,
      vertex.frame.color="#ffffff", 
     vertex.label.dist=0.5, vertex.label.color="black",edge.color=edge.col, edge.curved=.1) 

hist(as.integer(E(net)$x))
mean(E(net)$x)
sd(E(net)$x)

V(net)$community <- optimal.community(net)$membership
colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)
plot(net, vertex.color=colrs[V(net)$community])


dd <- degree.distribution(net, cumulative=T, mode="all")
plot(dd, pch=19, cex=1, col="orange", xlab="Degree", ylab="Cumulative Frequency")

head(graph_netconn.outbound.edges)
head(graph_netconn.outbound.vertices)

data(MisLinks)
data(MisNodes)

head(MisLinks)
head(MisNodes)



forceNetwork(Links = graph_netconn.outbound.edges, Nodes =graph_netconn.outbound.vertices ,
             Source = "from", Target = "to",
             Value = "x", NodeID = "vertices",
             Group = "vertices", 
             opacity = 0.8)


sankeyNetwork(Links = graph_netconn.outbound.edges, Nodes = graph_netconn.outbound.vertices,
              Source = "from", Target = "to",
              Value = "x", NodeID = "vertices",
              width = 700, fontSize = 12, nodeWidth = 30)



forceNetwork(Links = MisLinks, Nodes = MisNodes,
             Source = "source", Target = "target",
             Value = "value", NodeID = "name",
             Group = "name", 
             opacity = 0.8)


forceNetwork(Links = graph_netconn.outbound.edges, Nodes = graph_netconn.outbound.vertices, Source="from", Target="to",
             #NodeID = "idn", Group = "type.label",
             linkWidth = 1,
             linkColour = "#afafaf", fontSize=12, zoom=T, legend=T,
             Nodesize=6, opacity = 0.8, charge=-300, 
             width = 600, height = 400)


tkid <- tkplot(net,vertex.size=10,vertex.color="green") #tkid is the id of the tkplot that will open
l <- tkplot.getcoords(tkid) # grab the coordinates from tkplot
plot(net, layout=l)


coords <- layout.fruchterman.reingold(net, dim=3)
rglplot(net, layout=coords,vertex.color="green")


