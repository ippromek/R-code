graph_netconn <- read.csv("~/Datasets/1.netconn.csv",stringsAsFactors = FALSE)

str(graph_netconn)

graph_netconn<-subset(graph_netconn,graph_netconn$Hostname=='W7VM0980-ANCL') 
#        | graph_netconn$Hostname=='ajlcd-irrhel02')
#graph_netconn<-subset(graph_netconn, graph_netconn$Hostname=='ajlcd-irrhel02')
unique(graph_netconn$Hostname)
#graph_netconn<-subset(graph_netconn,graph_netconn$Hostname=='ajlcd-irrhel02')
#graph_netconn<-subset(graph_netconn,graph_netconn$Hostname=='PBNKWER')
nrow(graph_netconn)

graph_netconn.outbound<-subset(graph_netconn,graph_netconn$Connection_type=='TRUE')
nrow(graph_netconn.outbound)

graph_netconn.inbound<-subset(graph_netconn,graph_netconn$Connection_type=='FALSE')
nrow(graph_netconn.inbound)

#------------------- host name, remote ip address ------------------------------------------------
#graph_netconn.outbound.edges<-graph_netconn.outbound%>%select(Hostname,Remote_IP_address)

# ----------------------------------------------------------------------------------------
graph_netconn.outbound.edges<-graph_netconn.outbound%>%select(Process_name,Remote_IP_address,Event_time)
head(graph_netconn.outbound.edges)
graph_netconn.outbound.edges$Count<-1
graph_netconn.outbound.edges<-graph_netconn.outbound.edges%>%select(Process_name,Remote_IP_address,Count,Event_time)

#--------------- clean ip --------------
colnames(graph_netconn.outbound.edges)<-c('from','to','weight','time')
graph_netconn.outbound.edges$to<-as.character(sapply(graph_netconn.outbound.edges$to, function(x) long2ip(x)))
head(graph_netconn.outbound.edges)
str(graph_netconn.outbound.edges)
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
graph_netconn.outbound.vertices$vertices<-as.character(graph_netconn.outbound.vertices$vertices)
head(graph_netconn.outbound.vertices)
nrow(graph_netconn.outbound.vertices)

#----- aggreagte multiple links between the same two nodes --------------------------
str(graph_netconn.outbound.edges)
graph_netconn.outbound.edges <- aggregate(graph_netconn.outbound.edges[,4], graph_netconn.outbound.edges[,-4], sum)
graph_netconn.outbound.edges <- graph_netconn.outbound.edges[order(graph_netconn.outbound.edges$from, graph_netconn.outbound.edges$to),]
head(graph_netconn.outbound.edges)


head(links)
head(nodes)
net3 <- network(links,  vertex.attr=nodes, matrix.type="edgelist", 
                loops=F, multiple=F, ignore.eval = F)



head(graph_netconn.outbound.edges)
head(graph_netconn.outbound.vertices)



str(graph_netconn.outbound.vertices)


graph_netconn.outbound.vertices$vertices<-as.character(graph_netconn.outbound.vertices$vertices)

head(graph_netconn.outbound.vertices)

#gregexpr(pattern ='23',"the2quickbrownfoxeswere2tired")
#fruit <- c("apple")
#ifelse(str_count(fruit, "a"),'123','456')

graph_netconn.outbound.vertices$color<-ifelse(str_count(graph_netconn.outbound.vertices$vertices, ".exe"),1,2)
head(graph_netconn.outbound.vertices)


plot(net, vertex.color=colrs[V(net)$community])

my_net <- network(graph_netconn.outbound.edges,  vertex.attr=graph_netconn.outbound.vertices, matrix.type="edgelist", 
                loops=F, multiple=F, ignore.eval = F)


V(net)$community <- optimal.community(net)$membership
colrs <- adjustcolor( c("tomato", "gold"), alpha=.6)

plot.network(my_net,displaylabels = TRUE,
             usearrows = F, mode = "fruchtermanreingold",
             label.pad = 1.5,
             label.pos = 0,
             vertex.col=colrs[graph_netconn.outbound.vertices$color]
             ) 



nrow(graph_netconn.outbound.vertices)

as.matrix(my_net)

as.matrix(my_net, matrix.type="edgelist")[,2]

vs <- data.frame(onset=0, terminus=50, vertex.id=1:34)
es <- data.frame(onset=1:40, terminus=50, 
                 tail=as.matrix(my_net, matrix.type="edgelist")[,1],
                 head=as.matrix(my_net, matrix.type="edgelist")[,2])

net3.dyn <- networkDynamic(base.net=my_net, edge.spells=es, vertex.spells=vs)
plot(net3.dyn,displaylabels = TRUE,
     vertex.col=colrs[graph_netconn.outbound.vertices$color])


filmstrip(net3.dyn, displaylabels=T, mfrow=c(1, 5),
          slice.par=list(start=0, end=49, interval=10, 
                         aggregate.dur=10, rule='any'))

compute.animation(net3.dyn, animation.mode = "kamadakawai",
                  slice.par=list(start=0, end=50, interval=1, 
                                 aggregate.dur=1, rule='any'))

render.d3movie(
  net3.dyn,
  render.par = render.par,
  plot.par = plot.par,
  vertex.cex = 0.9,
  #vertex.col = "ndtvcol",
  edge.col = "darkgrey",
  vertex.border = "lightgrey",
  displaylabels = TRUE,
 # vertex.tooltip = function(slice){paste('name:',slice%v%'vertex.names','<br>',
                                     #    'status:', slice%v%'testatus')},
  output.mode='inline')

render.d3movie( net3.dyn,filename='short.stergm.html',
               launchBrowser=TRUE, 
               displaylabels = TRUE,
               edge.col = "darkgrey",
               vertex.border = "lightgrey",
            #   vertex.col = "gold",
               usearrows=F,
               plot.par = plot.par,
               vertex.cex = 0.9,
               render.par=list(tween.frames=10,
                               show.time=TRUE,
                               show.stats=NULL,
                               extraPlotCmds=NULL,
                               initial.coords=0),
               script.type='remoteSrc')

