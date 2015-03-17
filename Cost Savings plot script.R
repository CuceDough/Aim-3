
require(reshape2)
require(ggplot2)
require(colorbrewer)
### Plotting Cost savings

cost.dt <- data.table(read.csv("cost savings.csv")) #input

cost.melt <- melt(cost.dt, id.vars = "Vax", variable.name = "type" ) #melted

cost.melt$type <- cost.melt$type == 
#graph

ggplot(data=cost.melt, aes(x=Vax, y=value, colour=type)) + geom_line() + geom_point() + #
theme(axis.text.x = element_text(colour = 'black', angle = 90, size = 13, hjust = 0.5, vjust = 0.5),axis.title.x=element_blank()) + 
  ylab("Cost Savings in Millions, $)") + theme(axis.text.y = element_text(colour = 'black', size = 12), axis.title.y = element_text(size = 12, hjust = 0.5, vjust = 0.2)) + 
  theme(strip.text.y = element_text(size = 11, hjust = 0.5, vjust = 0.5, face = 'bold'))

ggplot(data=cost.melt, aes(x=Vax, y=value, colour=type)) + theme_bw() + geom_line(aes(group=factor(type)),size=2) + geom_point() + 
ylab("Cost Savings in Millions, $") + xlab("Vaccination Rate in 5-18 year olds") +
  theme(axis.title.y = element_text(size = rel(1.3), angle = 90, face = "bold")) +  #works making font larger
  theme(axis.title.x = element_text(size = rel(1.3), angle = 00, face = "bold")) +   #works 
  theme(axis.text = element_text(size = rel(1.2), colour = "black")) + # text sixe for axis number 
  theme(legend.title = element_text(colour="Black", size=12, face="bold"))+ 
  scale_color_discrete(name="Group") + #names legend 
  theme(legend.text=element_text(size=10, face= "bold")) + # makes size and font for legend 
  theme(legend.key = element_blank()) # gets rid of borders


