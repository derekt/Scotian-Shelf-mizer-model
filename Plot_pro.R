#Trying to make the plot of 10 year proportion with plot of model proportion

library(ggplot2)
bottle= ggplot(data = Mean_CB_sub, aes(x= sjob.COMM, y= sjob.TOTWGT, na.rm = TRUE))+ geom_bar(stat="identity") +xlab("species") + ylab("weight (kg)")
plot(bottle)

#dataframe with observed values
Mean_CB_sub

#dataframe with mizer output after 10 years
mizer_w

glass= ggplot(data = mizer_w, aes(x= species, y= w_kg, na.rm = TRUE))+ geom_bar(stat="identity") +xlab("species") + ylab("weight (kg)")

plot(glass)



#the weights are so different, let's try and do a proportion figure
#modelled pro
phone= ggplot(data = mizer_w, aes(x= species, y= pro, na.rm = TRUE))+ geom_bar(stat="identity") +xlab("species") + ylab("modelled proportion") +theme(plot.margin=unit(c(1.5,1.5,1.5,1.5),"cm"))  + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + coord_cartesian( ylim = c(0, 1))

plot(phone)

#observed pro
spoon= ggplot(data = observe_w, aes(x= species, y= pro, na.rm = TRUE))+ geom_bar(stat="identity") +xlab("species") + ylab("observed proportion") +theme(plot.margin=unit(c(1.5,1.5,1.5,1.5),"cm"))  + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + coord_cartesian( ylim = c(0, 1))

plot(spoon)

library(ggpubr)

grid.arrange(spoon, phone,  heights=c(3,3))
