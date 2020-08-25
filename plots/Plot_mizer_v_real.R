#Trying to make the plot of 10 year proportion with plot of model proportion

setwd("c:/users/derekt/work/IsabelleFishery/")
mizer_w = read.csv("mizer_w.csv")
observe_w = read.csv("observe_w.csv")

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


# Plot observed versus predicted
# NOTE CRITICAL THAT SPECIES ARE AT THE SAME POINT IN THE MATRIX
# FOR OBSERVED AND PREDICTED. SHOULD BUILD ERROR CHECKING FOR THIS.
dev.new()
axis_lim = max(c(observe_w$pro, mizer_w$pro))
plot(observe_w$pro, mizer_w$pro, col = rainbow(length(mizer_w$pro)), pch = 19, xlim = c(0, axis_lim), ylim = c(0, axis_lim), xlab = "Observed proportion", ylab = "Modelled proportion")
abline(a = 0, b = 1, lty = 2)
legend("top", legend = as.character(observe_w$ï..species), col = rainbow(length(mizer_w$pro)), pch = 19, cex = 0.75)


# Alternative plots
dev.new(width=8, height=11)
par(mfcol = c(1,3))
#par(mar = c(5,9,4,2))
y_locs = seq(length(mizer_w$pro),1)
a = sort(mizer_w$pro, decreasing = TRUE, index.return = TRUE)
plot(a$x, y_locs, xlim = c(0, axis_lim), xlab = "Modelled proportion", ylab = "", yaxt = "n", pch = 19, col = rainbow(length(mizer_w$pro)))
#axis(side = 2, at = y_locs, cex.axis = 0.6, labels =  mizer_w$ï..species[a$ix], las = 1)

#par(mar = c(5,5,4,2))
plot(observe_w$pro[a$ix], y_locs, xlim = c(0, axis_lim), xlab = "Observed proportion", ylab = "", yaxt = "n", pch = 19, col = rainbow(length(mizer_w$pro)))
plot(a$x - observe_w$pro[a$ix], y_locs, xlim = c(-axis_lim, axis_lim), xlab = "Modelled - observed", ylab = "", yaxt = "n", pch = 19, col = rainbow(length(mizer_w$pro)))
abline(v = 0, lty = 2)




