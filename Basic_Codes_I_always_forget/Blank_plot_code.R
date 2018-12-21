# Blank Plot Code

plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10))

par(mfrow=c(1,2))
dev.off()
plot.new()
quartz()
barplot(height=c(0,0), names.arg=c("Outside","Center"), xlab="Location of Vigilant Behavior", ylab="Proportion of Time / Individuals Vigilant", ylim=c(0, 1))
grid(nx=8, col="gray23")
par(xpd=TRUE)
legend(1.35, 1.15,legend=c("Proportion of Group Vigilant (scan sampling)","Proportion of Group Non-Vigilant (scan sampling)","Proportion of Time Vigilant (focal individual)","Proportion of Time Non-Vigilant (focal individual)"), fill=c("blue","blue","red","red"), border=c("blue","blue","red","red"), density=c(100,0,100,0), bg="white", cex=0.75)
locator(n=2)
