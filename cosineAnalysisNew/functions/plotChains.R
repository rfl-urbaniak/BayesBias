
plotChains <- function(model){
samples <- extract.samples(model)
samplesDf <- as.data.frame(do.call(cbind, samples))
samplesDf$step <- 1:nrow(samplesDf)
nplot <- ggplot(samplesDf, aes(x = step, y = no))+geom_line(alpha = 0.5)+theme_tufte()+ggtitle("null")
aplot <- ggplot(samplesDf, aes(x = step, y = a))+geom_line(alpha = 0.5)+theme_tufte()+ggtitle("a")
dplot <- ggplot(samplesDf, aes(x = step, y = d))+geom_line(alpha = 0.5)+theme_tufte()+ggtitle("d")
hplot <- ggplot(samplesDf, aes(x = step, y = h))+geom_line(alpha = 0.5)+theme_tufte()+ggtitle("h")


return(ggarrange(nplot, aplot, dplot, hplot))
}
