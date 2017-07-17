

library(ggplot2)
perm.sd = data.frame(acc = rep(NA,3), sensi = rep(NA,3), speci = rep(NA,3))
perm.p = data.frame(acc = rep(NA,3), sensi = rep(NA,3), speci = rep(NA,3))

perm.df = read.table("result_perm_hc_ptsd_vbmrsdtifc_svmt_k7_p0.001.csv", sep = ",", header = T)

png(filename = "histogram_hc_ptsd_vbmrsdtifc.png")
ggplot(data=perm.df, aes(perm.df$acc)) + 
	geom_histogram(breaks=seq(0, 1, by = .01), 
    				col="red", 
					fill="green", 
					alpha = .2) + 
	labs(title="histogram of acc") +
	labs(x="acc", y="Count") + 
	xlim(c(0,1)) + 
	ylim(c(0,100))
dev.off()

perm.sd[1,]$acc = sd(perm.df$acc)
perm.sd[1,]$sensi = sd(perm.df$sensi)
perm.sd[1,]$speci = sd(perm.df$speci)

print(sort(perm.df$acc))
perm.p[1,]$acc = sort(perm.df$acc)[950]
perm.p[1,]$sensi = sort(perm.df$sensi)[950]
perm.p[1,]$speci = sort(perm.df$speci)[950]

perm.df = read.table("result_perm_hc_trauma_vbmrsdtifc_svmt_k7_p0.001.csv", sep = ",", header = T)

png(filename = "histogram_hc_trauma_vbmrsdtifc.png")
ggplot(data=perm.df, aes(perm.df$acc)) + 
	geom_histogram(breaks=seq(0, 1, by = .01), 
    				col="red", 
					fill="green", 
					alpha = .2) + 
	labs(title="histogram of acc") +
	labs(x="acc", y="Count") + 
	xlim(c(0,1)) + 
	ylim(c(0,40))
dev.off()

perm.sd[2,]$acc = sd(perm.df$acc)
perm.sd[2,]$sensi = sd(perm.df$sensi)
perm.sd[2,]$speci = sd(perm.df$speci)

perm.p[2,]$acc = sort(perm.df$acc)[950]
perm.p[2,]$sensi = sort(perm.df$sensi)[950]
perm.p[2,]$speci = sort(perm.df$speci)[950]

perm.df = read.table("result_perm_trauma_ptsd_vbmrsdtifc_svmt_k7_p0.001.csv", sep = ",", header = T)

png(filename = "histogram_trauma_ptsd_vbmrsdtifc.png")
ggplot(data=perm.df, aes(perm.df$acc)) + 
	geom_histogram(breaks=seq(0, 1, by = .01), 
    				col="red", 
					fill="green", 
					alpha = .2) + 
	labs(title="histogram of acc") +
	labs(x="acc", y="Count") + 
	xlim(c(0,1)) + 
	ylim(c(0,40))
dev.off()

perm.sd[3,]$acc = sd(perm.df$acc)
perm.sd[3,]$sensi = sd(perm.df$sensi)
perm.sd[3,]$speci = sd(perm.df$speci)

perm.p[3,]$acc = sort(perm.df$acc)[950]
perm.p[3,]$sensi = sort(perm.df$sensi)[950]
perm.p[3,]$speci = sort(perm.df$speci)[950]
print(perm.sd)
print(colMeans(perm.sd))

print(perm.p)
