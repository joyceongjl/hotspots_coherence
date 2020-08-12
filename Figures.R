#R code for figures in global MS 
#### Code for Fig 1 pedagogical
#Plot of pedagogical figure,a is inphase on long ts but antiphase on short,
#b is antiphase on long ts but inphase on short
#c is coh with phase diff 0 (in phase), d is coherence with lag (non zero phase diff)

#code for a and b
tt=100 #number of timesteps
ncycles1=1.5 #number of cycles in signal 1
ncycles2=10 #number of cycles in signal 2
res=10 #resolution, number of sub-intervals per timestep--increasing this makes a smoother plot

x<-seq(from=res/tt,to=tt,by=res/tt)

s1<-sin(seq(from=0,to=2*pi*ncycles1,length.out=tt*res))+2.5
s2<-sin(seq(from=0,to=2*pi*ncycles2,length.out=tt*res))
s3<-(s1-2.5)*-1+2.5
s4<-s2*-1
#df for sine waves with long and short timescales
tsfd<-as.data.frame(x)
head(tsfd)
tsfd$a1<-s1+s2#this is for both a1 and b1
tsfd$a2<-s1*0.9+s4
tsfd$b2<-s3+s2


#code for c and d, coherence with phase diff = 0 and non-zero
tt=100
y1<-arima.sim(model=list(ar=c(0.7,-0.5)), n=tt)
y2<-y1 + rnorm(tt, 0, 0.3)
y3<-c(rnorm(2), y1[1:(tt-2)]) + rnorm(tt,0,0.3)
y4<-y1[sample(tt, tt, replace=F)]
#df for random ts with coherence, phase diff=0 or phase lags
tscd<-as.data.frame(1:tt)
tscd$c1<-as.numeric(y1)#same as d1
tscd$c2<-as.numeric(y2)
tscd$d2<-as.numeric(y3)
tscd$noc<-as.numeric(y4)
head(tscd)
#correlation tests
cor.test(tscd$c1, tscd$c2)#pearsons corr = 0.976, p<0.01, or p = 2.2e-16
cor.test(tscd$c1, tscd$d2)#pearsons corr = 0.044, p=0.662
cor.test(tscd$c1, tscd$noc)#pearsons corr = 0.046, p=0.648
#coherence tests
y1cln<-cleandat(y1, times=1:100, clev=1)$cdat
y2cln<-cleandat(y2, times=1:100, clev=1)$cdat
y3cln<-cleandat(y3, times=1:100, clev=1)$cdat
y4cln<-cleandat(y4, times=1:100, clev=1)$cdat
cohts<-coh(y1cln, y2cln, times=1:100, norm="powall", sigmethod="fast")
Mod(mean(cohts$coher, na.rm=T))#0.881
plotmag(cohts)#coherence sig from 2-20 timescales
cohts<-bandtest(cohts, c(2,30))
get_bandp(cohts)#p<0.01, or p = 0.00100
cohts2<-coh(y1cln, y3cln, times=1:100, norm="powall", sigmethod="fast")
Mod(mean(cohts2$coher, na.rm=T))#0.452
plotmag(cohts2)#coherence sig from 2-10 timescales
cohts2<-bandtest(cohts2, c(2,30))
get_bandp(cohts2)#p<0.01, or p = 0.00100
cohts3<-coh(y1cln, y4cln, times=1:100, norm="powall", sigmethod="fast")
Mod(mean(cohts3$coher, na.rm=T))#0.194
plotmag(cohts3)#coherence insig for all timescales
cohts3<-bandtest(cohts3, c(2,30))
get_bandp(cohts3)#p=0.437

p1a<-ggplot(tscd, aes(x=`1:tt`, y=c1)) + geom_line() + theme_classic(base_size=14) + 
  geom_line(aes(x=`1:tt`, y=c2), color="red") + labs(x="Time", y="Signal") +
  scale_y_continuous(breaks=seq(-4,4,2),limits=c(-4.2,4)) + 
  scale_x_continuous(breaks=seq(0,100, 25),limits=c(0,100))
p2a<-ggplot(tscd, aes(x=`1:tt`, y=c1)) + geom_line() + theme_classic(base_size=14) + 
  geom_line(aes(x=`1:tt`, y=d2), color="red") + labs(x="Time", y="Signal") +
  scale_y_continuous(breaks=seq(-4,4,2),limits=c(-4.2,4)) + 
  scale_x_continuous(breaks=seq(0,100, 25),limits=c(0,100))
p3a<-ggplot(tscd, aes(x=`1:tt`, y=c1)) + geom_line() + theme_classic(base_size=14) + 
  geom_line(aes(x=`1:tt`, y=noc), color="red") + labs(x="Time", y="Signal") +
  scale_y_continuous(breaks=seq(-4,4,2),limits=c(-4.2,4)) + 
  scale_x_continuous(breaks=seq(0,100, 25),limits=c(0,100))
g<-arrangeGrob(p1a, p2a, p3a, nrow=3)


####code for Fig 2
library(dplyr)
library(ggplot2)
library(cowplot)

theme_set(theme_classic(base_size=14))
theme_set(theme_bw(base_size=14))

###global scale figures
nullredqvdf<-read.csv("~/nullredqvdf_pvals_red07rand100_20190417.csv")
str(nullredqvdf)
length(which(nullredqvdf$globpt>0.568))#r=0, pval=0+1/100+1 = 0.0099

#histogram with numbers
globnullqvp2<-ggplot(nullredqvdf, aes(nfdr20))
globp2<-globnullqvp2 + geom_histogram(binwidth=20) + theme_classic(base_size=14) +
  labs(title="", x="", y="Count") + theme(axis.text=element_text(size=14)) +
  scale_y_continuous(expand=c(0,0), limits=c(0,30)) +
  scale_x_continuous(limits=c(0, 3400)) + geom_vline(xintercept = 3382, col="red")#

red07cohvalsampl<-read.csv("~/red07cohvalsampl_rand100.csv")
str(red07cohvalsampl)
#note that mean coh of observed 595686 rships globally = 0.5426
length(which(red07cohvalsampl$meancoh3382s>0.5426))#r=97, pval = 98/101 = 0.9703

globnullcoh3382sp1<-ggplot(red07cohvalsampl, aes(meancoh3382s))
globcoh<-globnullcoh3382sp1 + geom_histogram(binwidth=0.0004) + theme_classic(base_size=14) +
  labs(title="", x="", y="") + theme(axis.text=element_text(size=14)) +
  scale_y_continuous(expand=c(0,0), limits =c(0,20)) + 
  geom_vline(xintercept = 0.5426, col="red")

## circular histogram of all 3382 coh rships globally
coh3382ph<-read.csv("~/coh3382ph_20190813.csv")
length(which(coh3382ph$mn_phs>-0.786 & coh3382ph$mn_phs<0.786))#1058 out of 3382 are in-phase, ie. 31.29%

globdomphp2<-ggplot(coh3382ph, aes(mn_phs))
globcirp2<-globdomphp2 + geom_histogram(binwidth=0.08) +
  scale_x_continuous(breaks=c(0, pi/2, pi, -pi/2), labels=expression(0,pi/2,pi/-pi,-pi/2), expand=c(0,0)) +
  coord_polar() + labs(x="", y="", title="") + theme_bw(base_size=14) + theme(panel.grid.minor=element_blank(), axis.text=element_text(size=14))


###ocean scale figures
randout<-read.csv("~/oceans_randout_20181031.csv")
str(randout)
length(which(randout$winocpt>0.6037141))#r=1, estimate p value = 1+1/1000+1 = 0.002 for within ocean coh
length(which(randout$wincoh>0.6610896))#r=30, est pval = 0.031 for within ocean
oc4tsb<-read.csv("D:/Rutgers_postdoc/data/FAO landings data/withinocean_4tsband_20190419.csv")
str(oc4tsb)
length(which(oc4tsb$mn_phs<0.786 & oc4tsb$mn_phs>(-0.786)))#n=1069 out of 3127, 0.34186

#new hist for winoc by numbers
randout1a<-ggplot(data=randout, aes(randout$winoc))
ocp2<-randout1a + geom_histogram(binwidth=10) + theme_classic(base_size=14) +
  labs(title="", x="", y="Count") + theme(axis.text=element_text(size=14)) +
  scale_y_continuous(expand=c(0,0), limits=c(0,200)) +
  scale_x_continuous(limits=c(1250, 1450)) + geom_vline(xintercept = 1427, col="red")

randout2<-ggplot(data=randout, aes(randout$wincoh))
occoh<-randout2 + geom_histogram(binwidth=0.0005) + theme_classic(base_size=14) +
  labs(title="", x="", y="") + theme(axis.text = element_text(size=14)) +
  scale_y_continuous(expand=c(0,0), limits =c(0,150)) + 
  geom_vline(xintercept = 0.6610896, col="red")

#new cir hist for within oceans 
ocalltsb<-read.csv("~/coh1427_winoc_avgph_20190813.csv")
length(which(ocalltsb$mn_phs>-0.786 & ocalltsb$mn_phs<0.786))#501 out of 1427 are in-phase, ie. 35.11%

domphoc3<-ggplot(ocalltsb, aes(mn_phs))
occir2<-domphoc3 + geom_histogram(binwidth=0.15) +
  scale_x_continuous(breaks=c(0, pi/2, pi, -pi/2), labels=expression(0,pi/2,pi/-pi,-pi/2), expand=c(0,0)) +
  coord_polar() + labs(x="", y="", title="") + theme_bw(base_size=14) + theme(panel.grid.minor=element_blank(), axis.text=element_text(size=14))


#################fao scale
regrandout<-read.csv("~/faoregions_randout_20181030.csv")
regrandout<-regrandout[,-1]
str(regrandout)
length(which(regrandout$meancohintra>0.6814009))#r=0, pval = 0.001

#new hist for fao using numbers
regionrandout1a<-ggplot(data=regrandout, aes(regrandout$nintra))
faop2<-regionrandout1a + geom_histogram(binwidth=5) + theme_classic(base_size=14) +
  labs(title="", x="Number", y="Count") +
  scale_y_continuous(expand=c(0,0), limits=c(0,150)) + theme(axis.text = element_text(size=14)) +
  geom_vline(xintercept = 381, col="red") + scale_x_continuous(limits=c(200, 400))
ggsave(filename="D:/Rutgers_postdoc/Global MS/Figures/Aug2019/hist_rand1000_fao_num_20190812.eps", device="eps", scale=1, width=5, height=4, units="in", dpi=300)

regionrandout2<-ggplot(data=regrandout, aes(regrandout$meancohintra))#use ptinter for percentage inter
faocoh<-regionrandout2 + geom_histogram(binwidth=0.002) + theme_classic(base_size=14) + 
  labs(title="", x="Coherence", y="") + theme(axis.text = element_text(size=14)) +
  scale_y_continuous(expand=c(0,0), limits=c(0,150)) +#ptinter 200, mean coh 150
  geom_vline(xintercept = 0.681401, col="red") + theme(text = element_text(size=14))

#new cir hist for within fao
faodf<-read.csv("~/coh381_winfao_avgph_20190813.csv")
length(which(faodf$mn_phs>-0.786 & faodf$mn_phs<0.786))#185 out of 381 are in-phase, ie. 48.56%

domphfao<-ggplot(faodf, aes(mn_phs))
faocir2<-domphfao + geom_histogram(binwidth=0.15) +
  scale_x_continuous(breaks=c(0, pi/2, pi, -pi/2), labels=expression(0,pi/2,pi/-pi,-pi/2), expand=c(0,0)) +
  coord_polar() + labs(x="Mean phase", y="", title="") + theme_bw(base_size=14) + theme(panel.grid.minor=element_blank(), axis.text=element_text(size=14))

comb3x3<-plot_grid(globp2, globcoh, globcirp2, ocp2, occoh, occir2, faop2, faocoh, faocir2, 
                   labels=c("A", "B", "C", "D", "E", "F", "G", "H", "I"), align="h", nrow=3, ncol=3, label_size=18)
save_plot("~/combinedglobocfao.eps", 
          comb3x3, ncol=3, nrow=3, base_height=4, base_aspect_ratio=1.1)


#### Code for Fig 3, spatial map
library(maps)
library(mapdata)
library(rgdal)
library(plyr)
library(dplyr)
library(reshape2)
library(RColorBrewer)
library(fields) # colorbar.plot()

###do new map of fao regions with pvals of % coh rships (color) and pvals of coh values (size)
regionmat<-read.csv("~/regionmat_20181101.csv")
psigcvcoh2<-read.csv(file="~/cf_fao_rcorr_null_20180929.csv")
# Read FAO data
fao_areas <- readOGR(dsn="~/FAO mapping shape files", layer="FAO_AREAS_NOCOASTLINE", verbose=F)
fao_areas <- subset(fao_areas, F_LEVEL=="MAJOR")


regionpv<-regionmat[,c(1,4:5)]
str(regionpv)
psigcvcoh3<-psigcvcoh2[,c(2:4,6:7,40:41)]
psigcvcoh3<-psigcvcoh3 %>% left_join(regionpv, by="region")
#color
psigcvcoh3$pvalpt_bin <- cut(psigcvcoh3$pvalpt, breaks=c(0,0.01,0.05,1), include.lowest=TRUE)
colors_pvalpt <- colorRampPalette(c("red", "orange", "cyan"))(nlevels(psigcvcoh3$pvalpt_bin))
psigcvcoh3$pvalpt_bin_color <- colors_pvalpt[psigcvcoh3$pvalpt_bin]
psigcvcoh3$pvcoh_bin <- cut(psigcvcoh3$pvalcoh, breaks=c(0,0.01,0.05,1), include.lowest=TRUE)
size_pvcoh <- c(4, 3.5, 1.5)
psigcvcoh3$pvcohcex <- size_pvcoh[psigcvcoh3$pvcoh_bin]

postscript("~/map_pvptcoh.eps", width=720, height=480)
layout(matrix(c(1,2), ncol=2, byrow=T), widths=c(0.9,0.1))
par(mar=c(0.1,0.1,0.1,0.1), xpd=NA)

xlim <- c(-180, 180)
ylim <- c(-90, 90)

# Plot FAO stat areas
plot(fao_areas, border="grey70", lty=3, xlim=xlim, ylim=ylim)

# Plot world countries
map("world", col="grey65", fill=T, border="white", lwd=0.3, 
    xlim=xlim, ylim=ylim, add=T)

points(x=psigcvcoh3$x, y=psigcvcoh3$y, 
       pch=19, col=psigcvcoh3$pvalpt_bin_color, cex=psigcvcoh3$pvcohcex)

text(x=psigcvcoh3$x, y=psigcvcoh3$y, labels=psigcvcoh3$nsp, cex=0.9, pos=1, offset=0.65, font=2, col="black")
text(x=psigcvcoh3$x, y=psigcvcoh3$y, labels=psigcvcoh3$area_short, pos=3, offset=0.65, cex=0.9, font=2, col="black")

#size legend
legend(x=190, y=60,legend=c("p>0.05", "p<0.05","p<0.01"), bty="n", pch=c(1,1,1),
       title="Average\ncoherence", pt.cex=c(1.5, 3.5, 4), cex=0.9, y.intersp = 1.7)

#color legend
plot(1:10, 1:10, bty="n", type="n", xaxt="n", yaxt="n", xlab="", ylab="")
colorbar.plot(x=0, y=3, adj.x=0, adj.y=0, horiz=F, col=colors_pvalpt,
              strip=seq(-1.5,1.5, length.out=nlevels(psigcvcoh3$pvalpt_bin)), 
              strip.width=0.3, strip.length=1, xpd=NA)
text(x=2.5, y=c(3.2, 3.7, 4.2), pos=4, labels=c("p<0.01", "p<0.05", "p>0.05"), cex=0.9)
text(x=-2, y=4.8, pos=4, labels="Percentage of\ncoherent\nrelationships", font=2, cex=0.9)

dev.off()

# Portfolio effects and percentage of in-phase 
peallts<-ggplot(alltsphpe, aes(x=ptph0, y=pe))
peallts + geom_text(aes(label=area_short, color=ptphlag), size=5) + theme_classic(base_size=14) + 
  labs(x="Percentage of in-phase relationships", y="Portfolio effect") +
  scale_y_continuous(breaks=seq(15,85, 10),limits=c(20,85)) + scale_x_continuous(breaks=seq(0,100,10),limits=c(0, 100))


# Density plot of ANE and IOE
alldf2a<-read.csv("D:/Rutgers_postdoc/data/FAO landings data/alldf2a_20190411.csv")

win2<-subset(alldf2a, fao=="ANE"|fao=="IOE")
win2p3<-ggplot(win2, aes(mn_phs, color=fao))
win2p3 + geom_density() + theme_classic() +scale_y_continuous(expand=c(0,0), limits=c(0,0.7)) +
  labs(x="Mean phase", y="Density") + 
  scale_x_continuous(breaks=c(-pi, -pi/2, 0, pi/2, pi), labels=expression(-pi,-pi/2,0,pi/2,pi))
