#open packages that are available for download on Cran
library(corrplot)
library(dplyr)
library(ggplot2)
library(wsyn)
library(maps)
library(mapdata)
library(rgdal)
library(reshape2)
library(RColorBrewer)
library(fields) # colorbar.plot()

#preparation of data from FAO for analyses
fao<-read.csv(file="~/TS_FI_CAPTURE.csv")
#subset fishing area so it goes from 18-88 (the FAO regions)
fao2<-subset(fao, Fishing_Area>=18 & Fishing_Area<=88, select=Fishing_Area:Quantity)
fao2<-fao2[order(fao2$Fishing_Area, fao2$Species, fao2$Year),]
fao2$Fishing_Area<-as.factor(fao2$Fishing_Area)
#Use of dplyr to get summation of tonnes for each species by year in each FAO region on whole dataframe
fao3<- fao2 %>% group_by(Fishing_Area, Year, Species) %>%
  mutate(Quantity=NULL, sum_tonnes=sum(Quantity)) %>% as.data.frame()
fao4<- distinct(fao3, .keep_all = TRUE)
#get number of years of data for each timeseries
fao4de1<- fao4 %>% group_by(Fishing_Area, Species) %>%
  summarise(maxyr=max(Year), minyr=min(Year), nyrs=n(), mean=mean(sum_tonnes))
#use >=60 year data set first and see how many are marine fishes and inverts.
fao4de6<-subset(fao4de1, subset=(nyrs>=60))
unique(fao4de6$Species)#1005 species
fao4de2<- fao4 %>% group_by(Fishing_Area) %>%
  summarise(n_distinct(Species))
#need to subset data so that major_group is PISCES, CRUSTACEA and MOLLUSCA
#exclude ISSCAAP_Group == "Miscellaneous freshwater fishes" & "Carps, barbels and other cyprinids"
# and "Tilapias and other cichlids" and "Marine fishes not identified" and "Freshwater crustaceans"
#and "Miscellaneous marine crustaceans" and "Krill, planktonic crustaceans" and "Miscellaneous marine molluscs"
#and "Freshwater molluscs"

#from CL_FI_SPECIES_GROUPS, I want 3Alpha_Code, Scientific_Name, Major_Group and ISSCAAP_Group
fao.sp.key<-read.csv(file="~/CL_FI_SPECIES_GROUPS.csv")
names(fao.sp.key)
fao.vtk<-c(1,9,11,13,15)
fsk2<-fao.sp.key[, fao.vtk]

#get full dataset with nyrs
fao5<- fao4 %>% left_join(fao4de1, by=c("Fishing_Area", "Species"))
fao60yr<-subset(fao5, subset=(nyrs>=60))#143165 rows of 8 vars
fao60yr1<-fao60yr[,c(1:4,8)]
#join this dataset with the species key
names(fsk2)
fao60yr2<-fao60yr1 %>% left_join(fsk2, by=c("Species" = "X3Alpha_Code"))
fao60yr3<-subset(fao60yr2, Major_Group=="PISCES" | Major_Group=="MOLLUSCA" | Major_Group=="CRUSTACEA")
#subsetting data to remove unidentified rows.
fao60yr4<-subset(fao60yr3, ISSCAAP_Group != "Miscellaneous freshwater fishes" &
                   ISSCAAP_Group != "Carps, barbels and other cyprinids" &
                   ISSCAAP_Group != "Tilapias and other cichlids" &
                   ISSCAAP_Group != "Marine fishes not identified" &
                   ISSCAAP_Group != "Freshwater crustaceans" &
                   ISSCAAP_Group != "Miscellaneous marine crustaceans" &
                   ISSCAAP_Group != "Krill, planktonic crustaceans" &
                   ISSCAAP_Group != "Miscellaneous marine molluscs" &
                   ISSCAAP_Group != "Freshwater molluscs" &
                   ISSCAAP_Group != "Clams, cockles, arkshells" &
                   ISSCAAP_Group != "Pearls, mother-of-pearl, shells")
fao6de1<- fao60yr4 %>% group_by(Fishing_Area) %>%
  summarise(n_distinct(Scientific_Name))
unique(fao60yr4$Scientific_Name)#871 species groups

#subset data where mean<10 tonnes, decided to use median later on
str(fao60yr4)
fao60yr5<-subset(fao60yr4, fao60yr4$mean>10)
unique(fao60yr5$Species)#721 species groups
unique(fao60yr5$Scientific_Name)#719 sp names
which(fao60yr5$sum_tonnes==0)#21339 out of 104921 (20.34% consists of zeros)
names(fao60yr5)
fao60yr5$loc_sp<-paste(fao60yr5$Fishing_Area, fao60yr5$Scientific_Name)
unique(fao60yr5$loc_sp)#1588 population time series
str(fao60yr5)
fao60yr5<-fao60yr5[order(fao60yr5$Fishing_Area, fao60yr5$Family_Group, fao60yr5$Scientific_Name, fao60yr5$Year),]
unique(fao60yr5$Fishing_Area)#16 fao regions
max(fao60yr5$Year)#min=1950, max=2015

#getting time series format for Reumannplatz
fao60yr6<-subset(fao60yr5, select=c(Year, loc_sp, sum_tonnes))
head(fao60yr6)
m.fao60<-table2matrix(fao60yr6)
tm.fao60<-t(m.fao60)#1588 species originally, however there are NAs.
#delete all the 1950:1954 (col 1:5) and 2015 (col 66)
head(tm.fao60)
tm.fao.1955.2014<-tm.fao60[,6:65]
head(tm.fao.1955.2014)
md.tm2<-which(is.na(tm.fao.1955.2014), arr.ind=TRUE)
ts.fao60<-tm.fao.1955.2014[-c(unique(md.tm2[,1])),]#1530 species from 1955-2014
which(ts.fao60==0)#17298 of 91800 (18.84% are zeros)

#using median<10 subset the data.
med.val<-apply(ts.fao60, 1, median)
med10<-which(med.val<10, arr.ind=TRUE)#307 rows where median value <10
unique(med10)
t3<-ts.fao60[-c(unique(med10)),]
which(is.na(t3))#no more NAs
str(t3)#seems weird
which(t3==0)#4414 of 73380 (6.02% are zeros)
t3m<-as.matrix(t3)#only has 1223 species now
#See if the number of zeros is more than 15 for any row
zero15<-which(zeros>=15)
t3m2<-t3m[-c(zero15),]#only 1092 species left
str(t3m2)
zeros1<-rowSums(t3m2==0)
max(zeros1)
which(zeros1==14)

t3m2.cd<-CleanData(t3m2, normalize=TRUE, each.ts=TRUE)#using old Reumannplatz package
t5<-t3m2.cd$cleandat
head(t5)


#full dataset for 1092 species from 1955-2014
library(reshape2)
df.t5<-melt(t3m2, varnames=c("loc_sp", "year"), value.name="tonnes")
head(df.t5)
str(df.t5)
str(fao60yr5)
fsk3<-subset(fao60yr5, select=c(Fishing_Area, Scientific_Name, Family_Group, loc_sp))
head(fsk3) 
fsk4<- distinct(fsk3, .keep_all = TRUE)#1588 obs of 4 vars
df.t5.full<-df.t5 %>% left_join(fsk4, by="loc_sp")
str(df.t5.full)
t5de3<-df.t5.full %>% group_by(Fishing_Area) %>%
  summarise(n_distinct(Scientific_Name))
t5de4<-df.t5.full %>% group_by(Fishing_Area) %>%
  summarise(n_distinct(Family_Group))
summary(df.t5.full$tonnes)
which(df.t5.full$tonnes==0)#1619 out of 65520 (2.5% zeros)

######full dataset for transformed data of 1092 species from 1955-2014
df.t5cd<-melt(t5, varnames=c("loc_sp", "year"), value.name="clndat")
head(df.t5cd)
str(df.t5cd)
head(fsk4)
df.t5cd.full<-df.t5cd %>% left_join(fsk4, by="loc_sp")
str(df.t5cd.full)

####to get ordered (by family, then genera, then species) 1092 time series that have been box cox transformed:
load(file="~/t5ord.RData")
str(t5.ord)
#for full dataframe of 1092 species groups with loc_sp, year, clndat, fao region, scientific name and family group:
fao.sp<-read.csv(file="~/t5clndat_full_1955_2014_name_change.csv")
str(fao.sp)

#Code below was run using parallelization with a small computer cluster, using the function synmat in the old Reumannplatz package
#annotate code for redo of coh for fao 1092 species
library(Reumannplatz)
library(foreach)
library(parallel)
library(doParallel)
load(file="~/t5ord.RData")
str(t5.ord)
#source(file="/local/home/joyceo/modified synmat code/synmat14_cohcode_7r.R")
#source(file="/local/home/joyceo/ReXWT.wt_code.R")
source(file="/local/home/joyceo/modified synmat code/synmat14_cohcode_3r.R")
cl<-makeCluster(20)
registerDoParallel(cl)
#istpt<-seq(2, 771, 7)#110 iterations, p1 of full 1092 dataset
istpt<-seq(772, 1092, 3)#107 iterations, p2 of full 1092 dataset
#tsrange, short=2-4, med=5-10, long=11-20 for fao dataset
fcoh1092p2<- foreach(k=1:107, .packages="Reumannplatz", .combine="rbind") %dopar% 
  synmat14(t5.ord, istart=istpt[k], times=1955:2014, nsurrogs=1000, scale.min=2, scale.max.input=20, sigma=1.05, f0=1)
stopCluster(cl)
write.csv(fcoh1092p2, file="~/fcoh1092p2.csv")

#rebuild coh pvals into full matrix with rownames and colnames
#use fcoh_pval_1102_lowertri.csv in all_timescales folder, with 10ev.
fcohpv.lowtri<-read.csv(file="~fcoh_pval_1102_lowertri.csv")
str(fcohpv.lowtri)
fcohpv.lowtri<-fcohpv.lowtri[,-1]
colnames(fcohpv.lowtri)<-dimnames(t6)[[1]]
rownames(fcohpv.lowtri)<-dimnames(t6)[[1]]
fcohpv.lowtri[1:6,1:10]#check
fcohpvmat1<-as.matrix(fcohpv.lowtri)
fcohpvmat2<-t(fcohpvmat1)
diag(fcohpvmat1)<-0
fcohpvmat1[upper.tri(fcohpvmat1)]<-fcohpvmat2[upper.tri(fcohpvmat2)]
fcohpvmat1[1:6,1:10]#check

fcohval<-read.csv(file="~/coh_val_fao_1102_allts_20180619.csv")
str(fcohval)
rownames(fcohval)<-fcohval[,1]
fcohval<-fcohval[,-1]
colnames(fcohval)<-rownames(fcohval)
fcohval[1:5,1:8]#data check
fcohvalmat<-as.matrix(fcohval)
str(fcohvalmat)


#code to get df for 1092 species
str(fcohpvmat1)
str(fcohvalmat)
fcohvalmat1092<-fcohvalmat[1:1092,1:1092]
fcohpvmat1092<-fcohpvmat1[1:1092,1:1092]
fcohdf<-data.frame(row=rownames(fcohvalmat1092)[row(fcohvalmat1092)[lower.tri(fcohvalmat1092)]],
                   col=colnames(fcohvalmat1092)[col(fcohvalmat1092)[lower.tri(fcohvalmat1092)]],
                   corr=fcohvalmat1092[lower.tri(fcohvalmat1092)])
head(fcohdf)#595686 obs
fcohdf$row.region<-as.numeric(substr(fcohdf$row, 1, 2))
fcohdf$col.region<-as.numeric(substr(fcohdf$col, 1, 2))
fcohdf$pval<-fcohpvmat1092[lower.tri(fcohpvmat1092)]
plot(fcohdf$corr, -log10(fcohdf$pval))#data check
fcohdf$regionop<-as.factor(ifelse(fcohdf$row.region==fcohdf$col.region, fcohdf$row.region, "others"))
fcohdf$inout<-as.factor(ifelse(fcohdf$row.region==fcohdf$col.region, "intra-regional", "inter-regional"))
unique(fcohdf$inout)
unique(fcohdf$row.region)#15 regions
unique(fcohdf$regionop)#fao region 18, 48, 58, 88 are missing.
head(fcohdf)
fcohdf$sig<-as.factor(ifelse(fcohdf$pval<=0.01, "sig", "nonsig"))

area_lookup<-read.csv(file="~/SAUP data/area_lookup.csv")
str(area_lookup)
unique(area_lookup$area_code)
vtd<-c(1,9,12,19)
area<-area_lookup[-vtd,]#delete Atl_Ant, Ind_Ant and Pac_Ant
area$area_short
area$ocean<-substr(area$area_short,1,3)
oceanlookup<-area[,3:4]
oceanlookup[16,]<-c("others", "others")

fcohdf1<-fcohdf %>% left_join(oceanlookup, by=c("regionop" = "area_code"))
str(fcohdf1)
#reorder ocean factor levels
fcohdf1$oceans2<-factor(fcohdf1$ocean, levels=c("Atl", "Ind", "Med", "Pac", "others"))
levels(fcohdf1$oceans2)#data check

####for df of fao coherence with fdr<20%
fcohq2sub2<-read.csv(file="~/fcohq2sub2_ranked_20181106.csv")
str(fcohq2sub2)
fcohq2sub2<-fcohq2sub2[,-1]

#for df of all fao coherence, over all pvalues
fcohdf1<-read.csv(file="~/fcohdf1_20190521.csv")
str(fcohdf1)
fcohdf1<-fcohdf1[,-1]
fcohdf1$qval<-p.adjust(fcohdf1$pval, method="fdr")
fcohdf1sub<-fcohdf1[,c(1:3,7,14:15)]#basically want row, col, regionop, oceancon2, qval for family randomizations
str(fcohdf1sub)

#Code for randomizations within ocean basins
randlist<-list()
for (j in 1:1000){
  randlist[[j]]<-transform(fcohdf1sub, oceancon2=sample(oceancon2))
}#should give 1000 df of 595686 obs of 5 vars
randlist<-lapply(randlist, function (x) filter(x, qval<0.2))#should give 1000 df of 3382 obs of 5 vars
randout<-matrix(data=NA, nrow=1000, ncol=6)
colnames(randout)<-c("winoc", "btwoc", "winocpt", "btwocpt", "wincoh", "btwcoh")
for (j in 1:1000){
  randout[j,1]<-length(which(nchar(randlist[[j]]$oceancon2)<4))
  randout[j,5]<-mean(subset(randlist[[j]], nchar(randlist[[j]]$oceancon2)<4)$corr)
  randout[j,6]<-mean(subset(randlist[[j]], nchar(randlist[[j]]$oceancon2)>4)$corr)
}
head(randout)
randout<-as.data.frame(randout)
randout$btwoc<-with(randout, 3382-winoc)
randout$winocpt<-with(randout, (winoc/236331)*100)
randout$btwocpt<-with(randout, (btwoc/359355)*100)
summary(randout$wincoh)
#plot histograms
randout1<-ggplot(data=randout, aes(randout$winocpt))#use btwocpt for betw, winocpt for within
randout1 + geom_histogram(binwidth=0.005) + theme_classic() +
  labs(title="% of within ocean coherent rships", x="Percentage", y="Count") +
  scale_y_continuous(expand=c(0,0), limits=c(0,200)) +#250 limit for btw, 200 limit for within
  scale_x_continuous(limits=c(0.52, 0.62)) + geom_vline(xintercept = 0.60, col="red")#0.54 for betw, 0.60 for within

#estimate p values from null models, ie. 1000 randomizationsm for oceans
oceanrandout<-read.csv("~/oceans_randout_20181031.csv")
oceanrandout<-oceanrandout[,-1]
head(oceanrandout)
head(fcohq2sub2)
length(which(nchar(fcohq2sub2$oceancon2)>4))#total 1427 within ocean and 1955 between ocean obs
length(which(randout$winocpt>0.6037141))#r=1, estimate p value = 1+1/1000+1 = 0.002 for within ocean coh
length(which(randout$winoc>1427))#same, r=1. 
length(which(randout$btwocpt>0.5440303))# r=999, est p value = 999+1/1000+1 = 0.99 for between ocean
mean(subset(fcohq2sub2, nchar(fcohq2sub2$oceancon2)>4)$corr)#0.6610896 for within ocean, 0.6560049 for betw
length(which(randout$wincoh>0.6610896))#r=30, est pval = 0.031 for within ocean
length(which(randout$btwcoh>0.6560049))#r=982, est pval = 0.982 for btw ocean
length(which(oceanrandout$btwocpt<0.5440303))#r = 1, est pval = 0.002
length(which(oceanrandout$btwcoh<0.6560049))#r=18, est pval = 0.019

#Code to get mean phases for the significantly coherent pairs within FAO regions
#uses new package wsyn
regionames<-c("21","27","31","34","37","41","47","51","57","61","67","71","77","81","87","others")
regionames1<-as.numeric(regionames[1:15])
winoc1<-filter(fcohq2sub2,oceancon2 %in% c("atl","pac", "ind"))#1427 obs
str(winoc1)

cohsp<-list()
for (i in 1:15){
  cohsp[[i]]<-filter(winoc1, regionop==regionames1[i])
}
str(cohsp[[1]])#still data frame
nrow(cohsp[[1]])#25 rows
#25+63+24+18+31+15+3+11+65+42+1+54+5+20+4 = 381 within fao region significant relationships

winls<-vector(mode="list",15)
for (i in 1:15){
  for (j in 1:nrow(cohsp[[i]])){
    winls[[i]][[j]]<-coh(dat1=t5.ord[which(dimnames(t5.ord)[[1]]==cohsp[[i]][j,1]),],
                         dat2=t5.ord[which(dimnames(t5.ord)[[1]]==cohsp[[i]][j,2]),],times=1955:2014, norm="powall",
                         sigmethod="fast", nrand=1000, scale.max.input=20)
    winls[[i]][[j]]<-bandtest(winls[[i]][[j]], c(2,4))
    winls[[i]][[j]]<-bandtest(winls[[i]][[j]], c(4,8))
    winls[[i]][[j]]<-bandtest(winls[[i]][[j]], c(8,12))
    winls[[i]][[j]]<-bandtest(winls[[i]][[j]], c(12,16))
  }
}

#separate the 2 steps bcos 11 only has 1 coherent species pair
dflist<-vector(mode="list",15)
for (i in 1:15){
  dflist[[i]]<-as.data.frame(do.call(cbind, winls[[i]][[1]]$bandp))
}#so far this is correct
for (i in 1:10){
  for (j in 2:nrow(cohsp[[i]])){
    dflist[[i]]<-bind_rows(dflist[[i]],as.data.frame(do.call(cbind, winls[[i]][[j]]$bandp)))
  }
}#stops at dflist[[11]], 11 only has 1 coherent species, hence its out of bounds.
#so can stop at 10, do 11 separately, then start again from 12-15. 
for (i in 12:15){
  for (j in 2:nrow(cohsp[[i]])){
    dflist[[i]]<-bind_rows(dflist[[i]],as.data.frame(do.call(cbind, winls[[i]][[j]]$bandp)))
  }
}

for(i in 1:15){
  dflist[[i]]$fao<-faonames[i]
}

dflist<-lapply(dflist, function (x) filter(x, p_val<0.05))

alldf2<-bind_rows(dflist)# total should be 847 rows. 
str(alldf2)

alldf3ph<-alldf2 %>% group_by(fao) %>% 
  summarise(nph0=length(which(mn_phs>-0.786 & mn_phs<0.786)),
            nlagneg=length(which(mn_phs>-2.392 & mn_phs< -0.786)),
            nlagpos=length(which(mn_phs>0.786 & mn_phs<2.392)),
            nanti=length(which(mn_phs>2.392 | mn_phs< -2.392)), totn=n())
alldf3ph<-as.data.frame(alldf3ph)
alldf3ph$ptph0<-with(alldf3ph, nph0/totn*100)
alldf3ph$nlagtot<-with(alldf3ph, nlagneg+nlagpos)
alldf3ph$ptphlag<-with(alldf3ph, nlagtot/totn*100)
alldf3ph$ptphanti<-with(alldf3ph, nanti/totn*100)
str(alldf3ph)

penspshn<-alldfph0pe2[,5:8]
alldf3ph$area_short<-as.factor(alldf3ph$fao)
alldf3phpe3<- alldf3ph %>% left_join(penspshn, by="area_short")
str(alldf3phpe3)
