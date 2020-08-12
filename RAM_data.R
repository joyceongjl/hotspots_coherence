###################################################################################
###################################################################################
###################################################################################
#
# This will load database data from the file DBdata.RData. Put the data file in the
# working directory, then run the line at the bottom of the file.
#
###################################################################################
###################################################################################
###################################################################################
#
# The following objects are tables from RAM (data frames):
#
# --- metadata
#	Summarized metadata
# --- stock
#	General stock metadata
# --- assessment
#	General assessment metadata
# --- taxonomy
#	Taxonomic metadata
# --- management
#	Management authority metadata
# --- assessor
#	Stock assessor metadata
# --- assessmethod
#	Assessment method metadata
# --- area
#	Area metadata
# --- biometrics
#	Parameter data types with descriptions
# --- tsmetrics
#	Time series data types with descriptions
# --- timeseries
#	Full time series data listing
# --- bioparams
#	Full parameter data listing
# --- timeseries_values_views
#	Values by stock and year of common time series types
# --- timeseries_units_views
#	Units corresponding to values in timeseries_values_views
# --- timeseries_ids_views
#	Time series IDs corresponding to values in timeseries_values_views
# --- timeseries_assessments_views
#	Assessment IDs corresponding to values in timeseries_values_views
# --- timeseries_notes_views
#	Notes corresponding to values in timeseries_values_views
# --- timeseries_sources_views
#	Sources corresponding to values in timeseries_values_views
# --- timeseries_years_views
#	Year range corresponding to values in timeseries_values_views
# --- bioparams_values_views
#	Values by stock of common parameter types
# --- bioparams_units_views
#	Units corresponding to values in bioparams_values_views
# --- bioparams_ids_views
#	Parameter IDs corresponding to values in bioparams_values_views
# --- bioparams_assessments_views
#	Assessment IDs corresponding to values in bioparams_values_views
# --- bioparams_sources_views
#	Sources corresponding to values in bioparams_values_views
# --- bioparams_notes_views
#	Notes corresponding to values in bioparams_values_views
#
# ---------------------------------------------------------------------------------------------------
#
# There are also dataframes for the individual most-used time series:
#
# --- tb.data --- Total biomass data
# --- ssb.data --- Spawning stock biomass data
# --- tn.data --- Total abundance data
# --- r.data --- Recruits data
# --- tc.data --- Total catch data
# --- tl.data --- Total landings data
# --- recc.data --- Recreational catch data
# --- f.data --- Fishing mortality data (usually an instantaneous rate)
# --- er.data --- Exploitation rate data (usually an annual fraction harvested)
# --- divtb.data --- TB/TBmsy data
# --- divssb.data --- SSB/SSBmsy data
# --- divf.data --- F/Fmsy data
# --- diver.data --- ER/ERmsy data
# --- divbpref.data --- B/Bmsy pref data (B/Bmsy if available, otherwise B/Bmgt)
# --- divupref.data --- U/Umsy pref data (U/Umsy if available, otherwise U/Umgt)
# --- tbbest.data --- TBbest data (all in MT)
# --- tcbest.data --- TCbest data (all in MT)
# --- erbest.data --- ERbest data (usually an annual fraction harvested)
# --- divtb.mgt.data --- TB/TBmgt data
# --- divssb.mgt.data --- SSB/SSBmgt data
# --- divf.mgt.data --- F/Fmgt data
# --- diver.mgt.data --- ER/ERmgt data
# --- divbpref.mgt.data --- B/Bmgt pref data (B/Bmgt if available, otherwise B/Bmsy)
# --- divupref.mgt.data --- U/Umgt pref data (U/Umgt if available, otherwise U/Umsy)
# --- cpair.data --- Catch data that pairs with tac.data and/or cadv.data
# --- tac.data --- TAC data
# --- cadv.data --- Scientific advice for catch limit data
# --- survb.data --- Fishery-independent survey abundance data
# --- cpue.data --- CPUE data (fishery-dependent)
# --- effort.data --- Fishing effort data (fishery-dependent)
# --- divtn.data --- TN/TNmsy data
# --- divtn.mgt.data --- TN/TNmgt data
# --- cdivmeanc.data --- Catch/(mean catch) data
# --- cdivmsy.data --- Catch/MSY data
#
###################################################################################
###################################################################################
###################################################################################
#
# Once the DBdata.RData file is in the working directory, simply run the following command to
# load up the database data into matrix/dataframe files for the assessment only version of the database.
library(dplyr)
load("DBdata[asmt][v4.491].RData")
str(tbbest.data)#219 obs (1800-2018) of 471 variables (stocks)
tbbest.data[1:5,1:7]
rownames(tbbest.data)
tbbest1950.2018<-tbbest.data[151:219,]
str(tbbest1950.2018)
#remove columns that are entirely NAs
df[colSums(!is.na(df)) > 0]
df[, colSums(is.na(df)) < nrow(df) * 0.5]#to keep columns with at least 50% non blanks
length(which(colSums(!is.na(tbbest1950.2018)) > 0))#none of the columns are entirely NAs
length(which(colSums(is.na(tbbest1950.2018)) < 31))#only 156 stocks have <21 NAs, 179 <26 NAs,  228 stocks < 31 NAs
tb1950.2018sub<-tbbest1950.2018[,colSums(is.na(tbbest1950.2018)) < 31]
#69 obs/yrs of 228 vars/stocks. 

#from metadata, I want stockid,  primary_FAOarea and scientificname
colnames(metadata)
stk_fao_sp_key<-metadata[,c(2,13,5)]
str(stk_fao_sp_key)

tb.69ts<-as.data.frame(t(tb1950.2018sub))
tb.69ts$stockid<-rownames(tb.69ts)
#left join? with key to get fao regions and sp names
tb.69tsfaosp<-tb.69ts %>% left_join(stk_fao_sp_key, by="stockid")
length(which(is.na(tb.69tsfaosp$primary_FAOarea)))
str(tb.69tsfaosp)
tb.69tsfaosp$fao_sp<-paste(tb.69tsfaosp$primary_FAOarea,tb.69tsfaosp$scientificname,sep="_")
write.csv(tb.69tsfaosp, "biomass_fao_sp_1950-2018.csv")

## Subsetting biomass data for IOE (57)
ioe.tb.69ts<-tb.69tsfaosp %>% filter(primary_FAOarea=="57")#only 8 stocks with biomass time series

#for ioe, choose years from 1965-2014[,16:65] and fao_sp[,73]
str(ioe.tb.69ts)
colnames(ioe.tb.69ts)
ioe.tb.50ts<-ioe.tb.69ts[,c(16:65,70)]
rownames(ioe.tb.50ts)<-ioe.tb.50ts[,51]
str(ioe.tb.50ts)#df of 8 stocks over 50 years
ioe.tb.50yr.mat<-as.matrix(ioe.tb.50ts[,1:50])

##subsetting biomass data for ANE (27)
ane.tb.69ts<-tb.69tsfaosp %>% filter(primary_FAOarea=="27")#37 stocks with biomass time series
unique(ane.tb.69ts$scientificname)#16 unique species
#1965-2017 is a 53 yr timeseries. 
colnames(ane.tb.69ts)
ane.tb.53yr<-ane.tb.69ts[,c(16:68,70)]
rownames(ane.tb.53yr)<-ane.tb.53yr[,54]
ane.tb.53yr.mat<-as.matrix(ane.tb.53yr[,1:53])

##subsetting biomass data for PWC (71)
pwc.tb.69ts<-tb.69tsfaosp %>% filter(primary_FAOarea=="71")#only 4 stocks (3 tuna, 1 marlin species) with biomass time series
colnames(pwc.tb.69ts)
pwc.tb.50yr<-pwc.tb.69ts[,c(17:66,70)]
rownames(pwc.tb.50yr)<-pwc.tb.50yr[,51]
pwc.tb.50yr.mat<-as.matrix(pwc.tb.50yr[,1:50])

#####f.data timeseries
rownames(f.data)# 1948-2017 [149:218,]
f1948.2017<-f.data[149:218,]
str(f1948.2017)#70 obs/years of 478 stocks
#remove columns that are entirely NAs
length(which(colSums(is.na(f1948.2017)) < 36))
f1948.2017sub<-f1948.2017[,colSums(is.na(f1948.2017)) < 36]#70 obs/yrs of 255 vars/stocks. 

str(stk_fao_sp_key)

f.70yr<-as.data.frame(t(f1948.2017sub))
f.70yr$stockid<-rownames(f.70yr)
#left join with key to get fao regions and sp names
f.70yrfaosp<-f.70yr %>% left_join(stk_fao_sp_key, by="stockid")
length(which(is.na(f.70yrfaosp$primary_FAOarea)))#no NAs
str(f.70yrfaosp)
f.70yrfaosp$fao_sp<-paste(f.70yrfaosp$primary_FAOarea,f.70yrfaosp$scientificname,"fdat",sep="_")
f.70yrfaosp$stock_dat<-paste(f.70yrfaosp$stockid,"fdat",sep="_")

#####er.data timeseries
rownames(er.data)# 1948-2017 [77:146,]
er1948.2017<-er.data[77:146,]
str(er1948.2017)#70 obs/years of 745 stocks
#remove columns that are entirely NAs
length(which(colSums(is.na(er1948.2017)) < 21))
er1948.2017sub<-er1948.2017[,colSums(is.na(er1948.2017)) < 36]#70 obs/yrs of 381 vars/stocks. 

str(stk_fao_sp_key)

er.70yr<-as.data.frame(t(er1948.2017sub))
er.70yr$stockid<-rownames(er.70yr)
#left join with key to get fao regions and sp names
er.70yrfaosp<-er.70yr %>% left_join(stk_fao_sp_key, by="stockid")
length(which(is.na(er.70yrfaosp$primary_FAOarea)))#no NAs
str(er.70yrfaosp)
er.70yrfaosp$fao_sp<-paste(er.70yrfaosp$primary_FAOarea,er.70yrfaosp$scientificname,"erdat",sep="_")
er.70yrfaosp$stock_dat<-paste(er.70yrfaosp$stockid,"erdat",sep="_")

#combine all df of f, er and effort together, should be 8+381+255=644 stocks/rows of 75 variables.
str(effort.70yrfaosp)#dataframe
allf.70yrfaosp<-bind_rows(f.70yrfaosp, er.70yrfaosp, effort.70yrfaosp)
write.csv(allf.70yrfaosp, "allfishingeffort_fao_sp_1948-2017.csv")

##subsetting all fishing effort data for IOE (57)
ioe.allf.70yr<-allf.70yrfaosp %>% filter(primary_FAOarea=="57")#12 stocks with fishing effort time series
unique(ioe.allf.70yr$scientificname)#8 unique species out of the 12 stocks
#1960-2017 is a 57 yr timeseries. 
colnames(ioe.allf.70yr)
ioe.allf.57yr<-ioe.allf.70yr[,c(13:69,75)]
rownames(ioe.allf.57yr)<-ioe.allf.57yr[,58]
ioe.allf.57yr.mat<-as.matrix(ioe.allf.57yr[,1:57])
length(which(is.na(ioe.allf.57yr.mat)))
#total of 684 values, 68 were NAs = 9.9%

##subsetting all fishing effort data for ANE (27)
ane.allf.70yr<-allf.70yrfaosp %>% filter(primary_FAOarea=="27")#105 stocks with fishing effort time series
#can go up to 2017, try from 1968 to make a 50yr ts
unique(ane.allf.70yr$scientificname)#26 unique species out of the 105 stocks
colnames(ane.allf.70yr)
ane.allf.46yr.mat<-ane.allf.50yr.mat[,5:50]
length(which(is.na(ane.allf.46yr.mat)))
#total of 4830 values, 465 were NAs = 9.6%

##subsetting all fishing effort data for PWC (71)
pwc.allf.70yr<-allf.70yrfaosp %>% filter(primary_FAOarea=="71")#10 stocks with fishing effort time series
unique(pwc.allf.70yr$scientificname)#7 unique species out of the 10 stocks
colnames(pwc.allf.70yr)
length(which(is.na(pwc.allf.50yr.mat[,2:50])))
pwc.allf.49yr.mat<-pwc.allf.50yr.mat[,2:50]
length(which(is.na(pwc.allf.49yr.mat)))
#total of 490 values, 47 were NAs = 9.6%

#for stocks with multiple fishing ts, first use ER, if not than F
#order matrix by rownames
ane.allf.46yr.mat2.ord<-ane.allf.46yr.mat2[order(rownames(ane.allf.46yr.mat2)),]
head(ane.allf.46yr.mat2)
head(ane.allf.46yr.mat2.ord)
#separate rownames by "_"
ane.allf.46yr.ord.df<-as.data.frame(ane.allf.46yr.mat2.ord)
ane.allf.46yr.ord.df$stkid <- sub("_.*", "", rownames(ane.allf.46yr.ord.df))
ane.allf.46yr.ord.df$dat <- sub(".*_", "", rownames(ane.allf.46yr.ord.df))
unique(ane.allf.46yr.ord.df$dat)#only er and f dats
#find the replicate stocks, then delete the f.dat for replicates.
ane.allf.46yr.ord.df2<-distinct(ane.allf.46yr.ord.df, stkid, .keep_all = T)
length(which(ane.allf.46yr.ord.df$dat=="fdat"))#48 erdat, 57 fdat
length(which(ane.allf.46yr.ord.df2$dat=="fdat"))#48 erdat, 11 fdat
#seems to have worked and taken out the duplicate stocks that are fdat.
ane.allf.46yr.ord.df2$stk_dat<-paste(ane.allf.46yr.ord.df2$stkid,ane.allf.46yr.ord.df2$dat,sep="_")
rownames(ane.allf.46yr.ord.df2)<-ane.allf.46yr.ord.df2$stk_dat
colnames(ane.allf.46yr.ord.df2)
ane.allf.46yr.mat3<-as.matrix(ane.allf.46yr.ord.df2[,1:46])#should be 59 obs of 46 variables
str(ane.allf.46yr.mat3)#59 stocks from 1972-2017, 46yr ts


############### redo of biomass timeseries to only start where tc>0
rownames(pwc.tb.50yr)
str(tcbest.data)#942 stocks
colnames(tcbest.data)
head(tcbest.data)
#only keep 1950-2018 data
tcbest1950.2018<-tcbest.data[151:219,]
str(tcbest1950.2018)
tcbest.69yrs<-as.data.frame(tcbest1950.2018)
str(tcbest.69yrs)
colnames(tcbest.69yrs)
pwc.stk<-c(rownames(pwc.tb.50yr))
tcbest.69yrs.pwc<- tcbest.69yrs[, (names(tcbest.69yrs) %in% pwc.stk)]
str(tcbest.69yrs.pwc)# catch>0 for all 4 stocks

#ioe
rownames(ioe.tb.50ts)#8 stocks
ioe.stk<-c(rownames(ioe.tb.50ts))
tcbest.69yrs.ioe<- tcbest.69yrs[, (names(tcbest.69yrs) %in% ioe.stk)]
str(tcbest.69yrs.ioe)#8 stocks
#2 stocks have catch=0, BGRDRNSWWA should start from 1979, NZLINGWSE should start from 1979 also.

#ane
rownames(ane.tb.53yr)#37 stocks
ane.stk<-c(rownames(ane.tb.53yr))
tcbest.69yrs.ane<- tcbest.69yrs[, (names(tcbest.69yrs) %in% ane.stk)]
str(tcbest.69yrs.ane)#catch>0 for all stocks. 

#for ane and pwc, just use the .mat matrix. for ioe, put in NAs for the 2 stocks
str(ioe.tb.50yr.mat)
ioe.tb.50yr.mat.new<-ioe.tb.50yr.mat
ioe.tb.50yr.mat.new[1,1:14]<-NA
ioe.tb.50yr.mat.new[3,1:14]<-NA

str(ioe.tb.50yr.mat.new)#8 stocks, shortest overlapping period is 29 years
str(ane.tb.53yr.mat)#37 stocks, shortest overlapping period is 41 years
str(pwc.tb.50yr.mat)#4 stocks, shortest overlapping period is 40 years

#start function for individual pairwise coh analyses, this was run for each of the 6 matrices
tsrange=c(0,Inf)
nspp<-37
testmat<-ane.tb.53yr.mat

pvmat<-matrix(NA, nspp, nspp)
cohmat<-matrix(NA, nspp, nspp)
mnphmat<-matrix(NA, nspp, nspp)

for(ii in 2:nspp){
  for(jj in 1:(ii-1)){
    
    use<-!is.na(testmat[ii,]) & !is.na(testmat[jj,])
    tt<-as.numeric(colnames(testmat)[use])
    yy1<-testmat[ii,use]
    yy2<-testmat[jj,use]
    yy1<-cleandat(yy1,tt,clev=5)$cdat
    yy2<-cleandat(yy2,tt,clev=5)$cdat
    coh12<-coh(yy1,yy2,tt,norm="powall",sigmethod="fast")
    coh12<-bandtest(coh12,tsrange)
    pvmat[ii,jj]<-coh12$bandp$p_val
    cohmat[ii,jj]<-Mod(mean(coh12$coher))
    mnphmat[ii,jj]<-coh12$bandp$mn_phs
  }
}

rownames(ane.tb.53yr.mat)
rownames(cohmat)<-rownames(ane.tb.53yr.mat)
colnames(cohmat)<-rownames(ane.tb.53yr.mat)
ane.tb.coh.new<-cohmat
ane.tb.cohpv.new<-pvmat
ane.tb.cohmnph.new<-mnphmat
ane.tb.cohqv.new<-ane.tb.cohpv.new
ane.tb.cohqv.new[lower.tri(ane.tb.cohqv.new)]<-p.adjust(ane.tb.cohqv.new[lower.tri(ane.tb.cohqv.new)], method="fdr")
ane.tb.list<-list(ane.tb.coh.new, ane.tb.cohpv.new, ane.tb.cohqv.new, ane.tb.cohmnph.new)

length(which(!is.na(ane.tb.cohqv.new)))#666 possible pairwise obs.
length(which(ane.tb.cohqv.new<0.20))#59 out of the 666 are sig with fdr<20%, 8.86%

rownames(ioe.f.coh.new)<-gsub("_fdat", "", rownames(ioe.f.coh.new))
rownames(ioe.f.coh.new)<-gsub("_erdat", "", rownames(ioe.f.coh.new))
colnames(ioe.f.coh.new)<-rownames(ioe.f.coh.new)

#to plot synchrony matrices, using corrplot package
colbwr<-colorRampPalette(c("blue", "white", "red"))#to specify colour palette
png(filename="~/corrplot_RAM_ane_tb_fdr20_color_20200810.png", 
    width=1400, height=1300, units="px", res=120)
corrplot(ane.tb.coh.new, method="color", type="lower", tl.pos="ld", tl.srt=40, tl.offset=0.5, 
         col=colbwr(10), is.corr=TRUE, diag=F, tl.col="black", p.mat=ane.tb.cohqv.new, 
         sig.level=0.20, insig="blank", cl.ratio=0.1, tl.cex=1)
dev.off()
