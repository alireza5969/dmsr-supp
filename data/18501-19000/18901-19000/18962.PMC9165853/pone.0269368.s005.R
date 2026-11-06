# ##########################################################
# MA of TCZ treatment in COVID-19
# Alejandro, Angela, Renato, Vinay, Yuani, Michael, Adrian
# UConn, USIL, UPC
# Jan 14, 18, 25, and Feb 12 & 26, 2021
# ###########################################################

# Download package meta and metafor from Tools->Install Packages->meta/metafor->Install

# Make R package meta and metafor available
library(meta)  
library(metafor)

#upload .csv file for Mac; new .csv file 2.11.21
tcz <-read.csv(file.choose(), header=T) 
tcz
nrow(tcz)

#upload .csv using file.choose
file.choose()

#This is new file of 1.17.21
tcz <-read.csv(file="C:\\Users\\Alejandro Piscoya\\Dropbox\\Tocilizumab COVID-19\\Toci COVID-19 01.17.21.csv", header=TRUE, sep=",")
tcz
nrow(tcz)

#This is new file of 1.21.21
tcz <-read.csv(file="C:\\Users\\Alejandro Piscoya\\Dropbox\\Tocilizumab COVID-19\\Toci COVID-19 01.21.21.csv", header=TRUE, sep=",")
tcz
nrow(tcz)

#new file 1.22.21
tcz <-read.csv(file="C:\\Users\\Alejandro Piscoya\\Dropbox\\Tocilizumab COVID-19\\Toci COVID-19 01.21.21 AVH1.22.21.csv", header=TRUE, sep=",")
tcz
nrow(tcz)


################################# meta-analyses #######################


################################## mortality, subgroups by design (RCTs vs cohorts in same analysis)
mortality<-metabin(mortalitytcz, mortalitytczn, mortalityco, mortalitycon, studlab=study, sm="RR", method="inverse", 
                   method.tau="PM", hakn=TRUE, byvar=design, data=tcz)
mortality

#no HAKN adjustment (don't use this please)
#mortality2<-metabin(mortalitytcz, mortalitytczn, mortalityco, mortalitycon, studlab=study, sm="RR", method="inverse", 
#                   method.tau="PM", byvar=design, tau.common=TRUE, data=tcz)
#mortality2


#better figure; no combined effects by severity (addrow.overall=FALSE)
forest(mortality, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))


###severity subgroup in RCTs only
#select only those RCTs with mortality data. Create tcz.1
tcz.1 <-tcz[c(1:9),]  #with RECOVERY trial and Soin
nrow(tcz.1)

#MA severity subgroup RCTs; removed hakn.
mortality2<-metabin(mortalitytcz, mortalitytczn, mortalityco, mortalitycon, studlab=study, sm="RR", method="inverse", 
                    method.tau="PM", byvar=severity, data=tcz.1)
mortality2

#Figure severity subgroup RCTs
forest(mortality2, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))


###severity subgroups in cohorts only (write syntax similar as above)
tcz.1.2 <-tcz[c(10:18),]  
nrow(tcz.1.2)

#MA severity subgroup cohorts
mortality2<-metabin(mortalitytcz, mortalitytczn, mortalityco, mortalitycon, studlab=study, sm="RR", method="inverse", 
                    method.tau="PM", hakn=TRUE, byvar=severity, data=tcz.1.2)
mortality2

#CIs are too wide. removed hakn. Please use this MA in the future.
mortality2<-metabin(mortalitytcz, mortalitytczn, mortalityco, mortalitycon, studlab=study, sm="RR", method="inverse", 
                    method.tau="PM", byvar=severity, data=tcz.1.2)
mortality2

#Figure severity subgroup cohorts
forest(mortality2, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))




###RoB subgroups in RCTs

#MA RoB subgroup RCTs
mortality2<-metabin(mortalitytcz, mortalitytczn, mortalityco, mortalitycon, studlab=study, sm="RR", method="inverse", 
                    method.tau="PM", hakn=TRUE, byvar=rob, data=tcz.1)
mortality2

#Figure RoB subgroup RCTs
forest(mortality2, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))



###RoB subgroups in cohorts

#MA RoB subgroup cohorts
mortality2<-metabin(mortalitytcz, mortalitytczn, mortalityco, mortalitycon, studlab=study, sm="RR", method="inverse", 
                    method.tau="PM", hakn=TRUE, byvar=rob, data=tcz.1.2)
mortality2

#Figure RoB subgroup cohorts
forest(mortality2, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))



################################## clinical improvement

tcz3 <-tcz[c(3,4,12,17,18),]  #with RECOVERY trial; Rosas and Hermine removed after checking full publication on 2.25.21; Soin added
nrow(tcz3)


#meta-analysis
clinimprove<-metabin(clinimprovtcz, clinimprovtczn, clinimprovco, clinimprovcon, studlab=study, sm="RR", method="inverse", 
                  method.tau="PM", hakn=TRUE, byvar=design, data=tcz3)
clinimprove

#figure clinical improvement; labels flipped as clinical improv is a good outcome (i.e. we want more of them vs control)
forest(clinimprove, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors TCZ", label.left="Favors Control", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))


######severity subgroups in RCTs only
tcz3.1 <-tcz[c(3,4),]  
nrow(tcz3.1)


#severity subgroup
clinimprove<-metabin(clinimprovtcz, clinimprovtczn, clinimprovco, clinimprovcon, studlab=study, sm="RR", method="inverse", 
                     method.tau="PM", hakn=TRUE, byvar=severity, data=tcz3.1)
clinimprove

#CIs are too wide, Removed hakn. Please use this MA in the future.
clinimprove<-metabin(clinimprovtcz, clinimprovtczn, clinimprovco, clinimprovcon, studlab=study, sm="RR", method="inverse", 
                     method.tau="PM", byvar=severity, data=tcz3.1)
clinimprove


#forest plot
forest(clinimprove, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors TCZ", label.left="Favors Control", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))

###severity subgroups in cohorts only 
tcz3.2 <-tcz[c(11,16,17),]  
nrow(tcz3.2)

#severity subgroup
clinimprove<-metabin(clinimprovtcz, clinimprovtczn, clinimprovco, clinimprovcon, studlab=study, sm="RR", method="inverse", 
                     method.tau="PM", byvar=severity, data=tcz3.2)
clinimprove

#removed tau.common to allow analyses when 1 study per subgroup
clinimprove<-metabin(clinimprovtcz, clinimprovtczn, clinimprovco, clinimprovcon, studlab=study, sm="RR", method="inverse", 
                     method.tau="PM", byvar=severity, data=tcz3.2)
clinimprove

#forest plot
forest(clinimprove, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors TCZ", label.left="Favors Control", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))

####RoB subgroups RCTs
clinimprove<-metabin(clinimprovtcz, clinimprovtczn, clinimprovco, clinimprovcon, studlab=study, sm="RR", method="inverse", 
                     method.tau="PM", hakn=TRUE, byvar=rob, data=tcz3.1)
clinimprove

#forest plot (both some concerns, no need to have a figure)

forest(clinimprove, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors TCZ", label.left="Favors Control", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))


#######RoB subgroups cohorts
clinimprove<-metabin(clinimprovtcz, clinimprovtczn, clinimprovco, clinimprovcon, studlab=study, sm="RR", method="inverse", 
                     method.tau="PM", hakn=TRUE, byvar=rob, data=tcz3.2)
clinimprove

#forest plot

forest(clinimprove, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors TCZ", label.left="Favors Control", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))


####################################### clinical worsening

tcz4 <-tcz[c(1:4,9,17),]  #with RECOVERY trial, Hermine, and Soin.
nrow(tcz4)


#meta-analysis
clinworse<-metabin(clinworstcz, clinworstczn, clinworsco, clinworscon, studlab=study, sm="RR", method="inverse", 
                     method.tau="PM", hakn=TRUE, byvar=design, data=tcz4)
clinworse

#figure
forest(clinworse, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))


#######severity subgroups in RCTs only
tcz4.1 <-tcz[c(1:4,9),]  
nrow(tcz4.1)

#subgroup severity
clinworse<-metabin(clinworstcz, clinworstczn, clinworsco, clinworscon, studlab=study, sm="RR", method="inverse", 
                   method.tau="PM", hakn=TRUE, byvar=severity, data=tcz4.1)
clinworse

#CIs too wide. Removed hakn. Please use this for the future.
clinworse<-metabin(clinworstcz, clinworstczn, clinworsco, clinworscon, studlab=study, sm="RR", method="inverse", 
                   method.tau="PM", byvar=severity, data=tcz4.1)
clinworse

#forest plot
forest(clinworse, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))

#subgroup RoB
clinworse<-metabin(clinworstcz, clinworstczn, clinworsco, clinworscon, studlab=study, sm="RR", method="inverse", 
                   method.tau="PM", byvar=rob, data=tcz4.1)
clinworse

#forest plot
forest(clinworse, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))

### not made cohorts because only 1 study



##################################### mechanical ventilation

tcz7 <-tcz[c(2,4:9,13,17,18),]  #after adding RECOVERY trial, Gordon (2.25.21), Rosas (2.25.21), Soin (3.7.21)
nrow(tcz7)

#meta-analysis; incr="TACC" added as there is a study with zero events in one arm
mechvent<-metabin(mvtcz, mvtczn, mvco, mvcon, studlab=study, sm="RR", method="inverse", 
                  method.tau="PM", hakn=TRUE, incr="TACC", byvar=design, data=tcz7)
mechvent

#CIs are too broad due to small n of studies and imbalance between arms (i.e. different n). 
#Need to remove hakn adjustment
mechvent<-metabin(mvtcz, mvtczn, mvco, mvcon, studlab=study, sm="RR", method="inverse", 
                  method.tau="PM", incr="TACC", byvar=design, data=tcz7)
mechvent

#forest plot
forest(mechvent, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))


#RCT
tcz7.1 <-tcz[c(2,4:9),]  
nrow(tcz7.1)

#severity
mechvent<-metabin(mvtcz, mvtczn, mvco, mvcon, studlab=study, sm="RR", method="inverse", 
                  method.tau="PM", incr="TACC", byvar=severity, data=tcz7.1)
mechvent

#forest plot

forest(mechvent, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))

##cohorts
tcz7.2 <-tcz[c(12,16,17),]  
nrow(tcz7.2)

#severity
mechvent<-metabin(mvtcz, mvtczn, mvco, mvcon, studlab=study, sm="RR", method="inverse", 
                  method.tau="PM", incr="TACC", byvar=severity, tau.common=TRUE, data=tcz7.2)
mechvent

#removing tau.common to be able to run
mechvent<-metabin(mvtcz, mvtczn, mvco, mvcon, studlab=study, sm="RR", method="inverse", 
                  method.tau="PM", incr="TACC", byvar=severity, data=tcz7.2)
mechvent


#forest plot
forest(mechvent, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))



##RoB

#RCTs
mechvent<-metabin(mvtcz, mvtczn, mvco, mvcon, studlab=study, sm="RR", method="inverse", 
                  method.tau="PM", incr="TACC", byvar=rob, data=tcz7.1)
mechvent

#forest plot
forest(mechvent, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))


#cohorts
mechvent<-metabin(mvtcz, mvtczn, mvco, mvcon, studlab=study, sm="RR", method="inverse", 
                  method.tau="PM", incr="TACC", byvar=rob, tau.common=TRUE, data=tcz7.2)
mechvent

#tau.common removed. use this one.
mechvent<-metabin(mvtcz, mvtczn, mvco, mvcon, studlab=study, sm="RR", method="inverse", 
                  method.tau="PM", incr="TACC", byvar=rob, data=tcz7.2)
mechvent


#forest plot

forest(mechvent, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))



########################################### hospital length of stay

tcz11 <-tcz[c(5,7,12,16),]  
nrow(tcz11)


#meta-analysis
los<-metacont(hoplostczn, hoplostczm, hoplostczsd, hosploscon, hosploscom, 
              hosploscosd, studlab=study, sm="MD", method.tau="PM", byvar=design, fixed=FALSE, random=TRUE,
              data=tcz11)
los
#forest plot
forest(los, leftcols=c("studlab", "mean.e", "sd.e", "n.e", "mean.c", "sd.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Mean Difference (95% CI)", 
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(-40,40))

##RCTs
tcz11 <-tcz[c(5,7),]  
nrow(tcz11.1)

#severity
los2<-metacont(hoplostczn, hoplostczm, hoplostczsd, hosploscon, hosploscom, 
               hosploscosd, studlab=study, sm="MD", method.tau="PM", hakn=TRUE, byvar=severity, 
               data=tcz11.1)
los2
#forest
forest(los2, leftcols=c("studlab", "mean.e", "sd.e", "n.e", "mean.c", "sd.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Mean Difference (95% CI)", 
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(-40,40))

#RoB
los3<-metacont(hoplostczn, hoplostczm, hoplostczsd, hosploscon, hosploscom, 
               hosploscosd, studlab=study, sm="MD", method.tau="PM", hakn=TRUE, byvar=rob,  
               data=tcz11.1)
los3
#forest
forest(los3, leftcols=c("studlab", "mean.e", "sd.e", "n.e", "mean.c", "sd.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Mean Difference (95% CI)", 
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(-40,40))


##cohorts
tcz11 <-tcz[c(11,15),]  
nrow(tcz11.2)

#severity
los4<-metacont(hoplostczn, hoplostczm, hoplostczsd, hosploscon, hosploscom, 
               hosploscosd, studlab=study, sm="MD", method.tau="PM", hakn=TRUE, byvar=severity, 
               data=tcz11.2)
los4
#forest
forest(los4, leftcols=c("studlab", "mean.e", "sd.e", "n.e", "mean.c", "sd.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Mean Difference (95% CI)", 
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(-40,40))

#RoB
los5<-metacont(hoplostczn, hoplostczm, hoplostczsd, hosploscon, hosploscom, 
               hosploscosd, studlab=study, sm="MD", method.tau="PM", hakn=TRUE, byvar=rob, 
               data=tcz11.2)
los5
#forest
forest(los5, leftcols=c("studlab", "mean.e", "sd.e", "n.e", "mean.c", "sd.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Mean Difference (95% CI)", 
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(-40,40))


####################################### adverse events

tcz9 <-tcz[c(1:3,5,7:9),]  #Soin added
nrow(tcz9)


#meta-analysis
adveve<-metabin(advevetcz, advevetczn, adveveco, advevecon, studlab=study, sm="RR", method="inverse", 
                method.tau="PM", hakn=TRUE, data=tcz9)
adveve

#figure
forest(adveve, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = TRUE, print.byvar=TRUE, overall=TRUE, 
       col.by="black", xlim=c(0.1,10))


##severity
adveve<-metabin(advevetcz, advevetczn, adveveco, advevecon, studlab=study, sm="RR", method="inverse", 
                method.tau="PM", hakn=TRUE, byvar=severity, data=tcz9)
adveve

#forest plot
forest(adveve, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = TRUE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))

##RoB
adveve<-metabin(advevetcz, advevetczn, adveveco, advevecon, studlab=study, sm="RR", method="inverse", 
                method.tau="PM", hakn=TRUE, byvar=rob, data=tcz9)
adveve

#correct figure here by AVH:
forest(adveve, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)", 
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="6mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))



#################################### severe adverse events

tcz10 <-tcz[c(1:9),] #with RECOVERY trial; denominators of Rosas corrected (2.25.21); Soin added.
nrow(tcz10)


#meta-analysis; incr="TACC", allstudies=TRUE added for Zero events in both arms.
seradveve<-metabin(seradvevetcz, seradvevetczn, seradveveco, seradvevecon, studlab=study, sm="RR", method="inverse", 
                   method.tau="PM", hakn=TRUE, incr="TACC", allstudies=TRUE, data=tcz10)
seradveve

#figure
forest(seradveve, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=TRUE, 
       col.by="black", xlim=c(0.1,10))

##severity
seradveve<-metabin(seradvevetcz, seradvevetczn, seradveveco, seradvevecon, studlab=study, sm="RR", method="inverse", 
                   method.tau="PM", hakn=TRUE, incr="TACC", allstudies=TRUE, byvar=severity, data=tcz10)
seradveve

#CIs too wide. removed hakn. please use this in the future.
seradveve<-metabin(seradvevetcz, seradvevetczn, seradveveco, seradvevecon, studlab=study, sm="RR", method="inverse", 
                   method.tau="PM", incr="TACC", allstudies=TRUE, byvar=severity, data=tcz10)
seradveve

#Figure 
forest(seradveve, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))

##RoB
seradveve<-metabin(seradvevetcz, seradvevetczn, seradveveco, seradvevecon, studlab=study, sm="RR", method="inverse", 
                   method.tau="PM", incr="TACC", allstudies=TRUE, byvar=rob, data=tcz10)
seradveve

#correct figure here by AVH:
forest(seradveve, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)", 
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="6mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))


##################################### bacteremia/infection

tcz2 <-tcz[c(1:13,17,18),]  #after adding RECOVERY trial, and Gordon 2021 (2.25.21); plus Soin et al.
nrow(tcz2) 


# meta-analysis
bacteria<-metabin(bacteremiatcz, bacteremiatczn, bacteremiaco, bacteremiacon, studlab=study, sm="RR", method="inverse", 
                  method.tau="PM", hakn=TRUE, incr="TACC", byvar=design, data=tcz2)
bacteria


#forest plot
forest(bacteria, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))


###severity subgroup in RCTs only
#select only those RCTs with bacteremia data (8 RCTs here). Create tcz2.1
tcz2.1 <-tcz[c(1:9),]  
nrow(tcz2.1)

#MA severity subgroup RCTs
bacteria2<-metabin(bacteremiatcz, bacteremiatczn, bacteremiaco, bacteremiacon, studlab=study, sm="RR", method="inverse", 
                   method.tau="PM", hakn=TRUE, byvar=severity, data=tcz2.1)
bacteria2

#too wide CIs for RCTs, removed hakn. PLEASE USE THIS FOR FIGURE (AVH 2.11.21)
bacteria2<-metabin(bacteremiatcz, bacteremiatczn, bacteremiaco, bacteremiacon, studlab=study, sm="RR", method="inverse", 
                   method.tau="PM", byvar=severity, data=tcz2.1)
bacteria2


#Figure severity subgroup RCTs
forest(bacteria2, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))


###severity subgroups in cohorts only (write syntax similar as above)
tcz2.2 <-tcz[c(10:13,17,18),]  
nrow(tcz2.2)

#MA severity subgroup cohorts
bacteria2<-metabin(bacteremiatcz, bacteremiatczn, bacteremiaco, bacteremiacon, studlab=study, sm="RR", method="inverse", 
                   method.tau="PM", hakn=TRUE, byvar=severity, data=tcz2.2)
bacteria2

#too wide CI for subgroups. Removed hakn. Please use this MA for the future:
bacteria2<-metabin(bacteremiatcz, bacteremiatczn, bacteremiaco, bacteremiacon, studlab=study, sm="RR", method="inverse", 
                   method.tau="PM", byvar=severity, data=tcz2.2)
bacteria2

#Figure severity subgroup cohort
forest(bacteria2, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))


##RoB subgroup RCTs
bacteria2<-metabin(bacteremiatcz, bacteremiatczn, bacteremiaco, bacteremiacon, studlab=study, sm="RR", method="inverse", 
                   method.tau="PM", hakn=TRUE, byvar=rob, data=tcz2.1)
bacteria2

#CIs to wide. removed hakn.
bacteria2<-metabin(bacteremiatcz, bacteremiatczn, bacteremiaco, bacteremiacon, studlab=study, sm="RR", method="inverse", 
                   method.tau="PM", byvar=rob, data=tcz2.1)
bacteria2

#Figure RoB subgroup RCTs
forest(bacteria2, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))


###RoB subgroups in cohorts
bacteria2<-metabin(bacteremiatcz, bacteremiatczn, bacteremiaco, bacteremiacon, studlab=study, sm="RR", method="inverse", 
                   method.tau="PM", hakn=TRUE, byvar=rob, data=tcz2.2)
bacteria2

#CIs are too wide. hakn removed. please use this MA for the future.
bacteria2<-metabin(bacteremiatcz, bacteremiatczn, bacteremiaco, bacteremiacon, studlab=study, sm="RR", method="inverse", 
                   method.tau="PM", byvar=rob, data=tcz2.2)
bacteria2

#Figure RoB subgroup in cohorts
forest(bacteria2, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))


########################################### neutropenia

tcz8 <-tcz[c(1,3,4,7,17,18),]  #with RECOVERY trial 
nrow(tcz8)


#meta-analysis; incr="TACC", allstudies=TRUE added as there zero events in both arms
neutrop<-metabin(neutroptcz, neutroptczn, neutropco, neutropcon, studlab=study, sm="RR", method="inverse", 
                 method.tau="PM", hakn=TRUE, incr="TACC", allstudies=TRUE, byvar=design, data=tcz8)
neutrop

#CIs are too broad due to small n of studies and imbalance between arms (i.e. different n). 
#Need to remove hakn adjustment
neutrop<-metabin(neutroptcz, neutroptczn, neutropco, neutropcon, studlab=study, sm="RR", method="inverse", 
                 method.tau="PM", incr="TACC", allstudies=TRUE, byvar=design, data=tcz8)
neutrop

#figure
forest(neutrop, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))

#alternative figure with different axis limits (use this one)
forest(neutrop, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.02,50))


#RCTs
tcz8.1 <-tcz[c(1,3,4,7),]  
nrow(tcz8.1)

#severity
neutrop<-metabin(neutroptcz, neutroptczn, neutropco, neutropcon, studlab=study, sm="RR", method="inverse", 
                 method.tau="PM", incr="TACC", allstudies=TRUE, byvar=severity, data=tcz8.1)
neutrop

#forest plot
forest(neutrop, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.02,50))

#cohorts
tcz8.2 <-tcz[c(16,17),]  #with RECOVERY trial
nrow(tcz8.2)

#severity
neutrop<-metabin(neutroptcz, neutroptczn, neutropco, neutropcon, studlab=study, sm="RR", method="inverse", 
                 method.tau="PM", incr="TACC", allstudies=TRUE, byvar=severity, tau.common=TRUE, data=tcz8.2)
neutrop

#removed: tau.common. use this one please.
neutrop<-metabin(neutroptcz, neutroptczn, neutropco, neutropcon, studlab=study, sm="RR", method="inverse", 
                 method.tau="PM", incr="TACC", allstudies=TRUE, byvar=severity, data=tcz8.2)
neutrop

#forest plot. UGLY FIGURE, NO EVENTS. N/A IN SUPPLEMENT. same for RoB.
forest(neutrop, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))



#RoB RCTs
neutrop<-metabin(neutroptcz, neutroptczn, neutropco, neutropcon, studlab=study, sm="RR", method="inverse", 
                 method.tau="PM", incr="TACC", allstudies=TRUE, byvar=rob, data=tcz8.1)
neutrop

#forest plot
forest(neutrop, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.02,50))

#RoB cohorts
neutrop<-metabin(neutroptcz, neutroptczn, neutropco, neutropcon, studlab=study, sm="RR", method="inverse", 
                 method.tau="PM", incr="TACC", allstudies=TRUE, byvar=rob, data=tcz8.2)
neutrop

#forest plot

forest(neutrop, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))


######################################### bleeding events

tcz6 <-tcz[c(2,4,6,7,13),]  #with RECOVERY trial and Gordon 2021 (2.25.21)
nrow(tcz6)


#meta-analysis; incr="TACC" added as there is a study with zero events in one arm 
bleed<-metabin(bleedtcz, bleedtczn, bleedco, bleedcon, studlab=study, sm="RR", method="inverse", 
               method.tau="PM", hakn=TRUE, incr="TACC", byvar=design, data=tcz6)
bleed

#CIs are too broad due to small n of studies and imbalance between arms (i.e. different n). 
#Need to remove hakn adjustment
bleed<-metabin(bleedtcz, bleedtczn, bleedco, bleedcon, studlab=study, sm="RR", method="inverse", 
               method.tau="PM", incr="TACC", byvar=design, data=tcz6)
bleed

#forest plot
forest(bleed, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))


#RCTs only
tcz6.1 <-tcz[c(2,4,6,7),]  
nrow(tcz6.1)

#severity
bleed2<-metabin(bleedtcz, bleedtczn, bleedco, bleedcon, studlab=study, sm="RR", method="inverse", 
                method.tau="PM", incr="TACC", byvar=severity, data=tcz6.1)
bleed2

#forest plot
forest(bleed2, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))

##RoB
bleed3<-metabin(bleedtcz, bleedtczn, bleedco, bleedcon, studlab=study, sm="RR", method="inverse", 
                method.tau="PM", incr="TACC", byvar=rob, tau.common=TRUE, data=tcz6.1)
bleed3

#tau.common removed. use this one!
bleed3<-metabin(bleedtcz, bleedtczn, bleedco, bleedcon, studlab=study, sm="RR", method="inverse", 
                method.tau="PM", incr="TACC", byvar=rob, data=tcz6.1)
bleed3

#forest plot
forest(bleed3, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))



############################### thrombotic events

tcz5 <-tcz[c(1,4,6,7,9,11,17,18),]  #with RECOVERY trial & Gordon 2021 (2.26.21); Soin added.
nrow(tcz5)


#meta-analysis; incr="TACC" added as there is a study with zero events in one arm 
throm<-metabin(thromtcz, thromtczn, thromco, thromcon, studlab=study, sm="RR", method="inverse", 
                   method.tau="PM", hakn=TRUE, incr="TACC", byvar=design, data=tcz5)
throm

#CIs are too broad due to small n of studies and imbalance between arms (i.e. different n). 
#Needed to remove hakn adjustment
throm<-metabin(thromtcz, thromtczn, thromco, thromcon, studlab=study, sm="RR", method="inverse", 
               method.tau="PM", incr="TACC", byvar=design, data=tcz5)

#forest plot
forest(throm, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))


#RCT
tcz5.1 <-tcz[c(1,4,6,7,9),]  
nrow(tcz5.1)

#severity
throm<-metabin(thromtcz, thromtczn, thromco, thromcon, studlab=study, sm="RR", method="inverse", 
               method.tau="PM", incr="TACC", byvar=severity, data=tcz5.1)
throm

#forest plot
forest(throm, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))



#Cohort severity 
tcz5.2 <-tcz[c(10,16,17),]  #with RECOVERY trial
nrow(tcz5.2)

throm<-metabin(thromtcz, thromtczn, thromco, thromcon, studlab=study, sm="RR", method="inverse", 
               method.tau="PM", incr="TACC", byvar=severity, tau.common=TRUE, data=tcz5.2)
throm

#removed tau.common.please use this one.
throm<-metabin(thromtcz, thromtczn, thromco, thromcon, studlab=study, sm="RR", method="inverse", 
               method.tau="PM", incr="TACC", byvar=severity, data=tcz5.2)
throm

#forest plot
forest(throm, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))

#RCT RoB

#severity
throm<-metabin(thromtcz, thromtczn, thromco, thromcon, studlab=study, sm="RR", method="inverse", 
               method.tau="PM", incr="TACC", byvar=rob, tau.common=TRUE, data=tcz5.1)
throm

#removed tau.common. please us this one.
throm<-metabin(thromtcz, thromtczn, thromco, thromcon, studlab=study, sm="RR", method="inverse", 
               method.tau="PM", incr="TACC", byvar=rob, data=tcz5.1)
throm

#forest plot
forest(throm, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))


#Cohorts RoB
tcz5.2 <-tcz[c(10,16,17),]  #with RECOVERY trial
nrow(tcz5.2)

#RoB
throm<-metabin(thromtcz, thromtczn, thromco, thromcon, studlab=study, sm="RR", method="inverse", 
               method.tau="PM", incr="TACC", byvar=rob, data=tcz5.2)
throm

#forest plot
forest(throm, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))



####################################### abnormal liver function

tcz88 <-tcz[c(2:4,7,11,17,18),]  #with RECOVERY trial and Gordon 2021 (2.25.21)
nrow(tcz88)


#meta-analysis; incr="TACC" added as there is a study with zero events in one arm 
liver<-metabin(abnlivertcz, abnlivertczn, abnliverco, abnlivercon, studlab=study, sm="RR", method="inverse", 
               method.tau="PM", hakn=TRUE, incr="TACC", byvar=design, data=tcz88)
liver

#CIs are too broad due to small n of studies and imbalance between arms (i.e. different n). 
#Need to remove hakn adjustment
liver<-metabin(abnlivertcz, abnlivertczn, abnliverco, abnlivercon, studlab=study, sm="RR", method="inverse", 
               method.tau="PM", incr="TACC", byvar=design, data=tcz88)
liver

#forest plot
forest(liver, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))


##RCTs only
tcz88.1 <-tcz[c(2:4,7),]  
nrow(tcz88.1)

#severity
liver2<-metabin(abnlivertcz, abnlivertczn, abnliverco, abnlivercon,  studlab=study, sm="RR", method="inverse", 
                method.tau="PM", incr="TACC", byvar=severity, data=tcz88.1)
liver2

#forest plot
forest(liver2, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))

##RoB
liver3<-metabin(abnlivertcz, abnlivertczn, abnliverco, abnlivercon,  studlab=study, sm="RR", method="inverse", 
                method.tau="PM", incr="TACC", byvar=rob, tau.common=TRUE, data=tcz88.1)
liver3

#tau.common removed. use this one!
liver3<-metabin(abnlivertcz, abnlivertczn, abnliverco, abnlivercon, studlab=study, sm="RR", method="inverse", 
                method.tau="PM", incr="TACC", byvar=rob, data=tcz88.1)
liver3

#forest plot
forest(liver3, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))

##cohorts only
tcz88.2 <-tcz[c(10,16,17),]  
nrow(tcz88.2)

#severity
liver4<-metabin(abnlivertcz, abnlivertczn, abnliverco, abnlivercon, studlab=study, sm="RR", method="inverse", 
                method.tau="PM", incr="TACC", byvar=severity, data=tcz88.2)
liver4

#forest plot
forest(liver4, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))

##RoB
liver5<-metabin(abnlivertcz, abnlivertczn, abnliverco, abnlivercon,  studlab=study, sm="RR", method="inverse", 
                method.tau="PM", incr="TACC", byvar=rob, tau.common=TRUE, data=tcz88.2)
liver5

#tau.common removed. use this one!
liver5<-metabin(abnlivertcz, abnlivertczn, abnliverco, abnlivercon, studlab=study, sm="RR", method="inverse", 
                method.tau="PM", incr="TACC", byvar=rob, data=tcz88.2)
liver5

#forest plot
forest(liver5, leftcols=c("studlab", "event.e", "n.e", "event.c", "n.c", "effect.ci"), 
       rightcols=c("w.random"), comb.fixed=FALSE, comb.random=TRUE, xlab="Risk Ratio (95% CI)",
       col.diamond="lightblue", col.square="darkblue", addrow=FALSE, 
       label.right="Favors Control", label.left="Favors TCZ", bottom.lr=FALSE, ff.lr="bold", 
       colgap="3mm", lab.e="Tocilizumab", lab.c="Control", leftlabs=c("Source", NA, NA, NA, NA, NA), 
       pooled.events=TRUE, addrow.overall = FALSE, print.byvar=TRUE, overall=FALSE, 
       col.by="black", xlim=c(0.1,10))


###### end