#### Script for NMA analysis of BWLT for behavioral symptoms of TBI ########
Sys.time()
library(netmeta)
library(readxl)

data <- read_excel("D:/OneDrive - Chiang Mai University/0DiscE_don'tdelete/PoomResearch/PoomLightForTBI/data.light.xlsx")

##########
## see all treatment ###
## t1	t2	t3
## BWLT	ALT	NLT
## BWLT	NLT	
## BWLT	ALT	
## BWLT	ALT	


## define treatment set
ts <- c("BWLT", "ALT", "NLT")

###############################################################
## Transform data from arm-based format to contrast-based format
## Pairwise data set
# Outcome1:Sleepiness
p1 <- pairwise(treat=list(t1, t2, t3),
               mean=list(m.spn.t1, m.spn.t2, m.spn.t3),
               sd=list(sd.spn.t1, sd.spn.t2, sd.spn.t3),
               n=list(n.t1, n.t2, n.t3),
               studlab=study, data=data, sm="SMD")
# Outcome2: Sleep Disturbance
p2 <- pairwise(treat=list(t1, t2, t3), 
               mean=list(m.sd.t1, m.sd.t2, m.sd.t3),
               sd=list(sd.sd.t1, sd.sd.t2, sd.sd.t3), 
               n=list(n.t1, n.t2, n.t3),
               studlab=study, data=data, sm="SMD")
# Outcome3: Depression
p3 <- pairwise(treat=list(t1, t2, t3), 
               mean=list(m.dep.t1, m.dep.t2, m.dep.t3),
               sd=list(sd.dep.t1, sd.dep.t2, sd.dep.t3), 
               n=list(n.t1, n.t2, n.t3),
               studlab=study, data=data, sm="SMD")
# Outcome4: Fatigue
p4 <- pairwise(treat=list(t1, t2, t3), 
               mean=list(m.fat.t1, m.fat.t2, m.fat.t3),
               sd=list(sd.fat.t1, sd.fat.t2, sd.fat.t3), 
               n=list(n.t1, n.t2, n.t3),
               studlab=study, data=data, sm="SMD")

##############################################
## conduct random-effect NMAs
# Outcome1: Sleepiness
net1 <- netmeta(p1, comb.fixed=FALSE, reference="BWLT")
# Outcome2: Sleep Disturbance
net2 <- netmeta(p2, comb.fixed=FALSE, reference="BWLT")
# Outcome3: Depression
net3 <- netmeta(p3, comb.fixed=FALSE, reference="BWLT")
# Outcome4: Fatigue
net4 <- netmeta(p4, comb.fixed=FALSE, reference="BWLT")
############################
## geometry plots
# Outcome1: Sleepiness
netgraph(net1, cex = 1, scale = 0.7, col = "blue", 
         thickness = "number.of.studies", lwd = 4, points = TRUE, cex.points = 3, 
         plastic = FALSE, col.multiarm = "gray80",  col.points = "red",
         number.of.studies = TRUE, col.number.of.studies = "yellow",
         bg.number.of.studies = "blue", pos.number.of.studies = 0.5)
  
# Outcome2: Sleep Disturbance
netgraph(net2, cex = 1, scale = 0.7, col = "blue", 
         thickness = "number.of.studies", lwd = 2, points = TRUE, cex.points = 3, 
         plastic = FALSE, col.multiarm = "gray80",  col.points = "red",
         number.of.studies = TRUE, col.number.of.studies = "yellow",
         bg.number.of.studies = "blue", pos.number.of.studies = 0.5)
# Outcome3: Depression
netgraph(net3, cex = 1, scale = 0.7, col = "blue", 
         thickness = "number.of.studies", lwd = 2, points = TRUE, cex.points = 3, 
         plastic = FALSE, col.multiarm = "gray80",  col.points = "red",
         number.of.studies = TRUE, col.number.of.studies = "yellow",
         bg.number.of.studies = "blue", pos.number.of.studies = 0.5)
# Outcome4: Fatigue
netgraph(net4, cex = 1, scale = 0.7, col = "blue", 
         thickness = "number.of.studies", lwd = 2, points = TRUE, cex.points = 3, 
         plastic = FALSE, col.multiarm = "gray80",  col.points = "red",
         number.of.studies = TRUE, col.number.of.studies = "yellow",
         bg.number.of.studies = "blue", pos.number.of.studies = 0.5)
###############################################
## summary estimates of random-effect NMAs
# Outcome1: Sleepiness
print(summary(net1), digits=2)
### Ex. of output interpretation
### Quantifying heterogeneity / inconsistency:
###  tau^2 = 1.4810; tau = 1.2170; I^2 = 87.7% [70.7%; 94.8%]
###
### Tests of heterogeneity (within designs) and inconsistency (between designs):
###  Q d.f.  p-value
### Total           24.35    3 < 0.0001
### Within designs  13.45    1   0.0002
### Between designs 10.90    2   0.0043
### net1 had significant heterogeneity/inconsistency of results 
### (I^2=???87.4%, Q=24.35, p<0.0001). There were heterogeneity between
### studies of the same design (Q within designs=13.45, p=0.0002) and 
### (inconsistency between studies of different designs (Q between 
### designs=10.90, p=0.0043). NB: see decomp.design results for the
### design causing the inconsistency between designs.  

# OUtcome2: Sleep Disturbance
print(summary(net2), digits=2)
# Outcome3: Depression
print(summary(net3), digits=2)
# Outcome4: Fatigue
print(summary(net4), digits=2)
#############################################
## Consistency
## general approach: decomposition
## Krahn U, et al. (2013): A graphical tool for locating inconsistency in 
## network meta-analyses. BMC Medical Research Methodology, 13, 35

# Outcome1: Sleepiness
decomp.design(net1)
### Ex of output interpretation
### Q decreased markedly after detaching the design of 
### Blue Light:Amber Light:No Light
### (Q=0.00, p= --), we conclude that this study design contributed
### to between-design inconsistency.

# Outcome2: Sleep Disturbance
decomp.design(net2)

# Outcome3: Depression
decomp.design(net3)

# Outcome4: Fatigue
decomp.design(net4)
## Consistency
## local approach: netsplit
## visualize split direct and indirect evidence (netsplit) to check for consistency
## Dias S, Welton NJ, Caldwell DM, Ades AE (2010): Checking consistency in 
## mixed treatment comparison meta-analysis. Statistics in Medicine, 29, 932-44
## Efthimiou O, Rücker G, Schwarzer G, Higgins J, Egger M, Salanti G (2019): 
## A Mantel-Haenszel model for network meta-analysis of rare events. 
## Statistics in Medicine, 1-21, https://doi.org/10.1002/sim.8158
# Outcome1: Sleepiness
print(netsplit(net1), digits = 2, ci = TRUE, comb.fixed = FALSE, test = TRUE)
### Ex of output interpretation
### P-values of all comparisons > 0.05 suggest no significant inconsistency between
### direct and indirect evidence.

# Outcome2: Sleep Disturbance
print(netsplit(net2), digits = 2, ci = TRUE, comb.fixed = FALSE, test = TRUE)

# Outcome3: Depression
print(netsplit(net3), digits = 2, ci = TRUE, comb.fixed = FALSE, test = TRUE)

# Outcome4: Fatigue
print(netsplit(net4), digits = 2, ci = TRUE, comb.fixed = FALSE, test = TRUE)

######################
## plot forests 
## From https://rdrr.io/cran/netmeta/man/forest.netsplit.html
## Forest plot showing comparisons contributing both direct and
## indirect evidence

# Outcome1: Sleepiness
forest(net1, ref="BWLT", xlab="SMD for Sleepiness (>0 favors BWLT)", 
       leftcols=c("studlab", "k"), sortvar=-Pscore, 
       rightcols = c("effect", "ci"),drop=TRUE)
ns1 <- netsplit(net1)
forest(ns1, show="all", fontsize=6, spacing=0.5, addrow.subgroups=FALSE)

# Outcome2: Sleep Disturbance
forest(net2, ref="BWLT", xlab="SMD for Sleep Disturbance (>0 favors BWLT)", 
       leftcols=c("studlab", "k"), sortvar=-Pscore, 
       rightcols = c("effect", "ci"),drop=TRUE)
ns2 <- netsplit(net2)
forest(ns2, show="all", fontsize=6, spacing=0.5, addrow.subgroups=FALSE)

# Outcome3: Depression
forest(net3, ref="BWLT", xlab="SMD for Depression (>0 favors BWLT)", 
       leftcols=c("studlab", "k"), sortvar=-Pscore, 
       rightcols = c("effect", "ci"),drop=TRUE)
ns3 <- netsplit(net3)
forest(ns3, show="all", fontsize=6, spacing=0.5, addrow.subgroups=FALSE)

# Outcome4: Fatigue
forest(net4, ref="BWLT", xlab="SMD for Fatigue (>0 favors BWLT)", 
       leftcols=c("studlab", "k"), sortvar=-Pscore, 
       rightcols = c("effect", "ci"), drop=TRUE)
ns4 <- netsplit(net4)
forest(ns4, show="all", fontsize=6, spacing=0.5, addrow.subgroups=FALSE)

#########################################################################
## Prepare league tables 
# Outcome1: Sleepiness
netleague(net1, digits=2, seq = netrank(net1))
league1 <- netleague(net1, digits = 2, seq = netrank(net1),
                     bracket = "(", separator = " to ")
write.table(league1$random, file = "league1-random.t.csv",
            row.names = FALSE, col.names = FALSE,
            sep = ",")

# Outcome2: Sleep Disturbance
netleague(net2, digits=2, seq = netrank(net2))
league2 <- netleague(net2, digits = 2, seq = netrank(net2),
                     bracket = "(", separator = " to ")
write.table(league2$random, file = "league2-random.t.csv",
            row.names = FALSE, col.names = FALSE,
            sep = ",")

# Outcome3: Depression
netleague(net3, digits=2, seq = netrank(net3))
league3 <- netleague(net3, digits = 2, seq = netrank(net3),
                     bracket = "(", separator = " to ")
write.table(league3$random, file = "league3-random.t.csv",
            row.names = FALSE, col.names = FALSE,
            sep = ",")

# Outcome4: Fatigue
netleague(net4, digits=2, seq = netrank(net4))
league4 <- netleague(net4, digits = 2, seq = netrank(net4),
                     bracket = "(", separator = " to ")
write.table(league4$random, file = "league4-random.t.csv",
            row.names = FALSE, col.names = FALSE,
            sep = ",")
###################################
## plot funnels
# Outcome1: Sleepiness
f1 <-funnel(net1, order=ts, linreg = FALSE, rank = FALSE, mm = FALSE,
            xlab="SMD for Sleepiness, SMD centered at comparison-specific effect",
            pos.legend = "topleft")

# Outcome2: Sleep Disturbance
f2 <-funnel(net2, order=ts, linreg = FALSE, rank = FALSE, mm = FALSE,
            xlab="SMD for Sleep Disturbance, SMD centered at comparison-specific effect",
            pos.legend = "topleft")

# Outcome3: Depression
f3 <-funnel(net3, order=ts, linreg = FALSE, rank = FALSE, mm = FALSE,
            xlab="SMD for Depression, SMD centered at comparison-specific effect",
            pos.legend = "topleft")

# Outcome4: Fatigue
f4 <-funnel(net4, order=ts, linreg = FALSE, rank = FALSE, mm = FALSE,
            xlab="SMD for Fatigue, SMD centered at comparison-specific effect",
            pos.legend = "topleft")

###############################################
## setting data subset for meta-analysis 
## Outcome5: dropout rates (BWLT vs. ALT only)
library(meta)
data1 <- subset(data, id %in% c(3, 4))
m1 <- metabin(do.t1, n.t1, do.t2, n.t2,
               data = data1, sm = "RR", studlab = study)
summary(m1)
forest(m1, digits = 2, 
        lab.e="BWLT", lab.c="ALT", xlab="Dropout Rates (<1 favors BWLT)")
funnel(m1, comb.random = TRUE)

funnel(m1, comb.random = TRUE, linreg = FALSE, rank = FALSE, mm = FALSE,
  xlab="RR for Dropout Rates, RR centered at comparison-specific effect")

##### End (not run) ##########################
