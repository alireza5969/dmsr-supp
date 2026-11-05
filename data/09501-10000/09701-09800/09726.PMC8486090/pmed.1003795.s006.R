library(meta)
library(metafor)

# For MAC only
setwd('/Users/jacklo/Dropbox/2.\ Work/4.\ CNOC/Review/Helmet laws/')

####
# Forest plots
####

df=NULL
df=read.csv("r.csv", header = TRUE, fileEncoding="UTF-8-BOM")

run_forest <- function(v1,v2,v3,v4,df,l) {

        # Switch label
        if (l == 1) {
            label1 = "Favors pre-law"     
            label2 = 'Favors post-law'
        } else {
            label1 = "Favors post-law"     
            label2 = 'Favors pre-law'    
        }
        
        # generate abcd table
        df$a = v1
        df$b = v2 - v1
        df$c = v3
        df$d = v4 - v3

        # relabel income categories
        df$income[df$income==2] <- 'LMIC'
        df$income[df$income==1] <- 'HIC'

        # Calculate OR
        df <- escalc(measure="OR", ai=a, bi=b, ci=c, di=d, data=df) #yi = effect size #vi = variance
        df$sei <- sqrt(df$vi)

        # Prediction interval
        print('+++Prediction interval for HIC+++')
        print(metagen(-yi, sei, studlab=study,
                method.tau = "DL", # DerSimonian-Laird estimator
                sm="OR", # presents as OR rather than log-transformed OR
                prediction=TRUE,
                data=df[df$income=='HIC',]))
        
        print('+++Prediction interval for LMIC+++')
        print(metagen(-yi, sei, studlab=study,
                      method.tau = "DL", # DerSimonian-Laird estimator
                      sm="OR", # presents as OR rather than log-transformed OR
                      prediction=TRUE,
                      data=df[df$income=='LMIC',]))
        

        # Using the metagen from the 'meta' package as I like this interface a bit better
        # Alternative is the native 'rma' function from 'metafor'
        res <- metagen(-yi, sei, studlab=study,
                       byvar=income,
                       method.tau = "DL", # DerSimonian-Laird estimator
                       sm="OR", # presents as OR rather than log-transformed OR
                       prediction=TRUE,
                       data=df[!is.na(v1),])

        # This one pools everything together without stratifying by income
        res2 <- metagen(-yi, sei, studlab=study,
                        #byvar=income,
                        method.tau = "DL", # DerSimonian-Laird estimator
                        sm="OR", # presents as OR rather than log-transformed OR
                        prediction=TRUE,
                        data=df[!is.na(v1),])

        # Forest plot, and graphical setting
        forest(res,
               transf=exp,
               label.left=label1,
               label.right=label2,
               comb.fixed = FALSE,
               comb.random = TRUE,
               print.byvar = FALSE, bysort=TRUE, col.by = 'black',
               weight.study = 'random',
               resid.hetstat = FALSE,    # residual heterogeneity
               pooled.events = TRUE,
               print.tau2 = FALSE,
               print.Q = TRUE, print.pval.Q = TRUE, scientific.pval = FALSE,
               col.square = "black",
               col.square.lines = "black",
               #sortvar='publication',
               subgroup=TRUE,
               overall=TRUE,
               prediction=FALSE, # don't print out prediction interval
               leftcols=c("studlab"),
               colgap.forest.left = "35mm")

        print('+++ OVERALL +++')
        print(res)
        funnel(res, comb.fixed=FALSE, comb.random=TRUE)

        print('++Egger++')
        print(metabias(res2, method.bias="linreg", k.min=4, plotit=TRUE))
        
        print('++Begg++')
        print(metabias(res2, method.bias="rank", k.min=4, plotit=TRUE))

        #### METAREG
        print('Metaregression')
        print('+++Enactment gap+++')
        print(metareg(res, method.tau = 'DL', ~ enactment_gap))
        
        print('+++NOS+++')
        print(metareg(res, method.tau = 'DL', ~ nos))
        
        print('+++Income+++')
        print(metareg(res, method.tau = 'DL', ~ income))
        
        print('+++Income and Enactment gap+++')
        print(metareg(res, method.tau = 'DL', ~ income + enactment_gap))
        
        print('+++Income and NOS+++')
        print(metareg(res, method.tau = 'DL', ~ income + nos))
        
        #### Exclude kraus ####
        res <- metagen(-yi, sei, studlab=study,
                       byvar=income,
                       method.tau = "DL", # DerSimonian-Laird estimator
                       sm="OR", # presents as OR rather than log-transformed OR
                       prediction=TRUE,
                       data=df[!is.na(v1) & df$study!= 'Kraus J., 1995',])
        
        print('+++ Exclude Kraus+++')
        print(res)
        
        ### By TBI type ###
        
        res <- metagen(-yi, sei, studlab=study,
                       byvar=tbi_type,
                       method.tau = "DL", # DerSimonian-Laird estimator
                       sm="OR", # presents as OR rather than log-transformed OR
                       prediction=TRUE,
                       data=df[!is.na(df$tbi_type),])
        
        print('+++ by tbi type +++')
        print(res)
        
}

run_forest(df$h_pre,df$h_preN,df$h_post,df$h_postN,df,1)
run_forest(df$mf_pre,df$mf_preN,df$mf_post,df$mf_postN,df,2)
run_forest(df$tbi_pre,df$tbi_preN,df$tbi_post,df$tbi_postN,df,2)
