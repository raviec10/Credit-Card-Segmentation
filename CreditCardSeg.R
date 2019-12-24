# Credit Card Segmentation

rm(list=ls())

setwd("D:/ANALYTIX LAB/R/R Final Case Study/Credit Card Segmentation")
custseg= read.csv(file.choose(),header = TRUE)

View(custseg)
str(custseg)

custseg$CUST_ID = NULL

############ creating new variables and validating the values ################

#(1) Monthly average purchase and cash advance amount

custseg$m_avg_purchase= custseg$PURCHASES/custseg$TENURE
custseg$m_avg_cash_adamount = custseg$CASH_ADVANCE/custseg$TENURE

#(2) Purchases by type
# "0"- OneOff purchase
# "1"- Installments purchase
# "2"- Neither of above two

custseg$PURCHASE_TYPE = ifelse(custseg$ONEOFF_PURCHASES>0 & 
                             custseg$INSTALLMENTS_PURCHASES==0,0,
                           ifelse(custseg$ONEOFF_PURCHASES==0 & 
                                    custseg$INSTALLMENTS_PURCHASES >0,1,2))

#(3) Average amount per purchase and Cash advance transaction

custseg$avg_amt_per_purchase = custseg$PURCHASES/custseg$PURCHASES_TRX
custseg$avg_amt_per_purchase[is.na(custseg$avg_amt_per_purchase)] = 0

custseg$avg_amt_per_purchase[!is.finite(custseg$avg_amt_per_purchase)] = 0

custseg$avg_cash_advance_amt = custseg$CASH_ADVANCE/custseg$CASH_ADVANCE_TRX
custseg$avg_cash_advance_amt[!is.finite(custseg$avg_cash_advance_amt)] = 0

#(4) Balance to credit limit ratio

custseg$limit_usage =custseg$BALANCE/custseg$CREDIT_LIMIT

# (5) Payments to minimum payments ratio

custseg$pay_ratio = custseg$PAYMENTS/custseg$MINIMUM_PAYMENTS

View(custseg)

# Creating user defined function for descriptive statistics

mystats = function(x) {
  nmiss=sum(is.na(x))
  a = x[!is.na(x)]
  m = mean(a)
  n = length(a)
  s = sd(a)
  min = min(a)
  pctls=quantile(a,probs=c(0.01, 0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99))
  max = max(a)
  UC1 = m+2*s
  LC1 = m-2*s
  UC2 = m+3*s
  LC2 = m-3*s
  outlier_flag1= max>UC1 | min<LC1
  outlier_flag2= max>UC2 | min<LC2
  return(c(n=n, nmiss=nmiss, outlier_flag1=outlier_flag1, outlier_flag2=outlier_flag2, mean=m, stdev=s,min = min, pctls=pctls,max=max, UC1=UC1, LC1=LC1, UC2=UC2, LC2=LC2))
}

vars=c("BALANCE","BALANCE_FREQUENCY",
        "PURCHASES","ONEOFF_PURCHASES","INSTALLMENTS_PURCHASES",
        "CASH_ADVANCE","PURCHASES_FREQUENCY","ONEOFF_PURCHASES_FREQUENCY",
        "PURCHASES_INSTALLMENTS_FREQUENCY","CASH_ADVANCE_FREQUENCY","CASH_ADVANCE_TRX",
        "PURCHASES_TRX","CREDIT_LIMIT","PAYMENTS",
        "MINIMUM_PAYMENTS","PRC_FULL_PAYMENT","TENURE",
        "m_avg_purchase","m_avg_cash_adamount","avg_amt_per_purchase","avg_cash_advance_amt","limit_usage",
        "pay_ratio")

diagnostic_stats=t(data.frame(apply(custseg[vars], 2, mystats)))

write.csv(diagnostic_stats, "diagnostic_stats.csv")

options(scipen = 999)
View(diagnostic_stats)


# Outliers Treatment
#Replacing data points greater than UC1 with UC1

custseg$BALANCE[custseg$BALANCE>5727.538587]=5727.538587
custseg$BALANCE_FREQUENCY[custseg$BALANCE_FREQUENCY>1.3510787]=1.3510787
custseg$PURCHASES[custseg$PURCHASES>5276.474397]=5276.474397
custseg$ONEOFF_PURCHASES[custseg$ONEOFF_PURCHASES>3912.213206]=3912.213206
custseg$INSTALLMENTS_PURCHASES[custseg$INSTALLMENTS_PURCHASES>2219.743875]=2219.743875
custseg$CASH_ADVANCE[custseg$CASH_ADVANCE>5173.198866]=5173.198866
custseg$ONEOFF_PURCHASES_FREQUENCY[custseg$ONEOFF_PURCHASES_FREQUENCY>0.799129814]=0.799129814
custseg$CASH_ADVANCE_FREQUENCY[custseg$CASH_ADVANCE_FREQUENCY>0.535386977]=0.535386697
custseg$CASH_ADVANCE_TRX[custseg$CASH_ADVANCE_TRX>16.8981203]=16.8981203
custseg$PURCHASES_TRX[custseg$PURCHASES_TRX>64.4251306]=64.4251306
custseg$CREDIT_LIMIT[custseg$CREDIT_LIMIT>11772.09]=11772.09
custseg$PAYMENTS[custseg$PAYMENTS>7523.26]=7523.26
custseg$MINIMUM_PAYMENTS[custseg$MINIMUM_PAYMENTS>5609.1065423]=5609.1065423
custseg$PRC_FULL_PAYMENT[custseg$PRC_FULL_PAYMENT>0.738713]=0.738713
custseg$TENURE[custseg$TENURE>14.19398]=14.19398
custseg$m_avg_purchase [custseg$m_avg_purchase>447.192746] = 447.192746
custseg$m_avg_cash_adamount[custseg$m_avg_cash_adamount>475.2502132] = 475.2502132
custseg$avg_amt_per_purchase [custseg$avg_amt_per_purchase>394.9205613] = 394.9205613
custseg$avg_cash_advance_amt [custseg$avg_cash_advance_amt>1280.216151] = 1280.216151
custseg$limit_usage[custseg$limit_usage >1.16837] = 1.16837
custseg$pay_ratio[custseg$pay_ratio>249.923] = 249.923


View(custseg)

# Missing value imputation
#using respective MEAN value for imputation

custseg$CREDIT_LIMIT[which(is.na(custseg$CREDIT_LIMIT))] = 4494.44
custseg$MINIMUM_PAYMENTS[which(is.na(custseg$MINIMUM_PAYMENTS))] = 864.20654
custseg$limit_usage[which(is.na(custseg$limit_usage))] = 0.388926
custseg$pay_ratio[which(is.na(custseg$pay_ratio))] = 9.350070

View(custseg)

## FACTOR ANALYSIS

# CORRELATION MATRIX

corrm= cor(custseg) 

library("psych")
library("GPArotation")


#Exporting Correlation matrix

library("writexl")

View(corrm)
write.csv(corrm,"corrm.csv")


#DECIDING NUMBER OF FACTORS USING SCREE PLOT AND EIGEN VALUES

#SCREE PLOT

scree(corrm, factors=T, pc=T, main="scree plot", hline=NULL, add=FALSE) 

#CALCULATING EIGEN VALUES & VARIANCE

library("dplyr")

eigen_values = mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       ,pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       ,cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))  

write.csv(eigen_values, "eigenvalue.csv", row.names = F)

#CONDUCTING 5 FACTOR ANALYSIS BASED ON RESULTS OF EIGEN VALUES

FA=fa(r=corrm,5, rotate="varimax", fm="ml")  #Orthogonal rotations & Max. Likelihood factor analysis             
print(FA)  


FA_SORT=fa.sort(FA)    

ls(FA_SORT)     
FA_SORT$loadings

loadings = data.frame(FA_SORT$loadings[1:ncol(custseg),])

write.csv(loadings, "loadings.csv", row.names = T)


#### Cluster analysis ####

#Preparing final Data

Selected_vars <- c("ONEOFF_PURCHASES",
                   "avg_amt_per_purchase",
                   "CASH_ADVANCE",
                   "m_avg_cash_adamount",
                   "CASH_ADVANCE_TRX",
                   "avg_cash_advance_amt",
                   "limit_usage",
                   "MINIMUM_PAYMENTS",
                   "BALANCE_FREQUENCY",
                   "PRC_FULL_PAYMENT",
                   "pay_ratio"
)

#standardizing the data

custseg1 <- scale(custseg[Selected_vars])
View (custseg1)


#Elbow Method for finding the optimal number of clusters

set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
wss <- sapply(1:k.max, 
              function(k){kmeans(custseg1, k, nstart=50,iter.max = 15 )$tot.withinss})
wss

plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

  

#Building clusters using k-means clustering 

cluster_three <- kmeans(custseg1,3)
cluster_four <- kmeans(custseg1,4)
cluster_five <- kmeans(custseg1,5)
cluster_six <- kmeans(custseg1,6)

cluster_new<-cbind(custseg,
                   km_clust_3=cluster_three$cluster,
                   km_clust_4=cluster_four$cluster,
                   km_clust_5=cluster_five$cluster,
                   km_clust_6=cluster_six$cluster
                   )
View(cluster_new)
#Selecting cluster 4 for in-depth analysis


#Graph based on k-means - Optional

library("cluster")

clusplot(custseg1, #dataframe
         cluster_five$cluster, #clusterdata
         color = TRUE, #color
         shade = TRUE, # Lines in clusters
         lines =5, # lines connecting centroids
         labels = 2 # Labels clusters and cases
)

##### Profiling

### Converting into factors

cluster_new$km_clust_3=factor(cluster_new$km_clust_3)
cluster_new$km_clust_4=factor(cluster_new$km_clust_4)
cluster_new$km_clust_5=factor(cluster_new$km_clust_5)
cluster_new$km_clust_6=factor(cluster_new$km_clust_6)

#### preparing profiling sheet

library("tables")

profile<-tabular(1+ONEOFF_PURCHASES+
                 avg_amt_per_purchase+
                 CASH_ADVANCE+
                 m_avg_cash_adamount+
                 CASH_ADVANCE_TRX+
                 avg_cash_advance_amt+
                 limit_usage+
                 MINIMUM_PAYMENTS+
                 BALANCE_FREQUENCY+
                 PRC_FULL_PAYMENT+
                 pay_ratio
                 ~mean+(mean*km_clust_3)+
                   (mean*km_clust_4)+
                   (mean*km_clust_5)+
                   (mean*km_clust_6),
                 data=cluster_new)

profile1<-as.matrix(profile)
profile1<-data.frame(profile1)
View(profile1)


profile<-tabular(1~length+(length*km_clust_3)+(length*km_clust_4)+(length*km_clust_5)
                 +(length*km_clust_6),data=cluster_new)


profile2<-as.matrix(profile)
profile2<-data.frame(profile2)
View(profile2)

write.csv(profile1,"profile1.csv",row.names = F)
write.csv(profile2,"profile2.csv",row.names = F)


###### END ######

