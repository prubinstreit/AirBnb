library(stargazer)
library(readxl)
library(haven)
library(dplyr)
# library(ggplot2)
# library(multiwayvcov)
# library(lmtest)
# library(miceadds)
library(sandwich)
# library(plm)
# library(pglm)
# library(texreg)
# require(purrr)

place <- "Montreal"
df <- as.data.frame(read_dta(paste0("Documents/NU Econ PhD/Inside Airbnb Raw Data/Montreal/MontrealVictoriaoct22.dta")))
df <-as.data.frame(df)


# level_order <-c("sept","oct","nov", "dec", "jan", "feb", "mar", "april", "may", "june","july","aug")
# df$fact = ifelse(df$Group==1, "2019-2020","2018-2019")

#Making histogram


max_list_num <- 1000
diff1<- df %>% filter(total >=max_list_num) %>%  lm(total~ Group + post + did,
                                                    data=.)
diff2 <- df  %>%  lm(total~ Group + post + did  + bathrooms + bedrooms,
                     data=.)
# diff3 <- plm(total~ Group + post + did ,
                     # data=df)

# This is how to show regular and clustered standard errors
cov2         <- sqrt(diag(vcovHC(diff2, type = "HC1")))

stargazer(diff2, diff2, se=list(NULL, cov2),
          column.labels=c("default","robust"), align=TRUE, type='text')




# vcov_firm <- cluster.vcov(diff1, df$Monthcodes)
# coeftest(diff2, vcov_firm)

# mod1 <- miceadds::lm.cluster( data=df, formula=total~ Group + post + did , cluster = "Monthcodes" )
# coef(mod1)
# vcov(mod1)
# summary(mod1)

# df %>%
  # group_by(df$Group) %>%
   # summarise_if(is.numeric,"mean", na.rm=TRUE)

#THIS REALLY WORKS
df[,c("Group","bedrooms","beds","minimum_nights","maximum_nights","availability_30","availability_60",
        "number_of_reviews","review_scores_rating","host_listings_count", "reviews_per_month",
          "superhost")] %>% split(. $Group) %>% walk(~ stargazer(.,flip=FALSE,out="montrealvictoriadescriptives.tex",style="default",type = "latex"))

victoria <- subset(df, subset = (df$Group==0))
montreal <- subset(df, subset = (df$Group==1))


victoria[, (names(victoria) %in% c("Group","bedrooms","beds","minimum_nights","maximum_nights","availability_30","availability_60",
                        "number_of_reviews","review_scores_rating","host_listings_count", "reviews_per_month",
                        "superhost"))] %>%  stargazer(.,type='latex', out='victoria.tex', title = 'Victoria Descriptive Stats')

montreal[, (names(montreal) %in% c("Group","bedrooms","beds","minimum_nights","maximum_nights","availability_30","availability_60",
                                   "number_of_reviews","review_scores_rating","host_listings_count", "reviews_per_month",
                                   "superhost"))] %>%  stargazer(.,type='latex', out='montreal.tex', title = 'Montréal Descriptive Stats')
# df[ , (names(df) %in% c("Group","host_location"))]
# dplyr::select_if(df, is.numeric)

# by(df, df$Group, stargazer, type = 'text')



# info<-df %>%
  # group_by(df$Group) %>%
  # summarise_at(c('host_listings_count', "bedrooms","bathrooms"),"mean", na.rm=TRUE)


####something weird happening at 8 and 9 my_listing_count

stargazer(summary(mod1) , title=paste0(" Difference in Difference",max_list_num),order= c("did","post", "Group"),covariate.labels=c("Diff in Diff","Post","Montreal"),type='text')
stargazer(summary(mod1) , title=paste0(" Difference in Difference",max_list_num),order= c("did","post", "Group"),covariate.labels=c("Diff in Diff","Post","Montreal"),type='text')
# stargazer(df ~ Group, type = 'text')
# stargazer(diff2, diff2  ,title=paste0(" Clustered Monthcode SE and Difference in Difference",max_list_num),order= c("did","post", "Group"),covariate.labels=c("Diff in Diff","Post","Montreal"),type='text', se = list(NULL, cov1))

# stargazer(diff3 , title=paste0(" Difference in Difference",max_list_num),order= c("did","post", "Group"),covariate.labels=c("Diff in Diff","Post","Montreal"),type='text')
texreg(diff1,file = "texreg1.tex")

# data("Produc", package = "plm")
# zz <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
#           data = Produc, index = c("state","year"))
# summary(zz)
# stargazer(coef(zz), type='text')

# ROBUST V CLUSTER (month)
model1 <- lm(total~ Group + post + did  + bathrooms + bedrooms,
             data=df)
stargazer(model1,coeftest(model1,vcovHC),coeftest(model1,vcovCL,cluster=df$Month),covariate.labels = c("Montreal","Post","Diff","Bathrooms","Bedrooms"),type='text')
