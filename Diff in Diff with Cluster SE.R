library(stargazer)
library(readxl)
library(haven)
library(tidyverse)
# library(ggplot2)
# library(multiwayvcov)
library(lmtest)
# library(miceadds)
library(sandwich)
library(plm)
# library(pglm)
# library(texreg)
# require(purrr)
# library(tableone)
# library(vtable)
library(xtable)
library(psych)
library(rstatix)
library(kableExtra)
install.packages("magick")
install.packages("webshot")
webshot::install_phantomjs()
setwd("~/Documents/NU Econ PhD/AirBnb Presentations/November 2021/")


place <- "Montreal"
df <- as.data.frame(read_dta
                    (paste0("~/Documents/NU Econ PhD/Inside Airbnb Raw Data/Montreal/MontrealVictoriaoct22.dta")))
df <-as.data.frame(df)

df <- df %>%
  group_by(Month, Group) %>%
  mutate(unique_host_count=n_distinct(host_id)) 

df <- df %>%
  mutate(single_host_count = ifelse(calculated_host_listings_count==1, 1, 0))
df <- df %>%
  group_by(Month, Group) %>%
  mutate(share_single_host_count = mean(single_host_count))

df$calculated_host_listings_count
df <- df %>%
  group_by(Month, Group) %>%
  mutate(unique_listing_count=n_distinct(id)) 


df <- df %>%
  group_by(Month, Group) %>%
  mutate(mean_host = ifelse(calculated_host_listings_count<mean(calculated_host_listings_count), 1, 0)) 
df <- df %>%
  group_by(Month, Group) %>%
  mutate(mean_host1 = sum(mean_host)/n_distinct(id))

#pricing
df$price1 <- as.numeric(substring(df$price, 2,12))
df <- df %>%
  group_by(Month, Group) %>%
  mutate(mean_price = mean(price1, na.rm=TRUE)) 

tail(df[,c("Group","Month","mean_host1","mean_host", "mean_price")])
# sum(df$mean_host)


df_haven <- df
df_haven$Month <- haven::as_factor(df_host_listing$Month)
df_haven$Month <-           factor(df_host_listing$Month, level = level_order)
df_haven$Month <- haven::as_factor(df_host_listing$Month)
df_haven$Group <- haven::as_factor(df_host_listing$Group)
# df_haven$price <- haven::as_factor(df_host_listing$Group)

# stargazer_htest = function (data, ...) {
#   summary = data.frame(`Test statistic` = data$statistic,
#                        DF = data$parameter,
#                        `p value` = data$p.value,
#                        `Alternative hypothesis` = data$alternative,
#                        check.names = FALSE)
#   stargazer(summary, flip = TRUE, summary = FALSE,
#             notes = paste(data$method, data$data.name, sep = ': '), type='text',...)
# }

#THIS REALLY WORKS for separate summary stats per group
myVars <- c("Group","bedrooms","beds","minimum_nights","maximum_nights","availability_30","availability_60",
            "number_of_reviews","review_scores_rating","host_listings_count", "reviews_per_month",
            "superhost", "file")
dfSum <- df[myVars]
dfMontreal <- subset(dfSum, Group==1)
dfVictoria <- subset(dfSum, Group==0)

# stat.test <- dfSum %>%
#   # group_by(Group) %>%
#   t_test(df[,myVars] ~ Group) %>%
#   adjust_pvalue(method = "BH") %>%
#   add_significance()
# stat.test
# t.test(bedrooms + beds ~ Group, data =dfSum)
# yep <-lm(beds ~ as.factor(Group), data=dfSum)
# stargazer(dfMontreal, dfVictoria, type='text')
# t.test(bedrooms ~ Group, paired = TRUE, data = dfSum)
# stargazer_htest(t.test(bedrooms + bathrooms ~ as.factor(Group), data = dfSum))
# stargazer(dfSum["bedrooms"],dfSum["beds"], type='text')
summ <- dfSum %>% group_by(file) %>% summarise_all(funs(mean),na.rm=TRUE)

tsumm <-t(summ)

# kable(summ,format="html") %>% save_kable("test.png") %>% kableExtra::landscape()

kable(summ, "latex", caption="Summary Means", booktabs = T) %>% 
  kable_styling(latex_options = c("striped","scale_down")) %>% 
  row_spec(0,angle=70) %>% 
   save_kable("SummaryStatAngled3.png")


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
# modelp <- plm(total~ Group + post + did  + bathrooms + bedrooms,model = "within",
              # data=df.p)
# t.test(victoria[, (names(victoria) %in% c("bedrooms"))],
       # montreal[, (names(montreal) %in% c("bedrooms"))])

# Total Listings
model1 <- lm(total~ Group + post + did, data=df)
model2 <- lm(total ~Group+post +did + bathrooms + bedrooms, data = df)
model3 <- lm(total ~Group+post +did + bathrooms + bedrooms +review_scores_rating, data = df)
model4 <- lm(total ~Group+post +did + bathrooms + bedrooms +review_scores_rating +factor(Month), data = df)


df_total <- df %>%
  group_by(Month, Group) %>%
  count(total)

# level_order <-c("mar", "april", "may", "june","july","aug","sept","oct","nov", "dec", "jan", "feb")
level_order <-c("03", "04", "05", "06","07","08","09","10","11", "12", "01", "02")

df_total$Month <- haven::as_factor(df_total$Month)
df_total$Month <- factor(df_total$Month, level = level_order)
df_total$Month <- haven::as_factor(df_total$Month)
df_total$Group <- haven::as_factor(df_total$Group)

p1<- ggplot(df_total) +geom_line(aes(x=Month, y=total,color=Group, group=Group))+ggtitle("Total Listings Count")
ggsave(filename = paste0("Total Listings Count.png"),  width = 12, height = 8, dpi = 150, units = "in", device='png')

stargazer(coeftest(model1,vcovCL,cluster=df$Month),
          coeftest(model2,vcovCL,cluster=df$Month),
          coeftest(model3,vcovCL,cluster=df$Month),
          coeftest(model4,vcovCL,cluster=df$Month),
          # coeftest(model1,vcovCL,cluster=df$Month),
          # omit.table.layout = "m", 
          order = c(3, 1, 2, 4, 5, 6),
          title=paste0(" Difference in Difference "),
          dep.var.labels.include = TRUE,
          dep.var.labels = c("Total Listings Count"),
          covariate.labels = c("Diff*Post","Montreal","Post","Bathrooms","Bedrooms", "Review Score Rating"),
          add.lines=list(c('Month Dummy', 'No','No','No', 'Yes')),
          omit = c('Month'),
          # type='latex', style ='aer', out="total_listings.tex")
          type='text', style ='aer')

#Minimum Nights
df_min <- df %>%
  group_by(Month, Group) %>%
  mutate(min = mean(minimum_nights))

df_min$Month <- haven::as_factor(df_min$Month)
df_min$Month <-           factor(df_min$Month, level = level_order)
df_min$Month <- haven::as_factor(df_min$Month)
df_min$Group <- haven::as_factor(df_min$Group)
p2<-                      ggplot(df_min) +geom_line(aes(x=Month, y=min,color=Group, group=Group))+
  ggtitle("Minimum Nights")
ggsave(filename = paste0("Minimum Nights.png"),  width = 12, height = 8, dpi = 150, units = "in", device='png')

model1min <- lm(minimum_nights~ Group + post + did,   data=df)
model2min <- lm(minimum_nights ~Group+post +did + bathrooms + bedrooms, data = df)
model3min <- lm(minimum_nights ~Group+post +did + bathrooms + bedrooms +review_scores_rating, data = df)
model4min <- lm(minimum_nights ~Group+post +did + bathrooms + bedrooms +review_scores_rating+ factor(Month), data = df)


stargazer(coeftest(model1min,vcovCL,cluster=df$Month),
          coeftest(model2min,vcovCL,cluster=df$Month),
          coeftest(model3min,vcovCL,cluster=df$Month),
          coeftest(model4min,vcovCL,cluster=df$Month),
          # coeftest(model1,vcovCL,cluster=df$Month),
          # omit.table.layout = "m", 
          order = c(3, 1, 2, 4, 5, 6),
          title=paste0(" Difference in Difference "),
          dep.var.labels.include = TRUE,
          dep.var.labels = c("Minimum Nights"),
          covariate.labels = c("Diff*Post","Montreal","Post","Bathrooms","Bedrooms", "Review Score Rating"),
          add.lines=list(c('Month Dummy', 'No','No','No', 'Yes')),
          omit = c('Month'),
          type='latex', style ='aer', out="tminimum_nights_monthdummies.tex")
          # type='text', style ='aer')

#calculated host listings count
model1listing <- lm(My_listing_count ~Group+post +did, data = df)
model2listing <- lm( My_listing_count~Group+post +did + bathrooms + bedrooms, data = df)
model3listing <- lm(My_listing_count ~Group+post +did + bathrooms + bedrooms +review_scores_rating, data = df)
model4listing <- lm(My_listing_count ~Group+post +did + bathrooms + bedrooms +review_scores_rating + factor(Month), data = df)

df_host_listing <- df %>%
  group_by(Month, Group) %>%
  mutate(host_listing = mean(calculated_host_listings_count))

df_host_listing$Month <- haven::as_factor(df_host_listing$Month)
df_host_listing$Month <-           factor(df_host_listing$Month, level = level_order)
df_host_listing$Month <- haven::as_factor(df_host_listing$Month)
df_host_listing$Group <- haven::as_factor(df_host_listing$Group)
p3<-                               ggplot(df_host_listing) +geom_line(aes(x=Month, y=host_listing,color=Group, group=Group))+
   ggtitle("Average Listings Per Host")
ggsave(filename = paste0("Average Host Listings.png"),  width = 12, height = 8, dpi = 150, units = "in", device='png')


stargazer(coeftest(model1listing,vcovCL,cluster=df$Month),
          coeftest(model2listing,vcovCL,cluster=df$Month),
          coeftest(model3listing,vcovCL,cluster=df$Month),
          coeftest(model4listing,vcovCL,cluster=df$Month),
          # coeftest(model1,vcovCL,cluster=df$Month),
          # omit.table.layout = "m", 
          order = c(3, 1, 2, 4, 5, 6),
          title=paste0(" Difference in Difference Analysis"),
          dep.var.labels = c("Host Listing Count"),
          covariate.labels = c("Diff*Post","Montreal","Post","Bathrooms","Bedrooms", "Review Score Rating"),
          add.lines=list(c('Month Dummy', 'No','No','No', 'Yes')),
          omit = c('Month'),
          type='text', style ='aer')

# Total Hosts count unique
model1hosts <- lm(unique_host_count~ Group + post + did, data=df)
model2hosts <- lm(unique_host_count ~Group+post +did + bathrooms + bedrooms, data = df)
model3hosts <- lm(unique_host_count ~Group+post +did + bathrooms + bedrooms +review_scores_rating, data = df)
model4hosts <- lm(unique_host_count ~Group+post +did + bathrooms + bedrooms +review_scores_rating + factor(Month), data = df)

 # df_unique_host_listing <- df %>%
  # group_by(Month, Group) %>%
  # mutate(unique_host_listing = mean(calculated_host_listings_count))
df_unique_host_listing <- df
df_unique_host_listing$Month <- haven::as_factor(df$Month)
df_unique_host_listing$Month <-           factor(df$Month, level = level_order)
df_unique_host_listing$Month <- haven::as_factor(df$Month)
df_unique_host_listing$Group <- haven::as_factor(df$Group)
p4<-                                      ggplot(df_unique_host_listing) +geom_line(aes(x=Month, y=unique_host_count,color=Group, group=Group))+
  ggtitle("Unique Host Count")
ggsave(filename = paste0("Unique Host Count.png"),  width = 12, height = 8, dpi = 150, units = "in", device='png')




stargazer(coeftest(model1hosts,vcovCL,cluster=df$Month),
          coeftest(model2hosts,vcovCL,cluster=df$Month),
          coeftest(model3hosts,vcovCL,cluster=df$Month),
          coeftest(model4hosts,vcovCL,cluster=df$Month),
          # coeftest(model1,vcovCL,cluster=df$Month),
          # omit.table.layout = "m", 
          order = c(3, 1, 2, 4, 5, 6),
          title=paste0(" Difference in Difference "),
          dep.var.labels.include = TRUE,
          dep.var.labels = c("Total Unique Hosts Count"),
          covariate.labels = c("Diff*Post","Montreal","Post","Bathrooms","Bedrooms", "Review Score Rating"),
          add.lines=list(c('Month Dummy', 'No','No','No', 'Yes')),
          omit = c('Month'),
          # type='text', style ='aer')
          type='latex', style ='aer', out="total_hosts.tex")

# Count of 1-listing hosts
model1singlehost <- lm(single~ Group + post + did, data=df)
model2singlehost <- lm(share_single_host ~Group+post +did + bathrooms + bedrooms, data = df)
model3singlehost <- lm(share_single_host ~Group+post +did + bathrooms + bedrooms +review_scores_rating, data = df)
model4singlehost <- lm(share_single_host ~Group+post +did + bathrooms + bedrooms +review_scores_rating + factor(Month), data = df)

p5<- ggplot(df_haven) +geom_line(aes(x=Month, y=share_single_host_count,color=Group, group=Group))+
      ggtitle("Single Listing Host Count")
ggsave(filename = paste0("SingleListingHostCount.png"),  width = 12, height = 8, dpi = 150, units = "in", device='png')

stargazer(coeftest(model1singlehost,vcovCL,cluster=df$Month),
          coeftest(model2singlehost,vcovCL,cluster=df$Month),
          coeftest(model3singlehost,vcovCL,cluster=df$Month),
          coeftest(model4singlehost,vcovCL,cluster=df$Month),
          # coeftest(model1,vcovCL,cluster=df$Month),
          # omit.table.layout = "m", 
          order = c(3, 1, 2, 4, 5, 6),
          title=paste0(" Difference in Difference "),
          dep.var.labels.include = TRUE,
          dep.var.labels = c("Total Single Property Hosts Share"),
          covariate.labels = c("Diff*Post","Montreal","Post","Bathrooms","Bedrooms", "Review Score Rating"),
          add.lines=list(c('Month Dummy', 'No','No','No', 'Yes')),
          omit = c('Month'),
          type='text', style ='aer')
          # type='latex', style ='aer', out="single_hosts.tex")

# Count of Below Mean hosts
model1meanhost <- lm(mean_host1~ Group + post + did, data=df)
model2meanhost <- lm(mean_host1 ~Group+post +did + bathrooms + bedrooms, data = df)
model3meanhost <- lm(mean_host1 ~Group+post +did + bathrooms + bedrooms +review_scores_rating, data = df)
model4meanhost <- lm(mean_host1 ~Group+post +did + bathrooms + bedrooms +review_scores_rating + factor(Month), data = df)

p6<- ggplot(df_haven) +geom_line(aes(x=Month, y=mean_host1,color=Group, group=Group))+
       ggtitle("Share Below Mean Host")
ggsave(filename = paste0("sharebelowmeanhost.png"),  width = 12, height = 8, dpi = 150, units = "in", device='png')

stargazer(coeftest(model1meanhost,vcovCL,cluster=df$Month),
          coeftest(model2meanhost,vcovCL,cluster=df$Month),
          coeftest(model3meanhost,vcovCL,cluster=df$Month),
          coeftest(model4meanhost,vcovCL,cluster=df$Month),
          # coeftest(model1,vcovCL,cluster=df$Month),
          # omit.table.layout = "m", 
          order = c(3, 1, 2, 4, 5, 6),
          title=paste0(" Difference in Difference "),
          dep.var.labels.include = TRUE,
          dep.var.labels = c("Below Mean Host"),
          covariate.labels = c("Diff*Post","Montreal","Post","Bathrooms","Bedrooms", "Review Score Rating"),
          add.lines=list(c('Month Dummy', 'No','No','No', 'Yes')),
          omit = c('Month'),
          # type='text', style ='aer')
          type='latex', style ='aer', out="below_mean_host.tex")


# Pricing
model1meanprice <- lm(mean_price~ Group + post + did, data=df)
model2meanprice <- lm(mean_price ~Group+post +did + bathrooms + bedrooms, data = df)
model3meanprice <- lm(mean_price ~Group+post +did + bathrooms + bedrooms +review_scores_rating, data = df)
model4meanprice <- lm(mean_price ~Group+post +did + bathrooms + bedrooms +review_scores_rating + factor(Month), data = df)

p6<- ggplot(df_haven) +geom_line(aes(x=Month, y=mean_price,color=Group, group=Group))+
  ggtitle("Mean Price")

ggsave(filename = paste0("meanprice.png"),  width = 12, height = 8, dpi = 150, units = "in", device='png')

stargazer(coeftest(model1meanprice,vcovCL,cluster=df$Month),
          coeftest(model2meanprice,vcovCL,cluster=df$Month),
          coeftest(model3meanprice,vcovCL,cluster=df$Month),
          coeftest(model4meanprice,vcovCL,cluster=df$Month),
          # coeftest(model1,vcovCL,cluster=df$Month),
          # omit.table.layout = "m", 
          order = c(3, 1, 2, 4, 5, 6),
          title=paste0(" Difference in Difference "),
          dep.var.labels.include = TRUE,
          dep.var.labels = c("Mean Price"),
          covariate.labels = c("Diff*Post","Montreal","Post","Bathrooms","Bedrooms", "Review Score Rating"),
          add.lines=list(c('Month Dummy', 'No','No','No', 'Yes')),
          omit = c('Month'),
          # type='text', style ='aer')
          type='latex', style ='aer', out="mean_price.tex")

