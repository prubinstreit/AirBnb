library(stargazer)
library(readxl)
library(haven)
library(dplyr)
library(ggplot2)
setwd("~/Documents/NU Econ PhD/AirBnb Papers")
# stargazer(attitude)
# linear.1 <- lm(rating ~ complaints + privileges + learning + raises + critical,
#                data=attitude)
# stargazer(linear.1, title="Results", align=TRUE)
# 
# 
# stargazer(linear.1, title="Regression Results",
#           dep.var.labels=c("Overall Rating","High Rating"),
#           order=c("learning", "privileges"),
#           keep.stat="n", ci=TRUE, ci.level=0.90, single.row=TRUE)
# 
# 
# 
# stargazer(linear.1, title="Regression Results",
#           dep.var.labels=c("Overall Rating","High Rating"),
#           order=c("complaints", "privileges", "critical"),
#           keep.stat="n", ci=TRUE, ci.level=0.90, single.row=TRUE, out='filename.tex')
my_data <- read_excel("Aggregated at Listing Level.xlsx")
df <- as.data.frame(my_data)
stargazer(df, out = 'stargazer1.tex')
names(df)[10] <-"unique_count"
df$post = ifelse(df$Month >=9 | df$Month <= 2, 0,1)
df$did = df$Group * df$post

diff <- lm(unique_count~ Group + post + did,
               data=df)

stargazer(diff, title="Difference in Difference",order= c("did","post", "Group"),out = 'diff_new.tex')

df_listings <-as.data.frame(read.csv("listings_august_2020.csv"))

df_final <- merge(df, df_listings, by.x=c("listing_id"), by.y=c("id"), all.x)

merged <- as.data.frame(read_dta("merged_with_dummies.dta"))
# merged$post = ifelse(merged$Month >=9 | merged$Month <= 2, 0,1)
# merged$did = merged$_diff
#include selected variables
stargazer(subset(merged[c("Month","Year")]) , out = 'merged_summaries_other.tex')

level_order <-c("Sept","Oct","Nov", "Dec", "Jan", "Feb", "Mar", "April", "May", "June","July","Aug")
merged$fact = ifelse(merged$Group==1, "2019-2020","2018-2019")
plot<- ggplot(data = merged, aes(x = factor(Month_Long, level=level_order), y=uniquecount, color=fact)) + geom_point(stat="summary", fun="mean") + labs(title="AirBnb Mean Number of Reviews",x="Month", y="Reviews", color="Treatment Group")
plot
diff <- lm(review_scores_rating~ Group + post + did, data=merged)
aspect_ratio <- 2.5
ggsave("point plot.jpg",plot = last_plot(), height = 7, width=7*aspect_ratio)
stargazer(diff, title="Difference in Difference",order= c("did","post", "Group"),out = 'diff_scores.tex')

# Creates Diff n Diff Stargazer table in latex for presentation 
df <- as.data.frame(read_dta("Listings_July22.dta"))
df$did = df$Group * df$post
diff <- lm(host_listings_count~ Group + post + did, data=df)
myvars <-names(df) %in% c('post','did', 'Group', 'superhost', 'Monthcodes', 'latitude','longitude', 'id','scrape_id', 'host_id')
newdf <-df[!myvars]
stargazer(diff, title="Difference in Difference",order= c("did","post", "Group"), covariate.labels = c('Beta','Post', 'Treatment'),out = 'diff_host_listings1.tex')
stargazer(subset(df[c("host_listings_count","square_feet")]), title="Summary Stats for Listings Data",out = 'summary_stats_listings1.tex')

# df[sapply(df,is.numeric)] & 
stargazer(newdf[sapply(newdf,is.numeric)] , title="Summary Stats for Listings Data", out='summary_stats_listings_good.tex')
# sapply(df, class)
