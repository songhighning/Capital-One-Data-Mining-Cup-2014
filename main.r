rm(list = setdiff(ls(), lsf.str()))
gc()
##read files
setwd("N:\\DMC2014\\DMC 2014")
library(plyr)
source("helper.r")
customer<-read.csv("customer_zip.csv")
build_org <- read.csv("build_auth_m216.csv")
build_org$merchant_zip5 <-(as.numeric(as.character(build_org$merchant_zip5)))
merchant <- read.csv("merchant_metrics.csv")
zipcode <- read.csv("zipcode.csv")
colnames(zipcode)[1] <- "customer_zip5"
zipcode <- zipcode[c("customer_zip5","longitude", "latitude","city")]

validation <- read.csv("validation_auth.csv")

customer$latest_trxn_date<- as.Date(as.character(customer$latest_trxn_date),format= "%d%b%Y")

#######################
build_org$trxn_date<- as.Date(as.character(build_org$trxn_date),format= "%d%b%Y")
validation$trxn_date<- as.Date(as.character(validation$trxn_date),format= "%d%b%Y")

#add indicator variable to build
#build_org <- f.add_indicator(validation,merchant,"2011-06-30")
build_org <- f.add_indicator(build_org,merchant,"2011-06-30")

#zip code building##################
customer_zip <-merge(x = customer, y = zipcode, by = "customer_zip5", all.x= TRUE)
customer_zip <- customer_zip[order(customer_zip$acct_id_code,customer_zip$latest_trxn_date),]
customer_zip<-customer_zip[which(!is.na(customer_zip$city)),]

customer_zip_same_loc <- f.has_not_moved(customer_zip)
#customer_zip_diff_loc <- customer_zip[customer_zip$acct_id_code in]
build_org <- merge(x = build_org , y = customer_zip_same_loc ,
                   by = "acct_id_code", all.x= TRUE)

#customer_same_location
#build_x <- f.moved(build_org,customer_zip)

colnames(zipcode)[1] <- "merchant_zip5"
colnames(zipcode)[2] <- "mlongitude"
colnames(zipcode)[3] <- "mlatitude"
colnames(zipcode)[4] <- "mcity"

build_org <- merge(x = build_org, y = zipcode, by = "merchant_zip5", all.x =TRUE)

ll <- build_org[c("longitude", "latitude", "mlongitude", "mlatitude")]
ll[is.na(ll)] <- 0
build_org[c("longitude", "latitude", "mlongitude", "mlatitude")] <-ll

rm(customer,customer_zip,customer_zip_same_loc)
gc()

build_org <- f.distance_calc(build_org)


#build_org <- f.jbc(build_org, customer_zip)

build <- f.build_set(build_org,"2011-06-30")



#sample <- build_org[1:500,]
#result <- f.jbc(sample,customer_zip)

#sort customer table by customerid, date
#attach(build)
#build <- build [order(acct_id_code,trxn_date),]
#sample <- build[1:500,]


#add merc

#creating new table
#acct_id_code  with the following attributes

#customer_zip5	#latest_trxn_date  	#days since trxn m216 (2)
#days since consumer electronics	216 #event rate (3x2)
#total spending	(3x2) #avg spending	(per transaction 3)
#frequency(3x2) #average per zip code
#total per zip code#longitude #latitude
m216_subset <- build[which(build$m216_ind == 1),]
super_industry_subset <- build[which((build$industry_ind == 1)&(build$subindustry_ind == 0)),]
sub_industry_subset <- build[which((build$subindustry_ind == 1)&
                                     (build$m216_ind == 0)),]

#subsetting to inetrnet and non-internet
# nonint = non internet
# int = internet
m216_subset_nonint <- m216_subset[which(m216_subset$internet_trxn == 0),]
super_industry_subset_nonint<- super_industry_subset[which(super_industry_subset$internet_trxn == 0),]
sub_industry_subset_nonint<- sub_industry_subset[which(sub_industry_subset$internet_trxn == 0),]

m216_subset_int <- m216_subset[which(m216_subset$internet_trxn == 1),]
super_industry_subset_int<- super_industry_subset[which(super_industry_subset$internet_trxn == 1),]
sub_industry_subset_int<- sub_industry_subset[which(sub_industry_subset$internet_trxn == 1),]

build_nonint =build[which((build$internet_trxn==0)&
                      (build$industry_ind == 0)),]
build_int = build[which((build$internet_trxn==1)&
                          (build$industry_ind == 0)),]

#int = internet transaction
#non-int = physical transaction
spending_total <- ddply(build_nonint,~acct_id_code,summarise,spending_per_trxn=mean(trxn_amount), spending_total=sum(trxn_amount), spending_total_count = length(trxn_amount)
                        ,total_min_distance = min(distance))
spending_total_int <- ddply(build_int,~acct_id_code,summarise,spending_per_trxn_int=mean(trxn_amount), spending_total_int=sum(trxn_amount), spending_total_count_int = length(trxn_amount))

spending_m216 <- ddply(m216_subset_nonint,~acct_id_code,summarise,spending_per_trxn_m216=mean(trxn_amount), spending_total_m216=sum(trxn_amount),last_trxn_date_m216=max(trxn_date), spending_216_count = length(trxn_amount)
                       ,m216_min_distance = min(distance))
spending_super <- ddply(super_industry_subset_nonint,~acct_id_code,summarise,spending_per_trxn_super=mean(trxn_amount), spending_total_super=sum(trxn_amount),last_trxn_date_super=max(trxn_date), spending_super_count = length(trxn_amount)
                        ,super_min_distance = min(distance))
spending_sub <- ddply(sub_industry_subset_nonint,~acct_id_code,summarise,spending_per_trxn_sub=mean(trxn_amount), spending_total_sub=sum(trxn_amount),last_trxn_date_sub=max(trxn_date), spending_sub_count = length(trxn_amount)
                      ,sub_min_distance = min(distance))

spending_m216_int <- ddply(m216_subset_int,~acct_id_code,summarise,spending_per_trxn_m216_int=mean(trxn_amount), spending_total_m216_int=sum(trxn_amount),last_trxn_date_m216_int=max(trxn_date),  spending_216_int_count = length(trxn_amount))
spending_super_int <- ddply(super_industry_subset_int,~acct_id_code,summarise,spending_per_trxn_super_int=mean(trxn_amount), spending_total_super_int=sum(trxn_amount),last_trxn_date_super_int=max(trxn_date), spending_super_int_count = length(trxn_amount))
spending_sub_int <- ddply(sub_industry_subset_int,~acct_id_code,summarise,spending_per_trxn_sub_int=mean(trxn_amount), spending_total_sub_int=sum(trxn_amount),last_trxn_date_sub_int=max(trxn_date), spending_sub_int_count = length(trxn_amount))

uci<-unique_customer_id <- unique(build["acct_id_code"])


#merging the customer level table
final_table<-merge(x = uci, y = spending_total, by = "acct_id_code", all.x= TRUE)
final_table<-merge(x = final_table, y = spending_total_int, by = "acct_id_code", all.x= TRUE)
final_table<-merge(x = final_table, y = spending_m216 , by = "acct_id_code", all.x= TRUE)
final_table<-merge(x = final_table, y = spending_super, by = "acct_id_code", all.x= TRUE)
final_table<-merge(x = final_table, y = spending_sub, by = "acct_id_code", all.x= TRUE)
final_table<-merge(x = final_table, y = spending_m216_int, by = "acct_id_code", all.x= TRUE)
final_table<-merge(x = final_table, y = spending_super_int , by = "acct_id_code", all.x= TRUE)
final_table<-merge(x = final_table, y = spending_sub_int, by = "acct_id_code", all.x= TRUE)


rm(m216_subset)
rm(m216_subset_int)
rm(m216_subset_nonint)
rm(spending_m216)
rm(spending_m216_int)
rm(spending_sub)
rm(spending_sub_int)
rm(spending_super)
rm(spending_super_int)
rm(spending_total)
rm(spending_total_int)


rm(sub_industry_subset)
rm(sub_industry_subset_int)
rm(sub_industry_subset_nonint)
rm(super_industry_subset)
rm(super_industry_subset_int)
rm(super_industry_subset_nonint)
rm(uci)
rm(build_int)
rm(build_nonint)
gc()

spending_response_variate <- f.response_variate_helper2(
  "2011-06-30",build_org,"m216_ind",1,"m216_response")
final_table<-merge(x = final_table, y = spending_response_variate, by = "acct_id_code", all.x= TRUE)
final_table <- f.replace_null(final_table)

final_table[which(final_table$distance == 0),] 
valid_table <- final_table
save(valid_table,file = "valid_table_dis.rdata")
###############
load("final_data_distance.rdata")
build_final_table_lm <-final_table
save(build_final_table_lm, file="build_data_lm.rdata")

m.formula<-f.glm_no_dates(build_final_table)
m.glm <-glm(formula = m.formula,
          data = build_final_table, family = binomial(logit))
mx2.glm <- step(m.glm)
##############3
result.glm <- predict(mx2.glm, valid_table, type = "response")
write.csv(result.glm,"result_glm_distance.csv")



#model building (not include trxn)
m.glm <-glm(m216_response ~ spending_per_trxn + spending_total
            + spending_per_trxn_int + spending_total_int
            +spending_per_trxn_m216 + spending_total_m216
            + spending_per_trxn_super +spending_total_super
            + spending_per_trxn_sub + spending_total_sub
            + spending_per_trxn_m216_int + spending_total_m216_int
            + spending_per_trxn_super_int + 
              spending_total_super_int 
            + spending_per_trxn_sub_int + spending_total_sub_int, family = binomial(logit),
            data = final_table)

new.m<-step(m.glm, m216_response ~ spending_per_trxn + spending_total
            + spending_per_trxn_int + spending_total_int
            +spending_per_trxn_m216 + spending_total_m216
            + spending_per_trxn_super +spending_total_super
            + spending_per_trxn_sub + spending_total_sub
            + spending_per_trxn_m216_int + spending_total_m216_int
            + spending_per_trxn_super_int + 
              spending_total_super_int 
            + spending_per_trxn_sub_int + spending_total_sub_int, scale = 0,
            direction = "both",
            steps = 1000, k = 2)
#model building (including every column excepp date columns)
m2.glm<-glm(m216_response ~ spending_per_trxn + spending_total
            + spending_per_trxn_int + spending_total_int
            +spending_per_trxn_m216 + spending_total_m216
            + spending_per_trxn_super +spending_total_super
            + spending_per_trxn_sub + spending_total_sub
            + spending_per_trxn_m216_int + spending_total_m216_int
            + spending_per_trxn_super_int + 
              spending_total_super_int 
            + spending_per_trxn_sub_int + spending_total_sub_int, family = binomial(logit),
            data = final_table, weights=weight)

validation <- final_table
validation$m216_response <- NULL
result.glm <- predict(m.glm, validation, type = "response")

#check how many location m216 has
loca_m216 <- build[which(build$merchant_code == "M216"),]
loca_m216_unq<-unique(loca_m216[c("merchant_code" ,"merchant_zip5")])


##########################################################################
new.m<-step(m.glm, m216_response ~ spending_per_trxn + spending_total
            + spending_per_trxn_int + spending_total_int
            +spending_per_trxn_m216 + spending_total_m216
            + spending_per_trxn_super +spending_total_super
            + spending_per_trxn_sub + spending_total_sub
            + spending_per_trxn_m216_int + spending_total_m216_int
            + spending_per_trxn_super_int + 
              spending_total_super_int 
            + spending_per_trxn_sub_int + spending_total_sub_int, scale = 0,
            direction = "both",
            steps = 1000, k = 2)


#neural network
load("ultimate.rdata")
load("valid_table_ult.rdata")
colnames(ultimate_table)[37] = "response_spending_m216"
m.formula<-f.glm_no_dates(ultimate_table)
m.glm <-glm(formula = m.formula,
            data = ultimate_table, family = binomial(logit))
mx2.glm <- step(m.glm)###

m2.formula <- f.lm_no_dates(ultimate_table)
m2.lm <- lm(formula = m2.formula,
             data = ultimate_table) 
mx22.glm <- step(m2.lm)###
##############3
resultp.glm <- predict(mx2.glm, valid_table, type = "response")
write.csv(resultp.glm,"ult_pro2.csv")

resultamount.lm <- predict (mx22.glm, valid_table, type = "response")
write.csv(resultamount.lm,"ult_amount.csv")

