#two way interaction formula
f.two_way_interaction <- function(in_table){
  in_table$response_spending_m216 <- NULL
  in_table$acct_id_code <- NULL
  
  g <- glm(m216_response ~ .+.*., data=in_table,family=binomial(logit))
  
  string_formula <- as.character(informula)
  form_split <- strsplit(string_formula," ")
  x_formula <- form_split[[3]]
  x_formula <- x_formula[(x_formula != "\n")]
  x_formula <- paste(x_formula, collapse = ' ')
  
  x_formula <- x_formula[!grepl(pattern = "date", x = x_formula)]
  x_formula <- x_formula[!grepl(pattern = "acc_id_code", x = x_formula)]
  x_formula <- x_formula[!grepl(pattern = "response_spending_m216", x = x_formula)]
  x_formula <- paste(x_formula, collapse = ' ')
  
  first <- paste(form_split[[2]],form_split[[1]])
  final <- paste(first, x_formula)
  final <- paste(final, " 0 ")
  g <- glm(m216_response ~ .+.*., data=new_table,family=binomial(logit)) 
  return(final)
}

#this also adds up the total spending of m216 in the next 3 month
f.response_variate_helper2 <- function (cutoff_date, org_table,col_name,col_condition,response_col_name){
  starting_date = as.Date(cutoff_date) -365
  ending_date = as.Date(cutoff_date) + 90
  build_set <- org_table[which(org_table$trxn_date <= cutoff_date),]
  response_set_org <- org_table[which((org_table$trxn_date > cutoff_date)
                                  &(org_table[col_name] == col_condition)),]
  build_set <- unique(build_set["acct_id_code"])
  response_set <- unique(response_set_org["acct_id_code"])
  response_set[response_col_name] <- 1
  
  response_spending_set<-response_set_org
  spending_set<-ddply(response_spending_set,~acct_id_code,summarise,
                      response_m216_spending=sum(trxn_amount))
  print(names(spending_set))
  spending_set <- spending_set[c("acct_id_code","response_m216_spending")]
  
  response_set <- merge(x = response_set, y = spending_set,
                        by = "acct_id_code", all.x = TRUE)
  
  build_set<-merge(x = build_set, y = response_set, by = "acct_id_code", all.x= TRUE)
  build_set[is.na(build_set)] <-0
  return (build_set)
  
}

#add joining zipcode to those ppl who have moved
f.moved <- function(build_t,zip_t){
  zip_t_agg <-ddply(zip_t,~acct_id_code,summarise,zip_count=length(city))
  diff_loc <- zip_t_agg[which(zip_t_agg$zip_count > 1),]
  diff_zip <- zip_t[which(zip_t$acct_id_code %in% diff_loc$acct_id_code),]
  
  position <- which((build_t$acct_id_code %in% diff_zip$acct_id_code))
  new_t <- build_t[position,]
  starting_date = "1900-01-01"
  ending_date = "2100-12-31"
  unique_acct <- unique(zip_t$acct_id_code)
  #build_t$latitude <- 0 
  #build_t$longitude <- 0
  final_long_list <- rep(0,dim(new_t)[1])
  final_lat_list <- rep(0,dim(new_t)[1])
  prev_id = 0
  
  curr_info <- new_t[1,]
  curr_id <- curr_info$acct_id_code
  curr_trxn_date <- curr_info$trxn_date
  zip_info <- zip_t[which(zip_t$acct_id_code == curr_id),]
  zip_dates <- zip_info$latest_trxn_date
  date_list <- c(starting_date, as.character(zip_dates),ending_date)
  
  for(i in 1:nrow(new_t)){
    curr_info <- new_t[i,]
    curr_id <- curr_info$acct_id_code
    curr_trxn_date <- curr_info$trxn_date
    if (prev_id != curr_id){
      zip_info <- zip_t[which(zip_t$acct_id_code == curr_id),]
      zip_dates <- zip_info$latest_trxn_date
      date_list <- c(starting_date, as.character(zip_dates),ending_date)
    }
    for (j in 1:(length(date_list)-1)){
      y = j+1
      if((curr_trxn_date > date_list[j])&
           (curr_trxn_date <= date_list[y])){
        found_zip_info = zip_info[y,]
        build_t[position[i],]$longitude <- found_zip_info$longitude
        build_t[position[i],]$latitude <- found_zip_info$latitude
      }
    }
    print(i)
    prev_id = curr_id
  }
  
  return (build_t)
}

## lets the customer who hasnt moved
f.has_not_moved <- function (zip_t){
  zip_t_agg<-ddply(zip_t,~acct_id_code,summarise,zip_count=length(city))
  same_loc <- zip_t_agg[which(zip_t_agg$zip_count == 1),]
  ret <- zip_t[which(zip_t$acct_id_code %in% same_loc$acct_id_code),]
  
  return (ret)
}

# distance using latitude and longitude
f.distance_calc <-function (build_t){
  build_t$distance <- 99999
  
  position <-which((!(((build_t$longitude==0)&(build_t$latitude==0))|
                        ((build_t$mlongitude==0)&(build_t$mlatitude==0))))
                   &(build_t$internet_trxn == 0))
  new_t <- build_t[position,]
  ##calculate distance with valid latitude and longitude
  new_t["distance"]<-sqrt((new_t$longitude - new_t$mlongitude)^2
                          +(new_t$latitude - new_t$mlatitude)^2)
  build_t[position,]$distance <- new_t$distance
  return(build_t)
  
}

#join build_org and customer_zip
f.jbc <- function (build_t,zip_t){
  
  starting_date = "1900-01-01"
  ending_date = "2100-12-31"
  unique_acct <- unique(zip_t$acct_id_code)
  #build_t$latitude <- 0 
  #build_t$longitude <- 0
  build_count <- 1
  final_long_list <- rep(0,dim(build_t)[1])
  final_lat_list <- rep(0,dim(build_t)[1])
  prev_id = 0
  
  curr_info <- build_t[1,]
  curr_id <- curr_info$acct_id_code
  curr_trxn_date <- curr_info$trxn_date
  zip_info <- zip_t[which(zip_t$acct_id_code == curr_id),]
  zip_dates <- zip_info$latest_trxn_date
  date_list <- c(starting_date, as.character(zip_dates),ending_date)
  
  for(i in 1:nrow(build_t)){
    curr_info <- build_t[i,]
    curr_id <- curr_info$acct_id_code
    curr_trxn_date <- curr_info$trxn_date
    if (prev_id != curr_id){
      zip_info <- zip_t[which(zip_t$acct_id_code == curr_id),]
      zip_dates <- zip_info$latest_trxn_date
      date_list <- c(starting_date, as.character(zip_dates),ending_date)
    }
    for (j in 1:(length(date_list)-1)){
      y = j+1
      if((curr_trxn_date > date_list[j])&
        (curr_trxn_date <= date_list[y])){
        found_zip_info = zip_info[y,]
        final_long_list[i] <- found_zip_info$longitude
        final_lat_list[i] <- found_zip_info$latitude
      }
    }
    print(i)
    prev_id = curr_id
    }
 build_t$latitude <- final_lat_list
 build_t$longitude <- final_long_list
 return (build_t)
}

#build_set filtering
f.build_set <- function (org_table,cutoff_date){
  starting_date = as.Date(cutoff_date) -365
  t1 <- org_table[which((org_table$trxn_date >= starting_date)&
                                   (org_table$trxn_date <= cutoff_date)),]
  return(t1)
}


#logistic model that excludes date
f.glm_no_dates <- function(in_table){
  new_table <- in_table[!grepl(pattern = "date", x = names(in_table))]
  new_table <- new_table[!grepl(pattern = "acct_id_code", x = names( new_table ))]
  new_table <- new_table[!grepl(pattern = "response_spending_m216", x = names( new_table ))]
  new_table <- new_table[!grepl(pattern = "distance", x = names( new_table ))]
  
  t_names <- names(new_table)
  t_names <- t_names[t_names != "m216_response"]
  xnam <- paste(t_names, sep = "")
  fmla <- as.formula(paste("m216_response ~ ", paste(xnam, collapse = "+")))
  return(fmla)
}

#logistic model that excludes date
f.lm_no_dates <- function(in_table){
  new_table <- in_table[!grepl(pattern = "date", x = names(in_table))]
  new_table <- new_table[!grepl(pattern = "acct_id_code", x = names( new_table ))]
  new_table <- new_table[!grepl(pattern = "m216_response", x = names( new_table ))]
  
  t_names <- names(new_table)
  t_names <- t_names[t_names != "response_spending_m216"]
  xnam <- paste(t_names, sep = "")
  fmla <- as.formula(paste("response_spending_m216 ~ ", paste(xnam, collapse = "+")))
  return(fmla)
}


#function replaces all the null values in columns that does not contain
#substring "date" with 0

f.replace_null <- function(in_table){
  t_names <- names(in_table)
  date_names <- t_names[grep("date",t_names)]
  
  for (i in 1:(length(t_names))){
    if(!(t_names[i] %in% date_names)){
      in_table[(t_names[i])][is.na(in_table[(t_names[i])])] <-0
    }
  }
  return(in_table)
}

f.response_variate_helper <- function (cutoff_date, org_table,col_name,col_condition,response_col_name){
  starting_date = as.Date(cutoff_date) -365
  ending_date = as.Date(cutoff_date) + 90
  build_set <- org_table[which((org_table$trxn_date <= cutoff_date)
                               &(org_table$trxn_date >= starting_date)),]
  response_set <- org_table[which((org_table$trxn_date > cutoff_date)
                                  &(org_table$trxn_date <= ending_date)
                                  &(org_table[col_name] == col_condition)),]
  build_set <- unique(build_set["acct_id_code"])
  response_set <- unique(response_set["acct_id_code"])
  response_set[response_col_name] <- 1
  build_set<-merge(x = build_set, y = response_set, by = "acct_id_code", all.x= TRUE)
  build_set[is.na(build_set)] <-0
  return (build_set)
  
}

# filters data
# response_variate if customer purchased after 3 month of date_txrn
f.response_variate <-function(date_txrn,org_table){
  t1 <- org_table
  attach(t1)
  t1["purchase_after1"] <- NA
  t1["purchase_before1"] <- NA
  
  for(i in 1:(nrow(t1))){
    if((trxn_date[i]>=date_txrn)&(m216_ind[i]==1)){
      t1["purchase_after1"][i,] <- t1["acct_id_code"][i,]
    } 
    else if ((trxn_date[i]<date_txrn)&(m216_ind[i]==1)) {t1["purchase_before1"][i,] <- t1["acct_id_code"][i,]}
  }
  purchase_after = unique(t1["purchase_after1"])
  purchase_after = purchase_after[(!is.na(purchase_after))]
  purchase_before = unique(t1["purchase_before1"])
  purchase_before = purchase_before[(!is.na(purchase_before))]
  t1<-transform (t1, response_variate = ifelse(acct_id_code %in% purchase_after,1,0))
  return(t1)
}


#function adds three new indicator attributes
# m216_ind: indicates wather its a m216 transaction
# industry_ind: indicates weather its a Consumer Electronics and Computers transaction
# 
f.add_indicator <- function(org_table,merchant,cutoff_date){
  #starting_date = as.Date(cutoff_date) -365
  #t1 <- org_table[which((org_table$trxn_date >= starting_date)&
  #                                 (org_table$trxn_date <= cutoff_date)),]
  t1 = org_table
  super_industry_ind_dictionary = merchant[merchant$Super_Industry_Name == "Consumer Electronics and Computers",]
  super_industry_ind_dictionary<-super_industry_ind_dictionary$merchant_code
  t1<-transform (t1, m216_ind = 
                    ifelse(merchant_code == "N/A",-2,
                    ifelse(merchant_code == "", -1 ,
                          ifelse(merchant_code =="M216",1,0))))
  t1<-transform (t1, industry_ind = 
                   ifelse(merchant_code == "N/A",-2,
                   ifelse(merchant_code == "", -1 ,
                   ifelse(merchant_code %in% super_industry_ind_dictionary,1,0))))
  
  sub_industry_ind_dictionary = merchant[merchant$Industry_Name == "Computer / Software Stores",]
  sub_industry_ind_dictionary<-sub_industry_ind_dictionary$merchant_code
  
  t1<-transform (t1, subindustry_ind = 
                   ifelse(merchant_code == "N/A",-2,
                          ifelse(merchant_code == "", -1 ,
                                 ifelse(merchant_code %in% sub_industry_ind_dictionary,1,0))))
  
  
  
  return (t1)
}

#function adds three new attributes
f.new_var<- function (org_table){
  t1 <- org_table
  attach(t1)
  t1["last_trxn_amount"] <- NA
  t1["last_trxn_date"] <- NA
  t1["Days difference"] <- -1
  prev_acctid <- acct_id_code[1]
  
  for (i in 2:(nrow(t1))){
    curr_acctid <- acct_id_code[i]
    if ((prev_acctid==curr_acctid)&(!is.na(curr_acctid))){
          t1["last_trxn_amount"][i,] <- t1["trxn_amount"][(i-1),]
          t1["last_trxn_date"][i,] <- t1["trxn_date"][(i-1),]
          t1["Days difference"] <- (t1["trxn_date"][i,] - t1["last_trxn_date"])
    }
    prev_acctid <- curr_acctid
    
  }
  return(t1)
}

f.response_variate2 <- function(cutoff_date, org_table){
  starting_date = as.Date(cutoff_date) -365
  ending_date = as.Date(cutoff_date) + 90
  working_set <- org_table[which((org_table$trxn_date >= starting_date)&
                                   (org_table$trxn_date <= ending_date)),]
  
  m216_set <- f.response_variate_helper(cutoff_date,working_set,"m216_ind",1,"m216_response")
  #industry_set <- f.response_variate_helper(cutoff_date,working_set,"industry_ind",1,"industry_response")
  #sub_industry_set <- f.response_variate_helper(cutoff_date,working_set,"subindustry_ind",1,"industry_response")
  
  working_set <-merge(x = working_set, y = m216_set, by = "acct_id_code", all.x = TRUE)
  #working_set <-merge(x = working_set, y = industry_set, by = "acct_id_code", all.x = TRUE)
  #working_set <-merge(x = working_set, y = sub_industry_set, by = "acct_id_code", all.x = TRUE)
  return(working_set)
}
