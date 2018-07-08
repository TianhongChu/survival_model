sub = read.csv("/projects/b1078/Newsday/Rawdata/2017SUB_KEY.csv")
sub$odate = as.Date(sub$orig_date, "%m/%d/%Y")			# start date
sub$sdate = as.Date(sub$stop_date, "%m/%d/%Y")			# stop date
sub$sdate = sub$sdate-1     # end date = stop date - 1
sub$tdate = as.Date("2018-04-09")						# end of observation
sub$tdate[!is.na(sub$sdate)]=sub$sdate[!is.na(sub$sdate)] 	# end because of stop
sub = sub[order(sub$odate),]
sub = sub[sub$odate>=as.Date("2017-01-04"),]				# select sample, 2017-01-04 <= start date <= 2017-02-28
sub = sub[sub$odate<=as.Date("2017-03-31"),]
sub = sub[order(sub$SUB_KEY),]

n=nrow(sub)
df = matrix(0,0,5)		# a records SUB_KEY of the subscriber, elapsed week for the subscriber, start date of the week, end date of the week, whether the subscriber unsubscribe in that week 
for (i in 1:n) {
  k = sub$odate[i]
  sk = k
  while (k+7<=sub$tdate[i]) 
  {
    k=k+7
    sk = rbind(sk,k)
  }
  b = cbind(matrix(sub$SUB_KEY[i],length(sk),1),seq(1:length(sk)),sk,sk+6,0)
  if(!is.na(sub$sdate[i])){b[nrow(b),5]=1}
  df=rbind(df,b)
}
colnames(df)=c("SUB_KEY","week","start_date","end_date","unsub")
rownames(df)=c()
df= as.data.frame(df)
df$start_date = as.Date(df$start_date,origin = "1970-01-01")
df$end_date = as.Date(df$end_date,origin = "1970-01-01")

head(df)
dim(df)


# Find digital engagement only for sub #
polo = read.csv("/projects/b1078/Newsday/Rawdata/polopolyid_key.csv")
subpolo=merge(sub, polo, by="SUB_KEY",all.x=TRUE)
subpolo = subpolo[,c(1,12)]
subpolo$engage = 1
subpolo$engage[is.na(subpolo$polopoly_id)] = 0 
subpolo = subpolo[order(subpolo$SUB_KEY),]
subeng = subpolo[!duplicated(subpolo$SUB_KEY),c(1,3)]
df = merge(df,subeng, by="SUB_KEY")


library(data.table)
deng=fread("/projects/b1078/Newsday/Rawdata/Complete/digital_engagement_topics.csv", header = T, sep = ',')
setnames(deng, old="userid", new="polopoly_id")
dengsub = merge(subpolo[,1:2],deng,by="polopoly_id")

# optimum dummy #
dengsubopt = dengsub[,c(2,3)]
dengsubopt$opt = substr(dengsubopt[,2],1,3)
dengsubopt$isopt = 0
dengsubopt[dengsubopt$opt=="Opt","isopt"]=1
dengsubopt = aggregate(isopt~SUB_KEY,dengsubopt,sum)
dengsubopt[dengsubopt$isopt>1,"isopt"]=1
#end of optimum dummy#


# papguday
dengsub$pageuday = dengsub$articlepageuday = 0;
dengsub[dengsub$unique_days_raw>0,"pageuday"] = dengsub[dengsub$unique_days_raw>0,"pageviews_raw"]/dengsub[dengsub$unique_days_raw>0,"unique_days_raw"];
dengsub[dengsub$unique_days_raw>0,"articlepageuday"] = dengsub[dengsub$unique_days_raw>0,"articlepageviews_raw"]/dengsub[dengsub$unique_days_raw>0,"unique_days_raw"];


# decompose function #
digital_data = dengsub 
subscription_data = df
vars = c("unique_days_raw", "pageviews_raw", "articlepageviews_raw", "timeperpage_raw", "breadth_of_sections_raw", "pageuday", "articlepageuday")

digital_data = digital_data[order(digital_data$SUB_KEY, digital_data$date), c("polopoly_id", "SUB_KEY", vars, "date")]
digital_data[is.na(digital_data)] = 0
digi_data_aggr_pv = aggregate(. ~ SUB_KEY + date, digital_data[,c("SUB_KEY","pageviews_raw","articlepageviews_raw","date")], sum)
digi_data_aggr_ds = aggregate(. ~ SUB_KEY + date, digital_data[,c("SUB_KEY","unique_days_raw","breadth_of_sections_raw","date")], max)
digi_data_aggr_ti = aggregate(. ~ SUB_KEY + date, digital_data[,c("SUB_KEY","timeperpage_raw","pageuday", "articlepageuday","date")], mean)
digi_data_aggr = merge(digi_data_aggr_pv,digi_data_aggr_ds,by=c("SUB_KEY","date"))
digi_data_aggr = merge(digi_data_aggr,digi_data_aggr_ti,by=c("SUB_KEY","date"))
digi_data_aggr = digi_data_aggr[order(digi_data_aggr$SUB_KEY, digi_data_aggr$date), ]

## modifying the data format.
digi_data_aggr$yy = substr(digi_data_aggr$date,1,4)
digi_data_aggr$mm = substr(digi_data_aggr$date,5,6)
digi_data_aggr$dd = substr(digi_data_aggr$date,7,8)
digi_data_aggr$date1 = paste(digi_data_aggr$yy,digi_data_aggr$mm,digi_data_aggr$dd, sep="-")
digi_data_aggr$date = as.Date(digi_data_aggr$date1)
digi_data_aggr = digi_data_aggr[,1:(length(vars)+2)]

## define some assistant variables. intermediate variables
subscription_data$date1= subscription_data$date2 = NA ## you'll find out later that date1 is the starting date of the two engagement records where the starting date of a subscription continuation week lands between 

temp_vars_1 <- paste0(vars, "_1")
temp_vars_2 <- paste0(vars, "_2")
temp_sub_df = as.data.frame(x = matrix(data = NA, nrow = nrow(subscription_data), ncol = 2 * length(vars), dimnames = list(NULL, c(temp_vars_1 , temp_vars_2))))
nij=nrow(subscription_data) ## this is the number of total active weeks.

#subscription_data_init = subscription_data ## record the initial status of subscription_data.

pb <- txtProgressBar(style = 3, min = 0, max = nij)
for (i in 1:nij) {
  
  if (subscription_data$engage[i]==1) { ## if someone is digitally engaged, then build his/her covariates.
    test = digi_data_aggr[which(digi_data_aggr$SUB_KEY == subscription_data[i,1]),] ## extract the digital engagement activities of the subscriber to whom week i belongs.
    nj = dim(test)[1] ## nj is the row number of 'test' hence is the number of weeks of records of engagement.
    j = 1 ## define an looping index j here. 
    while(j<=nj)  { ## now let j vary from 1 to nj.
      if (subscription_data$start_date[i] == test$date[j])  { ## (Case 1) this means the current week start date is the same as this current engagement record week. 
        subscription_data$date1[i] = subscription_data$date2[i] = test$date[j]
        temp_sub_df[i, temp_vars_1] = temp_sub_df[i, temp_vars_2] = test[j, vars]
        # subscription_data$pv1[i] = subscription_data$pv2[i] = test$pageviews_raw[j] #date j - pageviews
        # subscription_data$apv1[i] = subscription_data$apv2[i] = test$articlepageviews_raw[j]
      }
      else if (subscription_data$start_date[i]<test$date[j] & j==1){ ## test$date[1] (Case 2) this means that the iteration is at the first engagement date/week but the subscriber hasn't started digital engagement.
        if (is.na(subscription_data$date2[i])) { ## if date2 is NA (notice that the initial value is NA), then assign date2 with the first engagement date, date1 is left still as NA
          subscription_data$date2[i] = test$date[j]   #date1 missing
          temp_sub_df[i, temp_vars_2] = test[j, vars]
          
          # subscription_data$pv2[i] = test$pageviews_raw[j]
          # subscription_data$apv2[i] = test$articlepageviews_raw[j]
        }
      }
      else if (subscription_data$start_date[i]>test$date[j] & subscription_data$start_date[i]<test$date[j+1] & j!=nj){ ## (Case 3) this means that the start date of the subscription continuation week i is between digital engagement date j and j + 1 and j is not the last engagement circle.
        if (is.na(subscription_data$date1[i])) { ## this check whether date1 has been assigned a value or not. 
          subscription_data$date1[i] = test$date[j]
          temp_sub_df[i, temp_vars_1] =  test[j, vars]
          
          # subscription_data$pv1[i] = test$pageviews_raw[j]
          # subscription_data$apv1[i] = test$articlepageviews_raw[j]
          
          subscription_data$date2[i] = test$date[j+1]
          temp_sub_df[i, temp_vars_2] = test[ j + 1, vars]
          
          # subscription_data$pv2[i] = test$pageviews_raw[j+1]
          # subscription_data$apv2[i] = test$articlepageviews_raw[j+1]          
        }        
      }
      else if (subscription_data$start_date[i]>test$date[j] & j==nj){ ## (Case 4) this is the opposite to Case 2. Now the subscription continues but the digital engagement has stopped.
        if (is.na(subscription_data$date1[i])) { ## date1 is assigned with the last engagement value but date2 left as it was (was NA).
          subscription_data$date1[i] = test$date[j]
          temp_sub_df[i, temp_vars_1] =  test[j, vars]
          
          # subscription_data$pv1[i] = test$pageviews_raw[j]
          # subscription_data$apv1[i] = test$articlepageviews_raw[j]
        }        
      }
      if(!is.na(subscription_data$date1[i]) & !is.na(subscription_data$date2[i])) ## if the condition checks out (I mean if the thing in the bracket is TRUE), then either case 1 or case 3 happens, and we jump out of the while loop. 
      {j = nj + 1} else {j=j+1} ## Otherwise j becomes j+1.
    }
  }
  setTxtProgressBar(pb, i)
}
close(pb)

temp_sub_df_final <- as.data.frame(x = matrix(data = 0, nrow = nrow(subscription_data), ncol = length(vars), dimnames = list(NULL, vars)))

# subscription_data$apv=subscription_data$pv=0 ## This will be the final column of apv and pv.
subscription_data$date1 = as.Date(subscription_data$date1,origin = "1970-01-01")
subscription_data$date2 = as.Date(subscription_data$date2,origin = "1970-01-01")
subscription_data$ddate = as.numeric(subscription_data$date2-subscription_data$date1) ## this is the difference between date2 and date1
subscription_data$do1 = as.numeric(subscription_data$start_date-subscription_data$date1) 
subscription_data$d2o = as.numeric(subscription_data$date2-subscription_data$start_date)

test = which(is.na(subscription_data$date1) & !is.na(subscription_data$date2)) ## this correspondes to Case 2
temp_sub_df_final[test, c("unique_days_raw", "pageviews_raw", "articlepageviews_raw", "breadth_of_sections_raw")] =((60-subscription_data[test,]$d2o)/60)*temp_sub_df[test, c("unique_days_raw_2","pageviews_raw_2", "articlepageviews_raw_2","breadth_of_sections_raw_2")] 
temp_sub_df_final[test, c("timeperpage_raw","pageuday", "articlepageuday")] =temp_sub_df[test, c("timeperpage_raw_2","pageuday_2", "articlepageuday_2")] 

test = which(!is.na(subscription_data$date1) & is.na(subscription_data$date2)) ## this correspondes to Case 4
temp_sub_df_final[test, c("unique_days_raw", "pageviews_raw", "articlepageviews_raw", "breadth_of_sections_raw")] =((60-subscription_data[test,]$do1)/60)*temp_sub_df[test, c("unique_days_raw_1","pageviews_raw_1", "articlepageviews_raw_1","breadth_of_sections_raw_1")] 
temp_sub_df_final[test, c("timeperpage_raw","pageuday", "articlepageuday")] =temp_sub_df[test, c("timeperpage_raw_1","pageuday_1", "articlepageuday_1")] 

test=which(!is.na(subscription_data$date1) & !is.na(subscription_data$date2) & subscription_data$ddate>60 & subscription_data$d2o<60 & subscription_data$do1>=60) ## this indicates that the landing interval is larger than 60 days, but is within 60 days to the ending date of the landing interval.
temp_sub_df_final[test, c("unique_days_raw", "pageviews_raw", "articlepageviews_raw", "breadth_of_sections_raw")] =((60-subscription_data[test,]$d2o)/60)*temp_sub_df[test, c("unique_days_raw_2","pageviews_raw_2", "articlepageviews_raw_2","breadth_of_sections_raw_2")]
temp_sub_df_final[test, c("timeperpage_raw","pageuday", "articlepageuday")] =temp_sub_df[test, c("timeperpage_raw_2","pageuday_2", "articlepageuday_2")] 

test=which(!is.na(subscription_data$date1) & !is.na(subscription_data$date2) & subscription_data$ddate>60 & subscription_data$do1<60 & subscription_data$d2o>=60)
temp_sub_df_final[test, c("unique_days_raw", "pageviews_raw", "articlepageviews_raw", "breadth_of_sections_raw")] =((60-subscription_data[test,]$do1)/60)*temp_sub_df[test, c("unique_days_raw_1","pageviews_raw_1", "articlepageviews_raw_1","breadth_of_sections_raw_1")] 
temp_sub_df_final[test, c("timeperpage_raw","pageuday", "articlepageuday")] =temp_sub_df[test, c("timeperpage_raw_1","pageuday_1", "articlepageuday_1")] 

test=which(!is.na(subscription_data$date1) & !is.na(subscription_data$date2) & subscription_data$ddate>60 & subscription_data$do1<60 & subscription_data$d2o<60)
temp_sub_df_final[test, c("unique_days_raw", "pageviews_raw", "articlepageviews_raw", "breadth_of_sections_raw")] =((60-subscription_data[test,]$do1)/60)*temp_sub_df[test, c("unique_days_raw_1","pageviews_raw_1", "articlepageviews_raw_1","breadth_of_sections_raw_1")] +((60-subscription_data[test,]$d2o)/60)*temp_sub_df[test, c("unique_days_raw_2","pageviews_raw_2", "articlepageviews_raw_2","breadth_of_sections_raw_2")]
temp_sub_df_final[test, c("timeperpage_raw","pageuday", "articlepageuday")] = (temp_sub_df[test, c("timeperpage_raw_1","pageuday_1", "articlepageuday_1")] + temp_sub_df[test, c("timeperpage_raw_2","pageuday_2", "articlepageuday_2")])/2

test=which(!is.na(subscription_data$date1) & !is.na(subscription_data$date2) & subscription_data$ddate<=60 & subscription_data$date1!=subscription_data$date2)
temp_sub_df_final[test, c("unique_days_raw", "pageviews_raw", "articlepageviews_raw", "breadth_of_sections_raw")] = (subscription_data[test,]$do1/(subscription_data[test,]$do1+subscription_data[test,]$d2o))*temp_sub_df[test, c("unique_days_raw_2","pageviews_raw_2", "articlepageviews_raw_2","breadth_of_sections_raw_2")] +(subscription_data[test,]$d2o/(subscription_data[test,]$do1+subscription_data[test,]$d2o))*temp_sub_df[test, c("unique_days_raw_1","pageviews_raw_1", "articlepageviews_raw_1","breadth_of_sections_raw_1")]
temp_sub_df_final[test, c("timeperpage_raw","pageuday", "articlepageuday")] = (temp_sub_df[test, c("timeperpage_raw_1","pageuday_1", "articlepageuday_1")] + temp_sub_df[test, c("timeperpage_raw_2","pageuday_2", "articlepageuday_2")])/2

test=which(!is.na(subscription_data$date1) & !is.na(subscription_data$date2) & subscription_data$ddate==0)
temp_sub_df_final[test, vars] = temp_sub_df[test, temp_vars_2]

temp_sub_df_final_vector <- unlist(temp_sub_df_final) 
temp_sub_df_final_vector[temp_sub_df_final_vector < 0 ] <- 0
temp_sub_df_final <- as.data.frame(matrix(temp_sub_df_final_vector, nrow = nrow(temp_sub_df_final), ncol = ncol(temp_sub_df_final), dimnames = list(NULL, vars)))
temp_sub_df_final[which(temp_sub_df_final$unique_days_raw==0 & temp_sub_df_final$timeperpage_raw>0),c("timeperpage_raw", "pageuday", "articlepageuday")]=0

# subscription_data[which(subscription_data$apv<0),]$apv=0
# subscription_data[which(subscription_data$pv<0),]$pv=0

transformed_data = cbind(subscription_data[,c(1:5)], temp_sub_df_final)
transformed_data$noengage_tv = 0;  # time-varying no engagement index = 1 if there is zero engagement at that time and 0 otherwise.
transformed_data[transformed_data$unique_days_raw==0,]$noengage_tv = 1;
transformed_data[transformed_data$orig_paper=="BRAIN BENDER",]$noengage_tv = 0;  

# baseline hazard dummies = bweek #  larger than 60weeks  -  60 weeks
table(transformed_data$week)
transformed_data$bweek = transformed_data$week
transformed_data$bweek[transformed_data$bweek>60]=60
transformed_data$b4week[transformed_data$bweek<=4]=1
transformed_data$b4week[transformed_data$bweek>4 & transformed_data$bweek<=8]=2
transformed_data$b4week[transformed_data$bweek>8 & transformed_data$bweek<=12]=3
transformed_data$b4week[transformed_data$bweek>12 & transformed_data$bweek<=16]=4
transformed_data$b4week[transformed_data$bweek>16 & transformed_data$bweek<=20]=5
transformed_data$b4week[transformed_data$bweek>20 & transformed_data$bweek<=24]=6
transformed_data$b4week[transformed_data$bweek>24 & transformed_data$bweek<=28]=7
transformed_data$b4week[transformed_data$bweek>28 & transformed_data$bweek<=32]=8
transformed_data$b4week[transformed_data$bweek>32 & transformed_data$bweek<=36]=9
transformed_data$b4week[transformed_data$bweek>36 & transformed_data$bweek<=40]=10
transformed_data$b4week[transformed_data$bweek>40 & transformed_data$bweek<=44]=11
transformed_data$b4week[transformed_data$bweek>44 & transformed_data$bweek<=48]=12
transformed_data$b4week[transformed_data$bweek>48 & transformed_data$bweek<=52]=13
transformed_data$b4week[transformed_data$bweek>52 & transformed_data$bweek<=56]=14
transformed_data$b4week[transformed_data$bweek>56 & transformed_data$bweek<=60]=15
transformed_data$b4week[transformed_data$bweek>60]=16
table(transformed_data$bweek)

# orig_paper and optimum (isopt==1) #
transformed_data = merge(sub[,c(1,3)],transformed_data,by="SUB_KEY")

transformed_data = merge(transformed_data,dengsubopt,by="SUB_KEY",all.x=TRUE)
transformed_data[is.na(transformed_data$isopt),]$isopt =0



# promotion and price data #
sub_promo = read.csv("/projects/b1078/Newsday/Rawdata/2017SUB_KEY_PROMO.csv")
sub_promo = merge(sub[,c("SUB_KEY", "orig_paper")],sub_promo, by="SUB_KEY")
sub_promo$EFF_DATE=as.Date(sub_promo$EFF_DATE,"%m/%d/%Y")
sub_promo = sub_promo[order(sub_promo$SUB_KEY, sub_promo$EFF_DATE),]

dfpromo = df[,1:4]
dfpromo$pro_key = dfpromo$term = dfpromo$pay = dfpromo$pct = dfpromo$packweek = 0
n = dim(sub_promo)[1]
for (i in 1:n) {
  if (sub_promo[i,]$orig_paper!="BRAIN BENDER") {
    dfpromo[which((dfpromo$SUB_KEY == sub_promo[i,1]) & (sub_promo[i,]$EFF_DATE<=dfpromo$start_date)),c("pct","pay","term","pro_key")] =sub_promo[i,c("PRO_PCT_NBR","weekly_pay","week_term","PRO_KEY")]
  } 
}

n = dim(dfpromo)[1]
for (i in 1:n) {
  if (dfpromo[i,"pro_key"]!=0) {
    if (i==1) {
      dfpromo[i,"packweek"] =  1;
    } else if ((dfpromo[i,"SUB_KEY"]!=dfpromo[i-1,"SUB_KEY"]) | (dfpromo[i,"pro_key"]!=dfpromo[i-1,"pro_key"])) {
      dfpromo[i,"packweek"] =  1;
    } else {
      dfpromo[i,"packweek"] = dfpromo[i-1,"packweek"] + 1;
    }
  }
}
dfpromo$packstart = dfpromo$packend = 0
dfpromo[which((dfpromo$term!=0) & (dfpromo$term==dfpromo$packweek)),"packend"]=1
dfpromo[which(dfpromo$packweek==1),"packstart"]=1
dfpromo$start_date=dfpromo$end_date=NULL

transformed_data$BB = 0
transformed_data[which(transformed_data$orig_paper=="BRAIN BENDER"),"BB"] = 1
transformed_data = merge(transformed_data,dfpromo,by=c("SUB_KEY","week"))
transformed_data = transformed_data[order(transformed_data$SUB_KEY,transformed_data$start_date),]


cohort = transformed_data[which(transformed_data$week==1),c("SUB_KEY","start_date")]
cohort$cohort = 1;
cohort[which(cohort$start_date>=as.Date("2017-02-01")),"cohort"] =2
cohort[which(cohort$start_date>=as.Date("2017-03-01")),"cohort"] =3
cohort$start_date=NULL
transformed_data = merge(transformed_data,cohort,by="SUB_KEY")

head(transformed_data, n = 20)
length(unique(transformed_data$SUB_KEY[transformed_data$isopt == F]))  # 2629 unique non-optimum subscribers
str(transformed_data)
transformed_data$orig_paper <- droplevels(transformed_data$orig_paper)
unique(transformed_data$orig_paper)

### add email list as fixed variable
setwd("/projects/b1078/Newsday/Rawdata/Complete")
subscriber <- fread("Subscriber_table.csv")
transformed_data <- transformed_data %>%
  dplyr::left_join(subscriber[,c(2:3)], by = "SUB_KEY")   # add subaccountnumber to dataframe
list <- fread("list_membership.csv")
str(list)
length(unique(list$SUBACCTNBR))  #210664 obs with email list subs

setwd("~/R/NewsDay_HH")
library(readxl)
listmatch <- readxl::read_excel("listmatch.xlsx")
head(listmatch)

list1 <- list %>%
  dplyr::left_join(listmatch[,-2], by = "list") %>%
  dplyr::filter(bounce==0 & testlists == 0) %>%
  dplyr::group_by(SUBACCTNBR) %>%
  dplyr::summarise(list_dummy = T,
                   local_lists_dummy = any(as.logical(local_lists)),  # combined + real_estate + local_politics + schoolsports
                   sports_dummy = any(as.logical(sports)),
                   business_dummy = any(as.logical(business)),
                   breakingnews_dummy = any(as.logical(breakingnews)),
                   entertainment_dummy = any(as.logical(entertainment)),
                   food_dummy = any(as.logical(food)),  # food
                   politics_dummy = any(as.logical(politics)),
                   family_dummy = any(as.logical(family)), #
                   newsdaynow_dummy = any(as.logical(newsdaynow)),
                   travel_dummy = any(as.logical(travel)),
                   real_estate_dummy = any(as.logical(real_estate)),  #
                   bestbets_dummy = any(as.logical(bestbets)), #local_things_to_do + family
                   localpolitics_dummy = any(as.logical(local_politics)), #
                   schoolsports_dummy = any(as.logical(high_school_sports))) #
list1$local_things_todo <- F
list1$local_things_todo[list1$bestbets_dummy == T | list1$family_dummy == T | list1$food_dummy == T ] <- T
list1$local_news <- F
list1$local_news[list1$local_lists_dummy == T | list1$real_estate_dummy == T | list1$localpolitics_dummy == T | list1$schoolsports_dummy == T] <- T
head(list1)

# add email list variables to transformed data
transformed_data$SUB_ACCOUNT_NBR <- as.character(transformed_data$SUB_ACCOUNT_NBR)
transformed_data1 <- transformed_data %>%
  dplyr::left_join(list1, by = c("SUB_ACCOUNT_NBR" = "SUBACCTNBR")) 
length(unique(transformed_data1$SUB_KEY[transformed_data1$isopt == F &transformed_data1$list_dummy == T])) # 668 non-optimum readers with email list subscriptions

# data type coersion = final data frame - transformed_data1
colnames(transformed_data1)
str(transformed_data1)
transformed_data1$list_dummy[is.na(transformed_data1$list_dummy)] <- F
transformed_data1[,c(25:40)][is.na(transformed_data1[,c(25:40)])] <- F
transformed_data1$SUB_ACCOUNT_NBR <- as.integer(transformed_data1$SUB_ACCOUNT_NBR)
transformed_data1$packend <- factor(transformed_data1$packend)

# add cohort variable
cohort <-  transformed_data[which(transformed_data$week==1),c("SUB_KEY","start_date")]
cohort$cohort <-  1;
cohort[which(cohort$start_date>=as.Date("2017-02-01")),"cohort"] <- 2
cohort[which(cohort$start_date>=as.Date("2017-03-01")),"cohort"] <- 3
cohort$start_date <- NULL
transformed_data2 <-  merge(transformed_data1,cohort,by="SUB_KEY")

# fit a logit regression #
fitdata = transformed_data2[transformed_data2$isopt==0 & transformed_data2$orig_paper!="BRAIN BENDER",]
fitdata$orig_paper <- droplevels(fitdata$orig_paper)
round(cor(fitdata[,c(7:13)]),2)
round(cor(fitdata[fitdata$noengage_tv==0,c(7:13)]),2)


# One engagement variable #
fit_pv = glm(unsub~orig_paper*noengage_tv+orig_paper*pageviews_raw+orig_paper*pay+orig_paper*pct+orig_paper*packend+orig_paper*factor(b4week) + 
               sports_dummy + business_dummy + breakingnews_dummy + entertainment_dummy + politics_dummy +
               newsdaynow_dummy + local_things_todo + local_news,binomial,fitdata)
summary(fit_pv)

fit_apv = glm(unsub~orig_paper*noengage_tv+orig_paper*articlepageviews_raw+orig_paper*pay+orig_paper*pct+orig_paper*packend+orig_paper*factor(b4week) + 
                sports_dummy + business_dummy + breakingnews_dummy + entertainment_dummy + politics_dummy +
               newsdaynow_dummy + local_things_todo + local_news,binomial,fitdata)
summary(fit_apv) #local_news; local_things_todo

fit_ud = glm(unsub~orig_paper*noengage_tv+orig_paper*unique_days_raw+orig_paper*pay+orig_paper*pct+orig_paper*packend+orig_paper*factor(b4week) + 
               sports_dummy + business_dummy + breakingnews_dummy + entertainment_dummy + politics_dummy +
               newsdaynow_dummy + local_things_todo + local_news,binomial,fitdata)
summary(fit_ud)

fit_pvd = glm(unsub~orig_paper*noengage_tv+orig_paper*pageuday+orig_paper*pay+orig_paper*pct+orig_paper*packend+orig_paper*factor(b4week) + 
                sports_dummy + business_dummy + breakingnews_dummy + entertainment_dummy + politics_dummy +
                newsdaynow_dummy + local_things_todo + local_news,binomial,fitdata)
summary(fit_pvd)

fit_apvd = glm(unsub~orig_paper*noengage_tv+orig_paper*articlepageuday+orig_paper*pay+orig_paper*pct+orig_paper*packend+orig_paper*factor(b4week) + 
                 sports_dummy + business_dummy + breakingnews_dummy + entertainment_dummy + politics_dummy +
                 newsdaynow_dummy + local_things_todo + local_news,binomial,fitdata)
summary(fit_apvd)

fit_ti = glm(unsub~orig_paper*noengage_tv+orig_paper*timeperpage_raw+orig_paper*pay+orig_paper*pct+orig_paper*packend+orig_paper*factor(b4week) + 
               sports_dummy + business_dummy + breakingnews_dummy + entertainment_dummy + politics_dummy +
               newsdaynow_dummy + local_things_todo + local_news,binomial,fitdata)
summary(fit_ti)

fit_bs = glm(unsub~orig_paper*noengage_tv+orig_paper*breadth_of_sections_raw+orig_paper*pay+orig_paper*pct+orig_paper*packend+orig_paper*factor(b4week) + 
               sports_dummy + business_dummy + breakingnews_dummy + entertainment_dummy + politics_dummy +
               newsdaynow_dummy + local_things_todo + local_news,binomial,fitdata)
summary(fit_bs)

# Two engagement variables #
fit_pvd_ud = glm(unsub~orig_paper*noengage_tv+orig_paper*pageuday+orig_paper*unique_days_raw+orig_paper*pay+orig_paper*pct+orig_paper*packend+orig_paper*factor(b4week) + sports_dummy + business_dummy + breakingnews_dummy + entertainment_dummy + politics_dummy +
                   newsdaynow_dummy + local_things_todo + local_news,binomial,fitdata)
summary(fit_pvd_ud)

fit_apvd_ud = glm(unsub~orig_paper*noengage_tv+orig_paper*articlepageuday+orig_paper*unique_days_raw+orig_paper*pay+orig_paper*pct+orig_paper*packend+orig_paper*factor(b4week) + sports_dummy + business_dummy + breakingnews_dummy + entertainment_dummy + politics_dummy +
                    newsdaynow_dummy + local_things_todo + local_news,binomial,fitdata)
summary(fit_apvd_ud)

fit_ud_bs = glm(unsub~orig_paper*noengage_tv+orig_paper*unique_days_raw+orig_paper*breadth_of_sections_raw+orig_paper*pay+orig_paper*pct+orig_paper*packend+orig_paper*factor(b4week) + sports_dummy + business_dummy + breakingnews_dummy + entertainment_dummy + politics_dummy +
                  newsdaynow_dummy + local_things_todo + local_news,binomial,fitdata)
summary(fit_ud_bs) # expect the sign breadths of sections & unique_days_raw


# #try pct*engagement_var
# fit_ud_pct = glm(unsub~orig_paper*noengage_tv+orig_paper*unique_days_raw*pct+orig_paper*pay+orig_paper*pct+orig_paper*packend+orig_paper*factor(b4week) + 
#                sports_dummy + business_dummy + breakingnews_dummy + entertainment_dummy + politics_dummy +
#                newsdaynow_dummy + local_things_todo + local_news,binomial,fitdata)
# summary(fit_ud_pct)

## descriptive - 52 weeks - week variable
## survival graph
# prepare data frame for creating graphs
table(fitdata$week, fitdata$unsub)
data1 <- fitdata %>%
  dplyr::filter(week <= 52) %>%
  dplyr::group_by(week) %>%
  dplyr::summarise(sub = sum(unsub ==0), unsub = sum(unsub == 1)) %>%
  dplyr::mutate(tot = sub + unsub, unsub_probs = unsub/tot) 
data1$unsub_probs_cumsum <- cumsum(data1$unsub_probs)
data1$survival <- 1 - data1$unsub_probs_cumsum

# http://sape.inf.usi.ch/quick-reference/ggplot2/colour
library(ggplot2)
ggplot(data1, aes(x = week, y = survival)) + geom_step(color = "mediumpurple", linetype = 1) + 
  labs(title = "Survival Curve", x = "number of week", y = "survival probability") + scale_y_continuous(limits = c(0, 1)) + theme_classic()

## Add Weilin's code
atrisk = aggregate(SUB_KEY~week,fitdata, length)
event = aggregate(unsub~week,fitdata, sum)
life = cbind(atrisk,event[,2])
life$onh = 1-life[,3]/life[,2]
life$survival = cumprod(life$onh)
n=dim(life)[1]
a=c(0,life$week,life$week)
b=c(1,1,life$survival[1:n-1],life$survival)
suv = cbind(a,b)
suv = suv[order(a),]
plot(suv,type="l",ylab = "survival rate", xlab = "week")

#Survival Rate by Products
atrisk = aggregate(SUB_KEY~week,fitdata[fitdata$orig_paper=="NEWSDAY.COM",], length)
event = aggregate(unsub~week,fitdata[fitdata$orig_paper=="NEWSDAY.COM",], sum)
life = cbind(atrisk,event[,2])
life$onh = 1-life[,3]/life[,2]
life$survival = cumprod(life$onh)
n=dim(life)[1]
a=c(0,life$week,life$week)
b=c(1,1,life$survival[1:n-1],life$survival)
suv = cbind(a,b)
suv = suv[order(a),]
plot(suv,type="l",ylab = "survival rate", xlab = "week")

atrisk = aggregate(SUB_KEY~week,fitdata[fitdata$orig_paper=="NEWSDAY",], length)
event = aggregate(unsub~week,fitdata[fitdata$orig_paper=="NEWSDAY",], sum)
life = cbind(atrisk,event[,2])
life$onh = 1-life[,3]/life[,2]
life$survival = cumprod(life$onh)
n=dim(life)[1]
a=c(0,life$week,life$week)
b=c(1,1,life$survival[1:n-1],life$survival)
suv = cbind(a,b)
suv = suv[order(a),]
lines(suv,col=2)
legend(35,0.95,c("NEWSDAY.COM","NEWSDAY"),lty=1,col=c(1,2),cex=0.8,box.lty=0)


## churn graph on unique_days_raw
# find the most frequent combination of pay and pct - 
# 0.99, 0.901, term = 52; 2.49, 0.917, term = 26;
fitdata %>% 
  dplyr::group_by(pay, pct, term) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::arrange(-n)

# fit_ud = glm(unsub~orig_paper*noengage_tv+orig_paper*unique_days_raw+orig_paper*pay+orig_paper*pct+orig_paper*packend+orig_paper*factor(b4week) + 
#                sports_dummy + business_dummy + breakingnews_dummy + entertainment_dummy + politics_dummy +
#                newsdaynow_dummy + local_things_todo + local_news,binomial,fitdata)
summary(fitdata$unique_days_raw[fitdata$noengage_tv == 0])
data_uni <- data.frame(b4week = 2,orig_paper = "NEWSDAY.COM",unique_days_raw = 1:60,noengage_tv = 0,
                       pay = 0.99, pct = 0.901,packend = as.factor(0),
                       sports_dummy = F, business_dummy = F, breakingnews_dummy = F, 
                       entertainment_dummy = F, politics_dummy = F,
                       newsdaynow_dummy = F, local_things_todo = F, local_news = F)
data_uni1 <- data.frame(unique_days_raw = 1:60, churn_probability = predict(fit_ud, data_uni, type = "response"))
ggplot(data_uni1, aes(x = unique_days_raw, y = churn_probability)) + geom_line(color = "dodgerblue2") + 
  labs(title = "Churn Probability with unique_days_raw", x = "unique days", y = "churn probability") + 
  scale_y_continuous(limits = c(0, 0.04)) + theme_classic()

# one day increase - change in probability of churning
# when a reader reads for 1 day, on average an additional day reading will decrease his probability of churning by 0.057%.
predict(fit_ud, data_uni[data_uni$unique_days_raw == 2,], type = "response") - predict(fit_ud, data_uni[data_uni$unique_days_raw == 1,], type = "response")
predict(fit_ud, data_uni[data_uni$unique_days_raw == 3,], type = "response") - predict(fit_ud, data_uni[data_uni$unique_days_raw == 2,], type = "response")

# churn graph on breadths of sections
summary(fitdata$breadth_of_sections_raw[fitdata$noengage_tv == 0])
data_bs <- data.frame(b4week = 2,orig_paper = "NEWSDAY.COM", breadth_of_sections_raw = 1:10,noengage_tv = 0, 
                      pay = 0.99, pct = 0.901,packend = as.factor(0),
                       sports_dummy = F, business_dummy = F, breakingnews_dummy = F, 
                      entertainment_dummy = F, politics_dummy = F,
                       newsdaynow_dummy = F, local_things_todo = F, local_news = F)
data_bs1 <- data.frame(breadth_of_sections_raw = 1:10, churn_probability = predict(fit_bs, data_bs, type = "response"))
ggplot(data_bs1, aes(x = breadth_of_sections_raw, y = churn_probability)) + geom_line(color = "dodgerblue2") + 
  labs(title = "Churn Probability with breadth_of_secrions_raw", x = "breadths of sections read", y = "churn probability") +
  scale_y_continuous(limits = c(0, 0.04)) + scale_x_continuous(breaks = c(2,4,6,8,10)) + theme_classic()

# one more section - change in probability of churning
# when a reader reads one section, on average, one more section reading will decrease his probability of churning by 0.3%
predict(fit_bs, data_bs[data_bs$breadth_of_sections_raw == 2,], type = "response") - predict(fit_bs, data_bs[data_bs$breadth_of_sections_raw == 1,], type = "response")
predict(fit_bs, data_bs[data_bs$breadth_of_sections_raw == 3,], type = "response") - predict(fit_bs, data_bs[data_bs$breadth_of_sections_raw == 2,], type = "response")

## list - number of readers
fitdata %>%
  dplyr::filter(politics_dummy == 1) %>%
  dplyr::summarise(n_cust = n_distinct(SUB_KEY))

# sign up for email lists
fit_list = glm(unsub~orig_paper*noengage_tv+orig_paper*unique_days_raw+orig_paper*pay+orig_paper*pct+orig_paper*packend+orig_paper*factor(b4week) + list_dummy,binomial,fitdata)
data_list <- data.frame(b4week = 2,orig_paper = "NEWSDAY.COM",unique_days_raw = 1,noengage_tv = 0,
                       pay = 0.99, pct = 0.901,packend = as.factor(0),
                       list_dummy = c(T,F))
predict(fit_list, data_list[data_list$list_dummy == T,], type = "response") - predict(fit_list, data_list[data_list$list_dummy == F,], type = "response")

# opt out of newsday now
fit_list_nd = glm(unsub~orig_paper*noengage_tv+orig_paper*unique_days_raw+orig_paper*pay+orig_paper*pct+orig_paper*packend+orig_paper*factor(b4week) + 
               sports_dummy + business_dummy + breakingnews_dummy + entertainment_dummy + politics_dummy +
               newsdaynow_dummy + local_things_todo + local_news,binomial,fitdata)
data_list_nd <- data.frame(b4week = 2,orig_paper = "NEWSDAY.COM",unique_days_raw = 1, noengage_tv = 0,
                       pay = 0.99, pct = 0.901,packend = as.factor(0),
                       sports_dummy = F, business_dummy = F, breakingnews_dummy = F, 
                       entertainment_dummy = F, politics_dummy = F,
                       newsdaynow_dummy = c(T,F), local_things_todo = F, local_news = F)
predict(fit_list_nd, data_list_nd[data_list_nd$newsdaynow_dummy == T,], type = "response") - predict(fit_list_nd, data_list_nd[data_list_nd$newsdaynow_dummy == F,], type = "response")
