#Reshaping Code (R)
library(dplyr)
library(tidyverse)
library(geosphere)
library(lubridate)
library(stringr)

mls <- read_csv("~/SDS379R/austin_mls_all_1996_2018.csv")
permits <- read_csv("~/SDS379R/Issued_Construction_Permits.csv")
permits_selected <- permits %>% select(c(`Permit Type`, `Permit Num`, `Permit Class Mapped`, `Permit Class`, `TCAD ID`, `Applied Date`, `Issued Date`, `Calendar Year Issued`, `Fiscal Year Issued`, `Status Current`, `Status Date`, `Expires Date`, `Completed Date`, `Original Address 1`, `Original City`, `Original State`, `Original Zip`, `Project ID`, Latitude, Longitude))

permits_residential <- permits_selected %>% filter(`Permit Class Mapped` == "Residential")
permits_commercial <- permits_selected %>% filter(`Permit Class Mapped` == "Commercial")

permits_residential <- permits_residential %>% filter(`Status Current` == "Final" | `Status Current` == "Active" | `Status Current` == "Closed")
permits_commercial <- permits_commercial %>% filter(`Status Current` == "Final" | `Status Current` == "Active" | `Status Current` == "Closed")

permits_residential$Importance <- NA
permits_residential$Importance[permits_residential$`Permit Type` == "BP" | permits_residential$`Permit Type` == "MP" |permits_residential$`Permit Type` == "EP"] <- "important"
permits_residential$Importance[permits_residential$`Permit Type` == "DS" | permits_residential$`Permit Type` == "PP"] <- "nonimportant"

permits_commercial$Importance <- NA
permits_commercial$Importance[permits_commercial$`Permit Type` == "BP" | permits_commercial$`Permit Type` == "MP" |permits_commercial$`Permit Type` == "EP"] <- "important"
permits_commercial$Importance[permits_commercial$`Permit Type` == "DS" | permits_commercial$`Permit Type` == "PP"] <- "nonimportant"

permits_commercial <- permits_commercial %>% na.omit
permits_residential <- permits_residential %>% na.omit

mls <- mls %>% select(c(3,4,5,13,14,16,22,23,24,25))
mls$`List Date` <- as.Date(mls$`List Date`,format="%m/%d/%Y") 

mls <- rename(mls, "Latitude_mls" = "Latitude")
mls <- rename(mls, "Longitude_mls" = "Longitude")
permits_residential <- rename(permits_residential, "Latitude_perm" = "Latitude")
permits_residential <- rename(permits_residential, "Longitude_perm" = "Longitude")

permits_commercial <- rename(permits_commercial, "Latitude_perm" = "Latitude")
permits_commercial <- rename(permits_commercial, "Longitude_perm" = "Longitude")

permits_selected <- rbind(permits_residential, permits_commercial)
mls <- mls %>% filter(Year >= 2008)
permits_selected <- permits_selected %>% filter(`Calendar Year Issued` >= 2008)

new_data_frame <- data.frame(matrix(ncol = 41, nrow = 0))

colnames(new_data_frame) <- c("MLS #", "Residential_Important_0-0.2", "Residential_Important_0.21-0.4", "Residential_Important_0.41-0.6", "Residential_Important_0.61-0.8", "Residential_Important_0.81-1", "Residential_Important_1.01-1.2", "Residential_Important_1.21-1.4", "Residential_Important_1.41-1.6", "Residential_Important_1.61-1.8", "Residential_Important_1.81-2.0", "Residential_Non-Important_0-0.2", "Residential_Non-Important_0.21-0.4", "Residential_Non-Important_0.41-0.6", "Residential_Non-Important_0.61-0.8", "Residential_Non-Important_0.81-1", "Residential_Non-Important_1.01-1.2", "Residential_Non-Important_1.21-1.4", "Residential_Non-Important_1.41-1.6", "Residential_Non-Important_1.61-1.8", "Residential_Non-Important_1.81-2.0", "Commercial_Important_0-0.2", "Commercial_Important_0.21-0.4", "Commercial_Important_0.41-0.6", "Commercial_Important_0.61-0.8", "Commercial_Important_0.81-1", "Commercial_Important_1.01-1.2", "Commercial_Important_1.21-1.4", "Commercial_Important_1.41-1.6", "Commercial_Important_1.61-1.8", "Commercial_Important_1.81-2.0", "Commercial_Non-Important_0-0.2", "Commercial_Non-Important_0.21-0.4", "Commercial_Non-Important_0.41-0.6", "Commercial_Non-Important_0.61-0.8", "Commercial_Non-Important_0.81-1", "Commercial_Non-Important_1.01-1.2", "Commercial_Non-Important_1.21-1.4", "Commercial_Non-Important_1.41-1.6", "Commercial_Non-Important_1.61-1.8", "Commercial_Non-Important_1.81-2.0")
for (mls_data in 1:125160) {
    lat_mls <- mls[mls_data,5]
    long_mls <- mls[mls_data,4]
    date_mls <- mls[mls_data,7]
    temp <- permits_selected
    temp$mls_lat <- lat_mls
    temp$mls_long <- long_mls
    temp$mls_date <- date_mls
    temp$diff_date <- as.numeric(temp$mls_date$`List Date` - temp$`Completed Date`)
    temp$dist <- geodist::geodist_vec(x1 = unlist(temp$mls_long$Longitude_mls), y1 = unlist(temp$mls_lat$Latitude_mls), x2 = unlist(temp$Longitude_perm), y2 = unlist(temp$Latitude_perm), paired = TRUE, measure = "haversine")
    temp$dist <- temp$dist / 1609.34
    new_data_frame[mls_data,1] <- mls[mls_data,1]
    temp_1 <- temp %>% filter(`Permit Class Mapped` == "Residential") %>% filter(Importance == "important") %>% filter(diff_date >= 0 & diff_date <= 365)
    new_data_frame[mls_data,2] <- temp_1 %>% filter(dist <= 0.2) %>% nrow
    new_data_frame[mls_data,3] <- temp_1 %>% filter(dist > 0.2 & dist <= 0.4) %>% nrow
    new_data_frame[mls_data,4] <- temp_1 %>% filter(dist > 0.4 & dist <= 0.6) %>% nrow
    new_data_frame[mls_data,5] <- temp_1 %>% filter(dist > 0.6 & dist <= 0.8) %>% nrow
    new_data_frame[mls_data,6] <- temp_1 %>% filter(dist > 0.8 & dist <= 1) %>% nrow
    new_data_frame[mls_data,7] <- temp_1 %>% filter(dist > 1 & dist <= 1.2) %>% nrow
    new_data_frame[mls_data,8] <- temp_1 %>% filter(dist > 1.2 & dist <= 1.4) %>% nrow
    new_data_frame[mls_data,9] <- temp_1 %>% filter(dist > 1.4 & dist <= 1.6) %>% nrow
    new_data_frame[mls_data,10] <- temp_1 %>% filter(dist > 1.6 & dist <= 1.8) %>% nrow
    new_data_frame[mls_data,11] <- temp_1 %>% filter(dist > 1.8 & dist <= 2) %>% nrow
    
    temp_2 <- temp %>% filter(`Permit Class Mapped` == "Residential") %>% filter(Importance == "nonimportant") %>% filter(diff_date >= 0 & diff_date <= 365)
    new_data_frame[mls_data,12] <- temp_2 %>% filter(dist <= 0.2) %>% nrow
    new_data_frame[mls_data,13] <- temp_2 %>% filter(dist > 0.2 & dist <= 0.4) %>% nrow
    new_data_frame[mls_data,14] <- temp_2 %>% filter(dist > 0.4 & dist <= 0.6) %>% nrow
    new_data_frame[mls_data,15] <- temp_2 %>% filter(dist > 0.6 & dist <= 0.8) %>% nrow
    new_data_frame[mls_data,16] <- temp_2 %>% filter(dist > 0.8 & dist <= 1) %>% nrow
    new_data_frame[mls_data,17] <- temp_2 %>% filter(dist > 1 & dist <= 1.2) %>% nrow
    new_data_frame[mls_data,18] <- temp_2 %>% filter(dist > 1.2 & dist <= 1.4) %>% nrow
    new_data_frame[mls_data,19] <- temp_2 %>% filter(dist > 1.4 & dist <= 1.6) %>% nrow
    new_data_frame[mls_data,20] <- temp_2 %>% filter(dist > 1.6 & dist <= 1.8) %>% nrow
    new_data_frame[mls_data,21] <- temp_2 %>% filter(dist > 1.8 & dist <= 2) %>% nrow

    temp_3 <- temp %>% filter(`Permit Class Mapped` == "Commercial") %>% filter(Importance == "important") %>% filter(diff_date >= 0 & diff_date <= 365)
    new_data_frame[mls_data,22] <- temp_3 %>% filter(dist <= 0.2) %>% nrow
    new_data_frame[mls_data,23] <- temp_3 %>% filter(dist > 0.2 & dist <= 0.4) %>% nrow
    new_data_frame[mls_data,24] <- temp_3 %>% filter(dist > 0.4 & dist <= 0.6) %>% nrow
    new_data_frame[mls_data,25] <- temp_3 %>% filter(dist > 0.6 & dist <= 0.8) %>% nrow
    new_data_frame[mls_data,26] <- temp_3 %>% filter(dist > 0.8 & dist <= 1) %>% nrow
    new_data_frame[mls_data,27] <- temp_3 %>% filter(dist > 1 & dist <= 1.2) %>% nrow
    new_data_frame[mls_data,28] <- temp_3 %>% filter(dist > 1.2 & dist <= 1.4) %>% nrow
    new_data_frame[mls_data,29] <- temp_3 %>% filter(dist > 1.4 & dist <= 1.6) %>% nrow
    new_data_frame[mls_data,30] <- temp_3 %>% filter(dist > 1.6 & dist <= 1.8) %>% nrow
    new_data_frame[mls_data,31] <- temp_3 %>% filter(dist > 1.8 & dist <= 2) %>% nrow
    
    temp_4 <- temp %>% filter(`Permit Class Mapped` == "Commercial") %>% filter(Importance == "nonimportant") %>% filter(diff_date >= 0 & diff_date <= 365)
    new_data_frame[mls_data,32] <- temp_4 %>% filter(dist <= 0.2) %>% nrow
    new_data_frame[mls_data,33] <- temp_4 %>% filter(dist > 0.2 & dist <= 0.4) %>% nrow
    new_data_frame[mls_data,34] <- temp_4 %>% filter(dist > 0.4 & dist <= 0.6) %>% nrow
    new_data_frame[mls_data,35] <- temp_4 %>% filter(dist > 0.6 & dist <= 0.8) %>% nrow
    new_data_frame[mls_data,36] <- temp_4 %>% filter(dist > 0.8 & dist <= 1) %>% nrow
    new_data_frame[mls_data,37] <- temp_4 %>% filter(dist > 1 & dist <= 1.2) %>% nrow
    new_data_frame[mls_data,38] <- temp_4 %>% filter(dist > 1.2 & dist <= 1.4) %>% nrow
    new_data_frame[mls_data,39] <- temp_4 %>% filter(dist > 1.4 & dist <= 1.6) %>% nrow
    new_data_frame[mls_data,40] <- temp_4 %>% filter(dist > 1.6 & dist <= 1.8) %>% nrow
    new_data_frame[mls_data,41] <- temp_4 %>% filter(dist > 1.8 & dist <= 2) %>% nrow
}
new_data_frame <- inner_join(mls, new_data_frame, by = "MLS #")
write.csv(new_data_frame, "~/SDS379R/Reshaped_Datasets/reshape.csv")

df <- read_csv("~/SDS379R/final_dataframe.csv")
df$avg_price_month_prev_year <- NA
for (y in 2009:2018){
  for (m in 1:12){
    df$avg_price_month_prev_year[df$Year == y & df$month == m] <- df %>% filter(Year == (y - 1)) %>% filter(month == m) %>% 
      summarise(median(`Sold/Lease Price`)) %>% as.numeric
  }
}
df <- df %>% filter(Year != 2008)
sapply(df, class)
df <- df %>% select(-1)
mls <- read_csv("~/SDS379R/austin_mls_all_1996_2018.csv")
mls <- mls %>% select(`MLS #`, `Sqft Total`)
df <- inner_join(mls, df)
write_csv(df, "~/SDS379R/housing_permits.csv")

#Box Plots (R) 
Final_data_set <- read_csv("~/SDS379R/Reshaped_Datasets/reshape.csv")
final_data_set$Year <- as.numeric(final_data_set$Year)
final_data_set$`Sold/Lease Price` <- as.numeric(parse_number(final_data_set$`Sold/Lease Price`))
final_data_set %>% group_by(Year) %>% summarize(median(`Sold/Lease Price`))
final_data_set$prev_med_property_value <- NA
final_data_set$prev_med_property_value[final_data_set$Year == 2009] <- 225000
final_data_set$prev_med_property_value[final_data_set$Year == 2010] <- 217000
final_data_set$prev_med_property_value[final_data_set$Year == 2011] <- 229000
final_data_set$prev_med_property_value[final_data_set$Year == 2012] <- 232000
final_data_set$prev_med_property_value[final_data_set$Year == 2013] <- 247900
final_data_set$prev_med_property_value[final_data_set$Year == 2014] <- 269000
final_data_set$prev_med_property_value[final_data_set$Year == 2015] <- 295000
final_data_set$prev_med_property_value[final_data_set$Year == 2016] <- 322500
final_data_set$prev_med_property_value[final_data_set$Year == 2017] <- 340990
final_data_set$price_obs_i_t <- final_data_set$`Sold/Lease Price` / final_data_set$prev_med_property_value
final_data_set$total_res_imp <- final_data_set %>% select(11:20) %>% rowSums()
final_data_set$total_res_nonimp <- final_data_set %>% select(21:30) %>% rowSums()
final_data_set$total_com_imp <- final_data_set %>% select(31:40) %>% rowSums()
final_data_set$total_com_nonimp <- final_data_set %>% select(41:50) %>% rowSums()
# bins: 0-1000, 1001-2000, 2001-max
hist(final_data_set$total_res_imp)
# bins: 0-500, 501-1000, 1001-max
hist(final_data_set$total_res_nonimp)
# bins: 0-500, 501-1000, 1001-max
hist(final_data_set$total_com_imp, breaks = 20)
# bins: 0-200, 201-400, 401-max
hist(final_data_set$total_com_nonimp)
final_data_set$res_imp_cat <- NA
final_data_set$res_imp_cat[final_data_set$total_res_imp <= 1000] <- "low"
final_data_set$res_imp_cat[final_data_set$total_res_imp > 1000 & final_data_set$total_res_imp <= 2000] <- "med"
final_data_set$res_imp_cat[final_data_set$total_res_imp > 2000] <- "high"
final_data_set$res_nonimp_cat <- NA
final_data_set$res_nonimp_cat[final_data_set$total_res_nonimp <= 500] <- "low"
final_data_set$res_nonimp_cat[final_data_set$total_res_nonimp > 500 & final_data_set$total_res_nonimp <= 1000] <- "med"
final_data_set$res_nonimp_cat[final_data_set$total_res_nonimp > 1000] <- "high"
final_data_set$com_imp_cat <- NA
final_data_set$com_imp_cat[final_data_set$total_com_imp <= 500] <- "low"
final_data_set$com_imp_cat[final_data_set$total_com_imp > 500 & final_data_set$total_com_imp <= 1000] <- "med"
final_data_set$com_imp_cat[final_data_set$total_com_imp > 1000] <- "high"
final_data_set$com_nonimp_cat <- NA
final_data_set$com_nonimp_cat[final_data_set$total_com_nonimp <= 200] <- "low"
final_data_set$com_nonimp_cat[final_data_set$total_com_nonimp > 200 & final_data_set$total_com_nonimp <= 400] <- "med"
final_data_set$com_nonimp_cat[final_data_set$total_com_nonimp > 400] <- "high"
final_data_set %>% filter(price_obs_i_t <= 3) %>% ggplot(aes(x=factor(res_imp_cat, levels=c('low', 'med', 'high')), y=price_obs_i_t)) + geom_boxplot(outlier.shape = NA) + xlab("Number of Important Residential Permits") + ylab("Relative Price") + facet_grid(~Year) + ggtitle("Number of Important Residential Permits vs Relative Price by Year")
final_data_set %>% filter(price_obs_i_t <= 3) %>% ggplot(aes(x=factor(res_nonimp_cat, levels=c('low', 'med', 'high')), y=price_obs_i_t)) + geom_boxplot(outlier.shape = NA) + xlab("Number of Non-Important Residential Permits") + ylab("Relative Price") + facet_grid(~Year) + ggtitle("Number of Non-Important Residential Permits vs Relative Price by Year")
final_data_set %>% filter(price_obs_i_t <= 3) %>% ggplot(aes(x=factor(com_imp_cat, levels=c('low', 'med', 'high')), y=price_obs_i_t)) + geom_boxplot(outlier.shape = NA) + xlab("Number of Important Commercial Permits") + ylab("Relative Price") + facet_grid(~Year) + ggtitle("Number of Important Commercial Permits vs Relative Price by Year")
final_data_set %>% filter(price_obs_i_t <= 3) %>% ggplot(aes(x=factor(com_nonimp_cat, levels=c('low', 'med', 'high')), y=price_obs_i_t)) + geom_boxplot(outlier.shape = NA) + xlab("Number of Non-Important Commercial Permits") + ylab("Relative Price") + facet_grid(~Year) + ggtitle("Number of Non-Important Commercial Permits vs Relative Price by Year")

#Natural Log vs Selling Price Histograms (R)
hist(df$natural_log, main = "Distribution of The Natural Log of Selling Prices", xlab = "Natural Log of Selling Prices")
hist(df$`Sold/Lease Price`, main = "Distribution of Selling Prices", xlab = "Selling Prices")
