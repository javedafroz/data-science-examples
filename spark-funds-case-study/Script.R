#Checkpoint 1:

library(dplyr)
library(tidyr)

companies <- read.delim("companies.txt")
rounds2 <- read.csv("rounds2.csv")

#Converting all data to lowercase for merging
companies <- mutate_all(companies,funs(tolower))
rounds2 <- mutate_all(rounds2,funs(tolower))

#Q - How many unique companies are present in rounds2?
rounds2_separated <- separate(rounds2, company_permalink, into=c("slash","organization", "company"), sep = "/",remove="F")
nrow(distinct(rounds2_separated, company))

#Q - How many unique companies are present in the companies file?
nrow(distinct(companies, name))

#Q - Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame.Name the merged frame master_frame.How many observations are present in master_frame ?
# Merging copanied with rounds2
master_frame <- merge(companies, rounds2, by.x = "permalink", by.y = "company_permalink")

#Q Average Values of Investments for Each of these Funding Types
#Grouping by funding_round_type
master_frame$raised_amount_usd <-as.numeric(master_frame$raised_amount_usd)
master_frame_grouped_by_funding_round_type <- group_by(master_frame, funding_round_type)
#Listing average raised_amount_usd by funding_round_type
result <- summarise(master_frame_grouped_by_funding_round_type, average_investment = mean(raised_amount_usd, na.rm = 'T'))
#Average funding amount of venture type
filter(result, result$funding_round_type == "venture")
#Average funding amount of angel type
filter(result, result$funding_round_type == "angel")
#Average funding amount of seed type
filter(result, result$funding_round_type == "seed")
#Average funding amount of private equity type
filter(result, result$funding_round_type == "private_equity")

#Q - Analysing the Top 3 English-Speaking Countries
#Countries who have english as one of the language
country_language <- read.csv(file = "countrylanguage.csv")
country_language <- mutate_all(country_language, funs(tolower))
master_frame_grouped_by_country_code <- group_by(master_frame, country_code)
total_raised_by_country_code <- summarise(master_frame_grouped_by_country_code, total_fund_raised = sum(raised_amount_usd,na.rm = TRUE))
merge_by_country <- merge(total_raised_by_country_code, country_language, by.x = "country_code", by.y = "CountryCode")
total_inv_by_english_speaking_country <- subset(merge_by_country, merge_by_country$englishSpeaking == "true")
arrange(total_inv_by_english_speaking_country, desc(total_fund_raised))[1:3,]

#Listing top 9 countries by total fund raised
arrange(total_raised_by_country_code, desc(total_fund_raised))[1:9,]


mapping <- read.csv("mapping.csv")
mapping <- mutate_all(mapping, funs(tolower))
library(stringr)

#Adding new column for storing primary catagory
master_frame$primary_sector <- NA
for(i in 1 : length(master_frame$category_list)) {
  #Extracting primary category or primary sector from the category_list
  master_frame$primary_sector [i] <- str_split_fixed(master_frame$category_list[i], pattern = "\\|", n = str_count(master_frame$category_list[i], "\\|") + 1)[1]
}

#Merging companies with mapping
master_frame2 <- merge(master_frame, mapping, by.x = "primary_sector", by.y = "category_list")
master_frame2 <- gather(master_frame2, main_sector, value, Automotive...Sports : Social..Finance..Analytics..Advertising)
master_frame2 <- master_frame2[!(master_frame2$value == 0),]

# Table 5.1 Sector-wise Investment Analysis	for USA, GBR, CAN			
D1 <- subset(master_frame, master_frame$country_code == "usa" 
             & master_frame$funding_round_type == "venture" 
             & raised_amount_usd>=5000000 & raised_amount_usd<=15000000)
D1_by_sector <- group_by(D1, primary_sector)
D1_summary_by_total <- summarise(D1_by_sector, total_amount_invested_in_this_sector = sum(raised_amount_usd, na.rm = TRUE))
D1_summary_by_count <- summarise(D1_by_sector, total_number_of_investment_in_this_sector = n())
D1 <- merge(D1, D1_summary_by_total, by = "primary_sector")
D1 <- merge(D1, D1_summary_by_count, by = "primary_sector")

D2 <- subset(master_frame, master_frame$country_code == "gbr" 
             & master_frame$funding_round_type == "venture"
             & raised_amount_usd>=5000000 & raised_amount_usd<=15000000)
D2_by_sector <- group_by(D2, primary_sector)
D2_summary_by_total <- summarise(D2_by_sector, total_amount_invested_in_this_sector = sum(raised_amount_usd, na.rm = TRUE))
D2_summary_by_count <- summarise(D2_by_sector, total_number_of_investment_in_this_sector = n())
D2 <- merge(D2, D2_summary_by_total, by = "primary_sector")
D2 <- merge(D2, D2_summary_by_count, by = "primary_sector")

D3 <- subset(master_frame, master_frame$country_code == "can" 
             & master_frame$funding_round_type == "venture"
             & raised_amount_usd>=5000000 & raised_amount_usd<=15000000)
D3_by_sector <- group_by(D3, primary_sector)
D3_summary_by_total <- summarise(D3_by_sector, total_amount_invested_in_this_sector = sum(raised_amount_usd, na.rm = TRUE))
D3_summary_by_count <- summarise(D3_by_sector, total_number_of_investment_in_this_sector = n())
D3 <- merge(D3, D3_summary_by_total, by = "primary_sector")
D3 <- merge(D3, D3_summary_by_count, by = "primary_sector")

#Total number of Investments in USA, GBR and CAN
length(D1$raised_amount_usd)
length(D2$raised_amount_usd)
length(D3$raised_amount_usd)

#Total amount of Investments in USA, GBR and CAN
sum(D1$raised_amount_usd)
sum(D2$raised_amount_usd)
sum(D3$raised_amount_usd)

#Top Sector name (no. of investment-wise) in USA, GBR and CAN
arrange(summarise(D1_by_sector, count = length(raised_amount_usd)), desc(count))[1,1]
arrange(summarise(D2_by_sector, count = length(raised_amount_usd)), desc(count))[1,1]
arrange(summarise(D3_by_sector, count = length(raised_amount_usd)), desc(count))[1,1]

#Second Sector name (no. of investment-wise) in USA, GBR and CAN
arrange(summarise(D1_by_sector, count = length(raised_amount_usd)), desc(count))[2,1]
arrange(summarise(D2_by_sector, count = length(raised_amount_usd)), desc(count))[2,1]
arrange(summarise(D3_by_sector, count = length(raised_amount_usd)), desc(count))[2,1]

#Third Sector name (no. of investment-wise) in USA, GBR and CAN
arrange(summarise(D1_by_sector, count = length(raised_amount_usd)), desc(count))[3,1]
arrange(summarise(D2_by_sector, count = length(raised_amount_usd)), desc(count))[3,1]
arrange(summarise(D3_by_sector, count = length(raised_amount_usd)), desc(count))[3,1]

#Number of investments in top sector (3) in USA, GBR and CAN
arrange(summarise(D1_by_sector, count = length(raised_amount_usd)), desc(count))[1,2]
arrange(summarise(D2_by_sector, count = length(raised_amount_usd)), desc(count))[1,2]
arrange(summarise(D3_by_sector, count = length(raised_amount_usd)), desc(count))[1,2]

#Number of investments in second sector (4) in USA, GBR and CAN
arrange(summarise(D1_by_sector, count = length(raised_amount_usd)), desc(count))[2,2]
arrange(summarise(D2_by_sector, count = length(raised_amount_usd)), desc(count))[2,2]
arrange(summarise(D3_by_sector, count = length(raised_amount_usd)), desc(count))[2,2]

#Number of investments in third sector (5) in USA, GBR and CAN
arrange(summarise(D1_by_sector, count = length(raised_amount_usd)), desc(count))[3,2]
arrange(summarise(D2_by_sector, count = length(raised_amount_usd)), desc(count))[3,2]
arrange(summarise(D3_by_sector, count = length(raised_amount_usd)), desc(count))[3,2]

#For point 3 (top sector count-wise), which company received the highest investment ?
arrange(filter(D1, primary_sector == "biotechnology"),desc(raised_amount_usd))$name[1]
arrange(filter(D2, primary_sector == "biotechnology"),desc(raised_amount_usd))$name[1]
arrange(filter(D3, primary_sector == "software"),desc(raised_amount_usd))$name[1]

#For point 4 (second best sector count-wise), which company received the highest investment?
arrange(filter(D1, primary_sector == "software"),desc(raised_amount_usd))$name[1]
arrange(filter(D2, primary_sector == "software"),desc(raised_amount_usd))$name[1]
arrange(filter(D3, primary_sector == "biotechnology"),desc(raised_amount_usd))$name[1]
