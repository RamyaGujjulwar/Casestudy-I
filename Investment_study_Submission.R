#===========================================Investment Case Study==============================================#



# Required Library
#####################################################
library(stringr)
library(tidyr)
library(dplyr)

#==============================================================================================================
"
1. Company details: 
"
companies <- read.delim("companies.txt", header=TRUE,stringsAsFactors = FALSE, sep = "\t",na.strings = TRUE)
companies[,sapply(companies,is.character)] <- sapply(
  companies[,sapply(companies,is.character)],
  iconv,"ISO-8859-1","Shift_JIS","UTF-8")
"
2. Funding round details: 
"
rounds2 <- read.csv("rounds2.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = TRUE)
rounds2[,sapply(rounds2,is.character)] <- sapply(
  rounds2[,sapply(rounds2,is.character)],
  iconv, "ISO-8859-1","Shift_JIS","UTF-8")
"
3. Sector Classification:
"
mapping <- read.csv("mapping.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = TRUE,check.names=TRUE)
mapping[,sapply(mapping,is.character)] <- sapply(
  mapping[,sapply(mapping,is.character)],
  iconv, "ISO-8859-1","Shift_JIS","UTF-8")

#==============================================================================================================

#Checkpoint 1: Data Cleaning
#####################################################

#Converting to lower case for filtering the list of unique companies
rounds2$company_permalink <- str_to_lower(rounds2$company_permalink)
companies$permalink <- str_to_lower(companies$permalink)

#Unique companies in rounds2 dataframe
length(unique(rounds2$company_permalink))

#Unique companies in companies dataframe
length(unique(companies$permalink)) 

#Merging the round2 and companies dataframe
master_frame<-merge(rounds2,companies,by.x = "company_permalink",by.y = "permalink", all.x = T)

#How many observations are present in master_frame
nrow(master_frame)


#Checkpoint 2: Funding Type Analysis
#####################################################

#Table 2.1:Average investment amount for each of the four funding types (venture, angel, seed, and private equity)

#Grouping the data by funding round type
funding_round_group <- group_by(master_frame,funding_round_type)
grouped_avg_amount <- summarise(funding_round_group,mean(raised_amount_usd,na.rm = T))

"
Considering that Spark Funds wants to invest between 5 to 15 million USD per investment round, 
which investment type is the most suitable for it?
"
subset(aggregate(raised_amount_usd~funding_round_type, 
                 data=subset(master_frame,
                             funding_round_type=='venture'|
                               funding_round_type=='angel'|
                               funding_round_type=='private_equity'|
                               funding_round_type=='seed'
                 )
                 ,mean),
       raised_amount_usd>5000000
       &raised_amount_usd<15000000)  #venture


#Checkpoint 3: Country Analysis
######################################################################

#Top9 with the top nine countries (based on the total investment amount each country has received)
top9 <-head(arrange(aggregate(raised_amount_usd~country_code, 
                              data=subset(master_frame,funding_round_type=='venture' & country_code != ""),sum),
                    desc(raised_amount_usd)),9)

#Checkpoint 4: Sector Analysis 1
######################################################################
#Extracting the primary sector of each category list from the category_list column
master_frame <- separate(master_frame, category_list, into=c("primary_sector"), sep = "\\|",extra = "drop",remove=FALSE,convert = FALSE)

#Converting columns that are not variable to find out the main_sector
#removing the blanks sector from the mapping dataframe as it is not considered as sector
mapping <- mapping[,-3]

#consolidating the required data from mapping data file
mapping<-gather(mapping, main_sector, sector_val,2:9)
mapping <- mapping[!(mapping$sector_val == 0), ]

#Cleaning the category_list data where we have 0 instead of na/Na like A0lytics instead of Analytics etc.
mapping <- mapping %>%
  mutate(category_list = str_replace(category_list, "^0", "Na"),
         category_list = str_replace(category_list, "[0]+", "na"),
         category_list = str_replace(category_list, "2.na", "2.0"))

#Merged master_frame with each primary sector mapped to its main sector 
master_frame<-merge(master_frame,mapping,by.x = "primary_sector",by.y = "category_list", all.x = T)


#Checkpoint 5: Sector Analysis 2
######################################################################

D1 <- merge(subset(master_frame,funding_round_type == 'venture'
                   & country_code == 'USA'
                   & main_sector!= 'Blanks'
                   & main_sector!='NA'
                   &raised_amount_usd >= 5000000 
                   &raised_amount_usd <= 15000000),aggregate(cbind(Total_number_of_investments=sector_val,Total_raised_usd=raised_amount_usd)~main_sector, 
                                                             data=subset(master_frame,funding_round_type == 'venture'
                                                                         & country_code == 'USA'
                                                                         & main_sector!= 'Blanks'
                                                                         & main_sector!='NA'
                                                                         &raised_amount_usd >= 5000000 
                                                                         &raised_amount_usd <= 15000000),sum),by = "main_sector",all.x=T)

D2 <- merge(subset(master_frame,funding_round_type == 'venture'
                   & country_code == 'GBR'
                   & main_sector!= 'Blanks'
                   & main_sector!='NA'
                   &raised_amount_usd >= 5000000 
                   &raised_amount_usd <= 15000000),aggregate(cbind(Total_number_of_investments=sector_val,Total_raised_usd=raised_amount_usd)~main_sector, 
                                                             data=subset(master_frame,funding_round_type == 'venture'
                                                                         & country_code == 'GBR'
                                                                         & main_sector!= 'Blanks'
                                                                         & main_sector!='NA'
                                                                         &raised_amount_usd >= 5000000 
                                                                         &raised_amount_usd <= 15000000),sum),by = "main_sector",all.x=T)

D3 <- merge(subset(master_frame,funding_round_type == 'venture'
                   & country_code == 'IND'
                   & main_sector!= 'Blanks'
                   & main_sector!='NA'
                   &raised_amount_usd >= 5000000 
                   &raised_amount_usd <= 15000000),aggregate(cbind(Total_number_of_investments=sector_val,Total_raised_usd=raised_amount_usd)~main_sector, 
                                                             data=subset(master_frame,funding_round_type == 'venture'
                                                                         & country_code == 'IND'
                                                                         & main_sector!= 'Blanks'
                                                                         & main_sector!='NA'
                                                                         &raised_amount_usd >= 5000000 
                                                                         &raised_amount_usd <= 15000000),sum),by = "main_sector",all.x=T)

##Table 5.1 : Sector-wise Investment Analysis
#==============================================================================================================

##1. Total number of investments (count)
sum(D1$sector_val,na.rm=TRUE)#Country 1
sum(D2$sector_val,na.rm=TRUE)#Country 2
sum(D3$sector_val,na.rm=TRUE)#Country 3

## 2. Total amount of investment (USD)
sum(unique(D1$Total_raised_usd),na.rm=TRUE)#Country 1
sum(unique(D2$Total_raised_usd),na.rm=TRUE)#Country 2
sum(unique(D3$Total_raised_usd),na.rm=TRUE)#Country 3

## 3. Top sector (based on count of investments)
#Country 1
D1[which.max(D1$Total_number_of_investments == sort(unique(D1$Total_number_of_investments),decreasing= T)[1]),1]
#Country 2
D2[which.max(D2$Total_number_of_investments == sort(unique(D2$Total_number_of_investments),decreasing= T)[1]),1]
#Country 3
D3[which.max(D3$Total_number_of_investments == sort(unique(D3$Total_number_of_investments),decreasing= T)[1]),1]

## 4. Second-best sector (based on count of investments)
#Country 1
D1[which.max(D1$Total_number_of_investments == sort(unique(D1$Total_number_of_investments),decreasing= T)[2]),1]
#Country 2
D2[which.max(D2$Total_number_of_investments == sort(unique(D2$Total_number_of_investments),decreasing= T)[2]),1]
#Country 3
D3[which.max(D3$Total_number_of_investments == sort(unique(D3$Total_number_of_investments),decreasing= T)[2]),1]

## 5. Third-best sector (based on count of investments)
#Country 1
D1[which.max(D1$Total_number_of_investments == sort(unique(D1$Total_number_of_investments),decreasing= T)[3]),1]
#Country 2
D2[which.max(D2$Total_number_of_investments == sort(unique(D2$Total_number_of_investments),decreasing= T)[3]),1]
#Country 3
D3[which.max(D3$Total_number_of_investments == sort(unique(D3$Total_number_of_investments),decreasing= T)[3]),1]

## 6. Number of investments in the top sector (refer to point 3)
#Country 1
D1[which.max(D1$Total_number_of_investments == sort(unique(D1$Total_number_of_investments),decreasing= T)[1]),19]
#Country 2
D2[which.max(D2$Total_number_of_investments == sort(unique(D2$Total_number_of_investments),decreasing= T)[1]),19]
#Country 3
D3[which.max(D3$Total_number_of_investments == sort(unique(D3$Total_number_of_investments),decreasing= T)[1]),19]

## 7. Number of investments in the second-best sector (refer to point 4)
#Country 1
D1[which.max(D1$Total_number_of_investments == sort(unique(D1$Total_number_of_investments),decreasing= T)[2]),19]
#Country 2
D2[which.max(D2$Total_number_of_investments == sort(unique(D2$Total_number_of_investments),decreasing= T)[2]),19]
#Country 3
D3[which.max(D3$Total_number_of_investments == sort(unique(D3$Total_number_of_investments),decreasing= T)[2]),19]

## 8. Number of investments in the third-best sector (refer to point 5)
#Country 1
D1[which.max(D1$Total_number_of_investments == sort(unique(D1$Total_number_of_investments),decreasing= T)[3]),19]
#Country 2
D2[which.max(D2$Total_number_of_investments == sort(unique(D2$Total_number_of_investments),decreasing= T)[3]),19]
#Country 3
D3[which.max(D3$Total_number_of_investments == sort(unique(D3$Total_number_of_investments),decreasing= T)[3]),19]

## 9. For the top sector count-wise (point 3), which company received the highest investment?

#Country wise top 3 sectors stored D1_Top_Sectors,D2_Top_Sectors,D3_Top_Sectors
D1_top_Sector<-head(arrange(aggregate(sector_val~main_sector,data=D1,sum),desc(sector_val)),3)
D2_top_Sector<-head(arrange(aggregate(sector_val~main_sector,data=D2,sum),desc(sector_val)),3)
D3_top_Sector<-head(arrange(aggregate(sector_val~main_sector,data=D3,sum),desc(sector_val)),3)

#subseting the data from D1 by top sector, finding the total amount rasied in each company and then finding the highly invested company
#Country 1
c(head(arrange(aggregate(raised_amount_usd~name,
                         data=subset(D1,main_sector==D1_top_Sector[1,1])
                         ,sum),desc(raised_amount_usd)),1))[1]
#Country 2
c(head(arrange(aggregate(raised_amount_usd~name,
                         data=subset(D2,main_sector==D2_top_Sector[1,1])
                         ,sum),desc(raised_amount_usd)),1))[1]
#Country 3
c(head(arrange(aggregate(raised_amount_usd~name,
                         data=subset(D3,main_sector==D3_top_Sector[1,1])
                         ,sum),desc(raised_amount_usd)),1))[1]

##  10. For the second-best sector count-wise (point 4), which company received the highest investment?
#subseting the data from D1 by second top sector, finding the total amount rasied in each company and then finding the highly invested company
#Country 1
c(head(arrange(aggregate(raised_amount_usd~name,
                         data=subset(D1,main_sector==D1_top_Sector[2,1])
                         ,sum),desc(raised_amount_usd)),1))[1]
#Country 2
c(head(arrange(aggregate(raised_amount_usd~name,
                         data=subset(D2,main_sector==D2_top_Sector[2,1])
                         ,sum),desc(raised_amount_usd)),1))[1]
#Country 3
c(head(arrange(aggregate(raised_amount_usd~name,
                         data=subset(D3,main_sector==D3_top_Sector[2,1])
                         ,sum),desc(raised_amount_usd)),1))[1]