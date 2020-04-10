
# #Ensure that the data 'censusdata' is in your working directory
#Setting the working directory
setwd('C:/Users/Victor/Desktop/Cancer Registry')
#installing packages
install.packages('dplyr')
#loading library
library(dplyr)
#getting help on a package
help(package='dplyr')
##########################################################################################
#Importing Excel Files
###########################################################################################
#Importing CSV files
excelfile1 <- read.csv('Cancer_Data.csv')

#Using library 'readxl'
install.packages('readxl')
library(readxl)
help(package='readxl')
excelfile2 <- read_xlsx('Cancer_Data.xlsx') #by default imports sheet 1
excelfile2 <- read_xlsx('Cancer_Data.xlsx', sheet = 1) #specify to import sheet 1
excelfile2 <- read_xlsx('Cancer_Data.xlsx', sheet = 2) #specify to import sheet 2
head(excelfile2)
excelfile3 <- read_xls('Cancer_Data.xls') # to read XLS
############################################################################################
# Exporting the files
########################################################################################
write.csv(excelfile2, 'Excel_Output1.csv',row.names = FALSE) #Exporting to CSV
#Exporting to xlsx
install.packages('writexl')
library(writexl)
help(package='writexl')
write_xlsx(excelfile1, 'Excel_Ouptut2.xlsx') #Exporting one sheet
sheets <- list("Males" = excelfile1, "Females" = excelfile2, "Both" = excelfile3) #assume excelfile1 , excelfile2 and excelfile3 are data frames
write_xlsx(sheets, "Excel_Ouptut2.xlsx") #Exporting multiple sheets
###############################################################################
###############################################################################
#Installing required packages #Ignore this step if you already have them installes
install.packages('stringr')
install.packages('reshape')
install.packages('tidyr')
install.packages('readxl')
install.packages('dplyr')
install.packages(readxl)
install.packages(lubridate)
install.packages(stringi)
#For Text Mining
install.packages('NLP')
install.packages('RColorBrewer')
install.packages('tm')
install.packages('SnowballC')
install.packages('wordcloud')
install.packages('RColorBrewer')
#For the Map and Dot Pot
install.packages(leaflet)
install.packages(ggmap)
####Required Packages
#For Importing
library(readxl)
#For data cleaning & Manupulation
library(dplyr)
library(stringr)
library(reshape)
library(tidyr)
library(readxl)
library(lubridate)
library(stringi)
#For Text Mining
library('NLP')
library('RColorBrewer')
library('tm')
library('SnowballC')
library('wordcloud')
library('RColorBrewer')
#For the Map and Dot Pot
library(leaflet)
library(ggmap)

#############################################################################################################################
#PART I
#############################################################################################################################

# 1. Why the data is not in tidy format
# Answer: The data has the variables as rows and they are repeated for every town, it needs to be cleaned intoa tidy format.

#2. 
#Importing 2011 data into R
setwd()
censusdata_unclean <- read_xlsx('C:/Users/Administrator.Cregistry-PC/Desktop/Census assignment/censusdata.xlsx', sheet = 1, col_names = F) # Importing using readxl
censusdata_unclean <- as.data.frame(censusdata_unclean) #Declaring it to be a data frame
names(censusdata_unclean)[1:6] <- c('Details', 'Rm_details',	'Victoria',	'Percentage_1', 'Australia',	'Percentage_2')
str(censusdata_unclean) #checking the structure
censusdata_unclean <- na.omit(censusdata_unclean) #Omitting missing values
head(censusdata_unclean) #Checking the head of the data
censusdata_unclean <- separate(censusdata_unclean, Rm_details, into = c('Rm_details', 'Other'), sep = '/', fill='right' )
censusdata_unclean <- censusdata_unclean[,-3] #deleting unwanted variable 'Other'

#Function to clean the data
data_func <- function(censusdata1){
  ### functions to use ###
  `%nin%` <- Negate(`%in%`)
  ### declare details as factor
  table(censusdata_unclean$Details)
  censusdata_unclean$Details<- as.factor(censusdata_unclean$Details)
  
  ### create town variable ####
  
  for(i in 1:nrow(censusdata_unclean)){
    j <- ifelse(stringr::str_detect(censusdata_unclean$Rm_details[i], '[(]'), 0, 1)
    censusdata_unclean$Town[i]<- ifelse(stringr::str_detect(censusdata_unclean$Rm_details[i], '[(]'), censusdata_unclean$Rm_details[i], censusdata_unclean$Town[i-j] )
    
  }
  
  #### clean up the details column###
  censusdata_unclean<- censusdata_unclean[!str_detect(censusdata_unclean$Rm_details, '[(]'),]
  
  
  ### first melt it down:
  censusdata_unclean1<-censusdata_unclean %>% melt(id = c('Town', 'Details'))
  censusdata_unclean1<- unique(censusdata_unclean1)
  
  ### dcast it
  censusdata_unclean2<-censusdata_unclean1%>% reshape2::dcast (Town + variable ~ Details, value.var = 'value')
  #### clean up for the city.... victoria/Austraria
  censusdata_unclean3<- censusdata_unclean2%>%filter(variable %nin% c('Victoria', 'Australia'))
  cities<- censusdata_unclean2%>%filter(variable %in% c('Victoria', 'Australia'))
  cities<- cities[!duplicated(cities%>%select(-Town)),][1:2,]
  cities$Town<- cities$variable
  cities$variable<- 'Rm_details'
  
  ###### bind data
  censusdata_unclean4 <- rbind(censusdata_unclean3, cities)
  
  #### Final cleaning to delete unwanted variables
  censusdata_unclean5 <- censusdata_unclean4[,c(-2,-7)]%>%filter(censusdata_unclean4$variable=='Rm_details')
  ####Rearranging the variables
  censusdata <<-censusdata_unclean5[,c(1,8,2,3,4,5,9,6,7)]
}
data_func(censusdata_unclean) #Applying the function to the dataset dataset
censusdata_unclean1 <- separate(censusdata, Town, into = c('Town', 'Other'), sep = '[(]', fill='right' ) #Spliting the variables (Columns)
censusdata_2011 <- censusdata_unclean1[,-2] # Removing unwanted variables
#Changing the names of the variables
names(censusdata_2011)[1:9] <- c('region','br_count_0','br_count_1','br_count_2','br_count_3','br_count_4_or_more','br_count_unstated','av_per_dwelling','av_per_household')

#Importing 2016 data into R - 
censusdata_unclean <- read_xlsx('censusdata.xlsx', sheet = 2, col_names = F) 
censusdata_unclean <- as.data.frame(censusdata_unclean) # setting it as data frame
censusdata_unclean[1,2] <- 'Banyule (C)' #Replacing 'Banyule' because our function identifies towns by using '('
censusdata_unclean[121,2] <- 'Kingston (C)' #Replacing 'Kingston (C) (Vic)' to delete (Vic)
names(censusdata_unclean)[1:6] <- c('Details', 'Rm_details',	'Victoria',	'Percentage_1', 'Australia',	'Percentage_2')
str(censusdata_unclean) #Checking the structure of the dataset
names(censusdata_unclean) #Checking the structure of the dataset
censusdata_unclean <- na.omit(censusdata_unclean) #Omitting missing values 
censusdata_unclean <- separate(censusdata_unclean, Rm_details, into = c('Rm_details', 'Other'), sep = '/', fill='right' )#Spliting the variables (Columns)
censusdata_unclean <- censusdata_unclean[,-3] # Removing unwanted variables
data_func(censusdata_unclean) #Applying the function to the dataset dataset
censusdata_unclean1 <- separate(censusdata, Town, into = c('Town', 'Other'), sep = '[(]', fill='right' )#Spliting the variables (Columns)
censusdata_2016 <- censusdata_unclean1[,-2]# Removing unwanted variables
#Changing the names of the variables
names(censusdata_2016)[1:9] <- c('region','br_count_0','br_count_1','br_count_2','br_count_3','br_count_4_or_more','br_count_unstated','av_per_dwelling','av_per_household')

#Adding variable 'year' to the data sets
censusdata_2016$year <- rep(2016,32)
censusdata_2016 <<-censusdata_2016[,c(1,10,2,3,4,5,6,7,8,9)] #Rearranging the variables
censusdata_2011$year <- rep(2011,32)
censusdata_2011 <<-censusdata_2011[,c(1,10,2,3,4,5,6,7,8,9)] #Rearranging the variables

#Merging the 2 data sets
censusdata_F <- rbind(censusdata_2011,censusdata_2016) # binding the two data sets
censusdata_F <- censusdata_F[order(censusdata_F$region),] #Ordering by region
censusdata_F1 <- filter(censusdata_F, region !='Victoria' & region !='Australia') #Subsetting 
censusdata_F2 <- filter(censusdata_F, region =='Victoria'| region =='Australia') #Subsetting to delete Australia & Victoria
censusdata_clean <- rbind(censusdata_F1,censusdata_F2) # Binding to get the final output

#Chaning the structure of the data
sapply(censusdata_clean, class) #checking the structure of the data
str(censusdata_clean)
id <- 3:10
censusdata_clean[id] = data.matrix(censusdata_clean[id])
censusdata_clean = mutate_each(censusdata_clean, funs(as.numeric), id)
censusdata_clean$year <- as.factor(censusdata_clean$year)

#Final output in CSV format
write.csv(censusdata_clean, 'censusdata_clean.csv',row.names = FALSE) #Exporting to CSV 

# 3. region(s) (ignoring Victoria and Australia) had the largest increase in the number of occupied dwellings with 3 or more bedrooms between 2011 and 2016
censusdata_clean_ <- mutate(censusdata_clean, br_count_3_or_more = br_count_3 + br_count_4_or_more)
censusdata_clean2011 <- filter(censusdata_clean_, year==2011)
censusdata_clean2016 <- filter(censusdata_clean_, year==2016)
both_br_count_3_or_more <- (censusdata_clean2016$br_count_3_or_more-censusdata_clean2011$br_count_3_or_more)  
data_census <- as.data.frame(cbind(censusdata_clean2016$region, both_br_count_3_or_more))
data_census$both_br_count_3_or_more <- as.numeric(as.character(data_census$both_br_count_3_or_more))
data_census[order(-data_census$both_br_count_3_or_more),] #Ordering to find out the top regions
##Answer is Wyndham

#############################################################################################################################
# PART II
#############################################################################################################################

#############################
#listings <- read.csv('http://data.insideairbnb.com/australia/vic/melbourne/2019-04-08/visualisations/listings.csv') #Reading data from website
listings <- read.csv('listings.csv') #Importing data from working directory
attach(listings) #Attaching dataset
#1. Top five neighbourhoods with the most listings
listings1 <- listings%>%select(neighbourhood,calculated_host_listings_count)%>%filter(calculated_host_listings_count >= 85) #Filtering to get top 5
listings1 <- listings1[order(-listings1$calculated_host_listings_count),] #Ordering the data
listings1 <- listings1 %>% distinct(neighbourhood, .keep_all = TRUE);listings1 #Removes duplicates and displays the results on the console

#2. listings contain the following words (upper or lower case or mixed) in the name column?
# a. Beautiful

sum(str_count(tolower(listings$name), 'beautiful'))
#Answer = 585
# b. Quiet
sum(str_count(tolower(listings$name), 'quiet'))
#Answer = 453
# c. Amazing
sum(str_count(tolower(listings$name), 'amazing'))
#Answer = 372
# d. <another adjective of your choice with at least 200 instances>
#good
sum(str_count(tolower(listings$name), 'good'))
#Answer = 59

# 3. listings that are there with last review in 2016
str(listings) #Checking the structure of the data
listings$last_review <- as.Date(listings$last_review) # Changing variable to date format
listings$last_review_year <- year(listings$last_review) # Extracting year
listings$last_review_month <-  month(listings$last_review, label = TRUE) #Extracting month
sub_listings<-filter(listings, listings$last_review_year==2016) # Filtering those of 2016
count(sub_listings,'last_review_month') # Counting the number of listings
sub_list <- sub_listings %>% count(last_review_month, sort = TRUE, name = 'Count') # Filters month by month
sub_list #displays the month by moth result

#4 new column of the table which calculates the number of ids that correspond to the given host_id. 
#Your answer will match the calculated_host_listings_count column (only use this column to check your answer).
listings_clean <- listings %>%select(id:availability_365) %>%add_count(host_id) # Adds the new column called 'n'

##############################################################################################################################
# Part III
##############################################################################################################################
# summarizing variables from dataset censusdata in part 1
sapply(censusdata_clean, class) #checking the structure of the data
str(censusdata_clean) #Checking the structure of the data
#changing the structure of the dataset
summary(censusdata_clean) #summarizing the dataset in part 1
summary(listings_clean) #summarizing the dataset in part 2

# summarizing variables from dataset listings in part 2
# Histogram showing the distribution of a variable of interest
# Variable of choice: 'av_per_dwelling' in censusdata
hist(censusdata_clean$av_per_dwelling, 
     main='av_per_dwelling', 
     xlab='Average number of bedrooms per dwelling', 
     border='blue', 
     col='green')

# A plot of one or more variables with time on the x axis (e.g. month, year or date) 
boxplot(censusdata_clean$av_per_dwelling~censusdata_clean$year,main='Average number of bedrooms per dwelling and Year',
        ylab='Average number of bedrooms per dwelling',xlab='Year',col='green')

# A word cloud of the words in the name column of the listings table. 
#First, copy the data in column 'name' and save it in a text file. Th data set here was saved as listings.txt 
# Loading the data
text <- readLines('C:/Users/Administrator.Cregistry-PC/Desktop/R-directory/listings.txt')
docs <- Corpus(VectorSource(text)) #Load the data as a corpus
inspect(docs) #Inspecting the document
#Removing abnormal characters
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, ' ', x))
docs <- tm_map(docs, toSpace, '/')
docs <- tm_map(docs, toSpace, '@')
docs <- tm_map(docs, toSpace, '\\|')
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords('english'))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c('blabla1', 'blabla2')) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

#The world cloud
set.seed(1234) #Setting seed for uniformity incase of re-running the code
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, 'Dark2'))

#   A map showing the price of listings by colour (e.g. a dot plot or heat map - you will need to use an R package that can map geospatial data) 
register_google(key = '') # Registering the API Key #Note - Here there is need to put a registered google API Key
#Ensure to enable the different types of APIs like (Geocoding,Geolocating and Maps Static APIs) to avoid errors
geocode('Melbourne') #Checking for the cordinates of Melbourne
#Creating price as a categorical variable
listings$Price_bucket <- as.factor(ifelse(listings$price<=49,'Below 50 AUD',ifelse(listings$price<=200,'Between 50 to 200 AUD','Above 200 AUD')))
sf <- geocode('Melbourne')
map <- get_map(location = sf, zoom = 14, scale = 2) #Creatig the map object
dot_plot <- ggmap(map) + geom_point(aes(longitude, latitude, color = Price_bucket), data = listings) # dot plot
#Labeling the dot plot
dot_plot_final <- dot_plot + ggtitle('Dot Plot Showing Listings by Prices in Melbourne') + xlab('Listings Longitude') + ylab('Listings Latitude') + theme(plot.title = element_text(hjust = 0.5))
dot_plot_final # Final Dot Plot

###################################################################################
#################################################################################
library(haven)
sti_data<-read_stata('C:/Users/Administrator.Cregistry-PC/Desktop/R-directory/stidata_unclean.dta')
################Subsetting data########################
Subset1 <- as.data.frame(sti_data)[c(11:15)]
Subset2 <-  as.data.frame(sti_data)[c(1:10)]
Subset3 <- as.data.frame(sti_data)[c(16:30)]
na.omit(sti_data)
################Changing Variable names##############
names(Subset2)[1:10]<-c('ID_Number','Case_Status','Date','Age','Occupation','Church','Level_of_Education','Marital_Status','Height','Sti_Status')
names(Subset1)[1:5]<-c('Burial_Society','Religious_Group','Savings_Club','Traders_Association','Age')
names(Subset3)[1:15]<-c('Group2_Category','Education_Status','Received_Funeral_Assistance','Recieved_Health_Services','Duration_Of_Illness',
                        'Give/Receive_For_Sex','Used_Condom','Done_Raw_Sex','Taken_Alcohol','HIV_Status','Living_Together','Age','Received_Credit','Give/Receive_For_Sex','Sex_Debut')
###############ommiting the null rows##########
Subset1<-na.omit(Subset1)
Subset2<-na.omit(Subset2)
Subset3<-na.omit(Subset3)
##################Showing the class of each column in the data############
sapply(Subset1, class)
sapply(Subset2, class)
sapply(Subset3, class)
##################changing numeric data type to factor#############################
names <- c(1:3,5)
Subset1[,names] <- lapply(Subset1[,names] , factor)
names <- c(2,5:8,10)
Subset2[,names] <- lapply(Subset2[,names] , factor)
names<-c(1:4,6:11,13:14)
censusdata_unclean <- separate(censusdata_unclean, Rm_details, into = c('Rm_details', 'Other'), sep = '/', fill='right' )[,names] <- lapply(Subset3[,names] , factor)
Subset1[,4] <- lapply(Subset1[,4],character)
Subset3$Duration_Of_Illness<- as.numeric(as.character(Subset3$Duration_Of_Illness))
#####################Checking for normality in R###################
attach(Subset3)
hist(Age,probability=T, main="Histogram of Age
data{Subset1}",xlab="AGE",col='Red' ,pch=15)
lines(density(Age),col=12)
##################Charts for Categorical data######################3
Categorical_data<-(Subset2)[5:8]
head(Categorical_data)
library('dplyr')
agg <- count(Categorical_data, Occupation,Church,Level_of_Education,Marital_Status)
head(agg)
library(ggplot2)
ggplot1 <- ggplot(Categorical_data) + geom_bar(aes(x = Occupation,fill=Occupation))
ggplot2 <- ggplot(Categorical_data) + geom_bar(aes(x = Church,fill=Church))
ggplot3 <- ggplot(Categorical_data) + geom_bar(aes(x = Level_of_Education,fill=Level_of_Education))
ggplot4 <- ggplot(Categorical_data) + geom_bar(aes(x = Marital_Status,fill=Marital_Status))
install.packages('ggpubr')
library('ggpubr')
ggarrange(ggplot1, ggplot2, ggplot3,ggplot4 + rremove("x.text"), labels = c("A", "B", "C","D"),ncol = 2, nrow = 2)






