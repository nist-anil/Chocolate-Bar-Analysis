library(tidyverse)
library(dplyr)
library(ggplot2)

chocolate_project<- read.csv("flavors_of_cacao.csv")
colnames(chocolate_project)
head(chocolate_project)
View(head(chocolate_project))
str(chocolate_project)

# Clean the column name
names(chocolate_project) <- gsub(x = names(chocolate_project), pattern = "\\.", replacement = "_") 
str(chocolate_project)
View(head(chocolate_project))

# Rename 2 column names
colnames(chocolate_project)[1]<- "Company_name"
colnames(chocolate_project)[2]<- "Bean_Origin"
View(head(chocolate_project))

# Find any null value in dataset
sapply(chocolate_project, function(x) sum(is.na(x)))
summary(chocolate_project)

# Find the unquie value in a column
table(chocolate_project$Bean_Type)

# Convert % into decimal 
chocolate_project$Cocoa_Percent <- as.numeric(sub("%", "",chocolate_project$Cocoa_Percent,fixed=TRUE))/100
View(head(chocolate_project))

# Cocoa Percentage patterns over the years

chocolate_review_date<-chocolate_project %>% 
  group_by(Review_Date) %>% 
  summarise(Cocoa_Percent = mean(Cocoa_Percent))

# chocolate_review_date

ggplot(data=chocolate_review_date, mapping=aes(x=Review_Date, y=Cocoa_Percent)) +
  geom_line( color="red")+
  scale_x_continuous(breaks = seq(2006, 2017, by = 1))+
  xlab("Date of Review") +
  ylab("Average Cocoa Percentage") +
  ggtitle("Cocoa Percentage patterns over the years")

# Rating Patterns over the year

rating_review_date<- chocolate_project %>% 
  group_by(Review_Date) %>% 
  summarise(Rating = mean(Rating))
rating_review_date

ggplot(data=rating_review_date, mapping=aes(x=Review_Date, y=Rating))+
  geom_line(color="blue")+
  scale_x_continuous(breaks = seq(2006, 2017, by = 1))+
  xlab("Date of Review")+
  ylab("Average Rating")+
  ggtitle("Average Rating over the years")

# Top 5 Companies in terms of Chocolate Bars"
top5_company <- chocolate_project %>%
  count(Company_name, sort = TRUE) %>% 
  slice(1:5)
top5_company

ggplot(data=top5_company, aes(x= reorder(Company_name, -n),y=n))+
  geom_bar(stat="identity", fill="blue")+
  geom_text(aes(label = n), vjust = -0.2, size = 3,position = position_dodge(0.9))
labs(x="Chocolate Company", y="Number of Bars", title="Top 5 Companies in terms of Chocolate Bars")

# Distribution of Chocolate Bars
company_count_chocolate_bars<-chocolate_project %>%
  group_by(Company_name) %>% 
  count(Company_name, sort = TRUE)
company_count_chocolate_bars

ggplot(data=company_count_chocolate_bars, aes(x= company_count_chocolate_bars$n))+
  geom_bar(stat="count")+
  scale_y_continuous(breaks = seq(0, 120, by = 20))+
  scale_x_discrete(limits = company_count_chocolate_bars$n, breaks = company_count_chocolate_bars$n)+
  labs(x="Count of chocolate bars", y="Number of Companies", title="Distribution of Chocolate Bars")

# Top 5 companies in terms of average ratings

average_rating_company <- aggregate(Rating ~ Company_name, data = chocolate_project, FUN = mean)
top5_average_rating_company<- head(average_rating_company[order(-average_rating_company$Rating),],5)
top5_average_rating_company

ggplot(data=top5_average_rating_company, aes(x= reorder(Company_name, -Rating),y=Rating))+
  geom_bar(stat="identity", fill="purple")+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  geom_text(aes(label = Rating), vjust = -0.2, size = 3,position = position_dodge(0.9))+
  labs(x="Average Rating", y="Chocolate Company", title="Top 5 Companies in terms of Average Ratings")

# Top 5 companies in terms of average Cocoa Percentage

average_cocoa_company <- aggregate(Cocoa_Percent ~ Company_name, data = chocolate_project, FUN = mean)
top5_average_cocoa_company<- head(average_cocoa_company[order(-average_cocoa_company$Cocoa_Percent),],5)
top5_average_cocoa_company

ggplot(data=top5_average_cocoa_company, aes(x= reorder(Company_name, -Cocoa_Percent),y=Cocoa_Percent))+
  geom_bar(stat="identity", fill="green")+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  geom_text(aes(label = Cocoa_Percent), vjust = -0.2, size = 3,position = position_dodge(0.9))+
  labs(x="Average Cocoa Percentage", y="Chocolate Company", title="Top 5 Companies in terms of Average Cocoa Percentage")

### Where does Soma get it's beans from ?
# Select Company_name & Broad_Bean_origin from dataset

company_Bean_origin<-select(chocolate_project, Company_name, Broad_Bean_Origin)

# Filter Soma, groupby broad_bean_origin, sort and select top 5

top5_soma_bean_origin<- filter(company_Bean_origin, Company_name == "Soma") %>% 
  group_by(Broad_Bean_Origin) %>% 
  tally(sort = T) %>%
  arrange(desc(n)) %>% slice(1:5)

top5_soma_bean_origin

ggplot(data=top5_soma_bean_origin, aes(x= reorder( Broad_Bean_Origin, -n),y=n))+
  geom_bar(stat="identity", fill="orange")+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  geom_text(aes(label = n), vjust = -0.2, size = 3,position = position_dodge(0.9))+
  labs(x="Broad Bean Origin", y="Number of Chocolate Bars", title="Where does Soma get it's beans from?")

### How are ratings of Chocolate bars by Soma ?

company_name_soma<-filter(chocolate_project, Company_name == "Soma")

ggplot(company_name_soma, aes(x=Rating)) + 
  geom_density(color="darkblue", fill="lightblue")+
  geom_vline(aes(xintercept=mean(Rating)),
             color="red", linetype="dashed", size=.5)+
  labs(x="Rating of the Chocolate Bar", y="Proportion of Chocolate Bars", title="Ratings of Chocolate produced by Soma")

### Soma's performance over the years
soma_performance<- aggregate(Rating ~ Review_Date, data = company_name_soma, FUN = mean) 
soma_performance 

ggplot(data=soma_performance, mapping=aes(x=Review_Date, y=Rating))+
  geom_line(color="green")+
  scale_x_continuous(breaks = seq(2009, 2016, by = 1))+
  scale_y_continuous(breaks = seq(3.40, 3.75, by = .05))+
  xlab("Date of Review")+
  ylab("Average Rating")+
  ggtitle("Soma's Average Rating over the years")

# Soma's Percentage of Cocoa over the years

soma_performance_percentage_cocoa<-aggregate(Cocoa_Percent ~ Review_Date, data = company_name_soma, FUN = mean) 
soma_performance_percentage_cocoa

ggplot(data=soma_performance_percentage_cocoa, mapping=aes(x=Review_Date, y=Cocoa_Percent))+
  geom_line(color="brown")+
  scale_x_continuous(breaks = seq(2009, 2016, by = 1))+
  scale_y_continuous(breaks = seq(.690, .725, by = .005))+
  xlab("Date of Review")+
  ylab("Percentage of Cocoa")+
  ggtitle("Soma's Percentage of Cocoa over the years")

# Cocoa percent and choco bars

cocoa_percentage_chocolate_bars<- count(chocolate_project, Cocoa_Percent,sort = TRUE) %>% 
  slice(1:10)
cocoa_percentage_chocolate_bars

cocoa_percentage_chocolate_bars$Cocoa_Percent <- factor(cocoa_percentage_chocolate_bars$Cocoa_Percent, levels = cocoa_percentage_chocolate_bars$Cocoa_Percent)

ggplot(data=cocoa_percentage_chocolate_bars,aes(x= Cocoa_Percent, y=n))+
  geom_bar(stat="identity", fill="brown")+
  scale_x_discrete(limits=cocoa_percentage_chocolate_bars$Cocoa_Percent)+
  labs(x="Percentage of Cocoa", y="Number of Chocolate Bars", title="Cocoa percent and choco bars")

# Cocoa Percent Vs Rating
ggplot(chocolate_project, aes(x=Cocoa_Percent, y=Rating)) + 
  geom_point(aes(colour = "red", size=2))+
  scale_x_continuous(breaks = seq(0.4, 1.0, by = 0.1))+
  scale_y_continuous(breaks = seq(1, 5, by = .5))+
  xlab("Percentage of Cocoa")+
  ylab("Rating of chocolate bar")+
  ggtitle("Cocoa Percent Vs Rating")

# Top Chocolate Producing Countries in the World

top10_chocolate_producing_Country<-count(chocolate_project, Company_Location,sort = TRUE) %>% 
  slice(1:10)
top10_chocolate_producing_Country

top10_chocolate_producing_Country %>% 
  ggplot(aes(x=reorder(Company_Location,-n),y=n)) +
  geom_point(size = 3, colour = "red") + 
  geom_segment( aes(x=Company_Location, xend=Company_Location, y=0, yend=n), colour = "blue")+
  coord_flip()+
  labs(x= "Number of chocolate bars", y="Country", title = "Top Chocolate Producing Countries in the World")

# Where are the Best Cocoa Beans grown?

## Top 5 countries producing most number of satisfactory rating chocolate Beans
satisfactory_rating_bean_origin<- filter(chocolate_project, Rating >= 3) %>% 
  group_by(Broad_Bean_Origin) %>% 
  tally(sort = T) %>%
  arrange(desc(n)) %>% slice(1:5)

satisfactory_rating_bean_origin

satisfactory_rating_bean_origin %>% 
  ggplot(aes(reorder(x=Broad_Bean_Origin,-n),y=n))+
  geom_bar(stat = "identity", fill="green")+
  geom_text(aes(label = n), vjust = -0.2, size = 3,position = position_dodge(0.9))+
  labs(x="Country", y="Number of Chocolate Bars", title = "Top 5 Broad origins of the Chocolate Beans with a Rating above 3.0")

## Top 5 countries producing most number of best rating chocolate Beans

best_rating_bean_origin<- filter(chocolate_project, Rating >= 4) %>% 
  group_by(Broad_Bean_Origin) %>% 
  tally(sort = T) %>%
  arrange(desc(n)) %>% slice(1:5)

best_rating_bean_origin

best_rating_bean_origin %>% 
  ggplot(aes(reorder(x=Broad_Bean_Origin,-n),y=n))+
  geom_bar(stat = "identity", fill="brown")+
  geom_text(aes(label = n), vjust = -0.2, size = 3,position = position_dodge(0.9))+
  labs(x="Country", y="Number of Chocolate Bars", title = "Top 5 Broad origins of the Chocolate Beans with a Rating above 4.0")

# Top Chocolate Producing Countries in the World (Ratings above 4.0)

top10_best_rating_company_location<- filter(chocolate_project, Rating >= 4) %>% 
  group_by(Company_Location) %>% 
  tally(sort = T) %>%
  arrange(desc(n)) %>% slice(1:10)

top10_best_rating_company_location


top10_best_rating_company_location %>% 
  ggplot(aes(x=reorder(Company_Location,-n),y=n)) +
  geom_point(size = 3, colour = "red") + 
  geom_segment( aes(x=Company_Location, xend=Company_Location, y=0, yend=n), colour = "blue")+
  coord_flip()+
  labs(x= "Number of chocolate bars", y="Country", title = "Top 10 Chocolate Producing Countries in the World with best chcocolate")

# The counts of each rating

agg_persen <- aggregate(Cocoa_Percent ~ Broad_Bean_Origin, data = chocolate_project, FUN = count)
head(agg_persen[order(-agg_persen$Cocoa_Percent),],5)

count(chocolate_project, Broad_Bean_Origin,sort = TRUE) %>% 
  slice(1:5)

rating_count<-count(chocolate_project, Rating,sort = TRUE)
rating_count

rating_count %>% 
  ggplot(aes(x=Rating, y = n)) + 
  geom_bar(stat = "identity", fill="red")+
  geom_text(aes(label = n), vjust = -0.2, size = 3,position = position_dodge(0.9))+
  labs(x="Rating of chocolate bar", y="Number of Chocolate Bars", title = "The counts of each rating")

# Categorizing Chocolate based on Ratings

rating_pie<-chocolate_project %>% 
  select(Rating) %>% 
  mutate(label_names = case_when(Rating < 3.0 ~ "unsatisfactory",
                                 Rating < 4.0 & Rating >= 3.0 ~ "satisfactory",
                                 Rating >= 4.0 ~ "premium")) %>% 
  mutate(count = n()) %>% 
  select(label_names,count) 

rating_count<- count(rating_pie, label_names,sort = TRUE)

rating_count_percent<- rating_count %>% 
  mutate(percent = n / sum(n) * 100) %>% 
  mutate_if(is.numeric, round, 1)

rating_count_percent

ggplot(rating_count_percent, aes(x="", y=percent, fill=label_names))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0)+
  geom_text(aes(label = paste0(percent, "%")), position = position_stack( vjust = 0.6))+
  labs(title="Ratings wise Category")+
  theme_void()






  
