# R code used for London House Price wrangling and analysis.

> library(dplyr)
# subset for only London properties
> london.hp <- subset(land.registry, District = "Greater London")
#Subset for London properties sold since 1 Jan 16.
> london16 <- subset(london.hp, london.hp$`Date of Transfer` > 2015-12-31)

# Changed classes of various columns and renamed for ease.
> class(london16$`Price (£)`)
[1] "character"
> london16$`Price (£)`<- as.numeric(london16$`Price (£)`)
> class(london16$`Price (£)`)
[1] "numeric"
> class(london16$`Property Type`)
[1] "character"
> london16$`Property Type` <-as.factor(london16$`Property Type`)
> london16$District <-as.factor(london16$District)

> colnames(london16)[2] <- "Price"
> colnames(london16)[5] <- "Lease"
> colnames(london16)[10] <- "Town"
# backed up data in CSV
> write.csv(london16, "london16")


# Reduced 115k addresses to 60k postcodes to reduce number of gmaps queries required.
postcodes <- unique(london16[,1])
london.final <- unique(london16)

london.final$District <- tolower(london.final$District)

# take a random sample of 10k postcodes to query via Gmaps.
london1 <- sample(postcodes, 10000)

# Exploratory data analysis.
#histogram of numbers of properties sold in each price bracket.
  > require(scales)
  > ggplot(london16, aes( x = london16$pricebracket))+
    +     geom_histogram(stat="count",  aes(fill = london16$pricebracket)) +
    +     xlab("Price") +
    +     ylab("Number of Properties") +
    +     theme(axis.text.x = element_text(angle =90)) +
    +     theme(legend.position="none")+
    +     ggtitle("Number of  London properties sold in each price bracket \n in 2016")
  Warning: Ignoring unknown parameters: binwidth, bins, pad
  
  > london16 <-group_by_(london16$District)
  > salesbydistrict <-summary(london16$District)
  > class(salesbydistrict)
  [1] "integer"
  > salesbydistrict <-as.data.frame(salesbydistrict)
  > colnames(salesbydistrict)[1] <- "Sales"
 
  # converted rownames into frist column
  
  > salesbydistrict<- tibble::rownames_to_column(salesbydistrict)
  > colnames(salesbydistrict)[1] <- "District"
  
  # Exploratory data plot, sales by district.
  > ggplot(salesbydistrict, aes(x = District, y = Sales, fill = District)) +
    +     geom_bar(stat = "identity") +
    +     theme(axis.text.x = element_text(angle = 90)) +
    +     theme(legend.position = "none")

# created dataframe with mean prices for each District.
  > Aveprice<-london16 %>% group_by(District) %>% summarise(mean=mean(Price), median = median(Price), min =min(Price), max = max(Price), std = sd(Price))
  > write.csv(Aveprice,"Average_price_district")
 
#Gmaps script to collect journey time by public transport to WC1 6BT.  It must be done by batch as there is a cap on the maximum number of transactions the API will accept.
  > tempt <- gmapsdistance(set.api.key("AIzaSyDFebeOppqSyU GSut_eGs8JcjdsgPBo8zk"),
                             +                          origin = london2[1501:1700,1],
                             +                          destination = "WC1E6BT",
                             +                          mode = "transit",
                             +                          dep_date = "2017-05-02",
                             +                          dep_time = "09:00:00",
                             +                          combinations = "all")
 
  # function required to convert output of gmaps API into a useful dataframe
  res <- data.frame(origin = london1[, 1],
                  desination = 'WC1E-6BT',
  do.call(data.frame, lapply(tempt, function(x) x[, 2])))

  # Binding outputs into a single dataframe.
   results1.12 <- rbind(res, res2, res3, res4, res5, res6, res7, res8, res9, res10, res11, res12)
 
# input crime data and create 'crime rate' dependent on population 
   library(readr)
   Crimestats_popn <- read_csv("~/MOOCs/Springboard/Capstone Project/Data sets raw/Crimestats_popn.csv", 
                                +     skip = 1)
  Parsed with column specification:
    cols(
      London = col_character(),
      Population = col_integer(),
      `Total Crime` = col_double()
    )
   View(Crimestats_popn)
   colnames(Crimestats_popn)[1] <- "NAME"
   Crimestats_popn$Rate <- Crimestats_popn$`Total Crime`/Crimestats_popn$Population
   Crimestats_popn$Rate <- Crimestats_popn$Rate*100000
 
 # Input GCSE scores 
   library(readr)
   GCSE_scores <- read_csv("~/MOOCs/Springboard/Capstone Project/Data sets raw/GCSE_scores.csv")
 Warning message:
    Missing column names filled in: 'X1' [1] 

   GCSE_scores$X1 <- tolower(GCSE_scores$X1)
   colnames(GCSE_scores)[1] <-"NAME"
 
 # Create master list by district.
 library(dplyr)
 master <- left_join(GCSE_scores, Crimestats_popn, by = "NAME")
 
 london.final$Postcode <- gsub(" ", "", london.final$Postcode)
 colnames(postcodetime)[1] <- "Postcode"

 #  Added journey time and distance to price overall dataframe. 
london.final <- left_join(london.final, postcodetime, by "Postcode")

# Removed all prices which did not have a time value
x <- london.final[!is.na(london.final$Time),]

# Removed all properties which were "Other" type.
x <- x[!(x$`Property Type` == "O"),]

# Adjusted columns ready to join
colnames(master)[1] <- "District"
master[33,1] <- "city of westminster"

# Added crime and education to prices dataframe
x <- left_join(x, master, by = "District")

# Tidied up column names
 colnames(x)[2] <- "Price"
 colnames(x)[5] <- "lease"
 colnames(x)[10] <- "town"
 colnames(x)[17] <- "pupils.district"
 colnames(x)[18] <- "ks4gcse5a-c" # % of students receiving 5+ A-C GCSEs
 colnames(x)[19] <- "ks4gcse5a-g" # % of students receiving 5+ A-g GCSEs
 colnames(x)[24] <- "avegcse_ptscore" # average GCSE point score per pupil

london.complete <- x

prop.model.best <- lm(Price~ avegcse_ptscore + Time + Total.Crime + Population, london.complete)
summary(prop.model.best)