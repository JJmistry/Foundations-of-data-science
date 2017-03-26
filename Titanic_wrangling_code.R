
# Load csv file with data into RStudio.
> titanic_original <- read_csv("~/MOOCs/Springboard/titanic_original.csv")
Parsed with column specification:
  cols(
    pclass = col_integer(),
    survived = col_integer(),
    name = col_character(),
    sex = col_character(),
    age = col_double(),
    sibsp = col_integer(),
    parch = col_integer(),
    ticket = col_character(),
    fare = col_double(),
    cabin = col_character(),
    embarked = col_character(),
    boat = col_character(),
    body = col_integer(),
    home.dest = col_character()

# Examine data structure in R
  
> View(Titanic)

#Load into data frame.

> Titanic <- data.frame(titanic_original)

#Replace all NA values in port of embarkation to "S" to represent Southampton.

> Titanic$embarked [is.na(Titanic$embarked)] <- "S"

#Calculate mean age.

> mean_age <- mean(Titanic$age, na.rm = TRUE)

#Insert mean age into all age values which are marked NA.

> Titanic$age [is.na(Titanic$age)] <- mean_age

# Create a field to show if a passenger does or doesnt have a cabin number.
> Titanic$has_cabin_number <- ifelse(Titanic$cabin == "NA",0,1)
> Titanic$has_cabin_number[is.na(Titanic$has_cabin_number)] <- 0

#Write file to csv.

> write.csv(Titanic, file ="titanic_clean.csv")