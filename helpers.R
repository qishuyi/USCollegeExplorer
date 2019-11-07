drive_deauth()

# Get data files from google drive
file1 <- drive_get(as_id("1kPNCGb4dv1l4Icn7WIzeWk7cyxvyCqml"))
file2 <- drive_get(as_id("1FJBMSIU_OIG7oJTL8q2lmc2wZLwIUEGt"))
file3 <- drive_get(as_id("1O4oUCz9IpV2s1pRvCkRVGmwCaEfq3Ydq"))
file4 <- drive_get(as_id("1euy-bM2yLrnH-8Mhlg0rjnZwFL-IpXAX"))
file5 <- drive_get(as_id("1iUP5qGTBsYHmXvQARkfv8uka_4iTyBGy"))
file6 <- drive_get(as_id("1FQG9Ag4o9hYbvfOM1wISOXVOysAsqGKa"))
file7 <- drive_get(as_id("1Zv0QVmuVbRJ_0GkXEyV5rLTU53Kuoqkn"))
file8 <- drive_get(as_id("1vGuVNV_3lHEI7cs7IC5J-cktJZiOaXob"))

# Download data files
drive_download(file1, overwrite = TRUE)
drive_download(file2, overwrite = TRUE)
drive_download(file3, overwrite = TRUE)
drive_download(file4, overwrite = TRUE)
drive_download(file5, overwrite = TRUE)
drive_download(file6, overwrite = TRUE)
drive_download(file7, overwrite = TRUE)
drive_download(file8, overwrite = TRUE)

# Read in data and select certain variables
data <- read.csv("MERGED2016_17_PP.csv")
data2016 <- select (data, INSTNM, ADM_RATE, SATVRMID, SATMTMID, SATWRMID, ACTENMID, ACTMTMID, ACTWRMID, UGDS, COSTT4_A, TUITFTE, AVGFACSAL, PFTFAC, PAR_ED_PCT_1STGEN)
data2 <- read.csv("MERGED2015_16_PP.csv")
data2015 <- select (data2, INSTNM, ADM_RATE, SATVRMID, SATMTMID, SATWRMID, ACTENMID, ACTMTMID, ACTWRMID, UGDS, COSTT4_A, TUITFTE, AVGFACSAL, PFTFAC, PAR_ED_PCT_1STGEN)
data3 <- read.csv("MERGED2014_15_PP.csv")
data2014 <- select (data3, INSTNM, ADM_RATE, SATVRMID, SATMTMID, SATWRMID, ACTENMID, ACTMTMID, ACTWRMID, UGDS, COSTT4_A, TUITFTE, AVGFACSAL, PFTFAC, PAR_ED_PCT_1STGEN)
data4 <- read.csv("MERGED2013_14_PP.csv")
data2013 <- select (data4, INSTNM, ADM_RATE, SATVRMID, SATMTMID, SATWRMID, ACTENMID, ACTMTMID, ACTWRMID, UGDS, COSTT4_A, TUITFTE, AVGFACSAL, PFTFAC, PAR_ED_PCT_1STGEN)
data5 <- read.csv("MERGED2012_13_PP.csv")
data2012 <- select (data5, INSTNM, ADM_RATE, SATVRMID, SATMTMID, SATWRMID, ACTENMID, ACTMTMID, ACTWRMID, UGDS, COSTT4_A, TUITFTE, AVGFACSAL, PFTFAC, PAR_ED_PCT_1STGEN)
data6 <- read.csv("MERGED2011_12_PP.csv")
data2011 <- select (data6, INSTNM, ADM_RATE, SATVRMID, SATMTMID, SATWRMID, ACTENMID, ACTMTMID, ACTWRMID, UGDS, COSTT4_A, TUITFTE, AVGFACSAL, PFTFAC, PAR_ED_PCT_1STGEN)
data7 <- read.csv("MERGED2010_11_PP.csv")
data2010 <- select (data7, INSTNM, ADM_RATE, SATVRMID, SATMTMID, SATWRMID, ACTENMID, ACTMTMID, ACTWRMID, UGDS, COSTT4_A, TUITFTE, AVGFACSAL, PFTFAC, PAR_ED_PCT_1STGEN)
data8 <- read.csv("MERGED2009_10_PP.csv")
data2009 <- select (data8, INSTNM, ADM_RATE, SATVRMID, SATMTMID, SATWRMID, ACTENMID, ACTMTMID, ACTWRMID, UGDS, COSTT4_A, TUITFTE, AVGFACSAL, PFTFAC, PAR_ED_PCT_1STGEN)

data2016_copy <- data

# Preprocessing 2016 data for map, clustering and prediction
# Converting variables from factors to integers
data2016_copy$UGDS <- as.character(data2016_copy$UGDS)
data2016_copy$UGDS <- as.integer(data2016_copy$UGDS)

data2016_copy$REGION <- as.character(data2016_copy$REGION)
data2016_copy$REGION <- as.integer(data2016_copy$REGION)

data2016_copy$COSTT4_A <- as.character(data2016_copy$COSTT4_A)
data2016_copy$COSTT4_A <- as.integer(data2016_copy$COSTT4_A)

data2016_copy$SATVRMID <- as.character(data2016_copy$SATVRMID)
data2016_copy$SATVRMID <- as.integer(data2016_copy$SATVRMID)

data2016_copy$SATMTMID <- as.character(data2016_copy$SATMTMID)
data2016_copy$SATMTMID <- as.integer(data2016_copy$SATMTMID)

data2016_copy$SATWRMID <- as.character(data2016_copy$SATWRMID)
data2016_copy$SATWRMID <- as.integer(data2016_copy$SATWRMID)

data2016_copy$ACTENMID <- as.character(data2016_copy$ACTENMID)
data2016_copy$ACTENMID <- as.integer(data2016_copy$ACTENMID)

data2016_copy$ACTMTMID <- as.character(data2016_copy$ACTMTMID)
data2016_copy$ACTMTMID <- as.integer(data2016_copy$ACTMTMID)

data2016_copy$ACTWRMID <- as.character(data2016_copy$ACTWRMID)
data2016_copy$ACTWRMID <- as.integer(data2016_copy$ACTWRMID)

data2016_copy$LATITUDE <- as.character(data2016_copy$LATITUDE)
data2016_copy$LATITUDE <- as.integer(data2016_copy$LATITUDE)

data2016_copy$LONGITUDE <- as.character(data2016_copy$LONGITUDE)
data2016_copy$LONGITUDE <- as.integer(data2016_copy$LONGITUDE)

# Converting admission rate from factor to double
data2016_copy$ADM_RATE <- as.numeric(levels(data2016_copy$ADM_RATE)[data2016_copy$ADM_RATE])

# Setting up data for clustering:
# We Choose a few numeric variables for modeling (admission rate, SAT/ACT scores, number of undergraduates, 4-year cost)
# and a few categorical variables for filtering (religious affiliation, region, locale, control), because with too many
# colleges the clustering plot looks messy
cluster_data <- select(data2016_copy, INSTNM, CONTROL, REGION, LOCALE, RELAFFIL, ADM_RATE, SATVRMID, SATMTMID, ACTENMID, ACTMTMID, UGDS, COSTT4_A)

# Set college names to be row names
college.names <- data2016_copy$INSTNM
rownames(cluster_data) = make.names(college.names, unique=TRUE)

# Set NULL entries to N/A
is.na(cluster_data) <- cluster_data == "NULL"

# For religious affiliation, if an entry is NULL, we denote it as “no religious affiliation” with the factor “-1”
levels <- levels(cluster_data$RELAFFIL)
levels[length(levels) + 1] <- "-1"
cluster_data$RELAFFIL <- factor(cluster_data$RELAFFIL, levels = levels)
cluster_data$RELAFFIL[is.na(cluster_data$RELAFFIL)] <- "-1"

# Omit all rows with N/A entries
cluster_data <- na.omit(cluster_data)

# Variables for clustering that users can choose from
select.vars <- c("Admission Rate", "SAT Verbal Median", "SAT Maths Median", "ACT English Median", "ACT Maths Median",
                 "Enrollment of undergraduates", "Cost of Attendance")

# Setting up data for mapping
# Change NAs to 0s in the SAT/ACT columns
data2016_copy$SATVRMID[is.na(data2016_copy$SATVRMID)] <- 0
data2016_copy$SATMTMID[is.na(data2016_copy$SATMTMID)] <- 0
data2016_copy$SATWRMID[is.na(data2016_copy$SATWRMID)] <- 0
data2016_copy$ACTENMID[is.na(data2016_copy$ACTENMID)] <- 0
data2016_copy$ACTMTMID[is.na(data2016_copy$ACTMTMID)] <- 0
data2016_copy$ACTWRMID[is.na(data2016_copy$ACTWRMID)] <- 0

# Converting region from integer to factor
data2016_copy[,'REGION']<-factor(data2016_copy[,'REGION'])
# Changing the region factors to strings
levels(data2016_copy$REGION) <- c("US Service Schools","New England", "Mid East", "Great Lakes", "Plains", 
                             "Southeast","Southwest", "Rocky Mountains", "Far West", "Outlying Areas")

# Converting locale from integer to factor
data2016_copy[,'LOCALE']<-factor(data2016_copy[,'LOCALE'])
# Putting empty string for the first factor -3 (undefined), and for the last factor NULL
levels(data2016_copy$LOCALE) <- c("","City","City", "City", "Suburb", "Suburb", 
                             "Suburb","Town", "Town", "Town", "Rural", "Rural", "Rural","")

# Converting control from integer to factor
data2016_copy[,'CONTROL']<-factor(data2016_copy[,'CONTROL'])
levels(data2016_copy$CONTROL) <- c("Public", "Private nonprofit", "Private for-profit")

# Removing schools with missing information
data2016.slider <- data2016_copy[complete.cases(data2016_copy$UGDS), ] 
data2016.slider <- data2016_copy[complete.cases(data2016_copy$COSTT4_A), ] 

# Changing PAR_ED_PCT_1STGEN to double from factor, so we can use more than and less than or equal to mutate a First_gen column
data2016_copy$PAR_ED_PCT_1STGEN <- as.character(data2016_copy$PAR_ED_PCT_1STGEN)
data2016_copy$PAR_ED_PCT_1STGEN <- as.double(data2016_copy$PAR_ED_PCT_1STGEN)

data2016_copy <- mutate (data2016_copy, First_gen = case_when (  PAR_ED_PCT_1STGEN > 0.3 ~ 1,
                                                       PAR_ED_PCT_1STGEN <= 0.3 ~ 0))

# Setting up data for prediction
data_pred <- select (data2016_copy, INSTNM, CONTROL, REGION, LOCALE, SATVRMID, SATMTMID, ACTENMID, ACTMTMID, UGDS, COSTT4_A, MENONLY, WOMENONLY, First_gen)

# We let region be a numeric variables because numbers close to each other denote regions that are geographically
# close to each other. E.g. 1 (New England), 2 (Mid East), 3 (Great Lakes), 8 (Far West)
data_pred$REGION <- as.numeric(data_pred$REGION)

# We let locale be a numeric variable because numbers close to each other denote a similar type of location
# E.g. 11 (City: Large), 12 (City: Midsize), 13 (City: Small), 21 (Suburb: Large), 22(Suburb: Midsize)
data_pred$LOCALE <- as.numeric(data_pred$LOCALE)

# Turn MENONLY and WOMENONLY into integer variables
#data_pred$MENONLY <- as.character(data_pred$MENONLY)
#data_pred$MENONLY <- as.integer(data_pred$MENONLY)
#data_pred$WOMENONLY <- as.character(data_pred$WOMENONLY)
#data_pred$WOMENONLY <- as.integer(data_pred$WOMENONLY)

# Use only rows with values for all columns
data_pred <- data_pred[complete.cases(data_pred),]

# Preprocessing for historical data
# Add year column
data2016 <- mutate(data2016, "YEAR" = 2016)
data2015 <- mutate(data2015, "YEAR" = 2015)
data2014 <-mutate(data2014, "YEAR" = 2014)
data2013 <- mutate(data2013, "YEAR" = 2013)
data2012 <- mutate(data2012, "YEAR" = 2012)
data2011 <- mutate(data2011, "YEAR" = 2011)
data2010 <- mutate(data2010, "YEAR" = 2010)
data2009 <-mutate(data2009, "YEAR" = 2009)

# Combine all years together
data_all <- rbind(data2016, data2015, data2014, data2013, data2012, data2011, data2010, data2009)

# Remove multiples and filter out colleges that aren't present in all years
data_multiples <- ddply(data_all,.(data_all$INSTNM),nrow)
college8 <- filter(data_multiples, V1 == 8)

# Remove the extra row 
college8 <- select(college8, -V1)
names(college8) <- c("INSTNM")

# Join years data and college name data
data2016 <- inner_join(college8, data2016, by = "INSTNM")
data2015 <- inner_join(college8, data2015, by = "INSTNM")
data2014 <- inner_join(college8, data2014, by = "INSTNM")
data2013 <- inner_join(college8, data2013, by = "INSTNM")
data2012 <- inner_join(college8, data2012, by = "INSTNM")
data2011 <- inner_join(college8, data2011, by = "INSTNM")
data2010 <- inner_join(college8, data2010, by = "INSTNM")
data2009 <- inner_join(college8, data2009, by = "INSTNM")

# Make into one dataset
data_all <- rbind(data2016, data2015, data2014, data2013, data2012, data2011, data2010, data2009)

# Make variables integers
data_all$SATVRMID <- as.character(data_all$SATVRMID)
data_all$SATVRMID <- as.integer(data_all$SATVRMID)
data_all$SATMTMID <- as.character(data_all$SATMTMID)
data_all$SATMTMID <- as.integer(data_all$SATMTMID)
data_all$SATWRMID <- as.character(data_all$SATWRMID)
data_all$SATWRMID <- as.integer(data_all$SATWRMID)
data_all$ACTENMID <- as.character(data_all$ACTENMID)
data_all$ACTENMID <- as.integer(data_all$ACTENMID)
data_all$ACTMTMID <- as.character(data_all$ACTMTMID)
data_all$ACTMTMID <- as.integer(data_all$ACTMTMID)
data_all$ACTWRMID <- as.character(data_all$ACTWRMID)
data_all$ACTWRMID <- as.integer(data_all$ACTWRMID)

data_all$UGDS <- as.character(data_all$UGDS)
data_all$UGDS <- as.integer(data_all$UGDS)
data_all$COSTT4_A<- as.character(data_all$COSTT4_A)
data_all$COSTT4_A<- as.integer(data_all$COSTT4_A)
data_all$AVGFACSAL <- as.character(data_all$AVGFACSAL)
data_all$AVGFACSAL <- as.integer(data_all$AVGFACSAL)

data_all$ADM_RATE <- as.character(data_all$ADM_RATE)
data_all$ADM_RATE <- as.double(data_all$ADM_RATE)
data_all$PFTFAC <- as.character(data_all$PFTFAC)
data_all$PFTFAC <- as.double(data_all$PFTFAC)
data_all$PAR_ED_PCT_1STGEN <- as.character(data_all$PAR_ED_PCT_1STGEN)
data_all$PAR_ED_PCT_1STGEN <- as.double(data_all$PAR_ED_PCT_1STGEN)

data_final <- data_all

# Get a vector or college names
collegenames <- data_final$INSTNM