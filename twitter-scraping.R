library(tidyverse)
library(tidytext)
library(rtweet)

######## TWITTER SCRAPPING AND DATA FRAME PREP ######## 
#hi this is matteo
#retrive tweets from handle @CovidFoodFuture (once we get API access...!)
# for now, we use a "dummy" twitter dataset on Australian elections

# read csv and select relevant variables

election <- read.csv("election_tweets.csv")
df <- select(election, created_at, full_text, user_name)

######## STRING MATCHING TO ADD VARIABLES ######## 

# add country and participants name
countries <-  c("Benin", "Burkina Faso", "Congo DRC", "Ethiopia", "Kenya", "Madagascar", "Malawi", "Nigeria", "Senegal", "South Africa")

df$country <- ifelse(grepl("Benin", df$full_text, ignore.case = T), "Benin", 
              ifelse(grepl("BurkinaFaso|Burkina Faso|Burkina", df$full_text, ignore.case = T), "Burkina Faso", 
              ifelse(grepl("Ghana", df$full_text, ignore.case = T), "Ghana",
              ifelse(grepl("Nigeria", df$full_text, ignore.case = T), "Nigeria", 
              ifelse(grepl("Senegal", df$full_text, ignore.case = T), "Senegal", 
              ifelse(grepl("Congo|DRC|RDC", df$full_text, ignore.case = T), "DRC",
              ifelse(grepl("Malawi", df$full_text, ignore.case = T), "Malawi",
              ifelse(grepl("Madagascar", df$full_text, ignore.case = T), "Madagascar",
              ifelse(grepl("Kenya", df$full_text, ignore.case = T), "Kenya",
              ifelse(grepl("Ethiopia", df$full_text, ignore.case = T), "Ethiopia",
              ifelse(grepl("South Africa|SouthAfrica", df$full_text, ignore.case = T), "South Africa",
                     "N/A")))))))))))

df$participant <- ifelse(grepl("alexander_tmg", df$full_text, ignore.case = T), "alexander_tmg", 
              ifelse(grepl("AmandaNamayi", df$full_text, ignore.case = T), "AmandaNamayi", 
              ifelse(grepl("bruno__sj", df$full_text, ignore.case = T), "bruno__sj", 
              ifelse(grepl("EdoSango", df$full_text, ignore.case = T), "EdoSango", 
              ifelse(grepl("EMvuenga", df$full_text, ignore.case = T), "EMvuenga",
              ifelse(grepl("jes_tmg", df$full_text, ignore.case = T), "jes_tmg", 
              ifelse(grepl("JohnAgboola_", df$full_text, ignore.case = T), "JohnAgboola_", 
              ifelse(grepl("josherbert25", df$full_text, ignore.case = T), "josherbert25",
              ifelse(grepl("kanyemba_nellie", df$full_text, ignore.case = T), "kanyemba_nellie",        
              ifelse(grepl("lousvenja", df$full_text, ignore.case = T), "lousvenja",
              ifelse(grepl("MalalaOnisoa", df$full_text, ignore.case = T), "MalalaOnisoa",
              ifelse(grepl("mayadisraeli", df$full_text, ignore.case = T), "mayadisraeli",
              ifelse(grepl("miss_abimbola", df$full_text, ignore.case = T), "miss_abimbola",
              ifelse(grepl("ndomfuh_1", df$full_text, ignore.case = T), "ndomfuh_1",
              ifelse(grepl("olareeh", df$full_text, ignore.case = T), "olareeh",
              ifelse(grepl("sarahdhaen", df$full_text, ignore.case = T), "sarahdhaen",
              ifelse(grepl("mayadisraeli", df$full_text, ignore.case = T), "mayadisraeli",
              ifelse(grepl("serahKiragu_tmg", df$full_text, ignore.case = T), "serahKiragu_tmg",
              ifelse(grepl("sharonjcheboi", df$full_text, ignore.case = T), "sharonjcheboi",
              ifelse(grepl("wangumwangi", df$full_text, ignore.case = T), "wangumwangi",
              ifelse(grepl("YadeYacine", df$full_text, ignore.case = T), "YadeYacine",
              ifelse(grepl("YaredTesema1", df$full_text, ignore.case = T), "YaredTesema1",
                     "N/A")))))))))))))))))))))) 

# add columns for the different relevant categories (affected/targeted groups, region, producer/consumer side etc.)
# here we sometimes need to develop lexicons (group of words) to help us adequately capture the information

# affected/targeted group

df$target_farmers_producers <-  ifelse(grepl("farmer|farmers", df$full_text, ignore.case = T), 1, 0)
df$target_consumers <-  ifelse(grepl("consumer|consumers", df$full_text, ignore.case = T), 1, 0)
df$target_distributors <-  ifelse(grepl("distributor|distributors", df$full_text, ignore.case = T), 1, 0)
df$target_vendors <-  ifelse(grepl("vendor|vendors", df$full_text, ignore.case = T), 1, 0)
df$target_youth <-  ifelse(grepl("youth|young", df$full_text, ignore.case = T), 1, 0)
df$target_women <-  ifelse(grepl("woman|women", df$full_text, ignore.case = T), 1, 0)
df$target_children <-  ifelse(grepl("child|children|kid|kids", df$full_text, ignore.case = T), 1, 0)
df$target_informal <-  ifelse(grepl("informal|street", df$full_text, ignore.case = T), 1, 0)
df$target_vulnerable <-  ifelse(grepl("vulnerable|poor|poorer|poorest", df$full_text, ignore.case = T), 1, 0)

# area
df$area_urban <-  ifelse(grepl("urban|city", df$full_text, ignore.case = T), 1, 0)
df$area_rural <-  ifelse(grepl("rural", df$full_text, ignore.case = T), 1, 0)
df$area_linkages <-  ifelse(grepl("urban&rural", df$full_text, ignore.case = T), 1, 0)

#production
planting <- c("planting", "")
fields <- c("field", "steal", "stolen", "theft", "")



