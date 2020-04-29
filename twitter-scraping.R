library(tidyverse)
library(tidytext)
library(readxl)
library(writexl)
library(rtweet)

######## TWITTER SCRAPPING AND DATA FRAME PREP ######## 

#retrive tweets from handle @CovidFoodFuture (once we get API access...!)
#retrieve tweets from handle @CovidFoodFuture (once we get API access...!)
# df <- select(election, created_at, full_text, user_name)

# read csv file of manually compiled tweets
df1 <- read_excel("tweets_page1_25_v2.xlsx")
df1 <- na.omit(df1) # remove rows with N/A
#df <- df[,-(2:11)] # brackets are useful to select or remove rows/columns
df1 <- df1 %>% mutate(week = "1")


#read new tweets

df2 <- read_excel("tweets_page25_63.xlsx")
df2 <- na.omit(df2) 
colnames(df2) <- "full_text"
df2 <- df2 %>% mutate(week = "2-3")



#merge data frames

df <-  merge(df1, df2, by = c("full_text", "week"),all = T)


######## STRING MATCHING TO ADD VARIABLES ######## 


# add country and participants name
#countries <-  c("Benin", "Burkina Faso", "Congo DRC", "Ethiopia", "Kenya", "Madagascar", "Malawi", "Nigeria", "Senegal", "South Africa")


#countries

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
                     "Global")))))))))))
              #and africa wide?

#participants

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
              ifelse(grepl("maya_disraeli", df$full_text, ignore.case = T), "mayadisraeli",
              ifelse(grepl("serahKiragu_tmg", df$full_text, ignore.case = T), "serahKiragu_tmg",
              ifelse(grepl("sharonjcheboi", df$full_text, ignore.case = T), "sharonjcheboi",
              ifelse(grepl("wangumwangi", df$full_text, ignore.case = T), "wangumwangi",
              ifelse(grepl("YadeYacine", df$full_text, ignore.case = T), "YadeYacine",
              ifelse(grepl("YaredTesema1", df$full_text, ignore.case = T), "YaredTesema1",
                     "Other")))))))))))))))))))))) 

# add columns for the different relevant categories (affected/targeted groups, region, producer/consumer side etc.)
# here we sometimes need to develop lexicons (group of words) to help us adequately capture the information



df$target_farmers_producers <-  ifelse(grepl("farmer|farmers|producer|producers|producteur|producteurs", df$full_text, ignore.case = T), 1, 0)
df$target_consumers <-  ifelse(grepl("consumer|consumers", df$full_text, ignore.case = T), 1, 0)
df$target_distributors <-  ifelse(grepl("distributor|distributors", df$full_text, ignore.case = T), 1, 0)
df$target_vendors <-  ifelse(grepl("vendor|vendors", df$full_text, ignore.case = T), 1, 0)
df$target_youth <-  ifelse(grepl("youth|young", df$full_text, ignore.case = T), 1, 0)
df$target_women <-  ifelse(grepl("woman|women", df$full_text, ignore.case = T), 1, 0)
df$target_children <-  ifelse(grepl("child|children|kid|kids", df$full_text, ignore.case = T), 1, 0)
df$target_informal <-  ifelse(grepl("informal|street", df$full_text, ignore.case = T), 1, 0)
df$target_vulnerable <-  ifelse(grepl("vulnerable|poor|poorer|poorest", df$full_text, ignore.case = T), 1, 0)

# return strings to quickly verify if info is captured
# return binaries for discrete output / visualisation

#target_strings

df$target <- 
              ifelse(grepl("farmer|farmers|farms | farming| producer|producers |peasants|smallholder
                            |smallholders|small-scale | small scale | agriculteur|agriculteurs|
                             producteur|producteurs|paysans|exploitants | petit exploitant|petits exploitants
                             ", df$full_text, ignore.case = T), "farmers",
              ifelse(grepl("consumer|consumers | employees", df$full_text, ignore.case = T), "consumers",
              ifelse(grepl("distributor|distributors |distribution |transport | suppliers |food-suppliers | food-supply", df$full_text, ignore.case = T), "distributors",
              ifelse(grepl("youth|young|younger|students", df$full_text, ignore.case = T), "youth",
              ifelse(grepl("vendor|vendors | food market| food markets| street market |street markets", df$full_text, ignore.case = T), "vendors",
              ifelse(grepl("woman|women|mother|mothers|female", df$full_text, ignore.case = T), "women",
              ifelse(grepl("child|children|kid|kids|pupils", df$full_text, ignore.case = T), "children",
              ifelse(grepl("informal|street|", df$full_text, ignore.case = T), "informal",
              ifelse(grepl("vulnerable|poor|poorer|poorest", df$full_text, ignore.case = T), "vulnerable",
                     "other")))))))))

#target_binaries

df$target_farmers_producers <-  ifelse(grepl("farmer|farmers|producer|producers", df$full_text, ignore.case = T), 1, 0)
df$target_consumers <-   ifelse(grepl("consumer|consumers", df$full_text, ignore.case = T), 1, 0)
df$target_distributors <-  ifelse(grepl("distributor|distributors |distribution |transport | markets |suppliers |food-suppliers | food-supply", df$full_text, ignore.case = T), 1, 0)
df$target_vendors <-  ifelse(grepl("vendor|vendors", df$full_text, ignore.case = T), 1, 0)
df$target_youth <-  ifelse(grepl("youth|young", df$full_text, ignore.case = T), 1, 0)
df$target_women <-  ifelse(grepl("woman|women", df$full_text, ignore.case = T), 1, 0)
df$target_children <-  ifelse(grepl("child|children|kid|kids", df$full_text, ignore.case = T), 1, 0)
df$target_informal <-  ifelse(grepl("informal|street", df$full_text, ignore.case = T), 1, 0)
df$target_vulnerable <-  ifelse(grepl("vulnerable|poor|poorer|poorest", df$full_text, ignore.case = T), 1, 0)

# area_strings

df$area <-       
              ifelse(grepl("urban|city|cities|settlements|slums", df$full_text, ignore.case = T), "urban",
              ifelse(grepl("rural", df$full_text, ignore.case = T), "rural",
              ifelse(grepl("urban .* rural | region | county", df$full_text, ignore.case = T), "linkages",
                         "other")))

#area_binaries

              df$area_urban <- ifelse(grepl("urban|city|cities|settlements|slums", df$full_text, ignore.case = T), 1,0)
              df$area_rural <-   ifelse(grepl("rural", df$full_text, ignore.case = T), 1,0)
              df$area_linkages <-        ifelse(grepl("urban .* rural | region | county", df$full_text, ignore.case = T), 1,0)



#production_strings

df$production <-  
                ifelse (grepl("crops|produce|harvest|yields|products", df$full_text, ignore.case = T),"crops",
                ifelse (grepl("staples|grains|rice|millet|wheat |cassava | tubercule | racine | manioc | céréales", df$full_text, ignore.case = T), "staples",
                ifelse (grepl("fruit|fruits|vegetables|légumes", df$full_text, ignore.case = T), "fruit and veg",
                            "other")))    



#production_binaries

              df$produ_crops <- ifelse (grepl("crops|produce|harvest|yields|products", df$full_text, ignore.case = T),1,0)
              df$produ_staples <-  ifelse (grepl("staples|grains|rice|millet|wheat|cassava| tubercule | racine | manioc | céréales", df$full_text, ignore.case = T), 1,0)
              df$produfruitandveg <-  ifelse (grepl("fruit|fruits|vegetables|légumes", df$full_text, ignore.case = T), 1,0)




#CONSUMPTION

#consumption_strings

df$consumption <- 
                    ifelse (grepl("access | availability | accès| disponibilité ", df$full_text, ignore.case = T),"access",
                    ifelse (grepl("buying power| buy| income to buy |income to eat| 
                                income to feed| money to buy | money to eat| money to feed 
                                | purchasing power | pouvoir d'achat | pouvoirs d'achat ", df$full_text, ignore.case = T), "buying power",
                    ifelse (grepl("meals", df$full_text, ignore.case = T),"missing meals",
                    ifelse (grepl("price | prices | food cost | food costs |inflation", df$full_text, ignore.case = T),"food prices",
                    ifelse (grepl("diet | dietary | nutrition | immune system | système immunitaire", df$full_text, ignore.case = T),"dietary",
                    ifelse (grepl("middle men | intermediate | intermediaries ", df$full_text, ignore.case = T),"middle men corruption",
                                "other"))))))


#consumption_binaries

              df$consu_buying_power <- ifelse (grepl("buying power| buy| income to buy |income to eat| 
                                income to feed| money to buy | money to eat| money to feed 
                                | purchasing power | pouvoir d'achat | pouvoirs d'achat ", df$full_text, ignore.case = T), 1 ,0)
              df$consu_missing_meals <- ifelse (grepl("missing meals", df$full_text, ignore.case = T),1,0)
              df$consu_access <-  ifelse (grepl("access | availability | accès| disponibilité ", df$full_text, ignore.case = T),1,0)
              df$consu_food_prices <- ifelse (grepl("price | prices | food costs |inflation", df$full_text, ignore.case = T),1,0)
              df$consu_dietary <- ifelse (grepl("diet | dietary | nutrition | immune system | système immunitaire", df$full_text, ignore.case = T),1,0)
              df$consu_middlemen <-ifelse (grepl("middle men | intermediate | intermediaries ", df$full_text, ignore.case = T),1,0)



#*****************************************TEST new tweets************************************************************************************************************

              df$target <- 
                ifelse(grepl("farmer|farmers|farms | farming| producer|producers |peasants|smallholder
                            |smallholders|small-scale | small scale | agriculteur|agriculteurs|
                             producteur|producteurs|paysans|exploitants |
                             petit exploitant|petits exploitants
                             ", df$full_text, ignore.case = T), "farmers",
                       ifelse(grepl("consumer|consumers | employees", df$full_text, ignore.case = T), "consumers",
                              ifelse(grepl("distributor|distributors |distribution |transport | suppliers |food-suppliers | food-supply", df$full_text, ignore.case = T), "distributors",
                                     ifelse(grepl("youth|young|younger|students", df$full_text, ignore.case = T), "youth",
                                            ifelse(grepl("vendor|vendors | food market| food markets| street market |street markets", df$full_text, ignore.case = T), "vendors",
                                                   ifelse(grepl("woman|women|mother|mothers|female", df$full_text, ignore.case = T), "women",
                                                          ifelse(grepl("child|children|kid|kids|pupils", df$full_text, ignore.case = T), "children",
                                                                 ifelse(grepl("informal|street", df$full_text, ignore.case = T), "informal",
                                                                        ifelse(grepl("vulnerable|poor|poorer|poorest", df$full_text, ignore.case = T), "vulnerable",
                                                                               "other")))))))))


              df$consumption <- 
                ifelse (grepl("access | availability | accès| disponibilité ", df$full_text, ignore.case = T),"access",
                        ifelse (grepl("buying power| buy| income to buy |income to eat| 
                                income to feed| money to buy | money to eat| money to feed 
                                | purchasing power | pouvoir d'achat | pouvoirs d'achat ", df$full_text, ignore.case = T), "buying power",
                                ifelse (grepl("meals", df$full_text, ignore.case = T),"missing meals",
                                        ifelse (grepl("price | prices | food cost | food costs |inflation", df$full_text, ignore.case = T),"food prices",
                                                ifelse (grepl("diet | dietary | nutrition | immune system | système immunitaire", df$full_text, ignore.case = T),"dietary",
                                                        ifelse (grepl("middle men | intermediate | intermediaries ", df$full_text, ignore.case = T),"middle men corruption",
                                                                "other"))))))

              
# separate ENG and FR tweets

#df$language <- ifelse(grepl("é | è | à | ê | ç", df$full_text, ignore.case = T), "French", "English")
#df_FR <- df %>% filter(country %in% c("Benin", "Burkina Faso", "DRC", "Madagascar", "Senegal") | language == "French")

df_FR <- df %>% 
  filter(participant %in% c("josherbert25", "EMvuenga", "mayadisraeli", "MalalaOnisoa", "EdoSango", "ed_sango", "YadeYacine"))

df_ENG <- df %>% 
    anti_join(df_FR)

count(df_FR, participant) # tweets per countries
count(df_ENG, participant)

# filter df by week 
df_FR_w23 <- filter(df_FR, week == "2-3" )
df_ENG_w23 <- filter(df_ENG, week == "2-3" )

######## EXPORT TO EXCEL ######## 

write_xlsx(df_FR_w23,"df_FR_w23.xlsx")
write_xlsx(df_ENG_w23,"df_ENG_w23.xlsx")






