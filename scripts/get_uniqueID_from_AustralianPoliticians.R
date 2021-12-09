# !diagnostics off
#### Preamble ####
# Purpose: This file adds the uniqueID from AustralianPoliticians to the voting data. It 
# sounds like a small thing but means we can link a lot of useful information.
# Author: Rohan Alexander
# Email: rohan.alexander@utoronto.ca
# Last updated: 6 September 2019
# Prerequisites: 
# - Need the AustralianPoliticians package
# - Need a rectangular Carr voting dataset from make_text_data_tabular.R
# To do: 
# - There's some outstanding issues where I think we've identified some issues in the original Carr data.
# This is going to take some effort to fix.
# - We're not going to be able to identify someone if they ran for a seat that they never 
# won. For instance, let's say that Turnbll gets sick of being on the sidelines and decides to run 
# for, say, Lilley, but Anika Wells beats him. We're not going to pick him up with the current approach.
# There'd be only a few politicians in this situation (Clive Palmer?) but we need to go back through 
# try to pick them up - maybe a broader names search or something?


#### Set up workspace ####
# install.packages("AustralianPoliticians")
library(AustralianPoliticians) # If you use this package, then email me and I'll send you a hex sticker.
library(lubridate)
library(tidyverse)

# Read in voting data
voting_data <- read_csv("outputs/voting_data.csv", guess_max = 10000)
# Read in Australian politicians data 
all <- AustralianPoliticians::get_auspol('all')
# Read in specifics about their division
by_division <- AustralianPoliticians::get_auspol('mps')


#### Prepare the dataset for matching ####
# Get rid of ones where they've gone back to the same seat - we're not going to match based on 
# year, so these repeat ones just gum up the joins.
by_division <- by_division %>% 
	distinct(uniqueID, division, .keep_all = TRUE)
# Join the division to the politicians
all <- all %>% 
	left_join(by_division, by = "uniqueID")
# The elections dataset is only for the reps.
all <- all %>% 
	filter(member == 1)
# Mash the name and the division together for a crude matcher based on that
all$name_and_division_for_matching <- if_else(is.na(all$commonName), 
																							paste(all$firstName, all$surname, all$division, sep = " "),
																							paste(all$commonName, all$surname, all$division, sep = " ")
																							)
all$name_and_division_for_matching <- tolower(all$name_and_division_for_matching)
all$name_and_division_for_matching <- str_remove_all(all$name_and_division_for_matching, "'")
all <- all %>% 
	select(uniqueID, displayName, surname, allOtherNames, division, name_and_division_for_matching)

# The politicians dataset only have people that won, so we don't want/need unsuccessful candidates as it just clutters things 
# voting_data <- voting_data %>% 
# 	filter(winnerDummy == 1)
# Create the same mash for the voting data
voting_data$name_and_division_for_matching <- paste(voting_data$full_name_for_matching, 
																										voting_data$division, 
																										sep = " ")
voting_data$name_and_division_for_matching <- tolower(voting_data$name_and_division_for_matching)
voting_data$name_and_division_for_matching <- str_remove_all(voting_data$name_and_division_for_matching, "'")

# voting_data <- voting_data %>% 
# 	select(division, state, election_date, original_name, name_and_division_for_matching)


#### Manual adjustments ####
# Fix the matcher
# Sometimes the election dataset doesn't use the common name
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "alexander downer mayo"] <- "alexander downer mayo"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "alexander downer angas"] <- "alick downer angas"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "archdale parkhill warringah"] <- "robert parkhill warringah"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "benjamin humphreys griffith"] <- "ben humphreys griffith"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "eric harrison wentworth"] <- "eric john harrison wentworth"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "james scullin yarra"] <- "jim scullin yarra"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "leslie johnson hughes"] <- "les johnson hughes"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "eric harrison wentworth"] <- "eric john harrison wentworth"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "reginald swartz darling downs"] <- "reg swartz darling downs"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "roger price chifley"] <- "leo price chifley"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "donald dobie cook"] <- "don dobie cook"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "james forbes barker"] <- "jim forbes barker"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "james fraser australian capital territory"] <- "jim fraser australian capital territory"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "raymond braithwaite dawson"] <- "ray braithwaite dawson"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "james carlton mackellar"] <- "jim carlton mackellar"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "edward holloway melbourne ports"] <- "jack holloway melbourne ports"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "robert katter kennedy" & year(voting_data$election_date) < 1990] <- "bob cummin katter kennedy"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "robert katter kennedy" & year(voting_data$election_date) > 1990] <- "bob carl katter kennedy"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "bob katter kennedy" & year(voting_data$election_date) > 1990] <- "bob carl katter kennedy"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "lionel bowen kingsford smith"] <- "lionel bowen kingsford-smith"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "james page maranoa"] <- "jim page maranoa"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "leslie haylen parkes"] <- "les haylen parkes"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "richard klugman prospect"] <- "dick klugman prospect"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "wilfrid kent hughes chisholm"] <- "bill kent hughes chisholm"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "douglas everingham capricornia"] <- "doug everingham capricornia"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "edwin yates adelaide"] <- "gunner yates adelaide"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "john beasley west sydney"] <- "jack beasley west sydney"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "victor johnson kalgoorlie"] <- "herbert johnson kalgoorlie"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "alexander buchanan mcmillan"] <- "alex buchanan mcmillan"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "james harrison blaxland"] <- "jim harrison blaxland"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "charles barnes mcpherson"] <- "ced barnes mcpherson"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "daniel curtin kingsford smith"] <- "dan curtin kingsford-smith"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "donald cameron brisbane"] <- "donald charles cameron brisbane"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "donald cameron griffith"] <- "donald milner cameron griffith"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "donald cameron moreton"] <- "donald milner cameron griffith"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "donald cameron oxley"] <- "donald alastair cameron oxley"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "edward riley cook"] <- "edward charles riley cook"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "kenneth fry fraser"] <- "ken fry fraser"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "laurie brereton kingsford smith"] <- "laurie brereton kingsford-smith"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "donald chipp higinbotham"] <- "don chipp higinbotham"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "ernest carr macquarie"] <- "ernie carr macquarie"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "james killen moreton"] <- "jim killen moreton"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "kathryn sullivan moncrieff"] <- "kathy sullivan moncrieff"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "robert ellicott wentworth"] <- "bob ellicott wentworth"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "russell gorman greenway"] <- "russ gorman greenway"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "wallace fife farrer"] <- "wal fife farrer"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "wallace fife hume"] <- "wal fife hume"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "william davies cunningham"] <- "billy davies cunningham"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "william morrison st george"] <- "bill morrison st george"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "dame enid lyons darwin"] <- "enid lyons darwin"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "david riordan kennedy"] <- "darby riordan kennedy"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "daniel mackinnon corangamite"] <- "dan mackinnon corangamite"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "daniel mackinnon corangamite"] <- "dan mackinnon corangamite"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "henry pearce capricornia"] <- "george pearce capricornia"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "robert brown charlton"] <- "bob mp brown charlton"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "robert brown hunter"] <- "bob mp brown hunter"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "donald chipp hotham"] <- "don chipp hotham"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "daniel curtin watson"] <- "dan curtin watson"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "david thomson leichhardt"] <- "david scott thomson leichhardt"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "gordon anderson kingsford smith"] <- "gordon anderson kingsford-smith"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "joseph berinson perth"] <- "joe berinson perth"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "joseph fitzgerald phillip"] <- "joe fitzgerald phillip"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "joseph gander reid"] <- "joe gander reid"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "kelvin thompson wills"] <- "kelvin thomson wills"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "kenneth aldred bruce"] <- "ken aldred bruce"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "leslie irwin mitchell"] <- "les irwin mitchell"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "roslyn kelly canberra"] <- "ros kelly canberra"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "sydney falstein watson"] <- "max falstein watson"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "william bourke fawkner"] <- "bill bourke fawkner"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "adam armstrong riverina"] <- "bill armstrong riverina"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "antony lamb la trobe"] <- "tony lamb la trobe"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "arthur dean herbert"] <- "gordon dean herbert"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "donald cameron fadden"] <- "donald milner cameron fadden"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "donald cameron lilley"] <- "donald james cameron lilley"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "eric harrison bendigo"] <- "eric fairweather harrison bendigo"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "francis baker griffith"] <- "francis matthew baker griffith"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "james hadley lilley"] <- "jim hadley lilley"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "james hume cook bourke"] <- "james cook bourke"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "john cotter kalgoorlie"] <- "mick cotter kalgoorlie"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "john duncan-hughes boothby"] <- "jack duncan-hughes boothby"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "kenneth aldred henty"] <- "ken aldred henty"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "keppel enderby australian capital territory"] <- "kep enderby australian capital territory"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "thomas andrews darebin"] <- "tom andrews darebin"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "philip lynch flinders"] <- "phillip lynch flinders"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "robert joshua ballaarat"] <- "bob joshua ballaarat"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "robert whan eden-monaro"] <- "bob whan eden-monaro"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "sophie panopoulos indi"] <- "sophie mirabella indi"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "ted holloway melbourne ports"] <- "jack holloway melbourne ports"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "thomas hughes parkes"] <- "tom hughes parkes"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "allan cadman mitchell"] <- "alan cadman mitchell"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "anne corcoran isaacs"] <- "ann corcoran isaacs"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "anthony luchetti macquarie"] <- "tony luchetti macquarie"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "antony whitlam grayndler"] <- "tony whitlam grayndler"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "chris pierce aston"] <- "chris pearce aston"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "christopher hurford adelaide"] <- "chris hurford adelaide"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "christopher miles braddon"] <- "chris miles braddon"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "daniel mackinnon wannon"] <- "dan mackinnon wannon"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "david cowan lyne"] <- "bruce cowan lyne"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "david thomson capricornia"] <- "david alexander thomson capricornia"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "donald cameron wilmot"] <- "donald norman cameron wilmot"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "donald dobie hughes"] <- "don dobie hughes"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "donald jessop grey"] <- "don jessop grey"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "edward holloway flinders"] <- "jack holloway flinders"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "edwin corboy swan"] <- "ted corboy swan"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "francis baker maranoa"] <- "francis patrick baker maranoa"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "francis baker oxley"] <- "francis matthew baker oxley"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "francis stewart lang"] <- "frank stewart lang"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "fred chaney pearce"] <- "fred michael chaney pearce"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "fred chaney perth"] <- "fred charles chaney perth"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "geoff giles angas"] <- "geoffrey giles angas"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "geoffrey prosser forrest"] <- "geoff prosser forrest"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "hon harry jenkins scullin"] <- "harry jr jenkins scullin"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "james scullin corangamite"] <- "jim scullin corangamite"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "james short ballaarat"] <- "jim short ballaarat"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "james short ballarat"] <- "jim short ballarat"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "john duncan-hughes wakefield"] <- "jack duncan-hughes wakefield"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "kathy martin moncrieff"] <- "kathy sullivan moncrieff"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "keppel enderby canberra"] <- "kep enderby canberra"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "langdon bonython barker"] <- "john bonython barker"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "leonard johnson burke"] <- "keith johnson burke"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "maxwell burr wilmot"] <- "max burr wilmot"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "mike hatton blaxland"] <- "michael hatton blaxland"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "raymond groom braddon"] <- "ray groom braddon"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "russell gorman chifley"] <- "russ gorman chifley"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "scott buchholtz wright"] <- "scott buchholz wright"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "steven lusher hume"] <- "stephen lusher hume"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "stewart macarthur corangamite"] <- "stewart mcarthur corangamite"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "thomas hughes berowra"] <- "tom hughes berowra"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "thomas mcveigh darling downs"] <- "tom mcveigh darling downs"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "wildred brimblecombe maranoa"] <- "wilfred brimblecombe maranoa"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "wilfred kent hughes chisholm"] <- "bill kent hughes chisholm"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "william arthur barton"] <- "bill arthur barton"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "harry jenkins scullin" & year(voting_data$election_date) <= 1985] <- "harry sr jenkins scullin"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "harry jenkins scullin" & year(voting_data$election_date) > 1985] <- "harry jr jenkins scullin"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "harold edwards berowra"] <- "harry edwards berowra"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "leonard keogh bowman"] <- "len keogh bowman"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "peter sidebottom braddon"] <- "sid sidebottom braddon"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "robert sercombe maribyrnong"] <- "bob sercombe maribyrnong"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "phillip barresi deakin"] <- "phil barresi deakin"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "malcolm washer moore"] <- "mal washer moore"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "steven ciobo moncrieff"] <- "steve ciobo moncrieff"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "patrick farmer macarthur"] <- "pat farmer macarthur"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "malcolm brough longman"] <- "mal brough longman"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "alexander mackenzie calare"] <- "sandy mackenzie calare"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "alexander wilson wimmera"] <- "alex wilson wimmera"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "david tollner solomon"] <- "dave tollner solomon"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "jim short ballarat"] <- "jim short ballaarat"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "donald cameron tasmania"] <- "donald norman cameron tasmania"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "langdon bonython south australia"] <- "john bonython south australia"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "langdon bonython barker"] <- "john bonython barker"


all %>% 
	group_by(name_and_division_for_matching) %>% 
	mutate(sadsf = n_distinct(uniqueID)) %>% 
	arrange(desc(sadsf))

# Urgh, still some duplicates
# lazza <- voting_data %>% 
# 	filter(name_and_division_for_matching == "larry anthony richmond")
# macca <- voting_data %>% 
# 	filter(name_and_division_for_matching == "john mcleay boothby")
# rm(lazza, macca)
all$name_and_division_for_matching[all$name_and_division_for_matching == "larry anthony richmond" & all$uniqueID == "Anthony1897"] <- "larry anthony richmond"
all$name_and_division_for_matching[all$name_and_division_for_matching == "larry anthony richmond" & all$uniqueID == "Anthony1961"] <- "larry james anthony richmond"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "larry anthony richmond" & year(voting_data$election_date) < 1993] <- "larry anthony richmond"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "larry anthony richmond" & year(voting_data$election_date) >= 1993] <- "larry james anthony richmond"

all$name_and_division_for_matching[all$name_and_division_for_matching == "john mcleay boothby" & all$uniqueID == "McLeay1893"] <- "john mcleay boothby"
all$name_and_division_for_matching[all$name_and_division_for_matching == "john mcleay boothby" & all$uniqueID == "McLeay1922"] <- "john eldrin mcleay boothby"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "john mcleay boothby" & voting_data$election_date < "1966-11-26"] <- "john mcleay boothby"
voting_data$name_and_division_for_matching[voting_data$name_and_division_for_matching == "john mcleay boothby" & voting_data$election_date >= "1966-11-26"] <- "john eldrin mcleay boothby"

# TODO
# Outstanding issues:
# "barry marks farrer" didn't win'
# "bob cronin stirling" didn't win'
# "joe murphy indi" didn't win
# "malcolm cole moreton" didn't win
# "rod mcgarvie lilley" didn't win
# These look like mistakes in the original dataset, so we're going to need to go in.


#### Match ####
rm(by_division)
all <- all %>% 
	select(uniqueID, name_and_division_for_matching)
# Do the match
voting_data <- voting_data %>% 
	left_join(all, by = "name_and_division_for_matching")

# voting_data_misses <- voting_data %>% 
# 	filter(is.na(uniqueID)) %>% 
# 	filter(election_date < "2019-01-01") %>% 
# 	group_by(name_and_division_for_matching) %>% 
# 	mutate(counter = n()) %>% 
# 	ungroup() %>% 
# 	arrange(desc(counter), name_and_division_for_matching)

voting_data <- 
	voting_data %>% 
	select(-full_name_for_matching, -full_name, -surname, -name_and_division_for_matching)

# hmmm <- voting_data %>% 
# 	filter(is.na(uniqueID) & capitalised == 1)
# hmmmm <- voting_data %>% 
# 	filter(is.na(uniqueID) & winnerDummy == 1)

# Save
write_csv(voting_data, "outputs/voting_data_with_ids.csv")


#### Let's try to address that names issue somehow ####
# The first aspect is that we shorten the names, so Tony Abbott is only used first, then Abbott is used after 
# that (in any given election count).
# So we want to assign that full name e.g. Tony Abbott to a uniqueID. 
# Then we can look for that full name anywhere else. It's not going to be perfect - if they changed their name or
# the records use a slightly different 'full name' e.g. T Abbott, but it's a start.

# Get a full name for each unique id
voting_data_names <- voting_data %>% 
	group_by(uniqueID) %>% 
	distinct(uniqueID, original_name) %>%
	filter(!is.na(uniqueID)) %>% 
	arrange(uniqueID) %>% 
	mutate(length_of_name = str_length(original_name)) %>% 
	mutate(longest_name = if_else(max(length_of_name) == length_of_name, 1, 0)) %>% 
	filter(longest_name == 1) %>% 
	ungroup() %>% 
	select(-length_of_name, -longest_name)


voting_data_temp <- voting_data %>% 
	filter(is.na(uniqueID)) %>%
	select(-uniqueID) %>% 
	left_join(voting_data_names, by = "original_name") %>% 
	filter(!is.na(uniqueID))

voting_data_temp <- voting_data_temp %>% 
	filter((division != "DENISON" & election_date != "1903-12-16") | (division == "DENISON" & uniqueID == "Cameron1851"))
	
voting_data_temp$uniqueID[voting_data_temp$division == "DENISON" & voting_data_temp$election_date == "1903-12-16" & voting_data_temp$uniqueID == "Cameron1879"] <- ""
voting_data_temp$uniqueID[voting_data_temp$division == "DENISON" & voting_data_temp$election_date == "1903-12-16" & voting_data_temp$uniqueID == "Cameron1900"] <- ""
voting_data_temp$uniqueID[voting_data_temp$division == "DENISON" & voting_data_temp$election_date == "1903-12-16" & voting_data_temp$uniqueID == "Cameron1917"] <- ""
voting_data_temp$uniqueID[voting_data_temp$division == "DENISON" & voting_data_temp$election_date == "1903-12-16" & voting_data_temp$uniqueID == "Cameron1940"] <- ""

voting_data_temp$uniqueID[voting_data_temp$division == "BALACLAVA" & voting_data_temp$election_date == "1929-10-12" & voting_data_temp$uniqueID == "Cameron1851"] <- ""
voting_data_temp$uniqueID[voting_data_temp$division == "BALACLAVA" & voting_data_temp$election_date == "1929-10-12" & voting_data_temp$uniqueID == "Cameron1879"] <- ""
voting_data_temp$uniqueID[voting_data_temp$division == "BALACLAVA" & voting_data_temp$election_date == "1929-10-12" & voting_data_temp$uniqueID == "Cameron1900"] <- ""
voting_data_temp$uniqueID[voting_data_temp$division == "BALACLAVA" & voting_data_temp$election_date == "1929-10-12" & voting_data_temp$uniqueID == "Cameron1917"] <- ""
voting_data_temp$uniqueID[voting_data_temp$division == "BALACLAVA" & voting_data_temp$election_date == "1929-10-12" & voting_data_temp$uniqueID == "Cameron1940"] <- ""







