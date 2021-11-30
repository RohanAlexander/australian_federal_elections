# !diagnostics off
#### Preamble ####
# Purpose: This file takes Australian voting text files downloaded from Adam Carr and cleans them into tables that can be analysed.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 5 March 2019
# Prerequisites: Download the text files (see get_HoR_voting.R)
# To do:
# - Probably better to split the function into pieces.
# - Update fullnames so that "Mcm" goes to "McM" and "O'c" gotes to "O'C"
# Misc:
# - Preferential voting starts from 14 December 1918.
# - The full 2PP counts aren't calculated for every election until ???.
# - Back in the day, there were some that won their seat unopposed. I've designated this as a vote count of -1.
# - There are also a few counts and percentages that are unknown. These are made NA.
# - Redistributed vote share seems a bit dodgy in parts, and if you need to use that, probably best to reconstruct it yourself and check the numbers.


#### Set up workspace ####
library(lubridate)
library(tidyverse)
library(stringr)
# update.packages()


#### Make function ####
# Make function (we're going to pass this function to purrr to run on all the text files)
# The function ends up being rather large. Probably better to split it up.
clean_adams_voting_data <-
	function(name_of_input_text_file) {
		# For testing
		# name_of_input_text_file <- c("inputs/voting_data/2007repsby.txt")
		
		#### Read data and initial information ####
		# Read data and convert to tibble
		votingData <- read_lines(name_of_input_text_file) # Used to read txt files
		votingData <- tibble(raw_data = votingData) # Convert to rectangular
		
		# Basic cleaning
		votingData$raw_data <-
			str_replace(votingData$raw_data, "\\t", "    ") # Get rid of tabs
		votingData$raw_data <-
			str_replace(votingData$raw_data, "\x92", "'") # Get rid of wrong apostrophes e.g. "O\x92CONNOR" to "O'CONNOR"
		
		# Add the filename to the tibble
		file_txt_name <- basename(name_of_input_text_file) %>%
			str_replace(".txt", "")
		votingData$txt_file <- file_txt_name
		
		# Grab the year
		year_of_file <-
			str_sub(file_txt_name, start = 1, end = 4) # Year is used later to help adjust for slight changes over years.
		
		# Grab the state
		state_of_file <-
			str_sub(file_txt_name, start = 9) # State is used later to help adjust for slight changes between states.
		
		# Specific issue - annoying exceptions to the usual formatting of the byelections:
		# 1901repsby.txt
		if (file_txt_name == "1901repsby") {
			votingData <-
				rbind(votingData[1:41, ], votingData[41:42, ], votingData[41:42, ], votingData[41:nrow(votingData), ])
		}
		# 1969repsby.txt
		if (file_txt_name == "1969repsby") {
			votingData <-
				rbind(votingData[1:101, ], votingData[103:nrow(votingData), ])
		}
		# 1993repsby.txt
		if (file_txt_name == "1993repsby") {
			votingData <-
				rbind(votingData[1:157, ], votingData[159:nrow(votingData), ])
		}
		# Kaye Westbury died after close of nominations in NEWCASTLE 1998repsnsw.txt
		if (file_txt_name == "1998repsnsw") {
			votingData <-
				rbind(votingData[1:2603, ], votingData[2628:nrow(votingData), ])
			votingData <-
				rbind(votingData[1:2607, ], votingData[2611:nrow(votingData), ])
		}
		# 1943repsvic.txt there is some random line in there that seems like an error
		if (file_txt_name == "1943repsvic") {
			votingData <-
				rbind(votingData[1:549, ], votingData[551:nrow(votingData), ])
		}
		# Walter Pegler died after close of nominations in DICKSON 1993repsqld.txt and there is a random line that seems like an error
		if (file_txt_name == "1993repsqld") {
			votingData <-
				rbind(votingData[1:521, ], votingData[549:nrow(votingData), ])
			votingData <-
				rbind(votingData[1:524, ], votingData[528:nrow(votingData), ])
			votingData <-
				rbind(votingData[1:1050, ], votingData[1052:nrow(votingData), ])
		}
		# 1996repsqld.txt there is some random line in there that seems like an error
		if (file_txt_name == "1996repsqld") {
			votingData <-
				rbind(votingData[1:1389, ], votingData[1391:nrow(votingData), ])
		}
		# 1996repsvic.txt is missing a line
		if (file_txt_name == "1996repsvic") {
			votingData <-
				rbind(votingData[1:1172, ], c("George Mitsou                            1,683    2.5", "1996repsvic"), votingData[1173:nrow(votingData), ])
		}

		
		#### Add division name and number of voters ####
		# Helpers to disinguish the divisions and the bits of information within the divisions
		votingData <- votingData %>%
			mutate(
				div = str_detect(raw_data, "={3,}"), # Find where there are at least three "=" in a row
				div_split_here = lead(div, 1),
				section = str_detect(raw_data, "-{3,}") # Find where there are at least three - in a row
			)
		
		# Special case: A couple of times it says 'elected' instead of 'enrolled':
		# Bennelong in 1998 - 1998repsnsw
		votingData$raw_data[votingData$raw_data == "BENNELONG, NSW                  83,167 elected, 79,601 (95.7%) voted"] <-
			"BENNELONG, NSW                  83,167 enrolled, 79,601 (95.7%) voted"
		# Capricornia in 1901 - 1901repsqld
		votingData$raw_data[votingData$raw_data == "CAPRICORNIA, Qld                 10,662 elected, 7,285 (68.3%) voted"] <-
			"CAPRICORNIA, Qld                 10,662 enrolled, 7,285 (68.3%) voted"
		
		# Special case: We don't know the total number of votes cast, all we know is the number of formal votes:
		# COOLGARDIE in 1901repswa
		votingData$raw_data[votingData$raw_data == "COOLGARDIE, WA      15,969 enrolled, 6,303 (39.5%) cast formal votes"] <-
			"COOLGARDIE, WA      15,969 enrolled, unknown (unknown%) voted"
		# KALGOORLIE in 1901repswa
		votingData$raw_data[votingData$raw_data == "KALGOORLIE, WA      18,811 enrolled, 8,389 (44.6%) cast formal votes"] <-
			"KALGOORLIE, WA      18,811 enrolled, unknown (unknown%) voted"
		
		# Special case: There's a typo in BALLARAT in 1998repsvic
		votingData$raw_data[votingData$raw_data == "BALLARAT, Vic                  80,864 enrolled, 79,139 (97.(%) voted"] <-
			"BALLARAT, Vic                  80,864 enrolled, 79,139 (97%) voted"
		
		# Special case: There's a typo in GROOM in 1987repsby
		votingData$raw_data[votingData$raw_data == "GROOM, Qld                     71,402 enrolled, 62,831   88.0%) vote"] <-
			"GROOM, Qld                     71,402 enrolled, 62,831 (88.0%) voted"
		
		# Special case: In ADELAIDE in 1906repsby the order is a different way
		votingData$raw_data[votingData$raw_data == "ADELAIDE, SA                   29,874 enrolled, (33.7%) 10,068 voted"] <-
			"ADELAIDE, SA                   29,874 enrolled, 10,068 (33.7%) voted"
		
		# Special case: In MENZIES in 2010repsvic the percent voted is a typo
		votingData$raw_data[votingData$raw_data == "MENZIES, Vic                 90,966 enrolled, 85,410 (81,913%) voted"] <-
			"MENZIES, Vic                 90,966 enrolled, 85,410 (unknown%) voted"
		
		# Isolate the name of the division
		votingData <- votingData %>%
			mutate(div = ifelse(div_split_here == 1, raw_data, NA)) %>%
			separate(
				div,
				into = c("division", "other"),
				sep = "  ",
				extra = "merge",
				fill = "left"
			)
		
		votingData <-
			fill(votingData, division) # Push the division name into every row
		
		# Isolate the number of voters
		votingData <- votingData %>%
			mutate(div = ifelse(div_split_here == 1, raw_data, NA)) %>%
			separate(
				other,
				into = c("division_num_enrolled", "division_total_voted"),
				sep = "enrolled",
				# Don't have a space because there's an issue with Sturt in 1998 where it's missing a space
				extra = "merge",
				fill = "left"
			)
		
		# Isolate  turnout
		votingData <- votingData %>%
			mutate(div = ifelse(div_split_here == 1, raw_data, NA)) %>%
			separate(
				division_total_voted,
				into = c("division_num_voted", "division_percent_voted"),
				sep = " \\(",
				extra = "merge",
				fill = "left"
			) %>%
			mutate(
				division_percent_voted = str_remove(division_percent_voted, "%\\) voted"),
				division_percent_voted = str_remove(division_percent_voted, "\\) voted"),
				division_percent_voted = str_remove(division_percent_voted, "%\\) votes counted"),
				division_percent_voted = str_remove(division_percent_voted, "%\\) informal"),
				# There's a typo in RIVERINA for 2001repsnsw where it says informal but means voted
				division_num_voted = str_remove(division_num_voted, "^, ")
			)
		
		# Clean up the few where enrollment is unknown e.g 1913repsby	for KALGOORLIE, WA. Need the different spelling for 1919repsby for PARRAMATTA.
		votingData <- votingData %>%
			mutate(
				division_num_enrolled = if_else(
					str_detect(
						votingData$division_percent_voted,
						"enrolment unknown|enrollment unknown"
					),
					"unknown",
					division_num_enrolled
				),
				division_percent_voted = if_else(
					str_detect(
						votingData$division_percent_voted,
						"enrolment unknown|enrollment unknown"
					),
					"unknown",
					division_percent_voted
				)
			)
		
		votingData <-
			fill(
				votingData,
				c(
					division_num_enrolled,
					division_num_voted,
					division_percent_voted
				)
			)
		
		
		#### Election date ####
		# Add the election date if byelection
		if (state_of_file %in% c(
			"by",
			"",
			"benn2016",
			"brad2016",
			"frem2016",
			"long2016",
			"mayo2016",
			"newe2016",
			"pert2016",
			"went2018"
		)) {
			votingData <- votingData %>%
				separate(
					raw_data,
					into = c("election_date", "other"),
					sep = ",",
					remove = FALSE,
					extra = "merge",
					fill = "left"
				) %>%
				mutate(election_date = dmy(election_date))
		} else {
			# Add the election date if not a byelection
			date_of_election <- votingData$raw_data[2] %>%
				str_remove("LEGISLATIVE ELECTION OF ") %>%
				str_replace("29-30 MARCH 1901", "29 MARCH 1901") %>%
				dmy()
			if (is.na(date_of_election)) {
				date_of_election <- votingData$raw_data[1] %>%
					str_remove("LEGISLATIVE ELECTION OF ") %>%
					dmy()
			}
			votingData$election_date <- date_of_election
		}
		
		if (file_txt_name == "1998repsnsw") {
			votingData$election_date[votingData$division == "NEWCASTLE, NSW"] <- ymd("1998-11-21")
		}

		if (file_txt_name == "1993repsqld") {
			votingData$election_date[votingData$division == "DICKSON, Qld"] <- ymd("1993-04-17")
		}
		
		votingData <-
			fill(votingData, election_date) # Push the election date into every row
		
		
		#### State of the division ####
		# Isolate the name of the state (don't need when just reading one file because they are state-based, but need when reading many!)
		votingData <- votingData %>%
			separate(
				division,
				into = c("division", "state"),
				sep = ",",
				extra = "merge",
				fill = "right"
			)
		
		votingData <-
			fill(votingData, state) # Push the state name into every row
		
		
		#### Voting, Part 1 ####
		# We need to isolate the votes.
		# The text files have each round of voting under the division name.
		# Each round is split by a full line of hyphens: "---"
		# So we're going to identify the first-preferences based on this.
		# Alternatives to this include trying to use some features of the language or based on finding ">".
		# Identify which round of redistribution it is
		# This is also going to identify some misc top matter for each division in the first round and some misc end matter in the final round, we use the hyphen lines to get around this, see below
		votingData <- votingData %>%
			separate(
				raw_data,
				into = c("vote_redistribution_round", "other"),
				sep = "count: ",
				extra = "merge",
				remove = FALSE,
				fill = "left"
			) %>%
			mutate(
				vote_redistribution_round = ifelse(is.na(other), NA, vote_redistribution_round),
				vote_redistribution_round = ifelse(div_split_here == "TRUE", "1st", vote_redistribution_round)
			)
		
		votingData <-
			fill(votingData, vote_redistribution_round) # Push the number of the redistribution round down
		
		# Clean up the vote redistribution round number
		votingData <- votingData %>%
			mutate(
				vote_redistribution_round = str_remove(vote_redistribution_round, "st|nd|rd|th"),
				# Remove st, nd, rd, th
				vote_redistribution_remove = str_detect(vote_redistribution_round, "Notional")
			) %>%
			filter(vote_redistribution_remove == "FALSE") %>%
			select(-vote_redistribution_remove) %>%
			mutate(vote_redistribution_round = as.numeric(vote_redistribution_round))
		
		# With the preferences, we usually want the second batch of data after a section for each number
		votingData <- votingData %>%
			group_by(division, vote_redistribution_round) %>%
			mutate(
				vote_redistribution_round_counter = if_else(section == TRUE, 1, 0),
				vote_redistribution_round_counter = cumsum(vote_redistribution_round_counter)
			) %>%
			ungroup()
		
		
		#### 2PP ####
		# 2PP is the final vote count after preferences when there are only two candidates left. Usually it's ALP vs whatever is the major conservative party, but sometimes it's not.
		# Identify 2PP or closest approximation
		votingData <- votingData %>%
			group_by(division) %>%
			mutate(
				twoPP = max(vote_redistribution_round),
				twoPP = if_else(vote_redistribution_round == twoPP, 1, 0)
			) %>%
			ungroup()
		
		
		#### Top matter ####
		# Get rid of top matter for now. This has the party definitions and the election date so might need to come back and get it later.
		str_to_detect <- case_when(
			file_txt_name %in% c("1951repsby") ~ "BY-ELECTION",
			file_txt_name %in% c("1996repsby") ~ "FRASER",
			state_of_file == "by" ~ "BY-ELECTIONS",
			file_txt_name %in% c(
				"batm2016",
				"benn2016",
				"brad2016",
				"frem2016",
				"long2016",
				"mayo2016",
				"newe2016",
				"pert2016",
				"went2018"
			) ~ "enrolled",
			# The formatting of the by-elections in the 2016 period changes
			year_of_file >= 2010 ~ "VOTING BY DIVISION",
			TRUE ~  "VOTING BY CONSTITUENCY",
		)
		
		votingData <- votingData %>%
			mutate(
				start = str_detect(raw_data, str_to_detect),
				start = if_else(start == "TRUE", 1, 0),
				start = cumsum(start)
			) %>%
			filter(start == 1) %>%
			select(-start)
		
		rm(str_to_detect)
		
		
		#### Voting, Part 2 ####
		# There's misc stuff before the first preferences, thankfully the different types of information are split by the lines of hyphens, so we can use that to get rid of them
		votingData <- votingData %>%
			mutate(section = str_detect(raw_data, "-{3,}")) # Find where there are at least three hyphens - in a row
		
		# We're going to through away information is there is different amounts of top matter for each division.
		# Filter away everything apart from the first preferences.
		# Order matters for case_when - go from most to least specific
		filter_for_first_preferences <- case_when(
			file_txt_name %in% c(
				"1949repsact",
				"1951repsact",
				"1954repsact",
				"1955repsact",
				"1958repsact",
				"1961repsact",
				"1963repsact",
				"1966repsact",
				"1969repsact",
				"1972repsact"
			) ~ 1,
			file_txt_name %in% c(
				"2007repsby",
				"2013repsby",
				"batm2016",
				"benn2016",
				"brad2016",
				"frem2016",
				"long2016",
				"mayo2016",
				"newe2016",
				"pert2016",
				"went2018"
			) ~ 4,
			file_txt_name %in% c("1969repsby", "1993repsby") ~ 2,
			state_of_file == "nt" & year_of_file <= 1998  ~ 1,
			state_of_file == "by" ~ 3,
			year_of_file == 1901 ~ 1,
			year_of_file >= 2007 ~ 3,
			TRUE ~ 2
		)
		
		# Now we want to grab the votes
		# The set-up is different for whether there are preferences or not so need to distinguish between those cases
		are_there_preferences <- max(votingData$vote_redistribution_round)

		# Filter to just the voting.		
		if (are_there_preferences > 1) {
			votingData <- votingData %>%
				group_by(division) %>%
				mutate(start = if_else(section == "TRUE", 1, 0),
							 counter = cumsum(start)) %>%
				ungroup() %>%
				filter(( # First grab the first-preferences - these start at different points so need to use a point that is specific to the file
					counter == filter_for_first_preferences &
						vote_redistribution_round == 1
				) |
					( # Also grab the other-preferences - these seem to be more consistent.
						vote_redistribution_round > 1 &
							vote_redistribution_round_counter == 1
					)
				)
			# My attempt to be clever and use the remainder didn't work too well. Maybe could try again some time. Something like: mutate(counter = (counter - filter_for_first_preferences) %% filter_for_other_preferences_remainder) %>%
		} else { # This is just for the elections (mostly back in the day) when there weren't preferences.
			votingData <- votingData %>%
				group_by(division) %>%
				mutate(start = if_else(section == "TRUE", 1, 0),
							 counter = cumsum(start)) %>%
				ungroup() %>%
				filter(counter == filter_for_first_preferences)
		}
		
		
		#### Separate the various bits for first-preferences ####
		# The bits and pieces of the voting data - name, party, votes_count, votes_share, swing - are all on the one line. 
		# I am separating these using brute force based on character counts.
		# I know this isn't the best thing to do.
		# But I can't think of some other way to do this ATM.
		# This is admirable attention to detail by Adam - it seems as though he always pushes the characters number for the first preferences by 32, and so on! A few exceptions and differences, but good enough.
		# DON'T TELL MONICA ABOUT THIS. SHE'LL DIVORCE ME.
		# Remember that order matters for case_when - go from most- to least-specific
		str_position_for_name <- case_when(
			file_txt_name %in% c(
				"1955repsby",
				"1969repsvic",
				"2004repsqld",
				"2016repsnsw",
				"2016repsvic"
			) ~ 29,
			year_of_file %in% c(1919, 1946, 1958) ~ 28,
			TRUE ~ 31
		)
		
		# Special case - 2016repsnsw
		votingData$raw_data[votingData$raw_data == "Milan Maksimovic            CDP      6,198  07.2   +04.4"] <-
			"Milan Maksimovic             CDP      6,198   07.2   +04.4"
		votingData$raw_data[votingData$raw_data == "Lyndal Howison                    ALP     26,270  28.5   -04.0"] <-
			"Lyndal Howison                    ALP   26,270  28.5   -04.0"
		votingData$raw_data[votingData$raw_data == "Katie Gompertz                    ALP     15,926  17.0   +00.4"] <-
			"Katie Gompertz                    ALP   15,926  17.0   +00.4"
		
		# Special case - 2016repsvic.txt
		votingData$raw_data[votingData$raw_data == "Elissa Sutherland            Grn     10,781  11.7   +00.8"] <-
			"Elissa Sutherland            Grn     10,781   11.7   +00.8"
		votingData$raw_data[votingData$raw_data == "Jeanette Swain                    Grn      8,616  09.7   +00.3"] <-
			"Jeanette Swain                    Grn    8,616  09.7   +00.3"
		votingData$raw_data[votingData$raw_data == "Natalie Abboud                    Grn      7,609  08.5   +03.1"] <-
			"Natalie Abboud                    Grn    7,609  08.5   +03.1"
		
		# Special case - went2018
		votingData$raw_data[votingData$raw_data == "Heath                  0                 1,729  02.3"] <- 
			"Heath                  0  0.00           1,729  02.3"
		votingData$raw_data[votingData$raw_data == "Keldoulis              0                   307  00.4"] <-
			"Keldoulis              0  0.00             307  00.4"

		# Special case - newe2016
		votingData$raw_data[votingData$raw_data == "Smyth                  0                   515  00.6"] <- 
			"Smyth                  0  0.00             515  00.6"
		
		# Special case - 2016repsnsw
		votingData$raw_data[votingData$raw_data == "Albanese *           204  19,2          40,941  46.6"] <- 
			"Albanese *           204  19.2          40,941  46.6"
		votingData$raw_data[votingData$raw_data == "August               132  13,3           1,392  01.5"] <- 
			"August               132  13.3           1,392  01.5"
		
		# Special case - 2013repsvic
		votingData$raw_data[votingData$raw_data == "Kirkman                0                   183  00.2"] <- 
			"Kirkman                0  0.00             183  00.2"
		
		# Special case - 2007repsby
		votingData$raw_data[votingData$raw_data == "Price                  5 (0101)            777  01.1"] <- 
			"Price                  5 (01.1)            777  01.1"
		
		
		votingData_first_prefs <- votingData %>%
			filter(vote_redistribution_round == 1) %>%
			mutate(
				name = str_sub(raw_data, start = 1, end = str_position_for_name),
				party = str_sub(raw_data, start = str_position_for_name + 1, end = str_position_for_name + 8),
				votes_count = str_sub(raw_data, start = str_position_for_name + 9, end = str_position_for_name + 18),
				votes_share = str_sub(raw_data, start = str_position_for_name + 18, end = str_position_for_name + 25),
				swing = str_sub(raw_data, start = str_position_for_name + 25)
			)
		
		# Special case - there's a space that's been added in 1990repssa.txt for Dr Bob Catley
		votingData_first_prefs$votes_count[votingData_first_prefs$votes_count == " 27 422   "] <-
			"27,422"
		
		# The better way to do this that comes to mind is to use separate e.g.:
		# votingData <- votingData %>%
		# 	separate(raw_data,
		# 					 into = c("name", "party", "votes_count", "votes_share", "swing"),
		# 					 sep = "[:space:]{2,}",
		# 					 fill = "right")
		# But separate doesn't work easily because some are missing a party, so you'd end up with a bunch of exceptions anyway.
		
		# Clean up the grabs
		votingData_first_prefs <- votingData_first_prefs %>%
			mutate(
				votes_count = str_remove(votes_count, "[:space:]{2,}[:digit:]$"),
				votes_count = str_remove(votes_count, "[:space:]{2,}-$"),
				votes_share = str_remove(votes_share, "^[:digit:][:space:]{2,}"),
				votes_share = str_remove(votes_share, "\\(-$"),
				votes_share = str_remove(votes_share, "\\(?\\+[:digit:]?$"), # Looks for something like: "52.0 (-" or "35.3 (+0"
				votes_share = str_remove(votes_share, "\\(?\\-[:digit:]?$"), # Looks for something like: "52.0 (-" or "35.3 (-0"
				votes_share = str_remove(votes_share, "\\([:space:]?$"), # Looks for trailing "(" or "( )
				votes_share = str_remove(votes_share, "[:space:]{2,}0$"), # Looks for trailing "  0"
				swing = str_remove(swing, "^[:digit:][:space:]{2,}"), # Looks for leading numbers followed by two spaces "9  22.1"
				swing = str_remove(swing, "^[:digit:][:space:]?$"), # Looks for leading numbers followed by a space and nothing else "9 " or "9"
			)
		
		# Special case - one of the vote shares is accidently negative - for "Adrian van der Byl" in 2013repsnsw
		votingData_first_prefs$votes_share[votingData_first_prefs$votes_share == "-1.5   "] <-
			"1.5"
		
		#	There are no redistribution counts for first preferences
		votingData_first_prefs$redistributed_vote_count <- NA
		votingData_first_prefs$redistributed_vote_share <- NA

		#### Separate the various bits for other preferences ####
		# There is more consistency for other preferences so it's less of a big deal.
		# Special case: A couple of times it doesn't have two spaces or has two spaces before the *: 
		votingData$raw_data <- str_replace_all(votingData$raw_data, 
																					 c("1,921 23.8" = "1,921  23.8", # WILMOT in 1922repstas
																					 "1,511 18.8" = "1,511  18.8", # WILMOT in 1922repstas
																					 "2,629 32.6" = "2,629  32.6", # WILMOT in 1922repstas
																					 "1,996 24.8" = "1,996  24.8", # WILMOT in 1922repstas
																					 "2,950 36.6" = "2,950  36.6", # WILMOT in 1922repstas
																					 "2,799 34.7" = "2,799  34.7", # WILMOT in 1922repstas
																					 "2,308 28.6" = "2,308  28.6", # WILMOT in 1922repstas
																					 "412 34.1" = "412  34.1", # NORTHERN TERRITORY in 1922repsnt
																					 "492 40.8" = "492  40.8", # NORTHERN TERRITORY in 1922repsnt
																					 "303 25.1" = "303  25.1", # NORTHERN TERRITORY in 1922repsnt
																					 "19,730 59.5" = "19,730  59.5", # MORETON in 1922repsqld
																					 "13,422 40.5" = "13,422  40.5", # MORETON in 1922repsqld
																					 "8,850 13.8" = "8,850  13.8", # LA TROBE in 1961repsvic
																					 "28,057 43.8" = "28,057  43.8", # LA TROBE in 1961repsvic
																					 "27,125 42.4" = "27,125  42.4",  # LA TROBE in 1961repsvic
																					 "14,497 \\(26.8\\)" = "14,497  26.8", # SCULLIN in 1987repsvic
																					 "39,539 \\(73.2\\)" = "39,539  73.2", # SCULLIN in 1987repsvic
																					 "CLEELAND  \\*" = "CLEELAND *", # McEWEN in 1987repsvic
																					 "FISCHER  \\*" = "FISCHER *", # FARRER in 1987repsnsw
																					 "Chesterfield-Evans 2,336" = "Chesterfield-Evans  2,336", # NORTH SYDNEY in 2013repsby
																					 "Cambel            l2,041" = "Cambell            2,041", # PERTH in 1987repswa
																					 "Varide               l71" = "Varidel               71", # BEROWRA in 1990repsnsw
																					 "Wright             1,563 (43/9)" = "Wright             1,563 (43.9)" # GILMORE in 1987repsnsw
																					 ))
		
		# Get the other preferences
		are_there_preferences <-
			max(votingData$vote_redistribution_round)
		if (are_there_preferences > 1) {
			if (year_of_file >= "2013" & file_txt_name != "2013repsby") {
				votingData_other_prefs <- votingData %>%
					filter(vote_redistribution_round != 1) %>%
					separate(
						raw_data,
						into = c(
							"name",
							"redistributed_vote_count",
							"redistributed_vote_share",
							"votes_count",
							"votes_share"
						),
						sep = "[:space:]{2,}",
						remove = FALSE,
						extra = "merge",
						fill = "right"
					)
			} else {
				votingData_other_prefs <- votingData %>%
					filter(vote_redistribution_round != 1) %>%
					separate(
						raw_data,
						into = c(
							"name",
							"redistribution_count",
							"votes_count",
							"votes_share"
						),
						sep = "[:space:]{2,}",
						remove = FALSE,
						extra = "merge",
						fill = "right"
					)
				votingData_other_prefs <- votingData_other_prefs %>%
					separate(
						redistribution_count,
						into = c(
							"redistributed_vote_count",
							"redistributed_vote_share"
						),
						sep = " \\(",
						remove = TRUE,
						extra = "merge",
						fill = "right"
					)
			}
			
			votingData_other_prefs$party <- ""
				# rep_along(nrow(votingData_other_prefs), "0")
			votingData_other_prefs$swing <- ""
			
			
			#### Combine the first preferences with the other preferences ####
			votingData_other_prefs <- votingData_other_prefs %>%
				select(
					raw_data,
					vote_redistribution_round,
					other,
					election_date,
					txt_file,
					division,
					state,
					division_num_enrolled,
					division_num_voted,
					division_percent_voted,
					div_split_here,
					section,
					div,
					vote_redistribution_round_counter,
					twoPP,
					start,
					counter,
					name,
					party,
					votes_count,
					votes_share,
					swing,
					redistributed_vote_count,
					redistributed_vote_share
				)
			
			votingData <-
				rbind(votingData_first_prefs, votingData_other_prefs)
			rm(votingData_first_prefs, votingData_other_prefs)
		} else {
			votingData <- votingData_first_prefs
			rm(votingData_first_prefs)
		}
		
		votingData <- votingData %>%
			arrange(division, vote_redistribution_round)
		
		votingData <- votingData %>%
			mutate(keep_me = if_else(name == "" |
															 	is.na(votes_count), 0, 1)) %>%
			filter(keep_me == 1) %>%
			select(-keep_me)
		
		votingData$redistributed_vote_share <- str_remove(votingData$redistributed_vote_share, "\\)")
		

		#### Proper cleaning ####
		# Clean up the columns
		votingData <- votingData %>%
			filter(section == "FALSE") %>%
			select(
				division,
				state,
				division_num_enrolled,
				division_num_voted,
				division_percent_voted,
				name,
				party,
				vote_redistribution_round,
				votes_count,
				votes_share,
				swing,
				redistributed_vote_count,
				redistributed_vote_share,
				txt_file,
				election_date
			)
		
		# Clean up types and misc
		votingData <- votingData %>%
			mutate(
				division = str_squish(division),
				state = str_squish(state),
				state = str_replace(state, "Qld", "QLD"),
				state = str_replace(state, "Tas", "TAS"),
				state = str_replace(state, "Vic", "VIC"),
				division_num_enrolled = str_squish(division_num_enrolled),
				division_num_enrolled = str_remove_all(division_num_enrolled, ","),
				division_num_enrolled = if_else(division_num_enrolled == "unknown", "NA", division_num_enrolled),
				division_num_enrolled = as.integer(division_num_enrolled),
				division_num_voted = str_squish(division_num_voted),
				division_num_voted = str_remove_all(division_num_voted, ","),
				division_num_voted = str_remove_all(division_num_voted, "\\."),
				division_num_voted = if_else(division_num_voted == "unknown", "NA", division_num_voted),
				division_num_voted = as.integer(division_num_voted),
				division_percent_voted = str_squish(division_percent_voted),
				division_percent_voted = str_remove_all(division_percent_voted, ","),
				division_percent_voted = if_else(division_percent_voted == "unknown", "NA", division_percent_voted),
				division_percent_voted = as.double(division_percent_voted),
				name = str_squish(name),
				name = if_else(name == "(exhausted", "exhausted", name),
				name = if_else(name == "(exhausted)", "exhausted", name),
				party = str_squish(party),
				party = str_replace(party, "Prot U", "Prot"),
				party = str_replace(party, "FT U", "FT"),
				votes_count = str_remove(votes_count, ","),
				votes_count = str_remove(votes_count, "\\."),
				# There's a few where the grouping is distinguished by decimal instead of comma e.g. 2007repssa for Nick Champion.
				votes_count = str_replace(
					votes_count,
					"Unopposed|unopposed|Unoppose|nopposed|unoppose|Unoppos|Unoppo|Unopp|Unop",
					"-1"
				), # Yeah, I'm a monster, but I don't know what to do instead. I want the vote count to be numeric.
				votes_count = as.integer(votes_count),
				votes_share = ifelse(votes_count == -1,-1, votes_share),
				votes_share = str_remove(votes_share, ","),
				votes_share = str_remove(votes_share, "\\)"),
				votes_share = as.double(votes_share),
				swing = str_remove(swing, "\\("),
				swing = str_remove(swing, "\\)"),
				swing = str_remove(swing, "\\("),
				swing = str_remove(swing, "\\)"),
				swing = str_remove(swing, "\\+"),
				swing = as.double(swing),
				redistributed_vote_count = str_remove(redistributed_vote_count, ","),
				redistributed_vote_count = as.integer(redistributed_vote_count),
				redistributed_vote_share = str_remove(redistributed_vote_share, ","),
				redistributed_vote_share = as.double(redistributed_vote_share)
			)
		
		
	}


#### Use function ####
# Create list of the text file names to apply the function to
all_of_adams_files <- list.files("inputs/voting_data",
																 pattern = "txt",
																 full.names = TRUE)

# Get rid of byelections files where there were no byelections during the term.
f_tibble <- tibble(filenames = all_of_adams_files)
f_tibble$just_name <- basename(f_tibble$filenames)
f_tibble <- f_tibble %>%
	filter(
		!just_name %in% c(
			"1922repsby.txt",
			"1946repsby.txt",
			"1949repsby.txt",
			"2010repsby.txt"
		)
	)
all_of_adams_files <- f_tibble$filenames
rm(f_tibble)

# Apply the function to that list
all_voting <-
	purrr::map_df(all_of_adams_files, clean_adams_voting_data)

rm(all_of_adams_files, clean_adams_voting_data)


#### Cleaning and checks of subsequent data ####
# Make the states consistent
all_voting$division[is.na(all_voting$state)] %>% unique()
all_voting$state[all_voting$division == "SOUTH AUSTRALIA"] <- "SA"
all_voting$state[all_voting$division == "TASMANIA"] <- "TAS"
all_voting$state[all_voting$division == "NORTHERN TERRITORY"] <- "NT"
all_voting$state[all_voting$division == "AUSTRALIAN CAPITAL TERRITORY"] <- "ACT"
all_voting$state %>% unique() %>% sort()

# Check the parties
counts_by_party <- all_voting %>%
	select(party) %>%
	group_by(party) %>%
	summarise(count = n()) %>%
	arrange(desc(count))

rm(counts_by_party)

# Check the files
all_voting$txt_file %>% unique()
# There should be 387 less the ones that have no content, so 383.

# Check the divisions
counts_by_division <- all_voting %>%
	select(division) %>%
	group_by(division) %>%
	summarise(count = n()) %>%
	arrange(desc(count))
all_voting$division[all_voting$division == "COWAN WA"] <- "COWAN"
# KINGSFORD SMITH used to be KINGSFORD-SMITH until 2001, but it's the same place so just changed all to the current spelling.
all_voting$division[all_voting$division == "KINGSFORD-SMITH"] <- "KINGSFORD SMITH"
rm(counts_by_division)

# Check the names
counts_by_name <- all_voting %>%
	select(name) %>%
	group_by(name) %>%
	summarise(count = n()) %>%
	arrange(desc(count))
# Fix names for consistency and matching, remove: 
# "Hon Dr "
# "Dr "
# "Hon "
# "Rt Hon "
# "Rt Hon Sir "
# "Rt Hon Dr "
# "Hon Sir "
# "Sir "
all_voting <- all_voting %>%
	mutate(name = str_remove_all(name, "^Rt Hon Sir |^Rt Hon Dr |^Hon Sir |^Rt Hon |^Hon Dr |^Hon |^Sir |^Dr "))
# Remove stars
all_voting <- all_voting %>%
	mutate(starred = str_detect(name, "\\*"),
				 name = str_remove_all(name, "\\*"))
# Remove plus
all_voting <- all_voting %>%
	mutate(plus = str_detect(name, "\\+"),
				 name = str_remove_all(name, "\\+"))
# Follow up to see what his name should be actually:
"Ivor F"
"F Ivor"
# Remove any trailing spaces
all_voting <- all_voting %>%
	mutate(name = str_squish(name))
# Checkk for unusual
counts_by_name <- all_voting %>%
	mutate(
		numbers = str_detect(str_squish(name), "[:digit:]"),
		punctuation = str_detect(str_squish(name), "[:punct:]")
	) %>%
	filter(numbers == TRUE | punctuation == TRUE)
# Follow up on to check if right, because seems unusual
"Jane Smith/New"
rm(counts_by_name)


#  Check the voting rounds - shouldn't be any > 1 before 1918 which was when preferences were brought in
voting_rounds <- all_voting %>%
	filter(year(election_date) > 1918) %>%
	group_by(vote_redistribution_round) %>%
	summarise(count = n())
# Small typo in 2019 Waringah
all_voting$vote_redistribution_round[all_voting$vote_redistribution_round == "98"] <- "9"
rm(voting_rounds)

# Fix a date
# There's a typo in the NT file where it has the wrong date
all_voting$election_date[all_voting$txt_file == "1972repsnt" & all_voting$division == "NORTHERN TERRITORY"] <- ymd("1972-12-02")


# Check the parties
counts_by_party <- all_voting %>%
	select(party) %>%
	group_by(party) %>%
	summarise(count = n()) %>%
	arrange(desc(count))

# Follow up on:
"ACon"
"ACons"
# Presumably these should be the same abbreviation.
rm(counts_by_party)

# Check the first preferences for whether reasonable - we want only numbers properly formatted. So "98001" is fine but "9  8001" suggests something wrong
# This check will best identify issues if you don't force numeric in the function
counts_by_first_pref <- all_voting %>%
	mutate(
		space = str_detect(str_squish(votes_count), "[:space:]"),
		letters = str_detect(str_squish(votes_count), "[:alpha:]"),
		punctuation = str_detect(str_squish(votes_count), "[:punct:]")
	) %>%
	filter(space == TRUE | letters == TRUE)
rm(counts_by_first_pref)

# Check the first preferences shares for whether reasonable - we want only numbers properly formatted. So "98001" is fine but "9  8001" suggests something wrong
# This check will best identify issues if you don't force numeric in the function
counts_by_votes_shares <- all_voting %>%
	mutate(
		space = str_detect(str_squish(votes_share), "[:space:]"),
		letters = str_detect(str_squish(votes_share), "[:alpha:]")
	) %>%
	filter(space == TRUE | letters == TRUE)

rm(counts_by_votes_shares)

# Fix a few
all_voting$votes_share[all_voting$votes_share == "71.4" & all_voting$original_name == "Wilson"] <- "64.48"
all_voting$votes_share[all_voting$votes_share == "28.6" & all_voting$original_name == "Payne"] <- "35.52"

# Check the voter counts and shares and turnouts for whether reasonable.
# This check will best identify issues if you don't force numeric in the function
enrolled <- all_voting %>%
	mutate(
		space = str_detect(str_squish(division_num_enrolled), "[:space:]"),
		letters = str_detect(str_squish(division_num_enrolled), "[:alpha:]"),
		punctuation = str_detect(str_squish(division_num_enrolled), "[:punct:]")
	) %>%
	filter(space == TRUE | letters == TRUE | punctuation == TRUE)
voted <- all_voting %>%
	mutate(
		space = str_detect(str_squish(division_num_voted), "[:space:]"),
		letters = str_detect(str_squish(division_num_voted), "[:alpha:]"),
		punctuation = str_detect(str_squish(division_num_voted), "[:punct:]")
	) %>%
	filter(space == TRUE | letters == TRUE | punctuation == TRUE)
share <- all_voting %>%
	mutate(
		division_percent_voted = str_remove(division_percent_voted, "\\."),
		space = str_detect(str_squish(division_percent_voted), "[:space:]"),
		letters = str_detect(str_squish(division_percent_voted), "[:alpha:]"),
		punctuation = str_detect(str_squish(division_percent_voted), "[:punct:]")
	) %>%
	filter(space == TRUE | letters == TRUE | punctuation == TRUE)

rm(enrolled, voted, share)

# Check the preferences
vote_count <- all_voting %>%
	mutate(
		space = str_detect(str_squish(redistributed_vote_count), "[:space:]"),
		letters = str_detect(str_squish(redistributed_vote_count), "[:alpha:]"),
		punctuation = str_detect(str_squish(redistributed_vote_count), "[:punct:]")
	) %>%
	filter(space == TRUE | letters == TRUE | punctuation == TRUE)
rm(vote_count)
vote_share <- all_voting %>%
	mutate(
		redistributed_vote_share = str_remove(redistributed_vote_share, "\\."),
		space = str_detect(str_squish(redistributed_vote_share), "[:space:]"),
		letters = str_detect(str_squish(redistributed_vote_share), "[:alpha:]"),
		punctuation = str_detect(str_squish(redistributed_vote_share), "[:punct:]")
	) %>%
	filter(space == TRUE | letters == TRUE | punctuation == TRUE)
rm(vote_share)


#### Fix names in preferences ####
# At the moment the names in the preferences are just the surnames. But if we want to be able to match the first-preferences with the 2PP then we'll need someway to match them.
all_voting$name[all_voting$name == "Birchall" & all_voting$division == "SWAN"] <- "Burchall"
all_voting$name[all_voting$name == "Bludworth" & all_voting$division == "NEWCASTLE"] <- "Budworth"
all_voting$name[all_voting$name == "Brotherton" & all_voting$division == "LALOR"] <- "Sheffield-Brotherton"
all_voting$name[all_voting$name == "Georgi" & all_voting$division == "FORREST"] <- "Giorgi"
all_voting$name[all_voting$name == "Jayawardene" & all_voting$division == "WENTWORTH"] <- "Jayawardena"
all_voting$name[all_voting$name == "Salter" & all_voting$division == "LEICHHARDT"] <- "Salier"
all_voting$name[all_voting$name == "Weare" & all_voting$division == "NEW ENGLAND"] <- "Wearne"
all_voting$name[all_voting$name == "Federicks" & all_voting$division == "RICHMOND"] <- "Fredericks"
all_voting$name[all_voting$name == "Westo" & all_voting$division == "WILMOT"] <- "Weston"
all_voting$name[all_voting$name == "Robin" & all_voting$division == "PATERSON"] <- "Robinson"
all_voting$name[all_voting$name == "Sainbury" & all_voting$division == "EDEN-MONARO"] <- "Sainsbury"
all_voting$name[all_voting$name == "Connley" & all_voting$division == "GIPPSLAND"] <- "Conley"
all_voting$name[all_voting$name == "Cambel" & all_voting$division == "PERTH"] <- "Campbell"
all_voting$name[all_voting$name == "Varide" & all_voting$division == "BEROWRA"] <- "Varidel"
all_voting$name[all_voting$name == "Carey" & all_voting$division == "FREMANTLE"] <- "Cary"
all_voting$name[all_voting$name == "Hetherich" & all_voting$division == "CASEY"] <- "Heatherich"
all_voting$name[all_voting$name == "Bekairis" & all_voting$division == "SCULLIN"] <- "Bekiaris"
all_voting$name[all_voting$name == "Petersen" & all_voting$division == "WATSON"] <- "Peterson"
all_voting$name[all_voting$name == "Heelam" & all_voting$division == "BRADFIELD"] <- "Neelam"
all_voting$name[all_voting$name == "Musolini" & all_voting$division == "HINDMARSH"] <- "Musolino"
all_voting$name[all_voting$name == "LAMP" & all_voting$division == "LONGMAN"] <- "LAMB"

all_voting$name[all_voting$name == "Owen Andrew" & all_voting$division == "CHIFLEY"] <- "Andrew Owen"
all_voting$name[all_voting$name == "Martin Hetherich" & all_voting$division == "CASEY"] <- "Martin Heatherich"
all_voting$name[all_voting$name == "Anthony Peterson" & all_voting$division == "GIPPSLAND"] <- "Anthony Petersen"
all_voting$name[all_voting$name == "Peter Procter" & all_voting$division == "FORREST"] <- "Peter Proctor"
all_voting$name[all_voting$name == "Joanne Clark" & all_voting$division == "FRASER"] <- "Joanne Clarke"
all_voting$name[all_voting$name == "Rod Benninson" & all_voting$division == "HUNTER"] <- "Rod Bennison"
all_voting$name[all_voting$name == "Garry Sloan" & all_voting$division == "CALARE"] <- "Garry Sloane"
all_voting$name[all_voting$name == "Kevin Glancey" & all_voting$division == "ROBERTSON"] <-  "Kevin Glancy"
all_voting$name[all_voting$name == "Adrian Deane" & all_voting$division == "FORDE"] <-  "Adrian Dean"
all_voting$name[all_voting$name == "Dikran Chabdijan" & all_voting$division == "BRUCE"] <-  "Dikran Chabdjian"
all_voting$name[all_voting$name == "Wolf Vogt" & all_voting$division == "LA TROBE"] <-  "Wolf Voigt"
all_voting$name[all_voting$name == "Robert Sims" & all_voting$division == "BOOTHBY"] <-  "Robert Simms"
all_voting$name[all_voting$name == "Graham Mathews" & all_voting$division == "BATMAN"] <-  "Graham Matthews"

all_voting <- all_voting %>% 
	mutate(surname = word(name, -1),
				 surname = tolower(surname))

all_voting$surname[all_voting$name == "KEATING, Hon P" & all_voting$txt_file == "1987repsnsw"] <- "KEATINGHonP"
all_voting$surname[all_voting$name == "Paul KEATING" & all_voting$txt_file == "1987repsnsw"] <- "KEATINGHonP"
all_voting$surname[all_voting$name == "Keating, P" & all_voting$txt_file == "1987repsnsw"] <- "KeatingP_notPM"
all_voting$surname[all_voting$name == "Paul Keating" & all_voting$txt_file == "1987repsnsw"] <- "KeatingP_notPM"

all_voting$surname[all_voting$name == "Kate Butler" & all_voting$txt_file == "1996repsnsw"] <- "KateButler"
all_voting$surname[all_voting$name == "Butler, Kate" & all_voting$txt_file == "1996repsnsw"] <- "KateButler"
all_voting$surname[all_voting$name == "Kevin Butler" & all_voting$txt_file == "1996repsnsw"] <- "KevinButler"
all_voting$surname[all_voting$name == "Butler, Kevin" & all_voting$txt_file == "1996repsnsw"] <- "KevinButler"

all_voting$surname[all_voting$name == "Charles Abbott" & all_voting$txt_file == "1943repsnsw"] <- "CharlesAbbott"
all_voting$surname[all_voting$name == "Abbott, C" & all_voting$txt_file == "1943repsnsw"] <- "CharlesAbbott"
all_voting$surname[all_voting$name == "Abbott, J" & all_voting$txt_file == "1943repsnsw"] <- "JosephAbbott"
all_voting$surname[all_voting$name == "Joseph Abbott" & all_voting$txt_file == "1943repsnsw"] <- "JosephAbbott"
all_voting$surname[all_voting$name == "ABBOTT, J" & all_voting$txt_file == "1943repsnsw"] <- "JosephAbbott"

all_voting$surname[all_voting$name == "Barry O'Brien" & all_voting$txt_file == "1966repsvic"] <- "BarryOBrien"
all_voting$surname[all_voting$name == "O'Brien, B" & all_voting$txt_file == "1966repsvic"] <- "BarryOBrien"
all_voting$surname[all_voting$name == "John O'Brien" & all_voting$txt_file == "1966repsvic"] <- "JohnOBrien"
all_voting$surname[all_voting$name == "O'Brien, J" & all_voting$txt_file == "1966repsvic"] <- "JohnOBrien"

all_voting$surname[all_voting$name == "Brian Hughes" & all_voting$txt_file == "1993repsnsw"] <- "BrianHughes"
all_voting$surname[all_voting$name == "Hughes, R" & all_voting$txt_file == "1993repsnsw"] <- "BrianHughes"
all_voting$surname[all_voting$name == "David Hughes" & all_voting$txt_file == "1993repsnsw"] <- "DavidHughes"
all_voting$surname[all_voting$name == "Hughes, D" & all_voting$txt_file == "1993repsnsw"] <- "DavidHughes"

all_voting$surname[all_voting$name == "Ted Lindsay" & all_voting$txt_file == "1996repsqld"] <- "TedLindsay"
all_voting$surname[all_voting$name == "Lindsay, T" & all_voting$txt_file == "1996repsqld"] <- "TedLindsay"
all_voting$surname[all_voting$name == "Peter Lindsay" & all_voting$txt_file == "1996repsqld"] <- "PeterLindsay"
all_voting$surname[all_voting$name == "Lindsay, P" & all_voting$txt_file == "1996repsqld"] <- "PeterLindsay"
all_voting$surname[all_voting$name == "LINDSAY, P" & all_voting$txt_file == "1996repsqld"] <- "PeterLindsay"
all_voting$surname[all_voting$name == "Zillah Jackson" & all_voting$txt_file == "1996repsqld"] <- "ZillahJackson"
all_voting$surname[all_voting$name == "Jackson, Z" & all_voting$txt_file == "1996repsqld"] <- "ZillahJackson"

all_voting <- all_voting %>%
	mutate(capitalised = str_detect(name, "[:upper:]{2,}"),
				 name = str_to_title(name))


all_voting$surname[all_voting$election_date == "1925-11-14" & all_voting$division == "RICHMOND" & all_voting$surname == "green" & all_voting$name == "Harry Green"] <- "HarryGreen"
all_voting$surname[all_voting$election_date == "1925-11-14" & all_voting$division == "RICHMOND" & all_voting$surname == "green" & all_voting$name == "H Green"] <- "HarryGreen"
all_voting$surname[all_voting$election_date == "1928-11-17" & all_voting$division == "MARIBYRNONG" & all_voting$surname == "fenton" & all_voting$name == "Arthur Fenton"] <- "ArthurFenton"
all_voting$surname[all_voting$election_date == "1929-10-12" & all_voting$division == "ECHUCA" & all_voting$surname == "hill" & all_voting$name == "Edward Hill"] <- "EdwardHill"
all_voting$surname[all_voting$election_date == "1931-12-19" & all_voting$division == "FREMANTLE" & all_voting$surname == "watson" & all_voting$name == "Keith Watson"] <- "KeithWatson"
all_voting$surname[all_voting$election_date == "1940-09-21" & all_voting$division == "EAST SYDNEY" & all_voting$surname == "gould" & all_voting$name == "Diana Gould"] <- "DianaGould"
all_voting$surname[all_voting$election_date == "1940-09-21" & all_voting$division == "EAST SYDNEY" & all_voting$surname == "gould" & all_voting$name == "D Gould"] <- "DianaGould"
all_voting$surname[all_voting$election_date == "1943-08-21" & all_voting$division == "LANG" & all_voting$surname == "mulcahy" & all_voting$name == "Daniel Mulcahy"] <- "DanielMulcahy"
all_voting$surname[all_voting$election_date == "1946-09-28" & all_voting$division == "BATMAN" & all_voting$surname == "brennan" & all_voting$name == "Allan Brennan"] <- "AllanBrennan"
all_voting$surname[all_voting$election_date == "1951-04-28" & all_voting$division == "COOK" & all_voting$surname == "hatfield" & all_voting$name == "Royston Hatfield"] <- "RoystonHatfield"
all_voting$surname[all_voting$election_date == "1954-05-29" & all_voting$division == "CUNNINGHAM" & all_voting$surname == "parkinson" & all_voting$name == "William Parkinson"] <- "WilliamParkinson"
all_voting$surname[all_voting$election_date == "1961-12-09" & all_voting$division == "KINGSFORD SMITH" & all_voting$surname == "cunningham" & all_voting$name == "John Cunningham"] <- "JohnCunningham"
all_voting$surname[all_voting$election_date == "1974-05-18" & all_voting$division == "SHORTLAND" & all_voting$surname == "morris" & all_voting$name == "David Morris"] <- "DavidMorris"
all_voting$surname[all_voting$election_date == "1975-12-13" & all_voting$division == "LANG" & all_voting$surname == "stewart" & all_voting$name == "John Stewart"] <- "JohnStewart"
all_voting$surname[all_voting$election_date == "1977-12-10" & all_voting$division == "HUGHES" & all_voting$surname == "johnson" & all_voting$name == "Kenneth Johnson"] <- "KennethJohnson"
all_voting$surname[all_voting$election_date == "1990-03-24" & all_voting$division == "BASS" & all_voting$surname == "smith" & all_voting$name == "Silvia Smith"] <- "SilviaSmith"
all_voting$surname[all_voting$election_date == "1990-03-24" & all_voting$division == "BASS" & all_voting$surname == "smith" & all_voting$name == "S Smith"] <- "SilviaSmith"
all_voting$surname[all_voting$election_date == "1993-03-13" & all_voting$division == "BASS" & all_voting$surname == "smith" & all_voting$name == "Silvia Smith"] <- "SilviaSmith"
all_voting$surname[all_voting$election_date == "1993-03-13" & all_voting$division == "BASS" & all_voting$surname == "smith" & all_voting$name == "S Smith"] <- "SilviaSmith"
all_voting$surname[all_voting$election_date == "1993-03-13" & all_voting$division == "CANBERRA" & all_voting$surname == "kelly" & all_voting$name == "Sally Kelly"] <- "SallyKelly"
all_voting$surname[all_voting$election_date == "1993-03-13" & all_voting$division == "LOWE" & all_voting$surname == "woods" & all_voting$name == "Peter Woods"] <- "PeterWoods"
all_voting$surname[all_voting$election_date == "1993-03-13" & all_voting$division == "LOWE" & all_voting$surname == "woods" & all_voting$name == "P Woods"] <- "PeterWoods"
all_voting$surname[all_voting$election_date == "1996-03-02" & all_voting$division == "BASS" & all_voting$surname == "smith" & all_voting$name == "Silvia Smith"] <- "SilviaSmith"
all_voting$surname[all_voting$election_date == "1996-03-02" & all_voting$division == "BASS" & all_voting$surname == "smith" & all_voting$name == "S Smith"] <- "SilviaSmith"
all_voting$surname[all_voting$election_date == "1998-10-03" & all_voting$division == "BASS" & all_voting$surname == "smith" & all_voting$name == "Harvey Smith"] <- "HarveySmith"
all_voting$surname[all_voting$election_date == "1998-10-03" & all_voting$division == "BRADDON" & all_voting$surname == "thompson" & all_voting$name == "Gavin Thompson"] <- "GavinThompson"
all_voting$surname[all_voting$election_date == "1998-10-03" & all_voting$division == "BRADDON" & all_voting$surname == "thompson" & all_voting$name == "Thomson"] <- "GavinThompson"
all_voting$surname[all_voting$election_date == "1998-10-03" & all_voting$division == "FADDEN" & all_voting$surname == "smith" & all_voting$name == "Fay Smith"] <- "FaySmith"
all_voting$surname[all_voting$election_date == "1998-10-03" & all_voting$division == "FADDEN" & all_voting$surname == "smith" & all_voting$name == "F Smith"] <- "FaySmith"
all_voting$surname[all_voting$election_date == "1998-10-03" & all_voting$division == "HERBERT" & all_voting$surname == "lindsay" & all_voting$name == "Ted Lindsay"] <- "TedLindsay"
all_voting$surname[all_voting$election_date == "1998-10-03" & all_voting$division == "HERBERT" & all_voting$surname == "lindsay" & all_voting$name == "T Lindsay"] <- "TedLindsay"
all_voting$surname[all_voting$election_date == "1998-10-03" & all_voting$division == "HERBERT" & all_voting$surname == "smith" & all_voting$name == "Althea Smith"] <- "AltheaSmith"
all_voting$surname[all_voting$election_date == "1998-10-03" & all_voting$division == "HERBERT" & all_voting$surname == "smith" & all_voting$name == "A Smith"] <- "AltheaSmith"
all_voting$surname[all_voting$election_date == "1998-10-03" & all_voting$division == "MAYO" & all_voting$surname == "coombe" & all_voting$name == "Anthony Coombe"] <- "AnthonyCoombe"
all_voting$surname[all_voting$election_date == "2001-11-10" & all_voting$division == "BOWMAN" & all_voting$surname == "smith" & all_voting$name == "Fay Smith"] <- "FaySmith"
all_voting$surname[all_voting$election_date == "2001-11-10" & all_voting$division == "NEWCASTLE" & all_voting$surname == "williams" & all_voting$name == "Harry Williams"] <- "HarryWilliams"
all_voting$surname[all_voting$election_date == "2001-11-10" & all_voting$division == "NEWCASTLE" & all_voting$surname == "williams" & all_voting$name == "H Williams"] <- "HarryWilliams"
all_voting$surname[all_voting$election_date == "2004-10-09" & all_voting$division == "GWYDIR" & all_voting$surname == "anderson" & all_voting$name == "Michael Anderson"] <- "MichaelAnderson"
all_voting$surname[all_voting$election_date == "2004-10-09" & all_voting$division == "GWYDIR" & all_voting$surname == "anderson" & all_voting$name == "M Anderson"] <- "MichaelAnderson"
all_voting$surname[all_voting$election_date == "2004-10-09" & all_voting$division == "LOWE" & all_voting$surname == "murphy" & all_voting$name == "Shireen Murphy"] <- "ShireenMurphy"
all_voting$surname[all_voting$election_date == "2004-10-09" & all_voting$division == "LOWE" & all_voting$surname == "murphy" & all_voting$name == "S Murphy"] <- "ShireenMurphy"
all_voting$surname[all_voting$election_date == "2004-10-09" & all_voting$division == "PEARCE" & all_voting$surname == "mclean" & all_voting$name == "Donella Mclean"] <- "DonellaMclean"
all_voting$surname[all_voting$election_date == "2004-10-09" & all_voting$division == "PEARCE" & all_voting$surname == "mclean" & all_voting$name == "D Mclean"] <- "DonellaMclean"
all_voting$surname[all_voting$election_date == "2004-10-09" & all_voting$division == "WARRINGAH" & all_voting$surname == "kelly" & all_voting$name == "Edward Kelly"] <- "EdwardKelly"
all_voting$surname[all_voting$election_date == "2004-10-09" & all_voting$division == "WARRINGAH" & all_voting$surname == "kelly" & all_voting$name == "E Kelly"] <- "EdwardKelly"
all_voting$surname[all_voting$election_date == "2007-11-24" & all_voting$division == "BANKS" & all_voting$surname == "nguyen" & all_voting$name == "Don Nguyen"] <- "DonNguyen"
all_voting$surname[all_voting$election_date == "2013-09-07" & all_voting$division == "FLINDERS" & all_voting$surname == "clark" & all_voting$name == "David Clark"] <- "DavidClark"
all_voting$surname[all_voting$election_date == "2013-09-07" & all_voting$division == "FLINDERS" & all_voting$surname == "clark" & all_voting$name == "D Clark"] <- "DavidClark"
all_voting$surname[all_voting$election_date == "2013-09-07" & all_voting$division == "WAKEFIELD" & all_voting$surname == "musolino" & all_voting$name == "Tony Musolino"] <- "TonyMusolino"
all_voting$surname[all_voting$election_date == "2013-09-07" & all_voting$division == "WAKEFIELD" & all_voting$surname == "musolino" & all_voting$name == "?"] <- "TonyMusolino"
all_voting$surname[all_voting$election_date == "2015-09-19" & all_voting$division == "CANNING" & all_voting$surname == "smith" & all_voting$name == "Greg Smith"] <- "GregSmith"
all_voting$surname[all_voting$election_date == "2015-09-19" & all_voting$division == "CANNING" & all_voting$surname == "smith" & all_voting$name == "G Smith"] <- "GregSmith"
all_voting$surname[all_voting$election_date == "2016-07-02" & all_voting$division == "FORDE" & all_voting$surname == "spain" & all_voting$name == "Sally Spain"] <- "SallySpain"
all_voting$surname[all_voting$election_date == "2016-07-02" & all_voting$division == "WRIGHT" & all_voting$surname == "smith" & all_voting$name == "Rod Smith"] <- "RodSmith"
all_voting$surname[all_voting$election_date == "2016-07-02" & all_voting$division == "WRIGHT" & all_voting$surname == "smith" & all_voting$name == "R Smith"] <- "RodSmith"




all_voting_firsts <- all_voting %>% 
	filter(vote_redistribution_round == 1) %>% 
	select(name, election_date, division, surname, party)

# all_voting_firsts <- all_voting_firsts %>%
# 	group_by(election_date, division, surname) %>%
# 	summarise(count = n()) %>%
# 	filter(count == 2)



all_voting_others <- all_voting %>% 
	filter(vote_redistribution_round != 1)

all_voting_others_joined <- all_voting_others %>% 
	left_join(all_voting_firsts, by = c("election_date", "division", "surname")) 


### Start debris
# all_voting_others_joined %>% 
# 	group_by(election_date, division, name, full_name) %>% 
# 	summarise(count = n())


# all_voting_others$conc <- paste0(all_voting_others$division, all_voting_others$surname, all_voting_others$election_date)
# all_voting_others_joined$conc <- paste0(all_voting_others_joined$division, all_voting_others_joined$surname, all_voting_others_joined$election_date)

# all_voting_others_joined$full_name <- NULL

# all_voting_others_joined[!all_voting_others_joined$conc%in%all_voting_others$conc,]
# 
# anti_join(all_voting_others_joined, all_voting_others, by = c("election_date", "division", "surname"))
# 
# write_csv(all_voting_others, "small.csv")
# write_csv(all_voting_others_joined, "big.csv")

### End debris

# Only need this when trying to identify the wrong ones
# all_voting_others <- all_voting_others %>% 
# 	filter(is.na(name.y)) %>% 
# 	filter(surname != "exhausted")
# rm(all_voting_others)

all_voting_others_joined <- all_voting_others_joined %>% 
	rename(original_name = name.x, full_name = name.y, party = party.y) %>% 
	select(-party.x)

names(all_voting_others_joined)

all_voting_firsts <- all_voting %>% 
	filter(vote_redistribution_round == 1) %>% 
	rename(original_name = name) %>% 
	mutate(full_name = original_name)

names(all_voting_firsts)

all_voting <- rbind(all_voting_firsts, all_voting_others_joined)

all_voting <- all_voting %>% 
	arrange(election_date, state, division, vote_redistribution_round, full_name)

rm(all_voting_firsts, all_voting_others, all_voting_others_joined)


# Fix division names
all_voting <- all_voting %>% 
	mutate(division = str_to_sentence(division))

# Add a matching name
all_voting <- all_voting %>% 
	mutate(full_name_for_matching = str_to_lower(full_name),
				 full_name_for_matching = str_remove(full_name_for_matching, "'")
				 )



#### Identify whether 2PP or not ####
# 2PP is the final vote count after preferences when there are only two candidates left. Usually it's ALP vs whatever is the major conservative party, but sometimes it's not.
# Identify 2PP or closest approximation
all_voting <- all_voting %>%
	group_by(election_date, division) %>%
	mutate(
		twoPP = max(vote_redistribution_round),
		twoPP = if_else(vote_redistribution_round == twoPP, 1, 0),
		twoPP = if_else(election_date < ymd("1918-12-14"), 0, twoPP) # Preferences only start in 14 December 1918, so anything before that is certainly not a 2PP.
	) %>%
	ungroup()

all_voting$twoPP %>% sum()

# The above will fail is there is only... 
# Sometimes they haven't actually worked out the 2PP counts
# twoPP = if_else(vote_redistribution_round == 1, 0, twoPP) # Sometimes, especially in the earlier data, they haven't actually worked out a 2PP count - all we have is a first-preferences vote. 

# Get the number of candidates by election, by division, by vote redistribution round.
all_voting <- all_voting %>%
	group_by(election_date, division, vote_redistribution_round) %>%
	mutate(
		number_of_candidates = n()
	) %>%
	ungroup()


# Identify whether the winner or not
all_voting <- all_voting %>%
	group_by(election_date, division) %>%
	mutate(
		winnerDummy = if_else(twoPP == 1, max(votes_count), as.integer(0)),
		winnerDummy = if_else(winnerDummy == votes_count, winnerDummy, as.integer(0)),
		winnerDummy = if_else(winnerDummy != 0, as.integer(1), as.integer(0))
	) %>%
	ungroup()

all_voting <- all_voting %>%
	group_by(election_date, division) %>%
	mutate(
		winnerDummyOld = if_else(election_date < ymd("1918-12-14"), max(votes_count), as.integer(0)),
		winnerDummyOld = if_else(winnerDummyOld == votes_count, winnerDummyOld, as.integer(0)),
		winnerDummyOld = if_else(winnerDummyOld != 0, as.integer(1), as.integer(0))
	) %>%
	ungroup()

all_voting <- all_voting %>%
	mutate(
		winnerDummy = if_else(election_date < ymd("1918-12-14"), winnerDummyOld, winnerDummy)
	) %>%
	select(-winnerDummyOld)

# Test if we've identified someone who isn't capitalised
all_voting_hmm <- all_voting %>%
	mutate(
		winnerCheck = if_else(winnerDummy == 1 & capitalised != 1, 1,0)
	) %>% 
	filter(winnerCheck == 1)






#### Save ####
# Fix the ordering
all_voting <- all_voting %>% 
	arrange(election_date, state, division, vote_redistribution_round, full_name)
# Save the data
write_csv(all_voting, "outputs/voting_data.csv")


#### Debris ####
# Some basic maps for fun.
all_voting %>%
	ggplot() +
	geom_bar(aes(x = party)) +
	coord_flip()

all_voting %>%
	filter(!votes_count == -1) %>%
	ggplot() +
	# geom_bar(aes(x = votes_count)) +
	geom_density(aes(x = votes_count))


# Some intersting questions:
# Who got the most first preference votes ever?
# Who got the least first preference votes and was still elected?
# Is there a difference between men and women?



# Parties
#
# * ALP - Australian Labor Party
# * Lib - Liberal Party of Australia
# * LM - Liberal Movement
# * NA - National Alliance
# * CP - Country Party
# * CPA - Communist Party of Australia
# * Lib - Liberal Party
# * F&S - Farmers and Settlers
# * Nat - Nationalist Party
# * PPU - Primary Producers Union
# * AD - Australian Democrats
# * Grn - Australian Greens
# * NPA - National Party of Australia
# * FT - Free Trade
# * Lab - Labour
# * Prot - Protectionist
# * CP - Country Party of Australia
# * DLP - Democratic Labor Party
# * Lib – Liberal Party
# * FFP - Family First Party
# * ON - One Nation
# * LD - Liberal Democrat
# * SL - State Labor Party (NSW)
# * OPA - One People for Australia
# * UAP – United Australia Party