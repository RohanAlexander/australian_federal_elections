voting_data <- read_csv("outputs/voting_data.csv",
												col_types = cols(
													division = col_character(), 
													state = col_character(), 
													division_num_enrolled = col_integer(),
													division_num_voted = col_integer(),
													division_percent_voted = col_double(),
													original_name = col_character(), 
													party = col_character(), 
													vote_redistribution_round = col_double(),
													votes_count = col_integer(),
													votes_share = col_double(),
													swing = col_double(),
													redistributed_vote_count = col_integer(), 
													redistributed_vote_share = col_double(), 
													txt_file = col_character(), 
													election_date = col_date(format = ""),
													starred = col_logical(),
													plus = col_logical(),
													surname = col_character(), 
													capitalised = col_logical(),
													full_name = col_character()
												))



# 2PP is the final vote count after preferences when there are only two candidates left. Usually it's ALP vs whatever is the major conservative party, but sometimes it's not.
# Identify 2PP or closest approximation
voting_data <- voting_data %>%
	group_by(election_date, division) %>%
	mutate(
		twoPP = max(vote_redistribution_round),
		twoPP = if_else(vote_redistribution_round == twoPP, 1, 0),
		twoPP = if_else(election_date < ymd("1918-12-14"), 0, twoPP) # Preferences only start in 14 December 1918, so anything before that is certainly not a 2PP.
	) %>%
	ungroup()

voting_data$twoPP %>% sum()

# The above will fail is there is only... 
# Sometimes they haven't actually worked out the 2PP counts
# twoPP = if_else(vote_redistribution_round == 1, 0, twoPP) # Sometimes, especially in the earlier data, they haven't actually worked out a 2PP count - all we have is a first-preferences vote. 

# Get the number of candidates by election, by division, by vote redistribution round.
voting_data <- voting_data %>%
	group_by(election_date, division, vote_redistribution_round) %>%
	mutate(
		number_of_candidates = n()
	) %>%
	ungroup()






