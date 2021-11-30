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




