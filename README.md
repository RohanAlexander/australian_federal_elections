# Australian Federal Election Results (1901 - 2018)
Voting data for Australian federal elections (1901 - 2018), taken from Adam Carr's website and converted into a CSVs.

Remember to cite Adam Carr's website: http://psephos.adam-carr.net/countries/a/australia/.

The first step is to download all of Adam's txt files. This is done with scripts/get_HoR_voting.R. (I've not included Adam's txt files here - if you want them please go to his website.) After this, Adam's files need to be cleaned and transformed in a CSV. This done with scripts/make_text_data_tabular.R. The output of this is a CSV that can be analysed: outputs/voting_data.csv.