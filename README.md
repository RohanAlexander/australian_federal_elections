# Australian Federal Election Results (1901 - 2021)

Voting data for Australian federal elections (1901 - 2021), taken from Adam Carr's website and converted into a CSVs.

If you use this dataset then please don't forget to cite Adam Carr's website: http://psephos.adam-carr.net/countries/a/australia/.

The first step is to download all of Adam's txt files. This is done with 'scripts/get_HoR_voting.R'. (I've not included Adam's txt files here - if you want them please go to his website.) After this, Adam's files need to be cleaned and transformed in a CSV. This done with 'scripts/make_text_data_tabular.R'. The output of this is a CSV that can be analysed: 'outputs/voting_data.csv'.

21 December 2021: There are a few files in Adam's dataset that don't work and I need to follow up with him about:

- http://psephos.adam-carr.net/countries/a/australia/1913/kalg1913.txt
- http://psephos.adam-carr.net/countries/a/australia/1913/adel1913.txt
- http://psephos.adam-carr.net/countries/a/australia/1943/frem1940.txt
- http://psephos.adam-carr.net/countries/a/australia/1943/wimm1940.txt
- http://psephos.adam-carr.net/countries/a/australia/1943/hent1940.txt
- http://psephos.adam-carr.net/countries/a/australia/1966/capr1966.txt
