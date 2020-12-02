:- consult(synth_rpc).
:- consult(wrangling).
% This part only does the interfacing between the (python) components and prolog.
% Here's a test run: 
% data_source_path(test_tables, F), synth_load_spreadsheet(F,S), synth_detect_tables(S,TL), member(table(S, T) , TL), synth_get_field_types(T, TF) 

data_source(test_tables, spreadsheet).
data_source_path(test_tables, '/mnt/e/code/prolog/somethingusefultosynthlog/test_tables.csv').


data_source(nurse_csv, spreadsheet).
data_source_path(nurse_csv, '/mnt/e/code/prolog/somethingusefultosynthlog/nurse.csv').