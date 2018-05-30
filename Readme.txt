README_beta1
--------
ILearnOD
Written by Ali Abbasi
Updated 5/2/2018

For research use only

--------
INPUT DATA
1) Death records
(example can be found in od_deaths_example.xlsx)
Need to have 1 column that uniquely identifies each record and text columns that contain the death certificate

2) Drug names
(Example can be found in drugs.csv)
Needs to have three columns

name — version of a drug name
consensus.name - consensus version of the drug name
class - class of drug

Note: If a word should be excluded from analysis (e.g. ‘alcoholism’), the columns consensus.name and class should be set to “trash”



————
Running the program
1) Specify
	working directory where all files are found
	name of file with death data
	name of file with drug names
	Name of columns that contain the text on the death certificate


2) Run code once by clicking on "Source". It should terminate with the message "Error: No input drug spellings file specified."

3) Look at the file drug_spellings_OUTPUT.csv
	For each “match”, decide whether this is truly a misspelling of the drug or not.
	If it is a true misspelling leave the value of the "use" column equal to 1
	If it is a false match, change the value of the "use" column to 0.
	Save and Rename the file drug_spellings_use.csv
	
4) Run code again. The file result.csv is now generated with the output.
	Each column indicates whether a drug/ drug class was involved in the overdose