# End-to-end text data extraction from Trade Agreement PDFs

To access raw trade agreement tariff schedule data, download the zip file from here: https://www.dropbox.com/scl/fo/vnhxs3kzof5xvby6aa38g/AB7rQ8CciCpZUrlsPyaNg_w?rlkey=sm2wppt08kmvept0riu424o0s&dl=0

PTariff/code/1 tabula.ipynb:               extracts the tables from the PDFs into csv files.

PTariff/code/2 clean tabula outputs.ipynb: clean up the outputs from tabula, which is often messy.

Manual step: make sure that the output is clean, make corrections if necessary.

PTariff/code/3 metadata.R:                 create metadata information using the DESTA database. 

Manual step: analyze trade agreement text to interpret and code staging categories. 

The cleaned individual tariff schedules (with metadata, schedule, and category sheets) are located in PTariff/cleaned_csv_schedule_data folder, which is cleaned with the last script below:

PTariff/code/4 clean schedule (8-digits):  run various functions in /code/functions to merge metadata, schedule, and category sheets into product-level dataset, where each tariff line's treatment is coded. Output one dataset with all trade agreements. 

*Note: this is for demonstration purposes. Please email ethai@ucsd.edu for supplementary files for certain script to run. 
