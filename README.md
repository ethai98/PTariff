# End-to-end text data extraction from Trade Agreement PDFs

To access raw trade agreement tariff schedule data, download the zip file from here: https://www.dropbox.com/scl/fo/vnhxs3kzof5xvby6aa38g/AB7rQ8CciCpZUrlsPyaNg_w?rlkey=sm2wppt08kmvept0riu424o0s&dl=0

**Challenge:** My dissertation requires tariff-line treatment from international trade agreements. However, there is no clean, on-the-shelf database that codes individual tariff lines from trade agreements. Collaborating with Elisabeth Van Lieshout, we created an end-to-end data pipeline that extracts tariff schedules in thousands of pages of non-standardized PDFs, manually codes each staging category according to the legal text, and merges country-level and product-level information together into a unified database that we call _PTariff_. 

**Accomplishment:** By extracting tabular data using Tabula-py, we saved thousands of dollars and hundreds of hours by avoiding the need to hire an OCR company to complete a critical step in the process. 

Generally, the steps are as follows:

PDF → Tabula & Clean → CSV → Meta Data → Manual Coding of Staging Categories → Unified Dataset. 

## Extraction
1. (PTariff/code/1 tabula.ipynb): Utilizes the Tabula-py library to programmatically identify and extract tabular data from PDF schedules.
2. (PTariff/code/2 clean tabula outputs.ipynb): A Python-based cleaning layer that handles the "noise" inherent in PDF-to-CSV conversion (e.g., misaligned rows, broken strings, and artifacts). This script cleans up messy outputs arising from Tabula. 
2.5. Manual step: make sure that the output is clean, make corrections if necessary.

**Output:** a cleaned 3-column sheet, containing code, baserate, and category. Code refers to the tariff line code used by the country, where the first six digits are the internationally harmonized system of product identification. Baserate refers to the tariff line's base rate. Category refers to the rule for the specific tariff line. Categories are often coded as characters (e.g., "A", "B"), where one must refer to the trade agreement's main text to understand when, how, and whether the tariff line is to be liberalized. 

## Metadata and Manual Coding
3. (PTariff/code/3 metadata.R): Creates metadata and category sheets. The metadata sheet contains information about the treaty members, such as the reporter and partner, as well as information about the agreement, such as the HS version used and the treaty signature and ratification years. The tariff schedule is structured as reporter-partner because it is the reporter country's tariff treatment of imports from the trade partner. The category sheet is created by taking all unique categories from the schedule sheet, enabling manual coding. 

3.5 Manual steps: I manually code each unique staging category by hand, referring to the FTA main text to make a determination on whether the tariff line with the category is (1) reduced, (2) eliminated, and if so, whether it is (3) immediately eliminated. Next, I code the (4) duration of the phaseout in years and (5) the means of reduction (linear or back-loaded). If the category backloads the phaseout, meaning there is a momentary pause prior to reduction, I also code (6) the duration of the initial pause. 

## Merge and Harmonization
Note on data: The cleaned individual tariff schedules (with metadata, schedule, and category sheets) are located in PTariff/cleaned_csv_schedule_data folder, which is cleaned with the last script below:

4. (PTariff/code/4 clean schedule (8-digits)): Runs various functions in /code/functions to merge metadata, schedule, and category sheets into ONE product-level dataset.

*Note: this is for demonstration purposes. Please email ethai@ucsd.edu for supplementary files for some scripts to run. 
