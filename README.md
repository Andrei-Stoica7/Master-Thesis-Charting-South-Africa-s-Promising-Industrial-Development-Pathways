# Master-Thesis-R-Scripts
A repository code and outputs for the Maser Thesis "Charting South Africa’s Promising Industrial Development Pathways: Identification and evaluation of promising sectors for South Africa using economic complexity-relatedness analysis".

## Usage
1. Downloaded raw data is laready provided from the data sources specified below
2. Specify the path to your raw data in RAW_DATA_PATH in the master_script_zaf.R script
3. Specify the baci_version, HS_version and HS_digits, as well as the years for which you have BACI data in the data_processing.R script
4. Run the master_script_zaf.R script, which will:
i. Create the directory and import the BACI trade data
ii. Run modular analysis scripts

Note that the master_script_zaf.R script serves as the principle analysis tools which uses modular scripts also provided in the same folder. Additionally, the outputs are already provided and running any script will overwrite the current outputs.

## Data Sources
* CEPII BACI international trade data (HS07, 2007-2024)(https://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37)
* UN HS product descriptions (https://unstats.un.org/unsd/classifications/Econ)
*   Downloaded "All HS codes and descriptions" xlsx file for product descriptions for all HS digits
* UN Gini coefficients (https://data.un.org/Data.aspx?d=WDI&f=Indicator_Code%3ASI.POV.GINI)

## Citation
Stoica, A. (2026). Master Thesis R Scripts [Source code]. https://github.com/Andrei-Stoica7/Master-Thesis-Charting-South-Africa-s-Promising-Industrial-Development-Pathways
