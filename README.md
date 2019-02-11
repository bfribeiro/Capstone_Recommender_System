# Capstone_Recommender_System

This project's goal was to build a Movie Recommendation System, based on the rating a specific user would give to a specific movie, and it was a requirement to achieve a passing grade in the Data Science Professional Certificate Program, provided by Harvard University through edX.

There are 6 scripts, coded in R language, in this repository:

<b>1. dl_script.r</b>: Contains the script for downloading and building the dataset;<br>
<b>2. data_wrangling.r</b>: Contains all the code used to put the dataset in the format we wanted; <br>
<b>3. data_exploration.r</b>: Contains the code for the exploration of the dataset, prior to building the model; <br>
<b>4. modelling.r</b>: This script provides the steps used to build the model, including the regularization process; <br>
<b>5. validation.r</b>: The application of the model against the "real-world" dataset;<br>
<b>6. support_data_markdown.r</b>: This script was used to build and save the tables that were used in our .Rmd file. This was done to speed up the process of creating our pdf file.


There's, also, a .Rmd file, which is the markdown file used to build the pdf report in the repository.

Note that, you'll need to create 3 folders in your project folder. Since all the data created is saved in specific folders to make it easier to load it. Specially the markdown file, which possess a Support Data folder, to make it's processing faster. The folders are: 

<b>1. data</b>: All the datasets you create will be stored in this folder;<br>
<b>2. img</b>: If you want to save the plots crated, I suggest saving them into this folder; <br>
<b>3. support_data</b>: The tables created to make easier to create the pdf report, or html if you like, was stored here.

The folders above are just suggestions so you can choose whatever name you like. Just remember to change it in the scripts to make it work fine. However, the scripts in this repository use data and support_data folders as references.
