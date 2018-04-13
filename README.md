# USIndustrialWater
Rshiny app of Historical Trends and Projections of U. S. Industrial Water Use

The R shiny app was completed a portion of our Master’s Project, evaluating the historical trends and projection of U. S. industrial water use. This work highlights the current trends and the need for better reporting and recording of industrial water use. This app acts to promote data transparency and visualization tools in planning for the future. The full written report can be found here: ……..d the working R shiny app can be viewed here:…..

This collection of files includes:

 1. R Scripts:
  - .ui and .server files for running the app
  - Install package list if you are downloading all of the data

 2.	Shapefiles of Texas, North Carolina, and IN counties

  - These were originally download from the American Community Survey (ACS)- Census Bureau and are used to create a choropleth maps of         the number of facilities and water use over time

 3.	Datasets for all three states acquired from the state agencies in charge of collecting and recording water data. 

  - These amount of data in each dataset, including the time frame and type of data collected differs between state. Additionally, some        of the files were too large to upload to GitHub, so unused columns were deleted

 4.	Water use projection files were also collected
   - An ARIMA model was run for Texas and a Seasonal ARIMA model was used to create the projections for Indiana and North Carolina
   - The projections were created in separate R scripts not included, but the files themselves include the water use projection along            with the error associated with 95% confidence interval. 
   
Many thanks to Dr. Martin Doyle for his guidance throughout this project. Also, thanks Dr. John Fay, Dr. Lauren Patterson, John Little, and Dr. Joel Herndon for their help with troubleshooting the R shiny app and Christopher Collins and Dr. Matt Ross for their help with putting the app online.     
