This is an HTML interactive application compiled in R that accompanies the text:
Simple algorithms to display ignorance maps of raw distributional data accessed from species observation databases: the Swedish LifeWatch implementation

This interactive application exemplifies the use of three algorithms to map the ignorance (i.e. lack of sampling effort) found in the observations accessed through the Swedish LifeWatch biodiversity analysis portal.

To run it you will need R <http://www.r-project.org/> with the following packages installed: shiny, raster, rgdal, and maptools.
       
       install.packages(c("shiny", "raster", "rgdal", "maptools"))

All data required to run this script is provided in the ZIP file. 
The structure of this folder should be kept as is.

Execute the following script in R to run the interactive application.

	require(shiny)
	runApp("~/SLWapp") # where ~ indicates the path of the folder. To see the R code include the command ',display.mode = "showcase"'
	runApp("~/SLWapp",display.mode = "showcase") # Use this command to see the R code  



### Authors and Contributors
Application developed by Alejandro Ruete in Dec 2014.
