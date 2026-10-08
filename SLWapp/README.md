Simple algorithms to display ignorance maps of raw data accessed from species observation databases
======================================================================================================================
The Swedish LifeWatch implementation

This interactive application exemplifies the use of three different algorithms to map the ignorance (i.e. bias and lack of sampling effort) present in the observations accessed through the Swedish LifeWatch biodiversity analysis portal <a href="http://www.swedishlifewatch.se">www.swedishlifewatch.se</a>.

You can test it <a href="https://aleruete.shinyapps.io/SLWapp/">here</a> or run it locally in your computer. To run it locally you will need to install <a href="http://www.r-project.org/">R</a> and install the following packages: shiny, shinythemes, and terra.

       install.packages(c("shiny", "shinythemes", "terra"))

Spatial data are read and processed with `terra`. The Sweden outline retains its
SWEREF99 TM coordinate reference system from the supplied shapefile, and the
existing GeoTIFFs are used without resampling. Grids without embedded CRS
metadata are assigned SWEREF99 TM (EPSG:3006), matching the Sweden map.

The old Packrat snapshot has been removed. Install the packages above in your
current R library. Before deploying, regenerate the deployment manifest using
`rsconnect::writeManifest(appDir = ".")` from the application folder so package
versions and file checksums match your environment. The included manifest was
regenerated for this migration.

Execute the following script in R to run the interactive application.

       require(shiny)
       shiny::runGitHub(repo="IgnoranceMaps2", username="alejandroruete", subdir="SLWapp")

Alternatively, download the files and run the following scripts.

       runApp("~/SLWapp") # where ~ indicates the path of the folder.
       runApp("~/SLWapp", display.mode = "showcase") # Use this command to see the R code


### Running the Application
Examples are provided for seven reference taxonomic groups (i.e. Amphibians, Birds, Butterflies, Land Mammals, Harvestmen, Dragonflies, and Vascular Plants) as groups with different sampling effort intensity and extension as well as different number of amateur observers reporting observations to the database. For each group we show examples of a common (or widely distributed) and a rare (or locally distributed) species. Note that in many cases common species are not reported as enthusiastically as more interesting species are (in Sweden, a moose is not as frequently reported as hedgehogs are). Therefore, one can expect that for common species the observations are very scattered and they do not cover the whole expected range of the species.

<b>Tabs (Maps, Data plots, and Read me)</b>

You are now reading under the &ldquo;Read me&rdquo; tab. The data is shown the form of &ldquo;Maps&rdquo; and &ldquo;Data plots&rdquo;. Four maps of Sweden with a resolution of 10 x 10 km are plotted under the tab &ldquo;Maps&rdquo; displaying:

1) the number of observations per pixel for the reference taxonomic group selected,

2) the ignorance scores per pixel,

3) an estimate of pseudo-absence (henceforth ps.absences) for the selected target species. Ps.absences are estimated with the same algorithms used to produce the ignorance maps. Be aware that the results of the algorithms for a single species have a different interpretation than for a reference target group. For individual target species the lack of observations could represent a true absence or lack of observers. To separate true absences from the lack of observers we could weight ps.absence estimates with the ignorance map as it is shown in the next map. Also, be aware that especially for common species or species that are not likely to be reported as frequently as they are seen, these estimates of ps.absences are not reliable because the raw data is not reliable. 
<br>Disclaimer: the pseudo-absence map is only an exercise to explore the use of the ignorance map.</br>

4) the presence map (P = 1- ps.absences; i.e. scaled observations). This is an example of the use of ignorance maps to weight and mask different estimates for target species. In this case, the black to white scale represents the scaled observations or the knowledge we have on the presence of the species. On top, a transparent layer shows areas where a minimum certainty for ps.absences or presence is achieved after multiplying 1-ignorance (i.e. certainty) by the ps.absences (red) or presence (green). Then, areas where there is low ignorance but the target species has not been found are shown in red, while areas where the target species has been found but a minimum certainty is required are shown in green.

The outline in those maps is a 10km buffer around the Swedish land surface.

Under the &ldquo;Data plots&rdquo; tab you find:

1) a density plot of the number of observations per
grid cell,

2) a species discovery plot and

3) curves of the transformations of the number of observation into ignorance scores (note that
this later plot is reactive to the value set for <i>O</i><sub>0.5</sub>).

<b>Options</b>

On the left panel you find options for the Reference taxonomic group and Target species to be displayed. First, you can opt to use the raw observations as a measure of sampling effort or the Observation Index, a sampling effort relative to the number of species sampled in a particular grid cell. On each subpanel you can set the algorithm can be set and O<sub>0.5</sub> (the number of observations that are enough to reduce the ignorance score to 0.5) for the Half-ignorance algorithm. For target species, a Step algorithm is added where any observation number below O<sub>0.5</sub> sets the ignorance score to
0. Else the Step algorithm is equal to the Half-ignorance algorithm.

### Authors and Contributors
Application developed by Alejandro Ruete in Dec 2014. Updated in August 2016
DOI: dx.doi.org/10.5281/zenodo.17593
### Licence GNU v.3

### Migration validation

Run `Rscript --vanilla tests/migration.R` from the application folder. This checks
all 28 grids, compares calculations with numeric formulas for all 336 combinations
of group, species, observation index and algorithms, and renders both plot tabs.
It also checks certainty-slider endpoints and confirms that the retired spatial
packages are not loaded.

If `terra` warns about an incompatible `proj.db` from another installation
(for example PostgreSQL), clear the inherited projection paths for the current
R session before loading the app: `Sys.unsetenv(c("PROJ_LIB", "PROJ_DATA"))`.
This does not change your system settings.
