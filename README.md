# Active transport accessibility to potential mobility hubs in Salzburg province

This repository presents a workflow to assess and rank the active transport accessibility to potential mobility hubs. It computes accessibility as the share of the population inside the catchment area of the hub that can reach the hub using only streets which are suitable for the corresponding active transport mode, without taking a unacceptable detour. It then shows how different definitions of what a suitable street is, and different threshold for detour acceptance, influence the computed accessibility levels. The workflow is presented by means of an interactive notebook.

*DISCLAIMER: The potential mobility hub locations used in the examples are not part of any concrete implementation plans, but chosen as examples to showcase the workflow*

## Requirements

### Software

The interactive notebook can be started through a Docker container in which all required software dependencies are installed. This means you only have to have Docker installed. See [here](https://docs.docker.com/engine/install/) for the installation instructions. In addition, you will need [Git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git) to clone this repository on your local disk.

### Data

The notebook requires pre-processed input data in RData format, meaning that they can be directly loaded as R objects into an R workspace. These data are stored in the [data folder](data) of this repository and contain for each hub and each active transport mode (i.e. bike and walk) the following information:

| Layer | Description |
| --- | --- |
| `hub`  | The name and location of the hub |
| `network`  | The complete street network around the hub |
| `households` | All household locations in proximity of the hub |
| `suitable_networks` | The bikeable/walkable networks around the hub for different bikeability/walkability thresholds |
| `connected_households` | The household locations with bikeable/walkable access to the hub for different bikeability/walkability thresholds and different detour factors |

The source data from which the pre-processed data are inferred are listed below. Some of them are proprietary. Data pre-processing can be reproduced with the script `process.R` in directory [scripts](scripts), given you have the source data available. Please contact mobilitylab@plus.ac.at for more information.

| Data | Description | Source |
| --- | --- | --- |
| `hubs.csv` | The examplary hub locations in Salzburg province | Provided by Research Studio iSpace |
| `salzburgerland-netascore-20240123.gpkg` | Street network of the Salzburg province with computed bikeability and walkability indices for each street | Created using the [NetAScore Toolbox](https://github.com/plus-mobilitylab/netascore/tree/v1.0.1), version v1.0.1. The mode profiles for respectively the bike and walk modes can be found in directory [profiles](profiles) |
| `salzburgerland-population.gpkg` | Population data of the Salzburg province for the year 2016 on a 100x100m grid | Purchased from Statistics Austria |
| `ADRESSE.csv` | Address locations in Austria for the year 2021 | Openly available through [this link](https://data.bev.gv.at/download/Adressregister/Archiv_Adressregister/Adresse_Relationale_Tabellen_Stichtagsdaten_20211001.zip) |

## Usage

### Running the notebook through a Docker container

The interactive notebook presenting the analysis can be started using Docker. This repository includes a Dockerfile from which a container can be build that has all required software dependencies installed. Running that container will run the interactive notebook on port 3838.

First, clone this repository and navigate to the cloned directory:

```bash
git clone https://github.com/plus-mobilitylab/accmobhub-salzburgerland.git
cd accmobhub-salzburgerland
```

Then, build the Docker image:

```bash
docker build -t accmobhub .
```

Then, run a Docker container using that image:

```bash
docker run --rm -p 3838:3838 accmobhub
```

Finally, navigate in your browser to http://localhost:3838/. Here you should see the interactive notebook.

### Running the notebook directly in RStudio

Alternatively, if you have R and RStudio installed, you can open the R project in RStudio and run the notebook document. You will need to first install the requirements:

```r
install.packages("rmarkdown")
install.packages("knitr")
install.packages("shiny")
install.packages("tidyverse")
install.packages("DT")
install.packages("here")
install.packages("leaflet")
install.packages("plotly")
install.packages("units")
install.packages("sf")
install.packages("sfnetworks")
```

And unzip the pre-processed data archive:

```r
unzip("data/data.zip")
```

### Exploring the web app

Instead of running the interactive notebook yourself, you can also just explore a hosted version online: https://plusmobilitylab.shinyapps.io/accmobhub-salzburgerland/ To keep the size of the app managable, the content displayed there is a subset of the full content. It only shows bicycle accessibility, for a limited set of potential hub locations.

## Acknowledgements

Parts of this research were funded by a commission from [Research Studios Austria - iSpace](https://ispace.maps.arcgis.com/home/index.html).

## License

This project is licensed under the MIT license. For details please see [LICENSE](LICENSE).

## References

For more background on the underlying methodologies, see our [conference paper](https://doi.org/10.5194/agile-giss-5-48-2024).
