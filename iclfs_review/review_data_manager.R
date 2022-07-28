# HEADER ----------------------------------------------------------------------
#
# Title: Review Data Manager
# Description:
#
#
#
# Authors:      Hugo Tameirao Seixas
# Contact:      tameirao.hugo@gmail.com
# Date:         2022-07-14
#
# Notes:
#
#
#
#
#
# LIBRARIES -------------------------------------------------------------------
#
library(fs)
library(magrittr)
library(gt)
library(glue)
library(tidyverse)
#
# OPTIONS ---------------------------------------------------------------------
#
#
# CREATE LITERATURE SEARCH METADATA -------------------------------------------

bind_rows(
  list(
    database = "Scopus",
    database_url = "https://www.scopus.com/",
    search_string = glue(.sep = " ",
      "TITLE-ABS-KEY(",
      "iclf OR iclfs OR crop-livestock-forestry OR crop-livestock-forest",
      "OR agrosilvipastoral OR ilpf OR lavoura-pecuária-floresta OR",
      "agrosilvipastoril) AND PUBYEAR > 1999"
    ),
    oldest_publication_date = 2007,
    newest_publication_date = 2022
  ),
  list(
    database = "Embrapa",
    database_url = "https://www.bdpa.cnptia.embrapa.br/consulta/busca",
    search_string = glue(.sep = " ",
      '((ICLF) OR (iclfs) OR (crop-livestock-forestry)',
      'OR (crop-livestock-forest) OR (agrosilvipastoral)',
      'OR (ilpf) OR (lavoura-pecuária-floresta) OR (agrosilvipastoril))',
      'AND (ano-publicacao:[2000 TO *])',
      'AND (tipo-material-sigla:"PC" OR tipo-material-sigla:"AP"',
      'OR tipo-material-sigla:"NC" OR tipo-material-sigla:"RT")',
      'AND (idioma:"Inglês" OR idioma:"Português")'
    ),
    oldest_publication_date = 2000,
    newest_publication_date = 2022
  )
) %>%
  rename_with( ~ str_replace_all(., "_", " ")) %>%
  gt() %>%
  tab_header(title = "LITERATURE SEARCH") %>%
  tab_options(
    heading.background.color = "#d2d2d2",
    table.border.top.width = 3,
    table.border.top.color = "#666663",
    heading.border.bottom.width = 3,
    heading.border.bottom.color = "#666663",
    table.border.bottom.width = 3,
    table.border.bottom.color = "#666663"
  ) %>%
  tab_source_note(
    "The search strategy was to include a wide variety of terms that are
    commonly used to refer an integrated crop-livestock-forestry system (ICLFS).
    This makes the search very broad, which gives us everything that have
    been researched about ICLFS. The inclusion of the Embrapa database is
    utmost important to create an overview of this system in Brazil, since it
    is the main developer and disseminator of this technology in the country. "
  ) %>%
  gtsave("iclfs_review/search_string.html")

# CREATE DATASET METADATA -----------------------------------------------------

# Create a table describing the files that composes the dataset
review_dataset <-
  bind_rows(
    list(
      name = "01_documents_registration.csv",
      description = "This table stores general information about the document.
      There are no data from experiments stored here. This table will provide
      information to identify the documents, its authors, year of publication.
      Will also store observations and quotes from the documents."
    ),
    list(
      name = "02_experiments_characteristics.csv",
      description = "This table is composed of specific information of the
      experiment as a whole. What are the farming systems analysed,
      the location, what are the species and some more information. It is
      linked to the {documents_registration} table by the unique identifier
      of the document."
    ),
    list(
      name = "03_system_management.csv",
      description = "Stores data about how is the management of each
      experimental plot of the experiment. You can find data regarding
      fertilization and supplementary diet in this table. It is linked to the
      {experiments_characteristics} table by the unique identifiers of
      the document and the experimental plot."
    ),
    list(
      name = "04_soil_properties.csv",
      description = "Stores data about properties of the soil in the
      experimental plot, along the soil profile and position.
      Examples of stored variables are the bulk density, soil texture,
      amount of carbon. It is linked to the
      {experiments_characteristics} table by the unique identifiers of
      the document and the experimental plot."
    ),
    list(
      name = "05_tree_component.csv",
      description = "Stores data about productivity of the tree component
      in the integrated production system. Examples of the stored variables are
      height, total carbon, diameter and basal area. It is linked to the
      {experiments_characteristics} table by the unique identifiers of
      the document and the experimental plot."
    ),
    list(
      name = "06_crop_component.csv",
      description = "Stores data about productivity of the crop component
      in the integrated production system. Examples of the stored variables are
      height, yield and population. It is linked to the
      {experiments_characteristics} table by the unique identifiers of
      the document and the experimental plot."
    ),
    list(
      name = "08_pasture_component.csv",
      description = "Stores data about productivity of the pasture component
      in the integrated production system. Examples of the stored variables are
      sowing rate, yield and digestability. It is linked to the
      {experiments_characteristics} table by the unique identifiers of
      the document and the experimental plot."
    ),
    list(
      name = "09_livestock_component.csv",
      description = "Stores data about productivity of the livestock component
      in the integrated production system. Examples of the stored variables are
      grazing method, stocking rate and live mean weight. It is linked to the
      {experiments_characteristics} table by the unique identifiers of
      the document and the experimental plot."
    ),
    list(
      name = "10_emissions.csv",
      description = "Stores data about greenhouse gas emissions in
      integrated production systems from diverse sources. Examples of the
      stored variables are gas, emission source, accumulated emissions.
      It is linked to the {experiments_characteristics} table by the unique
      identifiers of the document and the experimental plot."
    ),
    list(
      name = "11_radiation.csv",
      description = "Stores data about reduction of PAR transmittance in
      integrated production systems caused by canopy from tree component.
      Examples of the stored variables are PAR transmittance and position in
      relation to the tree lines.
      It is linked to the {experiments_characteristics} table by the unique
      identifiers of the document and the experimental plot."
    )
  )

# Add information about the tables
review_dataset %<>%
  mutate(
    number_of_columns = NA_integer_,
    number_of_rows = NA_integer_,
    size = NA_real_
  )

# CREATE TABLES METADATA ------------------------------------------------------

## Documents registration ----
documents_registration_meta <-
  bind_rows(
    list(
      name = "id",
      description = "This is a unique identificator for each downloaded
      document, the first letter indicates the base which the documents
      was downloaded from (E for embrapa and S for Scopus).",
      unit = "index"
    ),
    list(
      name = "author",
      description = "Name of the first author of the document. Full author
      surname followed by initials of first and middle names.",
      unit = "text"
    ),
    list(
      name = "publication_year",
      description = "Year of the publication of the document.",
      unit = "year according to the gregorian calendar"
    ),
    list(
      name = "title",
      description = "The title of the document.",
      unit = "text"
    ),
    list(
      name = "publication_type",
      description = "The type of the publitation. If the document was
      published in a peer reviewed journal, or in a congress, or as a
      technical document of an institution.",
      unit = "text"
    ),
    list(
      name = "publication_topic",
      description = "The general topic of the publication, completely
      subjective.",
      unit = "text"
    ),
    list(
      name = "publication_excerpts",
      description = "Parts of the document that I have considered of
      interest, completely subjective. Most of the text is copied from
      the publications, and therefore referenced to the authors.",
      unit = "text"
    ),
    list(
      name = "publication_obs",
      description = "Pertinent observations about the publication.",
      unit = "text"
    )
  )

## Experiments characteristics ----
experiments_characteristics_meta <-
  bind_rows(
    list(
      name = "id",
      description = "This is a unique identificator for each downloaded
      document, the first letter indicates the base which the documents
      was downloaded from (E for embrapa and S for Scopus).",
      unit = "index"
    ),
    list(
      name = "production_system",
      description = "Abbreviation of the production system in the experiment:
      ICLF = Integrated Crop-Livestock-Forestry;
      ICL = Integrated Crop-Livestock;
      ILF = Integrated Livestock-Forestry;
      C = Crop;
      L = Livestock;
      F = Forestry;
      EG = Extensive Grazing;
      SF = Seasonal Forest.",
      unit = "nominal category"
    ),
    list(
      name = "plot_id",
      description = "Unique experiment plot indentificator within the same
      publication (not unique across publications).",
      unit = "index"
    ),
    list(
      name = "plot_area",
      description = "The area of the experimental plot.",
      unit = "hectar (ha)"
    ),
    list(
      name = "total_area",
      description = "The total area of the experiment (all plots together).",
      unit = "hectar (ha)"
    ),
    list(
      name = "forest_component",
      description = "The species used in the production system that are
      part of the forestry component (woody species). This variable can
      be composed of one or more species, separated by comma.
      Species identified by scientific name.",
      unit = "nominal category"
    ),
    list(
      name = "crop_component",
      description = "The species used in the production system that are
      part of the crop component. This variable can be composed of one
      or more species, separated by comma.
      Species identified by scientific name.",
      unit = "nominal category"
    ),
    list(
      name = "pasture_component",
      description = "The species used in the production system that are
      part of the pasture component. This variable can be composed of one
      or more species, separated by comma.
      Species identified by scientific name.",
      unit = "nominal category"
    ),
    list(
      name = "other_component",
      description = "The species used in the production system that are
      not part of any of the components. This variable can be composed of
      one or more species, separated by comma.
      Species identified by scientific name.",
      unit = "nominal category"
    ),
    list(
      name = "livestock_component",
      description = "The species used in the production system that are
      part of livestock component. This variable can be composed of one
      or more species, separated by comma.
      Species identified by common name.",
      unit = "nominal category"
    ),
    list(
      name = "min_stocking_rate",
      description = "The minimum number of animals grazing in one hectare
      of pasture in the experiment or experimental plot.",
      unit = "float"
    ),
    list(
      name = "stocking_rate",
      description = "Number of animals grazing in one hectare of pasture
      in the experiment or experimental plot.",
      unit = "float"
    ),
    list(
      name = "max_stocking_rate",
      description = "The maximum number of animals grazing in one hectare
      of pasture in the experiment or experimental plot.",
      unit = "float"
    ),
    list(
      name = "integration_type",
      description = "Describe how the species are integrated in an
      experimental plot. Can be rotation, consortium or succession.
      One experimental plot can have different integration techniques,
      those are separated by a comma.",
      unit = "nominal category"
    ),
    list(
      name = "integration_obs",
      description = "Pertinent observations about the integration techniques
      used in an experimental plot.",
      unit = "text"
    ),
    list(
      name = "trees_spacing",
      description = "The spacing between trees adopted in an experimental
      plot. The spacing is represented by meters between trees.",
      unit = "nominal category"
    ),
    list(
      name = "trees_population",
      description = "The amount of trees individuals in one hectare of
      production.",
      unit = "integer"
    ),
    list(
      name = "soil_management",
      description = "Brief description of any relevant practice in soil
      management.",
      unit = "text"
    ),
    list(
      name = "previous_use",
      description = "What was the land use before the adoption of the
      production system analysed in the experiment.",
      unit = "nominal category"
    ),
    list(
      name = "establishment",
      description = "What was the year of the adoption of the
      production system analysed in the experiment.",
      unit = "year according to the gregorian calendar"
    ),
    list(
      name = "experiment_period",
      description = "Which years of the start and end of the experiment.
      The period is represented by two years, separated by a line.",
      unit = "years according to the gregorian calendar"
    ),
    list(
      name = "federative_unit",
      description = "The federative unit inside Brazil where the experimental
      plot was established.",
      unit = "nominal category"
    ),
    list(
      name = "municipality",
      description = "The municipality inside Brazil where the experimental
      plot was established.",
      unit = "nominal category"
    ),
    list(
      name = "experiment_location",
      description = "The specific location (e.g. farm name, institution unit)
      where the experimental plot was established.",
      unit = "nominal category"
    ),
    list(
      name = "coordinates",
      description = "The geographic coordinates provided to indicate the
      location of the experimental plot.",
      unit = "text"
    ),
    list(
      name = "soil_type_sibcs",
      description = "The soil type of the of the experimental plot(s),
      according to the Brazilian Soil Classification System.",
      unit = "nominal category"
    ),
    list(
      name = "soil_type_wrb",
      description = "The soil type of the of the experimental plot(s),
      according to the World Reference Base.",
      unit = "nominal category"
    ),
    list(
      name = "annual_precip",
      description = "Average historical accumulated precipitation in a year,
      for the experimental plot.",
      unit = "milimeters (mm)"
    ),
    list(
      name = "min_temp",
      description = "Average historical minimum temperature in a year,
      for the experimental plot.",
      unit = "celsius degrees (ºC)"
    ),
    list(
      name = "mean_temp",
      description = "Average historical mean temperature in a year,
      for the experimental plot.",
      unit = "celsius degrees (ºC)"
    ),
    list(
      name = "max_temp",
      description = "Average historical maximum temperature in a year,
      for the experimental plot.",
      unit = "celsius degrees (ºC)"
    )
  )


## System management ----
system_management_meta <-
  bind_rows(
    list(
      name = "id",
      description = "This is a unique identificator for each downloaded
      document, the first letter indicates the base which the documents
      was downloaded from (E for embrapa and S for Scopus).",
      unit = "index"
    ),
    list(
      name = "production_system",
      description = "Abbreviation of the production system in the experiment:
      ICLF = Integrated Crop-Livestock-Forestry;
      ICL = Integrated Crop-Livestock;
      ILF = Integrated Livestock-Forestry;
      C = Crop;
      L = Livestock;
      F = Forestry;
      EG = Extensive Grazing;
      SF = Seasonal Forest.",
      unit = "nominal category"
    ),
    list(
      name = "plot_id",
      description = "Unique experiment plot indentificator within the same
      publication (not unique across publications).",
      unit = "index"
    ),
    list(
      name = "management",
      description = "What is the management practice being described.",
      unit = "nominal category"
    ),
    list(
      name = "product",
      description = "What is the product being applied.",
      unit = "nominal category"
    ),
    list(
      name = "formula",
      description = "What is the formula of the product being applied
      (e.g. what is the proportion of nutrients in the fertilizer).",
      unit = "text"
    ),
    list(
      name = "quantity",
      description = "The amount of product being applied.",
      unit = "float"
    ),
    list(
      name = "year",
      description = "What is the year that the observation was recorded.",
      unit = "integer"
    ),
    list(
      name = "establishment",
      description = "What was the year of the adoption of the
      production system analysed in the experiment.",
      unit = "year according to the gregorian calendar"
    )
  )

## Soil properties ----
soil_properties_meta <-
  bind_rows(
    list(
      name = "id",
      description = "This is a unique identificator for each downloaded
      document, the first letter indicates the base which the documents
      was downloaded from (E for embrapa and S for Scopus).",
      unit = "index"
    ),
    list(
      name = "production_system",
      description = "Abbreviation of the production system in the experiment:
      ICLF = Integrated Crop-Livestock-Forestry;
      ICL = Integrated Crop-Livestock;
      ILF = Integrated Livestock-Forestry;
      C = Crop;
      L = Livestock;
      F = Forestry;
      EG = Extensive Grazing;
      SF = Seasonal Forest.",
      unit = "nominal category"
    ),
    list(
      name = "plot_id",
      description = "Unique experiment plot indentificator within the same
      publication (not unique across publications).",
      unit = "index"
    ),
    list(
      name = "soil_layer",
      description = "The layer of the soil, in depth, in which the observations
      were recorded.",
      unit = "centimeters (cm)"
    ),
    list(
      name = "clay_content",
      description = "The proportion of clay in the observations.",
      unit = "grams per kilogram (g/kg)"
    ),
    list(
      name = "silt_content",
      description = "The proportion of silt in the observations.",
      unit = "grams per kilogram (g/kg)"
    ),
    list(
      name = "sand_content",
      description = "The proportion of sand in the observations.",
      unit = "grams per kilogram (g/kg)"
    ),
    list(
      name = "bulk_density",
      description = "The soil bulk density in the observations.",
      unit = "kilograms per cubic meter (kg/m³)"
    ),
    list(
      name = "total_porosity",
      description = "The soil total porosity in the observations.",
      unit = "percent (%)"
    ),
    list(
      name = "particle_density",
      description = "The soil particle density in the observations.",
      unit = "kilograms per cubic meter (kg/m³)"
    ),
    list(
      name = "organic matter",
      description = "The amount of organic matter in the soil.",
      unit = "grams per kilogram (g/kg)"
    ),
    list(
      name = "organic_carbon_content",
      description = "The soil organic carbon content in the observations.",
      unit = "grams per kilogram (g/kg)"
    ),
    list(
      name = "correction",
      description = "If carbon content estimates were corrected.
      0 = no correction,
      1 = bulk density correction,
      2 = bulk density and soil mass correction.",
      unit = ""
    ),
    list(
      name = "number_of_observations",
      description = "The amount of observations that were used to
      generate the record.",
      unit = ""
    ),
    list(
      name = "estimator",
      description = "What estimator was used to represent the obervations.",
      unit = ""
    ),
    list(
      name = "position",
      description = "The position of the observations in relation
      to the tree component (how far the observations were performed
      in relation to the trees lines).",
      unit = "meters"
    ),
    list(
      name = "measured",
      description = "If the observations were measured or estimated.
      1 = measured, 0 = estimated",
      unit = "integer"
    ),
    list(
      name = "year",
      description = "What is the year that the observation was recorded.",
      unit = "integer"
    ),
    list(
      name = "establishment",
      description = "What was the year of the adoption of the
      production system analysed in the experiment.",
      unit = "year according to the gregorian calendar"
    )
  )

## Tree component ----
tree_component_meta <-
  bind_rows(
    list(
      name = "id",
      description = "This is a unique identificator for each downloaded
      document, the first letter indicates the base which the documents
      was downloaded from (E for embrapa and S for Scopus).",
      unit = "index"
    ),
    list(
      name = "production_system",
      description = "Abbreviation of the production system in the experiment:
      ICLF = Integrated Crop-Livestock-Forestry;
      ICL = Integrated Crop-Livestock;
      ILF = Integrated Livestock-Forestry;
      C = Crop;
      L = Livestock;
      F = Forestry;
      EG = Extensive Grazing;
      SF = Seasonal Forest.",
      unit = "nominal category"
    ),
    list(
      name = "plot_id",
      description = "Unique experiment plot indentificator within the same
      publication (not unique across publications).",
      unit = "index"
    ),
    list(
      name = "roots_fraction",
      description = "The fraction of roots composing the trees.",
      unit = "percentage (%)"
    ),
    list(
      name = "stem_fraction",
      description = "The fraction of stem composing the trees.",
      unit = "percentage (%)"
    ),
    list(
      name = "canopy_fraction",
      description = "The fraction of canopy composing the trees.",
      unit = "percentage (%)"
    ),
    list(
      name = "roots_volume",
      description = "The volume of roots composing the trees.",
      unit = "cubic meters (m³)"
    ),
    list(
      name = "stem_volume",
      description = "The volume of stem composing the trees.",
      unit = "cubic meters (m³)"
    ),
    list(
      name = "canopy_volume",
      description = "The volume of canopy composing the trees.",
      unit = "cubic meters (m³)"
    ),
    list(
      name = "roots_biomass",
      description = "The biomass of roots composing the trees.",
      unit = "tons per hectare (t/ha)"
    ),
    list(
      name = "stem_biomass",
      description = "The biomass of stem composing the trees.",
      unit = "tons per hectare (t/ha)"
    ),
    list(
      name = "canopy_biomass",
      description = "The biomass of canopy composing the trees.",
      unit = "tons per hectare (t/ha)"
    ),
    list(
      name = "roots_carbon",
      description = "The carbon of roots composing the trees.",
      unit = "tons per hectare (t/ha)"
    ),
    list(
      name = "stem_carbon",
      description = "The carbon of stem composing the trees.",
      unit = "tons per hectare (t/ha)"
    ),
    list(
      name = "canopy_carbon",
      description = "The carbon of canopy composing the trees.",
      unit = "tons per hectare (t/ha)"
    ),
    list(
      name = "total_biomass",
      description = "The biomass of the trees.",
      unit = "tons per hectare (t/ha)"
    ),
    list(
      name = "total_carbon",
      description = "The carbon of the trees.",
      unit = "tons per hectare (t/ha)"
    ),
    list(
      name = "diameter",
      description = "The diameter of the trees.",
      unit = "meters per hectare (m/ha)"
    ),
    list(
      name = "basal_area",
      description = "The basal area of the trees.",
      unit = "squared meters per hectare (m²/ha)"
    ),
    list(
      name = "height",
      description = "The height of the trees.",
      unit = "meters (m)"
    ),
    list(
      name = "mortality_rate",
      description = "The amount of trees that died.",
      unit = "percentage (%)"
    ),
    list(
      name = "year_after_planting",
      description = "The years after tree planting where the observations were
      registered.",
      unit = ""
    ),
    list(
      name = "month_after_planting",
      description = "The months after tree planting where the observations were
      registered.",
      unit = ""
    ),
    list(
      name = "number_of_observations",
      description = "The amount of observations that were used to
      generate the record.",
      unit = ""
    ),
    list(
      name = "estimator",
      description = "What estimator was used to represent the obervations.",
      unit = ""
    ),
    list(
      name = "measured",
      description = "If the observations were measured or estimated.
      1 = measured, 0 = estimated",
      unit = "integer"
    ),
    list(
      name = "year",
      description = "What is the year that the observation was recorded.",
      unit = "integer"
    ),
    list(
      name = "establishment",
      description = "What was the year of the adoption of the
      production system analysed in the experiment.",
      unit = "year according to the gregorian calendar"
    )
  )

## Crop component ----
crop_component_meta <-
  bind_rows(
    list(
      name = "id",
      description = "This is a unique identificator for each downloaded
      document, the first letter indicates the base which the documents
      was downloaded from (E for embrapa and S for Scopus).",
      unit = "index"
    ),
    list(
      name = "production_system",
      description = "Abbreviation of the production system in the experiment:
      ICLF = Integrated Crop-Livestock-Forestry;
      ICL = Integrated Crop-Livestock;
      ILF = Integrated Livestock-Forestry;
      C = Crop;
      L = Livestock;
      F = Forestry;
      EG = Extensive Grazing;
      SF = Seasonal Forest.",
      unit = "nominal category"
    ),
    list(
      name = "plot_id",
      description = "Unique experiment plot indentificator within the same
      publication (not unique across publications).",
      unit = "index"
    ),
    list(
      name = "specie",
      description = "What is the specie being observed.",
      unit = ""
    ),
    list(
      name = "population",
      description = "The amount of individuals planted per hectare.",
      unit = "plants per hectare (n/ha)"
    ),
    list(
      name = "yield",
      description = "The yield of the crop.",
      unit = "kilograms per hectare (kg/ha)"
    ),
    list(
      name = "total_biomass",
      description = "The biomass of the trees.",
      unit = "tons per hectare (t/ha)"
    ),
    list(
      name = "total_carbon",
      description = "The carbon of the trees.",
      unit = "tons per hectare (t/ha)"
    ),
    list(
      name = "height",
      description = "The height of the crops.",
      unit = "meters (m)"
    ),
    list(
      name = "month_after_planting",
      description = "The months after tree planting where the observations were
      registered.",
      unit = ""
    ),
    list(
      name = "number_of_observations",
      description = "The amount of observations that were used to
      generate the record.",
      unit = ""
    ),
    list(
      name = "estimator",
      description = "What estimator was used to represent the obervations.",
      unit = ""
    ),
    list(
      name = "measured",
      description = "If the observations were measured or estimated.
      1 = measured, 0 = estimated",
      unit = "integer"
    ),
    list(
      name = "year",
      description = "What is the year that the observation was recorded.",
      unit = "integer"
    ),
    list(
      name = "establishment",
      description = "What was the year of the adoption of the
      production system analysed in the experiment.",
      unit = "year according to the gregorian calendar"
    )
  )

## Pasture component ----
pasture_component_meta <-
  bind_rows(
    list(
      name = "id",
      description = "This is a unique identificator for each downloaded
      document, the first letter indicates the base which the documents
      was downloaded from (E for embrapa and S for Scopus).",
      unit = "index"
    ),
    list(
      name = "production_system",
      description = "Abbreviation of the production system in the experiment:
      ICLF = Integrated Crop-Livestock-Forestry;
      ICL = Integrated Crop-Livestock;
      ILF = Integrated Livestock-Forestry;
      C = Crop;
      L = Livestock;
      F = Forestry;
      EG = Extensive Grazing;
      SF = Seasonal Forest.",
      unit = "nominal category"
    ),
    list(
      name = "plot_id",
      description = "Unique experiment plot indentificator within the same
      publication (not unique across publications).",
      unit = "index"
    ),
    list(
      name = "specie",
      description = "What is the specie being observed.",
      unit = ""
    ),
    list(
      name = "spacing",
      description = "The spacing in relation to the crop component.",
      unit = "meters (m)"
    ),
    list(
      name = "sowing_rate",
      description = "The amount of seeds used in one hectare.",
      unit = "kilograms per hectare (kg/ha)"
    ),
    list(
      name = "position",
      description = "The position of the observations in relation
      to the tree component (how far the observations were performed
      in relation to the trees lines).",
      unit = "meters"
    ),
    list(
      name = "yield",
      description = "The total dry matter yield of the pasture.",
      unit = "kilograms per hectare (kg/ha)"
    ),
    list(
      name = "leaf_yield",
      description = "The leaf dry matter yield of the pasture.",
      unit = "kilograms per hectare (kg/ha)"
    ),
    list(
      name = "dry_matter_accumulation",
      description = "The total dry matter accumulation of the pasture.",
      unit = "kilograms per hectare per day (kg/ha day)"
    ),
    list(
      name = "digestability",
      description = "The in vitro digestability of the pasture.",
      unit = "percentage (%)"
    ),
    list(
      name = "crude_protein",
      description = "The protein concentration of the pasture.",
      unit = "percentage (%)"
    ),
    list(
      name = "lai",
      description = "The leaf area index of the pasture",
      unit = "squared meters per squared meters (m²/m²)"
    ),
    list(
      name = "sla",
      description = "The specific leaf area of the pasture",
      unit = "squared centimeters per gram (cm²/g)"
    ),
    list(
      name = "total_biomass",
      description = "The biomass of the trees.",
      unit = "tons per hectare (t/ha)"
    ),
    list(
      name = "total_carbon",
      description = "The carbon of the trees.",
      unit = "tons per hectare (t/ha)"
    ),
    list(
      name = "height",
      description = "The height of the crops.",
      unit = "meters (m)"
    ),
    list(
      name = "month_after_planting",
      description = "The months after tree planting where the observations were
      registered.",
      unit = ""
    ),
    list(
      name = "number_of_observations",
      description = "The amount of observations that were used to
      generate the record.",
      unit = ""
    ),
    list(
      name = "estimator",
      description = "What estimator was used to represent the obervations.",
      unit = ""
    ),
    list(
      name = "measured",
      description = "If the observations were measured or estimated.
      1 = measured, 0 = estimated",
      unit = "integer"
    ),
    list(
      name = "year",
      description = "What is the year that the observation was recorded.",
      unit = "integer"
    ),
    list(
      name = "establishment",
      description = "What was the year of the adoption of the
      production system analysed in the experiment.",
      unit = "year according to the gregorian calendar"
    )
  )

## Livestock component ----
livestock_component_meta <-
  bind_rows(
    list(
      name = "id",
      description = "This is a unique identificator for each downloaded
      document, the first letter indicates the base which the documents
      was downloaded from (E for embrapa and S for Scopus).",
      unit = "index"
    ),
    list(
      name = "production_system",
      description = "Abbreviation of the production system in the experiment:
      ICLF = Integrated Crop-Livestock-Forestry;
      ICL = Integrated Crop-Livestock;
      ILF = Integrated Livestock-Forestry;
      C = Crop;
      L = Livestock;
      F = Forestry;
      EG = Extensive Grazing;
      SF = Seasonal Forest.",
      unit = "nominal category"
    ),
    list(
      name = "plot_id",
      description = "Unique experiment plot indentificator within the same
      publication (not unique across publications).",
      unit = "index"
    ),
    list(
      name = "specie",
      description = "What is the specie being observed.",
      unit = ""
    ),
    list(
      name = "grazing_method",
      description = "What was the grazing method used in the plot.",
      unit = ""
    ),
    list(
      name = "stocking_rate",
      description = "What was the stocking rate of the plot.",
      unit = ""
    ),
    list(
      name = "live_mean_weigth",
      description = "The live mean weight of the livestock.",
      unit = "kilograms (kg)"
    ),
    list(
      name = "average_daily_gain",
      description = "The weight gain of the livestock per day",
      unit = "grams per animal per day (g/AU day)"
    ),
    list(
      name = "carcass",
      description = "The ilograms of carcass produced per hectar.",
      unit = "kilograms per hectare (kg/ha)"
    ),
    list(
      name = "age",
      description = "The age of the livestock in months.",
      unit = "month"
    ),
    list(
      name = "number_of_observations",
      description = "The amount of observations that were used to
      generate the record.",
      unit = ""
    ),
    list(
      name = "estimator",
      description = "What estimator was used to represent the obervations.",
      unit = ""
    ),
    list(
      name = "measured",
      description = "If the observations were measured or estimated.
      1 = measured, 0 = estimated",
      unit = "integer"
    ),
    list(
      name = "year",
      description = "What is the year that the observation was recorded.",
      unit = "integer"
    ),
    list(
      name = "month",
      description = "What is the month that the observation was recorded.",
      unit = "integer"
    ),
    list(
      name = "establishment",
      description = "What was the year of the adoption of the
      production system analysed in the experiment.",
      unit = "year according to the gregorian calendar"
    )
  )

## Emissions ----
emissions_meta <-
  bind_rows(
    list(
      name = "id",
      description = "This is a unique identificator for each downloaded
      document, the first letter indicates the base which the documents
      was downloaded from (E for embrapa and S for Scopus).",
      unit = "index"
    ),
    list(
      name = "production_system",
      description = "Abbreviation of the production system in the experiment:
      ICLF = Integrated Crop-Livestock-Forestry;
      ICL = Integrated Crop-Livestock;
      ILF = Integrated Livestock-Forestry;
      C = Crop;
      L = Livestock;
      F = Forestry;
      EG = Extensive Grazing;
      SF = Seasonal Forest.",
      unit = "nominal category"
    ),
    list(
      name = "plot_id",
      description = "Unique experiment plot indentificator within the same
      publication (not unique across publications).",
      unit = "index"
    ),
    list(
      name = "source",
      description = "What is the source of the emissions.",
      unit = ""
    ),
    list(
      name = "gas",
      description = "What is the gas being observed.",
      unit = ""
    ),
    list(
      name = "accumulated_emissions",
      description = "The ammount of emissions in a year, in one hectare.",
      unit = "kilograms per hectare (kg/ha)"
    ),
    list(
      name = "number_of_observations",
      description = "The amount of observations that were used to
      generate the record.",
      unit = ""
    ),
    list(
      name = "estimator",
      description = "What estimator was used to represent the obervations.",
      unit = ""
    ),
    list(
      name = "measured",
      description = "If the observations were measured or estimated.
      1 = measured, 0 = estimated",
      unit = "integer"
    ),
    list(
      name = "year",
      description = "What is the year that the observation was recorded.",
      unit = "integer"
    ),
    list(
      name = "establishment",
      description = "What was the year of the adoption of the
      production system analysed in the experiment.",
      unit = "year according to the gregorian calendar"
    )
  )

## Radiation ----
radiation_meta <-
  bind_rows(
    list(
      name = "id",
      description = "This is a unique identificator for each downloaded
      document, the first letter indicates the base which the documents
      was downloaded from (E for embrapa and S for Scopus).",
      unit = "index"
    ),
    list(
      name = "production_system",
      description = "Abbreviation of the production system in the experiment:
      ICLF = Integrated Crop-Livestock-Forestry;
      ICL = Integrated Crop-Livestock;
      ILF = Integrated Livestock-Forestry;
      C = Crop;
      L = Livestock;
      F = Forestry;
      EG = Extensive Grazing;
      SF = Seasonal Forest.",
      unit = "nominal category"
    ),
    list(
      name = "plot_id",
      description = "Unique experiment plot indentificator within the same
      publication (not unique across publications).",
      unit = "index"
    ),
    list(
      name = "par_transmittance",
      description = "The amount of photossinthetically active radiation
      that is transmitted by the tree component canopy.",
      unit = "percentage (%)"
    ),
    list(
      name = "position",
      description = "The position of the observations in relation
      to the tree component (how far the observations were performed
      in relation to the trees lines).",
      unit = "meters"
    ),
    list(
      name = "number_of_observations",
      description = "The amount of observations that were used to
      generate the record.",
      unit = ""
    ),
    list(
      name = "estimator",
      description = "What estimator was used to represent the
      obervations.",
      unit = ""
    ),
    list(
      name = "measured",
      description = "If the observations were measured or estimated.
      1 = measured, 0 = estimated",
      unit = "integer"
    ),
    list(
      name = "year",
      description = "What is the year that the observation was recorded.",
      unit = "integer"
    ),
    list(
      name = "month",
      description = "What is the month that the observation was recorded.",
      unit = "integer"
    ),
    list(
      name = "establishment",
      description = "What was the year of the adoption of the
      production system analysed in the experiment.",
      unit = "year according to the gregorian calendar"
    )
  )

## Create a list of the metadata tables ----
table_metadata <-
  list(
    documents_registration_meta,
    experiments_characteristics_meta,
    system_management_meta,
    soil_properties_meta,
    tree_component_meta,
    crop_component_meta,
    pasture_component_meta,
    livestock_component_meta,
    emissions_meta,
    radiation_meta
  )

# CREATE TABLES HEADERS -------------------------------------------------------

# Create list of headers
headers_list <-
  map(
    .x = table_metadata,
    function(table) {

      header <-
        table$name %>%
        map_dfc(~tibble(!!.x := character())) %>%
        mutate(publication_year = as.integer())

    }
  ) %>%
  purrr::set_names(review_dataset$name)

# CHECK CONSISTENCE -----------------------------------------------------------

walk2(
  .x = headers_list,
  .y = names(headers_list),
  function(header_table, file) {

    # Check if review table already exists
    if (file_exists(glue("iclfs_review/tables/{file}"))) {

      table <- read_delim(
        glue("iclfs_review/tables/{file}"),
        delim = ";",
        col_types = list(col_character())
      )

    } else { # If table does not exists, create one from the metadata

      write_delim(
        header_table,
        glue("iclfs_review/tables/{file}"),
        delim = ";"
      )

      table <- read_delim(
        glue("iclfs_review/tables/{file}"),
        delim = ";",
        col_types = list(col_character())
      )

    }

    # Check if all columns in the review table are in the metadata
    col_check <-
      all(
        names(table) %in% names(header_table)
      )

    if (col_check & nrow(table) > 0) {

      # If review table is consistent with metadata, merge and save them

      write_delim(
        bind_rows(header_table, table),
        glue("iclfs_review/tables/{file}"),
        delim = ";"
      )

      cat(sep = "\n",
          crayon::green("Tables are rady to be merged in repository")
      )

    } else if (nrow(table) == 0) {

      cat(sep = "\n",
        crayon::red(
          glue(
            "There aren't any records in {file}, please add at least one ",
            "record to update the table."
          )
        )
      )


    } else {

      cat(sep = "\n",
        crayon::red(
          glue(
            "There are columns in {file} not listed in the metadata. ",
            "Correct the metadata or the table before continuing."
          )
        )
      )


    }

  }
)

# SAVE METADATA TABLES --------------------------------------------------------

# Dataset metadata
review_dataset %>%
  mutate(
    size = file_size(dir_ls("iclfs_review/tables/", glob = "*.csv")),
    number_of_columns = map_int(
      dir_ls("iclfs_review/tables/", glob = "*.csv"),
      ~ {
        read_delim(
          .x,
          progress = FALSE,
          col_types = cols(),
          delim = ";"
        ) %>%
          ncol()
      }
    ),
    number_of_rows = map_int(
      dir_ls("iclfs_review/tables/", glob = "*.csv"),
      ~ {
        read_delim(
          .x,
          progress = FALSE,
          col_types = cols(),
          delim = ";"
        ) %>%
          nrow()
      }
    )
  ) %>%
  rename_with( ~ str_replace_all(., "_", " ")) %>%
  gt() %>%
  tab_header(title = "TABLES COLLECTION") %>%
  fmt_bytes(columns = size,standard = "binary") %>%
  cols_width(
    size ~ px(120),
    description ~ px(1100),
    name ~ px(400)
  ) %>%
  tab_options(
    heading.background.color = "#d2d2d2",
    table.border.top.width = 3,
    table.border.top.color = "#666663",
    heading.border.bottom.width = 3,
    heading.border.bottom.color = "#666663",
    table.border.bottom.width = 3,
    table.border.bottom.color = "#666663"
  ) %>%
  gtsave("iclfs_review/tables/00_tables_collection.html")

# Tables metadata
walk2(
  .x = table_metadata,
  .y = review_dataset$name,
  function(table, file) {

    file_name <- str_remove(file, ".csv")

    # Create table and save as HTML
    table %>%
      gt() %>%
      tab_header(
        title = str_to_upper(str_replace(file_name, "_", " "))
      ) %>%
      tab_options(
        heading.background.color = "#d2d2d2",
        table.border.top.width = 3,
        table.border.top.color = "#666663",
        heading.border.bottom.width = 3,
        heading.border.bottom.color = "#666663",
        table.border.bottom.width = 3,
        table.border.bottom.color = "#666663"
      ) %>%
      gtsave(glue("iclfs_review/tables/{file_name}.html"))

  }
)

# Create table with all variables to be recorded
bind_rows(table_metadata) %>%
  distinct(name, .keep_all = TRUE) %>%
  gt() %>%
  tab_header(
    title = "VARIABLES"
  ) %>%
  tab_options(
    heading.background.color = "#d2d2d2",
    table.border.top.width = 3,
    table.border.top.color = "#666663",
    heading.border.bottom.width = 3,
    heading.border.bottom.color = "#666663",
    table.border.bottom.width = 3,
    table.border.bottom.color = "#666663"
  ) %>%
  gtsave("iclfs_review/tables/all_variables.html")
