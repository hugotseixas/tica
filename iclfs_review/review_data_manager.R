# HEADER -----------------------------------------------------------------------
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
# LIBRARIES --------------------------------------------------------------------
#
library(tidyverse)
#
# OPTIONS ----------------------------------------------------------------------
#
#
# CREATE DATASET METADATA ------------------------------------------------------



# CREATE TABLES METADATA -------------------------------------------------------

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
      unit = "ha (hectar)"
    ),
    list(
      name = "forest_component",
      description = "The species used in the production system that are
      part of the forestry component (woody species). This variable can
      be composed of one or more species, separated by comma.
      Species identified by scientific name.",
      unit = "text"
    ),
    list(
      name = "crop_component",
      description = "The species used in the production system that are
      part of the crop component. This variable can be composed of one
      or more species, separated by comma.
      Species identified by scientific name.",
      unit = "text"
    ),
    list(
      name = "pasture_component",
      description = "The species used in the production system that are
      part of the pasture component. This variable can be composed of one
      or more species, separated by comma.
      Species identified by scientific name.",
      unit = "text"
    ),
    list(
      name = "other_component",
      description = "The species used in the production system that are
      not part of any of the components. This variable can be composed of
      one or more species, separated by comma.
      Species identified by scientific name.",
      unit = "text"
    ),
    list(
      name = "livestock_component",
      description = "The species used in the production system that are
      part of livestock component. This variable can be composed of one
      or more species, separated by comma.
      Species identified by common name.",
      unit = "text"
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
      name = "min_stocking_rate",
      description = "The minimum number of animals grazing in one hectare
      of pasture in the experiment or experimental plot.",
      unit = "float"
    ),
    list(
      name = "integration_type",
      description = "Describe how the species are integrated in an
      experimental plot. Can be rotation, consortium or succession.
      One experimental plot can have different integration techniques,
      those are separated by a comma.",
      unit = "text"
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
      unit = "text"
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
      unit = "text"
    ),
    list(
      name = "establishment",
      description = "What was the year of the adoption of the
      production system analysed in the experiment.",
      unit = "year according to the gregorian calendar"
    ),
    list(
      name = "experiment period",
      description = "Which years of the start and end of the experiment.
      The period is represented by two years, separated by a line.",
      unit = "years according to the gregorian calendar"
    )
  )

# CREATE TABLES ----------------------------------------------------------------

documents_registration_header <-
  documents_registration_meta$name %>%
  map_dfc(~tibble(!!.x := character()))


# CHECK CONSISTENCE ------------------------------------------------------------

# UPDATE TABLES ----------------------------------------------------------------
