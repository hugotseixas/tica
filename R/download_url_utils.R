
lulc_url_function <-
  function() {

    url_list <-
      glue::glue(
        "https://storage.googleapis.com/mapbiomas-public/",
        "initiatives/brasil/collection_8/lclu/coverage/",
        "brasil_coverage_{1985:2022}.tif"
      )

    url_list <-
      setNames(as.list(url_list), glue::glue("lulc_{1985:2022}"))

    return(url_list)

  }

vegsuppression_url_function <-
  function() {

    url_list <-
      glue::glue(
        "https://storage.googleapis.com/mapbiomas-public/",
        "initiatives/brasil/collection_8/deforest-secveg-annual/",
        "brasil_desmat_vsec_anual_{1987:2021}.tif"
      )

    url_list <-
      setNames(as.list(url_list), glue::glue("vegsuppression_{1987:2021}"))

    return(url_list)

  }

municipality_url_function <-
  function() {

    ipea_metadata <-
      readr::read_csv(
        glue::glue(
          "https://github.com/ipeaGIT/geobr/releases/",
          "download/v1.7.0/metadata_1.7.0_gpkg.csv"
        ),
        show_col_types = FALSE
      ) |>
      dplyr::filter(
        .data$geo == "municipality",
        stringr::str_detect(
          .data$download_path,
          "simplified",
          negate = TRUE
        )
      )

    url_list <- ipea_metadata$download_path

    url_list <-
      setNames(
        as.list(url_list),
        glue::glue(
          "municipality_{ipea_metadata$year}_",
          "{stringr::str_to_lower(ipea_metadata$code)}"
        )
      )

    return(url_list)

  }

biomes_url_function <-
  function() {

    ipea_metadata <-
      readr::read_csv(
        glue::glue(
          "https://github.com/ipeaGIT/geobr/releases/",
          "download/v1.7.0/metadata_1.7.0_gpkg.csv"
        ),
        show_col_types = FALSE
      ) |>
      dplyr::filter(
        .data$geo == "biomes",
        stringr::str_detect(
          .data$download_path,
          "simplified",
          negate = TRUE
        )
      )

    url_list <- ipea_metadata$download_path

    url_list <-
      setNames(
        as.list(url_list),
        glue::glue(
          "biomes_{ipea_metadata$year}"
        )
      )

    return(url_list)

  }

highways_url_function <-
  function() {

    url_list <-
      c(
        glue::glue(
          "http://servicos.dnit.gov.br/dnitcloud/index.php",
          "/s/oTpPRmYs5AAdiNr/download?",
          "path=%2FHist%C3%B3rico%20Planilhas%20(1994-2010)",
          "%2FPNV%20Planilhas%20(1994-2010)%20(XLS)&files=PNV",
          "{c(1994, 1996, 1998:2010)}.xlsx&download"
        ),
        glue::glue(
          "http://servicos.dnit.gov.br/dnitcloud/index.php",
          "/s/oTpPRmYs5AAdiNr/download?",
          "path=%2FSNV%20Planilhas%20(2011-Atual)",
          "%20(XLS)&files=SNV_",
          "{c(2011:2014, '201503A', '201612A', '201710B', '201811A', '201910A', '202010A', '202110A', '202210C', '202310A')}",
          ".{c(rep('xlsx', 4), rep('xls', 9))}&download"
        ),
        glue::glue(
          "http://servicos.dnit.gov.br/dnitcloud/index.php",
          "/s/oTpPRmYs5AAdiNr/download?",
          "path=%2FSNV%20Bases%20Geom%C3%A9tricas%20(2013-Atual)",
          "%20(SHP)&files=202310A.zip&download"
        )
      )

    url_list <-
      setNames(
        as.list(url_list),
        glue::glue("highways_{c(1994, 1996, 1998:2023, 2023)}")
      )

    return(url_list)

  }