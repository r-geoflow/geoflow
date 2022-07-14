geoflow – R engine to orchestrate and run geospatial (meta)data
workflows - List of actions and softwares
================

# 1 Actions

## 1.1 List of actions supported by geoflow

| id                                                                          | types                                                            | definition                                                                 | target | target_dir | pid_generator | packages                 |
|:----------------------------------------------------------------------------|:-----------------------------------------------------------------|:---------------------------------------------------------------------------|:-------|:-----------|:--------------|:-------------------------|
| [**geometa-create-iso-19115**](#geometa-create-iso-19115)<br>               | Metadata production                                              | Produce an ISO/OGC 19115/19139 metadata object                             | entity | metadata   | FALSE         | geometa,ows4R            |
| [**geometa-create-iso-19110**](#geometa-create-iso-19110)<br>               | Metadata production                                              | Produce an ISO 19110/19139 metadata object                                 | entity | metadata   | FALSE         | geometa                  |
| [**ows4R-publish-iso-19139**](#ows4R-publish-iso-19139)<br>                 | Metadata publication                                             | Publish/Update an ISO/OGC 19139 metadata object using OGC CSW Protocol     | NA     | NA         | FALSE         | ows4R                    |
| [**geonapi-publish-iso-19139**](#geonapi-publish-iso-19139)<br>             | Metadata publication                                             | Publish/Update an ISO/OGC 19139 metadata object with GeoNetwork API        | NA     | NA         | FALSE         | geonapi                  |
| [**geosapi-publish-ogc-services**](#geosapi-publish-ogc-services)<br>       | Data upload,Data publication,Metadata publication                | Publish vector data to GeoServer OGC web-services (WMS/WFS/WCS)            | NA     | NA         | FALSE         | geosapi                  |
| [**zen4R-deposit-record**](#zen4R-deposit-record)<br>                       | Data upload,Data publication,Metadata publication,DOI assignment | Deposits/Publish data and/or metadata in the Zenodo infrastructure         | job    | zenodo     | TRUE          | zen4R                    |
| [**atom4R-dataverse-deposit-record**](#atom4R-dataverse-deposit-record)<br> | Data upload,Data publication,Metadata publication,DOI assignment | Deposits/Publish data and/or metetadata on a Dataverse using the Sword API | job    | dataverse  | TRUE          | atom4R                   |
| [**dataone-upload-datapackage**](#dataone-upload-datapackage)<br>           | Data upload,Data publication,Metadata publication,DOI assignment | Uploads a data package to a DataOne metacat node                           | job    | dataone    | TRUE          | mime,datapack,dataone    |
| [**sf-write-generic**](#sf-write-generic)<br>                               | Data writing,Data upload                                         | Import features data into several formats                                  | entity | data       | FALSE         | sf,DBI,RSQLite,RPostgres |
| [**sf-write-dbi**](#sf-write-dbi)<br>                                       | Data writing,Data upload                                         | Import features data into Postgres/Postgis                                 | NA     | NA         | FALSE         | sf,DBI,RSQLite,RPostgres |
| [**sf-write-shp**](#sf-write-shp)<br>                                       | Data writing                                                     | Import features data and zip files                                         | entity | data       | FALSE         | sf                       |
| [**eml-create-eml**](#eml-create-eml)<br>                                   | Metadata production                                              | Produce an EML metadata object                                             | entity | metadata   | FALSE         | EML,emld                 |
| [**d4storagehub4R-upload-data**](#d4storagehub4R-upload-data)<br>           | Data upload                                                      | Upload data/metadata to a D4Science Workspace                              | NA     | NA         | FALSE         | d4storagehub4R           |
| [**create-metadata-rmd**](#create-metadata-rmd)<br>                         | Metadata production                                              | Generate a Markdown out of a entity                                        | entity | markdown   | FALSE         | rmarkdown                |

### 1.1.1 List of geometa-create-iso-19115 options<a name= geometa-create-iso-19115 />

| name                                      | definition                                                                                | default   |
|:------------------------------------------|:------------------------------------------------------------------------------------------|:----------|
| use_uuid                                  | Use UUID as metadata identifier, if not defined the UUID is pre-generated                 | FALSE     |
| doi                                       | Add entity DOI - if defined - as metadata identifier and online resource                  | FALSE     |
| doi_thumbnail                             | if option ‘doi’ is true and this option enabled, a DOI thumbnail will be added            | FALSE     |
| inspire                                   | Validates ISO 19139 metadata with INSPIRE reference validator                             | FALSE     |
| logo                                      | Add configure profile logo(s) - if defined - as metadata thumbnail(s)                     | FALSE     |
| addfeatures                               | Add entity data features - if defined - as metadata bounding polygon(s)                   | FALSE     |
| featureid                                 | ID of entity data features used to identify bounding polygon(s) with option ‘addfeatures’ | NA        |
| subject_geography                         | Identifier of the subject handling a Geographic coverage.                                 | geography |
| include_coverage_data_dimension_values    | Include data dimensions’s range values to coverage description                            | FALSE     |
| include_coverage_service_dimension_values | Include ogc dimensions’s range values to coverage description                             | FALSE     |

### 1.1.2 List of geometa-create-iso-19110 options<a name= geometa-create-iso-19110 />

| name                                 | definition                                                                          | default |
|:-------------------------------------|:------------------------------------------------------------------------------------|:--------|
| doi                                  | Add entity DOI - if defined - as metadata identifier and online resource            | FALSE   |
| exclude_attributes                   | Attributes that should be excluded from the ISO 19110 production                    |         |
| exclude_attributes_not_in_dictionary | Enable to exclude all attributes/variables not referenced as dictionary/featuretype | FALSE   |
| exclude_values_for_attributes        | Attribute names for which listed values should not be produced                      |         |
| extra_attributes                     | Extra attributes to add as feature catalog attributes although not in data          |         |
| default_min_occurs                   | The default min occurs value for feature attributes cardinality                     | 0       |
| default_max_occurs                   | The default max occurs value for feature attribute cardinality                      | Inf     |

### 1.1.3 List of ows4R-publish-iso-19139 options<a name= ows4R-publish-iso-19139 />

| name            | definition                                                                       | default |
|:----------------|:---------------------------------------------------------------------------------|:--------|
| geometa_inspire | Validates ISO 19139 metadata with INSPIRE reference validator before publication | FALSE   |

### 1.1.4 List of geonapi-publish-iso-19139 options<a name= geonapi-publish-iso-19139 />

| name               | definition                                                                                 | default               |
|:-------------------|:-------------------------------------------------------------------------------------------|:----------------------|
| geometa_inspire    | Validates ISO 19139 metadata with INSPIRE reference validator before publication           | FALSE                 |
| privileges         | Geonetwork privileges to set for the metadata to be published                              | view,dynamic,featured |
| group              | Geonetwork user group to which the metadata should be associated                           | 2                     |
| category           | Category of metadata resources to which the metadata record should be associated           | datasets              |
| publish_thumbnails | Uploads local thumbnails as attachments and publish them as thumbnails / graphic overviews | TRUE                  |

### 1.1.5 List of geosapi-publish-ogc-services options<a name= geosapi-publish-ogc-services />

| name                                    | definition                                                                                                                                  | default |
|:----------------------------------------|:--------------------------------------------------------------------------------------------------------------------------------------------|:--------|
| createWorkspace                         | Create workspace if not already existing                                                                                                    | FALSE   |
| createStore                             | Create data/coverage store if not already existing                                                                                          | FALSE   |
| store_description                       | Specify a decription for the new data/coverage store                                                                                        |         |
| enrich_with_relations                   | When enabled, enrichs entity with OGC relations                                                                                             | TRUE    |
| enrich_with_relation_wms                | When enabled, enrichs entity with a base WMS link relation                                                                                  | TRUE    |
| enrich_with_relation_wms_thumbnail      | When enabled, enrichs entity with a WMS-based thumbnail relation                                                                            | TRUE    |
| enrich_with_relation_wfs                | When enabled, enrichs entity with a base WFS link relation (applies to ‘vector’ only)                                                       | TRUE    |
| enrich_with_relation_wfs_download_links | When enabled, enrichs entity with WFS format-specific (GML, GeoJSON, SHAPE-ZIP, CSV) links for download purpose (applies to ‘vector’ only). | TRUE    |
| enrich_with_relation_wcs                | When enabled, enrichs entity with a base WCS link relation (applies to ‘grid’ only)                                                         | TRUE    |
| enrich_with_relation_wcs_download_links | When enabled, enrichs entity with WCS format-specific links for download purpose (applies to ‘grid’ only). Only GeoTIFF at now.             | TRUE    |

### 1.1.6 List of zen4R-deposit-record options<a name= zen4R-deposit-record />

| name                   | definition                                                                                  | default    |
|:-----------------------|:--------------------------------------------------------------------------------------------|:-----------|
| depositWithFiles       | Indicates if the action is uploading files                                                  | FALSE      |
| depositDataPattern     | A regular expression to filter data files to upload in Zenodo                               |            |
| depositMetadataPattern | A regular expression to filter metadata files to upload in Zenodo                           |            |
| zipEachDataFile        | Indicates if each data file should be zipped (to be used in case of large data files        | FALSE      |
| publish                | Indicates if the action should publish the deposit. Requires ‘depositWithFiles’ set to TRUE | FALSE      |
| strategy               | Strategy to use when handling published records, either ‘newversion’ (default) or ‘edition’ | newversion |
| deleteOldFiles         | Indicates if the action should delete old files prior upload new files                      | TRUE       |
| update_metadata        | For an existing deposit, indicates if metadata elements should be updated                   | TRUE       |
| update_files           | For an existing deposit, indicates if files should be updated                               | TRUE       |
| communities            | One or more communities to which the deposit should be associated                           | NA         |

### 1.1.7 List of atom4R-dataverse-deposit-record options<a name= atom4R-dataverse-deposit-record />

| name             | definition                                                                                  | default |
|:-----------------|:--------------------------------------------------------------------------------------------|:--------|
| depositWithFiles | Indicates if the action is uploading files                                                  | FALSE   |
| publish          | Indicates if the action should publish the deposit. Requires ‘depositWithFiles’ set to TRUE | FALSE   |
| deleteOldFiles   | Indicates if the action should delete old files prior upload new files                      | TRUE    |
| update_metadata  | For an existing deposit, indicates if metadata elements should be updated                   | TRUE    |
| update_files     | For an existing deposit, indicates if files should be updated                               | TRUE    |

### 1.1.8 List of dataone-upload-datapackage options<a name= dataone-upload-datapackage />

*No options available for this action*

### 1.1.9 List of sf-write-generic options<a name= sf-write-generic />

| name          | definition                                                                    | default |
|:--------------|:------------------------------------------------------------------------------|:--------|
| type          | format to convert                                                             | NA      |
| createIndexes | create indexes for columns                                                    | FALSE   |
| overwrite     | Overwrite policy                                                              | TRUE    |
| append        | Append policy                                                                 | FALSE   |
| chunk.size    | Size of DB upload data chunk. Default is 0L, meaning no chunking is operated. | 0       |

### 1.1.10 List of sf-write-dbi options<a name= sf-write-dbi />

| name          | definition                                                                    | default |
|:--------------|:------------------------------------------------------------------------------|:--------|
| createIndexes | create indexes for columns                                                    | FALSE   |
| overwrite     | Overwrite policy                                                              | TRUE    |
| append        | Append policy                                                                 | FALSE   |
| chunk.size    | Size of DB upload data chunk. Default is 0L, meaning no chunking is operated. | 0       |

### 1.1.11 List of sf-write-shp options<a name= sf-write-shp />

*No options available for this action*

### 1.1.12 List of eml-create-eml options<a name= eml-create-eml />

| name             | definition                                                 | default  |
|:-----------------|:-----------------------------------------------------------|:---------|
| subject_taxonomy | Identifier of the subject handling the Taxonomic coverage. | taxonomy |

### 1.1.13 List of d4storagehub4R-upload-data options<a name= d4storagehub4R-upload-data />

| name               | definition                                                                                                                  | default |
|:-------------------|:----------------------------------------------------------------------------------------------------------------------------|:--------|
| depositWithFiles   | Indicates if the action is uploading files                                                                                  | FALSE   |
| otherUploadFolders | List of Folders (other than ‘data’ and ‘metadata’) to upload and which may contain files which should enrich others actions |         |

### 1.1.14 List of create-metadata-rmd options<a name= create-metadata-rmd />

| name          | definition                                                       | default |
|:--------------|:-----------------------------------------------------------------|:--------|
| template      | Rmarkdown template                                               | generic |
| output_format | output format generate by Rmarkdown template (e.g. ‘html’,‘pdf’) | html    |

# 2 Software

## 2.1 List of software supported by geoflow

| software_type                                            | definition                                                                | packages              |
|:---------------------------------------------------------|:--------------------------------------------------------------------------|:----------------------|
| [**dbi**](#211-dbi)<br>                                  | Data Base Interface powered by ‘DBI’ package                              | DBI,RSQLite,RPostgres |
| [**googledrive**](#212-googledrive)<br>                  | Google Drive access powered by ‘googledrive’ package                      | googledrive           |
| [**inspire**](#213-inspire)<br>                          | INSPIRE Metadata validator, powered by ‘geometa’ package                  | geometa               |
| [**csw**](#214-csw)<br>                                  | OGC Catalogue Service for the Web (CSW) client powered by ‘ows4R’ package | ows4R                 |
| [**wfs**](#215-wfs)<br>                                  | OGC Web Feature Service (WFS) client powered by ‘ows4R’ package           | ows4R                 |
| [**wps**](#216-wps)<br>                                  | OGC Web Processing Service (WPS) client powered by ‘ows4R’ package        | ows4R                 |
| [**geonetwork**](#217-geonetwork)<br>                    | GeoNetwork API Client, powered by ‘geonapi’ package                       | geonapi               |
| [**geoserver**](#218-geoserver)<br>                      | GeoServer REST API Client, powered by ‘geosapi’ package                   | geosapi               |
| [**zenodo**](#219-zenodo)<br>                            | Zenodo client powered by ‘zen4R’ package                                  | zen4R                 |
| [**sword_for_dataverse**](#2110-sword_for_dataverse)<br> | Dataverse SWORD API Client powered by ‘atom4R’ package                    | atom4R                |
| [**dataone**](#2111-dataone)<br>                         | DataONe API Client powered by ‘dataone’ package                           | dataone               |
| [**d4storagehub**](#2112-d4storagehub)<br>               | D4science storage hub API Client powered by ‘d4storagehub4R’ package      | d4storagehub4R        |
| [**gbif**](#2113-gbif)<br>                               | Gbif API Client powered by ‘rgbif’ package                                | rgbif                 |
| [**thredds**](#2114-thredds)<br>                         | Thredds data server API Client powered by ‘thredds’ package               | thredds               |
| [**openapi**](#2115-openapi)<br>                         | OpenAPI client powered by ‘rapiclient’ package                            | rapiclient            |
| [**ocs**](#2116-ocs)<br>                                 | Open Collaboration Services (OCS) client powered by ‘ocs4R’ package       | ocs4R                 |

### 2.1.1 dbi

#### 2.1.1.1 List of dbi parameters<a name= dbi />

| name     | label         | definition      |
|:---------|:--------------|:----------------|
| drv      | DBI driver    | DBI driver name |
| user     | Username      | Username        |
| password | Password      | Password        |
| host     | Hostname      | Hostname        |
| port     | Port number   | Port number     |
| dbname   | Database name | Database name   |

#### 2.1.1.2 List of dbi properties

| name        | label                              | definition                                                                                                                                                                                                                                                                                                        |
|:------------|:-----------------------------------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| onstart_sql | SQL ‘on-start’ script              | An SQL script to be run on workflow start                                                                                                                                                                                                                                                                         |
| onstart_r   | R ‘on-start’ SQL generating script | R instructions to generate a SQL. It should be made of 2 properties ‘script’ (nameof the R script) that should include a function standardized with parameter config (being theDBI software config) and will output a character representing the SQL. The name of the function is to specify in ‘fun’ property.   |
| onend_sql   | SQL ‘on-end’ script                | An SQL script to be run on workflow end                                                                                                                                                                                                                                                                           |
| onend_r     | R ‘on-end’ SQL generating script   | R instructions to generate a SQL. It should be made of 2 properties ‘script’ (name of the R script) that should include a function standardized with parameter config (being the DBI software config) and will output a character representing the SQL. The name of the function is to specify in ‘fun’ property. |

### 2.1.2 googledrive

#### 2.1.2.1 List of googledrive parameters<a name= googledrive />

| name  | label      | definition                                                                                                                                                                                                     |
|:------|:-----------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| email | Email      | User email to authenticate in Google Drive                                                                                                                                                                     |
| path  | Path       | An optional path within the Google drive repository. Default will be the root                                                                                                                                  |
| token | User token | The user authentication token. To get your token in R: gargle::token_fetch()![credentials](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;credentials "credentials")access_token |

#### 2.1.2.2 List of googledrive properties

*No properties available for this software*

### 2.1.3 inspire

#### 2.1.3.1 List of inspire parameters<a name= inspire />

| name   | label                          | definition                                                                                                   |
|:-------|:-------------------------------|:-------------------------------------------------------------------------------------------------------------|
| url    | INSPIRE Metadata validator URL | URL of the INSPIRE metadata validator instance. By default use ‘<https://inspire.ec.europa.eu/validator/v2>’ |
| apiKey | API Key                        | API user key to authenticate to INSPIRE API gateway                                                          |

#### 2.1.3.2 List of inspire properties

*No properties available for this software*

### 2.1.4 csw

#### 2.1.4.1 List of csw parameters<a name= csw />

| name           | label           | definition                                             |
|:---------------|:----------------|:-------------------------------------------------------|
| url            | URL             | CSW service endpoint URL                               |
| serviceVersion | Service version | CSW service version (‘2.0.2’ or ‘3.0’)                 |
| user           | Username        | Username for CSW authentication (optional)             |
| pwd            | Password        | Password for CSW authentication (optional)             |
| logger         | Logger          | Level for ‘ows4R’ logger messages (NULL,INFO or DEBUG) |

#### 2.1.4.2 List of csw properties

*No properties available for this software*

### 2.1.5 wfs

#### 2.1.5.1 List of wfs parameters<a name= wfs />

| name           | label           | definition                                                  |
|:---------------|:----------------|:------------------------------------------------------------|
| url            | URL             | WFS service endpoint URL                                    |
| serviceVersion | Service version | WFS service version (‘1.0.0’, ‘1.1.1’, ‘2.0’)               |
| user           | Username        | Username for WFS authentication (optional)                  |
| pwd            | Password        | Password for WFS authentication (optional)                  |
| logger         | Logger          | Level for ‘ows4R’ logger messages (NULL, ‘INFO’ or ‘DEBUG’) |

#### 2.1.5.2 List of wfs properties

*No properties available for this software*

### 2.1.6 wps

#### 2.1.6.1 List of wps parameters<a name= wps />

| name           | label           | definition                                                  |
|:---------------|:----------------|:------------------------------------------------------------|
| url            | URL             | WPS service endpoint URL                                    |
| serviceVersion | Service version | WPS service version (limited to ‘1.0.0’)                    |
| user           | Username        | Username for WPS authentication (optional)                  |
| pwd            | Password        | Password for WPS authentication (optional)                  |
| logger         | Logger          | Level for ‘ows4R’ logger messages (NULL, ‘INFO’ or ‘DEBUG’) |

#### 2.1.6.2 List of wps properties

*No properties available for this software*

### 2.1.7 geonetwork

#### 2.1.7.1 List of geonetwork parameters<a name= geonetwork />

| name    | label              | definition                                                    |
|:--------|:-------------------|:--------------------------------------------------------------|
| url     | URL                | GeoNetwork catalogue URL                                      |
| version | Geonetwork version | Geonetwork catalogue version                                  |
| user    | Username           | Username for GeoNetwork authentication                        |
| pwd     | Password           | Password for GeoNetwork authentication                        |
| logger  | Logger             | Level for ‘geonapi’ logger messages (NULL, ‘INFO’ or ‘DEBUG’) |

#### 2.1.7.2 List of geonetwork properties

*No properties available for this software*

### 2.1.8 geoserver

#### 2.1.8.1 List of geoserver parameters<a name= geoserver />

| name   | label    | definition                                                    |
|:-------|:---------|:--------------------------------------------------------------|
| url    | URL      | GeoServer application URL                                     |
| user   | Username | Username for GeoServer authentication                         |
| pwd    | Password | Password for GeoServer authentication                         |
| logger | Logger   | Level for ‘geosapi’ logger messages (NULL, ‘INFO’ or ‘DEBUG’) |

#### 2.1.8.2 List of geoserver properties

| name      | label     | definition                         |
|:----------|:----------|:-----------------------------------|
| workspace | Workspace | GeoServer workspace name           |
| store     | Store     | GeoServer data/coverage store name |

### 2.1.9 zenodo

#### 2.1.9.1 List of zenodo parameters<a name= zenodo />

| name   | label      | definition                                                                |
|:-------|:-----------|:--------------------------------------------------------------------------|
| url    | URL        | Zenodo API URL. For sandbox tests, use ‘<https://sandbox.zenodo.org/api>’ |
| token  | User token | Zenodo user authentication token.                                         |
| logger | Logger     | Level for ‘zen4R’ logger messages (NULL, ‘INFO’ or ‘DEBUG’)               |

#### 2.1.9.2 List of zenodo properties

| name  | label | definition                                                                                                                                                                                                                                                       |
|:------|:------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| clean | Clean | An option, to clean draft Zenodo deposits prior to any new deposit. To clean deposits, enable ‘run’, and optionally specify either a ‘query’ (ElasticSearch Zenodo query), a list of ‘doi’, or ‘community’ for whichyou want to restrain the cleaning operation. |

### 2.1.10 sword_for_dataverse

#### 2.1.10.1 List of sword_for_dataverse parameters<a name= sword_for_dataverse />

| name     | label      | definition                                                   |
|:---------|:-----------|:-------------------------------------------------------------|
| hostname | URL        | Dataverse base URL                                           |
| token    | User token | Dataverse user authentication token                          |
| logger   | Logger     | Level for ‘atom4R’ logger messages (NULL, ‘INFO’ or ‘DEBUG’) |

#### 2.1.10.2 List of sword_for_dataverse properties

| name      | label        | definition                                    |
|:----------|:-------------|:----------------------------------------------|
| dataverse | Dataverse ID | Dataverse id where to deposit/publish records |

### 2.1.11 dataone

#### 2.1.11.1 List of dataone parameters<a name= dataone />

| name  | label                 | definition               |
|:------|:----------------------|:-------------------------|
| x     | Contributing Node URL | Contributing Node URL    |
| y     | Member Node URL       | Member Node URL          |
| token | User token            | User Authorization token |

#### 2.1.11.2 List of dataone properties

*No properties available for this software*

### 2.1.12 d4storagehub

#### 2.1.12.1 List of d4storagehub parameters<a name= d4storagehub />

| name   | label      | definition                                                           |
|:-------|:-----------|:---------------------------------------------------------------------|
| token  | User token | D4Science storage hub user authentication token                      |
| logger | Logger     | Level for ‘d4storagehub4R’ logger messages (NULL, ‘INFO’ or ‘DEBUG’) |

#### 2.1.12.2 List of d4storagehub properties

| name      | label     | definition                           |
|:----------|:----------|:-------------------------------------|
| workspace | Workspace | D4Science storage hub workspace name |

### 2.1.13 gbif

#### 2.1.13.1 List of gbif parameters<a name= gbif />

| name  | label    | definition                             |
|:------|:---------|:---------------------------------------|
| user  | Username | Username for Gbif authentication       |
| pwd   | Password | Password for Gbif authentication       |
| email | Email    | Email address for sending notification |

#### 2.1.13.2 List of gbif properties

*No properties available for this software*

### 2.1.14 thredds

#### 2.1.14.1 List of thredds parameters<a name= thredds />

| name   | label       | definition                       |
|:-------|:------------|:---------------------------------|
| x      | Catalog URL | url of top level catalog request |
| prefix | Namespace   | the namespace to examine         |

#### 2.1.14.2 List of thredds properties

*No properties available for this software*

### 2.1.15 openapi

#### 2.1.15.1 List of openapi parameters<a name= openapi />

| name          | label              | definition                                                     |
|:--------------|:-------------------|:---------------------------------------------------------------|
| url           | OpenAPI URL        | url of the OpenAPI JSON definition (now limited to OpenAPI v2) |
| api_key_name  | Open API key name  | Name of the API key for registered uses                        |
| api_key_value | Open API key value | Value of the API key for registered uses (typically a token)   |

#### 2.1.15.2 List of openapi properties

*No properties available for this software*

### 2.1.16 ocs

#### 2.1.16.1 List of ocs parameters<a name= ocs />

| name   | label    | definition                                             |
|:-------|:---------|:-------------------------------------------------------|
| url    | URL      | OCS service endpoint URL                               |
| user   | Username | Username for user authentication                       |
| pwd    | Password | Password for user authentication                       |
| logger | Logger   | Level for ‘ows4R’ logger messages (NULL,INFO or DEBUG) |

#### 2.1.16.2 List of ocs properties

*No properties available for this software*
