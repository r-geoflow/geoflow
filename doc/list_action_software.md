geoflow – R engine to orchestrate and run geospatial (meta)data
workflows - List of actions and softwares
================

# 1 Actions

## 1.1 List of actions supported by geoflow

| id                                                                          | types                                                            | definition                                                                 | target | target\_dir | pid\_generator | packages                 |
| :-------------------------------------------------------------------------- | :--------------------------------------------------------------- | :------------------------------------------------------------------------- | :----- | :---------- | :------------- | :----------------------- |
| [**geometa-create-iso-19115**](#geometa-create-iso-19115)<br>               | Metadata production                                              | Produce an ISO/OGC 19115/19139 metadata object                             | entity | metadata    | FALSE          | geometa                  |
| [**geometa-create-iso-19110**](#geometa-create-iso-19110)<br>               | Metadata production                                              | Produce an ISO 19110/19139 metadata object                                 | entity | metadata    | FALSE          | geometa                  |
| [**ows4R-publish-iso-19139**](#ows4R-publish-iso-19139)<br>                 | Metadata publication                                             | Publish/Update an ISO/OGC 19139 metadata object using OGC CSW Protocol     | NA     | NA          | FALSE          | ows4R                    |
| [**geonapi-publish-iso-19139**](#geonapi-publish-iso-19139)<br>             | Metadata publication                                             | Publish/Update an ISO/OGC 19139 metadata object with GeoNetwork API        | NA     | NA          | FALSE          | geonapi                  |
| [**geosapi-publish-ogc-services**](#geosapi-publish-ogc-services)<br>       | Data upload,Data publication,Metadata publication                | Publish vector data to GeoServer OGC web-services (WMS/WFS)                | NA     | NA          | FALSE          | geosapi                  |
| [**zen4R-deposit-record**](#zen4R-deposit-record)<br>                       | Data upload,Data publication,Metadata publication,DOI assignment | Deposits/Publish data and/or metadata in the Zenodo infrastructure         | job    | zenodo      | TRUE           | zen4R                    |
| [**atom4R-dataverse-deposit-record**](#atom4R-dataverse-deposit-record)<br> | Data upload,Data publication,Metadata publication,DOI assignment | Deposits/Publish data and/or metetadata on a Dataverse using the Sword API | job    | dataverse   | TRUE           | atom4R                   |
| [**dataone-upload-datapackage**](#dataone-upload-datapackage)<br>           | Data upload,Data publication,Metadata publication,DOI assignment | Uploads a data package to a DataOne metacat node                           | job    | dataone     | TRUE           | mime,datapack,dataone    |
| [**sf-write-generic**](#sf-write-generic)<br>                               | Data writing,Data upload                                         | Import features data into several formats                                  | entity | data        | FALSE          | sf,DBI,RSQLite,RPostgres |
| [**sf-write-dbi**](#sf-write-dbi)<br>                                       | Data writing,Data upload                                         | Import features data into Postgres/Postgis                                 | NA     | NA          | FALSE          | sf,DBI,RSQLite,RPostgres |
| [**sf-write-shp**](#sf-write-shp)<br>                                       | Data writing                                                     | Import features data and zip files                                         | entity | data        | FALSE          | sf                       |
| [**eml-create-eml**](#eml-create-eml)<br>                                   | Metadata production                                              | Produce an EML metadata object                                             | entity | metadata    | FALSE          | EML,emld                 |

### 1.1.1 List of geometa-create-iso-19115 options<a name= geometa-create-iso-19115 />

| name               | definition                                                                                | default   |
| :----------------- | :---------------------------------------------------------------------------------------- | :-------- |
| doi                | Add entity DOI - if defined - as metadata identifier and online resource                  | FALSE     |
| doi\_thumbnail     | if option ‘doi’ is true and this option enabled, a DOI thumbnail will be added            | FALSE     |
| inspire            | Validates ISO 19139 metadata with INSPIRE reference validator                             | FALSE     |
| logo               | Add configure profile logo(s) - if defined - as metadata thumbnail(s)                     | FALSE     |
| addfeatures        | Add entity data features - if defined - as metadata bounding polygon(s)                   | FALSE     |
| featureId          | ID of entity data features used to identify bounding polygon(s) with option ‘addfeatures’ | NA        |
| subject\_geography | Identifier of the subject handling a Geographic coverage.                                 | geography |

### 1.1.2 List of geometa-create-iso-19110 options<a name= geometa-create-iso-19110 />

| name                                     | definition                                                                          | default |
| :--------------------------------------- | :---------------------------------------------------------------------------------- | :------ |
| doi                                      | Add entity DOI - if defined - as metadata identifier and online resource            | FALSE   |
| exclude\_attributes                      | Attributes that should be excluded from the ISO 19110 production                    | NA      |
| exclude\_attributes\_not\_in\_dictionary | Enable to exclude all attributes/variables not referenced as dictionary/featuretype | FALSE   |
| exclude\_values\_for\_attributes         | Attribute names for which listed values should not be produced                      | NA      |
| extra\_attributes                        | Extra attributes to add as feature catalog attributes although not in data          | NA      |
| default\_min\_occurs                     | The default min occurs value for feature attributes cardinality                     | 1       |
| default\_max\_occurs                     | The default max occurs value for feature attribute cardinality                      | Inf     |

### 1.1.3 List of ows4R-publish-iso-19139 options<a name= ows4R-publish-iso-19139 />

| name             | definition                                                                       | default |
| :--------------- | :------------------------------------------------------------------------------- | :------ |
| geometa\_inspire | Validates ISO 19139 metadata with INSPIRE reference validator before publication | FALSE   |

### 1.1.4 List of geonapi-publish-iso-19139 options<a name= geonapi-publish-iso-19139 />

| name             | definition                                                                       | default               |
| :--------------- | :------------------------------------------------------------------------------- | :-------------------- |
| geometa\_inspire | Validates ISO 19139 metadata with INSPIRE reference validator before publication | FALSE                 |
| privileges       | Geonetwork privileges to set for the metadata to be published                    | view,dynamic,featured |
| group            | Geonetwork user group to which the metadata should be associated                 | 1                     |
| category         | Category of metadata resources to which the metadata record should be associated | datasets              |

### 1.1.5 List of geosapi-publish-ogc-services options<a name= geosapi-publish-ogc-services />

*No options available for this action*

### 1.1.6 List of zen4R-deposit-record options<a name= zen4R-deposit-record />

| name             | definition                                                                                  | default |
| :--------------- | :------------------------------------------------------------------------------------------ | :------ |
| depositWithFiles | Indicates if the action is uploading files                                                  | FALSE   |
| publish          | Indicates if the action should publish the deposit. Requires ‘depositWithFiles’ set to TRUE | FALSE   |
| deleteOldFiles   | Indicates if the action should delete old files prior upload new files                      | TRUE    |
| update\_metadata | For an existing deposit, indicates if metadata elements should be updated                   | TRUE    |
| update\_files    | For an existing deposit, indicates if files should be updated                               | TRUE    |
| communities      | One or more communities to which the deposit should be associated                           | NA      |

### 1.1.7 List of atom4R-dataverse-deposit-record options<a name= atom4R-dataverse-deposit-record />

| name             | definition                                                                                  | default |
| :--------------- | :------------------------------------------------------------------------------------------ | :------ |
| depositWithFiles | Indicates if the action is uploading files                                                  | FALSE   |
| publish          | Indicates if the action should publish the deposit. Requires ‘depositWithFiles’ set to TRUE | FALSE   |
| deleteOldFiles   | Indicates if the action should delete old files prior upload new files                      | TRUE    |
| update\_metadata | For an existing deposit, indicates if metadata elements should be updated                   | TRUE    |
| update\_files    | For an existing deposit, indicates if files should be updated                               | TRUE    |

### 1.1.8 List of dataone-upload-datapackage options<a name= dataone-upload-datapackage />

*No options available for this action*

### 1.1.9 List of sf-write-generic options<a name= sf-write-generic />

| name          | definition                                                                    | default |
| :------------ | :---------------------------------------------------------------------------- | :------ |
| type          | format to convert                                                             | NA      |
| createIndexes | create indexes for columns                                                    | FALSE   |
| overwrite     | Overwrite policy                                                              | TRUE    |
| append        | Append policy                                                                 | FALSE   |
| chunk.size    | Size of DB upload data chunk. Default is 0L, meaning no chunking is operated. | 0       |

### 1.1.10 List of sf-write-dbi options<a name= sf-write-dbi />

| name          | definition                                                                    | default |
| :------------ | :---------------------------------------------------------------------------- | :------ |
| createIndexes | create indexes for columns                                                    | FALSE   |
| overwrite     | Overwrite policy                                                              | TRUE    |
| append        | Append policy                                                                 | FALSE   |
| chunk.size    | Size of DB upload data chunk. Default is 0L, meaning no chunking is operated. | 0       |

### 1.1.11 List of sf-write-shp options<a name= sf-write-shp />

*No options available for this action*

### 1.1.12 List of eml-create-eml options<a name= eml-create-eml />

| name              | definition                                                 | default  |
| :---------------- | :--------------------------------------------------------- | :------- |
| subject\_taxonomy | Identifier of the subject handling the Taxonomic coverage. | taxonomy |

# 2 Software

## 2.1 List of software supported by geoflow

| software\_type                                        | definition                                                                | packages              |
| :---------------------------------------------------- | :------------------------------------------------------------------------ | :-------------------- |
| [**dbi**](#dbi)<br>                                   | Data Base Interface powered by ‘DBI’ package                              | DBI,RSQLite,RPostgres |
| [**googledrive**](#googledrive)<br>                   | Google Drive access powered by ‘googledrive’ package                      | googledrive           |
| [**csw**](#csw)<br>                                   | OGC Catalogue Service for the Web (CSW) client powered by ‘ows4R’ package | ows4R                 |
| [**wfs**](#wfs)<br>                                   | OGC Web Feature Service (WFS) client powered by ‘ows4R’ package           | ows4R                 |
| [**geonetwork**](#geonetwork)<br>                     | GeoNetwork API Client, powered by ‘geonapi’ package                       | geonapi               |
| [**geoserver**](#geoserver)<br>                       | GeoServer REST API Client, powered by ‘geosapi’ package                   | geosapi               |
| [**zenodo**](#zenodo)<br>                             | Zenodo client powered by ‘zen4R’ package                                  | zen4R                 |
| [**sword\_for\_dataverse**](#sword_for_dataverse)<br> | Dataverse SWORD API Client powered by ‘atom4R’ package                    | atom4R                |
| [**dataone**](#dataone)<br>                           | DataONe API Client powered by ‘dataone’ package                           | dataone               |
| [**d4storagehub**](#d4storagehub)<br>                 | D4science storage hub API Client powered by ‘d4storagehub4R’ package      | d4storagehub4R        |

### 2.1.1 List of dbi parameters<a name= dbi />

| name     | definition      |
| :------- | :-------------- |
| drv      | DBI driver name |
| user     | Username        |
| password | Password        |
| host     | Hostname        |
| port     | Port number     |
| dbname   | Database name   |

### 2.1.2 List of dbi properties

| name         | definition                                                                                                                                                                                                                                                                                                      |
| :----------- | :-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| onstart\_sql | An SQL script to be run on workflow start                                                                                                                                                                                                                                                                       |
| onstart\_r   | R instructions to generate a SQL. It should be made of 2 properties ‘script’ (nameof the R script) that should include a function standardized with parameter config (being theDBI software config) and will outpout a character representing the SQL. The name of the function is to specify in ‘fun’ property |

### 2.1.3 List of googledrive parameters<a name= googledrive />

| name  | definition                                                                                                |
| :---- | :-------------------------------------------------------------------------------------------------------- |
| email | User email to authenticate in Google Drive                                                                |
| path  | An optional path within the Google drive repository. Default will be the root                             |
| token | The user authentication token. To get your token in R: gargle::token\_fetch()\(credentials\)access\_token |

### 2.1.4 List of googledrive properties

*No properties available for this software*

### 2.1.5 List of csw parameters<a name= csw />

| name           | definition                                             |
| :------------- | :----------------------------------------------------- |
| url            | CSW service endpoint URL                               |
| serviceVersion | CSW service version (‘2.0.2’ or ‘3.0’)                 |
| user           | Username for CSW authentication                        |
| pwd            | Password for CSW authentication                        |
| logger         | Level for ‘ows4R’ logger messages (NULL,INFO or DEBUG) |

### 2.1.6 List of csw properties

*No properties available for this software*

### 2.1.7 List of wfs parameters<a name= wfs />

| name           | definition                                                  |
| :------------- | :---------------------------------------------------------- |
| url            | WFS service endpoint URL                                    |
| serviceVersion | WFS service version (‘1.0.0’, ‘1.1.1’, ‘2.0’)               |
| user           | Username for WFS authentication                             |
| pwd            | Password for WFS authentication                             |
| logger         | Level for ‘ows4R’ logger messages (NULL, ‘INFO’ or ‘DEBUG’) |

### 2.1.8 List of wfs properties

*No properties available for this software*

### 2.1.9 List of geonetwork parameters<a name= geonetwork />

| name    | definition                                                    |
| :------ | :------------------------------------------------------------ |
| url     | GeoNetwork catalogue URL                                      |
| version | Geonetwork catalogue version                                  |
| user    | Username for GeoNetwork authentication                        |
| pwd     | Password for GeoNetwork authentication                        |
| logger  | Level for ‘geonapi’ logger messages (NULL, ‘INFO’ or ‘DEBUG’) |

### 2.1.10 List of geonetwork properties

*No properties available for this software*

### 2.1.11 List of geoserver parameters<a name= geoserver />

| name   | definition                                                    |
| :----- | :------------------------------------------------------------ |
| url    | GeoServer application URL                                     |
| user   | Username for GeoServer authentication                         |
| pwd    | Password for GeoServer authentication                         |
| logger | Level for ‘geosapi’ logger messages (NULL, ‘INFO’ or ‘DEBUG’) |

### 2.1.12 List of geoserver properties

| name      | definition               |
| :-------- | :----------------------- |
| workspace | GeoServer workspace name |
| datastore | GeoServer datastore name |

### 2.1.13 List of zenodo parameters<a name= zenodo />

| name   | definition                                                                |
| :----- | :------------------------------------------------------------------------ |
| url    | Zenodo API URL. For sandbox tests, use ‘<https://sandbox.zenodo.org/api>’ |
| token  | Zenodo user authentication token.                                         |
| logger | Level for ‘zen4R’ logger messages (NULL, ‘INFO’ or ‘DEBUG’)               |

### 2.1.14 List of zenodo properties

| name  | definition                                                                                                                                                                                                                                                       |
| :---- | :--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| clean | An option, to clean draft Zenodo deposits prior to any new deposit. To clean deposits, enable ‘run’, and optionally specify either a ‘query’ (ElasticSearch Zenodo query), a list of ‘doi’, or ‘community’ for whichyou want to restrain the cleaning operation. |

### 2.1.15 List of sword\_for\_dataverse parameters<a name= sword_for_dataverse />

| name     | definition                                                   |
| :------- | :----------------------------------------------------------- |
| hostname | Dataverse base URL                                           |
| token    | Dataverse user authentication token                          |
| logger   | Level for ‘atom4R’ logger messages (NULL, ‘INFO’ or ‘DEBUG’) |

### 2.1.16 List of sword\_for\_dataverse properties

| name      | definition                                    |
| :-------- | :-------------------------------------------- |
| dataverse | Dataverse id where to deposit/publish records |

### 2.1.17 List of dataone parameters<a name= dataone />

| name  | definition            |
| :---- | :-------------------- |
| x     | Contributing Node URL |
| y     | Member Node URL       |
| token | Authorization token   |

### 2.1.18 List of dataone properties

*No properties available for this software*

### 2.1.19 List of d4storagehub parameters<a name= d4storagehub />

| name   | definition                                                           |
| :----- | :------------------------------------------------------------------- |
| token  | D4Science storage hub user authentication token                      |
| logger | Level for ‘d4storagehub4R’ logger messages (NULL, ‘INFO’ or ‘DEBUG’) |

### 2.1.20 List of d4storagehub properties

*No properties available for this software*
