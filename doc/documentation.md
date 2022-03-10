geoflow – R engine to orchestrate and run geospatial (meta)data
workflows
================

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3138920.svg)](https://doi.org/10.5281/zenodo.3138920)

R engine to orchestrate and run geospatial (meta)data workflows

-----

Many thanks to the following organizations that have provided fundings
for strenghtening the `geoflow` package:

<div style="float:left;">

<a href="http://www.fao.org"><img height=200 width=200 src="http://www.fao.org/fileadmin/templates/family-farming-decade/images/FAO-IFAD-Logos/FAO-Logo-EN.svg">
<a href="https://en.ird.fr/"><img src="http://www.umr-marbec.fr/images/logo-ird-en.png" height=200 width=200/>
<a href="https://www.inrae.fr"><img height=200 width=200 src="https://www.inrae.fr/themes/custom/inrae_socle/logo.svg"></a>

</div>

<br>

<div style="float:left;">

<a href="http://www.cnrs.fr"><img src="http://www.cnrs.fr/themes/custom/cnrs/logo.svg" height=200 width=200/></a>
<a href="https://inee.cnrs.fr/fr/zones-ateliers"><img src="https://inee.cnrs.fr/sites/institut_inee/files/inline-images/logo-za_0_0.jpg" height=150 width=300/></a>

</div>

The following projects have contributed to strenghten `geoflow`:

<a href="https://www.blue-cloud.org"><img height=100 width=300 src="https://www.blue-cloud.org/sites/all/themes/arcadia/logo.png"/></a>

*Blue-Cloud has received funding from the European Union’s Horizon
programme call BG-07-2019-2020, topic: \[A\] 2019 - Blue Cloud services,
Grant Agreement No.862409.*

For geoflow sponsoring/funding new developments, enhancements, support
requests, please contact me by
[e-mail](mailto:emmanuel.blondel1@gmail.com) \*\*\*

**Table of contents**

[**1. Overview**](#package_overview)<br/> [**2. Package
status**](#package_status)<br/> [**3. Credits**](#package_credits)<br/>
[**4. User guide**](#user_guide)<br/>    [4.1 How to install
geoflow](#install_guide)<br/>    [4.2 How to use
geoflow](#geoflow_execute)<br/>    [4.3 Description of a geoflow
configuration](#geoflow_config)<br/>       [4.3.1 Overall
structure](#geoflow_config_structure)<br/>       [4.3.2 Configuration
components – `profile`](#geoflow_config_profile)<br/>       [4.3.3
Configuration components – `metadata`](#geoflow_config_metadata)<br/>
      [4.3.4 Configuration components –
`software`](#geoflow_config_software)<br/>       [4.3.5 Configuration
components – `actions`](#geoflow_config_actions)<br/>       [4.3.6
Configuration components – `registers`](#geoflow_config_registers)<br/>
   [4.4 How to create a geoflow configuration
file](#geoflow_config_create)<br/>       [4.4.1 Create manually a
configuration file](#geoflow_config_manual)<br/>       [4.4.2 Use the
geoflow-shiny R Shiny application](#geoflow_config_shiny)<br/> [**5.
Issue reporting**](#package_issues)<br/>

<a name="package_overview"/>

### 1\. Overview and vision

-----

The principle of geoflow is to offer a simple framework in R to execute
and orchestrate geospatial (meta)data management and publication tasks
in an automated way.

<a name="package_status"/>

### 2\. Development status

-----

On GitHub.

<a name="package_credits"/>

### 3\. Credits

-----

Copyright – 2019, Emmanuel Blondel

Package distributed under MIT license.

If you use `geoflow`, w would be very grateful if you can add a citation
in your published work. By citing `geoflow`, beyond acknowledging the
work, you contribute to make it more visible and guarantee its growing
and sustainability. For citation, please use the DOI:
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3138920.svg)](https://doi.org/10.5281/zenodo.3138920)

<a name="user_guide"/>

### 4\. User guide

-----

<a name="install_guide"/>

#### 4.1 How to install geoflow

For now, the package can be installed from Github

``` r
install.packages("remotes")
```

Once the `remotes` package loaded, you can use the install\_github to
install `geoflow`. By default, package will be installed from `master`
which is the current version in development (likely to be unstable).

``` r
require("remotes")
install_github("eblondel/geoflow", dependencies = c("Depends", "Imports"))
```

<a name="geoflow_execute"/>

#### 4.2 How to use geoflow in R

In R, using `geoflow` consists essentially in running the function
\`\`executeWorkflow", which takes a main parameter: the name of a
configuration file in JSON format. An optional *dir* parameter can be
specified to instruct geoflow to store execution files in:

``` r
executeWorkflow("config.json", dir = NULL)
```

The workflow that is going to be executed is entirely described in a
configuration file. The main preparatory work of the data manager will
then to prepare the configuration file, depending on the tasks to
perform.

Prior to the execution, it also possible to check that the configuration
is ready to execute geoflow tasks. For that, once the configuration is
ready, it is possible to check it with the the function `initWorkflow`:

``` r
config <- initWorkflow("config.json")
```

> Note: The [geoflow-shiny](https://github.com/eblondel/geoflow-shiny)
> offers a graphic interface to help configuring the workflow in a
> user-friendly manner. If you are not familiar with JSON format, this
> shiny application can make your life easier.

<a name="geoflow_config"/>

#### 4.3 Description of a geoflow configuration

Before creating a configuration file first let’s describe how the
`geoflow` is structured and what are the key concepts.

<a name="geoflow_config_structure"/>

##### 4.3.1 Overall structure

###### Description

A geoflow configuration contains several parts that are defined here
below.

| Name                                     | Definition                                                                                                                                                                                                                                                                         | Optional/Required               |
| ---------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------- |
| [**profile**](#geoflow_config_profile)   | Global metadata workflow. Information that is common to all entities in case of mode *entity*, and that can be exploited in some of the actions. e.g. add a project logo for all dataset descriptions.                                                                             | Required                        |
| [**metadata**](#geoflow_config_metadata) | Part where the *entity* set is defined, to be used for executing actions in mode *entity*.                                                                                                                                                                                         | Required with *entity* mode     |
| [**software**](#geoflow_config_software) | Part where the software to interact with will be defined. It can be a software from where the user wants to get data, or a software where to publish data using [geoflow](https://github.com/eblondel/geoflow) e.g. a GeoNetwork metadata catalogue, a GeoServer, etc.             | Required for publishing actions |
| [**actions**](#geoflow_config_actions)   | Part where the *actions* to use are defined. These can be source R scripts in case of the *raw* mode, or entity-based actions in case of mode *entity*. An action put in the list can be enabled/disabled and parameterized with a set of options that is specific to each action. | Required                        |

###### JSON

If we take the different blocks that define the structure of a geoflow
configuration (as introduced earlier), the skeleton of the JSON
configuration file will look like this:

``` json
{
  "profile": { <global profile (metadata) defined here> },
  "metadata": { <metadata sources defined here> },
  "software": [ <pieces of software defined here> ],
  "actions": [ <actions defined here>  ]
}
```

<a name="geoflow_config_profile"/>

##### 4.3.2 Configuration components – `profile`

###### Description

The `profile` is where global workflow metadata is defined. This
includes information about the workflow execution as well as workflow
metadata elements that can be shared globally to all *entities* managed
and *actions* applied on them. See below the profile properties that
can/should be defined:

| Name             | Definition                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 | Optional/Required |
| ---------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | ----------------- |
| **id**           | A string identifier/name for the workflow                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  | Required          |
| **mode**         | A string, either ‘raw’ or ‘entity’ that defines the workflow mode: <br>\* *raw* mode: simple mode that allows to trigger basic tasks with R (known in geoflow as *actions*) in sequential way. This mode can be used by users that just want to chain R scripts. <br>\* *entity* mode: mode were all the *actions* will be performed based on a set of entities. In geoflow, an `entity` includes both metadata and data elements. In most of cases, an *entity* will describe a dataset for which we want to perform actions such as metadata handling/publishing in a web metadata catalogue, spatialdata upload in Geoserver, etc etc. With this mode, `geoflow` will take each *entity* for which a set of *actions* will be executed. | Required          |
| **name**         | A name for the workflow                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    | Optional          |
| **project**      | A project name for the workflow                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            | Optional          |
| **organization** | An organization name for the workflow                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      | Optional          |
| **logos**        | One or more logo URLs to reference.As now, this will be shared in actions such as `geometa_create_iso_19115` where they are used as metadata graphic overviews.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            | Optional          |
| **options**      | One or more options for the workflow execution                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             | Optional          |

The `options` are by definition optional. The table below defines the
possible geoflow global options:

| Name               | Definition                                                                                                                               | Default value |
| ------------------ | ---------------------------------------------------------------------------------------------------------------------------------------- | ------------- |
| `line_separator`   | Defines the suite of characters used for splitting metadata components with a single tabular cell of an *entity* (eg. Description field) | \*\*\_\*      |
| `skipFileDownload` | Indicates whether data associated to an entity should be download                                                                        | **false**     |

Note on the **mode**:

At its earliest stage, [geoflow](https://github.com/eblondel/geoflow)
was designed to chain a set of processings handled by different scripts.
This is known as *raw* mode, where the user just wants to use geoflow to
chain some tasks with a set of R scripts.

In order to facilitate the management of datasets within Spatial Data
Infrastructures (SDI), including their processing, publication and
description with proper metadata, a new mode called *entity* was
introduced. The concept of *entity* refers to the description of a
dataset or subset of it for which the user wants to perform actions. In
this mode, each action defined in geoflow will be executed for each
entity of the list of entities that will be defined in the
[**metadata**](#geoflow_config_metadata) configuration part.

###### JSON

JSON snippet of *profile*:

``` json
"profile": {
  "id": "my-workflow-identifier",
  "name": "My workflow",
    "project": "Test geoflow project",
    "organization": "My organization",
    "logos": [
        "https://via.placeholder.com/300x150.png/09f/fff?text=geometa",
        "https://via.placeholder.com/300x150.png/09f/fff?text=ows4R"
    ],
  "mode": "entity",
  "options": {
    "line_separator": "_\n",
    "skipFileDownload": false
  }
}
```

<a name="geoflow_config_metadata"/>

##### 4.3.3 Configuration components – `metadata`

###### Description

The metadata part is the section of the workflow configuration where to
define the sources of metadata content. Such content is split into three
categories: \* *entities*: source(s) for the list of entities, where
each *entity* represents the metadata \* *contacts*: source(s) for the
directory of contacts, referenced with roles in the dataset metadata \*
*dictionary*: source(s) for the data dictionaries, used to describe
datasets (eg. for producing ISO 19110)

The configuration consists in declaring the `source` (a file or URL) and
the `handler`, ie the source format.

The sources of *entities*, *contacts* and *dictionary* can be handled
for different `handlers` (for the timebeing `gsheet` - for Google
spreadsheets, `csv`, `excel` files, or a `dbi` source). The list of
`entity` and `contact` handlers can be retrieved in R with
`list_entity_handlers()`, `list_contact_handlers()` and
`list_dictionary_handlers()`. For the time being,
[geoflow](https://github.com/eblondel/geoflow) provides basic format
handlers.

  - List of `entity` handlers supported by geoflow:

| id     | definition                                                      | packages              |
| :----- | :-------------------------------------------------------------- | :-------------------- |
| csv    | Handle metadata entities from a CSV file                        |                       |
| excel  | Handle metadata entities from a Microsoft Excel (xls,xlsx) file | readxl                |
| gsheet | Handle metadata entities from a Google spreadsheet              | gsheet                |
| dbi    | Handle metadata entities from a DB source                       | DBI,RSQLite,RPostgres |

List of entity handlers supported by geoflow

  - List of `contact` handlers supported by geoflow:

| id     | definition                                                      | packages              |
| :----- | :-------------------------------------------------------------- | :-------------------- |
| csv    | Handle metadata contacts from a CSV file                        |                       |
| excel  | Handle metadata contacts from a Microsoft Excel (xls,xlsx) file | readxl                |
| gsheet | Handle metadata contacts from a Google spreadsheet              | gsheet                |
| dbi    | Handle metadata contacts from a DB source                       | DBI,RSQLite,RPostgres |

List of contact handlers supported by geoflow

  - List of `dictionary` handlers supported by geoflow:

| id     | definition                                               | packages              |
| :----- | :------------------------------------------------------- | :-------------------- |
| csv    | Handle dictionary from a CSV file                        |                       |
| excel  | Handle dictionary from a Microsoft Excel (xls,xlsx) file | readxl                |
| gsheet | Handle dictionary from a Google spreadsheet              | gsheet                |
| dbi    | Handle dictionary from a DB source                       | DBI,RSQLite,RPostgres |

List of dictionary handlers supported by geoflow

> Note: The list of handlers is expected to be extended in the future,
> eg LDAP handler for contacts.

###### JSON

  - JSON snippet for *entities* handled with a Google spreadsheet:

<!-- end list -->

``` json
    "entities": [
      {
        "handler": "gsheet",
        "source": "https://docs.google.com/spreadsheets/d/1iG7i3CE0W9zVM3QxWfCjoYbqj1dQvKsMnER6kqwDiqM/edit?usp=sharing"
      }
    ]
```

  - JSON snippet for *contacts* handled with a Google spreadsheet:

<!-- end list -->

``` json
    "contacts" : [
      {
        "handler": "gsheet",
        "source": "https://docs.google.com/spreadsheets/d/144NmGsikdIRE578IN0McK9uZEUHZdBuZcGy1pJS6nAg/edit?usp=sharing"
      }
    ]
```

  - JSON snippet for the metadata part (including *entities* and
    *contacts*)

<!-- end list -->

``` json
  "metadata": {
    "entities": [
      {
        "handler": "gsheet",
        "source": "https://docs.google.com/spreadsheets/d/1iG7i3CE0W9zVM3QxWfCjoYbqj1dQvKsMnER6kqwDiqM/edit?usp=sharing"
      }
    ],
    "contacts" : [
      {
        "handler": "gsheet",
        "source": "https://docs.google.com/spreadsheets/d/144NmGsikdIRE578IN0McK9uZEUHZdBuZcGy1pJS6nAg/edit?usp=sharing"
      }
    ]
  }
```

  - JSON snippet for custom handlers

It is possible to use a custom `handler` function provided by the user.
For this, the `handler` should be the *name* of the R function to be
provided by an R script. The R script must be defined in a extra
property named `script`. In this configuration, the `source` property
becomes optional (it could be hardcoded in the user’s `handler```
function if this`source\`\` is not expected to change from one
configuration to another).

The JSON configuration snippet for a custom contact LDAP `handler` would
look like this:

``` json
    "contacts" : [
      {
        "handler": "my_ldap_function_to_load_contacts",
        "source": "my_ldap_endpoint",
        "script": "my_ldap_script.R"
      }
    ]
```

In R, the `my_ldap_function_to_load_contacts` function writen by user in
`my_ldap_script.R` should implement a function having 2 arguments:
`config` (to access the overall workflow configuration object loaded
with `initWorkflow`), `source` (the source as defined in above JSON
snippet):

``` r
  my_ldap_function_to_load_contacts <- function(config, source){
    contacts <- list()
    #here some custom business logic to load contacts from a LDAP
    #....
    return(contacts)
  }
```

<a name="geoflow_config_software"/>

##### 4.3.4 Configuration components – `software`

###### Description

The `software` part of the configuration consists in listing the pieces
of software needed for the workflow.

  - **List of software managed by geoflow**

By default [geoflow](https://github.com/eblondel/geoflow) manages
specific `software` to interact with. These software are essentially R
*interfaces* to common tools (databases, web-applications, APIs). The
list of \``software` managed by geoflow can be retrieved in R with
`list_software()`. The list of software managed by `geoflow` are:

| software\_type        | definition                                                                | packages              |
| :-------------------- | :------------------------------------------------------------------------ | :-------------------- |
| dbi                   | Data Base Interface powered by ‘DBI’ package                              | DBI,RSQLite,RPostgres |
| googledrive           | Google Drive access powered by ‘googledrive’ package                      | googledrive           |
| csw                   | OGC Catalogue Service for the Web (CSW) client powered by ‘ows4R’ package | ows4R                 |
| wfs                   | OGC Web Feature Service (WFS) client powered by ‘ows4R’ package           | ows4R                 |
| geonetwork            | GeoNetwork API Client, powered by ‘geonapi’ package                       | geonapi               |
| geoserver             | GeoServer REST API Client, powered by ‘geosapi’ package                   | geosapi               |
| zenodo                | Zenodo client powered by ‘zen4R’ package                                  | zen4R                 |
| sword\_for\_dataverse | Dataverse SWORD API Client powered by ‘atom4R’ package                    | atom4R                |
| dataone               | DataONe API Client powered by ‘dataone’ package                           | dataone               |

List of software supported by geoflow

  - **How to configure a software**

To configure a piece of software, the latter should be provided with
various elements: \* an **id**: it should be a user string id to
identify the software in question. \* a **type**: string, either *input*
(software to use as *source*, to fetch data, eg. a database) or “output”
(software to use as *target*, to publish/manage data, eg a metadata
catalogue) \* a **software\_type**: a string identifying the software
types as managed by geoflow (see above table). For example, to declare a
GeoServer software, the `software_type` with `id` “geoserver” will be
used. \* a set of **parameters**: that depend on the type of software
configured. For the software managed by geoflow, it is possible to
interrogate geoflow to know which parameters are needed given a
`software_type` by doing in R
`list_software_parameters(<software_type>)` (e.g. for GeoServer type in
R `list_software_parameters("geoserver")` \* a set of **properties**:
that depend on the type of software configured. Those are extra
configuration elements to use with the software considered. For the
software managed by geoflow, it is possible to interrogate geoflow to
know which properties can be used fiven a `software_type` by doint in R
`list_software_properties(<software_type>)` (e.g. for Geoserver type in
R `list_software_properties("geoserver")`)

Let’s look at “geoserver” `parameters` that need to be declared in order
to interact with the Geoserver software:

``` r
geoflow::list_software_parameters("geoserver")
```

    ##     name                                                    definition
    ## 1    url                                     GeoServer application URL
    ## 2   user                         Username for GeoServer authentication
    ## 3    pwd                         Password for GeoServer authentication
    ## 4 logger Level for 'geosapi' logger messages (NULL, 'INFO' or 'DEBUG')

In similar way, we can list the properties required to configure a data
publication in Geoserver:

``` r
geoflow::list_software_properties("geoserver")
```

    ##        name               definition
    ## 1 workspace GeoServer workspace name
    ## 2 store GeoServer store name

###### JSON

  - JSON snippet for declaring a `database` (*input* software) for data
    fetching:

<!-- end list -->

``` json
  {
        "id": "my-database",
        "type": "input",
        "software_type": "dbi",
        "parameters": {
            "drv": "PostgreSQL",
            "user": "user",
            "password": "pwd",
            "host": "localhost",
            "port": "5432",
            "dbname": "mydb"
        }
  }
```

  - JSON snippet for declaring a `geoserver` (*output* software) for
    data publishing:

<!-- end list -->

``` json
  {
        "id": "my-geoserver",
        "type": "output",
        "software_type": "geoserver",
        "parameters": {
            "url": "http://localhost:800/geoserver",
            "user": "admin",
            "pwd": "geoserver",
            "logger": "DEBUG"
        },
        "properties" : {
            "workspace": "my_geoserver_workspace",
            "store": "my_geoserver_store"
        }
    }
```

  - JSON snippet for the overall “software” component

Since it is a list of software, the base JSON definition will be an
*array* (using square brackets `[ ]`):

``` json
"software": [
     <here will be listed the pieces of software>
]
```

  - JSON snippet for the overall “software” component (including one
    *input* - a database - and one *output* - a geoserver -)

<!-- end list -->

``` json
"software": [
  {
        "id": "my-database",
        "type": "input",
        "software_type": "dbi",
        "parameters": {
            "drv": "PostgreSQL",
            "user": "user",
            "password": "pwd",
            "host": "localhost",
            "port": "5432",
            "dbname": "mydb"
        }
  },
  {
        "id": "my-geoserver",
        "type": "output",
        "software_type": "geoserver",
        "parameters": {
            "url": "http://localhost:800/geoserver",
            "user": "admin",
            "pwd": "geoserver",
            "logger": "DEBUG"
        },
        "properties" : {
            "workspace": "my_geoserver_workspace",
            "store": "my_geoserver_store"
        }
  }
]
```

  - **How to use a user’s custom software**

DOCUMENTATION IN PREPARATION

<a name="geoflow_config_actions"/>

##### 4.3.5 Configuration components – `actions`

###### Description

The `actions` part of the configuration consists in listing and enabling
the tasks (named *actions* in geoflow) that should be run by the
workflow.

Each *action* is tightly associated with some *software* declared in the
configuration. In case an *action* is configured, and the needed
*software* is not configured or misconfigured, geoflow will stop at
initialization phase (`initWorkdlow`) and return an error. To check that
the configuration of *actions* and *software* is ok, the user can
trigger `initWorkflow` as follows:

``` r
  config <- initWorkflow("config.json")
```

  - **List of actions managed by geoflow**

By default [geoflow](https://github.com/eblondel/geoflow) manages
specific `actions` to run. These *actions* are essentially R turnkey
functions to interact with common tools (databases, web-applications,
APIs). The list of `actions` managed by geoflow can be retrieved in R
with `list_actions()`. The list of actions managed by `geoflow` are:

| id                              | types                                                            | definition                                                                 | target | target\_dir | pid\_generator | packages                 |
| :------------------------------ | :--------------------------------------------------------------- | :------------------------------------------------------------------------- | :----- | :---------- | :------------- | :----------------------- |
| geometa-create-iso-19115        | Metadata production                                              | Produce an ISO/OGC 19115/19139 metadata object                             | entity | metadata    | FALSE          | geometa                  |
| geometa-create-iso-19110        | Metadata production                                              | Produce an ISO 19110/19139 metadata object                                 | entity | metadata    | FALSE          | geometa                  |
| ows4R-publish-iso-19139         | Metadata publication                                             | Publish/Update an ISO/OGC 19139 metadata object using OGC CSW Protocol     | NA     | NA          | FALSE          | ows4R                    |
| geonapi-publish-iso-19139       | Metadata publication                                             | Publish/Update an ISO/OGC 19139 metadata object with GeoNetwork API        | NA     | NA          | FALSE          | geonapi                  |
| geosapi-publish-ogc-services    | Data upload,Data publication,Metadata publication                | Publish vector data to GeoServer OGC web-services (WMS/WFS)                | NA     | NA          | FALSE          | geosapi                  |
| zen4R-deposit-record            | Data upload,Data publication,Metadata publication,DOI assignment | Deposits/Publish data and/or metadata in the Zenodo infrastructure         | job    | zenodo      | TRUE           | zen4R                    |
| atom4R-dataverse-deposit-record | Data upload,Data publication,Metadata publication,DOI assignment | Deposits/Publish data and/or metetadata on a Dataverse using the Sword API | job    | dataverse   | TRUE           | atom4R                   |
| dataone-upload-datapackage      | Data upload,Data publication,Metadata publication,DOI assignment | Uploads a data package to a DataOne metacat node                           | job    | dataone     | TRUE           | mime,datapack,dataone    |
| sf-write-generic                | Data writing,Data upload                                         | Import features data into several formats                                  | entity | data        | FALSE          | sf,DBI,RSQLite,RPostgres |
| sf-write-dbi                    | Data writing,Data upload                                         | Import features data into Postgres/Postgis                                 | NA     | NA          | FALSE          | sf,DBI,RSQLite,RPostgres |
| sf-write-shp                    | Data writing                                                     | Import features data and zip files                                         | entity | data        | FALSE          | sf                       |
| eml-create-eml                  | Metadata production                                              | Produce an EML metadata object                                             | entity | metadata    | FALSE          | EML,emld                 |

List of actions supported by geoflow

  - **How to configure an action**

To configure an `action`, various elements should be provided: \* an
**id**: it should be the identifier of the `action` in question \*
**run**: set to **true** means the action has to be run. The action can
then be deactivated setting it to **false** \* **options**: a set of
options that are specific to each `action`. For a given action, these
options can be retrieved in R with the function
`list_action_options("<id of the action>")`. For each option, the name,
description and default value are given.

Let’s look at “geometa-create-iso-19115” `action` options available:

``` r
geoflow::list_action_options("geometa-create-iso-19115")
```

    ##                name
    ## 1               doi
    ## 2     doi_thumbnail
    ## 3           inspire
    ## 4              logo
    ## 5       addfeatures
    ## 6         featureId
    ## 7 subject_geography
    ##                                                                                  definition
    ## 1                  Add entity DOI - if defined - as metadata identifier and online resource
    ## 2            if option 'doi' is true and this option enabled, a DOI thumbnail will be added
    ## 3                             Validates ISO 19139 metadata with INSPIRE reference validator
    ## 4                     Add configure profile logo(s) - if defined - as metadata thumbnail(s)
    ## 5                   Add entity data features - if defined - as metadata bounding polygon(s)
    ## 6 ID of entity data features used to identify bounding polygon(s) with option 'addfeatures'
    ## 7                                 Identifier of the subject handling a Geographic coverage.
    ##     default
    ## 1     FALSE
    ## 2     FALSE
    ## 3     FALSE
    ## 4     FALSE
    ## 5     FALSE
    ## 6        NA
    ## 7 geography

###### JSON

  - JSON snippet for an action (eg “geometa-create-iso-19115”) with an
    option enabled:

<!-- end list -->

``` json
  {
    "id": "geometa-create-iso-19115",
      "options": {
          "logo": true
      },
    "run": true
  }
```

  - JSON snippet for the overall list of *actions*

Since it is a list of *actions*, the base JSON definition will be an
*array* (using square brackets `[ ]`):

``` json
"actions": [
     <here will be listed the actions to perform>
]
```

<a name="geoflow_config_register"/>

##### 4.3.5 Configuration components – `registers`

###### Description

The `registers` part of the configuration consists in listing the
registers needed for the workflow. Registers are here to fetch reference
data (eg. list of species names, list of countries, etc) that the user
want to exploit in actions, to enrich subjects or to enrich ISO 19110
feature attribute values with labels.

  - **List of registers managed by geoflow**

By default [geoflow](https://github.com/eblondel/geoflow) does not yet
handle registers but it’s planned to handle common standard registers
(e.g. list of ISO countries).

  - **How to configure a register**

To configure a register, the latter should be provided with various
elements: \* an **id** (mandatory): it should be a user string id to
identify the register in question. In case the user wants to handle his
own register by mean of a R function, the `id` should be the name of the
function. \* a **definition** (mandatory): some description to
characterize the register. \* a **script** (optional): the file
path/name of the R script that contains the user named function used to
handle his custom register.

###### JSON

  - JSON snippet for the overall “registers” component

Since it is a list of registers, the base JSON definition will be an
*array* (using square brackets `[ ]`):

``` json
"registers": [
     <here will be listed the registers>
]
```

  - **How to use a user’s custom register**

As described above, each `register` should have an `id` and
`definition`. For embed geoflow registers, these are the only
information required. The function that is used to fetch the register
content is handled internally by geoflow. On the other hand, a user’s
register should come with a function that will read the register and
return the register data to R and geoflow. To declare a user function as
register handler in the configuration, a `script` file path/name should
be defined, and the name of the R function should be specified as `id`.

``` json
"registers": [
  {
    "id": "my_function",
    "definition": "My custom register definition",
    "script": "<path>/my_script_including_my_function.R"
  }
]
```

The function should a return a standard structure for geoflow registers
with is an object of class `data.frame` with the following columns: \*
**code**: handles each register item code \* **uri**: handles each
register item URI (if existing) \* **label**: handles each register item
label \* **definition**: handles each register item definition

If this structure is not fulfilled,
[geoflow](https://github.com/eblondel/geoflow) will return an error at
initialization time. To make sure that your registers are well defined,
you can then test to initialize the configuratinon with `initWorkflow`.

  - Where/How registers are used in geoflow

For the time being, the registers are exploited as EXPERIMENTAL FEATURE
to enrich feature attribute (if annotated with the register id) listed
values in the action to create an ISO 19110 feature catalogue
(structural metadata describing the structure of dataset).

<a name="geoflow_config_create"/>

#### 4.4 How to create a geoflow configuration file

<a name="geoflow_config_manual"/>

##### 4.4.1 Create manually a configuration file

The above documentation gives you JSON snippets that help you preparing
your configuration file. In addition you may have a look at the
[examples](https://github.com/eblondel/geoflow/tree/master/inst/extdata)
provided with geoflow.

<a name="geoflow_config_shiny"/>

##### 4.4.2 Use the geoflow-shiny R Shiny application

The [geoflow-shiny](https://github.com/eblondel/geoflow-shiny) R Shiny
application can be used to ease the configuration of a workflow.

<a name="package_issues"/>

### 5\. Issue reporting

-----

Issues can be reported at <https://github.com/eblondel/geoflow/issues>
