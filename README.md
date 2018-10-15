# geoflow

[![Build Status](https://travis-ci.org/eblondel/geoflow.svg?branch=master)](https://travis-ci.org/eblondel/geoflow)
[![codecov.io](http://codecov.io/github/eblondel/geoflow/coverage.svg?branch=master)](http://codecov.io/github/eblondel/geoflow?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/geoflow)](https://cran.r-project.org/package=geoflow)
[![Github_Status_Badge](https://img.shields.io/badge/Github-0.1--0-blue.svg)](https://github.com/eblondel/geoflow)

**R engine to orchestrate and run geospatial (meta)data workflows**

``geoflow`` provides a simple R engine to orchestrate and run geospatial (meta)data workflows.

## Functioning

The principle of ``geoflow`` is to offer a simple framework in R to execute and orchestrate geospatial (meta)data management and publication tasks based on a
tasks configuration file.

### Configuration File

The configuration file used for defining a workflow is structured in a JSON file containing several building blocks (Please note that these building blocks are currently reviewed and ``geoflow`` is currently used as initiative to further standardize such building blocks):
* ``id``: a simple workflow identifier.
* ``dependencies``: a list of package and/or scripts dependencies. These can be CRAN packages, Github packages or individual (local or remote) scripts. The parameters ``cran_force_install`` and ``github_force_install`` set to TRUE will force the installation of packages prior to the workflow execution.
* ``sdi``: lists the Spatial Data Infrastructure components. Here as well this list will be growing with the support of additional SDI components. For the time being, The following components are supported: GeoServer (REST API), with package [``geosapi``](https://github.com/eblondel/geosapi)), GeoNetwork (Legacy API) with package [``geonapi``](https://github.com/eblondel/geonapi), and [``ows4R``](https://github.com/eblondel/ows4R) for R interfaces to OGC Web-Services such as WFS and CSW.
* ``actions``: one ore more actions, handled by script names, and true/false whether they should be run by the workflow. At this stage, no particular action will be embedded, but in the future some pre-defined actions (e.g. write/publish ISO/OGC metadata, create/publish Geoserver layer) might be embedded and available for users.


```{json}
{
  
  "id": "my-workflow",
  
  "dependencies": {
    "packages": {
      "cran": [
        "sp","raster", "maptools", "rgdal", "rgeos", "geosphere",
        "cleangeo", "geometa", "geosapi", "geonapi", "ows4R"
      ],
      "cran_force_install" : false,
      "github": ["openfigis/RFigisGeo"],
      "github_force_install": false
    },
    "scripts": []
  },
  
  "sdi": {
    "loggerLevel": "DEBUG",
    
    "geoserver": {
      "url": "http://somehost/geoserver",
      "user": "user",
      "pwd": "pwd",
      "workspace": "ws",
      "namespace": "http://somehost/ws",
      "datastore": "ds"
    },
    
    "geonetwork": {
      "url": "http://somehost/geonetwork",
      "user": "user",
      "pwd": "pwd",
      "version": "3.0"
    },
    
    "wfs": {
      "url": "http://somehost/geoserver/wfs",
      "version": "1.0.0"
      
    },
    
    "csw": {
      "url": "http://somehost/csw",
      "user": "user",
      "pwd": "pwd",
      "version": "2.0.2"
    }
  },
  
  "actions": {
    "myscript_action_1.R": true,
    "myscript_action_2.R": true,
    "myscript_action_3.R": false
  }
}

```

### Workflow Execution

The execution of a workflow job is done simply by performing the following R code, specifying as parameter the name of the configuration file:

```{r}
library(geoflow)
executeWorkflow("my_config.json")
```

When executing the workflow the following actions will be triggered:
* at first execution, a directory named ``jobs`` will be created with the current working directory
* at each execution: a directory giving the date/time of the execution will be created within the ``jobs`` directory. The configuration file used for the execution will be copied in this directory. Three subdirectories will be created: 

1. ``logs`` where a log file will be available with all logs specified in R (including ``print``, ``message`` and ``cat`` statements)
2. ``data`` as subdirectory where data resources processed through scripts will be writen as outputs of the workflow tasks.
3. ``metadata`` as subdirectory where metadata resources processed through scripts will be writen as outputs of the workflow tasks.

In case of failure of the workflow, the working directory will be set back to the current working directory used when executing the workflow.

# Developments

For geoflow sponsoring/funding new developments, enhancements, support requests, please contact me by [e-mail](mailto:emmanuel.blondel1@gmail.com)
