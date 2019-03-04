# geoflow

[![Build Status](https://travis-ci.org/eblondel/geoflow.svg?branch=master)](https://travis-ci.org/eblondel/geoflow)
[![codecov.io](http://codecov.io/github/eblondel/geoflow/coverage.svg?branch=master)](http://codecov.io/github/eblondel/geoflow?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/geoflow)](https://cran.r-project.org/package=geoflow)
[![Github_Status_Badge](https://img.shields.io/badge/Github-0.1--0-blue.svg)](https://github.com/eblondel/geoflow)

**R engine to orchestrate and run geospatial (meta)data workflows**

``geoflow`` provides a simple R engine to orchestrate and run geospatial (meta)data workflows.

## Functioning

The principle of ``geoflow`` is to offer a simple framework in R to execute and orchestrate geospatial (meta)data management and publication tasks based on a tasks configuration file.

We distinguish two workflow  ``modes``:
* ``raw`` if we want to execute raw scripts (e.g. geoprocessings) as sequence
* ``entity`` if we want to to execute functions, each function based on a metadata entity and having different purpose

### Configuration File

The configuration file used for defining a workflow is structured in a JSON file containing several building blocks (Please note that these building blocks are currently reviewed and ``geoflow`` is currently used as initiative to further standardize such building blocks):
* ``id``: a simple workflow identifier.
* ``mode``: with possible values ``raw`` (raw R processings, default value if the ``mode`` is omitted), or ``entity`` for entity-based actions. The latter requires to specify a ``metadata`` configuration.
* ``dependencies``: a list of package and/or scripts dependencies. These can be CRAN packages, Github packages or individual (local or remote) scripts. The parameters ``cran_force_install`` and ``github_force_install`` set to TRUE will force the installation of packages prior to the workflow execution.
* ``sdi``: lists the Spatial Data Infrastructure components. Here as well this list will be growing with the support of additional SDI components. For the time being, The following components are supported: GeoServer (REST API), with package [``geosapi``](https://github.com/eblondel/geosapi)), GeoNetwork (Legacy API) with package [``geonapi``](https://github.com/eblondel/geonapi), and [``ows4R``](https://github.com/eblondel/ows4R) for R interfaces to OGC Web-Services such as WFS and CSW.
* ``metadata``: This configuration section allows to define two kind of metadata entries: the ``entities`` (each entity represents a set of metadata e.g. describing a dataset), and ``contacts`` corresponding to the directory people/organizations involved in the entities.
* ``actions``: one ore more actions to execute in the workflow. In the case of an entity-based workflow, the user can add either embedded actions made available in geoflow (The list can be obtained with ``list_geoflow_actions()``) or custom actions the user wants to configure.

An embedded action is defined specifying the ``id`` of the action. An optional parameter can be used to specify ``options`` of the actions (e.g. create ISO 19115 and specify INSPIRE validation option). The parameter ``run`` indicates if the action has to be run in the workflow.

Example of action definition for creating ISO 19115 metadata with [geometa](https://github.com/eblondel/geometa) with option to test INSPIRE metadata validation.

```{json}
{
  "id": "geometa-create-iso-19115",
  "options": {
    "inspire": true
  }
  "run": true
}

```

A custom action can be defined specifying the R script. In case of an entity-based workflow, the R script should handle a function which name is to specify as the action ``id``.

```{json}
{
  "script": "my_script.R",
  "id": "my_function",
  "run": true
}

```

**Example of workflow with ``raw`` mode**

The below examples will execute 3 processing scripts as sequence.

```{json}
{
  
  "id": "my-workflow",
  "mode": "raw",
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
  
  "sdi": [
      "id": "geoserver",
      "url": "http://somehost/geoserver",
      "user": "user",
      "pwd": "pwd",
      "workspace": "ws",
      "namespace": "http://somehost/ws",
      "datastore": "ds"
    },
    
    {
      "id": "geonetwork",
      "url": "http://somehost/geonetwork",
      "user": "user",
      "pwd": "pwd",
      "version": "3.0"
    },
    
    {
      "id": "wfs",
      "url": "http://somehost/geoserver/wfs",
      "version": "1.0.0"
      
    },
    {
      "id": "csw",
      "url": "http://somehost/csw",
      "user": "user",
      "pwd": "pwd",
      "version": "2.0.2"
    }
  ],
  
  "actions": [
    {
      "id": "action-1",
      "script": "myscript_action_1.R",
      "run": true
    },
    {
      "id": "action-2",
      "script": "myscript_action_2.R",
      "run": true
    },
    {
      "id": "action-3",
      "script": "myscript_action_3.R",
      "run": true
    }
  ]
}

```

**Example of workflow with ``raw`` mode**

The below example configuration will handle an entity-based workflow with the creation of ISO 19115 metadata with geometa based on the metadata entities/contacts defined in the "metadata" configuration element.

```{json}
{
  
  "id": "my-workflow",
  "mode": "entity",
  "dependencies": {
    "packages": {
      "cran": [
        "gsheet"
      ],
      "cran_force_install" : false,
      "github": [],
      "github_force_install": false
    },
    "scripts": []
  },
  "metadata": {
    "entities": {
      "handler": "gsheet",
      "source": "https://docs.google.com/spreadsheets/d/1iG7i3CE0W9zVM3QxWfCjoYbqj1dQvKsMnER6kqwDiqM/edit?usp=sharing"
    },
    "contacts" : {
      "handler": "gsheet",
      "source": "https://docs.google.com/spreadsheets/d/144NmGsikdIRE578IN0McK9uZEUHZdBuZcGy1pJS6nAg/edit?usp=sharing"
    }
  },
  "software": [],
  "actions": [
    {
      "id": "geometa-create-iso-19115",
      "options" : {
        "inspire" : true
      },
      "run": true
    }
  ]
}

```

### Workflow Execution

#### R Code

The execution of a workflow job is done simply by performing the following R code, specifying as parameter the name of the configuration file:

```{r}
library(geoflow)
executeWorkflow("my_config.json")
```

#### Business logic understanding

When executing the workflow the following actions will be triggered:
* at first execution, a directory named ``jobs`` will be created with the current working directory
* at each execution: a directory giving the date/time of the execution will be created within the ``jobs`` directory. The configuration file used for the execution will be copied in this directory. Three subdirectories will be created: 

1. ``logs`` where a log file will be available with all logs specified in R (including ``print``, ``message`` and ``cat`` statements)
2. ``data`` as subdirectory where data resources processed through scripts will be writen as outputs of the workflow tasks.
3. ``metadata`` as subdirectory where metadata resources processed through scripts will be writen as outputs of the workflow tasks.

In case of failure of the workflow, the working directory will be set back to the current working directory used when executing the workflow.

#### Main business logic code

By default, there is no need of handling a ``main`` R script to handle the whole flow (sequence of tasks). The workflow configuration loader takes care of configuring the sequence of tasks for those ``actions`` scripts that are enabled (set to ``true`` in the configuration file).

Further flexibility may be required to enable the execution of a ``main`` script instead to fine-tune the sequence of actions (TODO).


# Developments

For geoflow sponsoring/funding new developments, enhancements, support requests, please contact me by [e-mail](mailto:emmanuel.blondel1@gmail.com)
