### **Metadata**

The _metadata_ configuration can be constituted of three different inputs:

* *entities*: correspond to the dataset descriptions. [geoflow](https://github.com/eblondel) models each dataset metadata as *entity*. An *entity* is compound by core metadata building blocks essentially inspired from the [Dublin Core metadata standard][https://dublincore.org/] . In order to build richer metadata, each building block can be described with finer levels of information. For more information, see details on how to describe a dataset with geoflow.
* _contacts_: correspond to a simplified directory of contacts (people / organizations) used by geoflow to manage contact/roles associated to *entities*. Having such directory of contacts will simplify the use and binding of *responsible parties* to dataset metadata.
* *dictionary*: allows to describe the structure of datasets. This is required for advanced [geoflow](https://github.com/eblondel) use cases where one wants to produce finer dataset description (dataset structural metadata).

In [geoflow](https://github.com/eblondel), Each type of metadata input can be structured through different input formats, called *handlers*. 



## **How to create a geoflow entity**

### from a spreadsheet (for basic users)

To facilitate the edition and maintenance of metadata, [geoflow](https://github.com/eblondel) opted for a simplified tabular data handler that allows user handling their *entities* (ie dataset metadata) in a table, that could be of different sources (CSV, Excel, Google Spreadsheet, or a database source), and where each table row will represent one single *entity*.

Although handling a dataset metadata description in one tabular data row may facilitate the edition and maintenance, adding rich metadata requires to  introduce some syntax to fill each of the metadata elements. [geoflow](https://github.com/eblondel) has adopted some writing syntax which essentially relies on key/value pairs, which allow to enter various properties in one single tabular data cell (eg. for a column on dataset "Date", the user will be able to specify multiple dates, depending on their nature: creation, revision, publication, etc).

Basic examples of key/value pairs:

* Identifier: ``id:my-metadata-identifier`` (key = `` id``, value = ``my-metadata-identifier`` )
* Creation date: ``creation:2010-01-01`` (key = ``creation``, value = ``2010-01-01``)

Handling such complexity of content within a same cell requires a way to distinguish each of the key/value pairs. Because [geoflow](https://github.com/eblondel) cannot know a priori what metadata content will be filled in by user, key/value pairs within a same cell have to be separated by a _line separator_ which by default is ``_\n`` (underscore symbol followed by a new line).

For example, a user wants to fill in 2 types of dates (creation and publication) associated with an *entity*, the following will be used:

``creation:2020-01-01_`` 

``publication:2020-02-01``

This syntax is later described in depth  for each of the columns required for describing a dataset, and illustrated with concrete examples.

For each column described thereafter, it is indicated whether content for the column considered is Optional (O) / Recommended (R) / Mandatory (M). 

Examples cover both:

* simple usage: where content corresponds to the main metadata text content the user wants to use for the column, in which case, the ``key`` of the metadata element can be omitted.
* advanced usage: where content is separated in metadata sub-elements.

#### Identifier

| **Column name**              | <u>**Identifier**</u>                                        |
| ---------------------------- | ------------------------------------------------------------ |
| **Definition**               | Provides the unique identifier for the dataset metadata, and eventual additional identifiers associated with the *entity* (e.g. DOI). Identifiers are expected to be used by information systems / software tools, etc hence it is recommended to use string concatenation (with no space). The user is free to define how the main identifier is compound to guarantee unicity for datasets, and to define it with semantics to make it self-explaining. |
| **Need**                     | Mandatory                                                    |
| **Default key (if omitted)** | ``id`` (unique identifier)                                   |
| **Examples**                 |                                                              |
| Simple usage                 | A simple string representing a unique *entity* identifier (``id``). ``my-metadata-identifier`` , equivalent to ``id:my-metadata-identifier``. |
| Advanced usage               | Main identifier + DOI: <br />``id:my-metadata-identifier_``<br />``doi:10.5281/zenodo.3138920`` |

#### **Title**

| **Column name**              | <u>Title</u>                 |
| ---------------------------- | ---------------------------- |
| **Definition**               | Dataset title.               |
| **Need**                     | Mandatory                    |
| **Default key (if omitted)** | *Not applicable*             |
| **Examples**                 |                              |
| Simple usage                 | ``This is my dataset title`` |
| Advanced usage               | *Not applicable*             |

#### **Description**

| **Column name**              | <u>Description</u>                                           |
| ---------------------------- | ------------------------------------------------------------ |
| **Definition**               | Dataset summary description.                                 |
| **Need**                     | Mandatory                                                    |
| **Default key (if omitted)** | ``abstract`` (basic abstract of the dataset)                 |
| **Examples**                 |                                                              |
| Simple usage                 | *This is my abstract* , equivalent to **abstract**:*This is my abstract*. |
| Advanced usages              | Abstract extended with supplemental information:  <br />`abstract:This is my abstract_` <br />`info:some more information about this dataset` |

#### **Subject**

| **Column name**              | <u>Subject</u>                                               |
| ---------------------------- | ------------------------------------------------------------ |
| **Definition**               | Subjects(s) associated to the dataset. A subject can be summarized as thesaurus to which will be associated a set of keywords that describe the dataset. Multiple thesauri and keyword sets can be added to a dataset. Each thesaurus (key) should be set with a comma-separated list of keywords |
| **Need**                     | Recommended                                                  |
| **Default key (if omitted)** | *Not applicable* : <br />Note: To add one subject, the subject name should be the key to use. |
| **Examples**                 |                                                              |
| Simple usage                 | **INSPIRE**:*Atmospheric conditions*,*Environmental monitoring facilities*,*Geology*,*Meteorological geographical features* (a "INSPIRE" "thesaurus" with 4 keywords) |
| Advanced usages              | Adding a hyperlinks to thesaurus and keywords (using @ symbol):<br /><br />**INSPIRE**@[http://www.eionet.europa.eu/gemet/inspire_themes](http://www.eionet.europa.eu/gemet/inspire_themes):*Atmospheric conditions*@[http://inspire.ec.europa.eu/theme/ac](http://inspire.ec.europa.eu/theme/ac),*Environmental monitoring facilities*@[http://inspire.ec.europa.eu/theme/ef](http://inspire.ec.europa.eu/theme/ef),*Geology*@[http://inspire.ec.europa.eu/theme/ge](http://inspire.ec.europa.eu/theme/ge),*Meteorological geographical features*@[http://inspire.ec.europa.eu/theme/mf](http://inspire.ec.europa.eu/theme/mf) |
|                              | Adding 2 thesauri:<br /><br />**General**:*keyword1,keyword2,keyword3*_<br />**INSPIRE**@[http://www.eionet.europa.eu/gemet/inspire_themes](http://www.eionet.europa.eu/gemet/inspire_themes):*Atmospheric conditions*@[http://inspire.ec.europa.eu/theme/ac](http://inspire.ec.europa.eu/theme/ac),*Environmental monitoring facilities*@[http://inspire.ec.europa.eu/theme/ef](http://inspire.ec.europa.eu/theme/ef),*Geology*@[http://inspire.ec.europa.eu/theme/ge](http://inspire.ec.europa.eu/theme/ge),*Meteorological geographical features*@[http://inspire.ec.europa.eu/theme/mf](http://inspire.ec.europa.eu/theme/mf) |



#### Creator

| **Column name**              | <u>Creator</u> |
| ---------------------------- | -------------- |
| **Definition**               |                |
| **Need**                     |                |
| **Default key (if omitted)** |                |
| **Examples**                 |                |
| Simple usage                 |                |
| Advanced usage               |                |

#### Date

| **Column name**              | <u>Date</u> |
| ---------------------------- | ----------- |
| **Definition**               |             |
| **Need**                     |             |
| **Default key (if omitted)** |             |
| **Examples**                 |             |
| Simple usage                 |             |
| Advanced usage               |             |

#### Type

| **Column name**              | <u>Type</u> |
| ---------------------------- | ----------- |
| **Definition**               |             |
| **Need**                     |             |
| **Default key (if omitted)** |             |
| **Examples**                 |             |
| Simple usage                 |             |
| Advanced usage               |             |

#### Language

| **Column name**              | <u>Language</u> |
| ---------------------------- | --------------- |
| **Definition**               |                 |
| **Need**                     |                 |
| **Default key (if omitted)** |                 |
| **Examples**                 |                 |
| Simple usage                 |                 |
| Advanced usage               |                 |

#### SpatialCoverage

| **Column name**              | <u>SpatialCoverage</u> |
| ---------------------------- | ---------------------- |
| **Definition**               |                        |
| **Need**                     |                        |
| **Default key (if omitted)** |                        |
| **Examples**                 |                        |
| Simple usage                 |                        |
| Advanced usage               |                        |

#### TemporalCoverage

| **Column name**              | <u>TemporalCoverage</u> |
| ---------------------------- | ----------------------- |
| **Definition**               |                         |
| **Need**                     |                         |
| **Default key (if omitted)** |                         |
| **Examples**                 |                         |
| Simple usage                 |                         |
| Advanced usage               |                         |

#### Relation

| **Column name**              | <u>Relation</u> |
| ---------------------------- | --------------- |
| **Definition**               |                 |
| **Need**                     |                 |
| **Default key (if omitted)** |                 |
| **Examples**                 |                 |
| Simple usage                 |                 |
| Advanced usage               |                 |

#### Rights

| **Column name**              | <u>Rights</u> |
| ---------------------------- | ------------- |
| **Definition**               |               |
| **Need**                     |               |
| **Default key (if omitted)** |               |
| **Examples**                 |               |
| Simple usage                 |               |
| Advanced usage               |               |

#### Provenance

| **Column name**              | <u>Provenance</u> |
| ---------------------------- | ----------------- |
| **Definition**               |                   |
| **Need**                     |                   |
| **Default key (if omitted)** |                   |
| **Examples**                 |                   |
| Simple usage                 |                   |
| Advanced usage               |                   |

#### Data

| **Column name**              | <u>Data</u> |
| ---------------------------- | ----------- |
| **Definition**               |             |
| **Need**                     |             |
| **Default key (if omitted)** |             |
| **Examples**                 |             |
| Simple usage                 |             |
| Advanced usage               |             |



**using R geoflow data model (for advanced R users)**

FOR LATER --> DEVELOPER ORIENTED