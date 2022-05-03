## **How to create a geoflow dictionary**

### From a spreadsheet (for basic users)
A **dictionary** allows to describe the structure of datasets. This is required for advanced [geoflow](https://github.com/eblondel/geoflow) use cases where users want to produce finer dataset description (dataset structural metadata). Presently, dataset structural metadata can be produced as ISO 19110 / Feature catalogue, or directly as dataset structure within EML metadata.

#### General recommendation

A same dictionary file can be use to refer multiple data structure definitions (DSD). Each dictionnary set is identify by a unique `FeatureType`. This `FeatureType` is the name to refer in the `featureType` key of the entities metadata table / **Data** column to link this structure with the dataset.

Each row of the file refer to a unique column name to relate to a dataset. A same column name (`MemberCode`) can't be use twice for a same `FeatureType`. 

#### Columns definitions

##### FeatureType
| **Column name**              | <u>FeatureType</u> |
| ---------------------------- | ----------------------- |
| **Definition**               | Unique unambigous identifier to identify a set of dictionary item to use in *Data* column of the entities metadata.|
| **Need**                     |Mandatory                |
| **Type of content** 		     |A single expression composed of text,symbol and numeric   |
| **Example**                  |`my_dictionary1`, `dsd1`       |

##### MemberCode

| **Column name**              | <u>MemberCode</u> |
| ---------------------------- | ----------------------- |
| **Definition**               | Exact column name to match within the target data set|
| **Need**                     |Mandatory                |
| **Type of content** 		   |A single expression composed of text,symbol and numeric without space   |
| **Example**                 |`flag`,`species`        |

##### MemberName
| **Column name**              | <u>MemberName</u> |
| ---------------------------- | ----------------------- |
| **Definition**               |Add a human readible name to Member Code (label)  |
| **Need**                     |Optional                |
| **Type of content** 		   | Accepted space, text, symbol and numeric |
| **Example**                 |`country flag`,`species name`        |

##### MemberType
| **Column name**              | <u>MemberType</u> |
| ---------------------------- | ----------------------- |
| **Definition**               |Indicates whether the column is an attribute or a variable  |
| **Need**                     |Optional                |
| **Type of content** 		   | `attribute`or`variable` |
| **Example**                 |`variable`        |

##### MinOccurs
| **Column name**              | <u>MinOccurs</u> |
| ---------------------------- | ----------------------- |
| **Definition**               |Manage the minimal number of items of the MemberCode.|
| **Need**                     |Mandatory                |
| **Type of content** 		   | numeric  |
| **Example**                 |`0, `1`         |

For geoflow usecase related to [OpenFairViewer](https://github.com/eblondel/OpenFairViewer), use MinOccurs `0` to indicate that selection of item is optional, reversely with `1` selection of item becomes mandatory.

##### MaxOccurs
| **Column name**              | <u>MaxOccurs</u> |
| ---------------------------- | ----------------------- |
| **Definition**               |Manage the maximal number of items of the MemberCode. |
| **Need**                     |Mandatory                |
| **Type of content** 		   | numeric,' or `Inf`  |
| **Example**                 |`1` ,`Inf`  |

For geoflow usecase is related to [OpenFairViewer](https://github.com/eblondel/OpenFairViewer), use MaxOccurs `1` to restrain to single item selection, or use `Inf` for no limitation of selection (multiple selection).

##### Definition
| **Column name**              | <u>Definition</u> |
| ---------------------------- | ----------------------- |
| **Definition**               |add a explanation text about the column or a definition of dimension|
| **Need**                     |Optional                |
| **Type of content** 		   | text  |
| **Example**                 |`Definition of this variable`        |

##### DefinitionSource
| **Column name**              | <u>DefinitionSource</u> |
| ---------------------------- | ----------------------- |
| **Definition**               |link a uri relate to definition|
| **Need**                     |Optional                |
| **Type of content** 		   |alias@uri  |
| **Example**                 |`My definition@https://link.com`  |

##### MeasurementUnit
| **Column name**              | <u>MeasurementUnit</u> |
| ---------------------------- | ----------------------- |
| **Definition**               |Add an information about the unit of measure in case of the member is a variable |
| **Need**                     |Optional                |
| **Type of content** 		   |unit symbol and abbreviation. The best practice is to use the official unit symbol from [udunits](https://www.unidata.ucar.edu/software/udunits). For a complete list of units see `units::valid_udunits()`|
| **Example**                 |`kg`, `t`, `m` |

##### RegisterId
| **Column name**              | <u>RegisterId</u> |
| ---------------------------- | ----------------------- |
| **Definition**               |Identifier of a register(reference dataset, code list). Used in combination with `RegisterScript`. `RegisterId` should match the name of the R function within the `RegisterScript`. See below `RegisterScript` for more details|
| **Need**                     |Optional                |
| **Type of content** 		   | *please see the register documentation*|
| **Example**                 |`register_month` |

##### RegisterScript
| **Column name**              | <u>RegisterScript</u> |
| ---------------------------- | ----------------------- |
| **Definition**               |a local or remote R script containing the register id objects. Each object should be a function returning a dataframe with 4 columns:<br/>- **code** : exact name of item as used in the dataset (mandatory)<br/>- **uri**: a link about item information (optional)<br/>- **label**:label of corresponding item (mandatory) <br/>- **definition**: a definition of each item (optional)<br>see below example |
| **Need**                     |Mandatory if `RegisterId` declared                |
| **Type of content** 		   |R script path |
| **Example**                 |`C://Mydirectory/Myscript.R` |
```{r} 
# Myscript.R
`register_month <- function(config){  
  out <- data.frame(
    code = as.integer(1:12),
    uri = NA,
    label = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
    definition = NA
  )
  return(out)		
}
}
```
