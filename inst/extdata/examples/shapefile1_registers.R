
register_species <- function(config){
	data.frame(
		code = c("sp1", "sp2", "sp3", "sp4"),
		uri = rep(NA, 4),
		label = c("Species 1", "Species 2", "Species 3", "Species 4"),
		definition = rep(NA, 4),
		stringsAsFactors = FALSE
	)
}

register_area <- function(config){
	data.frame(
		code = c("area1", "area2"),
		uri = rep(NA,2),
		label = c("Area 1", "Area 2"),
		definition = rep(NA,2),
		stringsAsFactors = FALSE
	)
}

