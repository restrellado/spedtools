#' Takes a vector of CA Dashboard subgroup codes
#' and converts them to the subgroup name
#' @param x A character vector of CA Dashboard subgroup codes
#' @return A character vector of subgroup names
#' @export

convert_subgroup <- function(x) {

  # Subgroup abbreviations
  subgroup_lookup <- c(
    "ALL" = "All Students",
    "AA"  = "Black/African American",
    "AI"  = "American Indian or Alaska Native",
    "AS"  = "Asian",
    "FI"  = "Filipino",
    "HI"  = "Hispanic",
    "PI"  = "Pacific Islander",
    "WH"  = "White",
    "MR"  = "Multiple Races/Two or More",
    "EL"  = "English Learner",
    "ELO" = "English Learners Only",
    "RFP" = "RFEPs Only",
    "EO"  = "English Only",
    "SED" = "Socioeconomically Disadvantaged",
    "SWD" = "Students with Disabilities",
    "FOS" = "Foster Youth",
    "HOM" = "Homeless Youth"
  )

  # Create a new vector of disability descriptions
  as.vector(subgroup_lookup[as.character(x)])
}
