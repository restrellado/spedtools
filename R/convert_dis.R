#' Takes a vector of CDE disability codes and converts them to the abbreviated disability name
#' @param x A numeric or character vector of disability codes
#' @return A character vector of disability names
#' @export

convert_dis <- function(x) {
  # Names for disability vector
  dis_codes <- c(seq(200, 280, by = 10), 281, seq(290, 330, by = 10))

  # Elements of disability vector
  dis_desc <- c(
    "none", "id", "hh", "deaf", "sli", "vi", "ed", "oi",
    "ohi", "emd", "sld", "db", "md", "aut", "tbi"
    )

  # Create named vector
  dis_lookup <- purrr::set_names(dis_desc, nm = dis_codes)

  # Create a new vector of disability descriptions
  as.vector(dis_lookup[as.character(x)])
}
