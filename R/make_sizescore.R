#' Plots subgroup size against distance from three score
#' @param ela Path for CA School Dashboard ELA dataset
#' @param math Path for CA School Dashboard math dataset
#' @return Plot of subgroup size vs distance from three score
#' @export

make_sizescore <- function(ela, math) {

  files <- c(ela, math)

  tests <- files %>% map(read_tsv)

  add_test <- function(dataset, test_name) {
    # Adds a test label column
    # Args:
    #  dataset: CA School Dashboard testing dataset
    #  test_name: Name of the test
    # Returns:
    #  Tibble with new test name column
    dataset %>% mutate(test = test_name)
  }

  purrr::map2(tests, as.list(c("ELA", "Math")), add_test) %>%
    dplyr::bind_rows() %>%
    # Turn subgroup codes into words
    dplyr::mutate(studentgroup = spedtools::convert_subgroup(studentgroup)) %>%
    dplyr::select(
      districtname, schoolname, studentgroup, currstatus, currdenom, test
      ) %>%
    # Remove district totals
    dplyr::filter(!is.na(schoolname)) %>%
    # Remove NAs in currstatus caused by currdenom < 10
    dplyr::filter(!is.na(currstatus)) %>% {
      # Build plot
      ggplot2::ggplot(
        data = ., aes(x = currdenom, y = currstatus, color = studentgroup)
        ) +
        ggplot2::geom_point(alpha = .5) +
        ggplot2::facet_wrap(~test) +
        ggplot2::labs(title = "Subgroup Size vs. Distance From Three",
             subtitle = paste(.$districtname, "Fall 2017"),
             caption = "Data: CA School Dashboards",
             x = "Subgroup Size",
             y = "Distance From Three")
    }
}
