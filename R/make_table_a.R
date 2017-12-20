#' Makes a CASEMIS Table A with randomized made-up names, IDs, schools, and districts
#' @param rows A number that is equal to the desired number of students
#' @return A tibble that is a practice CASEMIS Table A
#' @export

make_table_a <- function(rows) {

  make_id <- function(n, length) {
    # Makes a vector of random ID numbers
    # Args:
    #   n: The length of the vector
    #   length: The length of the ID number
    # Returns: A vector of random ID numbers
    x <- vector("character", n)
    purrr::map_chr(x, ~paste(sample(c(1:9), length, replace = T), collapse = ""))
  }

  # Vector of race codes
  race <- c(100, 201:208, 299, 301:304, 399, 400, 600, 700)
  race_na <- c(100, 201:208, 299, 301:304, 399, 400, 600, 700, NA)

  # Make tibble
  tibble::tibble(
    REPT_DATE = rep(lubridate::ymd(20171201), rows),
    SELPA_CODE = rep(make_id(1, 4), rows),
    SELPA_FROM = sample(make_id(5, 4), rows, replace = T),
    DIST_SERV = sample(make_id(3, 7), rows, replace = T),
    DIST_RESI = sample(make_id(5, 7), rows, replace = T),
    SCH_CODE = sample(make_id(10, 7), rows, replace = T),
    # TODO: Use actual school type codes
    SCH_TYPE = sample(sample(c(10:80), 15), rows, replace = T),
    LAST_NAME = randomNames::randomNames(
      rows, which.names = "last", ethnicity = sample(1:5, 1)
    ),
    FIRST_NAME = randomNames::randomNames(
      rows, which.names = "first", ethnicity = sample(1:5, 1)
    ),
    STUDENT_ID = make_id(rows, 16),
    SSID = make_id(rows, 10),
    FILLER = "",
    BIRTHDATE = sample(
      seq(lubridate::ymd(19950101), lubridate::ymd(20140101), by = "day"), rows, replace = T
    ),
    GENDER = sample(c("F", "M"), rows, replace = T),
    ETHNICITY = sample(c(500, 501, 900), rows, replace = T),
    RACE1 = sample(race, rows, replace = T),
    RACE2 = NA,
    RACE3 = NA,
    EL = sample(c("Y", "N"), rows, replace = T),
    NATIVE_LANG = sample(c(0:99), rows, replace = T),
    EARLY_INT = sample(c("Y", "N"), rows, replace = T),
    # TODO: Populate dates
    IN_RFRDATE = "",
    IN_RFRBY = sample(c("10", "20", "30", "40", "90"), rows, replace = T),
    # TODO: Populate dates
    IN_PRNTCST = "",
    # TODO: Populate dates
    IN_INTEVAL = "",
    # TODO: Populate dates
    RFRDATE = "",
    REFR_BY = sample(c("10", "20", "30", "40", "90"), rows, replace = T),
    # TODO: Populate dates
    PRNT_CSNT = "",
    # TODO: Populate dates
    INITEVAL = "",
    EVLDLAY = sample(c("10", "20", "30", "40", "90"), rows, replace = T),
    TBDLAY = sample(c("10", "20", "30", "40", "90"), rows, replace = T),
    PLAN_TYPE = sample(c(
      "10", "15", "20", "30", "70", "80", "90"
    ), rows, replace = T),
    MIGRANT = sample(c("Y", "N"), rows, replace = T),
    RESID_STAT = sample(c("10", "20", "30", "40", "50", "60", "71",
                          "72", "75", "90"), rows, replace = T),
    # TODO: Populate dates
    ENTRY_DATE = "",
    # TODO: Populate dates
    LAST_IEP = "",
    LAST_EVAL = "",
    DISABILIT1 = sample(c(seq(200, 280, by = 10), 281, seq(290, 330, by = 10)),
                        rows,
                        replace = T),
    DISABILIT2 = "",
    SOLE_LOW = sample(c("Y", "N"), rows, replace = T),
    INFANT_SET = sample(c(21:23), rows, replace = T),
    FEDSET_INF = sample(c(100, 200, 900), rows, replace = T),
    FEDSET_PRS = sample(c(
      400, 405, 410, 415, 440, 450, 460, 470, 475
    ), rows, replace = T),
    FEDSET_SCH = sample(c(400, seq(450, 500, by = 10)), rows, replace = T),
    IN_REGCLS = sample(c(1:100), rows, replace = T),
    GRADE = sample(c(
      "01", "02", "03", "04", "05", "06", "07", "08", "09",
      "10", "11", "12", "13", "15", "16", "17", "18"
    ),
    rows, replace = T),
    TRAN_REG1 = sample(c(10, 20), rows, replace = T),
    TRAN_REG2 = sample(c(10, 20), rows, replace = T),
    TRAN_REG3 = sample(c(10, 20), rows, replace = T),
    TRAN_REG4 = sample(c(10, 20), rows, replace = T),
    TRAN_REG5 = sample(c(10, 20), rows, replace = T),
    TRAN_REG6 = sample(c(10, 20), rows, replace = T),
    TRAN_REG7 = sample(c(10, 20), rows, replace = T),
    TRAN_REG8 = sample(c(10, 20, 30), rows, replace = T),
    SPEC_TRANS = sample(c("Y", "N"), rows, replace = T),
    PARINPUT = sample(seq(10, 30, by = 10), rows, replace = T),
    FILLER1 = "",
    FILLER2 = "",
    FILLER3 = "",
    FILLER4 = "",
    FILLER5 = "",
    FILLER6 = "",
    GRAD_PLAN = sample(c(10, 20), rows, replace = T),
    EXIT_DATE = "",
    EXIT_RESON = sample(c(70:74, 76:78, 81, 83), rows, replace = T),
    IEPDELAY = sample(c(seq(10, 40, by = 10), 90), rows, replace = T),
    TRIDELAY = sample(c(seq(10, 40, by = 10), 90), rows, replace = T)
  )
  #TODO: Introduce RACE2 and RACE3 fields
}
