
generated <- local({
  integrate_data <- function(x, y) {
    dbc::assert_is_number_nonNA_vector(x)
    dbc::assert_is_number_nonNA_vector(x)
    dbc::assert_is("length(x) == length(y)")
    n <- length(x)
    interval_dt <- data.table::data.table(
      x_diff = x[-1] - x[-n],
      y_start = y[-n],
      y_end   = y[-1]
    )
    # area of rectangle plus area of triangle
    interval_dt[, "area" := pmin(y_start, y_end) * x_diff]
    interval_dt[, "area" := area + abs(y_end - y_start) * x_diff / 2]
    cumsum(c(0.0, interval_dt[["area"]]))
  }
  lookup_stratum_col_nms <- function() "sex"
  lookup <- data.table::CJ(sex = 1:2, age = seq(0.0, 150.0, by = 0.01))
  lookup_get <- function() lookup
  lookup_get_values <- function(sex, age, value_col_nm) {
    lookup <- lookup_get()
    stopifnot(value_col_nm %in% names(lookup))
    join_dt <- data.table::data.table(sex = sex, age = age)
    lookup[
      i = join_dt,
      on = names(join_dt),
      j = .SD[[1]],
      .SDcols = value_col_nm,
      roll = "nearest"
    ]
  }
  lookup_get_ages <- function(sex, values, value_col_nm) {
    lookup <- lookup_get()
    stopifnot(value_col_nm %in% names(lookup))
    join_dt <- data.table::data.table(sex = sex, v = values)
    data.table::setnames(join_dt, "v", value_col_nm)
    lookup[
      i = join_dt,
      on = names(join_dt),
      j = .SD[[1]],
      .SDcols = "age",
      roll = "nearest"
    ]
  }

  # other -------------------------------------------------------------------
  param_other <- function(param_name) {
    c(a = -10.95, b = 0.10)[param_name]
  }
  lookup[, "h_other" := exp(param_other("a") + param_other("b") * age)]
  h_other <- function(age) {
    lookup_get_values(sex = 1L, age = age, value_col_nm = "h_other")
  }
  lookup[, "H_other" := integrate_data(x = age, y = h_other), by = "sex"]
  H_other <- function(age) {
    lookup_get_values(sex = 1L, age = age, value_col_nm = "H_other")
  }
  lookup[, "S_other" := exp(-H_other)]
  S_other <- function(age) {
    lookup_get_values(sex = 1L, age = age, value_col_nm = "S_other")
  }
  f_other <- function(sex, age) {
    lookup <- lookup_get()
    if (!"f_other" %in% names(lookup)) {
      lookup[, "f_other" := h_other * S_overall]
    }
    lookup_get_values(sex = 1L, age = age, value_col_nm = "f_other")
  }
  F_other <- function(sex, age) {
    lookup <- lookup_get()
    if (!"F_other" %in% names(lookup)) {
      lookup[, "F_other" := integrate_data(x = age, y = f_other),
             by = eval(lookup_stratum_col_nms())]
    }
    lookup_get_values(sex = 1L, age = age, value_col_nm = "F_other")
  }
  inv_F_other <- function(F_other) {
    lookup_get_ages(sex = 1L, values = F_other, value_col_nm = "F_other")
  }
  r_other <- function(sex, n) {
    u <- runif(n = n, min = 0.0, max = 1.0)
    lookup <- lookup_get()
    u <- u * lookup[lookup[["sex"]] == sex, max(.SD[[1]]),
                    .SDcols = "F_other"]
    inv_F_other(F_other = u)
  }

  # cancer ------------------------------------------------------------------
  param_cancer <- function(sex, param_name) {
    dt <- rbind(
      data.table::data.table(sex = 1L, param_name = c("mean", "sd"),
                             param = c(4.20, 0.3)),
      data.table::data.table(sex = 2L, param_name = c("mean", "sd"),
                             param = c(4.25, 0.3))
    )
    join_dt <- data.table::data.table(sex = sex, param_name = param_name)
    dt[
      i = join_dt,
      on = names(join_dt),
      j = .SD[[1]],
      .SDcols = "param"
    ]
  }
  lookup[, "h_cancer" := dlnorm(x = age,
                                meanlog = param_cancer(sex, "mean"),
                                sdlog = param_cancer(sex, "sd"))]
  h_cancer <- function(sex, age) {
    lookup_get_values(sex, age, "h_cancer")
  }
  lookup[, "H_cancer" := plnorm(q = age,
                                meanlog = param_cancer(sex, "mean"),
                                sdlog = param_cancer(sex, "sd"))]
  H_cancer <- function(sex, age) {
    lookup_get_values(sex, age, "H_cancer")
  }
  lookup[, "S_cancer" := exp(-H_cancer)]
  S_cancer <- function(sex, age) {
    lookup_get_values(sex, age, "S_cancer")
  }
  f_cancer <- function(sex, age) {
    lookup <- lookup_get()
    if (!"f_cancer" %in% names(lookup)) {
      lookup[, "f_cancer" := h_cancer * S_overall]
    }
    lookup_get_values(sex, age, "f_cancer")
  }
  F_cancer <- function(sex, age) {
    lookup <- lookup_get()
    if (!"F_cancer" %in% names(lookup)) {
      lookup[, "F_cancer" := integrate_data(x = age, y = f_cancer),
             by = eval(lookup_stratum_col_nms())]
    }
    lookup_get_values(sex, age, "F_cancer")
  }
  inv_F_cancer <- function(sex, F_cancer) {
    lookup_get_ages(sex, F_cancer, "F_cancer")
  }
  r_cancer <- function(n, sex) {
    stopifnot(length(sex) == 1, sex %in% 1:2)
    u <- runif(n = n, min = 0.0, max = 1.0)
    u <- u * F_cancer(rep(sex, n), 200)
    inv_F_cancer(sex = sex, F_cancer = u)
  }

  # overall -----------------------------------------------------------------
  lookup[, "h_overall" := h_other + h_cancer]
  h_overall <- function(sex, age) {
    lookup_get_values(sex, age, "h_overall")
  }
  lookup[, "H_overall" := H_other + H_cancer]
  H_overall <- function(sex, age) {
    lookup_get_values(sex, age, "H_overall")
  }
  lookup[, "S_overall" := exp(-H_overall)]
  S_overall <- function(sex, age) {
    lookup_get_values(sex, age, "S_overall")
  }
  lookup[, "f_overall" := h_overall * S_overall]
  f_overall <- function(sex, age) {
    h_overall(sex, age) * S_overall(sex, age)
  }
  lookup[, "F_overall" := integrate_data(x = age, y = f_overall),
         by = eval(lookup_stratum_col_nms())]
  F_overall <- function(sex, age) {
    lookup_get_values(sex, age, "F_overall")
  }
  inv_F_overall <- function(sex, F_overall) {
    lookup_get_ages(sex = sex, F_overall, "F_overall")
  }
  r_overall <- function(n, sex) {
    stopifnot(length(sex) == 1, sex %in% 1:2)
    u <- runif(n = n, min = 0.0, max = 1.0)
    u <- u * F_overall(sex = rep(sex, n), age = rep(200, n))
    inv_F_overall(sex = sex, F_overall = u)
  }

  # triggers ----------------------------------------------------------------
  # trigger these to be written into lookup
  invisible(f_cancer(1L, 50.0))
  invisible(F_cancer(1L, 50.0))
  invisible(f_other(1L, 50.0))
  invisible(F_other(1L, 50.0))

  # sample ------------------------------------------------------------------
  RNGversion("4.0.0")
  set.seed(1337)
  dt <- data.table::data.table(
    sex = rep(1:2, each = 1e3L),
    dg_y = sample(2001:2010, size = 2000, replace = TRUE)
  )
  dt[dt[["sex"]] == 1L, "de_age" := r_overall(n = .N, sex = 1L)]
  dt[dt[["sex"]] == 2L, "de_age" := r_overall(n = .N, sex = 2L)]
  dt[, "p_cancer" := F_cancer(sex = sex, age = de_age)]
  dt[, "p_other"  := F_other( sex = sex, age = de_age)]
  dt[
    j = c("p_cancer", "p_other") := data.table::data.table(.SD[[1]], .SD[[2]]) /
      (.SD[[1]] + .SD[[2]]),
    .SDcols = c("p_cancer", "p_other")
  ]
  dt[
    j = "de_cause" := {
      vapply(1:.N, function(i) {
        p <- c(cancer = .SD[[1]][i], other = .SD[[2]][i])
        sample(names(p), prob = p, size = 1L)
      }, character(1L))
    },
    .SDcols = c("p_cancer", "p_other")
  ]
  dt[, c("p_cancer", "p_other") := NULL]

  environment()
})

usethis::use_data(generated, internal = FALSE, overwrite = TRUE)


