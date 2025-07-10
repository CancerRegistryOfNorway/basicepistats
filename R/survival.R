lexis_set__ <- function(dt, ts_col_nms) {
  attr_nms <- c("time.scales", "time.since", "breaks")
  if (inherits(dt, "Lexis")) {
    attrs <- lapply(attr_nms, attr, x = dt)
  } else {
    attrs <- list(
      ts_col_nms,
      rep("", length(ts_col_nms)),
      structure(lapply(ts_col_nms, function(x) NULL), names = ts_col_nms)
    )
    names(attrs) <- attr_nms
  }
  data.table::setDT(dt)
  data.table::alloc.col(dt)
  data.table::setattr(dt, "class", union("Lexis", class(dt)))
  for (attr_nm in attr_nms) {
    data.table::setattr(dt, name = attr_nm, value = attrs[[attr_nm]])
  }
  return(invisible(dt[]))
}

surv_split <- function(
  dt,
  breaks
) {
  out <- fcrcore::dt_independent_frame_dependent_contents(dt)
  lexis_set__(out, ts_col_nms = names(breaks))
  out <- popEpi::splitMulti(
    data = out,
    breaks = breaks,
    drop = TRUE,
    merge = TRUE,
    verbose = FALSE
  )
  data.table::setDT(out)
  dbc::assert_prod_output_is_data_table_with_required_names(
    x = out,
    required_names = c(names(breaks), "lex.dur", "lex.Cst", "lex.Xst")
  )
  return(out[])
}

surv_merge <- function(
  dt,
  merge_by,
  merge_dt
) {
  merge_value_col_nms <- setdiff(names(merge_dt), merge_by)
  fcrcore::dt_join_assign(
    dt = dt,
    i = merge_dt,
    on = merge_by,
    dt.col.nms = merge_value_col_nms,
    i.col.nms = merge_value_col_nms
  )
  return(invisible(dt[]))
}

surv_aggregate_one_stratum__ <- function(
  sub_dt,
  aggre_breaks,
  aggre_values_expr
) {
  if (inherits(aggre_breaks, "list")) {
    aggre_breaks <- do.call(data.table::CJ, aggre_breaks)
  }
  expr_obj_nms <- all.vars(aggre_values_expr)
  ts_col_nms <- attr(sub_dt, "time.scales")
  lapply(ts_col_nms, function(ts_col_nm) {
    add_col_nms <- expr_obj_nms[
      grepl(sprintf("^%s_((lead)|(lag))[0-9]+$", ts_col_nm), expr_obj_nms)
    ]
    add_col_nms <- unique(add_col_nms)
    lapply(add_col_nms, function(add_col_nm) {
      settings <- list(type = "lead")
      if (grepl("lag", add_col_nm)) {
        settings[["type"]] <- "lag"
      }
      settings[["n"]] <- as.integer(sub("^[^0-9]+", "", add_col_nm))
      sub_dt[
        #' @importFrom data.table := .SD
        j = (add_col_nm) := .SD[[ts_col_nm]] - data.table::shift(
          x = .SD[[ts_col_nm]],
          n = settings[["n"]],
          type = settings[["type"]],
          fill = NA_integer_
        ),
        .SDcols = ts_col_nm,
        by = "lex.id"
      ]
    })
  })
  agg_expr <- quote(sub_dt[
    i = aggre_breaks,
    on = names(aggre_breaks),
    j = "placeholder",
    keyby = .EACHI
  ])
  agg_expr[["j"]] <- aggre_values_expr
  eval(agg_expr)
}

surv_split_merge_aggregate <- function(
  dt,
  breaks = list(cal = 2000:2025, age = 0:100, fot = seq(0, 5, 1 / 12)),
  merge_by = c("sex", "cal", "age"),
  merge_dt = data.table::data.table(
    sex = 0L,
    cal = 2000,
    age = 0,
    haz = 0.010
  ),
  aggre_stratum_dt = data.table::CJ(sex = 0:1, study_arm = 0:1),
  aggre_ts_col_nms = "fot",
  aggre_values = quote(list(
    pyrs = sum(duration),
    n = .N,
    d = sum(status == "dead"),
    w = sum(brenner_weight),
    c = sum(status == "lost to follow-up")
  )),
  n_max_subjects_per_split = 1e3L
) {
  aggre_values_expr <- substitute(aggre_values)
  if (identical(aggre_values_expr[[1]], quote(quote))) {
    aggre_values_expr <- eval(aggre_values_expr)
  }

  aggre_breaks_dt <- do.call(
    data.table::CJ,
    lapply(breaks[aggre_ts_col_nms], function(x) {
      x[-length(x)]
    })
  )

  out <- dt[
    j = {
      sub_dt <- surv_split(dt = .SD, breaks = breaks)
      surv_merge(sub_dt, merge_by = merge_by, merge_dt = merge_dt)
      surv_aggregate_one_stratum__(
        sub_dt = sub_dt,
        aggre_breaks = aggre_breaks_dt,
        aggre_values_expr = aggre_values_expr
      )
    },
    .SDcols = intersect(
      names(dt),
      c(
        names(breaks),
        merge_by,
        all.vars(expr = aggre_values_expr),
        c("lex.id", "lex.dur", "lex.Cst", "lex.Xst")
      )
    ),
    keyby = names(aggre_stratum_dt)
  ]
  out <- out[
    i = aggre_stratum_dt,
    on = names(aggre_stratum_dt)
  ]
  return(out[])
}

surv_split_merge_aggregate_by_interval <- function(
  dt,
  breaks = list(cal = 2000:2025, age = 0:100, fot = seq(0, 5, 1 / 12)),
  merge_by = c("sex", "cal", "age"),
  merge_dt = data.table::data.table(
    sex = 0L,
    cal = 2000,
    age = 0,
    haz = 0.010
  ),
  aggre_stratum_dt = data.table::CJ(sex = 0:1, study_arm = 0:1),
  aggre_ts_col_nms = "fot",
  aggre_values = quote(list(
    pyrs = sum(duration),
    n = .N,
    d = sum(status == "dead"),
    w = sum(brenner_weight),
    c = sum(status == "lost to follow-up")
  )),
  n_processing_strata = data.table::getDTthreads() * 2L
) {
  aggre_values_expr <- substitute(aggre_values)
  if (identical(aggre_values_expr[[1]], quote(quote))) {
    aggre_values_expr <- eval(aggre_values_expr)
  }

  breaks <- lapply(breaks, function(x) sort(unique(x)))
  interval_dt <- local({
    interval_breaks <- breaks[aggre_ts_col_nms]
    interval_dt_by_ts <- lapply(names(interval_breaks), function(ts_col_nm) {
      x <- interval_breaks[[ts_col_nm]]
      out <- data.table::data.table(
        lo = x[-length(x)],
        hi = x[-1L]
      )
      data.table::setnames(out, paste0(ts_col_nm, c("_lo", "_hi")))
      return(out[])
    })
    names(interval_dt_by_ts) <- names(interval_breaks)
    out <- do.call(data.table::CJ, lapply(interval_dt_by_ts, function(idt) {
      seq_len(nrow(idt))
    }))
    lapply(names(interval_dt_by_ts), function(ts_col_nm) {
      idt <- interval_dt_by_ts[[ts_col_nm]]
      indices <- out[[ts_col_nm]]
      data.table::set(
        out,
        j = names(idt),
        value = idt[indices, ]
      )
      data.table::set(
        out,
        j = ts_col_nm,
        value = NULL
      )
      NULL
    })
    out[]
  })

  # out <- local({
  #   out <- data.table::CJ(
  #     aggre_stratum_dt_row_no = seq_len(nrow(aggre_stratum_dt)),
  #     interval_dt_row_no = seq_len(nrow(interval_dt))
  #   )
  #   data.table::set(
  #     out,
  #     j = names(aggre_stratum_dt),
  #     value = aggre_stratum_dt[out[["aggre_stratum_dt_row_no"]]]
  #   )
  #   data.table::set(
  #     out,
  #     j = names(interval_dt),
  #     value = interval_dt[out[["interval_dt_row_no"]]]
  #   )
  #   data.table::set(
  #     out,
  #     j = c("aggre_stratum_dt_row_no", "interval_dt_row_no"),
  #     value = NULL
  #   )
  #   out[]
  # })
  processing_stratum_col_nm <- ".______PROCESSING_STRATUM_______."
  dt <- local({
    dt <- fcrcore::dt_independent_frame_dependent_contents(dt)
    data.table::set(
      dt,
      j = processing_stratum_col_nm,
      value = rep(seq_len(n_processing_strata), length.out = nrow(dt))
    )
    dt[]
  })
  out <- dt[
    j = {
      invisible(lapply(seq_len(nrow(interval_dt)), function(i) {
        sub_breaks <- lapply(names(breaks), function(ts_col_nm) {
          c(
            interval_dt[[paste0(ts_col_nm, "_lo")]][i],
            interval_dt[[paste0(ts_col_nm, "_hi")]][i]
          )
        })
        names(sub_breaks) <- names(breaks)
        sub_dt <- surv_split(
          dt = .SD,
          breaks = sub_breaks
        )
        if (nrow(sub_dt) == 0) {
          return(NULL)
        }
        surv_merge(sub_dt, merge_by = merge_by, merge_dt = merge_dt)
        agg_expr <- quote(sub_dt[
          j = "placeholder",
          keyby = eval(names(aggre_stratum_dt))
        ])
        agg_expr[["j"]] <- aggre_values_expr
        stratum_interval_value_dt <- eval(agg_expr)
        data.table::set(
          stratum_interval_value_dt,
          j = names(interval_dt),
          value = interval_dt[i, ]
        )
        join_col_nms <- c(names(aggre_stratum_dt), names(interval_dt))
        value_col_nms <- setdiff(names(stratum_interval_value_dt), join_col_nms)
        join_expr <- quote(out[
          i = stratum_interval_value_dt,
          on = join_col_nms,
          j = "placeholder"
        ])
        j_expr <- sprintf(
          "%s := list(%s)",
          deparse1(value_col_nms),
          paste0(
            value_col_nms, " + i.", value_col_nms
          )
        )
        join_expr[["j"]] <- parse(text = j_expr)[[1]]
        NULL
      }))
      0L
    },
    .SDcols = intersect(
      names(dt),
      c(
        names(aggre_stratum_dt),
        merge_by,
        names(breaks),
        all.vars(expr = aggre_values_expr),
        c("lex.id", "lex.dur", "lex.Cst", "lex.Xst")
      )
    ),
    keyby = processing_stratum_col_nm
  ]

  return(out[])
}


surv_split_merge_aggregate_by_row <- function(
  dt,
  breaks = list(cal = 2000:2025, age = 0:100, fot = seq(0, 5, 1 / 12)),
  merge_by = c("sex", "cal", "age"),
  merge_dt = data.table::data.table(
    sex = 0L,
    cal = 2000,
    age = 0,
    haz = 0.010
  ),
  aggre_stratum_dt = data.table::CJ(sex = 0:1, study_arm = 0:1),
  aggre_ts_col_nms = "fot",
  aggre_values = quote(list(
    pyrs = sum(duration),
    n = .N,
    d = sum(status == "dead"),
    w = sum(brenner_weight),
    c = sum(status == "lost to follow-up")
  )),
  n_processing_strata = data.table::getDTthreads() * 2L
) {
  aggre_values_expr <- substitute(aggre_values)
  if (identical(aggre_values_expr[[1]], quote(quote))) {
    aggre_values_expr <- eval(aggre_values_expr)
  }

  breaks <- lapply(breaks, function(x) sort(unique(x)))
  interval_dt <- do.call(
    data.table::CJ,
    lapply(breaks[aggre_ts_col_nms], function(x) {
      x[-length(x)]
    })
  )
  data.table::setkeyv(dt, c(names(aggre_stratum_dt), names(breaks)))
  work_dt <- cbind(aggre_stratum_dt[1L, ], interval_dt[1L, ], interval_dt[1L, ])
  ts_lo_col_nms <- paste0(names(interval_dt), "_lo")
  ts_hi_col_nms <- paste0(names(interval_dt), "_hi")
  names(ts_lo_col_nms) <- names(ts_hi_col_nms) <- names(interval_dt)
  data.table::setnames(
    work_dt,
    c(
      names(aggre_stratum_dt),
      ts_lo_col_nms,
      ts_hi_col_nms
    )
  )
  lapply(seq_len(nrow(dt)), function(i) {
    dt_i <- dt[i, ]
    for (j in seq_len(nrow(interval_dt))) {
      data.table::set(
        work_dt,
        j = ts_lo_col_nms,
        value = lapply(names(interval_dt), function(ts_col_nm) {
          pmax(dt_i[[ts_col_nm]], interval_dt[[ts_lo_col_nms[ts_col_nm]]][j])
        })
      )
    }
  })

  return(NULL)
}
