library(data.table)
library(ggplot2)
library(ggsci)

# The vnapply and discr_si function are taken directly from the EpiEstim R
# package (https://cran.r-project.org/package=EpiEstim)
# They are copyright Cori et al.

# the following is taken directly from EpiEstim:
vnapply <- function(X, FUN, ...) {
  vapply(X, FUN, numeric(1), ...)
}

# serial interval function, taken directly from EpiEstim:
discr_si <- function(k, mu, sigma)
{
  if (sigma < 0) {
    stop("sigma must be >=0.")
  }
  if (mu <= 1) {
    stop("mu must be >1")
  }
  if (any(k < 0)) {
    stop("all values in k must be >=0.")
  }

  a <- ((mu - 1) / sigma)^2
  b <- sigma^2 / (mu - 1)

  cdf_gamma <- function(k, a, b) stats::pgamma(k, shape = a, scale = b)

  res <- k * cdf_gamma(k, a, b) +
    (k - 2) * cdf_gamma(k - 2, a, b) - 2 * (k - 1) * cdf_gamma(k - 1, a, b)
  res <- res + a * b * (2 * cdf_gamma(k - 1, a + 1, b) -
                          cdf_gamma(k - 2, a + 1, b) - cdf_gamma(k, a + 1, b))
  res <- vnapply(res, function(e) max(0, e))

  return(res)
}

#' Calculate predicted number of new case from Rt.
#'
#' Calculated predicted number of new cases from Rt assuming the cases follow a
#' Poisson distribution. Relies on prespecifying the serial interval
#' distribution, which is assumed to have a Gamma distribution with mean SI_mean
#' and standard deviation SI_sd.
#'
#' @param dates Vector of dates corresponding to dates of new_counts
#' @param new_counts Vector of daily new cases.
#' @param rt Rt estimate to use when predicting new cases
#' @param n_new Number of days to predict
#' @param SI_mean Mean for serial interval distribution
#' @param SI_sd Standard deviation for serial interval distribution.
#' @param SI_NTS Number of days for which a current case can infect a new case.
#' Cases which are more than SI_NTS old are assumed to not be able to infect
#' anyone.
#' @return A list. yhat is a vector of predicted new cases, and y_vcov is the
#' variance-covariance matrix of yhat.
calc_pred <- function(dates, new_counts, rt, n_new = 14,
                      SI_mean = 5.2, SI_sd = 5.5, SI_NTS = 30) {
  stopifnot(length(dates) == length(new_counts))

  si_weights <- discr_si(1:SI_NTS, SI_mean, SI_sd)
  yhat <- c(new_counts, rep(NA_real_, n_new))

  y_vcov <- matrix(0.0, nrow = n_new, ncol = n_new)

  for (i in seq_len(n_new)) {
    t_cur <- length(new_counts) + i
    yhat_prev <- yhat[(t_cur - SI_NTS):(t_cur - 1)]
    stopifnot(length(yhat_prev) == SI_NTS)
    yhat[t_cur] <- rt * crossprod(si_weights, yhat_prev)
  }

  for (i in seq_len(n_new)) {
    t_cur <- length(new_counts) + i
    if (i == 1) {
      y_vcov[i, i] <- yhat[length(new_counts) + i]
    } else {
      weights_cur <- si_weights[1:(i - 1)]
      y_vcov[i, i] <- yhat[length(new_counts) + i] +
        (rt^2) * crossprod(weights_cur, y_vcov[1:(i - 1), 1:(i - 1)]) %*%
        weights_cur
    }

    j <- i + 1
    while (j <= n_new) {
      n_weights <- i
      weights_cur <- si_weights[1:(j - 1)]
      vcov_vec <- y_vcov[i, 1:(j - 1)]
      y_vcov[i, j] <- rt * crossprod(weights_cur, vcov_vec)
      y_vcov[j, i] <- rt * crossprod(weights_cur, vcov_vec)
      j <- j + 1
    }
  }

  preds <- yhat[(length(yhat) - n_new + 1):length(yhat)]
  stopifnot(length(preds) == n_new)
  ret <- list(yhat = preds, y_vcov = y_vcov)
  return(ret)
}

#' Calculate weekly new case predictions from daily new case predictions.
#'
#' Calculate weekly new case predictions from daily new case predictions. Can
#' pass in the output of calc_pred. Assumes the first element of yhat is the
#' start of a new week, and aggregates each subsequent block of 7 days. If the
#' total length of yhat is not divisible by 7, only calculates predictions for
#' the first floor(length(yhat) / 7) weeks. So, if you want to predict for every
#' week starting on a Monday, make sure the first element of yhat is a Monday.
#'
#' @param yhat Estimated number of new cases.
#' @param y_vcov Variance-covariance matrix of yhat.
#'
#' @return A list. yhat is a vector of predicted new cases, and y_vcov is the
#' variance-covariance matrix of yhat.
calc_week_preds_from_daily <- function(yhat, y_vcov) {
  n_weeks <- floor(length(yhat) / 7)
  week_idx <- rep(seq_len(n_weeks), each = 7)
  keep_idx <- seq_len(7 * n_weeks)
  week_preds <- tapply(yhat[keep_idx], week_idx, sum)

  ones_mat <- matrix(0.0, nrow = n_weeks, ncol = 7 * n_weeks)
  for (i in seq_len(n_weeks)) {
    week_start <- 1 + ((i - 1) * 7)
    week_end <- i * 7
    ones_mat[i, week_start:week_end] <- 1
  }
  week_vcov <- ones_mat %*% tcrossprod(y_vcov[keep_idx, keep_idx], ones_mat)

  ret <- list(yhat = week_preds, y_vcov = week_vcov)
}

#' Run new case predictions.
#'
#' Run new case predictions for daily and weekly new cases.
#'
#' @param df A subset of rt_long_all. Should contain the columns
#' positiveIncrease (number of daily new cases) and date (date of daily new
#' cases).
#' @param pred_start_date Date from which to start predicting. Dates <
#' pred_start_date will be used to calculate the prediction.
#' @param rt Vector of Rts to use for the prediction. If NULL (the default),
#' assumes that df has the elements rt_lower, rt, and rt_upper, and uses those
#' as the Rts. The names of the Rts will be put in the rt_type column in the
#' returned data.tables.
#' @param n_new Number of days to calculate the daily predictions. For the
#' weekly predcitions, predict floor(n_new / 7) weeks.
#'
#' @return A list with the following items:
#' \describe{
#'  \item{daily_plot_df}{data.table of daily predictions and daily observed data
#'  for the full period. Includes the columns
#'  positiveIncrease (predicted new cases), date, rt_type (actual or names(rt)),
#'  y_lwr (lower 95% CI of predicted cases), y_upr (upper 95% CI of predicted
#'  cases), and type (predicted or actual).}
#'  \item{weekly_plot_df}{data.table of weekly predictions. Includes the same
#'  columns as daily_plot_df.}
#'  \item{daily_pred_df}{data.table of daily predictions only. Does not include
#'  the observed data.}
#'  \item{weekly_pred_df}{data.table of weekly predictions only. Does not
#'  include the observed data.}
# }
run_prediction <- function(df, pred_start_date, rt = NULL, n_new = 14) {
  df_subset <- df[date < pred_start_date, ]
  if (is.null(rt)) {
    rt <- as.numeric(tail(df_subset, 1)[, c("rt_lower", "rt", "rt_upper")])
    names(rt) <- c("lower 95% CI", "estimate", "upper 95% CI")
    if (anyNA(rt)) {
      return(list())
    }
  }
  stopifnot(is.numeric(rt))
  if (is.null(names(rt))) {
    names(rt) <- paste0("rt_", seq_along(rt))
  }

  daily_preds <- lapply(rt, function(rt_cur) {
                        calc_pred(df_subset$date, df_subset$positiveIncrease,
                                  rt_cur, n_new = n_new)
                  })
  weekly_preds <- lapply(daily_preds, function(pred_ret) {
                      calc_week_preds_from_daily(pred_ret$yhat, pred_ret$y_vcov)
                  })

  # munge daily predictions
  daily_preds_df_lst <- lapply(daily_preds, function(pred_ret) {
    plt_df <- data.table(positiveIncrease = pred_ret$yhat,
                        date = seq(from = max(df_subset$date) + 1, by = 1,
                                   length.out = n_new),
                        y_lwr = pred_ret$yhat - 1.96 * sqrt(diag(pred_ret$y_vcov)),
                        y_upr = pred_ret$yhat + 1.96 * sqrt(diag(pred_ret$y_vcov)),
                        type = rep("predicted", n_new))
  })

  daily_preds_df <- do.call(rbind, daily_preds_df_lst)
  daily_preds_df$rt_type <- rep(names(rt), lapply(daily_preds_df_lst, nrow))

  daily_actual <- with(df,
                       data.table(positiveIncrease = positiveIncrease,
                                  date = date,
                                  rt_type = "actual",
                                  type = "actual",
                                  y_lwr = positiveIncrease,
                                  y_upr = positiveIncrease))
  daily_plot_df <- rbind(daily_actual, daily_preds_df, fill = TRUE)
  daily_plot_df[, type_fct := factor(type, levels = c("predicted", "estimated",
                                                      "actual"))]

  # munge weekly predictions
  weekly_preds_df_lst <- lapply(weekly_preds, function(pred_ret) {
    plt_df <- data.table(positiveIncrease = as.numeric(pred_ret$yhat),
                         date = seq(from = max(df_subset$date) + 1, by = 7,
                                    length.out = length(pred_ret$yhat)),
                         y_lwr = as.numeric(pred_ret$yhat -
                                            1.96 * sqrt(diag(pred_ret$y_vcov))),
                         y_upr = as.numeric(pred_ret$yhat +
                                            1.96 * sqrt(diag(pred_ret$y_vcov))),
                         type = rep("predicted", length(pred_ret$yhat)))
  })

  weekly_preds_df <- do.call(rbind, weekly_preds_df_lst)
  weekly_preds_df$rt_type <- rep(c("lower", "estimate", "upper"),
                              lapply(weekly_preds_df_lst, nrow))

  n_days <- sum((df$date <= pred_start_date + 7))
  n_weeks <- floor(n_days / 7)
  week_num <- rev(c(rep(seq(n_weeks), each = 7),
                    rep(n_weeks + 1, n_days - 7 * n_weeks)))
  df_weekly <- df[date <= pred_start_date + 7,
                  .(positiveIncrease = sum(positiveIncrease),
                    date = min(date),
                    week_end = max(date)), by = week_num]
  weekly_actual <- with(df_weekly,
                        data.table(positiveIncrease = positiveIncrease,
                                   date = date,
                                   rt_type = "actual",
                                   type = "actual",
                                   y_lwr = positiveIncrease,
                                   y_upr = positiveIncrease))
  weekly_plot_df <- rbind(weekly_actual, weekly_preds_df, fill = TRUE)

  ret <- list(daily_plot_df = daily_plot_df,
              weekly_plot_df = weekly_plot_df,
              daily_preds_df = daily_preds_df,
              weekly_preds_df = weekly_preds_df)
  return(ret)
}

draw_weekly_plot <- function(pred_ret, dispID, start_date, end_date) {
  plt_dat <- pred_ret$weekly_plot_df[date >= start_date & date <= end_date, ]

  actual_dates <- plt_dat[rt_type == "actual", date]
  ord <- order(actual_dates, decreasing = TRUE)
  last_actual <- plt_dat[ord[2], .(positiveIncrease, date)]
  connect1 <- do.call(rbind, replicate(3, last_actual, simplify = FALSE))
  connect2 <- plt_dat[rt_type != "actual" & date == actual_dates[ord[1]],
                      .(positiveIncrease, date)]
  connect_df <- cbind(connect1, connect2)
  colnames(connect_df) <- c("y1", "x1", "y2", "x2")

  ggplot(plt_dat, aes(y = positiveIncrease)) +
    geom_line(aes(x = date, color = rt_type, linetype = type)) +
    geom_pointrange(aes(x = date, ymin = y_lwr, ymax = y_upr, color = rt_type,
                      group = date)) +
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), linetype = 2,
                 data = connect_df) +
    scale_linetype_discrete(guide = FALSE) +
    scale_color_nejm(name = "Type") +
    ggtitle(sprintf("Prediction of Weekly New Cases for %s", dispID)) +
    xlab("Date") + ylab("Weekly New Cases") +
    theme_cowplot() +
    background_grid(major = "xy", minor = "xy") +
    theme(text = element_text(size = 18),
          axis.text = element_text(size = 15),
          legend.position = "bottom")
}
