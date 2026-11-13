# ============================================================
# FitVerse: Parametric Distribution Fitting and Analysis
# ============================================================
# File       : report.R
# Description: Automated report generation from a fitverse object.
#
#              generate_report() writes a self-contained HTML
#              report containing:
#                1. Executive summary (best-fit, key metrics)
#                2. Data description (statistics + histogram)
#                3. Top 5 fitted distributions (parameters +
#                   goodness-of-fit tables per distribution)
#                4. Four diagnostic plots (density overlay,
#                   Q-Q, P-P, CDF overlay) as embedded images
#                5. Full model comparison table
#
#              The HTML path requires no external dependencies
#              beyond base R + ggplot2 (which FitVerse already
#              imports).  PDF output uses rmarkdown::render().
#
# Authors:
#   Karuna G. Reddy <karuna.reddy@auckland.ac.nz>
#   University of Auckland, New Zealand
#
#   M. G. M. Khan <mgm.khan@usp.ac.fj>
#   University of the South Pacific, Fiji
#
# Package : FitVerse
# Version : 1.0-1
# Date    : 2026-06-17
# License : GPL (>= 3)
# ============================================================

# Suppress R CMD check NOTE for ggplot2 aes() variables used in .build_report_pp_plot
utils::globalVariables(c("emp", "theor"))

#' Generate an automated distribution-fitting report
#'
#' Produces a self-contained HTML report (or PDF via rmarkdown) from a
#' \code{fitverse} object.  HTML output requires no dependencies beyond
#' ggplot2 and optionally base64enc or jsonlite for embedding plot images.
#'
#' @param fit         A \code{fitverse} object.
#' @param output_file Path to the output file.  Extension determines format:
#'                    \code{".html"} (default) or \code{".pdf"}.
#'                    Defaults to \code{"fitverse_report.html"} in the
#'                    current working directory.
#' @param title       Report title.
#'                    Default \code{"FitVerse: Distribution Fitting Report"}.
#' @param author      Author name shown in the report header.
#' @param boot        Optional \code{fitverse_bootstrap} object from
#'                    \code{\link{bootstrap_ci}()}.
#' @param top_n       Number of top distributions to feature in detail.
#'                    Default \code{5}.
#' @param open        Open the file in the default browser after writing?
#'                    Default \code{TRUE}.
#'
#' @return Invisibly, the path to the generated report.
#' @export
#' @examples
#' \donttest{
#' set.seed(42)
#' x  <- rgamma(300, shape = 2, rate = 0.5)
#' fv <- fitverse(x, xlab = "Daily precipitation (mm)", verbose = FALSE)
#' generate_report(fv,
#'                 output_file = file.path(tempdir(), "fitverse_report.html"),
#'                 open = FALSE)
#' }
generate_report <- function(fit,
                             output_file = NULL,
                             title       = "FitVerse: Distribution Fitting Report",
                             author      = "",
                             boot        = NULL,
                             top_n       = 5L,
                             open        = TRUE) {

  stopifnot(inherits(fit, "fitverse"))

  if (is.null(output_file))
    output_file <- file.path(tempdir(), "fitverse_report.html")

  ext <- tolower(tools::file_ext(output_file))
  if (!ext %in% c("html", "pdf"))
    stop("output_file must end in .html or .pdf", call. = FALSE)

  if (ext == "pdf") {
    use_rmd <- requireNamespace("rmarkdown", quietly = TRUE) &&
               requireNamespace("knitr",     quietly = TRUE)
    if (!use_rmd)
      stop("PDF output requires the 'rmarkdown' and 'knitr' packages.",
           call. = FALSE)
    .report_rmarkdown(fit = fit, output_file = output_file,
                      title = title, author = author,
                      boot = boot, top_n = top_n, ext = ext)
  } else {
    .report_html_full(fit = fit, output_file = output_file,
                      title = title, author = author,
                      boot = boot, top_n = top_n)
  }

  message(sprintf("[FitVerse] Report written to: %s", output_file))

  if (open && interactive()) {
    tryCatch(utils::browseURL(output_file), error = function(e) NULL)
  }

  invisible(output_file)
}


# -- Full HTML report ----------------------------------------------------------

#' @keywords internal
.report_html_full <- function(fit, output_file, title, author, boot, top_n) {

  ds    <- fit$data_summary
  bf    <- fit$best_fit
  entry <- get_dist_entry(bf$dist_key)
  xlab  <- if (!is.null(ds$xlab) && nzchar(ds$xlab)) ds$xlab else "x"

  top_n <- min(as.integer(top_n), nrow(fit$ranking))

  # -- Generate all plots upfront ---------------------------------------------
  img_density <- .plot_to_base64(.plot_density_panel(fit, top_n = min(top_n, 5L),
                                                      xlab = xlab),
                                  w = 1100, h = 550, res = 120)

  img_qq  <- .plot_to_base64(.plot_qq_panel(fit, xlab = xlab),
                              w = 800, h = 580, res = 120)

  img_pp  <- .plot_to_base64(.build_report_pp_plot(fit, xlab = xlab),
                              w = 800, h = 580, res = 120)

  img_cdf <- .plot_to_base64(.build_report_cdf_plot(fit, top_n = min(top_n, 5L),
                                                      xlab = xlab),
                              w = 1100, h = 550, res = 120)

  # -- Section 1: Executive summary card --------------------------------------
  ks_p  <- get_ks_pvalue(bf)
  ad_p  <- get_ad_pvalue(bf)
  cs_p  <- get_chisq_pvalue(bf)

  ks_badge  <- .pval_badge(ks_p)
  ad_badge  <- .pval_badge(ad_p)
  cs_badge  <- .pval_badge(cs_p)

  summary_html <- sprintf(
    '<div class="summary-grid">
      <div class="summary-item">
        <div class="summary-label">Best-Fit Distribution</div>
        <div class="summary-value primary">%s</div>
        <div class="summary-sub">%s estimation</div>
      </div>
      <div class="summary-item">
        <div class="summary-label">Sample Size</div>
        <div class="summary-value">%d</div>
        <div class="summary-sub">observations</div>
      </div>
      <div class="summary-item">
        <div class="summary-label">AIC</div>
        <div class="summary-value">%s</div>
        <div class="summary-sub">BIC: %s</div>
      </div>
      <div class="summary-item">
        <div class="summary-label">Kolmogorov-Smirnov</div>
        <div class="summary-value">%s</div>
        <div class="summary-sub">p = %s</div>
      </div>
      <div class="summary-item">
        <div class="summary-label">Anderson-Darling</div>
        <div class="summary-value">%s</div>
        <div class="summary-sub">p = %s</div>
      </div>
      <div class="summary-item">
        <div class="summary-label">Chi-Squared</div>
        <div class="summary-value">%s</div>
        <div class="summary-sub">p = %s</div>
      </div>
    </div>',
    entry$display_name, bf$method,
    fit$n,
    .fmt(bf$aic, 2), .fmt(bf$bic, 2),
    ks_badge, .fmt_p(ks_p),
    ad_badge, .fmt_p(ad_p),
    cs_badge, .fmt_p(cs_p)
  )

  # -- Section 2: Data description --------------------------------------------
  cv_str <- if (!is.null(ds$cv) && is.finite(ds$cv)) .fmt(ds$cv, 4) else "N/A"
  data_stats_html <- .two_col_table(
    c("n", "Missing", "Mean", "Std Dev", "CV (%)",
      "Median", "IQR", "Skewness", "Excess Kurtosis", "Min", "Max", "Range"),
    c(ds$n, ds$n_missing,
      .fmt(ds$mean, 4), .fmt(ds$sd, 4), cv_str,
      .fmt(ds$median, 4), .fmt(ds$iqr, 4),
      .fmt(ds$skewness, 4), .fmt(ds$kurtosis, 4),
      .fmt(ds$min, 4), .fmt(ds$max, 4), .fmt(ds$range, 4))
  )

  # -- Section 3: Top distributions -------------------------------------------
  top_cards_html <- ""
  for (i in seq_len(top_n)) {
    row_i <- fit$ranking[i, ]
    fk_i  <- paste0(row_i$dist_key, "_", row_i$Method)
    fi    <- fit$all_fits[[fk_i]]
    if (is.null(fi)) next
    ei    <- tryCatch(get_dist_entry(row_i$dist_key), error = function(e) NULL)
    if (is.null(ei)) next

    # Parameter table
    param_rows <- paste(vapply(seq_along(fi$params), function(j)
      sprintf("<tr><td>%s</td><td class='num'>%s</td></tr>",
              names(fi$params)[j], .fmt(fi$params[j], 6)),
      character(1)), collapse = "\n")

    # GoF table for this distribution
    gof_tbl_html <- ""
    if (!is.null(fi$gof)) {
      gof_rows <- paste(apply(fi$gof, 1, function(r) {
        pv <- suppressWarnings(as.numeric(r["P_value"]))
        sprintf("<tr><td>%s</td><td class='num'>%s</td><td>%s</td><td class='stars'>%s</td></tr>",
                r["Test"], .fmt_num(r["Statistic"], 4),
                .fmt_p(pv), r["Stars"])
      }), collapse = "\n")
      gof_tbl_html <- sprintf(
        '<table class="inner-table">
          <thead><tr><th>Test</th><th>Statistic</th><th>p-value</th><th>Sig.</th></tr></thead>
          <tbody>%s</tbody>
        </table>', gof_rows)
    } else {
      gof_tbl_html <- "<p class='muted'>GoF tests not available.</p>"
    }

    rank_badge_colour <- c("#2e7d32", "#1565c0", "#6a1b9a", "#e65100", "#37474f")
    bc <- rank_badge_colour[min(i, length(rank_badge_colour))]

    conv_html <- if (isTRUE(row_i$Converged))
      '<span class="badge green">Converged</span>'
    else
      '<span class="badge red">Did not converge</span>'

    top_cards_html <- paste0(top_cards_html, sprintf(
      '<div class="dist-card">
        <div class="dist-card-header" style="border-left:5px solid %s;">
          <span class="rank-badge" style="background:%s;">%d</span>
          <span class="dist-name">%s</span>
          <span class="badge method">%s</span>
          %s
          <span class="dist-aic">AIC: %s &nbsp;|&nbsp; BIC: %s &nbsp;|&nbsp; &Delta;AIC: %s</span>
        </div>
        <div class="dist-card-body">
          <div class="card-col">
            <h4>Parameters</h4>
            <table class="inner-table">
              <thead><tr><th>Parameter</th><th>Estimate</th></tr></thead>
              <tbody>%s</tbody>
            </table>
          </div>
          <div class="card-col">
            <h4>Goodness-of-Fit</h4>
            %s
          </div>
        </div>
      </div>',
      bc, bc, i,
      ei$display_name,
      row_i$Method,
      conv_html,
      .fmt(row_i$AIC, 2), .fmt(row_i$BIC, 2), .fmt(row_i$Delta_AIC, 2),
      param_rows,
      gof_tbl_html
    ))
  }

  # -- Section 4: Diagnostic plots --------------------------------------------
  make_img <- function(b64, alt) {
    if (nchar(b64) == 0)
      return(sprintf('<p class="muted">%s could not be rendered.</p>', alt))
    sprintf('<img src="data:image/png;base64,%s" class="report-plot" alt="%s"/>', b64, alt)
  }

  plots_html <- sprintf(
    '<div class="plot-section">
      <div class="plot-block full">
        <h3>Probability Density Function (top %d overlay)</h3>
        %s
      </div>
      <div class="plot-row">
        <div class="plot-block half">
          <h3>Q-Q Plot (best fit)</h3>
          %s
        </div>
        <div class="plot-block half">
          <h3>P-P Plot (best fit)</h3>
          %s
        </div>
      </div>
      <div class="plot-block full">
        <h3>Cumulative Distribution Function (top %d overlay)</h3>
        %s
      </div>
    </div>',
    min(top_n, 5L), make_img(img_density, "Density plot"),
    make_img(img_qq, "Q-Q plot"),
    make_img(img_pp, "P-P plot"),
    min(top_n, 5L), make_img(img_cdf, "CDF plot")
  )

  # -- Section 5: Bootstrap CI ------------------------------------------------
  boot_html <- ""
  if (!is.null(boot)) {
    boot_html <- paste0(
      sprintf('<h2 id="bootstrap">Bootstrap Confidence Intervals</h2>
               <p>%d%% CI &bull; B = %d replicates</p>',
              round(boot$conf * 100), boot$B),
      .df_to_html_table(boot$params_ci, class = "report-table")
    )
    if (!is.null(boot$return_level_ci)) {
      boot_html <- paste0(boot_html,
                          "<h3>Return Levels</h3>",
                          .df_to_html_table(boot$return_level_ci,
                                            class = "report-table"))
    }
  }

  # -- Section 6: Full model comparison ---------------------------------------
  rank_tbl  <- fit$ranking
  show_cols <- c("Rank", "Distribution", "Method", "AIC", "Delta_AIC",
                 "BIC", "KS_p", "AD_p", "ChiSq_p", "Converged")
  show_cols <- show_cols[show_cols %in% names(rank_tbl)]
  full_tbl_html <- .df_to_html_table(rank_tbl[, show_cols, drop = FALSE],
                                      class = "report-table sortable",
                                      stripe = TRUE)

  # -- Assemble page ----------------------------------------------------------
  author_html <- if (nchar(trimws(author)) > 0)
    sprintf('<div class="report-author">%s</div>', author) else ""

  boot_nav <- if (!is.null(boot))
    '<li><a href="#bootstrap">Bootstrap CI</a></li>' else ""

  html <- sprintf(
    '<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8"/>
<meta name="viewport" content="width=device-width, initial-scale=1"/>
<title>%s</title>
<style>
%s
</style>
</head>
<body>
<nav class="sidebar" id="toc">
  <div class="toc-title">FitVerse</div>
  <ul>
    <li><a href="#summary">Executive Summary</a></li>
    <li><a href="#data">Data Description</a></li>
    <li><a href="#top-dists">Top Distributions</a></li>
    <li><a href="#diagnostic">Diagnostic Plots</a></li>
    %s
    <li><a href="#comparison">Full Comparison</a></li>
  </ul>
</nav>
<main class="report-main">
  <header class="report-header">
    <h1>%s</h1>
    %s
    <p class="report-meta">Generated: %s &bull; Variable: <strong>%s</strong> &bull; n = %d</p>
  </header>

  <section id="summary">
    <h2>Executive Summary</h2>
    %s
  </section>

  <section id="data">
    <h2>Data Description</h2>
    %s
  </section>

  <section id="top-dists">
    <h2>Top %d Fitted Distributions</h2>
    %s
  </section>

  <section id="diagnostic">
    <h2>Diagnostic Plots</h2>
    %s
  </section>

  %s

  <section id="comparison">
    <h2>Full Model Comparison</h2>
    <p class="muted">All distributions ranked by GoF tier, then AIC. &Delta;AIC relative to best model.</p>
    %s
  </section>

  <footer>
    Generated by the <strong>FitVerse</strong> R package
  </footer>
</main>
</body>
</html>',
    title,
    .report_css(),
    boot_nav,
    title,
    author_html,
    format(Sys.time(), "%Y-%m-%d %H:%M"),
    xlab, fit$n,
    summary_html,
    data_stats_html,
    top_n, top_cards_html,
    plots_html,
    boot_html,
    full_tbl_html
  )

  writeLines(html, output_file)
}


# -- Plot helpers --------------------------------------------------------------

#' Convert a ggplot2 object to a base64-encoded PNG string
#' @keywords internal
.plot_to_base64 <- function(p, w = 1000, h = 600, res = 120) {
  if (is.null(p)) return("")
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  tryCatch({
    grDevices::png(tmp, width = w, height = h, res = res)
    print(p)
    grDevices::dev.off()
    if (!file.exists(tmp) || file.size(tmp) == 0L) return("")
    raw <- readBin(tmp, "raw", file.size(tmp))
    base64enc_simple(raw)
  }, error = function(e) {
    tryCatch(grDevices::dev.off(), error = function(e) NULL)
    ""
  })
}


#' Build a P-P plot for the report (Hazen plotting positions)
#' @keywords internal
.build_report_pp_plot <- function(fv_obj, xlab = "x") {
  data  <- fv_obj$data
  fit   <- fv_obj$best_fit
  n     <- length(data)
  entry <- tryCatch(get_dist_entry(fit$dist_key), error = function(e) NULL)
  if (is.null(entry)) return(NULL)

  p_fun <- tryCatch(.lookup_dpqr("p", fit$dist_key), error = function(e) NULL)
  if (is.null(p_fun)) return(NULL)

  p_emp <- (seq_len(n) - 0.5) / n          # Hazen plotting positions
  x_ord <- sort(data)

  p_theor <- tryCatch(
    pmax(0, pmin(1, do.call(p_fun, c(list(q = x_ord), as.list(fit$params))))),
    error = function(e) NULL
  )
  if (is.null(p_theor) || !all(is.finite(p_theor))) return(NULL)

  df_pp <- data.frame(theor = p_theor, emp = p_emp)

  ggplot2::ggplot(df_pp, ggplot2::aes(x = theor, y = emp)) +
    ggplot2::geom_abline(slope = 1, intercept = 0,
                         colour = "#c0392b", linetype = "dashed", linewidth = 0.85) +
    ggplot2::geom_point(colour = "#2d6bc4", alpha = 0.72, size = 2.2) +
    ggplot2::labs(
      title    = sprintf("P-P Plot  --  %s (%s)", entry$display_name, fit$method),
      subtitle = sprintf("n = %d  |  AIC = %.2f", n, fit$aic),
      x        = "Theoretical CDF",
      y        = "Empirical CDF (Hazen)"
    ) +
    ggplot2::coord_fixed(ratio = 1, xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::theme_light(base_size = 13) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "#EDF2F7", colour = NA),
      plot.background  = ggplot2::element_rect(fill = "white",   colour = NA),
      plot.title       = ggplot2::element_text(face = "bold", size = 13,
                                               colour = "#1A2B3C"),
      plot.subtitle    = ggplot2::element_text(colour = "#4A6278", size = 10.5),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#D3DCE6", linewidth = 0.4),
      axis.title       = ggplot2::element_text(size = 11, colour = "#1A2B3C"),
      axis.text        = ggplot2::element_text(size = 10, colour = "#354A5E"),
      plot.margin      = ggplot2::margin(12, 16, 8, 12)
    )
}


#' Build a CDF overlay plot for the report (top_n distributions)
#' @keywords internal
.build_report_cdf_plot <- function(fv_obj, top_n = 3L, xlab = "x") {
  data    <- fv_obj$data
  n       <- length(data)
  ranking <- fv_obj$ranking
  top_n   <- min(top_n, nrow(ranking))

  # Empirical CDF (Hazen)
  x_ord  <- sort(data)
  p_emp  <- (seq_len(n) - 0.5) / n
  df_emp <- data.frame(x = x_ord, y = p_emp, label = "Empirical CDF")

  # x-grid for theoretical curves
  x_rng  <- range(data)
  x_buf  <- diff(x_rng) * 0.05
  x_grid <- seq(x_rng[1] - x_buf, x_rng[2] + x_buf, length.out = 400)

  colours <- c("#2d6bc4", "#27ae60", "#c0392b", "#8e44ad", "#e67e22")
  curve_list <- vector("list", top_n)

  for (i in seq_len(top_n)) {
    row_i  <- ranking[i, ]
    fk_i   <- paste0(row_i$dist_key, "_", row_i$Method)
    fi     <- fv_obj$all_fits[[fk_i]]
    if (is.null(fi)) next
    ei     <- tryCatch(get_dist_entry(row_i$dist_key), error = function(e) NULL)
    if (is.null(ei)) next
    p_fun  <- tryCatch(.lookup_dpqr("p", row_i$dist_key), error = function(e) NULL)
    if (is.null(p_fun)) next

    cdf_vals <- tryCatch(
      pmax(0, pmin(1, do.call(p_fun, c(list(q = x_grid), as.list(fi$params))))),
      error = function(e) NULL
    )
    if (is.null(cdf_vals) || !any(is.finite(cdf_vals))) next

    lbl <- sprintf("%s (%s)", ei$display_name, row_i$Method)
    curve_list[[i]] <- data.frame(x = x_grid, y = cdf_vals, label = lbl,
                                   colour = colours[i],
                                   lw = if (i == 1L) 1.4 else 0.8,
                                   stringsAsFactors = FALSE)
  }
  curve_list <- Filter(Negate(is.null), curve_list)
  if (length(curve_list) == 0L) return(NULL)

  df_curves <- do.call(rbind, curve_list)
  n_drawn   <- length(unique(df_curves$label))
  col_map   <- stats::setNames(colours[seq_len(n_drawn)],
                                unique(df_curves$label))

  p <- ggplot2::ggplot() +
    ggplot2::geom_point(data = df_emp,
                        ggplot2::aes(x = x, y = y),
                        colour = "#555", size = 0.8, alpha = 0.5) +
    ggplot2::geom_line(data = df_curves,
                       ggplot2::aes(x = x, y = y, colour = label),
                       linewidth = 0.9) +
    ggplot2::scale_colour_manual(values = col_map, name = NULL) +
    ggplot2::labs(
      title    = "CDF Overlay",
      subtitle = sprintf("Empirical CDF (dots) vs. top %d fitted distributions", top_n),
      x        = xlab,
      y        = "F(x)"
    ) +
    ggplot2::theme_light(base_size = 13) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "#EDF2F7", colour = NA),
      plot.background  = ggplot2::element_rect(fill = "white",   colour = NA),
      plot.title       = ggplot2::element_text(face = "bold", size = 13,
                                               colour = "#1A2B3C"),
      plot.subtitle    = ggplot2::element_text(colour = "#4A6278", size = 10.5),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "#D3DCE6",
                                                  linewidth = 0.4),
      axis.title       = ggplot2::element_text(size = 11, colour = "#1A2B3C"),
      axis.text        = ggplot2::element_text(size = 10, colour = "#354A5E"),
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.background = ggplot2::element_blank(),
      legend.text      = ggplot2::element_text(size = 10, colour = "#1A2B3C"),
      plot.margin      = ggplot2::margin(12, 16, 8, 12)
    )
  p
}


# -- HTML helpers --------------------------------------------------------------

#' @keywords internal
.two_col_table <- function(labels, values, caption = NULL) {
  rows <- paste(vapply(seq_along(labels), function(i)
    sprintf("  <tr><td class='stat-label'>%s</td><td class='num'>%s</td></tr>",
            labels[i], values[i]),
    character(1)), collapse = "\n")
  cap <- if (!is.null(caption))
    sprintf('<caption>%s</caption>', caption) else ""
  sprintf('<table class="report-table two-col">%s\n%s\n</table>', cap, rows)
}


#' @keywords internal
.df_to_html_table <- function(df, class = "report-table", stripe = TRUE) {
  if (is.null(df) || nrow(df) == 0L) return("")
  header <- paste0("<th>", names(df), "</th>", collapse = "")
  rows <- apply(df, 1, function(r) {
    cells <- paste0("<td>", r, "</td>", collapse = "")
    sprintf("<tr>%s</tr>", cells)
  })
  sprintf('<table class="%s"><thead><tr>%s</tr></thead><tbody>%s</tbody></table>',
          class, header, paste(rows, collapse = "\n"))
}


#' @keywords internal
.pval_badge <- function(p) {
  if (is.null(p) || is.na(p)) return('<span class="badge grey">N/A</span>')
  if (p >= 0.10) return('<span class="badge green">Good fit</span>')
  if (p >= 0.05) return('<span class="badge yellow">Marginal</span>')
  return('<span class="badge red">Poor fit</span>')
}


#' @keywords internal
.fmt <- function(x, digits = 4) {
  if (is.null(x) || is.na(x)) return("--")
  format(round(as.numeric(x), digits), big.mark = ",", scientific = FALSE)
}


#' @keywords internal
.fmt_num <- function(x, digits = 4) {
  v <- suppressWarnings(as.numeric(x))
  if (is.na(v)) return("--")
  format(round(v, digits), scientific = FALSE)
}


#' @keywords internal
.fmt_p <- function(p) {
  if (is.null(p) || is.na(p)) return("--")
  p <- as.numeric(p)
  if (p < 0.0001) return("< 0.0001")
  format(round(p, 4), nsmall = 4)
}


#' Minimal base64 encoder (avoids base64enc dependency when possible)
#' @keywords internal
base64enc_simple <- function(raw_vec) {
  if (requireNamespace("base64enc", quietly = TRUE))
    return(base64enc::base64encode(raw_vec))
  if (requireNamespace("jsonlite", quietly = TRUE))
    return(jsonlite::base64_enc(raw_vec))
  ""
}


# -- Report CSS ----------------------------------------------------------------

#' @keywords internal
.report_css <- function() {
'/* -- Base --------------------------------------- */
*,*::before,*::after{box-sizing:border-box;margin:0;padding:0}
body{font-family:"Segoe UI",system-ui,Arial,sans-serif;font-size:15px;
     color:#1a2b3c;background:#f5f7fa;line-height:1.6}
a{color:#2d6bc4;text-decoration:none}
a:hover{text-decoration:underline}

/* -- Sidebar TOC --------------------------------- */
.sidebar{position:fixed;top:0;left:0;width:210px;height:100vh;
         background:#1e3a5f;padding:28px 0 20px;overflow-y:auto;z-index:100}
.toc-title{color:#ffffff;font-size:1.1rem;font-weight:700;
           padding:0 20px 16px;border-bottom:1px solid rgba(255,255,255,0.15);
           margin-bottom:12px}
.sidebar ul{list-style:none;padding:0 8px}
.sidebar li{margin:2px 0}
.sidebar a{display:block;color:rgba(255,255,255,0.75);padding:7px 12px;
           border-radius:6px;font-size:0.87rem;transition:all .15s}
.sidebar a:hover{background:rgba(255,255,255,0.12);color:#fff;
                 text-decoration:none}

/* -- Main content -------------------------------- */
.report-main{margin-left:210px;padding:0 40px 60px;max-width:1100px}
.report-header{background:linear-gradient(135deg,#1e3a5f 0%,#2d6bc4 100%);
               color:#fff;padding:36px 40px;margin:0 -40px 40px;
               border-radius:0 0 12px 12px}
.report-header h1{font-size:1.7rem;font-weight:700;margin-bottom:6px}
.report-author{font-size:0.95rem;opacity:0.85;margin-bottom:4px}
.report-meta{font-size:0.85rem;opacity:0.75}

/* -- Sections ----------------------------------- */
section{margin-bottom:50px}
h2{font-size:1.25rem;color:#1e3a5f;border-bottom:2px solid #d0daea;
   padding-bottom:8px;margin-bottom:20px;margin-top:0}
h3{font-size:1.05rem;color:#2a4a6e;margin:18px 0 10px}
h4{font-size:0.92rem;color:#354a5e;margin:0 0 8px;text-transform:uppercase;
   letter-spacing:.04em}
p{margin-bottom:10px}
.muted{color:#6c7a8d;font-size:0.9rem}

/* -- Executive summary grid ---------------------- */
.summary-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(160px,1fr));
              gap:16px;margin-top:8px}
.summary-item{background:#fff;border:1px solid #d8e3ef;border-radius:10px;
              padding:16px 18px;box-shadow:0 1px 4px rgba(0,0,0,.06)}
.summary-label{font-size:0.78rem;color:#6c7a8d;text-transform:uppercase;
               letter-spacing:.05em;margin-bottom:6px}
.summary-value{font-size:1.35rem;font-weight:700;color:#1e3a5f;line-height:1.2}
.summary-value.primary{color:#2d6bc4}
.summary-sub{font-size:0.78rem;color:#8a9ab0;margin-top:3px}

/* -- Badges -------------------------------------- */
.badge{display:inline-block;padding:2px 10px;border-radius:20px;
       font-size:0.78rem;font-weight:600;white-space:nowrap}
.badge.green{background:#e8f5e9;color:#2e7d32}
.badge.yellow{background:#fff8e1;color:#e65100}
.badge.red{background:#fbe9e7;color:#c62828}
.badge.grey{background:#f0f0f0;color:#666}
.badge.method{background:#e3eef8;color:#1565c0;margin-left:6px}

/* -- Distribution cards -------------------------- */
.dist-card{background:#fff;border:1px solid #d8e3ef;border-radius:10px;
           margin-bottom:20px;overflow:hidden;
           box-shadow:0 1px 5px rgba(0,0,0,.06)}
.dist-card-header{display:flex;align-items:center;flex-wrap:wrap;gap:8px;
                  padding:14px 18px;background:#f8fafd;
                  border-bottom:1px solid #e4edf7}
.rank-badge{width:28px;height:28px;border-radius:50%;color:#fff;
            font-weight:700;font-size:0.9rem;display:inline-flex;
            align-items:center;justify-content:center;flex-shrink:0}
.dist-name{font-size:1.05rem;font-weight:700;color:#1e3a5f}
.dist-aic{font-size:0.82rem;color:#6c7a8d;margin-left:auto}
.dist-card-body{display:grid;grid-template-columns:1fr 1fr;gap:0}
.card-col{padding:18px 20px}
.card-col:first-child{border-right:1px solid #edf2f7}

/* -- Tables -------------------------------------- */
.report-table{border-collapse:collapse;width:100%;margin:12px 0;
              font-size:0.88rem}
.report-table th{background:#1e3a5f;color:#fff;padding:9px 12px;
                 text-align:left;font-weight:600;white-space:nowrap}
.report-table td{padding:7px 12px;border-bottom:1px solid #edf2f7;
                 vertical-align:top}
.report-table tr:nth-child(even) td{background:#f7fafd}
.report-table tr:hover td{background:#eef4fb}
.two-col{max-width:400px}
.inner-table{border-collapse:collapse;width:100%;font-size:0.85rem}
.inner-table th{background:#2d6bc4;color:#fff;padding:6px 10px;
                font-weight:600;text-align:left}
.inner-table td{padding:5px 10px;border-bottom:1px solid #e8eef5}
.inner-table tr:nth-child(even) td{background:#f4f8fd}
.num{text-align:right;font-variant-numeric:tabular-nums}
.stars{text-align:center;letter-spacing:.1em}
.stat-label{font-weight:500;color:#354a5e}

/* -- Plots --------------------------------------- */
.plot-section{}
.plot-block{margin-bottom:28px;background:#fff;border:1px solid #d8e3ef;
            border-radius:10px;padding:20px;
            box-shadow:0 1px 5px rgba(0,0,0,.06)}
.plot-block h3{margin-top:0}
.plot-block.full{width:100%}
.plot-row{display:grid;grid-template-columns:1fr 1fr;gap:20px;margin-bottom:28px}
.plot-block.half{}
.report-plot{width:100%;height:auto;border-radius:6px;display:block}

/* -- Footer -------------------------------------- */
footer{text-align:center;color:#8a9ab0;font-size:0.82rem;
       margin-top:60px;padding-top:20px;border-top:1px solid #dde5f0}

/* -- Print --------------------------------------- */
@media print{
  .sidebar{display:none}
  .report-main{margin-left:0;padding:0 20px}
  .report-header{border-radius:0}
  .plot-row{grid-template-columns:1fr 1fr}
  .dist-card,.plot-block{break-inside:avoid}
}'
}


# -- rmarkdown path (PDF output) -----------------------------------------------

#' @keywords internal
.report_rmarkdown <- function(fit, output_file, title, author,
                               boot, top_n, ext) {

  format_str <- if (ext == "pdf") {
    'pdf_document:\n    toc: true\n    number_sections: true'
  } else {
    'html_document:\n    toc: true\n    toc_float: true\n    self_contained: true\n    theme: flatly'
  }

  tmp_rds <- tempfile(fileext = ".rds")
  saveRDS(list(fit = fit, boot = boot, top_n = top_n), tmp_rds)
  on.exit(unlink(tmp_rds), add = TRUE)

  tmp_rmd <- tempfile(fileext = ".Rmd")
  on.exit(unlink(tmp_rmd), add = TRUE)

  writeLines(.build_rmd_content(title, author, format_str, tmp_rds), tmp_rmd)

  rmarkdown::render(input = tmp_rmd, output_file = output_file, quiet = TRUE)
}


#' @keywords internal
.build_rmd_content <- function(title, author, format_str, rds_path) {
  header <- sprintf(
    '---\ntitle: "%s"\nauthor: "%s"\ndate: "`r Sys.Date()`"\noutput:\n  %s\n---\n',
    title, author, format_str
  )
  body <- sprintf(
'```{r setup, include=FALSE}
knitr::opts_chunk$set(echo=FALSE,message=FALSE,warning=FALSE,
                      fig.width=10,fig.height=6)
env   <- readRDS("%s")
fit   <- env$fit
boot  <- env$boot
top_n <- env$top_n
entry <- FitVerse::get_dist_entry(fit$best_fit$dist_key)
ds    <- fit$data_summary
```

## Data Summary

```{r data-summary}
df_ds <- data.frame(
  Statistic = c("n","Missing","Mean","Std Dev","Median","Skewness","Min","Max"),
  Value     = c(ds$n, ds$n_missing, round(ds$mean,4), round(ds$sd,4),
                round(ds$median,4), round(ds$skewness,4),
                round(ds$min,4),   round(ds$max,4))
)
knitr::kable(df_ds)
```

## Best-Fit Distribution

**%s** | Method: %s

```{r params-table}
knitr::kable(data.frame(Parameter=names(fit$best_fit$params),
                         Estimate=round(fit$best_fit$params,6)),
             row.names=FALSE)
```

## Diagnostic Plot

```{r diagnostic-plot, fig.height=7}
plot(fit, top_n=top_n, interactive=FALSE)
```

## Model Comparison

```{r ranking-table}
tbl  <- fit$ranking
cols <- intersect(c("Rank","Distribution","Method","AIC","Delta_AIC",
                    "BIC","KS_p","AD_p","ChiSq_p","Converged"), names(tbl))
knitr::kable(tbl[,cols], row.names=FALSE)
```

## GoF Tests (Best Fit)

```{r gof-table}
if (!is.null(fit$best_fit$gof))
  knitr::kable(fit$best_fit$gof, row.names=FALSE)
```

---
*Generated by the **FitVerse** R package.*
', rds_path,
    'entry$display_name', 'fit$best_fit$method'
  )
  paste0(header, "\n", body)
}
