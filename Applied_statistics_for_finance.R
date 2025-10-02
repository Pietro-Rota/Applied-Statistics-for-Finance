#* AUTO-GENERATED STANDALONE R SCRIPT ----
#* Generated from R Markdown file using extract_r_code()
#* Source file: Applied_statistics_for_finance.qmd
#* Generated on: 2025-09-26 10:37:35.285598

#* REQUIRED PACKAGES ----
#? If you don't have these packages, run: install.packages(c("broom", "e1071", "foreach", "ggcorrplot", "gt", "knitr", "lubridate", "PerformanceAnalytics", "quantmod", "rugarch", "Sim", "tidyr", "tseries", "viridis", "yuima", "dplyr", "EnvStats", "forecast", "ggplot2", "gtExtras", "library("stats4")", "magrittr", "plotly", "reshape2", "Runuran", "Sim.DiffProc", "timeDate", "TTR", "viridisLite", "zoo", "DT", "fBasics", "GeneralizedHyperbolic", "ghyp", "hrbrthemes", "library("viridis")", "moments", "purrr", "reticulate", "sde", "stats4", "timeSeries", "VarianceGamma", "xts", " "))
# Load required packages
library(broom)
library(e1071)
library(foreach)
library(ggcorrplot)
library(gt)
library(knitr)
library(lubridate)
library(PerformanceAnalytics)
library(quantmod)
library(rugarch)
library(Sim)
library(tidyr)
library(tseries)
library(viridis)
library(yuima)
library(dplyr)
library(EnvStats)
library(forecast)
library(ggplot2)
library(gtExtras)
library(library("stats4"))
library(magrittr)
library(plotly)
library(reshape2)
library(Runuran)
library(Sim.DiffProc)
library(timeDate)
library(TTR)
library(viridisLite)
library(zoo)
library(DT)
library(fBasics)
library(GeneralizedHyperbolic)
library(ghyp)
library(hrbrthemes)
library(library("viridis"))
library(moments)
library(purrr)
library(reticulate)
library(sde)
library(stats4)
library(timeSeries)
library(VarianceGamma)
library(xts)

#* CUSTOM FUNCTIONS ----
# Custom functions from main file
black_scholes <- function (S = 100, K = 100, Time = 0.5, r = 0.05, sigma = 0.2, q = 0, type = "call", rounding = Inf) 
{
    d1 <- (log(S/K) + (r - q + sigma^2/2) * Time)/(sigma * sqrt(Time))
    d2 <- d1 - sigma * sqrt(Time)
    price <- switch(type, call = S * exp(-q * Time) * pnorm(d1) - K * exp(-r * Time) * pnorm(d2), put = K * exp(-r * Time) * pnorm(-d2) - S * exp(-q * Time) * pnorm(-d1))
    price <- ifelse(test = is.null(price), yes = "ERROR", no = round(price, rounding))
    return(price)
}

bprint <- function (obj) 
{
    for (var in names(obj)) {
        cat(var, "=", as.numeric(obj[var]), "\n")
    }
}

check_acc <- function (data1, data2, n = 10, title = NULL, visual = c(TRUE, FALSE, "both")) 
{
    stopifnot(length(data1) == length(data2), is.numeric(data1), is.numeric(data2))
    accuracy <- data.frame()
    for (i in 0:n) {
        new <- mean(round(data1, i) == round(data2, i), na.rm = TRUE)
        accuracy <- rbind(accuracy, new)
    }
    colnames(accuracy) <- "values"
    cor_val <- cor(data1, data2, use = "complete.obs")
    mae_val <- mean(abs(data1 - data2), na.rm = TRUE)
    mse_val <- mean((data1 - data2)^2, na.rm = TRUE)
    diff_data <- data.frame(data1 = data1, data2 = data2) %>% head(5000) %>% mutate(index = index(.), diff = data1 - data2)
    diff_range <- range(diff_data$diff, na.rm = TRUE)
    diff_span <- diff_range[2] - diff_range[1]
    y_limits <- if (diff_span < 1e-06) {
        c(-1e-06, 1e-06)
    }
    else {
        NULL
    }
    diff_plot <- ggplot(diff_data, aes(x = index, y = diff)) + geom_point(color = "darkred", size = 0.7) + labs(subtitle = "Difference", x = NULL, y = NULL) + coord_cartesian(ylim = y_limits)
    accplot <- ggplot(accuracy, aes(x = 0:n, y = values)) + geom_line(linewidth = 1, color = "dodgerblue2") + geom_point(size = 2, color = "dodgerblue3") + scale_x_continuous(breaks = seq(0, n, by = 1)) + ylim(0, 1) + scale_y_continuous(labels = label_percent(), limits = c(0, 1)) + labs(subtitle = "Percentage of accuracy", y = NULL, x = "Rounding decimals")
    plot <- data.frame(data1 = data1, data2 = data2) %>% head(5000) %>% ggplot(aes(x = index(data1))) + geom_point(aes(y = data1, color = "Data1"), size = 2) + geom_point(aes(y = data2, color = "Data2"), size = 2) + scale_color_manual(values = c(Data1 = "purple3", Data2 = "mediumseagreen"), name = NULL) + labs(title = "Visual inspection", y = NULL, x = NULL) + theme(legend.position = "bottom")
    sum_df <- summary(data.frame(data1 = data1, data2 = data2))
    full_df <- data.frame(accuracy[accuracy != 0] * 100) %>% round(2) %>% data.frame() %>% set_colnames("Values !=0 %")
    if (visual[1] == TRUE) {
        return(marrangeGrob(list(plot, diff_plot, accplot), layout_matrix = matrix(c(3, 3, 2, 2, 1, 1, 1, 1), nrow = 4, ncol = 2), top = title))
    }
    else if (visual[1] == FALSE) {
        return(list(distance = data.frame(COR = cor_val, MAE = mae_val, MSE = mse_val), df = full_df, sum_df = sum_df))
    }
    else if (visual[1] == "both") {
        return(list(plots = marrangeGrob(list(plot, diff_plot, accplot), layout_matrix = matrix(c(3, 3, 2, 2, 1, 1, 1, 1), nrow = 4, ncol = 2), top = title), df = full_df, sum_df = sum_df))
    }
}

desc_df <- function (data, quantiles = c(0.01, 0.25, 0.75, 0.99), digits = 4) 
{
    summary_stats <- function(x) {
        n <- sum(!is.na(x))
        mean <- mean(x, na.rm = TRUE)
        sd <- sd(x, na.rm = TRUE)
        median <- median(x, na.rm = TRUE)
        trimmed <- mean(x, trim = 0.1, na.rm = TRUE)
        min <- min(x, na.rm = TRUE)
        max <- max(x, na.rm = TRUE)
        range <- max - min
        skew <- sum((x - mean)^3, na.rm = TRUE)/(n * sd^3)
        kurtosis <- sum((x - mean)^4, na.rm = TRUE)/(n * sd^4) - 3
        se <- sd/sqrt(n)
        percent_missing <- sum(is.na(x))/length(x) * 100
        quantiles_values <- quantile(x, probs = quantiles, na.rm = TRUE)
        c(n = n, mean = mean, sd = sd, median = median, trimmed = trimmed, min = min, max = max, range = range, skew = skew, kurtosis = kurtosis, se = se, `%NA` = percent_missing, Q = quantiles_values[1], Q = quantiles_values[2], Q = quantiles_values[3], Q = quantiles_values[4])
    }
    stats <- sapply(data, function(col) {
        if (is.numeric(col)) 
            summary_stats(col)
        else rep(NA, length(summary_stats(0)))
    })
    as.data.frame(round(t(stats), digits = digits))
}

extract_r_code <- function (input_file, output_file, include_main = TRUE, source_path = "C:/Users/pietr/OneDrive/Desktop/formula.main.R") 
{
    lines <- readLines(input_file)
    in_chunk <- FALSE
    code_lines <- c()
    in_self_function <- FALSE
    self_function_start <- "^extract_r_code_complex\\s*<-\\s*function\\b"
    if (include_main) {
        code_lines <- c(code_lines, "#* AUTO-GENERATED STANDALONE R SCRIPT ----", "#* Generated from R Markdown file using extract_r_code()", paste0("#* Source file: ", basename(input_file)), paste0("#* Generated on: ", Sys.time()), "")
        tryCatch({
            required_pkgs <- required_packages(input_file)
            required_pkgs <- required_pkgs[!is.na(required_pkgs) & nzchar(trimws(required_pkgs)) & trimws(required_pkgs) != ""]
            if (length(required_pkgs) > 0) {
                code_lines <- c(code_lines, "#* REQUIRED PACKAGES ----", paste0("#? If you don't have these packages, run: install.packages(c(\"", paste(required_pkgs, collapse = "\", \""), "\"))"), "# Load required packages")
                for (pkg in required_pkgs) {
                  if (!is.na(pkg) && nzchar(trimws(pkg)) && trimws(pkg) != "") {
                    code_lines <- c(code_lines, paste0("library(", trimws(pkg), ")"))
                  }
                }
                code_lines <- c(code_lines, "")
            }
        }, error = function(e) {
            code_lines <<- c(code_lines, "# Warning: Could not automatically detect required packages", "# Please manually add library() calls as needed", "")
        })
        tryCatch({
            if (!exists("functions_loaded", mode = "function")) {
                stop("functions_loaded() function not found. Please source your main R file first.")
            }
            used_functions <- functions_loaded(input_file, dataframe = FALSE)
            cat("DEBUG: functions_loaded returned:", class(used_functions), "\n")
            cat("DEBUG: functions_loaded returned:", class(used_functions), "\n")
            if (is.null(used_functions)) {
                cat("DEBUG: No custom functions detected in the file\n")
            }
            else if (length(used_functions) > 0) {
                cat("DEBUG: Found", length(used_functions), "custom functions:", paste(used_functions, collapse = ", "), "\n")
                code_lines <- c(code_lines, "#* CUSTOM FUNCTIONS ----", "# Custom functions from main file")
                if (!file.exists(source_path)) {
                  stop("Main R file not found at: ", source_path)
                }
                mainEnv <- new.env()
                cat("DEBUG: Sourcing main file...\n")
                source(source_path, local = mainEnv)
                all_main_functions <- ls(envir = mainEnv)
                all_main_functions <- all_main_functions[sapply(all_main_functions, function(x) is.function(get(x, envir = mainEnv)))]
                cat("DEBUG: Functions available in main file:", paste(all_main_functions, collapse = ", "), "\n")
                functions_added <- 0
                for (func_name in used_functions) {
                  cat("DEBUG: Processing function:", func_name, "\n")
                  if (exists(func_name, envir = mainEnv)) {
                    func_obj <- get(func_name, envir = mainEnv)
                    if (!is.function(func_obj)) {
                      cat("DEBUG: Warning -", func_name, "is not a function\n")
                      next
                    }
                    func_text <- deparse(func_obj, width.cutoff = 500)
                    code_lines <- c(code_lines, paste0(func_name, " <- ", paste(func_text, collapse = "\n")), "")
                    functions_added <- functions_added + 1
                    cat("DEBUG: Successfully added function:", func_name, "\n")
                  }
                  else {
                    cat("DEBUG: Warning - function", func_name, "not found in main environment\n")
                  }
                }
                cat("DEBUG: Total functions added:", functions_added, "\n")
            }
        }, error = function(e) {
            cat("ERROR in custom function extraction:", e$message, "\n")
            code_lines <<- c(code_lines, paste0("# Warning: Could not automatically extract custom functions"), paste0("# Error: ", e$message), "# Please manually add function definitions as needed", "")
        })
        code_lines <- c(code_lines, "#* MAIN CODE ----", "")
    }
    for (line in lines) {
        if (grepl(self_function_start, line)) {
            in_self_function <- TRUE
        }
        if (in_self_function && grepl("^\\s*}\\s*$", line)) {
            in_self_function <- FALSE
            next
        }
        if (in_self_function) {
            next
        }
        if (include_main && (grepl("^\\s*(library|require)\\s*\\(", line) || grepl("^\\s*source\\s*\\(", line) || grepl("extract_r_code.*\\(", line))) {
            next
        }
        if (!in_chunk && grepl("^#{1,6} ", line)) {
            heading <- sub("^#+\\s+", "", line)
            code_lines <- c(code_lines, "", paste0("#* ", heading, " ----"))
        }
        else if (grepl("^```\\{r", line)) {
            in_chunk <- TRUE
            chunk_label <- sub("^```\\{r\\s*([^,}]*)?.*", "\\1", line)
            chunk_label <- trimws(chunk_label)
            label_line <- if (nzchar(chunk_label)) {
                paste0("## ", chunk_label, " ----")
            }
            else {
                "## unnamed chunk ----"
            }
            code_lines <- c(code_lines, "", label_line)
        }
        else if (grepl("^```", line) && in_chunk) {
            in_chunk <- FALSE
        }
        else if (in_chunk) {
            if (!grepl("^#\\|", line)) {
                code_lines <- c(code_lines, line)
            }
        }
    }
    writeLines(code_lines, output_file)
    cat("Standalone R script created successfully!\n")
    cat("Output file:", output_file, "\n")
    if (include_main) {
        cat("Dependencies automatically included.\n")
    }
}

get_portfolio <- function (tickers_portfolio, start_date = Sys.Date() - 6540, end_date = Sys.Date(), fun = Ad, clean_names = F, na.locf = T, include_date = F) 
{
    data1 = lapply(tickers_portfolio, FUN = function(x) {
        fun(getSymbols(x, from = start_date, to = end_date, auto.assign = FALSE))
    })
    portfolio = do.call(merge, data1)
    if (na.locf == TRUE) {
        portfolio <- na.locf(portfolio)
    }
    if (clean_names) {
        colnames(portfolio) <- before_dot(portfolio)
    }
    if (include_date) {
        return(data.frame(DATE = index(portfolio), coredata(portfolio)))
    }
    return(portfolio)
}

gg_qq_plot <- function (data, title = NA) 
{
    realtitle <- ifelse(is.na(title), yes = "QQ Plot", no = paste("QQ Plot of", title))
    df <- data.frame(sample = sort(data), theoretical = qnorm(ppoints(length(data))))
    fit <- lm(sample ~ theoretical, data = df)
    ggplot(df, aes(x = theoretical, y = sample)) + geom_hline(yintercept = 0, color = "black", linetype = "dotted", linewidth = 0.2) + geom_vline(xintercept = 0, color = "black", linetype = "dotted", linewidth = 0.2) + geom_point(color = "blue", size = 2) + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], color = "red", linewidth = 1) + labs(title = realtitle, x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_minimal()
}

greeks <- function (S = 100, K = 100, r = 0.05, sigma = 0.2, Time = 1, q = 0, type = "call", rounding = Inf) 
{
    d1 <- (log(S/K) + (r - q + sigma^2/2) * Time)/(sigma * sqrt(Time))
    d2 <- d1 - sigma * sqrt(Time)
    delta <- ifelse(type == "call", exp(-q * Time) * pnorm(d1), exp(-q * Time) * (pnorm(d1) - 1))
    gamma <- exp(-q * Time) * dnorm(d1)/(S * sigma * sqrt(Time))
    theta <- ifelse(type == "call", (-S * dnorm(d1) * sigma * exp(-q * Time)/(2 * sqrt(Time)) - r * K * exp(-r * Time) * pnorm(d2)), (-S * dnorm(d1) * sigma * exp(-q * Time)/(2 * sqrt(Time)) + r * K * exp(-r * Time) * pnorm(-d2)))
    vega <- S * exp(-q * Time) * dnorm(d1) * sqrt(Time)
    rho <- ifelse(type == "call", K * Time * exp(-r * Time) * pnorm(d2), -K * Time * exp(-r * Time) * pnorm(-d2))
    combined <- data.frame(delta = delta, gamma = gamma, theta = theta, vega = vega, rho = rho)
    return(round(combined, rounding))
}

normalize <- function (x, peak = 100) 
{
    MAX <- max(x, na.rm = TRUE)
    df <- (x/MAX) * peak
    return(df)
}

Qdate <- function (day, month, year) 
{
    date_string <- paste(year, month, day, sep = "-")
    date <- as.Date(date_string, format = "%Y-%m-%d")
    return(date)
}

quickplot <- function (data, title = NULL, plot_engine = c("ggplot", "plotly"), xlab = "Date", ylab = "Value", show_legend = TRUE, subtitle = NULL, caption = NULL, linewidth = 0.4, legend_name = "Variable", legend_position = c("right", "left", "bottom", "top"), alpha = 1, type = geom_line, facet_wrap = FALSE, x_size = 1, x_start = 1, x_step = 1, show_x = TRUE) 
{
    plot_data <- data.frame(Date = index(data), data)
    custom_palette <- rep(c("firebrick", "darkblue", "#006400", "gray30", "#457575", "#6100a8", "orange2", "brown", "#483D8B", "#556B2F", "#8B008B", "#5F9EA0", "#6B8E23", "#9932CC"), 1000)
    my_data_long <- pivot_longer(data = plot_data, cols = -Date, names_to = "Variable", values_to = "Value")
    if (class(data)[1] != "xts") {
        my_data_long$Date <- my_data_long$Date/x_size
    }
    if (x_start != 1) {
        my_data_long$Date <- (my_data_long$Date * x_step) + x_start
    }
    plot <- ggplot(my_data_long, aes(x = Date, y = Value, color = Variable)) + type(linewidth = linewidth, alpha = alpha) + labs(title = title, subtitle = subtitle, caption = caption, x = xlab, y = ylab) + scale_color_manual(name = legend_name, values = custom_palette) + theme(legend.position = legend_position[1], plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
    if (!show_legend) {
        plot <- plot + theme(legend.position = "none")
    }
    if (!show_x) {
        plot <- plot + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
    }
    if (facet_wrap) {
        plot <- plot + facet_wrap(~Variable)
        return(plot)
    }
    final_plot <- switch(plot_engine[1], ggplot = plot, plotly = ggplotly(plot) %>% layout(xaxis = list(rangeslider = list(visible = TRUE, thickness = 0.08)), yaxis = list(fixedrange = FALSE), dragmode = "zoom"))
    return(final_plot)
}

rebase <- function (data, start_value = 100, na.fill = 0, round = Inf) 
{
    data %>% ROC() %>% na.fill(fill = 0) %>% apply(., 2, delog) %>% round(digits = round) %>% as.data.frame()
}

show_df <- function (prices, n = 5, rounding = Inf, name_first_col = "DATE") 
{
    price_date <- cbind(index(prices), smart_round(data.frame(prices), rounding))
    colnames(price_date) <- (c(name_first_col, colnames(prices)))
    rownames(price_date) <- (1:length(index(prices)))
    first_rows <- head(price_date, n)
    last_rows <- tail(price_date, n)
    separator <- matrix(NA, nrow = 1, ncol = ncol(price_date))
    colnames(separator) <- (c(name_first_col, colnames(prices)))
    summary_table <- bind_rows(first_rows, as.data.frame(separator), last_rows)
    return(summary_table)
}

#* MAIN CODE ----


## setup ----
# library(quantmod)
# library(ggplot2)
# library(magrittr)
# library(dplyr)
# library(plotly)
# library(sde)
# knitr::opts_chunk$set(error=TRUE)
knitr::opts_chunk$set(warning = FALSE)
Sys.setlocale("LC_TIME", "C") # Set time locale to English
Sys.setlocale("LC_MESSAGES", "C") # Set messages to English
knitr::opts_chunk$set(fig.align = 'center')
knitr::opts_chunk$set(tidy=TRUE)


gghistogram <- function (x, add.normal = FALSE, add.kde = FALSE, add.rug = FALSE, bins, boundary = 0, 
                        fill = "#1f77b4", linewidth = 1, title = NULL, subtitle = NULL) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("ggplot2 is needed for this function to work. Install it via install.packages(\"ggplot2\")", 
            call. = FALSE)
    }
    else {
        if (missing(bins)) {
            bins <- min(500, grDevices::nclass.FD(na.exclude(x)))
        }
        data <- data.frame(x = as.numeric(c(x)))
        binwidth <- (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))/bins
        
        p <- ggplot2::ggplot() +
          ggplot2::geom_histogram(ggplot2::aes(x), data = data, binwidth = binwidth, 
                                  boundary = boundary, fill = fill) +
          ggplot2::labs(title = title, subtitle = subtitle)
          ggplot2::xlab(deparse(substitute(x)))
        if (add.normal || add.kde) {
            xmin <- min(x, na.rm = TRUE)
            xmax <- max(x, na.rm = TRUE)
            if (add.kde) {
                h <- stats::bw.SJ(x)
                xmin <- xmin - 3 * h
                xmax <- xmax + 3 * h
            }
            if (add.normal) {
                xmean <- mean(x, na.rm = TRUE)
                xsd <- sd(x, na.rm = TRUE)
                xmin <- min(xmin, xmean - 3 * xsd)
                xmax <- max(xmax, xmean + 3 * xsd)
            }
            xgrid <- seq(xmin, xmax, length.out = 512)
            if (add.normal) {
                df <- data.frame(x = xgrid, y = length(x) * binwidth * 
                  stats::dnorm(xgrid, xmean, xsd))
                p <- p + ggplot2::geom_line(ggplot2::aes(df$x, df$y), 
                                            col = "#ff8a62", linewidth = linewidth)
            }
            if (add.kde) {
                kde <- stats::density(x, bw = h, from = xgrid[1], 
                  to = xgrid[512], n = 512)
                p <- p + ggplot2::geom_line(ggplot2::aes(x = kde$x, y = length(x) * binwidth * kde$y), 
                                            col = "#67a9ff", linewidth = linewidth) 
            }
        }
        if (add.rug) {
            p <- p + ggplot2::geom_rug(ggplot2::aes(x))
        }
        return(p)
    }
}

bprint <- function(obj) {
  for (var in names(obj)) {
      cat(var, "=", as.numeric(obj[var]), "\n")
  }
} 

## set x to time ----
# if (isrendering()) {
#   # Code that runs only during rendering
#   print(knitr::is_latex_output())
#   print(knitr::is_html_output())
#   library(purrr, quietly = T, warn.conflicts = F)
#   quickplot <- partial(quickplot, xlab = "time")
# }

## session_info ----
session_info <- sessionInfo()
session_info$R.version$version.string
session_info$platform
rm(session_info)

## functions_loaded ----
file <- "C:\\Users\\pietr\\OneDrive\\Documenti\\CATTOLICA ANNO 24-25\\2 SEMESTRE\\(ASF) - Applied Statistics for Finance\\(ASF) - R\\ASF\\Applied_statistics_for_finance.qmd" 
when_rendering(functions_loaded(file))

## required_packages ----
when_rendering(required_packages(file))

## required_functions ----
when_rendering(required_functions(file))
when_rendering(dump_functions(file))

## github file ----

## time info ----
cat("time of creation", "\n")
print(file.info(file)$ctime, "\n")
cat("LAST MODIFICATION", "\n")
print(file.info(file)$mtime, "\n")
cat("Last Access", "\n")
print(file.info(file)$mtime, "\n")
cat("Last Render", "\n")
print(Sys.time(), "\n")

#* Random number generators and Monte Carlo ----

#* Random number generators ----

## Uniform distribution ----
hist(runif(1000), nclass  = 1, xlim = c(0,3))
quickplot(runif(1000), title = "Uniform distributions", show_legend = F)

replicate(10, runif(20)) %>% quickplot(title = "Simulation of different Uniform distributions", linewidth = 0.6, show_legend = F)

set.seed(123)
runif(10)
set.seed(123)
runif(10)

## inverse ----
# inverse transform method
X <- runif(1000)
lambda <- 5
Y <- exp(-lambda*X)
Y_inv <- (-1/lambda)*log(Y)
check_acc(X, Y_inv, n = 20)

## Acc Rej meth ----
dn <- dnorm(-400:400/100)

c <- 1
data.frame(density = dn, x = (1:801) / 801, unif = 1) %>%
  ggplot(aes(x = x)) +
  geom_line(aes(y = density)) +
  geom_ribbon(aes(ymin = 0, ymax = 1), alpha = 0.3, color = "black") +
  xlim(c(0, 1.5)) +
  labs(title = "Acceptance-rejection method", subtitle = paste0("c=", c),
      x = paste0("Result of the function ", round(max(dn/(c*1)), 4)))


max(dn/(c*1))

## Acc Rej 2 ----
c <- 2
dt_stud <- dt(x = -400:400 / 100, df = 1)
data.frame(density = dn, x = (1:801) / 801, tnorm = dt_stud) %>%
  ggplot(aes(x = x)) +
  geom_line(aes(y = density)) +
  geom_ribbon(aes(ymin = 0, ymax = tnorm * c), alpha = 0.3, color = "black") +
  labs(title = "Acceptance-rejection method", subtitle = paste0("c=", c),
       x = paste0("Result of the function ", round(max(dn/(c*dt_stud)), 4)))
max(dn/(c*dt_stud))


c <- seq(0.9, 3, length.out = 12)

aacplot <- function(c) {
  df <- data.frame(density = dn, x = (1:801) / 801, tnorm = dt_stud)

  ggplot(df, aes(x = x)) +
    geom_line(aes(y = density)) +
    geom_ribbon(aes(ymin = 0, ymax = tnorm * c), alpha = 0.3, color = "black") +
    geom_ribbon(aes(ymin = density, 
                    ymax = ifelse(density > tnorm, density, NA)), 
                fill = "red", alpha = 0.5) +
    ggtitle(label = NULL, subtitle = paste(paste0("c=", round(c, 2)), 
                                           "\nresult", round(max(dn / (c * dt_stud)), 4))) +
    theme_void()
}

suppressWarnings(grid.arrange(grobs = lapply(c, aacplot), nrow = 3))

#* The Monte Carlo Method ----

## Montecarlo ----
MNC_Sim <- cbind(Sim.DiffProc::GBM(N = 100, M = 100, x0 = 50, theta = 0.05, sigma = 0.2), 
                Sim.DiffProc::GBM(N = 100, M = 1, x0 = 50, theta = 0.50, sigma = 0.2)*2-50)
K <- 60
MNC_Sim %>% quickplot("Montecarlo simulation of geometric brownian motion", show_legend = F, x_size = 100)+
  geom_hline(aes(yintercept = K), linewidth = 1.2, color = "red")

mean_option <- mean(ifelse(last(MNC_Sim)>K, yes = last(MNC_Sim), 0))

cat("Mean of last datapoints", mean_option, "\n")
cat("Price of option according to MNC", exp(-1 * 1) * mean_option)

## lines ----
replicate(15, cumsum(rnorm(100))) %>%
  quickplot(title = "Montecarlo simulation of rownian motion", subtitle = "No variance reduction", 
            show_legend = F, x_size = 100)

#* Simulation of Stochastic Processes ----

#* Wiener process and basic Brownian motion ----

## wiener process ----
N <- 100                 # number of end-points of the grid including Time
Time <- 1                # length of the interval [0, Time] in time units
Delta <- Time/N          # time increment (dt)
W <- numeric(N+1)        # initialization of the vector W
Time <- seq(0, 1, length=N)

for (i in 2:(N + 1)) {
  W[i] <- W[i - 1] + rnorm(1) * sqrt(Delta)
}

quickplot(W, title = "Simulation of Wiener processes", show_legend = F, x_size = N, linewidth = 1)

## BM ----
set.seed(123)
# brutal code
BM.1 <- function(N=10000){ 
  W <- NULL
  for(i in 2:(N+1))
         W <- c(W, rnorm(1) / sqrt(N))
  return(W)
}
system.time(BM.1())

set.seed(123)
# smarter
BM.2 <- function(N=10000){
  W <- numeric(N)
  Z <- rnorm(N-1)
  for(i in 2:(N))
         W[i] <- W[i-1] + Z[i-1] / sqrt(N)
  return(W)
}
system.time(BM.2())

set.seed(123)
# awesome!
BM.vec <- function(N = 10000) {
  W <- c(0, cumsum(rnorm(N-1) / sqrt(N-1)))
  return(W)
}
system.time(BM.vec())

## der BM ----
dt <- seq(from = 0, to = 0.01, length.out = 100)
a <- 1
quickplot(abs((rnorm(1) * sqrt(a+dt)) - (rnorm(1) * sqrt(a)))/dt, 
          title = "Differential limit in 0 goes to infinity", linewidth = 1, show_legend = F)

#* Geometric Brownian motion ----

## GBM.vec ----
r <- 1
sigma <- 0.5
x <- 10
N <- 100                   # number of grid points including maturity Time
Time <- 1                  # length of the interval [0, Time] in time units
Delta <- Time/N            # time increment (dt)
W <- numeric(N+1)          # initialization of the vector W

Time <- seq(0, Time, length=N+1)

for(i in 2:(N + 1)) {
  W[i] <- W[i - 1] + rnorm(1) * sqrt(Delta)
}

S <- x * exp((r-sigma^2/2)*Time + sigma*W)

quickplot(data.frame(StockPath = S), 
          title = "geometric Brownian motion", x_size = N, linewidth = 1)


GBM.vec <- function(N = 100, x = 10, r = 1, sigma = 0.5, Time = 1) {
  # r = 1
  # sigma = 0.5
  # x = 10
  # N = 100                  # number of grid points including maturity Time
  # Time = 1                 # length of the interval [0, Time] in time units
  
  Delta <- Time/N            # time increment (dt)
  W <- numeric(N+1)          # initialization of the vector W
  
  Time <- seq(0, Time, length=N+1)
  for(i in 2:(N+1))
           W[i] <- W[i-1] + rnorm(1) * sqrt(Delta)
  
  S <- x * exp((r-sigma^2/2)*Time + sigma*W)
  return(S)
}

replicate(10, GBM.vec()) %>% 
  quickplot("Montecarlo simulation of geometric brownian motion", x_size = 100, show_legend = F)

#* Variance reduction techniques ----

## VRT slides ----
n <- 1000
beta <- 1
K <- 1
x <- rnorm(n)
y1 <- sapply(x , function(x) max(0 , K - exp(beta * x)))
y2 <- sapply(-x , function(x) max(0 , K - exp(beta * x)))
mean((y1 + y2) / 2) # MC with antithetic sampling
K * pnorm(log(K) / beta) - exp (beta^2 / 2) * pnorm(log(K) / beta - beta)

## VRT graph ----
# another function with  
GBM.vec_inv <- function(N = 100, x = 10, r = 1, sigma = 0.5, Time = 1) {
  # r = 1
  # sigma = 0.5
  # x = 10
  # N = 100                  # number of grid points including maturity Time
  # Time = 1                 # length of the interval [0, Time] in time units

  Delta <- Time / (N + 1)            # time increment (dt)
  W <- numeric(N)          # initialization of the vector W

  Time <- seq(0, Time, length = N)
  for (i in 2:N)
    W[i] <- W[i - 1] + rnorm(1) * sqrt(Delta)

  S <- x * exp((r - sigma^2 / 2) * Time - sigma * W)
  return(S)
}

N <- 100                # number of grid points including maturity Time
M <- 1000               # number of simulations (min 3)
x <- 10
r <- 1
sigma <- 0.5
Time <- 1

norm <- replicate(M,     GBM.vec(N = N, x = x, r = r, sigma = sigma, Time = Time))
inv  <- replicate(M, GBM.vec_inv(N = N, x = x, r = r, sigma = sigma, Time = Time))

long_norm <- melt(norm[,seq(from = 1, to = min(50,M), by =2)]) %>% 
  mutate(Var1 = Var1/N)
long_inv  <- melt( inv[,seq(from = 1, to = min(50,M), by =2)]) %>% 
    mutate(Var1 = Var1/N)

ggplot() +
  geom_line(data = long_norm, aes(x = Var1, y = value, group = Var2, colour = "Normal"), linewidth = 1) +
  geom_line(data = long_inv,  aes(x = Var1, y = value, group = Var2, colour = "Inverted"), linewidth = 1) +
  scale_color_manual(values = c(Normal = "darkblue", Inverted = "darkred")) +
  labs(title = ifelse(M > 50, paste("50 simulations out of", M, "total"), paste(M, "simulations")),
    subtitle = "Color coded split half normal half inverted", x = "time", y = "Value of GBM")

other_half_no_VRT <- replicate(M/2, GBM.vec(N = N, x = x, r = r, sigma = sigma, Time = Time))[N, ]
sim_no_VRT <- c(other_half_no_VRT, norm[N, ])

df <- data.frame(
  Category = c("BASE NORM", "BASE INV", "VRT", "NO VRT", "BASE OTHER HALF"),
  Variance = c(
    var(norm[N, ]),
    var(inv[N, ]),
    var(c(norm[N, ], inv[N, ])) ,
    var(sim_no_VRT),
    var(other_half_no_VRT)))

ggplot(df, aes(x = Category, y = Variance, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Variance Comparison", x = "Category", y = "Variance") +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(min(df$Variance) - 10, max(df$Variance) + 10))  # Zoom without data loss

#* Other examples of common stochastic processes in their closed formula ----

#* SDE package ----

## SDE ----
n <- 100                        # number of simulation steps.
Delta <- 0.01                   #   time step of the simulation
S0 <- 10                        # fix initial value
mu <- 0.1                       # fix the drift value
sigma <- 0.2                    # and one for the volatility
 
## theta = vector of parameters for cdist
sde.sim(X0=S0, N=n, delta=Delta, model="BS", theta=c(mu, sigma)) %>% 
quickplot(title="Geometric Brownian motion", show_legend = F)


sde.sim(X0=S0, N=n, M = 10, delta=Delta, model="BS", theta=c(mu, sigma)) %>%
  quickplot("Montecarlo simulation of SDE Brownian motion", subtitle = "`M` allows to simulate more than one path", show_legend = F)

#* Markov property ----

#* Martingale ----

#* Discertization / Simulation strategies ----

## testing convergence ----
set.seed(1222)
Time <- 2
N <- 100
dt <- Time/N
X0 <- 40
mu_v <- 0.05
sigma_v <- 0.2
r <- 0.05
K <- 3


# Simulate GBM paths using different methods
gbm_euler <-    sde.sim(X0=X0, N=N, model="BS", theta=c(mu_v, sigma_v), method = "euler")
gbm_milstein <- sde.sim(X0=X0, N=N, model="BS", theta=c(mu_v, sigma_v), method = "milstein")
gbm_exact <-    sde.sim(X0=X0, N=N, model="BS", theta=c(mu_v, sigma_v), method = "EA")

# Plot paths
data.frame(euler = gbm_euler, milstein = gbm_milstein, exact = gbm_exact) %>% quickplot()

# dirty visualization on deliberately few paths to better see the convergence
nsim <- 20
euler_option <-  mean(pmax(0, last(replicate(nsim, sde.sim(X0 = X0, N = 1, T = Time, model = "BS", theta = c(mu_v, sigma_v), method = "euler"))) - K)) * exp(-r * Time)

milstein_option <- mean(pmax(0, last(replicate(nsim, sde.sim(X0 = X0, N = 1, T = Time, model = "BS", theta = c(mu_v, sigma_v), method = "milstein"))) - K)) * exp(-r * Time)

EA_option <- mean(pmax(0, last(replicate(nsim, sde.sim(X0 = X0, N = 1, T = Time, model = "BS", theta = c(mu_v, sigma_v), method = "EA"))) - K)) * exp(-r * Time)

BS_ref_option <- black_scholes(S = X0, K = K, Time = Time, r = r, sigma = sigma_v, type = "call")

data.frame(
  euler = c(euler_option, euler_option - BS_ref_option, abs(euler_option - BS_ref_option)),
  milstein = c(milstein_option, milstein_option - BS_ref_option, abs(milstein_option - BS_ref_option)),
  EA = c(EA_option, EA_option - BS_ref_option, abs(EA_option - BS_ref_option)),
  BS = c(BS_ref_option, NA, NA)
  ) %>% round(4) %>% set_rownames(c("Price", "Diff with BS", "Abs diff")) %>% 
  gt(rownames_to_stub = T) %>%
  data_color(direction = "row", rows = 3, na_color = "white", colors = c("green", "gainsboro", "red"), alpha = 0.2, autocolor_text = F) %>%
  tab_header("Price and difference with the BS function",subtitle = paste0("Deliberately few paths (", nsim, ") to better see the convergence"))

## OU BY HAND ----
d <- expression(-5 * x)
s <- expression(3.5)

X <- sde.sim(X0=10,drift =d,sigma =s)

quickplot(sde.sim(X0 = 10, drift = d, sigma = s, M = 100, ), title = "OU process user defined", show_legend = F)

## CIR BY HAND ----
d <- expression(6 - 3 * x)
s <- expression(2 * sqrt(x))
X <- sde.sim(X0 = 10, drift = d, sigma = s)
quickplot(X, title = "User-defined CIR")



set.seed(123)
X2 <- sde.sim(X0 =10, theta =c(6 ,3,2) , model = "CIR", rcdist =rcCIR, method ="cdist", M = 1000)
set.seed(123)
X_many <- sde.sim(X0 =10, drift = d, sigma = s, M = 1000)


X2_mean <- apply(X2, 1, mean)
X_many_mean <- apply(X_many, 1, mean)
check_acc(X2_mean, X_many_mean, title = "hand-made CIR and sde CIR across 1000 simulations")


grid.arrange(ncol=2, top = "100 simulations of both models", 
            X2[, 1:100] %>% 
              quickplot(title = 'Using model = "CIR"', show_legend = F),
            X_many[, 1:100] %>%  
              quickplot(title = "Defining expressions",show_legend = F))


#* Time series analysis ----

## quantmod get ----
Sq <- getSymbols("AAPL", from = "2015-02-17", to = "2025-02-17", auto.assign = F)

show_df(Sq, rounding = 2) %>% gt() %>% 
  sub_missing(columns = everything(), missing_text = "⋮") %>% 
  tab_header(title = "Apple dataset") %>%
  opt_stylize(style = 5, add_row_striping = TRUE)

paste("This data was downloadded from", attr(Sq, "src"))

quickplot(Cl(Sq), title = "AAPL close price")

## tseries get ----
S <- get.hist.quote("AAPL", start = "2010-01-01", end = "2022-01-01")
chartSeries(S, TA=c(addVo(), addBBands()), theme="white")
S <- S$Close

#* Log-returns ----

## logret ----
X <- diff(log(S))
plot(X)
X <- na.omit(X)

#* Change-point analysis ----

## sde cpoint ----
cp <- cpoint(S)

bprint(cp)

addVLine = function(dtlist) plot(addTA(xts(rep(TRUE,NROW(dtlist)),dtlist),on=1,col="red"))
addVLine(cpoint(S)$tau0)

## Log-returns with change-point line
S <- as.numeric(S)
n <- length(S)
X <- log(S[-1]/S[-n])

plot(X, type = "l", main = "Apple stock log-returns")
abline(v = cpoint(X)$tau0, col = "red")

#* Parameter estimation ----

#* MLE maximum likelihood estimation ----

## MLE ----
cbind(dnorm(-1000:1000/100, mean = 4, sd = 3) ,
      dnorm(-1000:1000/100, mean = 2, sd = 3)) %>% quickplot() +
  geom_vline(aes(xintercept = 500))+
  labs(title = "MLE", subtitle = "vertical line is the unkonwn mean of my data, values increase as mu decreases")

cbind(dnorm(-400:400/100, mean = 1, sd = 1)) %>% quickplot(xlab = NULL) +
  geom_hline(aes(yintercept = 0.1))+
  geom_hline(aes(yintercept = 0.2))+
  geom_hline(aes(yintercept = 0.3))+
  geom_hline(aes(yintercept = 0.5), color = "red")+
  geom_hline(aes(yintercept = max(dnorm(-400:400/100, mean = 1, sd = 1))), color = "green")+
  labs(title = "Iterative process of trying to find the highest value",
      subtitle = "the likelihood keeps going until it overshoots and then comes back down")

## unnamed chunk ----
c(SMA(rnorm(801, sd = 20)/100, n = 50) + dnorm(-400:400/100, mean = 0, sd = 1)) %>% 
  quickplot(title = "Messy distribution with many obvious local maxima", x_start = -4, x_size = 101, show_legend = F)

## MLE estimation ----
# using the mle function
n <- 1000
xnorm <- rnorm(n, mean = 6, sd = 2)


log.lik <- function(mu = 2, sigma = 2) { # Choose good starting values
  -sum(dnorm(xnorm, mean = mu, sd = sigma, log = TRUE))
} 

fit <- mle(log.lik, lower = c(0, 0) , upper = c(Inf, Inf),  method = "L-BFGS-B")

summary(fit)

logLik(fit)

confint(fit)

check_acc(data1 = sort(xnorm), 
          data2 = sort(rnorm(n, mean = coef(fit)[1], coef(fit)[2])),
          title = "Sorted values to confront distributional assumptions")

data.frame(LR_sort = xnorm,
           norm_MLE = rnorm(n, mean = coef(fit)[1], coef(fit)[2])) %>%
  ggplot() +
  geom_histogram(aes(x = LR_sort, fill = "Norm"), alpha = 0.8, bins = 80) +
  geom_histogram(aes(x = norm_MLE, fill = "norm_MLE"), alpha = 0.8, bins = 80) +
  labs(title = "Distribution charts of both the real and the simulated data", x = NULL) +
  scale_fill_manual(name = NULL, values = c(Norm = "indianred3", norm_MLE = "olivedrab2")) +
  theme(legend.position = "bottom")

## Using stock data ----
# Delta <- 0.01         #FROM BEFORE          # time step of the simulation

x <- na.omit(diff(log(S)))
LR_S <- x

est.log.lik <- function(mu_v = 1, sigma_v = 1) {
  -sum(dnorm(x, 
            mean = (mu_v-0.5*sigma_v^2), 
            sd = sigma_v, 
            log = TRUE))
}

fit <- mle(est.log.lik, method = "L-BFGS-B", lower = c(0, 1e-6), upper = c(1, 2))

summary(fit)

logLik(fit)

confint(fit)

check_acc(data1 = sort(as.numeric(x)), 
          data2 = sort(rnorm(length(x), mean = coef(fit)[1], coef(fit)[2])), 
          title = "Sorted values to confront distributional assumptions")

  
  data.frame(LR_sort = as.numeric(x),
          MLE = rnorm(length(x), mean = coef(fit)[1], sd = coef(fit)[2])) %>%
  ggplot() +
  geom_histogram(aes(x = LR_sort, fill = "Log returns"), alpha = 0.8, bins = 80) +
  geom_histogram(aes(x = MLE, fill = "MLE"), alpha = 0.8, bins = 80) +
  labs(title = "Distribution charts of our real data and the one that we", x = NULL) +
  scale_fill_manual(name = NULL, values = c("Log returns" = "indianred3", MLE = "lightblue")) +
  theme(legend.position = "bottom")

## unnamed chunk ----
log_returns <- x
mu_prime <- mean(log_returns)
sigma_sq <- mean((log_returns - mu_prime)^2)  # MLE variance (divided by n)
sigma <- sqrt(sigma_sq)
mu <- mu_prime + 0.5 * sigma_sq  # MLE drift parameter

cat("Closed-form MLE Estimates:\n",
    "mu =", mu, "\n",
    "sigma =", sigma, "\n")
NLL <- function(mu, sigma) {
  mean <- mu - sigma^2 / 2  # Mean of log returns under GBM
  -sum(dnorm(log_returns, mean = mean, sd = sigma, log = TRUE))
}
# Initial parameters from closed-form estimates
start_params <- list(mu = 0.0001, sigma = 0.00001)

# Perform MLE with lower bound for sigma to ensure positivity
gbm_fit <- mle(minuslogl = NLL, start = start_params, 
               method = "L-BFGS-B", lower = c(-Inf, 1e-5))

# Display results
summary(gbm_fit)
mu_est <- coef(gbm_fit)["mu"]
sigma_est <- coef(gbm_fit)["sigma"]
mean_fit <- mu_est - sigma_est^2 / 2  # Fitted mean of log returns

hist(log_returns, breaks = 50, probability = TRUE,
     main = "Log Returns vs Fitted Normal Density",
     xlab = "Log Returns", col = "lightblue")
curve(dnorm(x, mean = mean_fit, sd = sigma_est), 
      col = "red", lwd = 2, add = TRUE)
legend("topright", legend = "Fitted Density", col = "red", lwd = 2)
qqnorm(log_returns, main = "Q-Q Plot of Log Returns")
qqline(log_returns, distribution = function(p) qnorm(p, mean = mean_fit, sd = sigma_est),
       col = "red", lwd = 2)

# Kolmogorov-Smirnov Test
ks.test(log_returns, "pnorm", mean = mean_fit, sd = sigma_est)

#* Quasi-MLE quasi-maximum likelihood estimation ----

## unnamed chunk ----
theta1 <- 0.3
theta2 <- 0.1

ymodel <- setModel (drift = c("(-1)*theta2*x"), diffusion = matrix(c("theta1"), 1, 1))
ymodel
n <- 100
ysamp <- setSampling(Terminal = (n) ^(1/3), n = n)
yuima <- setYuima(model = ymodel, sampling = ysamp)
set.seed(123)
yuima <- simulate(yuima, xinit = 1, true.parameter = list(theta1 = theta1, theta2 = theta2 ))
yuima


mle1 <- qmle(yuima, 
            start = list(theta1 = 0.8, theta2 = 0.7 ), 
            lower = list(theta1 =0.05, theta2 =0.05),
            upper = list (theta1 =0.5, theta2 =0.5), 
            method = "L-BFGS-B")

coef(mle1)
summary(mle1)

## QMLE ----
ymodel <- setModel(drift ="mu*x", diffusion = "sigma*x")
yuima <- setYuima(model = ymodel, data=setData(S), 
                  sampling=setSampling(delta=Delta,n=length(S)))

qmle <- qmle(yuima,
  start = list(mu = 0.05, sigma = 0.125),
  lower = list(mu = 0.025, sigma = 0.05), 
  upper = list(mu = 0.7, sigma = 0.5),
  method = "L-BFGS-B"
)

coef(qmle)

summary(qmle)

logLik(qmle)

check_acc(data1 = sort(as.numeric(LR_S)), 
          data2 = sort(rnorm(length(LR_S), mean = coef(qmle)[1], sd = coef(qmle)[2])), 
          title = "Sorted values to confront distributional assumptions")

data.frame(LR_sort = as.numeric(x),
           QMLE = rnorm(length(x), mean = coef(qmle)[1], coef(qmle)[2])
           ) %>%
  ggplot() +
  geom_histogram(aes(x = LR_sort, fill = "Log returns"), alpha = 0.8, bins = 80) +
  geom_histogram(aes(x = QMLE, fill = "QMLE"), alpha = 0.8, bins = 80) +
  labs(title = "Distribution charts of our real data and the one that we", x = NULL) +
  scale_fill_manual(name = NULL, values = c("Log returns" = "indianred3", QMLE = "palegreen")) +
  theme(legend.position = "bottom")

## Density plot and QQ-plot ----

S <- Cl(getSymbols("AAPL", from = "2020-01-24", to = "2022-01-01", auto.assign = F))
X <- na.omit(diff(log(S)))

gghistogram(X, add.normal = T)
plot(density(X), lwd=2, main="Apple stock Density Plot")
f <- function(u) dnorm(u, mean=mean(X), sd=sd(X))
curve( f, -0.1, 0.1, add=TRUE, col="red",lwd=2)


gg_qq_plot(as.numeric(X), title = "APPLE Stock")

qqnorm(X, main = "Apple stock QQ plot")
qqline(X, col="red",lwd=2)

#* Method of moments ----

## MoM ----
# Mean, Variance:
x <- mean(as.numeric(LR_S))
y <- var(as.numeric(LR_S))
data.frame(EX = x, VarX = y)

# finding the Gamma distribution parameters via Method of Moments
alpha <- x^2 / y
beta <- x / y
data.frame(alpha, beta)

# Estimation of the historical mean and volatility of the GBM via Method of Moments
Delta_v <- 1 / 252
alpha.hat <- mean(LR_S, na.rm = TRUE)
sigma.hat <- sqrt(var(LR_S, na.rm = TRUE))
mu.hat <- alpha.hat + 0.5 * sigma.hat^2

data.frame(
  sigma.hat = as.numeric(sigma.hat),
  mu.hat = as.numeric(mu.hat)
)

check_acc(data1 = sort(as.numeric(LR_S)), 
          data2 = sort(rnorm(length(LR_S), mean = mu.hat, sd = sigma.hat)), 
          title = "Sorted values to confront distributional assumptions")

data.frame(LR_sort = sort(as.numeric(LR_S)),
           MLE = sort(rnorm(length(LR_S), mean = mu.hat, sigma.hat))) %>%
  ggplot() +
  geom_histogram(aes(x = LR_sort, fill = "Log returns"), alpha = 0.8, bins = 80) +
  geom_histogram(aes(x = MLE, fill = "MOM"), alpha = 0.8, bins = 80) +
  labs(title = "Distribution charts of our real data and MoM", x = NULL) +
  scale_fill_manual(name = NULL, values = c("Log returns" = "indianred3", MOM = "#fea966")) +
  theme(legend.position = "bottom")

#* European options ----

#* Scheme to option pricing ----

#* Risk neutralization ----

#* Risk neutralization methods ----

## IDK_MCM ----
# Parameters
S0 <- 100      # Initial stock price
mu <- 0.05     # Drift
sigma <- 0.2   # Volatility
Time <- 1         # Time horizon (years)
n <- 252       # Number of time steps
M <- 100      # Number of simulated paths
r <- 0.03      # Risk-free rate

# Simulate paths using sde.sim
S_paths <- sde.sim(X0 = S0, model = "BS", theta =c(mu, sigma), T = Time, N = n, M = M)

# Mean of exponentiated paths at each time step
E_S_N1 <- apply(S_paths, 1, mean) 

# Apply Mean-Correcting Martingale (MCM) formula
S_RN_MCM <- S0 * S_paths * exp(r * (1:N) * (Time/N)) / E_S_N1

grid.arrange(top = paste("Simulated Stock Price Paths before and after MCM\n Showing", min(30, M), "simulations"), ncol =2, 
quickplot(S_paths[,1:min(30, M)], show_legend = F, title = "BEFORE"),
quickplot(S_RN_MCM[,1:min(30, M)], show_legend = F, title = " AFTER", ylab = NULL))


# Plot the original GBM paths vs. the Mean-Correcting Martingale
ggplot() +
  geom_line(data = melt(S_paths[,1:min(30, M)]), aes(x = Var1, y = value, color = "GBM Paths", group = Var2),
            linewidth = 1) +
  geom_line(data = melt(S_RN_MCM[,1:min(30, M)]), aes(x = Var1, y = value, color = "MCM Paths", group = Var2),
            linewidth = 1) +
  labs(title = "Stock Price Paths vs. Mean-Correcting Martingale Adjusted Paths", 
       subtitle = paste("Showing", min(30, M), "simulations"),
       x = "Time (Days)", y = "Stock Price") +
  scale_color_manual(values = c("#131cce", "red")) +
  theme(legend.title = element_blank(), legend.position = "bottom")


grid.arrange(top = "Boxplot to compare how the distribution changes across time steps", ncol=2,
melt(data.frame(t(as.data.frame(S_paths)))) %>%
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot(fill = "#131cce", color = "navyblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Before", x = NULL, y = NULL)+
    theme(
    axis.text.x = element_blank(),
    panel.grid.minor.x = element_blank(),  # Remove minor x-axis grid lines
    panel.grid.major.x = element_blank())  # Keep major x-axis grid lines
,

melt(data.frame(t(as.data.frame(S_RN_MCM)))) %>%
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot(fill = "red", color = "darkred") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "After", x = NULL, y = NULL) +
  theme(
    axis.text.x = element_blank(),
    panel.grid.minor.x = element_blank(),  # Remove minor x-axis grid lines
    panel.grid.major.x = element_blank()  # Keep major x-axis grid lines
  )
)

## deepseek attempt at MCM ----
# Set seed for reproducibility
set.seed(42)

# Parameters
S0 <- 100       # Initial stock price
r <- 0.05       # Risk-free rate
T <- 1          # Total time (1 year)
N_steps <- 20  # Number of time steps (daily)
N_paths <- 10 # Number of simulated paths

# Simulate log-prices (Brownian motion)
log_S_paths <- matrix(rnorm(N_paths * N_steps, mean = 0, sd = 0.01), nrow = N_paths, ncol = N_steps)
log_S_paths <- t(apply(log_S_paths, 1, cumsum))  # Cumulative sum (log-price)

# Convert to actual prices
S_paths <- S0 * exp(log_S_paths)

# Time grid
dt <- T / N_steps
t_grid <- seq(dt, T, by = dt)

# Martingale correction function
mean_correcting_martingale <- function(S_paths, S0, r, t_grid) {
  N_paths <- nrow(S_paths)
  N_steps <- ncol(S_paths)
  S_RN <- matrix(0, nrow = N_paths, ncol = N_steps)
  
  for (t in 1:N_steps) {
    # Expected value of S_{t+1} (if t < N_steps)
    if (t < N_steps) {
      E_S <- mean(S_paths[, t + 1])
    } else {
      # If at last step, approximate E[S_{t+1}] as S_t * exp(r * dt)
      E_S <- mean(S_paths[, t]) * exp(r * dt)
    }
    
    # Apply martingale correction
    S_RN[, t] <- S0 * S_paths[, t] * exp(r * t_grid[t]) / E_S
  }
  
  return(S_RN)
}

# Apply correction
S_RN <- mean_correcting_martingale(S_paths, S0, r, t_grid)

# Verify martingale property: E[S_RN[, t]] should ≈ S0 * exp(r * t)
# for (t in 1:N_steps) {
#   cat(
#     "t =", round(t_grid[t], 3), 
#     "| E[S_RN] =", round(mean(S_RN[, t]), 2), 
#     "| S0 * exp(r*t) =", round(S0 * exp(r * t_grid[t]), 2), 
#     "\n"
#   )
# }

#* B&S pricing ----

## BS pricing ----
# CALL formula
call.price <- function(x = 1, Time = 0, T_mat = 1, r = 1, sigma = 1, K = 1) {
  d2 <- (log(x / K) + (r - 0.5 * sigma^2) * (T_mat - Time)) / (sigma * sqrt(T_mat - Time))
  d1 <- d2 + sigma * sqrt(T_mat - Time)
  price <- x * pnorm(d1) - K * exp(-r * (T_mat - Time)) * pnorm(d2)
  return(price)
}

# PUT formula
put.price <- function(x = 1, Time = 0, T_mat = 1, r = 1, sigma = 1, K = 1) {
  d2 <- (log(x / K) + (r - 0.5 * sigma^2) * (T_mat - Time)) / (sigma * sqrt(T_mat - Time))
  d1 <- d2 + sigma * sqrt(T_mat - Time)
  price <- K * exp(-r * (T_mat - Time)) * pnorm(-d2) - x * pnorm(-d1)
  return(price)
}

# Example
S0 <- 100
K <- 110
r <- 0.05
Time <- 1/4
sigma <- 0.25
C <- call.price(x = S0, Time = 0, T_mat = Time, r = r, K = K, sigma = sigma)
cat("Call price according to Black and Scholes", C, "\n")

P <- put.price(x = S0, Time = 0, T_mat = Time, r = r, K = K, sigma = sigma)
cat("Put price according to Black and Scholes", P, "\n")


# Put-Call parity
P2 <- C - S0 + K * exp(-r * Time)
cat("Put price according to Put call parity", P2, "\n")

#* Montecarlo Pricing ----

## Monte Carlo pricing ----
MCPrice <- function(x = 1, Time = 0, T_mat = 1, r = 1, sigma = 1, M = 1000, f) {
  h <- function(m) {
    u <- rnorm(m / 2)
    tmp <- c(
      x * exp((r - 0.5 * sigma^2) * (T_mat - Time) + sigma * sqrt(T_mat - Time) * u),
      x * exp((r - 0.5 * sigma^2) * (T_mat - Time) + sigma * sqrt(T_mat - Time) * (-u))
    )

    mean(sapply(tmp, function(xx) f(xx)))
  }
  p <- h(M)
  p * exp(-r * (T_mat - Time))
}
# Example
f <- function(x) max(0, x - K)


M <- 1000
MC1 <- MCPrice(x = S0, Time = 0, T_mat = Time, r = r, sigma, M = M, f = f)
cat("Call price according to Monte Carlo", MC1, "after", M, "simulations, with a diffference of", MC1-C, "\n")


M <- 50000
MC2 <- MCPrice(x = S0, Time = 0, T_mat = Time, r = r, sigma, M = M, f = f)
cat("Call price according to Monte Carlo", MC2, "after", M, "simulations, with a diffference of", MC2-C, "\n")


M <- 1e+06
MC3 <- MCPrice(x = S0, Time = 0, T_mat = Time, r = r, sigma, M = M, f = f)
cat("Call price according to Monte Carlo", MC3, "after", M, "simulations, with a diffference of", MC3-C, "\n")

# Speed of convergence
m <- c(10, 50, 100, 150, 200, 250, 500, 1000)
p1 <- NULL
err <- NULL
nM <- length(m)
repl <- 100
mat <- matrix(, repl, nM)
for (k in 1:nM) {
  tmp <- numeric(repl)
  for (i in 1:repl) {
    tmp[i] <- MCPrice(x = S0, Time = 0, T_mat = Time, r = r, sigma, M = m[k], f = f)
  }
  mat[, k] <- tmp
  p1 <- c(p1, mean(tmp))
  err <- c(err, sd(tmp))
}

colnames(mat) <- m
mat %>% round(5) %>%  datatable()

## show mc ----
p0 <- C
minP <- min(p1 - err)
maxP <- max(p1 + err)
plot(m, p1, type = "n", ylim = c(minP, maxP), axes = F, ylab = "MC price",  xlab = "MC replications")
lines(m, p1 + err, col = "blue")
lines(m, p1 - err, col = "blue")
axis(2, p0, "B&S price")
axis(1, m)
boxplot(mat, add = TRUE, at = m, boxwex = 15, col = "orange",  axes = F)
points(m, p1, col = "blue", lwd = 3, lty = 3)
abline(h = p0, lty = 2, col = "red", lwd = 3)


# Assuming mat is a matrix: rows = replications, columns = sample sizes
# Convert matrix to long format
mat_long <- as.data.frame(mat)
mat_long$rep <- 1:nrow(mat_long)
mat_long <- pivot_longer(mat_long, cols = -rep, names_to = "m", values_to = "MC_price")
mat_long$m <- as.numeric(gsub("V", "", mat_long$m))  # Convert m to numeric if needed

# Create a summary frame for lines and CI bands
summary_df <- data.frame(
  m = m,
  p1 = p1,
  upper = p1 + err,
  lower = p1 - err
)

summary_df %>% round(4) %>% datatable()

# Plot
ggplot(mat_long, aes(x = factor(m), y = MC_price)) +
  geom_boxplot(fill = "orange", outlier.shape = 1, width = 0.5) +
  geom_point(data = summary_df, aes(x = factor(m), y = p1), color = "blue", size = 2) +
  geom_line(data = summary_df, aes(x = factor(m), y = upper, group = 1), color = "blue") +
  geom_line(data = summary_df, aes(x = factor(m), y = lower, group = 1), color = "blue") +
  geom_hline(yintercept = C, linetype = "dashed", color = "red", size = 1) +
  labs(x = "MC replications", y = "MC price")

#* FFT pricing ----

## FFT method ----
FFTcall.price <- function(phi, S0, K, r, Time, alpha = 1, N = 2^12, eta = 0.25) {
  m <- r - log(phi(-(0 + 1i)))
  phi.tilde <- function(u) (phi(u) * exp((0 + 1i) * u * m))^Time
  psi <- function(v) {
    exp(-r * Time) * phi.tilde((v - (alpha + 1) * (0 + 1i))) /
      (alpha^2 + alpha - v^2 + (0 + 1i) * (2 * alpha + 1) * v)
  }
  
  lambda <- (2 * pi) / (N * eta)
  b <- 1 / 2 * N * lambda
  ku <- -b + lambda * (0:(N - 1))
  v <- eta * (0:(N - 1))
  tmp <- exp((0 + 1i) * b * v) * psi(v) * eta * (3 + (-1)^(1:N) - ((1:N) - 1 == 0)) / 3
  ft <- fft(tmp)
  res <- exp(-alpha * ku) * ft / pi
  inter <- spline(ku, Re(res), xout = log(K / S0))
  return(inter$y * S0)
}

phiBS <- function(u) {
  exp((0 + 1i) * u * (mu - 0.5 * sigma^2) - 0.5 * sigma^2 * u^2)
}

mu <- 1

FFT <- FFTcall.price(phiBS, S0 = S0, K = K, r = r, Time = Time)

cat("Call price according to Monte Carlo", FFT, "with a diffference of", FFT-C, "\n")

#* Levy processes ----

## levy process data ----
startLV <- Qdate(9, 10, 2017)
ticker <- "TSLA"
S <- Cl(getSymbols(ticker, from = startLV, to = "2024-02-16", auto.assign = F))
cp <- cpoint(as.numeric(S))

bprint(cp)

ggplot(S, aes(x = index(S), y = S))+
  geom_line()+
  geom_vline(aes(xintercept = startLV+cp$tau0), color ="red")+
  labs(title = paste("Price of", ticker, "with changepoint"), x = "Date", y = "Price")

S <- Cl(getSymbols(ticker, from = startLV + cp$tau0, to = "2024-02-16", auto.assign = F))
l_ret <- na.omit(diff(log(S)))

## levy process ----
# Parameters fitting
vgfit <- vgFit(l_ret, ) # esitmate VG parameters on the sample
summary(vgfit)

bprint(vgfit$param)

## Assign parameters
vg_param <- as.numeric(vgfit$param)
c <- vg_param[1]
sigma <- vg_param[2]
theta <- vg_param[3]
nu <- vg_param[4]

Time <- 1/4  # option maturity = 3 months
N <- 100     # number of steps for each path
r <- 0.01    # arbitrary risk-free rate
nsim <- 100  # number of simulated path

# Variance Gamma function
# Variance Gamma function
VG <- function(sigma, nu, theta, Time, N, r) {
    a <- 1/nu
    b <- 1/nu
    h <- Time/N
    Time <- (0:N) * Time/N
    X <- rep(0, N + 1)
    I <- rep(0, N)
    X[1] <- 0

    for (i in 1:N) {
        I[i] <- rgamma(1, a * h, b)
        X[i + 1] <- X[i] + theta * I[i] + sigma * sqrt(I[i]) * rnorm(1)
    }

    return(X)
}

VG.vec <- function(sigma, nu, theta, Time, N, r) {
    h <- Time/N
    Timef <- seq(0, Time, length.out = N + 1)

    I <- rgamma(N, shape = h/nu, scale = nu)  # Vectorized gamma samples
    W <- rnorm(N, mean = 0, sd = 1)  # Pre-generate normal samples
    X <- c(0, cumsum(theta * I + sigma * sqrt(I) * W))  # Efficient cumsum

    return(X)
}

## Create a matrix to fill with random paths using the function VG just created
VG_paths <- matrix(nrow = nsim, ncol = N + 1)
for (i in 1:nsim) {
    VG_paths[i, ] <- VG(sigma, nu, theta, Time = Time, N, r)
}

t(VG_paths) %>%
    format(scientific = TRUE, digits = 2) %>%
    datatable()

# plot the Monte Carlo Simulation
colori <- viridis(nsim)
plot(VG_paths[1, ], col = 0, type = "l", ylim = c(min(VG_paths), max(VG_paths)),
    main = "Monte Carlo Simulation for VG returns", sub = paste(N, "steps", nsim,
        "paths"), xlab = "Time", ylab = "VG returns")
for (i in 2:nsim) {
    lines(VG_paths[i, ], col = colori[i], lwd = 2)
}


lims <- c(min(VG_paths), max(VG_paths))

grid.arrange(ncol = 2, top = "Monte Carlo Simulation for Variance Gamma returns",
    VG_paths %>%
        t() %>%
        quickplot(subtitle = paste("All", nsim, "simulations"), show_legend = F) +
        ylim(lims), quickplot(apply(VG_paths, 2, mean), subtitle = "Mean across timesteps",
        show_legend = F) + ylim(lims))

#* Testing basic levy ----

## levy process test of fitting ----
## TESTS (both graphical and not) OF DISTRIBUTIONAL ASSUMPTIONS QQplot
l_ret.s <- sort(as.numeric(l_ret))  # sort the log returns

p <- ppoints(length(l_ret.s))  # plotting position

VG.q <- qvg(p, vgC = c, sigma = sigma, theta = theta, nu = nu)  # compute the quantile

plot(x = VG.q, y = l_ret.s, main = "Variance-Gamma Q-Q Plot", xlab = "Theoretical Quantiles",
    ylab = "Sample Quantiles")
abline(0, 1, col = "red")  # Add reference line

ggplot(data.frame(VG.q), aes(sample = as.numeric(VG.q))) + stat_qq(color = "blue") +
    stat_qq_line(color = "black", size = 1) + labs(title = "QQ Plot of VG", x = "Theoretical Quantiles",
    y = "Sample Quantiles") + theme(plot.subtitle = element_text(hjust = 0.5))

# good result, linear

plot_density_comparison <- function(data, title = "Density Comparison", kde_color = "coral2", vg_color = "seagreen3") {
  # Compute Kernel Density Estimate (KDE)
  kde_data <- density(data)
  kde_df <- data.frame(x = kde_data$x, y = kde_data$y)

  # Compute Variance Gamma (VG) density
  x_vals <- seq(min(data), max(data), length.out = 500)
  vg_y_vals <- dvg(x_vals, mean(data), sd(data))
  vg_df <- data.frame(x = x_vals, y = vg_y_vals)

  # Plot using ggplot2
  ggplot() +
    geom_line(data = kde_df, aes(x = x, y = y, color = "Kernel Density"), linewidth = 1) +
    geom_line(data = vg_df, aes(x = x, y = y, color = "Variance Gamma"), linewidth = 1) +
    scale_color_manual(values = c("Kernel Density" = kde_color, "Variance Gamma" = vg_color)) +
    labs(x = "", y = "", title = title, color = "Density Type") +
    theme_minimal() 
}

# Example usage
plot_density_comparison(l_ret, title = "My Custom Density Plot")

## testing levy process ----
# Chi^2 test
test <- chisq.test(l_ret.s, VG.q)
test

paste("With a Chi^2 test: high p-value (0.24)")
ifelse(test$p.value < 0.24,
  paste("We can't reject the null hypotesis as the p-value is", round(test$p.value, 4)),
  paste("We reject the null hypotesis as the p-value is", round(test, 4))
)

# K-S test
test <- ks.test(as.numeric(l_ret), rvg(length(as.numeric(l_ret)), param = c(c, sigma, theta, nu)))
test

paste("With a Kolmogorov-Smirnov test: high p-value (0.10)")
ifelse(test$p.value < 0.10,
  paste("We can't reject the null hypotesis as the p-value is", round(test$p.value, 4)),
  paste("We reject the null hypotesis as the p-value is", round(test$p.value, 4))
)

# Summary statistics through basicStats from fbasics
final_retVG <- VG_paths[, N + 1]

# desc_df(data.frame(final_retVG))
data.frame(t(basicStats(final_retVG)))

gghistogram(final_retVG, bins = 30, title = "Histogram of last time steps", 
            subtitle = paste0("not much disclosing when nsim is small (now = ", nsim,")"))

#* From variance-gamma returns to stock prices ----

## unnamed chunk ----
r <- 0.01
S0 <- as.numeric(tail(S, n=1)) #prezzo inziale
S0

# function for stock price with VG returns
VGexp <- function(sigma, nu, theta, Time, N, r, S0) {
  a <- 1 / nu
  b <- 1 / nu
  h <- Time / N
  Time <- (0:N) * Time / N
  X <- rep(0, N + 1)
  I <- rep(0, N)
  X[1] <- S0
  for (i in 1:N) {
    I[i] <- rgamma(1, a * h, b) # gamma component for the jump
    X[i + 1] <- X[i] * exp(r * Time + theta * I[i] + sigma * sqrt(I[i]) * rnorm(1))
  }
  return(X)
}

VGexp_paths <- matrix(nrow = nsim, ncol = N + 1)
for (i in 1:nsim) {
  VGexp_paths[i, ] <- VGexp(sigma, nu, theta, Time, N, r, S0)
}

VGexp_paths %>% t() %>% round(4) %>%  datatable()

plot(VGexp_paths[1,], col=0, type="l", ylim = c(min(VGexp_paths),max(VGexp_paths)),
     main = "MC Simlation for VG stock prices", sub = "100 steps, 10 paths",
     xlab = "Time", ylab = "S&P 500")
for(i in 2:nsim){
  lines(VGexp_paths[i,], col=colori[i], lwd = 2);

}

lims <- c(min(VGexp_paths), max(VGexp_paths))

grid.arrange(
  ncol = 2, top = "Monte Carlo Simulation for Variance Gamma stock Levy process", 
  quickplot(t(VGexp_paths), title = "", subtitle = paste("All",nsim, "simulations"), show_legend = F)+
    ylim(lims),
  quickplot(apply(VGexp_paths, 2, mean), title = "Mean across timesteps", show_legend = F)+
    ylim(lims)
  )


## Statistics on final prices
final_pricesVG<-VGexp_paths[,N+1]

# Risk neutral transform MCM
rn_final_pricesVG<-S0*final_pricesVG*(exp(r*Time)/mean(final_pricesVG))

# (S0*VGexp_paths*(exp(r*Time)/apply(VGexp_paths,2, mean))) %>% 
#   quickplot("Mean correcting martingale of the whole VG stock paths", show_legend = F)  ## dont think its correct

# taking all the last 5 gridpoints to see how they change in the last time steps
basicStats(data.frame(
  VGexp_paths[, N - 4],
  VGexp_paths[, N - 3],
  VGexp_paths[, N - 2],
  VGexp_paths[, N - 1],
  VGexp_paths[, N],
  VGexp_paths[, N + 1])) %>% 
  set_colnames(c("T-5", "T-4", "T-3", "T-2", "T-1", "T")) %>% 
  round(4) %>%
  mutate(Statistics = rownames(basicStats(1)))%>%   # just to extract the rownames 
  relocate(Statistics, .before = everything())  %>% # move this new column
  gt() %>% 
  data_color(alpha = 0.2, columns = 2:7, rows = everything(), autocolor_text = F, direction = "row", 
             palette = c("red", "white", "green")) %>% 
  opt_stylize(5, "gray", F) %>%
  tab_options(table.font.size = px(11)) %>% 
  tab_header("Variance gamma model last stock stock prices statistics")


grid.arrange(top = "Distribution of last prices", ncol=2,
gghistogram(rn_final_pricesVG, bins = 30)+ labs(subtitle = "After MCM"),
gghistogram(final_pricesVG, bins = 30, fill = "firebrick")+ labs(subtitle = "before MCM"))

##OPTION PRICING
K <- S0 #prova: opzione ATM
payoff_VG <- pmax(rn_final_pricesVG - K, 0)
optprice_VG <- mean(payoff_VG)*exp(-r*Time)
optprice_VG

#* Pricing with FFT under the VG process ----

## VG FFT ----
# VG process
theta <- -0.1436
nu <- 0.3
r <- 0.1
sigma <- 0.12136
Time <- 1/4
K <- 101
S <- 100
alpha <- 1.65

phiVG <- function(u) {
    omega <- (1/nu) * (log(1 - theta * nu - sigma^2 * nu/2))
    tmp <- 1 - (0 + (0+1i)) * theta * nu * u + 0.5 * sigma^2 * u^2 * nu
    tmp <- tmp^(-1/nu)
    exp((0 + (0+1i)) * u * log(S0) + u * (r + omega) * (0 + (0+1i))) * tmp
}

FFTcall.price <- function(phi, S0, K, r, Time, alpha = 1, N = 2^12, eta = 0.25) {
    m <- r - log(phi(-(0 + (0+1i))))
    phi.tilde <- function(u) (phi(u) * exp((0 + (0+1i)) * u * m))^Time
    psi <- function(v) {
        exp(-r * Time) * phi.tilde((v - (alpha + 1) * (0 + (0+1i))))/(alpha^2 + alpha -
            v^2 + (0 + (0+1i)) * (2 * alpha + 1) * v)
    }
    lambda <- (2 * pi)/(N * eta)
    b <- 1/2 * N * lambda
    ku <- -b + lambda * (0:(N - 1))
    v <- eta * (0:(N - 1))
    tmp <- exp((0 + (0+1i)) * b * v) * psi(v) * eta * (3 + (-1)^(1:N) - ((1:N) -
        1 == 0))/3
    ft <- fft(tmp)
    res <- exp(-alpha * ku) * ft/pi
    inter <- spline(ku, Re(res), xout = log(K/S0))
    return(inter$y * S0)
}

FFTcall.price(phiVG, S0 = S0, K = K, r = r, Time = Time)
black_scholes(S = S0, K = S0, Time = Time, r = r, sigma = sigma, type = "call")

#* Meixner model ----

## Meixner model MoM ----
# Moments: mean, variance, skewness, kurtosis
x <- mean(l_ret, na.rm = TRUE)
y <- sd(l_ret, na.rm = TRUE)
z <- as.numeric(skewness(l_ret, na.rm = TRUE))
w <- as.numeric(kurtosis(l_ret, na.rm = TRUE))

# Mom: estimates parameters m, a, b, d as functions of the moments
m <- x - ((z * sqrt(y)) / (w - (z^2) - 3))
a <- sqrt(y * (2 * w - 3 * (z^2) - 6))
b <- 2 * atan(-sqrt((z^2) / (2 * w - 3 * (z^2) - 6)))
d <- 1 / (w - (z^2) - 3)

# risk neutral transformation
# Esscher transform: Meixner(a, a*theta + b, d, m) distribution

# theta <- -1/a * (b + 2 * atan((-cos(a/2)+ exp((m-r)/2*d))/sin(a/2)))
# b <- a*theta+b

# mean correction

# m <- r -2 *d*log(cos(b/2)/cos((a+b)/2))

data.frame(mean=x, sd=y, skew=z, kurt=w)
data.frame(a=a, b=b, d=d, m=m)

## Meixner model ----
# Meixner function
MX <- function(a, b, d, M, N) {
  distr <- udmeixner(a, b, d, m)   # meixner distribution
  gen <- pinvd.new(distr)          # Polynomial interpolation of INVerse CDF
  rdmMXgen <- ur(gen, N)           # randomly draws N objects from gen (from a Meixner distr)
  h <- Time / N
  X <- rep(0, N + 1)
  for (i in 1:N) {
    X[i + 1] <- X[1] + rdmMXgen[i]
  }
  return(X)
}

replicate(10,MX(a, b, d, m, N)) %>% quickplot()

MX_paths <- matrix(nrow = nsim, ncol = N + 1) # fill the matrix with random paths that follow
for (i in 1:nsim) { 
  MX_paths[i, ] <- MX(a, b, d, m, N)
}

MX_paths %>%
  t() %>%
  format(x = , scientific = TRUE, digits = 3) %>%
  datatable()

# plot the Monte Carlo Simulation
# plot(MX_paths[1, ],
#   col = 0, type = "l", ylim = c(min(MX_paths), max(MX_paths)),
#   main = "Monte Carlo Simulation for Meixner returns", sub = "100 steps, 10 paths",
#   xlab = "Time", ylab = "MXNR returns"
# )
# for (i in 2:nsim) {
#   lines(MX_paths[i, ], col = colori[i], lwd = 2)
# }

t(MX_paths)[1:10,] %>% quickplot(show_legend = F)

lims <- c(min(MX_paths), max(MX_paths))

grid.arrange(
  ncol = 2, top = "Monte Carlo Simulation for Meixner model Returns Levy process", 
  quickplot(t(MX_paths), title = "", subtitle = paste("All",nsim, "simulations"), show_legend = F)+
    ylim(lims),
  quickplot(apply(MX_paths, 2, mean), title = "Mean across timesteps", show_legend = F)+
    ylim(lims)
  )

# QQplot
MX.q <- uq(pinvd.new(udmeixner(a, b, d, m)), p) # compute the quantile

plot(MX.q, l_ret.s,
  main = "Meixner Q-Q Plot",
  xlab = "Theoretical Quantiles", ylab = "Sample Quantiles"
)
# good result, linear

# summary statistics
final_retMX <- MX_paths[, N + 1]
basicStats(final_retMX)
gghistogram(final_retMX, bins = 30, title = "Histogram of last time steps", 
            subtitle = paste0("not much disclosing when nsim is small (now = ", nsim,")"))

#* Meixner model stock prices ----

## Meixner model stock prices ----
#FROM MEIXNER RETURNS TO STOCK PRICES

#function for stock price with Meixner returns
MXexp=function(a, b, d, m, N, Time, r, S0) {
  distr <- udmeixner(a, b, d, m)     # meiner distribution
  gen <- pinvd.new(distr)            # Polynomial interpolation of INVerse CDF
  generazioni <- ur(gen,N)           # randomly draws N objects from gen (from a Meixner distr)
  h=Time/N
  Time=(0:N)*Time/N
  X=rep(0,N+1)
  X[1]=S0
  for (i in 1:N){
    X[i+1]=X[1]*exp(r*Time+generazioni[i])
  }
  return(X)
}

replicate(10,MXexp(a, b, d, m, N, Time, r, S0)) %>% quickplot()

MXexp_paths<-matrix(nrow = nsim, ncol=N+1)
for(i in 1:nsim){
  MXexp_paths[i,]<-MXexp(a,b,d,m,100,Time,r,S0) #vengono tutte le linee uguali perché MX non varia!
}

MXexp_paths %>% t() %>% round(2) %>% datatable()

lims <- c(min(MXexp_paths), max(MXexp_paths))

grid.arrange(
  ncol = 2, top = "Monte Carlo Simulation for Meixner exponential stock Levy process", 
  quickplot(t(MXexp_paths), title = "", subtitle = paste("All",nsim, "simulations"), show_legend = F)+
    ylim(lims),
  quickplot(apply(MXexp_paths, 2, mean), title = "Mean across timesteps", show_legend = F)+
    ylim(lims)
  )

#statistics on final prices
final_pricesMX<-MXexp_paths[,N+1]

basicStats(data.frame(
  MXexp_paths[, N - 4],
  MXexp_paths[, N - 3],
  MXexp_paths[, N - 2],
  MXexp_paths[, N - 1],
  MXexp_paths[, N],
  MXexp_paths[, N + 1])) %>% 
  set_colnames(c("T-5", "T-4", "T-3", "T-2", "T-1", "T")) %>% 
  round(4) %>%
  mutate(Statistics = rownames(basicStats(1)))%>%   # just to extract the rownames 
  relocate(Statistics, .before = everything())  %>% # move this new column
  gt() %>% 
  data_color(alpha = 0.2, columns = 2:7, rows = everything(), autocolor_text = F, direction = "row", 
             palette = c("red", "white", "green")) %>% 
  opt_stylize(5, "gray", F) %>%
  tab_options(table.font.size = px(11)) %>% 
  tab_header("Meixner model last stock stock prices statistics")



#risk neutral transform
rn_final_pricesMX<-S0*(final_pricesMX)*(exp(r*Time)/(mean(final_pricesMX)))
rn_final_pricesMX %>% t() %>% datatable()

basicStats(rn_final_pricesMX)
hist(rn_final_pricesMX)

########payoff if you use the Esscher transform or the first mean correcting martingale

#payoff_MX <- pmax(final_pricesMX - K, 0)

#optprice_MX <- mean(payoff_MX)*exp(-r*Time)

#optprice_MX

#########payoff if you mean correct of the simulated paths

payoff_MX <- pmax(rn_final_pricesMX - K, 0)

optprice_MX <- mean(payoff_MX)*exp(-r*Time)

optprice_MX

#* American options ----

## unnamed chunk ----
## least squares method
LSM <- function(n, d, S0, K, sigma, r, Time) {
  s0 <- S0 / K
  dt <- Time / d
  z <- rnorm(n)
  s.Time <-                  s0 * exp((r - 1 / 2 * sigma^2) * Time + sigma * z * (Time^0.5))
  s.Time[(n + 1):(2 * n)] <- s0 * exp((r - 1 / 2 * sigma^2) * Time - sigma * z * (Time^0.5))
  CC <- pmax(1 - s.Time, 0)
  payoffeu <- exp(-r * Time) * (CC[1:n] + CC[(n + 1):(2 * n)]) / 2 * K
  euprice <- mean(payoffeu)
  
  for (k in (d - 1):1) {
    z <- rnorm(n)
    mean <- (log(s0) + k * log(s.Time[1:n])) / (k + 1)
    vol <- (k * dt / (k + 1))^0.5 * z
    s.Time.1 <- exp(mean + sigma * vol)
    mean <- (log(s0) + k * log(s.Time[(n + 1):(2 * n)])) / (k + 1)
    s.Time.1[(n + 1):(2 * n)] <- exp(mean - sigma * vol)
    CE <- pmax(1 - s.Time.1, 0)
    idx <- (1:(2 * n))[CE > 0]
    discountedCC <- CC[idx] * exp(-r * dt)
    
    ## three basis function x^0 x^1 and x^2
    basis1 <- exp(-s.Time.1[idx] / 2)
    basis2 <- basis1 * (1 - s.Time.1[idx])
    basis3 <- basis1 * (1 - 2 * s.Time.1[idx] + (s.Time.1[idx]^2) / 2)
    
    
    p <- lm(discountedCC ~ basis1 + basis2 + basis3)$coefficients
    estimatedCC <- p[1] + p[2] * basis1 + p[3] * basis2 + p[4] * basis3  ## cont region
    EF <- rep(0, 2 * n)
    EF[idx] <- (CE[idx] > estimatedCC)                                   ## compare the payoff with cont 
    CC <- (EF == 0) * CC * exp(-r * dt) + (EF == 1) * CE
    s.Time <- s.Time.1
    
    # print(z)
    # print(mean)
    # print(vol)
    # print(s.Time.1)
    # print(mean)
    # print(s.Time.1)
    # print(CE)
    # print(idx)
    # print(discountedCC)
    # print(basis1)
    # print(basis2)
    # print(basis3)
    # print(p)
    # print(estimatedCC)
    # print(EF)
    # print(EF)
    # print(CC)
    # print(s.Time)
  }

  payoff <- exp(-r * dt) * (CC[1:n] + CC[(n + 1):(2 * n)]) / 2
  usprice <- mean(payoff * K)
  error <- 1.96 * sd(payoff * K) / sqrt(n)
  earlyex <- usprice - euprice
  data.frame(usprice, error, euprice)
}
S0 <- 135
K <- 135
Time <- 1
r <- 0.05
sigma <- 0.4
LSM(1000000, 3, S0, K, sigma, r, Time)

#* Fraone ----

## unnamed chunk ----
use_python("C:/Program Files/Python313/python.exe")

#* Black-Scholes model: Moving beyond the normality of returns ----

## unnamed chunk ----
grid.arrange(
  ncol = 2,
  quickplot(dnorm((-400:400) / 100), title = "Theoretical return distribution", show_legend = F, xlab = NULL, ylab = NULL, x_size = 100, x_start = -4),
  quickplot(dlnorm(seq(0.01, 5, length.out = 500), meanlog = 0, sdlog = 0.7), title = "Theoretical underlying price distribution", show_legend = F, xlab = NULL, ylab = NULL, x_size = 100, x_start = 0)
)

## unnamed chunk ----
x <- dlnorm(seq(0.01, 5, length.out = 500), meanlog = 0, sdlog = 0.7)

quickplot(x, title = "Theoretical underlying price distribution", xlab = NULL, ylab = NULL, x_size = 100, x_start = 0) +
  geom_vline(aes(xintercept = 2)) +
  geom_ribbon(aes(xmin = 0, xmax = 2, color = NULL, fill = "OTM"), alpha = 0.1) +
  geom_ribbon(aes(xmin = 2, xmax = 5, color = NULL, fill = "ITM"), alpha = 0.1) +
  scale_fill_manual("Moneyness", values = c(OTM = "lightblue", ITM = "indianred")) +
  guides(color = "none")

## include=FALSE ----
fraoneshow <- function(ticker, from, to, title = NA) {
  realtitle <- ifelse(is.na(title), yes = paste("Time difference of", to - from, "days, between", from, "and", to), no = title)
  df <- ROC(Cl(getSymbols(ticker, from = from, to = to, auto.assign = F)), type = "cont")

  suppressWarnings(grid.arrange(
    ncol = 2, top = realtitle,
    gghistogram(df, add.rug = F, title = NULL) +
      labs(subtitle = paste("Histogram of logreturns of", ticker)) +
      theme(plot.subtitle = element_text(hjust = 0.5))
    
    ,
    
    ggplot(df, aes(sample = as.numeric(df))) +
      stat_qq(color = "blue") +
      stat_qq_line(color = "black", size = 1) +
      labs(subtitle = paste("QQ Plot of", ticker), x = "Theoretical Quantiles", y = "Sample Quantiles") +
      theme(plot.subtitle = element_text(hjust = 0.5))))
}

#* Stock indeces ----

## unnamed chunk ----
fraoneshow("SPY", from = Qdate(1,1,2018), to = Qdate(1,1,2024))
fraoneshow("QQQ", from = Qdate(1,1,2018), to = Qdate(1,1,2024))
fraoneshow("DIA", from = Qdate(1,1,2018), to = Qdate(1,1,2024))

## unnamed chunk ----
fraoneshow("XLF", from = Qdate(1,1,2018), to = Qdate(1,1,2021))
fraoneshow("SLP", from = Qdate(1,1,2018), to = Qdate(1,1,2021))

#* Bonds ----

## unnamed chunk ----
fraoneshow("TLT", from = Qdate(1,1,2018), to = Qdate(1,1,2021))

#* Single stocks ----

#* Gamestop ----

## unnamed chunk ----
fraoneshow("GME", from = Qdate(1,1,2020), to = Qdate(1,6,2020))
fraoneshow("GME", from = Qdate(1,1,2021), to = Qdate(1,6,2021))

#* AMC Entertainment ----

## unnamed chunk ----
fraoneshow("AMC", from = Qdate(1,1,2020), to = Qdate(1,6,2020))
fraoneshow("AMC", from = Qdate(1,1,2021), to = Qdate(1,6,2021))

#* CCL carrnival cruises ----

## unnamed chunk ----
fraoneshow("CCL", from = Qdate(1,1,2018), to = Qdate(1,6,2019))
fraoneshow("CCL", from = Qdate(1,1,2020), to = Qdate(1,6,2021))

#* Zeta ----

## unnamed chunk ----
fraoneshow("Z", from = Qdate(1,1,2018), to = Qdate(1,1,2021))

#* Commodities ----

## unnamed chunk ----
fraoneshow("GLD", from = Qdate(1,1,2018), to = Qdate(1,1,2021))

#* OIL ----

## unnamed chunk ----
fraoneshow("USO", from = Qdate(1,1,2020), to = Qdate(1,6,2020))
fraoneshow("USO", from = Qdate(1,1,2021), to = Qdate(1,6,2021))

#* Conclusions ----

#* Black-Scholes model: Implied volatility ----

#* Implied volatilty ----

#* Black-Scholes pricing function ----

#* Implied volatility function ----

#* inside implied volatility  ----

#* Objective function ----

#* Inputs ----

#* Implied volatility function ----

#* Output ----

#* Black-Scholes model: Volatility smile ----

#* Compute the volatility smile: method 1 ----

#* Add an empty column in df_option_data (we will store the implied volatility values in this column later) ----

#* Loop through df_option_data rows, compute the implied volatility for each option and store the value in the 'Implied vol' column of the dataframe ----

#* Plot the volatility smile ----

#* The Volatility Index (VIX) ----

## unnamed chunk ----
VIXSPY <- get_portfolio(c("^VIX", "SPY"), fun = Cl, clean_names = T)
VIXSPY %>% rebase() %>% quickplot()

ggcorrplot(
  cor(ROC(VIXSPY, type = "cont"), use = "na.or.complete"),
  title = "Correlation between returns and volatilty",
  lab = TRUE,
  lab_size = 10,
  legend.title = "")

#* Option pricing and Liquidity ----

## unnamed chunk ----
# implied volatility
bs_price <- function(S, K, r, T, sigma, type = "call") {
  d1 <- (log(S/K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  if (type == "call") {
    return(S * pnorm(d1) - K * exp(-r * T) * pnorm(d2))
  } else if (type == "put") {
    return(K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1))
  } else {
    stop("Option type must be 'call' or 'put'")
  }
}

implied_vol <- function(market_price, S, K, r, T, type = "call") {
  objective <- function(sigma) {
    theoretical_price <- bs_price(S, K, r, T, sigma, type)
    return((theoretical_price - market_price)^2)
  }
  result <- optimize(objective, interval = c(1e-6, 5), tol = 1e-8)
  return(result$minimum)
}

T_left <- as.numeric(Qdate(19,12,2025)-Qdate(27,5,2025))/365
market_price <- as.numeric(Cl(getSymbols(Symbols = "SPY", auto.assign = FALSE, from = Qdate(29, 4, 2025), to = Qdate(30, 4, 2025))))

dir <- "C:\\Users\\pietr\\OneDrive\\Documenti\\CATTOLICA ANNO 24-25\\2 SEMESTRE\\(ASF) - Applied Statistics for Finance\\(ASF) - R\\ASF\\SPY chain.xlsx"

## calls 
data_calls <- readxl::read_xlsx(dir, sheet = "CALLS") %>% 
  mutate(type = "call",
        S = market_price,
        r = r,
        T = T_left
        )

data_calls$implied_vol <- mapply(FUN = implied_vol,
                                market_price = data_calls$`Last Price`,
                                S = data_calls$S,
                                K = data_calls$Strike,
                                r = r,
                                T = T_left,
                                type = data_calls$type
                                )

## puts
data_puts <- readxl::read_xlsx(dir, sheet = "PUTS") %>% 
  mutate(type = "put",
        S = market_price,
        r = r,
        T = T_left
  )

data_puts$implied_vol <- mapply(FUN = implied_vol,
                                market_price = data_puts$`Last Price`,
                                S = data_puts$S,
                                K = data_puts$Strike,
                                r = r,
                                T = T_left,
                                type = data_puts$type
)

ggplot()+
  geom_point(data = data_calls, size = 2, aes(x = Strike, implied_vol, color = "CALLS"), shape = ifelse(data_calls$Moneyness=="OTM", 4, 19)) +
  geom_point(data = data_puts, aes(x = Strike, implied_vol, color = "PUTS"), shape = ifelse(data_puts$Moneyness=="OTM", 4, 19)) +
  theme_minimal()


#* Greeks and Approximations in Option Pricing ----

#* Black-Scholes Greeks function ----

#* Closed-Form Greeks from Black-Scholes ----

#* Why use first-order approximations? ----

#* Calculate implied vol and Greeks ----

#* Greek-by-Greek Breakdown ----

#* Delta ----

#* Parameters ----

#* Positive Delta Shock ----

#* Negative Delta Shock ----

#* Gamma ----

#* Vega ----

#* Theta ----

#* Rho ----

#* Summary of Closed-Form Greek Use ----

#* Numerical Approximations ----

#* Final Thoughts ----

#* Delta (central difference) ----

#* Gamma (central second difference) ----

#* Vega (central difference in volatility) ----

#* Theta (forward difference in time) ----

#* Rho (central difference in rates) ----

#* Print all results ----

## unnamed chunk ----
S0 <- 154.73
timestep <- 1/252
K <- 155
Time <- 33 * timestep
r <- 0.05
sigma <- 0.3018

S0_shock <- 1
# Step 1: calculate the new option price, after a positive shock f(x + h), via full repricing under BS closed formula
optprice_positiveshock <- black_scholes(S0 + S0_shock, K, Time, r, sigma, type = "call")
optprice_positiveshock

# Step 2: calculate the new option price, after a negative shock f(x - h), via full repricing under BS closed formula
optprice_negativeshock <- black_scholes(S0 - S0_shock, K, Time, r, sigma, type = "call")
optprice_negativeshock

# Step 3: calculate the delta via numerical approximation (central difference)
numerical_delta <- (optprice_positiveshock - optprice_negativeshock)/(2 * S0_shock)
numerical_delta

## unnamed chunk ----
beepr::beep(4)
