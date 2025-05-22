#' Process and Plot Fisheries Data
#'
#' This function processes fisheries data, applies filtering, calculates statistics, and generates plots of the results.
#'
#' @param sub_list_dir_2 List of directories containing the data files.
#' @param parameter_fact Character string specifying the type of data ("catch" or "effort").
#' @param parameter_filtering List of filtering parameters to be passed to the filtering function.
#'
#' @return A list containing the processed data frame and the generated plots.
#' @examples
#' \dontrun{
#' result <- process_fisheries_data(sub_list_dir_2, "catch", parameter_filtering)
#' print(result$processed_data)
#' print(result$second_graf)
#' print(result$no_fish_plot)
#' print(result$tons_plot)
#' }
#' @export
#' @importFrom qs qread
#' @import ggplot2
process_fisheries_data <- function(sub_list_dir_2, parameter_fact, parameter_filtering) {

  if (parameter_fact == "catch") {

    # Check for nominal dataset
    have_nominal <- FALSE
    if (dir.exists("Markdown")) {
      nom_file1 <- "data/global_nominal_catch_firms_level0_harmonized.csv"
      nom_file2 <- "data/global_nominal_catch_firms_level0_2025.csv"
      if (file.exists(nom_file1) || file.exists(nom_file2)) {
        fname <- if (file.exists(nom_file1)) nom_file1 else nom_file2
        nominal_dataset <- readr::read_csv(fname)
        nominal_dataset <- CWP.dataset::filtering_function(nominal_dataset, parameter_filtering = parameter_filtering)
        nominal <- sum(nominal_dataset$measurement_value)
        have_nominal <- TRUE
      }
    }

    # Prepare column names
    base_cols <- c(
      "Step", "Explanation", "Functions", "Options",
      "Tons", "Number of fish", "Lines",
      "Difference (in % of tons)", "Difference in tons",
      "Difference (in % of fish)", "Difference in number of fish", "Difference (in % of lines)",
      "Conversion factors (kg)"
    )
    all_cols <- if (have_nominal) c(base_cols, "Percentage of nominal") else base_cols

    # Initialize empty data frame with correct column types
    df <- data.frame(matrix(ncol = length(all_cols), nrow = 0), stringsAsFactors = FALSE)
    colnames(df) <- all_cols
    # Ensure character columns
    df$Step <- character()
    df$Explanation <- character()
    df$Functions <- character()
    df$Options <- character()
    # Ensure numeric columns
    num_cols <- setdiff(colnames(df), c("Step","Explanation","Functions","Options"))
    for (col in num_cols) {
      df[[col]] <- numeric()
    }

    # Initial sums
    main <- CWP.dataset::filtering_function(qs::qread(paste0(sub_list_dir_2[1], "/data.qs")), parameter_filtering = parameter_filtering)
    tons_init <- sum((main %>% dplyr::filter(measurement_unit %in% c("MTNO", "MT", "t", "Tons")))$measurement_value)
    nofish_init <- sum((main %>% dplyr::filter(measurement_unit %in% c("NOMT", "NO", "no", "Number of fish")))$measurement_value)
    lines_init <- nrow(main)

    for (i in sub_list_dir_2) {
      step <- tail(str_split(i, "/")[[1]], n = 1)
      Explanation <- readLines(paste0(i, "/explanation.txt"))[1]
      Functions   <- readLines(paste0(i, "/functions.txt"))[1]
      Options     <- if (file.exists(paste0(i, "/options_written.txt"))) readLines(paste0(i, "/options_written.txt"))[1] else "None"

      if (isNullList(parameter_filtering)) {
        sums <- read_csv(paste0(i, "/sums.csv"))
        sum_t <- sums$sum_t; sum_no <- sums$sum_no; nrow_i <- sums$lines
      } else {
        main_i <- CWP.dataset::filtering_function(qs::qread(paste0(i, "/data.qs")), parameter_filtering = parameter_filtering)
        sum_t <- sum((main_i %>% dplyr::filter(measurement_unit %in% c("MTNO","MT","t","Tons")))$measurement_value)
        sum_no <- sum((main_i %>% dplyr::filter(measurement_unit %in% c("NOMT","NO","no","Number of fish")))$measurement_value)
        nrow_i <- nrow(main_i)
      }

      # Differences
      diff_pct_tons   <- -100 * ((tons_init - sum_t) / tons_init)
      diff_tons       <- -(tons_init - sum_t)
      diff_no         <- -(nofish_init - sum_no)
      diff_pct_no     <- -100 * ((nofish_init - sum_no) / nofish_init)
      diff_pct_lines  <- -100 * ((lines_init - nrow_i) / lines_init)

      # Conversion factor
      conv_kg <- if (!is.na(diff_no) && diff_no != 0 && diff_tons != 0) {
        (abs(diff_tons) / abs(diff_no)) * 1000
      } else NA

      # Build row
      row_vals <- list(
        Step                       = step,
        Explanation                = Explanation,
        Functions                  = Functions,
        Options                    = Options,
        Tons                       = sum_t,
        `Number of fish`          = sum_no,
        Lines                      = nrow_i,
        `Difference (in % of tons)`= diff_pct_tons,
        `Difference in tons`       = diff_tons,
        `Difference (in % of fish)`= diff_pct_no,
        `Difference in number of fish` = diff_no,
        `Difference (in % of lines)`= diff_pct_lines,
        `Conversion factors (kg)`  = conv_kg
      )
      if (have_nominal) {
        row_vals$`Percentage of nominal` <- round((sum_t * 100) / nominal, 1)
      }

      df <- dplyr::bind_rows(df, row_vals)

      # Update for next loop
      tons_init   <- sum_t
      nofish_init <- sum_no
      lines_init  <- nrow_i
    }

    # Prepare reduced for plotting
    reduced <- df %>%
      dplyr::mutate(`Millions of tons` = Tons / 1e6,
                    `Millions of fish` = `Number of fish` / 1e6)
    # remove nominal column from reduced if absent
    select_cols <- c("Step", "Millions of tons", "Millions of fish",
                     "Difference (in % of tons)", "Difference (in % of fish)",
                     if (have_nominal) "Percentage of nominal" else NULL,
                     "Conversion factors (kg)")
    reduced <- reduced %>% dplyr::select(dplyr::all_of(select_cols)) %>%
      dplyr::mutate(`Step number` = as.numeric(dplyr::row_number()))

    reduced$Step <- factor(reduced$Step, levels = reduced$Step[order(reduced$`Step number`)])

    # Plotting
    coeff <- 3
    temperatureColor <- "#69b3a2"; priceColor <- rgb(0.2, 0.6, 0.9, 1)

    second_graf <- ggplot2::ggplot(reduced, aes(x = Step, group = 1)) +
      ggplot2::geom_line(aes(y = `Millions of tons`), size = 0.5, color = priceColor) +
      ggplot2::geom_point(aes(y = `Millions of tons`)) +
      ggplot2::geom_line(aes(y = `Millions of fish` / coeff), size = 0.5, color = temperatureColor) +
      ggplot2::geom_point(aes(y = `Millions of fish` / coeff)) +
      ggplot2::scale_y_continuous(
        name = "Tons",
        sec.axis = ggplot2::sec_axis(~ . * coeff, name = "Number of fish")
      ) +
      ggplot2::theme(
        axis.title.y       = element_text(color = priceColor, size = 8),
        axis.title.y.right = element_text(color = temperatureColor, size = 8)
      ) +
      ggplot2::ggtitle("Evolution of the repartition of captures depending on units and Steps") +
      ggplot2::theme(axis.text.x = element_text(angle = 90))

    no_fish_plot <- ggplot2::ggplot(reduced, aes(x = Step, y = `Millions of fish`, group = 1)) +
      ggplot2::geom_line(size = 0.5) +
      ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.1))) +
      ggplot2::theme(axis.text.x = element_text(angle = 90))

    tons_plot <- ggplot2::ggplot(reduced, aes(x = Step, y = `Millions of tons`, group = 1)) +
      ggplot2::geom_line(size = 0.5) +
      ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.1))) +
      ggplot2::theme(axis.text.x = element_text(angle = 90))

    cowplot <- cowplot::plot_grid(no_fish_plot, tons_plot)
    columns_to_color <- c("Difference (in % of tons)", "Difference (in % of fish)")
    fig.capp <- 'Evolution of captures in tons and number of fish during the process'

  }

  return(list(
    reduced            = reduced,
    cowplot            = cowplot,
    second_graf        = second_graf,
    full_df            = df,
    columns_to_color   = columns_to_color,
    fig.capp           = fig.capp
  ))
}
