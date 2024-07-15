cross_tab_continuous <- function(data, var, group = NULL) {
  if (is.null(group)) {
    summary_stats <- data.frame(
      Variable = var,
      Missing = sum(is.na(data[[var]])),
      Overall = paste0(round(mean(data[[var]], na.rm = TRUE), 2), " (", round(sd(data[[var]], na.rm = TRUE), 2), ")")
    )
  } else {
    summary_stats <- data %>%
      group_by(!!sym(group)) %>%
      summarise(
        Mean = round(mean(!!sym(var), na.rm = TRUE), 2),
        SD = round(sd(!!sym(var), na.rm = TRUE), 2)
      ) %>%
      mutate(Value = paste0(Mean, " (", SD, ")")) %>%
      select(-Mean, -SD) %>%
      pivot_wider(names_from = !!sym(group), values_from = Value) %>%
      ungroup()

    missing_count <- sum(is.na(data[[var]]))
    overall <- paste0(round(mean(data[[var]], na.rm = TRUE), 2), " (", round(sd(data[[var]], na.rm = TRUE), 2), ")")
    summary_stats <- data.frame(Variable = var, Missing = missing_count, Overall = overall,summary_stats)
  }
  return(summary_stats)
}



cross_tab_categorical <- function(data, var, group = NULL) {
  if (is.null(group)) {
    summary_stats <- data %>%
      group_by(!!sym(var)) %>%
      summarise(N = n()) %>%
      mutate(Frequency = round(N / sum(N) * 100, 2)) %>%
      select(!!sym(var), Frequency)

    summary_stats <- data.frame(
      Variable = var,
      Missing = sum(is.na(data[[var]])),
      Overall = paste0(sum(!is.na(data[[var]])), " (", round(sum(!is.na(data[[var]])) / nrow(data) * 100, 2), "%)")
    )
  } else {
    summary_stats <- data %>%
      group_by(!!sym(group), !!sym(var)) %>%
      summarise(N = n()) %>%
      group_by(!!sym(var)) %>%
      mutate(Frequency = round(N / sum(N) * 100, 2)) %>%
      pivot_wider(names_from = !!sym(group), values_from = c("N", "Frequency")) %>%
      ungroup()

    missing_count <- sum(is.na(data[[var]]))
    overall <- paste0(sum(!is.na(data[[var]])), " (", round(sum(!is.na(data[[var]])) / nrow(data) * 100, 2), "%)")
    summary_stats <- data.frame(Variable = var, Missing = missing_count, Overall = overall,summary_stats)
  }
  return(summary_stats)
}

library(dplyr)
library(tidyr)

generate_table_one <- function(data, group = NULL, variables) {
  table_list <- list()

  for (var in variables) {
    if (is.numeric(data[[var]])) {
      tab <- cross_tab_continuous(data, var, group)
      tab$`P-Value` <- t.test(data[[var]] ~ data[[group]], data = data)$p.value

    } else {
      tab <- cross_tab_categorical(data, var, group)
      tab$`P-Value` <- chisq.test(table(data[[var]], data[[group]]))$p.value

    }
    table_list[[var]] <- tab
  }

  final_table <- bind_rows(table_list)
  #
  # if (!is.null(group)) {
  #   p_values <- sapply(variables, function(var) {
  #     if (is.numeric(data[[var]])) {
  #       t.test(data[[var]] ~ data[[group]], data = data)$p.value
  #     } else {
  #       chisq.test(table(data[[var]], data[[group]]))$p.value
  #     }
  #   })
  #   final_table$`P-Value` <- p_values
  # }

  return(final_table)
}




# Create the table one
variable_names <- c("x1", "x2", "x3", "x4", "abcat")
group <- "cat"
var="abcat"
table_one <- generate_table_one(data = dat, group = group, variables  = variable_names)


xtab_cat <-cross_tab_categorical(data = dat,var = "abcat",group =group )
xtab_cat

xtab_cont <-cross_tab_continuous(data = dat,var = "x1",group =group )

xtab_cont
