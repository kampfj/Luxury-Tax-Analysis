library(rvest)
library(dplyr)
library(magrittr)
library(stringr)
library(purrr)
library(readr)
library(ggplot2)
library(gt)
library(tidyr)
library(scales)

# Given team dollars over tax and whether or not they are 
# a repeat offender, return their tax rate.
get_rate <- function(repeater, overage) {
  if (overage <= 4999999) {
    return(ifelse(repeater, 2.5, 1.5))
  }
  if (overage <= 9999999) {
    return(ifelse(repeater, 2.75, 1.75))
  }
  if (overage <= 14999999) {
    return(ifelse(repeater, 3.5, 2.5))
  }
  if (overage <= 19999999) {
    return(ifelse(repeater, 4.25, 3.25))
  }
  # Figure out how many times we need to add fifty cents to your tax rate 
  # depending on how far above 20mm you are.
  add_fifty_cents <- floor((overage-20000000)/5000000)
  return(ifelse(repeater,(4.75 + 0.5*add_fifty_cents), (3.75 + 0.5*add_fifty_cents)))
}

# Given upper bound and whether or not the team is a repeat tax offender,
# return the incremental maximum paid for exceeding the upper bound.
get_incremental_maximum <- function(repeater, upper) {
  if (upper == 4999999) {
    return(ifelse(repeater, 12500000, 7500000))
  }
  if (upper == 9999999) {
    return(ifelse(repeater, 13750000, 8750000))
  }
  if (upper == 14999999) {
    return(ifelse(repeater, 17500000, 12500000))
  }
  if (upper == 19999999) {
    return(ifelse(repeater, 21250000, 16250000))
  }
}

# Given dollars over tax and whether or not team is repeat offender, 
# determine their tax bill.
get_tax_bill <- function(overage, repeater) {
  sum <- 0
  if (overage <= 4999999) {
    return(get_rate(repeater, overage)*overage);
  }
  sum <- sum + get_incremental_maximum(repeater, 4999999)
  if (overage <= 9999999) {
    diff <- overage - 5000000
    tax_dollars <- get_rate(repeater, overage)*diff
    return(sum + tax_dollars)
  }
  sum <- sum + get_incremental_maximum(repeater, 9999999)
  if (overage <= 14999999) {
    diff <- overage - 10000000
    tax_dollars <- get_rate(repeater, overage)*diff
    return(sum + tax_dollars)
  }
  sum <- sum + get_incremental_maximum(repeater, 14999999)
  if (overage <= 19999999) {
    diff <- overage - 15000000
    tax_dollars <- get_rate(repeater, overage)*diff
    return(sum + tax_dollars)
  }
  sum <- sum + get_incremental_maximum(repeater, 19999999)
  diff <- overage - 20000000
  return(sum + get_rate(repeater, overage)*diff)
}

# Look at repeater tax bills vs non repeat offenders.
get_repeater_vs_not <- function() {
  overages <- seq(1,40)*1000000
  repeater_tax_bill <-sapply(overages, FUN=get_tax_bill,repeater=TRUE)
  non_repeater_tax_bill <- sapply(overages, FUN=get_tax_bill,repeater=FALSE)
  df <- data.frame(overages, repeater_tax_bill, non_repeater_tax_bill)
  return(df)
}

plot_repeater_vs_not_bills <- function() {
  df <- get_repeater_vs_not()
  df %>% ggplot() + 
    geom_line(aes(x = overages, y = repeater_tax_bill), color = "red") +
    geom_line(aes(x = overages, y = non_repeater_tax_bill), color = "blue") +
    labs(
      x = "Dollars over tax threshold", 
      y = "Tax Bill",
      title = "Tax Accounting for Repeat vs Non-Repeat Offenders", 
      subtitle = "Luxury tax bills for teams based on severity of tax violation (in dollars)",
      caption = "Calculation logic from CBAFAQ.com. Visualization by JJ Kampf."
    ) +
    scale_y_continuous(labels = label_number(prefix = "$", suffix = " M", scale = 1e-6)) + # millions
    scale_x_continuous(labels = label_number(prefix = "$", suffix = " M", scale = 1e-6)) +
    theme_bw() + 
    theme(
      plot.margin = unit(c(1, 1, 3, 1), "lines"), 
      plot.caption = element_text(vjust = -15),
      axis.title.x = element_text(vjust=-3),
      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
    ) %>%
    ggsave(filename = "repeat_v_not_bills.png")
}

plot_repeater_vs_not_deltas <- function() {
  df <- get_repeater_vs_not()
  df$delta = df$repeater_tax_bill - df$non_repeater_tax_bill
  df %>% ggplot() + 
    geom_line(aes(x = overages, y = delta), color = "red") +
    labs(
      x = "Dollars over tax threshold", 
      y = "Delta",
      title = "Penalty for being a repeater scales linearly with dollars spent", 
      caption = "Calculation logic from CBAFAQ.com. Visualization by JJ Kampf."
    ) +
    scale_y_continuous(labels = label_number(prefix = "$", suffix = " M", scale = 1e-6)) + # millions
    scale_x_continuous(labels = label_number(prefix = "$", suffix = " M", scale = 1e-6)) +
    theme_bw() + 
    theme(
      plot.margin = unit(c(1, 1, 3, 1), "lines"), 
      plot.caption = element_text(vjust = -15),
      axis.title.x = element_text(vjust=-3),
      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
    ) %>%
    ggsave(filename = "repeat_v_not_deltas.png")
}

# Look at GSW tax dollars owed due to Wiseman and tax dollars owed due to 
# Payton for the remainder of their contracts.
compare_wiseman_payton_tax <- function() {
  years = 2022:2023
  gsw_overage_without = 32000000
  gsw_tax_rate_without = get_rate(TRUE, gsw_overage_without)
  wiseman = c(9603360, 12119440)
  payton = c(8300000, 8715000)
  wiseman_tax = gsw_tax_rate_without*wiseman
  payton_tax = gsw_tax_rate_without*payton
  df <- data.frame(years, wiseman, payton, wiseman_tax, payton_tax)
  df$years <- as.factor(df$years)
  colors <- c("G. Payton Salary" = "black", "G. Payton Tax" = "blue", "J. Wiseman Salary" = "gray", "J. Wiseman Tax" = "red")
  df %>% ggplot() + 
    geom_line(aes(x = years, y = payton, group=1, color = "G. Payton Salary")) +
    geom_line(aes(x = years, y = payton_tax, group=1, color = "G. Payton Tax")) +
    geom_line(aes(x = years, y = wiseman, group=1, color = "J. Wiseman Salary")) +
    geom_line(aes(x = years, y = wiseman_tax, group=1, color = "J. Wiseman Tax")) +
    labs(
      x = "Season", 
      y = "Tax Bill for Player",
      title = "Wiseman vs Payton Salary and Tax Obligation", 
      subtitle = "GSW tax cost of the remainder of their current contracts.",
      caption = "Calculation logic from CBAFAQ.com. Visualization by JJ Kampf."
    ) +
    scale_y_continuous(labels = label_number(prefix = "$", suffix = " M", scale = 1e-6)) + #millions
    scale_color_manual(values=colors) +
    theme_bw() + 
    theme(
      plot.margin = unit(c(1, 1, 3, 1), "lines"), 
      plot.caption = element_text(vjust = -15),
      axis.title.x = element_text(vjust=-3),
      legend.title = element_blank(),
      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
    )
  ggsave(filename = "payton_v_wiseman.png")
}

plot_tax_and_cap <- function() {
  years_cap <- c('2020-21', '2021-22', '2022-23')
  tax <- c(132627000, 136606000, 150267000)
  cap <- c(109140000, 112414000, 123655000)
  colors_cap <- c("Luxury Tax" = "Red", "Salary Cap" = "blue")
  data <- data.frame(years_cap, tax, cap)
  data %>% ggplot() + 
    geom_line(aes(x = years_cap, y = tax, group = 1, color = 'Luxury Tax')) +
    geom_line(aes(x = years_cap, y = cap, group = 1, color = 'Salary Cap')) +
    labs( 
      x = "Season", 
      y = "Tax Level and Salary Cap",
      title = "The Tax Moves in Lockstep with the Cap", 
      subtitle = "Looking at salary cap and luxury tax levels in the last three seasons.",
      caption = "Data from Spotrac. Visualization by JJ Kampf."
    ) +
    scale_y_continuous(labels = label_number(prefix = "$", suffix = " M", scale = 1e-6)) + #millions
    scale_color_manual(values=colors_cap) +
    theme_bw() + 
    theme(
      plot.margin = unit(c(1, 1, 3, 1), "lines"), 
      plot.caption = element_text(vjust = -15),
      axis.title.x = element_text(vjust=-3),
      legend.title = element_blank(),
      axis.title.y = element_text(margin = margin(t = 0, r = 30, b = 0, l = 0))
    ) %>%
    ggsave(filename = "tax_and_cap.png")
}

# Function to pull out luxury tax data from spotrac for a given year.
get_tax_tracker <- function(year) {
  tables <- str_interp('https://www.spotrac.com/nba/tax/${year}/') %>%
    read_html() %>%
    html_nodes(xpath="/html/body/div[1]/div[2]/div/div/div[1]/div/div/div[3]/table") %>%
    html_table(header=TRUE)
  table <- tables[[1]]
  table$year <- year
  table$Team = str_sub(table$Team, -3)
  return(table)
}

# Function to pull in all data for given years.
union_tax_data <- function(years) {
  years %>%
    map_dfr(get_tax_tracker)
}

# Utility: get team logo.
get_team_logo <- function(team_abbr) {
  abbr = team_abbr
  if (team_abbr == 'BKN') {
    abbr = 'NJN'
  }
  if (team_abbr == 'NOP') {
    abbr = 'NOH'
  }
  if (team_abbr == 'UTH') {
    abbr = 'UTA'
  }
  if (team_abbr == 'PHX') {
    abbr = 'PHO'
  }
  return(
    str_interp("https://cdn.ssref.net/req/202302231/tlogo/bbr/${abbr}.png")
  )
}

# Map team abbreviation column to logos.
map_abbr_to_logo <- function(abbr_column) {
  map_chr(abbr_column,
          ~ web_image(
            url = get_team_logo(.),
            height = 100
          ) 
  )
}

# Get tax data for desired years.
res <- union_tax_data(c('2018', '2019', '2020', '2021', '2022'))

# Clean col names.
res <- res %>%
       rename(`Full Payroll` = `Luxury TaxPayroll`,
              `Below Tax` = `Luxury TaxSpace`,
              `Tax Bill` = `Luxury TaxBill (est)`)

# Create table of team payrolls over the past three years.
res %>% 
  pivot_wider(id_cols=(`Team`), 
              names_from = year,
              values_from = c(`Full Payroll`)
  ) %>%
  gt() %>%
  tab_header(
    title= md("Team Historic Payrolls"), 
    subtitle = md("Full active payrolls for each team over the past five seasons.")
  ) %>%
  tab_source_note(source_note = md("Data from Spotrac. Table by JJ Kampf.")) %>%
  cols_align(
    align = 'left', 
    columns=everything()
  ) %>%
  tab_style(
    locations = cells_title(groups="title"),
    style = list(
      cell_text(weight="bold", size="24")
    )
  ) %>%
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style = list(
      cell_borders(sides="bottom", weight=px(3)),
      cell_text(weight = "bold")
    )
  ) %>% 
  tab_style(
    locations = cells_body(
      columns = everything(),
      rows = everything()
    ),
    style = cell_borders(sides = "all", color = "#000000", style = "solid", weight = px(1))
  ) %>%
  text_transform(
    locations=cells_body(c(`Team`)),
    fn=function(x) {
      map_abbr_to_logo(x)
    }
  ) %>%
  tab_options(
    #Remove border between column headers and title
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = "transparent",
    table.margin.right = px(10),
    table.margin.left = px(10),
    #Remove border around table
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    table.width = pct(100),
    row_group.font.weight = "bold",
    #Reduce the height of rows
    data_row.padding = px(3),
    #Adjust font sizes and alignment
    table.font.size = 34,
    source_notes.font.size = 30,
    heading.align = "left"
  ) %>%
  gtsave(filename = "payrolls_18_22.png", vwidth = 1400, vheight = 850)


# Convert dollar strings to numeric values.
res <- res %>%
        mutate_at(vars(`Active Payroll`,
                       `Dead Payroll`,
                       `Full Payroll`,
                       `Below Tax`,
                       `Tax Bill`), ~parse_number(.))

# Add column for whether the team is a tax violator.
res$Taxpayer = if_else(res$`Below Tax` >= 0, 'No', 'Yes')
# Stack columns back up.
taxpayers <- res %>%
        pivot_wider(id_cols=(`Team`), 
                    names_from = year,
                    values_from = c(Taxpayer)
                    ) 
# Get table of repeat tax offenders.
taxpayers %>% gt() %>%
  tab_header(
    title= md("Repeat tax offenders"), 
    subtitle = md("*Yes* in 3 out of the last 4 seasons (2018-2021 inclusive).")
  ) %>%
  tab_source_note(source_note = md("Data from Spotrac. Table by JJ Kampf")) %>%
  cols_align(
    align = 'left', 
    columns=everything()
  ) %>%
  cols_label(
    `2022` = md("2022 *proj.*")
  ) %>%
  tab_style(
    locations = cells_title(groups="title"),
    style = list(
      cell_text(weight="bold", size="36")
    )
  ) %>%
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style = list(
      cell_borders(sides="bottom", weight=px(3)),
      cell_text(weight = "bold")
    )
  ) %>% 
  tab_style(
    locations = cells_body(
      columns = everything(),
      rows = everything()
    ),
    style = cell_borders(sides = "all", color = "#000000", style = "solid", weight = px(1))
  ) %>%
  text_transform(
    locations=cells_body(c(`Team`)),
    fn=function(x) {
      map_abbr_to_logo(x)
    }
  ) %>%
  data_color(
    columns = c(`2018`, `2019`, `2020`, `2021`, `2022`),
    colors = scales::col_factor(
      palette=c("#FFCCCB", "#90EE90"),
      levels=c("Yes", "No")
    )
  ) %>%
  tab_options(
    #Remove border between column headers and title
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = "transparent",
    #Remove border around table
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    table.width = pct(110),
    row_group.font.weight = "bold",
    #Reduce the height of rows
    data_row.padding = px(3),
    #Adjust font sizes and alignment
    table.font.size = 34,
    source_notes.font.size = 30,
    heading.align = "left"
  ) %>%
  gtsave(filename = 'taxpayers_18_22.png', vwidth = 900, vheight = 850)



  



