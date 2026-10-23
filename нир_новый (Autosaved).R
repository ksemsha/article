library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

setwd("/Users/Ksusa/Desktop/")

df_raw <- read_excel(
  "02_06_loans_by_aggregated_interest_rate.xlsx",
  sheet = "всего",
  col_names = FALSE
)

col_names <- c("Период", as.character(unlist(df_raw[3, 2:ncol(df_raw)])))
colnames(df_raw) <- col_names

numeric_cols <- c("менее 4%", "4-7,99%", "8-11,99%",
                  "12-15,99%", "16-19,99%", "20-23,99%", "24% и выше")

df <- df_raw[4:nrow(df_raw), ] %>%
  as.data.frame() %>%
  filter(!is.na(Период)) %>%
  filter(grepl("\\d{4}", Период))

df[numeric_cols] <- lapply(df[numeric_cols], function(x) as.numeric(as.character(x)))

df <- tail(df, 12)

df <- df %>%
  mutate(
    `< 4%`   = `менее 4%`,
    `4-12%`  = `4-7,99%` + `8-11,99%`,
    `12-20%` = `12-15,99%` + `16-19,99%`,
    `> 20%`  = `20-23,99%` + `24% и выше`
  )

groups <- c("> 20%", "12-20%", "4-12%", "< 4%")

df_shares <- df %>%
  select(Период, all_of(groups)) %>%
  mutate(total = rowSums(across(all_of(groups)), na.rm = TRUE)) %>%
  mutate(across(all_of(groups), ~ . / total * 100)) %>%
  select(-total) %>%
  mutate(Период = as.character(Период))

df_shares$Период <- factor(df_shares$Период, levels = df_shares$Период)

df_long <- df_shares %>%
  pivot_longer(cols = all_of(groups), names_to = "Ставка", values_to = "Доля")

df_long$Ставка <- factor(df_long$Ставка, levels = groups)

ggplot(df_long, aes(x = Период, y = Доля, fill = Ставка)) +
  geom_bar(stat = "identity", position = "stack", na.rm = TRUE) +
  scale_fill_manual(
    values = c(
      "< 4%"   = "#1f77b4",
      "4-12%"  = "#ff7f0e",
      "12-20%" = "#2ca02c",
      "> 20%"  = "#d62728"
    ),
    breaks = c("< 4%", "4-12%", "12-20%", "> 20%")
  ) +
  scale_y_continuous(limits = c(0, 101), expand = c(0, 0)) +
  labs(
    title = "Структура кредитов по процентным ставкам (последние 12 месяцев)",
    y = "Доля, %",
    x = NULL,
    fill = "Ставка"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )