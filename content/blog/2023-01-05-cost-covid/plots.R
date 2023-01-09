# Packages ------------------------------------------------------------------------------------
library(tidyverse)
library(ggimprensa)
library(patchwork)

# Data and plots muscle mass and strength loss ------------------------------------------------
# Data muscle loss
CSA_loss <-
  tibble::tribble(
  ~time_period, ~group, ~muscle_loss,
  "Admission", "High muscle loss", 0,
  "Admission", "Low muscle loss", 0,
  "Discharge", "High muscle loss", -18,
  "Discharge", "Low muscle loss", -4,
  "6 months after discharge", "High muscle loss", -8,
  "6 months after discharge", "Low muscle loss", 3
)

CSA_loss

# Plot muscle loss
csa_plot <-
  CSA_loss |>
  ggplot(mapping = aes(x = fct_inorder(time_period),
                       y = muscle_loss,
                       group = group)) +
  geom_line(aes(color = group),
            size = 3) +
  geom_point(aes(color = group),
             size = 6) +
  labs(
    title = "Muscle Mass Loss Related to Hospitalization\nfor COVID-19",
    subtitle = "Relative decrease of the vastus lateralis cross-sectional area at admission, discharge\nand 6 months after discharge"
  ) +
  tema_g1() +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  ) +
  scale_color_manual(values = c("red", "blue"))

csa_plot

# Data strength loss
strength_loss <-
  tibble::tribble(
  ~time_period, ~group, ~str_loss,
  "Admission", "High muscle loss", 0,
  "Admission", "Low muscle loss", 0,
  "Discharge", "High muscle loss", -18,
  "Discharge", "Low muscle loss", -8,
  "6 months after discharge", "High muscle loss", -7,
  "6 months after discharge", "Low muscle loss", 9
)


strength_loss

# Plot strength loss
str_plot <-
  strength_loss |>
  ggplot(mapping = aes(x = fct_inorder(time_period),
                       y = str_loss,
                       group = group)) +
  geom_line(aes(color = group),
            size = 3,
            show.legend = FALSE) +
  geom_point(aes(color = group),
             size = 6,
             show.legend = FALSE) +
  labs(
    title = "Handgrip Strength Loss Related to Hospitalization\nfor COVID-19",
    subtitle = "Relative decrease of the handgrip strength at admission, discharge and 6 months after discharge"
  )  +
  tema_g1() +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  ) +
  scale_color_manual(values = c("red", "blue"))

str_plot

# layout

csa_plot/str_plot

# Persistent symptoms ------------------------------------------------------------------------
# creating dataframe
# PASCs
pasc <- c("Fatigue",
          "Myalgia",
          "Dyspnea",
          "Headache",
          "Chest Pain",
          "Dizziness",
          "Nausea",
          "Anosmia",
          "Abdominal Pain",
          "Vomiting",
          "Cough",
          "Diarrhea",
          "Runny Nose",
          "Disgeusia",
          "Earache",
          "Fever"
)

# Prevalence of PASC in the HML
prevalence_HML <- c(76,66,46,30,23,16,16,13,10,10,10,10,10,3,0,0)
prev_pasc_HML <-
  data.frame(pasc, prevalence_HML)

prev_pasc_HML

# Prevalence of PASC in the LML
prevalence_LML <- c(46,36,23,13,3,20,3,16,3,0,16,3,16,16,6,0)
prev_pasc_LML <-
  data.frame(pasc, prevalence_LML)

prev_pasc_LML

# PLOT - Prevalence of PASC in the HML
plot_pasc_HML <-
  prev_pasc_HML |>
  mutate(pasc = fct_reorder(pasc, prevalence_HML)) |>
  ggplot(mapping = aes(x = pasc,
                       y = prevalence_HML)) +
  geom_col(color = "black",
           fill = "red",
           show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "High Muscle Loss"
  ) +
  tema_g1() +
  theme(plot.title = element_text(hjust = 0.5)
  ) +
  geom_text(aes(label = glue::glue("{prevalence_HML}%")),
            hjust = 0,
            size = 4) +
  lims(y = (c(0,80)))

# PLOT - Prevalence of PASC in the LHML
plot_pasc_LML <-
  prev_pasc_LML |>
  mutate(pasc = fct_reorder(pasc, prevalence_LML)) |>
  ggplot(mapping = aes(x = pasc,
                       y = prevalence_LML)) +
  geom_col(color = "black",
           fill = "blue",
           show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Low Muscle Loss"
  ) +
  tema_g1() +
  theme(plot.title = element_text(hjust = 0.5)
  ) +
  geom_text(aes(label = glue::glue("{prevalence_LML}%")),
            hjust = 0,
            size = 4) +
  lims(y = (c(0,80)))

# layout
plot_pasc_HML + plot_pasc_LML

# health care costs ---------------------------------------------------------------------------
# Data health care costs in according muscle loss
costs <-
  tibble::tribble(
    ~time_period, ~group, ~value,
    "2 months after discharge", "High muscle loss", 77283,
    "2 months after discharge", "Low muscle loss", 3057,
    "6 months after discharge", "High muscle loss", 90001,
    "6 months after discharge", "Low muscle loss", 12913
  )

# Plot health care costs
cost_plot <-
  costs |>
  ggplot(mapping = aes(x = time_period,
                       y = value,
                       group = group)) +
  geom_col(
    aes(fill = group),
    color = "black",
    position = "dodge",
    show.legend = TRUE
  ) +
  labs(title = "Health Care Costs Estimates After 2 and 6 Months\nof Hospital Discharge",
       subtitle = "Total COVID-19-related health costs ($)")  +
  tema_g1() +
  theme(legend.position = "top",
        legend.title = element_blank()) +
  scale_fill_manual(values = c("red", "blue")) +
  geom_text(aes(label = glue::glue("U${value}")),
            position = position_dodge(width = 0.9),
            vjust = -1,
            size = 4
  )

cost_plot

