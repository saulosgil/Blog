# pacotes -------------------------------------------------------------------------------------
library(geobr)
library(ggplot2)
library(dplyr)
library(patchwork)

# objeto com todos os estados -----------------------------------------------------------------
states <-
  geobr::read_state(year = 2020,
                    showProgress = FALSE)

# dataset CNES - leitos -----------------------------------------------------------------------
df <-
  readr::read_rds("content/blog/2024-16-02-gender-equity/tabela")

df_joined <-
  dplyr::inner_join(x = states,
                    y = df,
                    by = c("abbrev_state" = "sigla_uf"))

dplyr::glimpse(df_joined)

# grafico -------------------------------------------------------------------------------------
# mapa indiferenciado
g1 <-
  df_joined |>
  mutate(leito_repouso_ind_urgencia_cem_mil =
           as.integer((
             total_leito_repouso_ind_urgencia / populacao
           ) * 100000)) |>
  ggplot() +
  geom_sf(aes(fill = leito_repouso_ind_urgencia_cem_mil),
          color = NA,
          size = .15) +
  labs(title = "Number of undifferentiated rest/observation beds in urgency/emergency units",
       subtitle = "Number of beds per 100,000 inhabitants",
       size = 8) +
  scale_fill_distiller(
    palette = "YlOrRd",
    name = "",
    limits = c(0, 400),
    direction = 2
  ) +
  theme_minimal() +
  geom_sf_text(aes(label = abbrev_state),
               size = 2) +
  xlab("") +
  ylab("")

# coluna indiferenciado
g2 <-
  df_joined |>
  mutate(leito_repouso_ind_urgencia_cem_mil =
           as.integer((
             total_leito_repouso_ind_urgencia / populacao
           ) * 100000)) |>
  mutate(name_state = forcats::fct_reorder(name_state, leito_repouso_ind_urgencia_cem_mil)) |>
  ggplot(
    aes(x = name_state,
        y = leito_repouso_ind_urgencia_cem_mil,
        fill = leito_repouso_ind_urgencia_cem_mil)
  ) +
  geom_bar(stat = "identity",
           colour = "black",
           show.legend = FALSE) +
  xlab("") +
  ylab("Number of beds per 100,000 inhabitants") +
  scale_fill_distiller(palette = "YlOrRd",
                       limits = c(0, 400),
                       direction = 2) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 400)) +
  coord_flip()

# mapa mulheres
g3 <-
  df_joined |>
  mutate(leito_repouso_fem_urgencia_cem_mil =
           as.integer((
             total_leito_repouso_fem_urgencia / populacao
           ) * 100000)) |>
  ggplot() +
  geom_sf(aes(fill = leito_repouso_fem_urgencia_cem_mil),
          color = NA,
          size = .15) +
  labs(title = "Number of women's rest/observation beds in urgency/emergency units",
       subtitle = "Number of beds per 100,000 inhabitants",
       size = 8) +
  scale_fill_distiller(
    palette = "PuRd",
    name = "",
    limits = c(0, 400),
    direction = 2
  ) +
  theme_minimal() +
  geom_sf_text(aes(label = abbrev_state),
               size = 2) +
  xlab("") +
  ylab("")

# coluna mulheres
g4 <-
  df_joined |>
  mutate(leito_repouso_fem_urgencia_cem_mil =
           as.integer((
             total_leito_repouso_fem_urgencia / populacao
           ) * 100000)) |>
  mutate(name_state = forcats::fct_reorder(name_state, leito_repouso_fem_urgencia_cem_mil)) |>
  ggplot(
    aes(x = name_state,
        y = leito_repouso_fem_urgencia_cem_mil,
        fill = leito_repouso_fem_urgencia_cem_mil)
  ) +
  geom_bar(stat = "identity",
           colour = "black",
           show.legend = FALSE) +
  xlab("") +
  ylab("Number of beds per 100,000 inhabitants") +
  scale_fill_distiller(palette = "PuRd",
                       limits = c(0, 400),
                       direction = 2) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 400)) +
  coord_flip()

# mapa homens
g5 <-
  df_joined |>
  mutate(leito_repouso_masc_urgencia_cem_mil =
           as.integer((
             total_leito_repouso_masc_urgencia / populacao
           ) * 100000)) |>
  ggplot() +
  geom_sf(aes(fill = leito_repouso_masc_urgencia_cem_mil),
          color = NA,
          size = .15) +
  labs(title = "Number of men's rest/observation beds in urgency/emergency units",
       subtitle = "Number of beds per 100,000 inhabitants",
       size = 8) +
  scale_fill_distiller(
    palette = "Blues",
    name = "",
    limits = c(0, 400),
    direction = 2
  ) +
  theme_minimal() +
  geom_sf_text(aes(label = abbrev_state),
               size = 2) +
  xlab("") +
  ylab("")

# coluna homens
g6 <-
  df_joined |>
  mutate(leito_repouso_masc_urgencia_cem_mil =
           as.integer((
             total_leito_repouso_masc_urgencia / populacao
           ) * 100000)) |>
  mutate(name_state = forcats::fct_reorder(name_state, leito_repouso_masc_urgencia_cem_mil)) |>
  ggplot(
    aes(x = name_state,
        y = leito_repouso_masc_urgencia_cem_mil,
        fill = leito_repouso_masc_urgencia_cem_mil)
  ) +
  geom_bar(stat = "identity",
           colour = "black",
           show.legend = FALSE) +
  xlab("") +
  ylab("Number of beds per 100,000 inhabitants") +
  scale_fill_distiller(palette = "Blues",
                       limits = c(0, 400),
                       direction = 2) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 400)) +
  coord_flip()

# Layout com todos graficos
(g1 + g2) / (g3 + g4) / (g5 + g6)
