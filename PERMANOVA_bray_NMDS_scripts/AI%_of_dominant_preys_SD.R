################################################################
# Fig 2.2 – Alimentary Index (AIi%) of dominant food items
# Hemigrammus marginatus – Atlantic Forest vs Caatinga
#ENIOLA OLU-AYORINDE
#################################################################

library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

df <- read_excel("generated_data/HMR_IAI.xlsx")

atlantic_sites <- c("A", "G", "P")
caatinga_sites <- c("C", "I", "N", "U")

site_names <- c(
  A = "Amparo",       G = "Gararu",       P = "Propria",
  C = "Curralinho",   I = "Ilha do ouro",
  N = "Niteroi",      U = "UHE Xingo"
)

dominant <- c("Insect Remains", "Filamentous Algae",
              "Formicidae", "Organic matter",
              "Ephemeroptera", "Others (Rare Items)")

# 3. Reshape: dominant items + compute Others (Rare Items)

long <- df %>%
  pivot_longer(-Prey, names_to = "Code", values_to = "AIi_prop") %>%
  mutate(
    AIi      = AIi_prop * 100,
    SiteCode = substr(Code, 1, 1),
    Biome    = case_when(
      SiteCode %in% atlantic_sites ~ "Atlantic Forest",
      SiteCode %in% caatinga_sites ~ "Caatinga",
      TRUE ~ NA_character_
    ),
    Site = site_names[SiteCode]
  ) %>%
  filter(!is.na(Biome))

# Sum rare items into "Others (Rare Items)" per sample column
dominant_items <- c("Insect Remains", "Filamentous Algae",
                    "Formicidae", "Organic matter", "Ephemeroptera")

others <- long %>%
  filter(!Prey %in% dominant_items) %>%
  group_by(Code, Biome, Site) %>%
  summarise(AIi = sum(AIi), .groups = "drop") %>%
  mutate(Prey = "Others (Rare Items)")

# Combine dominant + others
combined <- long %>%
  filter(Prey %in% dominant_items) %>%
  bind_rows(others)

# Average across seasons per site
site_means <- combined %>%
  group_by(Biome, Prey, Site) %>%
  summarise(AIi_site = mean(AIi), .groups = "drop")

# Biome-level mean ± SD across sites
summary_df <- site_means %>%
  group_by(Biome, Prey) %>%
  summarise(
    mean_AIi = mean(AIi_site),
    sd_AIi   = sd(AIi_site),
    .groups  = "drop"
  ) %>%
  mutate(
    Prey = factor(Prey, levels = dominant)
  )


pp <- ggplot(summary_df, aes(x = Prey, y = mean_AIi)) +
  
  geom_col(fill = "grey40", width = 0.6) +
  
  geom_errorbar(
    aes(ymin = mean_AIi - sd_AIi,
        ymax = mean_AIi + sd_AIi),
    width     = 0.25,
    colour    = "black",
    linewidth = 0.6
  ) +
  
  facet_wrap(~ Biome, ncol = 2,
             labeller = labeller(Biome = c(
               "Atlantic Forest" = "(A) Atlantic Forest",
               "Caatinga"        = "(B) Caatinga"
             ))) +
  
  labs(
    x = "Food Items",
    y = "Average Percentage (%)"
  ) +
  
  theme_classic(base_size = 11) +
  theme(
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold", size = 11, hjust = 0),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y      = element_text(size = 9),
    axis.title       = element_text(size = 11),
    panel.border     = element_rect(colour = "black", fill = NA,
                                    linewidth = 0.7)
  )
pp

ggsave("Fig2_2_AIi_errorbars.png", plot = pp,
       width = 8, height = 5, dpi = 300, bg = "white")

ggsave("Fig2_2_AIi_errorbars.pdf", plot = pp,
       width = 8, height = 5, bg = "white")

message("Done! Saved: Fig2_2_AIi_errorbars.png / .pdf")


#Define sites and biome assignments
atlantic_sites <- c("A", "G", "P")
caatinga_sites <- c("C", "I", "N", "U")

site_names <- c(
  A = "Amparo",       G = "Gararu",       P = "Propria",
  C = "Curralinho",   I = "Ilha do ouro",
  N = "Niteroi",      U = "UHE Xingo"
)

dominant_items <- c("Insect Remains", "Filamentous Algae",
                    "Formicidae", "Organic matter", "Ephemeroptera")

# 3. Reshape and compute Others (Rare Items)
long <- df %>%
  pivot_longer(-Prey, names_to = "Code", values_to = "AIi_prop") %>%
  mutate(
    AIi      = AIi_prop * 100,
    SiteCode = substr(Code, 1, 1),
    Biome    = case_when(
      SiteCode %in% atlantic_sites ~ "Atlantic Forest",
      SiteCode %in% caatinga_sites ~ "Caatinga",
      TRUE ~ NA_character_
    ),
    Site = site_names[SiteCode]
  ) %>%
  filter(!is.na(Biome))

# Compute Others per sample code
others <- long %>%
  filter(!Prey %in% dominant_items) %>%
  group_by(Code, Biome, Site, SiteCode) %>%
  summarise(AIi = sum(AIi), .groups = "drop") %>%
  mutate(Prey = "Others (Rare Items)")

# Combine dominant + others
combined <- long %>%
  filter(Prey %in% dominant_items) %>%
  bind_rows(others)

# Average across seasons per site
site_means <- combined %>%
  group_by(Biome, Site, Prey) %>%
  summarise(AIi_site = mean(AIi), .groups = "drop") %>%
  mutate(
    Prey = factor(Prey, levels = c(
      "Insect Remains", "Filamentous Algae",
      "Formicidae", "Organic matter",
      "Ephemeroptera", "Others (Rare Items)"
    )),
    # Order sites within biome
    Site = factor(Site, levels = c(
      "Amparo", "Gararu", "Propria",          # Atlantic Forest
      "Curralinho", "Ilha do ouro",            # Caatinga
      "Niteroi", "UHE Xingo"                  # Caatinga
    )),
    Biome = factor(Biome, levels = c("Atlantic Forest", "Caatinga"))
  )

prey_colours <- c(
  "Insect Remains"      = "#E69F00",
  "Filamentous Algae"   = "#56B4E9",
  "Formicidae"          = "#009E73",
  "Organic matter"      = "#F0E442",
  "Ephemeroptera"       = "#0072B2",
  "Others (Rare Items)" = "#999999"
)

ppp <- ggplot(site_means, aes(x = Site, y = AIi_site, fill = Prey)) +
  
  geom_col(width = 0.7, colour = "white", linewidth = 0.3) +
  
  facet_wrap(~ Biome, ncol = 2, scales = "free_x",
             labeller = labeller(Biome = c(
               "Atlantic Forest" = "(A) Atlantic Forest",
               "Caatinga"        = "(B) Caatinga"
             ))) +
  
  scale_fill_manual(values = prey_colours, name = "Food Items") +
  
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, 25),
                     expand = c(0, 0)) +
  
  labs(
    x = "Site",
    y = "Average Alimentary Index (AIi%)"
  ) +
  
  theme_classic(base_size = 11) +
  theme(
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold", size = 11, hjust = 0),
    axis.text.x      = element_text(angle = 35, hjust = 1, size = 9),
    axis.text.y      = element_text(size = 9),
    axis.title       = element_text(size = 11),
    legend.title     = element_text(size = 10, face = "bold"),
    legend.text      = element_text(size = 9),
    legend.position  = "right",
    panel.border     = element_rect(colour = "black", fill = NA,
                                    linewidth = 0.7)
  )
ppp
ggsave("Fig2_2_stacked_by_site.png", plot = ppp,
       width = 9, height = 5, dpi = 300, bg = "white")

ggsave("Fig2_2_stacked_by_site.pdf", plot = ppp,
       width = 9, height = 5, bg = "white")

message("Done! Saved: Fig2_2_stacked_by_site.png / .pdf")
