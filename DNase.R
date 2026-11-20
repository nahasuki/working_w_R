library("tidyverse")
DNase_Data = DNase

# Run: which of 11 assay runs the row belongs to
# conc: DNase concentration in ng/ml. It starts at 12.5 and is halved each step, 
# so the 8 values are evenly spaced on a log scale.
# density: the optical density the assay read out

# Each run measured each conc. twice: 11 x 8 x 2 = 176 rows
# My question: how repeatable is the assay across 11 runs? 

# Arguments (all required, in this order):
#   1. output file name, must end in .png
#   2. colour scheme: cool, warm or mono
#   3. base text size, a number between 8 and 20
#
# Command that produced the committed image:
#   Rscript DNaseR dnase.png cool 12
#
# Uses base R and tidyverse only.

# ---- Arguments -------------------------------------------------------
# Infographic: how repeatable is the DNase assay across 11 runs?
#
# Arguments (all required, in this order):
#   1. output file name, must end in .png
#   2. colour scheme: cool, warm or mono
#   3. base text size, a number between 8 and 20
#
# Command that produced the committed image:
#   Rscript DNase.R dnase.png cool 12
#
# Uses base R and tidyverse only.

suppressPackageStartupMessages(library(tidyverse))

# ---- Arguments -------------------------------------------------------
usage <- "Usage: Rscript DNase.R <output.png> <cool|warm|mono> <text size 8-20>"
args  <- commandArgs(trailingOnly = TRUE)

if (length(args) != 3) stop("Expected 3 arguments. ", usage, call. = FALSE)

out_file  <- args[1]
scheme    <- args[2]
text_size <- suppressWarnings(as.numeric(args[3]))

if (!grepl("\\.png$", out_file))
  stop("Output file must end in .png. ", usage, call. = FALSE)
if (!scheme %in% c("cool", "warm", "mono"))
  stop("Unknown colour scheme '", scheme, "'. ", usage, call. = FALSE)
if (is.na(text_size) || text_size < 8 || text_size > 20)
  stop("Text size must be a number between 8 and 20. ", usage, call. = FALSE)

palettes <- list(
  cool = list(run = "#8FA9B8", band = "#BFD7EA", mean = "#0B3C5D", accent = "#D1495B"),
  warm = list(run = "#C9A48A", band = "#F4D6B0", mean = "#7A2E0E", accent = "#1D7874"),
  mono = list(run = "#AAAAAA", band = "#DDDDDD", mean = "#111111", accent = "#555555")
)
pal <- palettes[[scheme]]

# ---- Data ------------------------------------------------------------
d <- as_tibble(as.data.frame(DNase)) |>
  mutate(Run = as.character(Run))

# average the duplicate wells within each run and concentration
run_means <- d |>
  group_by(Run, conc) |>
  summarise(density = mean(density), .groups = "drop")

# spread across the 11 runs at each concentration
by_conc <- run_means |>
  group_by(conc) |>
  summarise(mean = mean(density), lo = min(density), hi = max(density),
            .groups = "drop") |>
  mutate(rel_spread = (hi - lo) / mean)

pct  <- function(x) sprintf("%.0f%%", 100 * x)
low  <- slice_min(by_conc, conc, n = 1)
high <- slice_max(by_conc, conc, n = 1)

# ---- Plot ------------------------------------------------------------
label_size <- text_size / 1.6

p <- ggplot() +
  geom_ribbon(data = by_conc, aes(conc, ymin = lo, ymax = hi),
              fill = pal$band, alpha = 0.8) +
  geom_line(data = run_means, aes(conc, density, group = Run),
            colour = pal$run, linewidth = 0.3) +
  geom_line(data = by_conc, aes(conc, mean),
            colour = pal$mean, linewidth = 1.3) +
  annotate("text", x = low$conc, y = 1.45, hjust = 0, vjust = 0,
           colour = pal$accent, fontface = "bold", size = label_size,
           label = paste0(pct(low$rel_spread), "\nspread at\nlowest dose")) +
  annotate("text", x = high$conc, y = 0.45, hjust = 1, vjust = 1,
           colour = pal$accent, fontface = "bold", size = label_size,
           label = paste0(pct(high$rel_spread), "\nspread at\nhighest dose")) +
  scale_x_log10(breaks = by_conc$conc, labels = \(x) signif(x, 2)) +
  labs(
    title    = "Eleven runs, one curve",
    subtitle = paste0("The DNase assay is steady when there is plenty of enzyme\n",
                      "and shaky when there is little. Shaded band = lowest to highest\n",
                      "of the 11 runs; thick line = their average."),
    x = "DNase concentration (ng/ml, log scale)",
    y = "Optical density",
    caption = "Source: R's built-in DNase dataset (11 runs x 8 concentrations x 2 wells)\nSpread = (highest - lowest run) / average"
  ) +
  theme_minimal(base_size = text_size) +
  theme(
    plot.title.position = "plot",
    plot.title    = element_text(face = "bold", size = rel(1.8)),
    plot.subtitle = element_text(colour = "grey30", margin = margin(b = 12)),
    plot.caption  = element_text(colour = "grey40", hjust = 0, size = rel(0.75)),
    panel.grid.minor = element_blank(),
    plot.margin = margin(15, 15, 10, 15)
  )

# ---- Save (fixed size so every run matches) --------------------------
ggsave(out_file, p, width = 8, height = 7, dpi = 150, bg = "white")
message("Wrote ", out_file)




