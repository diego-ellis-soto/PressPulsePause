
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(cowplot)
  library(readr)
library(dplyr)
library(ggplot2)
library(magrittr)
require(rsoi)
require(tidyverse)
  library(tidyverse)
  library(tidycovid19)
  library(pdftools)
  library(png)
library(covid19mobility)
library(ggplot2)
library(dplyr)
library(tidyr)
  require(heatwaveR)
})
annual_temp1 <- read_csv('https://pkgstore.datahub.io/core/global-temp/annual_csv/data/a26b154688b061cdd04f1df36e4408be/annual_csv.csv')
annual_temp <- select(annual_temp1[annual_temp1$Source == 'GISTEMP',],c('Year','Mean'))
names(annual_temp) <- c('Year','Temperature_Anomaly')

enso = rsoi::download_enso()

ggplot(annual_temp, aes(x = Year, y = Temperature_Anomaly)) +
  geom_line(size = 1.5) +  # Increase line thickness
  geom_smooth(size = 1.5) +  # Increase smooth line thickness
  theme_classic(base_size = 16) +  # Increase base font size
  labs(
    x = "Year",
    y = "Temperature Anomaly (°C)",
    title = "Global Temperature Anomalies Over Time"
  ) +
  theme(
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 16),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
  )

press <- annual_temp %>%
  filter(Year >= 1980) %>%
  ggplot(aes(x = Year, y = Temperature_Anomaly)) +
  geom_line(size = 1.5) +
  geom_smooth(size = 1.5) +
  theme_classic(base_size = 16) +
  labs(
    x = "Year",
    y = "Temperature Anomaly (°C)",
    title = "Temperature Anomalies Since 1980"
  ) +
  theme(
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 16),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.key.size = unit(1.5, "lines")
  )

ts <- ts2clm(sst_WA, climatologyPeriod = c("1982-01-01", "2011-12-31"))
mhw <- detect_event(ts)


# Save the plot
ggsave(press, filename = "press.pdf", width = 10, height = 6)
# 
# enso.plot <- ggplot(data = enso, aes(x = Date, y = ONI, fill = phase)) +
#   geom_col() +
#   scale_fill_manual(values = c("blue", "green", "red"), name = "") +
#   theme_minimal(base_size = 16) +
#   labs(
#     x = "Date",
#     y = "ENSO Index",
#     title = "Oceanic Niño Index (ONI) Over Time"
#   ) +
#   theme(
#     legend.position = "top",
#     legend.title = element_blank(),
#     legend.text = element_text(size = 14),
#     axis.title = element_text(size = 18, face = "bold"),
#     axis.text = element_text(size = 16),
#     plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
#     panel.grid = element_line(linetype = "dotted")
#   )



enso_area_plot_pulse <- ggplot(data = enso, aes(x = Date, y = ONI)) +
  geom_area(alpha = 0.6, size = 1.5) +
  geom_smooth(size = 1.5) +
  theme_classic(base_size = 16) +
  labs(
    x = "Date",
    y = "ONI (°C)",
    title = "ENSO Pulse Over Time"
  ) +
  theme(
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 16),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.key.size = unit(1.5, "lines")
  )

# Save the plot
ggsave(enso_area_plot_pulse, filename = "pulse_long.pdf", width = 10, height = 6)

# View just a few metrics
mhw$event %>% 
  dplyr::ungroup() %>%
  dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative) %>% 
  dplyr::arrange(-intensity_max) %>% 
  head(5)


pulse_short <- lolli_plot(mhw, metric = "intensity_max") +
  theme_classic(base_size = 16) +
  labs(
    x = "Event Number",
    y = "Maximum Intensity (°C)",
    title = "Top 5 Marine Heatwave Events by Maximum Intensity"
  ) +
  theme(
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 16),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.key.size = unit(1.5, "lines")
  )

# Save the plot
ggsave(pulse_short, filename = "pulse_short.pdf", width = 10, height = 6)

# Now only get the drought:

# Panel 4 data: Pauses (Droughts)
set.seed(4)
time4 <- seq(0, 100, by = 1)
value4 <- rnorm(length(time4), mean = 5, sd = 0.5)

# Introduce drops (pauses) at certain periods
set.seed(5)
pause_starts <- sort(sample(time4, 3))

for (start in pause_starts) {
  end <- start + sample(5:10, 1)  # Drought lasts between 5 to 10 units
  indices <- which(time4 >= start & time4 <= end)
  value4[indices] <- value4[indices] - rnorm(length(indices), mean = 3, sd = 0.5)
}

data4 <- data.frame(Time = time4, Value = value4, Panel = "Pauses (Droughts)")

p4 <- ggplot(data4, aes(x = Time, y = Value)) +
  geom_line(color = "brown", lwd=3) +
  labs(title = "Pauses (Droughts)", x = "Time", y = "Precipitation") +
  theme_bw()

# Display the combined plot
print(p4)




