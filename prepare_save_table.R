

install.packages("webshot")
webshot::install_phantomjs()  # Install PhantomJS to take screenshots


# Load libraries
library(knitr)
library(kableExtra)
library(webshot)

# Create the table
table_data <- data.frame(
  CLASS = 1:5,
  VALUES = c("0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1"),
  CONSERVATION = c("Irrelevant", "Few relevant", "Medium relevant", "High relevant", "Very high relevant"),
  #COLOUR1 = c("#142c5f", "#124663", "#2e6a57", "#f6a895", "#ffbad7"),
  COLOUR1 = "", # remove colour code
  PRESSURE = c("Low-moderate pressure", "Moderate-strong pressure", "Strong-very strong pressure", "Very strong pressure", "extreme pressure"),
  #COLOUR2 = rev(c("#142c5f", "#124663", "#2e6a57", "#f6a895", "#ffbad7"))
  COLOUR2 = "" # remove colour code
)

# Save the table as an HTML file
kable(table_data, "html", escape = FALSE) |>
  kable_styling(full_width = FALSE) |>
  column_spec(4, background = c("#142c5f", "#124663", "#2e6a57", "#f6a895", "#ffbad7")) |>
  column_spec(6, background = rev(c("#142c5f", "#124663", "#2e6a57", "#f6a895", "#ffbad7"))) |>
  save_kable("table_output.html")

# Capture the HTML file as a PNG image
webshot("table_output.html", file = "table_output.png", zoom = 3)
