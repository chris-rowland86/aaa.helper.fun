# helper.fun
Functions that make my life easier
=================================

## Installation

```r
# Install from local source
devtools::install("packages/helper.fun")
```

## Table of Contents

- [Color Palettes](#color-palettes)
- [ggplot2 Theme](#ggplot2-theme)
- [Date Helpers](#date-helpers)
- [Data Import/Export](#data-importexport)
- [PowerPoint Helpers](#powerpoint-helpers)
- [Table Helpers](#table-helpers)

---

## Color Palettes

### Usage

```r
library(helper.fun)
library(ggplot2)

# View all available TG corporate colors
tg_corp_color()

# Get specific colors
tg_corp_color("green", "royal_blue")

# View available palettes
tg_corp_palette("all")

# Use in ggplot2
ggplot(mtcars, aes(x = mpg, y = hp, color = factor(cyl))) +
  geom_point(size = 3) +
  scale_color_tg()

# Fill scale
ggplot(mtcars, aes(x = factor(cyl), fill = factor(cyl))) +
  geom_bar() +
  scale_fill_tg(palette = "ae")
```

### Available Palettes

| Palette | Colors | Use Case |
|---------|--------|----------|
| `main` | green, royal_blue, navy_blue | Primary visualizations |
| `highlight` | green, ice_blue | Highlighting key data |
| `ae` | sky_blue, warm_gray, yellow, red | Adverse event severity |

Each individual color (e.g., `"ice_blue"`, `"green"`) also has a ramped hue palette available.

### Color Reference

| Color | Hex Code |
|-------|----------|
| ice_blue | #BBCBD3 |
| navy_blue | #121B4D |
| green | #99CC33 |
| sky_blue | #3EA7F3 |
| royal_blue | #003494 |
| warm_gray | #909FA7 |
| yellow | #FEE568 |
| red | #FF2600 |

### Functions

- `tg_corp_color()` - Extract TG colors as hex codes
- `ramp_hues()` - Generate color ramps from a center color
- `tg_corp_palette()` - Get predefined color palettes
- `palette_gen()` - Generate a palette function for n colors
- `scale_color_tg()` - ggplot2 color scale
- `scale_fill_tg()` - ggplot2 fill scale

---

## ggplot2 Theme

A custom ggplot2 theme with TG Therapeutics styling featuring Arial font, centered titles, clean backgrounds, and professional axis formatting.

### Usage

```r
library(ggplot2)

# Apply TG theme to a plot
ggplot(mtcars, aes(x = mpg, y = hp)) +
  geom_point() +
  theme_tg()

# Use with different base size
ggplot(mtcars, aes(x = mpg, y = hp)) +
  geom_point() +
  theme_tg(base_size = 14)

# Apply theme globally to all plots
theme_set(theme_tg())
```

### Functions

- `theme_tg()` - Custom ggplot2 theme with TG styling (base_size = 18, Arial font)

---

## Date Helpers

Utility functions for working with dates, particularly useful for clinical trial data with partial dates.

### Usage

```r
# Get last day of month
last_day(as.Date("2024-02-15"))
# Returns 2024-02-29 (leap year)

# Complete partial start dates (imputes earliest possible date)
complete_start_date("UNK", "03", "2024")  # Unknown day -> 1st
complete_start_date("UNK", "UNK", "2024") # Unknown month -> January 1st

# Complete partial end dates (imputes latest possible date)
complete_end_date("UNK", "02", "2024")    # Unknown day -> last day of month (Feb 29)
complete_end_date("UNK", "UNK", "2024")   # Unknown month -> December 31st

# Convert days to other units
days_to_months(365)  # Returns 11.99
days_to_years(365)   # Returns 1.00

# Convert HH:MM duration to minutes
hhmm_to_min(as.ITime("01:30"))  # Returns 90
```

### Functions

| Function | Description |
|----------|-------------|
| `last_day()` | Get the last day of the month for a given date |
| `complete_start_date()` | Build complete start dates from partial components (imputes earliest) |
| `complete_end_date()` | Build complete end dates from partial components (imputes latest) |
| `days_to_months()` | Convert days to months (using 30.4375 days/month) |
| `days_to_years()` | Convert days to years (using 365.25 days/year) |
| `hhmm_to_min()` | Convert ITime HH:MM duration to total minutes |

---

## Data Import/Export

Functions for importing data from various file formats and exporting to Excel.

### Usage

```r
# Import Excel file
import_excel_files_to_variables(
  folder = "data",
  file_name = "my_data.xlsx",
  sheet = "Sheet1"
)
# The data is now available as `my_data` in the global environment

# Import SAS file
import_sas_files_to_variables(folder = "data", file_name = "my_data.sas7bdat")

# Import XPT file
import_xpt_files_to_variables(folder = "data", file_name = "my_data.xpt")

# Import CSV file
import_csv_files_to_variables(folder = "data", file_name = "my_data.csv")

# Import from Microsoft Access
conn <- RODBC::odbcDriverConnect("Driver={Microsoft Access Driver};DBQ=path/to/db.accdb")
import_access_tables_to_variables(connection = conn, table_name = "MyTable")

# Export to Excel
export_to_excel(mtcars, "car_data")
# Creates "output-files/car_data.xlsx"
```

### Functions

| Function | Description |
|----------|-------------|
| `import_excel_files_to_variables()` | Import Excel file to global environment |
| `import_sas_files_to_variables()` | Import SAS .sas7bdat file to global environment |
| `import_xpt_files_to_variables()` | Import XPT file to global environment |
| `import_csv_files_to_variables()` | Import CSV file to global environment |
| `import_access_tables_to_variables()` | Import Access table to global environment |
| `export_to_excel()` | Export data.table/data.frame to Excel |

> **Note:** All import functions automatically clean column names using `janitor::clean_names()` and convert data to `data.table`.

---

## PowerPoint Helpers

Functions for creating PowerPoint presentations with editable ggplot2 graphics using the `officer` and `rvg` packages.

### Usage

```r
library(ggplot2)
library(officer)

# Initialize presentation from template
pptx <- init_pptx("template.pptx")

# Add title slide
pptx <- add_title_slide(
  pptx,
  title = "My Presentation Title",
  cutoff_date_f = "01-Jan-2024"
)

# Create plot and convert to editable RVG
p <- ggplot(mtcars, aes(x = mpg, y = hp)) +
  geom_point() +
  theme_tg()

rvg_plot <- convert_to_rvg(p)

# Add plot to presentation
pptx <- add_to_pptx(pptx, rvg_plot, title = "MPG vs Horsepower")

# Save presentation
print(pptx, target = "output.pptx")
```

### Functions

| Function | Description |
|----------|-------------|
| `init_pptx()` | Initialize empty PowerPoint from template (removes all existing slides) |
| `add_title_slide()` | Add formatted title slide with title and cutoff date |
| `convert_to_rvg()` | Convert ggplot to editable RVG graphic for PowerPoint |
| `add_to_pptx()` | Add RVG graphic to new slide with formatted title |

---

## Table Helpers

Functions for formatting and exporting tables, particularly useful with `gtsummary` and `flextable`.

### Usage

```r
library(flextable)
library(gtsummary)

# Replace NA and zero values with em dashes
na_to_dash(c("5 (10%)", "0 (0%)", "NA", "10 (20%)"))
# Returns: "5 (10%)", "—", "—", "10 (20%)"

# Create and format a summary table
tbl <- mtcars |>
  tbl_summary(include = c(mpg, hp, wt)) |>
  as_flex_table() |>
  format_flextable()

# Export formatted flextable to Excel (preserves background colors)
export_flextable_to_excel(tbl, "output/summary_table.xlsx")
```

### Functions

| Function | Description |
|----------|-------------|
| `na_to_dash()` | Replace NA, "0 (0%)", and Inf values with em dash (—) |
| `format_flextable()` | Apply TG styling to flextable (ice blue borders, Arial font) |
| `export_flextable_to_excel()` | Export flextable to Excel preserving formatting |

### format_flextable() Features

- Removes all borders, adds horizontal lines in ice blue (#BBCBD3)
- Sets Arial font at 12pt (8pt for footer)
- Centers vertical alignment
- Fixed column width layout