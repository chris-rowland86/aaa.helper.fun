# R Clinical Analysis Conventions

## Language & Framework
- **All code must be written in R**. Do not use Python, SAS, or any other language.
- Projects use **R/Quarto** (`.qmd` files) to produce HTML dashboards, PowerPoint decks, and Excel exports.
- Install packages via `librarian::shelf()`. Define project root with `here::i_am()`.

## aaa.helper.fun Package (MANDATORY)
The `aaa.helper.fun` package (`chris-rowland86/aaa.helper.fun`) is the **single source of truth** for reusable functions. Before writing any new utility function:
1. **Check** if `aaa.helper.fun` already exports it
2. If it does, **use it** — do not rewrite or duplicate
3. If it does not, and the function would be useful across projects, **add it to `aaa.helper.fun`** with roxygen2 docs and `@export`, then use it from the package

### Exported Functions Reference
| Category | Functions |
|----------|-----------|
| **Colors** | `tg_corp_color()`, `tg_corp_palette()`, `palette_gen()`, `scale_color_tg()`, `scale_fill_tg()`, `ramp_hues()` |
| **Theme** | `theme_tg()` — apply to all ggplots; set globally with `theme_set(theme_tg())` |
| **Dates** | `complete_start_date()`, `complete_end_date()`, `last_day()`, `days_to_months()`, `days_to_years()`, `hhmm_to_min()` |
| **Import** | `import_access_tables_to_variables()`, `import_excel_files_to_variables()`, `import_csv_files_to_variables()`, `import_sas_files_to_variables()`, `import_xpt_files_to_variables()`, `apply_column_map()` |
| **Export** | `export_to_excel()`, `export_flextable_to_excel()` |
| **PPTX** | `init_pptx()`, `add_to_pptx()`, `add_title_slide()`, `convert_to_rvg()` |
| **Tables** | `format_flextable()`, `na_to_dash()`, `format_incidence_percent()` |

All import functions store data as `data.table` in `.GlobalEnv` with `janitor::clean_names()` and automatically append a **`_source`** suffix to the variable name (e.g., importing `ae.xlsx` creates `ae_source`).

---

## data.table Conventions (STRICTLY ENFORCED)

### Source / Clean Naming Convention
- Imported data must be stored in a `data.table` with a **`_source`** suffix (e.g., `dt_source`). This is the **read-only** copy of the original imported data and must **never be modified**.
- Immediately after import, create a working copy with a **`_clean`** suffix: `dt_clean <- copy(dt_source)`.
- **All** data processing, transformations, and derivations are performed on the `_clean` table — never on `_source`.
- This preserves the original imported data for auditing, comparison, and re-derivation at any point.
```r
# Example workflow
# Import creates ae_source in .GlobalEnv automatically (with _source suffix)
import_excel_files_to_variables("data", "ae.xlsx", "Sheet1")

# Create working copy and apply standard column names
ae_clean <- copy(ae_source)
apply_column_map(ae_clean, domain = "ae")

# All processing uses _clean with standard column names
ae_clean[, ae_stdt := as.IDate(ae_stdt, format = "%Y-%m-%d")]
ae_clean[, duration := days_to_months(duration_days)]
```

- **ALL** data manipulation uses `data.table` syntax: `data[i, j, by]`. **Never use dplyr** (`mutate`, `filter`, `select`, `group_by`, etc.).
- Column selection with external vectors: `data[, ..cols]` (note `..` prefix)
- Update by reference: `data[, new_col := calculation]` — never reassign the whole table
- Chaining: `data[...] %>% .[...]` using `%>%` pipe between data.table operations
- Keys: `setkey(data, subjid)` for indexed joins/sorts
- Non-equi joins for window-based matching:
```r
data[other, on = .(subjid, date >= window_st, date <= window_en)]
```
- Dates: `IDate` (not `Date` or `POSIXct`); Times: `ITime` (integer-based HH:MM:SS)
- Unknown date imputation: use `complete_start_date()` / `complete_end_date()` from `aaa.helper.fun`

---

## Triple Export Pattern
Every summary table and figure must be exported to **three formats simultaneously**:

1. **HTML**: Display inline in dashboard via `gt`, `reactable`, or raw ggplot
2. **PowerPoint**: Tables → `as_flex_table() |> format_flextable() |> add_to_pptx()`; Plots → `convert_to_rvg() |> add_to_pptx()`
3. **Excel**: Data listings via `export_to_excel()` or `writexl::write_xlsx()`

---

## PowerPoint Export (Single Accumulated Object)
- Initialize ONE `officer` PPTX object at pipeline start: `editable_plots <- init_pptx()`
- Add a title slide: `editable_plots <- add_title_slide(editable_plots, title, cutoff_date_f)`
- Throughout the pipeline, add slides to this SAME object: `editable_plots <- add_to_pptx(editable_plots, rvg_graphic, "Slide Title")`
- **Do NOT create separate PPTX files** per table or figure
- Export once at the end: `print(editable_plots, target = "output.pptx")`

For tables: `gtsummary` → `as_flex_table()` → `format_flextable()` → add to PPTX via `officer::ph_with()` with `ph_location_type(type = "body")`
For multi-object slides: use `type_idx` parameter in `ph_location_type()` (not deprecated `id`)

---

## Summary Tables (gtsummary Pattern)
```r
data |>
    tbl_summary(by = strat_var, label = list(var ~ "Label"), missing = "no") |>
    modify_caption("**Title**") |>
    modify_footnote(all_stat_cols() ~ "context") |>
    bold_labels()
# Convert: as_flex_table() |> format_flextable() for PPTX
# AE tables: use gtreg::tbl_ae() for unique-patient denominators
```

---

## Visualization Pattern
All plotting functions should accept: `data`, `y_var`, `caption`, and export parameters. Build with `ggplot2` + `theme_tg()`. Use:
- `position_jitter(seed = 123)` for reproducible jitter
- `geom_text_repel()` for labels (always with `na.rm = TRUE`)
- Corporate colors: `tg_corp_color()`, `scale_color_tg()`, `scale_fill_tg()`
- Export: `convert_to_rvg(plot) |> add_to_pptx(editable_plots, rvg, "Title")`

Broken y-axis: stack two ggplots via `patchwork` (not `ggbreak`).
Faceted plots: add dummy rows for empty facets via `rbind(..., fill = TRUE)`.

---

## Data Quality Checks
- Render data issue tables as **interactive `reactable`** tables in HTML output
- Use sticky first column (typically `subjid`), searchable/filterable, with conditional styling
- Export same data as Excel listings for offline review

---

## Clinical Data Standards

### Column Mapping Convention
- **`_source` tables preserve original column names** — never modify or rename columns in `_source` data.
- When creating the `_clean` copy, apply `apply_column_map()` from `aaa.helper.fun` to rename key source-specific columns to **predefined standard variable names**.
- Unmapped columns pass through unchanged from `_source` to `_clean`.
- Each project maintains a **central lookup CSV** at `data/column_map.csv` with columns:

| `domain` | `source_col` | `standard_col` |
|----------|-------------|----------------|
| ae | aeterm | ae_term |
| ae | aestdtc | ae_stdt |
| ae | aeendtc | ae_endt |
| ae | aesev | ae_sev |
| dm | subjid_edc | subjid |
| dm | arm | trt |
| lb | lbtestcd | param |
| lb | lborres | param_val |

- `domain` matches the dataset prefix (e.g., `"ae"`, `"dm"`, `"lb"`, `"cm"`)
- `source_col` references column names **after** `janitor::clean_names()` (lowercase/snake_case)
- `standard_col` is a predefined standard name from the canonical list below

### Standard Variable Names (Canonical List)
| Category | Standard Name | Description |
|----------|--------------|-------------|
| **Identity** | `subjid` | Subject identifier (format: `XXX-NNN`) |
| | `siteid` | Site identifier |
| **Visit** | `visit` | Original visit text |
| | `visit_c` | Abbreviated visit code (e.g., "w1d1", "w24") |
| | `visit_n` | Numeric visit/week number |
| | `visit_dt` | Visit date |
| **Treatment** | `trt` | Treatment/arm assignment |
| | `trt_dt` | Treatment start date |
| **Adverse Events** | `ae_term` | AE verbatim term |
| | `ae_term_pt` | AE preferred term (MedDRA) |
| | `ae_sev` | AE severity |
| | `ae_ser` | AE serious (Y/N) |
| | `ae_rel` | AE relationship to treatment |
| | `ae_stdt` | AE start date |
| | `ae_endt` | AE end date |
| | `ae_out` | AE outcome |
| **Labs/Vitals** | `param` | Parameter name |
| | `param_val` | Parameter value (numeric) |
| | `param_unit` | Parameter unit |
| | `param_dt` | Parameter collection date |
| **Con Meds** | `cm_name` | Concomitant medication name |
| | `cm_stdt` | Con med start date |
| | `cm_endt` | Con med end date |
| **Milestones** | `rand_dt` | Randomization date |
| | `icf_dt` | Informed consent date |

*This list is extensible — add project-specific standard names as needed.*
- Primary key: `subjid` (subject identifier, format: `XXX-NNN`)
- Visit variables: `visit` (original text), `visit_c` (abbreviated: "w1d1", "w24", "w48_eos"), `visit_n` (numeric week)
- `cutoff_date` must be respected in every summary/figure — flag post-cutoff data with `new_data_fl`
- Protocol deviations: store as hard-coded `data.table` corrections, merge via `data[correction, on = .(subjid), col := i.col]`

---

## Common Pitfalls
- **`:=` not `=`** for data.table in-place modification
- **`type_idx`** not `id` in `officer::ph_location_type()` (deprecated in officer v0.6.7+)
- **Faceted plots need dummy rows** to ensure all panels appear even with no data
- **`visit_c` factor levels** are determined by `visit_n` — never manually reorder
- **Broken axis plots** intentionally exclude gap points — warnings are expected
- **Heterogeneous lab data** (Eurofins LOINC vs Labcorp test codes) requires cross-reference mapping
