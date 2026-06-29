test_that("parse_sas_data_step extracts file, columns, and format types", {
    block <- c(
        "DATA TGTH004.EX (LABEL='EX export table');",
        "INFILE \"&DataPath\\EX.dat\" encoding=\"utf-8\" DLM='|' DSD MISSOVER lrecl=453;",
        "ATTRIB EDC_ROWGUID length=$36 label='Row Identifier'",
        "SUBJID length=$7 label='Subject Identifier'",
        "EXSTDAT format=DATE9. label='Start Date'",
        "EXSTTIM format= TIME8. label='Start Time'",
        "EDC_EntryDate format=DATETIME16. label='Entry Date'",
        ";",
        "INPUT EDC_ROWGUID$",
        "SUBJID$",
        "EXSTDAT",
        "EXSTTIM",
        "EDC_EntryDate",
        ";",
        "RUN;"
    )

    def <- parse_sas_data_step(block)

    expect_equal(def$dat_file, "EX.dat")
    expect_equal(
        def$column_names,
        c("EDC_ROWGUID", "SUBJID", "EXSTDAT", "EXSTTIM", "EDC_EntryDate")
    )
    # DATETIME must not be mis-classified as DATE, and TIME must be distinct
    expect_equal(def$date_vars, "EXSTDAT")
    expect_equal(def$time_vars, "EXSTTIM")
    expect_equal(def$datetime_vars, "EDC_EntryDate")
})

test_that("parse_sas_data_step handles steps without an ATTRIB block", {
    block <- c(
        "DATA LIB.Subjects (LABEL='Subjects');",
        "INFILE \"&DataPath\\Subjects.dat\" DLM='|' DSD MISSOVER lrecl=52;",
        "INPUT SiteID$",
        "SubjectLabel$",
        ";",
        "RUN;"
    )

    def <- parse_sas_data_step(block)

    expect_equal(def$dat_file, "Subjects.dat")
    expect_equal(def$column_names, c("SiteID", "SubjectLabel"))
    expect_length(def$date_vars, 0)
    expect_length(def$datetime_vars, 0)
    expect_length(def$time_vars, 0)
})

test_that("convert_sas_date_columns converts SAS dates, datetimes, and times", {
    dt <- data.table::data.table(
        EXSTDAT = c(23505L, NA_integer_),
        EXSTTIM = c(28800L, 0L),
        EDC_EntryDate = c(2057398304, NA_real_)
    )
    def <- list(
        date_vars = "EXSTDAT",
        datetime_vars = "EDC_EntryDate",
        time_vars = "EXSTTIM"
    )

    convert_sas_date_columns(dt, def)

    # SAS date: days since 1960-01-01 -> IDate
    expect_s3_class(dt$EXSTDAT, "IDate")
    expect_equal(
        dt$EXSTDAT[1],
        data.table::as.IDate(as.Date(23505, origin = "1960-01-01"))
    )
    expect_true(is.na(dt$EXSTDAT[2]))

    # SAS time: seconds since midnight -> ITime (28800s == 08:00:00)
    expect_s3_class(dt$EXSTTIM, "ITime")
    expect_equal(as.integer(dt$EXSTTIM[1]), 28800L)

    # SAS datetime: seconds since 1960-01-01 -> POSIXct (UTC)
    expect_s3_class(dt$EDC_EntryDate, "POSIXct")
    expect_equal(
        dt$EDC_EntryDate[1],
        as.POSIXct(2057398304, origin = "1960-01-01", tz = "UTC")
    )
})

test_that("convert_sas_date_columns ignores columns absent from the data", {
    dt <- data.table::data.table(SUBJID = c("X-001", "X-002"))
    def <- list(
        date_vars = "EXSTDAT",
        datetime_vars = character(0),
        time_vars = character(0)
    )

    expect_silent(convert_sas_date_columns(dt, def))
    expect_equal(dt$SUBJID, c("X-001", "X-002"))
})
