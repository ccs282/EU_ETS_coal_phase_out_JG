# Import CSV --------------------------------------------------------------

phase_out <- read_csv("data/phase_outs.csv") %>%
        t() %>% # transpose csv
        as.data.frame() # convert from matrix/array to df

# change colnames
names(phase_out) <- phase_out %>%
        head(1)



# Restructure table -------------------------------------------------------

phase_out %<>%
        # keep only relevant columns and rename
        select(
                event_date = "Use for Stata",
                include = "Use in analysis",
                emissions = "emissions at announcement"
        ) %>%
        # remove first row containing the colnames and non-included events
        filter(row_number() > 1, !is.na(include)) %>%
        # change data type to double
        mutate(across(
                .cols = everything(),
                .fns = as.double
        ))



# create emissions weights ------------------------------------------------

if (weights == TRUE) {
        phase_out %<>%
                # create emissions weights
                mutate(emissions_weights = emissions /
                        sum(
                                emissions[include == 1],
                                na.rm = TRUE
                        )) %>%
                # drop observations without weights
                filter(!is.na(emissions))
}

# get a char vector of the event names for future operations
event_names <- row.names(phase_out)



# change dates if there is no matching trading date -----------------------

phase_out %<>%
        left_join(.,
                data,
                by = join_by(closest(event_date <= date))
        ) %>%
        mutate(event_date = date)

if (weights == TRUE) {
        phase_out %<>%
                select(event_date, emissions, emissions_weights)
} else {
        phase_out %<>%
                select(event_date)
}

row.names(phase_out) <- event_names # assign event strings as row titles