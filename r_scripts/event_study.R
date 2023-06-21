# create empty list for regression output ---------------------------------

if (reg_type == "abate" || reg_type == "abate_diff") {
        models <- list()
}



# create empty table for cumulative abnormal returns ----------------------

car_table <- data.frame(
        matrix(
                ncol = length(event_names),
                nrow = ev_length_post + ev_length_pre + 1
        )
) %>%
        setNames(glue("car_{event_names}")) %>%
        mutate(day = -ev_length_pre:ev_length_post)



# Loop across all events --------------------------------------------------

for (event in event_names) {



# Set new var names -------------------------------------------------------

        dev <- glue("dev_{event}") # deviation from event days
        ev_win <- glue("ev_win_{event}") # event windows
        est_win <- glue("est_win_{event}") # estimation windows
        nr <- glue("nr_{event}") # normal returns
        ar <- glue("ar_{event}") # abnormal returns
        # abnormal returns (sum event window)
        ar_ew_uw <- glue("ar_ew_uw_{event}")
        # abnormal returns (sum pre event)
        ar_pre_uw <- glue("ar_pre_uw_{event}")
        ar_ev_uw <- glue("ar_ev_uw_{event}") # abnormal returns (event day)
        # abnormal returns (sum post event)
        ar_post_uw <- glue("ar_post_uw_{event}")
        car <- glue("car_{event}") # abnormal returns (rolling sum)

        # weighted var names
        if (weights == TRUE) {
                ar_ew_w <- glue("ar_ew_w_{event}")
                ar_pre_w <- glue("ar_pre_w_{event}")
                ar_ev_w <- glue("ar_ev_w_{event}")
                ar_post_w <- glue("ar_post_w_{event}")
                car_w <- glue("car_w_{event}")
                weight <- phase_out[event, "emissions_weights"]
        }



# Create new vars for event study -----------------------------------------

        event_date <- phase_out[event, "event_date"] # get event dates

        event_row <- which(event_date == data$date) # row of event day

        data %<>%

                # create deviation from event day var
                mutate({{ dev }} := row_number() - event_row) %>%
                # create var indicating event window
                mutate({{ ev_win }} := as.integer(between(
                        get(dev),
                        -ev_length_pre, # lower bound of ev window
                        ev_length_post
                ))) %>% # upper bound of ev window

                # create var for estimation window
                mutate({{ est_win }} := as.integer(between(
                        get(dev),
                        # lower bound est window
                        -est_length - ev_length_pre,
                        # upper bound of est window
                        -ev_length_pre - 1
                )))



# NR using the constant mean model ----------------------------------------

        if (reg_type == "constant") {
                data %<>%
                        # NR equal to avg return in est_win
                        mutate({{ nr }} :=
                                mean(ln_return_eua[get({{ est_win }}) == 1]))
        }



# NR using two versions of the abatement cost model -----------------------

        if (reg_type == "abate" || reg_type == "abate_diff") {
                data %<>%
                        # create a duplicate version of ln_return_eua
                        # needed for the recursive prediction
                        mutate(ln_return_eua_2 = if_else(
                                get(dev) < ev_length_pre,
                                ln_return_eua,
                                NA
                        ))
        }

        # run the regressions for the main model
        if (reg_type == "abate") {
                models[[event]] <- lm_robust(
                        ln_return_eua_2 ~
                                lag(ln_return_eua_2) + ln_return_Oil +
                                ln_return_Coal + ln_return_Gas +
                                ln_return_Elec + ln_return_GSCI +
                                ln_return_VIX + ln_return_STOXX +
                                ln_return_Diff_BAA_AAA + ln_return_ECB_Spot_3M,
                        data = data,
                        subset = get(est_win) == 1,
                        se_type = "stata"
                )
        }

        # run same regression but use diff. log returns for coal and gas
        if (reg_type == "abate_diff") {
                models[[event]] <- lm_robust(
                        ln_return_eua_2 ~
                                lag(ln_return_eua_2) + ln_return_Oil +
                                d_ln_return_Coal + d_ln_return_Gas +
                                ln_return_Elec + ln_return_GSCI +
                                ln_return_VIX + ln_return_STOXX +
                                ln_return_Diff_BAA_AAA + ln_return_ECB_Spot_3M,
                        data = data,
                        subset = get(est_win) == 1,
                        se_type = "stata"
                )
        }


        if (reg_type == "abate" || reg_type == "abate_diff") {

                data %<>%
                        # calculate fitted values during the estimation window
                        mutate({{ nr }} := if_else(
                                get(est_win) == 1,
                                predict(
                                        models[[event]],
                                        newdata = .
                                ),
                                NA
                        ))

                # calculate NR during the event window
                # this is more complicated as it is a recursive prediction
                # the ln_return_eua_2 gets updated using prior predictions
                for (i in (-ev_length_pre):(ev_length_post)) {
                        data %<>%
                                mutate(
                                        # normal returns
                                        {{ nr }} := if_else(
                                                get(dev) == i,
                                                predict(
                                                        models[[event]],
                                                        newdata = .
                                                ),
                                                get(nr)
                                        ),
                                        # do not use actual eua returns
                                                # in the event window
                                        # use predicted returns instead
                                        ln_return_eua_2 = if_else(
                                                get(dev) == i,
                                                get(nr),
                                                ln_return_eua_2
                                        )
                                )
                }

                 # remove temp variable
                 data %<>%
                         select(-ln_return_eua_2)
        }



# Abnormal Returns --------------------------------------------------------

        data %<>%
                # abnormal returns = observed returns - normal returns
                mutate({{ ar }} := ln_return_eua - get(nr))



# Summed + cumulative abnormal returns ------------------------------------

        data %<>%
                mutate(
                        {{ ar_pre_uw }} :=
                                sum(get(ar)[between(
                                        get(dev), -ev_length_pre, -1)]),
                        {{ ar_ev_uw }} :=
                                sum(get(ar)[get(dev) == 0]),
                        {{ ar_post_uw }} :=
                                sum(get(ar)[between(
                                        get(dev), 1, ev_length_post)]),
                        {{ ar_ew_uw }} :=
                                sum(get(ar)[get(ev_win) == 1])) %>%
                group_by(get(ev_win)) %>%
                # create CAR (rolling sum over the entire event window)
                mutate(
                        {{ car }} :=
                                cumsum(get(ar))) %>%
                ungroup()



# get CAR into a separate table -------------------------------------------

        # reduce table to CAR only for one event
        car_temp <- data %>%
                filter(get(ev_win) == 1) %>%
                select({{car}}) %>%
                mutate(day = -ev_length_pre:ev_length_post)

        # merge the table above with the final car table
        car_table <- left_join(
                car_temp,
                car_table,
                by = "day",
                suffix = c("", ".y")
        )



# weighted versions of summed AR and CAR ----------------------------------

        if (weights == TRUE) {
                data %<>%
                        mutate(
                                {{ ar_pre_w }} := get(ar_pre_uw) * weight,
                                {{ ar_ev_w }} := get(ar_ev_uw) * weight,
                                {{ ar_post_w }} := get(ar_post_uw) * weight,
                                {{ ar_ew_w }} := get(ar_ew_uw) * weight,
                                {{ car_w }} := get(car) * weight
                        )

                # reduce table to CAR only for one event
                car_temp <- data %>%
                        filter(get(ev_win) == 1) %>%
                        select({{car_w}}) %>%
                        #filter(!is.na(get(car_w))) %>%
                        mutate(day = -ev_length_pre:ev_length_post)

                # merge the table above with the final car table
                car_table <- left_join(
                        car_temp,
                        car_table,
                        by = "day",
                        suffix = c("", ".y")
                )

        }
}

# improve car_table
car_table %<>%
        select(-ends_with(".y")) %>%
        select("day", order(colnames(.)))



# Average AR summed across different parts of the event window ------------

        data %<>%

                mutate(
                        ar_pre_uw_avg =
                                rowMeans(across(starts_with("ar_pre_uw"))),
                        ar_ev_uw_avg =
                                rowMeans(across(starts_with("ar_ev_uw"))),
                        ar_post_uw_avg =
                                rowMeans(across(starts_with("ar_post_uw"))),
                        ar_ew_uw_avg =
                                rowMeans(across(starts_with("ar_ew_uw")))
                )


if (weights == TRUE) {
        data %<>%

                mutate(
                        ar_pre_w_avg =
                                rowSums(across(starts_with("ar_pre_w"))),
                        ar_ev_w_avg =
                                rowSums(across(starts_with("ar_ev_w"))),
                        ar_post_w_avg =
                                rowSums(across(starts_with("ar_post_w"))),
                        ar_ew_w_avg =
                                rowSums(across(starts_with("ar_ew_w")))
                )
}



# Average CAR -------------------------------------------------------------

# non-weighted average CAR
car_table %<>%
        mutate(car_avg = rowMeans(
                pick(
                        starts_with("car_") &
                                -starts_with("car_w_")
                )
        ))
# weighted average CAR
if (weights == TRUE) {
        car_table %<>%
                mutate(car_w_avg = rowSums(pick(starts_with("car_w_"))))
}


# Drop unnecessary objects
rm(dev, ev_win, est_win, event_date, event, event_row, nr, i, weight)
rm(list = ls(pattern = "(^ar)|(^car)"))