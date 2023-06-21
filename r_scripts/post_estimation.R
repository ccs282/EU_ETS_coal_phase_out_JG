# Parameters needed for sd, t, p, ... -------------------------------------

# number of parameters in reg
if (reg_type == "abate" || reg_type == "abate_diff") {
        num_par <- 11
} else if (reg_type == "constant") {
        num_par <- 1
}

df <- est_length - num_par # degrees of freedom

number_events <- length(event_names) # number of events



# loop across all events --------------------------------------------------

for (event in event_names) {



# set var names -----------------------------------------------------------

        est_win <- glue("est_win_{event}") # estimation_windows
        ar <- glue("ar_{event}") # abnormal returns
        # variance of abnormal returns (estimation_window)
        var_est_uw <- glue("var_est_uw_{event}")
        # sd of abnormal returns (estimation_window)
        sd_est_uw <- glue("sd_est_uw_{event}")
        # variance of AR (event_window)
        var_ew_uw <- glue("var_ew_uw_{event}")
        # sd of AR (event_window)
        sd_ew_uw <- glue("sd_ew_uw_{event}")
        # variance of AR (pre_event_window)
        var_pre_uw <- glue("var_pre_uw_{event}")
        # sd of AR (pre_event_window)
        sd_pre_uw <- glue("sd_pre_uw_{event}")
        # variance of AR (post_event_window)
        var_post_uw <- glue("var_post_uw_{event}")
        # sd of AR (post_event_window)
        sd_post_uw <- glue("sd_post_uw_{event}")
        # variance of AR (event_day)
        var_ev_uw <- glue("var_ev_uw_{event}")
        # sd of AR (event_day)
        sd_ev_uw <- glue("sd_ev_uw_{event}")

        if (weights == TRUE) {
                var_ew_w <- glue("var_ew_w_{event}")
                var_pre_w <- glue("var_pre_w_{event}")
                var_post_w <- glue("var_post_w_{event}")
                var_ev_w <- glue("var_ev_w_{event}")
                sd_ew_w <- glue("sd_ew_w_{event}")
                sd_pre_w <- glue("sd_pre_w_{event}")
                sd_post_w <- glue("sd_post_w_{event}")
                sd_ev_w <- glue("sd_ev_w_{event}")
                weight <- phase_out[event, "emissions_weights"]
        }



# Variance and SD of the abnormal returns (estimation_window) -------------

        data %<>%
                mutate({{ var_est_uw }} := 1 / (est_length - num_par) *
                        sum(
                                get(ar)[get(est_win) == 1]^2
                        )) %>%
                mutate({{ sd_est_uw }} := sqrt(get(var_est_uw)))



# Variance and SD for different parts of the event window -----------------

        data %<>%
                mutate(
                        {{ var_ew_uw }} :=
                                (ev_length_pre + ev_length_post + 1)
                                * get(var_est_uw),
                        {{ var_pre_uw }} := ev_length_pre * get(var_est_uw),
                        {{ var_post_uw }} := ev_length_post * get(var_est_uw),
                        {{ var_ev_uw }} := get(var_est_uw)
                ) %>%
                mutate(
                        {{ sd_ew_uw }} := sqrt(get(var_ew_uw)),
                        {{ sd_pre_uw }} := sqrt(get(var_pre_uw)),
                        {{ sd_post_uw }} := sqrt(get(var_post_uw)),
                        {{ sd_ev_uw }} := sqrt(get(var_ev_uw))
                )

        # variance and sd of AR when using weights
        if (weights == TRUE) {
                data %<>%
                        mutate(
                                # whole event_window
                                {{ var_ew_w }} :=
                                        get(var_ew_uw) * weight^2,
                                # pre-event
                                {{ var_pre_w }} :=
                                        get(var_pre_uw) * weight^2,
                                # post-event
                                {{ var_post_w }} :=
                                        get(var_post_uw) * weight^2,
                                # event_day
                                {{ var_ev_w }} :=
                                        get(var_ev_uw) * weight^2
                        ) %>%
                        mutate(
                                {{ sd_ew_w }} := sqrt(get(var_ew_w)),
                                {{ sd_pre_w }} := sqrt(get(var_pre_w)),
                                {{ sd_post_w }} := sqrt(get(var_post_w)),
                                {{ sd_ev_w }} := sqrt(get(var_ev_w))
                        )
        }
}



# VAR & SD for average AR (weighted) --------------------------------------

if (weights == TRUE) {
        data %<>%
                mutate(
                        var_ew_w_avg =
                                rowSums(across(starts_with("var_ew_w_"))),
                        var_pre_w_avg =
                                rowSums(across(starts_with("var_pre_w_"))),
                        var_post_w_avg =
                                rowSums(across(starts_with("var_post_w_"))),
                        var_ev_w_avg =
                                rowSums(across(starts_with("var_ev_w_")))
                ) %>%
                mutate(
                        sd_ew_w_avg =
                                sqrt(var_ew_w_avg),
                        sd_pre_w_avg =
                                sqrt(var_pre_w_avg),
                        sd_post_w_avg =
                                sqrt(var_post_w_avg),
                        sd_ev_w_avg =
                                sqrt(var_ev_w_avg)
                )
}



# VAR & SD for average AR (unweighted) ------------------------------------

data %<>%
        mutate(
                var_ew_uw_avg =
                        rowSums(across(
                                starts_with("var_ew_uw")
                        )) /
                                number_events^2,
                var_pre_uw_avg =
                        rowSums(across(starts_with("var_pre_uw"))) /
                                number_events^2,
                var_post_uw_avg =
                        rowSums(across(starts_with("var_post_uw"))) /
                                number_events^2,
                var_ev_uw_avg =
                        rowSums(across(starts_with("var_ev_uw"))) /
                                number_events^2
        ) %>%
        mutate(
                sd_ew_uw_avg =
                        sqrt(var_ew_uw_avg),
                sd_pre_uw_avg =
                        sqrt(var_pre_uw_avg),
                sd_post_uw_avg =
                        sqrt(var_post_uw_avg),
                sd_ev_uw_avg =
                        sqrt(var_ev_uw_avg)
        )



# loop across events ------------------------------------------------------

for (event in event_names) {



# set var names -----------------------------------------------------------

        ar_ew_uw <- glue("ar_ew_uw_{event}")
        ar_pre_uw <- glue("ar_pre_uw_{event}")
        ar_post_uw <- glue("ar_post_uw_{event}")
        ar_ev_uw <- glue("ar_ev_uw_{event}")
        sd_ew_uw <- glue("sd_ew_uw_{event}")
        sd_pre_uw <- glue("sd_pre_uw_{event}")
        sd_post_uw <- glue("sd_post_uw_{event}")
        sd_ev_uw <- glue("sd_ev_uw_{event}")
        t_ew_uw <- glue("t_ew_uw_{event}")
        t_pre_uw <- glue("t_pre_uw_{event}")
        t_post_uw <- glue("t_post_uw_{event}")
        t_ev_uw <- glue("t_ev_uw_{event}")
        p_ew_uw <- glue("p_ew_uw_{event}")
        p_pre_uw <- glue("p_pre_uw_{event}")
        p_post_uw <- glue("p_post_uw_{event}")
        p_ev_uw <- glue("p_ev_uw_{event}")

        if (weights == TRUE) {
                ar_ew_w <- glue("ar_ew_w_{event}")
                ar_pre_w <- glue("ar_pre_w_{event}")
                ar_post_w <- glue("ar_post_w_{event}")
                ar_ev_w <- glue("ar_ev_w_{event}")
                sd_ew_w <- glue("sd_ew_w_{event}")
                sd_pre_w <- glue("sd_pre_w_{event}")
                sd_post_w <- glue("sd_post_w_{event}")
                sd_ev_w <- glue("sd_ev_w_{event}")
                t_ew_w <- glue("t_ew_w_{event}")
                t_pre_w <- glue("t_pre_w_{event}")
                t_post_w <- glue("t_post_w_{event}")
                t_ev_w <- glue("t_ev_w_{event}")
                p_ew_w <- glue("p_ew_w_{event}")
                p_pre_w <- glue("p_pre_w_{event}")
                p_post_w <- glue("p_post_w_{event}")
                p_ev_w <- glue("p_ev_w_{event}")
        }



# unweighted t-stats and p-values -----------------------------------------

        data %<>%
                # calculate t-statistics
                mutate(
                        {{ t_ew_uw }} := get(ar_ew_uw) / get(sd_ew_uw),
                        {{ t_pre_uw }} := get(ar_pre_uw) / get(sd_pre_uw),
                        {{ t_post_uw }} := get(ar_post_uw) / get(sd_post_uw),
                        {{ t_ev_uw }} := get(ar_ev_uw) / get(sd_ev_uw)
                ) %>%
                # calculate p-values
                mutate(
                        {{ p_ew_uw }} := 2 * pt(
                                abs(get(t_ew_uw)),
                                df = df,
                                lower.tail = FALSE
                        ),
                        {{ p_pre_uw }} := 2 * pt(
                                abs(get(t_pre_uw)),
                                df = df,
                                lower.tail = FALSE
                        ),
                        {{ p_post_uw }} := 2 * pt(
                                abs(get(t_post_uw)),
                                df = df,
                                lower.tail = FALSE
                        ),
                        {{ p_ev_uw }} := 2 * pt(
                                abs(get(t_ev_uw)),
                                df = df,
                                lower.tail = FALSE
                        )
                )



# weighted t-stats and p-values -------------------------------------------

        if (weights == TRUE) {
                data %<>%
                        # calculate t-statistics
                        mutate(
                                {{ t_ew_w }} := get(ar_ew_w) /
                                        get(sd_ew_w),
                                {{ t_pre_w }} := get(ar_pre_w) /
                                        get(sd_pre_w),
                                {{ t_post_w }} := get(ar_post_w) /
                                        get(sd_post_w),
                                {{ t_ev_w }} := get(ar_ev_w) /
                                        get(sd_ev_w)
                        ) %>%
                        # calculate p-values
                        mutate(
                                {{ p_ew_w }} := 2 * pt(
                                        abs(get(t_ew_w)),
                                        df = df,
                                        lower.tail = FALSE
                                ),
                                {{ p_pre_w }} := 2 * pt(
                                        abs(get(t_pre_w)),
                                        df = df,
                                        lower.tail = FALSE
                                ),
                                {{ p_post_w }} := 2 * pt(
                                        abs(get(t_post_w)),
                                        df = df,
                                        lower.tail = FALSE
                                ),
                                {{ p_ev_w }} := 2 * pt(
                                        abs(get(t_ev_w)),
                                        df = df,
                                        lower.tail = FALSE
                                )
                        )
        }
}

rm(list = ls(pattern = "(^p_)|(^t_)|(^sd_)|(^var_)|(^ar)"))
rm(est_win, event, weight)



# p-values and t-stats for AVG abnormal returns (unweighted) --------------

data %<>%
        # t-stats
        mutate(
                t_ew_uw_avg = ar_ew_uw_avg / sd_ew_uw_avg,
                t_pre_uw_avg = ar_pre_uw_avg / sd_pre_uw_avg,
                t_post_uw_avg = ar_post_uw_avg / sd_post_uw_avg,
                t_ev_uw_avg = ar_ev_uw_avg / sd_ev_uw_avg
        ) %>%
        # p-values
        mutate(
                p_ew_uw_avg = 2 * pt(
                        abs(t_ew_uw_avg),
                        df = df,
                        lower.tail = FALSE
                ),
                p_pre_uw_avg = 2 * pt(
                        abs(t_pre_uw_avg),
                        df = df,
                        lower.tail = FALSE
                ),
                p_post_uw_avg = 2 * pt(
                        abs(t_post_uw_avg),
                        df = df,
                        lower.tail = FALSE
                ),
                p_ev_uw_avg = 2 * pt(
                        abs(t_ev_uw_avg),
                        df = df,
                        lower.tail = FALSE
                )
        )



# p-values and t-stats for AVG abnormal returns (weighted) ----------------

if (weights == TRUE) {
        data %<>%
                # t-stats
                mutate(
                        t_ew_w_avg = ar_ew_w_avg / sd_ew_w_avg,
                        t_pre_w_avg = ar_pre_w_avg / sd_pre_w_avg,
                        t_post_w_avg = ar_post_w_avg / sd_post_w_avg,
                        t_ev_w_avg = ar_ev_w_avg / sd_ev_w_avg
                ) %>%
                # p-values
                mutate(
                        p_ew_w_avg = 2 * pt(
                                abs(t_ew_w_avg),
                                df = df,
                                lower.tail = FALSE
                        ),
                        p_pre_w_avg = 2 * pt(
                                abs(t_pre_w_avg),
                                df = df,
                                lower.tail = FALSE
                        ),
                        p_post_w_avg = 2 * pt(
                                abs(t_post_w_avg),
                                df = df,
                                lower.tail = FALSE
                        ),
                        p_ev_w_avg = 2 * pt(
                                abs(t_ev_w_avg),
                                df = df,
                                lower.tail = FALSE
                        )
                )
}



# Move scalars into separate data frames ----------------------------------

results_all <- data %>%
        select(
                starts_with("ar_pre"),
                starts_with("ar_post"),
                starts_with("ar_ew"),
                starts_with("ar_ev"),
                starts_with("sd"),
                starts_with("var"),
                starts_with("t_"),
                starts_with("p_")
        ) %>%
        pivot_longer(
                cols = everything(),
                names_to = c(".value", "time_section", "weighted", "event"),
                names_pattern = "([a-z]*)_([a-z]*)_([a-z]*)_(.*)",
                names_transform = list(
                        time_section = ~ case_when(
                                . == "pre" ~ "pre_event_window",
                                . == "post" ~ "post_event_window",
                                . == "ew" ~ "event_window",
                                . == "ev" ~ "event_day",
                                . == "est" ~ "estimation_window",
                                TRUE ~ .
                        ),
                        weighted = ~ case_when(
                                . == "uw" ~ "no",
                                . == "w" ~ "yes",
                                TRUE ~ .
                        )
                )
        ) %>%
        unique() %>%
        arrange(event, time_section, weighted) %>%
        select(event, time_section, weighted, ar, var, sd, t, p)



# get it into a format resembling the tables in the paper -----------------

make_results_table <- function(weight_switch) {
        results_table <- results_all %>%
                select(event, time_section, weighted, ar, sd, p) %>%
                filter(
                        weighted == weight_switch,
                        time_section != "estimation_window"
                ) %>%
                select(-weighted) %>%
                pivot_longer(
                        cols = c(ar, sd, p),
                        names_to = "metric",
                        values_to = "value"
                ) %>%
                pivot_wider(
                        names_from = time_section,
                        values_from = value
                ) %>%
                select(
                        "event", "metric", "pre_event_window",
                        "event_day", "post_event_window", "event_window"
                ) %>%
                mutate(across(
                        .cols = "pre_event_window":"event_window",
                        .fns = ~ case_when(
                                metric == "ar" ~ round(. * 100, 1),
                                metric == "sd" ~ round(., 3),
                                metric == "p" ~ round(., 3),
                                .default = .
                        )
                ))
                return(results_table)
}

if (weights == TRUE) {
        results_table <- make_results_table(weight_switch = "yes")
        results_table_uw <- make_results_table(weight_switch = "no")
} else {
   results_table <- make_results_table(weight_switch = "no")
}



# remove scalar variables from "data" -------------------------------------

data %<>%
        select(
                -starts_with("ar_pre"),
                -starts_with("ar_post"),
                -starts_with("ar_ew"),
                -starts_with("ar_ev"),
                -starts_with("sd_"),
                -starts_with("var_"),
                -starts_with("t_"),
                -starts_with("p_"),
                -starts_with("get")
        )



# create function for viewing results -------------------------------------

marvel_at_results <- function(results = results_table) {
        tibble::view(results)
}