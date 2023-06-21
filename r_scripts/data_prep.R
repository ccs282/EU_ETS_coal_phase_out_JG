# Read market data file ---------------------------------------------------

data <- read_csv("data/Data.csv")



# Prep dependent variable (EUA prices) ------------------------------------

data %<>%
        mutate(eua = as.numeric(NA)) # create empty EUA variable

eua_cols <- str_subset(names(data), "eua[[:digit:]]{2}$")

for (eu_xx in eua_cols) {
        # switch to next variable in mid-December-ish
        data %<>%
                mutate(eua = if_else(
                        condition = is.na(eua) & !is.na(get(eu_xx)),
                        true = get(eu_xx),
                        false = eua
                ))
}
rm(eu_xx, eua_cols)

# drop unnecessary variables
data %<>%
        select(Date, eua, everything(), -(eua07:eua2022_out))



# Log returns and their differences ---------------------------------------

variables <- c("eua", "Oil", "Coal", "Gas", "Elec", "GSCI", "VIX", "STOXX",
        "Diff_BAA_AAA", "ECB_Spot_3M") # vector of explanatory vars and eua

for (var in variables) {
        # name strings
        ln_return <- glue("ln_return_{var}")
        d_ln_return <- glue("d_ln_return_{var}")

        # data
        data_temp <- data %>%
                filter(!is.na(get(var))) %>%
                # log returns
                mutate({{ ln_return }} := log(get(var) / lag(get(var)))) %>%
                # differenced log returns
                mutate({{ d_ln_return }} := get(ln_return) -
                        lag(get(ln_return)))

        # merge with main data set
        data <- full_join(data_temp, data) %>%
        arrange(Date)
}

rm(data_temp, ln_return, var, d_ln_return, variables) # drop unnecessary objects



# clean data further ------------------------------------------------------

data %<>%
        # remove NA for ln_return data
        drop_na(starts_with("ln_return_")) %>%
        # sort data
        arrange(Date) %>%
        # remove unnecessary vars
        select(-AAA, -BAA) %>%
        # drop early observations; same as in Koch et al. (2016)
        filter(Date > 20080314) %>%
        rename(date = Date)