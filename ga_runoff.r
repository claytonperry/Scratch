library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

general2020_raw <- read.csv("https://raw.githubusercontent.com/openelections/openelections-data-ga/master/2020/20201103__ga__general.csv")

runoff2021_raw <- read.csv("https://raw.githubusercontent.com/openelections/openelections-data-ga/master/2021/20210105__ga__runoff.csv")

general2020 <- general2020_raw %>%
    filter(
        office %in% c("U.S. Senate (Special)", "U.S. Senate"),
        candidate %in% c("Raphael Warnock", "Kelly Loeffler", "David A. Perdue", "Jon Ossoff")
        ) %>%
    select(
        county, precinct, candidate, election_day_votes, advanced_votes,
        absentee_by_mail_votes, provisional_votes
    ) %>%
    mutate(
        cand = str_trunc(candidate, 1, "right", ""),
        total_votes = rowSums(across(election_day_votes:provisional_votes)
        )
    ) %>%
    pivot_wider(
        id_cols = c("county", "precinct"),
        names_from = cand,
        values_from = total_votes
    )

runoff2021 <- runoff2021_raw %>%
    filter(
        office %in% c("U.S. Senate (Special)", "U.S. Senate"),
        candidate %in% c("Raphael Warnock", "Kelly Loeffler", "David A. Perdue", "Jon Ossoff")
        ) %>%
    select(
        county, precinct, candidate, election_day_votes, advanced_votes,
        absentee_by_mail_votes, provisional_votes
    ) %>%
    mutate(
        cand = str_trunc(candidate, 1, "right", ""),
        total_votes = rowSums(across(election_day_votes:provisional_votes)
        )
    ) %>%
    pivot_wider(
        id_cols = c("county", "precinct"),
        names_from = cand,
        values_from = total_votes
    )

analysis <- runoff2021 %>%
    left_join(
        general2020,
        by = c("county", "precinct"),
        suffix = c("_g", "_r")
    ) %>%
    mutate(
        DJ_g_total = D_g + J_g,
        DJ_r_total = D_r + J_r,
        KR_g_total = K_g + R_g,
        KR_r_total = K_r + R_r,
        D_g_share = D_g / DJ_g_total,
        J_g_share = J_g / DJ_g_total,
        K_g_share = K_g / KR_g_total,
        R_g_share = R_g / KR_g_total,
        D_r_share = D_r / DJ_r_total,
        J_r_share = J_r / DJ_r_total,
        K_r_share = K_r / KR_r_total,
        R_r_share = R_r / KR_r_total,
        D_share_d = D_r_share - D_g_share,
        J_share_d = J_r_share - J_g_share,
        K_share_d = K_r_share - K_g_share,
        R_share_d = R_r_share - R_g_share
    ) %>%
    filter(
        J_share_d > 0 | R_share_d > 0,
        county %in% c(
            "Cobb", "Gwinnett", "DeKalb", "Fulton", "Henry",
            "Clayton", "Cherokee", "Douglas", "Fayette", "Henry", "Dougherty",
            "Hancock", "Calhoun", "Terrell", "Randolf", "Rockdale", "Macon",
            "Warren", "Richmond", "Bibb", "Washington", "Forsythe", "Butts"
        )
    ) %>%
    group_by(county) %>%
    summarise(
        precinct_n = n(),
        avg_J_d = mean(D_share_d),
        avg_R_d = mean(R_share_d)
    )
