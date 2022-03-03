##Install necessary packages if missing
if(!require(pacman)) install.packages("pacman")

pacman::p_load("tidyverse",
               "here",
               "broom",
               "ggplot2",
               "purrr",
               "gapminder",
               "infer",
               "ggthemes",
               "bench",
               "tictoc",
               "magrittr",
               "stringi",
               "glue",
               "furrr",
               "rvest",
               "usethis",
               "roxygen2",
               "testthat",
               "patchwork")


#Load a df of the raw csv's in SAME FOLDER AS .R
df <- tibble(do.call(rbind,
    list.files(pattern = '\\.csv') %>%
        lapply(read.csv, header = TRUE)))


# Translating of values and renaming of columns
ES_EN_translation <- c("Automotriz" = "Car Loan",
                       "Hipotecario" = "Mortgage",
                       "Nomina" = "Payroll Loan",
                       "Personal y Microcreditos Ind." = "SME",
                       "Tarjetas de Credito" = "Credit Cards")

df$Tipo.de.credito <- df$Tipo.de.credito %>% 
    str_replace_all(ES_EN_translation)

colnames(df) <- c("c_type","i_rate", "c_balance")


# Create "Household" credit type
# Relative weights of the credit type in the commercial banking balance
HH_weights <- c("Credit Cards" = 0.1976,
                "Payroll Loan" = 0.1444,
                "Mortgage" = 0.5778,
                "Car Loan" = 0.1385)


# Prepare new df
HH_df <- df %>%
    filter(c_type %in% c("Credit Cards",
                                "Payroll Loan",
                                "Mortgage",
                                "Car Loan"))


# Adjust each balance by the type' respective weight
for (i in 1:108){
    if (HH_df$c_type[i] == "Mortgage") {
        HH_df$c_balance[i] = 
            HH_df$c_balance[i] *
            HH_weights["Mortgage"] * 
            
            # correct error in Mortgage raw data where it doesn´t add to 100%
            
            2.1303} else {
                HH_df$c_balance[i] = 
                    HH_df$c_balance[i] *
                    HH_weights[HH_df$c_type[i]]
            }
    
}
rm("i")

# Condense the df for Household credits
HH_df <- aggregate(HH_df$c_balance,
                   by=list(HH_df$i_rate),
                   FUN=sum)


#drop irrelevant >100% rate row
HH_df <- HH_df[-1,]

 
# Rename columns and add missing
colnames(HH_df) <- c("i_rate", "c_balance")
HH_df$c_type <- "Household"


# Extract df for SME credits
SME_df <- df %>%
    filter(c_type %in% "SME")


# Gen numeric col for interest rate closed limits (upper), and into percentage
SME_df$ni_rate <- stri_sub(SME_df$i_rate,-3) %>%
    as.numeric() / 100
SME_df[52,4] <- 1.1
SME_df <- SME_df[,-2]

HH_df$ni_rate <- stri_sub(HH_df$i_rate,-3) %>%
    as.numeric() / 100
HH_df <- HH_df[,-1]


# Share of balance as percentage too
SME_df$c_balance <- SME_df$c_balance / 100
HH_df$c_balance <- HH_df$c_balance  / 100


# Set up final df
fdf <- bind_rows(SME_df, HH_df)


# Rate mean for Large Enterprises (no disaggregated data available)
large_mean = .062


# Plot graph
ggplot(fdf, aes(x=ni_rate, y=c_balance, fill=c_type)) +
    geom_col(color="black") +
    theme_gray() + xlab("Interest Rate") + 
    ylab("Share of Commercial Banking Loans") +
    geom_vline(mapping = aes(xintercept = large_mean,
                             fill = "LE Mean Rate"), color="green") +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    guides(fill=guide_legend(title="Client Type"))
