require("jsonlite")
require("RCurl")
require("ggplot2")

# Dataframe created from CSV table created in SQL for scatter plot
death_df <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from DISEASE where NUMBER_OF_DEATHS < 100000"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_nar784', PASS='orcl_nar784', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

summary(death_df)
head(death_df)


# Create the Bar Chart Data Frame

bar_df <- death_df %>% group_by(COUNTRY_NAME, SEX) %>% filter(COUNTRY_NAME %in% c("Afghanistan", "China", "Colombia", "Japan", "Korea, Republic of", "Pakistan", "Philippines", "Spain", "United Kingdom", "United States")) %>% filter(SEX %in% c("Female", "Male")) %>% summarize(AVG_DR = mean(DEATH_RATE_PER_100_000))

bar_comb <- bar_df %>% ungroup %>% group_by(SEX) %>% summarize(WINDOW_AVG_DR=mean(AVG_DR))
bar_df <- inner_join(bar_df, bar_comb, by="SEX") %>% arrange(COUNTRY_NAME)

spread(bar_df, COUNTRY_NAME, AVG_DR) %>% View


