require("jsonlite")
require("RCurl")
require("ggplot2")

# Dataframe created from CSV table created in SQL for scatter plot
death_df <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from DISEASE where NUMBER_OF_DEATHS < 100000"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_nar784', PASS='orcl_nar784', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

summary(death_df)
head(death_df)

# Create the Crosstab dataframe
KPI_Low_Max_value = .5 
KPI_Medium_Max_value = 1

KPI_df <- death_df %>% group_by(AGE_GROUP, SEX) %>% summarize(sum_death = sum(NUMBER_OF_DEATHS)/1000000, sum_100 = sum(DEATH_RATE_PER_100_000)/1000000) %>% mutate(ratio = sum_death / sum_100) %>% mutate(kpi = ifelse(ratio <= KPI_Low_Max_value, '03 Low', ifelse(ratio <= KPI_Medium_Max_value, '02 Medium', '01 High'))) %>% rename(age_group=AGE_GROUP, sex=SEX, SUM_DEATH=sum_death, SUM_100=sum_100, RATIO=ratio, KPI=kpi)%>%tbl_df

summary(KPI_df)
head(KPI_df)

# Create the Bar Chart Data Frame

df <- death_df %>% group_by(COUNTRY_NAME, SEX) %>% filter(COUNTRY_NAME %in% c("Afghanistan", "China", "Colombia", "Japan", "Korea, Republic of", "Pakistan", "Philippines", "Spain", "United Kingdom", "United States")) %>% filter(SEX %in% c("Female", "Male")) %>% summarize(AVG_DR = mean(DEATH_RATE_PER_100_000))
df1 <- df %>% ungroup %>% group_by(SEX) %>% summarize(WINDOW_AVG_DR=mean(AVG_DR))
df <- inner_join(df, df1, by="SEX") %>% arrange(COUNTRY_NAME)

spread(df, COUNTRY_NAME, AVG_DR) %>% View

