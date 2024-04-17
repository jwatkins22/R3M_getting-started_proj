# packages
library(tidyverse)
library(janitor)
library(scales)

# data
childcare <- read_csv("childcare_infants.csv")

# data frames
childcare_cost_burden <- childcare |> 
  mutate(median_center_annual = (median_center_infant*52),
         median_family_annual = (median_family_infant*52),
         percent_income_family = (median_family_annual/median_income_2018)*100,
         percent_income_center = (median_center_annual/median_income_2018)*100)

## summarizing DC and GA counties
DC_childcare_cost_burden <- childcare_cost_burden |> 
  filter(state_abbreviation=="DC") |> 
  drop_na(percent_income_family,percent_income_family)

GA_childcare_cost_burden <- childcare_cost_burden |>
  filter(state_abbreviation=="GA") |> 
  drop_na(percent_income_family,percent_income_family) |> 
  summarize(percent_income_family_ga=median(percent_income_family),
            percent_income_center_ga=median(percent_income_center),
            median_income_2018_ga=median(median_income_2018),
            state_name=unique(state_name)) |> 
  mutate(percent_income_family=percent_income_family_ga,
         percent_income_center=percent_income_center_ga,
         median_income_2018=median_income_2018_ga)

## combining the above summary data frames
cost_burden_by_state <- bind_rows(DC_childcare_cost_burden,GA_childcare_cost_burden) |> 
  select(state_name,median_income_2018,percent_income_family,percent_income_center) |> 
  pivot_longer(cols=c(percent_income_family,percent_income_center),
               names_to="care_type",
               names_prefix="percent_income_",
               values_to="percentage_of_income")



# data viz

## i tried to format the labels with the below data frame, 
## but kept getting an error when i added to ggplot

### percentage_of_income_formatted <- cost_burden_by_state |> 
###  mutate(percentage_of_income=number(percentage_of_income,accuracy=.1)) |> 
###  percent_format(percentage_of_income, suffix = "%")

ggplot(data=cost_burden_by_state,
       mapping=aes(x=care_type,
                   y=percentage_of_income,
                   fill=state_name,
                   label=percentage_of_income))+
  geom_col(position="dodge")+
  geom_label(vjust=1)+
  labs(title="Annual amount spent on childcare is tied to states' average income",
       subtitle="even across types of providers.",
       caption="GA annual income = $40,768
       DC annual income= $70,693",
       x="Type of Childcare Provider",
       y="Percentage of Income Spent",
       fill="State")+
  theme_minimal()

