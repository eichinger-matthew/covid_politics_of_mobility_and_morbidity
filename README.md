# covid_politics_of_mobility_and_morbidity
A study that tries to estimate the causal effect of political partisanship on mobility outcomes and death outcomes during the pandemic

# Note

I am currently updating the preemption model, survival model, and VAR models with more data. Consequently, some of the findings in the .pdf paper may change. Most of the findings will likely stay the same given that the new data pertains to COVID-19 and mobility data long after the public adapted their behavior to the virus. <br />


# Files

There are lots of files. This explains what each of them does. <br />

covid_data_retrieval_and_merging.R grabs the county COVID-19 data, county mobility data, and county sociodemographic data from online. It does minor cleaning for the purpose of merging, and then merges the COVID-19 and mobility data by county and date. It saves the resulting time-series dataframes as .rds files. <br />

data_cleaning_county_covid_and_mobility.R cleans the time-series dataframes from the retrieval file. Among other things, it prunes the sample to keep only counties with at least 825 observations, drops variables that are not useful, and creates new variables that are theoretically important - things like the cumulative number of cases per county, the day at which a county returned to a "baseline" level of pre-pandemic mobility, and so forth. It outputs a .rds file called "clean_county_covid_rates_and_mobility_retail.rds". <br />

match_models.R runs the matching models to get sample adjustment weights that address selection bias in how counties are "allocated" to the Republican and Democratic "treatment" groups. It also produces Love plots, balance plots, and balance tables using the "cobalt" package, and it stores the results in organized lists. The output file is "treatment_matching_results.RData". <br />

preemption.R specifies and estimates the preemption model. <br />

surv_models.R specifies and estimates the survival model. <br />

var_models.R specifies and estimates the vector autoregression models. TAKE NOTE before running the models - the file estimates a VAR model, estimates an impulse response, and produces an impulse response graphic for each county in the sample. It takes a while to run.




