# covid_politics_of_mobility_and_morbidity
A study that tries to estimate the causal effect of county Republicanship on mobility outcomes and death outcomes during the pandemic. I approach the problem in three ways. The first is studying how long it takes counties to return a pre-pandemic level baseline of mobility after they enter the pandemic - that is, how long it takes them to *survive*. The second is studying temporal patterns between county COVID-19 rates and mobility levels. I use time-series data on both variables to fit a unique vector autoregression system for each county, and then I use it to forecast how a county would respond over two weeks to a sudden increase in its COVID-19 rates (i.e., an impulse response function). After I calculate a response for each county, I group them into Republican and Democratic groups to see if there is a significant difference. The final approach is studying whether Republican counties preempted the pandemic at lower rates than Democratic counties. I get at this by measuring a "county preemption" variable based on whether a county reduced its mobility levels *after* the national emergency announcement on March 13th, 2020 and *before* passing a 15-cases-per-100,000-people threshold of COVID-19.

This repository contains the R-scripts I wrote to collect and clean data, specify and estimate models, and produce data visualizations, as well as a PDF file of the research paper itself. Please cite accordingly. 


# Files

There are lots of files. This explains what each of them does. <br />

data_retrieval_and_merging.R grabs the county COVID-19 data, county mobility data, and county sociodemographic data from online. It does minor cleaning for the purpose of merging, and then merges the COVID-19 and mobility data by county and date. It saves the resulting time-series dataframes as .rds files. <br />

data_cleaning.R cleans the time-series dataframes from the retrieval file. Among other things, it prunes the sample to keep only counties with at least 825 observations, drops variables that are not useful, and creates new variables that are theoretically important - things like the cumulative number of cases per county, the day at which a county returned to a "baseline" level of pre-pandemic mobility, and so forth. It outputs a .rds file called "clean_county_covid_rates_and_mobility_retail.rds". <br />

match_models.R runs the matching models to get sample adjustment weights that address selection bias in how counties are "allocated" to the Republican and Democratic "treatment" groups. It also produces Love plots, balance plots, and balance tables using the "cobalt" package, and it stores the results in organized .RData objects. The objects are too big to upload to Github, so if you would like a copy of them please just send me an email. <br />

descriptive_graphs.R produces most of the visualizations seen in the paper. A few in the paper come from surv_models.R, preemption.R, and var_models.R because it made sense to generate graphs after estimating models. <br />

preemption.R specifies and estimates the preemption model. It supercedes the preemption.R file. <br />

surv_models.R specifies and estimates the survival model. <br />

var_models.R specifies and estimates the vector autoregression models. TAKE NOTE before running the models - the file estimates a VAR model, estimates an impulse response, and produces an impulse response graphic for each county in the sample. It takes a while to run.




