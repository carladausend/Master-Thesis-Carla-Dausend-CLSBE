## ----------------------------------------------------------------------------
##-- Setup
## ----------------------------------------------------------------------------

rm(list = ls())

library(stargazer)
library(gsynth)
library(lfe)
library(ggthemes)
library(gridExtra)
library(Rmisc)
library(mfx)
library(tidyverse)
library(dplyr)


## ----------------------------------------------------------------------------
##-- Datasets preparation
## ----------------------------------------------------------------------------

#load datasets
dt.usage.de         <- read_csv("data/nielsen/usage_de.csv")
dt.usage.us         <- read_csv("data/nielsen/usage_us.csv")
dt.risk.score       <- read_csv("data/risk_scores/risk_subscores.csv")
dt.infringement     <- read_csv("data/risk_scores/Infringement.csv")
dt.conceal     <- read_csv("data/concealment/concealment_services.csv")
dt.cookie       <- read_csv("data/risk_scores/cookie_compliance.csv")


#rename columns
names(dt.usage.de) <- tolower(names(dt.usage.de))
names(dt.usage.us) <- tolower(names(dt.usage.us))
names(dt.risk.score) <- tolower(names(dt.risk.score))
dt.usage.de <-  rename (dt.usage.de, time_per_person = time__per_person)
dt.usage.us <-  rename (dt.usage.us, time_per_person = time__per_person)


#split column into categories age, gender and income
dt.usage.de<- dt.usage.de %>%  
  mutate(
    age = gsub('(.+Age[|])([0-9]+[-]*[0-9]*[+]*)(.+)','\\2', entity_category)
    ,  gender = gsub('(Gender[|])([a-zA-Z]+)([*].+)','\\2', entity_category)
    , income = gsub('(.+Income[|])(.*)$','\\2', entity_category))

dt.usage.us<-dt.usage.us %>% 
  mutate(
    age = gsub('(.+Age[|])([0-9]+[-]*[0-9]*[+]*)(.+)','\\2', entity_category)
    ,  gender = gsub('(Gender[|])([a-zA-Z]+)([*].+)','\\2', entity_category)
    , income = gsub('\\$ ', "",gsub('(.+Income[|])(.*)$','\\2', entity_category))) 


#combine us and germany data
dt.usage.combined      <- rbind(dt.usage.de, dt.usage.us)

#rename month to date
dt.usage.combined <-  rename (dt.usage.combined, date = month)

#join the nielson data with the risk scores
dt.usage.combined <- merge(x=dt.usage.combined, 
                           y=dt.risk.score, 
                           by.x=c("entity_name"), 
                           by.y=c("domain"), 
                           all.x=TRUE)


#join the nielson data with the infringement data
dt.usage.combined <- merge(x=dt.usage.combined, 
                           y=dt.infringement, 
                           by.x=c("entity_name"), 
                           by.y=c("domain"), 
                           all.x=TRUE)


# dt.usage.combined <- merge(x=dt.usage.combined, 
#                            y=dt.who.is %>% select(domain, date, is_private, registrant, registrar), 
#                            by.x = c("entity_name", "date"), 
#                            by.y = c("domain", "date"),
#                            all.x=TRUE)
# 
# dt.usage.combined<- dt.usage.combined %>%  mutate(concealnn = ifelse(is.na(registrant), NA, ifelse(registrant %in% dt.conceal$conceal, 1, 0)))


#gdpr column
dt.usage.combined <- dt.usage.combined %>% mutate(gdpr = ifelse(date<'2018-05-01','Before GDPR','After GDPR')) 
dt.usage.combined<- dt.usage.combined %>% mutate(gdpr_1 = ifelse(gdpr == 'After GDPR', 1, 0))

#risk score fill NAs
dt.usage.combined<-dt.usage.combined %>%  mutate(RS = replace_na(RS, 0),
                                                 CI = replace_na(CI, 0),
                                                 CI2 = replace_na(CI2, 0), 
                                                 CI3 = replace_na(CI3, 0), 
                                                 PS = replace_na(PS, 0), 
                                                 UC = replace_na(UC, 0), 
                                                 ML = replace_na(ML, 0),
                                                 SU = replace_na(SU, 0))

#harm column
dt.usage.combined <- dt.usage.combined %>% mutate(harm= ifelse(CI>0|CI2>0|PS>0|UC>0|CI3>0|ML>0|SU>0,1,0),
                                                  harm_chr = ifelse(CI>0|CI2>0|PS>0|UC>0|CI3>0|ML>0|SU>0,'Some Harm','No Harm'))
dt.usage.combined<-dt.usage.combined %>%  mutate(harm = replace_na(harm, 0))
#harm only Copyright infringement
dt.usage.combined <- dt.usage.combined %>% mutate(copyright= ifelse(CI>0|CI2>0|CI3>0|UC>0,1,0),
                                                  copyright_chr = ifelse(CI>0|CI2>0|CI3>0|UC>0,'Some Copyright Infringement','No Copyright Infringement'))
dt.usage.combined<-dt.usage.combined %>%  mutate(copyright = replace_na(copyright, 0))
#malicious websites
dt.usage.combined <- dt.usage.combined %>% mutate(malicious = ifelse(ML>0|SU>0,1,0),
                                                  malicious_chr  = ifelse(ML>0|SU>0,'Malicious','Not malicious'))
dt.usage.combined<-dt.usage.combined %>%  mutate(malicious  = replace_na(malicious, 0))
#phishing
dt.usage.combined <- dt.usage.combined %>% mutate(phishing_chr = ifelse(PS>0,'Some Phishing','No Phishing'))

#cookie_comply of website with risk score only
dt.cookie<-dt.cookie %>% mutate(country = str_squish(country))
dt.usage.combined<- dt.usage.combined %>%  left_join(dt.cookie, by = c("entity_name", "country"))

#Create id for first difference model
dt.usage.combined <- dt.usage.combined %>% mutate(id = paste(entity_name, country))

#country dummy
dt.usage.combined<-dt.usage.combined %>% mutate(germany= ifelse(country=='Germany', 1, 0))

# number of websites decreases over time - 
# used top 1000 from 05/17, when one disappears, that website is no longer in the top 1000
dt.usage.combined %>%
  group_by(country, gdpr, date) %>%
  summarise(nr_websites = n_distinct(entity_name))%>% 
  arrange(country, date)%>% print(n=40) 


#filter complete websites
dt.websites.complete <- dt.usage.combined %>% filter(age == 'Total') %>% group_by(id) %>%  summarise(nr_entries= n()) %>% filter(nr_entries==19)

dt.usage.combined<- dt.usage.combined %>%  mutate(website.complete = ifelse(id %in% dt.websites.complete$id, 1, 0))

#log
dt.usage.combined<-dt.usage.combined %>%  mutate(log_audience = log(audience),
                                                 log_active_reach = log(active_reach),
                                                 log_universe_reach = log(universe_reach),
                                                 log_page_views = log(page_views),
                                                 log_pages_per_person = log(pages_per_person),
                                                 log_total_sessions = log(total_sessions),
                                                 log_sessions_per_person = log(sessions_per_person),
                                                 log_total_minutes = log(total_minutes),
                                                 log_time_per_person = log(time_per_person))

#seperate into two data sets -> total observations & seperate categories
dt.usage.combined_total <- dt.usage.combined %>%  filter(age == 'Total')
dt.usage.combined_categories <- dt.usage.combined %>%  filter(age != 'Total')

# #variables needed for regressions
# #treated
# dt.usage.combined_total<-dt.usage.combined_total %>% mutate(D = ifelse(gdpr_1==1 & country=='Germany', 1, 0))

#period variable
dt.usage.combined_total <- dt.usage.combined_total %>% left_join(   
  dt.usage.combined_total %>% dplyr::select(date) %>% distinct() %>% 
    mutate(period = 1:n()),
  by = 'date')

#month dummies
dt.usage.combined_total <- dt.usage.combined_total %>% 
  mutate(
    may17 = as.double(date == as.Date('2017-05-01')),
    jun17 = as.double(date == as.Date('2017-06-01')),
    jul17 = as.double(date == as.Date('2017-07-01')),
    aug17 = as.double(date == as.Date('2017-08-01')),
    sep17 = as.double(date == as.Date('2017-09-01')),
    oct17 = as.double(date == as.Date('2017-10-01')),
    nov17 = as.double(date == as.Date('2017-11-01')),
    dec17 = as.double(date == as.Date('2017-12-01')),
    jan = as.double(date == as.Date('2018-01-01')),
    feb = as.double(date == as.Date('2018-02-01')),
    mar = as.double(date == as.Date('2018-03-01')),
    apr = as.double(date == as.Date('2018-04-01')),
    may = as.double(date == as.Date('2018-05-01')),
    jun = as.double(date == as.Date('2018-06-01')),
    jul = as.double(date == as.Date('2018-07-01')),
    aug = as.double(date == as.Date('2018-08-01')),
    sep = as.double(date == as.Date('2018-09-01')),
    oct = as.double(date == as.Date('2018-10-01')),
    nov = as.double(date == as.Date('2018-11-01')),
    us  = as.double(germany == 0))

#top level country code
dt.usage.combined_total<- dt.usage.combined_total %>% mutate( sname = gsub("(.+)([.])([a-zA-Z]+)","\\3",entity_name))

#dt.usage.combined_total%>%select(entity_name,sname) %>% unique() %>% dplyr::count(sname)

europe<- list("eu", "de", "at", "nl", "uk")
unitedstates<- list("us", "la", "gov")
dt.usage.combined_total<- dt.usage.combined_total %>% mutate( geography = ifelse(country =='Germany' & sname %in% europe, "EU", ifelse(country == 'United States' & sname %in% unitedstates, "US", "Other")))

#German traffic inside vs outside EU

dt.usage.combined_total<-dt.usage.combined_total %>% mutate(io_eu = ifelse(country=='United States', NA, ifelse(country == 'Germany' & geography=='EU', 1,0)))
dt.usage.combined_total<-dt.usage.combined_total %>% mutate(io_eu_chr = ifelse(io_eu==1, 'Inside EU', ifelse(io_eu==0, 'Outside EU', NA)))

dt.usage.combined_total <- dt.usage.combined_total %>% mutate(eu = ifelse(geography=="EU", 1, 0), us_geo = ifelse(geography=="US", 1, 0))


#string cookie
dt.usage.combined_total<-dt.usage.combined_total %>% mutate(cookie_june_chr = ifelse(cookie_june==1, 'Cookie', ifelse(cookie_june==0, 'No Cookie', NA)))



#sample size filter
sample_size_60<- dt.usage.combined_total %>% filter(sample_size<60) %>% select(id) %>% unique()
dt.usage.combined_total <- dt.usage.combined_total %>% mutate(ss_60 = ifelse(id %in% sample_size_60$id, 0, 1))

#SCM treatment 
dt.usage.combined_total <- dt.usage.combined_total %>% mutate(D2 = ifelse(gdpr_1==1 & geography=="EU", 1, 0))
dt.usage.combined_total <- dt.usage.combined_total %>% mutate(D3 = ifelse(gdpr_1==1 & io_eu==1, 1, 0))
dt.usage.combined_total <- dt.usage.combined_total %>% mutate(D4 = ifelse(gdpr_1==1 & cookie_june==1, 1, 0))


## ----------------------------------------------------------------------------
##-- Descriptive Statistics
## ----------------------------------------------------------------------------

# statistics for german traffic, cookie comparison
stargazer_fct_cookie <- function (cty, gdpr_lvl, cook) {
  tb.tmp1 <- as.data.frame(dt.usage.combined_total %>% filter(ss_60==1 & date>='2018-01-01'& country==cty & gdpr==gdpr_lvl & website.complete==1 & !is.na(cookie_june)& !is.na(cookie_nov)&cookie_june==cook) %>% dplyr::select(audience, pages_per_person, sessions_per_person, time_per_person))
  stargazer(tb.tmp1
            ,  type = "text", float =T, median = TRUE
            , title = "Summary statistics"
            , covariate.labels = c("Audience","Pages per person", "Sessions per person", "Time per person") )
}

stargazer_fct_cookie('Germany', 'Before GDPR','1')
stargazer_fct_cookie('Germany', 'After GDPR', '1')
stargazer_fct_cookie('Germany', 'Before GDPR', '0')
stargazer_fct_cookie('Germany', 'After GDPR', '0')

#statisics for country code level
stargazer_fct_cty_code <- function (cty, gdpr_lvl, geo) {
  tb.tmp1 <- as.data.frame(dt.usage.combined_total %>% filter(ss_60==1 & date>='2018-01-01'& country==cty & gdpr==gdpr_lvl & geography==geo & website.complete==1) %>% dplyr::select(audience, pages_per_person, sessions_per_person, time_per_person))
  stargazer(tb.tmp1
            ,  type = "text", float =T, median = TRUE
            , title = "Summary statistics"
            , covariate.labels = c("Audience","Pages per person", "Sessions per person", "Time per person") )
}

stargazer_fct_cty_code('Germany', 'Before GDPR', "EU")
stargazer_fct_cty_code('Germany', 'After GDPR', "EU")
stargazer_fct_cty_code('United States', 'Before GDPR', "US")
stargazer_fct_cty_code('United States', 'After GDPR', "US")

#statistics for german traffic outside of EU
stargazer_fct_germany <- function (cty, gdpr_lvl, geo) {
  tb.tmp1 <- as.data.frame(dt.usage.combined_total %>% filter(ss_60==1 & date>='2018-01-01'& country==cty & gdpr==gdpr_lvl & geography!=geo & website.complete==1) %>% dplyr::select(audience, pages_per_person, sessions_per_person, time_per_person))
  stargazer(tb.tmp1
            ,  type = "text", float =T, median = TRUE
            , title = "Summary statistics"
            , covariate.labels = c("Audience","Pages per person", "Sessions per person", "Time per person") )
}
stargazer_fct_germany('Germany', 'Before GDPR', "EU")
stargazer_fct_germany('Germany', 'After GDPR', "EU")


#risk percentages


dt.usage.combined_total %>% filter(ss_60==1 & date>='2018-01-01' & website.complete==1 & !is.na(cookie_june)& !is.na(cookie_nov)&country=='Germany') %>% 
  group_by(id) %>% 
  summarise(CI= mean(CI),
            CI2= mean(CI2),
            CI3= mean(CI3),
            PS= mean(PS),
            UC= mean(UC),
            ML= mean(ML),
            SU= mean(SU),
            copyright=mean(copyright),
            malicious=mean(malicious)) %>% 
  ungroup() %>% mutate(CI = ifelse(CI>0,1,0), 
                       CI2= ifelse(CI2>0,1,0),
                       CI3= ifelse(CI3>0,1,0),
                       PS= ifelse(PS>0,1,0),
                       UC= ifelse(UC>0,1,0),
                       ML= ifelse(ML>0,1,0), 
                       SU= ifelse(SU>0,1,0))%>% summary()



## ----------------------------------------------------------------------------
##-- Histrograms- Distribution, should we use log? YES
## ----------------------------------------------------------------------------

hist_function <- function (x_var) {
  plot1<-ggplot(data = dt.usage.combined_total, aes(x=get(x_var))) + geom_histogram() + facet_wrap(~country)+ theme_bw()
  plot2<-ggplot(data = dt.usage.combined_total, aes(x=log(get(x_var)))) + geom_histogram() + facet_wrap(~country)+ theme_bw()
  grid.arrange(plot1, plot2, ncol=2)
}
hist_function("audience")
hist_function("pages_per_person")
hist_function("sessions_per_person")
hist_function("time_per_person")



## ----------------------------------------------------------------------------
##-- Line Plots
## ----------------------------------------------------------------------------


#compliance
line_plot.compliance <- function (x_var, name, bottom, top) {
  dt.usage.combined_total %>% filter(date >= as.Date('2018-01-01') & ss_60==1& website.complete==1 & !is.na(cookie_june)) %>% 
    summarySE(measurevar = x_var, groupvars = c('date','cookie_june_chr')) %>%
    ggplot(aes( x = date, y = get(x_var), color = cookie_june_chr,linetype = ifelse(date <= as.Date('2018-05-25'),'Before','After GDPR') )) + 
    geom_line() + geom_errorbar(aes(ymin= get(x_var)-ci, ymax = get(x_var) + ci) , width = 0.1) +
    theme_bw() + scale_color_tableau() + expand_limits(y = c(bottom, top))+
    geom_vline(xintercept  = as.Date('2018-05-15'), linetype='dashed') + stat_smooth(method = 'lm')+
    annotate("text", x=as.Date('2018-05-22'), y= (bottom +.8), label="GDPR       25-05-2018", angle=90, size = 10/.pt)+
    labs( y = name, x = 'Month in 2018', color = ' ') +
    scale_color_manual(values = c("dark blue", "#99CCFF"), labels = c("Compliance", "Non-Compliance")) +
    theme(axis.text=element_text(size=14), axis.title=element_text(size=14) ,legend.position = "none", text = element_text(family = "Times New Roman"))
}

fig1 <- line_plot.compliance("log_audience","Log (Audience)",  11,15); fig1
fig1 <- line_plot.compliance("log_pages_per_person","Log (Pages per Person)", 0,3); fig1
fig1 <- line_plot.compliance("log_sessions_per_person","Log (Sessions per Person)",0,1.5 ); fig1
fig1 <- line_plot.compliance("log_time_per_person","Log (Hours per Person)",-6,-4.5); fig1

# ggsave(
#   filename = "~/paper-gdpr_whois/paper/plots/graph.compliance.audience.png"
#   , plot = fig1
#   , width = 5, height = 4)

#country code top level
line_plot.country <- function (x_var, name, bottom, top) {
  dt.usage.combined_total %>% filter(date >= as.Date('2018-01-01') & ss_60==1&website.complete==1 & geography!="Other") %>% 
    summarySE(measurevar = x_var, groupvars = c('date','geography')) %>%
    ggplot(aes( x = date, y = get(x_var), color = geography , linetype = ifelse(date <= as.Date('2018-05-25'),'Before','After GDPR'))) + 
    geom_line() + geom_errorbar(aes(ymin= get(x_var)-ci, ymax = get(x_var) + ci) , width = 0.1) +
    theme_bw() + scale_color_tableau() + expand_limits(y = c(bottom, top))+
    geom_vline(xintercept  = as.Date('2018-05-15'), linetype='dashed') + stat_smooth(method = 'lm')+
    annotate("text", x=as.Date('2018-05-22'), y= (bottom +1), label="GDPR       25-05-2018", angle=90, size = 10/.pt)+
    labs( y = name, x = 'Month in 2018', color = 'Country') +
    scale_color_manual(values = c("dark blue", "#99CCFF"), labels = c("Germany", "United States")) +
    theme(axis.text=element_text(size=24), axis.title=element_text(size=24), legend.position = "none", text = element_text(family = "Times New Roman"))
}



fig2 <- line_plot.country("log_audience","Log (Audience)",  12,15); fig2
fig2 <- line_plot.country("log_pages_per_person","Log (Pages per Person)", 1,4); fig2
fig2 <- line_plot.country("log_sessions_per_person","Log (Sessions per Person)",0,2 ); fig2
fig2 <- line_plot.country("log_time_per_person","Log (Hours per Person)",-5.5,-3.5); fig2

# ggsave(
#   filename = "~/paper-gdpr_whois/paper/plots/graph.audience.png"
#   , plot = fig2
#   , width = 5, height = 4)


#german traffic inside vs outside EU
line_plot.germany <- function (x_var, name, bottom, top) {
  dt.usage.combined_total %>% filter(date >= as.Date('2018-01-01') & ss_60==1&website.complete==1 & !is.na(io_eu_chr)) %>% 
    summarySE(measurevar = x_var, groupvars = c('date','io_eu_chr')) %>%
    ggplot(aes( x = date, y = get(x_var), color = io_eu_chr , linetype = ifelse(date <= as.Date('2018-05-25'),'Before','After GDPR'))) + 
    geom_line() + geom_errorbar(aes(ymin= get(x_var)-ci, ymax = get(x_var) + ci) , width = 0.1) +
    theme_bw() + scale_color_tableau() + expand_limits(y = c(bottom, top))+
    geom_vline(xintercept  = as.Date('2018-05-15'), linetype='dashed') + stat_smooth(method = 'lm')+
    annotate("text", x=as.Date('2018-05-22'), y= (bottom +0.8), label="GDPR       25-05-2018", angle=90, size = 10/.pt)+
    labs( y = name, x = 'Month in 2018', color = 'German Traffic') +
    scale_color_manual(values = c("dark blue", "#99CCFF"), labels = c("Inside EU", "Outside EU")) +
    theme(axis.text=element_text(size=14), axis.title=element_text(size=14), legend.text= element_text(size=24), legend.title=element_text(size=24),legend.position = "bottom", text = element_text(family = "Times New Roman"))
}

fig3 <- line_plot.germany("log_audience","Log (Audience)",  12.5,14); fig3
fig3 <- line_plot.germany("log_pages_per_person","Log (Pages per Person)", 1.5,3); fig3
fig3 <- line_plot.germany("log_sessions_per_person","Log (Sessions per Person)",0,1.5 ); fig3
fig3 <- line_plot.germany("log_time_per_person","Log (Hours per Person)",-5.5,-4); fig3

# ggsave(
#   filename = "~/paper-gdpr_whois/paper/plots/graph.germany.hours.png"
#   , plot = fig3
#   , width = 5, height = 4)


## ----------------------------------------------------------------------------
##-- Regression analysis
## ----------------------------------------------------------------------------

## ----------------------------------------------------------------------------
##-- Compliance vs non-compliance domain level
## ----------------------------------------------------------------------------

#Cookies in JUNE
#Difference-in-differences

DiD_geo <- function (vardep) {
  data1<-dt.usage.combined_total %>%  filter(date >= as.Date('2018-01-01') & website.complete==1 & country=='Germany' & ss_60==1 & !is.na(cookie_june) )
  out_per0<-felm(as.formula(paste(vardep, " ~ period+cookie_june + gdpr_1:cookie_june  | id | 0 | id")), data =data1) 
  out_per1<-felm(as.formula(paste(vardep, " ~ period + cookie_june:period+gdpr_1:cookie_june  | id | 0 | id")), data =data1) 
  out_0<-felm(as.formula(paste(vardep, " ~ feb+mar+apr+may+jun+jul+aug+sep+oct+nov+cookie_june + gdpr_1:cookie_june  | id | 0 | id")), data =data1) 
  out_1<-felm(as.formula(paste(vardep, " ~ feb+mar+apr+may+jun+jul+aug+sep+oct+nov + cookie_june + cookie_june:period +  gdpr_1:cookie_june  | id | 0 | id")), data =data1 ) 
  out_2<-felm(as.formula(paste(vardep, " ~ feb+mar+apr+may+jun+jul+aug+sep+oct+nov+ cookie_june + cookie_june:period + feb:cookie_june + mar:cookie_june +apr:cookie_june+ gdpr_1:cookie_june  | id | 0 | id")), data = data1 ) 
  stargazer(out_per0, out_per1, out_0, out_1, out_2, type='text',dep.var.labels=c("1","2","3","4","5"),  no.space = T
            #, omit = c('may','jun','jul','aug','sep','oct','nov') 
            #, covariate.labels = c("feb", "mar", "apr", ,'period'"Germany", "Germany*Period",  "February*Germany", "March*Germany", "April*Germany", "GDPR*Germany" )
            , title = c("Difference-in-difference")
            #, out  = paste("~/paper-gdpr_whois/paper/tables/results.did.country.",vardep,".tex")
            )
}


DiD_geo("log_audience")
DiD_geo("log_pages_per_person")
DiD_geo("log_sessions_per_person")
DiD_geo("log_time_per_person")


#Synthetic control method

SyC_geo <- function (vardep) {
  gsynth(as.formula(paste(vardep, "~ D4")),
         index=c("id", "date"),
         data=dt.usage.combined_total %>% filter(website.complete==1& country=='Germany' & ss_60==1 & !is.na(cookie_june)),
         r =4,
         se=TRUE,
         nboots=500,
         min.T0=9,
         CV = TRUE, k=10)
}

OUT_SyC_geo<- SyC_geo("log_audience")
OUT_SyC_geo<- SyC_geo("log_pages_per_person")
OUT_SyC_geo<- SyC_geo("log_sessions_per_person")
OUT_SyC_geo<- SyC_geo("log_time_per_person")

plot(OUT_SyC_geo,type="gap")
plot(OUT_SyC_geo,type="counterfactual")
plot(OUT_SyC_geo,type="raw")
print(OUT_SyC_geo)
OUT_SyC_geo$est.att
OUT_SyC_geo$est.avg
dev.off()


#Cookies in NOVEMBER
#Difference-in-differences

DiD_geo_nov <- function (vardep) {
  data1<-dt.usage.combined_total %>%  filter(date >= as.Date('2018-01-01') & website.complete==1 & country=='Germany' & ss_60==1 & !is.na(cookie_nov) )
  out_per0<-felm(as.formula(paste(vardep, " ~ period+cookie_nov + gdpr_1:cookie_nov  | id | 0 | id")), data =data1) 
  out_per1<-felm(as.formula(paste(vardep, " ~ period + cookie_nov:period+gdpr_1:cookie_nov  | id | 0 | id")), data =data1) 
  out_0<-felm(as.formula(paste(vardep, " ~ feb+mar+apr+may+jun+jul+aug+sep+oct+nov+cookie_nov + gdpr_1:cookie_nov  | id | 0 | id")), data =data1) 
  out_1<-felm(as.formula(paste(vardep, " ~ feb+mar+apr+may+jun+jul+aug+sep+oct+nov + cookie_nov + cookie_nov:period +  gdpr_1:cookie_nov  | id | 0 | id")), data =data1 ) 
  out_2<-felm(as.formula(paste(vardep, " ~ feb+mar+apr+may+jun+jul+aug+sep+oct+nov+ cookie_nov + cookie_nov:period + feb:cookie_nov + mar:cookie_nov +apr:cookie_nov+ gdpr_1:cookie_nov  | id | 0 | id")), data = data1 ) 
  stargazer(out_per0, out_per1, out_0, out_1, out_2, type='text',dep.var.labels=c("1","2","3","4","5"),  no.space = T
            #, omit = c('may','jun','jul','aug','sep','oct','nov') 
            #, covariate.labels = c("feb", "mar", "apr", ,'period'"Germany", "Germany*Period",  "February*Germany", "March*Germany", "April*Germany", "GDPR*Germany" )
            , title = c("Difference-in-difference")
            #, out  = paste("~/paper-gdpr_whois/paper/tables/results.did.country.",vardep,".tex")
            )
}


DiD_geo_nov("log_audience")
DiD_geo_nov("log_pages_per_person")
DiD_geo_nov("log_sessions_per_person")
DiD_geo_nov("log_time_per_person")


#logit regressions

data2<-dt.usage.combined_total %>% filter(!is.na(cookie_june)& !is.na(cookie_nov)&country=='Germany') %>% 
  group_by(id) %>%
  summarise(cookie_june=mean(cookie_june), cookie_nov=mean(cookie_nov), 
            harm=mean(harm), copyright= mean(copyright), 
            malicious=mean(malicious), phishing=mean(PS)) %>% 
  ungroup()

out.logit.june <- glm(cookie_june~harm, data = data2)
out.logit.nov <- glm(cookie_nov~harm, data = data2)

out.logit.june.h <- glm(cookie_june~copyright+malicious+phishing, data = data2)
out.logit.nov.h <- glm(cookie_nov~copyright+malicious+phishing, data = data2)

stargazer(out.logit.june, out.logit.june.h, out.logit.nov, out.logit.nov.h, type = 'text', no.space = TRUE, header = FALSE)

#APE
out.logit.june <- logitmfx(cookie_june~harm , data = data2, atmean = FALSE) 
out.logit.nov <- logitmfx(cookie_nov~harm , data = data2, atmean = FALSE) 
out.logit.june.h <- logitmfx(cookie_june~copyright+malicious+phishing , data = data2, atmean = FALSE) 
out.logit.nov.h <- logitmfx(cookie_nov~copyright+malicious+phishing , data = data2, atmean = FALSE) 
out.logit.june
out.logit.nov
out.logit.june.h
out.logit.nov.h

## ----------------------------------------------------------------------------
##-- EU vs US domain level
## ----------------------------------------------------------------------------

#Difference-in-differences

DiD_eu_us <- function (vardep) {
  data1<-dt.usage.combined_total %>%  filter(date >= as.Date('2018-01-01') & website.complete==1 & geography!="Other" & ss_60==1) 
  out_per0<-felm(as.formula(paste(vardep, " ~ period+eu + gdpr_1:eu  | 0 | 0 | id")), data =data1) 
  out_per1<-felm(as.formula(paste(vardep, " ~ period +eu+ eu:period+gdpr_1:eu  | 0 | 0 | id")), data =data1) 
  out_0<-felm(as.formula(paste(vardep, " ~ feb+mar+apr+may+jun+jul+aug+sep+oct+nov+eu + gdpr_1:eu  | id | 0 | id")), data =data1) 
  out_1<-felm(as.formula(paste(vardep, " ~ feb+mar+apr+may+jun+jul+aug+sep+oct+nov + eu + eu:period +  gdpr_1:eu  | id | 0 | id")), data =data1 ) 
  out_2<-felm(as.formula(paste(vardep, " ~ feb+mar+apr+may+jun+jul+aug+sep+oct+nov+ eu + eu:period + feb:eu + mar:eu +apr:eu+ gdpr_1:eu  | id | 0 | id")), data = data1 ) 
  stargazer(out_per0, out_per1, out_0, out_1, out_2, type='text',dep.var.labels=c("1","2","3","4","5"),  no.space = T
            #, omit = c('may','jun','jul','aug','sep','oct','nov') 
            #, covariate.labels = c("feb", "mar", "apr", ,'period'"Germany", "Germany*Period",  "February*Germany", "March*Germany", "April*Germany", "GDPR*Germany" )
            , title = c("Difference-in-difference")
            #, out  = paste("~/paper-gdpr_whois/paper/tables/results.did.country.",vardep,".tex")
            )
}


DiD_eu_us("log_audience")
DiD_eu_us("log_pages_per_person")
DiD_eu_us("log_sessions_per_person")
DiD_eu_us("log_time_per_person")

#Synthetic control method

SyC_eu_us <- function (vardep) {
  gsynth(as.formula(paste(vardep, "~ D2")),
         index=c("id", "date"),
         data=dt.usage.combined_total %>% filter(website.complete==1& geography!="Other"&ss_60==1),
         r =4,
         se=TRUE,
         nboots=500,
         min.T0=9,
         CV = TRUE, k=10)
}

OUT_SyC_eu_us<- SyC_eu_us("log_audience")
OUT_SyC_eu_us<- SyC_eu_us("log_pages_per_person")
OUT_SyC_eu_us<- SyC_eu_us("log_sessions_per_person")
OUT_SyC_eu_us<- SyC_eu_us("log_time_per_person")

plot(OUT_SyC_eu_us,type="gap")
plot(OUT_SyC_eu_us,type="counterfactual")
plot(OUT_SyC_eu_us,type="raw")
print(OUT_SyC_eu_us)
OUT_SyC_eu_us$est.att
OUT_SyC_eu_us$est.avg
dev.off()


## ----------------------------------------------------------------------------
##-- Germany inside vs outside EU domain level
## ----------------------------------------------------------------------------

#Difference-in-differences

DiD_ger <- function (vardep) {
  data1<-dt.usage.combined_total %>%  filter(date >= as.Date('2018-01-01') &ss_60==1& website.complete==1 & !is.na(io_eu)) 
  out_per0<-felm(as.formula(paste(vardep, " ~ period+io_eu + gdpr_1:io_eu  | 0 | 0 | id")), data =data1) 
  out_per1<-felm(as.formula(paste(vardep, " ~ period +io_eu+ io_eu:period+gdpr_1:io_eu  | 0 | 0 | id")), data =data1) 
  out_0<-felm(as.formula(paste(vardep, " ~ feb+mar+apr+may+jun+jul+aug+sep+oct+nov+io_eu + gdpr_1:io_eu  | id | 0 | id")), data =data1) 
  out_1<-felm(as.formula(paste(vardep, " ~ feb+mar+apr+may+jun+jul+aug+sep+oct+nov + io_eu + io_eu:period +  gdpr_1:io_eu  | id | 0 | id")), data =data1 ) 
  out_2<-felm(as.formula(paste(vardep, " ~ feb+mar+apr+may+jun+jul+aug+sep+oct+nov+ io_eu + io_eu:period + feb:io_eu + mar:io_eu +apr:io_eu+ gdpr_1:io_eu  | id | 0 | id")), data = data1 ) 
  stargazer(out_per0, out_per1, out_0, out_1, out_2, type='text',dep.var.labels=c("1","2","3","4","5"),  no.space = T
            #, omit = c('may','jun','jul','aug','sep','oct','nov') 
            #, covariate.labels = c("feb", "mar", "apr", ,'period'"Germany", "Germany*Period",  "February*Germany", "March*Germany", "April*Germany", "GDPR*Germany" )
            , title = c("Difference-in-difference")
            #, out  = paste("~/paper-gdpr_whois/paper/tables/results.did.country.",vardep,".tex")
            )
}


DiD_ger("log_audience")
DiD_ger("log_pages_per_person")
DiD_ger("log_sessions_per_person")
DiD_ger("log_time_per_person")

#Synthetic control method

SyC_ger <- function (vardep) {
  gsynth(as.formula(paste(vardep, "~ D3")),
         index=c("id", "date"),
         data=dt.usage.combined_total %>% filter(website.complete==1&ss_60==1& !is.na(io_eu)),
         r =4,
         se=TRUE,
         nboots=500,
         min.T0=9,
         CV = TRUE, k=10)
}

OUT_SyC_ger<- SyC_ger("log_audience")
OUT_SyC_ger<- SyC_ger("log_pages_per_person")
OUT_SyC_ger<- SyC_ger("log_sessions_per_person")
OUT_SyC_ger<- SyC_ger("log_time_per_person")

plot(OUT_SyC_ger,type="gap")
scm.plot<-plot(OUT_SyC_ger,type="counterfactual")
plot(OUT_SyC_ger,type="raw")
print(OUT_SyC_ger)
OUT_SyC_ger$est.att
OUT_SyC_ger$est.avg
dev.off()

