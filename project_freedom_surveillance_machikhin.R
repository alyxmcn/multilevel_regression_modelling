## Alexey Machikhin
## Multilevel Modelling Project

## ----------------------------------------------------------------------------------------
library(readr)
library(lme4)
library(performance)
library(dplyr)
library(tidyr)
library(countrycode)
library(sjPlot)
library(lavaan)
library(semTools)
library(corrplot)
library(factoextra)
library(corrplot)
library(readxl)
library(table1)
library(data.table)
library(tidyverse)
library(psych)
library(kableExtra)
library(lessR)
library(misty)
library(ggstatsplot)
library(interplot)
library(ggthemes)


## ----------------------------------------------------------------------------------------
wvs  <-  read_csv("data/WVS_Cross-National_Wave_7_csv_v4_0.csv")
gdpdf <- read_csv("data/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_4701206.csv")
globalpeace <- read_excel("data/GPI-2022-overall-scores-and-domains-2008-2022.xlsx",
                          sheet = "Overall Scores")
ginidf <- read_csv("data/API_SI.POV.GINI_DS2_en_csv_v2_4701295.csv")
libdemdf <- read_excel("data/V-Dem-CY-Core-v12.xlsx")
lifexpdf <- read_csv("data/API_SP.DYN.LE00.IN_DS2_en_csv_v2_4770434.csv")
infantmortdf <- read_csv("data/API_SP.DYN.IMRT.IN_DS2_en_csv_v2_4770442.csv")


## ----------------------------------------------------------------------------------------
wvs$freedom_vs_security = as.factor(wvs$Q150)


## ----------------------------------------------------------------------------------------
wvs <- wvs %>% mutate(freedom_vs_security=dplyr::recode(freedom_vs_security, '2' = 'Security',
                                                   '1' = 'Freedom'))


## ----------------------------------------------------------------------------------------
table(wvs$Q150)
table(wvs$freedom_vs_security) # correct re-coding


## ----------------------------------------------------------------------------------------
wvs$cntry = wvs$B_COUNTRY_ALPHA


## ----------------------------------------------------------------------------------------
wvs$country = countrycode(wvs$B_COUNTRY, origin = 'wvs', destination = "p4.name")
# some countries or territories are not matched
# but also there are not specific data from World Bank or other organizations for those countries or territories (like for Andorra or Puerto Rico)
# plus, renaming of countries or territories is conducted just for descriptive plots


## ----------------------------------------------------------------------------------------
wvs$country[wvs$country == "Myanmar (Burma)"] <- "Burma"
wvs$country[wvs$country == "Korea South"] <- "S. Korea"


## ----------------------------------------------------------------------------------------
# Plot to demonstrate difference in freedom-security preference between countries -----
x <- barplot(table(wvs$freedom_vs_security, wvs$country), xaxt="n", las=2, legend.text = T,
             col = c("cyan3", "darksalmon"))
labs <- paste(names(table(wvs$country)))
text(cex=0.80, x=x-0, y=-600, labs, xpd=TRUE, srt=90)


## ----------------------------------------------------------------------------------------
# DVs renaming -----
wvs$cctv_surveillance <- wvs$Q196
wvs$internet_surveillance <- wvs$Q197
wvs$overall_surveillance <- wvs$Q198


## ----------------------------------------------------------------------------------------
## Considerations on Dependent Variable (support of state surveillance) ------

# constructing a single index of attitude towards surveillance -----
# For this, I firstly conduct factor analysis
mod <- 'surv =~ cctv_surveillance + internet_surveillance + overall_surveillance'

fit.wlsmv <- lavaan::cfa(mod, 
                         data = wvs,
                         ordered = T,
                         missing = "listwise") 

# since indicators are ord. categor. (WLSMV estimation method)
# "listwise' is for NAs

semTools::reliability(fit.wlsmv, return.total = T) # high level alphas and omegas

#wvs$surv_index <- (wvs$Q196 + wvs$Q197 + wvs$Q198) / 3
#summary(wvs$surv_index)

## However, let's consider face validity of items (Q196-Q198): do they mesure the same? ----
# PCA
fit.pc <- princomp(~ cctv_surveillance + internet_surveillance + overall_surveillance, 
                   data = wvs, cor = FALSE,
                   na.action = na.omit) 

fviz_pca_var(fit.pc,
             col.var = "cos2",# Color by cos2 statistic
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
# Factor map with two dimensions suggests that there are two groups of indicators:
    #1) cctv_surv correlates positively with both PCs;
    #2) internet and overall surv. that correlate positively with PC1 and negatively with PC2.

# Moreover, cctv_surv is too distinct from a well-correlated pair of internet_surv and overall_syrv
# Therefore, in fact, Q196-198 measure different tupes of support towards state surveillance
# Surveillance without knowledge is perceived in exactly the same way as surveillance in internet

# Hence, I should not combine indicators into a single index, but distinguish two types of DVs: 1) support of public surveillance (via cctv) and 2) support of private surveillance (in Internet).


## ----------------------------------------------------------------------------------------
# Here I reverse surveillance-related DVs for convenience of use -------
wvs1 <- wvs
likertscale <- 1:4

wvs <- wvs %>%
  mutate(cctv_surveillance = min(likertscale) - cctv_surveillance + max(likertscale))

table(wvs1$cctv_surveillance)
table(wvs$cctv_surveillance) # correct reversing 

wvs <- wvs %>%
  mutate(internet_surveillance = min(likertscale) - internet_surveillance + max(likertscale))

table(wvs1$internet_surveillance)
table(wvs$internet_surveillance) # correct reversing 


## ----------------------------------------------------------------------------------------
aggrdata <- aggregate(cctv_surveillance ~ country, data = wvs, FUN = "mean")
aggrdata1 <- aggrdata[order(aggrdata$cctv_surveillance),]
dotchart(aggrdata1$cctv_surveillance, labels = aggrdata1$country, pch = 21,
         cex = 0.4, 
         main = "Countries' Support of CCTV Surveillance (mean values)",
         pt.cex = 0.8)


## ----------------------------------------------------------------------------------------
aggrdata2 <- aggregate(internet_surveillance ~ country, data = wvs, FUN = "mean")
aggrdata3 <- aggrdata2[order(aggrdata2$internet_surveillance),]
dotchart(aggrdata3$internet_surveillance, labels = aggrdata3$country, pch = 21,
         cex = 0.4, 
         main = "Countries' Support of Internet Surveillance (mean values)",
         pt.cex = 0.8)


## ----------------------------------------------------------------------------------------
wvs$age <- wvs$Q262
summary(wvs$age) # 103 y.o. - maybe but looks suspicious 
hist(wvs$age)
wvs <- wvs %>% filter(age <= 90)
summary(wvs$age) # ok
wvs$age_scaled <- scale(wvs$age)


## ----------------------------------------------------------------------------------------
wvs$gender <- as.factor(wvs$Q260)
wvs <- wvs %>% mutate(gender=dplyr::recode(gender, '2' = 'Female',
                                                   '1' = 'Male'))
table(wvs$Q260)
table(wvs$gender) #correct re-coding

## ----------------------------------------------------------------------------------------
wvs$income <- wvs$Q288
hist(wvs$income)
wvs$income_scaled <- wvs$income


## ----------------------------------------------------------------------------------------
wvs$edu <- wvs$Q275
hist(wvs$edu)
wvs$edu_scaled <- scale(wvs$edu)


## ----------------------------------------------------------------------------------------
wvs$individ_security <- wvs$Q131


## ----------------------------------------------------------------------------------------
# Individual Sense of Security reversing -------
wvs <- wvs %>%
  mutate(individ_security = min(likertscale) - individ_security + max(likertscale))


## ----------------------------------------------------------------------------------------
aggrdata4 <- aggregate(individ_security ~ country, data = wvs, FUN = "mean")
aggrdata5 <- aggrdata4[order(aggrdata4$individ_security),]
dotchart(aggrdata5$individ_security, labels = aggrdata5$country, pch = 21,
         cex = 0.4, 
         main = "Means of Individual Sense of Security Scores by Country",
         pt.cex = 0.8)
# individ. scores of security should be further centered within clusters


## ----------------------------------------------------------------------------------------
wvs$individ_security_cwc <- center(wvs$individ_security, type = "CWC", 
       cluster = wvs$cntry)
# CWC for centering within cluster
summary(wvs$individ_security_cwc) # correct centering (mean as 0)


## ----------------------------------------------------------------------------------------
wvs_small <- dplyr::select(wvs, cctv_surveillance, internet_surveillance,
                           freedom_vs_security, individ_security,
                           individ_security_cwc, gender, age, age_scaled,
                           income, income_scaled, edu, edu_scaled,
                           country, cntry)


## ----------------------------------------------------------------------------------------
# GPD for Existential Security Index (ESI) -----
gdpdf$GDP <- (gdpdf$`2017` + gdpdf$`2018` + gdpdf$`2019` + gdpdf$`2020` + gdpdf$`2021`) / 5
# mean GPD for the period 2017-2021
summary(gdpdf$GDP)

gdpdf$Code <- gdpdf$`Country Code` 

gdpdata <- dplyr::select(gdpdf, Code, GDP)

names(gdpdata) <- c("cntry", "GDP")

wvs7 <- merge(wvs_small, gdpdata, all.x = T)

tapply(wvs7$GDP, wvs7$cntry, FUN = "mean")

#gdpdf[gdpdf$`Country Code` == "VEN", ] # NAs for the period 2017-2021 (Venezuela)
# No Taiwan, Puerto Rico in the GDP dataset


## ----------------------------------------------------------------------------------------
## Life Expectancy Rate for ESI -------
lifexpdf$LE <- (lifexpdf$'2017' + lifexpdf$'2018' + lifexpdf$'2019' + lifexpdf$'2020') / 4
# mean LE for the period 2017-2020
# unfortunately, there is no data for 2021 (WHO does not have this data for 2021 too)
summary(lifexpdf$LE) # ok

lifeexpdata <- dplyr::select(lifexpdf, 'Country Code', LE)

names(lifeexpdata) <- c("cntry", "LE")

wvs7 <- merge(wvs7, lifeexpdata, all.x = T)

tapply(wvs7$LE, wvs7$cntry, FUN = "mean") # ok (except of Taiwan because there is no data by World Bank for this country)


## ----------------------------------------------------------------------------------------
## Infant mortality rate for ESI -----
infantmortdf$IM <- (infantmortdf$'2017' + infantmortdf$'2018' + infantmortdf$'2019' +
                      infantmortdf$'2020') / 4
# mean LE for the period 2017-2020
# unfortunately, there is no data for 2021 (WHO does not have this data for 2021 too)
summary(infantmortdf$IM) # ok

infantdata <- dplyr::select(infantmortdf, 'Country Code', IM)

names(infantdata) <- c("cntry", "IM")

wvs7 <- merge(wvs7, infantdata, all.x = T)

tapply(wvs7$IM, wvs7$cntry, FUN = "mean") # ok (except of Taiwan, Puerto Rico and Macao because there is no data by World Bank for these countries or territories)
# also there is no data on infant mortality in Hong Kong by World Bank


## ----------------------------------------------------------------------------------------
# Gini Index -----
ginidf$Gini <- (ginidf$`2017` + ginidf$`2018` + ginidf$`2019` + ginidf$`2020`) / 4
summary(ginidf$Gini)

ginidf$Code <- ginidf$`Country Code`

Ginidata <- dplyr::select(ginidf, Code, Gini)

names(Ginidata) <- c("cntry", "Gini")

wvs7 <- merge(wvs7, Ginidata, all.x = T)

tapply(wvs7$Gini, wvs7$cntry, FUN = "mean")

# Lots of missing values from World Bank data for many countries. Therefore, this country-level variable is not used. 


## ----------------------------------------------------------------------------------------
# Liberal Democracy Index
libdemdata <- libdemdf %>% 
                 dplyr::select(year,country_name,country_text_id,v2x_libdem) %>%
                 tidyr::pivot_wider(names_from=year, values_from=v2x_libdem) 

libdemdata$LibDemIndex <- (libdemdata$`2017` + libdemdata$`2018` + libdemdata$`2019` +
                             libdemdata$`2020` + libdemdata$`2021`) / 5

LBIdata <- dplyr::select(libdemdata, country_text_id, LibDemIndex)

names(LBIdata) <- c("cntry", "LibDemIndex")

wvs7 <- merge(wvs7, LBIdata, all.x = T)

tapply(wvs7$LibDemIndex, wvs7$cntry, FUN = "mean") # ok (except of Puerto Rico)


## ----------------------------------------------------------------------------------------
# Scaling LibDem Index
wvs7$LibDemIndex_scaled <- scale(wvs7$LibDemIndex)


## ----------------------------------------------------------------------------------------
# Scaling components of ESI
wvs7$GDP_scaled <- scale(wvs7$GDP)
wvs7$LE_scaled <- scale(wvs7$LE)
wvs7$IM_scaled <- scale(wvs7$IM)
#summary(wvs7$GDP_scaled) #ok
#summary(wvs7$LE_scaled) #ok
#summary(wvs7$IM_scaled) #ok


## ----------------------------------------------------------------------------------------
### Creating ISE ------------

# PCA 
cor.matrix <- cor(wvs7[c("GDP_scaled", "LE_scaled", "IM_scaled")], use = "pairwise.complete.obs")
corrplot::corrplot(cor.matrix, method = "number", addrect = 2)

fit.pc <- princomp(~ GDP_scaled + LE_scaled + IM_scaled, 
                   data = wvs7, cor = FALSE,
                   na.action = na.omit) 
summary(fit.pc) # only one component is reqired

fviz_eig(fit.pc,  choice = "eigenvalue",
         addlabels = TRUE, ylim = c(0,3),linecolor ="red") + 
  geom_hline(yintercept = 1, linetype = "dashed") # 'elbow' after the 1st component

fviz_eig(fit.pc,  choice = "variance",
         addlabels = TRUE,ylim = c(0,90)) # 79.8% of varience is explained by latent ESI factor

# EFA
fit.efa1 <- factanal(~ GDP_scaled + LE_scaled + IM_scaled, 
                     factors = 1,  data = wvs7, 
                    cor = F,
                    na.action = na.omit) 
print(fit.efa1, digits=2, cutoff=.3, sort=TRUE)
# Scores of factor loadings turned similar to (Inglehart et al., 2017)
# Large and negative factor loading score for Infant mortality rate as an item is not surprising, since there is obvious negative association between latent Existential Security factor and Infant mortality
# Hence, when creating ISE, scaled infant mortality should be subtracted from sum of scaled GDP and LE. Thus, additional IM scores should not increase existential security
# 74% - proportion of explained variance by a latent factor
# These all suggests that the latent structure under these indicators consists of a single factor


## ----------------------------------------------------------------------------------------
## Constructing ISE ---------
wvs7$ESI <- (wvs7$GDP_scaled + wvs7$LE_scaled) - wvs7$IM_scaled
summary(wvs7$ESI) # ok, I don't think that I should scale ESI once again, since this index is made of scaled values

aggrdata00 <- aggregate(ESI ~ cntry, data = wvs7, FUN = "unique")
aggrdata00$ESI <- aggrdata00$V1
aggrdata00 # for 53 countries (5 countries/territories didn't have items for ESI)


## ----------------------------------------------------------------------------------------
aggrdata6 <- aggregate(ESI ~ country, data = wvs7, FUN = "unique")
aggrdata6$ESI <- aggrdata6$V1
aggrdata7 <- aggrdata6[order(aggrdata6$ESI),]
dotchart(aggrdata7$ESI, labels = aggrdata7$country, pch = 21,
         cex = 0.4, 
         main = "Existential Security Index by Country",
         pt.cex = 0.8)


## ----------------------------------------------------------------------------------------
wvs7.to.use1 <- wvs7 %>% dplyr::select(cntry, cctv_surveillance, internet_surveillance,
                                      freedom_vs_security, individ_security,
                                      individ_security_cwc, gender, age, age_scaled,
                                      edu, edu_scaled, 
                                      income, income_scaled, ESI, LibDemIndex, 
                                      LibDemIndex_scaled)
wvs7.to.use <- na.omit(wvs7.to.use1)


## ----------------------------------------------------------------------------------------
# saving final df for analysis --------------
write.csv(wvs7.to.use , "data/data_complete.csv", row.names=FALSE)


## ----------------------------------------------------------------------------------------
wvs7.to.use <- read_csv("data/data_complete.csv")

wvs7.to.use$freedom_vs_security <- as.factor(wvs7.to.use$freedom_vs_security)
wvs7.to.use$freedom_vs_security <- relevel(wvs7.to.use$freedom_vs_security, ref = "Security")

wvs7.to.use$gender <- as.factor(wvs7.to.use$gender)


## ----------------------------------------------------------------------------------------
table1(~ as.factor(cctv_surveillance) + as.factor(internet_surveillance) + 
         freedom_vs_security + gender, data = wvs7.to.use)


## ----------------------------------------------------------------------------------------
desc = describe(wvs7[c('cctv_surveillance', 'internet_surveillance',
                                      'individ_security',
                                      'individ_security_cwc', 'age', 'age_scaled',
                                      'edu', 'edu_scaled', 
                                      'income', 'income_scaled', 'ESI', 'LibDemIndex', 
                                      'LibDemIndex_scaled')])

class(desc) <- "data.frame"

desc = desc[,c("n", "mean", "median", "sd", "min", "max", "skew", "kurtosis")]

desc %>% kbl(digits = 2) %>%
  kable_classic(full_width = FALSE, html_font = "Cambria")


## ----------------------------------------------------------------------------------------
cols <- hcl.colors(length(unique(wvs7.to.use$cntry)), "Zissou 1")
PieChart(cntry, hole = 0.6, values = "%", data = wvs7.to.use, main = NULL, fill = cols,
         labels_cex = 0.4*2, cex = 0.6)
# almost the same shares of respondents fore each country (1-2%), but the largest ones are for Indonesia, China and Canada (4%, 4% and 6% respectively)


## ----------------------------------------------------------------------------------------
tab_corr(wvs7.to.use[c('cctv_surveillance', 'internet_surveillance',
                                      'individ_security_cwc', 
                                      'age_scaled',
                                      'edu_scaled', 
                                      'income_scaled', 
                                      'ESI', 
                                      'LibDemIndex_scaled')], 
                                      corr.method = "pearson")


## ----------------------------------------------------------------------------------------
ggpiestats(
  data = wvs7.to.use,
  x = cctv_surveillance,
  y = freedom_vs_security, 
  legend.title = "CCTV surveillance\n support"
  ) + # further modification with `{ggplot2}` commands
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      color = "black",
      size = 14,
      hjust = 0
    )
  )


## ----------------------------------------------------------------------------------------
ggpiestats(
  data = wvs7.to.use,
  x = internet_surveillance,
  y = freedom_vs_security, 
  legend.title = "Internet surveillance\n support"
  ) + # further modification with `{ggplot2}` commands
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      color = "black",
      size = 14,
      hjust = 0
    )
  )


## ----------------------------------------------------------------------------------------
m_cctv_0 = lmer(cctv_surveillance ~ (1|cntry), data = wvs7.to.use)


## ----------------------------------------------------------------------------------------
icc(m_cctv_0) # ICC = 14% > 10%


## ----------------------------------------------------------------------------------------
m_cctv_1 = lmer(cctv_surveillance ~ freedom_vs_security + (1|cntry), data = wvs7.to.use)


## ----------------------------------------------------------------------------------------
summary(m_cctv_1)


## ----------------------------------------------------------------------------------------
BIC(m_cctv_0)
BIC(m_cctv_1) # model 2 became better


## ----------------------------------------------------------------------------------------
m_cctv_2 = lmer(cctv_surveillance ~ freedom_vs_security + individ_security_cwc + 
                  (1|cntry), data = wvs7.to.use)


## ----------------------------------------------------------------------------------------
summary(m_cctv_2)


## ----------------------------------------------------------------------------------------
anova(m_cctv_1, m_cctv_2) # m_cctv_2 significantly improves model


## ----------------------------------------------------------------------------------------
m_cctv_3 = lmer(cctv_surveillance ~ freedom_vs_security + individ_security_cwc + 
                  gender + (1|cntry), data = wvs7.to.use)


## ----------------------------------------------------------------------------------------
summary(m_cctv_3)


## ----------------------------------------------------------------------------------------
anova(m_cctv_2, m_cctv_3) # m_cctv_3 is better


## ----------------------------------------------------------------------------------------
m_cctv_4 = lmer(cctv_surveillance ~ freedom_vs_security + individ_security_cwc + 
                  gender + age_scaled + (1|cntry), data = wvs7.to.use)


## ----------------------------------------------------------------------------------------
summary(m_cctv_4)


## ----------------------------------------------------------------------------------------
anova(m_cctv_3, m_cctv_4) # m_cctv_4 is better


## ----------------------------------------------------------------------------------------
m_cctv_05 = lmer(cctv_surveillance ~ freedom_vs_security + individ_security_cwc + 
                  gender + age_scaled + edu_scaled + (1|cntry), data = wvs7.to.use)


## ----------------------------------------------------------------------------------------
summary(m_cctv_05)


## ----------------------------------------------------------------------------------------
anova(m_cctv_4, m_cctv_05) # education is insignificant factor and does not improve the model


## ----------------------------------------------------------------------------------------
m_cctv_005 = lmer(cctv_surveillance ~ freedom_vs_security + individ_security_cwc + 
                  gender + age_scaled + income_scaled + (1|cntry), data = wvs7.to.use)


## ----------------------------------------------------------------------------------------
summary(m_cctv_005)


## ----------------------------------------------------------------------------------------
anova(m_cctv_4, m_cctv_005) # Income is not significant and does not improve the model


## ----------------------------------------------------------------------------------------
m_cctv_5 = lmer(cctv_surveillance ~ freedom_vs_security + individ_security_cwc + 
                  gender + age_scaled + ESI + (1|cntry), data = wvs7.to.use)


## ----------------------------------------------------------------------------------------
summary(m_cctv_5)


## ----------------------------------------------------------------------------------------
anova(m_cctv_4, m_cctv_5) # ESI is not significant and does not improve the model, but let's see if interaction terms would change it


## ----------------------------------------------------------------------------------------
m_cctv_06 = lmer(cctv_surveillance ~ freedom_vs_security + individ_security_cwc + 
                  gender + age_scaled + ESI + LibDemIndex_scaled +
                  (1|cntry), data = wvs7.to.use)


## ----------------------------------------------------------------------------------------
summary(m_cctv_06)


## ----------------------------------------------------------------------------------------
anova(m_cctv_5, m_cctv_06) # LibDem Index is not significant and does not improve model


## ----------------------------------------------------------------------------------------
# here I randomize freedom choice
m_cctv_6 = lmer(cctv_surveillance ~ freedom_vs_security + individ_security_cwc + 
                  gender + age_scaled + ESI + (1 + freedom_vs_security|cntry), 
                data = wvs7.to.use)


## ----------------------------------------------------------------------------------------
summary(m_cctv_6)


## ----------------------------------------------------------------------------------------
anova(m_cctv_5, m_cctv_6) # Randomization of freedom choice significantly improves the model 


## ----------------------------------------------------------------------------------------
# vizualization of distribution of random effects
plot_model(m_cctv_6, type = "re",  
           sort = "freedom_vs_securityFreedom",
           sort.int = "cntry (intercept)",
           colors = c("darksalmon", "cyan3")) +
  theme_sjplot() 


## ----------------------------------------------------------------------------------------
# here I add interaction terms between freedom-security and ESS
m_cctv_7 = lmer(cctv_surveillance ~ freedom_vs_security*ESI + individ_security_cwc + 
                  gender + age_scaled + (1 + freedom_vs_security|cntry), 
                data = wvs7.to.use)


## ----------------------------------------------------------------------------------------
summary(m_cctv_7)


## ----------------------------------------------------------------------------------------
anova(m_cctv_6, m_cctv_7) # Interaction terms significantly improves the model 


## ----------------------------------------------------------------------------------------
# visualization of cross-level interaction effect
plot_model(m_cctv_7, type = "int",
          colors = c("darksalmon", "cyan3"),
          axis.title = c('Freedom or Security?', 'CCTV Surveillance Support'),
          title = 'Predicted values of CCTV surveillance support') + 
  theme_sjplot()


## ----------------------------------------------------------------------------------------
tab_model(m_cctv_0, m_cctv_1, m_cctv_2, m_cctv_3, m_cctv_4, m_cctv_5, m_cctv_6,
          m_cctv_7, show.ci = F, show.aic = T)


## ----------------------------------------------------------------------------------------
BIC(m_cctv_0)
BIC(m_cctv_1)
BIC(m_cctv_2)
BIC(m_cctv_3) 
BIC(m_cctv_4)
BIC(m_cctv_5)
BIC(m_cctv_6)
BIC(m_cctv_7)


## ----------------------------------------------------------------------------------------
interplot(m_cctv_7, var1 = "freedom_vs_security", var2 = "ESI") + 
 xlab('ESI') +
 ylab('Estimated Coefficient for Freedom Preference') +
  theme_few()


## ----------------------------------------------------------------------------------------
m_int_0 = lmer(internet_surveillance ~ (1|cntry), data = wvs7.to.use)


## ----------------------------------------------------------------------------------------
icc(m_int_0) # ICC = 16% > 10%


## ----------------------------------------------------------------------------------------
m_int_1 = lmer(internet_surveillance ~ freedom_vs_security + (1|cntry), data = wvs7.to.use)


## ----------------------------------------------------------------------------------------
summary(m_cctv_1)


## ----------------------------------------------------------------------------------------
BIC(m_int_0)
BIC(m_int_1) # model 2 became better


## ----------------------------------------------------------------------------------------
m_int_2 = lmer(internet_surveillance ~ freedom_vs_security + individ_security_cwc + 
                  (1|cntry), data = wvs7.to.use)


## ----------------------------------------------------------------------------------------
summary(m_int_2)


## ----------------------------------------------------------------------------------------
anova(m_int_1, m_int_2) # individ_security significantly improves model


## ----------------------------------------------------------------------------------------
m_int_3 = lmer(internet_surveillance ~ freedom_vs_security + individ_security_cwc + 
                  gender + (1|cntry), data = wvs7.to.use)


## ----------------------------------------------------------------------------------------
summary(m_int_3)


## ----------------------------------------------------------------------------------------
anova(m_int_2, m_int_3) # gender improves the model


## ----------------------------------------------------------------------------------------
m_int_4 = lmer(internet_surveillance ~ freedom_vs_security + individ_security_cwc + 
                  gender + age_scaled + (1|cntry), data = wvs7.to.use)


## ----------------------------------------------------------------------------------------
summary(m_int_4)


## ----------------------------------------------------------------------------------------
anova(m_int_3, m_int_4) # m_cctv_4 is better


## ----------------------------------------------------------------------------------------
m_int_5 = lmer(internet_surveillance ~ freedom_vs_security + individ_security_cwc + 
                  gender + age_scaled + edu_scaled + (1|cntry), data = wvs7.to.use)


## ----------------------------------------------------------------------------------------
summary(m_int_5)


## ----------------------------------------------------------------------------------------
anova(m_int_4, m_int_5) # education improves the model


## ----------------------------------------------------------------------------------------
m_int_05 = lmer(internet_surveillance ~ freedom_vs_security + individ_security_cwc + 
                  gender + age_scaled + edu_scaled + income_scaled + (1|cntry),
                data = wvs7.to.use)


## ----------------------------------------------------------------------------------------
summary(m_int_05)


## ----------------------------------------------------------------------------------------
anova(m_int_5, m_int_05) #  income is insignificant predictor, it does not improve the model and even makes it worse according to AIC and BIC


## ----------------------------------------------------------------------------------------
m_int_6 = lmer(internet_surveillance ~ freedom_vs_security + individ_security_cwc + 
                  gender + age_scaled + edu_scaled + ESI + (1|cntry), 
                data = wvs7.to.use)


## ----------------------------------------------------------------------------------------
summary(m_int_6)


## ----------------------------------------------------------------------------------------
anova(m_int_5, m_int_6) # ESI is significant predictor and improves the model


## ----------------------------------------------------------------------------------------
m_int_06 = lmer(internet_surveillance ~ freedom_vs_security + individ_security_cwc + 
                  gender + age_scaled + edu_scaled + ESI + LibDemIndex_scaled +
                  (1|cntry), data = wvs7.to.use)


## ----------------------------------------------------------------------------------------
summary(m_int_06)


## ----------------------------------------------------------------------------------------
anova(m_int_6, m_int_06) # LibDem Index is not significant and does not improve model


## ----------------------------------------------------------------------------------------
# here I randomize individual freedom preference
m_int_7 = lmer(internet_surveillance ~ freedom_vs_security + individ_security_cwc + 
                  gender + age_scaled + edu_scaled + ESI + (1 + freedom_vs_security|cntry), 
                data = wvs7.to.use)


## ----------------------------------------------------------------------------------------
summary(m_int_7)


## ----------------------------------------------------------------------------------------
anova(m_int_6, m_int_7) # Randomization of freedom choice scores significantly improves the model 


## ----------------------------------------------------------------------------------------
# vizualization of distribution of random effects
plot_model(m_int_7, type = "re", sort = "freedom_vs_securityFreedom",
           colors = c("darksalmon", "cyan3")) +
  theme_sjplot()


## ----------------------------------------------------------------------------------------
# here I add interaction effect by ESI
m_int_8 = lmer(internet_surveillance ~ freedom_vs_security*ESI + individ_security_cwc + 
                  gender + age_scaled + edu_scaled + (1 + freedom_vs_security|cntry), 
                data = wvs7.to.use)


## ----------------------------------------------------------------------------------------
summary(m_int_8)


## ----------------------------------------------------------------------------------------
anova(m_int_7, m_int_8) # Interaction terms does not significantly improve the model 


## ----------------------------------------------------------------------------------------
# lets try interaction terms without randomization -------
m_int_8_1 = lmer(internet_surveillance ~ freedom_vs_security*ESI + individ_security_cwc + 
                  gender + age_scaled + edu_scaled + (1 |cntry), 
                data = wvs7.to.use)


## ----------------------------------------------------------------------------------------
summary(m_int_8_1)


## ----------------------------------------------------------------------------------------
anova(m_int_8, m_int_8_1) # Firstly, the model with randomization is better than without


## ----------------------------------------------------------------------------------------
anova(m_int_7, m_int_8_1) # Secondly, the model with randomization but without interaction effect is also better


## ----------------------------------------------------------------------------------------
# visualization of cross-level interaction effect
plot_model(m_int_8_1, type = "int",
          colors = c("cyan3", "darksalmon"),
          axis.title = c('Freedom or Security?', 'Internet Surveillance Support'),
          title = 'Predicted values of Internet surveillance support') + 
  theme_sjplot()


## ----------------------------------------------------------------------------------------
interplot(m_int_8_1, var1 = "freedom_vs_security", var2 = "ESI") + 
 xlab('ESI') +
 ylab('Estimated Coefficient for Freedom Preference') +
  theme_few()


## ----------------------------------------------------------------------------------------
tab_model(m_int_1, m_int_2, m_int_3, m_int_4, m_int_5, m_int_6,
          m_int_7, m_int_8, m_int_8_1, show.ci = F, show.aic = T)


## ----------------------------------------------------------------------------------------
BIC(m_int_0)
BIC(m_int_1)
BIC(m_int_2)
BIC(m_int_3) 
BIC(m_int_4)
BIC(m_int_5)
BIC(m_int_6)
BIC(m_int_7)
BIC(m_int_8)
BIC(m_int_8_1)

