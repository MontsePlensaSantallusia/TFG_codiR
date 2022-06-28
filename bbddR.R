### ARTICLES

#install.packages("sqldf")
#install.packages("dplyr")
#install.packages("ggpubr")

library(sqldf)
library(tidyverse)
library(Hmisc)
library(readxl)
library(plyr)
library(dplyr)
library(scales)
library(kableExtra)
library(gridExtra)
library(stats)
library(gplots)
library(cluster)
library(factoextra)
library(FactoMineR)
library(data.table)
library(ggpubr)

jcr <- read_xlsx("jcr.xlsx", range = "A3:J119", col_names = TRUE)

names (jcr)[1] = "JTitle"
names (jcr)[2] = "AbbTitle"

j1 <- c("Journal of Quantitative Analysis in Sports","J QUANT ANAL SPORTS","2194-6388","1559-0410","SOCIAL SCIENCES, MATHEMATICAL METHODS - ESCI","411","n/a","	Q4","0.38","n/a")
j2 <- c("Journal of Sports Analytics","n/a","n/a","n/a","n/a","n/a","n/a","n/a","n/a","n/a")

jcr = rbind(jcr,j1,j2)

jcr[,1] <- toupper(c(jcr$JTitle)) 

glimpse(jcr)

# CINAHL

cinahl <- read.csv("cinahl.csv",sep=",",header=T)

names (cinahl)[1] = "Title"
names (cinahl)[3] = "STitle"

cinahl[,3] <- toupper(c(cinahl$STitle)) 

cinahl2 <- sqldf('SELECT * FROM cinahl LEFT JOIN jcr ON cinahl.STitle = jcr.JTitle')

cinahlbe <- cinahl2[!is.na(cinahl2$JTitle),]

CINAHL <- cinahlbe[,c(1,2,3,20:28)]

CINAHL <- as.data.frame(CINAHL)

glimpse(CINAHL)

# SPORTDISCUS

sd1 <- read.csv("delivery (1).csv",sep=",",header=T)
sd2 <- read.csv("delivery (2).csv",sep=",",header=T)

sd <- rbind(sd1,sd2)

names (sd)[1] = "Title"
names (sd)[3] = "STitle"

sd[,3] <- toupper(c(sd$STitle)) 

sd2 <- sqldf('SELECT * FROM sd LEFT JOIN jcr ON sd.Stitle = jcr.JTitle')

sdbe <- sd2[!is.na(sd2$JTitle),]

SD <- sdbe[,c(1,2,3,20:28)]

SD <- as.data.frame(SD)

glimpse(SD)

# WOS

wos <- read_excel("wos.xls")

wos <- as.data.frame(wos)

borrar <- c("Authors","Article Title","Source Title")
wos <- wos[,(names(wos) %in% borrar)]
str(wos)

names (wos)[1] = "Author"
names (wos)[2] = "Title"
names (wos)[3] = "STitle"

wos2 <- sqldf('SELECT wos.Title, wos.Author, wos.STitle, jcr.* FROM wos LEFT JOIN jcr ON wos.STitle = jcr.JTitle')

wosbe <- wos2[!is.na(wos2$JTitle),]

WOS <- wosbe[,c(1,2,3,5:13)]

glimpse(WOS)

# PUBMED

pubmed <- read_excel("pubmed.xlsx")

borrar2 <- c("Title","Author(s)","Journal Title")
pubmed <- pubmed[,(names(pubmed) %in% borrar2)]
str(pubmed)

names (pubmed)[1] = "Title"
names (pubmed)[2] = "Author"
names (pubmed)[3] = "STitle"

pubmed[,3] <- toupper(c(pubmed$STitle)) 

pubmed2 <- sqldf('SELECT * FROM pubmed LEFT JOIN jcr ON pubmed.STitle = jcr.JTitle')

pubmedbe <- pubmed2[!is.na(pubmed2$JTitle),]

PUBMED <- pubmedbe[,c(1,2,3,5:13)]

glimpse(PUBMED)

# REPETITS

CINAHL$num <- c(1:nrow(CINAHL))
SD$num <- c(1:nrow(SD)) 
WOS$num <- c(1:nrow(WOS))
PUBMED$num <- c(1:nrow(PUBMED))

SD[3,1] <- "Évaluation des blessures musculosquelettiques en « trail-running » chez deux populations amateurs d'experts et de novices définies par clustering : étude pilote"
SD[1,1] <- "The 'SHRed Injuries Basketball' Neuromuscular Training Warm-up Program Reduces Ankle and Knee Injury Rates by 36% in Youth Basketball"
CINAHL[9,1] <- "The 'SHRed Injuries Basketball' Neuromuscular Training Warm-up Program Reduces Ankle and Knee Injury Rates by 36% in Youth Basketball"
PUBMED[57,1] <- "A CLUSTER RANDOMIZED CONTROLLED TRIAL TO REDUCE OFFICE WORKERS' SITTING TIME: EFFECT ON ACTIVITY OUTCOMES"
PUBMED[18,1] <- "A META-REGRESSION OF THE EFFECTS OF RESISTANCE TRAINING FREQUENCY ON MUSCULAR STRENGTH AND HYPERTROPHY IN ADULTS OVER 60 YEARS OF AGE"
PUBMED[42,1] <- "A METHOD FOR DEVELOPING ORGANISATION-WIDE MANUAL HANDLING BASED PHYSICAL EMPLOYMENT STANDARDS IN A MILITARY CONTEXT"
PUBMED[45,1] <- "A SHOT TAXONOMY IN THE ERA OF TRACKING DATA IN PROFESSIONAL TENNIS"
PUBMED[30,1] <- "ACTIVITY MAPPING OF CHILDREN IN PLAY USING MULTIVARIATE ANALYSIS OF MOVEMENT EVENTS"
PUBMED[24,1] <- "ADOLESCENTS' PERSPECTIVES ON A SCHOOL-BASED PHYSICAL ACTIVITY INTERVENTION: A MIXED METHOD STUDY"
PUBMED[59,1] <- "ADVANCED TREATMENT MONITORING FOR OLYMPIC-LEVEL ATHLETES USING UNSUPERVISED MODELING TECHNIQUES"
PUBMED[35,1] <- "AEROBIC FITNESS THRESHOLDS TO DEFINE POOR CARDIOMETABOLIC HEALTH IN CHILDREN AND YOUTH"
PUBMED[55,1] <- "ANALYSIS OF INJURY MECHANISMS IN HEAD INJURIES IN SKIERS AND SNOWBOARDERS"
PUBMED[49,1] <- "APPLYING GRAPHS AND COMPLEX NETWORKS TO FOOTBALL METRIC INTERPRETATION"
PUBMED[70,1] <- "ASSOCIATION BETWEEN SERUM VITAMIN D STATUS AND METABOLIC SYNDROME IN KOREAN YOUNG MEN"
PUBMED[64,1] <- "ASSOCIATIONS BETWEEN SHOWERING BEHAVIOURS FOLLOWING PHYSICAL EDUCATION, PHYSICAL ACTIVITY AND FITNESS IN ENGLISH SCHOOLCHILDREN"
PUBMED[68,1] <- "ASSOCIATIONS BETWEEN YOUNG CHILDREN'S PERCEIVED AND ACTUAL BALL SKILL COMPETENCE AND PHYSICAL ACTIVITY"
PUBMED[52,1] <- "BALANCE, BASIC ANTHROPOMETRICS AND PERFORMANCE IN YOUNG ALPINE SKIERS; LONGITUDINAL ANALYSIS OF THE ASSOCIATIONS DURING TWO COMPETITIVE SEASONS"
PUBMED[85,1] <- "CARDIOVASCULAR RISK FACTOR CLUSTERING AND ITS ASSOCIATION WITH FITNESS IN NINE-YEAR-OLD RURAL NORWEGIAN CHILDREN"
PUBMED[36,1] <- "CLUSTER ANALYSIS USING PHYSICAL PERFORMANCE AND SELF-REPORT MEASURES TO IDENTIFY SHOULDER INJURY IN OVERHEAD FEMALE ATHLETES"
PUBMED[88,1] <- "CLUSTERED DATA IN SPORTS RESEARCH"
PUBMED[81,1] <- "CLUSTERING OF METABOLIC SYNDROME RISK FACTORS ASSOCIATED WITH LIFESTYLE FACTORS AND SERUM LEPTIN IN KOREAN CHILDREN"
PUBMED[25,1] <- "CLUSTERING OF SCREEN TIME BEHAVIOURS IN ADOLESCENTS AND ITS ASSOCIATION WITH WAIST CIRCUMFERENCE AND CARDIORESPIRATORY FITNESS"
PUBMED[43,1] <- "CLUSTERING PERFORMANCES IN THE NBA ACCORDING TO PLAYERS' ANTHROPOMETRIC ATTRIBUTES AND PLAYING EXPERIENCE"
PUBMED[34,1] <- "CLUSTERING TENNIS PLAYERS' ANTHROPOMETRIC AND INDIVIDUAL FEATURES HELPS TO REVEAL PERFORMANCE FINGERPRINTS"
CINAHL[1,1] <- "COMMUNITY SIZE AND SPORT PARTICIPATION ACROSS 22 COUNTRIES"
PUBMED[44,1] <- "COMPLIANCE OF ADOLESCENT GIRLS TO REPEATED DEPLOYMENTS OF WRIST-WORN ACCELEROMETERS"
PUBMED[22,1] <- "COMPLIANCE WITH THE 24-HOUR MOVEMENT GUIDELINES FOR THE EARLY YEARS: CROSS-SECTIONAL AND LONGITUDINAL ASSOCIATIONS WITH EXECUTIVE FUNCTION AND PSYCHOSOCIAL HEALTH IN PRESCHOOL CHILDREN"
CINAHL[2,1] <- "CONSIDERING CLUSTER ANALYSIS IN SPORT MEDICINE AND INJURY PREVENTION RESEARCH"
PUBMED[13,1] <- "CORRELATES OF DUAL TRAJECTORIES OF PHYSICAL ACTIVITY AND SEDENTARY TIME IN YOUTH: THE UP & DOWN LONGITUDINAL STUDY"
PUBMED[87,1] <- "DIFFERENT CENTRE OF PRESSURE PATTERNS WITHIN THE GOLF STROKE I: CLUSTER ANALYSIS"
PUBMED[5,1] <- "DIFFERENTIATING MOVEMENT STYLES IN PROFESSIONAL TENNIS: A MACHINE LEARNING AND HIERARCHICAL CLUSTERING APPROACH"
PUBMED[71,1] <- "EFFECT OF A SCHOOL ENVIRONMENT INTERVENTION ON ADOLESCENT ADIPOSITY AND PHYSICAL FITNESS"
PUBMED[16,1] <- "EFFECT OF AN 8-WEEK EXERCISE TRAINING ON GUT MICROBIOTA IN PHYSICALLY INACTIVE OLDER WOMEN"
PUBMED[72,1] <- "EFFECTS OF A 20-MONTH CLUSTER RANDOMISED CONTROLLED SCHOOL-BASED INTERVENTION TRIAL ON BMI OF SCHOOL-AGED BOYS AND GIRLS: THE HEIA STUDY"
PUBMED[41,1] <- "EVALUATION OF AN INTERVENTION TO REDUCE ADOLESCENT SITTING TIME DURING THE SCHOOL DAY: THE 'STAND UP FOR HEALTH' RANDOMISED CONTROLLED TRIAL"
PUBMED[23,1] <- "EVALUATION OF THE BILATERAL FUNCTION IN PARA-ATHLETES WITH SPASTIC HEMIPLEGIA: A MODEL-BASED CLUSTERING APPROACH"
PUBMED[67,1] <- "EXAMINING RELATIVE AGE EFFECTS IN FUNDAMENTAL SKILL PROFICIENCY IN BRITISH CHILDREN AGED 6-11 YEARS"
PUBMED[9,1] <- "EXPLORING ACTIVITY LEVELS IN PHYSICAL EDUCATION LESSONS IN THE UK: A CROSS-SECTIONAL EXAMINATION OF ACTIVITY TYPES AND FITNESS LEVELS"
PUBMED[26,1] <- "FAMILIARISATION OF NOVICE AND EXPERIENCED TREADMILL USERS DURING A RUNNING SESSION: GROUP SPECIFIC EVIDENCE, TIME AND INDIVIDUAL PATTERNS"
PUBMED[66,1] <- "FUNDAMENTAL MOVEMENT SKILLS IN RELATION TO WEEKDAY AND WEEKEND PHYSICAL ACTIVITY IN PRESCHOOL CHILDREN"
PUBMED[78,1] <- "GRADUAL INCREASE IN THE RISK OF MATCH INJURY IN NORWEGIAN MALE PROFESSIONAL FOOTBALL: A 6-YEAR PROSPECTIVE STUDY"
PUBMED[83,1] <- "HETEROGENEITY OF DIETARY PROFILES IN HIGHLY SEDENTARY YOUNG GUADELOUPEAN WOMEN"
PUBMED[4,1] <- "IDENTIFYING AND ANALYSING GAME STYLES AND FACTORS INFLUENCING A TEAM'S STRATEGY IN FIELD HOCKEY"
PUBMED[32,1] <- "IS SPORT ENOUGH? CONTRIBUTION OF SPORT TO OVERALL MODERATE- TO VIGOROUS-INTENSITY PHYSICAL ACTIVITY AMONG ADOLESCENTS"
PUBMED[6,1] <- "KINEMATIC AND KINETIC PARAMETERS TO IDENTIFY WATER POLO PLAYERS' EGGBEATER KICK TECHNIQUES"
PUBMED[31,1] <- "LONGITUDINAL ASSOCIATIONS BETWEEN PARENTS' MOTIVATIONS TO EXERCISE AND THEIR MODERATE-TO-VIGOROUS PHYSICAL ACTIVITY"
CINAHL[4,1] <- "LONGITUDINAL ASSOCIATIONS OF PHYSICAL ACTIVITY AND MODIFIED ORGANIZED SPORT PARTICIPATION WITH EXECUTIVE FUNCTION AND PSYCHOSOCIAL HEALTH IN PRESCHOOLERS"
PUBMED[29,1] <- "MODERATORS OF EXERCISE EFFECTS ON CANCER-RELATED FATIGUE: A META-ANALYSIS OF INDIVIDUAL PATIENT DATA"
PUBMED[37,1] <- "MUSCLE ACTIVATION DURING POWER-ORIENTED RESISTANCE TRAINING: CONTINUOUS VS. CLUSTER SET CONFIGURATIONS"
PUBMED[12,1] <- "NETWORK ANALYSIS OF SPORT-RELATED CONCUSSION RESEARCH DURING THE PAST DECADE (2010-2019)"
PUBMED[14,1] <- "NON-UNIFORM DISPLACEMENT WITHIN RUPTURED ACHILLES TENDON DURING ISOMETRIC CONTRACTION"
PUBMED[62,1] <- "PACE: A GROUP RANDOMISED CONTROLLED TRIAL TO INCREASE CHILDREN'S BREAK-TIME PLAYGROUND PHYSICAL ACTIVITY"
PUBMED[60,1] <- "POLICY CHANGE ELIMINATING BODY CHECKING IN NON-ELITE ICE HOCKEY LEADS TO A THREEFOLD REDUCTION IN INJURY AND CONCUSSION RISK IN 11- AND 12-YEAR-OLD PLAYERS"
CINAHL[5,1] <- "PREVENTION OF FALL-RELATED INJURIES IN 7-YEAR-OLD TO 12-YEAR-OLD CHILDREN: A CLUSTER RANDOMISED CONTROLLED TRIAL"
PUBMED[48,1] <- "PROFILING MOVEMENT AND GAIT QUALITY CHARACTERISTICS IN PRE-SCHOOL CHILDREN"
PUBMED[56,1] <- "PROFILING MOVEMENT QUALITY AND GAIT CHARACTERISTICS ACCORDING TO BODY-MASS INDEX IN CHILDREN (9-11 Y)"
PUBMED[40,1] <- "PSYCHODIAGNOSTICS: CLASSIFICATION OF THE YIPS PHENOMENON BASED ON MUSICIAN'S DYSTONIA"
PUBMED[63,1] <- "PSYCHOLOGICAL STRATEGIES INCLUDED BY STRENGTH AND CONDITIONING COACHES IN APPLIED STRENGTH AND CONDITIONING"
PUBMED[73,1] <- "QUANTIFICATION OF THE RELATIVE AGE EFFECT IN THREE INDICES OF PHYSICAL PERFORMANCE"
CINAHL[6,1] <- "RATES OF CONCUSSION ARE LOWER IN NATIONAL FOOTBALL LEAGUE GAMES PLAYED AT HIGHER ALTITUDES"
PUBMED[89,1] <- "RECREATIONAL ACTIVITY CLUSTERING AMONG ADOLESCENTS"
PUBMED[79,1] <- "RELATIONSHIP BETWEEN INTERCHANGE USAGE AND RISK OF HAMSTRING INJURIES IN THE AUSTRALIAN FOOTBALL LEAGUE"
PUBMED[27,1] <- "RISK OF ACUTE AND OVERUSE INJURIES IN YOUTH ELITE SOCCER PLAYERS: BODY SIZE AND GROWTH MATTER"
PUBMED[80,1] <- "RISK OF INJURY AND CONCUSSION ASSOCIATED WITH TEAM PERFORMANCE AND PENALTY MINUTES IN COMPETITIVE YOUTH ICE HOCKEY"
PUBMED[53,1] <- "SEDENTARY BEHAVIOR AND ARTERIAL STIFFNESS IN ADULTS WITH AND WITHOUT METABOLIC SYNDROME"
PUBMED[61,1] <- "SELECTED ANTHROPOMETRIC VARIABLES AND AEROBIC FITNESS AS PREDICTORS OF CARDIOVASCULAR DISEASE RISK IN CHILDREN"
PUBMED[77,1] <- "SERUM VITAMIN D, PHYSICAL ACTIVITY, AND METABOLIC RISK FACTORS IN KOREAN CHILDREN"
PUBMED[46,1] <- "SKI AND SNOWBOARD SCHOOL PROGRAMS: INJURY SURVEILLANCE AND RISK FACTORS FOR GRADE-SPECIFIC INJURY"
PUBMED[11,1] <- "SLEEP CHARACTERISTICS OF ELITE YOUTH ATHLETES: A CLUSTERING APPROACH TO OPTIMIZE SLEEP SUPPORT STRATEGIES"
PUBMED[86,1] <- "SOCIODEMOGRAPHIC PREDICTORS OF SPORT INJURY IN ADOLESCENTS"
PUBMED[58,1] <- "SPATIAL CHARACTERISTICS OF PROFESSIONAL TENNIS SERVES WITH IMPLICATIONS FOR SERVING ACES: A MACHINE LEARNING APPROACH"
PUBMED[28,1] <- "STATISTICAL METHODS FOR HANDLING OBSERVATION CLUSTERING IN SPORTS INJURY SURVEILLANCE"
WOS[83,1] <- "THE APPLICATION OF INERTIAL MEASUREMENT UNITS AND FUNCTIONAL PRINCIPAL COMPONENT ANALYSIS TO EVALUATE MOVEMENT IN THE FORWARD 3(1/2) PIKE SOMERSAULT SPRINGBOARD DIVE."
PUBMED[47,1] <- "THE EFFECT OF A PHYSICAL ACTIVITY INTERVENTION ON PRESCHOOLERS' FUNDAMENTAL MOTOR SKILLS - A CLUSTER RCT"
CINAHL[7,1] <- "THE EFFECT OF THE ZERO TOLERANCE FOR HEAD CONTACT RULE CHANGE ON THE RISK OF CONCUSSIONS IN YOUTH ICE HOCKEY PLAYERS"
CINAHL[8,1] <- "THE EFFECTIVENESS OF A NEUROMUSCULAR PREVENTION STRATEGY TO REDUCE INJURIES IN YOUTH SOCCER: A CLUSTER-RANDOMISED CONTROLLED TRIAL"
PUBMED[2,1] <- "THE INFLUENCE OF ANGLE-SPECIFIC TORQUE OF THE KNEE FLEXORS AND EXTENSORS ON THE ANGLE-SPECIFIC DYNAMIC CONTROL RATIO IN PROFESSIONAL FEMALE SOCCER PLAYERS"
PUBMED[7,1] <- "THE ORIGINS OF GOALS IN THE GERMAN BUNDESLIGA"
PUBMED[54,1] <- "THE PERCEIVED PSYCHOLOGICAL RESPONSIBILITIES OF A STRENGTH AND CONDITIONING COACH"
PUBMED[51,1] <- "THE RELATIONSHIP BETWEEN A JUMP-LANDING TASK AND FUNCTIONAL MOVEMENT SCREEN ITEMS : A VALIDATION STUDY"
PUBMED[10,1] <- "THE RELATIONSHIP BETWEEN EDUCATORS' AND CHILDREN'S PHYSICAL ACTIVITY AND SEDENTARY BEHAVIOUR IN EARLY CHILDHOOD EDUCATION AND CARE"
PUBMED[74,1] <- "THE UPPER LIMIT OF THE CARDIORESPIRATORY TRAINING ZONE (40-84%HRR) IS OVERESTIMATED FOR POSTMENOPAUSAL WOMEN"
PUBMED[69,1] <- "TRACKING CAREER PERFORMANCE OF SUCCESSFUL TRIATHLETES"
PUBMED[38,1] <- "TYPOLOGIES OF ADOLESCENT ACTIVITY RELATED HEALTH BEHAVIOURS"
PUBMED[19,1] <- "LONGITUDINAL ASSOCIATIONS OF PHYSICAL ACTIVITY AND MODIFIED ORGANIZED SPORT PARTICIPATION WITH EXECUTIVE FUNCTION AND PSYCHOSOCIAL HEALTH IN PRESCHOOLERS"
PUBMED[17,1] <- "NETWORK ANALYSIS OF SPORT-RELATED CONCUSSION RESEARCH DURING THE PAST DECADE (2010-2019)"
PUBMED[75,1] <- "PREVENTION OF FALL-RELATED INJURIES IN 7-YEAR-OLD TO 12-YEAR-OLD CHILDREN: A CLUSTER RANDOMISED CONTROLLED TRIAL"
PUBMED[60,1] <- "POLICY CHANGE ELIMINATING BODY CHECKING IN NON-ELITE ICE HOCKEY LEADS TO A THREEFOLD REDUCTION IN INJURY AND CONCUSSION RISK IN 11-AND 12-YEAR-OLD PLAYERS"
PUBMED[84,1] <- "THE EFFECTIVENESS OF A NEUROMUSCULAR PREVENTION STRATEGY TO REDUCE INJURIES IN YOUTH SOCCER: A CLUSTER-RANDOMISED CONTROLLED TRIAL"
SD[11,1] <- "CLUSTERING TENNIS PLAYERS' ANTHROPOMETRIC AND INDIVIDUAL FEATURES HELPS TO REVEAL PERFORMANCE FINGERPRINTS"
PUBMED[65,1] <- "COMMUNITY SIZE AND SPORT PARTICIPATION ACROSS 22 COUNTRIES"
WOS[24,1] <- "The 'SHRed Injuries Basketball' Neuromuscular Training Warm-up Program Reduces Ankle and Knee Injury Rates by 36% in Youth Basketball"

CINAHL$Title <- toupper(c(CINAHL$Title)) 
SD$Title <- toupper(c(SD$Title)) 
WOS$Title <- toupper(c(WOS$Title)) 
PUBMED$Title <- toupper(c(PUBMED$Title)) 

CINAHL$web <- "Cinahl" 
SD$web <- "SportDiscus" 
WOS$web <- "WoS" 
PUBMED$web <- "PubMed" 

articles <- full_join(CINAHL, SD)
articles <- full_join(articles, WOS)
articles <- full_join(articles, PUBMED)

articles <- as.data.frame(articles)
ARTICLES <- distinct(articles,Title, .keep_all = TRUE)

ARTICLES$id <- c(1:nrow(ARTICLES))
ARTICLES[10,14] <- "Cinahl"
ARTICLES[12,14] <- "Cinahl"
ARTICLES[13,14] <- "Cinahl"

no <- c(1:11,14,16,18:23,25:30,32,34,36:38,41,42,43,45,47,51:53,55,57:59,61,62,64,65,67,69:76,78,81,82,86:89,91,92,94:97,99,101,104,107,109:116,
        118:123,125,126,133:136,138:140,143:146,148,150:172,175:177,179:187,189:194,196:200)
ARTICLES <- ARTICLES[-no,]
rownames(ARTICLES) <- c(1:nrow(ARTICLES))

### RESULTATS

carac <- read_excel("F:/ESTADÍSTICA/5è CURS/2n QUATRI/TFG/BBDD/carac.xlsx")
# View(carac)
str(carac)
carac <- as.data.frame(carac)

## PLOTS

library(grDevices)
library(ggplot2)

# Type of clustering method

carac[carac$`Type of clustering method` == "Trajectory-based Longitudinal Clustering","Type of clustering method"] <- "Others"
carac[carac$`Type of clustering method` == "Two-step Clustering","Type of clustering method"] <- "Others"
carac[carac$`Type of clustering method` == "Hierarchical Clustering | Partitional Clustering","Type of clustering method"] <- "More than one type"
carac[carac$`Type of clustering method` == "Partitional Clustering | Model-based Clustering","Type of clustering method"] <- "More than one type"

(tipus <- table(carac$`Type of clustering method`))
(ptipus <- prop.table(tipus))

windows()
ggplot(carac, aes(x = reorder(`Type of clustering method`, `Type of clustering method`, function(x)-length(x)))) +
  geom_bar(fill = "lightblue") + labs(x='Type of clustering method', y='n') + ylim(0,20) + theme_bw() + theme(axis.text = element_text(size=8))

# Type of clustering method x Name of the clustering method used

tipusxmet <- table(carac$`Type of clustering method`,carac$`Name of the clustering method used`)
(tipusxmet <- prop.table(tipusxmet))

windows()
ggplot(carac, aes(x = reorder(`Type of clustering method`, `Type of clustering method`, function(x)-length(x)), fill = `Name of the clustering method used`)) +
  geom_bar() + labs(x='Type of clustering method',y='n',fill='Name of the clustering method used') + theme_bw() + theme(axis.text = element_text(size=7))

# Publication Year

(any <- table(carac$`Publication Year`))
(pany <- prop.table(any))

levels(carac$`Publication Year`)
freq <- c(nrow(carac[carac$`Publication Year` == 1981,]),nrow(carac[carac$`Publication Year` == 1982,]),
          nrow(carac[carac$`Publication Year` == 1983,]),nrow(carac[carac$`Publication Year` == 1984,]),
          nrow(carac[carac$`Publication Year` == 1985,]),nrow(carac[carac$`Publication Year` == 1986,]),
          nrow(carac[carac$`Publication Year` == 1987,]),nrow(carac[carac$`Publication Year` == 1988,]),
          nrow(carac[carac$`Publication Year` == 1989,]),nrow(carac[carac$`Publication Year` == 1990,]),
          nrow(carac[carac$`Publication Year` == 1991,]),nrow(carac[carac$`Publication Year` == 1992,]),
          nrow(carac[carac$`Publication Year` == 1993,]),nrow(carac[carac$`Publication Year` == 1994,]),
          nrow(carac[carac$`Publication Year` == 1995,]),nrow(carac[carac$`Publication Year` == 1996,]),
          nrow(carac[carac$`Publication Year` == 1997,]),nrow(carac[carac$`Publication Year` == 1998,]),
          nrow(carac[carac$`Publication Year` == 1999,]),nrow(carac[carac$`Publication Year` == 2000,]),
          nrow(carac[carac$`Publication Year` == 2001,]),nrow(carac[carac$`Publication Year` == 2002,]),
          nrow(carac[carac$`Publication Year` == 2003,]),nrow(carac[carac$`Publication Year` == 2004,]),
          nrow(carac[carac$`Publication Year` == 2005,]),nrow(carac[carac$`Publication Year` == 2006,]),
          nrow(carac[carac$`Publication Year` == 2007,]),nrow(carac[carac$`Publication Year` == 2008,]),
          nrow(carac[carac$`Publication Year` == 2009,]),nrow(carac[carac$`Publication Year` == 2010,]),
          nrow(carac[carac$`Publication Year` == 2011,]),nrow(carac[carac$`Publication Year` == 2012,]), 
          nrow(carac[carac$`Publication Year` == 2013,]),nrow(carac[carac$`Publication Year` == 2014,]), 
          nrow(carac[carac$`Publication Year` == 2015,]),nrow(carac[carac$`Publication Year` == 2016,]), 
          nrow(carac[carac$`Publication Year` == 2017,]),nrow(carac[carac$`Publication Year` == 2018,]), 
          nrow(carac[carac$`Publication Year` == 2019,]),nrow(carac[carac$`Publication Year` == 2020,]), 
          nrow(carac[carac$`Publication Year` == 2021,]),nrow(carac[carac$`Publication Year` == 2022,]))

data <- data.frame(year = 1981:2022,
                   n=freq)
windows()
ggplot(data, aes(x=year, y=n)) +
  geom_line(stat="identity") + ylim(0,12.5) + theme (plot.title = element_text(vjust=2, face="bold", 
  color="black", lineheight=1.5))+geom_point()+geom_smooth(se = F) + theme_bw() + theme(axis.text = element_text(size=12))

# Sport

(esport <- table(carac$Sport))

carac[carac$Sport == "Australian Rules Football","Sport"] <- "Others"
carac[carac$Sport == "Cross-country sit-ski","Sport"] <- "Others"
carac[carac$Sport == "Swimming","Sport"] <- "Others"
carac[carac$Sport == "Waterpolo","Sport"] <- "Others"
carac[carac$Sport == "Field Hockey","Sport"] <- "Others"
carac[carac$Sport == "Rugby","Sport"] <- "Others"
carac[carac$Sport == "Netball","Sport"] <- "Others"
carac[carac$Sport == "Cricket","Sport"] <- "Others"
carac[carac$Sport == "Running","Sport"] <- "Others"
carac[carac$Sport == "Volleyball","Sport"] <- "Others"
carac[carac$Sport == "Wheelchair Basketball","Sport"] <- "Basketball"
carac[carac$Sport == "Synchronized swimming | Handball","Sport"] <- "Multidisciplinar"
carac[carac$Sport == "Basketball | Football | Volleyball","Sport"] <- "Multidisciplinar"
carac[carac$Sport == "Gaelic Football | Hurling","Sport"] <- "Multidisciplinar"
carac[carac$Sport == "Ski | Snowboard","Sport"] <- "Multidisciplinar"

(esport <- table(carac$`Sport`))
(pesport <- prop.table(esport))

windows()
ggplot(carac, aes(x = reorder(Sport, Sport, function(x)-length(x)))) +
  geom_bar(fill = "coral") + labs(x='Sport', y='n') + ylim(0,15) + theme_bw() + theme(axis.text = element_text(size=12))


## Altres resultats

carac[carac$Country == "Australia | France | England | USA","Country"] <- "International"
carac[carac$Country == "England","Country"] <- "UK"
carac[carac$Country == "Wales","Country"] <- "UK"
carac[carac$Country == "EUA","Country"] <- "USA"

(country <- table(carac$Country))
(pcountry <- prop.table(country))

(long <- table(carac$`Longitudinal study`))
(plong <- prop.table(long))

(sexe <- table(carac$Gender))
(psexe <- prop.table(sexe))

(cpar <- table(carac$`Category participants`))
(pcpar <- prop.table(cpar))

(cclas <- table(carac$`Category classification`))
(pcclas <- prop.table(cclas))

carac[carac$`Name of the clustering method used` == "Gaussian mixture models","Name of the clustering method used"] <- "others"
carac[carac$`Name of the clustering method used` == "Johnson's hierarchical procedure","Name of the clustering method used"] <- "others"
carac[carac$`Name of the clustering method used` == "k-means | Multinomial Mixture Model","Name of the clustering method used"] <- "others"
carac[carac$`Name of the clustering method used` == "Latent class analysis","Name of the clustering method used"] <- "others"
carac[carac$`Name of the clustering method used` == "Mixed Data Method","Name of the clustering method used"] <- "others"
carac[carac$`Name of the clustering method used` == "hclust","Name of the clustering method used"] <- "others"
carac[carac$`Name of the clustering method used` == "kmlShape","Name of the clustering method used"] <- "others"
carac[carac$`Name of the clustering method used` == "Louvain method","Name of the clustering method used"] <- "others"
(name <- table(carac$`Name of the clustering method used`))
(pname <- prop.table(name))

carac[carac$`Method to decide the number of clusters` == "AIC | BIC | sBIC","Method to decide the number of clusters"] <- "others"
carac[carac$`Method to decide the number of clusters` == "BIC | Silhouette","Method to decide the number of clusters"] <- "others"
carac[carac$`Method to decide the number of clusters` == "Calinski-Harabasz index | elbow method | partition coefficient","Method to decide the number of clusters"] <- "others"
carac[carac$`Method to decide the number of clusters` == "CH Index","Method to decide the number of clusters"] <- "others"
carac[carac$`Method to decide the number of clusters` == "Clustergram | dendrogram","Method to decide the number of clusters"] <- "others"
carac[carac$`Method to decide the number of clusters` == "Elbow | BIC","Method to decide the number of clusters"] <- "others"
carac[carac$`Method to decide the number of clusters` == "Elbow | Silhouette","Method to decide the number of clusters"] <- "others"
carac[carac$`Method to decide the number of clusters` == "Fréchet mean","Method to decide the number of clusters"] <- "others"
carac[carac$`Method to decide the number of clusters` == "Johnson's dendrogram","Method to decide the number of clusters"] <- "dendrogram"
carac[carac$`Method to decide the number of clusters` == "Point biserial correlation | C-Index","Method to decide the number of clusters"] <- "others"
carac[carac$`Method to decide the number of clusters` == "Silhouette | Calinski-Harabasz","Method to decide the number of clusters"] <- "others"
(mopt <- table(carac$`Method to decide the number of clusters`))
(pmopt <- prop.table(mopt))

carac[carac$`Software used` == "NCSS","Software used"] <- "others"
carac[carac$`Software used` == "R | Excel | MATLAB | GraphPad Prism","Software used"] <- "others"
carac[carac$`Software used` == "SPSS | MATLAB","Software used"] <- "others"
carac[carac$`Software used` == "Statica | Modalisa","Software used"] <- "others"
carac[carac$`Software used` == "STATSports | Python | SPSS","Software used"] <- "others"
carac[carac$`Software used` == "MATLAB | SPSS","Software used"] <- "others"
carac[carac$`Software used` == "SAS | SPSS | MATLAB","Software used"] <- "others"
carac[carac$`Software used` == "SPSS | Excel","Software used"] <- "others"
carac[carac$`Software used` == "SPSS | R","Software used"] <- "others"
carac[carac$`Software used` == "Statica 11 | R","Software used"] <- "others"
(soft <- table(carac$`Software used`))
(psoft <- prop.table(soft))


(pack <- table(carac$`Package used`))
(npack <- c("no reported"))
(tpack <- c(sum(carac$`Package used`=="no reported")))
(ppack <- prop.table(pack))

(dat <- table(carac$`Data shared`))
(cod <- table(carac$`Code shared`))
(datcod <- table(carac$`Repository of Data or Code shared`))

## ESTUDI DE CAS

wnba <- read.csv("wnba-player-stats.csv",sep=",",header=T)
wnba <- data.frame(wnba)
nomin <- c(13:25)
wnba[,nomin] <- wnba[,nomin]/wnba$MP

wnba_kmeans <- as.data.table(wnba)
wnba_kmeans <- wnba_kmeans[,-c(3,5,6,7,9,10,11,26,27,28)]
wnba_kmeans[is.na(wnba_kmeans)] <- 0

wnba_kmeans <- data.frame(ddply(wnba_kmeans, c("Player","player_ID","Pos"), colwise(median)))
esca <- c(5:18)
wnba_kmeans[,esca] <- data.frame(scale(wnba_kmeans[,esca]))

no <- c(1:4)
nwnba_km <- wnba_kmeans[,-no]
nwnba_km <- data.frame(nwnba_km)

# k-means

set.seed(123)

# Check clusterability with Hopkins index
#A value for H higher than 0.75 indicates a clustering tendency at the 90% confidence level

H <- get_clust_tendency(nwnba_km, n = nrow(nwnba_km)-1,graph = FALSE)

H$hopkins_stat

# R package and function _NBclust_

library(NbClust)
nc <- NbClust(nwnba_km, min.nc=2, max.nc=10, method="kmeans") # k = 3,4

set.seed(123)

k2 <- kmeans(nwnba_km, centers = 2, nstart = 25)
k3 <- kmeans(nwnba_km, centers = 3, nstart = 25)
k2$size
k3$size

windows()
fviz_cluster(k2,  data = nwnba_km) + ggtitle("k = 2")
windows()
fviz_cluster(k3,  data = nwnba_km) + ggtitle("k = 3")

# S'elimina l'observació 220 i es torna a repetir tot

wnba_kmeans <- wnba_kmeans[-220,]
nwnba_km <- nwnba_km[-220,]

set.seed(123)

# Check clusterability with Hopkins index
#A value for H higher than 0.75 indicates a clustering tendency at the 90% confidence level

H <- get_clust_tendency(nwnba_km, n = nrow(nwnba_km)-1,graph = FALSE)

H$hopkins_stat

# R package and function _NBclust_

library(NbClust)
nc <- NbClust(nwnba_km, min.nc=2, max.nc=10, method="kmeans") # k = 4

# Method

set.seed(123)

k2 <- kmeans(nwnba_km, centers = 2, nstart = 25)
k2$size

wnba_final<- cbind(wnba_kmeans, k2$cluster) 
names(wnba_final)[19] <- "cluster"
wnba_final$cluster <- as.factor(wnba_final$cluster) 

windows()
ggplot(wnba_final, aes(x = cluster,fill = cluster)) +
  geom_bar() + labs(x='cluster', y='n') + theme_bw() + theme(axis.text = element_text(size=12))

#PC

windows()
fviz_cluster(k2,  data = nwnba_km) + ggtitle("k = 2")

pc1 <- prcomp(nwnba_km, scale=F) # ja estan estandaritzades
Psi <- pc1$x[,1:2]
(Phi <- cor(nwnba_km,Psi))

# Post-Method

wnba <- read.csv("wnba-player-stats.csv",sep=",",header=T)
wnba <- data.frame(wnba)
nomin <- c(13:25)
wnba[,nomin] <- wnba[,nomin]/wnba$MP

wnba_kmeans <- as.data.table(wnba)
wnba_kmeans <- wnba_kmeans[,-c(3,5,6,7,9,10,11,26,27,28)]
wnba_kmeans[is.na(wnba_kmeans)] <- 0

wnba_kmeans <- data.frame(ddply(wnba_kmeans, c("Player","player_ID","Pos"), colwise(median)))
wnba_kmeans <- wnba_kmeans[-220,]

wnba_final<- cbind(wnba_kmeans, k2$cluster) 

names(wnba_final)[19] <- "cluster"

wnba_final$cluster <- as.factor(wnba_final$cluster)  

# Diferencies entre clusters
shapiro.test(wnba_final$PER)
shapiro.test(wnba_final$TS_pct)
shapiro.test(wnba_final$ThrPAr)
shapiro.test(wnba_final$FTr)
shapiro.test(wnba_final$ORB_pct)
shapiro.test(wnba_final$TRB_pct)
shapiro.test(wnba_final$AST_pct)
shapiro.test(wnba_final$STL_pct)
shapiro.test(wnba_final$BLK_pct)
shapiro.test(wnba_final$TOV_pct)
shapiro.test(wnba_final$USG_pct)
shapiro.test(wnba_final$OWS)
shapiro.test(wnba_final$DWS)
shapiro.test(wnba_final$WS)

wilcox.test(wnba_final$PER~wnba_final$cluster)
wilcox.test(wnba_final$TS_pct~wnba_final$cluster)
wilcox.test(wnba_final$ThrPAr~wnba_final$cluster)
wilcox.test(wnba_final$FTr~wnba_final$cluster)
wilcox.test(wnba_final$ORB_pct~wnba_final$cluster)
wilcox.test(wnba_final$TRB_pct~wnba_final$cluster)
wilcox.test(wnba_final$AST_pct~wnba_final$cluster)
wilcox.test(wnba_final$STL_pct~wnba_final$cluster)
wilcox.test(wnba_final$BLK_pct~wnba_final$cluster)
wilcox.test(wnba_final$TOV_pct~wnba_final$cluster)
wilcox.test(wnba_final$USG_pct~wnba_final$cluster)
wilcox.test(wnba_final$OWS~wnba_final$cluster)
wilcox.test(wnba_final$DWS~wnba_final$cluster)
wilcox.test(wnba_final$WS~wnba_final$cluster)

chisq.test(wnba_final$Player,wnba_final$cluster)
chisq.test(wnba_final$Pos,wnba_final$cluster)
chisq.test(wnba_final$Age,wnba_final$cluster)

# PER

boxplot(k2$centers[,1], main = "PER")
(PER_min2 <- ggplot(wnba_final, aes(x = cluster, y = PER, fill = cluster)) + 
    geom_boxplot())

# TS_pct per minut

boxplot(k2$centers[,2], main = "Percentatge tir real per minut")
(TS_min2 <- ggplot(wnba_final, aes(x = cluster, y = TS_pct, fill = cluster)) + 
    geom_boxplot())

# ThrPAr per minut

boxplot(k2$centers[,3], main = "Taxa d'intents de 3p per minut")
(ThrPAr_min2 <- ggplot(wnba_final, aes(x = cluster, y = ThrPAr, fill = cluster)) + 
    geom_boxplot())

# FTr per minut

boxplot(k2$centers[,4], main = "Taxa de tirs lliures per minut")
(FTr_min2 <- ggplot(wnba_final, aes(x = cluster, y = FTr, fill = cluster)) + 
    geom_boxplot())

# ORB_pct 

boxplot(k2$centers[,5], main = "% rebots ofensius")
(ORB_min2 <- ggplot(wnba_final, aes(x = cluster, y = ORB_pct, fill = cluster)) + 
    geom_boxplot())

# TRB_pct 

boxplot(k2$centers[,6], main = "% total rebots")
(TRB_min2 <- ggplot(wnba_final, aes(x = cluster, y = TRB_pct, fill = cluster)) + 
    geom_boxplot())

# AST_pct

boxplot(k2$centers[,7], main = "% assistències")
(AST_min2 <- ggplot(wnba_final, aes(x = cluster, y = AST_pct, fill = cluster)) + 
    geom_boxplot())

# STL_pct

boxplot(k2$centers[,8], main = "% robos")
(STL_min2 <- ggplot(wnba_final, aes(x = cluster, y = STL_pct, fill = cluster)) + 
    geom_boxplot())

# BLK_pct

boxplot(k2$centers[,9], main = "% taps")
(BLK_min2 <- ggplot(wnba_final, aes(x = cluster, y = BLK_pct, fill = cluster)) + 
    geom_boxplot())

# TOV_pct per minut

boxplot(k2$centers[,10], main = "% canvis de possessió per minut")
(TOV_min2 <- ggplot(wnba_final, aes(x = cluster, y = TOV_pct, fill = cluster)) + 
    geom_boxplot())

# USG_pct

boxplot(k2$centers[,11], main = "% ús")
(USG_min2 <- ggplot(wnba_final, aes(x = cluster, y = USG_pct, fill = cluster)) + 
    geom_boxplot())

# OWS 

boxplot(k2$centers[,12], main = "% victòries ofensives")
(OWS_min2 <- ggplot(wnba_final, aes(x = cluster, y = OWS, fill = cluster)) + 
    geom_boxplot())

# DWS 

boxplot(k2$centers[,13], main = "% victòries defensives")
(DWS_min2 <- ggplot(wnba_final, aes(x = cluster, y = DWS, fill = cluster)) + 
    geom_boxplot())

# WS 

boxplot(k2$centers[,14], main = "% victòries")
(WS_min2 <- ggplot(wnba_final, aes(x = cluster, y = WS, fill = cluster)) + 
    geom_boxplot())

windows()
ggarrange(PER_min2, TS_min2, ThrPAr_min2,FTr_min2, ncol = 2, nrow = 2)
windows()
ggarrange(ORB_min2, TRB_min2,  ncol = 2, nrow = 2)
windows()
ggarrange(AST_min2,STL_min2,BLK_min2, TOV_min2, ncol = 2, nrow = 2)
windows()
ggarrange(USG_min2,OWS_min2,DWS_min2, WS_min2, ncol = 2, nrow = 2)


# RDATA

save(jcr, file = "jcr.RData")
save(cinahl, file = "cinahl.RData")
save(sd1, file = "sd1.RData")
save(sd2, file = "sd2.RData")
save(wos, file = "wos.RData")
save(pubmed, file = "pubmed.RData")
save(carac, file = "carac.RData")
save(wnba, file = "wnba.RData")

