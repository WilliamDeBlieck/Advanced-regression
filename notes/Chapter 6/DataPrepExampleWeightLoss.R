# Example of data prep 

setwd("D:/My Drive/Cornell College/Cornell Classes/STA 355/Materials Used in Class/data")


risk2009.data = foreign::read.spss("yrbs09.sav", to.data.frame=TRUE)
names(risk2009.data)

set.seed(33)
risk2009.samp <- risk2009.data %>%
  sample_n(500)
rm(risk2009.data)

# Data Prep
# Extract sex from df
sex = with(risk2009.samp,Q2)
# Recode sex as a binary 0,1 varaible
sex=with(risk2009.samp,ifelse(sex=="Female",0,1))

# Now change sex into an R factor with labels Female and Male
sex.l=with(risk2009.samp,factor(sex, labels = c("Female", "Male")))
# Count up
table(sex.l)

# Extract out Q66 results
lose.wt4 = with(risk2009.samp,Q66)
table(lose.wt4)

# Redefine Q66 from 4 choices to two
lose.wt2= with(risk2009.samp,ifelse(Q66=="Lose weight",1,0))
table(lose.wt2)

# Change lose.wt2 into an R factor with nice lables
lose.wt.l=with(risk2009.samp,factor(lose.wt2, labels = c("No weight loss", "Lose weight")))
table(lose.wt.l)

# Extract out Q84
sport4 = with(risk2009.samp,Q84)
table(sport4)

# Recode Q84 with 0,1 for if they played sports or not
sport =  with(risk2009.samp,ifelse(Q84=="0 teams",0,1))

# Change to an R factor and give the binary values nice labels
sport.l=with(risk2009.samp,factor(sport, labels = c("No sports", "Sports")))
table(sport.l)

# Extract out Q81
media=with(risk2009.samp,as.numeric(Q81)) # hours of TV per day
table(media)
media=media-2
table(media)
media=ifelse(media==0,0.5,media)
media=ifelse(media==-1,0,media)
table(media)

risk2009 <- risk2009.samp %>%
  mutate(sex = sex.l, lose.wt = lose.wt.l, sport = sport.l, 
         media = media, lose.wt.01 = lose.wt2) %>%
  mutate(sport_fac = sport4,
         sport_num = parse_number(as.character(sport4))) %>%
  dplyr::select(lose.wt, lose.wt.01, sex, sport, sport_fac,
                sport_num, media, bmipct) %>%
  filter(complete.cases(.))
rm(risk2009.samp)

write_csv(risk2009, "risk2009.csv")
