# df_raw <- read.xlsx("./data/Tango and Your Body (Responses).xlsx", sheetIndex=1, header=TRUE)
# country_raw <- read.xlsx("./util/Countries.xlsx",sheetIndex=1, header=TRUE)

# if (!file.exists("data")) {
#  dir.create("data")
#}
#install.packages(file.choose(), repos=NULL)
#library(xlsx)

# functions
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
count.responses <- function (x) {
  a <- rle(sort(as.character(x)))
  b <- data.frame(Value=a$values, Nbr=a$lengths)
  b[order(b$Nbr, na.last=T, decreasing=T),]
}

# data treatment starts here
df <- df_raw

# find unique rows when time rounded off to hours, 
# but go back to original time stamps
df <- cbind(Time.by.hour=round(df[,1], units="hours"), df)
df <- df[!duplicated(df[,-2]),]
df <- subset(df, select = -Time.by.hour)

# remove first entry, which was only for testing the form
df <- df[2:nrow(df),]

# remove records where answers indicate non-sincere responses
df <- df[df$Your.age.in.years.!=200 | is.na(df$Your.age.in.years.),] # also had country=suburbia

# remove email addresses and store them separately
colnames(df)[colnames(df) == "In.case.you.want.us.to.share.the.results.of.the.survey.with.you..please.enter.your.e.mail.address..will.be.used.for.this.specific.purpose.only.."] <- "Email"
emails <- df$Email[!is.na(df$Email)]
df <- subset(df, select = -Email)

# remove empty rows
df_notime <- df[,!names(df) %in% "Timestamp"]
df <- df[rowSums(is.na(df_notime)) != ncol(df_notime),]
rm(df_notime)

# Leader/follower
colnames(df)[colnames(df) == "Begin.by.telling.us.which.role.you.are.dancing.more.often."] <- "Dancing.role"
Is.a.leader <- df$Dancing.role == "Leader" | df$Dancing.role == "Leader and follower in equal amounts"
Is.a.follower <- df$Dancing.role == "Follower" | df$Dancing.role == "Leader and follower in equal amounts"
df <- cbind(df, Is.a.leader, Is.a.follower)

# Dancing frequency
colnames(df)[colnames(df) == "How.often.do.you.dance.tango..including.milongas..classes..practice..etc..."] <- "Dancing.frequency"
freq <-  data.frame(rbind(
  c("Less than once a month", 1),
  c("1-3 times a month", 2),
  c("Once a week", 3),
  c("2-3 times a week", 4),
  c("4 times a week or more", 5)
))
colnames(freq) <- c("Lookup.value", "Dancing.frequency.sort")
df = cbind(df, Dancing.frequency.sort = freq$Dancing.frequency.sort[match(df$Dancing.frequency,freq$Lookup.value)])

# Years in tango
colnames(df)[colnames(df) == "For.how.long.have.you.been.dancing.tango."] <- "Time.in.tango"
df <- cbind(df, Years.in.tango=substr(df$Time.in.tango,1,regexpr(' y', df$Time.in.tango)-1))
df <- cbind(df, Years.in.tango.num=df$Years.in.tango)
index <- df$Years.in.tango.num == "Less than 1" & !is.na(df$Years.in.tango.num)
df$Years.in.tango.num <- as.numeric(df$Years.in.tango.num)
df$Years.in.tango.num[index] <- 0
index <- df$Years.in.tango == 20 & !is.na(df$Years.in.tango)
df$Years.in.tango <- as.character(df$Years.in.tango)
df$Years.in.tango[index] <- "20 or more"
df$Years.in.tango <- as.factor(df$Years.in.tango)

# High heels
colnames(df)[colnames(df) == "When.you.dance.tango..how.often.are.you.wearing.high.heels..5.cm.or.more.."] <- "Heels.frequency"

# Pain in body parts
pain <- "Pain."
left <- "Pain in left side of the body"
right <- "Pain in right side of the body"
both <- "Pain in both sides"
nopain <- "No pain / Don't know"
body.parts.input = c("toe", "heel", "ball.of.foot", "foot..other.parts", "ankle", "knee", 
               "leg..other.parts", "hip", "lower.back", "upper.back", "abdomen", 
               "chest.or.ribs", "shoulder", "elbow", "wrist", "arm..other.parts", 
               "neck", "forhead", "head..other.parts")
body.parts.group1 = c("foot", "foot", "foot", "foot", "foot", "hip.leg.or.knee",
                     "hip.leg.or.knee", "hip.leg.or.knee", "back", "back", "front.of.torso",
                     "front.of.torso", "neck.or.shoulder", "arm.or.hand", "arm.or.hand", "arm.or.hand", 
                     "neck.or.shoulder", "head", "head")
body.parts.group2 = c("foot.or.leg", "foot.or.leg", "foot.or.leg", "foot.or.leg", "foot.or.leg", "foot.or.leg",
                      "foot.or.leg", "hip.back.or.torso", "hip.back.or.torso", "hip.back.or.torso", 
                      "hip.back.or.torso", "hip.back.or.torso", "shoulder.arm.neck.or.head", 
                      "shoulder.arm.neck.or.head", "shoulder.arm.neck.or.head", "shoulder.arm.neck.or.head", 
                      "shoulder.arm.neck.or.head", "shoulder.arm.neck.or.head", "shoulder.arm.neck.or.head")
grouping <- data.frame(part.raw=gsub("\\.\\.", "\\.",body.parts.input), 
  group1=body.parts.group1, group2=body.parts.group2)
body.part.group1 <- unique(grouping$group1)
body.part.group2 <- unique(grouping$group2)
for (body.part1 in body.part.group1) {
  df[paste(pain, body.part1, sep="")] <- F
}
for (body.part2 in body.part.group2) {
  df[paste(pain, body.part2, sep="")] <- F
}
for (body.part.input in body.parts.input) {
  body.part = tolower(gsub("\\.\\.", "\\.",body.part.input))
  body.part.raw = paste(body.part, ".raw", sep="")
  col.raw = paste(pain, body.part.raw, sep="")
  colnames(df)[tolower(colnames(df)) == paste("have.you.experienced.pain.after.tango.dancing.in.any.of.the",
    ".following.joints.or.body.parts...", tolower(body.part.input), ".", sep="")] <- col.raw
  df[is.na(df[[col.raw]]), col.raw] <- nopain # here we assume that no answer equals "No pain / don't know"
  df[paste(pain, body.part, ".left.side.only", sep="")] <- df[[col.raw]] == left
  df[paste(pain, body.part, ".right.side.only", sep="")] <- df[[col.raw]] == right
  df[paste(pain, body.part, ".both.sides", sep="")] <- df[[col.raw]] == both
  df[paste(pain, body.part, ".right.side", sep="")] <- df[[col.raw]] == right | df[[col.raw]] == both
  df[paste(pain, body.part, ".left.side", sep="")] <- df[[col.raw]] == left | df[[col.raw]] == both  
  df[paste(pain, body.part, sep="")] <- df[[col.raw]] == left | df[[col.raw]] == right | df[[col.raw]] == both
  part1 <- grouping$group1[grouping$part.raw == body.part]
  part2 <- grouping$group2[grouping$part.raw == body.part]
  df[paste(pain, part1, sep="")] <- df[paste(pain, part1, sep="")] | df[paste(pain, body.part, sep="")]
  df[paste(pain, part2, sep="")] <- df[paste(pain, part1, sep="")] | df[paste(pain, body.part, sep="")]
}
rm(list=ls(pattern="body.part"))
rm(list=c("part1","part2","pain","nopain","right","left","both"))

# Tension
dontknow <- "Don't know"
yes <- "Yes"
no <- "No"
colnames(df)[colnames(df) == "Do.you.ever.feel.tense.in.your.body.during..or.right.after..tango.dancing."] <- "Feeling.tense.raw"



# Age
# (rather than discarding unprecise answers, we make reasonable guesses)
colnames(df)[colnames(df) == "Your.age.in.years."] <- "Age.raw"
ages <- trim(df$Age.raw)
ages[ages == "54 today!"] <- 54
ages[ages == "56years"] <- 56
ages[ages == "fourtyfour"] <- 44
ages[ages == "59,5"] <- 59
ages[ages == "1949Malta"] <- 67
ages[ages == "almost 70"] <- 69
ages[ages == "40-50"] <- 45
ages[ages == "40+"] <- 43
ages[ages == "60+"] <- 63
ages[ages == "70+"] <- 73
df <- cbind(df, Age.in.years = ages)
rm(ages)
nbr.per.age <- count.responses(df$Age.in.years)

# clean up country
country.data <- cbind(toupper(country_raw$Short.name), country_raw)
colnames(country.data)[c(1,2)] <- c("Lookup.value", "Country.name")
country <- subset(country.data, T, c("Lookup.value", "Country.name"))
country <- rbind(country, setNames(country.data[,c("Alpha.2.code", "Country.name")], names(country)))
country <- rbind(country, setNames(country.data[,c("Alpha.3.code", "Country.name")], names(country)))
rm(country.data)
# df$Country.raw[is.na(df$Country.of.residence)]
mult <- "*More than one country specified"
unintelligible <- "*Not specified"
country.corr <- data.frame(rbind(
  c("BELGIE", as.character(country$Country.name[country$Lookup.value == "BEL"])),
  c("BELGUM", as.character(country$Country.name[country$Lookup.value == "BEL"])),
  c("CANANDA", as.character(country$Country.name[country$Lookup.value == "CAN"])),
  c("SWISS", as.character(country$Country.name[country$Lookup.value == "CHE"])),
  c("CZECH REPUBLIC", as.character(country$Country.name[country$Lookup.value == "CZE"])),
  c("CZECH", as.character(country$Country.name[country$Lookup.value == "CZE"])),
  c("DANISH", as.character(country$Country.name[country$Lookup.value == "DNK"])),
  c("GER", as.character(country$Country.name[country$Lookup.value == "DEU"])),
  c("BERLIN", as.character(country$Country.name[country$Lookup.value == "DEU"])),
  c("HAMBURG", as.character(country$Country.name[country$Lookup.value == "DEU"])),
  c("GMERMANY", as.character(country$Country.name[country$Lookup.value == "DEU"])),
  c("GERMAN", as.character(country$Country.name[country$Lookup.value == "DEU"])),
  c("GERMAY", as.character(country$Country.name[country$Lookup.value == "DEU"])),
  c(toupper("España"), as.character(country$Country.name[country$Lookup.value == "ESP"])),
  c("UK", as.character(country$Country.name[country$Lookup.value == "GBR"])),
  c("UKHE", as.character(country$Country.name[country$Lookup.value == "GBR"])), # questionable guess
  c("SCOTLAND", as.character(country$Country.name[country$Lookup.value == "GBR"])),
  c("ENGLAND", as.character(country$Country.name[country$Lookup.value == "GBR"])),
  c("UNITED KINGDOM", as.character(country$Country.name[country$Lookup.value == "GBR"])),
  c("U.K.", as.character(country$Country.name[country$Lookup.value == "GBR"])),
  c("LONDON", as.character(country$Country.name[country$Lookup.value == "GBR"])),
  c("CRO", as.character(country$Country.name[country$Lookup.value == "HRV"])),
  c("PALERMO ITALY", as.character(country$Country.name[country$Lookup.value == "ITA"])),
  c("ITALIA", as.character(country$Country.name[country$Lookup.value == "ITA"])),
  c(toupper("Malta -> Canada -> Switzerland -> Kenya ;-)"), as.character(country$Country.name[country$Lookup.value == "KEN"])),
  c("D F L", as.character(country$Country.name[country$Lookup.value == "LIE"])),
  c("THE NETHERLANDS", as.character(country$Country.name[country$Lookup.value == "NLD"])),
  c("HOLLAND", as.character(country$Country.name[country$Lookup.value == "NLD"])),
  c("RUSSIA", as.character(country$Country.name[country$Lookup.value == "RUS"])),
  c("RUSSIAN", as.character(country$Country.name[country$Lookup.value == "RUS"])),
  c("RUSSIAN FEDERATION", as.character(country$Country.name[country$Lookup.value == "RUS"])),
  c("SVERIGE", as.character(country$Country.name[country$Lookup.value == "SWE"])),
  c("S", as.character(country$Country.name[country$Lookup.value == "SWE"])),
  c("THUY DIEN", as.character(country$Country.name[country$Lookup.value == "SWE"])),
  c("TURKIYE", as.character(country$Country.name[country$Lookup.value == "TUR"])),
  c(toupper("Taipei,Taiwan, R.O.C"), as.character(country$Country.name[country$Lookup.value == "TWN"])),
  c("TAIWAN", as.character(country$Country.name[country$Lookup.value == "TWN"])),
  c("TAIPEI", as.character(country$Country.name[country$Lookup.value == "TWN"])),
  c("HAWAII USA", as.character(country$Country.name[country$Lookup.value == "USA"])),
#  c(toupper("T?oRKİYE"), as.character(country$Country.name[country$Lookup.value == "TUR"])),
  c("UNITED STATES", as.character(country$Country.name[country$Lookup.value == "USA"])),
  c("UNITED STATES OF AMERICA", as.character(country$Country.name[country$Lookup.value == "USA"])),
  c("MENDOCINO", as.character(country$Country.name[country$Lookup.value == "USA"])),
  c("U.S", as.character(country$Country.name[country$Lookup.value == "USA"])),
  c(toupper("usa (san francisco)"), as.character(country$Country.name[country$Lookup.value == "USA"])),
  c("UNITED STAT STATESES", as.character(country$Country.name[country$Lookup.value == "USA"])),
  c("UNITED SATES", as.character(country$Country.name[country$Lookup.value == "USA"])),
  c("U.S.", as.character(country$Country.name[country$Lookup.value == "USA"])),
  c("U.S.A.", as.character(country$Country.name[country$Lookup.value == "USA"])),
  c("SCANDINAVIA", mult),
  c("EU", mult),
  c("DK / UK", mult),
  c(toupper("Argentina-France-others"), mult),
  c(toupper("Guatemala/USA"), mult),
  c(toupper("USA, Taiwan"), mult),
  c(toupper("usa/mexico"), mult)
))
country <- rbind(country, setNames(country.corr,names(country)))
colnames(df)[colnames(df) == "Your.country.of.residence."] <- "Country.raw"
df <- cbind(df, Country.of.residence = country$Country.name[match(toupper(trim(df$Country.raw)), country$Lookup.value)])
index <- df$Country.raw == grep("RK", df$Country.raw, value=TRUE) & is.na(df$Country.of.residence)
df$Country.of.residence[index] <- as.character(country$Country.name[country$Lookup.value == "TUR"])
nbr.per.country <- count.responses(df$Country.of.residence)

# Test is more clean-up is needed
df$Country.raw[is.na(df$Country.of.residence)][!is.na(df$Country.raw[is.na(df$Country.of.residence)])]

export.to.file = F
if (export.to.file) {
  cols <- sapply(df, is.logical)
  df[,cols] <- lapply(df[,cols], as.numeric)
  write.xlsx(x=df, file="data/Clean.xlsx")
}