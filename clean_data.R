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
na.to.string <- function (x, str) {
  x <- as.character(x)
  x[is.na(x)] <- str
  as.factor(x)
}
repl.string <- function (x, before, after) {
  x <- as.character(x)
  x[x == before] <- after
  as.factor(x)
}

# Nice to have variables
dontknow <- "Don't know"
noresponse <- "(No response)"
yes <- "Yes"
no <- "No"

# data treatment starts here
df <- df_raw

# find unique rows when time rounded off to hours, 
# but go back to original time stamps
df <- cbind(Time.by.hour=round(df[,1], units="days"), df)
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
df$Dancing.role = na.to.string(df$Dancing.role, noresponse)
df$Is.a.leader <- df$Dancing.role == "Leader" | df$Dancing.role == "Leader and follower in equal amounts"
df$Is.a.follower <- df$Dancing.role == "Follower" | df$Dancing.role == "Leader and follower in equal amounts"
df$Is.a.leader.only <- df$Dancing.role == "Leader"
df$Is.a.follower.only <- df$Dancing.role == "Follower"
df$Is.a.follower.and.a.leader <- df$Dancing.role == "Leader and follower in equal amounts"

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

tension <- "Tension."
colnames(df)[colnames(df) == "Do.you.ever.feel.tense.in.your.body.during..or.right.after..tango.dancing."] <- "Feeling.tense.raw"
colnames(df)[colnames(df) == "If.yes..then.where."] <- "Feeling.tense.where.raw"
df$Feeling.tense.raw <- na.to.string(df$Feeling.tense.raw, noresponse)
df$Feeling.tense <- df$Feeling.tense.raw == yes
df$Feeling.tense.where.raw <- na.to.string(df$Feeling.tense.where.raw, noresponse)
# write.xlsx(x=, df$Feeling.tense.where.raw[!is.na(df$Feeling.tense.where.raw)], file="data/Tense_where_raw.xlsx")
body.parts = c("toe", "foot", "ankle", "calf", "knee", "leg", "hip", "lower back", "upper back", "back", "abdomen", 
                     "chest", "shoulder", "elbow", "wrist", "arm", "neck", "all body", "thigh", "right", "left")
col.names = gsub(" ", "\\.",body.parts)
map <- cbind(input.string=body.parts, col.name=col.names)
variants <- data.frame(rbind(
  c(" all ", "all.body"),
  c("everywhere", "all.body"),
  c("all over", "all.body"),
  c("kene", "knee"),
  c("beck", "back"),
  c("feet", "foot"),
  c("hamstring", "thigh"),
  c("calves", "calf"),
  c("core", "abdomen"),
  c("lower traps and lats", "upper.back"),
  c("scapula", "shoulder"),
  c("de hele rug", "back"),
  c("pciatica", "lower.back"),
  c("it band/quads", "thigh"),
  c("hip flexors", "hip"),
  c("lower belly", "abdomen"),
  c("rygg", "back"),
  c("skuldra", "shoulder"),
  c("loewe back", "lower.back"),
  c("skulder", "shoulder"),
  c("back, upper", "upper.back")
))
map <- rbind(map, setNames(variants,colnames(map)))
for (c in col.names) {
  df[paste(tension, c, sep="")] <- F
}
for (input.str in map$input.string) {
  colname <- paste(tension, map$col.name[map$input.string == input.str],sep="")
  hits <- df$Feeling.tense.where.raw %in% 
    grep(input.str, df$Feeling.tense.where.raw, ignore.case = T, value=T)
  df[,colname] <- df[,colname] | hits
}
false.pos <- data.frame(rbind(
  c("push back", "back"),
  c("Problems with knees and other parts appear very seldom", "knee"),
#  c("who project their head forward", "head"),  (not yet a hit column)
  c("without the right preparation", "right")
))
colnames(false.pos) <- c("input.string", "col.name")
for (input.str in false.pos$input.string) {
  colname <- paste(tension, false.pos$col.name[false.pos$input.string == input.str],sep="")
  false.hits <- df$Feeling.tense.where.raw %in% 
    grep(input.str, df$Feeling.tense.where.raw, ignore.case = T, value=T)
  df[,colname] <- df[,colname] & !false.hits
}
# Could be tested by looking at the free text of all rows that have no listed body parts mentioned

# Waking up sore
colnames(df)[colnames(df) == 
  paste("Have.you.ever.woken.up.with.a.sore.neck..upper.back",
        ".or.shoulder.after.a.night.of.tango.dancing.",sep="")] <- "Wake.up.sore.raw"
df$Wake.up.sore.raw <- na.to.string(df$Wake.up.sore.raw, noresponse)
df$Wake.up.sore <- df$Wake.up.sore.raw == yes

## Lower back pain
colnames(df)[colnames(df) == "Have.you.ever.experienced.lower.back.pain."] <- "Lower.back.pain.raw"
df$Lower.back.pain.raw <- na.to.string(df$Lower.back.pain.raw, noresponse)
yes.tango.and.unrelated <- "Yes, both in tango and in unrelated situations"
yes.tango.only <- "Yes, only during or right after tango"
yes.unrelated.only <- "Yes, only in situations unrelated to tango (work, at home...)"
df$Lower.back.pain.tango.and.elsewhere <- df$Lower.back.pain.raw == yes.tango.and.unrelated
df$Lower.back.pain.tango.only <- df$Lower.back.pain.raw == yes.tango.only
df$Lower.back.pain.elsewhere.only <- df$Lower.back.pain.raw == yes.unrelated.only

## Hallux valgus
colnames(df)[colnames(df) == "Do.you.have.bunions..hallux.valgus..on.your.big.toe.joints."] <- "Hallux.valgus.raw"
df$Hallux.valgus.raw <- na.to.string(df$Hallux.valgus.raw, noresponse)
df$Hallux.valgus <- df$Hallux.valgus.raw == yes

## Ankle sprain
colnames(df)[colnames(df) == paste("Have.you.ever.sprained.an.ankle..i.e..twisted.the.joint.between",
  ".the.leg.and.the.foot.so.it.got.sore.and.swollen..while.dancing.tango.", sep="")] <- "Ankle.sprain.raw"
df$Ankle.sprain.raw <- na.to.string(df$Ankle.sprain.raw, noresponse)
df$Ankle.sprain.from.tango <- df$Ankle.sprain.raw == yes

## Muscle strain
strained <- "Strained.muscle."
colnames(df)[colnames(df) == paste("Have.you.ever.strained.a.muscle..minor.muscle.injury.which.is.painful",
  ".but.often.heals.within.a.couple.of.days..while.dancing.tango.", sep="")] <- "Strained.muscle.raw"
df$Strained.muscle.raw <- na.to.string(df$Strained.muscle.raw, noresponse)
df$Strained.muscle <- df$Strained.muscle == yes
colnames(df)[colnames(df) == paste("If.your.answer.was.yes.to.the.above.question..in.which.body.parts.have",
  ".you.had.muscle.strains.", sep="")] <- "Strained.muscle.where.raw"
df$Strained.muscle.where.raw <- na.to.string(df$Strained.muscle.where.raw, noresponse)

body.parts = c("left", "right", "abdomen", "ankle", "arm", "back", "lower back", "upper back", "calf", 
               "chest", "foot", "glutes", "groin", "hamstring", "heel", "hip", "knee", "leg", "neck", 
               "shoulder", "quadriceps", "ribs", "thigh", "toe", "wrist")
col.names = gsub(" ", "\\.",body.parts)
map <- cbind(input.string=body.parts, col.name=col.names)
variants <- data.frame(rbind(
  c("bottom", "glutes"),
  c("spine", "back"),
  c("calves", "calf"),
  c("food", "foot"),
  c("achilles", "heel"),
  c("feet", "foot"),
  c("quadratus lumborum", "back"),           
  c("belly", "abdomen"),
  c("sciatica", "lower.back"),
  c("sciatic", "back"),
  c("posterior thigh", "hamstring"),
  c("gemelos", "calf"),
  c("trapecio", "neck"),
  c("isquiotibiales", "hamstring"),   
  c("arch", "foot"),
  c("onderbeen", "leg"),       
  c("rotator cuff", "shoulder"),
  c("instep", "foot"),
  c("adducctor", "thigh"),
  c("lower leg back muscle", "calf"), 
  c(" l ", "left"),
  c(" r ", "right"),
  c("thorax muscles", "chest"),
  c("quads", "quadriceps"),
  c("thigh front muscles", "quadriceps")
))
map <- rbind(map, setNames(variants,colnames(map)))
for (c in col.names) {
  df[paste(strained, c, sep="")] <- F
}
for (input.str in map$input.string) {
  colname <- paste(strained, map$col.name[map$input.string == input.str],sep="")
  hits <- df$Strained.muscle.where.raw %in% 
    grep(input.str, df$Strained.muscle.where.raw, ignore.case = T, value=T)
  df[,colname] <- df[,colname] | hits
}
false.pos <- data.frame(rbind(
  c("with foot planted", "foot"),
  c("heels too high", "heel"),
  c("back of", "back"),
  c("leg back muscle", "back"),
  c("wearing heels", "heel"),
  c("than on right", "right"),
  c("made it come back", "back"),
  c("dance in heels", "heel")
))
colnames(false.pos) <- c("input.string", "col.name")
for (input.str in false.pos$input.string) {
  colname <- paste(strained, false.pos$col.name[false.pos$input.string == input.str],sep="")
  false.hits <- df$Strained.muscle.where.raw %in% 
    grep(input.str, df$Strained.muscle.where.raw, ignore.case = T, value=T)
  df[,colname] <- df[,colname] & !false.hits
}

## Knee injury
colnames(df)[colnames(df) == "Have.you.ever.injured.a.knee.while.dancing.tango."] <- "Knee.injury"
df$Knee.injury <- na.to.string(df$Knee.injury, noresponse)
df$Knee.injury <- df$Knee.injury == yes
colnames(df)[colnames(df) == paste("If.your.answer.was.yes.to.the.above.question..please.specify.the.kind",
  ".of.injury.to.your.knee..meniscus.tear..torn.ligament......", sep="")] <- "Knee.injury.what.txt"
df$Knee.injury.what.txt <- na.to.string(df$Knee.injury.what.txt, noresponse)
# finds occurrences in the data.col of each given string from the pattern.vector
# an returns a column with T where any match was found, F otherwise
find.hits <- function (input.col, pattern.vector) {
  result <- input.col == F
  for (p in pattern.vector) {
    hits <- input.col %in% grep(p, input.col, ignore.case = T, value=T)
    result <- result | hits
  }
  result
}
map.symptoms.to.cols <- function(input.col, col.prefix, symptom.list) {
  result.df <- data.frame(input.col)
  colnames(result.df) <- "Dummy"
  for (n in names(symptom.list)) {
    colname <- paste(col.prefix, n, sep="")
    result.df[,colname] <- find.hits(input.col, unlist(symptom.list[n]))
  }
  data.frame(subset(result.df, select = -Dummy))
}
symptom.list <- list(
  swollen = c("swell", "swoll", "swilled"),
  inflammation = c("inflammation", "bursitis", "tendinitis"),
  pain = c("pain", "sore", "hurt", "impingement"),
  strain = c("strain", "stress", "pressure"),
  tear.or.sprain = c("torn", "tear", "sprain"),
  arthritis = c("artrosis", "arthrosis", "artros", "osteoarthritis"),
  loss.of.rom = c("could not bend", "stiff"),
  imbalanced = c("feeling an imbalance", "misaligned"),
  open.wound = c("open wound")
)
df <- cbind(df, map.symptoms.to.cols(df$Knee.injury.what, "Knee.injury.", symptom.list))

#Knee.injury.where.patella
# "patella"
# "cartilage"
# "meniscus"
# "ligament"/ "ACL" / "tendons"
# "fat pad"

#Knee.injury.cause.floor
#Knee.injury.cause.overuse
#Knee.injury.cause.pivot
#Knee.injury.cause.shoes
#Knee.injury.cause.leader
#Knee.injury.cause.nontango
# "floor" / "surface" / "rubber sole"
# "overuse" / "after the first marathon"
# "pivot" / "turn" / "ocho" / "rotations" / "twist"
# "leader"
# "ancient damage" / "from ski" / "not directly related to tango" / "due to the age" / "caused by "hope" dancing, but flaring up during tango"
# "not wearing high heels help"

#Knee.injury.duration.text
# "for a while"
# "three weeks"
# "a few days"
# "for a day"
# "a couple of days"
# "could not dance for awhile"
# "nothing a good nights sleep/rest havnt cured jet"


# 
## Open wound <- 
colnames(df)[colnames(df) == "Have.you.ever.gotten.an.open.wound..with.blood..from.dancing.tango."] <- "Open.wound.raw"                                                                                                                                                               
colnames(df)[colnames(df) == paste("If.your.answer.was.yes.to.the.above.question..in.which.body.part.did",
  ".you.get.wounded.and.how.", sep="")] <- "Open.wound.where.how.raw"

## Other tango injures <- 
colnames(df)[colnames(df) == paste("Please.describe.any.other.tango.injuries.resulting.from.something",
  ".specific.that.happened.on.the.dance.floor..e.g..hit.by.elbow..stepped.on..slipped.....",
  sep="")] <- "Other.tango.injury.raw"

## Longer term issues
colnames(df)[colnames(df) == paste("Have.you.experienced.any.longer.term.body.issues.you.have.experienced",
  ".that.might.be.related.to.your.tango.dancing.", sep="")] <- "Long.term.issues.raw"                                                                                                              
colnames(df)[colnames(df) == "If.yes..please.describe.the.issues."] <- "Long.term.issues.descr.raw"                                                                                                                                                                                             
colnames(df)[colnames(df) == paste("Please.describe.any.measures.you.have.taken..e.g..changing",
  ".of.technique..going.to.a.chiropractor..strengthening.muscles.in.the.gym..avoiding.to.dance",
  ".with.certain.partners...to.address.any.longer.term.tango.related.body.issues.",
  sep="")] <- "Long.term.issues.measures.taken.raw"

## Stopped dancing
colnames(df)[colnames(df) == paste("Did.you.ever.need.to.stop.dancing.for.a.period.of.time.because.of",
  ".body.issues..including.issues.caused.by.other.things.than.tango...", sep="")] <- "Stop.dancing.raw"
colnames(df)[colnames(df) == "If.yes..please.describe.the.cause."] <- "Stop.dancing.cause.raw"                                                                                                                                                                                                
colnames(df)[colnames(df) == "...and.tell.us.for.how.long.you.were.unable.to.dance."] <- 
  "Stop.dancing.how.long.raw"

## Other issues
colnames(df)[colnames(df) == paste("Any.more.body.issues.that.you.have.had.from.tango..but.that.weren.t",
  ".mentioned.so.far.in.the.survey.", sep="")] <- "Other.issues.raw"

## Oveall health
colnames(df)[colnames(df) == "How.would.you.rate.your.overall.health."] <- "Overall.health.raw"

## Age
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

# Country
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
  c("ENGLAND.", as.character(country$Country.name[country$Lookup.value == "GBR"])),
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
uk.flag <- "\xed\xa0\xbc\xed\xb7\xac\xed\xa0\xbc\xed\xb7\xa7 "
df$Country.raw <- repl.string(df$Country.raw, uk.flag, "UK")
df <- cbind(df, Country.of.residence = country$Country.name[match(toupper(trim(df$Country.raw)), country$Lookup.value)])
index <- df$Country.raw == grep("RK", df$Country.raw, value=TRUE) & is.na(df$Country.of.residence)
df$Country.of.residence[index] <- as.character(country$Country.name[country$Lookup.value == "TUR"])
nbr.per.country <- count.responses(df$Country.of.residence)
rm(uk.flag)

# Test is more clean-up is needed
df$Country.raw[is.na(df$Country.of.residence)][!is.na(df$Country.raw[is.na(df$Country.of.residence)])]

export.to.file = F
if (export.to.file) {
  cols <- sapply(df, is.logical)
  df[,cols] <- lapply(df[,cols], as.numeric)
  write.xlsx(x=df, file="data/Clean.xlsx")
}