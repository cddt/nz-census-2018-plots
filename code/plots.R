library(data.table)
library(ggplot2)

# ethnicity overall
data = fread('/home/cddt/cenus_2018/data/Ethnic_FILES/TABLECODE8343_Data_412ffc63-b208-41cb-bd95-43a4d4fd65f7.csv')
data = data[ ,c('Area','Sex','Year','Flags'):=NULL]
data[ , Ratio:= Value/sum(Value), by = `Ethnic group`]
data = data[`Ethnic group` != 'Not elsewhere included',]
data[data[,`Ethnic group` == 'Middle Eastern/Latin American/African'],`Ethnic group`:='MELAA']

image = ggplot(data, aes(x=reorder(`Ethnic group`,Value), y=`Ratio`, fill = `Religious affiliation`)) +
  geom_bar(stat = "identity", width=c(0.8), colour='black') +
  coord_flip() + 
  scale_fill_hue(name="Religious Affiliations", labels = c("None", "Object to answering","One or more")) +      # Set legend title
  #scale_fill_manual(values=c("red", "blue", "green"), name="Religious Affiliations", labels = c("None", "Object to answering","One or more")) +
  ylab("Percentage of Ethnic Group") + #xlab("Ethnic Group") + # Set axis labels
  ggtitle("Religious Affiliation by Ethnic Group")  +   # Set title
  theme_minimal(base_size = 18) + 
  theme(axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        plot.caption = element_text(colour = "grey", hjust = 1.4, vjust = 5, size = 10)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(caption="© Charles Tremlett / www.cddt.nz")

ggsave(file="/home/cddt/cenus_2018/plots/religion_total_by_ethnicity.svg", plot=image, width=12, height=5)

# religion by gender
data = fread('/home/cddt/cenus_2018/data/Religious_age_sex_TA_FILES/TABLECODE8289_Data_4d6b5d50-3782-4f1d-9538-366355560e34.csv')

data = data[Area != 'Area Outside Territorial Authority',]
data[is.na(data$Value),Value := 0]
data = data[ ,c('Year','Flags'):=NULL]
#data[ , Ratio:= Value/sum(Value), by = `Ethnic group`]
# check total TA

data_all = data[Area == 'Total - Territorial Authority areas']
data_all_sum_sex = data_all[,.(Value = sum(Value)), by = .(`Religious affiliation`,`Sex`)]
data_all_sum_sex[ , Ratio:= Value/sum(Value), by = `Sex`]

data_all_sum_all = data_all[,.(Value = sum(Value)), by = .(`Religious affiliation`)]
data_all_sum_all[ , Ratio:= Value/sum(Value)]

image = ggplot(data_all_sum_all, aes(x = 'Total', y=Ratio, fill = `Religious affiliation`)) +
  geom_bar(stat = "identity", width=c(0.8), colour='black') +
#  geom_bar(data = data_all_sum_sex, aes(x = Sex, y=Ratio), stat = "identity", width=c(0.4), colour='black') +
  coord_flip() + 
  scale_fill_hue(name="Religious Affiliations", labels = c("None", "Object to answering","One or more")) +      # Set legend title
  #scale_fill_manual(values=c("red", "blue", "green"), name="Religious Affiliations", labels = c("None", "Object to answering","One or more")) +
  ylab("Percentage of Population") + #xlab("Ethnic Group") + # Set axis labels
  ggtitle("Religious Affiliation in New Zealand")  +   # Set title
  theme_minimal(base_size = 18) + 
  theme(axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        plot.caption = element_text(colour = "grey", hjust = 1.4, vjust = 5, size = 10)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(caption="© Charles Tremlett / www.cddt.nz")
ggsave(file="/home/cddt/cenus_2018/plots/religion_total.svg", plot=image, width=12, height=3)

image = ggplot(data_all_sum_sex, aes(x = Sex, y=Ratio, fill = `Religious affiliation`)) +
  geom_bar(stat = "identity", width=c(0.8), colour='black') +
  #  geom_bar(data = data_all_sum_sex, aes(x = Sex, y=Ratio), stat = "identity", width=c(0.4), colour='black') +
  coord_flip() + 
  scale_fill_hue(name="Religious Affiliations", labels = c("None", "Object to answering","One or more")) +      # Set legend title
  #scale_fill_manual(values=c("red", "blue", "green"), name="Religious Affiliations", labels = c("None", "Object to answering","One or more")) +
  ylab("Percentage of Population") + #xlab("Ethnic Group") + # Set axis labels
  ggtitle("Religious Affiliation in New Zealand by Sex")  +   # Set title
  theme_minimal(base_size = 18) + 
  theme(axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        plot.caption = element_text(colour = "grey", hjust = 1.4, vjust = 5, size = 10)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(caption="© Charles Tremlett / www.cddt.nz")
ggsave(file="/home/cddt/cenus_2018/plots/religion_total_sex.svg", plot=image, width=12, height=3)

data[!(Area %like% 'Local Board') & (Area != 'Total - Territorial Authority areas'),sum(Value), by = Area]
data_area = data[!(Area %like% 'Local Board') & (Area != 'Total - Territorial Authority areas')]
data_area_auckland = data[(Area %like% 'Local Board')]


#unique(data[,'Area'], by=c('Area'))

data_area
data_area_sum_sex = data_area[,.(Value = sum(Value)), by = .(`Religious affiliation`,`Sex`,`Area`)]
data_area_sum_sex[ , Ratio:= Value/sum(Value), by = .(`Sex`,`Area`)]

data_area_sum = data_area[,.(Value = sum(Value)), by = .(`Religious affiliation`,`Area`)]
data_area_sum[ , Ratio:= Value/sum(Value), by = .(`Area`)]

data_area_sum_auckland = data_area_auckland[,.(Value = sum(Value)), by = .(`Religious affiliation`,`Area`)]
data_area_sum_auckland[ , Ratio:= Value/sum(Value), by = .(`Area`)]


data_sum_age = data_area[,.(Value = sum(Value)), by = .(`Religious affiliation`,`Age group`)]
data_sum_age[ , Ratio:= Value/sum(Value), by = .(`Age group`)]

image = ggplot(data_sum_age, aes(x = factor(`Age group`, level = c("0-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85-89 years", "90 years and over")), y=Ratio, fill = `Religious affiliation`)) +
  geom_bar(stat = "identity", width=c(0.8), colour='black') +
  #  geom_bar(data = data_all_sum_sex, aes(x = Sex, y=Ratio), stat = "identity", width=c(0.4), colour='black') +
  coord_flip() + 
  scale_fill_hue(name="Religious Affiliations", labels = c("None", "Object to answering","One or more")) +      # Set legend title
  #scale_fill_manual(values=c("red", "blue", "green"), name="Religious Affiliations", labels = c("None", "Object to answering","One or more")) +
  ylab("Percentage of Population") + #xlab("Ethnic Group") + # Set axis labels
  ggtitle("Religious Affiliation in New Zealand by Age")  +   # Set title
  theme_minimal(base_size = 18) + 
  theme(axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        plot.caption = element_text(colour = "grey", hjust = 1.4, vjust = 5, size = 10)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(caption="© Charles Tremlett / www.cddt.nz")
ggsave(file="/home/cddt/cenus_2018/plots/religion_total_age.svg", plot=image, width=12, height=8)

image = ggplot(data_area_sum, aes(x = reorder(Area, Ratio, first), y=Ratio, fill = `Religious affiliation`)) +
  geom_bar(stat = "identity", width=c(0.8), colour='black') +
  #  geom_bar(data = data_all_sum_sex, aes(x = Sex, y=Ratio), stat = "identity", width=c(0.4), colour='black') +
  coord_flip() + 
  scale_fill_hue(name="Religious Affiliations", labels = c("None", "Object to answering","One or more")) +      # Set legend title
  #scale_fill_manual(values=c("red", "blue", "green"), name="Religious Affiliations", labels = c("None", "Object to answering","One or more")) +
  ylab("Percentage of Population") + #xlab("Ethnic Group") + # Set axis labels
  ggtitle("Religious Affiliation in New Zealand by Territorial Authority")  +   # Set title
  theme_minimal(base_size = 18) + 
  theme(axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        plot.caption = element_text(colour = "grey", hjust = 1.4, vjust = 5, size = 10)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(caption="© Charles Tremlett / www.cddt.nz")
ggsave(file="/home/cddt/cenus_2018/plots/religion_total_area.svg", plot=image, width=12, height=15)

#setorder(data_area_sum_auckland, -'Religious affiliation',-'Ratio')

data_area_sum_auckland[,Area := gsub(' Local Board Area', '', data_area_sum_auckland[,Area])]

image = ggplot(data_area_sum_auckland, aes(x = reorder(Area, Ratio, first), y=Ratio, fill = `Religious affiliation`)) +
  geom_bar(stat = "identity", width=c(0.8), colour='black') +
  #  geom_bar(data = data_all_sum_sex, aes(x = Sex, y=Ratio), stat = "identity", width=c(0.4), colour='black') +
  coord_flip() + 
  scale_fill_hue(name="Religious Affiliations", labels = c("None", "Object to answering","One or more")) +      # Set legend title
  #scale_fill_manual(values=c("red", "blue", "green"), name="Religious Affiliations", labels = c("None", "Object to answering","One or more")) +
  ylab("Percentage of Population") + #xlab("Ethnic Group") + # Set axis labels
  ggtitle("Religious Affiliation in Auckland by Local Board")  +   # Set title
  theme_minimal(base_size = 18) + 
  theme(axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        plot.caption = element_text(colour = "grey", hjust = 1.4, vjust = 5, size = 10)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(caption="© Charles Tremlett / www.cddt.nz")
ggsave(file="/home/cddt/cenus_2018/plots/religion_total_area_auckland.svg", plot=image, width=12, height=8)

data_sum_age_sex = data_area[,.(Value = sum(Value)), by = .(`Religious affiliation`,`Age group`,`Sex`)]
data_sum_age_sex[ , Ratio:= Value/sum(Value), by = .(`Age group`,`Sex`)]

data_sum_age_sex_yes = data_sum_age_sex[`Religious affiliation` == 'Total people - with at least one religious affiliation']

image = ggplot(data_sum_age_sex_yes) +
  #geom_bar(stat = "identity", width=c(0.8), colour='black') +
  geom_line(aes(x = factor(`Age group`, level = c("0-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85-89 years", "90 years and over")), y=Ratio, group = Sex, colour = Sex), size = 1.5, alpha = 0.5) +
  #  geom_bar(data = data_all_sum_sex, aes(x = Sex, y=Ratio), stat = "identity", width=c(0.4), colour='black') +
#  coord_flip() + 
  scale_fill_hue(name="Religious Affiliations", labels = c("None", "Object to answering","One or more")) +      # Set legend title
  #scale_fill_manual(values=c("red", "blue", "green"), name="Religious Affiliations", labels = c("None", "Object to answering","One or more")) +
  ylab("Percentage with a Religious Affiliation") + #xlab("Ethnic Group") + # Set axis labels
  ggtitle("Religious Affiliation in New Zealand by Age and Sex")  +   # Set title
  theme_minimal(base_size = 18) + 
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        plot.caption = element_text(colour = "grey", hjust = 1.2, vjust = 0, size = 10)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(caption="© Charles Tremlett / www.cddt.nz")
ggsave(file="/home/cddt/cenus_2018/plots/religion_total_age_sex.svg", plot=image, width=12, height=8)

data_sum_area_age_sex = data_area[,.(Value = sum(Value)), by = .(`Religious affiliation`,`Age group`,`Sex`,`Area`)]
data_sum_area_age_sex[ , Ratio:= Value/sum(Value), by = .(`Age group`,`Sex`,`Area`)]

data_sum_area_age_sex_yes = data_sum_area_age_sex[`Religious affiliation` == 'Total people - with at least one religious affiliation']
data_sum_area_age_sex_yes = data_sum_area_age_sex_yes[`Area` != 'Chatham Islands Territory']

image = ggplot(data_sum_area_age_sex_yes) +
  #geom_bar(stat = "identity", width=c(0.8), colour='black') +
  geom_line(aes(x = factor(`Age group`, level = c("0-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85-89 years", "90 years and over")), y=Ratio, group = Sex, colour = Sex), size = 1.5, alpha = 0.5) +
  #  geom_bar(data = data_all_sum_sex, aes(x = Sex, y=Ratio), stat = "identity", width=c(0.4), colour='black') +
  #  coord_flip() + 
  facet_wrap(~Area, ncol = 6) + 
  scale_fill_hue(name="Religious Affiliations", labels = c("None", "Object to answering","One or more")) +      # Set legend title
  #scale_fill_manual(values=c("red", "blue", "green"), name="Religious Affiliations", labels = c("None", "Object to answering","One or more")) +
  ylab("Percentage with a Religious Affiliation") + xlab("Age Group") + # Set axis labels
  ggtitle("Religious Affiliation in New Zealand by Age and Sex")  +   # Set title
  theme_minimal(base_size = 18) + 
  theme(#axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        plot.caption = element_text(colour = "grey", hjust = 1.1, vjust = 0, size = 10)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(caption="© Charles Tremlett / www.cddt.nz")
ggsave(file="/home/cddt/cenus_2018/plots/religion_total_area_age_sex.svg", plot=image, width=20, height=30)

data_area_age_sex_sum_auckland = data_area_auckland[,.(Value = sum(Value)), by = .(`Religious affiliation`,`Area`,`Sex`,`Age group`)]
data_area_age_sex_sum_auckland[ , Ratio:= Value/sum(Value), by = .(`Area`,`Sex`,`Age group`)]
data_area_age_sex_sum_auckland[,Area := gsub(' Local Board Area', '', data_area_age_sex_sum_auckland[,Area])]
data_area_age_sex_sum_auckland_yes = data_area_age_sex_sum_auckland[`Religious affiliation` == 'Total people - with at least one religious affiliation']
data_area_age_sex_sum_auckland_yes = data_area_age_sex_sum_auckland_yes[`Area` != 'Great Barrier']


image = ggplot(data_area_age_sex_sum_auckland_yes) +
  #geom_bar(stat = "identity", width=c(0.8), colour='black') +
  geom_line(aes(x = factor(`Age group`, level = c("0-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85-89 years", "90 years and over")), y=Ratio, group = Sex, colour = Sex), size = 1.5, alpha = 0.5) +
  #  geom_bar(data = data_all_sum_sex, aes(x = Sex, y=Ratio), stat = "identity", width=c(0.4), colour='black') +
  #  coord_flip() + 
  facet_wrap(~Area, ncol = 5) + 
  scale_fill_hue(name="Religious Affiliations", labels = c("None", "Object to answering","One or more")) +      # Set legend title
  #scale_fill_manual(values=c("red", "blue", "green"), name="Religious Affiliations", labels = c("None", "Object to answering","One or more")) +
  ylab("Percentage with a Religious Affiliation") + xlab("Age Group") + # Set axis labels
  ggtitle("Religious Affiliation in Auckland by Age and Sex")  +   # Set title
  theme_minimal(base_size = 18) + 
  theme(#axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    plot.title = element_text(hjust = 0.5, face = 'bold'),
    plot.caption = element_text(colour = "grey", hjust = 1.1, vjust = 0, size = 10)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(caption="© Charles Tremlett / www.cddt.nz")
ggsave(file="/home/cddt/cenus_2018/plots/religion_total_area_auckland_age_sex.svg", plot=image, width=20, height=15)

# religion by birthplace
data = fread('/home/cddt/cenus_2018/data/Religious_birthplace_FILES/TABLECODE8290_Data_0b32dce9-d24d-4b3a-9de8-3dfe5986b9c0.csv')

data = data[Area == 'Total - New Zealand by Territorial Authority/SA2',]
#data[is.na(data$Value),Value := 0]
data = data[ ,c('Year','Flags','Age group','Area'):=NULL]
data[ , Ratio:= Value/sum(Value), by = .(`Birthplace`)]
data[ , Total:= sum(Value), by = .(`Birthplace`)]

data[,Birthplace := gsub('United Kingdom', 'UK', data[,Birthplace])]


image = ggplot(data, aes(x= reorder(Birthplace,Total), y=`Ratio`, fill = `Religious affiliation`)) +
  geom_bar(stat = "identity", width=c(0.8), colour='black') +
  coord_flip() + 
  scale_fill_hue(name="Religious Affiliations", labels = c("None", "Object to answering","One or more")) +      # Set legend title
  #scale_fill_manual(values=c("red", "blue", "green"), name="Religious Affiliations", labels = c("None", "Object to answering","One or more")) +
  ylab("Percentage") + #xlab("Ethnic Group") + # Set axis labels
  ggtitle("Religious Affiliation by Birthplace")  +   # Set title
  theme_minimal(base_size = 18) + 
  theme(axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        plot.caption = element_text(colour = "grey", hjust = 1.4, vjust = 5, size = 10)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(caption="© Charles Tremlett / www.cddt.nz")

ggsave(file="/home/cddt/cenus_2018/plots/religion_total_by_birthplace.svg", plot=image, width=12, height=5)

# ethnic group, age, sex
data = fread('/home/cddt/cenus_2018/data/Ethnic_highlevel_age_sex_FILES/TABLECODE8340_Data_c2b2ab94-eeb3-49dd-ad8e-8a3292312b52.csv')
data = data[Area == 'Total - New Zealand by Territorial Authority/SA2',]
#data[is.na(data$Value),Value := 0]
data = data[ ,c('Year','Flags','Area'):=NULL]
data[ , Ratio:= Value/sum(Value), by = .(`Ethnic group`)]
data[ , Total:= sum(Value), by = .(`Ethnic group`)]

setorder(data, -Total)



data$`Ethnic group` <- factor(data$`Ethnic group`, levels = sort(unique(data$`Ethnic group`),))
reorder(data$`Ethnic group`, data$Total)
data[,`Ethnic group` := gsub('Middle Eastern/Latin American/African', 'MELAA', data[,`Ethnic group`])]


image = ggplot(data) +
  #geom_bar(stat = "identity", width=c(0.8), colour='black') +
  geom_line(aes(x = factor(`Age group`, level = c("0-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85-89 years", "90 years and over")), y=Ratio, group = Sex, colour = Sex), size = 1.5, alpha = 0.5) +
  #  geom_bar(data = data_all_sum_sex, aes(x = Sex, y=Ratio), stat = "identity", width=c(0.4), colour='black') +
  #  coord_flip() + 
  facet_wrap(~reorder(data$`Ethnic group`, -data$Total), ncol = 4) + 
  scale_fill_hue(name="Ethnic Group") +      # Set legend title
  #scale_fill_manual(values=c("red", "blue", "green"), name="Religious Affiliations", labels = c("None", "Object to answering","One or more")) +
  ylab("Percentage of Ethnic Group") + xlab("Age Group") + # Set axis labels
  ggtitle("Age Profiles of Ethnic Groups")  +   # Set title
  theme_minimal(base_size = 18) + 
  theme(#axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    plot.title = element_text(hjust = 0.5, face = 'bold'),
    plot.caption = element_text(colour = "grey", hjust = 1.1, vjust = 0, size = 10)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(caption="© Charles Tremlett / www.cddt.nz")
ggsave(file="/home/cddt/cenus_2018/plots/age_profile_ethnic_group.svg", plot=image, width=16, height=16)

data[ , Ratio_all:= Value/sum(Value)]

image = ggplot(data) +
  #geom_bar(stat = "identity", width=c(0.8), colour='black') +
  geom_line(aes(x = factor(`Age group`, level = c("0-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85-89 years", "90 years and over")), y=Ratio_all, group = Sex, colour = Sex), size = 1.5, alpha = 0.5) +
  #  geom_bar(data = data_all_sum_sex, aes(x = Sex, y=Ratio), stat = "identity", width=c(0.4), colour='black') +
  #  coord_flip() + 
  facet_wrap(~reorder(data$`Ethnic group`, -data$Total), ncol = 4) + 
  scale_fill_hue(name="Ethnic Group") +      # Set legend title
  #scale_fill_manual(values=c("red", "blue", "green"), name="Religious Affiliations", labels = c("None", "Object to answering","One or more")) +
  ylab("Percentage of Total Population") + xlab("Age Group") + # Set axis labels
  ggtitle("Age Profiles of Ethnic Groups (within overall population)")  +   # Set title
  theme_minimal(base_size = 18) + 
  theme(#axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    plot.title = element_text(hjust = 0.5, face = 'bold'),
    plot.caption = element_text(colour = "grey", hjust = 1.1, vjust = 0, size = 10)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(caption="© Charles Tremlett / www.cddt.nz")
ggsave(file="/home/cddt/cenus_2018/plots/age_profile_ethnic_group_all.svg", plot=image, width=16, height=16)

paste(unique(data$`Ethnic group`),collapse='","')

data_only = data[!(`Ethnic group` %in% c("Maori/European","Pacific Peoples/European","Maori/Pacific Peoples","Maori/Pacific Peoples/European","Asian/European","Two groups not elsewhere included","Three groups not elsewhere included","Four to six groups")),]
data_only[,c('Ratio','Total','Ratio_all'):=NULL]
data_mixed = data[(`Ethnic group` %in% c("Maori/European","Pacific Peoples/European","Maori/Pacific Peoples","Maori/Pacific Peoples/European","Asian/European","Two groups not elsewhere included","Three groups not elsewhere included","Four to six groups")),]
data_mixed = data_mixed[,.(Value = sum(Value)),by=.(Sex,`Age group`)]
data_mixed[,`Ethnic group`:='Mixed']

setcolorder(data_mixed, c('Ethnic group','Age group','Sex','Value'))

data_all = rbindlist(list(data_only, data_mixed))
data_all = data_all[,.(Value = sum(Value)),by=.(`Ethnic group`,`Age group`)]

data_all[,percentage:=Value/sum(Value), by = .(`Age group`)]

paste(unique(data_all$`Ethnic group`), collapse='", "')

image = ggplot(data_all, aes(x=factor(`Age group`, level = c("0-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85-89 years", "90 years and over")), y=percentage, fill = factor(`Ethnic group`, levels= c("European only", "Asian only", "Maori only", "Pacific Peoples only", "MELAA only", "Other Ethnicity only", "Mixed")), group = factor(`Ethnic group`, levels= c("European only", "Asian only", "Maori only", "Pacific Peoples only", "MELAA only", "Other Ethnicity only", "Mixed")))) + 
  geom_area(position = 'stack', alpha = 0.8, size = 0.5, colour = 'black') + 
  scale_fill_hue(name="Ethnic Group") +      # Set legend title
  #scale_fill_manual(values=c("red", "blue", "green"), name="Religious Affiliations", labels = c("None", "Object to answering","One or more")) +
  ylab("Percentage of Total Population") + xlab("Age Group") + # Set axis labels
  ggtitle("Ethnic Groups by Age Group")  +   # Set title
  theme_minimal(base_size = 18) + 
  theme(#axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    plot.title = element_text(hjust = 0.5, face = 'bold'),
    plot.caption = element_text(colour = "grey", hjust = 1.4, vjust = 0, size = 10)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(caption="© Charles Tremlett / www.cddt.nz")
ggsave(file="/home/cddt/cenus_2018/plots/ethnic_age_profile.svg", plot=image, width=12, height=8)

image = ggplot(data_all, aes(x=factor(`Age group`, level = c("0-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85-89 years", "90 years and over")), y=Value, fill = factor(`Ethnic group`, levels= c("European only", "Asian only", "Maori only", "Pacific Peoples only", "MELAA only", "Other Ethnicity only", "Mixed")), group = factor(`Ethnic group`, levels= c("European only", "Asian only", "Maori only", "Pacific Peoples only", "MELAA only", "Other Ethnicity only", "Mixed")))) + 
  geom_area(position = 'stack', alpha = 0.8, size = 0.5, colour = 'black') + 
  scale_fill_hue(name="Ethnic Group") +      # Set legend title
  #scale_fill_manual(values=c("red", "blue", "green"), name="Religious Affiliations", labels = c("None", "Object to answering","One or more")) +
  ylab("Number of People") + xlab("Age Group") + # Set axis labels
  ggtitle("Ethnic Groups by Age Group")  +   # Set title
  theme_minimal(base_size = 18) + 
  theme(#axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    plot.title = element_text(hjust = 0.5, face = 'bold'),
    plot.caption = element_text(colour = "grey", hjust = 1.4, vjust = 0, size = 10)) +
  scale_y_continuous(labels = scales::comma) +
  labs(caption="© Charles Tremlett / www.cddt.nz")
ggsave(file="/home/cddt/cenus_2018/plots/ethnic_age_profile_count.svg", plot=image, width=12, height=8)


# birthplace by TA
data = fread('/home/cddt/cenus_2018/data/Birthplace_TA_FILES/TABLECODE8341_Data_711d61e0-a8c8-47ad-b43f-e37e1e2020e2.csv')
#data = data[Area == 'Total - New Zealand by Territorial Authority/SA2',]
#data[is.na(data$Value),Value := 0]
data = data[ ,c('Year','Flags','Age group','Ethnic group'):=NULL]
data[ , Ratio:= Value/sum(Value), by = .(Area)]
data[ , Total:= sum(Value), by = .(Area)]

data_area_birthplace = data[!(Area %like% 'Local Board') & (Area != 'Area Outside Territorial Authority') & (Area != 'Total - Territorial Authority areas')]
data_area_birthplace_auckland = data[(Area %like% 'Local Board')]
data_area_birthplace_auckland[,Area := gsub(' Local Board Area', '', data_area_birthplace_auckland[,Area])]

data[, Birthplace:=factor(`Birthplace`, levels = c("New Zealand", "North-West Europe", "North-East Asia", "Pacific Islands", "Southern and Central Asia", "South-East Asia", "Sub-Saharan Africa", "Australia", "The Americas", "Southern and Eastern Europe", "North Africa and the Middle East"))]

setorder(data_area_birthplace, Birthplace, Ratio)
setorder(data_area_birthplace_auckland, Birthplace, Ratio)

image = ggplot(data_area_birthplace, aes(x = reorder(Area, -Ratio, first), y=Ratio, fill = factor(`Birthplace`, levels = c("New Zealand", "North-West Europe", "North-East Asia", "Pacific Islands", "Southern and Central Asia", "South-East Asia", "Sub-Saharan Africa", "Australia", "The Americas", "Southern and Eastern Europe", "North Africa and the Middle East")))) +
  geom_bar(stat = "identity", width=c(0.8), colour='black', position = position_stack(reverse = TRUE)) +
  #  geom_bar(data = data_all_sum_sex, aes(x = Sex, y=Ratio), stat = "identity", width=c(0.4), colour='black') +
  coord_flip() + 
  scale_fill_hue(name="Birthplace") + #, labels = c("None", "Object to answering","One or more")) +      # Set legend title
  #scale_fill_manual(values=c("red", "blue", "green"), name="Religious Affiliations", labels = c("None", "Object to answering","One or more")) +
  ylab("Percentage of Population") + #xlab("Ethnic Group") + # Set axis labels
  ggtitle("Birthplace by Territorial Authority")  +   # Set title
  theme_minimal(base_size = 18) + 
  theme(axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        plot.caption = element_text(colour = "grey", hjust = 1.4, vjust = 5, size = 10)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(caption="© Charles Tremlett / www.cddt.nz")
ggsave(file="/home/cddt/cenus_2018/plots/birthplace_by_TA.svg", plot=image, width=12, height=15)

image = ggplot(data_area_birthplace_auckland, aes(x = reorder(Area, -Ratio, first), y=Ratio, fill = factor(`Birthplace`, levels = c("New Zealand", "North-West Europe", "North-East Asia", "Pacific Islands", "Southern and Central Asia", "South-East Asia", "Sub-Saharan Africa", "Australia", "The Americas", "Southern and Eastern Europe", "North Africa and the Middle East")))) +
  geom_bar(stat = "identity", width=c(0.8), colour='black', position = position_stack(reverse = TRUE)) +
  #  geom_bar(data = data_all_sum_sex, aes(x = Sex, y=Ratio), stat = "identity", width=c(0.4), colour='black') +
  coord_flip() + 
  scale_fill_hue(name="Birthplace") + #, labels = c("None", "Object to answering","One or more")) +      # Set legend title
  #scale_fill_manual(values=c("red", "blue", "green"), name="Religious Affiliations", labels = c("None", "Object to answering","One or more")) +
  ylab("Percentage of Population") + #xlab("Ethnic Group") + # Set axis labels
  ggtitle("Birthplace of Aucklanders by Local Board")  +   # Set title
  theme_minimal(base_size = 18) + 
  theme(axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        plot.caption = element_text(colour = "grey", hjust = 1.4, vjust = 5, size = 10)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(caption="© Charles Tremlett / www.cddt.nz")
ggsave(file="/home/cddt/cenus_2018/plots/birthplace_auckland_by_LB.svg", plot=image, width=12, height=8)

# median age by TA
data = fread('/home/cddt/cenus_2018/data/Median_age_TA_FILES/TABLECODE7510_Data_f432c156-118f-4d9c-b6d4-d610fd66af19.csv')
#data = data[Area == 'Total - New Zealand by Territorial Authority/SA2',]
#data[is.na(data$Value),Value := 0]
data = data[Measure == 'Median age']
data = data[,c('Flags','Measure'):=NULL]

data_median_age = data[!(Area %like% 'local board') & (Area != 'Area Outside Territorial Authority') & (Area != 'Total - Territorial Authority areas') & (Area != 'Total NZ by territorial authority')]
data_median_age_auckland = data[(Area %like% 'local board')]
data_median_age_auckland[,Area := gsub(' local board area', '', data_median_age_auckland[,Area])]

data_median_age = dcast(data_median_age, Area ~ `Year at 30 June`, value.var = 'Value')
data_median_age_auckland = dcast(data_median_age_auckland, Area ~ `Year at 30 June`, value.var = 'Value')

image = ggplot(data_median_age) +
  geom_segment(aes(x=reorder(Area,-`2013`), xend=Area, y=`2013`, yend=`2018`), color="darkgrey") +
  geom_point( aes(x=Area, y=`2013`), colour=rgb(0.2,0.7,0.1,0.7), size=4 ) +
  geom_point( aes(x=Area, y=`2013`), colour='black', shape = 1, size=4 ) +
  geom_point( aes(x=Area, y=`2018`), colour=rgb(0.7,0.2,0.1,0.7), size=4 ) +
  geom_point( aes(x=Area, y=`2018`), colour='black', shape = 1, size=4 ) +
  coord_flip() +
#  scale_color_manual("", breaks = c('2013','2018'), values = c('2013' = rgb(0.2,0.7,0.1,0.7),'2018' = rgb(0.7,0.2,0.1,0.7))) +
#  guides() + 
  #scale_fill_hue(name="Year") +
  ggtitle("Median Age by Territorial Authority, 2013-2018")  +   # Set title
  theme_minimal(base_size = 14) +
  annotate("text", x = 52, y = 52, label = "2013", color = 'black', size = 5) +
  geom_point(aes(x = 52, y = 50.5), colour =rgb(0.2,0.7,0.1,0.7), size=4) +
  geom_point(aes(x = 52, y = 50.5), colour='black', shape = 1, size=4  ) + 
  annotate("text", x = 50, y = 52, label = "2018", color = "black", size = 5) +
  geom_point(aes(x = 50, y = 50.5), colour =rgb(0.7,0.2,0.1,0.7), size=4  ) +
  geom_point(aes(x = 50, y = 50.5), colour='black', shape = 1, size=4  ) + 
  theme(#axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    plot.title = element_text(hjust = 0.5, face = 'bold'),
    plot.caption = element_text(colour = "grey", hjust = 1.0, vjust = 0, size = 10)) +
  #theme(legend.position = "none",) +
  xlab("") +
  ylab("Median Age") +
  labs(caption="© Charles Tremlett / www.cddt.nz")
ggsave(file="/home/cddt/cenus_2018/plots/median_age_by_TA.svg", plot=image, width=10, height=11)

image = ggplot(data_median_age_auckland) +
  geom_segment(aes(x=reorder(Area,-`2013`), xend=Area, y=`2013`, yend=`2018`), color="darkgrey") +
  geom_point( aes(x=Area, y=`2013`), colour=rgb(0.2,0.7,0.1,0.7), size=4 ) +
  geom_point( aes(x=Area, y=`2013`), colour='black', shape = 1, size=4 ) +
  geom_point( aes(x=Area, y=`2018`), colour=rgb(0.7,0.2,0.1,0.7), size=4 ) +
  geom_point( aes(x=Area, y=`2018`), colour='black', shape = 1, size=4 ) +
  coord_flip() +
  #  scale_color_manual("", breaks = c('2013','2018'), values = c('2013' = rgb(0.2,0.7,0.1,0.7),'2018' = rgb(0.7,0.2,0.1,0.7))) +
  #  guides() + 
  #scale_fill_hue(name="Year") +
  ggtitle("Median Age in Auckland by Local Board, 2013-2018")  +   # Set title
  theme_minimal(base_size = 14) +
  annotate("text", x = 16, y = 50, label = "2013", color = 'black', size = 5) +
  geom_point(aes(x = 16, y = 49), colour =rgb(0.2,0.7,0.1,0.7), size=4  ) + 
  geom_point(aes(x = 16, y = 49), colour='black', shape = 1, size=4  ) + 
  annotate("text", x = 14, y = 50, label = "2018", color = "black", size = 5) +
  geom_point(aes(x = 14, y = 49), colour =rgb(0.7,0.2,0.1,0.7), size=4  ) + 
  geom_point(aes(x = 14, y = 49), colour='black', shape = 1, size=4  ) + 
  
  theme(#axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    plot.title = element_text(hjust = 0.5, face = 'bold'),
    plot.caption = element_text(colour = "grey", hjust = 1.0, vjust = 0, size = 10)) +
  #theme(legend.position = "none",) +
  xlab("") +
  ylab("Median Age") +
  labs(caption="© Charles Tremlett / www.cddt.nz")
ggsave(file="/home/cddt/cenus_2018/plots/median_age_auckland_by_LB.svg", plot=image, width=10, height=5)



dev.off()

