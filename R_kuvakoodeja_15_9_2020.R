############################################
# BIOSTATISTIIKKA R KUVAKIRJASTO           #
############################################

# Päivitetty 18.9.2020
# Tekijä: Helena Ollila

# 1 IMPORT, PAKETIT ja muita neuvoja
# 2 KAPLAN-MEIER
# 3 VENN DIAGRAMMI
# 4 AIKASARJA / PERUSVIIVAKUVA
# 5 PYLVÄSKUVA ja kuvien yhdistely
# 6 AIKAPYLVÄSKUVA
# 7 REGRESSIOKUVA
# 8
# 9
# 10

############################## 1 IMPORT, PAKETIT ja muita neuvoja ############################

# Aseta työskentely kansio, huomaa kauttaviivat //

setwd("C:/Users/hekollDesktop/Siirretty verkkolevylle päivityksen ajaksi/Kotona/R kuvakirjasto")

# Sas datan tuominen R

install.packages("sas7bdat") # paketin asennus
library(sas7bdat) # paketin käyttöönotto
Data <- read.sas7bdat("datan_nimi.sas7bdat")

# XLSX datan tuominen R

install.packages("xlsx")
library(xlsx)
Data_a <- read.xlsx2("tiedostonnimi.xlsx", sheetIndex = 4, # sheet numero
                        header=TRUE) # muttujien nimet ekalla rivillä

# Muokkaa kategoriset muuttujat faktoriksi esim.

Data$Group <- factor(Data$kategorinenmuuttuja, levels = c("taso1", "taso2"))

# Seuraava paketti tarvitaan ggplot kuvien tekemiseen (eli kaikkien kirjaston kuvien tekemiseen)

install.packages("ggplot2")
library(ggplot2)

# Ole tarkkana sulkeiden kanssa, sulkeiden sisällä olevat määritykset kuuluvat samaan komentoon

############################## 2 KAPLAN-MEIER ############################

# Tarvittavat paketit 

# install.packages("survival")
# install.packages("survminer")
# install.packages("scales")

library(survival)
library(survminer)
library(scales)

# Tehdään elinaika analyysi, vastaa SASSIN proc lifetest

fit <- survfit(Surv(aika,tapahtuma)~Group,data=Data)

# Määritellään kuva

kuva <- ggsurvplot(
        fit, # mallin nimi
        data = Data, # aineisto
        size = 1,    # viivan koko
        palette = c("black", "red"),# värit, joita käytetää, niin monta kuin ryhmiä
        conf.int = TRUE,          # lisää luottamusväli alue
        risk.table = TRUE,        # lisää riskitaulukko kuvan alle
        legend.labs =c("po.", "iv. + po."),    # legendin ryhmien otsikot
        risk.table.height = 0.25, # riskitaulukon korkeus, kuinka iso osa koko kuvasta
        tables.theme = theme_void() , # riskitaulukon teema, theme_void on täysin tyhjä
        ggtheme = theme_bw(),            # kuvan teema
        xlim = c(0,12.6), # x akselin alku- ja loppupää
        break.time.by = 3, # kuinka monen luvun välein x-akselin aika ilmaistaan
        xlab = "Time since randomization, months", # x-akselin osikko
        fun = "event", # survival käyrän muunnos: "event" plots cumulative events (f(y) = 1-y), 
                        # "cumhaz" plots the cumulative hazard function (f(y) = -log(y))
        ylab = "Underwent Appendectomy", # y akselin otsikko
        legend.title= "", # legendin otsikko
        surv.scale = "percent", # y akseli prosenteiksi
        censor.shape="|", censor.size = 4, # sensoroitujen havaintojen merkintä merkki ja koko
        ylim=c(0,0.5), # y aksenlin alku- ja loppupää 
        fontsize=4, # riskitaulukon fonttikoko
        risk.table.title ="No. at risk", # riskitaulukon otsikko
        risk.table.y.text.col=FALSE, # riskitaulukon lukujen värit ryhmien mukaan
        risk.table.y.text = T) # riskitaulukkoon ryhmien nimet  

# Tällä lauseella saadaan y akselin prosenteista desimaalit pois

kuva$plot <- kuva$plot + scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.5)) 
# muista laittaa y akselin rajat uudestaan limits kohtaan

# Tulostetaan kuva "plots" osioon

kuva

# Jos kuvan päälle halutaan piirtää ylimääräisiä pisteitä tai viivoja, tehdään niille oma data
# ja piirretään uudesta datasta halutut kuviot geom_point tai geom_line komennon avulla.

# Esimerkki kuvassa on piirretty palloja viivojen päälle, datassa 2 on pallojen x ja y koordinaatit.

Data2 <- read.sas7bdat("data2.sas7bdat")

kuva$plot <-kuva$plot + geom_point(data = Data2, aes(x=x, y=y, colour = ryhma), size = 3) # colour komennolla saadaan ryhmille eri värit
                                                                                          # size on pallojen koko

kuva

############################## 3 VENN DIAGRAMMI ############################

# Tarvittavat paketit

# install.packages("VennDiagram")
library(VennDiagram)

# frekvenssitaulukot saadaan R:n table komennolla, sillä saa myös ristiintaulukot

table(Data$muuttuja) 
table(Data$muuttuja1, Data$muuttuja2) 

grid.newpage()
overrideTriple=T

# VENN diagrammi

draw.triple.venn(
        area1 = 1969, # koko sininen pallo
        area2 = 852, # koko vihreä pallo
        area3 = 849, # koko lila pallo
        n12 = 321, # alueiden leikkauskohdat
        n23 = 266, 
        n13 = 769,
        n123 = 248, 
        category = c("Inhaled corticosteroid medication", "Atopic eczema", "Asthma"),  # Otsikot
        rotation = 1, 
        reverse = FALSE, 
        euler.d = F, 
        scaled = FALSE,
        lty = "blank", # ei ääriviivoja
        fill = c("skyblue", "palegreen", "mediumorchid")) # värit


############################## 4 AIKASARJA / PERUSVIIVAKUVA ############################

# Esimerkissä x akseli on kategorinen

Data_c$aihe <- factor(Data_c$aihe, levels = c("Move","See","Hear","Breath","Sleep","Eat",
                "Speech","Excret","Uact","Mental","Disco","Depr","Distr","Vital","Sex"))

# Piiretään kuva C

kuva_c   <- ggplot(data = Data_c,  # Data
        aes(x=aihe, y=arvo, group = ryhma) )   + # x akseli, y akseli ja ryhmä muuttujat      
        ylab('Level value')    + # y akselin otsikko
        xlab('Dimensions')    + # x akselin otsikko
        geom_line(aes(linetype = ryhma), size = 1) + # eri viivatyypit ryhmille ja viivojen koko
        theme_minimal() + # kuvan teema, yksinkertainen
        geom_point(col="black") + # pisteet viivoihin ja niiden väri
        scale_y_continuous(breaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), limits = c(0, 1)) + # y akselin arvot ja päätepisteet
        theme(legend.position="bottom", legend.title = element_blank()) + # legendi kuvan alle ja ilman otsikkoa
        labs(title = "At 7 years") # koko kuvan otsikko


############################## 5 PYLVÄSKUVA ja kuvien yhdistely ############################

# Koska missään kuvissa ei ole kaikkia ryhmiä, legend pitää ottaa kuvasta, johon laitetaan kaikki ryhmät 

legendi <- ggplot(Data, # Data
                aes(x=tyytyvaisyys, y=tyytyvaisyys_pros, fill=Group)) + # x ja y akseli ja ryhmä muuuttuja
        geom_bar(stat="identity", # palkit erikseen, geom_bar = pylväskuva
                 position="dodge",  # eri ryhmien palkit vierekkäin eikä päällekkäin
                 colour="black") + # palkkien ääriviivat
        scale_fill_manual(values=c("gray23", "gray100","gray47","gray71")) # palkkien täyttövärit 

# Seuraava koodi ottaa legendi kuvasta pelkän legendin ja tallentaa sen muuttujaan legend 

legend <- get_legend(legendi + theme(legend.position="bottom"))

# Tehdään kuva A

A <- ggplot(dataA, aes(x=Tyytyvaisyys, y=tyytyvaisyys_pros, fill=Group)) + # x ja y akseli ja ryhmä muuttuja
        geom_bar(stat="identity", position="dodge", colour="black") + # pylväskuva, ryhmien palkit vierekkäin, viivan väri musta
        ggtitle("Patient satisfaction") + # Kuvan otsikko
        geom_text(aes(label=tyytyvaisyys_n, y=-2.4), # n määrät pylväiden alle, muuttuja, jossa n ja y akselin paikka
                  position=position_dodge(width=0.9), # n määrät ryhmittäin ja ryhmien lukujen välinen etäisyys
                  size=2.5)+ # fontin koko
        geom_text(aes(label=tyytyvaisyyspros, y=-5), position=position_dodge(width=0.9), size=2.5)+ # sama kuin yllä, mutta prosentti lukumäärät
        theme(legend.position = "none", # ei legendiä
              axis.title.y=element_blank(),  # ei y akselin otsikkoa
              panel.background = element_rect(fill = "white", colour = "black", # kuvan tausta valkoinen ja kehys musta
                                        size = 0.5, linetype = "solid"),# kehyksen koko ja viiva tyyppi
              panel.grid.major.y = element_line( size=0.25, color="grey" ), # harmaat y viivat kuvan taustalle
              plot.title = element_text(hjust = 0.5), # kuvan otsikon koko
              axis.text.x = element_text(size = 7)) + # x akselin arvojen koko
        scale_fill_manual(values=c("gray23", "gray100")) + # palkkien täyttövärit
        xlab("")  + # ei x akselin otsikkoa
        annotate("text", x = 0.5, y = 95, # lisätään tekstiä kuvaan tälle x ja y arvon kohdalle
                 label = "Difference between appendectomy and antibiotic groups, p<0.001*", # mikä teksti laitetaan
                 size=2.5, hjust = 0) + # tekstin koko 
        geom_errorbar(aes(ymin = C1, ymax = C2), # Pylväihin errorbar, joiden lukuarvon on laskettu muuttujiin C1 ja C2
                      width=0.2, position=position_dodge(.9)) + # virhemarginaalit ryhmien mukaan ja niiden etäisyys toisistaan
        coord_cartesian(ylim=c(-5,100)) # y akselin alku- ja loppupiste


# Yhdistetään osakuvat yhdeksi kuvaksi 

figure1 <- ggarrange(A, B, # Yhdistetään kuvat A ja B
                     labels = c("A", "B"), # Annetaan otsikot 
                     ncol = 2, nrow = 1) # laitetaan kuvat yhdelle riville vierekkäin

figure2 <- ggarrange(C, D, # Sama kuin yllä
                     labels = c("C", "D"),
                     ncol = 2, nrow = 1)

figure1 <- annotate_figure(figure1, left = text_grob("Percentage within group", # Lisätään kuviin yhteinen otsikko, joka on käännetty 90 asteen kulmassa
                                                     size = 15, rot = 90))      # y akselin suuntaiseksi, koko 15
figure2 <- annotate_figure(figure2, left = text_grob("Percentage within group",
                                                     size = 15, rot = 90))

# Yhdistetään osakuvat ja legendit väliin

figure3 <- plot_grid( figure1, legend, figure2, legend,
                      nrow = 4, rel_heights = c(1.1, 0.25, 1.1, 0.25)) # kuvat 4 riviin ja asetetaan kunkin rivin korkeudet
# Valmis kuva

figure3

############################## 6 AIKAPYLVÄSKUVA ############################

# Piirretään ajassa kehittyvä boxplot kuva

A <- ggplot(Data, aes(x = aika, y = EWL_pros_lahtopaino)) + # Data ja x ja y akseli
        theme( panel.background = element_rect(fill = "white", colour = "black", # Kuvan tausta valkoinen ja ääriviivat mustat
                                               size = 0.5, linetype = "solid"), # viivojen koko ja tyyppi
               panel.grid.major.y = element_line( size=0.25, color="grey" ), # kuvan taustalla olevat y akselin harmaat viivat
               legend.position = "none", # ei legendiä
               axis.text=element_text(size=12), # akselien tekstien koko
               axis.title=element_text(size=14)) +  # akselien otsikoiden koko
        stat_boxplot(aes(fill = leikkaus), # tehdään viikset ja ryhmitellään ne leikkauksien mukaan (fill)
                     geom = 'errorbar', width = 0.4, # määritellään viikset ja niiden etäisyys toisistaan
                     position = "dodge") + # ryhmien viikset vierekkäin
        geom_boxplot(aes(fill = leikkaus), # tehdään palkit viiksiin, ryhmitellään ne leikkauksen mukaan
                     width = 0.4, # palkkien etäisyys toisistaan
                     position = "dodge", # ryhmien palkit vierekkäin
                     fatten = 2) + # palkkien paksuus
        scale_fill_manual(values=c("firebrick", "dodgerblue4")) + # palkkien täyttövärit
        labs( x = "Years", y = "% Excess Weight Loss") + # x ja y akselien otsikot
        scale_y_continuous(breaks = c(0,25,50,75,100,125)) + # y akselin luvut
        stat_summary(data = Data,  # viivat palkkien väliin
                     fun = median, geom = 'line', # yhdistetään mediaanit (median) viivalla
                     aes(group = leikkaus), size=0.7) + # erikseen ryhmille ja määritellään koko
        stat_summary(fun.data = n_fun, # lisätään n määrät kuvan alareunaan
                     geom = "text", hjust = 0.1, aes(group = leikkaus), # tekstin korkeus 0.1 ja erikseen ryhmille leikkaus
                     position = position_stack(vjust = 0.0000001), size=3) + # tekstit päällekkäin ja niiden kooksi 3
        scale_x_discrete(breaks = c(0,0.5,1,2,3,4,5,6,7)) + # x akselin lukujen määrittäminen
        annotate(geom="text", x=3.35, y=-10, # lisätään n määrille otsikot, määritellään kohta x ja y arvoilla
                 label="No. of LRYGB patients:", size = 3)+ # teksti ja tekstin koko
        annotate(geom="text", x=3.1, y=-25, label="No. of LSG patients:", size = 3) # sama kuin yllä

A 

############################## 7 REGRESSIOKUVA ############################

# Sirontakuvio, mihin on piirretty regressiosuora

B <- ggplot(Data7, aes(x=EWL_pros_lahtopaino, y=BAROS, # Asetetaan data, x ja y akseli
                       color=leikkaus, shape=leikkaus)) + # asetetaan väri ja pisteiden muoto olemaan eri ryhmille (leikkaus)
        theme( panel.background = element_rect(fill = "white", colour = "black", # kuvan tausta valkoinen ja ääriviivat mustat
                                               size = 0.5, linetype = "solid"), # ääriviivojen muoto ja väri
               panel.grid.major.y = element_line( size=0.25, color="grey" ), # kuvan taustan harmaat y viivat
               legend.position = "none", # ei legendiä
               axis.text=element_text(size=12), # akselien arvojen koko
               axis.title=element_text(size=14)) + # akselien otsikoiden fonttikoko
        geom_point()+ # sirontakuvion pisteet
        geom_smooth(method=lm) + # regressiosuora
        scale_shape_manual(values=c(16, 17))+ # valitaan pisteiden muoto
        scale_color_manual(values=c("dodgerblue4","firebrick")) + # valitaan viivojen ja pisteiden väri
        scale_x_continuous(breaks = c(0,25,50,75,100)) + # x akselin arvot
        labs( x = "% Excess Weight Loss", y = "Moorehead-Ardelt QoL total score") # x ja y akselin otsikot

B