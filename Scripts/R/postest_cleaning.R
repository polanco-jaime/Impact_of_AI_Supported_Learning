#### 1. Reading #### 
library(readr)
library(dplyr)
# posttest_piloto2 <- read_csv("~/Downloads/results-survey324716 (piloto).csv")

#### 1. Reading csv #### 
posttest <- read_csv("~/Downloads/results-survey324716 (34).csv")
##### 1.1 Flag duplicates ####
posttest <- flag_duplicates(posttest)
##### 1.2 Q1 group ####
 
posttest <- posttest %>%
  mutate(Q1 = case_when(
    grepl("Leerpad groep 1", trimws(G00Q16)) ~ "1",
    grepl("Leerpad groep 2", trimws(G00Q16)) ~ "2",
    grepl("Leerpad groep 3", trimws(G00Q16)) ~ "3",
    TRUE ~ G00Q16
  ))
table(posttest$Q1)
posttest = posttest[is.na(posttest$Q1)==F , ]

##### 1.2 Resolve duplicates ####
# posttest <- resolve_duplicates(posttest)

posttest <- posttest[posttest$id!=311, ]
# posttest[posttest$id==155, ]$G04Q24
posttest <- posttest[posttest$id!=315, ]
posttest <- posttest[posttest$id!=434, ] #duplicate with 323

##### 1.3 Droping no completed at all #### 
# posttest =posttest[is.na(posttest$`PT01[SQ005]`)==F,]
posttest =posttest[is.na(posttest$PTF11)==F,]
#### 2. Personal Data #### 
##### 2.1 full name  ####
posttest$G01Q01 = homogenize_name(posttest$G01Q01)

fix_names <-  function(posttest_name, pretest_name, tabla = posttest, column="G01Q01") {
  ifelse(tabla[[column]] ==posttest_name , pretest_name, tabla[[column]] )
}
###### 2.1.1 Fixing names ####
pretest$G01Q01 = ifelse(pretest$G01Q01=='GAÉTAN_DEVOS', 'GAETAN_DEVOS', pretest$G01Q01)
posttest$G01Q01 = fix_names(posttest_name = "INGE_STRYNCK",
                            pretest_name = "STRYNCK_INGE",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "CLEO",
                            pretest_name = "JIMENEZ_LOPEZ_CLÉO",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "VAN_HOEY_SAAR",
                            pretest_name = "VAN_HOEY_TESS",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "LORE_STUYTS",
                            pretest_name = "STUYTS_LORE",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "SOW_HAMADOU",
                            pretest_name = "HAMADOU_SOW",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "LOUISE_COPPENS",
                            pretest_name = "COPPENS_LOUISE",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "AXL_PROOST",
                            pretest_name = "PROOST_AXL",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "HERZET_SEPPE",
                            pretest_name = "SEPPE_HERZET",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "BAILEY_DE_BOCK",
                            pretest_name = "DE_BOCK_BAILEY",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "HAYDEN_ORENS",
                            pretest_name = "ORENS_HAYDEN",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "MULTANI_.ISHAAN",
                            pretest_name = "MULTANI_ISHAAN",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "MULTANI_,_ISHAAN",
                            pretest_name = "MULTANI_ISHAAN",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "LAURENS_LATET",
                            pretest_name = "LATET_LAURENS",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "SIM_CLICTEUR",
                            pretest_name = "CLICTEUR_SIM",
                            posttest, "G01Q01")

posttest$G01Q01 = fix_names(posttest_name = "D'HONDT_JULIE",
                            pretest_name = "JULIE_D'HONDT",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "OCHOA_CHENNZZO",
                            pretest_name = "CHENNZZO_OCHOA",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "JASPER_NACHTERGAELE",
                            pretest_name = "NACHTERGAELE_JASPER",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "LARS_VAN_DER_LOOVEN",
                            pretest_name = "VAN_DER_LOOVEN_LARS",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "GOOSSENS_FABIAN_ANTONIO",
                            pretest_name = "GOOSSENS_FABIAN_ANTONIP",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "DAN_RAMAEKERS",
                            pretest_name = "MIKE_RAMAEKERS",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "BIELAWSKA_NIKOLA",
                            pretest_name = "NIKOLA_BIELAWSKA",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "FERRE_MERTENS",
                            pretest_name = "MERTENS_FERRE",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "MAARTEN_RAIJMAEKERS",
                            pretest_name = "RAIJMAEKERS_MAARTEN",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "MILAN_VANSUYPEENE",
                            pretest_name = "VANSUYPEENE_MILAN",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "NOAH_VAN_BOVEN",
                            pretest_name = "VAN_BOVEN_NOAH",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "DECLOEDT_BENNE",
                            pretest_name = "BENNE_DECLOEDT",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "JAKUB_PIASECKI",
                            pretest_name = "PIASECKI_JAKUB",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "VAN_BROEKHOVEN_CIS",
                            pretest_name = "CIS_VAN_BROEKHOVEN",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "QYLIAM_DE_SOMVIELE",
                            pretest_name = "DE_SOMVIELE_QYLIAM",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "BOONAERT_ISA",
                            pretest_name = "ISA_BOONAERT",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "AGOBIAN_MEGHRI",
                            pretest_name = "MEGHRI_AGOBIAN",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "ANGELIQUE_DEPRE",
                            pretest_name = "DEPRE_ANGELIQUE",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "MARIT_DE_CLERCK",
                            pretest_name = "DE_CLERCK_MARIT",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "ROBIN_DEVRIESE",
                            pretest_name = "DEVRIESE_ROBIN",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "LÉON_DELBEKE",
                            pretest_name = "DELBEKE_LÉON",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "STAN_DYLGAT",
                            pretest_name = "DYLGAT_STAN",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "PEPIJN_RAVELINGIEN",
                            pretest_name = "RAVELINGIEN_PEPIJN",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "NORE_VER_EECKE",
                            pretest_name = "NORE",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "VERBEKE_CHARLOTTE",
                            pretest_name = "CHARLOTTE_VERBEKE",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "HIMPE_MÉLISSE",
                            pretest_name = "MÉLISSE_HIMPE",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "EECKHOUT_AXELLE",
                            pretest_name = "AXELLE_EECKHOUT",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "DAAN_BOGEMANS",
                            pretest_name = "BOGEMANS_DAAN",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "PATTYN_JOPPE",
                            pretest_name = "JOPPE_PATTYN",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "JULIETTE_NUYTTENS",
                            pretest_name = "NUYTTENS_JULIETTE",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "DIEUWKE_SEGAERT",
                            pretest_name = "SEGAERT_DIEUWKE",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "THIAS_VAN_DEN_BOSSCHE",
                            pretest_name = "VAN_DEN_BOSSCHE_THIAS",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "JACK_DE_SCHUTTER_EN_MATTHIAS_LUTGEN",
                            pretest_name = "MATTHIAS_LUTGEN",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "BACHER_MEREL",
                            pretest_name = "MEREL_BACHER",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "LANDER_DE_VULDER",
                            pretest_name = "DE_VULDER_LANDER",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "BASTIAANSEN_ALEXANDER",
                            pretest_name = "ALEXANDER_BASTIAANSEN",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "NIELS_PEELMAN",
                            pretest_name = "PEELMAN_NIELS",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "CHLOË_DUBIN"  ,
                             pretest_name = "DUBIN_CHLOË",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "DE_VOS_TIMO",
                            pretest_name = "TIMO_DE_VOS",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "JOSSE_ZEGERS",
                            pretest_name = "ZEGERS_JOSSE",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "VAN_OSSELAER_LUCA"  ,
                             pretest_name = "LUCA_VAN_OSSELAER",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "WALRAVEYITSKE"  ,
                             pretest_name = "WALRAVE_YITSKE",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "NENA_MOENS",
                             pretest_name = "MOENS_NENA",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "NATHAN_COBBAERT"  ,
                             pretest_name = "COBBAERT_MEULENIJZER_NATHAN",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "WALRAVENS_SOFIA"  ,
                             pretest_name = "SOFIA_WALRAVENS",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "KOBE_DW"  ,
                             pretest_name = "KOBE_D'HONDT_WILLEKENS",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "RAYEN_SONCK"  ,
                             pretest_name = "RYAN_SONCK",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "SPOGMEI_MANGAL"  ,
                             pretest_name = "MANGAL_SPOGMEI",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "JESSE_HERZEEL"  ,
                             pretest_name = "JESSE.HERZEEL",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "JONAS_MOENS"  ,
                             pretest_name = "MOENS_JONAS",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "HANNES_NIELS"  ,
                             pretest_name = "NIELS_HANNES",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "MARCO_IBRAHIM"  ,
                             pretest_name = "IBRAHIM_MARCO",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "LAURENCE_VAN_GANSEN"  ,
                             pretest_name = "VAN_GANSEN_LAURENCE",
                             posttest, "G01Q01")

posttest$G01Q01 = fix_names( posttest_name = "NATHALIE_LUYTEN"  ,
                             pretest_name = "LUYTEN_NATHALIE",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "NICOLAS_BLEUX"  ,
                             pretest_name = "BLEUX_NICOLAS",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "WARRE_VANHESSCHE"  ,
                             pretest_name = "VANHESSCHE_WARRE",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "IAN_KIMPE"  ,
                             pretest_name = "KIMPE_IAN",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "ARNAUD_VANASSCHE"  ,
                             pretest_name = "VANASSCHE_ARNAUD",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "ZEYNEP_KOC"  ,
                             pretest_name = "KOC_ZEYNEP",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "MESTDAGH_JALINA"  ,
                             pretest_name = "JALINA_MESTDAGH",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "SYLVAIN_DEMEULENAERE"  ,
                             pretest_name = "DEMEULENAERE_SYLVAIN",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "ELLA_VANNIEUWKERKE"  ,
                             pretest_name = "VANNIEUWKERKE_ELLA",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "KOBE_PEEL"  ,
                             pretest_name = "PEEL_KOBE",
                             posttest, "G01Q01")

posttest$G01Q01 = fix_names( posttest_name = "ELINE_DE_WINTER"  ,
                             pretest_name = "DE_WINTER_ELINE",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "JORRE_UYTTENHOVE"  ,
                             pretest_name = "UYTTENHOVE_JORRE",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "VAN_DEN_DRIESSCHE"  ,
                             pretest_name = "VAN_DEN_DRIESSCHE_BENITO",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "RHUNE_WYDOOGHE"  ,
                             pretest_name = "WYDOOGHE_RHUNE",
                             posttest, "G01Q01")

posttest$G01Q01 = fix_names( posttest_name = "JUAN_DU_RENG"  ,
                             pretest_name = "JUAN_DU_RANG",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "ALEXIAN_DE_VOCHT"  ,
                             pretest_name = "DE_VOCHT_ALEXIAN",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "REYNDERS_GLORIA"  ,
                             pretest_name = "GLORIA_REYNDERS",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "REYNDERS_GLORIA"  ,
                             pretest_name = "GLORIA_REYNDERS",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "FEMKE_VANDECRUYS"  ,
                             pretest_name = "FEMKE_VDC",
                             posttest, "G01Q01")

posttest$G01Q01 = fix_names( posttest_name = "MICHAEL_ASENCIO"  ,
                             pretest_name = "MICHAEL_GABRIEL_ASENCIO_HIDALGO",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "LEONIE_VAN_OVERMEIRE"  ,
                             pretest_name = "VAN_OVERMEIRE_LEONIE",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "DEKKERS_AMELIE"  ,
                             pretest_name = "AMELIE_DEKKERS",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "OLIVIA_MAES"  ,
                             pretest_name = "MAES_OLIVIA",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "KIARA_BAUWERAERTS"  ,
                             pretest_name = "KIARA_LY",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "SALMA"  ,
                             pretest_name = "ENNIYA_SALMA",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "LUYTEN_VINCENT"  ,
                             pretest_name = "VINCENT_LUYTEN",
                             posttest, "G01Q01")
# posttest$G01Q01 = fix_names( posttest_name = "GAÉTAN_DEVOS"  ,
#                              pretest_name = "GAETAN_DEVOS",
#                              posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "LAVAERT_JARNE"  ,
                             pretest_name = "JARNE_LAVAERT",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "SETH_VAN_LOON"  ,
                             pretest_name = "VAN_LOON_SETH",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "MATTHIJS_TIBO"  ,
                             pretest_name = "TIBO_MATTHIJS",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "BOUNOUCHE_ANIS"  ,
                             pretest_name = "ANIS_BOUNOUCHE",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "BAS_MAGRIET"  ,
                             pretest_name = "MAGRIET_BAS",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names("REMIJSEN_HILDE" , 
                            "HILDE_REMIJSEN",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "MANEL_OUHARROU"  ,
                             pretest_name = "OUHARROU_MANEL",
                             posttest, "G01Q01")

posttest$G01Q01 = fix_names( posttest_name = "JASPER_DECLERCK"  ,
                             pretest_name = "DECLERCK_JASPER",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( "BOEVE_NORE"  , "NORE_BOEVE", posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "OSAKPAMWAN"  ,
                             pretest_name = "OSAKPAMWAN_AGHO",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "MARIA_CLARA_MUSMECI"  ,
                             pretest_name = "MUSMECI_MARIA_CLARA",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "INDY_HUBLÉ,_CERIEL_VLIEGHE"  ,
                             pretest_name = "INDY_HUBLÉ",
                             posttest, "G01Q01")

posttest$G01Q01 = fix_names( posttest_name = "ALEC_VAN_HERCK"  ,
                             pretest_name = "VAN_HERCK_ALEC",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "CEYHUN_TAHA_SOGUTLU"  ,
                             pretest_name = "SOGUTLU_CEYHUN_TAHA",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "AKRAM_ISKHAKOV"  ,
                             pretest_name = "AKRAM",
                             posttest, "G01Q01")

posttest$G01Q01 = fix_names( posttest_name = "ALAYA_VAN_RUYSKENSVELDE"  ,
                             pretest_name = "VAN_RUYSKENSVELDE_ALAYA",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "LORE_VAN_DRIESSCHE"  ,
                             pretest_name = "VAN_DRIESSCHE_LORE",
                             posttest, "G01Q01")


posttest$G01Q01 = fix_names( posttest_name = "JAMIE_VANRYCKEGHEM"  ,
                             pretest_name = "VANRYCKEGHEM_JAMIE",
                             posttest, "G01Q01")

posttest$G01Q01 = fix_names( posttest_name = "NINA_DESCHAMP"  ,
                             pretest_name = "DESCHAMP_NINA",
                             posttest, "G01Q01")

posttest$G01Q01 = fix_names( posttest_name = "VAN_DE_WALLE_AUGUST"  ,
                             pretest_name = "AUGUST_VAN_DE_WALLE",
                             posttest, "G01Q01")

posttest$G01Q01 = fix_names( posttest_name = "JULIE_VANRAES"  ,
                             pretest_name = "VANRAES_JULIE",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "CÉLESTINE_LANGENBERG"  ,
                             pretest_name = "LANGENBERG_CÉLESTINE",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "SEPPE_HOOYBERGHS"  ,
                             pretest_name = "HOOYBERGHS_SEPPE",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "CRABBÉ_ZANA"  ,
                             pretest_name = "ZANA_CRABBÉ",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "GESQUIERE_FLEUR"  ,
                             pretest_name = "GESQUIÈRE_FLEUR",
                             posttest, "G01Q01")

posttest$G01Q01 = fix_names( posttest_name = "GAUTHIER_SABBE"  ,
                             pretest_name = "GAUTHIER",
                             posttest, "G01Q01")

posttest$G01Q01 = fix_names( posttest_name = "EVA_FIERENS"  ,
                             pretest_name = "FIERENS_EVA",
                             posttest, "G01Q01")

posttest$G01Q01 = fix_names( posttest_name = "AXANA_DUTILLIE"  ,
                             pretest_name = "DUTILLIE_AXANA",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "ARTHUR_SIMOENS"  ,
                             pretest_name = "SIMOENS_ARTHUR",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "ELENA_GALLE"  ,
                             pretest_name = "GALLE_ELENA",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "AMINE_AJROUD"  ,
                             pretest_name = "AJROUD_AMINE",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "PLATTEEUW_MATHIS"  ,
                             pretest_name = "MATHIS_PLATTEEUW",
                             posttest, "G01Q01")

posttest$G01Q01 = fix_names( posttest_name = "IBE_CATELIN"  ,
                             pretest_name = "CATELIN_IBE",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "SAMUEL"  ,
                             pretest_name = "SAMUEL_D'HAENENS",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "JEF_VAN_LINT"  ,
                             pretest_name = "VAN_LINT_JEF",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "ELIZA_EECKHOUT"  ,
                             pretest_name = "EECKHOUT_ELIZA",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "XANDER_VAN_DE_VELDE"  ,
                             pretest_name = "VAN_DE_VELDE_XANDER",
                             posttest, "G01Q01")

posttest$G01Q01 = fix_names( posttest_name = "STEF_BOULONNE"  ,
                             pretest_name = "BOULONNE_STEF",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "VAN_DE_VENSTER_FRAUKE"  ,
                             pretest_name = "FRAUKE_VAN_DE_VENSTER",
                             posttest, "G01Q01")

posttest$G01Q01 = fix_names( posttest_name = "JANA_VAN_DE_CAPPELLE"  ,
                             pretest_name = "VAN_DE_CAPPELLE_JANA",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names( posttest_name = "VAN_ESSCHE"  ,
                             pretest_name = "LEON_VAN_ESSCHE",
                             posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "IEBEN_COENE",
                            pretest_name = "COENE_IEBEN",
                            posttest, "G01Q01")
posttest$G01Q01 = fix_names(posttest_name = "MYKOLAI_IVANENKO",
                            pretest_name = "IVANENKO_MYKOLAI",
                            posttest, "G01Q01")
###### 2.1.1 Fixing treatment group ####
posttest$AT=F
posttest$Q1 = ifelse(posttest$G01Q01=="BOUCKAERT_LARA",'2',posttest$Q1   )
posttest$Q1 = ifelse(posttest$G01Q01=="POLLENTIER_RODÉRIC",'1',posttest$Q1   )
posttest$Q1 = ifelse(posttest$G01Q01=="MATHIS_PLATTEEUW",'2',posttest$Q1   )
posttest$Q1 = ifelse(posttest$G01Q01=="MELLEBEEK_NOOR",'3',posttest$Q1   )
posttest$Q1 = ifelse(posttest$G01Q01=="JARNE_VAN_DOREN",'3',posttest$Q1   )
posttest$Q1 = ifelse(posttest$G01Q01=="HIGGS_ROXY",'2',posttest$Q1   )
posttest$Q1 = ifelse(posttest$G01Q01=="EMMA_DE_GROOTE",'2',posttest$Q1   )
posttest$Q1 = ifelse(posttest$G01Q01=="CHEYENNE_VAN_DAELE",'2',posttest$Q1   )
posttest$Q1 = ifelse(posttest$G01Q01=="HALIL_YURUK",'2',posttest$Q1   )
posttest$Q1 = ifelse(posttest$G01Q01=="KORKMAZ_BERAT",'2',posttest$Q1   )
posttest$Q1 = ifelse(posttest$G01Q01=="ENGELEN_MIEL",'3',posttest$Q1   )
posttest$Q1 = ifelse(posttest$G01Q01=="ASTRID_EÜLER",'2',posttest$Q1   )
posttest$Q1 = ifelse(posttest$G01Q01=="JARNE_LAVAERT",'3',posttest$Q1   )
posttest$Q1 = ifelse(posttest$G01Q01=="GLORIA_REYNDERS",'2',posttest$Q1   )
posttest$Q1 = ifelse(posttest$G01Q01=="BRUGGEMAN_RÉMI",'3',posttest$Q1   )
posttest$Q1 = ifelse(posttest$G01Q01=="BRAM_VAN_MOSSEVELDE",'2',posttest$Q1   )
posttest$Q1 = ifelse(posttest$G01Q01=="KOC_ZEYNEP",'3',posttest$Q1   )
posttest$Q1 = ifelse(posttest$G01Q01=="EMILE_CORNELIS",'3',posttest$Q1   )
posttest$Q1 = ifelse(posttest$G01Q01=="LOUIS_VANHERCK",'2',posttest$Q1   )
posttest$Q1 = ifelse(posttest$G01Q01=="BLEUX_NICOLAS",'3',posttest$Q1   )
posttest$Q1 = ifelse(posttest$G01Q01=="THORBEN_VETS",'1',posttest$Q1   )
posttest$Q1 = ifelse(posttest$G01Q01=="SIMANY_BRYAN",'2',posttest$Q1   )
posttest$Q1 = ifelse(posttest$G01Q01=="DE_VALCK_SUNE",'3',posttest$Q1   )
posttest$Q1 = ifelse(posttest$G01Q01=="SOFIA_WALRAVENS",'1',posttest$Q1   )
posttest$Q1 = ifelse(posttest$G01Q01=="LEWIS_COZYNS",'2',posttest$Q1   )
posttest$Q1 = ifelse(posttest$G01Q01=="VAN_DELSEN_CHARLOTTE",'2',posttest$Q1   )
posttest$Q1 = ifelse(posttest$G01Q01=="COBBAERT_MEULENIJZER_NATHAN",'1',posttest$Q1   )
posttest$Q1 = ifelse(posttest$G01Q01=="MOENS_NENA",'3',posttest$Q1)
posttest$Q1 = ifelse(posttest$G01Q01=="SIENNA_DE_CLERCK",'1',posttest$Q1)
posttest$Q1 = ifelse(posttest$G01Q01=="AMINE_EL_HAJJAMI",'1',posttest$Q1)
posttest$Q1 = ifelse(posttest$G01Q01=="HEREZ_INES",'3',posttest$Q1)
posttest$Q1 = ifelse(posttest$G01Q01=="VANDEVYVERE_LOWIE",'2',posttest$Q1)
posttest$Q1 = ifelse(posttest$G01Q01=="NUYTTENS_JULIETTE",'3',posttest$Q1)
posttest$Q1 = ifelse(posttest$G01Q01=="BÁLINT_RAKOSI",'2',posttest$Q1)
posttest$Q1 = ifelse(posttest$G01Q01=="MÉLISSE_HIMPE",'3',posttest$Q1)
posttest$Q1 = ifelse(posttest$G01Q01=="BUYSSE_LAURA",'2',posttest$Q1)
posttest$Q1 = ifelse(posttest$G01Q01=="CHARLOTTE_VERBEKE",'3',posttest$Q1)
posttest$Q1 = ifelse(posttest$G01Q01=="VERHAEGE_DAGMAR",'3',posttest$Q1)
posttest$Q1 = ifelse(posttest$G01Q01=="DYLGAT_STAN",'2',posttest$Q1)
posttest$Q1 = ifelse(posttest$G01Q01=="DEBOSSCHERE_LOWIE",'2',posttest$Q1)
posttest$Q1 = ifelse(posttest$G01Q01=="DEVRIESE_ROBIN",'2',posttest$Q1)
posttest$Q1 = ifelse(posttest$G01Q01=="KINNA_BASTIAAN",'2',posttest$Q1)
posttest$Q1 = ifelse(posttest$G01Q01=="LOÏC_NGOMBE",'2',posttest$Q1)
posttest$Q1 = ifelse(posttest$G01Q01=="ELIOT_DALDINI",'2',posttest$Q1)
posttest$Q1 = ifelse(posttest$G01Q01=="BUGGENHOUT_LISA",'2',posttest$Q1)
posttest$Q1 = ifelse(posttest$G01Q01=="KOBE_ROELS",'2',posttest$Q1)
posttest$Q1 = ifelse(posttest$G01Q01=="FLEUR_STRYBOL",'1',posttest$Q1)
posttest$Q1 = ifelse(posttest$G01Q01=="TIM_DRIESEN",'1',posttest$Q1)
posttest$Q1 = ifelse(posttest$G01Q01=="GILIS_OWEN",'1',posttest$Q1)
posttest$Q1 = ifelse(posttest$G01Q01=="VAN_HOEY_TESS",'3',posttest$Q1)

posttest$AT = ifelse(posttest$G01Q01=="VAN_HOEY_TESS",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="GILIS_OWEN",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="TIM_DRIESEN",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="FLEUR_STRYBOL",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="KOBE_ROELS",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="BUGGENHOUT_LISA",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="ELIOT_DALDINI",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="LOÏC_NGOMBE",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="KINNA_BASTIAAN",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="DEVRIESE_ROBIN",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="DEBOSSCHERE_LOWIE",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="DYLGAT_STAN",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="VERHAEGE_DAGMAR",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="CHARLOTTE_VERBEKE",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="BUYSSE_LAURA",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="MÉLISSE_HIMPE",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="BÁLINT_RAKOSI",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="NUYTTENS_JULIETTE",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="VANDEVYVERE_LOWIE",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="HEREZ_INES",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="AMINE_EL_HAJJAMI",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="SIENNA_DE_CLERCK",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="MOENS_NENA",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="COBBAERT_MEULENIJZER_NATHAN",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="VAN_DELSEN_CHARLOTTE",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="LEWIS_COZYNS",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="SOFIA_WALRAVENS",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="DE_VALCK_SUNE",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="SIMANY_BRYAN",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="THORBEN_VETS",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="BLEUX_NICOLAS",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="LOUIS_VANHERCK",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="EMILE_CORNELIS",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="KOC_ZEYNEP",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="BRAM_VAN_MOSSEVELDE",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="BRUGGEMAN_RÉMI",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="GLORIA_REYNDERS",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="JARNE_LAVAERT",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="ASTRID_EÜLER",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="ENGELEN_MIEL",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="BOUCKAERT_LARA",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="POLLENTIER_RODÉRIC",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="MATHIS_PLATTEEUW",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="MELLEBEEK_NOOR",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="JARNE_VAN_DOREN",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="HIGGS_ROXY",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="EMMA_DE_GROOTE",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="CHEYENNE_VAN_DAELE",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="HALIL_YURUK",T,posttest$AT   )
posttest$AT = ifelse(posttest$G01Q01=="KORKMAZ_BERAT",T,posttest$AT   )

# Cheking linked name
# filter_and_select(posttest,  "MANON")
# filter_and_select(posttest_only,  "MANON")
##### 2.2 Gender  ####
posttest$G01Q03 = translate_responses(posttest$G01Q03, translations)
table(posttest$G01Q03)
posttest$G01Q03 = ifelse(posttest$G01Q01=="COBBAERT_MEULENIJZER_NATHAN", 'Boy', posttest$G01Q03 )
posttest$G01Q03=ifelse(posttest$G01Q01=="BENITO_SOMERS",
                       'Boy',
                       posttest$G01Q03)

posttest$G01Q03=ifelse(posttest$G01Q01=="VAN_DER_LOOVEN_LARS",
                       'Boy',
                       posttest$G01Q03)
##### 2.3 City  ####
posttest$G01Q04 <- homogenize_cities(posttest$G01Q04)
##### 2.4 School Name  ####
posttest$G01Q05 <- homogenize_school_name(posttest$G01Q05)

###### 2.4.1 Fixing school name ####
posttest$G01Q05 = ifelse(posttest$G01Q01=="VAN_ES_MILAN",
                         'Sintnorbertusinstituut Snor',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="JIMENEZ_LOPEZ_CLÉO",
                         'Moretus',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="ANAS_KARAMZIANI",
                         'Moretus Ekeren',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="HAMADOU_SOW",
                         'Moretus Ekeren',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="VAN_HOEY_TESS",
                         'Leiepoort Sinthendrik',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="STUYTS_LORE",
                         'Moretus Ekeren',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="DE_BOCK_BAILEY",
                         'Broederschool Handel',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="IMRAN_SOUSSI",
                         'Broederschool',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="STRUYF_MIEL",
                         'Het Kompas',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="VLEMINCKX_NIO",
                         'Technisch Atheneum Brasschaat',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="VAN_GASSE_MICHIEL",
                         'Broederschool',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="MEERSMAN_SENNE",
                         'Broederschool',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="ORENS_HAYDEN",
                         'Moretus',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="AMIEN_JANSSENS",
                         'Sintnorbertus',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="VAN_DER_LOOVEN_LARS",
                         'Leiepoort Campus Sinthendrik',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="MALKIC_DAMIR",
                         'Moretusekeren',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="EMMA_IVENS",
                         'Virgo Sapiens',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="CLICTEUR_SIM",
                         'TA Brasschaat',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="NACHTERGAELE_JASPER",
                         'Leiepoort Campus Sinthendrik',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="JANNES_VAN_DEN_BRANDE",
                         'Virgosapiens Secundair',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="JACOBS_LIAM",
                         'Campus Hast',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="MAHIEU_JANI",
                         'Atheneum Courtmanslaan',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="SPILLEBEEN_NATHAN",
                         'Atheneum Courtmanslaan',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="YORBEN_VAN_HAECHT",
                         'Moretus Ekeren',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="KOBE_ROELS",
                         'Moretusekeren',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="MERTENS_FERRE",
                         'Moretus Ekeren',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="ELIOT_DALDINI",
                         'Kobos College',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="FLEUR_BLUEKENS",
                         'Heilig Graf',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="VAN_LOOVEREN_LOTTE",
                         'Heilig Graf',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="VANSUYPEENE_MILAN",
                         'Sint-Paulusinstituut',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="DE_SAEDELEER_SIMON",
                         'Sint-Paulusinstituut Herzele',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="PARMENTIER_MARIE",
                         'Sint-Paulusinstituut',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="BENNE_DECLOEDT",
                         'Sintpaulus Instituut',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="DEPRE_ANGELIQUE",
                         'Moretus',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="CARREIN_BRITT",
                         'Gotechnisch Atheneum',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="DE_CLERCK_MARIT",
                         'Technisch Atheneum Brasschaat',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="LORE_PEDE",
                         'Sintpaulus Instituut Herzele',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="DEVRIESE_ROBIN",
                         'Spes Nostra',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="KINNA_BASTIAAN",
                         'Spes Nostra Kuurne',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="DELBEKE_LÉON",
                         'Spes Nostra Kuurne',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="DEBOSSCHERE_LOWIE",
                         'Spes Nostra Kuurne',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="HUYS_THIBAU",
                         'Spes Nostra Kuurne',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="NORE",
                         'Spes Nostra',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="VAN_DAMME_ARNO",
                         'Spes Nostra',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="RAVELINGIEN_PEPIJN",
                         'Spes Nostra',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="RONSSE_MATHIEU",
                         'Spes Nostra Kuurne',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="BRUYNSEELS_NORA",
                         'Sint Clara College',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="DE_MEYER_IBEN",
                         'Sintpaulus Instituut',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="MARIAM_REVA",
                         'Heilige Familie Ieper',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="BOGEMANS_DAAN",
                         'Sint-Paulusinstituut Herzele',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="VINCENT_STERCKX",
                         'Sintclara College',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="BRUYNSEELS_NORA",
                         'Sint Clara College',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="MEREL_BACHER",
                         'Sintpaulus Instituut',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="TUUR_VERDONCK",
                         'Sintjanbergmanscollege',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="BOGAERTS_JADA",
                         'Sintjan Berchmanscollege',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="ZEGERS_JOSSE",
                         'Sintjan Berchmans College',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="STRYNCK_INGE",
                         'Leiepoort campus Sint Hendrik',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="EMILE_CORNELIS",
                         'Sintmaria',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="ANISSA_LAHRACH",
                         'Sintnorbertus Instituut',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="ALEXANDER_BASTIAANSEN",
                         'Sintjan Berchmanscollege Malle',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="COP_ARTHUR",
                         'Sint Jan Berchmanscollege',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="BENITO_SOMERS",
                         'Sint Jan Berghmanscollege',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="VANHOLDERBEKE_MAURO",
                         'Heilige Familie Ieper',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="SOMERS_FELIPE",
                         'Sint Jan Berchmanscollege',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="HEREZ_INES",
                         'Heilig Hart College',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="HAYLEY_BACHOT",
                         'Go Technisch Atheneum Brasschaat',
                         posttest$G01Q05 )
posttest$G01Q05 = ifelse(posttest$G01Q01=="LOWIE_STEPPE",
                         'Sintpaulus',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="YENTL_VAN_WIJNENDAELE",
                         'Sint-Paulusinstituut',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="VAN_DE_PONTSELE_JONAS",
                         'Sintpaulus',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="VAN_DER_BIEST_INE",
                         'Sintpaulus Instituut Herzele',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="BOGAERT_MATTHIAS",
                         'Kavoskenslaan Topsport',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="COBBAERT_MEULENIJZER_NATHAN",
                         'Sintpaulus Instituut',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="KOBE_D'HONDT_WILLEKENS",
                         'Topsport Voskenslaan',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="VERNAILLEN_STAN",
                         'Ka Athenuem Voskenslaan',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="KIMPE_IAN",
                         'Topsport Voskenslaan',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="VAN_DEN_HEEDE_BENTHE",
                         'Topsportschool Gent Voskenslaan',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="DAAN_VAN_LAERE",
                         'Ka Voskenslaan Topsport',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="CAUA_FORONI_DE_OLIVEIRA",
                         'Topsport Voskenslaan',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="ARTHUR_DUBOIS",
                         'Kavoskenslaan Athemeum',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="DE_VALCK_SUNE",
                         'Topsport Voskenslaan',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="YARNE_EYLENBOSCH",
                         'Topsport Voskenslaan',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="VAN_GANSEN_LAURENCE",
                         'Sint Maria Geel',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="GOOSSENS_MATHIJS",
                         'Sint Maria Geel',
                         posttest$G01Q05   )

posttest$G01Q05 = ifelse(posttest$G01Q01=="DE_MUYNCK_TARA",
                         'Topsportschool Ka Voskenslaan',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="BLEUX_NICOLAS",
                         'Sintmaria Geel',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="SLAGHMUYLDER_ELISE",
                         'Heilig Hart College Halle',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="LUYTEN_NATHALIE",
                         'Sint Maria Geel',
                         posttest$G01Q05   )

posttest$G01Q05 = ifelse(posttest$G01Q01=="CASTRO_MURILLO_JUAN-PEDRO",
                         'Heilighart Halle',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="JESSICA_DERAYMAEKER",
                         'Heilig-Hartcollege',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="VANHESSCHE_WARRE",
                         'Vrij Handels- en Sportinstituut',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="YORBEN_PAUWELS",
                         'Sint-Norbertusinstituut',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="SAID_BEN_AJIBA_HALIMA",
                         'Helighart',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="CLOÉ_DEVOS",
                         'Heilig-Hartcollege',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="VANASSCHE_ARNAUD",
                         'Kavokenslaan',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="CORDIER_JUUL",
                         'Zavo',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="VANHAELEN_LIAM",
                         'Heilig Hart en College',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="BRAM_VAN_MOSSEVELDE",
                         'Topsportschool Voskenslaan',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="BELINDA_BOLEMBO",
                         'Heilig-Hart & College Halle',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="DE_WINTER_ELINE",
                         'Topsport Vos',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="BENALI_RAMZI",
                         'Topsport Gent',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="MATHILDE_FRANCOTTE",
                         'Topsport Voskenslaan',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="CLICTEUR_SIM",
                         'TA Brasschaat',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="VERMOTE_CARSTEN",
                         'Sintjozefscolllege',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="ARIANE_DEPROOST",
                         'Sint-Jozefscollege Torhout',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="VERHOUGSTRAETE_TILLE",
                         'Topsport Voskenslaan',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="YELENA_DEVREKER",
                         'Topsport Voskenslaan',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="TRAEN_SINNE",
                         'Vrij Handels- en Sportinstituut',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="DEGRYSE_HANNE",
                         'Topsport Voskenslaan',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="KANATLAR_KIYAN_CAN",
                         'Topsportschool Gent',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="BAERT_CHLOE",
                         'Topsport Voskenslaan',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="PERGOOT_RUNE",
                         'Topsport Voskenslaan',
                         posttest$G01Q05   )

posttest$G01Q05 = ifelse(posttest$G01Q01=="VAN_HERCK_ALEC",
                         'Campus Sint-Jan Berchmans Mol',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="MICHAEL_GABRIEL_ASENCIO_HIDALGO",
                         'Zavo',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="BASTIAENS_MIA",
                         'Sintdimpna College',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="VERACHTERT_NEL",
                         'Sint Dimpna',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="GROENEN_ASTRID",
                         'Sint Dimpna',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="MUSTAFA_ENES_YIGIT",
                         'Zavo Sterrebeek',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="MAES_OLIVIA",
                         'Sint Dimpna Geel College',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="KIARA_LY",
                         'Sint Dimpna College',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="VERBRUGGHE_JENS",
                         'Technisch Instituut Heilige Familie Ieper',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="DEVOS_GAÉTAN",
                         'Sint-Jozefscollege Torhout',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="VAN_LOON_SETH",
                         'Sintclaracollege',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="ASTRID_EÜLER",
                         'Onzelieve Vrouwen Presentatie',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="MATTHIJS_TIBO",
                         'Sintpaulus Instituut',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="MOULOUD_KAOUTAR",
                         'Scheppersinstituut',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="ENNIYA_SALMA",
                         'Go Spronk',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="MAGRIET_BAS",
                         'Edugolo',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="JAYDA_BOGAERS",
                         'Sintclaracollege',
                         posttest$G01Q05)
posttest$G01Q05 = ifelse(posttest$G01Q01=="VAN_VLAENDEREN_SENNE",
                         'Heilig Hart College Halle',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="CUYL_ELODIE",
                         'Heilighart College',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="QUAGHEBEUR_TABITHA",
                         'MSKA Roeselare',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="FRANCIS_ANAÏS",
                         'Sint-Norbertusinstituut',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="KYLIAN_ABDEL_MAWGOUD",
                         'Mska Campus Tant',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="ACHBARI_HAFSA",
                         'Sintagensinstituut',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="DELEU_YOUNA",
                         'MSKA Roeselare',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="VAN_HERCK_ALEC",
                         'Campus Sint-Jan Berchmans Mol',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="RANA_CHARKI",
                         'Heilig Hartcollege Halle',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="MUSMECI_MARIA_CLARA",
                         'Mska Campus Tant',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="KERSTENS_MAXIM",
                         'Sint Jan Berchmanscollege',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="CRISTIAN_FRANTESCU",
                         'Campus Sint-Jan Berchmans Mol',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="ANNELIEN_GEBOERS",
                         'Sint Dimpna College Geel',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="JARNE_LOYEN",
                         'Sint Franciscuscollege',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="VANDERHEYDEN_SENNE",
                         'Sintmaria Geel',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="MILAN_VERMEERSCH",
                         'Sint-Jozefscollege Torhout',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="KERSTENS_ALEXANDER",
                         'Campus Sintjanberchmanscollege',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="VAN_RUYSKENSVELDE_ALAYA",
                         'Campus De Toren Edugo Lochristi',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="STRYNCK_INGE",
                         'Leiepoort Campus Sinthendrik',
                         posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="SEDRANI_QUINTO",
                         'Sintjan Berchmanscollege',
                         posttest$G01Q05   )

posttest$G01Q05 = ifelse(posttest$G01Q01=="VERHEYEN_FRÉ",
                         'Campus Sint-Jan Berchmans',
                         posttest$G01Q05   )

posttest$G01Q05 = ifelse(posttest$G01Q01=="HALIL_YURUK",
                         'Olvp Secundair Onderwijs',posttest$G01Q05   )

posttest$G01Q05 = ifelse(posttest$G01Q01=="CHEYENNE_VAN_DAELE",
                         'Onzelievevrouw Presentatie',posttest$G01Q05   )

posttest$G01Q05 = ifelse(posttest$G01Q01=="HAKAN_KURT",
                         'Olvpsint Niklaas',posttest$G01Q05   )

posttest$G01Q05 = ifelse(posttest$G01Q01=="LUKAS_HAECK",
                         'Sint Barbara',posttest$G01Q05   )

posttest$G01Q05 = ifelse(posttest$G01Q01=="CANSIER_FINN",
                         'Campus Sintjan Berchmanscollege Mol',posttest$G01Q05   ) 
posttest$G01Q05 = ifelse(posttest$G01Q01=="DELEU_YOUNA",
                         'Mska',
                         posttest$G01Q05   )

posttest$G01Q05 = ifelse(posttest$G01Q01=="DERMUL_EMMA",'Sintjozefs College Torhout',posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="ISENBAERT_MARIE",'Sint-Jozefscollege Torhout',posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="JUTTA_VAN_EENOO",'Sint-Jozefscollege Torhout',posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$id==46,'Sint-Rembert College Torhout',posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="VANPARYS_GABRIËL",'Sint-Jozefscollege Torhout',posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="AJROUD_AMINE",'Sint Jozefinstituut Bokrijk',posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="VANDER_CRUYS_MILAN",'Atheneum Halle',posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="DUTILLIE_AXANA",'Sint-Pauluscollege',posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="HOOYBERGHS_SEPPE",'Ksom Campus Sintjan Berchmans',posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="SPATHARAKIS_ELENI",'Sint Jozefinstituut Bokrijki',posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="ABDELLAH_KHATABI",'Atheneum Halle',posttest$G01Q05   )
posttest$G01Q05 = ifelse(posttest$G01Q01=="DUTILLIE_AXANA",
                         'Sint-Pauluscollege',posttest$G01Q05   ) 

posttest$G01Q05 = ifelse(posttest$G01Q01=="SAMUEL_D'HAENENS",
                         'Sintbarbara College',posttest$G01Q05   ) 

posttest$G01Q05 = ifelse(posttest$G01Q01=="JÉRÔME_VAN_EYGEN",
                         'Sint-Rembert College Torhout',posttest$G01Q05   )  
posttest$G01Q05 = ifelse(posttest$G01Q01=="CATELIN_IBE",
                         'Sint Pauluscollege Wevelgem',posttest$G01Q05   ) 
posttest$G01Q05 = ifelse(posttest$G01Q01=="DUTILLIE_AXANA",
                         'Sint-Pauluscollege',posttest$G01Q05   ) 

posttest$G01Q05 = ifelse(posttest$G01Q01=="VAN_DE_VELDE_XANDER",
                         'Sintbarbara College',posttest$G01Q05   ) 
posttest$G01Q05 = ifelse(posttest$G01Q01=="VINCENT_THIRIART",
                         'Heilig-Hartcollege',posttest$G01Q05   ) 

posttest$G01Q05 = ifelse(posttest$G01Q01=="HENRY_BERQUIN",
                         'Sint-Barbaracollege',posttest$G01Q05   )

##### 2.5 Course of Student  ####
posttest$G01Q06 <- homogenize_course_group(posttest$G01Q06)
##### 2.6 Type of School  ####
posttest$G01Q07 = translate_responses(posttest$G01Q07, translations)


posttest$G00Q16 <- translate_responses(posttest$G00Q16, translations)
##### 2.7 Where did you take the digital class? ####
posttest$G01Q08 <- translate_responses(posttest$G01Q08, translations)
table(posttest$G01Q08 )

#### 6. Cleanning posttest #### 


##### 6.1 Assesses understanding of percentages in the context of a tax rate. ####

posttest$PT03 = ifelse(posttest$PT03=='25%'| posttest$PT03== 'AO01', 1, 0)
posttest$PT03 = as.numeric(posttest$PT03)
table(posttest$PT03 )

##### 6.2 Knowledge Score ####
## Are there difference between control assigment and treatment assigment at baseline?
  
posttest$PTF01 =ifelse(posttest$PTF01=='243'| posttest$PTF01== 'AO01', 1, 0)
table(posttest$PTF01)
posttest$PTF01 = as.numeric(posttest$PTF01)

posttest$PTF02 = translate_responses(posttest$PTF02, translations)
table(posttest$PTF02)
posttest$PTF02 =ifelse(posttest$PTF02=="Progressive tax system"| posttest$PTF02== 'AO03', 1, 0)
posttest$PTF02 = as.numeric(posttest$PTF02)

table(posttest$PTF06)
22000*0.25+16000*0.40
posttest$PTF06 =ifelse(posttest$PTF06=="€11900"| posttest$PTF06== 'AO03', 1, 0)
posttest$PTF06 = as.numeric(posttest$PTF06)


table(posttest$PTF07)
posttest$PTF07=ifelse(posttest$PTF07=="€32640"| posttest$PTF07== 'AO01', 1, 0)
posttest$PTF07 = as.numeric(posttest$PTF07)

table(posttest$PTF09)
posttest$PTF09 = translate_responses(posttest$PTF09, translations)
posttest$PTF09=ifelse(posttest$PTF09=="Het gemiddelde nationale loon" | posttest$PTF09== 'AO03', 1, 0)
posttest$PTF09 = as.numeric(posttest$PTF09)

table(posttest$PTF10)
posttest$PTF10=ifelse(posttest$PTF10=="37%"| posttest$PTF10== 'AO01', 1, 0)
posttest$PTF10 = as.numeric(posttest$PTF10)

table(posttest$PTF11)

posttest$PTF11=ifelse(grepl(pattern = "degressief systeem", posttest$PTF11), 1, 0)
posttest$PTF11 = as.numeric(posttest$PTF11)


posttest$Score = (posttest$PTF01+posttest$PTF02+posttest$PTF06+posttest$PTF07+posttest$PTF09+posttest$PTF10+posttest$PTF11)/7
 


 
posttest$Treatment_state_i = ifelse(posttest$Q1=='1', 0, 1)
 

##### 6.3 Measure Emotional Attitudes ####
###### 6.3.1 Attitude and Motivation ####

posttest$`AS01[SQ001]` = as.numeric(translate_responses(posttest$`AS01[SQ001]`, translations_ls) )
posttest$`AS01[SQ002]` = as.numeric(translate_responses(posttest$`AS01[SQ002]`, translations_ls) )
posttest$`AS01[SQ003]` = as.numeric(translate_responses(posttest$`AS01[SQ003]`, translations_ls) )
posttest$`AS01[SQ004]` = as.numeric(translate_responses(posttest$`AS01[SQ004]`, translations_ls) )
 
posttest$`Attitude and Motivation` =  (posttest$`AS01[SQ001]` + posttest$`AS01[SQ002]`+   
                                  posttest$`AS01[SQ003]`+ posttest$`AS01[SQ004]`  )/4

hist(posttest$`Attitude and Motivation`)

###### 6.3.2 Learning Experience & User Experience ####
table(posttest$`AS02[SQ006]` )
posttest$`AS02[SQ001]` = as.numeric(translate_responses(posttest$`AS02[SQ001]`, translations_ls) )
posttest$`AS02[SQ002]` = as.numeric(translate_responses(posttest$`AS02[SQ002]`, translations_ls) )
posttest$`AS02[SQ003]` = as.numeric(translate_responses(posttest$`AS02[SQ003]`, translations_ls) )
posttest$`AS02[SQ004]` = as.numeric(translate_responses(posttest$`AS02[SQ004]`, translations_ls) )
posttest$`AS02[SQ005]` = as.numeric(translate_responses(posttest$`AS02[SQ005]`, translations_ls) )
posttest$`AS02[SQ006]` = as.numeric(translate_responses(posttest$`AS02[SQ006]`, translations_ls) )
posttest$`Learning Experience & User Experience` =  (posttest$`AS02[SQ001]` + posttest$`AS02[SQ002]`+   
                                        posttest$`AS02[SQ003]` + posttest$`AS02[SQ004]`+
                                        posttest$`AS02[SQ005]` + posttest$`AS02[SQ006]`)/6

hist(posttest$`Learning Experience & User Experience`)
###### 6.3.3 Self-Regulation & Metacognition ####
table(posttest$`AS03[SQ001]` )
posttest$`AS03[SQ001]` = as.numeric(translate_responses(posttest$`AS03[SQ001]`, translations_ls) )
posttest$`AS03[SQ002]` = as.numeric(translate_responses(posttest$`AS03[SQ002]`, translations_ls) )
posttest$`AS03[SQ003]` = as.numeric(translate_responses(posttest$`AS03[SQ003]`, translations_ls) )
posttest$`Self-Regulation & Metacognition` =  (posttest$`AS03[SQ001]` + posttest$`AS03[SQ002]`+   posttest$`AS03[SQ003]`   )/3

hist(posttest$`Self-Regulation & Metacognition`)

###### 6.3.4 Engagement & Commitment ####
colnames(posttest)

posttest$`AS04[SQ001]` = as.numeric(translate_responses(posttest$`AS04[SQ001]`, translations_ls) )
posttest$`AS04[SQ002]` = as.numeric(translate_responses(posttest$`AS04[SQ002]`, translations_ls) )
posttest$`AS04[SQ003]` = as.numeric(translate_responses(posttest$`AS04[SQ003]`, translations_ls) )
posttest$`AS04[SQ004]` = as.numeric(translate_responses(posttest$`AS04[SQ004]`, translations_ls) )
posttest$`AS04[SQ005]` = as.numeric(translate_responses(posttest$`AS04[SQ005]`, translations_ls) )
posttest$`Engagement & Commitment` =  (posttest$`AS04[SQ001]` + posttest$`AS04[SQ002]`+   
                                        posttest$`AS04[SQ003]`+ posttest$`AS04[SQ004]`+
                                        posttest$`AS04[SQ005]` 
                                        )/5

hist(posttest$`Engagement & Commitment`)

###### 6.3.5 Self-Confidence & Self-Efficacy ####
unique(posttest$`AS05[SQ001]`)
posttest$`AS05[SQ001]` = as.numeric(translate_responses(posttest$`AS05[SQ001]`, translations_ls) )
posttest$`AS05[SQ002]` = as.numeric(translate_responses(posttest$`AS05[SQ002]`, translations_ls) )
posttest$`AS05[SQ003]` = as.numeric(translate_responses(posttest$`AS05[SQ003]`, translations_ls) )

posttest$`Self-Confidence & Self-Efficacy` =  (posttest$`AS05[SQ001]` + posttest$`AS05[SQ002]`  +
                                              posttest$`AS05[SQ003]` )/3
hist(posttest$`Self-Confidence & Self-Efficacy` )
###### 6.3.6 Emotional & Psychological Factors ####
posttest$`AS06[SQ001]` = as.numeric(translate_responses(posttest$`AS06[SQ001]`, translations_ls) )
posttest$`AS06[SQ002]` = as.numeric(translate_responses(posttest$`AS06[SQ002]`, translations_ls) )
posttest$`AS06[SQ003]` = as.numeric(translate_responses(posttest$`AS06[SQ003]`, translations_ls) )
posttest$`AS06[SQ004]` = as.numeric(translate_responses(posttest$`AS06[SQ004]`, translations_ls) )
posttest$`AS06[SQ005]` = as.numeric(translate_responses(posttest$`AS06[SQ005]`, translations_ls) )

posttest$`Emotional & Psychological Factors` =  (posttest$`AS06[SQ001]` + posttest$`AS06[SQ002]` +
                                        posttest$`AS06[SQ003]` + posttest$`AS06[SQ004]` +
                                          posttest$`AS06[SQ005]`)/5


posttest$Q1 = as.character(posttest$Q1)
#Create a new ID in the posttest
# Id = Arm_gender_schoolname_studentname
posttest$Q1 <- trimws(posttest$Q1)
posttest$G01Q03 <- trimws(posttest$G01Q03)
posttest$G01Q05 <- trimws(posttest$G01Q05)
posttest$G01Q01 <- trimws(posttest$G01Q01)

posttest$Q1 <- as.character(posttest$Q1)
posttest$G01Q03 <- as.character(posttest$G01Q03)
posttest$G01Q05 <- as.character(posttest$G01Q05)
posttest$G01Q01 <- as.character(posttest$G01Q01)

# posttest$G01Q01 <- iconv(posttest$G01Q01, to = "ASCII//TRANSLIT")

posttest$id_student <- paste0(as.character(posttest$Q1), "_", posttest$G01Q03, "_",
                              posttest$G01Q05,"_", posttest$G01Q01)
# posttest = anonymize_column(posttest, "id_student")
