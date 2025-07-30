
#### 1. Reading #### 
library(readr)
library(dplyr)
library(arrow)
# posttest_out = arrow::read_parquet( "Data/posttest_out.parquet")
posttest_out = arrow::read_parquet( "Data/postest_out2.parquet")

colnames(posttest)
colnames(posttest_out)
 

##### 1.1 Flag duplicates ####
# postposttest <- flag_duplicates(postposttest)
##### 1.2 Q1 group ####

postposttest <- postposttest %>%
  mutate(Q1 = case_when(
    grepl("Leerpad groep 1", trimws(G00Q16)) ~ "1",
    grepl("Leerpad groep 2", trimws(G00Q16)) ~ "2",
    grepl("Leerpad groep 3", trimws(G00Q16)) ~ "3",
    TRUE ~ G00Q16
  ))
table(postposttest$Q1)
postposttest = postposttest[is.na(postposttest$Q1)==F , ]
#### 1. Reading #### 
library(readr)
library(dplyr)
# posttest_piloto2 <- read_csv("~/Downloads/results-survey324716 (piloto).csv")

# source(paste0(general_path,"Scripts/R/function_cleaning.R" ))

##### 1.1 Flag duplicates ####
postposttest <- flag_duplicates(postposttest)
##### 1.2 Q1 group ####
 

table(postposttest$Q1)
table(is.na(postposttest$G00Q16))

# Lost Around 18% for those who openened but not answer any question
postposttest = postposttest[is.na(postposttest$Q1)==F , ]

##### 1.2 Resolve duplicates ####

 
#### 2. Personal Data #### 
##### 2.1 full name  ####
postposttest$G01Q01 = homogenize_name(postposttest$G01Q01)

fix_names <-  function(postposttest_name, pretest_name, tabla = postposttest, column="G01Q01") {
  ifelse(tabla[[column]] ==postposttest_name , pretest_name, tabla[[column]] )
}
###### 2.1.1 Fixing names ####
postposttest$G01Q01 = fix_names(postposttest_name = "LANDER_MERVILLIE",
                            pretest_name = "MERVILLIE_LANDER",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "TIEBE_VORSTER",
                            pretest_name = "VORSTERS_TIEBE",
                            postposttest, "G01Q01")

postposttest$G01Q01 = fix_names(postposttest_name = "BOECKX_FLEUR",
                            pretest_name = "FLEUR_VAN_HOUWE",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "ROOSE_ROBIN",
                            pretest_name = "ROBIN_ROOSE",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "INGHELBRECHT_PAULINE",
                            pretest_name = "PAULINE_INGHELBRECHT",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "DEVREKER_LENA",
                            pretest_name = "YELENA_DEVREKER",
                            postposttest, "G01Q01")

postposttest$G01Q01 = fix_names(postposttest_name = "RAY_MORTON",
                            pretest_name = "MORTON_RAY",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "AMALIA_GABRIELA_MILU",
                            pretest_name = "MILU_AMALIA_GABRIELA",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "MAHMODI_KABIZADEH",
                            pretest_name = "KIMIA_MAHMODI_KABIZADEH",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "ARJEN_VAN_KERCKHOVEN",
                            pretest_name = "VAN_KERCKHOVEN_ARJEN",
                            postposttest, "G01Q01")

postposttest$G01Q01 = fix_names(postposttest_name = "ALEXANDER_KERSTENS",
                            pretest_name = "KERSTENS_ALEXANDER",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "SAHIEL",
                            pretest_name = "SAHIEL_SINGH",
                            postposttest, "G01Q01")

postposttest$G01Q01 = fix_names(postposttest_name = "JEROME_VAN_EYGEN",
                            pretest_name = "JÉRÔME_VAN_EYGEN",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "ANNA_COOLS",
                            pretest_name = "COOLS_ANNA",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "VAN_EENOO_JUTTA",
                            pretest_name = "JUTTA_VAN_EENOO",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "VANHOOREN_LOWI",
                            pretest_name = "VANHOOREN_LOWIE",
                            postposttest, "G01Q01")

postposttest$G01Q01 = fix_names(postposttest_name = "LILY_DEBURGHGRAEVE",
                            pretest_name = "DEBURGHGRAEVE_LILY",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "SNEPPE_VICTOR",
                            pretest_name = "VICTOR_SNEPPE",
                            postposttest, "G01Q01")
pretest$G01Q01 = ifelse(pretest$G01Q01=='GAÉTAN_DEVOS', 'GAETAN_DEVOS', pretest$G01Q01)
postposttest$G01Q01 = fix_names(postposttest_name = "PLUYM_DRIES",
                            pretest_name = "DRIES_PLUYM",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "GAÉTAN_DEVOS",
                            pretest_name = "DEVOS_GAÉTAN",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "ANNA_PLATTEAU",
                            pretest_name = "PLATTEAU_ANNA",
                            postposttest, "G01Q01")

postposttest$G01Q01 = fix_names(postposttest_name = "MIES_PEETERS",
                            pretest_name = "PEETERS_MIES",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "FLAVIE_VERHESTRAETEN",
                            pretest_name = "FLAVIE_VERHESTRAETEB",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "SAAR_HERMANS",
                            pretest_name = "HERMANS_SAAR",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "MATHIS_VANDEN_BEMPT",
                            pretest_name = "VANDEN_BEMPT_MATHIS",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "AMAURY_DEBELS",
                            pretest_name = "DEBELS_AMAURY",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "DE_MEYER_SEBASTIAN",
                            pretest_name = "SEBASTIAN_DE_MEYER",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "CARDON_DELPHINE",
                            pretest_name = "DELPHINE_CARDON",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "ADRIAAN_ROGIERS",
                            pretest_name = "ROGIERS_ADRIAAN",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "GADEYNE_CAMILLE",
                            pretest_name = "CAMILLE_GADEYNE",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "GILLES_VERELST",
                            pretest_name = "VERELST_GILLES",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "JACK_DE_BRABANDERE",
                            pretest_name = "DE_BRABANDERE_JACK",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "DE_WACHTER_DRIES",
                            pretest_name = "DE_WACHTER_LOU",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "BOUSERIE_OSCAR",
                            pretest_name = "OSCAR_BOUSERIE",
                            postposttest, "G01Q01")

postposttest$G01Q01 = fix_names(postposttest_name = "HAECK_LUKAS",
                            pretest_name = "LUKAS_HAECK",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "TUUR_VANDEGINSTE",
                            pretest_name = "VANDEGINSTE_TUUR",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "BERQUIN_HENRY",
                            pretest_name = "HENRY_BERQUIN",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "INGE_STRYNCK",
                            pretest_name = "STRYNCK_INGE",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "LEON_MERTENS",
                            pretest_name = "MERTENS_LEON",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "GILLES_VERMEULEN",
                            pretest_name = "VERMEULEN_GILLEQ",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "CLEO",
                            pretest_name = "JIMENEZ_LOPEZ_CLÉO",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "VAN_HOEY_SAAR",
                            pretest_name = "VAN_HOEY_TESS",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "LORE_STUYTS",
                            pretest_name = "STUYTS_LORE",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "SOW_HAMADOU",
                            pretest_name = "HAMADOU_SOW",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "LOUISE_COPPENS",
                            pretest_name = "COPPENS_LOUISE",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "AXL_PROOST",
                            pretest_name = "PROOST_AXL",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "HERZET_SEPPE",
                            pretest_name = "SEPPE_HERZET",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "BAILEY_DE_BOCK",
                            pretest_name = "DE_BOCK_BAILEY",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "HAYDEN_ORENS",
                            pretest_name = "ORENS_HAYDEN",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "MULTANI_.ISHAAN",
                            pretest_name = "MULTANI_ISHAAN",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "MULTANI_,_ISHAAN",
                            pretest_name = "MULTANI_ISHAAN",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "LAURENS_LATET",
                            pretest_name = "LATET_LAURENS",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "SIM_CLICTEUR",
                            pretest_name = "CLICTEUR_SIM",
                            postposttest, "G01Q01")

postposttest$G01Q01 = fix_names(postposttest_name = "D'HONDT_JULIE",
                            pretest_name = "JULIE_D'HONDT",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "OCHOA_CHENNZZO",
                            pretest_name = "CHENNZZO_OCHOA",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "JASPER_NACHTERGAELE",
                            pretest_name = "NACHTERGAELE_JASPER",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "LARS_VAN_DER_LOOVEN",
                            pretest_name = "VAN_DER_LOOVEN_LARS",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "GOOSSENS_FABIAN_ANTONIO",
                            pretest_name = "GOOSSENS_FABIAN_ANTONIP",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "DAN_RAMAEKERS",
                            pretest_name = "MIKE_RAMAEKERS",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "BIELAWSKA_NIKOLA",
                            pretest_name = "NIKOLA_BIELAWSKA",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "FERRE_MERTENS",
                            pretest_name = "MERTENS_FERRE",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "MAARTEN_RAIJMAEKERS",
                            pretest_name = "RAIJMAEKERS_MAARTEN",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "MILAN_VANSUYPEENE",
                            pretest_name = "VANSUYPEENE_MILAN",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "NOAH_VAN_BOVEN",
                            pretest_name = "VAN_BOVEN_NOAH",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "DECLOEDT_BENNE",
                            pretest_name = "BENNE_DECLOEDT",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "JAKUB_PIASECKI",
                            pretest_name = "PIASECKI_JAKUB",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "VAN_BROEKHOVEN_CIS",
                            pretest_name = "CIS_VAN_BROEKHOVEN",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "QYLIAM_DE_SOMVIELE",
                            pretest_name = "DE_SOMVIELE_QYLIAM",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "BOONAERT_ISA",
                            pretest_name = "ISA_BOONAERT",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "AGOBIAN_MEGHRI",
                            pretest_name = "MEGHRI_AGOBIAN",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "ANGELIQUE_DEPRE",
                            pretest_name = "DEPRE_ANGELIQUE",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "MARIT_DE_CLERCK",
                            pretest_name = "DE_CLERCK_MARIT",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "ROBIN_DEVRIESE",
                            pretest_name = "DEVRIESE_ROBIN",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "LÉON_DELBEKE",
                            pretest_name = "DELBEKE_LÉON",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "STAN_DYLGAT",
                            pretest_name = "DYLGAT_STAN",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "PEPIJN_RAVELINGIEN",
                            pretest_name = "RAVELINGIEN_PEPIJN",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "NORE_VER_EECKE",
                            pretest_name = "NORE",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "VERBEKE_CHARLOTTE",
                            pretest_name = "CHARLOTTE_VERBEKE",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "HIMPE_MÉLISSE",
                            pretest_name = "MÉLISSE_HIMPE",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "EECKHOUT_AXELLE",
                            pretest_name = "AXELLE_EECKHOUT",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "DAAN_BOGEMANS",
                            pretest_name = "BOGEMANS_DAAN",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "PATTYN_JOPPE",
                            pretest_name = "JOPPE_PATTYN",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "JULIETTE_NUYTTENS",
                            pretest_name = "NUYTTENS_JULIETTE",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "DIEUWKE_SEGAERT",
                            pretest_name = "SEGAERT_DIEUWKE",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "THIAS_VAN_DEN_BOSSCHE",
                            pretest_name = "VAN_DEN_BOSSCHE_THIAS",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "JACK_DE_SCHUTTER_EN_MATTHIAS_LUTGEN",
                            pretest_name = "MATTHIAS_LUTGEN",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "BACHER_MEREL",
                            pretest_name = "MEREL_BACHER",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "LANDER_DE_VULDER",
                            pretest_name = "DE_VULDER_LANDER",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "BASTIAANSEN_ALEXANDER",
                            pretest_name = "ALEXANDER_BASTIAANSEN",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "NIELS_PEELMAN",
                            pretest_name = "PEELMAN_NIELS",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "CHLOË_DUBIN"  ,
                             pretest_name = "DUBIN_CHLOË",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "DE_VOS_TIMO",
                            pretest_name = "TIMO_DE_VOS",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "JOSSE_ZEGERS",
                            pretest_name = "ZEGERS_JOSSE",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "VAN_OSSELAER_LUCA"  ,
                             pretest_name = "LUCA_VAN_OSSELAER",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "WALRAVEYITSKE"  ,
                             pretest_name = "WALRAVE_YITSKE",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "NENA_MOENS",
                             pretest_name = "MOENS_NENA",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "NATHAN_COBBAERT"  ,
                             pretest_name = "COBBAERT_MEULENIJZER_NATHAN",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "WALRAVENS_SOFIA"  ,
                             pretest_name = "SOFIA_WALRAVENS",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "KOBE_DW"  ,
                             pretest_name = "KOBE_D'HONDT_WILLEKENS",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "RAYEN_SONCK"  ,
                             pretest_name = "RYAN_SONCK",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "SPOGMEI_MANGAL"  ,
                             pretest_name = "MANGAL_SPOGMEI",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "JESSE_HERZEEL"  ,
                             pretest_name = "JESSE.HERZEEL",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "JONAS_MOENS"  ,
                             pretest_name = "MOENS_JONAS",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "HANNES_NIELS"  ,
                             pretest_name = "NIELS_HANNES",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "MARCO_IBRAHIM"  ,
                             pretest_name = "IBRAHIM_MARCO",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "LAURENCE_VAN_GANSEN"  ,
                             pretest_name = "VAN_GANSEN_LAURENCE",
                             postposttest, "G01Q01")

postposttest$G01Q01 = fix_names( postposttest_name = "NATHALIE_LUYTEN"  ,
                             pretest_name = "LUYTEN_NATHALIE",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "NICOLAS_BLEUX"  ,
                             pretest_name = "BLEUX_NICOLAS",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "WARRE_VANHESSCHE"  ,
                             pretest_name = "VANHESSCHE_WARRE",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "IAN_KIMPE"  ,
                             pretest_name = "KIMPE_IAN",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "ARNAUD_VANASSCHE"  ,
                             pretest_name = "VANASSCHE_ARNAUD",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "ZEYNEP_KOC"  ,
                             pretest_name = "KOC_ZEYNEP",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "MESTDAGH_JALINA"  ,
                             pretest_name = "JALINA_MESTDAGH",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "SYLVAIN_DEMEULENAERE"  ,
                             pretest_name = "DEMEULENAERE_SYLVAIN",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "ELLA_VANNIEUWKERKE"  ,
                             pretest_name = "VANNIEUWKERKE_ELLA",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "KOBE_PEEL"  ,
                             pretest_name = "PEEL_KOBE",
                             postposttest, "G01Q01")

postposttest$G01Q01 = fix_names( postposttest_name = "ELINE_DE_WINTER"  ,
                             pretest_name = "DE_WINTER_ELINE",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "JORRE_UYTTENHOVE"  ,
                             pretest_name = "UYTTENHOVE_JORRE",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "VAN_DEN_DRIESSCHE"  ,
                             pretest_name = "VAN_DEN_DRIESSCHE_BENITO",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "RHUNE_WYDOOGHE"  ,
                             pretest_name = "WYDOOGHE_RHUNE",
                             postposttest, "G01Q01")

postposttest$G01Q01 = fix_names( postposttest_name = "JUAN_DU_RENG"  ,
                             pretest_name = "JUAN_DU_RANG",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "ALEXIAN_DE_VOCHT"  ,
                             pretest_name = "DE_VOCHT_ALEXIAN",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "REYNDERS_GLORIA"  ,
                             pretest_name = "GLORIA_REYNDERS",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "REYNDERS_GLORIA"  ,
                             pretest_name = "GLORIA_REYNDERS",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "FEMKE_VANDECRUYS"  ,
                             pretest_name = "FEMKE_VDC",
                             postposttest, "G01Q01")

postposttest$G01Q01 = fix_names( postposttest_name = "MICHAEL_ASENCIO"  ,
                             pretest_name = "MICHAEL_GABRIEL_ASENCIO_HIDALGO",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "LEONIE_VAN_OVERMEIRE"  ,
                             pretest_name = "VAN_OVERMEIRE_LEONIE",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "DEKKERS_AMELIE"  ,
                             pretest_name = "AMELIE_DEKKERS",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "OLIVIA_MAES"  ,
                             pretest_name = "MAES_OLIVIA",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "KIARA_BAUWERAERTS"  ,
                             pretest_name = "KIARA_LY",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "SALMA"  ,
                             pretest_name = "ENNIYA_SALMA",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "LUYTEN_VINCENT"  ,
                             pretest_name = "VINCENT_LUYTEN",
                             postposttest, "G01Q01")
# postposttest$G01Q01 = fix_names( postposttest_name = "GAÉTAN_DEVOS"  ,
#                              pretest_name = "GAETAN_DEVOS",
#                              postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "LAVAERT_JARNE"  ,
                             pretest_name = "JARNE_LAVAERT",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "SETH_VAN_LOON"  ,
                             pretest_name = "VAN_LOON_SETH",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "MATTHIJS_TIBO"  ,
                             pretest_name = "TIBO_MATTHIJS",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "BOUNOUCHE_ANIS"  ,
                             pretest_name = "ANIS_BOUNOUCHE",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "BAS_MAGRIET"  ,
                             pretest_name = "MAGRIET_BAS",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names("REMIJSEN_HILDE" , 
                            "HILDE_REMIJSEN",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "MANEL_OUHARROU"  ,
                             pretest_name = "OUHARROU_MANEL",
                             postposttest, "G01Q01")

postposttest$G01Q01 = fix_names( postposttest_name = "JASPER_DECLERCK"  ,
                             pretest_name = "DECLERCK_JASPER",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( "BOEVE_NORE"  , "NORE_BOEVE", postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "OSAKPAMWAN"  ,
                             pretest_name = "OSAKPAMWAN_AGHO",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "MARIA_CLARA_MUSMECI"  ,
                             pretest_name = "MUSMECI_MARIA_CLARA",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "INDY_HUBLÉ,_CERIEL_VLIEGHE"  ,
                             pretest_name = "INDY_HUBLÉ",
                             postposttest, "G01Q01")

postposttest$G01Q01 = fix_names( postposttest_name = "ALEC_VAN_HERCK"  ,
                             pretest_name = "VAN_HERCK_ALEC",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "CEYHUN_TAHA_SOGUTLU"  ,
                             pretest_name = "SOGUTLU_CEYHUN_TAHA",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "AKRAM_ISKHAKOV"  ,
                             pretest_name = "AKRAM",
                             postposttest, "G01Q01")

postposttest$G01Q01 = fix_names( postposttest_name = "ALAYA_VAN_RUYSKENSVELDE"  ,
                             pretest_name = "VAN_RUYSKENSVELDE_ALAYA",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "LORE_VAN_DRIESSCHE"  ,
                             pretest_name = "VAN_DRIESSCHE_LORE",
                             postposttest, "G01Q01")


postposttest$G01Q01 = fix_names( postposttest_name = "JAMIE_VANRYCKEGHEM"  ,
                             pretest_name = "VANRYCKEGHEM_JAMIE",
                             postposttest, "G01Q01")

postposttest$G01Q01 = fix_names( postposttest_name = "NINA_DESCHAMP"  ,
                             pretest_name = "DESCHAMP_NINA",
                             postposttest, "G01Q01")

postposttest$G01Q01 = fix_names( postposttest_name = "VAN_DE_WALLE_AUGUST"  ,
                             pretest_name = "AUGUST_VAN_DE_WALLE",
                             postposttest, "G01Q01")

postposttest$G01Q01 = fix_names( postposttest_name = "JULIE_VANRAES"  ,
                             pretest_name = "VANRAES_JULIE",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "CÉLESTINE_LANGENBERG"  ,
                             pretest_name = "LANGENBERG_CÉLESTINE",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "SEPPE_HOOYBERGHS"  ,
                             pretest_name = "HOOYBERGHS_SEPPE",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "CRABBÉ_ZANA"  ,
                             pretest_name = "ZANA_CRABBÉ",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "GESQUIERE_FLEUR"  ,
                             pretest_name = "GESQUIÈRE_FLEUR",
                             postposttest, "G01Q01")

postposttest$G01Q01 = fix_names( postposttest_name = "GAUTHIER_SABBE"  ,
                             pretest_name = "GAUTHIER",
                             postposttest, "G01Q01")

postposttest$G01Q01 = fix_names( postposttest_name = "EVA_FIERENS"  ,
                             pretest_name = "FIERENS_EVA",
                             postposttest, "G01Q01")

postposttest$G01Q01 = fix_names( postposttest_name = "AXANA_DUTILLIE"  ,
                             pretest_name = "DUTILLIE_AXANA",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "ARTHUR_SIMOENS"  ,
                             pretest_name = "SIMOENS_ARTHUR",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "ELENA_GALLE"  ,
                             pretest_name = "GALLE_ELENA",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "AMINE_AJROUD"  ,
                             pretest_name = "AJROUD_AMINE",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "PLATTEEUW_MATHIS"  ,
                             pretest_name = "MATHIS_PLATTEEUW",
                             postposttest, "G01Q01")

postposttest$G01Q01 = fix_names( postposttest_name = "IBE_CATELIN"  ,
                             pretest_name = "CATELIN_IBE",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "SAMUEL"  ,
                             pretest_name = "SAMUEL_D'HAENENS",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "JEF_VAN_LINT"  ,
                             pretest_name = "VAN_LINT_JEF",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "ELIZA_EECKHOUT"  ,
                             pretest_name = "EECKHOUT_ELIZA",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "XANDER_VAN_DE_VELDE"  ,
                             pretest_name = "VAN_DE_VELDE_XANDER",
                             postposttest, "G01Q01")

postposttest$G01Q01 = fix_names( postposttest_name = "STEF_BOULONNE"  ,
                             pretest_name = "BOULONNE_STEF",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "VAN_DE_VENSTER_FRAUKE"  ,
                             pretest_name = "FRAUKE_VAN_DE_VENSTER",
                             postposttest, "G01Q01")

postposttest$G01Q01 = fix_names( postposttest_name = "JANA_VAN_DE_CAPPELLE"  ,
                             pretest_name = "VAN_DE_CAPPELLE_JANA",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names( postposttest_name = "VAN_ESSCHE"  ,
                             pretest_name = "LEON_VAN_ESSCHE",
                             postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "IEBEN_COENE",
                            pretest_name = "COENE_IEBEN",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "MYKOLAI_IVANENKO",
                            pretest_name = "IVANENKO_MYKOLAI",
                            postposttest, "G01Q01")
postposttest$G01Q01 = fix_names(postposttest_name = "WIKTOR_FRANCISZEK_BAZYLUK",
                                pretest_name = "WIKTOR_BAZYLUK",
                                postposttest, "G01Q01")
##### 2.2 Fixing treatment group ####
postposttest$AT=F
postposttest$Q1 = ifelse(postposttest$G01Q01=="BOUCKAERT_LARA",'2',postposttest$Q1   )
postposttest$Q1 = ifelse(postposttest$G01Q01=="POLLENTIER_RODÉRIC",'1',postposttest$Q1   )
postposttest$Q1 = ifelse(postposttest$G01Q01=="MATHIS_PLATTEEUW",'2',postposttest$Q1   )
postposttest$Q1 = ifelse(postposttest$G01Q01=="MELLEBEEK_NOOR",'3',postposttest$Q1   )
postposttest$Q1 = ifelse(postposttest$G01Q01=="JARNE_VAN_DOREN",'3',postposttest$Q1   )
postposttest$Q1 = ifelse(postposttest$G01Q01=="HIGGS_ROXY",'2',postposttest$Q1   )
postposttest$Q1 = ifelse(postposttest$G01Q01=="EMMA_DE_GROOTE",'2',postposttest$Q1   )
postposttest$Q1 = ifelse(postposttest$G01Q01=="CHEYENNE_VAN_DAELE",'2',postposttest$Q1   )
postposttest$Q1 = ifelse(postposttest$G01Q01=="HALIL_YURUK",'2',postposttest$Q1   )
postposttest$Q1 = ifelse(postposttest$G01Q01=="KORKMAZ_BERAT",'2',postposttest$Q1   )
postposttest$Q1 = ifelse(postposttest$G01Q01=="ENGELEN_MIEL",'3',postposttest$Q1   )
postposttest$Q1 = ifelse(postposttest$G01Q01=="ASTRID_EÜLER",'2',postposttest$Q1   )
postposttest$Q1 = ifelse(postposttest$G01Q01=="JARNE_LAVAERT",'3',postposttest$Q1   )
postposttest$Q1 = ifelse(postposttest$G01Q01=="GLORIA_REYNDERS",'2',postposttest$Q1   )
postposttest$Q1 = ifelse(postposttest$G01Q01=="BRUGGEMAN_RÉMI",'3',postposttest$Q1   )
postposttest$Q1 = ifelse(postposttest$G01Q01=="BRAM_VAN_MOSSEVELDE",'2',postposttest$Q1   )
postposttest$Q1 = ifelse(postposttest$G01Q01=="KOC_ZEYNEP",'3',postposttest$Q1   )
postposttest$Q1 = ifelse(postposttest$G01Q01=="EMILE_CORNELIS",'3',postposttest$Q1   )
postposttest$Q1 = ifelse(postposttest$G01Q01=="LOUIS_VANHERCK",'2',postposttest$Q1   )
postposttest$Q1 = ifelse(postposttest$G01Q01=="BLEUX_NICOLAS",'3',postposttest$Q1   )
postposttest$Q1 = ifelse(postposttest$G01Q01=="THORBEN_VETS",'1',postposttest$Q1   )
postposttest$Q1 = ifelse(postposttest$G01Q01=="SIMANY_BRYAN",'2',postposttest$Q1   )
postposttest$Q1 = ifelse(postposttest$G01Q01=="DE_VALCK_SUNE",'3',postposttest$Q1   )
postposttest$Q1 = ifelse(postposttest$G01Q01=="SOFIA_WALRAVENS",'1',postposttest$Q1   )
postposttest$Q1 = ifelse(postposttest$G01Q01=="LEWIS_COZYNS",'2',postposttest$Q1   )
postposttest$Q1 = ifelse(postposttest$G01Q01=="VAN_DELSEN_CHARLOTTE",'2',postposttest$Q1   )
postposttest$Q1 = ifelse(postposttest$G01Q01=="COBBAERT_MEULENIJZER_NATHAN",'1',postposttest$Q1   )
postposttest$Q1 = ifelse(postposttest$G01Q01=="MOENS_NENA",'3',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="SIENNA_DE_CLERCK",'1',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="AMINE_EL_HAJJAMI",'1',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="HEREZ_INES",'3',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="VANDEVYVERE_LOWIE",'2',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="NUYTTENS_JULIETTE",'3',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="BÁLINT_RAKOSI",'2',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="MÉLISSE_HIMPE",'3',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="BUYSSE_LAURA",'2',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="CHARLOTTE_VERBEKE",'3',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="VERHAEGE_DAGMAR",'3',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="DYLGAT_STAN",'2',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="DEBOSSCHERE_LOWIE",'2',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="DEVRIESE_ROBIN",'2',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="KINNA_BASTIAAN",'2',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="LOÏC_NGOMBE",'2',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="ELIOT_DALDINI",'2',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="BUGGENHOUT_LISA",'2',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="KOBE_ROELS",'2',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="FLEUR_STRYBOL",'1',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="TIM_DRIESEN",'1',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="GILIS_OWEN",'1',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="VAN_HOEY_TESS",'3',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="VERMEULEN_GILLEQ",'2',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="BRUYNINCKX_ARNE",'1',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="TOPS_TIBO",'1',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="CEULEMANS_SEPPE",'3',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="DEBELS_AMAURY",'1',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="VANHOUDT_VICA",'1',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="SNOECKX_MARA",'3',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="BETTENS_NATHAN",'1',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="ROGIERS_ADRIAAN",'2',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="RUELENS_RUNE",'3',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="PEETERS_MIES",'1',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="DIEDE_VERSCHUEREN",'2',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="HANNELORE_DAEMS",'2',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="VERBRUGGE_JASPER",'1',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="CAMILLE_GADEYNE",'1',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="VERMOTE_CARSTEN",'2',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="COOLS_ANNA",'1',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="MULLENDERS_EVI",'2',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="BOOGAERTS_FENNE",'1',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="VIKTOR_DECKX",'1',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="ARNE_DE_JONGH",'2',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="MANISHIMWE_MATHIAS",'1',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="DE_LEEUW_YELLE",'3',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="MEEUS_AIDAN",'1',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="MORTON_RAY",'2',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="DE_WINTER_ELINE",'3',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="JESSE.HERZEEL",'2',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="OLIVIERS_SIEBE",'2',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="SEPPE_VINKEN",'2',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="JOYE_KAMIEL",'1',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="ROBIN_ROOSE",'1',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="D'HONDT_WARD",'2',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="SAM_DE_BROUWER",'1',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="BOLLE_CAMILLE",'2',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="DELAERE_FRAN",'1',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="HELENA_WOUTERS",'3',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="FLEUR_VAN_HOUWE",'2',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="MERVILLIE_LANDER",'3',postposttest$Q1)
###### 2.2 Fixing treatment group ####
postposttest$Q1 = ifelse(postposttest$G01Q01=="JESSE.HERZEEL",'1',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="IAN_VAN_WEYENBERGHE",'1',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="DEVOS_GAÉTAN",'3',postposttest$Q1)
postposttest$Q1 = ifelse(postposttest$G01Q01=="MILAN_VERMEERSCH",'3',postposttest$Q1)

postposttest$AT = ifelse(postposttest$G01Q01=="MILAN_VERMEERSCH",T,postposttest$AT )
postposttest$AT = ifelse(postposttest$G01Q01=="DEVOS_GAÉTAN",T,postposttest$AT )
postposttest$AT = ifelse(postposttest$G01Q01=="IAN_VAN_WEYENBERGHE",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="JESSE.HERZEEL",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="MERVILLIE_LANDER",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="FLEUR_VAN_HOUWE",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="HELENA_WOUTERS",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="DELAERE_FRAN",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="SAM_DE_BROUWER",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="SAM_DE_BROUWER",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="D'HONDT_WARD",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="ROBIN_ROOSE",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="JOYE_KAMIEL",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="SEPPE_VINKEN",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="OLIVIERS_SIEBE",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="JESSE.HERZEEL",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="DE_WINTER_ELINE",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="MORTON_RAY",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="MEEUS_AIDAN",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="DE_LEEUW_YELLE",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="MANISHIMWE_MATHIAS",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="ARNE_DE_JONGH",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="VIKTOR_DECKX",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="BOOGAERTS_FENNE",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="MULLENDERS_EVI",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="COOLS_ANNA",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="VERMOTE_CARSTEN",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="CAMILLE_GADEYNE",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="VERBRUGGE_JASPER",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="HANNELORE_DAEMS",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="DIEDE_VERSCHUEREN",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="PEETERS_MIES",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="RUELENS_RUNE",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="ROGIERS_ADRIAAN",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="BETTENS_NATHAN",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="SNOECKX_MARA",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="VANHOUDT_VICA",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="DEBELS_AMAURY",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="CEULEMANS_SEPPE",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="TOPS_TIBO",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="BRUYNINCKX_ARNE",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="VERMEULEN_GILLEQ",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="VAN_HOEY_TESS",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="GILIS_OWEN",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="TIM_DRIESEN",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="FLEUR_STRYBOL",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="KOBE_ROELS",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="BUGGENHOUT_LISA",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="ELIOT_DALDINI",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="LOÏC_NGOMBE",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="KINNA_BASTIAAN",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="DEVRIESE_ROBIN",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="DEBOSSCHERE_LOWIE",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="DYLGAT_STAN",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="VERHAEGE_DAGMAR",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="CHARLOTTE_VERBEKE",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="BUYSSE_LAURA",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="MÉLISSE_HIMPE",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="BÁLINT_RAKOSI",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="NUYTTENS_JULIETTE",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="VANDEVYVERE_LOWIE",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="HEREZ_INES",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="AMINE_EL_HAJJAMI",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="SIENNA_DE_CLERCK",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="MOENS_NENA",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="COBBAERT_MEULENIJZER_NATHAN",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="VAN_DELSEN_CHARLOTTE",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="LEWIS_COZYNS",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="SOFIA_WALRAVENS",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="DE_VALCK_SUNE",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="SIMANY_BRYAN",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="THORBEN_VETS",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="BLEUX_NICOLAS",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="LOUIS_VANHERCK",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="EMILE_CORNELIS",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="KOC_ZEYNEP",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="BRAM_VAN_MOSSEVELDE",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="BRUGGEMAN_RÉMI",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="GLORIA_REYNDERS",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="JARNE_LAVAERT",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="ASTRID_EÜLER",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="ENGELEN_MIEL",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="BOUCKAERT_LARA",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="POLLENTIER_RODÉRIC",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="MATHIS_PLATTEEUW",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="MELLEBEEK_NOOR",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="JARNE_VAN_DOREN",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="HIGGS_ROXY",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="EMMA_DE_GROOTE",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="CHEYENNE_VAN_DAELE",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="HALIL_YURUK",T,postposttest$AT   )
postposttest$AT = ifelse(postposttest$G01Q01=="KORKMAZ_BERAT",T,postposttest$AT   )

# Cheking linked name
# filter_and_select(postposttest,  "MANON")
# filter_and_select(postposttest_only,  "MANON")
##### 2.2 Gender  ####
postposttest$G01Q03 = translate_responses(postposttest$G01Q03, translations)
table(postposttest$G01Q03)
postposttest$G01Q03 = ifelse(postposttest$G01Q01=="COBBAERT_MEULENIJZER_NATHAN", 'Boy', postposttest$G01Q03 )
postposttest$G01Q03=ifelse(postposttest$G01Q01=="BENITO_SOMERS",
                       'Boy',
                       postposttest$G01Q03)
postposttest$G01Q03=ifelse(postposttest$G01Q01=="DRIN_KRAJA",
                       'Boy',
                       postposttest$G01Q03)
postposttest$G01Q03=ifelse(postposttest$G01Q01=="VAN_DER_LOOVEN_LARS",
                       'Boy',
                       postposttest$G01Q03)
postposttest$G01Q03=ifelse(postposttest$G01Q01=="MATISSE_VERHEYEN",
                       'Boy',
                       postposttest$G01Q03)
##### 2.3 City  ####
# postposttest$G01Q04 <- homogenize_cities(postposttest$G01Q04)
##### 2.4 School Name  ####
postposttest$G01Q05 <- homogenize_school_name(postposttest$G01Q05)

###### 2.4.1 Fixing school name ####

postposttest$G01Q05 = ifelse(postposttest$G01Q01=="IAN_VAN_WEYENBERGHE",
                         'Sint-Paulusinstituut',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="MERVILLIE_LANDER",
                         'Leiepoort Campus Sinthendrik',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="MATTIAS_OCKERMAN",
                         'Ka Voskelaan Topsortschool Gent',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="FLEUR_VAN_HOUWE",
                         'Kvri',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VANRAES_JULIE",
                         'Sintbarbara',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="AMÉLIE_DE_BACKER",
                         'Sintbarbara',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="AMY_DHAENE",
                         'Sint-Barbaracollege',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="HELENA_WOUTERS",
                         'Campus Sinthendrik',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="MONALISA_VANHOLME",
                         'Sint-Barbaracollege',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VANAUDENAERDE_RANIA",
                         'Sint-Jozefscollege Torhout',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="MANISHIMWE_MATHIAS",
                         'Sintpaulus',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="TRANCEZ_SEM",
                         'Sint-Jozefscollege Torhout',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="JOYE_KAMIEL",
                         'Sint-Jozefscollege Torhout',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="SEPPE_VINKEN",
                         'Topsportschool Gent',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="OLIVIERS_SIEBE",
                         'Topsportschool Gent',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="YELENA_DEVREKER",
                         'Topsport Voskenslaan',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="NURGAMID_BOLATAYEV",
                         'Broederschool Handel',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="MEEUS_AIDAN",
                         'Broeders',
                         postposttest$G01Q05)

postposttest$G01Q05 = ifelse(postposttest$G01Q01=="GIEL_DE_BRABANDER",
                         'Topsport Gent Voskenslaan',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="CASSIERS_CARA",
                         'Sintnorbertus Insituut',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="MATTEO_VERLOO",
                         'Heilighartcollege Heistopdenberg',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="JARNE_REMORY",
                         'Sintpaulus Instituut',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="ARNE_DE_JONGH",
                         'Campus Sintursula',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="DUPONT_JEAN",
                         'Sint-Barbaracollege',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="DE_LEEUW_YELLE",
                         'Sint Paulusinstituut',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VAN_KERCKHOVEN_ARJEN",
                         'Sintursula',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="MATTHIS_LAHAEYE",
                         'Atheneum',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="SAHIEL_SINGH",
                         'Campus Ticherluij',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VAN_HAAREN_SEPPE",
                         'Sintursula Istituut',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="GOEGEBEUR_ARTHUR",
                         'Sint Ursula',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="JULES_GOELEN",
                         'Sint Ursula Instituut',
                         postposttest$G01Q05)
postposttest$G01Q01 = fix_names(postposttest_name = "TIEBE_DEGRAEVE",
                            pretest_name = "DEGRAEVE_TIEBE",
                            postposttest, "G01Q01")
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="KIMIA_MAHMODI_KABIZADEH",
                         'Campus Sintjanberchmanscollege Mol',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VIKTOR_DECKX",
                         'Campus Sint-Jan Berchmans Mol',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="MATISSE_VERHEYEN",
                         'Sintjan Berchmanscollege',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="BOOGAERTS_FENNE",
                         'Sintjanberchmanscollege',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="KERSTENS_ALEXANDER",
                         'Campus Sintjanberchmanscollege',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="BERRENS_VIK",
                         'Sint Janberchmanscollege',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="MAERSCHALCK_GIEL",
                         'Campus Sintjan Berghmans Mol',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="JÉRÔME_VAN_EYGEN",
                         'Sint-Rembert College Torhout',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="BEUCKELAERE_NINA",
                         'Sint-Jozefscollege Torhout',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VERMOTE_TRISTAN",
                         'Sint-Jozefscollege Torhout',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="NOOR_VANDENDIJCK",
                         'Campus Sint Jan Berschmans College',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VAN_KEILEGOM_MATS",
                         'Heilig-Hartcollege',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="SOPHIE_LOGGHE",
                         'Sint-Jozefscollege Torhoutsintrembert',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VICTOR_SNEPPE",
                         'Sint-Rembert College Torhout',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="ROGIERS_ADRIAAN",
                         'Sintbarbara College',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="LANGENBERG_CÉLESTINE",
                         'Sintbarbara',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="DE_LATHAUWER_ALEXANDER",
                         'Sint-Barbaracollege',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="LUKAS_HAECK",
                         'Sint Barbara',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="NIJS_ROMY",
                         'Kardinaal Van Roeyinstituut Vorselaar',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="JULIE_MENEZ",
                         'Immaculata',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VELDEMAN_PHÉLINE",
                         'Immaculata instituut',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="CEULEMANS_SEPPE",
                         'Heilig-Hartcollege',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VAN_ES_MILAN",
                         'Sintnorbertusinstituut Snor',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="TOPS_TIBO",
                         'Heilighartcollege Heistopdenberg',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VERMEULEN_GILLEQ",
                         'Heilig Hartcollege',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="JIMENEZ_LOPEZ_CLÉO",
                         'Moretus',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="ANAS_KARAMZIANI",
                         'Moretus Ekeren',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="HAMADOU_SOW",
                         'Moretus Ekeren',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VAN_HOEY_TESS",
                         'Leiepoort Sinthendrik',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="STUYTS_LORE",
                         'Moretus Ekeren',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="DE_BOCK_BAILEY",
                         'Broederschool Handel',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="IMRAN_SOUSSI",
                         'Broederschool',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="STRUYF_MIEL",
                         'Het Kompas',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VLEMINCKX_NIO",
                         'Technisch Atheneum Brasschaat',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VAN_GASSE_MICHIEL",
                         'Broederschool',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="MEERSMAN_SENNE",
                         'Broederschool',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="ORENS_HAYDEN",
                         'Moretus',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="AMIEN_JANSSENS",
                         'Sintnorbertus',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VAN_DER_LOOVEN_LARS",
                         'Leiepoort Campus Sinthendrik',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="MALKIC_DAMIR",
                         'Moretusekeren',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="EMMA_IVENS",
                         'Virgo Sapiens',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="CLICTEUR_SIM",
                         'TA Brasschaat',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="NACHTERGAELE_JASPER",
                         'Leiepoort Campus Sinthendrik',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="JANNES_VAN_DEN_BRANDE",
                         'Virgosapiens Secundair',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="JACOBS_LIAM",
                         'Campus Hast',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="MAHIEU_JANI",
                         'Atheneum Courtmanslaan',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="SPILLEBEEN_NATHAN",
                         'Atheneum Courtmanslaan',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="YORBEN_VAN_HAECHT",
                         'Moretus Ekeren',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="KOBE_ROELS",
                         'Moretusekeren',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="MERTENS_FERRE",
                         'Moretus Ekeren',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="ELIOT_DALDINI",
                         'Kobos College',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="FLEUR_BLUEKENS",
                         'Heilig Graf',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VAN_LOOVEREN_LOTTE",
                         'Heilig Graf',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VANSUYPEENE_MILAN",
                         'Sint-Paulusinstituut',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="DE_SAEDELEER_SIMON",
                         'Sint-Paulusinstituut Herzele',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="PARMENTIER_MARIE",
                         'Sint-Paulusinstituut',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="BENNE_DECLOEDT",
                         'Sintpaulus Instituut',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="DEPRE_ANGELIQUE",
                         'Moretus',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="CARREIN_BRITT",
                         'Gotechnisch Atheneum',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="DE_CLERCK_MARIT",
                         'Technisch Atheneum Brasschaat',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="LORE_PEDE",
                         'Sintpaulus Instituut Herzele',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="DEVRIESE_ROBIN",
                         'Spes Nostra',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="KINNA_BASTIAAN",
                         'Spes Nostra Kuurne',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="DELBEKE_LÉON",
                         'Spes Nostra Kuurne',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="DEBOSSCHERE_LOWIE",
                         'Spes Nostra Kuurne',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="HUYS_THIBAU",
                         'Spes Nostra Kuurne',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="NORE",
                         'Spes Nostra',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VAN_DAMME_ARNO",
                         'Spes Nostra',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="RAVELINGIEN_PEPIJN",
                         'Spes Nostra',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="RONSSE_MATHIEU",
                         'Spes Nostra Kuurne',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="BRUYNSEELS_NORA",
                         'Sint Clara College',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="DE_MEYER_IBEN",
                         'Sintpaulus Instituut',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="MARIAM_REVA",
                         'Heilige Familie Ieper',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="BOGEMANS_DAAN",
                         'Sint-Paulusinstituut Herzele',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VINCENT_STERCKX",
                         'Sintclara College',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="BRUYNSEELS_NORA",
                         'Sint Clara College',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="MEREL_BACHER",
                         'Sintpaulus Instituut',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="TUUR_VERDONCK",
                         'Sintjanbergmanscollege',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="BOGAERTS_JADA",
                         'Sintjan Berchmanscollege',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="ZEGERS_JOSSE",
                         'Sintjan Berchmans College',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="STRYNCK_INGE",
                         'Leiepoort campus Sint Hendrik',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="EMILE_CORNELIS",
                         'Sintmaria',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="ANISSA_LAHRACH",
                         'Sintnorbertus Instituut',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="ALEXANDER_BASTIAANSEN",
                         'Sintjan Berchmanscollege Malle',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="COP_ARTHUR",
                         'Sint Jan Berchmanscollege',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="BENITO_SOMERS",
                         'Sint Jan Berghmanscollege',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VANHOLDERBEKE_MAURO",
                         'Heilige Familie Ieper',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="SOMERS_FELIPE",
                         'Sint Jan Berchmanscollege',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="HEREZ_INES",
                         'Heilig Hart College',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="HAYLEY_BACHOT",
                         'Go Technisch Atheneum Brasschaat',
                         postposttest$G01Q05 )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="LOWIE_STEPPE",
                         'Sintpaulus',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="YENTL_VAN_WIJNENDAELE",
                         'Sint-Paulusinstituut',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VAN_DE_PONTSELE_JONAS",
                         'Sintpaulus',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VAN_DER_BIEST_INE",
                         'Sintpaulus Instituut Herzele',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="BOGAERT_MATTHIAS",
                         'Kavoskenslaan Topsport',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="COBBAERT_MEULENIJZER_NATHAN",
                         'Sintpaulus Instituut',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="KOBE_D'HONDT_WILLEKENS",
                         'Topsport Voskenslaan',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VERNAILLEN_STAN",
                         'Ka Athenuem Voskenslaan',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="KIMPE_IAN",
                         'Topsport Voskenslaan',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VAN_DEN_HEEDE_BENTHE",
                         'Topsportschool Gent Voskenslaan',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="DAAN_VAN_LAERE",
                         'Ka Voskenslaan Topsport',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="CAUA_FORONI_DE_OLIVEIRA",
                         'Topsport Voskenslaan',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="ARTHUR_DUBOIS",
                         'Kavoskenslaan Athemeum',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="DE_VALCK_SUNE",
                         'Topsport Voskenslaan',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="YARNE_EYLENBOSCH",
                         'Topsport Voskenslaan',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VAN_GANSEN_LAURENCE",
                         'Sint Maria Geel',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="GOOSSENS_MATHIJS",
                         'Sint Maria Geel',
                         postposttest$G01Q05   )

postposttest$G01Q05 = ifelse(postposttest$G01Q01=="DE_MUYNCK_TARA",
                         'Topsportschool Ka Voskenslaan',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="BLEUX_NICOLAS",
                         'Sintmaria Geel',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="SLAGHMUYLDER_ELISE",
                         'Heilig Hart College Halle',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="LUYTEN_NATHALIE",
                         'Sint Maria Geel',
                         postposttest$G01Q05   )

postposttest$G01Q05 = ifelse(postposttest$G01Q01=="CASTRO_MURILLO_JUAN-PEDRO",
                         'Heilighart Halle',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="JESSICA_DERAYMAEKER",
                         'Heilig-Hartcollege',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VANHESSCHE_WARRE",
                         'Vrij Handels- en Sportinstituut',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="YORBEN_PAUWELS",
                         'Sint-Norbertusinstituut',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="SAID_BEN_AJIBA_HALIMA",
                         'Helighart',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="CLOÉ_DEVOS",
                         'Heilig-Hartcollege',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VANASSCHE_ARNAUD",
                         'Kavokenslaan',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="CORDIER_JUUL",
                         'Zavo',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VANHAELEN_LIAM",
                         'Heilig Hart en College',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="BRAM_VAN_MOSSEVELDE",
                         'Topsportschool Voskenslaan',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="BELINDA_BOLEMBO",
                         'Heilig-Hart & College Halle',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="DE_WINTER_ELINE",
                         'Topsport Vos',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="BENALI_RAMZI",
                         'Topsport Gent',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="MATHILDE_FRANCOTTE",
                         'Topsport Voskenslaan',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="CLICTEUR_SIM",
                         'TA Brasschaat',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VERMOTE_CARSTEN",
                         'Sintjozefscolllege',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="ARIANE_DEPROOST",
                         'Sint-Jozefscollege Torhout',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VERHOUGSTRAETE_TILLE",
                         'Topsport Voskenslaan',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="YELENA_DEVREKER",
                         'Topsport Voskenslaan',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="TRAEN_SINNE",
                         'Vrij Handels- en Sportinstituut',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="DEGRYSE_HANNE",
                         'Topsport Voskenslaan',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="KANATLAR_KIYAN_CAN",
                         'Topsportschool Gent',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="BAERT_CHLOE",
                         'Topsport Voskenslaan',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="PERGOOT_RUNE",
                         'Topsport Voskenslaan',
                         postposttest$G01Q05   )

postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VAN_HERCK_ALEC",
                         'Campus Sint-Jan Berchmans Mol',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="MICHAEL_GABRIEL_ASENCIO_HIDALGO",
                         'Zavo',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="BASTIAENS_MIA",
                         'Sintdimpna College',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VERACHTERT_NEL",
                         'Sint Dimpna',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="GROENEN_ASTRID",
                         'Sint Dimpna',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="MUSTAFA_ENES_YIGIT",
                         'Zavo Sterrebeek',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="MAES_OLIVIA",
                         'Sint Dimpna Geel College',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="KIARA_LY",
                         'Sint Dimpna College',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VERBRUGGHE_JENS",
                         'Technisch Instituut Heilige Familie Ieper',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="DEVOS_GAÉTAN",
                         'Sint-Jozefscollege Torhout',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VAN_LOON_SETH",
                         'Sintclaracollege',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="ASTRID_EÜLER",
                         'Onzelieve Vrouwen Presentatie',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="MATTHIJS_TIBO",
                         'Sintpaulus Instituut',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="MOULOUD_KAOUTAR",
                         'Scheppersinstituut',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="ENNIYA_SALMA",
                         'Go Spronk',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="MAGRIET_BAS",
                         'Edugolo',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="JAYDA_BOGAERS",
                         'Sintclaracollege',
                         postposttest$G01Q05)
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VAN_VLAENDEREN_SENNE",
                         'Heilig Hart College Halle',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="CUYL_ELODIE",
                         'Heilighart College',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="QUAGHEBEUR_TABITHA",
                         'MSKA Roeselare',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="FRANCIS_ANAÏS",
                         'Sint-Norbertusinstituut',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="KYLIAN_ABDEL_MAWGOUD",
                         'Mska Campus Tant',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="ACHBARI_HAFSA",
                         'Sintagnesinstituut',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="DELEU_YOUNA",
                         'MSKA Roeselare',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VAN_HERCK_ALEC",
                         'Campus Sint-Jan Berchmans Mol',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="RANA_CHARKI",
                         'Heilig Hartcollege Halle',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="MUSMECI_MARIA_CLARA",
                         'Mska Campus Tant',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="KERSTENS_MAXIM",
                         'Sint Jan Berchmanscollege',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="CRISTIAN_FRANTESCU",
                         'Campus Sint-Jan Berchmans Mol',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="ANNELIEN_GEBOERS",
                         'Sint Dimpna College Geel',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="JARNE_LOYEN",
                         'Sint Franciscuscollege',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VANDERHEYDEN_SENNE",
                         'Sintmaria Geel',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="MILAN_VERMEERSCH",
                         'Sint-Jozefscollege Torhout',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="KERSTENS_ALEXANDER",
                         'Campus Sintjanberchmanscollege',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VAN_RUYSKENSVELDE_ALAYA",
                         'Campus De Toren Edugo Lochristi',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="STRYNCK_INGE",
                         'Leiepoort Campus Sinthendrik',
                         postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="SEDRANI_QUINTO",
                         'Sintjan Berchmanscollege',
                         postposttest$G01Q05   )

postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VERHEYEN_FRÉ",
                         'Campus Sint-Jan Berchmans',
                         postposttest$G01Q05   )

postposttest$G01Q05 = ifelse(postposttest$G01Q01=="HALIL_YURUK",
                         'Olvp Secundair Onderwijs',postposttest$G01Q05   )

postposttest$G01Q05 = ifelse(postposttest$G01Q01=="CHEYENNE_VAN_DAELE",
                         'Onzelievevrouw Presentatie',postposttest$G01Q05   )

postposttest$G01Q05 = ifelse(postposttest$G01Q01=="HAKAN_KURT",
                         'Olvpsint Niklaas',postposttest$G01Q05   )

postposttest$G01Q05 = ifelse(postposttest$G01Q01=="LUKAS_HAECK",
                         'Sint Barbara',postposttest$G01Q05   )

postposttest$G01Q05 = ifelse(postposttest$G01Q01=="CANSIER_FINN",
                         'Campus Sintjan Berchmanscollege Mol',postposttest$G01Q05   ) 
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="DELEU_YOUNA",
                         'Mska',
                         postposttest$G01Q05   )

postposttest$G01Q05 = ifelse(postposttest$G01Q01=="DERMUL_EMMA",'Sintjozefs College Torhout',postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="ISENBAERT_MARIE",'Sint-Jozefscollege Torhout',postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="JUTTA_VAN_EENOO",'Sint-Jozefscollege Torhout',postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$id==46,'Sint-Rembert College Torhout',postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VANPARYS_GABRIËL",'Sint-Jozefscollege Torhout',postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="AJROUD_AMINE",'Sint Jozefinstituut Bokrijk',postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VANDER_CRUYS_MILAN",'Atheneum Halle',postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="DUTILLIE_AXANA",'Sint-Pauluscollege',postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="HOOYBERGHS_SEPPE",'Ksom Campus Sintjan Berchmans',postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="SPATHARAKIS_ELENI",'Sint Jozefinstituut Bokrijki',postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="ABDELLAH_KHATABI",'Atheneum Halle',postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="DUTILLIE_AXANA",
                         'Sint-Pauluscollege',postposttest$G01Q05   ) 

postposttest$G01Q05 = ifelse(postposttest$G01Q01=="SAMUEL_D'HAENENS",
                         'Sintbarbara College',postposttest$G01Q05   ) 

postposttest$G01Q05 = ifelse(postposttest$G01Q01=="JÉRÔME_VAN_EYGEN",
                         'Sint-Rembert College Torhout',postposttest$G01Q05   )  
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="CATELIN_IBE",
                         'Sint Pauluscollege Wevelgem',postposttest$G01Q05   ) 
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="DUTILLIE_AXANA",
                         'Sint-Pauluscollege',postposttest$G01Q05   ) 

postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VAN_DE_VELDE_XANDER",
                         'Sintbarbara College',postposttest$G01Q05   ) 
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="VINCENT_THIRIART",
                         'Heilig-Hartcollege',postposttest$G01Q05   ) 

postposttest$G01Q05 = ifelse(postposttest$G01Q01=="HENRY_BERQUIN",
                         'Sint-Barbaracollege',postposttest$G01Q05   )
postposttest$G01Q05 = ifelse(postposttest$G01Q01=="DE_WINTER_ELINE" &
                             postposttest$Q1=='2',
                             'Sintpaulus Instituut',
                             postposttest$G01Q05)
##### 2.5 Course of Student  ####
# postposttest$G01Q06 <- homogenize_course_group(postposttest$G01Q06)
##### 2.6 Type of School  ####
# postposttest$G01Q07 = translate_responses(postposttest$G01Q07, translations)


# postposttest$G00Q16 <- translate_responses(postposttest$G00Q16, translations)
##### 2.7 Where did you take the digital class? ####
# postposttest$G01Q08 <- translate_responses(postposttest$G01Q08, translations)
# table(postposttest$G01Q08 )

#### 6. Cleanning postposttest #### 


##### 6.1 Assesses understanding of percentages in the context of a tax rate. ####

postposttest$PT03 = ifelse(postposttest$PT03=='25%'| postposttest$PT03== 'AO02', 1, 0)
postposttest$PT03 = as.numeric(postposttest$PT03)
table(postposttest$PT03 )

##### 6.2 Knowledge Score ####
## Are there difference between control assigment and treatment assigment at baseline?

postposttest$PTF01 =ifelse(postposttest$PTF01=='239'| postposttest$PTF01== 'AO01', 1, 0)
table(postposttest$PTF01)
postposttest$PTF01 = as.numeric(postposttest$PTF01)

postposttest$PTF02 = translate_responses(postposttest$PTF02, translations)
table(postposttest$PTF02)
postposttest$PTF02 =ifelse(postposttest$PTF02=="Progressive tax system"| postposttest$PTF02== 'AO03', 1, 0)
postposttest$PTF02 = as.numeric(postposttest$PTF02)

table(postposttest$PTF06)

postposttest$PTF06 =ifelse(postposttest$PTF06=="€13500"| postposttest$PTF06== 'AO03', 1, 0)
postposttest$PTF06 = as.numeric(postposttest$PTF06)


table(postposttest$PTF07)
postposttest$PTF07=ifelse(postposttest$PTF07=="€33800"| postposttest$PTF07== 'AO01', 1, 0)
postposttest$PTF07 = as.numeric(postposttest$PTF07)

table(postposttest$PTF09)

postposttest$PTF09=ifelse(postposttest$PTF09=="Het nationaal gemiddeld loon in België" | postposttest$PTF09== 'AO03', 1, 0)
postposttest$PTF09 = as.numeric(postposttest$PTF09)

table(postposttest$PTF10)
postposttest$PTF10=ifelse(postposttest$PTF10=="37%"| postposttest$PTF10== 'AO01', 1, 0)
postposttest$PTF10 = as.numeric(postposttest$PTF10)

table(postposttest$PTF11)
# ANS='Bij dit getrapt progressief systeem zal het belastingtarief op de extra € 12.000 50% bedragen, omdat dit inkomen in de hoogste schijf valt. '
postposttest$PTF11=ifelse(grepl(pattern = "12.000 50% bedrage", postposttest$PTF11), 1, 0)
postposttest$PTF11 = as.numeric(postposttest$PTF11)


postposttest$Score = (postposttest$PTF01+postposttest$PTF02+postposttest$PTF06+postposttest$PTF07+postposttest$PTF09+postposttest$PTF10+postposttest$PTF11)/7

hist(postposttest$Score)


postposttest$Treatment_state_i = ifelse(postposttest$Q1=='1', 0, 1)


##### 6.3 Measure Emotional Attitudes ####
###### 6.3.1 Attitude and Motivation ####

postposttest$`AS01[SQ001]` = as.numeric(translate_responses(postposttest$`AS01[SQ001]`, translations_ls) )
postposttest$`AS01[SQ002]` = as.numeric(translate_responses(postposttest$`AS01[SQ002]`, translations_ls) )
postposttest$`AS01[SQ003]` = as.numeric(translate_responses(postposttest$`AS01[SQ003]`, translations_ls) )
postposttest$`AS01[SQ004]` = as.numeric(translate_responses(postposttest$`AS01[SQ004]`, translations_ls) )

postposttest$`Attitude and Motivation` =  (postposttest$`AS01[SQ001]` + postposttest$`AS01[SQ002]`+   
                                         postposttest$`AS01[SQ003]`+ postposttest$`AS01[SQ004]`  )/4

hist(postposttest$`Attitude and Motivation`)

###### 6.3.2 Learning Experience & User Experience ####

postposttest$`AS02[SQ001]` = as.numeric(translate_responses(postposttest$`AS02[SQ001]`, translations_ls) )
postposttest$`AS02[SQ002]` = as.numeric(translate_responses(postposttest$`AS02[SQ002]`, translations_ls) )
postposttest$`AS02[SQ003]` = as.numeric(translate_responses(postposttest$`AS02[SQ003]`, translations_ls) )



postposttest$`Learning Experience & User Experience` =  (postposttest$`AS02[SQ001]` + postposttest$`AS02[SQ002]`+   
                                                       postposttest$`AS02[SQ003]`   )/3

hist(postposttest$`Learning Experience & User Experience`)
###### 6.3.3 Self-Regulation & Metacognition ####
table(postposttest$`AS03[SQ001]` )
# Corresponds to Post-test C3 items (identical to pre-test F3)

postposttest$`AS03[SQ001]` = as.numeric(translate_responses(postposttest$`AS03[SQ001]`, translations_ls) )
postposttest$`AS03[SQ002]` = as.numeric(translate_responses(postposttest$`AS03[SQ002]`, translations_ls) )
postposttest$`AS03[SQ003]` = as.numeric(translate_responses(postposttest$`AS03[SQ003]`, translations_ls) )

postposttest$`Self-Regulation & Metacognition` =  (postposttest$`AS03[SQ001]` + postposttest$`AS03[SQ002]`+   postposttest$`AS03[SQ003]`   )/3

hist(postposttest$`Self-Regulation & Metacognition`)

###### 6.3.4 Engagement & Commitment ####
colnames(postposttest)

postposttest$`AS04[SQ001]` = as.numeric(translate_responses(postposttest$`AS04[SQ001]`, translations_ls) )
postposttest$`AS04[SQ002]` = as.numeric(translate_responses(postposttest$`AS04[SQ002]`, translations_ls) )
postposttest$`AS04[SQ003]` = as.numeric(translate_responses(postposttest$`AS04[SQ003]`, translations_ls) )

# postposttest$`AS04[SQ005]` = as.numeric(translate_responses(postposttest$`AS04[SQ005]`, translations_ls) )
postposttest$`Engagement & Commitment` =  (postposttest$`AS04[SQ001]` + postposttest$`AS04[SQ002]`+   
                                         postposttest$`AS04[SQ003]`)/3

hist(postposttest$`Engagement & Commitment`)

###### 6.3.5 Self-Confidence & Self-Efficacy ####

postposttest$`AS05[SQ001]` = as.numeric(translate_responses(postposttest$`AS05[SQ001]`, translations_ls) )
postposttest$`AS05[SQ002]` = as.numeric(translate_responses(postposttest$`AS05[SQ002]`, translations_ls) )
postposttest$`AS05[SQ003]` = as.numeric(translate_responses(postposttest$`AS05[SQ003]`, translations_ls) )

postposttest$`Self-Confidence & Self-Efficacy` =  (postposttest$`AS05[SQ001]` + postposttest$`AS05[SQ002]`  +
                                                 postposttest$`AS05[SQ003]` )/3
hist(postposttest$`Self-Confidence & Self-Efficacy` )
###### 6.3.6 Emotional & Psychological Factors ####
# Pre-test F6.1: "Ik ben soms zenuwachtig als ik "
# Pre-test F6.2: "Ik kijk graag uit naar de "

postposttest$`AS06[SQ001]` = as.numeric(translate_responses(postposttest$`AS06[SQ001]`, translations_ls) )
postposttest$`AS06[SQ002]` = as.numeric(translate_responses(postposttest$`AS06[SQ002]`, translations_ls) )
postposttest$`AS06[SQ003]` = as.numeric(translate_responses(postposttest$`AS06[SQ003]`, translations_ls) )


postposttest$`Emotional & Psychological Factors` =  (postposttest$`AS06[SQ001]` + postposttest$`AS06[SQ002]` +
                                                   postposttest$`AS06[SQ003]` )/3


postposttest$Q1 = as.character(postposttest$Q1)
#Create a new ID in the postposttest
# Id = Arm_gender_schoolname_studentname
postposttest$Q1 <- trimws(postposttest$Q1)
postposttest$G01Q03 <- trimws(postposttest$G01Q03)
postposttest$G01Q05 <- trimws(postposttest$G01Q05)
postposttest$G01Q01 <- trimws(postposttest$G01Q01)

postposttest$Q1 <- as.character(postposttest$Q1)
postposttest$G01Q03 <- as.character(postposttest$G01Q03)
postposttest$G01Q05 <- as.character(postposttest$G01Q05)
postposttest$G01Q01 <- as.character(postposttest$G01Q01)

# postposttest$G01Q01 <- iconv(postposttest$G01Q01, to = "ASCII//TRANSLIT")




#### 7. Pretest extra cleaning ####



# postposttest= postposttest[postposttest$Score!=0, ]

postposttest = postposttest[is.na(postposttest$Score)==F, ]
colnecesaries = colnames(postposttest)

out <- bind_rows(postposttest, posttest_out)

postposttest = out[, colnecesaries]

# a = postposttest[postposttest$flag_duplicate_id_score==1,]
# arrow::write_parquet(subset(postposttest,  postposttest$flag_duplicate_id_score==1), "postest_out2.parquet")
# 
# postposttest <- subset(postposttest,  postposttest$flag_duplicate_id_score==0)
# postposttest <- subset(postposttest,  is.na(postposttest$Score)==F)
# summary(postposttest$Score)
# 
# sd(postposttest$Score, na.rm = T)
# 
# 
# summary(postposttest$groupTime14859)
# 
# p1=quantile(postposttest$groupTime14859, probs = 0.05)
# p99=quantile(postposttest$groupTime14859, probs = 0.99)
# 
# postposttest$anormal_asnwering_flag= ifelse(
#   postposttest$groupTime14859>=p99 |
#     postposttest$groupTime14859<=p1 , 1, 0
# )
# table(postposttest$anormal_asnwering_flag)
# 
# postposttest = postposttest[postposttest$anormal_asnwering_flag==0,]

# ifelse(postposttest$Score<= 0.35 & postposttest$Score >= 0.33,1,0 )
# De la 28 a la 80



# postposttest = postposttest[postposttest$groupTime14859>59, ]
###### Last Cleaning for xjoin ####

postposttest$G01Q01 <- ifelse(postposttest$id==5, 
                              'WIKTOR_FRANCISZEK_BAZYLUK', 
                              postposttest$G01Q01)

postposttest$G01Q01 <- ifelse(postposttest$id==8, 
                              'MILAN_VERMEERSCH', 
                              postposttest$G01Q01)

postposttest$Q1 <- ifelse(postposttest$id==8, 
                              '3', 
                              postposttest$Q1)

postposttest$G01Q05 <- ifelse(postposttest$id==1147, 
                              'Broederschool Handel', 
                              postposttest$G01Q05)

postposttest$G01Q05 <- ifelse(postposttest$id==225, 
                              'Sintpaulus Instituut', 
                              postposttest$G01Q05)

#### ID Creation ####
postposttest$id_student <- paste0(as.character(postposttest$Q1), "_", postposttest$G01Q03, "_",
                                  postposttest$G01Q05,"_", postposttest$G01Q01)
# postposttest = anonymize_column(postposttest, "id_student")
postposttest$id_student2 <- paste0(
  as.character(postposttest$Q1), "_",
  tolower( substr( 
    gsub( " ", "", gsub("[[:punct:]\\s]", "", postposttest$G01Q05) ), 1, 8 ) ), "_",
  postposttest$G01Q01
)
postposttest = flag_last_duplicate(postposttest, "id_student2")

table(postposttest$flag_duplicate_id_score)
table(postposttest$flag_duplicate_id)
# 
postposttest = postposttest[postposttest$flag_duplicate_id==0,]
# postposttest = postposttest[postposttest$flag_duplicate_id_score==0,]