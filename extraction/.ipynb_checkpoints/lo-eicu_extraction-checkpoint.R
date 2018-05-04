###############################################################################
# Lactate Discordance: R Based Extraction Using dbplyr & dplyr
#
# Practicum Project: This is a preliminary form of this project created as 
# part of my Harvard Chan practcium project; part of the goal of this script
# was to try out dbplyr and dplyr for database extraction instead of generating
# materialized views in SQL and then pulling them into tidyverse or pandas
# depending on approach. Future work will examine more laboratory variables
# and may switch to a full SQL extraction.
#
# Written by C.V. Cosgriff, MIT Critical Data
# 3/2018
#
# This script extracts the features we planned to examine for the lactate 
# outlier discordance project using a combination of dbplyr & dplyr.
#
###############################################################################

library(RPostgreSQL)
library(dplyr)
library(tidyr)
library(stringr)
library(dbplyr)

# Yes, I know this password is here, but I'm not concerned about this as its a
# very generic password, the server is a VM behind a firewall, and the database
# can only be connected to via trusted hosts.
con <- DBI::dbConnect(drv = DBI::dbDriver("PostgreSQL"), dbname = "eicu", 
                      host = "192.168.0.103", port = 5432, user = "mimicuser",
                      password = "harvardmit2018")


# Extraction steps
# 1. ICU stays / demo data
# 2. Vital summaries
# 3. Lab summaries
# 4. Vent status
# 5. Pressors
# 6. APACHE IVa
# 7. Diagnosis group
#
# Note 1: We are only using data from first 24hr
# Note 2: There are 1,377 admissions where the ICU admission happens much before 
# hospital; excluding for now since I believe they're an error in data entry.
# Note 3: some of the summary functions below are wrapped in "suppressWarnings"
# because patients who had absolutly no values for a certain variable trigger
# a warning. Because there are so many of these the warnings can overwhelm the
# output buffer and make RStudio bug, and so they are suppressed for 
# performance.

# Cohort
cohort_df <- tbl(con, in_schema("eicu_crd", "patient")) %>% 
  filter(hospitaladmitoffset <= 0) %>% 
  collect() %>% 
  arrange(desc(hospitaladmitoffset)) %>%
  distinct(patienthealthsystemstayid, .keep_all = T) %>% 
  filter(age != "> 89") %>% 
  mutate(age = as.numeric(age), male = as.numeric(ifelse(gender == "Male", 1, 0))) %>%
  filter(age >= 16 & age <= 89) %>% 
  select(patientunitstayid, age, male, hospitaladmitoffset)

# Vitals
vitals_df <- tbl(con, in_schema("eicu_crd", "vitalperiodic")) %>% 
  filter(observationoffset <= 1440) %>% select(patientunitstayid,
                                               hr = heartrate,
                                               rr = respiration,
                                               spo2 = sao2,
                                               sbp = systemicsystolic,
                                               dbp = systemicdiastolic, 
                                               map = systemicmean,
                                               temp = temperature) %>%
  collect() %>%
  group_by(patientunitstayid) %>%
  summarise(min_hr = suppressWarnings(min(hr, na.rm = T)), 
            max_hr = suppressWarnings(max(hr, na.rm = T)), 
            mean_hr = mean(hr, na.rm = T), 
            min_rr = suppressWarnings(min(rr, na.rm = T)), 
            max_rr = suppressWarnings(max(rr, na.rm = T)), 
            mean_rr = mean(rr, na.rm = T),
            min_spo2 = suppressWarnings(min(spo2, na.rm = T)), 
            max_spo2 = suppressWarnings(max(spo2, na.rm = T)),
            mean_spO2 = mean(spo2, na.rm = T),
            min_sbp = suppressWarnings(min(sbp, na.rm = T)),
            max_sbp = suppressWarnings(max(sbp, na.rm = T)), 
            mean_sbp = mean(sbp, na.rm = T), 
            min_dbp = suppressWarnings(min(dbp, na.rm = T)), 
            max_dbp = suppressWarnings(max(dbp, na.rm = T)), 
            mean_dbp = mean(dbp, na.rm = T),
            min_map = suppressWarnings(min(map, na.rm = T)), 
            max_map = suppressWarnings(max(map, na.rm = T)), 
            mean_map = mean(map, na.rm = T), 
            min_temp = suppressWarnings(min(temp, na.rm = T)), 
            max_temp = suppressWarnings(max(temp, na.rm = T)), 
            mean_temp = mean(temp, na.rm = T)) %>%
  mutate_all(funs(replace(., is.infinite(.), NA))) %>%
  mutate_all(funs(replace(., is.nan(.), NA))) %>% ungroup()

# Labs of interest
lab_df <- tbl(con, in_schema("eicu_crd", "lab")) %>% 
  filter(labresultoffset <= 1440) %>%
  select(patientunitstayid, labid, labname, labresult, labmeasurenamesystem) %>%
  collect()
lab_ext <- c("anion gap", "bicarbonate", "creatinine", "glucose", 
             "platelets x 1000", "PTT", "PT", "PT - INR", "sodium", "BUN", 
             "WBC x 1000", "lactate")
lab_df <- lab_df %>% filter(labname %in% lab_ext)

lab_df$labname[lab_df$labname == "anion gap"] <- "anion_gap"
lab_df$labname[lab_df$labname == "platelets x 1000"] <- "platelets"
lab_df$labname[lab_df$labname == "PT - INR"] <- "inr"
lab_df$labname[lab_df$labname == "PT"] <- "pt"
lab_df$labname[lab_df$labname == "PTT"] <- "ptt"
lab_df$labname[lab_df$labname == "BUN"] <- "bun"
lab_df$labname[lab_df$labname == "WBC x 1000"] <- "wbc"

lab_df <- lab_df %>% 
  spread(key = "labname", value = "labresult", fill = NA) %>%
  select(-labid, -labmeasurenamesystem) %>%
  group_by(patientunitstayid) %>%
  summarise(min_bicarbonate = suppressWarnings(min(bicarbonate, na.rm = T)),
            max_bicarbonate = suppressWarnings(max(bicarbonate, na.rm = T)),
            mean_bicarbonate = mean(bicarbonate, na.rm = T),
            min_bun = suppressWarnings(min(bun, na.rm = T)),
            max_bun = suppressWarnings(max(bun, na.rm = T)),
            mean_bun = mean(bun, na.rm = T),
            min_creatinine = suppressWarnings(min(creatinine, na.rm = T)),
            max_creatinine = suppressWarnings(max(creatinine, na.rm = T)),
            mean_creatinine = mean(creatinine, na.rm = T),
            min_glucose = suppressWarnings(min(glucose, na.rm = T)),
            max_glucose = suppressWarnings(max(glucose, na.rm = T)),
            mean_glucose = mean(glucose, na.rm = T),
            min_platelets = suppressWarnings(min(platelets, na.rm = T)),
            max_platelets = suppressWarnings(max(platelets, na.rm = T)),
            mean_platelets = mean(platelets, na.rm = T),
            min_wbc = suppressWarnings(min(wbc, na.rm = T)),
            max_wbc = suppressWarnings(max(wbc, na.rm = T)),
            mean_wbc = mean(wbc, na.rm = T),
            min_sodium = suppressWarnings(min(sodium, na.rm = T)),
            max_sodium = suppressWarnings(max(sodium, na.rm = T)),
            mean_sodium = mean(sodium, na.rm = T),
            min_pt = suppressWarnings(min(pt, na.rm = T)),
            max_pt = suppressWarnings(max(pt, na.rm = T)),
            mean_pt = mean(pt, na.rm = T),
            min_ptt = suppressWarnings(min(ptt, na.rm = T)),
            max_ptt = suppressWarnings(max(ptt, na.rm = T)),
            mean_ptt = mean(ptt, na.rm = T),
            min_inr = suppressWarnings(min(inr, na.rm = T)),
            max_inr = suppressWarnings(max(inr, na.rm = T)),
            mean_inr = mean(inr, na.rm = T),
            min_lactate = suppressWarnings(min(lactate, na.rm = T)),
            max_lactate = suppressWarnings(max(lactate, na.rm = T)),
            mean_lactate = mean(lactate, na.rm = T)) %>%
  mutate_all(funs(replace(., is.infinite(.), NA))) %>%
  mutate_all(funs(replace(., is.nan(.), NA))) %>% ungroup()

# Mechanical Ventilation
mv_df <- tbl(con, in_schema("eicu_crd", "apachepredvar")) %>% 
  select(patientunitstayid, oobintubday1) %>% collect()

# Pressors
inf_df <- tbl(con, in_schema("eicu_crd", "infusiondrug")) %>%
  filter(infusionoffset <= 1440) %>% 
  select(patientunitstayid, infusiondrugid, drugname) %>% 
  collect()
pressors_ext <- c("norepinephrine", "epinephrine", "phenylephrine", "vasopressin", 
                  "dopamine", "dobutamine", "milrinone")
inf_df <- inf_df %>% filter(str_detect(drugname, regex(pressors_ext[1], ignore_case = T)) |
                              str_detect(drugname, regex(pressors_ext[2], ignore_case = T)) |
                              str_detect(drugname, regex(pressors_ext[3], ignore_case = T)) |
                              str_detect(drugname, regex(pressors_ext[4], ignore_case = T)) |
                              str_detect(drugname, regex(pressors_ext[5], ignore_case = T)) |
                              str_detect(drugname, regex(pressors_ext[6], ignore_case = T)) |
                              str_detect(drugname, regex(pressors_ext[7], ignore_case = T)))
vasopressor_df <- tibble(patientunitstayid = cohort_df$patientunitstayid,
                         vasopressor_status = ifelse(patientunitstayid %in% inf_df$patientunitstayid, 1, 0))

# Apache IVa
apache_df <- tbl(con, in_schema("eicu_crd", "apachepatientresult")) %>%
  filter(apacheversion == "IVa") %>% 
  select(patientunitstayid, apachescore) %>%
  collect()

# Diagnosis groupings: we first use regular expressions to grab all of the 
# diagnosis strings that define the groups. The groups are lossley based on the
# Elixhauser groupings but do not fully recapitulate them and are really here
# with a differnt purpose. We drop strings that include "negating" words based 
# on EDA which showed there weren't many, but that when they were present it
# often meant the opposite of the search term
dx_tbl <- tbl(con, in_schema("eicu_crd", "diagnosis")) %>% collect()

# Copy over the queries generated during the diagnoses table exploration
chf_tbl <- dx_tbl %>% filter(str_detect(tolower(diagnosisstring), "congestive") |
                               str_detect(tolower(diagnosisstring), "heart failure") |
                               str_detect(tolower(diagnosisstring), "chf")) %>%
  mutate(negation = as.numeric(str_detect(tolower(diagnosisstring), "not") |
                                 str_detect(tolower(diagnosisstring), "without") |
                                 str_detect(tolower(diagnosisstring), "absence") |
                                 str_detect(tolower(diagnosisstring), "unassociated"))) %>%
  group_by(diagnosisstring, negation) %>% 
  summarise(count = n()) %>%
  filter(negation == 0)

arr_tbl <- dx_tbl %>% filter(str_detect(tolower(diagnosisstring), "arrhythmias")) %>%
  mutate(negation = as.numeric(str_detect(tolower(diagnosisstring), "not") |
                                 str_detect(tolower(diagnosisstring), "without") |
                                 str_detect(tolower(diagnosisstring), "absence") |
                                 str_detect(tolower(diagnosisstring), "unassociated"))) %>%
  group_by(diagnosisstring, negation) %>% 
  summarise(count = n()) %>%
  filter(negation == 0)

vlv_tbl <- dx_tbl %>% filter(str_detect(tolower(diagnosisstring), "valve") |
                               str_detect(tolower(diagnosisstring), "valvular")) %>%
  filter(!str_detect(tolower(diagnosisstring), "endocarditis")) %>%
  mutate(negation = as.numeric(str_detect(tolower(diagnosisstring), "not") |
                                 str_detect(tolower(diagnosisstring), "without") |
                                 str_detect(tolower(diagnosisstring), "absence") |
                                 str_detect(tolower(diagnosisstring), "unassociated"))) %>%
  group_by(diagnosisstring, negation) %>% 
  summarise(count = n()) %>%
  filter(negation == 0)

pulmcirc_tbl <- dx_tbl %>% filter(str_detect(tolower(diagnosisstring), "pulmonary hypertension") |
                                    str_detect(tolower(diagnosisstring), "pulmonary htn") |
                                    str_detect(tolower(diagnosisstring), "cor pulmonale")) %>%
  mutate(negation = as.numeric(str_detect(tolower(diagnosisstring), "not") |
                                 str_detect(tolower(diagnosisstring), "without") |
                                 str_detect(tolower(diagnosisstring), "absence") |
                                 str_detect(tolower(diagnosisstring), "unassociated"))) %>%
  group_by(diagnosisstring, negation) %>% 
  summarise(count = n()) %>%
  arrange(desc(negation, count)) %>%
  filter(negation == 0)

par_tbl <- dx_tbl %>% filter(str_detect(tolower(diagnosisstring), "paraplegia") |
                               str_detect(tolower(diagnosisstring), "quadriplegia") |
                               str_detect(tolower(diagnosisstring), "hemiplegia")) %>%
  mutate(negation = as.numeric(str_detect(tolower(diagnosisstring), "not") |
                                 str_detect(tolower(diagnosisstring), "without") |
                                 str_detect(tolower(diagnosisstring), "absence") |
                                 str_detect(tolower(diagnosisstring), "unassociated"))) %>%
  group_by(diagnosisstring, negation) %>% 
  summarise(count = n()) %>%
  filter(negation == 0)

ren_tbl <- dx_tbl %>% filter(str_detect(tolower(diagnosisstring), "renal failure") |
                               str_detect(tolower(diagnosisstring), "kidney disease")) %>%
  mutate(negation = as.numeric(str_detect(tolower(diagnosisstring), "not") |
                                 str_detect(tolower(diagnosisstring), "without") |
                                 str_detect(tolower(diagnosisstring), "absence") |
                                 str_detect(tolower(diagnosisstring), "unassociated"))) %>%
  group_by(diagnosisstring, negation) %>% 
  summarise(count = n()) %>%
  filter(negation == 0)

pulm_tbl <- dx_tbl %>% filter(
  (str_detect(tolower(diagnosisstring), "chronic") & str_detect(tolower(diagnosisstring), "respiratory") & !str_detect(tolower(diagnosisstring), "renal")) |
    (str_detect(tolower(diagnosisstring), "chronic") & str_detect(tolower(diagnosisstring), "bronchitis")) |
    (str_detect(tolower(diagnosisstring), "emphysema") & !str_detect(tolower(diagnosisstring), "gastrointestinal")) |
    str_detect(tolower(diagnosisstring), "bronchiectasis")
) %>%
  group_by(diagnosisstring) %>% 
  summarise(count = n())

pvd_tbl <- dx_tbl %>% filter(str_detect(tolower(diagnosisstring), "atherosclerosis") |
                               str_detect(tolower(diagnosisstring), "peripheral vascular") |
                               str_detect(tolower(diagnosisstring), "thromboangitis") |
                               str_detect(tolower(diagnosisstring), "aneurysm") & 
                               !str_detect(tolower(diagnosisstring), "neuro")) %>%
  group_by(diagnosisstring) %>% 
  summarise(count = n())

dm_tbl <- dx_tbl %>% filter((str_detect(tolower(diagnosisstring), "diabetes") |
                               str_detect(tolower(diagnosisstring), "diabetic")) &
                              !str_detect(tolower(diagnosisstring), "insipidus")) %>%
  group_by(diagnosisstring) %>% 
  summarise(count = n())

lvrDz_tbl <- dx_tbl %>% filter(str_detect(tolower(diagnosisstring), "hepatitis") |
                                 str_detect(tolower(diagnosisstring), "cirrhosis") |
                                 str_detect(tolower(diagnosisstring), "esophageal varices") |
                                 (str_detect(tolower(diagnosisstring), "liver") & str_detect(tolower(diagnosisstring), "transplant"))) %>%
  mutate(negation = as.numeric(str_detect(tolower(diagnosisstring), "not") |
                                 str_detect(tolower(diagnosisstring), "without") |
                                 str_detect(tolower(diagnosisstring), "absence") |
                                 str_detect(tolower(diagnosisstring), "unassociated"))) %>%
  group_by(diagnosisstring, negation) %>% 
  summarise(count = n()) %>%
  filter(negation == 0)

hypothy_tbl <- dx_tbl %>% filter(str_detect(tolower(diagnosisstring), "hypothyroid")) %>%
  mutate(negation = as.numeric(str_detect(tolower(diagnosisstring), "not") |
                                 str_detect(tolower(diagnosisstring), "without") |
                                 str_detect(tolower(diagnosisstring), "absence") |
                                 str_detect(tolower(diagnosisstring), "unassociated"))) %>%
  group_by(diagnosisstring, negation) %>% 
  summarise(count = n()) %>%
  filter(negation == 0)

pud_tbl <- dx_tbl %>% filter(
  (str_detect(tolower(diagnosisstring), "gastric") & str_detect(tolower(diagnosisstring), "ulcer")) |
    (str_detect(tolower(diagnosisstring), "duodenal") & str_detect(tolower(diagnosisstring), "ulcer")) |
    (str_detect(tolower(diagnosisstring), "gastrojejunal") & str_detect(tolower(diagnosisstring), "ulcer")) |
    str_detect(tolower(diagnosisstring), "peptic ulcer disease")) %>%
  mutate(negation = as.numeric(str_detect(tolower(diagnosisstring), "not") |
                                 str_detect(tolower(diagnosisstring), "without") |
                                 str_detect(tolower(diagnosisstring), "absence") |
                                 str_detect(tolower(diagnosisstring), "unassociated"))) %>%
  group_by(diagnosisstring, negation) %>% 
  summarise(count = n()) %>%
  filter(negation == 0)

ond_tbl <- dx_tbl %>% filter(str_detect(tolower(diagnosisstring), "demyelination") |
                               str_detect(tolower(diagnosisstring), "degeneration") |
                               str_detect(tolower(diagnosisstring), "hydrocephalus") |
                               str_detect(tolower(diagnosisstring), "multiple sclerosis") |
                               str_detect(tolower(diagnosisstring), "seizure")) %>%
  mutate(negation = as.numeric(str_detect(tolower(diagnosisstring), "not") |
                                 str_detect(tolower(diagnosisstring), "without") |
                                 str_detect(tolower(diagnosisstring), "absence") |
                                 str_detect(tolower(diagnosisstring), "unassociated"))) %>%
  group_by(diagnosisstring, negation) %>% 
  summarise(count = n()) %>%
  filter(negation == 0)

htn_tbl <- dx_tbl %>% filter((!str_detect(tolower(diagnosisstring), "neurologic") &
                                !str_detect(tolower(diagnosisstring), "pulmonary")) &
                               str_detect(tolower(diagnosisstring), "hypertension")) %>%
  mutate(negation = as.numeric(str_detect(tolower(diagnosisstring), "not") |
                                 str_detect(tolower(diagnosisstring), "without") |
                                 str_detect(tolower(diagnosisstring), "absence") |
                                 str_detect(tolower(diagnosisstring), "unassociated"))) %>%
  group_by(diagnosisstring, negation) %>% 
  summarise(count = n()) %>%
  filter(negation == 0)

hiv_tbl <- dx_tbl %>% filter(str_detect(tolower(diagnosisstring), "hiv") |
                               str_detect(tolower(diagnosisstring), "aids")) %>%
  mutate(negation = as.numeric(str_detect(tolower(diagnosisstring), "not") |
                                 str_detect(tolower(diagnosisstring), "without") |
                                 str_detect(tolower(diagnosisstring), "absence") |
                                 str_detect(tolower(diagnosisstring), "unassociated"))) %>%
  group_by(diagnosisstring, negation) %>% 
  summarise(count = n()) %>%
  filter(negation == 0)

lym_tbl <- dx_tbl %>% filter(str_detect(tolower(diagnosisstring), "lymphoma") |
                               str_detect(tolower(diagnosisstring), "leukemia")) %>%
  mutate(negation = as.numeric(str_detect(tolower(diagnosisstring), "not") |
                                 str_detect(tolower(diagnosisstring), "without") |
                                 str_detect(tolower(diagnosisstring), "absence") |
                                 str_detect(tolower(diagnosisstring), "unassociated"))) %>%
  group_by(diagnosisstring, negation) %>% 
  summarise(count = n()) %>%
  filter(negation == 0)

met_tbl <- dx_tbl %>% filter(str_detect(tolower(diagnosisstring), "metastasis") |
                               str_detect(tolower(diagnosisstring), "metastases") |
                               str_detect(tolower(diagnosisstring), "metastatic")) %>%
  mutate(negation = as.numeric(str_detect(tolower(diagnosisstring), "not") |
                                 str_detect(tolower(diagnosisstring), "without") |
                                 str_detect(tolower(diagnosisstring), "absence") |
                                 str_detect(tolower(diagnosisstring), "unassociated"))) %>%
  group_by(diagnosisstring, negation) %>% 
  summarise(count = n()) %>%
  filter(negation == 0)

ca_tbl <- dx_tbl %>% filter(!str_detect(tolower(diagnosisstring), "metastatic") &
                              (str_detect(tolower(diagnosisstring), "cancer") |
                                 str_detect(tolower(diagnosisstring), "carcinoma") |
                                 str_detect(tolower(diagnosisstring), "sarcoma") |
                                 str_detect(tolower(diagnosisstring), "neoplasm") |
                                 str_detect(tolower(diagnosisstring), "tumor"))) %>%
  mutate(negation = as.numeric(str_detect(tolower(diagnosisstring), "not") |
                                 str_detect(tolower(diagnosisstring), "without") |
                                 str_detect(tolower(diagnosisstring), "absence") |
                                 str_detect(tolower(diagnosisstring), "unassociated"))) %>%
  group_by(diagnosisstring, negation) %>% 
  summarise(count = n()) %>%
  filter(negation == 0)

racvd_tbl <- dx_tbl %>% filter(str_detect(tolower(diagnosisstring), "rheumatoid") |
                                 str_detect(tolower(diagnosisstring), "postrheumatic") |
                                 str_detect(tolower(diagnosisstring), "scleroderma") |
                                 str_detect(tolower(diagnosisstring), "systemic sclerosis") |
                                 str_detect(tolower(diagnosisstring), "polymyositis") |
                                 str_detect(tolower(diagnosisstring), "dermatomyositis") |
                                 str_detect(tolower(diagnosisstring), "felty") |
                                 str_detect(tolower(diagnosisstring), "enthesopathy") |
                                 str_detect(tolower(diagnosisstring), "sacroiliitis") |
                                 str_detect(tolower(diagnosisstring), "polymyalgia") |
                                 str_detect(tolower(diagnosisstring), "eosinophilia myalgia")) %>%
  mutate(negation = as.numeric(str_detect(tolower(diagnosisstring), "not") |
                                 str_detect(tolower(diagnosisstring), "without") |
                                 str_detect(tolower(diagnosisstring), "absence") |
                                 str_detect(tolower(diagnosisstring), "unassociated"))) %>%
  group_by(diagnosisstring, negation) %>% 
  summarise(count = n()) %>%
  filter(negation == 0)

coag_tbl <- dx_tbl %>% filter(str_detect(tolower(diagnosisstring), "factor ix") |
                                str_detect(tolower(diagnosisstring), "factor xi") |
                                str_detect(tolower(diagnosisstring), "hemophilia") |
                                str_detect(tolower(diagnosisstring), "clotting") |
                                str_detect(tolower(diagnosisstring), "von willebran") |
                                str_detect(tolower(diagnosisstring), "defibrination") |
                                str_detect(tolower(diagnosisstring), "factor deficiency") |
                                str_detect(tolower(diagnosisstring), "coagulation defect") |
                                str_detect(tolower(diagnosisstring), "acquired anticoag") |
                                str_detect(tolower(diagnosisstring), "platelet defect") |
                                str_detect(tolower(diagnosisstring), "thrombocytopenia")) %>%
  mutate(negation = as.numeric(str_detect(tolower(diagnosisstring), "not") |
                                 str_detect(tolower(diagnosisstring), "without") |
                                 str_detect(tolower(diagnosisstring), "absence") |
                                 str_detect(tolower(diagnosisstring), "unassociated"))) %>%
  group_by(diagnosisstring, negation) %>% 
  summarise(count = n()) %>%
  filter(negation == 0)

obesity_tbl <- dx_tbl %>% filter(str_detect(tolower(diagnosisstring), "obesity") |
                                   str_detect(tolower(diagnosisstring), "obese")) %>%
  mutate(negation = as.numeric(str_detect(tolower(diagnosisstring), "not") |
                                 str_detect(tolower(diagnosisstring), "without") |
                                 str_detect(tolower(diagnosisstring), "absence") |
                                 str_detect(tolower(diagnosisstring), "unassociated"))) %>%
  group_by(diagnosisstring, negation) %>% 
  summarise(count = n()) %>%
  filter(negation == 0)

wtloss_tbl <- dx_tbl %>% filter(str_detect(tolower(diagnosisstring), "kwashiorkor") |
                                  str_detect(tolower(diagnosisstring), "marasmus") |
                                  str_detect(tolower(diagnosisstring), "malnutrition")) %>%
  mutate(negation = as.numeric(str_detect(tolower(diagnosisstring), "not") |
                                 str_detect(tolower(diagnosisstring), "without") |
                                 str_detect(tolower(diagnosisstring), "absence") |
                                 str_detect(tolower(diagnosisstring), "unassociated"))) %>%
  group_by(diagnosisstring, negation) %>% 
  summarise(count = n()) %>%
  filter(negation == 0)

elec_tbl <- dx_tbl %>% filter(str_detect(tolower(diagnosisstring), "hypoosmol") |
                                str_detect(tolower(diagnosisstring), "hyponatremia") |
                                str_detect(tolower(diagnosisstring), "acidosis") |
                                str_detect(tolower(diagnosisstring), "alkalosis") |
                                str_detect(tolower(diagnosisstring), "hyperpotassemia") |
                                str_detect(tolower(diagnosisstring), "hypopotassemia") |
                                str_detect(tolower(diagnosisstring), "hypernatremia")) %>%
  mutate(negation = as.numeric(str_detect(tolower(diagnosisstring), "not") |
                                 str_detect(tolower(diagnosisstring), "without") |
                                 str_detect(tolower(diagnosisstring), "absence") |
                                 str_detect(tolower(diagnosisstring), "unassociated"))) %>%
  group_by(diagnosisstring, negation) %>% 
  summarise(count = n()) %>%
  filter(negation == 0)

bla_tbl <- dx_tbl %>% filter(str_detect(tolower(diagnosisstring), "blood loss")) %>%
  mutate(negation = as.numeric(str_detect(tolower(diagnosisstring), "not") |
                                 str_detect(tolower(diagnosisstring), "without") |
                                 str_detect(tolower(diagnosisstring), "absence") |
                                 str_detect(tolower(diagnosisstring), "unassociated"))) %>%
  group_by(diagnosisstring, negation) %>% 
  summarise(count = n()) %>%
  filter(negation == 0)

defanem_tbl <- dx_tbl %>% filter(str_detect(tolower(diagnosisstring), "anemia") &
                                   !str_detect(tolower(diagnosisstring), "blood loss") &
                                   !str_detect(tolower(diagnosisstring), "oncology")) %>%
  mutate(negation = as.numeric(str_detect(tolower(diagnosisstring), "not") |
                                 str_detect(tolower(diagnosisstring), "without") |
                                 str_detect(tolower(diagnosisstring), "absence") |
                                 str_detect(tolower(diagnosisstring), "unassociated"))) %>%
  group_by(diagnosisstring, negation) %>% 
  summarise(count = n()) %>%
  filter(negation == 0)

aa_tbl <- dx_tbl %>% filter(str_detect(tolower(diagnosisstring), "alcohol") |
                              str_detect(tolower(diagnosisstring), "etoh abuse") |
                              (str_detect(tolower(diagnosisstring), "ethanol") &
                                 !str_detect(tolower(diagnosisstring), "methanol"))) %>%
  mutate(negation = as.numeric(str_detect(tolower(diagnosisstring), "not") |
                                 str_detect(tolower(diagnosisstring), "without") |
                                 str_detect(tolower(diagnosisstring), "absence") |
                                 str_detect(tolower(diagnosisstring), "unassociated"))) %>%
  group_by(diagnosisstring, negation) %>% 
  summarise(count = n()) %>%
  filter(negation == 0)

da_tbl <- dx_tbl %>% filter(str_detect(tolower(diagnosisstring), "withdrawal") &
                              !str_detect(tolower(diagnosisstring), "alcohol") &
                              !str_detect(tolower(diagnosisstring), "from etoh") &
                              !str_detect(tolower(diagnosisstring), "nicotine")) %>%
  mutate(negation = as.numeric(str_detect(tolower(diagnosisstring), "not") |
                                 str_detect(tolower(diagnosisstring), "without") |
                                 str_detect(tolower(diagnosisstring), "absence") |
                                 str_detect(tolower(diagnosisstring), "unassociated"))) %>%
  group_by(diagnosisstring, negation) %>% 
  summarise(count = n()) %>%
  filter(negation == 0)

psycho_tbl <- dx_tbl %>% filter(str_detect(tolower(diagnosisstring), "psycho") |
                                  str_detect(tolower(diagnosisstring), "schizo")) %>%
  mutate(negation = as.numeric(str_detect(tolower(diagnosisstring), "not") |
                                 str_detect(tolower(diagnosisstring), "without") |
                                 str_detect(tolower(diagnosisstring), "absence") |
                                 str_detect(tolower(diagnosisstring), "unassociated"))) %>%
  group_by(diagnosisstring, negation) %>% 
  summarise(count = n()) %>%
  filter(negation == 0)

dep_tbl <- dx_tbl %>% filter(str_detect(tolower(diagnosisstring), "depression") &
                               !str_detect(tolower(diagnosisstring), "respiratory")) %>%
  mutate(negation = as.numeric(str_detect(tolower(diagnosisstring), "not") |
                                 str_detect(tolower(diagnosisstring), "without") |
                                 str_detect(tolower(diagnosisstring), "absence") |
                                 str_detect(tolower(diagnosisstring), "unassociated"))) %>%
  group_by(diagnosisstring, negation) %>% 
  summarise(count = n()) %>%
  filter(negation == 0)

# Withe strings constructed, we then use them to create the diagnostic groups
# based on the first 24 hour diagnoses.
dx_tbl <- dx_tbl %>% filter(diagnosisoffset <= 1440)
dx_group <- c("chf", "arrhythmia", "valvdz", "pulmcirc", "pvd", "dm", 
              "renalfail", "liverdz", "hypothyroidism", "pud", "neuro", "pulm", 
              "htn", "paralysis", "hiv", "lymphoma", "mets", "solidca", "cvd", 
              "coagdef", "obesity", "wtloss", "electrolyte", "blanemia", 
              "defanemia", "drugabuse", "etohabuse", "psychoses", "depression")
dx_group <- as.factor(dx_group)

dx_tbl <- dx_tbl %>% mutate(
  chf = as.numeric(diagnosisstring %in% chf_tbl$diagnosisstring),
  arryhthmia = as.numeric(diagnosisstring %in% arr_tbl$diagnosisstring),
  valvdz = as.numeric(diagnosisstring %in% vlv_tbl$diagnosisstring),
  pulmcirc = as.numeric(diagnosisstring %in% pulmcirc_tbl$diagnosisstring),
  pvd = as.numeric(diagnosisstring %in% pvd_tbl$diagnosisstring),
  dm = as.numeric(diagnosisstring %in% dm_tbl$diagnosisstring),
  renalfail = as.numeric(diagnosisstring %in% ren_tbl$diagnosisstring),
  liverdz = as.numeric(diagnosisstring %in% lvrDz_tbl$diagnosisstring),
  hypothyroid = as.numeric(diagnosisstring %in% hypothy_tbl$diagnosisstring),
  pud = as.numeric(diagnosisstring %in% pud_tbl$diagnosisstring),
  neuro = as.numeric(diagnosisstring %in% ond_tbl$diagnosisstring),
  pulm = as.numeric(diagnosisstring %in% pulm_tbl$diagnosisstring),
  htn = as.numeric(diagnosisstring %in% htn_tbl$diagnosisstring),
  paralysis = as.numeric(diagnosisstring %in% par_tbl$diagnosisstring),
  hiv = as.numeric(diagnosisstring %in% hiv_tbl$diagnosisstring),
  lymphoma = as.numeric(diagnosisstring %in% lym_tbl$diagnosisstring),
  mets = as.numeric(diagnosisstring %in% met_tbl$diagnosisstring),
  solidca = as.numeric(diagnosisstring %in% ca_tbl$diagnosisstring),
  cvd = as.numeric(diagnosisstring %in% racvd_tbl$diagnosisstring),
  coagdef = as.numeric(diagnosisstring %in% coag_tbl$diagnosisstring),
  obesity = as.numeric(diagnosisstring %in% obesity_tbl$diagnosisstring),
  wtloss = as.numeric(diagnosisstring %in% wtloss_tbl$diagnosisstring),
  electrolyte = as.numeric(diagnosisstring %in% elec_tbl$diagnosisstring),
  blanemia = as.numeric(diagnosisstring %in% bla_tbl$diagnosisstring),
  defanemia = as.numeric(diagnosisstring %in% defanem_tbl$diagnosisstring),
  drugabuse = as.numeric(diagnosisstring %in% da_tbl$diagnosisstring),
  etohabuse = as.numeric(diagnosisstring %in% aa_tbl$diagnosisstring),
  psychoses = as.numeric(diagnosisstring %in% psycho_tbl$diagnosisstring),
  depression = as.numeric(diagnosisstring %in% dep_tbl$diagnosisstring)
) %>%
  mutate(solidca = ifelse(mets == 1, 0, solidca)) %>%
  group_by(patientunitstayid) %>%
  summarise(
    chf = as.numeric(any(chf == 1)),
    arryhthmia = as.numeric(any(arryhthmia == 1)),
    valvdz = as.numeric(any(valvdz == 1)),
    pulmcirc = as.numeric(any(pulmcirc == 1)),
    pvd = as.numeric(any(pvd == 1)),
    dm = as.numeric(any(dm == 1)),
    renalfail = as.numeric(any(renalfail == 1)),
    liverdz = as.numeric(any(liverdz == 1)),
    hypothyroid = as.numeric(any(hypothyroid == 1)),
    pud = as.numeric(any(pud == 1)),
    neuro = as.numeric(any(neuro == 1)),
    pulm = as.numeric(any(pulm == 1)),
    htn = as.numeric(any(htn == 1)),
    paralysis = as.numeric(any(paralysis == 1)),
    hiv = as.numeric(any(hiv == 1)),
    lymphoma = as.numeric(any(lymphoma == 1)),
    mets = as.numeric(any(mets == 1)),
    solidca = as.numeric(any(solidca == 1)),
    cvd = as.numeric(any(cvd == 1)),
    coagdef = as.numeric(any(coagdef == 1)),
    obesity = as.numeric(any(obesity == 1)),
    wtloss = as.numeric(any(wtloss == 1)),
    electrolyte = as.numeric(any(electrolyte == 1)),
    blanemia = as.numeric(any(blanemia == 1)),
    defanemia = as.numeric(any(defanemia == 1)),
    drugabuse = as.numeric(any(drugabuse == 1)),
    etohabuse = as.numeric(any(etohabuse == 1)),
    psychoses = as.numeric(any(psychoses == 1)),
    depression = as.numeric(any(depression == 1))
  )

# Finally, we join all the extracted tables
lo_tbl <- inner_join(cohort_df, vitals_df, by = "patientunitstayid") %>%
  inner_join(lab_df, by = "patientunitstayid") %>%
  inner_join(mv_df, by = "patientunitstayid") %>%
  inner_join(vasopressor_df, by = "patientunitstayid") %>%
  inner_join(apache_df, by = "patientunitstayid") %>%
  inner_join(dx_tbl, by = "patientunitstayid")

# Output the result as a csv file
write.csv(lo_tbl, "lo-data.csv")