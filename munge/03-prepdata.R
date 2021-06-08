pdata <- pdata %>%
  mutate(
    num_Ef = coalesce(num_dcEf, num_opEf),
    num_Ef_cat = factor(case_when(
      is.na(num_Ef) ~ 0,
      num_Ef < 40 ~ 1,
      num_Ef <= 49 ~ 2,
      num_Ef >= 50 ~ 3
    ),
    levels = 0:3, labels = c("missing", "<40%", "40-49%", ">=50%")
    ),
    num_Ef_cat2 = factor(case_when(
      is.na(num_Ef) ~ 0,
      num_Ef < 41 ~ 1,
      num_Ef <= 49 ~ 2,
      num_Ef >= 50 ~ 3
    ),
    levels = 0:3, labels = c("missing", "<41%", "41-49%", ">=50%")
    ),
    num_Ef_cat3 = factor(case_when(
      num_Ef >= 35 & num_Ef <= 39 ~ 1,
      num_Ef == 40 ~ 2,
      num_Ef >= 41 & num_Ef <= 45 ~ 3
    ),
    levels = 1:3, labels = c("35-39%", "40%", "41-45%")
    ),
    num_Ef_missing = factor(case_when(
      is.na(num_Ef) ~ 1,
      TRUE ~ 2
    ),
    levels = 1:2, labels = c("missing", "non-missing")
    ),
    num_Ef_05 = str_sub(as.character(num_Ef), -1, -1),
    num_Ef_05cat = factor(case_when(
      is.na(num_Ef_05) ~ 0,
      num_Ef_05 %in% c("0", "5") ~ 1,
      TRUE ~ 2
    ),
    levels = 0:2, labels = c("missing", "05%", "non-05%")
    ),
    num_age_cat = case_when(
      num_age < 65 ~ "<65",
      num_age >= 65 ~ ">=65"
    ),
    num_dmBmi_cat = case_when(
      is.na(num_dmBmi) ~ NA_character_,
      num_dmBmi < 25 ~ "1.<25",
      num_dmBmi >= 25 ~ "2.>=25"
    ),
    num_dmhome = if_else(num_dmhome == "Other", "Other situation", as.character(num_dmhome)),
    num_Bp1 = case_when(
      num_dmPtype == "Hospital" ~ num_dcBp1,
      num_dmPtype == "Outpatient" ~ num_dmBp1
    ),
    num_Bp1_cat = factor(case_when(
      num_Bp1 < 110 ~ 2,
      num_Bp1 >= 110 ~ 1
    ),
    levels = 1:2,
    labels = c(">=110", "<110")
    ),
    num_Ryth = coalesce(num_dcRyth, num_opRyth),
    num_Ryth_cat = recode(num_Ryth,
      `Atrial Fibrillation/Flutter` = "Atrial Fibrillation",
      .default = "Other rythm"
    ),
    num_Bpm = case_when(
      num_dmPtype == "Hospital" ~ num_dcBpm,
      num_dmPtype == "Outpatient" ~ num_dmBpm
    ),
    num_Bpm_cat = factor(case_when(
      is.na(num_Bpm) | is.na(num_Ryth) ~ NA_real_,
      num_Bpm >= 70 & num_Ryth %in% c("Sinus", "Other") |
        num_Bpm >= 80 & num_Ryth == "Atrial Fibrillation/Flutter" ~ 2,
      TRUE ~ 1
    ),
    levels = 1:2,
    labels = c("<70/80(sinus/af)", ">=70/80(sinus/af)")
    ),
    d_dmHF_history = case_when(
      is.na(num_dmHF) ~ NA_character_,
      num_dmHF %in% c("Yes with previous hospitalisation", "Yes without previous hospitalisation") ~ "Yes",
      TRUE ~ "No"
    ),
    d_dmHF_cat = case_when(
      is.na(num_dmHF) ~ NA_character_,
      num_dmHF == "Yes with previous hospitalisation" ~ "Previous HF hosp",
      TRUE ~ "No previous HF hosp"
    ),
    d_HFdiagnosis = case_when(
      num_dmHF == "No" ~ "<12mo",
      num_dmMonth %in% c("< 6 months", "6 - 12 months") ~ "<12mo",
      num_dmMonth %in% c("> 12 months") ~ ">12mo"
    ),
    num_dmHF = if_else(num_dmHF == "No", "No history", as.character(num_dmHF)),
    num_dmEtio_c1 = relevel(num_dmEtio_c1, ref = "Non-ischemic heart disease"),
    num_dmEtio = factor(case_when(
      num_dmEtio == "Ischemic heart disease documented by coronary angiography" ~ "IHD doc by ca",
      num_dmEtio == "Ischemic heart disease not documented by coronary angiography" ~ "IHD not documented by ca",
      TRUE ~ as.character(num_dmEtio)
    )),
    num_dmDev_cat = factor(case_when(
      is.na(num_dmDev) ~ NA_real_,
      num_dmDev %in% c("No") ~ 1,
      num_dmDev %in% c("PM") ~ 2,
      num_dmDev %in% c("CRT-P", "CRT-D", "ICD") ~ 3
    ), levels = 1:3, labels = c("No device", "PM", "CT/ICD")),
    num_dmSmoking_cat = factor(case_when(
      is.na(num_dmSmoking) ~ NA_real_,
      num_dmSmoking == "Current" ~ 2,
      TRUE ~ 1
    ), levels = 1:2, labels = c("Never/Former", "Current")),
    num_dmAfib_cat2 = case_when(
      is.na(num_dmAfib) ~ NA_character_,
      num_dmAfib == "No" ~ "No",
      TRUE ~ "Yes"
    ),
    num_dmAfib_cat3 = case_when(
      is.na(num_dmAfib) ~ NA_character_,
      num_dmAfib == "No" ~ "No",
      num_dmAfib == "Permanent" ~ "Permanent",
      TRUE ~ "Persistent/Paroxysmal"
    ),
    num_dmDiab_cat = case_when(
      num_dmDiab == "Newly diagnosed" ~ "Yes",
      TRUE ~ as.character(num_dmDiab)
    ),
    num_dmThy_cat = case_when(
      num_dmThy == "No" ~ "No",
      num_dmThy %in% c("Hypothyroidism", "Hyperthyroidism") ~ "Yes"
    ),
    num_Ral = coalesce(num_dcRal, num_opRal),
    num_S3 = coalesce(num_dcS3, num_opS3),
    num_Jvp = coalesce(num_dcJvp, num_opJvp),
    num_Hypop = coalesce(num_dcHypop, num_opHypop),
    num_Eff = coalesce(num_dcEff, num_opEff),
    num_Cold = coalesce(num_dcCold, num_opCold),
    num_Hep = coalesce(num_dcHep, num_opHep),
    num_Mr = coalesce(num_dcMr, num_opMr),
    num_Oed = coalesce(num_dcOed, num_opOed),
    num_As = coalesce(num_dcAs, num_opAs),

    # lab
    num_BunorUre = coalesce(num_dcBun, num_dcUre, num_opBun, num_opUre),
    num_Urc = coalesce(num_dcUrc, num_opUrc),
    num_Prot = coalesce(num_dcProt, num_opProt),
    num_Tsh = coalesce(num_dcTsh, num_opTsh),
    num_Nt = coalesce(num_dcNt, num_opNt),
    num_Bnp = coalesce(num_dcBnp, num_opBnp),
    num_NtorBnp = coalesce(num_Nt, num_Bnp),
    num_Hba1 = coalesce(num_dcHba1, num_opHba1),
    num_Trop = coalesce(num_dcTrop, num_opTrop),
    num_TroporHtrop = coalesce(num_dcTrop, num_opTrop, num_dcHtrop, num_opHtrop),
    num_Sod = coalesce(num_dcSod, num_opSod),
    num_Pot = coalesce(num_dcPot, num_opPot),

    # eGFR according to CKD-EPI
    sex = recode(num_dmgender, "Male" = 1, "Female" = 0),
    ethnicity = case_when(
      is.na(num_dmEthnic) ~ NA_real_,
      num_dmEthnic == "Black" ~ 1,
      TRUE ~ 0
    ),
    num_Cre = coalesce(num_dcCre, num_opCre),
    num_CKDEPI = nephro::CKDEpi.creat(num_Cre, sex, num_age, ethnicity),
    num_CKDEPI_cat = factor(case_when(
      num_CKDEPI < 60 ~ 2,
      num_CKDEPI >= 60 ~ 1
    ),
    levels = 1:2,
    labels = c(">=60", "<60")
    ),
    num_Bun = coalesce(num_dcBun, num_opBun),
    num_Ure = coalesce(num_dcUre, num_opUre),
    num_Bili = coalesce(num_dcBili, num_opBili),
    num_Crp = coalesce(num_dcCrp, num_opCrp),
    num_Wbc = coalesce(num_dcWbc, num_opWbc),
    num_Hb = coalesce(num_dcHb, num_opHb),
    num_Hb_cat = factor(case_when(
      is.na(num_Hb) | is.na(num_dmgender) ~ NA_real_,
      num_Hb < 12 & num_dmgender == "Female" | num_Hb < 13 & num_dmgender == "Male" ~ 2,
      TRUE ~ 1
    ),
    levels = 1:2,
    labels = c(">=12/13(women/men)", "<12/13(women/men)")
    ),
    num_Nyha = coalesce(num_dcNyha, num_opNyha),
    num_Nyha_cat = case_when(
      num_Nyha %in% c("NYHA I", "NYHA II") ~ "I-II",
      num_Nyha %in% c("NYHA III", "NYHA IV") ~ "III-IV"
    ),
    num_Xrn = coalesce(num_dcXrn, num_opXrn),
    num_Xal = coalesce(num_dcXal, num_opXal),
    num_Xpu = coalesce(num_dcXpu, num_opXpu),
    d_X_pulmc_alvoedema = case_when(
      num_Xrn == "Yes" ~ "No",
      is.na(num_Xpu) | is.na(num_Xal) ~ NA_character_,
      num_Xpu == "No" & num_Xal == "No" ~ "No",
      num_Xpu == "Yes" | num_Xal == "Yes" ~ "Yes",
      TRUE ~ NA_character_
    ),
    num_Lbbb = coalesce(num_dcLbbb, num_opLbbb),
    num_Lvdd = coalesce(num_dcLvdd, num_opLvdd),
    num_Lvh = coalesce(num_dcLvh, num_opLvh),
    # num_EA = coalesce(num_dcEA, num_opEA),
    num_Edec = coalesce(num_dcEdec, num_opEdec),
    num_LaMes = coalesce(num_dcLaMes, num_opLaMes),
    num_Rsp = coalesce(num_dcRsp, num_opRsp),
    num_AorReg = coalesce(num_dcAorReg, num_opAorReg),
    num_MitReg = coalesce(num_dcMitReg, num_opMitReg),
    num_AorSte = coalesce(num_dcAorSte, num_opAorSte),
    num_TriCus = coalesce(num_dcTriCus, num_opTriCus),

    # # hosp specific stuff
    # num_hsHFstat = if_else(num_dmPtype == "Outpatient", "NA (outpatient)", as.character(num_hsHFstat)),
    # num_hsHf = if_else(num_dmPtype == "Outpatient", "NA (outpatient)", as.character(num_hsHf)),
    # num_hsAcs = if_else(num_dmPtype == "Outpatient", "NA (outpatient)", as.character(num_hsAcs)),
    # num_hsFacMy = if_else(num_dmPtype == "Outpatient", "NA (outpatient)", as.character(num_hsFacMy)),
    # num_hsFacNonc = if_else(num_dmPtype == "Outpatient", "NA (outpatient)", as.character(num_hsFacNonc)),
    # num_hsFacAf = if_else(num_dmPtype == "Outpatient", "NA (outpatient)", as.character(num_hsFacAf)),
    # num_hsFacVa = if_else(num_dmPtype == "Outpatient", "NA (outpatient)", as.character(num_hsFacVa)),
    # num_hsFacInf = if_else(num_dmPtype == "Outpatient", "NA (outpatient)", as.character(num_hsFacInf)),
    # num_hsFacUnh = if_else(num_dmPtype == "Outpatient", "NA (outpatient)", as.character(num_hsFacUnh)),
    # num_hsFacBrad = if_else(num_dmPtype == "Outpatient", "NA (outpatient)", as.character(num_hsFacBrad)),
    # num_hsFacRen = if_else(num_dmPtype == "Outpatient", "NA (outpatient)", as.character(num_hsFacRen)),
    # num_hsFacIat = if_else(num_dmPtype == "Outpatient", "NA (outpatient)", as.character(num_hsFacIat)),
    # num_hsFacAne = if_else(num_dmPtype == "Outpatient", "NA (outpatient)", as.character(num_hsFacAne)),
    # num_hsFacOt = if_else(num_dmPtype == "Outpatient", "NA (outpatient)", as.character(num_hsFacOt)),
    # num_hsHosPresCli = if_else(num_dmPtype == "Outpatient", "NA (outpatient)", as.character(num_hsHosPresCli)),
    # num_hsIntr = if_else(num_dmPtype == "Outpatient", "NA (outpatient)", as.character(num_hsIntr)),
    # num_hsNitrat = if_else(num_dmPtype == "Outpatient", "NA (outpatient)", as.character(num_hsNitrat)),
    # num_hsProDiu = if_else(num_dmPtype == "Outpatient", "NA (outpatient)", as.character(num_hsProDiu)),

    num_mdDiur_c2 = case_when(
      num_dmPtype == "Hospital" ~ num_mdDiurd_c2,
      num_dmPtype == "Outpatient" ~ num_mdDiurh_c2
    ),
    num_mdDiur2_c2 = case_when(
      num_dmPtype == "Hospital" ~ num_mdDiur2d_c2,
      num_dmPtype == "Outpatient" ~ num_mdDiur2h_c2
    ),
    d_mdloopDiur = case_when(
      is.na(num_mdDiur_c2) ~ NA_character_,
      num_mdDiur_c2 %in% c("Flurosemide", "Torasemide", "Bumetanide") |
        num_mdDiur2_c2 %in% c("Flurosemide", "Torasemide", "Bumetanide") ~ "Yes",
      TRUE ~ "No"
    ),
    #
    #     num_mdDiurdo = case_when(num_dmPtype == "Hospital" ~ num_mdDiurddo,
    #                             num_dmPtype == "Outpatient" ~ num_mdDiurhdo),
    #     num_mdDiur2do = case_when(num_dmPtype == "Hospital" ~ num_mdDiur2ddo,
    #                              num_dmPtype == "Outpatient" ~ num_mdDiur2hdo),
    #
    # tmp_dosed1 = case_when(
    #   num_mdDiur_c2 == "Flurosemide" ~ num_mdDiurdo / 40 * 40,
    #   num_mdDiur_c2 == "Torasemide" ~ num_mdDiurdo / 10 * 40,
    #   num_mdDiur_c2 == "Bumetanide" ~ num_mdDiurdo / 1 * 40
    # ),
    #
    # tmp_dosed2 = case_when(
    #   num_mdDiur2_c2 == "Flurosemide" ~ num_mdDiur2do / 40 * 40,
    #   num_mdDiur2_c2 == "Torasemide" ~ num_mdDiur2do / 10 * 40,
    #   num_mdDiur2_c2 == "Bumetanide" ~ num_mdDiur2do / 1 * 40
    # ),
    #
    # d_loopDiurddose_eqFurosemide = case_when(
    #   num_mdDiur_c2 %in% c("Flurosemide", "Torasemide", "Bumetanide") &
    #     num_mdDiur2_c2 %in% c("Flurosemide", "Torasemide", "Bumetanide") ~ tmp_dosed1 + tmp_dosed2,
    #   num_mdDiur_c2 %in% c("Flurosemide", "Torasemide", "Bumetanide") ~ tmp_dosed1,
    #   num_mdDiur2_c2 %in% c("Flurosemide", "Torasemide", "Bumetanide") ~ tmp_dosed2
    # ),


    num_mdDiur = case_when(
      num_dmPtype == "Hospital" ~ num_mdDiurd,
      num_dmPtype == "Outpatient" ~ num_mdDiurh
    ),
    num_mdACE = case_when(
      num_dmPtype == "Hospital" ~ num_mdACEd,
      num_dmPtype == "Outpatient" ~ num_mdACEh
    ),
    num_mdAT = case_when(
      num_dmPtype == "Hospital" ~ num_mdATd,
      num_dmPtype == "Outpatient" ~ num_mdATh
    ),
    num_mdARNI = case_when(
      num_dmPtype == "Hospital" ~ num_mdARNId,
      num_dmPtype == "Outpatient" ~ num_mdARNIh
    ),
    d_arb_or_ace_or_arni = case_when(
      is.na(num_mdACE) | is.na(num_mdAT) ~ NA_character_,
      num_mdACE == "Yes" | num_mdAT == "Yes" | num_mdARNI == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    num_mdAL = case_when(
      num_dmPtype == "Hospital" ~ num_mdALd,
      num_dmPtype == "Outpatient" ~ num_mdALh
    ),
    num_mdBB = case_when(
      num_dmPtype == "Hospital" ~ num_mdBBd,
      num_dmPtype == "Outpatient" ~ num_mdBBh
    ),
    num_mdCcb = case_when(
      num_dmPtype == "Hospital" ~ num_mdCcbd,
      num_dmPtype == "Outpatient" ~ num_mdCcbh
    ),
    num_mdNit = case_when(
      num_dmPtype == "Hospital" ~ num_mdNitd,
      num_dmPtype == "Outpatient" ~ num_mdNith
    ),
    num_mdIvab = case_when(
      num_dmPtype == "Hospital" ~ num_mdIvabd,
      num_dmPtype == "Outpatient" ~ num_mdIvabh
    ),
    num_mdDigo = case_when(
      num_dmPtype == "Hospital" ~ num_mdDigod,
      num_dmPtype == "Outpatient" ~ num_mdDigoh
    ),
    num_mdAmi = case_when(
      num_dmPtype == "Hospital" ~ num_mdAmid,
      num_dmPtype == "Outpatient" ~ num_mdAmih
    ),
    num_mdAntiar = case_when(
      num_dmPtype == "Hospital" ~ num_mdAntiard,
      num_dmPtype == "Outpatient" ~ num_mdAntiarh
    ),
    num_mdAntipl = case_when(
      num_dmPtype == "Hospital" ~ num_mdAntipld,
      num_dmPtype == "Outpatient" ~ num_mdAntiplh
    ),
    num_mdAC = case_when(
      num_dmPtype == "Hospital" ~ num_mdACd,
      num_dmPtype == "Outpatient" ~ num_mdACh
    ),
    num_mdStat = case_when(
      num_dmPtype == "Hospital" ~ num_mdStatd,
      num_dmPtype == "Outpatient" ~ num_mdStath
    ),

    # Outcomes
    enddtm = coalesce(num_f1DeathDt, num_f1contDt),
    startdtm = coalesce(num_dcDischdt, num_dmVisitdt),
    outtime_death = as.numeric(enddtm - startdtm),
    out_death = case_when(
      num_f1vital == "Alive" ~ 0,
      num_f1vital == "Dead" ~ 1
    ),
    out_deathcv = case_when(
      is.na(out_death) ~ NA_real_,
      num_f1DeathCs %in% c("Cardiac", "Vascular") ~ 1,
      TRUE ~ 0
    ), # pats with missing info are NOT included in CV

    # HF hosp
    out_hosphf = case_when(
      num_f1lost != "No" ~ NA_real_,
      num_f1hosp1cs == "HF" |
        num_f1hosp2cs == "HF" |
        num_f1hosp3cs == "HF" |
        num_f1hosp4cs == "HF" |
        num_f1hosp5cs == "HF" ~ 1,
      TRUE ~ 0
    ),
    out_hosphfdtm = case_when(
      num_f1hosp1cs == "HF" ~ num_f1hosp1dt,
      num_f1hosp2cs == "HF" ~ num_f1hosp2dt,
      num_f1hosp3cs == "HF" ~ num_f1hosp3dt,
      num_f1hosp4cs == "HF" ~ num_f1hosp4dt,
      num_f1hosp5cs == "HF" ~ num_f1hosp5dt
    ),
    outtime_hosphf = as.numeric(out_hosphfdtm - startdtm),
    outtime_hosphf = ifelse(out_hosphf == 1 & is.na(outtime_hosphf), outtime_death / 2, outtime_hosphf),
    outtime_hosphf = pmin(outtime_hosphf, outtime_death, na.rm = TRUE),

    # cv death or hf hosp
    out_deathcvhosphf = ifelse(out_hosphf == 1, 1, out_deathcv)
  ) %>%
  mutate_if(is.character, as.factor) %>%
  select(-starts_with("tmp_"))
