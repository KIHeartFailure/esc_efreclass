

# Primary criteria --------------------------------------------------------

flow <- c(paste0("Number of patients in ESC "), nrow(esc))

pdata <- esc %>%
  filter(num_dcVital == "Alive" | num_dmPtype == "Outpatient")
flow <- rbind(flow, c("If hospitalizaed, alive at disharge", nrow(pdata)))

pdata <- pdata %>%
  mutate(
    enddtm = coalesce(num_f1DeathDt, num_f1contDt),
    startdtm = coalesce(num_dcDischdt, num_dmVisitdt),
    outtime_death = as.numeric(enddtm - startdtm),
    survpop = num_f1lost == "No" & outtime_death >= 0 & !is.na(outtime_death)
  )

flow <- rbind(flow, c(
  " - Not lost to follow-up and not negative follow-up times (outcome analysis population)",
  nrow(pdata %>% filter(survpop))
))

colnames(flow) <- c("Criteria", "N")
