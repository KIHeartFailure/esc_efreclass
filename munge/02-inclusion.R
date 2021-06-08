

# Primary criteria --------------------------------------------------------

flow <- c(paste0("Number of patients in ESC "), nrow(esc))

pdata <- esc %>%
  filter(num_dcVital == "Alive" | num_dmPtype == "Outpatient")
flow <- rbind(flow, c("Alive at disharge from hospital", nrow(pdata)))

pdata <- pdata %>%
  mutate(pop_outcome = ifelse(num_f1lost == "No", 1, 0))
flow <- rbind(flow, c(".  Not lost to follow-up (used in outcome analysis)", nrow(pdata %>% filter(pop_outcome == 1))))

pdata <- pdata %>%
  mutate(
    enddtm = coalesce(num_f1DeathDt, num_f1contDt),
    startdtm = coalesce(num_dcDischdt, num_dmVisitdt),
    outtime_death = as.numeric(enddtm - startdtm),
    pop_outcome = ifelse(outtime_death >= 0 & pop_outcome == 1, 1, 0)
  )

colnames(flow) <- c("Criteria", "N")
