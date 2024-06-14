################################################################################
# ENCUESTA HUMEDALES
# Nuevos Nombres de Variables
################################################################################

## Nuevos nombres de variables -------------------------------------------------
humedales <- humedales |> 
  rename(
    a2.1 = a2.1...25,
    a2.1_otesp = a2.1...26,
    a2.2 = a2.2...27,
    a2.2_otesp = a2.2...28,
    b1.1_otesp = b1.1,
    b1.3_otesp = b1.3,
    b1.4_otesp = b1.4,
    b1.7 = b1.6,
    b1.6_1 = b1.5_1...115,
    b1.6_2 = b1.5_2...116,
    b1.6_3 = b1.5_3...117,
    b1.6_4 = b1.5_4...118,
    b1.5_1 = b1.5_1...100,
    b1.5_2 = b1.5_2...101,
    b1.5_3 = b1.5_3...102,
    b1.5_4 = b1.5_4...103,
    b1.5_otesp = b1.5_14_text,
    b2.2 = b2.2...121,
    b2.3_otesp = b2.3,
    b2.4 = b2.4...132,
    b2.4_otesp = b2.4...133,
    b2.7_1 = b2.7_4,
    b2.7_2 = b2.7_5,
    b2.7_3 = b2.7_6,
    b2.7_4 = b2.7_7,
    b2.7_5 = b2.7_8,
    b2.7_6 = b2.7_9,
    b2.7_7 = b2.7_10,
    b2.7_8 = b2.7_11,
    b2.7_9 = b2.7_12,
    b2.7_10 = b2.7_17,
    b2.7_11 = b2.7_14,
    b2.7_12 = b2.7_15,
    b2.7_14 = b2.7_16,
    b2.9_otesp = b2.9,
    b2.11 = b2.2...161,
    b2.11_otesp = b2.2...162,
    b3.3_otesp = b3.3,
    b3.4 = b3.4...175,
    b3.4_otesp = b3.4...176,
    b3.7_1 = q167_4,
    b3.7_2 = q167_5,
    b3.7_3 = q167_6,
    b3.7_4 = q167_7,
    b3.7_5 = q167_8,
    b3.7_6 = q167_9,
    b3.7_7 = q167_10,
    b3.7_8 = q167_11,
    b3.7_9 = q167_12,
    b3.7_10 = q167_17,
    b3.7_11 = q167_14,
    b3.7_12 = q167_15,
    b3.7_13 = q167_13,
    b3.7_14 = q167_16,
    b3.9_11 = b3.9_7,
    b3.9_otesp = b3.9,
    b3.11 = b3.1b...206,
    b3.11_otesp = b3.1b...207
)

# Limpiar variables que no son útiles ------------------------------------------

humedales <- humedales |>
  select(
    -recipientlastname,
    -recipientfirstname,
    -recipientemail,
    -externalreference,
    -distributionchannel,
    -userlanguage,
    -q_relevantidduplicate,
    -q_relevantidduplicatescore,
    -q_relevantidfraudscore,
    -q_relevantidlaststartdate,
    -d4,
    -d5,
    -f1_11,
    -f1_9,
    -ident
)

# GUARDAR Y EXPORTAR BASE DE DATOS HUMEDALES FINAL -----------------------------
# Formato R
saveRDS(humedales,
        file = "base_final/base_humedales_final.rds"
)

# Formato Excel xlsx
write.xlsx(humedales,
           file = "base_final/base_humedales_final.xlsx",
           colNames = TRUE
)



