CREATE UNIQUE INDEX req_prod_fac_per ON requisitions(programid, facilityid, processingperiodid) WHERE (emergency = FALSE)
