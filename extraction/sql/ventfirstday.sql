-- View for extraction MV status on first day

DROP MATERIALIZED VIEW IF EXISTS ventfirstday CASCADE;
CREATE MATERIALIZED VIEW ventfirstday AS

SELECT ap.patientunitstayid, ap.oobintubday1 FROM apachepredvar ap;