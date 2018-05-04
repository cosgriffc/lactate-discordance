-- View for extraction pressors status on first day

DROP MATERIALIZED VIEW IF EXISTS pressorfirstday CASCADE;
CREATE MATERIALIZED VIEW pressorfirstday AS

SELECT inf.patientunitstayid
, MAX(CASE
	WHEN LOWER(inf.drugname) LIKE '%norepinephrine%' THEN 1
	WHEN LOWER(inf.drugname) LIKE '%epinephrine%' THEN 1
	WHEN LOWER(inf.drugname) LIKE '%phenylephrine%' THEN 1
	WHEN LOWER(inf.drugname) LIKE '%vasopressin%' THEN 1
	WHEN LOWER(inf.drugname) LIKE '%dopamine%' THEN 1
	WHEN LOWER(inf.drugname) LIKE '%dobutamine%' THEN 1
	WHEN LOWER(inf.drugname) LIKE '%milrinone%' THEN 1
	ELSE 0
	END) AS pressor
FROM infusiondrug inf
WHERE inf.infusionoffset <= 1440
GROUP BY patientunitstayid;
