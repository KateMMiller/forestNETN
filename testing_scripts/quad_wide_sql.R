NETN_QuadSpecies_wide_kmm <-
  "
SELECT
Network, ParkUnit, ParkSubUnit, PlotTypeCode, PlotCode, IsAbandoned, PanelCode, StartYear, IsQAQC, PIVSQ.EventID, PlotID, SQQuadSum,
UC_SQ, UR_SQ, MR_SQ, BR_SQ, BC_SQ, BL_SQ, ML_SQ, UL_SQ, TSN,

IIF(SQQuadSum = 8 AND ScientificName_tmp IS NULL, 'None present', ScientificName_tmp) AS ScientificName,

IIF(UC_cd IS NULL AND UC_SQ = 'NP', 0, UC_cd) AS UC,
IIF(UR_cd IS NULL AND UR_SQ = 'NP', 0, UR_cd) AS UR,
IIF(MR_cd IS NULL AND MR_SQ = 'NP', 0, MR_cd) AS MR,
IIF(BR_cd IS NULL AND BR_SQ = 'NP', 0, BR_cd) AS BR,
IIF(BC_cd IS NULL AND BC_SQ = 'NP', 0, BC_cd) AS BC,
IIF(BL_cd IS NULL AND BL_SQ = 'NP', 0, BL_cd) AS BL,
IIF(ML_cd IS NULL AND ML_SQ = 'NP', 0, ML_cd) AS ML,
IIF(UL_cd IS NULL AND UL_SQ = 'NP', 0, UL_cd) AS UL,

IIF(UC_txt IS NULL AND UC_SQ = 'NP', '0%', UC_txt) AS UC_txt,
IIF(UR_txt IS NULL AND UR_SQ = 'NP', '0%', UR_txt) AS UR_txt,
IIF(MR_txt IS NULL AND MR_SQ = 'NP', '0%', MR_txt) AS MR_txt,
IIF(BR_txt IS NULL AND BR_SQ = 'NP', '0%', BR_txt) AS BR_txt,
IIF(BC_txt IS NULL AND BC_SQ = 'NP', '0%', BC_txt) AS BC_txt,
IIF(BL_txt IS NULL AND BL_SQ = 'NP', '0%', BL_txt) AS BL_txt,
IIF(ML_txt IS NULL AND ML_SQ = 'NP', '0%', ML_txt) AS ML_txt,
IIF(UL_txt IS NULL AND UL_SQ = 'NP', '0%', UL_txt) AS UL_txt,

IsGerminant, ConfidenceClassCode, IsCollected,
QuadSppNote, TRACode, ProtectedStatusCode, DPLCode
/* Remove SQQuadSum for final version. It's a temporary column for the WHERE statement that drops null TSN/ScientificNames when
>1 but <8 (ie all) quadrats are NS.
*/

FROM
-- Query 1: Compile SQs for each quadrat and join with temp table that sums num of quads sampled
(SELECT Network, ParkUnit, ParkSubUnit, PlotTypeCode, PlotCode, IsAbandoned, PanelCode,
       StartYear, IsQAQC, QuadData, SQ.EventID, PlotID, DataType, SQQuadSum, ExportDate
FROM
-- Query 1A: Compile SQs
(SELECT
  tluCOMN.Park.Network AS 'Network',
	tluCOMN.Park.Unit AS 'ParkUnit',
	tluCOMN.Park.SubUnit AS 'ParkSubUnit',
	tluCOMN.PlotType.Code AS 'PlotTypeCode',
	tblCOMN.Plot.Code AS 'PlotCode',
	tblCOMN.Plot.IsAbandoned AS 'IsAbandoned',
	tluCOMN.Panel.Code AS 'PanelCode',
	tblCOMN.[Event].StartDate AS 'StartDate',
	tblCOMN.[Event].IsQAQC AS 'IsQAQC',
	tblCOMN.[Event].StartYear AS 'StartYear',
	tluCOMN.SpeciesSampleQualifier.Code AS 'QuadData',
	tblCOMN.[Event].ID AS 'EventID',
    tblCOMN.Plot.ID AS 'PlotID',
	tluCOMN.Quadrat.Code+'_'+'SQ' AS DataType,
	GETDATE() AS 'ExportDate'

FROM tluCOMN.Park
    INNER JOIN tblCOMN.Plot ON tluCOMN.Park.ID = tblCOMN.Plot.ParkID
	INNER JOIN tluCOMN.Panel ON tblCOMN.Plot.PanelID = tluCOMN.Panel.ID
	INNER JOIN tluCOMN.PlotType ON tblCOMN.Plot.PlotTypeID = tluCOMN.PlotType.ID
    INNER JOIN tblCOMN.[Event] ON tblCOMN.[Event].PlotID = tblCOMN.Plot.ID
    LEFT JOIN tblCOMN.QuadratEvent ON tblCOMN.[Event].ID = tblCOMN.QuadratEvent.EventID
	LEFT JOIN tluCOMN.Quadrat ON tblCOMN.QuadratEvent.QuadratID = tluCOMN.Quadrat.ID
	LEFT JOIN tluCOMN.SpeciesSampleQualifier ON tblCOMN.QuadratEvent.SpeciesSampleQualifierID = tluCOMN.SpeciesSampleQualifier.ID) AS SQ

LEFT JOIN
-- Query 1B: Calculate number of quads sampled and join with Q1A.
(SELECT DISTINCT
	tblCOMN.[Event].ID AS 'EventID',
	SUM(CASE
	  WHEN tluCOMN.SpeciesSampleQualifier.Code IN ('NS', 'PM') THEN 0
	  WHEN tluCOMN.SpeciesSampleQualifier.Code IN ('SS', 'NP') THEN 1
	END) AS SQQuadSum

FROM tblCOMN.[Event]
  LEFT JOIN tblCOMN.QuadratEvent ON tblCOMN.[Event].ID = tblCOMN.QuadratEvent.EventID
	LEFT JOIN tluCOMN.SpeciesSampleQualifier ON tblCOMN.QuadratEvent.SpeciesSampleQualifierID = tluCOMN.SpeciesSampleQualifier.ID

GROUP BY tblCOMN.Event.ID) AS SQquadsum

ON SQ.EventID = SQquadsum.EventID) AS SQ

PIVOT (MAX(SQ.QuadData) FOR DataType IN
(UC_SQ, UR_SQ, MR_SQ, BR_SQ, BC_SQ, BL_SQ, ML_SQ, UL_SQ)) AS PIVSQ

-- Join wide SQ table with wide quadrat species cover data
LEFT JOIN
(SELECT * FROM
-- Query 2: Compile QuadSpp by cover class code and label
-- Query 2A: Cover Class Code
(SELECT
	tblCOMN.[Event].ID AS 'EventID',
	tluCOMN.Taxon.TSN AS 'TSN',
	tluCOMN.Taxon.ScientificName AS 'ScientificName_tmp',
	tluCOMN.ConfidenceClass.Code AS 'ConfidenceClassCode',
	tluCOMN.CoverClass.Code AS 'QuadData',
	tblNETN.QuadratEventSpeciesCover.IsCollected AS 'IsCollected',
	tblNETN.QuadratEventSpeciesCover.IsGerminant,
	tblNETN.QuadratEventSpeciesCover.Note AS 'QuadSppNote',
	tluCOMN.Quadrat.Code+'_'+'cd' AS DataType,
	tluCOMN.TaxonomicReferenceAuthority_Identification.Code AS TRACode,
    tluCOMN.ProtectedStatus.Code AS ProtectedStatusCode,
	tluCOMN.DataProcessingLevel.Code AS DPLCode

FROM tblCOMN.[Event]
    LEFT JOIN tblCOMN.QuadratEvent ON tblCOMN.[Event].ID = tblCOMN.QuadratEvent.EventID
	LEFT JOIN tluCOMN.Quadrat ON tblCOMN.QuadratEvent.QuadratID = tluCOMN.Quadrat.ID
	LEFT JOIN tblNETN.QuadratEventSpeciesCover ON tblCOMN.QuadratEvent.ID = tblNETN.QuadratEventSpeciesCover.QuadratEventID
	LEFT JOIN tluCOMN.CoverClass ON tblNETN.QuadratEventSpeciesCover.CoverClassID = tluCOMN.CoverClass.ID
	LEFT JOIN tluCOMN.ConfidenceClass ON tblNETN.QuadratEventSpeciesCover.ConfidenceClassID = tluCOMN.ConfidenceClass.ID
	LEFT JOIN tluCOMN.Taxon ON tblNETN.QuadratEventSpeciesCover.TaxonID = tluCOMN.Taxon.ID
	LEFT OUTER JOIN tluCOMN.TaxonomicReferenceAuthority_Identification ON
	  tblNETN.QuadratEventSpeciesCover.TaxonomicReferenceAuthority_IdentificationID = tluCOMN.TaxonomicReferenceAuthority_Identification.ID
	LEFT OUTER JOIN tluCOMN.ProtectedStatus ON
	  tblNETN.QuadratEventSpeciesCover.ProtectedStatusID = tluCOMN.ProtectedStatus.ID
	LEFT OUTER JOIN tluCOMN.DataProcessingLevel ON
	  tblCOMN.QuadratEvent.DataProcessingLevelID = tluCOMN.DataProcessingLevel.ID

UNION
-- Query 2B: Cover Class Label
SELECT
	tblCOMN.[Event].ID AS 'EventID',
	tluCOMN.Taxon.TSN AS 'TSN',
	tluCOMN.Taxon.ScientificName AS 'ScientificName_tmp',
	tluCOMN.ConfidenceClass.Code AS 'ConfidenceClassCode',
	tluCOMN.CoverClass.Label AS 'QuadData',
	tblNETN.QuadratEventSpeciesCover.IsCollected AS 'IsCollected',
	tblNETN.QuadratEventSpeciesCover.IsGerminant,
	tblNETN.QuadratEventSpeciesCover.Note AS 'QuadSppNote',
	tluCOMN.Quadrat.Code+'_'+'txt' AS DataType,
	tluCOMN.TaxonomicReferenceAuthority_Identification.Code AS TRACode,
    tluCOMN.ProtectedStatus.Code AS ProtectedStatusCode,
	tluCOMN.DataProcessingLevel.Code AS DPLCode

FROM tblCOMN.[Event]
    LEFT JOIN tblCOMN.QuadratEvent ON tblCOMN.[Event].ID = tblCOMN.QuadratEvent.EventID
	LEFT JOIN tluCOMN.Quadrat ON tblCOMN.QuadratEvent.QuadratID = tluCOMN.Quadrat.ID
	LEFT JOIN tblNETN.QuadratEventSpeciesCover ON tblCOMN.QuadratEvent.ID = tblNETN.QuadratEventSpeciesCover.QuadratEventID
	LEFT JOIN tluCOMN.CoverClass ON tblNETN.QuadratEventSpeciesCover.CoverClassID = tluCOMN.CoverClass.ID
	LEFT JOIN tluCOMN.ConfidenceClass ON tblNETN.QuadratEventSpeciesCover.ConfidenceClassID = tluCOMN.ConfidenceClass.ID
	LEFT JOIN tluCOMN.Taxon ON tblNETN.QuadratEventSpeciesCover.TaxonID = tluCOMN.Taxon.ID
	LEFT OUTER JOIN tluCOMN.TaxonomicReferenceAuthority_Identification ON
	  tblNETN.QuadratEventSpeciesCover.TaxonomicReferenceAuthority_IdentificationID = tluCOMN.TaxonomicReferenceAuthority_Identification.ID
	LEFT OUTER JOIN tluCOMN.ProtectedStatus ON
	  tblNETN.QuadratEventSpeciesCover.ProtectedStatusID = tluCOMN.ProtectedStatus.ID
	LEFT OUTER JOIN tluCOMN.DataProcessingLevel ON
	  tblCOMN.QuadratEvent.DataProcessingLevelID = tluCOMN.DataProcessingLevel.ID
	) AS SPP

-- Pivot Quadrat Code and Label wide
PIVOT (MAX(SPP.QuadData) FOR DataType IN
  (UC_cd, UR_cd, MR_cd, BR_cd, BC_cd, BL_cd, ML_cd, UL_cd,
   UC_txt, UR_txt, MR_txt, BR_txt, BC_txt, BL_txt, ML_txt, UL_txt
   )) AS PIVSPP1) AS PIVSPP

ON PIVSQ.EventID = PIVSPP.EventID

-- Filter out unwanted NULLs
WHERE (SQQuadSum > 0 AND SQQuadSum < 8 AND TSN IS NOT NULL AND ScientificName_tmp IS NOT NULL) -- drops the extra null record
OR (SQQuadSum = 0) -- keeps null TSN and ScientificName if all quads were not sampled
OR (SQQuadSum = 8) -- keeps all plots with all quads sampled
"
