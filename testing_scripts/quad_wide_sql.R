NETN_QuadSpecies_wide <-
"SELECT * FROM
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
	tluCOMN.Taxon.TSN AS 'TSN',
	tluCOMN.Taxon.ScientificName AS 'ScientificName',
	tluCOMN.ConfidenceClass.Code AS 'ConfidenceClassCode',

	tluCOMN.CoverClass.Code AS 'QuadData',
	tblNETN.QuadratEventSpeciesCover.IsCollected AS 'IsCollected',
	tblNETN.QuadratEventSpeciesCover.Note AS 'QuadSpp.Note',
	tblCOMN.[Event].ID AS 'EventID',
  tblCOMN.Plot.ID AS 'PlotID',
	tluCOMN.Quadrat.Code AS DataType,
  tluCOMN.TaxonomicReferenceAuthority_Identification.Code AS TRACode,
  tluCOMN.ProtectedStatus.Code AS ProtectedStatusCode,
	tluCOMN.DataProcessingLevel.Code AS DPLCode,
	GETDATE() AS 'ExportDate'

FROM tluCOMN.Park
  INNER JOIN tblCOMN.Plot ON tluCOMN.Park.ID = tblCOMN.Plot.ParkID
	INNER JOIN tluCOMN.Panel ON tblCOMN.Plot.PanelID = tluCOMN.Panel.ID
	INNER JOIN tluCOMN.PlotType ON tblCOMN.Plot.PlotTypeID = tluCOMN.PlotType.ID
  INNER JOIN tblCOMN.[Event] ON tblCOMN.[Event].PlotID = tblCOMN.Plot.ID
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

SELECT
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
	tluCOMN.Taxon.TSN AS 'TSN',
	tluCOMN.Taxon.ScientificName AS 'ScientificName',
	tluCOMN.ConfidenceClass.Code AS 'ConfidenceClassCode',

	tluCOMN.CoverClass.Label AS 'QuadData',
	tblNETN.QuadratEventSpeciesCover.IsCollected AS 'IsCollected',
	tblNETN.QuadratEventSpeciesCover.Note AS 'QuadSpp.Note',
	tblCOMN.[Event].ID AS 'EventID',
  tblCOMN.Plot.ID AS 'PlotID',
	tluCOMN.Quadrat.Code+'_'+'txt' AS DataType,
	tluCOMN.TaxonomicReferenceAuthority_Identification.Code AS TRACode,
  tluCOMN.ProtectedStatus.Code AS ProtectedStatusCode,
	tluCOMN.DataProcessingLevel.Code AS DPLCode,
	GETDATE() AS 'ExportDate'

FROM tluCOMN.Park
  INNER JOIN tblCOMN.Plot ON tluCOMN.Park.ID = tblCOMN.Plot.ParkID
	INNER JOIN tluCOMN.Panel ON tblCOMN.Plot.PanelID = tluCOMN.Panel.ID
	INNER JOIN tluCOMN.PlotType ON tblCOMN.Plot.PlotTypeID = tluCOMN.PlotType.ID
  INNER JOIN tblCOMN.[Event] ON tblCOMN.[Event].PlotID = tblCOMN.Plot.ID
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

SELECT
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
	tluCOMN.Taxon.TSN AS 'TSN',
	tluCOMN.Taxon.ScientificName AS 'ScientificName',
	tluCOMN.ConfidenceClass.Code AS 'ConfidenceClassCode',

	tluCOMN.SpeciesSampleQualifier.Code AS 'QuadData',
	tblNETN.QuadratEventSpeciesCover.IsCollected AS 'IsCollected',
	tblNETN.QuadratEventSpeciesCover.Note AS 'QuadSpp.Note',
	tblCOMN.[Event].ID AS 'EventID',
  tblCOMN.Plot.ID AS 'PlotID',
	tluCOMN.Quadrat.Code+'_'+'SQ' AS DataType,
	tluCOMN.TaxonomicReferenceAuthority_Identification.Code AS TRACode,
  tluCOMN.ProtectedStatus.Code AS ProtectedStatusCode,
	tluCOMN.DataProcessingLevel.Code AS DPLCode,
	GETDATE() AS 'ExportDate'

FROM tluCOMN.Park
  INNER JOIN tblCOMN.Plot ON tluCOMN.Park.ID = tblCOMN.Plot.ParkID
	INNER JOIN tluCOMN.Panel ON tblCOMN.Plot.PanelID = tluCOMN.Panel.ID
	INNER JOIN tluCOMN.PlotType ON tblCOMN.Plot.PlotTypeID = tluCOMN.PlotType.ID
  INNER JOIN tblCOMN.[Event] ON tblCOMN.[Event].PlotID = tblCOMN.Plot.ID
  LEFT JOIN tblCOMN.QuadratEvent ON tblCOMN.[Event].ID = tblCOMN.QuadratEvent.EventID
	LEFT JOIN tluCOMN.Quadrat ON tblCOMN.QuadratEvent.QuadratID = tluCOMN.Quadrat.ID
	LEFT JOIN tblNETN.QuadratEventSpeciesCover ON tblCOMN.QuadratEvent.ID = tblNETN.QuadratEventSpeciesCover.QuadratEventID
	LEFT JOIN tluCOMN.SpeciesSampleQualifier ON tblCOMN.QuadratEvent.SpeciesSampleQualifierID = tluCOMN.SpeciesSampleQualifier.ID
	LEFT JOIN tluCOMN.ConfidenceClass ON tblNETN.QuadratEventSpeciesCover.ConfidenceClassID = tluCOMN.ConfidenceClass.ID
	LEFT JOIN tluCOMN.Taxon ON tblNETN.QuadratEventSpeciesCover.TaxonID = tluCOMN.Taxon.ID
	LEFT OUTER JOIN tluCOMN.TaxonomicReferenceAuthority_Identification ON
	  tblNETN.QuadratEventSpeciesCover.TaxonomicReferenceAuthority_IdentificationID = tluCOMN.TaxonomicReferenceAuthority_Identification.ID
	LEFT OUTER JOIN tluCOMN.ProtectedStatus ON
	  tblNETN.QuadratEventSpeciesCover.ProtectedStatusID = tluCOMN.ProtectedStatus.ID
	LEFT OUTER JOIN tluCOMN.DataProcessingLevel ON
	  tblCOMN.QuadratEvent.DataProcessingLevelID = tluCOMN.DataProcessingLevel.ID)

X PIVOT (MAX(QuadData) FOR DataType IN
  (UC, UL, ML, BL, BC, BR, MR, UR,
  UC_txt, UL_txt, ML_txt, BL_txt, BC_txt, BR_txt, MR_txt, UR_txt,
	UC_SQ, UL_SQ, ML_SQ, BL_SQ, BC_SQ, BR_SQ, MR_SQ, UR_SQ)) PIV"
