library(stringr)
library(flextable)
library(DBI)
library(RMySQL)
library(officer)
library(flextable)
library(yaml)

################################################################################################################
# Helper functions for fetching and saving data
################################################################################################################

#' Load report config information
#' Used by Report Builder, nvd identifier and report browser
#'
#' @param con_rb Report builder database connection
#'
#' @return report_config
#'
#' @export
loadReportConfig <- function(con_rb)
{
  query <- "SELECT * FROM DefReportBuilderVar;"
  data_var <- DBI::dbGetQuery(con_rb, query)
  data_var <- data_var[, -1]

  #Create the named list, add named variables
  var_names <- unique(data_var$Variable)
  report_config <- list()
  for(var_name in var_names)
  {
    sub <- subset(data_var, data_var$Variable == var_name)
    report_config[[var_name]] <- sub$Value
  }

  query <- "SELECT * FROM DefReportBuilderVarMap;"
  data_map <- DBI::dbGetQuery(con_rb, query)
  data_map <- data_map[, -1]

  #Create the named list, add named lists
  map_names <- unique(data_map$Variable)
  for(map_name in map_names)
  {
    df_sub <- subset(data_map, data_map$Variable == map_name)
    sub_list <- list()
    var_names <- df_sub$Value
    for(var_name in var_names)
    {
      sub <- subset(df_sub, df_sub$Value == var_name)
      sub_list[[var_name]] <- sub$MappedValue
    }
    report_config[[map_name]] <- sub_list
  }

  return (report_config)
}


#' Load coverage file data
#' Used by Report Builder, nvd identifier and report browser
#'
#' @param con_rb Report builder database connection
#'
#' @return coverage_data
#'
#' @export
loadCoverageData <- function(con_rb)
{
  query <- "SELECT * FROM DefCoverage;"
  coverage_data <- DBI::dbGetQuery(con_rb, query)
  coverage_data <- coverage_data[, -1]
  colnames(coverage_data) <- c("Gene", "Transcript", "Targeted exons", "Assay")

  return (coverage_data)
}

#' Load clinical context mapping data
#' Used by Report Builder, nvd identifier and report browser
#'
#' @param con_rb Report builder database connection
#'
#' @return clinical_context_mapping
#'
#' @export
loadClinicalContextMapping <- function(con_rb)
{
  query <- "SELECT * FROM DefClinicalContextMapping;"
  df_mapping <- DBI::dbGetQuery(con_rb, query)
  df_mapping <- df_mapping[, -1]

  query <- "SELECT * FROM DefClinicalSigLookup;"
  df_lookup <- DBI::dbGetQuery(con_rb, query)
  df_lookup <- df_lookup[, -1]

  df_mapping["Lookup"] <- NA
  rownames(df_mapping) <- df_mapping$Header
  clinical_context_mapping <- apply(df_mapping, 1, as.list)

  clinical_context_mapping <- lapply(clinical_context_mapping, function(item)
  {
    lookup_name <- item$LookupName
    data <- subset(df_lookup, df_lookup$LookupName == lookup_name)
    data$GeneName <- trimws(data$GeneName, which="both")
    data$D <- as.logical(data$D)
    data$P <- as.logical(data$P)
    data$DT <- as.logical(data$DT)
    data$DR <- as.logical(data$DR)
    data$MRD <- as.logical(data$MRD)

    item$Lookup <- data

    (item)
  })

  return (clinical_context_mapping)
}


#' Load SNP backbone data
#' Used by Report Builder, nvd identifier and report browser
#'
#' @param con_rb Report builder database connection
#'
#' @return snp_backbone
#'
#' @export
loadSNPBackbone <- function(con_rb)
{
  query <- "SELECT * FROM DefSNPBackbone;"
  snp_backbone <- DBI::dbGetQuery(con_rb, query)
  snp_backbone <- snp_backbone[, -1]

  return (snp_backbone)
}


#' Load Specimen lookup data for mapping Auslab data to specimen and specimen details
#' Used by Report Builder, nvd identifier and report browser
#'
#' @param con_rb Report builder database connection
#'
#' @return specimen_lookup
#'
#' @export
loadSpecimenLookup <- function(con_rb)
{
  query <- "SELECT * FROM DefSpecimenLookup;"
  specimen_lookup <- DBI::dbGetQuery(con_rb, query)
  specimen_lookup <- specimen_lookup[, -1]

  return (specimen_lookup)
}


#' Load user and display name mapping
#' Used by Report Builder, nvd identifier and report browser
#'
#' @param con_rb Report builder database connection
#'
#' @return user_mapping
#'
#' @export
loadUserMapping <- function(con_rb)
{
  query <- "SELECT * FROM DefUserMapping;"
  user_mapping <- DBI::dbGetQuery(con_rb, query)
  user_mapping <- user_mapping[, -1]

  return (user_mapping)
}


#' Load sample information to reportInfo object. This is the main reactive object used
#' by the report builder app which is also a named list
#' Used by Report Builder
#'
#' @param con_pathOS PathOS DB connection
#' @param seqrun seqrun, character value
#' @param sample_accession sample accession, character value
#' @param reportInfo Named list from report builder/automatic generation tools with report information
#' @param path_gene_coverage_file path to gene coverage file
#' @param report_config report config variables
#'
#' @return reportInfo
#'
#' @export
loadSampleInfo <- function(con_pathOS, seqrun, sample_accession, reportInfo, path_gene_coverage_file, report_config)
{
  data <-  getSampleInfo(con_pathOS, seqrun, sample_accession)

  #coverage level
  index <- which(names(report_config$coverage_level) == data$panel)
  coverage_level <- unlist(report_config$coverage_level[index])

  #check for hgvsg format and give an error
  if (nrow(data) != 0)
  {
    reportInfo$sample_exists <- T

    coverageData <- getCoverageData(seqrun, sample_accession, path_gene_coverage_file, coverage_level)

    if (nrow(coverageData) != 0)
    {
      #Update the reactive variable
      reportInfo$initialized <- T
      reportInfo$sample_accession <- data$sample_name
      reportInfo$seqrun <- data$seqrun
      reportInfo$specimen_type <- " " #specimen type not available, check
      reportInfo$collected_date <- formatDate(data$collect_date)
      reportInfo$received_date <- formatDate(data$rcvd_date)
      reportInfo$requester <- data$requester
      reportInfo$referral_lab <- data$pathlab
      reportInfo$ext_ref <- data$ext_sample
      reportInfo$patient_name <- data$full_name
      reportInfo$urn <- data$urn
      reportInfo$dob <- formatDate(data$dob)
      reportInfo$gender <- data$sex
      reportInfo$panel <- data$panel
      reportInfo$coverage_level <- coverage_level

      reportInfo$coverage_data <- coverageData

      #Load variant data
      dataVariant <- getVariantInfo(con_pathOS, seqrun, sample_accession, report_config)
      if (nrow(dataVariant) == 0) #No reportable variants found
        reportInfo$variants <- NULL
      else #reportable variants found
        reportInfo$variants <- dataVariant

      #Load fusion data
      dataFusion <- getFusionInfo(con_pathOS, seqrun, sample_accession, report_config)
      if (nrow(dataFusion) == 0) #No reportable variants found
        reportInfo$fusions <- NULL
      else #reportable variants found
        reportInfo$fusions <- dataFusion

      #Reportable variants or fusions
      if ((nrow(dataVariant) != 0) || (nrow(dataFusion) != 0))
        reportInfo$reportable_variants <- T
      else
        reportInfo$reportable_variants <- F
    }
  }

  return (reportInfo)
}


#' Fetch variant info from pathos database
#' Used by Report Builder
#'
#' @param con_pathOS PathOS DB connection
#' @param seqrun seqrun, character value
#' @param sample_accession sample accession, character value
#' @param report_config report config variables
#'
#' @return dataframe with variant information
#'
#' @export
getVariantInfo <- function(con_pathOS, seqrun, sample_accession, report_config)
{
  seqrun <- trimws(seqrun, which="both")
  sample_accession <- trimws(sample_accession, which="both")

  #Check whether the sample, seqrun combination exists
  sample_names <- sample_accession
  sample_names <- c(sample_names, paste0(sample_names, "-1")) #because there are two samples
  sample_names <- paste0("'", sample_names, "'")
  samples_str <- paste(sample_names, collapse = ", ")
  #print(samples_str)

  query <- paste0("SELECT gene, hgvsc, hgvsg, hgvsp, pos, read_depth, refseq_mrna,
                    BIN(reportable) AS reportable, var_freq, var_panel_pct, variant FROM seq_variant
                    INNER JOIN seq_sample ON seq_sample.id = seq_variant.seq_sample_id
                    INNER JOIN seqrun ON seq_sample.seqrun_id = seqrun.id
                    WHERE seq_variant.sample_name IN (", samples_str, ") AND seqrun.seqrun = '", seqrun , "' AND reportable = 1;")
  data <- DBI::dbGetQuery(con_pathOS, query)

  if (nrow(data) == 0)
    return (data)

  #preprocess variants
  data <- data[, c("gene", "hgvsc", "hgvsp", "var_freq")]
  data$hgvsp[data$hgvsp == ""] <- report_config$hgvsp_NA
  data$variant <-  paste0(str_extract(data$hgvsc, "c[.].*$"), " ", str_extract(data$hgvsp, "p[.].*$"))
  df <- data.frame(ReportVariantID=NA, ReportID=NA, AssumedOrigin=NA, Gene=data$gene, Variant=data$variant, VRF=data$var_freq, ClinicalSignificance=NA, stringsAsFactors=F)
  df <- df[order(df$VRF, decreasing=T), ]
  df["VRF"] <- round(as.numeric(df$VRF))
  df["AssumedOrigin"] <- factor(df$AssumedOrigin, levels=report_config$assumed_origin_choices)

  return (df)
}


#' Fetch fusion info from pathos database
#' Used by Report Builder
#'
#' @param con_pathOS PathOS DB connection
#' @param seqrun seqrun, character value
#' @param sample_accession sample accession, character value
#' @param report_config report config variables
#'
#' @return dataframe with fusion information
#'
#' @export
getFusionInfo <- function(con_pathOS, seqrun, sample_accession, report_config)
{
  seqrun <- trimws(seqrun, which="both")
  sample_accession <- trimws(sample_accession, which="both")

  #Check whether the sample, seqrun combination exists
  sample_names <- sample_accession
  sample_names <- c(sample_names, paste0(sample_names, "-1")) #because there are two samples
  sample_names <- paste0("'", sample_names, "'")
  samples_str <- paste(sample_names, collapse = ", ")
  #print(samples_str)

  query <- paste0("SELECT fusion, gene1, gene1exon, gene1transcript, gene2, gene2exon, gene2transcript,
                    BIN(reportable) AS reportable, var_panel_pct, identifier, var_samples_seen_in_panel, var_samples_total_in_panel FROM seq_rna_fusion
                    INNER JOIN seq_sample ON seq_sample.id = seq_rna_fusion.seq_sample_id
                    INNER JOIN seqrun ON seq_sample.seqrun_id = seqrun.id
                    WHERE seq_sample.sample_name IN (", samples_str, ") AND seqrun.seqrun = '", seqrun , "' AND reportable = 1;")
  data <- DBI::dbGetQuery(con_pathOS, query)

  if (nrow(data) == 0)
    return (data)

  #preprocess fusions
  data$fusion <-  sub("[-]", "::", data$fusion)
  data["breakpoint"] <- paste0("exon ", data$gene1exon, "::exon ", data$gene2exon, " (", data$gene1transcript, "::", data$gene2transcript, ")")

  df <- data.frame(ReportFusionID=NA, ReportID=NA, Fusion=data$fusion, Breakpoint=data$breakpoint, ClinicalSignificance=NA, stringsAsFactors=F)

  return (df)
}


#' Load sample information to reportInfo object and report information to reportInfo objects
#' If there is a report existing, only load basic report information if there are multiple reports per sample
#' Used by Report Builder
#'
#' @param con_rb Report builder database connection
#' @param reportInfo Named list from report builder/automatic generation tools with sample and report information
#' @param report_config report config variables
#'
#' @return reportInfo
#'
#' @export
loadReportBuilderData <- function(con_rb, reportInfo, report_config)
{
  query <- paste0("SELECT * FROM Sample
                    WHERE Sample.SampleName = '", reportInfo$sample_accession, "' AND Seqrun = '", reportInfo$seqrun , "';")
  data <- DBI::dbGetQuery(con_rb, query)

  if (nrow(data) != 0)
  {
    #Load information from sample object
    reportInfo$specimen_type <- data$Specimen

    reportInfo$db_sample_id <- data$SampleID
    reportInfo$clinical_indication <- data$ClinicalIndication
    reportInfo$correlative_morphology <- data$CorrelativeMorphology
    reportInfo$specimen_details <- data$SpecimenDetails
    reportInfo$clinical_context_report <- data$InitialClinicalContextReport
    reportInfo$requested_panel <- data$RequestedPanel
    reportInfo$requester_code <- data$RequesterCode

    #Check whether a report exists (sample info can exist without a report if loaded via NVD identifier)
    query <- paste0("SELECT * FROM Report
                    INNER JOIN ReportBuilderInfo ON Report.ReportID = ReportBuilderInfo.ReportID
                    WHERE SampleID = '", data$SampleID, "';")
    report_data <- DBI::dbGetQuery(con_rb, query)

    if (nrow(report_data) > 1) #More than one report existing
    {
      reportInfo$new_report <- F
      reportInfo$db_report_id <- report_data$ReportID
      reportInfo$db_report_builder_info_id <- report_data$ReportBuilderInfoID
      reportInfo$report_template <- report_data$Template
      reportInfo$report_type <- report_data$Type
      reportInfo$report_name <- report_data$Name
      reportInfo$report_status <- report_data$Status
    }
    else if (nrow(report_data) == 1)
    {
      reportInfo$new_report <- F
      reportInfo$db_report_id <- report_data$ReportID
      reportInfo$db_report_builder_info_id <- report_data$ReportBuilderInfoID

      return (loadReportInformation(con_rb, report_data, reportInfo, report_config))
    }
  }

  return (reportInfo)
}


#' Load report information to reportInfo object
#' This call uses the reportID in reportInfo object
#' Used by Report Builder
#'
#' @param con_rb Report builder database connection
#' @param reportInfo Named list from report builder/automatic generation tools with sample and report information
#' @param report_config report config variables
#'
#' @return reportInfo
#'
#' @export
loadReportBuilderReport <- function(con_rb, reportInfo, report_config)
{
  #Check whether a report exists (sample info can exist without a report if loaded via NVD identifier)
  query <- paste0("SELECT * FROM Report
                  INNER JOIN ReportBuilderInfo ON Report.ReportID = ReportBuilderInfo.ReportID
                  WHERE Report.ReportID = '", reportInfo$db_report_id, "';")
  report_data <- DBI::dbGetQuery(con_rb, query)

  return (loadReportInformation(con_rb, report_data, reportInfo, report_config))
}


#' Save report to Report builder database, also fetching report id etc when saved if this is a new report
#' Used by Report Builder
#'
#' @param con_rb Report builder database connection
#' @param reportInfo Named list from report builder/automatic generation tools with sample and report information
#' @param report_config report config variables
#'
#' @return reportInfo
#'
#' @export
saveReport <- function(con_rb, reportInfo, report_config)
{
  sample_id <- NA
  #Sample can already exist in the database
  if (!is.null(reportInfo$db_sample_id))
  {
    sample_id <- reportInfo$db_sample_id
    query <- paste0('UPDATE Sample SET  Specimen="', reportInfo$specimen_type, '",
                                    ClinicalIndication="', escapeQuote(reportInfo$clinical_indication), '",
                                    CorrelativeMorphology="', escapeQuote(reportInfo$correlative_morphology), '",
                                    SpecimenDetails="', escapeQuote(reportInfo$specimen_details), '"
                                    WHERE SampleID ="', sample_id, '";')
    res <- DBI::dbSendQuery (con_rb, query)
    DBI::dbClearResult(res)
  }
  else
  {
    #Insert sample
    query <- paste0('INSERT INTO Sample SET SampleName="', reportInfo$sample_accession, '",
                                          Seqrun="', reportInfo$seqrun, '",
                                          Specimen="', reportInfo$specimen_type, '",
                                          ClinicalIndication="', escapeQuote(reportInfo$clinical_indication), '",
                                          CorrelativeMorphology="', escapeQuote(reportInfo$correlative_morphology), '",
                                          SpecimenDetails="', escapeQuote(reportInfo$specimen_details), '";')
    res <- DBI::dbSendQuery (con_rb, query)
    DBI::dbClearResult(res)
    query <- paste0("SELECT LAST_INSERT_ID();")
    res <- DBI::dbGetQuery (con_rb, query)
    sample_id <- res$`LAST_INSERT_ID()`
    reportInfo$db_sample_id <- sample_id
  }

  #Combined results summary and clinical indication
  results_summary <- paste0(reportInfo$results_summary_var, " ", reportInfo$results_summary_desc, " ",
                            reportInfo$results_summary_flt3, " ", reportInfo$results_summary_dna_rna, " ",
                            reportInfo$results_summary_qual, " ", reportInfo$results_summary_desc_other)
  results_summary <- trimws(results_summary, which="both")
  results_summary <- gsub("[ ]{2,}", " ", results_summary)

  clinical_interpretation <- paste0(reportInfo$clinical_interpretation_txt_var, " ", reportInfo$clinical_interpretation_sel, " ",
                                    reportInfo$clinical_interpretation_txt)
  clinical_interpretation <- trimws(clinical_interpretation, which="both")
  clinical_interpretation <- gsub("[ ]{2,}", " ", clinical_interpretation)

  if (reportInfo$new_report && reportInfo$report_create) #New report but can be a replace
  {
    #Insert Report
    query <- paste0('INSERT INTO Report SET SampleID="', sample_id, '",
                                          Template="', reportInfo$report_template, '",
                                          Type="', reportInfo$report_type, '",
                                          Status="', reportInfo$report_status, '",
                                          Name="', reportInfo$report_name, '",
                                          ResultsSummary="', escapeQuote(results_summary), '",
                                          ClinicalInterpretation="', escapeQuote(clinical_interpretation), '",
                                          ClinicalContextReport="', reportInfo$clinical_context_report, '",
                                          ClinicalContext="', reportInfo$clinical_context, '",
                                          FLT3ITDAnalysis="', escapeQuote(reportInfo$flt3_itd), '",
                                          GermlineVariantAnalysis="', escapeQuote(reportInfo$germline_variant_analysis), '",
                                          VariantConfirmationGene="', reportInfo$vc_gene, '",
                                          Comment="', escapeQuote(reportInfo$comment), '",
                                          AuthorisedBy="', reportInfo$authorised_by, '",
                                          ReportedBy="', reportInfo$reported_by, '",
                                          LastModifiedBy="', reportInfo$session_user, '",
                                          CreatedBy="', reportInfo$session_user, '",
                                          CreatedDate="', Sys.time(), '",
                                          LastModifiedDate="', Sys.time(), '";')
    res <- DBI::dbSendQuery (con_rb, query)
    DBI::dbClearResult(res)
    query <- paste0("SELECT LAST_INSERT_ID();")
    res <- DBI::dbGetQuery (con_rb, query)
    report_id <- res$`LAST_INSERT_ID()`

    #Insert reportInfo
    query <- paste0('INSERT INTO ReportBuilderInfo SET ReportID="', report_id, '",
                                          ResultsSummaryDesc="', escapeQuote(reportInfo$results_summary_desc), '",
                                          ResultsSummaryDescOther="', escapeQuote(reportInfo$results_summary_desc_other), '",
                                          ResultsSummaryFLT3="', reportInfo$results_summary_flt3, '",
                                          ResultsSummaryQual="', reportInfo$results_summary_qual, '",
                                          ResultsSummaryDNARNA="', reportInfo$results_summary_dna_rna, '",
                                          ResultsSummaryHAVCR2Result="', reportInfo$results_summary_havcr2_result, '",
                                          ResultsSummaryHAVCR2Comment="', reportInfo$results_summary_havcr2_comment, '",
                                          ResultsSummaryVCConclusion="', reportInfo$results_summary_vc_conclusion, '",
                                          ResultsSummaryVarDesc="', escapeQuote(reportInfo$results_summary_var), '",
                                          ClinicalInterpretationDesc="', escapeQuote(reportInfo$clinical_interpretation_txt), '",
                                          ClinicalInterpretationOther="', reportInfo$clinical_interpretation_sel, '",
                                          ClinicalInterpretationVarDesc="', escapeQuote(reportInfo$clinical_interpretation_txt_var), '",
                                          ClinicalInterpretationVar="', reportInfo$clinical_interpretation_var, '",
                                          ClinicalInterpretationSpecimen="', reportInfo$clinical_interpretation_specimen, '",
                                          ClinicalInterpretationDisease="', reportInfo$clinical_interpretation_disease, '",
                                          ClinicalInterpretationFusion="', reportInfo$clinical_interpretation_fusion, '",
                                          ClinicalInterpretationPathogenicity="', reportInfo$clinical_interpretation_pathogenicity, '",
                                          ClinicalInterpretationMiscChoices="', reportInfo$clinical_interpretation_misc_choices, '",
                                          ClinicalInterpretationMain="', escapeQuote(reportInfo$clinical_interpretation_main), '",
                                          GermlinePathogenicity="', reportInfo$germline_pathogenicity, '",
                                          GermlineVariantClassification="', reportInfo$germline_classification, '",
                                          GermlineCondition="', reportInfo$germline_condition, '",
                                          VarType="', reportInfo$var_type, '";')
    res <- DBI::dbSendQuery (con_rb, query)
    DBI::dbClearResult(res)
    query <- paste0("SELECT LAST_INSERT_ID();")
    res <- DBI::dbGetQuery (con_rb, query)
    report_builder_id <- res$`LAST_INSERT_ID()`

    if ((reportInfo$report_type == "VAR") && (!is.null(reportInfo$variants)))
    {
      #prepare data and append to the table
      variants <- reportInfo$variants
      variants["ReportID"] <- report_id
      variants$AssumedOrigin <- as.character(variants$AssumedOrigin)
      variants <- variants[, c("ReportID", "Variant", "Gene", "VRF", "AssumedOrigin", "ClinicalSignificance")]
      rownames(variants) <- NULL
      DBI::dbWriteTable(con_rb, "ReportVariant", variants, row.names=F, append=T)
    }

    if ((reportInfo$report_type == "VAR") && (!is.null(reportInfo$fusions)))
    {
      #prepare data and append to the table
      fusions <- reportInfo$fusions
      fusions["ReportID"] <- report_id
      fusions <- fusions[, c("ReportID", "Fusion", "Breakpoint", "ClinicalSignificance")]
      rownames(fusions) <- NULL
      DBI::dbWriteTable(con_rb, "ReportFusion", fusions, row.names=F, append=T)
    }

    #Assign values to report info
    reportInfo$db_report_id <- report_id
    reportInfo$db_report_builder_info_id <- report_builder_id
    reportInfo$new_report <- F #No longer a new report
    reportInfo$report_create <- F #No longer a create
  }
  else #create and replace or modify
  {
    #Insert Report
    query <- paste0('UPDATE Report SET Template="', reportInfo$report_template, '",
                                      Type="', reportInfo$report_type, '",
                                      Status="', reportInfo$report_status, '",
                                      Name="', reportInfo$report_name, '",
                                      ResultsSummary="', escapeQuote(results_summary), '",
                                      ClinicalInterpretation="', escapeQuote(clinical_interpretation), '",
                                      ClinicalContextReport="', reportInfo$clinical_context_report, '",
                                      ClinicalContext="', reportInfo$clinical_context, '",
                                      FLT3ITDAnalysis="', escapeQuote(reportInfo$flt3_itd), '",
                                      GermlineVariantAnalysis="', escapeQuote(reportInfo$germline_variant_analysis), '",
                                      VariantConfirmationGene="', reportInfo$vc_gene, '",
                                      Comment="', escapeQuote(reportInfo$comment), '",
                                      AuthorisedBy="', reportInfo$authorised_by, '",
                                      ReportedBy="', reportInfo$reported_by, '",
                                      LastModifiedBy="', reportInfo$session_user, '",
                                      LastModifiedDate="', Sys.time(), '"
                                      WHERE ReportID = "', reportInfo$db_report_id, '";')
    res <- DBI::dbSendQuery (con_rb, query)
    DBI::dbClearResult(res)

    #Insert reportInfo
    query <- paste0('UPDATE ReportBuilderInfo SET ResultsSummaryDesc="', escapeQuote(reportInfo$results_summary_desc), '",
                                        ResultsSummaryDescOther="', escapeQuote(reportInfo$results_summary_desc_other), '",
                                        ResultsSummaryFLT3="', reportInfo$results_summary_flt3, '",
                                        ResultsSummaryQual="', reportInfo$results_summary_qual, '",
                                        ResultsSummaryDNARNA="', reportInfo$results_summary_dna_rna, '",
                                        ResultsSummaryHAVCR2Result="', reportInfo$results_summary_havcr2_result, '",
                                        ResultsSummaryHAVCR2Comment="', reportInfo$results_summary_havcr2_comment, '",
                                        ResultsSummaryVCConclusion="', reportInfo$results_summary_vc_conclusion, '",
                                        ResultsSummaryVarDesc="', escapeQuote(reportInfo$results_summary_var), '",
                                        ClinicalInterpretationDesc="', escapeQuote(reportInfo$clinical_interpretation_txt), '",
                                        ClinicalInterpretationOther="', reportInfo$clinical_interpretation_sel, '",
                                        ClinicalInterpretationVarDesc="', escapeQuote(reportInfo$clinical_interpretation_txt_var), '",
                                        ClinicalInterpretationVar="', reportInfo$clinical_interpretation_var, '",
                                        ClinicalInterpretationSpecimen="', reportInfo$clinical_interpretation_specimen, '",
                                        ClinicalInterpretationDisease="', reportInfo$clinical_interpretation_disease, '",
                                        ClinicalInterpretationFusion="', reportInfo$clinical_interpretation_fusion, '",
                                        ClinicalInterpretationPathogenicity="', reportInfo$clinical_interpretation_pathogenicity, '",
                                        ClinicalInterpretationMiscChoices="', reportInfo$clinical_interpretation_misc_choices, '",
                                        ClinicalInterpretationMain="', escapeQuote(reportInfo$clinical_interpretation_main), '",
                                        GermlinePathogenicity="', reportInfo$germline_pathogenicity, '",
                                        GermlineVariantClassification="', reportInfo$germline_classification, '",
                                        GermlineCondition="', reportInfo$germline_condition, '",
                                        VarType="', reportInfo$var_type, '"
                                        WHERE ReportBuilderInfoID ="', reportInfo$db_report_builder_info_id, '";')
    res <- DBI::dbSendQuery (con_rb, query)
    DBI::dbClearResult(res)

    #Now match existing variants on DB and the variants in reportInfo and perform modify, insert or delete
    query <- paste0("SELECT * FROM ReportVariant
                WHERE ReportID = '", reportInfo$db_report_id, "';")
    existingVariants <- DBI::dbGetQuery(con_rb, query)

    existingVariants <- existingVariants[order(existingVariants$ReportVariantID, decreasing=F), ]
    existingVariants$VRF[existingVariants$VRF == "NA"] <- NA
    existingVariants["VRF"] <- round(as.numeric(existingVariants$VRF))
    existingVariants["AssumedOrigin"] <- factor(existingVariants$AssumedOrigin, levels=report_config$assumed_origin_choices)

    changedVariants <- reportInfo$variants
    if (is.null(changedVariants))
      changedVariants <- data.frame(ReportVariantID=integer(0), ReportID=integer(0), AssumedOrigin=character(0),
                                    Gene=character(0), Variant=character(0), VRF=numeric(0), ClinicalSignificance=character(0))

    existingVariants["Row"] <- apply(existingVariants, 1, function(col){paste(as.character(col), collapse = "::")})
    changedVariants["Row"] <- apply(changedVariants, 1, function(col){paste(as.character(col), collapse = "::")})

    changedDiff <- changedVariants[which(changedVariants$Row == setdiff(changedVariants$Row, existingVariants$Row)), ]
    existingDiff <- existingVariants[which(existingVariants$Row == setdiff(existingVariants$Row, changedVariants$Row)), ]

    #Record with same report variant ID are modifications
    indexes_modified <- which(changedDiff$ReportVariantID %in% existingDiff$ReportVariantID)
    if (length(indexes_modified) > 0)
    {
      for(index_modified in indexes_modified)
      {
        record <- changedDiff[index_modified, ]
        query <- paste0('UPDATE ReportVariant SET Variant="', record$Variant, '",
                                            Gene="', record$Gene, '",
                                            VRF="', record$VRF, '",
                                            AssumedOrigin="', record$AssumedOrigin, '",
                                            ClinicalSignificance="', record$ClinicalSignificance, '"
                                            WHERE ReportVariantID = "', record$ReportVariantID, '";')
        res <- DBI::dbSendQuery (con_rb, query)
        DBI::dbClearResult(res)
      }
    }
    #Existing in changed but not in DB means inserts
    records_inserted <- changedDiff[which(!(changedDiff$ReportVariantID %in% existingDiff$ReportVariantID)), ]
    if (nrow(records_inserted) > 0)
    {
      for(i in 1:nrow(records_inserted))
      {
        record <- records_inserted[i, ]
        query <- paste0('INSERT ReportVariant SET ReportID="', reportInfo$db_report_id, '",
                                            Variant="', record$Variant, '",
                                            Gene="', record$Gene, '",
                                            VRF="', record$VRF, '",
                                            AssumedOrigin="', record$AssumedOrigin, '",
                                            ClinicalSignificance="', record$ClinicalSignificance, '";')

        res <- DBI::dbSendQuery (con_rb, query)
        DBI::dbClearResult(res)
      }
    }
    #If existing in existing but not in changed then deleted
    records_deleted <- existingDiff[which(!(existingDiff$ReportVariantID %in% changedDiff$ReportVariantID)), ]
    if (nrow(records_deleted) > 0)
    {
      for(i in 1:nrow(records_deleted))
      {
        record <- records_deleted[i, ]
        query <- paste0("DELETE FROM ReportVariant WHERE ReportVariantID='", record$ReportVariantID, "';")

        res <- DBI::dbSendQuery (con_rb, query)
        DBI::dbClearResult(res)
      }
    }


    #Now match existing fusions on DB and the fusions in reportInfo and perform modify, insert or delete
    query <- paste0("SELECT * FROM ReportFusion
                WHERE ReportID = '", reportInfo$db_report_id, "';")
    existingFusions <- DBI::dbGetQuery(con_rb, query)

    existingFusions <- existingFusions[order(existingFusions$ReportFusionID, decreasing=F), ]
    changedFusions <- reportInfo$fusions
    if (is.null(changedFusions))
      changedFusions <- data.frame(ReportFusionID=integer(0), ReportID=integer(0), Fusion=character(0), Breakpoint=character(0), ClinicalSignificance=character(0))

    existingFusions["Row"] <- apply(existingFusions, 1, function(col){paste(as.character(col), collapse = "::")})
    changedFusions["Row"] <- apply(changedFusions, 1, function(col){paste(as.character(col), collapse = "::")})

    changedDiff <- changedFusions[which(changedFusions$Row == setdiff(changedFusions$Row, existingFusions$Row)), ]
    existingDiff <- existingFusions[which(existingFusions$Row == setdiff(existingFusions$Row, changedFusions$Row)), ]

    #Record with same report variant ID are modifications
    indexes_modified <- which(changedDiff$ReportFusionID %in% existingDiff$ReportFusionID)
    if (length(indexes_modified) > 0)
    {
      for(index_modified in indexes_modified)
      {
        record <- changedDiff[index_modified, ]
        query <- paste0('UPDATE ReportFusion SET Fusion="', record$Fusion, '",
                                                Breakpoint="', record$Breakpoint, '",
                                                ClinicalSignificance="', record$ClinicalSignificance, '"
                                            WHERE ReportFusionID = "', record$ReportFusionID, '";')
        res <- DBI::dbSendQuery (con_rb, query)
        DBI::dbClearResult(res)
      }
    }
    #Existing in changed but not in DB means inserts
    records_inserted <- changedDiff[which(!(changedDiff$ReportFusionID %in% existingDiff$ReportFusionID)), ]
    if (nrow(records_inserted) > 0)
    {
      for(i in 1:nrow(records_inserted))
      {
        record <- records_inserted[i, ]
        query <- paste0('INSERT ReportFusion SET ReportID="', reportInfo$db_report_id, '",
                                            Fusion="', record$Fusion, '",
                                            Breakpoint="', record$Breakpoint, '",
                                            ClinicalSignificance="', record$ClinicalSignificance, '";')

        res <- DBI::dbSendQuery (con_rb, query)
        DBI::dbClearResult(res)
      }
    }
    #If existing in existing but not in changed then deleted
    records_deleted <- existingDiff[which(!(existingDiff$ReportFusionID %in% changedDiff$ReportFusionID)), ]
    if (nrow(records_deleted) > 0)
    {
      for(i in 1:nrow(records_deleted))
      {
        record <- records_deleted[i, ]
        query <- paste0("DELETE FROM ReportFusion WHERE ReportFusionID='", record$ReportFusionID, "';")

        res <- DBI::dbSendQuery (con_rb, query)
        DBI::dbClearResult(res)
      }
    }
  }

  return (reportInfo)
}


#' Load samples of the seqrun from PathOS
#' Used by NVD Identifier
#'
#' @param con_pathOS PathOS database connection
#' @param seqrun seqrun
#'
#' @return a dataframe with sample information
#'
#' @export
loadSamples <- function(con_pathOS, seqrun)
{
  query <- paste0("SELECT seqrun.seqrun AS Seqrun, seq_sample.id AS SeqSampleID, seq_sample.sample_name AS SampleName, panel.manifest AS Panel FROM seq_sample
                    INNER JOIN seqrun ON seq_sample.seqrun_id = seqrun.id
                    INNER JOIN panel ON panel.id = seq_sample.panel_id
                    WHERE seqrun.seqrun = '", seqrun , "';")
  samples_df <- dbGetQuery(con_pathOS, query)

  return (samples_df)
}

#' Load variants of the samples in seqrun from PathOS
#' Used by NVD Identifier
#'
#' @param con_pathOS PathOS database connection
#' @param samples vector of sample accessions
#' @param seqrun seqrun
#'
#' @return a dataframe with variant information
#'
#' @export
loadVariants <- function(con_pathOS, samples, seqrun)
{
  samples <- paste0("'", samples, "'")
  samples_str <- paste(samples, collapse = ", ")
  query <- paste0("SELECT seq_variant.gene AS Gene, seq_variant.chr AS Chr, seq_variant.hgvsc AS Hgvsc, seq_variant.hgvsg AS Hgvsg,
                    seq_variant.hgvsp AS Hgvsp, seq_variant.pos AS Pos, seq_variant.var_freq AS VarFreq, seq_variant.var_panel_pct AS VarPanelPct,
                    filter_flag AS FilterFlag, seq_variant.var_samples_seen_in_panel AS VarSamplesSeenInPanel, seq_variant.sample_name AS SampleAccession,
                    cur_variant.overall_class AS ClinicalSignificance, cur_variant.pm_class AS ACMGClassification FROM seq_variant
                    INNER JOIN seq_sample ON seq_sample.id = seq_variant.seq_sample_id
                    INNER JOIN seqrun ON seq_sample.seqrun_id = seqrun.id
                    LEFT OUTER JOIN cur_variant ON seq_sample.clin_context_id = cur_variant.clin_context_id AND cur_variant.hgvsg = seq_variant.hgvsg
                    WHERE seq_variant.sample_name IN (", samples_str, ") AND seqrun.seqrun = '", seqrun , "';")
  data_all <- dbGetQuery(con_pathOS, query)

  return (data_all)
}


#' Load information in PathOS DB to prepare for saving a subset of samples of a seqrun
#' Used by NVD Identifier
#'
#' @param con_pathOS PathOS database connection
#' @param samples vector of sample accessions
#' @param seqrun seqrun
#' @param report_config report config variables
#'
#' @return a list of reportInfo objects one per sample (named list)
#'
#' @export
loadPathOSData <- function(con_pathOS, samples, seqrun, report_config)
{
  lst_reportInfo <- list()

  #Check whether the sample, seqrun combination exists
  sample_names <- samples
  sample_names <- paste0("'", sample_names, "'")
  samples_str <- paste(sample_names, collapse = ", ")
  #print(samples_str)

  query <- paste0("SELECT patient.*, seqrun.seqrun, pat_sample.collect_date, pat_sample.rcvd_date,
                    pat_sample.requester, pat_sample.pathlab, pat_sample.ext_sample, seq_sample.id as seq_sample_id,
                    seq_sample.sample_name, panel.manifest as panel FROM seq_sample
                    INNER JOIN seqrun ON seq_sample.seqrun_id = seqrun.id
                    INNER JOIN pat_sample ON seq_sample.pat_sample_id = pat_sample.id
                    INNER JOIN panel ON panel.id = seq_sample.panel_id
                    INNER JOIN patient ON pat_sample.patient_id = patient.id
                    WHERE seq_sample.sample_name IN (", samples_str, ") AND seqrun.seqrun = '", seqrun , "';")
  data <- dbGetQuery(con_pathOS, query)
  data <- data[!duplicated(data), ]

  #coverage level
  index <- which(names(report_config$coverage_level) == unique(data$panel))
  coverage_level <- unlist(report_config$coverage_level[index])

  #Create a named list per sample and add to the list
  for (i in 1:nrow(data))
  {
    dataItem <- data[i,]
    reportInfo <- list()
    reportInfo$sample_accession <- dataItem$sample_name
    reportInfo$seqrun <- dataItem$seqrun
    reportInfo$specimen_type <- NA #specimen type not available, check
    reportInfo$collected_date <- formatDate(dataItem$collect_date)
    reportInfo$received_date <- formatDate(dataItem$rcvd_date)
    reportInfo$requester <- dataItem$requester
    reportInfo$referral_lab <- dataItem$pathlab
    reportInfo$ext_ref <- dataItem$ext_sample
    reportInfo$patient_name <- dataItem$full_name
    reportInfo$urn <- dataItem$urn
    reportInfo$dob <- formatDate(dataItem$dob)
    reportInfo$gender <- dataItem$sex
    reportInfo$panel <- dataItem$panel
    reportInfo$coverage_level <- coverage_level

    lst_reportInfo[[i]] <- reportInfo
  }

  return(lst_reportInfo)
}


#' Check whether samples/reports exist in report builder database for the seqrun
#' Used by NVD Identifier
#'
#' @param con_rb Report builder database connection
#' @param seqrun seqrun
#'
#' @return a data frame with Sample, Report and ReportBuilderInfo tables
#'
#' @export
loadReportBuilderInfo <- function(con_rb, seqrun)
{
  query <- paste0("SELECT * FROM Sample
                    WHERE Seqrun = '", seqrun , "';")
  data <- dbGetQuery(con_rb, query)

  if (nrow(data) != 0) #Samples exist in report builder DB
  {
    sampleID_str <- paste0("'", data$SampleID, "'")
    sampleID_str <- paste(sampleID_str, collapse = ", ")

    query <- paste0("SELECT * FROM Report
                    INNER JOIN ReportBuilderInfo ON Report.ReportID = ReportBuilderInfo.ReportID
                    WHERE SampleID IN (", sampleID_str, ");")
    report_data <- dbGetQuery(con_rb, query)

    if(nrow(report_data) != 0)
    {
      data <- base::merge(data, report_data, all=T)
      data <- subset(data, select=-c(ReportID.1))
    }
    else
    {
      #Have the columns ready and add NA
      vals_NA <- rep(NA, nrow(data))
      data_other <- data.frame(ReportID=vals_NA, Template=vals_NA, Type=vals_NA, Name=vals_NA, Status=vals_NA, ResultsSummary=vals_NA, ClinicalInterpretation=vals_NA, ClinicalContextReport=vals_NA,
                               ClinicalContext=vals_NA, FLT3ITDAnalysis=vals_NA, GermlineVariantAnalysis=vals_NA, VariantConfirmationGene=vals_NA, Comment=vals_NA, AuthorisedBy=vals_NA,
                               ReportedBy=vals_NA, SecondCheckedBy=vals_NA, CreatedBy=vals_NA, CreatedDate=vals_NA, LastModifiedBy=vals_NA, LastModifiedDate=vals_NA, ReportBuilderInfoID=vals_NA,
                               ResultsSummaryDesc=vals_NA, ResultsSummaryDescOther=vals_NA, ResultsSummaryFLT3=vals_NA, ResultsSummaryQual=vals_NA, ResultsSummaryDNARNA=vals_NA, ResultsSummaryVarDesc=vals_NA,
                               ResultsSummaryHAVCR2Result=vals_NA, ResultsSummaryHAVCR2Comment=vals_NA, ResultsSummaryVCConclusion=vals_NA, ClinicalInterpretationDesc=vals_NA,
                               ClinicalInterpretationOther=vals_NA, ClinicalInterpretationVarDesc=vals_NA, ClinicalInterpretationVar=vals_NA, ClinicalInterpretationSpecimen=vals_NA,
                               ClinicalInterpretationDisease=vals_NA, ClinicalInterpretationFusion=vals_NA, ClinicalInterpretationPathogenicity=vals_NA, ClinicalInterpretationMiscChoices=vals_NA, ClinicalInterpretationMain=vals_NA,
                               GermlinePathogenicity=vals_NA, GermlineVariantClassification=vals_NA, GermlineCondition=vals_NA, VarType=vals_NA)
      data <- cbind(data, data_other)
    }
  }
  else
  {
    data <- data.frame(SampleID=numeric(0), SampleName=character(0), Seqrun=character(0), Specimen=character(0), ClinicalIndication=character(0), CorrelativeMorphology=character(0),
                       SpecimenDetails=character(0), RequestedPanel=character(0), InitialClinicalContextReport=character(0), RequesterCode=character(0), ReportID=numeric(0), Template=character(0), Type=character(0),
                       Name=character(0), Status=character(0), ResultsSummary=character(0), ClinicalInterpretation=character(0), ClinicalContextReport=character(0), ClinicalContext=character(0),
                       FLT3ITDAnalysis=character(0), GermlineVariantAnalysis=character(0), VariantConfirmationGene=character(0), Comment=character(0), AuthorisedBy=character(0),
                       ReportedBy=character(0), SecondCheckedBy=character(0), CreatedBy=character(0), CreatedDate=as.Date(character(0)), LastModifiedBy=character(0), LastModifiedDate=as.Date(character(0)),
                       ReportBuilderInfoID=numeric(0), ResultsSummaryDesc=character(0), ResultsSummaryDescOther=character(0), ResultsSummaryFLT3=character(0), ResultsSummaryQual=character(0), ResultsSummaryDNARNA=character(0),
                       ResultsSummaryVarDesc=character(0), ResultsSummaryHAVCR2Result=character(0), ResultsSummaryHAVCR2Comment=character(0), ResultsSummaryVCConclusion=character(0), ClinicalInterpretationDesc=character(0),
                       ClinicalInterpretationOther=character(0), ClinicalInterpretationVarDesc=character(0), ClinicalInterpretationVar=character(0), ClinicalInterpretationSpecimen=character(0),
                       ClinicalInterpretationDisease=character(0), ClinicalInterpretationFusion=character(0), ClinicalInterpretationPathogenicity=character(0), ClinicalInterpretationMiscChoices=character(0), ClinicalInterpretationMain=character(0), GermlinePathogenicity=character(0),
                       GermlineVariantClassification=character(0), GermlineCondition=character(0), VarType=character(0))
  }

  return (data)
}


#' Save sample information from data file to report builder db
#' Used by NVD Identifier
#'
#' @param con_rb Report builder database connection
#' @param data data with sample information
#'
#'
#' @export
saveSampleInfo <- function(con_rb, data)
{
  sample_data <- data[, c("SampleName", "Seqrun", "Specimen", "ClinicalIndication", "CorrelativeMorphology", "SpecimenDetails", "RequestedPanel", "InitialClinicalContextReport", "RequesterCode")]
  DBI::dbWriteTable(con_rb, "Sample", sample_data, row.names=F, append=T)
}

#' Save NVD reports to report builder db
#' Used by NVD Identifier
#'
#' @param con_rb Report builder database connection
#' @param report_DB_data data with NVD report information, one row per report, all tables in one row
#' @param seqrun seqrun
#'
#'
#' @export
saveNVDReports <- function(con_rb, report_DB_data, seqrun)
{
  #insert samples, if SampleID is not NA update, otherwise insert
  sample_data <- report_DB_data[, c("SampleID", "SampleName", "Seqrun", "Specimen", "ClinicalIndication", "CorrelativeMorphology", "SpecimenDetails")]
  index_samples <- which(!is.na(sample_data$SampleID))
  if (length(index_samples) > 0)
  {
    sample_data_existing <- sample_data[index_samples, ]
    sample_data <- sample_data[-index_samples, ]

    for (i in 1:nrow(sample_data_existing))
    {
      query <- paste0('UPDATE Sample SET  Specimen="', sample_data_existing[i, ]$Specimen, '",
                                    ClinicalIndication="', escapeQuote(sample_data_existing[i, ]$ClinicalIndication), '",
                                    CorrelativeMorphology="', escapeQuote(sample_data_existing[i, ]$CorrelativeMorphology), '",
                                    SpecimenDetails="', escapeQuote(sample_data_existing[i, ]$SpecimenDetails), '"
                                    WHERE SampleID = "', sample_data_existing[i, ]$SampleID, '";')
      res <- DBI::dbSendQuery (con_rb, query)
      DBI::dbClearResult(res)
    }
  }

  #Add new samples
  if (nrow(sample_data) > 0)
  {
    sample_data <- sample_data[, -1] #Remove SampleID column
    DBI::dbWriteTable(con_rb, "Sample", sample_data, row.names=F, append=T)

    #Get sample IDs
    query <- paste0("SELECT * FROM Sample
                    WHERE Seqrun = '", seqrun, "';")
    data <- dbGetQuery(con_rb, query)
    report_DB_data$SampleID <- sapply(report_DB_data$SampleName, function(sample_name, data)
    {
      index <- (which(data$SampleName == sample_name))
      data[index, ]$SampleID
    }, data=data)
  }

  #Add new reports
  report_data <- report_DB_data[, c("SampleID", "Template", "Type", "Name", "Status", "ResultsSummary", "ClinicalInterpretation", "ClinicalContextReport", "ClinicalContext", "AuthorisedBy", "ReportedBy",
                                    "CreatedBy", "CreatedDate", "LastModifiedBy", "LastModifiedDate", "FLT3ITDAnalysis", "GermlineVariantAnalysis", "VariantConfirmationGene", "Comment")]
  DBI::dbWriteTable(con_rb, "Report", report_data, row.names=F, append=T)
  #Get sample IDs
  query <- paste0("SELECT * FROM Report INNER JOIN Sample ON Sample.SampleID = Report.SampleID
                    WHERE Sample.Seqrun = '", seqrun, "';")
  data <- dbGetQuery(con_rb, query)
  report_DB_data$ReportID <- sapply(report_DB_data$SampleName, function(sample_name, data)  #We can do this because a Sample Seqrun combination with more than one reports cannot be here
  {
    index <- (which(data$SampleName == sample_name))
    data[index, ]$ReportID
  }, data=data)


  #Add new ReportBuilderInfo
  report_builderInfo_data <- report_DB_data[, c("ReportID", "ResultsSummaryDesc", "ResultsSummaryDescOther", "ResultsSummaryFLT3", "ResultsSummaryQual", "ResultsSummaryDNARNA", "ResultsSummaryHAVCR2Result", "ResultsSummaryHAVCR2Comment", "ResultsSummaryVCConclusion",
                                                "ClinicalInterpretationDesc", "ClinicalInterpretationOther", "ResultsSummaryVarDesc", "ClinicalInterpretationVarDesc", "ClinicalInterpretationVar", "ClinicalInterpretationSpecimen",
                                                "ClinicalInterpretationDisease", "ClinicalInterpretationFusion", "ClinicalInterpretationPathogenicity", "ClinicalInterpretationMiscChoices", "ClinicalInterpretationMain", "GermlinePathogenicity", "GermlineVariantClassification", "GermlineCondition", "VarType")]
  DBI::dbWriteTable(con_rb, "ReportBuilderInfo", report_builderInfo_data, row.names=F, append=T)
}


#' Function to load all reports from report builder database
#' Used by Report Browser
#'
#' @param con_rb Report builder database connection
#'
#' @return All reports
#'
#' @export
loadAllReports <- function(con_rb)
{
  query <- paste0("SELECT s.SampleName, s.Seqrun, s.Specimen, s.ClinicalIndication, s.CorrelativeMorphology, s.SpecimenDetails,
                  r.Template, r.Type, r.Name, r.Status, r.ResultsSummary, r.ClinicalInterpretation, r.ClinicalContext,
                  r.AuthorisedBy, r.ReportedBy, COUNT(DISTINCT rv.ReportVariantID) AS NumReportedVariants,
                  COUNT(DISTINCT rf.ReportFusionID) AS NumReportedFusions FROM Report r
                  LEFT JOIN Sample s ON s.SampleID = r.SampleID
                  LEFT JOIN ReportVariant rv ON rv.ReportID = r.ReportID
                  LEFT JOIN ReportFusion rf ON rf.ReportID = r.ReportID
                  GROUP BY r.ReportID;")
  report_data <- dbGetQuery(con_rb, query)

  return (report_data)
}

#' Function to check whether there are other possible samples for the same patient
#'
#' @param con_pathOS PathOS DB connection
#' @param reportInfo Named list from report builder/automatic generation tools with sample and report information
#' @param report_config report config variables
#'
#' @return data frame of matching patient name, sample name and collection dates
#'
#' @export
matchingPatientSamples <- function(con_pathOS, reportInfo, report_config)
{
  #split the name on space or , and check with like for every part
  #escape ' if existing in name
  full_name <- gsub("'", "''", reportInfo$patient_name)
  name_subs <- unlist(strsplit(full_name, "( |,)"))
  if (length(name_subs) == 0)
    return (data.frame())

  query_name_subs <- character(0)
  for(name_sub in name_subs)
  {
    query_name_subs <- c(query_name_subs, paste0("UPPER(full_name) LIKE UPPER('%",  name_sub,"%')"))
  }
  query_name <- paste(query_name_subs, collapse = " OR ")
  query_name <- paste0("(", query_name, ")")

  #Check whether the dob looks valid, if so match dob
  query_dob <- ""
  dob <- as.Date(reportInfo$dob, format="%d-%b-%Y")
  if (!is.na(dob))
  {
    if (as.numeric(format(dob,'%Y')) > 1901) # There are a lot of records as dob 1900/01/01. Removing those and other invalid entries
      query_dob <- paste0(" AND dob = '", formatDateDB(dob) , "'")
  }

  query_gender <- ""
  if (reportInfo$gender %in% c("F", "M")) #Check with gender if gender seem to be specified correctly
    query_gender <- paste0(" AND sex = '", reportInfo$gender , "'")

  data_all <- data.frame(full_name=character(0), sex=character(0), dob=character(0), urn=character(0), seqrun=character(0),
                     collect_date=character(0), sample_name=character(0), panel=character(0))
  query <- paste0("SELECT patient.full_name, patient.sex, patient.dob, patient.urn, seqrun.seqrun, pat_sample.collect_date,
                    seq_sample.sample_name, panel.manifest as panel FROM seq_sample
                    INNER JOIN seqrun ON seq_sample.seqrun_id = seqrun.id
                    INNER JOIN pat_sample ON seq_sample.pat_sample_id = pat_sample.id
                    INNER JOIN panel ON panel.id = seq_sample.panel_id
                    INNER JOIN patient ON pat_sample.patient_id = patient.id
                    WHERE ", query_name, query_gender, query_dob, ";")
  tryCatch({
    data_all <- dbGetQuery(con_pathOS, query)
    data_all["reported_variants"] <- "N/A"
  }, error=function(e){
    cat(paste0(e, "\n"))
    cat("Error in fetching matching records\n")
  })

  #Fetched patient full name can have partial word matches. So subset for full words
  data <- data.frame(full_name=character(0), sex=character(0), dob=character(0), urn=character(0), seqrun=character(0),
                         collect_date=character(0), sample_name=character(0), panel=character(0))
  if (nrow(data_all) > 0)
  {
    if (length(name_subs) == 1)
    {
      data <- data_all[which(grepl(paste0("\\<", toupper(name_subs[1]) , "\\>"), toupper(data_all$full_name))), ]
    }
    else if (length(name_subs) > 1)
    {
      for(name_sub in name_subs)
      {
        data_sub <- data_all[which(grepl(paste0("\\<", toupper(name_sub) , "\\>"), toupper(data_all$full_name))), ]
        data <- rbind(data, data_sub)
      }
    }
  }

  #Fetch reported variants of the samples
  sample_accessions <- data$sample_name

  #Check whether the sample, seqrun combination exists
  sample_names <- sample_accessions
  sample_names <- c(sample_names, paste0(sample_names, "-1")) #because there are two samples
  sample_names <- paste0("'", sample_names, "'")
  samples_str <- paste(sample_names, collapse = ", ")
  #print(samples_str)

  query <- paste0("SELECT seq_sample.sample_name, seqrun.seqrun, gene, hgvsc, hgvsg, hgvsp, pos, read_depth, refseq_mrna,
                    BIN(reportable) AS reportable, var_freq, var_panel_pct, variant FROM seq_variant
                    INNER JOIN seq_sample ON seq_sample.id = seq_variant.seq_sample_id
                    INNER JOIN seqrun ON seq_sample.seqrun_id = seqrun.id
                    WHERE seq_variant.sample_name IN (", samples_str, ") AND reportable = 1;")
  df_variants <- DBI::dbGetQuery(con_pathOS, query)

  if (nrow(df_variants) > 0)
  {
    #preprocess variants
    df_variants <- df_variants[, c("sample_name", "seqrun", "gene", "hgvsc", "hgvsp", "var_freq")]
    df_variants$hgvsp[df_variants$hgvsp == ""] <- report_config$hgvsp_NA
    df_variants$variant <-  paste0(df_variants$gene, ": ", str_extract(df_variants$hgvsc, "c[.].*$"), " ", str_extract(df_variants$hgvsp, "p[.].*$"), " ", round(df_variants$var_freq, 1))

    for(i in 1:nrow(data))
    {
      #Reported variants
      data_var_sub <- subset(df_variants, df_variants$sample_name == data[i, ]$sample_name & df_variants$seqrun == data[i, ]$seqrun)
      if (nrow(data_var_sub) > 0)
        data[i, ]$reported_variants <- paste(data_var_sub$variant, collapse = "\n ")
    }
  }

  #Fetch reported fusions of the samples
  sample_accessions <- data$sample_name

  #Check whether the sample, seqrun combination exists
  sample_names <- sample_accessions
  sample_names <- c(sample_names, paste0(sample_names, "-1")) #because there are two samples
  sample_names <- paste0("'", sample_names, "'")
  samples_str <- paste(sample_names, collapse = ", ")
  #print(samples_str)

  query <- paste0("SELECT seq_sample.sample_name, seqrun.seqrun, fusion,
                    BIN(reportable) AS reportable FROM seq_rna_fusion
                    INNER JOIN seq_sample ON seq_sample.id = seq_rna_fusion.seq_sample_id
                    INNER JOIN seqrun ON seq_sample.seqrun_id = seqrun.id
                    WHERE seq_sample.sample_name IN (", samples_str, ") AND reportable = 1;")
  df_fusions <- DBI::dbGetQuery(con_pathOS, query)

  if (nrow(df_fusions) > 0)
  {
    #preprocess variants
    df_fusions <- df_fusions[, c("sample_name", "seqrun", "fusion")]

    for(i in 1:nrow(data))
    {
      #Reported variants
      data_var_sub <- subset(df_fusions, df_fusions$sample_name == data[i, ]$sample_name & df_fusions$seqrun == data[i, ]$seqrun)
      if (nrow(data_var_sub) > 0)
        data[i, ]$reported_variants <- paste(data_var_sub$fusion, collapse = "\n ")
    }
  }

  return (data)
}


#' Function to generate report template using list object reportInfo
#'
#' @param reportInfo Named list from report builder/automatic generation tools with sample and report information
#' @param report_config Report config variables
#' @param coverage_data Report coverage table definitions to generate the coverage table
#'
#' @return report_template which is a word document template
#'
#' @export
generateReportTemplate <- function(reportInfo, report_config, coverage_data)
{
  #Filter coverage data based on the assay/panel
  index <- which(names(report_config$report_panel_version) == reportInfo$panel)
  assay_version <- unlist(report_config$report_panel_version[index])

  if (reportInfo$report_template != "RNA_v1")
  {
    coverage_data_sub <- subset(coverage_data, coverage_data$Assay == assay_version)
    coverage_data_sub <- subset(coverage_data_sub, select=-c(Assay))

    #generate the coverage table and format
    if (reportInfo$report_type != "FAIL")
    {
      data_coverage <- returnCoverageTable(reportInfo$coverage_data, reportInfo$report_template, reportInfo$vc_gene, report_config, coverage_data_sub, reportInfo$coverage_level)
      coverage_table <- coverageTableThemed(data_coverage, reportInfo$coverage_level)
    }
    else
    {
      data_coverage <- returnCoverageTableFail(reportInfo$report_template, reportInfo$vc_gene, report_config, coverage_data_sub)
      coverage_table <- coverageTableThemedFail(data_coverage)
    }
  }

  #Read the relevant template
  template <- reportInfo$report_template
  if (template == "MDX_MPN")
    template <- "MDX"
  else if ((template == "AHD_DDX41") && (reportInfo$report_type != "VAR"))
    template <- "AHD"

  template_name <- paste0(assay_version, "_", template, "_", reportInfo$report_type, ".docx")

  report_template <- officer::read_docx(system.file("templates", template_name, package = "reportWriter", mustWork=T))

  #replace variable's in the template
  #Patient info header
  report_template <- officer::body_replace_all_text(report_template, report_config$Patient, reportInfo$patient_name)
  report_template <- officer::body_replace_all_text(report_template, report_config$Urn, reportInfo$urn)
  report_template <- officer::body_replace_all_text(report_template, report_config$Dob,  reportInfo$dob)
  report_template <- officer::body_replace_all_text(report_template, report_config$Lab_No, reportInfo$sample_accession)
  report_template <- officer::body_replace_all_text(report_template, report_config$Sex, reportInfo$gender)
  report_template <- officer::body_replace_all_text(report_template, report_config$Ext_Ref, ifelse(is.na(reportInfo$ext_ref), report_config$val_na, reportInfo$ext_ref))
  report_template <- officer::body_replace_all_text(report_template, report_config$Collected_Date,  reportInfo$collected_date)
  report_template <- officer::body_replace_all_text(report_template, report_config$Received_Date,  reportInfo$received_date)
  report_template <- officer::body_replace_all_text(report_template, report_config$Specimen, reportInfo$specimen_type)
  report_template <- officer::body_replace_all_text(report_template, report_config$Requester, reportInfo$requester)
  report_template <- officer::body_replace_all_text(report_template, report_config$Referral_Lab, reportInfo$referral_lab)

  #Report comment
  if (identical(reportInfo$comment, report_config$section_Delete))
  {
    report_template <- officer::cursor_reach(report_template, report_config$Report_Comment)
    report_template <- officer::body_add_par(report_template, "", pos="before")
    report_template <- officer::cursor_reach(report_template, report_config$Report_Comment)
    report_template <- officer::body_remove(report_template)
  }
  else
    report_template <- officer::body_replace_all_text(report_template, report_config$Report_Comment, reportInfo$comment)

  #Report area
  report_template <- officer::body_replace_all_text(report_template, report_config$Specimen_Details, reportInfo$specimen_details)
  report_template <- officer::body_replace_all_text(report_template, report_config$Authorised_By, reportInfo$authorised_by)
  if (reportInfo$report_template != "SGVC")
    report_template <- officer::body_replace_all_text(report_template, report_config$Clinical_Indication, reportInfo$clinical_indication)

  #save "reported by"
  report_template <- officer::body_replace_all_text(report_template, report_config$Reported_By, reportInfo$reported_by)


  if (reportInfo$report_type == "NEG")
  {
    report_template <- negativeReportResultsSection(report_template, reportInfo, report_config)
  }
  else if (reportInfo$report_type == "VAR")
  {
    report_template <- variantsReportResultsSection(report_template, reportInfo, report_config)
  }
  else #failed
  {
    results_summary <- trimws(reportInfo$results_summary_dna_rna, which="both")

    report_template <- officer::body_replace_all_text(report_template, report_config$Results_Summary, results_summary)
  }

  #Variant confirmation single gene report
  if (reportInfo$report_template == "SGVC")
  {
    report_template <- officer::body_replace_all_text(report_template, report_config$VC_Gene, reportInfo$vc_gene)
  }

  if (reportInfo$report_template != "AH_cfDNA")
  {
    report_template <- officer::body_replace_all_text(report_template, report_config$Correlative_Morphology, reportInfo$correlative_morphology)
  }

  #FLT3_ITD and DDX41 germline variant analysis in reports with variants
  if (reportInfo$report_type == "VAR")
  {
    if (reportInfo$report_template %in% c("AH", "AHD", "AHD_DDX41"))
    {
      if (identical(reportInfo$flt3_itd, report_config$section_Delete))
      {
        report_template <- officer::cursor_reach(report_template, report_config$Flt3_Itd)
        report_template <- officer::body_remove(report_template)
      }
      else
        report_template <- officer::body_replace_all_text(report_template, report_config$Flt3_Itd, reportInfo$flt3_itd)
    }

    if ((reportInfo$report_template == "AHD_DDX41") || (reportInfo$report_template == "SGVC"))
    {
      if (identical(reportInfo$germline_variant_analysis, report_config$section_Delete))
      {
        report_template <- officer::cursor_reach(report_template, report_config$Germline_variant_analysis)
        report_template <- officer::body_remove(report_template)
      }
      else
        report_template <- officer::body_replace_all_text(report_template, report_config$Germline_variant_analysis, reportInfo$germline_variant_analysis)
    }
  }

  #footer
  report_template <- officer::footers_replace_all_text(report_template, report_config$Patient, reportInfo$patient_name, warn=F)
  report_template <- officer::footers_replace_all_text(report_template, report_config$Urn, reportInfo$urn, warn=F)
  report_template <- officer::footers_replace_all_text(report_template, report_config$Dob, reportInfo$dob, warn=F)
  report_template <- officer::footers_replace_all_text(report_template, report_config$Lab_No, reportInfo$sample_accession, warn=F)
  report_template <- officer::footers_replace_all_text(report_template, report_config$Requester_Code, reportInfo$requester_code, warn=F)

  #Add coverage table
  if (reportInfo$report_template != "RNA_v1")
  {
    if (reportInfo$report_type != "FAIL")
      report_template <- officer::cursor_reach(report_template, report_config$Coverage_table_text)
    else
      report_template <- officer::cursor_reach(report_template, report_config$Panel_table_text)

    #Align the table left in SG reports and center in others
    table_align <- "center"
    if (grepl("^(SG_|SGVC)", reportInfo$report_template))
      table_align <- "left"
    report_template <- flextable::body_add_flextable(report_template, coverage_table, align=table_align)
  }

  #Add variants table
  if (reportInfo$report_type == "VAR")
  {
    if ((reportInfo$report_template != "RNA_v1"))
    {
      if (!((reportInfo$report_template == "SG_HAVCR2") || (reportInfo$report_template == "SGVC")))
        variants_table <- variantsTableThemed(reportInfo$variants, reportInfo$clinical_context, report_config)
      else
        variants_table <- variantsTableThemedSG(reportInfo$variants, reportInfo$clinical_context, report_config, reportInfo$report_template)

      report_template <- officer::cursor_reach(report_template, report_config$Variants_table_text)
      report_template <- flextable::body_add_flextable(report_template, variants_table, align="center", pos="before")
    }
    else #RNA fusion template
    {
      variants_table <- variantsTableThemedRNA(reportInfo$fusions, reportInfo$clinical_context, report_config, reportInfo$report_template)
      report_template <- officer::cursor_reach(report_template, report_config$Variants_table_text_rna)
      report_template <- flextable::body_add_flextable(report_template, variants_table, align="center", pos="after")
    }
  }

  #Add clinical context to negative and variant existing reports
  if ((reportInfo$report_type != "FAIL") && (reportInfo$report_template != "SG_HAVCR2") && (reportInfo$report_template != "SGVC"))
  {
    if (reportInfo$clinical_context_report != "")
    {
      file_name <- paste0(gsub(" ", "_", reportInfo$clinical_context_report), ".docx")
      report_template <- officer::cursor_reach(report_template, report_config$Clinical_Context)
      report_template <- officer::body_replace_all_text(report_template, report_config$Clinical_Context, "")
      #report_template <- officer::body_add_break(report_template)
      report_template <- officer::body_add_docx(report_template, src=system.file("clinical_context", file_name, package = "reportWriter", mustWork=T), pos="on")
    }
    else
    {
      #Clinical context not chosen
      report_template <- officer::cursor_reach(report_template, report_config$Clinical_Context)
      report_template <- officer::body_remove(report_template)
    }
  }

  return (report_template)
}

