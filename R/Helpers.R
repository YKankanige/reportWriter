# ******************************************************************************************************
# Table themes for displaying and writing ----
# ****************************************************************************************************
# **********************************************************************************
## coverageTableThemed ----
# Gene coverage table style
# **********************************************************************************
coverageTableThemed <- function(dataframe, coverage_level) {
  flextable::set_flextable_defaults(font.size=6, font.family="Arial", na_str=" ", nan_str=" ")

  table <- flextable::flextable(dataframe)

  #Column names
  if (ncol(dataframe) == 4)
    table <- flextable::set_header_labels(table,
                               values = list(
                                 "Gene" = "Gene",
                                 "Transcript" = "Transcript",
                                 "Targeted exons" = "Targeted exons",
                                 "Coverage" = paste0("Coverage at >", coverage_level, " (%)")
                               ))
  else
    table <- flextable::set_header_labels(table,
                               values = list(
                                 "Gene" = "Gene",
                                 "Transcript" = "Transcript",
                                 "Targeted exons" = "Targeted exons",
                                 "Coverage1" = paste0("Coverage at >", coverage_level, " (%)"),
                                 "Gene2"= "Gene",
                                 "Transcript2" = "Transcript",
                                 "Targeted exons2" = "Targeted exons",
                                 "Coverage2" = paste0("Coverage at >", coverage_level, " (%)"),
                                 "Gene3"= "Gene",
                                 "Transcript3" = "Transcript",
                                 "Targeted exons3" = "Targeted exons",
                                 "Coverage3" = paste0("Coverage at >", coverage_level, " (%)")
                               ))


  #padding and colors
  table <- flextable::colformat_double(table, decimal.mark=".", digits=1)
  table <- flextable::padding(table, padding=0, part="all")
  table <- flextable::padding(table, padding.left=3, part="all")
  table <- flextable::padding(table, padding.top=1, padding.bottom=1, part="header")
  table <- flextable::bg(table, bg="#411E75", part="header")
  table <- flextable::color(table, color="white", part="header")
  table <- flextable::bold(table, part="header")
  table <- flextable::bg(table, bg="#E8E7EC", part="body")

  #text alignment
  table <- flextable::valign(table, valign="top", part="all")

  #borders
  big_border = officer::fp_border(color="white", width=3)
  small_border = officer::fp_border(color="white", width=0.5)

  table <- flextable::border_remove(table)
  table <- flextable::border_outer(table, part="all", border=small_border)
  table <- flextable::border_inner_h(table, part="all", border=small_border)
  table <- flextable::border_inner_v(table, part="all", border=small_border)

  if(ncol(dataframe) == 4)
  {
    #width
    table <- flextable::width(table, j=c(1, 2, 3, 4),
                   width=c(1.18, 1.88, 1.74, 1.2), unit="cm")

    table <- flextable::set_table_properties(table, layout="fixed", width=0.34)
  }
  else
  {
    table <- flextable::vline(table, j=c(4,8), border=big_border)

    #width
    table <- flextable::width(table, j=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                   width=c(1.18, 1.55, 1.87, 1.2, 1.33, 1.9, 1.87, 1.2, 1.33, 1.9, 1.7, 1.1), unit="cm")
    table <- flextable::set_table_properties(table, layout="fixed", width=1)
  }
  table <- flextable::set_table_properties(table, opts_word=list(split=F, keep_with_next=T))

  return (table)
}

# **********************************************************************************
## coverageTableThemedFail ----
# Coverage table for FAILs
# Fails wont have the coverage numbers but other info will be there
# **********************************************************************************
coverageTableThemedFail <- function(dataframe) {
  flextable::set_flextable_defaults(font.size=6, font.family = "Arial", na_str=" ", nan_str=" ")

  table <- flextable::flextable(dataframe)

  #Column names
  if (ncol(dataframe) == 3)
    table <- flextable::set_header_labels(table,
                               values = list(
                                 "Gene" = "Gene",
                                 "Transcript" = "Transcript",
                                 "Targeted exons" = "Targeted exons"
                               ))
  else
    table <- flextable::set_header_labels(table,
                               values = list(
                                 "Gene" = "Gene",
                                 "Transcript" = "Transcript",
                                 "Targeted exons" = "Targeted exons",
                                 "Gene2"= "Gene",
                                 "Transcript2" = "Transcript",
                                 "Targeted exons2" = "Targeted exons",
                                 "Gene3"= "Gene",
                                 "Transcript3" = "Transcript",
                                 "Targeted exons3" = "Targeted exons"
                               ))


  #padding and colors
  table <- flextable::colformat_double(table, decimal.mark=".", digits=1)
  table <- flextable::padding(table, padding=0, part="all")
  table <- flextable::padding(table, padding.left=3, part="all")
  table <- flextable::padding(table, padding.top=1, padding.bottom=1, part="header")
  table <- flextable::bg(table, bg="#411E75", part="header")
  table <- flextable::color(table, color="white", part="header")
  table <- flextable::bold(table, part="header")
  table <- flextable::bg(table, bg="#E8E7EC", part="body")

  #text alignment
  table <- flextable::valign(table, valign="top", part="all")

  #borders
  big_border = officer::fp_border(color="white", width=3)
  small_border = officer::fp_border(color="white", width=0.5)

  table <- flextable::border_remove(table)
  table <- flextable::border_outer(table, part="all", border=small_border)
  table <- flextable::border_inner_h(table, part="all", border=small_border)
  table <- flextable::border_inner_v(table, part="all", border=small_border)

  if(ncol(dataframe) == 3)
  {
    #width
    table <- flextable::width(table, j=c(1, 2, 3),
                   width=c(1.18, 1.88, 1.74), unit="cm")
    table <- flextable::set_table_properties(table, layout="fixed", width=0.34)
  }
  else
  {
    table <- flextable::vline(table, j=c(3,6), border=big_border)

    #width
    table <- flextable::width(table, j=c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                   width=c(1.18, 1.55, 1.87, 1.33, 1.9, 1.9, 1.33, 1.9, 1.7), unit="cm")
    table <- flextable::set_table_properties(table, layout="fixed", width=1)
  }
  table <- flextable::set_table_properties(table, opts_word=list(split=F, keep_with_next=T))

  return (table)
}

# **********************************************************************************
## variantsTableThemed ----
# Reported Variants table style
# **********************************************************************************
variantsTableThemed <- function(dataframe, clinical_significance_header, report_writer_config) {

  #Allow creation of the table without entries
  if (is.null(dataframe))
    dataframe <- data.frame(AssumedOrigin=character(0), Gene=character(0), Variant=character(0), VRF=numeric(0), ClinicalSignificance=character(0))

  dataframe <- dataframe[, c("AssumedOrigin", "Gene", "Variant", "VRF", "ClinicalSignificance")]
  dataframe$VRF[is.na(dataframe$VRF)] <- report_writer_config$vrf_na
  dataframe$Variant <- gsub(" p[.]", "\np.", dataframe$Variant) #make it two lines
  flextable::set_flextable_defaults(font.size=9, font.family = "Arial", na_str=" ", nan_str=" ")

  table <- flextable::flextable(dataframe)

  #Column names
  table <- flextable::set_header_labels(table,
                             values = list(
                               "AssumedOrigin" = "ASSUMED ORIGIN",
                               "Gene" = "GENE",
                               "Variant" = "VARIANT",
                               "VRF" = "VRF\n(%)",
                               "ClinicalSignificance" = paste0("CLINICAL SIGNIFICANCE IN ", clinical_significance_header)
                             ))

  #padding and colors
  table <- flextable::colformat_double(table, decimal.mark=".", digits=0)
  table <- flextable::padding(table, padding=0, part="all")
  table <- flextable::padding(table, padding.left=3, part="all")
  table <- flextable::bg(table, bg="#411E75", part="header")
  table <- flextable::color(table, color="white", part="header")
  table <- flextable::bold(table, part="all")

  #text alignment
  table <- flextable::valign(table, valign="center", part="all")
  table <- flextable::align(table, align="left", part="all")

  #width
  table <- flextable::width(table, j=c(1, 2, 3, 4, 5),
                 width=c(3.48, 2, 4.75, 1, 6.75), unit="cm")
  table <- flextable::set_table_properties(table, layout="fixed", width=1)
  table <- flextable::hrule(table, rule="atleast", part="body")
  table <- flextable::height(table, height=0.74, unit="cm", part="body")

  #borders
  small_border = officer::fp_border(color="white", width=1.8)
  big_border = officer::fp_border(color="white", width=3)

  table <- flextable::border_remove(table)
  table <- flextable::border_outer(table, part="all", border=small_border)
  table <- flextable::border_inner_h(table, part="all", border=small_border)
  table <- flextable::border_inner_v(table, part="all", border=small_border)

  #Colour rows based on Assumed origin (if gemline different color)
  index_germline <- which(dataframe$AssumedOrigin == report_writer_config$assumed_origin_choices[6])
  if (length(index_germline) == 0)
    table <- flextable::bg(table, bg="#CFCCD6", part="body")
  else
  {
    index_other <- setdiff(c(1:nrow(dataframe)), index_germline)
    if (length(index_other) != 0)
      table <- flextable::bg(table, bg="#CFCCD6", part="body", i=index_other)
    table <- flextable::bg(table, bg="#E8E7EC", part="body", i=index_germline)

    if ((nrow(dataframe) != 1) && (length(index_other) != 0))
      table <- flextable::hline(table, i=(min(index_germline)-1), border=big_border)
  }

  table <- flextable::font(table, fontname="Arial", part="header")

  return (table)
}

# **********************************************************************************
## variantsTableThemedSG ----
# Reported Variants table style for SG_HAVCR2 and SGVC report templates
# **********************************************************************************
variantsTableThemedSG <- function(dataframe, clinical_significance_header, report_writer_config, report_template) {
  #Allow creation of the table without entries
  if (is.null(dataframe))
    dataframe <- data.frame(Gene=character(0), Variant=character(0), VRF=numeric(0))

  dataframe <- dataframe[, c("Gene", "Variant", "VRF")]
  dataframe$VRF[is.na(dataframe$VRF)] <- report_writer_config$vrf_na
  dataframe["NVariant"] <- trimws(str_extract(dataframe$Variant, "^c[.].*\\s"), which="both")
  dataframe["PVariant"] <- str_extract(dataframe$Variant, "p[.].*$")
  dataframe <- dataframe[, c("Gene", "NVariant", "PVariant", "VRF")]

  flextable::set_flextable_defaults(font.size=9, font.family = "Arial", na_str=" ", nan_str=" ")

  table <- flextable::flextable(dataframe)

  #Column names
  table <- flextable::set_header_labels(table,
                                        values = list(
                                          "Gene" = "GENE",
                                          "NVariant" = "NUCLEOTIDE VARIANT",
                                          "PVariant" = "PROTEIN VARIANT",
                                          "VRF" = "VRF (%)"
                                        ))

  #padding and colors
  table <- flextable::colformat_double(table, decimal.mark=".", digits=0)
  table <- flextable::padding(table, padding=0, part="all")
  table <- flextable::padding(table, padding.left=3, part="all")
  table <- flextable::bg(table, bg="#411E75", part="header")
  table <- flextable::color(table, color="white", part="header")
  table <- flextable::bold(table, part="all")

  #text alignment
  table <- flextable::valign(table, valign="center", part="all")
  table <- flextable::align(table, align="left", part="all")

  #width
  table <- flextable::width(table, j=c(1, 2, 3, 4),
                            width=c(3.73, 5.5, 5.5, 3.25), unit="cm")
  table <- flextable::set_table_properties(table, layout="fixed", width=1)
  table <- flextable::hrule(table, rule="atleast", part="body")
  table <- flextable::height(table, height=0.74, unit="cm", part="body")

  #borders
  small_border = officer::fp_border(color="white", width=1.8)
  big_border = officer::fp_border(color="white", width=3)

  table <- flextable::border_remove(table)
  table <- flextable::border_outer(table, part="all", border=small_border)
  table <- flextable::border_inner_h(table, part="all", border=small_border)
  table <- flextable::border_inner_v(table, part="all", border=small_border)
  if (report_template == "SGVC")
    table <- flextable::bg(table, bg="#E8E7EC", part="body")
  else
    table <- flextable::bg(table, bg="#CFCCD6", part="body")

  table <- flextable::font(table, fontname="Arial", part="header")

  return (table)
}

# **********************************************************************************
## variantsTableThemedRNA ----
# Reported fusions table style for RNA report templates
# **********************************************************************************
variantsTableThemedRNA<- function(dataframe, clinical_significance_header, report_writer_config, report_template) {

  #Allow creation of the table without entries
  if (is.null(dataframe))
    dataframe <- data.frame(Fusion=character(0), Breakpoint=character(0), ClinicalSignificance=character(0))

  dataframe <- dataframe[, c("Fusion", "Breakpoint", "ClinicalSignificance")]

  flextable::set_flextable_defaults(font.size=9, font.family = "Arial", na_str=" ", nan_str=" ")

  table <- flextable::flextable(dataframe)

  #Column names
  table <- flextable::set_header_labels(table,
                                        values = list(
                                          "Fusion" = "FUSION",
                                          "Breakpoint" = "BREAKPOINT",
                                          "ClinicalSignificance" = paste0("CLINICAL SIGNIFICANCE IN ", clinical_significance_header)
                                        ))

  #padding and colors
  table <- flextable::colformat_double(table, decimal.mark=".", digits=0)
  table <- flextable::padding(table, padding=0, part="all")
  table <- flextable::padding(table, padding.left=3, part="all")
  table <- flextable::bg(table, bg="#411E75", part="header")
  table <- flextable::color(table, color="white", part="header")
  table <- flextable::bold(table, part="all")

  #text alignment
  table <- flextable::valign(table, valign="center", part="all")
  table <- flextable::align(table, align="left", part="all")

  #width
  table <- flextable::width(table, j=c(1, 2, 3),
                            width=c(4.48, 5.75, 7.78), unit="cm")
  table <- flextable::set_table_properties(table, layout="fixed", width=1)
  table <- flextable::hrule(table, rule="atleast", part="body")
  table <- flextable::height(table, height=0.74, unit="cm", part="body")

  #borders
  small_border = officer::fp_border(color="white", width=1.8)
  big_border = officer::fp_border(color="white", width=3)

  table <- flextable::border_remove(table)
  table <- flextable::border_outer(table, part="all", border=small_border)
  table <- flextable::border_inner_h(table, part="all", border=small_border)
  table <- flextable::border_inner_v(table, part="all", border=small_border)
  table <- flextable::bg(table, bg="#CFCCD6", part="body")

  table <- flextable::font(table, fontname="Arial", part="header")

  return (table)
}


# **********************************************************************************
# Fetch and format data ----
# **********************************************************************************
# **********************************************************************************
## getSampleInfo ----
# Fetch sample info from pathos database
# **********************************************************************************
getSampleInfo <- function(con_pathOS, seqrun, sample_accession)
{
  seqrun <- trimws(seqrun, which="both")
  sample_accession <- trimws(sample_accession, which="both")

  #Check whether the sample, seqrun combination exists
  sample_names <- sample_accession
  sample_names <- c(sample_names, paste0(sample_names, "-1")) #because there are two samples
  sample_names <- paste0("'", sample_names, "'")
  samples_str <- paste(sample_names, collapse = ", ")
  #print(samples_str)

  query <- paste0("SELECT patient.*, seqrun.seqrun, pat_sample.collect_date, pat_sample.rcvd_date,
                    pat_sample.requester, pat_sample.pathlab, pat_sample.ext_sample, seq_sample.id as seq_sample_id,
                    seq_sample.sample_name, panel.manifest as panel FROM seq_sample
                    INNER JOIN seqrun ON seq_sample.seqrun_id = seqrun.id
                    INNER JOIN pat_sample ON seq_sample.pat_sample_id = pat_sample.id
                    INNER JOIN patient ON pat_sample.patient_id = patient.id
                    INNER JOIN panel ON panel.id = seq_sample.panel_id
                    WHERE seq_sample.sample_name IN (", samples_str, ") AND seqrun.seqrun = '", seqrun , "';")
  data <- DBI::dbGetQuery(con_pathOS, query)

  data <- data[!duplicated(data), ]

  return(data)
}

# **********************************************************************************
## loadReportInformation ----
# Load report information to reportInfo object from the data returned from DB query
# **********************************************************************************
loadReportInformation <- function(con_rb, report_data, reportInfo, report_writer_config)
{
  reportInfo$report_template <- report_data$Template
  reportInfo$report_type <- report_data$Type
  reportInfo$results_summary_desc <- report_data$ResultsSummaryDesc
  reportInfo$results_summary_desc_other <- report_data$ResultsSummaryDescOther
  reportInfo$results_summary_var <- report_data$ResultsSummaryVarDesc
  reportInfo$results_summary_flt3 <- report_data$ResultsSummaryFLT3
  reportInfo$results_summary_qual <- report_data$ResultsSummaryQual
  reportInfo$results_summary_dna_rna <- report_data$ResultsSummaryDNARNA
  reportInfo$results_summary_havcr2_result <- report_data$ResultsSummaryHAVCR2Result
  reportInfo$results_summary_havcr2_comment <- report_data$ResultsSummaryHAVCR2Comment
  reportInfo$results_summary_vc_conclusion <- report_data$ResultsSummaryVCConclusion
  reportInfo$clinical_interpretation_sel <- report_data$ClinicalInterpretationOther
  reportInfo$clinical_interpretation_txt <- report_data$ClinicalInterpretationDesc
  reportInfo$clinical_interpretation_txt_var <- report_data$ClinicalInterpretationVarDesc
  reportInfo$clinical_interpretation_var <- report_data$ClinicalInterpretationVar
  reportInfo$clinical_interpretation_specimen <- report_data$ClinicalInterpretationSpecimen
  reportInfo$clinical_interpretation_disease <- report_data$ClinicalInterpretationDisease
  reportInfo$clinical_interpretation_fusion <- report_data$ClinicalInterpretationFusion
  reportInfo$clinical_interpretation_pathogenicity <- report_data$ClinicalInterpretationPathogenicity
  reportInfo$clinical_interpretation_misc_choices <- report_data$ClinicalInterpretationMiscChoices
  reportInfo$clinical_interpretation_main <- report_data$ClinicalInterpretationMain
  reportInfo$flt3_itd <- report_data$FLT3ITDAnalysis
  reportInfo$vc_gene <- report_data$VariantConfirmationGene
  reportInfo$comment <- report_data$Comment
  reportInfo$germline_variant_analysis <- report_data$GermlineVariantAnalysis
  reportInfo$germline_pathogenicity <- report_data$GermlinePathogenicity
  reportInfo$germline_classification <- report_data$GermlineVariantClassification
  reportInfo$germline_condition <- report_data$GermlineCondition
  reportInfo$var_type <- report_data$VarType
  reportInfo$authorised_by <- report_data$AuthorisedBy
  reportInfo$reported_by <- report_data$ReportedBy
  reportInfo$clinical_context <- report_data$ClinicalContext
  reportInfo$clinical_context_report <- report_data$ClinicalContextReport
  reportInfo$report_name <- report_data$Name
  reportInfo$report_status <- report_data$Status

  #Fetch variants and fusions in case of a VAR report
  if (report_data$Type == "VAR")
  {
    if (reportInfo$report_template != "RNA")
    {
      query <- paste0("SELECT * FROM ReportVariant
                WHERE ReportID = '", report_data$ReportID, "';")
      variants <- DBI::dbGetQuery(con_rb, query)

      variants <- variants[order(variants$ReportVariantID, decreasing=F), ]
      variants$VRF[variants$VRF == "NA"] <- NA
      variants["VRF"] <- round(as.numeric(variants$VRF))
      variants["AssumedOrigin"] <- factor(variants$AssumedOrigin, levels=report_writer_config$assumed_origin_choices)

      reportInfo$variants <- variants

    }
    else
    {
      query <- paste0("SELECT * FROM ReportFusion
                WHERE ReportID = '", report_data$ReportID, "';")
      fusions <- DBI::dbGetQuery(con_rb, query)

      fusions <- fusions[order(fusions$ReportFusionID, decreasing=F), ]
      reportInfo$fusions <- fusions
    }
  }

  return (reportInfo)
}

# **********************************************************************************
## getCoverageData ----
# Generate sample gene coverage table (depending on the report type)
# **********************************************************************************
getCoverageData <- function(seqrun, sample, path_gene_coverage_file, coverage_level)
{
  seqrun <- trimws(seqrun, which="both")
  sample <- trimws(sample, which="both")
  file_name <- paste0(path_gene_coverage_file, seqrun,"/", sample, "/QC/", sample, "_gene_coverage.tsv")

  if (file.exists(file_name))
  {
    sample_coverage_data <- read.table(file_name, stringsAsFactors=F, header=T, sep="\t", quote = "")
    colnames(sample_coverage_data) <- sub("^X", "", colnames(sample_coverage_data))

    #PLS CHECK, QUICK FIX FOR MFSD11;SRSF2
    sample_coverage_data$Gene[sample_coverage_data$Gene == "MFSD11;SRSF2"] <- "SRSF2"

    sample_coverage_data[, coverage_level] <- sample_coverage_data[, coverage_level] + 0.00000001 #Add a very small number to round up 0.05, 0.15 etc in MS excel way
    sample_coverage_data[, coverage_level] <- round(sample_coverage_data[, coverage_level], 1)
    sample_coverage_data[, coverage_level] <- as.character(sample_coverage_data[, coverage_level]) #Turn coverage also to character
    sample_coverage_data <- sample_coverage_data[order(sample_coverage_data$Gene), ]
    sample_coverage_data <- sample_coverage_data[, c("Gene", coverage_level)]

    return (sample_coverage_data)
  }

  return (data.frame())
}

# **********************************************************************************
## returnCoverageTable ----
# Generate sample gene coverage table (depending on the report type)
# **********************************************************************************
returnCoverageTable <- function(sample_coverage_data, report_type, vc_gene, report_config, coverage_data, coverage_level)
{
  if (report_type %in% c("AHD", "AHD_DDX41"))
  {

    sample_coverage_sub_all <- subset(sample_coverage_data, sample_coverage_data$Gene %in% report_config$AHD_genes)
    sample_coverage_sub_all <- base::merge(sample_coverage_sub_all, coverage_data)
    sample_coverage_sub_all <- sample_coverage_sub_all[, c("Gene", "Transcript", "Targeted exons", coverage_level)]
    sample_coverage_sub_all <- sample_coverage_sub_all[order(sample_coverage_sub_all$Gene), ]
    sample_coverage_sub_all[which(sample_coverage_sub_all$Gene == "FLT3"), "Gene"] <- "FLT3\u002A"

    #Prepare data for display
    coverage_data_sub <- cbind(sample_coverage_sub_all[1:27, ], sample_coverage_sub_all[28:54, ], sample_coverage_sub_all[55:81, ])
    colnames(coverage_data_sub) <- c("Gene", "Transcript", "Targeted exons", "Coverage1", "Gene2", "Transcript2", "Targeted exons2", "Coverage2",
                                     "Gene3", "Transcript3", "Targeted exons3", "Coverage3")

    return (coverage_data_sub)
  }
  else if (report_type %in% c("AH", "AH_cfDNA"))
  {
    sample_coverage_sub_no_ddx41 <- subset(sample_coverage_data, sample_coverage_data$Gene %in% report_config$AH_genes)
    sample_coverage_sub_no_ddx41 <- base::merge(sample_coverage_sub_no_ddx41, coverage_data)
    sample_coverage_sub_no_ddx41 <- sample_coverage_sub_no_ddx41[, c("Gene", "Transcript", "Targeted exons", coverage_level)]
    sample_coverage_sub_no_ddx41 <- sample_coverage_sub_no_ddx41[order(sample_coverage_sub_no_ddx41$Gene), ]
    sample_coverage_sub_no_ddx41[which(sample_coverage_sub_no_ddx41$Gene == "FLT3"), "Gene"] <- "FLT3\u002A"

    #Prepare data for display
    sample_coverage_sub_no_ddx41 <- rbind(sample_coverage_sub_no_ddx41, c(NA, NA, NA, NA))
    coverage_data_sub <- cbind(sample_coverage_sub_no_ddx41[1:27, ], sample_coverage_sub_no_ddx41[28:54, ], sample_coverage_sub_no_ddx41[55:81, ])
    colnames(coverage_data_sub) <- c("Gene", "Transcript", "Targeted exons", "Coverage1", "Gene2", "Transcript2", "Targeted exons2", "Coverage2",
                                     "Gene3", "Transcript3", "Targeted exons3", "Coverage3")

    return (coverage_data_sub)
  }
  else if (report_type %in% c("MDX", "MDX_MPN"))
  {
    sample_coverage_sub_mpn_dx <- subset(sample_coverage_data, sample_coverage_data$Gene %in% report_config$MPN_genes)
    sample_coverage_sub_mpn_dx <- base::merge(sample_coverage_sub_mpn_dx, coverage_data)
    sample_coverage_sub_mpn_dx <- sample_coverage_sub_mpn_dx[, c("Gene", "Transcript", "Targeted exons", coverage_level)]
    sample_coverage_sub_mpn_dx <- sample_coverage_sub_mpn_dx[order(sample_coverage_sub_mpn_dx$Gene), ]

    #Prepare data for display
    sample_coverage_sub_mpn_dx <- rbind(sample_coverage_sub_mpn_dx, c(NA, NA, NA, NA))
    sample_coverage_sub_mpn_dx <- rbind(sample_coverage_sub_mpn_dx, c(NA, NA, NA, NA))
    coverage_data_sub <- cbind(sample_coverage_sub_mpn_dx[1:8, ], sample_coverage_sub_mpn_dx[9:16, ], sample_coverage_sub_mpn_dx[17:24, ])
    colnames(coverage_data_sub) <- c("Gene", "Transcript", "Targeted exons", "Coverage1", "Gene2", "Transcript2", "Targeted exons2", "Coverage2",
                                     "Gene3", "Transcript3", "Targeted exons3", "Coverage3")

    return (coverage_data_sub)
  }
  else if (report_type == "SG_HAVCR2")
  {
    sample_coverage_sub_sg <- subset(sample_coverage_data, sample_coverage_data$Gene %in% report_config$SG_HAVCR2_genes)
    sample_coverage_sub_sg <- base::merge(sample_coverage_sub_sg, coverage_data)
    sample_coverage_sub_sg <- sample_coverage_sub_sg[, c("Gene", "Transcript", "Targeted exons", coverage_level)]

    return (sample_coverage_sub_sg)
  }
  else if (report_type == "SGVC")
  {
    sample_coverage_sub_sg <- subset(sample_coverage_data, sample_coverage_data$Gene %in% vc_gene)
    sample_coverage_sub_sg <- base::merge(sample_coverage_sub_sg, coverage_data)
    sample_coverage_sub_sg <- sample_coverage_sub_sg[, c("Gene", "Transcript", "Targeted exons", coverage_level)]

    return (sample_coverage_sub_sg)
  }

  return(NULL)
}

# **********************************************************************************
## returnCoverageTableFail ----
# Generate panel coverage info for failed reports
# **********************************************************************************
returnCoverageTableFail <- function(report_type, vc_gene, report_config, coverage_data)
{

  if (report_type %in% c("AHD", "AHD_DDX41"))
  {
    sample_coverage_sub_all <- subset(coverage_data, coverage_data$Gene %in% report_config$AHD_genes)
    sample_coverage_sub_all <- sample_coverage_sub_all[order(sample_coverage_sub_all$Gene), ]
    sample_coverage_sub_all[which(sample_coverage_sub_all$Gene == "FLT3"), "Gene"] <- "FLT3\u002A"

    #Prepare data for display
    coverage_data_sub <- cbind(sample_coverage_sub_all[1:27, ], sample_coverage_sub_all[28:54, ], sample_coverage_sub_all[55:81, ])
    colnames(coverage_data_sub) <- c("Gene", "Transcript", "Targeted exons", "Gene2", "Transcript2", "Targeted exons2",
                                     "Gene3", "Transcript3", "Targeted exons3")

    return (coverage_data_sub)
  }
  else if (report_type %in% c("AH", "AH_cfDNA"))
  {
    sample_coverage_sub_no_ddx41 <- subset(coverage_data, coverage_data$Gene %in% report_config$AH_genes)
    sample_coverage_sub_no_ddx41 <- sample_coverage_sub_no_ddx41[order(sample_coverage_sub_no_ddx41$Gene), ]
    sample_coverage_sub_no_ddx41[which(sample_coverage_sub_no_ddx41$Gene == "FLT3"), "Gene"] <- "FLT3\u002A"

    #Prepare data for display
    sample_coverage_sub_no_ddx41 <- rbind(sample_coverage_sub_no_ddx41, c(NA, NA, NA))
    coverage_data_sub <- cbind(sample_coverage_sub_no_ddx41[1:27, ], sample_coverage_sub_no_ddx41[28:54, ], sample_coverage_sub_no_ddx41[55:81, ])
    colnames(coverage_data_sub) <- c("Gene", "Transcript", "Targeted exons", "Gene2", "Transcript2", "Targeted exons2",
                                     "Gene3", "Transcript3", "Targeted exons3")

    return (coverage_data_sub)
  }
  else if (report_type %in% c("MDX", "MDX_MPN"))
  {
    sample_coverage_sub_mpn_dx <- subset(coverage_data, coverage_data$Gene %in% report_config$MPN_genes)
    sample_coverage_sub_mpn_dx <- sample_coverage_sub_mpn_dx[order(sample_coverage_sub_mpn_dx$Gene), ]

    #Prepare data for display
    sample_coverage_sub_mpn_dx <- rbind(sample_coverage_sub_mpn_dx, c(NA, NA, NA))
    sample_coverage_sub_mpn_dx <- rbind(sample_coverage_sub_mpn_dx, c(NA, NA, NA))
    coverage_data_sub <- cbind(sample_coverage_sub_mpn_dx[1:8, ], sample_coverage_sub_mpn_dx[9:16, ], sample_coverage_sub_mpn_dx[17:24, ])
    colnames(coverage_data_sub) <- c("Gene", "Transcript", "Targeted exons", "Gene2", "Transcript2", "Targeted exons2",
                                     "Gene3", "Transcript3", "Targeted exons3")

    return (coverage_data_sub)
  }
  else if (report_type == "SG_HAVCR2")
  {
    sample_coverage_sub_sg <- subset(coverage_data, coverage_data$Gene %in% report_config$SG_HAVCR2_genes)

    return (sample_coverage_sub_sg)
  }
  else if (report_type == "SGVC")
  {
    sample_coverage_sub_sg <- subset(coverage_data, coverage_data$Gene %in% vc_gene)

    return (sample_coverage_sub_sg)
  }

  return(NULL)
}

# **********************************************************************************
# Report generation functions ----
# **********************************************************************************
# **********************************************************************************
## negativeReportResultsSection ----
# Add clinical interpretation and results summary of negative reports to template
# **********************************************************************************
negativeReportResultsSection <- function(report, reportInfo, report_writer_config) {
  if ((reportInfo$report_template != "AH_cfDNA") && (reportInfo$report_template != "SG_HAVCR2") && (reportInfo$report_template != "SGVC"))
  {
    report <- officer::body_replace_all_text(report, report_writer_config$Clinical_Interpretation1, reportInfo$clinical_interpretation_sel)
    report <- officer::body_replace_all_text(report, report_writer_config$Clinical_Interpretation2, reportInfo$clinical_interpretation_txt)

    results_summary <- paste0(reportInfo$results_summary_flt3, " ", reportInfo$results_summary_dna_rna, " ", reportInfo$results_summary_qual, " ",
                              reportInfo$results_summary_desc_other)
    results_summary <- trimws(results_summary, which="both")
    results_summary <- gsub("[ ]{2,}", " ", results_summary)

    report <- officer::body_replace_all_text(report, report_writer_config$Results_Summary, results_summary)
  }
  else if (reportInfo$report_template == "AH_cfDNA")
  {
    report <- officer::body_replace_all_text(report, report_writer_config$Clinical_Interpretation1, reportInfo$clinical_interpretation_txt)

    results_summary <- paste0(reportInfo$results_summary_dna_rna, " ", reportInfo$results_summary_qual, " ", reportInfo$results_summary_desc_other)
    results_summary <- trimws(results_summary, which="both")
    results_summary <- gsub("[ ]{2,}", " ", results_summary)
    report <- officer::body_replace_all_text(report, report_writer_config$Results_Summary, results_summary)
  }
  else if (reportInfo$report_template == "SG_HAVCR2")
  {
    report <- officer::body_replace_all_text(report, report_writer_config$Results_Summary, trimws(reportInfo$results_summary_qual, which="both"))
  }
  else if (reportInfo$report_template == "SGVC")
  {
    report <- officer::body_replace_all_text(report, report_writer_config$Clinical_Interpretation1, reportInfo$clinical_interpretation_txt)
  }

  return (report)
}

# **********************************************************************************
## variantsReportResultsSection ----
# Add clinical interpretation and results summary of variants reports to template
# **********************************************************************************
variantsReportResultsSection <- function(report, reportInfo, report_writer_config) {
  #clinical interpretation
  if (reportInfo$report_template == "AHD_DDX41")#This report has 4 clinical interpretation lines
  {
    if (grepl("\n\n", reportInfo$clinical_interpretation_txt_var)) #multiple lines to break
    {
      index <- regexpr("\n\n", reportInfo$clinical_interpretation_txt_var)
      text1 <- trimws(substring(reportInfo$clinical_interpretation_txt_var, 0, index), which="both")
      text2 <- trimws(substring(reportInfo$clinical_interpretation_txt_var, index+1, nchar(reportInfo$clinical_interpretation_txt_var)), which="both")

      report <- officer::body_replace_all_text(report, report_writer_config$Clinical_Interpretation1, text1)
      report <- officer::body_replace_all_text(report, report_writer_config$Clinical_Interpretation2, text2)

      report <- replacepnSelClinicalInterpret(report, report_writer_config$Clinical_Interpretation3, report_writer_config$Clinical_Interpretation4, reportInfo$clinical_interpretation_sel, reportInfo$clinical_interpretation_txt)
    }
    else
    {
      report <- officer::body_replace_all_text(report, report_writer_config$Clinical_Interpretation1, reportInfo$clinical_interpretation_txt_var)
      report <- replacepnSelClinicalInterpret(report, report_writer_config$Clinical_Interpretation2, report_writer_config$Clinical_Interpretation3, reportInfo$clinical_interpretation_sel, reportInfo$clinical_interpretation_txt)
      report <- officer::body_replace_all_text(report, report_writer_config$Clinical_Interpretation4, "")
    }
  }
  else
  {
    report <- officer::body_replace_all_text(report, report_writer_config$Clinical_Interpretation1, reportInfo$clinical_interpretation_txt_var)
    if ((reportInfo$report_template != "SG_HAVCR2") && (reportInfo$report_template != "SGVC"))
      report <- replacepnSelClinicalInterpret(report, report_writer_config$Clinical_Interpretation2, report_writer_config$Clinical_Interpretation3, reportInfo$clinical_interpretation_sel, reportInfo$clinical_interpretation_txt)
  }

  #results summary
  if (reportInfo$report_template == "RNA")
  {
    results_summary <- paste0(reportInfo$results_summary_dna_rna, " ", reportInfo$results_summary_qual)
    results_summary <- trimws(results_summary, which="both")
    results_summary <- gsub("[ ]{2,}", " ", results_summary)
    report <- officer::body_replace_all_text(report, report_writer_config$Results_Summary, results_summary)
  }
  else if ((reportInfo$report_template != "SG_HAVCR2") && (reportInfo$report_template != "SGVC"))
  {
    report <- officer::body_replace_all_text(report, report_writer_config$Results_Summary1, reportInfo$results_summary_var)

    results_summary <- paste0(reportInfo$results_summary_dna_rna, " ", reportInfo$results_summary_qual, " ", reportInfo$results_summary_desc_other)
    results_summary <- trimws(results_summary, which="both")
    results_summary <- gsub("[ ]{2,}", " ", results_summary)
    report <- officer::body_replace_all_text(report, report_writer_config$Results_Summary2, results_summary)
  }
  else
    report <- officer::body_replace_all_text(report, report_writer_config$Results_Summary1, reportInfo$results_summary_desc)

  return(report)
}

# **********************************************************************************
## replacepnSelClinicalInterpret ----
# function to replace clinical interpretation select
# **********************************************************************************
replacepnSelClinicalInterpret <- function(report, label1, label2, sel_clinical_interpretation, txt_clinical_interpretation) {
  if (sel_clinical_interpretation != " ")
  {
    report <- officer::body_replace_all_text(report, label1, sel_clinical_interpretation)
    report <- officer::body_replace_all_text(report, label2, txt_clinical_interpretation)
  }
  else
  {
    report <- officer::body_replace_all_text(report, label1, txt_clinical_interpretation)
    report <- officer::body_replace_all_text(report, label2, "")
  }

  return (report)
}

# **********************************************************************************
# Other utility functions ----
# **********************************************************************************
# **********************************************************************************
## formatDate ----
# Convert all dates to %d-%b%-%Y format
# **********************************************************************************
formatDate <- function(date_str)
{
  date <- as.Date(date_str)

  return (format(date, "%d-%b-%Y"))
}

# **********************************************************************************
## formatDateDB ----
# Convert all dates to %Y-%m-%d format
# **********************************************************************************
formatDateDB <- function(date)
{
  return (format(date, "%Y-%m-%d"))
}

# **********************************************************************************
## escapeQuote ----
# Quotations in text breaks save queries because of "s used in paste0
# **********************************************************************************
escapeQuote <- function(val)
{
  val <- gsub('"', '""', val)

  return (val)
}
