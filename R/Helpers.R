####################################################################################
# Table themes for displaying and writing
####################################################################################
# ---------------------------------------------------------------------------------
# Gene coverage table style
# ---------------------------------------------------------------------------------
coverageTableThemed <- function(dataframe) {
  set_flextable_defaults(font.size=6, font.family = "Arial", na_str=" ", nan_str=" ")

  table <- flextable(dataframe)

  #Column names
  if (ncol(dataframe) == 4)
    table <- set_header_labels(table,
                               values = list(
                                 "Gene" = "Gene",
                                 "Transcript" = "Transcript",
                                 "Targeted exons" = "Targeted exons",
                                 "500x" = "Coverage at >500x (%)"
                               ))
  else
    table <- set_header_labels(table,
                               values = list(
                                 "Gene" = "Gene",
                                 "Transcript" = "Transcript",
                                 "Targeted exons" = "Targeted exons",
                                 "500x" = "Coverage at >500x (%)",
                                 "Gene2"= "Gene",
                                 "Transcript2" = "Transcript",
                                 "Targeted exons2" = "Targeted exons",
                                 "500x2"= "Coverage at >500x (%)",
                                 "Gene3"= "Gene",
                                 "Transcript3" = "Transcript",
                                 "Targeted exons3" = "Targeted exons",
                                 "500x3" = "Coverage at >500x (%)"
                               ))


  #padding and colors
  table <- colformat_double(table, decimal.mark=".", digits=1)
  table <- padding(table, padding=0, part="all")
  table <- padding(table, padding.left=3, part="all")
  table <- padding(table, padding.top=1, padding.bottom=1, part="header")
  table <- bg(table, bg="#411E75", part="header")
  table <- color(table, color="white", part="header")
  table <- bold(table, part="header")
  table <- bg(table, bg="#E8E7EC", part="body")

  #text alignment
  table <- valign(table, valign="top", part="all")

  #borders
  big_border = fp_border(color="white", width=3)
  small_border = fp_border(color="white", width=0.5)

  table <- border_remove(table)
  table <- border_outer(table, part="all", border=small_border)
  table <- border_inner_h(table, part="all", border=small_border)
  table <- border_inner_v(table, part="all", border=small_border)

  if(ncol(dataframe) == 4)
  {
    #width
    table <- width(table, j=c(1, 2, 3, 4),
                   width=c(1.18, 1.55, 1.87, 1.2), unit="cm")
    table <- set_table_properties(table, layout="fixed", width=0.33)
  }
  else
  {
    table <- vline(table, j=c(4,8), border=big_border)

    #width
    table <- width(table, j=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                   width=c(1.18, 1.55, 1.87, 1.2, 1.33, 1.9, 1.87, 1.2, 1.18, 1.9, 1.7, 1.2), unit="cm")
    table <- set_table_properties(table, layout="fixed", width=1)
  }


  return (table)
}

# ---------------------------------------------------------------------------------
# Coverage table for FAILs
# Fails wont have the coverage numbers but other info will be there
# ---------------------------------------------------------------------------------
coverageTableThemedFail <- function(dataframe) {
  set_flextable_defaults(font.size=6, font.family = "Arial", na_str=" ", nan_str=" ")

  table <- flextable(dataframe)

  #Column names
  if (ncol(dataframe) == 3)
    table <- set_header_labels(table,
                               values = list(
                                 "Gene" = "Gene",
                                 "Transcript" = "Transcript",
                                 "Targeted exons" = "Targeted exons"
                               ))
  else
    table <- set_header_labels(table,
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
  table <- colformat_double(table, decimal.mark=".", digits=1)
  table <- padding(table, padding=0, part="all")
  table <- padding(table, padding.left=3, part="all")
  table <- padding(table, padding.top=1, padding.bottom=1, part="header")
  table <- bg(table, bg="#411E75", part="header")
  table <- color(table, color="white", part="header")
  table <- bold(table, part="header")
  table <- bg(table, bg="#E8E7EC", part="body")

  #text alignment
  table <- valign(table, valign="top", part="all")

  #borders
  big_border = fp_border(color="white", width=3)
  small_border = fp_border(color="white", width=0.5)

  table <- border_remove(table)
  table <- border_outer(table, part="all", border=small_border)
  table <- border_inner_h(table, part="all", border=small_border)
  table <- border_inner_v(table, part="all", border=small_border)

  if(ncol(dataframe) == 3)
  {
    #width
    table <- width(table, j=c(1, 2, 3),
                   width=c(1.18, 1.55, 1.87), unit="cm")
    table <- set_table_properties(table, layout="fixed", width=0.33)
  }
  else
  {
    table <- vline(table, j=c(3,6), border=big_border)

    #width
    table <- width(table, j=c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                   width=c(1.18, 1.55, 1.87, 1.33, 1.9, 1.87, 1.18, 1.9, 1.7), unit="cm")
    table <- set_table_properties(table, layout="fixed", width=1)
  }


  return (table)
}

# ---------------------------------------------------------------------------------
# Reported Variants table style
# ---------------------------------------------------------------------------------
variantsTableThemed <- function(dataframe, clinical_significance_header) {

  dataframe <- dataframe[, c("AssumedOrigin", "Gene", "Variant", "VRF", "ClinicalSignificance")]
  dataframe$VRF[is.na(dataframe$VRF)] <- report_writer_config$vrf_na
  dataframe$Variant <- gsub(" p[.]", "\np.", dataframe$Variant) #make it two lines
  set_flextable_defaults(font.size=9, font.family = "Arial", na_str=" ", nan_str=" ")

  table <- flextable(dataframe)

  #Column names
  table <- set_header_labels(table,
                             values = list(
                               "AssumedOrigin" = "ASSUMED ORIGIN",
                               "Gene" = "GENE",
                               "Variant" = "VARIANT",
                               "VRF" = "VRF\n(%)",
                               "ClinicalSignificance" = paste0("CLINICAL SIGNIFICANCE IN ", clinical_significance_header)
                             ))

  #padding and colors
  table <- colformat_double(table, decimal.mark=".", digits=0)
  table <- padding(table, padding=0, part="all")
  #table <- padding(table, padding.left=3, part="all")
  table <- padding(table, padding.left=3, part="all")
  table <- bg(table, bg="#411E75", part="header")
  table <- color(table, color="white", part="header")
  table <- bold(table, part="all")

  #text alignment
  table <- valign(table, valign="center", part="all")
  table <- align(table, align="left", part="all")

  #width
  table <- width(table, j=c(1, 2, 3, 4, 5),
                 width=c(3.48, 2, 4.75, 1, 6.75), unit="cm")
  table <- set_table_properties(table, layout="fixed", width=1)
  table <- hrule(table, rule="atleast", part="body")
  table <- height(table, height=0.74, unit="cm", part="body")

  #borders
  small_border = fp_border(color="white", width=1.8)
  big_border = fp_border(color="white", width=3)

  table <- border_remove(table)
  table <- border_outer(table, part="all", border=small_border)
  table <- border_inner_h(table, part="all", border=small_border)
  table <- border_inner_v(table, part="all", border=small_border)

  #Colour rows based on Assumed origin (if gemline different color)
  index_germline <- which(dataframe$AssumedOrigin == report_writer_config$assumed_origin_choices[6])
  if (length(index_germline) == 0)
    table <- bg(table, bg="#CFCCD6", part="body")
  else
  {
    index_other <- setdiff(c(1:nrow(dataframe)), index_germline)
    table <- bg(table, bg="#CFCCD6", part="body", i=index_other)
    table <- bg(table, bg="#E8E7EC", part="body", i=index_germline)

    if (nrow(dataframe) != 1)
      table <- hline(table, i=(min(index_germline)-1), border=big_border)
  }

  table <- font(table, fontname="Arial", part="header")

  return (table)
}

####################################################################################
# Fetch and format data
####################################################################################
# ---------------------------------------------------------------------------------
# Fetch sample info from pathos database
# ---------------------------------------------------------------------------------
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
                    pat_sample.requester, pat_sample.pathlab, pat_sample.ext_sample, seq_sample.id as seq_sample_id, seq_sample.sample_name FROM seq_sample
                    INNER JOIN seqrun ON seq_sample.seqrun_id = seqrun.id
                    INNER JOIN pat_sample ON seq_sample.pat_sample_id = pat_sample.id
                    INNER JOIN patient ON pat_sample.patient_id = patient.id
                    WHERE seq_sample.sample_name IN (", samples_str, ") AND seqrun.seqrun = '", seqrun , "';")
  data <- dbGetQuery(con_pathOS, query)

  data <- data[!duplicated(data), ]

  return(data)
}

# ---------------------------------------------------------------------------------
# Load report information to reportInfo object from the data returned from DB query
# ---------------------------------------------------------------------------------
loadReportInformation <- function(con_rb, report_data, reportInfo)
{
  reportInfo$report_template <- report_data$Template
  reportInfo$report_type <- report_data$Type
  reportInfo$results_summary_desc <- report_data$ResultsSummaryDesc
  reportInfo$results_summary_var <- report_data$ResultsSummaryVarDesc
  reportInfo$results_summary_flt3 <- report_data$ResultsSummaryFLT3
  reportInfo$results_summary_qual <- report_data$ResultsSummaryQual
  reportInfo$clinical_interpretation_sel <- report_data$ClinicalInterpretationOther
  reportInfo$clinical_interpretation_txt <- report_data$ClinicalInterpretationDesc
  reportInfo$clinical_interpretation_txt_var <- report_data$ClinicalInterpretationVarDesc
  reportInfo$clinical_interpretation_var <- report_data$ClinicalInterpretationVar
  reportInfo$clinical_interpretation_specimen <- report_data$ClinicalInterpretationSpecimen
  reportInfo$clinical_interpretation_disease <- report_data$ClinicalInterpretationDisease
  reportInfo$clinical_interpretation_ddx41 <- report_data$ClinicalInterpretationDDX41
  reportInfo$clinical_interpretation_misc_choices <- report_data$ClinicalInterpretationMiscChoices
  reportInfo$flt3_itd <- report_data$FLT3ITDAnalysis
  reportInfo$ddx41_variant_analysis <- report_data$DDX41GermlineVarAnalysis
  reportInfo$ddx41_pathogenicity <- report_data$DDX41Pathogenicity
  reportInfo$ddx41_type <- report_data$DDX41Type
  reportInfo$authorised_by <- report_data$AuthorisedBy
  reportInfo$reported_by <- report_data$ReportedBy
  reportInfo$clinical_context <- report_data$ClinicalContext
  reportInfo$clinical_context_report <- report_data$ClinicalContextReport
  reportInfo$report_name <- report_data$Name
  reportInfo$report_status <- report_data$Status

  #Fetch variants in case of a VAR report
  if (report_data$Type == "VAR")
  {
    query <- paste0("SELECT * FROM ReportVariant
                WHERE ReportID = '", report_data$ReportID, "';")
    variants <- dbGetQuery(con_rb, query)

    variants <- variants[order(variants$ReportVariantID, decreasing=F), ]
    variants$VRF[variants$VRF == "NA"] <- NA
    variants["VRF"] <- round(as.numeric(variants$VRF))
    variants["AssumedOrigin"] <- factor(variants$AssumedOrigin, levels=report_writer_config$assumed_origin_choices)

    reportInfo$variants <- variants
  }

  return (reportInfo)
}

# ---------------------------------------------------------------------------------
# Generate sample gene coverage table (depending on the report type)
# ---------------------------------------------------------------------------------
getCoverageData <- function(seqrun, sample, path_gene_coverage_file)
{
  seqrun <- trimws(seqrun, which="both")
  sample <- trimws(sample, which="both")
  file_name <- paste0(path_gene_coverage_file, seqrun,"/", sample, "/QC/", sample, "_gene_coverage.tsv")

  if (file.exists(file_name))
  {
    sample_coverage_data <- read.table(file_name, stringsAsFactors=F, header=T, sep="\t")
    colnames(sample_coverage_data) <- sub("^X", "", colnames(sample_coverage_data))

    sample_coverage_data[, "500x"] <- sample_coverage_data[, "500x"] + 0.00000001 #Add a very small number to round up 0.05, 0.15 etc in MS excel way
    sample_coverage_data[, "500x"] <- round(sample_coverage_data[, "500x"], 1)
    sample_coverage_data[, "500x"] <- as.character(sample_coverage_data[, "500x"]) #Turn coverage also to character
    sample_coverage_data <- sample_coverage_data[order(sample_coverage_data$Gene), ]

    return (sample_coverage_data)
  }

  return (data.frame())
}

# ---------------------------------------------------------------------------------
# Generate sample gene coverage table (depending on the report type)
# ---------------------------------------------------------------------------------
returnCoverageTable <- function(sample_coverage_data, report_type)
{
  if (report_type %in% c("AHD", "AHD_DDX41"))
  {

    sample_coverage_sub_all <- subset(sample_coverage_data, sample_coverage_data$Gene %in% coverage_data$Gene)
    sample_coverage_sub_all <- base::merge(sample_coverage_sub_all, coverage_data)
    sample_coverage_sub_all <- sample_coverage_sub_all[, c("Gene", "Transcript", "Targeted exons", "500x")]
    sample_coverage_sub_all <- sample_coverage_sub_all[order(sample_coverage_sub_all$Gene), ]
    sample_coverage_sub_all[which(sample_coverage_sub_all$Gene == "FLT3"), "Gene"] <- "FLT3\u002A"
    colnames(sample_coverage_sub_all)[4] <- "Coverage at >500x (%)"

    #Prepare data for display
    coverage_data_sub <- cbind(sample_coverage_sub_all[1:19, ], sample_coverage_sub_all[20:38, ], sample_coverage_sub_all[39:57, ])
    colnames(coverage_data_sub) <- c("Gene", "Transcript", "Targeted exons", "500x", "Gene2", "Transcript2", "Targeted exons2", "500x2",
                                     "Gene3", "Transcript3", "Targeted exons3", "500x3")

    return (coverage_data_sub)
  }
  else if (report_type %in% c("AH", "AH_cfDNA"))
  {
    sample_coverage_sub_no_ddx41 <- subset(sample_coverage_data, sample_coverage_data$Gene %in% all_haem_no_ddx41)
    sample_coverage_sub_no_ddx41 <- base::merge(sample_coverage_sub_no_ddx41, coverage_data)
    sample_coverage_sub_no_ddx41 <- sample_coverage_sub_no_ddx41[, c("Gene", "Transcript", "Targeted exons", "500x")]
    sample_coverage_sub_no_ddx41 <- sample_coverage_sub_no_ddx41[order(sample_coverage_sub_no_ddx41$Gene), ]
    sample_coverage_sub_no_ddx41[which(sample_coverage_sub_no_ddx41$Gene == "FLT3"), "Gene"] <- "FLT3\u002A"
    colnames(sample_coverage_sub_no_ddx41)[4] <- "Coverage at >500x (%)"

    #Prepare data for display
    sample_coverage_sub_no_ddx41 <- rbind(sample_coverage_sub_no_ddx41, c(NA, NA, NA, NA))
    coverage_data_sub <- cbind(sample_coverage_sub_no_ddx41[1:19, ], sample_coverage_sub_no_ddx41[20:38, ], sample_coverage_sub_no_ddx41[39:57, ])
    colnames(coverage_data_sub) <- c("Gene", "Transcript", "Targeted exons", "500x", "Gene2", "Transcript2", "Targeted exons2", "500x2",
                                     "Gene3", "Transcript3", "Targeted exons3", "500x3")

    return (coverage_data_sub)
  }
  else if (report_type %in% c("MDX", "MDX_MPN"))
  {
    sample_coverage_sub_mpn_dx <- subset(sample_coverage_data, sample_coverage_data$Gene %in% mpn_dx)
    sample_coverage_sub_mpn_dx <- base::merge(sample_coverage_sub_mpn_dx, coverage_data)
    sample_coverage_sub_mpn_dx <- sample_coverage_sub_mpn_dx[, c("Gene", "Transcript", "Targeted exons", "500x")]
    sample_coverage_sub_mpn_dx <- sample_coverage_sub_mpn_dx[order(sample_coverage_sub_mpn_dx$Gene), ]
    colnames(sample_coverage_sub_mpn_dx)[4] <- "Coverage at >500x (%)"

    #Prepare data for display
    sample_coverage_sub_mpn_dx <- rbind(sample_coverage_sub_mpn_dx, c(NA, NA, NA, NA))
    sample_coverage_sub_mpn_dx <- rbind(sample_coverage_sub_mpn_dx, c(NA, NA, NA, NA))
    coverage_data_sub <- cbind(sample_coverage_sub_mpn_dx[1:8, ], sample_coverage_sub_mpn_dx[9:16, ], sample_coverage_sub_mpn_dx[17:24, ])
    colnames(coverage_data_sub) <- c("Gene", "Transcript", "Targeted exons", "500x", "Gene2", "Transcript2", "Targeted exons2", "500x2",
                                     "Gene3", "Transcript3", "Targeted exons3", "500x3")

    return (coverage_data_sub)
  }
  else if (report_type %in% c("SG_TP53", "SG_TP53_CLL"))
  {
    sample_coverage_sub_sg <- subset(sample_coverage_data, sample_coverage_data$Gene %in% sg_tp53)
    sample_coverage_sub_sg <- base::merge(sample_coverage_sub_sg, coverage_data)
    sample_coverage_sub_sg <- sample_coverage_sub_sg[, c("Gene", "Transcript", "Targeted exons", "500x")]
    colnames(sample_coverage_sub_sg)[4] <- "Coverage at >500x (%)"

    return (sample_coverage_sub_sg)
  }
  else if (report_type == "SG_CEBPA_germline")
  {
    sample_coverage_sub_sg <- subset(sample_coverage_data, sample_coverage_data$Gene %in% sg_cebpa)
    sample_coverage_sub_sg <- base::merge(sample_coverage_sub_sg, coverage_data)
    sample_coverage_sub_sg <- sample_coverage_sub_sg[, c("Gene", "Transcript", "Targeted exons", "500x")]
    colnames(sample_coverage_sub_sg)[4] <- "Coverage at >500x (%)"

    return (sample_coverage_sub_sg)
  }

  return(NULL)
}

# ---------------------------------------------------------------------------------
# Generate panel coverage info for failed reports
# ---------------------------------------------------------------------------------
returnCoverageTableFail <- function(report_type)
{
  if (report_type %in% c("AHD", "AHD_DDX41"))
  {
    sample_coverage_sub_all <- coverage_data
    sample_coverage_sub_all <- sample_coverage_sub_all[order(sample_coverage_sub_all$Gene), ]
    sample_coverage_sub_all[which(sample_coverage_sub_all$Gene == "FLT3"), "Gene"] <- "FLT3\u002A"

    #Prepare data for display
    coverage_data_sub <- cbind(sample_coverage_sub_all[1:19, ], sample_coverage_sub_all[20:38, ], sample_coverage_sub_all[39:57, ])
    colnames(coverage_data_sub) <- c("Gene", "Transcript", "Targeted exons", "Gene2", "Transcript2", "Targeted exons2",
                                     "Gene3", "Transcript3", "Targeted exons3")

    return (coverage_data_sub)
  }
  else if (report_type %in% c("AH", "AH_cfDNA"))
  {
    sample_coverage_sub_no_ddx41 <- subset(coverage_data, coverage_data$Gene %in% all_haem_no_ddx41)
    sample_coverage_sub_no_ddx41 <- sample_coverage_sub_no_ddx41[order(sample_coverage_sub_no_ddx41$Gene), ]
    sample_coverage_sub_no_ddx41[which(sample_coverage_sub_no_ddx41$Gene == "FLT3"), "Gene"] <- "FLT3\u002A"

    #Prepare data for display
    sample_coverage_sub_no_ddx41 <- rbind(sample_coverage_sub_no_ddx41, c(NA, NA, NA))
    coverage_data_sub <- cbind(sample_coverage_sub_no_ddx41[1:19, ], sample_coverage_sub_no_ddx41[20:38, ], sample_coverage_sub_no_ddx41[39:57, ])
    colnames(coverage_data_sub) <- c("Gene", "Transcript", "Targeted exons", "Gene2", "Transcript2", "Targeted exons2",
                                     "Gene3", "Transcript3", "Targeted exons3")

    return (coverage_data_sub)
  }
  else if (report_type %in% c("MDX", "MDX_MPN"))
  {
    sample_coverage_sub_mpn_dx <- subset(coverage_data, coverage_data$Gene %in% mpn_dx)
    sample_coverage_sub_mpn_dx <- sample_coverage_sub_mpn_dx[order(sample_coverage_sub_mpn_dx$Gene), ]

    #Prepare data for display
    sample_coverage_sub_mpn_dx <- rbind(sample_coverage_sub_mpn_dx, c(NA, NA, NA))
    sample_coverage_sub_mpn_dx <- rbind(sample_coverage_sub_mpn_dx, c(NA, NA, NA))
    coverage_data_sub <- cbind(sample_coverage_sub_mpn_dx[1:8, ], sample_coverage_sub_mpn_dx[9:16, ], sample_coverage_sub_mpn_dx[17:24, ])
    colnames(coverage_data_sub) <- c("Gene", "Transcript", "Targeted exons", "Gene2", "Transcript2", "Targeted exons2",
                                     "Gene3", "Transcript3", "Targeted exons3")

    return (coverage_data_sub)
  }
  else if (report_type %in% c("SG_TP53", "SG_TP53_CLL"))
  {
    sample_coverage_sub_sg <- subset(coverage_data, coverage_data$Gene %in% sg_tp53)

    return (sample_coverage_sub_sg)
  }
  else if (report_type == "SG_CEBPA_germline")
  {
    sample_coverage_sub_sg <- subset(coverage_data, coverage_data$Gene %in% sg_cebpa)

    return (sample_coverage_sub_sg)
  }

  return(NULL)
}

####################################################################################
# Report generation functions
####################################################################################
# ---------------------------------------------------------------------------------
# Add clinical interpretation and results summary of negative reports to template
# ---------------------------------------------------------------------------------
negativeReportResultsSection <- function(report, reportInfo) {
  if (!(reportInfo$report_template %in% c("SG_CEBPA_germline", "AH_cfDNA")))
  {
    if (reportInfo$clinical_interpretation_sel != " ")
    {
      report <- body_replace_all_text(report, report_writer_config$Clinical_Interpretation1, reportInfo$clinical_interpretation_sel)
      report <- body_replace_all_text(report, report_writer_config$Clinical_Interpretation2, reportInfo$clinical_interpretation_txt)
    }
    else
    {
      report <- body_replace_all_text(report, report_writer_config$Clinical_Interpretation1, reportInfo$clinical_interpretation_txt)
      if (reportInfo$report_template != "AH_cfDNA")
        report <- body_replace_all_text(report, report_writer_config$Clinical_Interpretation2, "")
    }

    report <- body_replace_all_text(report, report_writer_config$Results_Summary,
                                    trimws(paste0(reportInfo$results_summary_flt3, reportInfo$results_summary_qual), which="both"))
  }
  else
  {
    report <- body_replace_all_text(report, report_writer_config$Clinical_Interpretation1, reportInfo$clinical_interpretation_txt)

    if (reportInfo$report_template == "AH_cfDNA")
      report <- body_replace_all_text(report, report_writer_config$Results_Summary,
                                      trimws(paste0(reportInfo$results_summary_flt3, reportInfo$results_summary_qual), which="both"))
  }

  return (report)
}

# ---------------------------------------------------------------------------------
# Add clinical interpretation and results summary of variants reports to template
# ---------------------------------------------------------------------------------
variantsReportResultsSection <- function(report, reportInfo) {
  #clinical interpretation
  if (reportInfo$report_template == "AHD_DDX41") #This report has 4 clinical interpretation lines
  {
    if (grepl("\n\n", reportInfo$clinical_interpretation_txt_var)) #multiple lines to break
    {
      index <- regexpr("\n\n", reportInfo$clinical_interpretation_txt_var)
      text1 <- trimws(substring(reportInfo$clinical_interpretation_txt_var, 0, index), which="both")
      text2 <- trimws(substring(reportInfo$clinical_interpretation_txt_var, index+1, nchar(reportInfo$clinical_interpretation_txt_var)), which="both")

      report <- body_replace_all_text(report, report_writer_config$Clinical_Interpretation1, text1)
      report <- body_replace_all_text(report, report_writer_config$Clinical_Interpretation2, text2)

      report <- replacepnSelClinicalInterpret(report, report_writer_config$Clinical_Interpretation3, report_writer_config$Clinical_Interpretation4, reportInfo$clinical_interpretation_sel, reportInfo$clinical_interpretation_txt)
    }
    else
    {
      report <- body_replace_all_text(report, report_writer_config$Clinical_Interpretation1, reportInfo$clinical_interpretation_txt_var)
      report <- replacepnSelClinicalInterpret(report, report_writer_config$Clinical_Interpretation2, report_writer_config$Clinical_Interpretation3, reportInfo$clinical_interpretation_sel, reportInfo$clinical_interpretation_txt)
      report <- body_replace_all_text(report, report_writer_config$Clinical_Interpretation4, "")
    }
  }
  else if (reportInfo$report_template == "SG_CEBPA_germline")
  {
    report <- body_replace_all_text(report, report_writer_config$Clinical_Interpretation1, reportInfo$clinical_interpretation_txt_var)
    report <- body_replace_all_text(report, report_writer_config$Clinical_Interpretation2, reportInfo$clinical_interpretation_txt)
  }
  else
  {
    report <- body_replace_all_text(report, report_writer_config$Clinical_Interpretation1, reportInfo$clinical_interpretation_txt_var)
    report <- replacepnSelClinicalInterpret(report, report_writer_config$Clinical_Interpretation2, report_writer_config$Clinical_Interpretation3, reportInfo$clinical_interpretation_sel, reportInfo$clinical_interpretation_txt)
  }

  #results summary
  report <- body_replace_all_text(report, report_writer_config$Results_Summary1, reportInfo$results_summary_var)
  if (reportInfo$report_template != "SG_CEBPA_germline")
  {
    report <- body_replace_all_text(report, report_writer_config$Results_Summary2, reportInfo$results_summary_qual)
  }

  return(report)
}

# ---------------------------------------------------------------------------------
# function to replace clinical interpretation select
# ---------------------------------------------------------------------------------
replacepnSelClinicalInterpret <- function(report, label1, label2, sel_clinical_interpretation, txt_clinical_interpretation) {
  if (sel_clinical_interpretation != " ")
  {
    report <- body_replace_all_text(report, label1, sel_clinical_interpretation)
    report <- body_replace_all_text(report, label2, txt_clinical_interpretation)
  }
  else
  {
    report <- body_replace_all_text(report, label1, txt_clinical_interpretation)
    report <- body_replace_all_text(report, label2, "")
  }

  return (report)
}

####################################################################################
# Other utility functions
####################################################################################
# ---------------------------------------------------------------------------------
# Convert all dates to %d-%b%-%Y format
# ---------------------------------------------------------------------------------
formatDate <- function(date_str)
{
  date <- as.Date(date_str)

  return (format(date, "%d-%b-%Y"))
}
