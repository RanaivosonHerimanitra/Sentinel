#!/bin/bash
Rscript "/media/herimanitra/Document/IPM_sentinelle/sentinel_hrmntr 291115/Sentinel/initialize_data.R"
Rscript "/media/herimanitra/Document/IPM_sentinelle/sentinel_hrmntr 291115/Sentinel/execute_report.R"
cd "/media/herimanitra/Document/IPM_sentinelle/sentinel_hrmntr 291115/Sentinel/report"
zip report.zip Diar_sentinelles.pdf ILI_sentinelles.pdf missing_sent.docx palu_autoch_sentinelles.pdf Palu_sentinelles.pdf PFA_sentinelles.pdf report.pdf
Rscript "send_email_report.R"

