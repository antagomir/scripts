
####Longitudinal test
OTU_ANCOM_Test <- read.delim("~/Dropbox/Work at NIEHS/OTU_Ancom_Test_Counts.txt")

Mapping_File_ANCOM_Test <- read.delim("~/Dropbox/Work at NIEHS/OTU_Ancom_Test_Mapping.txt")

#data_test=merge(OTU_ANCOM_Test,Mapping_File_ANCOM_Test,by="Sample.ID")

longitudinal_comparison_test=ANCOM.main(OTUdat=OTU_ANCOM_Test,
                                        Vardat=Mapping_File_ANCOM_Test[Mapping_File_ANCOM_Test$group_info%in%c("2FL","3SL","pHMO"),],
                                        adjusted=F,
                                        repeated=T,
                                        main.var="group_info",
                                        adj.formula=NULL,
                                        repeat.var="day_of_treatment",
                                        longitudinal=T,
                                        random.formula="~1|host_subject_id",
                                        multcorr=2,
                                        sig=0.05,
                                        prev.cut=0.90)

longitudinal_comparison_test$W.taxa


longitudinal_comparison_test=ANCOM.main(OTUdat=OTU_ANCOM_Test,
                                        Vardat=Mapping_File_ANCOM_Test[Mapping_File_ANCOM_Test$group_info%in%c("2FL","3SL","pHMO"),],
                                        adjusted=F,
                                        repeated=T,
                                        main.var="group_info",
                                        adj.formula=NULL,
                                        repeat.var="day_of_treatment",
                                        longitudinal=T,
                                        random.formula="~1|host_subject_id",
                                        multcorr=2,
                                        sig=0.05,
                                        prev.cut=0.90)

longitudinal_comparison_test$W.taxa




longitudinal_comparison_test_HC_RA=ANCOM.main(OTUdat=otu_test,
                                        Vardat=map_test[which(map_test$Group%in%c("HC","RA")),],
                                        adjusted=F,
                                        repeated=T,
                                        main.var="Group",
                                        adj.formula=NULL,
                                        repeat.var="Visitorder",
                                        longitudinal=T,
                                        random.formula="~1|individual",
                                        multcorr=2,
                                        sig=0.05,
                                        prev.cut=0.50)

longitudinal_comparison_test_HC_RA$W.taxa



longitudinal_comparison_test_HC_RR=ANCOM.main(OTUdat=otu_test,
                                              Vardat=map_test[which(map_test$Group%in%c("HC","RR")),],
                                              adjusted=F,
                                              repeated=T,
                                              main.var="Group",
                                              adj.formula=NULL,
                                              repeat.var="Visitorder",
                                              longitudinal=T,
                                              random.formula="~1|individual",
                                              multcorr=2,
                                              sig=0.05,
                                              prev.cut=0.50)

longitudinal_comparison_test_HC_RR$W.taxa

longitudinal_comparison_test_RA_RR=ANCOM.main(OTUdat=otu_test,
                                              Vardat=map_test[which(map_test$Group%in%c("RA","RR")),],
                                              adjusted=F,
                                              repeated=T,
                                              main.var="Group",
                                              adj.formula=NULL,
                                              repeat.var="Visitorder",
                                              longitudinal=T,
                                              random.formula="~1|individual",
                                              multcorr=2,
                                              sig=0.05,
                                              prev.cut=0.50)

longitudinal_comparison_test_RA_RR$W.taxa


# plots_for_ancom=function(otu_list,otu_data,metadata,var){
#   data_sub=otu_data[,which(colnames(otu_data)%in%c("Sample.ID",check))]
#   meta_sub=metadata[,which(colnames(metadata)%in%c("Sample.ID",var))]
#   
#   data_for_plot=merge(data_sub,meta_sub,by="Sample.ID",all.x=T)
#   
#   
# }
#   
# check=c("OTU_564806","OTU_360015","OTU_1062061","OTU_581079")
# 


