#' @title PQI
#'
#' @param data the input data with diagnosis and procedure codes, object of class data.frame.
#' @param PQI numeric vector indicates which PQI's to calculate.
#' @param append logical. Should the output be appended to the original data file?  Default is FALSE.
#' @param diagcode Character string matching: 'dx' or 'dr'. Default is 'dx'. 
#' @param proccode Character string matching: 'px' or 'pr'. Default is 'px'.
#' @param agevar Character string identifying column name of the age variable.
#' @param idvar Character string identifying column name of the patient ID variable.
#' @param mdcvar Character string identifying column name of the MDC variable. 
#' @param version PQI update version. Currently, only version 6.0 is supported. 
#' @param ICD Character string matching: '9' or '10' to specify the ICD-CM version.
#' 
#' 
#' 
#'
#'

#document(pkg="/Users/gregorymatthews/Dropbox/rPQI")

computePQI <- function(data = data, pqi = c(1:3,5,7:12,14:16,90:93),
                       append = FALSE, diagcode = "dx", proccode = "px",
                       agevar = "age", idvar = "id", mdcvar = "mdc", 
                       version = "6.0", ICD="9"){

  data <- as.data.frame(data)
  id_col <- grep(idvar, colnames(data))
  ID <- data[, id_col]
  age_col <- grep(agevar, colnames(data))
  age <- data[, age_col]
  mdc_col <- grep(mdcvar, colnames(data))
  mdc <- data[, mdc_col]
  #Changing colnames to dx/px
  orig.col <- colnames(data)
  if (diagcode == "dr" & proccode == "pr"){
    newcols <- sub("r", "x", orig.col)
    newcols -> colnames(data)
  } else if(diagcode=="dr") {
    dx.col <- sub("dr", "dx", orig.col)
    dx.col -> colnames(data)
  } else if(proccode =="pr") {
    px.col <- sub("pr", "px", orig.col)
    px.col -> colnames(data)
  }
  
  indicators <- c("PQI.01", "PQI.02", "PQI.03", "PQI.05", "PQI.07", "PQI.08", 
                  "PQI.09", "PQI.10", "PQI.11", "PQI.12", "PQI.14", "PQI.15", 
                  "PQI.16", "PQI.90", "PQI.91", "PQI.92", "PQI.93")
  
  single.dig <- c(1,2,3,5,7,8,9)
  matches <- unique(grep(paste0(pqi,"$", collapse="|"),
                         single.dig, value=T))
  pqi.single <- paste0("0", matches)
  pqi.remain <- as.character(pqi[!pqi %in% matches])
  pqi.full <- c(pqi.single, pqi.remain)
  pqi.match <- unique(grep(paste0(pqi.full,"$", collapse="|"),
                           indicators, value=T))
  
  if (is.null(pqi)==TRUE) {
    pqi.match <- indicators
  }
  
  ################################################
  #PQI 01 : Diabetes Short Term 
  ################################################
  if ("PQI.01" %in% pqi.match & ICD == "9") {
    
  keep01 <- data$dx1 %in% c('25010', '25011', '25012', '25013', '25020', 
                            '25021', '25022', '25023', '25030', '25031', 
                            '25032', '25033')  & data$age >= 18
  
  PQI.01 <- ifelse(keep01, 1, 0)
  
  } else if ("PQI.01" %in% pqi.match & ICD == "10"){
    keep01 <- data$dx1 %in% c('E1010', 'E1011', 'E10641', 'E1065', 'E1100', 
                              'E1101', 'E11641', 'E1165') & data$age >= 18
    
    PQI.01 <- ifelse(keep01, 1, 0)
    
  } else {
    PQI.01 <- NULL
  }

  ################################################
  #PQI 02 : Perforated Appendix 
  ################################################ 
  if ("PQI.02" %in% pqi.match & ICD == "9") {
  keep02 <- (!data$dx1 %in% as.character(c(765:782,998))) & data$mdc != "14"
    data$dx1 %in% c('5400', '5401') |
    data$dx2 %in% c('5400', '5401') |
    data$dx3 %in% c('5400', '5401') |
    data$dx4 %in% c('5400', '5401') |
    data$dx5 %in% c('5400', '5401') |
    data$dx6 %in% c('5400', '5401') |
    data$dx7 %in% c('5400', '5401') |
    data$dx8 %in% c('5400', '5401') |
    data$dx9 %in% c('5400', '5401') |
    data$dx10 %in% c('5400', '5401') & data$age >= 18
  
  PQI.02 <- ifelse(keep02, 1, 0)
  
  keep02d <- (!data$dx1%in% as.character(c(765:782,998)))  & data$mdc != "14"
    (data$dx1 %in% c('5400', '5401', '5409', '541') |
     data$dx2 %in% c('5400', '5401', '5409', '541') |
     data$dx3 %in% c('5400', '5401', '5409', '541') |
     data$dx4 %in% c('5400', '5401', '5409', '541') |
     data$dx5 %in% c('5400', '5401', '5409', '541') |
     data$dx6 %in% c('5400', '5401', '5409', '541') |
     data$dx7 %in% c('5400', '5401', '5409', '541') |
     data$dx8 %in% c('5400', '5401', '5409', '541') |
     data$dx9 %in% c('5400', '5401', '5409', '541') |
     data$dx10 %in% c('5400', '5401', '5409', '541')) & data$age >= 18
    
  PQI.02D <- ifelse(keep02d,1,0)
  
  } else if ("PQI.02" %in% pqi.match & ICD == "10") {
    keep02 <- (data$dx1 %in% c('K352','K353') |
                 data$dx2 %in% c('K352','K353') |
                 data$dx3 %in% c('K352','K353') |
                 data$dx4 %in% c('K352','K353') |
                 data$dx5 %in% c('K352','K353') |
                 data$dx6 %in% c('K352','K353') |
                 data$dx7 %in% c('K352','K353') |
                 data$dx8 %in% c('K352','K353') |
                 data$dx9 %in% c('K352','K353') |
                 data$dx10 %in% c('K352','K353')) &
      data$age >=18 & data$mdc != "14"
    
    PQI.02 <- ifelse(keep02, 1, 0)
    
    keep02d <- (data$dx1 %in% c('K352','K353', 'K3580', 'K3589', 'K37') |
                  data$dx2 %in% c('K352','K353', 'K3580', 'K3589', 'K37') |
                  data$dx3 %in% c('K352','K353', 'K3580', 'K3589', 'K37') |
                  data$dx4 %in% c('K352','K353', 'K3580', 'K3589', 'K37') |
                  data$dx5 %in% c('K352','K353', 'K3580', 'K3589', 'K37') |
                  data$dx6 %in% c('K352','K353', 'K3580', 'K3589', 'K37') |
                  data$dx7 %in% c('K352','K353', 'K3580', 'K3589', 'K37') |
                  data$dx8 %in% c('K352','K353', 'K3580', 'K3589', 'K37') |
                  data$dx9 %in% c('K352','K353', 'K3580', 'K3589', 'K37') |
                  data$dx10 %in% c('K352','K353', 'K3580', 'K3589', 'K37')) &
      data$age >=18 & data$mdc != "14"
    
    PQI.02D <- ifelse(keep02d,1,0)
  }
  else {
    PQI.02 <- NULL
    PQI.02D <- NULL
  }  
  ################################################
  #PQI 03 : Diabetes Long Term Complications 
  ################################################
  if ("PQI.03" %in% pqi.match & ICD == "9") {
  keep03 <- data$dx1 %in% c('25040', '25041', '25042', '25043', '25050', 
                            '25051', '25052', '25053', '25060', '25061',
                            '25062', '25063', '25070', '25071', '25072', 
                            '25073', '25080', '25081', '25082', '25083', 
                            '25090', '25091', '25092', '25093') & data$age >= 18
  
  PQI.03 <- ifelse(keep03, 1, 0)
  
  } else if ("PQI.03" %in% pqi.match & ICD == "10"){
    keep03 <- data$dx1 %in% c('E1021', 'E1121', 'E1022', 'E1122', 'E1029',
                              'E1129', 'E10311', 'E11311', 'E10319', 'E11319',
                              'E10321', 'E11321', 'E10329', 'E11329', 'E10331',
                              'E11331', 'E10339', 'E11339', 'E10341', 'E11341',
                              'E10349', 'E11349', 'E10351', 'E11351', 'E10359',
                              'E11359', 'E1036', 'E1136', 'E1039', 'E1139',
                              'E1040', 'E1140', 'E1041', 'E1141', 'E1042', 
                              'E1142', 'E1043', 'E1143', 'E1044', 'E1144', 
                              'E1049', 'E1149', 'E1051', 'E1151', 'E1052',
                              'E1152', 'E1059', 'E1159', 'E10610', 'E11610',
                              'E10618', 'E11618', 'E10620', 'E11620', 'E10621', 
                              'E11621', 'E10622', 'E11622', 'E10628', 'E11628', 
                              'E10630', 'E11630', 'E10638', 'E11638', 'E1069', 
                              'E1169', 'E108', 'E118') & data$age >= 18
    
    PQI.03 <- ifelse(keep03, 1, 0)
  }
  else {
    PQI.03 <- NULL
  }
  ################################################
  #PQI 05 : COPD | Asthma in Older Adults 
  ################################################
  if ("PQI.05" %in% pqi.match & ICD == "9") {
  exc05 <- c('27700', '27701', '27702', '27703', '27709', '51661', '51662', 
             '51663', '51664', '51669', '74721', '7483', '7484', '7485', 
             '74860','74861', '74869', '7488', '7489', '7503', '7593', '7707') 
  
  keep05 <- data$dx1 %in% c('4910', '4911', '49120', '49121', '49122', '4918', 
                            '4919', '4920', '4928', '494', '4940', '4941', '496',
                            '49300', '49301', '49302', '49310', '49311', '49312',
                            '49320', '49321', '49322', '49381', '49382', '49390',
                            '49391', '49392') & #remove 4660 and 490
    !data$dx1 %in% exc05 & !data$dx2 %in% exc05 & !data$dx3 %in% exc05 & 
    !data$dx4 %in% exc05 & !data$dx5 %in% exc05 & !data$dx6 %in% exc05 & 
    !data$dx7 %in% exc05 & !data$dx8 %in% exc05 & !data$dx9 %in% exc05 & 
    !data$dx10 %in% exc05 & data$age >= 40
  
  PQI.05 <- ifelse(keep05, 1, 0)
  } else if("PQI.05" %in% pqi.match & ICD == "10"){
    exc05 <- c('E840', 'E8411', 'E8419', 'E848', 'E849', 'J8483',
               'J84841', 'J84842', 'J84843', 'J84848', 'P270', 'P271',
               'P278', 'P279', 'Q254', 'Q322', 'Q323', 'Q324', 'Q330', 
               'Q331', 'Q332', 'Q333', 'Q334', 'Q335', 'Q336', 'Q338',
               'Q339', 'Q340', 'Q341', 'Q348', 'Q311', 'Q312', 'Q313',
               'Q315', 'Q318', 'Q319', 'Q320', 'Q321', 'Q349', 'Q390',
               'Q391', 'Q392', 'Q393', 'Q394', 'Q893')
    
    keep05 <- data$dx1 %in% c('J410', 'J439', 'J411', 'J440', 'J418', 'J441', 'J42', 
                              'J449', 'J430', 'J470', 'J431', 'J471', 'J432', 'J479',
                              'J438','J4521', 'J4552', 'J4522', 'J45901', 'J4531',
                              'J45902', 'J4532', 'J45990', 'J4541', 'J45991',
                              'J4542', 'J45998', 'J4551') & 
      !data$dx1 %in% exc05 & !data$dx2 %in% exc05 & !data$dx3 %in% exc05 & 
      !data$dx4 %in% exc05 & !data$dx5 %in% exc05 & !data$dx6 %in% exc05 & 
      !data$dx7 %in% exc05 & !data$dx8 %in% exc05 & !data$dx9 %in% exc05 & 
      !data$dx10 %in% exc05 & data$age >= 40
    
    PQI.05 <- ifelse(keep05, 1, 0)  
    
  }  else {
    PQI.05 <- NULL
  }
  ################################################
  #PQI 07 : Hypertension Admission Rate 
  ################################################  
  if ("PQI.07" %in% pqi.match & ICD == "9") {
  ##Exclusions - Cardiac Procedure Codes##
  
  exc7 <- c('0050', '3582', '0051', '3583', '0052', '0053', '0054', '0056', 
            '0057', '0066', '3584', '3591', '3592', '3593', '3594', '3595',
            '1751', '1752', '3596', '3597', '1755', '3598', '3500', '3599',
            '3501', '3502', '3601', '3602', '3503', '3603', '3504', '3604',
            '3505', '3605', '3506', '3606', '3507', '3607', '3508', '3609',
            '3509', '3610', '3510', '3611', '3511', '3612', '3512', '3613',
            '3513', '3614', '3514', '3615', '3520', '3521', '3522', '3523',
            '3524', '3525', '3526', '3527', '3528', '3531', '3616', '3617',
            '3619', '362', '363', '3631', '3632', '3633', '3634', '3639', 
            '3532', '3691', '3533', '3699', '3534', '3731', '3535', '3732',
            '3539', '3541', '3542', '3733', '3734', '3735', '3550', '3551',
            '3552', '3736', '3737', '3741', '3553', '375', '3554', '3751',
            '3555', '3752', '3560', '3561', '3562', '3753', '3754', '3755',
            '3563', '3760', '3570', '3761', '3571', '3762', '3572', '3763',
            '3573', '3764', '3581', '3765', '3766', '3770', '3771', '3772',
            '3773', '3774', '3775', '3776', '3777', '3778', '3779', '3780',
            '3781', '3782', '3783', '3785', '3786', '3787', '3789', '3794',
            '3795', '3796', '3797', '3798', '3826')
  
  ##Exclusions - Dialysis access##
  
  exc7.2 <- c('3895', '3927', '3929', '3942', '3943', '3993', '3994')
  
  ##Exclusions - Stage I-IV Kidney disease##
  
  exc7.3 <- c('40300', '40310', '40390', '40400', '40410', '40490')
  
  keep07 <- data$dx1 %in% c('4010', '4019', '40200', '40210', '40290', '40300',
                            '40310', '40390', '40400', '40410', '40490') & 
    !data$px1 %in% exc7 & !data$px2 %in% exc7 & !data$px3 %in% exc7 & 
    !data$px4 %in% exc7 & !data$px5 %in% exc7 & !data$px6 %in% exc7 & 
    !data$px7 %in% exc7 & !data$px8 %in% exc7 & !data$px9 %in% exc7 & 
    !data$px10 %in% exc7 & 
    (!data$dx1 %in% exc7.3 | 
       (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 & 
          !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 & 
          !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 & 
          !data$px10 %in% exc7.2)) & 
    (!data$dx2 %in% exc7.3 | 
       (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
          !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 & 
          !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 & 
          !data$px10 %in% exc7.2)) & 
    (!data$dx3 %in% exc7.3 | 
       (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 & 
          !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
          !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
          !data$px10 %in% exc7.2)) & 
    (!data$dx4 %in% exc7.3 | 
       (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
          !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
          !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
          !data$px10 %in% exc7.2)) & 
    (!data$dx5 %in% exc7.3 | 
       (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
          !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
          !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
          !data$px10 %in% exc7.2)) & 
    (!data$dx6 %in% exc7.3 | 
       (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
          !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
          !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
          !data$px10 %in% exc7.2)) & 
    (!data$dx7 %in% exc7.3 | 
       (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
          !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
          !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
          !data$px10 %in% exc7.2)) & 
    (!data$dx8 %in% exc7.3 | 
       (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
          !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
          !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
          !data$px10 %in% exc7.2)) &
    (!data$dx9 %in% exc7.3 | 
       (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
          !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
          !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
          !data$px10 %in% exc7.2)) & 
    (!data$dx10 %in% exc7.3 | 
       (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
          !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
          !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
          !data$px10 %in% exc7.2)) & data$age >= 18
  
  PQI.07 <- ifelse(keep07, 1, 0)
  
  } else if ("PQI.07" %in% pqi.match & ICD == "10"){
    ##Exclusions - Cardiac Procedure Codes##
    
    exc7 <- c('0210093', '0210098', '0210099', '021009C', '021009F', '021009W', 
              '02100A3', '02100A8', '02100A9', '02100AC', '02100AF', '02100AW', 
              '02100J3', '02100J8', '02100J9', '02100JC', '02100JF', '02100JW', 
              '02100K3', '02100K8', '02100K9', '02100KC', '02100KF', '02100KW', 
              '02100Z3', '02100Z8', '02100Z9', '02100ZC', '02100ZF', '0210344',
              '02103D4', '0210444', '0210493', '0210498', '0210499', '021049C', 
              '021049F', '021049W', '02104A3', '02104A8', '02104A9', '02104AC', 
              '02104AF', '02104AW', '02104D4', '02104J3', '02104J8', '02104J9',
              '02104JC', '02104JF', '02104JW', '02104K3', '02104K8', '02104K9',
              '02104KC', '02104KF', '02104KW', '02104Z3', '02104Z8', '02104Z9',
              '02104ZC', '02104ZF', '0211093', '0211098', '0211099', '021109C',
              '021109F', '021109W', '02110A3', '02110A8', '02110A9', '02110AC',
              '02110AF', '02110AW', '02110J3', '02110J8', '02110J9', '02110JC',
              '02110JF', '02110JW', '02110K3', '02110K8', '02110K9', '02110KC',
              '02110KF', '02110KW', '02110Z3', '02110Z8', '02110Z9', '02110ZC',
              '02110ZF', '0211344', '02113D4', '0211444', '0211493', '0211498',
              '0211499', '021149C', '021149F', '021149W', '02114A3', '02114A8',
              '02114A9', '02114AC', '02114AF', '02114AW', '02114D4', '02114J3',
              '02114J8', '02114J9', '02114JC', '02114JF', '02114JW', '02114K3',
              '02114K8', '02114K9', '02114KC', '02114KF', '02114KW', '02114Z3',
              '02114Z8', '02114Z9', '02114ZC', '02114ZF', '0212093', '0212098',
              '0212099', '021209C', '021209F', '021209W', '02120A3', '02120A8',
              '02120A9', '02120AC', '02120AF', '02120AW', '02120J3', '02120J8',
              '02120J9', '02120JC', '02120JF', '02120JW', '02120K3', '02120K8',
              '02120K9', '02120KC', '02120KF', '02120KW', '02120Z3', '02120Z8',
              '02120Z9', '02120ZC', '02120ZF', '0212344', '02123D4', '0212444',
              '0212493', '0212498', '0212499', '021249C', '021249F', '021249W',
              '02124A3', '02124A8', '02124A9', '02124AC', '02124AF', '02124AW',
              '02124D4', '02124J3', '02124J8', '02124J9', '02124JC', '02124JF',
              '02124JW', '02124K3', '02124K8', '02124K9', '02124KC', '02124KF',
              '02124KW', '02124Z3', '02124Z8', '02124Z9', '02124ZC', '02124ZF',
              '0213093', '0213098', '0213099', '021309C', '021309F', '021309W',
              '02130A3', '02130A8', '02130A9', '02130AC', '02130AF', '02130AW',
              '02130J3', '02130J8', '02130J9', '02130JC', '02130JF', '02130JW',
              '02130K3', '02130K8', '02130K9', '02130KC', '02130KF', '02130KW',
              '02130Z3', '02130Z8', '02130Z9', '02130ZC', '02130ZF', '0213344',
              '02133D4', '0213444', '0213493', '0213498', '0213499', '021349C',
              '021349F', '021349W', '02134A3', '02134A8', '02134A9', '02134AC',
              '02134AF', '02134AW', '02134D4', '02134J3', '02134J8', '02134J9',
              '02134JC', '02134JF', '02134JW', '02134K3', '02134K8', '02134K9',
              '02134KC', '02134KF', '02134KW', '02134Z3', '02134Z8', '02134Z9',
              '02134ZC', '02134ZF', '021609P', '021609Q', '021609R', '02160AP',
              '02160AQ', '02160AR', '02160JP', '02160JQ', '02160JR', '02160KP',
              '02160KQ', '02160KR', '02160ZP', '02160ZQ', '02160ZR', '021649P',
              '021649Q', '021649R', '02164AP', '02164AQ', '02164AR', '02164JP',
              '02164JQ', '02164JR', '02164KP', '02164KQ', '02164KR', '02164ZP',
              '02164ZQ', '02164ZR', '021709P', '021709Q', '021709R', '02170AP',
              '02170AQ', '02170AR', '02170JP', '02170JQ', '02170JR', '02170KP',
              '02170KQ', '02170KR', '02170ZP', '02170ZQ', '02170ZR', '021749P',
              '021749Q', '021749R', '02174AP', '02174AQ', '02174AR', '02174JP',
              '02174JQ', '02174JR', '02174KP', '02174KQ', '02174KR', '02174ZP',
              '02174ZQ', '02174ZR', '021K09P', '021K09Q', '021K09R', '021K0AP',
              '021K0AQ', '021K0AR', '021K0JP', '021K0JQ', '021K0JR', '021K0KP',
              '021K0KQ', '021K0KR', '021K0Z5', '021K0Z8', '021K0Z9', '021K0ZC',
              '021K0ZF', '021K0ZP', '021K0ZQ', '021K0ZR', '021K0ZW', '021K49P',
              '021K49Q', '021K49R', '021K4AP', '021K4AQ', '021K4AR', '021K4JP',
              '021K4JQ', '021K4JR', '021K4KP', '021K4KQ', '021K4KR', '021K4Z5',
              '021K4Z8', '021K4Z9', '021K4ZC', '021K4ZF', '021L09R', '021L0AP', 
              '021L0AQ', '021L0AR', '021L0JP', '021L0JQ', '021K4ZP', '021K4ZQ',
              '021K4ZR', '021K4ZW', '021L09P', '021L09Q', '021L0JR', '021L0KP',
              '021L0KQ', '021L0KR', '021L0Z5', '021L0Z8', '021L0Z9', '021L0ZC',
              '021L0ZF', '021L0ZP', '021L0ZQ', '021L0ZR', '021L0ZW', '021L49P',
              '021L49Q', '021L49R', '021L4AP', '021L4AQ', '021L4AR', '021L4JP',
              '021L4JQ', '021L4JR', '021L4KP', '021L4KQ', '021L4KR', '021L4Z5',
              '021L4Z8', '021L4Z9', '021L4ZC', '021L4ZF', '021L4ZP', '021L4ZQ',
              '021L4ZR', '021L4ZW', '02540ZZ', '02543ZZ', '02544ZZ', '02550ZZ',
              '02553ZZ', '02554ZZ', '02560ZZ', '02563ZZ', '02564ZZ', '02570ZK',
              '02570ZZ', '02573ZK', '02573ZZ', '02574ZK', '02574ZZ', '02580ZZ',
              '02583ZZ', '02584ZZ', '02590ZZ', '02593ZZ', '02594ZZ', '025D0ZZ',
              '025D3ZZ', '025D4ZZ', '025F0ZZ', '025F3ZZ', '025F4ZZ', '025G0ZZ',
              '025G3ZZ', '025G4ZZ', '025H0ZZ', '025H3ZZ', '025H4ZZ', '025J0ZZ',
              '025J3ZZ', '025J4ZZ', '025K0ZZ', '025K3ZZ', '025K4ZZ', '025L0ZZ', 
              '025L3ZZ', '025L4ZZ', '025M0ZZ', '025M3ZZ', '025M4ZZ', '025N0ZZ', 
              '025N3ZZ', '025N4ZZ', '0270046', '027004Z', '02700D6', '02700DZ',
              '02700T6', '02700TZ', '02700Z6', '02700ZZ', '0270346', '027034Z', 
              '02703D6', '02703DZ', '02703T6', '02703TZ', '02703Z6', '02703ZZ',
              '0270446', '027044Z', '02704D6', '02704DZ', '02704T6', '02704TZ',
              '02704Z6', '02704ZZ', '0271046', '027104Z', '02710D6', '02710DZ',
              '02710T6', '02710TZ', '02710Z6', '02710ZZ', '0271346', '027134Z',
              '02713D6', '02713DZ', '02713T6', '02713TZ', '02713Z6', '02713ZZ',
              '0271446', '027144Z', '02714D6', '02714DZ', '02714T6', '02714TZ',
              '02714Z6', '02714ZZ', '0272046', '027204Z', '02720D6', '02720DZ',
              '02720T6', '02720TZ', '02720Z6', '02720ZZ', '0272346', '027234Z', 
              '02723D6', '02723DZ', '02723T6', '02723TZ', '02723Z6', '02723ZZ',
              '0272446', '027244Z', '02724D6', '02724DZ', '02724T6', '02724TZ',
              '02724Z6', '02724ZZ', '0273046', '027304Z', '02730D6', '02730DZ',
              '02730T6', '02730TZ', '02730Z6', '02730ZZ', '0273346', '027334Z',
              '02733D6', '02733DZ', '02733T6', '02733TZ', '02733Z6', '02733ZZ', 
              '0273446', '027344Z', '02734D6', '02734DZ', '02734T6', '02734TZ',
              '02734Z6', '02734ZZ', '027F04Z', '027F0DZ', '027F0ZZ', '027F34Z',
              '027F3DZ', '027F3ZZ', '027F44Z', '027F4DZ', '027F4ZZ', '027G04Z',
              '027G0DZ', '027G0ZZ', '027G34Z', '027G3DZ', '027G3ZZ', '027G44Z', 
              '027G4DZ', '027G4ZZ', '027H04Z', '027H0DZ', '027H0ZZ', '027H34Z',
              '027H3DZ', '027H3ZZ', '027H44Z', '027H4DZ', '027H4ZZ', '027J04Z',
              '027J0DZ', '027J0ZZ', '027J34Z', '027J3DZ', '027J3ZZ', '027J44Z',
              '027J4DZ', '027J4ZZ', '02890ZZ', '02893ZZ', '02894ZZ', '028D0ZZ',
              '028D3ZZ', '028D4ZZ', '02B40ZZ', '02B43ZZ', '02B44ZZ', '02B50ZZ',
              '02B53ZZ', '02B54ZZ', '02B60ZZ', '02B63ZZ', '02B64ZZ', '02B70ZK',
              '02B70ZZ', '02B73ZK', '02B73ZZ', '02B74ZK', '02B74ZZ', '02B80ZZ',
              '02B83ZZ', '02B84ZZ', '02B90ZZ', '02B93ZZ', '02B94ZZ', '02BD0ZZ',
              '02BD3ZZ', '02BD4ZZ', '02BF0ZZ', '02BF3ZZ', '02BF4ZZ', '02BG0ZZ',
              '02BG3ZZ', '02BG4ZZ', '02BH0ZZ', '02BH3ZZ', '02BH4ZZ', '02BJ0ZZ',
              '02BJ3ZZ', '02BJ4ZZ', '02BK0ZZ', '02BK3ZZ', '02BK4ZZ', '02BL0ZZ',
              '02BL3ZZ', '02BL4ZZ', '02BM0ZZ', '02BM3ZZ', '02BM4ZZ', '02BN0ZZ', 
              '02BN3ZZ', '02BN4ZZ', '02C00ZZ', '02C03ZZ', '02C04ZZ', '02C10ZZ',
              '02C13ZZ', '02C14ZZ', '02C20ZZ', '02C23ZZ', '02C24ZZ', '02C30ZZ',
              '02C33ZZ', '02C34ZZ', '02C40ZZ', '02C43ZZ', '02C44ZZ', '02C50ZZ',
              '02C53ZZ', '02C54ZZ', '02CD0ZZ', '02CD3ZZ', '02CD4ZZ', '02CF0ZZ',
              '02CF3ZZ', '02CF4ZZ', '02CG0ZZ', '02CG3ZZ', '02CG4ZZ', '02CH0ZZ', 
              '02CH3ZZ', '02CH4ZZ', '02CJ0ZZ', '02CJ3ZZ', '02CJ4ZZ', '02CM0ZZ', 
              '02CM3ZZ', '02CM4ZZ', '02H400Z', '02H402Z', '02H403Z', '02H40DZ',
              '02H40JZ', '02H40KZ', '02H40MZ', '02H430Z', '02H432Z', '02H433Z',
              '02H43DZ', '02H43JZ', '02H43KZ', '02H43MZ', '02H440Z', '02H442Z', 
              '02H443Z', '02H44DZ', '02H44JZ', '02H44KZ', '02H44MZ', '02H600Z',
              '02H60JZ', '02H60KZ', '02H60MZ', '02H630Z', '02H63JZ', '02H63KZ', 
              '02H63MZ', '02H640Z', '02H64JZ', '02H64KZ', '02H64MZ', '02H700Z',
              '02H70JZ', '02H70KZ', '02H70MZ', '02H730Z', '02H73JZ', '02H73KZ', 
              '02H73MZ', '02H740Z', '02H74JZ', '02H74KZ', '02H74MZ', '02HA0QZ',
              '02HA0RS', '02HA0RZ', '02HA3QZ', '02HA3RS', '02HA3RZ', '02HA4QZ',
              '02HA4RS', '02HA4RZ', '02HK00Z', '02HK02Z', '02HK0JZ', '02HK0KZ',
              '02HK0MZ', '02HK30Z', '02HK32Z', '02HK3JZ', '02HK3KZ', '02HK3MZ',
              '02HK40Z', '02HK42Z', '02HK4JZ', '02HK4KZ', '02HK4MZ', '02HL00Z',
              '02HL0JZ', '02HL0KZ', '02HL0MZ', '02HL30Z', '02HL3JZ', '02HL3KZ',
              '02HL3MZ', '02HL40Z', '02HL4JZ', '02HL4KZ', '02HL4MZ', '02HN0JZ',
              '02HN0KZ', '02HN0MZ', '02HN3JZ', '02HN3KZ', '02HN3MZ', '02HN4JZ',
              '02HN4KZ', '02HN4MZ', '02HS00Z', '02HS30Z', '02HS40Z', '02HT00Z', 
              '02HT30Z', '02HT40Z', '02HV00Z', '02HV30Z', '02HV40Z', '02L70CK',
              '02L70DK', '02L70ZK', '02L73CK', '02L73DK', '02L73ZK', '02L74CK',
              '02L74DK', '02L74ZK', '02LR0ZT', '02LS0ZZ', '02LT0ZZ', '02N50ZZ',
              '02N53ZZ', '02N54ZZ', '02N90ZZ', '02N93ZZ', '02N94ZZ', '02ND0ZZ',
              '02ND3ZZ', '02ND4ZZ', '02NF0ZZ', '02NF3ZZ', '02NF4ZZ', '02NG0ZZ',
              '02NG3ZZ', '02NG4ZZ', '02NH0ZZ', '02NH3ZZ', '02NH4ZZ', '02NJ0ZZ',
              '02NJ3ZZ', '02NJ4ZZ', '02NK0ZZ', '02NK3ZZ', '02NK4ZZ', '02NL0ZZ',
              '02NL3ZZ', '02NL4ZZ', '02NM0ZZ', '02NM3ZZ', '02NM4ZZ', '02PA0MZ',
              '02PA0QZ', '02PA0RZ', '02PA3MZ', '02PA3QZ', '02PA3RZ', '02PA4MZ',
              '02PA4QZ', '02PA4RZ', '02PAXMZ', '02Q00ZZ', '02Q03ZZ', '02Q04ZZ',
              '02Q10ZZ', '02Q13ZZ', '02Q14ZZ', '02Q20ZZ', '02Q23ZZ', '02Q24ZZ',
              '02Q30ZZ', '02Q33ZZ', '02Q34ZZ', '02Q40ZZ', '02Q43ZZ', '02Q44ZZ',
              '02Q50ZZ', '02Q53ZZ', '02Q54ZZ', '02Q70ZZ', '02Q73ZZ', '02Q74ZZ',
              '02Q90ZZ', '02Q93ZZ', '02Q94ZZ', '02QA0ZZ', '02QA3ZZ', '02QA4ZZ',
              '02QB0ZZ', '02QB3ZZ', '02QB4ZZ', '02QC0ZZ', '02QC3ZZ', '02QC4ZZ',
              '02QD0ZZ', '02QD3ZZ', '02QD4ZZ', '02QF0ZZ', '02QF3ZZ', '02QF4ZZ',
              '02QG0ZZ', '02QG3ZZ', '02QG4ZZ', '02QH0ZZ', '02QH3ZZ', '02QH4ZZ',
              '02QJ0ZZ', '02QJ3ZZ', '02QJ4ZZ', '02QM0ZZ', '02QM3ZZ', '02QM4ZZ',
              '02R907Z', '02R908Z', '02R90JZ', '02R90KZ', '02R947Z', '02R948Z',
              '02R94JZ', '02R94KZ', '02RD07Z', '02RD08Z', '02RD0JZ', '02RD0KZ',
              '02RD47Z', '02RD48Z', '02RD4JZ', '02RD4KZ', '02RF07Z', '02RF08Z', 
              '02RF0JZ', '02RF0KZ', '02RF37H', '02RF37Z', '02RF38H', '02RF38Z',
              '02RF3JH', '02RF3JZ', '02RF3KH', '02RF3KZ', '02RF47Z', '02RF48Z',
              '02RF4JZ', '02RF4KZ', '02RG07Z', '02RG08Z', '02RG0JZ', '02RG0KZ',
              '02RG37H', '02RG37Z', '02RG38H', '02RG38Z', '02RG3JH', '02RG3JZ',
              '02RG3KH', '02RG3KZ', '02RG47Z', '02RG48Z', '02RG4JZ', '02RG4KZ',
              '02RH07Z', '02RH08Z', '02RH0JZ', '02RH0KZ', '02RH37H', '02RH37Z',
              '02RH38H', '02RH38Z', '02RH3JH', '02RH3JZ', '02RH3KH', '02RH3KZ', 
              '02RH47Z', '02RH48Z', '02RH4JZ', '02RH4KZ', '02RJ07Z', '02RJ08Z',
              '02RJ0JZ', '02RJ0KZ', '02RJ47Z', '02RJ48Z', '02RJ4JZ', '02RJ4KZ',
              '02RK07Z', '02RK0JZ', '02RK0KZ', '02RK47Z', '02RK4KZ', '02RL07Z',
              '02RL0JZ', '02RL0KZ', '02RL47Z', '02RL4KZ', '02RM07Z', '02RM0JZ',
              '02RM0KZ', '02RM47Z', '02RM4JZ', '02RM4KZ', '02RP0JZ', '02RQ07Z',
              '02RQ0JZ', '02RR07Z', '02RR0JZ', '02SP0ZZ', '02SW0ZZ', '02T50ZZ',
              '02T53ZZ', '02T54ZZ', '02T80ZZ', '02T83ZZ', '02T84ZZ', '02T90ZZ',
              '02T93ZZ', '02T94ZZ', '02TD0ZZ', '02TD3ZZ', '02TD4ZZ', '02TH0ZZ',
              '02TH3ZZ', '02TH4ZZ', '02TM0ZZ', '02TM3ZZ', '02TM4ZZ', '02TN0ZZ',
              '02TN3ZZ', '02TN4ZZ', '02U507Z', '02U508Z', '02U50JZ', '02U50KZ',
              '02U537Z', '02U538Z', '02U53JZ', '02U53KZ', '02U547Z', '02U548Z',
              '02U54JZ', '02U54KZ', '02U607Z', '02U608Z', '02U60KZ', '02U707Z',
              '02U708Z', '02U70JZ', '02U70KZ', '02U737Z', '02U738Z', '02U73KZ',
              '02U747Z', '02U748Z', '02U74KZ', '02U907Z', '02U908Z', '02U90JZ', 
              '02U90KZ', '02U937Z', '02U938Z', '02U93JZ', '02U93KZ', '02U947Z',
              '02U948Z', '02U94JZ', '02U94KZ', '02UA0JZ', '02UA3JZ', '02UA4JZ',
              '02UD07Z', '02UD08Z', '02UD0JZ', '02UD0KZ', '02UD37Z', '02UD38Z',
              '02UD3JZ', '02UD3KZ', '02UD47Z', '02UD48Z', '02UD4JZ', '02UD4KZ',
              '02UF07Z', '02UF08Z', '02UF0JZ', '02UF0KZ', '02UF37Z', '02UF38Z',
              '02UF3JZ', '02UF3KZ', '02UF47Z', '02UF48Z', '02UF4JZ', '02UF4KZ', 
              '02UG07Z', '02UG08Z', '02UG0JZ', '02UG0KZ', '02UG37Z', '02UG38Z', 
              '02UG3JZ', '02UG3KZ', '02UG47Z', '02UG48Z', '02UG4JZ', '02UG4KZ',
              '02UH07Z', '02UH08Z', '02UH0JZ', '02UH0KZ', '02UH37Z', '02UH38Z',
              '02UH3JZ', '02UH3KZ', '02UH47Z', '02UH48Z', '02UH4JZ', '02UH4KZ',
              '02UJ07Z', '02UJ08Z', '02UJ0JZ', '02UJ0KZ', '02UJ37Z', '02UJ38Z',
              '02UJ3JZ', '02UJ3KZ', '02UJ47Z', '02UJ48Z', '02UJ4JZ', '02UJ4KZ',
              '02UK0KZ', '02UK3KZ', '02UK4KZ', '02UL0KZ', '02UL3KZ', '02UL4KZ', 
              '02UM07Z', '02UM0JZ', '02UM0KZ', '02UM37Z', '02UM38Z', '02UM3JZ',
              '02UM3KZ', '02UM47Z', '02UM48Z', '02UM4JZ', '02UM4KZ', '02VR0ZT',
              '02W50JZ', '02W54JZ', '02WA0JZ', '02WA0MZ', '02WA0QZ', '02WA0RZ',
              '02WA3MZ', '02WA3QZ', '02WA3RZ', '02WA4MZ', '02WA4QZ', '02WA4RZ',
              '02WF07Z', '02WF08Z', '02WF0JZ', '02WF0KZ', '02WF47Z', '02WF48Z',
              '02WF4JZ', '02WF4KZ', '02WG07Z', '02WG08Z', '02WG0JZ', '02WG0KZ',
              '02WG47Z', '02WG48Z', '02WG4JZ', '02WG4KZ', '02WH07Z', '02WH08Z',
              '02WH0JZ', '02WH0KZ', '02WH47Z', '02WH48Z', '02WH4JZ', '02WH4KZ',
              '02WJ07Z', '02WJ08Z', '02WJ0JZ', '02WJ0KZ', '02WJ47Z', '02WJ48Z',
              '02WJ4JZ', '02WJ4KZ', '02WM0JZ', '02WM4JZ', '02YA0Z0', '02YA0Z1',
              '02YA0Z2', '0JH600Z', '0JH604Z', '0JH605Z', '0JH606Z', '0JH607Z',
              '0JH608Z', '0JH609Z', '0JH60AZ', '0JH60PZ', '0JH630Z', '0JH634Z',
              '0JH635Z', '0JH636Z', '0JH637Z', '0JH638Z', '0JH639Z', '0JH63AZ',
              '0JH63PZ', '0JH800Z', '0JH804Z', '0JH805Z', '0JH806Z', '0JH807Z',
              '0JH808Z', '0JH809Z', '0JH80AZ', '0JH80PZ', '0JH830Z', '0JH834Z',
              '0JH835Z', '0JH836Z', '0JH837Z', '0JH838Z', '0JH839Z', '0JH83AZ',
              '0JH83PZ', '0JPT0PZ', '0JPT3PZ', '0JWT0PZ', '0JWT3PZ', '3E07017',
              '3E070PZ', '3E07317', '3E073PZ', '5A02110', '5A02116', '5A02210',
              '5A02216', '5A1213Z', '5A1223Z')
    
    ##Exclusions - Dialysis access##
    
    exc7.2 <- c('03170AD', '03170AF', '031209D', '031209F', '03120AD', '03120AF', 
                '03120JD', '03120JF', '03120KD', '03120KF', '03120ZD', '03120ZF',
                '031309D', '031309F', '03130AD', '03130AF', '03130JD', '03130JF', 
                '03130KD', '03130KF', '03130ZD', '03130ZF', '031409D', '031409F',
                '03140AD', '03140AF', '03140JD', '03140JF', '03140KD', '03140KF',
                '03140ZD', '03140ZF', '031509D', '031509F', '03150AD', '03150AF',
                '03150JD', '03150JF', '03150KD', '03150KF', '03150ZD', '03150ZF',
                '031609D', '031609F', '03160AD', '03160AF', '03160JD', '03160JF',
                '03160KD', '03160KF', '03160ZD', '03160ZF', '031709D', '031709F', 
                '03170JD', '03170JF', '03170KD', '03170KF', '03170ZD', '03170ZF',
                '031809D', '031809F', '03180AD', '03180AF', '03180JD', '03180JF',
                '03180KD', '03180KF', '03180ZD', '03180ZF', '031909F', '03190AF',
                '03190JF', '03190KF', '03190ZF', '031A09F', '031A0AF', '031A0JF',
                '031A0KF', '031A0ZF', '031B09F', '031B0AF', '031B0JF', '031B0KF',
                '031B0ZF', '031C09F', '031C0AF', '031C0JF', '031C0KF', '031C0ZF',
                '03PY07Z', '03PY0JZ', '03PY0KZ', '03PY37Z', '03PY3JZ', '03PY3KZ',
                '03PY47Z', '03PY4JZ', '03PY4KZ', '03WY0JZ', '03WY3JZ', '03WY4JZ',
                '03WYXJZ', '05HY33Z', '06HY33Z')
    
    
    ##Exclusions - Stage I-IV Kidney disease##
    
    exc7.3 <- c('I129', 'I1310')
    
    keep07 <- data$dx1 %in% c('I10', 'I129', 'I119', 'I1310') & 
      !data$px1 %in% exc7 & !data$px2 %in% exc7 & !data$px3 %in% exc7 & 
      !data$px4 %in% exc7 & !data$px5 %in% exc7 & !data$px6 %in% exc7 & 
      !data$px7 %in% exc7 & !data$px8 %in% exc7 & !data$px9 %in% exc7 & 
      !data$px10 %in% exc7 & 
      (!data$dx1 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 & 
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 & 
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 & 
            !data$px10 %in% exc7.2)) & 
      (!data$dx2 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 & 
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 & 
            !data$px10 %in% exc7.2)) & 
      (!data$dx3 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 & 
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
            !data$px10 %in% exc7.2)) & 
      (!data$dx4 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
            !data$px10 %in% exc7.2)) & 
      (!data$dx5 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
            !data$px10 %in% exc7.2)) & 
      (!data$dx6 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
            !data$px10 %in% exc7.2)) & 
      (!data$dx7 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
            !data$px10 %in% exc7.2)) & 
      (!data$dx8 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
            !data$px10 %in% exc7.2)) &
      (!data$dx9 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
            !data$px10 %in% exc7.2)) & 
      (!data$dx10 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
            !data$px10 %in% exc7.2)) & data$age >= 18
    
    PQI.07 <- ifelse(keep07, 1, 0)
  } else {
    PQI.07 <- NULL
  }
  ################################################
  #PQI 08 : Heart Failure  
  ################################################
  if ("PQI.08" %in% pqi.match & ICD == "9") {
  ##Exclusions - Cardiac Procedure Codes##
  
  exc8 <- c('0050', '3582', '0051', '3583', '0052', '0053', '0054', '0056',
            '0057', '0066', '3584', '3591', '3592', '3593', '3594', '3595', 
            '1751', '1752', '3596', '3597', '1755', '3598', '3500', '3599',
            '3501', '3502', '3601', '3602', '3503', '3603', '3504', '3604',
            '3505', '3605', '3506', '3606', '3507', '3607', '3508', '3609',
            '3509', '3610', '3510', '3611', '3511', '3612', '3512', '3613',
            '3513', '3614', '3514', '3615', '3520', '3521', '3522', '3523',
            '3524', '3525', '3526', '3527', '3528', '3531', '3616', '3617',
            '3619', '362', '363', '3631', '3632', '3633', '3634', '3639',
            '3532', '3691', '3533', '3699', '3534', '3731', '3535', '3732',
            '3539', '3541', '3542', '3733', '3734', '3735', '3550', '3551',
            '3552', '3736', '3737', '3741', '3553', '375', '3554', '3751', 
            '3555', '3752', '3560', '3561', '3562', '3753', '3754', '3755',
            '3563', '3760', '3570', '3761', '3571', '3762', '3572', '3763', 
            '3573', '3764', '3581', '3765', '3766', '3770', '3771', '3772',
            '3773', '3774', '3775', '3776', '3777', '3778', '3779', '3780',
            '3781', '3782', '3783', '3785', '3786', '3787', '3789', '3794',
            '3795', '3796', '3797', '3798', '3826')
  
  keep08 <- data$dx1 %in% c('39891', '40201', '40211', '40291', '40401', '40403',
                            '40411', '40413', '40491', '40493', '4280', '4281',
                            '42820', '42821', '42822', '42823', '42830',#42822 twice
                            '42831', '42832', '42833', '42840', '42841', '42842',
                            '42843', '4289')  & 
    (!data$px1 %in% exc8 & !data$px2 %in% exc8 & !data$px3 %in% exc8 & 
     !data$px4 %in% exc8 & !data$px5 %in% exc8 & !data$px6 %in% exc8 & 
     !data$px7 %in% exc8 & !data$px8 %in% exc8 & !data$px9 %in% exc8 & 
     !data$px10 %in% exc8) & data$age >= 18
  
  PQI.08 <- ifelse(keep08,1,0)
  
  } else if ("PQI.08" %in% pqi.match & ICD == "10"){
    ##Exclusions - Cardiac Procedures (same as exc7)
    
    exc8 <- c('0210093', '0210098', '0210099', '021009C', '021009F', '021009W', 
              '02100A3', '02100A8', '02100A9', '02100AC', '02100AF', '02100AW', 
              '02100J3', '02100J8', '02100J9', '02100JC', '02100JF', '02100JW', 
              '02100K3', '02100K8', '02100K9', '02100KC', '02100KF', '02100KW', 
              '02100Z3', '02100Z8', '02100Z9', '02100ZC', '02100ZF', '0210344',
              '02103D4', '0210444', '0210493', '0210498', '0210499', '021049C', 
              '021049F', '021049W', '02104A3', '02104A8', '02104A9', '02104AC', 
              '02104AF', '02104AW', '02104D4', '02104J3', '02104J8', '02104J9',
              '02104JC', '02104JF', '02104JW', '02104K3', '02104K8', '02104K9',
              '02104KC', '02104KF', '02104KW', '02104Z3', '02104Z8', '02104Z9',
              '02104ZC', '02104ZF', '0211093', '0211098', '0211099', '021109C',
              '021109F', '021109W', '02110A3', '02110A8', '02110A9', '02110AC',
              '02110AF', '02110AW', '02110J3', '02110J8', '02110J9', '02110JC',
              '02110JF', '02110JW', '02110K3', '02110K8', '02110K9', '02110KC',
              '02110KF', '02110KW', '02110Z3', '02110Z8', '02110Z9', '02110ZC',
              '02110ZF', '0211344', '02113D4', '0211444', '0211493', '0211498',
              '0211499', '021149C', '021149F', '021149W', '02114A3', '02114A8',
              '02114A9', '02114AC', '02114AF', '02114AW', '02114D4', '02114J3',
              '02114J8', '02114J9', '02114JC', '02114JF', '02114JW', '02114K3',
              '02114K8', '02114K9', '02114KC', '02114KF', '02114KW', '02114Z3',
              '02114Z8', '02114Z9', '02114ZC', '02114ZF', '0212093', '0212098',
              '0212099', '021209C', '021209F', '021209W', '02120A3', '02120A8',
              '02120A9', '02120AC', '02120AF', '02120AW', '02120J3', '02120J8',
              '02120J9', '02120JC', '02120JF', '02120JW', '02120K3', '02120K8',
              '02120K9', '02120KC', '02120KF', '02120KW', '02120Z3', '02120Z8',
              '02120Z9', '02120ZC', '02120ZF', '0212344', '02123D4', '0212444',
              '0212493', '0212498', '0212499', '021249C', '021249F', '021249W',
              '02124A3', '02124A8', '02124A9', '02124AC', '02124AF', '02124AW',
              '02124D4', '02124J3', '02124J8', '02124J9', '02124JC', '02124JF',
              '02124JW', '02124K3', '02124K8', '02124K9', '02124KC', '02124KF',
              '02124KW', '02124Z3', '02124Z8', '02124Z9', '02124ZC', '02124ZF',
              '0213093', '0213098', '0213099', '021309C', '021309F', '021309W',
              '02130A3', '02130A8', '02130A9', '02130AC', '02130AF', '02130AW',
              '02130J3', '02130J8', '02130J9', '02130JC', '02130JF', '02130JW',
              '02130K3', '02130K8', '02130K9', '02130KC', '02130KF', '02130KW',
              '02130Z3', '02130Z8', '02130Z9', '02130ZC', '02130ZF', '0213344',
              '02133D4', '0213444', '0213493', '0213498', '0213499', '021349C',
              '021349F', '021349W', '02134A3', '02134A8', '02134A9', '02134AC',
              '02134AF', '02134AW', '02134D4', '02134J3', '02134J8', '02134J9',
              '02134JC', '02134JF', '02134JW', '02134K3', '02134K8', '02134K9',
              '02134KC', '02134KF', '02134KW', '02134Z3', '02134Z8', '02134Z9',
              '02134ZC', '02134ZF', '021609P', '021609Q', '021609R', '02160AP',
              '02160AQ', '02160AR', '02160JP', '02160JQ', '02160JR', '02160KP',
              '02160KQ', '02160KR', '02160ZP', '02160ZQ', '02160ZR', '021649P',
              '021649Q', '021649R', '02164AP', '02164AQ', '02164AR', '02164JP',
              '02164JQ', '02164JR', '02164KP', '02164KQ', '02164KR', '02164ZP',
              '02164ZQ', '02164ZR', '021709P', '021709Q', '021709R', '02170AP',
              '02170AQ', '02170AR', '02170JP', '02170JQ', '02170JR', '02170KP',
              '02170KQ', '02170KR', '02170ZP', '02170ZQ', '02170ZR', '021749P',
              '021749Q', '021749R', '02174AP', '02174AQ', '02174AR', '02174JP',
              '02174JQ', '02174JR', '02174KP', '02174KQ', '02174KR', '02174ZP',
              '02174ZQ', '02174ZR', '021K09P', '021K09Q', '021K09R', '021K0AP',
              '021K0AQ', '021K0AR', '021K0JP', '021K0JQ', '021K0JR', '021K0KP',
              '021K0KQ', '021K0KR', '021K0Z5', '021K0Z8', '021K0Z9', '021K0ZC',
              '021K0ZF', '021K0ZP', '021K0ZQ', '021K0ZR', '021K0ZW', '021K49P',
              '021K49Q', '021K49R', '021K4AP', '021K4AQ', '021K4AR', '021K4JP',
              '021K4JQ', '021K4JR', '021K4KP', '021K4KQ', '021K4KR', '021K4Z5',
              '021K4Z8', '021K4Z9', '021K4ZC', '021K4ZF', '021L09R', '021L0AP', 
              '021L0AQ', '021L0AR', '021L0JP', '021L0JQ', '021K4ZP', '021K4ZQ',
              '021K4ZR', '021K4ZW', '021L09P', '021L09Q', '021L0JR', '021L0KP',
              '021L0KQ', '021L0KR', '021L0Z5', '021L0Z8', '021L0Z9', '021L0ZC',
              '021L0ZF', '021L0ZP', '021L0ZQ', '021L0ZR', '021L0ZW', '021L49P',
              '021L49Q', '021L49R', '021L4AP', '021L4AQ', '021L4AR', '021L4JP',
              '021L4JQ', '021L4JR', '021L4KP', '021L4KQ', '021L4KR', '021L4Z5',
              '021L4Z8', '021L4Z9', '021L4ZC', '021L4ZF', '021L4ZP', '021L4ZQ',
              '021L4ZR', '021L4ZW', '02540ZZ', '02543ZZ', '02544ZZ', '02550ZZ',
              '02553ZZ', '02554ZZ', '02560ZZ', '02563ZZ', '02564ZZ', '02570ZK',
              '02570ZZ', '02573ZK', '02573ZZ', '02574ZK', '02574ZZ', '02580ZZ',
              '02583ZZ', '02584ZZ', '02590ZZ', '02593ZZ', '02594ZZ', '025D0ZZ',
              '025D3ZZ', '025D4ZZ', '025F0ZZ', '025F3ZZ', '025F4ZZ', '025G0ZZ',
              '025G3ZZ', '025G4ZZ', '025H0ZZ', '025H3ZZ', '025H4ZZ', '025J0ZZ',
              '025J3ZZ', '025J4ZZ', '025K0ZZ', '025K3ZZ', '025K4ZZ', '025L0ZZ', 
              '025L3ZZ', '025L4ZZ', '025M0ZZ', '025M3ZZ', '025M4ZZ', '025N0ZZ', 
              '025N3ZZ', '025N4ZZ', '0270046', '027004Z', '02700D6', '02700DZ',
              '02700T6', '02700TZ', '02700Z6', '02700ZZ', '0270346', '027034Z', 
              '02703D6', '02703DZ', '02703T6', '02703TZ', '02703Z6', '02703ZZ',
              '0270446', '027044Z', '02704D6', '02704DZ', '02704T6', '02704TZ',
              '02704Z6', '02704ZZ', '0271046', '027104Z', '02710D6', '02710DZ',
              '02710T6', '02710TZ', '02710Z6', '02710ZZ', '0271346', '027134Z',
              '02713D6', '02713DZ', '02713T6', '02713TZ', '02713Z6', '02713ZZ',
              '0271446', '027144Z', '02714D6', '02714DZ', '02714T6', '02714TZ',
              '02714Z6', '02714ZZ', '0272046', '027204Z', '02720D6', '02720DZ',
              '02720T6', '02720TZ', '02720Z6', '02720ZZ', '0272346', '027234Z', 
              '02723D6', '02723DZ', '02723T6', '02723TZ', '02723Z6', '02723ZZ',
              '0272446', '027244Z', '02724D6', '02724DZ', '02724T6', '02724TZ',
              '02724Z6', '02724ZZ', '0273046', '027304Z', '02730D6', '02730DZ',
              '02730T6', '02730TZ', '02730Z6', '02730ZZ', '0273346', '027334Z',
              '02733D6', '02733DZ', '02733T6', '02733TZ', '02733Z6', '02733ZZ', 
              '0273446', '027344Z', '02734D6', '02734DZ', '02734T6', '02734TZ',
              '02734Z6', '02734ZZ', '027F04Z', '027F0DZ', '027F0ZZ', '027F34Z',
              '027F3DZ', '027F3ZZ', '027F44Z', '027F4DZ', '027F4ZZ', '027G04Z',
              '027G0DZ', '027G0ZZ', '027G34Z', '027G3DZ', '027G3ZZ', '027G44Z', 
              '027G4DZ', '027G4ZZ', '027H04Z', '027H0DZ', '027H0ZZ', '027H34Z',
              '027H3DZ', '027H3ZZ', '027H44Z', '027H4DZ', '027H4ZZ', '027J04Z',
              '027J0DZ', '027J0ZZ', '027J34Z', '027J3DZ', '027J3ZZ', '027J44Z',
              '027J4DZ', '027J4ZZ', '02890ZZ', '02893ZZ', '02894ZZ', '028D0ZZ',
              '028D3ZZ', '028D4ZZ', '02B40ZZ', '02B43ZZ', '02B44ZZ', '02B50ZZ',
              '02B53ZZ', '02B54ZZ', '02B60ZZ', '02B63ZZ', '02B64ZZ', '02B70ZK',
              '02B70ZZ', '02B73ZK', '02B73ZZ', '02B74ZK', '02B74ZZ', '02B80ZZ',
              '02B83ZZ', '02B84ZZ', '02B90ZZ', '02B93ZZ', '02B94ZZ', '02BD0ZZ',
              '02BD3ZZ', '02BD4ZZ', '02BF0ZZ', '02BF3ZZ', '02BF4ZZ', '02BG0ZZ',
              '02BG3ZZ', '02BG4ZZ', '02BH0ZZ', '02BH3ZZ', '02BH4ZZ', '02BJ0ZZ',
              '02BJ3ZZ', '02BJ4ZZ', '02BK0ZZ', '02BK3ZZ', '02BK4ZZ', '02BL0ZZ',
              '02BL3ZZ', '02BL4ZZ', '02BM0ZZ', '02BM3ZZ', '02BM4ZZ', '02BN0ZZ', 
              '02BN3ZZ', '02BN4ZZ', '02C00ZZ', '02C03ZZ', '02C04ZZ', '02C10ZZ',
              '02C13ZZ', '02C14ZZ', '02C20ZZ', '02C23ZZ', '02C24ZZ', '02C30ZZ',
              '02C33ZZ', '02C34ZZ', '02C40ZZ', '02C43ZZ', '02C44ZZ', '02C50ZZ',
              '02C53ZZ', '02C54ZZ', '02CD0ZZ', '02CD3ZZ', '02CD4ZZ', '02CF0ZZ',
              '02CF3ZZ', '02CF4ZZ', '02CG0ZZ', '02CG3ZZ', '02CG4ZZ', '02CH0ZZ', 
              '02CH3ZZ', '02CH4ZZ', '02CJ0ZZ', '02CJ3ZZ', '02CJ4ZZ', '02CM0ZZ', 
              '02CM3ZZ', '02CM4ZZ', '02H400Z', '02H402Z', '02H403Z', '02H40DZ',
              '02H40JZ', '02H40KZ', '02H40MZ', '02H430Z', '02H432Z', '02H433Z',
              '02H43DZ', '02H43JZ', '02H43KZ', '02H43MZ', '02H440Z', '02H442Z', 
              '02H443Z', '02H44DZ', '02H44JZ', '02H44KZ', '02H44MZ', '02H600Z',
              '02H60JZ', '02H60KZ', '02H60MZ', '02H630Z', '02H63JZ', '02H63KZ', 
              '02H63MZ', '02H640Z', '02H64JZ', '02H64KZ', '02H64MZ', '02H700Z',
              '02H70JZ', '02H70KZ', '02H70MZ', '02H730Z', '02H73JZ', '02H73KZ', 
              '02H73MZ', '02H740Z', '02H74JZ', '02H74KZ', '02H74MZ', '02HA0QZ',
              '02HA0RS', '02HA0RZ', '02HA3QZ', '02HA3RS', '02HA3RZ', '02HA4QZ',
              '02HA4RS', '02HA4RZ', '02HK00Z', '02HK02Z', '02HK0JZ', '02HK0KZ',
              '02HK0MZ', '02HK30Z', '02HK32Z', '02HK3JZ', '02HK3KZ', '02HK3MZ',
              '02HK40Z', '02HK42Z', '02HK4JZ', '02HK4KZ', '02HK4MZ', '02HL00Z',
              '02HL0JZ', '02HL0KZ', '02HL0MZ', '02HL30Z', '02HL3JZ', '02HL3KZ',
              '02HL3MZ', '02HL40Z', '02HL4JZ', '02HL4KZ', '02HL4MZ', '02HN0JZ',
              '02HN0KZ', '02HN0MZ', '02HN3JZ', '02HN3KZ', '02HN3MZ', '02HN4JZ',
              '02HN4KZ', '02HN4MZ', '02HS00Z', '02HS30Z', '02HS40Z', '02HT00Z', 
              '02HT30Z', '02HT40Z', '02HV00Z', '02HV30Z', '02HV40Z', '02L70CK',
              '02L70DK', '02L70ZK', '02L73CK', '02L73DK', '02L73ZK', '02L74CK',
              '02L74DK', '02L74ZK', '02LR0ZT', '02LS0ZZ', '02LT0ZZ', '02N50ZZ',
              '02N53ZZ', '02N54ZZ', '02N90ZZ', '02N93ZZ', '02N94ZZ', '02ND0ZZ',
              '02ND3ZZ', '02ND4ZZ', '02NF0ZZ', '02NF3ZZ', '02NF4ZZ', '02NG0ZZ',
              '02NG3ZZ', '02NG4ZZ', '02NH0ZZ', '02NH3ZZ', '02NH4ZZ', '02NJ0ZZ',
              '02NJ3ZZ', '02NJ4ZZ', '02NK0ZZ', '02NK3ZZ', '02NK4ZZ', '02NL0ZZ',
              '02NL3ZZ', '02NL4ZZ', '02NM0ZZ', '02NM3ZZ', '02NM4ZZ', '02PA0MZ',
              '02PA0QZ', '02PA0RZ', '02PA3MZ', '02PA3QZ', '02PA3RZ', '02PA4MZ',
              '02PA4QZ', '02PA4RZ', '02PAXMZ', '02Q00ZZ', '02Q03ZZ', '02Q04ZZ',
              '02Q10ZZ', '02Q13ZZ', '02Q14ZZ', '02Q20ZZ', '02Q23ZZ', '02Q24ZZ',
              '02Q30ZZ', '02Q33ZZ', '02Q34ZZ', '02Q40ZZ', '02Q43ZZ', '02Q44ZZ',
              '02Q50ZZ', '02Q53ZZ', '02Q54ZZ', '02Q70ZZ', '02Q73ZZ', '02Q74ZZ',
              '02Q90ZZ', '02Q93ZZ', '02Q94ZZ', '02QA0ZZ', '02QA3ZZ', '02QA4ZZ',
              '02QB0ZZ', '02QB3ZZ', '02QB4ZZ', '02QC0ZZ', '02QC3ZZ', '02QC4ZZ',
              '02QD0ZZ', '02QD3ZZ', '02QD4ZZ', '02QF0ZZ', '02QF3ZZ', '02QF4ZZ',
              '02QG0ZZ', '02QG3ZZ', '02QG4ZZ', '02QH0ZZ', '02QH3ZZ', '02QH4ZZ',
              '02QJ0ZZ', '02QJ3ZZ', '02QJ4ZZ', '02QM0ZZ', '02QM3ZZ', '02QM4ZZ',
              '02R907Z', '02R908Z', '02R90JZ', '02R90KZ', '02R947Z', '02R948Z',
              '02R94JZ', '02R94KZ', '02RD07Z', '02RD08Z', '02RD0JZ', '02RD0KZ',
              '02RD47Z', '02RD48Z', '02RD4JZ', '02RD4KZ', '02RF07Z', '02RF08Z', 
              '02RF0JZ', '02RF0KZ', '02RF37H', '02RF37Z', '02RF38H', '02RF38Z',
              '02RF3JH', '02RF3JZ', '02RF3KH', '02RF3KZ', '02RF47Z', '02RF48Z',
              '02RF4JZ', '02RF4KZ', '02RG07Z', '02RG08Z', '02RG0JZ', '02RG0KZ',
              '02RG37H', '02RG37Z', '02RG38H', '02RG38Z', '02RG3JH', '02RG3JZ',
              '02RG3KH', '02RG3KZ', '02RG47Z', '02RG48Z', '02RG4JZ', '02RG4KZ',
              '02RH07Z', '02RH08Z', '02RH0JZ', '02RH0KZ', '02RH37H', '02RH37Z',
              '02RH38H', '02RH38Z', '02RH3JH', '02RH3JZ', '02RH3KH', '02RH3KZ', 
              '02RH47Z', '02RH48Z', '02RH4JZ', '02RH4KZ', '02RJ07Z', '02RJ08Z',
              '02RJ0JZ', '02RJ0KZ', '02RJ47Z', '02RJ48Z', '02RJ4JZ', '02RJ4KZ',
              '02RK07Z', '02RK0JZ', '02RK0KZ', '02RK47Z', '02RK4KZ', '02RL07Z',
              '02RL0JZ', '02RL0KZ', '02RL47Z', '02RL4KZ', '02RM07Z', '02RM0JZ',
              '02RM0KZ', '02RM47Z', '02RM4JZ', '02RM4KZ', '02RP0JZ', '02RQ07Z',
              '02RQ0JZ', '02RR07Z', '02RR0JZ', '02SP0ZZ', '02SW0ZZ', '02T50ZZ',
              '02T53ZZ', '02T54ZZ', '02T80ZZ', '02T83ZZ', '02T84ZZ', '02T90ZZ',
              '02T93ZZ', '02T94ZZ', '02TD0ZZ', '02TD3ZZ', '02TD4ZZ', '02TH0ZZ',
              '02TH3ZZ', '02TH4ZZ', '02TM0ZZ', '02TM3ZZ', '02TM4ZZ', '02TN0ZZ',
              '02TN3ZZ', '02TN4ZZ', '02U507Z', '02U508Z', '02U50JZ', '02U50KZ',
              '02U537Z', '02U538Z', '02U53JZ', '02U53KZ', '02U547Z', '02U548Z',
              '02U54JZ', '02U54KZ', '02U607Z', '02U608Z', '02U60KZ', '02U707Z',
              '02U708Z', '02U70JZ', '02U70KZ', '02U737Z', '02U738Z', '02U73KZ',
              '02U747Z', '02U748Z', '02U74KZ', '02U907Z', '02U908Z', '02U90JZ', 
              '02U90KZ', '02U937Z', '02U938Z', '02U93JZ', '02U93KZ', '02U947Z',
              '02U948Z', '02U94JZ', '02U94KZ', '02UA0JZ', '02UA3JZ', '02UA4JZ',
              '02UD07Z', '02UD08Z', '02UD0JZ', '02UD0KZ', '02UD37Z', '02UD38Z',
              '02UD3JZ', '02UD3KZ', '02UD47Z', '02UD48Z', '02UD4JZ', '02UD4KZ',
              '02UF07Z', '02UF08Z', '02UF0JZ', '02UF0KZ', '02UF37Z', '02UF38Z',
              '02UF3JZ', '02UF3KZ', '02UF47Z', '02UF48Z', '02UF4JZ', '02UF4KZ', 
              '02UG07Z', '02UG08Z', '02UG0JZ', '02UG0KZ', '02UG37Z', '02UG38Z', 
              '02UG3JZ', '02UG3KZ', '02UG47Z', '02UG48Z', '02UG4JZ', '02UG4KZ',
              '02UH07Z', '02UH08Z', '02UH0JZ', '02UH0KZ', '02UH37Z', '02UH38Z',
              '02UH3JZ', '02UH3KZ', '02UH47Z', '02UH48Z', '02UH4JZ', '02UH4KZ',
              '02UJ07Z', '02UJ08Z', '02UJ0JZ', '02UJ0KZ', '02UJ37Z', '02UJ38Z',
              '02UJ3JZ', '02UJ3KZ', '02UJ47Z', '02UJ48Z', '02UJ4JZ', '02UJ4KZ',
              '02UK0KZ', '02UK3KZ', '02UK4KZ', '02UL0KZ', '02UL3KZ', '02UL4KZ', 
              '02UM07Z', '02UM0JZ', '02UM0KZ', '02UM37Z', '02UM38Z', '02UM3JZ',
              '02UM3KZ', '02UM47Z', '02UM48Z', '02UM4JZ', '02UM4KZ', '02VR0ZT',
              '02W50JZ', '02W54JZ', '02WA0JZ', '02WA0MZ', '02WA0QZ', '02WA0RZ',
              '02WA3MZ', '02WA3QZ', '02WA3RZ', '02WA4MZ', '02WA4QZ', '02WA4RZ',
              '02WF07Z', '02WF08Z', '02WF0JZ', '02WF0KZ', '02WF47Z', '02WF48Z',
              '02WF4JZ', '02WF4KZ', '02WG07Z', '02WG08Z', '02WG0JZ', '02WG0KZ',
              '02WG47Z', '02WG48Z', '02WG4JZ', '02WG4KZ', '02WH07Z', '02WH08Z',
              '02WH0JZ', '02WH0KZ', '02WH47Z', '02WH48Z', '02WH4JZ', '02WH4KZ',
              '02WJ07Z', '02WJ08Z', '02WJ0JZ', '02WJ0KZ', '02WJ47Z', '02WJ48Z',
              '02WJ4JZ', '02WJ4KZ', '02WM0JZ', '02WM4JZ', '02YA0Z0', '02YA0Z1',
              '02YA0Z2', '0JH600Z', '0JH604Z', '0JH605Z', '0JH606Z', '0JH607Z',
              '0JH608Z', '0JH609Z', '0JH60AZ', '0JH60PZ', '0JH630Z', '0JH634Z',
              '0JH635Z', '0JH636Z', '0JH637Z', '0JH638Z', '0JH639Z', '0JH63AZ',
              '0JH63PZ', '0JH800Z', '0JH804Z', '0JH805Z', '0JH806Z', '0JH807Z',
              '0JH808Z', '0JH809Z', '0JH80AZ', '0JH80PZ', '0JH830Z', '0JH834Z',
              '0JH835Z', '0JH836Z', '0JH837Z', '0JH838Z', '0JH839Z', '0JH83AZ',
              '0JH83PZ', '0JPT0PZ', '0JPT3PZ', '0JWT0PZ', '0JWT3PZ', '3E07017',
              '3E070PZ', '3E07317', '3E073PZ', '5A02110', '5A02116', '5A02210',
              '5A02216', '5A1213Z', '5A1223Z')
    
    
    keep08 <- data$dx1 %in% c('I0981', 'I110', 'I130', 'I132', 'I501', 'I5020', 
                              'I5021', 'I5022', 'I5023', '15030', 'I5031', 
                              'I5032', 'I5033', 'I5040', 'I5041', 'I5042',
                              'I5043', 'I509') & 
      (!data$px1 %in% exc8 & !data$px2 %in% exc8 & !data$px3 %in% exc8 & 
         !data$px4 %in% exc8 & !data$px5 %in% exc8 & !data$px6 %in% exc8 & 
         !data$px7 %in% exc8 & !data$px8 %in% exc8 & !data$px9 %in% exc8 & 
         !data$px10 %in% exc8) & data$age >= 18
    
    PQI.08 <- ifelse(keep08,1,0)
    
  } else {
    PQI.08 <- NULL
  }
  ################################################
  #PQI 09 : Low Birth Weight Rate 
  ################################################
  if ("PQI.09" %in% pqi.match & ICD == "9") {
  ##Definition - In-hospital live birth##
  birth <- c('V3000', 'V3001', 'V3100', 'V3101', 'V3200', 'V3201', 'V3300', 
             'V3301', 'V3400', 'V3401', 'V3500', 'V3501', 'V3600', 'V3601',
             'V3700', 'V3701', 'V3900', 'V3901', 'V290', 'V291', 'V292', 
             'V293', 'V298',' V299')
  
  #Definition - out-of-hospital live birth##
  oof <- c('V301', "V302", 'V311', "V312", 'V321', 'V322', 'V331', 'V332', 
           'V341', 'V342', 'V351', 'V352', 'V361', 'V362', 'V371', 'V372',
           'V391', 'V392')
  
  inc9 <- c(as.character(c(76400:76408, 76410:76418, 76420:76428, 76511:76518,
                           76490:76498, 76500:76508, 76510)))
  keep2<- data$age==0 & (data$dx1 %in% birth | data$dx2 %in% birth | 
                         data$dx3 %in% birth | data$dx4 %in% birth | 
                         data$dx5 %in% birth | data$dx6 %in% birth |
                         data$dx7 %in% birth | data$dx8 %in% birth |
                         data$dx9 %in% birth | data$dx10 %in% birth) & 
    (!data$dx1 %in% oof & !data$dx2 %in% oof & !data$dx3 %in% oof &
     !data$dx4 %in% oof & !data$dx5 %in% oof & !data$dx6 %in% oof &
     !data$dx7 %in% oof & !data$dx8 %in% oof & !data$dx9 %in% oof &
     !data$dx10 %in% oof)
  
  keep09 <- keep2 & (data$dx1 %in% inc9 | data$dx2 %in% inc9 | data$dx3 %in% inc9 | 
                     data$dx4 %in% inc9 | data$dx5 %in% inc9 | data$dx6 %in% inc9 |
                     data$dx7 %in% inc9 | data$dx8 %in% inc9 | data$dx9 %in% inc9 | 
                     data$dx10 %in% inc9) 
  
  PQI.09 <- ifelse(keep09, 1, 0)
  } else if ("PQI.09" %in% pqi.match & ICD == "10"){
    ##Definition - In-hospital live birth##
    birth <- c('Z3800', 'Z3801', 'Z3830', 'Z3831', 'Z3861', 'Z3862', 'Z3863', 'Z3864',
               'Z3865', 'Z3866', 'Z3868', 'Z3869')
    
    #Definition - out-of-hospital live birth##
    oof <- c('Z381', 'Z384', 'Z387')
    
    inc9 <- c('P0501', 'P0502', 'P0503', 'P0504', 'P0505', 'P0506', 'P0507', 'P0508',
              'P0511', 'P0512', 'P0513', 'P0514', 'P0515', 'P0516', 'P0517', 'P0518',
              'P0700', 'P0701', 'P0702', 'P0703', 'P0710', 'P0714', 'P0715', 'P0716',
              'P0717', 'P0718')
    
    keep2<- data$age==0 & (data$dx1 %in% birth | data$dx2 %in% birth | 
                             data$dx3 %in% birth | data$dx4 %in% birth | 
                             data$dx5 %in% birth | data$dx6 %in% birth |
                             data$dx7 %in% birth | data$dx8 %in% birth |
                             data$dx9 %in% birth | data$dx10 %in% birth) & 
      (!data$dx1 %in% oof & !data$dx2 %in% oof & !data$dx3 %in% oof &
         !data$dx4 %in% oof & !data$dx5 %in% oof & !data$dx6 %in% oof &
         !data$dx7 %in% oof & !data$dx8 %in% oof & !data$dx9 %in% oof &
         !data$dx10 %in% oof)
    
    keep09 <- keep2 & (data$dx1 %in% inc9 | data$dx2 %in% inc9 | data$dx3 %in% inc9 | 
                         data$dx4 %in% inc9 | data$dx5 %in% inc9 | data$dx6 %in% inc9 |
                         data$dx7 %in% inc9 | data$dx8 %in% inc9 | data$dx9 %in% inc9 | 
                         data$dx10 %in% inc9)
    PQI.09 <- ifelse(keep09, 1, 0)
    
  } else {
    PQI.09 <- NULL
  }
  ################################################
  #PQI 10 : Dehydration Admission Rate 
  ################################################
  if ("PQI.10" %in% pqi.match & ICD == "9") {
  ##Include - Dehydration##
  
  inc10  <- c('2765', '27650', '27651', '27652')
  
  ##Include - Principal diagnosis for hyperosmolality 
  ##and/or hypernatremia, gastroenteritis, or acute kidney injury
  ##with secondary diagnosis for dehydration
  
  inc10.2 <- c('2760', '00861', '00862', '00863', '00864', '00865', '00866',
               '00867', '00869', '0088', '0090', '0091', '0092', '0093', 
               '5589', '5845', '5846', '5847', '5848', '5849', '586', '9975')
  
  ##Exclusions - Chronic renal failure##
  
  exc10 <- c('40301', '40311', '40391', '40402', '40403', '40412', '40413',
             '40492', '40493', '5855', '5856')
  
  A <- (data$dx1 %in% inc10)
  
  B <- (data$dx1 %in% inc10.2 & 
          (data$dx2 %in% inc10 | data$dx3 %in% inc10 | data$dx4 %in% inc10 |
             data$dx5 %in% inc10 | data$dx6 %in% inc10 | data$dx7 %in% inc10 | 
             data$dx8 %in% inc10 | data$dx9 %in% inc10 | data$dx10 %in% inc10))
  
  C <- (!data$dx1 %in% exc10 & !data$dx2 %in% exc10 & !data$dx3 %in% exc10 & 
          !data$dx4 %in% exc10 & !data$dx5 %in% exc10 & !data$dx6 %in% exc10 &
          !data$dx7 %in% exc10 & !data$dx8 %in% exc10 & !data$dx9 %in% exc10 &
          !data$dx10 %in% exc10)
  
  keep10 <- (A | B) & C & data$age >= 18
  
  PQI.10 <- ifelse(keep10, 1, 0)
  } else if("PQI.10" %in% pqi.match & ICD == "10"){
    ##Include - Dehydration##
    
    inc10  <- c('E860', 'E861', 'E869')
    
    ##Include - Principal diagnosis for hyperosmolality 
    ##and/or hypernatremia, gastroenteritis, or acute kidney injury
    ##with secondary diagnosis for dehydration
    
    inc10.2 <- c('E870', 'A080', 'A0811', 'A0819', 'A082', 'A0831', 'A0832',
                 'A0839', 'A084', 'A088', 'A09', 'K5289', 'K529', 'N170', 'N171',
                 'N172', 'N178', 'N179', 'N19', 'N990')
    
    ##Exclusions - Chronic renal failure##
    exc10 <- c('I120', 'I1311', 'I132', 'N185', 'N186')
    
    A <- (data$dx1 %in% inc10)
    
    B <- (data$dx1 %in% inc10.2 & 
            (data$dx2 %in% inc10 | data$dx3 %in% inc10 | data$dx4 %in% inc10 |
               data$dx5 %in% inc10 | data$dx6 %in% inc10 | data$dx7 %in% inc10 | 
               data$dx8 %in% inc10 | data$dx9 %in% inc10 | data$dx10 %in% inc10))
    
    C <- (!data$dx1 %in% exc10 & !data$dx2 %in% exc10 & !data$dx3 %in% exc10 & 
            !data$dx4 %in% exc10 & !data$dx5 %in% exc10 & !data$dx6 %in% exc10 &
            !data$dx7 %in% exc10 & !data$dx8 %in% exc10 & !data$dx9 %in% exc10 &
            !data$dx10 %in% exc10)
    
    keep10 <- (A | B) & C & data$age >= 18
    
    PQI.10 <- ifelse(keep10, 1, 0)
    
  } else {
    PQI.10 <- NULL
  }
  ################################################
  #PQI 11 : Bacterial Pneumonia Admission Rate
  ################################################
  if ("PQI.11" %in% pqi.match & ICD == "9") {
  ##Exclusions - Sickle Cell Anemia##
  
  exc11  <- c('28241', '28242', '28260', '28261', '28262', '28263', '28264',
              '28268', '28269') 
  
  ##Exclusions - immunocompromised state diagnosis codes##
  
  exc11.2 <- c('042', '1363', '1992', '23873', '23876', '23877', '23879', 
               '260', '261', '262', '27900', '27901', '27902', '27903', 
               '27904', '27905', '27906', '27909', '27910', '27911', '27912',
               '27913', '27919', '2792', '2793', '2794', '27941', '27949', 
               '27950', '27951', '27952', '27953', '2798', '2799', '28409', 
               '2841', '28411', '28412', '28419', '2880', '28800', '28801',
               '28802', '28803', '28809', '2881', '2882', '2884', '28850', 
               '28851', '28859', '28953', '28983', '40301', '40311', '40391',
               '40402', '40403', '40412', '40413', '40492', '40493', '5793',
               '585', '5855', '5856', '9968', '99680', '99681', '99682', 
               '99683', '99684', '99685', '99686', '99687', '99688', '99689',
               'V420', 'V421', 'V426', 'V427', 'V428', 'V4281', 'V4282', 
               'V4283', 'V4284', 'V4289', 'V451', 'V4511', 'V560', 'V561', 
               'V562')
  
  ##Exclusions - immunocompromised state procedure codes##
  
  exc11.3 <- c('0018', '335', '336', '375', '3350', '3751', '3351', '410', 
               '3352', '4100', '5051', '4101', '4102', '5059', '5280', '4103',
               '5281', '4104', '4105', '4106', '5282', '5283', '5285', '4107', 
               '5286', '4108', '5569', '4109', '4697')#add 4697
  
  keep11 <- data$dx1 %in% c('481', '4822', '48230', '48231', '48232', '48239',
                            '48241', '48242', '4829', '4830', '4831', '4838',
                            '485', '486', '48249') & #add 48249
    (!data$dx1 %in% exc11 & !data$dx2 %in% exc11 & !data$dx3 %in% exc11 & 
       !data$dx4 %in% exc11 & !data$dx5 %in% exc11 & !data$dx6 %in% exc11 & 
       !data$dx7 %in% exc11 & !data$dx8 %in% exc11 & !data$dx9 %in% exc11 & 
       !data$dx10 %in% exc11) & 
    (!data$dx1 %in% exc11.2 & !data$dx2 %in% exc11.2 & !data$dx3 %in% exc11.2 & 
       !data$dx4 %in% exc11.2 & !data$dx5 %in% exc11.2 & !data$dx6 %in% exc11.2 &
       !data$dx7 %in% exc11.2 & !data$dx8 %in% exc11.2 & !data$dx9 %in% exc11.2 &
       !data$dx10 %in% exc11.2 ) & 
    (!data$px1 %in% exc11.3 & !data$px2 %in% exc11.3 & !data$px3 %in% exc11.3 &
       !data$px4 %in% exc11.3 & !data$px5 %in% exc11.3 & !data$px6 %in% exc11.3 &
       !data$px7 %in% exc11.3 & !data$px8 %in% exc11.3 & !data$px9 %in% exc11.3 &
       !data$px10 %in% exc11.3) & data$age >= 18
  
  PQI.11 <- ifelse(keep11, 1, 0)
  
  } else if ("PQI.11" %in% pqi.match & ICD == "10"){
    ##Exclusions - Sickle Cell Anemia##
    
    exc11  <- c('D5700', 'D5701', 'D5702', 'D571', 'D5720', 'D57211', 'D57212',
                'D57219', 'D5740', 'D57411', 'D57412', 'D57419', 'D5780', 'D57811',
                'D57812', 'D57819')
    
    ##Exclusions - immunocompromised state diagnosis codes##
    exc11.2 <- c('B20', 'B59', 'C802', 'C888', 'C9440', 'C9441', 'C9442', 'C946',
                 'D4622', 'D471', 'D479', 'D47Z1', 'D47Z9', 'D6109', 'D61810',
                 'D61811', 'D61818', 'D700', 'D701', 'D702', 'D704', 'D708', 'D709',
                 'D71', 'D720', 'D72810', 'D72818', 'D72819', 'D7381', 'D7581', 
                 'D761', 'D762', 'D763', 'D800', 'D801', 'D802', 'D803', 'D804',
                 'D805', 'D806', 'D807', 'D808', 'D809', 'D810', 'D811', 'D812',
                 'D814', 'D816', 'D817', 'D8189', 'D819', 'D820', 'D821', 'D822',
                 'D823', 'D824', 'D828', 'D829', 'D830', 'D831', 'D832', 'D838',
                 'D839', 'D840', 'D841', 'D848', 'D849', 'D893', 'D89810', 'D89811',
                 'D89812', 'D89813', 'D8982', 'D8989', 'D899', 'E40', 'E41', 'E42',
                 'E43', 'I120', 'I1311', 'I132', 'K912', 'M359', 'N185', 'N186', 
                 'T8600', 'T8601', 'T8602', 'T8603', 'T8609', 'T8610', 'T8611',
                 'T8612', 'T8613', 'T8619', 'T8620', 'T8621', 'T8622', 'T8623',
                 'T86290', 'T86298', 'T8630', 'T8631', 'T8632', 'T8633', 'T8639',
                 'T8640', 'T8641', 'T8642', 'T8643', 'T8649', 'T865', 'T86810',
                 'T86811', 'T86812', 'T86818', 'T86819', 'T86830', 'T86831',
                 'T86832', 'T86838', 'T86839', 'T86850', 'T86851', 'T86852',
                 'T86858', 'T86859', 'T86890', 'T86891', 'T86892', 'T86898',
                 'T86899', 'T8690', 'T8691', 'T8692', 'T8693', 'T8699', 'Z4821',
                 'Z4822', 'Z4823', 'Z4824', 'Z48280', 'Z48290', 'Z48298', 'Z4901',
                 'Z4902', 'Z4931', 'Z940', 'Z941', 'Z942', 'Z943', 'Z944', 'Z9481',
                 'Z9482', 'Z9483', 'Z9484', 'Z9489', 'Z992')
    
    ##Exclusions - immunocompromised state procedure codes##
    
    exc11.3 <- c('02YA0Z0', '02YA0Z2', '0BYC0Z0', '0BYC0Z2', '0BYD0Z0', '0BYD0Z2',
                 '0BYF0Z0', '0BYF0Z2', '0BYG0Z0', '0BYG0Z2', '0BYH0Z0', '0BYH0Z2',
                 '0BYJ0Z0', '0BYJ0Z2', '0BYK0Z0', '0BYK0Z2', '0BYL0Z0', '0BYL0Z2',
                 '0BYM0Z0', '0BYM0Z2', '0FSG0ZZ', '0FSG4ZZ', '0FY00Z0', '0FY00Z2',
                 '0FYG0Z0', '0FYG0Z2', '0TY00Z0', '0TY00Z2', '0TY10Z0', '0TY10Z2',
                 '30230AZ', '30230G0', '30230G1', '30230X0', '30230X1', '30230Y0',
                 '30230Y1', '30233AZ', '30233G0', '30233G1', '30233X0', '30233X1',
                 '30233Y0', '30233Y1', '30240AZ', '30240G0', '30240G1', '30240X0',
                 '30240X1', '30240Y0', '30240Y1', '30243AZ', '30243G0', '30243G1',
                 '30243X0', '30243X1', '30243Y0', '30243Y1', '30250G0', '30250G1',
                 '30250X0', '30250X1', '30250Y0', '30250Y1', '30253G0', '30253G1',
                 '30253X0', '30253X1', '30253Y0', '30253Y1', '30260G0', '30260G1',
                 '30260X0', '30260X1', '30260Y0', '30260Y1', '30263G0', '30263G1',
                 '30263X0', '30263X1', '30263Y0', '30263Y1', '3E03005', '3E0300M',
                 '3E0J3U1', '3E0J7U1', '3E0J8U1', '3E030U1', '3E030WL', '3E03305',
                 '3E0330M', '3E033U1', '3E033WL', '3E04005', '3E0400M', '3E040WL',
                 '3E04305', '3E0430M', '3E043WL', '3E05005', '3E0500M', '3E050WL', 
                 '3E05305', '3E0530M', '3E053WL', '3E06005', '3E0600M', '3E060WL',
                 '3E06305', '3E0630M', '3E063WL')
    
    keep11 <- data$dx1 %in% c('J13', 'J14', 'J15211', 'J15212', 'J153', 'J154',
                              'J157', 'J159', 'J160', 'J168', 'J180', 'J181', 
                              'J188', 'J189') &
      (!data$dx1 %in% exc11 & !data$dx2 %in% exc11 & !data$dx3 %in% exc11 & 
         !data$dx4 %in% exc11 & !data$dx5 %in% exc11 & !data$dx6 %in% exc11 & 
         !data$dx7 %in% exc11 & !data$dx8 %in% exc11 & !data$dx9 %in% exc11 & 
         !data$dx10 %in% exc11) & 
      (!data$dx1 %in% exc11.2 & !data$dx2 %in% exc11.2 & !data$dx3 %in% exc11.2 & 
         !data$dx4 %in% exc11.2 & !data$dx5 %in% exc11.2 & !data$dx6 %in% exc11.2 &
         !data$dx7 %in% exc11.2 & !data$dx8 %in% exc11.2 & !data$dx9 %in% exc11.2 &
         !data$dx10 %in% exc11.2 ) & 
      (!data$px1 %in% exc11.3 & !data$px2 %in% exc11.3 & !data$px3 %in% exc11.3 &
         !data$px4 %in% exc11.3 & !data$px5 %in% exc11.3 & !data$px6 %in% exc11.3 &
         !data$px7 %in% exc11.3 & !data$px8 %in% exc11.3 & !data$px9 %in% exc11.3 &
         !data$px10 %in% exc11.3) & data$age >= 18
    
    PQI.11 <- ifelse(keep11, 1, 0)
    
  } else {
    PQI.11 <- NULL
  }
  ################################################
  #PQI 12 : Urinary Tract Infection 
  ################################################
  if ("PQI.12" %in% pqi.match & ICD == "9") {
  ##Exclusions - Kidney/urinary tract disorder ##
  
  exc12  <- c('59000', '59001', '59370', '59371', '59372', '59373', '7530', 
              '75310', '75311', '75312', '75313', '75314', '75315', '75316',
              '75317', '75319', '75320', '75321', '75322', '75323', '75329',
              '7533', '7534', '7535', '7536', '7538', '7539') 
  
  ##Exclusions - immunocompromised state diagnosis codes##
  
  exc12.2 <- c('042', '1363', '1992', '23873', '23876', '23877', '23879', 
               '260','261', '262', '27900', '27901', '27902', '27903', 
               '27904', '27905', '27906', '27909', '27910', '27911', '27912',
               '27913', '27919', '2792', '2793', '2794', '27941', '27949', 
               '27950', '27951', '27952', '27953', '2798', '2799', '28409', 
               '2841', '28411', '28412', '28419', '2880', '28800', '28801', 
               '28802', '28803', '28809', '2881', '2882', '2884', '28850', 
               '28851', '28859','28953', '28983', '40301', '40311', '40391', 
               '40402', '40403', '40412', '40413', '40492', '40493', '5793', 
               '585', '5855', '5856', '9968', '99680', '99681', '99682', 
               '99683', '99684', '99685','99686', '99687', '99688', '99689', 
               'V420', 'V421', 'V426', 'V427', 'V428', 'V4281', 'V4282', 
               'V4283', 'V4284', 'V4289', 'V451','V4511', 'V560', 'V561', 
               'V562')
  
  ##Exclusions - immunocompromised state procedure codes##
  
  exc12.3 <- c('0018', '335', '336', '375', '3350', '3751', '3351', '410', 
               '3352', '4100', '5051', '4101', '4102', '5059', '5280', '4103', 
               '5281', '4104', '4105', '4106', '5282', '5283', '5285', '4107', 
               '5286', '4108', '5569', '4109', '4697')#add 4697
  
  keep12 <- data$dx1 %in% c('59010', '59011', '5902', '5903', '59080', '59081', 
                            '5909', '5950', '5959', '5990') & 
    (!data$dx1 %in% exc12 & !data$dx2 %in% exc12 & !data$dx3 %in% exc12 & 
       !data$dx4 %in% exc12 & !data$dx5 %in% exc12 & !data$dx6 %in% exc12 & 
       !data$dx7 %in% exc12 & !data$dx8 %in% exc12 & !data$dx9 %in% exc12 & 
       !data$dx10 %in% exc12) & 
    (!data$dx1 %in% exc12.2 & !data$dx2 %in% exc12.2 & !data$dx3 %in% exc12.2 &
       !data$dx4 %in% exc12.2 & !data$dx5 %in% exc12.2 & !data$dx6 %in% exc12.2 &
       !data$dx7 %in% exc12.2 & !data$dx8 %in% exc12.2 & !data$dx9 %in% exc12.2 &
       !data$dx10 %in% exc12.2 ) & 
    (!data$px1 %in% exc12.3 & !data$px2 %in% exc12.3 & !data$px3 %in% exc12.3 &
       !data$px4 %in% exc12.3 & !data$px5 %in% exc12.3 & !data$px6 %in% exc12.3 &
       !data$px7 %in% exc12.3 & !data$px8 %in% exc12.3 & !data$px9 %in% exc12.3 &
       !data$px10 %in% exc12.3) & data$age >= 18
  
  PQI.12 <- ifelse(keep12, 1, 0)
  } else if ("PQI.12" %in% pqi.match & ICD == "10"){
    ##Exclusions - Kidney/urinary tract disorder ##
    exc12  <- c('N110', 'N111', 'N118', 'N119', 'N1370', 'N1371', 'N13721', 'N13722',
                'N13729', 'N13731', 'N13732', 'N13739', 'N139', 'Q600', 'Q601', 
                'Q602', 'Q603', 'Q604', 'Q605', 'Q606', 'Q6100', 'Q6101', 'Q6102',
                'Q6111', 'Q6119', 'Q612', 'Q613', 'Q614', 'Q615', 'Q618', 'Q619', 
                'Q620', 'Q6210', 'Q6211', 'Q6212', 'Q622', 'Q6231', 'Q6232', 'Q6239',
                'Q624', 'Q625', 'Q6260', 'Q6261', 'Q6262', 'Q6263', 'Q6269', 'Q627',
                'Q628', 'Q630', 'Q631', 'Q632', 'Q633', 'Q638', 'Q639', 'Q6410', 
                'Q6411', 'Q6412', 'Q6419', 'Q642', 'Q6431', 'Q6432', 'Q6433', 'Q6439',
                'Q645', 'Q646', 'Q6470', 'Q6471', 'Q6472', 'Q6473', 'Q6474', 'Q6475',
                'Q6479', 'Q648', 'Q649')
    
    ##Exclusions - immunocompromised state diagnosis codes##
    exc12.2 <- c('B20', 'B59', 'C802', 'C888', 'C9440', 'C9441', 'C9442', 'C946',
                 'D4622', 'D471', 'D479', 'D47Z1', 'D47Z9', 'D6109', 'D61810',
                 'D61811', 'D61818', 'D700', 'D701', 'D702', 'D704', 'D708', 'D709',
                 'D71', 'D720', 'D72810', 'D72818', 'D72819', 'D7381', 'D7581', 
                 'D761', 'D762', 'D763', 'D800', 'D801', 'D802', 'D803', 'D804',
                 'D805', 'D806', 'D807', 'D808', 'D809', 'D810', 'D811', 'D812',
                 'D814', 'D816', 'D817', 'D8189', 'D819', 'D820', 'D821', 'D822',
                 'D823', 'D824', 'D828', 'D829', 'D830', 'D831', 'D832', 'D838',
                 'D839', 'D840', 'D841', 'D848', 'D849', 'D893', 'D89810', 'D89811',
                 'D89812', 'D89813', 'D8982', 'D8989', 'D899', 'E40', 'E41', 'E42',
                 'E43', 'I120', 'I1311', 'I132', 'K912', 'M359', 'N185', 'N186', 
                 'T8600', 'T8601', 'T8602', 'T8603', 'T8609', 'T8610', 'T8611',
                 'T8612', 'T8613', 'T8619', 'T8620', 'T8621', 'T8622', 'T8623',
                 'T86290', 'T86298', 'T8630', 'T8631', 'T8632', 'T8633', 'T8639',
                 'T8640', 'T8641', 'T8642', 'T8643', 'T8649', 'T865', 'T86810',
                 'T86811', 'T86812', 'T86818', 'T86819', 'T86830', 'T86831',
                 'T86832', 'T86838', 'T86839', 'T86850', 'T86851', 'T86852',
                 'T86858', 'T86859', 'T86890', 'T86891', 'T86892', 'T86898',
                 'T86899', 'T8690', 'T8691', 'T8692', 'T8693', 'T8699', 'Z4821',
                 'Z4822', 'Z4823', 'Z4824', 'Z48280', 'Z48290', 'Z48298', 'Z4901',
                 'Z4902', 'Z4931', 'Z940', 'Z941', 'Z942', 'Z943', 'Z944', 'Z9481',
                 'Z9482', 'Z9483', 'Z9484', 'Z9489', 'Z992')
    
    ##Exclusions - immunocompromised state procedure codes##
    
    exc12.3 <- c('02YA0Z0', '02YA0Z2', '0BYC0Z0', '0BYC0Z2', '0BYD0Z0', '0BYD0Z2',
                 '0BYF0Z0', '0BYF0Z2', '0BYG0Z0', '0BYG0Z2', '0BYH0Z0', '0BYH0Z2',
                 '0BYJ0Z0', '0BYJ0Z2', '0BYK0Z0', '0BYK0Z2', '0BYL0Z0', '0BYL0Z2',
                 '0BYM0Z0', '0BYM0Z2', '0FSG0ZZ', '0FSG4ZZ', '0FY00Z0', '0FY00Z2',
                 '0FYG0Z0', '0FYG0Z2', '0TY00Z0', '0TY00Z2', '0TY10Z0', '0TY10Z2',
                 '30230AZ', '30230G0', '30230G1', '30230X0', '30230X1', '30230Y0',
                 '30230Y1', '30233AZ', '30233G0', '30233G1', '30233X0', '30233X1',
                 '30233Y0', '30233Y1', '30240AZ', '30240G0', '30240G1', '30240X0',
                 '30240X1', '30240Y0', '30240Y1', '30243AZ', '30243G0', '30243G1',
                 '30243X0', '30243X1', '30243Y0', '30243Y1', '30250G0', '30250G1',
                 '30250X0', '30250X1', '30250Y0', '30250Y1', '30253G0', '30253G1',
                 '30253X0', '30253X1', '30253Y0', '30253Y1', '30260G0', '30260G1',
                 '30260X0', '30260X1', '30260Y0', '30260Y1', '30263G0', '30263G1',
                 '30263X0', '30263X1', '30263Y0', '30263Y1', '3E03005', '3E0300M',
                 '3E0J3U1', '3E0J7U1', '3E0J8U1', '3E030U1', '3E030WL', '3E03305',
                 '3E0330M', '3E033U1', '3E033WL', '3E04005', '3E0400M', '3E040WL',
                 '3E04305', '3E0430M', '3E043WL', '3E05005', '3E0500M', '3E050WL', 
                 '3E05305', '3E0530M', '3E053WL', '3E06005', '3E0600M', '3E060WL',
                 '3E06305', '3E0630M', '3E063WL')
    keep12 <- data$dx1 %in% c('N10', 'N119', 'N12', 'N151', 'N159', 'N16', 'N2884',
                              'N2885', 'N2886', 'N3000', 'N3001', 'N3090', 'N3091',
                              'N390') &
      (!data$dx1 %in% exc12 & !data$dx2 %in% exc12 & !data$dx3 %in% exc12 & 
         !data$dx4 %in% exc12 & !data$dx5 %in% exc12 & !data$dx6 %in% exc12 & 
         !data$dx7 %in% exc12 & !data$dx8 %in% exc12 & !data$dx9 %in% exc12 & 
         !data$dx10 %in% exc12) & 
      (!data$dx1 %in% exc12.2 & !data$dx2 %in% exc12.2 & !data$dx3 %in% exc12.2 &
         !data$dx4 %in% exc12.2 & !data$dx5 %in% exc12.2 & !data$dx6 %in% exc12.2 &
         !data$dx7 %in% exc12.2 & !data$dx8 %in% exc12.2 & !data$dx9 %in% exc12.2 &
         !data$dx10 %in% exc12.2 ) & 
      (!data$px1 %in% exc12.3 & !data$px2 %in% exc12.3 & !data$px3 %in% exc12.3 &
         !data$px4 %in% exc12.3 & !data$px5 %in% exc12.3 & !data$px6 %in% exc12.3 &
         !data$px7 %in% exc12.3 & !data$px8 %in% exc12.3 & !data$px9 %in% exc12.3 &
         !data$px10 %in% exc12.3) & data$age >= 18
    
    PQI.12 <- ifelse(keep12, 1, 0)
    
  } else {
    PQI.12 <- NULL
  }
  
  ##PQI 13 is retired##
  
  ################################################
  #PQI 14 : Uncontrolled Diabetes
  ################################################
  if ("PQI.14" %in% pqi.match & ICD == "9") {
    keep14 <- data$dx1 %in% c('25002', '25003') & data$age >= 18
    
    PQI.14 <- ifelse(keep14, 1, 0)
  
  } else if ("PQI.14" %in% pqi.match & ICD == "10") {
    keep14 <- data$dx1 %in% c('E1065', 'E1165', 'E10649', 'E11649') & data$age >= 18
    
    PQI.14 <- ifelse(keep14, 1, 0)
    
  } else {
    PQI.14 <- NULL
  }
  ################################################
  #PQI 15 : Asthma in Younger Adults
  ################################################
  if ("PQI.15" %in% pqi.match & ICD == "9") {
  ##Exclusions - Cystic fibrosis and anamolies of the respiratory system ##
  
  exc15 <- c('27700', '27701', '27702', '27703', '27709', '51661', '51662', 
             '51663', '51664', '51669', '74721', '7483', '7484', '7485', 
             '74860', '74861', '74869', '7488', '7489', '7503', '7593', '7707')
  
  keep15 <- data$dx1 %in% c('49300', '49301', '49302', '49310', '49311', 
                            '49312', '49320', '49321', '49322', '49381', 
                            '49382', '49390', '49391', '49392') & 
    (!data$dx1 %in% exc15 & !data$dx2 %in% exc15 & !data$dx3 %in% exc15 & 
       !data$dx4 %in% exc15 & !data$dx5 %in% exc15 & !data$dx6 %in% exc15 & 
       !data$dx7 %in% exc15 & !data$dx8 %in% exc15 & !data$dx9 %in% exc15 & 
       !data$dx10 %in% exc15) & data$age >= 18 & data$age <= 39
  
  PQI.15 <- ifelse(keep15, 1, 0)
  } else if ("PQI.15" %in% pqi.match & ICD == "10"){
    ##Exclusions - Cystic fibrosis and anamolies of the respiratory system ##
    
    exc15 <- c('E840', 'E8411', 'E8419', 'E848', 'E849', 'J8483', 'J84841', 'J84842',
               'J84843', 'J84848', 'P270', 'P271', 'P278', 'P279', 'Q254', 'Q311',
               'Q312', 'Q313', 'Q315', 'Q318', 'Q319', 'Q320', 'Q321', 'Q322', 'Q323',
               'Q324', 'Q330', 'Q331', 'Q332', 'Q333', 'Q334', 'Q335', 'Q336', 'Q338',
               'Q339', 'Q340', 'Q341', 'Q348', 'Q349', 'Q390', 'Q391', 'Q392', 'Q393',
               'Q394', 'Q893')
    
    keep15 <- data$dx1 %in% c('J4521', 'J4522', 'J4531', 'J4532', 'J4541', 'J4542',
                              'J4551', 'J4552', 'J45901', 'J45902', 'J45990', 
                              'J45991', 'J45998') &
      (!data$dx1 %in% exc15 & !data$dx2 %in% exc15 & !data$dx3 %in% exc15 & 
         !data$dx4 %in% exc15 & !data$dx5 %in% exc15 & !data$dx6 %in% exc15 & 
         !data$dx7 %in% exc15 & !data$dx8 %in% exc15 & !data$dx9 %in% exc15 & 
         !data$dx10 %in% exc15) & data$age >= 18 & data$age <= 39
    
    PQI.15 <- ifelse(keep15, 1, 0)
    
  } else {
    PQI.15 <- NULL
  }
  ################################################
  #PQI 16 : Lower Extremity Amputation with Diabetes
  ################################################
  if ("PQI.16" %in% pqi.match & ICD == "9") {
  #Include - Amputation##
  
  inc16 <- c('8410', '8412', '8413', '8414', '8415', '8416', '8417', '8418', '8419')
  
  ##Include Diabetes##
  
  inc16.2 <- c('25000', '25001', '25002', '25003', '25010', '25011', '25012', 
               '25013', '25020', '25021', '25022', '25023', '25030', '25031',
               '25032', '25033', '25040', '25041', '25042', '25043', '25050',
               '25051', '25052', '25053', '25060', '25061', '25062', '25063',
               '25070', '25071', '25072', '25073', '25080', '25081', '25082',
               '25083', '25090', '25091', '25092', '25093')
  #exclusions
  exc16 <- c('8950', '8951', '8960', '8961', '8962', '8963', '8970', '8971', 
             '8972', '8973', '8974', '8975', '8976', '8977')
  
  keep16 <- !data$dx1 %in% as.character(c(765:782,998)) & #MDC 14 exclusion
    ((data$px1 %in% inc16 | data$px2 %in% inc16 | data$px3 %in% inc16 |
      data$px4 %in% inc16 | data$px5 %in% inc16 | data$px6 %in% inc16 |
      data$px7 %in% inc16 | data$px8 %in% inc16 | data$px9 %in% inc16 |
      data$px10 %in% inc16) & 
       (data$dx1 %in% inc16.2 | data$dx2 %in% inc16.2 | 
        data$dx3 %in% inc16.2 | data$dx4 %in% inc16.2 | 
        data$dx5 %in% inc16.2 | data$dx6 %in% inc16.2 |
        data$dx7 %in% inc16.2 | data$dx8 %in% inc16.2 | 
        data$dx9 %in% inc16.2 | data$dx10 %in% inc16.2)) &
    !data$dx1 %in% exc16 & !data$dx2 %in% exc16 & !data$dx3 %in% exc16 &
    !data$dx4 %in% exc16 & !data$dx5 %in% exc16 & !data$dx6 %in% exc16 &
    !data$dx7 %in% exc16 & !data$dx8 %in% exc16 & !data$dx9 %in% exc16 &
    !data$dx10 %in% exc16 & data$age >= 18
  
  PQI.16 <- ifelse(keep16, 1, 0)
  
  } else  if ("PQI.16" %in% pqi.match & ICD == "10"){
    #Include - Amputation##
    
    inc16 <- c('0Y620ZZ', '0Y630ZZ', '0Y640ZZ', '0Y670ZZ', '0Y680ZZ', '0Y6C0Z1',
               '0Y6C0Z2', '0Y6C0Z3', '0Y6D0Z1', '0Y6D0Z2', '0Y6D0Z3', '0Y6F0ZZ',
               '0Y6G0ZZ', '0Y6H0Z1', '0Y6H0Z2', '0Y6H0Z3', '0Y6J0Z1', '0Y6J0Z2',
               '0Y6J0Z3', '0Y6M0Z0', '0Y6M0Z4', '0Y6M0Z5', '0Y6M0Z6', '0Y6M0Z7',
               '0Y6M0Z8', '0Y6M0Z9', '0Y6M0ZB', '0Y6M0ZC', '0Y6M0ZD', '0Y6M0ZF',
               '0Y6N0Z0', '0Y6N0Z4', '0Y6N0Z5', '0Y6N0Z6', '0Y6N0Z7', '0Y6N0Z8',
               '0Y6N0Z9', '0Y6N0ZB', '0Y6N0ZC', '0Y6N0ZD', '0Y6N0ZF')
    
    ##Include Diabetes##
    
    inc16.2 <- c('E1010', 'E1011', 'E1021', 'E1022', 'E1029', 'E10311', 'E10319',
                 'E10321', 'E10329', 'E10331', 'E10339', 'E10341', 'E10349', 
                 'E10351', 'E10359', 'E1036', 'E1039', 'E1040', 'E1041', 'E1042',
                 'E1043', 'E1044', 'E1049', 'E1051', 'E1052', 'E1059', 'E10610',
                 'E10618', 'E10620', 'E10621', 'E10622', 'E10628', 'E10630', 
                 'E10638', 'E10641', 'E10649', 'E1065', 'E1069', 'E108', 'E109',
                 'E1100', 'E1101', 'E1121', 'E1122', 'E1129', 'E11311', 'E11319',
                 'E11321', 'E11329', 'E11331', 'E11339', 'E11341', 'E11349', 
                 'E11351', 'E11359', 'E1136', 'E1139', 'E1140', 'E1141', 'E1142',
                 'E1143', 'E1144', 'E1149', 'E1151', 'E1152', 'E1159', 'E11610',
                 'E11618', 'E11620', 'E11621', 'E11622', 'E11628', 'E11630', 
                 'E11638', 'E11641', 'E11649', 'E1165', 'E1169', 'E118', 'E119',
                 'E1300', 'E1301', 'E1310', 'E1311', 'E1321', 'E1322', 'E1329',
                 'E13311', 'E13319', 'E13321', 'E13329', 'E13331', 'E13339',
                 'E13341', 'E13349', 'E13351', 'E13359', 'E1336', 'E1339', 'E1340',
                 'E1341', 'E1342', 'E1343', 'E1344', 'E1349', 'E1351', 'E1352', 
                 'E1359', 'E13610', 'E13618', 'E13620', 'E13621', 'E13622','E13628',
                 'E13630', 'E13638', 'E13641', 'E13649', 'E1365', 'E1369', 'E138', 
                 'E139')
    
    #exclusions
    exc16 <- c('S78011A', 'S78012A', 'S78019A', 'S78021A', 'S78022A', 'S78029A',
               'S78111A', 'S78112A', 'S78119A', 'S78121A', 'S78122A', 'S78129A',
               'S78911A', 'S78912A', 'S78919A', 'S78921A', 'S78922A', 'S78929A',
               'S88011A', 'S88012A', 'S88019A', 'S88021A', 'S88022A', 'S88029A', 
               'S88111A', 'S88112A', 'S88119A', 'S88121A', 'S88122A', 'S88129A',
               'S88911A', 'S88912A', 'S88919A', 'S88921A', 'S88922A', 'S88929A',
               'S98011A', 'S98012A', 'S98019A', 'S98021A', 'S98022A', 'S98029A',
               'S98111A', 'S98112A', 'S98119A', 'S98121A', 'S98122A', 'S98129A',
               'S98131A', 'S98132A', 'S98139A', 'S98141A', 'S98142A', 'S98149A',
               'S98211A', 'S98212A', 'S98219A', 'S98221A', 'S98222A', 'S98229A',
               'S98311A', 'S98312A', 'S98319A', 'S98321A', 'S98322A', 'S98329A',
               'S98911A', 'S98912A', 'S98919A', 'S98921A', 'S98922A', 'S98929A')
    
    keep16 <- ((data$px1 %in% inc16 | data$px2 %in% inc16 | data$px3 %in% inc16 |
                  data$px4 %in% inc16 | data$px5 %in% inc16 | data$px6 %in% inc16 |
                  data$px7 %in% inc16 | data$px8 %in% inc16 | data$px9 %in% inc16 |
                  data$px10 %in% inc16) & 
                 (data$dx1 %in% inc16.2 | data$dx2 %in% inc16.2 | 
                    data$dx3 %in% inc16.2 | data$dx4 %in% inc16.2 | 
                    data$dx5 %in% inc16.2 | data$dx6 %in% inc16.2 |
                    data$dx7 %in% inc16.2 | data$dx8 %in% inc16.2 | 
                    data$dx9 %in% inc16.2 | data$dx10 %in% inc16.2)) &
      (!data$dx1 %in% exc16 & !data$dx2 %in% exc16 & !data$dx3 %in% exc16 &
         !data$dx4 %in% exc16 & !data$dx5 %in% exc16 & !data$dx6 %in% exc16 &
         !data$dx7 %in% exc16 & !data$dx8 %in% exc16 & !data$dx9 %in% exc16 &
         !data$dx10 %in% exc16) & data$age >= 18 & data$mdc != "14"
    
    PQI.16 <- ifelse(keep16, 1, 0)
    
  } else {
    PQI.16 <- NULL
  }
  ################################################
  #PQI 90 : Prevention Quality Overall Composite 
  ################################################
  if ("PQI.90" %in% pqi.match & ICD == "9") {
  ##Includes PQI: 1, 3, 5, 7, 8, 10, 11, 12, 14, 15, 16
  
  ##PQI 01##
  keep1 <- data$dx1 %in% c('25010', '25011', '25012', '25013', '25020', '25021',
                             '25022', '25023', '25030', '25031', '25032', '25033') & 
    data$age >= 18
    
  ##PQI 03##  
  keep3 <- data$dx1 %in% c('25040', '25041', '25042', '25043', '25050', '25051', 
                           '25052', '25053', '25060', '25061', '25062', '25063',
                           '25070', '25071', '25072', '25073', '25080', '25081',
                           '25082', '25083', '25090', '25091', '25092', '25093')
    
  ##PQI 05##  
  exc05 <- c('27700', '27701', '27702', '27703', '27709', '51661', '51662', 
               '51663', '51664', '51669', '74721', '7483', '7484', '7485', 
               '74860','74861', '74869', '7488', '7489', '7503', '7593', '7707') 
    
  keep05 <- data$dx1 %in% c('4910', '4911', '49120', '49121', '49122', '4918', 
                              '4919', '4920', '4928', '494', '4940', '4941', '496',
                              '49300', '49301', '49302', '49310', '49311', '49312',
                              '49320', '49321', '49322', '49381', '49382', '49390',
                              '49391', '49392') & #remove 4660 and 490
      !data$dx1 %in% exc05 & !data$dx2 %in% exc05 & !data$dx3 %in% exc05 & 
      !data$dx4 %in% exc05 & !data$dx5 %in% exc05 & !data$dx6 %in% exc05 & 
      !data$dx7 %in% exc05 & !data$dx8 %in% exc05 & !data$dx9 %in% exc05 & 
      !data$dx10 %in% exc05 & data$age >= 40
    
    
  ##PQI 07## 
  ##Exclusions - Cardiac Procedure Codes##
  exc7.1 <- c('0050', '3582', '0051', '3583', '0052', '0053', '0054', '0056', 
              '0057', '0066', '3584', '3591', '3592', '3593', '3594', '3595',
              '1751', '1752', '3596', '3597', '1755', '3598', '3500', '3599',
              '3501', '3502', '3601', '3602', '3503', '3603', '3504', '3604',
              '3505', '3605', '3506', '3606', '3507', '3607', '3508', '3609',
              '3509', '3610', '3510', '3611', '3511', '3612', '3512', '3613',
              '3513', '3614', '3514', '3615', '3520', '3521', '3522', '3523',
              '3524', '3525', '3526', '3527', '3528', '3531', '3616', '3617',
              '3619', '362', '363', '3631', '3632', '3633', '3634', '3639', 
              '3532', '3691', '3533', '3699', '3534', '3731', '3535', '3732',
              '3539', '3541', '3542', '3733', '3734', '3735', '3550', '3551',
              '3552', '3736', '3737', '3741', '3553', '375', '3554', '3751',
              '3555', '3752', '3560', '3561', '3562', '3753', '3754', '3755',
              '3563', '3760', '3570', '3761', '3571', '3762', '3572', '3763',
              '3573', '3764', '3581', '3765', '3766', '3770', '3771', '3772',
              '3773', '3774', '3775', '3776', '3777', '3778', '3779', '3780',
              '3781', '3782', '3783', '3785', '3786', '3787', '3789', '3794',
              '3795', '3796', '3797', '3798', '3826')
    
  ##Exclusions - Dialysis access##
  exc7.2 <- c('3895', '3927', '3929', '3942', '3943', '3993', '3994')
    
  ##Exclusions - Stage I-IV Kidney disease##  
  exc7.3 <- c('40300', '40310', '40390', '40400', '40410', '40490')
    
  keep07 <- data$dx1 %in% c('4010', '4019', '40200', '40210', '40290', '40300',
                            '40310', '40390', '40400', '40410', '40490') & 
      !data$px1 %in% exc7.1 & !data$px2 %in% exc7.1 & !data$px3 %in% exc7.1 & 
      !data$px4 %in% exc7.1 & !data$px5 %in% exc7.1 & !data$px6 %in% exc7.1 & 
      !data$px7 %in% exc7.1 & !data$px8 %in% exc7.1 & !data$px9 %in% exc7.1 & 
      !data$px10 %in% exc7.1 & 
      (!data$dx1 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 & 
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 & 
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 & 
            !data$px10 %in% exc7.2)) & 
      (!data$dx2 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 & 
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 & 
            !data$px10 %in% exc7.2)) & 
      (!data$dx3 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 & 
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
            !data$px10 %in% exc7.2)) & 
      (!data$dx4 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
            !data$px10 %in% exc7.2)) & 
      (!data$dx5 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
            !data$px10 %in% exc7.2)) & 
      (!data$dx6 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
            !data$px10 %in% exc7.2)) & 
      (!data$dx7 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
            !data$px10 %in% exc7.2)) & 
      (!data$dx8 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
            !data$px10 %in% exc7.2)) &
      (!data$dx9 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
            !data$px10 %in% exc7.2)) & 
      (!data$dx10 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
            !data$px10 %in% exc7.2)) & data$age >= 18
    
  ##PQI 08## 
  ##Exclusions - Cardiac Procedure Codes##
  exc8 <- c('0050', '3582', '0051', '3583', '0052', '0053', '0054', '0056',
            '0057', '0066', '3584', '3591', '3592', '3593', '3594', '3595', 
            '1751', '1752', '3596', '3597', '1755', '3598', '3500', '3599',
            '3501', '3502', '3601', '3602', '3503', '3603', '3504', '3604',
            '3505', '3605', '3506', '3606', '3507', '3607', '3508', '3609',
            '3509', '3610', '3510', '3611', '3511', '3612', '3512', '3613',
            '3513', '3614', '3514', '3615', '3520', '3521', '3522', '3523',
            '3524', '3525', '3526', '3527', '3528', '3531', '3616', '3617',
            '3619', '362', '363', '3631', '3632', '3633', '3634', '3639',
            '3532', '3691', '3533', '3699', '3534', '3731', '3535', '3732',
            '3539', '3541', '3542', '3733', '3734', '3735', '3550', '3551',
            '3552', '3736', '3737', '3741', '3553', '375', '3554', '3751', 
            '3555', '3752', '3560', '3561', '3562', '3753', '3754', '3755',
            '3563', '3760', '3570', '3761', '3571', '3762', '3572', '3763', 
            '3573', '3764', '3581', '3765', '3766', '3770', '3771', '3772',
            '3773', '3774', '3775', '3776', '3777', '3778', '3779', '3780',
            '3781', '3782', '3783', '3785', '3786', '3787', '3789', '3794',
            '3795', '3796', '3797', '3798', '3826')
    
  keep08 <- data$dx1 %in% c('39891', '40201', '40211', '40291', '40401', '40403',
                            '40411', '40413', '40491', '40493', '4280', '4281',
                            '42820', '42821', '42822', '42823', '42830',#42822 twice
                            '42831', '42832', '42833', '42840', '42841', '42842',
                            '42843', '4289')  & 
      (!data$px1 %in% exc8 & !data$px2 %in% exc8 & !data$px3 %in% exc8 & 
         !data$px4 %in% exc8 & !data$px5 %in% exc8 & !data$px6 %in% exc8 & 
         !data$px7 %in% exc8 & !data$px8 %in% exc8 & !data$px9 %in% exc8 & 
         !data$px10 %in% exc8) & data$age >= 18
    
  ##PQI 10##
  ##Include - Dehydration##
  inc10  <- c('2765', '27650', '27651', '27652')
    
  ##Include - Principal diagnosis for hyperosmolality 
  ##and/or hypernatremia, gastroenteritis, or acute kidney injury
  ##with secondary diagnosis for dehydration  
  inc10.2 <- c('2760', '00861', '00862', '00863', '00864', '00865', '00866',
                '00867', '00869', '0088', '0090', '0091', '0092', '0093', '5589',
                '5845', '5846', '5847', '5848', '5849', '586', '9975')
    
  ##Exclusions - Chronic renal failure##  
  exc10 <- c('40301', '40311', '40391', '40402', '40403', '40412', '40413',
             '40492', '40493', '5855', '5856')
    
  A <- (data$dx1 %in% inc10)
    
  B <- (data$dx1 %in% inc10.2 & 
          (data$dx2 %in% inc10 | data$dx3 %in% inc10 | data$dx4 %in% inc10 |
           data$dx5 %in% inc10 | data$dx6 %in% inc10 | data$dx7 %in% inc10 | 
           data$dx8 %in% inc10 | data$dx9 %in% inc10 | data$dx10 %in% inc10))
    
  C <- (!data$dx1 %in% exc10 & !data$dx2 %in% exc10 & !data$dx3 %in% exc10 & 
        !data$dx4 %in% exc10 & !data$dx5 %in% exc10 & !data$dx6 %in% exc10 &
        !data$dx7 %in% exc10 & !data$dx8 %in% exc10 & !data$dx9 %in% exc10 &
        !data$dx10 %in% exc10)
    
  keep10 <- (A | B) & C & data$age >= 18
    
  ##PQI 11## 
  ##Exclusions - Sickle Cell Anemia##
  exc11  <- c('28241', '28242', '28260', '28261', '28262', '28263', '28264',
              '28268', '28269') 
    
  ##Exclusions - immunocompromised state diagnosis codes##  
  exc11.2 <- c('042', '1363', '1992', '23873', '23876', '23877', '23879', 
               '260', '261', '262', '27900', '27901', '27902', '27903', 
               '27904', '27905', '27906', '27909', '27910', '27911', '27912',
               '27913', '27919', '2792', '2793', '2794', '27941', '27949', 
               '27950', '27951', '27952', '27953', '2798', '2799', '28409', 
               '2841', '28411', '28412', '28419', '2880', '28800', '28801',
               '28802', '28803', '28809', '2881', '2882', '2884', '28850', 
               '28851', '28859', '28953', '28983', '40301', '40311', '40391',
               '40402', '40403', '40412', '40413', '40492', '40493', '5793',
               '585', '5855', '5856', '9968', '99680', '99681', '99682', 
               '99683', '99684', '99685', '99686', '99687', '99688', '99689',
               'V420', 'V421', 'V426', 'V427', 'V428', 'V4281', 'V4282', 'V4283',
               'V4284', 'V4289', 'V451', 'V4511', 'V560', 'V561', 'V562')
    
  ##Exclusions - immunocompromised state procedure codes##  
  exc11.3 <- c('0018', '335', '336', '375', '3350', '3751', '3351', '410', '3352',
               '4100', '5051', '4101', '4102', '5059', '5280', '4103', '5281',
               '4104', '4105', '4106', '5282', '5283', '5285', '4107', '5286',
               '4108', '5569', '4109', '4697')#add 4697
    
  keep11 <- data$dx1 %in% c('481', '4822', '48230', '48231', '48232', '48239',
                            '48241', '48242', '4829', '4830', '4831', '4838',
                            '485', '486', '48249') & #add 48249
    (!data$dx1 %in% exc11 & !data$dx2 %in% exc11 & !data$dx3 %in% exc11 & 
     !data$dx4 %in% exc11 & !data$dx5 %in% exc11 & !data$dx6 %in% exc11 & 
     !data$dx7 %in% exc11 & !data$dx8 %in% exc11 & !data$dx9 %in% exc11 & 
     !data$dx10 %in% exc11) & 
    (!data$dx1 %in% exc11.2 & !data$dx2 %in% exc11.2 & !data$dx3 %in% exc11.2 & 
     !data$dx4 %in% exc11.2 & !data$dx5 %in% exc11.2 & !data$dx6 %in% exc11.2 &
     !data$dx7 %in% exc11.2 & !data$dx8 %in% exc11.2 & !data$dx9 %in% exc11.2 &
     !data$dx10 %in% exc11.2 ) & 
    (!data$px1 %in% exc11.3 & !data$px2 %in% exc11.3 & !data$px3 %in% exc11.3 &
     !data$px4 %in% exc11.3 & !data$px5 %in% exc11.3 & !data$px6 %in% exc11.3 &
     !data$px7 %in% exc11.3 & !data$px8 %in% exc11.3 & !data$px9 %in% exc11.3 &
     !data$px10 %in% exc11.3) & data$age >= 18
    
  ##PQI 12##
  ##Exclusions - Kidney/urinary tract disorder##
  exc12  <- c('59000', '59001', '59370', '59371', '59372', '59373', '7530', 
              '75310', '75311', '75312', '75313', '75314', '75315', '75316',
              '75317', '75319', '75320', '75321', '75322', '75323', '75329',
              '7533', '7534', '7535', '7536', '7538', '7539') 
    
  ##Exclusions - immunocompromised state diagnosis codes##  
  exc12.2 <- c('042', '1363', '1992', '23873', '23876', '23877', '23879', 
               '260','261', '262', '27900', '27901', '27902', '27903', 
               '27904', '27905', '27906', '27909', '27910', '27911', '27912',
               '27913', '27919', '2792', '2793', '2794', '27941', '27949', 
               '27950', '27951', '27952', '27953', '2798', '2799', '28409', 
               '2841', '28411', '28412', '28419', '2880', '28800', '28801', 
               '28802', '28803', '28809', '2881', '2882', '2884', '28850', 
               '28851', '28859','28953', '28983', '40301', '40311', '40391', 
               '40402', '40403', '40412', '40413', '40492', '40493', '5793', 
               '585', '5855', '5856', '9968', '99680', '99681', '99682', 
               '99683', '99684', '99685','99686', '99687', '99688', '99689', 
               'V420', 'V421', 'V426', 'V427', 'V428', 'V4281', 'V4282', 
               'V4283', 'V4284', 'V4289', 'V451','V4511', 'V560', 'V561', 
               'V562')
    
  ##Exclusions - immunocompromised state procedure codes##
  exc12.3 <- c('0018', '335', '336', '375', '3350', '3751', '3351', '410', 
               '3352', '4100', '5051', '4101', '4102', '5059', '5280', '4103', 
               '5281', '4104', '4105', '4106', '5282', '5283', '5285', '4107', 
               '5286', '4108', '5569', '4109', '4697')#add 4697
    
  keep12 <- data$dx1 %in% c('59010', '59011', '5902', '5903', '59080', '59081', 
                            '5909', '5950', '5959', '5990') & 
    (!data$dx1 %in% exc12 & !data$dx2 %in% exc12 & !data$dx3 %in% exc12 & 
     !data$dx4 %in% exc12 & !data$dx5 %in% exc12 & !data$dx6 %in% exc12 & 
     !data$dx7 %in% exc12 & !data$dx8 %in% exc12 & !data$dx9 %in% exc12 & 
     !data$dx10 %in% exc12) & 
    (!data$dx1 %in% exc12.2 & !data$dx2 %in% exc12.2 & !data$dx3 %in% exc12.2 &
     !data$dx4 %in% exc12.2 & !data$dx5 %in% exc12.2 & !data$dx6 %in% exc12.2 &
     !data$dx7 %in% exc12.2 & !data$dx8 %in% exc12.2 & !data$dx9 %in% exc12.2 &
     !data$dx10 %in% exc12.2 ) & 
    (!data$px1 %in% exc12.3 & !data$px2 %in% exc12.3 & !data$px3 %in% exc12.3 &
     !data$px4 %in% exc12.3 & !data$px5 %in% exc12.3 & !data$px6 %in% exc12.3 &
     !data$px7 %in% exc12.3 & !data$px8 %in% exc12.3 & !data$px9 %in% exc12.3 &
     !data$px10 %in% exc12.3) & data$age >= 18
    
  ##PQI 14##  
  keep14 <- data$dx1 %in% c('25002', '25003') & data$age >= 18
    
  ##PQI 15##
  ##Exclusions - Cystic fibrosis and anamolies of the respiratory system ##
  exc15 <- c('27700', '27701', '27702', '27703', '27709', '51661', '51662', 
             '51663', '51664', '51669', '74721', '7483', '7484', '7485', 
             '74860', '74861', '74869', '7488', '7489', '7503', '7593', '7707')
    
  keep15 <- data$dx1 %in% c('49300', '49301', '49302', '49310', '49311', 
                            '49312', '49320', '49321', '49322', '49381', 
                            '49382', '49390', '49391', '49392') & 
    (!data$dx1 %in% exc15 & !data$dx2 %in% exc15 & !data$dx3 %in% exc15 & 
     !data$dx4 %in% exc15 & !data$dx5 %in% exc15 & !data$dx6 %in% exc15 & 
     !data$dx7 %in% exc15 & !data$dx8 %in% exc15 & !data$dx9 %in% exc15 & 
     !data$dx10 %in% exc15 ) & data$age >= 18 & data$age <= 39
    
  ##PQI 16##
    
  ##Include - Amputation##
  inc16 <- c('8410', '8412', '8413', '8414', '8415', '8416', '8417', '8418', '8419')
    
  ##Include Diabetes##
  inc16.2 <- c('25000', '25001', '25002', '25003', '25010', '25011', '25012', 
               '25013', '25020', '25021', '25022', '25023', '25030', '25031',
               '25032', '25033', '25040', '25041', '25042', '25043', '25050',
               '25051', '25052', '25053', '25060', '25061', '25062', '25063',
               '25070', '25071', '25072', '25073', '25080', '25081', '25082',
               '25083', '25090', '25091', '25092', '25093')
    
  ##Exclusions##
  exc16 <- c('8950', '8951', '8960', '8961', '8962', '8963', '8970', '8971', 
             '8972', '8973', '8974', '8975', '8976', '8977')
    
  keep16 <- ((data$px1 %in% inc16 | data$px2 %in% inc16 | data$px3 %in% inc16 |
              data$px4 %in% inc16 | data$px5 %in% inc16 | data$px6 %in% inc16 |
              data$px7 %in% inc16 | data$px8 %in% inc16 | data$px9 %in% inc16 |
              data$px10 %in% inc16) & 
            (data$dx1 %in% inc16.2 | data$dx2 %in% inc16.2 | 
             data$dx3 %in% inc16.2 | data$dx4 %in% inc16.2 | 
             data$dx5 %in% inc16.2 | data$dx6 %in% inc16.2 |
             data$dx7 %in% inc16.2 | data$dx8 %in% inc16.2 | 
             data$dx9 %in% inc16.2 | data$dx10 %in% inc16.2)) &
    !data$dx1 %in% exc16 & !data$dx2 %in% exc16 & !data$dx3 %in% exc16 &
    !data$dx4 %in% exc16 & !data$dx5 %in% exc16 & !data$dx6 %in% exc16 &
    !data$dx7 %in% exc16 & !data$dx8 %in% exc16 & !data$dx9 %in% exc16 &
    !data$dx10 %in% exc16 & data$age >= 18
    
    
  keep90 <- keep01 | keep03 | keep05 | keep07 | keep08 | keep10 | keep11 | keep12 |
    keep14 | keep15 | keep16
  
  PQI.90 <- ifelse(keep90, 1, 0)
  } else if ("PQI.90" %in% pqi.match & ICD == "10"){
    ##Includes PQI: 1, 3, 5, 7, 8, 10, 11, 12, 14, 15, 16
    
    ##PQI 01##
    keep01 <- data$dx1 %in% c('E1010', 'E1011', 'E10641', 'E1065', 'E1100', 
                              'E1101', 'E11641', 'E1165') & data$age >= 18
    
    ##PQI 03##
    keep03 <- data$dx1 %in% c('E1021', 'E1121', 'E1022', 'E1122', 'E1029',
                              'E1129', 'E10311', 'E11311', 'E10319', 'E11319',
                              'E10321', 'E11321', 'E10329', 'E11329', 'E10331',
                              'E11331', 'E10339', 'E11339', 'E10341', 'E11341',
                              'E10349', 'E11349', 'E10351', 'E11351', 'E10359',
                              'E11359', 'E1036', 'E1136', 'E1039', 'E1139',
                              'E1040', 'E1140', 'E1041', 'E1141', 'E1042', 
                              'E1142', 'E1043', 'E1143', 'E1044', 'E1144', 
                              'E1049', 'E1149', 'E1051', 'E1151', 'E1052',
                              'E1152', 'E1059', 'E1159', 'E10610', 'E11610',
                              'E10618', 'E11618', 'E10620', 'E11620', 'E10621', 
                              'E11621', 'E10622', 'E11622', 'E10628', 'E11628', 
                              'E10630', 'E11630', 'E10638', 'E11638', 'E1069', 
                              'E1169', 'E108', 'E118') & data$age >= 18
    
    ##PQI 05##
    exc05 <- c('E840', 'E8411', 'E8419', 'E848', 'E849', 'J8483',
               'J84841', 'J84842', 'J84843', 'J84848', 'P270', 'P271',
               'P278', 'P279', 'Q254', 'Q322', 'Q323', 'Q324', 'Q330', 
               'Q331', 'Q332', 'Q333', 'Q334', 'Q335', 'Q336', 'Q338',
               'Q339', 'Q340', 'Q341', 'Q348', 'Q311', 'Q312', 'Q313',
               'Q315', 'Q318', 'Q319', 'Q320', 'Q321', 'Q349', 'Q390',
               'Q391', 'Q392', 'Q393', 'Q394', 'Q893')
    
    keep05 <- data$dx1 %in% c('J410', 'J439', 'J411', 'J440', 'J418', 'J441', 'J42', 
                              'J449', 'J430', 'J470', 'J431', 'J471', 'J432', 'J479',
                              'J438','J4521', 'J4552', 'J4522', 'J45901', 'J4531',
                              'J45902', 'J4532', 'J45990', 'J4541', 'J45991',
                              'J4542', 'J45998', 'J4551') & 
      !data$dx1 %in% exc05 & !data$dx2 %in% exc05 & !data$dx3 %in% exc05 & 
      !data$dx4 %in% exc05 & !data$dx5 %in% exc05 & !data$dx6 %in% exc05 & 
      !data$dx7 %in% exc05 & !data$dx8 %in% exc05 & !data$dx9 %in% exc05 & 
      !data$dx10 %in% exc05 & data$age >= 40
    
    ##PQI 07##
    ##Exclusions - Cardiac Procedure Codes##
    
    exc7 <- c('0210093', '0210098', '0210099', '021009C', '021009F', '021009W', 
              '02100A3', '02100A8', '02100A9', '02100AC', '02100AF', '02100AW', 
              '02100J3', '02100J8', '02100J9', '02100JC', '02100JF', '02100JW', 
              '02100K3', '02100K8', '02100K9', '02100KC', '02100KF', '02100KW', 
              '02100Z3', '02100Z8', '02100Z9', '02100ZC', '02100ZF', '0210344',
              '02103D4', '0210444', '0210493', '0210498', '0210499', '021049C', 
              '021049F', '021049W', '02104A3', '02104A8', '02104A9', '02104AC', 
              '02104AF', '02104AW', '02104D4', '02104J3', '02104J8', '02104J9',
              '02104JC', '02104JF', '02104JW', '02104K3', '02104K8', '02104K9',
              '02104KC', '02104KF', '02104KW', '02104Z3', '02104Z8', '02104Z9',
              '02104ZC', '02104ZF', '0211093', '0211098', '0211099', '021109C',
              '021109F', '021109W', '02110A3', '02110A8', '02110A9', '02110AC',
              '02110AF', '02110AW', '02110J3', '02110J8', '02110J9', '02110JC',
              '02110JF', '02110JW', '02110K3', '02110K8', '02110K9', '02110KC',
              '02110KF', '02110KW', '02110Z3', '02110Z8', '02110Z9', '02110ZC',
              '02110ZF', '0211344', '02113D4', '0211444', '0211493', '0211498',
              '0211499', '021149C', '021149F', '021149W', '02114A3', '02114A8',
              '02114A9', '02114AC', '02114AF', '02114AW', '02114D4', '02114J3',
              '02114J8', '02114J9', '02114JC', '02114JF', '02114JW', '02114K3',
              '02114K8', '02114K9', '02114KC', '02114KF', '02114KW', '02114Z3',
              '02114Z8', '02114Z9', '02114ZC', '02114ZF', '0212093', '0212098',
              '0212099', '021209C', '021209F', '021209W', '02120A3', '02120A8',
              '02120A9', '02120AC', '02120AF', '02120AW', '02120J3', '02120J8',
              '02120J9', '02120JC', '02120JF', '02120JW', '02120K3', '02120K8',
              '02120K9', '02120KC', '02120KF', '02120KW', '02120Z3', '02120Z8',
              '02120Z9', '02120ZC', '02120ZF', '0212344', '02123D4', '0212444',
              '0212493', '0212498', '0212499', '021249C', '021249F', '021249W',
              '02124A3', '02124A8', '02124A9', '02124AC', '02124AF', '02124AW',
              '02124D4', '02124J3', '02124J8', '02124J9', '02124JC', '02124JF',
              '02124JW', '02124K3', '02124K8', '02124K9', '02124KC', '02124KF',
              '02124KW', '02124Z3', '02124Z8', '02124Z9', '02124ZC', '02124ZF',
              '0213093', '0213098', '0213099', '021309C', '021309F', '021309W',
              '02130A3', '02130A8', '02130A9', '02130AC', '02130AF', '02130AW',
              '02130J3', '02130J8', '02130J9', '02130JC', '02130JF', '02130JW',
              '02130K3', '02130K8', '02130K9', '02130KC', '02130KF', '02130KW',
              '02130Z3', '02130Z8', '02130Z9', '02130ZC', '02130ZF', '0213344',
              '02133D4', '0213444', '0213493', '0213498', '0213499', '021349C',
              '021349F', '021349W', '02134A3', '02134A8', '02134A9', '02134AC',
              '02134AF', '02134AW', '02134D4', '02134J3', '02134J8', '02134J9',
              '02134JC', '02134JF', '02134JW', '02134K3', '02134K8', '02134K9',
              '02134KC', '02134KF', '02134KW', '02134Z3', '02134Z8', '02134Z9',
              '02134ZC', '02134ZF', '021609P', '021609Q', '021609R', '02160AP',
              '02160AQ', '02160AR', '02160JP', '02160JQ', '02160JR', '02160KP',
              '02160KQ', '02160KR', '02160ZP', '02160ZQ', '02160ZR', '021649P',
              '021649Q', '021649R', '02164AP', '02164AQ', '02164AR', '02164JP',
              '02164JQ', '02164JR', '02164KP', '02164KQ', '02164KR', '02164ZP',
              '02164ZQ', '02164ZR', '021709P', '021709Q', '021709R', '02170AP',
              '02170AQ', '02170AR', '02170JP', '02170JQ', '02170JR', '02170KP',
              '02170KQ', '02170KR', '02170ZP', '02170ZQ', '02170ZR', '021749P',
              '021749Q', '021749R', '02174AP', '02174AQ', '02174AR', '02174JP',
              '02174JQ', '02174JR', '02174KP', '02174KQ', '02174KR', '02174ZP',
              '02174ZQ', '02174ZR', '021K09P', '021K09Q', '021K09R', '021K0AP',
              '021K0AQ', '021K0AR', '021K0JP', '021K0JQ', '021K0JR', '021K0KP',
              '021K0KQ', '021K0KR', '021K0Z5', '021K0Z8', '021K0Z9', '021K0ZC',
              '021K0ZF', '021K0ZP', '021K0ZQ', '021K0ZR', '021K0ZW', '021K49P',
              '021K49Q', '021K49R', '021K4AP', '021K4AQ', '021K4AR', '021K4JP',
              '021K4JQ', '021K4JR', '021K4KP', '021K4KQ', '021K4KR', '021K4Z5',
              '021K4Z8', '021K4Z9', '021K4ZC', '021K4ZF', '021L09R', '021L0AP', 
              '021L0AQ', '021L0AR', '021L0JP', '021L0JQ', '021K4ZP', '021K4ZQ',
              '021K4ZR', '021K4ZW', '021L09P', '021L09Q', '021L0JR', '021L0KP',
              '021L0KQ', '021L0KR', '021L0Z5', '021L0Z8', '021L0Z9', '021L0ZC',
              '021L0ZF', '021L0ZP', '021L0ZQ', '021L0ZR', '021L0ZW', '021L49P',
              '021L49Q', '021L49R', '021L4AP', '021L4AQ', '021L4AR', '021L4JP',
              '021L4JQ', '021L4JR', '021L4KP', '021L4KQ', '021L4KR', '021L4Z5',
              '021L4Z8', '021L4Z9', '021L4ZC', '021L4ZF', '021L4ZP', '021L4ZQ',
              '021L4ZR', '021L4ZW', '02540ZZ', '02543ZZ', '02544ZZ', '02550ZZ',
              '02553ZZ', '02554ZZ', '02560ZZ', '02563ZZ', '02564ZZ', '02570ZK',
              '02570ZZ', '02573ZK', '02573ZZ', '02574ZK', '02574ZZ', '02580ZZ',
              '02583ZZ', '02584ZZ', '02590ZZ', '02593ZZ', '02594ZZ', '025D0ZZ',
              '025D3ZZ', '025D4ZZ', '025F0ZZ', '025F3ZZ', '025F4ZZ', '025G0ZZ',
              '025G3ZZ', '025G4ZZ', '025H0ZZ', '025H3ZZ', '025H4ZZ', '025J0ZZ',
              '025J3ZZ', '025J4ZZ', '025K0ZZ', '025K3ZZ', '025K4ZZ', '025L0ZZ', 
              '025L3ZZ', '025L4ZZ', '025M0ZZ', '025M3ZZ', '025M4ZZ', '025N0ZZ', 
              '025N3ZZ', '025N4ZZ', '0270046', '027004Z', '02700D6', '02700DZ',
              '02700T6', '02700TZ', '02700Z6', '02700ZZ', '0270346', '027034Z', 
              '02703D6', '02703DZ', '02703T6', '02703TZ', '02703Z6', '02703ZZ',
              '0270446', '027044Z', '02704D6', '02704DZ', '02704T6', '02704TZ',
              '02704Z6', '02704ZZ', '0271046', '027104Z', '02710D6', '02710DZ',
              '02710T6', '02710TZ', '02710Z6', '02710ZZ', '0271346', '027134Z',
              '02713D6', '02713DZ', '02713T6', '02713TZ', '02713Z6', '02713ZZ',
              '0271446', '027144Z', '02714D6', '02714DZ', '02714T6', '02714TZ',
              '02714Z6', '02714ZZ', '0272046', '027204Z', '02720D6', '02720DZ',
              '02720T6', '02720TZ', '02720Z6', '02720ZZ', '0272346', '027234Z', 
              '02723D6', '02723DZ', '02723T6', '02723TZ', '02723Z6', '02723ZZ',
              '0272446', '027244Z', '02724D6', '02724DZ', '02724T6', '02724TZ',
              '02724Z6', '02724ZZ', '0273046', '027304Z', '02730D6', '02730DZ',
              '02730T6', '02730TZ', '02730Z6', '02730ZZ', '0273346', '027334Z',
              '02733D6', '02733DZ', '02733T6', '02733TZ', '02733Z6', '02733ZZ', 
              '0273446', '027344Z', '02734D6', '02734DZ', '02734T6', '02734TZ',
              '02734Z6', '02734ZZ', '027F04Z', '027F0DZ', '027F0ZZ', '027F34Z',
              '027F3DZ', '027F3ZZ', '027F44Z', '027F4DZ', '027F4ZZ', '027G04Z',
              '027G0DZ', '027G0ZZ', '027G34Z', '027G3DZ', '027G3ZZ', '027G44Z', 
              '027G4DZ', '027G4ZZ', '027H04Z', '027H0DZ', '027H0ZZ', '027H34Z',
              '027H3DZ', '027H3ZZ', '027H44Z', '027H4DZ', '027H4ZZ', '027J04Z',
              '027J0DZ', '027J0ZZ', '027J34Z', '027J3DZ', '027J3ZZ', '027J44Z',
              '027J4DZ', '027J4ZZ', '02890ZZ', '02893ZZ', '02894ZZ', '028D0ZZ',
              '028D3ZZ', '028D4ZZ', '02B40ZZ', '02B43ZZ', '02B44ZZ', '02B50ZZ',
              '02B53ZZ', '02B54ZZ', '02B60ZZ', '02B63ZZ', '02B64ZZ', '02B70ZK',
              '02B70ZZ', '02B73ZK', '02B73ZZ', '02B74ZK', '02B74ZZ', '02B80ZZ',
              '02B83ZZ', '02B84ZZ', '02B90ZZ', '02B93ZZ', '02B94ZZ', '02BD0ZZ',
              '02BD3ZZ', '02BD4ZZ', '02BF0ZZ', '02BF3ZZ', '02BF4ZZ', '02BG0ZZ',
              '02BG3ZZ', '02BG4ZZ', '02BH0ZZ', '02BH3ZZ', '02BH4ZZ', '02BJ0ZZ',
              '02BJ3ZZ', '02BJ4ZZ', '02BK0ZZ', '02BK3ZZ', '02BK4ZZ', '02BL0ZZ',
              '02BL3ZZ', '02BL4ZZ', '02BM0ZZ', '02BM3ZZ', '02BM4ZZ', '02BN0ZZ', 
              '02BN3ZZ', '02BN4ZZ', '02C00ZZ', '02C03ZZ', '02C04ZZ', '02C10ZZ',
              '02C13ZZ', '02C14ZZ', '02C20ZZ', '02C23ZZ', '02C24ZZ', '02C30ZZ',
              '02C33ZZ', '02C34ZZ', '02C40ZZ', '02C43ZZ', '02C44ZZ', '02C50ZZ',
              '02C53ZZ', '02C54ZZ', '02CD0ZZ', '02CD3ZZ', '02CD4ZZ', '02CF0ZZ',
              '02CF3ZZ', '02CF4ZZ', '02CG0ZZ', '02CG3ZZ', '02CG4ZZ', '02CH0ZZ', 
              '02CH3ZZ', '02CH4ZZ', '02CJ0ZZ', '02CJ3ZZ', '02CJ4ZZ', '02CM0ZZ', 
              '02CM3ZZ', '02CM4ZZ', '02H400Z', '02H402Z', '02H403Z', '02H40DZ',
              '02H40JZ', '02H40KZ', '02H40MZ', '02H430Z', '02H432Z', '02H433Z',
              '02H43DZ', '02H43JZ', '02H43KZ', '02H43MZ', '02H440Z', '02H442Z', 
              '02H443Z', '02H44DZ', '02H44JZ', '02H44KZ', '02H44MZ', '02H600Z',
              '02H60JZ', '02H60KZ', '02H60MZ', '02H630Z', '02H63JZ', '02H63KZ', 
              '02H63MZ', '02H640Z', '02H64JZ', '02H64KZ', '02H64MZ', '02H700Z',
              '02H70JZ', '02H70KZ', '02H70MZ', '02H730Z', '02H73JZ', '02H73KZ', 
              '02H73MZ', '02H740Z', '02H74JZ', '02H74KZ', '02H74MZ', '02HA0QZ',
              '02HA0RS', '02HA0RZ', '02HA3QZ', '02HA3RS', '02HA3RZ', '02HA4QZ',
              '02HA4RS', '02HA4RZ', '02HK00Z', '02HK02Z', '02HK0JZ', '02HK0KZ',
              '02HK0MZ', '02HK30Z', '02HK32Z', '02HK3JZ', '02HK3KZ', '02HK3MZ',
              '02HK40Z', '02HK42Z', '02HK4JZ', '02HK4KZ', '02HK4MZ', '02HL00Z',
              '02HL0JZ', '02HL0KZ', '02HL0MZ', '02HL30Z', '02HL3JZ', '02HL3KZ',
              '02HL3MZ', '02HL40Z', '02HL4JZ', '02HL4KZ', '02HL4MZ', '02HN0JZ',
              '02HN0KZ', '02HN0MZ', '02HN3JZ', '02HN3KZ', '02HN3MZ', '02HN4JZ',
              '02HN4KZ', '02HN4MZ', '02HS00Z', '02HS30Z', '02HS40Z', '02HT00Z', 
              '02HT30Z', '02HT40Z', '02HV00Z', '02HV30Z', '02HV40Z', '02L70CK',
              '02L70DK', '02L70ZK', '02L73CK', '02L73DK', '02L73ZK', '02L74CK',
              '02L74DK', '02L74ZK', '02LR0ZT', '02LS0ZZ', '02LT0ZZ', '02N50ZZ',
              '02N53ZZ', '02N54ZZ', '02N90ZZ', '02N93ZZ', '02N94ZZ', '02ND0ZZ',
              '02ND3ZZ', '02ND4ZZ', '02NF0ZZ', '02NF3ZZ', '02NF4ZZ', '02NG0ZZ',
              '02NG3ZZ', '02NG4ZZ', '02NH0ZZ', '02NH3ZZ', '02NH4ZZ', '02NJ0ZZ',
              '02NJ3ZZ', '02NJ4ZZ', '02NK0ZZ', '02NK3ZZ', '02NK4ZZ', '02NL0ZZ',
              '02NL3ZZ', '02NL4ZZ', '02NM0ZZ', '02NM3ZZ', '02NM4ZZ', '02PA0MZ',
              '02PA0QZ', '02PA0RZ', '02PA3MZ', '02PA3QZ', '02PA3RZ', '02PA4MZ',
              '02PA4QZ', '02PA4RZ', '02PAXMZ', '02Q00ZZ', '02Q03ZZ', '02Q04ZZ',
              '02Q10ZZ', '02Q13ZZ', '02Q14ZZ', '02Q20ZZ', '02Q23ZZ', '02Q24ZZ',
              '02Q30ZZ', '02Q33ZZ', '02Q34ZZ', '02Q40ZZ', '02Q43ZZ', '02Q44ZZ',
              '02Q50ZZ', '02Q53ZZ', '02Q54ZZ', '02Q70ZZ', '02Q73ZZ', '02Q74ZZ',
              '02Q90ZZ', '02Q93ZZ', '02Q94ZZ', '02QA0ZZ', '02QA3ZZ', '02QA4ZZ',
              '02QB0ZZ', '02QB3ZZ', '02QB4ZZ', '02QC0ZZ', '02QC3ZZ', '02QC4ZZ',
              '02QD0ZZ', '02QD3ZZ', '02QD4ZZ', '02QF0ZZ', '02QF3ZZ', '02QF4ZZ',
              '02QG0ZZ', '02QG3ZZ', '02QG4ZZ', '02QH0ZZ', '02QH3ZZ', '02QH4ZZ',
              '02QJ0ZZ', '02QJ3ZZ', '02QJ4ZZ', '02QM0ZZ', '02QM3ZZ', '02QM4ZZ',
              '02R907Z', '02R908Z', '02R90JZ', '02R90KZ', '02R947Z', '02R948Z',
              '02R94JZ', '02R94KZ', '02RD07Z', '02RD08Z', '02RD0JZ', '02RD0KZ',
              '02RD47Z', '02RD48Z', '02RD4JZ', '02RD4KZ', '02RF07Z', '02RF08Z', 
              '02RF0JZ', '02RF0KZ', '02RF37H', '02RF37Z', '02RF38H', '02RF38Z',
              '02RF3JH', '02RF3JZ', '02RF3KH', '02RF3KZ', '02RF47Z', '02RF48Z',
              '02RF4JZ', '02RF4KZ', '02RG07Z', '02RG08Z', '02RG0JZ', '02RG0KZ',
              '02RG37H', '02RG37Z', '02RG38H', '02RG38Z', '02RG3JH', '02RG3JZ',
              '02RG3KH', '02RG3KZ', '02RG47Z', '02RG48Z', '02RG4JZ', '02RG4KZ',
              '02RH07Z', '02RH08Z', '02RH0JZ', '02RH0KZ', '02RH37H', '02RH37Z',
              '02RH38H', '02RH38Z', '02RH3JH', '02RH3JZ', '02RH3KH', '02RH3KZ', 
              '02RH47Z', '02RH48Z', '02RH4JZ', '02RH4KZ', '02RJ07Z', '02RJ08Z',
              '02RJ0JZ', '02RJ0KZ', '02RJ47Z', '02RJ48Z', '02RJ4JZ', '02RJ4KZ',
              '02RK07Z', '02RK0JZ', '02RK0KZ', '02RK47Z', '02RK4KZ', '02RL07Z',
              '02RL0JZ', '02RL0KZ', '02RL47Z', '02RL4KZ', '02RM07Z', '02RM0JZ',
              '02RM0KZ', '02RM47Z', '02RM4JZ', '02RM4KZ', '02RP0JZ', '02RQ07Z',
              '02RQ0JZ', '02RR07Z', '02RR0JZ', '02SP0ZZ', '02SW0ZZ', '02T50ZZ',
              '02T53ZZ', '02T54ZZ', '02T80ZZ', '02T83ZZ', '02T84ZZ', '02T90ZZ',
              '02T93ZZ', '02T94ZZ', '02TD0ZZ', '02TD3ZZ', '02TD4ZZ', '02TH0ZZ',
              '02TH3ZZ', '02TH4ZZ', '02TM0ZZ', '02TM3ZZ', '02TM4ZZ', '02TN0ZZ',
              '02TN3ZZ', '02TN4ZZ', '02U507Z', '02U508Z', '02U50JZ', '02U50KZ',
              '02U537Z', '02U538Z', '02U53JZ', '02U53KZ', '02U547Z', '02U548Z',
              '02U54JZ', '02U54KZ', '02U607Z', '02U608Z', '02U60KZ', '02U707Z',
              '02U708Z', '02U70JZ', '02U70KZ', '02U737Z', '02U738Z', '02U73KZ',
              '02U747Z', '02U748Z', '02U74KZ', '02U907Z', '02U908Z', '02U90JZ', 
              '02U90KZ', '02U937Z', '02U938Z', '02U93JZ', '02U93KZ', '02U947Z',
              '02U948Z', '02U94JZ', '02U94KZ', '02UA0JZ', '02UA3JZ', '02UA4JZ',
              '02UD07Z', '02UD08Z', '02UD0JZ', '02UD0KZ', '02UD37Z', '02UD38Z',
              '02UD3JZ', '02UD3KZ', '02UD47Z', '02UD48Z', '02UD4JZ', '02UD4KZ',
              '02UF07Z', '02UF08Z', '02UF0JZ', '02UF0KZ', '02UF37Z', '02UF38Z',
              '02UF3JZ', '02UF3KZ', '02UF47Z', '02UF48Z', '02UF4JZ', '02UF4KZ', 
              '02UG07Z', '02UG08Z', '02UG0JZ', '02UG0KZ', '02UG37Z', '02UG38Z', 
              '02UG3JZ', '02UG3KZ', '02UG47Z', '02UG48Z', '02UG4JZ', '02UG4KZ',
              '02UH07Z', '02UH08Z', '02UH0JZ', '02UH0KZ', '02UH37Z', '02UH38Z',
              '02UH3JZ', '02UH3KZ', '02UH47Z', '02UH48Z', '02UH4JZ', '02UH4KZ',
              '02UJ07Z', '02UJ08Z', '02UJ0JZ', '02UJ0KZ', '02UJ37Z', '02UJ38Z',
              '02UJ3JZ', '02UJ3KZ', '02UJ47Z', '02UJ48Z', '02UJ4JZ', '02UJ4KZ',
              '02UK0KZ', '02UK3KZ', '02UK4KZ', '02UL0KZ', '02UL3KZ', '02UL4KZ', 
              '02UM07Z', '02UM0JZ', '02UM0KZ', '02UM37Z', '02UM38Z', '02UM3JZ',
              '02UM3KZ', '02UM47Z', '02UM48Z', '02UM4JZ', '02UM4KZ', '02VR0ZT',
              '02W50JZ', '02W54JZ', '02WA0JZ', '02WA0MZ', '02WA0QZ', '02WA0RZ',
              '02WA3MZ', '02WA3QZ', '02WA3RZ', '02WA4MZ', '02WA4QZ', '02WA4RZ',
              '02WF07Z', '02WF08Z', '02WF0JZ', '02WF0KZ', '02WF47Z', '02WF48Z',
              '02WF4JZ', '02WF4KZ', '02WG07Z', '02WG08Z', '02WG0JZ', '02WG0KZ',
              '02WG47Z', '02WG48Z', '02WG4JZ', '02WG4KZ', '02WH07Z', '02WH08Z',
              '02WH0JZ', '02WH0KZ', '02WH47Z', '02WH48Z', '02WH4JZ', '02WH4KZ',
              '02WJ07Z', '02WJ08Z', '02WJ0JZ', '02WJ0KZ', '02WJ47Z', '02WJ48Z',
              '02WJ4JZ', '02WJ4KZ', '02WM0JZ', '02WM4JZ', '02YA0Z0', '02YA0Z1',
              '02YA0Z2', '0JH600Z', '0JH604Z', '0JH605Z', '0JH606Z', '0JH607Z',
              '0JH608Z', '0JH609Z', '0JH60AZ', '0JH60PZ', '0JH630Z', '0JH634Z',
              '0JH635Z', '0JH636Z', '0JH637Z', '0JH638Z', '0JH639Z', '0JH63AZ',
              '0JH63PZ', '0JH800Z', '0JH804Z', '0JH805Z', '0JH806Z', '0JH807Z',
              '0JH808Z', '0JH809Z', '0JH80AZ', '0JH80PZ', '0JH830Z', '0JH834Z',
              '0JH835Z', '0JH836Z', '0JH837Z', '0JH838Z', '0JH839Z', '0JH83AZ',
              '0JH83PZ', '0JPT0PZ', '0JPT3PZ', '0JWT0PZ', '0JWT3PZ', '3E07017',
              '3E070PZ', '3E07317', '3E073PZ', '5A02110', '5A02116', '5A02210',
              '5A02216', '5A1213Z', '5A1223Z')
    
    ##Exclusions - Dialysis access##
    
    exc7.2 <- c('03170AD', '03170AF', '031209D', '031209F', '03120AD', '03120AF', 
                '03120JD', '03120JF', '03120KD', '03120KF', '03120ZD', '03120ZF',
                '031309D', '031309F', '03130AD', '03130AF', '03130JD', '03130JF', 
                '03130KD', '03130KF', '03130ZD', '03130ZF', '031409D', '031409F',
                '03140AD', '03140AF', '03140JD', '03140JF', '03140KD', '03140KF',
                '03140ZD', '03140ZF', '031509D', '031509F', '03150AD', '03150AF',
                '03150JD', '03150JF', '03150KD', '03150KF', '03150ZD', '03150ZF',
                '031609D', '031609F', '03160AD', '03160AF', '03160JD', '03160JF',
                '03160KD', '03160KF', '03160ZD', '03160ZF', '031709D', '031709F', 
                '03170JD', '03170JF', '03170KD', '03170KF', '03170ZD', '03170ZF',
                '031809D', '031809F', '03180AD', '03180AF', '03180JD', '03180JF',
                '03180KD', '03180KF', '03180ZD', '03180ZF', '031909F', '03190AF',
                '03190JF', '03190KF', '03190ZF', '031A09F', '031A0AF', '031A0JF',
                '031A0KF', '031A0ZF', '031B09F', '031B0AF', '031B0JF', '031B0KF',
                '031B0ZF', '031C09F', '031C0AF', '031C0JF', '031C0KF', '031C0ZF',
                '03PY07Z', '03PY0JZ', '03PY0KZ', '03PY37Z', '03PY3JZ', '03PY3KZ',
                '03PY47Z', '03PY4JZ', '03PY4KZ', '03WY0JZ', '03WY3JZ', '03WY4JZ',
                '03WYXJZ', '05HY33Z', '06HY33Z')
    
    
    ##Exclusions - Stage I-IV Kidney disease##
    
    exc7.3 <- c('I129', 'I1310')
    
    keep07 <- data$dx1 %in% c('I10', 'I129', 'I119', 'I1310') & 
      !data$px1 %in% exc7 & !data$px2 %in% exc7 & !data$px3 %in% exc7 & 
      !data$px4 %in% exc7 & !data$px5 %in% exc7 & !data$px6 %in% exc7 & 
      !data$px7 %in% exc7 & !data$px8 %in% exc7 & !data$px9 %in% exc7 & 
      !data$px10 %in% exc7 & 
      (!data$dx1 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 & 
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 & 
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 & 
            !data$px10 %in% exc7.2)) & 
      (!data$dx2 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 & 
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 & 
            !data$px10 %in% exc7.2)) & 
      (!data$dx3 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 & 
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
            !data$px10 %in% exc7.2)) & 
      (!data$dx4 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
            !data$px10 %in% exc7.2)) & 
      (!data$dx5 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
            !data$px10 %in% exc7.2)) & 
      (!data$dx6 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
            !data$px10 %in% exc7.2)) & 
      (!data$dx7 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
            !data$px10 %in% exc7.2)) & 
      (!data$dx8 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
            !data$px10 %in% exc7.2)) &
      (!data$dx9 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
            !data$px10 %in% exc7.2)) & 
      (!data$dx10 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
            !data$px10 %in% exc7.2)) & data$age >= 18
    
    ##PQI 08##
    ##Exclusions - Cardiac Procedures (same as exc7)
    
    exc8 <- c('0210093', '0210098', '0210099', '021009C', '021009F', '021009W', 
              '02100A3', '02100A8', '02100A9', '02100AC', '02100AF', '02100AW', 
              '02100J3', '02100J8', '02100J9', '02100JC', '02100JF', '02100JW', 
              '02100K3', '02100K8', '02100K9', '02100KC', '02100KF', '02100KW', 
              '02100Z3', '02100Z8', '02100Z9', '02100ZC', '02100ZF', '0210344',
              '02103D4', '0210444', '0210493', '0210498', '0210499', '021049C', 
              '021049F', '021049W', '02104A3', '02104A8', '02104A9', '02104AC', 
              '02104AF', '02104AW', '02104D4', '02104J3', '02104J8', '02104J9',
              '02104JC', '02104JF', '02104JW', '02104K3', '02104K8', '02104K9',
              '02104KC', '02104KF', '02104KW', '02104Z3', '02104Z8', '02104Z9',
              '02104ZC', '02104ZF', '0211093', '0211098', '0211099', '021109C',
              '021109F', '021109W', '02110A3', '02110A8', '02110A9', '02110AC',
              '02110AF', '02110AW', '02110J3', '02110J8', '02110J9', '02110JC',
              '02110JF', '02110JW', '02110K3', '02110K8', '02110K9', '02110KC',
              '02110KF', '02110KW', '02110Z3', '02110Z8', '02110Z9', '02110ZC',
              '02110ZF', '0211344', '02113D4', '0211444', '0211493', '0211498',
              '0211499', '021149C', '021149F', '021149W', '02114A3', '02114A8',
              '02114A9', '02114AC', '02114AF', '02114AW', '02114D4', '02114J3',
              '02114J8', '02114J9', '02114JC', '02114JF', '02114JW', '02114K3',
              '02114K8', '02114K9', '02114KC', '02114KF', '02114KW', '02114Z3',
              '02114Z8', '02114Z9', '02114ZC', '02114ZF', '0212093', '0212098',
              '0212099', '021209C', '021209F', '021209W', '02120A3', '02120A8',
              '02120A9', '02120AC', '02120AF', '02120AW', '02120J3', '02120J8',
              '02120J9', '02120JC', '02120JF', '02120JW', '02120K3', '02120K8',
              '02120K9', '02120KC', '02120KF', '02120KW', '02120Z3', '02120Z8',
              '02120Z9', '02120ZC', '02120ZF', '0212344', '02123D4', '0212444',
              '0212493', '0212498', '0212499', '021249C', '021249F', '021249W',
              '02124A3', '02124A8', '02124A9', '02124AC', '02124AF', '02124AW',
              '02124D4', '02124J3', '02124J8', '02124J9', '02124JC', '02124JF',
              '02124JW', '02124K3', '02124K8', '02124K9', '02124KC', '02124KF',
              '02124KW', '02124Z3', '02124Z8', '02124Z9', '02124ZC', '02124ZF',
              '0213093', '0213098', '0213099', '021309C', '021309F', '021309W',
              '02130A3', '02130A8', '02130A9', '02130AC', '02130AF', '02130AW',
              '02130J3', '02130J8', '02130J9', '02130JC', '02130JF', '02130JW',
              '02130K3', '02130K8', '02130K9', '02130KC', '02130KF', '02130KW',
              '02130Z3', '02130Z8', '02130Z9', '02130ZC', '02130ZF', '0213344',
              '02133D4', '0213444', '0213493', '0213498', '0213499', '021349C',
              '021349F', '021349W', '02134A3', '02134A8', '02134A9', '02134AC',
              '02134AF', '02134AW', '02134D4', '02134J3', '02134J8', '02134J9',
              '02134JC', '02134JF', '02134JW', '02134K3', '02134K8', '02134K9',
              '02134KC', '02134KF', '02134KW', '02134Z3', '02134Z8', '02134Z9',
              '02134ZC', '02134ZF', '021609P', '021609Q', '021609R', '02160AP',
              '02160AQ', '02160AR', '02160JP', '02160JQ', '02160JR', '02160KP',
              '02160KQ', '02160KR', '02160ZP', '02160ZQ', '02160ZR', '021649P',
              '021649Q', '021649R', '02164AP', '02164AQ', '02164AR', '02164JP',
              '02164JQ', '02164JR', '02164KP', '02164KQ', '02164KR', '02164ZP',
              '02164ZQ', '02164ZR', '021709P', '021709Q', '021709R', '02170AP',
              '02170AQ', '02170AR', '02170JP', '02170JQ', '02170JR', '02170KP',
              '02170KQ', '02170KR', '02170ZP', '02170ZQ', '02170ZR', '021749P',
              '021749Q', '021749R', '02174AP', '02174AQ', '02174AR', '02174JP',
              '02174JQ', '02174JR', '02174KP', '02174KQ', '02174KR', '02174ZP',
              '02174ZQ', '02174ZR', '021K09P', '021K09Q', '021K09R', '021K0AP',
              '021K0AQ', '021K0AR', '021K0JP', '021K0JQ', '021K0JR', '021K0KP',
              '021K0KQ', '021K0KR', '021K0Z5', '021K0Z8', '021K0Z9', '021K0ZC',
              '021K0ZF', '021K0ZP', '021K0ZQ', '021K0ZR', '021K0ZW', '021K49P',
              '021K49Q', '021K49R', '021K4AP', '021K4AQ', '021K4AR', '021K4JP',
              '021K4JQ', '021K4JR', '021K4KP', '021K4KQ', '021K4KR', '021K4Z5',
              '021K4Z8', '021K4Z9', '021K4ZC', '021K4ZF', '021L09R', '021L0AP', 
              '021L0AQ', '021L0AR', '021L0JP', '021L0JQ', '021K4ZP', '021K4ZQ',
              '021K4ZR', '021K4ZW', '021L09P', '021L09Q', '021L0JR', '021L0KP',
              '021L0KQ', '021L0KR', '021L0Z5', '021L0Z8', '021L0Z9', '021L0ZC',
              '021L0ZF', '021L0ZP', '021L0ZQ', '021L0ZR', '021L0ZW', '021L49P',
              '021L49Q', '021L49R', '021L4AP', '021L4AQ', '021L4AR', '021L4JP',
              '021L4JQ', '021L4JR', '021L4KP', '021L4KQ', '021L4KR', '021L4Z5',
              '021L4Z8', '021L4Z9', '021L4ZC', '021L4ZF', '021L4ZP', '021L4ZQ',
              '021L4ZR', '021L4ZW', '02540ZZ', '02543ZZ', '02544ZZ', '02550ZZ',
              '02553ZZ', '02554ZZ', '02560ZZ', '02563ZZ', '02564ZZ', '02570ZK',
              '02570ZZ', '02573ZK', '02573ZZ', '02574ZK', '02574ZZ', '02580ZZ',
              '02583ZZ', '02584ZZ', '02590ZZ', '02593ZZ', '02594ZZ', '025D0ZZ',
              '025D3ZZ', '025D4ZZ', '025F0ZZ', '025F3ZZ', '025F4ZZ', '025G0ZZ',
              '025G3ZZ', '025G4ZZ', '025H0ZZ', '025H3ZZ', '025H4ZZ', '025J0ZZ',
              '025J3ZZ', '025J4ZZ', '025K0ZZ', '025K3ZZ', '025K4ZZ', '025L0ZZ', 
              '025L3ZZ', '025L4ZZ', '025M0ZZ', '025M3ZZ', '025M4ZZ', '025N0ZZ', 
              '025N3ZZ', '025N4ZZ', '0270046', '027004Z', '02700D6', '02700DZ',
              '02700T6', '02700TZ', '02700Z6', '02700ZZ', '0270346', '027034Z', 
              '02703D6', '02703DZ', '02703T6', '02703TZ', '02703Z6', '02703ZZ',
              '0270446', '027044Z', '02704D6', '02704DZ', '02704T6', '02704TZ',
              '02704Z6', '02704ZZ', '0271046', '027104Z', '02710D6', '02710DZ',
              '02710T6', '02710TZ', '02710Z6', '02710ZZ', '0271346', '027134Z',
              '02713D6', '02713DZ', '02713T6', '02713TZ', '02713Z6', '02713ZZ',
              '0271446', '027144Z', '02714D6', '02714DZ', '02714T6', '02714TZ',
              '02714Z6', '02714ZZ', '0272046', '027204Z', '02720D6', '02720DZ',
              '02720T6', '02720TZ', '02720Z6', '02720ZZ', '0272346', '027234Z', 
              '02723D6', '02723DZ', '02723T6', '02723TZ', '02723Z6', '02723ZZ',
              '0272446', '027244Z', '02724D6', '02724DZ', '02724T6', '02724TZ',
              '02724Z6', '02724ZZ', '0273046', '027304Z', '02730D6', '02730DZ',
              '02730T6', '02730TZ', '02730Z6', '02730ZZ', '0273346', '027334Z',
              '02733D6', '02733DZ', '02733T6', '02733TZ', '02733Z6', '02733ZZ', 
              '0273446', '027344Z', '02734D6', '02734DZ', '02734T6', '02734TZ',
              '02734Z6', '02734ZZ', '027F04Z', '027F0DZ', '027F0ZZ', '027F34Z',
              '027F3DZ', '027F3ZZ', '027F44Z', '027F4DZ', '027F4ZZ', '027G04Z',
              '027G0DZ', '027G0ZZ', '027G34Z', '027G3DZ', '027G3ZZ', '027G44Z', 
              '027G4DZ', '027G4ZZ', '027H04Z', '027H0DZ', '027H0ZZ', '027H34Z',
              '027H3DZ', '027H3ZZ', '027H44Z', '027H4DZ', '027H4ZZ', '027J04Z',
              '027J0DZ', '027J0ZZ', '027J34Z', '027J3DZ', '027J3ZZ', '027J44Z',
              '027J4DZ', '027J4ZZ', '02890ZZ', '02893ZZ', '02894ZZ', '028D0ZZ',
              '028D3ZZ', '028D4ZZ', '02B40ZZ', '02B43ZZ', '02B44ZZ', '02B50ZZ',
              '02B53ZZ', '02B54ZZ', '02B60ZZ', '02B63ZZ', '02B64ZZ', '02B70ZK',
              '02B70ZZ', '02B73ZK', '02B73ZZ', '02B74ZK', '02B74ZZ', '02B80ZZ',
              '02B83ZZ', '02B84ZZ', '02B90ZZ', '02B93ZZ', '02B94ZZ', '02BD0ZZ',
              '02BD3ZZ', '02BD4ZZ', '02BF0ZZ', '02BF3ZZ', '02BF4ZZ', '02BG0ZZ',
              '02BG3ZZ', '02BG4ZZ', '02BH0ZZ', '02BH3ZZ', '02BH4ZZ', '02BJ0ZZ',
              '02BJ3ZZ', '02BJ4ZZ', '02BK0ZZ', '02BK3ZZ', '02BK4ZZ', '02BL0ZZ',
              '02BL3ZZ', '02BL4ZZ', '02BM0ZZ', '02BM3ZZ', '02BM4ZZ', '02BN0ZZ', 
              '02BN3ZZ', '02BN4ZZ', '02C00ZZ', '02C03ZZ', '02C04ZZ', '02C10ZZ',
              '02C13ZZ', '02C14ZZ', '02C20ZZ', '02C23ZZ', '02C24ZZ', '02C30ZZ',
              '02C33ZZ', '02C34ZZ', '02C40ZZ', '02C43ZZ', '02C44ZZ', '02C50ZZ',
              '02C53ZZ', '02C54ZZ', '02CD0ZZ', '02CD3ZZ', '02CD4ZZ', '02CF0ZZ',
              '02CF3ZZ', '02CF4ZZ', '02CG0ZZ', '02CG3ZZ', '02CG4ZZ', '02CH0ZZ', 
              '02CH3ZZ', '02CH4ZZ', '02CJ0ZZ', '02CJ3ZZ', '02CJ4ZZ', '02CM0ZZ', 
              '02CM3ZZ', '02CM4ZZ', '02H400Z', '02H402Z', '02H403Z', '02H40DZ',
              '02H40JZ', '02H40KZ', '02H40MZ', '02H430Z', '02H432Z', '02H433Z',
              '02H43DZ', '02H43JZ', '02H43KZ', '02H43MZ', '02H440Z', '02H442Z', 
              '02H443Z', '02H44DZ', '02H44JZ', '02H44KZ', '02H44MZ', '02H600Z',
              '02H60JZ', '02H60KZ', '02H60MZ', '02H630Z', '02H63JZ', '02H63KZ', 
              '02H63MZ', '02H640Z', '02H64JZ', '02H64KZ', '02H64MZ', '02H700Z',
              '02H70JZ', '02H70KZ', '02H70MZ', '02H730Z', '02H73JZ', '02H73KZ', 
              '02H73MZ', '02H740Z', '02H74JZ', '02H74KZ', '02H74MZ', '02HA0QZ',
              '02HA0RS', '02HA0RZ', '02HA3QZ', '02HA3RS', '02HA3RZ', '02HA4QZ',
              '02HA4RS', '02HA4RZ', '02HK00Z', '02HK02Z', '02HK0JZ', '02HK0KZ',
              '02HK0MZ', '02HK30Z', '02HK32Z', '02HK3JZ', '02HK3KZ', '02HK3MZ',
              '02HK40Z', '02HK42Z', '02HK4JZ', '02HK4KZ', '02HK4MZ', '02HL00Z',
              '02HL0JZ', '02HL0KZ', '02HL0MZ', '02HL30Z', '02HL3JZ', '02HL3KZ',
              '02HL3MZ', '02HL40Z', '02HL4JZ', '02HL4KZ', '02HL4MZ', '02HN0JZ',
              '02HN0KZ', '02HN0MZ', '02HN3JZ', '02HN3KZ', '02HN3MZ', '02HN4JZ',
              '02HN4KZ', '02HN4MZ', '02HS00Z', '02HS30Z', '02HS40Z', '02HT00Z', 
              '02HT30Z', '02HT40Z', '02HV00Z', '02HV30Z', '02HV40Z', '02L70CK',
              '02L70DK', '02L70ZK', '02L73CK', '02L73DK', '02L73ZK', '02L74CK',
              '02L74DK', '02L74ZK', '02LR0ZT', '02LS0ZZ', '02LT0ZZ', '02N50ZZ',
              '02N53ZZ', '02N54ZZ', '02N90ZZ', '02N93ZZ', '02N94ZZ', '02ND0ZZ',
              '02ND3ZZ', '02ND4ZZ', '02NF0ZZ', '02NF3ZZ', '02NF4ZZ', '02NG0ZZ',
              '02NG3ZZ', '02NG4ZZ', '02NH0ZZ', '02NH3ZZ', '02NH4ZZ', '02NJ0ZZ',
              '02NJ3ZZ', '02NJ4ZZ', '02NK0ZZ', '02NK3ZZ', '02NK4ZZ', '02NL0ZZ',
              '02NL3ZZ', '02NL4ZZ', '02NM0ZZ', '02NM3ZZ', '02NM4ZZ', '02PA0MZ',
              '02PA0QZ', '02PA0RZ', '02PA3MZ', '02PA3QZ', '02PA3RZ', '02PA4MZ',
              '02PA4QZ', '02PA4RZ', '02PAXMZ', '02Q00ZZ', '02Q03ZZ', '02Q04ZZ',
              '02Q10ZZ', '02Q13ZZ', '02Q14ZZ', '02Q20ZZ', '02Q23ZZ', '02Q24ZZ',
              '02Q30ZZ', '02Q33ZZ', '02Q34ZZ', '02Q40ZZ', '02Q43ZZ', '02Q44ZZ',
              '02Q50ZZ', '02Q53ZZ', '02Q54ZZ', '02Q70ZZ', '02Q73ZZ', '02Q74ZZ',
              '02Q90ZZ', '02Q93ZZ', '02Q94ZZ', '02QA0ZZ', '02QA3ZZ', '02QA4ZZ',
              '02QB0ZZ', '02QB3ZZ', '02QB4ZZ', '02QC0ZZ', '02QC3ZZ', '02QC4ZZ',
              '02QD0ZZ', '02QD3ZZ', '02QD4ZZ', '02QF0ZZ', '02QF3ZZ', '02QF4ZZ',
              '02QG0ZZ', '02QG3ZZ', '02QG4ZZ', '02QH0ZZ', '02QH3ZZ', '02QH4ZZ',
              '02QJ0ZZ', '02QJ3ZZ', '02QJ4ZZ', '02QM0ZZ', '02QM3ZZ', '02QM4ZZ',
              '02R907Z', '02R908Z', '02R90JZ', '02R90KZ', '02R947Z', '02R948Z',
              '02R94JZ', '02R94KZ', '02RD07Z', '02RD08Z', '02RD0JZ', '02RD0KZ',
              '02RD47Z', '02RD48Z', '02RD4JZ', '02RD4KZ', '02RF07Z', '02RF08Z', 
              '02RF0JZ', '02RF0KZ', '02RF37H', '02RF37Z', '02RF38H', '02RF38Z',
              '02RF3JH', '02RF3JZ', '02RF3KH', '02RF3KZ', '02RF47Z', '02RF48Z',
              '02RF4JZ', '02RF4KZ', '02RG07Z', '02RG08Z', '02RG0JZ', '02RG0KZ',
              '02RG37H', '02RG37Z', '02RG38H', '02RG38Z', '02RG3JH', '02RG3JZ',
              '02RG3KH', '02RG3KZ', '02RG47Z', '02RG48Z', '02RG4JZ', '02RG4KZ',
              '02RH07Z', '02RH08Z', '02RH0JZ', '02RH0KZ', '02RH37H', '02RH37Z',
              '02RH38H', '02RH38Z', '02RH3JH', '02RH3JZ', '02RH3KH', '02RH3KZ', 
              '02RH47Z', '02RH48Z', '02RH4JZ', '02RH4KZ', '02RJ07Z', '02RJ08Z',
              '02RJ0JZ', '02RJ0KZ', '02RJ47Z', '02RJ48Z', '02RJ4JZ', '02RJ4KZ',
              '02RK07Z', '02RK0JZ', '02RK0KZ', '02RK47Z', '02RK4KZ', '02RL07Z',
              '02RL0JZ', '02RL0KZ', '02RL47Z', '02RL4KZ', '02RM07Z', '02RM0JZ',
              '02RM0KZ', '02RM47Z', '02RM4JZ', '02RM4KZ', '02RP0JZ', '02RQ07Z',
              '02RQ0JZ', '02RR07Z', '02RR0JZ', '02SP0ZZ', '02SW0ZZ', '02T50ZZ',
              '02T53ZZ', '02T54ZZ', '02T80ZZ', '02T83ZZ', '02T84ZZ', '02T90ZZ',
              '02T93ZZ', '02T94ZZ', '02TD0ZZ', '02TD3ZZ', '02TD4ZZ', '02TH0ZZ',
              '02TH3ZZ', '02TH4ZZ', '02TM0ZZ', '02TM3ZZ', '02TM4ZZ', '02TN0ZZ',
              '02TN3ZZ', '02TN4ZZ', '02U507Z', '02U508Z', '02U50JZ', '02U50KZ',
              '02U537Z', '02U538Z', '02U53JZ', '02U53KZ', '02U547Z', '02U548Z',
              '02U54JZ', '02U54KZ', '02U607Z', '02U608Z', '02U60KZ', '02U707Z',
              '02U708Z', '02U70JZ', '02U70KZ', '02U737Z', '02U738Z', '02U73KZ',
              '02U747Z', '02U748Z', '02U74KZ', '02U907Z', '02U908Z', '02U90JZ', 
              '02U90KZ', '02U937Z', '02U938Z', '02U93JZ', '02U93KZ', '02U947Z',
              '02U948Z', '02U94JZ', '02U94KZ', '02UA0JZ', '02UA3JZ', '02UA4JZ',
              '02UD07Z', '02UD08Z', '02UD0JZ', '02UD0KZ', '02UD37Z', '02UD38Z',
              '02UD3JZ', '02UD3KZ', '02UD47Z', '02UD48Z', '02UD4JZ', '02UD4KZ',
              '02UF07Z', '02UF08Z', '02UF0JZ', '02UF0KZ', '02UF37Z', '02UF38Z',
              '02UF3JZ', '02UF3KZ', '02UF47Z', '02UF48Z', '02UF4JZ', '02UF4KZ', 
              '02UG07Z', '02UG08Z', '02UG0JZ', '02UG0KZ', '02UG37Z', '02UG38Z', 
              '02UG3JZ', '02UG3KZ', '02UG47Z', '02UG48Z', '02UG4JZ', '02UG4KZ',
              '02UH07Z', '02UH08Z', '02UH0JZ', '02UH0KZ', '02UH37Z', '02UH38Z',
              '02UH3JZ', '02UH3KZ', '02UH47Z', '02UH48Z', '02UH4JZ', '02UH4KZ',
              '02UJ07Z', '02UJ08Z', '02UJ0JZ', '02UJ0KZ', '02UJ37Z', '02UJ38Z',
              '02UJ3JZ', '02UJ3KZ', '02UJ47Z', '02UJ48Z', '02UJ4JZ', '02UJ4KZ',
              '02UK0KZ', '02UK3KZ', '02UK4KZ', '02UL0KZ', '02UL3KZ', '02UL4KZ', 
              '02UM07Z', '02UM0JZ', '02UM0KZ', '02UM37Z', '02UM38Z', '02UM3JZ',
              '02UM3KZ', '02UM47Z', '02UM48Z', '02UM4JZ', '02UM4KZ', '02VR0ZT',
              '02W50JZ', '02W54JZ', '02WA0JZ', '02WA0MZ', '02WA0QZ', '02WA0RZ',
              '02WA3MZ', '02WA3QZ', '02WA3RZ', '02WA4MZ', '02WA4QZ', '02WA4RZ',
              '02WF07Z', '02WF08Z', '02WF0JZ', '02WF0KZ', '02WF47Z', '02WF48Z',
              '02WF4JZ', '02WF4KZ', '02WG07Z', '02WG08Z', '02WG0JZ', '02WG0KZ',
              '02WG47Z', '02WG48Z', '02WG4JZ', '02WG4KZ', '02WH07Z', '02WH08Z',
              '02WH0JZ', '02WH0KZ', '02WH47Z', '02WH48Z', '02WH4JZ', '02WH4KZ',
              '02WJ07Z', '02WJ08Z', '02WJ0JZ', '02WJ0KZ', '02WJ47Z', '02WJ48Z',
              '02WJ4JZ', '02WJ4KZ', '02WM0JZ', '02WM4JZ', '02YA0Z0', '02YA0Z1',
              '02YA0Z2', '0JH600Z', '0JH604Z', '0JH605Z', '0JH606Z', '0JH607Z',
              '0JH608Z', '0JH609Z', '0JH60AZ', '0JH60PZ', '0JH630Z', '0JH634Z',
              '0JH635Z', '0JH636Z', '0JH637Z', '0JH638Z', '0JH639Z', '0JH63AZ',
              '0JH63PZ', '0JH800Z', '0JH804Z', '0JH805Z', '0JH806Z', '0JH807Z',
              '0JH808Z', '0JH809Z', '0JH80AZ', '0JH80PZ', '0JH830Z', '0JH834Z',
              '0JH835Z', '0JH836Z', '0JH837Z', '0JH838Z', '0JH839Z', '0JH83AZ',
              '0JH83PZ', '0JPT0PZ', '0JPT3PZ', '0JWT0PZ', '0JWT3PZ', '3E07017',
              '3E070PZ', '3E07317', '3E073PZ', '5A02110', '5A02116', '5A02210',
              '5A02216', '5A1213Z', '5A1223Z')
    
    
    keep08 <- data$dx1 %in% c('I0981', 'I110', 'I130', 'I132', 'I501', 'I5020', 
                              'I5021', 'I5022', 'I5023', '15030', 'I5031', 
                              'I5032', 'I5033', 'I5040', 'I5041', 'I5042',
                              'I5043', 'I509') & 
      (!data$px1 %in% exc8 & !data$px2 %in% exc8 & !data$px3 %in% exc8 & 
         !data$px4 %in% exc8 & !data$px5 %in% exc8 & !data$px6 %in% exc8 & 
         !data$px7 %in% exc8 & !data$px8 %in% exc8 & !data$px9 %in% exc8 & 
         !data$px10 %in% exc8) & data$age >= 18

    ##PQI 10##
    ##Include - Dehydration##
    
    inc10  <- c('E860', 'E861', 'E869')
    
    ##Include - Principal diagnosis for hyperosmolality 
    ##and/or hypernatremia, gastroenteritis, or acute kidney injury
    ##with secondary diagnosis for dehydration
    
    inc10.2 <- c('E870', 'A080', 'A0811', 'A0819', 'A082', 'A0831', 'A0832',
                 'A0839', 'A084', 'A088', 'A09', 'K5289', 'K529', 'N170', 'N171',
                 'N172', 'N178', 'N179', 'N19', 'N990')
    
    ##Exclusions - Chronic renal failure##
    exc10 <- c('I120', 'I1311', 'I132', 'N185', 'N186')
    
    A <- (data$dx1 %in% inc10)
    
    B <- (data$dx1 %in% inc10.2 & 
            (data$dx2 %in% inc10 | data$dx3 %in% inc10 | data$dx4 %in% inc10 |
               data$dx5 %in% inc10 | data$dx6 %in% inc10 | data$dx7 %in% inc10 | 
               data$dx8 %in% inc10 | data$dx9 %in% inc10 | data$dx10 %in% inc10))
    
    C <- (!data$dx1 %in% exc10 & !data$dx2 %in% exc10 & !data$dx3 %in% exc10 & 
            !data$dx4 %in% exc10 & !data$dx5 %in% exc10 & !data$dx6 %in% exc10 &
            !data$dx7 %in% exc10 & !data$dx8 %in% exc10 & !data$dx9 %in% exc10 &
            !data$dx10 %in% exc10)
    
    keep10 <- (A | B) & C & data$age >= 18
    
    ##PQI 11##
    ##Exclusions - Sickle Cell Anemia##
    
    exc11  <- c('D5700', 'D5701', 'D5702', 'D571', 'D5720', 'D57211', 'D57212',
                'D57219', 'D5740', 'D57411', 'D57412', 'D57419', 'D5780', 'D57811',
                'D57812', 'D57819')
    
    ##Exclusions - immunocompromised state diagnosis codes##
    exc11.2 <- c('B20', 'B59', 'C802', 'C888', 'C9440', 'C9441', 'C9442', 'C946',
                 'D4622', 'D471', 'D479', 'D47Z1', 'D47Z9', 'D6109', 'D61810',
                 'D61811', 'D61818', 'D700', 'D701', 'D702', 'D704', 'D708', 'D709',
                 'D71', 'D720', 'D72810', 'D72818', 'D72819', 'D7381', 'D7581', 
                 'D761', 'D762', 'D763', 'D800', 'D801', 'D802', 'D803', 'D804',
                 'D805', 'D806', 'D807', 'D808', 'D809', 'D810', 'D811', 'D812',
                 'D814', 'D816', 'D817', 'D8189', 'D819', 'D820', 'D821', 'D822',
                 'D823', 'D824', 'D828', 'D829', 'D830', 'D831', 'D832', 'D838',
                 'D839', 'D840', 'D841', 'D848', 'D849', 'D893', 'D89810', 'D89811',
                 'D89812', 'D89813', 'D8982', 'D8989', 'D899', 'E40', 'E41', 'E42',
                 'E43', 'I120', 'I1311', 'I132', 'K912', 'M359', 'N185', 'N186', 
                 'T8600', 'T8601', 'T8602', 'T8603', 'T8609', 'T8610', 'T8611',
                 'T8612', 'T8613', 'T8619', 'T8620', 'T8621', 'T8622', 'T8623',
                 'T86290', 'T86298', 'T8630', 'T8631', 'T8632', 'T8633', 'T8639',
                 'T8640', 'T8641', 'T8642', 'T8643', 'T8649', 'T865', 'T86810',
                 'T86811', 'T86812', 'T86818', 'T86819', 'T86830', 'T86831',
                 'T86832', 'T86838', 'T86839', 'T86850', 'T86851', 'T86852',
                 'T86858', 'T86859', 'T86890', 'T86891', 'T86892', 'T86898',
                 'T86899', 'T8690', 'T8691', 'T8692', 'T8693', 'T8699', 'Z4821',
                 'Z4822', 'Z4823', 'Z4824', 'Z48280', 'Z48290', 'Z48298', 'Z4901',
                 'Z4902', 'Z4931', 'Z940', 'Z941', 'Z942', 'Z943', 'Z944', 'Z9481',
                 'Z9482', 'Z9483', 'Z9484', 'Z9489', 'Z992')
    
    ##Exclusions - immunocompromised state procedure codes##
    
    exc11.3 <- c('02YA0Z0', '02YA0Z2', '0BYC0Z0', '0BYC0Z2', '0BYD0Z0', '0BYD0Z2',
                 '0BYF0Z0', '0BYF0Z2', '0BYG0Z0', '0BYG0Z2', '0BYH0Z0', '0BYH0Z2',
                 '0BYJ0Z0', '0BYJ0Z2', '0BYK0Z0', '0BYK0Z2', '0BYL0Z0', '0BYL0Z2',
                 '0BYM0Z0', '0BYM0Z2', '0FSG0ZZ', '0FSG4ZZ', '0FY00Z0', '0FY00Z2',
                 '0FYG0Z0', '0FYG0Z2', '0TY00Z0', '0TY00Z2', '0TY10Z0', '0TY10Z2',
                 '30230AZ', '30230G0', '30230G1', '30230X0', '30230X1', '30230Y0',
                 '30230Y1', '30233AZ', '30233G0', '30233G1', '30233X0', '30233X1',
                 '30233Y0', '30233Y1', '30240AZ', '30240G0', '30240G1', '30240X0',
                 '30240X1', '30240Y0', '30240Y1', '30243AZ', '30243G0', '30243G1',
                 '30243X0', '30243X1', '30243Y0', '30243Y1', '30250G0', '30250G1',
                 '30250X0', '30250X1', '30250Y0', '30250Y1', '30253G0', '30253G1',
                 '30253X0', '30253X1', '30253Y0', '30253Y1', '30260G0', '30260G1',
                 '30260X0', '30260X1', '30260Y0', '30260Y1', '30263G0', '30263G1',
                 '30263X0', '30263X1', '30263Y0', '30263Y1', '3E03005', '3E0300M',
                 '3E0J3U1', '3E0J7U1', '3E0J8U1', '3E030U1', '3E030WL', '3E03305',
                 '3E0330M', '3E033U1', '3E033WL', '3E04005', '3E0400M', '3E040WL',
                 '3E04305', '3E0430M', '3E043WL', '3E05005', '3E0500M', '3E050WL', 
                 '3E05305', '3E0530M', '3E053WL', '3E06005', '3E0600M', '3E060WL',
                 '3E06305', '3E0630M', '3E063WL')
    
    keep11 <- data$dx1 %in% c('J13', 'J14', 'J15211', 'J15212', 'J153', 'J154',
                              'J157', 'J159', 'J160', 'J168', 'J180', 'J181', 
                              'J188', 'J189') &
      (!data$dx1 %in% exc11 & !data$dx2 %in% exc11 & !data$dx3 %in% exc11 & 
         !data$dx4 %in% exc11 & !data$dx5 %in% exc11 & !data$dx6 %in% exc11 & 
         !data$dx7 %in% exc11 & !data$dx8 %in% exc11 & !data$dx9 %in% exc11 & 
         !data$dx10 %in% exc11) & 
      (!data$dx1 %in% exc11.2 & !data$dx2 %in% exc11.2 & !data$dx3 %in% exc11.2 & 
         !data$dx4 %in% exc11.2 & !data$dx5 %in% exc11.2 & !data$dx6 %in% exc11.2 &
         !data$dx7 %in% exc11.2 & !data$dx8 %in% exc11.2 & !data$dx9 %in% exc11.2 &
         !data$dx10 %in% exc11.2 ) & 
      (!data$px1 %in% exc11.3 & !data$px2 %in% exc11.3 & !data$px3 %in% exc11.3 &
         !data$px4 %in% exc11.3 & !data$px5 %in% exc11.3 & !data$px6 %in% exc11.3 &
         !data$px7 %in% exc11.3 & !data$px8 %in% exc11.3 & !data$px9 %in% exc11.3 &
         !data$px10 %in% exc11.3) & data$age >= 18
    
    ##PQI 12##
    ##Exclusions - Kidney/urinary tract disorder ##
    exc12  <- c('N110', 'N111', 'N118', 'N119', 'N1370', 'N1371', 'N13721', 'N13722',
                'N13729', 'N13731', 'N13732', 'N13739', 'N139', 'Q600', 'Q601', 
                'Q602', 'Q603', 'Q604', 'Q605', 'Q606', 'Q6100', 'Q6101', 'Q6102',
                'Q6111', 'Q6119', 'Q612', 'Q613', 'Q614', 'Q615', 'Q618', 'Q619', 
                'Q620', 'Q6210', 'Q6211', 'Q6212', 'Q622', 'Q6231', 'Q6232', 'Q6239',
                'Q624', 'Q625', 'Q6260', 'Q6261', 'Q6262', 'Q6263', 'Q6269', 'Q627',
                'Q628', 'Q630', 'Q631', 'Q632', 'Q633', 'Q638', 'Q639', 'Q6410', 
                'Q6411', 'Q6412', 'Q6419', 'Q642', 'Q6431', 'Q6432', 'Q6433', 'Q6439',
                'Q645', 'Q646', 'Q6470', 'Q6471', 'Q6472', 'Q6473', 'Q6474', 'Q6475',
                'Q6479', 'Q648', 'Q649')
    
    ##Exclusions - immunocompromised state diagnosis codes##
    exc12.2 <- c('B20', 'B59', 'C802', 'C888', 'C9440', 'C9441', 'C9442', 'C946',
                 'D4622', 'D471', 'D479', 'D47Z1', 'D47Z9', 'D6109', 'D61810',
                 'D61811', 'D61818', 'D700', 'D701', 'D702', 'D704', 'D708', 'D709',
                 'D71', 'D720', 'D72810', 'D72818', 'D72819', 'D7381', 'D7581', 
                 'D761', 'D762', 'D763', 'D800', 'D801', 'D802', 'D803', 'D804',
                 'D805', 'D806', 'D807', 'D808', 'D809', 'D810', 'D811', 'D812',
                 'D814', 'D816', 'D817', 'D8189', 'D819', 'D820', 'D821', 'D822',
                 'D823', 'D824', 'D828', 'D829', 'D830', 'D831', 'D832', 'D838',
                 'D839', 'D840', 'D841', 'D848', 'D849', 'D893', 'D89810', 'D89811',
                 'D89812', 'D89813', 'D8982', 'D8989', 'D899', 'E40', 'E41', 'E42',
                 'E43', 'I120', 'I1311', 'I132', 'K912', 'M359', 'N185', 'N186', 
                 'T8600', 'T8601', 'T8602', 'T8603', 'T8609', 'T8610', 'T8611',
                 'T8612', 'T8613', 'T8619', 'T8620', 'T8621', 'T8622', 'T8623',
                 'T86290', 'T86298', 'T8630', 'T8631', 'T8632', 'T8633', 'T8639',
                 'T8640', 'T8641', 'T8642', 'T8643', 'T8649', 'T865', 'T86810',
                 'T86811', 'T86812', 'T86818', 'T86819', 'T86830', 'T86831',
                 'T86832', 'T86838', 'T86839', 'T86850', 'T86851', 'T86852',
                 'T86858', 'T86859', 'T86890', 'T86891', 'T86892', 'T86898',
                 'T86899', 'T8690', 'T8691', 'T8692', 'T8693', 'T8699', 'Z4821',
                 'Z4822', 'Z4823', 'Z4824', 'Z48280', 'Z48290', 'Z48298', 'Z4901',
                 'Z4902', 'Z4931', 'Z940', 'Z941', 'Z942', 'Z943', 'Z944', 'Z9481',
                 'Z9482', 'Z9483', 'Z9484', 'Z9489', 'Z992')
    
    ##Exclusions - immunocompromised state procedure codes##
    
    exc12.3 <- c('02YA0Z0', '02YA0Z2', '0BYC0Z0', '0BYC0Z2', '0BYD0Z0', '0BYD0Z2',
                 '0BYF0Z0', '0BYF0Z2', '0BYG0Z0', '0BYG0Z2', '0BYH0Z0', '0BYH0Z2',
                 '0BYJ0Z0', '0BYJ0Z2', '0BYK0Z0', '0BYK0Z2', '0BYL0Z0', '0BYL0Z2',
                 '0BYM0Z0', '0BYM0Z2', '0FSG0ZZ', '0FSG4ZZ', '0FY00Z0', '0FY00Z2',
                 '0FYG0Z0', '0FYG0Z2', '0TY00Z0', '0TY00Z2', '0TY10Z0', '0TY10Z2',
                 '30230AZ', '30230G0', '30230G1', '30230X0', '30230X1', '30230Y0',
                 '30230Y1', '30233AZ', '30233G0', '30233G1', '30233X0', '30233X1',
                 '30233Y0', '30233Y1', '30240AZ', '30240G0', '30240G1', '30240X0',
                 '30240X1', '30240Y0', '30240Y1', '30243AZ', '30243G0', '30243G1',
                 '30243X0', '30243X1', '30243Y0', '30243Y1', '30250G0', '30250G1',
                 '30250X0', '30250X1', '30250Y0', '30250Y1', '30253G0', '30253G1',
                 '30253X0', '30253X1', '30253Y0', '30253Y1', '30260G0', '30260G1',
                 '30260X0', '30260X1', '30260Y0', '30260Y1', '30263G0', '30263G1',
                 '30263X0', '30263X1', '30263Y0', '30263Y1', '3E03005', '3E0300M',
                 '3E0J3U1', '3E0J7U1', '3E0J8U1', '3E030U1', '3E030WL', '3E03305',
                 '3E0330M', '3E033U1', '3E033WL', '3E04005', '3E0400M', '3E040WL',
                 '3E04305', '3E0430M', '3E043WL', '3E05005', '3E0500M', '3E050WL', 
                 '3E05305', '3E0530M', '3E053WL', '3E06005', '3E0600M', '3E060WL',
                 '3E06305', '3E0630M', '3E063WL')
    keep12 <- data$dx1 %in% c('N10', 'N119', 'N12', 'N151', 'N159', 'N16', 'N2884',
                              'N2885', 'N2886', 'N3000', 'N3001', 'N3090', 'N3091',
                              'N390') &
      (!data$dx1 %in% exc12 & !data$dx2 %in% exc12 & !data$dx3 %in% exc12 & 
         !data$dx4 %in% exc12 & !data$dx5 %in% exc12 & !data$dx6 %in% exc12 & 
         !data$dx7 %in% exc12 & !data$dx8 %in% exc12 & !data$dx9 %in% exc12 & 
         !data$dx10 %in% exc12) & 
      (!data$dx1 %in% exc12.2 & !data$dx2 %in% exc12.2 & !data$dx3 %in% exc12.2 &
         !data$dx4 %in% exc12.2 & !data$dx5 %in% exc12.2 & !data$dx6 %in% exc12.2 &
         !data$dx7 %in% exc12.2 & !data$dx8 %in% exc12.2 & !data$dx9 %in% exc12.2 &
         !data$dx10 %in% exc12.2 ) & 
      (!data$px1 %in% exc12.3 & !data$px2 %in% exc12.3 & !data$px3 %in% exc12.3 &
         !data$px4 %in% exc12.3 & !data$px5 %in% exc12.3 & !data$px6 %in% exc12.3 &
         !data$px7 %in% exc12.3 & !data$px8 %in% exc12.3 & !data$px9 %in% exc12.3 &
         !data$px10 %in% exc12.3) & data$age >= 18
  
    ##PQI 14##
    keep14 <- data$dx1 %in% c('E1065', 'E1165', 'E10649', 'E11649') & data$age >= 18
    
    ##PQI 15##
    ##Exclusions - Cystic fibrosis and anamolies of the respiratory system ##
    
    exc15 <- c('E840', 'E8411', 'E8419', 'E848', 'E849', 'J8483', 'J84841', 'J84842',
               'J84843', 'J84848', 'P270', 'P271', 'P278', 'P279', 'Q254', 'Q311',
               'Q312', 'Q313', 'Q315', 'Q318', 'Q319', 'Q320', 'Q321', 'Q322', 'Q323',
               'Q324', 'Q330', 'Q331', 'Q332', 'Q333', 'Q334', 'Q335', 'Q336', 'Q338',
               'Q339', 'Q340', 'Q341', 'Q348', 'Q349', 'Q390', 'Q391', 'Q392', 'Q393',
               'Q394', 'Q893')
    
    keep15 <- data$dx1 %in% c('J4521', 'J4522', 'J4531', 'J4532', 'J4541', 'J4542',
                              'J4551', 'J4552', 'J45901', 'J45902', 'J45990', 
                              'J45991', 'J45998') &
      (!data$dx1 %in% exc15 & !data$dx2 %in% exc15 & !data$dx3 %in% exc15 & 
         !data$dx4 %in% exc15 & !data$dx5 %in% exc15 & !data$dx6 %in% exc15 & 
         !data$dx7 %in% exc15 & !data$dx8 %in% exc15 & !data$dx9 %in% exc15 & 
         !data$dx10 %in% exc15) & data$age >= 18 & data$age <= 39
    
    ##PQI 16##
    #Include - Amputation##
    
    inc16 <- c('0Y620ZZ', '0Y630ZZ', '0Y640ZZ', '0Y670ZZ', '0Y680ZZ', '0Y6C0Z1',
               '0Y6C0Z2', '0Y6C0Z3', '0Y6D0Z1', '0Y6D0Z2', '0Y6D0Z3', '0Y6F0ZZ',
               '0Y6G0ZZ', '0Y6H0Z1', '0Y6H0Z2', '0Y6H0Z3', '0Y6J0Z1', '0Y6J0Z2',
               '0Y6J0Z3', '0Y6M0Z0', '0Y6M0Z4', '0Y6M0Z5', '0Y6M0Z6', '0Y6M0Z7',
               '0Y6M0Z8', '0Y6M0Z9', '0Y6M0ZB', '0Y6M0ZC', '0Y6M0ZD', '0Y6M0ZF',
               '0Y6N0Z0', '0Y6N0Z4', '0Y6N0Z5', '0Y6N0Z6', '0Y6N0Z7', '0Y6N0Z8',
               '0Y6N0Z9', '0Y6N0ZB', '0Y6N0ZC', '0Y6N0ZD', '0Y6N0ZF')
    
    ##Include Diabetes##
    
    inc16.2 <- c('E1010', 'E1011', 'E1021', 'E1022', 'E1029', 'E10311', 'E10319',
                 'E10321', 'E10329', 'E10331', 'E10339', 'E10341', 'E10349', 
                 'E10351', 'E10359', 'E1036', 'E1039', 'E1040', 'E1041', 'E1042',
                 'E1043', 'E1044', 'E1049', 'E1051', 'E1052', 'E1059', 'E10610',
                 'E10618', 'E10620', 'E10621', 'E10622', 'E10628', 'E10630', 
                 'E10638', 'E10641', 'E10649', 'E1065', 'E1069', 'E108', 'E109',
                 'E1100', 'E1101', 'E1121', 'E1122', 'E1129', 'E11311', 'E11319',
                 'E11321', 'E11329', 'E11331', 'E11339', 'E11341', 'E11349', 
                 'E11351', 'E11359', 'E1136', 'E1139', 'E1140', 'E1141', 'E1142',
                 'E1143', 'E1144', 'E1149', 'E1151', 'E1152', 'E1159', 'E11610',
                 'E11618', 'E11620', 'E11621', 'E11622', 'E11628', 'E11630', 
                 'E11638', 'E11641', 'E11649', 'E1165', 'E1169', 'E118', 'E119',
                 'E1300', 'E1301', 'E1310', 'E1311', 'E1321', 'E1322', 'E1329',
                 'E13311', 'E13319', 'E13321', 'E13329', 'E13331', 'E13339',
                 'E13341', 'E13349', 'E13351', 'E13359', 'E1336', 'E1339', 'E1340',
                 'E1341', 'E1342', 'E1343', 'E1344', 'E1349', 'E1351', 'E1352', 
                 'E1359', 'E13610', 'E13618', 'E13620', 'E13621', 'E13622','E13628',
                 'E13630', 'E13638', 'E13641', 'E13649', 'E1365', 'E1369', 'E138', 
                 'E139')
    
    #exclusions
    exc16 <- c('S78011A', 'S78012A', 'S78019A', 'S78021A', 'S78022A', 'S78029A',
               'S78111A', 'S78112A', 'S78119A', 'S78121A', 'S78122A', 'S78129A',
               'S78911A', 'S78912A', 'S78919A', 'S78921A', 'S78922A', 'S78929A',
               'S88011A', 'S88012A', 'S88019A', 'S88021A', 'S88022A', 'S88029A', 
               'S88111A', 'S88112A', 'S88119A', 'S88121A', 'S88122A', 'S88129A',
               'S88911A', 'S88912A', 'S88919A', 'S88921A', 'S88922A', 'S88929A',
               'S98011A', 'S98012A', 'S98019A', 'S98021A', 'S98022A', 'S98029A',
               'S98111A', 'S98112A', 'S98119A', 'S98121A', 'S98122A', 'S98129A',
               'S98131A', 'S98132A', 'S98139A', 'S98141A', 'S98142A', 'S98149A',
               'S98211A', 'S98212A', 'S98219A', 'S98221A', 'S98222A', 'S98229A',
               'S98311A', 'S98312A', 'S98319A', 'S98321A', 'S98322A', 'S98329A',
               'S98911A', 'S98912A', 'S98919A', 'S98921A', 'S98922A', 'S98929A')
    
    keep16 <- ((data$px1 %in% inc16 | data$px2 %in% inc16 | data$px3 %in% inc16 |
                  data$px4 %in% inc16 | data$px5 %in% inc16 | data$px6 %in% inc16 |
                  data$px7 %in% inc16 | data$px8 %in% inc16 | data$px9 %in% inc16 |
                  data$px10 %in% inc16) & 
                 (data$dx1 %in% inc16.2 | data$dx2 %in% inc16.2 | 
                    data$dx3 %in% inc16.2 | data$dx4 %in% inc16.2 | 
                    data$dx5 %in% inc16.2 | data$dx6 %in% inc16.2 |
                    data$dx7 %in% inc16.2 | data$dx8 %in% inc16.2 | 
                    data$dx9 %in% inc16.2 | data$dx10 %in% inc16.2)) &
      (!data$dx1 %in% exc16 & !data$dx2 %in% exc16 & !data$dx3 %in% exc16 &
         !data$dx4 %in% exc16 & !data$dx5 %in% exc16 & !data$dx6 %in% exc16 &
         !data$dx7 %in% exc16 & !data$dx8 %in% exc16 & !data$dx9 %in% exc16 &
         !data$dx10 %in% exc16) & data$age >= 18 & data$mdc != "14"
    
    keep90 <- keep01 | keep03 | keep05 | keep07 | keep08 | keep10 | keep11 | keep12 |
      keep14 | keep15 | keep16
    
    PQI.90 <- ifelse(keep90, 1, 0)
  } else {
    PQI.90 <- NULL
  }
  ################################################
  #PQI 91 : Prevention Quality Acute Composite 
  ################################################
  if ("PQI.91" %in% pqi.match & ICD == "9") {
  ##Includes PQI: 10, 11, 12
    
  ##PQI 10##  
  ##Include - Dehydration##
    
  inc10  <- c('2765', '27650', '27651', '27652')
    
  ##Include - Principal diagnosis for hyperosmolality 
  ##and/or hypernatremia, gastroenteritis, or acute kidney injury
  ##with secondary diagnosis for dehydration
    
  inc10.2 <- c('2760', '00861', '00862', '00863', '00864', '00865', '00866',
               '00867', '00869', '0088', '0090', '0091', '0092', '0093', 
               '5589', '5845', '5846', '5847', '5848', '5849', '586', '9975')
    
  ##Exclusions - Chronic renal failure##
    
  exc10 <- c('40301', '40311', '40391', '40402', '40403', '40412', '40413',
             '40492', '40493', '5855', '5856')
    
  A <- (data$dx1 %in% inc10)
    
  B <- (data$dx1 %in% inc10.2 & 
        (data$dx2 %in% inc10 | data$dx3 %in% inc10 | data$dx4 %in% inc10 |
         data$dx5 %in% inc10 | data$dx6 %in% inc10 | data$dx7 %in% inc10 | 
         data$dx8 %in% inc10 | data$dx9 %in% inc10 | data$dx10 %in% inc10))
    
  C <- (!data$dx1 %in% exc10 & !data$dx2 %in% exc10 & !data$dx3 %in% exc10 & 
        !data$dx4 %in% exc10 & !data$dx5 %in% exc10 & !data$dx6 %in% exc10 &
        !data$dx7 %in% exc10 & !data$dx8 %in% exc10 & !data$dx9 %in% exc10 &
        !data$dx10 %in% exc10)
    
  keep10 <- (A | B) & C & data$age >= 18
    
  ##PQI 11##
  ##Exclusions - Sickle Cell Anemia##
    
  exc11  <- c('28241', '28242', '28260', '28261', '28262', '28263', '28264',
              '28268', '28269') 
    
  ##Exclusions - immunocompromised state diagnosis codes##
    
  exc11.2 <- c('042', '1363', '1992', '23873', '23876', '23877', '23879', 
               '260', '261', '262', '27900', '27901', '27902', '27903', 
               '27904', '27905', '27906', '27909', '27910', '27911', '27912',
               '27913', '27919', '2792', '2793', '2794', '27941', '27949', 
               '27950', '27951', '27952', '27953', '2798', '2799', '28409', 
               '2841', '28411', '28412', '28419', '2880', '28800', '28801',
               '28802', '28803', '28809', '2881', '2882', '2884', '28850', 
               '28851', '28859', '28953', '28983', '40301', '40311', '40391',
               '40402', '40403', '40412', '40413', '40492', '40493', '5793',
               '585', '5855', '5856', '9968', '99680', '99681', '99682', 
               '99683', '99684', '99685', '99686', '99687', '99688', '99689',
               'V420', 'V421', 'V426', 'V427', 'V428', 'V4281', 'V4282', 
               'V4283', 'V4284', 'V4289', 'V451', 'V4511', 'V560', 'V561', 
               'V562')
    
  ##Exclusions - immunocompromised state procedure codes##
    
  exc11.3 <- c('0018', '335', '336', '375', '3350', '3751', '3351', '410', 
               '3352', '4100', '5051', '4101', '4102', '5059', '5280', '4103',
               '5281', '4104', '4105', '4106', '5282', '5283', '5285', '4107', 
               '5286', '4108', '5569', '4109', '4697')#add 4697
    
  keep11 <- data$dx1 %in% c('481', '4822', '48230', '48231', '48232', '48239',
                            '48241', '48242', '4829', '4830', '4831', '4838',
                            '485', '486', '48249') & #add 48249
    (!data$dx1 %in% exc11 & !data$dx2 %in% exc11 & !data$dx3 %in% exc11 & 
     !data$dx4 %in% exc11 & !data$dx5 %in% exc11 & !data$dx6 %in% exc11 & 
     !data$dx7 %in% exc11 & !data$dx8 %in% exc11 & !data$dx9 %in% exc11 & 
     !data$dx10 %in% exc11) & 
    (!data$dx1 %in% exc11.2 & !data$dx2 %in% exc11.2 & !data$dx3 %in% exc11.2 & 
     !data$dx4 %in% exc11.2 & !data$dx5 %in% exc11.2 & !data$dx6 %in% exc11.2 &
     !data$dx7 %in% exc11.2 & !data$dx8 %in% exc11.2 & !data$dx9 %in% exc11.2 &
     !data$dx10 %in% exc11.2 ) & 
    (!data$px1 %in% exc11.3 & !data$px2 %in% exc11.3 & !data$px3 %in% exc11.3 &
     !data$px4 %in% exc11.3 & !data$px5 %in% exc11.3 & !data$px6 %in% exc11.3 &
     !data$px7 %in% exc11.3 & !data$px8 %in% exc11.3 & !data$px9 %in% exc11.3 &
     !data$px10 %in% exc11.3) & data$age >= 18
    
  ##PQI 12##
  ##Exclusions - Kidney/urinary tract disorder ##
    
  exc12  <- c('59000', '59001', '59370', '59371', '59372', '59373', '7530', 
              '75310', '75311', '75312', '75313', '75314', '75315', '75316',
              '75317', '75319', '75320', '75321', '75322', '75323', '75329',
              '7533', '7534', '7535', '7536', '7538', '7539') 
    
  ##Exclusions - immunocompromised state diagnosis codes##
    
  exc12.2 <- c('042', '1363', '1992', '23873', '23876', '23877', '23879', 
               '260','261', '262', '27900', '27901', '27902', '27903', 
               '27904', '27905', '27906', '27909', '27910', '27911', '27912',
               '27913', '27919', '2792', '2793', '2794', '27941', '27949', 
               '27950', '27951', '27952', '27953', '2798', '2799', '28409', 
               '2841', '28411', '28412', '28419', '2880', '28800', '28801', 
               '28802', '28803', '28809', '2881', '2882', '2884', '28850', 
               '28851', '28859','28953', '28983', '40301', '40311', '40391', 
               '40402', '40403', '40412', '40413', '40492', '40493', '5793', 
               '585', '5855', '5856', '9968', '99680', '99681', '99682', 
               '99683', '99684', '99685','99686', '99687', '99688', '99689', 
               'V420', 'V421', 'V426', 'V427', 'V428', 'V4281', 'V4282', 
               'V4283', 'V4284', 'V4289', 'V451','V4511', 'V560', 'V561', 
               'V562')
    
  ##Exclusions - immunocompromised state procedure codes##
    
  exc12.3 <- c('0018', '335', '336', '375', '3350', '3751', '3351', '410', 
               '3352', '4100', '5051', '4101', '4102', '5059', '5280', '4103', 
               '5281', '4104', '4105', '4106', '5282', '5283', '5285', '4107', 
               '5286', '4108', '5569', '4109', '4697')#add 4697
    
  keep12 <- data$dx1 %in% c('59010', '59011', '5902', '5903', '59080', '59081', 
                            '5909', '5950', '5959', '5990') & 
    (!data$dx1 %in% exc12 & !data$dx2 %in% exc12 & !data$dx3 %in% exc12 & 
     !data$dx4 %in% exc12 & !data$dx5 %in% exc12 & !data$dx6 %in% exc12 & 
     !data$dx7 %in% exc12 & !data$dx8 %in% exc12 & !data$dx9 %in% exc12 & 
     !data$dx10 %in% exc12) & 
    (!data$dx1 %in% exc12.2 & !data$dx2 %in% exc12.2 & !data$dx3 %in% exc12.2 &
     !data$dx4 %in% exc12.2 & !data$dx5 %in% exc12.2 & !data$dx6 %in% exc12.2 &
     !data$dx7 %in% exc12.2 & !data$dx8 %in% exc12.2 & !data$dx9 %in% exc12.2 &
     !data$dx10 %in% exc12.2 ) & 
    (!data$px1 %in% exc12.3 & !data$px2 %in% exc12.3 & !data$px3 %in% exc12.3 &
     !data$px4 %in% exc12.3 & !data$px5 %in% exc12.3 & !data$px6 %in% exc12.3 &
     !data$px7 %in% exc12.3 & !data$px8 %in% exc12.3 & !data$px9 %in% exc12.3 &
     !data$px10 %in% exc12.3) & data$age >= 18
  
  keep91 <- keep10 | keep11 | keep12 
  
  PQI.91 <- ifelse(keep91, 1, 0)
  } else if ("PQI.91" %in% pqi.match & ICD == "10"){
    ##Includes PQI: 10, 11, 12
    
    ##PQI 10##
    ##Include - Dehydration##
    
    inc10  <- c('E860', 'E861', 'E869')
    
    ##Include - Principal diagnosis for hyperosmolality 
    ##and/or hypernatremia, gastroenteritis, or acute kidney injury
    ##with secondary diagnosis for dehydration
    
    inc10.2 <- c('E870', 'A080', 'A0811', 'A0819', 'A082', 'A0831', 'A0832',
                 'A0839', 'A084', 'A088', 'A09', 'K5289', 'K529', 'N170', 'N171',
                 'N172', 'N178', 'N179', 'N19', 'N990')
    
    ##Exclusions - Chronic renal failure##
    exc10 <- c('I120', 'I1311', 'I132', 'N185', 'N186')
    
    A <- (data$dx1 %in% inc10)
    
    B <- (data$dx1 %in% inc10.2 & 
            (data$dx2 %in% inc10 | data$dx3 %in% inc10 | data$dx4 %in% inc10 |
               data$dx5 %in% inc10 | data$dx6 %in% inc10 | data$dx7 %in% inc10 | 
               data$dx8 %in% inc10 | data$dx9 %in% inc10 | data$dx10 %in% inc10))
    
    C <- (!data$dx1 %in% exc10 & !data$dx2 %in% exc10 & !data$dx3 %in% exc10 & 
            !data$dx4 %in% exc10 & !data$dx5 %in% exc10 & !data$dx6 %in% exc10 &
            !data$dx7 %in% exc10 & !data$dx8 %in% exc10 & !data$dx9 %in% exc10 &
            !data$dx10 %in% exc10)
    
    keep10 <- (A | B) & C & data$age >= 18
    
    ##PQI 11##
    ##Exclusions - Sickle Cell Anemia##
    
    exc11  <- c('D5700', 'D5701', 'D5702', 'D571', 'D5720', 'D57211', 'D57212',
                'D57219', 'D5740', 'D57411', 'D57412', 'D57419', 'D5780', 'D57811',
                'D57812', 'D57819')
    
    ##Exclusions - immunocompromised state diagnosis codes##
    exc11.2 <- c('B20', 'B59', 'C802', 'C888', 'C9440', 'C9441', 'C9442', 'C946',
                 'D4622', 'D471', 'D479', 'D47Z1', 'D47Z9', 'D6109', 'D61810',
                 'D61811', 'D61818', 'D700', 'D701', 'D702', 'D704', 'D708', 'D709',
                 'D71', 'D720', 'D72810', 'D72818', 'D72819', 'D7381', 'D7581', 
                 'D761', 'D762', 'D763', 'D800', 'D801', 'D802', 'D803', 'D804',
                 'D805', 'D806', 'D807', 'D808', 'D809', 'D810', 'D811', 'D812',
                 'D814', 'D816', 'D817', 'D8189', 'D819', 'D820', 'D821', 'D822',
                 'D823', 'D824', 'D828', 'D829', 'D830', 'D831', 'D832', 'D838',
                 'D839', 'D840', 'D841', 'D848', 'D849', 'D893', 'D89810', 'D89811',
                 'D89812', 'D89813', 'D8982', 'D8989', 'D899', 'E40', 'E41', 'E42',
                 'E43', 'I120', 'I1311', 'I132', 'K912', 'M359', 'N185', 'N186', 
                 'T8600', 'T8601', 'T8602', 'T8603', 'T8609', 'T8610', 'T8611',
                 'T8612', 'T8613', 'T8619', 'T8620', 'T8621', 'T8622', 'T8623',
                 'T86290', 'T86298', 'T8630', 'T8631', 'T8632', 'T8633', 'T8639',
                 'T8640', 'T8641', 'T8642', 'T8643', 'T8649', 'T865', 'T86810',
                 'T86811', 'T86812', 'T86818', 'T86819', 'T86830', 'T86831',
                 'T86832', 'T86838', 'T86839', 'T86850', 'T86851', 'T86852',
                 'T86858', 'T86859', 'T86890', 'T86891', 'T86892', 'T86898',
                 'T86899', 'T8690', 'T8691', 'T8692', 'T8693', 'T8699', 'Z4821',
                 'Z4822', 'Z4823', 'Z4824', 'Z48280', 'Z48290', 'Z48298', 'Z4901',
                 'Z4902', 'Z4931', 'Z940', 'Z941', 'Z942', 'Z943', 'Z944', 'Z9481',
                 'Z9482', 'Z9483', 'Z9484', 'Z9489', 'Z992')
    
    ##Exclusions - immunocompromised state procedure codes##
    
    exc11.3 <- c('02YA0Z0', '02YA0Z2', '0BYC0Z0', '0BYC0Z2', '0BYD0Z0', '0BYD0Z2',
                 '0BYF0Z0', '0BYF0Z2', '0BYG0Z0', '0BYG0Z2', '0BYH0Z0', '0BYH0Z2',
                 '0BYJ0Z0', '0BYJ0Z2', '0BYK0Z0', '0BYK0Z2', '0BYL0Z0', '0BYL0Z2',
                 '0BYM0Z0', '0BYM0Z2', '0FSG0ZZ', '0FSG4ZZ', '0FY00Z0', '0FY00Z2',
                 '0FYG0Z0', '0FYG0Z2', '0TY00Z0', '0TY00Z2', '0TY10Z0', '0TY10Z2',
                 '30230AZ', '30230G0', '30230G1', '30230X0', '30230X1', '30230Y0',
                 '30230Y1', '30233AZ', '30233G0', '30233G1', '30233X0', '30233X1',
                 '30233Y0', '30233Y1', '30240AZ', '30240G0', '30240G1', '30240X0',
                 '30240X1', '30240Y0', '30240Y1', '30243AZ', '30243G0', '30243G1',
                 '30243X0', '30243X1', '30243Y0', '30243Y1', '30250G0', '30250G1',
                 '30250X0', '30250X1', '30250Y0', '30250Y1', '30253G0', '30253G1',
                 '30253X0', '30253X1', '30253Y0', '30253Y1', '30260G0', '30260G1',
                 '30260X0', '30260X1', '30260Y0', '30260Y1', '30263G0', '30263G1',
                 '30263X0', '30263X1', '30263Y0', '30263Y1', '3E03005', '3E0300M',
                 '3E0J3U1', '3E0J7U1', '3E0J8U1', '3E030U1', '3E030WL', '3E03305',
                 '3E0330M', '3E033U1', '3E033WL', '3E04005', '3E0400M', '3E040WL',
                 '3E04305', '3E0430M', '3E043WL', '3E05005', '3E0500M', '3E050WL', 
                 '3E05305', '3E0530M', '3E053WL', '3E06005', '3E0600M', '3E060WL',
                 '3E06305', '3E0630M', '3E063WL')
    
    keep11 <- data$dx1 %in% c('J13', 'J14', 'J15211', 'J15212', 'J153', 'J154',
                              'J157', 'J159', 'J160', 'J168', 'J180', 'J181', 
                              'J188', 'J189') &
      (!data$dx1 %in% exc11 & !data$dx2 %in% exc11 & !data$dx3 %in% exc11 & 
         !data$dx4 %in% exc11 & !data$dx5 %in% exc11 & !data$dx6 %in% exc11 & 
         !data$dx7 %in% exc11 & !data$dx8 %in% exc11 & !data$dx9 %in% exc11 & 
         !data$dx10 %in% exc11) & 
      (!data$dx1 %in% exc11.2 & !data$dx2 %in% exc11.2 & !data$dx3 %in% exc11.2 & 
         !data$dx4 %in% exc11.2 & !data$dx5 %in% exc11.2 & !data$dx6 %in% exc11.2 &
         !data$dx7 %in% exc11.2 & !data$dx8 %in% exc11.2 & !data$dx9 %in% exc11.2 &
         !data$dx10 %in% exc11.2 ) & 
      (!data$px1 %in% exc11.3 & !data$px2 %in% exc11.3 & !data$px3 %in% exc11.3 &
         !data$px4 %in% exc11.3 & !data$px5 %in% exc11.3 & !data$px6 %in% exc11.3 &
         !data$px7 %in% exc11.3 & !data$px8 %in% exc11.3 & !data$px9 %in% exc11.3 &
         !data$px10 %in% exc11.3) & data$age >= 18
    
    ##PQI 12##
    ##Exclusions - Kidney/urinary tract disorder ##
    exc12  <- c('N110', 'N111', 'N118', 'N119', 'N1370', 'N1371', 'N13721', 'N13722',
                'N13729', 'N13731', 'N13732', 'N13739', 'N139', 'Q600', 'Q601', 
                'Q602', 'Q603', 'Q604', 'Q605', 'Q606', 'Q6100', 'Q6101', 'Q6102',
                'Q6111', 'Q6119', 'Q612', 'Q613', 'Q614', 'Q615', 'Q618', 'Q619', 
                'Q620', 'Q6210', 'Q6211', 'Q6212', 'Q622', 'Q6231', 'Q6232', 'Q6239',
                'Q624', 'Q625', 'Q6260', 'Q6261', 'Q6262', 'Q6263', 'Q6269', 'Q627',
                'Q628', 'Q630', 'Q631', 'Q632', 'Q633', 'Q638', 'Q639', 'Q6410', 
                'Q6411', 'Q6412', 'Q6419', 'Q642', 'Q6431', 'Q6432', 'Q6433', 'Q6439',
                'Q645', 'Q646', 'Q6470', 'Q6471', 'Q6472', 'Q6473', 'Q6474', 'Q6475',
                'Q6479', 'Q648', 'Q649')
    
    ##Exclusions - immunocompromised state diagnosis codes##
    exc12.2 <- c('B20', 'B59', 'C802', 'C888', 'C9440', 'C9441', 'C9442', 'C946',
                 'D4622', 'D471', 'D479', 'D47Z1', 'D47Z9', 'D6109', 'D61810',
                 'D61811', 'D61818', 'D700', 'D701', 'D702', 'D704', 'D708', 'D709',
                 'D71', 'D720', 'D72810', 'D72818', 'D72819', 'D7381', 'D7581', 
                 'D761', 'D762', 'D763', 'D800', 'D801', 'D802', 'D803', 'D804',
                 'D805', 'D806', 'D807', 'D808', 'D809', 'D810', 'D811', 'D812',
                 'D814', 'D816', 'D817', 'D8189', 'D819', 'D820', 'D821', 'D822',
                 'D823', 'D824', 'D828', 'D829', 'D830', 'D831', 'D832', 'D838',
                 'D839', 'D840', 'D841', 'D848', 'D849', 'D893', 'D89810', 'D89811',
                 'D89812', 'D89813', 'D8982', 'D8989', 'D899', 'E40', 'E41', 'E42',
                 'E43', 'I120', 'I1311', 'I132', 'K912', 'M359', 'N185', 'N186', 
                 'T8600', 'T8601', 'T8602', 'T8603', 'T8609', 'T8610', 'T8611',
                 'T8612', 'T8613', 'T8619', 'T8620', 'T8621', 'T8622', 'T8623',
                 'T86290', 'T86298', 'T8630', 'T8631', 'T8632', 'T8633', 'T8639',
                 'T8640', 'T8641', 'T8642', 'T8643', 'T8649', 'T865', 'T86810',
                 'T86811', 'T86812', 'T86818', 'T86819', 'T86830', 'T86831',
                 'T86832', 'T86838', 'T86839', 'T86850', 'T86851', 'T86852',
                 'T86858', 'T86859', 'T86890', 'T86891', 'T86892', 'T86898',
                 'T86899', 'T8690', 'T8691', 'T8692', 'T8693', 'T8699', 'Z4821',
                 'Z4822', 'Z4823', 'Z4824', 'Z48280', 'Z48290', 'Z48298', 'Z4901',
                 'Z4902', 'Z4931', 'Z940', 'Z941', 'Z942', 'Z943', 'Z944', 'Z9481',
                 'Z9482', 'Z9483', 'Z9484', 'Z9489', 'Z992')
    
    ##Exclusions - immunocompromised state procedure codes##
    
    exc12.3 <- c('02YA0Z0', '02YA0Z2', '0BYC0Z0', '0BYC0Z2', '0BYD0Z0', '0BYD0Z2',
                 '0BYF0Z0', '0BYF0Z2', '0BYG0Z0', '0BYG0Z2', '0BYH0Z0', '0BYH0Z2',
                 '0BYJ0Z0', '0BYJ0Z2', '0BYK0Z0', '0BYK0Z2', '0BYL0Z0', '0BYL0Z2',
                 '0BYM0Z0', '0BYM0Z2', '0FSG0ZZ', '0FSG4ZZ', '0FY00Z0', '0FY00Z2',
                 '0FYG0Z0', '0FYG0Z2', '0TY00Z0', '0TY00Z2', '0TY10Z0', '0TY10Z2',
                 '30230AZ', '30230G0', '30230G1', '30230X0', '30230X1', '30230Y0',
                 '30230Y1', '30233AZ', '30233G0', '30233G1', '30233X0', '30233X1',
                 '30233Y0', '30233Y1', '30240AZ', '30240G0', '30240G1', '30240X0',
                 '30240X1', '30240Y0', '30240Y1', '30243AZ', '30243G0', '30243G1',
                 '30243X0', '30243X1', '30243Y0', '30243Y1', '30250G0', '30250G1',
                 '30250X0', '30250X1', '30250Y0', '30250Y1', '30253G0', '30253G1',
                 '30253X0', '30253X1', '30253Y0', '30253Y1', '30260G0', '30260G1',
                 '30260X0', '30260X1', '30260Y0', '30260Y1', '30263G0', '30263G1',
                 '30263X0', '30263X1', '30263Y0', '30263Y1', '3E03005', '3E0300M',
                 '3E0J3U1', '3E0J7U1', '3E0J8U1', '3E030U1', '3E030WL', '3E03305',
                 '3E0330M', '3E033U1', '3E033WL', '3E04005', '3E0400M', '3E040WL',
                 '3E04305', '3E0430M', '3E043WL', '3E05005', '3E0500M', '3E050WL', 
                 '3E05305', '3E0530M', '3E053WL', '3E06005', '3E0600M', '3E060WL',
                 '3E06305', '3E0630M', '3E063WL')
    
    keep12 <- data$dx1 %in% c('N10', 'N119', 'N12', 'N151', 'N159', 'N16', 'N2884',
                              'N2885', 'N2886', 'N3000', 'N3001', 'N3090', 'N3091',
                              'N390') &
      (!data$dx1 %in% exc12 & !data$dx2 %in% exc12 & !data$dx3 %in% exc12 & 
         !data$dx4 %in% exc12 & !data$dx5 %in% exc12 & !data$dx6 %in% exc12 & 
         !data$dx7 %in% exc12 & !data$dx8 %in% exc12 & !data$dx9 %in% exc12 & 
         !data$dx10 %in% exc12) & 
      (!data$dx1 %in% exc12.2 & !data$dx2 %in% exc12.2 & !data$dx3 %in% exc12.2 &
         !data$dx4 %in% exc12.2 & !data$dx5 %in% exc12.2 & !data$dx6 %in% exc12.2 &
         !data$dx7 %in% exc12.2 & !data$dx8 %in% exc12.2 & !data$dx9 %in% exc12.2 &
         !data$dx10 %in% exc12.2 ) & 
      (!data$px1 %in% exc12.3 & !data$px2 %in% exc12.3 & !data$px3 %in% exc12.3 &
         !data$px4 %in% exc12.3 & !data$px5 %in% exc12.3 & !data$px6 %in% exc12.3 &
         !data$px7 %in% exc12.3 & !data$px8 %in% exc12.3 & !data$px9 %in% exc12.3 &
         !data$px10 %in% exc12.3) & data$age >= 18
    
    keep91 <- keep10 | keep11 | keep12 
    
    PQI.91 <- ifelse(keep91, 1, 0)
    
  } else {
    PQI.91 <- NULL
  }
  ################################################
  #PQI 92 : Prevention Quality Chronic Composite 
  ################################################
  if ("PQI.92" %in% pqi.match & ICD == "9") {
  ##Includes PQI: 1, 3, 5, 7, 8, 14, 15, 16
  ##PQI 01##  
  keep01 <- data$dx1 %in% c('25010', '25011', '25012', '25013', '25020', 
                            '25021', '25022', '25023', '25030', '25031', 
                            '25032', '25033') & data$age >= 18
    
  ##PQI 03##
    
  keep03 <- data$dx1 %in% c('25040', '25041', '25042', '25043', '25050', 
                            '25051', '25052', '25053', '25060', '25061',
                            '25062', '25063', '25070', '25071', '25072', 
                            '25073', '25080', '25081', '25082', '25083', 
                            '25090', '25091', '25092', '25093') & data$age >= 18
    
  ##PQI 05##
  exc05 <- c('27700', '27701', '27702', '27703', '27709', '51661', '51662', 
             '51663', '51664', '51669', '74721', '7483', '7484', '7485', 
             '74860','74861', '74869', '7488', '7489', '7503', '7593', '7707') 
    
  keep05 <- data$dx1 %in% c('4910', '4911', '49120', '49121', '49122', '4918', 
                            '4919', '4920', '4928', '494', '4940', '4941', '496',
                            '49300', '49301', '49302', '49310', '49311', '49312',
                            '49320', '49321', '49322', '49381', '49382', '49390',
                            '49391', '49392') & #remove 4660 and 490
    !data$dx1 %in% exc05 & !data$dx2 %in% exc05 & !data$dx3 %in% exc05 & 
    !data$dx4 %in% exc05 & !data$dx5 %in% exc05 & !data$dx6 %in% exc05 & 
    !data$dx7 %in% exc05 & !data$dx8 %in% exc05 & !data$dx9 %in% exc05 & 
    !data$dx10 %in% exc05  & data$age >= 40
    
  ##PQI 07##
    
  ##Exclusions - Cardiac Procedure Codes##
    
  exc7 <- c('0050', '3582', '0051', '3583', '0052', '0053', '0054', '0056', 
            '0057', '0066', '3584', '3591', '3592', '3593', '3594', '3595',
            '1751', '1752', '3596', '3597', '1755', '3598', '3500', '3599',
            '3501', '3502', '3601', '3602', '3503', '3603', '3504', '3604',
            '3505', '3605', '3506', '3606', '3507', '3607', '3508', '3609',
            '3509', '3610', '3510', '3611', '3511', '3612', '3512', '3613',
            '3513', '3614', '3514', '3615', '3520', '3521', '3522', '3523',
            '3524', '3525', '3526', '3527', '3528', '3531', '3616', '3617',
            '3619', '362', '363', '3631', '3632', '3633', '3634', '3639', 
            '3532', '3691', '3533', '3699', '3534', '3731', '3535', '3732',
            '3539', '3541', '3542', '3733', '3734', '3735', '3550', '3551',
            '3552', '3736', '3737', '3741', '3553', '375', '3554', '3751',
            '3555', '3752', '3560', '3561', '3562', '3753', '3754', '3755',
            '3563', '3760', '3570', '3761', '3571', '3762', '3572', '3763',
            '3573', '3764', '3581', '3765', '3766', '3770', '3771', '3772',
            '3773', '3774', '3775', '3776', '3777', '3778', '3779', '3780',
            '3781', '3782', '3783', '3785', '3786', '3787', '3789', '3794',
            '3795', '3796', '3797', '3798', '3826')
    
  ##Exclusions - Dialysis access##
    
  exc7.2 <- c('3895', '3927', '3929', '3942', '3943', '3993', '3994')
    
  ##Exclusions - Stage I-IV Kidney disease##
    
  exc7.3 <- c('40300', '40310', '40390', '40400', '40410', '40490')
    
  keep07 <- data$dx1 %in% c('4010', '4019', '40200', '40210', '40290', '40300',
                            '40310', '40390', '40400', '40410', '40490') & 
    !data$px1 %in% exc7 & !data$px2 %in% exc7 & !data$px3 %in% exc7 & 
    !data$px4 %in% exc7 & !data$px5 %in% exc7 & !data$px6 %in% exc7 & 
    !data$px7 %in% exc7 & !data$px8 %in% exc7 & !data$px9 %in% exc7 & 
    !data$px10 %in% exc7 & 
    (!data$dx1 %in% exc7.3 | 
     (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 & 
      !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 & 
      !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &           !data$px10 %in% exc7.2)) & 
      (!data$dx2 %in% exc7.3 | 
       (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
        !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 & 
        !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 & 
        !data$px10 %in% exc7.2)) & 
    (!data$dx3 %in% exc7.3 | 
     (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 & 
      !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
      !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &      
      !data$px10 %in% exc7.2)) & 
    (!data$dx4 %in% exc7.3 | 
     (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
      !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
      !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
      !data$px10 %in% exc7.2)) & 
    (!data$dx5 %in% exc7.3 | 
     (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
      !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
      !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
      !data$px10 %in% exc7.2)) & 
    (!data$dx6 %in% exc7.3 | 
     (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
      !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
      !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &        
      !data$px10 %in% exc7.2)) & 
    (!data$dx7 %in% exc7.3 | 
     (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
      !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
      !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
      !data$px10 %in% exc7.2)) & 
    (!data$dx8 %in% exc7.3 | 
     (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
      !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
      !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
      !data$px10 %in% exc7.2)) &
    (!data$dx9 %in% exc7.3 | 
     (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
      !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
      !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
      !data$px10 %in% exc7.2)) & 
    (!data$dx10 %in% exc7.3 | 
     (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
      !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
      !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
      !data$px10 %in% exc7.2)) & data$age >= 18
    
  ##PQI 08##
    
  ##Exclusions - Cardiac Procedure Codes##
    
  exc8 <- c('0050', '3582', '0051', '3583', '0052', '0053', '0054', '0056',
            '0057', '0066', '3584', '3591', '3592', '3593', '3594', '3595', 
            '1751', '1752', '3596', '3597', '1755', '3598', '3500', '3599',
            '3501', '3502', '3601', '3602', '3503', '3603', '3504', '3604',
            '3505', '3605', '3506', '3606', '3507', '3607', '3508', '3609',
            '3509', '3610', '3510', '3611', '3511', '3612', '3512', '3613',
            '3513', '3614', '3514', '3615', '3520', '3521', '3522', '3523',
            '3524', '3525', '3526', '3527', '3528', '3531', '3616', '3617',
            '3619', '362', '363', '3631', '3632', '3633', '3634', '3639',
            '3532', '3691', '3533', '3699', '3534', '3731', '3535', '3732',
            '3539', '3541', '3542', '3733', '3734', '3735', '3550', '3551',
            '3552', '3736', '3737', '3741', '3553', '375', '3554', '3751', 
            '3555', '3752', '3560', '3561', '3562', '3753', '3754', '3755',
            '3563', '3760', '3570', '3761', '3571', '3762', '3572', '3763', 
            '3573', '3764', '3581', '3765', '3766', '3770', '3771', '3772',
            '3773', '3774', '3775', '3776', '3777', '3778', '3779', '3780',
            '3781', '3782', '3783', '3785', '3786', '3787', '3789', '3794',
            '3795', '3796', '3797', '3798', '3826')
    
  keep08 <- data$dx1 %in% c('39891', '40201', '40211', '40291', '40401', '40403',
                            '40411', '40413', '40491', '40493', '4280', '4281',
                            '42820', '42821', '42822', '42823', '42830',#42822 twice
                            '42831', '42832', '42833', '42840', '42841', '42842',
                            '42843', '4289')  & 
    (!data$px1 %in% exc8 & !data$px2 %in% exc8 & !data$px3 %in% exc8 & 
     !data$px4 %in% exc8 & !data$px5 %in% exc8 & !data$px6 %in% exc8 & 
     !data$px7 %in% exc8 & !data$px8 %in% exc8 & !data$px9 %in% exc8 & 
     !data$px10 %in% exc8) & data$age >= 18
    
  ##PQI 14##
    
  keep14 <- data$dx1 %in% c('25002', '25003') & data$age >= 18
    
  ##PQI 15##
    
  ##Exclusions - Cystic fibrosis and anamolies of the respiratory system ##
    
  exc15 <- c('27700', '27701', '27702', '27703', '27709', '51661', '51662', 
             '51663', '51664', '51669', '74721', '7483', '7484', '7485', 
             '74860', '74861', '74869', '7488', '7489', '7503', '7593', '7707')
    
  keep15 <- data$dx1 %in% c('49300', '49301', '49302', '49310', '49311', 
                            '49312', '49320', '49321', '49322', '49381', 
                            '49382', '49390', '49391', '49392') & 
    (!data$dx1 %in% exc15 & !data$dx2 %in% exc15 & !data$dx3 %in% exc15 & 
     !data$dx4 %in% exc15 & !data$dx5 %in% exc15 & !data$dx6 %in% exc15 & 
     !data$dx7 %in% exc15 & !data$dx8 %in% exc15 & !data$dx9 %in% exc15 & 
     !data$dx10 %in% exc15 ) & data$age >= 18 & data$age <= 39
    
  ##PQI 16##
    
  ##Include - Amputation##
    
  inc16 <- c('8410', '8412', '8413', '8414', '8415', '8416', '8417', '8418', '8419')
    
  ##Include Diabetes##
    
  inc16.2 <- c('25000', '25001', '25002', '25003', '25010', '25011', '25012', 
               '25013', '25020', '25021', '25022', '25023', '25030', '25031',
               '25032', '25033', '25040', '25041', '25042', '25043', '25050',
               '25051', '25052', '25053', '25060', '25061', '25062', '25063',
               '25070', '25071', '25072', '25073', '25080', '25081', '25082',
               '25083', '25090', '25091', '25092', '25093')
  #exclusions
  exc16 <- c('8950', '8951', '8960', '8961', '8962', '8963', '8970', '8971', 
             '8972', '8973', '8974', '8975', '8976', '8977')
    
  keep16 <- ((data$px1 %in% inc16 | data$px2 %in% inc16 | data$px3 %in% inc16 |
              data$px4 %in% inc16 | data$px5 %in% inc16 | data$px6 %in% inc16 |
              data$px7 %in% inc16 | data$px8 %in% inc16 | data$px9 %in% inc16 |
              data$px10 %in% inc16) & 
             (data$dx1 %in% inc16.2 | data$dx2 %in% inc16.2 | 
              data$dx3 %in% inc16.2 | data$dx4 %in% inc16.2 | 
              data$dx5 %in% inc16.2 | data$dx6 %in% inc16.2 |
              data$dx7 %in% inc16.2 | data$dx8 %in% inc16.2 | 
              data$dx9 %in% inc16.2 | data$dx10 %in% inc16.2)) &
    !data$dx1 %in% exc16 & !data$dx2 %in% exc16 & !data$dx3 %in% exc16 &
    !data$dx4 %in% exc16 & !data$dx5 %in% exc16 & !data$dx6 %in% exc16 &
    !data$dx7 %in% exc16 & !data$dx8 %in% exc16 & !data$dx9 %in% exc16 &
    !data$dx10 %in% exc16 & data$age >= 18
  
  keep92 <- keep01 | keep03 | keep05 | keep07 | keep08 | keep14 | keep15 | keep16
  
  PQI.92 <- ifelse(keep92, 1, 0)
  } else if ("PQI.92" %in% pqi.match & ICD == "10"){
    ##Includes PQI: 1, 3, 5, 7, 8, 14, 15, 16
    ##PQI 01##
    keep01 <- data$dx1 %in% c('E1010', 'E1011', 'E10641', 'E1065', 'E1100', 
                              'E1101', 'E11641', 'E1165') & data$age >= 18
    
    ##PQI 03##
    keep03 <- data$dx1 %in% c('E1021', 'E1121', 'E1022', 'E1122', 'E1029',
                              'E1129', 'E10311', 'E11311', 'E10319', 'E11319',
                              'E10321', 'E11321', 'E10329', 'E11329', 'E10331',
                              'E11331', 'E10339', 'E11339', 'E10341', 'E11341',
                              'E10349', 'E11349', 'E10351', 'E11351', 'E10359',
                              'E11359', 'E1036', 'E1136', 'E1039', 'E1139',
                              'E1040', 'E1140', 'E1041', 'E1141', 'E1042', 
                              'E1142', 'E1043', 'E1143', 'E1044', 'E1144', 
                              'E1049', 'E1149', 'E1051', 'E1151', 'E1052',
                              'E1152', 'E1059', 'E1159', 'E10610', 'E11610',
                              'E10618', 'E11618', 'E10620', 'E11620', 'E10621', 
                              'E11621', 'E10622', 'E11622', 'E10628', 'E11628', 
                              'E10630', 'E11630', 'E10638', 'E11638', 'E1069', 
                              'E1169', 'E108', 'E118') & data$age >= 18
    
    ##PQI 05##
    exc05 <- c('E840', 'E8411', 'E8419', 'E848', 'E849', 'J8483',
               'J84841', 'J84842', 'J84843', 'J84848', 'P270', 'P271',
               'P278', 'P279', 'Q254', 'Q322', 'Q323', 'Q324', 'Q330', 
               'Q331', 'Q332', 'Q333', 'Q334', 'Q335', 'Q336', 'Q338',
               'Q339', 'Q340', 'Q341', 'Q348', 'Q311', 'Q312', 'Q313',
               'Q315', 'Q318', 'Q319', 'Q320', 'Q321', 'Q349', 'Q390',
               'Q391', 'Q392', 'Q393', 'Q394', 'Q893')
    
    keep05 <- data$dx1 %in% c('J410', 'J439', 'J411', 'J440', 'J418', 'J441', 'J42', 
                              'J449', 'J430', 'J470', 'J431', 'J471', 'J432', 'J479',
                              'J438','J4521', 'J4552', 'J4522', 'J45901', 'J4531',
                              'J45902', 'J4532', 'J45990', 'J4541', 'J45991',
                              'J4542', 'J45998', 'J4551') & 
      !data$dx1 %in% exc05 & !data$dx2 %in% exc05 & !data$dx3 %in% exc05 & 
      !data$dx4 %in% exc05 & !data$dx5 %in% exc05 & !data$dx6 %in% exc05 & 
      !data$dx7 %in% exc05 & !data$dx8 %in% exc05 & !data$dx9 %in% exc05 & 
      !data$dx10 %in% exc05 & data$age >= 40
    
    ##PQI 07##
    ##Exclusions - Cardiac Procedure Codes##
    
    exc7 <- c('0210093', '0210098', '0210099', '021009C', '021009F', '021009W', 
              '02100A3', '02100A8', '02100A9', '02100AC', '02100AF', '02100AW', 
              '02100J3', '02100J8', '02100J9', '02100JC', '02100JF', '02100JW', 
              '02100K3', '02100K8', '02100K9', '02100KC', '02100KF', '02100KW', 
              '02100Z3', '02100Z8', '02100Z9', '02100ZC', '02100ZF', '0210344',
              '02103D4', '0210444', '0210493', '0210498', '0210499', '021049C', 
              '021049F', '021049W', '02104A3', '02104A8', '02104A9', '02104AC', 
              '02104AF', '02104AW', '02104D4', '02104J3', '02104J8', '02104J9',
              '02104JC', '02104JF', '02104JW', '02104K3', '02104K8', '02104K9',
              '02104KC', '02104KF', '02104KW', '02104Z3', '02104Z8', '02104Z9',
              '02104ZC', '02104ZF', '0211093', '0211098', '0211099', '021109C',
              '021109F', '021109W', '02110A3', '02110A8', '02110A9', '02110AC',
              '02110AF', '02110AW', '02110J3', '02110J8', '02110J9', '02110JC',
              '02110JF', '02110JW', '02110K3', '02110K8', '02110K9', '02110KC',
              '02110KF', '02110KW', '02110Z3', '02110Z8', '02110Z9', '02110ZC',
              '02110ZF', '0211344', '02113D4', '0211444', '0211493', '0211498',
              '0211499', '021149C', '021149F', '021149W', '02114A3', '02114A8',
              '02114A9', '02114AC', '02114AF', '02114AW', '02114D4', '02114J3',
              '02114J8', '02114J9', '02114JC', '02114JF', '02114JW', '02114K3',
              '02114K8', '02114K9', '02114KC', '02114KF', '02114KW', '02114Z3',
              '02114Z8', '02114Z9', '02114ZC', '02114ZF', '0212093', '0212098',
              '0212099', '021209C', '021209F', '021209W', '02120A3', '02120A8',
              '02120A9', '02120AC', '02120AF', '02120AW', '02120J3', '02120J8',
              '02120J9', '02120JC', '02120JF', '02120JW', '02120K3', '02120K8',
              '02120K9', '02120KC', '02120KF', '02120KW', '02120Z3', '02120Z8',
              '02120Z9', '02120ZC', '02120ZF', '0212344', '02123D4', '0212444',
              '0212493', '0212498', '0212499', '021249C', '021249F', '021249W',
              '02124A3', '02124A8', '02124A9', '02124AC', '02124AF', '02124AW',
              '02124D4', '02124J3', '02124J8', '02124J9', '02124JC', '02124JF',
              '02124JW', '02124K3', '02124K8', '02124K9', '02124KC', '02124KF',
              '02124KW', '02124Z3', '02124Z8', '02124Z9', '02124ZC', '02124ZF',
              '0213093', '0213098', '0213099', '021309C', '021309F', '021309W',
              '02130A3', '02130A8', '02130A9', '02130AC', '02130AF', '02130AW',
              '02130J3', '02130J8', '02130J9', '02130JC', '02130JF', '02130JW',
              '02130K3', '02130K8', '02130K9', '02130KC', '02130KF', '02130KW',
              '02130Z3', '02130Z8', '02130Z9', '02130ZC', '02130ZF', '0213344',
              '02133D4', '0213444', '0213493', '0213498', '0213499', '021349C',
              '021349F', '021349W', '02134A3', '02134A8', '02134A9', '02134AC',
              '02134AF', '02134AW', '02134D4', '02134J3', '02134J8', '02134J9',
              '02134JC', '02134JF', '02134JW', '02134K3', '02134K8', '02134K9',
              '02134KC', '02134KF', '02134KW', '02134Z3', '02134Z8', '02134Z9',
              '02134ZC', '02134ZF', '021609P', '021609Q', '021609R', '02160AP',
              '02160AQ', '02160AR', '02160JP', '02160JQ', '02160JR', '02160KP',
              '02160KQ', '02160KR', '02160ZP', '02160ZQ', '02160ZR', '021649P',
              '021649Q', '021649R', '02164AP', '02164AQ', '02164AR', '02164JP',
              '02164JQ', '02164JR', '02164KP', '02164KQ', '02164KR', '02164ZP',
              '02164ZQ', '02164ZR', '021709P', '021709Q', '021709R', '02170AP',
              '02170AQ', '02170AR', '02170JP', '02170JQ', '02170JR', '02170KP',
              '02170KQ', '02170KR', '02170ZP', '02170ZQ', '02170ZR', '021749P',
              '021749Q', '021749R', '02174AP', '02174AQ', '02174AR', '02174JP',
              '02174JQ', '02174JR', '02174KP', '02174KQ', '02174KR', '02174ZP',
              '02174ZQ', '02174ZR', '021K09P', '021K09Q', '021K09R', '021K0AP',
              '021K0AQ', '021K0AR', '021K0JP', '021K0JQ', '021K0JR', '021K0KP',
              '021K0KQ', '021K0KR', '021K0Z5', '021K0Z8', '021K0Z9', '021K0ZC',
              '021K0ZF', '021K0ZP', '021K0ZQ', '021K0ZR', '021K0ZW', '021K49P',
              '021K49Q', '021K49R', '021K4AP', '021K4AQ', '021K4AR', '021K4JP',
              '021K4JQ', '021K4JR', '021K4KP', '021K4KQ', '021K4KR', '021K4Z5',
              '021K4Z8', '021K4Z9', '021K4ZC', '021K4ZF', '021L09R', '021L0AP', 
              '021L0AQ', '021L0AR', '021L0JP', '021L0JQ', '021K4ZP', '021K4ZQ',
              '021K4ZR', '021K4ZW', '021L09P', '021L09Q', '021L0JR', '021L0KP',
              '021L0KQ', '021L0KR', '021L0Z5', '021L0Z8', '021L0Z9', '021L0ZC',
              '021L0ZF', '021L0ZP', '021L0ZQ', '021L0ZR', '021L0ZW', '021L49P',
              '021L49Q', '021L49R', '021L4AP', '021L4AQ', '021L4AR', '021L4JP',
              '021L4JQ', '021L4JR', '021L4KP', '021L4KQ', '021L4KR', '021L4Z5',
              '021L4Z8', '021L4Z9', '021L4ZC', '021L4ZF', '021L4ZP', '021L4ZQ',
              '021L4ZR', '021L4ZW', '02540ZZ', '02543ZZ', '02544ZZ', '02550ZZ',
              '02553ZZ', '02554ZZ', '02560ZZ', '02563ZZ', '02564ZZ', '02570ZK',
              '02570ZZ', '02573ZK', '02573ZZ', '02574ZK', '02574ZZ', '02580ZZ',
              '02583ZZ', '02584ZZ', '02590ZZ', '02593ZZ', '02594ZZ', '025D0ZZ',
              '025D3ZZ', '025D4ZZ', '025F0ZZ', '025F3ZZ', '025F4ZZ', '025G0ZZ',
              '025G3ZZ', '025G4ZZ', '025H0ZZ', '025H3ZZ', '025H4ZZ', '025J0ZZ',
              '025J3ZZ', '025J4ZZ', '025K0ZZ', '025K3ZZ', '025K4ZZ', '025L0ZZ', 
              '025L3ZZ', '025L4ZZ', '025M0ZZ', '025M3ZZ', '025M4ZZ', '025N0ZZ', 
              '025N3ZZ', '025N4ZZ', '0270046', '027004Z', '02700D6', '02700DZ',
              '02700T6', '02700TZ', '02700Z6', '02700ZZ', '0270346', '027034Z', 
              '02703D6', '02703DZ', '02703T6', '02703TZ', '02703Z6', '02703ZZ',
              '0270446', '027044Z', '02704D6', '02704DZ', '02704T6', '02704TZ',
              '02704Z6', '02704ZZ', '0271046', '027104Z', '02710D6', '02710DZ',
              '02710T6', '02710TZ', '02710Z6', '02710ZZ', '0271346', '027134Z',
              '02713D6', '02713DZ', '02713T6', '02713TZ', '02713Z6', '02713ZZ',
              '0271446', '027144Z', '02714D6', '02714DZ', '02714T6', '02714TZ',
              '02714Z6', '02714ZZ', '0272046', '027204Z', '02720D6', '02720DZ',
              '02720T6', '02720TZ', '02720Z6', '02720ZZ', '0272346', '027234Z', 
              '02723D6', '02723DZ', '02723T6', '02723TZ', '02723Z6', '02723ZZ',
              '0272446', '027244Z', '02724D6', '02724DZ', '02724T6', '02724TZ',
              '02724Z6', '02724ZZ', '0273046', '027304Z', '02730D6', '02730DZ',
              '02730T6', '02730TZ', '02730Z6', '02730ZZ', '0273346', '027334Z',
              '02733D6', '02733DZ', '02733T6', '02733TZ', '02733Z6', '02733ZZ', 
              '0273446', '027344Z', '02734D6', '02734DZ', '02734T6', '02734TZ',
              '02734Z6', '02734ZZ', '027F04Z', '027F0DZ', '027F0ZZ', '027F34Z',
              '027F3DZ', '027F3ZZ', '027F44Z', '027F4DZ', '027F4ZZ', '027G04Z',
              '027G0DZ', '027G0ZZ', '027G34Z', '027G3DZ', '027G3ZZ', '027G44Z', 
              '027G4DZ', '027G4ZZ', '027H04Z', '027H0DZ', '027H0ZZ', '027H34Z',
              '027H3DZ', '027H3ZZ', '027H44Z', '027H4DZ', '027H4ZZ', '027J04Z',
              '027J0DZ', '027J0ZZ', '027J34Z', '027J3DZ', '027J3ZZ', '027J44Z',
              '027J4DZ', '027J4ZZ', '02890ZZ', '02893ZZ', '02894ZZ', '028D0ZZ',
              '028D3ZZ', '028D4ZZ', '02B40ZZ', '02B43ZZ', '02B44ZZ', '02B50ZZ',
              '02B53ZZ', '02B54ZZ', '02B60ZZ', '02B63ZZ', '02B64ZZ', '02B70ZK',
              '02B70ZZ', '02B73ZK', '02B73ZZ', '02B74ZK', '02B74ZZ', '02B80ZZ',
              '02B83ZZ', '02B84ZZ', '02B90ZZ', '02B93ZZ', '02B94ZZ', '02BD0ZZ',
              '02BD3ZZ', '02BD4ZZ', '02BF0ZZ', '02BF3ZZ', '02BF4ZZ', '02BG0ZZ',
              '02BG3ZZ', '02BG4ZZ', '02BH0ZZ', '02BH3ZZ', '02BH4ZZ', '02BJ0ZZ',
              '02BJ3ZZ', '02BJ4ZZ', '02BK0ZZ', '02BK3ZZ', '02BK4ZZ', '02BL0ZZ',
              '02BL3ZZ', '02BL4ZZ', '02BM0ZZ', '02BM3ZZ', '02BM4ZZ', '02BN0ZZ', 
              '02BN3ZZ', '02BN4ZZ', '02C00ZZ', '02C03ZZ', '02C04ZZ', '02C10ZZ',
              '02C13ZZ', '02C14ZZ', '02C20ZZ', '02C23ZZ', '02C24ZZ', '02C30ZZ',
              '02C33ZZ', '02C34ZZ', '02C40ZZ', '02C43ZZ', '02C44ZZ', '02C50ZZ',
              '02C53ZZ', '02C54ZZ', '02CD0ZZ', '02CD3ZZ', '02CD4ZZ', '02CF0ZZ',
              '02CF3ZZ', '02CF4ZZ', '02CG0ZZ', '02CG3ZZ', '02CG4ZZ', '02CH0ZZ', 
              '02CH3ZZ', '02CH4ZZ', '02CJ0ZZ', '02CJ3ZZ', '02CJ4ZZ', '02CM0ZZ', 
              '02CM3ZZ', '02CM4ZZ', '02H400Z', '02H402Z', '02H403Z', '02H40DZ',
              '02H40JZ', '02H40KZ', '02H40MZ', '02H430Z', '02H432Z', '02H433Z',
              '02H43DZ', '02H43JZ', '02H43KZ', '02H43MZ', '02H440Z', '02H442Z', 
              '02H443Z', '02H44DZ', '02H44JZ', '02H44KZ', '02H44MZ', '02H600Z',
              '02H60JZ', '02H60KZ', '02H60MZ', '02H630Z', '02H63JZ', '02H63KZ', 
              '02H63MZ', '02H640Z', '02H64JZ', '02H64KZ', '02H64MZ', '02H700Z',
              '02H70JZ', '02H70KZ', '02H70MZ', '02H730Z', '02H73JZ', '02H73KZ', 
              '02H73MZ', '02H740Z', '02H74JZ', '02H74KZ', '02H74MZ', '02HA0QZ',
              '02HA0RS', '02HA0RZ', '02HA3QZ', '02HA3RS', '02HA3RZ', '02HA4QZ',
              '02HA4RS', '02HA4RZ', '02HK00Z', '02HK02Z', '02HK0JZ', '02HK0KZ',
              '02HK0MZ', '02HK30Z', '02HK32Z', '02HK3JZ', '02HK3KZ', '02HK3MZ',
              '02HK40Z', '02HK42Z', '02HK4JZ', '02HK4KZ', '02HK4MZ', '02HL00Z',
              '02HL0JZ', '02HL0KZ', '02HL0MZ', '02HL30Z', '02HL3JZ', '02HL3KZ',
              '02HL3MZ', '02HL40Z', '02HL4JZ', '02HL4KZ', '02HL4MZ', '02HN0JZ',
              '02HN0KZ', '02HN0MZ', '02HN3JZ', '02HN3KZ', '02HN3MZ', '02HN4JZ',
              '02HN4KZ', '02HN4MZ', '02HS00Z', '02HS30Z', '02HS40Z', '02HT00Z', 
              '02HT30Z', '02HT40Z', '02HV00Z', '02HV30Z', '02HV40Z', '02L70CK',
              '02L70DK', '02L70ZK', '02L73CK', '02L73DK', '02L73ZK', '02L74CK',
              '02L74DK', '02L74ZK', '02LR0ZT', '02LS0ZZ', '02LT0ZZ', '02N50ZZ',
              '02N53ZZ', '02N54ZZ', '02N90ZZ', '02N93ZZ', '02N94ZZ', '02ND0ZZ',
              '02ND3ZZ', '02ND4ZZ', '02NF0ZZ', '02NF3ZZ', '02NF4ZZ', '02NG0ZZ',
              '02NG3ZZ', '02NG4ZZ', '02NH0ZZ', '02NH3ZZ', '02NH4ZZ', '02NJ0ZZ',
              '02NJ3ZZ', '02NJ4ZZ', '02NK0ZZ', '02NK3ZZ', '02NK4ZZ', '02NL0ZZ',
              '02NL3ZZ', '02NL4ZZ', '02NM0ZZ', '02NM3ZZ', '02NM4ZZ', '02PA0MZ',
              '02PA0QZ', '02PA0RZ', '02PA3MZ', '02PA3QZ', '02PA3RZ', '02PA4MZ',
              '02PA4QZ', '02PA4RZ', '02PAXMZ', '02Q00ZZ', '02Q03ZZ', '02Q04ZZ',
              '02Q10ZZ', '02Q13ZZ', '02Q14ZZ', '02Q20ZZ', '02Q23ZZ', '02Q24ZZ',
              '02Q30ZZ', '02Q33ZZ', '02Q34ZZ', '02Q40ZZ', '02Q43ZZ', '02Q44ZZ',
              '02Q50ZZ', '02Q53ZZ', '02Q54ZZ', '02Q70ZZ', '02Q73ZZ', '02Q74ZZ',
              '02Q90ZZ', '02Q93ZZ', '02Q94ZZ', '02QA0ZZ', '02QA3ZZ', '02QA4ZZ',
              '02QB0ZZ', '02QB3ZZ', '02QB4ZZ', '02QC0ZZ', '02QC3ZZ', '02QC4ZZ',
              '02QD0ZZ', '02QD3ZZ', '02QD4ZZ', '02QF0ZZ', '02QF3ZZ', '02QF4ZZ',
              '02QG0ZZ', '02QG3ZZ', '02QG4ZZ', '02QH0ZZ', '02QH3ZZ', '02QH4ZZ',
              '02QJ0ZZ', '02QJ3ZZ', '02QJ4ZZ', '02QM0ZZ', '02QM3ZZ', '02QM4ZZ',
              '02R907Z', '02R908Z', '02R90JZ', '02R90KZ', '02R947Z', '02R948Z',
              '02R94JZ', '02R94KZ', '02RD07Z', '02RD08Z', '02RD0JZ', '02RD0KZ',
              '02RD47Z', '02RD48Z', '02RD4JZ', '02RD4KZ', '02RF07Z', '02RF08Z', 
              '02RF0JZ', '02RF0KZ', '02RF37H', '02RF37Z', '02RF38H', '02RF38Z',
              '02RF3JH', '02RF3JZ', '02RF3KH', '02RF3KZ', '02RF47Z', '02RF48Z',
              '02RF4JZ', '02RF4KZ', '02RG07Z', '02RG08Z', '02RG0JZ', '02RG0KZ',
              '02RG37H', '02RG37Z', '02RG38H', '02RG38Z', '02RG3JH', '02RG3JZ',
              '02RG3KH', '02RG3KZ', '02RG47Z', '02RG48Z', '02RG4JZ', '02RG4KZ',
              '02RH07Z', '02RH08Z', '02RH0JZ', '02RH0KZ', '02RH37H', '02RH37Z',
              '02RH38H', '02RH38Z', '02RH3JH', '02RH3JZ', '02RH3KH', '02RH3KZ', 
              '02RH47Z', '02RH48Z', '02RH4JZ', '02RH4KZ', '02RJ07Z', '02RJ08Z',
              '02RJ0JZ', '02RJ0KZ', '02RJ47Z', '02RJ48Z', '02RJ4JZ', '02RJ4KZ',
              '02RK07Z', '02RK0JZ', '02RK0KZ', '02RK47Z', '02RK4KZ', '02RL07Z',
              '02RL0JZ', '02RL0KZ', '02RL47Z', '02RL4KZ', '02RM07Z', '02RM0JZ',
              '02RM0KZ', '02RM47Z', '02RM4JZ', '02RM4KZ', '02RP0JZ', '02RQ07Z',
              '02RQ0JZ', '02RR07Z', '02RR0JZ', '02SP0ZZ', '02SW0ZZ', '02T50ZZ',
              '02T53ZZ', '02T54ZZ', '02T80ZZ', '02T83ZZ', '02T84ZZ', '02T90ZZ',
              '02T93ZZ', '02T94ZZ', '02TD0ZZ', '02TD3ZZ', '02TD4ZZ', '02TH0ZZ',
              '02TH3ZZ', '02TH4ZZ', '02TM0ZZ', '02TM3ZZ', '02TM4ZZ', '02TN0ZZ',
              '02TN3ZZ', '02TN4ZZ', '02U507Z', '02U508Z', '02U50JZ', '02U50KZ',
              '02U537Z', '02U538Z', '02U53JZ', '02U53KZ', '02U547Z', '02U548Z',
              '02U54JZ', '02U54KZ', '02U607Z', '02U608Z', '02U60KZ', '02U707Z',
              '02U708Z', '02U70JZ', '02U70KZ', '02U737Z', '02U738Z', '02U73KZ',
              '02U747Z', '02U748Z', '02U74KZ', '02U907Z', '02U908Z', '02U90JZ', 
              '02U90KZ', '02U937Z', '02U938Z', '02U93JZ', '02U93KZ', '02U947Z',
              '02U948Z', '02U94JZ', '02U94KZ', '02UA0JZ', '02UA3JZ', '02UA4JZ',
              '02UD07Z', '02UD08Z', '02UD0JZ', '02UD0KZ', '02UD37Z', '02UD38Z',
              '02UD3JZ', '02UD3KZ', '02UD47Z', '02UD48Z', '02UD4JZ', '02UD4KZ',
              '02UF07Z', '02UF08Z', '02UF0JZ', '02UF0KZ', '02UF37Z', '02UF38Z',
              '02UF3JZ', '02UF3KZ', '02UF47Z', '02UF48Z', '02UF4JZ', '02UF4KZ', 
              '02UG07Z', '02UG08Z', '02UG0JZ', '02UG0KZ', '02UG37Z', '02UG38Z', 
              '02UG3JZ', '02UG3KZ', '02UG47Z', '02UG48Z', '02UG4JZ', '02UG4KZ',
              '02UH07Z', '02UH08Z', '02UH0JZ', '02UH0KZ', '02UH37Z', '02UH38Z',
              '02UH3JZ', '02UH3KZ', '02UH47Z', '02UH48Z', '02UH4JZ', '02UH4KZ',
              '02UJ07Z', '02UJ08Z', '02UJ0JZ', '02UJ0KZ', '02UJ37Z', '02UJ38Z',
              '02UJ3JZ', '02UJ3KZ', '02UJ47Z', '02UJ48Z', '02UJ4JZ', '02UJ4KZ',
              '02UK0KZ', '02UK3KZ', '02UK4KZ', '02UL0KZ', '02UL3KZ', '02UL4KZ', 
              '02UM07Z', '02UM0JZ', '02UM0KZ', '02UM37Z', '02UM38Z', '02UM3JZ',
              '02UM3KZ', '02UM47Z', '02UM48Z', '02UM4JZ', '02UM4KZ', '02VR0ZT',
              '02W50JZ', '02W54JZ', '02WA0JZ', '02WA0MZ', '02WA0QZ', '02WA0RZ',
              '02WA3MZ', '02WA3QZ', '02WA3RZ', '02WA4MZ', '02WA4QZ', '02WA4RZ',
              '02WF07Z', '02WF08Z', '02WF0JZ', '02WF0KZ', '02WF47Z', '02WF48Z',
              '02WF4JZ', '02WF4KZ', '02WG07Z', '02WG08Z', '02WG0JZ', '02WG0KZ',
              '02WG47Z', '02WG48Z', '02WG4JZ', '02WG4KZ', '02WH07Z', '02WH08Z',
              '02WH0JZ', '02WH0KZ', '02WH47Z', '02WH48Z', '02WH4JZ', '02WH4KZ',
              '02WJ07Z', '02WJ08Z', '02WJ0JZ', '02WJ0KZ', '02WJ47Z', '02WJ48Z',
              '02WJ4JZ', '02WJ4KZ', '02WM0JZ', '02WM4JZ', '02YA0Z0', '02YA0Z1',
              '02YA0Z2', '0JH600Z', '0JH604Z', '0JH605Z', '0JH606Z', '0JH607Z',
              '0JH608Z', '0JH609Z', '0JH60AZ', '0JH60PZ', '0JH630Z', '0JH634Z',
              '0JH635Z', '0JH636Z', '0JH637Z', '0JH638Z', '0JH639Z', '0JH63AZ',
              '0JH63PZ', '0JH800Z', '0JH804Z', '0JH805Z', '0JH806Z', '0JH807Z',
              '0JH808Z', '0JH809Z', '0JH80AZ', '0JH80PZ', '0JH830Z', '0JH834Z',
              '0JH835Z', '0JH836Z', '0JH837Z', '0JH838Z', '0JH839Z', '0JH83AZ',
              '0JH83PZ', '0JPT0PZ', '0JPT3PZ', '0JWT0PZ', '0JWT3PZ', '3E07017',
              '3E070PZ', '3E07317', '3E073PZ', '5A02110', '5A02116', '5A02210',
              '5A02216', '5A1213Z', '5A1223Z')
    
    ##Exclusions - Dialysis access##
    
    exc7.2 <- c('03170AD', '03170AF', '031209D', '031209F', '03120AD', '03120AF', 
                '03120JD', '03120JF', '03120KD', '03120KF', '03120ZD', '03120ZF',
                '031309D', '031309F', '03130AD', '03130AF', '03130JD', '03130JF', 
                '03130KD', '03130KF', '03130ZD', '03130ZF', '031409D', '031409F',
                '03140AD', '03140AF', '03140JD', '03140JF', '03140KD', '03140KF',
                '03140ZD', '03140ZF', '031509D', '031509F', '03150AD', '03150AF',
                '03150JD', '03150JF', '03150KD', '03150KF', '03150ZD', '03150ZF',
                '031609D', '031609F', '03160AD', '03160AF', '03160JD', '03160JF',
                '03160KD', '03160KF', '03160ZD', '03160ZF', '031709D', '031709F', 
                '03170JD', '03170JF', '03170KD', '03170KF', '03170ZD', '03170ZF',
                '031809D', '031809F', '03180AD', '03180AF', '03180JD', '03180JF',
                '03180KD', '03180KF', '03180ZD', '03180ZF', '031909F', '03190AF',
                '03190JF', '03190KF', '03190ZF', '031A09F', '031A0AF', '031A0JF',
                '031A0KF', '031A0ZF', '031B09F', '031B0AF', '031B0JF', '031B0KF',
                '031B0ZF', '031C09F', '031C0AF', '031C0JF', '031C0KF', '031C0ZF',
                '03PY07Z', '03PY0JZ', '03PY0KZ', '03PY37Z', '03PY3JZ', '03PY3KZ',
                '03PY47Z', '03PY4JZ', '03PY4KZ', '03WY0JZ', '03WY3JZ', '03WY4JZ',
                '03WYXJZ', '05HY33Z', '06HY33Z')
    
    
    ##Exclusions - Stage I-IV Kidney disease##
    
    exc7.3 <- c('I129', 'I1310')
    
    keep07 <- data$dx1 %in% c('I10', 'I129', 'I119', 'I1310') & 
      !data$px1 %in% exc7 & !data$px2 %in% exc7 & !data$px3 %in% exc7 & 
      !data$px4 %in% exc7 & !data$px5 %in% exc7 & !data$px6 %in% exc7 & 
      !data$px7 %in% exc7 & !data$px8 %in% exc7 & !data$px9 %in% exc7 & 
      !data$px10 %in% exc7 & 
      (!data$dx1 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 & 
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 & 
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 & 
            !data$px10 %in% exc7.2)) & 
      (!data$dx2 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 & 
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 & 
            !data$px10 %in% exc7.2)) & 
      (!data$dx3 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 & 
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
            !data$px10 %in% exc7.2)) & 
      (!data$dx4 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
            !data$px10 %in% exc7.2)) & 
      (!data$dx5 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
            !data$px10 %in% exc7.2)) & 
      (!data$dx6 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
            !data$px10 %in% exc7.2)) & 
      (!data$dx7 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
            !data$px10 %in% exc7.2)) & 
      (!data$dx8 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
            !data$px10 %in% exc7.2)) &
      (!data$dx9 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
            !data$px10 %in% exc7.2)) & 
      (!data$dx10 %in% exc7.3 | 
         (!data$px1 %in% exc7.2 & !data$px2 %in% exc7.2 & !data$px3 %in% exc7.2 &
            !data$px4 %in% exc7.2 & !data$px5 %in% exc7.2 & !data$px6 %in% exc7.2 &
            !data$px7 %in% exc7.2 & !data$px8 %in% exc7.2 & !data$px9 %in% exc7.2 &
            !data$px10 %in% exc7.2)) & data$age >= 18
    
    ##PQI 08##
    ##Exclusions - Cardiac Procedures (same as exc7)
    
    exc8 <- c('0210093', '0210098', '0210099', '021009C', '021009F', '021009W', 
              '02100A3', '02100A8', '02100A9', '02100AC', '02100AF', '02100AW', 
              '02100J3', '02100J8', '02100J9', '02100JC', '02100JF', '02100JW', 
              '02100K3', '02100K8', '02100K9', '02100KC', '02100KF', '02100KW', 
              '02100Z3', '02100Z8', '02100Z9', '02100ZC', '02100ZF', '0210344',
              '02103D4', '0210444', '0210493', '0210498', '0210499', '021049C', 
              '021049F', '021049W', '02104A3', '02104A8', '02104A9', '02104AC', 
              '02104AF', '02104AW', '02104D4', '02104J3', '02104J8', '02104J9',
              '02104JC', '02104JF', '02104JW', '02104K3', '02104K8', '02104K9',
              '02104KC', '02104KF', '02104KW', '02104Z3', '02104Z8', '02104Z9',
              '02104ZC', '02104ZF', '0211093', '0211098', '0211099', '021109C',
              '021109F', '021109W', '02110A3', '02110A8', '02110A9', '02110AC',
              '02110AF', '02110AW', '02110J3', '02110J8', '02110J9', '02110JC',
              '02110JF', '02110JW', '02110K3', '02110K8', '02110K9', '02110KC',
              '02110KF', '02110KW', '02110Z3', '02110Z8', '02110Z9', '02110ZC',
              '02110ZF', '0211344', '02113D4', '0211444', '0211493', '0211498',
              '0211499', '021149C', '021149F', '021149W', '02114A3', '02114A8',
              '02114A9', '02114AC', '02114AF', '02114AW', '02114D4', '02114J3',
              '02114J8', '02114J9', '02114JC', '02114JF', '02114JW', '02114K3',
              '02114K8', '02114K9', '02114KC', '02114KF', '02114KW', '02114Z3',
              '02114Z8', '02114Z9', '02114ZC', '02114ZF', '0212093', '0212098',
              '0212099', '021209C', '021209F', '021209W', '02120A3', '02120A8',
              '02120A9', '02120AC', '02120AF', '02120AW', '02120J3', '02120J8',
              '02120J9', '02120JC', '02120JF', '02120JW', '02120K3', '02120K8',
              '02120K9', '02120KC', '02120KF', '02120KW', '02120Z3', '02120Z8',
              '02120Z9', '02120ZC', '02120ZF', '0212344', '02123D4', '0212444',
              '0212493', '0212498', '0212499', '021249C', '021249F', '021249W',
              '02124A3', '02124A8', '02124A9', '02124AC', '02124AF', '02124AW',
              '02124D4', '02124J3', '02124J8', '02124J9', '02124JC', '02124JF',
              '02124JW', '02124K3', '02124K8', '02124K9', '02124KC', '02124KF',
              '02124KW', '02124Z3', '02124Z8', '02124Z9', '02124ZC', '02124ZF',
              '0213093', '0213098', '0213099', '021309C', '021309F', '021309W',
              '02130A3', '02130A8', '02130A9', '02130AC', '02130AF', '02130AW',
              '02130J3', '02130J8', '02130J9', '02130JC', '02130JF', '02130JW',
              '02130K3', '02130K8', '02130K9', '02130KC', '02130KF', '02130KW',
              '02130Z3', '02130Z8', '02130Z9', '02130ZC', '02130ZF', '0213344',
              '02133D4', '0213444', '0213493', '0213498', '0213499', '021349C',
              '021349F', '021349W', '02134A3', '02134A8', '02134A9', '02134AC',
              '02134AF', '02134AW', '02134D4', '02134J3', '02134J8', '02134J9',
              '02134JC', '02134JF', '02134JW', '02134K3', '02134K8', '02134K9',
              '02134KC', '02134KF', '02134KW', '02134Z3', '02134Z8', '02134Z9',
              '02134ZC', '02134ZF', '021609P', '021609Q', '021609R', '02160AP',
              '02160AQ', '02160AR', '02160JP', '02160JQ', '02160JR', '02160KP',
              '02160KQ', '02160KR', '02160ZP', '02160ZQ', '02160ZR', '021649P',
              '021649Q', '021649R', '02164AP', '02164AQ', '02164AR', '02164JP',
              '02164JQ', '02164JR', '02164KP', '02164KQ', '02164KR', '02164ZP',
              '02164ZQ', '02164ZR', '021709P', '021709Q', '021709R', '02170AP',
              '02170AQ', '02170AR', '02170JP', '02170JQ', '02170JR', '02170KP',
              '02170KQ', '02170KR', '02170ZP', '02170ZQ', '02170ZR', '021749P',
              '021749Q', '021749R', '02174AP', '02174AQ', '02174AR', '02174JP',
              '02174JQ', '02174JR', '02174KP', '02174KQ', '02174KR', '02174ZP',
              '02174ZQ', '02174ZR', '021K09P', '021K09Q', '021K09R', '021K0AP',
              '021K0AQ', '021K0AR', '021K0JP', '021K0JQ', '021K0JR', '021K0KP',
              '021K0KQ', '021K0KR', '021K0Z5', '021K0Z8', '021K0Z9', '021K0ZC',
              '021K0ZF', '021K0ZP', '021K0ZQ', '021K0ZR', '021K0ZW', '021K49P',
              '021K49Q', '021K49R', '021K4AP', '021K4AQ', '021K4AR', '021K4JP',
              '021K4JQ', '021K4JR', '021K4KP', '021K4KQ', '021K4KR', '021K4Z5',
              '021K4Z8', '021K4Z9', '021K4ZC', '021K4ZF', '021L09R', '021L0AP', 
              '021L0AQ', '021L0AR', '021L0JP', '021L0JQ', '021K4ZP', '021K4ZQ',
              '021K4ZR', '021K4ZW', '021L09P', '021L09Q', '021L0JR', '021L0KP',
              '021L0KQ', '021L0KR', '021L0Z5', '021L0Z8', '021L0Z9', '021L0ZC',
              '021L0ZF', '021L0ZP', '021L0ZQ', '021L0ZR', '021L0ZW', '021L49P',
              '021L49Q', '021L49R', '021L4AP', '021L4AQ', '021L4AR', '021L4JP',
              '021L4JQ', '021L4JR', '021L4KP', '021L4KQ', '021L4KR', '021L4Z5',
              '021L4Z8', '021L4Z9', '021L4ZC', '021L4ZF', '021L4ZP', '021L4ZQ',
              '021L4ZR', '021L4ZW', '02540ZZ', '02543ZZ', '02544ZZ', '02550ZZ',
              '02553ZZ', '02554ZZ', '02560ZZ', '02563ZZ', '02564ZZ', '02570ZK',
              '02570ZZ', '02573ZK', '02573ZZ', '02574ZK', '02574ZZ', '02580ZZ',
              '02583ZZ', '02584ZZ', '02590ZZ', '02593ZZ', '02594ZZ', '025D0ZZ',
              '025D3ZZ', '025D4ZZ', '025F0ZZ', '025F3ZZ', '025F4ZZ', '025G0ZZ',
              '025G3ZZ', '025G4ZZ', '025H0ZZ', '025H3ZZ', '025H4ZZ', '025J0ZZ',
              '025J3ZZ', '025J4ZZ', '025K0ZZ', '025K3ZZ', '025K4ZZ', '025L0ZZ', 
              '025L3ZZ', '025L4ZZ', '025M0ZZ', '025M3ZZ', '025M4ZZ', '025N0ZZ', 
              '025N3ZZ', '025N4ZZ', '0270046', '027004Z', '02700D6', '02700DZ',
              '02700T6', '02700TZ', '02700Z6', '02700ZZ', '0270346', '027034Z', 
              '02703D6', '02703DZ', '02703T6', '02703TZ', '02703Z6', '02703ZZ',
              '0270446', '027044Z', '02704D6', '02704DZ', '02704T6', '02704TZ',
              '02704Z6', '02704ZZ', '0271046', '027104Z', '02710D6', '02710DZ',
              '02710T6', '02710TZ', '02710Z6', '02710ZZ', '0271346', '027134Z',
              '02713D6', '02713DZ', '02713T6', '02713TZ', '02713Z6', '02713ZZ',
              '0271446', '027144Z', '02714D6', '02714DZ', '02714T6', '02714TZ',
              '02714Z6', '02714ZZ', '0272046', '027204Z', '02720D6', '02720DZ',
              '02720T6', '02720TZ', '02720Z6', '02720ZZ', '0272346', '027234Z', 
              '02723D6', '02723DZ', '02723T6', '02723TZ', '02723Z6', '02723ZZ',
              '0272446', '027244Z', '02724D6', '02724DZ', '02724T6', '02724TZ',
              '02724Z6', '02724ZZ', '0273046', '027304Z', '02730D6', '02730DZ',
              '02730T6', '02730TZ', '02730Z6', '02730ZZ', '0273346', '027334Z',
              '02733D6', '02733DZ', '02733T6', '02733TZ', '02733Z6', '02733ZZ', 
              '0273446', '027344Z', '02734D6', '02734DZ', '02734T6', '02734TZ',
              '02734Z6', '02734ZZ', '027F04Z', '027F0DZ', '027F0ZZ', '027F34Z',
              '027F3DZ', '027F3ZZ', '027F44Z', '027F4DZ', '027F4ZZ', '027G04Z',
              '027G0DZ', '027G0ZZ', '027G34Z', '027G3DZ', '027G3ZZ', '027G44Z', 
              '027G4DZ', '027G4ZZ', '027H04Z', '027H0DZ', '027H0ZZ', '027H34Z',
              '027H3DZ', '027H3ZZ', '027H44Z', '027H4DZ', '027H4ZZ', '027J04Z',
              '027J0DZ', '027J0ZZ', '027J34Z', '027J3DZ', '027J3ZZ', '027J44Z',
              '027J4DZ', '027J4ZZ', '02890ZZ', '02893ZZ', '02894ZZ', '028D0ZZ',
              '028D3ZZ', '028D4ZZ', '02B40ZZ', '02B43ZZ', '02B44ZZ', '02B50ZZ',
              '02B53ZZ', '02B54ZZ', '02B60ZZ', '02B63ZZ', '02B64ZZ', '02B70ZK',
              '02B70ZZ', '02B73ZK', '02B73ZZ', '02B74ZK', '02B74ZZ', '02B80ZZ',
              '02B83ZZ', '02B84ZZ', '02B90ZZ', '02B93ZZ', '02B94ZZ', '02BD0ZZ',
              '02BD3ZZ', '02BD4ZZ', '02BF0ZZ', '02BF3ZZ', '02BF4ZZ', '02BG0ZZ',
              '02BG3ZZ', '02BG4ZZ', '02BH0ZZ', '02BH3ZZ', '02BH4ZZ', '02BJ0ZZ',
              '02BJ3ZZ', '02BJ4ZZ', '02BK0ZZ', '02BK3ZZ', '02BK4ZZ', '02BL0ZZ',
              '02BL3ZZ', '02BL4ZZ', '02BM0ZZ', '02BM3ZZ', '02BM4ZZ', '02BN0ZZ', 
              '02BN3ZZ', '02BN4ZZ', '02C00ZZ', '02C03ZZ', '02C04ZZ', '02C10ZZ',
              '02C13ZZ', '02C14ZZ', '02C20ZZ', '02C23ZZ', '02C24ZZ', '02C30ZZ',
              '02C33ZZ', '02C34ZZ', '02C40ZZ', '02C43ZZ', '02C44ZZ', '02C50ZZ',
              '02C53ZZ', '02C54ZZ', '02CD0ZZ', '02CD3ZZ', '02CD4ZZ', '02CF0ZZ',
              '02CF3ZZ', '02CF4ZZ', '02CG0ZZ', '02CG3ZZ', '02CG4ZZ', '02CH0ZZ', 
              '02CH3ZZ', '02CH4ZZ', '02CJ0ZZ', '02CJ3ZZ', '02CJ4ZZ', '02CM0ZZ', 
              '02CM3ZZ', '02CM4ZZ', '02H400Z', '02H402Z', '02H403Z', '02H40DZ',
              '02H40JZ', '02H40KZ', '02H40MZ', '02H430Z', '02H432Z', '02H433Z',
              '02H43DZ', '02H43JZ', '02H43KZ', '02H43MZ', '02H440Z', '02H442Z', 
              '02H443Z', '02H44DZ', '02H44JZ', '02H44KZ', '02H44MZ', '02H600Z',
              '02H60JZ', '02H60KZ', '02H60MZ', '02H630Z', '02H63JZ', '02H63KZ', 
              '02H63MZ', '02H640Z', '02H64JZ', '02H64KZ', '02H64MZ', '02H700Z',
              '02H70JZ', '02H70KZ', '02H70MZ', '02H730Z', '02H73JZ', '02H73KZ', 
              '02H73MZ', '02H740Z', '02H74JZ', '02H74KZ', '02H74MZ', '02HA0QZ',
              '02HA0RS', '02HA0RZ', '02HA3QZ', '02HA3RS', '02HA3RZ', '02HA4QZ',
              '02HA4RS', '02HA4RZ', '02HK00Z', '02HK02Z', '02HK0JZ', '02HK0KZ',
              '02HK0MZ', '02HK30Z', '02HK32Z', '02HK3JZ', '02HK3KZ', '02HK3MZ',
              '02HK40Z', '02HK42Z', '02HK4JZ', '02HK4KZ', '02HK4MZ', '02HL00Z',
              '02HL0JZ', '02HL0KZ', '02HL0MZ', '02HL30Z', '02HL3JZ', '02HL3KZ',
              '02HL3MZ', '02HL40Z', '02HL4JZ', '02HL4KZ', '02HL4MZ', '02HN0JZ',
              '02HN0KZ', '02HN0MZ', '02HN3JZ', '02HN3KZ', '02HN3MZ', '02HN4JZ',
              '02HN4KZ', '02HN4MZ', '02HS00Z', '02HS30Z', '02HS40Z', '02HT00Z', 
              '02HT30Z', '02HT40Z', '02HV00Z', '02HV30Z', '02HV40Z', '02L70CK',
              '02L70DK', '02L70ZK', '02L73CK', '02L73DK', '02L73ZK', '02L74CK',
              '02L74DK', '02L74ZK', '02LR0ZT', '02LS0ZZ', '02LT0ZZ', '02N50ZZ',
              '02N53ZZ', '02N54ZZ', '02N90ZZ', '02N93ZZ', '02N94ZZ', '02ND0ZZ',
              '02ND3ZZ', '02ND4ZZ', '02NF0ZZ', '02NF3ZZ', '02NF4ZZ', '02NG0ZZ',
              '02NG3ZZ', '02NG4ZZ', '02NH0ZZ', '02NH3ZZ', '02NH4ZZ', '02NJ0ZZ',
              '02NJ3ZZ', '02NJ4ZZ', '02NK0ZZ', '02NK3ZZ', '02NK4ZZ', '02NL0ZZ',
              '02NL3ZZ', '02NL4ZZ', '02NM0ZZ', '02NM3ZZ', '02NM4ZZ', '02PA0MZ',
              '02PA0QZ', '02PA0RZ', '02PA3MZ', '02PA3QZ', '02PA3RZ', '02PA4MZ',
              '02PA4QZ', '02PA4RZ', '02PAXMZ', '02Q00ZZ', '02Q03ZZ', '02Q04ZZ',
              '02Q10ZZ', '02Q13ZZ', '02Q14ZZ', '02Q20ZZ', '02Q23ZZ', '02Q24ZZ',
              '02Q30ZZ', '02Q33ZZ', '02Q34ZZ', '02Q40ZZ', '02Q43ZZ', '02Q44ZZ',
              '02Q50ZZ', '02Q53ZZ', '02Q54ZZ', '02Q70ZZ', '02Q73ZZ', '02Q74ZZ',
              '02Q90ZZ', '02Q93ZZ', '02Q94ZZ', '02QA0ZZ', '02QA3ZZ', '02QA4ZZ',
              '02QB0ZZ', '02QB3ZZ', '02QB4ZZ', '02QC0ZZ', '02QC3ZZ', '02QC4ZZ',
              '02QD0ZZ', '02QD3ZZ', '02QD4ZZ', '02QF0ZZ', '02QF3ZZ', '02QF4ZZ',
              '02QG0ZZ', '02QG3ZZ', '02QG4ZZ', '02QH0ZZ', '02QH3ZZ', '02QH4ZZ',
              '02QJ0ZZ', '02QJ3ZZ', '02QJ4ZZ', '02QM0ZZ', '02QM3ZZ', '02QM4ZZ',
              '02R907Z', '02R908Z', '02R90JZ', '02R90KZ', '02R947Z', '02R948Z',
              '02R94JZ', '02R94KZ', '02RD07Z', '02RD08Z', '02RD0JZ', '02RD0KZ',
              '02RD47Z', '02RD48Z', '02RD4JZ', '02RD4KZ', '02RF07Z', '02RF08Z', 
              '02RF0JZ', '02RF0KZ', '02RF37H', '02RF37Z', '02RF38H', '02RF38Z',
              '02RF3JH', '02RF3JZ', '02RF3KH', '02RF3KZ', '02RF47Z', '02RF48Z',
              '02RF4JZ', '02RF4KZ', '02RG07Z', '02RG08Z', '02RG0JZ', '02RG0KZ',
              '02RG37H', '02RG37Z', '02RG38H', '02RG38Z', '02RG3JH', '02RG3JZ',
              '02RG3KH', '02RG3KZ', '02RG47Z', '02RG48Z', '02RG4JZ', '02RG4KZ',
              '02RH07Z', '02RH08Z', '02RH0JZ', '02RH0KZ', '02RH37H', '02RH37Z',
              '02RH38H', '02RH38Z', '02RH3JH', '02RH3JZ', '02RH3KH', '02RH3KZ', 
              '02RH47Z', '02RH48Z', '02RH4JZ', '02RH4KZ', '02RJ07Z', '02RJ08Z',
              '02RJ0JZ', '02RJ0KZ', '02RJ47Z', '02RJ48Z', '02RJ4JZ', '02RJ4KZ',
              '02RK07Z', '02RK0JZ', '02RK0KZ', '02RK47Z', '02RK4KZ', '02RL07Z',
              '02RL0JZ', '02RL0KZ', '02RL47Z', '02RL4KZ', '02RM07Z', '02RM0JZ',
              '02RM0KZ', '02RM47Z', '02RM4JZ', '02RM4KZ', '02RP0JZ', '02RQ07Z',
              '02RQ0JZ', '02RR07Z', '02RR0JZ', '02SP0ZZ', '02SW0ZZ', '02T50ZZ',
              '02T53ZZ', '02T54ZZ', '02T80ZZ', '02T83ZZ', '02T84ZZ', '02T90ZZ',
              '02T93ZZ', '02T94ZZ', '02TD0ZZ', '02TD3ZZ', '02TD4ZZ', '02TH0ZZ',
              '02TH3ZZ', '02TH4ZZ', '02TM0ZZ', '02TM3ZZ', '02TM4ZZ', '02TN0ZZ',
              '02TN3ZZ', '02TN4ZZ', '02U507Z', '02U508Z', '02U50JZ', '02U50KZ',
              '02U537Z', '02U538Z', '02U53JZ', '02U53KZ', '02U547Z', '02U548Z',
              '02U54JZ', '02U54KZ', '02U607Z', '02U608Z', '02U60KZ', '02U707Z',
              '02U708Z', '02U70JZ', '02U70KZ', '02U737Z', '02U738Z', '02U73KZ',
              '02U747Z', '02U748Z', '02U74KZ', '02U907Z', '02U908Z', '02U90JZ', 
              '02U90KZ', '02U937Z', '02U938Z', '02U93JZ', '02U93KZ', '02U947Z',
              '02U948Z', '02U94JZ', '02U94KZ', '02UA0JZ', '02UA3JZ', '02UA4JZ',
              '02UD07Z', '02UD08Z', '02UD0JZ', '02UD0KZ', '02UD37Z', '02UD38Z',
              '02UD3JZ', '02UD3KZ', '02UD47Z', '02UD48Z', '02UD4JZ', '02UD4KZ',
              '02UF07Z', '02UF08Z', '02UF0JZ', '02UF0KZ', '02UF37Z', '02UF38Z',
              '02UF3JZ', '02UF3KZ', '02UF47Z', '02UF48Z', '02UF4JZ', '02UF4KZ', 
              '02UG07Z', '02UG08Z', '02UG0JZ', '02UG0KZ', '02UG37Z', '02UG38Z', 
              '02UG3JZ', '02UG3KZ', '02UG47Z', '02UG48Z', '02UG4JZ', '02UG4KZ',
              '02UH07Z', '02UH08Z', '02UH0JZ', '02UH0KZ', '02UH37Z', '02UH38Z',
              '02UH3JZ', '02UH3KZ', '02UH47Z', '02UH48Z', '02UH4JZ', '02UH4KZ',
              '02UJ07Z', '02UJ08Z', '02UJ0JZ', '02UJ0KZ', '02UJ37Z', '02UJ38Z',
              '02UJ3JZ', '02UJ3KZ', '02UJ47Z', '02UJ48Z', '02UJ4JZ', '02UJ4KZ',
              '02UK0KZ', '02UK3KZ', '02UK4KZ', '02UL0KZ', '02UL3KZ', '02UL4KZ', 
              '02UM07Z', '02UM0JZ', '02UM0KZ', '02UM37Z', '02UM38Z', '02UM3JZ',
              '02UM3KZ', '02UM47Z', '02UM48Z', '02UM4JZ', '02UM4KZ', '02VR0ZT',
              '02W50JZ', '02W54JZ', '02WA0JZ', '02WA0MZ', '02WA0QZ', '02WA0RZ',
              '02WA3MZ', '02WA3QZ', '02WA3RZ', '02WA4MZ', '02WA4QZ', '02WA4RZ',
              '02WF07Z', '02WF08Z', '02WF0JZ', '02WF0KZ', '02WF47Z', '02WF48Z',
              '02WF4JZ', '02WF4KZ', '02WG07Z', '02WG08Z', '02WG0JZ', '02WG0KZ',
              '02WG47Z', '02WG48Z', '02WG4JZ', '02WG4KZ', '02WH07Z', '02WH08Z',
              '02WH0JZ', '02WH0KZ', '02WH47Z', '02WH48Z', '02WH4JZ', '02WH4KZ',
              '02WJ07Z', '02WJ08Z', '02WJ0JZ', '02WJ0KZ', '02WJ47Z', '02WJ48Z',
              '02WJ4JZ', '02WJ4KZ', '02WM0JZ', '02WM4JZ', '02YA0Z0', '02YA0Z1',
              '02YA0Z2', '0JH600Z', '0JH604Z', '0JH605Z', '0JH606Z', '0JH607Z',
              '0JH608Z', '0JH609Z', '0JH60AZ', '0JH60PZ', '0JH630Z', '0JH634Z',
              '0JH635Z', '0JH636Z', '0JH637Z', '0JH638Z', '0JH639Z', '0JH63AZ',
              '0JH63PZ', '0JH800Z', '0JH804Z', '0JH805Z', '0JH806Z', '0JH807Z',
              '0JH808Z', '0JH809Z', '0JH80AZ', '0JH80PZ', '0JH830Z', '0JH834Z',
              '0JH835Z', '0JH836Z', '0JH837Z', '0JH838Z', '0JH839Z', '0JH83AZ',
              '0JH83PZ', '0JPT0PZ', '0JPT3PZ', '0JWT0PZ', '0JWT3PZ', '3E07017',
              '3E070PZ', '3E07317', '3E073PZ', '5A02110', '5A02116', '5A02210',
              '5A02216', '5A1213Z', '5A1223Z')
    
    
    keep08 <- data$dx1 %in% c('I0981', 'I110', 'I130', 'I132', 'I501', 'I5020', 
                              'I5021', 'I5022', 'I5023', '15030', 'I5031', 
                              'I5032', 'I5033', 'I5040', 'I5041', 'I5042',
                              'I5043', 'I509') & 
      (!data$px1 %in% exc8 & !data$px2 %in% exc8 & !data$px3 %in% exc8 & 
         !data$px4 %in% exc8 & !data$px5 %in% exc8 & !data$px6 %in% exc8 & 
         !data$px7 %in% exc8 & !data$px8 %in% exc8 & !data$px9 %in% exc8 & 
         !data$px10 %in% exc8) & data$age >= 18
    
    ##PQI 14##
    keep14 <- data$dx1 %in% c('E1065', 'E1165', 'E10649', 'E11649') & data$age >= 18
    
    ##PQI 15##
    ##Exclusions - Cystic fibrosis and anamolies of the respiratory system ##
    
    exc15 <- c('E840', 'E8411', 'E8419', 'E848', 'E849', 'J8483', 'J84841', 'J84842',
               'J84843', 'J84848', 'P270', 'P271', 'P278', 'P279', 'Q254', 'Q311',
               'Q312', 'Q313', 'Q315', 'Q318', 'Q319', 'Q320', 'Q321', 'Q322', 'Q323',
               'Q324', 'Q330', 'Q331', 'Q332', 'Q333', 'Q334', 'Q335', 'Q336', 'Q338',
               'Q339', 'Q340', 'Q341', 'Q348', 'Q349', 'Q390', 'Q391', 'Q392', 'Q393',
               'Q394', 'Q893')
    
    keep15 <- data$dx1 %in% c('J4521', 'J4522', 'J4531', 'J4532', 'J4541', 'J4542',
                              'J4551', 'J4552', 'J45901', 'J45902', 'J45990', 
                              'J45991', 'J45998') &
      (!data$dx1 %in% exc15 & !data$dx2 %in% exc15 & !data$dx3 %in% exc15 & 
         !data$dx4 %in% exc15 & !data$dx5 %in% exc15 & !data$dx6 %in% exc15 & 
         !data$dx7 %in% exc15 & !data$dx8 %in% exc15 & !data$dx9 %in% exc15 & 
         !data$dx10 %in% exc15) & data$age >= 18 & data$age <= 39
    
    ##PQI 16##
    #Include - Amputation##
    
    inc16 <- c('0Y620ZZ', '0Y630ZZ', '0Y640ZZ', '0Y670ZZ', '0Y680ZZ', '0Y6C0Z1',
               '0Y6C0Z2', '0Y6C0Z3', '0Y6D0Z1', '0Y6D0Z2', '0Y6D0Z3', '0Y6F0ZZ',
               '0Y6G0ZZ', '0Y6H0Z1', '0Y6H0Z2', '0Y6H0Z3', '0Y6J0Z1', '0Y6J0Z2',
               '0Y6J0Z3', '0Y6M0Z0', '0Y6M0Z4', '0Y6M0Z5', '0Y6M0Z6', '0Y6M0Z7',
               '0Y6M0Z8', '0Y6M0Z9', '0Y6M0ZB', '0Y6M0ZC', '0Y6M0ZD', '0Y6M0ZF',
               '0Y6N0Z0', '0Y6N0Z4', '0Y6N0Z5', '0Y6N0Z6', '0Y6N0Z7', '0Y6N0Z8',
               '0Y6N0Z9', '0Y6N0ZB', '0Y6N0ZC', '0Y6N0ZD', '0Y6N0ZF')
    
    ##Include Diabetes##
    
    inc16.2 <- c('E1010', 'E1011', 'E1021', 'E1022', 'E1029', 'E10311', 'E10319',
                 'E10321', 'E10329', 'E10331', 'E10339', 'E10341', 'E10349', 
                 'E10351', 'E10359', 'E1036', 'E1039', 'E1040', 'E1041', 'E1042',
                 'E1043', 'E1044', 'E1049', 'E1051', 'E1052', 'E1059', 'E10610',
                 'E10618', 'E10620', 'E10621', 'E10622', 'E10628', 'E10630', 
                 'E10638', 'E10641', 'E10649', 'E1065', 'E1069', 'E108', 'E109',
                 'E1100', 'E1101', 'E1121', 'E1122', 'E1129', 'E11311', 'E11319',
                 'E11321', 'E11329', 'E11331', 'E11339', 'E11341', 'E11349', 
                 'E11351', 'E11359', 'E1136', 'E1139', 'E1140', 'E1141', 'E1142',
                 'E1143', 'E1144', 'E1149', 'E1151', 'E1152', 'E1159', 'E11610',
                 'E11618', 'E11620', 'E11621', 'E11622', 'E11628', 'E11630', 
                 'E11638', 'E11641', 'E11649', 'E1165', 'E1169', 'E118', 'E119',
                 'E1300', 'E1301', 'E1310', 'E1311', 'E1321', 'E1322', 'E1329',
                 'E13311', 'E13319', 'E13321', 'E13329', 'E13331', 'E13339',
                 'E13341', 'E13349', 'E13351', 'E13359', 'E1336', 'E1339', 'E1340',
                 'E1341', 'E1342', 'E1343', 'E1344', 'E1349', 'E1351', 'E1352', 
                 'E1359', 'E13610', 'E13618', 'E13620', 'E13621', 'E13622','E13628',
                 'E13630', 'E13638', 'E13641', 'E13649', 'E1365', 'E1369', 'E138', 
                 'E139')
    
    #exclusions
    exc16 <- c('S78011A', 'S78012A', 'S78019A', 'S78021A', 'S78022A', 'S78029A',
               'S78111A', 'S78112A', 'S78119A', 'S78121A', 'S78122A', 'S78129A',
               'S78911A', 'S78912A', 'S78919A', 'S78921A', 'S78922A', 'S78929A',
               'S88011A', 'S88012A', 'S88019A', 'S88021A', 'S88022A', 'S88029A', 
               'S88111A', 'S88112A', 'S88119A', 'S88121A', 'S88122A', 'S88129A',
               'S88911A', 'S88912A', 'S88919A', 'S88921A', 'S88922A', 'S88929A',
               'S98011A', 'S98012A', 'S98019A', 'S98021A', 'S98022A', 'S98029A',
               'S98111A', 'S98112A', 'S98119A', 'S98121A', 'S98122A', 'S98129A',
               'S98131A', 'S98132A', 'S98139A', 'S98141A', 'S98142A', 'S98149A',
               'S98211A', 'S98212A', 'S98219A', 'S98221A', 'S98222A', 'S98229A',
               'S98311A', 'S98312A', 'S98319A', 'S98321A', 'S98322A', 'S98329A',
               'S98911A', 'S98912A', 'S98919A', 'S98921A', 'S98922A', 'S98929A')
    
    keep16 <- ((data$px1 %in% inc16 | data$px2 %in% inc16 | data$px3 %in% inc16 |
                  data$px4 %in% inc16 | data$px5 %in% inc16 | data$px6 %in% inc16 |
                  data$px7 %in% inc16 | data$px8 %in% inc16 | data$px9 %in% inc16 |
                  data$px10 %in% inc16) & 
                 (data$dx1 %in% inc16.2 | data$dx2 %in% inc16.2 | 
                    data$dx3 %in% inc16.2 | data$dx4 %in% inc16.2 | 
                    data$dx5 %in% inc16.2 | data$dx6 %in% inc16.2 |
                    data$dx7 %in% inc16.2 | data$dx8 %in% inc16.2 | 
                    data$dx9 %in% inc16.2 | data$dx10 %in% inc16.2)) &
      (!data$dx1 %in% exc16 & !data$dx2 %in% exc16 & !data$dx3 %in% exc16 &
         !data$dx4 %in% exc16 & !data$dx5 %in% exc16 & !data$dx6 %in% exc16 &
         !data$dx7 %in% exc16 & !data$dx8 %in% exc16 & !data$dx9 %in% exc16 &
         !data$dx10 %in% exc16) & data$age >= 18 & data$mdc != "14"
    
    keep92 <- keep01 | keep03 | keep05 | keep07 | keep08 | keep14 | keep15 | keep16
    
    PQI.92 <- ifelse(keep92, 1, 0)
    
  } else {
    PQI.92 <- NULL
  }
  ################################################
  #PQI 93 : Prevention Quality Diabetes Composite 
  ################################################
  if ("PQI.93" %in% pqi.match & ICD == "9") {
  ##Includes PQI: 1, 3, 14, 16
  ##PQI 01##
  keep01 <- data$dx1 %in% c('25010', '25011', '25012', '25013', '25020', 
                            '25021', '25022', '25023', '25030', '25031', 
                            '25032', '25033') & data$age >= 18
    
  ##PQI 03##
  keep03 <- data$dx1 %in% c('25040', '25041', '25042', '25043', '25050', 
                            '25051', '25052', '25053', '25060', '25061',
                            '25062', '25063', '25070', '25071', '25072', 
                            '25073', '25080', '25081', '25082', '25083', 
                            '25090', '25091', '25092', '25093') & data$age >= 18
    
  ##PQI 14##
  keep14 <- data$dx1 %in% c('25002', '25003') & data$age >= 18
    
  ##PQI 16##
  ##Include - Amputation##
    
  inc16 <- c('8410', '8412', '8413', '8414', '8415', '8416', '8417', '8418', '8419')
    
  ##Include Diabetes##
    
  inc16.2 <- c('25000', '25001', '25002', '25003', '25010', '25011', '25012', 
               '25013', '25020', '25021', '25022', '25023', '25030', '25031',
               '25032', '25033', '25040', '25041', '25042', '25043', '25050',
               '25051', '25052', '25053', '25060', '25061', '25062', '25063',
               '25070', '25071', '25072', '25073', '25080', '25081', '25082',
               '25083', '25090', '25091', '25092', '25093')
  
  #exclusions
  exc16 <- c('8950', '8951', '8960', '8961', '8962', '8963', '8970', '8971', 
             '8972', '8973', '8974', '8975', '8976', '8977')
    
  keep16 <- ((data$px1 %in% inc16 | data$px2 %in% inc16 | data$px3 %in% inc16 |
              data$px4 %in% inc16 | data$px5 %in% inc16 | data$px6 %in% inc16 |
              data$px7 %in% inc16 | data$px8 %in% inc16 | data$px9 %in% inc16 |
              data$px10 %in% inc16) & 
             (data$dx1 %in% inc16.2 | data$dx2 %in% inc16.2 | 
              data$dx3 %in% inc16.2 | data$dx4 %in% inc16.2 | 
              data$dx5 %in% inc16.2 | data$dx6 %in% inc16.2 |
              data$dx7 %in% inc16.2 | data$dx8 %in% inc16.2 | 
              data$dx9 %in% inc16.2 | data$dx10 %in% inc16.2)) &
    !data$dx1 %in% exc16 & !data$dx2 %in% exc16 & !data$dx3 %in% exc16 &
    !data$dx4 %in% exc16 & !data$dx5 %in% exc16 & !data$dx6 %in% exc16 &
    !data$dx7 %in% exc16 & !data$dx8 %in% exc16 & !data$dx9 %in% exc16 &
    !data$dx10 %in% exc16 & data$age >= 18
    
  keep93 <- keep01 | keep03 | keep14 | keep16
  
  PQI.93 <- ifelse(keep93, 1, 0)
  } else if ("PQI.93" %in% pqi.match & ICD == "10") {
    ##Includes PQI: 1, 3, 14, 16
    
    ##PQI 01##
    keep01 <- data$dx1 %in% c('E1010', 'E1011', 'E10641', 'E1065', 'E1100', 
                              'E1101', 'E11641', 'E1165') & data$age >= 18
    
    ##PQI 03##
    keep03 <- data$dx1 %in% c('E1021', 'E1121', 'E1022', 'E1122', 'E1029',
                              'E1129', 'E10311', 'E11311', 'E10319', 'E11319',
                              'E10321', 'E11321', 'E10329', 'E11329', 'E10331',
                              'E11331', 'E10339', 'E11339', 'E10341', 'E11341',
                              'E10349', 'E11349', 'E10351', 'E11351', 'E10359',
                              'E11359', 'E1036', 'E1136', 'E1039', 'E1139',
                              'E1040', 'E1140', 'E1041', 'E1141', 'E1042', 
                              'E1142', 'E1043', 'E1143', 'E1044', 'E1144', 
                              'E1049', 'E1149', 'E1051', 'E1151', 'E1052',
                              'E1152', 'E1059', 'E1159', 'E10610', 'E11610',
                              'E10618', 'E11618', 'E10620', 'E11620', 'E10621', 
                              'E11621', 'E10622', 'E11622', 'E10628', 'E11628', 
                              'E10630', 'E11630', 'E10638', 'E11638', 'E1069', 
                              'E1169', 'E108', 'E118') & data$age >= 18
    
    ##PQI 14##
    keep14 <- data$dx1 %in% c('E1065', 'E1165', 'E10649', 'E11649') & data$age >= 18
    
    ##PQI 16##
    #Include - Amputation##
    
    inc16 <- c('0Y620ZZ', '0Y630ZZ', '0Y640ZZ', '0Y670ZZ', '0Y680ZZ', '0Y6C0Z1',
               '0Y6C0Z2', '0Y6C0Z3', '0Y6D0Z1', '0Y6D0Z2', '0Y6D0Z3', '0Y6F0ZZ',
               '0Y6G0ZZ', '0Y6H0Z1', '0Y6H0Z2', '0Y6H0Z3', '0Y6J0Z1', '0Y6J0Z2',
               '0Y6J0Z3', '0Y6M0Z0', '0Y6M0Z4', '0Y6M0Z5', '0Y6M0Z6', '0Y6M0Z7',
               '0Y6M0Z8', '0Y6M0Z9', '0Y6M0ZB', '0Y6M0ZC', '0Y6M0ZD', '0Y6M0ZF',
               '0Y6N0Z0', '0Y6N0Z4', '0Y6N0Z5', '0Y6N0Z6', '0Y6N0Z7', '0Y6N0Z8',
               '0Y6N0Z9', '0Y6N0ZB', '0Y6N0ZC', '0Y6N0ZD', '0Y6N0ZF')
    
    ##Include Diabetes##
    
    inc16.2 <- c('E1010', 'E1011', 'E1021', 'E1022', 'E1029', 'E10311', 'E10319',
                 'E10321', 'E10329', 'E10331', 'E10339', 'E10341', 'E10349', 
                 'E10351', 'E10359', 'E1036', 'E1039', 'E1040', 'E1041', 'E1042',
                 'E1043', 'E1044', 'E1049', 'E1051', 'E1052', 'E1059', 'E10610',
                 'E10618', 'E10620', 'E10621', 'E10622', 'E10628', 'E10630', 
                 'E10638', 'E10641', 'E10649', 'E1065', 'E1069', 'E108', 'E109',
                 'E1100', 'E1101', 'E1121', 'E1122', 'E1129', 'E11311', 'E11319',
                 'E11321', 'E11329', 'E11331', 'E11339', 'E11341', 'E11349', 
                 'E11351', 'E11359', 'E1136', 'E1139', 'E1140', 'E1141', 'E1142',
                 'E1143', 'E1144', 'E1149', 'E1151', 'E1152', 'E1159', 'E11610',
                 'E11618', 'E11620', 'E11621', 'E11622', 'E11628', 'E11630', 
                 'E11638', 'E11641', 'E11649', 'E1165', 'E1169', 'E118', 'E119',
                 'E1300', 'E1301', 'E1310', 'E1311', 'E1321', 'E1322', 'E1329',
                 'E13311', 'E13319', 'E13321', 'E13329', 'E13331', 'E13339',
                 'E13341', 'E13349', 'E13351', 'E13359', 'E1336', 'E1339', 'E1340',
                 'E1341', 'E1342', 'E1343', 'E1344', 'E1349', 'E1351', 'E1352', 
                 'E1359', 'E13610', 'E13618', 'E13620', 'E13621', 'E13622','E13628',
                 'E13630', 'E13638', 'E13641', 'E13649', 'E1365', 'E1369', 'E138', 
                 'E139')
    
    #exclusions
    exc16 <- c('S78011A', 'S78012A', 'S78019A', 'S78021A', 'S78022A', 'S78029A',
               'S78111A', 'S78112A', 'S78119A', 'S78121A', 'S78122A', 'S78129A',
               'S78911A', 'S78912A', 'S78919A', 'S78921A', 'S78922A', 'S78929A',
               'S88011A', 'S88012A', 'S88019A', 'S88021A', 'S88022A', 'S88029A', 
               'S88111A', 'S88112A', 'S88119A', 'S88121A', 'S88122A', 'S88129A',
               'S88911A', 'S88912A', 'S88919A', 'S88921A', 'S88922A', 'S88929A',
               'S98011A', 'S98012A', 'S98019A', 'S98021A', 'S98022A', 'S98029A',
               'S98111A', 'S98112A', 'S98119A', 'S98121A', 'S98122A', 'S98129A',
               'S98131A', 'S98132A', 'S98139A', 'S98141A', 'S98142A', 'S98149A',
               'S98211A', 'S98212A', 'S98219A', 'S98221A', 'S98222A', 'S98229A',
               'S98311A', 'S98312A', 'S98319A', 'S98321A', 'S98322A', 'S98329A',
               'S98911A', 'S98912A', 'S98919A', 'S98921A', 'S98922A', 'S98929A')
    
    keep16 <- ((data$px1 %in% inc16 | data$px2 %in% inc16 | data$px3 %in% inc16 |
                  data$px4 %in% inc16 | data$px5 %in% inc16 | data$px6 %in% inc16 |
                  data$px7 %in% inc16 | data$px8 %in% inc16 | data$px9 %in% inc16 |
                  data$px10 %in% inc16) & 
                 (data$dx1 %in% inc16.2 | data$dx2 %in% inc16.2 | 
                    data$dx3 %in% inc16.2 | data$dx4 %in% inc16.2 | 
                    data$dx5 %in% inc16.2 | data$dx6 %in% inc16.2 |
                    data$dx7 %in% inc16.2 | data$dx8 %in% inc16.2 | 
                    data$dx9 %in% inc16.2 | data$dx10 %in% inc16.2)) &
      (!data$dx1 %in% exc16 & !data$dx2 %in% exc16 & !data$dx3 %in% exc16 &
         !data$dx4 %in% exc16 & !data$dx5 %in% exc16 & !data$dx6 %in% exc16 &
         !data$dx7 %in% exc16 & !data$dx8 %in% exc16 & !data$dx9 %in% exc16 &
         !data$dx10 %in% exc16) & data$age >= 18 & data$mdc != "14"
    
    keep93 <- keep01 | keep03 | keep14 | keep16
    
    PQI.93 <- ifelse(keep93, 1, 0)
    
  } else {
    PQI.93 <- NULL
  }
  
  PQI.print <- cbind(PQI.01, PQI.02, PQI.02D, PQI.03, PQI.05, PQI.07, PQI.08, PQI.09, 
                   PQI.10, PQI.11, PQI.12, PQI.14, PQI.15, PQI.16, PQI.90, 
                   PQI.91, PQI.92, PQI.93)
  
  if (append == TRUE) {
    orig.col -> colnames(data) #including original colnames
    return(as.data.frame(cbind(data, PQI.print)))
  } else (return(as.data.frame(cbind(ID, PQI.print))))

}
