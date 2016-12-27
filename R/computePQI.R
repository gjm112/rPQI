#' 
#'
#' @param data the inpute data with diagnosis and procedure codes, object of class data.frame
#' @param pqi Which PQI's to calculate
#' @param append Should we append the output to the original data file.  Default is FALSE.
#' @param diagcode Character string matching: 'dx', 'dr' 
#' @param proccode Character string matching: 'px', 'pr'
#' @param agevar Character string identifying column name of the age variable (not yet included)
#'
#' 
#' 
#' 
#' 
#'
#'

#document(pkg="/Users/gregorymatthews/Dropbox/rPQI")

computePQI <- function(data = data, pqi = c(1:3,5,7:12,14:16,90:93), append = FALSE, diagcode = "dx", proccode = "px", agevar = "age", version = "6.0"){

  data <- as.data.frame(data)
  #rename variables to dx and px.
  # code.name <- colnames(data)
  #if (code == "dx") {
  #}
  
  ################################################
  #PQI 01 : Diabetes Short Term 
  ################################################
  keep01 <- data$dx1 %in% c('25010', '25011', '25012', '25013', '25020', 
                            '25021', '25022', '25023', '25030', '25031', 
                            '25032', '25033')
  
  PQI.01 <- ifelse(keep01, 1, 0)
  
  ################################################
  #PQI 02 : Perforated Appendix 
  ################################################ 
  
  keep02 <- data$dx1 %in% c('5400', '5401') |
    data$dx2 %in% c('5400', '5401') |
    data$dx3 %in% c('5400', '5401') |
    data$dx4 %in% c('5400', '5401') |
    data$dx5 %in% c('5400', '5401') |
    data$dx6 %in% c('5400', '5401') |
    data$dx7 %in% c('5400', '5401') |
    data$dx8 %in% c('5400', '5401') |
    data$dx9 %in% c('5400', '5401') |
    data$dx10 %in% c('5400', '5401')
  
  PQI.02 <- ifelse(keep02, 1, 0)
  
  ################################################
  #PQI 03 : Diabetes Long Term Complications 
  ################################################
  
  keep03 <- data$dx1 %in% c('25040', '25041', '25042', '25043', '25050', 
                            '25051', '25052', '25053', '25060', '25061',
                            '25062', '25063', '25070', '25071', '25072', 
                            '25073', '25080', '25081', '25082', '25083', 
                            '25090', '25091', '25092', '25093')
  
  PQI.03 <- ifelse(keep03, 1, 0)
  
  ################################################
  #PQI 05 : COPD | Asthma in Older Adults 
  ################################################
  
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
    !data$dx10 %in% exc05
  
  PQI.05 <- ifelse(keep05, 1, 0)
  
  ################################################
  #PQI 07 : Hypertension Admission Rate 
  ################################################  
  
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
          !data$px10 %in% exc7.2))
  
  PQI.07 <- ifelse(keep07, 1, 0)
  
  ################################################
  #PQI 08 : Heart Failure  
  ################################################
  
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
       !data$px10 %in% exc8)
  
  PQI.08 <- ifelse(keep08,1,0)
  
  ################################################
  #PQI 09 : Low Birth Weight Rate 
  ################################################
  
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
  keep2<- (data$dx1 %in% birth | data$dx2 %in% birth |data$dx3 %in% birth |
             data$dx4 %in% birth | data$dx5 %in% birth |data$dx6 %in% birth |
             data$dx7 %in% birth | data$dx8 %in% birth |data$dx9 %in% birth |
             data$dx10 %in% birth ) & 
    (!data$dx1 %in% oof & !data$dx2 %in% oof & !data$dx3 %in% oof &
       !data$dx4 %in% oof & !data$dx5 %in% oof & !data$dx6 %in% oof &
       !data$dx7 %in% oof & !data$dx8 %in% oof & !data$dx9 %in% oof &
       !data$dx10 %in% oof)
  
  keep09 <- keep2 & (data$dx1 %in% inc9 | data$dx2 %in% inc9 | data$dx3 %in% inc9 | 
                       data$dx4 %in% inc9 | data$dx5 %in% inc9 | data$dx6 %in% inc9 |
                       data$dx7 %in% inc9 | data$dx8 %in% inc9 | data$dx9 %in% inc9 | 
                       data$dx10 %in% inc9) 
  
  PQI.09 <- ifelse(keep09, 1, 0)
  
  ################################################
  #PQI 10 : Dehydration Admission Rate 
  ################################################
  
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
  
  keep10 <- (A | B) & C
  
  PQI.10 <- ifelse(keep10, 1, 0)
  
  ################################################
  #PQI 11 : Bacterial Pneumonia Admission Rate
  ################################################
  
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
       !data$px10 %in% exc11.3) 
  
  PQI.11 <- ifelse(keep11, 1, 0)
  
  ################################################
  #PQI 12 : Urinary Tract Infection 
  ################################################
  
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
       !data$px10 %in% exc12.3)
  
  PQI.12 <- ifelse(keep12, 1, 0)
  
  ##PQI 13 is retired##
  
  ################################################
  #PQI 14 : Uncontrolled Diabetes
  ################################################
  
  keep14 <- data$dx1 %in% c('25002', '25003')
  
  PQI.14 <- ifelse(keep14, 1, 0)
  
  ################################################
  #PQI 15 : Asthma in Younger Adults
  ################################################
  
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
       !data$dx10 %in% exc15 )
  
  PQI.15 <- ifelse(keep15, 1, 0)
  
  ################################################
  #PQI 16 : Lower Extremity Amputation with Diabetes
  ################################################
  
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
    !data$dx10 %in% exc16
  
  PQI.16 <- ifelse(keep16, 1, 0)
  
  ################################################
  #PQI 90 : Prevention Quality Overall Composite 
  ################################################
  
  ##Includes PQI: 1, 3, 5, 7, 8, 10, 11, 12, 14, 15, 16
  
  keep90 <- keep01 | keep03 | keep05 | keep07 | keep08 | keep10 | keep11 | keep12 |
    keep14 | keep15 | keep16
  
  PQI.90 <- ifelse(keep90, 1, 0)
  
  ################################################
  #PQI 91 : Prevention Quality Acute Composite 
  ################################################
  
  ##Includes PQI: 10, 11, 12
  
  keep91 <- keep10 | keep11 | keep12 
  
  PQI.91 <- ifelse(keep91, 1, 0)
  
  ################################################
  #PQI 92 : Prevention Quality Chronic Composite 
  ################################################
  
  ##Includes PQI: 1, 3, 5, 7, 8, 14, 15, 16
  
  keep92 <- keep01 | keep03 | keep05 | keep07 | keep08 | keep14 | keep15 | keep16
  
  PQI.92 <- ifelse(keep92, 1, 0)
  
  ################################################
  #PQI 93 : Prevention Quality Diabetes Composite 
  ################################################
  
  ##Includes PQI: 1, 3, 14, 16
  
  keep93 <- keep01 | keep03 | keep14 | keep16
  
  PQI.93 <- ifelse(keep93, 1, 0)
  
  
  PQI.fin <- cbind(PQI.01, PQI.02, PQI.03, PQI.05, PQI.07, PQI.08, PQI.09, 
                   PQI.10, PQI.11, PQI.12, PQI.14, PQI.15, PQI.16, PQI.90, 
                   PQI.91, PQI.92, PQI.93)
  ID <- data$id
  
  if (append == TRUE) {
    return(as.data.frame(cbind(data, PQI.fin)))
  } else (return(as.data.frame(cbind(ID, PQI.fin))))

  ##return only selected PQIs

}
