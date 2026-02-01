/******************************************************************
 * PROJECT: Occupational Health Analysis of Fast-Food Chain Workers
 * INSTITUTION: Taichung Veterans General Hospital (TVGH)
 * ANALYST: Shu-Yi (Stella) Wang, Research Assistant
 * PURPOSE: Data Cleaning, Clinical Recoding, and Logistic Regression
 * PUBLICATION: JOEM (2025) - DOI: 10.1097/JOM.0000000000003492
 ******************************************************************/

/* --- SECTION 1: DATA CLEANING & CLINICAL SCALE RECODING --- */

/* Recoding BSRS-5 (Brief Symptom Rating Scale) into severity levels*/
DATA b; 
    SET a.a1;
    IF 0 <= BSRS_5 <= 5  THEN b_score = "1"; /* No depression */
    IF 6 <= BSRS_5 <= 9  THEN b_score = "2"; /* Mild */
    IF 10 <= BSRS_5 <= 14 THEN b_score = "3"; /* Moderate */
    IF BSRS_5 >= 15      THEN b_score = "4"; /* Severe */
RUN;

/* Recoding Copenhagen Burnout Inventory (Personal & Work-related)*/
DATA a.a1;
    SET b;
    /* Personal Burnout (i_score): Mild (0-49), Moderate (50-70), Severe (>70) */
    IF i_score < 50  THEN i_score1 = "0";
    IF 50 <= i_score <= 70 THEN i_score1 = "1";
    IF i_score > 70  THEN i_score1 = "2";
    
    /* Work-related Burnout (j_score): Mild (0-44), Moderate (45-60), Severe (>60) */
    IF j_score < 45  THEN j_score1 = "0";
    IF 45 <= j_score <= 60 THEN j_score1 = "1";
    IF j_score > 60  THEN j_score1 = "2";
RUN;

/* Grouping Musculoskeletal Pain sites into anatomical regions  */
DATA part;
    SET a.a1;
    /* Group 1: Trunk (Neck, Upper Back, Lower Back) */
    IF _COL17=1 OR _COL19=1 OR _COL20=1 THEN GROUP1=1; ELSE GROUP1=0;
    /* Group 2: Upper Extremities (Shoulders, Elbows, Wrists) */
    IF _COL18=1 OR _COL21=1 OR _COL22=1 THEN GROUP2=1; ELSE GROUP2=0;
    /* Group 3: Lower Extremities (Hips, Knees, Ankles) */
    IF _COL23=1 OR _COL24=1 OR _COL25=1 THEN GROUP3=1; ELSE GROUP3=0;
RUN;

/* --- SECTION 2: EXPLORATORY DATA ANALYSIS (EDA) --- */

/* Checking normality and distribution for continuous variables */
/* This step ensures the validity of using medians for categorization */
PROC UNIVARIATE DATA=part normal plot;
    VAR _col1 _col2 _col4 _col6; /* Seniority, Age, Work hours, Sleep */
RUN;

/* PROC MEANS for Table 1: Demographic and Clinical Characteristics*/
PROC MEANS DATA=part MEAN N STD MEDIAN MAXDEC=3;
    VAR _col1 _col2 _col4 _col6;
RUN;

/* --- SECTION 3: BIVARIATE ANALYSIS (CHI-SQUARE) --- */

/* Testing relationship between pain regions and Burnout/Depression levels  */
PROC FREQ DATA=part;
    TABLES (GROUP1 GROUP2 GROUP3) * (b_score i_score1 j_score1) / CHISQ;
RUN;

/* Macro for individual pain location association tests */
%MACRO run_chisq(var);
    PROC FREQ DATA=part;
        TABLES j_score1 * &var / CHISQ;
    RUN;
%MEND;
%run_chisq(_COL17); /* Neck pain */
%run_chisq(_COL18); /* Shoulder pain */

/* --- SECTION 4: MULTIVARIATE LOGISTIC REGRESSION --- */

/* Creating binary outcomes for Regression Models */
DATA logit_prep;
    SET part;
    IF b_score > 2 THEN BGROUP=1; ELSE BGROUP=0;   /* High Depression Risk */
    IF i_score1 > 0 THEN IGROUP=1; ELSE IGROUP=0; /* High Personal Burnout Risk */
    IF j_score1 > 0 THEN JGROUP=1; ELSE JGROUP=0; /* High Work Burnout Risk */
RUN;

/* Multivariate Model: Predicting Work-related Burnout */
/* Adjusts for age, gender, seniority, work hours, sleep, and chronic disease */
PROC LOGISTIC DATA=logit_prep DESCENDING;
    CLASS GROUP1(REF="0") _COL3(REF="1") _COL8(REF="0") / PARAM=REF;
    MODEL JGROUP = GROUP1 _COL1 _COL2 _COL3 _COL4 _COL6 _COL8 _COL9 DIABETES HIGHPRE;
RUN;

/* --- SECTION 5: SUBGROUP STRATIFICATION --- */

/* 5a.Stratification by Gender  */
DATA male female;
    SET logit_prep;
    IF _col3=1 THEN OUTPUT male; /* Male */
    ELSE OUTPUT female;         /* Female */
RUN;

PROC LOGISTIC DATA=female DESCENDING;
    TITLE "Predicting Depression Risk in Female Employees";
    MODEL BGROUP = GROUP1 GROUP2 GROUP3 _COL1 _COL2 _COL4 _COL6 _COL8 _COL9;
RUN;

/* 5b. Stratification by Seniority (Median Split = 22 months)  */
DATA seniority_split;
    SET logit_prep;
    IF _col1 < 22 THEN seniority_cat=1;
    ELSE seniority_cat=2;
RUN;

PROC LOGISTIC DATA=seniority_split DESCENDING;
    WHERE seniority_cat=2; /* Analysis for experienced workers subgroup */
    TITLE "Work Burnout Risks for Senior Staff (>= 22 Months)";
    MODEL JGROUP = GROUP1 GROUP2 GROUP3 _COL2 _COL4 _COL6 _COL8 _COL9;
RUN;