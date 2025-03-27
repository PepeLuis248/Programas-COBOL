       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PROVEN. 
      ******************************
      * SISTEMA DE VENCIMIENTOS    *
      * AUTOR: JOSE LUIS PLANES    *
      ******************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           
           SELECT IMPU ASSIGN TO DISK
                  ORGANIZATION IS INDEXED
                  ACCESS IS DYNAMIC 
                  RECORD KEY IS NUM-CO1
                  ALTERNATE RECORD IS TIP-IMP
                  WITH DUPLICATES
                  ALTERNATE RECORD IS FEC-VEN
                  WITH DUPLICATES
                  ALTERNATE RECORD IS FEC-PAG
                  WITH DUPLICATES
                  ALTERNATE RECORD IS ANO
                  WITH DUPLICATES
                  FILE STATUS IS IM.

           SELECT CODI ASSIGN TO DISK
                  ORGANIZATION IS INDEXED
                  ACCESS IS DYNAMIC
                  RECORD KEY IS N-CODI.

           SELECT CONTRI ASSIGN TO DISK
                  ORGANIZATION IS INDEXED
                  ACCESS IS DYNAMIC
                  RECORD KEY IS NUM-CO
                  ALTERNATE RECORD IS NOMBRE
                  WITH DUPLICATES.

       DATA DIVISION.
       FILE SECTION.

       FD  IMPU LABEL RECORD IS STANDARD
           VALUE OF FILE-ID "ARIMPU"
           DATA RECORD IS REG-IMP.

       01  REG-IMP.
           03 NUM-CO1.
              05 CONT            PIC X(25).
              05 PERI.
                 07 AA           PIC 99.
                 07 P1           PIC 99.
           03 TIP-IMP            PIC 99.
           03 FEC-VEN.
              05 DDV             PIC 99.
              05 MMV             PIC 99.
              05 AAV             PIC 99.
           03 FEC-PAG.
              05 DDP             PIC 99.
              05 MMP             PIC 99.
              05 AAP             PIC 99.
           03 MONTO              PIC 9(11)V99.
           03 ANO                PIC 99.

       FD  CONTRI LABEL RECORD IS STANDARD
           VALUE OF FILE-ID "ARCONT"
           DATA RECORD IS REG-CON.

       01  REG-CON.
           03 NUM-CO             PIC X(25).
           03 NOMBRE             PIC X(25).
           03 UBICA              PIC X(25).

       FD  CODI LABEL RECORD IS STANDARD
           VALUE OF FILE-ID "ARCODI"
           DATA RECORD IS REG-CODI.

       01  REG-CODI.
           03 N-CODI             PIC 99.
           03 B-CODI             PIC X(15).

       WORKING-STORAGE SECTION.
       77  OPC                   PIC XX VALUE SPACES. 
       77  GUION                 PIC X(76) VALUE ALL "-".
       77  CAM                   PIC 9 VALUE ZERO.
       77  B                     PIC 9 VALUE ZERO.
       77  SEN                   PIC 9 VALUE ZERO.
       77  IM                    PIC XX.
       77  TIP-VEN0              PIC 99 VALUE ZERO.
       77  FEC-PRO1              PIC 9(06) VALUE ZEROS.
       77  FEC-VEN2              PIC 9(06) VALUE ZEROS.
       77  FEC-PAG2              PIC 9(06) VALUE ZEROS.

       01  FEC-PRO.                        
           03 DDP1               PIC 99 VALUE ZEROS.
           03 MMP1               PIC 99 VALUE ZEROS.
           03 AAP1               PIC 99 VALUE ZEROS.

       01  FEC-VEN1.
           03 DDV1               PIC 99 VALUE ZEROS.
           03 MMV1               PIC 99 VALUE ZEROS.
           03 AAV1               PIC 99 VALUE ZEROS.

       01  FEC-PAG1.
           03 DDP2               PIC 99.
           03 MMP2               PIC 99.
           03 AAP2               PIC 99.

       01  LINCL.
           03 LIN                PIC 99 VALUE ZEROS.
           03 CL                 PIC 99 VALUE ZEROS.

       01  DESPLIEGE.
           03 FILLER             PIC X(09) VALUE SPACES.
           03 CON-D              PIC X(25) VALUE SPACES.
           03 FILLER             PIC X(08) VALUE SPACES.
           03 PER-D              PIC 9(02) VALUE ZEROS.
           03 FILLER             PIC X(01) VALUE "/".
           03 ANO-D              PIC 9(02) VALUE ZEROS.
           03 FILLER             PIC X(10) VALUE SPACES.
           03 DD-D               PIC 9(02) VALUE ZEROS.
           03 FILLER             PIC X(01) VALUE "/".
           03 MM-D               PIC 9(02) VALUE ZEROS.
           03 FILLER             PIC X(01) VALUE "/".
           03 AA-D               PIC 9(02) VALUE ZEROS.

       01  DESPLIEGE1.
           03 FILLER             PIC X(06) VALUE SPACES.
           03 NOM-S              PIC X(25).
           03 FILLER             PIC X(11) VALUE SPACES.
           03 UBI-S              PIC X(25).

       01  S-CODI.
           03 SN-CODI            PIC Z9.             
           03 F                  PIC X VALUE "-".
           03 SB-CODI            PIC X(15) VALUE SPACES.

       SCREEN SECTION.

       01  PANTALLA-VENCIMIENTOS.
           03 LINE 08 COLUMN 18 FOREGROUND-COLOR 4
              VALUE "TIP.VEN. A PRORROGAR:".
           03 LINE 10 COLUMN 18 FOREGROUND-COLOR 4
              VALUE "FECHA DE VENCIMIENTO:".
           03 LINE 12 COLUMN 18 FOREGROUND-COLOR 4
              VALUE "FECHA DE PRORROGA...:".
           03 LINE 14 COLUMN 18 FOREGROUND-COLOR 4
              VALUE "SON CORRECTOS LOS DATOS S/N".

       01  PANTALLA-SAL.
           03 LINE 18 COLUMN 18 REVERSE-VIDEO FOREGROUND-COLOR 6
              VALUE " PARA SALIR TIPEAR 0       ".

       01  PANTALLA-BORRA.
           03 LINE 08 COLUMN 40
              VALUE "  ".
           03 LINE 10 COLUMN 40
              VALUE "           ".
           03 LINE 12 COLUMN 40
              VALUE "           ".
           03 LINE 14 COLUMN 46
              VALUE "   ".         

       01  PANTALLA-TI.
           03 LINE 08 COLUMN 10 FOREGROUND-COLOR 4
              VALUE "NRO DE CLIENTE  ".
           03 LINE 08 COLUMN  45 FOREGROUND-COLOR 4
              VALUE "CUO/A#O".
           03 LINE 08 COLUMN 55 FOREGROUND-COLOR 4
              VALUE "FECHA DE PRORROGA".
           03 LINE 09 COLUMN 10 FOREGROUND-COLOR 4
              VALUE "--- -- -------".
           03 LINE 09 COLUMN 45 FOREGROUND-COLOR 4
              VALUE "-------".
           03 LINE 09 COLUMN 55 FOREGROUND-COLOR 4
              VALUE "----- -- --------".

       01  TF.
           03 LINE 23 COLUMN 03 FOREGROUND-COLOR 6
              VALUE "SI DESEA IMPRIMIR LOS DATOS".
           03 LINE 23 COLUMN 30 FOREGROUND-COLOR 6
              VALUE " TIPEAR LA TECLA PRINT-SCRN, DE LO".
           03 LINE 23 COLUMN 64 FOREGROUND-COLOR 6
              VALUE " CONTRARIO DAR ".
           03 LINE 24 COLUMN 03 FOREGROUND-COLOR 6
              VALUE "ENTER".

       01  TV.
           03 LINE 08 COLUMN 60 FOREGROUND-COLOR 6
              VALUE "TIPO DE VENCIMIENTO".

       PROCEDURE DIVISION.
       INICIO.
           OPEN I-O IMPU CODI CONTRI.

       ENCABEZA.
           CALL "CALEN" USING B.
           DISPLAY GUION AT 0603 WITH FOREGROUND-COLOR 3.

       CAR.
           MOVE SPACES TO OPC.
           DISPLAY PANTALLA-VENCIMIENTOS.
           DISPLAY PANTALLA-SAL.
           DISPLAY TV.
       CAR-C.
           MOVE 9 TO LIN.
           MOVE 61 TO CL.
       CAR-C1.
           READ CODI NEXT RECORD AT END GO TO L-PANT.
           IF N-CODI = 0 GO TO CAR-C1.
           MOVE N-CODI TO SN-CODI.
           MOVE B-CODI TO SB-CODI.
           DISPLAY S-CODI AT LINCL.
           ADD 1 TO LIN.
           IF LIN = 21
           DISPLAY "P/CONTINUAR ENTER" AT 2161
           WITH FOREGROUND-COLOR 6
           MOVE SPACES TO OPC
           ACCEPT OPC AT 2178 WITH PROMPT
           DISPLAY "                  " AT 2161
           PERFORM BL THRU BL-F
           MOVE 9 TO LIN.
           GO TO CAR-C1.

       L-PANT.
           CLOSE CODI.
           OPEN I-O CODI.
           DISPLAY PANTALLA-BORRA.

       D6.
           MOVE ZERO TO TIP-VEN0.
           ACCEPT TIP-VEN0 AT 0840 WITH PROMPT.

       D61.
           IF TIP-VEN0 = 0 GO TO CIERRE.
           MOVE TIP-VEN0 TO N-CODI.
           READ CODI KEY IS N-CODI
           INVALID KEY
           DISPLAY "CODIGO INCORRECTO" AT 0940
           ACCEPT OPC AT 0958 WITH PROMPT
           DISPLAY "                  "  AT 0940
           GO TO L-PANT.

       D7.
           MOVE ZEROS TO FEC-VEN2.
           DISPLAY "          " AT 1040.
           ACCEPT FEC-VEN2 AT 1040 WITH PROMPT.
           MOVE FEC-VEN2 TO FEC-VEN1.
           DISPLAY DDV1 AT 1040.
           DISPLAY "/" AT 1042.
           DISPLAY MMV1 AT 1043.
           DISPLAY "/" AT 1045.
           DISPLAY AAV1 AT 1046.
           IF DDV1 = SPACES GO TO D7.
           IF FEC-VEN1 = SPACES GO TO D7.
           IF MMV1 = SPACES OR
           AAV1 = SPACES GO TO D7.
           IF DDV1 < 0 OR DDV1 > 31 OR DDV1 = 00
           DISPLAY "ERROR EN EL DIA" AT 1140
           WITH FOREGROUND-COLOR 3
           MOVE SPACES TO OPC
           ACCEPT OPC AT 1156
           DISPLAY "                  " AT 1140
           GO TO D7.
           IF MMV1 < 0 OR > 12 OR MMV1 = 00
           DISPLAY "ERROR EN EL MES" AT 1140
           WITH FOREGROUND-COLOR 3
           MOVE SPACES TO OPC
           ACCEPT OPC  AT 1156
           DISPLAY "                  " AT 1140
           GO TO D7.
           PERFORM GRABA1.

       D8.
           MOVE ZEROS TO FEC-PRO1.
           DISPLAY "          " AT 1240.
           ACCEPT FEC-PRO1 AT 1240 WITH PROMPT.
           MOVE FEC-PRO1 TO FEC-PRO.
           DISPLAY  DDP1 AT 1240.
           DISPLAY  "/" AT 1242.
           DISPLAY  MMP1 AT 1243.
           DISPLAY  "/" AT 1245.
           DISPLAY  AAP1 AT 1246.
           IF DDP1 = SPACES GO TO D8.
           IF FEC-PRO = SPACES GO TO D8.
           IF MMP1 = SPACES OR
           AAP1 = SPACES GO TO D8.
           IF DDP1 < 0 OR DDP1 > 31 OR DDP1 = 00
           DISPLAY "ERROR EN EL DIA" AT 1340
           WITH FOREGROUND-COLOR 3
           MOVE SPACES TO OPC
           ACCEPT  OPC AT 1356
           DISPLAY "                  " AT 1340
           GO TO D8.
           IF MMP1 < 0 OR MMP1 > 12 OR MMP1 = 00
           DISPLAY "ERROR EN EL MES" AT 1340
           WITH FOREGROUND-COLOR 3
           MOVE SPACES TO OPC
           ACCEPT  OPC AT 1356
           DISPLAY  "                 " AT 1340
           GO TO D8.
       RES2.
           MOVE SPACES TO OPC.
           ACCEPT OPC AT 1447 WITH PROMPT.
           IF OPC = "S" GO TO INCOR.
           IF OPC = "N" GO TO L-PANT.
           GO TO RES2.
       GRABA1.
           CLOSE IMPU.
           OPEN I-O IMPU.
           MOVE FEC-VEN1 TO FEC-VEN.
           START IMPU KEY IS EQUAL FEC-VEN
           INVALID KEY
           DISPLAY "FECHA-VENCIMIENTO NO ENCONTRADA" AT 1618
           MOVE SPACES TO OPC
           ACCEPT  OPC AT 1651
           DISPLAY "                               "  AT 1618
           GO TO D7.
       INCOR.
           READ IMPU NEXT RECORD AT END
           GO TO INCOR-F.
           IF FEC-VEN = FEC-VEN1 AND TIP-IMP = TIP-VEN0
           GO TO REGRABA.
           IF FEC-VEN = FEC-VEN1 AND TIP-IMP NOT = TIP-VEN0
           GO TO INCOR.
           IF FEC-VEN NOT = FEC-VEN1 GO TO INCOR.
           GO TO INCOR.
       REGRABA.
           MOVE FEC-PRO TO FEC-VEN.
           REWRITE REG-IMP.
           GO TO INCOR.

        INCOR-F.
           CLOSE IMPU.
           OPEN I-O IMPU.
           PERFORM ENCABEZA.
           DISPLAY PANTALLA-TI.
           MOVE FEC-PRO TO FEC-VEN.
           START IMPU KEY IS EQUAL FEC-VEN
           INVALID KEY
           DISPLAY "FECHA O CODIGO DE IMP. ERROSE" AT 1618
           WITH FOREGROUND-COLOR 1
           MOVE SPACES TO OPC
           ACCEPT OPC AT 1651 WITH PROMPT
           DISPLAY "                             " AT 1618
           CLOSE CODI
           OPEN I-O CODI
           GO TO ENCABEZA.
        INCOR-FI.
           MOVE ZEROS TO FEC-PAG1.
           MOVE 9 TO LIN.
           MOVE 3 TO CL.
       RINCOR.
           READ IMPU NEXT RECORD AT END
           PERFORM TER THRU F-TER GO TO CIERRE.
           IF TIP-IMP NOT = TIP-VEN0 GO TO RINCOR.
           IF MMV NOT = MMP1 OR AAV NOT = AAP1
           PERFORM TER THRU F-TER GO TO CIERRE.
           IF MMP NOT = MMP2 OR AAP NOT = AAP2
           GO TO RINCOR.
           MOVE CONT TO CON-D.
           MOVE P1 TO PER-D.
           MOVE AA TO ANO-D.
           MOVE DDV TO DD-D.
           MOVE MMV TO MM-D.
           MOVE AAV TO AA-D.
           ADD 1 TO LIN.
           DISPLAY DESPLIEGE AT LINCL.
           MOVE CONT TO NUM-CO.
           READ CONTRI KEY IS NUM-CO
           INVALID KEY GO TO RINCOR.
           MOVE NOMBRE TO NOM-S.
           MOVE UBICA  TO UBI-S.
           CLOSE CONTRI.
           OPEN I-O CONTRI.
           ADD 1 TO LIN.
           DISPLAY DESPLIEGE1 AT LINCL.
           ADD 1 TO LIN.
           IF LIN = 21 PERFORM TER THRU F-TER.
           GO TO RINCOR.

       BL.
           MOVE 9 TO LIN.
           MOVE 61 TO CL.
       BL1.
           DISPLAY "                  " AT LINCL.
           ADD 1 TO LIN.
           IF LIN = 21 GO TO BL-F.
           GO TO BL1.
       BL-F.

       TER.
           DISPLAY TF.
           MOVE SPACES TO OPC.
           ACCEPT OPC AT 2410.
           PERFORM ENCABEZA.
           MOVE 7 TO LIN.
       F-TER.
           EXIT.
       CIERRE.
           CLOSE IMPU CODI CONTRI.
           CHAIN "MENU".
           STOP RUN.
