       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CONSUL. 
      ******************************
      * SISTEMA DE VENCIMIENTOS    *
      * AUTOR: JOSE LUIS PLANES    *
      ******************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CONTRI ASSIGN TO DISK
                  ORGANIZATION IS INDEXED
                  ACCESS IS DYNAMIC
                  RECORD KEY IS NUM-CO
                  ALTERNATE RECORD IS NOMBRE
                  WITH DUPLICATES.

           SELECT IMPU ASSIGN TO DISK
                  ORGANIZATION IS INDEXED
                  ACCESS IS DYNAMIC
                  RECORD KEY IS NUM-CO1
                  ALTERNATE RECORD IS TIP-VEN
                  WITH DUPLICATES
                  ALTERNATE RECORD IS FEC-VEN
                  WITH DUPLICATES
                  ALTERNATE RECORD IS FEC-PAG
                  WITH DUPLICATES
                  ALTERNATE RECORD IS ANO
                  WITH DUPLICATES.

           SELECT CODI ASSIGN TO DISK
                  ORGANIZATION IS INDEXED
                  ACCESS IS DYNAMIC
                  RECORD KEY IS N-CODI.

           SELECT IMPRE ASSIGN TO PRINTER.

           SELECT RDATOS ASSIGN TO DISK.
       DATA DIVISION.
       FILE SECTION.

       FD  CONTRI LABEL RECORD IS STANDARD
           VALUE OF FILE-ID "ARCONT"
           DATA RECORD IS REG-CON.

       01  REG-CON.
           03 NUM-CO                  PIC X(25).
           03 NOMBRE                  PIC X(25).
           03 UBICA                   PIC X(25).

       FD  IMPU LABEL RECORD IS STANDARD
           VALUE OF FILE-ID "ARIMPU"
           DATA RECORD IS REG-IMP.

       01  REG-IMP.
           03 NUM-CO1.
              05 CONT            PIC X(25).
              05 PERI.
                 07 AA           PIC 99.
                 07 P1           PIC 99.
           03 TIP-VEN            PIC 99.
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

       FD  IMPRE LABEL RECORD IS OMITTED.

       01  RENGLON               PIC X(132).

       FD  RDATOS LABEL RECORD IS STANDARD
           VALUE OF FILE-ID "ARDATOS"
           DATA RECORD IS REG-DATOS.

       01  REG-DATOS.
           03 NOMEMP             PIC X(40).
           03 CIUD               PIC X(40).

       FD  CODI LABEL RECORD IS STANDARD
           VALUE OF FILE-ID "ARCODI"
           DATA RECORD IS REG-CODI.

       01  REG-CODI.
           03 N-CODI             PIC 99.
           03 B-CODI             PIC X(15).

       WORKING-STORAGE SECTION.
       77  OPC                   PIC XX VALUE SPACES. 
       77  GUION                 PIC X(77) VALUE ALL "-".
       77  CAM                   PIC 9 VALUE ZERO.
       77  B                     PIC 9 VALUE ZERO.
       77  SEN                   PIC 9 VALUE ZERO.
       77  CON-LI                PIC 999 VALUE ZEROS.
       77  CON-HO                PIC 9999 VALUE ZEROS.
       77  ANO-P                 PIC XX VALUE SPACES.
       77  CON-RE                PIC 9999 VALUE ZEROS.
       77  NUM-CO2               PIC X(25) VALUE SPACES.
       77  FEC-VEN2              PIC 9(06) VALUE ZEROS.
       77  B-NOM                 PIC X VALUE SPACES.

       01  LINCL.
           03  LIN               PIC 99.
           03  CL                PIC 99.

       01  FECHA.
           03 AAR                PIC 99.
           03 MMR                PIC 99.
           03 DDR                PIC 99.

       01  FEC-VEN1.
           03 DDV1               PIC 99 VALUE ZEROS.
           03 MMV1               PIC 99 VALUE ZEROS.
           03 AAV1               PIC 99 VALUE ZEROS.

       01  DESPLIEGE.
           03 FILLER             PIC X(07) VALUE SPACES.
           03 CON-D              PIC X(25) VALUE SPACES.
           03 FILLER             PIC X(11) VALUE SPACES.
           03 PER-D              PIC 9(02) VALUE ZEROS.
           03 FILLER             PIC X(01) VALUE "/".
           03 ANO-D              PIC 9(02) VALUE ZEROS.
           03 FILLER             PIC X(07) VALUE SPACES.
           03 DD-D               PIC 9(02) VALUE ZEROS.
           03 FILLER             PIC X(01) VALUE "/".
           03 MM-D               PIC 9(02) VALUE ZEROS.
           03 FILLER             PIC X(01) VALUE "/".
           03 AA-D               PIC 9(02) VALUE ZEROS.

       01  DESPLIEGE1.
           03 CON-E              PIC X(24) VALUE SPACES.
           03 PER-E              PIC 9(02) VALUE ZEROS.
           03 F                  PIC X     VALUE "/".
           03 ANO-E              PIC 9(02) VALUE ZEROS.
           03 F                  PIC XXXX  VALUE SPACES.
           03 TIP-E              PIC ZZ    VALUE ZEROS.
           03 F                  PIC X(05) VALUE SPACES.
           03 DDV-E              PIC XX    VALUE SPACES.
           03 F                  PIC X     VALUE "/".
           03 MMV-E              PIC XX    VALUE SPACES.
           03 F                  PIC X     VALUE "/".
           03 AAV-E              PIC XX    VALUE SPACES.
           03 F                  PIC XXX   VALUE SPACES.
           03 DDP-E              PIC XX    VALUE SPACES.
           03 F                  PIC X     VALUE "/".
           03 MMP-E              PIC XX    VALUE SPACES.
           03 F                  PIC X     VALUE "/".
           03 AAP-E              PIC XX    VALUE SPACES.
           03 MONTO-E            PIC    ZZZ,ZZZ,ZZ9.99.

       01  DESPLIEGE2.
           03 F                  PIC X(03) VALUE SPACES.
           03 CONTRI-D           PIC X(25).
           03 F                  PIC X(04) VALUE SPACES.
           03 NOM-SA             PIC X(25).

       01  DESPLIEGE211.
           03 F                  PIC X(18) VALUE SPACES.
           03 CONTRI-D1          PIC X(25).
           03 F                  PIC X(04) VALUE SPACES.
           03 NOM-SA1            PIC X(25).

       01  DESPLIEGE21.
           03 F                  PIC X(03) VALUE SPACES.
           03 UBI-SA             PIC X(25).

       01  DESPLIEGE2111.
           03 F                  PIC X(18) VALUE SPACES.
           03 UBI-SA1            PIC X(25).

       01  T1.
           03 F                  PIC X(80) VALUE ALL "*".

       01  T2.
           03 F                  PIC X(80) VALUE SPACES.

       01  T3.
           03 F  PIC X(35) VALUE "* SISTEMA DE VENCIMIENTOS          ".
           03 F  PIC X(35) VALUE "                            FECHA: ".
           03 DDI PIC 99.
           03 F   PIC X VALUE "/".
           03 MMI PIC 99.
           03 F   PIC X VALUE "/".
           03 AAI PIC 99.
           03 F   PIC XX VALUE " *".

       01  T31.
           03 F  PIC X(02) VALUE "* ".
           03 S-NOMEMP PIC X(40).
           03 F  PIC X(37) VALUE SPACES.
           03 F  PIC X VALUE "*".

       01  T4.
           03 F  PIC X(02) VALUE "* ".
           03 S-CIUD  PIC X(40).
           03 F  PIC X(28) VALUE "                       HOJA ".
           03 F  PIC X(04) VALUE "NRO:".
           03 HO PIC ZZZ9.
           03 F  PIC XX    VALUE " *".

       01  T5.
           03 F  PIC X(35) VALUE "NRO DE CLIENTE         CUO/A#O  TIP".
           03 F  PIC X(35) VALUE "-VENCI      FEC-VEN    FEC-PAG     ".
           03 F  PIC X(10) VALUE "     MONTO".

       01  T6.
           03 F  PIC X(35) VALUE "--- -- -------         -------  ---".
           03 F  PIC X(35) VALUE "------      -------    -------     ".
           03 F  PIC X(10) VALUE "     -----".

       01  DESPLIEGE3.
           03 CON-S              PIC X(24).
           03 PER-S              PIC 99.
           03 F                  PIC X VALUE "/".
           03 ANO-S              PIC 99.
           03 F                  PIC X(06) VALUE SPACES.
           03 TI-S               PIC 99.
           03 F                  PIC X(10) VALUE SPACES.
           03 DD-S               PIC 99.
           03 F                  PIC X VALUE "/".
           03 MM-S               PIC 99.
           03 F                  PIC X VALUE "/".
           03 AA-S               PIC 99.
           03 F                  PIC XXX VALUE SPACES.
           03 DD-S1              PIC 99.
           03 F                  PIC X VALUE "/".
           03 MM-S1              PIC 99.
           03 F                  PIC X VALUE "/".
           03 AA-S1              PIC 99.
           03 MONTO-S            PIC ZZZ,ZZZ,ZZ9.99.

       01  TOT-RE.
           03 F       PIC X(19)  VALUE "TOTAL DE REGISTROS:".
           03 RE-TOT  PIC ZZZ9.

       01  DESPLIEGE4.
           03 F       PIC X(07) VALUE SPACES.
           03 NOM-S   PIC X(25).
           03 F       PIC X(11) VALUE SPACES.
           03 UBI-S   PIC X(25).

       01  DESPLIEGE5.
           03 NOM1-S  PIC X(25).
           03 F       PIC X(03) VALUE SPACES.
           03 UBI1-S  PIC X(25).

       01  DESPLIEGE6.
           03 F       PIC X(30) VALUE SPACES.
           03 SN-CODI PIC Z9.
           03 F       PIC X(08) VALUE SPACES.
           03 SB-CODI PIC X(15).

       01  DESPLIEGE7.
           03 F PIC X(07) VALUE SPACES.
           03 F PIC X(16) VALUE "NRO DE CLIENTE".
           03 F PIC X(19) VALUE SPACES.
           03 F PIC X(07) VALUE "CUO/A#O".
           03 F PIC X(03) VALUE SPACES.
           03 F PIC X(20) VALUE "FECHA DE VENCIMIENTO".

       01  DESPLIEGE8.
           03 F PIC X(07) VALUE SPACES.
           03 F PIC X(16) VALUE "--- -- -------".
           03 F PIC X(19) VALUE SPACES.
           03 F PIC X(07) VALUE "-------".
           03 F PIC X(03) VALUE SPACES.
           03 F PIC X(20) VALUE "----- -- -----------".

       01  DESPLIEGE9.
           03 SN1-COD     PIC Z9.
           03 FILLER      PIC X VALUE "-".
           03 SB1-COD     PIC X(15).

       01  DESPLIEGE10.
           03 FILLER      PIC X(31) VALUE SPACES.
           03 SN2-COD     PIC Z9.
           03 FILLER      PIC X VALUE "-".
           03 SB2-COD     PIC X(15).

       SCREEN SECTION.

       01  PANTALLA-MENU.
           03 LINE 08 COLUMN 22 REVERSE-VIDEO FOREGROUND-COLOR 4
              VALUE "             MENU                ".
           03 LINE 10 COLUMN 22
              VALUE "1-VENCIMIENTOS POR FECHA".
           03 LINE 11 COLUMN 22
              VALUE "2-VENCIMIENTOS-LISTADO TOTAL".
           03 LINE 12 COLUMN 22
              VALUE "3-LISTADO DE TIPO DE VENCIMIENTOS".
           03 LINE 13 COLUMN 22
              VALUE "4-LISTADO TOTAL DE CLIENTES".
           03 LINE 14 COLUMN 22
              VALUE "5-LISTADO POR A#O".
           03 LINE 15 COLUMN 22
              VALUE "6-REGRESO MENU PRINCIPAL".
           03 LINE 17 COLUMN 22 REVERSE-VIDEO FOREGROUND-COLOR 4
              VALUE "     INGRESE OPCION:             ".

       01  PANTALLA-CONTRI.
           03 LINE 11 COLUMN 18 FOREGROUND-COLOR 5
              VALUE "NRO DE CLIENTE :".

       01  PANTALLA-SAL.
           03 LINE 13 COLUMN 18 REVERSE-VIDEO FOREGROUND-COLOR 4
              VALUE " PARA SALIR TIPEAR 0       ".

       01  PANTALLA-ANO.
           03 LINE 11 COLUMN 18 FOREGROUND-COLOR 5
              VALUE "A#O VEN. PAGADOS Y POR VENCER".

       01  PANTALLA-TI.
           03 LINE 07 COLUMN 10 FOREGROUND-COLOR 5
              VALUE "NRO DE CLIENTE      ".
           03 LINE 07 COLUMN  45 FOREGROUND-COLOR 5
              VALUE "CUO/A#O".
           03 LINE 07 COLUMN 55 FOREGROUND-COLOR 5
              VALUE "FECHA DE VENCIMIENTO".
           03 LINE 08 COLUMN 10 FOREGROUND-COLOR 5
              VALUE "--- -- -------      ".
           03 LINE 08 COLUMN 45 FOREGROUND-COLOR 5
              VALUE "-------".
           03 LINE 08 COLUMN 55 FOREGROUND-COLOR 5
              VALUE "----- -- -----------".

       01  PANTALLA-VENCI.
           03 LINE 09 COLUMN 18 FOREGROUND-COLOR 5
              VALUE "FECHA DE VENCIMIENTO:".
           03 LINE 11 COLUMN 18 FOREGROUND-COLOR 5
              VALUE "ES CORRECTA LA FECHA  S/N".

       01  I-TER.
           03 LINE 22 COLUMN 03 FOREGROUND-COLOR 6
              VALUE "SI DESEA IMPRIMIR LOS DATOS TIPEAR LA ".
           03 LINE 22 COLUMN 41 FOREGROUND-COLOR 6
              VALUE "TECLA PRINT-SCREEN, DE LO CONTRARIO".
           03 LINE 23 COLUMN 03 FOREGROUND-COLOR 6
              VALUE "DAR RETURN".

       01  PANTALLA-IMP.
           03 LINE 12 COLUMN 10 FOREGROUND-COLOR 2
              VALUE "1=LISTA POR PANTALLA  2=LISTA POR IMPRESORA".
           03 LINE 12 COLUMN 55 FOREGROUND-COLOR 2
              VALUE "3=CANCELA".

       01  PANTALLA-IMP1.
           03 LINE 12 COLUMN 10 FOREGROUND-COLOR 2
              VALUE "1=LISTA POR NRO CLIENTE".
           03 LINE 12 COLUMN 36 FOREGROUND-COLOR 2
              VALUE "2-LISTA ALFABETICAMENTE".
           03 LINE 12 COLUMN 62 FOREGROUND-COLOR 2
              VALUE "3=CANCELA".

       01  PANTALLA-IMP11.
           03 LINE 12 COLUMN 10 FOREGROUND-COLOR 2
              VALUE "1-LISTA POR NRO DE CLIENTE ".
           03 LINE 12 COLUMN 38 FOREGROUND-COLOR 2
              VALUE "2-LISTA ALFABETICAMENTE".
           03 LINE 13 COLUMN 10 FOREGROUND-COLOR 2
              VALUE "3-LISTA LETRA INICIAL APELLIDO  4-CANCELA".

       01  NO-LETRA.
           03 LINE 14 COLUMN 10 FOREGROUND-COLOR 4
              VALUE "LETRA INICIAL NO ENCONTRADA".

       PROCEDURE DIVISION.
       INICIO.
           OPEN I-O IMPU.
           OPEN I-O CONTRI.
           OPEN INPUT RDATOS.
           READ RDATOS AT END.
           MOVE NOMEMP TO S-NOMEMP.
           MOVE CIUD TO S-CIUD.
           CLOSE RDATOS.

       ENCABEZA.
           CALL "CALEN" USING B.
           DISPLAY GUION AT 0603 WITH FOREGROUND-COLOR 3.

       CAR.
           DISPLAY PANTALLA-MENU.
       L-PANT.
           MOVE SPACES TO OPC.
           ACCEPT OPC AT 1759.
           IF OPC = "1" GO TO VENCI.
           IF OPC = "2" GO TO VENCILT.
           IF OPC = "3" GO TO TVEN.
           IF OPC = "4" GO TO CONTRILT.
           IF OPC = "5" GO TO LPANO.
           IF OPC = "6" GO TO CIERRE.
           GO TO L-PANT.

       VENCI.
           PERFORM ENCABEZA.
           DISPLAY "FECHA DE VENCIMIENTO:" AT 0918.
           DISPLAY "ES CORRECTA LA FECHA    S/N" AT 1118.
           DISPLAY PANTALLA-SAL.

       D7.
           DISPLAY "   " AT 1147.
           MOVE ZEROS TO FEC-VEN2.
           DISPLAY "          " AT 0940.
           ACCEPT FEC-VEN2 AT 0940 WITH PROMPT.
           IF FEC-VEN2 = 0 GO TO ENCABEZA.
           MOVE FEC-VEN2 TO FEC-VEN1.
           DISPLAY DDV1 AT 0940.
           DISPLAY "/"  AT 0942
           DISPLAY MMV1 AT 0943.
           DISPLAY "/"  AT 0945.
           DISPLAY AAV1 AT 0946.
           IF DDV1 = SPACES GO TO D7.
           IF FEC-VEN1 = SPACES GO TO D7.
           IF MMV1 = SPACES OR
           AAV1 = ZEROS GO TO D7.
           IF DDV1 < 0 OR DDV1 > 31 OR DDV1 = 00
           DISPLAY "ERROR EN EL DIA" AT 0949
           WITH FOREGROUND-COLOR 6
           MOVE SPACES TO OPC
           ACCEPT OPC AT 0965
           DISPLAY "                           " AT 0949
           GO TO D7.
           IF MMV1 < 0 OR > 12 OR MMV1 = 00
           DISPLAY "ERROR EN EL MES" AT 0949
           WITH FOREGROUND-COLOR 6
           MOVE SPACES TO OPC
           ACCEPT OPC AT 0965
           DISPLAY "                           " AT 0949
           GO TO D7.

       RES2.
           MOVE SPACES TO OPC.
           ACCEPT OPC AT 1147.
           IF OPC = "S" GO TO VEN-S.
           IF OPC = "N" GO TO D7.
           GO TO RES2.
       VEN-S.
           CLOSE IMPU.
           OPEN I-O IMPU.
           MOVE FEC-VEN1 TO FEC-VEN.
           START IMPU KEY IS EQUAL FEC-VEN
           INVALID KEY
           DISPLAY "FECHA-VENCIMIENTO INEXISTENTE" AT 1618
           WITH FOREGROUND-COLOR 6
           MOVE SPACES TO OPC
           ACCEPT OPC AT 1650
           DISPLAY "                               "  AT 1618
           GO TO D7.

       INCOR-FI1.
           PERFORM ENCABEZA.
           DISPLAY PANTALLA-TI.
           MOVE 8 TO LIN.
           MOVE 3 TO CL.
       RINCOR1.
           READ IMPU NEXT RECORD AT END
           PERFORM TER THRU F-TER GO TO ENCABEZA.
           IF FEC-PAG NOT = 000000 GO TO RINCOR1.
           IF FEC-VEN NOT = FEC-VEN1 GO TO RINCOR1.
           MOVE CONT TO CON-D.
           MOVE P1 TO PER-D.
           MOVE AA TO ANO-D.
           MOVE DDV TO DD-D.
           MOVE MMV TO MM-D.
           MOVE AAV TO AA-D.
           ADD 1 TO LIN.
           DISPLAY DESPLIEGE AT LINCL.
           MOVE NUM-CO1 TO NUM-CO.
           READ CONTRI KEY IS NUM-CO
           INVALID KEY GO TO RINCOR1.
           MOVE NOMBRE TO NOM-S.
           MOVE UBICA  TO UBI-S.
           CLOSE CONTRI.
           OPEN I-O CONTRI.
           ADD 1 TO LIN.
           DISPLAY DESPLIEGE4 AT LINCL.
           ADD 1 TO LIN.
           IF LIN = 20 PERFORM TER THRU F-TER.
           GO TO RINCOR1.

       VENCILT.
           PERFORM ENCABEZA.
           MOVE 000000 TO FEC-PAG.
           START IMPU KEY IS EQUAL FEC-PAG
           INVALID KEY
           DISPLAY "NO EXISTEN VENCIMIENTOS" AT 1430
           MOVE SPACES TO OPC
           ACCEPT OPC AT 1454
           GO TO ENCABEZA.

       INCOR-FI.
           PERFORM ENCABEZA.
           DISPLAY PANTALLA-IMP.
           MOVE SPACES TO OPC.
           ACCEPT OPC AT 1266.
           IF OPC = 1 GO TO INCOR-F11.
           IF OPC = 2 GO TO INCOR-FI2.
           IF OPC = 3 GO TO ENCABEZA.
           GO TO INCOR-FI.

       INCOR-F11.
           PERFORM ENCABEZA.
           DISPLAY PANTALLA-TI.
           MOVE 8 TO LIN.
           MOVE 3 TO CL.
       RINCOR.
           READ IMPU NEXT RECORD AT END
           PERFORM TER THRU F-TER GO TO ENCABEZA.
           IF FEC-PAG NOT = 000000 GO TO RINCOR.
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
           DISPLAY DESPLIEGE4 AT LINCL.
           ADD 1 TO LIN.
           IF LIN = 20 PERFORM TER THRU F-TER
           GO TO INCOR-F11.
           GO TO RINCOR.

       INCOR-FI2.
           OPEN OUTPUT IMPRE.
           PERFORM REGRESE1.
       IT.
           PERFORM IMPRIME3.
           WRITE RENGLON FROM DESPLIEGE7 AFTER 1.
           WRITE RENGLON FROM DESPLIEGE8 AFTER 1.
           ADD 2 TO CON-LI.
       IT1.
           READ IMPU NEXT RECORD AT END
           MOVE CON-RE TO RE-TOT
           WRITE RENGLON FROM TOT-RE AFTER 2
           CLOSE IMPRE
           GO TO ENCABEZA.
           MOVE CONT TO CON-D.
           IF FEC-PAG NOT = 000000 GO TO IT1.
           MOVE P1 TO PER-D.
           MOVE AA TO ANO-D.
           MOVE DDV TO DD-D.
           MOVE MMV TO MM-D.
           MOVE AAV TO AA-D.
           WRITE RENGLON FROM DESPLIEGE AFTER 2.
           MOVE CONT TO NUM-CO.
           READ CONTRI KEY NUM-CO
           INVALID KEY GO TO RINCOR.
           MOVE NOMBRE TO NOM-S.
           MOVE UBICA TO UBI-S.
           CLOSE CONTRI.
           OPEN I-O CONTRI.
           WRITE RENGLON FROM DESPLIEGE4 AFTER 1.
           ADD 1 TO CON-RE.
           ADD 3 TO CON-LI.
           IF CON-LI = 59 MOVE ZEROS TO CON-LI GO TO IT.
           GO TO IT1.

       TER.
           DISPLAY I-TER.
           MOVE SPACES TO OPC.
           ACCEPT OPC AT 2314.
           DISPLAY "                           " AT 2203.
           DISPLAY "                              " AT 2228.
           DISPLAY "                               " AT 2303.
           PERFORM ENCABEZA.
           MOVE 7 TO LIN.
       F-TER.
           EXIT.

       TVEN.
           OPEN I-O CODI.
           PERFORM ENCABEZA.
       PCONT.
           DISPLAY PANTALLA-IMP.
           MOVE SPACES TO OPC.
           ACCEPT OPC AT 1266.
           IF OPC = 1 GO TO PCONT1.
           IF OPC = 2 GO TO PCONT2.
           IF OPC = 3 CLOSE CODI GO TO ENCABEZA.
           GO TO PCONT.

       PCONT1.
           PERFORM ENCABEZA.
           MOVE 7 TO LIN.
           MOVE 3 TO CL.
       RINCOR3.
           READ CODI NEXT RECORD AT END
           PERFORM TER THRU F-TER CLOSE CODI
           GO TO ENCABEZA.
           IF N-CODI = 0 GO TO RINCOR3.
       RINCOR11.
           MOVE N-CODI TO SN1-COD.
           MOVE B-CODI TO SB1-COD.
           ADD 1 TO LIN.
           DISPLAY DESPLIEGE9 AT LINCL
           IF LIN = 20 ADD 19 TO CL
                    MOVE 7 TO LIN.
           IF CL = 79 PERFORM TER THRU F-TER
           GO TO PCONT1.
           GO TO RINCOR3.

       PCONT2.
           OPEN OUTPUT IMPRE.
           ACCEPT FECHA FROM DATE.
           MOVE DDR TO DDI.
           MOVE MMR TO MMI.
           MOVE AAR TO AAI.
           MOVE 0 TO CON-LI CON-RE CON-HO.
       IMPRIME3.
           ADD 1 TO CON-HO.
           MOVE CON-HO TO HO.
           IF CON-HO = 1 WRITE RENGLON FROM T1 AFTER 1
           ELSE
           WRITE RENGLON FROM T1 AFTER PAGE.
           WRITE RENGLON FROM T3 AFTER 1.
           WRITE RENGLON FROM T31 AFTER 1.
           WRITE RENGLON FROM T4 AFTER 1.
           WRITE RENGLON FROM T1 AFTER 1.
           WRITE RENGLON FROM T2 AFTER 1.
           MOVE 6 TO CON-LI.
       RINCOR31.
           READ CODI NEXT RECORD AT END GO TO CONCLU.
           IF N-CODI = 0 GO TO RINCOR31.
           MOVE N-CODI TO SN2-COD.
           MOVE B-CODI TO SB2-COD.
           WRITE RENGLON FROM DESPLIEGE10 AFTER 1.
           COMPUTE CON-RE = CON-RE + 1.
           IF CON-LI = 66 MOVE ZEROS TO CON-LI GO TO IMPRIME3.
           COMPUTE CON-LI = CON-LI + 1.
           GO TO RINCOR31.
       CONCLU.
           MOVE CON-RE TO RE-TOT.
           WRITE RENGLON FROM TOT-RE AFTER 2.
           CLOSE IMPRE CODI.
           GO TO ENCABEZA.

       CONTRILT.
           CLOSE CONTRI.
           OPEN I-O CONTRI.
       L.
           PERFORM ENCABEZA.
           MOVE 7 TO LIN.
           MOVE 3 TO CL.
           DISPLAY PANTALLA-IMP.
           MOVE SPACES TO OPC.
           ACCEPT OPC AT 1265 WITH PROMPT.
           IF OPC = 1 GO TO L-1.
           IF OPC = 2 GO TO L-2.
           IF OPC = 3 GO TO ENCABEZA.
           GO TO L.
       L-1.
           PERFORM ENCABEZA.
           DISPLAY PANTALLA-IMP11.
           MOVE SPACES TO OPC.
           ACCEPT OPC AT 1357 WITH PROMPT.
           IF OPC = 1 PERFORM ENCABEZA GO TO L-3.
           IF OPC = 2 GO TO L-4.
           IF OPC = 3 GO TO L-5.
           IF OPC = 4 GO TO ENCABEZA.
           GO TO L-1.

       L-3.
           READ CONTRI NEXT RECORD AT END
           PERFORM TER THRU F-TER GO TO ENCABEZA.
           IF NOMBRE = "A" GO TO L-3.
           MOVE NUM-CO TO CONTRI-D.
           MOVE NOMBRE TO NOM-SA.
           ADD 1 TO LIN.
           DISPLAY DESPLIEGE2 AT LINCL.
           MOVE UBICA  TO UBI-SA.
           ADD 1 TO LIN.
           DISPLAY DESPLIEGE21 AT LINCL.
           ADD 1 TO LIN.
           IF LIN = 19 PERFORM TER THRU F-TER.
           GO TO L-3.

       L-4.
           MOVE SPACES TO NOMBRE.
           MOVE "A" TO NOMBRE.
           START CONTRI KEY IS NOT LESS THAN NOMBRE
           INVALID KEY GO TO L-1.
           PERFORM ENCABEZA.
           GO TO L-3.

       L-5.
           PERFORM ENCABEZA.
           MOVE SPACES TO B-NOM.
           DISPLAY "LETRA INICIAL APELLIDO" AT 1210
           WITH FOREGROUND-COLOR 6.
           ACCEPT B-NOM AT 1233 WITH PROMPT.
           IF B-NOM = SPACES GO TO ENCABEZA.
           MOVE B-NOM TO NOMBRE.
           START CONTRI KEY IS NOT LESS THAN NOMBRE
           INVALID KEY DISPLAY NO-LETRA
           MOVE SPACES TO OPC
           ACCEPT OPC AT 1440 WITH PROMPT
           DISPLAY "                                           " AT 1440.
           GO TO L-5.
           PERFORM ENCABEZA.
           GO TO L-3.

       L-2.
           PERFORM ENCABEZA.
           OPEN OUTPUT IMPRE.
           MOVE 0 TO CON-LI CON-RE CON-HO.
           DISPLAY PANTALLA-IMP11.
       L-21.
           MOVE SPACES TO OPC.
           MOVE ZEROS TO CON-RE.
           ACCEPT OPC AT 1357 WITH PROMPT.
           IF OPC = 1 GO TO L-6.
           IF OPC = 2 GO TO L-7.
           IF OPC = 3 GO TO L-8.
           IF OPC = 4 CLOSE IMPRE
           GO TO ENCABEZA.
           GO TO L-21.
       L-6.
           ACCEPT FECHA FROM DATE.
           MOVE DDR TO DDI.
           MOVE MMR TO MMI.
           MOVE AAR TO AAI.
           PERFORM IMPRIME3.
           GO TO L-9.
       L-7.
           MOVE "A" TO NOMBRE.
           START CONTRI KEY IS NOT LESS THAN NOMBRE
           INVALID KEY GO TO L-1.
           GO TO L-6.
       L-8.
           PERFORM ENCABEZA.
           MOVE SPACES TO B-NOM.
           DISPLAY "LETRA INICIAL APELLIDO" AT 1210
           WITH FOREGROUND-COLOR 6.
           ACCEPT B-NOM AT 1233 WITH PROMPT.
           IF B-NOM = SPACES GO TO ENCABEZA.
           MOVE B-NOM TO NOMBRE.
           START CONTRI KEY IS NOT LESS THAN NOMBRE
           INVALID KEY DISPLAY NO-LETRA
           MOVE SPACES TO OPC
           ACCEPT OPC AT 1440 WITH PROMPT
           DISPLAY "                        " AT 1440
           GO TO L-8.
           GO TO L-6.
       L-9.
           READ CONTRI NEXT RECORD AT END GO TO CONCLU1.
           IF NUM-CO = 0 GO TO L-9.
           IF NOMBRE = "A" GO TO L-9.
           MOVE NUM-CO TO CONTRI-D1.
           MOVE NOMBRE TO NOM-SA1.
           ADD 1 TO CON-LI.
           WRITE RENGLON FROM DESPLIEGE211 AFTER 2.
           MOVE UBICA TO UBI-SA1.
           ADD 2 TO CON-LI.
           WRITE RENGLON FROM DESPLIEGE2111 AFTER 1.
           COMPUTE CON-RE = CON-RE + 1.
           IF CON-LI = 66 MOVE ZEROS TO CON-LI GO TO L-6.
           COMPUTE CON-LI = CON-LI + 1.
           GO TO L-9.
       CONCLU1.
           MOVE CON-RE TO RE-TOT.
           WRITE RENGLON FROM TOT-RE AFTER 2.
           CLOSE IMPRE.
           GO TO ENCABEZA.

       LPANO.
           PERFORM ENCABEZA.
           DISPLAY PANTALLA-ANO.
           DISPLAY PANTALLA-SAL.
       REGRE.
           CLOSE IMPU.
           OPEN I-O IMPU.
           MOVE SPACES TO ANO-P.
           ACCEPT ANO-P AT 1148 WITH PROMPT.
           IF ANO-P = 0 GO TO ENCABEZA.
           IF ANO-P = SPACES GO TO REGRE.
           MOVE ANO-P TO ANO.
           START IMPU KEY IS EQUAL ANO
           INVALID KEY
           DISPLAY "A#O NO ENCONTRADO"  AT 1155
           WITH FOREGROUND-COLOR 6
           MOVE SPACES TO OPC
           ACCEPT OPC AT 1174
           DISPLAY "                  " AT 1155
           GO TO REGRE.
           OPEN OUTPUT IMPRE.
       REGRESE1.
           MOVE ZEROS TO CON-LI.
           MOVE ZEROS TO CON-RE.
           MOVE ZEROS TO CON-HO.
           ACCEPT FECHA FROM DATE.
           MOVE DDR TO DDI.
           MOVE MMR TO MMI.
           MOVE AAR TO AAI.

       IMPRIME1.
           ADD 1 TO CON-HO.
           MOVE CON-HO TO HO.
           IF CON-HO = 1 WRITE RENGLON FROM T1 AFTER 1
           ELSE
           WRITE RENGLON FROM T1 AFTER PAGE.
           WRITE RENGLON FROM T3 AFTER 1.
           WRITE RENGLON FROM T31 AFTER 1.
           WRITE RENGLON FROM T4 AFTER 1.
           WRITE RENGLON FROM T1 AFTER 1.
           WRITE RENGLON FROM T5 AFTER 2.
           WRITE RENGLON FROM T6 AFTER 1.
           MOVE 8 TO CON-LI.
       IMPRIME2.
           READ IMPU NEXT RECORD AT END GO TO FIN.
           MOVE CONT TO CON-S.
           MOVE P1 TO PER-S.
           MOVE AA TO ANO-S.
           MOVE TIP-VEN TO TI-S.
           CLOSE CONTRI.
           OPEN I-O CONTRI.
           MOVE CONT TO NUM-CO.
           READ CONTRI KEY IS NUM-CO
           INVALID KEY GO TO FIN.
           MOVE NOMBRE TO NOM1-S.
           MOVE UBICA  TO UBI1-S.
           MOVE DDV TO DD-S.
           MOVE MMV TO MM-S.
           MOVE AAV TO AA-S.
           MOVE DDP TO DD-S1.
           MOVE MMP TO MM-S1.
           MOVE AAP TO AA-S1.
           MOVE MONTO TO MONTO-S.
           WRITE RENGLON FROM DESPLIEGE3 AFTER 2.
           WRITE RENGLON FROM DESPLIEGE5 AFTER 1.
           COMPUTE CON-RE = CON-RE + 1.
           IF CON-LI = 59 MOVE ZEROS TO CON-LI
           GO TO IMPRIME1.
           COMPUTE CON-LI = CON-LI + 3.
           GO TO IMPRIME2.

       FIN.
           MOVE CON-RE TO RE-TOT
           WRITE RENGLON FROM TOT-RE AFTER 2.
           CLOSE IMPRE.
           GO TO ENCABEZA.

       CIERRE.
           CLOSE IMPU CONTRI.
           CHAIN "MENU".
           STOP RUN.
