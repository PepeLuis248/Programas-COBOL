       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DEPURAR.
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
                  ALTERNATE RECORD IS TIP-IMP
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

           SELECT DECO ASSIGN TO DISK
                  ORGANIZATION IS INDEXED
                  ACCESS IS DYNAMIC
                  RECORD KEY IS NUM-COD
                  ALTERNATE RECORD IS NOMBRED
                  WITH DUPLICATES.

           SELECT DEIM ASSIGN TO DISK
                  ORGANIZATION IS INDEXED
                  ACCESS IS DYNAMIC
                  RECORD KEY IS NUM-CO1D
                  ALTERNATE RECORD IS TIP-IMPD
                  WITH DUPLICATES
                  ALTERNATE RECORD IS FEC-VEND
                  WITH DUPLICATES
                  ALTERNATE RECORD IS FEC-PAGD
                  WITH DUPLICATES
                  ALTERNATE RECORD IS ANOD
                  WITH DUPLICATES.

           SELECT DECOD ASSIGN TO DISK
                  ORGANIZATION IS INDEXED
                  ACCESS IS DYNAMIC
                  RECORD KEY IS N-CODID.

           SELECT IMPRE ASSIGN TO PRINTER.

           SELECT BORRE ASSIGN TO DISK.

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

       FD  CODI LABEL RECORD IS STANDARD
           VALUE OF FILE-ID "ARCODI"
           DATA RECORD IS REG-CODI.

       01  REG-CODI.
           03 N-CODI             PIC 99.
           03 B-CODI             PIC X(15).

       FD  DECO LABEL RECORD IS STANDARD
           VALUE OF FILE-ID "ARDECO"
           DATA RECORD IS REG-COND.

       01  REG-COND.
           03 NUM-COD            PIC X(25).
           03 NOMBRED            PIC X(25).
           03 DOMICILIO          PIC X(25).

       FD  DEIM LABEL RECORD IS STANDARD
           VALUE OF FILE-ID "ARDEIM"
           DATA RECORD IS REG-IMPD.

       01  REG-IMPD.
           03 NUM-CO1D.
              05 CONTD           PIC X(25).
              05 PERID.
                 07 AAD          PIC 99.
                 07 P1D          PIC 99.
           03 TIP-IMPD           PIC 99.
           03 FEC-VEND.
              05 DDVD            PIC XX.
              05 MMVD            PIC XX.
              05 AAVD            PIC XX.
           03 FEC-PAGD.
              05 DDPD            PIC XX.
              05 MMPD            PIC XX.
              05 AAPD            PIC XX.
           03 MONTOD             PIC 9(11)V99.
           03 ANOD               PIC 99.

       FD  DECOD LABEL RECORD IS STANDARD
           VALUE OF FILE-ID "ARDECOD"
           DATA RECORD IS REG-CODD.

       01  REG-CODD.
           03 N-CODID            PIC 99.
           03 B-CODID            PIC X(15).

       FD  IMPRE LABEL RECORD IS OMITTED.

       01  RENGLON               PIC X(132).

       FD  BORRE LABEL RECORD IS STANDARD
           VALUE OF FILE-ID "BORRE"
           DATA RECORD IS R-BORRE.

       01  R-BORRE               PIC 9.

       WORKING-STORAGE SECTION.
       77  OPC                   PIC XX VALUE SPACES.
       77  GUION                 PIC X(76) VALUE ALL "-".
       77  CAM                   PIC 9 VALUE ZERO.
       77  B                     PIC 9 VALUE ZERO.
       77  SEN                   PIC 9 VALUE ZERO.
       77  CON-LI                PIC 999 VALUE ZEROS.
       77  CON-HO                PIC 9999 VALUE ZEROS.
       77  CON-RE                PIC 9999 VALUE ZEROS.
       77  BAND                  PIC 9 VALUE ZERO.
       77  NUM-CO2               PIC X(25) VALUE SPACES.
       77  ANO1                  PIC XX VALUE SPACES.

       01  FECHA.
           03 AAR                PIC 99.
           03 MMR                PIC 99.
           03 DDR                PIC 99.

       01  T1.
           03 F                  PIC X(80) VALUE ALL "*".

       01  T2.
           03 F                  PIC X VALUE "*".
           03 F                  PIC X(78) VALUE SPACES.
           03 F                  PIC X VALUE "*".

       01  T3.
           03 F  PIC X(35) VALUE "* SISTEMA DE VENCIMIENTOS          ".
           03 F  PIC X(35) VALUE "                            FECHA: ".
           03 DDI PIC 99.
           03 F   PIC X VALUE "/".
           03 MMI PIC 99.
           03 F   PIC X VALUE "/".
           03 AAI PIC 99.
           03 F   PIC XX VALUE " *".

       01  T4.
           03 F  PIC X(35) VALUE "* LISTADO POR CLIENTE              ".
           03 F  PIC X(35) VALUE "                              HOJA ".
           03 F  PIC X(04) VALUE "NRO:".
           03 HO PIC ZZZ9.
           03 F  PIC XX    VALUE " *".

       01  T41.
           03 F  PIC X(35) VALUE "* LISTADO POR A#O                  ".
           03 F  PIC X(35) VALUE "                              HOJA ".
           03 F  PIC X(04) VALUE "NRO:".
           03 HO1 PIC ZZZ9.
           03 F  PIC XX    VALUE " *".

       01  T5.
           03 F  PIC X(35) VALUE "NRO DE CLIENTE          CUO/A#O  TI".
           03 F  PIC X(35) VALUE "P-V   FEC-VEN    FEC-PAG          M".
           03 F  PIC X(10) VALUE "ONTO      ".

       01  T6.
           03 F  PIC X(35) VALUE "--- -- -------          -------  --".
           03 F  PIC X(35) VALUE "--    -------    -------          -".
           03 F  PIC X(10) VALUE "----      ".

       01  DESPLIEGE3.
           03 CON-S              PIC X(24).
           03 PER-S              PIC 99.
           03 F                  PIC X VALUE "/".
           03 ANO-S              PIC 99.
           03 F                  PIC X(05) VALUE SPACES.
           03 TI-S               PIC 99.
           03 F                  PIC X(05) VALUE SPACES.
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

       01  DESPLIEGE5.
           03 NOM1-S             PIC X(25).
           03 F                  PIC X(03) VALUE SPACES.
           03 UBI1-S              PIC X(25).

       01  TOT-RE.
           03 F       PIC X(19)  VALUE "TOTAL DE REGISTROS:".
           03 RE-TOT  PIC ZZZ9.


       SCREEN SECTION.

       01  PANTALLA-MENU.
           03 LINE 08 COLUMN 24 REVERSE-VIDEO FOREGROUND-COLOR 4
              VALUE "          MENU               ".
           03 LINE 10 COLUMN 24
              VALUE "1-DEPURACION POR NRO CLIENTE.".
           03 LINE 12 COLUMN 24
              VALUE "2-DEPURACION POR BAJAS".
           03 LINE 14 COLUMN 24
              VALUE "3-DEPURACION POR A#O".
           03 LINE 16 COLUMN 24
              VALUE "4-REGRESO MENU PRINCIPAL".
           03 LINE 18 COLUMN 24 REVERSE-VIDEO  FOREGROUND-COLOR 4
              VALUE "INGRESE OPCION:              ".

       01  PANTALLA-CONTRI.
           03 LINE 11 COLUMN 18 FOREGROUND-COLOR 5
              VALUE "NRO DE CLIENTE......:".
       01  PANTALLA-SAL.
           03 LINE 13 COLUMN 18 REVERSE-VIDEO FOREGROUND-COLOR 4
              VALUE " PARA SALIR TIPEAR 0       ".

       01  PANTALLA-A.
           03 LINE 11 COLUMN 18 FOREGROUND-COLOR 5
              VALUE "A#O..:".

       01  PANTALLA-PREVIA.
           03 LINE 10 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
              VALUE "HAY QUE TENER CUIDADO CON ESTA OPCION,".
           03 LINE 12 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
              VALUE "PUESTO QUE, UNA VEZ REALIZADA UNA DE  ".
           03 LINE 14 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
              VALUE "LAS SUB-OPCIONES NO SE PODRA REALIZAR-".
           03 LINE 16 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
              VALUE "SE NUEVAMENTE. SI SURGIERE UN ERROR SE".
           03 LINE 18 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
              VALUE "DEBERA RESTAURAR LOS ARCHIVOS Y REALI-".
           03 LINE 20 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
              VALUE "ZAR LA OPCION OTRA VEZ.".

       PROCEDURE DIVISION.
       INICIO.
           OPEN I-O IMPU.
           OPEN I-O CONTRI.
           OPEN I-O CODI.
           MOVE 0 TO BAND.
           CALL "CALEN" USING B.
           DISPLAY GUION AT 0603 WITH FOREGROUND-COLOR 3.
           MOVE SPACES TO OPC.
           DISPLAY PANTALLA-PREVIA.
           ACCEPT OPC AT 2225 WITH PROMPT.
       ENCABEZA.
           CALL "CALEN" USING B.
           DISPLAY GUION AT 0603 WITH FOREGROUND-COLOR 3.

       L-PANT.
           DISPLAY PANTALLA-MENU.
           MOVE SPACES TO OPC.
           ACCEPT OPC AT 1855 WITH PROMPT.
           IF OPC = "1" GO TO D-CONT.
           IF OPC = "2" GO TO D-BAJAS.
           IF OPC = "3" GO TO D-ANO.
           IF OPC = "4" GO TO CIERRE.
           GO TO L-PANT.

       D-CONT.
           IF BAND = 1
           PERFORM ENCABEZA
           DISPLAY "YA SE REALIZO LA DEPURACION" AT 1110
           WITH FOREGROUND-COLOR 6
           DISPLAY "DEBE SALIR PARA ACTUALIZAR" AT 1210
           WITH FOREGROUND-COLOR 6
           DISPLAY "LOS ARCHIVOS (RETURN)" AT 1310
           WITH FOREGROUND-COLOR 6
           MOVE SPACES TO OPC
           ACCEPT OPC AT 1349 WITH PROMPT
           GO TO ENCABEZA.
           PERFORM ENCABEZA.
           DISPLAY PANTALLA-CONTRI.
           DISPLAY PANTALLA-SAL.
       D-CONT1.
           CLOSE CONTRI.
           OPEN I-O CONTRI.
           MOVE SPACES TO NUM-CO2.
           ACCEPT NUM-CO2 WITH PROMPT AT 1140.
           IF NUM-CO2 = "0" GO TO ENCABEZA.
           MOVE NUM-CO2 TO NUM-CO.
           START CONTRI KEY IS EQUAL NUM-CO
           INVALID KEY
           DISPLAY "CLIENTE INCORRECTO" AT 1160
           MOVE SPACES TO OPC
           ACCEPT OPC AT 1180
           DISPLAY "                   " AT 1160
           GO TO D-CONT1.

       D-CONT-D.
           CLOSE CONTRI.
           OPEN I-O CONTRI.
           OPEN OUTPUT DECO.
           OPEN OUTPUT DEIM.
           MOVE 1 TO BAND.
           PERFORM ENCABEZA.
           DISPLAY "DEPURANDO POR NRO DE CLIENTE" AT 1110
           WITH FOREGROUND-COLOR 1.
       LCONT.
           READ CONTRI NEXT RECORD AT END GO TO LIMPU.
           IF NUM-CO = NUM-CO2 GO TO LCONT.
           MOVE REG-CON TO REG-COND.
           WRITE REG-COND.
           GO TO LCONT.

       LIMPU.
           MOVE 0 TO CON-HO.
           MOVE 0 TO CON-RE.
           MOVE 0 TO CON-LI.
           MOVE 0 TO SEN.
           OPEN OUTPUT IMPRE.
           ACCEPT FECHA FROM DATE
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
           WRITE RENGLON FROM T2 AFTER 1.
           WRITE RENGLON FROM T4 AFTER 1.
           WRITE RENGLON FROM T1 AFTER 1.
           WRITE RENGLON FROM T5 AFTER 2.
           WRITE RENGLON FROM T6 AFTER 1.

       IMPRIME2.
           READ IMPU NEXT RECORD AT END GO TO FINALIZA.
           IF CONT = NUM-CO2 GO TO C.
           MOVE REG-IMP TO REG-IMPD.
           WRITE REG-IMPD.
           GO TO IMPRIME2.
       C.
           CLOSE CONTRI.
           OPEN I-O CONTRI.
           MOVE CONT TO NUM-CO.
           READ CONTRI KEY IS NUM-CO
           INVALID KEY CLOSE IMPRE GO TO ENCABEZA.
           MOVE NOMBRE TO NOM1-S.
           MOVE UBICA  TO UBI1-S.
           MOVE CONT TO CON-S.
           MOVE P1 TO PER-S.
           MOVE AA TO ANO-S.
           MOVE TIP-IMP TO TI-S.
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
           COMPUTE CON-LI = CON-LI + 3.
           IF CON-LI = 72 MOVE ZEROS TO CON-LI
           GO TO IMPRIME1.
           GO TO IMPRIME2.

       FINALIZA.
           MOVE CON-RE TO RE-TOT.
           WRITE RENGLON FROM TOT-RE AFTER 2.
           CLOSE IMPRE DECO DEIM.
           GO TO ENCABEZA.

       D-BAJAS.
           IF BAND = 1
           PERFORM ENCABEZA
           DISPLAY "YA SE REALIZO LA DEPURACION" AT 1110
           WITH FOREGROUND-COLOR 6
           DISPLAY "DEBE SALIR PARA ACTUALIZAR" AT 1210
           WITH FOREGROUND-COLOR 6
           DISPLAY "LOS ARCHIVOS (RETURN)" AT 1310
           WITH FOREGROUND-COLOR 6
           MOVE SPACES TO OPC
           ACCEPT OPC AT 1349
           GO TO ENCABEZA.
       D-BAJAS1.
           OPEN OUTPUT DECO.
           OPEN OUTPUT DEIM.
           OPEN OUTPUT DECOD.
           PERFORM ENCABEZA.
           MOVE 1 TO BAND.
           DISPLAY "DEPURANDO POR BAJAS" AT 1110
           WITH FOREGROUND-COLOR 1.
       D-CONTR.
           READ CONTRI NEXT RECORD AT END GO TO D-IMP.
           MOVE REG-CON TO REG-COND.
           WRITE REG-COND.
           GO TO D-CONTR.

       D-IMP.
           READ IMPU NEXT RECORD AT END GO TO D-CODI.
           MOVE REG-IMP TO REG-IMPD.
           WRITE REG-IMPD.
           GO TO D-IMP.

       D-CODI.
           READ CODI NEXT RECORD AT END CLOSE DECO DEIM DECOD
           GO TO ENCABEZA.
           MOVE REG-CODI TO REG-CODD.
           WRITE REG-CODD.
           GO TO D-CODI.

       D-ANO.
           IF BAND = 1
           PERFORM ENCABEZA
           DISPLAY "YA SE REALIZO LA DEPURACION" AT 1110
           WITH FOREGROUND-COLOR 6
           DISPLAY "DEBE SALIR PARA ACTUALIZAR" AT 1210
           WITH FOREGROUND-COLOR 6
           DISPLAY "LOS ARCHIVOS (RETURN)" AT 1310
           WITH FOREGROUND-COLOR 6
           MOVE SPACES TO OPC
           ACCEPT OPC AT 1349 WITH PROMPT
           GO TO ENCABEZA.
           PERFORM ENCABEZA.
           DISPLAY PANTALLA-A.
           DISPLAY PANTALLA-SAL.
       D-A.
           CLOSE IMPU.
           OPEN I-O IMPU.
           MOVE SPACES TO ANO1.
           ACCEPT ANO1 WITH PROMPT AT 1125.
           IF ANO1 = "0" GO TO ENCABEZA.
           MOVE ANO1 TO ANO.
           START IMPU KEY IS EQUAL ANO
           INVALID KEY
           DISPLAY "A#O INCORRECTO" AT 1145
           MOVE SPACES TO OPC
           ACCEPT OPC AT 1160
           DISPLAY "               " AT 1145
           GO TO D-A.

       D-AD.
           CLOSE IMPU CONTRI.
           OPEN I-O IMPU.
           OPEN I-O CONTRI.
           OPEN OUTPUT DEIM.
           MOVE 1 TO BAND.
           PERFORM ENCABEZA.
           DISPLAY "DEPURANDO POR A#O" AT 1110
           WITH FOREGROUND-COLOR 1.

       D-AD2.
           MOVE REG-IMP TO REG-IMPD.
           WRITE REG-IMPD.

       D-AD3.
           MOVE 0 TO CON-HO.
           MOVE 0 TO CON-RE.
           MOVE 0 TO CON-LI.
           MOVE 0 TO SEN.
           OPEN OUTPUT IMPRE.
           ACCEPT FECHA FROM DATE.
           MOVE DDR TO DDI.
           MOVE MMR TO MMI.
           MOVE AAR TO AAI.
       IMPRIME11.
           ADD 1 TO CON-HO.
           MOVE CON-HO TO HO1.
           IF CON-HO = 1 WRITE RENGLON FROM T1 AFTER 1
           ELSE
           WRITE RENGLON FROM T1 AFTER PAGE.
           WRITE RENGLON FROM T3 AFTER 1.
           WRITE RENGLON FROM T2 AFTER 1.
           WRITE RENGLON FROM T41 AFTER 1.
           WRITE RENGLON FROM T1 AFTER 1.
           WRITE RENGLON FROM T5 AFTER 2.
           WRITE RENGLON FROM T6 AFTER 1.
       IMPRIME21.
           READ IMPU NEXT RECORD AT END GO TO FINALIZA1.
           IF CONT = SPACES GO TO IMPRIME21.
           IF ANO = ANO1 AND FEC-PAG = 000000 PERFORM D-AD2
           GO TO IMPRIME21.
           IF ANO NOT = ANO1 PERFORM D-AD2 GO TO IMPRIME21.
           CLOSE CONTRI.
           OPEN I-O CONTRI.
           MOVE CONT TO NUM-CO.
           READ CONTRI KEY IS NUM-CO
           INVALID KEY CLOSE IMPRE GO TO ENCABEZA.
           MOVE NOMBRE TO NOM1-S.
           MOVE UBICA TO UBI1-S.
           MOVE CONT TO CON-S.
           MOVE P1 TO PER-S.
           MOVE AA TO ANO-S.
           MOVE TIP-IMP TO TI-S.
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
           COMPUTE CON-LI = CON-LI + 3.
           IF CON-LI = 72 MOVE ZEROS TO CON-LI
           GO TO IMPRIME11.
           GO TO IMPRIME21.

       FINALIZA1.
           MOVE CON-RE TO RE-TOT.
           WRITE RENGLON FROM TOT-RE AFTER 2.
           CLOSE IMPRE DEIM.
           GO TO ENCABEZA.

       CIERRE.
           CLOSE IMPU CONTRI CODI.
           IF BAND = 1 GO TO CIERRE1.
           OPEN OUTPUT BORRE.
           WRITE R-BORRE.
           CLOSE BORRE.

       CIERRE1.
           STOP RUN.
