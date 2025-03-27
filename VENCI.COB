       IDENTIFICATION DIVISION.
       PROGRAM-ID.    VENCI.  
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

       DATA DIVISION.
       FILE SECTION.

       FD  CONTRI LABEL RECORD IS STANDARD
           VALUE OF FILE-ID "ARCONT"
           DATA RECORD IS REG-CON.

       01  REG-CON.
           03 NUM-CO          PIC X(25).
           03 NOMBRE          PIC X(25).
           03 UBICA           PIC X(25).

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

       WORKING-STORAGE SECTION.
       77  OPC                   PIC XX VALUE SPACES.
       77  GUION                 PIC X(77) VALUE ALL "-".
       77  CAM                   PIC 9 VALUE ZERO.
       77  B                     PIC 9 VALUE ZERO.
       77  CONT1                 PIC X(25) VALUE SPACES.
       77  FEC-VEN1              PIC 9(06) VALUE ZEROS.

       01  S-CODI.
           03 SN-CODI            PIC Z9.
           03 F                  PIC X VALUE "-".
           03 SB-CODI            PIC X(15) VALUE SPACES.

       01  LINCL.
           03 LIN                PIC 99.
           03 CL                 PIC 99.

       SCREEN SECTION.

       01  PANTALLA-CARGA.
           03 LINE 7 COLUMN 3 FOREGROUND-COLOR 4
              VALUE "  NRO CONTRIBUYENTE:".
           03 LINE 9 COLUMN 3 FOREGROUND-COLOR 4
              VALUE "  APELLIDO Y NOMBRE:".
           03 LINE 11 COLUMN 3 FOREGROUND-COLOR 4
              VALUE "  UBICACION........:".
           03 LINE 13 COLUMN 3 FOREGROUND-COLOR 4
              VALUE "1-FECHA VENCIMIENTO:".
           03 LINE 15 COLUMN 3 FOREGROUND-COLOR 4
              VALUE "2-TIPO VENCIMIENTO.:".
           03 LINE 17 COLUMN 3 FOREGROUND-COLOR 4
              VALUE "3-CUOTA............:".
           03 LINE 19 COLUMN 3 FOREGROUND-COLOR 4
              VALUE "4-A#O DE LA CUOTA..:".

       01  PANTALLA-RES1.
           03 LINE 20 COLUMN 25 REVERSE-VIDEO FOREGROUND-COLOR 6
              VALUE "1=GRABA  2=ANULA  3=MODIFICA".

       01  PANTALLA-MODI1.
           03 LINE 21 COLUMN 3 FOREGROUND-COLOR 6
              VALUE "INGRESE NRO DE CAMPO A MODIFICAR".

       01  PANTALLA-SAL.
           03 LINE 22 COLUMN 25 REVERSE-VIDEO FOREGROUND-COLOR 6
              VALUE "   PARA SALIR TIPEAR 0      ".

       01  PANTALLA-BORRA.
           03 LINE 7 COLUMN 23
              VALUE "                              ".
           03 LINE 9 COLUMN 23
              VALUE "                              ".
           03 LINE 11 COLUMN 23
              VALUE "                              ".
           03 LINE 13 COLUMN 23
              VALUE "              ".
           03 LINE 15 COLUMN 23
              VALUE "              ".
           03 LINE 17 COLUMN 23
              VALUE "              ".
           03 LINE 19 COLUMN 23
              VALUE "              ".
           03 LINE 20 COLUMN 56
              VALUE "   ".

       PROCEDURE DIVISION.
       INICIO.
           OPEN I-O CONTRI.
           OPEN I-O IMPU.
           OPEN I-O CODI.
       ENCABEZA.
           CALL "CALEN" USING B.
           DISPLAY GUION AT 0603 WITH FOREGROUND-COLOR 3.
       CAR.
           MOVE SPACES TO OPC.
           DISPLAY PANTALLA-CARGA.
           DISPLAY PANTALLA-SAL.
           DISPLAY PANTALLA-RES1.
           DISPLAY "TIPO DE VENCIMIENTOS" AT 0858
           WITH FOREGROUND-COLOR 6.
       CAR-C.
           MOVE 9 TO LIN.
           MOVE 59 TO CL.
       CAR-C1.
           READ CODI NEXT RECORD AT END GO TO CON-D1.
           IF N-CODI = 0 GO TO CAR-C1.
           MOVE N-CODI TO SN-CODI.
           MOVE B-CODI TO SB-CODI.
           DISPLAY S-CODI AT LINCL.
           ADD 1 TO LIN.
           IF LIN = 21
           DISPLAY "P/CONTINUAR ENTER" AT 2261
           WITH FOREGROUND-COLOR 6
           ACCEPT OPC AT 2278 WITH PROMPT
           DISPLAY "                  " AT 2261
           PERFORM BL THRU BL-F
           MOVE 9 TO LIN.
           GO TO CAR-C1.
       CON-D1.
           MOVE SPACES TO OPC.
           DISPLAY PANTALLA-BORRA.
           MOVE SPACES TO CONT1.
           MOVE ZEROS TO P1 TIP-IMP AA MONTO.
       D3.
           MOVE SPACES TO CONT1.
           ACCEPT CONT1 AT 0724 WITH PROMPT.
           IF CONT1 = "0" GO TO CIERRE.
           IF CONT1 = SPACES GO TO D3.
           MOVE CONT1 TO NUM-CO.
           READ CONTRI KEY IS NUM-CO
           INVALID KEY
           DISPLAY "CLIENTE INCORRECTO" AT 0825
           WITH FOREGROUND-COLOR 6
           MOVE SPACES TO OPC
           ACCEPT OPC AT 0845 WITH PROMPT
           DISPLAY "                     " AT 0825
           GO TO D3.
           DISPLAY NOMBRE AT 0924.
           DISPLAY UBICA AT 1124.
       D7.
           MOVE ZEROS TO FEC-VEN1 FEC-VEN.
           DISPLAY "          " AT 1325.
           ACCEPT FEC-VEN1 AT 1324 WITH PROMPT.
           MOVE FEC-VEN1 TO FEC-VEN.
           DISPLAY DDV AT 1324.
           DISPLAY "/"  AT 1326.
           DISPLAY MMV AT 1327.
           DISPLAY "/" AT 1329.
           DISPLAY AAV AT 1330.
           IF FEC-VEN1 = ZEROS GO TO D7.
           IF MMV = ZEROS OR AAV = ZEROS GO TO D7.
           IF DDV < 0 OR DDV > 31 OR DDV = 00
           DISPLAY "ERROR EN EL DIA" AT 1335
           WITH FOREGROUND-COLOR 4
           MOVE SPACES TO OPC
           ACCEPT OPC AT 1351 WITH PROMPT
           DISPLAY "                      " AT 1335
           GO TO D7.
           IF MMV < 0 OR > 12 OR MMV = 00
           DISPLAY "ERROR EN EL MES" AT 1335
           WITH FOREGROUND-COLOR 4
           MOVE SPACES TO OPC
           ACCEPT OPC AT 1351 WITH PROMPT
           DISPLAY "                         " AT 1325
           GO TO D7.
       D71.
           START IMPU KEY IS EQUAL TO FEC-VEN
           INVALID KEY GO TO D81.
       D711.
           READ IMPU NEXT RECORD AT END GO TO D81.
           IF CONT = CONT1
           DISPLAY "VENCIMIENTO YA INCORPORADO" AT 2103
           WITH FOREGROUND-COLOR 4
           MOVE SPACES TO OPC
           ACCEPT OPC AT 2140 WITH PROMPT
           DISPLAY "                                   " AT 2103
           GO TO CON-D1.
           IF FEC-VEN NOT = FEC-VEN1 GO TO D81.
           GO TO D711.
       D81.
           CLOSE CODI.
           OPEN I-O CODI.
           MOVE ZEROS TO TIP-IMP.
           ACCEPT TIP-IMP AT 1524 WITH PROMPT.
           MOVE TIP-IMP TO N-CODI.
           READ CODI KEY N-CODI
           INVALID KEY
           DISPLAY "CODIGO INCORRECTO" AT 1527
           WITH FOREGROUND-COLOR 6
           MOVE SPACES TO OPC
           ACCEPT OPC AT 1546 WITH PROMPT
           DISPLAY "                  " AT 1527
           GO TO D81.

       D82.
           MOVE ZEROS TO P1.
           ACCEPT P1 AT 1724 WITH PROMPT.

       D83.
           MOVE ZEROS TO AA.
           ACCEPT AA AT 1924 WITH PROMPT.
           MOVE AA TO ANO.
       D9.
           MOVE ZEROS TO MONTO FEC-PAG.
       RES2.
           MOVE SPACES TO OPC.
           ACCEPT OPC AT 2057 WITH PROMPT.
           IF OPC = "1" GO TO GRABA1.
           IF OPC = "2" GO TO CON-D1.
           IF OPC = "3" GO TO MODIF1.
           GO TO RES2.
       GRABA1.
           CLOSE IMPU.
           OPEN I-O IMPU.
           MOVE CONT1 TO CONT.
           MOVE FEC-VEN1 TO FEC-VEN.
           WRITE REG-IMP.
           GO TO CON-D1.
       MODIF1.
           DISPLAY PANTALLA-MODI1.
           MOVE 0 TO CAM.
           ACCEPT CAM AT 2140.
           IF CAM < 1 OR CAM > 4 GO TO MODIF1.
           IF CAM = 1 PERFORM D7
           PERFORM D71
           PERFORM D711
           GO TO F-MODI1
           ELSE
           IF CAM = 2 PERFORM D81
           GO TO F-MODI1
           ELSE
           IF CAM = 3 PERFORM D82
           GO TO F-MODI1
           ELSE
           IF CAM = 4 PERFORM D83.
       F-MODI1.
           DISPLAY "                                        " AT 2103.
           MOVE ZERO TO CAM.
           GO TO RES2.

       BL.
           MOVE 9 TO LIN.
           MOVE 59 TO CL.
       BL1.
           DISPLAY "                  " AT LINCL.
           ADD 1 TO LIN.
           IF LIN = 21  GO TO BL-F.
           GO TO BL1.
       BL-F.

       CIERRE.
           CLOSE CONTRI IMPU CODI.
           CHAIN "MENU".
           STOP RUN.
