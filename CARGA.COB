       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CARGA.  
      ******************************
      * SISTEMA DE IMPUESTO        *
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

           SELECT CODI ASSIGN TO DISK
                  ORGANIZATION IS INDEXED
                  ACCESS IS DYNAMIC
                  RECORD KEY IS N-CODI.

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

       DATA DIVISION.
       FILE SECTION.

       FD  CONTRI LABEL RECORD IS STANDARD
           VALUE OF FILE-ID "ARCONT"
           DATA RECORD IS REG-CON.

       01  REG-CON.
           03 NUM-CO          PIC X(25).
           03 NOMBRE          PIC X(25).
           03 DOMICILIO       PIC X(25).

       FD  CODI LABEL RECORD IS STANDARD
           VALUE OF FILE-ID "ARCODI"
           DATA RECORD IS REG-CODI.

       01  REG-CODI.
           03 N-CODI          PIC 99.
           03 B-CODI          PIC X(15).

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

       WORKING-STORAGE SECTION.
       77  OPC                   PIC XX VALUE SPACES.
       77  GUION                 PIC X(76) VALUE ALL "-".
       77  CAM                   PIC 9 VALUE ZERO.
       77  B                     PIC 9 VALUE ZERO.
       77  MONTO1                PIC Z(11).ZZ VALUE ZEROS.
       77  CONT1                 PIC X(25) VALUE SPACES.
       77  L-GUION               PIC X(77) VALUE ALL "-".
       77  FEC-PAG1              PIC 9(06) VALUE ZEROS.
       77  FEC-VEN1              PIC 9(06) VALUE ZEROS.
       77  CONTA                 PIC 99 VALUE ZEROS.

       01  S-CODI.
           03 SN-CODI            PIC Z9.
           03 F                  PIC X VALUE "-".
           03 SB-CODI            PIC X(15) VALUE SPACES.

       SCREEN SECTION.

       01 L-PANTALLA.
           03 BLANK SCREEN.

       01  MENU-CARGA.
           03 LINE 09 COLUMN 24 FOREGROUND-COLOR 4 REVERSE-VIDEO
              VALUE "     CARGA DEL SISTEMA     ".
           03 LINE 11 COLUMN 27
              VALUE "1-TIPO DE VENCIMIENTOS".
           03 LINE 13 COLUMN 27
              VALUE "2-NRO DEL CLIENTE      ".
           03 LINE 15 COLUMN 27
              VALUE "3-VENCIMIENTOS PAGADOS".
           03 LINE 17 COLUMN 27
              VALUE "4-REGRESO MENU PRINCIPAL".
           03 LINE 19 COLUMN 24 FOREGROUND-COLOR 4 REVERSE-VIDEO
              VALUE "     INGRESE OPCION:       ".

       01  PANTALLA-CONTR.
           03 LINE 09 COLUMN 10 FOREGROUND-COLOR 4
              VALUE "1-NRO DE CLIENTE...:".
           03 LINE 11 COLUMN 10 FOREGROUND-COLOR 4
              VALUE "2-APELLIDO Y NOMBRE:".
           03 LINE 13 COLUMN 10 FOREGROUND-COLOR 4
              VALUE "3-DOMICILIO........:".

       01  PANTALLA-CODI.
           03 LINE 11 COLUMN 10 FOREGROUND-COLOR 4
              VALUE "1-NRO DE VENCIMIENTO.:".
           03 LINE 13 COLUMN 10 FOREGROUND-COLOR 4
              VALUE "2-NOMBRE VENCIMIENTO.:".

       01  PANTALLA-CARGA.
           03 LINE 7 COLUMN 3 FOREGROUND-COLOR 4
              VALUE "  NRO DE CLIENTE...:".
           03 LINE 9 COLUMN 3 FOREGROUND-COLOR 4
              VALUE "  APELLIDO Y NOMBRE:".
           03 LINE 11 COLUMN 3 FOREGROUND-COLOR 4
              VALUE "  DOMICILIO........:".
           03 LINE 13 COLUMN 3 FOREGROUND-COLOR 4
              VALUE "1-FECHA VENCIMIENTO:".
           03 LINE 14 COLUMN 3 FOREGROUND-COLOR 4
              VALUE "2-FECHA DE PAGO....:".
           03 LINE 15 COLUMN 3 FOREGROUND-COLOR 4
              VALUE "3-TIPO DE VENCIMIEN:".
           03 LINE 16 COLUMN 3 FOREGROUND-COLOR 4
              VALUE "4-CUOTA............:".
           03 LINE 17 COLUMN 3 FOREGROUND-COLOR 4
              VALUE "5-A#O DE LA CUOTA..:".
           03 LINE 18 COLUMN 3 FOREGROUND-COLOR 4
              VALUE "6-MONTO............:".

       01  PANTALLA-RES.
           03 LINE 17 COLUMN 25 FOREGROUND-COLOR 6 REVERSE-VIDEO
              VALUE "1=GRABA  2=ANULA  3=MODIFICA".

       01  PANTALLA-MODI.
           03 LINE 18 COLUMN 15 FOREGROUND-COLOR 6
              VALUE "INGRESE NRO DE CAMPO A MODIFICAR".

       01  PANTALLA-RES1.
           03 LINE 21 COLUMN 03 FOREGROUND-COLOR 6 REVERSE-VIDEO
              VALUE "1=GRABA  2=ANULA  3=MODIFICA".

       01  PANTALLA-MODI1.
           03 LINE 22 COLUMN 3 FOREGROUND-COLOR 6
              VALUE "INGRESE NRO DE CAMPO A MODIFICAR".

       01  PANTALLA-SAL.
           03 LINE 23 COLUMN 03 FOREGROUND-COLOR 6 REVERSE-VIDEO
              VALUE "   PARA SALIR TIPEAR 0      ".

       01  PANTALLA-SAL1.
           03 LINE 19 COLUMN 25 FOREGROUND-COLOR 6 REVERSE-VIDEO
              VALUE "   PARA SALIR TIPEAR 0      ".

       01  TV.
           03 LINE 08 COLUMN 59 FOREGROUND-COLOR 6
           VALUE "TIPO DE VENCIMIENTOS".

       01  PANTALLA-BORRA.
           03 LINE 7 COLUMN 24
              VALUE "                              ".
           03 LINE 9 COLUMN 24
              VALUE "                             ".
           03 LINE 11 COLUMN 24
              VALUE "                             ".
           03 LINE 13 COLUMN 23
              VALUE "                 ".
           03 LINE 14 COLUMN 23
              VALUE "                 ".
           03 LINE 15 COLUMN 24
              VALUE "                 ".
           03 LINE 16 COLUMN 24
              VALUE "                 ".
           03 LINE 17 COLUMN 24
              VALUE "                 ".
           03 LINE 18 COLUMN 24
              VALUE "                     ".
           03 LINE 21 COLUMN 33
              VALUE "       ".

       01  PANTALLA-BORRA1.
           03 LINE 09 COLUMN 32
              VALUE "                                 ".
           03 LINE 11 COLUMN 32
              VALUE "                                 ".
           03 LINE 13 COLUMN 32
              VALUE "                                 ".
           03 LINE 15 COLUMN 32
              VALUE "   ".
           03 LINE 17 COLUMN 55
              VALUE "      ".

       01  PANTALLA-BORRA2.
           03 LINE 11 COLUMN 32
              VALUE "      ".
           03 LINE 13 COLUMN 32
              VALUE "                   ".
           03 LINE 17 COLUMN 55
              VALUE "      ".

       01  GUION.
           03 LINE 06 COLUMN 03 FOREGROUND-COLOR 3
           USING GUION.

       PROCEDURE DIVISION.
       INICIO.
           OPEN I-O CONTRI.
           OPEN I-O IMPU.
           OPEN I-O CODI.
       ENCABEZA.
           CALL "CALEN" USING B.
           DISPLAY GUION.
       MENU.
           MOVE SPACES TO OPC.
           DISPLAY MENU-CARGA.
           ACCEPT (19, 55) OPC WITH PROMPT.
           IF OPC = "1" GO TO COD.
           IF OPC = "2" GO TO CON.
           IF OPC = "3" GO TO CAR.
           IF OPC = "4" GO TO CIERRE.
           GO TO MENU.

       COD.
           PERFORM ENCABEZA.
           MOVE SPACES TO OPC.
           DISPLAY PANTALLA-CODI.
           DISPLAY PANTALLA-RES.
           DISPLAY PANTALLA-SAL1.
           MOVE 0 TO CONTA.
       COD-D.
           MOVE SPACES TO OPC.
           DISPLAY PANTALLA-BORRA2.
       LCOD.
           READ CODI NEXT RECORD AT END
           CLOSE CODI OPEN I-O CODI GO TO H1.
           ADD 1 TO CONTA.
           IF CONTA = 6 PERFORM CARTEL THRU F-CARTEL
           GO TO CIERRE.
           GO TO LCOD.
       H1.
           MOVE SPACES TO OPC.
           DISPLAY PANTALLA-BORRA2.
           MOVE ZEROS TO N-CODI.
           ACCEPT (11, 33) N-CODI WITH PROMPT.
           ADD 1 TO CONTA.
           IF CONTA > 6 PERFORM CARTEL THRU F-CARTEL
           GO TO CIERRE.
           IF N-CODI = SPACES GO TO H1.
       H111.
           READ CODI KEY IS N-CODI
           INVALID KEY GO TO H11.
           IF N-CODI = 0 GO TO H11.
           DISPLAY (11, 36) "CODIGO YA INCORPORADO"
           WITH FOREGROUND-COLOR 6
           ACCEPT (11, 60) OPC WITH PROMPT
           DISPLAY (11, 36) "                      "
           SUBTRACT 1 FROM CONTA
           GO TO H1.
       H11.
           IF N-CODI = 0 CLOSE CODI
                         OPEN I-O CODI
                         PERFORM ENCABEZA
                         GO TO MENU.
       H2.
           MOVE SPACES TO B-CODI.
           ACCEPT (13, 33) B-CODI WITH PROMPT.
       HRES.
           MOVE SPACES TO OPC.
           ACCEPT (17, 55) OPC WITH PROMPT.
           IF OPC = "1" GO TO HGRABA.
           IF OPC = "2" GO TO H1.
           IF OPC = "3" GO TO HMODIF.
           GO TO HRES.
       HGRABA.
           WRITE REG-CODI.
           GO TO H1.
       HMODIF.
           MOVE ZERO TO CAM.
           DISPLAY PANTALLA-MODI.
           ACCEPT (18, 50) CAM WITH PROMPT.
           IF CAM < 1 OR CAM > 2 GO TO HMODIF.
           IF CAM = 1 PERFORM H1
           ELSE
           IF CAM = 2 PERFORM H2.
       F-HMODIF.
           DISPLAY (18, 15) "                                     ".
           MOVE ZERO TO CAM.
           GO TO HRES.

       CARTEL.
           DISPLAY (11, 40) "Esta es una version SHAREWARE,solamente"
           DISPLAY (12, 40) "admite 5(cinco) tipo de vencimientos,la"
           DISPLAY (13, 40) "version completa tiene hasta 99(noventa"
           DISPLAY (14, 40) "y nueve) tipo de vencimientos."
           MOVE SPACES TO OPC
           ACCEPT (15, 40) OPC WITH PROMPT.
       F-CARTEL.
           EXIT.

       CARTEL1.
           DISPLAY (11, 40) "Esta es una version SHAREWARE,solamente"
           DISPLAY (12, 40) "admite 10(diez) clientes,en la version "
           DISPLAY (13, 40) "completa el nro de clientes es infinito"
           MOVE SPACES TO OPC
           ACCEPT (14, 40) OPC WITH PROMPT.
       F-CARTEL1.
           EXIT.

       CON.
           PERFORM ENCABEZA.
           MOVE SPACES TO OPC.
           DISPLAY PANTALLA-CONTR.
           DISPLAY PANTALLA-RES.
           DISPLAY PANTALLA-SAL1.
           MOVE 0 TO CONTA.
       CON-1.
           READ CONTRI NEXT RECORD AT END
           CLOSE CONTRI OPEN I-O CONTRI GO TO CON-D.
           ADD 1 TO CONTA.
           IF CONTA = 11 PERFORM CARTEL1 THRU F-CARTEL1
           GO TO CIERRE.
           GO TO CON-1.
       CON-D.
           MOVE SPACES TO OPC.
           DISPLAY PANTALLA-BORRA1.
       D1.
           MOVE SPACES TO NUM-CO.
           ACCEPT NUM-CO AT 0932 WITH PROMPT.
           IF NUM-CO = SPACES GO TO D1.
           ADD 1 TO CONTA.
           IF CONTA > 11 PERFORM CARTEL1 THRU F-CARTEL1
           GO TO CIERRE.
       D111.
           READ CONTRI KEY IS NUM-CO
           INVALID KEY GO TO D11.
           DISPLAY (10, 32) "CLIENTE YA INCORPORADO"
           MOVE SPACES TO OPC
           ACCEPT (10, 57) OPC WITH PROMPT
           DISPLAY (10, 32) "                               "
           SUBTRACT 1 FROM CONTA
           GO TO D1.
       D11.
           IF NUM-CO = "0" PERFORM ENCABEZA GO TO MENU.
       D2.
           MOVE SPACES TO NOMBRE.
           ACCEPT (11, 32) NOMBRE WITH PROMPT.
           IF NOMBRE = SPACES GO TO D2.
       D3.
           MOVE SPACES TO DOMICILIO.
           ACCEPT (13, 32) DOMICILIO WITH PROMPT.
           IF DOMICILIO = SPACES GO TO D3.
       RES1.
           MOVE SPACES TO OPC.
           ACCEPT (17, 55) OPC WITH PROMPT.
           IF OPC = "1" GO TO GRABA.
           IF OPC = "2" GO TO CON-D.
           IF OPC = "3" GO TO MODIF.
           GO TO RES1.
       GRABA.
           WRITE REG-CON.
           GO TO CON-D.
       MODIF.
           MOVE ZERO TO CAM.
           DISPLAY PANTALLA-MODI.
           ACCEPT (18, 50) WITH PROMPT.
           IF CAM < 1 OR CAM > 3 GO TO MODIF.
           IF CAM = 1 PERFORM D1
           ELSE
           IF CAM = 2 PERFORM D2
           ELSE
           IF CAM = 3 PERFORM D3.
       F-MODI.
           DISPLAY (18, 15) "                                     ".
           MOVE ZERO TO CAM.
           GO TO RES1.

       CAR.
           PERFORM ENCABEZA.
           MOVE SPACES TO OPC.
           DISPLAY PANTALLA-CARGA.
           DISPLAY PANTALLA-SAL.
           DISPLAY PANTALLA-RES1.
           DISPLAY TV.
       CAR-C.
           MOVE 9  TO LIN.
           MOVE 61 TO COL.
           CLOSE CODI.
           OPEN I-O CODI.
       CAR-C1.
           READ CODI NEXT RECORD AT END GO TO CON-D1.
           IF N-CODI = ZEROS GO TO CAR-C1.
           MOVE N-CODI TO SN-CODI.
           MOVE B-CODI TO SB-CODI.
           DISPLAY S-CODI (LIN, COL).
           ADD 1 TO LIN.
           IF LIN = 21
           DISPLAY (22, 61) "P/CONTINUAR ENTER"
           MOVE SPACES TO OPC
           ACCEPT (22, 78) OPC WITH PROMPT
           DISPLAY (22, 61)            "
           PERFORM BL THRU BL-F
           MOVE 9 TO LIN.
           GO TO CAR-C1.
       CON-D1.
           MOVE ZEROS TO MONTO1.
           MOVE SPACES TO OPC.
           DISPLAY PANTALLA-BORRA.
       D5.
           MOVE SPACES TO CONT1.
           ACCEPT (07, 25) CONT1 WITH PROMPT.
           IF CONT1 = 0 PERFORM ENCABEZA GO TO MENU.
           IF CONT1 = SPACES GO TO D5.
           MOVE CONT1 TO NUM-CO.
           READ CONTRI KEY IS NUM-CO
           INVALID KEY
           DISPLAY (07, 52) "CONTRIBUYENTE INCORRECTO"
           MOVE SPACES TO OPC
           ACCEPT (07, 77) OPC WITH PROMPT
           DISPLAY (07, 52) "                        "
           GO TO D5.
           DISPLAY (09, 25) NOMBRE.
           DISPLAY (11, 25) DOMICILIO.
       D7.
           DISPLAY "         " AT 1323.
           MOVE ZEROS TO FEC-VEN1.
           DISPLAY "          " AT 1325.
           ACCEPT FEC-VEN1 AT 1325 WITH PROMPT.
           MOVE FEC-VEN1 TO FEC-VEN.
           DISPLAY (13, 25).
           DISPLAY (13, 27) "/".
           DISPLAY (13, 28) MMV.
           DISPLAY (13, 30) "/".
           DISPLAY (13, 31).
           IF FEC-VEN = ZEROS GO TO D7.
           IF MMV = ZEROS OR AAV = ZEROS GO TO D7.
           IF DDV < 0 OR DDV > 31 OR DDV = 00
           DISPLAY (13, 34) "ERROR EN EL DIA"
           MOVE SPACES TO OPC
           ACCEPT OPC AT 1349
           DISPLAY "                           "  AT 1323
           GO TO D7.
           IF MMV < 0 OR > 12 OR MMV = 00
           DISPLAY "ERROR EN EL MES" AT 1334
           WITH FOREGROUND-COLOR 6
           MOVE SPACES TO OPC
           ACCEPT OPC AT 1349
           DISPLAY "                           " AT 1323
           GO TO D7.
       D71.
           MOVE FEC-VEN1 TO FEC-VEN.
           START IMPU KEY IS EQUAL TO FEC-VEN
           INVALID KEY GO TO D8.
       D711.
           READ IMPU NEXT RECORD AT END GO TO D8.
           IF CONT = CONT1
           DISPLAY "VENCIMIENTO YA INCORPORADO" AT 2203
           MOVE SPACES TO OPC
           ACCEPT OPC AT 2239
           DISPLAY "                                   " AT 2203
           GO TO CON-D1.
           IF FEC-VEN NOT = FEC-VEN1 GO TO D8.
           GO TO D711.
       D8.
           DISPLAY "        " AT 1423.
           MOVE ZEROS TO FEC-PAG1.
           DISPLAY "          " AT 1425.
           ACCEPT FEC-PAG1 AT 1425 WITH PROMPT.
           MOVE FEC-PAG1 TO FEC-PAG.
           DISPLAY DDP AT 1425.
           DISPLAY "/" AT 1427.
           DISPLAY MMP AT 1428.
           DISPLAY "/" AT 1430.
           DISPLAY AAP AT 1431.
           IF FEC-PAG = ZEROS GO TO D8.
           IF MMP = ZEROS OR AAP = ZEROS GO TO D8.
           IF DDP < 0 OR DDP > 31 OR DDP = 00
           DISPLAY "ERROR EN EL DIA" AT 1434
           WITH FOREGROUND-COLOR 6
           MOVE SPACES TO OPC
           ACCEPT OPC AT 1449
           DISPLAY "                           " AT 1423
           GO TO D8.
           IF MMP < 0 OR MMP > 12 OR MMP = 00
           DISPLAY "ERROR EN EL MES" AT 1434
           WITH FOREGROUND-COLOR 6
           MOVE SPACES TO OPC
           ACCEPT OPC AT 1449
           DISPLAY "                            " AT 1423
           GO TO D8.
       D81.
           CLOSE CODI.
           OPEN I-O CODI.
           MOVE ZEROS TO TIP-IMP.
           ACCEPT TIP-IMP AT 1525 WITH PROMPT.
           MOVE TIP-IMP TO N-CODI.
           READ CODI KEY IS N-CODI
           INVALID KEY
           DISPLAY "CODIGO INCORRECTO" AT 1528
           WITH FOREGROUND-COLOR 6
           MOVE SPACES TO OPC
           ACCEPT OPC AT 1547 WITH PROMPT
           DISPLAY "                  " AT 1528
           GO TO D81.

       D82.
           MOVE ZEROS TO P1.
           ACCEPT P1 AT 1625 WITH PROMPT.

       D83.
           MOVE ZEROS TO AA.
           ACCEPT AA AT 1725 WITH PROMPT.
           MOVE AA TO ANO.
       D9.
           MOVE ZEROS TO MONTO1.
           ACCEPT MONTO1 AT 1825.
           IF MONTO1 = 0 GO TO D9.
           MOVE MONTO1 TO MONTO.
       RES2.
           MOVE SPACES TO OPC.
           ACCEPT OPC AT 2134.
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
           MOVE SPACES TO OPC.
           GO TO CON-D1.
       MODIF1.
           MOVE ZERO TO CAM.
           DISPLAY PANTALLA-MODI1.
           ACCEPT CAM AT 2237.
           IF CAM < 1 OR CAM > 6 GO TO MODIF1.
           IF CAM = 1 PERFORM D7
           GO TO F-MODI1
           ELSE
           IF CAM = 2 PERFORM D8
           GO TO F-MODI1
           ELSE
           IF CAM = 3 PERFORM D81
           GO TO F-MODI1
           ELSE
           IF CAM = 4 PERFORM D82
           GO TO F-MODI1
           ELSE
           IF CAM = 5 PERFORM D83
           GO TO F-MODI1 
           ELSE
           IF CAM = 6 PERFORM D9.
       F-MODI1.
           DISPLAY "                                        " AT 2203.
           MOVE ZERO TO CAM.
           GO TO RES2.

       BL.
           MOVE 9 TO LIN.
           MOVE 61 TO CL.
       BL1.
           DISPLAY "                  " AT LINCL.
           ADD 1 TO LIN.
           IF LIN = 21 GO TO BL-F.
           GO TO BL1.
       BL-F.

       CIERRE.
           CLOSE CONTRI IMPU CODI.
           CHAIN "MENU".
           STOP RUN.
