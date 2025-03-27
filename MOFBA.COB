       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MOFBA.
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

           SELECT RPAGOS ASSIGN TO DISK.

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

       FD  RPAGOS LABEL RECORD IS STANDARD
           VALUE OF FILE-ID "ARPAGOS"
           DATA RECORD IS REG-PAG.

       01  REG-PAG.
           03 REG-CIM            PIC X(25).
           03 REG-MPO            PIC 9(11)V99.
           03 REG-FEC.
              05 RFED            PIC 99.
              05 RFM             PIC 99.
              05 RFA             PIC 99.
           03 REG-GM             PIC 99.

       WORKING-STORAGE SECTION.
       77  OPC                   PIC XX VALUE SPACES.
       77  GUION                 PIC X(76) VALUE ALL "-".
       77  CAM                   PIC 9 VALUE ZERO.
       77  B                     PIC 9 VALUE ZERO.
       77  CONT1                 PIC X(25) VALUE SPACES.
       77  BAND                  PIC 9 VALUE ZERO.
       77  LIN                   PIC 99 VALUE ZEROS.
       77  CO                    PIC 99 VALUE ZEROS.
       77  FEC-VEN1              PIC 9(06) VALUE ZEROS.
       77  FEC-PAG1              PIC 9(06) VALUE ZEROS.
       77  AA1                   PIC 99 VALUE ZEROS.
       77  P11                   PIC 99 VALUE ZEROS.
       77  TIP-IMP1              PIC 99 VALUE ZEROS.
       77  MONTO1                PIC Z(11).ZZ VALUE ZEROS.

       01  LICL.
           03 LI                 PIC 99.
           03 CL                 PIC 99.

       01  S-CODI.
           03 SN-CODI            PIC Z9.
           03 F                  PIC X VALUE "-".
           03 SB-CODI            PIC X(15) VALUE SPACES.

       SCREEN SECTION.

       01  MENU-CARGA.
           03 LINE 09 COLUMN 28 REVERSE-VIDEO FOREGROUND-COLOR 4
              VALUE "MOD./BAJA DEL SISTEMA".
           03 LINE 11 COLUMN 28
              VALUE "1-MOD/BAJA DE CLIENTES     ".
           03 LINE 13 COLUMN 28
              VALUE "2-MOD/BAJA MOVIMIENTOS     ".
           03 LINE 15 COLUMN 28
              VALUE "3-MOD/BAJA TIP. DE VTOS    ".
           03 LINE 17 COLUMN 28
              VALUE "4-REGRESO MENU PRINCIPAL   ".
           03 LINE 19 COLUMN 28 REVERSE-VIDEO FOREGROUND-COLOR 4
              VALUE "INGRESE OPCION:      ".

       01  PANTALLA-CONTR.
           03 LINE 07 COLUMN 10 FOREGROUND-COLOR 5
              VALUE "1-NRO DE CLIENTE......:".
           03 LINE 09 COLUMN 10 FOREGROUND-COLOR 5
              VALUE "2-APELLIDO Y NOMBRE...:".
           03 LINE 11 COLUMN 10 FOREGROUND-COLOR 5
              VALUE "3-DOMICILIO...........:".

       01  PANTALLA-CODI.
           03 LINE 11 COLUMN 10 FOREGROUND-COLOR 5
              VALUE "1-NRO DE VENCIMIENTOS:".
           03 LINE 13 COLUMN 10 FOREGROUND-COLOR 5
              VALUE "2-NOMBRE VENCIMIENTOS:".

       01  PANTALLA-CARGA.
           03 LINE 7 COLUMN 3 FOREGROUND-COLOR 5
              VALUE "  NRO CLIENTE......:".
           03 LINE 9 COLUMN 3 FOREGROUND-COLOR 5
              VALUE "  APELLIDO Y NOMBRE:".
           03 LINE 11 COLUMN 3 FOREGROUND-COLOR 5
              VALUE "  DOMICILIO........:".
           03 LINE 13 COLUMN 3 FOREGROUND-COLOR 5
              VALUE "1-FECHA VENCIMIENTO:".
           03 LINE 14 COLUMN 3 FOREGROUND-COLOR 5
              VALUE "2-FECHA DE PAGO....:".
           03 LINE 15 COLUMN 3 FOREGROUND-COLOR 5
              VALUE "3-TIPO DE VENC.....:".
           03 LINE 16 COLUMN 3 FOREGROUND-COLOR 5
              VALUE "4-CUOTA............:".
           03 LINE 17 COLUMN 3 FOREGROUND-COLOR 5
              VALUE "5-A#O DE LA CUOTA..:".
           03 LINE 18 COLUMN 3 FOREGROUND-COLOR 5
              VALUE "6-MONTO............:".

       01  PANTALLA-RES.
           03 LINE 17 COLUMN 25 REVERSE-VIDEO FOREGROUND-COLOR 4
              VALUE "1=GRABA  2=ANULA  3=MODIFICA  4=BAJA".

       01  PANTALLA-MODI.
           03 LINE 18 COLUMN 5 FOREGROUND-COLOR 4
              VALUE "INGRESE NRO DE CAMPO A MODIFICAR".

       01  PANTALLA-RES1.
           03 LINE 21 COLUMN 03 REVERSE-VIDEO FOREGROUND-COLOR 4
              VALUE "1=GRABA  2=ANULA  3=MODIFICA  4=BAJA".

       01  PANTALLA-MODI1.
           03 LINE 22 COLUMN 3 FOREGROUND-COLOR 4
              VALUE "INGRESE NRO DE CAMPO A MODIFICAR".

       01  PANTALLA-SAL.
           03 LINE 23 COLUMN 03 REVERSE-VIDEO FOREGROUND-COLOR 4
              VALUE "   PARA SALIR TIPEAR 0      ".

       01  PANTALLA-SAL1.
           03 LINE 19 COLUMN 25 REVERSE-VIDEO FOREGROUND-COLOR 4
              VALUE "   PARA SALIR TIPEAR 0      ".

       01  PANTALLA-BORRA.
           03 LINE 7 COLUMN 24
              VALUE "                              ".
           03 LINE 9 COLUMN 24
              VALUE "                              ".
           03 LINE 11 COLUMN 24
              VALUE "                              ".
           03 LINE 13 COLUMN 24
              VALUE "                              ".
           03 LINE 14 COLUMN 24
              VALUE "                              ".
           03 LINE 15 COLUMN 24
              VALUE "                              ".
           03 LINE 16 COLUMN 24
              VALUE "                              ".
           03 LINE 17 COLUMN 24
              VALUE "                              ".
           03 LINE 18 COLUMN 24
              VALUE "                              ".
           03 LINE 21 COLUMN 42
              VALUE "     ".

       01  PANTALLA-BORRA1.
           03 LINE 09 COLUMN 34
              VALUE "                               ".
           03 LINE 11 COLUMN 34
              VALUE "                                  ".
           03 LINE 13 COLUMN 34
              VALUE "   ".
           03 LINE 17 COLUMN 62
              VALUE "      ".

       01  PANTALLA-BORRA2.
           03 LINE 11 COLUMN 32
              VALUE "     ".
           03 LINE 13 COLUMN 32
              VALUE "                      ".
           03 LINE 17 COLUMN 62
              VALUE "       ".

       PROCEDURE DIVISION.
       INICIO.
           OPEN I-O CONTRI.
           OPEN I-O IMPU.
           OPEN I-O CODI.
           OPEN I-O RPAGOS.
       ENCABEZA.
           CALL "CALEN" USING B.
           DISPLAY GUION AT 0603 WITH FOREGROUND-COLOR 3.
       MENU.
           DISPLAY MENU-CARGA.
           MOVE SPACES TO OPC.
           ACCEPT OPC AT 1952 WITH PROMPT.
           IF OPC = "1" GO TO CON.
           IF OPC = "2" GO TO CAR.
           IF OPC = "3" GO TO COD.
           IF OPC = "4" GO TO CIERRE.
           GO TO MENU.

       CON.
           PERFORM ENCABEZA.
           MOVE SPACES TO OPC.
           DISPLAY PANTALLA-CONTR.
           DISPLAY PANTALLA-RES.
           DISPLAY PANTALLA-SAL1.
       CON-D.
           MOVE SPACES TO OPC.
           DISPLAY PANTALLA-BORRA1.
           MOVE SPACES TO NUM-CO.
       D1.
           CLOSE CONTRI.
           OPEN I-O CONTRI.
           ACCEPT NUM-CO AT 0734 WITH PROMPT.
           IF NUM-CO = 0 GO TO ENCABEZA.
           IF NUM-CO = SPACES GO TO D1.
       D111.
           READ CONTRI KEY IS NUM-CO
           INVALID KEY
           DISPLAY "CLIENTE NO INCORPORADO" AT 0838
           WITH FOREGROUND-COLOR 3
           MOVE SPACES TO OPC
           ACCEPT OPC AT 0862 WITH PROMPT
           DISPLAY "                        " AT 0838
           GO TO CON-D.
           DISPLAY NOMBRE AT 0934.
           DISPLAY DOMICILIO AT 1134.
           GO TO RES1.
       D2.
           MOVE SPACES TO NOMBRE.
           ACCEPT NOMBRE WITH PROMPT AT 0934.
       D4.
           MOVE SPACES TO DOMICILIO.
           ACCEPT DOMICILIO WITH PROMPT AT 1134.
       RES1.
           MOVE SPACES TO OPC.
           ACCEPT OPC AT 1762 WITH PROMPT.
           IF OPC = "1" GO TO GRABA.
           IF OPC = "2" GO TO CON-D.
           IF OPC = "3" GO TO MODIF.
           IF OPC = "4" GO TO BAJA.
           GO TO RES1.
       GRABA.
           CLOSE CONTRI.
           OPEN I-O CONTRI.
           REWRITE REG-CON.
           GO TO CON-D.
       MODIF.
           DISPLAY PANTALLA-MODI.
           MOVE ZERO TO CAM.
           ACCEPT CAM AT 1840 WITH PROMPT.
           IF CAM < 1 OR CAM > 4 GO TO MODIF.
           IF CAM = SPACES GO TO MODIF.
           IF CAM = 1 DISPLAY "NO SE PUEDE MODIFICAR" AT 0749
           WITH FOREGROUND-COLOR 2
           DISPLAY "EL CAMPO, DAR DE BAJA" AT 0849
           WITH FOREGROUND-COLOR 2
           MOVE SPACES TO OPC
           ACCEPT OPC AT 0870 WITH PROMPT
           DISPLAY "                     " AT 0749
           DISPLAY "                     "  AT 0849
           GO TO F-MODI
           ELSE
           IF CAM = 2 PERFORM D2
           ELSE
           IF CAM = 3 PERFORM D4.
       F-MODI.
           DISPLAY "                                     " AT 1805.
           MOVE ZERO TO CAM.
           GO TO RES1.

       BAJA.
           DELETE CONTRI INVALID KEY GO TO CON-D.
           DISPLAY "REGISTRO DADO DE BAJA" AT 1825
           WITH FOREGROUND-COLOR 4.
           MOVE SPACES TO OPC.
           ACCEPT OPC AT 1847.
           DISPLAY "                      " AT 1825.
           GO TO CON-D.

       CAR.
           PERFORM ENCABEZA
           MOVE SPACES TO OPC.
           DISPLAY PANTALLA-CARGA.
           DISPLAY PANTALLA-SAL.
           DISPLAY PANTALLA-RES1.
           DISPLAY  "TIPO DE VENCIMIENTOS" AT 0858
           WITH FOREGROUND-COLOR 6.
       CAR-C.
           MOVE 9 TO LI.
           MOVE 59 TO CL.
       CAR-C1.
           READ CODI NEXT RECORD AT END GO TO CON-D1.
           IF N-CODI = 0 GO TO CAR-C1.
           MOVE N-CODI TO SN-CODI.
           MOVE B-CODI TO SB-CODI.
           DISPLAY S-CODI AT LICL.
           ADD 1 TO LI.
           IF LI = 21
           DISPLAY "P/CONTINUAR ENTER" AT 2261
           WITH FOREGROUND-COLOR 6
           ACCEPT OPC AT 2278 WITH PROMPT
           DISPLAY "                  " AT 2261
           PERFORM BL THRU BL-F
           MOVE 9 TO LI.
           GO TO CAR-C1.
       CON-D1.
           MOVE SPACES TO OPC.
       D3.
           DISPLAY PANTALLA-BORRA.
           MOVE SPACES TO CONT1.
           ACCEPT CONT1 AT 0724 WITH PROMPT.
           IF CONT1 = 0 GO TO ENCABEZA.
           IF CONT1 = SPACES GO TO D3.
           CLOSE IMPU RPAGOS CONTRI CODI.
           OPEN I-O IMPU.
           OPEN I-O RPAGOS.
           OPEN I-O CONTRI.
           OPEN I-O CODI.
           MOVE CONT1 TO NUM-CO.
           READ CONTRI KEY IS NUM-CO
           INVALID KEY
           DISPLAY "CLIENTE NO ENCONTRADO" AT 0756
           WITH FOREGROUND-COLOR 5
           ACCEPT OPC AT 0777 WITH PROMPT
           DISPLAY "                      " AT 0756
           GO TO D3.
           DISPLAY NOMBRE AT 0924.
           DISPLAY DOMICILIO AT 1124.
           DISPLAY "         " AT 1324.
       D3I.
           MOVE ZEROS TO FEC-VEN1.
           ACCEPT FEC-VEN1 AT 1324 WITH PROMPT.
           MOVE FEC-VEN1 TO FEC-VEN.
           DISPLAY DDV AT 1324.
           DISPLAY "/" AT 1326.
           DISPLAY MMV AT 1327.
           DISPLAY "/" AT 1329.
           DISPLAY AAV AT 1330.
           MOVE CONT1 TO CONT.
           START IMPU KEY IS EQUAL CONT
           INVALID KEY
           DISPLAY "CLIENTE NO ENCONTRADO  " AT 1333
           WITH FOREGROUND-COLOR 5
           ACCEPT OPC AT 1361 WITH PROMPT
           DISPLAY "                         " AT 1333
           GO TO CON-D1.

       D3R.
           READ IMPU NEXT RECORD AT END GO TO D3.
           IF FEC-VEN = FEC-VEN1 GO TO L-P.
           GO TO D3R.
       L-P.
           READ RPAGOS AT END MOVE 1 TO BAND
           GO TO PRESENTAR.
           IF REG-CIM = CONT1 MOVE 0 TO BAND
           GO TO PRESENTAR.
           GO TO L-P.
       PRESENTAR.
           DISPLAY DDP AT 1424.
           DISPLAY "/" AT 1426.
           DISPLAY MMP AT 1427.
           DISPLAY "/" AT 1429.
           DISPLAY AAP AT 1430.
           DISPLAY TIP-IMP AT 1524.
           DISPLAY P1 AT 1624.
           DISPLAY AA AT 1724.
           MOVE MONTO TO MONTO1.
           DISPLAY MONTO1 AT 1824.
           GO TO RES2.

       D7M.
           DISPLAY "          " AT 1324.
           MOVE ZEROS TO FEC-VEN1.
           ACCEPT FEC-VEN1 AT 1324 WITH PROMPT.
           MOVE FEC-VEN1 TO FEC-VEN.
           DISPLAY DDV AT 1324.
           DISPLAY "/" AT 1326.
           DISPLAY MMV AT 1327.
           DISPLAY "/" AT 1329.
           DISPLAY AAV AT 1330.
           IF FEC-VEN = 0 GO TO D7M.
           IF FEC-VEN = SPACES GO TO D7M.
           IF MMV = SPACES OR AAV = SPACES GO TO D7M.
           IF DDV < 0 OR DDV > 31 OR DDV = 00
           DISPLAY "ERROR EN EL DIA" AT 1332
           WITH FOREGROUND-COLOR 5
           MOVE SPACES TO OPC
           ACCEPT OPC AT 1348 WITH PROMPT
           DISPLAY "                           " AT 1322
           GO TO D7M.
           IF MMV < 0 OR > 12 OR MMV = 00
           DISPLAY "ERROR EN EL MES" AT 1332
           WITH FOREGROUND-COLOR 5
           ACCEPT OPC AT 1348 WITH PROMPT
           DISPLAY "                           " AT 1322
           GO TO D7M.

       D8M.
           DISPLAY "          " AT 1424.
           MOVE ZEROS TO FEC-PAG1.
           ACCEPT FEC-PAG1 AT 1424 WITH PROMPT.
           DISPLAY DDP AT 1424.
           DISPLAY "/" AT 1426.
           DISPLAY MMP AT 1427.
           DISPLAY "/" AT 1429.
           DISPLAY AAP AT 1430.
           IF FEC-PAG = SPACES GO TO D8M.
           IF MMP = SPACES OR AAP = SPACES GO TO D8M.
           IF DDP < 0 OR DDP > 31 OR DDP = 00
           DISPLAY "ERROR EN EL DIA" AT 1432
           WITH FOREGROUND-COLOR 5
           ACCEPT OPC AT 1448 WITH PROMPT
           DISPLAY "                           " AT 1422
           GO TO D8M.
           IF MMP < 0 OR MMP > 12
           DISPLAY "ERROR EN EL MES" AT 1432
           WITH FOREGROUND-COLOR 5
           ACCEPT OPC AT 1448 WITH PROMPT
           DISPLAY "                            " AT 1422
           GO TO D8M.
           MOVE FEC-PAG1 TO FEC-PAG.

       D81M.
           MOVE ZEROS TO TIP-IMP1.
           ACCEPT TIP-IMP1 AT 1524 WITH PROMPT.
           MOVE TIP-IMP1 TO TIP-IMP.

       D82M.
           MOVE ZEROS TO P11.
           ACCEPT P11 AT 1624 WITH PROMPT.
           IF P11 < 1 OR P11 > 12 GO TO D82M.
           MOVE P11 TO P1.

       D83M.
           MOVE ZEROS TO AA1.
           ACCEPT AA1 AT 1724 WITH PROMPT.
           MOVE AA1 TO AA ANO.
       D9M.
           MOVE ZEROS TO MONTO1.
           ACCEPT MONTO1 AT 1824 WITH PROMPT.
           MOVE MONTO1 TO MONTO.
           IF MONTO = 00000000000.00
           MOVE ZEROS TO FEC-PAG1
           MOVE FEC-PAG1 TO FEC-PAG
           DISPLAY "          " AT 1424
           DISPLAY DDP AT 1424
           DISPLAY "/" AT 1426
           DISPLAY MMP AT 1427
           DISPLAY "/" AT 1429
           DISPLAY AAP AT 1430.

       RES2.
           MOVE SPACES TO OPC.
           ACCEPT OPC AT 2142 WITH PROMPT.
           IF OPC = "1" GO TO GRABA1.
           IF OPC = "2" GO TO CON-D1.
           IF OPC = "3" GO TO MODIF1.
           IF OPC = "4" GO TO BAJA1.
           GO TO RES2.
       GRABA1.
           REWRITE REG-IMP.
           IF BAND = 1 GO TO GRABA2.
           MOVE FEC-PAG TO REG-FEC.
           MOVE TIP-IMP TO REG-GM.
           MOVE MONTO TO REG-MPO.
           REWRITE REG-PAG.
       GRABA2.
           MOVE SPACES TO OPC.
           GO TO CON-D1.
       MODIF1.
           DISPLAY PANTALLA-MODI1.
           MOVE ZERO TO CAM.
           ACCEPT CAM AT 2239 WITH PROMPT.
           IF CAM < 1 OR CAM > 6 GO TO MODIF.
           IF CAM = SPACES GO TO MODIF1.
           IF CAM = 1 PERFORM D7M
           GO TO F-MODI1
           ELSE
           IF CAM = 2 PERFORM D8M
           GO TO F-MODI1
           ELSE
           IF CAM = 3 PERFORM D81M
           GO TO F-MODI1
           ELSE
           IF CAM = 4 PERFORM TEL THRU F-TEL
           GO TO F-MODI1
           ELSE
           IF CAM = 5 PERFORM TEL THRU F-TEL
           GO TO F-MODI1
           ELSE
           IF CAM = 6 PERFORM D9M.
       F-MODI1.
           DISPLAY "                                        " AT 2203.
           MOVE ZERO TO CAM.
           GO TO RES2.

       TEL.
           DISPLAY "NO SE PUEDE MODIFICAR" AT 1627
           WITH FOREGROUND-COLOR 2
           DISPLAY "EL CAMPO,DAR DE BAJA" AT 1727
           WITH FOREGROUND-COLOR 2
           MOVE SPACES TO OPC
           ACCEPT OPC AT 1749
           DISPLAY "                      " AT 1627
           DISPLAY "                      " AT 1727.
       F-TEL.
           EXIT.

       BAJA1.
           DELETE IMPU INVALID KEY GO TO CON-D1.
           DISPLAY "REGISTRO DADO DE BAJA" AT 2225
           WITH FOREGROUND-COLOR 4.
           MOVE SPACES TO OPC.
           ACCEPT OPC AT 2247 WITH PROMPT.
           DISPLAY "                      " AT 2225.
           IF BAND = 1 GO TO BAJA6.
           MOVE ZEROS TO REG-FEC.
           REWRITE REG-PAG.
       BAJA6.
           GO TO CON-D1.

       BL.
           MOVE 9 TO LI.
           MOVE 59 TO CL.
       BL1.
           DISPLAY "                     " AT LICL.
           ADD 1 TO LI.
           IF LI = 21 GO TO BL-F.
           GO TO BL1.
       BL-F.

       COD.
           PERFORM ENCABEZA.
           MOVE SPACES TO OPC.
           DISPLAY PANTALLA-CODI.
           DISPLAY PANTALLA-RES.
           DISPLAY PANTALLA-SAL1.
       COD-D.
           MOVE SPACES TO OPC.
           DISPLAY PANTALLA-BORRA2.
       H1.
           MOVE ZEROS TO N-CODI.
           ACCEPT N-CODI AT 1134 WITH PROMPT.
           IF N-CODI = 0 GO TO ENCABEZA.
       H111.
           READ CODI KEY IS N-CODI
           INVALID KEY
           DISPLAY "CODIGO NO ENCONTRADO" AT 1138
           WITH FOREGROUND-COLOR 5
           MOVE SPACES TO OPC
           ACCEPT OPC AT 1160 WITH PROMPT
           DISPLAY "                    " AT 1138
           GO TO H1.

       H2.
           DISPLAY B-CODI AT 1334.
       HRES.
           MOVE SPACES TO OPC.
           ACCEPT OPC AT 1764 WITH PROMPT.
           IF OPC = "1" GO TO HGRABA.
           IF OPC = "2" GO TO COD-D.
           IF OPC = "3" GO TO HMODIF.
           IF OPC = "4" GO TO HBAJA.
           GO TO HRES.
       HGRABA.
           REWRITE REG-CODI.
           GO TO COD-D.
       HMODIF.
           DISPLAY PANTALLA-MODI.
           MOVE ZERO TO CAM.
           ACCEPT CAM AT 1840 WITH PROMPT.
           IF CAM < 1 OR CAM > 2 GO TO HMODIF.
           IF CAM = SPACES GO TO HMODIF.
           IF CAM = 1
           DISPLAY "NO SE PUEDE MODIFICAR" AT 1149
           WITH FOREGROUND-COLOR 3
           DISPLAY "EL CAMPO, DAR DE BAJA" AT 1249
           WITH FOREGROUND-COLOR 3
           MOVE SPACES TO OPC
           ACCEPT OPC AT 1272 WITH PROMPT
           DISPLAY "                     " AT 1149
           DISPLAY "                          " AT 1249
           ELSE
           IF CAM = 2 ACCEPT B-CODI AT 1334 WITH PROMPT.
       F-HMODIF.
           DISPLAY "                                    " AT 1805.
           MOVE ZERO TO CAM.
           GO TO HRES.
       HBAJA.
           DELETE CODI INVALID KEY GO TO CON-D.
           DISPLAY "REGISTRO DADO DE BAJA" AT 1825
           WITH FOREGROUND-COLOR 3.
           MOVE SPACES TO OPC.
           ACCEPT OPC AT 1847 WITH PROMPT.
           DISPLAY "                      " AT 1825.
           GO TO COD-D.

       CIERRE.
           CLOSE CONTRI IMPU.
           CHAIN "MENU".
           STOP RUN.
