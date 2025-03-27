       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PAGOS.  
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

           SELECT RPAGOS ASSIGN TO DISK.

           SELECT IMPRE ASSIGN TO PRINTER.

           SELECT RDATOS ASSIGN TO DISK.

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

       FD  RPAGOS LABEL RECORD IS STANDARD
           VALUE OF FILE-ID "ARPAGOS"
           DATA RECORD IS REG-PAG.

       01  REG-PAG.
           03 REG-CIM           PIC X(25).
           03 REG-MPO           PIC 9(11)V99.
           03 REG-FEC.
              05 RFD            PIC 99.
              05 RFM            PIC 99.
              05 RFA            PIC 99.
           03 REG-GM            PIC 99.

       FD  IMPRE LABEL RECORD IS OMITTED.

       01  RENGLON              PIC X(132).

       FD  CODI LABEL RECORD IS STANDARD
           VALUE OF FILE-ID "ARCODI"
           DATA RECORD IS REG-CODI.

       01  REG-CODI.
           03 N-CODI            PIC 99.
           03 B-CODI            PIC X(15).

       FD  RDATOS LABEL RECORD IS STANDARD
           VALUE OF FILE-ID "ARDATOS"
           DATA RECORD IS REG-DATOS.

       01  REG-DATOS.
           03 REG-NOMEMP         PIC X(40).
           03 REG-CIUD           PIC X(40).

       WORKING-STORAGE SECTION.
       77  OPC                   PIC XX VALUE SPACES.
       77  GUION                 PIC X(77) VALUE ALL "-".
       77  CAM                   PIC 9 VALUE ZERO.
       77  B                     PIC 9 VALUE ZERO.
       77  MONTO1                PIC Z(11).99 VALUE ZEROS.
       77  CONT1                 PIC X(25) VALUE SPACES.
       77  C-CONTA               PIC 9 VALUE ZERO.
       77  C-HOJA                PIC 999 VALUE ZEROS.
       77  T-GM                  PIC 9(12)V99 VALUE ZEROS.
       77  C-REG                 PIC 99 VALUE ZEROS.
       77  FEC-VEN1              PIC 9(06) VALUE ZEROS.
       77  FEC-PAG1              PIC 9(06) VALUE ZEROS.

       01  FECHA.
           03 AAF                PIC 99.
           03 MMF                PIC 99.
           03 DDF                PIC 99.

       01  PARAM1.
           03 QTY                PIC 9(11)V99.
           03 MAX                PIC 9(3) VALUE 80.
           03 SKP                PIC 9(2) VALUE 00.
           03 SWA                PIC X    VALUE "1".
           03 SWB                PIC X    VALUE "1".

       01  PARAM2.
           03 LETRA-1            PIC X(80).
           03 LETRA-2            PIC X(80).
           03 FILLER             PIC X(90).

       01  S-CODI.
           03 SN-CODI            PIC Z9.
           03 F                  PIC X  VALUE "-".
           03 SB-CODI            PIC X(15) VALUE SPACES.

       01  S-REG.
           03 S-RNC              PIC X(25).
           03 F                  PIC X(02) VALUE SPACES.
           03 RF1                PIC 99.
           03 F                  PIC X VALUE "/".
           03 RF2                PIC 99.
           03 F                  PIC X VALUE "/".
           03 RF3                PIC 99.
           03 F                  PIC XX VALUE SPACES.
           03 S-RCO              PIC X(15).
           03 F                  PIC X(03) VALUE SPACES.
           03 F                  PIC XX VALUE "$ ".
           03 S-IMP              PIC ZZ,ZZZ,ZZZ,ZZ9.99.

       01  S-REG1.
           03 S-NOM              PIC X(25).
           03 F                  PIC X(30) VALUE SPACES.
           03 S-UBI              PIC X(25).

       01  L1.
           03 F PIC X(80) VALUE ALL "*".

       01  L3.
           03 F PIC X(25) VALUE "* SISTEMA DE VENCIMIENTOS".
           03 F PIC X(38) VALUE SPACES.
           03 F PIC X(07) VALUE "FECHA: ".
           03 S-DD  PIC 99.
           03 F PIC X VALUE "/".
           03 S-MM  PIC 99.
           03 F PIC X VALUE "/".
           03 S-AA  PIC 99.
           03 F PIC XX VALUE " *".

       01  L4.
           03 F PIC X(02) VALUE "* ".
           03 S-NOMEMP    PIC X(40) VALUE SPACES.
           03 F PIC X(37) VALUE SPACES.
           03 F PIC X     VALUE "*".

       01  L5.
           03 F PIC X(02) VALUE "* ".
           03 S-CIUD PIC X(40) VALUE SPACES.
           03 F PIC X(23) VALUE SPACES.
           03 F PIC X(10) VALUE "HOJA NRO: ".
           03 S-HOJA PIC ZZ9.
           03 F PIC X(02) VALUE " *".

       01  L6.
           03 F PIC X(30) VALUE SPACES.
           03 F PIC X(20) VALUE "VENCIMIENTOS PAGADOS".

       01  L7.
           03 F PIC X(30) VALUE SPACES.
           03 F PIC X(20) VALUE "------------ -------".

       01  L8.
           03 F PIC X(27) VALUE "TOTAL VENCIMIENTOS PAGADOS:".
           03 F PIC X(03) VALUE " $ ".
           03 MON-T PIC Z,ZZZ,ZZZ,ZZ9.99.

       SCREEN SECTION.

       01  PANTALLA-PAGOS.
           03 LINE 7 COLUMN 3 FOREGROUND-COLOR 4
              VALUE "  NRO CONTRIBUYENTE:".
           03 LINE 9 COLUMN 3 FOREGROUND-COLOR 4
              VALUE "  APELLIDO Y NOMBRE:".
           03 LINE 11 COLUMN 3 FOREGROUND-COLOR 4
              VALUE "  UBICACION........:".
           03 LINE 13 COLUMN 3 FOREGROUND-COLOR 4
              VALUE "  FECHA VENCIMIENTO:".
           03 LINE 14 COLUMN 3 FOREGROUND-COLOR 4
              VALUE "1-FECHA DE PAGO....:".
           03 LINE 15 COLUMN 3 FOREGROUND-COLOR 4
              VALUE "2-MONTO............:".
           03 LINE 16 COLUMN 3 FOREGROUND-COLOR 4
              VALUE "  CUOTA............:".
           03 LINE 17 COLUMN 3 FOREGROUND-COLOR 4
              VALUE "  A#O DE LA CUOTA.:".
           03 LINE 18 COLUMN 3 FOREGROUND-COLOR 4
              VALUE "  TIPO DE IMPUESTO.:".

       01  PANTALLA-RES1.
           03 LINE 22 COLUMN 25 REVERSE-VIDEO FOREGROUND-COLOR 6
              VALUE "1=GRABA  2=ANULA  3=MODIFICA".

       01  PANTALLA-MODI1.
           03 LINE 23 COLUMN 3 FOREGROUND-COLOR 6
              VALUE "INGRESE NRO DE CAMPO A MODIFICAR".

       01  PANTALLA-SAL.
           03 LINE 24 COLUMN 25 REVERSE-VIDEO FOREGROUND-COLOR 6
              VALUE "   PARA SALIR TIPEAR 0      ".

       01  PANTALLA-BORRA.
           03 LINE 7 COLUMN 22
              VALUE "                              ".
           03 LINE 9 COLUMN 22
              VALUE "                              ".
           03 LINE 11 COLUMN 22
              VALUE "                              ".
           03 LINE 13 COLUMN 22
              VALUE "                              ".
           03 LINE 14 COLUMN 22
              VALUE "                              ".
           03 LINE 15 COLUMN 22
              VALUE "                              ".
           03 LINE 16 COLUMN 22
              VALUE "                              ".
           03 LINE 17 COLUMN 22
              VALUE "                     ".
           03 LINE 18 COLUMN 22
              VALUE "                     ".
           03 LINE 22 COLUMN 54
              VALUE "                     ".

       01  B-PAN.
           03 BLANK SCREEN.

       01  INIC.
           03 LINE 12 COLUMN 10 FOREGROUND-COLOR 2
              VALUE "EXISTEN DATOS GRABADOS 1=BORRA DATOS".
           03 LINE 12 COLUMN 47 FOREGROUND-COLOR 2
              VALUE "2=INCORPORA DATOS".
       01  LP.
           03 LINE 12 COLUMN 23 FOREGROUND-COLOR 4
              VALUE "EMITE EL LISTADO DE PAGOS  S/N".

       PROCEDURE DIVISION.
       INICIO.
           OPEN I-O CONTRI.
           OPEN I-O IMPU.
           OPEN I-O CODI.
       INICIO1.
           DISPLAY B-PAN.
           DISPLAY INIC.
           ACCEPT OPC AT 1266 WITH PROMPT.
           IF OPC = "1" OPEN OUTPUT RPAGOS GO TO ENCABEZA.
           IF OPC = "2" OPEN EXTEND RPAGOS GO TO ENCABEZA.
           GO TO INICIO1.

       ENCABEZA.
           CALL "CALEN" USING B.
           DISPLAY GUION AT 0603 WITH FOREGROUND-COLOR 3.

       PAG.
           MOVE SPACES TO OPC.
           DISPLAY PANTALLA-PAGOS.
           DISPLAY PANTALLA-SAL.
           DISPLAY PANTALLA-RES1.
       CON-D1.
           CLOSE CONTRI IMPU.
           OPEN I-O CONTRI.
           OPEN I-O IMPU.
           DISPLAY PANTALLA-BORRA.
       D3.
           MOVE SPACES TO CONT1.
           ACCEPT CONT1 AT 0724 WITH PROMPT.
           IF CONT1 = "0" GO TO LISTADO.
           IF CONT1 = SPACES GO TO D3.
           MOVE CONT1 TO NUM-CO.
           READ CONTRI KEY IS NUM-CO
           INVALID KEY
           DISPLAY "CLIENTE INCORRECTO" AT 0756
           MOVE SPACES TO OPC
           ACCEPT OPC AT 0776 WITH PROMPT
           DISPLAY "                     " AT 0756
           GO TO D3.
           DISPLAY NOMBRE AT 0924.
           DISPLAY UBICA AT 1124.

       D7.
           MOVE ZEROS TO FEC-VEN1.
           DISPLAY "          " AT 1324.
           ACCEPT FEC-VEN1 AT 1324 WITH PROMPT.
           MOVE FEC-VEN1 TO FEC-VEN.
           DISPLAY DDV AT 1324.
           DISPLAY "/" AT 1326.
           DISPLAY MMV AT 1327.
           DISPLAY "/" AT 1329.
           DISPLAY AAV AT 1330.
           IF FEC-VEN = SPACES GO TO D7.
           IF MMV = 00 OR AAV = 00 GO TO D7.
           IF DDV < 0 OR DDV > 31 OR DDV = 00
           DISPLAY "ERROR EN EL DIA" AT 1334
           WITH FOREGROUND-COLOR 6
           MOVE SPACES TO OPC
           ACCEPT OPC AT 1350 WITH PROMPT
           DISPLAY "                           " AT 1322
           GO TO D7.
           IF MMV < 0 OR > 12 OR MMV = 00
           DISPLAY "ERROR EN EL MES" AT 1334
           WITH FOREGROUND-COLOR 6
           MOVE SPACES TO OPC
           ACCEPT  OPC AT 1350 WITH PROMPT
           DISPLAY "                           " AT 1334
           GO TO D7.
       D71.
           START IMPU KEY IS EQUAL TO FEC-VEN
           INVALID KEY GO TO D7C.
       D711.
           READ IMPU NEXT RECORD AT END GO TO D7C.
           IF CONT = CONT1 GO TO D7D.
           GO TO D711.
       D7C.
           DISPLAY "FECHA DE VENCIMIENTO INEXISTENTE   " AT 2303
           WITH FOREGROUND-COLOR 4
           MOVE SPACES TO OPC
           ACCEPT  OPC AT 2339 WITH PROMPT
           DISPLAY "                                   " AT 2303
           GO TO CON-D1.
       D7D.
           IF FEC-PAG = 000000 GO TO D8.
           DISPLAY "VENCIMIENTO YA PAGADO " AT 1424
           WITH FOREGROUND-COLOR 4
           MOVE SPACES TO OPC
           ACCEPT OPC AT 1447 WITH PROMPT
           DISPLAY "                                " AT 1424
           GO TO CON-D1.

       D8.
           MOVE ZEROS TO FEC-PAG1.
           DISPLAY "          " AT 1424.
           ACCEPT FEC-PAG1 AT 1424 WITH PROMPT.
           MOVE FEC-PAG1 TO FEC-PAG.
           DISPLAY DDP AT 1424.
           DISPLAY "/" AT 1426.
           DISPLAY MMP AT 1427.
           DISPLAY "/" AT 1429.
           DISPLAY AAP AT 1430.
           IF FEC-PAG = 0 GO TO D8.
           IF MMP = 0 OR AAP = 0 GO TO D8.
           IF DDP < 0 OR DDP > 31 OR DDP = 00
           DISPLAY "ERROR EN EL DIA" AT 1434
           WITH FOREGROUND-COLOR 4
           MOVE SPACES TO OPC
           ACCEPT OPC AT 1450
           DISPLAY "                           " AT 1434
           GO TO D8.
           IF MMP < 0 OR MMP > 12 OR MMP = 00
           DISPLAY "ERROR EN EL MES" AT 1434
           WITH FOREGROUND-COLOR 4
           MOVE SPACES TO OPC
           ACCEPT OPC AT 1450 WITH PROMPT
           DISPLAY "                            " AT 1434
           GO TO D8.

       D81.
           MOVE ZEROS TO MONTO1.
           ACCEPT MONTO1 AT 1524 WITH PROMPT.
           IF MONTO1 = 0 GO TO D81.

       D82.
           DISPLAY P1 AT 1624.

       D83.
           DISPLAY AA AT 1724.

       D9.
           PERFORM BC THRU F-BC.
           DISPLAY TIP-VEN AT 1824.
           DISPLAY B-CODI AT 1828.
       RES2.
           MOVE SPACES TO OPC.
           ACCEPT OPC AT 2254 WITH PROMPT.
           IF OPC = "1" GO TO GRABA1.
           IF OPC = "2" GO TO CON-D1.
           IF OPC = "3" GO TO MODIF1.
           GO TO RES2.
       GRABA1.
           MOVE MONTO1 TO MONTO.
           MOVE NUM-CO1 TO REG-CIM.
           MOVE MONTO1 TO REG-MPO.
           MOVE TIP-VEN TO REG-GM.
           MOVE FEC-PAG1 TO REG-FEC.
           WRITE REG-PAG.
           REWRITE REG-IMP.
           MOVE SPACES TO OPC.
           GO TO CON-D1.
       MODIF1.
           MOVE ZERO TO CAM.
           DISPLAY PANTALLA-MODI1.
           ACCEPT CAM AT 2337 WITH PROMPT.
           IF CAM < 1 OR CAM > 2 GO TO MODIF1.
           IF CAM = 1 PERFORM D8
           GO TO F-MODI1
           ELSE
           IF CAM = 2 PERFORM D81.
       F-MODI1.
           DISPLAY "                                        " AT 2303.
           MOVE ZERO TO CAM.
           GO TO RES2.
       BC.
           CLOSE CODI.
           OPEN I-O CODI.
       BC1.
           READ CODI NEXT RECORD AT END GO TO F-BC.
           IF N-CODI EQUAL TO TIP-VEN GO TO F-BC.
           GO TO BC1.
       F-BC.

       LISTADO.
           PERFORM ENCABEZA.
           CLOSE RPAGOS CONTRI CODI.
           OPEN I-O RPAGOS CONTRI RDATOS CODI.
           OPEN OUTPUT IMPRE.
       PREG.
           DISPLAY LP.
           MOVE SPACES TO OPC.
           ACCEPT OPC AT 1256 WITH PROMPT.
           IF OPC = "S" GO TO EMISION.
           IF OPC = "N" GO TO CIERRE.
           GO TO PREG.

       EMISION.
           MOVE ZEROS TO T-GM C-CONTA C-HOJA.
           READ RDATOS AT END.
           MOVE REG-NOMEMP TO S-NOMEMP.
           MOVE REG-CIUD TO S-CIUD.
           ACCEPT FECHA FROM DATE.
           MOVE DDF TO S-DD.
           MOVE MMF TO S-MM.
           MOVE AAF TO S-AA.

       C-EMI.
           MOVE ZEROS TO C-REG.
           ADD 1 TO C-CONTA C-HOJA.
           MOVE C-HOJA TO S-HOJA.
           IF C-CONTA = 1
           WRITE RENGLON FROM L1 AFTER 1
           ELSE
           WRITE RENGLON FROM L1 AFTER PAGE.
           WRITE RENGLON FROM L3 AFTER 1.
           WRITE RENGLON FROM L4 AFTER 1.
           WRITE RENGLON FROM L5 AFTER 1.
           WRITE RENGLON FROM L1 AFTER 1.
           WRITE RENGLON FROM L6 AFTER 2.
           WRITE RENGLON FROM L7 AFTER 1.

       LEE-R.
           READ RPAGOS NEXT RECORD AT END GO TO LEE-F.
           IF REG-FEC = 000000 GO TO LEE-R.
           COMPUTE T-GM = T-GM + REG-MPO.
           MOVE REG-GM TO N-CODI.
           READ CODI KEY IS N-CODI
           INVALID KEY
           DISPLAY  "CODIGO INCORRECTO" AT 1410
           WITH FOREGROUND-COLOR 6
           MOVE SPACES TO OPC
           ACCEPT OPC AT 1435 WITH PROMPT
           DISPLAY "                         "  AT 1410
           GO TO CIERRE.
           MOVE REG-CIM TO NUM-CO.
           READ CONTRI KEY IS NUM-CO
           INVALID KEY
           DISPLAY "CLIENTE INCORRECTO" AT 1410
           WITH FOREGROUND-COLOR 6
           MOVE SPACES TO OPC
           ACCEPT OPC AT 1435 WITH PROMPT
           DISPLAY "                       " AT 1410
           GO TO CIERRE.
           MOVE NOMBRE TO S-NOM.
           MOVE UBICA TO S-UBI.
           CLOSE CODI CONTRI.
           OPEN I-O CODI CONTRI.
           MOVE REG-CIM TO S-RNC.
           MOVE REG-MPO TO S-IMP.
           MOVE B-CODI TO S-RCO.
           MOVE RFD TO RF1.
           MOVE RFM TO RF2.
           MOVE RFA TO RF3.
           IF C-REG= 51 GO TO C-EMI.
           WRITE RENGLON FROM S-REG AFTER 2.
           WRITE RENGLON FROM S-REG1 AFTER 1.
           ADD 3 TO C-REG.
           GO TO LEE-R.

       LEE-F.
           MOVE T-GM TO MON-T.
           WRITE RENGLON FROM L8 AFTER 3.
           MOVE T-GM TO QTY.
           CALL "NUMLE" USING PARAM1, PARAM2.
           WRITE RENGLON FROM LETRA-1 AFTER 2.
           WRITE RENGLON FROM LETRA-2 AFTER 1.

       CIERRE.
           CLOSE CONTRI IMPU RPAGOS CODI IMPRE RDATOS.
           CHAIN "MENU".
           STOP RUN.
