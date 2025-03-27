       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MENU.   
      ******************************
      * SISTEMA DE VENCIMIENTOS    *
      * AUTOR: JOSE LUIS PLANES    *
      ******************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT SBACKUP ASSIGN TO DISK.

           SELECT NBACKUP ASSIGN TO DISK.

           SELECT RESTAURA ASSIGN TO DISK.
 
           SELECT RECUPERA ASSIGN TO DISK.
         
           SELECT DEPU ASSIGN TO DISK.

       DATA DIVISION.
       FILE SECTION.

       FD  SBACKUP LABEL RECORD IS STANDARD
           VALUE OF FILE-ID "SBACKUP"
           DATA RECORD IS R-SBA.

       01  R-SBA           PIC 9.

       FD  NBACKUP LABEL RECORD IS STANDARD
           VALUE OF FILE-ID "NBACKUP"
           DATA RECORD IS R-NBA.

       01  R-NBA              PIC 9.

       FD  RESTAURA LABEL RECORD IS STANDARD
           VALUE OF FILE-ID "RESTA"
           DATA RECORD IS REG-RES.

       01  REG-RES               PIC 9.

       FD  RECUPERA LABEL RECORD IS STANDARD
           VALUE OF FILE-ID "RECU"
           DATA RECORD IS REG-REC.

       01  REG-REC               PIC 9.

       FD  DEPU LABEL RECORD IS STANDARD
           VALUE OF FILE-ID "DEPU"
           DATA RECORD IS R-DEPU.

       01  R-DEPU                PIC 9.

       WORKING-STORAGE SECTION.
       77  OPC                      PIC 99 VALUE ZEROS.
       77  B                        PIC 9 VALUE ZERO.
       77  GUION                    PIC X(77) VALUE ALL "-".

       SCREEN SECTION.

       01  L-PANTALLA.
           03 BLANK SCREEN.

       01  L-GUION.
           03 LINE 6 COLUMN 3 FOREGROUND-COLOR 3 PIC X(77)
              USING GUION.

       01  PANTALLA-MENU.
           03 LINE 8 COLUMN 25 FOREGROUND-COLOR 4 REVERSE-VIDEO
              VALUE "    MENU PRINCIPAL          ".
           03 LINE 10 COLUMN 25
              VALUE "  1-CARGA DEL SISTEMA    ".
           03 LINE 11 COLUMN 25
              VALUE "  2-VENCIMIENTOS         ".
           03 LINE 12 COLUMN 25
              VALUE "  3-PRORROGA DE VENCIMIENTOS".
           03 LINE 13 COLUMN 25
              VALUE "  4-PAGOS                ".
           03 LINE 14 COLUMN 25
              VALUE "  5-CONSULTAS            ".
           03 LINE 15 COLUMN 25
              VALUE "  6-MODIFICACIONES / BAJAS".
           03 LINE 16 COLUMN 25
              VALUE "  7-DEPURACION ARCH-MAE   ".
           03 LINE 17 COLUMN 25
              VALUE "  8-RESTAURACION          ".
           03 LINE 18 COLUMN 25
              VALUE "  9-RECUPERACION          ".
           03 LINE 19 COLUMN 25
              VALUE "  10-SALIDA DEL SISTEMA   ".
           03 LINE 21 COLUMN 25 FOREGROUND-COLOR 4 REVERSE-VIDEO
              VALUE "    INGRESE OPCION          ".

       01  PANTALLA-ADVER.
           03 LINE 14 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
              VALUE "REALICE EL BACKUP TODOS LOS DIAS,".
           03 LINE 16 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
              VALUE "ASI EVITARA MALOS MOMENTOS POR LA".
           03 LINE 18 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
              VALUE "PERDIDA DE LOS DATOS.".

       PROCEDURE DIVISION.
       INICIO.
           CALL "CALEN" USING B.
           DISPLAY L-GUION.
           DISPLAY PANTALLA-MENU.

       PREG.
           MOVE ZEROS TO OPC.
           ACCEPT (21, 55) OPC WITH PROMPT.
           IF OPC = 1 CHAIN "CARGA".
           IF OPC = 2 CHAIN "VENCI".
           IF OPC = 3 CHAIN "PROVEN".
           IF OPC = 4 CHAIN "PAGOS".
           IF OPC = 5 CHAIN "CONSUL".
           IF OPC = 6 CHAIN "MOFBA".
           IF OPC = 7 GO TO DEPUR.
           IF OPC = 8 GO TO RESTAR.
           IF OPC = 9 GO TO RECUPE.
           IF OPC = 10 GO TO TER.
           GO TO PREG.

       RESTAR.
           OPEN OUTPUT RESTAURA.
           WRITE REG-RES.
           CLOSE RESTAURA.
           GO TO FIN.

       RECUPE.
           OPEN OUTPUT RECUPERA.
           WRITE REG-REC.
           CLOSE RECUPERA.
           GO TO FIN.

       TER.
           DISPLAY L-PANTALLA.
           CALL "CALEN" USING B.
           DISPLAY L-GUION.
           DISPLAY (10, 30) "SE REALIZA EL BACKUP".
           DISPLAY (12, 30) "1=SI            2=NO".
           DISPLAY PANTALLA-ADVER.
           MOVE ZERO TO OPC.
       FIPR.
           ACCEPT (12, 53) OPC WITH PROMPT.
           IF OPC = 1 GO TO SIBA.
           IF OPC = 2 GO TO NOBA.
           GO TO FIPR.

       SIBA.
           DISPLAY L-PANTALLA.
           OPEN OUTPUT SBACKUP.
           WRITE R-SBA.
           CLOSE SBACKUP.
           GO TO FIN.
       NOBA.
           DISPLAY L-PANTALLA.
           OPEN OUTPUT NBACKUP.
           WRITE R-NBA.
           CLOSE NBACKUP.
           GO TO FIN.

       DEPUR.
           DISPLAY L-PANTALLA.
           OPEN OUTPUT DEPU.
           WRITE R-DEPU.
           CLOSE DEPU.

       FIN.
           STOP RUN.
