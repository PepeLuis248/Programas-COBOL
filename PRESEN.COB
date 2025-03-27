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

       DATA DIVISION.
       FILE SECTION.

       FD  SBACKUP LABEL RECORD IS STANDARD
           VALUE OF FILE-ID "SBACKUP"
           DATA RECORD IS R-SBA.

       01  R-SBA           PIC 9.


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

       01  PANTALLA-PRE.
           03 LINE 8 COLUMN 3 FOREGROUND-COLOR 6 HIGHLIGHT
              VALUE "  JOSE LUIS PLANES ".
           03 LINE 10 COLUMN 3 FOREGROUND-COLOR 6 HIGHLIGHT
              VALUE "    ZUVIRIA 1121   ".
           03 LINE 12 COLUMN 3 FOREGROUND-COLOR 6 HIGHLIGHT
              VALUE "     Bo JUNIORS    ".
           03 LINE 14 COLUMN 3 FOREGROUND-COLOR 6 HIGHLIGHT
              VALUE "      CORDOBA      ".
           03 LINE 16 COLUMN 3 FOREGROUND-COLOR 6 HIGHLIGHT
              VALUE "   REP. ARGENTINA  ".
           03 LINE 18 COLUMN 3 FOREGROUND-COLOR 6 HIGHLIGHT
              VALUE "      C.P. 5000    ".
           03 LINE 20 COLUMN 3 FOREGROUND-COLOR 6 HIGHLIGHT
              VALUE "  TE:(051) 64-3618 ".
           03 LINE 08 COLUMN 30 FOREGROUND-COLOR 4 HIGHLIGHT
              VALUE "SISTEMA DE VENCIMIENTOS es una base de datos por".
           03 LINE 10 COLUMN 30 FOREGROUND-COLOR 4 HIGHLIGHT
              VALUE "la cual se pueden llevar todo tipo de vencimien-".
           03 LINE 12 COLUMN 30 FOREGROUND-COLOR 4 HIGHLIGHT
              VALUE "tos, como por ejemplo impuestos,tasas,servicios ".
           03 LINE 14 COLUMN 30 FOREGROUND-COLOR 4 HIGHLIGHT
              VALUE "(agua,luz,etc),o cualquier otra forma de pagos. ".
           03 LINE 16 COLUMN 30 FOREGROUND-COLOR 4 HIGHLIGHT
`              VALUE "En el manual se describe la forma de uso y ejem-".
           03 LINE 18 COLUMN 30 FOREGROUND-COLOR 4 HIGHLIGHT
              VALUE "plos (MANUAL.DOC - Realizado en Windows version ".
           03 LINE 20 COLUMN 30 FOREGROUND-COLOR 4 HIGHLIGHT
              VALUE "3.1. Para registrarse ver el manual).Esta es una".
           03 LINE 22 COLUMN 30 FOREGROUND-COLOR 4 HIGHLIGHT
              VALUE "version SHAREWARE                               ".


       PROCEDURE DIVISION.
       INICIO.
           CALL "CALEN" USING B.
           DISPLAY L-GUION.
           DISPLAY PANTALLA-PRE.
           MOVE ZEROS TO OPC.
           ACCEPT (24, 75) OPC WITH PROMPT.
           GO TO FIN.
       PREG.
           OPEN OUTPUT SBACKUP.
           WRITE R-SBA.
           CLOSE SBACKUP.
           GO TO FIN.

       FIN.
           STOP RUN.
