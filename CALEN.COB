       IDENTIFICATION DIVISION.
       PROGRAM-ID.     CALEN.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT RDATOS ASSIGN TO DISK.

       DATA DIVISION.
       FILE SECTION.

       FD  RDATOS LABEL RECORD IS STANDARD
           VALUE OF FILE-ID "ARDATOS"
           DATA RECORD IS REG-DATOS.

       01  REG-DATOS.
           03 REG-NOMEMP          PIC X(40).
           03 REG-CIUD            PIC X(40).

       WORKING-STORAGE SECTION.
       01  FECHA.
           03 AA             PIC XX.
           03 MM             PIC XX.
           03 DD             PIC XX.

       01  FECHA1.
           02 DD1            PIC XX.
           02 F              PIC X VALUE "/".
           02 MM1            PIC XX.
           02 F              PIC X VALUE "/".
           02 AA1            PIC XX.

       01  LINEAS.
           03 L-1            PIC X(78) VALUE ALL "�".
           03 L-2            PIC X VALUE "�".

       LINKAGE SECTION.

       01  A                 PIC 9.

       SCREEN SECTION.

       01  L-PANTALLA.
           03 BLANK SCREEN.

       01  RECUADRO.
           03 LINE 2 COLUMN 1 FOREGROUND-COLOR 2  VALUE "�".
           03 LINE 2 COLUMN 2 FOREGROUND-COLOR 2  PIC X(78) USING L-1.
           03 LINE 2 COLUMN 80 FOREGROUND-COLOR 2  VALUE "�".
           03 LINE 3 COLUMN 1 FOREGROUND-COLOR 2 PIC X USING L-2.
           03 LINE 3 COLUMN 80 FOREGROUND-COLOR 2 PIC X USING L-2.
           03 LINE 4 COLUMN 1 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 4 COLUMN 80 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 5 COLUMN 1 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 5 COLUMN 80 FOREGROUND-COLOR 2   PIC X USING L-2.
           03 LINE 6 COLUMN 1 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 6 COLUMN 80 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 7 COLUMN 1 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 7 COLUMN 80 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 8 COLUMN 1 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 8 COLUMN 80 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 9 COLUMN 1 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 9 COLUMN 80 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 10 COLUMN 1 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 10 COLUMN 80 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 11 COLUMN 1 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 11 COLUMN 80 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 12 COLUMN 1 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 12 COLUMN 80 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 13 COLUMN 1 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 13 COLUMN 80 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 14 COLUMN 1 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 14 COLUMN 80 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 15 COLUMN 1 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 15 COLUMN 80 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 16 COLUMN 1 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 16 COLUMN 80 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 17 COLUMN 1 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 17 COLUMN 80 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 18 COLUMN 1 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 18 COLUMN 80 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 19 COLUMN 1 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 19 COLUMN 80 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 20 COLUMN 1 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 20 COLUMN 80 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 21 COLUMN 1 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 21 COLUMN 80 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 22 COLUMN 1 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 22 COLUMN 80 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 23 COLUMN 1 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 23 COLUMN 80 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 24 COLUMN 1 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 24 COLUMN 80 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 25 COLUMN 1 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 25 COLUMN 80 FOREGROUND-COLOR 2  PIC X USING L-2.
           03 LINE 25 COLUMN 1 FOREGROUND-COLOR 2  VALUE "�".
           03 LINE 25 COLUMN 2 FOREGROUND-COLOR 2  PIC X(78) USING L-1.
           03 LINE 25 COLUMN 80 FOREGROUND-COLOR 2  VALUE "�".

       01  TITULOS.
           03 LINE 3 COLUMN 3 FOREGROUND-COLOR 3
              VALUE "Sistema de Vencimientos".
           03 LINE 4 COLUMN 3 FOREGROUND-COLOR 3
              PIC X(40) USING REG-NOMEMP.
           03 LINE 5 COLUMN 3 FOREGROUND-COLOR 3
              PIC X(40) USING REG-CIUD.
           03 LINE 4 COLUMN 68 FOREGROUND-COLOR 3
              PIC XX USING DD1.
           03 LINE 4 COLUMN 70 FOREGROUND-COLOR 3
              VALUE "/".
           03 LINE 4 COLUMN 71 FOREGROUND-COLOR 3
              PIC XX USING MM1.
           03 LINE 4 COLUMN 73 FOREGROUND-COLOR 3
              VALUE "/".
           03 LINE 4 COLUMN 74 FOREGROUND-COLOR 3
              PIC XX USING AA1.

       PROCEDURE DIVISION USING A.
       ABRIR.
           OPEN INPUT RDATOS.
           READ RDATOS AT END.
       ENCABEZA.
           DISPLAY L-PANTALLA.
           DISPLAY RECUADRO.
           ACCEPT FECHA FROM DATE.
           MOVE DD TO DD1.
           MOVE MM TO MM1.
           MOVE AA TO AA1.
           DISPLAY TITULOS.
           CLOSE RDATOS.
           EXIT PROGRAM.
