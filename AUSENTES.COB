       IDENTIFICATION DIVISION.
       PROGRAM-ID.    AUSENTES.
       AUTHOR.        JOSE LUIS PLANES.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT AREMPLE ASSIGN TO DISK
                  ORGANIZATION IS INDEXED
                  ACCESS IS DYNAMIC
                  RECORD KEY IS LEGAJO.


           SELECT ARMOVIM ASSIGN TO DISK
                  ORGANIZATION IS INDEXED
                  ACCESS IS DYNAMIC
                  RECORD KEY IS LEGAJO1
                  ALTERNATE RECORD KEY IS APE-NOM1
                  WITH DUPLICATES.

           SELECT ARCODI ASSIGN TO DISK
                  ORGANIZATION IS INDEXED
                  ACCESS IS DYNAMIC
                  RECORD KEY IS L-CODI.

           SELECT ARFECPRO ASSIGN TO DISK.

           SELECT IMPRE ASSIGN TO PRINTER.

           SELECT LC97 ASSIGN TO DISK
                  ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  AREMPLE LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID "AREMPLE"
                   DATA RECORD IS REG-EMP.

       01  REG-EMP.
           03 LEGAJO              PIC 9(05).
           03 DIG-VER.
              05 DV1              PIC 9.
              05 DV2              PIC 9.
           03 APE-NOM             PIC X(30).

       FD  ARMOVIM LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID "ARMOVIM"
                   DATA RECORD IS REG-MOV.

       01  REG-MOV.
           03 LEGAJO1             PIC 9(05).
           03 DIG-VER1.
              05 DV11             PIC 9.
              05 DV22             PIC 9.
           03 APE-NOM1            PIC X(30).
           03 D1                  PIC XX.
           03 D2                  PIC XX.
           03 D3                  PIC XX.
           03 CO.
              05 D3-L             PIC XX.
              05 D3-C             PIC X(15).
              05 D3-FI.
                 07 FI5           PIC XX.
                 07 FI6           PIC XX.
              05 D3-FF.
                 07 FF5           PIC XX.
                 07 FF6           PIC XX.
           03 D4                  PIC XX.
           03 CO1.
              05 D4-L             PIC XX.
              05 D4-C             PIC X(15).
              05 D4-FI.
                 07 FI7           PIC XX.
                 07 FI8           PIC XX.
              05 D4-FF.
                 07 FF7           PIC XX.
                 07 FF8           PIC XX.
           03 D5                  PIC XX.
           03 CO2.
              05 D5-L             PIC XX.
              05 D5-C             PIC X(15).
              05 D5-FI.
                 07 FI9           PIC XX.
                 07 FI10          PIC XX.
              05 D5-FF.
                 07 FF9           PIC XX.
                 07 FF10          PIC XX.
           03 D6                  PIC XX.
           03 CO3.
              05 D6-L             PIC XX.
              05 D6-C             PIC X(15).
              05 D6-FI.
                 07 FI11          PIC XX.
                 07 FI12          PIC XX.
              05 D6-FF.
                 07 FF11          PIC XX.
                 07 FF12          PIC XX.
           03 D7                  PIC XX.
           03 CO4.
              05 D7-L             PIC XX.
              05 D7-C             PIC X(15).
              05 D7-FI.
                 07 FI13          PIC XX.
                 07 FI14          PIC XX.
              05 D7-FF.
                 07 FF13          PIC XX.
                 07 FF14          PIC XX.
           03 OBS-M               PIC X(40).

       FD  ARFECPRO LABEL RECORD IS STANDARD
                    VALUE OF FILE-ID "ARFECPRO"
                    DATA RECORD IS REG-FEC.

       01  REG-FEC.
           03 FECHA.
              05 DDP              PIC XX.
              05 MMP              PIC XX.
              05 AAP              PIC XX.
           03 S1.
              05 S11              PIC XX.
              05 S12              PIC XX.
           03 S2.
              05 S21              PIC XX.
              05 S22              PIC XX.

       FD  ARCODI LABEL RECORD IS STANDARD
                  VALUE OF FILE-ID "ARCODI"
                  DATA RECORD IS REG-CODI.

       01  REG-CODI.
           03 L-CODI              PIC XX.
           03 C-CODI              PIC X(15).

       FD  IMPRE LABEL RECORD IS OMITTED.

       01  RENGLON                PIC X(133).

       FD  LC97 LABEL RECORD IS STANDARD
                VALUE OF FILE-ID "LC97.DAT"
                DATA RECORD IS CLAVE.

       01 CLAVE.
           03 SECRE               PIC X(08).
           03 BAN                 PIC X(01).
           03 LGAJO.
              06 ALF              PIC A(01).
              06 NUM              PIC 9(05).

       WORKING-STORAGE SECTION.

       01  NUMERICOS.
           03 CON-REG             PIC 99 VALUE ZEROS.
           03 CON-HOJ             PIC 999 VALUE ZEROS.
           03 SAL-H               PIC 9  VALUE ZERO.
           03 LEGAJO2             PIC 9(05) VALUE ZEROS.
           03 CON-IMP             PIC 9  VALUE ZERO.
           03 I                   PIC 9  VALUE ZERO.
           03 BAND                PIC 9  VALUE ZERO.
           03 FC1                 PIC 99 VALUE ZEROS.
           03 FE1                 PIC 99 VALUE ZEROS.
           03 FC                  PIC 99 VALUE ZEROS.
           03 FE                  PIC 99 VALUE ZEROS.
           03 RF                  PIC 99 VALUE ZEROS.
           03 RF1                 PIC 99 VALUE ZEROS.
           03 RF2                 PIC 99 VALUE ZEROS.

       01  ALFANUM.
           03 OPC1                PIC X VALUE "N". 
           03 OPC                 PIC XX VALUE SPACES.
           03 DD                  PIC 99.
           03 DD-FI.
              05 DD-FI1           PIC 99.
              05 DD-FI2           PIC 99.
           03 DD-FF.
              05 DD-FF1           PIC 99.
              05 DD-FF2           PIC 99.

       01  REG-IMP.
           03 LEG-S               PIC Z(05).
           03 F                   PIC X VALUE "/".
           03 DV1-S               PIC X.
           03 F                   PIC X VALUE "/".
           03 DV2-S               PIC X.
           03 F                   PIC X(04) VALUE SPACES.
           03 APE-NOM-S           PIC X(30) VALUE SPACES.
           03 F                   PIC X(05) VALUE SPACES.
           03 D1-S                PIC XX    VALUE SPACES.
           03 F                   PIC X(04) VALUE SPACES.
           03 D2-S                PIC XX    VALUE SPACES.
           03 F                   PIC X(04) VALUE SPACES.
           03 D3-S                PIC XX    VALUE SPACES.
           03 F                   PIC X(04) VALUE SPACES.
           03 D4-S                PIC XX    VALUE SPACES.
           03 F                   PIC X(04) VALUE SPACES.
           03 D5-S                PIC XX    VALUE SPACES.
           03 F                   PIC X(04) VALUE SPACES.
           03 D6-S                PIC XX    VALUE SPACES.
           03 F                   PIC X(04) VALUE SPACES.
           03 D7-S                PIC XX    VALUE SPACES.
           03 F                   PIC X(06) VALUE SPACES.
           03 OBS-M-S             PIC X(40) VALUE SPACES.

       01  REG-I. 
           03 L1-S                PIC XX.
           03 F-G                 PIC X.
           03 C1-S                PIC X(15).
           03 F                   PIC XX    VALUE SPACES.
           03 DD-S                PIC XX.
           03 F-B                 PIC X.
           03 MM-S                PIC XX.
           03 FA                  PIC X(03).
           03 DD1-S               PIC XX.
           03 F-B1                PIC X.
           03 MM1-S               PIC XX.

       01  REG-I1.                                           
           03 L2-S                PIC XX.
           03 F-G1                PIC X.
           03 C2-S                PIC X(15).
           03 F                   PIC XX    VALUE SPACES.
           03 DD2-S               PIC XX.
           03 F-B2                PIC X.
           03 MM2-S               PIC XX.
           03 FA1                 PIC X(03).
           03 DD3-S               PIC XX.
           03 F-B3                PIC X.
           03 MM3-S               PIC XX.

       01  REG-I2.                                           
           03 L3-S                PIC XX.
           03 F-G2                PIC X.
           03 C3-S                PIC X(15).
           03 F                   PIC XX    VALUE SPACES.
           03 DD4-S               PIC XX.
           03 F-B4                PIC X.
           03 MM4-S               PIC XX.
           03 FA2                 PIC X(03).
           03 DD5-S               PIC XX.
           03 F-B5                PIC X.
           03 MM5-S               PIC XX.

       01  REG-I3. 
           03 L4-S                PIC XX.
           03 F-G3                PIC X.
           03 C4-S                PIC X(15).
           03 F                   PIC XX    VALUE SPACES.
           03 DD6-S               PIC XX.
           03 F-B6                PIC X.
           03 MM6-S               PIC XX.
           03 FA3                 PIC X(03).
           03 DD7-S               PIC XX.
           03 F-B7                PIC X.
           03 MM7-S               PIC XX.

       01  REG-I4.                                           
           03 L5-S                PIC XX.
           03 F-G4                PIC X.
           03 C5-S                PIC X(15).
           03 F                   PIC XX    VALUE SPACES.
           03 DD8-S               PIC XX.
           03 F-B8                PIC X.
           03 MM8-S               PIC XX.
           03 FA4                 PIC X(03).
           03 DD9-S               PIC XX.
           03 F-B9                PIC X.
           03 MM9-S               PIC XX.

       01  VEC-IMP.
           03 VECTOR OCCURS 5 TIMES.
              05 DV-L             PIC XX.
              05 DV-C             PIC X(15).
              05 DV-FI.
                 07 FVI           PIC XX.
                 07 FVI1          PIC XX.
              05 DV-FF.
                 07 FFV           PIC XX.
                 07 FFV1          PIC XX.

       01 VEC-IMPS.
          05 DV-LS                PIC XX.
          05 DV-CS                PIC X(15).
          05 DV-FIS.
             07 FVIS              PIC XX.
             07 FVI1S             PIC XX.
          05 DV-FFS.
             07 FFVS              PIC XX.
             07 FFV1S             PIC XX.

       01  REG-IMP1.
           03 F                   PIC X(45) VALUE SPACES.
           03 LEG-S1              PIC Z(05).
           03 F                   PIC X     VALUE "/".
           03 DV1-S1              PIC X.
           03 F                   PIC X     VALUE "/".
           03 DV2-S1              PIC X.
           03 F                   PIC X(10) VALUE SPACES.
           03 APE-NOM-S1          PIC X(30) VALUE SPACES.

       01  REG-IMP2.
           03 F                   PIC X(52) VALUE SPACES.
           03 COD-S               PIC XX.
           03 F                   PIC X(09) VALUE SPACES.
           03 CON-S               PIC X(15).

       01  R-CODI.
           03 LS-CODI             PIC XX.
           03 F                   PIC XX.
           03 CS-CODI             PIC X(15).

       01  T.
           03 F                   PIC X(133) VALUE ALL "*".

       01  T1.
           03 F                   PIC X      VALUE "*".
           03 F                   PIC X(131) VALUE SPACES.
           03 F                   PIC X      VALUE "*".

       01  T2.
           03 F                   PIC X(08)  VALUE "* BANCO ".
           03 F                   PIC X(12)  VALUE "HIPOTECARIO ".
           03 F                   PIC X(08)  VALUE "NACIONAL".
           03 F                   PIC X(35)  VALUE SPACES.
           03 F                   PIC X(11)  VALUE "DELEGACION ".
           03 F                   PIC X(07)  VALUE "CORDOBA".
           03 F                   PIC X(35)  VALUE SPACES.
           03 F                   PIC X(07)  VALUE "FECHA: ".
           03 DD4                 PIC XX.
           03 F                   PIC X      VALUE "/".
           03 MM4                 PIC XX.
           03 F                   PIC X      VALUE "/".
           03 AA4                 PIC XX.
           03 F                   PIC XX     VALUE " *".

       01  T15.
           03 F                   PIC X(52)  VALUE SPACES.
           03 F                   PIC X(10)  VALUE "LISTADO DE".
           03 F                   PIC X(08)  VALUE " AGENTES".

       01  T16.
           03 F                   PIC X(52)  VALUE SPACES.
           03 F                   PIC X(10)  VALUE "------- --".
           03 F                   PIC X(08)  VALUE " -------".

       01  T151.
           03 F                   PIC X(52)  VALUE SPACES.
           03 F                   PIC X(10)  VALUE "LISTADO DE".
           03 F                   PIC X(08)  VALUE " CODIGOS".

       01  T161.
           03 F                   PIC X(52)  VALUE SPACES.
           03 F                   PIC X(10)  VALUE "------- --".
           03 F                   PIC X(08)  VALUE " -------".

       01  T3.
           03 F  PIC X(21) VALUE "* PARTE DE AUSENCIAS ".
           03 F  PIC X(20) VALUE "E IMPUNTUALIDADES - ".
           03 F  PIC X(24) VALUE "SEMANA COMPRENDIDA ENTRE".
           03 F  PIC X(11) VALUE " EL SABADO ".
           03 S11-S PIC 99.
           03 F  PIC X     VALUE "/".
           03 S12-S PIC 99.
           03 F  PIC X(14) VALUE " Y EL VIERNES ".
           03 S21-S PIC 99.
           03 F  PIC X     VALUE "/".
           03 S22-S PIC 99.
           03 F  PIC X(04) VALUE " DE ".
           03 F  PIC X(02) VALUE "19".
           03 AAP-S PIC 99.
           03 F  PIC X(21) VALUE "            HOJA NRO ".
           03 HOJ-S PIC ZZ.
           03 F  PIC XX    VALUE " *".

       01  T31.
           03 F  PIC XX    VALUE "* ".
           03 F  PIC X(15) VALUE "NRO DE LEGAJO: ".
           03 LGAJO-S  PIC ZZZZZZ.
           03 F  PIC X(109) VALUE SPACES.
           03 F  PIC X VALUE "*".

       01  T11.
           03 F  PIC X(02) VALUE SPACES.
           03 F  PIC X(06) VALUE "LEGAJO".
           03 F  PIC X(05) VALUE SPACES.
           03 F  PIC X(18) VALUE "APELLIDO Y NOMBRES".
           03 F  PIC X(16) VALUE SPACES.
           03 F  PIC X(16) VALUE "SAB.  DOM.  LUN.".
           03 F  PIC X(18) VALUE "  MART  MIER  JUEV".
           03 F  PIC X(16) VALUE "  VIER       OBS".
           03 F  PIC X(17) VALUE "ERVACIONES       ".

       01  T12.
           03 F  PIC X(02) VALUE SPACES.
           03 F  PIC X(06) VALUE "------".
           03 F  PIC X(05) VALUE SPACES.
           03 F  PIC X(18) VALUE "------------------".
           03 F  PIC X(16) VALUE SPACES.
           03 F  PIC X(16) VALUE "----  ----  ----".
           03 F  PIC X(18) VALUE "  ----  ----  ----".
           03 F  PIC X(16) VALUE "  ----       ---".
           03 F  PIC X(17) VALUE "----------       ".

       01  T17.
           03 F  PIC X(45) VALUE SPACES.
           03 F  PIC X(07) VALUE "LEGAJOS".
           03 F  PIC X(12) VALUE SPACES.
           03 F  PIC X(18) VALUE "APELLIDO Y NOMBRES".

       01  T18.
           03 F  PIC X(45) VALUE SPACES.
           03 F  PIC X(07) VALUE "-------".
           03 F  PIC X(12) VALUE SPACES.
           03 F  PIC X(18) VALUE "------------------".

       01  T19.
           03 F  PIC X(20) VALUE SPACES.
           03 F  PIC X(13) VALUE "-------------".
           03 F  PIC X(30) VALUE SPACES.
           03 F  PIC X(18) VALUE "------------------".

       01  T22.
           03 F  PIC X(132) VALUE ALL "-".

       01  T21.
           03 F  PIC X(21) VALUE SPACES.
           03 F  PIC X(11) VALUE "CONFECCIONO".
           03 F  PIC X(32) VALUE SPACES.
           03 F  PIC X(16) VALUE "FIRMA AUTORIZADA".

       01  MCLAVE.
           03 ITECLA           PIC X OCCURS 8.
       01  CAMPOS              PIC 9999 COMP-0.
       01  POSCAM REDEFINES CAMPOS.
           03 FILLER           PIC X.
           03 SEG              PIC X.
       01  AREPAT.
           03 FILLER           PIC 9999 COMP VALUE 112.
           03 FILLER           PIC 9999 COMP VALUE 102.
           03 FILLER           PIC 9999 COMP VALUE 115.
           03 FILLER           PIC 9999 COMP VALUE 055.
           03 FILLER           PIC 9999 COMP VALUE 109.
           03 FILLER           PIC 9999 COMP VALUE 042.
           03 FILLER           PIC 9999 COMP VALUE 075.
           03 FILLER           PIC 9999 COMP VALUE 091.
       01  TABPAT REDEFINES AREPAT.
           03 PATERN PIC 9999 COMP OCCURS 8.
       01  SUB                 PIC 99 VALUE 0.
       01  SUPOPE              PIC X(08).
       01  SUPNUO              PIC X(08).
       01  SENSUP              PIC 9 VALUE 0.

       01  LINEAS.
           03 L-1              PIC X(53) VALUE ALL "Í".
           03 L-2              PIC X     VALUE "º".

       SCREEN SECTION.

       01  LIM-P.
           03 BLANK SCREEN.

       01  TITU.
           03 LINE 2 COLUMN 23 FOREGROUND-COLOR 1 HIGHLIGHT
              VALUE "BANCO HIPOTECARIO NACIONAL".
           03 LINE 3 COLUMN 18 FOREGROUND-COLOR 1 HIGHLIGHT
              VALUE "PARTE DE AUSENCIAS E IMPUNTUALIDADES".

       01  BM-MENU.
           03 LINE 6 COLUMN 19 FOREGROUND-COLOR 4 REVERSE-VIDEO VALUE
              "               BAJAS              ".
           03 LINE 8 COLUMN 15 VALUE
              "            1-ARCH. EMPLEADOS      ".
           03 LINE 10 COLUMN 15 VALUE
              "            2-MOVIMIENTOS          ".
           03 LINE 12 COLUMN 15 VALUE
              "            3-CODIGOS               ".
           03 LINE 14 COLUMN 15 VALUE
              "            4-REGRESO MENU PRINCIPAL".
           03 LINE 16 COLUMN 19 FOREGROUND-COLOR 4 REVERSE-VIDEO VALUE
              "        INGRESE OPCION             ".

       01  AL-MENU.
           03 LINE 6 COLUMN 19 FOREGROUND-COLOR 4 REVERSE-VIDEO VALUE
              "              ALTAS               ".  
           03 LINE 8 COLUMN 15 VALUE
              "            1-ARCH. EMPLEADOS      ".
           03 LINE 10 COLUMN 15 VALUE
              "            2-CODIGOS              ".
           03 LINE 12 COLUMN 15 VALUE
              "            3-REGRESO MENU PRINCIPAL".
           03 LINE 14 COLUMN 19 FOREGROUND-COLOR 4 REVERSE-VIDEO VALUE
              "        INGRESE OPCION            ".  

       01  T-MENU.
           03 LINE 6 COLUMN 19 FOREGROUND-COLOR 4 REVERSE-VIDEO VALUE
              "             MENU                 ".
           03 LINE 8 COLUMN 19 VALUE
              "            1-ALTAS               ".
           03 LINE 10 COLUMN 19 VALUE
              "            2-PROCESO             ".
           03 LINE 12 COLUMN 19 VALUE
              "            3-BAJA/MODIF.         ".
           03 LINE 14 COLUMN 19 VALUE
              "            4-SALIDA              ".
           03 LINE 16 COLUMN 19 FOREGROUND-COLOR 4 REVERSE-VIDEO VALUE
              "            INGRESE OPCION        ".

       01  ING-FEC.
           03 LINE 5 COLUMN 1 FOREGROUND-COLOR 2 VALUE
              "1-FECHA DEL PROCESO....:".
           03 LINE 6 COLUMN 1 FOREGROUND-COLOR 2 VALUE
              "2-SEMANA COMPRENDIDA...:".
           03 LINE 6 COLUMN 33 FOREGROUND-COLOR 2 VALUE
              "3-Y EL: ".

       01  ING-DAT.
           03 LINE 5 COLUMN 1 FOREGROUND-COLOR 2 VALUE
              "NUMERO DE LEGAJO.......:".
           03 LINE 6 COLUMN 1 FOREGROUND-COLOR 2 VALUE
              "APELLIDO Y NOMBRE......:".
           03 LINE 7 COLUMN 1 FOREGROUND-COLOR 2 VALUE
              "1-SABADO....:".
           03 LINE 7 COLUMN 20 FOREGROUND-COLOR 2 VALUE
              "2-DOMINGO...:".
           03 LINE 7 COLUMN 39 FOREGROUND-COLOR 2 VALUE
              "3-LUNES.....:".                  
           03 LINE 8 COLUMN 1 FOREGROUND-COLOR 2 VALUE
              "4-MARTES....:".
           03 LINE 8 COLUMN 20 FOREGROUND-COLOR 2 VALUE
              "5-MIERCOLES.:".
           03 LINE 8 COLUMN 39 FOREGROUND-COLOR 2 VALUE
              "6-JUEVES....:".
           03 LINE 9 COLUMN 1 FOREGROUND-COLOR 2 VALUE
              "7-VIERNES...:".
           03 LINE 9 COLUMN 20 FOREGROUND-COLOR 2 VALUE
              "FEC-INICIO..:".
           03 LINE 9 COLUMN 40 FOREGROUND-COLOR 2 VALUE
              "FEC-FINALIZA:".
           03 LINE 10 COLUMN 1 FOREGROUND-COLOR 2 VALUE
              "8-OBSERVACIONES........:".
           03 LINE 11 COLUMN 1 FOREGROUND-COLOR 2 VALUE
              "F1= 9-".
           03 LINE 11 COLUMN 14 FOREGROUND-COLOR 2 VALUE
              "10-".
           03 LINE 11 COLUMN 24 FOREGROUND-COLOR 2 VALUE
              "F2=11-".
           03 LINE 11 COLUMN 37 FOREGROUND-COLOR 2 VALUE
              "12-".
           03 LINE 11 COLUMN 48 FOREGROUND-COLOR 2 VALUE
              "F3=13-".
           03 LINE 11 COLUMN 60 FOREGROUND-COLOR 2 VALUE
              "14-".
           03 LINE 12 COLUMN 1  FOREGROUND-COLOR 2 VALUE
              "F4=15-".
           03 LINE 12 COLUMN 14 FOREGROUND-COLOR 2 VALUE
              "16-".
           03 LINE 12 COLUMN 24 FOREGROUND-COLOR 2 VALUE
              "F5=17-".
           03 LINE 12 COLUMN 37 FOREGROUND-COLOR 2 VALUE
              "18-".

       01  PREGUNTA.
           03 LINE 14 COLUMN 1 FOREGROUND-COLOR 4 REVERSE-VIDEO VALUE
              "1=GRABA  2=ANULA  3=MODIFICA ".

       01  SALIDA.
           03 LINE 14 COLUMN 40 FOREGROUND-COLOR 4          
           REVERSE-VIDEO VALUE
              "     PARA SALIR TIPEAR  9    ".

       01  PE-FEC.
           03 LINE 8 COLUMN 10 FOREGROUND-COLOR 3 VALUE
              "1-INCORPORA FECHA DEL PROCESO  2-MODIFICA".

       01  PE-FEC1.
           03 LINE 10 COLUMN 10 FOREGROUND-COLOR 3 VALUE
              "      PARA CONTINUAR DAR ENTER          ".

       01  MODIFICA.
           03 LINE 13 COLUMN 10 FOREGROUND-COLOR 5 VALUE
              "INGRESE NRO DE CAMPO A MODIFICAR:".

       01  PREGUNTA1.
           03 LINE 14 COLUMN 10 FOREGROUND-COLOR 4           
           REVERSE-VIDEO VALUE
              "1=GRABA  2=BAJA   3=MODIFICA  4=SALIDA ".

       01  PREGUNTA3.
           03 LINE 14 COLUMN 10 FOREGROUND-COLOR 4          
           REVERSE-VIDEO VALUE
              "1=GRABA  2=BAJA   3=MODIFICA  4=SALIDA ".

       01  PANTALLA-BORRAS.
           03 LINE 13 COLUMN  10 VALUE
              "                                                 ".
       01  PANTALLA-BORRA.
           03 LINE 5 COLUMN  26 VALUE
              "                                       ".
           03 LINE 6 COLUMN  26 VALUE
              "                                       ".
           03 LINE 7 COLUMN  16 VALUE
              "  ".
           03 LINE 7 COLUMN  35 VALUE
              "  ".
           03 LINE 7 COLUMN  55 VALUE
              "  ".
           03 LINE 8 COLUMN  16 VALUE
              "  ".
           03 LINE 8 COLUMN  35 VALUE
              "  ".
           03 LINE 8 COLUMN  55 VALUE
              "  ".
           03 LINE 9 COLUMN  16 VALUE
              "  ".
           03 LINE 9 COLUMN  34 VALUE
              "     ".
           03 LINE 9 COLUMN  56 VALUE
              "     ".
           03 LINE 10 COLUMN 26 VALUE
              "                               ".
           03 LINE 11 COLUMN 7  VALUE
              "     ".
           03 LINE 11 COLUMN 17 VALUE
              "     ".
           03 LINE 11 COLUMN 30 VALUE
              "     ".
           03 LINE 11 COLUMN 40 VALUE
              "     ".
           03 LINE 11 COLUMN 54 VALUE
              "     ".
           03 LINE 11 COLUMN 64 VALUE
              "     ".
           03 LINE 12 COLUMN 7  VALUE
              "     ".
           03 LINE 12 COLUMN 17 VALUE
              "     ".
           03 LINE 12 COLUMN 30 VALUE
              "     ".
           03 LINE 12 COLUMN 40 VALUE
              "     ".

       01  BORRA.
           03 LINE 14 COLUMN 32 VALUE
              "  ".

       01  ING-AGE.
           03 LINE 08 COLUMN 10 FOREGROUND-COLOR 2 VALUE 
              "1-NUMERO DE LEGAJO............:".
           03 LINE 10 COLUMN 10 FOREGROUND-COLOR 2 VALUE
              "2-DIGITOS VERIFICADORES.......:".
           03 LINE 12 COLUMN 10 FOREGROUND-COLOR 2 VALUE
              "3-APELLIDOS Y NOMBRES.........:".

       01  ING-COD.
           03 LINE 8 COLUMN 10 FOREGROUND-COLOR 2 VALUE
              "1-CODIGO......................:".
           03 LINE 10 COLUMN 10 FOREGROUND-COLOR 2 VALUE
              "2-CONCEPTO....................:".

       01  PREGUNTA2.
           03 LINE 14 COLUMN 10 FOREGROUND-COLOR 4           
           REVERSE-VIDEO VALUE
              " 1=GRABA  2=ANULA  3=MODIFICA  ".

       01  MODIFICA1.
           03 LINE 15 COLUMN 10 FOREGROUND-COLOR 5 VALUE
              "INGRESE NRO DE CAMPO A MODIFICAR".

       01  MODIFICA11.
           03 LINE 13 COLUMN 10 FOREGROUND-COLOR 5 VALUE
              "INGRESE NRO DE CAMPO A MODIFICAR".

       01  PRO-F.
           03 LINE 9 COLUMN 10 FOREGROUND-COLOR 5 VALUE
              "INGRESE NRO DE CAMPO A MODIFICAR:".

       01  SALIDA1.
           03 LINE 16 COLUMN 10 FOREGROUND-COLOR 4 REVERSE-VIDEO VALUE
              "     PARA SALIR TIPEAR   9     ".

       01  PANTALLA-BORRA1.
           03 LINE 08 COLUMN 42 VALUE
              "                                          ".
           03 LINE 10 COLUMN 42 VALUE
              "                                          ".
           03 LINE 12 COLUMN 42 VALUE
              "                                          ".
           03 LINE 14 COLUMN 47 VALUE
              "       ".

       01  PANTALLA-BORRA33.
           03 LINE 08 COLUMN 42 VALUE
              "                                          ".
           03 LINE 10 COLUMN 42 VALUE
              "                                          ".
           03 LINE 12 COLUMN 42 VALUE
              "                                          ".
           03 LINE 14 COLUMN 32 VALUE
              "       ".

       01 PANTALLA-B.
           03 LINE 08 COLUMN 42 VALUE
              "                                  ".
           03 LINE 10 COLUMN 42 VALUE
              "                                  ".

       01  PANTALLA-BORRA11.
           03 LINE 15 COLUMN 10 VALUE
              "                                                 ".
       01  PANT-I.
           03 LINE 15 COLUMN 20 FOREGROUND-COLOR 4 HIGHLIGHT VALUE
              "REALIZA EL LISTADO S/N".

       01  PANT-H.
           03 LINE 10 COLUMN 10 FOREGROUND-COLOR 6 VALUE
           "EXISTEN DATOS GRABADOS  1=BORRA DATOS   2=INCORPORA DATOS".

       01  RECUADRO.
           03 LINE 08 COLUMN 15 FOREGROUND-COLOR 2 VALUE "É".
           03 LINE 08 COLUMN 16 FOREGROUND-COLOR 2 PIC X(53) USING L-1.
           03 LINE 08 COLUMN 69 FOREGROUND-COLOR 2 VALUE "»".
           03 LINE 09 COLUMN 15 FOREGROUND-COLOR 2 PIC X USING L-2.
           03 LINE 09 COLUMN 69 FOREGROUND-COLOR 2 PIC X USING L-2.
           03 LINE 10 COLUMN 15 FOREGROUND-COLOR 2 PIC X USING L-2.
           03 LINE 10 COLUMN 69 FOREGROUND-COLOR 2 PIC X USING L-2.
           03 LINE 11 COLUMN 15 FOREGROUND-COLOR 2 PIC X USING L-2.
           03 LINE 11 COLUMN 69 FOREGROUND-COLOR 2 PIC X USING L-2.
           03 LINE 12 COLUMN 15 FOREGROUND-COLOR 2 PIC X USING L-2.
           03 LINE 12 COLUMN 69 FOREGROUND-COLOR 2 PIC X USING L-2.
           03 LINE 13 COLUMN 15 FOREGROUND-COLOR 2 PIC X USING L-2.
           03 LINE 13 COLUMN 69 FOREGROUND-COLOR 2 PIC X USING L-2.
           03 LINE 14 COLUMN 15 FOREGROUND-COLOR 2 PIC X USING L-2.
           03 LINE 14 COLUMN 69 FOREGROUND-COLOR 2 PIC X USING L-2.
           03 LINE 15 COLUMN 15 FOREGROUND-COLOR 2 PIC X USING L-2.
           03 LINE 15 COLUMN 69 FOREGROUND-COLOR 2 PIC X USING L-2.
           03 LINE 16 COLUMN 15 FOREGROUND-COLOR 2 PIC X USING L-2.
           03 LINE 16 COLUMN 69 FOREGROUND-COLOR 2 PIC X USING L-2.
           03 LINE 17 COLUMN 15 FOREGROUND-COLOR 2 PIC X USING L-2.
           03 LINE 17 COLUMN 69 FOREGROUND-COLOR 2 PIC X USING L-2.
           03 LINE 18 COLUMN 15 FOREGROUND-COLOR 2 VALUE "È".
           03 LINE 18 COLUMN 16 FOREGROUND-COLOR 2 PIC X(53) USING L-1.
           03 LINE 18 COLUMN 69 FOREGROUND-COLOR 2 VALUE "¼".

       01  ING-CLA.
           03 LINE 10 COLUMN 16 FOREGROUND-COLOR 3
              VALUE "CLAVE....................:".

       01  PREG.
           03 LINE 12 COLUMN 16 FOREGROUND-COLOR 3
              VALUE "INGRESA NUEVA CLAVE (S/N):".

       01  ING-NUCLA.
           03 LINE 14 COLUMN 16 FOREGROUND-COLOR 3
              VALUE "NUEVA CLAVE..............:".

       01  ING-LEG.
           03 LINE 16 COLUMN 16 FOREGROUND-COLOR 3
              VALUE "NRO DE LEGAJO............:".

       01  PASOPE.
           03 LINE 10 COLUMN 43 FOREGROUND-COLOR 6
              PIC X(08) TO SUPOPE SECURE BELL FULL.

       01  PASNUO.
           03 LINE 14 COLUMN 43 FOREGROUND-COLOR 6
              PIC X(08) TO SUPNUO SECURE BELL FULL.

       01  DISERR.
           03 LINE 10 COLUMN 52 FOREGROUND-COLOR 5 HIGHLIGHT
              VALUE "PERMISO DENEGADO" BELL REVERSE-VIDEO.

       PROCEDURE DIVISION.
       ABRIR.
           OPEN I-O LC97.
           READ LC97 AT END.
       CLOPE.
           DISPLAY RECUADRO.
           DISPLAY ING-CLA.
           DISPLAY PREG.
           DISPLAY ING-NUCLA.
           DISPLAY ING-LEG.
           ACCEPT PASOPE.
           MOVE 0 TO SENSUP.
           MOVE SUPOPE TO MCLAVE.
           MOVE 0 TO SUB.
       ACLOPE.
           ADD 1 TO SUB.
           IF SUB > 8 GO TO CLFINAL.
           MOVE ITECLA(SUB) TO SEG.
           ADD PATERN(SUB) TO CAMPOS.
           MOVE SEG TO ITECLA(SUB).
           GO TO ACLOPE.
       CLFINAL. EXIT.
       FINOPE.
           IF MCLAVE NOT = SECRE DISPLAY DISERR GO TO CONOPE.
           MOVE MCLAVE TO SECRE.
           DISPLAY (12, 43) OPC1.
           ACCEPT (12, 43) OPC1.
           IF OPC1 = "N" GO TO LEGA.
           IF OPC1 = " " GO TO LEGA.
           ACCEPT PASNUO.
           MOVE SUPNUO TO MCLAVE.
           MOVE 0 TO SUB.
           PERFORM ACLOPE THRU CLFINAL.
           MOVE MCLAVE TO SECRE.
       LEGA.
           ACCEPT (16, 43) ALF WITH PROMPT.
           IF ALF = "  " GO TO LEGA.
       LEGA1.
           ACCEPT (16, 45) NUM WITH PROMPT.
           IF NUM = ZEROS GO TO LEGA1.
           REWRITE CLAVE. 
           GO TO MENU.
       CONOPE.
           REWRITE CLAVE.
           CLOSE LC97.
           STOP RUN.

       MENU.
           DISPLAY LIM-P.
           DISPLAY TITU.
           DISPLAY T-MENU.
           ACCEPT (16, 56) OPC WITH PROMPT.
           IF OPC = "1" PERFORM ALTAS THRU FIN.
           IF OPC = "2" PERFORM PROCESO THRU FIN-1.
           IF OPC = "3" GO TO BAMOD.
           IF OPC = "4" GO TO CIERRE.
           GO TO MENU.

      *********************SUBRUTINA DE ALTAS******************
       ALTAS.
           OPEN I-O AREMPLE ARCODI.
       ALTAS-B.
           DISPLAY LIM-P.
           DISPLAY TITU.
           DISPLAY AL-MENU.
           ACCEPT (14, 56) OPC WITH PROMPT.
           IF OPC = "1" GO TO A11.
           IF OPC = "2" GO TO A22.
           IF OPC = "3" GO TO A33.
           GO TO ALTAS-B.
       A11.
           DISPLAY LIM-P.
           DISPLAY TITU.
           DISPLAY ING-AGE.
           DISPLAY PREGUNTA2.
           DISPLAY SALIDA1.
       AL-D.
           DISPLAY PANTALLA-BORRA1.
       AL-D1.
           ACCEPT (8, 42) LEGAJO WITH PROMPT.
           READ AREMPLE KEY IS LEGAJO
           INVALID KEY GO TO AL-D11.
           DISPLAY (8, 52) "LEGAJO INCORPORADO"
           ACCEPT (8, 74) OPC WITH PROMPT
           DISPLAY (8, 52) "                  "
           GO TO AL-D1.
       AL-D11.
           IF LEGAJO = 9 GO TO AL-I.
       AL-D2.
           ACCEPT (10, 42) DIG-VER WITH PROMPT.
       AL-D3.
           ACCEPT (12, 42) APE-NOM WITH PROMPT.
       AL-P.
           DISPLAY PANTALLA-BORRA11.
           ACCEPT (14, 42) OPC WITH PROMPT.
           IF OPC = "1" GO TO AL-G.
           IF OPC = "2" GO TO AL-D.
           IF OPC = "3" GO TO AL-M.
           GO TO AL-P.
       AL-G.
           WRITE REG-EMP.
           GO TO AL-D.
       AL-M.
           DISPLAY MODIFICA1.
           ACCEPT (15, 44) OPC WITH PROMPT.
           IF OPC = "1" PERFORM AL-D1
           ELSE
           IF OPC = "2" PERFORM AL-D2
           ELSE
           IF OPC = "3" PERFORM AL-D3.
           GO TO AL-P.
       AL-I.
           DISPLAY LIM-P.
           DISPLAY PANT-I.
       AL-I1.
           ACCEPT (15, 44) OPC WITH PROMPT.
           IF OPC = "S" GO TO AL-I2.
           IF OPC = "N" GO TO ALTAS-B.
           GO TO AL-I1.
       AL-I2.
           CLOSE AREMPLE.
           OPEN I-O AREMPLE.
           OPEN OUTPUT IMPRE.
           MOVE ZERO TO SAL-H.
       AL-I21.
           MOVE ZEROS TO CON-REG.
           IF SAL-H = 0
           WRITE RENGLON FROM T15 AFTER 2 LINE
           ELSE
           WRITE RENGLON FROM T15 AFTER PAGE.
           WRITE RENGLON FROM T16 AFTER 1 LINE.
           WRITE RENGLON FROM T17 AFTER 2 LINE.
           WRITE RENGLON FROM T18 AFTER 1 LINE.
           MOVE 1 TO SAL-H.
       AL-I211.
           ADD 1 TO CON-REG.
           READ AREMPLE NEXT RECORD AT END GO TO AL-F.
           MOVE LEGAJO TO LEG-S1.
           MOVE DV1 TO DV1-S1.
           MOVE DV2 TO DV2-S1.
           MOVE APE-NOM TO APE-NOM-S1.
           WRITE RENGLON FROM REG-IMP1 AFTER 1 LINE.
           IF CON-REG = 57 GO TO AL-I21.
           GO TO AL-I211.
       AL-F.
           CLOSE IMPRE.
           GO TO ALTAS-B.
       A22.
           DISPLAY LIM-P.
           DISPLAY TITU.
           DISPLAY ING-COD.
           DISPLAY PREGUNTA.
           DISPLAY SALIDA.
       AC-D.
           DISPLAY PANTALLA-BORRA33.
       AC-D1.
           ACCEPT (8, 42) L-CODI WITH PROMPT.
           IF L-CODI = "    " GO TO AC-D1.
           READ ARCODI KEY IS L-CODI
           INVALID KEY GO TO AC-D11.
           DISPLAY (8, 52) "CODIGO INCORPORADO"
           ACCEPT (8, 74) OPC WITH PROMPT
           DISPLAY (8, 52) "                  "
           GO TO AC-D1.
       AC-D11.
           IF L-CODI = 9 GO TO AC-I.
       AC-D2.
           ACCEPT (10, 42) C-CODI WITH PROMPT.
       AC-P.
           DISPLAY PANTALLA-BORRAS.
           MOVE SPACES TO OPC.
           ACCEPT (14, 32) OPC WITH PROMPT.
           IF OPC = "1" GO TO AC-G.
           IF OPC = "2" GO TO AC-D.
           IF OPC = "3" GO TO AC-M.
           GO TO AC-P.
       AC-G.
           WRITE REG-CODI.
           GO TO AC-D.
       AC-M.
           DISPLAY MODIFICA11.
           MOVE SPACES TO OPC.
           ACCEPT (13 , 48) OPC WITH PROMPT.
           IF OPC = "1" PERFORM AC-D1
           ELSE
           IF OPC = "2" PERFORM AC-D2.
       AC-MC.
           MOVE SPACES TO OPC.
           GO TO AC-P.
       AC-I.
           DISPLAY LIM-P.
           DISPLAY PANT-I.
       AC-I1.
           ACCEPT (15, 44) OPC WITH PROMPT.
           IF OPC = "S" GO TO C-I22.
           IF OPC = "N" GO TO ALTAS-B.
           GO TO AC-I1.
       C-I22.
           CLOSE ARCODI.
           OPEN I-O ARCODI.
           OPEN OUTPUT IMPRE.
           MOVE ZEROS TO SAL-H.
       AC-I21.
           MOVE ZEROS TO CON-REG.
           IF SAL-H = 0
           WRITE RENGLON FROM T151 AFTER 2 LINE
           ELSE
           WRITE RENGLON FROM T151 AFTER PAGE.
           WRITE RENGLON FROM T161 AFTER 1 LINE.
           MOVE 1 TO SAL-H.
       AC-I211.
           ADD 1 TO CON-REG.
           READ ARCODI NEXT RECORD AT END GO TO AC-F.
           MOVE L-CODI TO COD-S.
           MOVE C-CODI TO CON-S.
           WRITE RENGLON FROM REG-IMP2 AFTER 1 LINE.
           IF CON-REG = 57 GO TO AC-I21.
           GO TO AC-I211.
       AC-F.
           CLOSE IMPRE.
           GO TO ALTAS-B.
       A33.
           CLOSE AREMPLE ARCODI.
           MOVE SPACES TO OPC.
       FIN.
      *****************SUBRUTINA DE PROCESO****************
       PROCESO.
           OPEN I-O AREMPLE ARFECPRO ARMOVIM ARCODI.
       PROCESO1.
           READ ARFECPRO.
           DISPLAY LIM-P.
           DISPLAY TITU.
           DISPLAY ING-FEC.
       I-FEC.
           DISPLAY (5, 26) DDP.
           DISPLAY (5, 28) "/".
           DISPLAY (5, 29) MMP.
           DISPLAY (5, 31) "/".
           DISPLAY (5, 32) AAP.
           DISPLAY (6, 26) S11.
           DISPLAY (6, 28) "/".
           DISPLAY (6, 29) S12.
           DISPLAY (6, 41) S21.
           DISPLAY (6, 43) "/".
           DISPLAY (6, 44) S22.
           DISPLAY PE-FEC.
           DISPLAY PE-FEC1.
       H-FEC.
           DISPLAY (9, 10) "                                   ".
           DISPLAY (9, 59) "   ".
           MOVE SPACES TO OPC.
           ACCEPT (8, 60) OPC WITH PROMPT.
           IF OPC = "1" GO TO F-PRO.
           IF OPC = "2" GO TO F-MOD.
           IF OPC = " " GO TO ID.
           GO TO H-FEC.
       F-PRO.
           DISPLAY (5, 26) "            ".
           ACCEPT (5, 26) FECHA WITH PROMPT.
           DISPLAY (5, 26) DDP.
           DISPLAY (5, 28) "/".
           DISPLAY (5, 29) MMP.
           DISPLAY (5, 31) "/".
           DISPLAY (5, 32) AAP.
           IF FECHA = "0" GO TO F-PRO.
           IF DDP < "01" OR DDP > "31"
           DISPLAY (5, 35) "ERROR EN EL DIA"
           ACCEPT (5, 51) OPC WITH PROMPT
           DISPLAY (5, 35) "                "
           GO TO F-PRO.
           IF MMP < "01" OR MMP > "12"
           DISPLAY (5, 35) "ERROR EN EL MES"
           ACCEPT (5, 51) OPC WITH PROMPT
           DISPLAY (5, 35) "                "
           GO TO F-PRO.
       F-PRO1.
           DISPLAY (6, 26) "     ".
           ACCEPT (6, 26) S1 WITH PROMPT.
           IF S1 = SPACES GO TO F-PRO1.
           IF S1 = "0" GO TO F-PRO1.
           DISPLAY (6, 26) S11.
           DISPLAY (6, 28) "/".
           DISPLAY (6, 29) S12.
           IF S1 = 0 GO TO F-PRO1.
           IF S11 < "01" OR S11 > "31"
           DISPLAY (7, 26) "ERROR EN EL DIA"
           ACCEPT (7, 51) OPC WITH PROMPT
           DISPLAY (7, 26) "                "
           GO TO F-PRO1.
           IF S12 < "01" OR S12 > "12"
           DISPLAY (7, 26) "ERROR EN EL MES"
           ACCEPT (7, 51) OPC WITH PROMPT
           DISPLAY (7, 26) "                 "
           GO TO F-PRO1.
       F-PRO2.
           DISPLAY (6, 41) "     ".
           ACCEPT (6, 41) S2 WITH PROMPT.
           IF S2 = SPACES GO TO F-PRO2.
           IF S2 = "0" GO TO F-PRO2.
           DISPLAY (6, 41) S21.
           DISPLAY (6, 43) "/".
           DISPLAY (6, 44) S22.
           IF S2 = 0 GO TO F-PRO2.
           IF S21 < "01" OR S21 > "31"
           DISPLAY (7, 41) "ERROR EN EL DIA"
           ACCEPT (7, 57) OPC WITH PROMPT
           DISPLAY (7, 41) "                "
           GO TO F-PRO2.
           IF S22 < "01" OR S22 > "12"
           DISPLAY (7, 41) "ERROR EN EL MES"
           ACCEPT (7, 57) OPC WITH PROMPT
           DISPLAY (7, 41) "                 "
           GO TO F-PRO2.
       F-PROS.
           GO TO H-FEC.
       F-MOD.
           DISPLAY PRO-F.
           MOVE SPACES TO OPC.
           ACCEPT (9, 60) OPC WITH PROMPT.
           IF OPC = "1" PERFORM F-PRO
           ELSE
           IF OPC = "2" PERFORM F-PRO1
           ELSE
           IF OPC = "3" PERFORM F-PRO2.
           GO TO H-FEC.
       ID.
           REWRITE REG-FEC.
           DISPLAY LIM-P.
           DISPLAY PANT-H.
       A.
           ACCEPT (10, 70) OPC WITH PROMPT.
           IF OPC = "1" GO TO B.
           IF OPC = "2" GO TO CONTINUA.
           GO TO A.
       B.  CLOSE ARMOVIM.
           OPEN OUTPUT ARMOVIM.
           MOVE "A" TO APE-NOM1.
           WRITE REG-MOV.
           CLOSE ARMOVIM.
           OPEN I-O ARMOVIM.
       CONTINUA.
           DISPLAY LIM-P.
           DISPLAY TITU.
           DISPLAY ING-DAT.
           DISPLAY PREGUNTA.
           DISPLAY SALIDA.
           MOVE 1 TO COL.
       D-CODI.
           MOVE 15 TO LIN.
       D-CODI1.
           ADD 1 TO LIN.
           READ ARCODI NEXT RECORD AT END GO TO D-CODI2.
           MOVE L-CODI TO LS-CODI.
           MOVE C-CODI TO CS-CODI.
           DISPLAY (LIN, COL) R-CODI.
           IF LIN > 23 ADD 20 TO COL GO TO D-CODI.
           GO TO D-CODI1.
       D-CODI2.
           CLOSE ARCODI.
           OPEN I-O ARCODI.
       I-DATOS.
           DISPLAY PANTALLA-BORRA.
           DISPLAY BORRA.
           ACCEPT (5, 26) LEGAJO WITH PROMPT.
           IF LEGAJO = 9 GO TO PR-I.
           READ AREMPLE KEY IS LEGAJO
           INVALID KEY
           DISPLAY (5, 32) "LEGAJO NO ENCONTRADO (ENTER)"
           ACCEPT (5, 63) OPC WITH PROMPT
           DISPLAY (5, 32) "                            "
           GO TO I-DATOS.
       PR4.
           MOVE LEGAJO TO LEGAJO1.
           READ ARMOVIM KEY IS LEGAJO1
           INVALID KEY GO TO PR41.
           DISPLAY (5, 32) "AGENTE YA INCORPORADO"
           ACCEPT (5, 57) OPC WITH PROMPT
           DISPLAY (5, 32) "                     "
           GO TO I-DATOS.
       PR41.
           DISPLAY (6, 26) APE-NOM.
       PR5.
           ACCEPT (7, 16) D1 WITH PROMPT.
           IF D1 = SPACES MOVE "-" TO D1 GO TO PR6.
           MOVE D1 TO DD.
           PERFORM Z THRU F-Z.
           IF BAND = 1 GO TO PR5.
       PR6.
           ACCEPT (7, 35) D2 WITH PROMPT.
           IF D2 = SPACES MOVE "-" TO D2 GO TO PR7.
           MOVE D2 TO DD.
           PERFORM Z THRU F-Z.
           IF BAND = 1 GO TO PR6.
       PR7.
           ACCEPT (7, 55) D3 WITH PROMPT.
           IF D3 = "  " MOVE "-" TO D3
           MOVE SPACES TO D3-L D3-C D3-FI D3-FF
           GO TO PR8.
           MOVE D3 TO DD.
           PERFORM Z THRU F-Z.
           IF BAND = 1 GO TO PR7.
           MOVE L-CODI TO D3-L.
           MOVE C-CODI TO D3-C.
       PF.
           PERFORM Z1 THRU F-Z1.
           MOVE DD-FI TO D3-FI.
           DISPLAY (11, 7) FI5.
           DISPLAY (11, 9) "/".
           DISPLAY (11, 10) FI6.
       PF1.
           PERFORM Z2 THRU F-Z2.
           MOVE DD-FF TO D3-FF.
           DISPLAY (11, 17) FF5.
           DISPLAY (11, 19) "/".
           DISPLAY (11, 20) FF6.
       CALC1.
           MOVE 0 TO BAND.
           MOVE FI5 TO FC.
           MOVE FF5 TO FE.
           MOVE FI6 TO FC1.
           MOVE FF6 TO FE1.
           PERFORM CALC THRU F-CALC.
           IF BAND = 2 GO TO PF.
           IF RF = 1 MOVE D3 TO D4
           DISPLAY (08, 16) D4
           MOVE SPACES TO D4-L D4-C D4-FI D4-FF
           GO TO PR9.
           IF RF = 2 MOVE D3 TO D4 D5
           DISPLAY (08, 16) D4
           DISPLAY (08, 35) D5
           MOVE SPACES TO D4-L D4-C D4-FI D4-FF
                          D5-L D5-C D5-FI D5-FF
           GO TO PR10.
           IF RF = 3 MOVE D3 TO D4 D5 D6
           DISPLAY (08, 16) D4
           DISPLAY (08, 35) D5
           DISPLAY (08, 55) D6
           MOVE SPACES TO D4-L D4-C D4-FI D4-FF
                          D5-L D5-C D5-FI D5-FF
                          D6-L D6-C D6-FI D6-FF
           GO TO PR11.
           IF RF = 4 MOVE D3 TO D4 D5 D6 D7
           DISPLAY (08, 16) D4
           DISPLAY (08, 35) D5
           DISPLAY (08, 55) D6
           DISPLAY (09, 16) D7
           MOVE SPACES TO D4-L D4-C D4-FI D4-FF
                          D5-L D5-C D5-FI D5-FF
                          D6-L D6-C D6-FI D6-FF
                          D7-L D7-C D7-FI D7-FF
           GO TO PR12.

       PR8.
           ACCEPT (08, 16) D4 WITH PROMPT.
           IF D4 = "  " MOVE "-" TO D4
           MOVE SPACES TO D4-L D4-C D4-FI D4-FF
           GO TO PR9.
           MOVE D4 TO DD.
           PERFORM Z THRU F-Z.
           IF BAND = 1 GO TO PR8.
           MOVE L-CODI TO D4-L.
           MOVE C-CODI TO D4-C.
       PF2.
           PERFORM Z1 THRU F-Z1.
           MOVE DD-FI TO D4-FI.
           DISPLAY (11, 30) FI7.
           DISPLAY (11, 32) "/".
           DISPLAY (11, 33) FI8.
       PF3.
           PERFORM Z2 THRU F-Z2.
           MOVE DD-FF TO D4-FF.
           DISPLAY (11, 40) FF7.
           DISPLAY (11, 42) "/".
           DISPLAY (11, 43) FF8.
       CALC2.
           MOVE 0 TO BAND.
           MOVE FI7 TO FC.
           MOVE FF7 TO FE.
           MOVE FI8 TO FC1.
           MOVE FF8 TO FE1.
           PERFORM CALC THRU F-CALC.
           IF BAND = 2 GO TO PF2.
           IF RF = 1 MOVE D4 TO D5
           DISPLAY (08, 35) D5
           MOVE SPACES TO D5-L D5-C D5-FI D5-FF
           GO TO PR10.
           IF RF = 2 MOVE D4 TO D5 D6
           DISPLAY (08, 35) D5
           DISPLAY (08, 55) D6
           MOVE SPACES TO D5-L D5-C D5-FI D5-FF
                          D6-L D6-C D6-FI D6-FF
           GO TO PR11.
           IF RF = 3 MOVE D4 TO D5 D6 D7
           DISPLAY (08, 35) D5
           DISPLAY (08, 55) D6
           DISPLAY (09, 16) D7
           MOVE SPACES TO D5-L D5-C D5-FI D5-FF
                          D6-L D6-C D6-FI D6-FF
                          D7-L D7-C D7-FI D7-FF
           GO TO PR12.
       PR9.
           ACCEPT (08, 35) D5 WITH PROMPT.
           IF D5 = "  " MOVE "-" TO D5
           MOVE SPACES TO D5-L D5-C D5-FI D5-FF
           GO TO PR10.
           MOVE D5 TO DD.
           PERFORM Z THRU F-Z.
           IF BAND = 1 GO TO PR9.
           MOVE L-CODI TO D5-L.
           MOVE C-CODI TO D5-C.
       PF4.
           PERFORM Z1 THRU F-Z1.
           MOVE DD-FI TO D5-FI.
           DISPLAY (11, 54) FI9.
           DISPLAY (11, 56) "/".
           DISPLAY (11, 57) FI10.
       PF5.
           PERFORM Z2 THRU F-Z2.
           MOVE DD-FF TO D5-FF.
           DISPLAY (11, 64) FF9.
           DISPLAY (11, 66) "/".
           DISPLAY (11, 67) FF10.
       CALC3.
           MOVE 0 TO BAND.
           MOVE FI9 TO FC.
           MOVE FF9 TO FE.
           MOVE FI10 TO FC1.
           MOVE FF10 TO FE1.
           PERFORM CALC THRU F-CALC.
           IF BAND = 2 GO TO PF4.
           IF RF = 1 MOVE D5 TO D6
           DISPLAY (08, 55) D6
           MOVE SPACES TO D6-L D6-C D6-FI D6-FF
           GO TO PR11.
           IF RF = 2 MOVE D5 TO D6 D7
           DISPLAY (08, 55) D6
           DISPLAY (09, 16) D7
           MOVE SPACES TO D6-L D6-C D6-FI D6-FF
                          D7-L D7-C D7-FI D7-FF
           GO TO PR12.
       PR10.
           ACCEPT (08, 55) D6 WITH PROMPT.
           IF D6 = "  " MOVE "-" TO D6
           MOVE SPACES TO D6-L D6-C D6-FI D6-FF
           GO TO PR11.
           MOVE D6 TO DD.
           PERFORM Z THRU F-Z.
           IF BAND = 1 GO TO PR10.
           MOVE L-CODI TO D6-L.
           MOVE C-CODI TO D6-C.
       PF6.
           PERFORM Z1 THRU F-Z1.
           MOVE DD-FI TO D6-FI.
           DISPLAY (12, 7) FI11.
           DISPLAY (12, 9) "/".
           DISPLAY (12, 10) FI12.
       PF7.
           PERFORM Z2 THRU F-Z2.
           MOVE DD-FF TO D6-FF.
           DISPLAY (12, 17) FF11.
           DISPLAY (12, 19) "/".
           DISPLAY (12, 20) FF12.
       CALC4.
           MOVE 0 TO BAND.
           MOVE FI11 TO FC.
           MOVE FF11 TO FE.
           MOVE FI12 TO FC1.
           MOVE FF12 TO FE1.
           PERFORM CALC THRU F-CALC.
           IF BAND = 2 GO TO PF6.
           IF RF = 1 MOVE D6 TO D7
           DISPLAY (09, 16) D7
           MOVE SPACES TO D7-L D7-C D7-FI D7-FF
           GO TO PR12.
       PR11.
           ACCEPT (09, 16) D7 WITH PROMPT.
           IF D7 = "  " MOVE "-" TO D7
           MOVE SPACES TO D7-L D7-C D7-FI D7-FF
           GO TO PR12.
           IF D7 = D6
           MOVE SPACES TO D7-L D7-C D7-FI D7-FF
           GO TO PR12.
           IF BAND = 1 GO TO PR11.
           MOVE D7 TO DD.
       PF8.
           PERFORM Z THRU F-Z.
           MOVE L-CODI TO D7-L.
           MOVE C-CODI TO D7-C.
           PERFORM Z1 THRU F-Z1.
           MOVE DD-FI TO D7-FI.
           DISPLAY (12, 30) FI13.
           DISPLAY (12, 32) "/".
           DISPLAY (12, 33) FI14.
       PF9.
           PERFORM Z2 THRU F-Z2.
           MOVE DD-FF TO D7-FF.
           DISPLAY (12, 40) FF13.
           DISPLAY (12, 42) "/".
           DISPLAY (12, 43) FF14.
       PR12.
           ACCEPT (10, 26) OBS-M WITH PROMPT.
       PR-P.
           DISPLAY PANTALLA-BORRAS.
           ACCEPT (14, 32) OPC WITH PROMPT
           IF OPC = "1" GO TO PR-G.
           IF OPC = "2" GO TO I-DATOS.
           IF OPC = "3" GO TO PR-M.
           GO TO PR-P.
       PR-G.
           MOVE LEGAJO TO LEGAJO1.
           MOVE APE-NOM TO APE-NOM1.
           MOVE DIG-VER TO DIG-VER1.
           WRITE REG-MOV.
           GO TO I-DATOS.
       PR-M.
           DISPLAY MODIFICA.
           ACCEPT (13, 45) OPC WITH PROMPT.
           IF OPC = "1" PERFORM AR5 THRU F-AR5 
           ELSE
           IF OPC = "2" PERFORM AR6 THRU F-AR6 
           ELSE
           IF OPC = "3" PERFORM AR7 THRU F-AR7
                        PERFORM DR1 THRU F-DR1
                        PERFORM DR2 THRU F-DR2
                        PERFORM CR1 THRU F-CR1
           ELSE
           IF OPC = "4" PERFORM AR8 THRU F-AR8
                        PERFORM DR3 THRU F-DR3
                        PERFORM DR4 THRU F-DR4
                        PERFORM CR2 THRU F-CR2
           ELSE
           IF OPC = "5" PERFORM AR9 THRU F-AR9
                        PERFORM DR5 THRU F-DR5
                        PERFORM DR6 THRU F-DR6
                        PERFORM CR3 THRU F-CR3
           ELSE
           IF OPC = "6" PERFORM AR10 THRU F-AR10
                        PERFORM DR7  THRU F-DR7
                        PERFORM DR8  THRU F-DR8
                        PERFORM CR4  THRU F-CR4
           ELSE
           IF OPC = "7" PERFORM AR11 THRU F-AR11
                        PERFORM DR9  THRU F-DR9
                        PERFORM DR10 THRU F-DR10
           ELSE
           IF OPC = "8" PERFORM PR12
           ELSE
           IF OPC = "9" PERFORM DR1 THRU F-DR1
           ELSE
           IF OPC = "10" PERFORM DR2 THRU F-DR2
           ELSE
           IF OPC = "11" PERFORM DR3 THRU F-DR3
           ELSE
           IF OPC = "12" PERFORM DR4 THRU F-DR4
           ELSE
           IF OPC = "13" PERFORM DR5 THRU F-DR5
           ELSE
           IF OPC = "14" PERFORM DR6 THRU F-DR6
           ELSE
           IF OPC = "15" PERFORM DR7 THRU F-DR7
           ELSE
           IF OPC = "16" PERFORM DR8 THRU F-DR8
           ELSE
           IF OPC = "17" PERFORM DR9 THRU F-DR9
           ELSE
           IF OPC = "18" PERFORM DR10 THRU F-DR10.
           GO TO PR-P.
       PR-I.
           DISPLAY LIM-P.
           DISPLAY PANT-I.
       PR-I1.
           ACCEPT (15, 44) OPC WITH PROMPT.
           IF OPC = "S" MOVE 0 TO CON-IMP GO TO PR-I2.
           IF OPC = "N" CLOSE AREMPLE ARMOVIM ARFECPRO ARCODI.
           GO TO FIN-1.
       PR-I2.
           OPEN OUTPUT IMPRE.
           MOVE ZEROS TO CON-HOJ.
           READ ARFECPRO AT END.
           MOVE "A" TO APE-NOM1.
           START ARMOVIM KEY IS NOT LESS THAN APE-NOM1
           INVALID KEY DISPLAY (17, 20)
           "REGISTRO NO ENCONTRADO"
           CLOSE AREMPLE ARFECPRO IMPRE ARMOVIM
           GO TO FIN-1.
       PR-I21.
           MOVE 9  TO CON-REG.
           IF CON-HOJ = 0
           WRITE RENGLON FROM T AFTER 1 LINE
           ELSE
           WRITE RENGLON FROM T AFTER PAGE.
           MOVE DDP TO DD4.
           MOVE MMP TO MM4.
           MOVE AAP TO AA4 AAP-S.
           WRITE RENGLON FROM T2 AFTER 1 LINE.
           ADD 1 TO CON-HOJ.
           MOVE CON-HOJ TO HOJ-S.
           MOVE S11 TO S11-S.
           MOVE S12 TO S12-S.
           MOVE S21 TO S21-S.
           MOVE S22 TO S22-S.
           WRITE RENGLON FROM T3 AFTER 1 LINE.
           MOVE LGAJO TO LGAJO-S.
           WRITE RENGLON FROM T31 AFTER 1 LINE.
           WRITE RENGLON FROM T  AFTER 1 LINE.
           WRITE RENGLON FROM T11 AFTER 2 LINE.
           WRITE RENGLON FROM T12 AFTER 1 LINE.
       PR-I211.
           READ ARMOVIM NEXT RECORD AT END GO TO PR-F.
           IF APE-NOM1 = "A" GO TO PR-I211.
           MOVE LEGAJO1 TO LEG-S.
           MOVE DV11 TO DV1-S.
           MOVE DV22 TO DV2-S.
           MOVE APE-NOM1 TO APE-NOM-S.
           MOVE D1 TO D1-S.
           MOVE D2 TO D2-S.
           MOVE D3 TO D3-S.
           MOVE D4 TO D4-S.
           MOVE D5 TO D5-S.
           MOVE D6 TO D6-S.
           MOVE D7 TO D7-S.
           MOVE OBS-M TO OBS-M-S.
           WRITE RENGLON FROM REG-IMP AFTER 1 LINE.
           MOVE 1 TO I.
           MOVE SPACES TO VEC-IMP.
           IF D3-L = SPACES GO TO W.
           MOVE CO TO VECTOR(I).
           ADD 1 TO I.
       W.
           IF D4-L = SPACES GO TO W1.
           MOVE CO1 TO VECTOR(I).
           ADD 1 TO I.
       W1.
           IF D5-L = SPACES GO TO W2.
           MOVE CO2 TO VECTOR(I).
           ADD 1 TO I.
       W2.
           IF D6-L = SPACES GO TO W3.
           MOVE CO3 TO VECTOR(I).
           ADD 1 TO I.
       W3.
           IF D7-L = SPACES GO TO W4.
           MOVE CO4 TO VECTOR(I).
       W4.
           MOVE SPACES TO REG-I REG-I1 REG-I2 REG-I3 REG-I4. 
           IF VECTOR(1) = SPACES GO TO W5.
           MOVE VECTOR(1) TO VEC-IMPS.
           MOVE "=" TO F-G.
           MOVE " a " TO FA.
           MOVE "/" TO F-B F-B1.
           MOVE DV-LS TO L1-S.
           MOVE DV-CS TO C1-S.
           MOVE FVIS TO DD-S.
           MOVE FVI1S TO MM-S.
           MOVE FFVS TO DD1-S.
           MOVE FFV1S TO MM1-S.
           WRITE RENGLON FROM REG-I AFTER 1 LINE.
       W5.
           IF VECTOR(2) = SPACES GO TO W6.
           MOVE VECTOR(2) TO VEC-IMPS.
           MOVE "=" TO F-G1.
           MOVE " a " TO FA1.
           MOVE "/" TO F-B2 F-B3.
           MOVE DV-LS TO L2-S.
           MOVE DV-CS TO C2-S.
           MOVE FVIS TO DD2-S.
           MOVE FVI1S TO MM2-S.
           MOVE FFVS TO DD3-S.
           MOVE FFV1S TO MM3-S.
           WRITE RENGLON FROM REG-I1 AFTER 1 LINE.
       W6.
           IF VECTOR(3) = SPACES GO TO W7.
           MOVE VECTOR(3) TO VEC-IMPS.
           MOVE "=" TO F-G2.
           MOVE " a " TO FA2.
           MOVE "/" TO F-B4 F-B5.
           MOVE DV-LS TO L3-S.
           MOVE DV-CS TO C3-S.
           MOVE FVIS TO DD4-S.
           MOVE FVI1S TO MM4-S.
           MOVE FFVS TO DD5-S.
           MOVE FFV1S TO MM5-S.
           WRITE RENGLON FROM REG-I2 AFTER 1 LINE.
       W7.
           IF VECTOR(4) = SPACES GO TO W8.
           MOVE VECTOR(4) TO VEC-IMPS.
           MOVE "=" TO F-G3.
           MOVE " a " TO FA3.
           MOVE "/" TO F-B6 F-B7.
           MOVE DV-LS TO L4-S.
           MOVE DV-CS TO C4-S.
           MOVE FVIS TO DD6-S.
           MOVE FVI1S TO MM6-S.
           MOVE FFVS TO DD7-S.
           MOVE FFV1S TO MM7-S.
           WRITE RENGLON FROM REG-I3 AFTER 1 LINE.
       W8.
           IF VECTOR(5) = SPACES GO TO W9.
           MOVE VECTOR(5) TO VEC-IMPS.
           MOVE "=" TO F-G4.
           MOVE " a " TO FA4.
           MOVE "/" TO F-B8 F-B9.
           MOVE DV-LS TO L5-S.
           MOVE DV-CS TO C5-S.
           MOVE FVIS TO DD8-S.
           MOVE FVI1S TO MM8-S.
           MOVE FFVS TO DD9-S.
           MOVE FFV1S TO MM9-S.
           WRITE RENGLON FROM REG-I4 AFTER 1 LINE.
       W9. 
           WRITE RENGLON FROM T22 AFTER 1 LINE.
           IF CON-REG = 93 GO TO PR-I21.
           ADD 7 TO CON-REG.
           GO TO PR-I211.
       PR-F.
           WRITE RENGLON FROM T19 AFTER 8 LINE.
           WRITE RENGLON FROM T21 AFTER 1 LINE.
           CLOSE ARMOVIM IMPRE.
           ADD 1 TO CON-IMP.
           IF CON-IMP = 2 GO TO F-IMP.
           OPEN I-O ARMOVIM.
           GO TO PR-I2.
       F-IMP.
           CLOSE ARFECPRO AREMPLE ARCODI.
       FIN-1.

       Z.
           MOVE 0 TO BAND.
           CLOSE ARCODI.
           OPEN I-O ARCODI.
           MOVE DD TO L-CODI.
           READ ARCODI KEY IS L-CODI
           INVALID KEY
           DISPLAY (10, 33) "CODIGO INCORRECTO"
           ACCEPT (10, 52) OPC WITH PROMPT
           DISPLAY (10, 33) "                 "
           MOVE 1 TO BAND.
       F-Z.

       Z1.
           DISPLAY (9, 34) "     ".
           ACCEPT (9, 34) DD-FI WITH PROMPT.
           IF DD-FI = "      " GO TO F-Z1.
           IF DD-FI1 <  0  OR DD-FI1 >  31
           DISPLAY (10, 33) "ERROR EN EL DIA"
           ACCEPT (10, 49) OPC WITH PROMPT
           DISPLAY (10, 33) "               "
           GO TO Z1.
           IF DD-FI2 <  0  OR  DD-FI2 >  12
           DISPLAY (10, 33) "ERROR EN EL MES"
           ACCEPT (10, 49) OPC WITH PROMPT
           DISPLAY (10, 33) "               "
           GO TO Z1.
       F-Z1.

       Z2.
           DISPLAY (9, 56) "     ".
           ACCEPT (9, 56) DD-FF WITH PROMPT.
           IF DD-FF = "      " GO TO F-Z2.
           IF DD-FF1 <  0  OR DD-FF1 >  31
           DISPLAY (10, 33) "ERROR EN EL DIA"
           ACCEPT (10, 49) OPC WITH PROMPT
           DISPLAY (10, 33) "               "
           GO TO Z2.
           IF DD-FF2 <  0  OR DD-FF2 >  12
           DISPLAY (10, 33) "ERROR EN EL MES"
           ACCEPT (10, 49) OPC WITH PROMPT
           DISPLAY (10, 33) "               "
           GO TO Z2.
       F-Z2.

       CALC.
           IF FC = "00" OR FE = "00" OR FC1 = "00" OR FE1 = "00"
           MOVE 4 TO RF
           GO TO F-CALC.
           IF FC1 = FE1 GO TO IGUAL.
           IF FC1 < FE1 GO TO DIFERENCIA.
           IF FC1 > FE1 GO TO DIFERENCIA.
           GO TO CALC.
       IGUAL.
           COMPUTE RF = FE - FC.
       IGUAL1.
           IF RF = 0 GO TO F-CALC.
           IF RF = 1 GO TO F-CALC.
           IF RF = 2 GO TO F-CALC.
           IF RF = 3 GO TO F-CALC.
           IF RF = 4 GO TO F-CALC.
           IF RF > 4 MOVE 4 TO RF GO TO F-CALC.
       VUELVE1.
           DISPLAY (10, 34) "ERROR EN LA FECHA".
           ACCEPT (10, 52) OPC WITH PROMPT.
           DISPLAY (10, 34) "                  ".
           MOVE 2 TO BAND.
           GO TO F-CALC.
       DIFERENCIA.
           IF FC1 = 01 AND FE1 = 02 GO TO DIFERENCIA1.
           IF FC1 = 02 AND FE1 = 03 GO TO DIFERENCIA1.
           IF FC1 = 03 AND FE1 = 04 GO TO DIFERENCIA1.
           IF FC1 = 04 AND FE1 = 05 GO TO DIFERENCIA1.
           IF FC1 = 05 AND FE1 = 06 GO TO DIFERENCIA1.
           IF FC1 = 06 AND FE1 = 07 GO TO DIFERENCIA1.
           IF FC1 = 07 AND FE1 = 08 GO TO DIFERENCIA1.
           IF FC1 = 08 AND FE1 = 09 GO TO DIFERENCIA1.
           IF FC1 = 09 AND FE1 = 10 GO TO DIFERENCIA1.
           IF FC1 = 10 AND FE1 = 11 GO TO DIFERENCIA1.
           IF FC1 = 11 AND FE1 = 12 GO TO DIFERENCIA1.
           IF FC1 = 12 AND FE1 = 01 GO TO DIFERENCIA1.
           MOVE 4 TO RF GO TO F-CALC.
       DIFERENCIA1.
           IF FC1 = 02 GO TO BISIESTO.
           GO TO DIFERENCIA2.
       BISIESTO.
           DISPLAY (10, 34) "ES A#O BISIESTO S/N".
           ACCEPT (10, 56) OPC WITH PROMPT.
           DISPLAY (10, 34) "                    ".
           IF OPC = "S" MOVE 29 TO RF2 GO TO DIFERENCIA2.
           IF OPC = "N" MOVE 28 TO RF2 GO TO DIFERENCIA2.
           GO TO BISIESTO.
       DIFERENCIA2.
           IF FC1 = 01 MOVE 31 TO RF2 GO TO DIFERENCIA3.
           IF FC1 = 03 MOVE 31 TO RF2 GO TO DIFERENCIA3.
           IF FC1 = 04 MOVE 30 TO RF2 GO TO DIFERENCIA3.
           IF FC1 = 05 MOVE 31 TO RF2 GO TO DIFERENCIA3.
           IF FC1 = 06 MOVE 30 TO RF2 GO TO DIFERENCIA3.
           IF FC1 = 07 MOVE 31 TO RF2 GO TO DIFERENCIA3.
           IF FC1 = 08 MOVE 31 TO RF2 GO TO DIFERENCIA3.
           IF FC1 = 09 MOVE 30 TO RF2 GO TO DIFERENCIA3.
           IF FC1 = 10 MOVE 31 TO RF2 GO TO DIFERENCIA3.
           IF FC1 = 11 MOVE 30 TO RF2 GO TO DIFERENCIA3.
           IF FC1 = 12 MOVE 31 TO RF2.
       DIFERENCIA3.
           COMPUTE RF1 = RF2 - FC.
           COMPUTE RF = RF1 + FE.
           IF RF > 4 MOVE 4 TO RF GO TO F-CALC.
           GO TO IGUAL1.
       F-CALC.

       AR5.
           ACCEPT (7, 16) D1 WITH PROMPT.
           IF D1 = SPACES MOVE "-" TO D1 GO TO F-AR5.
           MOVE D1 TO DD.
           PERFORM Z THRU F-Z.
           IF BAND = 1 GO TO AR5.
       F-AR5.

       AR6.
           ACCEPT (7, 35) D2 WITH PROMPT.
           IF D2 = SPACES MOVE "-" TO D2 GO TO F-AR6.
           MOVE D2 TO DD.
           PERFORM Z THRU F-Z.
           IF BAND = 1 GO TO AR6.
       F-AR6.
 
       AR7.
           ACCEPT (7, 55) D3 WITH PROMPT.
           IF D3 = "  " MOVE "-" TO D3
           MOVE SPACES TO D3-L D3-C D3-FI D3-FF
           DISPLAY (11, 7) FI5
           DISPLAY (11, 9) "/"
           DISPLAY (11, 10) FI6
           DISPLAY (11, 17) FF5
           DISPLAY (11, 19) "/"
           DISPLAY (11, 20) FF6
           GO TO F-AR7.
           MOVE D3 TO DD.
           PERFORM Z THRU F-Z.
           IF BAND = 1 GO TO AR7.
           MOVE L-CODI TO D3-L.
           MOVE C-CODI TO D3-C.
       F-AR7.

       AR8.
           ACCEPT (8, 16) D4 WITH PROMPT.
           IF D4 = "  " MOVE "-" TO D4
           MOVE SPACES TO D4-L D4-C D4-FI D4-FF
           DISPLAY (11, 30) FI7
           DISPLAY (11, 32) "/"
           DISPLAY (11, 33) FI8
           DISPLAY (11, 40) FF7
           DISPLAY (11, 42) "/"
           DISPLAY (11, 43) FF8
           GO TO F-AR8.
           MOVE D4 TO DD.
           PERFORM Z THRU F-Z.
           IF BAND = 1 GO TO AR8.
           MOVE L-CODI TO D4-L.
           MOVE C-CODI TO D4-C.
       F-AR8.

       AR9.
           ACCEPT (08, 35) D5 WITH PROMPT.
           IF D5 = "  " MOVE "-" TO D5
           MOVE SPACES TO D5-L D5-C D5-FI D5-FF
           DISPLAY (11, 54) FI9
           DISPLAY (11, 56) "/"
           DISPLAY (11, 57) FI10
           DISPLAY (11, 64) FF9
           DISPLAY (11, 66) "/"
           DISPLAY (11, 67) FF10
           GO TO F-AR9.
           MOVE D5 TO DD.
           PERFORM Z THRU F-Z.
           IF BAND = 1  GO TO AR9.
           MOVE L-CODI TO D5-L.
           MOVE C-CODI TO D5-C.
       F-AR9.

       AR10.
           ACCEPT (08, 55) D6 WITH PROMPT.
           IF D6 = "  " MOVE "-" TO D6
           MOVE SPACES TO D6-L D6-C D6-FI D6-FF
           DISPLAY (12, 7) FI11
           DISPLAY (12, 9) "/"
           DISPLAY (12, 10) FI12
           DISPLAY (12, 17) FF11
           DISPLAY (12, 19) "/"
           DISPLAY (12, 20) FF12
           GO TO F-AR8.
           MOVE D6 TO DD.
           PERFORM Z THRU F-Z.
           IF BAND = 1 GO TO AR10.
           MOVE L-CODI TO D6-L.
           MOVE C-CODI TO D6-C.
       F-AR10.

       AR11.
           ACCEPT (09, 16) D7 WITH PROMPT.
           IF D7 = "  " MOVE "-" TO D7
           MOVE SPACES TO D7-L D7-C D7-FI D7-FF
           DISPLAY (12, 30) FI13
           DISPLAY (12, 32) "/"
           DISPLAY (12, 33) FI14
           DISPLAY (12, 40) FF13
           DISPLAY (12, 42) "/"
           DISPLAY (12, 43) FF14
           GO TO F-AR11.
           MOVE D7 TO DD.
           PERFORM Z THRU F-Z.
           IF BAND = 1 GO TO AR11.
           MOVE L-CODI TO D7-L.
           MOVE C-CODI TO D7-C.
       F-AR11.

       CR1.
           MOVE 0 TO BAND.
           MOVE FI5 TO FC.
           MOVE FF5 TO FE.
           MOVE FI6 TO FC1.
           MOVE FF6 TO FE1.
           PERFORM CALC THRU F-CALC.
           IF BAND = 2 GO TO F-CR1.
           IF RF = 1 MOVE D3 TO D4
           DISPLAY (08, 16) D4
           MOVE SPACES TO D4-L D4-C D4-FI D4-FF
           GO TO F-CR1.
           IF RF = 2 MOVE D3 TO D4 D5
           DISPLAY (08, 16) D4
           DISPLAY (08, 35) D5
           MOVE SPACES TO D4-L D4-C D4-FI D4-FF
                          D5-L D5-C D5-FI D5-FF
           GO TO F-CR1.
           IF RF = 3 MOVE D3 TO D4 D5 D6
           DISPLAY (08, 16) D4
           DISPLAY (08, 35) D5
           DISPLAY (08, 55) D6
           MOVE SPACES TO D4-L D4-C D4-FI D4-FF
                          D5-L D5-C D5-FI D5-FF
                          D6-L D6-C D6-FI D6-FF
           GO TO F-CR1.
           IF RF = 4 MOVE D3 TO D4 D5 D6 D7
           DISPLAY (08, 16) D4
           DISPLAY (08, 35) D5
           DISPLAY (08, 55) D6
           DISPLAY (09, 16) D7
           MOVE SPACES TO D4-L D4-C D4-FI D4-FF
                          D5-L D5-C D5-FI D5-FF
                          D6-L D6-C D6-FI D6-FF
                          D7-L D7-C D7-FI D7-FF.
       F-CR1.      
       
       CR2.
           MOVE 0 TO BAND.
           MOVE FI7 TO FC.
           MOVE FF7 TO FE.
           MOVE FI8 TO FC1.
           MOVE FF8 TO FE1.
           PERFORM CALC THRU F-CALC.
           IF BAND = 2 GO TO F-CR2.
           IF RF = 1 MOVE D4 TO D5
           DISPLAY (08, 35) D5
           MOVE SPACES TO D5-L D5-C D5-FI D5-FF
           GO TO F-CR2.
           IF RF = 2 MOVE D4 TO D5 D6
           DISPLAY (08, 35) D5
           DISPLAY (08, 55) D6
           MOVE SPACES TO D5-L D5-C D5-FI D5-FF
                          D6-L D6-C D6-FI D6-FF
           GO TO F-CR2.
           IF RF = 3 MOVE D4 TO D5 D6 D7
           DISPLAY (08, 35) D5
           DISPLAY (08, 55) D6
           DISPLAY (09, 16) D7
           MOVE SPACES TO D5-L D5-C D5-FI D5-FF
                          D6-L D6-C D6-FI D6-FF
                          D7-L D7-C D7-FI D7-FF.
       F-CR2.

       CR3.
           MOVE 0 TO BAND.
           MOVE FI9 TO FC.
           MOVE FF9 TO FE.
           MOVE FI10 TO FC1.
           MOVE FF10 TO FE1.
           PERFORM CALC THRU F-CALC.
           IF BAND = 2 GO TO F-CR3.     
           IF RF = 1 MOVE D5 TO D6
           DISPLAY (08, 55) D6
           MOVE SPACES TO D6-L D6-C D6-FI D6-FF
           GO TO F-CR3.
           IF RF = 2 MOVE D5 TO D6 D7
           DISPLAY (08, 55) D6
           DISPLAY (09, 16) D7
           MOVE SPACES TO D6-L D6-C D6-FI D6-FF
                          D7-L D7-C D7-FI D7-FF.
       F-CR3.

       CR4.
           MOVE 0 TO BAND.
           MOVE FI11 TO FC.
           MOVE FF11 TO FE.
           MOVE FI12 TO FC1.
           MOVE FF12 TO FE1.
           PERFORM CALC THRU F-CALC.
           IF BAND = 2 GO TO F-CR4.
           IF RF = 1 MOVE D6 TO D7
           DISPLAY (09, 16) D7
           MOVE SPACES TO D7-L D7-C D7-FI D7-FF.
       F-CR4.          

       DR1.
           PERFORM Z1 THRU F-Z1.
           MOVE DD-FI TO D3-FI.
           IF D3-FI = "     "
           MOVE SPACES TO D3-L D3-C D3-FI D3-FF.
           DISPLAY (11, 7) FI5.
           DISPLAY (11, 9) "/"
           DISPLAY (11, 10) FI6.
       F-DR1.

       DR2.
           PERFORM Z2 THRU F-Z2.
           MOVE DD-FF TO D3-FF.
           IF D3-FF = "     "     
           MOVE SPACES TO D3-L D3-C D3-FI D3-FF.
           DISPLAY (11, 17) FF5.
           DISPLAY (11, 19) "/"
           DISPLAY (11, 20) FF6.
       F-DR2.

       DR3.
           PERFORM Z1 THRU F-Z1.
           MOVE DD-FI TO D4-FI.
           IF D4-FI = "     "
           MOVE SPACES TO D4-L D4-C D4-FI D4-FF.
           DISPLAY (11, 30) FI7.
           DISPLAY (11, 32) "/"
           DISPLAY (11, 33) FI8.
       F-DR3.

       DR4.
           PERFORM Z2 THRU F-Z2.
           MOVE DD-FF TO D4-FF.
           IF D4-FF = "     "
           MOVE SPACES TO D4-L D4-C D4-FI D4-FF.
           DISPLAY (11, 40) FF7.
           DISPLAY (11, 42) "/"
           DISPLAY (11, 43) FF8.
       F-DR4.

       DR5.
           PERFORM Z1 THRU F-Z1.
           MOVE DD-FI TO D5-FI.
           IF D5-FI = "     "
           MOVE SPACES TO D5-L D5-C D5-FI D5-FF.
           DISPLAY (11, 54) FI9.
           DISPLAY (11, 56) "/"
           DISPLAY (11, 57) FI10.
       F-DR5.

       DR6.
           PERFORM Z2 THRU F-Z2.
           MOVE DD-FF TO D5-FF.
           IF D5 = "      "
           MOVE SPACES TO D5-L D5-C D5-FI D5-FF.
           DISPLAY (11, 64) FF9.
           DISPLAY (11, 66) "/"
           DISPLAY (11, 67) FF10.
       F-DR6.

       DR7.
           PERFORM Z1 THRU F-Z1.
           MOVE DD-FI TO D6-FI.
           IF D6-FI = "     "
           MOVE SPACES TO D6-L D6-C D6-FI D6-FF.
           DISPLAY (12, 7) FI11.
           DISPLAY (12, 9) "/"
           DISPLAY (12, 10) FI12.
       F-DR7.
           
       DR8.
           PERFORM Z2 THRU F-Z2.
           MOVE DD-FF TO D6-FF.
           IF D6-FF = "     "
           MOVE SPACES TO D6-L D6-C D6-FI D6-FF.
           DISPLAY (12, 17) FF11.
           DISPLAY (12, 19) "/"
           DISPLAY (12, 20) FF12.
       F-DR8.

       DR9.
           PERFORM Z1 THRU F-Z1.
           MOVE DD-FI TO D7-FI.
           IF D7-FI = "     "
           MOVE SPACES TO D7-L D7-C D7-FI D7-FF.
           DISPLAY (12, 30) FI13.
           DISPLAY (12, 32) "/"
           DISPLAY (12, 33) FI14.
       F-DR9.

       DR10.
           PERFORM Z2 THRU F-Z2.
           MOVE DD-FF TO D7-FF.
           IF D7-FF = "     "
           MOVE SPACES TO D7-L D7-C D7-FI D7-FF.
           DISPLAY (12, 40) FF13.
           DISPLAY (12, 42) "/"
           DISPLAY (12, 43) FF14.
       F-DR10.  

      ********************SUBRUTINA MODIFICACION/BAJA*****************
       BAMOD.
           DISPLAY LIM-P.
           DISPLAY TITU.
           DISPLAY BM-MENU.
       MB.
           MOVE SPACES TO OPC.
           ACCEPT (16, 56) OPC WITH PROMPT.
           IF OPC = "1" GO TO BME.
           IF OPC = "2" GO TO BMM.
           IF OPC = "3" GO TO BMC.
           IF OPC = "4" GO TO FIN-3.
           GO TO MB.
       BME.
           OPEN I-O AREMPLE.
           DISPLAY (5, 1) ERASE.
           DISPLAY ING-AGE.
           DISPLAY PREGUNTA3.
       BME1.
           ACCEPT (08, 42) LEGAJO2 WITH PROMPT.
       BME2.
           MOVE LEGAJO2 TO LEGAJO.
           READ AREMPLE KEY IS LEGAJO
           INVALID KEY DISPLAY (08, 50) "REGISTRO NO ENCONTRADO"
           ACCEPT (08, 74) OPC WITH PROMPT
           DISPLAY (08, 50) "                       "
           GO TO BME1.
           DISPLAY (10, 42) DIG-VER.
           DISPLAY (12, 42) APE-NOM.
       BME3.
           MOVE SPACES TO OPC.
           ACCEPT (14, 52) OPC WITH PROMPT.
           IF OPC = "1" GO TO BME4.
           IF OPC = "2" GO TO BME5.
           IF OPC = "3" GO TO BME6.
           IF OPC = "4" CLOSE AREMPLE GO TO BAMOD.
           GO TO BME3.
       BME4.
           REWRITE REG-EMP.
           DISPLAY (15, 10) "REGISTRO GRABADO".
           ACCEPT (15, 28) OPC WITH PROMPT.
           DISPLAY (15, 10) "                        ".
           GO TO BME3.
       BME5.
           DISPLAY (15, 10) "ES EL REGISTRO A DAR DE BAJA S/N".
           ACCEPT (15, 45) OPC WITH PROMPT.
           IF OPC = "S" GO TO BME51.
           IF OPC = "N"
           DISPLAY (15, 10) "                                           "
           GO TO BME3.
           GO TO BME5.
       BME51.
           DELETE AREMPLE INVALID KEY GO TO BME5.
           DISPLAY (15, 10) "                                           ".
           DISPLAY (15, 10) "REGISTRO DADO DE BAJA".
           ACCEPT  (15, 42) OPC WITH PROMPT.
           DISPLAY (15, 10) "                                           ".
           GO TO BME3.
       BME6.
           DISPLAY MODIFICA1.
           ACCEPT (15, 45) OPC WITH PROMPT.
           IF OPC = "1" DISPLAY (8, 52) "NO SE PUEDE MODIFICAR"
                        DISPLAY (9, 52) "EL LEGAJO, DAR DE BAJA"
                        ACCEPT (9, 74) OPC WITH PROMPT
                        DISPLAY (8, 52) "                      "
                        DISPLAY (9, 52) "                      "
                        GO TO BME6
           ELSE
           IF OPC = "2" PERFORM AL-D2
           ELSE
           IF OPC = "3" PERFORM AL-D3.
           DISPLAY PANTALLA-BORRA11.
           GO TO BME3.
       BMM.
           OPEN I-O ARMOVIM ARCODI.
           DISPLAY (5, 1) ERASE.
           DISPLAY ING-DAT.
           DISPLAY PREGUNTA1.
           MOVE 1 TO COL.
       B-CODI.
           MOVE 15 TO LIN.
       B-CODI1.
           ADD 1 TO LIN.
           READ ARCODI NEXT RECORD AT END GO TO B-CODI2.
           MOVE L-CODI TO LS-CODI.
           MOVE C-CODI TO CS-CODI.
           DISPLAY (LIN, COL) R-CODI.
           IF LIN > 23 ADD 20 TO COL GO TO B-CODI.
           GO TO B-CODI1.
       B-CODI2.
           CLOSE ARCODI
           OPEN I-O ARCODI.
       B-DATOS.
           DISPLAY PANTALLA-BORRA.
           ACCEPT (5, 26) LEGAJO1 WITH PROMPT.
           READ ARMOVIM KEY IS LEGAJO1
           INVALID KEY
           DISPLAY (5, 32) "MOVIMIENTO NO ENCONTRADO"
           ACCEPT (5, 57) OPC WITH PROMPT
           GO TO B-DATOS.
       B-MUESTRA.
           DISPLAY (6, 26) APE-NOM1.
           DISPLAY (7, 16) D1.
           DISPLAY (7, 35) D2.
           DISPLAY (7, 55) D3.
           DISPLAY (8, 16) D4.
           DISPLAY (8, 35) D5.
           DISPLAY (8, 55) D6.
           DISPLAY (9, 16) D7.
           DISPLAY (10, 26) OBS-M.
           DISPLAY (11, 7) FI5.
           DISPLAY (11, 9) "/".
           DISPLAY (11, 10) FI6.
           DISPLAY (11, 17) FF5.
           DISPLAY (11, 19) "/".
           DISPLAY (11, 20) FF6.
           DISPLAY (11, 30) FI7.
           DISPLAY (11, 32) "/".
           DISPLAY (11, 33) FI8.
           DISPLAY (11, 40) FF7.
           DISPLAY (11, 42) "/".
           DISPLAY (11, 43) FF8.
           DISPLAY (11, 54) FI9.
           DISPLAY (11, 56) "/".
           DISPLAY (11, 57) FI10.
           DISPLAY (11, 64) FF9.
           DISPLAY (11, 66) "/".
           DISPLAY (11, 67) FF10.
           DISPLAY (12, 7) FI11.
           DISPLAY (12, 9) "/".
           DISPLAY (12, 10) FI12.
           DISPLAY (12, 17) FF11.
           DISPLAY (12, 19) "/".
           DISPLAY (12, 20) FF12.
           DISPLAY (12, 30) FI13.
           DISPLAY (12, 32) "/".
           DISPLAY (12, 33) FI14.
           DISPLAY (12, 40) FF13.
           DISPLAY (12, 42) "/".
           DISPLAY (12, 43) FF14.
       B-PRE.
           DISPLAY PANTALLA-BORRAS.
           DISPLAY (14, 49) "    ".
           ACCEPT (14, 50) OPC WITH PROMPT.
           IF OPC = "1" GO TO B-GRA.
           IF OPC = "2" GO TO B-BAJ.
           IF OPC = "3" GO TO B-MOD.
           IF OPC = "4" CLOSE ARMOVIM ARCODI GO TO BAMOD.
           GO TO B-PRE.
       B-GRA.
           REWRITE REG-MOV.
           DISPLAY (13, 10) "REGISTRO GRABADO".
           ACCEPT (13, 28) OPC WITH PROMPT.
           DISPLAY (13, 10) "                         ".
           GO TO B-PRE.
       B-BAJ.
           DISPLAY (13, 10) "ES EL REGISTRO A DAR DE BAJA S/N"
           ACCEPT (13, 45) OPC WITH PROMPT.
           IF OPC = "S" GO TO B-BAJ1.
           IF OPC = "N"
           DISPLAY (13, 10) "                                           "
           GO TO B-PRE.
           GO TO B-BAJ.
       B-BAJ1.
           DELETE ARMOVIM INVALID KEY GO TO B-PRE.
           DISPLAY (13, 10) "                                           ".
           DISPLAY (13, 10) "REGISTRO DADO DE BAJA".
           ACCEPT (13, 42) OPC WITH PROMPT.
           DISPLAY (13, 10) "                         ".
           GO TO B-PRE.
       B-MOD.
           DISPLAY MODIFICA.
           ACCEPT (13, 45) OPC WITH PROMPT.
           IF OPC = "1" PERFORM AR5 THRU F-AR5 
           ELSE
           IF OPC = "2" PERFORM AR6 THRU F-AR6 
           ELSE
           IF OPC = "3" PERFORM AR7 THRU F-AR7
                        PERFORM DR1 THRU F-DR1
                        PERFORM DR2 THRU F-DR2
                        PERFORM CR1 THRU F-CR1
           ELSE
           IF OPC = "4" PERFORM AR8 THRU F-AR8
                        PERFORM DR3 THRU F-DR3
                        PERFORM DR4 THRU F-DR4
                        PERFORM CR2 THRU F-CR2
           ELSE
           IF OPC = "5" PERFORM AR9 THRU F-AR9
                        PERFORM DR5 THRU F-DR5
                        PERFORM DR6 THRU F-DR6
                        PERFORM CR3 THRU F-CR3
           ELSE
           IF OPC = "6" PERFORM AR10 THRU F-AR10
                        PERFORM DR7  THRU F-DR7
                        PERFORM DR8  THRU F-DR8
                        PERFORM CR4  THRU F-CR4
           ELSE
           IF OPC = "7" PERFORM AR11 THRU F-AR11
                        PERFORM DR9  THRU F-DR9
                        PERFORM DR10 THRU F-DR10
           ELSE
           IF OPC = "8" PERFORM PR12
           ELSE
           IF OPC = "9" PERFORM DR1 THRU F-DR1
           ELSE
           IF OPC = "10" PERFORM DR2 THRU F-DR2
           ELSE
           IF OPC = "11" PERFORM DR3 THRU F-DR3
           ELSE
           IF OPC = "12" PERFORM DR4 THRU F-DR4
           ELSE
           IF OPC = "13" PERFORM DR5 THRU F-DR5
           ELSE
           IF OPC = "14" PERFORM DR6 THRU F-DR6
           ELSE
           IF OPC = "15" PERFORM DR7 THRU F-DR7
           ELSE
           IF OPC = "16" PERFORM DR8 THRU F-DR8
           ELSE
           IF OPC = "17" PERFORM DR9 THRU F-DR9
           ELSE
           IF OPC = "18" PERFORM DR10 THRU F-DR10.
           GO TO B-PRE.
       BMC.
           OPEN I-O ARCODI.
           DISPLAY (5, 1) ERASE.
           DISPLAY ING-COD.
           DISPLAY PREGUNTA1.
       E-CO.
           DISPLAY PANTALLA-B.
       E-CO1.
           ACCEPT (8, 42) L-CODI WITH PROMPT.
           IF L-CODI = "       " GO TO E-CO1.
           READ ARCODI KEY IS L-CODI
           INVALID KEY DISPLAY (8, 52) "CODIGO NO ENCONTRADO"
           ACCEPT (8, 74) OPC WITH PROMPT
           DISPLAY (8, 52) "                          "
           GO TO E-CO1.
       E-CO2.
           DISPLAY (10, 42) C-CODI.
       E-CO3.
           DISPLAY PANTALLA-BORRAS.
           MOVE SPACES TO OPC.
           ACCEPT (14, 55) OPC WITH PROMPT.
           IF OPC = "1" GO TO E-COG.
           IF OPC = "2" GO TO E-COB.
           IF OPC = "3" GO TO E-COM.
           IF OPC = "4" CLOSE ARCODI GO TO BAMOD.
           GO TO E-CO3.
       E-COG.
           REWRITE REG-CODI.
           DISPLAY (13, 10) "REGISTRO GRABADO"
           ACCEPT (13, 27) OPC WITH PROMPT 
           DISPLAY (13, 10) "                   ".
           GO TO E-CO3.
       E-COB.
           DISPLAY (13, 10) "ES EL REGISTRO A DAR DE BAJA S/N".
           ACCEPT (13, 45) OPC WITH PROMPT.
           IF OPC = "S" GO TO E-COB1.
           IF OPC = "N" 
           DISPLAY (13, 10) "                                 "                             
           GO TO E-CO3.
       E-COB1.
           DELETE ARCODI INVALID KEY GO TO E-CO3.
           DISPLAY (13, 10) "                                 ".
           DISPLAY (13, 10) "REGISTRO DADO DE BAJA".
           ACCEPT (13, 33) OPC WITH PROMPT.
           DISPLAY (13, 10) "                            ".
           GO TO E-CO3.
       E-COM.
           DISPLAY MODIFICA11.
           ACCEPT (13, 48) OPC WITH PROMPT.
           IF OPC = "1" DISPLAY (8, 52) "NO SE PUEDE MODIFICAR"
                        DISPLAY (9, 52) "EL CODIGO, DAR DE BAJA"
                        ACCEPT (8, 74) OPC WITH PROMPT
                        DISPLAY (8, 52) "                       "
                        DISPLAY (9, 52) "                        "
           ELSE
           IF OPC = "2" PERFORM AC-D2.
           GO TO E-CO3.
       FIN-3.
           GO TO MENU.
       CIERRE.
           DISPLAY LIM-P.
       CIERRE1.
           CLOSE LC97.
           STOP RUN.





