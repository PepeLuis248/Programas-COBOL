       IDENTIFICATION  DIVISION.
       PROGRAM-ID.         NUMLE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  I1      PIC 9(3)  VALUE ZERO.
       77  I2      PIC 9(3)  VALUE ZERO.
       77  I3      PIC 9(3)  VALUE ZERO.
       77  I4      PIC 9(3)  VALUE ZERO.
       77  I5      PIC 9(3)  VALUE ZERO.
       77  AC      PIC 9(3)  VALUE ZERO.
       77  QX      PIC 9(3)  VALUE ZERO.
       01  PARAM.
           03  QTY         PIC 9(11)V99.
           03  MAX         PIC 9(3).
           03  SKP         PIC 99.
           03  SWA         PIC X.
           03  SWB         PIC X.
       01  TAB1.
           03  T1A         PIC X(120) VALUE SPACES.
           03  T1B         PIC X(120) VALUE SPACES.
       01  TAB2   REDEFINES  TAB1.
           03  T2          PIC X   OCCURS 240 TIMES.
       01  TAB3.
           03  T3          PIC X(14) VALUE SPACES.
       01  TAB4   REDEFINES  TAB3.
           03  T4          PIC X   OCCURS 14 TIMES.
       01  TAB5.
           03  FILLER      PIC X(19) VALUE "00103UN*           ".
           03  FILLER      PIC X(19) VALUE "00204DOS*          ".
           03  FILLER      PIC X(19) VALUE "00305TRES*         ".
           03  FILLER      PIC X(19) VALUE "00407CUATRO*       ".
           03  FILLER      PIC X(19) VALUE "00506CINCO*        ".
           03  FILLER      PIC X(19) VALUE "00605SEIS*         ".
           03  FILLER      PIC X(19) VALUE "00706SIETE*        ".
           03  FILLER      PIC X(19) VALUE "00805OCHO*         ".
           03  FILLER      PIC X(19) VALUE "00906NUEVE*        ".
           03  FILLER      PIC X(19) VALUE "01005DIEZ*         ".
           03  FILLER      PIC X(19) VALUE "01105ONCE*         ".
           03  FILLER      PIC X(19) VALUE "01205DOCE*         ".
           03  FILLER      PIC X(19) VALUE "01306TRECE*        ".
           03  FILLER      PIC X(19) VALUE "01408CATORCE*      ".
           03  FILLER      PIC X(19) VALUE "01507QUINCE*       ".
           03  FILLER      PIC X(19) VALUE "01610DIECISEIS*    ".
           03  FILLER      PIC X(19) VALUE "01711DIECISIETE*   ".
           03  FILLER      PIC X(19) VALUE "01810DIECIOCHO*    ".
           03  FILLER      PIC X(19) VALUE "01911DIECINUEVE*   ".
           03  FILLER      PIC X(19) VALUE "02007VEINTE*       ".
           03  FILLER      PIC X(19) VALUE "02109VEINTIUN*     ".
           03  FILLER      PIC X(19) VALUE "02210VEINTIDOS*    ".
           03  FILLER      PIC X(19) VALUE "02311VEINTITRES*   ".
           03  FILLER      PIC X(19) VALUE "02413VEINTICUATRO* ".
           03  FILLER      PIC X(19) VALUE "02512VEINTICINCO*  ".
           03  FILLER      PIC X(19) VALUE "02611VEINTISEIS*   ".
           03  FILLER      PIC X(19) VALUE "02712VEINTISIETE*  ".
           03  FILLER      PIC X(19) VALUE "02811VEINTIOCHO*   ".
           03  FILLER      PIC X(19) VALUE "02912VEINTINUEVE*  ".
           03  FILLER      PIC X(19) VALUE "03008TREINTA*      ".
           03  FILLER      PIC X(19) VALUE "04009CUARENTA*     ".
           03  FILLER      PIC X(19) VALUE "05010CINCUENTA*    ".
           03  FILLER      PIC X(19) VALUE "06008SESENTA*      ".
           03  FILLER      PIC X(19) VALUE "07008SETENTA*      ".
           03  FILLER      PIC X(19) VALUE "08008OCHENTA*      ".
           03  FILLER      PIC X(19) VALUE "09008NOVENTA*      ".
           03  FILLER      PIC X(19) VALUE "10007CIENTO*       ".
           03  FILLER      PIC X(19) VALUE "20011DOSCIENTOS*   ".
           03  FILLER      PIC X(19) VALUE "30012TRESCIENTOS*  ".
           03  FILLER      PIC X(19) VALUE "40014CUATROCIENTOS*".
           03  FILLER      PIC X(19) VALUE "50011QUINIENTOS*   ".
           03  FILLER      PIC X(19) VALUE "60012SEISCIENTOS*  ".
           03  FILLER      PIC X(19) VALUE "70012SETECIENTOS*  ".
           03  FILLER      PIC X(19) VALUE "80012OCHOCIENTOS*  ".
           03  FILLER      PIC X(19) VALUE "90012NOVECIENTOS*  ".
           03  FILLER      PIC X(19) VALUE "99904MIL*          ".
           03  FILLER      PIC X(19) VALUE "99907MILLON*       ".
           03  FILLER      PIC X(19) VALUE "99909MILLONES*     ".
           03  FILLER      PIC X(19) VALUE "99902Y*            ".
           03  FILLER      PIC X(19) VALUE "99904CON*          ".
           03  FILLER      PIC X(19) VALUE "99909CENTAVOS*     ".
           03  FILLER      PIC X(19) VALUE "99905CIEN*         ".
           03  FILLER      PIC X(19) VALUE "99904UNO*          ".
           03  FILLER      PIC X(19) VALUE "99910VEINTIUNO*    ".
           03  FILLER      PIC X(19) VALUE "99904SON*          ".
           03  FILLER      PIC X(19) VALUE "99914             *".
           03  FILLER      PIC X(19) VALUE "99907BILLON*       ".
           03  FILLER      PIC X(19) VALUE "99909BILLONES*     ".
           03  FILLER      PIC X(19) VALUE "99908CENTAVO*      ".
       01  TAB6   REDEFINES  TAB5.
           03  T6          OCCURS 59 TIMES.
                   05  T6QTY       PIC 9(3).
                   05  T6LON       PIC 99.
                   05  T6DES       PIC X(14).
       01  TAB7.
           03  T7A         PIC 9(11)V99.
           03  T7B         REDEFINES T7A.
                   05  T7B1        PIC 9(11).
                   05  T7B2        PIC 9(2).
           03  T7C         REDEFINES T7A.
                   05  T7CY        PIC 9(2).
                   05  T7C1        PIC 9(3).
                   05  T7C2        PIC 9(3).
                   05  T7C3        PIC 9(3).
                   05  T7C4        PIC 9(2).
       01  TAB8.
           03  T8          PIC 9(3)  OCCURS 35 TIMES.
       LINKAGE SECTION.
       01  PARAM1          PIC X(24).
       01  PARAM2          PIC X(240).
       PROCEDURE DIVISION USING PARAM1 PARAM2.
       X1. MOVE PARAM1 TO PARAM.
           INSPECT PARAM REPLACING ALL SPACE BY ZERO.
           IF MAX < 014 MOVE 014 TO MAX.
           MOVE QTY TO T7A.
           MOVE SPACES TO TAB1.
           PERFORM A1 THRU A9.
           MOVE TAB1 TO PARAM2.
           GO TO Z9.
       A1. MOVE ZERO TO I1.
           IF SWA NOT = "0"
                   MOVE 55 TO T8(1) MOVE 56 TO T8(2) MOVE 2 TO I1.
           IF T7A = ZERO GO TO A8.
           IF T7B1 NOT = ZERO GO TO AY.
           IF SWA NOT = "0" SUBTRACT 1 FROM I1.
           GO TO A6.
       AY. IF T7CY = ZERO GO TO A2.
           MOVE T7CY TO QX.
           PERFORM B1 THRU B7.
           ADD 1 TO I1.
           MOVE 46 TO T8 (I1).
       A2. IF T7C1 = ZERO GO TO AZ.
           MOVE T7C1 TO QX.
           PERFORM B1 THRU B7.
           ADD 1 TO I1.
           IF T7C1 = 1 AND T7CY = ZERO MOVE 47 TO T8 (I1)
           ELSE MOVE 48 TO T8 (I1).
           GO TO A3.
       AZ. IF T7CY = ZERO GO TO A3.
           ADD 1 TO I1.
           MOVE 48 TO T8 (I1).
       A3. IF T7C2 = ZERO GO TO A4.
           MOVE T7C2 TO QX.
           PERFORM B1 THRU B7.
           ADD 1 TO I1.
           MOVE 46 TO T8 (I1).
       A4. IF T7C3 = ZERO GO TO A5.
           MOVE T7C3 TO QX.
           PERFORM B1 THRU B7.
      *  OJO QUE LA SENTENCIA PROXIMA PUEDE SER IF T8 (I1) = 0
           IF T8 (I1) = 1  MOVE 53 TO T8 (I1).
           IF T8 (I1) = 21 MOVE 54 TO T8 (I1).
       A5. IF T7C4 = ZERO GO TO A7.
           ADD 1 TO I1.
           MOVE 50 TO T8 (I1).
       A6. MOVE T7C4 TO QX.
           PERFORM B1 THRU B7.
           ADD 1 TO I1.
           IF T7C4 = 1 MOVE 59 TO T8 (I1)
           ELSE MOVE 51 TO T8 (I1).
       A7. PERFORM C1 THRU C5.
           MOVE "." TO T2 (I3).
           ADD 1 TO I3.
           MOVE "-" TO T2 (I3).
           PERFORM D1 THRU D3.
           MOVE I3 TO MAX.
           INSPECT TAB1 REPLACING ALL "*" BY " ".
           GO TO A9.
       A8. MOVE ALL "*" TO TAB1.
       A9. EXIT.
       B1. MOVE ZERO TO I3.
       B2. ADD 1 TO I3.
           IF QX < T6QTY (I3) GO TO B3.
           IF QX > T6QTY (I3) GO TO B2.
           IF I3 NOT = 46 GO TO B4.
       B3. SUBTRACT 1 FROM I3.
       B4. ADD 1 TO I1.
           MOVE I3 TO T8 (I1).
           SUBTRACT T6QTY (I3) FROM QX.
           IF I3 < 30 GO TO B6.
           IF I3 > 36 GO TO B5.
           IF QX NOT = ZERO ADD 1 TO I1 MOVE 49 TO T8 (I1).
           GO TO B6.
       B5. IF I3 NOT = 37 GO TO B6.
           IF QX NOT = ZERO GO TO B6.
           MOVE 52 TO T8 (I1).
       B6. IF QX NOT = ZERO GO TO B1.
       B7. EXIT.
       C1. MOVE ZERO TO AC I2 I3.
           MOVE SKP TO I3.
       C2. ADD 1 TO I2.
           MOVE T8 (I2) TO I4.
           MOVE T6DES (I4) TO TAB3.
           ADD I3 T6LON (I4) GIVING I5.
           SUBTRACT AC FROM I5.
           IF I5 NOT > MAX GO TO C3.
           ADD MAX TO AC.
           MOVE AC TO I3.
       C3. MOVE ZERO TO I5.
       C4. ADD 1 TO I5.
           ADD 1 TO I3.
           MOVE T4 (I5) TO T2 (I3).
           IF I5 < T6LON (I4) GO TO C4.
           IF I2 < I1 GO TO C2.
       C5. EXIT.
       D1. IF SWB = "0" GO TO D3.
       D2. ADD 1 TO I3.
           MOVE "-" TO T2 (I3).
           IF I3 < 240 GO TO D2.
       D3. EXIT.
       Z9. EXIT PROGRAM.
