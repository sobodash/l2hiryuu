PROGRAM FE2;
USES CRT, DOS, GRAPH;
CONST
   MAX=255;
   FNAME='8x16.fnt';
   HEADERTEXT = 'Langrisser2';
   POWA:ARRAY[1..16] OF BYTE=(128,64,32,16,8,4,2,1,128,64,32,16,8,4,2,1);
   CHARZ:ARRAY[1..4] OF BYTE=(32,250,176,177);
   COL:ARRAY[1..4] OF BYTE=(7, 8, 7, 15);
VAR
   FONT:ARRAY[0..MAX+1, 1..32] OF BYTE; {last is buffer for swap}
   FIN, FOUT:FILE;
   I,J,K:WORD;
   X,Y:BYTE;
   Z:INTEGER;
   PS:ARRAY[0..1] OF INTEGER;
   C:CHAR;
   CORD:BYTE;
   SPEC,ARGH,AUTOSTAT:BOOLEAN;
   PEN,OLDPEN:BYTE;
procedure Checkfile(FileName: String);
var
 F: file;
begin
 {$I-}
 Assign(F, FileName);
 FileMode := 0;  { Set file access to read only }
 Reset(F);
 Close(F);
 FileMode := 2;
 {$I+}
 If (IOResult = 0) and (FileName <> '') then
 begin

 end
 else
 begin
    writeln (FileName, ' not found. Aborting.');
    halt(1);
 end;
end;

PROCEDURE FTEXT(OIE:BYTE);
BEGIN
     TEXTCOLOR(COL[OIE]);
     WRITE (CHR(CHARZ[OIE]),CHR(CHARZ[OIE]));
     TEXTCOLOR(7);
END;
FUNCTION GETCBYTE(XC, YC:BYTE):BYTE;

BEGIN
     IF XC > 8 THEN DEC(XC, 8);
           IF YC > 8 THEN
           BEGIN {BOTTOM}
                 GETCBYTE := 16 + ((YC - 8) * 2);
           END
           ELSE
           BEGIN {TOP}
                 GETCBYTE := (YC * 2);
           END;
END;

FUNCTION CBYTE(XC, YC:BYTE):BYTE;
VAR
   OIE:INTEGER;
BEGIN
     IF XC < 9 THEN
     BEGIN {CHAR0}
           OIE := PS[0];
           IF (FONT[OIE, GETCBYTE(XC, YC) - 1] AND POWA[XC]) > 0 THEN
              BEGIN
                   IF (FONT[OIE, GETCBYTE(XC, YC)] AND POWA[XC]) > 0 THEN
                      BEGIN
                           CBYTE := 4;
                      END
                      ELSE
                      BEGIN
                           CBYTE := 3;
                      END;
              END
              ELSE
              BEGIN
                   CBYTE := 2;
              END;
     END
     ELSE
     BEGIN {CHAR1}
           OIE := PS[1];
           DEC(XC, 8);
           IF (FONT[OIE, GETCBYTE(XC, YC) - 1] AND POWA[XC]) > 0 THEN
              BEGIN
                   IF (FONT[OIE, GETCBYTE(XC, YC)] AND POWA[XC]) > 0 THEN
                      BEGIN
                           CBYTE := 4;
                      END
                      ELSE
                      BEGIN
                           CBYTE := 3;
                      END;
              END
              ELSE
              BEGIN
                   CBYTE := 2;
              END;
     END;
END;
PROCEDURE WRITECURSOR;
BEGIN
     GOTOXY(X*2,Y+1);
     TEXTCOLOR(COL[CBYTE(X,Y)]);
     WRITE ('[]');
     TEXTCOLOR(7);
END;
PROCEDURE ERASECURSOR;
BEGIN
     GOTOXY(X*2,Y+1);
     FTEXT(CBYTE(X,Y));
END;
PROCEDURE UPDATESCR;
var
   avoid:boolean;
   ZEE:byte;
BEGIN
       GOTOXY (2, 1);
       TEXTCOLOR(7);
       WRITE ('1ST CHR:');
       TEXTCOLOR(10);
       WRITE (PS[0],'  ');
       GOTOXY (18, 1);
       TEXTCOLOR(7);
       WRITE ('2ND CHR:');
       TEXTCOLOR(10);
       WRITE (PS[1],'  ');
       TEXTCOLOR(7);
       GOTOXY (1, 19);
       TEXTCOLOR(15);
       GOTOXY(66,1);
       IF X > 8 THEN
       BEGIN
            WRITE('[2ND]');
            GOTOXY (2, 19);
            WRITE('                ----------------');
            ZEE := 1;
       END
       ELSE
       BEGIN
            WRITE('[1ST]');
            GOTOXY (2, 19);
            WRITE('----------------                ');
            ZEE := 0;
       END;
       TEXTCOLOR(7);
       GOTOXY(66,2);
       IF X < 9 THEN
       BEGIN
            WRITE ('X:',X, ' ');
       END
       ELSE
       BEGIN
            WRITE ('X:',X-8, ' ');
       END;
       GOTOXY(66,3);
       WRITE ('Y:',Y, ' ');
       GOTOXY(66,4);
       WRITE ('CHR:');
       TEXTCOLOR(10);
       IF X < 9 THEN
       BEGIN
            WRITE (PS[0],'  ');
       END
       ELSE
       BEGIN
            WRITE (PS[1],'  ');
       END;
       TEXTCOLOR(7);
       GOTOXY (66,5);
       WRITE ('COL:');
       TEXTCOLOR(COL[CBYTE(X,Y)]);
       WRITE (CBYTE(X, Y));
       TEXTCOLOR(7);
       GOTOXY (66,6);
       WRITE ('PEN:');
       TEXTCOLOR(COL[PEN]);
       WRITE (PEN);
       GOTOXY (66,7);
       IF PEN = 2 THEN WRITE ('BLACK');
       IF PEN = 3 THEN WRITE ('GREY ');
       IF PEN = 4 THEN WRITE ('WHITE');
       GOTOXY (67,8);
       WRITE (CHR(219),CHR(219),CHR(219));
       GOTOXY (67,9);
       WRITE (CHR(219),CHR(219),CHR(219));
       GOTOXY (67,10);
       WRITE (CHR(219),CHR(219),CHR(219));
       WRITECURSOR;
       GOTOXY (66,12);
       TEXTCOLOR(12);
       IF AUTOSTAT THEN
       BEGIN
            WRITE ('AUTO!');
       END
       ELSE
       BEGIN
            WRITE ('     ');
       END;
       TEXTCOLOR(15);
       GOTOXY(14, 1);
       IF ZEE = 1 THEN GOTOXY (30,1);
       AVOID := FALSE;

       IF PS[ZEE] = 7 THEN AVOID:=TRUE;
       IF PS[ZEE] = 10 THEN AVOID:=TRUE;
       IF PS[ZEE] = 13 THEN AVOID:=TRUE;
       IF AVOID THEN
       BEGIN
            WRITE (''' ''');
       END
       ELSE
       BEGIN
            WRITE ('''',CHR(PS[ZEE]),'''');
            GOTOXY( (PS[ZEE] - (TRUNC(PS[ZEE] / 16) * 16))  +41, TRUNC(PS[ZEE] / 16) + 2);
            WRITE (CHR(PS[ZEE]));
       END;
       TEXTCOLOR(7);
END;

PROCEDURE ERASEASCII;
VAR
   AVOID:BOOLEAN;
   ZEE:BYTE;
BEGIN
       AVOID := FALSE;
              IF X > 8 THEN
                 BEGIN
                      ZEE := 1;
                 END
                 ELSE
                 BEGIN
                      ZEE := 0;
                 END;
       IF PS[ZEE] = 7 THEN AVOID:=TRUE;
       IF PS[ZEE] = 10 THEN AVOID:=TRUE;
       IF PS[ZEE] = 13 THEN AVOID:=TRUE;
       IF NOT AVOID THEN
       BEGIN
            TEXTCOLOR(7);
            GOTOXY( (PS[ZEE] - (TRUNC(PS[ZEE] / 16) * 16))  +41, TRUNC(PS[ZEE] / 16) + 2);
            WRITE (CHR(PS[ZEE]));
       END;
END;

PROCEDURE INITIAL;
BEGIN
       GOTOXY (1, 20);
       writeln ('quickhelp:            A = wrap current char         X = erase current char');
       WRITELN ('arrow keys = move     + & - = change char # by 1    * & / = change char # by 16');
       writeln ('W & E = change pen    tab = switch 1st & 2nd char   esc = exit and save 8x16');
       writeln ('space bar = write     bk & del = erase (pencol2)    Q = toggle autopen');
       writeln ('S = swap chars        enter = manual char entry (instead of + & -, * & /)');
       write   ('NOTE! Valid chars #0-255. Manual entry outside range may crash program!!');
       gotoxy (66, 14);
       write (headertext);
       gotoxy (66, 15);
       write ('8x16 font');
       gotoxy (66, 16);
       write ('editor by');
       gotoxy (66, 17);
       write ('alamone.');
       gotoxy (66, 18);
       write ('v0.1');
       GOTOXY (30,1);
       TEXTCOLOR(15);
       WRITE ('''',chr(1),'''');
       TEXTCOLOR(7);

END;
PROCEDURE INITSCR;
VAR
   J,K,L:WORD;
   AVOID:BOOLEAN;
   BUF:ARRAY[1..32] OF BYTE;
BEGIN
     MOVE (FONT[PS[0]], BUF, 32);
       FOR J := 1 TO 8 DO
          BEGIN
          GOTOXY (2, J+1);
              FOR K := 1 TO 8 DO
              BEGIN
                  IF (BUF[(J*2)-1] AND POWA[K]) > 0 THEN
                      BEGIN
                           IF (BUF[J*2] AND POWA[K]) > 0 THEN
                           BEGIN
                               FTEXT(4); {1-2 BYTE BITS: LOW INTENSITY}
                           END
                           ELSE
                           BEGIN
                               FTEXT(3); {2ND BYTE BIT: HIGH
                                INTENSITY}
                           END;
                      END
                      ELSE
                      BEGIN
                           FTEXT(2); {0 BYTE BITS: NOTHING}
                      END;
              END;
          END;
          FOR J := 1 TO 8 DO
          BEGIN
          GOTOXY (2, J+9);
              FOR K := 1 TO 8 DO
              BEGIN
                  IF (BUF[16+(J*2)-1] AND POWA[K]) > 0 THEN
                      BEGIN
                           IF (BUF[16+J*2] AND POWA[K]) > 0 THEN
                           BEGIN
                               FTEXT(4);
                           END
                           ELSE
                           BEGIN
                               FTEXT(3);
                           END;
                      END
                      ELSE
                      BEGIN
                           FTEXT(2);
                      END;
              END;
          END;

     MOVE (FONT[PS[1]], BUF, 32);
       FOR J := 1 TO 8 DO
          BEGIN
          GOTOXY (18, J+1);
              FOR K := 1 TO 8 DO
              BEGIN
                  IF (BUF[(J*2)-1] AND POWA[K]) > 0 THEN
                      BEGIN
                           IF (BUF[J*2] AND POWA[K]) > 0 THEN
                           BEGIN
                               FTEXT(4);
                           END
                           ELSE
                           BEGIN
                               FTEXT(3);
                           END;
                      END
                      ELSE
                      BEGIN
                           FTEXT(2);
                      END;
              END;
          END;
          FOR J := 1 TO 8 DO
          BEGIN
          GOTOXY (18, J+9);
              FOR K := 1 TO 8 DO
              BEGIN
                  IF (BUF[16+(J*2)-1] AND POWA[K]) > 0 THEN
                      BEGIN
                           IF (BUF[16+J*2] AND POWA[K]) > 0 THEN
                           BEGIN
                               FTEXT(4);
                           END
                           ELSE
                           BEGIN
                               FTEXT(3);
                           END;
                      END
                      ELSE
                      BEGIN
                           FTEXT(2);
                      END;
              END;
          END;

       TEXTCOLOR(9);
       GOTOXY(41,1);
       WRITE ('0123456789ABCDEF');
       FOR J := 0 TO 9 DO BEGIN
       GOTOXY(40,J+2);
       WRITE (J);
       END;
       GOTOXY(40,12);
       WRITE ('A');
       GOTOXY(40,13);
       WRITE ('B');
       GOTOXY(40,14);
       WRITE ('C');
       GOTOXY(40,15);
       WRITE ('D');
       GOTOXY(40,16);
       WRITE ('E');
       GOTOXY(40,17);
       WRITE ('F');
       GOTOXY (1, 18);
       WRITELN ('+0-1-2-3-4-5-6-7 0-1-2-3-4-5-6-7');
       FOR J := 0 TO 9 DO
       BEGIN
       GOTOXY (1, J+2);
       WRITE (J);
       END;
       GOTOXY (1, 10+2);
       WRITE ('A');
       GOTOXY (1, 11+2);
       WRITE ('B');
       GOTOXY (1, 12+2);
       WRITE ('C');
       GOTOXY (1, 13+2);
       WRITE ('D');
       GOTOXY (1, 14+2);
       WRITE ('E');
       GOTOXY (1, 15+2);
       WRITE ('F');

       TEXTCOLOR(7);
       FOR J := 0 TO 15 DO BEGIN
       FOR K := 0 TO 15 DO BEGIN
       GOTOXY(K+41, J + 2);
       L := (J * 16) + K;
       AVOID := FALSE;
       IF L = 7 THEN AVOID:=TRUE;
       IF L = 10 THEN AVOID:=TRUE;
       IF L = 13 THEN AVOID:=TRUE;
       IF AVOID THEN
       BEGIN
            WRITE (' ');
       END
       ELSE
       BEGIN
            WRITE (CHR(L));
       END;
       END;
       END;

       UPDATESCR;
END;
PROCEDURE WRITEPEN(XC, YC:BYTE);
VAR
   BLEH, BLEH3, I:BYTE;
   BLEH2: INTEGER;
   BITS:ARRAY[1..8, 0..1] OF BYTE;
BEGIN
     BLEH := GETCBYTE(XC, YC);
     BLEH3 := 0;
     IF XC > 8 THEN
     BEGIN
          BLEH3 := 1;
          DEC(XC, 8);
     END;
     BLEH2 := PS[BLEH3];

     FOR I := 1 TO 8 DO
     BEGIN
          BITS[I, 0] := 0;
          BITS[I, 1] := 0;
          IF (FONT[BLEH2, BLEH - 1] AND POWA[I]) > 0 THEN BITS[I, 0] := 1;
          IF (FONT[BLEH2, BLEH] AND POWA[I]) > 0 THEN BITS[I, 1] := 1;
     END;
     IF PEN = 2 THEN
     BEGIN
          BITS[XC, 0] := 0;
          BITS[XC, 1] := 1;
     END;
     IF PEN = 3 THEN
     BEGIN
          BITS[XC, 0] := 1;
          BITS[XC, 1] := 0;
     END;
     IF PEN = 4 THEN
     BEGIN
          BITS[XC, 0] := 1;
          BITS[XC, 1] := 1;
     END;
     FONT[BLEH2, BLEH-1] := 0;
     FONT[BLEH2, BLEH] := 0;
     FOR I := 1 TO 8 DO
     BEGIN
          IF BITS[I, 0] = 1 THEN INC(FONT[BLEH2, BLEH-1], POWA[I]);
          IF BITS[I, 1] = 1 THEN INC(FONT[BLEH2, BLEH], POWA[I]);
     END;
END;
PROCEDURE ERASEPEN(XC, YC:BYTE);
VAR
   BLEH, BLEH3, I:BYTE;
   BLEH2: INTEGER;
   BITS:ARRAY[1..8, 0..1] OF BYTE;
BEGIN
     BLEH := GETCBYTE(XC, YC);
     BLEH3 := 0;
     IF XC > 8 THEN
     BEGIN
          BLEH3 := 1;
          DEC(XC, 8);
     END;
     BLEH2 := PS[BLEH3];
     FOR I := 1 TO 8 DO
     BEGIN
          BITS[I, 0] := 0;
          BITS[I, 1] := 0;
          IF (FONT[BLEH2, BLEH - 1] AND POWA[I]) > 0 THEN BITS[I, 0] := 1;
          IF (FONT[BLEH2, BLEH] AND POWA[I]) > 0 THEN BITS[I, 1] := 1;
     END;
          BITS[XC, 0] := 0;
          BITS[XC, 1] := 1;
     FONT[BLEH2, BLEH-1] := 0;
     FONT[BLEH2, BLEH] := 0;
     FOR I := 1 TO 8 DO
     BEGIN
          IF BITS[I, 0] = 1 THEN INC(FONT[BLEH2, BLEH-1], POWA[I]);
          IF BITS[I, 1] = 1 THEN INC(FONT[BLEH2, BLEH], POWA[I]);
     END;
END;
BEGIN
   checkfile(fname);
   ASSIGN (FIN, fname);
   RESET (FIN, 1);
   FOR I := 0 TO MAX DO
   BEGIN
        BLOCKREAD (FIN, FONT[I], 32);
   END;
   CLOSE (FIN);
   X := 1;
   Y := 1;
   PS[0] := 0;
   PS[1] := 1;
   PEN := 2;
   CLRSCR;
   INITSCR;
   INITIAL;

   WHILE ORD(C) <> 27 DO
   BEGIN
        UPDATESCR;
        SPEC := FALSE;
        WHILE NOT KEYPRESSED DO
        BEGIN
{             WRITECURSOR;}
             GOTOXY(X * 2, Y + 1);
        END;
        ERASEASCII;
        C := READKEY;
        IF ORD(C) = 0 THEN
        BEGIN
             C := READKEY;
             SPEC := TRUE;
        END
        ELSE
        BEGIN
             C := UPCASE(C);
        END;
        CORD := ORD(C);
        IF SPEC THEN
        BEGIN {direction}
              ERASECURSOR;
              IF CORD = 72 THEN DEC(Y);
              IF CORD = 80 THEN INC(Y);
              IF CORD = 75 THEN DEC(X);
              IF CORD = 77 THEN INC(X);
              IF CORD = 71 THEN DEC(Y);
              IF CORD = 73 THEN DEC(Y);
              IF CORD = 79 THEN INC(Y);
              IF CORD = 81 THEN INC(Y);
              IF CORD = 71 THEN DEC(X);
              IF CORD = 73 THEN INC(X);
              IF CORD = 79 THEN DEC(X);
              IF CORD = 81 THEN INC(X);
              IF X < 1 THEN INC(X, 16);
              IF X > 16 THEN DEC(X, 16);
              IF Y < 1 THEN INC(Y, 16);
              IF Y > 16 THEN DEC(Y, 16);
              IF CORD = 83 THEN ERASEPEN(X, Y);
        END
        ELSE
        BEGIN {regular}
              ARGH := FALSE;
              IF X > 8 THEN
                 BEGIN
                      Z := 1;
                 END
                 ELSE
                 BEGIN
                      Z := 0;
                 END;
              IF C = '+' THEN
                 BEGIN
                 INC(PS[Z]); {+}
                 ARGH := TRUE;
                 END;
              IF C = '-' THEN
                 BEGIN
                 DEC(PS[Z]); {-}
                 ARGH := TRUE;
                 END;
              IF CORD = 42 THEN
                 BEGIN
                 INC(PS[Z], 16); {*}
                 ARGH := TRUE;
                 END;
              IF CORD = 47 THEN
                 BEGIN
                 DEC(PS[Z], 16); {/}
                 ARGH := TRUE;
                 END;
              IF CORD = 13 THEN
                 BEGIN
                      IF Z = 0 THEN
                      BEGIN
                           GOTOXY(10,1);
                           WRITE ('   ');
                           GOTOXY(10,1);
                           READLN(PS[Z]);
                           ARGH:=TRUE;
                      END
                      ELSE
                      BEGIN
                           GOTOXY(42,1);
                           WRITE ('   ');
                           GOTOXY(42,1);
                           READLN(PS[Z]);
                           ARGH:=TRUE;
                      END;
                 END;
              IF PS[Z] < 0 THEN INC(PS[Z], MAX);
              IF PS[Z] > MAX THEN DEC(PS[Z], MAX);
              IF PS[0] = PS[1] THEN
              BEGIN
                   IF CORD = 13 THEN
                   BEGIN
                      INC(PS[Z], 1);
                      IF PS[Z] > MAX THEN DEC(PS[Z], MAX);
                   END;
                   IF CORD = 43 THEN
                   BEGIN
                      INC(PS[Z], 1);
                      IF PS[Z] > MAX THEN DEC(PS[Z], MAX);
                   END;
                   IF CORD = 45 THEN
                   BEGIN
                      DEC(PS[Z], 1);
                      IF PS[Z] < 0 THEN INC(PS[Z], MAX);
                   END;
              END;
              IF CORD = 9 THEN
                 BEGIN
                 ERASECURSOR;
                 INC(X, 16);
                 IF X > 32 THEN DEC(X, 32);
                 END;
              IF C = 'Q' THEN AUTOSTAT := NOT AUTOSTAT;
              IF C = 'W' THEN DEC(PEN);
              IF C = 'E' THEN INC(PEN);
              IF PEN > 4 THEN DEC(PEN, 3);
              IF PEN < 2 THEN INC(PEN, 3);
              IF CORD = 32 THEN WRITEPEN(X, Y);
              IF CORD = 8 THEN ERASEPEN(X, Y);
              IF C = 'C' THEN
                 BEGIN
                 MOVE (FONT[PS[Z]], FONT[MAX+1], 32);
                 END;
              IF C = 'V' THEN
                 BEGIN
                 MOVE (FONT[MAX+1], FONT[PS[Z]], 32);
                 ARGH:=TRUE;
                 END;
              IF C = 'S' THEN
              BEGIN
                 MOVE (FONT[PS[1]], FONT[MAX+1], 64);
                 MOVE (FONT[PS[0]], FONT[PS[1]], 64);
                 MOVE (FONT[MAX+1], FONT[PS[0]], 64);
                 ARGH:=TRUE;
              END;
              IF C = 'X' THEN
              BEGIN
                 FOR I := 1 TO 16 DO
                 BEGIN
                      FONT[PS[Z], (I*2)-1] := 0;
                      FONT[PS[Z], (I*2)] := 255;
                 END;
                 ARGH:=TRUE;
              END;
              IF C = 'A' THEN
              BEGIN
               OLDPEN := PEN;
               PEN := 3;
                IF Z = 0 THEN
                BEGIN
                 FOR I := 1 TO 8 DO
                  BEGIN
                  FOR J := 1 TO 16 DO
                   BEGIN
                    IF CBYTE(I,J) = 4 THEN
                     BEGIN
                      IF I > 1 THEN
                       BEGIN
                        IF CBYTE(I-1,J) = 2 THEN WRITEPEN(I-1, J);
                       END;
                      IF I < 8 THEN
                       BEGIN
                        IF CBYTE(I+1,J) = 2 THEN WRITEPEN(I+1, J);
                       END;
                      IF J > 1 THEN
                       BEGIN
                        IF CBYTE(I,J-1) = 2 THEN WRITEPEN(I,J-1);
                       END;
                      IF J < 16 THEN
                       BEGIN
                        IF CBYTE(I,J+1) = 2 THEN WRITEPEN(I,J+1);
                       END;
                     END;
                   END;
                  END;
                END
                ELSE
                BEGIN
                 FOR I := 9 TO 16 DO
                  BEGIN
                  FOR J := 1 TO 16 DO
                   BEGIN
                    IF CBYTE(I,J) = 4 THEN
                     BEGIN
                      IF I > 9 THEN
                       BEGIN
                        IF CBYTE(I-1,J) = 2 THEN WRITEPEN(I-1, J);
                       END;
                      IF I < 16 THEN
                       BEGIN
                        IF CBYTE(I+1,J) = 2 THEN WRITEPEN(I+1, J);
                       END;
                      IF J > 1 THEN
                       BEGIN
                        IF CBYTE(I,J-1) = 2 THEN WRITEPEN(I,J-1);
                       END;
                      IF J < 16 THEN
                       BEGIN
                        IF CBYTE(I,J+1) = 2 THEN WRITEPEN(I,J+1);
                       END;
                     END;
                   END;
                  END;
                END;
               ARGH := TRUE;
               PEN := OLDPEN;
              END;
              IF ARGH THEN INITSCR;
        END;
        IF AUTOSTAT THEN WRITEPEN(X, Y);
   END;
   ASSIGN (FOUT, fname);
   REWRITE (FOUT, 1);
   FOR I := 0 TO MAX DO
   BEGIN
        BLOCKWRITE (FOUT, FONT[I], 32);
   END;
   CLOSE (FOUT);
   TEXTCOLOR(7);
   CLRSCR;
   WRITELN ('font saved as ',fname,'!');
END.