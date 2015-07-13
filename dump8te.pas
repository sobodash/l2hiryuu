{$X+}
program dump8te;
uses DOS, STRINGS;
var
   EUCTABLE:array[0..255, 0..1] OF BYTE;
   EUCTABLEQUOTE:array[0..45, 0..1] of byte;
   EUCTABLESQUARE:array[0..45, 0..1] of byte;
   TOTPHRASE:LONGINT;
   STRINGT:STRING;

PROCEDURE READEUC;

VAR
   F:FILE;
   B1, B2:BYTE;
   I:INTEGER;

BEGIN
     ASSIGN (F, 'LANG8x8.EUC');
     RESET (F, 1);

     I := -1;
     WHILE I < 255 DO
     BEGIN
     BLOCKREAD (F, B1, 1);
     IF B1 = 13 THEN
     BEGIN
     BLOCKREAD (F, B2, 1); {10}
     END
     ELSE
     BEGIN
          INC(I);
          BLOCKREAD (F, B2, 1);
          EUCTABLE[I, 0] := B1;
          EUCTABLE[I, 1] := B2;
     END;
     END;
     I := -1;
     WHILE I < 45 DO
     BEGIN
     BLOCKREAD (F, B1, 1);
     IF B1 = 13 THEN
     BEGIN
     BLOCKREAD (F, B2, 1); {10}
     END
     ELSE
     BEGIN
          INC(I);
          BLOCKREAD (F, B2, 1);
          EUCTABLEQUOTE[I, 0] := B1;
          EUCTABLEQUOTE[I, 1] := B2;
     END;
     END;
     I := -1;
     WHILE I < 45 DO
     BEGIN
     BLOCKREAD (F, B1, 1);
     IF B1 = 13 THEN
     BEGIN
     BLOCKREAD (F, B2, 1); {10}
     END
     ELSE
     BEGIN
          INC(I);
          BLOCKREAD (F, B2, 1);
          EUCTABLESQUARE[I, 0] := B1;
          EUCTABLESQUARE[I, 1] := B2;
     END;
     END;
     CLOSE(F);
END;
FUNCTION ENDIAN(LI:LONGINT):LONGINT;
VAR
   LARRAY:ARRAY[0..3] OF BYTE;
   XARRAY:ARRAY[0..3] OF LONGINT;
BEGIN
     LARRAY[0] := LI SHR 24;
     LARRAY[1] := LI SHR 16;
     LARRAY[2] := LI SHR 8;
     LARRAY[3] := LI;
     XARRAY[0] := LARRAY[0];
     XARRAY[1] := LARRAY[1];
     XARRAY[2] := LARRAY[2];
     XARRAY[3] := LARRAY[3];
     ENDIAN := XARRAY[0] + (XARRAY[1] SHL 8) + (XARRAY[2] SHL 16) + (XARRAY[3] SHL 24);
END;
PROCEDURE TRANSLATE(OFFS:LONGINT; FNAME:STRING);
VAR
   F:FILE;
   OUTF:FILE;
   B,B1,B2,B3:BYTE;
   BARRAY:ARRAY[0..3] OF BYTE;
   LARRAY:ARRAY[0..3] OF LONGINT;
   I,J,K,L:INTEGER;
   T,FPOS:LONGINT;
   S:ARRAY[0..20] OF CHAR;
   ST:STRING[10];
   CH:CHAR;
   BLEH,BLEH2, BORED:BOOLEAN;
   pointno:LONGINT;
   pointval:LONGINT;

begin
   ASSIGN (F, 'lungris2.bin');
   RESET (F, 1);
   FPOS := OFFS;
   ASSIGN (OUTF, FNAME);
   REWRITE (OUTF, 1);
   L := 0;
   totphrase:=0;
   SEEK(F, FPOS);
   BLOCKREAD(F, POINTVAL, 4);
   POINTVAL := ENDIAN(POINTVAL);
   POINTNO := (POINTVAL - FPOS) DIV 4;

   FOR I := 1 TO POINTNO DO
   BEGIN
        SEEK(F, FPOS);
        INC(FPOS, 4);
        BLOCKREAD(F, POINTVAL, 4);
        POINTVAL := ENDIAN(POINTVAL);

   BORED := FALSE;
   WHILE NOT BORED DO
   BEGIN
        SEEK(F, POINTVAL);
        BLOCKREAD (F, B1, 1);
        BLOCKREAD (F, B2, 1);
        SEEK(F, POINTVAL);
        BLEH:=FALSE;
        IF B2 = $DE THEN BEGIN
           DEC(B1, $B0);
           BLOCKWRITE (OUTF, EUCTABLEQUOTE[B1], 2);
           BLEH:=TRUE;
           INC(POINTVAL, 2);
           INC(L, 2);
        END;
        IF B2 = $DF THEN BEGIN
           DEC(B1, $B0);
           BLOCKWRITE (OUTF, EUCTABLESQUARE[B1], 2);
           BLEH:=TRUE;
           INC(POINTVAL, 2);
           INC(L, 2);
        END;
        IF NOT BLEH THEN BEGIN
           BLOCKWRITE (OUTF, EUCTABLE[B1], 2);
           INC(L);
           IF B1 = $FF THEN BEGIN
              DEC(L);
              B3 := 10;
              BLOCKWRITE (OUTF, B3, 1);
              B3 := 10;
              BLOCKWRITE (OUTF, B3, 1);
              INC(TOTPHRASE);
              L := 0;
              BORED := TRUE;
           END;
           INC(POINTVAL, 1);
        END;
   END;
   END;

   CLOSE (OUTF);
   writeln (FNAME, ' - ',totphrase);
   totphrase:=0;

   CLOSE (F);

end;

BEGIN
{clrscr;}
writeln ('Begin lungris2.bin -> dump8-?.euc');

READEUC;
{386774: start of troop & classes text pointers
394084: start of item text pointers
399592: start of name text pointers}
TRANSLATE(386774, 'dump8-1.euc');
TRANSLATE(394084, 'dump8-2.euc');
TRANSLATE(399592, 'dump8-3.euc');

end.
