{$X+}
{$S 65535}
program dump16te; {main}
{}

uses DOS, STRINGS;

CONST
   bufsize = 4096;

VAR
   BUF:array[0..bufsize] OF BYTE;
   EUCTABLE:array[0..2000, 0..1] OF BYTE;
   POINTERTABLE:array[0..94] of longint;
   INTERVALPHRASE:LONGINT;
   TOTALPHRASE:LONGINT;
   POFFSET:LONGINT;
   STRINGT:STRING;
   JLOOPS:LONGINT;
{
PROCEDURE READPOINTER;
VAR
 A:ARRAY[0..2] OF LONGINT;
 P,OLDP:LONGINT;
 I:byte;
 F:FILE;
BEGIN
 A[0] := 0;
 A[1] := 0;
 A[2] := 0;
 ASSIGN(F, 'LANGRISS.SMC');
 RESET(F, 1);
 SEEK(F, POFFSET);
 FOR I := 0 TO 94 DO
 BEGIN
 BLOCKREAD (F, A[0], 1);
 BLOCKREAD (F, A[1], 1);
 BLOCKREAD (F, A[2], 1);
 POINTERTABLE[I] := (A[2] * 65536) + (A[1] * 256) + A[0];
 END;
 CLOSE(F);
END;
}

PROCEDURE READEUC;

VAR
   F:FILE;
   B1, B2:BYTE;
   I:WORD;

BEGIN
     ASSIGN (F, 'LANGRISS.EUC');
     RESET (F, 1);
     I := 0;
     WHILE NOT EOF(F) DO
     BEGIN
     BLOCKREAD (F, B1, 1);
        IF B1 = 10 THEN
        BEGIN
        END
        ELSE
        BEGIN
        IF B1 = 13 THEN
        BEGIN
        END
        ELSE
         BEGIN
          BLOCKREAD (F, B2, 1);
          EUCTABLE[I, 0] := B1;
          EUCTABLE[I, 1] := B2;
          INC(I);
          END;
         END;
     END;
     {0-946}
     CLOSE(F);
END;

PROCEDURE PAUSEKEY;
VAR
  C:CHAR;
BEGIN
{  REPEAT
  UNTIL KEYPRESSED;

  REPEAT
     C := READKEY;
  UNTIL NOT KEYPRESSED;
  IF C = CHR(27) THEN HALT(1);
}
END;

PROCEDURE EMPTYKEYBUF;
VAR
A:CHAR;
BEGIN
{  REPEAT
     A := READKEY;
  UNTIL NOT KEYPRESSED}
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
CONST
     MYP:ARRAY[0..8] OF STRING=('<>'+chr(10), '<??01>', '<ch>', '<##>',
     '<p1#', '  ', '<wait>'+chr(10), '<clsr>', '<p2#');

     CBRK='>';
     quot='"';

VAR
   FIN:FILE;
   FOUT:FILE;
   B,B1,B2,B3:BYTE;
   I,J,K:LONGINT;
   T:LONGINT;
   S:STRING;
   CH:CHAR;
   LOOPS:LONGINT;
   LAST:BOOLEAN;
   BORED:BOOLEAN;
   PNT:ARRAY[0..1024] OF LONGINT;
   TEMPLI:ARRAY[0..4] OF LONGINT;
   CURPNT:LONGINT;
   WRITTEN:BOOLEAN;
   CBRK1, CBRK2, COMMA:CHAR;

BEGIN{2097664}
   CBRK1 := '(';
   CBRK2 := ')';
   COMMA := ',';
   ASSIGN (FIN, 'lungris2.bin');
   RESET (FIN, 1);

   ASSIGN (FOUT, FNAME);
   REWRITE (FOUT, 1);

   POFFSET := OFFS;

 FOR J := 1 TO JLOOPS DO
 BEGIN
   SEEK(FIN, POFFSET);
   blockread(FIN, PNT[0], 4);
   PNT[0] := ENDIAN(PNT[0]);
   LOOPS := (PNT[0] - POFFSET) DIV 4;

   FOR I := 1 TO LOOPS-1 DO
   BEGIN
        blockread(FIN, PNT[I], 4);
        PNT[I] := ENDIAN(PNT[I]);
   END;
   FOR I := 0 TO LOOPS-1 DO
   BEGIN
        seek(FIN, PNT[I]);
        bored := true;
        WHILE BORED DO
        BEGIN
             BLOCKREAD(FIN, BUF, 2);
{             WRITELN(BUF[0], ' ' , BUF[1]);}

             WRITTEN := FALSE;
             IF BUF[0] < 4 THEN
             BEGIN
                  K := (256*BUF[0]) + BUF[1];
                  IF K < 947 THEN
                  BEGIN
                       BLOCKWRITE(FOUT, EUCTABLE[K], 2);
                       WRITTEN := TRUE;
                  END;
             END;
             IF BUF[0] = 255 THEN
             BEGIN
                  IF BUF[1] = 255 THEN
                  BEGIN
                       BLOCKWRITE(FOUT, MYP[0, 1], 3);
                       BORED := FALSE;
                       WRITTEN := TRUE;
                  END;
                  IF BUF[1] = 254 THEN
                  BEGIN
                       BLOCKWRITE(FOUT, MYP[7, 1], 7);
                       BORED := FALSE;
                       WRITTEN := TRUE;
                  END;
             END;
             IF NOT WRITTEN THEN
             BEGIN
                  BLOCKWRITE(FOUT, CBRK1, 1);
                  STR(BUF[0], S);
                  BLOCKWRITE(FOUT, S[1], ORD(S[0]));
                  BLOCKWRITE(FOUT, COMMA, 1);
                  STR(BUF[1], S);
                  BLOCKWRITE(FOUT, S[1], ORD(S[0]));
                  BLOCKWRITE(FOUT, CBRK2, 1);
             END;
        END;
   END;
   POFFSET := FILEPOS(FIN);

 END;

   CLOSE (FOUT);
   CLOSE (FIN);
END;

BEGIN

{clrscr;}
writeln ('langrisser ii script dumper v1.0 by alamone.');
READEUC;
{READPOINTER;}
{READPHRASE;}
JLOOPS := 1;
TRANSLATE(4198, 'l2-1.euc'); {unused}
TRANSLATE(535186, 'l2-2.euc');
TRANSLATE(535458, 'l2-3.euc');
TRANSLATE(535838, 'l2-4.euc'); {unused?}
TRANSLATE(619052, 'l2-5.euc');
{660766 - item descriptions}
{TRANSLATE(661746, 'l2-oie.euc');} {items!!!! uses lookup at 660636}
TRANSLATE(668458, 'l2-6.euc');
{669584 - Scenario (4 char)}
{669594 - Total (5 char)}
{669606 - Turn (3 char)}
{669614 - "The name is decided"?}
{669654 - 8x8 to 16x16 conversion chart}
{682768 - intro/ending texts}

END.
