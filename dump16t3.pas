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
   LOOPS:LONGINT;

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



PROCEDURE TRANSLATE;
CONST
     MYP:ARRAY[0..8] OF STRING=('<>'+chr(10), '<??01>', '<ch>', '<##>',
     '<c#>', '\n'+chr(10), '<wait>'+chr(10), '<clsr>', '<p2#');

     CBRK='>';
     quot='"';

VAR
   FIN:FILE;
   FOUT:FILE;
   B,B1,B2,B3:BYTE;
   I,J,K,L,M:LONGINT;
   T:LONGINT;
   S:STRING;
   CH:CHAR;
   LAST:BOOLEAN;
   WRITTEN:BOOLEAN;
   TEMPLI:ARRAY[0..3] OF LONGINT;
   CBRK1, CBRK2, COMMA:CHAR;


BEGIN{2097664}
   CBRK1 := '(';
   CBRK2 := ')';
   COMMA := ',';

   ASSIGN (FIN, 'lungris2.bin');
   RESET (FIN, 1);
   SEEK(FIN, POFFSET);
   ASSIGN (FOUT, 'l2-oie3.euc');
   REWRITE (FOUT, 1);
{   WHILE NOT EOF(F) DO
   BEGIN

   END;
}

{   LOOPS := TRUNC((2097152 - POFFSET) DIV BUFSIZE);}

   for i := 1 to LOOPS do
{   for i := 1 to 10 do}
   begin
        blockread(FIN, buf, bufsize);
        for j := 0 to (bufsize div 2) - 1 do
        begin
             TEMPLI[0] := BUF[J*2];
             TEMPLI[1] := BUF[J*2+1];
             K := (TEMPLI[0] * 256) + TEMPLI[1];
             WRITTEN:=FALSE;
{             writeln(buf[j*2], ' ', buf[j*2+1], ' ', K);
             PAUSEKEY;}

             if buf[j*2] < 4 then
             begin
                  IF K < 947 THEN
                  BEGIN
                       blockwrite(FOUT, euctable[K], 2);
                       LAST := FALSE;
                       WRITTEN:=TRUE;
                  END;
             end;
             if buf[j*2] = 255 then
             begin
                  if buf[j*2+1] = 255 then
                  begin
                       IF NOT LAST THEN
                       BEGIN
                            blockwrite(FOUT, myp[0, 1], 3);
                            LAST := TRUE;
                            WRITTEN:=TRUE;
                       END;
                  end;
                  if buf[j*2+1] = 254 then
                  begin
                       blockwrite(FOUT, myp[5, 1], 3);
                       WRITTEN:=TRUE;
                  end;
                  if buf[j*2+1] = 253 then
                  begin
                       blockwrite(FOUT, myp[6, 1], 7);
                       WRITTEN:=TRUE;
                  end;
                  if buf[j*2+1] = 247 then
                  begin
                       inc(j);
                       TEMPLI[0] := BUF[J*2];
                       TEMPLI[1] := BUF[J*2+1];
                       L := (TEMPLI[0] * 256) + TEMPLI[1];
                       str(L, S);
                       blockwrite(FOUT, myp[4, 1], 3);
                       BLOCKWRITE(FOUT, s[1], ord(s[0]));
                       blockwrite(FOUT, myp[4, 4], 1);
                       WRITTEN:=TRUE;
                  end;
             end;
             IF NOT WRITTEN THEN
             BEGIN
                  BLOCKWRITE(FOUT, CBRK1, 1);
                  STR(TEMPLI[0], S);
                  BLOCKWRITE(FOUT, S[1], ORD(S[0]));
                  BLOCKWRITE(FOUT, COMMA, 1);
                  STR(TEMPLI[1], S);
                  BLOCKWRITE(FOUT, S[1], ORD(S[0]));
                  BLOCKWRITE(FOUT, CBRK2, 1);
             END;
        end;
   end;
   CLOSE (FOUT);
   CLOSE (FIN);
END;

BEGIN

{clrscr;}
writeln ('langrisser ii script dumper v1.0 by alamone.');

{POFFSET:=619520;} {NAME}
{POFFSET:=1599862;} {SCENARIO 2?}
{POFFSET:=1591384; {SCENARIO 1}
{POFFSET:=535242;} {SYSTEM}
{poffset := 660766;}
{poffset := 660636;}
{poffset := 661152;}
{poffset := 669550;}

loops := 3;
{poffset := 667418;}

{poffset := 682768;}
poffset := 1589610; {184db0;}
poffset := 1597632;
READEUC;
{READPOINTER;}
{READPHRASE;}
TRANSLATE;

END.
