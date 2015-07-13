{$X+}
{$S 65535}
program dumplt1;
uses DOS, STRINGS;
CONST
   bufsize = 4096;
var
   lookup:array[0..3, 0..255, 1..2] of byte;
   EUCTABLE:array[0..2000, 0..1] OF BYTE;
   BUF:array[0..bufsize] OF BYTE;
   INTERVALPHRASE:LONGINT;
   TOTALPHRASE:LONGINT;
   POFFSET:LONGINT;
   STRINGT:STRING;
   PNT:ARRAY[0..1024] OF LONGINT;
   PNT2:ARRAY[0..1024] OF LONGINT;
   NAMES:ARRAY[0..118] OF STRING;
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

PROCEDURE READEUC;

VAR
   F:FILE;
   B1, B2, DUMMY:BYTE;
   POSIT:LONGINT;
   I:WORD;
   BORED:BOOLEAN;
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
     ASSIGN(F, 'L2-NAMES.EUC');
     RESET(F, 1);
     POSIT := 0;
     FOR I := 0 TO 116 DO
     BEGIN
          BORED := TRUE;
          WHILE BORED DO
          BEGIN
               BLOCKREAD(F, B1, 1);
               IF B1 = 10 THEN
               BEGIN
               B2 := FILEPOS(F) - POSIT;
               SEEK(F, POSIT);
               BLOCKREAD(F, NAMES[I,1], B2 - 1);
               BLOCKREAD(F, DUMMY, 1);
               NAMES[I, 0] := CHR(B2-1);
               POSIT := FILEPOS(F);
               BORED := FALSE;
               END;
          END;
     END;
     CLOSE(F);

END;

PROCEDURE TRANSLATE(LOFFS:LONGINT; POFFS:LONGINT; MAXLINE:BYTE; FNAME:STRING);
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
   bored:boolean;
   LOOKUPBYTE:byte;
   LOOKUPBANK:byte;
   LOOPS:LONGINT;
   CURCURSOR:BYTE;
   NEXTLINE: BYTE;
BEGIN
   CBRK1 := '(';
   CBRK2 := ')';
   COMMA := ',';
   NEXTLINE := 10;

   ASSIGN (FIN, 'lungris2.bin');
   RESET (FIN, 1);
   SEEK(FIN, LOFFS);
   bored := true;
   lookupbyte := 0;
   lookupbank := 0;
   while bored do
   begin
        blockread(FIN, B1, 1);
        blockread(FIN, B2, 1);
        if B1 = $FF then
           begin
                if B2 = $FF then
                begin
                     bored := false;
                end;
           end
           else
           begin
                lookup[lookupbank, lookupbyte, 1] := B1;
                lookup[lookupbank, lookupbyte, 2] := B2;
                if lookupbyte = $FF then
                begin
                lookupbyte := 0;
                inc(lookupbank);
                end
                else
                begin
                inc(lookupbyte);
                end;
           end;
   end;
{   writeln(lookupbank,':',lookupbyte);}
   ASSIGN (FOUT, FNAME);
   REWRITE (FOUT, 1);

{   HALT(0);}

   CURCURSOR := 0;
   SEEK(FIN, POFFS);
   blockread(FIN, PNT[0], 4);
   PNT[0] := ENDIAN(PNT[0]);
   LOOPS := (PNT[0] - POFFS) DIV 4;

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
             blockread(FIN, buf, 2);
             TEMPLI[0] := LOOKUP[BUF[0], BUF[1], 1];
             TEMPLI[1] := LOOKUP[BUF[0], BUF[1], 2];
             K := (TEMPLI[0] * 256) + TEMPLI[1];
             WRITTEN:=FALSE;

             if buf[0] = 255 then
             begin
                  if buf[1] = 255 then
                  begin
                       IF NOT LAST THEN
                       BEGIN
                            blockwrite(FOUT, myp[0, 1], 3);
                            LAST := TRUE;
                            WRITTEN:=TRUE;
                            BORED := FALSE;
                       END;
                  end;
                  if buf[1] = 254 then
                  begin
                       blockwrite(FOUT, myp[5, 1], 3);
                       WRITTEN:=TRUE;
                       curcursor := 0;
                  end;
                  if buf[1] = 253 then
                  begin
                       blockwrite(FOUT, myp[6, 1], 7);
                       WRITTEN:=TRUE;
                  end;
                  if buf[1] = 247 then
                  begin
                       BLOCKREAD(FIN, BUF, 2);
                       TEMPLI[0] := BUF[0];
                       TEMPLI[1] := BUF[1];
                       L := (TEMPLI[0] * 256) + TEMPLI[1];
{                       str(L, S);
                       blockwrite(FOUT, myp[4, 1], 3);
                       BLOCKWRITE(FOUT, s[1], ord(s[0]));
                       blockwrite(FOUT, myp[4, 4], 1);
}
                       BLOCKWRITE(FOUT, NAMES[L, 1], ORD(NAMES[L, 0]));
                  end;
             end;
             if TEMPLI[0] < 4 then
             begin
                  IF NOT WRITTEN THEN
                  BEGIN

                  IF K < 947 THEN
                  BEGIN
                       blockwrite(FOUT, euctable[K], 2);
                       IF MAXLINE > 0 THEN
                       BEGIN
                       inc(CURCURSOR);
                       IF CURCURSOR = MAXLINE THEN
                       BEGIN
                            CURCURSOR := 0;
                            BLOCKWRITE(FOUT, NEXTLINE, 1);
                       END;
                       END;
                       LAST := FALSE;
                       WRITTEN:=TRUE;
                  END;

                  END;
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
   END;

   CLOSE (FOUT);
   CLOSE (FIN);
END;

PROCEDURE TRANSLATE2(LOFFS:LONGINT; POFFS:LONGINT; MAXLINE:BYTE; FNAME:STRING);
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
   bored:boolean;
   LOOKUPBYTE:byte;
   LOOKUPBANK:byte;
   LOOPS:LONGINT;
   CURCURSOR:BYTE;
   NEXTLINE: BYTE;
BEGIN
   CBRK1 := '(';
   CBRK2 := ')';
   COMMA := ',';
   NEXTLINE := 10;

   ASSIGN (FIN, 'lungris2.bin');
   RESET (FIN, 1);

   ASSIGN (FOUT, FNAME);
   REWRITE (FOUT, 1);

{   HALT(0);}

   CURCURSOR := 0;
   SEEK(FIN, POFFS);
   blockread(FIN, PNT[0], 4);
   PNT[0] := ENDIAN(PNT[0]);
   LOOPS := (PNT[0] - POFFS) DIV 4;

   FOR I := 1 TO LOOPS-1 DO
   BEGIN
        blockread(FIN, PNT[I], 4);
        PNT[I] := ENDIAN(PNT[I]);
   END;
   seek(fin, LOFFS);
   FOR I := 0 TO LOOPS-1 DO
   begin
        blockread(FIN, PNT2[I], 4);
        PNT2[I] := ENDIAN(PNT2[I]);
   end;
   FOR I := 0 TO LOOPS-1 DO
   BEGIN

   SEEK(FIN, PNT2[I]);
   bored := true;
   lookupbyte := 0;
   lookupbank := 0;
   while bored do
   begin
        blockread(FIN, B1, 1);
        blockread(FIN, B2, 1);
        if B1 = $FF then
           begin
                if B2 = $FF then
                begin
                     bored := false;
                end;
           end
           else
           begin
                lookup[lookupbank, lookupbyte, 1] := B1;
                lookup[lookupbank, lookupbyte, 2] := B2;
                if lookupbyte = $FF then
                begin
                lookupbyte := 0;
                inc(lookupbank);
                end
                else
                begin
                inc(lookupbyte);
                end;
           end;
   end;
{   writeln(lookupbank,':',lookupbyte);}

        seek(FIN, PNT[I]);
        bored := true;
        WHILE BORED DO
        BEGIN
             blockread(FIN, buf, 2);
             TEMPLI[0] := LOOKUP[BUF[0], BUF[1], 1];
             TEMPLI[1] := LOOKUP[BUF[0], BUF[1], 2];
             K := (TEMPLI[0] * 256) + TEMPLI[1];
             WRITTEN:=FALSE;

             if buf[0] = 255 then
             begin
                  if buf[1] = 255 then
                  begin
                       IF NOT LAST THEN
                       BEGIN
                            blockwrite(FOUT, myp[0, 1], 3);
                            LAST := TRUE;
                            WRITTEN:=TRUE;
                            BORED := FALSE;
                       END;
                  end;
                  if buf[1] = 254 then
                  begin
                       blockwrite(FOUT, myp[5, 1], 3);
                       WRITTEN:=TRUE;
                       curcursor := 0;
                  end;
                  if buf[1] = 253 then
                  begin
                       blockwrite(FOUT, myp[6, 1], 7);
                       WRITTEN:=TRUE;
                  end;

                  if buf[1] = 247 then
                  begin
                       BLOCKREAD(FIN, BUF, 2);
                       TEMPLI[0] := BUF[0];
                       TEMPLI[1] := BUF[1];
                       L := (TEMPLI[0] * 256) + TEMPLI[1];
{                       str(L, S);
                       blockwrite(FOUT, myp[4, 1], 3);
                       BLOCKWRITE(FOUT, s[1], ord(s[0]));
                       blockwrite(FOUT, myp[4, 4], 1);
}
                       BLOCKWRITE(FOUT, NAMES[L, 1], ORD(NAMES[L, 0]));
                       WRITTEN:=TRUE;
                  end;

             end;
             if TEMPLI[0] < 4 then
             begin
                  IF NOT WRITTEN THEN
                  BEGIN

                  IF K < 947 THEN
                  BEGIN
                       blockwrite(FOUT, euctable[K], 2);
                       IF MAXLINE > 0 THEN
                       BEGIN
                       inc(CURCURSOR);
                       IF CURCURSOR = MAXLINE THEN
                       BEGIN
                            CURCURSOR := 0;
                            BLOCKWRITE(FOUT, NEXTLINE, 1);
                       END;
                       END;
                       LAST := FALSE;
                       WRITTEN:=TRUE;
                  END;

                  END;
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
   END;

   CLOSE (FOUT);
   CLOSE (FIN);
END;


{
item list
lookup (null terminated) - 660636
pointer table to text - 661746
8 max

item descript
lookup (null terminated) - 660766
pointer table to text- 662892
9x5 max

scenario prologues
pointer table to lookups - 635644
pointer table to texts - 642940
18x* max

win/lose
pointer table to lookups - 624326
pointer table to texts - 626042
16x7 max

}

begin
     readeuc;
     translate(660636, 661746, 0, 'l2-10.euc');
     translate(660766, 662892, 9, 'l2-11.euc');
     translate2(635644, 642940, 0, 'l2-12.euc');
     translate2(624326, 626042, 16, 'l2-13.euc');

     {remember, 1st pointer location is last byte in pointer table}
end.