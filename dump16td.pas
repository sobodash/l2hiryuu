{$S 65535}
PROGRAM DUMP16TD;
{uses crt;}
const
   bufsize = 16384;
   MYP:ARRAY[0..8] OF STRING=('<>'+chr(10), '<??01>', '<ch>', '<##>',
   '<c#>', '\n'+chr(10), '<wait>'+chr(10), '<clsr>', '<p2#');
   CBRK='>';
   quot='"';
   MAXSCEN = 77;
var
   fin,fout:file;
   i,j,k:longint;
   table1:array[0..MAXSCEN] of longint; {master table}
   table2:array[0..MAXSCEN] of longint; {text locations}
   s,S2:string;
   b:byte;
   BUF:array[0..bufsize] OF byte;
   EUCTABLE:array[0..1500, 0..1] OF BYTE;
   poz,poz2,blocksize,pointer1:longint;
   hiword,loword,phrases:word;
   bored:boolean;
   dastring:array[0..3] of byte;
   NAMES:ARRAY[0..118] OF STRING[18];
   PORTRAITS:ARRAY[0..211] OF STRING[48];
   b1,b2,b3,b4,b5:byte;
   lenstr:byte;
   CBRK1, CBRK2, COMMA:CHAR;

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
     close(F);
     ASSIGN(F, 'L2-PORTS.EUC');
     RESET(F, 1);
     POSIT := 0;
     FOR I := 0 TO 211 DO
     BEGIN
          blockread(F, BUF, 5);
          inc(POSIT, 5);
          BORED := TRUE;
          WHILE BORED DO
          BEGIN
               BLOCKREAD(F, B1, 1);
               IF B1 = 10 THEN
               BEGIN
               B2 := FILEPOS(F) - POSIT;
               SEEK(F, POSIT);
               BLOCKREAD(F, PORTRAITS[I,1], B2 - 1);
               BLOCKREAD(F, DUMMY, 1);
               PORTRAITS[I, 0] := CHR(B2-1);
               PORTRAITS[I, 1] := CHR(ORD(PORTRAITS[I, 1]) - 32);
               POSIT := FILEPOS(F);
               BORED := FALSE;
               END;
          END;
     END;
     close(F);


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
function convhex(b:byte):string;
var
   hi,lo:byte;
   f,s:byte;
begin
     hi := b shr 4;
     lo := b mod 16;
     if hi < 10 then
     begin
        f := 48 + hi;
     end
     else
     begin
        f := 55 + hi;
     end;
     if lo < 10 then
     begin
        s := 48 + lo;
     end
     else
     begin
        s := 55 + lo;
     end;
     convhex := chr(f) + chr(s);
end;





PROCEDURE TRANSLATE;
VAR
   B,B1,B2,B3:BYTE;
   I,J,M:LONGINT;
   K,L:word;
   T:LONGINT;
   S:STRING;
   CH:CHAR;
   LAST:BOOLEAN;
   WRITTEN:BOOLEAN;
   TEMPLI:ARRAY[0..3] OF LONGINT;
   bored:boolean;
BEGIN{2097664}
   BORED := TRUE;
   WHILE BORED DO
   BEGIN
             BLOCKREAD(FIN, K, 2);
             INC(POZ, 2);
             K := SWAP(K);
             WRITTEN:=FALSE;
             if HI(K) < 4 then
             begin
                  IF K < 947 THEN
                  BEGIN
                       blockwrite(FOUT, euctable[K], 2);
                       LAST := FALSE;
                       WRITTEN:=TRUE;
                  END;
             end;
             if HI(K) = 255 then
             begin
                  if LO(K) = 255 then
                  begin
                       IF NOT LAST THEN
                       BEGIN
                            blockwrite(FOUT, myp[0, 1], 3);
                            LAST := TRUE;
                            WRITTEN:=TRUE;
                            BORED := FALSE;
                       END;
                  end;
                  if LO(K) = 254 then
                  begin
{                       blockwrite(FOUT, myp[5, 1], 3);}
                       blockwrite(FOUT, myp[5, 3], 1);
                       WRITTEN:=TRUE;
                  end;
                  if LO(K) = 253 then
                  begin
                       blockwrite(FOUT, myp[6, 1], 7);
                       WRITTEN:=TRUE;
                  end;
                  if LO(K) = 247 then
                  begin
                       BLOCKREAD(FIN, L, 2);
                       INC(POZ, 2);
                       L := SWAP(L);
                       str(L, S);
{
                       blockwrite(FOUT, myp[4, 1], 3);
                       BLOCKWRITE(FOUT, s[1], ord(s[0]));
                       blockwrite(FOUT, myp[4, 4], 1);
}
                       BLOCKWRITE(FOUT, NAMES[L, 1], ORD(NAMES[L, 0]));

                       WRITTEN:=TRUE;
                  end;
             end;
             IF NOT WRITTEN THEN
             BEGIN
                  BLOCKWRITE(FOUT, CBRK1, 1);
                  STR(HI(L), S);
                  BLOCKWRITE(FOUT, S[1], ORD(S[0]));
                  BLOCKWRITE(FOUT, COMMA, 1);
                  STR(LO(L), S);
                  BLOCKWRITE(FOUT, S[1], ORD(S[0]));
                  BLOCKWRITE(FOUT, CBRK2, 1);
             END;
   end;

END;








begin

   writeln('Dumping Langrisser 2 Script.');
   READEUC;
             b1 := ord('[');
             b2 := ord(']');
             b3 := 32;
             b4 := 10;
             b5 := ord(':');
   CBRK1 := '(';
   CBRK2 := ')';
   COMMA := ',';

   assign(fin, 'lungris2.bin');
   reset(fin, 1);
   seek(fin, $18005e);

   for i := 0 to MAXSCEN do
   begin
        seek(fin, $18005e + (i * 4));
        blockread(fin, table1[i], 4);
        table1[i] := endian(table1[i]);
   end;

   table2[47] := $184858; {1}
   table2[48] := 1599630; {2}
   table2[49] := 1607744; {3}
   table2[50] := 1614790; {4}
   table2[51] := 1623882; {5}
   table2[52] := 1629496; {6}
   table2[53] := 1636610; {7}
   table2[54] := 1643662; {8}
   table2[55] := 1652008; {9}
   table2[56] := 1662254; {10}
   table2[57] := 1668858; {11}
   table2[58] := 1676222; {12}
   table2[59] := 1683290; {13}
   table2[60] := 1691638; {14}
   table2[61] := 1701886; {15}
   table2[62] := 1708244; {16}
   table2[63] := 1714066; {17}
   table2[64] := 1722666; {18}
   table2[65] := 1729750; {19}
   table2[66] := 1736322; {20}
   table2[67] := 1743622; {21}
   table2[68] := 1749502; {22}
   table2[69] := 1763522; {23}
   table2[70] := 1767780; {24}
   table2[71] := 1772028; {25}
   table2[72] := 1778958; {26} {+2?}
   table2[73] := 1785450; {27}
   table2[74] := 1793000; {?1}
   table2[75] := 1797996; {?2}
   table2[76] := 1801080; {?3}
   table2[77] := 1804334; {?4}

   for i := 47 to MAXSCEN do
   begin

   STR(I - 46, S);
   IF I - 46 < 10 THEN S := '0'+S;
   S2 := 'l2-d'+S+'j.euc';
   assign (FOUT, S2);
   rewrite(FOUT, 1);
   S := '<Scenario '+S+'>'+CHR(10);
   blockwrite(fout, S[1], ORD(S[0]));

        poz := table2[i];
        phrases := 0;
        bored:=true;
        pointer1 := 0;

        while bored do
        begin

             {find the pointer to current position}
             seek(fin, table1[i]);
             blocksize := table2[i] - table1[i];
             blockread(fin, buf, blocksize);
             dastring[0] := poz shr 24;
             dastring[1] := (poz shl 8) shr 24;
             dastring[2] := (poz shl 16) shr 24;
             dastring[3] := (poz shl 24) shr 24;
             for j := 0 to (blocksize div 2) do
             begin
                  if buf[j * 2] = dastring[0] then
                     begin
                     if buf[(j * 2) + 1] = dastring[1] then
                        begin
                             if buf[(j * 2) + 2] = dastring[2] then
                             begin
                                  if buf[(j * 2) + 3] = dastring[3] then
                                  begin
                                       pointer1 := table1[i] + (j * 2);
                                       j := (blocksize div 2);
                                  end;
                             end;
                        end;
                     end;
             end;


             seek(fin, pointer1 - 4);
             blockread(fin, dastring, 4);
             {2?, name, portrait, 0/1?}
             blockwrite(fout, b1, 1);
             s := convhex(hi(phrases));
             blockwrite(fout, s[1], 2);
             s := convhex(lo(phrases));
             blockwrite(fout, s[1], 2);
             blockwrite(fout, b5, 1);
             dec(pointer1, 4);
             s := convhex(pointer1 shr 24);
             blockwrite(fout, s[1], 2);
             s := convhex((pointer1 shl 8) shr 24);
             blockwrite(fout, s[1], 2);
             s := convhex((pointer1 shl 16) shr 24);
             blockwrite(fout, s[1], 2);
             s := convhex((pointer1 shl 24) shr 24);
             blockwrite(fout, s[1], 2);
             blockwrite(Fout, b5, 1);
             s := convhex(dastring[0]);
             blockwrite(fout, s[1], 2);
             s := convhex(dastring[1]);
             blockwrite(fout, s[1], 2);
             s := convhex(dastring[2]);
             blockwrite(fout, s[1], 2);
             s := convhex(dastring[3]);
             blockwrite(fout, s[1], 2);
             blockwrite(fout, b3, 1);
             if dastring[2] < 212 then
               begin
                 s := portraits[dastring[2]];
                 blockwrite(fout, s[1], ord(s[0]));
               end;
             blockwrite(fout, b2, 1);
             blockwrite(fout, b4, 1);
             s := names[dastring[1]];
             lenstr := ord(s[0]);
             if lenstr < 9 then
             begin {full tile}
                   lenstr := lenstr * 2;
             end
             else
             begin {dual tile}
                   if lenstr mod 2 > 0 then inc(lenstr, 1);
             end;
             {lenstr is now equal to dual tiles #}
             lenstr := lenstr - ord(s[0]);
             {lenstr is now equal to necessary white space}
             blockwrite(fout, s[1], ord(s[0]));
             {
             for j := 1 to lenstr do
             begin
                  blockwrite(fout, b3, 1);
             end;
             blockwrite(fout, b3, 1);
             }
             blockwrite(fout, b5, 1);

             seek(fin, poz);
             translate;

             blockwrite(fout, b4, 1);

             inc(phrases);
             IF I = MAXSCEN THEN IF PHRASES = $2c THEN bored := false;
             if poz = table1[i+1] then bored := false;

        end;
        writeln ('Scenario ', i - 46, ': ', phrases - 1, ' strings.');

   CLOSE(fout);

   end;

   close(fin);
   writeln('done!');

end.