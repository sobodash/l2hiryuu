{$S 65535}
PROGRAM readpnt;
{uses crt;}
const
   bufsize = 16384;
var
   fin,fout:file;
   i,j,k:longint;
   table1:array[0..77] of longint; {master table}
   table2:array[0..77] of longint; {text locations}
   s:string;
   b:byte;
   BUF:array[0..bufsize] OF byte;
   EUCTABLE:array[0..2000, 0..1] OF BYTE;
   poz,poz2,phrases,blocksize,pointer1:longint;
   hiword,loword:word;
   bored:boolean;
   dastring:array[0..3] of byte;

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
procedure writehex(b:byte);
var
   hi,lo:byte;
begin
     hi := b shr 4;
     lo := b mod 16;
     if hi < 10 then
     begin
        write(hi);
     end
     else
     begin
        if hi = 10 then write('A');
        if hi = 11 then write('B');
        if hi = 12 then write('C');
        if hi = 13 then write('D');
        if hi = 14 then write('E');
        if hi = 15 then write('F');
     end;
     if lo < 10 then
     begin
        write(lo);
     end
     else
     begin
        if lo = 10 then write('A');
        if lo = 11 then write('B');
        if lo = 12 then write('C');
        if lo = 13 then write('D');
        if lo = 14 then write('E');
        if lo = 15 then write('F');
     end;
end;





PROCEDURE TRANSLATE;
CONST
     MYP:ARRAY[0..8] OF STRING=('<>'+chr(10), '<??01>', '<ch>', '<##>',
     '<c#>', '\n'+chr(10), '<wait>'+chr(10), '<clsr>', '<p2#');

     CBRK='>';
     quot='"';

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
   CBRK1, CBRK2, COMMA:CHAR;
   bored:boolean;
BEGIN{2097664}
   CBRK1 := '(';
   CBRK2 := ')';
   COMMA := ',';

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
                       blockwrite(FOUT, myp[5, 1], 3);
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
                       blockwrite(FOUT, myp[4, 1], 3);
                       BLOCKWRITE(FOUT, s[1], ord(s[0]));
                       blockwrite(FOUT, myp[4, 4], 1);
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
   READEUC;

   assign(fin, 'lungris2.bin');
   reset(fin, 1);
   seek(fin, $18005e);

   for i := 0 to 77 do
   begin
        seek(fin, $18005e + (i * 4));
        blockread(fin, table1[i], 4);
        table1[i] := endian(table1[i]);
   end;

   table2[47] := $184858;

{   for i := 47 to 76 do} {30  1 - 26 , ?1 - ?4}

   for i := 47 to 47 do
   begin

   STR(I - 46, S);
   IF I < 10 THEN S := '0'+S;
   S := 'l2-d'+S+'j.euc';
   assign (FOUT, S);
   rewrite(FOUT, 1);

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
                                       writeln (pointer1);
                                  end;
                             end;
                        end;
                     end;
             end;
             seek(fin, poz);
             translate;
             inc(phrases);
             if poz = table1[i+1] then bored := false;
        end;

   CLOSE(fout);

   end;

   close(fin);

end.