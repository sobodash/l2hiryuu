{$S 65535}
program conv16_L;
{uses crt;}
const
   powa:array[1..8] of byte=(128,64,32,16,8,4,2,1);
   bufsize=32768;
var
   valff:byte;
   bored, bored2, bored3, ignore:boolean;
   i,j,k,l,filepoz:longint;
   code:integer;
   b,z:byte;
   test:byte;
   dual:array[0..1] of byte;
   dualpos:byte;
   single:byte;
   count:array[0..255] of longint;
   lookup:array[0..255] of byte;
   compare1,compare2:string;
   bufin, bufout:array[0..1000] of byte;
   bufcmd:string;
   s:string;
   fin, fout, fout2, fout3, fout4, fpnt:file;
   ftext:text;
   position:longint;
   linewidth:byte;
   allready:boolean;
   hexbank:byte;
   currentpoz:longint;
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
PROCEDURE CHECKABORT;
var
   c:char;
BEGIN
{   while keypressed do
   begin
        c := readkey;
        if ord(c) = 27 then halt(1);
   end;}
END;
procedure writepointer;
begin
     position := endian(position); {convert endian}
     blockwrite(fout2, position, 4); {MOTOROLA}
     position := endian(position); {convert back}
end;
procedure special(sbyte:byte);
     begin
          blockwrite(fout, valff, 1);
          blockwrite(fout, sbyte, 1);
          inc(position, 2);
          if sbyte = $ff then writepointer;
     end;
begin
   valff := $ff;
   {first must sift thru .EUCs to find used asciis, then create look-up
    tables}

   write ('Converting lookup EUC files');
   {retrieve position}
   assign (fout4, 'position.dat');
   reset (fout4, 1);
   blockread(fout4, position, 4);
   seek(fout4, 0);

   assign (fout, 'data0001.dat');
   reset (fout, 1);
   seek(fout, filesize(fout));

   {1 - data, 2 - pointerTEXT(P), 3 - pointerLOOKUP(L), 4 - position}

     for i := 0 to 1 do
     begin
          write('.');
          checkabort;
          str(i, s);
          checkfile('l2-1'+s+'e.euc');
          assign (fin, 'l2-1'+s+'e.euc');
          reset(fin, 1);
          seek(fin, 0);
          bored := true;
          filepoz := 0;
          while bored do
          begin
               blockread(fin, b, 1);
               inc(filepoz);
               ignore := false;
               if b < 32 then ignore := true; {ignore misc, japanese}
               if b > 127 then
               begin {set EUC as space}
                  blockread(fin, b, 1);
                  inc(filepoz);
                  b := 32;
               end;
               if b = 60 then {<}
               begin {process <> area}
                    bored2 := true;
                    k := 0;
                    while bored2 do
                    begin
                         blockread(fin, b, 1);
                         inc(filepoz);
                         inc(k);
                         bufcmd[k] := chr(b);
                         if b = 62 then {>}
                         begin
                              bored2 := false;
                              bufcmd[0] := chr(k-1);
                         end;
                    end;
{               write ('<',bufcmd,'>');}

               end

               else

               begin {not <> area}
                    if b = 92 then begin {\}
                         blockread(fin, b, 1);
                         inc(filepoz);
                         if b = 110 then {n?}
                         begin
                              ignore := true; {ignore \n}
{                              b := 92;}
                         end
                         else
                         begin {?}
                              ignore := true;
                              b := 32;
                         end;
                    end;

                    if ignore = false then
                    begin {normal english}
                          inc(count[b]);
                    end;
               end;
               if filepoz = filesize(fin) then bored := false;
          end;


          assign (fout3, 'pnt1'+s+'l.dat');
          rewrite(fout3, 1);
          k := 0;
          for j := 0 to 255 do
          begin
               if count[j] > 0 then
               begin
                    lookup[j] := k;
                    inc(k);
                    hexbank := $73;
                    blockwrite(fout3, hexbank, 1);
                    blockwrite(fout3, j, 1);
               end;
          end;
          hexbank := $ff;
          blockwrite(fout3, hexbank, 1);
          blockwrite(fout3, hexbank, 1);
          close(fout3);

          seek(fin, 0);
          filepoz := 0;
          assign (fout2, 'pnt1'+s+'p.dat');
          rewrite(fout2, 1);
          writepointer;
          filepoz := 0;

          bored:=true;
          seek(fin, filepoz); {seek to 0 again}
          while bored do
          begin
               blockread(fin, b, 1);
               inc(filepoz);
               ignore := false;
               if b < 32 then ignore := true; {ignore misc, japanese}
               if b > 127 then
               begin {set EUC as space}
                  blockread(fin, b, 1);
                  inc(filepoz);
                  b := 32;
               end;
               if b = 60 then {<}
               begin {process <> area}
                    bored2 := true;
                    k := 0;
                    while bored2 do
                    begin
                         blockread(fin, b, 1);
                         inc(filepoz);
                         inc(k);
                         bufcmd[k] := chr(b);
                         if b = 62 then {>}
                         begin
                              bored2 := false;
                              bufcmd[0] := chr(k-1);
                         end;
                    end;
{               write ('<',bufcmd,'>');}

                    if length(bufcmd) = 0 then {FF - <>}
                    begin
                         special($FF);
                    end;
                    if bufcmd = 'wait' then {FE - WAIT}
                       begin
                            special($FD);
                       end;
               end

               else

               begin {not <> area}
                    if b = 92 then begin {\}
                         blockread(fin, b, 1);
                         inc(filepoz);
                         if b = 110 then {n?}
                         begin
                              ignore := true; {ignore \n}
{                              b := 92;}
                              special(254);
                         end
                         else
                         begin {?}
                              ignore := true;
                              b := 32;
                         end;
                    end;

                    if ignore = false then
                    begin {normal english}
                          hexbank := $00;
                          blockwrite(fout, hexbank, 1);
                          blockwrite(fout, lookup[b], 1);
                          inc(position, 2);
                    end;
               end;
               if filepoz = filesize(fin) then bored := false;
          end;
          close(fin);
          seek(fout2, filesize(fout2) - 4);
          truncate(fout2);
          close(fout2);
     end;
{     inc(position); {final wrapup}




















   {1 - data, 2 - pointerTEXT(P), 3 - pointerLOOKUP(L), 4 - position}

     for i := 2 to 3 do
     begin
          write('.');
          checkabort;
          str(i, s);
          checkfile('l2-1'+s+'e.euc');
          assign (fin, 'l2-1'+s+'e.euc');
          reset(fin, 1);
          assign (fout3, 'pnt1'+s+'l.dat');
          rewrite(fout3, 1);
          assign (fout2, 'pnt1'+s+'p.dat');
          rewrite(fout2, 1);
          filepoz := 0;

bored3 := true;
while bored3 do
begin

          seek(fin, filepoz);
          currentpoz := filepoz;
          for j := 0 to 255 do
          begin
               count[j] := 0;
          end;
          bored := true;
          while bored do
          begin
               blockread(fin, b, 1);
               inc(filepoz);
               ignore := false;
               if b < 32 then ignore := true; {ignore misc, japanese}
               if b > 127 then
               begin {set EUC as space}
                  blockread(fin, b, 1);
                  inc(filepoz);
                  b := 32;
               end;
               if b = 60 then {<}
               begin {process <> area}
                    bored2 := true;
                    k := 0;
                    while bored2 do
                    begin
                         blockread(fin, b, 1);
                         inc(filepoz);
                         inc(k);
                         bufcmd[k] := chr(b);
                         if b = 62 then {>}
                         begin
                              bored2 := false;
                              bufcmd[0] := chr(k-1);
                         end;
                    end;
                    if length(bufcmd) = 0 then {FF - <>}
                    begin
                         bored := false;
                    end;

{               write ('<',bufcmd,'>');}

               end

               else

               begin {not <> area}
                    if b = 92 then begin {\}
                         blockread(fin, b, 1);
                         inc(filepoz);
                         if b = 110 then {n?}
                         begin
                              ignore := true; {ignore \n}
{                              b := 92;}
                         end
                         else
                         begin {?}
                              ignore := true;
                              b := 32;
                         end;
                    end;

                    if ignore = false then
                    begin {normal english}
                          inc(count[b]);
                    end;
               end;
               if filepoz = filesize(fin) then bored := false;
          end;

          position := endian(position); {convert endian}
          blockwrite(fout3, position, 4); {lookup pointer}
          position := endian(position); {convert back}

          k := 0;
          for j := 0 to 255 do
          begin
               if count[j] > 0 then
               begin
                    lookup[j] := k;
                    inc(k);
                    hexbank := $73;
                    blockwrite(fout, hexbank, 1);
                    blockwrite(fout, j, 1);
                    inc(position, 2);
               end;
          end;
          hexbank := $ff;
          blockwrite(fout, hexbank, 1);
          blockwrite(fout, hexbank, 1);
          inc(position, 2);

          writepointer; {text pointer}
          bored:=true;
          filepoz := currentpoz;
          seek(fin, filepoz); {seek to begin of area}
          while bored do
          begin
               blockread(fin, b, 1);
               inc(filepoz);
               ignore := false;
               if b < 32 then ignore := true; {ignore misc, japanese}
               if b > 127 then
               begin {set EUC as space}
                  blockread(fin, b, 1);
                  inc(filepoz);
                  b := 32;
               end;
               if b = 60 then {<}
               begin {process <> area}
                    bored2 := true;
                    k := 0;
                    while bored2 do
                    begin
                         blockread(fin, b, 1);
                         inc(filepoz);
                         inc(k);
                         bufcmd[k] := chr(b);
                         if b = 62 then {>}
                         begin
                              bored2 := false;
                              bufcmd[0] := chr(k-1);
                         end;
                    end;
{               write ('<',bufcmd,'>');}

                    if length(bufcmd) = 0 then {FF - <>}
                    begin
{                        special($FF);} {don't want to write pointer}
                         valff := $ff;
                         blockwrite(fout, valff, 1);
                         blockwrite(fout, valff, 1);
                         inc(position, 2);
                         bored := false;
                    end;
                    if bufcmd = 'wait' then {FE - WAIT}
                       begin
                            special($FE);
                       end;
               end

               else

               begin {not <> area}
                    if b = 92 then begin {\}
                         blockread(fin, b, 1);
                         inc(filepoz);
                         if b = 110 then {n?}
                         begin
                              ignore := true; {ignore \n}
{                              b := 92;}
                              special(254);
                         end
                         else
                         begin {?}
                              ignore := true;
                              b := 32;
                         end;
                    end;

                    if ignore = false then
                    begin {normal english}
                          hexbank := $00;
                          blockwrite(fout, hexbank, 1);
                          blockwrite(fout, lookup[b], 1);
                          inc(position, 2);
                    end;
               end;
               if filepoz = filesize(fin) then bored := false;
          end;
          if filepoz = filesize(fin) then bored3 := false;
end;
          close(fin);
{          seek(fout2, filesize(fout2) - 4);
          truncate(fout2);} {don't need to do this}
          close(fout2);
     end;

     blockwrite(fout4, position, 4); {INTEL}
     close(fout);
     close(fout4);

     writeln ('done!');
end.