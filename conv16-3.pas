{$S 65535}
program conv16_3;
{uses crt;}
const
   powa:array[1..8] of byte=(128,64,32,16,8,4,2,1);
   bufsize=32768;
var
   bored, bored2, sing, ignore:boolean;
   i,j,k,l,filepoz:longint;
   code:integer;
   b,z:byte;
   test:byte;
   dual:array[0..1] of byte;
   dualpos:byte;
   single:byte;
   count:array[32..127, 32..127, 1..2] of byte;
   countpos:array[1..2] of byte;
   countlong:longint;
   compare1,compare2:string;
   bufin, bufout:array[0..1000] of byte;
   bufcmd:string;
   s:string;
   fin, fout, fout2, fout3, fpnt:file;
   ftext:text;
   position:longint;
   linewidth:byte;
   allready:boolean;
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
procedure fillout;
begin
     if dualpos = 0 then {none}
     begin
     end;
     if dualpos = 1 then {last to fill}
     begin
          dual[1] := 32; {space}
          dualpos := 0;
          blockwrite(fout, count[dual[0], dual[1], 1], 2);
          inc(position, 2);
          {fill}
     end;
     if dualpos = 2 then {already full}
     begin
          dualpos := 0;
          blockwrite(fout, count[dual[0], dual[1], 1], 2);
          inc(position, 2);
          {fill}
     end;
end;
procedure writepointer;
begin
     position := endian(position); {convert endian}
     blockwrite(fout2, position, 4); {MOTOROLA}
     position := endian(position); {convert back}
end;
procedure special(sbyte:byte);
var
   valff:byte;
     begin
          valff := $ff;
          blockwrite(fout, valff, 1);
          blockwrite(fout, sbyte, 1);
          inc(position, 2);
          if sbyte = $ff then writepointer;
     end;
procedure singletile(stile:byte);
var
   valff:byte;
     begin
          valff := $73;
          blockwrite(fout, valff, 1);
          blockwrite(fout, stile, 1);
          inc(position, 2);
     end;
begin
   write ('Converting dialogue EUC files');

   {retrieve position}
   assign (fout3, 'position.dat');
   reset (fout3, 1);
   blockread(fout3, position, 4);
   seek(fout3, 0);

   assign (fout, 'data0001.dat');
   reset (fout, 1);
   seek(fout, filesize(fout));

   assign (fin, 'dual.dat');
   reset(fin, 1);
   blockread(fin, count, filesize(fin));
   close(fin);

     for i := 1 to 31 do
     begin
          write('.');
          checkabort;
          str(i, s);
          if length(s) = 1 then s := '0'+s;
          checkfile('l2-d'+s+'f.euc');
          assign (fin, 'l2-d'+s+'f.euc');
          reset(fin, 1);
          assign (fout2, 'pntd-'+s+'d.dat');
          rewrite(fout2, 1);
          writepointer;
          filepoz := 0;
          dualpos := 0;
          bored:=true;
          seek(fin, filepoz); {seek to 0 again}
          while bored do
          begin
               blockread(fin, b, 1);
               inc(filepoz);
               ignore := false;
               if b < 32 then ignore := true; {ignore misc, japanese}
               if b > 127 then
               begin
                  ignore := true; {ignore next unicode byte}
                  blockread(fin, b, 1);
                  inc(filepoz);
                  b := 0;
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
                         fillout;
                         special($FF);
                    end;
                    if bufcmd = 'wait' then {FE - WAIT}
                       begin
                            fillout;
                            special($FD);
                       end;
                    sing := false;
                    if length(bufcmd) = 1 then
                    begin
                         test := ord(bufcmd[1]);
                         if (test > 32) and (test < 128) then sing := true;
                    end;
                    if sing = true then
                    begin
                         fillout;
                         singletile(test);
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
                              fillout;
                              special(254);
                         end
                         else
                         begin {?}
                              ignore := true;
                              b := 32;
                              fillout;
                         end;
                    end;

                    if ignore = false then
                    begin {normal english}

                         dual[dualpos] := b;
                         inc(dualpos);
                         if dualpos = 2 then {successful pair}
                         begin
                              fillout;
                         end;
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
     blockwrite(fout3, position, 4); {INTEL}
     close(fout3);
     close(fout);
     writeln ('done!');
end.