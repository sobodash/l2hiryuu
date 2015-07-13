{$X+}
program conv8x8;
const
   EUCTABLE:array[32..123] of byte=(
  { sp  !   "   #   $   %   &   '   (   )   *   +   ,   -   .   /}
   $20,$2c,$20,$20,$20,$3b,$20,$20,$20,$20,$3C,$2b,$00,$2d,$2e,$3e,
  { 0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ?}
   $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$20,$20,$20,$20,$20,$3f,
  { @   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O}
   $20,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,
  { P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _}
   $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$20,$20,$20,$20,$20,
  { `   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O}
   $20,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,
  { P   Q   R   S   T   U   V   W   X   Y   Z}
   $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$ff);
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

procedure convert;
var
   b,dumc:byte;
   leftover,i:longint;
   fin,fout,fout2,fout3:file;
   buf:array[0..1024] of byte;
   position:longint;
   trigger:boolean;
   s:string;
   loops:byte;
begin
   trigger := true;

   {retrieve position}
   assign (fout3, 'position.dat');
   reset (fout3, 1);
   blockread(fout3, position, 4);
   seek(fout3, 0);

   checkfile('dump8-1e.euc');
   assign (fout, 'data0001.dat');
   reset (fout, 1);
   seek(fout, filesize(fout));
   {1 - text, 2 - pointers, 3 - position}

   for loops := 1 to 3 do
   begin
   str(loops, s);
   assign (fin, 'dump8-'+s+'e.euc');
   reset (fin, 1);
   assign (fout2, 'pnt8-'+s+'.dat');
   rewrite (fout2, 1);
   while not eof(fin) do
   begin
        blockread(fin, b, 1);
        dumc := 1;
        if b = 10 then dumc := 0; {ignore return}
        if b = 13 then dumc := 0; {ignore return}

        if b > 123 then dumc := 0; {ignore non alpha and non '{' }
        if b < 32 then dumc := 0;

        if b = ord('{') then {get spaces}
        begin
             dumc := 0;
             while not (b = ord('}')) do
             begin
                  inc(dumc);
                  blockread(fin, b, 1);
             end;
             if dumc = 1 then b := 32;
        end;
        if b = ord('<') then {get nulls}
        begin
             dumc := 0;
             while not (b = ord('>')) do
             begin
                  inc(dumc);
                  blockread(fin, b, 1);
             end;
             if dumc = 1 then b := 123;
        end;
        if dumc = 1 then
        begin
             if trigger then
             begin
                  position := endian(position); {convert endian}
                  blockwrite(fout2, position, 4); {write 32 bit pointer MOTOROLA}
                  position := endian(position); {convert back}
                  trigger := false;
             end;
             blockwrite(fout, euctable[b], 1);
             inc(position);
             if b = 123 then trigger := true;
        end;
   end;
   close(fin);
   close(fout2);

   end;


   close(fout);

   blockwrite(fout3, position, 4); {write 32 bit pointer INTEL}
   close(fout3);

end;
begin
   WRITE ('Converting 8x8 text to game format...');
   convert;
   WRITELN ('done!');

end.