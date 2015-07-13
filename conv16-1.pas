program conv16_1;
const
     MAX = 38;
     FNAMES:ARRAY[1..MAX] OF STRING=(
     'l2-1e','l2-2e','l2-3e','l2-4e','l2-5e','l2-6e',
     'l2-dummy',
     'l2-d01f','l2-d02f','l2-d03f','l2-d04f','l2-d05f','l2-d06f','l2-d07f','l2-d08f','l2-d09f','l2-d10f',
     'l2-d11f','l2-d12f','l2-d13f','l2-d14f','l2-d15f','l2-d16f','l2-d17f','l2-d18f','l2-d19f','l2-d20f',
     'l2-d21f','l2-d22f','l2-d23f','l2-d24f','l2-d25f','l2-d26f','l2-d27f','l2-d28f','l2-d29f','l2-d30f',
     'l2-d31f');
var
   bored, bored2, sing, ignore:boolean;
   i,j,k,l,filepoz,total:longint;
   x,y:byte;
   b:byte;
   test:byte;
   dual:array[0..1] of byte;
   dualpos:byte;
   single:byte;
   count:array[32..127, 32..127] of longint;
   font:array[0..255, 1..32] of byte;
   compare1,compare2:string;
   bufin, bufout:array[0..1000] of byte;
   bufcmd:string;
   s:string;
   fin, fout, fout2:file;
   z:word;
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
   end;
}
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
          inc(count[dual[0], dual[1]]);
     end;
     if dualpos = 2 then {already full}
     begin
          dualpos := 0;
          inc(count[dual[0], dual[1]]);
     end;

end;

begin
     write ('generating dual tile list');
     assign(fin, '8x16.fnt');
     reset(fin, 1);
     FOR i := 0 to 255 do
     begin
          blockread(fin, font[i], 32);
     end;
     close(fin);
     for i := 1 to MAX do
     begin
          write('.');
          checkabort;
          checkfile(FNAMES[i]+'.euc');
          assign(fin, FNAMES[i]+'.euc');
          reset(fin, 1);
          filepoz := 0;
          seek(fin, filepoz); {seek to 0 again}
          bored := true;
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

{                    if bufcmd = 'clsr' then fillout;}
                    if bufcmd = 'wait' then fillout;
                    if bufcmd = 'ch' then fillout;
                    if bufcmd = '##' then fillout;

                    if length(bufcmd) = 0 then
                    begin
                         fillout;
                    end;
                    sing := false;
                    if length(bufcmd) = 1 then
                    begin
                         test := ord(bufcmd[1]);
                         if (test > 32) and (test < 128) then sing := true;
                    end;
                    if sing = true then fillout;
               end

               else

               begin {not <> area}
                    if b = 92 then begin {\}
                         blockread(fin, b, 1);
                         inc(filepoz);
                         if b = 110 then {n?}
                         begin
                              ignore := true; {ignore \n}
                              b := 92;
                              fillout;
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
     end;
     writeln ('done!');

     assign (fout, 'main.fnt');
     rewrite(fout, 1);
     assign (fout2, 'dual.dat');
     rewrite (fout2, 1);
     x := $77;
     y := 00;
     z := 0000;
     total := 0;
     for i := 32 to 127 do begin
      for j := 32 to 127 do begin
       if count[i, j] > 0 then
      begin
            blockwrite(fout2, x, 1);
            blockwrite(fout2, y, 1);
            blockwrite(fout, font[i], 16);
            blockwrite(fout, font[j], 16);
            blockwrite(fout, font[i, 17], 16);
            blockwrite(fout, font[j, 17], 16);
            if y = $ff then
            begin
                 y := 0;
                 inc(x);
            end
            else
            begin
                 inc(y);
            end;
            inc(total);
       end
       else
       begin
            blockwrite(fout2, z, 2);
       end;
      end;
     end;
     close(fout);
     close(fout2);
     writeln (total, ' / 4096 dual tiles created.');
end.