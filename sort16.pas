{$S 65535}
program sort16;
{USES CRT;}
CONST
   BUFSIZE=16384;
label 1;
var
   sorttype:byte;
   code:integer;
   bored, bored2, bored4, sing, ignore, trigger:boolean;
   i,j,k,l,filepoz,oldfilepoz,oldoutpoz,pointer1:longint;
   oldb, b, test,chara:byte;
   b1, b2, b3, b4:longint;
   windowline, curwidth, lastwidth:byte;
   firstwindow,wasalpha:boolean;
   bufcmd:string;
   s:string;
   fin,fout,fout2:file;
   lessthan,greaterthan,aspace:byte;
   BUF:ARRAY[0..BUFSIZE] OF BYTE;
   clearwintxt:array[0..6] of char;
   newlinetxt:array[0..2] of char;
   enterit:array[0..1] of char;
   charlen:array[0..116] of byte;
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
procedure initvars;
begin
   lessthan:=60;
   greaterthan:=62;
   aspace:=32;
   enterit:=chr(10)+chr(10);
   clearwintxt:='<wait>' + chr(10);
   newlinetxt:='\n' + chr(10);
   assign(fin, 'l2-names.euc');
   reset(fin, 1);
   for i := 0 to 116 do
   begin
        bored := true;
        j := 0;
        while bored do
        begin
             blockread(fin, b, 1);
             inc(j);
             if b = 10 then bored := false;
        end;
        dec(j);
        if j < 9 then j := j * 2;
        if j mod 2 > 0 then inc(j, 1);
        charlen[i] := j;
   end;
end;
procedure fillout;
begin
     blockwrite(fout, lessthan, 1);
     blockwrite(fout, bufcmd[1], length(bufcmd));
     blockwrite(fout, greaterthan, 1);
     if length(bufcmd) = 0 then blockwrite(fout, enterit, 2);
     if bufcmd = 'wait' then blockwrite(fout, enterit, 1);
end;
procedure clearwindow;
begin
     seek(fout, filepos(fout) - 3);
     blockwrite(fout, clearwintxt, 7);
     windowline := 1;
end;
procedure gonewline;
begin
     inc(windowline);
     if windowline = 5 then clearwindow;
     curwidth := 0;

end;
procedure insertspace;
begin
     lastwidth := curwidth;
     oldfilepoz := filepoz;
     oldoutpoz := filepos(fout);
     inc(curwidth);
     blockwrite(fout, aspace, 1);
     wasalpha := false;
     b := 32;
end;
procedure insertspace2;
begin
     lastwidth := curwidth;
     oldfilepoz := filepoz - 4;
     oldoutpoz := filepos(fout);
     inc(curwidth);
     blockwrite(fout, aspace, 1);
     wasalpha := false;
     b := 32;
end;
begin
     initvars;
     write ('optimise l2-d??e to l2-d??f');
     for i := 1 to 31 do
     begin
          checkabort;
          str(i, s);
          if length(s) = 1 then s := '0'+s;
          checkfile('l2-d'+s+'e.euc');
          assign(fin, 'l2-d'+s+'e.euc');
          reset(fin, 1);
          assign(fout, 'l2-d'+s+'f.euc');
          rewrite(fout, 1);
          assign(fout2, 'pntd-'+s+'.dat');
          rewrite(fout2, 1);
          filepoz := 0;
          bored:=true;
          write('.');
          while bored do
          begin
               windowline:=1;
               wasalpha:=false;
               trigger:=false;
               {seek to [}
               bored2 := true;
               while bored2 do
               begin
                     blockread(fin, b, 1);
                     inc(filepoz);
                     if filepoz = filesize(fin) then bored2 := false;
                     if b = ord('[') then bored2 := false;
               end;
               if filepoz = filesize(fin) then goto 1;
               blockread(fin, buf, 5);
               blockread(fin, b, 1);
               if b < 58 then dec(b, 48) else dec(b, 55);
               b1 := b * 16;
               blockread(fin, b, 1);
               if b < 58 then dec(b, 48) else dec(b, 55);
               inc(b1, b);
               blockread(fin, b, 1);
               if b < 58 then dec(b, 48) else dec(b, 55);
               b2 := b * 16;
               blockread(fin, b, 1);
               if b < 58 then dec(b, 48) else dec(b, 55);
               inc(b2, b);
               blockread(fin, b, 1);
               if b < 58 then dec(b, 48) else dec(b, 55);
               b3 := b * 16;
               blockread(fin, b, 1);
               if b < 58 then dec(b, 48) else dec(b, 55);
               inc(b3, b);
               blockread(fin, b, 1);
               if b < 58 then dec(b, 48) else dec(b, 55);
               b4 := b * 16;
               blockread(fin, b, 1);
               if b < 58 then dec(b, 48) else dec(b, 55);
               inc(b4, b);
               pointer1 := (b1 shl 24) + (b2 shl 16) + (b3 shl 8) + b4;
               inc(pointer1, 4);
               blockwrite(fout2, pointer1, 4);
               blockread(fin, b, 1); {:}
               blockread(fin, b, 1); {0}
               blockread(fin, b, 1); {2}
               blockread(fin, b, 1);
               if b < 58 then dec(b, 48) else dec(b, 55);
               chara := b * 16;
               blockread(fin, b, 1);
               if b < 58 then dec(b, 48) else dec(b, 55);
               inc(chara, b);
               inc(filepoz, 18);
               curwidth := charlen[chara] + 3 - 2;
               lastwidth := curwidth;
               bored2 := true;
               while bored2 do
               begin
                    blockread(fin, b, 1);
                    inc(filepoz);
                    if b = ord(':') then bored2 := false;
               end;
               oldfilepoz := filepoz;
               {ok, now it starts}

               bored4 := true;
               while bored4 do
               begin
                         blockread(fin, b, 1);
                         inc(filepoz);
                         ignore := false;
                         if b > 127 then {ignore unicode}
                         begin
                              ignore := true;
                              blockread(fin, b, 1);
                              inc(filepoz);
                              b := 0;
                         end;
                         if b = 13 then ignore := true; {ignore Carriage Return}
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
                               if bufcmd = 'wait' then
                               begin
                                    fillout;
                                    windowline := 1;
                                    firstwindow := false;
                                    curwidth := 0;
                                    wasalpha := false;
                               end;
                               if length(bufcmd) = 0 then
                               begin
                                    fillout;
                                    bored4 := false;
                               end;

                         sing := false;
                         if length(bufcmd) = 1 then
                         begin
                              test := ord(bufcmd[1]);
                              if test = 43 then begin
                                 sing := true;{+}
                              end;
                              if test = 45 then begin
                                 sing := true;{-}
                              end;
                              if test > 47 then begin
                                 if test < 58 then begin
                                    sing := true;{numbers}
                                 end;
                              end;
                              if test > 64 then begin
                                 if test < 91 then begin
                                    sing := true;{caps}
                                 end;
                              end;
                              if test > 96 then begin
                                 if test < 123 then begin
                                    sing := true;{lcase}
                                 end;
                              end;
                         end;
                         if sing = true then
                         begin
                              if (curwidth mod 2) > 0 then insertspace;
                              inc(curwidth, 2);
                              fillout;
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
                                     b := 92;
                                     blockwrite(fout, newlinetxt, 3);
                                     gonewline;
                                     wasalpha := false;
{                                    fillout;}
                                     end
                               else
                               begin {?}
                                     ignore := true;
{                              fillout;}
                               end;
                         end;

                    if ignore = false then
                    begin {normal english}
                          if b = 10 then
                          begin
                               if oldb = 10 then
                               begin
                               end
                               else
                               begin
                                    if wasalpha then
                                    begin
                                         b := 32;
                                    end;
                               end;
                          end;
                          if b = 32 then
                          begin
                               lastwidth := curwidth;
                               oldfilepoz := filepoz;
                               oldoutpoz := filepos(fout);
                          end;
                          if b > 31 then
                          begin
                               inc(curwidth);
                               blockwrite(fout, b, 1);
                          end;
                          wasalpha := false;
                          if b > 32 then wasalpha:=true;
                    end;
               end;

               if curwidth > 30 then
               begin
                   seek(fin, oldfilepoz);
                   filepoz := oldfilepoz;
                   seek(fout, oldoutpoz);
                   blockwrite(fout, newlinetxt, 3);
                   gonewline;
                   lastwidth := 0;
                   trigger:= true;
               end;

               oldb := b;
               1:
               if filepoz = filesize(fin) then bored := false;

               end;

     end;

     close(fout2);
     close(fin);
     close(fout);

     end;

     writeln('done!');
end.