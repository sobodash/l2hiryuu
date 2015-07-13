var
   f:file;
   buf:array[1..32768] of byte;
   i:word;
begin

     assign(f, '3.fnt');
     rewrite(f, 1);
     for i := 1 to 16384 do
     begin
     buf[(i*2)-1] := $00;
     buf[i*2] := $ff;
     end;
     blockwrite(f, buf, 32768);
     close(f);
end.