program openfilt;
var
   f, f2:file;
   font:array[0..255, 1..32] of byte;
   final:array[1..64] of byte;
   i:word;
   b1,b2:byte;
begin
     assign(f, '8x16.fnt');
     reset(f, 1);
     for i := 0 to 255 do
     begin
          blockread(f, font[i], 32);
     end;
     close(f);

     assign(f, '4.fnt');
     reset(f, 1);
     seek(f, 16384);
     assign(f2, 'opening.txt');
     reset(f2, 1);
     for i := 1 to filesize(f2) div 2 do
     begin
     blockread(f2, b1, 1);
     blockread(f2, b2, 1);
     move(font[b1], final[1], 32);
     move(font[b2], final[33], 32);
     blockwrite(f, final, 64);
     end;
     close(f);
end.