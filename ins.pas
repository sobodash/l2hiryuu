{$S 65535}
program INS;
const
   BUFSIZE = 16386;
var
   buf:array[1..BUFSIZE] of byte;
   sourcepos, targetpos, delta, i:longint;
   loops, leftover:longint;
   fin, fout:file;
   code:integer;
begin
     if paramcount <> 5 then
     begin
          writeln('syntax: ins [source] [offset] [target] [offset] [delta]');
          halt(1);
     end;
     assign(fin, paramstr(1));
     reset(fin, 1);
     assign(fout, paramstr(3));
     reset(fout, 1);
     val(paramstr(2), sourcepos, code);
     val(paramstr(4), targetpos, code);
     val(paramstr(5), delta, code);
     loops := DELTA DIV BUFSIZE;
     leftover := DELTA MOD BUFSIZE;
     write('source: ', paramstr(1), ' at offset ', sourcepos, ' / target: ');
     writeln(paramstr(3), ' at offset ', targetpos, ', delta ', DELTA);
     seek(fin, sourcepos);
     seek(fout, targetpos);
     for i := 1 to loops do
     begin
          blockread(fin, buf, bufsize);
          blockwrite(fout, buf, bufsize);
     end;
     blockread(fin, buf, leftover);
     blockwrite(fout, buf, leftover);
     seek(fin, filesize(fin));
     seek(fout, filesize(fout));
     close(fin);
     close(fout);
end.