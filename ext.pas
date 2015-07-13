{$S 65535}
program ext;
const
   BUFSIZE = 16386;
var
   FPOS:longint;
   FSIZE:longint;
   FIN, FOUT:file;
   code:integer;
   BUF:array[1..BUFSIZE] of byte;

PROCEDURE COPYit(SIZ:LONGINT);
VAR
   LEFTOVER, LOOPS, I:LONGINT;
BEGIN
   SEEK(FIN, FPOS);
   LOOPS := TRUNC(SIZ DIV BUFSIZE);
   LEFTOVER := SIZ MOD BUFSIZE;
   FOR I := 1 TO LOOPS DO
   BEGIN
        BLOCKREAD (FIN, BUF, BUFSIZE);
        BLOCKWRITE (FOUT, BUF, BUFSIZE);
        write ('.');
   END;
   IF LEFTOVER > 0 THEN
   BEGIN
   BLOCKREAD (FIN, BUF, LEFTOVER);
   BLOCKWRITE (FOUT, BUF, LEFTOVER);
   write ('.');
   END;
END;

begin
     if paramcount <> 4 then
     begin
          writeln('syntax: ext [source] [target] [offset] [delta]');
          halt(1);
     end;
     assign(fin, paramstr(1));
     reset(fin, 1);
     assign(fout, paramstr(2));
     rewrite(fout, 1);
     val(paramstr(3), fpos, code);
     val(paramstr(4), fsize, code);
     writeln(paramstr(2), ': ', fpos, ' to ', fpos+fsize-1, ' (delta', fsize, ')');
     COPYit(fsize);

end.