{$S 65535}
PROGRAM IMPORT;
CONST
   BUFSIZE = 8192;
   TWOMEGS = 2097152;
   THREEMEGS = 3145728;
   MAXIMUMDESC = 99; {1st}
   MAXIMUMINFO = 199; {2nd}
VAR
   MAXFILE, MAXINFO, CURPOS:LONGINT;
   FORIG, FIN, FPNT1, FPNT2, FOUT:FILE;
   I:BYTE;
   J,K:LONGINT;
   POINTER1,POINTER2:LONGINT;
   FILEDESC:ARRAY [1..MAXIMUMDESC, 1..2] OF STRING; {1-> filename, 2->description}
   FILENPOS:ARRAY [1..MAXIMUMDESC] OF LONGINT; {position in filenames}

   FILEINFO:ARRAY [1..MAXIMUMINFO, 1..3] OF LONGINT;
   {1..99 -> list, 1->startpos, 2->length, 3->filenam #}
   BUF:ARRAY[1..BUFSIZE] OF BYTE;
   s:string;
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

PROCEDURE ICHECKSUM;
CONST
   BUFSIZE=512;
VAR
   I,J,LOOPS:LONGINT;
   CHECKSUM:LONGINT;
   FINAL:WORD;
   BUF:ARRAY[1..BUFSIZE DIV 2] OF WORD;
BEGIN
     CHECKSUM := 0;
     SEEK(FOUT, 512);
     LOOPS := (THREEMEGS - 512) DIV BUFSIZE;
     FOR J := 1 TO LOOPS DO
     BEGIN
          BLOCKREAD(FOUT, BUF, BUFSIZE);
          FOR I := 1 TO BUFSIZE DIV 2 DO
          BEGIN
               INC(CHECKSUM, SWAP(BUF[I]));
          END;
          WHILE CHECKSUM > 65535 DO DEC(CHECKSUM, 65536);
     END;
     MOVE(CHECKSUM, FINAL, 2);
     FINAL := SWAP(FINAL);
     SEEK(FOUT, $18E);
     BLOCKWRITE (FOUT, FINAL, 2);
END;
PROCEDURE INITVARS;
VAR
   I,J:BYTE;
   F:TEXT;
BEGIN
     CHECKFILE('import.dat');
     ASSIGN(F, 'import.dat');
     RESET(F);
     READLN(F, MAXFILE);
     FOR I := 1 TO MAXFILE DO
     BEGIN
          READLN (F, FILEDESC[I, 1]);
          READLN (F, FILEDESC[I, 2]);
          FILENPOS[I] := 0;
     END;
     READLN(F, MAXINFO);
     FOR I := 1 TO MAXINFO DO
     BEGIN
     FOR J := 1 TO 3 DO
     BEGIN
     READLN(F,FILEINFO[I, J]);
     END;
     END;
     FILEINFO[MAXINFO+1, 1] := THREEMEGS;
     CLOSE(F);
     CURPOS := 0;
END;

PROCEDURE COPYORIG(SIZ:LONGINT); {ORIGINAL FILE}
VAR
   LEFTOVER, LOOPS, I:LONGINT;
   SIZ2:LONGINT;
BEGIN
{   WRITELN(SIZ, ' bytes from ',CURPOS,' to new.smc pos. ',CURPOS);}

   IF (CURPOS + SIZ) > TWOMEGS THEN
   BEGIN

        IF CURPOS < TWOMEGS THEN
        BEGIN
   SEEK(FORIG, CURPOS);
   SIZ2 := TWOMEGS - CURPOS;
   INC(CURPOS, SIZ2);
   LOOPS := TRUNC(SIZ2 DIV BUFSIZE);
   LEFTOVER := SIZ2 MOD BUFSIZE;
   FOR I := 1 TO LOOPS DO
   BEGIN
        BLOCKREAD (FORIG, BUF, BUFSIZE);
        BLOCKWRITE (FOUT, BUF, BUFSIZE);
{        write ('.');}
   END;
   IF LEFTOVER > 0 THEN
   BEGIN
   BLOCKREAD (FORIG, BUF, LEFTOVER);
   BLOCKWRITE (FOUT, BUF, LEFTOVER);
{   write ('.');}
   END;
   DEC(SIZ, SIZ2);
        END;

   SEEK(FORIG, CURPOS);
   INC(CURPOS, SIZ);
   LOOPS := TRUNC(SIZ DIV BUFSIZE);
   LEFTOVER := SIZ MOD BUFSIZE;
   FOR I := 1 TO LOOPS DO
   BEGIN
        FILLCHAR(BUF, BUFSIZE, 0);
        BLOCKWRITE (FOUT, BUF, BUFSIZE);
{        write ('.');}
   END;
   IF LEFTOVER > 0 THEN
   BEGIN
   BLOCKWRITE (FOUT, BUF, LEFTOVER);
{   write ('.');}
   END;

   END
   ELSE
   BEGIN
   SEEK(FORIG, CURPOS);
   INC(CURPOS, SIZ);
   LOOPS := TRUNC(SIZ DIV BUFSIZE);
   LEFTOVER := SIZ MOD BUFSIZE;
   FOR I := 1 TO LOOPS DO
   BEGIN
        BLOCKREAD (FORIG, BUF, BUFSIZE);
        BLOCKWRITE (FOUT, BUF, BUFSIZE);
{        write ('.');}
   END;
   IF LEFTOVER > 0 THEN
   BEGIN
   BLOCKREAD (FORIG, BUF, LEFTOVER);
   BLOCKWRITE (FOUT, BUF, LEFTOVER);
{   write ('.');}
   END;
   END;

END;
PROCEDURE COPYFILE(LISTNO:BYTE); {DATAFILE}
VAR
   LEFTOVER, LOOPS, I, SIZ:LONGINT;
BEGIN
   CHECKFILE(FILEDESC[FILEINFO[LISTNO, 3], 1]);
   ASSIGN(FIN, FILEDESC[FILEINFO[LISTNO, 3], 1]);
   RESET(FIN, 1);

   SEEK(FIN, FILENPOS[ FILEINFO[LISTNO, 3] ]);

   SIZ := FILEINFO[LISTNO, 2];
   IF SIZ > FILESIZE(FIN) THEN SIZ := FILESIZE(FIN);
   IF SIZ = 0 THEN SIZ := FILESIZE(FIN);
{   WRITE(SIZ, ' bytes from ', FILEDESC[ FILEINFO[LISTNO, 3], 1]);
   WRITELN(' (',FILEDESC[ FILEINFO[LISTNO, 3], 2], ') at pos. ',FILENPOS[ FILEINFO[LISTNO, 3] ],' to pos. ',CURPOS);
}
   write('.');

   INC(FILENPOS[ FILEINFO[LISTNO, 3]], SIZ);
   INC(CURPOS, SIZ);

   LOOPS := TRUNC(SIZ DIV BUFSIZE);
   LEFTOVER := SIZ MOD BUFSIZE;
   FOR I := 1 TO LOOPS DO
   BEGIN
        BLOCKREAD (FIN, BUF, BUFSIZE);
        BLOCKWRITE (FOUT, BUF, BUFSIZE);
{        write ('.');}
   END;
   IF LEFTOVER > 0 THEN
   BEGIN
   BLOCKREAD (FIN, BUF, LEFTOVER);
   BLOCKWRITE (FOUT, BUF, LEFTOVER);
{   write ('.');}
   END;
   CLOSE(FIN);
END;

BEGIN
   INITVARS;
   WRITE ('creating new.bin');
   CHECKFILE('lungris2.bin');
   ASSIGN (FORIG, 'lungris2.bin');
   RESET (FORIG, 1);
   ASSIGN (FOUT, 'new.bin');
   REWRITE (FOUT, 1);

{   COPYORIG(FILEINFO[1,1]); {beginning chunk}
   FOR I := 1 TO MAXIMUMDESC DO
   BEGIN
        FILENPOS[I] := 0;
   END;

   FOR I := 1 TO MAXINFO DO
   BEGIN
   COPYFILE(I);
{   writeln (FILEINFO[I+1, 1], ' ', CURPOS);}
   if (FILEINFO[I+1, 1] - CURPOS) > 0 then COPYORIG((FILEINFO[I+1, 1] - CURPOS));
   END;

   {import the dialogue pointers now}
   FOR I := 1 TO 31 DO
   BEGIN
          str(i, s);
          if length(s) = 1 then s := '0'+s;
          assign(fpnt1, 'pntd-'+s+'.dat');
          reset(fpnt1, 1);
          seek(fpnt1, 0);
          assign(fpnt2, 'pntd-'+s+'d.dat');
          reset(fpnt2, 1);
          seek(fpnt2, 0);
          J := FILESIZE(fpnt1) div 4;
          FOR K := 1 TO J DO
          BEGIN
               BLOCKREAD(FPNT1, POINTER1, 4);
               SEEK(FOUT, POINTER1);
               BLOCKREAD(FPNT2, POINTER2, 4);
               BLOCKWRITE(FOUT, POINTER2, 4);
          END;
          CLOSE(fpnt2);
          CLOSE(fpnt1);
   END;
   ICHECKSUM;
   CLOSE (FOUT);
   CLOSE (FORIG);
   WRITELN ('done!');
END.