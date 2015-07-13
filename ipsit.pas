{need to fix the RLE part, i think it is resetting to 0 without bothering
to write the RLE part, it should set the BEGINOFFS to the curpos and write
the RLE block to save space...}

{$S 65535}
PROGRAM IPSIT;
USES CRT;
CONST
   BUFSIZE = 16384;
{   MAXBLOCK = 65535;}
   MAXBLOCK = 65535;
   EOFVAL = $454F46;
var
   BUF1:ARRAY[1..BUFSIZE] OF BYTE;
   BUF2:ARRAY[1..BUFSIZE] OF BYTE;
   BUF3:ARRAY[1..5] OF BYTE;
   BUF4:ARRAY[1..5] OF BYTE;
   BUF5:ARRAY[1..BUFSIZE] OF BYTE;
   BORED:BOOLEAN;
   I,J:LONGINT;
   COMMAND:BYTE;
   S,EXT,FILE1, FILE2, FILE3:STRING;
   FSOURCE,FTARGET,FIPS:FILE;
PROCEDURE CHECKABORT;
var
   c:char;
BEGIN
   while keypressed do
   begin
        c := readkey;
        if ord(c) = 27 then
        BEGIN
             WRITELN('');
             WRITELN('* Error: User abort');
             HALT(1);
        END;
   end;
END;
PROCEDURE PROGRESS(INDICATOR:LONGINT;MAX:LONGINT);
VAR
   PERCENT:BYTE;
BEGIN
     PERCENT := TRUNC((INDICATOR / MAX) * 100);
     GOTOXY(WHEREX - 4, WHEREY);
     IF PERCENT < 100 THEN WRITE(' ');
     IF PERCENT < 10 THEN WRITE('0');
     WRITE(PERCENT,'%');
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
FUNCTION UCASE(S:STRING):STRING;
VAR
   I:LONGINT;
BEGIN
     FOR I := 1 TO LENGTH(S) DO
     BEGIN
          IF ((ORD(S[I]) > 96) AND (ORD(S[I]) < 123)) THEN S[I] := CHR(ORD(S[I])-32);
     END;
     UCASE := S;
END;
PROCEDURE WRITEPOINTER(ADR:LONGINT);
VAR
   A1,A2,A3:LONGINT;
   B:BYTE;
BEGIN
     A1 := ADR SHR 16;
     B := A1;
     BLOCKWRITE(FIPS, B, 1);
     A2 := (ADR SHR 8) - (A1 SHL 8);
     B := A2;
     BLOCKWRITE(FIPS, B, 1);
     A3 := ADR - ((A1 SHL 16) + (A2 SHL 8));
     B := A3;
     BLOCKWRITE(FIPS, B, 1);
END;
PROCEDURE SYNTAX;
BEGIN
          WRITELN('Commands: -a = apply patch, -c = create patch, -f = parse FC input');
          WRITELN('Examples:');
          WRITELN('IPSIT -a original.bin patch.ips');
          WRITELN('  Applies "patch.ips" to "original.bin".');
          WRITELN('IPSIT -c original.bin patched.bin patch.ips');
          WRITELN('  Creates "patch.ips" by comparing "original.bin" and "patched.bin."');
          WRITELN('  File order is important: [source] [target] [ips]');
          WRITELN('IPSIT -f fcoutput.txt patch.ips');
          WRITELN('  Creates "patch.ips" by parsing "fcoutput.txt" which should have been');
          WRITELN('  created earlier using FC (example: FC /B file1.bin file2.bin >fcoutput.txt).');
          WRITELN('  The 2nd file''s contents (file2.bin) will be used to create the patch.');
          HALT(1);
END;
PROCEDURE COPYIT(FOFFSET:LONGINT;FSIZE:LONGINT);
VAR
   LOOPS,LEFTOVER:LONGINT;
   I:LONGINT;
BEGIN
     LOOPS := FSIZE DIV BUFSIZE;
     LEFTOVER := FSIZE MOD BUFSIZE;
     SEEK(FTARGET, FOFFSET);
     FOR I := 1 TO LOOPS DO
     BEGIN
          BLOCKREAD(FTARGET, BUF5, BUFSIZE);
          BLOCKWRITE(FIPS, BUF5, BUFSIZE);
     END;
     BLOCKREAD(FTARGET, BUF5, LEFTOVER);
     BLOCKWRITE(FIPS, BUF5, LEFTOVER);
END;
PROCEDURE APPLYPATCH(TARGETFILE:STRING;IPSFILE:STRING);
VAR
   BORED:BOOLEAN;
   B:BYTE;
   TEMP:LONGINT;
   ADR:LONGINT;
   BSIZE:WORD;
   I:LONGINT;
   LOOPS,LEFTOVER:LONGINT;
LABEL SKIPIT;
BEGIN
     B := 0;
     BORED := TRUE;
     TEMP := 0;
     ADR := 0;
     BSIZE := 0;
     I := 0;
     LOOPS := 0;
     LEFTOVER := 0;
     CHECKFILE(TARGETFILE);
     ASSIGN(FTARGET, TARGETFILE);
     RESET(FTARGET, 1);
     CHECKFILE(IPSFILE);
     ASSIGN(FIPS, IPSFILE);
     RESET(FIPS, 1);
     BLOCKREAD(FIPS, S[1], 5); {READ PATCH}
     S[0] := CHR(5);
     IF S <> 'PATCH' THEN
     BEGIN
          WRITELN('* Error: IPS file invalid.');
          HALT(1);
     END;
     BORED := TRUE;
     WHILE BORED DO
     BEGIN
          CHECKABORT;
          PROGRESS(FILEPOS(FIPS), FILESIZE(FIPS));
          BLOCKREAD(FIPS, TEMP, 1);
          ADR := TEMP SHL 16;
          BLOCKREAD(FIPS, TEMP, 1);
          INC(ADR, TEMP SHL 8);
          BLOCKREAD(FIPS, TEMP, 1);
          INC(ADR, TEMP);
          IF ADR = EOFVAL THEN BORED := FALSE;
          IF ADR = EOFVAL THEN GOTO SKIPIT;
          BLOCKREAD(FIPS, BSIZE, 2);
          BSIZE := SWAP(BSIZE);
          IF BSIZE = 0 THEN
          BEGIN
               {PROCESS RLE}
               BLOCKREAD(FIPS, BSIZE, 2);
               BSIZE := SWAP(BSIZE);
               BLOCKREAD(FIPS, B, 1);
               LOOPS := BSIZE DIV BUFSIZE;
               LEFTOVER := BSIZE MOD BUFSIZE;
               FILLCHAR(BUF1, BUFSIZE, B);
               SEEK(FTARGET, ADR);
               FOR I := 1 TO LOOPS DO
               BEGIN
                    BLOCKWRITE(FTARGET, BUF1, BUFSIZE);
               END;
               BLOCKWRITE(FTARGET, BUF1, LEFTOVER);
          END
          ELSE
          BEGIN
               {PROCESS BLOCK}
               LOOPS := BSIZE DIV BUFSIZE;
               LEFTOVER := BSIZE MOD BUFSIZE;
               SEEK(FTARGET, ADR);
               FOR I := 1 TO LOOPS DO
               BEGIN
                    BLOCKREAD(FIPS, BUF1, BUFSIZE);
                    BLOCKWRITE(FTARGET, BUF1, BUFSIZE);
               END;
               BLOCKREAD(FIPS, BUF1, LEFTOVER);
               BLOCKWRITE(FTARGET, BUF1, LEFTOVER);
          END;
          SKIPIT:
     END;
END;

PROCEDURE PARSEFC(SOURCEFILE:STRING;IPSFILE:STRING);
VAR
   BORED,BORED2:BOOLEAN;
   B,C:BYTE;
   B1,B2,B3,B4:LONGINT;
   TEST:LONGINT;
   ADR,OLDADR:LONGINT;
   K:WORD;
   BEGINOFFS:LONGINT;
   S:STRING;
   FPOZ:LONGINT;
   BLOCKNO:LONGINT;
LABEL SKIPIT;
BEGIN
     FILLCHAR(BUF1, BUFSIZE, 0);
     BORED := TRUE;
     BORED2 := TRUE;
     B := 0;
     C := 0;
     B1 := 0;
     B2 := 0;
     B3 := 0;
     B4 := 0;
     TEST := 0;
     ADR := 0;
     OLDADR := 0;
     K := 0;
     BEGINOFFS := 0;
     S := '';
     FPOZ := 0;
     BLOCKNO := 0;
     CHECKFILE(SOURCEFILE);
     ASSIGN(FSOURCE, SOURCEFILE);
     RESET(FSOURCE, 1);
     ASSIGN(FIPS, IPSFILE);
     REWRITE(FIPS, 1);
     FPOZ := 0;
     SEEK(FIPS, FPOZ);
     S := 'PATCH';
     BLOCKWRITE(FIPS, S[1], 5);
     INC(FPOZ, 5);

     BORED2 := TRUE;
     WHILE BORED2 DO
     BEGIN
          BLOCKREAD(FSOURCE, B, 1);
          IF B = 10 THEN BORED2 := FALSE;
     END;
BORED := TRUE;
WHILE BORED DO
BEGIN
     CHECKABORT;
     PROGRESS(FILEPOS(FSOURCE), FILESIZE(FSOURCE));
     BORED2 := TRUE;
     WHILE BORED2 DO
     BEGIN
          BLOCKREAD(FSOURCE, B, 1);
          IF (B > 47) AND (B < 58) THEN BORED2 := FALSE;
          IF (B > 64) AND (B < 71) THEN BORED2 := FALSE;
          IF FILEPOS(FSOURCE) = FILESIZE(FSOURCE) THEN BORED2 := FALSE;
     END;
     IF FILEPOS(FSOURCE) = FILESIZE(FSOURCE) THEN BORED := FALSE;
     IF FILEPOS(FSOURCE) = FILESIZE(FSOURCE) THEN GOTO SKIPIT;
     SEEK(FSOURCE, FILEPOS(FSOURCE) - 1);
     BLOCKREAD(FSOURCE, B, 1);
     IF ((B > 47) AND (B < 58)) THEN DEC(B, 48) ELSE DEC(B, 55);
     BLOCKREAD(FSOURCE, C, 1);
     IF ((C > 47) AND (C < 58)) THEN DEC(C, 48) ELSE DEC(C, 55);
     B1 := (B SHL 4) + C;
     IF B1 = $FC THEN
     BEGIN
          BLOCKREAD(FSOURCE, B, 1);
          IF B = ORD(':') THEN BORED := FALSE;
     END
     ELSE
     BEGIN
          BLOCKREAD(FSOURCE, B, 1);
          IF ((B > 47) AND (B < 58)) THEN DEC(B, 48) ELSE DEC(B, 55);
     END;
     IF B = ORD(':') THEN GOTO SKIPIT;
     BLOCKREAD(FSOURCE, C, 1);
     IF ((C > 47) AND (C < 58)) THEN DEC(C, 48) ELSE DEC(C, 55);
     B2 := (B SHL 4) + C;
     BLOCKREAD(FSOURCE, B, 1);
     IF ((B > 47) AND (B < 58)) THEN DEC(B, 48) ELSE DEC(B, 55);
     BLOCKREAD(FSOURCE, C, 1);
     IF ((C > 47) AND (C < 58)) THEN DEC(C, 48) ELSE DEC(C, 55);
     B3 := (B SHL 4) + C;
     BLOCKREAD(FSOURCE, B, 1);
     IF ((B > 47) AND (B < 58)) THEN DEC(B, 48) ELSE DEC(B, 55);
     BLOCKREAD(FSOURCE, C, 1);
     IF ((C > 47) AND (C < 58)) THEN DEC(C, 48) ELSE DEC(C, 55);
     B4 := (B SHL 4) + C;
     ADR := (B1 SHL 24) + (B2 SHL 16) + (B3 SHL 8) + B4;
     BLOCKREAD(FSOURCE, BUF2, 5);
     BLOCKREAD(FSOURCE, B, 1);
     IF ((B > 47) AND (B < 58)) THEN DEC(B, 48) ELSE DEC(B, 55);
     BLOCKREAD(FSOURCE, C, 1);
     IF ((C > 47) AND (C < 58)) THEN DEC(C, 48) ELSE DEC(C, 55);
     B1 := (B SHL 4) + C;
     B := B1;
     IF K = 0 THEN
     BEGIN
          BEGINOFFS := ADR;
     END;
     INC(K);
     BUF1[K] := B;
     IF BLOCKNO = 0 THEN
     BEGIN
          OLDADR := ADR;
          INC(BLOCKNO);
     END;
     IF ADR > (OLDADR + 1) THEN
     BEGIN
          DEC(K);
          SEEK(FIPS, FPOZ);
          WRITEPOINTER(BEGINOFFS);
          K := SWAP(K);
          BLOCKWRITE(FIPS, K, 2);
          K := SWAP(K);
          INC(FPOZ, 5);
          SEEK(FIPS,FPOZ);
          BLOCKWRITE(FIPS, BUF1[1], K);
          INC(FPOZ, K);
          K := 1;
          BEGINOFFS := ADR;
          BUF1[K] := B;
          INC(BLOCKNO);
     END;
     OLDADR := ADR;
     SKIPIT:
END;
    IF K > 0 THEN
    BEGIN
          SEEK(FIPS, FPOZ);
          WRITEPOINTER(BEGINOFFS);
          K := SWAP(K);
          BLOCKWRITE(FIPS, K, 2);
          K := SWAP(K);
          INC(FPOZ, 5);
          SEEK(FIPS,FPOZ);
          BLOCKWRITE(FIPS, BUF1[1], K);
          INC(FPOZ, K);
          K := 1;
          BEGINOFFS := ADR;
          BUF1[K] := B;
          INC(BLOCKNO);
    END;

     S := 'EOF';
     SEEK(FIPS, FPOZ);
     BLOCKWRITE(FIPS, S[1], 3);
     INC(FPOZ, 3);

END;

PROCEDURE CREATEPATCH(SOURCEFILE:STRING;TARGETFILE:STRING;IPSFILE:STRING);
VAR
   FPOZ:LONGINT;
   TILL:LONGINT;
   BEGINOFFS,RLEPOZ:LONGINT;
   LOOPS,LEFTOVER:LONGINT;
   I:LONGINT;
   L:BYTE;
   J:WORD;
   K:WORD;
   S:STRING;
   OLDB:BYTE;
   RLECOUNT:WORD;
   NULLWORD:WORD;
   TBUFSIZE:LONGINT;
   BLOCKNO:LONGINT;
BEGIN
     FPOZ := 0;
     TILL := 0;
     BEGINOFFS := 0;
     RLEPOZ := 0;
     LOOPS := 0;
     LEFTOVER := 0;
     I := 0;
     L := 0;
     J := 0;
     K := 0;
     S := '';
     OLDB := 0;
     RLECOUNT := 0;
     NULLWORD := 0;
     TBUFSIZE := 0;
     BLOCKNO := 0;
     CHECKFILE(SOURCEFILE);
     CHECKFILE(TARGETFILE);
     ASSIGN(FSOURCE, SOURCEFILE);
     RESET(FSOURCE, 1);
     ASSIGN(FTARGET, TARGETFILE);
     RESET(FTARGET, 1);
     ASSIGN(FIPS, IPSFILE);
     REWRITE(FIPS, 1);
     S := 'PATCH';
{     WRITELN('Writing patch header.');}
     BLOCKWRITE(FIPS, S[1], 5);
     NULLWORD := 0;
     TILL := FILESIZE(FSOURCE);
     IF FILESIZE(FTARGET) < FILESIZE(FSOURCE) THEN
     BEGIN
          WRITELN('* Warning: TARGET filesize is smaller than SOURCE.');
          TILL := FILESIZE(FTARGET);
     END;
     FPOZ := 0;
     LOOPS := TILL DIV BUFSIZE;
     LEFTOVER := TILL MOD BUFSIZE;
     K := 0;
     FOR I := 1 TO LOOPS + 1 DO
     BEGIN
          CHECKABORT;
          PROGRESS(FILEPOS(FTARGET), FILESIZE(FTARGET));
          TBUFSIZE := BUFSIZE;
          IF I = LOOPS + 1 THEN TBUFSIZE := LEFTOVER;
          SEEK(FSOURCE, (I - 1) * BUFSIZE);
          SEEK(FTARGET, (I - 1) * BUFSIZE);
          BLOCKREAD(FSOURCE, BUF1, TBUFSIZE);
          BLOCKREAD(FTARGET, BUF2, TBUFSIZE);
          FOR J := 1 TO TBUFSIZE DO
          BEGIN
               IF BUF1[J] <> BUF2[J] THEN
               BEGIN
                    IF K = 0 THEN
                    BEGIN
                         BEGINOFFS := FPOZ;
                         IF BEGINOFFS = EOFVAL THEN BEGINOFFS := EOFVAL - 1;
                         RLECOUNT := 0;
                    END;
                    INC(K);
                    IF FPOZ = 0 THEN OLDB := NOT BUF2[J];
                    IF BUF2[J] = OLDB THEN
                    BEGIN
                         IF RLECOUNT = 0 THEN
                         BEGIN
                              RLEPOZ := FPOZ - 1;
                              RLECOUNT := 1;
                         END;
                         INC(RLECOUNT);
                         IF RLECOUNT = MAXBLOCK THEN
                         BEGIN
                              {WRITE RLE BLOCK...}
                              IF RLEPOZ < BEGINOFFS THEN INC(RLEPOZ);
{                              WRITELN('Writing RLE block from offset ', RLEPOZ, ' delta ', RLECOUNT, '.');}
                              WRITEPOINTER(BEGINOFFS);
                              BLOCKWRITE(FIPS, NULLWORD, 2); {16 BIT NULL SIZE}
                              RLECOUNT := SWAP(RLECOUNT);
                              BLOCKWRITE(FIPS, RLECOUNT, 2); {16 BIT RLE SIZE}
                              RLECOUNT := SWAP(RLECOUNT);
                              BLOCKWRITE(FIPS, OLDB, 1); {8 BIT BYTE TO BE RLE'D}
                              INC(BLOCKNO);
                              RLECOUNT := 0;
                              K := 0;
                         END;
                    END
                    ELSE
                    BEGIN
                         IF RLECOUNT > 2 THEN
                         BEGIN
                              {WRITE BLOCK BEFORE RLE BLOCK, IF NEEDED}
                              IF RLEPOZ < BEGINOFFS THEN INC(RLEPOZ);
                              WRITEPOINTER(BEGINOFFS);
                              K := RLEPOZ - BEGINOFFS;
                              IF K > 0 THEN
                              BEGIN {WRITE BLOCK BEFORE}
{                                   WRITELN('Writing block from offset ', BEGINOFFS, ' delta ', K, '.');}
                                   K := SWAP(K); {16 BIT LENGTH}
                                   BLOCKWRITE(FIPS, K, 2);
                                   K := SWAP(K);
                                   COPYIT(BEGINOFFS, K);
{                                   WRITELN('Writing RLE block from offset ', RLEPOZ, ' delta ', RLECOUNT, '.');}
                                   WRITEPOINTER(RLEPOZ);
                                   BLOCKWRITE(FIPS, NULLWORD, 2);
                                   INC(BLOCKNO);
                              END
                              ELSE
                              BEGIN
                                   K := 0;
                                   BLOCKWRITE(FIPS, K, 2);
{                                   WRITELN('Writing RLE block from offset ', RLEPOZ, ' delta ', RLECOUNT, '.');}
                              END;
                              {WRITE RLE BLOCK, RESET K}
                              RLECOUNT := SWAP(RLECOUNT);
                              BLOCKWRITE(FIPS, RLECOUNT, 2); {16 BIT RLE SIZE}
                              RLECOUNT := SWAP(RLECOUNT);
                              BLOCKWRITE(FIPS, OLDB, 1); {8 BIT BYTE TO BE RLE'D}
                              INC(BLOCKNO);
                              K := 1;
                              BEGINOFFS := FPOZ;
                              IF BEGINOFFS = EOFVAL THEN BEGINOFFS := EOFVAL - 1;
                         END;
                         RLECOUNT := 0;
                    END;
                    IF K = MAXBLOCK THEN
                    BEGIN
                         IF RLECOUNT > 2 THEN
                         BEGIN
                              {WRITE BLOCK BEFORE RLE BLOCK, IF NEEDED}
                              IF RLEPOZ < BEGINOFFS THEN INC(RLEPOZ);
                              WRITEPOINTER(BEGINOFFS);
                              K := RLEPOZ - BEGINOFFS;
                              IF K > 0 THEN
                              BEGIN {WRITE BLOCK BEFORE}
{                                   WRITELN('Writing block from offset ', BEGINOFFS, ' delta ', K, '.');}
                                   K := SWAP(K); {16 BIT LENGTH}
                                   BLOCKWRITE(FIPS, K, 2);
                                   K := SWAP(K);
                                   COPYIT(BEGINOFFS, K);
                                   WRITEPOINTER(RLEPOZ);
                                   BLOCKWRITE(FIPS, NULLWORD, 2);
                                   INC(BLOCKNO);
{                                   WRITELN('Writing RLE block from offset ', RLEPOZ, ' delta ', RLECOUNT, '.');}
                              END
                              ELSE
                              BEGIN
                                   BLOCKWRITE(FIPS, K, 2);
{                                  WRITELN('Writing RLE block from offset ', RLEPOZ, ' delta ', RLECOUNT, '.');}
                              END;
                              {WRITE RLE BLOCK, RESET K}
                              RLECOUNT := SWAP(RLECOUNT);
                              BLOCKWRITE(FIPS, RLECOUNT, 2); {16 BIT RLE SIZE}
                              RLECOUNT := SWAP(RLECOUNT);
                              BLOCKWRITE(FIPS, OLDB, 1); {8 BIT BYTE TO BE RLE'D}
                              K := 0;
                              RLECOUNT := 0;
                              INC(BLOCKNO);
                         END
                         ELSE
                         BEGIN
                              {WRITE BLOCK, RESET K}
{                              INC(K);}
{                              WRITELN('Writing block from offset ', BEGINOFFS, ' delta ', K, '.');}
                              WRITEPOINTER(BEGINOFFS);
                              K := SWAP(K); {16 BIT LENGTH}
                              BLOCKWRITE(FIPS, K, 2);
                              K := SWAP(K);
                              COPYIT(BEGINOFFS, K);
                              K := 0;
                              INC(BLOCKNO);
                         END;
                         RLECOUNT := 0;
                    END;
               END
               ELSE
               BEGIN

                    IF K > 0 THEN
                    BEGIN
                         {DETERMINE IF SPACE WILL BE SAVED}
                         L := 0;
                         IF FILESIZE(FSOURCE) - FPOZ > 5 THEN
                         BEGIN

                         SEEK(FSOURCE, FPOZ);
                         BLOCKREAD(FSOURCE, BUF3, 5);
                         SEEK(FTARGET, FPOZ);
                         BLOCKREAD(FTARGET, BUF4, 5);

                         IF BUF3[1] <> BUF4[1] THEN L := 1;
                         IF BUF3[2] <> BUF4[2] THEN L := 2;
                         IF BUF3[3] <> BUF4[3] THEN L := 3;
                         IF BUF3[4] <> BUF4[4] THEN L := 4;
                         IF BUF3[5] <> BUF4[5] THEN L := 5;
                         END;

{                         L := 0;}

                         IF L > 0 THEN
                         BEGIN
                              INC(K);
                              IF BUF2[J] = OLDB THEN INC(RLECOUNT) ELSE RLECOUNT := 0;
                         END
                         ELSE
                         BEGIN





                         IF RLECOUNT > 2 THEN
                         BEGIN
                              {WRITE BLOCK BEFORE RLE BLOCK, IF NEEDED}
                              IF RLEPOZ < BEGINOFFS THEN INC(RLEPOZ);
                              WRITEPOINTER(BEGINOFFS);
                              K := RLEPOZ - BEGINOFFS;
                              IF K > 0 THEN
                              BEGIN {WRITE BLOCK BEFORE}
{                                   WRITELN('Writing block from offset ', BEGINOFFS, ' delta ', K, '.');}
                                   K := SWAP(K); {16 BIT LENGTH}
                                   BLOCKWRITE(FIPS, K, 2);
                                   K := SWAP(K);
                                   COPYIT(BEGINOFFS, K);
                                   WRITEPOINTER(RLEPOZ);
                                   BLOCKWRITE(FIPS, NULLWORD, 2);
                                   INC(BLOCKNO);
{                                   WRITELN('Writing RLE block from offset ', RLEPOZ, ' delta ', RLECOUNT, '.');}
                              END
                              ELSE
                              BEGIN
                                   BLOCKWRITE(FIPS, K, 2);
{                                   WRITELN('Writing RLE block from offset ', RLEPOZ, ' delta ', RLECOUNT, '.');}
                              END;
                              {WRITE RLE BLOCK, RESET K}
                              RLECOUNT := SWAP(RLECOUNT);
                              BLOCKWRITE(FIPS, RLECOUNT, 2); {16 BIT RLE SIZE}
                              RLECOUNT := SWAP(RLECOUNT);
                              BLOCKWRITE(FIPS, OLDB, 1); {8 BIT BYTE TO BE RLE'D}
                              K := 0;
                              RLECOUNT := 0;
                              INC(BLOCKNO);
                         END;
                         RLECOUNT := 0;





                         END;
                    END
                    ELSE
                    BEGIN
                         K := 0;
                    END;
               END;
               OLDB := BUF2[J];
               INC(FPOZ);
          END;
     END;

IF FILESIZE(FTARGET) > FILESIZE(FSOURCE) THEN
BEGIN

     LOOPS := (FILESIZE(FTARGET) - TILL) DIV BUFSIZE;
     LEFTOVER := (FILESIZE(FTARGET) - TILL) MOD BUFSIZE;
     FOR I := 1 TO LOOPS + 1 DO
     BEGIN
          CHECKABORT;
          PROGRESS(FILEPOS(FTARGET), FILESIZE(FTARGET));
          TBUFSIZE := BUFSIZE;
          IF I = LOOPS + 1 THEN TBUFSIZE := LEFTOVER;
          SEEK(FTARGET, TILL + ((I - 1) * BUFSIZE));
          BLOCKREAD(FTARGET, BUF2, TBUFSIZE);
          FOR J := 1 TO TBUFSIZE DO
          BEGIN

                    IF K = 0 THEN
                    BEGIN
                         BEGINOFFS := FPOZ;
                         IF BEGINOFFS = EOFVAL THEN BEGINOFFS := EOFVAL - 1;
                         RLECOUNT := 0;
                    END;
                    INC(K);
                    IF FPOZ = 0 THEN OLDB := NOT BUF2[J];
                    IF BUF2[J] = OLDB THEN
                    BEGIN
                         IF RLECOUNT = 0 THEN
                         BEGIN
                              RLEPOZ := FPOZ - 1;
                              RLECOUNT := 1;
                         END;
                         INC(RLECOUNT);
                         IF RLECOUNT = MAXBLOCK THEN
                         BEGIN
                              {WRITE RLE BLOCK...}
                              IF RLEPOZ < BEGINOFFS THEN INC(RLEPOZ);
{                              WRITELN('Writing RLE block from offset ', RLEPOZ, ' delta ', RLECOUNT, '.');}
                              WRITEPOINTER(BEGINOFFS);
                              BLOCKWRITE(FIPS, NULLWORD, 2); {16 BIT NULL SIZE}
                              RLECOUNT := SWAP(RLECOUNT);
                              BLOCKWRITE(FIPS, RLECOUNT, 2); {16 BIT RLE SIZE}
                              RLECOUNT := SWAP(RLECOUNT);
                              BLOCKWRITE(FIPS, OLDB, 1); {8 BIT BYTE TO BE RLE'D}
                              INC(BLOCKNO);
                              K := 1;
                              BEGINOFFS := FPOZ;
                              IF BEGINOFFS = EOFVAL THEN BEGINOFFS := EOFVAL - 1;
                              RLECOUNT := 0;
                         END;
                    END
                    ELSE
                    BEGIN
                         IF RLECOUNT > 2 THEN
                         BEGIN
                              {WRITE BLOCK BEFORE RLE BLOCK, IF NEEDED}
                              IF RLEPOZ < BEGINOFFS THEN INC(RLEPOZ);
                              WRITEPOINTER(BEGINOFFS);
                              K := RLEPOZ - BEGINOFFS;
                              IF K > 0 THEN
                              BEGIN {WRITE BLOCK BEFORE}
{                                   WRITELN('Writing block from offset ', BEGINOFFS, ' delta ', K, '.');}
                                   K := SWAP(K); {16 BIT LENGTH}
                                   BLOCKWRITE(FIPS, K, 2);
                                   K := SWAP(K);
                                   COPYIT(BEGINOFFS, K);
                                   WRITEPOINTER(RLEPOZ);
                                   BLOCKWRITE(FIPS, NULLWORD, 2);
                                   INC(BLOCKNO);
{                                   WRITELN('Writing RLE block from offset ', RLEPOZ, ' delta ', RLECOUNT, '.');}
                              END
                              ELSE
                              BEGIN
{                                   WRITELN('Writing RLE block from offset ', RLEPOZ, ' delta ', RLECOUNT, '.');}
                                   K := 0;
                                   BLOCKWRITE(FIPS, K, 2);
                              END;
                              {WRITE RLE BLOCK, RESET K}
                              RLECOUNT := SWAP(RLECOUNT);
                              BLOCKWRITE(FIPS, RLECOUNT, 2); {16 BIT RLE SIZE}
                              RLECOUNT := SWAP(RLECOUNT);
                              BLOCKWRITE(FIPS, OLDB, 1); {8 BIT BYTE TO BE RLE'D}
                              INC(BLOCKNO);
                              BEGINOFFS := FPOZ;
                              IF BEGINOFFS = EOFVAL THEN BEGINOFFS := EOFVAL - 1;
                              K := 1;
                         END;
                         RLECOUNT := 0;
                    END;
                    IF K = MAXBLOCK THEN
                    BEGIN
                         IF RLECOUNT > 2 THEN
                         BEGIN
                              WRITEPOINTER(BEGINOFFS);
                              {WRITE BLOCK BEFORE RLE BLOCK, IF NEEDED}
                              IF RLEPOZ < BEGINOFFS THEN INC(RLEPOZ);
                              K := RLEPOZ - BEGINOFFS;
                              IF K > 0 THEN
                              BEGIN {WRITE BLOCK BEFORE}
{                                   WRITELN('Writing block from offset ', BEGINOFFS, ' delta ', K, '.');}
                                   K := SWAP(K); {16 BIT LENGTH}
                                   BLOCKWRITE(FIPS, K, 2);
                                   K := SWAP(K);
                                   COPYIT(BEGINOFFS, K);
                                   WRITEPOINTER(RLEPOZ);
                                   BLOCKWRITE(FIPS, NULLWORD, 2);
                                   INC(BLOCKNO);
{                                   WRITELN('Writing RLE block from offset ', RLEPOZ, ' delta ', RLECOUNT, '.');}
                              END
                              ELSE
                              BEGIN
                                   BLOCKWRITE(FIPS, K, 2);
{                                   WRITELN('Writing RLE block from offset ', RLEPOZ, ' delta ', RLECOUNT, '.');}
                              END;
                              {WRITE RLE BLOCK, RESET K}
                              RLECOUNT := SWAP(RLECOUNT);
                              BLOCKWRITE(FIPS, RLECOUNT, 2); {16 BIT RLE SIZE}
                              RLECOUNT := SWAP(RLECOUNT);
                              BLOCKWRITE(FIPS, OLDB, 1); {8 BIT BYTE TO BE RLE'D}
                              BEGINOFFS := FPOZ;
                              IF BEGINOFFS = EOFVAL THEN BEGINOFFS := EOFVAL - 1;
                              K := 1;
                              RLECOUNT := 0;
                              INC(BLOCKNO);
                         END
                         ELSE
                         BEGIN
                              {WRITE BLOCK, RESET K}
{                              INC(K);}
{                              WRITELN('Writing block from offset ', BEGINOFFS, ' delta ', K, '.');}
                              WRITEPOINTER(BEGINOFFS);
                              K := SWAP(K); {16 BIT LENGTH}
                              BLOCKWRITE(FIPS, K, 2);
                              K := SWAP(K);
                              COPYIT(BEGINOFFS, K);
                              BEGINOFFS := FPOZ;
                              IF BEGINOFFS = EOFVAL THEN BEGINOFFS := EOFVAL - 1;
                              K := 1;
                              INC(BLOCKNO);
                         END;
                         RLECOUNT := 0;
                    END;

               OLDB := BUF2[J];
               INC(FPOZ);
          END;
     END;
END;

    IF K > 0 THEN
    BEGIN
                         IF RLECOUNT > 2 THEN
                         BEGIN

                              {WRITE BLOCK BEFORE RLE BLOCK, IF NEEDED}
                              IF RLEPOZ < BEGINOFFS THEN INC(RLEPOZ);
                              WRITEPOINTER(BEGINOFFS);
                              K := RLEPOZ - BEGINOFFS;
                              IF K > 0 THEN
                              BEGIN {WRITE BLOCK BEFORE}
{                                   WRITELN('Writing block from offset ', BEGINOFFS, ' delta ', K, '.');}
                                   K := SWAP(K); {16 BIT LENGTH}
                                   BLOCKWRITE(FIPS, K, 2);
                                   K := SWAP(K);
                                   COPYIT(BEGINOFFS, K);
                                   WRITEPOINTER(RLEPOZ);
                                   BLOCKWRITE(FIPS, NULLWORD, 2);
                                   INC(BLOCKNO);
{                                   WRITELN('Writing RLE block from offset ', RLEPOZ, ' delta ', RLECOUNT, '.');}
                              END
                              ELSE
                              BEGIN
                                   BLOCKWRITE(FIPS, K, 2);
{                                   WRITELN('Writing RLE block from offset ', RLEPOZ, ' delta ', RLECOUNT, '.');}
                              END;
                              {WRITE RLE BLOCK, RESET K}
                              RLECOUNT := SWAP(RLECOUNT);
                              BLOCKWRITE(FIPS, RLECOUNT, 2); {16 BIT RLE SIZE}
                              RLECOUNT := SWAP(RLECOUNT);
                              BLOCKWRITE(FIPS, OLDB, 1); {8 BIT BYTE TO BE RLE'D}
                              BEGINOFFS := FPOZ;
                              IF BEGINOFFS = EOFVAL THEN BEGINOFFS := EOFVAL - 1;
                              K := 1;
                              RLECOUNT := 0;
                              INC(BLOCKNO);
                         END
                         ELSE
                         BEGIN
                              {WRITE BLOCK, RESET K}
{                              INC(K);}
{                              WRITELN('Writing block from offset ', BEGINOFFS, ' delta ', K, '.');}
                              WRITEPOINTER(BEGINOFFS);
                              K := SWAP(K); {16 BIT LENGTH}
                              BLOCKWRITE(FIPS, K, 2);
                              K := SWAP(K);
                              COPYIT(BEGINOFFS, K);
                              BEGINOFFS := FPOZ;
                              IF BEGINOFFS = EOFVAL THEN BEGINOFFS := EOFVAL - 1;
                              K := 1;
                              INC(BLOCKNO);
                         END;
                         RLECOUNT := 0;
    END;

{     WRITELN('Writing EOF.');}
     S := 'EOF';
     BLOCKWRITE(FIPS, S[1], 3);
     CLOSE(FIPS);
     CLOSE(FSOURCE);
     CLOSE(FTARGET);
END;

BEGIN
     WRITELN('IPSIT v0.1 by alamone (alamone@iname.com).');
     IF PARAMCOUNT < 3 THEN SYNTAX;
     BORED := TRUE;
     S := PARAMSTR(1);
     FOR I := 1 TO LENGTH(S) - 1 DO
     BEGIN
          IF S[I] = '-' THEN
             BEGIN
                  BORED := FALSE;
                  J := I;
                  I := LENGTH(S) - 1;
             END;
          IF S[I] = '/' THEN
             BEGIN
                  BORED := FALSE;
                  J := I;
                  I := LENGTH(S) - 1;
             END;
     END;
     IF BORED = TRUE THEN SYNTAX;
     COMMAND := ORD(S[J+1]);
     IF ((COMMAND > 96) AND (COMMAND < 123)) THEN DEC(COMMAND, 32);
     CASE CHR(COMMAND) OF
         'A':
         begin
              S := PARAMSTR(2);
              S := UCASE(S);
              EXT := S[LENGTH(S)-2] + S[LENGTH(S)-1] + S[LENGTH(S)];
              IF EXT = 'IPS' THEN FILE2 := S ELSE FILE1 := S;
              S := PARAMSTR(3);
              S := UCASE(S);
              EXT := S[LENGTH(S)-2] + S[LENGTH(S)-1] + S[LENGTH(S)];
              IF EXT = 'IPS' THEN FILE2 := S ELSE FILE1 := S;
              IF FILE1 = '' THEN SYNTAX;
              IF FILE2 = '' THEN SYNTAX;
              WRITE('* Apply: patch ', FILE2, ', target ', FILE1, ':     ');
              APPLYPATCH(FILE1, FILE2);
              GOTOXY(WHEREX - 4, WHEREY);
              WRITELN('100%');
         end;
         'C':
         begin
              FILE1 := PARAMSTR(2);
              FILE1 := UCASE(FILE1);
              FILE2 := PARAMSTR(3);
              FILE2 := UCASE(FILE2);
              FILE3 := PARAMSTR(4);
              FILE3 := UCASE(FILE3);
              IF FILE1 = '' THEN SYNTAX;
              IF FILE2 = '' THEN SYNTAX;
              IF FILE3 = '' THEN SYNTAX;
              WRITE('* Create: patch ',FILE3,', source ', FILE1, ', target ', FILE2,':     ');
              CREATEPATCH(FILE1, FILE2, FILE3);
              GOTOXY(WHEREX - 4, WHEREY);
              WRITELN('100%');
         end;
         'F':
         begin
              S := PARAMSTR(2);
              S := UCASE(S);
              EXT := S[LENGTH(S)-2] + S[LENGTH(S)-1] + S[LENGTH(S)];
              IF EXT = 'IPS' THEN FILE2 := S ELSE FILE1 := S;
              S := PARAMSTR(3);
              S := UCASE(S);
              EXT := S[LENGTH(S)-2] + S[LENGTH(S)-1] + S[LENGTH(S)];
              IF EXT = 'IPS' THEN FILE2 := S ELSE FILE1 := S;
              IF FILE1 = '' THEN SYNTAX;
              IF FILE2 = '' THEN SYNTAX;
              WRITE('* Create: patch ', FILE2, ', FC input ', FILE1, ':     ');
              PARSEFC(FILE1, FILE2);
              GOTOXY(WHEREX - 4, WHEREY);
              WRITELN('100%');
         end;
         else syntax;
     end;
END.