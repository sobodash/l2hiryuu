{$S 65535}
program bioPROJect; {main}
{written by Albert lee in June 1996}

uses CRT, DOS;

{CONST}
{
VAR
   Happygfx:array[0..20, 0..16] OF BOOLEAN;
   Sadgfx:array[0..20, 0..16] OF BOOLEAN;
   Elecgfx:array[0..10, 0..8] OF BOOLEAN;
   atomelec:array[1..maxat] OF BYTE;
   atompro:array[1..maxat] of BYTE;
   atomnew:array[1..maxat] of BYTE;
   atompos:array[1..maxat, 1..2] of INTEGER;
   atomsymb:array[1..maxat] of STRING;
   atomelecinfo:array[1..maxat, 1..40] of longint;
   flags:ARRAY[0..10] of boolean;
}

PROCEDURE SEARCHIT;
CONST
   BUFSIZE = 1000;
   LOOPS = (2097152 DIV BUFSIZE) - 1;

VAR
   F:FILE;
   B:BYTE;
   I,J,K,L:INTEGER;
   T:LONGINT;
   LI:LONGINT;
   OLDBUF:array[0..BUFSIZE-1] of BYTE;
   BUF:array[0..BUFSIZE-1] of BYTE;
   BO:ARRAY[1..8] OF BOOLEAN;
   S:STRING;

BEGIN{2097664}
   ASSIGN (F, 'LUNGRIS2.BIN');
   RESET (F, 1);


   CLRSCR;
   L:=0;

   FOR I := 0 TO LOOPS DO
       BEGIN
            BLOCKREAD (F, BUF, BUFSIZE);
            FOR J := 0 TO BUFSIZE-8 DO
            BEGIN
            FOR K := 1 TO 8 DO
            BEGIN
                 BO[K] := FALSE;
            END;
{            IF BUF[J] = $B5 THEN BO[1] := TRUE;}
{            IF BUF[J] = $B1 THEN BO[1] := TRUE;}
            IF BUF[J] = BUF[J+2] + 2 THEN BO[1] := TRUE;
            IF BUF[J] = BUF[J+4] - 3 THEN BO[2] := TRUE;
            IF BUF[J] = BUF[J+6] - 8 THEN BO[3] := TRUE;
            IF BUF[J] = BUF[J+8] + 5 THEN BO[4] := TRUE;
{            IF BUF[J+1] = BUF[J+3] THEN BO[6] := TRUE;
            IF BUF[J+3] = BUF[J+5] THEN BO[7] := TRUE;
            IF BUF[J+5] = BUF[J+7] THEN BO[8] := TRUE;
}
{
            IF BUF[J+3] = $85 THEN BO[4] := TRUE;
            IF BUF[J+3] = $C0 THEN BO[4] := TRUE;
            IF BUF[J+4] = $A3 THEN BO[5] := TRUE;
            IF BUF[J+4] = $B0 THEN BO[5] := TRUE;
}
            L := 0;
            FOR K := 1 TO 8 DO
            BEGIN
                 IF BO[K] THEN INC(L);
            END;
            IF L > 3 THEN
            BEGIN
                 WRITELN (I,' ', J,'  ',buf[j],'  ',buf[j+1]);
            END;

            end;
       END;

   CLOSE (F);


END;

PROCEDURE PAUSEKEY;
VAR
  C:CHAR;
BEGIN
  REPEAT
  UNTIL KEYPRESSED;

  REPEAT
     C := READKEY;
  UNTIL NOT KEYPRESSED;

END;

PROCEDURE EMPTYKEYBUF;
VAR
A:CHAR;
BEGIN
  REPEAT
     A := READKEY;
  UNTIL NOT KEYPRESSED
END;

BEGIN

SEARCHIT;

END.