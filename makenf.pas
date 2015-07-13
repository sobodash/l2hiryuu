program makenf;
const
{  .... 0
 9 ...* 1
 a ..*. 2
 b ..** 3
 c .*.. 4
 d .*.* 5
 e .**. 6
 f .*** 7
   *... 8}
   {*******-
    *-------
    *-------
    *******-
    ------*-
    ------*-
    *******-
    --------}
   FONT:array[0..$F, 0..7] of byte=(
{0}($FE,$82,$82,$82,$82,$82,$FE,$00),
{1}($10,$30,$50,$10,$10,$10,$FE,$00),
{2}($FE,$02,$02,$FE,$80,$80,$FE,$00),
{3}($FE,$02,$02,$FE,$02,$02,$FE,$00),
{4}($82,$82,$82,$FE,$02,$02,$02,$00),
{5}($FE,$80,$80,$FE,$02,$02,$FE,$00),
{6}($FE,$80,$80,$FE,$82,$82,$FE,$00),
{7}($FE,$02,$02,$02,$02,$02,$02,$00),
{8}($FE,$82,$82,$FE,$82,$82,$FE,$00),
{9}($FE,$82,$82,$FE,$02,$02,$02,$00),
{A}($FE,$82,$82,$FE,$82,$82,$82,$00),
{B}($F8,$84,$82,$FC,$82,$84,$F8,$00),
{C}($FE,$80,$80,$80,$80,$80,$FE,$00),
{D}($F8,$84,$82,$82,$82,$84,$F8,$00),
{E}($FE,$80,$80,$FE,$80,$80,$FE,$00),
{F}($FE,$80,$80,$FE,$80,$80,$80,$00));
VAR
   I,J:WORD;
   BUF:ARRAY[0..63] OF BYTE;
   BANK,BYTE1,BYTE2:BYTE;
   F:FILE;
begin
     BANK:=0;
     BYTE1:=0;
     BYTE2:=0;
     ASSIGN(F, 'numbers.fnt');
     REWRITE(F, 1);
     FOR I := 1 TO 32 DO
     BEGIN
          BUF[(I*2)-1] := $FF;
     END;
     FOR I := 0 TO 1091 DO
     BEGIN
          FOR J := 0 TO 7 DO
          BEGIN
               BUF[ 0+(J*2)] := FONT[0,J];
               BUF[16+(J*2)] := FONT[BANK,J];
               BUF[32+J*2] := FONT[BYTE2,J];
               BUF[48+(J*2)] := FONT[BYTE1,J];
          END;
          BLOCKWRITE(F, BUF, 64);
          INC(BYTE1);
          IF BYTE1 = $10 THEN
          BEGIN
               BYTE1 := 0;
               INC(BYTE2);
               IF BYTE2 = $10 THEN
               BEGIN
                    BYTE2 := 0;
                    INC(BANK);
               END;
          END;
     END;
     CLOSE(F);
end.