DEFINT A-Z
DECLARE SUB Picture (r, c, s, f$)
DECLARE SUB UnCalcAttr (f, b, BYVAL a)
DIM SHARED Ndm(12) AS INTEGER

SUB EntryD (r, c, e, b, f$)
    '[date field entry]
     Ndm(1) = 31: Ndm(4) = 30: Ndm(7) = 31: Ndm(10) = 31
     Ndm(2) = 28: Ndm(5) = 31: Ndm(8) = 31: Ndm(11) = 30
     Ndm(3) = 31: Ndm(6) = 30: Ndm(9) = 30: Ndm(12) = 31
     e = 0: b = 0: f = 1: s = 10
     a = SCREEN(r, c, 1)
     COLOR 15, 1: LOCATE r, c, 0: PRINT f$;
d10: LOCATE r, c + X, 1
     IF X = 2 THEN PRINT "-"; :   X = X + 1
     IF X = 5 THEN PRINT "-20"; : X = X + 3
     DO: k$ = INKEY$: LOOP WHILE k$ = ""
     SELECT CASE k$
        CASE CHR$(27)      '---------------Esc
             e = 1: GOTO d30
        CASE CHR$(0) + "M" '---------------Right
             f = 0
             X = X + 1
             IF X > s - 1 THEN X = s - 1
        CASE CHR$(0) + "K" '---------------Left
             IF X = 3 OR X = 6 THEN X = X - 2 ELSE X = X - 1
             IF X < 0 THEN X = 0
        CASE CHR$(0) + "H" '---------------Up
             b = 1
             GOTO d30
        CASE CHR$(0) + "P" '---------------Down
             GOTO d20
        CASE CHR$(8)       '---------------Backspace
             IF f THEN b = 1: GOTO d30
             IF X = 3 OR X = 6 THEN X = X - 2 ELSE X = X - 1
             PRINT " ";
             IF X < 0 THEN b = 1: GOTO d30
        CASE CHR$(13)      '---------------Enter
             GOTO d20
        CASE IS < CHR$(31) '---------------
        CASE ELSE
             IF f THEN
                PRINT SPC(s); : LOCATE r, c: f = 0
                IF k$ = " " GOTO d20
             END IF
             IF INSTR(" 0123456789", k$) = 0 GOTO d10
             LOCATE r, c + X: PRINT k$;
             X = X + 1
             IF X > s - 1 THEN X = s - 1
     END SELECT
     GOTO d10
d20: Picture r, c, s, f$
     IF f$ > "0" THEN
        m = VAL(LEFT$(f$, 2))
        d = VAL(MID$(f$, 4, 2))
        y = VAL(RIGHT$(f$, 2))
        IF (y MOD 4) THEN Ndm(2) = 28 ELSE Ndm(2) = 29
        IF m > 12 OR m < 1 THEN X = 0: BEEP: GOTO d10
        IF d > Ndm(m) OR d < 1 THEN X = 2: BEEP: GOTO d10
        IF y > 99 THEN X = 6: BEEP: GOTO d10
     END IF
d30: CALL UnCalcAttr(fc, bc, a)
     COLOR fc, bc: LOCATE r, c, 0: PRINT f$;
END SUB

SUB EntryN (r, c, s, d, e, b, f$)
     e = 0: b = 0: f = 1
     p$ = STRING$(s, "#"): IF d > 0 THEN MID$(p$, s - d, 1) = "."
     c$ = STRING$(s, "9"): IF d > 0 THEN MID$(c$, s - d, 1) = "."
     a = SCREEN(r, c, 1)
     COLOR 15, 1: LOCATE r, c, 0: PRINT f$;
n10: LOCATE r, c + X, 1
     DO: k$ = INKEY$: LOOP WHILE k$ = ""
     SELECT CASE k$
        CASE CHR$(27)      '---------------Esc
             e = 1: GOTO n30
        CASE CHR$(0) + "M" '---------------Right
             f = 0
             X = X + 1
             IF X > s - 1 THEN X = s - 1
        CASE CHR$(0) + "K" '---------------Left
             X = X - 1
             IF X < 0 THEN X = 0
        CASE CHR$(0) + "H" '---------------Up
             b = 1
             GOTO n20
        CASE CHR$(0) + "P" '---------------Down
             GOTO n20
        CASE CHR$(8)       '---------------Backspace
             IF f THEN b = 1: GOTO n20
             X = X - 1
             PRINT " ";
             IF X < 0 THEN b = 1: GOTO n20
        CASE CHR$(13)      '---------------Enter
             GOTO n20
        CASE IS < CHR$(32) '---------------
        CASE ELSE
             IF f THEN
                PRINT SPC(s); : LOCATE r, c: f = 0
                IF k$ = " " GOTO n20
             END IF
             IF INSTR("-0123456789.", k$) = 0 GOTO n10
             LOCATE r, c + X: PRINT k$;
             X = X + 1
             IF X > s - 1 THEN X = s - 1
     END SELECT
     GOTO n10
n20: Picture r, c, s, f$
     IF f$ <> SPACE$(s) THEN
        IF VAL(f$) > VAL(c$) THEN BEEP: X = s - 1: GOTO n10
        LOCATE r, c: PRINT USING p$; VAL(f$);
        Picture r, c, s, f$
     END IF
n30: CALL UnCalcAttr(fc, bc, a)
     COLOR fc, bc: LOCATE r, c, 0: PRINT f$;
END SUB

SUB EntryS (r, c, s, e, b, f$)
    '[any case string entry]
     e = 0: b = 0: f = 1
     a = SCREEN(r, c, 1)
     COLOR 15, 1: LOCATE r, c, 0: PRINT f$;
s10: LOCATE r, c + X, 1
     DO: k$ = INKEY$: LOOP WHILE k$ = ""
     SELECT CASE k$
        CASE CHR$(27)      '---------------Esc
             e = 1: GOTO s30
        CASE CHR$(0) + "M" '---------------Right
             f = 0
             X = X + 1
             IF X > s - 1 THEN X = s - 1
        CASE CHR$(0) + "K" '---------------Left
             X = X - 1
             IF X < 0 THEN X = 0
        CASE CHR$(0) + "H" '---------------Up
             b = 1
             GOTO s20
        CASE CHR$(0) + "P" '---------------Down
             GOTO s20
        CASE CHR$(8)       '---------------Backspace
             IF f THEN b = 1: GOTO s20
             PRINT " ";
             X = X - 1
             IF X < 0 THEN b = 1: GOTO s20
        CASE CHR$(13)      '---------------Enter
             GOTO s20
        CASE IS < CHR$(32) '---------------
        CASE ELSE
             IF f THEN
                PRINT SPC(s); : LOCATE r, c: f = 0
                IF k$ = " " GOTO s20
             END IF
             LOCATE r, c + X: PRINT k$;
             X = X + 1
             IF X > s - 1 THEN X = s - 1
     END SELECT
     GOTO s10
s20: Picture r, c, s, f$
s30: CALL UnCalcAttr(fc, bc, a)
     COLOR fc, bc: LOCATE r, c, 0: PRINT f$;
END SUB

SUB EntryT (r, c, e, b, f$)
    '[telephone number entry]
     e = 0: b = 0: f = 1: s = 12
     a = SCREEN(r, c, 1)
     COLOR 15, 1: LOCATE r, c, 0: PRINT f$;
t10: LOCATE r, c + X, 1
     IF X = 3 THEN PRINT "-"; : X = X + 1
     IF X = 7 THEN PRINT "-"; : X = X + 1
     DO: k$ = INKEY$: LOOP WHILE k$ = ""
     SELECT CASE k$
        CASE CHR$(27)      '---------------Esc
             e = 1: GOTO t30
        CASE CHR$(0) + "M" '---------------Right
             f = 0
             X = X + 1
             IF X > s - 1 THEN X = s - 1
        CASE CHR$(0) + "K" '---------------Left
             IF X = 4 OR X = 8 THEN X = X - 2 ELSE X = X - 1
             IF X < 0 THEN X = 0
        CASE CHR$(0) + "H" '---------------Up
             b = 1
             GOTO t20
        CASE CHR$(0) + "P" '---------------Down
             GOTO t20
        CASE CHR$(8)       '---------------Backspace
             IF f THEN b = 1: GOTO t20
             IF X = 4 OR X = 8 THEN X = X - 2 ELSE X = X - 1
             PRINT " ";
             IF X < 0 THEN b = 1: GOTO t20
        CASE CHR$(13)      '---------------Enter
             GOTO t20
        CASE IS < CHR$(32) '---------------
        CASE ELSE
             IF f THEN
                PRINT SPC(s); : LOCATE r, c: f = 0
                IF k$ = " " GOTO t20
             END IF
             IF INSTR("0123456789", k$) = 0 GOTO t10
             LOCATE r, c + X: PRINT k$;
             X = X + 1
             IF X > s - 1 THEN X = s - 1
     END SELECT
     GOTO t10
t20: Picture r, c, s, f$
t30: CALL UnCalcAttr(fc, bc, a)
     COLOR fc, bc: LOCATE r, c, 0: PRINT f$;
END SUB

SUB EntryU (r, c, s, e, b, f$)
    '[upper case string entry]
     e = 0: b = 0: f = 1
     a = SCREEN(r, c, 1)
     COLOR 15, 1: LOCATE r, c, 0: PRINT f$;
u10: LOCATE r, c + X, 1
     DO: k$ = INKEY$: LOOP WHILE k$ = ""
     SELECT CASE k$
        CASE CHR$(27)      '---------------Esc
             e = 1: GOTO u30
        CASE CHR$(0) + "M" '---------------Right
             f = 0
             X = X + 1
             IF X > s - 1 THEN X = s - 1
        CASE CHR$(0) + "K" '---------------Left
             X = X - 1
             IF X < 0 THEN X = 0
        CASE CHR$(0) + "H" '---------------Up
             b = 1
             GOTO u20
        CASE CHR$(0) + "P" '---------------Down
             GOTO u20
        CASE CHR$(8)       '---------------Backspace
             IF f THEN b = 1: GOTO u20
             PRINT " ";
             X = X - 1
             IF X < 0 THEN b = 1: GOTO u20
        CASE CHR$(13)      '---------------Enter
             GOTO u20
        CASE IS < CHR$(32) '---------------
        CASE ELSE
             IF f THEN
                PRINT SPC(s); : LOCATE r, c: f = 0
                IF k$ = " " GOTO u20
             END IF
             LOCATE r, c + X: PRINT UCASE$(k$);
             X = X + 1
             IF X > s - 1 THEN X = s - 1
     END SELECT
     GOTO u10
u20: Picture r, c, s, f$
u30: CALL UnCalcAttr(fc, bc, a)
     COLOR fc, bc: LOCATE r, c, 0: PRINT f$;
END SUB

SUB EntryX (r, c, e, b, f$)
    '[social security number]
     e = 0: b = 0: f = 1: s = 11
     a = SCREEN(r, c, 1)
     COLOR 15, 1: LOCATE r, c, 0: PRINT f$;
x10: LOCATE r, c + X, 1
     IF X = 3 THEN PRINT "-"; : X = X + 1
     IF X = 6 THEN PRINT "-"; : X = X + 1
     DO: k$ = INKEY$: LOOP WHILE k$ = ""
     SELECT CASE k$
        CASE CHR$(27)      '---------------Esc
             e = 1: GOTO x30
        CASE CHR$(0) + "M" '---------------Right
             f = 0
             X = X + 1
             IF X > s - 1 THEN X = s - 1
        CASE CHR$(0) + "K" '---------------Left
             IF X = 4 OR X = 7 THEN X = X - 2 ELSE X = X - 1
             IF X < 0 THEN X = 0
        CASE CHR$(0) + "H" '---------------Up
             b = 1
             GOTO x20
        CASE CHR$(0) + "P" '---------------Down
             GOTO x20
        CASE CHR$(8)       '---------------Backspace
             IF f THEN b = 1: GOTO x20
             IF X = 4 OR X = 7 THEN X = X - 2 ELSE X = X - 1
             PRINT " ";
             IF X < 0 THEN b = 1: GOTO x20
        CASE CHR$(13)      '---------------Enter
             GOTO x20
        CASE IS < CHR$(32) '---------------
        CASE ELSE
             IF f THEN
                PRINT SPC(s); : LOCATE r, c: f = 0
                IF k$ = " " GOTO x20
             END IF
             IF INSTR("0123456789", k$) = 0 GOTO x10
             LOCATE r, c + X: PRINT k$;
             X = X + 1
             IF X > s - 1 THEN X = s - 1
     END SELECT
     GOTO x10
x20: Picture r, c, s, f$
x30: CALL UnCalcAttr(fc, bc, a)
     COLOR fc, bc: LOCATE r, c, 0: PRINT f$;
END SUB

SUB Picture (r, c, s, f$)
    f$ = ""
    FOR X = c TO c + s - 1
    f$ = f$ + CHR$(SCREEN(r, X))
    NEXT
END SUB

