DECLARE FUNCTION FJulian$ (d&)
DECLARE FUNCTION TJulian& (d$)
DECLARE FUNCTION DayWeek$ (d$)
DECLARE FUNCTION MonMath$ (d$, N%)
DECLARE FUNCTION DayMath$ (d$, d%)
DECLARE FUNCTION DateDif& (F$, t$)

FUNCTION DateDif& (F$, t$)
    DateDif& = TJulian&(t$) - TJulian&(F$)
END FUNCTION

FUNCTION DayMath$ (d$, d%)
     DayMath$ = FJulian$(TJulian&(d$) + d%)
END FUNCTION

FUNCTION DayWeek$ (d$)
    J = TJulian&(d$) MOD 7
    DayWeek$ = MID$("Monday    Tuesday   Wednesday Thursday  Friday    Saturday  Sunday    ", 1 + J * 10, 10)
END FUNCTION

DEFLNG A-Z
FUNCTION FJulian$ (J)
        t = J - 1721119
        y = INT((4 * t - 1) / 146097)
        t = 4 * t - 1 - 146097 * y
        d = INT(t / 4)
        t = INT((4 * d + 3) / 1461)
        d = 4 * d + 3 - 1461 * t
        d = INT((d + 4) / 4)
        m = INT((5 * d - 3) / 153)
        d = 5 * d - 3 - 153 * m
        d = INT((d + 5) / 5)
        y = 100 * y + t
        IF m < 10 THEN
           m = m + 3
        ELSE
           m = m - 9
           y = y + 1
        END IF
        d$ = RIGHT$("00" + MID$(STR$(d), 2), 2)
        m$ = RIGHT$("00" + MID$(STR$(m), 2), 2)
        FJulian$ = m$ + "-" + d$ + "-" + RIGHT$(STR$(y), 4)
END FUNCTION

DEFINT A-Z
FUNCTION MonMath$ (d$, N)
      y = VAL(MID$(d$, 7, 4))
      m = VAL(MID$(d$, 1, 2)) + N
      WHILE m > 12
         y = y + m \ 12
         m = m MOD 12
      WEND
      m$ = RIGHT$("00" + MID$(STR$(m), 2), 2)
      MonMath$ = m$ + MID$(d$, 3, 4) + RIGHT$(STR$(y), 4)
END FUNCTION

DEFLNG A-Z
FUNCTION TJulian& (d$)
    m = VAL(MID$(d$, 1, 2))
    d = VAL(MID$(d$, 4, 2))
    y = VAL(MID$(d$, 7, 4))
    IF m > 2 THEN
       T1 = m - 3
       T2 = y
    ELSE
       T1 = m + 9
       T2 = y - 1
    END IF
    T3 = INT(T2 / 100): T4 = T2 - 100 * T3
    TJulian& = INT(146097 * T3 / 4) + d + INT(1461 * T4 / 4) + 1721119 + INT((153 * T1 + 2) / 5)
END FUNCTION

