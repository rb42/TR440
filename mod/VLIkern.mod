
(****************************************************************************************)
(*											*)
(*	Copyright © 2018	Rainard Buchmann					*)
(*				Software Entwicklung Buchmann, Solingen, Germany	*)
(*											*)
(****************************************************************************************)
(*											*)
(*	This Module is base on the work of Egbert J. van der Haring			*)
(*	for Stony Brook MODULA-2, modified by Stony Brook in 2001			*)
(*											*)
(****************************************************************************************)

IMPLEMENTATION MODULE VLIkern;

(*	10.01.16	*)

<*/OPT:T*>
<*/NOPACK*>

<*/VALIDVER:UseASM*>
<*/VER:UseASM*>

FROM SYSTEM IMPORT
    BYTE,
    ADR, ADDADR, SUBADR,
    ASSERT, FUNC;

FROM ExStorage IMPORT
    ALLOCATE, DeallocateEx, ReallocateEx, DEALLOCATE,
    GetHeap, HeapInfoPointer;

FROM RandomNumbers IMPORT
    RandomHandle, RandomizeEx, SeedEx, RndEx, DisposeRandomHandle;


    ASSERT(SIZE(Digit2) = 2*SIZE(Digit));


TYPE
    CompareResults      = (Less, Equal, Greater);


<*/PUSH/NOOPT:R*>(*IA-32*)
PROCEDURE Allocate(size : CARDINAL) : VLI;
VAR
    vli         : VLI;
BEGIN
    IF (size REM AllocIncrement) <> 0 THEN
        size := size + (AllocIncrement - (size REM AllocIncrement));
    END;
    ALLOCATE(vli, SIZE(VLIrec) - SIZE(VLIrec.digits) + (size*SIZE(Digit)));

    vli^.heap := GetHeap();
    vli^.neg := FALSE;
    vli^.size := size;
    vli^.used := 1;(*always one digit. eliminates a boundary condition*)
    vli^.digits[0] := 0;
    RETURN vli;
END Allocate;

PROCEDURE Grow(VAR INOUT vli : VLI; index : CARDINAL);
VAR
    alloc       : CARDINAL;
BEGIN
    IF index >= vli^.size THEN
        alloc := vli^.size;
        REPEAT
            INC(alloc, AllocIncrement);
        UNTIL alloc > index;
        vli^.size := alloc;

        alloc := SIZE(VLIrec) - SIZE(VLIrec.digits) + (alloc*SIZE(Digit));
        ReallocateEx(vli, alloc, vli^.heap);
    END;
END Grow;
<*/POP*>

PROCEDURE MoveDigits(ptrD, ptrS : DigitsPointer; count : CARDINAL);
BEGIN
    LOOP
        IF count >= 4 THEN
            count := count - 4;
            ptrD^[0] := ptrS^[0];
            ptrD^[1] := ptrS^[1];
            ptrD^[2] := ptrS^[2];
            ptrD^[3] := ptrS^[3];
            ptrD := ADDADR(ptrD, 4*SIZE(Digit));
            ptrS := ADDADR(ptrS, 4*SIZE(Digit));
            IF count = 0 THEN
                EXIT;
            END;
        ELSIF count >= 2 THEN
            count := count - 2;
            ptrD^[0] := ptrS^[0];
            ptrD^[1] := ptrS^[1];
            ptrD := ADDADR(ptrD, 2*SIZE(Digit));
            ptrS := ADDADR(ptrS, 2*SIZE(Digit));
            IF count = 0 THEN
                EXIT;
            END;
        ELSE
            ptrD^[0] := ptrS^[0];
            EXIT;
        END;
    END;
END MoveDigits;

PROCEDURE ZapDigits(ptrD : DigitsPointer; count : CARDINAL);
BEGIN
    LOOP
        IF count >= 4 THEN
            count := count - 4;
            ptrD^[0] := 0;
            ptrD^[1] := 0;
            ptrD^[2] := 0;
            ptrD^[3] := 0;
            ptrD := ADDADR(ptrD, 4*SIZE(Digit));
            IF count = 0 THEN
                EXIT;
            END;
        ELSIF count >= 2 THEN
            count := count - 2;
            ptrD^[0] := 0;
            ptrD^[1] := 0;
            ptrD := ADDADR(ptrD, 2*SIZE(Digit));
            IF count = 0 THEN
                EXIT;
            END;
        ELSE
            ptrD^[0] := 0;
            EXIT;
        END;
    END;
END ZapDigits;

PROCEDURE CreateI() : VLI [INLINE];
BEGIN
    RETURN Allocate(AllocIncrement);
END CreateI;

PROCEDURE Create() : VLI;
BEGIN
    RETURN CreateI();
END Create;

PROCEDURE Dispose(VAR INOUT vli : VLI);
VAR
    alloc       : CARDINAL;
BEGIN
    IF vli <> NIL THEN
        alloc := SIZE(VLIrec) - SIZE(VLIrec.digits) + (vli^.size*SIZE(Digit));
        DeallocateEx(vli, alloc, vli^.heap);
    END;
END Dispose;

PROCEDURE Assign(vliSrc : VLI; VAR INOUT vliDst : VLI);
BEGIN
    IF vliSrc <> vliDst THEN
        IF vliDst^.size < vliSrc^.used THEN
            Grow(vliDst, vliSrc^.used-1);
        END;

        vliDst^.neg := vliSrc^.neg;
        vliDst^.used := vliSrc^.used;
        MoveDigits(ADR(vliDst^.digits), ADR(vliSrc^.digits), vliSrc^.used);
    END;
END Assign;

PROCEDURE IsZeroI(vli : VLI) : BOOLEAN;
BEGIN
    RETURN (vli^.used = 1) AND (vli^.digits[0] = 0);
END IsZeroI;

PROCEDURE IsZero(vli : VLI) : BOOLEAN;
BEGIN
    RETURN IsZeroI(vli);
END IsZero;

PROCEDURE IsNegative(vli : VLI) : BOOLEAN;
BEGIN
    RETURN vli^.neg;
END IsNegative;

PROCEDURE SetZeroI(vli : VLI) [INLINE];
BEGIN
    vli^.neg := FALSE;
    vli^.used := 1;
    vli^.digits[0] := 0;
END SetZeroI;

PROCEDURE SetZero(vli : VLI);
BEGIN
    SetZeroI(vli);
END SetZero;

PROCEDURE Abs(vli : VLI);
BEGIN
    vli^.neg := FALSE;
END Abs;

PROCEDURE SetNegI(vli : VLI; neg : BOOLEAN) [INLINE];
BEGIN
    vli^.neg := FALSE;
    IF NOT IsZeroI(vli) THEN
        vli^.neg := neg;
    END;
END SetNegI;

PROCEDURE GetBitSizeI(vli : VLI) : CARDINAL [INLINE];
BEGIN
    RETURN vli^.used * DigitBits;
END GetBitSizeI;

PROCEDURE SetDigit(VAR INOUT vli : VLI; index : CARDINAL; dig : Digit);
BEGIN
    IF index >= vli^.size THEN
        Grow(vli, index);
    END;

    vli^.digits[index] := dig;

    IF dig > 0 THEN
        IF index >= vli^.used THEN
            vli^.used := index + 1;
        END;
    ELSE
        (* trim leading zeros. leave at least one digit. *)
        WHILE (vli^.used > 1) AND (vli^.digits[vli^.used-1] = 0) DO
            DEC(vli^.used);
        END;
    END;
END SetDigit;

PROCEDURE SetValue(vli : VLI; value : INTEGER64);
VAR
    i   : CARDINAL;
BEGIN
    IF value <> 0 THEN
        vli^.neg := value < 0;
        value := ABS(value);
        vli^.used := 1;
        FOR i := 0 TO (SIZE(value)/SIZE(Digit))-1 DO
            SetDigit(vli, i, value BAND MAX(Digit));
            value := value SHR DigitBits;
        END;
    ELSE
        SetZero(vli);
    END;
END SetValue;

PROCEDURE IsBitSet(vli : VLI; bit : CARDINAL) : BOOLEAN;
VAR
    index       : CARDINAL;
    b           : Digit;
BEGIN
    index := bit / DigitBits;
    bit := bit REM DigitBits;

    IF index < vli^.used THEN
        b := VAL(Digit, 1) SHL bit;
        RETURN (vli^.digits[index] BAND b) <> 0;
    END;
    RETURN FALSE;
END IsBitSet;

PROCEDURE CompareAbs(left, right : VLI) : CompareResults;
VAR
    index       : CARDINAL;
BEGIN
    IF left^.used < right^.used THEN
        RETURN Less;
    ELSIF left^.used > right^.used THEN
        RETURN Greater;
    ELSE
        index := left^.used;
        REPEAT
            DEC(index);

            IF left^.digits[index] < right^.digits[index] THEN
                RETURN Less;
            ELSIF left^.digits[index] > right^.digits[index] THEN
                RETURN Greater;
            END;
        UNTIL index = 0;
        RETURN Equal;
    END;
END CompareAbs;

PROCEDURE CompareDigit(left : VLI; right : DigitS) : CompareResults;
VAR
    rightNeg    : BOOLEAN;
    res         : CompareResults;
BEGIN
    rightNeg := right < 0;
    right := ABS(right);

    IF left^.neg = rightNeg THEN
        res := CompareAbsDig(left, right);
        IF NOT left^.neg THEN
            RETURN res;
        ELSIF res = Greater THEN
            RETURN Less;
        ELSIF res = Less THEN
            RETURN Greater;
        END;
        RETURN Equal;
    ELSE
        IF left^.neg THEN
            RETURN Less;
        END;
        RETURN Greater;
    END;
END CompareDigit;

PROCEDURE CompareAbsDig(left : VLI; right : Digit) : CompareResults;
BEGIN
    IF left^.used > 1 THEN
        RETURN Greater;
    ELSE
        IF left^.digits[0] < right THEN
            RETURN Less;
        ELSIF left^.digits[0] > right THEN
            RETURN Greater;
        END;
        RETURN Equal;
    END;
END CompareAbsDig;

(* performs subtraction *)
(* NB: on entry vliA > vliB, vli = 0 *)
PROCEDURE DoSubtract2(vliA, vliB : VLI; VAR INOUT result : VLI);
VAR
    res         : VLI;
    a,
    bUsed       : CARDINAL;
    borrow,
    bVal,
    aVal        : Digit;
    temp        : Digit2;
BEGIN
    ASSERT(IsZero(result));
    ASSERT(CompareAbs(vliA, vliB) > Equal);

    Grow(result, vliA^.used-1);
    res := result;

    res^.used := vliA^.used;

    bUsed := vliB^.used;
    borrow := 0;
    a := 0;
    REPEAT
        bVal := vliB^.digits[a];
        aVal := vliA^.digits[a];
        <*/PUSH/NOCHECK:O*>
        temp := VAL(Digit2, aVal) - VAL(Digit2, bVal) - VAL(Digit2, borrow);
        res^.digits[a] := temp;
        borrow := VAL(Digit, temp SHR DigitBits) BAND 1;
        <*/POP*>
        INC(a);
    UNTIL a = bUsed;
    WHILE a <> vliA^.used DO
        aVal := vliA^.digits[a];
        <*/PUSH/NOCHECK:O*>
        temp := VAL(Digit2, aVal) - VAL(Digit2, borrow);
        res^.digits[a] := temp;
        borrow := VAL(Digit, temp SHR DigitBits) BAND 1;
        <*/POP*>
        INC(a);
    END;

    (* trim leading zeros. leave at least one digit. *)
    WHILE (res^.used > 1) AND (res^.digits[res^.used-1] = 0) DO
        DEC(res^.used);
    END;
END DoSubtract2;

PROCEDURE DoSubtract(vliA, vliB : VLI; VAR INOUT result : VLI);
VAR
    neg         : BOOLEAN;
    temp        : VLI;
BEGIN
    ASSERT(vliA <> result);
    ASSERT(vliB <> result);

    SetZeroI(result);
    neg := FALSE;

    IF CompareAbs(vliA, vliB) = Less THEN
        neg := TRUE;
        temp := vliA;
        vliA := vliB;
        vliB := temp;
    END;

    DoSubtract2(vliA, vliB, result);

    SetNegI(result, neg);
END DoSubtract;

PROCEDURE DoAdd(vliA, vliB : VLI; VAR INOUT result : VLI);
VAR
    res         : VLI;
    a           : CARDINAL;
    carry       : Digit;
    bUsed       : CARDINAL;
    aVal,
    bVal        : Digit;
    temp        : Digit2;
    tv          : VLI;
BEGIN
    SetZeroI(result);

    IF vliA^.used < vliB^.used THEN
        tv := vliA;
        vliA := vliB;
        vliB := tv;
    END;

    Grow(result, vliA^.used);
    res := result;

    res^.used := vliA^.used+1;

    bUsed := vliB^.used;
    carry := 0;
    a := 0;
    REPEAT
        aVal := vliA^.digits[a];
        bVal := vliB^.digits[a];
        temp := VAL(Digit2, aVal) + VAL(Digit2, bVal) + VAL(Digit2, carry);
        <*/PUSH/NOCHECK:A*>
        res^.digits[a] := temp;
        <*/POP*>
        carry := temp SHR DigitBits;
        INC(a);
    UNTIL a = bUsed;
    WHILE a <> vliA^.used DO
        aVal := vliA^.digits[a];
        <*/PUSH/NOCHECK:O*>
        temp := VAL(Digit2, aVal) + VAL(Digit2, carry);
        res^.digits[a] := temp;
        carry := temp SHR DigitBits;
        <*/POP*>
        INC(a);
    END;
    res^.digits[a] := carry;

    (* trim leading zeros. leave at least one digit. *)
    IF res^.digits[res^.used-1] = 0 THEN
        DEC(res^.used);
    END;
END DoAdd;

PROCEDURE Add(vliA, vliB : VLI; VAR INOUT result : VLI);
BEGIN
    IF vliA^.neg THEN
        IF vliB^.neg THEN
            DoAdd(vliA, vliB, result);(* (-a)+(-b) = -(a+b) *)
            SetNegI(result, TRUE);
        ELSE
            DoSubtract(vliB, vliA, result);(* (-a)+b = b-a *)
        END;
    ELSIF vliB^.neg THEN
        DoSubtract(vliA, vliB, result);(* a+(-b) = a-b *)
    ELSE
        DoAdd(vliA, vliB, result);(* a + b *)
    END;
END Add;

PROCEDURE Subtract(vliA, vliB : VLI; VAR INOUT result : VLI);
BEGIN
    IF vliA^.neg THEN
        IF vliB^.neg THEN
            DoSubtract(vliB, vliA, result);(* (-a)-(-b) = (-a)+b = b-a *)
        ELSE
            DoAdd(vliA, vliB, result);(* (-a)-b = (-a)+(-b) = -(a+b) *)
            SetNegI(result, TRUE);
        END;
    ELSIF vliB^.neg THEN
        DoAdd(vliA, vliB, result);(* a-(-b) = a+b *)
    ELSE
        DoSubtract(vliA, vliB, result);(* a - b *)
    END;
    (*
    vliB^.neg := NOT vliB^.neg;
    Add(vliA, vliB, result);
    vliB^.neg := NOT vliB^.neg;
    *)
END Subtract;

%IF IA32 %AND UseASM %THEN

(* this procedure only works if Digit is 32-bit *)
PROCEDURE MultiplyDigitAdd(ptrA : DigitsPointer;
                           digitB : Digit;
                           ptrR : DigitsPointer;
                           count : CARDINAL;
                           assign : BOOLEAN) : Digit; ASSEMBLER;
(*
  ESI = ptrA
  EDI = ptrR
  ECX = digitB
  EBP = count

  EBX = carry      Initialized to 0

  This procedure loops computing as follows:
  tempDigit2 := (ptrA^[x] * digitB) + ptrR^[x] + carry;
  carry := tempDigit2 SHR DigitBits
  ptr^[x] := tempDigits;


  NOTE: tempDigit2 is accumulated in EDX:EAX. (* SIZE Digit2 *)
        ptrR, ptrA point to a Digit
        carry is a Digit
*)
ASM
        MOV     ESI, ptrA
        MOV     EDI, ptrR
        MOV     ECX, digitB
        MOV     EBX, 0              (* carry *)
        MOV     EAX, count
        PUSH    EBP
        MOV     EBP, EAX
    @Top:
        CMP     EBP, 8
        JB      @Try4

    @Do8:
        MOV     EAX, [ESI]
        SUB     EBP, 8
        MUL     ECX
        ADD     EAX, [EDI]
        ADC     EDX, 0
        ADD     EAX, EBX
        ADC     EDX, 0
        MOV     EBX, EDX
        MOV     [EDI], EAX

        MOV     EAX, [ESI+4]
        MUL     ECX
        ADD     EAX, [EDI+4]
        ADC     EDX, 0
        ADD     EAX, EBX
        ADC     EDX, 0
        MOV     EBX, EDX
        MOV     [EDI+4], EAX

        MOV     EAX, [ESI+8]
        MUL     ECX
        ADD     EAX, [EDI+8]
        ADC     EDX, 0
        ADD     EAX, EBX
        ADC     EDX, 0
        MOV     EBX, EDX
        MOV     [EDI+8], EAX

        MOV     EAX, [ESI+12]
        MUL     ECX
        ADD     EAX, [EDI+12]
        ADC     EDX, 0
        ADD     EAX, EBX
        ADC     EDX, 0
        MOV     EBX, EDX
        MOV     [EDI+12], EAX

        MOV     EAX, [ESI+16]
        MUL     ECX
        ADD     EAX, [EDI+16]
        ADC     EDX, 0
        ADD     EAX, EBX
        ADC     EDX, 0
        MOV     EBX, EDX
        MOV     [EDI+16], EAX

        MOV     EAX, [ESI+20]
        MUL     ECX
        ADD     EAX, [EDI+20]
        ADC     EDX, 0
        ADD     EAX, EBX
        ADC     EDX, 0
        MOV     EBX, EDX
        MOV     [EDI+20], EAX

        MOV     EAX, [ESI+24]
        MUL     ECX
        ADD     EAX, [EDI+24]
        ADC     EDX, 0
        ADD     EAX, EBX
        ADC     EDX, 0
        MOV     EBX, EDX
        MOV     [EDI+24], EAX

        MOV     EAX, [ESI+28]
        MUL     ECX
        ADD     EAX, [EDI+28]
        ADC     EDX, 0
        ADD     EAX, EBX
        ADC     EDX, 0
        MOV     EBX, EDX
        MOV     [EDI+28], EAX

        ADD     ESI, 8*4
        ADD     EDI, 8*4

        CMP     EBP, 8
        JGE     @Do8
        TEST    EBP, EBP
        JLE     @EndLoop

    @Try4:
        CMP     EBP, 4
        JB      @Try2
        SUB     EBP, 4

        MOV     EAX, [ESI]
        MUL     ECX
        ADD     EAX, [EDI]
        ADC     EDX, 0
        ADD     EAX, EBX
        ADC     EDX, 0
        MOV     EBX, EDX
        MOV     [EDI], EAX

        MOV     EAX, [ESI+4]
        MUL     ECX
        ADD     EAX, [EDI+4]
        ADC     EDX, 0
        ADD     EAX, EBX
        ADC     EDX, 0
        MOV     EBX, EDX
        MOV     [EDI+4], EAX

        MOV     EAX, [ESI+8]
        MUL     ECX
        ADD     EAX, [EDI+8]
        ADC     EDX, 0
        ADD     EAX, EBX
        ADC     EDX, 0
        MOV     EBX, EDX
        MOV     [EDI+8], EAX

        MOV     EAX, [ESI+12]
        MUL     ECX
        ADD     EAX, EBX
        ADC     EDX, 0
        ADD     EAX, [EDI+12]
        ADC     EDX, 0
        MOV     EBX, EDX
        MOV     [EDI+12], EAX

        ADD     ESI, 4*4
        ADD     EDI, 4*4

        TEST    EBP, EBP
        JE      @EndLoop

    @Try2:
        CMP     EBP, 2
        JB      @Try1
        SUB     EBP, 2

        MOV     EAX, [ESI]
        MUL     ECX
        ADD     EAX, [EDI]
        ADC     EDX, 0
        ADD     EAX, EBX
        ADC     EDX, 0
        MOV     EBX, EDX
        MOV     [EDI], EAX

        MOV     EAX, [ESI+4]
        MUL     ECX
        ADD     EAX, [EDI+4]
        ADC     EDX, 0
        ADD     EAX, EBX
        ADC     EDX, 0
        MOV     EBX, EDX
        MOV     [EDI+4], EAX

        ADD     ESI, 2*4
        ADD     EDI, 2*4

        TEST    EBP, EBP
        JE      @EndLoop

    @Try1:
        MOV     EAX, [ESI]
        MUL     ECX
        ADD     EAX, [EDI]
        ADC     EDX, 0
        ADD     EAX, EBX
        ADC     EDX, 0
        MOV     [EDI], EAX
        (*MOV     EBX, EDX*)

        ADD     EDI, 4

    @EndLoop:
        POP     EBP

        CMP     assign, 0
        je      @Exit
        MOV     [EDI], EDX

    @Exit:
        MOV     EAX, EDX
END MultiplyDigitAdd;

%ELSE

PROCEDURE MultiplyDigitAdd(ptrA : DigitsPointer;
                           digitB : Digit;
                           ptrR : DigitsPointer;
                           count : CARDINAL;
                           assign : BOOLEAN) : Digit;
VAR
    temp        : Digit2;
    carry       : Digit;
BEGIN
    (* writing the code ugly (pointer arithmetic) causes simpler addressing
       to be used for memory ops.
       ugly is okay for a performance critical loop.
    *)
    carry := 0;
    LOOP
        IF count >= 8 THEN
            count := count - 8;

            temp := (VAL(Digit2, digitB) * VAL(Digit2, ptrA^[0])) +
                     VAL(Digit2, ptrR^[0]) +
                     VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[0] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, digitB) * VAL(Digit2, ptrA^[1])) +
                     VAL(Digit2, ptrR^[1]) +
                     VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[1] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, digitB) * VAL(Digit2, ptrA^[2])) +
                     VAL(Digit2, ptrR^[2]) +
                     VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[2] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, digitB) * VAL(Digit2, ptrA^[3])) +
                     VAL(Digit2, ptrR^[3]) +
                     VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[3] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, digitB) * VAL(Digit2, ptrA^[4])) +
                     VAL(Digit2, ptrR^[4]) +
                     VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[4] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, digitB) * VAL(Digit2, ptrA^[5])) +
                     VAL(Digit2, ptrR^[5]) +
                     VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[5] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, digitB) * VAL(Digit2, ptrA^[6])) +
                     VAL(Digit2, ptrR^[6]) +
                     VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[6] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, digitB) * VAL(Digit2, ptrA^[7])) +
                     VAL(Digit2, ptrR^[7]) +
                     VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[7] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            ptrA := ADDADR(ptrA, 8*SIZE(Digit));
            ptrR := ADDADR(ptrR, 8*SIZE(Digit));
            IF count = 0 THEN
                EXIT;
            END;

        ELSIF count >= 4 THEN
            count := count - 4;

            temp := (VAL(Digit2, digitB) * VAL(Digit2, ptrA^[0])) +
                     VAL(Digit2, ptrR^[0]) +
                     VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[0] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, digitB) * VAL(Digit2, ptrA^[1])) +
                     VAL(Digit2, ptrR^[1]) +
                     VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[1] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, digitB) * VAL(Digit2, ptrA^[2])) +
                     VAL(Digit2, ptrR^[2]) +
                     VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[2] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, digitB) * VAL(Digit2, ptrA^[3])) +
                     VAL(Digit2, ptrR^[3]) +
                     VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[3] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            ptrA := ADDADR(ptrA, 4*SIZE(Digit));
            ptrR := ADDADR(ptrR, 4*SIZE(Digit));
            IF count = 0 THEN
                EXIT;
            END;

        ELSIF count >= 2 THEN
            count := count - 2;

            temp := (VAL(Digit2, digitB) * VAL(Digit2, ptrA^[0])) +
                     VAL(Digit2, ptrR^[0]) +
                     VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[0] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, digitB) * VAL(Digit2, ptrA^[1])) +
                     VAL(Digit2, ptrR^[1]) +
                     VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[1] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            ptrA := ADDADR(ptrA, 2*SIZE(Digit));
            ptrR := ADDADR(ptrR, 2*SIZE(Digit));
            IF count = 0 THEN
                EXIT;
            END;

        ELSE
            temp := (VAL(Digit2, digitB) * VAL(Digit2, ptrA^[0])) +
                     VAL(Digit2, ptrR^[0]) +
                     VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[0] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;
            ptrR := ADDADR(ptrR, SIZE(Digit));
            EXIT;
        END;
    END;

    IF assign THEN
        ptrR^[0] := carry;
    END;

    RETURN carry;
END MultiplyDigitAdd;

%END

PROCEDURE Multiply2(vliA, vliB : VLI; VAR INOUT result : VLI);
VAR
    b           : CARDINAL;
    res         : VLI;
    aUsed,
    bUsed       : CARDINAL;
BEGIN
    ASSERT(IsZero(result));

    Grow(result, vliB^.used + vliA^.used -1);
    result^.used := vliB^.used + vliA^.used;
    ZapDigits(ADR(result^.digits), result^.used);

    res := result;
    bUsed := vliB^.used;
    aUsed := vliA^.used;
    b := 0;
    REPEAT
        FUNC MultiplyDigitAdd(ADR(vliA^.digits),
                              vliB^.digits[b],
                              ADR(res^.digits[b]),
                              aUsed,
                              TRUE);

        INC(b);
    UNTIL b = bUsed;

    (* trim leading zeros. leave at least one digit. *)
    IF res^.digits[res^.used-1] = 0 THEN
        DEC(res^.used);
    END;
END Multiply2;

PROCEDURE SquareDigits(R, A : DigitsPointer; N : CARDINAL) [INLINE];
VAR
    i           : CARDINAL;
    carry       : Digit;
    temp        : Digit2;
    last        : CARDINAL;
    digitB      : Digit;
    ptrR        : DigitsPointer;
BEGIN
    last := N-1;
    i := 0;
    REPEAT
        FUNC MultiplyDigitAdd(ADR(A^[i+1]),
                              A^[i],
                              ADR(R^[i+i+1]),
                              last-i,
                              TRUE);
        INC(i);
    UNTIL i = last;

    ptrR := R;
    carry := 0;
    FOR i := 0 TO (N*2)-1 DO
        digitB := ptrR^[0];
        temp := VAL(Digit2, digitB) + VAL(Digit2, digitB) + VAL(Digit2, carry);
        <*/PUSH/NOCHECK:A*>
        ptrR^[0] := temp;
        <*/POP*>
        carry := temp SHR DigitBits;
        ptrR := ADDADR(ptrR, SIZE(Digit));
    END;

    ptrR := R;
    carry := 0;
    FOR i := 0 TO N-1 DO
        digitB := A^[i];
        temp := (VAL(Digit2, digitB) * VAL(Digit2, digitB)) +
                 VAL(Digit2, ptrR^[0]) +
                 VAL(Digit2, carry);
        <*/PUSH/NOCHECK:A*>
        ptrR^[0] := temp;
        <*/POP*>
        carry := temp SHR DigitBits;
        temp := VAL(Digit2, ptrR^[1]) + VAL(Digit2, carry);
        <*/PUSH/NOCHECK:A*>
        ptrR^[1] := temp;
        <*/POP*>
        carry := temp SHR DigitBits;
        ptrR := ADDADR(ptrR, 2*SIZE(Digit));
    END;
END SquareDigits;

PROCEDURE Square(vliA : VLI; VAR INOUT result : VLI);
VAR
    res         : VLI;
BEGIN
    ASSERT(IsZero(result));

    Grow(result, vliA^.used*2-1);
    result^.used := vliA^.used*2;
    ZapDigits(ADR(result^.digits), result^.used);

    res := result;

    SquareDigits(ADR(res^.digits), ADR(vliA^.digits), vliA^.used);

    (* trim leading zeros. leave at least one digit. *)
    IF res^.digits[res^.used-1] = 0 THEN
        DEC(res^.used);
    END;
END Square;

PROCEDURE MultiplyByDigit(vliA : VLI; digitIn : Digit; VAR INOUT result : VLI);
VAR
    i           : CARDINAL;
    ptrA, ptrR  : DigitsPointer;
    temp        : Digit2;
    carry       : Digit;
    res         : VLI;
BEGIN
    ASSERT(IsZero(result));

    IF digitIn = 0 THEN
        RETURN;
    END;

    Grow(result, vliA^.used+1);
    res := result;

    res^.used := vliA^.used+1;
    res^.digits[0] := 0;

    (* writing the code ugly (pointer arithmetic) causes simpler addressing
       to be used for memory ops.
       ugly is okay for a performance critical loop.
    *)
    ptrA := ADR(vliA^.digits);
    ptrR := ADR(res^.digits);
    i := vliA^.used;
    carry := 0;
    LOOP
        IF i >= 8 THEN
            i := i - 8;

            temp := (VAL(Digit2, ptrA^[0]) * VAL(Digit2, digitIn)) + VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[0] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, ptrA^[1]) * VAL(Digit2, digitIn)) + VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[1] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, ptrA^[2]) * VAL(Digit2, digitIn)) + VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[2] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, ptrA^[3]) * VAL(Digit2, digitIn)) + VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[3] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, ptrA^[4]) * VAL(Digit2, digitIn)) + VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[4] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, ptrA^[5]) * VAL(Digit2, digitIn)) + VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[5] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, ptrA^[6]) * VAL(Digit2, digitIn)) + VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[6] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, ptrA^[7]) * VAL(Digit2, digitIn)) + VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[7] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            ptrA := ADDADR(ptrA, 8*SIZE(Digit));
            ptrR := ADDADR(ptrR, 8*SIZE(Digit));
            IF i = 0 THEN
                EXIT;
            END;

        ELSIF i >= 4 THEN
            i := i - 4;

            temp := (VAL(Digit2, ptrA^[0]) * VAL(Digit2, digitIn)) + VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[0] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, ptrA^[1]) * VAL(Digit2, digitIn)) + VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[1] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, ptrA^[2]) * VAL(Digit2, digitIn)) + VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[2] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, ptrA^[3]) * VAL(Digit2, digitIn)) + VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[3] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            ptrA := ADDADR(ptrA, 4*SIZE(Digit));
            ptrR := ADDADR(ptrR, 4*SIZE(Digit));
            IF i = 0 THEN
                EXIT;
            END;

        ELSIF i >= 2 THEN
            i := i - 2;

            temp := (VAL(Digit2, ptrA^[0]) * VAL(Digit2, digitIn)) + VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[0] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, ptrA^[1]) * VAL(Digit2, digitIn)) + VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[1] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            ptrA := ADDADR(ptrA, 2*SIZE(Digit));
            ptrR := ADDADR(ptrR, 2*SIZE(Digit));
            IF i = 0 THEN
                EXIT;
            END;

        ELSE
            temp := (VAL(Digit2, ptrA^[0]) * VAL(Digit2, digitIn)) + VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[0] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            ptrR := ADDADR(ptrR, SIZE(Digit));
            EXIT;
        END;
    END;

    IF carry <> 0 THEN
        ptrR^[0] := carry;
    ELSE
        DEC(res^.used);
    END;
END MultiplyByDigit;

PROCEDURE CompareDigits(L, R : DigitsPointer; N : CARDINAL) : INTEGER;
BEGIN
    REPEAT
        DEC(N);

        IF L^[N] < R^[N] THEN
            RETURN -1;
        ELSIF L^[N] > R^[N] THEN
            RETURN 1;
        END;
    UNTIL N = 0;
    RETURN 0;
END CompareDigits;

PROCEDURE SubDigits(R, A, B : DigitsPointer; N : CARDINAL);
VAR
    borrow      : Digit;
    i           : CARDINAL;
    temp        : Digit2;
    aVal,
    bVal        : Digit;
BEGIN
    borrow := 0;
    i := 0;
    REPEAT
        INC(i);
        aVal := A^[0];
        A := ADDADR(A, SIZE(Digit));
        bVal := B^[0];
        B := ADDADR(B, SIZE(Digit));
        <*/PUSH/NOCHECK:OA*>
        temp := VAL(Digit2, aVal) - VAL(Digit2, bVal) - VAL(Digit2, borrow);
        R^[0] := temp;
        <*/POP*>
        borrow := VAL(Digit, temp SHR DigitBits) BAND 1;
        R := ADDADR(R, SIZE(Digit));
    UNTIL i = N;
END SubDigits;

PROCEDURE AddDigits(R, A, B : DigitsPointer; N : CARDINAL) : Digit;
VAR
    aVal, bVal  : Digit;
    carry       : Digit;
    i           : CARDINAL;
    temp        : Digit2;
BEGIN
    i := 0;
    carry := 0;
    REPEAT
        INC(i);
        aVal := A^[0];
        A := ADDADR(A, SIZE(Digit));
        bVal := B^[0];
        B := ADDADR(B, SIZE(Digit));
        temp := VAL(Digit2, aVal) + VAL(Digit2, bVal) + VAL(Digit2, carry);
        <*/PUSH/NOCHECK:A*>
        R^[0] := temp;
        <*/POP*>
        carry := temp SHR DigitBits;
        R := ADDADR(R, SIZE(Digit));
    UNTIL i = N;

    RETURN carry;
END AddDigits;

PROCEDURE IncDigits(A : DigitsPointer; B : Digit; N : CARDINAL) : Digit;
VAR
    save, s     : Digit;
    i           : CARDINAL;
    carry       : CARDINAL;
BEGIN
    save := A^[0];
    s := save + B;
    A^[0] := s;
    carry := ORD(s < save);

    i := 1;
    WHILE (carry <> 0) AND (i < N) DO
        <*/PUSH/NOCHECK:O*>
        save := A^[i] + 1;
        <*/POP*>
        A^[i] := save;
        carry := ORD(save = 0);
        INC(i);
    END;
    RETURN carry;
END IncDigits;

PROCEDURE Karatsuba(R, T, A, B : DigitsPointer; N : CARDINAL);
CONST
    BasicThreshold      = 32;
VAR
    i           : CARDINAL;
    N2          : CARDINAL;
    aComp       : INTEGER;
    bComp       : INTEGER;
    carry       : INTEGER;
    c           : Digit;
    Ah, Bh      : DigitsPointer;
    T1, T2, T3  : DigitsPointer;
    R1, R2, R3  : DigitsPointer;
BEGIN
    IF (N <= BasicThreshold) OR ((N REM 2) <> 0) THEN
        ZapDigits(R, N*2);
        i := 0;
        REPEAT
            FUNC MultiplyDigitAdd(A, B^[i], ADR(R^[i]), N, TRUE);
            INC(i);
        UNTIL i = N;
    ELSE
        N2 := N / 2;

        T1 := ADR(T^[N2]);
        T2 := ADR(T^[N]);
        T3 := ADR(T^[N+N2]);
        R1 := ADR(R^[N2]);
        R2 := ADR(R^[N]);
        R3 := ADR(R^[N+N2]);
        Ah := ADR(A^[N2]);
        Bh := ADR(B^[N2]);

        aComp := CompareDigits(A, Ah, N2);
        bComp := CompareDigits(B, Bh, N2);
        CASE (3*aComp) + bComp OF
        -4:
            SubDigits(R, Ah, A, N2);
            SubDigits(R1, B, Bh, N2);
            Karatsuba(T, T2, R, R1, N2);
            SubDigits(T1, T1, R, N2);
            carry := -1;
        |
        -2:
            SubDigits(R, Ah, A, N2);
            SubDigits(R1, B, Bh, N2);
            Karatsuba(T, T2, R, R1, N2);
            carry := 0;
        |
        2:
            SubDigits(R, A, Ah, N2);
            SubDigits(R1, Bh, B, N2);
            Karatsuba(T, T2, R, R1, N2);
            carry := 0;
        |
        4:
            SubDigits(R, Ah, A, N2);
            SubDigits(R1, B, Bh, N2);
            Karatsuba(T, T2, R, R1, N2);
            SubDigits(T1, T1, R1, N2);
            carry := -1;
        ELSE
            ZapDigits(T, N);
            carry := 0;
        END;

        Karatsuba(R, T2, A, B, N2);
        Karatsuba(R2, T2, Ah, Bh, N2);

        (* T[0..1] = (Ah-Al)*(Bl-Bh),
           R[0..1] = Al*Bl,
           R[2..3] = Ah*Bh
        *)

        c := AddDigits(T, T, R, N);
        c := c + AddDigits(T, T, R2, N);
        c := c + AddDigits(R1, R1, T, N);

        c := IncDigits(R3, carry + INT(c), N2);
    END;
END Karatsuba;

PROCEDURE KaratsubaSquare(R, T, A : DigitsPointer; N : CARDINAL);
CONST
    BasicThreshold      = 32;
VAR
    N2          : CARDINAL;
    c           : Digit;
    Ah          : DigitsPointer;
    T2          : DigitsPointer;
    R1, R2, R3  : DigitsPointer;
BEGIN
    IF (N <= BasicThreshold) OR ((N REM 2) <> 0) THEN
        ZapDigits(R, N*2);
        SquareDigits(R, A, N);
    ELSE
        N2 := N / 2;

        T2 := ADR(T^[N]);
        R1 := ADR(R^[N2]);
        R2 := ADR(R^[N]);
        R3 := ADR(R^[N+N2]);
        Ah := ADR(A^[N2]);

        KaratsubaSquare(R, T2, A, N2);
        KaratsubaSquare(R2, T2, Ah, N2);
        Karatsuba(T, T2, A, Ah, N2);

        c := AddDigits(R1, R1, T, N);
        c := c + AddDigits(R1, R1, T, N);
        c := IncDigits(R3, c, N2);
    END;
END KaratsubaSquare;

CONST
    KaratsubaThreshold  = 48;

PROCEDURE KaraMultiply(vliA, vliB : VLI; VAR INOUT result : VLI);
VAR
    N2          : CARDINAL;
    aN, bN      : CARDINAL;
    i, t        : CARDINAL;
    carry       : Digit;
    res         : VLI;
    tA          : DigitsPointer;
    A, B        : DigitsPointer;
    T, T2, R    : DigitsPointer;
BEGIN
    aN := vliA^.used;
    IF ODD(aN) THEN
        vliA^.digits[aN] := 0;(*place a leading zero*)
        INC(aN);
    END;

    bN := vliB^.used;
    IF ODD(bN) THEN
        vliB^.digits[bN] := 0;(*place a leading zero*)
        INC(bN);
    END;

    Grow(result, aN+bN-1);
    result^.used := aN+bN;

    N2 := aN * 2;

    ALLOCATE(T, N2*SIZE(Digit));

    R := ADR(result^.digits);
    A := ADR(vliA^.digits);
    B := ADR(vliB^.digits);

    IF A = B THEN
        KaratsubaSquare(R, T, A, bN);
    ELSE
        Karatsuba(R, T, A, B, bN);
    END;

    IF aN <> bN THEN
        (* put zeros into the high digits of the result
           which have not yet been assigned.
        *)
        ZapDigits(ADR(R^[bN*2]), result^.used - (bN*2));

        R := ADDADR(R, bN*SIZE(Digit));
        A := ADDADR(A, bN*SIZE(Digit));
        T2 := ADDADR(T, bN*2*SIZE(Digit));
        aN := aN - bN;

        (* we always keep A the same size or bigger than B *)
        IF aN < bN THEN
            tA := A;
            A := B;
            B := tA;

            t := aN;
            aN := bN;
            bN := t;
        END;

        WHILE bN >= KaratsubaThreshold DO
            Karatsuba(T, T2, A, B, bN);

            (* not possible to carry out of the last digit position.
               the high digit of this product is always adding into
               a zero in the result.
            *)
            carry := AddDigits(R, R, T, bN*2);
            IF carry <> 0 THEN
                A := NIL;
                A^[0] := 23;
            END;

            A := ADDADR(A, bN*SIZE(Digit));
            R := ADDADR(R, bN*SIZE(Digit));
            aN := aN - bN;

            IF aN < bN THEN
                tA := A;
                A := B;
                B := tA;

                t := aN;
                aN := bN;
                bN := t;
            END;
        END;

        (* handle anything left with a basic multiply *)

        IF aN <> 0 THEN
            ZapDigits(T, aN+bN);
            i := 0;
            REPEAT
                FUNC MultiplyDigitAdd(A, B^[i], ADR(T^[i]), aN, TRUE);
                INC(i);
            UNTIL i = bN;
            (* not possible to carry out of the last digit position
               the high digit of this product is always adding into
               a zero in the result.
            *)
            carry := AddDigits(R, R, T, aN+bN);
            IF carry <> 0 THEN
                A := NIL;
                A^[0] := 23;
            END;
        END;
    END;

    DEALLOCATE(T, N2*SIZE(Digit));

    (* trim leading zeros. leave at least one digit. *)
    res := result;
    WHILE (res^.used > 1) AND (res^.digits[res^.used-1] = 0) DO
        DEC(res^.used);
    END;
END KaraMultiply;

PROCEDURE Multiply(vliA, vliB : VLI; VAR INOUT result : VLI);
CONST
    KaratsubaSquareThreshold  = 256;
VAR
    neg         : BOOLEAN;
    t           : VLI;
BEGIN
    SetZeroI(result);

    IF (vliA^.used > 1) AND (vliB^.used > 1) THEN
        IF vliA = vliB THEN
            IF vliA^.used < KaratsubaSquareThreshold THEN
                Square(vliA, result);
            ELSE
                KaraMultiply(vliA, vliA, result);
            END;
        ELSE
            IF vliB^.used > vliA^.used THEN
                t := vliB;
                vliB := vliA;
                vliA := t;
            END;

            IF vliB^.used < KaratsubaThreshold THEN
                Multiply2(vliA, vliB, result);
            ELSE
                KaraMultiply(vliA, vliB, result);
            END;
        END;
    ELSIF vliB^.used = 1 THEN
        IF vliB^.digits[0] > 1 THEN
            MultiplyByDigit(vliA, vliB^.digits[0], result);
        ELSIF vliB^.digits[0] = 1 THEN
            Assign(vliA, result);
        END;
    ELSE
        IF vliA^.digits[0] > 1 THEN
            MultiplyByDigit(vliB, vliA^.digits[0], result);
        ELSIF vliA^.digits[0] = 1 THEN
            Assign(vliB, result);
        END;
    END;

    neg := FALSE;
    IF vliA^.neg THEN
        neg := NOT vliB^.neg;
    ELSIF vliB^.neg THEN
        neg := TRUE;
    END;
    SetNegI(result, neg);
END Multiply;

(* returns estimate for vliA/vliB. A > B in entry *)
PROCEDURE GetEstimate(vliA, vliB : VLI; VAR OUT lower, upper : Digit);
VAR
    digitA      : Digit;
    digitB      : Digit;
    temp        : Digit2;
BEGIN
    ASSERT(CompareAbs(vliA, vliB) > Equal);

    digitA := vliA^.digits[vliA^.used-1];
    digitB := vliB^.digits[vliB^.used-1];

    IF digitA >= digitB THEN
        upper := digitA / digitB;
        lower := digitA / (digitB+1);
    ELSE
        temp := VAL(Digit2, digitA) SHL DigitBits;
        temp := temp BOR VAL(Digit2, vliA^.digits[vliA^.used-2]);

        upper := temp / VAL(Digit2, digitB);
        lower := temp / VAL(Digit2, digitB+1);
    END;
END GetEstimate;

%IF IA32 %AND UseAsm %THEN

PROCEDURE DoSubtract3(vliA, vliB : VLI); ASSEMBLER;
ASM
    MOV         ESI, vliB
    MOV         ECX, [ESI].VLIrec.used       (* ECX = count *)
    LEA         ESI, [ESI].VLIrec.digits     (* ESI = ADR(vliB^.digits) *)

    MOV         EDI, vliA
    LEA         EDI, [EDI].VLIrec.digits     (* EDI = ADR(vliA^.digits) *)

    MOV         EDX, 0      (* borrow *)

    CMP         ECX, 8
    JB          @Try4
@LoopTop:
    SUB         ECX, 8
    ADD         EDX, 0FFFFFFFFh   (* Get carry flag out of EDX *)

    MOV         EAX, [EDI]
    SBB         EAX, [ESI]
    MOV         [EDI], EAX

    MOV         EDX, 0            (* clear EDX so we can put carry in later *)

    MOV         EAX, [EDI+4]
    SBB         EAX, [ESI+4]
    MOV         [EDI+4], EAX

    MOV         EAX, [EDI+8]
    SBB         EAX, [ESI+8]
    MOV         [EDI+8], EAX

    MOV         EAX, [EDI+12]
    SBB         EAX, [ESI+12]
    MOV         [EDI+12], EAX

    MOV         EAX, [EDI+16]
    SBB         EAX, [ESI+16]
    MOV         [EDI+16], EAX

    MOV         EAX, [EDI+20]
    SBB         EAX, [ESI+20]
    MOV         [EDI+20], EAX

    MOV         EAX, [EDI+24]
    SBB         EAX, [ESI+24]
    MOV         [EDI+24], EAX

    MOV         EAX, [EDI+28]
    SBB         EAX, [ESI+28]
    MOV         [EDI+28], EAX

    ADC         EDX, 0

    ADD         EDI, 8*4
    ADD         ESI, 8*4

    CMP         ECX, 8
    JGE         @LoopTop
    OR          ECX, ECX
    JZ          @CarryLoop

@Try4:
    CMP         ECX, 4
    JL          @Try2

    SUB         ECX, 4
    ADD         EDX, 0FFFFFFFFh   (* Get carry flag out of EDX *)

    MOV         EAX, [EDI]
    SBB         EAX, [ESI]
    MOV         [EDI], EAX

    MOV         EDX, 0            (* clear EDX so we can put carry in later *)

    MOV         EAX, [EDI+4]
    SBB         EAX, [ESI+4]
    MOV         [EDI+4], EAX

    MOV         EAX, [EDI+8]
    SBB         EAX, [ESI+8]
    MOV         [EDI+8], EAX

    MOV         EAX, [EDI+12]
    SBB         EAX, [ESI+12]
    MOV         [EDI+12], EAX

    ADC         EDX, 0

    ADD         EDI, 4*4
    ADD         ESI, 4*4

    OR          ECX, ECX
    JZ          @CarryLoop

@Try2:
    CMP         ECX, 2
    JL          @Do1
    SUB         ECX, 2

    ADD         EDX, 0FFFFFFFFh   (* Get carry flag out of EDX *)

    MOV         EAX, [EDI]
    SBB         EAX, [ESI]
    MOV         [EDI], EAX

    MOV         EDX, 0            (* clear EDX so we can put carry in later *)

    MOV         EAX, [EDI+4]
    SBB         EAX, [ESI+4]
    MOV         [EDI+4], EAX

    ADC         EDX, 0

    ADD         EDI, 2*4
    ADD         ESI, 2*4

    OR          ECX, ECX
    JZ          @CarryLoop

@Do1:
    ADD         EDX, 0FFFFFFFFh   (* Get carry flag out of EDX *)

    MOV         EAX, [EDI]
    SBB         EAX, [ESI]
    MOV         [EDI], EAX

    MOV         EDX, 0            (* clear EDX so we can put carry in later *)

    ADC         EDX, 0

    ADD         EDI, 4

@CarryLoop:
    OR          EDX, EDX
    JZ          @EndCarryLoop
    SHR         EDX, 1

@CarryLoop1:
    SBB         DWORD PTR [EDI], 0
    ADD         EDI, 4
    OR          EDX, EDX
    JNZ         @CarryLoop1
@EndCarryLoop:

    MOV         EDI, vliA
    MOV         EAX, [EDI].VLIrec.used
@UsedLoop:
    CMP         EAX, 1
    JLE         @Done
    CMP         DWORD PTR [EDI+EAX*4-4].VLIrec.digits, 0
    JNE         @Done
    DEC         EAX
    JMP         @UsedLoop
@Done:
    MOV         [EDI].VLIrec.used, EAX
END DoSubtract3;

%ELSE

PROCEDURE DoSubtract3(vliA, vliB : VLI);(* A := A - B *)
VAR
    count       : CARDINAL;
    borrow,
    aVal        : Digit;
    ptrA, ptrB  : DigitsPointer;
    temp        : Digit2;
BEGIN
    ASSERT(CompareAbs(vliA, vliB) > Equal);

    ptrA := ADR(vliA^.digits);
    ptrB := ADR(vliB^.digits);
    count := vliB^.used;
    borrow := 0;
    LOOP
        IF count >= 8 THEN
            count := count - 8;

            <*/PUSH/NOCHECK:O*>
            temp := VAL(Digit2, ptrA^[0]) - VAL(Digit2, ptrB^[0]) - VAL(Digit2, borrow);
            ptrA^[0] := temp;
            borrow := VAL(Digit, temp SHR DigitBits) BAND 1;

            temp := VAL(Digit2, ptrA^[1]) - VAL(Digit2, ptrB^[1]) - VAL(Digit2, borrow);
            ptrA^[1] := temp;
            borrow := VAL(Digit, temp SHR DigitBits) BAND 1;

            temp := VAL(Digit2, ptrA^[2]) - VAL(Digit2, ptrB^[2]) - VAL(Digit2, borrow);
            ptrA^[2] := temp;
            borrow := VAL(Digit, temp SHR DigitBits) BAND 1;

            temp := VAL(Digit2, ptrA^[3]) - VAL(Digit2, ptrB^[3]) - VAL(Digit2, borrow);
            ptrA^[3] := temp;
            borrow := VAL(Digit, temp SHR DigitBits) BAND 1;

            temp := VAL(Digit2, ptrA^[4]) - VAL(Digit2, ptrB^[4]) - VAL(Digit2, borrow);
            ptrA^[4] := temp;
            borrow := VAL(Digit, temp SHR DigitBits) BAND 1;

            temp := VAL(Digit2, ptrA^[5]) - VAL(Digit2, ptrB^[5]) - VAL(Digit2, borrow);
            ptrA^[5] := temp;
            borrow := VAL(Digit, temp SHR DigitBits) BAND 1;

            temp := VAL(Digit2, ptrA^[6]) - VAL(Digit2, ptrB^[6]) - VAL(Digit2, borrow);
            ptrA^[6] := temp;
            borrow := VAL(Digit, temp SHR DigitBits) BAND 1;

            temp := VAL(Digit2, ptrA^[7]) - VAL(Digit2, ptrB^[7]) - VAL(Digit2, borrow);
            ptrA^[7] := temp;
            borrow := VAL(Digit, temp SHR DigitBits) BAND 1;
            <*/POP*>

            ptrA := ADDADR(ptrA, 8*SIZE(Digit));
            ptrB := ADDADR(ptrB, 8*SIZE(Digit));
            IF count = 0 THEN
                EXIT;
            END;

        ELSIF count >= 4 THEN
            count := count - 4;

            <*/PUSH/NOCHECK:O*>
            temp := VAL(Digit2, ptrA^[0]) - VAL(Digit2, ptrB^[0]) - VAL(Digit2, borrow);
            ptrA^[0] := temp;
            borrow := VAL(Digit, temp SHR DigitBits) BAND 1;

            temp := VAL(Digit2, ptrA^[1]) - VAL(Digit2, ptrB^[1]) - VAL(Digit2, borrow);
            ptrA^[1] := temp;
            borrow := VAL(Digit, temp SHR DigitBits) BAND 1;

            temp := VAL(Digit2, ptrA^[2]) - VAL(Digit2, ptrB^[2]) - VAL(Digit2, borrow);
            ptrA^[2] := temp;
            borrow := VAL(Digit, temp SHR DigitBits) BAND 1;

            temp := VAL(Digit2, ptrA^[3]) - VAL(Digit2, ptrB^[3]) - VAL(Digit2, borrow);
            ptrA^[3] := temp;
            borrow := VAL(Digit, temp SHR DigitBits) BAND 1;
            <*/POP*>

            ptrA := ADDADR(ptrA, 4*SIZE(Digit));
            ptrB := ADDADR(ptrB, 4*SIZE(Digit));
            IF count = 0 THEN
                EXIT;
            END;

        ELSIF count >= 2 THEN
            count := count - 2;

            <*/PUSH/NOCHECK:O*>
            temp := VAL(Digit2, ptrA^[0]) - VAL(Digit2, ptrB^[0]) - VAL(Digit2, borrow);
            ptrA^[0] := temp;
            borrow := VAL(Digit, temp SHR DigitBits) BAND 1;

            temp := VAL(Digit2, ptrA^[1]) - VAL(Digit2, ptrB^[1]) - VAL(Digit2, borrow);
            ptrA^[1] := temp;
            borrow := VAL(Digit, temp SHR DigitBits) BAND 1;
            <*/POP*>

            ptrA := ADDADR(ptrA, 2*SIZE(Digit));
            ptrB := ADDADR(ptrB, 2*SIZE(Digit));
            IF count = 0 THEN
                EXIT;
            END;

        ELSE
            <*/PUSH/NOCHECK:O*>
            temp := VAL(Digit2, ptrA^[0]) - VAL(Digit2, ptrB^[0]) - VAL(Digit2, borrow);
            ptrA^[0] := temp;
            borrow := VAL(Digit, temp SHR DigitBits) BAND 1;
            <*/POP*>

            ptrA := ADDADR(ptrA, SIZE(Digit));
            EXIT;
        END;
    END;
    WHILE borrow <> 0 DO
        aVal := ptrA^[0];
        <*/PUSH/NOCHECK:O*>
        ptrA^[0] := aVal - 1;
        <*/POP*>
        ptrA := ADDADR(ptrA, SIZE(Digit));

        borrow := ORD(aVal = 0);
    END;

    (* trim leading zeros. leave at least one digit. *)
    WHILE (vliA^.used > 1) AND (vliA^.digits[vliA^.used-1] = 0) DO
        DEC(vliA^.used);
    END;
END DoSubtract3;

%END

PROCEDURE ShiftLeftDigitAndSet(VAR INOUT vli : VLI; dig : Digit);
VAR
    count       : CARDINAL;
    ptr         : POINTER TO ARRAY [0..4] OF Digit;
BEGIN
    IF NOT IsZeroI(vli) THEN
        INC(vli^.used);
        Grow(vli, vli^.used-1);

        count := vli^.used;
        (*ptr := ADR(vli^.digits[count-5]);*)
        ptr := ADR(vli^.digits[count-1]);
        ptr := SUBADR(ptr, 4*SIZE(Digit));
        LOOP
            IF count >= 4 THEN
                count := count - 4;
                ptr^[4] := ptr^[3];
                ptr^[3] := ptr^[2];
                ptr^[2] := ptr^[1];
                ptr^[1] := ptr^[0];
                ptr := SUBADR(ptr, 4 * SIZE(Digit));
                IF count = 0 THEN
                    EXIT;
                END;

            ELSIF count >= 2 THEN
                count := count - 2;
                ptr^[4] := ptr^[3];
                ptr^[3] := ptr^[2];
                ptr := SUBADR(ptr, 2 * SIZE(Digit));
                IF count = 0 THEN
                    EXIT;
                END;

            ELSE
                ptr^[4] := ptr^[3];
                EXIT;
            END;
        END;
    END;

    vli^.digits[0] := dig;
END ShiftLeftDigitAndSet;

(* divides A / B *)
(* NB: on entry vliA <> 0, vliB <> 0, vliA > vliB *)
PROCEDURE DoDivide(vliA, vliB : VLI;
                   VAR INOUT result : VLI;
                   VAR INOUT remainder : VLI);

VAR
    indexA      : CARDINAL;
    times       : CARDINAL;
    vliT1       : VLI;
    upper,
    lower       : Digit;
    aUsed,
    bUsed       : CARDINAL;
    top         : CARDINAL;
    firstTime   : BOOLEAN;
BEGIN
    ASSERT(IsZero(result));
    ASSERT(IsZero(remainder));
    ASSERT(NOT IsZero(vliA));
    ASSERT(NOT IsZero(vliB));
    ASSERT(CompareAbs(vliA, vliB) > Equal);

    vliT1 := CreateI();

    aUsed := vliA^.used;
    bUsed := vliB^.used;
    IF vliA^.digits[aUsed-1] < vliB^.digits[bUsed-1] THEN
        top := bUsed;
    ELSE
        top := bUsed-1;
    END;
    Grow(remainder, aUsed); (* Yes, aUsed*)
    remainder^.used := top+1;
    (*remainder^.digits[0..top] := vliA^.digits[aUsed-top-1..aUsed-1];*)
    MoveDigits(ADR(remainder^.digits),
               ADR(vliA^.digits[aUsed-top-1]),
               top+1);

    firstTime := TRUE;
    indexA := vliA^.used-top;
    REPEAT
        DEC(indexA);

        IF NOT firstTime THEN
            ShiftLeftDigitAndSet(remainder, vliA^.digits[indexA]);
        END;
        firstTime := FALSE;

        IF CompareAbs(remainder, vliB) >= Equal THEN
            GetEstimate(remainder, vliB, lower, upper);
            LOOP
                IF upper - lower <= 1 THEN
                    times := upper;
                ELSE
                    times := lower + ((upper-lower) / 2);
                END;
                SetZeroI(vliT1);
                MultiplyByDigit(vliB, times, vliT1);

                CASE CompareAbs(vliT1, remainder) OF
                Less:
                    IF times < MAX(Digit) THEN
                        lower := times + 1;
                    ELSE
                        upper := lower-1; (* get out of this loop *)
                    END;
                    IF lower > upper THEN
                        DoSubtract3(remainder, vliT1);
                        EXIT;
                    END;
                |
                Greater:
                    upper := times-1;
                    DEC(times);
                    IF lower > upper THEN
                        DoSubtract3(vliT1, vliB);
                        DoSubtract3(remainder, vliT1);
                        EXIT;
                    END;
                |
                Equal:
                    SetZero(remainder);
                    EXIT;
                END;
            END;
            ShiftLeftDigitAndSet(result, times);
        END;
    UNTIL indexA = 0;

    Dispose(vliT1);
END DoDivide;

PROCEDURE DivideByDigit(vliA : VLI;
                        digit : Digit;
                        VAR INOUT result : VLI;
                        VAR INOUT remainder : VLI);
VAR
    temp        : Digit2;
    numTimes    : Digit;
    rem         : Digit;
    index       : INTEGER;
    ptrA,
    ptrR        : POINTER TO Digit;
BEGIN
    ASSERT(IsZero(result));
    ASSERT(IsZero(remainder));
    ASSERT(digit <> 0);
    ASSERT(result <> vliA);
    ASSERT(remainder <> vliA);

    index := vliA^.used-1;
    remainder^.used := 1;
    IF index = 0 THEN
        result^.digits[0] := vliA^.digits[0] / digit;
        result^.used := 1;
        remainder^.digits[0] := vliA^.digits[0] REM digit;
        RETURN;
    END;

    Grow(result, vliA^.used-1);

    IF digit > vliA^.digits[index] THEN
        result^.used := index;
    ELSE
        result^.used := index+1;
    END;

    temp := 0;
    rem := 0;
    ptrA := ADR(vliA^.digits[index]);
    ptrR := ADR(result^.digits[index]);
    FOR index := index TO 0 BY -1 DO
        temp := (VAL(Digit2, rem) SHL DigitBits) BOR VAL(Digit2, ptrA^);
        numTimes := temp / VAL(Digit2, digit);
        ptrR^ := numTimes;
        rem := temp - (VAL(Digit2, numTimes) * VAL(Digit2, digit));
        ptrA := SUBADR(ptrA, SIZE(Digit));
        ptrR := SUBADR(ptrR, SIZE(Digit));
    END;
    remainder^.digits[0] := rem;
END DivideByDigit;

PROCEDURE Divide(vliA, vliB : VLI;
                 VAR INOUT result : VLI;
                 VAR INOUT remainder : VLI);
VAR
    neg         : BOOLEAN;
BEGIN
    SetZeroI(result);
    SetZeroI(remainder);

    IF NOT IsZeroI(vliA) THEN
        CASE CompareAbs(vliA, vliB) OF
        Greater:
            IF vliB^.used > 1 THEN
                DoDivide(vliA, vliB, result, remainder);
            ELSE
                DivideByDigit(vliA, vliB^.digits[0], result, remainder);
            END;
            neg := FALSE;
            IF vliA^.neg THEN
                SetNegI(remainder, TRUE);

                neg := NOT vliB^.neg;
            ELSIF vliB^.neg THEN
                neg := TRUE;
            END;
            SetNegI(result, neg);
        |
        Less:
            Assign(vliA, remainder);
        |
        Equal:
            SetDigit(result, 0, 1);
        END;
    END;
END Divide;

PROCEDURE GetDigits(vli : VLI;
                    VAR OUT digits : ARRAY OF Digit;
                    VAR INOUT amount : CARDINAL;
                    VAR OUT negative : BOOLEAN) : BOOLEAN;
VAR
    i   : CARDINAL;
BEGIN
    IF vli^.used <= amount THEN
        amount := vli^.used;
        negative := vli^.neg;
        FOR i := 0 TO vli^.used-1 DO
            digits[i] := vli^.digits[i];
        END;
        RETURN TRUE;
    END;
    RETURN FALSE;
END GetDigits;

PROCEDURE GetValue (vli : VLI ; VAR value : INTEGER64) : BOOLEAN ;
	VAR	wert :	CARDINAL64 ;
BEGIN
	wert := VAL (CARDINAL64, vli^.digits[0]) ;
	IF vli^.used > 1 THEN
		wert := wert + (VAL(CARDINAL64, vli^.digits[1]) SHL 32) ;
	END ;
	IF vli^.neg THEN
		value := 0 - VAL(INTEGER64, wert) ;
	ELSE
		value := VAL (INTEGER64, wert) ;
	END ;
	RETURN vli^.used < 3 ;
END GetValue ;

END VLIkern.
