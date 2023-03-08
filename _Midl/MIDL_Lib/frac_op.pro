Function Frac_op, a, b, add= add, subtract= sub, multiply= mul, divide= div, $
	l64 = l64

;+
; NAME:
;		FRAC_OP
; VERSION:
;		7.1
; PURPOSE:
;		Performs fraction arithmetics.
; CATEGORY:
;		Mathematical, general.
; CALLING SEQUENCE:
;		Result = FRAC_OP( A, B [, keywords])
; INPUTS:
;	A
;		Numeric, any of the integer types.  The following are possible:
;			1	-	Scalar.  Accepted as the fraction A/1.
;			2	-	Vector of length 2.  Accepted as the fraction A[0]/A[1].
;			3	-	A [2,N] array.  Accepted as a set of N fractions, 
;					A[0,i]/A[1,i]
;	B
;		Same as A.
;
;		Note:	Both A and B must be provided.
;		Note:	If both A and B are of type (3), N must be the same for both.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	/ADD									|
; 		Switch, specifies addition.			|	One and only one of these 4
; 	/SUBTRACT								|	must be set.
; 		Switch, specifies subtraction.		|
; 	/MULTIPLY								|
; 		Switch, specifies multiplication.	|
; 	/DIVIDE									|
; 		Switch, specifies division.			|
; 	/L64
; 		Switch, specifies that the type of the output is 64 bit long.  Default
; 		is regular (32 bit) long.
; OUTPUTS:
;		Returns the operation result as a [2,N] array where Result[0,*] 
;		contains the numerators and result [1,*] contains the denominators.  
;		Note that if one of the inputs represents a single fraction, it is 
;		extended to the length of the other input.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other that both inputs must be of one of the integer types.
; PROCEDURE:
;		Straightforward.  Calls CALCTYPE, CAST, ISNUM, GCD and ONE_OF, from 
;		MIDL.
; MODIFICATION HISTORY:
;		Created 15-SEP-2009 by Mati Meron.
;-


	on_error, 1

	if Isnum(a,/int) and Isnum(b,/int) then begin
		if keyword_set(l64) then typ = 14 else typ = 3
		typ = typ > Calctype(a,b)

		if n_elements(a) eq 1 then wa = [a,1] else wa = a
		wa = Cast(reform(wa),typ,typ)
		siza = size(wa)
		if siza[0] eq 1 then begin
			if siza[1] eq 2 then begin
				wa = reform(wa,2,1)
				siza = size(wa)
			endif else message,'Unacceptable A format!'
		endif
		if siza[0] eq 2 then alen= siza[2] else message,'Unacceptable A format!'

		if n_elements(b) eq 1 then wb = [b,1] else wb = b
		wb = Cast(reform(wb),typ,typ)
		sizb = size(wb)
		if sizb[0] eq 1 then begin
			if sizb[1] eq 2 then begin
				wb = reform(wb,2,1)
				sizb = size(wb)
			endif else message,'Unacceptable B format!'
		endif
		if sizb[0] eq 2 then blen= sizb[2] else message,'Unacceptable B format!'

		if alen ne blen then begin
			if (alen < blen) eq 1 then begin
				if alen eq 1 then wa = transpose( $
				[[replicate(wa[0],blen)],[replicate(wa[1],blen)]]) else $
				wb= transpose([[replicate(wb[0],alen)],[replicate(wb[1],alen)]])
			endif else message, "Inputs' sizes mismatch!"
		endif
	endif else message, 'Missing or improper inputs!'

	case One_of(add,sub,mul,div,/nozero) of
		-1	:	message, 'Operation not specified!'
		0	:	begin
					res = wa*wb
					res[0,*] = wa[0,*]*wb[1,*] + wa[1,*]*wb[0,*]
				end
		1	:	begin
					res = wa*wb
					res[0,*] = wa[0,*]*wb[1,*] - wa[1,*]*wb[0,*]
				end
		2	:	res = wa*wb
		3	:	res = wa*reverse(wb)
	endcase

	g = GCD(res[0,*],res[1,*])
	res[0,*] = res[0,*]/g
	res[1,*] = res[1,*]/g

	return, res
end