Function Polbound, coef

;+
; NAME:
;		POLBOUND
; VERSION:
;		8.15
; PURPOSE:
; 		Finds the upper absolute bound on the roots of a polynomial equation 
; 		of the form c(0) + c(1)*X + ... + c(n)*X^n 
; CATEGORY:
;		Mathemetical function (general).
; CALLING SEQUENCE:
;		Result = POLBOUND( COEF)
; INPUTS:
;	COEF
;		Numeric vector, length >=2, representing the coefficients of a 
;		polynomial.  Mandatory.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		Returns an (Fujiwara) upper bound on the absolute value of the 
;		polynomial's root equation represented by COEF.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other than the length o COEF must be >=2.
; PROCEDURE:
;		Using the Fujiwara bound formula (see Wiki).  Calls ABS_MM, CAST
;		and FPU_FIX, from MIDL.  
; MODIFICATION HISTORY:
;		Created 15-APR-2012 by Mati Meron.
;-

	on_error, 1

	ord = n_elements(coef) - 1
	if ord gt 0 then begin
		while ord gt 0 and coef[ord] eq 0 do begin
			ord = ord - 1
			coef = coef[0:ord]
		endwhile
		if ord eq 0 then message, 'Insufficient input!'
	endif else message, 'Missing or insufficient input!'

	wcoef = Cast(coef,4)
	tem = Abs_mm(wcoef[0:ord-1]/wcoef[ord])
	tem[0] = tem[0]/2
	res = 2*max(tem^(1./(1 + lindgen(ord))))

	return, FPU_fix(res)
end