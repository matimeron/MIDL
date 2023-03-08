Function Bincoef, n, m, intout = int

;+
; NAME:
;		BINCOEF
; VERSION:
;		8.425
; PURPOSE:
;		Calculates binomial coefficients.
; CATEGORY:
;		Mathematical.
; CALLING SEQUENCE:
;		Result = BINCOEF( N, M)
; INPUTS:
;	N
;		Scalar. Must be either negative or non-negative and >= M.
;	M
;		Integer type and non-negative, else arbitrary.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/INTOUT
;		Switch.  If set and N is integer in value (not necessarily in type) the
;		output is converted to integer type.  For N > 32,it'll be L64 type.
; OUTPUTS:
;		Returns the binomial coefficient(s) C^N_M.  These are defined for
;		non-negative M <= N or for nonegative M and N < 0, in which case
;		C^N_M = (-1)^M * C^(M+N-1)_M. The result type is DOUBLE if N is double,
;		FLOAT otherwise, unless /INTOUT is set.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		If /INTOUT is set, the result may be inaccurate for N > 49, and 
;		conversion error may occur for N > 66
; PROCEDURE:
;		Straightforward.  Calls CALCTYPE, CAST, FPU_FIX, ISNUM, LNGAMMA_MM and
;		TOLER, from MIDL.
; MODIFICATION HISTORY:
;		Created 30-AUG-2005 by Mati Meron.
;		Modified 20-OCT-2011 by Mati Meron.  Internal changes.
;		Modified 25-AUG-2015 by Mati Meron.  Added keyword INTOUT.
;-

	on_error, 1

	if (size(n))[0] eq 0 then begin
		typ = Calctype(0.,n)
		ifl = (n - long(n)) le Toler(n)
		nd = double(n)
		if Isnum(m,/int) then begin
			if min(m) ge 0 then begin
				if n ge 0 then begin
					res = dblarr(n_elements(m))
					dum = where(n ge m, ndum)
					if ndum gt 0 then res[dum] = exp(Lngamma_mm(nd+1,int=ifl)- $
					Lngamma_mm(nd-m[dum]+1,int=ifl)-Lngamma_mm(m[dum]+1d,/int))
				endif else res = (-1)^m*exp(Lngamma_mm(m-nd,int=ifl) - $
				Lngamma_mm(-nd,int=ifl) - Lngamma_mm(m+1d,/int))
				if (size(m))[0] eq 0 then res = res[0]
			endif else message, 'M must be non-negative!'
		endif else message, 'M must be of integer type!'
	endif else message, 'N must be a scalar!'

	if keyword_set(int) and ifl then res = round(res,l64=(n gt 32)) $
	else res = Cast(res,4,typ)

	return, FPU_fix(res)
end