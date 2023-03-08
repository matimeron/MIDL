Function Bessel_roots, n, nrt, eps, double = dob, yfunction = yfn

;+
; NAME:
;		BESSEL_ROOTS
; VERSION:
;		0.9
; PURPOSE:
;		Finding the roots of Bessel J and Y functions.
; CATEGORY:
;		Mathematical function.
; CALLING SEQUENCE:
;		Result = BESSEL_ROOTS( N [, NRT, EPS] [, keywords])
; INPUTS:
;	N
;		Integer parameter, the order of the Bessel function.  Rounded down on
;		input if needed.
; OPTIONAL INPUT PARAMETERS:
;	NRT
;		Integer parameter, the number of roots required.  Default is 1.
;		Rounded down on input if needed.
;	EPS
;		Required calculation precision.  Default set by machine precision.
; KEYWORD PARAMETERS:
;	/DOUBLE
;		Switch, specifies that the calculation needs to be done in double
;		precision.
;	/YFUNCTION
;		Switch, specifies that zeroes of a Y function are needed.  Default
;		calculation is for a J function.
; OUTPUTS:
;		Returns a vector of the calculated roots.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Uses the ROOT function from MIDL for low roots and an assymptotic
;		approximation for higher ones.  Also calls DEFAULT, FPU_FIX and TOLER
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 1-FEB-2000 by Mati Meron.
;-

	on_error, 1
	teps = 2*Toler(double=dob)
	weps = Default(eps,teps,/dtype) > teps
	if keyword_set(dob) then wpi = !dpi else wpi = !pi
	wnrt = Default(nrt,1l,/dtype) > 1

	if keyword_set(yfn) then begin
		q = -.5
		fname = 'besely'
	endif else begin
		q = .5
		fname = 'beselj'
	endelse

	blm = (abs(2*(4.*n^2 - 1)*(28.*n^2 - 31)/(3*weps)))^(1./4.)/(4*wpi)
	nlm = floor(blm + (1-n-q)/2) > n

	ncl = wnrt < nlm
	ran = wpi/2*(n + [.5,2*ncl])
	res = Root(fname, wpi/2*(n + [.5,2*ncl]), weps, par = n, /rel, multi = -1)
	if wnrt gt ncl then begin
		tem = wpi*(ncl + 1 + 0.5*(n + q - 1) + findgen(wnrt - ncl))
		res = [res,tem - (4*n^2 - 1)/(8*tem)]
	endif

	return, FPU_fix(res)
end