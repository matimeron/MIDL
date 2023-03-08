Function Splinft, k, spc, deriv = der

;+
; NAME:
;		SPLINFT
; VERSION:
;		7.09
; PURPOSE:
;		Calculates (finite range) Fourier Transform of a function provided as a 
;		set of spline coefficients.
; CATEGORY:
;		Mathematical Function (general).
; CALLING SEQUENCE:
;		Result = SPLINFT (K, SPC [ DERIV = DER])
; INPUTS:
;	K
;		Numeric, scalar or vector.  The K values for the transform.
;	SPC
;		An [n,3] array of spline coefficients, created by the function
;		SPLIN_COEEFS.
; KEYWORD PARAMETERS:
;	DERIV
;		Integer.  If provided and nonzero, Fourier transform of the function's 
;		derivative of order DERIV is evaluated.  Default value is 0, maximal 
;		value is 2.
; OUTPUTS:
;		Returns the values of the transform, i.e. of the integral of 
;		f(x)*exp(-ikx) where f is the function defined by the spline and the
;		integration is over the range over which the spline is defined.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Exact transform calculation for the cubic-spline approximation to the 
;		function.  Calls CALCTYPE, CAST, CONJ_MM, DEFAULT, MAKE_RANGE and 
;		POWFT01, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-JUL-2009 by Mati Meron.
;-

	on_error, 1
	siz = size(spc)
	if siz[0] ne 2 or siz[2] ne 3 then message, 'Bad coefficient array!'
	ns = siz[1]
	nk = n_elements(k)
	typ = Calctype(k,spc,complex(0))
	ic = dcomplex(0,1)
	nder = Default(der,0,/dtyp) > 0
	if nder gt 2 then message, $
	'Derivative of order ' + string(nder,form='(i0)') +' not allowed!'
 
 	l = Make_range([0,ns-2])
 	h = Make_range([1,ns-1])
 	x = spc[*,0]
 	y = spc[*,1]
 	sc = spc[*,2]
 	dex = x[h] - x[l]
 	ph = exp(-ic*x#k)
 	f = make_array([ns-1,nk,4],typ=typ)
 	for i = 0, 3-nder do f[*,*,i] = Powft01(dex#k,i)
 	uv = replicate(1,nk)
 	dex = dex#uv
 	y = y#uv
 	sc = sc#uv
 
	case nder of
		0:	begin
				fir = f[*,*,0] - f[*,*,1]
				sec = 2*f[*,*,1] - 3*f[*,*,2] + f[*,*,3]
				res = dex*(y[l,*]*ph[l,*]*fir + y[h,*]*ph[h,*]*Conj_mm(fir) - $
				dex^2*(sc[l,*]*ph[l,*]*sec + sc[h,*]*ph[h,*]*Conj_mm(sec)))
			end
		1:	begin
				fir = f[*,*,0]
				sec = -2*f[*,*,0] + 6*f[*,*,1] - 3*f[*,*,2]
				res = -(y[l,*]*ph[l,*]*fir - y[h,*]*ph[h,*]*Conj_mm(fir) - $
				dex^2*(sc[l,*]*ph[l,*]*sec - sc[h,*]*ph[h,*]*Conj_mm(sec)))
			end
		2:	begin
				fir = f[*,*,0] - f[*,*,1]
				res = 6*dex*(sc[l,*]*ph[l,*]*fir + sc[h,*]*ph[h,*]*Conj_mm(fir))
			end
	endcase

	return, Cast(total(res,1),typ,typ,/fix)
end