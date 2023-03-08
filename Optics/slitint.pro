Function Slitint, t, sigma = sig, zeta = zet

;+
; NAME:
;		SLITINT
; VERSION:
;		4.3
; PURPOSE:
;		Calculates an integral needed for the general slit diffraction problem.
; CATEGORY:
;		Optical calculations.
; CALLING SEQUENCE:
;		Result = SLITINT( T, [, keywords])
; INPUTS:
;	T
;		Numerical, real, otherwise arbitrary (but only values <= 1 yield a
;		non-zero result.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	SIGMA
;		Scalar real parameter.  Equals 1/sqrt(pi)*beam_size/slit_size.
;	ZETA
;		Scalar real parameter, the beam "compressibility.
;		Equals distance_from_source*angular_size^2/wavelength
; OUTPUTS:
;		Returns the value(s) of sqrt(pi/2)*sigma*exp(-1/2*(zeta*t/sigma)^2*
;		Re(Erf((1-abs(t)-i*zeta*t)/(sqrt(2)*sigma)))
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Expresses the result in terms of the normalized complementary error
;		function.  Calls CALCTYPE, CAST, DEFAULT, NERFC_MM, REAL_MM and TOLER,
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 15-APR-2003 by Mati Meron.
;-

	on_error, 1

	i = dcomplex(0,1)
	wsig = sqrt(2d)*Default(sig,1/Toler(t))
	wzet = Default(zet,0.)
	rtyp = Calctype(t,0.)
	res = 0*t
	abt = abs(t)
	dum = where(abt lt 1, ndum)

	if ndum gt 0 then begin
		a = (1 - abt[dum])/wsig
		b = wzet*abt[dum]/wsig
		res[dum] = sqrt(!dpi/4)*wsig*$
		Real_mm(exp(-b^2) - exp(-a^2-2*i*a*b)*Nerfc_mm(a+i*b))
	endif

	return, Cast(res,rtyp,rtyp,/fix)
end