Function Si_exp_coeff, t

;+
; NAME:
;		SI_EXP_COEFF
; VERSION:
;		8.716
; PURPOSE:
;		Calculates the linear expansion coefficient of Silicon.
; CATEGORY:
;		Experimental utility.
; CALLING SEQUENCE:
;		Result = SI_EXP_COEFF( T)
; INPUTS:
; 	T
; 		Vector or scalar, temperature, in degrees Kelvin.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 		None.
; OUTPUTS:
;		Returns the value(s) of linear expansion coefficient of Silicon for the
;		provided value(s) of temperature.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
; 		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
; 		Not reliable for temperatures significantly above 600 K.
; PROCEDURE:
;		Using a fit formula from:
;		https://trc.nist.gov/cryogenics/materials/Silicon/Silicon.htm
;		Calls ERRORF_MM from MIDL.
; MODIFICATION HISTORY:
;		Created 15-NOV-2019 by Mati Meron.
;-

	on_error, 1

	a = 1.00500e-05
	b = -5.99688e-06
	c = 1.25574e-06
	d = -1.12086e-07
	e = 3.63225e-09
	f = 2.67708e-02
	g = -1.22829e-04
	h = 1.62544e-18
	i = 4.72374e+02
	j = -3.58796e+04
	k = -1.24191e+07
	l = 1.25972e+09

	t = 1.*t
	st = sqrt(t)
	tt = t - 76
	one = 4.8e-5*t^3 + t^5*(a+ b*st+ c*t+ d*t*st+ e*t^2)*(1+ Errorf_mm(t-15))/2
	two = (-47.6+ f*tt^2 + g*tt^3 + h*tt^9)*(1+ Errorf_mm(0.2*(t-52)))/2
	three = (i+ j/t+ k/t^2+ l/t^3)*(1 + Errorf_mm(0.1*(t-200)))/2

	res = 1e-8*(one*Errorf_mm(0.2*(t-52),/comp)/2 + $
				two*Errorf_mm(0.1*(t-200),/comp)/2 + three)

	return, res
end