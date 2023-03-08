Function Wire_absig, ene, diameter = dia, elem = ele, approx = apr, _extra=_e

;+
; NAME:
;		WIRE_ABSIG
; VERSION:
;		8.72
; PURPOSE:
;		Calculaties the Gaussian sigma in absorption using a wire of given
;		diameter.
; CATEGORY:
;		X-ray specific.
; CALLING SEQUENCE:
;		Result = WIRE_ABSIG ( ENE, DIAMETER = DIA, ELEM = ELE [, keywords])
; INPUTS:
; 	ENE
; 		Numeric, vector or scalar.  Beam energy in keV.
; OPTIONAL INPUT PARAMETERS:
; 		None.
; KEYWORD PARAMETERS:
;	DIAMETER
;		Numeric scalar.  Absorbing wire radius, in mm.
;	ELEM
;		Character scalar, the chemical symbol of the wire's element.
;	/APPROX
;		Switch.  If set, an approximate calculation, which is much faster and
;		usually good enough is performed.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to be
;		used directly.
; OUTPUTS:
;		Returns the sigma value of the wire edge induced broadening, in microns.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
; 		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
; 		None other than the restrictions specified for inputs.
; PROCEDURE:
;		Follows the formalism present in the Edge Scan Broadening writeup. Calls
;		ABSIG_KERN from BLOPT_LIB and ABS_COEFF from SRUFF_LIB.  Calls CALCTYPE,
;		CAST and ROMBERG from MIDL.
; MODIFICATION HISTORY:
; 		Created 10-JAN-2018 by Mati Meron.
;		Rewritten and documented 30-DEC-2020 by Mati Meron.
;-

	on_error, 1

	thre = 150
	typ = Calctype(ene,0.)
	mud = 0.1d*dia*Abs_coeff(ene,elem=ele)
	res = 5d/mud^4

	if not keyword_set(apr) then begin
		dum = where(mud le thre, ndum)
		if ndum gt 0 then begin
			tmud = mud[dum]
			tres = res[dum]
			for i = 0, ndum-1 do begin
				i0= Romberg('Absig_kern',[0,1d],mud=tmud[i],/wir,n=0,try=3,/ver)
				i1= Romberg('Absig_kern',[0,1d],mud=tmud[i],/wir,n=1,try=3,/ver)
				i2= Romberg('Absig_kern',[0,1d],mud=tmud[i],/wir,n=2,try=3,/ver)
				tres[i] = (i0*i2 - i1^2)/i0^2
			endfor
			res[dum] = tres
		endif
	endif

	res = 1d3*dia/2d*sqrt(res > 0)
	if n_elements(ene) eq 1 then res = res[0]

	return, Cast(res,typ,typ,/fix)
end