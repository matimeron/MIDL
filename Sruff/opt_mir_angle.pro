Function Opt_mir_angle, ene, mirror= mir, dfac= dfc, harmonic= har, ratio= rat,$
	_extra = _e

;+
; NAME:
;		OPT_MIR_ANGLE
; VERSION:
;		8.714
; PURPOSE:
;		Calculates the optimal (for harmonic suppression) mirror angle.
; CATEGORY:
;		X-ray specific.
; CALLING SEQUENCE:
;		Result = OPT_MIR_ANGLE( ENE, MIRROR = MIR [, keywords]) $
; INPUTS:
;	ENE
;		Beam energy in keV, scalar or vector.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	MIRROR
; 		The x-ray mirror element (e.g. 'si' or 'rh').
; 	DFAC
; 		Scalar, density factor.  The value of mirror surface density relative
; 		to bulk density of same material.  Default is 1.
; 	HARMONIC
; 		The monochromator harmonic to be suppressed.  Only possible values are
; 		2 and 3.  Default is 3.
; 	RATIO
; 		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to REFLECT.  Not to be used
;		directly.
; OUTPUTS:
;		Returns the value(s) of the optimal angle(s), in mr, in same format as
;		the ENE input.
; OPTIONAL OUTPUT PARAMETERS:
;	RATIO
;		Returns the ratio(s) of the the reflected intensity to the higher,
;		suppressed harmonic, in the same format as the ENE input.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Compares the reflectivities for the primary and suppressed harmonics.
;		Calls QCRIT and REFLECT from SRUFF_LIB.  Calls CAST, DEFAULT and
;		MAKE_GRID, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-JUL-2019 by Mati Meron.
;-

	on_error, 1
	sqmin = 0.9

	wene = Cast(ene[sort(ene)],4)
	res = (rat = 0*wene)
	qc = Qcrit(ele=mir,dfac=dfc,_extra=_e)
	wha = Default(har,3,/dtyp)
	if wha ne 2 and wha ne 3 then message, $
		'Only acceptable harmonic values are 2 or 3 (default is 3)!'
	for i = 0, n_elements(wene)-1 do begin
		if i eq 0 then begin
			repeat begin
				q = Make_grid([sqmin,1]*qc,1e-3,/step)
				rat[0] = $
				max(Reflect(q=q,ene=wene[0],ele=mir,dfac=dfc,_extra=_e)/$
				Reflect(q=wha*q,ene=wha*wene[0],ele=mir,dfac=dfc,_extra=_e),loc)
				done = loc gt 0
				if not done then begin
					if sqmin gt 0 then sqmin = sqmin - 0.1 $
					else message, 'Something wrong, exiting!'
				endif else res[0] = q[loc]
			endrep until done
		endif else begin
			rat[i] = $
			max(Reflect(q=q,ene=wene[i],ele=mir,dfac=dfc,_extra=_e)/$
			Reflect(q=wha*q,ene=wha*wene[i],ele=mir,dfac=dfc,_extra=_e),loc)
			res[i] = q[loc]
		endelse
	endfor

	res = 1e3*asin((float(!srcon.conv)*res)/(4*!pi*wene))
	if n_elements(ene) eq 1 then begin
		res = res[0]
		rat = rat[0]
	endif

	return, res
end