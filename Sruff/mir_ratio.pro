Function Mir_ratio, ene, ang, mirror= mir, dfac= dfc, harmonic= har, _extra = _e

;+
; NAME:
;		MIR_RATIO
; VERSION:
;		8.714
; PURPOSE:
;		Calculates the ratio between the fundamental and higher harmonic, for
;		specular reflection.
; CATEGORY:
;		X-ray specific.
; CALLING SEQUENCE:
;		Result = MIR_RATIO ( ENE, ANG, MIRROR = MIR [, keywords]) $
; INPUTS:
;	ENE
;		Beam energy in keV, scalar or vector.
;	ANG
;		Mirror angle, in radians or miliradians (the routine will distinguish).
;
;	Note:	Either ENE of ANG may be a vector, but not both of them.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	MIRROR
; 		The x-ray mirror element (e.g. 'si' or 'rh').
; 	DFAC
; 		Scalar, density factor.  The value of mirror surface density relative
; 		to bulk density of same material.  Default is 1.
; 	HARMONIC
; 		The monochromator harmonic to be compared with.  Default is 3.
;	_EXTRA
;		A formal keyword used to pass keywords to REFLECT.  Not to be used
;		directly.
; OUTPUTS:
;		Returns the ratio(s) of the the reflected intensity to the higher,
;		harmonic, in the same format as the larger of the ENE and ANG inputs.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Calls REFLECT from SRUFF_LIB, and DEFAULT from MIDL.
; MODIFICATION HISTORY:
;		Created 25-JUL-2019 by Mati Meron.
;-

	on_error, 1

	nums = [n_elements(ene),n_elements(ang)]
	min = min(nums,loc,max=max)
	case min of
		0	:	message, 'Missing input(s)!'
		1	:	begin
					if max gt 1 then begin
						if loc eq 0 then begin
							wene = replicate(ene,max)
							wang = ang
						endif else begin
							wene = ene
							wang = replicate(ang,max)
						endelse
					endif else begin
						wene = ene
						wang = ang
					endelse
				end
		else:	begin
					if min eq max then begin
						wene = ene
						wang = ang
					endif else message, 'Dimensional mismatch!'
				end
	endcase

	whar = Default(har,3,/dtyp)
	if max(wang) gt 0.1 then wang = 1e-3*wang
	res = Reflect(theta=wang,ene=wene,ele=mir,dfac=dfc,_extra=_e)/$
		Reflect(theta=wang,ene=whar*wene,ele=mir,dfac=dfc,_extra=_e)
	if n_elements(res) eq 1 then res = res[0]

	return, res
end