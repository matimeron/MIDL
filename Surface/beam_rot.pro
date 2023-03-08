Function Beam_rot, qz= qz, alpha= alp, lambda= lam, energy= ene, default= def, $
	crystal = crs, index = ind, scale = sca, radians = rad, degrees = deg

;+
; NAME:
;		BEAM_ROT
; VERSION:
;		8.05
; PURPOSE:
;		Calculates beam rotation under reflection in steering crystal.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		Result = BEAM_ROT( QZ, [, keywords])
; INPUTS:
; 		
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	QZ
;			Numeric vector or scalar, Q_z value(s)			|	Either QZ or 
;			corresponding to reflection,in A^(-1) units.	|	ALPHA, but 
;	ALPHA													|	never both, 
;			The downward beam angle following the 			|	must be given.
;			crystal reflection.
;	LAMBDA													|	Either LAMBDA or
;		Scalar, the value of the wavelength in Angstrem.	|	ENERGY, but
;	ENERGY													|	never both,
;		Scalar, the energy value in keV.					|	must be given.
;	/DEFAULT
;		Switch.  If set, the beamline default values for CRYSTAL and INDEX (see
;		below) are used.  These values are 'GE' and [1,1,1].
;	CRYSTAL
;		Character scalar translating to a name of recognized crystal.
;		Currently the recognized list includes : Diamond, Silicon, Germanium.
;		Only the first two letters are needed.
;	INDEX
;		The reflection index, given as 3 element vector.  Example: [1,1,1].
;	/SCALE
;		Switch.  if set, the result is scaled by twice the Chi value of the
;		rotation.
;	/RADIANS
;		Switch.  If set, the inputs and outputs are given in radians.  That
;		includes the output itself and ALPHA.
;	/DEGREES
;		Switch.  If set, the inputs and outputs are given in degrees.  See
;		RADIANS above for list.
;
;		Comment:	Either RADIANS or DEGREES (but not both) may be set.  If
;					neither is set, the default is DEGREES.
; OUTPUTS:
;		Returns the value(s) of the beam rotation(s) corresponding to the
;		energy and Q_z (or Alpha) value(s) given.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other that the reflection must be physically realizable.
; PROCEDURE:
;		Straightforward geometrical calculation.  Calls FPU_FIX, HOW_MANY, 
;		ONE_OF and TOLER from MIDL.  Also calls QVAL from MONO_LIB.
; MODIFICATION HISTORY:
;		Created 30-MAR-2011 by Mati Meron.
;		Modified 10-JUN-2011 by Mati Meron.  Added keywords ALPHA and LAMBDA. 
;-

	on_error, 1

	if (One_of(deg,rad) > 0) eq 0 then mult = !radeg else mult = 1.

	case One_of(lam,ene,val=val) of
		0	:	k = 2*!dpi/val
		1	:	k = 2*!dpi*val/!srcon.conv
		else:	message,'Missing energy/wavelength/k input!'
	endcase
	if n_elements(k) eq 1 then k = float(k[0]) $
	else message, 'Energy/wavelength input must be a scalar!'
	
	case One_of(qz,alp,val=val) of
		0	:	a = val/(2*k)
		1	:	a = sin(val/mult)
		else:	message, 'Missing qz/alpha input!'
	endcase

	dum = How_many(fir=def,sec=crs,thi=ind,whi=whi)
	case whi of
		1	:	begin
					crs = 'Ge'
					ind = [1,1,1]
				end
		6	:
		else:	message, 'Specify Either Crystal and Index or Default!'
	endcase

	dum = Qval(crystal=crs,index=ind,dref=dref)
	qq = 2*!pi/dref
	b = qq/(2*k)
	if abs(b) gt 1 then message, 'Energy too low for Bragg reflection!'
	lim = sqrt(4*b^2*(1-b^2))
	if max(abs(a)) gt lim then message, $
	'Unachievable Q_z value(s), maximal value is '+string(2*k*lim,form='(g6.4)')

	rres = atan(a*sqrt(lim^2 - a^2),2*b^2 - a^2)
	if keyword_set(sca) then begin
		zer = where(abs(rres) le Toler(),nzer,comp=czer,ncomp=nczer)
		if nzer gt 0 then rres[zer] = 1 - b^2
		if nczer gt 0 then rres[czer] = rres[czer]/(2*asin(a[czer]/lim))
		mult = 1.
	endif

	return, FPU_fix(mult*rres)
end