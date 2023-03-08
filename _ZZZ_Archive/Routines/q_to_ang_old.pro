Function Q_to_ang_old, qx, qy, qz, lam = lam, energy = ene, k = kvl, qeps = eps, $
	xreverse = xrv, radians = rad, status = sta

;+
; NAME:
;		Q_TO_ANG
; VERSION:
;		8.05
; PURPOSE:
;		Calculating scattering angles from Q components.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		Result = Q_to_ang ( QX, QY, QZ {,LAM= LAM, ENERGY= ENE, K= K} $
;				[, RADIANS = RAD])
; INPUTS:
;	QX
;		The Qx component.
;	QY
;		The Qy component.
;	QZ
;		The Qz component.
;
;		Note:	Each of QX, Qy and QZ can be either scalar or vector.
;				However, all the vector entries must be same length.
;
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
;	LAM														|
;		Scalar, the value of the wavelength in Angstrem.	| One and only
;	ENE														| one of these
;		Scalar, the energy value in keV.					| three must be
;	K														| defined.
;		Scalar, the K value in inverse Angstrem.			|
;	QEPS
;		Specifies the minimal value below which the inputs QX, QY, QZ are
;		considered zero.  Default is 32*machine_precision (this is multiplied,
;		internally, by K).
;	/XREVERSE
;		Switch.  If set, the values of X are reversed.  To be used when the
;		x-axis points outboard (default is inboard).
;	/RADIANS
;		Switch.  If set, the output is in radians.  Default is degrees.
;	STATUS
;		Optional output, see below.
; OUTPUTS:
;		Returns the angles ALPHA, BETA and PHI corresponding to the input
;		value(s).  If all the inputs are scalars, the result is a 3-element
;		vector, if one or more of the inputs is a vector, the result is a [3,N]
;		array (where N is the vector length), assuming all the vectors are of
;		same length.
;
;		Note:	If all the inputs are 0 (in practical sense, less in absolute 
;				value then QEPS), there is no unique solution for the angles.
;				In this case the routine returns the solution
;					Alpha = Beta = Dth = 0
;				and sets STATUS to 0, as described below.
; OPTIONAL OUTPUT PARAMETERS:
;	STATUS
;		Integer vector, returns 1 for each point where calculation was performed
;		and 0 for points where evalaution is not possible (all Q values are 0).
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The Q values must be physically possible.
; PROCEDURE:
;		Straightforward, from definition.  Calls CALCTYPE, CAST, CODIMS, 
;		DEFAULT, ONE_OF and TOLER, from MIDL.
; MODIFICATION HISTORY:
;		Created 30-OCT-2008 by Mati Meron.
;		Modified 5-APR-2009 by Mati Meron.  Internal changes, added keywords
;		QEPS and STATUS.
;		Modified 25-OCT-2009 by Mati Meron.  Added keyword XREVERSE.
;		Modified 10-FEB-2010 by Mati Meron.  Internal changes.
;		Modified 20-JUN-2011 by Mati Meron.  Internal changes.
;-

	on_error, 1

	case One_of(lam,ene,kvl,val=lval) of
		0	:	k = 2*!dpi/lval
		1	:	k = 2*!dpi*lval/!srcon.conv
		2	:	k = 1d*lval
		else:	message,'Missing energy/wavelength/k input!'
	endcase
	weps = 32*k*Default(eps,Toler())
	deps = 512*Default(eps,Toler())

	wqx = reform(qx)
	wqy = reform(qy)
	wqz = reform(qz)
	if Codims(wqx,wqy,wqz,dim=dim) then begin
		if n_elements(dim) eq 1 then begin
			typ = Calctype(0.,wqx,wqy,wqz)
			res = dblarr(3,dim>1)
			sta = intarr(dim>1)
			dum = where((abs(wqx) > abs(wqy) > abs(wqz)) gt weps, ndum)
			if ndum gt 0 then begin
				qqx = wqx[dum]/k
				qqy = wqy[dum]/k
				qqz = wqz[dum]/k
				if keyword_set(xrv) then qqx = -qqx
				qqyz = sqrt(qqy^2 + qqz^2)
				qqsq = qqx^2 + qqyz^2
				if max(qqsq - 2*qqyz) gt deps^2 $
				then message, 'Unacceptable input!'
				res[0,dum] = asin((qqsq/(2*qqyz)) < 1) + asin(qqy/qqyz)
				res[1,dum] = asin(qqz - sin(res[0,dum]))
				res[2,dum] = -asin(qqx/cos(res[1,dum]))
				sta[dum] = 1
			endif
			if not keyword_set(rad) then res = 180/!dpi*res
		endif else message, 'Inputs must be scalars or vectors!
	endif else message, 'Dimensional inconsistancy!

	return, Cast(res,typ,typ,/fix)
end