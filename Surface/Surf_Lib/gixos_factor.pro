Function Gixos_factor, alpha = alp, beta = bet, dth = dth, qxy = qxy, qz = qz, $
	lambda = lam, energy = ene, k = kvl, sigma = sig, temp = tmp, gamma = gam, $
	qcrit = qcr, radians = rad, ret_xy = wqxy, ret_z = wqz, _extra = _e

;+
; NAME:
;		GIXOS_FACTOR
; VERSION:
;		7.09
; PURPOSE:
;		Calculating the capillary wave factor for diffuse scattering.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		Result = GIXOS_FACTOR ( keywords )
; INPUTS:
;		None.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	ALPHA													| Note:  Out of
;		Scalar, the Alpha angle(s), in degrees unless 		| these 5 parameters
;		/RADIANS is set.									| the only allowed
;	BETA													| combinations are:
;		Scalar or vector, the Beta angle(s), in degrees		|
;		unless /RADIANS is set.								| ALPHA, BETA, DTH
;	DTH														|
;		Scalar, the Detector_theta angle(s), in degrees		| ALPHA, DTH, QZ
;		unless /RADIANS is set.								|
;	QXY														| QXY, QZ
;		Scalar or vector, Q_xy value(s) in inverse Angstrem.|
;	QZ														| Anything else will
;		Scalar or vector, Q_z value(s) in inverse Angstrem.	| yield an error.
;	LAMBDA													|
;		Scalar, the value of the wavelength in Angstrem.	| One and only
;	ENERGY													| one of these
;		Scalar, the energy value in keV.					| three must be
;	K														| defined.
;		Scalar, the K value in inverse Angstrem.			|
;	SIGMA
;		Scalar, the scattering particles' size, in Angstrem.
;	TEMP
;		Scalar, temperature (centigrade unless /ABSOLUTE is set).
;	GAMMA
;		Scalar, surface tension (in mN/m).
;	QCRIT
;		Scalar, Q_critical in inverse Angstrem.  Defaults to 0.
;	/RADIANS
;		Switch.  If set, all input angles are assumed to be given in radians.
;		Default is degrees.
;	RET_XY
;		Optional output, see below.
;	RET_Z
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to embedded
;		routines.  Not to be used directly.
; OUTPUTS:
;		Returns the value(s) of the capillary wave factor, corresponding to the
;		input parameters.
; OPTIONAL OUTPUT PARAMETERS:
;	RET_XY
;		Returns the Q_xy values used in the calculation.
;	RET_Z
;		Returns the Q_z values used in the calculation.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other than the dimensional restrictions listed above.
; PROCEDURE:
;		Straightforward, from definition.  Calls QVEC and XR_ETA.  Calls ABS_MM,
;		CODIMS, DEFAULT, FPU_FIX, HOW_MANY, ISNUM, ONE_OF and SP_BESELJ,
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 1-MAR-2009 by Mati Meron.
;		Modified 15-JUN-2009 by Mati Meron.  Added keyword QCRIT.
;-

	on_error, 1
	if keyword_set(rad) then mult = 1. else mult = !dtor

	kdef = 1
	case One_of(lam,ene,kvl,val=lval) of
		0	:	k = 2*!pi/lval
		1	:	k = 2*!pi*lval/!srcon.conv
		2	:	k = 1.*lval
		else:	kdef = 0
	endcase

	wha = How_many(fir=alp,sec=bet,thi=dth,fou=qxy,fif=qz,whi=whi)
	case whi of
		7	:	begin
					if kdef then begin
						qvc = Qvec(alp,bet,dth=dth,k=k,rad=rad)
						wqxy = sqrt(total(qvc[0:1,*]^2,1))
						wqz = reform(qvc[2,*])
					endif else message, 'Missing energy/wavelength/k input!'
				end
		21	:	begin
					if kdef then begin
						walp = mult*alp
						wbet = asin(qz/k - sin(walp))
						qvc = Qvec(walp,wbet,dth=mult*dth,k=k,/rad)
						wqxy = sqrt(total(qvc[0:1,*]^2,1))
						wqz = qz
					endif else message, 'Missing energy/wavelength/k input!'
				end
		24	:	begin
					wqxy = qxy
					wqz = qz
					if not Codims(qxy,qz) then $
					message, 'Incompatible dimensions!'
				end
		else:	message, 'unacceptable input!'
	endcase

	qcr = Default(qcr,Toler())
	if not Isnum(qcr,/complex) then wqcr = complex(qcr,0) else wqcr = qcr
	trf = Abs_mm(2*wqz/(wqz + sqrt(wqz^2 - wqcr^2)))
	eta = XR_eta(wqz,temp=tmp,gamma=gam,_extra=_e)
	res = $
	(trf*sig/2)^2*(wqxy*sig/2)^(eta-2)*Sp_beselj(!pi*eta/2,0)*gamma(1-eta/2)^2

	return, FPU_fix(res)
end