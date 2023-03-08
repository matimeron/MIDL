Function Qvec, alp, bet, dth = dth, lam = lam, energy = ene, k = kvl,$
	radians = rad, qtot = qtot, qxy = qxy, qz = qz, ivec = kvc

;+
; NAME:
;		QVEC
; VERSION:
;		8.47
; PURPOSE:
;		Calculating the components of Q (momentum transfer).
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		Result = QVEC ( ALP [, BET] [, DTH= DTH] {,LAM= LAM, ENERGY= ENE, K= K}$
;				[, RADIANS = RAD] {,/QTOT ,/QXY ,/QZ]})
; INPUTS:
;	ALP
;		The Alpha angle(s), in degrees unless /RADIANS is set.  Mandatory, no
;		default is provided.
; OPTIONAL INPUT PARAMETERS:
;	BET
;		The Beta angle(s), in degrees unless /RADIANS is set.  Default is
;		Beta = Alpha.
; KEYWORD PARAMETERS:
;	DTH
;		The Detector_theta angle(s), in degrees unless /RADIANS is set.  Default
;		is zero.
;
;		Note:	Each of ALP, BET and DTH can be either scalar or vector.
;				However, all the vector entries must be same length.
;	LAM														|
;		Scalar, the value of the wavelength in Angstrem.	| One and only
;	ENE														| one of these
;		Scalar, the energy value in keV.					| three must be
;	K														| defined.
;		Scalar, the K value in inverse Angstrem.			|
;	/RADIANS
;		Switch.  If set, all input angles are assumed to be given in radians.
;		Default is degrees.
;	/QTOT										|
;		Switch.  Modifies output, see below.	|
;	/QXY										|   One and only one of these
;		Switch.  Modifies output, see below.    |   three may be set.
;	/QZ                                         |
;		Switch.  Modifies output, see below.    |
;	IVEC
;		Optional output, see below.
; OUTPUTS:
;		Returns the components of Q corresponding to the input value(s).  If all
;		the inputs are scalars, the result is a 3-element vector, if one or more
;		of the inputs is an array, the result is a [3,*,*..] array (where
;		[*,*...] are the dimensions of the original array), with each triple
;		RESULT[*,..] representing the Q components for one set of [ALP,BET,DTH].
;
;		Note:	All non scalar inputs (if any exist) must be of the same
;				dimensionality.
;
;		The output can be modified using the Q-switches (of which no more than
;		one may be set).  Specifically:
;
;			1)	If /QTOT is set, QVEC returns the absolute values of the
;				Q-vector, corresponding to all inputs.
;			2)	If /QXY is set, QVEC returns the absolute values of the inplane
;				vector Q_xy, corresponding to all inputs.
;			1)	If /QZ is set, QVEC returns the absolute values of the normal
;				vector Q_z, corresponding to all inputs.
;		In all these cases the dimensions of the output are those of the
;		(array) input.
; OPTIONAL OUTPUT PARAMETERS:
;	IVEC
;		Returns the initial K-vector corresponding to the input value(s), in 
;		the same format as the function output.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The angles must be all in degrees or all in radians (with /RADIANS set),
;		no mixing.
;		If more than one non scalar input is used, they must share same
;		dimensions.
; PROCEDURE:
;		Straightforward, from definition.  Calls CALCTYPE, CAST, CODIMS,
;		DEFAULT, HOW_MANY and ONE_OF, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-APR-2004 by Mati Meron.
;		Modified 15-FEB-2007 by Mati Meron.  Internal changes.
;		Rewritten 25-FEB-2008 by Mati Meron, to accept arbitrary format inputs.
;		Added output modifying keywords /QTOT, /QXY and /QZ.
;		Modified 20-JUL-2016 by Mati Meron.  Added optional output KVEC.
;-

	on_error, 1

	case One_of(lam,ene,kvl,val=lval) of
		0	:	k = 2*!pi/lval
		1	:	k = 2*!pi*lval/!srcon.conv
		2	:	k = lval
		else:	message,'Missing energy/wavelength/k input!'
	endcase

	bet = Default(bet,alp)
	dth = Default(dth,0.)
	if Codims(alp,bet,dth,dim=dim,same=sam) then begin
		typ = Calctype(0.,alp,bet,dth)
		if keyword_set(rad) then mult = 1d else mult = !dpi/180d
		walp = mult*alp
		wbet = mult*bet
		wdth = mult*dth

		if not sam then begin
			if n_elements(walp) eq 1 then walp = replicate(walp[0],dim)
			if n_elements(wbet) eq 1 then wbet = replicate(wbet[0],dim)
			if n_elements(wdth) eq 1 then wdth = replicate(wdth[0],dim)
		endif

		res = make_array([3,dim>1],typ=typ)
		res[0,*,*,*,*,*,*,*] = -cos(wbet)*sin(wdth)
		res[1,*,*,*,*,*,*,*] = cos(wbet)*cos(wdth) - cos(walp)
		res[2,*,*,*,*,*,*,*] = sin(wbet) + sin(walp)
		if arg_present(kvc) then begin
			kvc = make_array([3,dim>1],typ=typ)
			kvc[1,*,*,*,*,*,*,*] = cos(walp)
			kvc[2,*,*,*,*,*,*,*] = -sin(walp)
			kvc = Cast(k*kvc,typ,typ)
		endif

		dum = How_many(fir=qtot,sec=qxy,thi=qz,/noz,whi=whi)
		case whi of
			0	:	res = reform(res)
			1	:	res = sqrt(total(res^2,1))
			2	:	res = sqrt(total(res[0:1,*,*,*,*,*,*,*]^2,1))
			4	:	res = reform(res[2,*,*,*,*,*,*,*])
			else:	message, 'Only one Q-switch may be set!'
		endcase
		if n_elements(res) eq 1 then res = res[0]
	endif else message, 'Missing or inconsistent inputs!'

	return, Cast(k*res,typ,typ,/fix)
end