Function Qxy, alp, bet, dth = dth, energy = ene, lam = lam, radians = rad

;+
; NAME:
;		QXY
; VERSION:
;		7.0
; PURPOSE:
;		Calculating the value(s) of Q_xy.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		Result = QXY ( ALP [, BET] [, DTH = DTH] {, ENERGY = ENE, LAM = LAM} $
;				[, RADIANS = RAD])
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
;	ENERGY
;		Scalar, the energy value in keV.				| One and only one
;	LAM													| of these two
;		Scalar, the value of the wavelength in Angstrem.| may be set.
;	/RADIANS
;		Switch.  If set, all input angles are assumed to be given in radians.
;		Default is degrees.
; OUTPUTS:
;		Returns the Q_xy value(s) corresponding to the input value(s).  The
;		result can be scalar (if all angle inputs are scalars) or vector.
;		The units of the result are the inverse of the units of LAM.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The angles must be all in degrees or all in radians (with /RADIANS set).
;		No mixing.
; PROCEDURE:
;		Straightforward, from definition.  Calls QVEC.
; MODIFICATION HISTORY:
;		Created 15-AUG-2003 by Mati Meron.
;		Rewritten 15-FEB-2007 by Mati Meron, as a front end to QVEC.
;		Obsoleted 25-APR-2008 by Mati Meron, following an upgrade of QVEC.
;		Maintained temporarily for backward compatibility only.
;-

	on_error, 1

	return, Qvec(alp, bet,dth=dth,energy=ene,lam=lam,radians=rad,/qxy)
end