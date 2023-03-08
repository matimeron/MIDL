Function Boxrep, z, x, y

;+
; NAME:
;		BOXREP
; VERSION:
;		4.6
; PURPOSE:
;		Resampling of a function.
; CATEGORY:
;		Mathematical
; CALLING SEQUENCE:
;		Result = BOXREP( Z, X, Y)
; INPUTS:
;	Z
;		Numerical vector, at least 2 elements.
;	X
;		Numerical vector, at least 2 elements.
;	Y
;		Numerical vector, at least 2 elements.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		Retains "averaged" values for the function defined by X, Y, at the
;		point set Z.  See PROCEDURE below for details.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other than the minimal length of the vectors.
; PROCEDURE:
;		BOXREP takes X and Y as defining a function (with value Y[i] at X[i])
;		It then defines the averaged value at Z as follows:
;
;		Result[Z[i]] = 	0, for i=0
;		Result[Z[i]] = 	(Integral of Y*dX from Z[i-1] to Z[i])/(Z[i] - Z[i-1]),
;						for i gt 0.
;
;		Calls DIF, SPLIN_COEFFS, SPLINT and SPLIT_XY from MIDL.
; MODIFICATION HISTORY:
;		Created 10-FEB-2004 by Mati Meron.
;-

	on_error,1

	nxy = Split_xy(x,y,x=wx,y=wy,inpx=inx)
	if nxy gt 1 and inx then int = Splint(z,Splin_coeffs(wx,wy)) $
	else message, 'Missing or incomplete input data!'

	return, [0,(Dif(int))[1:*]/(Dif(z))[1:*]]
end