Function Peak_stsums, x, y, ser, r_sum= rsm, s_sum= ssm, $
	x_ret=xrt, y_ret=siv, dx_scal = rov, es_scal = ses, erfl = efl, _extra = _e

;+
; NAME:
;		PEAK_STSUMS
; VERSION:
;		4.9
; PURPOSE:
;		Converting spectral data into vectors for statistical processing
;		purposes.
; CATEGORY:
;		Data analysis.
; CALLING SEQUENCE:
;		Result = PEAK_STSUMS( X [, Y [, Z]] [, keywords])
; INPUTS:
;	X
;		Numeric, Can be a vector (scalar is considered to be a vector of
;		length 1), a [2,*] array or a [3,*] array.
; OPTIONAL INPUT PARAMETERS:
;	Y
;		Numeric, acceptable only when X is a vector, in which case Y has to be
;		a vector of same length.
;	SER
;		Numeric, acceptable only when X and Y are vectors, in which case SER has
;		to be a vector of same length.
; KEYWORD PARAMETERS:
;	R_SUM
;		Optional output, see below.
;	S_SUM
;		Optional output, see below.
;	X_RET
;		Optional output, see below.
;	Y_RET
;		Optional output, see below.
;	DX_SCAL
;		Optional output, see below.
;	ES_SCAL
;		Optional output, see below.
;	ERFL
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to SPLIT_XY.  Not to be used
;		directly.
; OUTPUTS:
;		Returns the length of the resulting vectors, as LONG.
; OPTIONAL OUTPUT PARAMETERS:
;	R_SUM
;		The name of the variable to receive the sum of the DX values (these are
;		the central differences of the original X values.
;	S_SUM
;		The name of the variable to receive the sum of Y_i*DX_i, where Y_i are
;		the original Y values.
;	X_RET
;		The name of the variable to receive the resulting X vector, which equals
;		the original X vector.
;	Y_RET
;		The name of the variable to receive the resulting Y vector.  The
;		entries of this vector are the original Y values normalized by the
;		value of S_SUM/R_SUM.
;	DX_SCAL
;		The name of the variable to receive the vector DX/R_SUM.
;	ES_SCAL
;		The name of the variable to receive the vector of scaled errors. It is
;		assumed that the SER values in the original data (if present) represent
;		the statistical errors of the Y data.  The scaled errors are obtained
;		from the squares of the original ones through the multiplication by
;		DX/S_SUM.  If no SER data is present, ES_SCAL returns a vector of zeroes
;		(same length as the other vectors).
;	ERFL
;		Returns 1 if ES_SCAL values are meaningful (i.e. if original Z input
;		exists), 0 otherwise.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other than those specified in types and sizes of variables.
; PROCEDURE:
;		Uses SPLIT_XY from MIDL (see there) to split the input data, then
;		proceeds according to statistical definitions. Also calls DIF from MIDL.
; MODIFICATION HISTORY:
;		Created 5-OCT-2003 by Mati Meron.
;		Modified 15-APR-2004 by Mati Meron.  Changed data formats.
;-

	on_error, 1

	nxy = Split_xy(x,y,ser,x=wx,y=wy,z=wser,inpz=efl,/keep)
	if nxy gt 0 then begin
		xrt = wx
		dx = Dif(wx,/cen,/lin)
		rsm = total(dx)
		ssm = total(wy*dx)
		yav = ssm/rsm
		rov = dx/rsm
		siv = wy/yav
		if efl then ses = abs(rov*wser/yav) else ses = make_array(size=size(wy))
	endif

	return, nxy
end