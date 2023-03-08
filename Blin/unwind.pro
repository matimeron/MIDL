Function Unwind, x, y, radius = rad, inverse = inv

;+
; NAME:
;		UNWIND
; VERSION:
;		7.09
; PURPOSE:
;		Maps circles to straight lines and vice versa.
; CATEGORY:
;		Mathematical, geometrical transformation..
; CALLING SEQUENCE:
;		Result = UNWIND( X [, Y], RADIUS = RAD [, /INVERSE)
; INPUTS:
;	X
;		Numeric, A vector (scalar is considered to be a vector of length 1), an
;		[2,*] array or a [3,*] array.
; OPTIONAL INPUT PARAMETERS:
;	Y
;		Numeric, acceptable only when X is a vector, in which case Y has to be
;		a vector of same length.
;
;		Note:	If only one input is provided then:
;				1)	If it is a vector, it is taken to be Y and X is generated
;					internally with a spacing of 1.
;				2)	If it is a [2,*] array, it is split into X and Y vectors.
;				3)	If it is a [3,*] array, it is split into X and Y vectors
;					and the third column is ignored.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	RADIUS
;		The raius used in the line-circle trnsformation.
;	/INVERSE
;		Switch.  If set, the "wind" transformation, i.e. transformation from
;		line to circular arc, is performed.  Default is "unwind".
; OUTPUTS:
;		Returns the transformed point(s) in a [2,N] array ("shape array")
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Simple geometry.  Calls FPU_FIX, JOIN_XY and SPLIT_XY, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-APR-2009 by Mati Meron.
;-

	on_error, 1

	n = Split_xy(x,y,x=wx,y=wy)
	rmy = rad - wy
	if keyword_set(inv) then begin
		rx = rmy*sin(wx/rad)
		ry = rad - rmy*cos(wx/rad)
	endif else begin
		rx = rad*atan(wx,rmy)
		ry = (2*rad*wy - wx^2 - wy^2)/(rad + sqrt(wx^2 + rmy^2))
	endelse

	return, FPU_fix(Join_xy(rx,ry))
end
