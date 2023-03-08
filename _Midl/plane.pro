Function Plane, r_0, r_1, r_2,  r_3,  r_4,  r_5,  r_6,  r_7, $
	r_8, r_9, r_10, r_11, r_12, r_13, r_14, r_15, $
	r_array = rarr, distance = dist, threshold = thresh, status = stat

;+
; NAME:
;		PLANE
; VERSION
; 		2.0
; PURPOSE:
;		Determines the best plane passing through a set of points.
; CATEGORY:
;		Geometrical.
; CALLING SEQUENCE:
;		Result = PLANE ([R_0, R_1, ...
; INPUTS:
;	R_0 through R_15
;		Vectors representing points in space.  At least as many vectors as 
;		there are dimensions should be given, unless the keyword R_ARRAY is
;		being used.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	R_ARRAY
;		Allows to provide all the input vectors through a single array of the 
;		form:  Transpose([[r_0],[r_1], ... ])
;	THRESHOLD
;		Provides threshold value for singularity rejection.  Default value is 
;		1e-6.
;	DISTANCE
;		Optional output, see below.
;	STATUS
;		Optional output, see below.
; OUTPUTS:
;		Returns a vector containing the direction cosines of the normal to the
;		plane.
; OPTIONAL OUTPUT PARAMETERS:
;	DISTANCE
;		The name of a variable to receive the distance of the plane from the
;		origin.
;	STATUS
;		The name of a variable to receive calculation status.  Returns 0 if the
;		points are insuficient to determine a plane, else returns 1.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		Numbers of points must be equal or greater than number of dimensions.
; PROCEDURE:
;		Uses SOLVE_LINSYS from MIDL.  Also calls DEFAULT (from MIDL).
; MODIFICATION HISTORY:
;		Created 20-MAY-1994 by Mati Meron.
;-

	on_error, 1
	thr = Default(thresh,1e-6)
	if n_elements(rarr) eq 0 then begin
		nr = n_params()
		if nr gt 0 then begin
			rarr = transpose(r_0)
			rnam = strcompress(replicate('r_',nr) + string(indgen(nr)),/remove)
			for i = 1, nr - 1 do begin
				est = execute('rarr = [[rarr],transpose(' + rnam(i) + ')]')
				if not est then message, 'Data problems!'
			endfor
		endif else message, 'Missing input data!'
	endif else nr = (size(rarr))(1)
	if nr lt (size(rarr))(2) then message, 'Insufficient data!'

	res = Solve_linsys(rarr,replicate(1.,nr),/svd, thresh = thr, status = stat)
	if not stat then begin
		off = transpose(total(abs(rarr),1))/nr
		sarr = rarr + off(lonarr(nr),*)
		res = Solve_linsys(sarr,replicate(1.,nr),/svd,thresh= thr,status= stat)
		dist = 0.
	endif else dist = 1.
	tres = sqrt(total(res^2))
	dist = dist/tres

	return, res/tres
end