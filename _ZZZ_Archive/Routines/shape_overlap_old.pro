Function Shape_overlap_old, shape1, shape2, clean= cln, tol_mult= tml, exists= exs

;+
; NAME:
;		SHAPE_OVERLAP
; VERSION:
;		8.1
; PURPOSE:
;		Finds the SHAPE (see SHAPE_VER for a definition) resulting from the
;		overlap of two given shapes.
; CATEGORY:
;		General Graphics.
; CALLING SEQUENCE:
;		Result = SHAPE_OVERLAP( SHAPE1, SHAPE2[, EXISTS = EXS])
; INPUTS:
;	SHAPE1
;		Two dimensional shape i.e. a [2,*] numeric array.
;	SHAPE2
;		Ditto
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	CLEAN
; 		Switch.  If set, spurious points and zero area loops are cleaned off the
; 		shape.
;	TOL_MULT
;		Numeric scalar, tolerance multiplier to be used by the function 
;		SHAPE_CLEAN.  See there for details.
;	EXISTS
;		Optional output, see below.
; OUTPUTS:
;		Returns a new shape, which is the overlap of the original two shapes.
;		Important:  For the purpose of the calculation the two shapes are
;		assumed to have the same direction (even if they don't)
;		If there are no points left in the result shape, a single point (i.e.
;		2D vector) with X and Y coordinates equal to the square root of the
;		maximal floating value (machine dependent) is returned.
; OPTIONAL OUTPUT PARAMETERS:
;	EXISTS
;		The name of a variable to receive calculation status result.  Returns
;		1b if the result shape is non-empty, 0b otherwise.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Performs cuts on one shape with the lines of the other, using the
;		routine SHAPE_EDGE from MIDL.  Also calls FPU_FIX, SHAPE_AREA,
;		SHAPE_CLEAN, SHAPE_CLOSE and SHAPE_VER, from MIDL.
; MODIFICATION HISTORY:
;		Created 20-JUL-1999 by Mati Meron.
;		Checked for operation under Windows, 30-JAN-2001, by Mati Meron.
;		Modified 10-FEB-2007 by Mati Meron.  Bug fix.
;		Modified 5-SEP-2011 by Mati Meron.  Added keywords CLEAN and TOL_MULT.
;-

	on_error, 1
	exs = 0b

	wsh1 = Shape_close(shape1)
	wsh2 = Shape_close(shape2)
	nd1 = Shape_ver(wsh1,len = np1)
	nd2 = Shape_ver(wsh2,len = np2)
	if nd1 ne 2 or nd2 ne 2 then begin
		message, 'Only 2-D shapes accepted!', /continue
		return, 0b
	endif

	if np1 ge np2 then begin
		fir = temporary(wsh1)
		sec = temporary(wsh2)
		np = np2
	endif else begin
		fir = temporary(wsh2)
		sec = temporary(wsh1)
		np = np1
	endelse
	if Shape_area(sec) lt 0 then sec = reverse(sec,2)

	for i = 0l, np-2 do begin
		fir = Shape_edge(fir,sec[*,i:i+1],/seg,exists=exs)
		if exs eq 0 then i = np-1
	endfor
	if exs and keyword_set(cln) then fir = Shape_clean(fir,tol=tml)

	return, FPU_fix(fir)
end