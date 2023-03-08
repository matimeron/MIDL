Pro Op_display_mm, x, y, ppos = pps, range = ran, _extra = _e

;+
; NAME:
;		OP_DISPLAY_MM
; VERSION:
;		8.00
; PURPOSE:
;		Overplotting images.
; CATEGORY:
;		Display.
; CALLING SEQUENCE:
;		OP_DISPLAY_MM, X, Y [,keywords]
; INPUTS:
;	X
;		Numeric, either a vector (scalar is considered to be a vector of
;		length 1) or an [N,*] array with N <=3.
; OPTIONAL INPUT PARAMETERS:
;	Y
;		Numeric, acceptable only when X is a vector, in which case Y has to be
;		a vector of same length.
;
;		Note:  X and Y serve as the plot coordinates.  The following cases are 
;		possible:
;			1)	Both X and Y are vectors of same length.
;			2)	X is an array.  In this case the first column of X is used as
;				x-coordinates and the second is used as y.
;			3)  X is a vector.  In this case it is used as y-coordinates, while
;				x-coordinates are generated internally.
;		See SPLIT_XY for more details.
; KEYWORD PARAMETERS:
;	PPOS
;		A 4-element integer vector containing the "plot position" (see 
;		!P.POSITION) of the image in device units.  
;	RANGE
;		A 4-element floating vector containing the X and Y coordinate ranges 
;		of the image in same format as PPOS, i.e. [X_min, Y_min, X_max, Y_max].
;
;		Note:	PPOS and RANGE are provided as outputs by the routine DISPLAY_MM
;				which is used to generate the image.  See there for details.
;	_EXTRA
;		A formal keyword used to pass  keywords acceptable by the PLOTS routine.
;		Not to be used directly.
; OUTPUTS:
;		None.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Calls SPLIT_XY to process the inputs, then uses the
;		PPOS and RANGE values to establish data coordinates and plots over the
;		image. 
; MODIFICATION HISTORY:
;		Created 5-OCT-2010 by Mati Meron.
;-

	on_error, 1

	if n_elements(pps) ne 4 or n_elements(ran) ne 4 then message, $
	'Bad position and/or range inputs!'
	nxy = Split_xy(x,y,x_ret=wx,y_ret=wy)
	plot, ran[[0,2]], ran[[1,3]], pos=pps, /dev, xsty=13, ysty=13, $
	/noerase, /nodata
	plots, wx, wy, noclip = 0, _extra = _e

	return
end