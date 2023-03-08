Function Img_equalize_old, dat, xonly = xon, yonly = yon, threshhold = tre, $
	mean = me, down = dw, up = up

;+
; NAME:
;		IMG_EQUALIZE
; VERSION:
;		8.21
; PURPOSE:
;		Reformats an image into one with equispaced coordinates.
; CATEGORY:
;		2D data processing
; CALLING SEQUENCE:
;		Result = IMG_EQUALIZE( DAT [, keywords])
; INPUTS:
;	DAT
;		Standard data representation 3D array with dimensions [4,M,N].
;		Dimensions [3,M,N] (no errors) also acceptable.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/XONLY
;		Switch, specifies that only the X coordinates are to be equalized.
;	/YONLY
;		Switch, specifies that only the Y coordinates are to be equalized.
;
;		Note:	Setting both /XONLY and /YONLY, or setting none, results in 
;				equalizing both X and Y.
;	THRESHOLD
;		Scalar value, specifies the minimal relative deviation of the grid from
;		equispacing that requires correction.  If not given, the default is 
;		8*TOLER(DAT) (see routine TOLER).
;	/MEAN													|	One and only one
;		Switch.  Specifies equalizing using mean spacing.	|	of these 3 may
;	/DOWN													|	be specified. If
;		Switch.  Specifies equalizing using minimal spacing.|	not given, the
;	/UP														|	default is MEAN.
;		Switch.  Specifies equalizing using maximal spacing.|
;		
;		Note:	/DOWN should be used with care as it may be time and memory
;				consuming.
; OUTPUTS:
;		Returns the equalized data, in [4,M',N'] (or [3,M',N']) format.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Generates the equalized data from the original one using interpolation.  
;		Calls IMG_REFORM.  Calls DEFAULT, DIF, HOW_MANY, MAKE_GRID, ONE_OF and
;		TOLER, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-SEP-2013 by Mati Meron.
;-

	on_error, 1

	eps = 8*Toler(dat)
	siz = size(dat)
	if siz[0] eq 3 then begin
		dum = How_many(fir=xon,sec=yon,whi=whi)
		if whi eq 0 then whi = 3
		wha = One_of(me,dw,up) > 0
		wtre = eps > Default(tre,0.,/dtyp) < 1
		if n_elements(wtre) eq 1 then wtre = [wtre,wtre]

		if whi and 1 then begin
			xs = reform(dat[0,*,0])
			xn = n_elements(xs)
			xran = [min(xs,max=xmax),xmax]
			dxav = (xran[1]-xran[0])/(xn-1)
			dx = Dif(xs,/lin)
			if max(abs(dx/dxav-1.)) gt wtre[0] then begin
				case wha of
					0	:	xr = Make_grid(xran,xn)
					1	:	xr = Make_grid(xran,min(dx),/step)
					2	:	xr = Make_grid(xran,max(dx),/step)
				endcase
			endif else xr = []
		endif else xr = []

		if whi/2 and 1 then begin
			ys = reform(dat[1,0,*])
			yn = n_elements(ys)
			yran = [min(ys,max=ymax),ymax]
			dyav = (yran[1]-yran[0])/(yn-1)
			dy = Dif(ys,/lin)
			if max(abs(dy/dyav-1.)) gt wtre[1] then begin
				case wha of
					0	:	yr = Make_grid(yran,yn)
					1	:	yr = Make_grid(yran,min(dy),/step)
					2	:	yr = Make_grid(yran,max(dy),/step)
				endcase
			endif else yr = []
		endif else yr = []

		res = Img_reform(dat,xval=xr,yval=yr)
	endif else message, 'Bad input!

	return, res
end