Pro Inset_mm, x, y, location = loc, region = reg, $
	xaxis = xax, yaxis = yax, box = box, _extra = _e

;+
; NAME:
;		INSET_MM
; VERSION:
;		8.0
; PURPOSE:
;		Generates an insent within a plot.
; CATEGORY:
;		Graphics utility.
; CALLING SEQUENCE:
;		INSET_MM, X, Y, keywords
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
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	LOCATION
;		Character input, specifies location of inset.  Four acceptable inputs:
;			UR	:	Upper Right corner.
;			UL	:	Upper Left corner.
;			LL	:	Lower Left corner.
;			LR	:	Lower Right corner.
;
;		Default location is UR.
;	REGION
;		4-element vector, specifies inset location in the format:
;		[X_lo, Y_lo, X_hi, Y_hi], in normalized coordinates.  If given, 
;		overrides the settings of LOCATION.
;	XAXIS
;		Specifies location of inset's x-axis, 0 for low, 1 for high.  The 
;		default is 0.
;	YAXIS
;		Specifies location of inset's y-axis, 0 for left, 1 for right.  The 
;		default is 0 if the inset is on the left side, 1 for the right.
;	/BOX
;		Switch.  If set, a full box is drawn to enclose the inset.  In this
;		case XAXIS and YAXIS are ignored.
;	_EXTRA
;		A formal keyword used to pass all plotting keywords.  Not to be used
;		directly.
; OUTPUTS:
;		None other than the Graphic output.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Calls DEFAULT, SPLIT_XY, STRMATCH_MM and WHERINSTRUCT,
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 10-OCT-2010 by Mati Meron.
;-

	on_error, 1

	posib = ['ur','ul','ll','lr']
	rar= [[.5, .5, .9, .9], [.1, .5, .5, .9], [.1, .1, .5, .5],[.5, .1, .9, .5]]

	nxy = Split_xy(x,y,x_ret=wx,y_ret=wy)
	wloc = 	Strmatch_mm(loc,posib,2) > 0
	if n_elements(reg) eq 4 then wreg = 0 > reg < 1 else wreg = rar[*,wloc]
	if keyword_set(box) then begin
		sty = 0
		bfl = 1
	endif else begin
		sty = 4
		wxax = 0 > Default(xax,0) < 1
		wyax = 0 > Default(yax,floor(wreg[0] + wreg[2])) < 1
		bfl = 0
	endelse

	chs = 0.8
	coli = Wherinstruct('col',_e)
	if coli ge 0 then begin
		ckeep = _e.(coli)
		_e.(coli) = !p.color
	endif

	tem = !p.region
	!p.region = wreg
	plot, wx, wy, xsty=sty, ysty=sty, charsize=chs, /noerase, /nodata, _extra=_e
	if bfl eq 0 then begin
		axis, xaxis = wxax, charsize = chs, _extra = _e
		axis, yaxis = wyax, charsize = chs, _extra = _e
	endif
	
	if coli ge 0 then _e.(coli) = ckeep
	oplot, wx, wy, color = col, _extra = _e
	!p.region = tem

	return
end